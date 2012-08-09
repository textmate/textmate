;;; ediff-util.el --- the core commands and utilities of ediff

;; Copyright (C) 1994-2012  Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: ediff

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


(provide 'ediff-util)

;; Compiler pacifier
(defvar ediff-use-toolbar-p)
(defvar ediff-toolbar-height)
(defvar ediff-toolbar)
(defvar ediff-toolbar-3way)
(defvar bottom-toolbar)
(defvar bottom-toolbar-visible-p)
(defvar bottom-toolbar-height)
(defvar mark-active)

(defvar ediff-after-quit-hook-internal nil)

(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest  r))))

(eval-when-compile
  (require 'ediff))

;; end pacifier


(require 'ediff-init)
(require 'ediff-help)
(require 'ediff-mult)
(require 'ediff-wind)
(require 'ediff-diff)
(require 'ediff-merg)
;; for compatibility with current stable version of xemacs
(if (featurep 'xemacs)
    (require 'ediff-tbar))


;;; Functions

(defun ediff-mode ()
  "Ediff mode controls all operations in a single Ediff session.
This mode is entered through one of the following commands:
	`ediff'
	`ediff-files'
	`ediff-buffers'
	`ebuffers'
	`ediff3'
	`ediff-files3'
	`ediff-buffers3'
	`ebuffers3'
	`ediff-merge'
	`ediff-merge-files'
	`ediff-merge-files-with-ancestor'
	`ediff-merge-buffers'
	`ediff-merge-buffers-with-ancestor'
	`ediff-merge-revisions'
	`ediff-merge-revisions-with-ancestor'
	`ediff-windows-wordwise'
	`ediff-windows-linewise'
	`ediff-regions-wordwise'
	`ediff-regions-linewise'
	`epatch'
	`ediff-patch-file'
	`ediff-patch-buffer'
	`epatch-buffer'
        `erevision'
	`ediff-revision'

Commands:
\\{ediff-mode-map}"
  ;; FIXME: Use define-derived-mode.
  (kill-all-local-variables)
  (setq major-mode 'ediff-mode)
  (setq mode-name "Ediff")
  ;; We use run-hooks instead of run-mode-hooks for two reasons.
  ;; The ediff control buffer is read-only and it is not supposed to be
  ;; modified by minor modes and such. So, run-mode-hooks doesn't do anything
  ;; useful here on top of what run-hooks does.
  ;; Second, changing run-hooks to run-mode-hooks would require an
  ;; if-statement, since XEmacs doesn't have this.
  (run-hooks 'ediff-mode-hook))



;;; Build keymaps

(ediff-defvar-local ediff-mode-map nil
  "Local keymap used in Ediff mode.
This is local to each Ediff Control Panel, so they may vary from invocation
to invocation.")

;; Set up the keymap in the control buffer
(defun ediff-set-keys ()
  "Set up Ediff keymap, if necessary."
  (if (null ediff-mode-map)
      (ediff-setup-keymap))
  (use-local-map ediff-mode-map))

;; Reload Ediff keymap.  For debugging only.
(defun ediff-reload-keymap ()
  (interactive)
  (setq ediff-mode-map nil)
  (ediff-set-keys))


(defun ediff-setup-keymap ()
  "Set up the keymap used in the control buffer of Ediff."
  (setq ediff-mode-map (make-sparse-keymap))
  (suppress-keymap ediff-mode-map)

  (define-key ediff-mode-map
    (if (featurep 'emacs) [mouse-2] [button2]) 'ediff-help-for-quick-help)
  (define-key ediff-mode-map "\C-m"  'ediff-help-for-quick-help)

  (define-key ediff-mode-map "p" 'ediff-previous-difference)
  (define-key ediff-mode-map "\C-?" 'ediff-previous-difference)
  (define-key ediff-mode-map [delete] 'ediff-previous-difference)
  (define-key ediff-mode-map "\C-h" (if ediff-no-emacs-help-in-control-buffer
					'ediff-previous-difference nil))
  ;; must come after C-h, or else C-h wipes out backspace's binding in XEmacs
  (define-key ediff-mode-map [backspace] 'ediff-previous-difference)
  (define-key ediff-mode-map "n" 'ediff-next-difference)
  (define-key ediff-mode-map " " 'ediff-next-difference)
  (define-key ediff-mode-map "j" 'ediff-jump-to-difference)
  (define-key ediff-mode-map "g"  nil)
  (define-key ediff-mode-map "ga" 'ediff-jump-to-difference-at-point)
  (define-key ediff-mode-map "gb" 'ediff-jump-to-difference-at-point)
  (define-key ediff-mode-map "q" 'ediff-quit)
  (define-key ediff-mode-map "D" 'ediff-show-diff-output)
  (define-key ediff-mode-map "z" 'ediff-suspend)
  (define-key ediff-mode-map "\C-l" 'ediff-recenter)
  (define-key ediff-mode-map "|" 'ediff-toggle-split)
  (define-key ediff-mode-map "h" 'ediff-toggle-hilit)
  (or ediff-word-mode
      (define-key ediff-mode-map "@" 'ediff-toggle-autorefine))
  (if ediff-narrow-job
      (define-key ediff-mode-map "%" 'ediff-toggle-narrow-region))
  (define-key ediff-mode-map "~" 'ediff-swap-buffers)
  (define-key ediff-mode-map "v" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "\C-v" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "^" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "\M-v" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "V" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "<" 'ediff-scroll-horizontally)
  (define-key ediff-mode-map ">" 'ediff-scroll-horizontally)
  (define-key ediff-mode-map "i" 'ediff-status-info)
  (define-key ediff-mode-map "E" 'ediff-documentation)
  (define-key ediff-mode-map "?" 'ediff-toggle-help)
  (define-key ediff-mode-map "!" 'ediff-update-diffs)
  (define-key ediff-mode-map "M" 'ediff-show-current-session-meta-buffer)
  (define-key ediff-mode-map "R" 'ediff-show-registry)
  (or ediff-word-mode
      (define-key ediff-mode-map "*" 'ediff-make-or-kill-fine-diffs))
  (define-key ediff-mode-map "a"  nil)
  (define-key ediff-mode-map "b"  nil)
  (define-key ediff-mode-map "r"  nil)
  (cond (ediff-merge-job
	 ;; Will barf if no ancestor
	 (define-key ediff-mode-map "/" 'ediff-show-ancestor)
	 ;; In merging, we allow only A->C and B->C copying.
	 (define-key ediff-mode-map "a" 'ediff-copy-A-to-C)
	 (define-key ediff-mode-map "b" 'ediff-copy-B-to-C)
	 (define-key ediff-mode-map "r" 'ediff-restore-diff-in-merge-buffer)
	 (define-key ediff-mode-map "s" 'ediff-shrink-window-C)
	 (define-key ediff-mode-map "+" 'ediff-combine-diffs)
	 (define-key ediff-mode-map "$"  nil)
	 (define-key ediff-mode-map "$$" 'ediff-toggle-show-clashes-only)
	 (define-key ediff-mode-map "$*" 'ediff-toggle-skip-changed-regions)
	 (define-key ediff-mode-map "&" 'ediff-re-merge))
	(ediff-3way-comparison-job
	 (define-key ediff-mode-map "ab" 'ediff-copy-A-to-B)
	 (define-key ediff-mode-map "ba" 'ediff-copy-B-to-A)
	 (define-key ediff-mode-map "ac" 'ediff-copy-A-to-C)
	 (define-key ediff-mode-map "bc" 'ediff-copy-B-to-C)
	 (define-key ediff-mode-map "c" nil)
	 (define-key ediff-mode-map "ca" 'ediff-copy-C-to-A)
	 (define-key ediff-mode-map "cb" 'ediff-copy-C-to-B)
	 (define-key ediff-mode-map "ra" 'ediff-restore-diff)
	 (define-key ediff-mode-map "rb" 'ediff-restore-diff)
	 (define-key ediff-mode-map "rc" 'ediff-restore-diff)
	 (define-key ediff-mode-map "C"  'ediff-toggle-read-only))
	(t ; 2-way comparison
	 (define-key ediff-mode-map "a"  'ediff-copy-A-to-B)
	 (define-key ediff-mode-map "b"  'ediff-copy-B-to-A)
	 (define-key ediff-mode-map "ra" 'ediff-restore-diff)
	 (define-key ediff-mode-map "rb" 'ediff-restore-diff))
	) ; cond
  (define-key ediff-mode-map "G" 'ediff-submit-report)
  (define-key ediff-mode-map "#"  nil)
  (define-key ediff-mode-map "#h"  'ediff-toggle-regexp-match)
  (define-key ediff-mode-map "#f"  'ediff-toggle-regexp-match)
  (define-key ediff-mode-map "#c"  'ediff-toggle-ignore-case)
  (or ediff-word-mode
      (define-key ediff-mode-map "##"  'ediff-toggle-skip-similar))
  (define-key ediff-mode-map "o"   nil)
  (define-key ediff-mode-map "A"  'ediff-toggle-read-only)
  (define-key ediff-mode-map "B"  'ediff-toggle-read-only)
  (define-key ediff-mode-map "w"   nil)
  (define-key ediff-mode-map "wa"  'ediff-save-buffer)
  (define-key ediff-mode-map "wb"  'ediff-save-buffer)
  (define-key ediff-mode-map "wd"  'ediff-save-buffer)
  (define-key ediff-mode-map "="   'ediff-inferior-compare-regions)
  (if (and (fboundp 'ediff-show-patch-diagnostics) (ediff-patch-job))
      (define-key ediff-mode-map "P"  'ediff-show-patch-diagnostics))
  (if ediff-3way-job
      (progn
	(define-key ediff-mode-map "wc" 'ediff-save-buffer)
	(define-key ediff-mode-map "gc" 'ediff-jump-to-difference-at-point)
	))

  (define-key ediff-mode-map "m" 'ediff-toggle-wide-display)

  ;; Allow ediff-mode-map to be referenced indirectly
  (fset 'ediff-mode-map ediff-mode-map)
  (run-hooks 'ediff-keymap-setup-hook))


;;; Setup functions

;; Common startup entry for all Ediff functions It now returns control buffer
;; so other functions can do post-processing SETUP-PARAMETERS is a list of the
;; form ((param .val) (param . val)...)  This serves a similar purpose to
;; STARTUP-HOOKS, but these parameters are set in the new control buffer right
;; after this buf is created and before any windows are set and such.
(defun ediff-setup (buffer-A file-A buffer-B file-B buffer-C file-C
			     startup-hooks setup-parameters
			     &optional merge-buffer-file)
  (run-hooks 'ediff-before-setup-hook)
  ;; ediff-convert-standard-filename puts file names in the form appropriate
  ;; for the OS at hand.
  (setq file-A (ediff-convert-standard-filename (expand-file-name file-A)))
  (setq file-B (ediff-convert-standard-filename (expand-file-name file-B)))
  (if (stringp file-C)
      (setq file-C
	    (ediff-convert-standard-filename (expand-file-name file-C))))
  (if (stringp merge-buffer-file)
      (progn
	(setq merge-buffer-file
	      (ediff-convert-standard-filename
	       (expand-file-name merge-buffer-file)))
	;; check the directory exists
	(or (file-exists-p (file-name-directory merge-buffer-file))
	    (error "Directory %s given as place to save the merge doesn't exist"
		   (abbreviate-file-name
		    (file-name-directory merge-buffer-file))))
	(if (and (file-exists-p merge-buffer-file)
		 (file-directory-p merge-buffer-file))
	    (error "The merge buffer file %s must not be a directory"
		   (abbreviate-file-name merge-buffer-file)))
	))
  (let* ((control-buffer-name
	  (ediff-unique-buffer-name "*Ediff Control Panel" "*"))
	 (control-buffer (ediff-with-current-buffer buffer-A
			   (get-buffer-create control-buffer-name))))
    (ediff-with-current-buffer control-buffer
      (ediff-mode)

      (make-local-variable 'ediff-use-long-help-message)
      (make-local-variable 'ediff-prefer-iconified-control-frame)
      (make-local-variable 'ediff-split-window-function)
      (make-local-variable 'ediff-default-variant)
      (make-local-variable 'ediff-merge-window-share)
      (make-local-variable 'ediff-window-setup-function)
      (make-local-variable 'ediff-keep-variants)

      (make-local-variable 'window-min-height)
      (setq window-min-height 2)

      (if (featurep 'xemacs)
	  (make-local-hook 'ediff-after-quit-hook-internal))

      ;; unwrap set up parameters passed as argument
      (while setup-parameters
	(set (car (car setup-parameters)) (cdr (car setup-parameters)))
	(setq setup-parameters (cdr setup-parameters)))

      ;; set variables classifying the current ediff job
      ;; must come AFTER setup-parameters
      (setq ediff-3way-comparison-job (ediff-3way-comparison-job)
	    ediff-merge-job (ediff-merge-job)
	    ediff-merge-with-ancestor-job (ediff-merge-with-ancestor-job)
	    ediff-3way-job (ediff-3way-job)
	    ediff-diff3-job (ediff-diff3-job)
	    ediff-narrow-job (ediff-narrow-job)
	    ediff-windows-job (ediff-windows-job)
	    ediff-word-mode-job (ediff-word-mode-job))

      ;; Don't delete variants in case of ediff-buffer-* jobs without asking.
      ;; This is because one may lose work---dangerous.
      (if (string-match "buffer" (symbol-name ediff-job-name))
	  (setq ediff-keep-variants t))

      (if (featurep 'xemacs)
	  (make-local-hook 'pre-command-hook))

      (if (ediff-window-display-p)
	  (add-hook 'pre-command-hook 'ediff-spy-after-mouse nil 'local))
      (setq ediff-mouse-pixel-position (mouse-pixel-position))

      ;; adjust for merge jobs
      (if ediff-merge-job
	  (let ((buf
		 ;; If default variant is `combined', the right stuff is
		 ;; inserted by ediff-do-merge
		 ;; Note: at some point, we tried to put ancestor buffer here
		 ;; (which is currently buffer C.  This didn't work right
		 ;; because the merge buffer will contain lossage: diff regions
		 ;; in the ancestor, which correspond to revisions that agree
		 ;; in both buf A and B.
		 (cond ((eq ediff-default-variant 'default-B)
			buffer-B)
		       (t buffer-A))))

	    (setq ediff-split-window-function
		  ediff-merge-split-window-function)

	    ;; remember the ancestor buffer, if any
	    (setq ediff-ancestor-buffer buffer-C)

	    (setq buffer-C
		  (get-buffer-create
		   (ediff-unique-buffer-name "*ediff-merge" "*")))
	    (with-current-buffer buffer-C
	      (insert-buffer-substring buf)
	      (goto-char (point-min))
	      (funcall (ediff-with-current-buffer buf major-mode))
	      (widen) ; merge buffer is always widened
	      (add-hook 'local-write-file-hooks 'ediff-set-merge-mode nil t)
	      )))
      (setq buffer-read-only nil
	    ediff-buffer-A buffer-A
	    ediff-buffer-B buffer-B
	    ediff-buffer-C buffer-C
	    ediff-control-buffer control-buffer)

      (ediff-choose-syntax-table)

      (setq ediff-control-buffer-suffix
	    (if (string-match "<[0-9]*>" control-buffer-name)
		(substring control-buffer-name
			   (match-beginning 0) (match-end 0))
	      "")
	    ediff-control-buffer-number
	    (max
	     0
	     (1-
	      (string-to-number
	       (substring
		ediff-control-buffer-suffix
		(or
		 (string-match "[0-9]+" ediff-control-buffer-suffix)
		 0))))))

      (setq ediff-error-buffer
	    (get-buffer-create (ediff-unique-buffer-name "*ediff-errors" "*")))

      (with-current-buffer ediff-error-buffer
	(setq buffer-undo-list t))

      (ediff-with-current-buffer buffer-A (ediff-strip-mode-line-format))
      (ediff-with-current-buffer buffer-B (ediff-strip-mode-line-format))
      (if ediff-3way-job
	  (ediff-with-current-buffer buffer-C (ediff-strip-mode-line-format)))
      (if (ediff-buffer-live-p ediff-ancestor-buffer)
	  (ediff-with-current-buffer ediff-ancestor-buffer
	    (ediff-strip-mode-line-format)))

      (ediff-save-protected-variables) ; save variables to be restored on exit

      ;; ediff-setup-diff-regions-function must be set after setup
      ;; parameters are processed.
      (setq ediff-setup-diff-regions-function
	    (if ediff-diff3-job
		'ediff-setup-diff-regions3
	      'ediff-setup-diff-regions))

      (setq ediff-wide-bounds
	    (list (ediff-make-bullet-proof-overlay
		   '(point-min) '(point-max) ediff-buffer-A)
		  (ediff-make-bullet-proof-overlay
		   '(point-min) '(point-max) ediff-buffer-B)
		  (ediff-make-bullet-proof-overlay
		   '(point-min) '(point-max) ediff-buffer-C)))

      ;; This has effect only on ediff-windows/regions
      ;; In all other cases, ediff-visible-region sets visibility bounds to
      ;; ediff-wide-bounds, and ediff-narrow-bounds are ignored.
      (if ediff-start-narrowed
	  (setq ediff-visible-bounds ediff-narrow-bounds)
	(setq ediff-visible-bounds ediff-wide-bounds))

      (ediff-set-keys) ; comes after parameter setup

      ;; set up ediff-narrow-bounds, if not set
      (or ediff-narrow-bounds
	  (setq ediff-narrow-bounds ediff-wide-bounds))

      ;; All these must be inside ediff-with-current-buffer control-buffer,
      ;; since these vars are local to control-buffer
      ;; These won't run if there are errors in diff
      (ediff-with-current-buffer ediff-buffer-A
	(ediff-nuke-selective-display)
	(run-hooks 'ediff-prepare-buffer-hook)
	(if (ediff-with-current-buffer control-buffer ediff-merge-job)
	    (setq buffer-read-only t))
	;; add control-buffer to the list of sessions--no longer used, but may
	;; be used again in the future
	(or (memq control-buffer ediff-this-buffer-ediff-sessions)
	    (setq ediff-this-buffer-ediff-sessions
		  (cons control-buffer ediff-this-buffer-ediff-sessions)))
	(if ediff-make-buffers-readonly-at-startup
	    (setq buffer-read-only t))
	)

      (ediff-with-current-buffer ediff-buffer-B
	(ediff-nuke-selective-display)
	(run-hooks 'ediff-prepare-buffer-hook)
	(if (ediff-with-current-buffer control-buffer ediff-merge-job)
	    (setq buffer-read-only t))
	;; add control-buffer to the list of sessions
	(or (memq control-buffer ediff-this-buffer-ediff-sessions)
	    (setq ediff-this-buffer-ediff-sessions
		  (cons control-buffer ediff-this-buffer-ediff-sessions)))
	(if ediff-make-buffers-readonly-at-startup
	    (setq buffer-read-only t))
	)

      (if ediff-3way-job
	  (ediff-with-current-buffer ediff-buffer-C
	    (ediff-nuke-selective-display)
	    ;; the merge buffer should never be narrowed
	    ;; (it can happen if it is on rmail-mode or similar)
	    (if (ediff-with-current-buffer control-buffer ediff-merge-job)
		(widen))
	    (run-hooks 'ediff-prepare-buffer-hook)
	    ;; add control-buffer to the list of sessions
	    (or (memq control-buffer ediff-this-buffer-ediff-sessions)
		(setq ediff-this-buffer-ediff-sessions
		      (cons control-buffer
			    ediff-this-buffer-ediff-sessions)))
	    (if ediff-make-buffers-readonly-at-startup
		(setq buffer-read-only t)
	      (setq buffer-read-only nil))
	    ))

      (if (ediff-buffer-live-p ediff-ancestor-buffer)
	  (ediff-with-current-buffer ediff-ancestor-buffer
	    (ediff-nuke-selective-display)
	    (setq buffer-read-only t)
	    (run-hooks 'ediff-prepare-buffer-hook)
	    (or (memq control-buffer ediff-this-buffer-ediff-sessions)
		(setq ediff-this-buffer-ediff-sessions
		      (cons control-buffer
			    ediff-this-buffer-ediff-sessions)))
	    ))

      ;; the following must be after setting up  ediff-narrow-bounds AND after
      ;; nuking selective display
      (funcall ediff-setup-diff-regions-function file-A file-B file-C)
      (setq ediff-number-of-differences (length ediff-difference-vector-A))
      (setq ediff-current-difference -1)

      (ediff-make-current-diff-overlay 'A)
      (ediff-make-current-diff-overlay 'B)
      (if ediff-3way-job
	  (ediff-make-current-diff-overlay 'C))
      (if ediff-merge-with-ancestor-job
	  (ediff-make-current-diff-overlay 'Ancestor))

      (ediff-setup-windows buffer-A buffer-B buffer-C control-buffer)

      (let ((shift-A (ediff-overlay-start
		      (ediff-get-value-according-to-buffer-type
		       'A ediff-narrow-bounds)))
	    (shift-B (ediff-overlay-start
		      (ediff-get-value-according-to-buffer-type
		       'B ediff-narrow-bounds)))
	    (shift-C (ediff-overlay-start
		      (ediff-get-value-according-to-buffer-type
		       'C ediff-narrow-bounds))))
	;; position point in buf A
	(save-excursion
	  (select-window ediff-window-A)
	  (goto-char shift-A))
	;; position point in buf B
	(save-excursion
	  (select-window ediff-window-B)
	  (goto-char shift-B))
	(if ediff-3way-job
	    (save-excursion
	      (select-window ediff-window-C)
	      (goto-char shift-C)))
	)

      (select-window ediff-control-window)
      (ediff-visible-region)

      (run-hooks 'startup-hooks)
      (ediff-arrange-autosave-in-merge-jobs merge-buffer-file)

      (ediff-refresh-mode-lines)
      (setq buffer-read-only t)
      (setq ediff-session-registry
	    (cons control-buffer ediff-session-registry))
      (ediff-update-registry)
      (if (ediff-buffer-live-p ediff-meta-buffer)
	  (ediff-update-meta-buffer
	   ediff-meta-buffer nil ediff-meta-session-number))
      (run-hooks 'ediff-startup-hook)
      ) ; eval in control-buffer
    control-buffer))


;; This function assumes that we are in the window where control buffer is
;; to reside.
(defun ediff-setup-control-buffer (ctl-buf)
  "Set up window for control buffer."
  (if (window-dedicated-p (selected-window))
      (set-buffer ctl-buf) ; we are in control frame but just in case
    (switch-to-buffer ctl-buf))
  (let ((window-min-height 2))
    (erase-buffer)
    (ediff-set-help-message)
    (insert ediff-help-message)
    (shrink-window-if-larger-than-buffer)
    (or (ediff-multiframe-setup-p)
	(ediff-indent-help-message))
    (ediff-set-help-overlays)

    (set-buffer-modified-p nil)
    (ediff-refresh-mode-lines)
    (setq ediff-control-window (selected-window))
    (setq ediff-window-config-saved
	  (format "%S%S%S%S%S%S%S"
		  ediff-control-window
		  ediff-window-A
		  ediff-window-B
		  ediff-window-C
		  ediff-split-window-function
		  (ediff-multiframe-setup-p)
		  ediff-wide-display-p))

    (set-window-dedicated-p (selected-window) t)
    ;; In multiframe, toolbar is set in ediff-setup-control-frame
    (if (not (ediff-multiframe-setup-p))
	(ediff-make-bottom-toolbar)) ; this checks if toolbar is requested
    (goto-char (point-min))
    (skip-chars-forward ediff-whitespace)))

;; This executes in control buffer and sets auto-save, visited file name, etc,
;; in the merge buffer
(defun ediff-arrange-autosave-in-merge-jobs (merge-buffer-file)
  (if (not ediff-merge-job)
      ()
    (if (stringp merge-buffer-file)
	(setq ediff-autostore-merges t
	      ediff-merge-store-file merge-buffer-file))
    (if (stringp ediff-merge-store-file)
	(progn
	  ;; save before leaving ctl buffer
	  (ediff-verify-file-merge-buffer ediff-merge-store-file)
	  (setq merge-buffer-file ediff-merge-store-file)
	  (ediff-with-current-buffer ediff-buffer-C
	    (set-visited-file-name merge-buffer-file))))
    (ediff-with-current-buffer ediff-buffer-C
      (setq buffer-offer-save t) ; ask before killing buffer
      ;; make sure the contents is auto-saved
      (auto-save-mode 1))
    ))


;;; Commands for working with Ediff

(defun ediff-update-diffs ()
  "Recompute difference regions in buffers A, B, and C.
Buffers are not synchronized with their respective files, so changes done
to these buffers are not saved at this point---the user can do this later,
if necessary."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (if (and (ediff-buffer-live-p ediff-ancestor-buffer)
	   (not
	    (y-or-n-p
	     "Ancestor buffer will not be used.  Recompute diffs anyway? ")))
      (error "Recomputation of differences canceled"))

  (let ((point-A (ediff-with-current-buffer ediff-buffer-A (point)))
	;;(point-B (ediff-with-current-buffer ediff-buffer-B (point)))
	(tmp-buffer (get-buffer-create ediff-tmp-buffer))
	(buf-A-file-name (buffer-file-name ediff-buffer-A))
	(buf-B-file-name (buffer-file-name ediff-buffer-B))
	;; (null ediff-buffer-C) is no problem, as we later check if
	;; ediff-buffer-C is alive
	(buf-C-file-name (buffer-file-name ediff-buffer-C))
	(overl-A (ediff-get-value-according-to-buffer-type
		  'A ediff-narrow-bounds))
	(overl-B (ediff-get-value-according-to-buffer-type
		  'B ediff-narrow-bounds))
	(overl-C (ediff-get-value-according-to-buffer-type
		  'C ediff-narrow-bounds))
	beg-A end-A beg-B end-B beg-C end-C
	file-A file-B file-C)

    (if (stringp buf-A-file-name)
	(setq buf-A-file-name (file-name-nondirectory buf-A-file-name)))
    (if (stringp buf-B-file-name)
	(setq buf-B-file-name (file-name-nondirectory buf-B-file-name)))
    (if (stringp buf-C-file-name)
	(setq buf-C-file-name (file-name-nondirectory buf-C-file-name)))

    (ediff-unselect-and-select-difference -1)

    (setq beg-A (ediff-overlay-start overl-A)
	  beg-B (ediff-overlay-start overl-B)
	  beg-C (ediff-overlay-start overl-C)
	  end-A (ediff-overlay-end overl-A)
	  end-B (ediff-overlay-end overl-B)
	  end-C (ediff-overlay-end overl-C))

    (if ediff-word-mode
	(progn
	  (ediff-wordify beg-A end-A ediff-buffer-A tmp-buffer)
	  (setq file-A (ediff-make-temp-file tmp-buffer "regA"))
	  (ediff-wordify beg-B end-B ediff-buffer-B tmp-buffer)
	  (setq file-B (ediff-make-temp-file tmp-buffer "regB"))
	  (if ediff-3way-job
	      (progn
		(ediff-wordify beg-C end-C ediff-buffer-C tmp-buffer)
		(setq file-C (ediff-make-temp-file tmp-buffer "regC"))))
	  )
      ;; not word-mode
      (setq file-A (ediff-make-temp-file ediff-buffer-A buf-A-file-name))
      (setq file-B (ediff-make-temp-file ediff-buffer-B buf-B-file-name))
      (if ediff-3way-job
	  (setq file-C (ediff-make-temp-file ediff-buffer-C buf-C-file-name)))
      )

    (ediff-clear-diff-vector 'ediff-difference-vector-A 'fine-diffs-also)
    (ediff-clear-diff-vector 'ediff-difference-vector-B 'fine-diffs-also)
    (ediff-clear-diff-vector 'ediff-difference-vector-C 'fine-diffs-also)
    (ediff-clear-diff-vector
     'ediff-difference-vector-Ancestor 'fine-diffs-also)
    ;; let them garbage collect.  we can't use the ancestor after recomputing
    ;; the diffs.
    (setq ediff-difference-vector-Ancestor nil
	  ediff-ancestor-buffer nil
	  ediff-state-of-merge nil)

    (setq ediff-killed-diffs-alist nil) ; invalidate saved killed diff regions

    ;; In case of merge job, fool it into thinking that it is just doing
    ;; comparison
    (let ((ediff-setup-diff-regions-function ediff-setup-diff-regions-function)
	  (ediff-3way-comparison-job ediff-3way-comparison-job)
	  (ediff-merge-job ediff-merge-job)
	  (ediff-merge-with-ancestor-job ediff-merge-with-ancestor-job)
	  (ediff-job-name ediff-job-name))
      (if ediff-merge-job
	  (setq ediff-setup-diff-regions-function 'ediff-setup-diff-regions3
		ediff-3way-comparison-job t
		ediff-merge-job nil
		ediff-merge-with-ancestor-job nil
		ediff-job-name 'ediff-files3))
      (funcall ediff-setup-diff-regions-function file-A file-B file-C))

    (setq ediff-number-of-differences (length ediff-difference-vector-A))
    (delete-file file-A)
    (delete-file file-B)
    (if file-C
	(delete-file file-C))

    (if ediff-3way-job
	(ediff-set-state-of-all-diffs-in-all-buffers ediff-control-buffer))

    (ediff-jump-to-difference (ediff-diff-at-point 'A point-A))
    (message "")
    ))

;; Not bound to any key---to dangerous.  A user can do it if necessary.
(defun ediff-revert-buffers-then-recompute-diffs (noconfirm)
  "Revert buffers A, B and C.  Then rerun Ediff on file A and file B."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (let ((bufA ediff-buffer-A)
	(bufB ediff-buffer-B)
	(bufC ediff-buffer-C)
	(ctl-buf ediff-control-buffer)
	(keep-variants ediff-keep-variants)
	(ancestor-buf ediff-ancestor-buffer)
	(ancestor-job ediff-merge-with-ancestor-job)
	(merge ediff-merge-job)
	(comparison ediff-3way-comparison-job))
    (ediff-with-current-buffer bufA
      (revert-buffer t noconfirm))
    (ediff-with-current-buffer bufB
      (revert-buffer t noconfirm))
    ;; this should only be executed in a 3way comparison, not in merge
    (if comparison
	(ediff-with-current-buffer bufC
	  (revert-buffer t noconfirm)))
    (if merge
	(progn
	  (set-buffer ctl-buf)
	  ;; the argument says whether to reverse the meaning of
	  ;; ediff-keep-variants, i.e., ediff-really-quit runs here with
	  ;; variants kept.
	  (ediff-really-quit (not keep-variants))
	  (kill-buffer bufC)
	  (if ancestor-job
	      (ediff-merge-buffers-with-ancestor bufA bufB ancestor-buf)
	    (ediff-merge-buffers bufA bufB)))
      (ediff-update-diffs))))


;; optional NO-REHIGHLIGHT says to not rehighlight buffers
(defun ediff-recenter (&optional no-rehighlight)
  "Bring the highlighted region of all buffers being compared into view.
Reestablish the default three-window display."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let (buffer-read-only)
    (if (and (ediff-buffer-live-p ediff-buffer-A)
	     (ediff-buffer-live-p ediff-buffer-B)
	     (or (not ediff-3way-job)
		 (ediff-buffer-live-p ediff-buffer-C)))
	(ediff-setup-windows
	 ediff-buffer-A ediff-buffer-B ediff-buffer-C ediff-control-buffer)
      (or (eq this-command 'ediff-quit)
	  (message ediff-KILLED-VITAL-BUFFER
		   (beep 1)))
      ))

  ;; set visibility range appropriate to this invocation of Ediff.
  (ediff-visible-region)
  ;; raise
  (if (and (ediff-window-display-p)
	   (symbolp this-command)
	   (symbolp last-command)
	   ;; Either one of the display-changing commands
	   (or (memq this-command
		     '(ediff-recenter
		       ediff-dir-action ediff-registry-action
		       ediff-patch-action
		       ediff-toggle-wide-display ediff-toggle-multiframe))
	       ;; Or one of the movement cmds and prev cmd was an Ediff cmd
	       ;; This avoids raising frames unnecessarily.
	       (and (memq this-command
			  '(ediff-next-difference
			    ediff-previous-difference
			    ediff-jump-to-difference
			    ediff-jump-to-difference-at-point))
		    (not (string-match "^ediff-" (symbol-name last-command)))
		    )))
      (progn
	(if (window-live-p ediff-window-A)
	    (raise-frame (window-frame ediff-window-A)))
	(if (window-live-p ediff-window-B)
	    (raise-frame (window-frame ediff-window-B)))
	(if (window-live-p ediff-window-C)
	    (raise-frame (window-frame ediff-window-C)))))
  (if (and (ediff-window-display-p)
	   (frame-live-p ediff-control-frame)
	   (not ediff-use-long-help-message)
	   (not (ediff-frame-iconified-p ediff-control-frame)))
      (raise-frame ediff-control-frame))

  ;; Redisplay whatever buffers are showing, if there is a selected difference
  (let ((control-frame ediff-control-frame)
	(control-buf ediff-control-buffer))
    (if (and (ediff-buffer-live-p ediff-buffer-A)
	     (ediff-buffer-live-p ediff-buffer-B)
	     (or (not ediff-3way-job)
		 (ediff-buffer-live-p ediff-buffer-C)))
	(progn
	  (or no-rehighlight
	      (ediff-select-difference ediff-current-difference))

	  (ediff-recenter-one-window 'A)
	  (ediff-recenter-one-window 'B)
	  (if ediff-3way-job
	      (ediff-recenter-one-window 'C))

	  (ediff-with-current-buffer control-buf
	    (ediff-recenter-ancestor) ; check if ancestor is alive

	    (if (and (ediff-multiframe-setup-p)
		     (not ediff-use-long-help-message)
		     (not (ediff-frame-iconified-p ediff-control-frame)))
		;; never grab mouse on quit in this place
		(ediff-reset-mouse
		 control-frame
		 (eq this-command 'ediff-quit))))
	  ))

    (or no-rehighlight
	(ediff-restore-highlighting))
    (ediff-with-current-buffer control-buf (ediff-refresh-mode-lines))
    ))

;; this function returns to the window it was called from
;; (which was the control window)
(defun ediff-recenter-one-window (buf-type)
  (if (ediff-valid-difference-p)
      ;; context must be saved before switching to windows A/B/C
      (let* ((ctl-wind (selected-window))
	     (shift (ediff-overlay-start
		     (ediff-get-value-according-to-buffer-type
		      buf-type ediff-narrow-bounds)))
	     (job-name ediff-job-name)
	     (control-buf ediff-control-buffer)
	     (window-name (ediff-get-symbol-from-alist
			   buf-type ediff-window-alist))
	     (window (if (window-live-p (symbol-value window-name))
			 (symbol-value window-name))))

	(if (and window ediff-windows-job)
	    (set-window-start window shift))
	(if window
	    (progn
	      (select-window window)
	      (ediff-deactivate-mark)
	      (ediff-position-region
	       (ediff-get-diff-posn buf-type 'beg nil control-buf)
	       (ediff-get-diff-posn buf-type 'end nil control-buf)
	       (ediff-get-diff-posn buf-type 'beg nil control-buf)
	       job-name
	       )))
	(select-window ctl-wind)
	)))

(defun ediff-recenter-ancestor ()
  ;; do half-hearted job by recentering the ancestor buffer, if it is alive and
  ;; visible.
  (if (and (ediff-buffer-live-p ediff-ancestor-buffer)
	   (ediff-valid-difference-p))
      (let ((window (ediff-get-visible-buffer-window ediff-ancestor-buffer))
	    (ctl-wind (selected-window))
	    (job-name ediff-job-name)
	    (ctl-buf ediff-control-buffer))
	(ediff-with-current-buffer ediff-ancestor-buffer
	  (goto-char (ediff-get-diff-posn 'Ancestor 'beg nil ctl-buf))
	  (if window
	      (progn
		(select-window window)
		(ediff-position-region
		 (ediff-get-diff-posn 'Ancestor 'beg nil ctl-buf)
		 (ediff-get-diff-posn 'Ancestor 'end nil ctl-buf)
		 (ediff-get-diff-posn 'Ancestor 'beg nil ctl-buf)
		 job-name))))
	(select-window ctl-wind)
	)))


;; This will have to be refined for 3way jobs
(defun ediff-toggle-split ()
  "Toggle vertical/horizontal window split.
Does nothing if file-A and file-B are in different frames."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let* ((wind-A (if (window-live-p ediff-window-A) ediff-window-A))
	 (wind-B (if (window-live-p ediff-window-B) ediff-window-B))
	 (wind-C (if (window-live-p ediff-window-C) ediff-window-C))
	 (frame-A (if wind-A (window-frame wind-A)))
	 (frame-B (if wind-B (window-frame wind-B)))
	 (frame-C (if wind-C (window-frame wind-C))))
    (if (or (eq frame-A frame-B)
	    (not (frame-live-p frame-A))
	    (not (frame-live-p frame-B))
	    (if ediff-3way-comparison-job
		(or (not (frame-live-p frame-C))
		    (eq frame-A frame-C) (eq frame-B frame-C))))
	(setq ediff-split-window-function
	      (if (eq ediff-split-window-function 'split-window-vertically)
		  'split-window-horizontally
		'split-window-vertically))
      (message "Buffers being compared are in different frames"))
    (ediff-recenter 'no-rehighlight)))

(defun ediff-toggle-hilit ()
  "Switch between highlighting using ASCII flags and highlighting using faces.
On a dumb terminal, switches between ASCII highlighting and no highlighting."
  (interactive)
  (ediff-barf-if-not-control-buffer)

  (ediff-unselect-and-select-difference
   ediff-current-difference 'unselect-only)
  ;; cycle through highlighting
  (cond ((and ediff-use-faces
	      (ediff-has-face-support-p)
	      ediff-highlight-all-diffs)
	 (message "Unhighlighting unselected difference regions")
	 (setq ediff-highlight-all-diffs  nil
	       ediff-highlighting-style  'face))
	((or (and ediff-use-faces  (ediff-has-face-support-p)
		  (eq ediff-highlighting-style 'face))       ; has face support
	     (and (not (ediff-has-face-support-p))           ; no face support
		  (eq ediff-highlighting-style 'off)))
	 (message "Highlighting with ASCII flags")
	 (setq ediff-highlighting-style  'ascii
	       ediff-highlight-all-diffs  nil
	       ediff-use-faces            nil))
	((eq ediff-highlighting-style 'ascii)
	 (message "ASCII highlighting flags removed")
	 (setq ediff-highlighting-style  'off
	       ediff-highlight-all-diffs  nil))
	((ediff-has-face-support-p)   ; catch-all for cases with face support
	 (message "Re-highlighting all difference regions")
	 (setq ediff-use-faces            t
	       ediff-highlighting-style  'face
	       ediff-highlight-all-diffs  t)))

  (if (and ediff-use-faces ediff-highlight-all-diffs)
      (ediff-paint-background-regions)
    (ediff-paint-background-regions 'unhighlight))

  (ediff-unselect-and-select-difference
   ediff-current-difference 'select-only))


(defun ediff-toggle-autorefine ()
  "Toggle auto-refine mode."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (if ediff-word-mode
      (error "No fine differences in this mode"))
  (cond ((eq ediff-auto-refine 'nix)
	 (setq ediff-auto-refine 'on)
	 (ediff-make-fine-diffs ediff-current-difference 'noforce)
	 (message "Auto-refining is ON"))
	((eq ediff-auto-refine 'on)
	 (message "Auto-refining is OFF")
	 (setq ediff-auto-refine 'off))
	(t ;; nix 'em
	 (ediff-set-fine-diff-properties ediff-current-difference 'default)
	 (message "Refinements are HIDDEN")
	 (setq ediff-auto-refine 'nix))
	))

(defun ediff-show-ancestor ()
  "Show the ancestor buffer in a suitable window."
  (interactive)
  (ediff-recenter)
  (or (ediff-buffer-live-p ediff-ancestor-buffer)
      (if ediff-merge-with-ancestor-job
	  (error "Lost connection to ancestor buffer...sorry")
	(error "Not merging with ancestor")))
  (let (wind)
    (cond ((setq wind (ediff-get-visible-buffer-window ediff-ancestor-buffer))
	   (raise-frame (window-frame wind)))
	  (t (set-window-buffer ediff-window-C ediff-ancestor-buffer)))))

(defun ediff-make-or-kill-fine-diffs (arg)
  "Compute fine diffs.  With negative prefix arg, kill fine diffs.
In both cases, operates on the current difference region."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (cond ((eq arg '-)
	 (ediff-clear-fine-differences ediff-current-difference))
	((and (numberp arg) (< arg 0))
	 (ediff-clear-fine-differences ediff-current-difference))
	(t (ediff-make-fine-diffs))))


(defun ediff-toggle-help ()
  "Toggle short/long help message."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let (buffer-read-only)
    (erase-buffer)
    (setq ediff-use-long-help-message (not ediff-use-long-help-message))
    (ediff-set-help-message))
  ;; remember the icon status of the control frame when the user requested
  ;; full control message
  (if (and ediff-use-long-help-message (ediff-multiframe-setup-p))
      (setq ediff-prefer-iconified-control-frame
	    (ediff-frame-iconified-p ediff-control-frame)))

  (setq ediff-window-config-saved "") ; force redisplay
  (ediff-recenter 'no-rehighlight))


;; If BUF, this is the buffer to toggle, not current buffer.
(defun ediff-toggle-read-only (&optional buf)
  "Toggle read-only in current buffer.
If buffer is under version control and locked, check it out first.
If optional argument BUF is specified, toggle read-only in that buffer instead
of the current buffer."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((ctl-buf (if (null buf) (current-buffer)))
	(buf-type (ediff-char-to-buftype (ediff-last-command-char))))
    (or buf (ediff-recenter))
    (or buf
	(setq buf (ediff-get-buffer buf-type)))

    (ediff-with-current-buffer buf     ; eval in buf A/B/C
      (let* ((file (buffer-file-name buf))
	     (file-writable (and file
				 (file-exists-p file)
				 (file-writable-p file)))
	     (toggle-ro-cmd (cond (ediff-toggle-read-only-function)
				  ((ediff-file-checked-out-p file)
				   'toggle-read-only)
				  (file-writable 'toggle-read-only)
				  (t (key-binding "\C-x\C-q")))))
	;; If the file is checked in, make sure we don't make buffer modifiable
	;; without warning the user.  The user can fool our checks by making the
	;; buffer non-RO without checking the file out.  We regard this as a
	;; user problem.
	(if (and (ediff-file-checked-in-p file)
		 ;; If ctl-buf is null, this means we called this
		 ;; non-interactively, in which case don't ask questions
		 ctl-buf)
	    (cond ((not buffer-read-only)
		   (setq toggle-ro-cmd 'toggle-read-only))
		  ((and (or (beep 1) t) ; always beep
			(y-or-n-p
			 (format
			  "File %s is under version control.  Check it out? "
			  (ediff-abbreviate-file-name file))))
		   ;; if we checked the file out, we should also change the
		   ;; original state of buffer-read-only to nil.  If we don't
		   ;; do this, the mode line will show %%, since the file was
		   ;; RO before ediff started, so the user will think the file
		   ;; is checked in.
		   (ediff-with-current-buffer ctl-buf
		     (ediff-change-saved-variable
		      'buffer-read-only nil buf-type)))
		  (t
		   (setq toggle-ro-cmd 'toggle-read-only)
		   (beep 1) (beep 1)
		   (message
		    "Boy, this is risky! Don't modify this file...")
		   (sit-for 3)))) ; let the user see the warning
	(if (and toggle-ro-cmd
		 (string-match "toggle-read-only" (symbol-name toggle-ro-cmd)))
	    (save-excursion
	      (save-window-excursion
		(select-window (ediff-get-visible-buffer-window buf))
		(command-execute toggle-ro-cmd)))
	  (error "Don't know how to toggle read-only in buffer %S" buf))

	;; Check if we made the current buffer updatable, but its file is RO.
	;; Signal a warning in this case.
	(if (and file (not buffer-read-only)
		 (eq this-command 'ediff-toggle-read-only)
		 (file-exists-p file)
		 (not (file-writable-p file)))
	    (progn
	      (beep 1)
	      (message "Warning: file %s is read-only"
		       (ediff-abbreviate-file-name file))))
	))))

;; checkout if visited file is checked in
(defun ediff-maybe-checkout (buf)
  (let ((file (expand-file-name (buffer-file-name buf)))
	(checkout-function (key-binding "\C-x\C-q")))
    (if (and (ediff-file-checked-in-p file)
	     (or (beep 1) t)
	     (y-or-n-p
	      (format
	       "File %s is under version control.  Check it out? "
	       (ediff-abbreviate-file-name file))))
	(ediff-with-current-buffer buf
	  (command-execute checkout-function)))))


;; This is a simple-minded check for whether a file is under version control.
;; If file,v exists but file doesn't, this file is considered to be not checked
;; in and not checked out for the purpose of patching (since patch won't be
;; able to read such a file anyway).
;; FILE is a string representing file name
;;(defun ediff-file-under-version-control (file)
;;  (let* ((filedir (file-name-directory file))
;;	 (file-nondir (file-name-nondirectory file))
;;	 (trial (concat file-nondir ",v"))
;;	 (full-trial (concat filedir trial))
;;	 (full-rcs-trial (concat filedir "RCS/" trial)))
;;    (and (stringp file)
;;	 (file-exists-p file)
;;	 (or
;;	  (and
;;	   (file-exists-p full-trial)
;;	   ;; in FAT FS, `file,v' and `file' may turn out to be the same!
;;	   ;; don't be fooled by this!
;;	   (not (equal (file-attributes file)
;;		       (file-attributes full-trial))))
;;	  ;; check if a version is in RCS/ directory
;;	  (file-exists-p full-rcs-trial)))
;;       ))


(defun ediff-file-checked-out-p (file)
  (or (not (featurep 'vc-hooks))
      (and (vc-backend file)
	   (if (fboundp 'vc-state)
	       (or (memq (vc-state file) '(edited needs-merge))
		   (stringp (vc-state file)))
	     ;; XEmacs has no vc-state
	     (when (featurep 'xemacs) (vc-locking-user file)))
	   )))

(defun ediff-file-checked-in-p (file)
  (and (featurep 'vc-hooks)
       ;; Only RCS and SCCS files are considered checked in
       (memq (vc-backend file) '(RCS SCCS))
       (if (fboundp 'vc-state)
	   (and
	    (not (memq (vc-state file) '(edited needs-merge)))
	    (not (stringp (vc-state file))))
	 ;; XEmacs has no vc-state
	 (when (featurep 'xemacs) (not (vc-locking-user file))))
       ))

(defun ediff-file-compressed-p (file)
  (condition-case nil
      (require 'jka-compr)
    (error))
  (if (featurep 'jka-compr)
      (string-match (jka-compr-build-file-regexp) file)))


(defun ediff-swap-buffers ()
  "Rotate the display of buffers A, B, and C."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (if (and (window-live-p ediff-window-A) (window-live-p ediff-window-B))
      (let ((buf ediff-buffer-A)
	    (values ediff-buffer-values-orig-A)
	    (diff-vec ediff-difference-vector-A)
	    (hide-regexp ediff-regexp-hide-A)
	    (focus-regexp ediff-regexp-focus-A)
	    (wide-visibility-p (eq ediff-visible-bounds ediff-wide-bounds))
	    (overlay (if (ediff-has-face-support-p)
			 ediff-current-diff-overlay-A)))
	(if ediff-3way-comparison-job
	    (progn
	      (set-window-buffer ediff-window-A ediff-buffer-C)
	      (set-window-buffer ediff-window-B ediff-buffer-A)
	      (set-window-buffer ediff-window-C ediff-buffer-B)
	      )
	  (set-window-buffer ediff-window-A ediff-buffer-B)
	  (set-window-buffer ediff-window-B ediff-buffer-A))
	;; swap diff buffers
	(if ediff-3way-comparison-job
	    (setq ediff-buffer-A ediff-buffer-C
		  ediff-buffer-C ediff-buffer-B
		  ediff-buffer-B buf)
	  (setq ediff-buffer-A ediff-buffer-B
		ediff-buffer-B buf))

	;; swap saved buffer characteristics
	(if ediff-3way-comparison-job
	    (setq ediff-buffer-values-orig-A ediff-buffer-values-orig-C
		  ediff-buffer-values-orig-C ediff-buffer-values-orig-B
		  ediff-buffer-values-orig-B values)
	  (setq ediff-buffer-values-orig-A ediff-buffer-values-orig-B
		ediff-buffer-values-orig-B values))

	;; swap diff vectors
	(if ediff-3way-comparison-job
	    (setq ediff-difference-vector-A ediff-difference-vector-C
		  ediff-difference-vector-C ediff-difference-vector-B
		  ediff-difference-vector-B diff-vec)
	  (setq ediff-difference-vector-A ediff-difference-vector-B
		ediff-difference-vector-B diff-vec))

	;; swap hide/focus regexp
	(if ediff-3way-comparison-job
	    (setq ediff-regexp-hide-A ediff-regexp-hide-C
		  ediff-regexp-hide-C ediff-regexp-hide-B
		  ediff-regexp-hide-B hide-regexp
		  ediff-regexp-focus-A ediff-regexp-focus-C
		  ediff-regexp-focus-C ediff-regexp-focus-B
		  ediff-regexp-focus-B focus-regexp)
	  (setq ediff-regexp-hide-A ediff-regexp-hide-B
		ediff-regexp-hide-B hide-regexp
		ediff-regexp-focus-A ediff-regexp-focus-B
		ediff-regexp-focus-B focus-regexp))

	;; The following is needed for XEmacs, since there one can't move
	;; overlay to another buffer.  In Emacs, this swap is redundant.
	(if (ediff-has-face-support-p)
	    (if ediff-3way-comparison-job
		(setq ediff-current-diff-overlay-A ediff-current-diff-overlay-C
		      ediff-current-diff-overlay-C ediff-current-diff-overlay-B
		      ediff-current-diff-overlay-B overlay)
	      (setq ediff-current-diff-overlay-A ediff-current-diff-overlay-B
		    ediff-current-diff-overlay-B overlay)))

	;; swap wide bounds
	(setq ediff-wide-bounds
	      (cond (ediff-3way-comparison-job
		     (list (nth 2 ediff-wide-bounds)
			   (nth 0 ediff-wide-bounds)
			   (nth 1 ediff-wide-bounds)))
		    (ediff-3way-job
		     (list (nth 1 ediff-wide-bounds)
			   (nth 0 ediff-wide-bounds)
			   (nth 2 ediff-wide-bounds)))
		    (t
		     (list (nth 1 ediff-wide-bounds)
			   (nth 0 ediff-wide-bounds)))))
	;; swap narrow bounds
	(setq ediff-narrow-bounds
	      (cond (ediff-3way-comparison-job
		     (list (nth 2 ediff-narrow-bounds)
			   (nth 0 ediff-narrow-bounds)
			   (nth 1 ediff-narrow-bounds)))
		    (ediff-3way-job
		     (list (nth 1 ediff-narrow-bounds)
			   (nth 0 ediff-narrow-bounds)
			   (nth 2 ediff-narrow-bounds)))
		    (t
		     (list (nth 1 ediff-narrow-bounds)
			   (nth 0 ediff-narrow-bounds)))))
	(if wide-visibility-p
	    (setq ediff-visible-bounds ediff-wide-bounds)
	  (setq ediff-visible-bounds ediff-narrow-bounds))
	))
  (if ediff-3way-job
      (ediff-set-state-of-all-diffs-in-all-buffers ediff-control-buffer))
  (ediff-recenter 'no-rehighlight)
  )


(defun ediff-toggle-wide-display ()
  "Toggle wide/regular display.
This is especially useful when comparing buffers side-by-side."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (or (ediff-window-display-p)
      (error "%sEmacs is not running as a window application"
	     (if (featurep 'emacs) "" "X")))
  (ediff-recenter 'no-rehighlight) ; make sure buffs are displayed in windows
  (let ((ctl-buf ediff-control-buffer))
    (setq ediff-wide-display-p (not ediff-wide-display-p))
    (if (not ediff-wide-display-p)
	(ediff-with-current-buffer ctl-buf
	  (modify-frame-parameters
	   ediff-wide-display-frame ediff-wide-display-orig-parameters)
	  ;;(sit-for (if (featurep 'xemacs) 0.4 0))
	  ;; restore control buf, since ctl window may have been deleted
	  ;; during resizing
	  (set-buffer ctl-buf)
	  (setq ediff-wide-display-orig-parameters nil
		ediff-window-B nil) ; force update of window config
	  (ediff-recenter 'no-rehighlight))
      (funcall ediff-make-wide-display-function)
      ;;(sit-for (if (featurep 'xemacs) 0.4 0))
      (ediff-with-current-buffer ctl-buf
	(setq ediff-window-B nil) ; force update of window config
	(ediff-recenter 'no-rehighlight)))))

;;;###autoload
(defun ediff-toggle-multiframe ()
  "Switch from multiframe display to single-frame display and back.
To change the default, set the variable `ediff-window-setup-function',
which see."
  (interactive)
  (let (window-setup-func)
    (or (ediff-window-display-p)
	(error "%sEmacs is not running as a window application"
	       (if (featurep 'emacs) "" "X")))

  (cond ((eq ediff-window-setup-function 'ediff-setup-windows-multiframe)
	 (setq ediff-multiframe nil)
	 (setq window-setup-func 'ediff-setup-windows-plain))
	((eq ediff-window-setup-function 'ediff-setup-windows-plain)
	 (if (ediff-in-control-buffer-p)
	     (ediff-kill-bottom-toolbar))
	 (if (and (ediff-buffer-live-p ediff-control-buffer)
		  (window-live-p ediff-control-window))
	     (set-window-dedicated-p ediff-control-window nil))
	 (setq ediff-multiframe t)
	 (setq window-setup-func 'ediff-setup-windows-multiframe))
	(t
	 (if (and (ediff-buffer-live-p ediff-control-buffer)
		  (window-live-p ediff-control-window))
	     (set-window-dedicated-p ediff-control-window nil))
	 (setq ediff-multiframe t)
	 (setq window-setup-func 'ediff-setup-windows-multiframe))
	)

  ;; change default
  (setq-default ediff-window-setup-function window-setup-func)
  ;; change in all active ediff sessions
  (mapc (lambda(buf) (ediff-with-current-buffer buf
		       (setq ediff-window-setup-function window-setup-func
			     ediff-window-B nil)))
	ediff-session-registry)
  (if (ediff-in-control-buffer-p)
      (progn
	(set-window-dedicated-p (selected-window) nil)
	(ediff-recenter 'no-rehighlight)))))


;;;###autoload
(defun ediff-toggle-use-toolbar ()
  "Enable or disable Ediff toolbar.
Works only in versions of Emacs that support toolbars.
To change the default, set the variable `ediff-use-toolbar-p', which see."
  (interactive)
  (if (featurep 'ediff-tbar)
      (progn
	(or (ediff-window-display-p)
	    (error "%sEmacs is not running as a window application"
		   (if (featurep 'emacs) "" "X")))
	(if (ediff-use-toolbar-p)
	    (ediff-kill-bottom-toolbar))
	;; do this only after killing the toolbar
	(setq ediff-use-toolbar-p (not ediff-use-toolbar-p))

	(mapc (lambda(buf)
		(ediff-with-current-buffer buf
		  ;; force redisplay
		  (setq ediff-window-config-saved "")
		  ))
	      ediff-session-registry)
	(if (ediff-in-control-buffer-p)
	    (ediff-recenter 'no-rehighlight)))))


;; if was using toolbar, kill it
(defun ediff-kill-bottom-toolbar ()
  ;; Using ctl-buffer or ediff-control-window for LOCALE does not
  ;; work properly in XEmacs 19.14: we have to use
  ;;(selected-frame).
  ;; The problem with this is that any previous bottom-toolbar
  ;; will not re-appear after our cleanup here.  Is there a way
  ;; to do "push" and "pop" toolbars ?  --marcpa
  (if (featurep 'xemacs)
      (when (ediff-use-toolbar-p)
	(set-specifier bottom-toolbar (list (selected-frame) nil))
	(set-specifier bottom-toolbar-visible-p (list (selected-frame) nil)))))

;; If wants to use toolbar, make it.
;; If not, zero the toolbar for XEmacs.
;; Do nothing for Emacs.
(defun ediff-make-bottom-toolbar (&optional frame)
  (when (ediff-window-display-p)
    (setq frame (or frame (selected-frame)))
    (if (featurep 'xemacs)
	(cond ((ediff-use-toolbar-p) ; this checks for XEmacs
	       (set-specifier
		bottom-toolbar
		(list frame (if (ediff-3way-comparison-job)
				ediff-toolbar-3way ediff-toolbar)))
	       (set-specifier bottom-toolbar-visible-p (list frame t))
	       (set-specifier bottom-toolbar-height
			      (list frame ediff-toolbar-height)))
	      ((ediff-has-toolbar-support-p)
	       (set-specifier bottom-toolbar-height (list frame 0)))))))

;; Merging

(defun ediff-toggle-show-clashes-only ()
  "Toggle the mode that shows only the merge regions where both variants differ from the ancestor."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (if (not ediff-merge-with-ancestor-job)
      (error "This command makes sense only when merging with an ancestor"))
  (setq ediff-show-clashes-only (not ediff-show-clashes-only))
  (if ediff-show-clashes-only
      (message "Focus on regions where both buffers differ from the ancestor")
    (message "Canceling focus on regions where changes clash")))

(defun ediff-toggle-skip-changed-regions ()
  "Toggle the mode that skips the merge regions that differ from the default."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (setq ediff-skip-merge-regions-that-differ-from-default
	(not ediff-skip-merge-regions-that-differ-from-default))
  (if ediff-skip-merge-regions-that-differ-from-default
      (message "Skipping regions that differ from default setting")
    (message "Showing regions that differ from default setting")))



;; Widening/narrowing

(defun ediff-toggle-narrow-region ()
  "Toggle narrowing in buffers A, B, and C.
Used in ediff-windows/regions only."
  (interactive)
  (if (eq ediff-buffer-A ediff-buffer-B)
      (error ediff-NO-DIFFERENCES))
  (if (eq ediff-visible-bounds ediff-wide-bounds)
      (setq ediff-visible-bounds ediff-narrow-bounds)
    (setq ediff-visible-bounds ediff-wide-bounds))
  (ediff-recenter 'no-rehighlight))

;; Narrow bufs A/B/C to ediff-visible-bounds.  If this is currently set to
;; ediff-wide-bounds, then this actually widens.
;; This function does nothing if job-name is not
;; ediff-regions-wordwise/linewise or ediff-windows-wordwise/linewise.
;; Does nothing if buffer-A  = buffer-B since we can't narrow
;; to two different regions in one buffer.
(defun ediff-visible-region ()
  (if (or (eq ediff-buffer-A ediff-buffer-B)
	  (eq ediff-buffer-A ediff-buffer-C)
	  (eq ediff-buffer-C ediff-buffer-B))
      ()
    ;; If ediff-*-regions/windows, ediff-visible-bounds is already set
    ;; Otherwise, always use full range.
    (if (not ediff-narrow-job)
	(setq ediff-visible-bounds ediff-wide-bounds))
    (let ((overl-A (ediff-get-value-according-to-buffer-type
		    'A  ediff-visible-bounds))
	  (overl-B (ediff-get-value-according-to-buffer-type
		    'B  ediff-visible-bounds))
	  (overl-C (ediff-get-value-according-to-buffer-type
		    'C  ediff-visible-bounds))
	  )
      (ediff-with-current-buffer ediff-buffer-A
	(if (ediff-overlay-buffer overl-A)
	    (narrow-to-region
	     (ediff-overlay-start overl-A) (ediff-overlay-end overl-A))))
      (ediff-with-current-buffer ediff-buffer-B
	(if (ediff-overlay-buffer overl-B)
	    (narrow-to-region
	     (ediff-overlay-start overl-B) (ediff-overlay-end overl-B))))

      (if (and ediff-3way-job (ediff-overlay-buffer overl-C))
	  (ediff-with-current-buffer ediff-buffer-C
	    (narrow-to-region
	     (ediff-overlay-start overl-C) (ediff-overlay-end overl-C))))
      )))


;; Window scrolling operations

;; Performs some operation on the two file windows (if they are showing).
;; Traps all errors on the operation in windows A/B/C.
;; Usually, errors come from scrolling off the
;; beginning or end of the buffer, and this gives error messages.
(defun ediff-operate-on-windows (operation arg)

  ;; make sure windows aren't dead
  (if (not (and (window-live-p ediff-window-A) (window-live-p ediff-window-B)))
      (ediff-recenter 'no-rehighlight))
  (if (not (and (ediff-buffer-live-p ediff-buffer-A)
		(ediff-buffer-live-p ediff-buffer-B)
		(or (not ediff-3way-job) ediff-buffer-C)
		))
      (error ediff-KILLED-VITAL-BUFFER))

  (let* ((wind (selected-window))
	 (wind-A ediff-window-A)
	 (wind-B ediff-window-B)
	 (wind-C ediff-window-C)
	 (coefA (ediff-get-region-size-coefficient 'A operation))
	 (coefB (ediff-get-region-size-coefficient 'B operation))
	 (three-way ediff-3way-job)
	 (coefC (if three-way
		    (ediff-get-region-size-coefficient 'C operation))))

    (select-window wind-A)
    (condition-case nil
	(funcall operation (round (* coefA arg)))
      (error))
    (select-window wind-B)
    (condition-case nil
	(funcall operation (round (* coefB arg)))
      (error))
    (if three-way
	(progn
	  (select-window wind-C)
	  (condition-case nil
	      (funcall operation (round (* coefC arg)))
	    (error))))
    (select-window wind)))

(defun ediff-scroll-vertically (&optional arg)
  "Vertically scroll buffers A, B \(and C if appropriate\).
With optional argument ARG, scroll ARG lines; otherwise scroll by nearly
the one half of the height of window-A."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)

  ;; make sure windows aren't dead
  (if (not (and (window-live-p ediff-window-A) (window-live-p ediff-window-B)))
      (ediff-recenter 'no-rehighlight))
  (if (not (and (ediff-buffer-live-p ediff-buffer-A)
		(ediff-buffer-live-p ediff-buffer-B)
		(or (not ediff-3way-job)
		    (ediff-buffer-live-p ediff-buffer-C))
		))
      (error ediff-KILLED-VITAL-BUFFER))

  (ediff-operate-on-windows
   (if (memq (ediff-last-command-char) '(?v ?\C-v))
       'scroll-up
     'scroll-down)
   ;; calculate argument to scroll-up/down
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount (the window height)
     (let (default-amount)
       (setq default-amount
	     (- (/ (min (window-height ediff-window-A)
			(window-height ediff-window-B)
			(if ediff-3way-job
			    (window-height ediff-window-C)
			  500)) ; some large number
		   2)
		1 next-screen-context-lines))
       ;; window found
       (if arg
	   ;; C-u as argument means half of default amount
	   (/ default-amount 2)
	 ;; no argument means default amount
	 default-amount)))))


(defun ediff-scroll-horizontally (&optional arg)
  "Horizontally scroll buffers A, B \(and C if appropriate\).
If an argument is given, that is how many columns are scrolled, else nearly
the width of the A/B/C windows."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)

  ;; make sure windows aren't dead
  (if (not (and (window-live-p ediff-window-A) (window-live-p ediff-window-B)))
      (ediff-recenter 'no-rehighlight))
  (if (not (and (ediff-buffer-live-p ediff-buffer-A)
		(ediff-buffer-live-p ediff-buffer-B)
		(or (not ediff-3way-job)
		    (ediff-buffer-live-p ediff-buffer-C))
		))
      (error ediff-KILLED-VITAL-BUFFER))

  (ediff-operate-on-windows
   ;; Arrange for scroll-left and scroll-right being called
   ;; interactively so that they set the window's min_hscroll.
   ;; Otherwise, automatic hscrolling will undo the effect of
   ;; hscrolling.
   (if (= (ediff-last-command-char) ?<)
       (lambda (arg)
	 (let ((prefix-arg arg))
	   (call-interactively 'scroll-left)))
     (lambda (arg)
       (let ((prefix-arg arg))
	 (call-interactively 'scroll-right))))
   ;; calculate argument to scroll-left/right
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount
     ;; (half the window width)
     (if (null ediff-control-window)
	 ;; no control window, use nil
	 nil
       (let ((default-amount
	       (- (/ (min (window-width ediff-window-A)
			  (window-width ediff-window-B)
			  (if ediff-3way-comparison-job
			      (window-width ediff-window-C)
			    500) ; some large number
			  )
		     2)
		  3)))
	 ;; window found
	 (if arg
	     ;; C-u as argument means half of default amount
	     (/ default-amount 2)
	   ;; no argument means default amount
	   default-amount))))))


;;BEG, END show the region to be positioned.
;;JOB-NAME holds ediff-job-name.  The ediff-windows job positions regions
;;differently.
(defun ediff-position-region (beg end pos job-name)
  (if (> end (point-max))
      (setq end (point-max)))
  (if ediff-windows-job
      (if (pos-visible-in-window-p end)
	  () ; do nothing, wind is already positioned
	;; at this point, windows are positioned at the beginning of the
	;; file regions (not diff-regions)  being compared.
	(save-excursion
	  (move-to-window-line (- (window-height) 2))
	  (let ((amount (+ 2 (count-lines (point) end))))
	    (scroll-up amount))))
    (set-window-start (selected-window) beg)
    (if (pos-visible-in-window-p end)
	;; Determine the number of lines that the region occupies
	(let ((lines 0)
	      (prev-point 0))
	  (while ( and (> end (progn
				(move-to-window-line lines)
				(point)))
		       ;; `end' may be beyond the window bottom, so check
		       ;; that we are making progress
		       (< prev-point (point)))
	    (setq prev-point (point))
	    (setq lines (1+ lines)))
	  ;; And position the beginning on the right line
	  (goto-char beg)
	  (recenter (/ (1+ (max (- (1- (window-height (selected-window)))
				   lines)
				1)
			   )
		       2))))
    (goto-char pos)
    ))

;; get number of lines from window start to region end
(defun ediff-get-lines-to-region-end (buf-type &optional n ctl-buf)
  (or n (setq n ediff-current-difference))
  (or ctl-buf (setq ctl-buf ediff-control-buffer))
  (ediff-with-current-buffer ctl-buf
    (let* ((buf (ediff-get-buffer buf-type))
	   (wind (eval (ediff-get-symbol-from-alist
			buf-type ediff-window-alist)))
	   (beg (window-start wind))
	   (end (ediff-get-diff-posn buf-type 'end))
	   lines)
      (ediff-with-current-buffer buf
	(if (< beg end)
	    (setq lines (count-lines beg end))
	  (setq lines 0))
	lines
	))))

;; Calculate the number of lines from window end to the start of diff region
(defun ediff-get-lines-to-region-start (buf-type &optional diff-num ctl-buf)
  (or diff-num (setq diff-num ediff-current-difference))
  (or ctl-buf (setq ctl-buf ediff-control-buffer))
  (ediff-with-current-buffer ctl-buf
    (let* ((buf (ediff-get-buffer buf-type))
	   (wind (eval (ediff-get-symbol-from-alist
			buf-type ediff-window-alist)))
	   (end (or (window-end wind) (window-end wind t)))
	   (beg (ediff-get-diff-posn buf-type 'beg diff-num)))
      (ediff-with-current-buffer buf
	(if (< beg end)
	    (count-lines (max beg (point-min)) (min end (point-max))) 0))
      )))


;; region size coefficient is a coefficient by which to adjust scrolling
;; up/down of the window displaying buffer of type BUFTYPE.
;; The purpose of this coefficient is to make the windows scroll in sync, so
;; that it won't happen that one diff region is scrolled off while the other is
;; still seen.
;;
;; If the difference region is invalid, the coefficient is 1
(defun ediff-get-region-size-coefficient (buf-type op &optional n ctl-buf)
  (ediff-with-current-buffer (or ctl-buf ediff-control-buffer)
    (if (ediff-valid-difference-p n)
	(let* ((func (cond ((eq op 'scroll-down)
			    'ediff-get-lines-to-region-start)
			   ((eq op 'scroll-up)
			    'ediff-get-lines-to-region-end)
			   (t (lambda (a b c) 0))))
	       (max-lines (max (funcall func 'A n ctl-buf)
			       (funcall func 'B n ctl-buf)
			       (if (ediff-buffer-live-p ediff-buffer-C)
				   (funcall func 'C n ctl-buf)
				 0))))
	  ;; this covers the horizontal coefficient as well:
	  ;; if max-lines = 0 then coef = 1
	  (if (> max-lines 0)
	      (/ (+ (funcall func buf-type n ctl-buf) 0.0)
		 (+ max-lines 0.0))
	    1))
      1)))


(defun ediff-next-difference (&optional arg)
  "Advance to the next difference.
With a prefix argument, go forward that many differences."
  (interactive "p")
  (ediff-barf-if-not-control-buffer)
  (if (< ediff-current-difference ediff-number-of-differences)
      (let ((n (min ediff-number-of-differences
		    (+ ediff-current-difference (or arg 1))))
	    non-clash-skip skip-changed regexp-skip)

	(ediff-visible-region)
	(or (>= n ediff-number-of-differences)
	    (setq regexp-skip (funcall ediff-skip-diff-region-function n))
	    ;; this won't exec if regexp-skip is t
	    (setq non-clash-skip (ediff-merge-region-is-non-clash-to-skip n)
		  skip-changed
		  (ediff-skip-merge-region-if-changed-from-default-p n))
	    (ediff-install-fine-diff-if-necessary n))
	;; Skip loop
	(while (and (< n ediff-number-of-differences)
		    (or
		     ;; regexp skip
		     regexp-skip
		     ;; skip clashes, if necessary
		     non-clash-skip
		     ;; skip processed regions
		     skip-changed
		     ;; skip difference regions that differ in white space
		     (and ediff-ignore-similar-regions
			  (ediff-merge-region-is-non-clash n)
			  (or (eq (ediff-no-fine-diffs-p n) t)
			      (and (ediff-merge-job)
				   (eq (ediff-no-fine-diffs-p n) 'C)))
			  )))
	  (setq n (1+ n))
	  (if (= 0 (mod n 20))
	      (message "Skipped over region %d and counting ..."  n))
	  (or (>= n ediff-number-of-differences)
	      (setq regexp-skip (funcall ediff-skip-diff-region-function n))
	      ;; this won't exec if regexp-skip is t
	      (setq non-clash-skip (ediff-merge-region-is-non-clash-to-skip n)
		    skip-changed
		    (ediff-skip-merge-region-if-changed-from-default-p n))
	      (ediff-install-fine-diff-if-necessary n))
	  )
	(message "")
	(ediff-unselect-and-select-difference n)
	) ; let
    (ediff-visible-region)
    (error "At end of the difference list")))

(defun ediff-previous-difference (&optional arg)
  "Go to the previous difference.
With a prefix argument, go back that many differences."
  (interactive "p")
  (ediff-barf-if-not-control-buffer)
  (if (> ediff-current-difference -1)
      (let ((n (max -1 (- ediff-current-difference (or arg 1))))
	    non-clash-skip skip-changed regexp-skip)

	(ediff-visible-region)
	(or (< n 0)
	    (setq regexp-skip (funcall ediff-skip-diff-region-function n))
	    ;; this won't exec if regexp-skip is t
	    (setq non-clash-skip (ediff-merge-region-is-non-clash-to-skip n)
		  skip-changed
		  (ediff-skip-merge-region-if-changed-from-default-p n))
	    (ediff-install-fine-diff-if-necessary n))
	(while (and (> n -1)
		    (or
		     ;; regexp skip
		     regexp-skip
		     ;; skip clashes, if necessary
		     non-clash-skip
		     ;; skipp changed regions
		     skip-changed
		     ;; skip difference regions that differ in white space
		     (and ediff-ignore-similar-regions
			  (ediff-merge-region-is-non-clash n)
			  (or (eq (ediff-no-fine-diffs-p n) t)
			      (and (ediff-merge-job)
				   (eq (ediff-no-fine-diffs-p n) 'C)))
			  )))
	  (if (= 0 (mod (1+ n) 20))
	      (message "Skipped over region %d and counting ..."  (1+ n)))
	  (setq n (1- n))
	  (or (< n 0)
	      (setq regexp-skip (funcall ediff-skip-diff-region-function n))
	      ;; this won't exec if regexp-skip is t
	      (setq non-clash-skip (ediff-merge-region-is-non-clash-to-skip n)
		    skip-changed
		    (ediff-skip-merge-region-if-changed-from-default-p n))
	      (ediff-install-fine-diff-if-necessary n))
	  )
	(message "")
	(ediff-unselect-and-select-difference n)
	) ; let
    (ediff-visible-region)
    (error "At beginning of the difference list")))

;; The diff number is as perceived by the user (i.e., 1+ the internal
;; representation)
(defun ediff-jump-to-difference (difference-number)
  "Go to the difference specified as a prefix argument.
If the prefix is negative, count differences from the end."
  (interactive "p")
  (ediff-barf-if-not-control-buffer)
  (setq difference-number
	(cond ((< difference-number 0)
	       (+ ediff-number-of-differences difference-number))
	      ((> difference-number 0) (1- difference-number))
	      (t -1)))
  ;; -1 is allowed by ediff-unselect-and-select-difference --- it is the
  ;; position before the first one.
  (if (and (>= difference-number -1)
	   (<= difference-number ediff-number-of-differences))
      (ediff-unselect-and-select-difference difference-number)
    (error ediff-BAD-DIFF-NUMBER
	   this-command (1+ difference-number) ediff-number-of-differences)))

(defun ediff-jump-to-difference-at-point (arg)
  "Go to difference closest to the point in buffer A, B, or C.
The buffer depends on last command character \(a, b, or c\) that invoked this
command.  For instance, if the command was `ga' then the point value in buffer
A is used.
With a prefix argument, synchronize all files around the current point position
in the specified buffer."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (let* ((buf-type (ediff-char-to-buftype (ediff-last-command-char)))
	 (buffer (ediff-get-buffer buf-type))
	 (pt (ediff-with-current-buffer buffer (point)))
	 (diff-no (ediff-diff-at-point buf-type nil (if arg 'after)))
	 (past-last-diff (< ediff-number-of-differences diff-no))
	 (beg (if past-last-diff
		  (ediff-with-current-buffer buffer (point-max))
		(ediff-get-diff-posn buf-type 'beg (1- diff-no))))
	 ctl-wind wind-A wind-B wind-C
	 shift)
    (if past-last-diff
	(ediff-jump-to-difference -1)
      (ediff-jump-to-difference diff-no))
    (setq ctl-wind (selected-window)
	  wind-A ediff-window-A
	  wind-B ediff-window-B
	  wind-C ediff-window-C)
    (if arg
	(progn
	  (ediff-with-current-buffer buffer
	    (setq shift (- beg pt)))
	  (select-window wind-A)
	  (if past-last-diff (goto-char (point-max)))
	  (condition-case nil
	      (backward-char shift) ; noerror, if beginning of buffer
	    (error))
	  (recenter)
	  (select-window wind-B)
	  (if past-last-diff (goto-char (point-max)))
	  (condition-case nil
	      (backward-char shift) ; noerror, if beginning of buffer
	    (error))
	  (recenter)
	  (if (window-live-p wind-C)
	      (progn
		(select-window wind-C)
		(if past-last-diff (goto-char (point-max)))
		(condition-case nil
		    (backward-char shift) ; noerror, if beginning of buffer
		  (error))
		(recenter)
		))
	  (select-window ctl-wind)
	  ))
    ))


;; find region most related to the current point position (or POS, if given)
;; returns diff number as seen by the user (i.e., 1+ the internal
;; representation)
;; The optional argument WHICH-DIFF can be `after' or `before'.  If `after',
;; find the diff after the point.  If `before', find the diff before the
;; point.  If the point is inside a diff, return that diff.
(defun ediff-diff-at-point (buf-type &optional pos which-diff)
  (let ((buffer (ediff-get-buffer buf-type))
	(ctl-buffer ediff-control-buffer)
	(max-dif-num (1- ediff-number-of-differences))
	(diff-no -1)
	(prev-beg 0)
	(prev-end 0)
	(beg 0)
	(end 0))

    (ediff-with-current-buffer buffer
      (setq pos (or pos (point)))
      (while (and (or (< pos prev-beg) (> pos beg))
		  (< diff-no max-dif-num))
	(setq diff-no (1+ diff-no))
	(setq prev-beg beg
	      prev-end end)
	(setq beg (ediff-get-diff-posn buf-type 'beg diff-no ctl-buffer)
	      end (ediff-get-diff-posn buf-type 'end diff-no ctl-buffer))
	)

      ;; boost diff-no by 1, if past the last diff region
      (if (and (memq which-diff '(after before))
	       (> pos beg) (= diff-no max-dif-num))
	  (setq diff-no (1+ diff-no)))

      (cond ((eq which-diff 'after) (1+ diff-no))
	    ((eq which-diff 'before) diff-no)
	    ((< (abs (count-lines pos (max 1 prev-end)))
		(abs (count-lines pos (max 1 beg))))
	     diff-no) 	    ; choose prev difference
	    (t
	     (1+ diff-no))) ; choose next difference
     )))


;;; Copying diffs.

(defun ediff-diff-to-diff (arg &optional keys)
  "Copy buffer-X'th difference region to buffer Y \(X,Y are A, B, or C\).
If numerical prefix argument, copy the difference specified in the arg.
Otherwise, copy the difference given by `ediff-current-difference'.
This command assumes it is bound to a 2-character key sequence, `ab', `ba',
`ac', etc., which is used to determine the types of buffers to be used for
copying difference regions.  The first character in the sequence specifies
the source buffer and the second specifies the target.

If the second optional argument, a 2-character string, is given, use it to
determine the source and the target buffers instead of the command keys."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (or keys (setq keys (this-command-keys)))
  (if (eq arg '-) (setq arg -1)) ; translate neg arg to -1
  (if (numberp arg) (ediff-jump-to-difference arg))

  (let* ((key1 (aref keys 0))
	 (key2 (aref keys 1))
	 (char1 (ediff-event-key key1))
	 (char2 (ediff-event-key key2))
	 ediff-verbose-p)
    (ediff-copy-diff ediff-current-difference
		     (ediff-char-to-buftype char1)
		     (ediff-char-to-buftype char2))
    ;; recenter with rehighlighting, but no messages
    (ediff-recenter)))

(defun ediff-copy-A-to-B (arg)
  "Copy ARGth difference region from buffer A to B.
ARG is a prefix argument.  If nil, copy the current difference region."
  (interactive "P")
  (ediff-diff-to-diff arg "ab"))

(defun ediff-copy-B-to-A (arg)
  "Copy ARGth difference region from buffer B to A.
ARG is a prefix argument.  If nil, copy the current difference region."
  (interactive "P")
  (ediff-diff-to-diff arg "ba"))

(defun ediff-copy-A-to-C (arg)
  "Copy ARGth difference region from buffer A to buffer C.
ARG is a prefix argument.  If nil, copy the current difference region."
  (interactive "P")
  (ediff-diff-to-diff arg "ac"))

(defun ediff-copy-B-to-C (arg)
  "Copy ARGth difference region from buffer B to buffer C.
ARG is a prefix argument.  If nil, copy the current difference region."
  (interactive "P")
  (ediff-diff-to-diff arg "bc"))

(defun ediff-copy-C-to-B (arg)
  "Copy ARGth difference region from buffer C to B.
ARG is a prefix argument.  If nil, copy the current difference region."
  (interactive "P")
  (ediff-diff-to-diff arg "cb"))

(defun ediff-copy-C-to-A (arg)
  "Copy ARGth difference region from buffer C to A.
ARG is a prefix argument.  If nil, copy the current difference region."
  (interactive "P")
  (ediff-diff-to-diff arg "ca"))



;; Copy diff N from FROM-BUF-TYPE \(given as A, B or C\) to TO-BUF-TYPE.
;; If optional DO-NOT-SAVE is non-nil, do not save the old value of the
;; target diff.  This is used in merging, when constructing the merged
;; version.
(defun ediff-copy-diff (n from-buf-type to-buf-type
			  &optional batch-invocation reg-to-copy)
  (let* ((to-buf (ediff-get-buffer to-buf-type))
	 ;;(from-buf (if (not reg-to-copy) (ediff-get-buffer from-buf-type)))
	 (ctrl-buf ediff-control-buffer)
	 (saved-p t)
	 (three-way ediff-3way-job)
	 messg
	 ediff-verbose-p
	 reg-to-delete reg-to-delete-beg reg-to-delete-end)

    (setq reg-to-delete-beg
	  (ediff-get-diff-posn to-buf-type 'beg n ctrl-buf))
    (setq reg-to-delete-end
	  (ediff-get-diff-posn to-buf-type 'end n ctrl-buf))

    (if reg-to-copy
	(setq from-buf-type nil)
      (setq reg-to-copy (ediff-get-region-contents n from-buf-type ctrl-buf)))

    (setq reg-to-delete (ediff-get-region-contents
			 n to-buf-type ctrl-buf
			 reg-to-delete-beg reg-to-delete-end))

    (if (string= reg-to-delete reg-to-copy)
	(setq saved-p nil) ; don't copy identical buffers
      ;; seems ok to copy
      (if (or batch-invocation (ediff-test-save-region n to-buf-type))
	  (condition-case conds
	      (progn
		(ediff-with-current-buffer to-buf
		  ;; to prevent flags from interfering if buffer is writable
		  (let ((inhibit-read-only (null buffer-read-only)))

		    (goto-char reg-to-delete-end)
		    (insert reg-to-copy)

		    (if (> reg-to-delete-end reg-to-delete-beg)
			(kill-region reg-to-delete-beg reg-to-delete-end))
		    ))
		(or batch-invocation
		    (setq
		     messg
		     (ediff-save-diff-region n to-buf-type reg-to-delete))))
	    (error (message "ediff-copy-diff: %s %s"
			    (car conds)
			    (mapconcat 'prin1-to-string (cdr conds) " "))
		   (beep 1)
		   (sit-for 2) ; let the user see the error msg
		   (setq saved-p nil)
		   )))
      )

    ;; adjust state of difference in case 3-way and diff was copied ok
    (if (and saved-p three-way)
	(ediff-set-state-of-diff-in-all-buffers n ctrl-buf))

    (if batch-invocation
	(ediff-clear-fine-differences n)
      ;; If diff3 job, we should recompute fine diffs so we clear them
      ;; before reinserting flags (and thus before ediff-recenter).
      (if (and saved-p three-way)
	  (ediff-clear-fine-differences n))

      (ediff-refresh-mode-lines)

      ;; For diff2 jobs, don't recompute fine diffs, since we know there
      ;; aren't any.  So we clear diffs after ediff-recenter.
      (if (and saved-p (not three-way))
	  (ediff-clear-fine-differences n))
      ;; Make sure that the message about saving and how to restore is seen
      ;; by the user
      (message "%s" messg))
    ))

;; Save Nth diff of buffer BUF-TYPE \(A, B, or C\).
;; That is to say, the Nth diff on the `ediff-killed-diffs-alist'.  REG
;; is the region to save.  It is redundant here, but is passed anyway, for
;; convenience.
(defun ediff-save-diff-region (n buf-type reg)
  (let* ((n-th-diff-saved (assoc n ediff-killed-diffs-alist))
	 (buf (ediff-get-buffer buf-type))
	 (this-buf-n-th-diff-saved (assoc buf (cdr n-th-diff-saved))))

    (if this-buf-n-th-diff-saved
	;; either nothing saved for n-th diff and buffer or we OK'ed
	;; overriding
	(setcdr this-buf-n-th-diff-saved reg)
      (if n-th-diff-saved ;; n-th diff saved, but for another buffer
	  (nconc n-th-diff-saved  (list (cons buf reg)))
	(setq ediff-killed-diffs-alist  ;; create record for n-th diff
	      (cons (list n (cons buf reg))
		    ediff-killed-diffs-alist))))
    (message "Saving old diff region #%d of buffer %S.  To recover, type `r%s'"
	     (1+ n) buf-type
	     (if ediff-merge-job
		 "" (downcase (symbol-name buf-type))))
    ))

;; Test if saving Nth difference region of buffer BUF-TYPE is possible.
(defun ediff-test-save-region (n buf-type)
  (let* ((n-th-diff-saved (assoc n ediff-killed-diffs-alist))
	 (buf (ediff-get-buffer buf-type))
	 (this-buf-n-th-diff-saved (assoc buf (cdr n-th-diff-saved))))

    (if this-buf-n-th-diff-saved
	(if (yes-or-no-p
	     (format
	      "You've previously copied diff region %d to buffer %S.  Confirm? "
	      (1+ n) buf-type))
	    t
	  (error "Quit"))
      t)))

(defun ediff-pop-diff (n buf-type)
  "Pop last killed Nth diff region from buffer BUF-TYPE."
  (let* ((n-th-record (assoc n ediff-killed-diffs-alist))
	 (buf (ediff-get-buffer buf-type))
	 (saved-rec (assoc buf (cdr n-th-record)))
	 (three-way ediff-3way-job)
	 (ctl-buf ediff-control-buffer)
	 ediff-verbose-p
	 saved-diff reg-beg reg-end recovered)

    (if (cdr saved-rec)
	(setq saved-diff (cdr saved-rec))
      (if (> ediff-number-of-differences 0)
	  (error "Nothing saved for diff %d in buffer %S" (1+ n) buf-type)
	(error ediff-NO-DIFFERENCES)))

    (setq reg-beg (ediff-get-diff-posn buf-type 'beg n ediff-control-buffer))
    (setq reg-end (ediff-get-diff-posn buf-type 'end n ediff-control-buffer))

    (condition-case conds
	(ediff-with-current-buffer buf
	  (let ((inhibit-read-only (null buffer-read-only)))

	    (goto-char reg-end)
	    (insert saved-diff)

	    (if (> reg-end reg-beg)
		(kill-region reg-beg reg-end))

	    (setq recovered t)
	    ))
      (error (message "ediff-pop-diff: %s %s"
		      (car conds)
		      (mapconcat 'prin1-to-string (cdr conds) " "))
	     (beep 1)))

    ;; Clearing fine diffs is necessary for
    ;; ediff-unselect-and-select-difference to properly recompute them.  We
    ;; can't rely on ediff-copy-diff to clear this vector, as the user might
    ;; have modified diff regions after copying and, thus, may have recomputed
    ;; fine diffs.
    (if recovered
	(ediff-clear-fine-differences n))

    ;; adjust state of difference
    (if (and three-way recovered)
	(ediff-set-state-of-diff-in-all-buffers n ctl-buf))

    (ediff-refresh-mode-lines)

    (if recovered
	(progn
	  (setq n-th-record (delq saved-rec n-th-record))
	  (message "Diff region %d in buffer %S restored" (1+ n) buf-type)
	  ))
    ))

(defun ediff-restore-diff  (arg &optional key)
  "Restore ARGth diff from `ediff-killed-diffs-alist'.
ARG is a prefix argument.  If ARG is nil, restore the current-difference.
If the second optional argument, a character, is given, use it to
determine the target buffer instead of (ediff-last-command-char)"
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (if (numberp arg)
      (ediff-jump-to-difference arg))
  (ediff-pop-diff ediff-current-difference
		  (ediff-char-to-buftype (or key (ediff-last-command-char))))
  ;; recenter with rehighlighting, but no messages
  (let (ediff-verbose-p)
    (ediff-recenter)))

(defun ediff-restore-diff-in-merge-buffer (arg)
  "Restore ARGth diff in the merge buffer.
ARG is a prefix argument.  If nil, restore the current diff."
  (interactive "P")
  (ediff-restore-diff arg ?c))


(defun ediff-toggle-regexp-match ()
  "Toggle between focusing and hiding of difference regions that match
a regular expression typed in by the user."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((regexp-A "")
	(regexp-B "")
	(regexp-C "")
	msg-connective alt-msg-connective alt-connective)
    (cond
     ((or (and (eq ediff-skip-diff-region-function
		   ediff-focus-on-regexp-matches-function)
	       (eq (ediff-last-command-char) ?f))
	  (and (eq ediff-skip-diff-region-function
		   ediff-hide-regexp-matches-function)
	       (eq (ediff-last-command-char) ?h)))
      (message "Selective browsing by regexp turned off")
      (setq ediff-skip-diff-region-function 'ediff-show-all-diffs))
     ((eq (ediff-last-command-char) ?h)
      (setq ediff-skip-diff-region-function ediff-hide-regexp-matches-function
	    regexp-A
	    (read-string
	     (format
	      "Ignore A-regions matching this regexp (default %s): "
	      ediff-regexp-hide-A))
	    regexp-B
	    (read-string
	     (format
	      "Ignore B-regions matching this regexp (default %s): "
	      ediff-regexp-hide-B)))
      (if ediff-3way-comparison-job
	  (setq regexp-C
		(read-string
		 (format
		  "Ignore C-regions matching this regexp (default %s): "
		  ediff-regexp-hide-C))))
      (if (eq ediff-hide-regexp-connective 'and)
	  (setq msg-connective "BOTH"
		alt-msg-connective "ONE OF"
		alt-connective 'or)
	(setq msg-connective "ONE OF"
	      alt-msg-connective "BOTH"
	      alt-connective 'and))
      (if (y-or-n-p
	   (format
	    "Ignore regions that match %s regexps, OK? "
	    msg-connective))
	  (message "Will ignore regions that match %s regexps" msg-connective)
	(setq ediff-hide-regexp-connective alt-connective)
	(message "Will ignore regions that match %s regexps"
		 alt-msg-connective))

      (or (string= regexp-A "") (setq ediff-regexp-hide-A regexp-A))
      (or (string= regexp-B "") (setq ediff-regexp-hide-B regexp-B))
      (or (string= regexp-C "") (setq ediff-regexp-hide-C regexp-C)))

     ((eq (ediff-last-command-char) ?f)
      (setq ediff-skip-diff-region-function
	    ediff-focus-on-regexp-matches-function
	    regexp-A
	    (read-string
	     (format
	      "Focus on A-regions matching this regexp (default %s): "
	      ediff-regexp-focus-A))
	    regexp-B
	    (read-string
	     (format
	      "Focus on B-regions matching this regexp (default %s): "
	      ediff-regexp-focus-B)))
      (if ediff-3way-comparison-job
	  (setq regexp-C
		(read-string
		 (format
		  "Focus on C-regions matching this regexp (default %s): "
		  ediff-regexp-focus-C))))
      (if (eq ediff-focus-regexp-connective 'and)
	  (setq msg-connective "BOTH"
		alt-msg-connective "ONE OF"
		alt-connective 'or)
	(setq msg-connective "ONE OF"
	      alt-msg-connective "BOTH"
	      alt-connective 'and))
      (if (y-or-n-p
	   (format
	    "Focus on regions that match %s regexps, OK? "
	    msg-connective))
	  (message "Will focus on regions that match %s regexps"
		   msg-connective)
	(setq ediff-focus-regexp-connective alt-connective)
	(message "Will focus on regions that match %s regexps"
		 alt-msg-connective))

      (or (string= regexp-A "") (setq ediff-regexp-focus-A regexp-A))
      (or (string= regexp-B "") (setq ediff-regexp-focus-B regexp-B))
      (or (string= regexp-C "") (setq ediff-regexp-focus-C regexp-C))))))

(defun ediff-toggle-skip-similar ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (if (not (eq ediff-auto-refine 'on))
      (error
       "Can't skip over whitespace regions: first turn auto-refining on"))
  (setq ediff-ignore-similar-regions (not ediff-ignore-similar-regions))
  (if ediff-ignore-similar-regions
      (message
       "Skipping regions that differ only in white space & line breaks")
    (message "Skipping over white-space differences turned off")))

(defun ediff-focus-on-regexp-matches (n)
  "Focus on diffs that match regexp `ediff-regexp-focus-A/B'.
Regions to be ignored according to this function are those where
buf A region doesn't match `ediff-regexp-focus-A' and buf B region
doesn't match `ediff-regexp-focus-B'.
This function returns nil if the region number N (specified as
an argument) is not to be ignored and t if region N is to be ignored.

N is a region number used by Ediff internally.  It is 1 less
the number seen by the user."
  (if (ediff-valid-difference-p n)
      (let* ((ctl-buf ediff-control-buffer)
	     (regex-A ediff-regexp-focus-A)
	     (regex-B ediff-regexp-focus-B)
	     (regex-C ediff-regexp-focus-C)
	     (reg-A-match (ediff-with-current-buffer ediff-buffer-A
			    (save-restriction
			      (narrow-to-region
			       (ediff-get-diff-posn 'A 'beg n ctl-buf)
			       (ediff-get-diff-posn 'A 'end n ctl-buf))
			      (goto-char (point-min))
			      (re-search-forward regex-A nil t))))
	     (reg-B-match (ediff-with-current-buffer ediff-buffer-B
			    (save-restriction
			      (narrow-to-region
			       (ediff-get-diff-posn 'B 'beg n ctl-buf)
			       (ediff-get-diff-posn 'B 'end n ctl-buf))
			      (re-search-forward regex-B nil t))))
	     (reg-C-match (if ediff-3way-comparison-job
			      (ediff-with-current-buffer ediff-buffer-C
				(save-restriction
				  (narrow-to-region
				   (ediff-get-diff-posn 'C 'beg n ctl-buf)
				   (ediff-get-diff-posn 'C 'end n ctl-buf))
				  (re-search-forward regex-C nil t))))))
	(not (eval (if ediff-3way-comparison-job
		       (list ediff-focus-regexp-connective
			     reg-A-match reg-B-match reg-C-match)
		     (list ediff-focus-regexp-connective
			   reg-A-match reg-B-match))))
	)))

(defun ediff-hide-regexp-matches (n)
  "Hide diffs that match regexp `ediff-regexp-hide-A/B/C'.
Regions to be ignored are those where buf A region matches
`ediff-regexp-hide-A' and buf B region matches `ediff-regexp-hide-B'.
This function returns nil if the region number N (specified as
an argument) is not to be ignored and t if region N is to be ignored.

N is a region number used by Ediff internally.  It is 1 less
the number seen by the user."
  (if (ediff-valid-difference-p n)
      (let* ((ctl-buf ediff-control-buffer)
	     (regex-A ediff-regexp-hide-A)
	     (regex-B ediff-regexp-hide-B)
	     (regex-C ediff-regexp-hide-C)
	     (reg-A-match (ediff-with-current-buffer ediff-buffer-A
			    (save-restriction
			      (narrow-to-region
			       (ediff-get-diff-posn 'A 'beg n ctl-buf)
			       (ediff-get-diff-posn 'A 'end n ctl-buf))
			      (goto-char (point-min))
			      (re-search-forward regex-A nil t))))
	     (reg-B-match (ediff-with-current-buffer ediff-buffer-B
			    (save-restriction
			      (narrow-to-region
			       (ediff-get-diff-posn 'B 'beg n ctl-buf)
			       (ediff-get-diff-posn 'B 'end n ctl-buf))
			      (goto-char (point-min))
			      (re-search-forward regex-B nil t))))
	     (reg-C-match (if ediff-3way-comparison-job
			      (ediff-with-current-buffer ediff-buffer-C
				(save-restriction
				  (narrow-to-region
				   (ediff-get-diff-posn 'C 'beg n ctl-buf)
				   (ediff-get-diff-posn 'C 'end n ctl-buf))
				  (goto-char (point-min))
				  (re-search-forward regex-C nil t))))))
	(eval (if ediff-3way-comparison-job
		  (list ediff-hide-regexp-connective
			reg-A-match reg-B-match reg-C-match)
		(list ediff-hide-regexp-connective reg-A-match reg-B-match)))
	)))



;;; Quitting, suspending, etc.

(defun ediff-quit (reverse-default-keep-variants)
  "Finish an Ediff session and exit Ediff.
Unselects the selected difference, if any, restores the read-only and modified
flags of the compared file buffers, kills Ediff buffers for this session
\(but not buffers A, B, C\).

If `ediff-keep-variants' is nil, the user will be asked whether the buffers
containing the variants should be removed \(if they haven't been modified\).
If it is t, they will be preserved unconditionally.  A prefix argument,
temporarily reverses the meaning of this variable."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (let ((ctl-buf (current-buffer))
	(ctl-frm (selected-frame))
	(minibuffer-auto-raise t))
    (if (y-or-n-p (format "Quit this Ediff session%s? "
			  (if (ediff-buffer-live-p ediff-meta-buffer)
			      " & show containing session group" "")))
	(progn
	  (message "")
	  (set-buffer ctl-buf)
	  (ediff-really-quit reverse-default-keep-variants))
      (select-frame ctl-frm)
      (raise-frame ctl-frm)
      (message ""))))


;; Perform the quit operations.
(defun ediff-really-quit (reverse-default-keep-variants)
  (ediff-unhighlight-diffs-totally)
  (ediff-clear-diff-vector 'ediff-difference-vector-A 'fine-diffs-also)
  (ediff-clear-diff-vector 'ediff-difference-vector-B 'fine-diffs-also)
  (ediff-clear-diff-vector 'ediff-difference-vector-C 'fine-diffs-also)
  (ediff-clear-diff-vector 'ediff-difference-vector-Ancestor 'fine-diffs-also)

  (ediff-delete-temp-files)

  ;; Restore the visibility range.  This affects only ediff-*-regions/windows.
  ;; Since for other job names ediff-visible-region sets
  ;; ediff-visible-bounds to ediff-wide-bounds, the settings below are
  ;; ignored for such jobs.
  (if ediff-quit-widened
      (setq ediff-visible-bounds ediff-wide-bounds)
    (setq ediff-visible-bounds ediff-narrow-bounds))

  ;; Apply selective display to narrow or widen
  (ediff-visible-region)
  (mapc (lambda (overl)
	  (if (ediff-overlayp overl)
	      (ediff-delete-overlay overl)))
	ediff-wide-bounds)
  (mapc (lambda (overl)
	  (if (ediff-overlayp overl)
	      (ediff-delete-overlay overl)))
	ediff-narrow-bounds)

  ;; restore buffer mode line id's in buffer-A/B/C
  (let ((control-buffer ediff-control-buffer)
	(meta-buffer ediff-meta-buffer)
	(after-quit-hook-internal ediff-after-quit-hook-internal)
	(session-number ediff-meta-session-number)
	;; suitable working frame
	(warp-frame (if (and (ediff-window-display-p) (eq ediff-grab-mouse t))
			(cond ((window-live-p ediff-window-A)
			       (window-frame ediff-window-A))
			      ((window-live-p ediff-window-B)
			       (window-frame ediff-window-B))
			      (t (next-frame))))))
    (condition-case nil
	(ediff-with-current-buffer ediff-buffer-A
	  (setq ediff-this-buffer-ediff-sessions
		(delq control-buffer ediff-this-buffer-ediff-sessions))
	  (kill-local-variable 'mode-line-buffer-identification)
	  (kill-local-variable 'mode-line-format)
	  )
      (error))

    (condition-case nil
	(ediff-with-current-buffer ediff-buffer-B
	  (setq ediff-this-buffer-ediff-sessions
		(delq control-buffer ediff-this-buffer-ediff-sessions))
	  (kill-local-variable 'mode-line-buffer-identification)
	  (kill-local-variable 'mode-line-format)
	  )
      (error))

    (condition-case nil
	(ediff-with-current-buffer ediff-buffer-C
	  (setq ediff-this-buffer-ediff-sessions
		(delq control-buffer ediff-this-buffer-ediff-sessions))
	  (kill-local-variable 'mode-line-buffer-identification)
	  (kill-local-variable 'mode-line-format)
	  )
      (error))

    (condition-case nil
	(ediff-with-current-buffer ediff-ancestor-buffer
	  (setq ediff-this-buffer-ediff-sessions
		(delq control-buffer ediff-this-buffer-ediff-sessions))
	  (kill-local-variable 'mode-line-buffer-identification)
	  (kill-local-variable 'mode-line-format)
	  )
      (error))

  (setq ediff-session-registry
	(delq ediff-control-buffer ediff-session-registry))
  (ediff-update-registry)
  ;; restore state of buffers to what it was before ediff
  (ediff-restore-protected-variables)

  ;; If the user interrupts (canceling saving the merge buffer), continue
  ;; normally.
  (condition-case nil
      (if (ediff-merge-job)
	  (run-hooks 'ediff-quit-merge-hook))
    (quit))

  (run-hooks 'ediff-cleanup-hook)

  (ediff-janitor
   'ask
   ;; reverse-default-keep-variants is t if the user quits with a prefix arg
   (if reverse-default-keep-variants
       (not ediff-keep-variants)
     ediff-keep-variants))

  ;; one hook here is ediff-cleanup-mess, which kills the control buffer and
  ;; other auxiliary buffers. we made it into a hook to let the users do their
  ;; own cleanup, if needed.
  (run-hooks 'ediff-quit-hook)
  (ediff-update-meta-buffer meta-buffer nil session-number)

  ;; warp mouse into a working window
  (setq warp-frame  ; if mouse is over a reasonable frame, use it
	(cond ((ediff-good-frame-under-mouse))
	      (t warp-frame)))
  (if (and (ediff-window-display-p) (frame-live-p warp-frame) ediff-grab-mouse)
      (set-mouse-position (if (featurep 'emacs)
			      warp-frame
			    (frame-selected-window warp-frame))
			  2 1))

  (run-hooks 'after-quit-hook-internal)
  ))

;; Returns frame under mouse, if this frame is not a minibuffer
;; frame.  Otherwise: nil
(defun ediff-good-frame-under-mouse ()
  (let ((frame-or-win (car (mouse-position)))
	(buf-name "")
	frame obj-ok)
    (setq obj-ok
	  (if (featurep 'emacs)
	      (frame-live-p frame-or-win)
	    (window-live-p frame-or-win)))
    (if obj-ok
	(setq frame (if (featurep 'emacs) frame-or-win (window-frame frame-or-win))
	      buf-name
	      (buffer-name (window-buffer (frame-selected-window frame)))))
    (if (string-match "Minibuf" buf-name)
	nil
      frame)))


(defun ediff-delete-temp-files ()
  (if (and (stringp ediff-temp-file-A) (file-exists-p ediff-temp-file-A))
      (delete-file ediff-temp-file-A))
  (if (and (stringp ediff-temp-file-B) (file-exists-p ediff-temp-file-B))
      (delete-file ediff-temp-file-B))
  (if (and (stringp ediff-temp-file-C) (file-exists-p ediff-temp-file-C))
      (delete-file ediff-temp-file-C)))


;; Kill control buffer, other auxiliary Ediff buffers.
;; Leave one of the frames split between buffers A/B/C
(defun ediff-cleanup-mess ()
  (let* ((buff-A ediff-buffer-A)
	 (buff-B ediff-buffer-B)
	 (buff-C ediff-buffer-C)
	 (ctl-buf  ediff-control-buffer)
	 (ctl-wind  (ediff-get-visible-buffer-window ctl-buf))
	 (ctl-frame ediff-control-frame)
	 (three-way-job ediff-3way-job)
	 (main-frame (cond ((window-live-p ediff-window-A)
			    (window-frame ediff-window-A))
			   ((window-live-p ediff-window-B)
			    (window-frame ediff-window-B)))))

    (ediff-kill-buffer-carefully ediff-diff-buffer)
    (ediff-kill-buffer-carefully ediff-custom-diff-buffer)
    (ediff-kill-buffer-carefully ediff-fine-diff-buffer)
    (ediff-kill-buffer-carefully ediff-tmp-buffer)
    (ediff-kill-buffer-carefully ediff-error-buffer)
    (ediff-kill-buffer-carefully ediff-msg-buffer)
    (ediff-kill-buffer-carefully ediff-debug-buffer)
    (if (boundp 'ediff-patch-diagnostics)
	(ediff-kill-buffer-carefully ediff-patch-diagnostics))

    ;; delete control frame or window
    (cond ((and (ediff-window-display-p) (frame-live-p ctl-frame))
	   (delete-frame ctl-frame))
	  ((window-live-p ctl-wind)
	   (delete-window ctl-wind)))

    ;; Hide bottom toolbar.  --marcpa
    (if (not (ediff-multiframe-setup-p))
	(ediff-kill-bottom-toolbar))

    (ediff-kill-buffer-carefully ctl-buf)

    (if (frame-live-p main-frame)
	(select-frame main-frame))

    ;; display only if not visible
    (condition-case nil
	(or (ediff-get-visible-buffer-window buff-B)
	    (switch-to-buffer buff-B))
      (error))
    (condition-case nil
	(or (ediff-get-visible-buffer-window buff-A)
	    (progn
	      (if (and (ediff-get-visible-buffer-window buff-B)
		       (ediff-buffer-live-p buff-A))
		  (funcall ediff-split-window-function))
	      (switch-to-buffer buff-A)))
      (error))
    (if three-way-job
	(condition-case nil
	    (or (ediff-get-visible-buffer-window buff-C)
		(progn
		  (if (and (or (ediff-get-visible-buffer-window buff-A)
			       (ediff-get-visible-buffer-window buff-B))
			   (ediff-buffer-live-p buff-C))
		      (funcall ediff-split-window-function))
		  (switch-to-buffer buff-C)))
	  (error)))
    (balance-windows)
    (message "")
    ))

(defun ediff-janitor (ask keep-variants)
  "Kill buffers A, B, and, possibly, C, if these buffers aren't modified.
In merge jobs, buffer C is not deleted here, but rather according to
ediff-quit-merge-hook.
A side effect of cleaning up may be that you should be careful when comparing
the same buffer in two separate Ediff sessions: quitting one of them might
delete this buffer in another session as well."
  (ediff-dispose-of-variant-according-to-user
   ediff-buffer-A 'A ask keep-variants)
  (ediff-dispose-of-variant-according-to-user
   ediff-buffer-B 'B ask keep-variants)
  (if ediff-merge-job  ; don't del buf C if merging--del ancestor buf instead
      (ediff-dispose-of-variant-according-to-user
       ediff-ancestor-buffer 'Ancestor ask keep-variants)
    (ediff-dispose-of-variant-according-to-user
     ediff-buffer-C 'C ask keep-variants)
    ))

;; Kill the variant buffer, according to user directives (ask, kill
;; unconditionally, keep)
;; BUFF is the buffer, BUFF-TYPE is either 'A, or 'B, 'C, 'Ancestor
(defun ediff-dispose-of-variant-according-to-user (buff bufftype ask keep-variants)
  ;; if this is indirect buffer, kill it and substitute with direct buf
  (if (and (ediff-buffer-live-p buff)
	   (ediff-with-current-buffer buff ediff-temp-indirect-buffer))
      (let ((wind (ediff-get-visible-buffer-window buff))
	    (base (buffer-base-buffer buff))
	    (modified-p (buffer-modified-p buff)))
	(if (and (window-live-p wind) (ediff-buffer-live-p base))
	    (set-window-buffer wind base))
	;; Kill indirect buffer even if it is modified, because the base buffer
	;; is still there. Note that if the base buffer is dead then so will be
	;; the indirect buffer
	(ediff-with-current-buffer buff
	  (set-buffer-modified-p nil))
	(ediff-kill-buffer-carefully buff)
	(ediff-with-current-buffer base
	  (set-buffer-modified-p modified-p)))
    ;; otherwise, ask or use the value of keep-variants
    (or (not (ediff-buffer-live-p buff))
	keep-variants
	(buffer-modified-p buff)
	(and ask
	     (not (y-or-n-p (format "Kill buffer %S [%s]? "
				    bufftype (buffer-name buff)))))
	(ediff-kill-buffer-carefully buff))
    ))

(defun ediff-maybe-save-and-delete-merge (&optional save-and-continue)
  "Default hook to run on quitting a merge job.
This can also be used to save merge buffer in the middle of an Ediff session.

If the optional SAVE-AND-CONTINUE argument is non-nil, save merge buffer and
continue.  Otherwise:
If `ediff-autostore-merges' is nil, this does nothing.
If it is t, it saves the merge buffer in the file `ediff-merge-store-file'
or asks the user, if the latter is nil.  It then asks the user whether to
delete the merge buffer.
If `ediff-autostore-merges' is neither nil nor t, the merge buffer is saved
only if this merge job is part of a group, i.e., was invoked from within
`ediff-merge-directories', `ediff-merge-directory-revisions', and such."
  (let ((merge-store-file ediff-merge-store-file)
	(ediff-autostore-merges ; fake ediff-autostore-merges, if necessary
	 (if save-and-continue t ediff-autostore-merges)))
    (if ediff-autostore-merges
	(cond ((stringp merge-store-file)
	       ;; store, ask to delete
	       (ediff-write-merge-buffer-and-maybe-kill
		ediff-buffer-C merge-store-file 'show-file save-and-continue))
	      ((eq ediff-autostore-merges t)
	       ;; ask for file name
	       (setq merge-store-file
		     (read-file-name "Save the result of the merge in file: "))
	       (ediff-write-merge-buffer-and-maybe-kill
		ediff-buffer-C merge-store-file nil save-and-continue))
	      ((and (ediff-buffer-live-p ediff-meta-buffer)
		    (ediff-with-current-buffer ediff-meta-buffer
		      (ediff-merge-metajob)))
	       ;; The parent metajob passed nil as the autostore file.
	       nil)))
    ))

;; write merge buffer.  If the optional argument save-and-continue is non-nil,
;; then don't kill the merge buffer
(defun ediff-write-merge-buffer-and-maybe-kill (buf file
					       &optional
					       show-file save-and-continue)
  (if (not (eq (find-buffer-visiting file) buf))
      (let ((warn-message
	     (format "Another buffer is visiting file %s. Too dangerous to save the merge buffer"
		     file)))
	(beep)
	(message "%s" warn-message)
	(with-output-to-temp-buffer ediff-msg-buffer
	  (princ "\n\n")
	  (princ warn-message)
	  (princ "\n\n")
	  )
	(sit-for 2))
    (ediff-with-current-buffer buf
      (if (or (not (file-exists-p file))
	      (y-or-n-p (format "File %s exists, overwrite? " file)))
	  (progn
	    ;;(write-region nil nil file)
	    (ediff-with-current-buffer buf
	      (set-visited-file-name file)
	      (save-buffer))
	    (if show-file
		(progn
		  (message "Merge buffer saved in: %s" file)
		  (set-buffer-modified-p nil)
		  (sit-for 3)))
	    (if (and
		 (not save-and-continue)
		 (y-or-n-p "Merge buffer saved.  Now kill the buffer? "))
		(ediff-kill-buffer-carefully buf)))))
    ))

;; The default way of suspending Ediff.
;; Buries Ediff buffers, kills all windows.
(defun ediff-default-suspend-function ()
  (let* ((buf-A ediff-buffer-A)
	 (buf-B ediff-buffer-B)
	 (buf-C ediff-buffer-C)
	 (buf-A-wind (ediff-get-visible-buffer-window buf-A))
	 (buf-B-wind (ediff-get-visible-buffer-window buf-B))
	 (buf-C-wind (ediff-get-visible-buffer-window buf-C))
	 (buf-patch  (if (boundp 'ediff-patchbufer) ediff-patchbufer nil))
	 (buf-patch-diag (if (boundp 'ediff-patch-diagnostics)
			     ediff-patch-diagnostics nil))
	 (buf-err  ediff-error-buffer)
	 (buf-diff ediff-diff-buffer)
	 (buf-custom-diff ediff-custom-diff-buffer)
	 (buf-fine-diff ediff-fine-diff-buffer))

    ;; hide the control panel
    (if (and (ediff-window-display-p) (frame-live-p ediff-control-frame))
	(iconify-frame ediff-control-frame)
      (bury-buffer))
    (if buf-err (bury-buffer buf-err))
    (if buf-diff (bury-buffer buf-diff))
    (if buf-custom-diff (bury-buffer buf-custom-diff))
    (if buf-fine-diff (bury-buffer buf-fine-diff))
    (if buf-patch (bury-buffer buf-patch))
    (if buf-patch-diag (bury-buffer buf-patch-diag))
    (if (window-live-p buf-A-wind)
	(progn
	  (select-window buf-A-wind)
	  (delete-other-windows)
	  (bury-buffer))
      (if (ediff-buffer-live-p buf-A)
	  (progn
	    (set-buffer buf-A)
	    (bury-buffer))))
    (if (window-live-p buf-B-wind)
	(progn
	  (select-window buf-B-wind)
	  (delete-other-windows)
	  (bury-buffer))
      (if (ediff-buffer-live-p buf-B)
	  (progn
	    (set-buffer buf-B)
	    (bury-buffer))))
    (if (window-live-p buf-C-wind)
	(progn
	  (select-window buf-C-wind)
	  (delete-other-windows)
	  (bury-buffer))
      (if (ediff-buffer-live-p buf-C)
	  (progn
	    (set-buffer buf-C)
	    (bury-buffer))))
    ))


(defun ediff-suspend ()
  "Suspend Ediff.
To resume, switch to the appropriate `Ediff Control Panel'
buffer and then type \\[ediff-recenter].  Ediff will automatically set
up an appropriate window config."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (run-hooks 'ediff-suspend-hook)
  (message
   "To resume, type M-x eregistry and select the desired Ediff session"))

;; ediff-barf-if-not-control-buffer ensures only called from ediff.
(declare-function ediff-version "ediff" ())

(defun ediff-status-info ()
  "Show the names of the buffers or files being operated on by Ediff.
Hit \\[ediff-recenter] to reset the windows afterward."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (save-excursion
    (ediff-skip-unsuitable-frames))
  (with-output-to-temp-buffer ediff-msg-buffer
    (ediff-with-current-buffer standard-output
      (fundamental-mode))
    (raise-frame (selected-frame))
    (princ (ediff-version))
    (princ "\n\n")
    (ediff-with-current-buffer ediff-buffer-A
      (if buffer-file-name
	  (princ
	   (format "File A = %S\n" buffer-file-name))
	(princ
	 (format "Buffer A = %S\n" (buffer-name)))))
    (ediff-with-current-buffer ediff-buffer-B
      (if buffer-file-name
	  (princ
	   (format "File B = %S\n" buffer-file-name))
	(princ
	 (format "Buffer B = %S\n" (buffer-name)))))
    (if ediff-3way-job
	(ediff-with-current-buffer ediff-buffer-C
	  (if buffer-file-name
	      (princ
	       (format "File C = %S\n" buffer-file-name))
	    (princ
	     (format "Buffer C = %S\n" (buffer-name))))))
    (princ (format "Customized diff output %s\n"
		   (if (ediff-buffer-live-p ediff-custom-diff-buffer)
		       (concat "\tin buffer "
			       (buffer-name ediff-custom-diff-buffer))
		     " is not available")))
    (princ (format "Plain diff output %s\n"
		   (if (ediff-buffer-live-p ediff-diff-buffer)
		       (concat "\tin buffer "
			       (buffer-name ediff-diff-buffer))
		     " is not available")))

    (let* ((A-line (ediff-with-current-buffer ediff-buffer-A
		     (1+ (count-lines (point-min) (point)))))
	   (B-line (ediff-with-current-buffer ediff-buffer-B
		     (1+ (count-lines (point-min) (point)))))
	   C-line)
      (princ (format "\Buffer A's point is on line %d\n" A-line))
      (princ (format "Buffer B's point is on line %d\n" B-line))
      (if ediff-3way-job
	  (progn
	    (setq C-line (ediff-with-current-buffer ediff-buffer-C
			   (1+ (count-lines (point-min) (point)))))
	    (princ (format "Buffer C's point is on line %d\n" C-line)))))

    (princ (format "\nCurrent difference number = %S\n"
		   (cond ((< ediff-current-difference 0) 'start)
			 ((>= ediff-current-difference
			      ediff-number-of-differences) 'end)
			 (t (1+ ediff-current-difference)))))

    (princ
     (format "\n%s regions that differ in white space & line breaks only"
	     (if ediff-ignore-similar-regions
		 "Ignoring" "Showing")))
    (if (and ediff-merge-job ediff-show-clashes-only)
	(princ
	 "\nFocusing on regions where both buffers differ from the ancestor"))
    (if (and ediff-skip-merge-regions-that-differ-from-default ediff-merge-job)
	(princ
	 "\nSkipping merge regions that differ from default setting"))

    (cond ((eq ediff-skip-diff-region-function 'ediff-show-all-diffs)
	   (princ "\nSelective browsing by regexp is off\n"))
	  ((eq ediff-skip-diff-region-function
	       ediff-hide-regexp-matches-function)
	   (princ
	    "\nIgnoring regions that match")
	   (princ
	    (format
	     "\n\t regexp `%s' in buffer A  %S\n\t regexp `%s' in buffer B\n"
	     ediff-regexp-hide-A ediff-hide-regexp-connective
	     ediff-regexp-hide-B)))
	  ((eq ediff-skip-diff-region-function
	       ediff-focus-on-regexp-matches-function)
	   (princ
	    "\nFocusing on regions that match")
	   (princ
	    (format
	     "\n\t regexp `%s' in buffer A  %S\n\t regexp `%s' in buffer B\n"
	     ediff-regexp-focus-A ediff-focus-regexp-connective
	     ediff-regexp-focus-B)))
	  (t (princ "\nSelective browsing via a user-defined method.\n")))

    (princ
     (format "\nBugs/suggestions: type `%s' while in Ediff Control Panel."
	     (substitute-command-keys "\\[ediff-submit-report]")))
    ) ; with output
  (if (frame-live-p ediff-control-frame)
      (ediff-reset-mouse ediff-control-frame))
  (if (window-live-p ediff-control-window)
      (select-window ediff-control-window)))




;;; Support routines

;; Select a difference by placing the ASCII flags around the appropriate
;; group of lines in the A, B buffers
;; This may have to be modified for buffer C, when it will be supported.
(defun ediff-select-difference (n)
  (if (and (ediff-buffer-live-p ediff-buffer-A)
	   (ediff-buffer-live-p ediff-buffer-B)
	   (ediff-valid-difference-p n))
      (progn
	(cond
	    ((and (ediff-has-face-support-p) ediff-use-faces)
	       (ediff-highlight-diff n))
	    ((eq ediff-highlighting-style 'ascii)
	     (ediff-place-flags-in-buffer
	      'A ediff-buffer-A ediff-control-buffer n)
	     (ediff-place-flags-in-buffer
	      'B ediff-buffer-B ediff-control-buffer n)
	     (if ediff-3way-job
		 (ediff-place-flags-in-buffer
		  'C ediff-buffer-C ediff-control-buffer n))
	     (if (ediff-buffer-live-p ediff-ancestor-buffer)
		 (ediff-place-flags-in-buffer
		  'Ancestor ediff-ancestor-buffer
		  ediff-control-buffer n))
	     ))

	(ediff-install-fine-diff-if-necessary n)
	;; set current difference here so the hook will be able to refer to it
	(setq ediff-current-difference n)
	(run-hooks 'ediff-select-hook))))


;; Unselect a difference by removing the ASCII flags in the buffers.
;; This may have to be modified for buffer C, when it will be supported.
(defun ediff-unselect-difference (n)
  (if (ediff-valid-difference-p n)
      (progn
	(cond ((and (ediff-has-face-support-p) ediff-use-faces)
	       (ediff-unhighlight-diff))
	      ((eq ediff-highlighting-style 'ascii)
	       (ediff-remove-flags-from-buffer
		ediff-buffer-A
		(ediff-get-diff-overlay n 'A))
	       (ediff-remove-flags-from-buffer
		ediff-buffer-B
		(ediff-get-diff-overlay n 'B))
	       (if ediff-3way-job
		   (ediff-remove-flags-from-buffer
		    ediff-buffer-C
		    (ediff-get-diff-overlay n 'C)))
	       (if (ediff-buffer-live-p ediff-ancestor-buffer)
		   (ediff-remove-flags-from-buffer
		    ediff-ancestor-buffer
		    (ediff-get-diff-overlay n 'Ancestor)))
	       ))

	;; unhighlight fine diffs
	(ediff-set-fine-diff-properties ediff-current-difference 'default)
	(run-hooks 'ediff-unselect-hook))))


;; Unselects prev diff and selects a new one, if FLAG has value other than
;; 'select-only or 'unselect-only.  If FLAG is 'select-only, the
;; next difference is selected, but the current selection is not
;; unselected.  If FLAG is 'unselect-only then the current selection is
;; unselected, but the next one is not selected.  If NO-RECENTER is non-nil,
;; don't recenter buffers after selecting/unselecting.
(defun ediff-unselect-and-select-difference (n &optional flag no-recenter)
  (let ((ediff-current-difference n))
    (or no-recenter
	(ediff-recenter 'no-rehighlight)))

  (let ((control-buf ediff-control-buffer))
    (unwind-protect
	(progn
	  (or (eq flag 'select-only)
	      (ediff-unselect-difference ediff-current-difference))

	  (or (eq flag 'unselect-only)
	      (ediff-select-difference n))
	  ;; need to set current diff here even though it is also set in
	  ;; ediff-select-difference because ediff-select-difference might not
	  ;; be called if unselect-only is specified
	  (setq ediff-current-difference n)
	  ) ; end protected section

      (ediff-with-current-buffer control-buf (ediff-refresh-mode-lines)))
    ))



(defun ediff-highlight-diff-in-one-buffer (n buf-type)
  (if (ediff-buffer-live-p (ediff-get-buffer buf-type))
      (let* ((buff (ediff-get-buffer buf-type))
	     (last (ediff-with-current-buffer buff (point-max)))
	     (begin (ediff-get-diff-posn buf-type 'beg n))
	     (end (ediff-get-diff-posn buf-type 'end n))
	     (xtra (if (equal begin end) 1 0))
	     (end-hilit (min last (+ end xtra)))
	     (current-diff-overlay
	      (symbol-value
	       (ediff-get-symbol-from-alist
		buf-type ediff-current-diff-overlay-alist))))

	(if (featurep 'xemacs)
	    (ediff-move-overlay current-diff-overlay begin end-hilit)
	  (ediff-move-overlay current-diff-overlay begin end-hilit buff))
	(ediff-overlay-put current-diff-overlay 'priority
			   (ediff-highest-priority begin end-hilit buff))
	(ediff-overlay-put current-diff-overlay 'ediff-diff-num n)

	;; unhighlight the background overlay for diff n so it won't
	;; interfere with the current diff overlay
	(ediff-set-overlay-face (ediff-get-diff-overlay n buf-type) nil)
	)))


(defun ediff-unhighlight-diff-in-one-buffer (buf-type)
  (if (ediff-buffer-live-p (ediff-get-buffer buf-type))
      (let ((current-diff-overlay
	     (symbol-value
	      (ediff-get-symbol-from-alist
	       buf-type ediff-current-diff-overlay-alist)))
	    (overlay
	     (ediff-get-diff-overlay ediff-current-difference buf-type))
	    )

	(ediff-move-overlay current-diff-overlay 1 1)

	;; rehighlight the overlay in the background of the
	;; current difference region
	(ediff-set-overlay-face
	 overlay
	 (if (and (ediff-has-face-support-p)
		  ediff-use-faces ediff-highlight-all-diffs)
	     (ediff-background-face buf-type ediff-current-difference)))
	)))

(defun ediff-unhighlight-diffs-totally-in-one-buffer (buf-type)
  (ediff-unselect-and-select-difference -1)
  (if (and (ediff-has-face-support-p) ediff-use-faces)
      (let* ((inhibit-quit t)
	     (current-diff-overlay-var
	      (ediff-get-symbol-from-alist
	       buf-type ediff-current-diff-overlay-alist))
	     (current-diff-overlay (symbol-value current-diff-overlay-var)))
	(ediff-paint-background-regions 'unhighlight)
	(if (ediff-overlayp current-diff-overlay)
	    (ediff-delete-overlay current-diff-overlay))
	(set current-diff-overlay-var nil)
	)))


(defun ediff-highlight-diff (n)
  "Put face on diff N.  Invoked for X displays only."
  (ediff-highlight-diff-in-one-buffer n 'A)
  (ediff-highlight-diff-in-one-buffer n 'B)
  (ediff-highlight-diff-in-one-buffer n 'C)
  (ediff-highlight-diff-in-one-buffer n 'Ancestor)
  )


(defun ediff-unhighlight-diff ()
  "Remove overlays from buffers A, B, and C."
  (ediff-unhighlight-diff-in-one-buffer 'A)
  (ediff-unhighlight-diff-in-one-buffer 'B)
  (ediff-unhighlight-diff-in-one-buffer 'C)
  (ediff-unhighlight-diff-in-one-buffer 'Ancestor)
  )

;; delete highlighting overlays, restore faces to their original form
(defun ediff-unhighlight-diffs-totally ()
  (ediff-unhighlight-diffs-totally-in-one-buffer 'A)
  (ediff-unhighlight-diffs-totally-in-one-buffer 'B)
  (ediff-unhighlight-diffs-totally-in-one-buffer 'C)
  (ediff-unhighlight-diffs-totally-in-one-buffer 'Ancestor)
  )


;; for compatibility
(defmacro ediff-minibuffer-with-setup-hook (fun &rest body)
  `(if (fboundp 'minibuffer-with-setup-hook)
       (minibuffer-with-setup-hook ,fun ,@body)
     ,@body))

;; This is adapted from a similar function in `emerge.el'.
;; PROMPT should not have a trailing ': ', so that it can be modified
;; according to context.
;; If DEFAULT-FILE is set, it should be used as the default value.
;; If DEFAULT-DIR is non-nil, use it as the default directory.
;; Otherwise, use the value of `default-directory.'
(defun ediff-read-file-name (prompt default-dir default-file &optional no-dirs)
  ;; hack default-dir if it is not set
  (setq default-dir
	(file-name-as-directory
	 (ediff-abbreviate-file-name
	  (expand-file-name (or default-dir
				(and default-file
				     (file-name-directory default-file))
				default-directory)))))

  ;; strip the directory from default-file
  (if default-file
      (setq default-file (file-name-nondirectory default-file)))
  (if (string= default-file "")
      (setq default-file nil))

  (let ((defaults (and (fboundp 'dired-dwim-target-defaults)
		       (dired-dwim-target-defaults
			(and default-file (list default-file))
			default-dir)))
	f)
    (setq f (ediff-minibuffer-with-setup-hook
		(lambda () (when defaults
			     (setq minibuffer-default defaults)))
	      (read-file-name
	       (format "%s%s "
		       prompt
		       (cond (default-file
			       (concat " (default " default-file "):"))
			     (t (concat " (default " default-dir "):"))))
	       default-dir
	       (or default-file default-dir)
	       t			; must match, no-confirm
	       (if default-file (file-name-directory default-file)))))
    (setq f (expand-file-name f default-dir))
    ;; If user entered a directory name, expand the default file in that
    ;; directory.  This allows the user to enter a directory name for the
    ;; B-file and diff against the default-file in that directory instead
    ;; of a DIRED listing!
    (if (and (file-directory-p f) default-file)
	(setq f (expand-file-name
		 (file-name-nondirectory default-file) f)))
    (if (and no-dirs (file-directory-p f))
	(error "File %s is a directory" f))
    f))

;; If PREFIX is given, then it is used as a prefix for the temp file
;; name.  Otherwise, `ediff' is used.  If FILE is given, use this
;; file and don't create a new one.
;; In MS-DOS, make sure the prefix isn't too long, or else
;; `make-temp-name' isn't guaranteed to return a unique filename.
;; Also, save buffer from START to END in the file.
;; START defaults to (point-min), END to (point-max)
(defun ediff-make-temp-file (buff &optional prefix given-file start end)
  (let* ((p (ediff-convert-standard-filename (or prefix "ediff")))
	 (short-p p)
	 (coding-system-for-write ediff-coding-system-for-write)
	 f short-f)
    (if (and (fboundp 'msdos-long-file-names)
	     (not (msdos-long-file-names))
	     (> (length p) 2))
	(setq short-p (substring p 0 2)))

    (setq f (concat ediff-temp-file-prefix p)
	  short-f (concat ediff-temp-file-prefix short-p)
  	  f (cond (given-file)
		  ((find-file-name-handler f 'insert-file-contents)
		   ;; to thwart file handlers in write-region, e.g., if file
		   ;; name ends with .Z or .gz
		   ;; This is needed so that patches produced by ediff will
		   ;; have more meaningful names
		   (ediff-make-empty-tmp-file short-f))
		  (prefix
		   ;; Prefix is most often the same as the file name for the
		   ;; variant.  Here we are trying to use the original file
		   ;; name but in the temp directory.
		   (ediff-make-empty-tmp-file f 'keep-name))
		  (t
		   ;; If don't care about name, add some random stuff
		   ;; to proposed file name.
		   (ediff-make-empty-tmp-file short-f))))

    ;; create the file
    (ediff-with-current-buffer buff
      (write-region (if start start (point-min))
		    (if end end (point-max))
		    f
		    nil          ; don't append---erase
		    'no-message)
      (set-file-modes f ediff-temp-file-mode)
      (expand-file-name f))))

;; Create a temporary file.
;; The returned file name (created by appending some random characters at the
;; end of PROPOSED-NAME is guaranteed to point to a newly created empty file.
;; This is a replacement for make-temp-name, which eliminates a security hole.
;; If KEEP-PROPOSED-NAME isn't nil, try to keep PROPOSED-NAME, unless such file
;; already exists.
;; It is a modified version of make-temp-file in emacs 20.5
(defun ediff-make-empty-tmp-file (proposed-name &optional keep-proposed-name)
  (let ((file proposed-name))
    (while (condition-case ()
               (progn
		 (if (or (file-exists-p file) (not keep-proposed-name))
		     (setq file (make-temp-name proposed-name)))
		 ;; the with-temp-buffer thing is a workaround for an XEmacs
		 ;; bug: write-region complains that we are trying to visit a
		 ;; file in an indirect buffer, failing to notice that the
		 ;; VISIT flag is unset and that we are actually writing from a
		 ;; string and not from any buffer.
		 (with-temp-buffer
		   (write-region "" nil file nil 'silent nil 'excl))
                 nil)
            (file-already-exists t))
      ;; the file was somehow created by someone else between
      ;; `make-temp-name' and `write-region', let's try again.
      nil)
    file))


;; Quote metacharacters (using \) when executing diff in Unix, but not in
;; EMX OS/2
;;(defun ediff-protect-metachars (str)
;;  (or (memq system-type '(emx))
;;      (let ((limit 0))
;;	(while (string-match ediff-metachars str limit)
;;	  (setq str (concat (substring str 0 (match-beginning 0))
;;			    "\\"
;;			    (substring str (match-beginning 0))))
;;	  (setq limit (1+ (match-end 0))))))
;;  str)

;; Make sure the current buffer (for a file) has the same contents as the
;; file on disk, and attempt to remedy the situation if not.
;; Signal an error if we can't make them the same, or the user doesn't want
;; to do what is necessary to make them the same.
;; Also, Ediff always offers to revert obsolete buffers, whether they
;; are modified or not.
(defun ediff-verify-file-buffer (&optional file-magic)
  ;; First check if the file has been modified since the buffer visited it.
  (if (verify-visited-file-modtime (current-buffer))
      (if (buffer-modified-p)
	  ;; If buffer is not obsolete and is modified, offer to save
	  (if (yes-or-no-p
	       (format "Buffer %s has been modified. Save it in file %s? "
		       (buffer-name)
		       buffer-file-name))
	      (condition-case nil
		  (save-buffer)
		(error
		 (beep)
		 (message "Couldn't save %s" buffer-file-name)))
	    (error "Buffer is out of sync for file %s" buffer-file-name))
	;; If buffer is not obsolete and is not modified, do nothing
	nil)
    ;; If buffer is obsolete, offer to revert
    (if (yes-or-no-p
	 (format "File %s was modified since visited by buffer %s.  REVERT file %s? "
		 buffer-file-name
		 (buffer-name)
		 buffer-file-name))
	(progn
	  (if file-magic
	      (erase-buffer))
	  (revert-buffer t t))
      (error "Buffer out of sync for file %s" buffer-file-name))))

;; if there is another buffer visiting the file of the merge buffer, offer to
;; save and delete the buffer; else bark
(defun ediff-verify-file-merge-buffer (file)
  (let ((buff (if (stringp file) (find-buffer-visiting file)))
	warn-message)
    (or (null buff)
	(progn
	  (setq warn-message
		(format "Buffer %s is visiting %s. Save and kill the buffer? "
			(buffer-name buff) file))
	  (with-output-to-temp-buffer ediff-msg-buffer
	    (princ "\n\n")
	    (princ warn-message)
	    (princ "\n\n"))
	  (if (y-or-n-p
	       (message "%s" warn-message))
	      (with-current-buffer buff
		(save-buffer)
		(kill-buffer (current-buffer)))
	    (error "Too dangerous to merge versions of a file visited by another buffer"))))
    ))



(defun ediff-filename-magic-p (file)
  (or (ediff-file-compressed-p file)
      (ediff-file-remote-p file)))


(defun ediff-save-buffer (arg)
  "Safe way of saving buffers A, B, C, and the diff output.
`wa' saves buffer A, `wb' saves buffer B, `wc' saves buffer C,
and `wd' saves the diff output.

With prefix argument, `wd' saves plain diff output.
Without an argument, it saves customized diff argument, if available
\(and plain output, if customized output was not generated\)."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (ediff-compute-custom-diffs-maybe)
  (ediff-with-current-buffer
      (cond ((memq (ediff-last-command-char) '(?a ?b ?c))
	     (ediff-get-buffer
	      (ediff-char-to-buftype (ediff-last-command-char))))
	    ((eq (ediff-last-command-char) ?d)
	     (message "Saving diff output ...")
	     (sit-for 1) ; let the user see the message
	     (cond ((and arg (ediff-buffer-live-p ediff-diff-buffer))
		    ediff-diff-buffer)
		   ((ediff-buffer-live-p ediff-custom-diff-buffer)
		    ediff-custom-diff-buffer)
		   ((ediff-buffer-live-p ediff-diff-buffer)
		    ediff-diff-buffer)
		   (t (error "Output from `diff' not found"))))
	    )
    (let ((window-min-height 2))
      (save-buffer))))


;; idea suggested by Hannu Koivisto <azure@iki.fi>
(defun ediff-clone-buffer-for-region-comparison (buff region-name)
  (let ((cloned-buff (ediff-make-cloned-buffer buff region-name))
	(pop-up-windows t)
	wind
	other-wind
	msg-buf)
    (ediff-with-current-buffer cloned-buff
      (setq ediff-temp-indirect-buffer t))
    (pop-to-buffer cloned-buff)
    (setq wind (ediff-get-visible-buffer-window cloned-buff))
    (select-window wind)
    (delete-other-windows)
    (ediff-activate-mark)
    (split-window-vertically)
    (ediff-select-lowest-window)
    (setq other-wind (selected-window))
    (with-temp-buffer
      (erase-buffer)
      (insert
       (format "\n   *******  Mark a region in buffer %s (or confirm the existing one)  *******\n"
	       (buffer-name cloned-buff)))
      (insert
       (ediff-with-current-buffer buff
	 (format "\n\t      When done, type %s       Use %s to abort\n    "
		 (ediff-format-bindings-of 'exit-recursive-edit)
		 (ediff-format-bindings-of 'abort-recursive-edit))))
      (goto-char (point-min))
      (setq msg-buf (current-buffer))
      (set-window-buffer other-wind msg-buf)
      (shrink-window-if-larger-than-buffer)
      (if (window-live-p wind)
	  (select-window wind))
      (condition-case nil
	  (recursive-edit)
	(quit
	 (ediff-kill-buffer-carefully cloned-buff)))
      )
    cloned-buff))


(defun ediff-clone-buffer-for-window-comparison (buff wind region-name)
  (let ((cloned-buff (ediff-make-cloned-buffer buff region-name)))
    (ediff-with-current-buffer cloned-buff
      (setq ediff-temp-indirect-buffer t))
    (set-window-buffer wind cloned-buff)
    cloned-buff))

(defun ediff-clone-buffer-for-current-diff-comparison (buff buf-type reg-name)
  (let ((cloned-buff (ediff-make-cloned-buffer buff reg-name))
	(reg-start (ediff-get-diff-posn buf-type 'beg))
	(reg-end (ediff-get-diff-posn buf-type 'end)))
    (ediff-with-current-buffer cloned-buff
      ;; set region to be the current diff region
      (goto-char reg-start)
      (set-mark reg-end)
      (setq ediff-temp-indirect-buffer t))
    cloned-buff))



(defun ediff-make-cloned-buffer (buff region-name)
  (ediff-make-indirect-buffer
   buff (generate-new-buffer-name
         (concat (if (stringp buff) buff (buffer-name buff)) region-name))))


(defun ediff-make-indirect-buffer (base-buf indirect-buf-name)
  (if (featurep 'xemacs)
      (make-indirect-buffer base-buf indirect-buf-name)
    (make-indirect-buffer base-buf indirect-buf-name 'clone)))


;; This function operates only from an ediff control buffer
(defun ediff-compute-custom-diffs-maybe ()
  (let ((buf-A-file-name (buffer-file-name ediff-buffer-A))
	(buf-B-file-name (buffer-file-name ediff-buffer-B))
	file-A file-B)
    (unless (and buf-A-file-name
		 (file-exists-p buf-A-file-name)
		 (not (ediff-file-remote-p buf-A-file-name)))
      (setq file-A (ediff-make-temp-file ediff-buffer-A)))
    (unless (and buf-B-file-name
		 (file-exists-p buf-B-file-name)
		 (not (ediff-file-remote-p buf-B-file-name)))
      (setq file-B (ediff-make-temp-file ediff-buffer-B)))
    (or (ediff-buffer-live-p ediff-custom-diff-buffer)
	(setq ediff-custom-diff-buffer
	      (get-buffer-create
	       (ediff-unique-buffer-name "*ediff-custom-diff" "*"))))
    (ediff-with-current-buffer ediff-custom-diff-buffer
			       (setq buffer-read-only nil)
			       (erase-buffer))
    (ediff-exec-process
     ediff-custom-diff-program ediff-custom-diff-buffer 'synchronize
     ediff-custom-diff-options
     (or file-A buf-A-file-name)
     (or file-B buf-B-file-name))
    ;; put the diff file in diff-mode, if it is available
    (if (fboundp 'diff-mode)
	(with-current-buffer ediff-custom-diff-buffer
	  (diff-mode)))
    (and file-A (file-exists-p file-A) (delete-file file-A))
    (and file-B (file-exists-p file-B) (delete-file file-B))
    ))

(defun ediff-show-diff-output (arg)
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (ediff-compute-custom-diffs-maybe)
  (save-excursion
    (ediff-skip-unsuitable-frames ' ok-unsplittable))
  (let ((buf (cond ((and arg (ediff-buffer-live-p ediff-diff-buffer))
		    ediff-diff-buffer)
		   ((ediff-buffer-live-p ediff-custom-diff-buffer)
		    ediff-custom-diff-buffer)
		   ((ediff-buffer-live-p ediff-diff-buffer)
		    ediff-diff-buffer)
		   (t
		    (beep)
		    (message "Output from `diff' not found")
		    nil))))
    (if buf
	(progn
	  (ediff-with-current-buffer buf
	    (goto-char (point-min)))
	  (switch-to-buffer buf)
	  (raise-frame (selected-frame)))))
  (if (frame-live-p ediff-control-frame)
      (ediff-reset-mouse ediff-control-frame))
  (if (window-live-p ediff-control-window)
      (select-window ediff-control-window)))


(defun ediff-inferior-compare-regions ()
  "Compare regions in an active Ediff session.
Like ediff-regions-linewise but is called from under an active Ediff session on
the files that belong to that session.

After quitting the session invoked via this function, type C-l to the parent
Ediff Control Panel to restore highlighting."
  (interactive)
  (let ((answer "")
	(possibilities (list ?A ?B ?C))
	(zmacs-regions t)
	use-current-diff-p
	begA begB endA endB bufA bufB)

    (if (ediff-valid-difference-p ediff-current-difference)
	(progn
	  (ediff-set-fine-diff-properties ediff-current-difference 'default)
	  (ediff-unhighlight-diff)))
    (ediff-paint-background-regions 'unhighlight)

    (cond ((ediff-merge-job)
	   (setq bufB ediff-buffer-C)
	   ;; ask which buffer to compare to the merge buffer
	   (while (cond ((eq answer ?A)
			 (setq bufA ediff-buffer-A
			       possibilities '(?B))
			 nil)
			((eq answer ?B)
			 (setq bufA ediff-buffer-B
			       possibilities '(?A))
			 nil)
			((equal answer ""))
			(t (beep 1)
			   (message "Valid values are A or B")
			   (sit-for 2)
			   t))
	     (let ((cursor-in-echo-area t))
	       (message
		"Which buffer to compare to the merge buffer (A or B)? ")
	       (setq answer (capitalize (read-char-exclusive))))))

	  ((ediff-3way-comparison-job)
	   ;; ask which two buffers to compare
	   (while (cond ((memq answer possibilities)
			 (setq possibilities (delq answer possibilities))
			 (setq bufA
			       (eval
				(ediff-get-symbol-from-alist
				 answer ediff-buffer-alist)))
			 nil)
			((equal answer ""))
			(t (beep 1)
			   (message
			    "Valid values are %s"
			    (mapconcat 'char-to-string possibilities " or "))
			   (sit-for 2)
			   t))
	     (let ((cursor-in-echo-area t))
	       (message "Enter the 1st buffer you want to compare (%s): "
			(mapconcat 'char-to-string possibilities " or "))
	       (setq answer (capitalize (read-char-exclusive)))))
	   (setq answer "") ; silence error msg
	   (while (cond ((memq answer possibilities)
			 (setq possibilities (delq answer possibilities))
			 (setq bufB
			       (eval
				(ediff-get-symbol-from-alist
				 answer ediff-buffer-alist)))
			 nil)
			((equal answer ""))
			(t (beep 1)
			   (message
			    "Valid values are %s"
			    (mapconcat 'char-to-string possibilities " or "))
			   (sit-for 2)
			   t))
	     (let ((cursor-in-echo-area t))
	       (message "Enter the 2nd buffer you want to compare (%s): "
			(mapconcat 'char-to-string possibilities "/"))
	       (setq answer (capitalize (read-char-exclusive))))))
	  (t ; 2way comparison
	   (setq bufA ediff-buffer-A
		 bufB ediff-buffer-B
		 possibilities nil)))

    (if (and (ediff-valid-difference-p ediff-current-difference)
	     (y-or-n-p "Compare currently highlighted difference regions? "))
	(setq use-current-diff-p t))

    (setq bufA (if use-current-diff-p
		   (ediff-clone-buffer-for-current-diff-comparison
		    bufA 'A "-Region.A-")
		 (ediff-clone-buffer-for-region-comparison bufA "-Region.A-")))
    (ediff-with-current-buffer bufA
      (setq begA (region-beginning)
	    endA (region-end))
      (goto-char begA)
      (beginning-of-line)
      (setq begA (point))
      (goto-char endA)
      (end-of-line)
      (or (eobp) (forward-char)) ; include the newline char
      (setq endA (point)))

    (setq bufB (if use-current-diff-p
		   (ediff-clone-buffer-for-current-diff-comparison
		    bufB 'B "-Region.B-")
		 (ediff-clone-buffer-for-region-comparison bufB "-Region.B-")))
    (ediff-with-current-buffer bufB
      (setq begB (region-beginning)
	    endB (region-end))
      (goto-char begB)
      (beginning-of-line)
      (setq begB (point))
      (goto-char endB)
      (end-of-line)
      (or (eobp) (forward-char)) ; include the newline char
      (setq endB (point)))


    (ediff-regions-internal
     bufA begA endA bufB begB endB
     nil     	     	     	; setup-hook
     (if use-current-diff-p	; job name
	 'ediff-regions-wordwise
       'ediff-regions-linewise)
     (if use-current-diff-p	; word mode, if diffing current diff
	 t nil)
     ;; setup param to pass to ediff-setup
     (list (cons 'ediff-split-window-function ediff-split-window-function)))
    ))



(defun ediff-remove-flags-from-buffer (buffer overlay)
  (ediff-with-current-buffer buffer
    (let ((inhibit-read-only t))
      (if (featurep 'xemacs)
	  (ediff-overlay-put overlay 'begin-glyph nil)
	(ediff-overlay-put overlay 'before-string nil))

      (if (featurep 'xemacs)
	  (ediff-overlay-put overlay 'end-glyph nil)
	(ediff-overlay-put overlay 'after-string nil))
      )))



(defun ediff-place-flags-in-buffer (buf-type buffer ctl-buffer diff)
  (ediff-with-current-buffer buffer
    (ediff-place-flags-in-buffer1 buf-type ctl-buffer diff)))


(defun ediff-place-flags-in-buffer1 (buf-type ctl-buffer diff-no)
  (let* ((curr-overl (ediff-with-current-buffer ctl-buffer
		       (ediff-get-diff-overlay diff-no buf-type)))
	 (before (ediff-get-diff-posn buf-type 'beg diff-no ctl-buffer))
	 after beg-of-line flag)

    ;; insert flag before the difference
    (goto-char before)
    (setq beg-of-line (bolp))

    (setq flag (ediff-with-current-buffer ctl-buffer
		 (if (eq ediff-highlighting-style 'ascii)
		     (if beg-of-line
			 ediff-before-flag-bol ediff-before-flag-mol))))

    ;; insert the flag itself
    (if (featurep 'xemacs)
	(ediff-overlay-put curr-overl 'begin-glyph flag)
      (ediff-overlay-put curr-overl 'before-string flag))

    ;; insert the flag after the difference
    ;; `after' must be set here, after the before-flag was inserted
    (setq after (ediff-get-diff-posn buf-type 'end diff-no ctl-buffer))
    (goto-char after)
    (setq beg-of-line (bolp))

    (setq flag (ediff-with-current-buffer ctl-buffer
		 (if (eq ediff-highlighting-style 'ascii)
		     (if beg-of-line
			 ediff-after-flag-eol ediff-after-flag-mol))))

    ;; insert the flag itself
    (if (featurep 'xemacs)
	(ediff-overlay-put curr-overl 'end-glyph flag)
      (ediff-overlay-put curr-overl 'after-string flag))
    ))


;;; Some diff region tests

;; t if diff region is empty.
;; In case of buffer C, t also if it is not a 3way
;; comparison job (merging jobs return t as well).
(defun ediff-empty-diff-region-p (n buf-type)
  (if (eq buf-type 'C)
      (or (not ediff-3way-comparison-job)
	  (= (ediff-get-diff-posn 'C 'beg n)
	     (ediff-get-diff-posn 'C 'end n)))
    (= (ediff-get-diff-posn buf-type 'beg n)
       (ediff-get-diff-posn buf-type 'end n))))

;; Test if diff region is white space only.
;; If 2-way job and buf-type = C, then returns t.
(defun ediff-whitespace-diff-region-p (n buf-type)
  (or (and (eq buf-type 'C) (not ediff-3way-job))
      (ediff-empty-diff-region-p n buf-type)
      (let ((beg (ediff-get-diff-posn buf-type 'beg n))
	    (end (ediff-get-diff-posn buf-type 'end n)))
	(ediff-with-current-buffer (ediff-get-buffer buf-type)
	  (save-excursion
	    (goto-char beg)
	    (skip-chars-forward ediff-whitespace)
	    (>= (point) end))))))


(defun ediff-get-region-contents (n buf-type ctrl-buf &optional start end)
  (ediff-with-current-buffer
      (ediff-with-current-buffer ctrl-buf (ediff-get-buffer buf-type))
    (buffer-substring
     (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
     (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf)))))

;; Returns positions of difference sectors in the BUF-TYPE buffer.
;; BUF-TYPE should be a symbol -- `A', `B', or `C'.
;; POS is either `beg' or `end'--it specifies whether you want the position at
;; the beginning of a difference or at the end.
;;
;; The optional argument N says which difference (default:
;; `ediff-current-difference').  N is the internal difference number (1- what
;; the user sees).  The optional argument CONTROL-BUF says
;; which control buffer is in effect in case it is not the current
;; buffer.
(defun ediff-get-diff-posn (buf-type pos &optional n control-buf)
  (let (diff-overlay)
    (or control-buf
	(setq control-buf (current-buffer)))

    (ediff-with-current-buffer control-buf
      (or n  (setq n ediff-current-difference))
      (if (or (< n 0) (>= n ediff-number-of-differences))
	  (if (> ediff-number-of-differences 0)
	      (error ediff-BAD-DIFF-NUMBER
		     this-command (1+ n) ediff-number-of-differences)
	    (error ediff-NO-DIFFERENCES)))
      (setq diff-overlay (ediff-get-diff-overlay n buf-type)))
    (if (not (ediff-buffer-live-p (ediff-overlay-buffer diff-overlay)))
	(error ediff-KILLED-VITAL-BUFFER))
    (if (eq pos 'beg)
	(ediff-overlay-start diff-overlay)
      (ediff-overlay-end diff-overlay))
    ))


;; Restore highlighting to what it should be according to ediff-use-faces,
;; ediff-highlighting-style, and ediff-highlight-all-diffs variables.
(defun ediff-restore-highlighting (&optional ctl-buf)
  (ediff-with-current-buffer (or ctl-buf (current-buffer))
    (if (and (ediff-has-face-support-p)
	     ediff-use-faces
	     ediff-highlight-all-diffs)
	(ediff-paint-background-regions))
    (ediff-select-difference ediff-current-difference)))



;; null out difference overlays so they won't slow down future
;; editing operations
;; VEC is either a difference vector or a fine-diff vector
(defun ediff-clear-diff-vector (vec-var &optional fine-diffs-also)
  (if (vectorp (symbol-value vec-var))
      (mapc (lambda (elt)
	      (ediff-delete-overlay
	       (ediff-get-diff-overlay-from-diff-record elt))
	      (if fine-diffs-also
		  (ediff-clear-fine-diff-vector elt))
	      )
	    (symbol-value vec-var)))
  ;; allow them to be garbage collected
  (set vec-var nil))



;;; Misc

;; In Emacs, this just makes overlay.  In the future, when Emacs will start
;; supporting sticky overlays, this function will make a sticky overlay.
;; BEG and END are expressions telling where overlay starts.
;; If they are numbers or buffers, then all is well.  Otherwise, they must
;; be expressions to be evaluated in buffer BUF in order to get the overlay
;; bounds.
;; If BUFF is not a live buffer, then return nil; otherwise, return the
;; newly created overlay.
(defun ediff-make-bullet-proof-overlay (beg end buff)
  (if (ediff-buffer-live-p buff)
      (let (overl)
	(ediff-with-current-buffer buff
	  (or (number-or-marker-p beg)
	      (setq beg (eval beg)))
	  (or (number-or-marker-p end)
	      (setq end (eval end)))
	  (setq overl
		(if (featurep 'xemacs)
		    (make-extent beg end buff)
		  ;; advance front and rear of the overlay
		  (make-overlay beg end buff nil 'rear-advance)))

	  ;; never detach
	  (ediff-overlay-put
	   overl (if (featurep 'emacs) 'evaporate 'detachable) nil)
	  ;; make overlay open-ended
	  ;; In emacs, it is made open ended at creation time
	  (when (featurep 'xemacs)
	    (ediff-overlay-put overl 'start-open nil)
	    (ediff-overlay-put overl 'end-open nil))
	  (ediff-overlay-put overl 'ediff-diff-num 0)
	  overl))))


(defun ediff-make-current-diff-overlay (type)
  (if (ediff-has-face-support-p)
      (let ((overlay (ediff-get-symbol-from-alist
		      type ediff-current-diff-overlay-alist))
	    (buffer (ediff-get-buffer type))
	    (face (ediff-get-symbol-from-alist
		    type ediff-current-diff-face-alist)))
	(set overlay
	     (ediff-make-bullet-proof-overlay (point-max) (point-max) buffer))
	(ediff-set-overlay-face (symbol-value overlay) face)
	(ediff-overlay-put (symbol-value overlay) 'ediff ediff-control-buffer))
    ))


;; Like other-buffer, but prefers visible buffers and ignores temporary or
;; other insignificant buffers (those beginning with "^[ *]").
;; Gets one arg--buffer name or a list of buffer names (it won't return
;; these buffers).
;; EXCL-BUFF-LIST is an exclusion list.
(defun ediff-other-buffer (excl-buff-lst)
  (or (listp excl-buff-lst) (setq excl-buff-lst (list excl-buff-lst)))
  (let* ((all-buffers (nconc (ediff-get-selected-buffers) (buffer-list)))
	 ;; we compute this the second time because we need to do memq on it
	 ;; later, and nconc above will break it. Either this or use slow
	 ;; append instead of nconc
	 (selected-buffers (ediff-get-selected-buffers))
	 (preferred-buffer (car all-buffers))
	 visible-dired-buffers
	 (excl-buff-name-list
	  (mapcar
	   (lambda (b) (cond ((stringp b) b)
			     ((bufferp b) (buffer-name b))))
	   excl-buff-lst))
	 ;; if at least one buffer on the exclusion list is dired, then force
	 ;; all others to be dired. This is because this means that the user
	 ;; has already chosen a dired buffer before
	 (use-dired-major-mode
	  (cond ((null (ediff-buffer-live-p (car excl-buff-lst))) 'unknown)
		((eq (ediff-with-current-buffer (car excl-buff-lst) major-mode)
		     'dired-mode)
		 'yes)
		(t 'no)))
	 ;; significant-buffers must be visible and not belong
	 ;; to the exclusion list `buff-list'
	 ;; We also exclude temporary buffers, but keep mail and gnus buffers
	 ;; Furthermore, we exclude dired buffers, unless they are the only
	 ;; ones visible (and there are at least two of them).
	 ;; Also, any visible window not on the exclusion list that is first in
	 ;; the buffer list is chosen regardless. (This is because the user
	 ;; clicked on it or did something to distinguish it).
	 (significant-buffers
	  (mapcar
	   (lambda (x)
	     (cond ((member (buffer-name x) excl-buff-name-list) nil)
		   ((memq x selected-buffers) x)
		   ((not (ediff-get-visible-buffer-window x)) nil)
		   ((eq x preferred-buffer) x)
		   ;; if prev selected buffer is dired, look only at
		   ;; dired.
		   ((eq use-dired-major-mode 'yes)
		    (if (eq (ediff-with-current-buffer x major-mode)
			    'dired-mode)
			x nil))
		   ((eq (ediff-with-current-buffer x major-mode)
			'dired-mode)
		    (if (null use-dired-major-mode)
			;; don't know if we must enforce dired.
			;; Remember this buffer in case
			;; dired buffs are the only ones visible.
			(setq visible-dired-buffers
			      (cons x visible-dired-buffers)))
		    ;; skip, if dired is not forced
		    nil)
		   ((memq (ediff-with-current-buffer x major-mode)
			  '(rmail-mode
			    vm-mode
			    gnus-article-mode
			    mh-show-mode))
		    x)
		   ((string-match "^[ *]" (buffer-name x)) nil)
		   ((string= "*scratch*" (buffer-name x)) nil)
		   (t x)))
	   all-buffers))
	 (clean-significant-buffers (delq nil significant-buffers))
	 less-significant-buffers)

    (if (and (null clean-significant-buffers)
	     (> (length visible-dired-buffers) 0))
	(setq clean-significant-buffers visible-dired-buffers))

    (cond (clean-significant-buffers (car clean-significant-buffers))
	  ;; try also buffers that are not displayed in windows
	  ((setq less-significant-buffers
		 (delq nil
		       (mapcar
			(lambda (x)
			  (cond ((member (buffer-name x) excl-buff-name-list)
				 nil)
				((eq use-dired-major-mode 'yes)
				 (if (eq (ediff-with-current-buffer
					     x major-mode)
					 'dired-mode)
				     x nil))
				((eq (ediff-with-current-buffer x major-mode)
				     'dired-mode)
				 nil)
				((string-match "^[ *]" (buffer-name x)) nil)
				((string= "*scratch*" (buffer-name x)) nil)
				(t x)))
			all-buffers)))
	   (car less-significant-buffers))
	  (t "*scratch*"))
    ))


;; If current buffer is a Buffer-menu buffer, then take the selected buffers
;; and append the buffer at the cursor to the end.
;; This list would be the preferred list.
(defun ediff-get-selected-buffers ()
  (if (eq major-mode 'Buffer-menu-mode)
      (let ((lis (condition-case nil
		     (list (Buffer-menu-buffer t))
		   (error))
		 ))
	(save-excursion
	  (goto-char (point-max))
	  (while (search-backward "\n>" nil t)
	    (forward-char 1)
	    (setq lis (cons (Buffer-menu-buffer t) lis)))
	  lis))
    ))

;; Construct a unique buffer name.
;; The first one tried is prefixsuffix, then prefix<2>suffix,
;; prefix<3>suffix, etc.
(defun ediff-unique-buffer-name (prefix suffix)
  (if (null (get-buffer (concat prefix suffix)))
      (concat prefix suffix)
    (let ((n 2))
      (while (get-buffer (format "%s<%d>%s" prefix n suffix))
	(setq n (1+ n)))
      (format "%s<%d>%s" prefix n suffix))))


(defun ediff-submit-report ()
  "Submit bug report on Ediff."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((reporter-prompt-for-summary-p t)
	(ctl-buf ediff-control-buffer)
	(ediff-device-type (ediff-device-type))
	varlist salutation buffer-name)
    (setq varlist '(ediff-diff-program ediff-diff-options
                    ediff-diff3-program ediff-diff3-options
		    ediff-patch-program ediff-patch-options
		    ediff-shell
		    ediff-use-faces
		    ediff-auto-refine ediff-highlighting-style
		    ediff-buffer-A ediff-buffer-B ediff-control-buffer
		    ediff-forward-word-function
		    ediff-control-frame
		    ediff-control-frame-parameters
		    ediff-control-frame-position-function
		    ediff-prefer-iconified-control-frame
		    ediff-window-setup-function
		    ediff-split-window-function
		    ediff-job-name
		    ediff-word-mode
		    buffer-name
		    ediff-device-type
		    ))
    (setq salutation "
Congratulations!  You may have unearthed a bug in Ediff!

Please make a concise and accurate summary of what happened
and mail it to the address above.
-----------------------------------------------------------
")

    (ediff-skip-unsuitable-frames)
    (ediff-reset-mouse)

    (switch-to-buffer ediff-msg-buffer)
    (erase-buffer)
    (delete-other-windows)
    (insert "
Please read this first:
----------------------

Some ``bugs'' may actually be no bugs at all.  For instance, if you are
reporting that certain difference regions are not matched as you think they
should, this is most likely due to the way Unix diff program decides what
constitutes a difference region.  Ediff is an Emacs interface to diff, and
it has nothing to do with those decisions---it only takes the output from
diff and presents it in a way that is better suited for human browsing and
manipulation.

If Emacs happens to dump core, this is NOT an Ediff problem---it is
an Emacs bug.  Report this to Emacs maintainers.

Another popular topic for reports is compilation messages.  Because Ediff
interfaces to several other packages and runs under Emacs and XEmacs,
byte-compilation may produce output like this:

       While compiling toplevel forms in file ediff.el:
	 ** reference to free variable pm-color-alist
	   ........................
       While compiling the end of the data:
	 ** The following functions are not known to be defined:
	   xxx, yyy
	   ........................

These are NOT errors, but inevitable warnings, which ought to be ignored.

Please do not report those and similar things.  However, comments and
suggestions are always welcome.

Mail anyway? (y or n) ")

    (if (y-or-n-p "Mail anyway? ")
	(progn
	  (if (ediff-buffer-live-p ctl-buf)
	      (set-buffer ctl-buf))
	  (setq buffer-name (buffer-name))
	  (require 'reporter)
	  (reporter-submit-bug-report "kifer@cs.stonybrook.edu"
				      (ediff-version)
				      varlist
				      nil
				      'delete-other-windows
				      salutation))
      (bury-buffer)
      (beep 1)(message "Bug report aborted")
      (if (ediff-buffer-live-p ctl-buf)
	  (ediff-with-current-buffer ctl-buf
	    (ediff-recenter 'no-rehighlight))))
    ))


;; Find an appropriate syntax table for everyone to use
;; If buffer B is not fundamental or text mode, use its syntax table
;; Otherwise, use buffer B's.
;; The syntax mode is used in ediff-forward-word-function
;; The important thing is that every buffer should use the same syntax table
;; during the refinement operation
(defun ediff-choose-syntax-table ()
  (setq ediff-syntax-table
	(ediff-with-current-buffer ediff-buffer-A
	  (if (not (memq major-mode
			 '(fundamental-mode text-mode indented-text-mode)))
	      (syntax-table))))
  (if (not ediff-syntax-table)
      (setq ediff-syntax-table
	    (ediff-with-current-buffer ediff-buffer-B
	      (syntax-table))))
  )


(defun ediff-deactivate-mark ()
  (if (featurep 'xemacs)
      (zmacs-deactivate-region)
    (deactivate-mark)))

(defun ediff-activate-mark ()
  (if (featurep 'xemacs)
      (zmacs-activate-region)
    (make-local-variable 'transient-mark-mode)
    (setq mark-active t transient-mark-mode t)))

(defun ediff-nuke-selective-display ()
  (if (featurep 'xemacs)
      (nuke-selective-display)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let ((mod-p (buffer-modified-p))
	      buffer-read-only end)
	  (and (eq t selective-display)
	       (while (search-forward "\^M" nil t)
		 (end-of-line)
		 (setq end (point))
		 (beginning-of-line)
		 (while (search-forward "\^M" end t)
		   (delete-char -1)
		   (insert "\^J"))))
	  (set-buffer-modified-p mod-p)
	  (setq selective-display nil))))))


;; The next two are modified versions from emerge.el.
;; VARS must be a list of symbols
;; ediff-save-variables returns an association list: ((var . val) ...)
(defsubst ediff-save-variables (vars)
  (mapcar (lambda (v) (cons v (symbol-value v)))
	  vars))
;; VARS is a list of variable symbols.
(defun ediff-restore-variables (vars assoc-list)
  (while vars
    (set (car vars) (cdr (assoc (car vars) assoc-list)))
    (setq vars (cdr vars))))

(defun ediff-change-saved-variable (var value buf-type)
  (let* ((assoc-list
	  (symbol-value (ediff-get-symbol-from-alist
			 buf-type
			 ediff-buffer-values-orig-alist)))
	 (assoc-elt (assoc var assoc-list)))
  (if assoc-elt
      (setcdr assoc-elt value))))


;; must execute in control buf
(defun ediff-save-protected-variables ()
  (setq ediff-buffer-values-orig-A
	(ediff-with-current-buffer ediff-buffer-A
	  (ediff-save-variables ediff-protected-variables)))
  (setq ediff-buffer-values-orig-B
	(ediff-with-current-buffer ediff-buffer-B
	  (ediff-save-variables ediff-protected-variables)))
  (if ediff-3way-comparison-job
      (setq ediff-buffer-values-orig-C
	    (ediff-with-current-buffer ediff-buffer-C
	      (ediff-save-variables ediff-protected-variables))))
  (if (ediff-buffer-live-p ediff-ancestor-buffer)
      (setq ediff-buffer-values-orig-Ancestor
	    (ediff-with-current-buffer ediff-ancestor-buffer
	      (ediff-save-variables ediff-protected-variables)))))

;; must execute in control buf
(defun ediff-restore-protected-variables ()
  (let ((values-A ediff-buffer-values-orig-A)
	(values-B ediff-buffer-values-orig-B)
	(values-C ediff-buffer-values-orig-C)
	(values-Ancestor ediff-buffer-values-orig-Ancestor))
    (ediff-with-current-buffer ediff-buffer-A
      (ediff-restore-variables ediff-protected-variables values-A))
    (ediff-with-current-buffer ediff-buffer-B
      (ediff-restore-variables ediff-protected-variables values-B))
    (if ediff-3way-comparison-job
	(ediff-with-current-buffer ediff-buffer-C
	  (ediff-restore-variables ediff-protected-variables values-C)))
    (if (ediff-buffer-live-p ediff-ancestor-buffer)
	(ediff-with-current-buffer ediff-ancestor-buffer
	  (ediff-restore-variables ediff-protected-variables values-Ancestor)))
    ))

;; save BUFFER in FILE.  used in hooks.
(defun ediff-save-buffer-in-file (buffer file)
  (ediff-with-current-buffer buffer
    (write-file file)))


;;; Debug

(ediff-defvar-local ediff-command-begin-time '(0 0 0) "")

;; calculate time used by command
(defun ediff-calc-command-time ()
  (or (equal ediff-command-begin-time '(0 0 0))
      (message "Elapsed time: %g second(s)"
	       (float-time (time-since ediff-command-begin-time)))))

(defsubst ediff-save-time ()
  (setq ediff-command-begin-time (current-time)))

(defun ediff-profile ()
  "Toggle profiling Ediff commands."
  (interactive)
  (ediff-barf-if-not-control-buffer)

  (if (featurep 'xemacs)
      (make-local-hook 'post-command-hook))

  (let ((pre-hook 'pre-command-hook)
	(post-hook 'post-command-hook))
    (if (not (equal ediff-command-begin-time '(0 0 0)))
	(progn (remove-hook pre-hook 'ediff-save-time)
	       (remove-hook post-hook 'ediff-calc-command-time)
	       (setq ediff-command-begin-time '(0 0 0))
	       (message "Ediff profiling disabled"))
      (add-hook pre-hook 'ediff-save-time t 'local)
      (add-hook post-hook 'ediff-calc-command-time nil 'local)
      (message "Ediff profiling enabled"))))

(defun ediff-print-diff-vector (diff-vector-var)
  (princ (format "\n*** %S ***\n" diff-vector-var))
  (mapcar (lambda (overl-vec)
	    (princ
	     (format
	      "Diff %d: \tOverlay:    %S
\t\tFine diffs: %s
\t\tNo-fine-diff-flag: %S
\t\tState-of-diff:\t   %S
\t\tState-of-merge:\t   %S
"
	      (1+ (ediff-overlay-get (aref overl-vec 0) 'ediff-diff-num))
	      (aref overl-vec 0)
	      ;; fine-diff-vector
	      (if (= (length (aref overl-vec 1)) 0)
		  "none\n"
		(mapconcat 'prin1-to-string
			   (aref overl-vec 1) "\n\t\t\t    "))
	      (aref overl-vec 2) ; no fine diff flag
	      (aref overl-vec 3) ; state-of-diff
	      (aref overl-vec 4) ; state-of-merge
	      )))
	  (eval diff-vector-var)))



(defun ediff-debug-info ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (with-output-to-temp-buffer ediff-debug-buffer
    (ediff-with-current-buffer standard-output
      (fundamental-mode))
    (princ (format "\nCtl buffer: %S\n" ediff-control-buffer))
    (ediff-print-diff-vector (intern "ediff-difference-vector-A"))
    (ediff-print-diff-vector (intern "ediff-difference-vector-B"))
    (ediff-print-diff-vector (intern "ediff-difference-vector-C"))
    (ediff-print-diff-vector (intern "ediff-difference-vector-Ancestor"))
    ))


;;; General utilities

;; this uses comparison-func to decide who is a member
(defun ediff-member (elt lis comparison-func)
  (while (and lis (not (funcall comparison-func (car lis) elt)))
    (setq lis (cdr lis)))
  lis)

;; Make a readable representation of the invocation sequence for FUNC-DEF.
;; It would either be a key or M-x something.
(defun ediff-format-bindings-of (func-def)
  (let ((desc (car (where-is-internal func-def
				      overriding-local-map
				      nil nil))))
    (if desc
	(key-description desc)
      (format "M-x %s" func-def))))

;; this uses comparison-func to decide who is a member, and this determines how
;; intersection looks like
(defun ediff-intersection (lis1 lis2 comparison-func)
  (let ((result (list 'a)))
    (while lis1
      (if (ediff-member (car lis1) lis2 comparison-func)
	  (nconc result (list (car lis1))))
      (setq lis1 (cdr lis1)))
    (cdr result)))


;; eliminates duplicates using comparison-func
(defun ediff-union (lis1 lis2 comparison-func)
  (let ((result (list 'a)))
    (while lis1
      (or (ediff-member (car lis1) (cdr result) comparison-func)
	  (nconc result (list (car lis1))))
      (setq lis1 (cdr lis1)))
    (while lis2
      (or (ediff-member (car lis2) (cdr result) comparison-func)
	  (nconc result (list (car lis2))))
      (setq lis2 (cdr lis2)))
    (cdr result)))

;; eliminates duplicates using comparison-func
(defun ediff-set-difference (lis1 lis2 comparison-func)
  (let ((result (list 'a)))
    (while lis1
      (or (ediff-member (car lis1) (cdr result) comparison-func)
	  (ediff-member (car lis1) lis2 comparison-func)
	  (nconc result (list (car lis1))))
      (setq lis1 (cdr lis1)))
    (cdr result)))

(defun ediff-add-to-history (history-var newelt)
  (if (fboundp 'add-to-history)
      (add-to-history history-var newelt)
    (set history-var (cons newelt (symbol-value history-var)))))

(defalias 'ediff-copy-list 'copy-sequence)


;; don't report error if version control package wasn't found
;;(ediff-load-version-control 'silent)

(run-hooks 'ediff-load-hook)


;; Local Variables:
;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;; eval: (put 'ediff-with-current-buffer 'lisp-indent-hook 1)
;; eval: (put 'ediff-with-current-buffer 'edebug-form-spec '(form body))
;; End:

;;; ediff-util.el ends here
