;;; rfn-eshadow.el --- Highlight `shadowed' part of read-file-name input text
;;
;; Copyright (C) 2000-2012 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: convenience minibuffer
;; Package: emacs

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
;;
;; Defines the mode `file-name-shadow-mode'.
;;
;; The `read-file-name' function passes its result through
;; `substitute-in-file-name', so any part of the string preceding
;; multiple slashes (or a drive indicator on MS-DOS/MS-Windows) is
;; ignored.
;;
;; If `file-name-shadow-mode' is active, any part of the
;; minibuffer text that would be ignored because of this is given the
;; properties in `file-name-shadow-properties', which may
;; be used to make the ignored text invisible, dim, etc.
;;

;;; Code:


;;; Customization

(defconst file-name-shadow-properties-custom-type
  '(list
    (checklist :inline t
	       (const :tag "Invisible"
		      :doc "Make shadowed part of filename invisible"
		      :format "%t%n%h"
		      :inline t
		      (invisible t intangible t))
	       (list :inline t
		     :format "%v"
		     :tag "Face"
		     :doc "Display shadowed part of filename using a different face"
		     (const :format "" face)
		     (face :value file-name-shadow))
	       (list :inline t
		     :format "%t: %v%h"
		     :tag "Brackets"
		     ;; Note the 4 leading spaces in the doc string;
		     ;; this is hack to get around the fact that the
		     ;; newline after the second string widget comes
		     ;; from the string widget, and doesn't indent
		     ;; correctly.  We could use a :size attribute to
		     ;; make the second string widget not have a
		     ;; terminating newline, but this makes it impossible
		     ;; to enter trailing whitespace, and it's desirable
		     ;; that it be possible.
		     :doc "    Surround shadowed part of filename with brackets"
		     (const :format "" before-string)
		     (string :format "%v" :size 4 :value "{")
		     (const :format "" after-string)
		     ;; see above about why the 2nd string doesn't use :size
		     (string :format " and: %v" :value "} "))
	       (list :inline t
		     :format "%t: %v%n%h"
		     :tag "String"
		     :doc "Display a string instead of the shadowed part of filename"
		     (const :format "" display)
		     (string :format "%v" :size 15 :value "<...ignored...>"))
	       (const :tag "Avoid"
		      :doc "Try to keep cursor out of shadowed part of filename"
		      :format "%t%n%h"
		      :inline t
		      (field shadow)))
    (repeat :inline t
	    :tag "Other Properties"
	    (list :inline t
		  :format "%v"
		  (symbol :tag "Property")
		  (sexp :tag "Value")))))

(defcustom file-name-shadow-properties
  ;; FIXME: should we purecopy this?
'(face file-name-shadow field shadow)
  "Properties given to the `shadowed' part of a filename in the minibuffer.
Only used when `file-name-shadow-mode' is active.
If Emacs is not running under a window system,
`file-name-shadow-tty-properties' is used instead."
  :type file-name-shadow-properties-custom-type
  :group 'minibuffer
  :version "22.1")

(defcustom file-name-shadow-tty-properties
  (purecopy '(before-string "{" after-string "} " field shadow))
  "Properties given to the `shadowed' part of a filename in the minibuffer.
Only used when `file-name-shadow-mode' is active and Emacs
is not running under a window-system; if Emacs is running under a window
system, `file-name-shadow-properties' is used instead."
  :type file-name-shadow-properties-custom-type
  :group 'minibuffer
  :version "22.1")

(defface file-name-shadow
  '((t :inherit shadow))
  "Face used by `file-name-shadow-mode' for the shadow."
  :group 'minibuffer
  :version "22.1")

(defvar rfn-eshadow-setup-minibuffer-hook nil
  "Minibuffer setup functions from other packages.")

(defvar rfn-eshadow-update-overlay-hook nil
  "Customer overlay functions from other packages")


;;; Internal variables

;; A list of minibuffers to which we've added a post-command-hook.
(defvar rfn-eshadow-frobbed-minibufs nil)

;; An overlay covering the shadowed part of the filename (local to the
;; minibuffer).
(defvar rfn-eshadow-overlay)
(make-variable-buffer-local 'rfn-eshadow-overlay)


;;; Hook functions

;; This function goes on minibuffer-setup-hook
(defun rfn-eshadow-setup-minibuffer ()
  "Set up a minibuffer for `file-name-shadow-mode'.
The prompt and initial input should already have been inserted."
  (when minibuffer-completing-file-name
    (setq rfn-eshadow-overlay
	  (make-overlay (minibuffer-prompt-end) (minibuffer-prompt-end)))
    ;; Give rfn-eshadow-overlay the user's props.
    (let ((props
	   (if window-system
	       file-name-shadow-properties
	     file-name-shadow-tty-properties)))
      (while props
	(overlay-put rfn-eshadow-overlay (pop props) (pop props))))
    ;; Turn on overlay evaporation so that we don't have to worry about
    ;; odd effects when the overlay sits empty at the beginning of the
    ;; minibuffer.
    (overlay-put rfn-eshadow-overlay 'evaporate t)
    ;; Add our post-command hook, and make sure can remove it later.
    (add-to-list 'rfn-eshadow-frobbed-minibufs (current-buffer))
    (add-hook 'post-command-hook #'rfn-eshadow-update-overlay nil t)
    ;; Run custom hook
    (run-hooks 'rfn-eshadow-setup-minibuffer-hook)))

(defsubst rfn-eshadow-sifn-equal (goal pos)
  (equal goal (condition-case nil
		  (substitute-in-file-name
		   (buffer-substring-no-properties pos (point-max)))
		;; `substitute-in-file-name' can fail on partial input.
		(error nil))))

;; post-command-hook to update overlay
(defun rfn-eshadow-update-overlay ()
  "Update `rfn-eshadow-overlay' to cover shadowed part of minibuffer input.
This is intended to be used as a minibuffer `post-command-hook' for
`file-name-shadow-mode'; the minibuffer should have already
been set up by `rfn-eshadow-setup-minibuffer'."
  (condition-case nil
      (let ((goal (substitute-in-file-name (minibuffer-contents)))
            (mid (overlay-end rfn-eshadow-overlay))
            (start (minibuffer-prompt-end))
            (end (point-max))
	    (non-essential t))
        (unless
            ;; Catch the common case where the shadow does not need to move.
            (and mid
                 (or (eq mid end)
                     (not (rfn-eshadow-sifn-equal goal (1+ mid))))
                 (or (eq mid start)
                     (rfn-eshadow-sifn-equal goal mid)))
          ;; Binary search for the greatest position still equivalent to
          ;; the whole.
          (while (or (< (1+ start) end)
                     (if (and (< (1+ end) (point-max))
                              (rfn-eshadow-sifn-equal goal (1+ end)))
                         ;; (SIFN end) != goal, but (SIFN (1+end)) == goal,
                         ;; We've reached a discontinuity: this can happen
                         ;; e.g. if `end' point to "/:...".
                         (setq start (1+ end) end (point-max))))
            (setq mid (/ (+ start end) 2))
            (if (rfn-eshadow-sifn-equal goal mid)
                (setq start mid)
              (setq end mid)))
          (move-overlay rfn-eshadow-overlay (minibuffer-prompt-end) start))
	;; Run custom hook
	(run-hooks 'rfn-eshadow-update-overlay-hook))
    ;; `substitute-in-file-name' can fail on partial input.
    (error nil)))

(define-minor-mode file-name-shadow-mode
  "Toggle file-name shadowing in minibuffers (File-Name Shadow mode).
With a prefix argument ARG, enable File-Name Shadow mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

File-Name Shadow mode is a global minor mode.  When enabled, any
part of a filename being read in the minibuffer that would be
ignored (because the result is passed through
`substitute-in-file-name') is given the properties in
`file-name-shadow-properties', which can be used to make that
portion dim, invisible, or otherwise less visually noticeable."
  :global t
  ;; We'd like to use custom-initialize-set here so the setup is done
  ;; before dumping, but at the point where the defcustom is evaluated,
  ;; the corresponding function isn't defined yet, so
  ;; custom-initialize-set signals an error.
  :initialize 'custom-initialize-delay
  :init-value t
  :group 'minibuffer
  :version "22.1"
  (if file-name-shadow-mode
      ;; Enable the mode
      (add-hook 'minibuffer-setup-hook 'rfn-eshadow-setup-minibuffer)
    ;; Disable the mode
    (remove-hook 'minibuffer-setup-hook 'rfn-eshadow-setup-minibuffer)
    ;; Remove our entry from any post-command-hook variable's it's still in
    (dolist (minibuf rfn-eshadow-frobbed-minibufs)
      (with-current-buffer minibuf
	(remove-hook 'post-command-hook #'rfn-eshadow-update-overlay t)))
    (setq rfn-eshadow-frobbed-minibufs nil)))


(provide 'rfn-eshadow)

;;; rfn-eshadow.el ends here
