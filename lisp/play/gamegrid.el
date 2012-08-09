;;; gamegrid.el --- library for implementing grid-based games on Emacs

;; Copyright (C) 1997-1998, 2001-2012  Free Software Foundation, Inc.

;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Version: 1.02
;; Created: 1997-08-13
;; Keywords: games

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

(eval-when-compile
  (require 'cl))

;; ;;;;;;;;;;;;; buffer-local variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gamegrid-use-glyphs t
  "Non-nil means use glyphs when available.")

(defvar gamegrid-use-color t
  "Non-nil means use color when available.")

(defvar gamegrid-font "-*-courier-medium-r-*-*-*-140-100-75-*-*-iso8859-*"
  "Name of the font used in X mode.")

(defvar gamegrid-face nil
  "Indicates the face to use as a default.")
(make-variable-buffer-local 'gamegrid-face)

(defvar gamegrid-display-options nil)

(defvar gamegrid-buffer-width 0)
(defvar gamegrid-buffer-height 0)
(defvar gamegrid-blank 0)

(defvar gamegrid-timer nil)

(defvar gamegrid-display-mode nil)

(defvar gamegrid-display-table)

(defvar gamegrid-face-table nil)

(defvar gamegrid-buffer-start 1)

(defvar gamegrid-score-file-length 50
  "Number of high scores to keep.")

(defvar gamegrid-user-score-file-directory
  (locate-user-emacs-file "games/")
  "A directory for game scores which can't be shared.
If Emacs was built without support for shared game scores, then this
directory will be used.")

(make-variable-buffer-local 'gamegrid-use-glyphs)
(make-variable-buffer-local 'gamegrid-use-color)
(make-variable-buffer-local 'gamegrid-font)
(make-variable-buffer-local 'gamegrid-display-options)
(make-variable-buffer-local 'gamegrid-buffer-width)
(make-variable-buffer-local 'gamegrid-buffer-height)
(make-variable-buffer-local 'gamegrid-blank)
(make-variable-buffer-local 'gamegrid-timer)
(make-variable-buffer-local 'gamegrid-display-mode)
(make-variable-buffer-local 'gamegrid-display-table)
(make-variable-buffer-local 'gamegrid-face-table)
(make-variable-buffer-local 'gamegrid-buffer-start)
(make-variable-buffer-local 'gamegrid-score-file-length)

;; ;;;;;;;;;;;;; global variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gamegrid-grid-x-face nil)
(defvar gamegrid-mono-x-face nil)
(defvar gamegrid-mono-tty-face nil)

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst gamegrid-glyph-height 16)

(defconst gamegrid-xpm "\
/* XPM */
static char *noname[] = {
/* width height ncolors chars_per_pixel */
\"16 16 3 1\",
/* colors */
\"+ s col1\",
\". s col2\",
\"- s col3\",
/* pixels */
\"---------------+\",
\"--------------++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"-+++++++++++++++\",
\"++++++++++++++++\"
};
"
  "XPM format image used for each square")

(defvar gamegrid-xbm "\
/* gamegrid XBM */
#define gamegrid_width 16
#define gamegrid_height 16
static unsigned char gamegrid_bits[] = {
   0xff, 0xff, 0xff, 0x7f, 0xff, 0x3f, 0xaf, 0x0a, 0x57, 0x15, 0xaf, 0x0a,
   0x57, 0x15, 0xaf, 0x0a, 0x57, 0x15, 0xaf, 0x0a, 0x57, 0x15, 0xaf, 0x0a,
   0x57, 0x15, 0x07, 0x00, 0x03, 0x00, 0x01, 0x00 };"
  "XBM format image used for each square.")

;; ;;;;;;;;;;;;;;;; miscellaneous functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst gamegrid-characterp (arg)
  (if (fboundp 'characterp)
      (characterp arg)
    (integerp arg)))

(defsubst gamegrid-event-x (event)
  (if (fboundp 'event-x)
      (event-x event)
    (car (posn-col-row (event-end event)))))

(defsubst gamegrid-event-y (event)
  (if (fboundp 'event-y)
      (event-y event)
    (cdr (posn-col-row (event-end event)))))

;; ;;;;;;;;;;;;; display functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gamegrid-color (color shade)
  (let* ((v (floor (* shade 255)))
	 (r (* v (aref color 0)))
	 (g (* v (aref color 1)))
	 (b (* v (aref color 2))))
    (format "#%02x%02x%02x" r g b)))

(defun gamegrid-set-font (face)
  (if gamegrid-font
      (condition-case nil
	  (set-face-font face gamegrid-font)
	(error nil))))

(defun gamegrid-setup-face (face color)
  (set-face-foreground face color)
  (set-face-background face color)
  (gamegrid-set-font face)
  (condition-case nil
      (set-face-background-pixmap face [nothing]);; XEmacs
    (error nil))
  (condition-case nil
      (set-face-background-pixmap face nil);; Emacs
    (error nil)))

(defun gamegrid-make-mono-tty-face ()
  (let ((face (make-face 'gamegrid-mono-tty-face)))
    (set-face-inverse-video-p face t)
    face))

(defun gamegrid-make-color-tty-face (color)
  (let* ((color-str (if (symbolp color) (symbol-value color) color))
	 (name (intern (format "gamegrid-color-tty-face-%s" color-str)))
	 (face (make-face name)))
    (gamegrid-setup-face face color-str)
    face))

(defun gamegrid-make-grid-x-face ()
  (let ((face (make-face 'gamegrid-x-border-face)))
    (gamegrid-set-font face)
    face))

(defun gamegrid-make-mono-x-face ()
  (let ((face (make-face 'gamegrid-mono-x-face))
	(color (face-foreground 'default)))
    (if (null color)
	(setq color
	      (cdr-safe (assq 'foreground-color (frame-parameters)))))
    (gamegrid-setup-face face color)
    face))

(defun gamegrid-make-color-x-face (color)
  (let* ((hex (gamegrid-color color 1.0))
	 (name (intern (format "gamegrid-color-x-face-%s" hex)))
	 (face (make-face name)))
    (gamegrid-setup-face face hex)
    face))

(defun gamegrid-make-face (data-spec-list color-spec-list)
  (let ((data (gamegrid-match-spec-list data-spec-list))
	(color (gamegrid-match-spec-list color-spec-list)))
    (case data
      (color-x
       (gamegrid-make-color-x-face color))
      (grid-x
       (unless gamegrid-grid-x-face
	 (setq gamegrid-grid-x-face (gamegrid-make-grid-x-face)))
       gamegrid-grid-x-face)
      (mono-x
       (unless gamegrid-mono-x-face
	 (setq gamegrid-mono-x-face (gamegrid-make-mono-x-face)))
       gamegrid-mono-x-face)
      (color-tty
       (gamegrid-make-color-tty-face color))
      (mono-tty
       (unless gamegrid-mono-tty-face
	 (setq gamegrid-mono-tty-face (gamegrid-make-mono-tty-face)))
       gamegrid-mono-tty-face))))

(defun gamegrid-colorize-glyph (color)
  (find-image `((:type xpm :data ,gamegrid-xpm
		       :ascent center
		       :color-symbols
		       (("col1" . ,(gamegrid-color color 0.6))
			("col2" . ,(gamegrid-color color 0.8))
			("col3" . ,(gamegrid-color color 1.0))))
		(:type xbm :data ,gamegrid-xbm
		       :ascent center
		       :foreground ,(gamegrid-color color 1.0)
		       :background ,(gamegrid-color color 0.5)))))

(defun gamegrid-match-spec (spec)
  (let ((locale (car spec))
	(value (cadr spec)))
    (and (or (eq locale t)
	     (and (listp locale)
		  (memq gamegrid-display-mode locale))
	     (and (symbolp locale)
		  (eq gamegrid-display-mode locale)))
	 value)))

(defun gamegrid-match-spec-list (spec-list)
  (and spec-list
       (or (gamegrid-match-spec (car spec-list))
	   (gamegrid-match-spec-list (cdr spec-list)))))

(defun gamegrid-make-glyph (data-spec-list color-spec-list)
  (let ((data (gamegrid-match-spec-list data-spec-list))
	(color (gamegrid-match-spec-list color-spec-list)))
    (cond ((gamegrid-characterp data)
	   (vector data))
	  ((eq data 'colorize)
	   (gamegrid-colorize-glyph color))
	  ((listp data)
	   (find-image data)) ;untested!
	  ((vectorp data)
	   (gamegrid-make-image-from-vector data)))))

(defun gamegrid-make-image-from-vector (vect)
  "Convert an XEmacs style \"glyph\" to an image-spec."
  (let ((l (list 'image :type)))
    (dotimes (n (length vect))
      (setf l (nconc l (list (aref vect n)))))
    (nconc l (list :ascent 'center))))

(defun gamegrid-display-type ()
  (cond ((and gamegrid-use-glyphs
	      (display-images-p))
	 'glyph)
	((and gamegrid-use-color
	      (display-graphic-p)
	      (display-color-p))
	 'color-x)
	((display-graphic-p)
	 'mono-x)
	((and gamegrid-use-color
	      (display-color-p))
	 'color-tty)
	((display-multi-font-p) ;???
	 'mono-tty)
	(t
	   'emacs-tty)))

(defun gamegrid-set-display-table ()
  (if (featurep 'xemacs)
      (add-spec-to-specifier current-display-table
			     gamegrid-display-table
			     (current-buffer)
			     nil
			     'remove-locale)
    (setq buffer-display-table gamegrid-display-table)))

(declare-function image-size "image.c" (spec &optional pixels frame))

(defun gamegrid-setup-default-font ()
  (setq gamegrid-face
	(copy-face 'default
		   (intern (concat "gamegrid-face-" (buffer-name)))))
  (when (eq gamegrid-display-mode 'glyph)
    (let ((max-height nil))
      (loop for c from 0 to 255 do
	    (let ((glyph (aref gamegrid-display-table c)))
	      (when (and (listp glyph) (eq (car  glyph) 'image))
		(let ((height (cdr (image-size glyph))))
		  (if (or (null max-height)
			  (< max-height height))
		      (setq max-height height))))))
      (when (and max-height (< max-height 1))
	(let ((default-font-height (face-attribute 'default :height))
	      (resy (/ (display-pixel-height) (/ (display-mm-height) 25.4)))
	      point-size pixel-size)
	  (setq point-size (/ (* (float default-font-height) max-height) 10)
		pixel-size (floor (* resy (/ point-size 72.27)))
		point-size (* (/ pixel-size resy) 72.27))
	  (face-spec-set gamegrid-face
			 `((t :height ,(floor (* point-size 10))))))))))

(defun gamegrid-initialize-display ()
  (setq gamegrid-display-mode (gamegrid-display-type))
  (setq gamegrid-display-table (make-display-table))
  (setq gamegrid-face-table (make-vector 256 nil))
  (loop for c from 0 to 255 do
    (let* ((spec (aref gamegrid-display-options c))
	   (glyph (gamegrid-make-glyph (car spec) (caddr spec)))
	   (face (gamegrid-make-face (cadr spec) (caddr spec))))
      (aset gamegrid-face-table c face)
      (aset gamegrid-display-table c glyph)))
  (gamegrid-setup-default-font)
  (gamegrid-set-display-table)
  (setq cursor-type nil))


(defun gamegrid-set-face (c)
  (if (eq gamegrid-display-mode 'glyph)
      (add-text-properties (1- (point)) (point)
			   (list 'display (list (aref gamegrid-display-table c))))
    (put-text-property (1- (point))
		       (point)
		       'face
		       (aref gamegrid-face-table c))))

(defun gamegrid-cell-offset (x y)
  (+ gamegrid-buffer-start
     (* (1+ gamegrid-buffer-width) y)
     x))

;; ;;;;;;;;;;;;;;;; grid functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gamegrid-get-cell (x y)
  (char-after (gamegrid-cell-offset x y)))

(defun gamegrid-set-cell (x y c)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (gamegrid-cell-offset x y))
      (delete-char 1)
      (insert-char c 1)
      (gamegrid-set-face c))))

(defun gamegrid-init-buffer (width height blank)
  (setq gamegrid-buffer-width width
	gamegrid-buffer-height height)
  (let ((line (concat
	       (make-string width blank)
	       "\n"))
	(buffer-read-only nil))
    (erase-buffer)
    (setq gamegrid-buffer-start (point))
    (dotimes (i height)
      (insert line))
    ;; Adjust the height of the default face to the height of the
    ;; images. Unlike XEmacs, Emacs doesn't allow to make the default
    ;; face buffer-local; so we do this with an overlay.
    (when (eq gamegrid-display-mode 'glyph)
      (overlay-put (make-overlay (point-min) (point-max))
		   'face gamegrid-face))
    (goto-char (point-min))))

(defun gamegrid-init (options)
  (setq buffer-read-only t
	truncate-lines t
	line-spacing 0
	gamegrid-display-options options)
  (buffer-disable-undo (current-buffer))
  (gamegrid-initialize-display))

;; ;;;;;;;;;;;;;;;; timer functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gamegrid-start-timer (period func)
  (setq gamegrid-timer
	(if (featurep 'xemacs)
	    (start-itimer "Gamegrid"
			  func
			  period
			  period
			  nil
			  t
			  (current-buffer))
	  (run-with-timer period
			  period
			  func
			  (current-buffer)))))

(defun gamegrid-set-timer (delay)
  (if gamegrid-timer
      (if (fboundp 'set-itimer-restart)
	  (set-itimer-restart gamegrid-timer delay)
	(timer-set-time gamegrid-timer
			(list (aref gamegrid-timer 1)
			      (aref gamegrid-timer 2)
			      (aref gamegrid-timer 3))
			delay))))

(defun gamegrid-kill-timer ()
  (if gamegrid-timer
      (if (featurep 'xemacs)
          (delete-itimer gamegrid-timer)
        (cancel-timer gamegrid-timer)))
  (setq gamegrid-timer nil))

;; ;;;;;;;;;;;;;;; high score functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gamegrid-add-score (file score)
  "Add the current score to the high score file.

On POSIX systems there may be a shared game directory for all users in
which the scorefiles are kept.  On such systems Emacs doesn't create
the score file FILE in this directory, if it doesn't already exist.
In this case Emacs searches for FILE in the directory specified by
`gamegrid-user-score-file-directory' and creates it there, if
necessary.

To add the score file for a game to the system wide shared game
directory, create the file with the shell command \"touch\" in this
directory and make sure that it is owned by the correct user and
group.  You probably need special user privileges to do this.

On non-POSIX systems Emacs searches for FILE in the directory
specified by the variable `temporary-file-directory'.  If necessary,
FILE is created there."
  (case system-type
    ((ms-dos windows-nt)
     (gamegrid-add-score-insecure file score))
    (t
     (gamegrid-add-score-with-update-game-score file score))))


;; On POSIX systems there are four cases to distinguish:

;;     1. FILE is an absolute filename.  Then it should be a file in
;;        temporary file directory.  This is the way,
;;        `gamegrid-add-score' was supposed to be used in the past and
;;        is covered here for backward-compatibility.
;;
;;     2. The helper program "update-game-score" is setuid and the
;;        file FILE does already exist in a system wide shared game
;;        directory.  This should be the normal case on POSIX systems,
;;        if the game was installed system wide.  Use
;;        "update-game-score" to add the score to the file in the
;;        shared game directory.
;;
;;     3. "update-game-score" is setuid, but the file FILE does *not*
;;        exist in the system wide shared game directory.  Use
;;        `gamegrid-add-score-insecure' to create--if necessary--and
;;        update FILE.  This is for the case that a user has installed
;;        a game on her own.
;;
;;     4. "update-game-score" is not setuid.  Use it to create/update
;;        FILE in the user's home directory.  There is presumably no
;;        shared game directory.

(defvar gamegrid-shared-game-dir)

(defun gamegrid-add-score-with-update-game-score (file score)
  (let ((gamegrid-shared-game-dir
	 (not (zerop (logand (file-modes
			      (expand-file-name "update-game-score"
						exec-directory))
			     #o4000)))))
    (cond ((file-name-absolute-p file)
	   (gamegrid-add-score-insecure file score))
	  ((and gamegrid-shared-game-dir
		(file-exists-p (expand-file-name file shared-game-score-directory)))
	   ;; Use the setuid "update-game-score" program to update a
	   ;; system-wide score file.
	   (gamegrid-add-score-with-update-game-score-1 file
	    (expand-file-name file shared-game-score-directory) score))
	  ;; Else: Add the score to a score file in the user's home
	  ;; directory.
	  (gamegrid-shared-game-dir
	   ;; If `gamegrid-shared-game-dir' is non-nil, then
	   ;; "update-gamescore" program is setuid, so don't use it.
	   (unless (file-exists-p
		    (directory-file-name gamegrid-user-score-file-directory))
	     (make-directory gamegrid-user-score-file-directory t))
	   (gamegrid-add-score-insecure file score
					gamegrid-user-score-file-directory))
	  (t (let ((f (expand-file-name
		       gamegrid-user-score-file-directory)))
	       (when (file-writable-p f)
		 (unless (eq (car-safe (file-attributes f))
			     t)
		   (make-directory f))
		 (setq f (expand-file-name file f))
		 (unless (file-exists-p f)
		   (write-region "" nil f nil 'silent nil 'excl)))
	       (gamegrid-add-score-with-update-game-score-1 file f score))))))

(defun gamegrid-add-score-with-update-game-score-1 (file target score)
  (let ((default-directory "/")
	(errbuf (generate-new-buffer " *update-game-score loss*"))
        (marker-string (concat
			(user-full-name)
			" <"
			(cond ((fboundp 'user-mail-address)
			       (user-mail-address))
			      ((boundp 'user-mail-address)
			       user-mail-address)
			      (t ""))
			">  "
			(current-time-string))))
    ;; This can be called from a timer, so enable local quits.
    (with-local-quit
      (apply
       'call-process
       (append
	(list
	 (expand-file-name "update-game-score" exec-directory)
	 nil errbuf nil
	 "-m" (int-to-string gamegrid-score-file-length)
	 "-d" (if gamegrid-shared-game-dir
		  (expand-file-name shared-game-score-directory)
		(file-name-directory target))
	 file
	 (int-to-string score)
	 marker-string))))
    (if (buffer-modified-p errbuf)
	(progn
	  (display-buffer errbuf)
	  (error "Failed to update game score file"))
      (kill-buffer errbuf))
    (let ((buf (find-buffer-visiting target)))
      (save-excursion
        (if buf
	    (progn
	      (switch-to-buffer buf)
	      (revert-buffer nil t nil)
	      (display-buffer buf))
	  (find-file-read-only target))
        (goto-char (point-min))
        (search-forward (concat (int-to-string score)
				" " (user-login-name) " "
				marker-string))
        (beginning-of-line)))))

(defun gamegrid-add-score-insecure (file score &optional directory)
  (save-excursion
    (setq file (expand-file-name file (or directory
					  temporary-file-directory)))
    (find-file-other-window file)
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert (format "%05d\t%s\t%s <%s>\n"
		    score
		    (current-time-string)
		    (user-full-name)
		    (cond ((fboundp 'user-mail-address)
			   (user-mail-address))
			  ((boundp 'user-mail-address)
			   user-mail-address)
			  (t ""))))
    (sort-fields 1 (point-min) (point-max))
    (reverse-region (point-min) (point-max))
    (goto-char (point-min))
    (forward-line gamegrid-score-file-length)
    (delete-region (point) (point-max))
    (setq buffer-read-only t)
    (save-buffer)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'gamegrid)

;;; gamegrid.el ends here
