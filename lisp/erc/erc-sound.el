;;; erc-sound.el --- CTCP SOUND support for ERC

;; Copyright (C) 2002-2003, 2006-2012 Free Software Foundation, Inc.

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

;; Play sounds when users send you CTCP SOUND messages.

;; This file also defines the command /sound so that you can send
;; sound requests to other users.

;;; Usage:

;; Add the following to your .emacs if you want to play sounds.
;;
;; (require 'erc-sound)
;; (erc-sound-enable)
;;
;; To send requests to other users from within query buffers, type the
;; following:
;;
;; /sound filename optional-message-text
;;
;; You can also type the following:
;;
;; /ctcp nickname sound filename optional-message

;;; Code:

(require 'erc)

;;;###autoload (autoload 'erc-sound-mode "erc-sound")
(define-erc-module sound ctcp-sound
  "In ERC sound mode, the client will respond to CTCP SOUND requests
and play sound files as requested."
  ;; Enable:
  ((add-hook 'erc-ctcp-query-SOUND-hook 'erc-ctcp-query-SOUND)
   (define-key erc-mode-map "\C-c\C-s" 'erc-toggle-sound))
  ;; Disable:
  ((remove-hook 'erc-ctcp-query-SOUND-hook 'erc-ctcp-query-SOUND)
   (define-key erc-mode-map "\C-c\C-s" 'undefined)))

(erc-define-catalog-entry 'english 'CTCP-SOUND "%n (%u@%h) plays %s:%m")

(defgroup erc-sound nil
  "Make ERC play bells and whistles while chatting with people."
  :group 'erc)

(defcustom erc-play-sound t
  "*Play sounds when you receive CTCP SOUND requests."
  :group 'erc-sound
  :type 'boolean)

(defcustom erc-sound-path nil
  "List of directories that contain sound samples to play on SOUND events."
  :group 'erc-sound
  :type '(repeat directory))

(defcustom erc-default-sound nil
  "Play this sound if the requested file was not found.
If this is set to nil or the file doesn't exist a beep will sound."
  :group 'erc-sound
  :type '(choice (const nil)
		 file))

(defvar erc-ctcp-query-SOUND-hook nil
  "Hook to run after receiving a CTCP SOUND request.")

(defun erc-cmd-SOUND (line &optional force)
  "Send a CTCP SOUND message to the default target.
If `erc-play-sound' is non-nil, play the sound as well.

/sound filename optional-message-text

LINE is the text entered, including the command."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\(\\s-.*\\)?$" line)
    (let ((file (match-string 1 line))
	  (msg (match-string 2 line))
	  (tgt (erc-default-target)))
      (if (null msg)
	  (setq msg "")
	;; remove the first white space
	(setq msg (substring msg 1)))
      (if tgt
	  (progn
	    (erc-send-ctcp-message tgt (format "SOUND %s %s" file msg) force)
	    (if erc-play-sound (erc-play-sound file)))
	(erc-display-message nil 'error (current-buffer) 'no-target))
      t))
   (t nil)))

(defun erc-ctcp-query-SOUND (proc nick login host to msg)
  "Display a CTCP SOUND message and play sound if `erc-play-sound' is non-nil."
  (when (string-match "^SOUND\\s-+\\(\\S-+\\)\\(\\(\\s-+.*\\)\\|\\(\\s-*\\)\\)$" msg)
    (let ((sound (match-string 1 msg))
	  (comment (match-string 2 msg)))
      (when erc-play-sound (erc-play-sound sound))
      (erc-display-message
       nil 'notice nil
       'CTCP-SOUND ?n nick ?u login ?h host ?s sound ?m comment)))
  nil)

(defun erc-play-sound (file)
  "Play a sound file located in one of the directories in `erc-sound-path'.
See also `play-sound-file'."
  (let ((filepath (erc-find-file file erc-sound-path)))
    (if (and (not filepath) erc-default-sound)
	(setq filepath erc-default-sound))
    (cond ((and filepath (file-exists-p filepath))
	   (play-sound-file filepath))
	  (t (beep)))
    (erc-log (format "Playing sound file %S" filepath))))

(defun erc-toggle-sound (&optional arg)
  "Toggles playing sounds on and off.  With positive argument,
  turns them on.  With any other argument turns sounds off."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
	 (setq erc-play-sound t))
	(arg (setq erc-play-sound nil))
	(t (setq erc-play-sound (not erc-play-sound))))
  (message "ERC sound is %s" (if erc-play-sound "ON" "OFF")))


(provide 'erc-sound)

;;; erc-sound.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

