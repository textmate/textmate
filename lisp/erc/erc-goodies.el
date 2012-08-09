;; erc-goodies.el --- Collection of ERC modules

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: Jorgen Schaefer <forcer@forcix.cx>

;; Most code is taken verbatim from erc.el, see there for the original
;; authors.

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

;; This provides some small but still useful modes for ERC.

;;; Code:

(require 'erc)

;;; Imenu support

(defun erc-imenu-setup ()
  "Setup Imenu support in an ERC buffer."
  (set (make-local-variable 'imenu-create-index-function)
       'erc-create-imenu-index))

(add-hook 'erc-mode-hook 'erc-imenu-setup)
(autoload 'erc-create-imenu-index "erc-imenu" "Imenu index creation function")

;;; Automatically scroll to bottom
(defcustom erc-input-line-position nil
  "Specify where to position the input line when using `erc-scroll-to-bottom'.

This should be an integer specifying the line of the buffer on which
the input line should stay.  A value of \"-1\" would keep the input
line positioned on the last line in the buffer.  This is passed as an
argument to `recenter'."
  :group 'erc-display
  :type '(choice integer (const nil)))

(define-erc-module scrolltobottom nil
  "This mode causes the prompt to stay at the end of the window."
  ((add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)
   (dolist (buffer (erc-buffer-list))
     (with-current-buffer buffer
       (erc-add-scroll-to-bottom))))
  ((remove-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)
   (dolist (buffer (erc-buffer-list))
     (with-current-buffer buffer
       (remove-hook 'post-command-hook 'erc-scroll-to-bottom t)))))

(defun erc-add-scroll-to-bottom ()
  "A hook function for `erc-mode-hook' to recenter output at bottom of window.

If you find that ERC hangs when using this function, try customizing
the value of `erc-input-line-position'.

This works whenever scrolling happens, so it's added to
`window-scroll-functions' rather than `erc-insert-post-hook'."
  (add-hook 'post-command-hook 'erc-scroll-to-bottom nil t))

(defun erc-scroll-to-bottom ()
  "Recenter WINDOW so that `point' is on the last line.

This is added to `window-scroll-functions' by `erc-add-scroll-to-bottom'.

You can control which line is recentered to by customizing the
variable `erc-input-line-position'."
      ;; Temporarily bind resize-mini-windows to nil so that users who have it
      ;; set to a non-nil value will not suffer from premature minibuffer
      ;; shrinkage due to the below recenter call.  I have no idea why this
      ;; works, but it solves the problem, and has no negative side effects.
      ;; (Fran Litterio, 2003/01/07)
  (let ((resize-mini-windows nil))
    (save-restriction
      (widen)
      (when (and erc-insert-marker
		 ;; we're editing a line. Scroll.
		 (> (point) erc-insert-marker))
	(save-excursion
	  (goto-char (point-max))
	  (recenter (or erc-input-line-position -1)))))))

;;; Make read only
(define-erc-module readonly nil
  "This mode causes all inserted text to be read-only."
  ((add-hook 'erc-insert-post-hook 'erc-make-read-only)
   (add-hook 'erc-send-post-hook 'erc-make-read-only))
  ((remove-hook 'erc-insert-post-hook 'erc-make-read-only)
   (remove-hook 'erc-send-post-hook 'erc-make-read-only)))

(defun erc-make-read-only ()
  "Make all the text in the current buffer read-only.
Put this function on `erc-insert-post-hook' and/or `erc-send-post-hook'."
  (put-text-property (point-min) (point-max) 'read-only t)
  (put-text-property (point-min) (point-max) 'front-sticky t)
  (put-text-property (point-min) (point-max) 'rear-nonsticky t))

;;; Move to prompt when typing text
(define-erc-module move-to-prompt nil
  "This mode causes the point to be moved to the prompt when typing text."
  ((add-hook 'erc-mode-hook 'erc-move-to-prompt-setup)
   (dolist (buffer (erc-buffer-list))
     (with-current-buffer buffer
       (erc-move-to-prompt-setup))))
  ((remove-hook 'erc-mode-hook 'erc-move-to-prompt-setup)
   (dolist (buffer (erc-buffer-list))
     (with-current-buffer buffer
       (remove-hook 'pre-command-hook 'erc-move-to-prompt t)))))

(defun erc-move-to-prompt ()
  "Move the point to the ERC prompt if this is a self-inserting command."
  (when (and erc-input-marker (< (point) erc-input-marker)
             (eq 'self-insert-command this-command))
    (deactivate-mark)
    (push-mark)
    (goto-char (point-max))))

(defun erc-move-to-prompt-setup ()
  "Initialize the move-to-prompt module for XEmacs."
  (add-hook 'pre-command-hook 'erc-move-to-prompt nil t))

;;; Keep place in unvisited channels
(define-erc-module keep-place nil
  "Leave point above un-viewed text in other channels."
  ((add-hook 'erc-insert-pre-hook  'erc-keep-place))
  ((remove-hook 'erc-insert-pre-hook  'erc-keep-place)))

(defun erc-keep-place (ignored)
  "Move point away from the last line in a non-selected ERC buffer."
  (when (and (not (eq (window-buffer (selected-window))
                      (current-buffer)))
             (>= (point) erc-insert-marker))
    (deactivate-mark)
    (goto-char (erc-beg-of-input-line))
    (forward-line -1)))

;;; Distinguish non-commands
(defvar erc-noncommands-list '(erc-cmd-ME
                               erc-cmd-COUNTRY
                               erc-cmd-SV
                               erc-cmd-SM
                               erc-cmd-SMV
                               erc-cmd-LASTLOG)
  "List of commands that are aliases for CTCP ACTION or for ERC messages.

If a command's function symbol is in this list, the typed command
does not appear in the ERC buffer after the user presses ENTER.")

(define-erc-module noncommands nil
  "This mode distinguishes non-commands.
Commands listed in `erc-insert-this' know how to display
themselves."
  ((add-hook 'erc-send-pre-hook 'erc-send-distinguish-noncommands))
  ((remove-hook 'erc-send-pre-hook 'erc-send-distinguish-noncommands)))

(defun erc-send-distinguish-noncommands (str)
  "If STR is an ERC non-command, set `erc-insert-this' to nil."
  (let* ((command (erc-extract-command-from-line str))
         (cmd-fun (and command
                       (car command))))
    (when (and cmd-fun
               (not (string-match "\n.+$" str))
               (memq cmd-fun erc-noncommands-list))
      (setq erc-insert-this nil))))

;;; IRC control character processing.
(defgroup erc-control-characters nil
  "Dealing with control characters."
  :group 'erc)

(defcustom erc-interpret-controls-p t
  "*If non-nil, display IRC colors and other highlighting effects.

If this is set to the symbol `remove', ERC removes all IRC colors and
highlighting effects.  When this variable is non-nil, it can cause Emacs to run
slowly on systems lacking sufficient CPU speed.  In chatty channels, or in an
emergency (message flood) it can be turned off to save processing time.  See
`erc-toggle-interpret-controls'."
  :group 'erc-control-characters
  :type '(choice (const :tag "Highlight control characters" t)
                 (const :tag "Remove control characters" remove)
                 (const :tag "Display raw control characters" nil)))

(defcustom erc-interpret-mirc-color nil
  "*If non-nil, ERC will interpret mIRC color codes."
  :group 'erc-control-characters
  :type 'boolean)

(defcustom erc-beep-p nil
  "Beep if C-g is in the server message.
The value `erc-interpret-controls-p' must also be t for this to work."
  :group 'erc-control-characters
  :type 'boolean)

(defface erc-bold-face '((t (:bold t)))
  "ERC bold face."
  :group 'erc-faces)
(defface erc-inverse-face
  '((t (:foreground "White" :background "Black")))
  "ERC inverse face."
  :group 'erc-faces)
(defface erc-underline-face '((t (:underline t)))
  "ERC underline face."
  :group 'erc-faces)

(defface fg:erc-color-face0 '((t (:foreground "White")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face1 '((t (:foreground "black")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face2 '((t (:foreground "blue4")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face3 '((t (:foreground "green4")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face4 '((t (:foreground "red")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face5 '((t (:foreground "brown")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face6 '((t (:foreground "purple")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face7 '((t (:foreground "orange")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face8 '((t (:foreground "yellow")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face9 '((t (:foreground "green")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face10 '((t (:foreground "lightblue1")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face11 '((t (:foreground "cyan")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face12 '((t (:foreground "blue")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face13 '((t (:foreground "deeppink")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face14 '((t (:foreground "gray50")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face15 '((t (:foreground "gray90")))
  "ERC face."
  :group 'erc-faces)

(defface bg:erc-color-face0 '((t (:background "White")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face1 '((t (:background "black")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face2 '((t (:background "blue4")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face3 '((t (:background "green4")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face4 '((t (:background "red")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face5 '((t (:background "brown")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face6 '((t (:background "purple")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face7 '((t (:background "orange")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face8 '((t (:background "yellow")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face9 '((t (:background "green")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face10 '((t (:background "lightblue1")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face11 '((t (:background "cyan")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face12 '((t (:background "blue")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face13 '((t (:background "deeppink")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face14 '((t (:background "gray50")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face15 '((t (:background "gray90")))
  "ERC face."
  :group 'erc-faces)

(defun erc-get-bg-color-face (n)
  "Fetches the right face for background color N (0-15)."
  (if (stringp n) (setq n (string-to-number n)))
  (if (not (numberp n))
      (prog1 'default
        (erc-error "erc-get-bg-color-face: n is NaN: %S" n))
    (when (> n 16)
      (erc-log (format "   Wrong color: %s" n))
      (setq n (mod n 16)))
    (cond
     ((and (>= n 0) (< n 16))
      (intern (concat "bg:erc-color-face" (number-to-string n))))
     (t (erc-log (format "   Wrong color: %s" n)) 'default))))

(defun erc-get-fg-color-face (n)
  "Fetches the right face for foreground color N (0-15)."
  (if (stringp n) (setq n (string-to-number n)))
  (if (not (numberp n))
      (prog1 'default
        (erc-error "erc-get-fg-color-face: n is NaN: %S" n))
    (when (> n 16)
      (erc-log (format "   Wrong color: %s" n))
      (setq n (mod n 16)))
    (cond
     ((and (>= n 0) (< n 16))
      (intern (concat "fg:erc-color-face" (number-to-string n))))
     (t (erc-log (format "   Wrong color: %s" n)) 'default))))

(define-erc-module irccontrols nil
  "This mode enables the interpretation of IRC control chars."
  ((add-hook 'erc-insert-modify-hook 'erc-controls-highlight)
   (add-hook 'erc-send-modify-hook 'erc-controls-highlight))
  ((remove-hook 'erc-insert-modify-hook 'erc-controls-highlight)
   (remove-hook 'erc-send-modify-hook 'erc-controls-highlight)))

(defun erc-controls-interpret (str)
   "Return a copy of STR after dealing with IRC control characters.
See `erc-interpret-controls-p' and `erc-interpret-mirc-color' for options."
   (when str
     (let ((s str))
       (cond ((eq erc-interpret-controls-p 'remove)
              (erc-controls-strip s))
             (erc-interpret-controls-p
              (let ((boldp nil)
                    (inversep nil)
                    (underlinep nil)
                    (fg nil)
                    (bg nil))
                (while (string-match erc-controls-highlight-regexp s)
                  (let ((control (match-string 1 s))
                        (fg-color (match-string 2 s))
                        (bg-color (match-string 4 s))
                        (start (match-beginning 0))
                        (end (+ (match-beginning 0)
                                (length (match-string 5 s)))))
                    (setq s (erc-replace-match-subexpression-in-string
                             "" s control 1 start))
                    (cond ((and erc-interpret-mirc-color (or fg-color bg-color))
                           (setq fg fg-color)
                           (setq bg bg-color))
                          ((string= control "\C-b")
                           (setq boldp (not boldp)))
                          ((string= control "\C-v")
                           (setq inversep (not inversep)))
                          ((string= control "\C-_")
                           (setq underlinep (not underlinep)))
                          ((string= control "\C-c")
                           (setq fg nil
                                 bg nil))
                          ((string= control "\C-g")
                           (when erc-beep-p
                             (ding)))
                          ((string= control "\C-o")
                           (setq boldp nil
                                 inversep nil
                                 underlinep nil
                                 fg nil
                                 bg nil))
                          (t nil))
                    (erc-controls-propertize
                     start end boldp inversep underlinep fg bg s)))
                s))
             (t s)))))

(defun erc-controls-strip (str)
  "Return a copy of STR with all IRC control characters removed."
  (when str
    (let ((s str))
      (while (string-match erc-controls-remove-regexp s)
        (setq s (replace-match "" nil nil s)))
      s)))

(defvar erc-controls-remove-regexp
  "\C-b\\|\C-_\\|\C-v\\|\C-g\\|\C-o\\|\C-c[0-9]?[0-9]?\\(,[0-9][0-9]?\\)?"
  "Regular expression which matches control characters to remove.")

(defvar erc-controls-highlight-regexp
  (concat "\\(\C-b\\|\C-v\\|\C-_\\|\C-g\\|\C-o\\|"
          "\C-c\\([0-9][0-9]?\\)?\\(,\\([0-9][0-9]?\\)\\)?\\)"
          "\\([^\C-b\C-v\C-_\C-c\C-g\C-o\n]*\\)")
  "Regular expression which matches control chars and the text to highlight.")

(defun erc-controls-highlight ()
  "Highlight IRC control chars in the buffer.
This is useful for `erc-insert-modify-hook' and `erc-send-modify-hook'.
Also see `erc-interpret-controls-p' and `erc-interpret-mirc-color'."
  (goto-char (point-min))
  (cond ((eq erc-interpret-controls-p 'remove)
         (while (re-search-forward erc-controls-remove-regexp nil t)
           (replace-match "")))
        (erc-interpret-controls-p
         (let ((boldp nil)
               (inversep nil)
               (underlinep nil)
               (fg nil)
               (bg nil))
           (while (re-search-forward erc-controls-highlight-regexp nil t)
             (let ((control (match-string 1))
                   (fg-color (match-string 2))
                   (bg-color (match-string 4))
                   (start (match-beginning 0))
                   (end (+ (match-beginning 0) (length (match-string 5)))))
               (replace-match "" nil nil nil 1)
               (cond ((and erc-interpret-mirc-color (or fg-color bg-color))
                      (setq fg fg-color)
                      (setq bg bg-color))
                     ((string= control "\C-b")
                      (setq boldp (not boldp)))
                     ((string= control "\C-v")
                      (setq inversep (not inversep)))
                     ((string= control "\C-_")
                      (setq underlinep (not underlinep)))
                     ((string= control "\C-c")
                      (setq fg nil
                            bg nil))
                     ((string= control "\C-g")
                      (when erc-beep-p
                        (ding)))
                     ((string= control "\C-o")
                      (setq boldp nil
                            inversep nil
                            underlinep nil
                            fg nil
                            bg nil))
                     (t nil))
               (erc-controls-propertize start end
                                        boldp inversep underlinep fg bg)))))
        (t nil)))

(defun erc-controls-propertize (from to boldp inversep underlinep fg bg
                                     &optional str)
  "Prepend properties from IRC control characters between FROM and TO.
If optional argument STR is provided, apply to STR, otherwise prepend properties
to a region in the current buffer."
  (font-lock-prepend-text-property
   from
   to
   'face
   (append (if boldp
               '(erc-bold-face)
             nil)
           (if inversep
               '(erc-inverse-face)
             nil)
           (if underlinep
               '(erc-underline-face)
             nil)
           (if fg
               (list (erc-get-fg-color-face fg))
             nil)
           (if bg
               (list (erc-get-bg-color-face bg))
             nil))
   str)
  str)

(defun erc-toggle-interpret-controls (&optional arg)
  "Toggle interpretation of control sequences in messages.

If ARG is positive, interpretation is turned on.
Else interpretation is turned off."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
         (setq erc-interpret-controls-p t))
        (arg (setq erc-interpret-controls-p nil))
        (t (setq erc-interpret-controls-p (not erc-interpret-controls-p))))
  (message "ERC color interpretation %s"
           (if erc-interpret-controls-p "ON" "OFF")))

;; Smiley
(define-erc-module smiley nil
  "This mode translates text-smileys such as :-) into pictures.
This requires the function `smiley-region', which is defined in
smiley.el, which is part of Gnus."
  ((add-hook 'erc-insert-modify-hook 'erc-smiley)
   (add-hook 'erc-send-modify-hook 'erc-smiley))
  ((remove-hook 'erc-insert-modify-hook 'erc-smiley)
   (remove-hook 'erc-send-modify-hook 'erc-smiley)))

(defun erc-smiley ()
  "Smilify a region.
This function should be used with `erc-insert-modify-hook'."
  (when (fboundp 'smiley-region)
    (smiley-region (point-min) (point-max))))

;; Unmorse
(define-erc-module unmorse nil
  "This mode causes morse code in the current channel to be unmorsed."
  ((add-hook 'erc-insert-modify-hook 'erc-unmorse))
  ((remove-hook 'erc-insert-modify-hook 'erc-unmorse)))

(defun erc-unmorse ()
  "Unmorse some text.
Add this to `erc-insert-modify-hook' if you happen to be on a
channel that has weird people talking in morse to each other.

See also `unmorse-region'."
  (goto-char (point-min))
  (when (re-search-forward "[.-]+\\([.-]*/? *\\)+[.-]+/?" nil t)
    (save-restriction
      (narrow-to-region (match-beginning 0) (match-end 0))
      ;; Turn " / " into "  "
      (goto-char (point-min))
      (while (re-search-forward " / " nil t)
        (replace-match "  "))
      ;; Turn "/ " into "/"
      (goto-char (point-min))
      (while (re-search-forward "/ " nil t)
        (replace-match "/"))
      ;; Unmorse region
      (unmorse-region (point-min) (point-max)))))

;;; erc-occur
(defun erc-occur (string &optional proc)
  "Search for STRING in all buffers related to current server.
If called interactively and prefix argument is given, search on all connected
servers.  If called from a program, PROC specifies the server process."
  (interactive
   (list (read-string "Search for: ")
         (if current-prefix-arg
             nil erc-server-process)))
  (if (fboundp 'multi-occur)
      (multi-occur (erc-buffer-list nil proc) string)
    (error "`multi-occur' is not defined as a function")))

(provide 'erc-goodies)

;;; erc-goodies.el ends here
