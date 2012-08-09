;;; vip.el --- a VI Package for GNU Emacs

;; Copyright (C) 1986-1988, 1992-1993, 1998, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Masahiko Sato <ms@sail.stanford.edu>
;; Keywords: emulations

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

;; A full-featured vi(1) emulator.
;;
;; In Japan, the author's address is: masahiko@sato.riec.tohoku.junet
;;
;; Send suggestions and bug reports to one of the above addresses.
;; When you report a bug, be sure to include the version number of VIP and
;; Emacs you are using.

;; Execute info command by typing "M-x info" to get information on VIP.

;;; Code:

(defgroup vip nil
  "A VI Package for GNU Emacs."
  :prefix "vip-"
  :group 'emulations)

;; external variables

(defvar vip-emacs-local-map nil
  "Local map used in Emacs mode.  (Buffer-specific.)")

(defvar vip-insert-local-map nil
  "Local map used in insert command mode.  (Buffer-specific.)")

(make-variable-buffer-local 'vip-emacs-local-map)
(make-variable-buffer-local 'vip-insert-local-map)

(defvar vip-insert-point nil
  "Remember insert point as a marker.  (Buffer-specific.)")

(set-default 'vip-insert-point (make-marker))
(make-variable-buffer-local 'vip-insert-point)

(defvar vip-com-point nil
  "Remember com point as a marker.  (Buffer-specific.)")

(set-default 'vip-com-point (make-marker))
(make-variable-buffer-local 'vip-com-point)

(defvar vip-current-mode nil
  "Current mode.  One of `emacs-mode', `vi-mode', `insert-mode'.")

(make-variable-buffer-local 'vip-current-mode)
(setq-default vip-current-mode 'emacs-mode)

(defvar vip-emacs-mode-line-buffer-identification nil
  "Value of mode-line-buffer-identification in Emacs mode within vip.")
(make-variable-buffer-local 'vip-emacs-mode-line-buffer-identification)
(setq-default vip-emacs-mode-line-buffer-identification
	      '("Emacs: %17b"))

(defvar vip-current-major-mode nil
  "vip-current-major-mode is the major-mode vi considers it is now.
\(buffer specific\)")

(make-variable-buffer-local 'vip-current-major-mode)

(defvar vip-last-shell-com nil
  "Last shell command executed by ! command.")

(defvar vip-use-register nil
  "Name of register to store deleted or yanked strings.")

(defvar vip-d-com nil
  "How to reexecute last destructive command.  Value is list (M-COM VAL COM).")

(defcustom vip-shift-width 8
  "The number of columns shifted by > and < command."
  :type 'integer
  :group 'vip)

(defcustom vip-re-replace nil
  "If t then do regexp replace, if nil then do string replace."
  :type 'boolean
  :group 'vip)

(defvar vip-d-char nil
  "The character remembered by the vi \"r\" command.")

(defvar vip-f-char nil
  "For use by \";\" command.")

(defvar vip-F-char nil
  "For use by \".\" command.")

(defvar vip-f-forward nil
  "For use by \";\" command.")

(defvar vip-f-offset nil
  "For use by \";\" command.")

(defcustom vip-search-wrap-around t
  "If t, search wraps around."
  :type 'boolean
  :group 'vip)

(defcustom vip-re-search nil
  "If t, search is reg-exp search, otherwise vanilla search."
  :type 'boolean
  :group 'vip)

(defvar vip-s-string nil
  "Last vip search string.")

(defvar vip-s-forward nil
  "If t, search is forward.")

(defcustom vip-case-fold-search nil
  "If t, search ignores cases."
  :type 'boolean
  :group 'vip)

(defcustom vip-re-query-replace nil
  "If t then do regexp replace, if nil then do string replace."
  :type 'boolean
  :group 'vip)

(defcustom vip-open-with-indent nil
  "If t, indent when open a new line."
  :type 'boolean
  :group 'vip)

(defcustom vip-help-in-insert-mode nil
  "If t then C-h is bound to help-command in insert mode.
If nil then it is bound to `delete-backward-char'."
  :type 'boolean
  :group 'vip)

(defvar vip-quote-string "> "
  "String inserted at the beginning of region.")

(defvar vip-tags-file-name "TAGS")

(defvar vip-inhibit-startup-message nil)

(defvar vip-startup-file (convert-standard-filename "~/.vip")
  "Filename used as startup file for vip.")

;; key bindings

(defvar vip-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-a" 'beginning-of-line)
    (define-key map "\C-b" 'vip-scroll-back)
    (define-key map "\C-c" 'vip-ctl-c)
    (define-key map "\C-d" 'vip-scroll-up)
    (define-key map "\C-e" 'vip-scroll-up-one)
    (define-key map "\C-f" 'vip-scroll)
    (define-key map "\C-g" 'vip-keyboard-quit)
    (define-key map "\C-h" 'help-command)
    (define-key map "\C-m" 'vip-scroll-back)
    (define-key map "\C-n" 'vip-other-window)
    (define-key map "\C-o" 'vip-open-line-at-point)
    (define-key map "\C-u" 'vip-scroll-down)
    (define-key map "\C-x" 'vip-ctl-x)
    (define-key map "\C-y" 'vip-scroll-down-one)
    (define-key map "\C-z" 'vip-change-mode-to-emacs)
    (define-key map "\e" 'vip-ESC)

    (define-key map " " 'vip-scroll)
    (define-key map "!" 'vip-command-argument)
    (define-key map "\"" 'vip-command-argument)
    (define-key map "#" 'vip-command-argument)
    (define-key map "$" 'vip-goto-eol)
    (define-key map "%" 'vip-paren-match)
    (define-key map "&" 'vip-nil)
    (define-key map "'" 'vip-goto-mark-and-skip-white)
    (define-key map "(" 'vip-backward-sentence)
    (define-key map ")" 'vip-forward-sentence)
    (define-key map "*" 'call-last-kbd-macro)
    (define-key map "+" 'vip-next-line-at-bol)
    (define-key map "," 'vip-repeat-find-opposite)
    (define-key map "-" 'vip-previous-line-at-bol)
    (define-key map "." 'vip-repeat)
    (define-key map "/" 'vip-search-forward)

    (define-key map "0" 'vip-beginning-of-line)
    (define-key map "1" 'vip-digit-argument)
    (define-key map "2" 'vip-digit-argument)
    (define-key map "3" 'vip-digit-argument)
    (define-key map "4" 'vip-digit-argument)
    (define-key map "5" 'vip-digit-argument)
    (define-key map "6" 'vip-digit-argument)
    (define-key map "7" 'vip-digit-argument)
    (define-key map "8" 'vip-digit-argument)
    (define-key map "9" 'vip-digit-argument)

    (define-key map ":" 'vip-ex)
    (define-key map ";" 'vip-repeat-find)
    (define-key map "<" 'vip-command-argument)
    (define-key map "=" 'vip-command-argument)
    (define-key map ">" 'vip-command-argument)
    (define-key map "?" 'vip-search-backward)
    (define-key map "@" 'vip-nil)

    (define-key map "A" 'vip-Append)
    (define-key map "B" 'vip-backward-Word)
    (define-key map "C" 'vip-ctl-c-equivalent)
    (define-key map "D" 'vip-kill-line)
    (define-key map "E" 'vip-end-of-Word)
    (define-key map "F" 'vip-find-char-backward)
    (define-key map "G" 'vip-goto-line)
    (define-key map "H" 'vip-window-top)
    (define-key map "I" 'vip-Insert)
    (define-key map "J" 'vip-join-lines)
    (define-key map "K" 'vip-kill-buffer)
    (define-key map "L" 'vip-window-bottom)
    (define-key map "M" 'vip-window-middle)
    (define-key map "N" 'vip-search-Next)
    (define-key map "O" 'vip-Open-line)
    (define-key map "P" 'vip-Put-back)
    (define-key map "Q" 'vip-query-replace)
    (define-key map "R" 'vip-replace-string)
    (define-key map "S" 'vip-switch-to-buffer-other-window)
    (define-key map "T" 'vip-goto-char-backward)
    (define-key map "U" 'vip-nil)
    (define-key map "V" 'vip-find-file-other-window)
    (define-key map "W" 'vip-forward-Word)
    (define-key map "X" 'vip-ctl-x-equivalent)
    (define-key map "Y" 'vip-yank-line)
    (define-key map "ZZ" 'save-buffers-kill-emacs)

    (define-key map "[" 'vip-nil)
    (define-key map "\\" 'vip-escape-to-emacs)
    (define-key map "]" 'vip-nil)
    (define-key map "^" 'vip-bol-and-skip-white)
    (define-key map "_" 'vip-nil)
    (define-key map "`" 'vip-goto-mark)

    (define-key map "a" 'vip-append)
    (define-key map "b" 'vip-backward-word)
    (define-key map "c" 'vip-command-argument)
    (define-key map "d" 'vip-command-argument)
    (define-key map "e" 'vip-end-of-word)
    (define-key map "f" 'vip-find-char-forward)
    (define-key map "g" 'vip-info-on-file)
    (define-key map "h" 'vip-backward-char)
    (define-key map "i" 'vip-insert)
    (define-key map "j" 'vip-next-line)
    (define-key map "k" 'vip-previous-line)
    (define-key map "l" 'vip-forward-char)
    (define-key map "m" 'vip-mark-point)
    (define-key map "n" 'vip-search-next)
    (define-key map "o" 'vip-open-line)
    (define-key map "p" 'vip-put-back)
    (define-key map "q" 'vip-nil)
    (define-key map "r" 'vip-replace-char)
    (define-key map "s" 'vip-switch-to-buffer)
    (define-key map "t" 'vip-goto-char-forward)
    (define-key map "u" 'vip-undo)
    (define-key map "v" 'vip-find-file)
    (define-key map "w" 'vip-forward-word)
    (define-key map "x" 'vip-delete-char)
    (define-key map "y" 'vip-command-argument)
    (define-key map "zH" 'vip-line-to-top)
    (define-key map "zM" 'vip-line-to-middle)
    (define-key map "zL" 'vip-line-to-bottom)
    (define-key map "z\C-m" 'vip-line-to-top)
    (define-key map "z." 'vip-line-to-middle)
    (define-key map "z-" 'vip-line-to-bottom)

    (define-key map "{" 'vip-backward-paragraph)
    (define-key map "|" 'vip-goto-col)
    (define-key map "}" 'vip-forward-paragraph)
    (define-key map "~" 'vip-nil)
    (define-key map "\177" 'vip-delete-backward-char)
    map))

(defun vip-version ()
  (interactive)
  (message "VIP version 3.5 of September 15, 1987"))


;; basic set up

;;;###autoload
(defun vip-setup ()
  "Set up bindings for C-x 7 and C-z that are useful for VIP users."
  (define-key ctl-x-map "7" 'vip-buffer-in-two-windows)
  (global-set-key "\C-z" 'vip-change-mode-to-vi))

(defmacro vip-loop (count body)
  "(COUNT BODY) Execute BODY COUNT times."
  (list 'let (list (list 'count count))
	(list 'while (list '> 'count 0)
	      body
	      (list 'setq 'count (list '1- 'count)))))

(defun vip-push-mark-silent (&optional location)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
No message."
  (if (null (mark t))
      nil
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (if (> (length mark-ring) mark-ring-max)
	(progn
	  (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
	  (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil))))
  (set-mark (or location (point))))

(defun vip-goto-col (arg)
  "Go to ARG's column."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (save-excursion
      (end-of-line)
      (if (> val (1+ (current-column))) (error "")))
    (if com (move-marker vip-com-point (point)))
    (beginning-of-line)
    (forward-char (1- val))
    (if com (vip-execute-com 'vip-goto-col val com))))

(defun vip-copy-keymap (map)
  (if (null map) (make-sparse-keymap) (copy-keymap map)))


;; changing mode

(defun vip-change-mode (new-mode)
  "Change mode to NEW-MODE---either emacs-mode, vi-mode, or insert-mode."
  (or (eq new-mode vip-current-mode)
      (progn
	(cond ((eq new-mode 'vi-mode)
	       (if (eq vip-current-mode 'insert-mode)
		   (progn
		     (vip-copy-region-as-kill (point) vip-insert-point)
		     (vip-repeat-insert-command))
		 (setq vip-emacs-local-map (current-local-map)
		       vip-emacs-mode-line-buffer-identification
		       mode-line-buffer-identification
		       vip-insert-local-map (vip-copy-keymap
					     (current-local-map))))
	       (vip-change-mode-line "Vi:   ")
	       (use-local-map vip-mode-map))
	      ((eq new-mode 'insert-mode)
	       (move-marker vip-insert-point (point))
	       (if (eq vip-current-mode 'emacs-mode)
		   (setq vip-emacs-local-map (current-local-map)
			 vip-emacs-mode-line-buffer-identification
			 mode-line-buffer-identification
			 vip-insert-local-map (vip-copy-keymap
					       (current-local-map)))
		 (setq vip-insert-local-map (vip-copy-keymap
					     vip-emacs-local-map)))
	       (vip-change-mode-line "Insert")
	       (use-local-map vip-insert-local-map)
	       (define-key vip-insert-local-map "\e" 'vip-change-mode-to-vi)
	       (define-key vip-insert-local-map "\C-z" 'vip-ESC)
	       (define-key vip-insert-local-map "\C-h"
		 (if vip-help-in-insert-mode 'help-command
		   'delete-backward-char))
	       (define-key vip-insert-local-map "\C-w"
		 'vip-delete-backward-word))
	      ((eq new-mode 'emacs-mode)
	       (vip-change-mode-line "Emacs:")
	       (use-local-map vip-emacs-local-map)))
	(setq vip-current-mode new-mode)
	(force-mode-line-update))))

(defun vip-copy-region-as-kill (beg end)
  "If BEG and END do not belong to the same buffer, it copies empty region."
  (condition-case nil
      (copy-region-as-kill beg end)
    (error (copy-region-as-kill beg beg))))

(defun vip-change-mode-line (string)
  "Assuming that the mode line format contains the string \"Emacs:\", this
function replaces the string by \"Vi:   \" etc."
  (setq mode-line-buffer-identification
	(if (string= string "Emacs:")
	    vip-emacs-mode-line-buffer-identification
	  (list (concat string " %17b")))))

;;;###autoload
(defun vip-mode ()
  "Turn on VIP emulation of VI."
  (interactive)
  (if (not vip-inhibit-startup-message)
      (progn
	(switch-to-buffer "VIP Startup Message")
	(erase-buffer)
	(insert
	 "VIP is a Vi emulation package for GNU Emacs.  VIP provides most Vi commands
including Ex commands.  VIP is however different from Vi in several points.
You can get more information on VIP by:
    1.  Typing `M-x info' and selecting menu item \"vip\".
    2.  Typing `C-h k' followed by a key whose description you want.
    3.  Printing VIP manual which can be found as GNU/man/vip.texinfo
    4.  Printing VIP Reference Card which can be found as GNU/etc/vipcard.tex

This startup message appears whenever you load VIP unless you type `y' now.
Type `n' to quit this window for now.\n")
	(goto-char (point-min))
	(if (y-or-n-p "Inhibit VIP startup message? ")
	    (progn
	      (with-current-buffer
                  (find-file-noselect
                   (substitute-in-file-name vip-startup-file))
		(goto-char (point-max))
		(insert "\n(setq vip-inhibit-startup-message t)\n")
		(save-buffer)
		(kill-buffer (current-buffer)))
	      (message "VIP startup message inhibited.")
	      (sit-for 2)))
	(kill-buffer (current-buffer))
	(message "")
	(setq vip-inhibit-startup-message t)))
  (vip-change-mode-to-vi))

(defun vip-change-mode-to-vi ()
  "Change mode to vi mode."
  (interactive)
  (vip-change-mode 'vi-mode))

(defun vip-change-mode-to-insert ()
  "Change mode to insert mode."
  (interactive)
  (vip-change-mode 'insert-mode))

(defun vip-change-mode-to-emacs ()
  "Change mode to Emacs mode."
  (interactive)
  (vip-change-mode 'emacs-mode))


;; escape to emacs mode temporarily

(defun vip-escape-to-emacs (arg &optional events)
  "Escape to Emacs mode for one Emacs command.
ARG is used as the prefix value for the executed command.  If
EVENTS is a list of events, which become the beginning of the command."
  (interactive "P")
  (let (com key (old-map (current-local-map)))
    (if events (setq unread-command-events events))
    (setq prefix-arg arg)
    (use-local-map vip-emacs-local-map)
    (unwind-protect
	(setq com (key-binding (setq key (read-key-sequence nil))))
      (use-local-map old-map))
    (command-execute com prefix-arg)
    (setq prefix-arg nil)  ;; reset prefix arg
    ))

(defun vip-message-conditions (conditions)
  "Print CONDITIONS as a message."
  (let ((case (car conditions)) (msg (cdr conditions)))
    (if (null msg)
	(message "%s" case)
      (message "%s %s" case (prin1-to-string msg)))
    (ding)))

(defun vip-ESC (arg)
  "Emulate ESC key in Emacs mode."
  (interactive "P")
  (vip-escape-to-emacs arg '(?\e)))

(defun vip-ctl-c (arg)
  "Emulate C-c key in Emacs mode."
  (interactive "P")
  (vip-escape-to-emacs arg '(?\C-c)))

(defun vip-ctl-x (arg)
  "Emulate C-x key in Emacs mode."
  (interactive "P")
  (vip-escape-to-emacs arg '(?\C-x)))

(defun vip-ctl-h (arg)
  "Emulate C-h key in Emacs mode."
  (interactive "P")
  (vip-escape-to-emacs arg '(?\C-h)))


;; prefix argument for vi mode

;; In vi mode, prefix argument is a dotted pair (NUM . COM) where NUM
;; represents the numeric value of the prefix argument and COM represents
;; command prefix such as "c", "d", "m" and "y".

(defun vip-prefix-arg-value (char value com)
  "Compute numeric prefix arg value.  Invoked by CHAR.  VALUE is the value
obtained so far, and COM is the command part obtained so far."
  (while (and (>= char ?0) (<= char ?9))
    (setq value (+ (* (if (numberp value) value 0) 10) (- char ?0)))
    (setq char (read-char)))
  (setq prefix-arg value)
  (if com (setq prefix-arg (cons prefix-arg com)))
  (while (= char ?U)
    (vip-describe-arg prefix-arg)
    (setq char (read-char)))
  (setq unread-command-events (list char)))

(defun vip-prefix-arg-com (char value com)
  "Vi operator as prefix argument."
  (let ((cont t))
    (while (and cont
		(or (= char ?c) (= char ?d) (= char ?y)
		    (= char ?!) (= char ?<) (= char ?>) (= char ?=)
		    (= char ?#) (= char ?r) (= char ?R) (= char ?\")))
      (if com
	  ;; this means that we already have a command character, so we
	  ;; construct a com list and exit while.  however, if char is "
	  ;; it is an error.
	  (progn
	    ;; new com is (CHAR . OLDCOM)
	    (if (or (= char ?#) (= char ?\")) (error ""))
	    (setq com (cons char com))
	    (setq cont nil))
	;; if com is nil we set com as char, and read more.  again, if char
	;; is ", we read the name of register and store it in vip-use-register.
	;; if char is !, =, or #, a complete com is formed so we exit while.
	(cond ((or (= char ?!) (= char ?=))
	       (setq com char)
	       (setq char (read-char))
	       (setq cont nil))
	      ((= char ?#)
	       ;; read a char and encode it as com
	       (setq com (+ 128 (read-char)))
	       (setq char (read-char))
	       (setq cont nil))
	      ((or (= char ?<) (= char ?>))
	       (setq com char)
	       (setq char (read-char))
	       (if (= com char) (setq com (cons char com)))
	       (setq cont nil))
	      ((= char ?\")
	       (let ((reg (read-char)))
		 (if (or (and (<= ?A reg) (<= reg ?z))
			 (and (<= ?1 reg) (<= reg ?9)))
		     (setq vip-use-register reg)
		   (error ""))
		 (setq char (read-char))))
	      (t
	       (setq com char)
	       (setq char (read-char)))))))
  (if (atom com)
      ;; com is a single char, so we construct prefix-arg
      ;; and if char is ?, describe prefix arg, otherwise exit by
      ;; pushing the char back
      (progn
	(setq prefix-arg (cons value com))
	(while (= char ?U)
	  (vip-describe-arg prefix-arg)
	  (setq char (read-char)))
	(setq unread-command-events (list char)))
    ;; as com is non-nil, this means that we have a command to execute
    (if (or (= (car com) ?r) (= (car com) ?R))
	;; execute appropriate region command.
	(let ((char (car com)) (com (cdr com)))
	  (setq prefix-arg (cons value com))
	  (if (= char ?r) (vip-region prefix-arg)
	    (vip-Region prefix-arg))
	  ;; reset prefix-arg
	  (setq prefix-arg nil))
      ;; otherwise, reset prefix arg and call appropriate command
      (setq value (if (null value) 1 value))
      (setq prefix-arg nil)
      (cond ((equal com '(?c . ?c)) (vip-line (cons value ?C)))
	    ((equal com '(?d . ?d)) (vip-line (cons value ?D)))
	    ((equal com '(?d . ?y)) (vip-yank-defun))
	    ((equal com '(?y . ?y)) (vip-line (cons value ?Y)))
	    ((equal com '(?< . ?<)) (vip-line (cons value ?<)))
	    ((equal com '(?> . ?>)) (vip-line (cons value ?>)))
	    ((equal com '(?! . ?!)) (vip-line (cons value ?!)))
	    ((equal com '(?= . ?=)) (vip-line (cons value ?=)))
	    (t (error ""))))))

(defun vip-describe-arg (arg)
  (let (val com)
    (setq val (vip-P-val arg)
	  com (vip-getcom arg))
    (if (null val)
	(if (null com)
	    (message "Value is nil, and command is nil.")
	  (message "Value is nil, and command is %c." com))
      (if (null com)
	  (message "Value is %d, and command is nil." val)
	(message "Value is %d, and command is %c." val com)))))

(defun vip-digit-argument (arg)
  "Begin numeric argument for the next command."
  (interactive "P")
  (vip-prefix-arg-value last-command-event nil
			(if (consp arg) (cdr arg) nil)))

(defun vip-command-argument (arg)
  "Accept a motion command as an argument."
  (interactive "P")
  (condition-case conditions
      (vip-prefix-arg-com
       last-command-event
       (cond ((null arg) nil)
	     ((consp arg) (car arg))
	     ((numberp arg) arg)
	     (t (error "strange arg")))
       (cond ((null arg) nil)
	     ((consp arg) (cdr arg))
	     ((numberp arg) nil)
	     (t (error "strange arg"))))
    (quit
     (setq vip-use-register nil)
     (signal 'quit nil))))

(defun vip-p-val (arg)
  "Get value part of prefix-argument ARG."
  (cond ((null arg) 1)
	((consp arg) (if (null (car arg)) 1 (car arg)))
	(t arg)))

(defun vip-P-val (arg)
  "Get value part of prefix-argument ARG."
  (cond ((consp arg) (car arg))
	(t arg)))

(defun vip-getcom (arg)
  "Get com part of prefix-argument ARG."
  (cond ((null arg) nil)
	((consp arg) (cdr arg))
	(t nil)))

(defun vip-getCom (arg)
  "Get com part of prefix-argument ARG and modify it."
  (let ((com (vip-getcom arg)))
    (cond ((equal com ?c) ?C)
	  ((equal com ?d) ?D)
	  ((equal com ?y) ?Y)
	  (t com))))


;; repeat last destructive command

(defun vip-append-to-register (reg start end)
  "Append region to text in register REG.
START and END are buffer positions indicating what to append."
  (set-register reg (concat (or (get-register reg) "")
			    (buffer-substring start end))))

(defun vip-execute-com (m-com val com)
  "(M-COM VAL COM)  Execute command COM. The list (M-COM VAL COM) is set
to vip-d-com for later use by vip-repeat"
  (let ((reg vip-use-register))
    (if com
	(cond ((= com ?c) (vip-change vip-com-point (point)))
	      ((= com (- ?c)) (vip-change-subr vip-com-point (point)))
	      ((or (= com ?C) (= com (- ?C)))
	       (save-excursion
		 (set-mark vip-com-point)
		 (vip-enlarge-region (mark) (point))
		 (if vip-use-register
		     (progn
		       (cond ((and (<= ?a vip-use-register)
				   (<= vip-use-register ?z))
			      (copy-to-register
			       vip-use-register (mark) (point) nil))
			     ((and (<= ?A vip-use-register)
				   (<= vip-use-register ?Z))
			      (vip-append-to-register
			       (+ vip-use-register 32) (mark) (point)))
			     (t (setq vip-use-register nil)
				(error "")))
		       (setq vip-use-register nil)))
		 (delete-region (mark) (point)))
	       (open-line 1)
	       (if (= com ?C) (vip-change-mode-to-insert) (yank)))
	      ((= com ?d)
	       (if vip-use-register
		   (progn
		     (cond ((and (<= ?a vip-use-register)
				 (<= vip-use-register ?z))
			    (copy-to-register
			     vip-use-register vip-com-point (point) nil))
			   ((and (<= ?A vip-use-register)
				 (<= vip-use-register ?Z))
			    (vip-append-to-register
			     (+ vip-use-register 32) vip-com-point (point)))
			   (t (setq vip-use-register nil)
			      (error "")))
		     (setq vip-use-register nil)))
	       (setq last-command
		     (if (eq last-command 'd-command) 'kill-region nil))
	       (kill-region vip-com-point (point))
	       (setq this-command 'd-command))
	      ((= com ?D)
	       (save-excursion
		 (set-mark vip-com-point)
		 (vip-enlarge-region (mark) (point))
		 (if vip-use-register
		     (progn
		       (cond ((and (<= ?a vip-use-register)
				   (<= vip-use-register ?z))
			      (copy-to-register
			       vip-use-register (mark) (point) nil))
			     ((and (<= ?A vip-use-register)
				   (<= vip-use-register ?Z))
			      (vip-append-to-register
			       (+ vip-use-register 32) (mark) (point)))
			     (t (setq vip-use-register nil)
				(error "")))
		       (setq vip-use-register nil)))
		 (setq last-command
		       (if (eq last-command 'D-command) 'kill-region nil))
		 (kill-region (mark) (point))
		 (if (eq m-com 'vip-line) (setq this-command 'D-command)))
	       (back-to-indentation))
	      ((= com ?y)
	       (if vip-use-register
		   (progn
		     (cond ((and (<= ?a vip-use-register)
				 (<= vip-use-register ?z))
			    (copy-to-register
			     vip-use-register vip-com-point (point) nil))
			   ((and (<= ?A vip-use-register)
				 (<= vip-use-register ?Z))
			    (vip-append-to-register
			     (+ vip-use-register 32) vip-com-point (point)))
			   (t (setq vip-use-register nil)
			      (error "")))
		     (setq vip-use-register nil)))
	       (setq last-command nil)
	       (copy-region-as-kill vip-com-point (point))
	       (goto-char vip-com-point))
	      ((= com ?Y)
	       (save-excursion
		 (set-mark vip-com-point)
		 (vip-enlarge-region (mark) (point))
		 (if vip-use-register
		     (progn
		       (cond ((and (<= ?a vip-use-register)
				   (<= vip-use-register ?z))
			      (copy-to-register
			       vip-use-register (mark) (point) nil))
			     ((and (<= ?A vip-use-register)
				   (<= vip-use-register ?Z))
			      (vip-append-to-register
			       (+ vip-use-register 32) (mark) (point)))
			     (t (setq vip-use-register nil)
				(error "")))
		       (setq vip-use-register nil)))
		 (setq last-command nil)
		 (copy-region-as-kill (mark) (point)))
	       (goto-char vip-com-point))
	      ((or (= com ?!) (= com (- ?!)))
	       (save-excursion
		 (set-mark vip-com-point)
		 (vip-enlarge-region (mark) (point))
		 (shell-command-on-region
		  (mark) (point)
		  (if (= com ?!)
		      (setq vip-last-shell-com (vip-read-string "!"))
		    vip-last-shell-com)
		  t)))
	      ((= com ?=)
	       (save-excursion
		 (set-mark vip-com-point)
		 (vip-enlarge-region (mark) (point))
		 (if (> (mark) (point)) (exchange-point-and-mark))
		 (indent-region (mark) (point) nil)))
	      ((= com ?<)
	       (save-excursion
		 (set-mark vip-com-point)
		 (vip-enlarge-region (mark) (point))
		 (indent-rigidly (mark) (point) (- vip-shift-width)))
	       (goto-char vip-com-point))
	      ((= com ?>)
	       (save-excursion
		 (set-mark vip-com-point)
		 (vip-enlarge-region (mark) (point))
		 (indent-rigidly (mark) (point) vip-shift-width))
	       (goto-char vip-com-point))
	      ((>= com 128)
	       ;; this is special command #
	       (vip-special-prefix-com (- com 128)))))
    (setq vip-d-com (list m-com val (if (or (= com ?c) (= com ?C) (= com ?!))
					(- com) com)
			  reg))))

(defun vip-repeat (arg)
  "(ARG)  Re-execute last destructive command.  vip-d-com has the form
\(COM ARG CH REG), where COM is the command to be re-executed, ARG is the
argument for COM, CH is a flag for repeat, and REG is optional and if exists
is the name of the register for COM."
  (interactive "P")
  (if (eq last-command 'vip-undo)
      ;; if the last command was vip-undo, then undo-more
      (vip-undo-more)
    ;; otherwise execute the command stored in vip-d-com.  if arg is non-nil
    ;; its prefix value is used as new prefix value for the command.
    (let ((m-com (car vip-d-com))
	  (val (vip-P-val arg))
	  (com (car (cdr (cdr vip-d-com))))
	  (reg (nth 3 vip-d-com)))
      (if (null val) (setq val (car (cdr vip-d-com))))
      (if (null m-com) (error "No previous command to repeat"))
      (setq vip-use-register reg)
      (funcall m-com (cons val com)))))

(defun vip-special-prefix-com (char)
  "This command is invoked interactively by the key sequence #<char>"
  (cond ((= char ?c)
	 (downcase-region (min vip-com-point (point))
			  (max vip-com-point (point))))
	((= char ?C)
	 (upcase-region (min vip-com-point (point))
			(max vip-com-point (point))))
	((= char ?g)
	 (set-mark vip-com-point)
	 (vip-global-execute))
	((= char ?q)
	 (set-mark vip-com-point)
	 (vip-quote-region))
	((= char ?s) (ispell-region vip-com-point (point)))))


;; undoing

(defun vip-undo ()
  "Undo previous change."
  (interactive)
  (message "undo!")
  (undo-start)
  (undo-more 2)
  (setq this-command 'vip-undo))

(defun vip-undo-more ()
  "Continue undoing previous changes."
  (message "undo more!")
  (undo-more 1)
  (setq this-command 'vip-undo))


;; utilities

(defun vip-string-tail (str)
  (if (or (null str) (string= str "")) nil
    (substring str 1)))

(defun vip-yank-defun ()
  (mark-defun)
  (copy-region-as-kill (point) (mark)))

(defun vip-enlarge-region (beg end)
  "Enlarge region between BEG and END."
  (if (< beg end)
      (progn (goto-char beg) (set-mark end))
    (goto-char end)
    (set-mark beg))
  (beginning-of-line)
  (exchange-point-and-mark)
  (if (or (not (eobp)) (not (bolp))) (with-no-warnings (next-line 1)))
  (beginning-of-line)
  (if (> beg end) (exchange-point-and-mark)))

(defun vip-global-execute ()
  "Call last keyboard macro for each line in the region."
  (if (> (point) (mark)) (exchange-point-and-mark))
  (beginning-of-line)
  (call-last-kbd-macro)
  (while (< (point) (mark))
    (forward-line 1)
    (beginning-of-line)
    (call-last-kbd-macro)))

(defun vip-quote-region ()
  "Quote region by inserting the user supplied string at the beginning of
each line in the region."
  (setq vip-quote-string
	(let ((str
	       (vip-read-string (format "quote string (default %s): "
					vip-quote-string))))
	  (if (string= str "") vip-quote-string str)))
  (vip-enlarge-region (point) (mark))
  (if (> (point) (mark)) (exchange-point-and-mark))
  (insert vip-quote-string)
  (beginning-of-line)
  (forward-line 1)
  (while (and (< (point) (mark)) (bolp))
    (insert vip-quote-string)
    (beginning-of-line)
    (forward-line 1)))

(defun vip-end-with-a-newline-p (string)
  "Check if the string ends with a newline."
  (or (string= string "")
      (= (aref string (1- (length string))) ?\n)))

(defvar vip-save-minibuffer-local-map)

(defun vip-read-string (prompt &optional init)
  (setq vip-save-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key minibuffer-local-map "\C-h" 'backward-char)
  (define-key minibuffer-local-map "\C-w" 'backward-word)
  (define-key minibuffer-local-map "\e" 'exit-minibuffer)
  (let (str)
    (condition-case conditions
	(setq str (read-string prompt init))
      (quit
       (setq minibuffer-local-map vip-save-minibuffer-local-map)
       (signal 'quit nil)))
    (setq minibuffer-local-map vip-save-minibuffer-local-map)
    str))


;; insertion commands

(defun vip-repeat-insert-command ()
  "This function is called when mode changes from insertion mode to
vi command mode.  It will repeat the insertion command if original insertion
command was invoked with argument > 1."
  (let ((i-com (car vip-d-com)) (val (car (cdr vip-d-com))))
    (if (and val (> val 1)) ;; first check that val is non-nil
	(progn
	  (setq vip-d-com (list i-com (1- val) ?r))
	  (vip-repeat nil)
	  (setq vip-d-com (list i-com val ?r))))))

(defun vip-insert (arg) ""
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-insert val ?r))
    (if com (vip-loop val (yank))
      (vip-change-mode-to-insert))))

(defun vip-append (arg)
  "Append after point."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-append val ?r))
    (if (not (eolp)) (forward-char))
    (if (equal com ?r)
	(vip-loop val (yank))
      (vip-change-mode-to-insert))))

(defun vip-Append (arg)
  "Append at end of line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-Append val ?r))
    (end-of-line)
    (if (equal com ?r)
	(vip-loop val (yank))
      (vip-change-mode-to-insert))))

(defun vip-Insert (arg)
  "Insert before first non-white."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-Insert val ?r))
    (back-to-indentation)
    (if (equal com ?r)
	(vip-loop val (yank))
      (vip-change-mode-to-insert))))

(defun vip-open-line (arg)
  "Open line below."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-open-line val ?r))
    (let ((col (current-indentation)))
      (if (equal com ?r)
	  (vip-loop val
		(progn
		  (end-of-line)
		  (newline 1)
		  (if vip-open-with-indent (indent-to col))
		  (yank)))
	(end-of-line)
	(newline 1)
	(if vip-open-with-indent (indent-to col))
	(vip-change-mode-to-insert)))))

(defun vip-Open-line (arg)
  "Open line above."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
  (setq vip-d-com (list 'vip-Open-line val ?r))
  (let ((col (current-indentation)))
    (if (equal com ?r)
	(vip-loop val
	      (progn
		(beginning-of-line)
		(open-line 1)
		(if vip-open-with-indent (indent-to col))
		(yank)))
      (beginning-of-line)
      (open-line 1)
      (if vip-open-with-indent (indent-to col))
      (vip-change-mode-to-insert)))))

(defun vip-open-line-at-point (arg)
  "Open line at point."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-open-line-at-point val ?r))
    (if (equal com ?r)
	(vip-loop val
	      (progn
		(open-line 1)
		(yank)))
      (open-line 1)
      (vip-change-mode-to-insert))))

(defun vip-substitute (arg)
  "Substitute characters."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (save-excursion
      (set-mark (point))
      (forward-char val)
      (if (equal com ?r)
	  (vip-change-subr (mark) (point))
	(vip-change (mark) (point))))
    (setq vip-d-com (list 'vip-substitute val ?r))))

(defun vip-substitute-line (arg)
  "Substitute lines."
  (interactive "p")
  (vip-line (cons arg ?C)))


;; line command

(defun vip-line (arg)
  (let ((val (car arg)) (com (cdr arg)))
    (move-marker vip-com-point (point))
    (with-no-warnings (next-line (1- val)))
    (vip-execute-com 'vip-line val com)))

(defun vip-yank-line (arg)
  "Yank ARG lines (in vi's sense)"
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (vip-line (cons val ?Y))))


;; region command

(defun vip-region (arg)
  (interactive "P")
  (let ((val (vip-P-val arg))
	(com (vip-getcom arg)))
    (move-marker vip-com-point (point))
    (exchange-point-and-mark)
    (vip-execute-com 'vip-region val com)))

(defun vip-Region (arg)
  (interactive "P")
  (let ((val (vip-P-val arg))
	(com (vip-getCom arg)))
    (move-marker vip-com-point (point))
    (exchange-point-and-mark)
    (vip-execute-com 'vip-Region val com)))

(defun vip-replace-char (arg)
  "Replace the following ARG chars by the character read."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-replace-char val ?r))
    (vip-replace-char-subr (if (equal com ?r) vip-d-char (read-char)) val)))

(defun vip-replace-char-subr (char arg)
  (delete-char arg t)
  (setq vip-d-char char)
  (vip-loop (if (> arg 0) arg (- arg)) (insert char))
  (backward-char arg))

(defun vip-replace-string ()
  "Replace string.  If you supply null string as the string to be replaced,
the query replace mode will toggle between string replace and regexp replace."
  (interactive)
  (let (str)
    (setq str (vip-read-string
	       (if vip-re-replace "Replace regexp: " "Replace string: ")))
    (if (string= str "")
	(progn
	  (setq vip-re-replace (not vip-re-replace))
	  (message "Replace mode changed to %s."
		   (if vip-re-replace "regexp replace"
		     "string replace")))
      (if vip-re-replace
	  ;; (replace-regexp
	  ;;  str
	  ;;  (vip-read-string (format "Replace regexp \"%s\" with: " str)))
	  (while (re-search-forward str nil t)
	    (replace-match (vip-read-string
			    (format "Replace regexp \"%s\" with: " str))
			   nil nil))
	(with-no-warnings
	  (replace-string
	   str
	   (vip-read-string (format "Replace \"%s\" with: " str))))))))


;; basic cursor movement.  j, k, l, m commands.

(defun vip-forward-char (arg)
  "Move point right ARG characters (left if ARG negative).On reaching end
of buffer, stop and signal error."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (forward-char val)
    (if com (vip-execute-com 'vip-forward-char val com))))

(defun vip-backward-char (arg)
  "Move point left ARG characters (right if ARG negative).  On reaching
beginning of buffer, stop and signal error."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (backward-char val)
    (if com (vip-execute-com 'vip-backward-char val com))))


;; word command

(defun vip-forward-word (arg)
  "Forward word."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (forward-word val)
    (skip-chars-forward " \t\n")
    (if com
	(progn
	  (if (or (= com ?c) (= com (- ?c)))
	      (progn (backward-word 1) (forward-word 1)))
	  (if (or (= com ?d) (= com ?y))
	      (progn
		(backward-word 1)
		(forward-word 1)
		(skip-chars-forward " \t")))
	  (vip-execute-com 'vip-forward-word val com)))))

(defun vip-end-of-word (arg)
  "Move point to end of current word."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (forward-char)
    (forward-word val)
    (backward-char)
    (if com
	(progn
	  (forward-char)
	  (vip-execute-com 'vip-end-of-word val com)))))

(defun vip-backward-word (arg)
  "Backward word."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (backward-word val)
    (if com (vip-execute-com 'vip-backward-word val com))))

(defun vip-forward-Word (arg)
  "Forward word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (re-search-forward "[^ \t\n]*[ \t\n]+" nil t val)
    (if com
	(progn
	  (if (or (= com ?c) (= com (- ?c)))
	      (progn (backward-word 1) (forward-word 1)))
	  (if (or (= com ?d) (= com ?y))
	      (progn
		(backward-word 1)
		(forward-word 1)
		(skip-chars-forward " \t")))
	  (vip-execute-com 'vip-forward-Word val com)))))

(defun vip-end-of-Word (arg)
  "Move forward to end of word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (forward-char)
    (if (re-search-forward "[^ \t\n]+" nil t val) (backward-char))
    (if com
	(progn
	  (forward-char)
	  (vip-execute-com 'vip-end-of-Word val com)))))

(defun vip-backward-Word (arg)
  "Backward word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (if (re-search-backward "[ \t\n]+[^ \t\n]+" nil t val)
	(forward-char)
      (goto-char (point-min)))
    (if com (vip-execute-com 'vip-backward-Word val com))))

(defun vip-beginning-of-line (arg)
  "Go to beginning of line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (beginning-of-line val)
    (if com (vip-execute-com 'vip-beginning-of-line val com))))

(defun vip-bol-and-skip-white (arg)
  "Beginning of line at first non-white character."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (back-to-indentation)
    (if com (vip-execute-com 'vip-bol-and-skip-white val com))))

(defun vip-goto-eol (arg)
  "Go to end of line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (end-of-line val)
    (if com (vip-execute-com 'vip-goto-eol val com))))

(defun vip-next-line (arg)
  "Go to next line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (line-move val)
    (setq this-command 'next-line)
    (if com (vip-execute-com 'vip-next-line val com))))

(defun vip-next-line-at-bol (arg)
  "Next line at beginning of line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (with-no-warnings (next-line val))
    (back-to-indentation)
    (if com (vip-execute-com 'vip-next-line-at-bol val com))))

(defun vip-previous-line (arg)
  "Go to previous line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (with-no-warnings (next-line (- val)))
    (setq this-command 'previous-line)
    (if com (vip-execute-com 'vip-previous-line val com))))

(defun vip-previous-line-at-bol (arg)
  "Previous line at beginning of line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (with-no-warnings (next-line (- val)))
    (back-to-indentation)
    (if com (vip-execute-com 'vip-previous-line val com))))

(defun vip-change-to-eol (arg)
  "Change to end of line."
  (interactive "P")
  (vip-goto-eol (cons arg ?c)))

(defun vip-kill-line (arg)
  "Delete line."
  (interactive "P")
  (vip-goto-eol (cons arg ?d)))


;; moving around

(defun vip-goto-line (arg)
  "Go to ARG's line.  Without ARG go to end of buffer."
  (interactive "P")
  (let ((val (vip-P-val arg)) (com (vip-getCom arg)))
    (move-marker vip-com-point (point))
    (set-mark (point))
    (if (null val)
	(goto-char (point-max))
      (goto-char (point-min))
      (forward-line (1- val)))
    (back-to-indentation)
    (if com (vip-execute-com 'vip-goto-line val com))))

(defun vip-find-char (arg char forward offset)
  "Find ARG's occurrence of CHAR on the current line.  If FORWARD then
search is forward, otherwise backward.  OFFSET is used to adjust point
after search."
  (let ((arg (if forward arg (- arg))) point)
    (save-excursion
      (save-restriction
	(if (> arg 0)
	    (narrow-to-region
	     ;; forward search begins here
	     (if (eolp) (error "") (point))
	     ;; forward search ends here
	     (progn (with-no-warnings (next-line 1)) (beginning-of-line) (point)))
	  (narrow-to-region
	   ;; backward search begins from here
	   (if (bolp) (error "") (point))
	   ;; backward search ends here
	   (progn (beginning-of-line) (point))))
	;; if arg > 0, point is forwarded before search.
	(if (> arg 0) (goto-char (1+ (point-min)))
	  (goto-char (point-max)))
	(let ((case-fold-search nil))
	  (search-forward (char-to-string char) nil 0 arg))
	(setq point (point))
	(if (or (and (> arg 0) (= point (point-max)))
		(and (< arg 0) (= point (point-min))))
	    (error ""))))
    (goto-char (+ point (if (> arg 0) (if offset -2 -1) (if offset 1 0))))))

(defun vip-find-char-forward (arg)
  "Find char on the line.  If called interactively read the char to find
from the terminal, and if called from vip-repeat, the char last used is
used.  This behavior is controlled by the sign of prefix numeric value."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward t
	      vip-f-offset nil)
      (setq val (- val)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) t nil)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char);; set new vip-F-char
	  (forward-char)
	  (vip-execute-com 'vip-find-char-forward val com)))))

(defun vip-goto-char-forward (arg)
  "Go up to char ARG forward on line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward t
	      vip-f-offset t)
      (setq val (- val)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) t t)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char);; set new vip-F-char
	  (forward-char)
	  (vip-execute-com 'vip-goto-char-forward val com)))))

(defun vip-find-char-backward (arg)
  "Find char ARG on line backward."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward nil
	      vip-f-offset nil)
      (setq val (- val)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char
     val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) nil nil)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char);; set new vip-F-char
	  (vip-execute-com 'vip-find-char-backward val com)))))

(defun vip-goto-char-backward (arg)
  "Go up to char ARG backward on line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward nil
	      vip-f-offset t)
      (setq val (- val)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) nil t)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char);; set new vip-F-char
	  (vip-execute-com 'vip-goto-char-backward val com)))))

(defun vip-repeat-find (arg)
  "Repeat previous find command."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char val vip-f-char vip-f-forward vip-f-offset)
    (if com
	(progn
	  (if vip-f-forward (forward-char))
	  (vip-execute-com 'vip-repeat-find val com)))))

(defun vip-repeat-find-opposite (arg)
  "Repeat previous find command in the opposite direction."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char val vip-f-char (not vip-f-forward) vip-f-offset)
    (if com
	(progn
	  (if vip-f-forward (forward-char))
	  (vip-execute-com 'vip-repeat-find-opposite val com)))))


;; window scrolling etc.

(defun vip-other-window (arg)
  "Switch to other window."
  (interactive "p")
  (other-window arg)
  (or (not (eq vip-current-mode 'emacs-mode))
      (string= (buffer-name (current-buffer)) " *Minibuf-1*")
      (vip-change-mode-to-vi)))

(defun vip-window-top (arg)
  "Go to home window line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (move-to-window-line (1- val))
    (if com (vip-execute-com 'vip-window-top val com))))

(defun vip-window-middle (arg)
  "Go to middle window line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (move-to-window-line (+ (/ (1- (window-height)) 2) (1- val)))
    (if com (vip-execute-com 'vip-window-middle val com))))

(defun vip-window-bottom (arg)
  "Go to last window line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (move-to-window-line (- val))
    (if com (vip-execute-com 'vip-window-bottom val com))))

(defun vip-line-to-top (arg)
  "Put current line on the home line."
  (interactive "p")
  (recenter (1- arg)))

(defun vip-line-to-middle (arg)
  "Put current line on the middle line."
  (interactive "p")
  (recenter (+ (1- arg) (/ (1- (window-height)) 2))))

(defun vip-line-to-bottom (arg)
  "Put current line on the last line."
  (interactive "p")
  (recenter (- (window-height) (1+ arg))))


;; paren match

(defun vip-paren-match (arg)
  "Go to the matching parenthesis."
  (interactive "P")
  (let ((com (vip-getcom arg)))
    (if (numberp arg)
	(if (or (> arg 99) (< arg 1))
	    (error "Prefix must be between 1 and 99")
	  (goto-char
	   (if (> (point-max) 80000)
	       (* (/ (point-max) 100) arg)
	     (/ (* (point-max) arg) 100)))
	  (back-to-indentation))
    (cond ((looking-at "[\(\[{]")
	   (if com (move-marker vip-com-point (point)))
	   (forward-sexp 1)
	   (if com
	       (vip-execute-com 'vip-paren-match nil com)
	     (backward-char)))
	  ((looking-at "[])}]")
	   (forward-char)
	   (if com (move-marker vip-com-point (point)))
	   (backward-sexp 1)
	   (if com (vip-execute-com 'vip-paren-match nil com)))
	  (t (error ""))))))


;; sentence and paragraph

(defun vip-forward-sentence (arg)
  "Forward sentence."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (forward-sentence val)
    (if com (vip-execute-com 'vip-forward-sentence nil com))))

(defun vip-backward-sentence (arg)
  "Backward sentence."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (backward-sentence val)
    (if com (vip-execute-com 'vip-backward-sentence nil com))))

(defun vip-forward-paragraph (arg)
  "Forward paragraph."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (forward-paragraph val)
    (if com (vip-execute-com 'vip-forward-paragraph nil com))))

(defun vip-backward-paragraph (arg)
  "Backward paragraph."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (backward-paragraph val)
    (if com (vip-execute-com 'vip-backward-paragraph nil com))))


;; scrolling

(defun vip-scroll (arg)
  "Scroll to next screen."
  (interactive "p")
  (if (> arg 0)
      (while (> arg 0)
	(scroll-up)
	(setq arg (1- arg)))
    (while (> 0 arg)
      (scroll-down)
      (setq arg (1+ arg)))))

(defun vip-scroll-back (arg)
  "Scroll to previous screen."
  (interactive "p")
  (vip-scroll (- arg)))

(defun vip-scroll-down (arg)
  "Scroll up half screen."
  (interactive "P")
  (if (null arg) (scroll-down (/ (window-height) 2))
    (scroll-down arg)))

(defun vip-scroll-down-one (arg)
  "Scroll up one line."
  (interactive "p")
  (scroll-down arg))

(defun vip-scroll-up (arg)
  "Scroll down half screen."
  (interactive "P")
  (if (null arg) (scroll-up (/ (window-height) 2))
    (scroll-up arg)))

(defun vip-scroll-up-one (arg)
  "Scroll down one line."
  (interactive "p")
  (scroll-up arg))


;; splitting window

(defun vip-buffer-in-two-windows ()
  "Show current buffer in two windows."
  (interactive)
  (delete-other-windows)
  (split-window-below))


;; searching

(defun vip-search-forward (arg)
  "Search a string forward.  ARG is used to find the ARG's occurrence
of the string.  Default is vanilla search.  Search mode can be toggled by
giving null search string."
  (interactive "P")
  (let ((val (vip-P-val arg)) (com (vip-getcom arg)))
    (setq vip-s-forward t
	  vip-s-string (vip-read-string (if vip-re-search "RE-/" "/")))
    (if (string= vip-s-string "")
	(progn
	  (setq vip-re-search (not vip-re-search))
	  (message "Search mode changed to %s search."
		   (if vip-re-search "regular expression"
		     "vanilla")))
      (vip-search vip-s-string t val)
      (if com
	  (progn
	    (move-marker vip-com-point (mark))
	    (vip-execute-com 'vip-search-next val com))))))

(defun vip-search-backward (arg)
  "Search a string backward.  ARG is used to find the ARG's occurrence
of the string.  Default is vanilla search.  Search mode can be toggled by
giving null search string."
  (interactive "P")
  (let ((val (vip-P-val arg)) (com (vip-getcom arg)))
    (setq vip-s-forward nil
	  vip-s-string (vip-read-string (if vip-re-search "RE-?" "?")))
    (if (string= vip-s-string "")
	(progn
	  (setq vip-re-search (not vip-re-search))
	  (message "Search mode changed to %s search."
		   (if vip-re-search "regular expression"
		     "vanilla")))
      (vip-search vip-s-string nil val)
      (if com
	  (progn
	    (move-marker vip-com-point (mark))
	    (vip-execute-com 'vip-search-next val com))))))

(defun vip-search (string forward arg &optional no-offset init-point)
  "(STRING FORWARD COUNT &optional NO-OFFSET) Search COUNT's occurrence of
STRING.  Search will be forward if FORWARD, otherwise backward."
  (let ((val (vip-p-val arg)) (com (vip-getcom arg))
	(null-arg (null (vip-P-val arg))) (offset (not no-offset))
	(case-fold-search vip-case-fold-search)
	(start-point (or init-point (point))))
    (if forward
	(condition-case conditions
	    (progn
	      (if (and offset (not (eobp))) (forward-char))
	      (if vip-re-search
		  (progn
		    (re-search-forward string nil nil val)
		    (re-search-backward string))
		(search-forward string nil nil val)
		(search-backward string))
	      (push-mark start-point))
	  (search-failed
	   (if (and null-arg vip-search-wrap-around)
	       (progn
		 (goto-char (point-min))
		 (vip-search string forward (cons 1 com) t start-point))
	     (goto-char start-point)
	     (signal 'search-failed (cdr conditions)))))
      (condition-case conditions
	    (progn
	      (if vip-re-search
		    (re-search-backward string nil nil val)
		(search-backward string nil nil val))
	      (push-mark start-point))
	  (search-failed
	   (if (and null-arg vip-search-wrap-around)
	       (progn
		 (goto-char (point-max))
		 (vip-search string forward (cons 1 com) t start-point))
	     (goto-char start-point)
	     (signal 'search-failed (cdr conditions))))))))

(defun vip-search-next (arg)
  "Repeat previous search."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (null vip-s-string) (error "No previous search string"))
    (vip-search vip-s-string vip-s-forward arg)
    (if com (vip-execute-com 'vip-search-next val com))))

(defun vip-search-Next (arg)
  "Repeat previous search in the reverse direction."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (null vip-s-string) (error "No previous search string"))
    (vip-search vip-s-string (not vip-s-forward) arg)
    (if com (vip-execute-com 'vip-search-Next val com))))


;; visiting and killing files, buffers

(defun vip-switch-to-buffer ()
  "Switch to buffer in the current window."
  (interactive)
  (let (buffer)
    (setq buffer
	  (read-buffer
	   (format "switch to buffer \(%s\): "
		   (buffer-name (other-buffer (current-buffer))))))
    (switch-to-buffer buffer)
    (vip-change-mode-to-vi)))

(defun vip-switch-to-buffer-other-window ()
  "Switch to buffer in another window."
  (interactive)
  (let (buffer)
    (setq buffer
	  (read-buffer
	   (format "Switch to buffer \(%s\): "
		   (buffer-name (other-buffer (current-buffer))))))
    (switch-to-buffer-other-window buffer)
    (vip-change-mode-to-vi)))

(defun vip-kill-buffer ()
  "Kill a buffer."
  (interactive)
  (let (buffer buffer-name)
    (setq buffer-name
	  (read-buffer
	   (format "Kill buffer \(%s\): "
		   (buffer-name (current-buffer)))))
    (setq buffer
	  (if (null buffer-name)
	      (current-buffer)
	    (get-buffer buffer-name)))
    (if (null buffer) (error "Buffer %s nonexistent" buffer-name))
    (if (or (not (buffer-modified-p buffer))
	    (y-or-n-p "Buffer is modified, are you sure? "))
	(kill-buffer buffer)
      (error "Buffer not killed"))))

(defun vip-find-file ()
  "Visit file in the current window."
  (interactive)
  (let (file)
    (setq file (read-file-name "visit file: "))
    (switch-to-buffer (find-file-noselect file))
    (vip-change-mode-to-vi)))

(defun vip-find-file-other-window ()
  "Visit file in another window."
  (interactive)
  (let (file)
    (setq file (read-file-name "Visit file: "))
    (switch-to-buffer-other-window (find-file-noselect file))
    (vip-change-mode-to-vi)))

(defun vip-info-on-file ()
  "Give information of the file associated to the current buffer."
  (interactive)
  (message "\"%s\" line %d of %d"
	   (if (buffer-file-name) (buffer-file-name) "")
	   (1+ (count-lines (point-min) (point)))
	   (1+ (count-lines (point-min) (point-max)))))


;; yank and pop

(defun vip-yank (text)
  "yank TEXT silently."
  (save-excursion
    (vip-push-mark-silent (point))
    (insert text)
    (exchange-point-and-mark))
  (skip-chars-forward " \t"))

(defun vip-put-back (arg)
  "Put back after point/below line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(text (if vip-use-register
		  (if (and (<= ?1 vip-use-register) (<= vip-use-register ?9))
		      (current-kill (- vip-use-register ?1) 'do-not-rotate)
		    (get-register vip-use-register))
		(current-kill 0))))
    (if (null text)
	(if vip-use-register
	    (let ((reg vip-use-register))
	      (setq vip-use-register nil)
	      (error "Nothing in register %c" reg))
	  (error "")))
    (setq vip-use-register nil)
    (if (vip-end-with-a-newline-p text)
	(progn
	  (with-no-warnings (next-line 1))
	  (beginning-of-line))
      (if (and (not (eolp)) (not (eobp))) (forward-char)))
    (setq vip-d-com (list 'vip-put-back val nil vip-use-register))
    (vip-loop val (vip-yank text))))

(defun vip-Put-back (arg)
  "Put back at point/above line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(text (if vip-use-register
		  (if (and (<= ?1 vip-use-register) (<= vip-use-register ?9))
		      (current-kill (- vip-use-register ?1) 'do-not-rotate)
		    (get-register vip-use-register))
		(current-kill 0))))
    (if (null text)
	(if vip-use-register
	    (let ((reg vip-use-register))
	      (setq vip-use-register nil)
	      (error "Nothing in register %c" reg))
	  (error "")))
    (setq vip-use-register nil)
    (if (vip-end-with-a-newline-p text) (beginning-of-line))
    (setq vip-d-com (list 'vip-Put-back val nil vip-use-register))
    (vip-loop val (vip-yank text))))

(defun vip-delete-char (arg)
  "Delete character."
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (setq vip-d-com (list 'vip-delete-char val nil))
    (if vip-use-register
	(progn
	  (if (and (<= ?A vip-use-register) (<= vip-use-register ?Z))
	      (vip-append-to-register
	       (+ vip-use-register 32) (point) (- (point) val))
	    (copy-to-register vip-use-register (point) (- (point) val) nil))
	  (setq vip-use-register nil)))
    (delete-char val t)))

(defun vip-delete-backward-char (arg)
  "Delete previous character."
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (setq vip-d-com (list 'vip-delete-backward-char val nil))
    (if vip-use-register
	(progn
	  (if (and (<= ?A vip-use-register) (<= vip-use-register ?Z))
	      (vip-append-to-register
	       (+ vip-use-register 32) (point) (+ (point) val))
	    (copy-to-register vip-use-register (point) (+ (point) val) nil))
	  (setq vip-use-register nil)))
    (delete-backward-char val t)))


;; join lines.

(defun vip-join-lines (arg)
  "Join this line to next, if ARG is nil.  Otherwise, join ARG lines"
  (interactive "*P")
  (let ((val (vip-P-val arg)))
    (setq vip-d-com (list 'vip-join-lines val nil))
    (vip-loop (if (null val) 1 (1- val))
	  (progn
	    (end-of-line)
	    (if (not (eobp))
		(progn
		  (forward-line 1)
		  (delete-region (point) (1- (point)))
		  (fixup-whitespace)))))))


;; making small changes

(defvar vip-c-string)

(defun vip-change (beg end)
  (setq vip-c-string
	(vip-read-string (format "%s => " (buffer-substring beg end))))
  (vip-change-subr beg end))

(defun vip-change-subr (beg end)
  (if vip-use-register
      (progn
	(copy-to-register vip-use-register beg end nil)
	(setq vip-use-register nil)))
  (kill-region beg end)
  (setq this-command 'vip-change)
  (insert vip-c-string))


;; query replace

(defun vip-query-replace ()
  "Query replace.  If you supply null string as the string to be replaced,
the query replace mode will toggle between string replace and regexp replace."
  (interactive)
  (let (str)
    (setq str (vip-read-string
	       (if vip-re-query-replace "Query replace regexp: "
		 "Query replace: ")))
    (if (string= str "")
	(progn
	  (setq vip-re-query-replace (not vip-re-query-replace))
	  (message "Query replace mode changed to %s."
		   (if vip-re-query-replace "regexp replace"
		     "string replace")))
      (if vip-re-query-replace
	  (query-replace-regexp
	   str
	   (vip-read-string (format "Query replace regexp \"%s\" with: " str)))
	(query-replace
	 str
	 (vip-read-string (format "Query replace \"%s\" with: " str)))))))


;; marking

(defun vip-mark-beginning-of-buffer ()
  (interactive)
  (set-mark (point))
  (goto-char (point-min))
  (exchange-point-and-mark)
  (message "mark set at the beginning of buffer"))

(defun vip-mark-end-of-buffer ()
  (interactive)
  (set-mark (point))
  (goto-char (point-max))
  (exchange-point-and-mark)
  (message "mark set at the end of buffer"))

(defun vip-mark-point (char)
  (interactive "c")
  (cond ((and (<= ?a char) (<= char ?z))
	 (point-to-register (- char (- ?a ?\C-a)) nil))
	((= char ?<) (vip-mark-beginning-of-buffer))
	((= char ?>) (vip-mark-end-of-buffer))
	((= char ?.) (push-mark))
	((= char ?,) (set-mark-command 1))
	((= char ?D) (mark-defun))
	(t (error ""))))

(defun vip-goto-mark (arg)
  "Go to mark."
  (interactive "P")
  (let ((char (read-char)) (com (vip-getcom arg)))
    (vip-goto-mark-subr char com nil)))

(defun vip-goto-mark-and-skip-white (arg)
  "Go to mark and skip to first non-white on line."
  (interactive "P")
  (let ((char (read-char)) (com (vip-getCom arg)))
    (vip-goto-mark-subr char com t)))

(defun vip-goto-mark-subr (char com skip-white)
  (cond ((and (<= ?a char) (<= char ?z))
	 (let ((buff (current-buffer)))
	   (if com (move-marker vip-com-point (point)))
	   (goto-char (register-to-point (- char (- ?a ?\C-a))))
	   (if skip-white (back-to-indentation))
	   (vip-change-mode-to-vi)
	   (if com
	       (if (equal buff (current-buffer))
		   (vip-execute-com (if skip-white
					'vip-goto-mark-and-skip-white
				      'vip-goto-mark)
				    nil com)
		 (switch-to-buffer buff)
		 (goto-char vip-com-point)
		 (vip-change-mode-to-vi)
		 (error "")))))
	((and (not skip-white) (= char ?`))
	 (if com (move-marker vip-com-point (point)))
	 (exchange-point-and-mark)
	 (if com (vip-execute-com 'vip-goto-mark nil com)))
	((and skip-white (= char ?'))
	 (if com (move-marker vip-com-point (point)))
	 (exchange-point-and-mark)
	 (back-to-indentation)
	 (if com (vip-execute-com 'vip-goto-mark-and-skip-white nil com)))
	(t (error ""))))

(defun vip-exchange-point-and-mark ()
  (interactive)
  (exchange-point-and-mark)
  (back-to-indentation))

(defun vip-keyboard-quit ()
  "Abort partially formed or running command."
  (interactive)
  (setq vip-use-register nil)
  (keyboard-quit))

(defun vip-ctl-c-equivalent (arg)
  "Emulate C-c in Emacs mode."
  (interactive "P")
  (vip-ctl-key-equivalent "\C-c" arg))

(defun vip-ctl-x-equivalent (arg)
  "Emulate C-x in Emacs mode."
  (interactive "P")
  (vip-ctl-key-equivalent "\C-x" arg))

(defun vip-ctl-key-equivalent (key arg)
  (let ((char (read-char)))
    (if (and (<= ?A char) (<= char ?Z))
	(setq char (- char (- ?A ?\C-a))))
    (vip-escape-to-emacs arg (list (aref key 0) char))))

;; commands in insertion mode

(defun vip-delete-backward-word (arg)
  "Delete previous word."
  (interactive "p")
  (save-excursion
    (set-mark (point))
    (backward-word arg)
    (delete-region (point) (mark))))


;; implement ex commands

(defvar ex-token-type nil
  "type of token.  if non-nil, gives type of address.  if nil, it
is a command.")

(defvar ex-token nil
  "value of token.")

(defvar ex-addresses nil
  "list of ex addresses")

(defvar ex-flag nil
  "flag for ex flag")

(defvar ex-buffer nil
  "name of ex buffer")

(defvar ex-count nil
  "value of ex count")

(defvar ex-g-flag nil
  "flag for global command")

(defvar ex-g-variant nil
  "if t global command is executed on lines not matching ex-g-pat")

(defvar ex-reg-exp nil
  "save reg-exp used in substitute")

(defvar ex-repl nil
  "replace pattern for substitute")

(defvar ex-g-pat nil
  "pattern for global command")

(defvar ex-map (make-sparse-keymap)
  "save commands for mapped keys")

(defvar ex-tag nil
  "save ex tag")

(defvar ex-file nil)

(defvar ex-variant nil)

(defvar ex-offset nil)

(defvar ex-append nil)

(defun vip-nil ()
  (interactive)
  (error ""))

(defun vip-looking-back (str)
  "returns t if looking back reg-exp STR before point."
  (and (save-excursion (re-search-backward str nil t))
       (= (point) (match-end 0))))

(defun vip-check-sub (str)
  "check if ex-token is an initial segment of STR"
  (let ((length (length ex-token)))
    (if (and (<= length (length str))
	     (string= ex-token (substring str 0 length)))
	(setq ex-token str)
      (setq ex-token-type "non-command"))))

(defun vip-get-ex-com-subr ()
  "get a complete ex command"
  (set-mark (point))
  (re-search-forward "[a-z][a-z]*")
  (setq ex-token-type "command")
  (setq ex-token (buffer-substring (point) (mark)))
  (exchange-point-and-mark)
  (cond ((looking-at "a")
	 (cond ((looking-at "ab") (vip-check-sub "abbreviate"))
	       ((looking-at "ar") (vip-check-sub "args"))
	       (t (vip-check-sub "append"))))
	((looking-at "[bh]") (setq ex-token-type "non-command"))
	((looking-at "c")
	 (if (looking-at "co") (vip-check-sub "copy")
	   (vip-check-sub "change")))
	((looking-at "d") (vip-check-sub "delete"))
	((looking-at "e")
	 (if (looking-at "ex") (vip-check-sub "ex")
	   (vip-check-sub "edit")))
	((looking-at "f") (vip-check-sub "file"))
	((looking-at "g") (vip-check-sub "global"))
	((looking-at "i") (vip-check-sub "insert"))
	((looking-at "j") (vip-check-sub "join"))
	((looking-at "l") (vip-check-sub "list"))
	((looking-at "m")
	 (cond ((looking-at "map") (vip-check-sub "map"))
	       ((looking-at "mar") (vip-check-sub "mark"))
	       (t (vip-check-sub "move"))))
	((looking-at "n")
	 (if (looking-at "nu") (vip-check-sub "number")
	   (vip-check-sub "next")))
	((looking-at "o") (vip-check-sub "open"))
	((looking-at "p")
	 (cond ((looking-at "pre") (vip-check-sub "preserve"))
	       ((looking-at "pu") (vip-check-sub "put"))
	       (t (vip-check-sub "print"))))
	((looking-at "q") (vip-check-sub "quit"))
	((looking-at "r")
	 (cond ((looking-at "rec") (vip-check-sub "recover"))
	       ((looking-at "rew") (vip-check-sub "rewind"))
	       (t (vip-check-sub "read"))))
	((looking-at "s")
	 (cond ((looking-at "se") (vip-check-sub "set"))
	       ((looking-at "sh") (vip-check-sub "shell"))
	       ((looking-at "so") (vip-check-sub "source"))
	       ((looking-at "st") (vip-check-sub "stop"))
	       (t (vip-check-sub "substitute"))))
	((looking-at "t")
	 (if (looking-at "ta") (vip-check-sub "tag")
	   (vip-check-sub "t")))
	((looking-at "u")
	 (cond ((looking-at "una") (vip-check-sub "unabbreviate"))
	       ((looking-at "unm") (vip-check-sub "unmap"))
	       (t (vip-check-sub "undo"))))
	((looking-at "v")
	 (cond ((looking-at "ve") (vip-check-sub "version"))
	       ((looking-at "vi") (vip-check-sub "visual"))
	       (t (vip-check-sub "v"))))
	((looking-at "w")
	 (if (looking-at "wq") (vip-check-sub "wq")
	   (vip-check-sub "write")))
	((looking-at "x") (vip-check-sub "xit"))
	((looking-at "y") (vip-check-sub "yank"))
	((looking-at "z") (vip-check-sub "z")))
  (exchange-point-and-mark))

(defun vip-get-ex-token ()
  "get an ex-token which is either an address or a command.
a token has type \(command, address, end-mark\) and value."
  (with-current-buffer " *ex-working-space*"
    (skip-chars-forward " \t")
    (cond ((looking-at "[k#]")
	   (setq ex-token-type "command")
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  ((looking-at "[a-z]") (vip-get-ex-com-subr))
	  ((looking-at "\\.")
	   (forward-char 1)
	   (setq ex-token-type "dot"))
	  ((looking-at "[0-9]")
	   (set-mark (point))
	   (re-search-forward "[0-9]*")
	   (setq ex-token-type
		 (cond ((string= ex-token-type "plus") "add-number")
		       ((string= ex-token-type "minus") "sub-number")
		       (t "abs-number")))
	   (setq ex-token (string-to-number (buffer-substring (point) (mark)))))
	  ((looking-at "\\$")
	   (forward-char 1)
	   (setq ex-token-type "end"))
	  ((looking-at "%")
	   (forward-char 1)
	   (setq ex-token-type "whole"))
	  ((looking-at "+")
	   (cond ((or (looking-at "+[-+]") (looking-at "+[\n|]"))
		  (forward-char 1)
		  (insert "1")
		  (backward-char 1)
		  (setq ex-token-type "plus"))
		 ((looking-at "+[0-9]")
		  (forward-char 1)
		  (setq ex-token-type "plus"))
		 (t
		  (error "Badly formed address"))))
	  ((looking-at "-")
	   (cond ((or (looking-at "-[-+]") (looking-at "-[\n|]"))
		  (forward-char 1)
		  (insert "1")
		  (backward-char 1)
		  (setq ex-token-type "minus"))
		 ((looking-at "-[0-9]")
		  (forward-char 1)
		  (setq ex-token-type "minus"))
		 (t
		  (error "Badly formed address"))))
	  ((looking-at "/")
	   (forward-char 1)
	   (set-mark (point))
	   (let ((cont t))
	     (while (and (not (eolp)) cont)
	       ;;(re-search-forward "[^/]*/")
	       (re-search-forward "[^/]*\\(/\\|\n\\)")
	       (if (not (vip-looking-back "[^\\\\]\\(\\\\\\\\\\)*\\\\/"))
		   (setq cont nil))))
	   (backward-char 1)
	   (setq ex-token (buffer-substring (point) (mark)))
	   (if (looking-at "/") (forward-char 1))
	   (setq ex-token-type "search-forward"))
	  ((looking-at "\\?")
	   (forward-char 1)
	   (set-mark (point))
	   (let ((cont t))
	     (while (and (not (eolp)) cont)
	       ;;(re-search-forward "[^\\?]*\\?")
	       (re-search-forward "[^\\?]*\\(\\?\\|\n\\)")
	       (if (not (vip-looking-back "[^\\\\]\\(\\\\\\\\\\)*\\\\\\?"))
		   (setq cont nil))
	       (backward-char 1)
	       (if (not (looking-at "\n")) (forward-char 1))))
	   (setq ex-token-type "search-backward")
	   (setq ex-token (buffer-substring (1- (point)) (mark))))
	  ((looking-at ",")
	   (forward-char 1)
	   (setq ex-token-type "comma"))
	  ((looking-at ";")
	   (forward-char 1)
	   (setq ex-token-type "semi-colon"))
	  ((looking-at "[!=><&~]")
	   (setq ex-token-type "command")
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  ((looking-at "'")
	   (setq ex-token-type "goto-mark")
	   (forward-char 1)
	   (cond ((looking-at "'") (setq ex-token nil))
		 ((looking-at "[a-z]") (setq ex-token (following-char)))
		 (t (error "Marks are ' and a-z")))
	   (forward-char 1))
	  ((looking-at "\n")
	   (setq ex-token-type "end-mark")
	   (setq ex-token "goto"))
	  (t
	   (error "invalid token")))))

(defun vip-ex (&optional string)
  "ex commands within VIP."
  (interactive)
  (or string
      (setq ex-g-flag nil
	    ex-g-variant nil))
  (let ((com-str (or string (vip-read-string ":")))
	(address nil) (cont t) (dot (point)))
    (with-current-buffer (get-buffer-create " *ex-working-space*")
      (delete-region (point-min) (point-max))
      (insert com-str "\n")
      (goto-char (point-min)))
    (setq ex-token-type "")
    (setq ex-addresses nil)
    (while cont
      (vip-get-ex-token)
      (cond ((or (string= ex-token-type "command")
		 (string= ex-token-type "end-mark"))
	     (if address (setq ex-addresses (cons address ex-addresses)))
	     (cond ((string= ex-token "global")
		    (ex-global nil)
		    (setq cont nil))
		   ((string= ex-token "v")
		    (ex-global t)
		    (setq cont nil))
		   (t
		    (vip-execute-ex-command)
		    (with-current-buffer " *ex-working-space*"
		      (skip-chars-forward " \t")
		      (cond ((looking-at "|")
			     (forward-char 1))
			    ((looking-at "\n")
			     (setq cont nil))
			    (t (error "Extra character at end of a command")))))))
	    ((string= ex-token-type "non-command")
	     (error "%s: Not an editor command" ex-token))
	    ((string= ex-token-type "whole")
	     (setq ex-addresses
		   (cons (point-max) (cons (point-min) ex-addresses))))
	    ((string= ex-token-type "comma")
	     (setq ex-addresses
		   (cons (if (null address) (point) address) ex-addresses)))
	    ((string= ex-token-type "semi-colon")
	     (if address (setq dot address))
	     (setq ex-addresses
		   (cons (if (null address) (point) address) ex-addresses)))
	    (t (let ((ans (vip-get-ex-address-subr address dot)))
		 (if ans (setq address ans))))))))

(defun vip-get-ex-pat ()
  "get a regular expression and set ex-variant if found"
  (with-current-buffer " *ex-working-space*"
    (skip-chars-forward " \t")
    (if (looking-at "!")
	(progn
	  (setq ex-g-variant (not ex-g-variant)
		ex-g-flag (not ex-g-flag))
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (if (looking-at "/")
	(progn
	  (forward-char 1)
	  (set-mark (point))
	  (let ((cont t))
	    (while (and (not (eolp)) cont)
	      (re-search-forward "[^/]*\\(/\\|\n\\)")
	      ;;(re-search-forward "[^/]*/")
	      (if (not (vip-looking-back "[^\\\\]\\(\\\\\\\\\\)*\\\\/"))
		  (setq cont nil))))
	  (setq ex-token
		(if (= (mark) (point)) ""
		  (buffer-substring (1- (point)) (mark))))
	  (backward-char 1))
      (setq ex-token nil))))

(defun vip-get-ex-command ()
  "get an ex command"
  (with-current-buffer " *ex-working-space*"
    (if (looking-at "/") (forward-char 1))
    (skip-chars-forward " \t")
    (cond ((looking-at "[a-z]")
	   (vip-get-ex-com-subr)
	   (if (string= ex-token-type "non-command")
	       (error "%s: not an editor command" ex-token)))
	  ((looking-at "[!=><&~]")
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  (t (error "Could not find an ex command")))))

(defun vip-get-ex-opt-gc ()
  "get an ex option g or c"
  (with-current-buffer " *ex-working-space*"
    (if (looking-at "/") (forward-char 1))
    (skip-chars-forward " \t")
    (cond ((looking-at "g")
	   (setq ex-token "g")
	   (forward-char 1)
	   t)
	  ((looking-at "c")
	   (setq ex-token "c")
	   (forward-char 1)
	   t)
	  (t nil))))

(defun vip-default-ex-addresses (&optional whole-flag)
  "compute default addresses.  whole-flag means whole buffer."
  (cond ((null ex-addresses)
	 (setq ex-addresses
	       (if whole-flag
		   (cons (point-max) (cons (point-min) nil))
		 (cons (point) (cons (point) nil)))))
	((null (cdr ex-addresses))
	 (setq ex-addresses
	       (cons (car ex-addresses) ex-addresses)))))

(defun vip-get-ex-address ()
  "get an ex-address as a marker and set ex-flag if a flag is found"
  (let ((address (point-marker)) (cont t))
    (setq ex-token "")
    (setq ex-flag nil)
    (while cont
      (vip-get-ex-token)
      (cond ((string= ex-token-type "command")
	     (if (or (string= ex-token "print") (string= ex-token "list")
		     (string= ex-token "#"))
		 (progn
		   (setq ex-flag t)
		   (setq cont nil))
	     (error "address expected")))
	    ((string= ex-token-type "end-mark")
	     (setq cont nil))
	    ((string= ex-token-type "whole")
	     (error "a trailing address is expected"))
	    ((string= ex-token-type "comma")
	     (error "Extra characters after an address"))
	    (t (let ((ans (vip-get-ex-address-subr address (point-marker))))
		 (if ans (setq address ans))))))
    address))

(defun vip-get-ex-address-subr (old-address dot)
  "returns an address as a point"
  (let ((address nil))
    (if (null old-address) (setq old-address dot))
    (cond ((string= ex-token-type "dot")
	   (setq address dot))
	  ((string= ex-token-type "add-number")
	   (save-excursion
	     (goto-char old-address)
	     (forward-line (if (= old-address 0) (1- ex-token) ex-token))
	     (setq address (point-marker))))
	  ((string= ex-token-type "sub-number")
	   (save-excursion
	     (goto-char old-address)
	     (forward-line (- ex-token))
	     (setq address (point-marker))))
	  ((string= ex-token-type "abs-number")
	   (save-excursion
	     (goto-char (point-min))
	     (if (= ex-token 0) (setq address 0)
	       (forward-line (1- ex-token))
	       (setq address (point-marker)))))
	  ((string= ex-token-type "end")
	   (setq address (point-max-marker)))
	  ((string= ex-token-type "plus") t);; do nothing
	  ((string= ex-token-type "minus") t);; do nothing
	  ((string= ex-token-type "search-forward")
	   (save-excursion
	     (ex-search-address t)
	     (setq address (point-marker))))
	  ((string= ex-token-type "search-backward")
	   (save-excursion
	     (ex-search-address nil)
	     (setq address (point-marker))))
	  ((string= ex-token-type "goto-mark")
	   (save-excursion
	     (if (null ex-token)
		 (exchange-point-and-mark)
	       (goto-char (register-to-point (- ex-token (- ?a ?\C-a)))))
	     (setq address (point-marker)))))
    address))

(defun ex-search-address (forward)
  "search pattern and set address"
  (if (string= ex-token "")
      (if (null vip-s-string) (error "No previous search string")
	(setq ex-token vip-s-string))
    (setq vip-s-string ex-token))
  (if forward
      (progn
	(forward-line 1)
	(re-search-forward ex-token))
    (forward-line -1)
    (re-search-backward ex-token)))

(defun vip-get-ex-buffer ()
  "get a buffer name and set ex-count and ex-flag if found"
  (setq ex-buffer nil)
  (setq ex-count nil)
  (setq ex-flag nil)
  (with-current-buffer " *ex-working-space*"
    (skip-chars-forward " \t")
    (if (looking-at "[a-zA-Z]")
	(progn
	  (setq ex-buffer (following-char))
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (if (looking-at "[0-9]")
	(progn
	  (set-mark (point))
	  (re-search-forward "[0-9][0-9]*")
	  (setq ex-count (string-to-number (buffer-substring (point) (mark))))
	  (skip-chars-forward " \t")))
    (if (looking-at "[pl#]")
	(progn
	  (setq ex-flag t)
	  (forward-char 1)))
    (if (not (looking-at "[\n|]"))
	(error "Invalid extra characters"))))

(defun vip-get-ex-count ()
  (setq ex-variant nil
	ex-count nil
	ex-flag nil)
  (with-current-buffer " *ex-working-space*"
    (skip-chars-forward " \t")
    (if (looking-at "!")
	(progn
	  (setq ex-variant t)
	  (forward-char 1)))
    (skip-chars-forward " \t")
    (if (looking-at "[0-9]")
	(progn
	  (set-mark (point))
	  (re-search-forward "[0-9][0-9]*")
	  (setq ex-count (string-to-number (buffer-substring (point) (mark))))
	  (skip-chars-forward " \t")))
    (if (looking-at "[pl#]")
	(progn
	  (setq ex-flag t)
	  (forward-char 1)))
    (if (not (looking-at "[\n|]"))
	(error "Invalid extra characters"))))

(defun vip-get-ex-file ()
  "get a file name and set ex-variant, ex-append and ex-offset if found"
  (setq ex-file nil
	ex-variant nil
	ex-append nil
	ex-offset nil)
  (with-current-buffer " *ex-working-space*"
    (skip-chars-forward " \t")
    (if (looking-at "!")
	(progn
	  (setq ex-variant t)
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (if (looking-at ">>")
	(progn
	  (setq ex-append t
		ex-variant t)
	  (forward-char 2)
	  (skip-chars-forward " \t")))
    (if (looking-at "+")
	(progn
	  (forward-char 1)
	  (set-mark (point))
	  (re-search-forward "[ \t\n]")
	  (backward-char 1)
	  (setq ex-offset (buffer-substring (point) (mark)))
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (set-mark (point))
    (re-search-forward "[ \t\n]")
    (backward-char 1)
    (setq ex-file (buffer-substring (point) (mark)))))

(defun vip-execute-ex-command ()
  "execute ex command using the value of addresses."
  (cond ((string= ex-token "goto") (ex-goto))
	((string= ex-token "copy") (ex-copy nil))
	((string= ex-token "delete") (ex-delete))
	((string= ex-token "edit") (ex-edit))
	((string= ex-token "file") (vip-info-on-file))
	;((string= ex-token "global") (ex-global nil))
	((string= ex-token "join") (ex-line "join"))
	((string= ex-token "k") (ex-mark))
	((string= ex-token "mark") (ex-mark))
	((string= ex-token "map") (ex-map))
	((string= ex-token "move") (ex-copy t))
	((string= ex-token "put") (ex-put))
	((string= ex-token "quit") (ex-quit))
	((string= ex-token "read") (ex-read))
	((string= ex-token "set") (ex-set))
	((string= ex-token "shell") (ex-shell))
	((string= ex-token "substitute") (ex-substitute))
	((string= ex-token "stop") (suspend-emacs))
	((string= ex-token "t") (ex-copy nil))
	((string= ex-token "tag") (ex-tag))
	((string= ex-token "undo") (vip-undo))
	((string= ex-token "unmap") (ex-unmap))
	;((string= ex-token "v") (ex-global t))
	((string= ex-token "version") (vip-version))
	((string= ex-token "visual") (ex-edit))
	((string= ex-token "write") (ex-write nil))
	((string= ex-token "wq") (ex-write t))
	((string= ex-token "yank") (ex-yank))
	((string= ex-token "!") (ex-command))
	((string= ex-token "=") (ex-line-no))
	((string= ex-token ">") (ex-line "right"))
	((string= ex-token "<") (ex-line "left"))
	((string= ex-token "&") (ex-substitute t))
	((string= ex-token "~") (ex-substitute t t))
	((or (string= ex-token "append")
	     (string= ex-token "args")
	     (string= ex-token "change")
	     (string= ex-token "insert")
	     (string= ex-token "open")
	     )
	 (error "%s: no such command from VIP" ex-token))
	((or (string= ex-token "abbreviate")
	     (string= ex-token "list")
	     (string= ex-token "next")
	     (string= ex-token "print")
	     (string= ex-token "preserve")
	     (string= ex-token "recover")
	     (string= ex-token "rewind")
	     (string= ex-token "source")
	     (string= ex-token "unabbreviate")
	     (string= ex-token "xit")
	     (string= ex-token "z")
	     )
	 (error "%s: not implemented in VIP" ex-token))
	(t (error "%s: Not an editor command" ex-token))))

(defun ex-goto ()
  "ex goto command"
  (if (null ex-addresses)
      (setq ex-addresses (cons (point) nil)))
  (push-mark (point))
  (goto-char (car ex-addresses))
  (beginning-of-line))

(defun ex-copy (del-flag)
  "ex copy and move command.  DEL-FLAG means delete."
  (vip-default-ex-addresses)
  (let ((address (vip-get-ex-address))
	(end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (goto-char end)
    (save-excursion
      (set-mark beg)
      (vip-enlarge-region (mark) (point))
      (if del-flag (kill-region (point) (mark))
	(copy-region-as-kill (point) (mark)))
      (if ex-flag
	  (progn
	    (with-output-to-temp-buffer "*copy text*"
	      (princ
	       (if (or del-flag ex-g-flag ex-g-variant)
		   (current-kill 0)
		 (buffer-substring (point) (mark)))))
	    (condition-case nil
		(progn
		  (vip-read-string "[Hit return to continue] ")
		  (save-excursion (kill-buffer "*copy text*")))
	      (quit
	       (save-excursion (kill-buffer "*copy text*"))
	       (signal 'quit nil))))))
      (if (= address 0)
	  (goto-char (point-min))
	(goto-char address)
	(forward-line 1))
      (insert (current-kill 0))))

(defun ex-delete ()
  "ex delete"
  (vip-default-ex-addresses)
  (vip-get-ex-buffer)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (if ex-count
	  (progn
	    (set-mark (point))
	    (forward-line (1- ex-count)))
	(set-mark end))
      (vip-enlarge-region (point) (mark))
      (if ex-flag
	  ;; show text to be deleted and ask for confirmation
	  (progn
	    (with-output-to-temp-buffer " *delete text*"
	      (princ (buffer-substring (point) (mark))))
	    (condition-case conditions
		(vip-read-string "[Hit return to continue] ")
	      (quit
	       (save-excursion (kill-buffer " *delete text*"))
	       (error "")))
	    (save-excursion (kill-buffer " *delete text*")))
	(if ex-buffer
	    (if (and (<= ?A ex-buffer) (<= ex-buffer ?Z))
		(vip-append-to-register
		 (+ ex-buffer 32) (point) (mark))
	      (copy-to-register ex-buffer (point) (mark) nil)))
	(delete-region (point) (mark))))))

(defun ex-edit ()
  "ex-edit"
  (vip-get-ex-file)
  (if (and (not ex-variant) (buffer-modified-p) buffer-file-name)
      (error "No write since last change \(:e! overrides\)"))
  (vip-change-mode-to-emacs)
  (set-buffer
   (find-file-noselect (concat default-directory ex-file)))
  (vip-change-mode-to-vi)
  (goto-char (point-min))
  (if ex-offset
      (progn
	(with-current-buffer " *ex-working-space*"
	  (delete-region (point-min) (point-max))
	  (insert ex-offset "\n")
	  (goto-char (point-min)))
	(goto-char (vip-get-ex-address))
	(beginning-of-line))))

(defun ex-global (variant)
  "ex global command"
  (if (or ex-g-flag ex-g-variant)
      (error "Global within global not allowed")
    (if variant
	(setq ex-g-flag nil
	      ex-g-variant t)
      (setq ex-g-flag t
	    ex-g-variant nil)))
  (vip-get-ex-pat)
  (if (null ex-token)
      (error "Missing regular expression for global command"))
  (if (string= ex-token "")
      (if (null vip-s-string) (error "No previous search string")
	(setq ex-g-pat vip-s-string))
    (setq ex-g-pat ex-token
	  vip-s-string ex-token))
  (if (null ex-addresses)
      (setq ex-addresses (list (point-max) (point-min))))
  (let ((marks nil) (mark-count 0)
	com-str (end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (let ((cont t) (limit (point-marker)))
	(exchange-point-and-mark)
	;; skip the last line if empty
	(beginning-of-line)
	(if (and (eobp) (not (bobp))) (backward-char 1))
	(while (and cont (not (bobp)) (>= (point) limit))
	  (beginning-of-line)
	  (set-mark (point))
	  (end-of-line)
	  (let ((found (re-search-backward ex-g-pat (mark) t)))
	    (if (or (and ex-g-flag found)
		    (and ex-g-variant (not found)))
		(progn
		  (end-of-line)
		  (setq mark-count (1+ mark-count))
		  (setq marks (cons (point-marker) marks)))))
	  (beginning-of-line)
	  (if (bobp) (setq cont nil)
	    (forward-line -1)
	    (end-of-line)))))
    (with-current-buffer " *ex-working-space*"
      (setq com-str (buffer-substring (1+ (point)) (1- (point-max)))))
    (while marks
      (goto-char (car marks))
      ;; report progress of execution on a slow machine.
      ;;(message "Executing global command...")
      ;;(if (zerop (% mark-count 10))
      ;;    (message "Executing global command...%d" mark-count))
      (vip-ex com-str)
      (setq mark-count (1- mark-count))
      (setq marks (cdr marks)))))
;;(message "Executing global command...done")))

(defun ex-line (com)
  "ex line commands.  COM is join, shift-right or shift-left."
  (vip-default-ex-addresses)
  (vip-get-ex-count)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))) point)
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (if ex-count
	  (progn
	    (set-mark (point))
	    (forward-line ex-count)))
      (if ex-flag
	  ;; show text to be joined and ask for confirmation
	  (progn
	    (with-output-to-temp-buffer " *text*"
	      (princ (buffer-substring (point) (mark))))
	    (condition-case conditions
		(progn
		  (vip-read-string "[Hit return to continue] ")
		  (ex-line-subr com (point) (mark)))
	      (quit
	       (ding)))
	    (save-excursion (kill-buffer " *text*")))
	(ex-line-subr com (point) (mark)))
      (setq point (point)))
    (goto-char (1- point))
    (beginning-of-line)))

(defun ex-line-subr (com beg end)
  (cond ((string= com "join")
	 (goto-char (min beg end))
	 (while (and (not (eobp)) (< (point) (max beg end)))
	   (end-of-line)
	   (if (and (<= (point) (max beg end)) (not (eobp)))
	       (progn
		 (forward-line 1)
		 (delete-region (point) (1- (point)))
		 (if (not ex-variant) (fixup-whitespace))))))
	((or (string= com "right") (string= com "left"))
	 (indent-rigidly
	  (min beg end) (max beg end)
	  (if (string= com "right") vip-shift-width (- vip-shift-width)))
	 (goto-char (max beg end))
	 (end-of-line)
	 (forward-char 1))))

(defun ex-mark ()
  "ex mark"
  (let (char)
    (if (null ex-addresses)
	(setq ex-addresses
	      (cons (point) nil)))
    (with-current-buffer " *ex-working-space*"
      (skip-chars-forward " \t")
      (if (looking-at "[a-z]")
	  (progn
	    (setq char (following-char))
	    (forward-char 1)
	    (skip-chars-forward " \t")
	    (if (not (looking-at "[\n|]"))
		(error "Extra characters at end of \"k\" command")))
	(if (looking-at "[\n|]")
	    (error "\"k\" requires a following letter")
	  (error "Mark must specify a letter"))))
    (save-excursion
      (goto-char (car ex-addresses))
      (point-to-register (- char (- ?a ?\C-a)) nil))))

(defun ex-map ()
  "ex map"
  (let (char string)
    (with-current-buffer " *ex-working-space*"
      (skip-chars-forward " \t")
      (setq char (char-to-string (following-char)))
      (forward-char 1)
      (skip-chars-forward " \t")
      (if (looking-at "[\n|]") (error "Missing rhs"))
      (set-mark (point))
      (with-no-warnings
	(end-of-buffer))
      (backward-char 1)
      (setq string (buffer-substring (mark) (point))))
    (if (not (lookup-key ex-map char))
	(define-key ex-map char
	  (or (lookup-key vip-mode-map char) 'vip-nil)))
    (define-key vip-mode-map char
      (eval
       (list 'quote
	     (cons 'lambda
		   (list '(count)
			 '(interactive "p")
			 (list 'execute-kbd-macro string 'count))))))))

(defun ex-unmap ()
  "ex unmap"
  (let (char)
    (with-current-buffer " *ex-working-space*"
      (skip-chars-forward " \t")
      (setq char (char-to-string (following-char)))
      (forward-char 1)
      (skip-chars-forward " \t")
      (if (not (looking-at "[\n|]")) (error "Macro must be a character")))
    (if (not (lookup-key ex-map char))
	(error "That macro wasn't mapped"))
    (define-key vip-mode-map char (lookup-key ex-map char))
    (define-key ex-map char nil)))

(defun ex-put ()
  "ex put"
  (let ((point (if (null ex-addresses) (point) (car ex-addresses))))
    (vip-get-ex-buffer)
    (setq vip-use-register ex-buffer)
    (goto-char point)
    (if (= point 0) (vip-Put-back 1) (vip-put-back 1))))

(defun ex-quit ()
  "ex quit"
  (let (char)
    (with-current-buffer " *ex-working-space*"
      (skip-chars-forward " \t")
      (setq char (following-char)))
    (if (= char ?!) (kill-emacs t) (save-buffers-kill-emacs))))

(defun ex-read ()
  "ex read"
  (let ((point (if (null ex-addresses) (point) (car ex-addresses)))
	(variant nil) command file)
    (goto-char point)
    (if (not (= point 0)) (with-no-warnings (next-line 1)))
    (beginning-of-line)
    (with-current-buffer " *ex-working-space*"
      (skip-chars-forward " \t")
      (if (looking-at "!")
	  (progn
	    (setq variant t)
	    (forward-char 1)
	    (skip-chars-forward " \t")
	    (set-mark (point))
	    (end-of-line)
	    (setq command (buffer-substring (mark) (point))))
	(set-mark (point))
	(re-search-forward "[ \t\n]")
	(backward-char 1)
	(setq file (buffer-substring (point) (mark)))))
      (if variant
	  (shell-command command t)
	(with-no-warnings
	  (insert-file file)))))

(defun ex-set ()
  (eval (list 'setq
	      (read-variable "Variable: ")
	      (eval (read-minibuffer "Value: ")))))

(defun ex-shell ()
  "ex shell"
  (vip-change-mode-to-emacs)
  (shell))

(defun ex-substitute (&optional repeat r-flag)
  "ex substitute.
If REPEAT use previous reg-exp which is ex-reg-exp or
vip-s-string"
  (let (pat repl (opt-g nil) (opt-c nil) (matched-pos nil))
    (if repeat (setq ex-token nil) (vip-get-ex-pat))
    (if (null ex-token)
	(setq pat (if r-flag vip-s-string ex-reg-exp)
	      repl ex-repl)
      (setq pat (if (string= ex-token "") vip-s-string ex-token))
      (setq vip-s-string pat
	    ex-reg-exp pat)
      (vip-get-ex-pat)
      (if (null ex-token)
	  (setq ex-token ""
		ex-repl "")
	(setq repl ex-token
	      ex-repl ex-token)))
    (while (vip-get-ex-opt-gc)
      (if (string= ex-token "g") (setq opt-g t) (setq opt-c t)))
    (vip-get-ex-count)
    (if ex-count
	(save-excursion
	  (if ex-addresses (goto-char (car ex-addresses)))
	  (set-mark (point))
	  (forward-line (1- ex-count))
	  (setq ex-addresses (cons (point) (cons (mark) nil))))
      (if (null ex-addresses)
	  (setq ex-addresses (cons (point) (cons (point) nil)))
	(if (null (cdr ex-addresses))
	    (setq ex-addresses (cons (car ex-addresses) ex-addresses)))))
    ;(setq G opt-g)
    (let ((beg (car ex-addresses)) (end (car (cdr ex-addresses)))
	  (cont t) eol-mark)
      (save-excursion
	(vip-enlarge-region beg end)
	(let ((limit (save-excursion
		       (goto-char (max (point) (mark)))
		       (point-marker))))
	  (goto-char (min (point) (mark)))
	  (while (< (point) limit)
	    (end-of-line)
	    (setq eol-mark (point-marker))
	    (beginning-of-line)
	    (if opt-g
		(progn
		  (while (and (not (eolp))
			      (re-search-forward pat eol-mark t))
		    (if (or (not opt-c) (y-or-n-p "Replace? "))
			(progn
			  (setq matched-pos (point))
			  (replace-match repl))))
		  (end-of-line)
		  (forward-char))
	      (if (and (re-search-forward pat eol-mark t)
		       (or (not opt-c) (y-or-n-p "Replace? ")))
		  (progn
		    (setq matched-pos (point))
		    (replace-match repl)))
	      (end-of-line)
	      (forward-char))))))
    (if matched-pos (goto-char matched-pos))
    (beginning-of-line)
    (if opt-c (message "done"))))

(defun ex-tag ()
  "ex tag"
  (let (tag)
    (with-current-buffer " *ex-working-space*"
      (skip-chars-forward " \t")
      (set-mark (point))
      (skip-chars-forward "^ |\t\n")
      (setq tag (buffer-substring (mark) (point))))
    (if (not (string= tag "")) (setq ex-tag tag))
    (vip-change-mode-to-emacs)
    (condition-case conditions
	(progn
	  (if (string= tag "")
	      (find-tag ex-tag t)
	    (find-tag-other-window ex-tag))
	  (vip-change-mode-to-vi))
      (error
       (vip-change-mode-to-vi)
       (vip-message-conditions conditions)))))

(defun ex-write (q-flag)
  "ex write"
  (vip-default-ex-addresses t)
  (vip-get-ex-file)
  (if (string= ex-file "")
      (progn
	(if (null buffer-file-name)
	    (error "No file associated with this buffer"))
	(setq ex-file buffer-file-name))
    (setq ex-file (expand-file-name ex-file)))
  (if (and (not (string= ex-file (buffer-file-name)))
	   (file-exists-p ex-file)
	   (not ex-variant))
      (error "\"%s\" File exists - use w! to override" ex-file))
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (vip-enlarge-region beg end)
      (write-region (point) (mark) ex-file ex-append t)))
  (if (null buffer-file-name) (setq buffer-file-name ex-file))
  (if q-flag (save-buffers-kill-emacs)))

(defun ex-yank ()
  "ex yank"
  (vip-default-ex-addresses)
  (vip-get-ex-buffer)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (if (or ex-g-flag ex-g-variant) (error "Can't yank within global"))
      (if ex-count
	  (progn
	    (set-mark (point))
	    (forward-line (1- ex-count)))
	(set-mark end))
      (vip-enlarge-region (point) (mark))
      (if ex-flag (error "Extra characters at end of command"))
      (if ex-buffer
	  (copy-to-register ex-buffer (point) (mark) nil))
      (copy-region-as-kill (point) (mark)))))

(defun ex-command ()
  "execute shell command"
  (let (command)
    (with-current-buffer " *ex-working-space*"
      (skip-chars-forward " \t")
      (set-mark (point))
      (end-of-line)
      (setq command (buffer-substring (mark) (point))))
    (if (null ex-addresses)
	(shell-command command)
      (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
	(if (null beg) (setq beg end))
	(save-excursion
	  (goto-char beg)
	  (set-mark end)
	  (vip-enlarge-region (point) (mark))
	  (shell-command-on-region (point) (mark) command t))
	(goto-char beg)))))

(defun ex-line-no ()
  "print line number"
  (message "%d"
	   (1+ (count-lines
		(point-min)
		(if (null ex-addresses) (point-max) (car ex-addresses))))))

(if (file-exists-p vip-startup-file) (load vip-startup-file))

(provide 'vip)

;;; vip.el ends here
