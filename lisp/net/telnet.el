;;; telnet.el --- run a telnet session from within an Emacs buffer

;; Copyright (C) 1985, 1988, 1992, 1994, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: William F. Schelter
;; Maintainer: FSF
;; Keywords: unix, comm

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

;; This mode is intended to be used for telnet or rsh to a remote host;
;; `telnet' and `rsh' are the two entry points.  Multiple telnet or rsh
;; sessions are supported.
;;
;; Normally, input is sent to the remote telnet/rsh line-by-line, as you
;; type RET or LFD.  C-c C-c sends a C-c to the remote immediately;
;; C-c C-z sends C-z immediately.  C-c C-q followed by any character
;; sends that character immediately.
;;
;; All RET characters are filtered out of the output coming back from the
;; remote system.  The mode tries to do other useful translations based
;; on what it sees coming back from the other system before the password
;; query.  It knows about UNIX, ITS, TOPS-20 and Explorer systems.
;;
;; You can use the global telnet-host-properties to associate a telnet
;; program and login name with each host you regularly telnet to.

;;; Code:

;; to do fix software types for lispm:
;; to eval current expression.  Also to try to send escape keys correctly.
;; essentially we'll want the rubout-handler off.

;; filter is simplistic but should be okay for typical shell usage.
;; needs hacking if it is going to deal with asynchronous output in a sane
;; manner

(require 'comint)

(defvar telnet-host-properties ()
  "Specify which telnet program to use for particular hosts.
Each element has the form (HOSTNAME PROGRAM [LOGIN-NAME])
HOSTNAME says which machine the element applies to.
PROGRAM says which program to run, to talk to that machine.
LOGIN-NAME, which is optional, says what to log in as on that machine.")

(defvar telnet-new-line "\r")
(defvar telnet-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\C-m" 'telnet-send-input)
    ;; (define-key map "\C-j" 'telnet-send-input)
    (define-key map "\C-c\C-q" 'send-process-next-char)
    (define-key map "\C-c\C-c" 'telnet-interrupt-subjob)
    (define-key map "\C-c\C-z" 'telnet-c-z)
    map))

(defvar telnet-prompt-pattern "^[^#$%>\n]*[#$%>] *")
(defvar telnet-replace-c-g nil)
(make-variable-buffer-local
 (defvar telnet-remote-echoes t
   "True if the telnet process will echo input."))
(make-variable-buffer-local
 (defvar telnet-interrupt-string "\C-c" "String sent by C-c."))

(defvar telnet-count 0
  "Number of output strings from telnet process while looking for password.")
(make-variable-buffer-local 'telnet-count)

(defvar telnet-program "telnet"
  "Program to run to open a telnet connection.")

(defvar telnet-initial-count -50
  "Initial value of `telnet-count'.  Should be set to the negative of the
number of terminal writes telnet will make setting up the host connection.")

(defvar telnet-maximum-count 4
  "Maximum value `telnet-count' can have.
After this many passes, we stop looking for initial setup data.
Should be set to the number of terminal writes telnet will make
rejecting one login and prompting again for a username and password.")

(defun telnet-interrupt-subjob ()
  "Interrupt the program running through telnet on the remote host."
  (interactive)
  (process-send-string nil telnet-interrupt-string))

(defun telnet-c-z ()
  (interactive)
  (process-send-string nil "\C-z"))

(defun send-process-next-char ()
  (interactive)
  (process-send-string nil
                       (char-to-string
                        (let ((inhibit-quit t))
                          (prog1 (read-char)
                            (setq quit-flag nil))))))

;;maybe should have a flag for when have found type
(defun telnet-check-software-type-initialize (string)
  "Tries to put correct initializations in.  Needs work."
  (let ((case-fold-search t))
    (cond ((string-match "unix" string)
	 (setq telnet-prompt-pattern comint-prompt-regexp)
	 (setq telnet-new-line "\n"))
	((string-match "tops-20" string) ;;maybe add telnet-replace-c-g
	 (setq telnet-prompt-pattern  "[@>]*"))
	((string-match "its" string)
	 (setq telnet-prompt-pattern  "^[^*>\n]*[*>] *"))
	((string-match "explorer" string)  ;;explorer telnet needs work
	 (setq telnet-replace-c-g ?\n))))
  (setq comint-prompt-regexp telnet-prompt-pattern))

(defun telnet-initial-filter (proc string)
  ;For reading up to and including password; also will get machine type.
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((case-fold-search t))
      (cond ((string-match "No such host" string)
	     (kill-buffer (process-buffer proc))
	     (error "No such host"))
	    ((string-match "passw" string)
	     (telnet-filter proc string)
	     (setq telnet-count 0)
	     (process-send-string proc (concat (comint-read-noecho "Password: " t)
                                               telnet-new-line))
	     (clear-this-command-keys))
	    (t (telnet-check-software-type-initialize string)
	       (telnet-filter proc string)
	       (cond ((> telnet-count telnet-maximum-count)
		      (set-process-filter proc 'telnet-filter))
		     (t (setq telnet-count (1+ telnet-count)))))))))

;; Identical to comint-simple-send, except that it sends telnet-new-line
;; instead of "\n".
(defun telnet-simple-send (proc string)
  (comint-send-string proc string)
  (if comint-input-sender-no-newline
      (if (not (string-equal string ""))
	  (process-send-eof))
    (comint-send-string proc telnet-new-line)))

(defun telnet-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let* ((last-insertion (marker-position (process-mark proc)))
	   (delta (- (point) last-insertion))
	   (ie (and comint-last-input-end
		    (marker-position comint-last-input-end)))
	   (w (get-buffer-window (current-buffer)))
	   (ws (and w (window-start w))))
      (goto-char last-insertion)
      (insert string)
      (set-marker comint-last-output-start last-insertion)
      (set-marker (process-mark proc) (point))
      (if ws (set-window-start w ws t))
      (if ie (set-marker comint-last-input-end ie))
      (while (progn (skip-chars-backward "^\C-m" last-insertion)
		    (> (point) last-insertion))
	(delete-region (1- (point)) (point)))
      (goto-char (process-mark proc))
      (and telnet-replace-c-g
	   (subst-char-in-region last-insertion (point) ?\C-g
				 telnet-replace-c-g t))
      ;; If point is after the insertion place, move it
      ;; along with the text.
      (if (> delta 0)
	  (goto-char (+ (process-mark proc) delta))))))

(defun telnet-send-input ()
  (interactive)
;  (comint-send-input telnet-new-line telnet-remote-echoes)
  (comint-send-input)
  (if telnet-remote-echoes
      (delete-region comint-last-input-start
		     comint-last-input-end)))

;;;###autoload
(defun telnet (host &optional port)
  "Open a network login connection to host named HOST (a string).
Optional arg PORT specifies alternative port to connect to.
Interactively, use \\[universal-argument] prefix to be prompted for port number.

Communication with HOST is recorded in a buffer `*PROGRAM-HOST*'
where PROGRAM is the telnet program being used.  This program
is controlled by the contents of the global variable `telnet-host-properties',
falling back on the value of the global variable `telnet-program'.
Normally input is edited in Emacs and sent a line at a time."
  (interactive (list (read-string "Open connection to host: ")
		     (cond
		      ((null current-prefix-arg) nil)
		      ((consp current-prefix-arg) (read-string "Port: "))
		      (t (prefix-numeric-value current-prefix-arg)))))
  (if (and port (numberp port))
      (setq port (int-to-string port)))
  (let* ((comint-delimiter-argument-list '(?\  ?\t))
	 (properties (cdr (assoc host telnet-host-properties)))
	 (telnet-program (if properties (car properties) telnet-program))
	 (hname (if port (concat host ":" port) host))
         (name (concat telnet-program "-" (comint-arguments hname 0 nil) ))
	 (buffer (get-buffer (concat "*" name "*")))
	 (telnet-options (if (cdr properties) (cons "-l" (cdr properties))))
	 process)
    (if (and buffer (get-buffer-process buffer))
	(switch-to-buffer (concat "*" name "*"))
      (switch-to-buffer
       (apply 'make-comint name telnet-program nil telnet-options))
      (setq process (get-buffer-process (current-buffer)))
      (set-process-filter process 'telnet-initial-filter)
      ;; Don't send the `open' cmd till telnet is ready for it.
      (accept-process-output process)
      (erase-buffer)
      (process-send-string process (concat "open " host
                                           (if port " " "") (or port "")
                                           "\n"))
      (telnet-mode)
      (setq comint-input-sender 'telnet-simple-send)
      (setq telnet-count telnet-initial-count))))

(put 'telnet-mode 'mode-class 'special)

(define-derived-mode telnet-mode comint-mode "Telnet"
  "This mode is for using telnet (or rsh) from a buffer to another host.
It has most of the same commands as comint-mode.
There is a variable ``telnet-interrupt-string'' which is the character
sent to try to stop execution of a job on the remote host.
Data is sent to the remote host when RET is typed."
  (set (make-local-variable 'window-point-insertion-type) t)
  (set (make-local-variable 'comint-prompt-regexp) telnet-prompt-pattern)
  (set (make-local-variable 'comint-use-prompt-regexp) t))

;;;###autoload
(defun rsh (host)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*rsh-HOST*'.
Normally input is edited in Emacs and sent a line at a time."
  (interactive "sOpen rsh connection to host: ")
  (require 'shell)
  (let ((name (concat "rsh-" host )))
    (switch-to-buffer (make-comint name remote-shell-program nil host))
    (set-process-filter (get-process name) 'telnet-initial-filter)
    (telnet-mode)
    (setq telnet-count -16)))

(provide 'telnet)

;;; telnet.el ends here
