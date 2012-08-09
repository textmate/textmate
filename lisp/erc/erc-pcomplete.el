;;; erc-pcomplete.el --- Provides programmable completion for ERC

;; Copyright (C) 2002-2004, 2006-2012 Free Software Foundation, Inc.

;; Author: Sacha Chua <sacha@free.net.ph>
;; Keywords: comm, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcCompletion

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

;; This file replaces erc-complete.el.  It provides nick completion
;; for ERC based on pcomplete.  If you do not have pcomplete, you may
;; try to use erc-complete.el.
;;
;; To use, (require 'erc-auto) or (require 'erc-pcomplete), then
;;   (erc-pcomplete-mode 1)
;;
;; If you want nickname completions ordered such that the most recent
;; speakers are listed first, set
;; `erc-pcomplete-order-nickname-completions' to `t'.
;;
;; See CREDITS for other contributors.
;;
;;; Code:

(require 'pcomplete)
(require 'erc)
(require 'erc-compat)
(require 'time-date)
(eval-when-compile (require 'cl))

(defgroup erc-pcomplete nil
  "Programmable completion for ERC"
  :group 'erc)

(defcustom erc-pcomplete-nick-postfix ":"
  "*When `pcomplete' is used in the first word after the prompt,
add this string to nicks completed."
  :group 'erc-pcomplete
  :type 'string)

(defcustom erc-pcomplete-order-nickname-completions t
  "If t, channel nickname completions will be ordered such that
the most recent speakers are listed first."
  :group 'erc-pcomplete
  :type 'boolean)

;;;###autoload (autoload 'erc-completion-mode "erc-pcomplete" nil t)
(define-erc-module pcomplete Completion
  "In ERC Completion mode, the TAB key does completion whenever possible."
  ((add-hook 'erc-mode-hook 'pcomplete-erc-setup)
   (add-hook 'erc-complete-functions 'erc-pcompletions-at-point)
   (erc-buffer-list #'pcomplete-erc-setup))
  ((remove-hook 'erc-mode-hook 'pcomplete-erc-setup)
   (remove-hook 'erc-complete-functions 'erc-pcompletions-at-point)))

(defun erc-pcompletions-at-point ()
  "ERC completion data from pcomplete.
for use on `completion-at-point-function'."
  (when (> (point) (erc-beg-of-input-line))
    (or (let ((pcomplete-default-completion-function #'ignore))
          (pcomplete-completions-at-point))
        (let ((c (pcomplete-completions-at-point)))
          (if c (nconc c '(:exclusive no)))))))

(defun erc-pcomplete ()
  "Complete the nick before point."
  (interactive)
  (when (> (point) (erc-beg-of-input-line))
    (let ((last-command (if (eq last-command 'erc-complete-word)
                            'pcomplete
                          last-command)))
      (call-interactively 'pcomplete))
    t))

;;; Setup function

(defun pcomplete-erc-setup ()
  "Setup `erc-mode' to use pcomplete."
  (set (make-local-variable 'pcomplete-ignore-case)
       t)
  (set (make-local-variable 'pcomplete-use-paring)
       nil)
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'pcomplete-erc-parse-arguments)
  (set (make-local-variable 'pcomplete-command-completion-function)
       'pcomplete/erc-mode/complete-command)
  (set (make-local-variable 'pcomplete-command-name-function)
       'pcomplete-erc-command-name)
  (set (make-local-variable 'pcomplete-default-completion-function)
       (lambda () (pcomplete-here (pcomplete-erc-nicks)))))

;;; Programmable completion logic

(defun pcomplete/erc-mode/complete-command ()
  (pcomplete-here
   (append
    (pcomplete-erc-commands)
    (pcomplete-erc-nicks erc-pcomplete-nick-postfix t))))

(defvar erc-pcomplete-ctcp-commands
  '("ACTION" "CLIENTINFO" "ECHO" "FINGER" "PING" "TIME" "USERINFO" "VERSION"))

(defun pcomplete/erc-mode/CTCP ()
  (pcomplete-here (pcomplete-erc-nicks))
  (pcomplete-here erc-pcomplete-ctcp-commands))

(defun pcomplete/erc-mode/CLEARTOPIC ()
  (pcomplete-here (pcomplete-erc-channels)))

(defun pcomplete/erc-mode/DEOP ()
  (while (pcomplete-here (pcomplete-erc-ops))))

(defun pcomplete/erc-mode/DESCRIBE ()
  (pcomplete-here (pcomplete-erc-nicks)))

(defun pcomplete/erc-mode/IDLE ()
  (while (pcomplete-here (pcomplete-erc-nicks))))

(defun pcomplete/erc-mode/KICK ()
  (pcomplete-here (pcomplete-erc-channels))
  (pcomplete-here (pcomplete-erc-nicks)))

(defun pcomplete/erc-mode/LOAD ()
  (pcomplete-here (pcomplete-entries)))

(defun pcomplete/erc-mode/MODE ()
  (pcomplete-here (pcomplete-erc-channels))
  (while (pcomplete-here (pcomplete-erc-nicks))))

(defun pcomplete/erc-mode/ME ()
  (while (pcomplete-here (pcomplete-erc-nicks))))

(defun pcomplete/erc-mode/SAY ()
  (pcomplete-here (pcomplete-erc-nicks))
  (pcomplete-here (pcomplete-erc-nicks))
  (while (pcomplete-here (pcomplete-erc-nicks))))

(defun pcomplete/erc-mode/MSG ()
  (pcomplete-here (append (pcomplete-erc-all-nicks)
                          (pcomplete-erc-channels)))
  (while (pcomplete-here (pcomplete-erc-nicks))))

(defun pcomplete/erc-mode/NAMES ()
  (while (pcomplete-here (pcomplete-erc-channels))))

(defalias 'pcomplete/erc-mode/NOTICE 'pcomplete/erc-mode/MSG)

(defun pcomplete/erc-mode/OP ()
  (while (pcomplete-here (pcomplete-erc-not-ops))))

(defun pcomplete/erc-mode/PART ()
  (pcomplete-here (pcomplete-erc-channels)))

(defalias 'pcomplete/erc-mode/LEAVE 'pcomplete/erc-mode/PART)

(defun pcomplete/erc-mode/QUERY ()
  (pcomplete-here (append (pcomplete-erc-all-nicks)
                          (pcomplete-erc-channels)))
  (while (pcomplete-here (pcomplete-erc-nicks)))
  )

(defun pcomplete/erc-mode/SOUND ()
  (while (pcomplete-here (pcomplete-entries))))

(defun pcomplete/erc-mode/TOPIC ()
  (pcomplete-here (pcomplete-erc-channels)))

(defun pcomplete/erc-mode/WHOIS ()
  (while (pcomplete-here (pcomplete-erc-nicks))))

(defun pcomplete/erc-mode/UNIGNORE ()
  (pcomplete-here (erc-with-server-buffer erc-ignore-list)))

;;; Functions that provide possible completions.

(defun pcomplete-erc-commands ()
  "Returns a list of strings of the defined user commands."
  (let ((case-fold-search nil))
    (mapcar (lambda (x)
              (concat "/" (downcase (substring (symbol-name x) 8))))
            (apropos-internal "erc-cmd-[A-Z]+"))))

(defun pcomplete-erc-ops ()
  "Returns a list of nicks with ops."
  (let (ops)
    (maphash (lambda (nick cdata)
               (if (and (cdr cdata)
                        (erc-channel-user-op (cdr cdata)))
                   (setq ops (cons nick ops))))
             erc-channel-users)
    ops))

(defun pcomplete-erc-not-ops ()
  "Returns a list of nicks without ops."
  (let (not-ops)
    (maphash (lambda (nick cdata)
               (if (and (cdr cdata)
                        (not (erc-channel-user-op (cdr cdata))))
                   (setq not-ops (cons nick not-ops))))
             erc-channel-users)
    not-ops))


(defun pcomplete-erc-nicks (&optional postfix ignore-self)
  "Returns a list of nicks in the current channel.
Optional argument POSTFIX is something to append to the nickname.
If optional argument IGNORE-SELF is non-nil, don't return the current nick."
  (let ((users (if erc-pcomplete-order-nickname-completions
                   (erc-sort-channel-users-by-activity
                    (erc-get-channel-user-list))
                 (erc-get-channel-user-list)))
        (nicks nil))
    (dolist (user users)
      (unless (and ignore-self
                   (string= (erc-server-user-nickname (car user))
                            (erc-current-nick)))
        (setq nicks (cons (concat (erc-server-user-nickname (car user))
                                  postfix)
                          nicks))))
    (nreverse nicks)))

(defun pcomplete-erc-all-nicks (&optional postfix)
  "Returns a list of all nicks on the current server."
  (let (nicks)
    (erc-with-server-buffer
      (maphash (lambda (nick user)
                 (setq nicks (cons (concat nick postfix) nicks)))
               erc-server-users))
      nicks))

(defun pcomplete-erc-channels ()
  "Returns a list of channels associated with the current server."
  (mapcar (lambda (buf) (with-current-buffer buf (erc-default-target)))
          (erc-channel-list erc-server-process)))

;;; Functions for parsing

(defun pcomplete-erc-command-name ()
  "Returns the command name of the first argument."
  (if (eq (elt (pcomplete-arg 'first) 0) ?/)
      (upcase (substring (pcomplete-arg 'first) 1))
    "SAY"))

(defun pcomplete-erc-parse-arguments ()
  "Returns a list of parsed whitespace-separated arguments.
These are the words from the beginning of the line after the prompt
up to where point is right now."
  (let* ((start erc-input-marker)
         (end (point))
         args beginnings)
    (save-excursion
      (if (< (skip-chars-backward " \t\n" start) 0)
          (setq args '("")
                beginnings (list end)))
      (setq end (point))
      (while (< (skip-chars-backward "^ \t\n" start) 0)
        (setq beginnings (cons (point) beginnings)
              args (cons (buffer-substring-no-properties
                          (point) end)
                         args))
        (skip-chars-backward " \t\n" start)
        (setq end (point))))
    (cons args beginnings)))

(provide 'erc-pcomplete)

;;; erc-pcomplete.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

