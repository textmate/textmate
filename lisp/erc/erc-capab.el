;;; erc-capab.el --- support for dancer-ircd and hyperion's CAPAB

;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

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

;; This file defines the ERC module `erc-capab-identify', which allows
;; flagging of unidentified users on servers running dancer-ircd or
;; hyperion.  freenode.net supports this capability, for example.

;; With CAPAB IDENTIFY-MSG and IDENTIFY-CTCP enabled, messages from
;; users who have identified themselves to NickServ will have a plus
;; sign and messages from unidentified users will have a minus sign
;; added as a prefix.  Note that it is not necessary for your nickname
;; to be identified in order to receive these marked messages.

;; The plus or minus sign is removed from the message, and a prefix,
;; `erc-capab-identify-prefix', is inserted in the front of the user's
;; nickname if the nickname is not identified.

;; Please note that once this has been enabled on a server, there is no
;; way to tell the server to stop sending marked messages.  If you
;; disable this module, it will continue removing message flags, but the
;; unidentified nickname prefix will not be added to messages.

;; Visit <http://freenode.net/faq.shtml#spoofing> and
;; <http://freenode.net/faq.shtml#registering> to find further
;; explanations of this capability.

;; From freenode.net's web site (not there anymore) on how to mark
;; unidentified users:
;; "We recommend that you add an asterisk before the nick, and
;;  optionally either highlight or colorize the line in some
;;  appropriate fashion, if the user is not identified."

;;; Usage:

;; Put the following in your ~/.emacs file.

;; (require 'erc-capab)
;; (erc-capab-identify-mode 1)

;; `erc-capab-identify-prefix' will now be added to the beginning of
;; unidentified users' nicknames.  The default is an asterisk, "*".
;; You can customize the prefix and the face used to display it,
;; `erc-capab-identify-unidentified'.  If the value of
;; `erc-capab-identify-prefix' is nil or you disable this module (see
;; `erc-capab-identify-disable'), no prefix will be inserted, but the
;; flag sent by the server will still be stripped.

;;; Code:

(require 'erc)
(eval-when-compile (require 'cl))

;;; Customization:

(defgroup erc-capab nil
  "Support for dancer-ircd's CAPAB settings."
  :group 'erc)

(defcustom erc-capab-identify-prefix "*"
  "The prefix used for unidentified users.

If you change this from the default \"*\", be sure to use a
character not found in IRC nicknames to avoid confusion."
  :group 'erc-capab
  :type '(choice string (const nil)))

(defface erc-capab-identify-unidentified '((t)) ; same as `erc-default-face'
  "Face to use for `erc-capab-identify-prefix'."
  :group 'erc-capab
  :group 'erc-faces)

;;; Define module:

;;;###autoload (autoload 'erc-capab-identify-mode "erc-capab" nil t)
(define-erc-module capab-identify nil
  "Handle dancer-ircd's CAPAB IDENTIFY-MSG and IDENTIFY-CTCP."
  ;; append so that `erc-server-parameters' is already set by `erc-server-005'
  ((add-hook 'erc-server-005-functions 'erc-capab-identify-setup t)
   (add-hook 'erc-server-290-functions 'erc-capab-identify-activate)
   (add-hook 'erc-server-PRIVMSG-functions
             'erc-capab-identify-remove/set-identified-flag)
   (add-hook 'erc-server-NOTICE-functions
             'erc-capab-identify-remove/set-identified-flag)
   (add-hook 'erc-insert-modify-hook 'erc-capab-identify-add-prefix t)
   (mapc (lambda (buffer)
           (when buffer
             (with-current-buffer buffer (erc-capab-identify-setup))))
         (erc-buffer-list 'erc-open-server-buffer-p)))
  ((remove-hook 'erc-server-005-functions 'erc-capab-identify-setup)
   (remove-hook 'erc-server-290-functions 'erc-capab-identify-activate)
   ;; we don't remove the `erc-capab-identify-remove/set-identified-flag' hooks
   ;; because there doesn't seem to be a way to tell the server to turn it off
   (remove-hook 'erc-insert-modify-hook 'erc-capab-identify-add-prefix)))

;;; Variables:

(defvar erc-capab-identify-activated nil
  "CAPAB IDENTIFY-MSG has been activated.")
(make-variable-buffer-local 'erc-capab-identify-activated)

(defvar erc-capab-identify-sent nil
  "CAPAB IDENTIFY-MSG and IDENTIFY-CTCP messages have been sent.")
(make-variable-buffer-local 'erc-capab-identify-sent)

;;; Functions:

(defun erc-capab-identify-setup (&optional proc parsed)
  "Set up CAPAB IDENTIFY on the current server.

Optional argument PROC is the current server's process.
Optional argument PARSED is the current message, a response struct.

These arguments are sent to this function when called as a hook in
`erc-server-005-functions'."
  (unless erc-capab-identify-sent
    (erc-capab-identify-send-messages)))

(defun erc-capab-identify-send-messages ()
  "Send CAPAB IDENTIFY messages if the server supports it."
  (when (and (stringp erc-server-version)
             (string-match "^\\(dancer-ircd\\|hyperion\\)" erc-server-version)
             ;; could possibly check for '("IRCD" . "dancer") in
             ;; `erc-server-parameters' instead of looking for a specific name
             ;; in `erc-server-version'
             (assoc "CAPAB" erc-server-parameters))
    (erc-log "Sending CAPAB IDENTIFY-MSG and IDENTIFY-CTCP")
    (erc-server-send "CAPAB IDENTIFY-MSG")
    (erc-server-send "CAPAB IDENTIFY-CTCP")
    (setq erc-capab-identify-sent t)))


(defun erc-capab-identify-activate (proc parsed)
  "Set `erc-capab-identify-activated' and display an activation message.

PROC is the current server's process.
PARSED is an `erc-parsed' response struct."
  (when (or (string= "IDENTIFY-MSG" (erc-response.contents parsed))
            (string= "IDENTIFY-CTCP" (erc-response.contents parsed)))
    (setq erc-capab-identify-activated t)
    (erc-display-message
     parsed 'notice 'active (format "%s activated"
                                    (erc-response.contents parsed)))))

(defun erc-capab-identify-remove/set-identified-flag (proc parsed)
  "Remove PARSED message's id flag and add the `erc-identified' text property.

PROC is the current server's process.
PARSED is an `erc-parsed' response struct."
  (let ((msg (erc-response.contents parsed)))
    (when (and erc-capab-identify-activated
               (string-match "^\\([-\\+]\\)\\(.+\\)$" msg))
      (setf (erc-response.contents parsed)
            (if erc-capab-identify-mode
                (erc-propertize (match-string 2 msg)
                                'erc-identified
                                (if (string= (match-string 1 msg) "+")
                                    1
                                  0))
              (match-string 2 msg)))
      nil)))

(defun erc-capab-identify-add-prefix ()
  "Add `erc-capab-identify-prefix' to nickname if user is unidentified."
  (when (and erc-capab-identify-prefix
             (erc-with-server-buffer erc-capab-identify-activated))
    (goto-char (or (erc-find-parsed-property) (point-min)))
    (let ((nickname (erc-capab-identify-get-unidentified-nickname
                     (erc-get-parsed-vector (point)))))
      (when (and nickname
                 (goto-char (point-min))
                 ;; assuming the first use of `nickname' is the sender's nick
                 (re-search-forward (regexp-quote nickname) nil t))
        (goto-char (match-beginning 0))
        (insert (erc-propertize erc-capab-identify-prefix
                                'face 'erc-capab-identify-unidentified))))))

(defun erc-capab-identify-get-unidentified-nickname (parsed)
  "Return the nickname of the user if unidentified.
PARSED is an `erc-parsed' response struct."
  (when (and (erc-response-p parsed)
             (equal 0 (get-text-property 0 'erc-identified
                                     (erc-response.contents parsed))))
    (let ((nickuserhost (erc-get-parsed-vector-nick parsed)))
      (when nickuserhost
       (nth 0 (erc-parse-user nickuserhost))))))

(provide 'erc-capab)

;;; erc-capab.el ends here
