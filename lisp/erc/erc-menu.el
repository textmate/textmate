;; erc-menu.el -- Menu-bar definitions for ERC

;; Copyright (C) 2001-2002, 2004-2012  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: comm, processes, menu

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

;; Loading this file defines a menu for ERC.

;;; Code:

(require 'erc)
(require 'easymenu)

(defvar erc-menu-definition
  (list "ERC"
	["Connect to server..." erc t]
	["Disconnect from server..." erc-quit-server erc-server-connected]
	"-"
	["List channels..." erc-list-channels
	 (and erc-server-connected (fboundp 'erc-list-channels))]
	["Join channel..." erc-join-channel erc-server-connected]
	["Start a query..." erc-cmd-QUERY erc-server-connected]
	["Input action..." erc-input-action (erc-default-target)]
	"-"
	(list
	 "Current channel"
	 ["List users in channel" erc-channel-names erc-channel-users]
	 ["List channel operators" erc-cmd-OPS erc-channel-users]
	 ["Set topic..." erc-set-topic
	  (and (and (erc-default-target) (not (erc-query-buffer-p)))
	       (or (not (member "t" erc-channel-modes))
		   (erc-channel-user-op-p (erc-current-nick))))]
	 (list "Channel modes"
	       ["Change mode..." erc-insert-mode-command
		(erc-channel-user-op-p (erc-current-nick))]
	       ["No external send" (erc-toggle-channel-mode "n")
		:active (erc-channel-user-op-p (erc-current-nick))
		:style toggle :selected (member "n" erc-channel-modes)]
	       ["Topic set by channel operator" (erc-toggle-channel-mode "t")
		:style toggle :selected (member "t" erc-channel-modes)
		:active (erc-channel-user-op-p (erc-current-nick))]
	       ["Invite only" (erc-toggle-channel-mode "i")
		:style toggle :selected (member "i" erc-channel-modes)
		:active (erc-channel-user-op-p (erc-current-nick))]
	       ["Private" (erc-toggle-channel-mode "p")
		:style toggle :selected (member "p" erc-channel-modes)
		:active (erc-channel-user-op-p (erc-current-nick))]
	       ["Secret" (erc-toggle-channel-mode "s")
		:style toggle :selected (member "s" erc-channel-modes)
		:active (erc-channel-user-op-p (erc-current-nick))]
	       ["Moderated" (erc-toggle-channel-mode "m")
		:style toggle :selected (member "m" erc-channel-modes)
		:active (erc-channel-user-op-p (erc-current-nick))]
	       ["Set a limit..." erc-set-channel-limit
		(erc-channel-user-op-p (erc-current-nick))]
	       ["Set a key..." erc-set-channel-key
		(erc-channel-user-op-p (erc-current-nick))])
	 ["Leave this channel..." erc-part-from-channel erc-channel-users])
	"-"
	(list "Pals, fools and other keywords"
	      ["Add pal..." erc-add-pal]
	      ["Delete pal..." erc-delete-pal]
	      ["Add fool..." erc-add-fool]
	      ["Delete fool..." erc-delete-fool]
	      ["Add keyword..." erc-add-keyword]
	      ["Delete keyword..." erc-delete-keyword]
	      ["Add dangerous host..." erc-add-dangerous-host]
	      ["Delete dangerous host..." erc-delete-dangerous-host])
	"-"
	(list "IRC services"
	      ["Identify to NickServ..." erc-nickserv-identify
	       (and erc-server-connected (functionp 'erc-nickserv-identify))])
	"-"
	["Save buffer in log" erc-save-buffer-in-logs
	 (fboundp 'erc-save-buffer-in-logs)]
	["Truncate buffer" erc-truncate-buffer (fboundp 'erc-truncate-buffer)]
	"-"
	["Customize ERC" (customize-group 'erc) t]
	["Enable/Disable ERC Modules" (customize-variable 'erc-modules) t]
	["Show ERC version" erc-version t])
  "ERC menu definition.")

(defvar erc-menu-defined nil
  "Internal variable used to keep track of whether we've defined the
ERC menu yet.")

;;;###autoload (autoload 'erc-menu-mode "erc-menu" nil t)
(define-erc-module menu nil
  "Enable a menu in ERC buffers."
  ((unless erc-menu-defined
     ;; make sure the menu only gets defined once, since Emacs 22
     ;; activates it immediately
     (easy-menu-define erc-menu erc-mode-map "ERC menu" erc-menu-definition)
     (setq erc-menu-defined t))
   (if (featurep 'xemacs)
       (progn
	 ;; the menu isn't automatically added to the menu bar in
	 ;; XEmacs
	 (add-hook 'erc-mode-hook 'erc-menu-add)
	 (dolist (buffer (erc-buffer-list))
	   (with-current-buffer buffer (erc-menu-add))))
     (erc-menu-add)))
  ((if (featurep 'xemacs)
       (progn
	 (remove-hook 'erc-mode-hook 'erc-menu-add)
	 (dolist (buffer (erc-buffer-list))
	   (with-current-buffer buffer (erc-menu-remove))))
     (erc-menu-remove)
     ;; `easy-menu-remove' is a no-op in Emacs 22
     (message "You might have to restart Emacs to remove the ERC menu"))))

;; silence byte-compiler warning
(defvar erc-menu)

(defun erc-menu-add ()
  "Add the ERC menu to the current buffer."
  (easy-menu-add erc-menu erc-mode-map))

(defun erc-menu-remove ()
  "Remove the ERC menu from the current buffer."
  (easy-menu-remove erc-menu))

(provide 'erc-menu)

;;; erc-menu.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

