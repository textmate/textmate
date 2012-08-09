;;; sieve.el --- Utilities to manage sieve scripts

;; Copyright (C) 2001-2012 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>

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

;; This file contain utilities to facilitate upload, download and
;; general management of sieve scripts.  Currently only the
;; Managesieve protocol is supported (using sieve-manage.el), but when
;; (useful) alternatives become available, they might be supported as
;; well.
;;
;; The cursor navigation was inspired by biff-mode by Franklin Lee.
;;
;; Release history:
;;
;; 2001-10-31 Committed to Oort Gnus.
;; 2002-07-27 Fix down-mouse-2 and down-mouse-3 in manage-mode.  Fix menubar
;;            in manage-mode.  Change some messages.  Added sieve-deactivate*,
;;            sieve-remove.  Fixed help text in manage-mode.  Suggested by
;;            Ned Ludd.
;;
;; Todo:
;;
;; * Namespace?  This file contains `sieve-manage' and
;;   `sieve-manage-mode', but there is a sieve-manage.el file as well.
;;   Can't think of a good solution though, this file need a *-mode,
;;   and naming it `sieve-mode' would collide with sieve-mode.el.  One
;;   solution would be to come up with some better name that this file
;;   can use that doesn't have the managesieve specific "manage" in
;;   it.  sieve-dired?  i dunno.  we could copy all off sieve.el into
;;   sieve-manage.el too, but I'd like to separate the interface from
;;   the protocol implementation since the backends are likely to
;;   change (well).
;;
;; * Define servers?  We could have a customize buffer to create a server,
;;   with authentication/stream/etc parameters, much like Gnus, and then
;;   only use names of defined servers when interacting with M-x sieve-*.
;;   Right now you can't use STARTTLS, which sieve-manage.el provides

;;; Code:

(require 'sieve-manage)
(require 'sieve-mode)

;; User customizable variables:

(defgroup sieve nil
  "Manage sieve scripts."
  :version "22.1"
  :group 'tools)

(defcustom sieve-new-script "<new script>"
  "Name of name script indicator."
  :type 'string
  :group 'sieve)

(defcustom sieve-buffer "*sieve*"
  "Name of sieve management buffer."
  :type 'string
  :group 'sieve)

(defcustom sieve-template "\
require \"fileinto\";

# Example script (remove comment character '#' to make it effective!):
#
# if header :contains \"from\" \"coyote\" {
#   discard;
# } elsif header :contains [\"subject\"] [\"$$$\"] {
#   discard;
# } else {
#  fileinto \"INBOX\";
# }
"
  "Template sieve script."
  :type 'string
  :group 'sieve)

;; Internal variables:

(defvar sieve-manage-buffer nil)
(defvar sieve-buffer-header-end nil)
(defvar sieve-buffer-script-name nil
  "The real script name of the buffer.")
(make-local-variable 'sieve-buffer-script-name)

;; Sieve-manage mode:

(defvar sieve-manage-mode-map
  (let ((map (make-sparse-keymap)))
    ;; various
    (define-key map "?" 'sieve-help)
    (define-key map "h" 'sieve-help)
    (define-key map "q" 'sieve-bury-buffer)
    ;; activating
    (define-key map "m" 'sieve-activate)
    (define-key map "u" 'sieve-deactivate)
    (define-key map "\M-\C-?" 'sieve-deactivate-all)
    ;; navigation keys
    (define-key map "\C-p" 'sieve-prev-line)
    (define-key map [up] 'sieve-prev-line)
    (define-key map "\C-n" 'sieve-next-line)
    (define-key map [down] 'sieve-next-line)
    (define-key map " " 'sieve-next-line)
    (define-key map "n" 'sieve-next-line)
    (define-key map "p" 'sieve-prev-line)
    (define-key map "\C-m" 'sieve-edit-script)
    (define-key map "f" 'sieve-edit-script)
    (define-key map "o" 'sieve-edit-script-other-window)
    (define-key map "r" 'sieve-remove)
    (define-key map "q" 'sieve-manage-quit)
    (define-key map [(down-mouse-2)] 'sieve-edit-script)
    (define-key map [(down-mouse-3)] 'sieve-manage-mode-menu)
    map)
  "Keymap for `sieve-manage-mode'.")

(easy-menu-define sieve-manage-mode-menu sieve-manage-mode-map
  "Sieve Menu."
  '("Manage Sieve"
    ["Edit script" sieve-edit-script t]
    ["Activate script" sieve-activate t]
    ["Deactivate script" sieve-deactivate t]))

(define-derived-mode sieve-manage-mode fundamental-mode "Sieve-manage"
  "Mode used for sieve script management."
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines t)
  (easy-menu-add sieve-manage-mode-menu sieve-manage-mode-map))

(put 'sieve-manage-mode 'mode-class 'special)

;; Commands used in sieve-manage mode:

(defun sieve-manage-quit ()
  "Quit."
  (interactive)
  (kill-buffer (current-buffer)))

(defun sieve-activate (&optional pos)
  (interactive "d")
  (let ((name (sieve-script-at-point)) err)
    (when (or (null name) (string-equal name sieve-new-script))
      (error "No sieve script at point"))
    (message "Activating script %s..." name)
    (setq err (sieve-manage-setactive name sieve-manage-buffer))
    (sieve-refresh-scriptlist)
    (if (sieve-manage-ok-p err)
	(message "Activating script %s...done" name)
      (message "Activating script %s...failed: %s" name (nth 2 err)))))

(defun sieve-deactivate-all (&optional pos)
  (interactive "d")
  (let ((name (sieve-script-at-point)) err)
    (message "Deactivating scripts...")
    (setq err (sieve-manage-setactive "" sieve-manage-buffer))
    (sieve-refresh-scriptlist)
    (if (sieve-manage-ok-p err)
	(message "Deactivating scripts...done")
      (message "Deactivating scripts...failed: %s" (nth 2 err)))))

(defalias 'sieve-deactivate 'sieve-deactivate-all)

(defun sieve-remove (&optional pos)
  (interactive "d")
  (let ((name (sieve-script-at-point)) err)
    (when (or (null name) (string-equal name sieve-new-script))
      (error "No sieve script at point"))
    (message "Removing sieve script %s..." name)
    (setq err (sieve-manage-deletescript name sieve-manage-buffer))
    (unless (sieve-manage-ok-p err)
      (error "Removing sieve script %s...failed: " err))
    (sieve-refresh-scriptlist)
    (message "Removing sieve script %s...done" name)))

(defun sieve-edit-script (&optional pos)
  (interactive "d")
  (let ((name (sieve-script-at-point)))
    (unless name
      (error "No sieve script at point"))
    (if (not (string-equal name sieve-new-script))
	(let ((newbuf (generate-new-buffer name))
	      err)
	  (setq err (sieve-manage-getscript name newbuf sieve-manage-buffer))
	  (switch-to-buffer newbuf)
	  (unless (sieve-manage-ok-p err)
	    (error "Sieve download failed: %s" err)))
      (switch-to-buffer (get-buffer-create "template.siv"))
      (insert sieve-template))
    (sieve-mode)
    (setq sieve-buffer-script-name name)
    (message
     (substitute-command-keys
      "Press \\[sieve-upload] to upload script to server."))))

(defmacro sieve-change-region (&rest body)
  "Turns off sieve-region before executing BODY, then re-enables it after.
Used to bracket operations which move point in the sieve-buffer."
  `(progn
     (sieve-highlight nil)
     ,@body
     (sieve-highlight t)))
(put 'sieve-change-region 'lisp-indent-function 0)

(defun sieve-next-line (&optional arg)
  (interactive)
  (unless arg
    (setq arg 1))
  (if (save-excursion
	(forward-line arg)
	(sieve-script-at-point))
      (sieve-change-region
	(forward-line arg))
    (message "End of list")))

(defun sieve-prev-line (&optional arg)
  (interactive)
  (unless arg
    (setq arg -1))
  (if (save-excursion
	(forward-line arg)
	(sieve-script-at-point))
      (sieve-change-region
	(forward-line arg))
    (message "Beginning of list")))

(defun sieve-help ()
  "Display help for various sieve commands."
  (interactive)
  (if (eq last-command 'sieve-help)
      ;; would need minor-mode for log-edit-mode
      (describe-function 'sieve-mode)
    (message "%s" (substitute-command-keys
	      "`\\[sieve-edit-script]':edit `\\[sieve-activate]':activate `\\[sieve-deactivate]':deactivate `\\[sieve-remove]':remove"))))

(defun sieve-bury-buffer (buf &optional mainbuf)
  "Hide the buffer BUF that was temporarily popped up.
BUF is assumed to be a temporary buffer used from the buffer MAINBUF."
  (interactive (list (current-buffer)))
  (save-current-buffer
    (let ((win (if (eq buf (window-buffer (selected-window))) (selected-window)
		 (get-buffer-window buf t))))
      (when win
	(if (window-dedicated-p win)
	    (condition-case ()
		(delete-window win)
	      (error (iconify-frame (window-frame win))))
	  (if (and mainbuf (get-buffer-window mainbuf))
	      (delete-window win)))))
    (with-current-buffer buf
      (bury-buffer (unless (and (eq buf (window-buffer (selected-window)))
				(not (window-dedicated-p (selected-window))))
		     buf)))
    (when mainbuf
      (let ((mainwin (or (get-buffer-window mainbuf)
			 (get-buffer-window mainbuf 'visible))))
	(when mainwin (select-window mainwin))))))

;; Create buffer:

(defun sieve-setup-buffer (server port)
  (setq buffer-read-only nil)
  (erase-buffer)
  (buffer-disable-undo)
  (insert "\
Server  : " server ":" (or port "2000") "

")
  (set (make-local-variable 'sieve-buffer-header-end)
       (point-max)))

(defun sieve-script-at-point (&optional pos)
  "Return name of sieve script at point POS, or nil."
  (interactive "d")
  (get-char-property (or pos (point)) 'script-name))

(eval-and-compile
  (defalias 'sieve-make-overlay (if (featurep 'xemacs)
				    'make-extent
				  'make-overlay))
  (defalias 'sieve-overlay-put (if (featurep 'xemacs)
				   'set-extent-property
				 'overlay-put))
  (defalias 'sieve-overlays-at (if  (featurep 'xemacs)
				   'extents-at
				 'overlays-at)))

(defun sieve-highlight (on)
  "Turn ON or off highlighting on the current language overlay."
  (sieve-overlay-put (car (sieve-overlays-at (point)))
		     'face (if on 'highlight 'default)))

(defun sieve-insert-scripts (scripts)
  "Format and insert LANGUAGE-LIST strings into current buffer at point."
  (while scripts
    (let ((p (point))
	  (ext nil)
	  (script (pop scripts)))
      (if (consp script)
	  (insert (format " ACTIVE %s" (cdr script)))
	(insert (format "        %s" script)))
      (setq ext (sieve-make-overlay p (point)))
      (sieve-overlay-put ext 'mouse-face 'highlight)
      (sieve-overlay-put ext 'script-name (if (consp script)
					      (cdr script)
					    script))
      (insert "\n"))))

(defun sieve-open-server (server &optional port)
  "Open SERVER (on PORT) and authenticate."
  (with-current-buffer
      (or ;; open server
       (set (make-local-variable 'sieve-manage-buffer)
	    (sieve-manage-open server))
       (error "Error opening server %s" server))
    (sieve-manage-authenticate)))

(defun sieve-refresh-scriptlist ()
  (interactive)
  (with-current-buffer sieve-buffer
    (setq buffer-read-only nil)
    (delete-region (or sieve-buffer-header-end (point-max)) (point-max))
    (goto-char (point-max))
    ;; get list of script names and print them
    (let ((scripts (sieve-manage-listscripts sieve-manage-buffer)))
      (if (null scripts)
	  (insert
           (substitute-command-keys
            (format
             "No scripts on server, press \\[sieve-edit-script] on %s to create a new script.\n"
             sieve-new-script)))
	(insert
         (substitute-command-keys
          (format (concat "%d script%s on server, press \\[sieve-edit-script] on a script "
                          "name edits it, or\npress \\[sieve-edit-script] on %s to create "
                          "a new script.\n") (length scripts)
                          (if (eq (length scripts) 1) "" "s")
                          sieve-new-script))))
      (save-excursion
	(sieve-insert-scripts (list sieve-new-script))
	(sieve-insert-scripts scripts)))
    (sieve-highlight t)
    (setq buffer-read-only t)))

;;;###autoload
(defun sieve-manage (server &optional port)
  (interactive "sServer: ")
  (switch-to-buffer (get-buffer-create sieve-buffer))
  (sieve-manage-mode)
  (sieve-setup-buffer server port)
  (if (sieve-open-server server port)
      (sieve-refresh-scriptlist)
    (message "Could not open server %s" server)))

;;;###autoload
(defun sieve-upload (&optional name)
  (interactive)
  (when (or (get-buffer sieve-buffer) (call-interactively 'sieve-manage))
    (let ((script (buffer-string)) err)
      (with-current-buffer (get-buffer sieve-buffer)
	(setq err (sieve-manage-putscript
                   (or name sieve-buffer-script-name (buffer-name))
                   script sieve-manage-buffer))
	(if (sieve-manage-ok-p err)
	    (message (substitute-command-keys
		      "Sieve upload done.  Use \\[sieve-manage] to manage scripts."))
	  (message "Sieve upload failed: %s" (nth 2 err)))))))

;;;###autoload
(defun sieve-upload-and-bury (&optional name)
  (interactive)
  (sieve-upload name)
  (bury-buffer))

(provide 'sieve)

;; sieve.el ends here
