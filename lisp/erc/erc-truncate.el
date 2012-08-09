;;; erc-truncate.el --- Functions for truncating ERC buffers

;; Copyright (C) 2003-2004, 2006-2012 Free Software Foundation, Inc.

;; Author: Andreas Fuchs <asf@void.at>
;; Keywords: IRC, chat, client, Internet, logging

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

;; This implements buffer truncation (and optional log file writing
;; support for the Emacs IRC client. Use `erc-truncate-mode' to switch
;; on. Use `erc-enable-logging' to enable logging of the stuff which
;; is getting truncated.

;;; Code:

(require 'erc)

(defgroup erc-truncate nil
  "Truncate buffers when they reach a certain size"
  :group 'erc)

(defcustom erc-max-buffer-size 30000
  "*Maximum size in chars of each ERC buffer.
Used only when auto-truncation is enabled.
\(see `erc-truncate-buffer' and `erc-insert-post-hook')."
  :group 'erc-truncate
  :type 'integer)

;;;###autoload (autoload 'erc-truncate-mode "erc-truncate" nil t)
(define-erc-module truncate nil
  "Truncate a query buffer if it gets too large.
This prevents the query buffer from getting too large, which can
bring any grown Emacs to its knees after a few days worth of
tracking heavy-traffic channels."
  ;;enable
  ((add-hook 'erc-insert-post-hook 'erc-truncate-buffer))
  ;; disable
  ((remove-hook 'erc-insert-post-hook 'erc-truncate-buffer)))

;;;###autoload
(defun erc-truncate-buffer-to-size (size &optional buffer)
  "Truncates the buffer to the size SIZE.
If BUFFER is not provided, the current buffer is assumed.  The deleted
region is logged if `erc-logging-enabled' returns non-nil."
  ;; If buffer is non-nil, but get-buffer does not return anything,
  ;; then this is a bug.  If buffer is a buffer name, get the buffer
  ;; object.  If buffer is nil, use the current buffer.
  (if (not buffer)
      (setq buffer (current-buffer))
    (unless (get-buffer buffer)
      (error "erc-truncate-buffer-to-size: %S is not a buffer" buffer)))
  (when (> (buffer-size buffer) (+ size 512))
    (with-current-buffer buffer
      ;; Note that when erc-insert-post-hook runs, the buffer is
      ;; narrowed to the new message.  So do this delicate widening.
      ;; I am not sure, I think this was not recommended behavior in
      ;; Emacs 20.
      (save-restriction
	(widen)
	(let ((end (- erc-insert-marker size)))
	  ;; truncate at line boundaries
	  (goto-char end)
	  (beginning-of-line)
	  (setq end (point))
	  ;; try to save the current buffer using
	  ;; `erc-save-buffer-in-logs'.  We use this, in case the
	  ;; user has both `erc-save-buffer-in-logs' and
	  ;; `erc-truncate-buffer' in `erc-insert-post-hook'.  If
	  ;; this is the case, only the non-saved part of the current
	  ;; buffer should be saved.  Rather than appending the
	  ;; deleted part of the buffer to the log file.
	  ;;
	  ;; Alternatively this could be made conditional on:
	  ;; (not (memq 'erc-save-buffer-in-logs
	  ;;             erc-insert-post-hook))
	  ;; Comments?
	  (when (and (boundp 'erc-enable-logging)
		     erc-enable-logging
		     (erc-logging-enabled buffer))
	    (erc-save-buffer-in-logs))
	  ;; disable undoing for the truncating
	  (buffer-disable-undo)
	  (let ((inhibit-read-only t))
	    (delete-region (point-min) end)))
	(buffer-enable-undo)))))

;;;###autoload
(defun erc-truncate-buffer ()
  "Truncates the current buffer to `erc-max-buffer-size'.
Meant to be used in hooks, like `erc-insert-post-hook'."
  (interactive)
  (erc-truncate-buffer-to-size erc-max-buffer-size))

(provide 'erc-truncate)
;;; erc-truncate.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

