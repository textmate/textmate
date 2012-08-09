;;; mail-hist.el --- headers and message body history for outgoing mail

;; Copyright (C) 1994, 2001-2012 Free Software Foundation, Inc.

;; Author: Karl Fogel <kfogel@red-bean.com>
;; Created: March, 1994
;; Keywords: mail, history
;; Package: mail-utils

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

;; Thanks to Jim Blandy for mentioning ring.el.  It saved a lot of
;; time.
;;
;; To use this package, put it in a directory in your load-path, and
;; put this in your .emacs file:
;;
;; (load "mail-hist" nil t)
;;
;; Or you could do it with autoloads and hooks in your .emacs:
;;
;; (add-hook 'mail-mode-hook 'mail-hist-define-keys)
;; (add-hook 'mail-send-hook 'mail-hist-put-headers-into-history)
;; (add-hook 'vm-mail-mode-hook 'mail-hist-define-keys) ;or rmail, etc
;; (autoload 'mail-hist-define-keys "mail-hist")
;; (autoload 'mail-hist-put-headers-into-history "mail-hist")
;;
;; Once it's installed, use M-p and M-n from mail headers to recover
;; previous/next contents in the history for that header, or, in the
;; body of the message, to recover previous/next text of the message.
;; This only applies to outgoing mail -- mail-hist ignores received
;; messages.
;;
;; Although repeated history requests do clear out the text from the
;; previous request, an isolated request just inserts its text at
;; point, so that you can mix the histories of different messages
;; easily.  This might be confusing at times, but there should be no
;; problems that undo can't handle.

;;; Code:
(require 'ring)
(require 'sendmail)

(defgroup mail-hist nil
  "Headers and message body history for outgoing mail."
  :prefix "mail-hist-"
  :group 'mail)

;;;###autoload
(defun mail-hist-define-keys ()
  "Define keys for accessing mail header history.  For use in hooks."
  (local-set-key "\M-p" 'mail-hist-previous-input)
  (local-set-key "\M-n" 'mail-hist-next-input))

;;;###autoload
(defun mail-hist-enable ()
  (add-hook 'mail-mode-hook 'mail-hist-define-keys)
  (add-hook 'mail-send-hook 'mail-hist-put-headers-into-history))

(defvar mail-hist-header-ring-alist nil
  "Alist of form (header-name . history-ring).
Used for knowing which history list to look in when the user asks for
previous/next input.")

(defcustom mail-hist-history-size (or kill-ring-max 1729)
  "*The maximum number of elements in a mail field's history.
Oldest elements are dumped first."
  :type 'integer
  :group 'mail-hist)

;;;###autoload
(defcustom mail-hist-keep-history t
  "*Non-nil means keep a history for headers and text of outgoing mail."
  :type 'boolean
  :group 'mail-hist)

;; For handling repeated history requests
(defvar mail-hist-access-count 0)

(defvar mail-hist-last-bounds nil)
;; (start . end) A pair indicating the buffer positions delimiting the
;; last inserted history, so it can be replaced by a new input if the
;; command is repeated.

(defvar mail-hist-header-regexp "^[^:]*:"
  "Regular expression for matching headers in a mail message.")

(defsubst mail-hist-current-header-name ()
  "Get name of mail header point is currently in, without the colon.
Returns nil if not in a header, implying that point is in the body of
the message."
  (if (>= (point) (mail-text-start))
      nil ; then we are in the body of the message
    (save-excursion
      (let* ((body-start
	      (mail-text-start))
             (name-start
              (re-search-backward mail-hist-header-regexp nil t))
             (name-end
              (prog2 (search-forward ":" body-start t) (1- (point)))))
        (and
         name-start
         name-end
         (downcase (buffer-substring-no-properties name-start name-end)))))))

(defsubst mail-hist-forward-header (count)
  "Move forward COUNT headers (backward if COUNT is negative).
If last/first header is encountered first, stop there and returns
nil.

Places point on the first non-whitespace on the line following the
colon after the header name, or on the second space following that if
the header is empty."
  (let ((boundary (mail-header-end)))
    (and
     (> boundary 0)
     (let ((unstopped t))
       (setq boundary (save-excursion
                    (goto-char boundary)
                    (beginning-of-line)
                    (1- (point))))
       (if (> count 0)
           (while (> count 0)
             (setq
              unstopped
              (re-search-forward mail-hist-header-regexp boundary t))
             (setq count (1- count)))
         ;; because the current header will match too.
         (setq count (1- count))
         ;; count is negative
         (while (< count 0)
           (setq
            unstopped
            (re-search-backward mail-hist-header-regexp nil t))
           (setq count (1+ count)))
         ;; we end up behind the header, so must move to the front
         (re-search-forward mail-hist-header-regexp boundary t))
       ;; Now we are right after the colon
       (and (looking-at "\\s-") (forward-char 1))
       ;; return nil if didn't go as far as asked, otherwise point
       unstopped))))

(defsubst mail-hist-beginning-of-header ()
  "Move to the start of the current header.
The start of the current header is defined as one space after the
colon, or just after the colon if it is not followed by whitespace."
  ;; this is slick as all heck:
  (if (mail-hist-forward-header -1)
      (mail-hist-forward-header 1)
    (mail-hist-forward-header 1)
    (mail-hist-forward-header -1)))

(defsubst mail-hist-current-header-contents ()
  "Get the contents of the mail header in which point is located."
  (save-excursion
    (mail-hist-beginning-of-header)
    (let ((start (point)))
      (or (mail-hist-forward-header 1)
          (goto-char (mail-header-end)))
      (beginning-of-line)
      (buffer-substring start (1- (point))))))

(defsubst mail-hist-get-header-ring (header)
  "Get HEADER's history ring, or nil if none.
HEADER is a string without the colon."
  (setq header (downcase header))
  (cdr (assoc header mail-hist-header-ring-alist)))

(defcustom mail-hist-text-size-limit nil
  "*Don't store any header or body with more than this many characters.
If the value is nil, that means no limit on text size."
  :type '(choice (const nil) integer)
  :group 'mail-hist)

(defun mail-hist-text-too-long-p (text)
  "Return non-nil if TEXT's length exceeds `mail-hist-text-size-limit'."
  (if mail-hist-text-size-limit
      (> (length text) mail-hist-text-size-limit)))

(defsubst mail-hist-add-header-contents-to-ring (header &optional contents)
  "Add the contents of HEADER to the header history ring.
Optional argument CONTENTS is a string which will be the contents
\(instead of whatever's found in the header)."
  (setq header (downcase header))
  (let ((ctnts (or contents (mail-hist-current-header-contents)))
        (ring  (cdr (assoc header mail-hist-header-ring-alist))))
    (if (mail-hist-text-too-long-p ctnts) (setq ctnts ""))
    (or ring
        ;; If the ring doesn't exist, we'll have to make it and add it
        ;; to the mail-header-ring-alist:
        (prog1
            (setq ring (make-ring mail-hist-history-size))
          (setq mail-hist-header-ring-alist
                (cons (cons header ring) mail-hist-header-ring-alist))))
    (ring-insert ring ctnts)))

;;;###autoload
(defun mail-hist-put-headers-into-history ()
  "Put headers and contents of this message into mail header history.
Each header has its own independent history, as does the body of the
message.

This function normally would be called when the message is sent."
  (and
   mail-hist-keep-history
   (save-excursion
     (goto-char (point-min))
     (while (mail-hist-forward-header 1)
       (mail-hist-add-header-contents-to-ring
        (mail-hist-current-header-name)))
     (let ((body-contents
	    (buffer-substring (mail-text-start) (point-max))))
       (mail-hist-add-header-contents-to-ring "body" body-contents)))))



(defun mail-hist-retrieve-and-insert (header access-func)
  "Helper for `mail-hist-previous-input' and `mail-hist-next-input'."
  (setq header (downcase header))
  (let* ((ring (cdr (assoc header mail-hist-header-ring-alist)))
         (len (ring-length ring))
         (repeat (eq last-command 'mail-hist-input-access)))
    (if repeat
        (setq mail-hist-access-count
              (funcall access-func mail-hist-access-count len))
      (setq mail-hist-access-count 0))
    (if (null ring)
        (progn
          (ding)
          (message "No history for \"%s\"." header))
      (if (ring-empty-p ring)
          (error "\"%s\" ring is empty" header)
        (and repeat
             (delete-region (car mail-hist-last-bounds)
                            (cdr mail-hist-last-bounds)))
        (let ((start (point)))
          (insert (ring-ref ring mail-hist-access-count))
          (setq mail-hist-last-bounds (cons start (point)))
          (setq this-command 'mail-hist-input-access)
          ;; Special case: when flipping through message bodies, it's
          ;; usually most useful for point to stay at the top.  This
          ;; is because the unique part of a message in a thread is
          ;; more likely to be at the top than the bottom, as the
          ;; bottom is often just the same quoted history for every
          ;; message in the thread, differing only in indentation
          ;; level.
          (if (string-equal header "body")
              (goto-char start)))
        ))))


(defun mail-hist-previous-input (header)
  "Insert the previous contents of this mail header or message body.
Moves back through the history of sent mail messages.  Each header has
its own independent history, as does the body of the message.

The history only contains the contents of outgoing messages, not
received mail."
  (interactive (list (or (mail-hist-current-header-name) "body")))
  (mail-hist-retrieve-and-insert header 'ring-plus1))


(defun mail-hist-next-input (header)
  "Insert next contents of this mail header or message body.
Moves back through the history of sent mail messages.  Each header has
its own independent history, as does the body of the message.

Although you can do so, it does not make much sense to call this
without having called `mail-hist-previous-header' first
\(\\[mail-hist-previous-header]).

The history only contains the contents of outgoing messages, not
received mail."
  (interactive (list (or (mail-hist-current-header-name) "body")))
  (mail-hist-retrieve-and-insert header 'ring-minus1))


(provide 'mail-hist)

;;; mail-hist.el ends here
