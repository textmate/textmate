;;; gnus-draft.el --- draft message support for Gnus

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;;; Code:

(require 'gnus)
(require 'gnus-sum)
(require 'message)
(require 'gnus-msg)
(require 'nndraft)
(require 'gnus-agent)
(eval-when-compile (require 'cl))
(eval-when-compile
  (when (featurep 'xemacs)
    (require 'easy-mmode))) ; for `define-minor-mode'

;;; Draft minor mode

(defvar gnus-draft-mode-map
  (let ((map (make-sparse-keymap)))
    (gnus-define-keys map
     "Dt" gnus-draft-toggle-sending
     "e"  gnus-draft-edit-message ;; Use `B w' for `gnus-summary-edit-article'
     "De" gnus-draft-edit-message
     "Ds" gnus-draft-send-message
     "DS" gnus-draft-send-all-messages)
    map))

(defun gnus-draft-make-menu-bar ()
  (unless (boundp 'gnus-draft-menu)
    (easy-menu-define
     gnus-draft-menu gnus-draft-mode-map ""
     '("Drafts"
       ["Toggle whether to send" gnus-draft-toggle-sending t]
       ["Edit" gnus-draft-edit-message t]
       ["Send selected message(s)" gnus-draft-send-message t]
       ["Send all messages" gnus-draft-send-all-messages t]
       ["Delete draft" gnus-summary-delete-article t]))))

(define-minor-mode gnus-draft-mode
  "Minor mode for providing a draft summary buffers.

\\{gnus-draft-mode-map}"
  :lighter " Draft" :keymap gnus-draft-mode-map
  (cond
   ((not (derived-mode-p 'gnus-summary-mode)) (setq gnus-draft-mode nil))
   (gnus-draft-mode
    ;; Set up the menu.
    (when (gnus-visual-p 'draft-menu 'menu)
      (gnus-draft-make-menu-bar))
    (add-hook 'gnus-summary-prepare-exit-hook 'gnus-draft-clear-marks t t))))

;;; Commands

(defun gnus-draft-toggle-sending (article)
  "Toggle whether to send an article or not."
  (interactive (list (gnus-summary-article-number)))
  (if (gnus-draft-article-sendable-p article)
      (progn
	(push article gnus-newsgroup-unsendable)
	(gnus-summary-mark-article article gnus-unsendable-mark))
    (setq gnus-newsgroup-unsendable
	  (delq article gnus-newsgroup-unsendable))
    (gnus-summary-mark-article article gnus-unread-mark))
  (gnus-summary-position-point))

(defun gnus-draft-edit-message ()
  "Enter a mail/post buffer to edit and send the draft."
  (interactive)
  (let ((article (gnus-summary-article-number))
	(group gnus-newsgroup-name))
    (gnus-draft-check-draft-articles (list article))
    (gnus-summary-mark-as-read article gnus-canceled-mark)
    (gnus-draft-setup article group t)
    (set-buffer-modified-p t)
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(message-remove-header "date")))
    (let ((message-draft-headers
	   (delq 'Date (copy-sequence message-draft-headers))))
      (save-buffer))
    (let ((gnus-verbose-backends nil))
      (gnus-request-expire-articles (list article) group t))
    (push
     `((lambda ()
	 (when (gnus-buffer-exists-p ,gnus-summary-buffer)
	   (save-excursion
	     (set-buffer ,gnus-summary-buffer)
	     (gnus-cache-possibly-remove-article ,article nil nil nil t)))))
     message-send-actions)))

(defun gnus-draft-send-message (&optional n)
  "Send the current draft(s).
Obeys the standard process/prefix convention."
  (interactive "P")
  (let* ((articles (gnus-summary-work-articles n))
	 (total (length articles))
	 article)
    (gnus-draft-check-draft-articles articles)
    (while (setq article (pop articles))
      (gnus-summary-remove-process-mark article)
      (unless (memq article gnus-newsgroup-unsendable)
	(let ((message-sending-message
	       (format "Sending message %d of %d..."
		       (- total (length articles)) total)))
	  (gnus-draft-send article gnus-newsgroup-name t))
	(gnus-summary-mark-article article gnus-canceled-mark)))))

(defun gnus-draft-send (article &optional group interactive)
  "Send message ARTICLE."
  (let* ((is-queue (or (not group)
                       (equal group "nndraft:queue")))
         (message-syntax-checks (if interactive message-syntax-checks
                                  'dont-check-for-anything-just-trust-me))
         (message-hidden-headers nil)
         (message-inhibit-body-encoding (or is-queue
                                            message-inhibit-body-encoding))
         (message-send-hook (and (not is-queue)
                                 message-send-hook))
         (message-setup-hook (and (not is-queue)
                                  message-setup-hook))
	 (gnus-message-setup-hook (and (not is-queue)
				       gnus-message-setup-hook))
	 (message-signature (and (not is-queue)
				 message-signature))
         (gnus-agent-queue-mail (and (not is-queue)
                                     gnus-agent-queue-mail))
	 (rfc2047-encode-encoded-words nil)
         type method move-to)
    (gnus-draft-setup article (or group "nndraft:queue") nil 'dont-pop)
    ;; We read the meta-information that says how and where
    ;; this message is to be sent.
    (save-restriction
      (message-narrow-to-headers)
      (when (re-search-forward
	     (concat "^" (regexp-quote gnus-agent-target-move-group-header)
		     ":") nil t)
	(skip-syntax-forward "-")
	(setq move-to (buffer-substring (point) (point-at-eol)))
	(message-remove-header gnus-agent-target-move-group-header))
      (goto-char (point-min))
      (when (re-search-forward
	     (concat "^" (regexp-quote gnus-agent-meta-information-header) ":")
	     nil t)
	(setq type (ignore-errors (read (current-buffer)))
	      method (ignore-errors (read (current-buffer))))
	(message-remove-header gnus-agent-meta-information-header)))
    ;; Let Agent restore any GCC lines and have message.el perform them.
    (gnus-agent-restore-gcc)
    ;; Then we send it.  If we have no meta-information, we just send
    ;; it and let Message figure out how.
    (when (and (or (null method)
		   (gnus-server-opened method)
		   (gnus-open-server method))
	       (if type
		   (let ((message-this-is-news (eq type 'news))
			 (message-this-is-mail (eq type 'mail))
			 (gnus-post-method method)
			 (message-post-method method))
		     (if move-to
			 (gnus-inews-do-gcc move-to)
		       (message-send-and-exit)))
		 (if move-to
		     (gnus-inews-do-gcc move-to)
		   (message-send-and-exit))))
      (let ((gnus-verbose-backends nil))
	(gnus-request-expire-articles
	 (list article) (or group "nndraft:queue") t)))))

(defun gnus-draft-send-all-messages ()
  "Send all the sendable drafts."
  (interactive)
  (when (or
	 gnus-expert-user
	 (gnus-y-or-n-p
	  "Send all drafts? "))
    (gnus-uu-mark-buffer)
    (gnus-draft-send-message)))

(defun gnus-group-send-queue ()
  "Send all sendable articles from the queue group."
  (interactive)
  (when (or gnus-plugged
	    (not gnus-agent-prompt-send-queue)
	    (gnus-y-or-n-p "Gnus is unplugged; really send queue? "))
    (gnus-activate-group "nndraft:queue")
    (save-excursion
      (let* ((articles (nndraft-articles))
	     (unsendable (gnus-uncompress-range
			  (cdr (assq 'unsend
				     (gnus-info-marks
				      (gnus-get-info "nndraft:queue"))))))
	     (gnus-posting-styles nil)
	     message-send-mail-partially-limit
	     (total (length articles))
	     article)
	(while (setq article (pop articles))
	  (unless (memq article unsendable)
	    (let ((message-sending-message
		   (format "Sending message %d of %d..."
			   (- total (length articles)) total)))
	      (gnus-draft-send article))))))
    (gnus-group-refresh-group "nndraft:queue")))

;;;###autoload
(defun gnus-draft-reminder ()
  "Reminder user if there are unsent drafts."
  (interactive)
  (if (gnus-alive-p)
      (let (active)
	(catch 'continue
	  (dolist (group '("nndraft:drafts" "nndraft:queue"))
	    (setq active (gnus-activate-group group))
	    (if (and active (>= (cdr active) (car active)))
		(if (y-or-n-p "There are unsent drafts.  Confirm to exit? ")
		    (throw 'continue t)
		  (error "Stop!"))))))))

(defcustom gnus-draft-setup-hook nil
  "Hook run after setting up a draft buffer."
  :group 'gnus-message
  :version "23.1" ;; No Gnus
  :type 'hook)


(defun gnus-draft-setup (narticle group &optional restore dont-pop)
  "Setup a mail draft buffer.
If DONT-POP is nil, display the buffer after setting it up."
  (let (ga)
    (gnus-setup-message 'forward
      (let ((article narticle))
        (message-mail nil nil nil nil
                      (if dont-pop
                          (lambda (buf) (set-buffer (get-buffer-create buf)))))
        (let ((inhibit-read-only t))
          (erase-buffer))
        (if (not (gnus-request-restore-buffer article group))
            (error "Couldn't restore the article")
          (when (and restore
                     (equal group "nndraft:queue"))
            (mime-to-mml))
          ;; Insert the separator.
          (goto-char (point-min))
          (search-forward "\n\n")
          (forward-char -1)
          (save-restriction
            (narrow-to-region (point-min) (point))
            (setq ga
                  (message-fetch-field gnus-draft-meta-information-header)))
          (insert mail-header-separator)
          (forward-line 1)
          (message-set-auto-save-file-name))))
    (gnus-backlog-remove-article group narticle)
    (when (and ga
               (ignore-errors (setq ga (car (read-from-string ga)))))
      (setq gnus-newsgroup-name
            (if (equal (car ga) "") nil (car ga)))
      (gnus-configure-posting-styles)
      (setq gnus-message-group-art (cons gnus-newsgroup-name (cadr ga)))
      (setq message-post-method
            `(lambda (arg)
               (gnus-post-method arg ,(car ga))))
      (unless (equal (cadr ga) "")
        (dolist (article (cdr ga))
          (message-add-action
           `(progn
              (gnus-add-mark ,(car ga) 'replied ,article)
              (gnus-request-set-mark ,(car ga) (list (list (list ,article)
                                                           'add '(reply)))))
           'send))))
    (run-hooks 'gnus-draft-setup-hook)))

(defun gnus-draft-article-sendable-p (article)
  "Say whether ARTICLE is sendable."
  (not (memq article gnus-newsgroup-unsendable)))

(defun gnus-draft-check-draft-articles (articles)
  "Check whether the draft articles ARTICLES are under edit."
  (when (equal gnus-newsgroup-name "nndraft:drafts")
    (let ((buffers (buffer-list))
	  file buffs buff)
      (save-current-buffer
	(while (and articles
		    (not buff))
	  (setq file (nndraft-article-filename (pop articles))
		buffs buffers)
	  (while buffs
	    (set-buffer (setq buff (pop buffs)))
	    (if (and buffer-file-name
		     (equal (file-remote-p file)
			    (file-remote-p buffer-file-name))
		     (string-equal (file-truename buffer-file-name)
				   (file-truename file))
		     (buffer-modified-p))
		(setq buffs nil)
	      (setq buff nil)))))
      (when buff
	(let* ((window (get-buffer-window buff t))
	       (frame (and window (window-frame window))))
	  (if frame
	      (gnus-select-frame-set-input-focus frame)
	    (pop-to-buffer buff t)))
	(error "The draft %s is under edit" file)))))

(defun gnus-draft-clear-marks ()
  (setq gnus-newsgroup-reads nil
	gnus-newsgroup-marked nil
	gnus-newsgroup-unreads (nndraft-articles)))

(provide 'gnus-draft)

;;; gnus-draft.el ends here
