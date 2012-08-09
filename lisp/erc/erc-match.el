;;; erc-match.el --- Highlight messages matching certain regexps

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

;; Author: Andreas Fuchs <asf@void.at>
;; Keywords: comm, faces
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcMatch

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

;; This file includes stuff to work with pattern matching in ERC. If
;; you were used to customizing erc-fools, erc-keywords, erc-pals,
;; erc-dangerous-hosts and the like, this file contains these
;; customizable variables.

;; Usage:
;; Put (erc-match-mode 1) into your ~/.emacs file.

;;; Code:

(require 'erc)
(eval-when-compile (require 'cl))

;; Customization:

(defgroup erc-match nil
  "Keyword and Friend/Foe/... recognition.
Group containing all things concerning pattern matching in ERC
messages."
  :group 'erc)

;;;###autoload (autoload 'erc-match-mode "erc-match")
(define-erc-module match nil
  "This mode checks whether messages match certain patterns.  If so,
they are hidden or highlighted.  This is controlled via the variables
`erc-pals', `erc-fools', `erc-keywords', `erc-dangerous-hosts', and
`erc-current-nick-highlight-type'.  For all these highlighting types,
you can decide whether the entire message or only the sending nick is
highlighted."
  ((add-hook 'erc-insert-modify-hook 'erc-match-message 'append))
  ((remove-hook 'erc-insert-modify-hook 'erc-match-message)))

;; Remaining customizations

(defcustom erc-pals nil
  "List of pals on IRC."
  :group 'erc-match
  :type '(repeat regexp))

(defcustom erc-fools nil
  "List of fools on IRC."
  :group 'erc-match
  :type '(repeat regexp))

(defcustom erc-keywords nil
  "List of keywords to highlight in all incoming messages.
Each entry in the list is either a regexp, or a cons cell with the
regexp in the car and the face to use in the cdr.  If no face is
specified, `erc-keyword-face' is used."
  :group 'erc-match
  :type '(repeat (choice regexp
			 (list regexp face))))

(defcustom erc-dangerous-hosts nil
  "List of regexps for hosts to highlight.
Useful to mark nicks from dangerous hosts."
  :group 'erc-match
  :type '(repeat regexp))

(defcustom erc-current-nick-highlight-type 'keyword
  "*Determines how to highlight text in which your current nickname appears
\(does not apply to text sent by you\).

The following values are allowed:

 nil              - do not highlight the message at all
 'keyword         - highlight all instances of current nickname in message
 'nick            - highlight the nick of the user who typed your nickname
 'nick-or-keyword - highlight the nick of the user who typed your nickname,
                    or all instances of the current nickname if there was
                    no sending user
 'all             - highlight the entire message where current nickname occurs

Any other value disables highlighting of current nickname altogether."
  :group 'erc-match
  :type '(choice (const nil)
		 (const nick)
		 (const keyword)
		 (const nick-or-keyword)
		 (const all)))

(defcustom erc-pal-highlight-type 'nick
  "*Determines how to highlight messages by pals.
See `erc-pals'.

The following values are allowed:

    nil   - do not highlight the message at all
    'nick - highlight pal's nickname only
    'all  - highlight the entire message from pal

Any other value disables pal highlighting altogether."
  :group 'erc-match
  :type '(choice (const nil)
		 (const nick)
		 (const all)))

(defcustom erc-fool-highlight-type 'nick
  "*Determines how to highlight messages by fools.
See `erc-fools'.

The following values are allowed:

    nil   - do not highlight the message at all
    'nick - highlight fool's nickname only
    'all  - highlight the entire message from fool

Any other value disables fool highlighting altogether."
  :group 'erc-match
  :type '(choice (const nil)
		 (const nick)
		 (const all)))

(defcustom erc-keyword-highlight-type 'keyword
  "*Determines how to highlight messages containing keywords.
See variable `erc-keywords'.

The following values are allowed:

    'keyword - highlight keyword only
    'all     - highlight the entire message containing keyword

Any other value disables keyword highlighting altogether."
  :group 'erc-match
  :type '(choice (const nil)
		 (const keyword)
		 (const all)))

(defcustom erc-dangerous-host-highlight-type 'nick
  "*Determines how to highlight messages by nicks from dangerous-hosts.
See `erc-dangerous-hosts'.

The following values are allowed:

    'nick - highlight nick from dangerous-host only
    'all  - highlight the entire message from dangerous-host

Any other value disables dangerous-host highlighting altogether."
  :group 'erc-match
  :type '(choice (const nil)
		 (const nick)
		 (const all)))


(defcustom erc-log-matches-types-alist '((keyword . "ERC Keywords"))
  "Alist telling ERC where to log which match types.
Valid match type keys are:
- keyword
- pal
- dangerous-host
- fool
- current-nick

The other element of each cons pair in this list is the buffer name to
use for the logged message."
  :group 'erc-match
  :type '(repeat (cons (choice :tag "Key"
			       (const keyword)
			       (const pal)
			       (const dangerous-host)
			       (const fool)
			       (const current-nick))
		       (string :tag "Buffer name"))))

(defcustom erc-log-matches-flag 'away
  "Flag specifying when matched message logging should happen.
When nil, don't log any matched messages.
When t, log messages.
When 'away, log messages only when away."
  :group 'erc-match
  :type '(choice (const nil)
		 (const away)
		 (const t)))

(defcustom erc-log-match-format "%t<%n:%c> %m"
  "Format for matched Messages.
This variable specifies how messages in the corresponding log buffers will
be formatted. The various format specs are:

%t Timestamp (uses `erc-timestamp-format' if non-nil or \"[%Y-%m-%d %H:%M] \")
%n Nickname of sender
%u Nickname!user@host of sender
%c Channel in which this was received
%m Message"
  :group 'erc-match
  :type 'string)

(defcustom erc-beep-match-types '(current-nick)
  "Types of matches to beep for when a match occurs.
The function `erc-beep-on-match' needs to be added to `erc-text-matched-hook'
for beeping to work."
  :group 'erc-match
  :type '(choice (repeat :tag "Beep on match" (choice
					       (const current-nick)
					       (const keyword)
					       (const pal)
					       (const dangerous-host)
					       (const fool)))
		 (const :tag "Don't beep" nil)))

(defcustom erc-text-matched-hook '(erc-log-matches)
  "Hook run when text matches a given match-type.
Functions in this hook are passed as arguments:
\(match-type nick!user@host message) where MATCH-TYPE is a symbol of:
current-nick, keyword, pal, dangerous-host, fool"
  :options '(erc-log-matches erc-hide-fools erc-beep-on-match)
  :group 'erc-match
  :type 'hook)

;; Internal variables:

;; This is exactly the same as erc-button-syntax-table.  Should we
;; just put it in erc.el
(defvar erc-match-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "w" table)
    (modify-syntax-entry ?\) "w" table)
    (modify-syntax-entry ?\[ "w" table)
    (modify-syntax-entry ?\] "w" table)
    (modify-syntax-entry ?\{ "w" table)
    (modify-syntax-entry ?\} "w" table)
    (modify-syntax-entry ?` "w" table)
    (modify-syntax-entry ?' "w" table)
    (modify-syntax-entry ?^ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?| "w" table)
    (modify-syntax-entry ?\\ "w" table)
    table)
  "Syntax table used when highlighting messages.
This syntax table should make all the valid nick characters word
constituents.")

;; Faces:

(defface erc-current-nick-face '((t (:bold t :foreground "DarkTurquoise")))
  "ERC face for occurrences of your current nickname."
  :group 'erc-faces)

(defface erc-dangerous-host-face '((t (:foreground "red")))
  "ERC face for people on dangerous hosts.
See `erc-dangerous-hosts'."
  :group 'erc-faces)

(defface erc-pal-face '((t (:bold t :foreground "Magenta")))
  "ERC face for your pals.
See `erc-pals'."
  :group 'erc-faces)

(defface erc-fool-face '((t (:foreground "dim gray")))
  "ERC face for fools on the channel.
See `erc-fools'."
  :group 'erc-faces)

(defface erc-keyword-face '((t (:bold t :foreground "pale green")))
  "ERC face for your keywords.
Note that this is the default face to use if
`erc-keywords' does not specify another."
  :group 'erc-faces)

;; Functions:

(defun erc-add-entry-to-list (list prompt &optional completions)
  "Add an entry interactively to a list.
LIST must be passed as a symbol
The query happens using PROMPT.
Completion is performed on the optional alist COMPLETIONS."
  (let ((entry (completing-read
		prompt
		completions
		(lambda (x)
		  (not (erc-member-ignore-case (car x) (symbol-value list)))))))
    (if (erc-member-ignore-case entry (symbol-value list))
	(error "\"%s\" is already on the list" entry)
      (set list (cons entry (symbol-value list))))))

(defun erc-remove-entry-from-list (list prompt)
  "Remove an entry interactively from a list.
LIST must be passed as a symbol.
The elements of LIST can be strings, or cons cells where the
car is the string."
  (let* ((alist (mapcar (lambda (x)
			  (if (listp x)
			      x
			    (list x)))
			(symbol-value list)))
	 (entry (completing-read
		 prompt
		 alist
		 nil
		 t)))
    (if (erc-member-ignore-case entry (symbol-value list))
	;; plain string
	(set list (delete entry (symbol-value list)))
      ;; cons cell
      (set list (delete (assoc entry (symbol-value list))
			(symbol-value list))))))

;;;###autoload
(defun erc-add-pal ()
  "Add pal interactively to `erc-pals'."
  (interactive)
  (erc-add-entry-to-list 'erc-pals "Add pal: " (erc-get-server-nickname-alist)))

;;;###autoload
(defun erc-delete-pal ()
  "Delete pal interactively to `erc-pals'."
  (interactive)
  (erc-remove-entry-from-list 'erc-pals "Delete pal: "))

;;;###autoload
(defun erc-add-fool ()
  "Add fool interactively to `erc-fools'."
  (interactive)
  (erc-add-entry-to-list 'erc-fools "Add fool: "
			 (erc-get-server-nickname-alist)))

;;;###autoload
(defun erc-delete-fool ()
  "Delete fool interactively to `erc-fools'."
  (interactive)
  (erc-remove-entry-from-list 'erc-fools "Delete fool: "))

;;;###autoload
(defun erc-add-keyword ()
  "Add keyword interactively to `erc-keywords'."
  (interactive)
  (erc-add-entry-to-list 'erc-keywords "Add keyword: "))

;;;###autoload
(defun erc-delete-keyword ()
  "Delete keyword interactively to `erc-keywords'."
  (interactive)
  (erc-remove-entry-from-list 'erc-keywords "Delete keyword: "))

;;;###autoload
(defun erc-add-dangerous-host ()
  "Add dangerous-host interactively to `erc-dangerous-hosts'."
  (interactive)
  (erc-add-entry-to-list 'erc-dangerous-hosts "Add dangerous-host: "))

;;;###autoload
(defun erc-delete-dangerous-host ()
  "Delete dangerous-host interactively to `erc-dangerous-hosts'."
  (interactive)
  (erc-remove-entry-from-list 'erc-dangerous-hosts "Delete dangerous-host: "))

(defun erc-match-current-nick-p (nickuserhost msg)
  "Check whether the current nickname is in MSG.
NICKUSERHOST will be ignored."
  (with-syntax-table erc-match-syntax-table
    (and msg
	 (string-match (concat "\\b"
			       (regexp-quote (erc-current-nick))
			       "\\b")
		       msg))))

(defun erc-match-pal-p (nickuserhost msg)
  "Check whether NICKUSERHOST is in `erc-pals'.
MSG will be ignored."
  (and nickuserhost
       (erc-list-match erc-pals nickuserhost)))

(defun erc-match-fool-p (nickuserhost msg)
  "Check whether NICKUSERHOST is in `erc-fools' or MSG is directed at a fool."
  (and msg nickuserhost
       (or (erc-list-match erc-fools nickuserhost)
	   (erc-match-directed-at-fool-p msg))))

(defun erc-match-keyword-p (nickuserhost msg)
  "Check whether any keyword of `erc-keywords' matches for MSG.
NICKUSERHOST will be ignored."
  (and msg
       (erc-list-match
	(mapcar (lambda (x)
		  (if (listp x)
		      (car x)
		    x))
		erc-keywords)
	msg)))

(defun erc-match-dangerous-host-p (nickuserhost msg)
  "Check whether NICKUSERHOST is in `erc-dangerous-hosts'.
MSG will be ignored."
  (and nickuserhost
       (erc-list-match erc-dangerous-hosts nickuserhost)))

(defun erc-match-directed-at-fool-p (msg)
  "Check whether MSG is directed at a fool.
In order to do this, every entry in `erc-fools' will be used.
In any of the following situations, MSG is directed at an entry FOOL:

- MSG starts with \"FOOL: \" or \"FOO, \"
- MSG contains \", FOOL.\" (actually, \"\\s. FOOL\\s.\")"
  (let ((fools-beg (mapcar (lambda (entry)
				 (concat "^" entry "[:,] "))
			   erc-fools))
	(fools-end (mapcar (lambda (entry)
				 (concat "\\s. " entry "\\s."))
			       erc-fools)))
    (or (erc-list-match fools-beg msg)
	(erc-list-match fools-end msg))))

(defun erc-match-message ()
  "Mark certain keywords in a region.
Use this defun with `erc-insert-modify-hook'."
  ;; This needs some refactoring.
  (goto-char (point-min))
  (let* ((to-match-nick-dep '("pal" "fool" "dangerous-host"))
	 (to-match-nick-indep '("keyword" "current-nick"))
	 (vector (erc-get-parsed-vector (point-min)))
	 (nickuserhost (erc-get-parsed-vector-nick vector))
	 (nickname (and nickuserhost
			(nth 0 (erc-parse-user nickuserhost))))
	 (old-pt (point))
	 (nick-beg (and nickname
			(re-search-forward (regexp-quote nickname)
					   (point-max) t)
			(match-beginning 0)))
	 (nick-end (when nick-beg
		     (match-end 0)))
	 (message (buffer-substring (if (and nick-end
					     (<= (+ 2 nick-end) (point-max)))
					(+ 2 nick-end)
				      (point-min))
				    (point-max))))
    (when vector
      (mapc
       (lambda (match-type)
	 (goto-char (point-min))
	 (let* ((match-prefix (concat "erc-" match-type))
		(match-pred (intern (concat "erc-match-" match-type "-p")))
		(match-htype (eval (intern (concat match-prefix
						   "-highlight-type"))))
		(match-regex (if (string= match-type "current-nick")
				 (regexp-quote (erc-current-nick))
			       (eval (intern (concat match-prefix "s")))))
		(match-face (intern (concat match-prefix "-face"))))
	   (when (funcall match-pred nickuserhost message)
	     (cond
	      ;; Highlight the nick of the message
	      ((and (eq match-htype 'nick)
		    nick-end)
	       (erc-put-text-property
		nick-beg nick-end
		'face match-face (current-buffer)))
	      ;; Highlight the nick of the message, or the current
	      ;; nick if there's no nick in the message (e.g. /NAMES
	      ;; output)
	      ((and (string= match-type "current-nick")
		    (eq match-htype 'nick-or-keyword))
	       (if nick-end
		   (erc-put-text-property
		    nick-beg nick-end
		    'face match-face (current-buffer))
		 (goto-char (+ 2 (or nick-end
				     (point-min))))
		 (while (re-search-forward match-regex nil t)
		   (erc-put-text-property (match-beginning 0) (match-end 0)
					  'face match-face))))
	      ;; Highlight the whole message
	      ((eq match-htype 'all)
	       (erc-put-text-property
		(point-min) (point-max)
		'face match-face (current-buffer)))
	      ;; Highlight all occurrences of the word to be
	      ;; highlighted.
	      ((and (string= match-type "keyword")
		    (eq match-htype 'keyword))
	       (mapc (lambda (elt)
		       (let ((regex elt)
			     (face match-face))
			 (when (consp regex)
			   (setq regex (car elt)
				 face (cdr elt)))
			 (goto-char (+ 2 (or nick-end
					     (point-min))))
			 (while (re-search-forward regex nil t)
			   (erc-put-text-property
			    (match-beginning 0) (match-end 0)
			    'face face))))
		     match-regex))
	      ;; Highlight all occurrences of our nick.
	      ((and (string= match-type "current-nick")
		    (eq match-htype 'keyword))
	       (goto-char (+ 2 (or nick-end
				   (point-min))))
	       (while (re-search-forward match-regex nil t)
		 (erc-put-text-property (match-beginning 0) (match-end 0)
					'face match-face)))
	      ;; Else twiddle your thumbs.
	      (t nil))
	     (run-hook-with-args
	      'erc-text-matched-hook
	      (intern match-type)
	      (or nickuserhost
		  (concat "Server:" (erc-get-parsed-vector-type vector)))
	      message))))
       (if nickuserhost
	   (append to-match-nick-dep to-match-nick-indep)
	 to-match-nick-indep)))))

(defun erc-log-matches (match-type nickuserhost message)
  "Log matches in a separate buffer, determined by MATCH-TYPE.
The behavior of this function is controlled by the variables
`erc-log-matches-types-alist' and `erc-log-matches-flag'.
Specify the match types which should be logged in the former,
and deactivate/activate match logging in the latter.
See `erc-log-match-format'."
  (let  ((match-buffer-name (cdr (assq match-type
				       erc-log-matches-types-alist)))
	 (nick (nth 0 (erc-parse-user nickuserhost))))
    (when (and
	   (or (eq erc-log-matches-flag t)
	       (and (eq erc-log-matches-flag 'away)
		    (erc-away-time)))
	   match-buffer-name)
      (let ((line (format-spec erc-log-match-format
		   (format-spec-make
		    ?n nick
		    ?t (format-time-string
			(or (and (boundp 'erc-timestamp-format)
				 erc-timestamp-format)
			    "[%Y-%m-%d %H:%M] "))
		    ?c (or (erc-default-target) "")
		    ?m message
		    ?u nickuserhost))))
	(with-current-buffer (erc-log-matches-make-buffer match-buffer-name)
	  (let ((inhibit-read-only t))
	    (goto-char (point-max))
	    (insert line)))))))

(defun erc-log-matches-make-buffer (name)
  "Create or get a log-matches buffer named NAME and return it."
  (let* ((buffer-already (get-buffer name))
	 (buffer (or buffer-already
		     (get-buffer-create name))))
    (with-current-buffer buffer
      (unless buffer-already
	(insert " == Type \"q\" to dismiss messages ==\n")
	(erc-view-mode-enter nil (lambda (buffer)
				   (when (y-or-n-p "Discard messages? ")
				     (kill-buffer buffer)))))
      buffer)))

(defun erc-log-matches-come-back (proc parsed)
  "Display a notice that messages were logged while away."
  (when (and (erc-away-time)
	     (eq erc-log-matches-flag 'away))
    (mapc
     (lambda (match-type)
       (let ((buffer (get-buffer (cdr match-type)))
	     (buffer-name (cdr match-type)))
	 (when buffer
	   (let* ((last-msg-time (erc-emacs-time-to-erc-time
				  (with-current-buffer buffer
				    (get-text-property (1- (point-max))
						       'timestamp))))
		  (away-time (erc-emacs-time-to-erc-time (erc-away-time))))
	     (when (and away-time last-msg-time
			(erc-time-gt last-msg-time away-time))
	       (erc-display-message
		nil 'notice 'active
		(format "You have logged messages waiting in \"%s\"."
			buffer-name))
	       (erc-display-message
		nil 'notice 'active
		(format "Type \"C-c C-k %s RET\" to view them."
			buffer-name)))))))
     erc-log-matches-types-alist))
  nil)

; This handler must be run _before_ erc-process-away is.
(add-hook 'erc-server-305-functions 'erc-log-matches-come-back nil)

(defun erc-go-to-log-matches-buffer ()
  "Interactively open an erc-log-matches buffer."
  (interactive)
  (let ((buffer-name (completing-read "Switch to ERC Log buffer: "
				      (mapcar (lambda (x)
						(cons (cdr x) t))
					      erc-log-matches-types-alist)
				      (lambda (buffer-cons)
					(get-buffer (car buffer-cons))))))
    (switch-to-buffer buffer-name)))

(define-key erc-mode-map "\C-c\C-k" 'erc-go-to-log-matches-buffer)

(defun erc-hide-fools (match-type nickuserhost message)
 "Hide foolish comments.
This function should be called from `erc-text-matched-hook'."
 (when (eq match-type 'fool)
   (erc-put-text-properties (point-min) (point-max)
			    '(invisible intangible)
			    (current-buffer))))

(defun erc-beep-on-match (match-type nickuserhost message)
  "Beep when text matches.
This function is meant to be called from `erc-text-matched-hook'."
  (when (member match-type erc-beep-match-types)
    (beep)))

(provide 'erc-match)

;;; erc-match.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:
