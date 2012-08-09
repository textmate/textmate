;;; erc-backend.el --- Backend network communication for ERC

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Filename: erc-backend.el
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2004-05-7
;; Keywords: IRC chat client internet

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

;; This file defines backend network communication handlers for ERC.
;;
;; How things work:
;;
;; You define a new handler with `define-erc-response-handler'.  This
;; defines a function, a corresponding hook variable, and populates a
;; global hash table `erc-server-responses' with a map from response
;; to hook variable.  See the function documentation for more
;; information.
;;
;; Upon receiving a line from the server, `erc-parse-server-response'
;; is called on it.
;;
;; A line generally looks like:
;;
;; LINE := ':' SENDER ' ' COMMAND ' ' (COMMAND-ARGS ' ')* ':' CONTENTS
;; SENDER := Not ':' | ' '
;; COMMAND := Not ':' | ' '
;; COMMAND-ARGS := Not ':' | ' '
;;
;; This gets parsed and stuffed into an `erc-response' struct.  You
;; can access the fields of the struct with:
;;
;; COMMAND --- `erc-response.command'
;; COMMAND-ARGS --- `erc-response.command-args'
;; CONTENTS --- `erc-response.contents'
;; SENDER --- `erc-response.sender'
;; LINE --- `erc-response.unparsed'
;;
;; WARNING, WARNING!!
;; It's probably not a good idea to destructively modify the list
;; of command-args in your handlers, since other functions down the
;; line may well need to access the arguments too.
;;
;; That is, unless you're /absolutely/ sure that your handler doesn't
;; invoke some other function that needs to use COMMAND-ARGS, don't do
;; something like
;;
;; (while (erc-response.command-args parsed)
;;   (let ((a (pop (erc-response.command-args parsed))))
;;     ...))
;;
;; The parsed response is handed over to
;; `erc-handle-parsed-server-response', which checks whether it should
;; carry out duplicate suppression, and then runs `erc-call-hooks'.
;; `erc-call-hooks' retrieves the relevant hook variable from
;; `erc-server-responses' and runs it.
;;
;; Most handlers then destructure the parsed response in some way
;; (depending on what the handler is, the arguments have different
;; meanings), and generally display something, usually using
;; `erc-display-message'.

;;; TODO:

;; o Generalize the display-line code so that we can use it to
;;   display the stuff we send, as well as the stuff we receive.
;;   Then, move all display-related code into another backend-like
;;   file, erc-display.el, say.
;;
;; o Clean up the handlers using new display code (has to be written
;;   first).

;;; History:

;; 2004/05/10 -- Handler bodies taken out of erc.el and ported to new
;;               interface.

;; 2005-08-13 -- Moved sending commands from erc.el.

;;; Code:

(require 'erc-compat)
(eval-when-compile (require 'cl))
(autoload 'erc-with-buffer "erc" nil nil 'macro)
(autoload 'erc-log "erc" nil nil 'macro)

;;;; Variables and options

(defvar erc-server-responses (make-hash-table :test #'equal)
  "Hashtable mapping server responses to their handler hooks.")

(defstruct (erc-response (:conc-name erc-response.))
  (unparsed "" :type string)
  (sender "" :type string)
  (command "" :type string)
  (command-args '() :type list)
  (contents "" :type string))

;;; User data

(defvar erc-server-current-nick nil
  "Nickname on the current server.
Use `erc-current-nick' to access this.")
(make-variable-buffer-local 'erc-server-current-nick)

;;; Server attributes

(defvar erc-server-process nil
  "The process object of the corresponding server connection.")
(make-variable-buffer-local 'erc-server-process)

(defvar erc-session-server nil
  "The server name used to connect to for this session.")
(make-variable-buffer-local 'erc-session-server)

(defvar erc-session-connector nil
  "The function used to connect to this session (nil for the default).")
(make-variable-buffer-local 'erc-session-connector)

(defvar erc-session-port nil
  "The port used to connect to.")
(make-variable-buffer-local 'erc-session-port)

(defvar erc-server-announced-name nil
  "The name the server announced to use.")
(make-variable-buffer-local 'erc-server-announced-name)

(defvar erc-server-version nil
  "The name and version of the server's ircd.")
(make-variable-buffer-local 'erc-server-version)

(defvar erc-server-parameters nil
  "Alist listing the supported server parameters.

This is only set if the server sends 005 messages saying what is
supported on the server.

Entries are of the form:
  (PARAMETER . VALUE)
or
  (PARAMETER) if no value is provided.

Some examples of possible parameters sent by servers:
CHANMODES=b,k,l,imnpst - list of supported channel modes
CHANNELLEN=50 - maximum length of channel names
CHANTYPES=#&!+ - supported channel prefixes
CHARMAPPING=rfc1459 - character mapping used for nickname and channels
KICKLEN=160 - maximum allowed kick message length
MAXBANS=30 - maximum number of bans per channel
MAXCHANNELS=10 - maximum number of channels allowed to join
NETWORK=EFnet -  the network identifier
NICKLEN=9 - maximum allowed length of nicknames
PREFIX=(ov)@+ - list of channel modes and the user prefixes if user has mode
RFC2812 - server supports RFC 2812 features
SILENCE=10 - supports the SILENCE command, maximum allowed number of entries
TOPICLEN=160 - maximum allowed topic length
WALLCHOPS - supports sending messages to all operators in a channel")
(make-variable-buffer-local 'erc-server-parameters)

;;; Server and connection state

(defvar erc-server-ping-timer-alist nil
  "Mapping of server buffers to their specific ping timer.")

(defvar erc-server-connected nil
  "Non-nil if the current buffer has been used by ERC to establish
an IRC connection.

If you wish to determine whether an IRC connection is currently
active, use the `erc-server-process-alive' function instead.")
(make-variable-buffer-local 'erc-server-connected)

(defvar erc-server-reconnect-count 0
  "Number of times we have failed to reconnect to the current server.")
(make-variable-buffer-local 'erc-server-reconnect-count)

(defvar erc-server-quitting nil
  "Non-nil if the user requests a quit.")
(make-variable-buffer-local 'erc-server-quitting)

(defvar erc-server-reconnecting nil
  "Non-nil if the user requests an explicit reconnect, and the
current IRC process is still alive.")
(make-variable-buffer-local 'erc-server-reconnecting)

(defvar erc-server-timed-out nil
  "Non-nil if the IRC server failed to respond to a ping.")
(make-variable-buffer-local 'erc-server-timed-out)

(defvar erc-server-banned nil
  "Non-nil if the user is denied access because of a server ban.")
(make-variable-buffer-local 'erc-server-banned)

(defvar erc-server-error-occurred nil
  "Non-nil if the user triggers some server error.")
(make-variable-buffer-local 'erc-server-error-occurred)

(defvar erc-server-lines-sent nil
  "Line counter.")
(make-variable-buffer-local 'erc-server-lines-sent)

(defvar erc-server-last-peers '(nil . nil)
  "Last peers used, both sender and receiver.
Those are used for /MSG destination shortcuts.")
(make-variable-buffer-local 'erc-server-last-peers)

(defvar erc-server-last-sent-time nil
  "Time the message was sent.
This is useful for flood protection.")
(make-variable-buffer-local 'erc-server-last-sent-time)

(defvar erc-server-last-ping-time nil
  "Time the last ping was sent.
This is useful for flood protection.")
(make-variable-buffer-local 'erc-server-last-ping-time)

(defvar erc-server-last-received-time nil
  "Time the last message was received from the server.
This is useful for detecting hung connections.")
(make-variable-buffer-local 'erc-server-last-received-time)

(defvar erc-server-lag nil
  "Calculated server lag time in seconds.
This variable is only set in a server buffer.")
(make-variable-buffer-local 'erc-server-lag)

(defvar erc-server-filter-data nil
  "The data that arrived from the server
but has not been processed yet.")
(make-variable-buffer-local 'erc-server-filter-data)

(defvar erc-server-duplicates (make-hash-table :test 'equal)
  "Internal variable used to track duplicate messages.")
(make-variable-buffer-local 'erc-server-duplicates)

;; From Circe
(defvar erc-server-processing-p nil
  "Non-nil when we're currently processing a message.

When ERC receives a private message, it sets up a new buffer for
this query.  These in turn, though, do start flyspell. This
involves starting an external process, in which case Emacs will
wait - and when it waits, it does accept other stuff from, say,
network exceptions.  So, if someone sends you two messages
quickly after each other, ispell is started for the first, but
might take long enough for the second message to be processed
first.")
(make-variable-buffer-local 'erc-server-processing-p)

(defvar erc-server-flood-last-message 0
  "When we sent the last message.
See `erc-server-flood-margin' for an explanation of the flood
protection algorithm.")
(make-variable-buffer-local 'erc-server-flood-last-message)

(defvar erc-server-flood-queue nil
  "The queue of messages waiting to be sent to the server.
See `erc-server-flood-margin' for an explanation of the flood
protection algorithm.")
(make-variable-buffer-local 'erc-server-flood-queue)

(defvar erc-server-flood-timer nil
  "The timer to resume sending.")
(make-variable-buffer-local 'erc-server-flood-timer)

;;; IRC protocol and misc options

(defgroup erc-server nil
  "Parameters for dealing with IRC servers."
  :group 'erc)

(defcustom erc-server-auto-reconnect t
  "Non-nil means that ERC will attempt to reestablish broken connections.

Reconnection will happen automatically for any unexpected disconnection."
  :group 'erc-server
  :type 'boolean)

(defcustom erc-server-reconnect-attempts 2
  "The number of times that ERC will attempt to reestablish a
broken connection, or t to always attempt to reconnect.

This only has an effect if `erc-server-auto-reconnect' is non-nil."
  :group 'erc-server
  :type '(choice (const :tag "Always reconnect" t)
                 integer))

(defcustom erc-server-reconnect-timeout 1
  "The amount of time, in seconds, that ERC will wait between
successive reconnect attempts.

If a key is pressed while ERC is waiting, it will stop waiting."
  :group 'erc-server
  :type 'number)

(defcustom erc-split-line-length 440
  "*The maximum length of a single message.
If a message exceeds this size, it is broken into multiple ones.

IRC allows for lines up to 512 bytes. Two of them are CR LF.
And a typical message looks like this:

  :nicky!uhuser@host212223.dialin.fnordisp.net PRIVMSG #lazybastards :Hello!

You can limit here the maximum length of the \"Hello!\" part.
Good luck."
  :type 'integer
  :group 'erc-server)

(defcustom erc-coding-system-precedence '(utf-8 undecided)
  "List of coding systems to be preferred when receiving a string from the server.
This will only be consulted if the coding system in
`erc-server-coding-system' is `undecided'."
  :group 'erc-server
  :version "24.1"
  :type '(repeat coding-system))

(defcustom erc-server-coding-system (if (and (fboundp 'coding-system-p)
                                             (coding-system-p 'undecided)
                                             (coding-system-p 'utf-8))
                                        '(utf-8 . undecided)
                                      nil)
  "The default coding system for incoming and outgoing text.
This is either a coding system, a cons, a function, or nil.

If a cons, the encoding system for outgoing text is in the car
and the decoding system for incoming text is in the cdr. The most
interesting use for this is to put `undecided' in the cdr. This
means that `erc-coding-system-precedence' will be consulted, and the
first match there will be used.

If a function, it is called with the argument `target' and should
return a coding system or a cons as described above.

If you need to send non-ASCII text to people not using a client that
does decoding on its own, you must tell ERC what encoding to use.
Emacs cannot guess it, since it does not know what the people on the
other end of the line are using."
  :group 'erc-server
  :type '(choice (const :tag "None" nil)
                 coding-system
                 (cons (coding-system :tag "encoding" :value utf-8)
                       (coding-system :tag "decoding" :value undecided))
                 function))

(defcustom erc-encoding-coding-alist nil
  "Alist of target regexp and coding-system pairs to use.
This overrides `erc-server-coding-system' depending on the
current target as returned by `erc-default-target'.

Example: If you know that the channel #linux-ru uses the coding-system
`cyrillic-koi8', then add '(\"#linux-ru\" . cyrillic-koi8) to the
alist."
  :group 'erc-server
  :type '(repeat (cons (string :tag "Target")
                       coding-system)))

(defcustom erc-server-connect-function 'open-network-stream
  "Function used to initiate a connection.
It should take same arguments as `open-network-stream' does."
  :group 'erc-server
  :type 'function)

(defcustom erc-server-prevent-duplicates '("301")
  "*Either nil or a list of strings.
Each string is a IRC message type, like PRIVMSG or NOTICE.
All Message types in that list of subjected to duplicate prevention."
  :type '(choice (const nil) (list string))
  :group 'erc-server)

(defcustom erc-server-duplicate-timeout 60
  "*The time allowed in seconds between duplicate messages.

If two identical messages arrive within this value of one another, the second
isn't displayed."
  :type 'integer
  :group 'erc-server)

;;; Flood-related

;; Most of this is courtesy of Jorgen Schaefer and Circe
;; (http://www.nongnu.org/circe)

(defcustom erc-server-flood-margin 10
  "*A margin on how much excess data we send.
The flood protection algorithm of ERC works like the one
detailed in RFC 2813, section 5.8 \"Flood control of clients\".

  * If `erc-server-flood-last-message' is less than the current
    time, set it equal.
  * While `erc-server-flood-last-message' is less than
    `erc-server-flood-margin' seconds ahead of the current
    time, send a message, and increase
    `erc-server-flood-last-message' by
    `erc-server-flood-penalty' for each message."
  :type 'integer
  :group 'erc-server)

(defcustom erc-server-flood-penalty 3
  "How much we penalize a message.
See `erc-server-flood-margin' for an explanation of the flood
protection algorithm."
  :type 'integer
  :group 'erc-server)

;; Ping handling

(defcustom erc-server-send-ping-interval 30
  "*Interval of sending pings to the server, in seconds.
If this is set to nil, pinging the server is disabled."
  :group 'erc-server
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom erc-server-send-ping-timeout 120
  "*If the time between ping and response is greater than this, reconnect.
The time is in seconds.

This must be greater than or equal to the value for
`erc-server-send-ping-interval'.

If this is set to nil, never try to reconnect."
  :group 'erc-server
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defvar erc-server-ping-handler nil
  "This variable holds the periodic ping timer.")
(make-variable-buffer-local 'erc-server-ping-handler)

;;;; Helper functions

;; From Circe
(defun erc-split-line (longline)
  "Return a list of lines which are not too long for IRC.
The length is specified in `erc-split-line-length'.

Currently this is called by `erc-send-input'."
  (if (< (length longline)
         erc-split-line-length)
      (list longline)
    (with-temp-buffer
      (insert longline)
      (let ((fill-column erc-split-line-length))
        (fill-region (point-min) (point-max)
                     nil t))
      (split-string (buffer-string) "\n"))))

;; Used by CTCP functions
(defun erc-upcase-first-word (str)
  "Upcase the first word in STR."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (upcase-word 1)
    (buffer-string)))

(defun erc-server-setup-periodical-ping (buffer)
  "Set up a timer to periodically ping the current server.
The current buffer is given by BUFFER."
  (with-current-buffer buffer
    (and erc-server-ping-handler (erc-cancel-timer erc-server-ping-handler))
    (when erc-server-send-ping-interval
      (setq erc-server-ping-handler (run-with-timer
                                     4 erc-server-send-ping-interval
                                     #'erc-server-send-ping
                                     buffer))
      (setq erc-server-ping-timer-alist (cons (cons buffer
                                                    erc-server-ping-handler)
                                              erc-server-ping-timer-alist)))))

(defun erc-server-process-alive ()
  "Return non-nil when `erc-server-process' is open or running."
  (and erc-server-process
       (processp erc-server-process)
       (memq (process-status erc-server-process) '(run open))))

;;;; Connecting to a server

(defun erc-server-connect (server port buffer)
  "Perform the connection and login using the specified SERVER and PORT.
We will store server variables in the buffer given by BUFFER."
  (let ((msg (erc-format-message 'connect ?S server ?p port)))
    (message "%s" msg)
    (let ((process (funcall erc-server-connect-function
                            (format "erc-%s-%s" server port)
                            nil server port)))
      (unless (processp process)
        (error "Connection attempt failed"))
      (message "%s...done" msg)
      ;; Misc server variables
      (with-current-buffer buffer
        (setq erc-server-process process)
        (setq erc-server-quitting nil)
        (setq erc-server-reconnecting nil)
        (setq erc-server-timed-out nil)
        (setq erc-server-banned nil)
        (setq erc-server-error-occurred nil)
        (let ((time (erc-current-time)))
          (setq erc-server-last-sent-time time)
          (setq erc-server-last-ping-time time)
          (setq erc-server-last-received-time time))
        (setq erc-server-lines-sent 0)
        ;; last peers (sender and receiver)
        (setq erc-server-last-peers '(nil . nil)))
      ;; we do our own encoding and decoding
      (when (fboundp 'set-process-coding-system)
        (set-process-coding-system process 'raw-text))
      ;; process handlers
      (set-process-sentinel process 'erc-process-sentinel)
      (set-process-filter process 'erc-server-filter-function)
      (set-process-buffer process buffer)))
  (erc-log "\n\n\n********************************************\n")
  (message "%s" (erc-format-message
            'login ?n
            (with-current-buffer buffer (erc-current-nick))))
  ;; wait with script loading until we receive a confirmation (first
  ;; MOTD line)
  (if (eq erc-server-connect-function 'open-network-stream-nowait)
      ;; it's a bit unclear otherwise that it's attempting to establish a
      ;; connection
      (erc-display-message nil nil buffer "Opening connection..\n")
    (erc-login)))

(defun erc-server-reconnect ()
"Reestablish the current IRC connection.
Make sure you are in an ERC buffer when running this."
  (let ((buffer (erc-server-buffer)))
    (unless (buffer-live-p buffer)
      (if (eq major-mode 'erc-mode)
          (setq buffer (current-buffer))
        (error "Reconnect must be run from an ERC buffer")))
    (with-current-buffer buffer
      (erc-update-mode-line)
      (erc-set-active-buffer (current-buffer))
      (setq erc-server-last-sent-time 0)
      (setq erc-server-lines-sent 0)
      (let ((erc-server-connect-function (or erc-session-connector
                                             'open-network-stream)))
        (erc-open erc-session-server erc-session-port erc-server-current-nick
                  erc-session-user-full-name t erc-session-password)))))

(defun erc-server-filter-function (process string)
  "The process filter for the ERC server."
  (with-current-buffer (process-buffer process)
    (setq erc-server-last-received-time (erc-current-time))
    ;; If you think this is written in a weird way - please refer to the
    ;; docstring of `erc-server-processing-p'
    (if erc-server-processing-p
        (setq erc-server-filter-data
              (if erc-server-filter-data
                  (concat erc-server-filter-data string)
                string))
      ;; This will be true even if another process is spawned!
      (let ((erc-server-processing-p t))
        (setq erc-server-filter-data (if erc-server-filter-data
                                           (concat erc-server-filter-data
                                                   string)
                                         string))
        (while (and erc-server-filter-data
                    (string-match "[\n\r]+" erc-server-filter-data))
          (let ((line (substring erc-server-filter-data
                                 0 (match-beginning 0))))
            (setq erc-server-filter-data
                  (if (= (match-end 0)
                         (length erc-server-filter-data))
                      nil
                    (substring erc-server-filter-data
                               (match-end 0))))
            (erc-log-irc-protocol line nil)
            (erc-parse-server-response process line)))))))

(defsubst erc-server-reconnect-p (event)
  "Return non-nil if ERC should attempt to reconnect automatically.
EVENT is the message received from the closed connection process."
  (or erc-server-reconnecting
      (and erc-server-auto-reconnect
           (not erc-server-banned)
           (not erc-server-error-occurred)
           ;; make sure we don't infinitely try to reconnect, unless the
           ;; user wants that
           (or (eq erc-server-reconnect-attempts t)
               (and (integerp erc-server-reconnect-attempts)
                    (< erc-server-reconnect-count
                       erc-server-reconnect-attempts)))
           (or erc-server-timed-out
               (not (string-match "^deleted" event)))
           ;; open-network-stream-nowait error for connection refused
           (not (string-match "^failed with code 111" event)))))

(defun erc-process-sentinel-2 (event buffer)
  "Called when `erc-process-sentinel-1' has detected an unexpected disconnect."
  (if (not (buffer-live-p buffer))
      (erc-update-mode-line)
    (with-current-buffer buffer
      (let ((reconnect-p (erc-server-reconnect-p event)))
        (erc-display-message nil 'error (current-buffer)
                             (if reconnect-p 'disconnected
                               'disconnected-noreconnect))
        (if (not reconnect-p)
            ;; terminate, do not reconnect
            (progn
              (erc-display-message nil 'error (current-buffer)
                                   'terminated ?e event)
              ;; Update mode line indicators
              (erc-update-mode-line)
              (set-buffer-modified-p nil))
          ;; reconnect
          (condition-case err
              (progn
                (setq erc-server-reconnecting nil)
                (erc-server-reconnect)
                (setq erc-server-reconnect-count 0))
            (error (when (buffer-live-p buffer)
                     (set-buffer buffer)
                     (if (integerp erc-server-reconnect-attempts)
                         (setq erc-server-reconnect-count
                               (1+ erc-server-reconnect-count))
                       (message "%s ... %s"
                                "Reconnecting until we succeed"
                                "kill the ERC server buffer to stop"))
                     (if (numberp erc-server-reconnect-timeout)
                         (run-at-time erc-server-reconnect-timeout nil
                                      #'erc-process-sentinel-2
                                      event buffer)
                       (error (concat "`erc-server-reconnect-timeout`"
                                      " must be a number")))))))))))

(defun erc-process-sentinel-1 (event buffer)
  "Called when `erc-process-sentinel' has decided that we're disconnecting.
Determine whether user has quit or whether erc has been terminated.
Conditionally try to reconnect and take appropriate action."
  (with-current-buffer buffer
    (if erc-server-quitting
        ;; normal quit
        (progn
          (erc-display-message nil 'error (current-buffer) 'finished)
          ;; Update mode line indicators
          (erc-update-mode-line)
          ;; Kill server buffer if user wants it
          (set-buffer-modified-p nil)
          (when erc-kill-server-buffer-on-quit
            (kill-buffer (current-buffer))))
      ;; unexpected disconnect
      (erc-process-sentinel-2 event buffer))))

(defun erc-process-sentinel (cproc event)
  "Sentinel function for ERC process."
  (let ((buf (process-buffer cproc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (erc-log (format
                  "SENTINEL: proc: %S	 status: %S  event: %S (quitting: %S)"
                  cproc (process-status cproc) event erc-server-quitting))
        (if (string-match "^open" event)
            ;; newly opened connection (no wait)
            (erc-login)
          ;; assume event is 'failed
          (erc-with-all-buffers-of-server cproc nil
                                          (setq erc-server-connected nil))
          (when erc-server-ping-handler
            (progn (erc-cancel-timer erc-server-ping-handler)
                   (setq erc-server-ping-handler nil)))
          (run-hook-with-args 'erc-disconnected-hook
                              (erc-current-nick) (system-name) "")
          ;; Remove the prompt
          (goto-char (or (marker-position erc-input-marker) (point-max)))
          (forward-line 0)
          (erc-remove-text-properties-region (point) (point-max))
          (delete-region (point) (point-max))
          ;; Decide what to do with the buffer
          ;; Restart if disconnected
          (erc-process-sentinel-1 event buf))))))

;;;; Sending messages

(defun erc-coding-system-for-target (target)
  "Return the coding system or cons cell appropriate for TARGET.
This is determined via `erc-encoding-coding-alist' or
`erc-server-coding-system'."
  (unless target (setq target (erc-default-target)))
  (or (when target
        (let ((case-fold-search t))
          (catch 'match
            (dolist (pat erc-encoding-coding-alist)
              (when (string-match (car pat) target)
                (throw 'match (cdr pat)))))))
      (and (functionp erc-server-coding-system)
           (funcall erc-server-coding-system target))
      erc-server-coding-system))

(defun erc-decode-string-from-target (str target)
  "Decode STR as appropriate for TARGET.
This is indicated by `erc-encoding-coding-alist', defaulting to the value of
`erc-server-coding-system'."
  (unless (stringp str)
    (setq str ""))
  (let ((coding (erc-coding-system-for-target target)))
    (when (consp coding)
      (setq coding (cdr coding)))
    (when (eq coding 'undecided)
      (let ((codings (detect-coding-string str))
            (precedence erc-coding-system-precedence))
        (while (and precedence
                    (not (memq (car precedence) codings)))
          (pop precedence))
        (when precedence
          (setq coding (car precedence)))))
    (erc-decode-coding-string str coding)))

;; proposed name, not used by anything yet
(defun erc-send-line (text display-fn)
  "Send TEXT to the current server.  Wrapping and flood control apply.
Use DISPLAY-FN to show the results."
  (mapc (lambda (line)
          (erc-server-send line)
          (funcall display-fn))
        (erc-split-line text)))

;; From Circe, with modifications
(defun erc-server-send (string &optional forcep target)
  "Send STRING to the current server.
If FORCEP is non-nil, no flood protection is done - the string is
sent directly. This might cause the messages to arrive in a wrong
order.

If TARGET is specified, look up encoding information for that
channel in `erc-encoding-coding-alist' or
`erc-server-coding-system'.

See `erc-server-flood-margin' for an explanation of the flood
protection algorithm."
  (erc-log (concat "erc-server-send: " string "(" (buffer-name) ")"))
  (setq erc-server-last-sent-time (erc-current-time))
  (let ((encoding (erc-coding-system-for-target target)))
    (when (consp encoding)
      (setq encoding (car encoding)))
    (if (erc-server-process-alive)
        (erc-with-server-buffer
          (let ((str (concat string "\r\n")))
            (if forcep
                (progn
                  (setq erc-server-flood-last-message
                        (+ erc-server-flood-penalty
                           erc-server-flood-last-message))
                  (erc-log-irc-protocol str 'outbound)
                  (condition-case err
                      (progn
                        ;; Set encoding just before sending the string
                        (when (fboundp 'set-process-coding-system)
                          (set-process-coding-system erc-server-process
                                                     'raw-text encoding))
                        (process-send-string erc-server-process str))
                    ;; See `erc-server-send-queue' for full
                    ;; explanation of why we need this condition-case
                    (error nil)))
              (setq erc-server-flood-queue
                    (append erc-server-flood-queue
                            (list (cons str encoding))))
              (erc-server-send-queue (current-buffer))))
          t)
      (message "ERC: No process running")
      nil)))

(defun erc-server-send-ping (buf)
  "Send a ping to the IRC server buffer in BUF.
Additionally, detect whether the IRC process has hung."
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (if (and erc-server-send-ping-timeout
                 (>
                  (erc-time-diff (erc-current-time)
                                 erc-server-last-received-time)
                  erc-server-send-ping-timeout))
            (progn
              ;; if the process is hung, kill it
              (setq erc-server-timed-out t)
              (delete-process erc-server-process))
          (erc-server-send (format "PING %.0f" (erc-current-time)))))
    ;; remove timer if the server buffer has been killed
    (let ((timer (assq buf erc-server-ping-timer-alist)))
      (when timer
        (erc-cancel-timer (cdr timer))
        (setcdr timer nil)))))

;; From Circe
(defun erc-server-send-queue (buffer)
  "Send messages in `erc-server-flood-queue'.
See `erc-server-flood-margin' for an explanation of the flood
protection algorithm."
  (with-current-buffer buffer
    (let ((now (erc-current-time)))
      (when erc-server-flood-timer
        (erc-cancel-timer erc-server-flood-timer)
        (setq erc-server-flood-timer nil))
      (when (< erc-server-flood-last-message
               now)
        (setq erc-server-flood-last-message now))
      (while (and erc-server-flood-queue
                  (< erc-server-flood-last-message
                     (+ now erc-server-flood-margin)))
        (let ((msg (caar erc-server-flood-queue))
              (encoding (cdar erc-server-flood-queue)))
          (setq erc-server-flood-queue (cdr erc-server-flood-queue)
                erc-server-flood-last-message
                (+ erc-server-flood-last-message
                   erc-server-flood-penalty))
          (erc-log-irc-protocol msg 'outbound)
          (erc-log (concat "erc-server-send-queue: "
                           msg "(" (buffer-name buffer) ")"))
          (when (erc-server-process-alive)
            (condition-case err
                ;; Set encoding just before sending the string
                (progn
                  (when (fboundp 'set-process-coding-system)
                    (set-process-coding-system erc-server-process
                                               'raw-text encoding))
                  (process-send-string erc-server-process msg))
              ;; Sometimes the send can occur while the process is
              ;; being killed, which results in a weird SIGPIPE error.
              ;; Catch this and ignore it.
              (error nil)))))
      (when erc-server-flood-queue
        (setq erc-server-flood-timer
              (run-at-time (+ 0.2 erc-server-flood-penalty)
                           nil #'erc-server-send-queue buffer))))))

(defun erc-message (message-command line &optional force)
  "Send LINE to the server as a privmsg or a notice.
MESSAGE-COMMAND should be either \"PRIVMSG\" or \"NOTICE\".
If the target is \",\", the last person you've got a message from will
be used. If the target is \".\", the last person you've sent a message
to will be used."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\) ?\\(.*\\)" line)
    (let ((tgt (match-string 1 line))
          (s (match-string 2 line)))
      (erc-log (format "cmd: MSG(%s): [%s] %s" message-command tgt s))
      (cond
       ((string= tgt ",")
        (if (car erc-server-last-peers)
            (setq tgt (car erc-server-last-peers))
          (setq tgt nil)))
       ((string= tgt ".")
        (if (cdr erc-server-last-peers)
            (setq tgt (cdr erc-server-last-peers))
          (setq tgt nil))))
      (cond
       (tgt
        (setcdr erc-server-last-peers tgt)
        (erc-server-send (format "%s %s :%s" message-command tgt s)
                         force))
       (t
        (erc-display-message nil 'error (current-buffer) 'no-target))))
    t)
   (t nil)))

;;; CTCP

(defun erc-send-ctcp-message (tgt l &optional force)
  "Send CTCP message L to TGT.

If TGT is nil the message is not sent.
The command must contain neither a prefix nor a trailing `\\n'.

See also `erc-server-send'."
  (let ((l (erc-upcase-first-word l)))
    (cond
     (tgt
      (erc-log (format "erc-send-CTCP-message: [%s] %s" tgt l))
      (erc-server-send (format "PRIVMSG %s :\C-a%s\C-a" tgt l)
                       force)))))

(defun erc-send-ctcp-notice (tgt l &optional force)
  "Send CTCP notice L to TGT.

If TGT is nil the message is not sent.
The command must contain neither a prefix nor a trailing `\\n'.

See also `erc-server-send'."
  (let ((l (erc-upcase-first-word l)))
    (cond
     (tgt
      (erc-log (format "erc-send-CTCP-notice: [%s] %s" tgt l))
      (erc-server-send (format "NOTICE %s :\C-a%s\C-a" tgt l)
                       force)))))

;;;; Handling responses

(defun erc-parse-server-response (proc string)
  "Parse and act upon a complete line from an IRC server.
PROC is the process (connection) from which STRING was received.
PROCs `process-buffer' is `current-buffer' when this function is called."
  (unless (string= string "") ;; Ignore empty strings
    (save-match-data
      (let ((posn (if (eq (aref string 0) ?:)
                      (string-match " " string)
                    0))
            (msg (make-erc-response :unparsed string)))

        (setf (erc-response.sender msg)
              (if (eq posn 0)
                  erc-session-server
                (substring string 1 posn)))

        (setf (erc-response.command msg)
              (let* ((bposn (string-match "[^ \n]" string posn))
                     (eposn (string-match " " string bposn)))
                (setq posn (and eposn
                                (string-match "[^ \n]" string eposn)))
                (substring string bposn eposn)))

        (while (and posn
                    (not (eq (aref string posn) ?:)))
          (push (let* ((bposn posn)
                       (eposn (string-match " " string bposn)))
                  (setq posn (and eposn
                                  (string-match "[^ \n]" string eposn)))
                  (substring string bposn eposn))
                (erc-response.command-args msg)))
        (when posn
      (let ((str (substring string (1+ posn))))
        (push str (erc-response.command-args msg))))

    (setf (erc-response.contents msg)
          (first (erc-response.command-args msg)))

    (setf (erc-response.command-args msg)
          (nreverse (erc-response.command-args msg)))

    (erc-decode-parsed-server-response msg)

    (erc-handle-parsed-server-response proc msg)))))

(defun erc-decode-parsed-server-response (parsed-response)
  "Decode a pre-parsed PARSED-RESPONSE before it can be handled.

If there is a channel name in `erc-response.command-args', decode
`erc-response' according to this channel name and
`erc-encoding-coding-alist', or use `erc-server-coding-system'
for decoding."
  (let ((args (erc-response.command-args parsed-response))
        (decode-target nil)
        (decoded-args ()))
    (dolist (arg args nil)
      (when (string-match "^[#&].*" arg)
        (setq decode-target arg)))
    (when (stringp decode-target)
      (setq decode-target (erc-decode-string-from-target decode-target nil)))
    (setf (erc-response.unparsed parsed-response)
          (erc-decode-string-from-target
           (erc-response.unparsed parsed-response)
           decode-target))
    (setf (erc-response.sender parsed-response)
          (erc-decode-string-from-target
           (erc-response.sender parsed-response)
           decode-target))
    (setf (erc-response.command parsed-response)
          (erc-decode-string-from-target
           (erc-response.command parsed-response)
           decode-target))
    (dolist (arg (nreverse args) nil)
      (push (erc-decode-string-from-target arg decode-target)
            decoded-args))
    (setf (erc-response.command-args parsed-response) decoded-args)
    (setf (erc-response.contents parsed-response)
          (erc-decode-string-from-target
           (erc-response.contents parsed-response)
           decode-target))))

(defun erc-handle-parsed-server-response (process parsed-response)
  "Handle a pre-parsed PARSED-RESPONSE from PROCESS.

Hands off to helper functions via `erc-call-hooks'."
  (if (member (erc-response.command parsed-response)
              erc-server-prevent-duplicates)
      (let ((m (erc-response.unparsed parsed-response)))
        ;; duplicate suppression
        (if (< (or (gethash m erc-server-duplicates) 0)
               (- (erc-current-time) erc-server-duplicate-timeout))
            (erc-call-hooks process parsed-response))
        (puthash m (erc-current-time) erc-server-duplicates))
    ;; Hand off to the relevant handler.
    (erc-call-hooks process parsed-response)))

(defun erc-get-hook (command)
  "Return the hook variable associated with COMMAND.

See also `erc-server-responses'."
  (gethash (format (if (numberp command) "%03i" "%s") command)
           erc-server-responses))

(defun erc-call-hooks (process message)
  "Call hooks associated with MESSAGE in PROCESS.

Finds hooks by looking in the `erc-server-responses' hashtable."
  (let ((hook (or (erc-get-hook (erc-response.command message))
                  'erc-default-server-functions)))
    (run-hook-with-args-until-success hook process message)
    (erc-with-server-buffer
      (run-hook-with-args 'erc-timer-hook (erc-current-time)))))

(add-hook 'erc-default-server-functions 'erc-handle-unknown-server-response)

(defun erc-handle-unknown-server-response (proc parsed)
  "Display unknown server response's message."
  (let ((line (concat (erc-response.sender parsed)
                      " "
                      (erc-response.command parsed)
                      " "
                      (mapconcat 'identity (erc-response.command-args parsed)
                                 " "))))
    (erc-display-message parsed 'notice proc line)))


(put 'define-erc-response-handler 'edebug-form-spec
     '(&define :name erc-response-handler
               (name &rest name)
               &optional sexp sexp def-body))

(defmacro* define-erc-response-handler ((name &rest aliases)
                                        &optional extra-fn-doc extra-var-doc
                                        &rest fn-body)
  "Define an ERC handler hook/function pair.
NAME is the response name as sent by the server (see the IRC RFC for
meanings).

This creates:
 - a hook variable `erc-server-NAME-functions' initialized to `erc-server-NAME'.
 - a function `erc-server-NAME' with body FN-BODY.

If ALIASES is non-nil, each alias in ALIASES is `defalias'ed to
`erc-server-NAME'.
Alias hook variables are created as `erc-server-ALIAS-functions' and
initialized to the same default value as `erc-server-NAME-functions'.

FN-BODY is the body of `erc-server-NAME' it may refer to the two
function arguments PROC and PARSED.

If EXTRA-FN-DOC is non-nil, it is inserted at the beginning of the
defined function's docstring.

If EXTRA-VAR-DOC is non-nil, it is inserted at the beginning of the
defined variable's docstring.

As an example:

  (define-erc-response-handler (311 WHOIS WI)
    \"Some non-generic function documentation.\"
    \"Some non-generic variable documentation.\"
    (do-stuff-with-whois proc parsed))

Would expand to:

  (prog2
      (defvar erc-server-311-functions 'erc-server-311
        \"Some non-generic variable documentation.

  Hook called upon receiving a 311 server response.
  Each function is called with two arguments, the process associated
  with the response and the parsed response.
  See also `erc-server-311'.\")

      (defun erc-server-311 (proc parsed)
        \"Some non-generic function documentation.

  Handler for a 311 server response.
  PROC is the server process which returned the response.
  PARSED is the actual response as an `erc-response' struct.
  If you want to add responses don't modify this function, but rather
  add things to `erc-server-311-functions' instead.\"
        (do-stuff-with-whois proc parsed))

    (puthash \"311\" 'erc-server-311-functions erc-server-responses)
    (puthash \"WHOIS\" 'erc-server-WHOIS-functions erc-server-responses)
    (puthash \"WI\" 'erc-server-WI-functions erc-server-responses)

    (defalias 'erc-server-WHOIS 'erc-server-311)
    (defvar erc-server-WHOIS-functions 'erc-server-311
      \"Some non-generic variable documentation.

  Hook called upon receiving a WHOIS server response.

  Each function is called with two arguments, the process associated
  with the response and the parsed response.  If the function returns
  non-nil, stop processing the hook.  Otherwise, continue.

  See also `erc-server-311'.\")

    (defalias 'erc-server-WI 'erc-server-311)
    (defvar erc-server-WI-functions 'erc-server-311
      \"Some non-generic variable documentation.

  Hook called upon receiving a WI server response.
  Each function is called with two arguments, the process associated
  with the response and the parsed response.  If the function returns
  non-nil, stop processing the hook.  Otherwise, continue.

  See also `erc-server-311'.\"))

\(fn (NAME &rest ALIASES) &optional EXTRA-FN-DOC EXTRA-VAR-DOC &rest FN-BODY)"
  (if (numberp name) (setq name (intern (format "%03i" name))))
  (setq aliases (mapcar (lambda (a)
                          (if (numberp a)
                              (format "%03i" a)
                            a))
                        aliases))
  (let* ((hook-name (intern (format "erc-server-%s-functions" name)))
         (fn-name (intern (format "erc-server-%s" name)))
         (hook-doc (format "%sHook called upon receiving a %%s server response.
Each function is called with two arguments, the process associated
with the response and the parsed response.  If the function returns
non-nil, stop processing the hook.  Otherwise, continue.

See also `%s'."
                           (if extra-var-doc
                               (concat extra-var-doc "\n\n")
                             "")
                           fn-name))
         (fn-doc (format "%sHandler for a %s server response.
PROC is the server process which returned the response.
PARSED is the actual response as an `erc-response' struct.
If you want to add responses don't modify this function, but rather
add things to `%s' instead."
                         (if extra-fn-doc
                             (concat extra-fn-doc "\n\n")
                           "")
                         name hook-name))
         (fn-alternates
          (loop for alias in aliases
                collect (intern (format "erc-server-%s" alias))))
         (var-alternates
          (loop for alias in aliases
                collect (intern (format "erc-server-%s-functions" alias)))))
    `(prog2
         ;; Normal hook variable.
         (defvar ,hook-name ',fn-name ,(format hook-doc name))
         ;; Handler function
         (defun ,fn-name (proc parsed)
           ,fn-doc
           ,@fn-body)

       ;; Make find-function and find-variable find them
       (put ',fn-name 'definition-name ',name)
       (put ',hook-name 'definition-name ',name)

       ;; Hashtable map of responses to hook variables
       ,@(loop for response in (cons name aliases)
               for var in (cons hook-name var-alternates)
               collect `(puthash ,(format "%s" response) ',var
                                 erc-server-responses))
       ;; Alternates.
       ;; Functions are defaliased, hook variables are defvared so we
       ;; can add hooks to one alias, but not another.
       ,@(loop for fn in fn-alternates
               for var in var-alternates
               for a in aliases
               nconc (list `(defalias ',fn ',fn-name)
                           `(defvar ,var ',fn-name ,(format hook-doc a))
                           `(put ',var 'definition-name ',hook-name))))))

(define-erc-response-handler (ERROR)
  "Handle an ERROR command from the server." nil
  (setq erc-server-error-occurred t)
  (erc-display-message
   parsed 'error nil 'ERROR
   ?s (erc-response.sender parsed) ?c (erc-response.contents parsed)))

(define-erc-response-handler (INVITE)
  "Handle invitation messages."
  nil
  (let ((target (first (erc-response.command-args parsed)))
        (chnl (erc-response.contents parsed)))
    (multiple-value-bind (nick login host)
        (values-list (erc-parse-user (erc-response.sender parsed)))
      (setq erc-invitation chnl)
      (when (string= target (erc-current-nick))
        (erc-display-message
         parsed 'notice 'active
         'INVITE ?n nick ?u login ?h host ?c chnl)))))


(define-erc-response-handler (JOIN)
  "Handle join messages."
  nil
  (let ((chnl (erc-response.contents parsed))
        (buffer nil))
    (multiple-value-bind (nick login host)
        (values-list (erc-parse-user (erc-response.sender parsed)))
      ;; strip the stupid combined JOIN facility (IRC 2.9)
      (if (string-match "^\\(.*\\)?\^g.*$" chnl)
          (setq chnl (match-string 1 chnl)))
      (save-excursion
        (let* ((str (cond
                     ;; If I have joined a channel
                     ((erc-current-nick-p nick)
                      (setq buffer (erc-open erc-session-server erc-session-port
                                             nick erc-session-user-full-name
                                             nil nil
                                             (list chnl) chnl
                                             erc-server-process))
                      (when buffer
                        (set-buffer buffer)
                        (erc-add-default-channel chnl)
                        (erc-server-send (format "MODE %s" chnl)))
                      (erc-with-buffer (chnl proc)
                        (erc-channel-begin-receiving-names))
                      (erc-update-mode-line)
                      (run-hooks 'erc-join-hook)
                      (erc-make-notice
                       (erc-format-message 'JOIN-you ?c chnl)))
                     (t
                      (setq buffer (erc-get-buffer chnl proc))
                      (erc-make-notice
                       (erc-format-message
                        'JOIN ?n nick ?u login ?h host ?c chnl))))))
          (when buffer (set-buffer buffer))
          (erc-update-channel-member chnl nick nick t nil nil host login)
          ;; on join, we want to stay in the new channel buffer
          ;;(set-buffer ob)
          (erc-display-message parsed nil buffer str))))))

(define-erc-response-handler (KICK)
  "Handle kick messages received from the server." nil
  (let* ((ch (first (erc-response.command-args parsed)))
         (tgt (second (erc-response.command-args parsed)))
         (reason (erc-trim-string (erc-response.contents parsed)))
         (buffer (erc-get-buffer ch proc)))
    (multiple-value-bind (nick login host)
        (values-list (erc-parse-user (erc-response.sender parsed)))
      (erc-remove-channel-member buffer tgt)
      (cond
       ((string= tgt (erc-current-nick))
        (erc-display-message
         parsed 'notice buffer
         'KICK-you ?n nick ?u login ?h host ?c ch ?r reason)
        (run-hook-with-args 'erc-kick-hook buffer)
        (erc-with-buffer
            (buffer)
          (erc-remove-channel-users))
        (erc-delete-default-channel ch buffer)
        (erc-update-mode-line buffer))
       ((string= nick (erc-current-nick))
        (erc-display-message
         parsed 'notice buffer
         'KICK-by-you ?k tgt ?c ch ?r reason))
       (t (erc-display-message
             parsed 'notice buffer
             'KICK ?k tgt ?n nick ?u login ?h host ?c ch ?r reason))))))

(define-erc-response-handler (MODE)
  "Handle server mode changes." nil
  (let ((tgt (first (erc-response.command-args parsed)))
        (mode (mapconcat 'identity (cdr (erc-response.command-args parsed))
                         " ")))
    (multiple-value-bind (nick login host)
        (values-list (erc-parse-user (erc-response.sender parsed)))
      (erc-log (format "MODE: %s -> %s: %s" nick tgt mode))
      ;; dirty hack
      (let ((buf (cond ((erc-channel-p tgt)
                        (erc-get-buffer tgt proc))
                       ((string= tgt (erc-current-nick)) nil)
                       ((erc-active-buffer) (erc-active-buffer))
                       (t (erc-get-buffer tgt)))))
        (with-current-buffer (or buf
                                 (current-buffer))
          (erc-update-modes tgt mode nick host login))
          (if (or (string= login "") (string= host ""))
              (erc-display-message parsed 'notice buf
                                   'MODE-nick ?n nick
                                   ?t tgt ?m mode)
            (erc-display-message parsed 'notice buf
                                 'MODE ?n nick ?u login
                                 ?h host ?t tgt ?m mode)))
      (erc-banlist-update proc parsed))))

(define-erc-response-handler (NICK)
  "Handle nick change messages." nil
  (let ((nn (erc-response.contents parsed))
        bufs)
    (multiple-value-bind (nick login host)
        (values-list (erc-parse-user (erc-response.sender parsed)))
      (setq bufs (erc-buffer-list-with-nick nick proc))
      (erc-log (format "NICK: %s -> %s" nick nn))
      ;; if we had a query with this user, make sure future messages will be
      ;; sent to the correct nick. also add to bufs, since the user will want
      ;; to see the nick change in the query, and if it's a newly begun query,
      ;; erc-channel-users won't contain it
      (erc-buffer-filter
       (lambda ()
         (when (equal (erc-default-target) nick)
           (setq erc-default-recipients
                 (cons nn (cdr erc-default-recipients)))
           (rename-buffer nn)
           (erc-update-mode-line)
           (add-to-list 'bufs (current-buffer)))))
      (erc-update-user-nick nick nn host nil nil login)
      (cond
       ((string= nick (erc-current-nick))
        (add-to-list 'bufs (erc-server-buffer))
        (erc-set-current-nick nn)
        (erc-update-mode-line)
        (setq erc-nick-change-attempt-count 0)
        (setq erc-default-nicks (if (consp erc-nick) erc-nick (list erc-nick)))
        (erc-display-message
         parsed 'notice bufs
         'NICK-you ?n nick ?N nn)
        (run-hook-with-args 'erc-nick-changed-functions nn nick))
       (t
        (erc-handle-user-status-change 'nick (list nick login host) (list nn))
        (erc-display-message parsed 'notice bufs 'NICK ?n nick
                             ?u login ?h host ?N nn))))))

(define-erc-response-handler (PART)
  "Handle part messages." nil
  (let* ((chnl (first (erc-response.command-args parsed)))
         (reason (erc-trim-string (erc-response.contents parsed)))
         (buffer (erc-get-buffer chnl proc)))
    (multiple-value-bind (nick login host)
        (values-list (erc-parse-user (erc-response.sender parsed)))
      (erc-remove-channel-member buffer nick)
      (erc-display-message parsed 'notice buffer
                           'PART ?n nick ?u login
                           ?h host ?c chnl ?r (or reason ""))
      (when (string= nick (erc-current-nick))
        (run-hook-with-args 'erc-part-hook buffer)
        (erc-with-buffer
            (buffer)
          (erc-remove-channel-users))
        (erc-delete-default-channel chnl buffer)
        (erc-update-mode-line buffer)
        (when erc-kill-buffer-on-part
          (kill-buffer buffer))))))

(define-erc-response-handler (PING)
  "Handle ping messages." nil
  (let ((pinger (first (erc-response.command-args parsed))))
    (erc-log (format "PING: %s" pinger))
    ;; ping response to the server MUST be forced, or you can lose big
    (erc-server-send (format "PONG :%s" pinger) t)
    (when erc-verbose-server-ping
      (erc-display-message
       parsed 'error proc
       'PING ?s (erc-time-diff erc-server-last-ping-time (erc-current-time))))
    (setq erc-server-last-ping-time (erc-current-time))))

(define-erc-response-handler (PONG)
  "Handle pong messages." nil
  (let ((time (string-to-number (erc-response.contents parsed))))
    (when (> time 0)
      (setq erc-server-lag (erc-time-diff time (erc-current-time)))
      (when erc-verbose-server-ping
        (erc-display-message
         parsed 'notice proc 'PONG
         ?h (first (erc-response.command-args parsed)) ?i erc-server-lag
         ?s (if (/= erc-server-lag 1) "s" "")))
      (erc-update-mode-line))))

(define-erc-response-handler (PRIVMSG NOTICE)
  "Handle private messages, including messages in channels." nil
  (let ((sender-spec (erc-response.sender parsed))
        (cmd (erc-response.command parsed))
        (tgt (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (if (or (erc-ignored-user-p sender-spec)
            (erc-ignored-reply-p msg tgt proc))
        (when erc-minibuffer-ignored
          (message "Ignored %s from %s to %s" cmd sender-spec tgt))
      (let* ((sndr (erc-parse-user sender-spec))
             (nick (nth 0 sndr))
             (login (nth 1 sndr))
             (host (nth 2 sndr))
             (msgp (string= cmd "PRIVMSG"))
             (noticep (string= cmd "NOTICE"))
             ;; S.B. downcase *both* tgt and current nick
             (privp (erc-current-nick-p tgt))
             s buffer
             fnick)
        (setf (erc-response.contents parsed) msg)
        (setq buffer (erc-get-buffer (if privp nick tgt) proc))
        (when buffer
          (with-current-buffer buffer
            ;; update the chat partner info.  Add to the list if private
            ;; message.  We will accumulate private identities indefinitely
            ;; at this point.
            (erc-update-channel-member (if privp nick tgt) nick nick
                                       privp nil nil host login nil nil t)
            (let ((cdata (erc-get-channel-user nick)))
              (setq fnick (funcall erc-format-nick-function
                                   (car cdata) (cdr cdata))))))
        (cond
         ((erc-is-message-ctcp-p msg)
          (setq s (if msgp
                      (erc-process-ctcp-query proc parsed nick login host)
                    (erc-process-ctcp-reply proc parsed nick login host
                                            (match-string 1 msg)))))
         (t
          (setcar erc-server-last-peers nick)
          (setq s (erc-format-privmessage
                   (or fnick nick) msg
                   ;; If buffer is a query buffer,
                   ;; format the nick as for a channel.
                   (and (not (and buffer
                                  (erc-query-buffer-p buffer)
                                  erc-format-query-as-channel-p))
                        privp)
                   msgp))))
        (when s
          (if (and noticep privp)
              (progn
                (run-hook-with-args 'erc-echo-notice-always-hook
                                    s parsed buffer nick)
                (run-hook-with-args-until-success
                 'erc-echo-notice-hook s parsed buffer nick))
            (erc-display-message parsed nil buffer s)))
        (when (string= cmd "PRIVMSG")
          (erc-auto-query proc parsed))))))

;; FIXME: need clean way of specifying extra hooks in
;; define-erc-response-handler.
(add-hook 'erc-server-PRIVMSG-functions 'erc-auto-query)

(define-erc-response-handler (QUIT)
  "Another user has quit IRC." nil
  (let ((reason (erc-response.contents parsed))
        bufs)
    (multiple-value-bind (nick login host)
        (values-list (erc-parse-user (erc-response.sender parsed)))
      (setq bufs (erc-buffer-list-with-nick nick proc))
      (erc-remove-user nick)
      (setq reason (erc-wash-quit-reason reason nick login host))
      (erc-display-message parsed 'notice bufs
                           'QUIT ?n nick ?u login
                           ?h host ?r reason))))

(define-erc-response-handler (TOPIC)
  "The channel topic has changed." nil
  (let* ((ch (first (erc-response.command-args parsed)))
         (topic (erc-trim-string (erc-response.contents parsed)))
         (time (format-time-string "%T %m/%d/%y" (current-time))))
    (multiple-value-bind (nick login host)
        (values-list (erc-parse-user (erc-response.sender parsed)))
      (erc-update-channel-member ch nick nick nil nil nil host login)
      (erc-update-channel-topic ch (format "%s\C-o (%s, %s)" topic nick time))
      (erc-display-message parsed 'notice (erc-get-buffer ch proc)
                           'TOPIC ?n nick ?u login ?h host
                           ?c ch ?T topic))))

(define-erc-response-handler (WALLOPS)
  "Display a WALLOPS message." nil
  (let ((message (erc-response.contents parsed)))
    (multiple-value-bind (nick login host)
        (values-list (erc-parse-user (erc-response.sender parsed)))
      (erc-display-message
       parsed 'notice nil
       'WALLOPS ?n nick ?m message))))

(define-erc-response-handler (001)
  "Set `erc-server-current-nick' to reflect server settings and display the welcome message."
  nil
  (erc-set-current-nick (first (erc-response.command-args parsed)))
  (erc-update-mode-line)                ; needed here?
  (setq erc-nick-change-attempt-count 0)
  (setq erc-default-nicks (if (consp erc-nick) erc-nick (list erc-nick)))
  (erc-display-message
   parsed 'notice 'active (erc-response.contents parsed)))

(define-erc-response-handler (MOTD 002 003 371 372 374 375)
  "Display the server's message of the day." nil
  (erc-handle-login)
  (erc-display-message
   parsed 'notice (if erc-server-connected 'active proc)
   (erc-response.contents parsed)))

(define-erc-response-handler (376 422)
  "End of MOTD/MOTD is missing." nil
  (erc-server-MOTD proc parsed)
  (erc-connection-established proc parsed))

(define-erc-response-handler (004)
  "Display the server's identification." nil
  (multiple-value-bind (server-name server-version)
      (values-list (cdr (erc-response.command-args parsed)))
    (setq erc-server-version server-version)
    (setq erc-server-announced-name server-name)
    (erc-update-mode-line-buffer (process-buffer proc))
    (erc-display-message
     parsed 'notice proc
     's004 ?s server-name ?v server-version
     ?U (fourth (erc-response.command-args parsed))
     ?C (fifth (erc-response.command-args parsed)))))

(define-erc-response-handler (005)
  "Set the variable `erc-server-parameters' and display the received message.

According to RFC 2812, suggests alternate servers on the network.
Many servers, however, use this code to show which parameters they have set,
for example, the network identifier, maximum allowed topic length, whether
certain commands are accepted and more.  See documentation for
`erc-server-parameters' for more information on the parameters sent.

A server may send more than one 005 message."
  nil
  (let ((line (mapconcat 'identity
                         (setf (erc-response.command-args parsed)
                               (cdr (erc-response.command-args parsed)))
                         " ")))
    (while (erc-response.command-args parsed)
      (let ((section (pop (erc-response.command-args parsed))))
        ;; fill erc-server-parameters
        (when (string-match "^\\([A-Z]+\\)\=\\(.*\\)$\\|^\\([A-Z]+\\)$"
                            section)
          (add-to-list 'erc-server-parameters
                       `(,(or (match-string 1 section)
                              (match-string 3 section))
                         .
                         ,(match-string 2 section))))))
    (erc-display-message parsed 'notice proc line)))

(define-erc-response-handler (221)
  "Display the current user modes." nil
  (let* ((nick (first (erc-response.command-args parsed)))
         (modes (mapconcat 'identity
                           (cdr (erc-response.command-args parsed)) " ")))
    (erc-set-modes nick modes)
    (erc-display-message parsed 'notice 'active 's221 ?n nick ?m modes)))

(define-erc-response-handler (252)
  "Display the number of IRC operators online." nil
  (erc-display-message parsed 'notice 'active 's252
                       ?i (second (erc-response.command-args parsed))))

(define-erc-response-handler (253)
  "Display the number of unknown connections." nil
  (erc-display-message parsed 'notice 'active 's253
                       ?i (second (erc-response.command-args parsed))))

(define-erc-response-handler (254)
  "Display the number of channels formed." nil
  (erc-display-message parsed 'notice 'active 's254
                       ?i (second (erc-response.command-args parsed))))

(define-erc-response-handler (250 251 255 256 257 258 259 265 266 377 378)
  "Generic display of server messages as notices.

See `erc-display-server-message'." nil
  (erc-display-server-message proc parsed))

(define-erc-response-handler (275)
  "Display secure connection message." nil
  (multiple-value-bind (nick user message)
      (values-list (cdr (erc-response.command-args parsed)))
    (erc-display-message
     parsed 'notice 'active 's275
     ?n nick
     ?m (mapconcat 'identity (cddr (erc-response.command-args parsed))
                   " "))))

(define-erc-response-handler (290)
  "Handle dancer-ircd CAPAB messages." nil nil)

(define-erc-response-handler (301)
  "AWAY notice." nil
  (erc-display-message parsed 'notice 'active 's301
                       ?n (second (erc-response.command-args parsed))
                       ?r (erc-response.contents parsed)))

(define-erc-response-handler (303)
  "ISON reply" nil
  (erc-display-message parsed 'notice 'active 's303
                       ?n (second (erc-response.command-args parsed))))

(define-erc-response-handler (305)
  "Return from AWAYness." nil
  (erc-process-away proc nil)
  (erc-display-message parsed 'notice 'active
                       's305 ?m (erc-response.contents parsed)))

(define-erc-response-handler (306)
  "Set AWAYness." nil
  (erc-process-away proc t)
  (erc-display-message parsed 'notice 'active
                       's306 ?m (erc-response.contents parsed)))

(define-erc-response-handler (307)
  "Display nick-identified message." nil
  (multiple-value-bind (nick user message)
      (values-list (cdr (erc-response.command-args parsed)))
    (erc-display-message
     parsed 'notice 'active 's307
     ?n nick
     ?m (mapconcat 'identity (cddr (erc-response.command-args parsed))
                   " "))))

(define-erc-response-handler (311 314)
  "WHOIS/WHOWAS notices." nil
  (let ((fname (erc-response.contents parsed))
        (catalog-entry (intern (format "s%s" (erc-response.command parsed)))))
    (multiple-value-bind (nick user host)
        (values-list (cdr (erc-response.command-args parsed)))
      (erc-update-user-nick nick nick host nil fname user)
      (erc-display-message
       parsed 'notice 'active catalog-entry
       ?n nick ?f fname ?u user ?h host))))

(define-erc-response-handler (312)
  "Server name response in WHOIS." nil
  (multiple-value-bind (nick server-host)
      (values-list (cdr (erc-response.command-args parsed)))
    (erc-display-message
     parsed 'notice 'active 's312
     ?n nick ?s server-host ?c (erc-response.contents parsed))))

(define-erc-response-handler (313)
  "IRC Operator response in WHOIS." nil
  (erc-display-message
   parsed 'notice 'active 's313
   ?n (second (erc-response.command-args parsed))))

(define-erc-response-handler (315 318 323 369)
  ;; 315 - End of WHO
  ;; 318 - End of WHOIS list
  ;; 323 - End of channel LIST
  ;; 369 - End of WHOWAS
  "End of WHO/WHOIS/LIST/WHOWAS notices." nil
  (ignore proc parsed))

(define-erc-response-handler (317)
  "IDLE notice." nil
  (multiple-value-bind (nick seconds-idle on-since time)
      (values-list (cdr (erc-response.command-args parsed)))
    (setq time (when on-since
                 (format-time-string "%T %Y/%m/%d"
                                     (erc-string-to-emacs-time on-since))))
    (erc-update-user-nick nick nick nil nil nil
                          (and time (format "on since %s" time)))
    (if time
        (erc-display-message
         parsed 'notice 'active 's317-on-since
         ?n nick ?i (erc-sec-to-time (string-to-number seconds-idle)) ?t time)
      (erc-display-message
       parsed 'notice 'active 's317
       ?n nick ?i (erc-sec-to-time (string-to-number seconds-idle))))))

(define-erc-response-handler (319)
  "Channel names in WHOIS response." nil
  (erc-display-message
   parsed 'notice 'active 's319
   ?n (second (erc-response.command-args parsed))
   ?c (erc-response.contents parsed)))

(define-erc-response-handler (320)
  "Identified user in WHOIS." nil
  (erc-display-message
   parsed 'notice 'active 's320
   ?n (second (erc-response.command-args parsed))))

(define-erc-response-handler (321)
  "LIST header." nil
  (setq erc-channel-list nil))

(defun erc-server-321-message (proc parsed)
  "Display a message for the 321 event."
  (erc-display-message parsed 'notice proc 's321)
  nil)
(add-hook 'erc-server-321-functions 'erc-server-321-message t)

(define-erc-response-handler (322)
  "LIST notice." nil
  (let ((topic (erc-response.contents parsed)))
    (multiple-value-bind (channel num-users)
        (values-list (cdr (erc-response.command-args parsed)))
      (add-to-list 'erc-channel-list (list channel))
      (erc-update-channel-topic channel topic))))

(defun erc-server-322-message (proc parsed)
  "Display a message for the 322 event."
  (let ((topic (erc-response.contents parsed)))
    (multiple-value-bind (channel num-users)
        (values-list (cdr (erc-response.command-args parsed)))
      (erc-display-message
       parsed 'notice proc 's322
       ?c channel ?u num-users ?t (or topic "")))))
(add-hook 'erc-server-322-functions 'erc-server-322-message t)

(define-erc-response-handler (324)
  "Channel or nick modes." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (modes (mapconcat 'identity (cddr (erc-response.command-args parsed))
                          " ")))
    (erc-set-modes channel modes)
    (erc-display-message
     parsed 'notice (erc-get-buffer channel proc)
     's324 ?c channel ?m modes)))

(define-erc-response-handler (328)
  "Channel URL (on freenode network)." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (url (erc-response.contents parsed)))
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's328 ?c channel ?u url)))

(define-erc-response-handler (329)
  "Channel creation date." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (time (erc-string-to-emacs-time
               (third (erc-response.command-args parsed)))))
    (erc-display-message
     parsed 'notice (erc-get-buffer channel proc)
     's329 ?c channel ?t (format-time-string "%A %Y/%m/%d %X" time))))

(define-erc-response-handler (330)
  "Nick is authed as (on Quakenet network)." nil
  ;; FIXME: I don't know what the magic numbers mean.  Mummy, make
  ;; the magic numbers go away.
  ;; No seriously, I have no clue about the format of this command,
  ;; and don't sit on Quakenet, so can't test.  Originally we had:
  ;; nick == (aref parsed 3)
  ;; authaccount == (aref parsed 4)
  ;; authmsg == (aref parsed 5)
  ;; The guesses below are, well, just that. -- Lawrence 2004/05/10
  (let ((nick (second (erc-response.command-args parsed)))
        (authaccount (third (erc-response.command-args parsed)))
        (authmsg (erc-response.contents parsed)))
    (erc-display-message parsed 'notice 'active 's330
                         ?n nick ?a authmsg ?i authaccount)))

(define-erc-response-handler (331)
  "No topic set for channel." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (topic (erc-response.contents parsed)))
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's331 ?c channel)))

(define-erc-response-handler (332)
  "TOPIC notice." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (topic (erc-response.contents parsed)))
    (erc-update-channel-topic channel topic)
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's332 ?c channel ?T topic)))

(define-erc-response-handler (333)
  "Who set the topic, and when." nil
  (multiple-value-bind (channel nick time)
      (values-list (cdr (erc-response.command-args parsed)))
    (setq time (format-time-string "%T %Y/%m/%d"
                                   (erc-string-to-emacs-time time)))
    (erc-update-channel-topic channel
                              (format "\C-o (%s, %s)" nick time)
                              'append)
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's333 ?c channel ?n nick ?t time)))

(define-erc-response-handler (341)
  "Let user know when an INVITE attempt has been sent successfully."
  nil
  (multiple-value-bind (nick channel)
      (values-list (cdr (erc-response.command-args parsed)))
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's341 ?n nick ?c channel)))

(define-erc-response-handler (352)
  "WHO notice." nil
  (multiple-value-bind (channel user host server nick away-flag)
      (values-list (cdr (erc-response.command-args parsed)))
    (let ((full-name (erc-response.contents parsed))
          hopcount)
      (when (string-match "\\(^[0-9]+ \\)\\(.*\\)$" full-name)
        (setq hopcount (match-string 1 full-name))
        (setq full-name (match-string 2 full-name)))
      (erc-update-channel-member channel nick nick nil nil nil host
                                 user full-name)
      (erc-display-message parsed 'notice 'active 's352
                           ?c channel ?n nick ?a away-flag
                           ?u user ?h host ?f full-name))))

(define-erc-response-handler (353)
  "NAMES notice." nil
  (let ((channel (third (erc-response.command-args parsed)))
        (users (erc-response.contents parsed)))
    (erc-display-message parsed 'notice (or (erc-get-buffer channel proc)
                                            'active)
                         's353 ?c channel ?u users)
    (erc-with-buffer (channel proc)
      (erc-channel-receive-names users))))

(define-erc-response-handler (366)
  "End of NAMES." nil
  (erc-with-buffer ((second (erc-response.command-args parsed)) proc)
    (erc-channel-end-receiving-names)))

(define-erc-response-handler (367)
  "Channel ban list entries." nil
  (multiple-value-bind (channel banmask setter time)
      (values-list (cdr (erc-response.command-args parsed)))
    ;; setter and time are not standard
    (if setter
        (erc-display-message parsed 'notice 'active 's367-set-by
                             ?c channel
                             ?b banmask
                             ?s setter
                             ?t (or time ""))
      (erc-display-message parsed 'notice 'active 's367
                           ?c channel
                           ?b banmask))))

(define-erc-response-handler (368)
  "End of channel ban list." nil
  (let ((channel (second (erc-response.command-args parsed))))
    (erc-display-message parsed 'notice 'active 's368
                         ?c channel)))

(define-erc-response-handler (379)
  "Forwarding to another channel." nil
  ;; FIXME: Yet more magic numbers in original code, I'm guessing this
  ;; command takes two arguments, and doesn't have any "contents". --
  ;; Lawrence 2004/05/10
  (multiple-value-bind (from to)
      (values-list (cdr (erc-response.command-args parsed)))
    (erc-display-message parsed 'notice 'active
                         's379 ?c from ?f to)))

(define-erc-response-handler (391)
  "Server's time string." nil
  (erc-display-message
   parsed 'notice 'active
   's391 ?s (second (erc-response.command-args parsed))
   ?t (third (erc-response.command-args parsed))))

(define-erc-response-handler (401)
  "No such nick/channel." nil
  (let ((nick/channel (second (erc-response.command-args parsed))))
    (when erc-whowas-on-nosuchnick
      (erc-log (format "cmd: WHOWAS: %s" nick/channel))
      (erc-server-send (format "WHOWAS %s 1" nick/channel)))
    (erc-display-message parsed '(notice error) 'active
                         's401 ?n nick/channel)))

(define-erc-response-handler (403)
  "No such channel." nil
  (erc-display-message parsed '(notice error) 'active
                       's403 ?c (second (erc-response.command-args parsed))))

(define-erc-response-handler (404)
  "Cannot send to channel." nil
  (erc-display-message parsed '(notice error) 'active
                       's404 ?c (second (erc-response.command-args parsed))))


(define-erc-response-handler (405)
  "Can't join that many channels." nil
  (erc-display-message parsed '(notice error) 'active
                       's405 ?c (second (erc-response.command-args parsed))))

(define-erc-response-handler (406)
  "No such nick." nil
  (erc-display-message parsed '(notice error) 'active
                       's406 ?n (second (erc-response.command-args parsed))))

(define-erc-response-handler (412)
  "No text to send." nil
  (erc-display-message parsed '(notice error) 'active 's412))

(define-erc-response-handler (421)
  "Unknown command." nil
  (erc-display-message parsed '(notice error) 'active 's421
                       ?c (second (erc-response.command-args parsed))))

(define-erc-response-handler (432)
  "Bad nick." nil
  (erc-display-message parsed '(notice error) 'active 's432
                       ?n (second (erc-response.command-args parsed))))

(define-erc-response-handler (433)
  "Login-time \"nick in use\"." nil
  (erc-nickname-in-use (second (erc-response.command-args parsed))
                       "already in use"))

(define-erc-response-handler (437)
  "Nick temporarily unavailable (on IRCnet)." nil
  (let ((nick/channel (second (erc-response.command-args parsed))))
    (unless (erc-channel-p nick/channel)
      (erc-nickname-in-use nick/channel "temporarily unavailable"))))

(define-erc-response-handler (442)
  "Not on channel." nil
  (erc-display-message parsed '(notice error) 'active 's442
                       ?c (second (erc-response.command-args parsed))))

(define-erc-response-handler (461)
  "Not enough parameters for command." nil
  (erc-display-message parsed '(notice error)  'active 's461
                       ?c (second (erc-response.command-args parsed))
                       ?m (erc-response.contents parsed)))

(define-erc-response-handler (465)
  "You are banned from this server." nil
  (setq erc-server-banned t)
  ;; show the server's message, as a reason might be provided
  (erc-display-error-notice
   parsed
   (erc-response.contents parsed)))

(define-erc-response-handler (474)
  "Banned from channel errors." nil
  (erc-display-message parsed '(notice error) nil
                       (intern (format "s%s"
                                       (erc-response.command parsed)))
                       ?c (second (erc-response.command-args parsed))))

(define-erc-response-handler (475)
  "Channel key needed." nil
  (erc-display-message parsed '(notice error) nil 's475
                       ?c (second (erc-response.command-args parsed)))
  (when erc-prompt-for-channel-key
    (let ((channel (second (erc-response.command-args parsed)))
          (key (read-from-minibuffer
                (format "Channel %s is mode +k.  Enter key (RET to cancel): "
                        (second (erc-response.command-args parsed))))))
      (when (and key (> (length key) 0))
          (erc-cmd-JOIN channel key)))))

(define-erc-response-handler (477)
  "Channel doesn't support modes." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (message (erc-response.contents parsed)))
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         (format "%s: %s" channel message))))

(define-erc-response-handler (482)
  "You need to be a channel operator to do that." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (message (erc-response.contents parsed)))
    (erc-display-message parsed '(error notice) 'active 's482
                         ?c channel ?m message)))

(define-erc-response-handler (671)
  "Secure connection response in WHOIS." nil
  (let ((nick (second (erc-response.command-args parsed)))
        (securemsg (erc-response.contents parsed)))
    (erc-display-message parsed 'notice 'active 's671
                         ?n nick ?a securemsg)))

(define-erc-response-handler (431 445 446 451 462 463 464 481 483 484 485
                                  491 501 502)
  ;; 431 - No nickname given
  ;; 445 - SUMMON has been disabled
  ;; 446 - USERS has been disabled
  ;; 451 - You have not registered
  ;; 462 - Unauthorized command (already registered)
  ;; 463 - Your host isn't among the privileged
  ;; 464 - Password incorrect
  ;; 481 - Need IRCop privileges
  ;; 483 - You can't kill a server!
  ;; 484 - Your connection is restricted!
  ;; 485 - You're not the original channel operator
  ;; 491 - No O-lines for your host
  ;; 501 - Unknown MODE flag
  ;; 502 - Cannot change mode for other users
  "Generic display of server error messages.

See `erc-display-error-notice'." nil
  (erc-display-error-notice
   parsed
   (intern (format "s%s" (erc-response.command parsed)))))

;; FIXME: These are yet to be implemented, they're just stubs for now
;; -- Lawrence 2004/05/12

;; response numbers left here for reference

;; (define-erc-response-handler (323 364 365 381 382 392 393 394 395
;;                               200 201 202 203 204 205 206 208 209 211 212 213
;;                               214 215 216 217 218 219 241 242 243 244 249 261
;;                               262 302 342 351 402 407 409 411 413 414 415
;;                               423 424 436 441 443 444 467 471 472 473 KILL)
;;   nil nil
;;   (ignore proc parsed))

(provide 'erc-backend)

;;; erc-backend.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
