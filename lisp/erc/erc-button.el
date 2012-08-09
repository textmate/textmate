;; erc-button.el --- A way of buttonizing certain things in ERC buffers

;; Copyright (C) 1996-2004, 2006-2012 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: irc, button, url, regexp
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcButton

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

;; Heavily borrowed from gnus-art.el. Thanks to the original authors.
;; This buttonizes nicks and other stuff to make it all clickable.
;; To enable, add to your ~/.emacs:
;; (require 'erc-button)
;; (erc-button-mode 1)
;;
;; Todo:
;; * Rewrite all this to do the same, but use button.el from GNU Emacs
;; if it's available for xemacs too.  Why? button.el is much faster,
;; and much more elegant, and solves the problem we get with large buffers
;; and a large erc-button-marker-list.


;;; Code:

(require 'erc)
(require 'wid-edit)
(require 'erc-fill)

;;; Minor Mode

(defgroup erc-button nil
  "Define how text can be turned into clickable buttons."
  :group 'erc)

;;;###autoload (autoload 'erc-button-mode "erc-button" nil t)
(define-erc-module button nil
  "This mode buttonizes all messages according to `erc-button-alist'."
  ((add-hook 'erc-insert-modify-hook 'erc-button-add-buttons 'append)
   (add-hook 'erc-send-modify-hook 'erc-button-add-buttons 'append)
   (add-hook 'erc-complete-functions 'erc-button-next-function)
   (add-hook 'erc-mode-hook 'erc-button-setup))
  ((remove-hook 'erc-insert-modify-hook 'erc-button-add-buttons)
   (remove-hook 'erc-send-modify-hook 'erc-button-add-buttons)
   (remove-hook 'erc-complete-functions 'erc-button-next-function)
   (remove-hook 'erc-mode-hook 'erc-button-setup)
   (when (featurep 'xemacs)
     (dolist (buffer (erc-buffer-list))
       (with-current-buffer buffer
         (kill-local-variable 'widget-button-face))))))

;;; Variables

(defface erc-button '((t (:bold t)))
  "ERC button face."
  :group 'erc-faces)

(defcustom erc-button-face 'erc-button
  "Face used for highlighting buttons in ERC buffers.

A button is a piece of text that you can activate by pressing
`RET' or `mouse-2' above it. See also `erc-button-keymap'."
  :type 'face
  :group 'erc-faces)

(defcustom erc-button-nickname-face 'erc-nick-default-face
  "Face used for ERC nickname buttons."
  :type 'face
  :group 'erc-faces)

(defcustom erc-button-mouse-face 'highlight
  "Face used for mouse highlighting in ERC buffers.

Buttons will be displayed in this face when the mouse cursor is
above them."
  :type 'face
  :group 'erc-faces)

(defcustom erc-button-url-regexp
  (concat "\\(www\\.\\|\\(s?https?\\|"
          "ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)"
          "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
          "[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,()]+[-a-zA-Z0-9_=#$@~`%&*+\\/()]")
  "Regular expression that matches URLs."
  :group 'erc-button
  :type 'regexp)

(defcustom erc-button-wrap-long-urls nil
  "If non-nil, \"long\" URLs matching `erc-button-url-regexp' will be wrapped.

If this variable is a number, consider URLs longer than its value to
be \"long\".  If t, URLs will be considered \"long\" if they are
longer than `erc-fill-column'."
  :group 'erc-button
  :type '(choice integer boolean))

(defcustom erc-button-buttonize-nicks t
  "Flag indicating whether nicks should be buttonized or not."
  :group 'erc-button
  :type 'boolean)

(defcustom erc-button-rfc-url "http://www.faqs.org/rfcs/rfc%s.html"
  "*URL used to browse rfc references.
%s is replaced by the number."
  :group 'erc-button
  :type 'string)

(defcustom erc-button-google-url "http://www.google.com/search?q=%s"
  "*URL used to browse Google search references.
%s is replaced by the search string."
  :group 'erc-button
  :type 'string)

(defcustom erc-button-alist
  ;; Since the callback is only executed when the user is clicking on
  ;; a button, it makes no sense to optimize performance by
  ;; bytecompiling lambdas in this alist.  On the other hand, it makes
  ;; things hard to maintain.
  '(('nicknames 0 erc-button-buttonize-nicks erc-nick-popup 0)
    (erc-button-url-regexp 0 t browse-url 0)
    ("<URL: *\\([^<> ]+\\) *>" 0 t browse-url 1)
    ("(\\(\\([^~\n \t@][^\n \t@]*\\)@\\([a-zA-Z0-9.:-]+\\)\\)" 1 t finger 2 3)
    ;; emacs internal
    ("[`]\\([a-zA-Z][-a-zA-Z_0-9]+\\)[']" 1 t erc-button-describe-symbol 1)
    ;; pseudo links
    ("\\bInfo:[\"]\\([^\"]+\\)[\"]" 0 t Info-goto-node 1)
    ("\\b\\(Ward\\|Wiki\\|WardsWiki\\|TheWiki\\):\\([A-Z][a-z]+\\([A-Z][a-z]+\\)+\\)"
     0 t (lambda (page)
           (browse-url (concat "http://c2.com/cgi-bin/wiki?" page)))
     2)
    ("EmacsWiki:\\([A-Z][a-z]+\\([A-Z][a-z]+\\)+\\)" 0 t erc-browse-emacswiki 1)
    ("Lisp:\\([a-zA-Z.+-]+\\)" 0 t erc-browse-emacswiki-lisp 1)
    ("\\bGoogle:\\([^ \t\n\r\f]+\\)"
     0 t (lambda (keywords)
           (browse-url (format erc-button-google-url keywords)))
     1)
    ("\\brfc[#: ]?\\([0-9]+\\)"
     0 t (lambda (num)
           (browse-url (format erc-button-rfc-url num)))
     1)
    ;; other
    ("\\s-\\(@\\([0-9][0-9][0-9]\\)\\)" 1 t erc-button-beats-to-time 2))
  "*Alist of regexps matching buttons in ERC buffers.
Each entry has the form (REGEXP BUTTON FORM CALLBACK PAR...), where

REGEXP is the string matching text around the button or a symbol
  indicating a variable holding that string, or a list of
  strings, or an alist with the strings in the car.  Note that
  entries in lists or alists are considered to be nicks or other
  complete words.  Therefore they are enclosed in \\< and \\>
  while searching.  REGEXP can also be the quoted symbol
  'nicknames, which matches the nickname of any user on the
  current server.

BUTTON is the number of the regexp grouping actually matching the
  button,  This is ignored if REGEXP is 'nicknames.

FORM is a lisp expression which must eval to true for the button to
  be added,

CALLBACK is the function to call when the user push this button.
  CALLBACK can also be a symbol.  Its variable value will be used
  as the callback function.

PAR is a number of a regexp grouping whose text will be passed to
  CALLBACK.  There can be several PAR arguments.  If REGEXP is
  'nicknames, these are ignored, and CALLBACK will be called with
  the nickname matched as the argument."
  :group 'erc-button
  :type '(repeat
          (list :tag "Button"
                (choice :tag "Matches"
                        regexp
                        (variable :tag "Variable containing regexp")
                        (const :tag "Nicknames" 'nicknames))
                (integer :tag "Number of the regexp section that matches")
                (choice :tag "When to buttonize"
                        (const :tag "Always" t)
                        (sexp :tag "Only when this evaluates to non-nil"))
                (function :tag "Function to call when button is pressed")
                (repeat :tag "Sections of regexp to send to the function"
                        :inline t
                        (integer :tag "Regexp section number")))))

(defcustom erc-emacswiki-url "http://www.emacswiki.org/cgi-bin/wiki.pl?"
  "*URL of the EmacsWiki Homepage."
  :group 'erc-button
  :type 'string)

(defcustom erc-emacswiki-lisp-url "http://www.emacswiki.org/elisp/"
  "*URL of the EmacsWiki ELisp area."
  :group 'erc-button
  :type 'string)

(defvar erc-button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'erc-button-press-button)
    (if (featurep 'xemacs)
        (define-key map (kbd "<button2>") 'erc-button-click-button)
      (define-key map (kbd "<mouse-2>") 'erc-button-click-button))
    (define-key map (kbd "TAB") 'erc-button-next)
    (define-key map (kbd "<backtab>") 'erc-button-previous)
    (define-key map [follow-link] 'mouse-face)
    (set-keymap-parent map erc-mode-map)
    map)
  "Local keymap for ERC buttons.")

(defvar erc-button-syntax-table
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
  "Syntax table used when buttonizing messages.
This syntax table should make all the valid nick characters word
constituents.")

(defvar erc-button-keys-added nil
  "Internal variable used to keep track of whether we've added the
global-level ERC button keys yet.")

(defun erc-button-setup ()
  "Add ERC mode-level button movement keys.  This is only done once."
  ;; Make XEmacs use `erc-button-face'.
  (when (featurep 'xemacs)
    (set (make-local-variable 'widget-button-face) nil))
  ;; Add keys.
  (unless erc-button-keys-added
    (define-key erc-mode-map (kbd "<backtab>") 'erc-button-previous)
    (setq erc-button-keys-added t)))

(defun erc-button-add-buttons ()
  "Find external references in the current buffer and make buttons of them.
\"External references\" are things like URLs, as
specified by `erc-button-alist'."
  (interactive)
  (save-excursion
    (with-syntax-table erc-button-syntax-table
      (let ((buffer-read-only nil)
            (inhibit-point-motion-hooks t)
            (inhibit-field-text-motion t)
            (alist erc-button-alist)
            entry regexp data)
        (erc-button-remove-old-buttons)
        (dolist (entry alist)
          (if (equal (car entry) (quote (quote nicknames)))
              (erc-button-add-nickname-buttons entry)
            (progn
              (setq regexp (or (and (stringp (car entry)) (car entry))
                               (and (boundp (car entry))
                                    (symbol-value (car entry)))))
              (cond ((stringp regexp)
                     (erc-button-add-buttons-1 regexp entry))
                    ((and (listp regexp) (stringp (car regexp)))
                     (dolist (r regexp)
                       (erc-button-add-buttons-1
                        (concat "\\<" (regexp-quote r) "\\>")
                        entry)))
                    ((and (listp regexp) (listp (car regexp))
                          (stringp (caar regexp)))
                     (dolist (elem regexp)
                       (erc-button-add-buttons-1
                        (concat "\\<" (regexp-quote (car elem)) "\\>")
                        entry)))))))))))

(defun erc-button-add-nickname-buttons (entry)
  "Search through the buffer for nicknames, and add buttons."
  (let ((form (nth 2 entry))
        (fun (nth 3 entry))
        bounds word)
    (when (or (eq t form)
              (eval form))
      (goto-char (point-min))
      (while (forward-word 1)
        (setq bounds (bounds-of-thing-at-point 'word))
        (setq word (buffer-substring-no-properties
                    (car bounds) (cdr bounds)))
        (when (or (and (erc-server-buffer-p) (erc-get-server-user word))
                  (and erc-channel-users (erc-get-channel-user word)))
          (erc-button-add-button (car bounds) (cdr bounds)
                                 fun t (list word)))))))

(defun erc-button-add-buttons-1 (regexp entry)
  "Search through the buffer for matches to ENTRY and add buttons."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (let ((start (match-beginning (nth 1 entry)))
          (end (match-end (nth 1 entry)))
          (form (nth 2 entry))
          (fun (nth 3 entry))
          (data (mapcar 'match-string (nthcdr 4 entry))))
      (when (or (eq t form)
                (eval form))
        (erc-button-add-button start end fun nil data regexp)))))

(defun erc-button-remove-old-buttons ()
  "Remove all existing buttons.
This is called with narrowing in effect, just before the text is
buttonized again.  Removing a button means to remove all the properties
that `erc-button-add-button' adds, except for the face."
  (remove-text-properties
   (point-min) (point-max)
   '(erc-callback nil
                  erc-data nil
                  mouse-face nil
                  keymap nil)))

(defun erc-button-add-button (from to fun nick-p &optional data regexp)
  "Create a button between FROM and TO with callback FUN and data DATA.
NICK-P specifies if this is a nickname button.
REGEXP is the regular expression which matched for this button."
  ;; Really nasty hack to <URL: > ise urls, and line-wrap them if
  ;; they're going to be wider than `erc-fill-column'.
  ;; This could be a lot cleaner, but it works for me -- lawrence.
  (let (fill-column)
    (when (and erc-button-wrap-long-urls
               (string= regexp erc-button-url-regexp)
               (> (- to from)
                  (setq fill-column (- (if (numberp erc-button-wrap-long-urls)
                                           erc-button-wrap-long-urls
                                         erc-fill-column)
                                       (length erc-fill-prefix)))))
      (setq to (prog1 (point-marker) (insert ">"))
            from (prog2 (goto-char from) (point-marker) (insert "<URL: ")))
      (let ((pos (copy-marker from)))
        (while (> (- to pos) fill-column)
          (goto-char (+ pos fill-column))
          (insert "\n" erc-fill-prefix) ; This ought to figure out
                                        ; what type of filling we're
                                        ; doing, and indent accordingly.
          (move-marker pos (point))))))
  (if nick-p
      (when erc-button-nickname-face
        (erc-button-add-face from to erc-button-nickname-face))
    (when erc-button-face
      (erc-button-add-face from to erc-button-face)))
  (add-text-properties
   from to
   (nconc (and erc-button-mouse-face
               (list 'mouse-face erc-button-mouse-face))
          (list 'erc-callback fun)
          (list 'keymap erc-button-keymap)
          (list 'rear-nonsticky t)
          (and data (list 'erc-data data))))
  (when (featurep 'xemacs)
    (widget-convert-button 'link from to :action 'erc-button-press-button
                           :suppress-face t
                           ;; Make XEmacs use our faces.
                           :button-face (if nick-p
                                            erc-button-nickname-face
                                          erc-button-face)
                           ;; Make XEmacs behave with mouse-clicks, for
                           ;; some reason, widget stuff overrides the
                           ;; 'keymap text-property.
                           :mouse-down-action 'erc-button-click-button)))

(defun erc-button-add-face (from to face)
  "Add FACE to the region between FROM and TO."
  ;; If we just use `add-text-property', then this will overwrite any
  ;; face text property already used for the button.  It will not be
  ;; merged correctly.  If we use overlays, then redisplay will be
  ;; very slow with lots of buttons.  This is why we manually merge
  ;; face text properties.
  (let ((old (erc-list (get-text-property from 'face)))
        (pos from)
        (end (next-single-property-change from 'face nil to))
        new)
    ;; old is the face at pos, in list form.  It is nil if there is no
    ;; face at pos.  If nil, the new face is FACE.  If not nil, the
    ;; new face is a list containing FACE and the old stuff.  end is
    ;; where this face changes.
    (while (< pos to)
      (setq new (if old (cons face old) face))
      (put-text-property pos end 'face new)
      (setq pos end
            old (erc-list (get-text-property pos 'face))
            end (next-single-property-change pos 'face nil to)))))

;; widget-button-click calls with two args, we ignore the first.
;; Since Emacs runs this directly, rather than with
;; widget-button-click, we need to fake an extra arg in the
;; interactive spec.
(defun erc-button-click-button (ignore event)
  "Call `erc-button-press-button'."
  (interactive "P\ne")
  (save-excursion
    (mouse-set-point event)
    (erc-button-press-button)))

;; XEmacs calls this via widget-button-press with a bunch of arguments
;; which we don't care about.
(defun erc-button-press-button (&rest ignore)
  "Check text at point for a callback function.
If the text at point has a `erc-callback' property,
call it with the value of the `erc-data' text property."
  (interactive)
  (let* ((data (get-text-property (point) 'erc-data))
         (fun (get-text-property (point) 'erc-callback)))
    (unless fun
      (message "No button at point"))
    (when (and fun (symbolp fun) (not (fboundp fun)))
      (error "Function %S is not bound" fun))
    (apply fun data)))

(defun erc-button-next-function ()
  "Pseudo completion function that actually jumps to the next button.
For use on `completion-at-point-functions'."
    (when (< (point) (erc-beg-of-input-line))
      `(lambda ()
         (let ((here ,(point)))
           (while (and (get-text-property here 'erc-callback)
                       (not (= here (point-max))))
             (setq here (1+ here)))
           (while (and (not (get-text-property here 'erc-callback))
                       (not (= here (point-max))))
             (setq here (1+ here)))
           (if (< here (point-max))
               (goto-char here)
             (error "No next button"))
           t))))

(defun erc-button-next ()
  "Go to the next button in this buffer."
  (interactive)
  (let ((f (erc-button-next-function)))
    (if f (funcall f))))

(defun erc-button-previous ()
  "Go to the previous button in this buffer."
  (interactive)
  (let ((here (point)))
    (when (< here (erc-beg-of-input-line))
      (while (and (get-text-property here 'erc-callback)
                  (not (= here (point-min))))
        (setq here (1- here)))
      (while (and (not (get-text-property here 'erc-callback))
                  (not (= here (point-min))))
        (setq here (1- here)))
      (if (> here (point-min))
          (goto-char here)
        (error "No previous button"))
      t)))

(defun erc-browse-emacswiki (thing)
  "Browse to thing in the emacs-wiki."
  (browse-url (concat erc-emacswiki-url thing)))

(defun erc-browse-emacswiki-lisp (thing)
  "Browse to THING in the emacs-wiki elisp area."
  (browse-url (concat erc-emacswiki-lisp-url thing)))

;;; Nickname buttons:

(defcustom erc-nick-popup-alist
  '(("DeOp"  . (erc-cmd-DEOP nick))
    ("Kick"  . (erc-cmd-KICK (concat nick " "
                                     (read-from-minibuffer
                                      (concat "Kick " nick ", reason: ")))))
    ("Msg"   . (erc-cmd-MSG (concat nick " "
                                    (read-from-minibuffer
                                     (concat "Message to " nick ": ")))))
    ("Op"    . (erc-cmd-OP nick))
    ("Query" . (erc-cmd-QUERY nick))
    ("Whois" . (erc-cmd-WHOIS nick))
    ("Lastlog" . (erc-cmd-LASTLOG nick)))
  "*An alist of possible actions to take on a nickname.
An entry looks like (\"Action\" . SEXP) where SEXP is evaluated with
the variable `nick' bound to the nick in question.

Examples:
 (\"DebianDB\" .
  (shell-command
   (format
    \"ldapsearch -x -P 2 -h db.debian.org -b dc=debian,dc=org ircnick=%s\"
    nick)))"
  :group 'erc-button
  :type '(repeat (cons (string :tag "Op")
                       sexp)))

(defun erc-nick-popup (nick)
  (let* ((completion-ignore-case t)
         (action (completing-read (concat "What action to take on '" nick "'? ")
                                  erc-nick-popup-alist))
         (code (cdr (assoc action erc-nick-popup-alist))))
    (when code
      (erc-set-active-buffer (current-buffer))
      (eval code))))

;;; Callback functions
(defun erc-button-describe-symbol (symbol-name)
  "Describe SYMBOL-NAME.
Use `describe-function' for functions, `describe-variable' for variables,
and `apropos' for other symbols."
  (let ((symbol (intern-soft symbol-name)))
    (cond ((and symbol (fboundp symbol))
           (describe-function symbol))
          ((and symbol (boundp symbol))
           (describe-variable symbol))
          (t (apropos symbol-name)))))

(defun erc-button-beats-to-time (beats)
  "Display BEATS in a readable time format."
  (let* ((seconds (- (* (string-to-number beats) 86.4)
                     3600
                     (- (car (current-time-zone)))))
         (hours (mod (floor seconds 3600) 24))
         (minutes (mod (round seconds 60) 60)))
    (message (format "@%s is %d:%02d local time"
                     beats hours minutes))))

(provide 'erc-button)

;;; erc-button.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

