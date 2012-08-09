;;; tutorial.el --- tutorial for Emacs

;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help, internal
;; Package: emacs

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

;; Code for running the Emacs tutorial.

;;; History:

;; File was created 2006-09.

;;; Code:

(require 'help-mode) ;; for function help-buffer

(defface tutorial-warning-face
  '((t :inherit font-lock-warning-face))
  "Face used to highlight warnings in the tutorial."
  :group 'help)

(defvar tutorial--point-before-chkeys 0
  "Point before display of key changes.")
(make-variable-buffer-local 'tutorial--point-before-chkeys)

(defvar tutorial--point-after-chkeys 0
  "Point after display of key changes.")
(make-variable-buffer-local 'tutorial--point-after-chkeys)

(defvar tutorial--lang nil
  "Tutorial language.")
(make-variable-buffer-local 'tutorial--lang)

(defun tutorial--describe-nonstandard-key (value)
  "Give more information about a changed key binding.
This is used in `help-with-tutorial'.  The information includes
the key sequence that no longer has a default binding, the
default binding and the current binding.  It also tells in what
keymap the new binding has been done and how to access the
function in the default binding from the keyboard.

For `cua-mode' key bindings that try to combine CUA key bindings
with default Emacs bindings information about this is shown.

VALUE should have either of these formats:

  \(cua-mode)
  \(current-binding KEY-FUN DEF-FUN KEY WHERE)

Where
  KEY         is a key sequence whose standard binding has been changed
  KEY-FUN     is the actual binding for KEY
  DEF-FUN     is the standard binding of KEY
  WHERE       is a text describing the key sequences to which DEF-FUN is
              bound now (or, if it is remapped, a key sequence
              for the function it is remapped to)"
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'tutorial--describe-nonstandard-key value)
                     (called-interactively-p 'interactive))
    (with-current-buffer (help-buffer)
      (insert
       "Your Emacs customizations override the default binding for this key:"
       "\n\n")
      (let ((inhibit-read-only t))
        (cond
         ((eq (car value) 'cua-mode)
          (insert
           "CUA mode is enabled.

When CUA mode is enabled, you can use C-z, C-x, C-c, and C-v to
undo, cut, copy, and paste in addition to the normal Emacs
bindings.  The C-x and C-c keys only do cut and copy when the
region is active, so in most cases, they do not conflict with the
normal function of these prefix keys.

If you really need to perform a command which starts with one of
the prefix keys even when the region is active, you have three
options:
- press the prefix key twice very quickly (within 0.2 seconds),
- press the prefix key and the following key within 0.2 seconds, or
- use the SHIFT key with the prefix key, i.e. C-S-x or C-S-c."))
         ((eq (car value) 'current-binding)
          (let ((cb    (nth 1 value))
                (db    (nth 2 value))
                (key   (nth 3 value))
                (where (nth 4 value))
                map
                (maps (current-active-maps))
                mapsym)
            ;; Look at the currently active keymaps and try to find
            ;; first the keymap where the current binding occurs:
            (while maps
              (let* ((m (car maps))
                     (mb (lookup-key m key t)))
                (setq maps (cdr maps))
                (when (eq mb cb)
                  (setq map m)
                  (setq maps nil))))
            ;; Now, if a keymap was found we must found the symbol
            ;; name for it to display to the user.  This can not
            ;; always be found since all keymaps does not have a
            ;; symbol pointing to them, but here they should have
            ;; that:
            (when map
              (mapatoms (lambda (s)
                          (and
                           ;; If not already found
                           (not mapsym)
                           ;; and if s is a keymap
                           (and (boundp s)
                                (keymapp (symbol-value s)))
                           ;; and not the local symbol map
                           (not (eq s 'map))
                           ;; and the value of s is map
                           (eq map (symbol-value s))
                           ;; then save this value in mapsym
                           (setq mapsym s)))))
            (insert "The default Emacs binding for the key "
                    (key-description key)
                    " is the command `")
            (insert (format "%s" db))
            (insert "'.  "
                    "However, your customizations have "
                    (if cb
                        (format "rebound it to the command `%s'" cb)
                      "unbound it"))
            (insert ".")
            (when mapsym
              (insert "  (For the more advanced user:"
                      " This binding is in the keymap `"
                      (format "%s" mapsym)
                      "'.)"))
            (if (string= where "")
                (unless (keymapp db)
                  (insert "\n\nYou can use M-x "
                          (format "%s" db)
                          " RET instead."))
              (insert "\n\nWith your current key bindings"
                      " you can use "
                      (if (string-match "^the .*menus?$" where)
                          ""
                        "the key")
                      where
                      " to get the function `"
                      (format "%s" db)
                      "'.")))
          (fill-region (point-min) (point)))))
      (help-print-return-message))))

(defun tutorial--sort-keys (left right)
  "Sort predicate for use with `tutorial--default-keys'.
This is a predicate function to `sort'.

The sorting is for presentation purpose only and is done on the
key sequence.

LEFT and RIGHT are the elements to compare."
  (let ((x (append (cadr left)  nil))
        (y (append (cadr right) nil)))
    ;; Skip the front part of the key sequences if they are equal:
    (while (and x y
                (listp x) (listp y)
                (equal (car x) (car y)))
      (setq x (cdr x))
      (setq y (cdr y)))
    ;; Try to make a comparison that is useful for presentation (this
    ;; could be made nicer perhaps):
    (let ((cx (car x))
          (cy (car y)))
      ;;(message "x=%s, y=%s;;;; cx=%s, cy=%s" x y cx cy)
      (cond
       ;; Lists? Then call this again
       ((and cx cy
             (listp cx)
             (listp cy))
        (tutorial--sort-keys cx cy))
       ;; Are both numbers? Then just compare them
       ((and (wholenump cx)
             (wholenump cy))
        (> cx cy))
       ;; Is one of them a number? Let that be bigger then.
       ((wholenump cx)
        t)
       ((wholenump cy)
        nil)
       ;; Are both symbols? Compare the names then.
       ((and (symbolp cx)
             (symbolp cy))
        (string< (symbol-name cy)
                 (symbol-name cx)))))))

(defconst tutorial--default-keys
  ;; On window system, `suspend-emacs' is replaced in the default
  ;; keymap
  (let* ((suspend-emacs 'suspend-frame)
         (default-keys
           `((ESC-prefix [27])
             (Control-X-prefix [?\C-x])
             (mode-specific-command-prefix [?\C-c])
             (save-buffers-kill-terminal [?\C-x ?\C-c])

             ;; * SUMMARY
             (scroll-up-command [?\C-v])
             (scroll-down-command [?\M-v])
             (recenter-top-bottom [?\C-l])

             ;; * BASIC CURSOR CONTROL
             (forward-char [?\C-f])
             (backward-char [?\C-b])
             (forward-word [?\M-f])
             (backward-word [?\M-b])
             (next-line [?\C-n])
             (previous-line [?\C-p])
             (move-beginning-of-line [?\C-a])
             (move-end-of-line [?\C-e])
             (backward-sentence [?\M-a])
             (forward-sentence [?\M-e])
             (newline "\r")
             (beginning-of-buffer [?\M-<])
             (end-of-buffer [?\M->])
             (universal-argument [?\C-u])

             ;; * WHEN EMACS IS HUNG
             (keyboard-quit [?\C-g])

             ;; * DISABLED COMMANDS
             (downcase-region [?\C-x ?\C-l])

             ;; * WINDOWS
             (delete-other-windows [?\C-x ?1])
             ;; C-u 0 C-l
             ;; Type CONTROL-h k CONTROL-f.

             ;; * INSERTING AND DELETING
             ;; C-u 8 * to insert ********.
             (delete-backward-char "\d")
             (delete-char [?\C-d])
             (backward-kill-word [?\M-\d])
             (kill-word [?\M-d])
             (kill-line [?\C-k])
             (kill-sentence [?\M-k])
             (set-mark-command [?\C-@])
             (set-mark-command [?\C- ])
             (kill-region [?\C-w])
             (yank [?\C-y])
             (yank-pop [?\M-y])

             ;; * UNDO
             (undo [?\C-x ?u])

             ;; * FILES
             (find-file [?\C-x ?\C-f])
             (save-buffer [?\C-x ?\C-s])

             ;; * BUFFERS
             (list-buffers [?\C-x ?\C-b])
             (switch-to-buffer [?\C-x ?b])
             (save-some-buffers [?\C-x ?s])

             ;; * EXTENDING THE COMMAND SET
             ;; C-x	Character eXtend.  Followed by one character.
             (execute-extended-command [?\M-x])
             ;; C-x C-f		Find file
             ;; C-x C-s		Save file
             ;; C-x s		Save some buffers
             ;; C-x C-b		List buffers
             ;; C-x b		Switch buffer
             ;; C-x C-c		Quit Emacs
             ;; C-x 1		Delete all but one window
             ;; C-x u		Undo

             ;; * MODE LINE
             (describe-mode [?\C-h ?m])
             (set-fill-column [?\C-x ?f])
             (fill-paragraph [?\M-q])

             ;; * SEARCHING
             (isearch-forward [?\C-s])
             (isearch-backward [?\C-r])

             ;; * MULTIPLE WINDOWS
             (split-window-below [?\C-x ?2])
             (scroll-other-window [?\C-\M-v])
             (other-window [?\C-x ?o])
             (find-file-other-window [?\C-x ?4 ?\C-f])

             ;; * RECURSIVE EDITING LEVELS
             (keyboard-escape-quit [27 27 27])

             ;; * GETTING MORE HELP
             ;; The most basic HELP feature is C-h c
             (describe-key-briefly [?\C-h ?c])
             (describe-key [?\C-h ?k])

             ;; * MORE FEATURES
             ;; F10

             ;; * CONCLUSION
             ;;(iconify-or-deiconify-frame [?\C-z])
             (,suspend-emacs [?\C-z]))))
    (sort default-keys 'tutorial--sort-keys))
  "Default Emacs key bindings that the tutorial depends on.")

(defun tutorial--detailed-help (button)
  "Give detailed help about changed keys."
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'tutorial--detailed-help button)
                     (called-interactively-p 'interactive))
    (with-current-buffer (help-buffer)
      (let* ((tutorial-buffer  (button-get button 'tutorial-buffer))
             (explain-key-desc (button-get button 'explain-key-desc))
             (changed-keys (with-current-buffer tutorial-buffer
                             (save-excursion
                               (goto-char (point-min))
                               (tutorial--find-changed-keys
				tutorial--default-keys)))))
        (when changed-keys
          (insert
           "The following key bindings used in the tutorial have been changed
from the Emacs default:\n\n" )
          (let ((frm "   %-14s %-27s %-16s\n"))
            (insert (format frm
			    "Standard Key" "Command" "In Your Emacs")))
          (dolist (tk changed-keys)
            (let* ((def-fun     (nth 1 tk))
                   (key         (nth 0 tk))
                   (def-fun-txt (nth 2 tk))
                   (where       (nth 3 tk))
                   (remark      (nth 4 tk))
                   (rem-fun (command-remapping def-fun))
                   (key-txt (key-description key))
                   (key-fun (with-current-buffer tutorial-buffer (key-binding key)))
                   tot-len)
              (unless (eq def-fun key-fun)
                ;; Insert key binding description:
                (when (string= key-txt explain-key-desc)
                  (put-text-property 0 (length key-txt)
				     'face 'tutorial-warning-face key-txt))
                (insert "   " key-txt " ")
		(indent-to 18)
                ;; Insert a link describing the old binding:
                (insert-button def-fun-txt
                               'value def-fun
                               'action
                               (lambda (button) (interactive)
                                 (describe-function
                                  (button-get button 'value)))
                               'follow-link t)
		(indent-to 45)
                (when (listp where)
                  (setq where "list"))
                ;; Tell where the old binding is now:
                (insert (format " %-16s "
                                (if (string= "" where)
                                    (format "M-x %s" def-fun-txt)
                                  where)))
                ;; Insert a link with more information, for example
                ;; current binding and keymap or information about
                ;; cua-mode replacements:
                (insert-button (car remark)
                               'action
                               (lambda (b) (interactive)
                                 (let ((value (button-get b 'value)))
                                   (tutorial--describe-nonstandard-key value)))
                               'value (cdr remark)
                               'follow-link t)
                (insert "\n")))))

        (insert "
It is OK to change key bindings, but changed bindings do not
correspond to what the tutorial says.\n\n")
        (help-print-return-message)))))

(defun tutorial--find-changed-keys (default-keys)
  "Find the key bindings used in the tutorial that have changed.
Return a list with elements of the form

  '(KEY DEF-FUN DEF-FUN-TXT WHERE REMARK QUIET)

where

  KEY         is a key sequence whose standard binding has been changed
  DEF-FUN     is the standard binding of KEY
  DEF-FUN-TXT is a short descriptive text for DEF-FUN
  WHERE       is a text describing the key sequences to which DEF-FUN is
              bound now (or, if it is remapped, a key sequence
              for the function it is remapped to)
  REMARK      is a list with info about rebinding. It has either of
              these formats:

                \(TEXT cua-mode)
                \(TEXT current-binding KEY-FUN DEF-FUN KEY WHERE)

              Here TEXT is a link text to show to the user.  The
              rest of the list is used to show information when
              the user clicks the link.

              KEY-FUN is the actual binding for KEY.
  QUIET       is t if this changed keybinding should be handled quietly.
              This is used by `tutorial--display-changes'."
  (let (changed-keys remark)
    ;; Look up the bindings in a Fundamental mode buffer
    ;; so we do not get fooled by some other major mode.
    (with-temp-buffer
      (fundamental-mode)
      (dolist (kdf default-keys)
	;; The variables below corresponds to those with the same names
	;; described in the doc string.
	(let* ((key     (nth 1 kdf))
	       (def-fun (nth 0 kdf))
	       (def-fun-txt (format "%s" def-fun))
	       (rem-fun (command-remapping def-fun))
	       ;; Handle prefix definitions specially
	       ;; so that a mode that rebinds some subcommands
	       ;; won't make it appear that the whole prefix is gone.
	       (key-fun (if (eq def-fun 'ESC-prefix)
			    (lookup-key global-map [27])
			  (if (eq def-fun 'Control-X-prefix)
			      (lookup-key global-map [24])
			    (key-binding key))))
	       (where (where-is-internal (if rem-fun rem-fun def-fun)))
	       cwhere)

	  (if where
	      (progn
		(setq cwhere (car where)
		      where (key-description cwhere))
		(when (and (< 10 (length where))
			   (string= (substring where 0 (length "<menu-bar>"))
				    "<menu-bar>"))
		  (setq where
			(if (and (vectorp cwhere)
				 (setq cwhere (elt cwhere 1))
				 (setq cwhere
				       (cadr
					(assoc cwhere
					       (lookup-key global-map
							   [menu-bar]))))
				 (stringp cwhere))
			    (format "the `%s' menu" cwhere)
			  "the menus"))))
	    (setq where ""))
	  (setq remark nil)
	  (unless
	      (cond ((eq key-fun def-fun)
		     ;; No rebinding, return t
		     t)
		    ((and key-fun
			  (eq key-fun (command-remapping def-fun)))
		     ;; Just a remapping, return t
		     t)
		    ;; cua-mode specials:
		    ((and cua-mode
			  (or (and
			       (equal key [?\C-v])
			       (eq key-fun 'cua-paste))
			      (and
			       (equal key [?\C-z])
			       (eq key-fun 'undo))))
		     (setq remark (list "cua-mode, more info" 'cua-mode))
		     nil)
		    ((and cua-mode
			  (or (and (eq def-fun 'ESC-prefix)
				   (equal key-fun
					  `(keymap
					    (118 . cua-repeat-replace-region)))
				   (setq def-fun-txt "\"ESC prefix\""))
			      (and (eq def-fun 'mode-specific-command-prefix)
				   (equal key-fun
					  '(keymap
					    (timeout . copy-region-as-kill)))
				   (setq def-fun-txt "\"C-c prefix\""))
			      (and (eq def-fun 'Control-X-prefix)
				   (equal key-fun
					  '(keymap (timeout . kill-region)))
				   (setq def-fun-txt "\"C-x prefix\""))))
		     (setq remark (list "cua-mode replacement" 'cua-mode))
		     (setq where "Same key")
		     nil)
		    ;; viper-mode specials:
		    ((and (boundp 'viper-mode-string)
			  (boundp 'viper-current-state)
			  (eq viper-current-state 'vi-state)
			  (or (and (eq def-fun 'isearch-forward)
				   (eq key-fun 'viper-isearch-forward))
			      (and (eq def-fun 'isearch-backward)
				   (eq key-fun 'viper-isearch-backward))))
		     ;; These bindings works as the default bindings,
		     ;; return t
		     t)
		    ((when normal-erase-is-backspace
		       (or (and (equal key [C-delete])
				(equal key-fun 'kill-word))
			   (and (equal key [C-backspace])
				(equal key-fun 'backward-kill-word))))
		     ;; This is the strange handling of C-delete and
		     ;; C-backspace, return t
		     t)
		    (t
		     ;; This key has indeed been rebound. Put information
		     ;; in `remark' and return nil
		     (setq remark
			   (list "more info" 'current-binding
				 key-fun def-fun key where))
		     nil))
	    (add-to-list 'changed-keys
			 (list key def-fun def-fun-txt where remark nil))))))
    changed-keys))

(defun tutorial--key-description (key)
  (let ((desc (key-description key)))
    (cond ((string= "ESC" desc) "<ESC>")
	  ((string= "RET" desc) "<Return>")
	  ((string= "DEL" desc) "<Delback>")
	  (t desc))))

(defun tutorial--display-changes ()
  "Display changes to some default key bindings.
If some of the default key bindings that the tutorial depends on
have been changed then display the changes in the tutorial buffer
with some explanatory links."
  (let* ((changed-keys (tutorial--find-changed-keys
			tutorial--default-keys))
	 ;; Alist of element (DESC . CK) where DESC is the
	 ;; key-description of a changed key and CK is the
	 ;; corresponding element in `changed-keys'.
	 (changed-keys-alist
	  (mapcar (lambda (ck) (cons (tutorial--key-description (car ck)) ck))
		  changed-keys))
	 changed-key
	 (start (point))
	 (case-fold-search nil)
	 (keybindings-regexp
	  (concat "[[:space:]]\\("
		  (mapconcat (lambda (kdf) (regexp-quote
					    (tutorial--key-description
					     (nth 1 kdf))))
			     tutorial--default-keys
			     "\\|")
		  "\\)[[:punct:][:space:]]")))
    ;; Need the custom button face for viper buttons:
    (if (boundp 'viper-mode-string) (require 'cus-edit))

    (if (or changed-keys (boundp 'viper-mode-string))
	(let ((head  (get-lang-string tutorial--lang 'tut-chgdhead))
	      (head2 (get-lang-string tutorial--lang 'tut-chgdhead2)))
	  (when (and head head2)
	    (goto-char tutorial--point-before-chkeys)
	    (insert head " [")
	    (insert-button head2 'tutorial-buffer (current-buffer)
			   'action 'tutorial--detailed-help
			   'follow-link t 'face 'link)
	    (insert "]\n\n")
	    (add-text-properties tutorial--point-before-chkeys (point)
				 '(tutorial-remark remark
				   face tutorial-warning-face
				   read-only t)))))

    ;; Scan the tutorial for all key sequences.
    (goto-char (point-min))
    (while (re-search-forward keybindings-regexp (point-max) t)
      ;; Then highlight each rebound key sequence.
      ;; This avoids issuing a warning for, e.g., C-x C-b if C-b is rebound.
      (setq changed-key (assoc (match-string 1) changed-keys-alist))
      (and changed-key
	   (not (get-text-property (match-beginning 1) 'tutorial-remark))
	   (let* ((desc    (car changed-key))
		  (ck      (cdr changed-key))
		  (key     (nth 0 ck))
		  (def-fun (nth 1 ck))
		  (where   (nth 3 ck))
		  s1 s2 help-string)
	     (unless (string= where "Same key")
	       (when (string= where "")
		 (setq where (format "M-x %s" def-fun)))
	       (setq tutorial--point-after-chkeys (point-marker)
		     s1 (get-lang-string tutorial--lang 'tut-chgdkey)
		     s2 (get-lang-string tutorial--lang 'tut-chgdkey2)
		     help-string (and s1 s2 (format s1 desc where)))
	       (add-text-properties (match-beginning 1) (match-end 1)
				    '(face tutorial-warning-face
				      tutorial-remark key-sequence))
	       (if help-string
		   (if (nth 5 ck)
		       ;; Put help string in the tooltip.
		       (put-text-property (match-beginning 1) (match-end 1)
					  'help-echo help-string)
		     ;; Put help string in the buffer.
		     (save-excursion
		       (setcar (nthcdr 5 ck) t)
		       (forward-line)
		       ;; Two or more changed keys were on the same line.
		       (while (eq (get-text-property (point) 'tutorial-remark)
				  'remark)
			 (forward-line))
		       (setq start (point))
		       (insert "** " help-string " [")
		       (insert-button s2 'tutorial-buffer (current-buffer)
				      'action 'tutorial--detailed-help
				      'explain-key-desc desc 'follow-link t
				      'face 'link)
		       (insert "] **\n")
		       (add-text-properties start (point)
					    '(tutorial-remark remark
					      rear-nonsticky t
					      face tutorial-warning-face
					      read-only t)))))))))))

(defun tutorial--saved-dir ()
  "Directory to which tutorials are saved."
  (locate-user-emacs-file "tutorial/"))

(defun tutorial--saved-file ()
  "File name in which to save tutorials."
  (let ((file-name tutorial--lang)
        (ext (file-name-extension tutorial--lang)))
    (when (or (not ext)
              (string= ext ""))
      (setq file-name (concat file-name ".tut")))
    (expand-file-name file-name (tutorial--saved-dir))))

(defun tutorial--remove-remarks ()
  "Remove the remark lines that was added to the tutorial buffer."
  (save-excursion
    (goto-char (point-min))
    (let (prop-start
          prop-end
          prop-val)
      ;; Catch the case when we already are on a remark line
      (while (if (get-text-property (point) 'tutorial-remark)
                 (setq prop-start (point))
               (setq prop-start (next-single-property-change (point) 'tutorial-remark)))
        (setq prop-end (next-single-property-change prop-start 'tutorial-remark))
        (setq prop-val (get-text-property prop-start 'tutorial-remark))
        (unless prop-end
          (setq prop-end (point-max)))
        (goto-char prop-end)
        (unless (eq prop-val 'key-sequence)
	  (delete-region prop-start prop-end))))))

(defun tutorial--save-tutorial ()
  "Save the tutorial buffer.
This saves the part of the tutorial before and after the area
showing changed keys.  It also saves the point position and the
position where the display of changed bindings was inserted."
  ;; This runs in a hook so protect it:
  (condition-case err
      (if (y-or-n-p "Save your position in the tutorial? ")
	  (tutorial--save-tutorial-to (tutorial--saved-file))
	(message "Tutorial position not saved"))
    (error (message "Error saving tutorial state: %s"
		    (error-message-string err)))))

(defun tutorial--save-tutorial-to (saved-file)
  "Save the tutorial buffer to SAVED-FILE.
See `tutorial--save-tutorial' for more information."
  ;; Anything to save?
  (when (or (buffer-modified-p)
            (< 1 (point)))
    (let ((tutorial-dir (tutorial--saved-dir))
          save-err)
      ;; The tutorial is saved in a subdirectory in the user home
      ;; directory. Create this subdirectory first.
      (unless (file-directory-p tutorial-dir)
        (condition-case err
            (make-directory tutorial-dir nil)
          (error (setq save-err t)
                 (warn "Could not create directory %s: %s" tutorial-dir
                       (error-message-string err)))))
      ;; Make sure we have that directory.
      (if (file-directory-p tutorial-dir)
          (let ((tut-point (if (= 0 tutorial--point-after-chkeys)
                               ;; No info about changed keys is
                               ;; displayed.
                               (point)
                             (if (< (point) tutorial--point-after-chkeys)
                                 (- (point))
                               (- (point) tutorial--point-after-chkeys))))
                (old-point (point))
                ;; Use a special undo list so that we easily can undo
                ;; the changes we make to the tutorial buffer.  This is
                ;; currently not needed since we now delete the buffer
                ;; after saving, but kept for possible future use of
                ;; this function.
                buffer-undo-list
                (inhibit-read-only t))
            ;; Delete the area displaying info about changed keys.
            ;;             (when (< 0 tutorial--point-after-chkeys)
            ;;               (delete-region tutorial--point-before-chkeys
            ;;                              tutorial--point-after-chkeys))
            ;; Delete the remarks:
            (tutorial--remove-remarks)
            ;; Put the value of point first in the buffer so it will
            ;; be saved with the tutorial.
            (goto-char (point-min))
            (insert (number-to-string tut-point)
                    "\n"
                    (number-to-string (marker-position
                                       tutorial--point-before-chkeys))
                    "\n")
            (condition-case err
                (write-region nil nil saved-file)
              (error (setq save-err t)
                     (warn "Could not save tutorial to %s: %s"
                           saved-file
                           (error-message-string err))))
            ;; An error is raised here?? Is this a bug?
            (condition-case err
                (undo-only)
              (error nil))
            ;; Restore point
            (goto-char old-point)
            (if save-err
                (message "Could not save tutorial state.")
              (message "Saved tutorial state.")))
        (message "Can't save tutorial: %s is not a directory"
                 tutorial-dir)))))


;;;###autoload
(defun help-with-tutorial (&optional arg dont-ask-for-revert)
  "Select the Emacs learn-by-doing tutorial.
If there is a tutorial version written in the language
of the selected language environment, that version is used.
If there's no tutorial in that language, `TUTORIAL' is selected.
With ARG, you are asked to choose which language.
If DONT-ASK-FOR-REVERT is non-nil the buffer is reverted without
any question when restarting the tutorial.

If any of the standard Emacs key bindings that are used in the
tutorial have been changed then an explanatory note about this is
shown in the beginning of the tutorial buffer.

When the tutorial buffer is killed the content and the point
position in the buffer is saved so that the tutorial may be
resumed later."
  (interactive "P")
  (if (boundp 'viper-current-state)
      (let ((prompt1
             "You can not run the Emacs tutorial directly because you have \
enabled Viper.")
	    (prompt2 "\nThere is however a Viper tutorial you can run instead.
Run the Viper tutorial? "))
	(if (fboundp 'viper-tutorial)
	    (if (y-or-n-p (concat prompt1 prompt2))
		(progn (message "")
		       (funcall 'viper-tutorial 0))
	      (message "Tutorial aborted by user"))
	  (message prompt1)))
    (let* ((lang (if arg
                     (let ((minibuffer-setup-hook minibuffer-setup-hook))
                       (add-hook 'minibuffer-setup-hook
                                 'minibuffer-completion-help)
                       (read-language-name 'tutorial "Language: " "English"))
                   (if (get-language-info current-language-environment 'tutorial)
                       current-language-environment
                     "English")))
           (filename (get-language-info lang 'tutorial))
           (tut-buf-name filename)
           (old-tut-buf (get-buffer tut-buf-name))
           (old-tut-win (when old-tut-buf (get-buffer-window old-tut-buf t)))
           (old-tut-is-ok (when old-tut-buf
                            (not (buffer-modified-p old-tut-buf))))
           old-tut-file
           (old-tut-point 1))
      (setq tutorial--point-after-chkeys (point-min))
      ;; Try to display the tutorial buffer before asking to revert it.
      ;; If the tutorial buffer is shown in some window make sure it is
      ;; selected and displayed:
      (if old-tut-win
          (raise-frame
           (window-frame
            (select-window (get-buffer-window old-tut-buf t))))
        ;; Else, is there an old tutorial buffer? Then display it:
        (when old-tut-buf
          (switch-to-buffer old-tut-buf)))
      ;; Use whole frame for tutorial
      (delete-other-windows)
      ;; If the tutorial buffer has been changed then ask if it should
      ;; be reverted:
      (when (and old-tut-buf
                 (not old-tut-is-ok))
        (setq old-tut-is-ok
              (if dont-ask-for-revert
                  nil
                (not (y-or-n-p
                      "You have changed the Tutorial buffer.  Revert it? ")))))
      ;; (Re)build the tutorial buffer if it is not ok
      (unless old-tut-is-ok
        (switch-to-buffer (get-buffer-create tut-buf-name))
        ;; (unless old-tut-buf (text-mode))
        (unless lang (error "Variable lang is nil"))
        (setq tutorial--lang lang)
        (setq old-tut-file (file-exists-p (tutorial--saved-file)))
        (let ((inhibit-read-only t))
          (erase-buffer))
        (message "Preparing tutorial ...") (sit-for 0)

        ;; Do not associate the tutorial buffer with a file. Instead use
        ;; a hook to save it when the buffer is killed.
        (setq buffer-auto-save-file-name nil)
        (add-hook 'kill-buffer-hook 'tutorial--save-tutorial nil t)

        ;; Insert the tutorial. First offer to resume last tutorial
        ;; editing session.
        (when dont-ask-for-revert
          (setq old-tut-file nil))
        (when old-tut-file
          (setq old-tut-file
                (y-or-n-p "Resume your last saved tutorial? ")))
        (if old-tut-file
            (progn
              (insert-file-contents (tutorial--saved-file))
	      (let ((enable-local-variables :safe))
		(hack-local-variables))
              ;; FIXME?  What we actually want is to ignore dir-locals (?).
              (setq buffer-read-only nil) ; bug#11118
              (goto-char (point-min))
              (setq old-tut-point
                    (string-to-number
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
              (forward-line)
              (setq tutorial--point-before-chkeys
                    (string-to-number
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
              (forward-line)
              (delete-region (point-min) (point))
              (goto-char tutorial--point-before-chkeys)
              (setq tutorial--point-before-chkeys (point-marker)))
          (insert-file-contents (expand-file-name filename tutorial-directory))
	  (let ((enable-local-variables :safe))
	    (hack-local-variables))
          ;; FIXME?  What we actually want is to ignore dir-locals (?).
          (setq buffer-read-only nil) ; bug#11118
          (forward-line)
          (setq tutorial--point-before-chkeys (point-marker)))

	(tutorial--display-changes)

        ;; Clear message:
        (unless dont-ask-for-revert
          (message "") (sit-for 0))


        (if old-tut-file
            ;; Just move to old point in saved tutorial.
            (let ((old-point
                   (if (> 0 old-tut-point)
                       (- old-tut-point)
                     (+ old-tut-point tutorial--point-after-chkeys))))
              (when (< old-point 1)
                (setq old-point 1))
              (goto-char old-point))
          ;; Delete the arch-tag line, so as not to confuse readers.
          (goto-char (point-max))
          (if (search-backward ";;; arch-tag: " nil t)
              (delete-region (point) (point-max)))
          (goto-char (point-min))
          (search-forward "\n<<")
          (beginning-of-line)
          ;; Convert the <<...>> line to the proper [...] line,
          ;; or just delete the <<...>> line if a [...] line follows.
          (cond ((save-excursion
                   (forward-line 1)
                   (looking-at "\\["))
                 (delete-region (point) (progn (forward-line 1) (point))))
                ((looking-at "<<Blank lines inserted.*>>")
                 (replace-match "[Middle of page left blank for didactic purposes.   Text continues below]"))
                (t
                 (looking-at "<<")
                 (replace-match "[")
                 (search-forward ">>")
                 (replace-match "]")))
          (beginning-of-line)
          ;; FIXME: if the window is not tall, and especially if the
          ;; big red "NOTICE: The main purpose..." text has been
          ;; inserted at the start of the buffer, the "type C-v to
          ;; move to the next screen" might not be visible on the
          ;; first screen (n < 0).  How will the novice know what to do?
          (let ((n (- (window-height (selected-window))
                      (count-lines (point-min) (point))
                      6)))
            (if (< n 8)
                (progn
                  ;; For a short gap, we don't need the [...] line,
                  ;; so delete it.
                  (delete-region (point) (progn (end-of-line) (point)))
                  (if (> n 0) (newline n)))
              ;; Some people get confused by the large gap.
              (newline (/ n 2))

              ;; Skip the [...] line (don't delete it).
              (forward-line 1)
              (newline (- n (/ n 2)))))
          (goto-char (point-min)))
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil)))))


;; Below is some attempt to handle language specific strings. These
;; are currently only used in the tutorial.

(defconst lang-strings
  '(("English" .
     ((tut-chgdkey . "%s has been rebound, but you can use %s instead")
      (tut-chgdkey2 . "More")
      (tut-chgdhead . "
 NOTICE: The main purpose of the Emacs tutorial is to teach you
 the most important standard Emacs commands (key bindings).
 However, your Emacs has been customized by changing some of
 these basic editing commands, so it doesn't correspond to the
 tutorial.  We have inserted colored notices where the altered
 commands have been introduced.")
      (tut-chgdhead2 . "More"))))
  "Language specific strings for Emacs.
This is an association list with the keys equal to the strings
that can be returned by `read-language-name'.  The elements in
the list are themselves association lists with keys that are
string ids and values that are the language specific strings.

See `get-lang-string' for more information.")

(defun get-lang-string (lang stringid &optional no-eng-fallback)
  "Get a language specific string for Emacs.
In certain places Emacs can replace a string shown to the user with
a language specific string.  This function retrieves such strings.

LANG is the language specification.  It should be one of those
strings that can be returned by `read-language-name'.  STRINGID
is a symbol that specifies the string to retrieve.

If no string is found for STRINGID in the chosen language then
the English string is returned unless NO-ENG-FALLBACK is non-nil.

See `lang-strings' for more information.

Currently this feature is only used in `help-with-tutorial'."
  (let ((my-lang-strings (assoc lang lang-strings))
        (found-string))
    (when my-lang-strings
      (let ((entry (assoc stringid (cdr my-lang-strings))))
        (when entry
          (setq found-string (cdr entry)))))
    ;; Fallback to English strings
    (unless (or found-string
                no-eng-fallback)
      (setq found-string (get-lang-string "English" stringid t)))
    found-string))

;;(get-lang-string "English" 'tut-chgdkey)

(provide 'tutorial)

;;; tutorial.el ends here
