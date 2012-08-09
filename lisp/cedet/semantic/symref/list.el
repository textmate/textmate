;;; semantic/symref/list.el --- Symref Output List UI.

;; Copyright (C) 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;;
;; Provide a simple user facing API to finding symbol references.
;;
;; This UI is the base of some refactoring tools.  For any refactor,
;; the user will execute `semantic-symref' in a tag.
;; Once that data is collected, the output will be listed in a buffer.
;; In the output buffer, the user can then initiate different
;; refactoring operations.
;;
;; NOTE: Need to add some refactoring tools.

(require 'semantic/symref)
(require 'semantic/complete)
(require 'semantic/senator)
(require 'pulse)

;;; Code:

;;;###autoload
(defun semantic-symref ()
  "Find references to the current tag.
This command uses the currently configured references tool within the
current project to find references to the current tag.  The
references are organized by file and the name of the function
they are used in.
Display the references in `semantic-symref-results-mode'."
  (interactive)
  (semantic-fetch-tags)
  (let ((ct (semantic-current-tag))
	(res nil)
	)
    ;; Must have a tag...
    (when (not ct) (error "Place cursor inside tag to be searched for"))
    ;; Check w/ user.
    (when (not (y-or-n-p (format "Find references for %s? " (semantic-tag-name ct))))
      (error "Quit"))
    ;; Gather results and tags
    (message "Gathering References...")
    (setq res (semantic-symref-find-references-by-name (semantic-tag-name ct)))
    (semantic-symref-produce-list-on-results res (semantic-tag-name ct))))

;;;###autoload
(defun semantic-symref-symbol (sym)
  "Find references to the symbol SYM.
This command uses the currently configured references tool within the
current project to find references to the input SYM.  The
references are organized by file and the name of the function
they are used in.
Display the references in `semantic-symref-results-mode'."
  (interactive (list (semantic-tag-name (semantic-complete-read-tag-buffer-deep
					 "Symrefs for: "))))
  (semantic-fetch-tags)
  (let ((res nil)
	)
    ;; Gather results and tags
    (message "Gathering References...")
    (setq res (semantic-symref-find-references-by-name sym))
    (semantic-symref-produce-list-on-results res sym)))

;;;###autoload
(defun semantic-symref-regexp (sym)
  "Find references to the a symbol regexp SYM.
This command uses the currently configured references tool within the
current project to find references to the input SYM.  The
references are the organized by file and the name of the function
they are used in.
Display the references in`semantic-symref-results-mode'."
  (interactive (list (semantic-tag-name (semantic-complete-read-tag-buffer-deep
					 "Symrefs for: "))))
  (semantic-fetch-tags)
  (let ((res nil)
	)
    ;; Gather results and tags
    (message "Gathering References...")
    (setq res (semantic-symref-find-text sym))
    (semantic-symref-produce-list-on-results res sym)))


(defun semantic-symref-produce-list-on-results (res str)
  "Produce a symref list mode buffer on the results RES."
    (when (not res) (error "No references found"))
    (semantic-symref-result-get-tags res t)
    (message "Gathering References...done")
    ;; Build a references buffer.
    (let ((buff (get-buffer-create
		 (format "*Symref %s" str)))
	  )
      (switch-to-buffer-other-window buff)
      (set-buffer buff)
      (semantic-symref-results-mode res))
    )

;;; RESULTS MODE
;;
(defgroup semantic-symref-results-mode nil
  "Symref Results group."
  :group 'semantic)

(defvar semantic-symref-results-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-i" 'forward-button)
    (define-key km "\M-C-i" 'backward-button)
    (define-key km " " 'push-button)
    (define-key km "-" 'semantic-symref-list-toggle-showing)
    (define-key km "=" 'semantic-symref-list-toggle-showing)
    (define-key km "+" 'semantic-symref-list-toggle-showing)
    (define-key km "n" 'semantic-symref-list-next-line)
    (define-key km "p" 'semantic-symref-list-prev-line)
    (define-key km "q" 'semantic-symref-hide-buffer)
    (define-key km "\C-c\C-e" 'semantic-symref-list-expand-all)
    (define-key km "\C-c\C-r" 'semantic-symref-list-contract-all)
    (define-key km "R" 'semantic-symref-list-rename-open-hits)
    (define-key km "(" 'semantic-symref-list-create-macro-on-open-hit)
    (define-key km "E" 'semantic-symref-list-call-macro-on-open-hits)
    km)
  "Keymap used in `semantic-symref-results-mode'.")

(defvar semantic-symref-list-menu-entries
  (list
   "Symref"
   (semantic-menu-item
    ["Toggle Line Open"
     semantic-symref-list-toggle-showing
     :active t
     :help "Toggle the current line open or closed."
     ])
   (semantic-menu-item
    ["Expand All Entries"
     semantic-symref-list-expand-all
     :active t
     :help "Expand every expandable entry."
     ])
   (semantic-menu-item
    ["Contract All Entries"
     semantic-symref-list-contract-all
     :active t
     :help "Close every expandable entry."
     ])
   (semantic-menu-item
    ["Rename Symbol in Open hits"
     semantic-symref-list-rename-open-hits
     :active t
     :help "Rename the searched for symbol in all hits that are currently open."
     ])
   )
  "Menu entries for the Semantic Symref list mode.")

(defvar semantic-symref-list-menu nil
  "Menu keymap build from `semantic-symref-results-mode'.")

(easy-menu-define semantic-symref-list-menu
  semantic-symref-results-mode-map
  "Symref Mode Menu"
  semantic-symref-list-menu-entries)

(defcustom semantic-symref-auto-expand-results nil
  "Non-nil to expand symref results on buffer creation."
  :group 'semantic-symref
  :type 'boolean)

(defcustom semantic-symref-results-mode-hook nil
  "Hook run when `semantic-symref-results-mode' starts."
  :group 'semantic-symref
  :type 'hook)

(defvar semantic-symref-current-results nil
  "The current results in a results mode buffer.")

(defun semantic-symref-results-mode (results)
  ;; FIXME: Use define-derived-mode.
  "Major-mode for displaying Semantic Symbol Reference RESULTS.
RESULTS is an object of class `semantic-symref-results'."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'semantic-symref-results-mode
        mode-name "Symref"
	)
  (use-local-map semantic-symref-results-mode-map)
  (set (make-local-variable 'semantic-symref-current-results)
       results)
  (semantic-symref-results-dump results)
  (goto-char (point-min))
  (buffer-disable-undo)
  (set (make-local-variable 'font-lock-global-modes) nil)
  (font-lock-mode -1)
  (run-mode-hooks 'semantic-symref-results-mode-hook)
  )

(defun semantic-symref-hide-buffer ()
  "Hide buffer with semantic-symref results."
  (interactive)
  (bury-buffer))

(defcustom semantic-symref-results-summary-function 'semantic-format-tag-prototype
  "*Function to use when creating items in Imenu.
Some useful functions are found in `semantic-format-tag-functions'."
  :group 'semantic-symref
  :type semantic-format-tag-custom-list)

(defun semantic-symref-results-dump (results)
  "Dump the RESULTS into the current buffer."
  ;; Get ready for the insert.
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Insert the contents.
    (let ((lastfile nil))
      (dolist (T (oref results :hit-tags))
	(unless (equal lastfile (semantic-tag-file-name T))
	  (setq lastfile (semantic-tag-file-name T))
	  (insert-button lastfile
			 'mouse-face 'custom-button-pressed-face
			 'action 'semantic-symref-rb-goto-file
			 'tag T)
	  (insert "\n"))
	(insert "  ")
	(insert-button "[+]"
		       'mouse-face 'highlight
		       'face nil
		       'action 'semantic-symref-rb-toggle-expand-tag
		       'tag T
		       'state 'closed)
	(insert " ")
	(insert-button (funcall semantic-symref-results-summary-function
				T nil t)
		       'mouse-face 'custom-button-pressed-face
		       'face nil
		       'action 'semantic-symref-rb-goto-tag
		       'tag T)
	(insert "\n")))
    ;; Auto expand
    (when semantic-symref-auto-expand-results
      (semantic-symref-list-expand-all)))
    ;; Clean up the mess
  (set-buffer-modified-p nil))

;;; Commands for semantic-symref-results
;;
(defun semantic-symref-list-toggle-showing ()
  "Toggle showing the contents below the current line."
  (interactive)
  (beginning-of-line)
  (when (re-search-forward "\\[[-+]\\]" (point-at-eol) t)
    (forward-char -1)
    (push-button)))

(defun semantic-symref-rb-toggle-expand-tag (&optional button)
  "Go to the file specified in the symref results buffer.
BUTTON is the button that was clicked."
  (interactive)
  (let* ((tag (button-get button 'tag))
	 (buff (semantic-tag-buffer tag))
	 (hits (semantic--tag-get-property tag :hit))
	 (state (button-get button 'state))
	 (text nil))
    (cond
     ((eq state 'closed)
      (with-current-buffer buff
	(dolist (H hits)
	  (goto-char (point-min))
	  (forward-line (1- H))
	  (beginning-of-line)
	  (back-to-indentation)
	  (setq text (cons (buffer-substring (point) (point-at-eol)) text)))
	(setq text (nreverse text)))
      (goto-char (button-start button))
      (forward-char 1)
      (let ((inhibit-read-only t))
	(delete-char 1)
	(insert "-")
	(button-put button 'state 'open)
	(save-excursion
	  (end-of-line)
	  (while text
	    (insert "\n")
	    (insert "    ")
	    (insert-button (car text)
			   'mouse-face 'highlight
			   'face nil
			   'action 'semantic-symref-rb-goto-match
			   'tag tag
			   'line (car hits))
	    (setq text (cdr text)
		  hits (cdr hits))))))
     ((eq state 'open)
      (let ((inhibit-read-only t))
	(button-put button 'state 'closed)
	;; Delete the various bits.
	(goto-char (button-start button))
	(forward-char 1)
	(delete-char 1)
	(insert "+")
	(save-excursion
	  (end-of-line)
	  (forward-char 1)
	  (delete-region (point)
			 (save-excursion
			   (forward-char 1)
			   (forward-line (length hits))
			   (point)))))))))

(defun semantic-symref-rb-goto-file (&optional button)
  "Go to the file specified in the symref results buffer.
BUTTON is the button that was clicked."
  (let* ((tag (button-get button 'tag))
	 (buff (semantic-tag-buffer tag))
	 (win (selected-window))
	 )
    (switch-to-buffer-other-window buff)
    (pulse-momentary-highlight-one-line (point))
    (when (eq last-command-event ?\s) (select-window win))
    ))


(defun semantic-symref-rb-goto-tag (&optional button)
  "Go to the file specified in the symref results buffer.
BUTTON is the button that was clicked."
  (interactive)
  (let* ((tag (button-get button 'tag))
	 (buff (semantic-tag-buffer tag))
	 (win (selected-window))
	 )
    (switch-to-buffer-other-window buff)
    (semantic-go-to-tag tag)
    (pulse-momentary-highlight-one-line (point))
    (when (eq last-command-event ?\s) (select-window win))
    )
  )

(defun semantic-symref-rb-goto-match (&optional button)
  "Go to the file specified in the symref results buffer.
BUTTON is the button that was clicked."
  (interactive)
  (let* ((tag (button-get button 'tag))
	 (line (button-get button 'line))
	 (buff (semantic-tag-buffer tag))
	 (win (selected-window))
	 )
    (switch-to-buffer-other-window buff)
    (goto-char (point-min))
    (forward-line (1- line))
    (pulse-momentary-highlight-one-line (point))
    (when (eq last-command-event ?\s) (select-window win))
    )
  )

(defun semantic-symref-list-next-line ()
  "Next line in `semantic-symref-results-mode'."
  (interactive)
  (forward-line 1)
  (back-to-indentation))

(defun semantic-symref-list-prev-line ()
  "Next line in `semantic-symref-results-mode'."
  (interactive)
  (forward-line -1)
  (back-to-indentation))

(defun semantic-symref-list-expand-all ()
  "Expand all the nodes in the current buffer."
  (interactive)
  (let ((start (make-marker)))
    (move-marker start (point))
    (goto-char (point-min))
    (while (re-search-forward "\\[[+]\\]" nil t)
      (semantic-symref-list-toggle-showing))
    ;; Restore position
    (goto-char start)))

(defun semantic-symref-list-contract-all ()
  "Expand all the nodes in the current buffer."
  (interactive)
  (let ((start (make-marker)))
    (move-marker start (point))
    (goto-char (point-min))
    (while (re-search-forward "\\[[-]\\]" nil t)
      (semantic-symref-list-toggle-showing))
    ;; Restore position
    (goto-char start)))

;;; UTILS
;;
;; List mode utils for understanding the current line

(defun semantic-symref-list-on-hit-p ()
  "Return the line number if the cursor is on a buffer line with a hit.
Hits are the line of code from the buffer, not the tag summar or file lines."
  (save-excursion
    (end-of-line)
    (let* ((ol (car (semantic-overlays-at (1- (point)))))) ;; trust this for now
      (when ol (semantic-overlay-get ol 'line)))))


;;; Keyboard Macros on a Hit
;;
;; Record a macro on a hit, and store in a special way for execution later.
(defun semantic-symref-list-create-macro-on-open-hit ()
  "Record a keyboard macro at the location of the hit in the current list.
Under point should be one hit for the active keyword.  Move
cursor to the beginning of that symbol, then record a macro as if
`kmacro-start-macro' was pressed.  Use `kmacro-end-macro',
{kmacro-end-macro} to end the macro, and return to the symbol found list."
  (interactive)
  (let* ((oldsym (oref (oref semantic-symref-current-results
			    :created-by)
		      :searchfor))
	 (ol (save-excursion
	       (end-of-line)
	       (car (semantic-overlays-at (1- (point))))))
	 (tag (when ol (semantic-overlay-get ol 'tag)))
	 (line (when ol (semantic-overlay-get ol 'line))))
    (when (not line)
      (error "Cannot create macro on a non-hit line"))
    ;; Go there, and do something useful.
    (switch-to-buffer-other-window (semantic-tag-buffer tag))
    (goto-char (point-min))
    (forward-line (1- line))
    (when (not (re-search-forward (regexp-quote oldsym) (point-at-eol) t))
      (error "Cannot find hit.  Cannot record macro"))
    (goto-char (match-beginning 0))
    ;; Cursor is now in the right location.  Start recording a macro.
    (kmacro-start-macro nil)
    ;; Notify the user
    (message "Complete with C-x ).  Use E in the symref buffer to call this macro.")))

(defun semantic-symref-list-call-macro-on-open-hits ()
  "Call the most recently created keyboard macro on each hit.
Cursor is placed at the beginning of the symbol found, even if
there is more than one symbol on the current line.  The
previously recorded macro is then executed."
  (interactive)
  (save-window-excursion
    (let ((count (semantic-symref-list-map-open-hits
		  (lambda ()
		    (switch-to-buffer (current-buffer))
		    (kmacro-call-macro nil)))))
      (semantic-symref-list-update-open-hits)
      (message "Executed Macro %d times." count))))

;;; REFACTORING EDITS
;;
;; Utilities and features for refactoring across a list of hits.
;;
(defun semantic-symref-list-rename-open-hits (newname)
  "Rename the discovered symbol references to NEWNAME.
Only renames the locations that are open in the symref list.
Closed items will be skipped."
  (interactive
   (list (read-string "Rename to: "
		      (oref (oref semantic-symref-current-results
				  :created-by)
			    :searchfor))))
  (let ((count (semantic-symref-list-map-open-hits
		(lambda () (replace-match newname nil t)))))
    (semantic-symref-list-update-open-hits)
    (message "Renamed %d occurrences." count)))

;;; REFACTORING UTILITIES
;;
;; Refactoring tools want to operate on only the "good" stuff the
;; user selected.
(defun semantic-symref-list-map-open-hits (function)
  "For every open hit in the symref buffer, perform FUNCTION.
The `match-data' will be set to a successful hit of the searched for symbol.
Return the number of occurrences FUNCTION was operated upon."

  ;; First Pass in this function - a straight rename.
  ;; Second Pass - Allow context specification based on
  ;;               class members. (Not Done)

  (let ((oldsym (oref (oref semantic-symref-current-results
			    :created-by)
		      :searchfor))
	(count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	;; Is this line a "hit" line?
	(let* ((ol (car (semantic-overlays-at (1- (point))))) ;; trust this for now
	       (tag (when ol (semantic-overlay-get ol 'tag)))
	       (line (when ol (semantic-overlay-get ol 'line))))
	  (when line
	    ;; The "line" means we have an open hit.
	    (with-current-buffer (semantic-tag-buffer tag)
	      (goto-char (point-min))
	      (forward-line (1- line))
	      (beginning-of-line)
	      (while (re-search-forward (regexp-quote oldsym) (point-at-eol) t)
		(setq count (1+ count))
		(save-excursion ;; Leave cursor after the matched name.
		  (goto-char (match-beginning 0)) ;; Go to beginning of that sym
		  (funcall function))))))
	;; Go to the next line
	(forward-line 1)
	(end-of-line)))
    count))

(defun semantic-symref-list-update-open-hits ()
  "Update the text for all the open hits in the symref list."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[-\\]" nil t)
      (end-of-line)
      (let* ((ol (car (semantic-overlays-at (1- (point))))) ;; trust this for now
	     (tag (when ol (semantic-overlay-get ol 'tag))))
	;; If there is a tag, then close/open it.
	(when tag
	  (semantic-symref-list-toggle-showing)
	  (semantic-symref-list-toggle-showing))))))

(provide 'semantic/symref/list)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/symref/list"
;; End:

;;; semantic/symref/list.el ends here
