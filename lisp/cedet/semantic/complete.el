;;; semantic/complete.el --- Routines for performing tag completion

;; Copyright (C) 2003-2005, 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax

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
;; Completion of tags by name using tables of semantic generated tags.
;;
;; While it would be a simple matter of flattening all tag known
;; tables to perform completion across them using `all-completions',
;; or `try-completion', that process would be slow.  In particular,
;; when a system database is included in the mix, the potential for a
;; ludicrous number of options becomes apparent.
;;
;; As such, dynamically searching across tables using a prefix,
;; regular expression, or other feature is needed to help find symbols
;; quickly without resorting to "show me every possible option now".
;;
;; In addition, some symbol names will appear in multiple locations.
;; If it is important to distinguish, then a way to provide a choice
;; over these locations is important as well.
;;
;; Beyond brute force offers for completion of plain strings,
;; using the smarts of semantic-analyze to provide reduced lists of
;; symbols, or fancy tabbing to zoom into files to show multiple hits
;; of the same name can be provided.
;;
;;; How it works:
;;
;; There are several parts of any completion engine.  They are:
;;
;; A.  Collection of possible hits
;; B.  Typing or selecting an option
;; C.  Displaying possible unique completions
;; D.  Using the result
;;
;; Here, we will treat each section separately (excluding D)
;; They can then be strung together in user-visible commands to
;; fulfill specific needs.
;;
;; COLLECTORS:
;;
;; A collector is an object which represents the means by which tags
;; to complete on are collected.  It's first job is to find all the
;; tags which are to be completed against.  It can also rename
;; some tags if needed so long as `semantic-tag-clone' is used.
;;
;; Some collectors will gather all tags to complete against first
;; (for in buffer queries, or other small list situations).  It may
;; choose to do a broad search on each completion request.  Built in
;; functionality automatically focuses the cache in as the user types.
;;
;; A collector choosing to create and rename tags could choose a
;; plain name format, a postfix name such as method:class, or a
;; prefix name such as class.method.
;;
;; DISPLAYORS
;;
;; A displayor is in charge if showing the user interesting things
;; about available completions, and can optionally provide a focus.
;; The simplest display just lists all available names in a separate
;; window.  It may even choose to show short names when there are
;; many to choose from, or long names when there are fewer.
;;
;; A complex displayor could opt to help the user 'focus' on some
;; range.  For example, if 4 tags all have the same name, subsequent
;; calls to the displayor may opt to show each tag one at a time in
;; the buffer.  When the user likes one, selection would cause the
;; 'focus' item to be selected.
;;
;; CACHE FORMAT
;;
;; The format of the tag lists used to perform the completions are in
;; semanticdb "find" format, like this:
;;
;; ( ( DBTABLE1 TAG1 TAG2 ...)
;;   ( DBTABLE2 TAG1 TAG2 ...)
;;   ... )
;;
;; INLINE vs MINIBUFFER
;;
;; Two major ways completion is used in Emacs is either through a
;; minibuffer query, or via completion in a normal editing buffer,
;; encompassing some small range of characters.
;;
;; Structure for both types of completion are provided here.
;; `semantic-complete-read-tag-engine' will use the minibuffer.
;; `semantic-complete-inline-tag-engine' will complete text in
;; a buffer.

(eval-when-compile (require 'cl))
(require 'semantic)
(require 'eieio-opt)
(require 'semantic/analyze)
(require 'semantic/ctxt)
(require 'semantic/decorate)
(require 'semantic/format)

(eval-when-compile
  ;; For the semantic-find-tags-for-completion macro.
  (require 'semantic/find))

;;; Code:

(defvar semantic-complete-inline-overlay nil
  "The overlay currently active while completing inline.")

(defun semantic-completion-inline-active-p ()
  "Non-nil if inline completion is active."
  (when (and semantic-complete-inline-overlay
	     (not (semantic-overlay-live-p semantic-complete-inline-overlay)))
    (semantic-overlay-delete semantic-complete-inline-overlay)
    (setq semantic-complete-inline-overlay nil))
  semantic-complete-inline-overlay)

;;; ------------------------------------------------------------
;;; MINIBUFFER or INLINE utils
;;
(defun semantic-completion-text ()
  "Return the text that is currently in the completion buffer.
For a minibuffer prompt, this is the minibuffer text.
For inline completion, this is the text wrapped in the inline completion
overlay."
  (if semantic-complete-inline-overlay
      (semantic-complete-inline-text)
    (minibuffer-contents)))

(defun semantic-completion-delete-text ()
  "Delete the text that is actively being completed.
Presumably if you call this you will insert something new there."
  (if semantic-complete-inline-overlay
      (semantic-complete-inline-delete-text)
    (delete-minibuffer-contents)))

(defun semantic-completion-message (fmt &rest args)
  "Display the string FMT formatted with ARGS at the end of the minibuffer."
  (if semantic-complete-inline-overlay
      (apply 'message fmt args)
    (message (concat (buffer-string) (apply 'format fmt args)))))

;;; ------------------------------------------------------------
;;; MINIBUFFER: Option Selection harnesses
;;
(defvar semantic-completion-collector-engine nil
  "The tag collector for the current completion operation.
Value should be an object of a subclass of
`semantic-completion-engine-abstract'.")

(defvar semantic-completion-display-engine nil
  "The tag display engine for the current completion operation.
Value should be a ... what?")

(defvar semantic-complete-key-map
  (let ((km (make-sparse-keymap)))
    (define-key km " " 'semantic-complete-complete-space)
    (define-key km "\t" 'semantic-complete-complete-tab)
    (define-key km "\C-m" 'semantic-complete-done)
    (define-key km "\C-g" 'abort-recursive-edit)
    (define-key km "\M-n" 'next-history-element)
    (define-key km "\M-p" 'previous-history-element)
    (define-key km "\C-n" 'next-history-element)
    (define-key km "\C-p" 'previous-history-element)
    ;; Add history navigation
    km)
  "Keymap used while completing across a list of tags.")

(defvar semantic-completion-default-history nil
  "Default history variable for any unhistoried prompt.
Keeps STRINGS only in the history.")


(defun semantic-complete-read-tag-engine (collector displayor prompt
						    default-tag initial-input
						    history)
  "Read a semantic tag, and return a tag for the selection.
Argument COLLECTOR is an object which can be used to calculate
a list of possible hits.  See `semantic-completion-collector-engine'
for details on COLLECTOR.
Argument DISPLAYOR is an object used to display a list of possible
completions for a given prefix.  See`semantic-completion-display-engine'
for details on DISPLAYOR.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to story the history in."
  (let* ((semantic-completion-collector-engine collector)
	 (semantic-completion-display-engine displayor)
	 (semantic-complete-active-default nil)
	 (semantic-complete-current-matched-tag nil)
	 (default-as-tag (semantic-complete-default-to-tag default-tag))
	 (default-as-string (when (semantic-tag-p default-as-tag)
			      (semantic-tag-name default-as-tag)))
	 )

    (when default-as-string
      ;; Add this to the prompt.
      ;;
      ;; I really want to add a lookup of the symbol in those
      ;; tags available to the collector and only add it if it
      ;; is available as a possibility, but I'm too lazy right
      ;; now.
      ;;

      ;; @todo - move from () to into the editable area
      (if (string-match ":" prompt)
	  (setq prompt (concat
			(substring prompt 0 (match-beginning 0))
			" (default " default-as-string ")"
			(substring prompt (match-beginning 0))))
	(setq prompt (concat prompt " (" default-as-string "): "))))
    ;;
    ;; Perform the Completion
    ;;
    (unwind-protect
	(read-from-minibuffer prompt
			      initial-input
			      semantic-complete-key-map
			      nil
			      (or history
				  'semantic-completion-default-history)
			      default-tag)
      (semantic-collector-cleanup semantic-completion-collector-engine)
      (semantic-displayor-cleanup semantic-completion-display-engine)
      )
    ;;
    ;; Extract the tag from the completion machinery.
    ;;
    semantic-complete-current-matched-tag
    ))


;;; Util for basic completion prompts
;;

(defvar semantic-complete-active-default nil
  "The current default tag calculated for this prompt.")

(defun semantic-complete-default-to-tag (default)
  "Convert a calculated or passed in DEFAULT into a tag."
  (if (semantic-tag-p default)
      ;; Just return what was passed in.
      (setq semantic-complete-active-default default)
    ;; If none was passed in, guess.
    (if (null default)
	(setq default (semantic-ctxt-current-thing)))
    (if (null default)
	;; Do nothing
	nil
      ;; Turn default into something useful.
      (let ((str
	     (cond
	      ;; Semantic-ctxt-current-symbol will return a list of
	      ;; strings.  Technically, we should use the analyzer to
	      ;; fully extract what we need, but for now, just grab the
	      ;; first string
	      ((and (listp default) (stringp (car default)))
	       (car default))
	      ((stringp default)
	       default)
	      ((symbolp default)
	       (symbol-name default))
	      (t
	       (signal 'wrong-type-argument
		       (list default 'semantic-tag-p)))))
	    (tag nil))
	;; Now that we have that symbol string, look it up using the active
	;; collector.  If we get a match, use it.
	(save-excursion
	  (semantic-collector-calculate-completions
	   semantic-completion-collector-engine
	   str nil))
	;; Do we have the perfect match???
	(let ((ml (semantic-collector-current-exact-match
		   semantic-completion-collector-engine)))
	  (when ml
	    ;; We don't care about uniqueness.  Just guess for convenience
	    (setq tag (semanticdb-find-result-nth-in-buffer ml 0))))
	;; save it
	(setq semantic-complete-active-default tag)
	;; Return it.. .whatever it may be
	tag))))


;;; Prompt Return Value
;;
;; Getting a return value out of this completion prompt is a bit
;; challenging.  The read command returns the string typed in.
;; We need to convert this into a valid tag.  We can exit the minibuffer
;; for different reasons.  If we purposely exit, we must make sure
;; the focused tag is calculated... preferably once.
(defvar semantic-complete-current-matched-tag nil
  "Variable used to pass the tags being matched to the prompt.")

;; semantic-displayor-focus-abstract-child-p is part of the
;; semantic-displayor-focus-abstract class, defined later in this
;; file.
(declare-function semantic-displayor-focus-abstract-child-p "semantic/complete"
		  t t)

(defun semantic-complete-current-match ()
  "Calculate a match from the current completion environment.
Save this in our completion variable.  Make sure that variable
is cleared if any other keypress is made.
Return value can be:
  tag - a single tag that has been matched.
  string - a message to show in the minibuffer."
  ;; Query the environment for an active completion.
  (let ((collector semantic-completion-collector-engine)
	(displayor semantic-completion-display-engine)
	(contents (semantic-completion-text))
	matchlist
	answer)
    (if (string= contents "")
	;; The user wants the defaults!
	(setq answer semantic-complete-active-default)
      ;; This forces a full calculation of completion on CR.
      (save-excursion
	(semantic-collector-calculate-completions collector contents nil))
      (semantic-complete-try-completion)
      (cond
       ;; Input match displayor focus entry
       ((setq answer (semantic-displayor-current-focus displayor))
	;; We have answer, continue
	)
       ;; One match from the collector
       ((setq matchlist (semantic-collector-current-exact-match collector))
	(if (= (semanticdb-find-result-length matchlist) 1)
	    (setq answer (semanticdb-find-result-nth-in-buffer matchlist 0))
	  (if (semantic-displayor-focus-abstract-child-p displayor)
	      ;; For focusing displayors, we can claim this is
	      ;; not unique.  Multiple focuses can choose the correct
	      ;; one.
	      (setq answer "Not Unique")
	    ;; If we don't have a focusing displayor, we need to do something
	    ;; graceful.  First, see if all the matches have the same name.
	    (let ((allsame t)
		  (firstname (semantic-tag-name
			      (car
			       (semanticdb-find-result-nth matchlist 0)))
			     )
		  (cnt 1)
		  (max (semanticdb-find-result-length matchlist)))
	      (while (and allsame (< cnt max))
		(if (not (string=
			  firstname
			  (semantic-tag-name
			   (car
			    (semanticdb-find-result-nth matchlist cnt)))))
		    (setq allsame nil))
		(setq cnt (1+ cnt))
		)
	      ;; Now we know if they are all the same.  If they are, just
	      ;; accept the first, otherwise complain.
	      (if allsame
		  (setq answer (semanticdb-find-result-nth-in-buffer
				matchlist 0))
		(setq answer "Not Unique"))
	      ))))
       ;; No match
       (t
	(setq answer "No Match")))
      )
    ;; Set it into our completion target.
    (when (semantic-tag-p answer)
      (setq semantic-complete-current-matched-tag answer)
      ;; Make sure it is up to date by clearing it if the user dares
      ;; to touch the keyboard.
      (add-hook 'pre-command-hook
		(lambda () (setq semantic-complete-current-matched-tag nil)))
      )
    ;; Return it
    answer
    ))


;;; Keybindings
;;
;; Keys are bound to perform completion using our mechanisms.
;; Do that work here.
(defun semantic-complete-done ()
  "Accept the current input."
  (interactive)
  (let ((ans (semantic-complete-current-match)))
    (if (stringp ans)
	(semantic-completion-message (concat " [" ans "]"))
      (exit-minibuffer)))
  )

(defun semantic-complete-complete-space ()
  "Complete the partial input in the minibuffer."
  (interactive)
  (semantic-complete-do-completion t))

(defun semantic-complete-complete-tab ()
  "Complete the partial input in the minibuffer as far as possible."
  (interactive)
  (semantic-complete-do-completion))

;;; Completion Functions
;;
;; Thees routines are functional entry points to performing completion.
;;
(defun semantic-complete-hack-word-boundaries (original new)
  "Return a string to use for completion.
ORIGINAL is the text in the minibuffer.
NEW is the new text to insert into the minibuffer.
Within the difference bounds of ORIGINAL and NEW, shorten NEW
to the nearest word boundary, and return that."
  (save-match-data
    (let* ((diff (substring new (length original)))
	   (end (string-match "\\>" diff))
	   (start (string-match "\\<" diff)))
      (cond
       ((and start (> start 0))
	;; If start is greater than 0, include only the new
	;; white-space stuff
	(concat original (substring diff 0 start)))
       (end
	(concat original (substring diff 0 end)))
       (t new)))))

(defun semantic-complete-try-completion (&optional partial)
  "Try a completion for the current minibuffer.
If PARTIAL, do partial completion stopping at spaces."
  (let ((comp (semantic-collector-try-completion
               semantic-completion-collector-engine
	       (semantic-completion-text))))
    (cond
     ((null comp)
      (semantic-completion-message " [No Match]")
      (ding)
      )
     ((stringp comp)
      (if (string= (semantic-completion-text) comp)
	  (when partial
	    ;; Minibuffer isn't changing AND the text is not unique.
	    ;; Test for partial completion over a word separator character.
	    ;; If there is one available, use that so that SPC can
	    ;; act like a SPC insert key.
	    (let ((newcomp (semantic-collector-current-whitespace-completion
			    semantic-completion-collector-engine)))
	      (when newcomp
		(semantic-completion-delete-text)
		(insert newcomp))
	      ))
	(when partial
	  (let ((orig (semantic-completion-text)))
	    ;; For partial completion, we stop and step over
	    ;; word boundaries.  Use this nifty function to do
	    ;; that calculation for us.
	    (setq comp
		  (semantic-complete-hack-word-boundaries orig comp))))
	;; Do the replacement.
	(semantic-completion-delete-text)
        (insert comp))
      )
     ((and (listp comp) (semantic-tag-p (car comp)))
      (unless (string= (semantic-completion-text)
		       (semantic-tag-name (car comp)))
        ;; A fully unique completion was available.
        (semantic-completion-delete-text)
        (insert (semantic-tag-name (car comp))))
      ;; The match is complete
      (if (= (length comp) 1)
          (semantic-completion-message " [Complete]")
        (semantic-completion-message " [Complete, but not unique]"))
      )
     (t nil))))

(defun semantic-complete-do-completion (&optional partial inline)
  "Do a completion for the current minibuffer.
If PARTIAL, do partial completion stopping at spaces.
if INLINE, then completion is happening inline in a buffer."
  (let* ((collector semantic-completion-collector-engine)
	 (displayor semantic-completion-display-engine)
	 (contents (semantic-completion-text))
	 (ans nil))

    (save-excursion
      (semantic-collector-calculate-completions collector contents partial))
    (let* ((na (semantic-complete-next-action partial)))
      (cond
       ;; We're all done, but only from a very specific
       ;; area of completion.
       ((eq na 'done)
	(semantic-completion-message " [Complete]")
	(setq ans 'done))
       ;; Perform completion
       ((or (eq na 'complete)
	    (eq na 'complete-whitespace))
	(semantic-complete-try-completion partial)
	(setq ans 'complete))
       ;; We need to display the completions.
       ;; Set the completions into the display engine
       ((or (eq na 'display) (eq na 'displayend))
	(semantic-displayor-set-completions
	 displayor
	 (or
	  ;; For the below - This caused problems for Chong Yidong
	  ;; when experimenting with the completion engine.  I don't
	  ;; remember what the problem was though, and I wasn't sure why
	  ;; the below two lines were there since they obviously added
	  ;; some odd behavior.  -EML
	  ;; (and (not (eq na 'displayend))
	  ;;      (semantic-collector-current-exact-match collector))
	  (semantic-collector-all-completions collector contents))
	 contents)
	;; Ask the displayor to display them.
	(semantic-displayor-show-request displayor))
       ((eq na 'scroll)
	(semantic-displayor-scroll-request displayor)
	)
       ((eq na 'focus)
	(semantic-displayor-focus-next displayor)
	(semantic-displayor-focus-request displayor)
	)
       ((eq na 'empty)
	(semantic-completion-message " [No Match]"))
       (t nil)))
    ans))


;;; ------------------------------------------------------------
;;; INLINE: tag completion harness
;;
;; Unlike the minibuffer, there is no mode nor other traditional
;; means of reading user commands in completion mode.  Instead
;; we use a pre-command-hook to inset in our commands, and to
;; push ourselves out of this mode on alternate keypresses.
(defvar semantic-complete-inline-map
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-i" 'semantic-complete-inline-TAB)
    (define-key km "\M-p" 'semantic-complete-inline-up)
    (define-key km "\M-n" 'semantic-complete-inline-down)
    (define-key km "\C-m" 'semantic-complete-inline-done)
    (define-key km "\C-\M-c" 'semantic-complete-inline-exit)
    (define-key km "\C-g" 'semantic-complete-inline-quit)
    (define-key km "?"
      (lambda () (interactive)
	(describe-variable 'semantic-complete-inline-map)))
    km)
  "Keymap used while performing Semantic inline completion.")

(defface semantic-complete-inline-face
  '((((class color) (background dark))
     (:underline "yellow"))
    (((class color) (background light))
     (:underline "brown")))
  "*Face used to show the region being completed inline.
The face is used in `semantic-complete-inline-tag-engine'."
  :group 'semantic-faces)

(defun semantic-complete-inline-text ()
  "Return the text that is being completed inline.
Similar to `minibuffer-contents' when completing in the minibuffer."
  (let ((s (semantic-overlay-start semantic-complete-inline-overlay))
	(e (semantic-overlay-end semantic-complete-inline-overlay)))
    (if (= s e)
	""
      (buffer-substring-no-properties s e ))))

(defun semantic-complete-inline-delete-text ()
  "Delete the text currently being completed in the current buffer."
  (delete-region
   (semantic-overlay-start semantic-complete-inline-overlay)
   (semantic-overlay-end semantic-complete-inline-overlay)))

(defun semantic-complete-inline-done ()
  "This completion thing is DONE, OR, insert a newline."
  (interactive)
  (let* ((displayor semantic-completion-display-engine)
	 (tag (semantic-displayor-current-focus displayor)))
    (if tag
	(let ((txt (semantic-completion-text)))
	  (insert (substring (semantic-tag-name tag)
			     (length txt)))
	  (semantic-complete-inline-exit))

      ;; Get whatever binding RET usually has.
      (let ((fcn
	     (condition-case nil
		 (lookup-key (current-active-maps) (this-command-keys))
	       (error
		;; I don't know why, but for some reason the above
		;; throws an error sometimes.
		(lookup-key (current-global-map) (this-command-keys))
		))))
	(when fcn
	  (funcall fcn)))
      )))

(defun semantic-complete-inline-quit ()
  "Quit an inline edit."
  (interactive)
  (semantic-complete-inline-exit)
  (keyboard-quit))

(defun semantic-complete-inline-exit ()
  "Exit inline completion mode."
  (interactive)
  ;; Remove this hook FIRST!
  (remove-hook 'pre-command-hook 'semantic-complete-pre-command-hook)

  (condition-case nil
      (progn
	(when semantic-completion-collector-engine
	  (semantic-collector-cleanup semantic-completion-collector-engine))
	(when semantic-completion-display-engine
	  (semantic-displayor-cleanup semantic-completion-display-engine))

	(when semantic-complete-inline-overlay
	  (let ((wc (semantic-overlay-get semantic-complete-inline-overlay
					  'window-config-start))
		(buf (semantic-overlay-buffer semantic-complete-inline-overlay))
		)
	    (semantic-overlay-delete semantic-complete-inline-overlay)
	    (setq semantic-complete-inline-overlay nil)
	    ;; DONT restore the window configuration if we just
	    ;; switched windows!
	    (when (eq buf (current-buffer))
	      (set-window-configuration wc))
	    ))

	(setq semantic-completion-collector-engine nil
	      semantic-completion-display-engine nil))
    (error nil))

  ;; Remove this hook LAST!!!
  ;; This will force us back through this function if there was
  ;; some sort of error above.
  (remove-hook 'post-command-hook 'semantic-complete-post-command-hook)

  ;;(message "Exiting inline completion.")
  )

(defun semantic-complete-pre-command-hook ()
  "Used to redefine what commands are being run while completing.
When installed as a `pre-command-hook' the special keymap
`semantic-complete-inline-map' is queried to replace commands normally run.
Commands which edit what is in the region of interest operate normally.
Commands which would take us out of the region of interest, or our
quit hook, will exit this completion mode."
  (let ((fcn (lookup-key semantic-complete-inline-map
			 (this-command-keys) nil)))
    (cond ((commandp fcn)
	   (setq this-command fcn))
	  (t nil)))
  )

(defun semantic-complete-post-command-hook ()
  "Used to determine if we need to exit inline completion mode.
If completion mode is active, check to see if we are within
the bounds of `semantic-complete-inline-overlay', or within
a reasonable distance."
  (condition-case nil
      ;; Exit if something bad happened.
      (if (not semantic-complete-inline-overlay)
	  (progn
	    ;;(message "Inline Hook installed, but overlay deleted.")
	    (semantic-complete-inline-exit))
	;; Exit if commands caused us to exit the area of interest
	(let ((s (semantic-overlay-start semantic-complete-inline-overlay))
	      (e (semantic-overlay-end semantic-complete-inline-overlay))
	      (b (semantic-overlay-buffer semantic-complete-inline-overlay))
	      (txt nil)
	      )
	  (cond
	   ;; EXIT when we are no longer in a good place.
	   ((or (not (eq b (current-buffer)))
		(< (point) s)
		(> (point) e))
	    ;;(message "Exit: %S %S %S" s e (point))
	    (semantic-complete-inline-exit)
	    )
	   ;; Exit if the user typed in a character that is not part
	   ;; of the symbol being completed.
	   ((and (setq txt (semantic-completion-text))
		 (not (string= txt ""))
		 (and (/= (point) s)
		      (save-excursion
			(forward-char -1)
			(not (looking-at "\\(\\w\\|\\s_\\)")))))
	    ;;(message "Non symbol character.")
	    (semantic-complete-inline-exit))
	   ((lookup-key semantic-complete-inline-map
			(this-command-keys) nil)
	    ;; If the last command was one of our completion commands,
	    ;; then do nothing.
	    nil
	    )
	   (t
	    ;; Else, show completions now
	    (semantic-complete-inline-force-display)

	    ))))
    ;; If something goes terribly wrong, clean up after ourselves.
    (error (semantic-complete-inline-exit))))

(defun semantic-complete-inline-force-display ()
  "Force the display of whatever the current completions are.
DO NOT CALL THIS IF THE INLINE COMPLETION ENGINE IS NOT ACTIVE."
  (condition-case e
      (save-excursion
	(let ((collector semantic-completion-collector-engine)
	      (displayor semantic-completion-display-engine)
	      (contents (semantic-completion-text)))
	  (when collector
	    (semantic-collector-calculate-completions
	     collector contents nil)
	    (semantic-displayor-set-completions
	     displayor
	     (semantic-collector-all-completions collector contents)
	     contents)
	    ;; Ask the displayor to display them.
	    (semantic-displayor-show-request displayor))
	  ))
    (error (message "Bug Showing Completions: %S" e))))

(defun semantic-complete-inline-tag-engine
  (collector displayor buffer start end)
  "Perform completion based on semantic tags in a buffer.
Argument COLLECTOR is an object which can be used to calculate
a list of possible hits.  See `semantic-completion-collector-engine'
for details on COLLECTOR.
Argument DISPLAYOR is an object used to display a list of possible
completions for a given prefix.  See`semantic-completion-display-engine'
for details on DISPLAYOR.
BUFFER is the buffer in which completion will take place.
START is a location for the start of the full symbol.
If the symbol being completed is \"foo.ba\", then START
is on the \"f\" character.
END is at the end of the current symbol being completed."
  ;; Set us up for doing completion
  (setq semantic-completion-collector-engine collector
	semantic-completion-display-engine displayor)
  ;; Create an overlay
  (setq semantic-complete-inline-overlay
	(semantic-make-overlay start end buffer nil t))
  (semantic-overlay-put semantic-complete-inline-overlay
			'face
			'semantic-complete-inline-face)
  (semantic-overlay-put semantic-complete-inline-overlay
			'window-config-start
			(current-window-configuration))
  ;; Install our command hooks
  (add-hook 'pre-command-hook 'semantic-complete-pre-command-hook)
  (add-hook 'post-command-hook 'semantic-complete-post-command-hook)
  ;; Go!
  (semantic-complete-inline-force-display)
  )

;;; Inline Completion Keymap Functions
;;
(defun semantic-complete-inline-TAB ()
  "Perform inline completion."
  (interactive)
  (let ((cmpl (semantic-complete-do-completion nil t)))
    (cond
     ((eq cmpl 'complete)
      (semantic-complete-inline-force-display))
     ((eq cmpl 'done)
      (semantic-complete-inline-done))
     ))
  )

(defun semantic-complete-inline-down()
  "Focus forwards through the displayor."
  (interactive)
  (let ((displayor semantic-completion-display-engine))
    (semantic-displayor-focus-next    displayor)
    (semantic-displayor-focus-request displayor)
    ))

(defun semantic-complete-inline-up ()
  "Focus backwards through the displayor."
  (interactive)
  (let ((displayor semantic-completion-display-engine))
    (semantic-displayor-focus-previous displayor)
    (semantic-displayor-focus-request  displayor)
    ))


;;; ------------------------------------------------------------
;;; Interactions between collection and displaying
;;
;; Functional routines used to help collectors communicate with
;; the current displayor, or for the previous section.

(defun semantic-complete-next-action (partial)
  "Determine what the next completion action should be.
PARTIAL is non-nil if we are doing partial completion.
First, the collector can determine if we should perform a completion or not.
If there is nothing to complete, then the displayor determines if we are
to show a completion list, scroll, or perhaps do a focus (if it is capable.)
Expected return values are:
  done -> We have a singular match
  empty -> There are no matches to the current text
  complete -> Perform a completion action
  complete-whitespace -> Complete next whitespace type character.
  display -> Show the list of completions
  scroll -> The completions have been shown, and the user keeps hitting
            the complete button.  If possible, scroll the completions
  focus -> The displayor knows how to shift focus among possible completions.
           Let it do that.
  displayend -> Whatever options the displayor had for repeating options, there
           are none left.  Try something new."
  (let ((ans1 (semantic-collector-next-action
		semantic-completion-collector-engine
		partial))
	(ans2 (semantic-displayor-next-action
		semantic-completion-display-engine))
	)
    (cond
     ;; No collector answer, use displayor answer.
     ((not ans1)
      ans2)
     ;; Displayor selection of 'scroll, 'display, or 'focus trumps
     ;; 'done
     ((and (eq ans1 'done) ans2)
      ans2)
     ;; Use ans1 when we have it.
     (t
      ans1))))



;;; ------------------------------------------------------------
;;; Collection Engines
;;
;; Collection engines can scan tags from the current environment and
;; provide lists of possible completions.
;;
;; General features of the abstract collector:
;; * Cache completion lists between uses
;; * Cache itself per buffer.  Handle reparse hooks
;;
;; Key Interface Functions to implement:
;; * semantic-collector-next-action
;; * semantic-collector-calculate-completions
;; * semantic-collector-try-completion
;; * semantic-collector-all-completions

(defvar semantic-collector-per-buffer-list nil
  "List of collectors active in this buffer.")
(make-variable-buffer-local 'semantic-collector-per-buffer-list)

(defvar semantic-collector-list nil
  "List of global collectors active this session.")

(defclass semantic-collector-abstract ()
  ((buffer :initarg :buffer
	   :type buffer
	   :documentation "Originating buffer for this collector.
Some collectors use a given buffer as a starting place while looking up
tags.")
   (cache :initform nil
	  :type (or null semanticdb-find-result-with-nil)
	  :documentation "Cache of tags.
These tags are re-used during a completion session.
Sometimes these tags are cached between completion sessions.")
   (last-all-completions :initarg nil
			 :type semanticdb-find-result-with-nil
			 :documentation "Last result of `all-completions'.
This result can be used for refined completions as `last-prefix' gets
closer to a specific result.")
   (last-prefix :type string
		:protection :protected
		:documentation "The last queried prefix.
This prefix can be used to cache intermediate completion offers.
making the action of homing in on a token faster.")
   (last-completion :type (or null string)
		    :documentation "The last calculated completion.
This completion is calculated and saved for future use.")
   (last-whitespace-completion :type (or null string)
			       :documentation "The last whitespace completion.
For partial completion, SPC will disambiguate over whitespace type
characters.  This is the last calculated version.")
   (current-exact-match :type list
			:protection :protected
			:documentation "The list of matched tags.
When tokens are matched, they are added to this list.")
   )
  "Root class for completion engines.
The baseclass provides basic functionality for interacting with
a completion displayor object, and tracking the current progress
of a completion."
  :abstract t)

(defmethod semantic-collector-cleanup ((obj semantic-collector-abstract))
  "Clean up any mess this collector may have."
  nil)

(defmethod semantic-collector-next-action
  ((obj semantic-collector-abstract) partial)
  "What should we do next?  OBJ can predict a next good action.
PARTIAL indicates if we are doing a partial completion."
  (if (and (slot-boundp obj 'last-completion)
	   (string= (semantic-completion-text) (oref obj last-completion)))
      (let* ((cem (semantic-collector-current-exact-match obj))
	     (cemlen (semanticdb-find-result-length cem))
	     (cac (semantic-collector-all-completions
		   obj (semantic-completion-text)))
	     (caclen (semanticdb-find-result-length cac)))
	(cond ((and cem (= cemlen 1)
		    cac (> caclen 1)
		    (eq last-command this-command))
	       ;; Defer to the displayor...
	       nil)
	      ((and cem (= cemlen 1))
	       'done)
	      ((and (not cem) (not cac))
	       'empty)
	      ((and partial (semantic-collector-try-completion-whitespace
			     obj (semantic-completion-text)))
	       'complete-whitespace)))
    'complete))

(defmethod semantic-collector-last-prefix= ((obj semantic-collector-abstract)
					    last-prefix)
  "Return non-nil if OBJ's prefix matches PREFIX."
  (and (slot-boundp obj 'last-prefix)
       (string= (oref obj last-prefix) last-prefix)))

(defmethod semantic-collector-get-cache ((obj semantic-collector-abstract))
  "Get the raw cache of tags for completion.
Calculate the cache if there isn't one."
  (or (oref obj cache)
      (semantic-collector-calculate-cache obj)))

(defmethod semantic-collector-calculate-completions-raw
  ((obj semantic-collector-abstract) prefix completionlist)
  "Calculate the completions for prefix from completionlist.
Output must be in semanticdb Find result format."
  ;; Must output in semanticdb format
  (let ((table (with-current-buffer (oref obj buffer)
		 semanticdb-current-table))
	(result (semantic-find-tags-for-completion
		 prefix
		 ;; To do this kind of search with a pre-built completion
		 ;; list, we need to strip it first.
		 (semanticdb-strip-find-results completionlist)))
	)
    (if result
	(list (cons table result)))))

(defmethod semantic-collector-calculate-completions
  ((obj semantic-collector-abstract) prefix partial)
  "Calculate completions for prefix as setup for other queries."
  (let* ((case-fold-search semantic-case-fold)
	 (same-prefix-p (semantic-collector-last-prefix= obj prefix))
	 (completionlist
	  (if (or same-prefix-p
		  (and (slot-boundp obj 'last-prefix)
		       (eq (compare-strings (oref obj last-prefix) 0 nil
					    prefix 0 (length prefix))
			   t)))
	      ;; New prefix is subset of old prefix
	      (oref obj last-all-completions)
	    (semantic-collector-get-cache obj)))
	 ;; Get the result
	 (answer (if same-prefix-p
		     completionlist
		   (semantic-collector-calculate-completions-raw
		    obj prefix completionlist))
		 )
	 (completion nil)
	 (complete-not-uniq nil)
	 )
    ;;(semanticdb-find-result-test answer)
    (when (not same-prefix-p)
      ;; Save results if it is interesting and beneficial
      (oset obj last-prefix prefix)
      (oset obj last-all-completions answer))
    ;; Now calculate the completion.
    (setq completion (try-completion
		      prefix
		      (semanticdb-strip-find-results answer)))
    (oset obj last-whitespace-completion nil)
    (oset obj current-exact-match nil)
    ;; Only do this if a completion was found.  Letting a nil in
    ;; could cause a full semanticdb search by accident.
    (when completion
      (oset obj last-completion
	    (cond
	     ;; Unique match in AC.  Last completion is a match.
	     ;; Also set the current-exact-match.
	     ((eq completion t)
	      (oset obj current-exact-match answer)
	      prefix)
	     ;; It may be complete (a symbol) but still not unique.
	     ;; We can capture a match
	     ((setq complete-not-uniq
		    (semanticdb-find-tags-by-name
		     prefix
		     answer))
	      (oset obj current-exact-match
		    complete-not-uniq)
	      prefix
	      )
	     ;; Non unique match, return the string that handles
	     ;; completion
	     (t (or completion prefix))
	     )))
    ))

(defmethod semantic-collector-try-completion-whitespace
  ((obj semantic-collector-abstract) prefix)
  "For OBJ, do whitespace completion based on PREFIX.
This implies that if there are two completions, one matching
the test \"prefix\\>\", and one not, the one matching the full
word version of PREFIX will be chosen, and that text returned.
This function requires that `semantic-collector-calculate-completions'
has been run first."
  (let* ((ac (semantic-collector-all-completions obj prefix))
	 (matchme (concat "^" prefix "\\>"))
	 (compare (semanticdb-find-tags-by-name-regexp matchme ac))
	 (numtag (semanticdb-find-result-length compare))
	 )
    (if compare
	(let* ((idx 0)
	       (cutlen (1+ (length prefix)))
	       (twws (semanticdb-find-result-nth compare idx)))
	  ;; Is our tag with whitespace a match that has whitespace
	  ;; after it, or just an already complete symbol?
	  (while (and (< idx numtag)
		      (< (length (semantic-tag-name (car twws))) cutlen))
	    (setq idx (1+ idx)
		  twws (semanticdb-find-result-nth compare idx)))
	  (when (and twws (car-safe twws))
	    ;; If COMPARE has succeeded, then we should take the very
	    ;; first match, and extend prefix by one character.
	    (oset obj last-whitespace-completion
		  (substring (semantic-tag-name (car twws))
			     0 cutlen))))
      )))


(defmethod semantic-collector-current-exact-match ((obj semantic-collector-abstract))
  "Return the active valid MATCH from the semantic collector.
For now, just return the first element from our list of available
matches.  For semanticdb based results, make sure the file is loaded
into a buffer."
  (when (slot-boundp obj 'current-exact-match)
    (oref obj current-exact-match)))

(defmethod semantic-collector-current-whitespace-completion ((obj semantic-collector-abstract))
  "Return the active whitespace completion value."
  (when (slot-boundp obj 'last-whitespace-completion)
    (oref obj last-whitespace-completion)))

(defmethod semantic-collector-get-match ((obj semantic-collector-abstract))
  "Return the active valid MATCH from the semantic collector.
For now, just return the first element from our list of available
matches.  For semanticdb based results, make sure the file is loaded
into a buffer."
  (when (slot-boundp obj 'current-exact-match)
    (semanticdb-find-result-nth-in-buffer (oref obj current-exact-match) 0)))

(defmethod semantic-collector-all-completions
  ((obj semantic-collector-abstract) prefix)
  "For OBJ, retrieve all completions matching PREFIX.
The returned list consists of all the tags currently
matching PREFIX."
  (when (slot-boundp obj 'last-all-completions)
    (oref obj last-all-completions)))

(defmethod semantic-collector-try-completion
  ((obj semantic-collector-abstract) prefix)
  "For OBJ, attempt to match PREFIX.
See `try-completion' for details on how this works.
Return nil for no match.
Return a string for a partial match.
For a unique match of PREFIX, return the list of all tags
with that name."
  (if (slot-boundp obj 'last-completion)
      (oref obj last-completion)))

(defmethod semantic-collector-calculate-cache
  ((obj semantic-collector-abstract))
  "Calculate the completion cache for OBJ."
  nil
  )

(defmethod semantic-collector-flush ((this semantic-collector-abstract))
  "Flush THIS collector object, clearing any caches and prefix."
  (oset this cache nil)
  (slot-makeunbound this 'last-prefix)
  (slot-makeunbound this 'last-completion)
  (slot-makeunbound this 'last-all-completions)
  (slot-makeunbound this 'current-exact-match)
  )

;;; PER BUFFER
;;
(defclass semantic-collector-buffer-abstract (semantic-collector-abstract)
  ()
  "Root class for per-buffer completion engines.
These collectors track themselves on a per-buffer basis."
  :abstract t)

(defmethod constructor :STATIC ((this semantic-collector-buffer-abstract)
				newname &rest fields)
  "Reuse previously created objects of this type in buffer."
  (let ((old nil)
	(bl semantic-collector-per-buffer-list))
    (while (and bl (null old))
      (if (eq (object-class (car bl)) this)
	  (setq old (car bl))))
    (unless old
      (let ((new (call-next-method)))
	(add-to-list 'semantic-collector-per-buffer-list new)
	(setq old new)))
    (slot-makeunbound old 'last-completion)
    (slot-makeunbound old 'last-prefix)
    (slot-makeunbound old 'current-exact-match)
    old))

;; Buffer specific collectors should flush themselves
(defun semantic-collector-buffer-flush (newcache)
  "Flush all buffer collector objects.
NEWCACHE is the new tag table, but we ignore it."
  (condition-case nil
      (let ((l semantic-collector-per-buffer-list))
	(while l
	  (if (car l) (semantic-collector-flush (car l)))
	  (setq l (cdr l))))
    (error nil)))

(add-hook 'semantic-after-toplevel-cache-change-hook
	  'semantic-collector-buffer-flush)

;;; DEEP BUFFER SPECIFIC COMPLETION
;;
(defclass semantic-collector-buffer-deep
  (semantic-collector-buffer-abstract)
  ()
  "Completion engine for tags in the current buffer.
When searching for a tag, uses semantic  deep searche functions.
Basics search only in the current buffer.")

(defmethod semantic-collector-calculate-cache
  ((obj semantic-collector-buffer-deep))
  "Calculate the completion cache for OBJ.
Uses `semantic-flatten-tags-table'"
  (oset obj cache
	;; Must create it in SEMANTICDB find format.
	;; ( ( DBTABLE TAG TAG ... ) ... )
	(list
	 (cons semanticdb-current-table
	       (semantic-flatten-tags-table (oref obj buffer))))))

;;; PROJECT SPECIFIC COMPLETION
;;
(defclass semantic-collector-project-abstract (semantic-collector-abstract)
  ((path :initarg :path
	 :initform nil
	 :documentation "List of database tables to search.
At creation time, it can be anything accepted by
`semanticdb-find-translate-path' as a PATH argument.")
   )
  "Root class for project wide completion engines.
Uses semanticdb for searching all tags in the current project."
  :abstract t)

;;; Project Search
(defclass semantic-collector-project (semantic-collector-project-abstract)
  ()
  "Completion engine for tags in a project.")


(defmethod semantic-collector-calculate-completions-raw
  ((obj semantic-collector-project) prefix completionlist)
  "Calculate the completions for prefix from completionlist."
  (semanticdb-find-tags-for-completion prefix (oref obj path)))

;;; Brutish Project search
(defclass semantic-collector-project-brutish (semantic-collector-project-abstract)
  ()
  "Completion engine for tags in a project.")

(declare-function semanticdb-brute-deep-find-tags-for-completion
		  "semantic/db-find")

(defmethod semantic-collector-calculate-completions-raw
  ((obj semantic-collector-project-brutish) prefix completionlist)
  "Calculate the completions for prefix from completionlist."
  (require 'semantic/db-find)
  (semanticdb-brute-deep-find-tags-for-completion prefix (oref obj path)))

;;; Current Datatype member search.
(defclass semantic-collector-local-members (semantic-collector-project-abstract)
  ((scope :initform nil
	  :type (or null semantic-scope-cache)
	  :documentation
	  "The scope the local members are being completed from."))
  "Completion engine for tags in a project.")

(defmethod semantic-collector-calculate-completions-raw
  ((obj semantic-collector-local-members) prefix completionlist)
  "Calculate the completions for prefix from completionlist."
  (let* ((scope (or (oref obj scope)
		    (oset obj scope (semantic-calculate-scope))))
	 (localstuff (oref scope scope)))
    (list
     (cons
      (oref scope :table)
      (semantic-find-tags-for-completion prefix localstuff)))))
    ;(semanticdb-brute-deep-find-tags-for-completion prefix (oref obj path))))

;;; Smart completion collector
(defclass semantic-collector-analyze-completions (semantic-collector-abstract)
  ((context :initarg :context
	    :type semantic-analyze-context
	    :documentation "An analysis context.
Specifies some context location from whence completion lists will be drawn."
	    )
   (first-pass-completions :type list
			   :documentation "List of valid completion tags.
This list of tags is generated when completion starts.  All searches
derive from this list.")
   )
  "Completion engine that uses the context analyzer to provide options.
The only options available for completion are those which can be logically
inserted into the current context.")

(defmethod semantic-collector-calculate-completions-raw
  ((obj semantic-collector-analyze-completions) prefix completionlist)
  "calculate the completions for prefix from completionlist."
  ;; if there are no completions yet, calculate them.
  (if (not (slot-boundp obj 'first-pass-completions))
      (oset obj first-pass-completions
	    (semantic-analyze-possible-completions (oref obj context))))
  ;; search our cached completion list.  make it look like a semanticdb
  ;; results type.
  (list (cons (with-current-buffer (oref (oref obj context) buffer)
		semanticdb-current-table)
	      (semantic-find-tags-for-completion
	       prefix
	       (oref obj first-pass-completions)))))


;;; ------------------------------------------------------------
;;; Tag List Display Engines
;;
;; A typical displayor accepts a pre-determined list of completions
;; generated by a collector.  This format is in semanticdb search
;; form.  This vaguely standard form is a bit challenging to navigate
;; because the tags do not contain buffer info, but the file associated
;; with the tags precedes the tag in the list.
;;
;; Basic displayors don't care, and can strip the results.
;; Advanced highlighting displayors need to know when they need
;; to load a file so that the tag in question can be highlighted.
;;
;; Key interface methods to a displayor are:
;; * semantic-displayor-next-action
;; * semantic-displayor-set-completions
;; * semantic-displayor-current-focus
;; * semantic-displayor-show-request
;; * semantic-displayor-scroll-request
;; * semantic-displayor-focus-request

(defclass semantic-displayor-abstract ()
  ((table :type (or null semanticdb-find-result-with-nil)
	  :initform nil
	  :protection :protected
	  :documentation "List of tags this displayor is showing.")
   (last-prefix :type string
		:protection :protected
		:documentation "Prefix associated with slot `table'")
   )
  "Abstract displayor baseclass.
Manages the display of some number of tags.
Provides the basics for a displayor, including interacting with
a collector, and tracking tables of completion to display."
  :abstract t)

(defmethod semantic-displayor-cleanup ((obj semantic-displayor-abstract))
  "Clean up any mess this displayor may have."
  nil)

(defmethod semantic-displayor-next-action ((obj semantic-displayor-abstract))
  "The next action to take on the minibuffer related to display."
  (if (and (slot-boundp obj 'last-prefix)
	   (string= (oref obj last-prefix) (semantic-completion-text))
	   (eq last-command this-command))
      'scroll
    'display))

(defmethod semantic-displayor-set-completions ((obj semantic-displayor-abstract)
					       table prefix)
  "Set the list of tags to be completed over to TABLE."
  (oset obj table table)
  (oset obj last-prefix prefix))

(defmethod semantic-displayor-show-request ((obj semantic-displayor-abstract))
  "A request to show the current tags table."
  (ding))

(defmethod semantic-displayor-focus-request ((obj semantic-displayor-abstract))
  "A request to for the displayor to focus on some tag option."
  (ding))

(defmethod semantic-displayor-scroll-request ((obj semantic-displayor-abstract))
  "A request to for the displayor to scroll the completion list (if needed)."
  (scroll-other-window))

(defmethod semantic-displayor-focus-previous ((obj semantic-displayor-abstract))
  "Set the current focus to the previous item."
  nil)

(defmethod semantic-displayor-focus-next ((obj semantic-displayor-abstract))
  "Set the current focus to the next item."
  nil)

(defmethod semantic-displayor-current-focus ((obj semantic-displayor-abstract))
  "Return a single tag currently in focus.
This object type doesn't do focus, so will never have a focus object."
  nil)

;; Traditional displayor
(defcustom semantic-completion-displayor-format-tag-function
  #'semantic-format-tag-name
  "*A Tag format function to use when showing completions."
  :group 'semantic
  :type semantic-format-tag-custom-list)

(defclass semantic-displayor-traditional (semantic-displayor-abstract)
  ()
  "Display options in *Completions* buffer.
Traditional display mechanism for a list of possible completions.
Completions are showin in a new buffer and listed with the ability
to click on the items to aid in completion.")

(defmethod semantic-displayor-show-request ((obj semantic-displayor-traditional))
  "A request to show the current tags table."

  ;; NOTE TO SELF.  Find the character to type next, and emphasize it.

  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list
     (mapcar semantic-completion-displayor-format-tag-function
	     (semanticdb-strip-find-results (oref obj table))))
    )
  )

;;; Abstract baseclass for any displayor which supports focus
(defclass semantic-displayor-focus-abstract (semantic-displayor-abstract)
  ((focus :type number
	  :protection :protected
	  :documentation "A tag index from `table' which has focus.
Multiple calls to the display function can choose to focus on a
given tag, by highlighting its location.")
   (find-file-focus
    :allocation :class
    :initform nil
    :documentation
    "Non-nil if focusing requires a tag's buffer be in memory.")
   )
  "Abstract displayor supporting `focus'.
A displayor which has the ability to focus in on one tag.
Focusing is a way of differentiating among multiple tags
which have the same name."
  :abstract t)

(defmethod semantic-displayor-next-action ((obj semantic-displayor-focus-abstract))
  "The next action to take on the minibuffer related to display."
  (if (and (slot-boundp obj 'last-prefix)
	   (string= (oref obj last-prefix) (semantic-completion-text))
	   (eq last-command this-command))
      (if (and
	   (slot-boundp obj 'focus)
	   (slot-boundp obj 'table)
	   (<= (semanticdb-find-result-length (oref obj table))
	       (1+ (oref obj focus))))
	  ;; We are at the end of the focus road.
	  'displayend
	;; Focus on some item.
	'focus)
    'display))

(defmethod semantic-displayor-set-completions ((obj semantic-displayor-focus-abstract)
					       table prefix)
  "Set the list of tags to be completed over to TABLE."
  (call-next-method)
  (slot-makeunbound obj 'focus))

(defmethod semantic-displayor-focus-previous ((obj semantic-displayor-focus-abstract))
  "Set the current focus to the previous item.
Not meaningful return value."
  (when (and (slot-boundp obj 'table) (oref obj table))
    (with-slots (table) obj
      (if (or (not (slot-boundp obj 'focus))
	      (<= (oref obj focus) 0))
	  (oset obj focus (1- (semanticdb-find-result-length table)))
	(oset obj focus (1- (oref obj focus)))
	)
      )))

(defmethod semantic-displayor-focus-next ((obj semantic-displayor-focus-abstract))
  "Set the current focus to the next item.
Not meaningful return value."
  (when (and (slot-boundp obj 'table) (oref obj table))
    (with-slots (table) obj
      (if (not (slot-boundp obj 'focus))
	  (oset obj focus 0)
	(oset obj focus (1+ (oref obj focus)))
	)
      (if (<= (semanticdb-find-result-length table) (oref obj focus))
	  (oset obj focus 0))
      )))

(defmethod semantic-displayor-focus-tag ((obj semantic-displayor-focus-abstract))
  "Return the next tag OBJ should focus on."
  (when (and (slot-boundp obj 'table) (oref obj table))
    (with-slots (table) obj
      (semanticdb-find-result-nth table (oref obj focus)))))

(defmethod semantic-displayor-current-focus ((obj semantic-displayor-focus-abstract))
  "Return the tag currently in focus, or call parent method."
  (if (and (slot-boundp obj 'focus)
	   (slot-boundp obj 'table)
	   ;; Only return the current focus IFF the minibuffer reflects
	   ;; the list this focus was derived from.
	   (slot-boundp obj 'last-prefix)
	   (string= (semantic-completion-text) (oref obj last-prefix))
	   )
      ;; We need to focus
      (if (oref obj find-file-focus)
	  (semanticdb-find-result-nth-in-buffer (oref obj table) (oref obj focus))
	;; result-nth returns a cons with car being the tag, and cdr the
	;; database.
	(car (semanticdb-find-result-nth (oref obj table) (oref obj focus))))
    ;; Do whatever
    (call-next-method)))

;;; Simple displayor which performs traditional display completion,
;; and also focuses with highlighting.
(defclass semantic-displayor-traditional-with-focus-highlight
  (semantic-displayor-focus-abstract semantic-displayor-traditional)
  ((find-file-focus :initform t))
  "Display completions in *Completions* buffer, with focus highlight.
A traditional displayor which can focus on a tag by showing it.
Same as `semantic-displayor-traditional', but with selection between
multiple tags with the same name done by 'focusing' on the source
location of the different tags to differentiate them.")

(defmethod semantic-displayor-focus-request
  ((obj semantic-displayor-traditional-with-focus-highlight))
  "Focus in on possible tag completions.
Focus is performed by cycling through the tags and highlighting
one in the source buffer."
  (let* ((tablelength (semanticdb-find-result-length (oref obj table)))
	 (focus (semantic-displayor-focus-tag obj))
	 ;; Raw tag info.
	 (rtag (car focus))
	 (rtable (cdr focus))
	 ;; Normalize
	 (nt (semanticdb-normalize-one-tag rtable rtag))
	 (tag (cdr nt))
	 (table (car nt))
	)
    ;; If we fail to normalize, reset.
    (when (not tag) (setq table rtable tag rtag))
    ;; Do the focus.
    (let ((buf (or (semantic-tag-buffer tag)
		   (and table (semanticdb-get-buffer table)))))
      ;; If no buffer is provided, then we can make up a summary buffer.
      (when (not buf)
	(with-current-buffer (get-buffer-create "*Completion Focus*")
	  (erase-buffer)
	  (insert "Focus on tag: \n")
	  (insert (semantic-format-tag-summarize tag nil t) "\n\n")
	  (when table
	    (insert "From table: \n")
	    (insert (object-name table) "\n\n"))
	  (when buf
	    (insert "In buffer: \n\n")
	    (insert (format "%S" buf)))
	  (setq buf (current-buffer))))
      ;; Show the tag in the buffer.
      (if (get-buffer-window buf)
	  (select-window (get-buffer-window buf))
	(switch-to-buffer-other-window buf t)
	(select-window (get-buffer-window buf)))
      ;; Now do some positioning
      (unwind-protect
	  (if (semantic-tag-with-position-p tag)
	      ;; Full tag positional information available
	      (progn
		(goto-char (semantic-tag-start tag))
		;; This avoids a dangerous problem if we just loaded a tag
		;; from a file, but the original position was not updated
		;; in the TAG variable we are currently using.
		(semantic-momentary-highlight-tag (semantic-current-tag))
		))
	(select-window (minibuffer-window)))
      ;; Calculate text difference between contents and the focus item.
      (let* ((mbc (semantic-completion-text))
	     (ftn (semantic-tag-name tag))
	     (diff (substring ftn (length mbc))))
	(semantic-completion-message
	 (format "%s [%d of %d matches]" diff (1+ (oref obj focus)) tablelength)))
      )))


;;; Tooltip completion lister
;;
;; Written and contributed by Masatake YAMATO <jet@gyve.org>
;;
;; Modified by Eric Ludlam for
;; * Safe compatibility for tooltip free systems.
;; * Don't use 'avoid package for tooltip positioning.

(defclass semantic-displayor-tooltip (semantic-displayor-traditional)
  ((max-tags     :type integer
		 :initarg :max-tags
		 :initform 5
		 :custom integer
		 :documentation
		 "Max number of tags displayed on tooltip at once.
If `force-show' is 1,  this value is ignored with typing tab or space twice continuously.
if `force-show' is 0, this value is always ignored.")
   (force-show   :type integer
		 :initarg :force-show
	         :initform 1
		 :custom (choice (const
				  :tag "Show when double typing"
				  1)
				 (const
				  :tag "Show always"
				  0)
				 (const
				  :tag "Show if the number of tags is less than `max-tags'."
				  -1))
	         :documentation
		 "Control the behavior of the number of tags is greater than `max-tags'.
-1 means tags are never shown.
0 means the tags are always shown.
1 means tags are shown if space or tab is typed twice continuously.")
   (typing-count :type integer
		 :initform 0
		 :documentation
		 "Counter holding how many times the user types space or tab continuously before showing tags.")
   (shown        :type boolean
		 :initform nil
		 :documentation
		 "Flag representing whether tags is shown once or not.")
   )
  "Display completions options in a tooltip.
Display mechanism using tooltip for a list of possible completions.")

(defmethod initialize-instance :AFTER ((obj semantic-displayor-tooltip) &rest args)
  "Make sure we have tooltips required."
  (condition-case nil
      (require 'tooltip)
    (error nil))
  )

(defmethod semantic-displayor-show-request ((obj semantic-displayor-tooltip))
  "A request to show the current tags table."
  (if (or (not (featurep 'tooltip)) (not tooltip-mode))
      ;; If we cannot use tooltips, then go to the normal mode with
      ;; a traditional completion buffer.
      (call-next-method)
    (let* ((tablelong (semanticdb-strip-find-results (oref obj table)))
	   (table (semantic-unique-tag-table-by-name tablelong))
	   (l (mapcar semantic-completion-displayor-format-tag-function table))
	   (ll (length l))
	   (typing-count (oref obj typing-count))
	   (force-show (oref obj force-show))
	   (matchtxt (semantic-completion-text))
	   msg)
      (if (or (oref obj shown)
	      (< ll (oref obj max-tags))
	      (and (<= 0 force-show)
		   (< (1- force-show) typing-count)))
	  (progn
	    (oset obj typing-count 0)
	    (oset obj shown t)
	    (if (eq 1 ll)
		;; We Have only one possible match.  There could be two cases.
		;; 1) input text != single match.
		;;    --> Show it!
		;; 2) input text == single match.
		;;   --> Complain about it, but still show the match.
		(if (string= matchtxt (semantic-tag-name (car table)))
		    (setq msg (concat "[COMPLETE]\n" (car l)))
		  (setq msg (car l)))
	      ;; Create the long message.
	      (setq msg (mapconcat 'identity l "\n"))
	      ;; If there is nothing, say so!
	      (if (eq 0 (length msg))
		  (setq msg "[NO MATCH]")))
	    (semantic-displayor-tooltip-show msg))
	;; The typing count determines if the user REALLY REALLY
	;; wanted to show that much stuff.  Only increment
	;; if the current command is a completion command.
	(if (and (stringp (this-command-keys))
		 (string= (this-command-keys) "\C-i"))
	    (oset obj typing-count (1+ typing-count)))
	;; At this point, we know we have too many items.
	;; Let's be brave, and truncate l
	(setcdr (nthcdr (oref obj max-tags) l) nil)
	(setq msg (mapconcat 'identity l "\n"))
	(cond
	 ((= force-show -1)
	  (semantic-displayor-tooltip-show (concat msg "\n...")))
	 ((= force-show 1)
	  (semantic-displayor-tooltip-show (concat msg "\n(TAB for more)")))
	 )))))

;;; Compatibility
;;
(eval-and-compile
  (if (fboundp 'window-inside-edges)
      ;; Emacs devel.
      (defalias 'semantic-displayor-window-edges
        'window-inside-edges)
    ;; Emacs 21
    (defalias 'semantic-displayor-window-edges
      'window-edges)
    ))

(defun semantic-displayor-point-position ()
  "Return the location of POINT as positioned on the selected frame.
Return a cons cell (X . Y)"
  (let* ((frame (selected-frame))
	 (left (frame-parameter frame 'left))
	 (top (frame-parameter frame 'top))
	 (point-pix-pos (posn-x-y (posn-at-point)))
	 (edges (window-inside-pixel-edges (selected-window))))
    (cons (+ (car point-pix-pos) (car edges) left)
          (+ (cdr point-pix-pos) (cadr edges) top))))


(defun semantic-displayor-tooltip-show (text)
  "Display a tooltip with TEXT near cursor."
  (let ((point-pix-pos (semantic-displayor-point-position))
	(tooltip-frame-parameters
	 (append tooltip-frame-parameters nil)))
    (push
     (cons 'left (+ (car point-pix-pos) (frame-char-width)))
     tooltip-frame-parameters)
    (push
     (cons 'top (+ (cdr point-pix-pos) (frame-char-height)))
     tooltip-frame-parameters)
    (tooltip-show text)))

(defmethod semantic-displayor-scroll-request ((obj semantic-displayor-tooltip))
  "A request to for the displayor to scroll the completion list (if needed)."
  ;; Do scrolling in the tooltip.
  (oset obj max-tags 30)
  (semantic-displayor-show-request obj)
  )

;; End code contributed by Masatake YAMATO <jet@gyve.org>


;;; Ghost Text displayor
;;
(defclass semantic-displayor-ghost (semantic-displayor-focus-abstract)

  ((ghostoverlay :type overlay
		 :documentation
		 "The overlay the ghost text is displayed in.")
   (first-show :initform t
	       :documentation
	       "Non nil if we have not seen our first show request.")
   )
  "Cycle completions inline with ghost text.
Completion displayor using ghost chars after point for focus options.
Whichever completion is currently in focus will be displayed as ghost
text using overlay options.")

(defmethod semantic-displayor-next-action ((obj semantic-displayor-ghost))
  "The next action to take on the inline completion related to display."
  (let ((ans (call-next-method))
	(table (when (slot-boundp obj 'table)
		       (oref obj table))))
    (if (and (eq ans 'displayend)
	     table
	     (= (semanticdb-find-result-length table) 1)
	     )
	nil
      ans)))

(defmethod semantic-displayor-cleanup ((obj semantic-displayor-ghost))
  "Clean up any mess this displayor may have."
  (when (slot-boundp obj 'ghostoverlay)
    (semantic-overlay-delete (oref obj ghostoverlay)))
  )

(defmethod semantic-displayor-set-completions ((obj semantic-displayor-ghost)
					       table prefix)
  "Set the list of tags to be completed over to TABLE."
  (call-next-method)

  (semantic-displayor-cleanup obj)
  )


(defmethod semantic-displayor-show-request ((obj semantic-displayor-ghost))
  "A request to show the current tags table."
;  (if (oref obj first-show)
;      (progn
;	(oset obj first-show nil)
	(semantic-displayor-focus-next obj)
	(semantic-displayor-focus-request obj)
;	)
    ;; Only do the traditional thing if the first show request
    ;; has been seen.  Use the first one to start doing the ghost
    ;; text display.
;    (call-next-method)
;    )
)

(defmethod semantic-displayor-focus-request
  ((obj semantic-displayor-ghost))
  "Focus in on possible tag completions.
Focus is performed by cycling through the tags and showing a possible
completion text in ghost text."
  (let* ((tablelength (semanticdb-find-result-length (oref obj table)))
	 (focus (semantic-displayor-focus-tag obj))
	 (tag (car focus))
	 )
    (if (not tag)
	(semantic-completion-message "No tags to focus on.")
      ;; Display the focus completion as ghost text after the current
      ;; inline text.
      (when (or (not (slot-boundp obj 'ghostoverlay))
		(not (semantic-overlay-live-p (oref obj ghostoverlay))))
	(oset obj ghostoverlay
	      (semantic-make-overlay (point) (1+ (point)) (current-buffer) t)))

      (let* ((lp (semantic-completion-text))
	     (os (substring (semantic-tag-name tag) (length lp)))
	     (ol (oref obj ghostoverlay))
	     )

	(put-text-property 0 (length os) 'face 'region os)

	(semantic-overlay-put
	 ol 'display (concat os (buffer-substring (point) (1+ (point)))))
	)
      ;; Calculate text difference between contents and the focus item.
      (let* ((mbc (semantic-completion-text))
	     (ftn (concat (semantic-tag-name tag)))
	     )
	(put-text-property (length mbc) (length ftn) 'face
			   'bold ftn)
	(semantic-completion-message
	 (format "%s [%d of %d matches]" ftn (1+ (oref obj focus)) tablelength)))
      )))


;;; ------------------------------------------------------------
;;; Specific queries
;;
(defvar semantic-complete-inline-custom-type
  (append '(radio)
	  (mapcar
	   (lambda (class)
	     (let* ((C (intern (car class)))
		    (doc (documentation-property C 'variable-documentation))
		    (doc1 (car (split-string doc "\n")))
		    )
	       (list 'const
		     :tag doc1
		     C)))
	   (eieio-build-class-alist semantic-displayor-abstract t))
	  )
  "Possible options for inline completion displayors.
Use this to enable custom editing.")

(defcustom semantic-complete-inline-analyzer-displayor-class
  'semantic-displayor-traditional
  "*Class for displayor to use with inline completion."
  :group 'semantic
  :type semantic-complete-inline-custom-type
  )

(defun semantic-complete-read-tag-buffer-deep (prompt &optional
						      default-tag
						      initial-input
						      history)
  "Ask for a tag by name from the current buffer.
Available tags are from the current buffer, at any level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in."
  (semantic-complete-read-tag-engine
   (semantic-collector-buffer-deep prompt :buffer (current-buffer))
   (semantic-displayor-traditional-with-focus-highlight "simple")
   ;;(semantic-displayor-tooltip "simple")
   prompt
   default-tag
   initial-input
   history)
  )

(defun semantic-complete-read-tag-local-members (prompt &optional
							default-tag
							initial-input
							history)
  "Ask for a tag by name from the local type members.
Available tags are from the current scope.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in."
  (semantic-complete-read-tag-engine
   (semantic-collector-local-members prompt :buffer (current-buffer))
   (semantic-displayor-traditional-with-focus-highlight "simple")
   ;;(semantic-displayor-tooltip "simple")
   prompt
   default-tag
   initial-input
   history)
  )

(defun semantic-complete-read-tag-project (prompt &optional
						  default-tag
						  initial-input
						  history)
  "Ask for a tag by name from the current project.
Available tags are from the current project, at the top level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in."
  (semantic-complete-read-tag-engine
   (semantic-collector-project-brutish prompt
				       :buffer (current-buffer)
				       :path (current-buffer)
				       )
   (semantic-displayor-traditional-with-focus-highlight "simple")
   prompt
   default-tag
   initial-input
   history)
  )

(defun semantic-complete-inline-tag-project ()
  "Complete a symbol name by name from within the current project.
This is similar to `semantic-complete-read-tag-project', except
that the completion interaction is in the buffer where the context
was calculated from.
Customize `semantic-complete-inline-analyzer-displayor-class'
to control how completion options are displayed.
See `semantic-complete-inline-tag-engine' for details on how
completion works."
  (let* ((collector (semantic-collector-project-brutish
		     "inline"
		     :buffer (current-buffer)
		     :path (current-buffer)))
	 (sbounds (semantic-ctxt-current-symbol-and-bounds))
	 (syms (car sbounds))
	 (start (car (nth 2 sbounds)))
	 (end (cdr (nth 2 sbounds)))
	 (rsym (reverse syms))
	 (thissym (nth 1 sbounds))
	 (nextsym (car-safe (cdr rsym)))
	 (complst nil))
    (when (and thissym (or (not (string= thissym ""))
			   nextsym))
      ;; Do a quick calcuation of completions.
      (semantic-collector-calculate-completions
       collector thissym nil)
      ;; Get the master list
      (setq complst (semanticdb-strip-find-results
		     (semantic-collector-all-completions collector thissym)))
      ;; Shorten by name
      (setq complst (semantic-unique-tag-table-by-name complst))
      (if (or (and (= (length complst) 1)
		   ;; Check to see if it is the same as what is there.
		   ;; if so, we can offer to complete.
		   (let ((compname (semantic-tag-name (car complst))))
		     (not (string= compname thissym))))
	      (> (length complst) 1))
	  ;; There are several options.  Do the completion.
	  (semantic-complete-inline-tag-engine
	   collector
	   (funcall semantic-complete-inline-analyzer-displayor-class
		    "inline displayor")
	   ;;(semantic-displayor-tooltip "simple")
	   (current-buffer)
	   start end))
      )))

(defun semantic-complete-read-tag-analyzer (prompt &optional
						   context
						   history)
  "Ask for a tag by name based on the current context.
The function `semantic-analyze-current-context' is used to
calculate the context.  `semantic-analyze-possible-completions' is used
to generate the list of possible completions.
PROMPT is the first part of the prompt.  Additional prompt
is added based on the contexts full prefix.
CONTEXT is the semantic analyzer context to start with.
HISTORY is a symbol representing a variable to store the history in.
usually a default-tag and initial-input are available for completion
prompts.  these are calculated from the CONTEXT variable passed in."
  (if (not context) (setq context (semantic-analyze-current-context (point))))
  (let* ((syms (semantic-ctxt-current-symbol (point)))
	 (inp (car (reverse syms))))
    (setq syms (nreverse (cdr (nreverse syms))))
    (semantic-complete-read-tag-engine
     (semantic-collector-analyze-completions
      prompt
      :buffer (oref context buffer)
      :context context)
     (semantic-displayor-traditional-with-focus-highlight "simple")
     (with-current-buffer (oref context buffer)
       (goto-char (cdr (oref context bounds)))
       (concat prompt (mapconcat 'identity syms ".")
	       (if syms "." "")
	       ))
     nil
     inp
     history)))

(defun semantic-complete-inline-analyzer (context)
  "Complete a symbol name by name based on the current context.
This is similar to `semantic-complete-read-tag-analyze', except
that the completion interaction is in the buffer where the context
was calculated from.
CONTEXT is the semantic analyzer context to start with.
Customize `semantic-complete-inline-analyzer-displayor-class'
to control how completion options are displayed.

See `semantic-complete-inline-tag-engine' for details on how
completion works."
  (if (not context) (setq context (semantic-analyze-current-context (point))))
  (if (not context) (error "Nothing to complete on here"))
  (let* ((collector (semantic-collector-analyze-completions
		     "inline"
		     :buffer (oref context buffer)
		     :context context))
	 (syms (semantic-ctxt-current-symbol (point)))
	 (rsym (reverse syms))
	 (thissym (car rsym))
	 (nextsym (car-safe (cdr rsym)))
	 (complst nil))
    (when (and thissym (or (not (string= thissym ""))
			   nextsym))
      ;; Do a quick calcuation of completions.
      (semantic-collector-calculate-completions
       collector thissym nil)
      ;; Get the master list
      (setq complst (semanticdb-strip-find-results
		     (semantic-collector-all-completions collector thissym)))
      ;; Shorten by name
      (setq complst (semantic-unique-tag-table-by-name complst))
      (if (or (and (= (length complst) 1)
		   ;; Check to see if it is the same as what is there.
		   ;; if so, we can offer to complete.
		   (let ((compname (semantic-tag-name (car complst))))
		     (not (string= compname thissym))))
	      (> (length complst) 1))
	  ;; There are several options.  Do the completion.
	  (semantic-complete-inline-tag-engine
	   collector
	   (funcall semantic-complete-inline-analyzer-displayor-class
		    "inline displayor")
	   ;;(semantic-displayor-tooltip "simple")
	   (oref context buffer)
	   (car (oref context bounds))
	   (cdr (oref context bounds))
	   ))
      )))

(defcustom semantic-complete-inline-analyzer-idle-displayor-class
  'semantic-displayor-ghost
  "*Class for displayor to use with inline completion at idle time."
  :group 'semantic
  :type semantic-complete-inline-custom-type
  )

(defun semantic-complete-inline-analyzer-idle (context)
  "Complete a symbol name by name based on the current context for idle time.
CONTEXT is the semantic analyzer context to start with.
This function is used from `semantic-idle-completions-mode'.

This is the same as `semantic-complete-inline-analyzer', except that
it uses `semantic-complete-inline-analyzer-idle-displayor-class'
to control how completions are displayed.

See `semantic-complete-inline-tag-engine' for details on how
completion works."
  (let ((semantic-complete-inline-analyzer-displayor-class
	 semantic-complete-inline-analyzer-idle-displayor-class))
    (semantic-complete-inline-analyzer context)
    ))


;;;###autoload
(defun semantic-complete-jump-local ()
  "Jump to a local semantic symbol."
  (interactive)
  (let ((tag (semantic-complete-read-tag-buffer-deep "Jump to symbol: ")))
    (when (semantic-tag-p tag)
      (push-mark)
      (goto-char (semantic-tag-start tag))
      (semantic-momentary-highlight-tag tag)
      (message "%S: %s "
	       (semantic-tag-class tag)
	       (semantic-tag-name  tag)))))

;;;###autoload
(defun semantic-complete-jump ()
  "Jump to a semantic symbol."
  (interactive)
  (let* ((tag (semantic-complete-read-tag-project "Jump to symbol: ")))
    (when (semantic-tag-p tag)
      (push-mark)
      (semantic-go-to-tag tag)
      (switch-to-buffer (current-buffer))
      (semantic-momentary-highlight-tag tag)
      (message "%S: %s "
	       (semantic-tag-class tag)
	       (semantic-tag-name  tag)))))

;;;###autoload
(defun semantic-complete-jump-local-members ()
  "Jump to a semantic symbol."
  (interactive)
  (let* ((tag (semantic-complete-read-tag-local-members "Jump to symbol: ")))
    (when (semantic-tag-p tag)
      (let ((start (condition-case nil (semantic-tag-start tag)
		     (error nil))))
	(unless start
	  (error "Tag %s has no location" (semantic-format-tag-prototype tag)))
	(push-mark)
	(goto-char start)
	(semantic-momentary-highlight-tag tag)
	(message "%S: %s "
		 (semantic-tag-class tag)
		 (semantic-tag-name  tag))))))

;;;###autoload
(defun semantic-complete-analyze-and-replace ()
  "Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The minibuffer is used to perform the completion.
The result is inserted as a replacement of the text that was there."
  (interactive)
  (let* ((c (semantic-analyze-current-context (point)))
	 (tag (save-excursion (semantic-complete-read-tag-analyzer "" c))))
    ;; Take tag, and replace context bound with its name.
    (goto-char (car (oref c bounds)))
    (delete-region (point) (cdr (oref c bounds)))
    (insert (semantic-tag-name tag))
    (message "%S" (semantic-format-tag-summarize tag))))

;;;###autoload
(defun semantic-complete-analyze-inline ()
  "Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion.
Configure `semantic-complete-inline-analyzer-displayor-class' to change
how completion options are displayed."
  (interactive)
  ;; Only do this if we are not already completing something.
  (if (not (semantic-completion-inline-active-p))
      (semantic-complete-inline-analyzer
       (semantic-analyze-current-context (point))))
  ;; Report a message if things didn't startup.
  (if (and (called-interactively-p 'any)
	   (not (semantic-completion-inline-active-p)))
      (message "Inline completion not needed.")
    ;; Since this is most likely bound to something, and not used
    ;; at idle time, throw in a TAB for good measure.
    (semantic-complete-inline-TAB)))

;;;###autoload
(defun semantic-complete-analyze-inline-idle ()
  "Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion.
Configure `semantic-complete-inline-analyzer-idle-displayor-class'
to change how completion options are displayed."
  (interactive)
  ;; Only do this if we are not already completing something.
  (if (not (semantic-completion-inline-active-p))
      (semantic-complete-inline-analyzer-idle
       (semantic-analyze-current-context (point))))
  ;; Report a message if things didn't startup.
  (if (and (called-interactively-p 'interactive)
	   (not (semantic-completion-inline-active-p)))
      (message "Inline completion not needed.")))

;;;###autoload
(defun semantic-complete-self-insert (arg)
  "Like `self-insert-command', but does completion afterwards.
ARG is passed to `self-insert-command'.  If ARG is nil,
use `semantic-complete-analyze-inline' to complete."
  (interactive "p")
  ;; If we are already in a completion scenario, exit now, and then start over.
  (semantic-complete-inline-exit)

  ;; Insert the key
  (self-insert-command arg)

  ;; Prepare for doing completion, but exit quickly if there is keyboard
  ;; input.
  (when (save-window-excursion
	  (save-excursion
	    (and (not (semantic-exit-on-input 'csi
			(semantic-fetch-tags)
			(semantic-throw-on-input 'csi)
			nil))
		 (= arg 1)
		 (not (semantic-exit-on-input 'csi
			(semantic-analyze-current-context)
			(semantic-throw-on-input 'csi)
			nil)))))
    (condition-case nil
	(semantic-complete-analyze-inline)
      ;; Ignore errors.  Seems likely that we'll get some once in a while.
      (error nil))
    ))

(provide 'semantic/complete)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/complete"
;; End:

;;; semantic/complete.el ends here
