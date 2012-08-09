;;; flyspell.el --- on-the-fly spell checker

;; Copyright (C) 1998, 2000-2012  Free Software Foundation, Inc.

;; Author: Manuel Serrano <Manuel.Serrano@sophia.inria.fr>
;; Maintainer: FSF
;; Keywords: convenience

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
;; Flyspell is a minor Emacs mode performing on-the-fly spelling
;; checking.
;;
;; To enable Flyspell minor mode, type M-x flyspell-mode.
;; This applies only to the current buffer.
;;
;; To enable Flyspell in text representing computer programs, type
;; M-x flyspell-prog-mode.
;; In that mode only text inside comments is checked.
;;
;; Some user variables control the behavior of flyspell.  They are
;; those defined under the `User variables' comment.

;;; Code:

(require 'ispell)

;;*---------------------------------------------------------------------*/
;;*    Group ...                                                        */
;;*---------------------------------------------------------------------*/
(defgroup flyspell nil
  "Spell checking on the fly."
  :tag "FlySpell"
  :prefix "flyspell-"
  :group 'ispell
  :group 'processes)

;;*---------------------------------------------------------------------*/
;;*    User configuration ...                                           */
;;*---------------------------------------------------------------------*/
(defcustom flyspell-highlight-flag t
  "How Flyspell should indicate misspelled words.
Non-nil means use highlight, nil means use minibuffer messages."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-mark-duplications-flag t
  "Non-nil means Flyspell reports a repeated word as an error.
See `flyspell-mark-duplications-exceptions' to add exceptions to this rule.
Detection of repeated words is not implemented in
\"large\" regions; see `flyspell-large-region'."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-mark-duplications-exceptions
  '((nil . ("that" "had")) ; Common defaults for English.
    ("\\`francais" . ("nous" "vous")))
  "A list of exceptions for duplicated words.
It should be a list of (LANGUAGE . EXCEPTION-LIST).

LANGUAGE is nil, which means the exceptions apply regardless of
the current dictionary, or a regular expression matching the
dictionary name (`ispell-local-dictionary' or
`ispell-dictionary') for which the exceptions should apply.

EXCEPTION-LIST is a list of strings.  The checked word is
downcased before comparing with these exceptions."
  :group 'flyspell
  :type '(alist :key-type (choice (const :tag "All dictionaries" nil)
				  string)
		:value-type (repeat string))
  :version "24.1")

(defcustom flyspell-sort-corrections nil
  "Non-nil means, sort the corrections alphabetically before popping them."
  :group 'flyspell
  :version "21.1"
  :type 'boolean)

(defcustom flyspell-duplicate-distance -1
  "The maximum distance for finding duplicates of unrecognized words.
This applies to the feature that when a word is not found in the dictionary,
if the same spelling occurs elsewhere in the buffer,
Flyspell uses a different face (`flyspell-duplicate') to highlight it.
This variable specifies how far to search to find such a duplicate.
-1 means no limit (search the whole buffer).
0 means do not search for duplicate unrecognized spellings."
  :group 'flyspell
  :version "21.1"
  :type '(choice (const :tag "no limit" -1)
		 number))

(defcustom flyspell-delay 3
  "The number of seconds to wait before checking, after a \"delayed\" command."
  :group 'flyspell
  :type 'number)

(defcustom flyspell-persistent-highlight t
  "Non-nil means misspelled words remain highlighted until corrected.
If this variable is nil, only the most recently detected misspelled word
is highlighted."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-highlight-properties t
  "Non-nil means highlight incorrect words even if a property exists for this word."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-default-delayed-commands
  '(self-insert-command
    delete-backward-char
    backward-or-forward-delete-char
    delete-char
    scrollbar-vertical-drag
    backward-delete-char-untabify)
  "The standard list of delayed commands for Flyspell.
See `flyspell-delayed-commands'."
  :group 'flyspell
  :version "21.1"
  :type '(repeat (symbol)))

(defcustom flyspell-delayed-commands nil
  "List of commands that are \"delayed\" for Flyspell mode.
After these commands, Flyspell checking is delayed for a short time,
whose length is specified by `flyspell-delay'."
  :group 'flyspell
  :type '(repeat (symbol)))

(defcustom flyspell-default-deplacement-commands
  '(next-line
    previous-line
    scroll-up
    scroll-down)
  "The standard list of deplacement commands for Flyspell.
See `flyspell-deplacement-commands'."
  :group 'flyspell
  :version "21.1"
  :type '(repeat (symbol)))

(defcustom flyspell-deplacement-commands nil
  "List of commands that are \"deplacement\" for Flyspell mode.
After these commands, Flyspell checking is performed only if the previous
command was not the very same command."
  :group 'flyspell
  :version "21.1"
  :type '(repeat (symbol)))

(defcustom flyspell-issue-welcome-flag t
  "Non-nil means that Flyspell should display a welcome message when started."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-issue-message-flag t
  "Non-nil means that Flyspell emits messages when checking words."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-incorrect-hook nil
  "List of functions to be called when incorrect words are encountered.
Each function is given three arguments.  The first two
arguments are the beginning and the end of the incorrect region.
The third is either the symbol `doublon' or the list
of possible corrections as returned by `ispell-parse-output'.

If any of the functions return non-nil, the word is not highlighted as
incorrect."
  :group 'flyspell
  :version "21.1"
  :type 'hook)

(defcustom flyspell-default-dictionary nil
  "A string that is the name of the default dictionary.
This is passed to the `ispell-change-dictionary' when flyspell is started.
If the variable `ispell-local-dictionary' or `ispell-dictionary' is non-nil
when flyspell is started, the value of that variable is used instead
of `flyspell-default-dictionary' to select the default dictionary.
Otherwise, if `flyspell-default-dictionary' is nil, it means to use
Ispell's ultimate default dictionary."
  :group 'flyspell
  :version "21.1"
  :type '(choice string (const :tag "Default" nil)))

(defcustom flyspell-tex-command-regexp
  "\\(\\(begin\\|end\\)[ \t]*{\\|\\(cite[a-z*]*\\|label\\|ref\\|eqref\\|usepackage\\|documentclass\\)[ \t]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)"
  "A string that is the regular expression that matches TeX commands."
  :group 'flyspell
  :version "21.1"
  :type 'string)

(defcustom flyspell-check-tex-math-command nil
  "Non-nil means check even inside TeX math environment.
TeX math environments are discovered by `texmathp', implemented
inside AUCTeX package.  That package may be found at
URL `http://www.gnu.org/software/auctex/'"
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-dictionaries-that-consider-dash-as-word-delimiter
  '("francais" "deutsch8" "norsk")
  "List of dictionary names that consider `-' as word delimiter."
  :group 'flyspell
  :version "21.1"
  :type '(repeat (string)))

(defcustom flyspell-abbrev-p
  nil
  "If non-nil, add correction to abbreviation table."
  :group 'flyspell
  :version "21.1"
  :type 'boolean)

(defcustom flyspell-use-global-abbrev-table-p
  nil
  "If non-nil, prefer global abbrev table to local abbrev table."
  :group 'flyspell
  :version "21.1"
  :type 'boolean)

(defcustom flyspell-mode-line-string " Fly"
  "String displayed on the modeline when flyspell is active.
Set this to nil if you don't want a modeline indicator."
  :group 'flyspell
  :type '(choice string (const :tag "None" nil)))

(defcustom flyspell-large-region 1000
  "The threshold that determines if a region is small.
If the region is smaller than this number of characters,
`flyspell-region' checks the words sequentially using regular
flyspell methods.  Else, if the region is large, a new Ispell process is
spawned for speed.

Doubled words are not detected in a large region, because Ispell
does not check for them.

If this variable is nil, all regions are treated as small."
  :group 'flyspell
  :version "21.1"
  :type '(choice number (const :tag "All small" nil)))

(defcustom flyspell-insert-function (function insert)
  "Function for inserting word by flyspell upon correction."
  :group 'flyspell
  :type 'function)

(defcustom flyspell-before-incorrect-word-string nil
  "String used to indicate an incorrect word starting."
  :group 'flyspell
  :type '(choice string (const nil)))

(defcustom flyspell-after-incorrect-word-string nil
  "String used to indicate an incorrect word ending."
  :group 'flyspell
  :type '(choice string (const nil)))

(defvar flyspell-mode-map)

(defcustom flyspell-use-meta-tab t
  "Non-nil means that flyspell uses M-TAB to correct word."
  :group 'flyspell
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
	 (define-key flyspell-mode-map "\M-\t"
	   (if (set sym val)
	       'flyspell-auto-correct-word))))

(defcustom flyspell-auto-correct-binding
  [(control ?\;)]
  "The key binding for flyspell auto correction."
  :group 'flyspell)

;;*---------------------------------------------------------------------*/
;;*    Mode specific options                                            */
;;*    -------------------------------------------------------------    */
;;*    Mode specific options enable users to disable flyspell on        */
;;*    certain word depending of the emacs mode. For instance, when     */
;;*    using flyspell with mail-mode add the following expression       */
;;*    in your .emacs file:                                             */
;;*       (add-hook 'mail-mode                                          */
;;*    	     (lambda () (setq flyspell-generic-check-word-predicate    */
;;*    			       'mail-mode-flyspell-verify)))            */
;;*---------------------------------------------------------------------*/
(defvar flyspell-generic-check-word-predicate nil
  "Function providing per-mode customization over which words are flyspelled.
Returns t to continue checking, nil otherwise.
Flyspell mode sets this variable to whatever is the `flyspell-mode-predicate'
property of the major mode name.")
(make-variable-buffer-local 'flyspell-generic-check-word-predicate)
(defvaralias 'flyspell-generic-check-word-p
  'flyspell-generic-check-word-predicate)

;;*--- mail mode -------------------------------------------------------*/
(put 'mail-mode 'flyspell-mode-predicate 'mail-mode-flyspell-verify)
(put 'message-mode 'flyspell-mode-predicate 'mail-mode-flyspell-verify)
(defvar message-signature-separator)
(defun mail-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate' in Mail mode."
  (let ((header-end (save-excursion
		      (goto-char (point-min))
		      (re-search-forward
		       (concat "^"
			       (regexp-quote mail-header-separator)
			       "$")
		       nil t)
		      (point)))
	(signature-begin
         (if (not (boundp 'message-signature-separator))
             (point-max)
           (save-excursion
             (goto-char (point-max))
             (re-search-backward message-signature-separator nil t)
             (point)))))
    (cond ((< (point) header-end)
	   (and (save-excursion (beginning-of-line)
				(looking-at "^Subject:"))
		(> (point) (match-end 0))))
	  ((> (point) signature-begin)
	   nil)
	  (t
	   (save-excursion
	     (beginning-of-line)
	     (not (looking-at "[>}|]\\|To:")))))))

;;*--- texinfo mode ----------------------------------------------------*/
(put 'texinfo-mode 'flyspell-mode-predicate 'texinfo-mode-flyspell-verify)
(defun texinfo-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate' in Texinfo mode."
  (save-excursion
    (forward-word -1)
    (not (looking-at "@"))))

;;*--- tex mode --------------------------------------------------------*/
(put 'tex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)
(defun tex-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate' in LaTeX mode."
  (and
   (not (save-excursion
	  (re-search-backward "^[ \t]*%%%[ \t]+Local" nil t)))
   (not (save-excursion
	  (let ((this (point)))
	    (beginning-of-line)
	    (and (re-search-forward "\\\\\\(cite\\|label\\|ref\\){[^}]*}"
				    (line-end-position) t)
		 (>= this (match-beginning 0))
		 (<= this (match-end 0))))))))

;;*--- sgml mode -------------------------------------------------------*/
(put 'sgml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)
(put 'html-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)
(put 'nxml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)

(autoload 'sgml-lexical-context "sgml-mode")

(defun sgml-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate' in SGML mode.
Tag and attribute names are not spell checked, everything else is.

String values of attributes are checked because they can be text
like <img alt=\"Some thing.\">."

  (not (memq (car (sgml-lexical-context))
	     '(tag pi))))

;;*---------------------------------------------------------------------*/
;;*    Programming mode                                                 */
;;*---------------------------------------------------------------------*/
(defvar flyspell-prog-text-faces
  '(font-lock-string-face font-lock-comment-face font-lock-doc-face)
  "Faces corresponding to text in programming-mode buffers.")

(defun flyspell-generic-progmode-verify ()
  "Used for `flyspell-generic-check-word-predicate' in programming modes."
  ;; (point) is next char after the word. Must check one char before.
  (let ((f (get-text-property (- (point) 1) 'face)))
    (memq f flyspell-prog-text-faces)))

;;;###autoload
(defun flyspell-prog-mode ()
  "Turn on `flyspell-mode' for comments and strings."
  (interactive)
  (setq flyspell-generic-check-word-predicate
        'flyspell-generic-progmode-verify)
  (flyspell-mode 1)
  (run-hooks 'flyspell-prog-mode-hook))

;;*---------------------------------------------------------------------*/
;;*    Overlay compatibility                                            */
;;*---------------------------------------------------------------------*/
(autoload 'make-overlay            "overlay" "Overlay compatibility kit." t)
(autoload 'overlayp                "overlay" "Overlay compatibility kit." t)
(autoload 'overlays-in             "overlay" "Overlay compatibility kit." t)
(autoload 'delete-overlay          "overlay" "Overlay compatibility kit." t)
(autoload 'overlays-at             "overlay" "Overlay compatibility kit." t)
(autoload 'overlay-put             "overlay" "Overlay compatibility kit." t)
(autoload 'overlay-get             "overlay" "Overlay compatibility kit." t)
(autoload 'previous-overlay-change "overlay" "Overlay compatibility kit." t)

;;*---------------------------------------------------------------------*/
;;*    The minor mode declaration.                                      */
;;*---------------------------------------------------------------------*/
(defvar flyspell-mouse-map
  (let ((map (make-sparse-keymap)))
    (if (featurep 'xemacs)
	(define-key map [button2] #'flyspell-correct-word)
      (define-key map [down-mouse-2] #'flyspell-correct-word)
      (define-key map [mouse-2] 'undefined))
    map)
  "Keymap for Flyspell to put on erroneous words.")

(defvar flyspell-mode-map
  (let ((map (make-sparse-keymap)))
    (if flyspell-use-meta-tab
      (define-key map "\M-\t" 'flyspell-auto-correct-word))
    (define-key map flyspell-auto-correct-binding 'flyspell-auto-correct-previous-word)
    (define-key map [(control ?\,)] 'flyspell-goto-next-error)
    (define-key map [(control ?\.)] 'flyspell-auto-correct-word)
    (define-key map [?\C-c ?$] 'flyspell-correct-word-before-point)
    map)
  "Minor mode keymap for Flyspell mode--for the whole buffer.")

;; dash character machinery
(defvar flyspell-consider-dash-as-word-delimiter-flag nil
   "*Non-nil means that the `-' char is considered as a word delimiter.")
(make-variable-buffer-local 'flyspell-consider-dash-as-word-delimiter-flag)
(defvar flyspell-dash-dictionary nil)
(make-variable-buffer-local 'flyspell-dash-dictionary)
(defvar flyspell-dash-local-dictionary nil)
(make-variable-buffer-local 'flyspell-dash-local-dictionary)

;;*---------------------------------------------------------------------*/
;;*    Highlighting                                                     */
;;*---------------------------------------------------------------------*/
(defface flyspell-incorrect
  '((((class color)) (:foreground "OrangeRed" :bold t :underline t))
    (t (:bold t)))
  "Face used for marking a misspelled word in Flyspell."
  :group 'flyspell)
(define-obsolete-face-alias 'flyspell-incorrect-face 'flyspell-incorrect "22.1")

(defface flyspell-duplicate
  '((((class color)) (:foreground "Gold3" :bold t :underline t))
    (t (:bold t)))
  "Face used for marking a misspelled word that appears twice in the buffer.
See also `flyspell-duplicate-distance'."
  :group 'flyspell)
(define-obsolete-face-alias 'flyspell-duplicate-face 'flyspell-duplicate "22.1")

(defvar flyspell-overlay nil)

;;*---------------------------------------------------------------------*/
;;*    flyspell-mode ...                                                */
;;*---------------------------------------------------------------------*/
;;;###autoload(defvar flyspell-mode nil "Non-nil if Flyspell mode is enabled.")
;;;###autoload
(define-minor-mode flyspell-mode
  "Toggle on-the-fly spell checking (Flyspell mode).
With a prefix argument ARG, enable Flyspell mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Flyspell mode is a buffer-local minor mode.  When enabled, it
spawns a single Ispell process and checks each word.  The default
flyspell behavior is to highlight incorrect words.

Bindings:
\\[ispell-word]: correct words (using Ispell).
\\[flyspell-auto-correct-word]: automatically correct word.
\\[flyspell-auto-correct-previous-word]: automatically correct the last misspelled word.
\\[flyspell-correct-word] (or down-mouse-2): popup correct words.

Hooks:
This runs `flyspell-mode-hook' after flyspell mode is entered or exit.

Remark:
`flyspell-mode' uses `ispell-mode'.  Thus all Ispell options are
valid.  For instance, a different dictionary can be used by
invoking `ispell-change-dictionary'.

Consider using the `ispell-parser' to check your text.  For instance
consider adding:
\(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))
in your .emacs file.

\\[flyspell-region] checks all words inside a region.
\\[flyspell-buffer] checks the whole buffer."
  :lighter flyspell-mode-line-string
  :keymap flyspell-mode-map
  :group 'flyspell
  (if flyspell-mode
      (condition-case err
	  (flyspell-mode-on)
	(error (message "Error enabling Flyspell mode:\n%s" (cdr err))
	       (flyspell-mode -1)))
    (flyspell-mode-off)))

;;;###autoload
(defun turn-on-flyspell ()
  "Unconditionally turn on Flyspell mode."
  (flyspell-mode 1))

;;;###autoload
(defun turn-off-flyspell ()
  "Unconditionally turn off Flyspell mode."
  (flyspell-mode -1))

(custom-add-option 'text-mode-hook 'turn-on-flyspell)

;;*---------------------------------------------------------------------*/
;;*    flyspell-buffers ...                                             */
;;*    -------------------------------------------------------------    */
;;*    For remembering buffers running flyspell                         */
;;*---------------------------------------------------------------------*/
(defvar flyspell-buffers nil)

;;*---------------------------------------------------------------------*/
;;*    flyspell-minibuffer-p ...                                        */
;;*---------------------------------------------------------------------*/
(defun flyspell-minibuffer-p (buffer)
  "Is BUFFER a minibuffer?"
  (let ((ws (get-buffer-window-list buffer t)))
    (and (consp ws) (window-minibuffer-p (car ws)))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-accept-buffer-local-defs ...                            */
;;*---------------------------------------------------------------------*/
(defvar flyspell-last-buffer nil
  "The buffer in which the last flyspell operation took place.")

(defun flyspell-accept-buffer-local-defs (&optional force)
  ;; When flyspell-word is used inside a loop (e.g. when processing
  ;; flyspell-changes), the calls to `ispell-accept-buffer-local-defs' end
  ;; up dwarfing everything else, so only do it when the buffer has changed.
  (when (or force (not (eq flyspell-last-buffer (current-buffer))))
    (setq flyspell-last-buffer (current-buffer))
    ;; Strange problem:  If buffer in current window has font-lock turned on,
    ;; but SET-BUFFER was called to point to an invisible buffer, this ispell
    ;; call will reset the buffer to the buffer in the current window.
    ;; However, it only happens at startup (fix by Albert L. Ting).
    (save-current-buffer
      (ispell-accept-buffer-local-defs))
    (unless (and (eq flyspell-dash-dictionary ispell-dictionary)
                 (eq flyspell-dash-local-dictionary ispell-local-dictionary))
      ;; The dictionary has changed
      (setq flyspell-dash-dictionary ispell-dictionary)
      (setq flyspell-dash-local-dictionary ispell-local-dictionary)
      (setq flyspell-consider-dash-as-word-delimiter-flag
            (member (or ispell-local-dictionary ispell-dictionary)
                    flyspell-dictionaries-that-consider-dash-as-word-delimiter)))))

(defun flyspell-hack-local-variables-hook ()
  ;; When local variables are loaded, see if the dictionary context
  ;; has changed.
  (flyspell-accept-buffer-local-defs 'force))

(defun flyspell-kill-ispell-hook ()
  (setq flyspell-last-buffer nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (kill-local-variable 'flyspell-word-cache-word))))

;; Make sure we flush our caches when needed.  Do it here rather than in
;; flyspell-mode-on, since flyspell-region may be used without ever turning
;; on flyspell-mode.
(add-hook 'ispell-kill-ispell-hook 'flyspell-kill-ispell-hook)

;;*---------------------------------------------------------------------*/
;;*    flyspell-mode-on ...                                             */
;;*---------------------------------------------------------------------*/
(defun flyspell-mode-on ()
  "Turn Flyspell mode on.  Do not use this; use `flyspell-mode' instead."
  (ispell-set-spellchecker-params) ; Initialize variables and dicts alists
  (setq ispell-highlight-face 'flyspell-incorrect)
  ;; local dictionaries setup
  (or ispell-local-dictionary ispell-dictionary
      (if flyspell-default-dictionary
	  (ispell-change-dictionary flyspell-default-dictionary)))
  ;; we have to force ispell to accept the local definition or
  ;; otherwise it could be too late, the local dictionary may
  ;; be forgotten!
  ;; Pass the `force' argument for the case where flyspell was active already
  ;; but the buffer's local-defs have been edited.
  (flyspell-accept-buffer-local-defs 'force)
  ;; we put the `flyspell-delayed' property on some commands
  (flyspell-delay-commands)
  ;; we put the `flyspell-deplacement' property on some commands
  (flyspell-deplacement-commands)
  ;; we bound flyspell action to post-command hook
  (add-hook 'post-command-hook (function flyspell-post-command-hook) t t)
  ;; we bound flyspell action to pre-command hook
  (add-hook 'pre-command-hook (function flyspell-pre-command-hook) t t)
  ;; we bound flyspell action to after-change hook
  (add-hook 'after-change-functions 'flyspell-after-change-function nil t)
  ;; we bound flyspell action to hack-local-variables-hook
  (add-hook 'hack-local-variables-hook
	    (function flyspell-hack-local-variables-hook) t t)
  ;; set flyspell-generic-check-word-predicate based on the major mode
  (let ((mode-predicate (get major-mode 'flyspell-mode-predicate)))
    (if mode-predicate
	(setq flyspell-generic-check-word-predicate mode-predicate)))
  ;; the welcome message
  (if (and flyspell-issue-message-flag
	   flyspell-issue-welcome-flag
	   (called-interactively-p 'interactive))
      (let ((binding (where-is-internal 'flyspell-auto-correct-word
					nil 'non-ascii)))
	(message "%s"
	 (if binding
	     (format "Welcome to flyspell. Use %s or Mouse-2 to correct words."
		     (key-description binding))
	   "Welcome to flyspell. Use Mouse-2 to correct words.")))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-delay-commands ...                                      */
;;*---------------------------------------------------------------------*/
(defun flyspell-delay-commands ()
  "Install the standard set of Flyspell delayed commands."
  (mapc 'flyspell-delay-command flyspell-default-delayed-commands)
  (mapcar 'flyspell-delay-command flyspell-delayed-commands))

;;*---------------------------------------------------------------------*/
;;*    flyspell-delay-command ...                                       */
;;*---------------------------------------------------------------------*/
(defun flyspell-delay-command (command)
  "Set COMMAND to be delayed, for Flyspell.
When flyspell `post-command-hook' is invoked because a delayed command
as been used the current word is not immediately checked.
It will be checked only after `flyspell-delay' seconds."
  (interactive "SDelay Flyspell after Command: ")
  (put command 'flyspell-delayed t))

;;*---------------------------------------------------------------------*/
;;*    flyspell-deplacement-commands ...                                */
;;*---------------------------------------------------------------------*/
(defun flyspell-deplacement-commands ()
  "Install the standard set of Flyspell deplacement commands."
  (mapc 'flyspell-deplacement-command flyspell-default-deplacement-commands)
  (mapcar 'flyspell-deplacement-command flyspell-deplacement-commands))

;;*---------------------------------------------------------------------*/
;;*    flyspell-deplacement-command ...                                 */
;;*---------------------------------------------------------------------*/
(defun flyspell-deplacement-command (command)
  "Set COMMAND that implement cursor movements, for Flyspell.
When flyspell `post-command-hook' is invoked because of a deplacement command
as been used the current word is checked only if the previous command was
not the very same deplacement command."
  (interactive "SDeplacement Flyspell after Command: ")
  (put command 'flyspell-deplacement t))

;;*---------------------------------------------------------------------*/
;;*    flyspell-word-cache ...                                          */
;;*---------------------------------------------------------------------*/
(defvar flyspell-word-cache-start  nil)
(defvar flyspell-word-cache-end    nil)
(defvar flyspell-word-cache-word   nil)
(defvar flyspell-word-cache-result '_)
(make-variable-buffer-local 'flyspell-word-cache-start)
(make-variable-buffer-local 'flyspell-word-cache-end)
(make-variable-buffer-local 'flyspell-word-cache-word)
(make-variable-buffer-local 'flyspell-word-cache-result)

;;*---------------------------------------------------------------------*/
;;*    The flyspell pre-hook, store the current position. In the        */
;;*    post command hook, we will check, if the word at this position   */
;;*    has to be spell checked.                                         */
;;*---------------------------------------------------------------------*/
(defvar flyspell-pre-buffer     nil)
(defvar flyspell-pre-point      nil)
(defvar flyspell-pre-column     nil)
(defvar flyspell-pre-pre-buffer nil)
(defvar flyspell-pre-pre-point  nil)
(make-variable-buffer-local 'flyspell-pre-point)

;;*---------------------------------------------------------------------*/
;;*    flyspell-previous-command ...                                    */
;;*---------------------------------------------------------------------*/
(defvar flyspell-previous-command nil
  "The last interactive command checked by Flyspell.")

;;*---------------------------------------------------------------------*/
;;*    flyspell-pre-command-hook ...                                    */
;;*---------------------------------------------------------------------*/
(defun flyspell-pre-command-hook ()
  "Save the current buffer and point for Flyspell's post-command hook."
  (interactive)
  (setq flyspell-pre-buffer (current-buffer))
  (setq flyspell-pre-point  (point))
  (setq flyspell-pre-column (current-column)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-mode-off ...                                            */
;;*---------------------------------------------------------------------*/
;;;###autoload
(defun flyspell-mode-off ()
  "Turn Flyspell mode off."
  ;; we remove the hooks
  (remove-hook 'post-command-hook (function flyspell-post-command-hook) t)
  (remove-hook 'pre-command-hook (function flyspell-pre-command-hook) t)
  (remove-hook 'after-change-functions 'flyspell-after-change-function t)
  (remove-hook 'hack-local-variables-hook
	       (function flyspell-hack-local-variables-hook) t)
  ;; we remove all the flyspell highlightings
  (flyspell-delete-all-overlays)
  ;; we have to erase pre cache variables
  (setq flyspell-pre-buffer nil)
  (setq flyspell-pre-point  nil)
  ;; we mark the mode as killed
  (setq flyspell-mode nil))

;;*---------------------------------------------------------------------*/
;;*    flyspell-check-pre-word-p ...                                    */
;;*---------------------------------------------------------------------*/
(defun flyspell-check-pre-word-p ()
  "Return non-nil if we should check the word before point.
More precisely, it applies to the word that was before point
before the current command."
  (cond
   ((or (not (numberp flyspell-pre-point))
	(not (bufferp flyspell-pre-buffer))
	(not (buffer-live-p flyspell-pre-buffer)))
    nil)
   ((and (eq flyspell-pre-pre-point flyspell-pre-point)
	 (eq flyspell-pre-pre-buffer flyspell-pre-buffer))
    nil)
   ((or (and (= flyspell-pre-point (- (point) 1))
	     (eq (char-syntax (char-after flyspell-pre-point)) ?w))
	(= flyspell-pre-point (point))
	(= flyspell-pre-point (+ (point) 1)))
    nil)
   ((and (symbolp this-command)
	 (not executing-kbd-macro)
	 (or (get this-command 'flyspell-delayed)
	     (and (get this-command 'flyspell-deplacement)
		  (eq flyspell-previous-command this-command)))
	 (or (= (current-column) 0)
	     (= (current-column) flyspell-pre-column)
	     ;; If other post-command-hooks change the buffer,
	     ;; flyspell-pre-point can lie past eob (bug#468).
	     (null (char-after flyspell-pre-point))
	     (eq (char-syntax (char-after flyspell-pre-point)) ?w)))
    nil)
   ((not (eq (current-buffer) flyspell-pre-buffer))
    t)
   ((not (and (numberp flyspell-word-cache-start)
	      (numberp flyspell-word-cache-end)))
    t)
   (t
    (or (< flyspell-pre-point flyspell-word-cache-start)
	(> flyspell-pre-point flyspell-word-cache-end)))))

;;*---------------------------------------------------------------------*/
;;*    The flyspell after-change-hook, store the change position. In    */
;;*    the post command hook, we will check, if the word at this        */
;;*    position has to be spell checked.                                */
;;*---------------------------------------------------------------------*/
(defvar flyspell-changes nil)
(make-variable-buffer-local 'flyspell-changes)

;;*---------------------------------------------------------------------*/
;;*    flyspell-after-change-function ...                               */
;;*---------------------------------------------------------------------*/
(defun flyspell-after-change-function (start stop len)
  "Save the current buffer and point for Flyspell's post-command hook."
  (push (cons start stop) flyspell-changes))

;;*---------------------------------------------------------------------*/
;;*    flyspell-check-changed-word-p ...                                */
;;*---------------------------------------------------------------------*/
(defun flyspell-check-changed-word-p (start stop)
  "Return t when the changed word has to be checked.
The answer depends of several criteria.
Mostly we check word delimiters."
  (cond
   ((and (memq (char-after start) '(?\n ? )) (> stop start))
    t)
   ((not (numberp flyspell-pre-point))
    t)
   ((and (>= flyspell-pre-point start) (<= flyspell-pre-point stop))
    nil)
   ((let ((pos (point)))
      (or (>= pos start) (<= pos stop) (= pos (1+ stop))))
    nil)
   (t
    t)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-check-word-p ...                                        */
;;*---------------------------------------------------------------------*/
(defun flyspell-check-word-p ()
  "Return t when the word at `point' has to be checked.
The answer depends of several criteria.
Mostly we check word delimiters."
  (cond
   ((<= (- (point-max) 1) (point-min))
    ;; the buffer is not filled enough
    nil)
   ((and (and (> (current-column) 0)
	      (not (eq (current-column) flyspell-pre-column)))
	 (save-excursion
	   (backward-char 1)
	   (and (looking-at (flyspell-get-not-casechars))
		(or flyspell-consider-dash-as-word-delimiter-flag
		    (not (looking-at "-"))))))
    ;; yes because we have reached or typed a word delimiter.
    t)
   ((symbolp this-command)
    (cond
     ((get this-command 'flyspell-deplacement)
      (not (eq flyspell-previous-command this-command)))
     ((get this-command 'flyspell-delayed)
      ;; the current command is not delayed, that
      ;; is that we must check the word now
      (and (not unread-command-events)
	   (sit-for flyspell-delay)))
     (t t)))
   (t t)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-debug-signal-no-check ...                               */
;;*---------------------------------------------------------------------*/
(defun flyspell-debug-signal-no-check (msg obj)
  (setq debug-on-error t)
  (with-current-buffer (get-buffer-create "*flyspell-debug*")
    (erase-buffer)
    (insert "NO-CHECK:\n")
    (insert (format "    %S : %S\n" msg obj))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-debug-signal-pre-word-checked ...                       */
;;*---------------------------------------------------------------------*/
(defun flyspell-debug-signal-pre-word-checked ()
  (setq debug-on-error t)
  (with-current-buffer (get-buffer-create "*flyspell-debug*")
    (insert "PRE-WORD:\n")
    (insert (format "  pre-point  : %S\n" flyspell-pre-point))
    (insert (format "  pre-buffer : %S\n" flyspell-pre-buffer))
    (insert (format "  cache-start: %S\n" flyspell-word-cache-start))
    (insert (format "  cache-end  : %S\n" flyspell-word-cache-end))
    (goto-char (point-max))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-debug-signal-word-checked ...                           */
;;*---------------------------------------------------------------------*/
(defun flyspell-debug-signal-word-checked ()
  (setq debug-on-error t)
  (let ((oldbuf (current-buffer))
        (point  (point)))
    (with-current-buffer (get-buffer-create "*flyspell-debug*")
      (insert "WORD:\n")
      (insert (format "  this-cmd   : %S\n" this-command))
      (insert (format "  delayed    : %S\n" (and (symbolp this-command)
						 (get this-command 'flyspell-delayed))))
      (insert (format "  point      : %S\n" point))
      (insert (format "  prev-char  : [%c] %S\n"
		      (with-current-buffer oldbuf
			(let ((c (if (> (point) (point-min))
				     (save-excursion
				       (backward-char 1)
				       (char-after (point)))
				   ? )))
			  c))
		      (with-current-buffer oldbuf
			(let ((c (if (> (point) (point-min))
				     (save-excursion
				       (backward-char 1)
				       (and (and (looking-at (flyspell-get-not-casechars)) 1)
					    (and (or flyspell-consider-dash-as-word-delimiter-flag
						     (not (looking-at "\\-"))) 2))))))
			  c))))
      (insert (format "  because    : %S\n"
		      (cond
		       ((not (and (symbolp this-command)
				  (get this-command 'flyspell-delayed)))
			;; the current command is not delayed, that
			;; is that we must check the word now
			'not-delayed)
		       ((with-current-buffer oldbuf
			  (let ((c (if (> (point) (point-min))
				       (save-excursion
					 (backward-char 1)
					 (and (looking-at (flyspell-get-not-casechars))
					      (or flyspell-consider-dash-as-word-delimiter-flag
						  (not (looking-at "\\-"))))))))
			    c))
			;; yes because we have reached or typed a word delimiter.
			'separator)
		       ((not (integerp flyspell-delay))
			;; yes because the user had set up a no-delay configuration.
			'no-delay)
		       (t
			'sit-for))))
      (goto-char (point-max)))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-debug-signal-changed-checked ...                        */
;;*---------------------------------------------------------------------*/
(defun flyspell-debug-signal-changed-checked ()
  (setq debug-on-error t)
  (let ((point  (point)))
    (with-current-buffer (get-buffer-create "*flyspell-debug*")
      (insert "CHANGED WORD:\n")
      (insert (format "  point   : %S\n" point))
      (goto-char (point-max)))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-post-command-hook ...                                   */
;;*    -------------------------------------------------------------    */
;;*    It is possible that we check several words:                      */
;;*    1- the current word is checked if the predicate                  */
;;*       FLYSPELL-CHECK-WORD-P is true                                 */
;;*    2- the word that used to be the current word before the          */
;;*       THIS-COMMAND is checked if:                                   */
;;*        a- the previous word is different from the current word      */
;;*        b- the previous word as not just been checked by the         */
;;*           previous FLYSPELL-POST-COMMAND-HOOK                       */
;;*    3- the words changed by the THIS-COMMAND that are neither the    */
;;*       previous word nor the current word                            */
;;*---------------------------------------------------------------------*/
(defun flyspell-post-command-hook ()
  "The `post-command-hook' used by flyspell to check a word on-the-fly."
  (interactive)
  (when flyspell-mode
    (with-local-quit
      (let ((command this-command)
            ;; Prevent anything we do from affecting the mark.
            deactivate-mark)
        (if (flyspell-check-pre-word-p)
            (with-current-buffer flyspell-pre-buffer
              '(flyspell-debug-signal-pre-word-checked)
              (save-excursion
                (goto-char flyspell-pre-point)
                (flyspell-word))))
        (if (flyspell-check-word-p)
            (progn
              '(flyspell-debug-signal-word-checked)
              ;; FIXME: This should be asynchronous!
              (flyspell-word)
              ;; we remember which word we have just checked.
              ;; this will be used next time we will check a word
              ;; to compare the next current word with the word
              ;; that as been registered in the pre-command-hook
              ;; that is these variables are used within the predicate
              ;; FLYSPELL-CHECK-PRE-WORD-P
              (setq flyspell-pre-pre-buffer (current-buffer))
              (setq flyspell-pre-pre-point  (point)))
          (progn
            (setq flyspell-pre-pre-buffer nil)
            (setq flyspell-pre-pre-point  nil)
            ;; when a word is not checked because of a delayed command
            ;; we do not disable the ispell cache.
            (if (and (symbolp this-command)
                     (get this-command 'flyspell-delayed))
                (progn
                  (setq flyspell-word-cache-end -1)
                  (setq flyspell-word-cache-result '_)))))
        (while (and (not (input-pending-p)) (consp flyspell-changes))
          (let ((start (car (car flyspell-changes)))
                (stop  (cdr (car flyspell-changes))))
            (if (flyspell-check-changed-word-p start stop)
                (save-excursion
                  '(flyspell-debug-signal-changed-checked)
                  (goto-char start)
                  (flyspell-word)))
            (setq flyspell-changes (cdr flyspell-changes))))
        (setq flyspell-previous-command command)))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-notify-misspell ...                                     */
;;*---------------------------------------------------------------------*/
(defun flyspell-notify-misspell (word poss)
  (let ((replacements (if (stringp poss)
			  poss
			(if flyspell-sort-corrections
			    (sort (car (cdr (cdr poss))) 'string<)
			  (car (cdr (cdr poss)))))))
    (if flyspell-issue-message-flag
	(message "misspelling `%s'  %S" word replacements))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-word-search-backward ...                                */
;;*---------------------------------------------------------------------*/
(defun flyspell-word-search-backward (word bound &optional ignore-case)
  (save-excursion
    (let ((r '())
	  (inhibit-point-motion-hooks t)
	  p)
      (while (and (not r) (setq p (search-backward word bound t)))
	(let ((lw (flyspell-get-word)))
	  (if (and (consp lw)
		   (if ignore-case
		       (string-equal (downcase (car lw)) (downcase word))
		     (string-equal (car lw) word)))
	      (setq r p)
	    (goto-char p))))
      r)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-word-search-forward ...                                 */
;;*---------------------------------------------------------------------*/
(defun flyspell-word-search-forward (word bound)
  (save-excursion
    (let ((r '())
	  (inhibit-point-motion-hooks t)
	  p)
      (while (and (not r) (setq p (search-forward word bound t)))
	(let ((lw (flyspell-get-word)))
	  (if (and (consp lw) (string-equal (car lw) word))
	      (setq r p)
	    (goto-char (1+ p)))))
      r)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-word ...                                                */
;;*---------------------------------------------------------------------*/
(defun flyspell-word (&optional following known-misspelling)
  "Spell check a word.
If the optional argument FOLLOWING, or, when called interactively
`ispell-following-word', is non-nil, checks the following (rather
than preceding) word when the cursor is not over a word.  If
optional argument KNOWN-MISSPELLING is non nil considers word a
misspelling and skips redundant spell-checking step."
  (interactive (list ispell-following-word))
  (ispell-set-spellchecker-params)    ; Initialize variables and dicts alists
  (save-excursion
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (let* ((cursor-location (point))
           (flyspell-word (flyspell-get-word following))
           start end poss word ispell-filter)
      (if (or (eq flyspell-word nil)
 	      (and (fboundp flyspell-generic-check-word-predicate)
 		   (not (funcall flyspell-generic-check-word-predicate))))
	  t
	(progn
	  ;; destructure return flyspell-word info list.
	  (setq start (car (cdr flyspell-word))
		end (car (cdr (cdr flyspell-word)))
		word (car flyspell-word))
	  ;; before checking in the directory, we check for doublons.
	  (cond
	   ((and (or (not (eq ispell-parser 'tex))
		     (and (> start (point-min))
			  (not (memq (char-after (1- start)) '(?\} ?\\)))))
		 flyspell-mark-duplications-flag
		 (not (catch 'exception
			(let ((dict (or ispell-local-dictionary
					ispell-dictionary)))
			  (dolist (except flyspell-mark-duplications-exceptions)
			    (and (or (null (car except))
				     (and (stringp dict)
					  (string-match (car except) dict)))
				 (member (downcase word) (cdr except))
				 (throw 'exception t))))))
		 (save-excursion
		   (goto-char start)
		   (let* ((bound
			   (- start
			      (- end start)
			      (- (skip-chars-backward " \t\n\f"))))
			  (p (when (>= bound (point-min))
			       (flyspell-word-search-backward word bound t))))
		     (and p (/= p start)))))
	    ;; yes, this is a doublon
	    (flyspell-highlight-incorrect-region start end 'doublon)
	    nil)
	   ((and (eq flyspell-word-cache-start start)
		 (eq flyspell-word-cache-end end)
		 (string-equal flyspell-word-cache-word word))
	    ;; this word had been already checked, we skip
	    flyspell-word-cache-result)
	   ((and (eq ispell-parser 'tex)
		 (flyspell-tex-command-p flyspell-word))
	    ;; this is a correct word (because a tex command)
	    (flyspell-unhighlight-at start)
	    (if (> end start)
		(flyspell-unhighlight-at (- end 1)))
	    t)
	   (t
	    ;; we setup the cache
	    (setq flyspell-word-cache-start start)
	    (setq flyspell-word-cache-end end)
	    (setq flyspell-word-cache-word word)
	    ;; now check spelling of word.
            (if (not known-misspelling)
                (progn
                  (ispell-send-string "%\n")
                  ;; put in verbose mode
                  (ispell-send-string (concat "^" word "\n"))
                  ;; we mark the ispell process so it can be killed
                  ;; when emacs is exited without query
                  (set-process-query-on-exit-flag ispell-process nil)
                  ;; Wait until ispell has processed word.
                  (while (progn
                           (accept-process-output ispell-process)
                           (not (string= "" (car ispell-filter)))))
                  ;; (ispell-send-string "!\n")
                  ;; back to terse mode.
                  ;; Remove leading empty element
                  (setq ispell-filter (cdr ispell-filter))
                  ;; ispell process should return something after word is sent.
                  ;; Tag word as valid (i.e., skip) otherwise
                  (or ispell-filter
                      (setq ispell-filter '(*)))
                  (if (consp ispell-filter)
                      (setq poss (ispell-parse-output (car ispell-filter)))))
              ;; Else, this was a known misspelling to begin with, and
              ;; we should forge an ispell return value.
              (setq poss (list word 1 nil nil)))
	    (let ((res (cond ((eq poss t)
			      ;; correct
			      (setq flyspell-word-cache-result t)
			      (flyspell-unhighlight-at start)
			      (if (> end start)
				  (flyspell-unhighlight-at (- end 1)))
			      t)
			     ((and (stringp poss) flyspell-highlight-flag)
			      ;; correct
			      (setq flyspell-word-cache-result t)
			      (flyspell-unhighlight-at start)
			      (if (> end start)
				  (flyspell-unhighlight-at (- end 1)))
			      t)
			     ((null poss)
			      (setq flyspell-word-cache-result t)
			      (flyspell-unhighlight-at start)
			      (if (> end start)
				  (flyspell-unhighlight-at (- end 1)))
			      t)
			     ((or (and (< flyspell-duplicate-distance 0)
				       (or (save-excursion
					     (goto-char start)
					     (flyspell-word-search-backward
					      word
					      (point-min)))
					   (save-excursion
					     (goto-char end)
					     (flyspell-word-search-forward
					      word
					      (point-max)))))
				  (and (> flyspell-duplicate-distance 0)
				       (or (save-excursion
					     (goto-char start)
					     (flyspell-word-search-backward
					      word
					      (- start
						 flyspell-duplicate-distance)))
					   (save-excursion
					     (goto-char end)
					     (flyspell-word-search-forward
					      word
					      (+ end
						 flyspell-duplicate-distance))))))
			      ;; This is a misspelled word which occurs
			      ;; twice within flyspell-duplicate-distance.
			      (setq flyspell-word-cache-result nil)
			      (if flyspell-highlight-flag
				  (flyspell-highlight-duplicate-region
				   start end poss)
				(message "duplicate `%s'" word))
			      nil)
			     (t
			      (setq flyspell-word-cache-result nil)
			      ;; Highlight the location as incorrect,
			      ;; including offset specified in POSS.
			      (if flyspell-highlight-flag
				  (flyspell-highlight-incorrect-region
				   (if (and (consp poss)
					    (integerp (nth 1 poss)))
				       (+ start (nth 1 poss) -1)
				     start)
				   end poss)
				(flyspell-notify-misspell word poss))
			      nil))))
	      ;; return to original location
	      (goto-char cursor-location)
	      (if ispell-quit (setq ispell-quit nil))
	      res))))))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-math-tex-command-p ...                                  */
;;*    -------------------------------------------------------------    */
;;*    This function uses the texmathp package to check if point        */
;;*    is within a TeX math environment. `texmathp' can yield errors    */
;;*    if the document is currently not valid TeX syntax.               */
;;*---------------------------------------------------------------------*/
(defun flyspell-math-tex-command-p ()
  (when (fboundp 'texmathp)
    (if flyspell-check-tex-math-command
        nil
      (condition-case nil
          (texmathp)
        (error nil)))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-tex-command-p ...                                       */
;;*---------------------------------------------------------------------*/
(defun flyspell-tex-command-p (word)
  "Return t if WORD is a TeX command."
  (or (save-excursion
	(let ((b  (car (cdr word))))
	  (and (re-search-backward "\\\\" (- (point) 100) t)
	       (or (= (match-end 0) b)
		   (and (goto-char (match-end 0))
			(looking-at flyspell-tex-command-regexp)
			(>= (match-end 0) b))))))
      (flyspell-math-tex-command-p)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-casechars-cache ...                                     */
;;*---------------------------------------------------------------------*/
(defvar flyspell-casechars-cache nil)
(defvar flyspell-ispell-casechars-cache nil)
(make-variable-buffer-local 'flyspell-casechars-cache)
(make-variable-buffer-local 'flyspell-ispell-casechars-cache)

;;*---------------------------------------------------------------------*/
;;*    flyspell-get-casechars ...                                       */
;;*---------------------------------------------------------------------*/
(defun flyspell-get-casechars ()
  "This function builds a string that is the regexp of word chars.
In order to avoid one useless string construction,
this function changes the last char of the `ispell-casechars' string."
  (let ((ispell-casechars (ispell-get-casechars)))
    (cond
     ((eq ispell-parser 'tex)
      (setq flyspell-ispell-casechars-cache ispell-casechars)
      (setq flyspell-casechars-cache
	    (concat (substring ispell-casechars
			       0
			       (- (length ispell-casechars) 1))
		    "]"))
      flyspell-casechars-cache)
     (t
      (setq flyspell-ispell-casechars-cache ispell-casechars)
      (setq flyspell-casechars-cache ispell-casechars)
      flyspell-casechars-cache))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-get-not-casechars-cache ...                             */
;;*---------------------------------------------------------------------*/
(defvar flyspell-not-casechars-cache nil)
(defvar flyspell-ispell-not-casechars-cache nil)
(make-variable-buffer-local 'flyspell-not-casechars-cache)
(make-variable-buffer-local 'flyspell-ispell-not-casechars-cache)

;;*---------------------------------------------------------------------*/
;;*    flyspell-get-not-casechars ...                                   */
;;*---------------------------------------------------------------------*/
(defun flyspell-get-not-casechars ()
  "This function builds a string that is the regexp of non-word chars."
  (let ((ispell-not-casechars (ispell-get-not-casechars)))
    (cond
     ((eq ispell-parser 'tex)
      (setq flyspell-ispell-not-casechars-cache ispell-not-casechars)
      (setq flyspell-not-casechars-cache
	    (concat (substring ispell-not-casechars
			       0
			       (- (length ispell-not-casechars) 1))
		    "]"))
      flyspell-not-casechars-cache)
     (t
      (setq flyspell-ispell-not-casechars-cache ispell-not-casechars)
      (setq flyspell-not-casechars-cache ispell-not-casechars)
      flyspell-not-casechars-cache))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-get-word ...                                            */
;;*---------------------------------------------------------------------*/
(defun flyspell-get-word (&optional following extra-otherchars)
  "Return the word for spell-checking according to Ispell syntax.
Optional argument FOLLOWING non-nil means to get the following
\(rather than preceding) word when the cursor is not over a word.
Optional second argument EXTRA-OTHERCHARS is a regexp of characters
that may be included as part of a word (see `ispell-dictionary-alist')."
  (let* ((flyspell-casechars (flyspell-get-casechars))
	 (flyspell-not-casechars (flyspell-get-not-casechars))
	 (ispell-otherchars (ispell-get-otherchars))
	 (ispell-many-otherchars-p (ispell-get-many-otherchars-p))
	 (word-regexp (concat flyspell-casechars
			      "+\\("
			      (if (not (string= "" ispell-otherchars))
				  (concat ispell-otherchars "?"))
			      (if extra-otherchars
				  (concat extra-otherchars "?"))
			      flyspell-casechars
			      "+\\)"
			      (if (or ispell-many-otherchars-p
				      extra-otherchars)
				  "*" "?")))
	 did-it-once prevpt
	 start end word)
    ;; find the word
    (if (not (looking-at flyspell-casechars))
	(if following
	    (re-search-forward flyspell-casechars nil t)
	  (re-search-backward flyspell-casechars nil t)))
    ;; move to front of word
    (re-search-backward flyspell-not-casechars nil 'start)
    (while (and (or (and (not (string= "" ispell-otherchars))
			 (looking-at ispell-otherchars))
		    (and extra-otherchars (looking-at extra-otherchars)))
		(not (bobp))
		(or (not did-it-once)
		    ispell-many-otherchars-p)
		(not (eq prevpt (point))))
      (if (and extra-otherchars (looking-at extra-otherchars))
	  (progn
	    (backward-char 1)
	    (if (looking-at flyspell-casechars)
		(re-search-backward flyspell-not-casechars nil 'move)))
	(setq did-it-once t
	      prevpt (point))
	(backward-char 1)
	(if (looking-at flyspell-casechars)
	    (re-search-backward flyspell-not-casechars nil 'move)
	  (backward-char -1))))
    ;; Now mark the word and save to string.
    (if (not (re-search-forward word-regexp nil t))
	nil
      (progn
	(setq start (match-beginning 0)
	      end (point)
	      word (buffer-substring-no-properties start end))
	(list word start end)))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-small-region ...                                        */
;;*---------------------------------------------------------------------*/
(defun flyspell-small-region (beg end)
  "Flyspell text between BEG and END."
  (save-excursion
    (if (> beg end)
	(let ((old beg))
	  (setq beg end)
	  (setq end old)))
    (goto-char beg)
    (let ((count 0))
      (while (< (point) end)
	(if (and flyspell-issue-message-flag (= count 100))
	    (progn
	      (message "Spell Checking...%d%%"
		       (* 100 (/ (float (- (point) beg)) (- end beg))))
	      (setq count 0))
	  (setq count (+ 1 count)))
	(flyspell-word)
	(sit-for 0)
	(let ((cur (point)))
	  (forward-word 1)
	  (if (and (< (point) end) (> (point) (+ cur 1)))
	      (backward-char 1)))))
    (backward-char 1)
    (if flyspell-issue-message-flag (message "Spell Checking completed."))
    (flyspell-word)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-external-ispell-process ...                             */
;;*---------------------------------------------------------------------*/
(defvar flyspell-external-ispell-process '()
  "The external Flyspell Ispell process.")

;;*---------------------------------------------------------------------*/
;;*    flyspell-external-ispell-buffer ...                              */
;;*---------------------------------------------------------------------*/
(defvar flyspell-external-ispell-buffer '())
(defvar flyspell-large-region-buffer '())
(defvar flyspell-large-region-beg (point-min))
(defvar flyspell-large-region-end (point-max))

;;*---------------------------------------------------------------------*/
;;*    flyspell-external-point-words ...                                */
;;*---------------------------------------------------------------------*/
(defun flyspell-external-point-words ()
  "Mark words from a buffer listing incorrect words in order of appearance.
The list of incorrect words should be in `flyspell-external-ispell-buffer'.
\(We finish by killing that buffer and setting the variable to nil.)
The buffer to mark them in is `flyspell-large-region-buffer'."
  (let (words-not-found
	(ispell-otherchars (ispell-get-otherchars))
	(buffer-scan-pos flyspell-large-region-beg)
	case-fold-search)
    (with-current-buffer flyspell-external-ispell-buffer
      (goto-char (point-min))
      ;; Loop over incorrect words, in the order they were reported,
      ;; which is also the order they appear in the buffer being checked.
      (while (re-search-forward "\\([^\n]+\\)\n" nil t)
	;; Bind WORD to the next one.
	(let ((word (match-string 1)) (wordpos (point)))
	  ;; Here there used to be code to see if WORD is the same
	  ;; as the previous iteration, and count the number of consecutive
	  ;; identical words, and the loop below would search for that many.
	  ;; That code seemed to be incorrect, and on principle, should
	  ;; be unnecessary too. -- rms.
	  (if flyspell-issue-message-flag
	      (message "Spell Checking...%d%% [%s]"
		       (* 100 (/ (float (point)) (point-max)))
		       word))
	  (with-current-buffer flyspell-large-region-buffer
	    (goto-char buffer-scan-pos)
	    (let ((keep t))
	      ;; Iterate on string search until string is found as word,
	      ;; not as substring
	      (while keep
		(if (search-forward word
				    flyspell-large-region-end t)
		    (let* ((found-list
			    (save-excursion
			      ;; Move back into the match
			      ;; so flyspell-get-word will find it.
			      (forward-char -1)
			      (flyspell-get-word)))
			   (found (car found-list))
			   (found-length (length found))
			   (misspell-length (length word)))
		      (when (or
			     ;; Size matches, we really found it.
			     (= found-length misspell-length)
			     ;; Matches as part of a boundary-char separated word
			     (member word
				     (split-string found ispell-otherchars))
			     ;; Misspelling has higher length than
			     ;; what flyspell considers the
			     ;; word.  Caused by boundary-chars
			     ;; mismatch.  Validating seems safe.
			     (< found-length misspell-length)
			     ;; ispell treats beginning of some TeX
			     ;; commands as nroff control sequences
			     ;; and strips them in the list of
			     ;; misspelled words thus giving a
			     ;; non-existent word.  Skip if ispell
			     ;; is used, string is a TeX command
			     ;; (char before beginning of word is
			     ;; backslash) and none of the previous
			     ;; conditions match.
			     (and (not ispell-really-aspell)
				  (save-excursion
				    (goto-char (- (nth 1 found-list) 1))
				    (if (looking-at "[\\]" )
					t
				      nil))))
			(setq keep nil)
			(flyspell-word nil t)
			;; Search for next misspelled word will begin from
			;; end of last validated match.
			(setq buffer-scan-pos (point))))
		  ;; Record if misspelling is not found and try new one
		  (add-to-list 'words-not-found
			       (concat " -> " word " - "
				       (int-to-string wordpos)))
		  (setq keep nil)))))))
      ;; we are done
      (if flyspell-issue-message-flag (message "Spell Checking completed.")))
    ;; Warn about not found misspellings
    (dolist (word words-not-found)
      (message "%s: word not found" word))
    ;; Kill and forget the buffer with the list of incorrect words.
    (kill-buffer flyspell-external-ispell-buffer)
    (setq flyspell-external-ispell-buffer nil)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-process-localwords ...                                  */
;;*    -------------------------------------------------------------    */
;;*    This function is used to prevent marking of words explicitly     */
;;*    declared correct.                                                */
;;*---------------------------------------------------------------------*/
(defun flyspell-process-localwords (misspellings-buffer)
  (let (localwords case-fold-search
	(ispell-casechars (ispell-get-casechars)))
    ;; Get localwords from the original buffer
    (save-excursion
      (goto-char (point-min))
      ;; Localwords parsing copied from ispell.el.
      (while (search-forward ispell-words-keyword nil t)
	(let ((end (point-at-eol))
	      string)
	  ;; buffer-local words separated by a space, and can contain
	  ;; any character other than a space.  Not rigorous enough.
	  (while (re-search-forward " *\\([^ ]+\\)" end t)
	    (setq string (buffer-substring-no-properties (match-beginning 1)
							 (match-end 1)))
	    ;; This can fail when string contains a word with invalid chars.
	    ;; Error handling needs to be added between Ispell and Emacs.
	    (if (and (< 1 (length string))
		     (equal 0 (string-match ispell-casechars string)))
		(push string localwords))))))
    ;; Remove localwords matches from misspellings-buffer.
    ;; The usual mechanism of communicating the local words to ispell
    ;; does not affect the special ispell process used by
    ;; flyspell-large-region.
    (with-current-buffer misspellings-buffer
      (save-excursion
	(dolist (word localwords)
	  (goto-char (point-min))
	  (let ((regexp (concat "^" word "\n")))
	    (while (re-search-forward regexp nil t)
	      (delete-region (match-beginning 0) (match-end 0)))))))))

;;* ---------------------------------------------------------------
;;*     flyspell-check-region-doublons
;;* ---------------------------------------------------------------
(defun flyspell-check-region-doublons (beg end)
  "Check for adjacent duplicated words (doublons) in the given region."
  (save-excursion
    (goto-char beg)
    (flyspell-word)     ; Make sure current word is checked
    (backward-word 1)
    (while (and (< (point) end)
		(re-search-forward "\\<\\(\\w+\\)\\>[ \n\t\f]+\\1\\>"
				   end 'move))
      (flyspell-word)
      (backward-word 1))
    (flyspell-word)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-large-region ...                                        */
;;*---------------------------------------------------------------------*/
(defun flyspell-large-region (beg end)
  (let* ((curbuf  (current-buffer))
	 (buffer  (get-buffer-create "*flyspell-region*")))
    (setq flyspell-external-ispell-buffer buffer)
    (setq flyspell-large-region-buffer curbuf)
    (setq flyspell-large-region-beg beg)
    (setq flyspell-large-region-end end)
    (flyspell-accept-buffer-local-defs)
    (set-buffer buffer)
    (erase-buffer)
    ;; this is done, we can start checking...
    (if flyspell-issue-message-flag (message "Checking region..."))
    (set-buffer curbuf)
    (ispell-set-spellchecker-params)  ; Initialize variables and dicts alists
    ;; Local dictionary becomes the global dictionary in use.
    (setq ispell-current-dictionary
	  (or ispell-local-dictionary ispell-dictionary))
    (setq ispell-current-personal-dictionary
	  (or ispell-local-pdict ispell-personal-dictionary))
    (let ((args (ispell-get-ispell-args))
	  (encoding (ispell-get-coding-system))
	  c)
      (if (and ispell-current-dictionary  ; use specified dictionary
	       (not (member "-d" args)))  ; only define if not overridden
	  (setq args
		(append (list "-d" ispell-current-dictionary) args)))
      (if ispell-current-personal-dictionary ; use specified pers dict
	  (setq args
		(append args
			(list "-p"
			      (expand-file-name
			       ispell-current-personal-dictionary)))))

      ;; Check for extended character mode
      (let ((extended-char-mode (ispell-get-extended-character-mode)))
        (and extended-char-mode          ; ~ extended character mode
	     (string-match "[^~]+$" extended-char-mode)
	     (add-to-list 'args (concat "-T" (match-string 0 extended-char-mode)))))

      ;; Add ispell-extra-args
      (setq args (append args ispell-extra-args))

      ;; If we are using recent aspell or hunspell, make sure we use the right encoding
      ;; for communication. ispell or older aspell/hunspell does not support this
      (if ispell-encoding8-command
	  (setq args
		(append args
			(list
			 (concat ispell-encoding8-command
				 (symbol-name
				  encoding))))))

      (let ((process-coding-system-alist (list (cons "\\.*" encoding))))
	(setq c (apply 'ispell-call-process-region beg
		       end
		       ispell-program-name
		       nil
		       buffer
		       nil
		       (if ispell-really-aspell "list" "-l")
		       args)))
      (if (eq c 0)
	  (progn
	    (flyspell-process-localwords buffer)
	    (with-current-buffer curbuf
	      (flyspell-delete-region-overlays beg end)
	      (flyspell-check-region-doublons beg end))
	    (flyspell-external-point-words))
	(error "Can't check region")))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-region ...                                              */
;;*    -------------------------------------------------------------    */
;;*    Because `ispell -a' is too slow, it is not possible to use       */
;;*    it on large region. Then, when ispell is invoked on a large      */
;;*    text region, a new `ispell -l' process is spawned. The           */
;;*    pointed out words are then searched in the region a checked with */
;;*    regular flyspell means.                                          */
;;*---------------------------------------------------------------------*/
;;;###autoload
(defun flyspell-region (beg end)
  "Flyspell text between BEG and END."
  (interactive "r")
  (ispell-set-spellchecker-params)  ; Initialize variables and dicts alists
  (if (= beg end)
      ()
    (save-excursion
      (if (> beg end)
	  (let ((old beg))
	    (setq beg end)
	    (setq end old)))
      (if (and flyspell-large-region (> (- end beg) flyspell-large-region))
	  (flyspell-large-region beg end)
	(flyspell-small-region beg end)))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-buffer ...                                              */
;;*---------------------------------------------------------------------*/
;;;###autoload
(defun flyspell-buffer ()
  "Flyspell whole buffer."
  (interactive)
  (flyspell-region (point-min) (point-max)))

;;*---------------------------------------------------------------------*/
;;*    old next error position ...                                      */
;;*---------------------------------------------------------------------*/
(defvar flyspell-old-buffer-error nil)
(defvar flyspell-old-pos-error nil)

;;*---------------------------------------------------------------------*/
;;*    flyspell-goto-next-error ...                                     */
;;*---------------------------------------------------------------------*/
(defun flyspell-goto-next-error ()
  "Go to the next previously detected error.
In general FLYSPELL-GOTO-NEXT-ERROR must be used after
FLYSPELL-BUFFER."
  (interactive)
  (let ((pos (point))
	(max (point-max)))
    (if (and (eq (current-buffer) flyspell-old-buffer-error)
	     (eq pos flyspell-old-pos-error))
	(progn
	  (if (= flyspell-old-pos-error max)
	      ;; goto beginning of buffer
	      (progn
		(message "Restarting from beginning of buffer")
		(goto-char (point-min)))
	    (forward-word 1))
	  (setq pos (point))))
    ;; seek the next error
    (while (and (< pos max)
		(let ((ovs (overlays-at pos))
		      (r '()))
		  (while (and (not r) (consp ovs))
		    (if (flyspell-overlay-p (car ovs))
			(setq r t)
		      (setq ovs (cdr ovs))))
		  (not r)))
      (setq pos (1+ pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (if (= pos max)
	(message "No more miss-spelled word!"))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-overlay-p ...                                           */
;;*---------------------------------------------------------------------*/
(defun flyspell-overlay-p (o)
  "Return true if O is an overlay used by flyspell."
  (and (overlayp o) (overlay-get o 'flyspell-overlay)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-delete-region-overlays, flyspell-delete-all-overlays    */
;;*    -------------------------------------------------------------    */
;;*    Remove overlays introduced by flyspell.                          */
;;*---------------------------------------------------------------------*/
(defun flyspell-delete-region-overlays (beg end)
  "Delete overlays used by flyspell in a given region."
  (remove-overlays beg end 'flyspell-overlay t))


(defun flyspell-delete-all-overlays ()
  "Delete all the overlays used by flyspell."
  (remove-overlays (point-min) (point-max) 'flyspell-overlay t))

;;*---------------------------------------------------------------------*/
;;*    flyspell-unhighlight-at ...                                      */
;;*---------------------------------------------------------------------*/
(defun flyspell-unhighlight-at (pos)
  "Remove the flyspell overlay that are located at POS."
  (if flyspell-persistent-highlight
      (let ((overlays (overlays-at pos)))
	(while (consp overlays)
	  (if (flyspell-overlay-p (car overlays))
	      (delete-overlay (car overlays)))
	  (setq overlays (cdr overlays))))
    (if (flyspell-overlay-p flyspell-overlay)
        (delete-overlay flyspell-overlay))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-properties-at-p ...                                     */
;;*    -------------------------------------------------------------    */
;;*    Is there an highlight properties at position pos?                */
;;*---------------------------------------------------------------------*/
(defun flyspell-properties-at-p (pos)
  "Return t if there is a text property at POS, not counting `local-map'.
If variable `flyspell-highlight-properties' is set to nil,
text with properties are not checked.  This function is used to discover
if the character at POS has any other property."
  (let ((prop (text-properties-at pos))
	(keep t))
    (while (and keep (consp prop))
      (if (and (eq (car prop) 'local-map) (consp (cdr prop)))
	  (setq prop (cdr (cdr prop)))
	(setq keep nil)))
    (consp prop)))

;;*---------------------------------------------------------------------*/
;;*    make-flyspell-overlay ...                                        */
;;*---------------------------------------------------------------------*/
(defun make-flyspell-overlay (beg end face mouse-face)
  "Allocate an overlay to highlight an incorrect word.
BEG and END specify the range in the buffer of that word.
FACE and MOUSE-FACE specify the `face' and `mouse-face' properties
for the overlay."
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'mouse-face mouse-face)
    (overlay-put overlay 'flyspell-overlay t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'help-echo "mouse-2: correct word at point")
    (overlay-put overlay 'keymap flyspell-mouse-map)
    (when (eq face 'flyspell-incorrect)
      (and (stringp flyspell-before-incorrect-word-string)
           (overlay-put overlay 'before-string
                        flyspell-before-incorrect-word-string))
      (and (stringp flyspell-after-incorrect-word-string)
           (overlay-put overlay 'after-string
                        flyspell-after-incorrect-word-string)))
    overlay))

;;*---------------------------------------------------------------------*/
;;*    flyspell-highlight-incorrect-region ...                          */
;;*---------------------------------------------------------------------*/
(defun flyspell-highlight-incorrect-region (beg end poss)
  "Set up an overlay on a misspelled word, in the buffer from BEG to END.
POSS is usually a list of possible spelling/correction lists,
as returned by `ispell-parse-output'.
It can also be the symbol `doublon', in the case where the word
is itself incorrect, but suspiciously repeated."
  (let ((inhibit-read-only t))
    (unless (run-hook-with-args-until-success
	     'flyspell-incorrect-hook beg end poss)
      (if (or flyspell-highlight-properties
	      (not (flyspell-properties-at-p beg)))
	  (progn
	    ;; we cleanup all the overlay that are in the region, not
	    ;; beginning at the word start position
	    (if (< (1+ beg) end)
		(let ((os (overlays-in (1+ beg) end)))
		  (while (consp os)
		    (if (flyspell-overlay-p (car os))
			(delete-overlay (car os)))
		    (setq os (cdr os)))))
	    ;; we cleanup current overlay at the same position
            (flyspell-unhighlight-at beg)
	    ;; now we can use a new overlay
	    (setq flyspell-overlay
		  (make-flyspell-overlay
		   beg end
		   (if (eq poss 'doublon) 'flyspell-duplicate 'flyspell-incorrect)
		   'highlight)))))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-highlight-duplicate-region ...                          */
;;*---------------------------------------------------------------------*/
(defun flyspell-highlight-duplicate-region (beg end poss)
  "Set up an overlay on a duplicate misspelled word, in the buffer from BEG to END.
POSS is a list of possible spelling/correction lists,
as returned by `ispell-parse-output'."
  (let ((inhibit-read-only t))
    (unless (run-hook-with-args-until-success
	     'flyspell-incorrect-hook beg end poss)
      (if (or flyspell-highlight-properties
	      (not (flyspell-properties-at-p beg)))
	  (progn
	    ;; we cleanup current overlay at the same position
            (flyspell-unhighlight-at beg)
	    ;; now we can use a new overlay
	    (setq flyspell-overlay
		  (make-flyspell-overlay beg end
					 'flyspell-duplicate
					 'highlight)))))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-auto-correct-cache ...                                  */
;;*---------------------------------------------------------------------*/
(defvar flyspell-auto-correct-pos nil)
(defvar flyspell-auto-correct-region nil)
(defvar flyspell-auto-correct-ring nil)
(defvar flyspell-auto-correct-word nil)
(make-variable-buffer-local 'flyspell-auto-correct-pos)
(make-variable-buffer-local 'flyspell-auto-correct-region)
(make-variable-buffer-local 'flyspell-auto-correct-ring)
(make-variable-buffer-local 'flyspell-auto-correct-word)

;;*---------------------------------------------------------------------*/
;;*    flyspell-check-previous-highlighted-word ...                     */
;;*---------------------------------------------------------------------*/
(defun flyspell-check-previous-highlighted-word (&optional arg)
  "Correct the closer misspelled word.
This function scans a mis-spelled word before the cursor. If it finds one
it proposes replacement for that word. With prefix arg, count that many
misspelled words backwards."
  (interactive)
  (let ((pos1 (point))
	(pos  (point))
	(arg  (if (or (not (numberp arg)) (< arg 1)) 1 arg))
	ov ovs)
    (if (catch 'exit
	  (while (and (setq pos (previous-overlay-change pos))
		      (not (= pos pos1)))
	    (setq pos1 pos)
	    (if (> pos (point-min))
		(progn
		  (setq ovs (overlays-at (1- pos)))
		  (while (consp ovs)
		    (setq ov (car ovs))
		    (setq ovs (cdr ovs))
		    (if (and (flyspell-overlay-p ov)
			     (= 0 (setq arg (1- arg))))
			(throw 'exit t)))))))
	(save-excursion
	  (goto-char pos)
	  (ispell-word)
	  (setq flyspell-word-cache-word nil) ;; Force flyspell-word re-check
	  (flyspell-word))
      (error "No word to correct before point"))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-display-next-corrections ...                            */
;;*---------------------------------------------------------------------*/
(defun flyspell-display-next-corrections (corrections)
  (let ((string "Corrections:")
	(l corrections)
	(pos '()))
    (while (< (length string) 80)
      (if (equal (car l) flyspell-auto-correct-word)
	  (setq pos (cons (+ 1 (length string)) pos)))
      (setq string (concat string " " (car l)))
      (setq l (cdr l)))
    (while (consp pos)
      (let ((num (car pos)))
	(put-text-property num
			   (+ num (length flyspell-auto-correct-word))
			   'face 'flyspell-incorrect
			   string))
      (setq pos (cdr pos)))
    (if (fboundp 'display-message)
	(display-message 'no-log string)
      (message "%s" string))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-abbrev-table ...                                        */
;;*---------------------------------------------------------------------*/
(defun flyspell-abbrev-table ()
  (if flyspell-use-global-abbrev-table-p
      global-abbrev-table
    (or local-abbrev-table global-abbrev-table)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-define-abbrev ...                                       */
;;*---------------------------------------------------------------------*/
(defun flyspell-define-abbrev (name expansion)
  (let ((table (flyspell-abbrev-table)))
    (when table
      (define-abbrev table (downcase name) expansion))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-auto-correct-word ...                                   */
;;*---------------------------------------------------------------------*/
(defun flyspell-auto-correct-word ()
  "Correct the current word.
This command proposes various successive corrections for the current word."
  (interactive)
  (let ((pos     (point))
	(old-max (point-max)))
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (if (and (eq flyspell-auto-correct-pos pos)
	     (consp flyspell-auto-correct-region))
	;; we have already been using the function at the same location
	(let* ((start (car flyspell-auto-correct-region))
	       (len   (cdr flyspell-auto-correct-region)))
	  (flyspell-unhighlight-at start)
	  (delete-region start (+ start len))
	  (setq flyspell-auto-correct-ring (cdr flyspell-auto-correct-ring))
	  (let* ((word (car flyspell-auto-correct-ring))
		 (len  (length word)))
	    (rplacd flyspell-auto-correct-region len)
	    (goto-char start)
	    (if flyspell-abbrev-p
		(if (flyspell-already-abbrevp (flyspell-abbrev-table)
					      flyspell-auto-correct-word)
		    (flyspell-change-abbrev (flyspell-abbrev-table)
					    flyspell-auto-correct-word
					    word)
		  (flyspell-define-abbrev flyspell-auto-correct-word word)))
	    (funcall flyspell-insert-function word)
	    (flyspell-word)
	    (flyspell-display-next-corrections flyspell-auto-correct-ring))
	  (flyspell-ajust-cursor-point pos (point) old-max)
	  (setq flyspell-auto-correct-pos (point)))
      ;; fetch the word to be checked
      (let ((word (flyspell-get-word)))
	(if (consp word)
	    (let ((start (car (cdr word)))
		  (end (car (cdr (cdr word))))
		  (word (car word))
		  poss ispell-filter)
	      (setq flyspell-auto-correct-word word)
	      ;; now check spelling of word.
	      (ispell-send-string "%\n") ;put in verbose mode
	      (ispell-send-string (concat "^" word "\n"))
              ;; wait until ispell has processed word.
              (while (progn
                       (accept-process-output ispell-process)
                       (not (string= "" (car ispell-filter)))))
	      ;; Remove leading empty element
	      (setq ispell-filter (cdr ispell-filter))
	      ;; ispell process should return something after word is sent.
	      ;; Tag word as valid (i.e., skip) otherwise
	      (or ispell-filter
		  (setq ispell-filter '(*)))
	      (if (consp ispell-filter)
		  (setq poss (ispell-parse-output (car ispell-filter))))
	      (cond
	       ((or (eq poss t) (stringp poss))
		;; don't correct word
		t)
	       ((null poss)
		;; ispell error
		(error "Ispell: error in Ispell process"))
	       (t
		;; the word is incorrect, we have to propose a replacement
		(let ((replacements (if flyspell-sort-corrections
					(sort (car (cdr (cdr poss))) 'string<)
				      (car (cdr (cdr poss))))))
		  (setq flyspell-auto-correct-region nil)
		  (if (consp replacements)
		      (progn
			(let ((replace (car replacements)))
			  (let ((new-word replace))
			    (if (not (equal new-word (car poss)))
				(progn
				  ;; the save the current replacements
				  (setq flyspell-auto-correct-region
					(cons start (length new-word)))
				  (let ((l replacements))
				    (while (consp (cdr l))
				      (setq l (cdr l)))
				    (rplacd l (cons (car poss) replacements)))
				  (setq flyspell-auto-correct-ring
					replacements)
				  (flyspell-unhighlight-at start)
				  (delete-region start end)
				  (funcall flyspell-insert-function new-word)
				  (if flyspell-abbrev-p
				      (if (flyspell-already-abbrevp
					   (flyspell-abbrev-table) word)
					  (flyspell-change-abbrev
					   (flyspell-abbrev-table)
					   word
					   new-word)
					(flyspell-define-abbrev word
								new-word)))
				  (flyspell-word)
				  (flyspell-display-next-corrections
				   (cons new-word flyspell-auto-correct-ring))
				  (flyspell-ajust-cursor-point pos
							       (point)
							       old-max))))))))))
	      (setq flyspell-auto-correct-pos (point))
	      (ispell-pdict-save t)))))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-auto-correct-previous-pos ...                           */
;;*---------------------------------------------------------------------*/
(defvar flyspell-auto-correct-previous-pos nil
  "Holds the start of the first incorrect word before point.")

;;*---------------------------------------------------------------------*/
;;*    flyspell-auto-correct-previous-hook ...                          */
;;*---------------------------------------------------------------------*/
(defun flyspell-auto-correct-previous-hook ()
  "Hook to track successive calls to `flyspell-auto-correct-previous-word'.
Sets `flyspell-auto-correct-previous-pos' to nil"
  (interactive)
  (remove-hook 'pre-command-hook (function flyspell-auto-correct-previous-hook) t)
  (unless (eq this-command (function flyspell-auto-correct-previous-word))
    (setq flyspell-auto-correct-previous-pos nil)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-auto-correct-previous-word ...                          */
;;*---------------------------------------------------------------------*/
(defun flyspell-auto-correct-previous-word (position)
  "Auto correct the first misspelled word that occurs before point.
But don't look beyond what's visible on the screen."
  (interactive "d")

  (let ((top (window-start))
	(bot (window-end)))
    (save-excursion
      (save-restriction
	(narrow-to-region top bot)
	(overlay-recenter (point))

	(add-hook 'pre-command-hook
		  (function flyspell-auto-correct-previous-hook) t t)

	(unless flyspell-auto-correct-previous-pos
	  ;; only reset if a new overlay exists
	  (setq flyspell-auto-correct-previous-pos nil)

	  (let ((overlay-list (overlays-in (point-min) position))
		(new-overlay 'dummy-value))

	    ;; search for previous (new) flyspell overlay
	    (while (and new-overlay
			(or (not (flyspell-overlay-p new-overlay))
			    ;; check if its face has changed
			    (not (eq (get-char-property
				      (overlay-start new-overlay) 'face)
				     'flyspell-incorrect))))
	      (setq new-overlay (car-safe overlay-list))
	      (setq overlay-list (cdr-safe overlay-list)))

	    ;; if nothing new exits new-overlay should be nil
	    (if new-overlay ;; the length of the word may change so go to the start
		(setq flyspell-auto-correct-previous-pos
		      (overlay-start new-overlay)))))

	(when flyspell-auto-correct-previous-pos
	  (save-excursion
	    (goto-char flyspell-auto-correct-previous-pos)
	    (let ((ispell-following-word t)) ;; point is at start
	      (if (numberp flyspell-auto-correct-previous-pos)
		  (goto-char flyspell-auto-correct-previous-pos))
	      (flyspell-auto-correct-word))
	    ;; the point may have moved so reset this
	    (setq flyspell-auto-correct-previous-pos (point))))))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-correct-word ...                                        */
;;*---------------------------------------------------------------------*/

(defun flyspell-correct-word (event)
  "Pop up a menu of possible corrections for a misspelled word.
The word checked is the word at the mouse position."
  (interactive "e")
  (let ((save (point)))
    (mouse-set-point event)
    (flyspell-correct-word-before-point event save)))

(defun flyspell-correct-word-before-point (&optional event opoint)
  "Pop up a menu of possible corrections for misspelled word before point.
If EVENT is non-nil, it is the mouse event that invoked this operation;
that controls where to put the menu.
If OPOINT is non-nil, restore point there after adjusting it for replacement."
  (interactive)
  (unless (mouse-position)
    (error "Pop-up menus do not work on this terminal"))
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (or opoint (setq opoint (point)))
  (let ((cursor-location (point))
	(word (flyspell-get-word)))
    (if (consp word)
	(let ((start (car (cdr word)))
	      (end (car (cdr (cdr word))))
	      (word (car word))
	      poss ispell-filter)
	  ;; now check spelling of word.
	  (ispell-send-string "%\n")	;put in verbose mode
	  (ispell-send-string (concat "^" word "\n"))
	  ;; wait until ispell has processed word
	  (while (progn
		   (accept-process-output ispell-process)
		   (not (string= "" (car ispell-filter)))))
	  ;; Remove leading empty element
	  (setq ispell-filter (cdr ispell-filter))
	  ;; ispell process should return something after word is sent.
	  ;; Tag word as valid (i.e., skip) otherwise
	  (or ispell-filter
	      (setq ispell-filter '(*)))
	  (if (consp ispell-filter)
	      (setq poss (ispell-parse-output (car ispell-filter))))
	  (cond
	   ((or (eq poss t) (stringp poss))
	    ;; don't correct word
	    t)
	   ((null poss)
	    ;; ispell error
	    (error "Ispell: error in Ispell process"))
	   ((featurep 'xemacs)
	    (flyspell-xemacs-popup
	     poss word cursor-location start end opoint))
	   (t
	    ;; The word is incorrect, we have to propose a replacement.
	    (flyspell-do-correct (flyspell-emacs-popup event poss word)
				 poss word cursor-location start end opoint)))
	  (ispell-pdict-save t)))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-do-correct ...                                      */
;;*---------------------------------------------------------------------*/
(defun flyspell-do-correct (replace poss word cursor-location start end save)
  "The popup menu callback."
  ;; Originally, the XEmacs code didn't do the (goto-char save) here and did
  ;; it instead right after calling the function.
  (cond ((eq replace 'ignore)
         (goto-char save)
	 nil)
	((eq replace 'save)
         (goto-char save)
	 (ispell-send-string (concat "*" word "\n"))
         ;; This was added only to the XEmacs side in revision 1.18 of
         ;; flyspell.  I assume its absence on the Emacs side was an
         ;; oversight.  --Stef
	 (ispell-send-string "#\n")
	 (flyspell-unhighlight-at cursor-location)
	 (setq ispell-pdict-modified-p '(t)))
	((or (eq replace 'buffer) (eq replace 'session))
	 (ispell-send-string (concat "@" word "\n"))
	 (flyspell-unhighlight-at cursor-location)
	 (if (null ispell-pdict-modified-p)
	     (setq ispell-pdict-modified-p
		   (list ispell-pdict-modified-p)))
         (goto-char save)
	 (if (eq replace 'buffer)
	     (ispell-add-per-file-word-list word)))
	(replace
         ;; This was added only to the Emacs side.  I assume its absence on
         ;; the XEmacs side was an oversight.  --Stef
         (flyspell-unhighlight-at cursor-location)
	 (let ((old-max (point-max))
	       (new-word (if (atom replace)
			     replace
			   (car replace)))
	       (cursor-location (+ (- (length word) (- end start))
				   cursor-location)))
	   (unless (equal new-word (car poss))
             (delete-region start end)
             (goto-char start)
             (funcall flyspell-insert-function new-word)
             (if flyspell-abbrev-p
                 (flyspell-define-abbrev word new-word)))
           ;; In the original Emacs code, this was only called in the body
           ;; of the if.  I arbitrarily kept the XEmacs behavior instead.
           (flyspell-ajust-cursor-point save cursor-location old-max)))
        (t
         (goto-char save)
         nil)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-ajust-cursor-point ...                                  */
;;*---------------------------------------------------------------------*/
(defun flyspell-ajust-cursor-point (save cursor-location old-max)
  (if (>= save cursor-location)
      (let ((new-pos (+ save (- (point-max) old-max))))
	(goto-char (cond
		    ((< new-pos (point-min))
		     (point-min))
		    ((> new-pos (point-max))
		     (point-max))
		    (t new-pos))))
    (goto-char save)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-emacs-popup ...                                         */
;;*---------------------------------------------------------------------*/
(defun flyspell-emacs-popup (event poss word)
  "The Emacs popup menu."
  (unless window-system
    (error "This command requires pop-up dialogs"))
  (if (not event)
      (let* ((mouse-pos  (mouse-position))
	     (mouse-pos  (if (nth 1 mouse-pos)
			     mouse-pos
			   (set-mouse-position (car mouse-pos)
				 	       (/ (frame-width) 2) 2)
			   (mouse-position))))
	(setq event (list (list (car (cdr mouse-pos))
				(1+ (cdr (cdr mouse-pos))))
			  (car mouse-pos)))))
  (let* ((corrects   (if flyspell-sort-corrections
			 (sort (car (cdr (cdr poss))) 'string<)
		       (car (cdr (cdr poss)))))
	 (cor-menu   (if (consp corrects)
			 (mapcar (lambda (correct)
				   (list correct correct))
				 corrects)
		       '()))
	 (affix      (car (cdr (cdr (cdr poss)))))
	 show-affix-info
	 (base-menu  (let ((save (if (and (consp affix) show-affix-info)
				     (list
				      (list (concat "Save affix: " (car affix))
					    'save)
				      '("Accept (session)" session)
				      '("Accept (buffer)" buffer))
				   '(("Save word" save)
				     ("Accept (session)" session)
				     ("Accept (buffer)" buffer)))))
		       (if (consp cor-menu)
			   (append cor-menu (cons "" save))
			 save)))
	 (menu       (cons "flyspell correction menu" base-menu)))
    (car (x-popup-menu event
		       (list (format "%s [%s]" word (or ispell-local-dictionary
							ispell-dictionary))
			     menu)))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-xemacs-popup ...                                        */
;;*---------------------------------------------------------------------*/
(defun flyspell-xemacs-popup (poss word cursor-location start end save)
  "The XEmacs popup menu."
  (let* ((corrects   (if flyspell-sort-corrections
			 (sort (car (cdr (cdr poss))) 'string<)
		       (car (cdr (cdr poss)))))
	 (cor-menu   (if (consp corrects)
			 (mapcar (lambda (correct)
				   (vector correct
					   (list 'flyspell-do-correct
						 correct
						 (list 'quote poss)
						 word
						 cursor-location
						 start
						 end
						 save)
					   t))
				 corrects)
		       '()))
	 (affix      (car (cdr (cdr (cdr poss)))))
	 show-affix-info
	 (menu       (let ((save (if (and (consp affix) show-affix-info)
				     (vector
				      (concat "Save affix: " (car affix))
				      (list 'flyspell-do-correct
					    ''save
					    (list 'quote poss)
					    word
					    cursor-location
					    start
					    end
					    save)
				      t)
				   (vector
				    "Save word"
				    (list 'flyspell-do-correct
					  ''save
					  (list 'quote poss)
					  word
					  cursor-location
					  start
					  end
					  save)
				    t)))
			   (session (vector "Accept (session)"
					    (list 'flyspell-do-correct
						  ''session
						  (list 'quote poss)
						  word
						  cursor-location
						  start
						  end
						  save)
					    t))
			   (buffer  (vector "Accept (buffer)"
					    (list 'flyspell-do-correct
						  ''buffer
						  (list 'quote poss)
						  word
						  cursor-location
						  start
						  end
						  save)
					    t)))
		       (if (consp cor-menu)
			   (append cor-menu (list "-" save session buffer))
			 (list save session buffer)))))
    (popup-menu (cons (format "%s [%s]" word (or ispell-local-dictionary
						 ispell-dictionary))
		      menu))))

;;*---------------------------------------------------------------------*/
;;*    Some example functions for real autocorrecting                   */
;;*---------------------------------------------------------------------*/
(defun flyspell-maybe-correct-transposition (beg end poss)
  "Check replacements for transposed characters.

If the text between BEG and END is equal to a correction suggested by
Ispell, after transposing two adjacent characters, correct the text,
and return t.

The third arg POSS is either the symbol 'doublon' or a list of
possible corrections as returned by `ispell-parse-output'.

This function is meant to be added to `flyspell-incorrect-hook'."
  (when (consp poss)
    (catch 'done
      (let ((str (buffer-substring beg end))
	    (i 0) (len (- end beg)) tmp)
	(while (< (1+ i) len)
	  (setq tmp (aref str i))
	  (aset str i (aref str (1+ i)))
	  (aset str (1+ i) tmp)
          (when (member str (nth 2 poss))
	    (save-excursion
	      (goto-char (+ beg i 1))
	      (transpose-chars 1))
	    (throw 'done t))
	  (setq tmp (aref str i))
	  (aset str i (aref str (1+ i)))
	  (aset str (1+ i) tmp)
	  (setq i (1+ i))))
      nil)))

(defun flyspell-maybe-correct-doubling (beg end poss)
  "Check replacements for doubled characters.

If the text between BEG and END is equal to a correction suggested by
Ispell, after removing a pair of doubled characters, correct the text,
and return t.

The third arg POSS is either the symbol 'doublon' or a list of
possible corrections as returned by `ispell-parse-output'.

This function is meant to be added to `flyspell-incorrect-hook'."
  (when (consp poss)
    (catch 'done
      (let ((str (buffer-substring beg end))
	    (i 0) (len (- end beg)))
	(while (< (1+ i) len)
	  (when (and (= (aref str i) (aref str (1+ i)))
		     (member (concat (substring str 0 (1+ i))
				     (substring str (+ i 2)))
			     (nth 2 poss)))
	    (goto-char (+ beg i))
	    (delete-char 1)
	    (throw 'done t))
	  (setq i (1+ i))))
      nil)))

;;*---------------------------------------------------------------------*/
;;*    flyspell-already-abbrevp ...                                     */
;;*---------------------------------------------------------------------*/
(defun flyspell-already-abbrevp (table word)
  (let ((sym (abbrev-symbol word table)))
    (and sym (symbolp sym))))

;;*---------------------------------------------------------------------*/
;;*    flyspell-change-abbrev ...                                       */
;;*---------------------------------------------------------------------*/
(defun flyspell-change-abbrev (table old new)
  (set (abbrev-symbol old table) new))

(provide 'flyspell)

;;; flyspell.el ends here
