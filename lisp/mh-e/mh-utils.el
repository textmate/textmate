;;; mh-utils.el --- MH-E general utilities

;; Copyright (C) 1993, 1995, 1997, 2000-2012  Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;;; Change Log:

;;; Code:

(require 'mh-e)
(mh-require-cl)

(require 'font-lock)

;;; CL Replacements

;;;###mh-autoload
(defun mh-search-from-end (char string)
  "Return the position of last occurrence of CHAR in STRING.
If CHAR is not present in STRING then return nil. The function is
used in lieu of `search' in the CL package."
  (loop for index from (1- (length string)) downto 0
        when (equal (aref string index) char) return index
        finally return nil))



;;; General Utilities

;;;###mh-autoload
(defun mh-beginning-of-word (&optional n)
  "Return position of the N th word backwards."
  (unless n (setq n 1))
  (let ((syntax-table (syntax-table)))
    (unwind-protect
        (save-excursion
          (mh-mail-abbrev-make-syntax-table)
          (set-syntax-table mail-abbrev-syntax-table)
          (backward-word n)
          (point))
      (set-syntax-table syntax-table))))

;;;###mh-autoload
(defun mh-colors-available-p ()
  "Check if colors are available in the Emacs being used."
  (or (featurep 'xemacs)
      (let ((color-cells (mh-display-color-cells)))
        (and (numberp color-cells) (>= color-cells 8)))))

;;;###mh-autoload
(defun mh-colors-in-use-p ()
  "Check if colors are being used in the folder buffer."
  (and mh-colors-available-flag font-lock-mode))

;;;###mh-autoload
(defun mh-delete-line (lines)
  "Delete the next LINES lines."
  (delete-region (point) (progn (forward-line lines) (point))))

;;;###mh-autoload
(defun mh-make-local-vars (&rest pairs)
  "Initialize local variables according to the variable-value PAIRS."
  (while pairs
    (set (make-local-variable (car pairs)) (car (cdr pairs)))
    (setq pairs (cdr (cdr pairs)))))

;;;###mh-autoload
(defun mh-mapc (function list)
  "Apply FUNCTION to each element of LIST for side effects only."
  (while list
    (funcall function (car list))
    (setq list (cdr list))))

(defvar mh-pick-regexp-chars ".*$["
  "List of special characters in pick regular expressions.")

;;;###mh-autoload
(defun mh-quote-pick-expr (pick-expr)
  "Quote `mh-pick-regexp-chars' in PICK-EXPR.
PICK-EXPR is a list of strings. Return nil if PICK-EXPR is nil."
  (let ((quoted-pick-expr))
    (dolist (string pick-expr)
      (when (and string
                 (not (string-equal string "")))
        (loop for i from 0 to (1- (length mh-pick-regexp-chars)) do
              (let ((s (string ?\\ (aref mh-pick-regexp-chars i))))
                (setq string (mh-replace-regexp-in-string s s string t t))))
        (setq quoted-pick-expr (append quoted-pick-expr (list string)))))
    quoted-pick-expr))

;;;###mh-autoload
(defun mh-replace-string (old new)
  "Replace all occurrences of OLD with NEW in the current buffer.
Ignores case when searching for OLD."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (search-forward old nil t)
      (replace-match new t t))))



;;; Logo Display

(defvar mh-logo-cache nil)

;; Shush compiler.
(defvar image-load-path)

;;;###mh-autoload
(defun mh-logo-display ()
  "Modify mode line to display MH-E logo."
  (mh-do-in-gnu-emacs
    (let* ((load-path (mh-image-load-path-for-library "mh-e" "mh-logo.xpm"))
           (image-load-path (cons (car load-path)
                                  (when (boundp 'image-load-path)
                                    image-load-path))))
      (add-text-properties
       0 2
       `(display ,(or mh-logo-cache
                      (setq mh-logo-cache
                            (mh-funcall-if-exists
                             find-image '((:type xpm :ascent center
                                                 :file "mh-logo.xpm"))))))
       (car mode-line-buffer-identification))))
  (mh-do-in-xemacs
    (setq modeline-buffer-identification
          (list
           (if mh-modeline-glyph
               (cons modeline-buffer-id-left-extent mh-modeline-glyph)
             (cons modeline-buffer-id-left-extent "XEmacs%N:"))
           (cons modeline-buffer-id-right-extent " %17b")))))



;;; Read MH Profile

(defvar mh-find-path-run nil
  "Non-nil if `mh-find-path' has been run already.
Do not access this variable; `mh-find-path' already uses it to
avoid running more than once.")

;;;###mh-autoload
(defun mh-find-path ()
  "Set variables from user's MH profile.

This function sets `mh-user-path' from your \"Path:\" MH profile
component (but defaults to \"Mail\" if one isn't present),
`mh-draft-folder' from \"Draft-Folder:\", `mh-unseen-seq' from
\"Unseen-Sequence:\", `mh-previous-seq' from
\"Previous-Sequence:\", and `mh-inbox' from \"Inbox:\" (defaults
to \"+inbox\").

The hook `mh-find-path-hook' is run after these variables have
been set. This hook can be used the change the value of these
variables if you need to run with different values between MH and
MH-E."
  (unless mh-find-path-run
    ;; Sanity checks.
    (if (and (getenv "MH")
             (not (file-readable-p (getenv "MH"))))
        (error "MH environment variable contains unreadable file %s"
               (getenv "MH")))
    (if (null (mh-variants))
        (error "Install MH and run install-mh before running MH-E"))
    (if (not (or (getenv "MH") (file-readable-p "~/.mh_profile")))
        (error "Run install-mh before running MH-E"))
    ;; Read MH profile.
    (setq mh-user-path (mh-profile-component "Path"))
    (if (not mh-user-path)
        (setq mh-user-path "Mail"))
    (setq mh-user-path
          (file-name-as-directory
           (expand-file-name mh-user-path (expand-file-name "~"))))
    (mh-set-x-image-cache-directory (expand-file-name ".mhe-x-image-cache"
                                                      mh-user-path))
    (setq mh-draft-folder (mh-profile-component "Draft-Folder"))
    (if mh-draft-folder
        (progn
          (if (not (mh-folder-name-p mh-draft-folder))
              (setq mh-draft-folder (format "+%s" mh-draft-folder)))
          (if (not (file-exists-p (mh-expand-file-name mh-draft-folder)))
              (error
               "Draft folder \"%s\" not found; create it and try again"
               (mh-expand-file-name mh-draft-folder)))))
    (setq mh-inbox (mh-profile-component "Inbox"))
    (cond ((not mh-inbox)
           (setq mh-inbox "+inbox"))
          ((not (mh-folder-name-p mh-inbox))
           (setq mh-inbox (format "+%s" mh-inbox))))
    (setq mh-unseen-seq (mh-profile-component "Unseen-Sequence"))
    (if mh-unseen-seq
        (setq mh-unseen-seq (intern mh-unseen-seq))
      (setq mh-unseen-seq 'unseen))     ;old MH default?
    (setq mh-previous-seq (mh-profile-component "Previous-Sequence"))
    (if mh-previous-seq
        (setq mh-previous-seq (intern mh-previous-seq)))
    (run-hooks 'mh-find-path-hook)
    (mh-collect-folder-names)
    (setq mh-find-path-run t)))



;;; Help Functions

;;;###mh-autoload
(defun mh-ephem-message (string)
  "Display STRING in the minibuffer momentarily."
  (message "%s" string)
  (sit-for 5)
  (message ""))

(defvar mh-help-default nil
  "Mode to use if messages are not present for the current mode.")

(defvar mh-help-messages nil
  "Help messages for all modes.
This is an alist of alists. The primary key is a symbol
representing the mode; the value is described in `mh-set-help'.")

;;;###mh-autoload
(defun mh-set-help (messages &optional default)
  "Set help messages.

The MESSAGES are assumed to be an associative array. It is used
to show help for the most common commands in the current mode.
The key is a prefix char. The value is one or more strings which
are concatenated together and displayed in a help buffer if ? is
pressed after the prefix character. The special key nil is used
to display the non-prefixed commands.

The substitutions described in `substitute-command-keys' are performed as
well.

If optional argument DEFAULT is non-nil, then these messages will
be used if help is asked for an unknown mode."
  (add-to-list 'mh-help-messages (cons major-mode messages))
  (if default
      (setq mh-help-default major-mode)))

;;;###mh-autoload
(defun mh-help (&optional help-messages)
  "Display cheat sheet for the MH-E commands.
See `mh-set-help' for setting the help messages.
HELP-MESSAGES are used instead if given.
This is a list of one or more strings which are concatenated together
and displayed in a help buffer."
  (interactive)
  (let* ((help (or help-messages
                  (cdr (assoc nil (assoc major-mode mh-help-messages)))))
         (text (substitute-command-keys (mapconcat 'identity help ""))))
    (with-electric-help
     (function
      (lambda ()
        (insert text)))
     mh-help-buffer)))

;;;###mh-autoload
(defun mh-prefix-help ()
  "Display cheat sheet for the commands of the current prefix in minibuffer."
  (interactive)
  ;; We got here because the user pressed a "?", but he pressed a prefix key
  ;; before that. Since the key vector starts at index 0, the index of the
  ;; last keystroke is length-1 and thus the second to last keystroke is at
  ;; length-2. We use that information to obtain a suitable prefix character
  ;; from the recent keys.
  (let* ((keys (recent-keys))
         (prefix-char (elt keys (- (length keys) 2)))
         (help (cdr (assoc prefix-char (assoc major-mode mh-help-messages)))))
    (mh-help help)))



;;; Message Number Utilities

;;;###mh-autoload
(defun mh-coalesce-msg-list (messages)
  "Given a list of MESSAGES, return a list of message number ranges.
This is the inverse of `mh-read-msg-list', which expands ranges.
Message lists passed to MH programs should be processed by this
function to avoid exceeding system command line argument limits."
  (let ((msgs (sort (copy-sequence messages) 'mh-greaterp))
        (range-high nil)
        (prev -1)
        (ranges nil))
    (while prev
      (if range-high
          (if (or (not (numberp prev))
                  (not (equal (car msgs) (1- prev))))
              (progn                    ;non-sequential, flush old range
                (if (eq prev range-high)
                    (setq ranges (cons range-high ranges))
                  (setq ranges (cons (format "%s-%s" prev range-high) ranges)))
                (setq range-high nil))))
      (or range-high
          (setq range-high (car msgs))) ;start new or first range
      (setq prev (car msgs))
      (setq msgs (cdr msgs)))
    ranges))

(defun mh-greaterp (msg1 msg2)
  "Return the greater of two message indicators MSG1 and MSG2.
Strings are \"smaller\" than numbers.
Valid values are things like \"cur\", \"last\", 1, and 1820."
  (if (numberp msg1)
      (if (numberp msg2)
          (> msg1 msg2)
        t)
    (if (numberp msg2)
        nil
      (string-lessp msg2 msg1))))

;;;###mh-autoload
(defun mh-lessp (msg1 msg2)
  "Return the lesser of two message indicators MSG1 and MSG2.
Strings are \"smaller\" than numbers.
Valid values are things like \"cur\", \"last\", 1, and 1820."
  (not (mh-greaterp msg1 msg2)))

;;;###mh-autoload
(defun mh-get-msg-num (error-if-no-message)
  "Return the message number of the displayed message.
If the argument ERROR-IF-NO-MESSAGE is non-nil, then complain if
the cursor is not pointing to a message."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at (mh-scan-msg-number-regexp))
           (string-to-number (buffer-substring (match-beginning 1)
                                               (match-end 1))))
          (error-if-no-message
           (error "Cursor not pointing to message"))
          (t nil))))

(add-to-list 'debug-ignored-errors "^Cursor not pointing to message$")



;;; Folder Cache and Access

(defvar mh-sub-folders-cache (make-hash-table :test #'equal))
(defvar mh-current-folder-name nil)
(defvar mh-flists-partial-line "")
(defvar mh-flists-process nil)

;;;###mh-autoload
(defun mh-clear-sub-folders-cache ()
  "Clear `mh-sub-folders-cache'."
  (clrhash mh-sub-folders-cache))

;; Initialize mh-sub-folders-cache...
(defun mh-collect-folder-names ()
  "Collect folder names by running \"folders\"."
  (unless mh-flists-process
    (setq mh-flists-process
          (mh-exec-cmd-daemon "folders" 'mh-collect-folder-names-filter
                              "-recurse" "-fast"))))

(defun mh-collect-folder-names-filter (process output)
  "Read folder names.
PROCESS is the flists process that was run to collect folder
names and the function is called when OUTPUT is available."
  (let ((position 0)
        (prevailing-match-data (match-data))
        line-end folder)
    (unwind-protect
        (while (setq line-end (string-match "\n" output position))
          (setq folder (format "+%s%s"
                               mh-flists-partial-line
                               (substring output position line-end)))
          (setq mh-flists-partial-line "")
          (unless (equal (aref folder 1) ?.)
            (mh-populate-sub-folders-cache folder))
          (setq position (1+ line-end)))
      (set-match-data prevailing-match-data))
    (setq mh-flists-partial-line (substring output position))))

(defun mh-populate-sub-folders-cache (folder)
  "Tell `mh-sub-folders-cache' about FOLDER."
  (let* ((last-slash (mh-search-from-end ?/ folder))
         (child1 (substring folder (1+ (or last-slash 0))))
         (parent (and last-slash (substring folder 0 last-slash)))
         (parent-slash (and parent (mh-search-from-end ?/ parent)))
         (child2 (and parent (substring parent (1+ (or parent-slash 0)))))
         (grand-parent (and parent-slash (substring parent 0 parent-slash)))
         (cache-entry (gethash parent mh-sub-folders-cache)))
    (unless (loop for x in cache-entry when (equal (car x) child1) return t
                  finally return nil)
      (push (list child1) cache-entry)
      (setf (gethash parent mh-sub-folders-cache)
            (sort cache-entry (lambda (x y) (string< (car x) (car y)))))
      (when parent
        (loop for x in (gethash grand-parent mh-sub-folders-cache)
              when (equal (car x) child2)
              do (progn (setf (cdr x) t) (return)))))))

(defun mh-normalize-folder-name (folder &optional empty-string-okay
                                        dont-remove-trailing-slash
                                        return-nil-if-folder-empty)
  "Normalizes FOLDER name.

Makes sure that two '/' characters never occur next to each
other. Also all occurrences of \"..\" and \".\" are suitably
processed. So \"+inbox/../news\" will be normalized to \"+news\".

If optional argument EMPTY-STRING-OKAY is nil then a '+' is added
at the front if FOLDER lacks one. If non-nil and FOLDER is the
empty string then nothing is added.

If optional argument DONT-REMOVE-TRAILING-SLASH is non-nil then a
trailing '/' if present is retained (if present), otherwise it is
removed.

If optional argument RETURN-NIL-IF-FOLDER-EMPTY is non-nil, then
return nil if FOLDER is \"\" or \"+\". This is useful when
normalizing the folder for the \"folders\" command which displays
the directories in / if passed \"+\". This is usually not
desired. If this argument is non-nil, then EMPTY-STRING-OKAY has
no effect."
  (cond
   ((if (and (or (equal folder "+") (equal folder ""))
             return-nil-if-folder-empty)
        (setq folder nil)))
   ((stringp folder)
    ;; Replace two or more consecutive '/' characters with a single '/'
    (while (string-match "//" folder)
      (setq folder (replace-match "/" nil t folder)))
    (let* ((length (length folder))
           (trailing-slash-present (and (> length 0)
                                        (equal (aref folder (1- length)) ?/)))
           (leading-slash-present (and (> length 0)
                                       (equal (aref folder 0) ?/))))
      (when (and (> length 0) (equal (aref folder 0) ?@)
                 (stringp mh-current-folder-name))
        (setq folder (format "%s/%s/" mh-current-folder-name
                             (substring folder 1))))
      ;; XXX: Purge empty strings from the list that split-string
      ;; returns. In XEmacs, (split-string "+foo/" "/") returns
      ;; ("+foo" "") while in GNU Emacs it returns ("+foo"). In the
      ;; code it is assumed that the components list has no empty
      ;; strings.
      (let ((components (delete "" (split-string folder "/")))
            (result ()))
        ;; Remove .. and . from the pathname.
        (dolist (component components)
          (cond ((and (equal component "..") result)
                 (pop result))
                ((equal component ".."))
                ((equal component "."))
                (t (push component result))))
        (setq folder "")
        (dolist (component result)
          (setq folder (concat component "/" folder)))
        ;; Remove trailing '/' if needed.
        (unless (and trailing-slash-present dont-remove-trailing-slash)
          (when (not (equal folder ""))
            (setq folder (substring folder 0 (1- (length folder))))))
        (when leading-slash-present
          (setq folder (concat "/" folder)))))
    (cond ((and empty-string-okay (equal folder "")))
          ((equal folder "")
           (setq folder "+"))
          ((not (equal (aref folder 0) ?+))
           (setq folder (concat "+" folder))))))
  folder)

(defmacro mh-children-p (folder)
  "Return t if FOLDER from sub-folders cache has children."
;; The car of folder is the name, and the cdr is either t or some
;; sort of count that I do not understand. It's too small to be the
;; number of messages in the sub-folders and too large to be the
;; number of sub-folders. XXX
  `(if (cdr ,folder)
       t
     nil))

;;;###mh-autoload
(defun mh-folder-list (folder)
  "Return FOLDER and its descendants.
FOLDER may have a + prefix. Returns a list of strings without the
+ prefix. If FOLDER is nil, then all folders are considered. For
example, if your Mail directory only contains the folders +inbox,
+outbox, +lists, and +lists/mh-e, then

  (mh-folder-list nil)
       => (\"inbox\" \"lists\" \"lists/mh-e\" \"outbox\")
  (mh-folder-list \"+lists\")
       => (\"lists\" \"lists/mh-e\")

Respects the value of `mh-recursive-folders-flag'. If this flag
is nil, and the sub-folders have not been explicitly viewed, then
they will not be returned."
  (let ((folder-list))
    ;; Normalize folder. Strip leading + and trailing slash(es). If no
    ;; folder is specified, ensure it is nil to avoid adding the
    ;; folder to the folder-list and adding a slash to it.
    (when folder
      (setq folder (mh-replace-regexp-in-string "^\+" "" folder))
      (setq folder (mh-replace-regexp-in-string "/+$" "" folder))
      (if (equal folder "")
          (setq folder nil)))
    ;; Add provided folder to list, unless all folders are asked for.
    ;; Then append slash to separate sub-folders.
    (unless (null folder)
      (setq folder-list (list folder))
      (setq folder (concat folder "/")))
    (loop for f in (mh-sub-folders folder) do
          (setq folder-list
                (append folder-list
                        (if (mh-children-p f)
                            (mh-folder-list (concat folder (car f)))
                          (list (concat folder (car f)))))))
    folder-list))

;;;###mh-autoload
(defun mh-sub-folders (folder &optional add-trailing-slash-flag)
  "Find the subfolders of FOLDER.
The function avoids running folders unnecessarily by caching the
results of the actual folders call.

If optional argument ADD-TRAILING-SLASH-FLAG is non-nil then a
slash is added to each of the sub-folder names that may have
nested folders within them."
  (let* ((folder (mh-normalize-folder-name folder nil nil t))
         (match (gethash folder mh-sub-folders-cache 'no-result))
         (sub-folders (cond ((eq match 'no-result)
                             (setf (gethash folder mh-sub-folders-cache)
                                   (mh-sub-folders-actual folder)))
                            (t match))))
    (if add-trailing-slash-flag
        (mapcar #'(lambda (x)
                    (if (cdr x) (cons (concat (car x) "/") (cdr x)) x))
                sub-folders)
      sub-folders)))

;; FIXME: This function does not do well if FOLDER does not exist. It
;; then changes the context to that folder which causes problems down
;; the line. Since a folder in the cache could later be deleted, it
;; would be good for mh-sub-folders-actual to return nil in this case
;; so that mh-sub-folders could delete it from the cache. This
;; function could protect itself by using a temporary context.
(defun mh-sub-folders-actual (folder)
  "Execute the command folders to return the sub-folders of FOLDER.
Filters out the folder names that start with \".\" so that
directories that aren't usually mail folders are hidden.
Expects FOLDER to have already been normalized with
  (mh-normalize-folder-name folder nil nil t)"
  (let ((arg-list `(,(expand-file-name "folders" mh-progs)
                    nil (t nil) nil "-noheader" "-norecurse" "-nototal"
                    ,@(if (stringp folder) (list folder) ())))
        (results ())
        (current-folder (concat
                         (with-temp-buffer
                           (call-process (expand-file-name "folder" mh-progs)
                                         nil '(t nil) nil "-fast")
                           (buffer-substring (point-min) (1- (point-max))))
                         "+")))
    (with-temp-buffer
      (apply #'call-process arg-list)
      (goto-char (point-min))
      (while (not (and (eolp) (bolp)))
        (goto-char (mh-line-end-position))
        (let ((start-pos (mh-line-beginning-position))
              (has-pos (search-backward " has "
                                        (mh-line-beginning-position) t)))
          (when (integerp has-pos)
            (while (equal (char-after has-pos) ? )
              (decf has-pos))
            (incf has-pos)
            (while (equal (char-after start-pos) ? )
              (incf start-pos))
            (let* ((name (buffer-substring start-pos has-pos))
                   (first-char (aref name 0))
                   (last-char (aref name (1- (length name)))))
              (unless (member first-char '(?. ?# ?,))
                (when (and (equal last-char ?+) (equal name current-folder))
                  (setq name (substring name 0 (1- (length name)))))
                (push
                 (cons name
                       (search-forward "(others)" (mh-line-end-position) t))
                 results))))
          (forward-line 1))))
    (setq results (nreverse results))
    (when (stringp folder)
      (setq results (cdr results))
      (let ((folder-name-len (length (format "%s/" (substring folder 1)))))
        (setq results (mapcar (lambda (f)
                                (cons (substring (car f) folder-name-len)
                                      (cdr f)))
                              results))))
    results))

;;;###mh-autoload
(defun mh-remove-from-sub-folders-cache (folder)
  "Remove FOLDER and its parent from `mh-sub-folders-cache'.
FOLDER should be unconditionally removed from the cache. Also the
last ancestor of FOLDER present in the cache must be removed as
well.

To see why this is needed assume we have a folder +foo which has
a single sub-folder qux. Now we create the folder +foo/bar/baz.
Here we will need to invalidate the cached sub-folders of +foo,
otherwise completion on +foo won't tell us about the option
+foo/bar!"
  (remhash folder mh-sub-folders-cache)
  (block ancestor-found
    (let ((parent folder)
          (one-ancestor-found nil)
          last-slash)
      (while (setq last-slash (mh-search-from-end ?/ parent))
        (setq parent (substring parent 0 last-slash))
        (unless (eq (gethash parent  mh-sub-folders-cache 'none) 'none)
          (remhash parent mh-sub-folders-cache)
          (if one-ancestor-found
              (return-from ancestor-found)
            (setq one-ancestor-found t))))
      (remhash nil mh-sub-folders-cache))))



;;; Folder Utilities

;;;###mh-autoload
(defun mh-folder-name-p (name)
  "Return non-nil if NAME is the name of a folder.
A name (a string or symbol) can be a folder name if it begins
with \"+\"."
  (if (symbolp name)
      (eq (aref (symbol-name name) 0) ?+)
    (and (> (length name) 0)
         (eq (aref name 0) ?+))))

;;;###mh-autoload
(defun mh-expand-file-name (filename &optional default)
  "Expand FILENAME like `expand-file-name', but also handle MH folder names.
Any filename that starts with '+' is treated as a folder name.
See `expand-file-name' for description of DEFAULT."
  (if (mh-folder-name-p filename)
      (expand-file-name (substring filename 1) mh-user-path)
    (expand-file-name filename default)))

(defvar mh-folder-hist nil)

;; Shush compiler.
(defvar mh-speed-flists-cache)

(defvar mh-allow-root-folder-flag nil
  "Non-nil means \"+\" is an acceptable folder name.
This variable is used to communicate with
`mh-folder-completion-function'. That function can have exactly
three arguments so we bind this variable to t or nil.

This variable should never be set.")

(defvar mh-folder-completion-map (copy-keymap minibuffer-local-completion-map))
(define-key mh-folder-completion-map " " 'minibuffer-complete)  ;Why???

(defvar mh-speed-flists-inhibit-flag nil)

;;;###mh-autoload
(defun mh-speed-flists-active-p ()
  "Check if speedbar is running with message counts enabled."
  (and (featurep 'mh-speed)
       (not mh-speed-flists-inhibit-flag)
       (> (hash-table-count mh-speed-flists-cache) 0)))

;;;###mh-autoload
(defun mh-folder-completion-function (name predicate flag)
  "Programmable completion for folder names.
NAME is the partial folder name that has been input. PREDICATE if
non-nil is a function that is used to filter the possible
choices. FLAG is nil to indicate `try-completion', t for
`all-completions', or the symbol lambda for `test-completion'.
See Info node `(elisp) Programmed Completion' for details."
  (let* ((orig-name name)
         ;; After normalization, name is nil, +, or +something. If a
         ;; trailing slash is present, it is preserved.
         (name (mh-normalize-folder-name name nil t))
         (last-slash (mh-search-from-end ?/ name))
         ;; nil if + or +folder; +folder/ if slash present.
         (last-complete (if last-slash (substring name 0 (1+ last-slash)) nil))
         ;; Either +folder/remainder, +remainder, or "".
         (remainder (cond (last-complete (substring name (1+ last-slash)))
                          (name (substring name 1))
                          (t ""))))
    (cond ((eq (car-safe flag) 'boundaries)
           (list* 'boundaries
                  (let ((slash (mh-search-from-end ?/ orig-name)))
                    (if slash (1+ slash)
                      (if (string-match "\\`\\+" orig-name) 1 0)))
                  (if (cdr flag) (string-match "/" (cdr flag)))))
          ((eq flag nil)
           (let ((try-res
                  (try-completion
                   remainder
                   (mh-sub-folders last-complete t)
                   predicate)))
             (cond ((eq try-res nil) nil)
                   ((and (eq try-res t) (equal name orig-name)) t)
                   ((eq try-res t) name)
                   (t (concat (or last-complete "+") try-res)))))
          ((eq flag t)
           (all-completions
            remainder (mh-sub-folders last-complete t) predicate))
          ((eq flag 'lambda)
           (let ((path (concat (unless (and (> (length name) 1)
                                            (eq (aref name 1) ?/))
                                 mh-user-path)
                               (substring name 1))))
             (cond (mh-allow-root-folder-flag (file-directory-p path))
                   ((equal path mh-user-path) nil)
                   (t (file-directory-p path))))))))

;; Shush compiler.
(mh-do-in-xemacs
  (defvar completion-root-regexp)
  (defvar minibuffer-completing-file-name))

(defun mh-folder-completing-read (prompt default allow-root-folder-flag)
  "Read folder name with PROMPT and default result DEFAULT.
If ALLOW-ROOT-FOLDER-FLAG is non-nil then \"+\" is allowed to be
a folder name corresponding to `mh-user-path'."
  (mh-normalize-folder-name
   (let ((completion-root-regexp "^[+/]")
         (minibuffer-local-completion-map mh-folder-completion-map)
         (mh-allow-root-folder-flag allow-root-folder-flag))
     (completing-read prompt 'mh-folder-completion-function nil nil nil
                      'mh-folder-hist default))
   t))

;;;###mh-autoload
(defun mh-prompt-for-folder (prompt default can-create
                             &optional default-string allow-root-folder-flag)
  "Prompt for a folder name with PROMPT.
Returns the folder's name as a string. DEFAULT is used if the
folder exists and the user types return. If the CAN-CREATE flag
is t, then a folder is created if it doesn't already exist. If
optional argument DEFAULT-STRING is non-nil, use it in the prompt
instead of DEFAULT. If ALLOW-ROOT-FOLDER-FLAG is non-nil then the
function will accept the folder +, which means all folders when
used in searching."
  (if (null default)
      (setq default ""))
  (let* ((default-string (cond (default-string (format " (default %s)" default-string))
                               ((equal "" default) "")
                               (t (format " (default %s)" default))))
         (prompt (format "%s folder%s: " prompt default-string))
         (mh-current-folder-name mh-current-folder)
         read-name folder-name)
    (while (and (setq read-name (mh-folder-completing-read
                                 prompt default allow-root-folder-flag))
                (equal read-name "")
                (equal default "")))
    (cond ((or (equal read-name "")
               (and (equal read-name "+") (not allow-root-folder-flag)))
           (setq read-name default))
          ((not (mh-folder-name-p read-name))
           (setq read-name (format "+%s" read-name))))
    (if (or (not read-name) (equal "" read-name))
        (error "No folder specified"))
    (setq folder-name read-name)
    (cond ((and (> (length folder-name) 0)
                (eq (aref folder-name (1- (length folder-name))) ?/))
           (setq folder-name (substring folder-name 0 -1))))
    (let* ((last-slash (mh-search-from-end ?/ folder-name))
           (parent (and last-slash (substring folder-name 0 last-slash)))
           (child (if last-slash
                      (substring folder-name (1+ last-slash))
                    (substring folder-name 1))))
      (unless (member child
                      (mapcar #'car (gethash parent mh-sub-folders-cache)))
        (mh-remove-from-sub-folders-cache folder-name)))
    (let ((new-file-flag
           (not (file-exists-p (mh-expand-file-name folder-name)))))
      (cond ((and new-file-flag
                  can-create
                  (y-or-n-p
                   (format "Folder %s does not exist.  Create it? "
                           folder-name)))
             (message "Creating %s" folder-name)
             (mh-exec-cmd-error nil "folder" folder-name)
             (mh-remove-from-sub-folders-cache folder-name)
             (when (boundp 'mh-speed-folder-map)
               (mh-speed-add-folder folder-name))
             (message "Creating %s...done" folder-name))
            (new-file-flag
             (error "Folder %s does not exist" folder-name))
            ((not (file-directory-p (mh-expand-file-name folder-name)))
             (error "%s is not a directory"
                    (mh-expand-file-name folder-name)))))
    folder-name))



;;; Message Utilities

;; Functions that would ordinarily be in mh-letter.el that are needed
;; by mh-show.el are found here in order to prevent the loading of
;; mh-letter.el until a message is actually composed.

;;;###mh-autoload
(defun mh-in-header-p ()
  "Return non-nil if the point is in the header of a draft message."
  (< (point) (mh-mail-header-end)))

;;;###mh-autoload
(defun mh-extract-from-header-value ()
  "Extract From: string from header."
  (save-excursion
    (if (not (mh-goto-header-field "From:"))
        nil
      (skip-chars-forward " \t")
      (buffer-substring-no-properties
       (point) (progn (mh-header-field-end)(point))))))

;;;###mh-autoload
(defun mh-get-header-field (field)
  "Find and return the body of FIELD in the mail header.
Returns the empty string if the field is not in the header of the
current buffer."
  (if (mh-goto-header-field field)
      (progn
        (skip-chars-forward " \t")      ;strip leading white space in body
        (let ((start (point)))
          (mh-header-field-end)
          (buffer-substring-no-properties start (point))))
    ""))

;;;###mh-autoload
(defun mh-goto-header-field (field)
  "Move to FIELD in the message header.
Move to the end of the FIELD name, which should end in a colon.
Returns t if found, nil if not."
  (goto-char (point-min))
  (let ((case-fold-search t)
        (headers-end (save-excursion
                       (mh-goto-header-end 0)
                       (point))))
    (re-search-forward (format "^%s" field) headers-end t)))

;;;###mh-autoload
(defun mh-goto-header-end (arg)
  "Move the cursor ARG lines after the header."
  (if (re-search-forward (concat "^\\(" (regexp-quote mh-mail-header-separator)
                                 "\\)?$") nil nil)
      (forward-line arg)))

;;;###mh-autoload
(defun mh-mail-header-end ()
  "Substitute for `mail-header-end' that doesn't widen the buffer.

In MH-E we frequently need to find the end of headers in nested
messages, where the buffer has been narrowed. This function works
in this situation."
  (save-excursion
    ;; XXX: The following replaces a call to rfc822-goto-eoh. Occasionally,
    ;; mail headers that MH-E has to read contains lines of the form:
    ;;    From xxx@yyy Mon May 10 11:48:07 2004
    ;; In this situation, rfc822-goto-eoh doesn't go to the end of the
    ;; header. The replacement allows From_ lines in the mail header.
    (goto-char (point-min))
    (loop for p = (re-search-forward
                   "^\\([:\n]\\|[^: \t\n]+[ \t\n]\\)" nil 'move)
          do (cond ((null p) (return))
                   (t (goto-char (match-beginning 0))
                      (unless (looking-at "From ") (return))
                      (goto-char p))))
    (point)))

;;;###mh-autoload
(defun mh-header-field-beginning ()
  "Move to the beginning of the current header field.
Handles RFC 822 continuation lines."
  (beginning-of-line)
  (while (looking-at "^[ \t]")
    (forward-line -1)))

;;;###mh-autoload
(defun mh-header-field-end ()
  "Move to the end of the current header field.
Handles RFC 822 continuation lines."
  (forward-line 1)
  (while (looking-at "^[ \t]")
    (forward-line 1))
  (backward-char 1))                    ;to end of previous line

;;;###mh-autoload
(defun mh-letter-hide-all-skipped-fields ()
  "Hide all skipped fields."
  (save-excursion
    (goto-char (point-min))
    (save-restriction
      (narrow-to-region (point) (mh-mail-header-end))
      (while (re-search-forward mh-letter-header-field-regexp nil t)
        (if (mh-letter-skipped-header-field-p (match-string 1))
            (mh-letter-toggle-header-field-display -1)
          (mh-letter-toggle-header-field-display 'long))
        (beginning-of-line 2)))))

;;;###mh-autoload
(defun mh-letter-skipped-header-field-p (field)
  "Check if FIELD is to be skipped."
  (let ((field (downcase field)))
    (loop for x in mh-compose-skipped-header-fields
          when (equal (downcase x) field) return t
          finally return nil)))

(defvar mh-hidden-header-keymap
  (let ((map (make-sparse-keymap)))
    (mh-do-in-gnu-emacs
      (define-key map [mouse-2] 'mh-letter-toggle-header-field-display-button))
    (mh-do-in-xemacs
      (define-key map '(button2)
        'mh-letter-toggle-header-field-display-button))
    map))

;;;###mh-autoload
(defun mh-letter-toggle-header-field-display (arg)
  "Toggle display of header field at point.

Use this command to display truncated header fields. This command
is a toggle so entering it again will hide the field. This
command takes a prefix argument ARG: if negative then the field
is hidden, if positive then the field is displayed."
  (interactive (list nil))
  (when (and (mh-in-header-p)
             (progn
               (end-of-line)
               (re-search-backward mh-letter-header-field-regexp nil t)))
    (let ((buffer-read-only nil)
          (modified-flag (buffer-modified-p))
          (begin (point))
          end)
      (end-of-line)
      (setq end (1- (if (re-search-forward "^[^ \t]" nil t)
                        (match-beginning 0)
                      (point-max))))
      (goto-char begin)
      ;; Make it clickable...
      (add-text-properties begin end `(keymap ,mh-hidden-header-keymap
                                       mouse-face highlight))
      (unwind-protect
          (cond ((or (and (not arg)
                          (text-property-any begin end 'invisible 'vanish))
                     (and (numberp arg)
                          (>= arg 0))
                     (and (eq arg 'long)
                          (> (mh-line-beginning-position 5) end)))
                 (remove-text-properties begin end '(invisible nil))
                 (search-forward ":" (mh-line-end-position) t)
                 (mh-letter-skip-leading-whitespace-in-header-field))
                ;; XXX Redesign to make usable by user. Perhaps use a positive
                ;; numeric prefix to make that many lines visible.
                ((eq arg 'long)
                 (end-of-line 4)
                 (mh-letter-truncate-header-field end)
                 (beginning-of-line))
                (t (end-of-line)
                   (mh-letter-truncate-header-field end)
                   (beginning-of-line)))
        (set-buffer-modified-p modified-flag)))))

;;;###mh-autoload
(defun mh-letter-skip-leading-whitespace-in-header-field ()
  "Skip leading whitespace in a header field.
If the header field doesn't have at least one space after the
colon then a space character is added."
  (let ((need-space t))
    (while (memq (char-after) '(?\t ?\ ))
      (forward-char)
      (setq need-space nil))
    (when need-space (insert " "))))

(defun mh-letter-truncate-header-field (end)
  "Replace text from current line till END with an ellipsis.
If the current line is too long truncate a part of it as well."
  (let ((max-len (min (window-width) 62)))
    (when (> (+ (current-column) 4) max-len)
      (backward-char (- (+ (current-column) 5) max-len)))
    (when (> end (point))
      (add-text-properties (point) end '(invisible vanish)))))

;;;###mh-autoload
(defun mh-signature-separator-p ()
  "Return non-nil if buffer includes \"^-- $\"."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward mh-signature-separator-regexp nil t)))

(provide 'mh-utils)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-utils.el ends here
