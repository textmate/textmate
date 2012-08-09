;;; org-ctags.el - Integrate Emacs "tags" facility with org mode.
;;
;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Paul Sexton <eeeickythump@gmail.com>


;; Keywords: org, wp
;;
;; This file is part of GNU Emacs.
;;
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

;;
;; Synopsis
;; ========
;;
;; Allows org-mode to make use of the Emacs `etags' system. Defines tag
;; destinations in org-mode files as any text between <<double angled
;; brackets>>. This allows the tags-generation program `exuberant ctags' to
;; parse these files and create tag tables that record where these
;; destinations are found. Plain [[links]] in org mode files which do not have
;; <<matching destinations>> within the same file will then be interpreted as
;; links to these 'tagged' destinations, allowing seamless navigation between
;; multiple org-mode files. Topics can be created in any org mode file and
;; will always be found by plain links from other files. Other file types
;; recognized by ctags (source code files, latex files, etc) will also be
;; available as destinations for plain links, and similarly, org-mode links
;; will be available as tags from source files. Finally, the function
;; `org-ctags-find-tag-interactive' lets you choose any known tag, using
;; autocompletion, and quickly jump to it.
;;
;; Installation
;; ============
;;
;; Install org mode
;; Ensure org-ctags.el is somewhere in your emacs load path.
;; Download and install Exuberant ctags -- "http://ctags.sourceforge.net/"
;; Edit your .emacs file (see next section) and load emacs.

;; To put in your init file (.emacs):
;; ==================================
;;
;; Assuming you already have org mode installed and set up:
;;
;;    (setq org-ctags-path-to-ctags "/path/to/ctags/executable")
;;    (add-hook 'org-mode-hook
;;      (lambda ()
;;        (define-key org-mode-map "\C-co" 'org-ctags-find-tag-interactive)))
;;
;; By default, with org-ctags loaded, org will first try and visit the tag
;; with the same name as the link; then, if unsuccessful, ask the user if
;; he/she wants to rebuild the 'TAGS' database and try again; then ask if
;; the user wishes to append 'tag' as a new toplevel heading at the end of
;; the buffer; and finally, defer to org's default behaviour which is to
;; search the entire text of the current buffer for 'tag'.
;;
;; This behaviour can be modified by changing the value of
;; ORG-CTAGS-OPEN-LINK-FUNCTIONS. For example I have the following in my
;; .emacs, which describes the same behaviour as the above paragraph with
;; one difference:
;;
;; (setq org-ctags-open-link-functions
;;       '(org-ctags-find-tag
;;         org-ctags-ask-rebuild-tags-file-then-find-tag
;;         org-ctags-ask-append-topic
;;         org-ctags-fail-silently))  ; <-- prevents org default behaviour
;;
;;
;; Usage
;; =====
;;
;; When you click on a link "[[foo]]" and org cannot find a matching "<<foo>>"
;; in the current buffer, the tags facility will take over. The file TAGS in
;; the active directory is examined to see if the tags facility knows about
;; "<<foo>>" in any other files. If it does, the matching file will be opened
;; and the cursor will jump to the position of "<<foo>>" in that file.
;;
;; User-visible functions:
;; - `org-ctags-find-tag-interactive': type a tag (plain link) name and visit
;;   it. With autocompletion. Bound to ctrl-O in the above setup.
;; - All the etags functions should work. These include:
;;
;;      M-.    `find-tag' -- finds the tag at point
;;
;;      C-M-.  find-tag based on regular expression
;;
;;      M-x tags-search RET -- like C-M-. but searches through ENTIRE TEXT
;;             of ALL the files referenced in the TAGS file. A quick way to
;;             search through an entire 'project'.
;;
;;      M-*    "go back" from a tag jump. Like `org-mark-ring-goto'.
;;             You may need to bind this key yourself with (eg)
;;             (global-set-key (kbd "<M-kp-multiply>") 'pop-tag-mark)
;;
;;      (see etags chapter in Emacs manual for more)
;;
;;
;; Keeping the TAGS file up to date
;; ================================
;;
;; Tags mode has no way of knowing that you have created new tags by typing in
;; your org-mode buffer.  New tags make it into the TAGS file in 3 ways:
;;
;; 1. You re-run (org-ctags-create-tags "directory") to rebuild the file.
;; 2. You put the function `org-ctags-ask-rebuild-tags-file-then-find-tag' in
;;    your `org-open-link-functions' list, as is done in the setup
;;    above. This will cause the TAGS file to be rebuilt whenever a link
;;    cannot be found. This may be slow with large file collections however.
;; 3. You run the following from the command line (all 1 line):
;;
;;      ctags --langdef=orgmode --langmap=orgmode:.org
;;        --regex-orgmode="/<<([^>]+)>>/\1/d,definition/"
;;          -f /your/path/TAGS -e -R /your/path/*.org
;;
;; If you are paranoid, you might want to run (org-ctags-create-tags
;; "/path/to/org/files") at startup, by including the following toplevel form
;; in .emacs. However this can cause a pause of several seconds if ctags has
;; to scan lots of files.
;;
;;     (progn
;;       (message "-- rebuilding tags tables...")
;;       (mapc 'org-create-tags tags-table-list))

;;; Code:

(eval-when-compile (require 'cl))

(require 'org)

(declare-function org-pop-to-buffer-same-window "org-compat" (&optional buffer-or-name norecord label))

(defgroup org-ctags nil
  "Options concerning use of ctags within org mode."
  :tag "Org-Ctags"
  :group 'org-link)

(defvar org-ctags-enabled-p t
  "Activate ctags support in org mode?")

(defvar org-ctags-tag-regexp "/<<([^>]+)>>/\\1/d,definition/"
  "Regexp expression used by ctags external program.
The regexp matches tag destinations in org-mode files.
Format is: /REGEXP/TAGNAME/FLAGS,TAGTYPE/
See the ctags documentation for more information.")

(defcustom org-ctags-path-to-ctags
  (case system-type
    (windows-nt "ctags.exe")
    (darwin "ctags-exuberant")
    (t "ctags-exuberant"))
  "Full path to the ctags executable file."
  :group 'org-ctags
  :version "24.1"
  :type 'file)

(defcustom org-ctags-open-link-functions
  '(org-ctags-find-tag
    org-ctags-ask-rebuild-tags-file-then-find-tag
    org-ctags-ask-append-topic)
  "List of functions to be prepended to ORG-OPEN-LINK-FUNCTIONS when ORG-CTAGS is active."
  :group 'org-ctags
  :version "24.1"
  :type 'hook
  :options '(org-ctags-find-tag
             org-ctags-ask-rebuild-tags-file-then-find-tag
             org-ctags-rebuild-tags-file-then-find-tag
             org-ctags-ask-append-topic
             org-ctags-append-topic
             org-ctags-ask-visit-buffer-or-file
             org-ctags-visit-buffer-or-file
             org-ctags-fail-silently))


(defvar org-ctags-tag-list nil
  "List of all tags in the active TAGS file.
Created as a local variable in each buffer.")

(defcustom org-ctags-new-topic-template
  "* <<%t>>\n\n\n\n\n\n"
  "Text to insert when creating a new org file via opening a hyperlink.
The following patterns are replaced in the string:
    `%t' - replaced with the capitalized title of the hyperlink"
  :group 'org-ctags
  :type 'string)


(add-hook 'org-mode-hook
          (lambda ()
            (when (and org-ctags-enabled-p
                       (buffer-file-name))
              ;; Make sure this file's directory is added to default
              ;; directories in which to search for tags.
              (let ((tags-filename
                     (expand-file-name
                      (concat (file-name-directory (buffer-file-name))
                              "/TAGS"))))
                (when (file-exists-p tags-filename)
                  (visit-tags-table tags-filename))))))


(defadvice visit-tags-table (after org-ctags-load-tag-list activate compile)
  (when (and org-ctags-enabled-p tags-file-name)
    (set (make-local-variable 'org-ctags-tag-list)
         (org-ctags-all-tags-in-current-tags-table))))


(defun org-ctags-enable ()
  (put 'org-mode 'find-tag-default-function 'org-ctags-find-tag-at-point)
  (setq org-ctags-enabled-p t)
  (dolist (fn org-ctags-open-link-functions)
    (add-hook 'org-open-link-functions fn t)))


;;; General utility functions.  ===============================================
;; These work outside org-ctags mode.

(defun org-ctags-get-filename-for-tag (tag)
  "TAG is a string.  Search the active TAGS file for a matching tag.
If the tag is found, return a list containing the filename, line number, and
buffer position where the tag is found."
  (interactive "sTag: ")
  (unless tags-file-name
    (call-interactively (visit-tags-table)))
  (save-excursion
    (visit-tags-table-buffer 'same)
    (when tags-file-name
      (with-current-buffer (get-file-buffer tags-file-name)
        (goto-char (point-min))
        (cond
         ((re-search-forward (format "^.*%s\\([0-9]+\\),\\([0-9]+\\)$"
                                     (regexp-quote tag)) nil t)
          (let ((line (string-to-number (match-string 1)))
                (pos (string-to-number (match-string 2))))
            (cond
             ((re-search-backward "\n\\(.*\\),[0-9]+\n")
              (list (match-string 1) line pos))
             (t              ; can't find a file name preceding the matched
                             ; tag??
              (error "Malformed TAGS file: %s" (buffer-name))))))
         (t                               ; tag not found
          nil))))))


(defun org-ctags-all-tags-in-current-tags-table ()
  "Read all tags defined in the active TAGS file, into a list of strings.
Return the list."
  (interactive)
  (let ((taglist nil))
    (unless tags-file-name
      (call-interactively (visit-tags-table)))
    (save-excursion
      (visit-tags-table-buffer 'same)
      (with-current-buffer (get-file-buffer tags-file-name)
        (goto-char (point-min))
        (while (re-search-forward "^.*\\(.*\\)\\([0-9]+\\),\\([0-9]+\\)$"
                                  nil t)
          (push (substring-no-properties (match-string 1)) taglist)))
      taglist)))


(defun org-ctags-string-search-and-replace (search replace string)
  "Replace all instances of SEARCH with REPLACE in STRING."
  (replace-regexp-in-string (regexp-quote search) replace string t t))


(defun y-or-n-minibuffer (prompt)
  (let ((use-dialog-box nil))
    (y-or-n-p prompt)))


;;; Internal functions =======================================================


(defun org-ctags-open-file (name &optional title)
  "Visit or create a file called `NAME.org', and insert a new topic.
The new topic will be titled NAME (or TITLE if supplied)."
  (interactive "sFile name: ")
  (let ((filename (substitute-in-file-name (expand-file-name name))))
    (condition-case v
        (progn
          (org-open-file name t)
          (message "Opened file OK")
          (goto-char (point-max))
          (insert (org-ctags-string-search-and-replace
                   "%t" (capitalize (or title name))
                   org-ctags-new-topic-template))
          (message "Inserted new file text OK")
          (org-mode-restart))
      (error (error "Error %S in org-ctags-open-file" v)))))


;;;; Misc interoperability with etags system =================================


(defadvice find-tag (before org-ctags-set-org-mark-before-finding-tag
			    activate compile)
  "Before trying to find a tag, save our current position on org mark ring."
  (save-excursion
    (if (and (eq major-mode 'org-mode) org-ctags-enabled-p)
        (org-mark-ring-push))))



(defun org-ctags-find-tag-at-point ()
  "Determine default tag to search for, based on text at point.
If there is no plausible default, return nil."
  (let (from to bound)
    (when (or (ignore-errors
		;; Look for hyperlink around `point'.
		(save-excursion
		  (search-backward "[[") (setq from (+ 2 (point))))
		(save-excursion
                  (goto-char from)
		  (search-forward "]") (setq to (- (point) 1)))
		(and (> to from) (>= (point) from) (<= (point) to)))
              (progn
		;; Look at text around `point'.
		(save-excursion
		  (skip-syntax-backward "w_") (setq from (point)))
		(save-excursion
		  (skip-syntax-forward "w_") (setq to (point)))
		(> to from))
	      ;; Look between `line-beginning-position' and `point'.
	      (save-excursion
		(and (setq bound (line-beginning-position))
		     (skip-syntax-backward "^w_" bound)
		     (> (setq to (point)) bound)
		     (skip-syntax-backward "w_")
		     (setq from (point))))
	      ;; Look between `point' and `line-end-position'.
	      (save-excursion
		(and (setq bound (line-end-position))
		     (skip-syntax-forward "^w_" bound)
		     (< (setq from (point)) bound)
		     (skip-syntax-forward "w_")
		     (setq to (point)))))
      (buffer-substring-no-properties from to))))


;;; Functions for use with 'org-open-link-functions' hook =================


(defun org-ctags-find-tag (name)
  "This function is intended to be used in ORG-OPEN-LINK-FUNCTIONS.
Look for a tag called `NAME' in the current TAGS table.  If it is found,
visit the file and location where the tag is found."
  (interactive "sTag: ")
  (let ((old-buf (current-buffer))
        (old-pnt (point-marker))
        (old-mark (copy-marker (mark-marker))))
    (condition-case nil
        (progn (find-tag name)
               t)
      (error
       ;; only restore old location if find-tag raises error
       (set-buffer old-buf)
       (goto-char old-pnt)
       (set-marker (mark-marker) old-mark)
       nil))))


(defun org-ctags-visit-buffer-or-file (name &optional create)
  "This function is intended to be used in ORG-OPEN-LINK-FUNCTIONS.
Visit buffer named `NAME.org'.  If there is no such buffer, visit the file
with the same name if it exists.  If the file does not exist, then behavior
depends on the value of CREATE.

If CREATE is nil (default), then return nil.  Do not create a new file.
If CREATE is t, create the new file and visit it.
If CREATE is the symbol `ask', then ask the user if they wish to create
the new file."
  (interactive)
  (let ((filename (concat (substitute-in-file-name
                           (expand-file-name name))
                          ".org")))
    (cond
     ((get-buffer (concat name ".org"))
      ;; Buffer is already open
      (org-pop-to-buffer-same-window (get-buffer (concat name ".org"))))
     ((file-exists-p filename)
      ;; File exists but is not open --> open it
      (message "Opening existing org file `%S'..."
               filename)
      (org-open-file filename t))
     ((or (eql create t)
          (and (eql create 'ask)
               (y-or-n-p (format "File `%s.org' not found; create?" name))))
      (org-ctags-open-file filename name))
     (t ;; File does not exist, and we don't want to create it.
      nil))))


(defun org-ctags-ask-visit-buffer-or-file (name)
  "This function is intended to be used in ORG-OPEN-LINK-FUNCTIONS.
Wrapper for org-ctags-visit-buffer-or-file, which ensures the user is
asked before creating a new file."
  (org-ctags-visit-buffer-or-file name 'ask))


(defun org-ctags-append-topic (name &optional narrowp)
  "This function is intended to be used in ORG-OPEN-LINK-FUNCTIONS.
Append a new toplevel heading to the end of the current buffer. The
heading contains NAME surrounded by <<angular brackets>>, thus making
the heading a destination for the tag `NAME'."
  (interactive "sTopic: ")
  (widen)
  (goto-char (point-max))
  (newline 2)
  (message "Adding topic in buffer %s" (buffer-name))
  (insert (org-ctags-string-search-and-replace
           "%t" (capitalize name) org-ctags-new-topic-template))
  (backward-char 4)
  (org-update-radio-target-regexp)
  (end-of-line)
  (forward-line 2)
  (when narrowp
    ;;(org-tree-to-indirect-buffer 1)  ;; opens new frame
    (org-narrow-to-subtree))
  t)


(defun org-ctags-ask-append-topic (name &optional narrowp)
  "This function is intended to be used in ORG-OPEN-LINK-FUNCTIONS.
Wrapper for org-ctags-append-topic, which first asks the user if they want
to append a new topic."
  (if (y-or-n-p (format "Topic `%s' not found; append to end of buffer?"
                        name))
      (org-ctags-append-topic name narrowp)
    nil))


(defun org-ctags-rebuild-tags-file-then-find-tag (name)
  "This function is intended to be used in ORG-OPEN-LINK-FUNCTIONS.
Like ORG-CTAGS-FIND-TAG, but calls the external ctags program first,
to rebuild (update) the TAGS file."
  (unless tags-file-name
    (call-interactively (visit-tags-table)))
  (when (buffer-file-name)
    (org-ctags-create-tags))
  (org-ctags-find-tag name))


(defun org-ctags-ask-rebuild-tags-file-then-find-tag (name)
  "This function is intended to be used in ORG-OPEN-LINK-FUNCTIONS.
Wrapper for org-ctags-rebuild-tags-file-then-find-tag."
  (if (and (buffer-file-name)
             (y-or-n-p
              (format
               "Tag `%s' not found.  Rebuild table `%s/TAGS' and look again?"
               name
               (file-name-directory (buffer-file-name)))))
    (org-ctags-rebuild-tags-file-then-find-tag name)
    nil))


(defun org-ctags-fail-silently (name)
  "This function is intended to be used in ORG-OPEN-LINK-FUNCTIONS.
Put as the last function in the list if you want to prevent org's default
behavior of free text search."
  t)


;;; User-visible functions ===================================================


(defun org-ctags-create-tags (&optional directory-name)
  "(Re)create tags file in the directory of the active buffer.
The file will contain tag definitions for all the files in the
directory and its subdirectories which are recognized by ctags.
This will include files ending in `.org' as well as most other
source files (.C, .H, .EL, .LISP, etc).  All the resulting tags
end up in one file, called TAGS, located in the directory.  This
function may take several seconds to finish if the directory or
its subdirectories contain large numbers of taggable files."
  (interactive)
  (assert (buffer-file-name))
  (let ((dir-name (or directory-name
                      (file-name-directory (buffer-file-name))))
        (exitcode nil))
    (save-excursion
      (setq exitcode
            (shell-command
             (format (concat "%s --langdef=orgmode --langmap=orgmode:.org "
                             "--regex-orgmode=\"%s\" -f \"%s\" -e -R \"%s\"")
                     org-ctags-path-to-ctags
                     org-ctags-tag-regexp
                     (expand-file-name (concat dir-name "/TAGS"))
                     (expand-file-name (concat dir-name "/*")))))
      (cond
       ((eql 0 exitcode)
        (set (make-local-variable 'org-ctags-tag-list)
             (org-ctags-all-tags-in-current-tags-table)))
       (t
        ;; This seems to behave differently on Linux, so just ignore
        ;; error codes for now
        ;;(error "Calling ctags executable resulted in error code: %s"
        ;;       exitcode)
        nil)))))


(defvar org-ctags-find-tag-history nil
  "History of tags visited by org-ctags-find-tag-interactive.")

(defun org-ctags-find-tag-interactive ()
  "Prompt for the name of a tag, with autocompletion, then visit the named tag.
Uses `ido-mode' if available.
If the user enters a string that does not match an existing tag, create
a new topic."
  (interactive)
  (let* ((completing-read-fn (if (fboundp 'ido-completing-read)
                                 'ido-completing-read
                               'completing-read))
         (tag (funcall completing-read-fn "Topic: " org-ctags-tag-list
                       nil 'confirm nil 'org-ctags-find-tag-history)))
    (when tag
      (cond
       ((member tag org-ctags-tag-list)
        ;; Existing tag
        (push tag org-ctags-find-tag-history)
        (find-tag tag))
       (t
        ;; New tag
        (run-hook-with-args-until-success
		'org-open-link-functions tag))))))


(org-ctags-enable)

(provide 'org-ctags)

;;; org-ctags.el ends here
