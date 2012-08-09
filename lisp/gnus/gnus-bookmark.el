;;; gnus-bookmark.el --- Bookmarks in Gnus

;; Copyright (C) 2006-2012  Free Software Foundation, Inc.

;; Author: Bastien Guerry <bzg AT altern DOT org>
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

;; This file implements real bookmarks for Gnus, closely following the way
;; `bookmark.el' handles bookmarks.  Most of the code comes from
;; `bookmark.el'.
;;
;; Set a Gnus bookmark:
;; M-x `gnus-bookmark-set' from the summary buffer.
;;
;; Jump to a Gnus bookmark:
;; M-x `gnus-bookmark-jump'.
;;
;; Display a list of bookmarks
;; M-x `gnus-bookmark-bmenu-list'.
;;

;;; Todo:

;; - add tags to bookmarks
;; - don't write file each time a bookmark is created
;; - better annotation interactive buffer
;; - edit annotation in gnus-bookmark-bmenu
;; - sort gnus-bookmark-buffer by author/subject/date/group/message-id
;; - auto-bmk-name customizable format
;; - renaming bookmarks in gnus-bookmark-bmenu-list
;; - better (formatted string) display in bmenu-list

;; - Integrate the `gnus-summary-*-bookmark' functionality
;; - Initialize defcustoms from corresponding `bookmark.el' variables?

;;; Code:

(require 'gnus-sum)

;; FIXME: should avoid using C-c (no?)
;; (define-key gnus-summary-mode-map "\C-crm" 'gnus-bookmark-set)
;; (define-key global-map "\C-crb" 'gnus-bookmark-jump)
;; (define-key global-map "\C-crj" 'gnus-bookmark-jump)
;; (define-key global-map "\C-crl" 'gnus-bookmark-bmenu-list)

;; FIXME: Add keybindings, see
;; http://thread.gmane.org/gmane.emacs.gnus.general/63101/focus=63379
;; http://thread.gmane.org/v9fxx9fkm4.fsf@marauder.physik.uni-ulm.de

;; FIXME: Check if `gnus-bookmark.el' should use
;; `bookmark-make-cell-function'.
;; Cf. http://article.gmane.org/gmane.emacs.gnus.general/66076

(defgroup gnus-bookmark nil
  "Setting, annotation and jumping to Gnus bookmarks."
  :group 'gnus)

(defcustom gnus-bookmark-default-file
  (cond
   ;; Backward compatibility with previous versions:
   ((file-exists-p "~/.gnus.bmk") "~/.gnus.bmk")
   (t (nnheader-concat gnus-directory "bookmarks.el")))
  "The default Gnus bookmarks file."
  :type 'string
  :group 'gnus-bookmark)

(defcustom gnus-bookmark-file-coding-system
  (if (mm-coding-system-p 'iso-2022-7bit)
      'iso-2022-7bit)
  "Coding system used for writing Gnus bookmark files."
  :type '(symbol :tag "Coding system")
  :group 'gnus-bookmark)

(defcustom gnus-bookmark-sort-flag t
  "Non-nil means Gnus bookmarks are sorted by bookmark names.
Otherwise they will be displayed in LIFO order (that is,
most recently set ones come first, oldest ones come last)."
  :type 'boolean
  :group 'gnus-bookmark)

(defcustom gnus-bookmark-bmenu-toggle-infos t
  "Non-nil means show details when listing Gnus bookmarks.
List of details is defined in `gnus-bookmark-bookmark-inline-details'.
This may result in truncated bookmark names.  To disable this, put the
following in your `.emacs' file:

\(setq gnus-bookmark-bmenu-toggle-infos nil\)"
  :type 'boolean
  :group 'gnus-bookmark)

(defcustom gnus-bookmark-bmenu-file-column 30
  "Column at which to display details in a buffer listing Gnus bookmarks.
You can toggle whether details are shown with \\<gnus-bookmark-bmenu-mode-map>\\[gnus-bookmark-bmenu-toggle-infos]."
  :type 'integer
  :group 'gnus-bookmark)

(defcustom gnus-bookmark-use-annotations nil
  "If non-nil, ask for an annotation when setting a bookmark."
  :type 'boolean
  :group 'gnus-bookmark)

(defcustom gnus-bookmark-bookmark-inline-details '(author)
  "Details to be shown with `gnus-bookmark-bmenu-toggle-infos'.
The default value is \(subject\)."
  :type '(list :tag "Gnus bookmark details"
	       (set :inline t
		    (const :tag "Author" author)
		    (const :tag "Subject" subject)
		    (const :tag "Date" date)
		    (const :tag "Group" group)
		    (const :tag "Message-id" message-id)))
  :group 'gnus-bookmark)

(defcustom gnus-bookmark-bookmark-details
  '(author subject date group annotation)
  "Details to be shown with `gnus-bookmark-bmenu-show-details'.
The default value is \(author subject date group annotation\)."
  :type '(list :tag "Gnus bookmark details"
	       (set :inline t
		    (const :tag "Author" author)
		    (const :tag "Subject" subject)
		    (const :tag "Date" date)
		    (const :tag "Group" group)
		    (const :tag "Message-id" message-id)
		    (const :tag "Annotation" annotation)))
  :group 'gnus-bookmark)

(defface gnus-bookmark-menu-heading
  '((t (:inherit font-lock-type-face)))
  "Face used to highlight the heading in Gnus bookmark menu buffers."
  :version "23.1" ;; No Gnus
  :group 'gnus-bookmark)

(defconst gnus-bookmark-end-of-version-stamp-marker
  "-*- End Of Bookmark File Format Version Stamp -*-\n"
  "This string marks the end of the version stamp in a Gnus bookmark file.")

(defconst gnus-bookmark-file-format-version 0
  "The current version of the format used by bookmark files.
You should never need to change this.")

(defvar gnus-bookmark-alist ()
  "Association list of Gnus bookmarks and their records.
The format of the alist is

     \(BMK1 BMK2 ...\)

where each BMK is of the form

\(NAME
  \(group . GROUP\)
  \(message-id . MESSAGE-ID\)
  \(author . AUTHOR\)
  \(date . DATE\)
  \(subject . SUBJECT\)
  \(annotation . ANNOTATION\)\)

So the cdr of each bookmark is an alist too.")

(defmacro gnus-bookmark-mouse-available-p ()
  "Return non-nil if a mouse is available."
  (if (featurep 'xemacs)
      '(device-on-window-system-p)
    '(display-mouse-p)))

(defun gnus-bookmark-remove-properties (string)
  "Remove all text properties from STRING."
  (set-text-properties 0 (length string) nil string)
  string)

;;;###autoload
(defun gnus-bookmark-set ()
  "Set a bookmark for this article."
  (interactive)
  (gnus-bookmark-maybe-load-default-file)
  (if (or (not (eq major-mode 'gnus-summary-mode))
	  (not gnus-article-current))
      (error "Please select an article in the Gnus summary buffer")
    (let* ((group (car gnus-article-current))
	   (article (cdr gnus-article-current))
	   (header (gnus-summary-article-header article))
	   (author (mail-header-from header))
	   (message-id (mail-header-id header))
	   (date (mail-header-date header))
	   (subject (gnus-summary-subject-string))
	   (bmk-name (gnus-bookmark-set-bookmark-name group author subject))
	   ;; Maybe ask for annotation
	   (annotation
	    (if gnus-bookmark-use-annotations
		 (read-from-minibuffer
		  (format "Annotation for %s: " bmk-name)) "")))
      ;; Set the bookmark list
      (setq gnus-bookmark-alist
	    (cons
	     (list (gnus-bookmark-remove-properties bmk-name)
		   (gnus-bookmark-make-record
		    group message-id author date subject annotation))
	     gnus-bookmark-alist))))
  (gnus-bookmark-bmenu-surreptitiously-rebuild-list)
  (gnus-bookmark-write-file))

(defun gnus-bookmark-make-record
  (group message-id author date subject annotation)
  "Return the record part of a new bookmark, given GROUP MESSAGE-ID AUTHOR DATE SUBJECT and ANNOTATION."
  (let ((the-record
	 `((group . ,(gnus-bookmark-remove-properties group))
	   (message-id . ,(gnus-bookmark-remove-properties message-id))
	   (author . ,(gnus-bookmark-remove-properties author))
	   (date . ,(gnus-bookmark-remove-properties date))
	   (subject . ,(gnus-bookmark-remove-properties subject))
	   (annotation . ,(gnus-bookmark-remove-properties annotation)))))
    the-record))

(defun gnus-bookmark-set-bookmark-name (group author subject)
  "Set bookmark name from GROUP AUTHOR and SUBJECT."
  (let* ((subject (split-string subject))
	 (default-name-0 ;; Should be merged with -1?
	   (concat (car (nreverse (delete "" (split-string group "[\\.:]"))))
		   "-" (car (split-string author))
		   "-" (car subject) "-" (cadr subject)))
	 (default-name-1
	   ;; Strip "[]" chars from the bookmark name:
	   (gnus-replace-in-string default-name-0 "[]_[]" ""))
	 (name (read-from-minibuffer
		(format "Set bookmark (%s): " default-name-1)
		nil nil nil nil
		default-name-1)))
    (if (string-equal name "")
	default-name-1
      name)))

(defun gnus-bookmark-write-file ()
  "Write currently defined Gnus bookmarks into `gnus-bookmark-default-file'."
  (interactive)
  (save-excursion
    (save-window-excursion
      ;; Avoir warnings?
      ;; (message "Saving Gnus bookmarks to file %s..." gnus-bookmark-default-file)
      (set-buffer (get-buffer-create  " *Gnus bookmarks*"))
      (erase-buffer)
      (gnus-bookmark-insert-file-format-version-stamp)
      (pp gnus-bookmark-alist (current-buffer))
      (condition-case nil
	  (let ((coding-system-for-write gnus-bookmark-file-coding-system))
	    (write-region (point-min) (point-max)
			  gnus-bookmark-default-file))
	(file-error (message "Can't write %s"
			     gnus-bookmark-default-file)))
      (kill-buffer (current-buffer))
      (message
       "Saving Gnus bookmarks to file %s...done"
       gnus-bookmark-default-file))))

(defun gnus-bookmark-insert-file-format-version-stamp ()
  "Insert text indicating current version of Gnus bookmark file format."
  (insert
   (format ";;;; Gnus Bookmark Format Version %d %s;;;;\n"
	   gnus-bookmark-file-format-version
	   (if gnus-bookmark-file-coding-system
	       (concat "-*- coding: "
		       (symbol-name gnus-bookmark-file-coding-system)
		       "; -*- ")
	     "")))
  (insert ";;; This format is meant to be slightly human-readable;\n"
          ";;; nevertheless, you probably don't want to edit it.\n"
          ";;; "
          gnus-bookmark-end-of-version-stamp-marker))

;;;###autoload
(defun gnus-bookmark-jump (&optional bmk-name)
  "Jump to a Gnus bookmark (BMK-NAME)."
  (interactive)
  (gnus-bookmark-maybe-load-default-file)
  (let* ((bookmark (or bmk-name
                       (gnus-completing-read "Jump to bookmarked article"
                                             (mapcar 'car gnus-bookmark-alist))))
	 (bmk-record (cadr (assoc bookmark gnus-bookmark-alist)))
	 (group (cdr (assoc 'group bmk-record)))
	 (message-id (cdr (assoc 'message-id bmk-record))))
    (when group
      (unless (get-buffer gnus-group-buffer)
	(gnus-no-server))
      (gnus-activate-group group)
      (gnus-group-quick-select-group 0 group))
    (if message-id
      (or (gnus-summary-goto-article message-id nil 'force)
	  (if (fboundp 'gnus-summary-insert-cached-articles)
	      (progn
		(gnus-summary-insert-cached-articles)
		(gnus-summary-goto-article message-id nil 'force))
	    (message "Message could not be found."))))))

(defvar gnus-bookmark-already-loaded nil)

(defun gnus-bookmark-alist-from-buffer ()
  "Return a `gnus-bookmark-alist' from the current buffer.
The buffer must of course contain Gnus bookmark format information.
Does not care from where in the buffer it is called, and does not
affect point."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward
	 gnus-bookmark-end-of-version-stamp-marker nil t)
        (read (current-buffer))
      ;; Else no hope of getting information here.
      (error "Not Gnus bookmark format"))))

(defun gnus-bookmark-load (file)
  "Load Gnus bookmarks from FILE (which must be in bookmark format)."
  (interactive
   (list (read-file-name
          (format "Load Gnus bookmarks from: (%s) "
                  gnus-bookmark-default-file)
          "~/" gnus-bookmark-default-file 'confirm)))
  (setq file (expand-file-name file))
  (if (file-readable-p file)
      (save-excursion
	(save-window-excursion
	  (set-buffer (let ((enable-local-variables nil))
                        (find-file-noselect file)))
          (goto-char (point-min))
	  (let ((blist (gnus-bookmark-alist-from-buffer)))
	    (if (listp blist)
		(progn (setq gnus-bookmark-already-loaded t)
		       (setq gnus-bookmark-alist blist))
	      (error "Not Gnus bookmark format")))))))

(defun gnus-bookmark-maybe-load-default-file ()
  "Maybe load Gnus bookmarks in `gnus-bookmark-alist'."
  (and (not gnus-bookmark-already-loaded)
       (null gnus-bookmark-alist)
       (file-readable-p (expand-file-name gnus-bookmark-default-file))
       (gnus-bookmark-load gnus-bookmark-default-file)))

(defun gnus-bookmark-maybe-sort-alist ()
  "Return the gnus-bookmark-alist for display.
If the gnus-bookmark-sort-flag is non-nil, then return a sorted
copy of the alist."
  (when gnus-bookmark-sort-flag
    (setq gnus-bookmark-alist
	  (sort (copy-alist gnus-bookmark-alist)
		(function
		 (lambda (x y) (string-lessp (car x) (car y))))))))

;;;###autoload
(defun gnus-bookmark-bmenu-list ()
  "Display a list of existing Gnus bookmarks.
The list is displayed in a buffer named `*Gnus Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
  (interactive)
  (gnus-bookmark-maybe-load-default-file)
  (if (interactive-p)
      (switch-to-buffer (get-buffer-create "*Gnus Bookmark List*"))
    (set-buffer (get-buffer-create "*Gnus Bookmark List*")))
  (let ((inhibit-read-only t)
	alist name start end)
    (erase-buffer)
    (insert "% Gnus Bookmark\n- --------\n")
    (add-text-properties (point-min) (point)
			 '(font-lock-face gnus-bookmark-menu-heading))
    ;; sort before displaying
    (gnus-bookmark-maybe-sort-alist)
    ;; Display gnus bookmarks
    (setq alist gnus-bookmark-alist)
    (while alist
      (setq name (gnus-bookmark-name-from-full-record (pop alist)))
      ;; if a Gnus bookmark has an annotation, prepend a "*"
      ;; in the list of bookmarks.
      (insert (if (member (gnus-bookmark-get-annotation name) (list nil ""))
		  "  "
		" *"))
      (if (gnus-bookmark-mouse-available-p)
	  (add-text-properties
	   (prog1
	       (point)
	     (insert name))
	   (let ((end (point)))
	     (prog2
		 (re-search-backward "[^ \t]")
		 (1+ (point))
	       (goto-char end)
	       (insert "\n")))
	   `(mouse-face highlight follow-link t
			help-echo ,(format "%s: go to this article"
					   (aref gnus-mouse-2 0))))
	(insert name "\n")))
    (goto-char (point-min))
    (forward-line 2)
    (gnus-bookmark-bmenu-mode)
    (if gnus-bookmark-bmenu-toggle-infos
	(gnus-bookmark-bmenu-toggle-infos t))))

(defun gnus-bookmark-bmenu-surreptitiously-rebuild-list ()
  "Rebuild the Bookmark List if it exists.
Don't affect the buffer ring order."
  (if (get-buffer "*Gnus Bookmark List*")
      (save-excursion
        (save-window-excursion
          (gnus-bookmark-bmenu-list)))))

(defun gnus-bookmark-get-annotation (bookmark)
  "Return the annotation of Gnus BOOKMARK, or nil if none."
  (cdr (assq 'annotation (gnus-bookmark-get-bookmark-record bookmark))))

(defun gnus-bookmark-get-bookmark (bookmark)
  "Return the full entry for Gnus BOOKMARK in `gnus-bookmark-alist'.
If BOOKMARK is not a string, return nil."
  (when (stringp bookmark)
    (assoc bookmark gnus-bookmark-alist)))

(defun gnus-bookmark-get-bookmark-record (bookmark)
  "Return the guts of the entry for Gnus BOOKMARK in `gnus-bookmark-alist'.
That is, all information but the name."
  (car (cdr (gnus-bookmark-get-bookmark bookmark))))

(defun gnus-bookmark-name-from-full-record (full-record)
  "Return name of FULL-RECORD \(an alist element instead of a string\)."
  (car full-record))

(defvar gnus-bookmark-bmenu-bookmark-column nil)
(defvar gnus-bookmark-bmenu-hidden-bookmarks ())
(defvar gnus-bookmark-bmenu-mode-map nil)

(if gnus-bookmark-bmenu-mode-map
    nil
  (setq gnus-bookmark-bmenu-mode-map (make-keymap))
  (suppress-keymap gnus-bookmark-bmenu-mode-map t)
  (define-key gnus-bookmark-bmenu-mode-map "q" (if (fboundp 'quit-window)
						   'quit-window
						 'bury-buffer))
  (define-key gnus-bookmark-bmenu-mode-map "\C-m" 'gnus-bookmark-bmenu-select)
  (define-key gnus-bookmark-bmenu-mode-map "v" 'gnus-bookmark-bmenu-select)
  (define-key gnus-bookmark-bmenu-mode-map "d" 'gnus-bookmark-bmenu-delete)
  (define-key gnus-bookmark-bmenu-mode-map "k" 'gnus-bookmark-bmenu-delete)
  (define-key gnus-bookmark-bmenu-mode-map "\C-d" 'gnus-bookmark-bmenu-delete-backwards)
  (define-key gnus-bookmark-bmenu-mode-map "x" 'gnus-bookmark-bmenu-execute-deletions)
  (define-key gnus-bookmark-bmenu-mode-map " " 'next-line)
  (define-key gnus-bookmark-bmenu-mode-map "n" 'next-line)
  (define-key gnus-bookmark-bmenu-mode-map "p" 'previous-line)
  (define-key gnus-bookmark-bmenu-mode-map "\177" 'gnus-bookmark-bmenu-backup-unmark)
  (define-key gnus-bookmark-bmenu-mode-map "?" 'describe-mode)
  (define-key gnus-bookmark-bmenu-mode-map "u" 'gnus-bookmark-bmenu-unmark)
  (define-key gnus-bookmark-bmenu-mode-map "m" 'gnus-bookmark-bmenu-mark)
  (define-key gnus-bookmark-bmenu-mode-map "l" 'gnus-bookmark-bmenu-load)
  (define-key gnus-bookmark-bmenu-mode-map "s" 'gnus-bookmark-bmenu-save)
  (define-key gnus-bookmark-bmenu-mode-map "t" 'gnus-bookmark-bmenu-toggle-infos)
  (define-key gnus-bookmark-bmenu-mode-map "a" 'gnus-bookmark-bmenu-show-details)
  (define-key gnus-bookmark-bmenu-mode-map gnus-mouse-2
    'gnus-bookmark-bmenu-select-by-mouse))

;; Bookmark Buffer Menu mode is suitable only for specially formatted
;; data.
(put 'gnus-bookmark-bmenu-mode 'mode-class 'special)

;; Been to lazy to use gnus-bookmark-save...
(defalias 'gnus-bookmark-bmenu-save 'gnus-bookmark-write-file)

(defun gnus-bookmark-bmenu-mode ()
  "Major mode for editing a list of Gnus bookmarks.
Each line describes one of the bookmarks in Gnus.
Letters do not insert themselves; instead, they are commands.
Gnus bookmarks names preceded by a \"*\" have annotations.
\\<gnus-bookmark-bmenu-mode-map>
\\[gnus-bookmark-bmenu-mark] -- mark bookmark to be displayed.
\\[gnus-bookmark-bmenu-select] -- select bookmark of line point is on.
  Also show bookmarks marked using m in other windows.
\\[gnus-bookmark-bmenu-toggle-infos] -- toggle displaying of details (they may obscure long bookmark names).
\\[gnus-bookmark-bmenu-locate] -- display (in minibuffer) location of this bookmark.
\\[gnus-bookmark-bmenu-rename] -- rename this bookmark \(prompts for new name\).
\\[gnus-bookmark-bmenu-delete] -- mark this bookmark to be deleted, and move down.
\\[gnus-bookmark-bmenu-delete-backwards] -- mark this bookmark to be deleted, and move up.
\\[gnus-bookmark-bmenu-execute-deletions] -- delete bookmarks marked with `\\[gnus-bookmark-bmenu-delete]'.
\\[gnus-bookmark-bmenu-load] -- load in a file of bookmarks (prompts for file.)
\\[gnus-bookmark-bmenu-save] -- load in a file of bookmarks (prompts for file.)
\\[gnus-bookmark-bmenu-unmark] -- remove all kinds of marks from current line.
  With prefix argument, also move up one line.
\\[gnus-bookmark-bmenu-backup-unmark] -- back up a line and remove marks.
\\[gnus-bookmark-bmenu-show-details] -- show the annotation, if it exists, for the current bookmark
  in another buffer.
\\[gnus-bookmark-bmenu-show-all-annotations] -- show the annotations of all bookmarks in another buffer.
\\[gnus-bookmark-bmenu-edit-annotation] -- edit the annotation for the current bookmark."
  (kill-all-local-variables)
  (use-local-map gnus-bookmark-bmenu-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'gnus-bookmark-bmenu-mode)
  (setq mode-name "Bookmark Menu")
  (gnus-run-mode-hooks 'gnus-bookmark-bmenu-mode-hook))

;; avoid compilation warnings
(defvar gnus-bookmark-bmenu-toggle-infos nil)

(defun gnus-bookmark-bmenu-toggle-infos (&optional show)
  "Toggle whether details are shown in the Gnus bookmark list.
Optional argument SHOW means show them unconditionally."
  (interactive)
  (cond
   (show
    (setq gnus-bookmark-bmenu-toggle-infos nil)
    (gnus-bookmark-bmenu-show-infos)
    (setq gnus-bookmark-bmenu-toggle-infos t))
   (gnus-bookmark-bmenu-toggle-infos
    (gnus-bookmark-bmenu-hide-infos)
    (setq gnus-bookmark-bmenu-toggle-infos nil))
   (t
    (gnus-bookmark-bmenu-show-infos)
    (setq gnus-bookmark-bmenu-toggle-infos t))))

(defun gnus-bookmark-bmenu-show-infos (&optional force)
  "Show infos in bmenu, maybe FORCE display of infos."
  (if (and (not force) gnus-bookmark-bmenu-toggle-infos)
      nil ;already shown, so do nothing
    (save-excursion
      (save-window-excursion
        (goto-char (point-min))
        (forward-line 2)
        (setq gnus-bookmark-bmenu-hidden-bookmarks ())
        (let ((inhibit-read-only t))
          (while (< (point) (point-max))
            (let ((bmrk (gnus-bookmark-bmenu-bookmark)))
              (setq gnus-bookmark-bmenu-hidden-bookmarks
                    (cons bmrk gnus-bookmark-bmenu-hidden-bookmarks))
	      (let ((start (point-at-eol)))
		(move-to-column gnus-bookmark-bmenu-file-column t)
		;; Strip off `mouse-face' from the white spaces region.
		(if (gnus-bookmark-mouse-available-p)
		    (remove-text-properties start (point)
					    '(mouse-face nil help-echo nil))))
	      (delete-region (point) (progn (end-of-line) (point)))
              (insert "  ")
              ;; Pass the NO-HISTORY arg:
              (gnus-bookmark-insert-details bmrk)
              (forward-line 1))))))))

(defun gnus-bookmark-insert-details (bmk-name)
  "Insert the details of the article associated with BMK-NAME."
  (let ((start (point)))
    (prog1
	(insert (gnus-bookmark-get-details
		 bmk-name
		 gnus-bookmark-bookmark-inline-details))
      (if (gnus-bookmark-mouse-available-p)
	  (add-text-properties
	   start
	   (save-excursion (re-search-backward
			    "[^ \t]")
					       (1+ (point)))
	   `(mouse-face highlight
	     follow-link t
	     help-echo ,(format "%s: go to this article"
				(aref gnus-mouse-2 0))))))))

(defun gnus-bookmark-kill-line (&optional newline-too)
  "Kill from point to end of line.
If optional arg NEWLINE-TOO is non-nil, delete the newline too.
Does not affect the kill ring."
  (delete-region (point) (point-at-eol))
  (if (and newline-too (looking-at "\n"))
      (delete-char 1)))

(defun gnus-bookmark-get-details (bmk-name details-list)
  "Get details for a Gnus BMK-NAME depending on DETAILS-LIST."
  (let ((details (cadr (assoc bmk-name gnus-bookmark-alist))))
    (mapconcat
     (lambda (info)
       (cdr (assoc info details)))
     details-list " | ")))

(defun gnus-bookmark-bmenu-hide-infos (&optional force)
  "Hide infos in bmenu, maybe FORCE."
  (if (and (not force) gnus-bookmark-bmenu-toggle-infos)
      ;; nothing to hide if above is nil
      (save-excursion
        (save-window-excursion
          (goto-char (point-min))
          (forward-line 2)
          (setq gnus-bookmark-bmenu-hidden-bookmarks
                (nreverse gnus-bookmark-bmenu-hidden-bookmarks))
          (save-excursion
            (goto-char (point-min))
            (search-forward "Gnus Bookmark")
            (backward-word 2)
            (setq gnus-bookmark-bmenu-bookmark-column (current-column)))
          (save-excursion
            (let ((inhibit-read-only t))
              (while gnus-bookmark-bmenu-hidden-bookmarks
                (move-to-column gnus-bookmark-bmenu-bookmark-column t)
                (gnus-bookmark-kill-line)
		(let ((start (point)))
		  (insert (car gnus-bookmark-bmenu-hidden-bookmarks))
		  (if (gnus-bookmark-mouse-available-p)
		      (add-text-properties
		       start
		       (save-excursion (re-search-backward
					"[^ \t]")
				       (1+ (point)))
		       `(mouse-face highlight
			 follow-link t
			 help-echo
			 ,(format "%s: go to this bookmark in other window"
				  (aref gnus-mouse-2 0))))))
                (setq gnus-bookmark-bmenu-hidden-bookmarks
                      (cdr gnus-bookmark-bmenu-hidden-bookmarks))
                (forward-line 1))))))))

(defun gnus-bookmark-bmenu-check-position ()
  "Return non-nil if on a line with a bookmark.
The actual value returned is gnus-bookmark-alist.  Else
reposition and try again, else return nil."
  (cond ((< (count-lines (point-min) (point)) 2)
         (goto-char (point-min))
         (forward-line 2)
         gnus-bookmark-alist)
        ((and (bolp) (eobp))
         (beginning-of-line 0)
         gnus-bookmark-alist)
        (t
         gnus-bookmark-alist)))

(defun gnus-bookmark-bmenu-bookmark ()
  "Return a string which is bookmark of this line."
  (if (gnus-bookmark-bmenu-check-position)
      (save-excursion
        (save-window-excursion
          (goto-char (point-min))
          (search-forward "Gnus Bookmark")
          (backward-word 2)
          (setq gnus-bookmark-bmenu-bookmark-column (current-column)))))
  (if gnus-bookmark-bmenu-toggle-infos
      (gnus-bookmark-bmenu-hide-infos))
  (save-excursion
    (save-window-excursion
      (beginning-of-line)
      (forward-char gnus-bookmark-bmenu-bookmark-column)
      (prog1
          (buffer-substring-no-properties (point)
                            (progn
                              (end-of-line)
                              (point)))
        ;; well, this is certainly crystal-clear:
        (if gnus-bookmark-bmenu-toggle-infos
            (gnus-bookmark-bmenu-toggle-infos t))))))

(defun gnus-bookmark-show-details (bookmark)
  "Display the annotation for BOOKMARK in a buffer."
  (let ((record (gnus-bookmark-get-bookmark-record bookmark))
	(old-buf (current-buffer))
	(details gnus-bookmark-bookmark-details)
	detail)
    (save-excursion
      (pop-to-buffer (get-buffer-create "*Gnus Bookmark Annotation*") t)
      (erase-buffer)
      (while details
	(setq detail (pop details))
	(unless (equal (cdr (assoc detail record)) "")
	  (insert (symbol-name detail) ": " (cdr (assoc detail record)) "\n")))
      (goto-char (point-min))
      (pop-to-buffer old-buf))))

(defun gnus-bookmark-bmenu-show-details ()
  "Show the annotation for the current bookmark in another window."
  (interactive)
  (let ((bookmark (gnus-bookmark-bmenu-bookmark)))
    (if (gnus-bookmark-bmenu-check-position)
	(gnus-bookmark-show-details bookmark))))

(defun gnus-bookmark-bmenu-mark ()
  "Mark bookmark on this line to be displayed by \\<gnus-bookmark-bmenu-mode-map>\\[gnus-bookmark-bmenu-select]."
  (interactive)
  (beginning-of-line)
  (if (gnus-bookmark-bmenu-check-position)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ?>)
        (forward-line 1)
        (gnus-bookmark-bmenu-check-position))))

(defun gnus-bookmark-bmenu-unmark (&optional backup)
  "Cancel all requested operations on bookmark on this line and move down.
Optional BACKUP means move up."
  (interactive "P")
  (beginning-of-line)
  (if (gnus-bookmark-bmenu-check-position)
      (progn
        (let ((inhibit-read-only t))
          (delete-char 1)
          ;; any flags to reset according to circumstances?  How about a
          ;; flag indicating whether this bookmark is being visited?
          ;; well, we don't have this now, so maybe later.
          (insert " "))
        (forward-line (if backup -1 1))
        (gnus-bookmark-bmenu-check-position))))

(defun gnus-bookmark-bmenu-backup-unmark ()
  "Move up and cancel all requested operations on bookmark on line above."
  (interactive)
  (forward-line -1)
  (if (gnus-bookmark-bmenu-check-position)
      (progn
        (gnus-bookmark-bmenu-unmark)
        (forward-line -1)
        (gnus-bookmark-bmenu-check-position))))

(defun gnus-bookmark-bmenu-delete ()
  "Mark Gnus bookmark on this line to be deleted.
To carry out the deletions that you've marked, use
\\<gnus-bookmark-bmenu-mode-map>\\[gnus-bookmark-bmenu-execute-deletions]."
  (interactive)
  (beginning-of-line)
  (if (gnus-bookmark-bmenu-check-position)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ?D)
        (forward-line 1)
        (gnus-bookmark-bmenu-check-position))))

(defun gnus-bookmark-bmenu-delete-backwards ()
  "Mark bookmark on this line to be deleted, then move up one line.
To carry out the deletions that you've marked, use
\\<gnus-bookmark-bmenu-mode-map>\\[gnus-bookmark-bmenu-execute-deletions]."
  (interactive)
  (gnus-bookmark-bmenu-delete)
  (forward-line -2)
  (if (gnus-bookmark-bmenu-check-position)
      (forward-line 1))
  (gnus-bookmark-bmenu-check-position))

(defun gnus-bookmark-bmenu-select ()
  "Select this line's bookmark; also display bookmarks marked with `>'.
You can mark bookmarks with the
\\<gnus-bookmark-bmenu-mode-map>\\[gnus-bookmark-bmenu-mark]
command."
  (interactive)
  (if (gnus-bookmark-bmenu-check-position)
      (let ((bmrk (gnus-bookmark-bmenu-bookmark))
            (menu (current-buffer)))
        (goto-char (point-min))
        (delete-other-windows)
        (gnus-bookmark-jump bmrk)
        (bury-buffer menu))))

(defun gnus-bookmark-bmenu-select-by-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (gnus-bookmark-bmenu-select))

(defun gnus-bookmark-bmenu-load ()
  "Load the Gnus bookmark file and rebuild the bookmark menu-buffer."
  (interactive)
  (if (gnus-bookmark-bmenu-check-position)
      (save-excursion
        (save-window-excursion
          ;; This will call `gnus-bookmark-bmenu-list'
          (call-interactively 'gnus-bookmark-load)))))

(defun gnus-bookmark-bmenu-execute-deletions ()
  "Delete Gnus bookmarks marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-delete] commands."
  (interactive)
  (message "Deleting Gnus bookmarks...")
  (let ((hide-em gnus-bookmark-bmenu-toggle-infos)
        (o-point  (point))
        (o-str    (save-excursion
                    (beginning-of-line)
                    (if (looking-at "^D")
                        nil
                      (buffer-substring
                       (point)
                       (progn (end-of-line) (point))))))
        (o-col     (current-column)))
    (if hide-em (gnus-bookmark-bmenu-hide-infos))
    (setq gnus-bookmark-bmenu-toggle-infos nil)
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^D" (point-max) t)
      (gnus-bookmark-delete (gnus-bookmark-bmenu-bookmark) t)) ; pass BATCH arg
    (gnus-bookmark-bmenu-list)
    (setq gnus-bookmark-bmenu-toggle-infos hide-em)
    (if gnus-bookmark-bmenu-toggle-infos
        (gnus-bookmark-bmenu-toggle-infos t))
    (if o-str
        (progn
          (goto-char (point-min))
          (search-forward o-str)
          (beginning-of-line)
          (forward-char o-col))
      (goto-char o-point))
    (beginning-of-line)
    (gnus-bookmark-write-file)
    (message "Deleting bookmarks...done")))

(defun gnus-bookmark-delete (bookmark &optional batch)
  "Delete BOOKMARK from the bookmark list.
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark \(that is, the
one most recently used in this file, if any\).
Optional second arg BATCH means don't update the bookmark list buffer,
probably because we were called from there."
  (gnus-bookmark-maybe-load-default-file)
  (let ((will-go (gnus-bookmark-get-bookmark bookmark)))
    (setq gnus-bookmark-alist (delq will-go gnus-bookmark-alist)))
  ;; Don't rebuild the list
  (if batch
      nil
    (gnus-bookmark-bmenu-surreptitiously-rebuild-list)))

(provide 'gnus-bookmark)

;;; gnus-bookmark.el ends here
