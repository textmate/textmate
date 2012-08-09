;;; page-ext.el --- extended page handling commands

;; Copyright (C) 1990-1991, 1993-1994, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Robert J. Chassell <bob@gnu.org>
;; (according to ack.texi)
;; Keywords: wp data

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

;; You may use these commands to handle an address list or other
;; small data base.


;;; Summary

;; The current page commands are:

;;     forward-page         C-x ]
;;     backward-page        C-x [
;;     narrow-to-page       C-x p
;;     count-lines-page     C-x l
;;     mark-page            C-x C-p  (change this to C-x C-p C-m)
;;     sort-pages           not bound
;;     what-page            not bound

;; The new page handling commands all use `C-x C-p' as a prefix.  This
;; means that the key binding for `mark-page' must be changed.
;; Otherwise, no other changes are made to the current commands or
;; their bindings.

;; New page handling commands:

;;     next-page                        C-x C-p C-n
;;     previous-page                    C-x C-p C-p
;;     search-pages                     C-x C-p C-s
;;     add-new-page                     C-x C-p C-a
;;     sort-pages-buffer                C-x C-p s
;;     set-page-delimiter               C-x C-p C-l
;;     pages-directory                  C-x C-p C-d
;;     pages-directory-for-addresses    C-x C-p d
;;        pages-directory-goto          C-c C-c


;;; Using the page commands

;; The page commands are helpful in several different contexts.  For
;; example, programmers often divide source files into sections using the
;; `page-delimiter'; you can use the `pages-directory' command to list
;; the sections.

;; You may change the buffer local value of the `page-delimiter' with
;; the `set-page-delimiter' command.  This command is bound to `C-x C-p
;; C-l' The command prompts you for a new value for the page-delimiter.
;; Called with a prefix-arg, the command resets the value of the
;; page-delimiter to its original value.

;; You may set several user options:
;;
;;   The `pages-directory-buffer-narrowing-p' variable causes the
;;   `pages-directory-goto' command to narrow to the destination page.
;;
;;   The `pages-directory-for-adding-page-narrowing-p' variable, causes the
;;   `add-new-page' command to narrow to the new entry.
;;
;;   The `pages-directory-for-adding-new-page-before-current-page-p' variable
;;   causes the `add-new-page' command to insert a new page before current
;;   page.
;;
;; These variables are true by default.
;;
;; Additional, addresses-related user options are described in the next page
;; of this file.


;;; Handling an address list or small data base

;; You may use the page commands to handle an address list or other
;; small data base.  Put each address or entry on its own page.  The
;; first line of text in each page is a `header line' and is listed by
;; the `pages-directory' or `pages-directory-for-addresses' command.

;; Specifically:
;;
;;   1. Begin each entry with a `page-delimiter' (which is, by default,
;;      `^L' at the beginning of the line).
;;
;;   2. The first line of text in each entry is the `heading line'; it
;;      will appear in the pages-directory-buffer which is constructed
;;      using the `C-x C-p C-d' (pages-directory) command or the `C-x
;;      C-p d' (pages-directory-for-addresses) command.
;;
;;      The heading line may be on the same line as the page-delimiter
;;      or it may follow after.  It is the first non-blank line on the
;;      page.  Conventionally, the heading line is placed on the line
;;      immediately following the line containing page-delimiter.
;;
;;   3. Follow the heading line with the body of the entry.  The body
;;      extends up to the next `page-delimiter'.  The body may be of any
;;      length.  It is conventional to place a blank line after the last
;;      line of the body.

;; For example, a file might look like this:
;;
;;     FSF
;;     Free Software Foundation
;;     51 Franklin Street, Fifth Floor
;;     Boston, MA 02110-1301  USA.
;;     (617) 542-5942
;;     gnu@gnu.org
;;
;;     
;;     House Subcommittee on Intellectual Property,
;;     U.S. House of Representatives,
;;     Washington, DC  20515
;;
;;     Congressional committee concerned with permitting or preventing
;;     monopolistic restrictions on the use of software technology.
;;
;;     
;;     George Lakoff
;;     ``Women, Fire, and Dangerous Things:
;;     What Categories Reveal about the Mind''
;;     1987, Univ. of Chicago Press
;;
;;     About philosophy, Whorfian effects, and linguistics.
;;
;;     
;;     OBI   (On line text collection.)
;;     Open Book Initiative
;;     c/o Software Tool & Die
;;     1330 Beacon St, Brookline, MA 02146 USA
;;     (617) 739-0202
;;     obi@world.std.com

;; In this example, the heading lines are:
;;
;;     FSF
;;     House Subcommittee on Intellectual Property
;;     George Lakoff
;;     OBI (On line text collection.)

;; The `C-x C-p s' (sort-pages-buffer) command sorts the entries in the
;; buffer alphabetically.

;; You may use any of the page commands, including the `next-page',
;; `previous-page', `add-new-page', `mark-page', and `search-pages'
;; commands.

;; You may use either the `C-x C-p d' (pages-directory-for-addresses)
;; or the `C-x C-p C-d' (pages-directory) command to construct and
;; display a directory of all the heading lines.

;; In the directory, you may position the cursor over a heading line
;; and type `C-c C-c' (pages-directory-goto) to go to the entry to
;; which it refers in the pages buffer.

;; You can type `C-c C-p C-a' (add-new-page) to add a new entry in the
;; pages buffer or address file.  This is the same command you use to
;; add a new entry when you are in the pages buffer or address file.

;; If you wish, you may create several different directories,
;; one for each different buffer.

;; `pages-directory-for-addresses' in detail

;; The `pages-directory-for-addresses' assumes a default addresses
;; file.  You do not need to specify the addresses file but merely type
;; `C-x C-p d' from any buffer.  The command finds the file, constructs
;; a directory for it, and switches you to the directory.  If you call
;; the command with a prefix arg, `C-u C-x C-p d', it prompts you for a
;; file name.

;; You may customize the addresses commands:

;;   The `pages-addresses-file-name' variable determines the name of
;;   the addresses file; by default it is "~/addresses".

;;   The `pages-directory-for-addresses-goto-narrowing-p' variable
;;   determines whether `pages-directory-goto' narrows the addresses
;;   buffer to the entry, which it does by default.

;;   The `pages-directory-for-addresses-buffer-keep-windows-p' variable
;;   determines whether `pages-directory-for-addresses' deletes other
;;   windows to show as many lines as possible on the screen or works
;;   in the usual Emacs manner and keeps other windows.  Default is to
;;   keep other windows.

;;   The `pages-directory-for-adding-addresses-narrowing-p' variable
;;   determines whether `pages-directory-for-addresses' narrows the
;;   addresses buffer to a new entry when you are adding that entry.
;;   Default is to narrow to new entry, which means you see a blank
;;   screen before you write the new entry.

;; `pages-directory' in detail

;; Call the `pages-directory' command from the buffer for which you
;; want a directory created; it creates a directory for the buffer and
;; pops you to the directory.

;; The `pages-directory' command has several options:

;;   Called with a prefix arg, `C-u C-x C-p C-d', the `pages-directory'
;;   prompts you for a regular expression and only lists only those
;;   header lines that are part of pages that contain matches to the
;;   regexp.  In the example above, `C-u C-x C-p C-d 617 RET' would
;;   match the telephone area code of the first and fourth entries, so
;;   only the header lines of those two entries would appear in the
;;   pages-directory-buffer.
;;
;;   Called with a numeric argument, the `pages-directory' command
;;   lists the number of lines in each page.  This is helpful when you
;;   are printing hardcopy.

;;   Called with a negative numeric argument, the `pages-directory'
;;   command lists the lengths of pages whose contents match a regexp.

;;; Code:


;;; Customarily customizable variable definitions

(defgroup pages nil
  "Extended page-handling commands."
  :group 'extensions)


(defcustom pages-directory-buffer-narrowing-p t
  "If non-nil, `pages-directory-goto' narrows pages buffer to entry."
  :type 'boolean
  :group 'pages)

(defcustom pages-directory-for-adding-page-narrowing-p t
  "If non-nil, `add-new-page' narrows page buffer to new entry."
  :type 'boolean
  :group 'pages)

(defcustom pages-directory-for-adding-new-page-before-current-page-p t
  "If non-nil, `add-new-page' inserts new page before current page."
  :type 'boolean
  :group 'pages)


;;; Addresses related variables

(defcustom pages-addresses-file-name "~/addresses"
  "Standard name for file of addresses. Entries separated by page-delimiter.
Used by `pages-directory-for-addresses' function."
  :type 'file
  :group 'pages)

(defcustom pages-directory-for-addresses-goto-narrowing-p t
  "If non-nil, `pages-directory-goto' narrows addresses buffer to entry."
  :type 'boolean
  :group 'pages)

(defcustom pages-directory-for-addresses-buffer-keep-windows-p t
  "If nil, `pages-directory-for-addresses' deletes other windows."
  :type 'boolean
  :group 'pages)

(defcustom pages-directory-for-adding-addresses-narrowing-p t
  "If non-nil, `add-new-page' narrows addresses buffer to new entry."
  :type 'boolean
  :group 'pages)


;;; Key bindings for page handling functions

(global-unset-key "\C-x\C-p")

(defvar ctl-x-ctl-p-map (make-sparse-keymap)
  "Keymap for subcommands of C-x C-p, which are for page handling.")

(define-key ctl-x-map "\C-p" 'ctl-x-ctl-p-prefix)
(fset 'ctl-x-ctl-p-prefix ctl-x-ctl-p-map)

(define-key ctl-x-ctl-p-map "\C-n" 'next-page)
(define-key ctl-x-ctl-p-map "\C-p" 'previous-page)
(define-key ctl-x-ctl-p-map "\C-a" 'add-new-page)
(define-key ctl-x-ctl-p-map "\C-m" 'mark-page)
(define-key ctl-x-ctl-p-map "\C-s" 'search-pages)
(define-key ctl-x-ctl-p-map "s"    'sort-pages-buffer)
(define-key ctl-x-ctl-p-map "\C-l" 'set-page-delimiter)
(define-key ctl-x-ctl-p-map "\C-d" 'pages-directory)
(define-key ctl-x-ctl-p-map "d"    'pages-directory-for-addresses)


;;; Page movement function definitions

(defun next-page (&optional count)
  "Move to the next page bounded by the `page-delimiter' variable.
With arg (prefix if interactive), move that many pages."
  (interactive "p")
  (or count (setq count 1))
  (widen)
  ;; Cannot use forward-page because of problems at page boundaries.
  (while (and (> count 0) (not (eobp)))
    (if (re-search-forward page-delimiter nil t)
        nil
      (goto-char (point-max)))
    (setq count (1- count)))
  ;; If COUNT is negative, we want to go back -COUNT + 1 page boundaries.
  ;; The first page boundary we reach is the top of the current page,
  ;; which doesn't count.
  (while (and (< count 1) (not (bobp)))
    (if (re-search-backward page-delimiter nil t)
	(goto-char (match-beginning 0))
      (goto-char (point-min)))
    (setq count (1+ count)))
  (narrow-to-page)
  (goto-char (point-min))
  (recenter 0))

(defun previous-page (&optional count)
  "Move to the previous page bounded by the `page-delimiter' variable.
With arg (prefix if interactive), move that many pages."
  (interactive "p")
  (or count (setq count 1))
  (next-page (- count)))


;;; Adding and searching pages

(defun add-new-page (header-line)
  "Insert new page.  Prompt for header line.

If point is in the pages directory buffer, insert the new page in the
buffer associated with the directory.

Insert the new page just before current page if
  pages-directory-for-adding-new-page-before-current-page-p  variable
is non-nil.  Else insert at exact location of point.

Narrow to new page if
  pages-directory-for-adding-page-narrowing-p variable
is non-nil.

Page begins with a `^L' as the default page-delimiter.
Use \\[set-page-delimiter] to change the page-delimiter.
Point is left in the body of page."
  (interactive "sHeader line: ")
  (widen)
  ;; If in pages directory buffer
  (if (eq major-mode 'pages-directory-mode)
      (progn
        ;; Add new page before or after current page?
        (if pages-directory-for-adding-new-page-before-current-page-p
            (pages-directory-goto)
          (pages-directory-goto)
          (forward-page)
          (or (eobp) (forward-line -1)))))
  (widen)
  ;; Move point before current delimiter if desired.
  (and pages-directory-for-adding-new-page-before-current-page-p
       (if (re-search-backward page-delimiter nil t)
           (goto-char (match-beginning 0))
         ;; If going to beginning of file, insert a page-delimiter
         ;; before current first page.
         (goto-char (point-min))
         (insert
          (format "%s\n"
                  ;; Remove leading `^' from page-delimiter string
                  (if (eq '^ (car (read-from-string page-delimiter)))
                      (substring page-delimiter 1))))
         (goto-char (point-min))))
  ;; Insert page delimiter at beginning of line.
  (if (not (looking-at "^."))   (forward-line 1))
  (insert (format "%s\n%s\n\n\n"
                  (if (eq '^ (car (read-from-string page-delimiter)))
                      (substring page-delimiter 1))
                  header-line))
  (forward-line -1)
  (and pages-directory-for-adding-page-narrowing-p (narrow-to-page)))

(defvar pages-last-search nil
  "Value of last regexp searched for.  Initially, nil.")

(defun search-pages (regexp)
  "Search for REGEXP, starting from point, and narrow to page it is in."
  (interactive (list
                (read-string
                 (format "Search for `%s' (end with RET): "
                         (or pages-last-search "regexp")))))
  (if (equal regexp "")
      (setq regexp pages-last-search)
    (setq pages-last-search regexp))
  (widen)
  (re-search-forward regexp)
  (narrow-to-page))


;;; Sorting pages

(autoload 'sort-subr "sort" "Primary function for sorting." t nil)

(defun sort-pages-in-region (reverse beg end)
  "Sort pages in region alphabetically.  Prefix arg means reverse order.

Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."

;;; This sort function handles ends of pages differently than
;;; `sort-pages' and works better with lists of addresses and similar
;;; files.

  (interactive "P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    ;;; `sort-subr' takes three arguments
    (sort-subr reverse

               ;; NEXTRECFUN is called with point at the end of the
               ;; previous record. It moves point to the start of the
               ;; next record.
	       (function (lambda ()
                           (re-search-forward page-delimiter nil t)
                           (skip-chars-forward " \t\n")
                           ))

               ;; ENDRECFUN is called with point within the record.
               ;; It should move point to the end of the record.
	       (function (lambda ()
                           (if (re-search-forward
                                page-delimiter
                                nil
                                t)
                               (goto-char (match-beginning 0))
                             (goto-char (point-max))))))))

(defun sort-pages-buffer (&optional reverse)
  "Sort pages alphabetically in buffer.  Prefix arg means reverse order.
\(Non-nil arg if not interactive.\)"

  (interactive "P")
  (or reverse (setq reverse nil))
  (widen)
  (let ((beginning (point-min))
        (end (point-max)))
    (sort-pages-in-region reverse beginning end)))


;;; Pages directory ancillary definitions

(defvar pages-directory-previous-regexp nil
  "Value of previous regexp used by `pages-directory'.
\(This regular expression may be used to select only those pages that
contain matches to the regexp.\)")

(defvar pages-buffer nil
  "The buffer for which the pages-directory function creates the directory.")

(defvar pages-directory-prefix "*Directory for:"
  "Prefix of name of temporary buffer for pages-directory.")

(defvar pages-pos-list nil
  "List containing the positions of the pages in the pages-buffer.")

(defvar pages-target-buffer)

(defvar pages-directory-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'pages-directory-goto)
    (define-key map "\C-c\C-p\C-a" 'add-new-page)
    (define-key map [mouse-2] 'pages-directory-goto-with-mouse)
    map)
  "Keymap for the pages-directory-buffer.")
(defvaralias 'pages-directory-map 'pages-directory-mode-map)

(defvar original-page-delimiter "^\f"
  "Default page delimiter.")

(defun set-page-delimiter (regexp reset-p)
  "Set buffer local value of page-delimiter to REGEXP.
Called interactively with a prefix argument, reset `page-delimiter' to
its original value.

In a program, non-nil second arg causes first arg to be ignored and
resets the page-delimiter to the original value."

  (interactive
   (if current-prefix-arg
       (list original-page-delimiter "^\f")
     (list (read-string "Set page-delimiter to regexp: " page-delimiter)
           nil)))
  (make-local-variable 'original-page-delimiter)
  (make-local-variable 'page-delimiter)
  (setq original-page-delimiter
        (or original-page-delimiter page-delimiter))
  (if (not reset-p)
      (setq page-delimiter regexp)
    (setq page-delimiter original-page-delimiter))
  (if (called-interactively-p 'interactive)
      (message "The value of `page-delimiter' is now: %s" page-delimiter)))


;;; Pages directory main definitions

(defun pages-directory
  (pages-list-all-headers-p count-lines-p &optional regexp)
  "Display a directory of the page headers in a temporary buffer.
A header is the first non-blank line after the page-delimiter.
\\[pages-directory-mode]
You may move point to one of the lines in the temporary buffer,
then use \\<pages-directory-goto> to go to the same line in the pages buffer.

In interactive use:

    1. With no prefix arg, display all headers.

    2. With prefix arg, display the headers of only those pages that
       contain matches to a regular expression for which you are
       prompted.

    3. With numeric prefix arg, for every page, print the number of
       lines within each page.

    4. With negative numeric prefix arg, for only those pages that
       match a regular expression, print the number of lines within
       each page.

When called from a program, non-nil first arg means list all headers;
non-nil second arg means print numbers of lines in each page; if first
arg is nil, optional third arg is regular expression.

If the buffer is narrowed, the `pages-directory' command creates a
directory for only the accessible portion of the buffer."

  (interactive
   (cond ((not current-prefix-arg)
          (list t nil nil))
         ((listp current-prefix-arg)
          (list nil
                nil
                (read-string
                 (format "Select according to `%s' (end with RET): "
                         (or pages-directory-previous-regexp "regexp")))))
         ((> (prefix-numeric-value current-prefix-arg) 0)
          (list t t nil))
         ((< (prefix-numeric-value current-prefix-arg) 0)
          (list nil
                t
                (read-string
                 (format "Select according to `%s' (end with RET): "
                         (or pages-directory-previous-regexp "regexp")))))))

  (if (equal regexp "")
      (setq regexp pages-directory-previous-regexp)
    (setq pages-directory-previous-regexp regexp))

  (if (called-interactively-p 'interactive)
      (message "Creating directory for: %s "
               (buffer-name)))

  (let ((pages-target-buffer (current-buffer))
        (pages-directory-buffer
	 (concat pages-directory-prefix " " (buffer-name)))
        (linenum 1)
        (pages-buffer-original-position (point))
        (pages-buffer-original-page 0))

    ;; `with-output-to-temp-buffer' binds the value of the variable
    ;; `standard-output' to the buffer named as its first argument,
    ;; but does not switch to that buffer.
    (with-output-to-temp-buffer pages-directory-buffer
      (with-current-buffer standard-output
        (pages-directory-mode)
        (insert
         "==== Pages Directory: use `C-c C-c' to go to page under cursor. ====" ?\n)
        (setq pages-buffer pages-target-buffer)
        (setq pages-pos-list nil))

      (if pages-list-all-headers-p

          ;; 1. If no prefix argument, list all headers
          (save-excursion
            (goto-char (point-min))

            ;; (a) Point is at beginning of buffer; but the first
            ;;     page may not begin with a page-delimiter
            (save-restriction
              ;; If page delimiter is at beginning of buffer, skip it
              (if (and (save-excursion
                         (re-search-forward page-delimiter nil t))
                       (= 1 (match-beginning 0)))
                  (goto-char (match-end 0)))
              (narrow-to-page)
              (pages-copy-header-and-position count-lines-p))

            ;; (b) Search within pages buffer for next page-delimiter
            (while (re-search-forward page-delimiter nil t)
              (pages-copy-header-and-position count-lines-p)))

        ;; 2. Else list headers whose pages match regexp.
        (save-excursion
          ;; REMOVED  save-restriction  AND  widen  FROM HERE
          (goto-char (point-min))

          ;; (a) Handle first page
          (save-restriction
            (narrow-to-page)
            ;; search for selection regexp
            (if (save-excursion (re-search-forward regexp nil t))
                (pages-copy-header-and-position count-lines-p)))

          ;; (b) Search for next page-delimiter
          (while (re-search-forward page-delimiter nil t)
            (save-restriction
              (narrow-to-page)
              ;; search for selection regexp
              (if (save-excursion (re-search-forward regexp nil t))
                  (pages-copy-header-and-position count-lines-p)
                )))))

      (set-buffer standard-output)
      ;; Put positions in increasing order to go with buffer.
      (setq pages-pos-list (nreverse pages-pos-list))
      (if (called-interactively-p 'interactive)
          (message "%d matching lines in: %s"
                   (length pages-pos-list) (buffer-name pages-target-buffer))))
    (pop-to-buffer pages-directory-buffer)
    (sit-for 0)  ; otherwise forward-line fails if N > window height.
    (forward-line (if (= 0 pages-buffer-original-page)
                      1
                    pages-buffer-original-page))))

(defvar pages-buffer-original-position)
(defvar pages-buffer-original-page)
(defvar pages-buffer-original-page)

(defun pages-copy-header-and-position (count-lines-p)
  "Copy page header and its position to the Pages Directory.
Only arg non-nil, count lines in page and insert before header.
Used by `pages-directory' function."

  (let (position line-count)

    (if count-lines-p
        (save-excursion
          (save-restriction
            (narrow-to-page)
            (setq line-count (count-lines (point-min) (point-max))))))

    ;; Keep track of page for later cursor positioning
    (if (<= (point) pages-buffer-original-position)
        (setq pages-buffer-original-page
              (1+ pages-buffer-original-page)))

    (save-excursion
      ;; go to first non-blank char after the page-delimiter
      (skip-chars-forward " \t\n")
      ;; set the marker here; this the place to which the
      ;; `pages-directory-goto' command will go
      (setq position (make-marker))
      (set-marker position (point))
      (let ((start (point))
            (end (line-end-position))
	    inserted-at)
        ;; change to directory buffer
        (set-buffer standard-output)
        ;; record page position
        (setq pages-pos-list (cons position pages-pos-list))
        ;; insert page header
	(setq inserted-at (point))
	(insert-buffer-substring pages-target-buffer start end)
	(add-text-properties inserted-at (point)
			     '(mouse-face highlight
			       help-echo "mouse-2: go to this page"))
	(put-text-property inserted-at (point) 'rear-nonsticky 'highlight))

      (if count-lines-p
          (save-excursion
            (beginning-of-line)
            (insert (format "%3d: " line-count))))

      (terpri))
    (end-of-line 1)))

(defun pages-directory-mode ()
  "Mode for handling the pages-directory buffer.

Move point to one of the lines in this buffer, then use \\[pages-directory-goto] to go
to the same line in the pages buffer."

  (kill-all-local-variables)
  (use-local-map pages-directory-mode-map)
  (setq major-mode 'pages-directory-mode)
  (setq mode-name "Pages-Directory")
  (make-local-variable 'pages-buffer)
  (make-local-variable 'pages-pos-list)
  (make-local-variable 'pages-directory-buffer-narrowing-p)
  (run-mode-hooks 'pages-directory-mode-hook))

(defun pages-directory-goto ()
  "Go to the corresponding line in the pages buffer."

;;; This function is mostly a copy of `occur-mode-goto-occurrence'

  (interactive)
  (if (or (not pages-buffer)
	  (not (buffer-name pages-buffer)))
      (progn
	(setq pages-buffer nil
	      pages-pos-list nil)
	(error "Buffer in which pages were found is deleted")))
  (beginning-of-line)
  (let* ((pages-number (1- (count-lines (point-min) (point))))
	 (pos (nth pages-number pages-pos-list))
         (end-of-directory-p (eobp))
         (narrowing-p  pages-directory-buffer-narrowing-p))
    (pop-to-buffer pages-buffer)
    (widen)
    (if end-of-directory-p
        (goto-char (point-max))
      (goto-char (marker-position pos)))
    (if narrowing-p (narrow-to-page))))

(defun pages-directory-goto-with-mouse  (event)
  "Go to the corresponding line under the mouse pointer in the pages buffer."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (pages-directory-goto))))

;;; The `pages-directory-for-addresses' function and ancillary code

(defun pages-directory-for-addresses (&optional filename)
  "Find addresses file and display its directory.
By default, create and display directory of `pages-addresses-file-name'.
Optional argument is FILENAME.  In interactive use, with prefix
argument, prompt for file name and provide completion.

Move point to one of the lines in the displayed directory,
then use \\[pages-directory-goto] to go to the same line
in the addresses buffer.

If    pages-directory-for-addresses-goto-narrowing-p    is non-nil,
`pages-directory-goto' narrows addresses buffer to entry.

If    pages-directory-for-addresses-buffer-keep-windows-p     is nil,
this command deletes other windows when it displays the addresses
directory."

  (interactive
   (list (if current-prefix-arg
             (read-file-name "Filename: " pages-addresses-file-name))))

  (if (called-interactively-p 'interactive)
      (message "Creating directory for: %s "
               (or filename pages-addresses-file-name)))
  (if (file-exists-p (or filename pages-addresses-file-name))
      (progn
        (set-buffer
         (find-file-noselect
          (expand-file-name
           (or filename pages-addresses-file-name))))
        (widen)
        (pages-directory t nil nil)
        ;; by RJC, 2006 Jun 11: including this causes failure; it results in
        ;;  the message "Buffer in which pages were found is deleted"
        ;;        (pages-directory-address-mode)
        (setq pages-directory-buffer-narrowing-p
              pages-directory-for-addresses-goto-narrowing-p)
        (or pages-directory-for-addresses-buffer-keep-windows-p
            (delete-other-windows))
        (save-excursion
          (goto-char (point-min))
          (delete-region (point) (line-end-position))
          (insert
           "=== Address List Directory: use `C-c C-c' to go to page under cursor. ===")
          (set-buffer-modified-p nil)
          ))
    (error "No addresses file found!")))

(define-derived-mode pages-directory-address-mode pages-directory-mode
  "Addresses Directory"
  "Mode for handling the Addresses Directory buffer.

Move point to one of the lines in this buffer,
then use \\[pages-directory-goto] to go
to the same line in the pages buffer."
  :syntax-table nil)

(provide 'page-ext)

;;; page-ext.el ends here
