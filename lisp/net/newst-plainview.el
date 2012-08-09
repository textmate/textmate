;;; newst-plainview.el --- Single buffer frontend for newsticker.

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Filename:    newst-plainview.el
;; URL:         http://www.nongnu.org/newsticker
;; Time-stamp:  "13. Mai 2011, 19:28:34 (ulf)"
;; Package:     newsticker

;; ======================================================================

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

;; ======================================================================
;;; Commentary:

;; See newsticker.el

;; ======================================================================
;;; Code:

(require 'newst-ticker)
(require 'newst-reader)
(require 'derived)
(require 'xml)

;; Silence warnings
(defvar w3-mode-map)
(defvar w3m-minor-mode-map)

;; ======================================================================
;;; Customization
;; ======================================================================
(defgroup newsticker-plainview nil
  "Settings for the simple plain view reader.
See also `newsticker-plainview-hooks'."
  :group 'newsticker-reader)


(defun newsticker--set-customvar-buffer (symbol value)
  "Set newsticker-variable SYMBOL value to VALUE.
Calls all actions which are necessary in order to make the new
value effective."
  (if (or (not (boundp symbol))
          (equal (symbol-value symbol) value))
      (set symbol value)
    ;; something must have changed
    (set symbol value)
    (newsticker--buffer-set-uptodate nil)))

(defun newsticker--set-customvar-sorting (symbol value)
  "Set newsticker-variable SYMBOL value to VALUE.
Calls all actions which are necessary in order to make the new
value effective."
  (if (or (not (boundp symbol))
          (equal (symbol-value symbol) value))
      (set symbol value)
    ;; something must have changed
    (set symbol value)
    (message "Applying new sort method...")
    (when (fboundp 'newsticker--cache-sort) (newsticker--cache-sort))
    (when (fboundp 'newsticker--buffer-set-uptodate)
      (newsticker--buffer-set-uptodate nil))
    (message "Applying new sort method...done")))

(defcustom newsticker-sort-method
  'sort-by-original-order
  "Sort method for news items.
The following sort methods are available:
* `sort-by-original-order' keeps the order in which the items
  appear in the headline file (please note that for immortal items,
  which have been removed from the news feed, there is no original
  order),
* `sort-by-time' looks at the time at which an item has been seen
  the first time.  The most recent item is put at top,
* `sort-by-title' will put the items in an alphabetical order."
  :type '(choice
          (const :tag "Keep original order" sort-by-original-order)
          (const :tag "Sort by time"        sort-by-time)
          (const :tag "Sort by title"       sort-by-title))
  :set 'newsticker--set-customvar-sorting
  :group 'newsticker-plainview)

(defcustom newsticker-heading-format
  "%l
%t %d %s"
  "Format string for feed headings.
The following printf-like specifiers can be used:
%d  The date the feed was retrieved.  See `newsticker-date-format'.
%l  The logo (image) of the feed.  Most news feeds provide a small
    image as logo.  Newsticker can display them, if Emacs can --
    see `image-types' for a list of supported image types.
%L  The logo (image) of the feed.  If the logo is not available
    the title of the feed is used.
%s  The statistical data of the feed.  See `newsticker-statistics-format'.
%t  The title of the feed, i.e. its name."
  :type 'string
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-plainview)

(defcustom newsticker-item-format
  "%t %d"
  "Format string for news item headlines.
The following printf-like specifiers can be used:
%d  The date the item was (first) retrieved.  See `newsticker-date-format'.
%l  The logo (image) of the feed.  Most news feeds provide a small
    image as logo.  Newsticker can display them, if Emacs can --
    see `image-types' for a list of supported image types.
%L  The logo (image) of the feed.  If the logo is not available
    the title of the feed is used.
%t  The title of the item."
  :type 'string
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-plainview)

(defcustom newsticker-desc-format
  "%d %c"
  "Format string for news descriptions (contents).
The following printf-like specifiers can be used:
%c  The contents (description) of the item.
%d  The date the item was (first) retrieved.  See
    `newsticker-date-format'."
  :type 'string
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-plainview)

(defcustom newsticker-statistics-format
  "[%n + %i + %o + %O = %a]"
  "Format for the statistics part in feed lines.
The following printf-like specifiers can be used:
%a  The number of all items in the feed.
%i  The number of immortal items in the feed.
%n  The number of new items in the feed.
%o  The number of old items in the feed.
%O  The number of obsolete items in the feed."
  :type 'string
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-plainview)


;; ======================================================================
;; faces

(defface newsticker-new-item-face
  '((((class color) (background dark))
     (:family "sans" :bold t))
    (((class color) (background light))
     (:family "sans" :bold t)))
  "Face for new news items."
  :group 'newsticker-faces)

(defface newsticker-old-item-face
  '((((class color) (background dark))
     (:family "sans" :bold t :foreground "orange3"))
    (((class color) (background light))
     (:family "sans" :bold t :foreground "red4")))
  "Face for old news items."
  :group 'newsticker-faces)

(defface newsticker-immortal-item-face
  '((((class color) (background dark))
     (:family "sans" :bold t :italic t :foreground "orange"))
    (((class color) (background light))
     (:family "sans" :bold t :italic t :foreground "blue")))
  "Face for immortal news items."
  :group 'newsticker-faces)

(defface newsticker-obsolete-item-face
  '((((class color) (background dark))
     (:family "sans" :bold t :strike-through t))
    (((class color) (background light))
     (:family "sans" :bold t :strike-through t)))
  "Face for old news items."
  :group 'newsticker-faces)

(defface newsticker-date-face
  '((((class color) (background dark))
     (:family "sans" :italic t :height 0.8))
    (((class color) (background light))
     (:family "sans" :italic t :height 0.8)))
  "Face for newsticker dates."
  :group 'newsticker-faces)

(defface newsticker-statistics-face
  '((((class color) (background dark))
     (:family "sans" :italic t :height 0.8))
    (((class color) (background light))
     (:family "sans" :italic t :height 0.8)))
  "Face for newsticker dates."
  :group 'newsticker-faces)

(defface newsticker-default-face
  '((((class color) (background dark))
     (:inherit default))
    (((class color) (background light))
     (:inherit default)))
  "Face for the description of news items."
  ;;:set 'newsticker--set-customvar
  :group 'newsticker-faces)

(defcustom newsticker-hide-old-items-in-newsticker-buffer
  nil
  "Decides whether to automatically hide old items in the *newsticker* buffer.
If set to t old items will be completely folded and only new
items will show up in the *newsticker* buffer.  Otherwise old as
well as new items will be visible."
  :type 'boolean
  :set 'newsticker--set-customvar-buffer
  :group 'newsticker-plainview)

(defcustom newsticker-show-descriptions-of-new-items
  t
  "Whether to automatically show descriptions of new items in *newsticker*.
If set to t old items will be folded and new items will be
unfolded.  Otherwise old as well as new items will be folded."
  :type 'boolean
  :set 'newsticker--set-customvar-buffer
  :group 'newsticker-plainview)

(defcustom newsticker-show-all-news-elements
  nil
  "Show all news elements."
  :type 'boolean
  ;;:set 'newsticker--set-customvar
  :group 'newsticker-plainview)

;; ======================================================================
;; hooks
(defgroup newsticker-plainview-hooks nil
  "Settings for newsticker hooks which apply to plainview only."
  :group 'newsticker-hooks)

(defcustom newsticker-select-item-hook
  'newsticker--buffer-make-item-completely-visible
  "List of functions run after a headline has been selected.
Each function is called after one of `newsticker-next-item',
`newsticker-next-new-item', `newsticker-previous-item',
`newsticker-previous-new-item' has been called.

The default value 'newsticker--buffer-make-item-completely-visible
assures that the current item is always completely visible."
  :type 'hook
  :options '(newsticker--buffer-make-item-completely-visible)
  :group 'newsticker-plainview-hooks)

(defcustom newsticker-select-feed-hook
  'newsticker--buffer-make-item-completely-visible
  "List of functions run after a feed has been selected.
Each function is called after one of `newsticker-next-feed', and
`newsticker-previous-feed' has been called.

The default value 'newsticker--buffer-make-item-completely-visible
assures that the current feed is completely visible."
  :type 'hook
  :options '(newsticker--buffer-make-item-completely-visible)
  :group 'newsticker-plainview-hooks)

(defcustom newsticker-buffer-change-hook
  'newsticker-w3m-show-inline-images
  "List of functions run after the newsticker buffer has been updated.
Each function is called after `newsticker-buffer-update' has been called.

The default value '`newsticker-w3m-show-inline-images' loads inline
images."
  :type 'hook
  :group 'newsticker-plainview-hooks)

(defcustom newsticker-narrow-hook
  'newsticker-w3m-show-inline-images
  "List of functions run after narrowing in newsticker buffer has changed.
Each function is called after
`newsticker-toggle-auto-narrow-to-feed' or
`newsticker-toggle-auto-narrow-to-item' has been called.

The default value '`newsticker-w3m-show-inline-images' loads inline
images."
  :type 'hook
  :group 'newsticker-plainview-hooks)

;; ======================================================================
;;; Toolbar
;; ======================================================================

(defvar newsticker--plainview-tool-bar-map
  (if (featurep 'xemacs)
      nil
    (if (boundp 'tool-bar-map)
        (let ((tool-bar-map (make-sparse-keymap)))
          (tool-bar-add-item "newsticker/prev-feed"
                             'newsticker-previous-feed
                             'newsticker-previous-feed
                             :help "Go to previous feed"
                             :enable '(newsticker-previous-feed-available-p))
          (tool-bar-add-item "newsticker/prev-item"
                             'newsticker-previous-item
                             'newsticker-previous-item
                             :help "Go to previous item"
                             :enable '(newsticker-previous-item-available-p))
          (tool-bar-add-item "newsticker/next-item"
                             'newsticker-next-item
                             'newsticker-next-item
                             :help "Go to next item"
                             :enable '(newsticker-next-item-available-p))
          (tool-bar-add-item "newsticker/next-feed"
                             'newsticker-next-feed
                             'newsticker-next-feed
                             :help "Go to next feed"
                             :enable '(newsticker-next-feed-available-p))
          (tool-bar-add-item "newsticker/narrow"
                             'newsticker-toggle-auto-narrow-to-feed
                             'newsticker-toggle-auto-narrow-to-feed
                             :help "Toggle visibility of other feeds")
          (tool-bar-add-item "newsticker/mark-immortal"
                             'newsticker-mark-item-at-point-as-immortal
                             'newsticker-mark-item-at-point-as-immortal
                             :help "Mark current item as immortal"
                             :enable '(newsticker-item-not-immortal-p))
          (tool-bar-add-item "newsticker/mark-read"
                             'newsticker-mark-item-at-point-as-read
                             'newsticker-mark-item-at-point-as-read
                             :help "Mark current item as read"
                             :enable '(newsticker-item-not-old-p))
          (tool-bar-add-item "newsticker/get-all-news"
                             'newsticker-get-all-news
                             'newsticker-get-all-news
                             :help "Get news for all feeds")
          (tool-bar-add-item "newsticker/update"
                             'newsticker-buffer-force-update
                             'newsticker-buffer-force-update
                             :help "Update newsticker buffer"
                             :enable '(not newsticker--buffer-uptodate-p))
          (tool-bar-add-item "newsticker/browse-url"
                             'newsticker-browse-url
                             'newsticker-browse-url
                             :help "Browse URL for item at point")
          ;; standard icons / actions
          (define-key tool-bar-map [newsticker-sep-1]
            (list 'menu-item "--double-line"))
          (tool-bar-add-item "close"
                             'newsticker-close-buffer
                             'newsticker-close-buffer
                             :help "Close newsticker buffer")
          (tool-bar-add-item "preferences"
                             'newsticker-customize
                             'newsticker-customize
                             :help "Customize newsticker")
          tool-bar-map))))

;; ======================================================================
;;; Newsticker mode
;; ======================================================================


;; newsticker menu
(defvar newsticker-menu
  (let ((map (make-sparse-keymap "Newsticker")))

    (define-key map [newsticker-browse-url]
      '("Browse URL for item at point" . newsticker-browse-url))
    (define-key map [newsticker-separator-1]
      '("--"))
    (define-key map [newsticker-buffer-update]
      '("Update buffer" . newsticker-buffer-update))
    (define-key map [newsticker-separator-2]
      '("--"))
    (define-key map [newsticker-get-all-news]
      '("Get news from all feeds" . newsticker-get-all-news))
    (define-key map [newsticker-get-news-at-point]
      '("Get news from feed at point" . newsticker-get-news-at-point))
    (define-key map [newsticker-separator-3]
      '("--"))
    (define-key map [newsticker-mark-all-items-as-read]
      '("Mark all items as read" . newsticker-mark-all-items-as-read))
    (define-key map [newsticker-mark-all-items-at-point-as-read]
      '("Mark all items in feed at point as read" .
        newsticker-mark-all-items-at-point-as-read))
    (define-key map [newsticker-mark-item-at-point-as-read]
      '("Mark item at point as read" .
        newsticker-mark-item-at-point-as-read))
    (define-key map [newsticker-mark-item-at-point-as-immortal]
      '("Toggle immortality for item at point" .
        newsticker-mark-item-at-point-as-immortal))
    (define-key map [newsticker-separator-4]
      '("--"))
    (define-key map [newsticker-toggle-auto-narrow-to-item]
      '("Narrow to single item" . newsticker-toggle-auto-narrow-to-item))
    (define-key map [newsticker-toggle-auto-narrow-to-feed]
      '("Narrow to single news feed" . newsticker-toggle-auto-narrow-to-feed))
    (define-key map [newsticker-hide-old-items]
      '("Hide old items" . newsticker-hide-old-items))
    (define-key map [newsticker-show-old-items]
      '("Show old items" . newsticker-show-old-items))
    (define-key map [newsticker-next-item]
      '("Go to next item" . newsticker-next-item))
    (define-key map [newsticker-previous-item]
      '("Go to previous item" . newsticker-previous-item))
    map))

(defvar newsticker-mode-map
  (let ((map (make-keymap)))
    (define-key map "sO" 'newsticker-show-old-items)
    (define-key map "hO" 'newsticker-hide-old-items)
    (define-key map "sa" 'newsticker-show-all-desc)
    (define-key map "ha" 'newsticker-hide-all-desc)
    (define-key map "sf" 'newsticker-show-feed-desc)
    (define-key map "hf" 'newsticker-hide-feed-desc)
    (define-key map "so" 'newsticker-show-old-item-desc)
    (define-key map "ho" 'newsticker-hide-old-item-desc)
    (define-key map "sn" 'newsticker-show-new-item-desc)
    (define-key map "hn" 'newsticker-hide-new-item-desc)
    (define-key map "se" 'newsticker-show-entry)
    (define-key map "he" 'newsticker-hide-entry)
    (define-key map "sx" 'newsticker-show-extra)
    (define-key map "hx" 'newsticker-hide-extra)

    (define-key map " "  'scroll-up-command)
    (define-key map "q"  'newsticker-close-buffer)
    (define-key map "p"  'newsticker-previous-item)
    (define-key map "P"  'newsticker-previous-new-item)
    (define-key map "F"  'newsticker-previous-feed)
    (define-key map "\t" 'newsticker-next-item)
    (define-key map "n"  'newsticker-next-item)
    (define-key map "N"  'newsticker-next-new-item)
    (define-key map "f"  'newsticker-next-feed)
    (define-key map "M"  'newsticker-mark-all-items-as-read)
    (define-key map "m"
      'newsticker-mark-all-items-at-point-as-read-and-redraw)
    (define-key map "o"
      'newsticker-mark-item-at-point-as-read)
    (define-key map "O"
      'newsticker-mark-all-items-at-point-as-read)
    (define-key map "G"  'newsticker-get-all-news)
    (define-key map "g"  'newsticker-get-news-at-point)
    (define-key map "u"  'newsticker-buffer-update)
    (define-key map "U"  'newsticker-buffer-force-update)
    (define-key map "a"  'newsticker-add-url)

    (define-key map "i"
      'newsticker-mark-item-at-point-as-immortal)

    (define-key map "xf"
      'newsticker-toggle-auto-narrow-to-feed)
    (define-key map "xi"
      'newsticker-toggle-auto-narrow-to-item)

    ;; Bind menu to mouse.
    (define-key map [down-mouse-3] newsticker-menu)
    ;; Put menu in menu-bar.
    (define-key map [menu-bar Newsticker]
      (cons "Newsticker" newsticker-menu))

    map))

(define-derived-mode newsticker-mode fundamental-mode
  "NewsTicker"
  "Viewing news feeds in Emacs."
  (if (boundp 'tool-bar-map)
      (set (make-local-variable 'tool-bar-map)
           newsticker--plainview-tool-bar-map))
  (set (make-local-variable 'imenu-sort-function) nil)
  (set (make-local-variable 'scroll-conservatively) 999)
  (setq imenu-create-index-function 'newsticker--imenu-create-index)
  (setq imenu-default-goto-function 'newsticker--imenu-goto)
  (setq buffer-read-only t)
  (auto-fill-mode -1) ;; turn auto-fill off!
  (font-lock-mode -1) ;; turn off font-lock!!
  (set (make-local-variable 'font-lock-defaults) nil)
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (setq mode-line-format
        (list "-"
              'mode-line-mule-info
              'mode-line-modified
              'mode-line-frame-identification
              " Newsticker ("
              '(newsticker--buffer-uptodate-p
                "up to date"
                "NEED UPDATE")
              ") "
              '(:eval (format "[%d]" (length newsticker--process-ids)))
              " -- "
              '(:eval (newsticker--buffer-get-feed-title-at-point))
              ": "
              '(:eval (newsticker--buffer-get-item-title-at-point))
              " %-"))
  (add-to-invisibility-spec 't)
  (unless newsticker-show-all-news-elements
    (add-to-invisibility-spec 'extra))
  (newsticker--buffer-set-uptodate nil))

;; maps for the clickable portions
(defvar newsticker--url-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'newsticker-mouse-browse-url)
    (define-key map [mouse-2] 'newsticker-mouse-browse-url)
    (define-key map "\n" 'newsticker-browse-url)
    (define-key map "\C-m" 'newsticker-browse-url)
    (define-key map [(control return)] 'newsticker-handle-url)
    map)
  "Key map for click-able headings in the newsticker buffer.")


;; ======================================================================
;;; User fun
;; ======================================================================
;;;###autoload
(defun newsticker-plainview ()
  "Start newsticker plainview."
  (interactive)
  (newsticker-buffer-update t)
  (switch-to-buffer "*newsticker*"))

(defun newsticker-buffer-force-update ()
  "Update the newsticker buffer, even if not necessary."
  (interactive)
  (newsticker-buffer-update t))

(defun newsticker-buffer-update (&optional force)
  "Update the *newsticker* buffer.
Unless FORCE is t this is done only if necessary, i.e. when the
*newsticker* buffer is not up-to-date."
  (interactive)
  ;; bring cache data into proper order....
  (newsticker--cache-sort)
  ;; fill buffer
  (save-excursion
    (let ((buf (get-buffer "*newsticker*")))
      (if buf
          (switch-to-buffer buf)
        (switch-to-buffer (get-buffer-create "*newsticker*"))
        (newsticker--buffer-set-uptodate nil)))
   (when (or force
             (not newsticker--buffer-uptodate-p))
     (message "Preparing newsticker buffer...")
     (setq buffer-undo-list t)
     (let ((inhibit-read-only t))
       (set-buffer-modified-p nil)
       (erase-buffer)
       (newsticker-mode)
       ;; Emacs 21.3.50 does not care if we turn off auto-fill in the
       ;; definition of newsticker-mode, so we do it here (again)
       (auto-fill-mode -1)

       (set-buffer-file-coding-system 'utf-8)

       (if newsticker-use-full-width
           (set (make-local-variable 'fill-column) (1- (window-width))))
       (newsticker--buffer-insert-all-items)

       ;; FIXME: needed for methods buffer in ecb
       ;; (set-visited-file-name "*newsticker*")

       (set-buffer-modified-p nil)
        (newsticker-hide-all-desc)
        (if newsticker-hide-old-items-in-newsticker-buffer
            (newsticker-hide-old-items))
        (if newsticker-show-descriptions-of-new-items
            (newsticker-show-new-item-desc))
       )
     (message ""))
   (newsticker--buffer-set-uptodate t)
   (run-hooks 'newsticker-buffer-change-hook)))

(defun newsticker-get-news-at-point ()
  "Launch retrieval of news for the feed point is in.
This does NOT start the retrieval timers."
  (interactive)
  ;; launch retrieval of news
  (let ((feed (get-text-property (point) 'feed)))
      (when feed
        (newsticker--debug-msg "Getting news for %s" (symbol-name feed))
        (newsticker-get-news (symbol-name feed)))))

(unless (fboundp 'declare-function) (defmacro declare-function (&rest r)))
(declare-function w3m-toggle-inline-image "ext:w3m" (&optional force no-cache))

(defun newsticker-w3m-show-inline-images ()
  "Show inline images in visible text ranges.
In-line images in invisible text ranges are hidden.  This function
calls `w3m-toggle-inline-image'.  It works only if
`newsticker-html-renderer' is set to `w3m-region'."
  (interactive)
  (if (eq newsticker-html-renderer 'w3m-region)
      (let ((inhibit-read-only t))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (let ((pos (point)))
              (while pos
                (setq pos (next-single-property-change pos 'w3m-image))
                (when pos
                  (goto-char pos)
                  (when (get-text-property pos 'w3m-image)
                    (let ((invis (newsticker--lists-intersect-p
                                  (get-text-property (1- (point))
                                                     'invisible)
                                  buffer-invisibility-spec)))
                      (unless  (car (get-text-property (1- (point))
                                                       'display))
                        (unless invis
                          (w3m-toggle-inline-image t)))))))))))))

;; ======================================================================
;;; Keymap stuff
;; ======================================================================
(defun newsticker-close-buffer ()
  "Close the newsticker buffer."
  (interactive)
  (newsticker--cache-update t)
  (bury-buffer))

(defun newsticker-next-new-item (&optional do-not-wrap-at-eob)
  "Go to next new news item.
If no new item is found behind point, search is continued at
beginning of buffer unless optional argument DO-NOT-WRAP-AT-EOB
is non-nil."
  (interactive)
  (widen)
  (let ((go-ahead t))
    (while go-ahead
      (unless (newsticker--buffer-goto '(item) 'new)
	;; found nothing -- wrap
	(unless do-not-wrap-at-eob
	  (goto-char (point-min))
	  (newsticker-next-new-item t))
	(setq go-ahead nil))
      (unless (newsticker--lists-intersect-p
               (get-text-property (point) 'invisible)
               buffer-invisibility-spec)
	;; this item is invisible -- continue search
        (setq go-ahead nil))))
  (run-hooks 'newsticker-select-item-hook)
  (point))

(defun newsticker-previous-new-item (&optional do-not-wrap-at-bob)
  "Go to previous new news item.
If no new item is found before point, search is continued at
beginning of buffer unless optional argument DO-NOT-WRAP-AT-BOB
is non-nil."
  (interactive)
  (widen)
  (let ((go-ahead t))
    (while go-ahead
      (unless (newsticker--buffer-goto '(item) 'new t)
	(unless do-not-wrap-at-bob
	  (goto-char (point-max))
	  (newsticker--buffer-goto '(item) 'new t)))
      (unless (newsticker--lists-intersect-p
               (get-text-property (point) 'invisible)
		    buffer-invisibility-spec)
	(setq go-ahead nil))))
  (run-hooks 'newsticker-select-item-hook)
  (point))

(defun newsticker-next-item (&optional do-not-wrap-at-eob)
  "Go to next news item.
Return new buffer position.
If no item is found below point, search is continued at beginning
of buffer unless optional argument DO-NOT-WRAP-AT-EOB is
non-nil."
  (interactive)
  (widen)
  (let ((go-ahead t)
        (search-list '(item)))
    (if newsticker--auto-narrow-to-item
        (setq search-list '(item feed)))
    (while go-ahead
      (unless (newsticker--buffer-goto search-list)
	;; found nothing -- wrap
	(unless do-not-wrap-at-eob
	  (goto-char (point-min)))
	(setq go-ahead nil))
      (unless (newsticker--lists-intersect-p
               (get-text-property (point) 'invisible)
		    buffer-invisibility-spec)
	(setq go-ahead nil))))
  (run-hooks 'newsticker-select-item-hook)
  (force-mode-line-update)
  (point))

(defun newsticker-next-item-same-feed ()
  "Go to next news item in the same feed.
Return new buffer position.  If no item is found below point or if
auto-narrow-to-item is enabled, nil is returned."
  (interactive)
  (if newsticker--auto-narrow-to-item
      nil
    (let ((go-ahead t)
          (current-pos (point))
          (end-of-feed (save-excursion (newsticker--buffer-end-of-feed))))
      (while go-ahead
        (unless (newsticker--buffer-goto '(item))
          (setq go-ahead nil))
        (unless (newsticker--lists-intersect-p
                 (get-text-property (point) 'invisible)
                 buffer-invisibility-spec)
          (setq go-ahead nil)))
      (if (and (> (point) current-pos)
               (< (point) end-of-feed))
          (point)
        (goto-char current-pos)
        nil))))

(defun newsticker-previous-item (&optional do-not-wrap-at-bob)
  "Go to previous news item.
Return new buffer position.
If no item is found before point, search is continued at
beginning of buffer unless optional argument DO-NOT-WRAP-AT-BOB
is non-nil."
  (interactive)
  (widen)
  (let ((go-ahead t)
        (search-list '(item)))
    (if newsticker--auto-narrow-to-item
        (setq search-list '(item feed)))
    (when (bobp)
      (unless do-not-wrap-at-bob
	(goto-char (point-max))))
    (while go-ahead
      (if (newsticker--buffer-goto search-list nil t)
          (unless (newsticker--lists-intersect-p
                   (get-text-property (point) 'invisible)
                   buffer-invisibility-spec)
            (setq go-ahead nil))
        (goto-char (point-min))
        (setq go-ahead nil))))
  (run-hooks 'newsticker-select-item-hook)
  (force-mode-line-update)
  (point))

(defun newsticker-next-feed ()
  "Go to next news feed.
Return new buffer position."
  (interactive)
  (widen)
  (newsticker--buffer-goto '(feed))
  (run-hooks 'newsticker-select-feed-hook)
  (force-mode-line-update)
  (point))

(defun newsticker-previous-feed ()
  "Go to previous news feed.
Return new buffer position."
  (interactive)
  (widen)
  (newsticker--buffer-goto '(feed) nil t)
  (run-hooks 'newsticker-select-feed-hook)
  (force-mode-line-update)
  (point))

(defun newsticker-mark-all-items-at-point-as-read-and-redraw ()
  "Mark all items as read and clear ticker contents."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark items as read? "))
    (newsticker-mark-all-items-of-feed-as-read
     (get-text-property (point) 'feed))))

(defun newsticker-mark-all-items-of-feed-as-read (feed)
  "Mark all items of FEED as read, clear ticker, and redraw buffer."
  (when feed
    (let ((pos (point)))
      (message "Marking all items as read for %s" (symbol-name feed))
      (newsticker--cache-replace-age newsticker--cache feed 'new 'old)
      (newsticker--cache-replace-age newsticker--cache feed 'obsolete
                                     'old)
      (newsticker--cache-update)
      (newsticker--buffer-set-uptodate nil)
      (newsticker--ticker-text-setup)
      (newsticker-buffer-update)
      ;; Go back to where we came from.
      (goto-char pos)
      (end-of-line)
      (newsticker--buffer-goto '(feed) nil t))))

(defun newsticker-mark-all-items-at-point-as-read ()
  "Mark all items as read and clear ticker contents."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark items as read? "))
    (newsticker--do-mark-item-at-point-as-read t)
    (while (newsticker-next-item-same-feed)
      (newsticker--do-mark-item-at-point-as-read t))
    (newsticker-next-item t)))

(defun newsticker-mark-item-at-point-as-read (&optional respect-immortality)
  "Mark item at point as read and move to next item.
If optional argument RESPECT-IMMORTALITY is not nil immortal items do
not get changed."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark this item as read? "))
    (newsticker--do-mark-item-at-point-as-read respect-immortality)
    ;; move forward
    (newsticker-next-item t)))

(defun newsticker--do-mark-item-at-point-as-read (&optional respect-immortality)
  "Mark item at point as read.
If optional argument RESPECT-IMMORTALITY is not nil immortal items do
not get changed."
  (let ((feed (get-text-property (point) 'feed)))
    (when feed
      (save-excursion
        (newsticker--buffer-beginning-of-item)
        (let ((inhibit-read-only t)
              (age (get-text-property (point) 'nt-age))
              (title (get-text-property (point) 'nt-title))
              (guid (get-text-property (point) 'nt-guid))
              (nt-desc (get-text-property (point) 'nt-desc))
              (pos (save-excursion (newsticker--buffer-end-of-item)))
              item)
          (when (or (eq age 'new)
                    (eq age 'obsolete)
                    (and (eq age 'immortal)
                         (not respect-immortality)))
            ;; find item
            (setq item (newsticker--cache-contains newsticker--cache
                                                   feed title nt-desc
                                                   nil nil guid))
            ;; mark as old
            (when item
              (setcar (nthcdr 4 item) 'old)
              (newsticker--do-forget-preformatted item))
            ;; clean up ticker
            (if (or (and (eq age 'new)
                         newsticker-hide-immortal-items-in-echo-area)
                    (and (memq age '(old immortal))
                         (not
                          (eq newsticker-hide-old-items-in-newsticker-buffer
                              newsticker-hide-immortal-items-in-echo-area))))
                (newsticker--ticker-text-remove feed title))
            ;; set faces etc.
            (save-excursion
              (save-restriction
                (widen)
                (put-text-property (point) pos 'nt-age 'old)
                (newsticker--buffer-set-faces (point) pos)))
            (set-buffer-modified-p nil)))))))

(defun newsticker-mark-item-at-point-as-immortal ()
  "Mark item at point as read."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark this item as read? "))
    (let ((feed (get-text-property (point) 'feed))
          (item nil))
      (when feed
        (save-excursion
          (newsticker--buffer-beginning-of-item)
          (let ((inhibit-read-only t)
                (oldage (get-text-property (point) 'nt-age))
                (title (get-text-property (point) 'nt-title))
                (guid (get-text-property (point) 'nt-guid))
                (pos  (save-excursion (newsticker--buffer-end-of-item))))
            (let ((newage 'immortal))
              (if (eq oldage 'immortal)
                  (setq newage 'old))
              (setq item (newsticker--cache-contains newsticker--cache
                                                     feed title nil nil nil
                                                     guid))
              ;; change age
              (when item
                (setcar (nthcdr 4 item) newage)
                (newsticker--do-forget-preformatted item))
              (if (or (and (eq newage 'immortal)
                           newsticker-hide-immortal-items-in-echo-area)
                      (and (eq newage 'obsolete)
                           newsticker-hide-obsolete-items-in-echo-area)
                      (and (eq oldage 'immortal)
                           (not
                            (eq newsticker-hide-old-items-in-newsticker-buffer
                                newsticker-hide-immortal-items-in-echo-area))))
                  (newsticker--ticker-text-remove feed title)
                (newsticker--ticker-text-setup))
              (save-excursion
                (save-restriction
                  (widen)
                  (put-text-property (point) pos 'nt-age newage)
                  (if (eq newage 'immortal)
                      (put-text-property (point) pos 'nt-age 'immortal)
                    (put-text-property (point) pos 'nt-age 'old))
                  (newsticker--buffer-set-faces (point) pos))))))
          (if item
            (newsticker-next-item t))))))

(defun newsticker-mark-all-items-as-read ()
  "Mark all items as read and clear ticker contents."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark items as read? "))
    (newsticker--cache-replace-age newsticker--cache 'any 'new 'old)
    (newsticker--buffer-set-uptodate nil)
    (newsticker--ticker-text-setup)
    (newsticker--cache-update)
    (newsticker-buffer-update)))

(defun newsticker-hide-extra ()
  "Hide the extra elements of items."
  (interactive)
  (newsticker--buffer-hideshow 'extra nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-extra ()
  "Show the extra elements of items."
  (interactive)
  (newsticker--buffer-hideshow 'extra t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-old-item-desc ()
  "Hide the description of old items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-old nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-old-item-desc ()
  "Show the description of old items."
  (interactive)
  (newsticker--buffer-hideshow 'item-old t)
  (newsticker--buffer-hideshow 'desc-old t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-new-item-desc ()
  "Hide the description of new items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-new nil)
  (newsticker--buffer-hideshow 'desc-immortal nil)
  (newsticker--buffer-hideshow 'desc-obsolete nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-new-item-desc ()
  "Show the description of new items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-new t)
  (newsticker--buffer-hideshow 'desc-immortal t)
  (newsticker--buffer-hideshow 'desc-obsolete t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-feed-desc ()
  "Hide the description of feeds."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-feed-desc ()
  "Show the description of old items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-all-desc ()
  "Hide the descriptions of feeds and all items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed nil)
  (newsticker--buffer-hideshow 'desc-immortal nil)
  (newsticker--buffer-hideshow 'desc-obsolete nil)
  (newsticker--buffer-hideshow 'desc-new  nil)
  (newsticker--buffer-hideshow 'desc-old  nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-all-desc ()
  "Show the descriptions of feeds and all items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed t)
  (newsticker--buffer-hideshow 'desc-immortal  t)
  (newsticker--buffer-hideshow 'desc-obsolete  t)
  (newsticker--buffer-hideshow 'desc-new  t)
  (newsticker--buffer-hideshow 'desc-old  t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-old-items ()
  "Hide old items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-old nil)
  (newsticker--buffer-hideshow 'item-old nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-old-items ()
  "Show old items."
  (interactive)
  (newsticker--buffer-hideshow 'item-old t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-entry ()
  "Hide description of entry at point."
  (interactive)
  (save-excursion
    (let* (pos1 pos2
                (inhibit-read-only t)
                inv-prop org-inv-prop
                is-invisible)
      (newsticker--buffer-beginning-of-item)
      (newsticker--buffer-goto '(desc))
      (setq pos1 (max (point-min) (1- (point))))
      (newsticker--buffer-goto '(extra feed item nil))
      (setq pos2 (max (point-min) (1- (point))))
      (setq inv-prop (get-text-property pos1 'invisible))
      (setq org-inv-prop (get-text-property pos1 'org-invisible))
      (cond ((eq inv-prop t)
             ;; do nothing
             )
            ((eq org-inv-prop nil)
             (add-text-properties pos1 pos2
                                  (list 'invisible (list t)
                                        'org-invisible inv-prop)))
            (t
             ;; toggle
             (add-text-properties pos1 pos2
                                  (list 'invisible org-inv-prop))
             (remove-text-properties pos1 pos2 '(org-invisible))))))
  (newsticker--buffer-redraw))

(defun newsticker-show-entry ()
  "Show description of entry at point."
  (interactive)
  (save-excursion
    (let* (pos1 pos2
                (inhibit-read-only t)
                inv-prop org-inv-prop
                is-invisible)
      (newsticker--buffer-beginning-of-item)
      (newsticker--buffer-goto '(desc))
      (setq pos1 (max (point-min) (1- (point))))
      (newsticker--buffer-goto '(extra feed item))
      (setq pos2 (max (point-min) (1- (point))))
      (setq inv-prop (get-text-property pos1 'invisible))
      (setq org-inv-prop (get-text-property pos1 'org-invisible))
      (cond ((eq org-inv-prop nil)
             (add-text-properties pos1 pos2
                                  (list 'invisible nil
                                        'org-invisible inv-prop)))
            (t
             ;; toggle
             (add-text-properties pos1 pos2
                                  (list 'invisible org-inv-prop))
             (remove-text-properties pos1 pos2 '(org-invisible))))))
  (newsticker--buffer-redraw))

(defun newsticker-toggle-auto-narrow-to-feed ()
  "Toggle narrowing to current news feed.
If auto-narrowing is active, only news item of the current feed
are visible."
  (interactive)
  (newsticker-set-auto-narrow-to-feed
   (not newsticker--auto-narrow-to-feed)))

(defun newsticker-set-auto-narrow-to-feed (value)
  "Turn narrowing to current news feed on or off.
If VALUE is nil, auto-narrowing is turned off, otherwise it is turned on."
  (interactive)
  (setq newsticker--auto-narrow-to-item nil)
  (setq newsticker--auto-narrow-to-feed value)
  (widen)
  (newsticker--buffer-make-item-completely-visible)
  (run-hooks 'newsticker-narrow-hook))

(defun newsticker-toggle-auto-narrow-to-item ()
  "Toggle narrowing to current news item.
If auto-narrowing is active, only one item of the current feed
is visible."
  (interactive)
  (newsticker-set-auto-narrow-to-item
   (not newsticker--auto-narrow-to-item)))

(defun newsticker-set-auto-narrow-to-item (value)
  "Turn narrowing to current news item on or off.
If VALUE is nil, auto-narrowing is turned off, otherwise it is turned on."
  (interactive)
  (setq newsticker--auto-narrow-to-feed nil)
  (setq newsticker--auto-narrow-to-item value)
  (widen)
  (newsticker--buffer-make-item-completely-visible)
  (run-hooks 'newsticker-narrow-hook))

(defun newsticker-next-feed-available-p ()
  "Return t if position is before last feed, nil otherwise."
  (save-excursion
    (let ((p (point)))
      (newsticker--buffer-goto '(feed))
      (not (= p (point))))))

(defun newsticker-previous-feed-available-p ()
  "Return t if position is behind first feed, nil otherwise."
  (save-excursion
    (let ((p (point)))
      (newsticker--buffer-goto '(feed) nil t)
      (not (= p (point))))))

(defun newsticker-next-item-available-p ()
  "Return t if position is before last feed, nil otherwise."
  (save-excursion
    (catch 'result
      (while (< (point) (point-max))
        (unless (newsticker--buffer-goto '(item))
          (throw 'result nil))
        (unless (newsticker--lists-intersect-p
                 (get-text-property (point) 'invisible)
                 buffer-invisibility-spec)
          (throw 'result t))))))

(defun newsticker-previous-item-available-p ()
  "Return t if position is behind first item, nil otherwise."
  (save-excursion
    (catch 'result
      (while (> (point) (point-min))
        (unless (newsticker--buffer-goto '(item) nil t)
          (throw 'result nil))
        (unless (newsticker--lists-intersect-p
                 (get-text-property (point) 'invisible)
                 buffer-invisibility-spec)
          (throw 'result t))))))

(defun newsticker-item-not-old-p ()
  "Return t if there is an item at point which is not old, nil otherwise."
    (when (get-text-property (point) 'feed)
      (save-excursion
        (newsticker--buffer-beginning-of-item)
        (let ((age (get-text-property (point) 'nt-age)))
          (and (memq  age '(new immortal obsolete)) t)))))

(defun newsticker-item-not-immortal-p ()
  "Return t if there is an item at point which is not immortal, nil otherwise."
    (when (get-text-property (point) 'feed)
      (save-excursion
        (newsticker--buffer-beginning-of-item)
        (let ((age (get-text-property (point) 'nt-age)))
          (and (memq  age '(new old obsolete)) t)))))

;; ======================================================================
;;; Imenu stuff
;; ======================================================================
(defun newsticker--imenu-create-index ()
  "Scan newsticker buffer and return an index for imenu."
  (save-excursion
    (goto-char (point-min))
    (let ((index-alist nil)
          (feed-list nil)
          (go-ahead t))
      (while go-ahead
        (let ((type  (get-text-property (point) 'nt-type))
              (title (get-text-property (point) 'nt-title)))
          (cond ((eq type 'feed)
                 ;; we're on a feed heading
                 (when feed-list
                   (if index-alist
                       (nconc index-alist (list feed-list))
                     (setq index-alist (list feed-list))))
                 (setq feed-list (list title)))
                (t
                 (nconc feed-list
                        (list (cons title (point)))))))
	(setq go-ahead (newsticker--buffer-goto '(item feed))))
      (if index-alist
	  (nconc index-alist (list feed-list))
	(setq index-alist (list feed-list)))
      index-alist)))

(defun newsticker--imenu-goto (name pos &rest args)
  "Go to item NAME at position POS and show item.
ARGS are ignored."
  (goto-char pos)
  ;; show headline
  (newsticker--buffer-goto '(desc extra feed item))
  (let* ((inhibit-read-only t)
         (pos1 (max (point-min) (1- pos)))
         (pos2 (max pos1 (1- (point))))
         (inv-prop (get-text-property pos 'invisible))
         (org-inv-prop (get-text-property pos 'org-invisible)))
    (when (eq org-inv-prop nil)
      (add-text-properties pos1 pos2 (list 'invisible nil
                                          'org-invisible inv-prop))))
  ;; show desc
  (newsticker-show-entry))

;; ======================================================================
;;; Buffer stuff
;; ======================================================================
(defun newsticker--buffer-set-uptodate (value)
  "Set the uptodate-status of the newsticker buffer to VALUE.
The mode-line is changed accordingly."
  (setq newsticker--buffer-uptodate-p value)
  (let ((b (get-buffer "*newsticker*")))
    (when b
      (with-current-buffer b
        (setq mode-name (if value
                            "Newsticker -- up to date -- "
                          "Newsticker -- NEED UPDATE -- ")))
      (force-mode-line-update 0))))

(defun newsticker--buffer-redraw ()
  "Redraw the newsticker window."
  (if (fboundp 'force-window-update)
      (force-window-update (current-buffer))
    (redraw-frame (selected-frame)))
  (run-hooks 'newsticker-buffer-change-hook)
  (sit-for 0))

(defun newsticker--buffer-insert-all-items ()
  "Insert all cached newsticker items into the current buffer.
Keeps order of feeds as given in `newsticker-url-list' and
`newsticker-url-list-defaults'."
  (goto-char (point-min))
  (mapc (lambda (url-item)
          (let* ((feed-name (car url-item))
                 (feed-name-symbol (intern feed-name))
                 (feed (assoc feed-name-symbol newsticker--cache))
                 (items (cdr feed))
                 (pos (point)))
            (when feed
              ;; insert the feed description
              (mapc (lambda (item)
                      (when (eq (newsticker--age item) 'feed)
                        (newsticker--buffer-insert-item item
                                                        feed-name-symbol)))
                    items)
              ;;insert the items
              (mapc (lambda (item)
                      (if (memq (newsticker--age item) '(new immortal old
                                                             obsolete))
                          (newsticker--buffer-insert-item item
                                                          feed-name-symbol)))
                    items)
              (put-text-property pos (point) 'feed (car feed))

              ;; insert empty line between feeds
              (let ((p (point)))
                (insert "\n")
                (put-text-property p (point) 'hard t)))))
        (append newsticker-url-list newsticker-url-list-defaults))

  (newsticker--buffer-set-faces (point-min) (point-max))
  (newsticker--buffer-set-invisibility (point-min) (point-max))
  (goto-char (point-min)))

(defun newsticker--buffer-insert-item (item &optional feed-name-symbol)
  "Insert a news item in the current buffer.
Insert a formatted representation of the ITEM.  The optional parameter
FEED-NAME-SYMBOL determines how the item is formatted and whether the
item-retrieval time is added as well."
  ;; insert headline
  (if (eq (newsticker--age item) 'feed)
      (newsticker--buffer-do-insert-text item 'feed feed-name-symbol)
    (newsticker--buffer-do-insert-text item 'item feed-name-symbol))
  ;; insert the description
  (newsticker--buffer-do-insert-text item 'desc feed-name-symbol))

(defun newsticker--buffer-do-insert-text (item type feed-name-symbol)
  "Actually insert contents of news item, format it, render it and all that.
ITEM is a news item, TYPE tells which part of the item shall be inserted,
FEED-NAME-SYMBOL tells to which feed this item belongs."
  (let* ((pos (point))
         (format newsticker-desc-format)
         (pos-date-start nil)
         (pos-date-end nil)
         (pos-stat-start nil)
         (pos-stat-end nil)
         (pos-text-start nil)
         (pos-text-end nil)
         (pos-extra-start nil)
         (pos-extra-end nil)
         (pos-enclosure-start nil)
         (pos-enclosure-end nil)
         (age (newsticker--age item))
         (preformatted-contents (newsticker--preformatted-contents item))
         (preformatted-title (newsticker--preformatted-title item)))
    (cond ((and preformatted-contents
                (not (eq (aref preformatted-contents 0) ?\n));; we must
                                                       ;; NOT have a line
                                                       ;; break!
                (eq type 'desc))
           (insert preformatted-contents))
          ((and preformatted-title
                (not (eq (aref preformatted-title 0) ?\n));; we must NOT have a
                                                    ;; line break!
                (eq type 'item))
           (insert preformatted-title))
          (t
           ;; item was not formatted before.
           ;; Let's go.
           (if (eq type 'item)
               (setq format newsticker-item-format)
             (if (eq type 'feed)
                 (setq format newsticker-heading-format)))

           (while (> (length format) 0)
             (let ((prefix (if (> (length format) 1)
                               (substring format 0 2)
                             "")))
               (cond ((string= "%c" prefix)
                      ;; contents
                      (when (newsticker--desc item)
                        (setq pos-text-start (point-marker))
                        (insert (newsticker--desc item))
                        (setq pos-text-end (point-marker)))
                      (setq format (substring format 2)))
                     ((string= "%d" prefix)
                      ;; date
                      (setq pos-date-start (point-marker))
                      (if (newsticker--time item)
                          (insert (format-time-string newsticker-date-format
                                                      (newsticker--time item))))
                      (setq pos-date-end (point-marker))
                      (setq format (substring format 2)))
                     ((string= "%l" prefix)
                      ;; logo
                      (let ((disabled (cond ((eq (newsticker--age item) 'feed)
                                             (= (newsticker--stat-num-items
                                                 feed-name-symbol 'new) 0))
                                            (t
                                             (not (eq (newsticker--age item)
                                                      'new))))))
                        (let ((img (newsticker--image-read feed-name-symbol
                                                           disabled)))
                          (when img
                            (newsticker--insert-image img (car item)))))
                      (setq format (substring format 2)))
                     ((string= "%L" prefix)
                      ;; logo or title
                      (let ((disabled (cond ((eq (newsticker--age item) 'feed)
                                             (= (newsticker--stat-num-items
                                                 feed-name-symbol 'new) 0))
                                            (t
                                             (not (eq (newsticker--age item)
                                                      'new))))))
                        (let ((img (newsticker--image-read feed-name-symbol
                                                           disabled)))
                          (if img
                              (newsticker--insert-image img (car item))
                            (when (car item)
                              (setq pos-text-start (point-marker))
			      (if (eq (newsticker--age item) 'feed)
				  (insert (newsticker--title item))
				;; FIXME: This is not the "real" title!
				(insert (format "%s"
						(car (newsticker--cache-get-feed
						      feed-name-symbol)))))
                              (setq pos-text-end (point-marker))))))
                      (setq format (substring format 2)))
                     ((string= "%s" prefix)
                      ;; statistics
                      (setq pos-stat-start (point-marker))
                      (if (eq (newsticker--age item) 'feed)
                          (insert (newsticker--buffer-statistics
                                   feed-name-symbol)))
                      (setq pos-stat-end (point-marker))
                      (setq format (substring format 2)))
                     ((string= "%t" prefix)
                      ;; title
                      (when (car item)
                        (setq pos-text-start (point-marker))
                        (insert (car item))
                        (setq pos-text-end (point-marker)))
                      (setq format (substring format 2)))
                     ((string-match "%." prefix)
                      ;; unknown specifier!
                      (insert prefix)
                      (setq format (substring format 2)))
                     ((string-match "^\\([^%]*\\)\\(.*\\)" format) ;; FIXME!
                      ;; everything else
                      (let ((p (point)))
                        (insert (substring format
                                           (match-beginning 1) (match-end 1)))
                        ;; in case that the format string contained newlines
                        (put-text-property p (point) 'hard t))
                      (setq format (substring format (match-beginning 2)))))))

           ;; decode HTML if possible...
           (let ((is-rendered-HTML nil))
             (when (and newsticker-html-renderer pos-text-start pos-text-end)
               (condition-case error-data
                   (save-excursion
                     ;; check whether it is necessary to call html renderer
                     ;; (regexp inspired by htmlr.el)
                     (goto-char pos-text-start)
                     (when (re-search-forward
                            "</?[A-Za-z1-6]*\\|&#?[A-Za-z0-9]+;" pos-text-end t)
                       ;; (message "%s" (newsticker--title item))
                       (let ((w3m-fill-column (if newsticker-use-full-width
                                                  -1 fill-column))
                             (w3-maximum-line-length
                              (if newsticker-use-full-width nil fill-column)))
                         (save-excursion
                           (funcall newsticker-html-renderer pos-text-start
                                    pos-text-end)))
                       (cond ((eq newsticker-html-renderer 'w3m-region)
                              (add-text-properties pos (point-max)
                                                   (list 'keymap
                                                         w3m-minor-mode-map)))
                             ((eq newsticker-html-renderer 'w3-region)
                              (add-text-properties pos (point-max)
                                                   (list 'keymap w3-mode-map))))
                       (setq is-rendered-HTML t)))
                 (error
                  (message "Error: HTML rendering failed: %s, %s"
                           (car error-data) (cdr error-data)))))
             ;; After html rendering there might be chunks of blank
             ;; characters between rendered text and date, statistics or
             ;; whatever.  Remove it
             (when (and (eq type 'item) is-rendered-HTML)
               (goto-char pos)
               (while (re-search-forward "[ \t]*\n[ \t]*" nil t)
                 (replace-match " " nil nil))
               (goto-char (point-max)))
             (when (and newsticker-justification
                        (memq type '(item desc))
                        (not is-rendered-HTML))
               (condition-case nil
                   (let ((use-hard-newlines t))
                     (fill-region pos (point-max) newsticker-justification))
                 (error nil))))

           ;; remove leading and trailing newlines
           (goto-char pos)
           (unless (= 0 (skip-chars-forward " \t\r\n"))
             (delete-region pos (point)))
           (goto-char (point-max))
           (let ((end (point)))
             (unless (= 0 (skip-chars-backward " \t\r\n" (1+ pos)))
               (delete-region (point) end)))
           (goto-char (point-max))
           ;; closing newline
           (unless nil ;;(eq pos (point))
             (insert "\n")
             (put-text-property (1- (point)) (point) 'hard t))

           ;; insert enclosure element
           (when (eq type 'desc)
             (setq pos-enclosure-start (point))
             (newsticker--insert-enclosure item newsticker--url-keymap)
             (setq pos-enclosure-end (point)))

           ;; show extra elements
           (when (eq type 'desc)
             (goto-char (point-max))
             (setq pos-extra-start (point))
             (newsticker--print-extra-elements item newsticker--url-keymap)
             (setq pos-extra-end (point)))

           ;; text properties
           (when (memq type '(feed item))
             (add-text-properties pos (1- (point))
                                  (list 'mouse-face 'highlight
                                        'nt-link (newsticker--link item)
                                        'help-echo
                                        (format "mouse-2: visit item (%s)"
                                                (newsticker--link item))
                                        'keymap newsticker--url-keymap))
             (add-text-properties pos (point)
                                  (list 'nt-title (newsticker--title item)
                                        'nt-desc (newsticker--desc item))))

           (add-text-properties pos (point)
                                (list 'nt-type type
                                      'nt-face type
                                      'nt-age  age
                                      'nt-guid (newsticker--guid item)))
           (when (and pos-date-start pos-date-end)
             (put-text-property pos-date-start pos-date-end 'nt-face 'date))
           (when (and pos-stat-start pos-stat-end)
             (put-text-property pos-stat-start pos-stat-end 'nt-face 'stat))
           (when (and pos-extra-start pos-extra-end)
             (put-text-property pos-extra-start pos-extra-end
                                'nt-face 'extra)
             (put-text-property pos-extra-start pos-extra-end
                                'nt-type 'extra))
           (when (and pos-enclosure-start pos-enclosure-end
                      (> pos-enclosure-end pos-enclosure-start))
             (put-text-property pos-enclosure-start (1- pos-enclosure-end)
                                'nt-face 'enclosure))

           ;; left margin
           ;;(unless (memq type '(feed item))
           ;;(set-left-margin pos (1- (point)) 1))

           ;; save rendered stuff
           (cond ((eq type 'desc)
		  ;; preformatted contents
		  (newsticker--cache-set-preformatted-contents
		   item (buffer-substring pos (point))))
		  ((eq type 'item)
		   ;; preformatted title
		   (newsticker--cache-set-preformatted-title
		    item (buffer-substring pos (point)))))))))

(defun newsticker--buffer-statistics (feed-name-symbol)
  "Return a statistic string for the feed given by FEED-NAME-SYMBOL.
See `newsticker-statistics-format'."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "%a"
     (format "%d" (newsticker--stat-num-items feed-name-symbol))
     (replace-regexp-in-string
      "%i"
      (format "%d" (newsticker--stat-num-items feed-name-symbol 'immortal))
      (replace-regexp-in-string
       "%n"
       (format "%d" (newsticker--stat-num-items feed-name-symbol 'new))
       (replace-regexp-in-string
        "%o"
        (format "%d" (newsticker--stat-num-items feed-name-symbol 'old))
        (replace-regexp-in-string
         "%O"
         (format "%d" (newsticker--stat-num-items feed-name-symbol 'obsolete))
         newsticker-statistics-format)))))))

(defun newsticker--buffer-set-faces (start end)
  "Add face properties according to mark property.
Scans the buffer between START and END."
  (save-excursion
    (put-text-property start end 'face 'newsticker-default-face)
    (goto-char start)
    (let ((pos1 start)
          (pos2 1)
          (nt-face (get-text-property start 'nt-face))
          (nt-age (get-text-property start 'nt-age)))
      (when nt-face
        (setq pos2 (next-single-property-change (point) 'nt-face))
        (newsticker--set-face-properties pos1 pos2 nt-face nt-age)
        (setq nt-face (get-text-property pos2 'nt-face))
        (setq pos1 pos2))
      (while (and (setq pos2 (next-single-property-change pos1 'nt-face))
                  (<= pos2 end)
                  (> pos2 pos1))
        (newsticker--set-face-properties pos1 pos2 nt-face nt-age)
        (setq nt-face (get-text-property pos2 'nt-face))
        (setq nt-age (get-text-property pos2 'nt-age))
        (setq pos1 pos2)))))

(defun newsticker--buffer-set-invisibility (start end)
  "Add invisibility properties according to nt-type property.
Scans the buffer between START and END.  Sets the 'invisible
property to '(<nt-type>-<nt-age> <nt-type> <nt-age>)."
  (save-excursion
    ;; reset invisibility settings
    (put-text-property start end 'invisible nil)
    ;; let's go
    (goto-char start)
    (let ((pos1 start)
          (pos2 1)
          (nt-type (get-text-property start 'nt-type))
          (nt-age (get-text-property start 'nt-age)))
      (when nt-type
        (setq pos2 (next-single-property-change (point) 'nt-type))
        (put-text-property (max (point-min) pos1) (1- pos2)
                           'invisible
                           (list (intern
                                  (concat
                                   (symbol-name
                                    (if (eq nt-type 'extra) 'desc nt-type))
                                   "-"
                                   (symbol-name nt-age)))
                                 nt-type
                                 nt-age))
        (setq nt-type (get-text-property pos2 'nt-type))
        (setq pos1 pos2))
      (while (and (setq pos2 (next-single-property-change pos1 'nt-type))
                  (<= pos2 end)
                  (> pos2 pos1))
        ;; must shift one char to the left in order to handle invisible
        ;; newlines, motion in invisible text areas and all that correctly
        (put-text-property (1- pos1) (1- pos2)
                           'invisible
                           (list (intern
                                  (concat
                                   (symbol-name
                                    (if (eq nt-type 'extra) 'desc nt-type))
                                   "-"
                                   (symbol-name nt-age)))
                                 nt-type
                                 nt-age))
        (setq nt-type (get-text-property pos2 'nt-type))
        (setq nt-age (get-text-property pos2 'nt-age))
        (setq pos1 pos2)))))

(defun newsticker--set-face-properties (pos1 pos2 nt-face age)
  "Set the face for the text between the positions POS1 and POS2.
The face is chosen according the values of NT-FACE and AGE."
  (let ((face (cond ((eq nt-face 'feed)
                     'newsticker-feed-face)
                    ((eq nt-face 'item)
                     (cond ((eq age 'new)
                            'newsticker-new-item-face)
                           ((eq age 'old)
                            'newsticker-old-item-face)
                           ((eq age 'immortal)
                            'newsticker-immortal-item-face)
                           ((eq age 'obsolete)
                            'newsticker-obsolete-item-face)))
                    ((eq nt-face 'date)
                     'newsticker-date-face)
                    ((eq nt-face 'stat)
                     'newsticker-statistics-face)
                    ((eq nt-face 'extra)
                     'newsticker-extra-face)
                    ((eq nt-face 'enclosure)
                     'newsticker-enclosure-face))))
    (when face
      (put-text-property pos1 (max pos1 pos2) 'face face))))

;; ======================================================================
;;; Functions working on the *newsticker* buffer
;; ======================================================================
(defun newsticker--buffer-make-item-completely-visible ()
  "Scroll buffer until current item is completely visible."
  (when newsticker--auto-narrow-to-feed
    (let* ((min (or (save-excursion (newsticker--buffer-beginning-of-feed))
                    (point-min)))
           (max (or (save-excursion (newsticker--buffer-end-of-feed))
                    (point-max))))
      (narrow-to-region min max)))
  (when newsticker--auto-narrow-to-item
    (let* ((min (or (save-excursion (newsticker--buffer-beginning-of-item))
                    (point-min)))
           (max (or (save-excursion (newsticker--buffer-end-of-item))
                    (point-max))))
      (narrow-to-region min max)))
  (sit-for 0)
  ;; do not count lines and stuff because that does not work when images
  ;; are displayed. Do it the simple way:
  (save-excursion
    (newsticker--buffer-end-of-item)
    (unless (pos-visible-in-window-p)
      (recenter -1)))
  (unless (pos-visible-in-window-p)
    (recenter 0)))

(defun newsticker--buffer-get-feed-title-at-point ()
  "Return feed symbol of headline at point."
  (format "%s" (or (get-text-property (point) 'feed) " ")))

(defun newsticker--buffer-get-item-title-at-point ()
  "Return feed symbol of headline at point."
  (format "%s" (or (get-text-property (point) 'nt-title) " ")))

(defun newsticker--buffer-goto (types &optional age backwards)
  "Search next occurrence of TYPES in current buffer.
TYPES is a list of symbols.  If TYPES is found point is moved, if
not point is left unchanged.  If optional parameter AGE is not
nil, the type AND the age must match.  If BACKWARDS is t, search
backwards."
  (let ((pos (save-excursion
	       (save-restriction
		 (widen)
		 (catch 'found
		   (let ((tpos (point)))
		     (while (setq tpos
				  (if backwards
				      (if (eq tpos (point-min))
					  nil
					(or (previous-single-property-change
					     tpos 'nt-type)
					    (point-min)))
				    (next-single-property-change
				     tpos 'nt-type)))
		       (and (memq (get-text-property tpos 'nt-type) types)
			    (or (not age)
				(eq (get-text-property tpos 'nt-age) age))
			    (throw 'found tpos)))))))))
    (when pos
      (goto-char pos))
    pos))

(defun newsticker--buffer-hideshow (mark-age onoff)
  "Hide or show items of type MARK-AGE.
If ONOFF is nil the item is hidden, otherwise it is shown."
  (if onoff
      (remove-from-invisibility-spec mark-age)
    (add-to-invisibility-spec mark-age)))

(defun newsticker--buffer-beginning-of-item ()
  "Move point to the beginning of the item at point.
Return new position."
  (if (bobp)
      (point)
    (let ((type (get-text-property (point) 'nt-type))
          (typebefore (get-text-property (1- (point)) 'nt-type)))
      (if (and (memq type '(item feed))
                   (not (eq type typebefore)))
          (point)
        (newsticker--buffer-goto '(item feed) nil t)
        (point)))))

(defun newsticker--buffer-beginning-of-feed ()
  "Move point to the beginning of the feed at point.
Return new position."
  (if (bobp)
      (point)
    (let ((type (get-text-property (point) 'nt-type))
          (typebefore (get-text-property (1- (point)) 'nt-type)))
      (if (and (memq type '(feed))
                   (not (eq type typebefore)))
          (point)
        (newsticker--buffer-goto '(feed) nil t)
        (point)))))

(defun newsticker--buffer-end-of-item ()
  "Move point to the end of the item at point.
Take care: end of item is at the end of its last line!"
  (when (newsticker--buffer-goto '(item feed nil))
    (point)))

(defun newsticker--buffer-end-of-feed ()
  "Move point to the end of the last item of the feed at point.
Take care: end of item is at the end of its last line!"
  (when (newsticker--buffer-goto '(feed nil))
    (backward-char 1)
    (point)))

;; ======================================================================
;;; misc
;; ======================================================================

(defun newsticker-mouse-browse-url (event)
  "Call `browse-url' for the link of the item at which the EVENT occurred."
  (interactive "e")
  (save-excursion
    (switch-to-buffer (window-buffer (posn-window (event-end event))))
    (let ((url (get-text-property (posn-point (event-end event))
                                  'nt-link)))
      (when url
        (browse-url url)
        (save-excursion
          (goto-char (posn-point (event-end event)))
          (if newsticker-automatically-mark-visited-items-as-old
              (newsticker-mark-item-at-point-as-read t)))))))

(defun newsticker-browse-url ()
  "Call `browse-url' for the link of the item at point."
  (interactive)
  (let ((url (get-text-property (point) 'nt-link)))
    (when url
      (browse-url url)
      (if newsticker-automatically-mark-visited-items-as-old
          (newsticker-mark-item-at-point-as-read t)))))

(defvar newsticker-open-url-history
  '("wget" "xmms" "realplay")
  "...")

(defun newsticker-handle-url ()
  "Ask for a program to open the link of the item at point."
  (interactive)
  (let ((url (get-text-property (point) 'nt-link)))
    (when url
      (let ((prog (read-string "Open url with: " nil
                               'newsticker-open-url-history)))
        (when prog
          (message "%s %s" prog url)
          (start-process prog prog prog url)
      (if newsticker-automatically-mark-visited-items-as-old
          (newsticker-mark-item-at-point-as-read t)))))))


;; ======================================================================
;;; Misc
;; ======================================================================

(defun newsticker--cache-sort ()
  "Sort the newsticker cache data."
  (let ((sort-fun (cond ((eq newsticker-sort-method 'sort-by-time)
                         'newsticker--cache-item-compare-by-time)
                        ((eq newsticker-sort-method 'sort-by-title)
                         'newsticker--cache-item-compare-by-title)
                        ((eq newsticker-sort-method 'sort-by-original-order)
                         'newsticker--cache-item-compare-by-position))))
    (mapc (lambda (feed-list)
            (setcdr feed-list (sort (cdr feed-list)
                                    sort-fun)))
          newsticker--cache)))

(provide 'newst-plainview)

;;; newst-plainview.el ends here
