;;; newsticker.el --- A Newsticker for Emacs.

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Filename:    newsticker.el
;; URL:         http://www.nongnu.org/newsticker
;; Created:     17. June 2003
;; Keywords:    News, RSS, Atom
;; Time-stamp:  "6. Dezember 2009, 19:15:18 (ulf)"
;; Version:     1.99

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

(defconst newsticker-version "1.99" "Version number of newsticker.el.")

;; ======================================================================
;;; Commentary:

;; Overview
;; --------

;; Newsticker provides a newsticker for Emacs.  A newsticker is a thing
;; that asynchronously retrieves headlines from a list of news sites,
;; prepares these headlines for reading, and allows for loading the
;; corresponding articles in a web browser.

;; Headlines consist of a title and (possibly) a small description.  They
;; are contained in "RSS" (RDF Site Summary) or "Atom" files.  Newsticker
;; should work with the following RSS formats:
;;  * RSS 0.91
;;    (see http://backend.userland.com/rss091 or
;;    http://my.netscape.com/publish/formats/rss-spec-0.91.html)
;;  * RSS 0.92
;;    (see http://backend.userland.com/rss092)
;;  * RSS 1.0
;;    (see http://purl.org/rss/1.0/spec)
;;  * RSS 2.0
;;    (see http://blogs.law.harvard.edu/tech/rss)
;; as well as the following Atom formats:
;;  * Atom 0.3
;;  * Atom 1.0
;;    (see http://www.ietf.org/internet-drafts/draft-ietf-atompub-format-11.txt)
;; That makes Newsticker.el an "Atom aggregator, "RSS reader", "RSS
;; aggregator", and "Feed Reader".

;; Newsticker provides several commands for reading headlines, navigating
;; through them, marking them as read/unread, hiding old headlines
;; etc.  Headlines can be displayed as plain text or as rendered HTML.

;; Headlines can be displayed in the echo area, either scrolling like
;; messages in a stock-quote ticker, or just changing.

;; Newsticker allows for automatic processing of headlines by providing
;; hooks and (sample) functions for automatically downloading images and
;; enclosed files (as delivered by podcasts, e.g.).

;; Requirements
;; ------------
;; Newsticker can be used with GNU Emacs version 21.1 or later as well as
;; XEmacs.  It requires an XML-parser (`xml.el') which is part of GNU
;; Emacs.  If you are using XEmacs you want to get the `net-utils' package
;; which contains `xml.el' for XEmacs.

;; Newsticker requires a program which can retrieve files via http and
;; prints them to stdout.  By default Newsticker will use wget for this
;; task.

;; Installation
;; ------------

;; If you are using Newsticker as part of GNU Emacs there is no need to
;; perform any installation steps in order to use Newsticker.  Otherwise
;; place Newsticker in a directory where Emacs can find it.  Add the
;; following line to your Emacs startup file (`~/.emacs').
;;   (add-to-list 'load-path "/path/to/newsticker/")
;;   (autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
;;   (autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)

;; If you are using `imenu', which allows for navigating with the help of a
;; menu, you should add the following to your Emacs startup file
;; (`~/.emacs').
;;   (add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)

;; That's it.

;; Usage
;; -----
;; The command newsticker-show-news will display all available headlines in
;; a special buffer, called `*newsticker*'.  It will also start the
;; asynchronous download of headlines.  The modeline in the `*newsticker*'
;; buffer informs whenever new headlines have arrived.  Clicking
;; mouse-button 2 or pressing RET in this buffer on a headline will call
;; browse-url to load the corresponding news story in your favorite web
;; browser.

;; The scrolling, or flashing of headlines in the echo area, can be started
;; with the command newsticker-start-ticker.  It can be stopped with
;; newsticker-stop-ticker.

;; If you just want to start the periodic download of headlines use the
;; command newsticker-start.  Calling newsticker-stop will stop the
;; periodic download, but will call newsticker-stop-ticker as well.

;; Configuration
;; -------------
;; All Newsticker options are customizable, i.e. they can be changed with
;; Emacs customization methods: Call the command customize-group and enter
;; `newsticker' for the customization group.

;; All Newsticker options have reasonable default values, so that in most
;; cases it is not necessary to customize settings before starting
;; Newsticker for the first time.

;; Newsticker options are organized in the following groups.

;; * newsticker-feed contains options that define which news
;;   feeds are retrieved and how this is done.
;;   o newsticker-url-list defines the list of headlines which are
;;     retrieved.
;;   o newsticker-retrieval-interval defines how often headlines are
;;     retrieved.
;; * newsticker-headline-processing contains options that define how the
;;   retrieved headlines are processed.
;;   o newsticker-keep-obsolete-items decides whether unread headlines that
;;     have been removed from the feed are kept in the Newsticker cache.
;; * newsticker-layout contains options that define how the buffer for
;;   reading news headlines is formatted.
;;   o newsticker-item-format defines how the title of a headline is
;;     formatted.
;; * newsticker-ticker contains options that define how headlines are shown
;;   in the echo area.
;;   o newsticker-display-interval and newsticker-scroll-smoothly define
;;     how headlines are shown in the echo area.
;; * newsticker-hooks contains options for hooking other Emacs commands to
;;   newsticker functions.
;;   o newsticker-new-item-functions allows for automatic processing of
;;     headlines.  See `newsticker-download-images', and
;;     `newsticker-download-enclosures' for sample functions.
;; * newsticker-miscellaneous contains other Newsticker options.

;; Please have a look at the customization buffers for the complete list of
;; options.

;; Remarks
;; -------
;; This newsticker is designed do its job silently in the background
;; without disturbing you.  However, it is probably impossible to prevent
;; such a tool from slightly attenuating your Editor's responsiveness every
;; once in a while.

;; Byte-compiling newsticker.el is recommended.

;; ======================================================================
;;; History:

;; 1.99
;;     * Lots! of changes.

;; 1.10x
;;     * Support for download via url. Setting the new variable
;;       `newsticker-download-method' to 'intern will make newsticker
;;       use the url-package instead of the external program
;;       wget. Default value is 'extern.
;;     * Re-enabled `newsticker-default-face'.
;;     * Workaround for broken extra-data.

;; 1.10 (2007-01-29)
;;     * Bugfixes mostly: `newsticker--decode-iso8601-date',
;;       `newsticker--sentinel', and others.
;;     * Renamed `newsticker--retrieval-timer-list' to
;;       `newsticker-retrieval-timer-list'.  Removed
;;       `newsticker-running-p' -- check newsticker-retrieval-timer-list
;;       to find out whether newsticker is running.  Removed
;;       `newsticker-ticker-running-p'.
;;     * Try to cache images in w3m-rendered HTML text.
;;     * Other minor changes.

;; 1.9 (2005-11-01)
;;     * Rewrote feed parsing part.  Newsticker now supports RSS 0.91,
;;       0.92, 1.0, 2.0 as well as Atom 0.3 and 1.0 -- thanks to Thien-Thi
;;       Nguyen.
;;     * Changed auto-marking mechanism: Replaced variable
;;       `newsticker-auto-mark-filter' with new variable
;;       `newsticker-auto-mark-filter-list', which allows for looking not
;;       only at the title but also at the description of a headline.
;;     * Call `newsticker--ticker-text-setup' only after all pending
;;       downloads processes have finished.
;;     * Improved handling of coding systems.
;;     * Added magic autoload comments.
;;     * Bugfixes:
;;       - `hide-entry' was hiding too much when called for the last
;;          headline,
;;       - update mode-line and menu-bar when necessary,
;;       - repaired `newsticker--imenu-goto',
;;       - other minor things.

;; 1.8 (2005-08-26)
;;     * Added commands `newsticker-show-extra' and `newsticker-hide-extra'
;;       to show and hide extra RSS elements, bound to "sx" and "hx"
;;       resp. Changed default value of `newsticker-show-all-rss-elements'
;;       to nil.
;;     * mode-line: Introduced special mode-line-format for newsticker.
;;     * Get feed logos only once every 24 h.
;;     * Default faces changed.
;;     * Minor fixes.

;; 1.7 (2005-06-25)
;;     * Tool-bar support: most important commands can be called from
;;       tool-bar buttons.
;;     * Auto-Narrowing introduced: *newsticker* buffer can be narrowed to
;;       a single item (bound to key `xi') or a single feed (bound to
;;       `xf').
;;     * Enclosure support: enclosed items are shown (see
;;       `newsticker-enclosure-face') and can be (automatically) downloaded
;;       (see below). For those of you who read "podcasts".
;;     * Added variable `newsticker-auto-mark-filter' for automatically
;;       marking items as immortal or old.
;;     * Added hook variable `newsticker-new-item-functions' for handling
;;       new items.  Added sample functions `newsticker-download-images',
;;       and `newsticker-download-enclosures'.
;;     * Added hook variable `newsticker-select-item-hook' which is run
;;       after `newsticker-(next|previous)-(new-)?-item'.
;;     * Added hook variable `newsticker-select-feed-hook' which is run
;;       after `newsticker-(next|previous)-feed'.
;;     * Added hook variable `newsticker-buffer-change-hook' which is run
;;       after the contents or visibility of the newsticker buffer has
;;       changed, e.g. after `newsticker-buffer-update' or
;;       `newsticker-show-feed-desc'.
;;     * Added command `newsticker-handle-url' for interactively launching
;;       arbitrary programs for URLs, bound to `C-RET'.
;;     * URLs in extra elements are clickable.
;;     * Better support for w3, added command
;;       `newsticker-w3m-show-inline-images' for displaying all inline
;;       images.
;;     * Insert an artificial headline which notifies about failed
;;       retrievals.
;;     * Use pubDate element (RSS 2.0) instead of retrieval time when
;;       available.
;;     * Customizable options grouped.
;;     * Bugfixes: `newsticker--imenu-create-index'; strip whitespace
;;       from links; apply coding-system to extra-elements; time-comparison
;;       for obsolete items; and others which I have forgotten.
;;     * Workaround for another bug in xml-parse-region -- thanks to
;;       anonymous for sending patch.
;;     * Renamed invisible buffers ` *wget-newsticker-<feed>*' to
;;       ` *newsticker-wget-<feed>*'.
;;     * Tested with GNU Emacs versions 21.3 and 22.0 and XEmacs
;;       21.something.

;; 1.6 * Support for (some) optional RSS elements: guid, dc:date. See
;;       `newsticker-show-all-rss-elements' `newsticker-extra-face'.
;;     * Better support for w3m -- `newsticker-default-face' is obsolete
;;       now, removed `newsticker-w3m-toggle-inline-image'.
;;     * Added `newsticker-desc-comp-max' -- comparison of item
;;       descriptions can take quite some time.
;;     * Added `newsticker--buffer-make-item-completely-visible' to
;;       ensure that the current item is fully visible.
;;     * Allow for non-positive retrieval-interval, which make newsticker
;;       get news only once.
;;     * Use :set for customizable variables.
;;     * Added `newsticker-buffer-force-update', bound to key `U'.
;;     * Added concept of obsolete items, see
;;       `newsticker-keep-obsolete-items', `newsticker-obsolete-item-face',
;;       `newsticker-obsolete-item-max-age'.
;;     * Added `newsticker-add-url'.
;;     * OPML export.
;;     * Save pre-formatted titles => even better performance!!
;;     * `newsticker-*-new-item' wraps at beginning/end of buffer.
;;     * Always sort obsolete items to end of item list.
;;     * Bugfixes:
;;       - newsticker-hide-entry,
;;       - changes of feed-titles led to duplicate feed items,
;;       - faces for rendered HTML texts,
;;       - length of ticker-text (for "exotic"/multibyte texts),
;;         Thanks to Hiroshi Maruyama.
;;       - suppress items with empty title and description
;;       - newsticker-sort-method was ignored!
;;       - prevent call of fill-region on HTML-rendered descriptions.

;; 1.5 * Rewrote the visibility stuff. newsticker does not inherit
;;       outline anymore.  Now you have complete freedom for
;;       `newsticker-*-format'.
;;     * Save pre-formatted descriptions => incredible performance boost!!
;;     * Introduced `newsticker-(start|stop)-ticker'.
;;     * Introduced statistics for heading-format and
;;       `newsticker-statistics-face'.
;;     * Introduced `newsticker-enable-logo-manipulations'.
;;     * Compare link of items (as well as title and desc).
;;     * Added `newsticker-start-hook' and `newsticker-stop-hook', thanks
;;       to mace.
;;     * Bugfixes -- thanks to Ryan Yeske, Jari Aalto, Bruce Ingalls.
;;     * Tested with Emacs 21.3.50, 21.3.1, 21.2, 21.1; XEmacs 21.4.15

;; 1.4 * Enabled HTML rendering, added `newsticker-html-renderer' to
;;       choose a HTML rendering engine, thanks to Greg Scott for testing
;;     * New Outline handling using text properties instead of "**"
;;       prefixes.
;;     * Added possibility to mark single item as old (bound to key
;;       `o' (`newsticker-mark-item-at-point-as-read').
;;     * Added possibility to mark single item as immortal (bound to key
;;       `i' (`newsticker-mark-item-at-point-as-immortal').
;;     * Added possibility to display feed logos.
;;     * Added `newsticker-heading-format', `newsticker-item-format'.
;;     * Added `newsticker-date-format'.
;;     * Added `newsticker-justification'.
;;     * Added `newsticker-automatically-mark-visited-items-as-old'.
;;     * Added `newsticker-w3m-toggle-inline-image' which calls
;;       `w3m-toggle-inline-image' if `newsticker-html-renderer' is
;;       `w3m-region'. Exists for convenience only (bound to key
;;       `RET').

;; 1.3 * Compare title AND desc to check whether item is old, except
;;       for feed desc
;;     * Mark as not-up-to-date only after new items have arrived.
;;     * Added XEmacs compatibility code, tested with XEmacs 21.4.13.
;;     * Tested with Emacs 21.3.50 and Emacs 21.2.something.
;;     * Bugfix: Apply coding-systems to feed title and description,
;;       thanks to OHASHI Akira
;;     * Bugfix: xml-parser-workaround did not work for japanese texts,
;;       thanks to OHASHI Akira
;;     * Kill wget-buffers unless newsticker-debug is not nil.
;;     * Bugfix: xml-parser-workaround for "DOCTYPE rdf:RDF"

;; 1.2 Peter S Galbraith <psg@debian.org>
;;     * Added `newsticker-url-list-defaults', splitting the URLs into
;;       a customizable selection list, and a user add-on list.
;;     * Minor checkdoc fixes.

;; 1.1 * Introduced optional feed-specific wget-arguments.
;;     * Keep order of feeds as given in `newsticker-url-list' in
;;       *newsticker* buffer.
;;     * Ignore unsupported coding systems.

;; 1.0 * Introduced feed-specific retrieval-timers.
;;     * Removed dependency on 'cl (cddddr).
;;     * Thanks to Kevin Rodgers and T.V.  Raman for their help.
;;     * Use utf-8 for reading and writing cache data.
;;     * Reported to work with Emacs 21.3.50.

;; 0.99 * Minor tweaks.
;;      * Tested with Emacs 21.3.2

;; 0.98 * Check exit status of wget processes.  Keep cache data if
;;        something went wrong.  Throw error when old wget-processes
;;        are hanging around.
;;      * Introduced newsticker-specific faces.
;;      * Added `newsticker-show-descriptions-of-new-items'.
;;      * Added `newsticker-hide-old-items-in-newsticker-buffer'.
;;      * Added `newsticker-(hide|show)-old-items'.

;; 0.97 * Minor tweaks.

;; 0.96 * Added caching.
;;      * newsticker-mode inherits outline-mode.
;;      * newsticker-mode supports imenu.
;;      * Easy buffer-navigation with newsticker-mode's keymap.
;;      * Some bugs fixed.
;;      * Thanks to Moritz Epple for documentation tips.

;; 0.95 * Added newsticker-mode -- Thanks to T.V.  Raman.
;;      * Catch xml-parser errors -- Thanks to T.V.  Raman.
;;      * Remove stupid newlines in titles (headlines) -- Thanks to
;;        Jeff Rancier.

;; 0.94 * Added clickerability and description for channel headings.
;;      * Made it work for (at least some) rss 0.9<something> feeds.

;; 0.93 * Added some more sites.
;;      * Do not flood the *Messages* buffer.
;;      * First attempt at handling coding systems.

;; 0.92 * Added `newsticker-wget-name'.
;;      * Try to display message only if minibuffer and echo area are
;;        not in use already.
;;      * Dirty workaround for newer versions of xml.el: Remove
;;        whitespace in rdf.
;;      * Tested with Emacs 21.3.2 and CVS-snapshot of 2003-06-21.

;; 0.91 * First bugfix: *newsticker* is read-only.

;; 0.9  * First release.
;;      * Tested with Emacs 21.3.2 and wget 1.8.2.

;; ======================================================================
;;; Code:

(require 'newst-backend)
(require 'newst-ticker)
(require 'newst-reader)
(require 'newst-plainview)
(require 'newst-treeview)

(provide 'newsticker)

;;; newsticker.el ends here
