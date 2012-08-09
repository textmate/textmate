;;; bruce.el --- bruce phrase utility for overloading the Communications -*- no-byte-compile: t -*-
;;; Decency Act snoops, if any.

;; Copyright (C) 1988, 1993, 1997, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: games
;; Created: Jan 1997

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

;; This program was written to protest the miss-named "Communications
;; Decency Act of 1996. This Act bans "indecent speech", whatever that is,
;; from the Internet. For more on the CDA, see Richard Stallman's essay on
;; censorship, included in the etc directory of emacs distributions 19.34
;; and up. See also http://www.eff.org/blueribbon.html.

;; For many years, emacs has included a program called Spook. This program
;; adds a series of "keywords" to email just before it goes out. On the
;; theory that the NSA monitors people's email, the keywords would be
;; picked up by the NSA's snoop computers, causing them to waste time
;; reading your meeting schedule notices or other email boring to everyone
;; but you and (you hope) the recipient. See below (I left in the original
;; writeup when I made this conversion), or the emacs documentation at
;; ftp://prep.ai.mit.edu/pub/gnu/emacs-manual*.

;; Bruce is a direct copy of spook, with the word "spook" replaced with
;; the word "bruce". Thanks to "esr", whoever he, she or it may be, this
;; conversion was an extremely easy piece of editing, suitable for a first
;; essay at elisp programming.

;; You may think of the name as having been derived from a certain Monty
;; Python routine. Or from Lenny Bruce, who opposed censorship in his own
;; inimitable way. Bruce does exactly what Spook does: it throws keywords
;; into your email messages or other documents.

;; However, in order to comply with the CDA as interpreted by Richard
;; Stallman (see the essay on censorship), bruce is distributed without a
;; data file from which to select words at random. Sorry about that. I
;; believe the average user will be able to come up with a few words on
;; his or her own. If that is a problem, feel free to ask any American
;; teenager, preferably one who attends a government school. Failing
;; that, you might write to Mr. Clinton or Ms Reno or their successors and
;; ask them for suggestions. Think of it as a public spirited act: the
;; time they spend answering you is time not spent persecuting someone
;; else. However, do ask them to respond by snail mail, where their
;; suggestions would be legal.

;; To build the data file, just start a file called bruce.lines in the etc
;; directory of your emacs distribution. Note that each phrase or word has
;; to be followed by an ascii 0, control-@. See the file spook.lines in
;; the etc directory for an example. In emacs, use c-q c-@ to insert the
;; ascii 0s.

;; Once you have edited up a data file, you have to tell emacs how to find
;; the program bruce. Add the following two lines to your .emacs file. Be
;; sure to uncomment the second line.

;; for bruce mode
;; (autoload 'bruce "bruce" "Use the Bruce program to protest the CDA" t)

;; Shut down emacs and fire it up again. Then "M-x bruce" should put some
;; shocking words in the current buffer.


;; Please note that I am not suggesting that you actually use this program
;; to add "illegal" words to your email, or any other purpose. First, you
;; don't really need a program to do it, and second, it would be illegal
;; for me to suggest or advise that you actually break the law. This
;; program was written as a demonstration only, and as an act of political
;; protest and free expression protected by the First Amendment, or
;; whatever is left of it.


;; We now return to the original writeup for spook:

;; Steve Strassmann <straz@media-lab.media.mit.edu> didn't write the
;; program spook, from which this was adapted, and even if he did, he
;; really didn't mean for you to use it in an anarchistic way.
;;
;; To use this:
;;  Just before sending mail, do M-x spook.
;;  A number of phrases will be inserted into your buffer, to help
;;  give your message that extra bit of attractiveness for automated
;;  keyword scanners.  Help defeat the NSA trunk trawler!

;;; Code:

(require 'cookie1)

; Variables
(defgroup bruce nil
  "Insert phrases selected at random from a file into a buffer."
  :prefix "bruce-"
  :group 'games)

(defcustom bruce-phrases-file "~/bruce.lines"
  "Keep your favorite phrases here."
  :type 'file
  :group 'bruce)

(defcustom bruce-phrase-default-count 15
  "Default number of phrases to insert."
  :type 'integer
  :group 'bruce)

;;;###autoload
(defun bruce ()
  "Adds that special touch of class to your outgoing mail."
  (interactive)
  (or (file-exists-p bruce-phrases-file)
      (error "You need to create %s" bruce-phrases-file))
  (cookie-insert bruce-phrases-file
		 bruce-phrase-default-count
		 "Checking authorization..."
		 "Checking authorization...Approved"))

;;;###autoload
(defun snarf-bruces ()
  "Return a vector containing the lines from `bruce-phrases-file'."
  (or (file-exists-p bruce-phrases-file)
      (error "You need to create %s" bruce-phrases-file))
  (cookie-snarf bruce-phrases-file
		"Checking authorization..."
		"Checking authorization...Approved"))

;; Note: the implementation that used to take up most of this file has been
;; cleaned up, generalized, gratuitously broken by esr, and now resides in
;; cookie1.el.

(provide 'bruce)

;;; bruce.el ends here
