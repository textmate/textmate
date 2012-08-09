;;; manoj-dark.el --- A dark theme from Manoj

;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: Manoj Srivastava <srivasta@ieee.org>
;; Keywords: lisp, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I spend a lot of time working in front of a screen (many hours in a
;; dimly lit room) and eye fatigue is an issue. This is a dark color
;; theme for emacs, which is easier on the eyes than light themes.

;; It does not help that I am blue-green color blind, so subtle
;; variations are often lost on me. I do want to use color contrast to
;; increase productivity, but I also want to avoid the jarring angry
;; fruit salad look, and so I am in the process of crafting a logical
;; color scheme that is high contrast enough for me, without being too
;; unpleasing.

;; In circumstances where there a lot of related faces that can be
;; viewed, for example, the Gnus group buffer, consistent and logical
;; color choices are the only sane option. Gnus groups can be newa
;; (blueish) or mail (greenish), have states (large number of under
;; messages, normal, and empty). The large number unread groups have
;; highest luminance (appear brighter), and the empty one have lower
;; luminance (appear grayer), but have the same chroma and saturation.
;; Sub states and group priorities are rendered using a color series
;; which has constant luminance and saturation, and vary in hue by a
;; constant separation -- so all the related groups have the same
;; brightness ({mail,news}/{unread,normal,empty}), and a graded
;; selection of foreground colors.  It sounds more complicated that it
;; looks. The eye is drawn naturally to the unread groups, and first
;; to the mail, then USENET groups (which is my preference).

;; Similar color variations occur for individual messages in a group;
;; high scoring messages bubble to the top, and have a higher
;; luminance.  This color schema has made me slightly faster at
;; reading mail/USENET.

;; In the message itself, quoted mail messages from different people
;; are color coordinated, with high contrast between citations that are
;; close to each other in the hierarchy, so it is less likely that one
;; misunderstands who said what in a long conversation.

;; The following scheme covers programming languages, Gnus, Erc, mail,
;; org-mode, CUA-mode, apt-utils, bbdb, compilation buffers, changelog
;; mode, diff and ediff, eshell, and more. You need emacs-goodies
;; package on Debian to use this.  See the wiki page at
;; http://www.emacswiki.org/cgi-bin/wiki?ColorTheme for details. The
;; project home page is at https://gna.org/projects/color-theme.

;;; Code:

(deftheme manoj-dark
  "Very high contrast faces with a black background.
This theme avoids subtle color variations, while avoiding the
jarring angry fruit salad look to reduce eye fatigue.")

(custom-theme-set-faces
 'manoj-dark
 '(default ((t (:background "black" :foreground "WhiteSmoke"))))
 ;; Font lock faces
 '(font-lock-builtin-face       ((t (:foreground "LightSteelBlue"))))
 '(font-lock-constant-face      ((t (:foreground "LightSlateBlue" :bold t))))
 '(font-lock-preprocessor-face  ((t (:foreground "CornFlowerBlue" :italic t))))
 '(font-lock-keyword-face       ((t (:foreground "cyan1"))))
 '(font-lock-type-face          ((t (:foreground "SteelBlue1"))))
 '(font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "Aquamarine"))))
 '(font-lock-function-name-face ((t (:foreground "mediumspringgreen"
				     :weight bold :height 1.1))))
 '(font-lock-string-face        ((t (:foreground "RosyBrown1"))))
 '(font-lock-comment-face       ((t (:italic t :slant oblique :foreground "chocolate1"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "Salmon"))))
 '(font-lock-doc-face           ((t (:italic t :slant oblique :foreground "LightCoral"))))
 '(font-lock-doc-string-face    ((t (:foreground "Plum"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))

 '(cperl-array-face ((t (:foreground "LawnGreen" :background "B;ack" :bold t))))
 '(cperl-hash-face ((t (:foreground "SpringGreen" :background "B;ack" :bold t :italic t))))
 '(cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

 '(gnus-button ((t (:bold t :weight bold :background "#191932" :box (:line-width 2 :style released-button)))))
 '(gnus-cite-attribution-face ((t (:italic t))))
 '(gnus-cite-face-1  ((t (:foreground "CornflowerBlue"))))
 '(gnus-cite-face-2  ((t (:foreground "PaleGreen"))))
 '(gnus-cite-face-3  ((t (:foreground "LightGoldenrod"))))
 '(gnus-cite-face-4  ((t (:foreground "LightPink"))))
 '(gnus-cite-face-5  ((t (:foreground "turquoise"))))
 '(gnus-cite-face-6  ((t (:foreground "khaki"))))
 '(gnus-cite-face-7  ((t (:foreground "plum"))))
 '(gnus-cite-face-8  ((t (:foreground "DeepSkyBlue1"))))
 '(gnus-cite-face-9  ((t (:foreground "chartreuse1"))))
 '(gnus-cite-face-10 ((t (:foreground "thistle1"))))
 '(gnus-cite-face-11 ((t (:foreground "LightYellow1"))))
 '(gnus-emphasis-bold ((t (:bold t :weight bold))))
 '(gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
 '(gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))
 '(gnus-emphasis-italic ((t (:italic t :slant italic))))
 '(gnus-emphasis-strikethru ((t (:strike-through t))))
 '(gnus-emphasis-underline ((t (:underline t))))
 '(gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))
 '(gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))
 '(gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))

 '(gnus-header-content ((t (:italic t :foreground "DarkKhaki" :slant italic))))
 '(gnus-header-content-face ((t (:italic t :foreground "DarkKhaki" :slant italic))))
 '(gnus-header-from ((t (:foreground "PaleGreen1"))))
 '(gnus-header-from-face ((t (:foreground "PaleGreen1"))))
 '(gnus-header-name ((t (:bold t :foreground "BlanchedAlmond" :weight bold))))
 '(gnus-header-name-face ((t (:bold t :foreground "BlanchedAlmond" :weight bold))))
 '(gnus-header-newsgroups ((t (:italic t :foreground "yellow" :slant italic))))
 '(gnus-header-newsgroups-face ((t (:italic t :foreground "yellow" :slant italic))))
 '(gnus-header-subject ((t (:foreground "coral1"))))
 '(gnus-header-subject-face ((t (:foreground "coral1"))))
 '(gnus-signature ((t (:italic t :slant italic))))
 '(gnus-signature-face ((t (:italic t :slant italic))))
 '(gnus-splash ((t (:foreground "#cccccc"))))
 '(gnus-summary-cancelled ((t (:background "black" :foreground "yellow"))))
 '(gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
 '(gnus-summary-high-ancient ((t (:bold t :foreground "CornflowerBlue" :weight bold))))
 '(gnus-summary-high-ancient-face ((t (:bold t :foreground "CornflowerBlue" :weight bold))))
 '(gnus-summary-normal-ancient ((t (:foreground "SkyBlue"))))
 '(gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))
 '(gnus-summary-low-ancient ((t (:italic t :foreground "LightSteelBlue" :slant italic))))
 '(gnus-summary-low-ancien-facet ((t (:italic t :foreground "LightSteelBlue" :slant italic))))

 '(gnus-summary-high-read ((t (:bold t :foreground "grey60" :weight bold))))
 '(gnus-summary-high-read-face ((t (:bold t :foreground "grey60" :weight bold))))
 '(gnus-summary-normal-read ((t (:foreground "grey50"))))
 '(gnus-summary-normal-read-face ((t (:foreground "grey50"))))
 '(gnus-summary-low-read ((t (:italic t :foreground "LightSlateGray" :slant italic))))
 '(gnus-summary-low-read-face ((t (:italic t :foreground "LightSlateGray" :slant italic))))

 '(gnus-summary-high-ticked ((t (:bold t :foreground "RosyBrown" :weight bold))))
 '(gnus-summary-high-ticked-face ((t (:bold t :foreground "RosyBrown" :weight bold))))
 '(gnus-summary-normal-ticked ((t (:foreground "LightSalmon"))))
 '(gnus-summary-normal-ticked-face ((t (:foreground "LightSalmon"))))
 '(gnus-summary-low-ticked ((t (:italic t :foreground "pink" :slant italic))))
 '(gnus-summary-low-ticked-face ((t (:italic t :foreground "pink" :slant italic))))

 '(gnus-summary-high-undownloaded ((t (:bold t :foreground "ivory3" :weight bold))))
 '(gnus-summary-normal-undownloaded ((t (:foreground "LightGray" :weight normal))))
 '(gnus-summary-low-undownloaded ((t (:italic t :foreground "grey75" :slant italic :weight normal))))

 '(gnus-summary-high-unread ((t (:bold t :foreground "PaleGreen" :weight bold))))
 '(gnus-summary-high-unread-face ((t (:bold t :foreground "PaleGreen" :weight bold))))
 '(gnus-summary-normal-unread ((t (:foreground "YellowGreen"))))
 '(gnus-summary-normal-unread-face ((t (:foreground "YellowGreen"))))
 '(gnus-summary-low-unread ((t (:italic t :foreground "MediumSeaGreen" :slant italic))))
 '(gnus-summary-low-unread-face ((t (:italic t :foreground "MediumSeaGreen" :slant italic))))
 '(gnus-summary-root-face ((t (:bold t :foreground "Red" :weight bold))))
 '(gnus-summary-selected ((t (:underline t :foreground "LemonChiffon"))))
 '(gnus-summary-selected-face ((t (:underline t :foreground "LemonChiffon"))))
 '(gnus-user-agent-bad-face ((t (:bold t :background "black" :foreground "red" :weight bold))))
 '(gnus-user-agent-good-face ((t (:background "black" :foreground "green"))))
 '(gnus-user-agent-unknown-face ((t (:bold t :background "black" :foreground "orange" :weight bold))))
 '(gnus-x-face ((t (:background "white" :foreground "black"))))

 '(gnus-group-mail-1 ((t (:bold t :foreground "#3BFF00" :weight normal))))
 '(gnus-group-mail-1-face ((t (:bold t :foreground "#3BFF00" :weight normal))))
 '(gnus-group-mail-2 ((t (:bold t :foreground "#5EFF00" :weight normal))))
 '(gnus-group-mail-2-face ((t (:bold t :foreground "#5EFF00" :weight normal))))
 '(gnus-group-mail-3 ((t (:bold t :foreground "#80FF00" :weight normal))))
 '(gnus-group-mail-3-face ((t (:bold t :foreground "#A1FF00" :weight normal))))


 '(gnus-group-mail-1-empty ((t (:foreground "#249900"))))
 '(gnus-group-mail-1-empty-face ((t (:foreground "#249900"))))
 '(gnus-group-mail-2-empty ((t (:foreground "#389900"))))
 '(gnus-group-mail-2-empty-face ((t (:foreground "#389900"))))
 '(gnus-group-mail-3-empty ((t (:foreground "#4D9900"))))
 '(gnus-group-mail-3-empty-face ((t (:foreground "#4D9900"))))

 '(gnus-group-mail-low ((t (:bold t :foreground "aquamarine2" :weight bold))))
 '(gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine2" :weight bold))))
 '(gnus-group-mail-low-empty ((t (:foreground "aquamarine2"))))
 '(gnus-group-mail-low-empty-face ((t (:foreground "aquamarine2"))))

 '(gnus-group-news-1 ((t (:bold t :foreground "#8480FF" :weight bold))))
 '(gnus-group-news-1-face ((t (:bold t :foreground "#8480FF" :weight bold))))
 '(gnus-group-news-2 ((t (:bold t :foreground "#8088FF" :weight bold))))
 '(gnus-group-news-2-face ((t (:bold t :foreground "#8088FF" :weight bold))))
 '(gnus-group-news-3 ((t (:bold t :foreground "#8095FF" :weight bold))))
 '(gnus-group-news-3-face ((t (:bold t :foreground "#8095FF" :weight bold))))
 '(gnus-group-news-4 ((t (:bold t :foreground "#80A1FF" :weight bold))))
 '(gnus-group-news-4-face ((t (:bold t :foreground "#80A1FF" :weight bold))))
 '(gnus-group-news-5 ((t (:bold t :foreground "#80AEFF" :weight bold))))
 '(gnus-group-news-5-face ((t (:bold t :foreground "#80AEFF" :weight bold))))
 '(gnus-group-news-6 ((t (:bold t :foreground "#80BBFF" :weight bold))))
 '(gnus-group-news-6-face ((t (:bold t :foreground "#80BBFF" :weight bold))))

 '(gnus-group-news-1-empty ((t (:foreground "#524DFF"))))
 '(gnus-group-news-1-empty-face ((t (:foreground "#524DFF"))))
 '(gnus-group-news-2-empty ((t (:foreground "#4D58FF"))))
 '(gnus-group-news-2-empty-face ((t (:foreground "#4D58FF"))))
 '(gnus-group-news-3-empty ((t (:foreground "#4D6AFF"))))
 '(gnus-group-news-3-empty-face ((t (:foreground "#4D6AFF"))))
 '(gnus-group-news-4-empty ((t (:foreground "#4D7CFF"))))
 '(gnus-group-news-4-empty-face ((t (:foreground "#4D7CFF"))))
 '(gnus-group-news-5-empty ((t (:foreground "#4D8EFF"))))
 '(gnus-group-news-5-empty-face ((t (:foreground "#4D8EFF"))))
 '(gnus-group-news-6-empty ((t (:foreground "#4DA0FF"))))
 '(gnus-group-news-6-empty-face ((t (:foreground "#4DA0FF"))))

 '(gnus-group-news-low ((t (:bold t :foreground "DarkTurquoise" :weight bold))))
 '(gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise" :weight bold))))
 '(gnus-group-news-low-empty ((t (:foreground "DarkTurquoise"))))
 '(gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

 ;;message faces
 '(message-cited-text ((t (:foreground "red3"))))
 '(message-header-cc ((t (:bold t :foreground "chartreuse1" :weight bold))))
 '(message-header-cc-face ((t (:bold t :foreground "chartreuse1" :weight bold))))
 '(message-header-name ((t (:foreground "green"))))
 '(message-header-name-face ((t (:foreground "green"))))
 '(message-header-newsgroups ((t (:italic t :bold t :foreground "papaya whip" :slant italic :weight bold))))
 '(message-header-newsgroups-face ((t (:italic t :bold t :foreground "papaya whip" :slant italic :weight bold))))
 '(message-header-other ((t (:foreground "ivory"))))
 '(message-header-other-face ((t (:foreground "ivory"))))
 '(message-header-subject ((t (:foreground "OliveDrab1"))))
 '(message-header-subject-face ((t (:foreground "OliveDrab1"))))
 '(message-header-to ((t (:bold t :foreground "floral white" :weight bold))))
 '(message-header-to-face ((t (:bold t :foreground "floral white" :weight bold))))
 '(message-header-xheader ((t (:foreground "DeepSkyBlue1"))))
 '(message-header-xheader-face ((t (:foreground "DeepSkyBlue1"))))
 '(message-mml ((t (:foreground "MediumSpringGreen"))))
 '(message-mml-face ((t (:foreground "MediumSpringGreen"))))
 '(message-separator ((t (:foreground "LightSkyBlue1"))))
 '(message-separator-face ((t (:foreground "LightSkyBlue1"))))
 '(message-url ((t (:bold t :foreground "blue" :weight bold))))

 '(bg:erc-color-face0 ((t (:background "saddle brown"))))
 '(bg:erc-color-face1 ((t (:background "black"))))
 '(bg:erc-color-face10 ((t (:background "DodgerBlue4"))))
 '(bg:erc-color-face11 ((t (:background "cyan4"))))
 '(bg:erc-color-face12 ((t (:background "blue"))))
 '(bg:erc-color-face13 ((t (:background "deeppink"))))
 '(bg:erc-color-face14 ((t (:background "gray50"))))
 '(bg:erc-color-face15 ((t (:background "grey15"))))
 '(bg:erc-color-face2 ((t (:background "blue4"))))
 '(bg:erc-color-face3 ((t (:background "green4"))))
 '(bg:erc-color-face4 ((t (:background "red"))))
 '(bg:erc-color-face5 ((t (:background "brown"))))
 '(bg:erc-color-face6 ((t (:background "purple"))))
 '(bg:erc-color-face7 ((t (:background "orange"))))
 '(bg:erc-color-face8 ((t (:background "yellow4"))))
 '(bg:erc-color-face9 ((t (:background "green"))))
 '(erc-action-face ((t (:bold t :weight bold :foreground "turquoise1"))))
 '(erc-bold-face ((t (:bold t :weight bold))))
 '(erc-button ((t (:bold t :weight bold :foreground "RoyalBlue1" :box (:line-width 2 :style released-button)))))
 '(erc-button-face ((t (:bold t :weight bold :foreground "RoyalBlue1" :box (:line-width 2 :style released-button)))))
 '(erc-command-indicator-face ((t (:bold t :weight bold))))
 '(erc-current-nick-face ((t (:bold t :foreground "DarkTurquoise" :weight bold))))
 '(erc-dangerous-host-face ((t (:foreground "red"))))
 '(erc-direct-msg-face ((t (:foreground "sandybrown"))))
 '(erc-error-face ((t (:foreground "red"))))
 '(erc-fool-face ((t (:foreground "dim gray"))))
 '(erc-header-line ((t (:background "grey95" :foreground "ConFlowerBlue"))))
 '(erc-input-face ((t (:foreground "brown"))))
 '(erc-inverse-face ((t (:background "Black" :foreground "White"))))
 '(erc-keyword-face ((t (:bold t :foreground "pale green" :weight bold))))
 '(erc-my-nick-face ((t (:bold t :foreground "brown" :weight bold))))
 '(erc-nick-default-face ((t (:bold t :weight bold :foreground "DodgerBlue1"))))
 '(erc-button-nickname-face ((t (:bold t :weight bold :background "lightgrey" :foreground "black"  :box (:line-width 2 :style released-button) ))))
 '(erc-nick-msg-face ((t (:bold t :foreground "IndianRed" :weight bold))))
 '(erc-notice-face ((t (:bold t :foreground "SlateBlue" :weight bold))))
 '(erc-pal-face ((t (:bold t :foreground "Magenta" :weight bold))))
 '(erc-prompt-face ((t (:bold t :background "Navy" :foreground "lightBlue2" :weight bold))))
 '(erc-timestamp-face ((t (:bold t :foreground "SeaGreen1" :weight bold))))
 '(erc-underline-face ((t (:underline t))))
 '(fg:erc-color-face0 ((t (:foreground "BlanchedAlmond"))))
 '(fg:erc-color-face1 ((t (:foreground "beige"))))
 '(fg:erc-color-face10 ((t (:foreground "pale goldenrod"))))
 '(fg:erc-color-face11 ((t (:foreground "cyan"))))
 '(fg:erc-color-face12 ((t (:foreground "lightblue1"))))
 '(fg:erc-color-face13 ((t (:foreground "deeppink"))))
 '(fg:erc-color-face14 ((t (:foreground "gray50"))))
 '(fg:erc-color-face15 ((t (:foreground "gray90"))))
 '(fg:erc-color-face2 ((t (:foreground "blue4"))))
 '(fg:erc-color-face3 ((t (:foreground "green4"))))
 '(fg:erc-color-face4 ((t (:foreground "red"))))
 '(fg:erc-color-face5 ((t (:foreground "brown"))))
 '(fg:erc-color-face6 ((t (:foreground "purple"))))
 '(fg:erc-color-face7 ((t (:foreground "orange"))))
 '(fg:erc-color-face8 ((t (:foreground "yellow"))))
 '(fg:erc-color-face9 ((t (:foreground "green"))))

 '(org-agenda-date ((t (:foreground "LightSkyBlue"))))
 '(org-agenda-date-weekend ((t (:bold t :foreground "LightSkyBlue" :weight bold))))
 '(org-agenda-restriction-lock ((t (:background "skyblue4"))))
 '(org-agenda-structure ((t (:foreground "LightSkyBlue"))))
 '(org-archived ((t (:foreground "grey70"))))
 '(org-code ((t (:foreground "grey70"))))
 '(org-column ((t (:background "grey30" :slant normal :weight normal :height 81 :family "unknown-DejaVu Sans Mono"))))
 '(org-column-title ((t (:bold t :background "grey30" :underline t :weight bold))))
 '(org-date ((t (:foreground "Cyan" :underline t))))
 '(org-done ((t (:bold t :foreground "PaleGreen" :weight bold))))
 '(org-drawer ((t (:foreground "LightSkyBlue"))))
 '(org-ellipsis ((t (:foreground "LightGoldenrod" :underline t))))
 '(org-formula ((t (:foreground "chocolate1"))))
 '(org-headline-done ((t (:foreground "LightSalmon"))))
 '(org-hide ((t (:foreground "black"))))
 '(org-latex-and-export-specials ((t (:foreground "burlywood"))))
 '(org-level-1 ((t (:foreground "LightSkyBlue"))))
 '(org-level-2 ((t (:foreground "LightGoldenrod"))))
 '(org-level-3 ((t (:foreground "Cyan1"))))
 '(org-level-4 ((t (:foreground "chocolate1"))))
 '(org-level-5 ((t (:foreground "PaleGreen"))))
 '(org-level-6 ((t (:foreground "Aquamarine"))))
 '(org-level-7 ((t (:foreground "LightSteelBlue"))))
 '(org-level-8 ((t (:foreground "LightSalmon"))))
 '(org-link ((t (:foreground "Cyan" :underline t))))
 '(org-mode-line-clock  ((t (:foreground "DarkGreen" :underline t))))
 '(org-scheduled-previously ((t (:foreground "chocolate1"))))
 '(org-scheduled-today ((t (:foreground "PaleGreen"))))
 '(org-sexp-date ((t (:foreground "Cyan"))))
 '(org-special-keyword ((t (:foreground "LightSalmon"))))
 '(org-table ((t (:foreground "LightSkyBlue"))))
 '(org-tag ((t (:bold t :weight bold))))
 '(org-target ((t (:underline t))))
 '(org-time-grid ((t (:foreground "LightGoldenrod"))))
 '(org-todo ((t (:bold t :foreground "Pink" :weight bold))))
 '(org-upcoming-deadline ((t (:foreground "chocolate1"))))
 '(org-verbatim ((t (:foreground "grey70" :underline t))))
 '(org-warning ((t (:bold t :weight bold :foreground "Pink"))))
 '(outline-1 ((t (:foreground "LightSkyBlue"))))
 '(outline-2 ((t (:foreground "LightGoldenrod"))))
 '(outline-3 ((t (:foreground "Cyan1"))))
 '(outline-4 ((t (:foreground "chocolate1"))))
 '(outline-5 ((t (:foreground "PaleGreen"))))
 '(outline-6 ((t (:foreground "Aquamarine"))))
 '(outline-7 ((t (:foreground "LightSteelBlue"))))
 '(outline-8 ((t (:foreground "LightSalmon"))))


 '(CUA-global-mark-face ((t (:background "cyan" :foreground "black"))))
 '(CUA-rectangle-face ((t (:background "maroon" :foreground "white"))))
 '(CUA-rectangle-noselect-face ((t (:background "dimgray" :foreground "white"))))
 '(Info-title-1-face ((t (:bold t :weight bold :family "helv" :height 1.728))))
 '(Info-title-2-face ((t (:bold t :family "helv" :weight bold :height 1.44))))
 '(Info-title-3-face ((t (:bold t :weight bold :family "helv" :height 1.2))))
 '(Info-title-4-face ((t (:bold t :family "helv" :weight bold))))
 '(align-highlight-nochange-face ((t (:background "SkyBlue4"))))

 '(antlr-font-lock-keyword-face ((t (:foreground "SteelBlue")))) ;%
 '(antlr-font-lock-literal-face ((t (:foreground "PaleVioletRed"))))
 '(antlr-font-lock-ruledef-face ((t (:foreground "DarkGreen"))))
 '(antlr-font-lock-ruleref-face ((t (:foreground "SteelBlue"))))
 '(antlr-font-lock-tokendef-face ((t (:foreground "khaki"))))
 '(antlr-font-lock-tokenref-face ((t (:foreground "LightSteelBlue4"))))

 '(bbdb-company ((t (:italic t :slant italic :foreground "indian red"))))
 '(bbdb-field-name ((t (:bold t :weight bold :foreground "steel blue"))))
 '(bbdb-field-value ((t (:foreground "AntiqueWhite2"))))
 '(bbdb-name ((t (:underline t :foreground "cadet blue"))))

 '(bold ((t (:bold t :weight bold))))
 '(bold-italic ((t (:bold t :italic t :slant italic :weight bold))))
 '(border ((t (:background "gold" :foreground "black" ))))
 '(buffer-menu-buffer ((t (:bold t :weight bold))))
 '(button ((t (:underline t :box (:line-width 2 :color "grey"
                                              :style released-button)
                          :foreground "black" :background "grey"
                          :weight bold ))))
 '(calendar-today-face ((t (:underline t :bold t :foreground "cornsilk"))))
 '(change-log-acknowledgement-face ((t (:italic t :slant oblique :foreground "AntiqueWhite3"))))
 '(change-log-conditionals-face ((t (:foreground "Aquamarine"))))
 '(change-log-date-face ((t (:italic t :slant oblique :foreground "BurlyWood"))))
 '(change-log-email-face ((t (:foreground "Aquamarine"))))
 '(change-log-file-face ((t (:bold t :family "Verdana" :weight bold :foreground "LightSkyBlue" :height 0.9))))
 '(change-log-function-face ((t (:foreground "Aquamarine"))))
 '(change-log-list-face ((t (:foreground "LightSkyBlue"))))
 '(change-log-name-face ((t (:bold t :weight bold :foreground "Gold"))))

 '(comint-highlight-input ((t (:bold t :weight bold))))
 '(comint-highlight-prompt ((t (:foreground "cyan1"))))
 '(compilation-column-number ((t (:foreground "PaleGreen"))))
 '(compilation-error ((t (:bold t :weight bold :foreground "Brown1"))))
 '(compilation-info ((t (:bold t :foreground "LightPink1" :weight bold))))
 '(compilation-line-number ((t (:foreground "LightGoldenrod"))))
 '(compilation-message-face  ((t (:underline t))))
 '(compilation-warning ((t (:bold t :foreground "Orange" :weight bold))))
 '(compilation-warning-face ((t (:bold t :foreground "Orange" :weight bold))))
 '(completions-common-part ((t (:family "unknown-DejaVu Sans Mono"
                                        :width normal :weight normal
                                        :slant normal :foreground "WhiteSmoke"
                                        :background "black" :height 81))))
 '(completions-first-difference ((t (:bold t :weight bold))))

 '(css-selector ((t (:foreground "LightSteelBlue"))))
 '(css-property ((t (:foreground "light sea green"))))

 '(cursor ((t (:background "orchid"))))
 '(custom-button-face ((t (:background "lightgrey" :foreground "black"
                                       :box '(:line-width 2 :style released-button)))))
 '(custom-button-pressed-face ((t (:background "lightgrey"
                                               :foreground "black"
                                               :box '(:line-width 2 :style pressed-button)))))
 '(custom-changed-face ((t (:foreground "wheat" :background "blue"))))
 '(custom-comment-face ((t (:background "dim gray"))))
 '(custom-comment-tag-face ((t (:foreground "gray80"))))
 '(custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.1))))
 '(custom-group-tag-face ((t (:bold t :family "helv" :foreground "light blue" :weight bold :height 1.1))))
 '(custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "pink" :weight bold :height 1.1))))
 '(custom-invalid-face ((t (:background "red" :foreground "yellow"))))
 '(custom-modified-face ((t (:background "blue" :foreground "white"))))
 '(custom-rogue-face ((t (:background "black" :foreground "pink"))))
 '(custom-saved-face ((t (:underline t))))
 '(custom-set-face ((t (:background "white" :foreground "blue"))))
 '(custom-state-face ((t (:foreground "lime green"))))
 '(custom-variable-button-face ((t (:bold t :underline t :weight bold
                                          :background "lightgrey"
                                          :foreground "black"
                                          :box '(:line-width 2 :style released-button)))))
 '(custom-variable-tag-face ((t (:bold t :family "helv"
                                       :foreground "light blue"
                                       :weight bold :height 1.2))))

 '(diary ((t (:foreground "IndianRed"))))
 '(diary-anniversary ((t (:foreground "Cyan1"))))
 '(diary-button ((t (:background "lightgrey" :foreground "black"  :box (:line-width 2 :style released-button)))))
 '(diary-face ((t (:foreground "IndianRed"))))
 '(diary-time ((t (:foreground "LightGoldenrod"))))
 '(diff-added ((t (:foreground "Green"))))
 '(diff-added-face ((t (:foreground "Green"))))
 '(diff-changed-face ((t (:foreground "Khaki"))))
 '(diff-context-face ((t (:foreground "grey70"))))
 '(diff-file-header ((t (:bold t :background "grey20"  :foreground "ivory1" :weight bold))))
 '(diff-file-header-face ((t (:bold t :background "grey20" :foreground "ivory1" :weight bold))))
 '(diff-function-face ((t (:foreground "SpringGreen1"))))
 '(diff-header-face ((t (:background "SlateBlue4"))))
 '(diff-hunk-header ((t (:slant italic :background "DodgerBlue4"))))
 '(diff-hunk-header-face ((t (:slant italic :background "DodgerBlue4"))))
 '(diff-index-face ((t (:bold t :weight bold :background "SteelBlue4" :foreground "linen" ))))
 '(diff-nonexistent ((t (:bold t :weight bold :background "Black" :foreground "Wheat1"))))
 '(diff-nonexistent-face ((t (:bold t :weight bold :background "Black" :foreground "Wheat1"))))
 '(diff-removed ((t (:foreground "salmon1"))))
 '(diff-removed-face ((t (:foreground "salmon1"))))
 '(diff-refine-change-face ((t (:background "MidnightBlue"))))
 '(diff-refine-change      ((t (:background "MidnightBlue"))))

 '(ediff-current-diff-face-A ((t (:foreground "firebrick" :background "pale green"))))
 '(ediff-current-diff-face-Ancestor ((t (:foreground "Black" :background "VioletRed"))))
 '(ediff-current-diff-face-B ((t (:foreground "DarkOrchid" :background "Yellow"))))
 '(ediff-current-diff-face-C ((t (:foreground "Navy" :background "Pink"))))
 '(ediff-even-diff-face-A ((t (:foreground "Black" :background "light grey"))))
 '(ediff-even-diff-face-Ancestor ((t (:foreground "White" :background "Grey"))))
 '(ediff-even-diff-face-B ((t (:foreground "White" :background "Grey"))))
 '(ediff-even-diff-face-C ((t (:foreground "Black" :background "light grey"))))
 '(ediff-fine-diff-face-A ((t (:foreground "Navy" :background "sky blue"))))
 '(ediff-fine-diff-face-Ancestor ((t (:foreground "Black" :background "Green"))))
 '(ediff-fine-diff-face-B ((t (:foreground "Black" :background "cyan"))))
 '(ediff-fine-diff-face-C ((t (:foreground "Black" :background "Turquoise"))))
 '(ediff-odd-diff-face-A ((t (:foreground "White" :background "Grey"))))
 '(ediff-odd-diff-face-Ancestor ((t (:foreground "Black" :background "light grey"))))
 '(ediff-odd-diff-face-B ((t (:foreground "Black" :background "light grey"))))
 '(ediff-odd-diff-face-C ((t (:foreground "White" :background "Grey"))))

 '(eieio-custom-slot-tag-face ((t (:foreground "light blue"))))
 '(eldoc-highlight-function-argument ((t (:bold t :weight bold))))
 '(epa-field-body ((t (:italic t :foreground "turquoise" :slant italic))))
 '(epa-field-name ((t (:bold t :foreground "PaleTurquoise" :weight bold))))
 '(epa-mark ((t (:bold t :foreground "orange" :weight bold))))
 '(epa-string ((t (:foreground "lightyellow"))))
 '(epa-validity-disabled ((t (:italic t :slant italic))))
 '(epa-validity-high ((t (:bold t :foreground "PaleTurquoise" :weight bold))))
 '(epa-validity-low ((t (:italic t :slant italic))))
 '(epa-validity-medium ((t (:italic t :foreground "PaleTurquoise" :slant italic))))

 '(escape-glyph ((t (:foreground "cyan"))))

 '(eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))
 '(eshell-ls-backup-face ((t (:foreground "Grey"))))
 '(eshell-ls-clutter-face ((t (:foreground "DimGray"))))
 '(eshell-ls-directory-face ((t (:bold t :foreground "MediumSlateBlue"))))
 '(eshell-ls-executable-face ((t (:foreground "Coral"))))
 '(eshell-ls-missing-face ((t (:foreground "black"))))
 '(eshell-ls-picture-face ((t (:foreground "Violet"))))
 '(eshell-ls-product-face ((t (:foreground "sandybrown"))))
 '(eshell-ls-readonly-face ((t (:foreground "Aquamarine"))))
 '(eshell-ls-special-face ((t (:foreground "Gold"))))
 '(eshell-ls-symlink-face ((t (:foreground "DarkCyan" :bold t))))
 '(eshell-ls-symlink-face ((t (:foreground "White"))))
 '(eshell-ls-unreadable-face ((t (:foreground "DimGray"))))
 '(eshell-prompt-face ((t (:foreground "MediumAquamarine"))))
 '(eshell-test-failed-face ((t (:foreground "OrangeRed" :bold t))))
 '(eshell-test-ok-face ((t (:foreground "Green" :bold t))))

 '(excerpt ((t (:italic t))))
 '(file-name-shadow ((t (:foreground "grey70"))))
 '(fixed ((t (:bold t))))
 '(fixed-pitch ((t (:family "courier"))))
 '(flyspell-duplicate-face ((t (:foreground "IndianRed" :bold t :underline t))))
 '(flyspell-incorrect-face ((t (:foreground "Pink" :bold t :underline t))))

 '(fringe ((t (:background "grey30" :foreground "Wheat"))))
 '(header-line ((t (:box (:line-width -1 :color "grey20" :style released-button) :background "grey20" :foreground "grey90"  :height 0.9))))
 '(help-argument-name ((t (:italic t :slant italic))))
 '(highlight ((t (:background "gray10" :foreground "Old Lace"))))
 '(hl-line  ((t (:background "grey10" :foreground "Old Lace"))))
 '(gnus-mouse-face ((t (:background "darkseagreen2" :foreground "blue"))))
 '(erc-button-mouse-face ((t (:background "darkseagreen2" :foreground "blue"))))
 '(align-highlight-change-face  ((t (:background "darkseagreen2" :foreground "blue"))))
 '(goto-address-url-mouse-face ((t (:background "darkseagreen2" :foreground "blue"))))
 '(goto-address-url-mouse-face ((t (:background "darkseagreen2" :foreground "blue"))))
 '(ispell-highlight-face ((t (:background "darkseagreen2" :foreground "blue"))))
 '(ispell-highlight-face ((t (:background "darkseagreen2" :foreground "blue"))))
 '(widget-mouse-face  ((t (:background "darkseagreen2" :foreground "blue"))))

 '(highlight-beyond-fill-column-face ((t (:underline t))))
 '(highlight-changes ((t (:foreground nil :background "#382f2f"))))
 '(highlight-changes-delete ((t (:foreground nil :background "#916868"))))

 '(holiday ((t (:background "chocolate4"))))
 '(holiday-face ((t (:background "chocolate4"))))

 '(ibuffer-dired-buffer-face ((t (:foreground "mediumspringgreen" :weight bold :height 1.1))))
 '(ibuffer-help-buffer-face  ((t (:italic t :slant oblique :foreground "chocolate1"))))
 '(ibuffer-hidden-buffer-face  ((t (:bold t :foreground "Pink" :weight bold))))
 '(ibuffer-occur-match-face ((t (:bold t :foreground "Pink" :weight bold))))
 '(ibuffer-read-only-buffer-face ((t (:foreground "SteelBlue1"))))
 '(ibuffer-special-buffer-face ((t (:foreground "SteelBlue1"))))

 '(ido-first-match ((t (:bold t :weight bold))))
 '(ido-incomplete-regexp ((t (:bold t :weight bold :foreground "Pink"))))
 '(ido-indicator ((t (:background "red1" :foreground "yellow1" :width condensed))))
 '(ido-only-match ((t (:foreground "ForestGreen"))))
 '(ido-subdir ((t (:foreground "red1"))))
 '(info-menu-5 ((t (:underline t))))
 '(info-menu-header ((t (:bold t :family "helv" :weight bold))))
 '(info-node ((t (:bold t :italic t :foreground "yellow"))))
 '(info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
 '(info-xref ((t (:bold t :foreground "DodgerBlue1"))))
 '(info-xref ((t (:bold t :foreground "cyan" :weight bold))))
 '(isearch ((t (:background "palevioletred2" :foreground "brown4"))))
 '(isearch-fail ((t (:background "red4"))))
 '(isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))
 '(isearch-secondary ((t (:foreground "red3"))))
 '(italic ((t (:italic t))))

 '(js2-builtin-face ((t (:foreground "sandy brown"))))
 '(js2-comment-face ((t (:foreground "dark orchid"))))
 '(js2-constant-face ((t (:foreground "pale violet red"))))
 '(js2-error-face ((t (:background "indian red" :foreground "green" :bold t))))
 '(js2-function-name-face ((t (:foreground "cadet blue"))))
 '(js2-function-param-face ((t (:foreground "IndianRed1"))))
 '(js2-instance-member-face ((t (:foreground "IndianRed1"))))
 '(js2-jsdoc-tag-face ((t (:foreground "medium orchid"))))
 '(js2-jsdoc-type-face ((t (:foreground "medium orchid"))))
 '(js2-jsdoc-value-face ((t (:foreground "medium orchid"))))
 '(js2-keyword-face ((t (:foreground "steel blue"))))
 '(js2-private-function-call-face ((t (:foreground "cadet blue"))))
 '(js2-private-member-face ((t (:foreground "IndianRed1"))))
 '(js2-regexp-face ((t (:foreground "khaki"))))
 '(js2-string-face ((t (:foreground "lemon chiffon"))))
 '(js2-type-face ((t (:foreground "medium sea green"))))
 '(js2-variable-name-face ((t (:foreground "IndianRed1"))))
 '(js2-warning-face ((t (:background "indian red" :foreground "green"))))

 '(lazy-highlight ((t (:background "paleturquoise4"))))
 '(link ((t (:foreground "cyan1" :underline t))))
 '(link-visited ((t (:underline t :foreground "violet"))))

 '(makefile-space ((t (:background "hotpink"))))
 '(man-bold ((t (:bold t))))
 '(man-heading ((t (:bold t))))
 '(man-italic ((t (:foreground "yellow"))))
 '(man-xref ((t (:underline t))))
 '(match ((t (:background "RoyalBlue3"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(mode-line ((t (:background "grey75" :foreground "Blue"
                  :box '(:line-width -1 :style released-button)
                  :height 0.9))))
 '(mode-line-buffer-id ((t (:background "grey65" :foreground "red"
                                        :bold t :weight bold :height 0.9))))
 '(mode-line-emphasis ((t (:bold t :weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40"
                                              :style released-button :height 0.9)))))
 '(mode-line-inactive ((t (:background "grey30" :foreground "grey80"
                                       :box '(:line-width -1 :color "grey40")
                                       :weight light :height 0.9))))
 '(mouse ((t (:background "OrangeRed"))))

 '(next-error ((t (:background "blue3"))))
 '(nobreak-space ((t (:foreground "cyan" :underline t))))
 '(paren-blink-off ((t (:foreground "black"))))
 '(paren-mismatch-face ((t (:bold t :background "white" :foreground "red"))))
 '(paren-no-match-face ((t (:bold t :background "white" :foreground "red"))))
 '(query-replace ((t (:foreground "brown4" :background "palevioletred2"))))
 '(region ((t (:background "blue3"))))
 '(scroll-bar ((t (:background "grey75" :foreground "WhiteSmoke"))))
 '(secondary-selection ((t (:background "SkyBlue4"))))
 '(semantic-dirty-token-face ((t (:background "lightyellow"))))
 '(semantic-highlight-edits-face ((t (:background "gray20"))))
 '(semantic-unmatched-syntax-face ((t (:underline "red"))))
 '(senator-intangible-face ((t (:foreground "gray75"))))
 '(senator-momentary-highlight-face ((t (:background "gray30"))))
 '(senator-read-only-face ((t (:background "#664444"))))
 '(sgml-doctype-face ((t (:foreground "orange"))))
 '(sgml-end-tag-face ((t (:foreground "greenyellow"))))
 '(sgml-entity-face ((t (:foreground "gold"))))
 '(sgml-ignored-face ((t (:foreground "gray20" :background "gray60"))))
 '(sgml-sgml-face ((t (:foreground "yellow"))))
 '(sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))
 '(shadow ((t (:foreground "grey70"))))

 '(show-paren-match ((t (:background "steelblue3"))))
 '(show-paren-match-face ((t (:background "steelblue3"))))
 '(show-paren-mismatch ((t (:background "purple" :foreground "white"))))
 '(smerge-base ((t (:foreground "orange"))))
 '(smerge-markers ((t (:background "grey30"))))
 '(smerge-mine ((t (:foreground "cyan"))))
 '(smerge-other ((t (:foreground "lightgreen"))))
 '(smerge-refined-change ((t (:background "blue4"))))
 '(speedbar-button-face ((t (:foreground "green3"))))
 '(speedbar-directory-face ((t (:foreground "light blue"))))
 '(speedbar-file-face ((t (:foreground "cyan"))))
 '(speedbar-highlight-face ((t (:background "sea green"))))
 '(speedbar-selected-face ((t (:foreground "red" :underline t))))
 '(speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))
 '(speedbar-tag-face ((t (:foreground "yellow"))))
 '(table-cell ((t (:background "blue1" :foreground "gray90"))))

 '(tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(tooltip ((t (:family "helv" :background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red1"))))
 '(underline ((t (:underline t))))
 '(variable-pitch ((t (:family "helv"))))
 '(vcursor ((t (:foreground "blue" :background "cyan" :underline t))))
 '(vertical-border ((t (:background "dim gray"))))
 '(vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))
 '(vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))
 '(vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))
 '(vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))
 '(vhdl-font-lock-prompt-face ((t (:foreground "Red" :bold t))))
 '(vhdl-font-lock-reserved-words-face ((t (:foreground "Orange" :bold t))))
 '(vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))
 '(vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))
 '(vhdl-speedbar-architecture-selected-face ((t (:foreground "Blue" :underline t))))
 '(vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))
 '(vhdl-speedbar-configuration-selected-face ((t (:foreground "DarkGoldenrod" :underline t))))
 '(vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))
 '(vhdl-speedbar-entity-selected-face ((t (:foreground "ForestGreen" :underline t))))
 '(vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))
 '(vhdl-speedbar-instantiation-selected-face ((t (:foreground "Brown" :underline t))))
 '(vhdl-speedbar-package-face ((t (:foreground "Grey50"))))
 '(vhdl-speedbar-package-selected-face ((t (:foreground "Grey50" :underline t))))

 '(viper-minibuffer-emacs-face ((t (:foreground "Black" :background "darkseagreen2"))))
 '(viper-minibuffer-insert-face ((t (:foreground "Black" :background "pink"))))
 '(viper-minibuffer-vi-face ((t (:foreground "DarkGreen" :background "grey"))))
 '(viper-replace-overlay-face ((t (:foreground "Black" :background "darkseagreen2"))))
 '(viper-search-face ((t (:foreground "Black" :background "khaki"))))
 '(vm-highlight-url-face  ((t (:bold t :italic t :slant italic :weight bold))))
 '(vm-highlighted-header-face  ((t (:bold t :weight bold))))
 '(vm-mime-button-face  ((t (:background "grey75" :foreground "black" :box (:line-width 2 :style released-button)))))
 '(vm-summary-highlight-face  ((t (:bold t :weight bold))))
 '(vm-xface ((t (:background "white" :foreground "black"))))

 '(which-func ((t (:foreground "Blue1"))))
 '(widget ((t (:height 1.2 :background "Gray80" :foreground "black"))))
 '(widget-button ((t (:bold t :weight bold :box (:line-width 2 :style released-button)))))
 '(widget-button-face ((t (:bold t :weight bold :box (:line-width 2 :style released-button)))))
 '(widget-button-pressed ((t (:foreground "red1" :background "lightgrey" :box (:line-width 2 :style pressed-button)))))
 '(widget-button-pressed-face ((t (:foreground "red1" :background "lightgrey" :box (:line-width 2 :style pressed-button)))))
 '(widget-documentation ((t (:foreground "lime green"))))
 '(widget-documentation-face ((t (:foreground "lime green"))))
 '(widget-field ((t (:background "dim gray"))))
 '(widget-field-face ((t (:background "dim gray"))))
 '(widget-inactive ((t (:foreground "grey70"))))
 '(widget-inactive-face ((t (:foreground "grey70"))))
 '(widget-single-line-field ((t (:background "dim gray"))))
 '(widget-single-line-field-face ((t (:background "dim gray"))))
 '(woman-bold-face ((t (:bold t))))
 '(woman-italic-face ((t (:foreground "beige"))))
 '(woman-unknown-face ((t (:foreground "LightSalmon")))))

(provide-theme 'manoj-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; manoj-dark.el ends here
