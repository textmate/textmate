;;; idlw-toolbar.el --- a debugging toolbar for IDLWAVE

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@astro.uva.nl>
;; Maintainer: J.D. Smith <jdsmith@as.arizona.edu>
;; Version: 6.1.22
;; Keywords: processes
;; Package: idlwave

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

;; This file implements a debugging toolbar for IDLWAVE.  It requires
;; Emacs or XEmacs with toolbar and xpm support.

;; New versions of IDLWAVE, documentation, and more information
;; available from:
;;                 http://idlwave.org


;;; Code:

(eval-when-compile (require 'cl))

(defun idlwave-toolbar-make-button (image)
  (if (featurep 'xemacs)
      (toolbar-make-button-list image)
    (list 'image :type 'xpm :data image)))

(defvar idlwave-toolbar)
(defvar default-toolbar)
(defvar idlwave-toolbar-is-possible)

(if (not (or (and (featurep 'xemacs)                ; This is XEmacs
		  (featurep 'xpm)                   ; need xpm
		  (featurep 'toolbar))              ; ... and the toolbar
	     (and (not (featurep 'xemacs))          ; This is Emacs
		  (boundp 'tool-bar-button-margin)   ; need toolbar
		  (fboundp 'image-type-available-p) ; need image stuff
		  (image-type-available-p 'xpm))    ; need xpm
	     ))
    ;; oops - cannot do the toolbar
    (message "Sorry, IDLWAVE xpm toolbar cannot be used on this version of Emacs")
;; OK, we can define a toolbar

(defconst idlwave-toolbar-is-possible t
  "When defined, indicates that a toolbar is possible with this Emacs.")
(defvar idlwave-toolbar-compile-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 2 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"                            \",
\"                            \",
\"                            \",
\"             .              \",
\"       ..   ...   ..        \",
\"       .... ... ....        \",
\"       .............        \",
\"        ...........         \",
\"    ...................     \",
\"    ........   ........     \",
\"     .....    ........      \",
\"      ....  .........       \",
\"    .....  .. ... .....     \",
\"   ...... ..   .. ......    \",
\"    ..... ... ..  .....     \",
\"      .........  ....       \",
\"     ........    .....      \",
\"    ........   ........     \",
\"    ...................     \",
\"        ...........         \",
\"       .............        \",
\"       .... ... ....        \",
\"       ..   ...   ..        \",
\"             .              \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The compile icon.")

(defvar idlwave-toolbar-next-error-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 3 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"R	c #FFFF00000000\",
\"                            \",
\"                            \",
\"             R              \",
\"       RR   RRR   RR        \",
\"       RRRR RRR RRRR        \",
\"       RRRRRRRRRRRRR        \",
\"        RRRRRRRRRRR         \",
\"    RRRRRRRRRRRRRRRRRRR     \",
\"    RRRRRRRR                \",
\"     RRRRR                  \",
\"      RRRR                  \",
\"               ........     \",
\"              ........      \",
\"            .........       \",
\"    .....  .. ... .....     \",
\"   ...... ..   .. ......    \",
\"    ..... ... ..  .....     \",
\"      .........  ....       \",
\"     ........    .....      \",
\"    ........   ........     \",
\"    ...................     \",
\"        ...........         \",
\"       .............        \",
\"       .... ... ....        \",
\"       ..   ...   ..        \",
\"             .              \",
\"                            \",
\"                            \"};")
  "The Next Error icon.")

(defvar idlwave-toolbar-stop-at-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 3 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"R	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"          ........          \",
\"         .RRRRRRRR.         \",
\"        .RRRRRRRRRR.        \",
\"       .RRRRRRRRRRRR.       \",
\"      .RRRRRRRRRRRRRR.      \",
\"     .RRRRRRRRRRRRRRRR.     \",
\"    .RRRRRRRRRRRRRRRRRR.    \",
\"    .RRRRRRRRRRRRRRRRRR.    \",
\"    .RRRRRRRRRRRRRRRRRR.    \",
\"    .RRRRRRRRRRRRRRRRRR.    \",
\"    .RRRRRRRRRRRRRRRRRR.    \",
\"    .RRRRRRRRRRRRRRRRRR.    \",
\"    .RRRRRRRRRRRRRRRRRR.    \",
\"    .RRRRRRRRRRRRRRRRRR.    \",
\"     .RRRRRRRRRRRRRRRR.     \",
\"      .RRRRRRRRRRRRRR.      \",
\"       .RRRRRRRRRRRR.       \",
\"        .RRRRRRRRRR.        \",
\"         .RRRRRRRR.         \",
\"          ........          \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Stop At icon.")


(defvar idlwave-toolbar-clear-at-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 3 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"R	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"  ...                  ...  \",
\"   ...    ........    ...   \",
\"    ...  .RRRRRRRR.  ...    \",
\"     ....RRRRRRRRRR....     \",
\"      ...RRRRRRRRRR...      \",
\"      ....RRRRRRRR....      \",
\"     .RR...RRRRRR...RR.     \",
\"    .RRRR...RRRR...RRRR.    \",
\"    .RRRRR...RR...RRRRR.    \",
\"    .RRRRRR......RRRRRR.    \",
\"    .RRRRRRR....RRRRRRR.    \",
\"    .RRRRRRR....RRRRRRR.    \",
\"    .RRRRRR......RRRRRR.    \",
\"    .RRRRR...RR...RRRRR.    \",
\"    .RRRR...RRRR...RRRR.    \",
\"     .RR...RRRRRR...RR.     \",
\"      ....RRRRRRRR....      \",
\"      ...RRRRRRRRRR...      \",
\"     ....RRRRRRRRRR....     \",
\"    ...  .RRRRRRRR.  ...    \",
\"   ...    ........    ...   \",
\"  ...                  ...  \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Clear At icon.")

(defvar idlwave-toolbar-clear-all-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 4 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"X	c #FFFFFFFFFFFF\",
\"R	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"  .. ....         .... ..   \",
\"  ...RRRR.       .RRRR...   \",
\"   ...RRRR.     .RRRR...    \",
\"  .R...RRRR.   .RRRR...R.   \",
\"  .RR...RRR.   .RRR...RR.   \",
\"  .RRR...RR.   .RR...RRR.   \",
\"  .RRRR...R.   .R...RRRR.   \",
\"   .RRRR...     ...RRRR.    \",
\"    .RRRR...   ...RRRR.     \",
\"     .... ... ... ....      \",
\"           .....            \",
\"            ...             \",
\"     ....  .....  ....      \",
\"    .RRRR.... ....RRRR.     \",
\"   .RRRRR...   ...RRRRR.    \",
\"  .RRRRR....   ....RRRRR.   \",
\"  .RRRR...R.   .R...RRRR.   \",
\"  .RRR...RR.   .RR...RRR.   \",
\"  .RR...RRR.   .RRR...RR.   \",
\"   ....RRR.     .RRR....    \",
\"   ...RRR.       .RRR...    \",
\"  .......         .......   \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Clear-All icon.")

(defvar idlwave-toolbar-stop-beginning-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 4 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"X	c #FFFF00000000\",
\"_	c #FFFFFFFFFFFF\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"          ........          \",
\"         .XXXXXXXX.         \",
\"        .XXXXXXXXXX.        \",
\"       .XXXXXXXXXXXX.       \",
\"      .XX..XXXXXXXXXX.      \",
\"     .XX.XX.X______XXX.     \",
\"    .XXX.XX.X______XXXX.    \",
\"    .XXXX..XXXXXXXXXXXX.    \",
\"    .XXXXXXXXXX____XXXX.    \",
\"    .XXXXXXXXXX____XXXX.    \",
\"    .XXXXXXXXXXXXXXXXXX.    \",
\"    .XXXXXXXXXX____XXXX.    \",
\"    .XXXXXXXXXX____XXXX.    \",
\"    .XXXXXXXXXXXXXXXXXX.    \",
\"     .XXXXXXXXX____XXX.     \",
\"      .XXXXXXXX____XX.      \",
\"       .XXXXXXXXXXXX.       \",
\"        .XXXXXXXXXX.        \",
\"         .XXXXXXXX.         \",
\"          ........          \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Stop at Beginning icon.")

(defvar idlwave-toolbar-stop-in-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 4 1\",
\" 	c None s backgroundToolBarColor\",
\"_	c #FFFFFFFFFFFF\",
\".	c #000000000000\",
\"R	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"          ........          \",
\"         .RRRRRRRR.         \",
\"        .RRRRRRRRRR.        \",
\"       .RRRRRRRRRRRR.       \",
\"      .RRR___RR___RRR.      \",
\"     .RRRR__RRRR__RRRR.     \",
\"    .RRRRR__RRRR__RRRRR.    \",
\"    .RRRRR__RRRR__RRRRR.    \",
\"    .RRRRR__RRRR__RRRRR.    \",
\"    .RRRR__RRRRRR__RRRR.    \",
\"    .RRRRR__RRRR__RRRRR.    \",
\"    .RRRRR__RRRR__RRRRR.    \",
\"    .RRRRR__RRRR__RRRRR.    \",
\"    .RRRRR__RRRR__RRRRR.    \",
\"     .RRRR___RR___RRRR.     \",
\"      .RRRRRRRRRRRRRR.      \",
\"       .RRRRRRRRRRRR.       \",
\"        .RRRRRRRRRR.        \",
\"         .RRRRRRRR.         \",
\"          ........          \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Stop in icon.")

(defvar idlwave-toolbar-edit-cmd-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 2 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"   ..                       \",
\"    ..                      \",
\"     ..                     \",
\"    ..                      \",
\"   ..                       \",
\"                            \",
\"                            \",
\"        .................   \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The edit-cmd icon")

(defvar idlwave-toolbar-run-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 3 1\",
\" 	c None s backgroundToolBarColor\",
\".	s FgColor c #000000000000\",
\"G	c #0000BBBB0000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"     .....                  \",
\"     .GGG.                  \",
\"     .GGG.                  \",
\"     .GGG.       .......    \",
\"     .GGG.                  \",
\"     .GGG.                  \",
\"     .GGG.       .......    \",
\"     .GGG.                  \",
\"  ....GGG....               \",
\"   .GGGGGGG.     .......    \",
\"    .GGGGG.                 \",
\"     .GGG.                  \",
\"      .G.        .......    \",
\"       .                    \",
\"                            \",
\"                 .......    \",
\"                            \",
\"                            \",
\"                 .......    \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Run icon.")

(defvar idlwave-toolbar-cont-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 3 1\",
\" 	c None s backgroundToolBarColor\",
\".	s FgColor c #000000000000\",
\"G	c #0000BBBB0000\",
\"                            \",
\"                            \",
\"                            \",
\"                 .......    \",
\"                            \",
\"       .......              \",
\"      .GGGGGG.   .......    \",
\"     .GGGGGGG.              \",
\"     .GGG.....              \",
\"     .GGG.       .......    \",
\"     .GGG.                  \",
\"     .GGG.                  \",
\"     .GGG.       .......    \",
\"     .GGG.                  \",
\"  ....GGG....               \",
\"   .GGGGGGG.     .......    \",
\"    .GGGGG.                 \",
\"     .GGG.                  \",
\"      .G.        .......    \",
\"       .                    \",
\"                            \",
\"                 .......    \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Cont icon.")	  

(defvar idlwave-toolbar-to-here-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 4 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"G	c #0000BBBB0000\",
\"R	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"      .....   ........      \",
\"     .GGGG.                 \",
\"    .GGGGG.                 \",
\"    .GG....   ........      \",
\"    .GG.                    \",
\"    .GG. .                  \",
\"    .GG. ..                 \",
\"    .GG. .G.    ......      \",
\"    .GG...GG.               \",
\"    .GGGGGGGG.  RRRRRR      \",
\"    .GGGGGGGGG. RRRRRR      \",
\"     .GGGGGGG.  RRRRRR      \",
\"      ....GG.               \",
\"         .G.    ......      \",
\"         ..                 \",
\"         .                  \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Cont-to-here icon.")

(defvar idlwave-toolbar-step-over-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 3 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"G	c #0000BBBB0000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"      .....                 \",
\"     .GGGG.      .......    \",
\"    .GGGGG.                 \",
\"    .GG....                 \",
\"    .GG.         .......    \",
\"    .GG. .                  \",
\"    .GG. ..                 \",
\"    .GG. .G.     .......    \",
\"    .GG...GG.               \",
\"    .GGGGGGGG.              \",
\"    .GGGGGGGGG.  .......    \",
\"     .GGGGGGG.              \",
\"      ....GG.               \",
\"         .G.     .......    \",
\"         ..                 \",
\"         .                  \",
\"                 .......    \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Step Over icon.")

(defvar idlwave-toolbar-step-into-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 3 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"G	c #0000BBBB0000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"      .....   .......       \",
\"     .GGGG.                 \",
\"    .GGGGG.                 \",
\"    .GG....   ........      \",
\"    .GG.                    \",
\"    .GG. .                  \",
\"    .GG. ..                 \",
\"    .GG. .G.                \",
\"    .GG...GG.    .......    \",
\"    .GGGGGGGG.              \",
\"    .GGGGGGGGG.             \",
\"     .GGGGGGG.   .......    \",
\"      ....GG.               \",
\"         .G.                \",
\"         ..      .......    \",
\"         .                  \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Step Into icon.")

(defvar idlwave-toolbar-step-out-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 3 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"G	c #0000BBBB0000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"         .                  \",
\"         ..     ........    \",
\"         .G.                \",
\"      ....GG.               \",
\"     .GGGGGGG.  ........    \",
\"    .GGGGGGGGG.             \",
\"    .GGGGGGGG.              \",
\"    .GG...GG.   ........    \",
\"    .GG. .G.                \",
\"    .GG. ..                 \",
\"    .GG. .                  \",
\"    .GG.                    \",
\"    .GG.......    .......   \",
\"    .GGGGGGGG.              \",
\"     .GGGGGGG.              \",
\"      ........    .......   \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Step up icon.")


(defvar idlwave-toolbar-eval-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 2 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #000000000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"         ....               \",
\"         .. ..  ......      \",
\"         .. ..  ......      \",
\"         .. ..              \",
\"         .. ..  ......      \",
\"         .. ..  ......      \",
\"         ....               \",
\"         ..                 \",
\"         ..                 \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Evaluate icon.")

(defvar idlwave-toolbar-stack-up-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c None s backgroundToolBarColor\",
\".	s FgColor c #000000000000\",
\"_	c #FFFFFFFFFFFF\",
\"G	c #0000BBBB0000\",
\"R	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"      ........     .        \",
\"      .______.    ...       \",
\"      .______.   .....      \",
\"      .______.  .......     \",
\"      .______.    ...       \",
\"      .______.    ...       \",
\"      ........    ...       \",
\"      .GGGGGG.    ...       \",
\"      .GGGGGG.    ...       \",
\"      .GGGGGG.              \",
\"      .GGGGGG.              \",
\"      .GGGGGG.              \",
\"      ........              \",
\"      .RRRRRR.              \",
\"      .RRRRRR.              \",
\"      .RRRRRR.              \",
\"      .RRRRRR.              \",
\"      .RRRRRR.              \",
\"      ........              \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Stack Up icon.")

(defvar idlwave-toolbar-stack-down-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c None s backgroundToolBarColor\",
\".	s FgColor c #000000000000\",
\"_	c #FFFFFFFFFFFF\",
\"G	c #0000BBBB0000\",
\"R	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"      ........              \",
\"      .______.              \",
\"      .______.              \",
\"      .______.              \",
\"      .______.              \",
\"      .______.              \",
\"      ........              \",
\"      .GGGGGG.              \",
\"      .GGGGGG.              \",
\"      .GGGGGG.              \",
\"      .GGGGGG.    ...       \",
\"      .GGGGGG.    ...       \",
\"      ........    ...       \",
\"      .RRRRRR.    ...       \",
\"      .RRRRRR.    ...       \",
\"      .RRRRRR.  .......     \",
\"      .RRRRRR.   .....      \",
\"      .RRRRRR.    ...       \",
\"      ........     .        \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
  "The Stack Down icon.")

(defvar idlwave-toolbar-reset-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 3 1\",
\" 	c None s backgroundToolBarColor\",
\"G	c #0000BBBB0000\",
\". 	c #000000000000\",
\"                            \",
\"                            \",
\"                            \",
\"     .                      \",
\"    .G.     .               \",
\"   .GGG..  .G.              \",
\"   .GGGGG..GG.              \",
\"    ..GGGGGGGG.             \",
\"      ..GGGGGG.             \",
\"        ..GGGGG.            \",
\"        .GGGGGG.            \",
\"        .G......            \",
\"                            \",
\"                 .....      \",
\"               .........    \",
\"               .........    \",
\"               .........    \",
\"               . ..... .    \",
\"               .       .    \",
\"               .       .    \",
\"               .       .    \",
\"               .       .    \",
\"               .       .    \",
\"               ..     ..    \",
\"                .......     \",
\"                 .....      \",
\"                            \",
\"                            \"};")
  "The Reset icon.")

(defvar idlwave-toolbar-electric-debug-icon
  (idlwave-toolbar-make-button
   "/* XPM */
static char * file[] = {
\"28 28 8 1\",
\" 	c None s backgroundToolBarColor\",
\".	c #CFC854\",
\"+	c #EEDB0E\",
\"@	c #D2C739\",
\"#	c #A39C54\",
\"$	c #CDC020\",
\"%	c #020202\",
\"&	c #D60E36\",
\"                            \",
\"                            \",
\"      ..                    \",
\"   +++++@                   \",
\"   ++++++                   \",
\"    +++++@                  \",
\"     +++++                  \",
\"     #++++@                 \",
\"      $+++@       %% %%     \",
\"     ++++++$       % %      \",
\"     #+++++$       % %      \",
\"      #++$#        %%%      \",
\"      #+++       %%%%%%%    \",
\"       .++     %%%%%%%%%%%  \",
\"        ++$$   %%%%%%%%%%%  \",
\"        .+@#   &&%%%%%%%&&  \",
\"      .++++#  &&&&&%%%&&&&& \",
\"      +++++$  &&%%&&&&&%%&& \",
\"       $+++$  &&%%&&&&&%%&& \",
\"        $++@  &&&&&&%&&&&&& \",
\"         $+@  &&&&&&%&&&&&& \",
\"          @+  &&%%&&&&&%%&& \",
\"           ++. &%%&&%&&%%&  \",
\"            +. &&&&%%%&&&&  \",
\"            .+  &&%%%%%&&   \",
\"             ++  %%%%%%%    \",
\"              .    %%%      \",
\"                            \"};")
  "The electric debug icon.")

(defvar idlwave-toolbar
  '(
    [idlwave-toolbar-compile-icon
     idlwave-shell-save-and-compile
     t
     "Save and Compile this file (or recompile last)"]
    [idlwave-toolbar-next-error-icon
     idlwave-shell-goto-next-error
     t
     "Goto Next Error"]
    [idlwave-toolbar-stop-at-icon
     idlwave-shell-break-here
     (derived-mode-p 'idlwave-mode)
     "Set Breakpoint at selected position"]
    [idlwave-toolbar-clear-at-icon
     idlwave-shell-clear-current-bp
     t
     "Clear Breakpoint at selected position"]
    [idlwave-toolbar-clear-all-icon
     idlwave-shell-clear-all-bp
     t
     "Clear all Breakpoints"]
    [idlwave-toolbar-stop-beginning-icon
     idlwave-shell-break-this-module
     (derived-mode-p 'idlwave-mode)
     "Stop at beginning of enclosing Routine"]
    [idlwave-toolbar-stop-in-icon
     idlwave-shell-break-in
     t
     "Stop in Routine with name near point"]
    [idlwave-toolbar-edit-cmd-icon
     idlwave-shell-edit-default-command-line
     t
     "Edit Default Command Line"]
    [idlwave-toolbar-run-icon
     idlwave-shell-execute-default-command-line
     t
     "Reset, then Execute Default Command Line"]
    [idlwave-toolbar-cont-icon
     idlwave-shell-cont
     t
     "Continue Current Program"]
    [idlwave-toolbar-to-here-icon
     idlwave-shell-to-here
     (derived-mode-p 'idlwave-mode)
     "Continue to Here (cursor position)"]
    [idlwave-toolbar-step-over-icon
     idlwave-shell-stepover
     t
     "Step Over (aka next)"]
    [idlwave-toolbar-step-into-icon
     idlwave-shell-step
     t
     "Step Into (aka step)"]
    [idlwave-toolbar-step-out-icon
     idlwave-shell-out
     t
     "Step Out (of subroutine)"]
    [idlwave-toolbar-eval-icon
     idlwave-shell-print
     t
     "Print Expression at or before Point"]
    [idlwave-toolbar-stack-up-icon
     idlwave-shell-stack-up
     t
     "Stack Up (towards \"cooler\" - less recently visited - frames)"]
    [idlwave-toolbar-stack-down-icon
     idlwave-shell-stack-down
     t
     "Stack Down (towards \"warmer\" - more recently visited - frames)"]
    [idlwave-toolbar-reset-icon
     idlwave-shell-reset
     t
     "Reset IDL (RETALL & CLOSE,/ALL and more)"]
    [idlwave-toolbar-electric-debug-icon
     idlwave-shell-electric-debug-mode
     (derived-mode-p 'idlwave-mode)
     "Toggle Electric Debug Mode"]
    ))

;; When the shell exits, arrange to remove the special toolbar everywhere.
(add-hook 'idlwave-shell-cleanup-hook
	  'idlwave-toolbar-remove-everywhere)
);; End can define toolbar

(defun idlwave-toolbar-add ()
  "Add the IDLWAVE toolbar if appropriate."
  (if (and (featurep 'xemacs)    ; This is a noop on Emacs
	   (boundp 'idlwave-toolbar-is-possible)
	   (derived-mode-p 'idlwave-mode 'idlwave-shell-mode))
      (set-specifier default-toolbar (cons (current-buffer)
					   idlwave-toolbar))))

(defun idlwave-toolbar-remove ()
  "Add the IDLWAVE toolbar if appropriate."
  (if (and (featurep 'xemacs)    ; This is a noop on Emacs
	   (boundp 'idlwave-toolbar-is-possible)
           (derived-mode-p 'idlwave-mode 'idlwave-shell-mode))
      (remove-specifier default-toolbar (current-buffer))))

(defvar idlwave-shell-mode-map)
(defvar idlwave-mode-map)
(defvar idlwave-toolbar-visible nil)
(defun idlwave-toolbar-add-everywhere ()
  "Add the toolbar in all appropriate buffers."
  (when (boundp 'idlwave-toolbar-is-possible)

    ;; First make sure new buffers will get the toolbar
    (add-hook 'idlwave-mode-hook 'idlwave-toolbar-add)
    ;; Then add it to all existing buffers
    (if (featurep 'xemacs)
	;; For XEmacs, map over all buffers to add toolbar
	(save-excursion
	  (mapcar (lambda (buf)
		    (set-buffer buf)
		    (idlwave-toolbar-add))
		  (buffer-list)))
      ;; For Emacs, add the key definitions to the mode maps
      (mapc (lambda (x)
	      (let* ((icon (aref x 0))
		     (func (aref x 1))
		     (show (aref x 2))
		     (help (aref x 3))
		     (key (vector 'tool-bar func))
		     (def (list 'menu-item
				"a"
				func
				:image (symbol-value icon)
				:visible show
				:help help)))
		(define-key idlwave-mode-map key def)
		(define-key idlwave-shell-mode-map key def)))
	    (reverse idlwave-toolbar)))
    (setq idlwave-toolbar-visible t)))

(defun idlwave-toolbar-remove-everywhere ()
  "Remove the toolbar in all appropriate buffers."
  ;; First make sure new buffers won't get the toolbar
  (when idlwave-toolbar-is-possible
    (remove-hook 'idlwave-mode-hook 'idlwave-toolbar-add)
    ;; Then remove it in all existing buffers.
    (if (featurep 'xemacs)
	;; For XEmacs, map over all buffers to remove toolbar
	(save-excursion
	  (mapcar (lambda (buf)
		    (set-buffer buf)
		    (idlwave-toolbar-remove))
		  (buffer-list)))
      ;; For Emacs, remove the key definitions from the mode maps
      (mapc (lambda (x)
	      (let* (;;(icon (aref x 0))
		     (func (aref x 1))
		     ;;(show (aref x 2))
		     ;;(help (aref x 3))
		     (key (vector 'tool-bar func)))
		(define-key idlwave-mode-map key nil)
		(define-key idlwave-shell-mode-map key nil)))
	    idlwave-toolbar))
    (setq idlwave-toolbar-visible nil)))

(defun idlwave-toolbar-toggle (&optional force-on)
  (interactive)
  (if idlwave-toolbar-visible
      (or force-on (idlwave-toolbar-remove-everywhere))
    (idlwave-toolbar-add-everywhere))
  ;; Now make sure this
  (if (featurep 'xemacs)
      nil ; no action necessary, toolbar gets updated automatically
    ;; On Emacs, redraw the frame to make sure the Toolbar is updated.
    (redraw-frame (selected-frame))))

(provide 'idlw-toolbar)
(provide 'idlwave-toolbar)

;;; idlw-toolbar.el ends here
