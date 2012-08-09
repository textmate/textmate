;;; bubbles.el --- Puzzle game for Emacs

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; URL:         http://ulf.epplejasper.de/
;; Created:     5. Feb. 2007
;; Keywords:    games

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

;; Bubbles is a puzzle game.  Its goal is to remove as many bubbles as
;; possible in as few moves as possible.

;; Bubbles is an implementation of the "Same Game", similar to "Same
;; GNOME" and many others, see <http://en.wikipedia.org/wiki/SameGame>.

;; Installation
;; ------------

;; Add the following lines to your Emacs startup file (`~/.emacs').
;; (add-to-list 'load-path "/path/to/bubbles/")
;; (autoload 'bubbles "bubbles" "Play Bubbles" t)

;; ======================================================================

;;; History:

;; 0.5 (2007-09-14)
;;     - Minor bugfixes.

;; 0.4 (2007-08-27)
;;     - Allow for undoing last move.
;;     - Bonus for removing all bubbles.
;;     - Speed improvements.
;;     - Animation enhancements.
;;     - Added `bubbles-mode-hook'.
;;     - Fixes: Don't move point.
;;     - New URL.

;; 0.3 (2007-03-11)
;;     - Renamed shift modes and thus names of score files. All
;;       high scores are lost, unless you rename the score files from
;;       bubbles-shift-... to bubbles-...!
;;     - Bugfixes: Check for successful image creation.
;;                 Disable menus and counter when game is over.
;;     Tested with GNU Emacs 22.0.93

;; 0.2 (2007-02-24)
;;     - Introduced game themes.
;;     - Introduced graphics themes (changeable while playing).
;;     - Added menu.
;;     - Customization: grid size, colors, chars, shift mode.
;;     - More keybindings.
;;     - Changed shift direction from to-right to to-left.
;;     - Bugfixes: Don't remove single-bubble regions;
;;                 Animation glitches fixed.
;;     Tested with GNU Emacs 22.0.93 and 21.4.1.

;; 0.1 (2007-02-11)
;;     Initial release. Tested with GNU Emacs 22.0.93 and 21.4.1.

;; ======================================================================

;;; Code:

(defconst bubbles-version "0.5" "Version number of bubbles.el.")

(require 'gamegrid)
(eval-when-compile (require 'cl))       ; for 'case

;; User options

;; Careful with that axe, Eugene! Order does matter in the custom
;; section below.

(defcustom bubbles-game-theme
  'easy
  "Overall game theme.
The overall game theme specifies a grid size, a set of colors,
and a shift mode."
  :type '(radio (const :tag "Easy" easy)
                (const :tag "Medium" medium)
                (const :tag "Difficult" difficult)
                (const :tag "Hard" hard)
                (const :tag "User defined" user-defined))
  :group 'bubbles)

(defun bubbles-set-game-easy ()
  "Set game theme to 'easy'."
  (interactive)
  (setq bubbles-game-theme 'easy)
  (bubbles))

(defun bubbles-set-game-medium ()
  "Set game theme to 'medium'."
  (interactive)
  (setq bubbles-game-theme 'medium)
  (bubbles))

(defun bubbles-set-game-difficult ()
  "Set game theme to 'difficult'."
  (interactive)
  (setq bubbles-game-theme 'difficult)
  (bubbles))

(defun bubbles-set-game-hard ()
  "Set game theme to 'hard'."
  (interactive)
  (setq bubbles-game-theme 'hard)
  (bubbles))

(defun bubbles-set-game-userdefined ()
  "Set game theme to 'user-defined'."
  (interactive)
  (setq bubbles-game-theme 'user-defined)
  (bubbles))

(defgroup bubbles nil
  "Bubbles, a puzzle game."
  :group 'games)

(defcustom bubbles-graphics-theme
  'circles
  "Graphics theme.
It is safe to choose a graphical theme.  If Emacs cannot display
images the `ascii' theme will be used."
  :type '(radio (const :tag "Circles" circles)
                (const :tag "Squares" squares)
                (const :tag "Diamonds" diamonds)
                (const :tag "Balls" balls)
                (const :tag "Emacs" emacs)
                (const :tag "ASCII (no images)" ascii))
  :group 'bubbles)

(defconst bubbles--grid-small '(10 . 10)
  "Predefined small bubbles grid.")

(defconst bubbles--grid-medium '(15 . 10)
  "Predefined medium bubbles grid.")

(defconst bubbles--grid-large '(20 . 15)
  "Predefined large bubbles grid.")

(defconst bubbles--grid-huge '(30 . 20)
  "Predefined huge bubbles grid.")

(defcustom bubbles-grid-size
  bubbles--grid-medium
  "Size of bubbles grid."
  :type `(radio (const :tag "Small" ,bubbles--grid-small)
                (const :tag "Medium" ,bubbles--grid-medium)
                (const :tag "Large" ,bubbles--grid-large)
                (const :tag "Huge" ,bubbles--grid-huge)
                (cons :tag "User defined"
                      (integer :tag "Width")
                      (integer :tag "Height")))
  :group 'bubbles)

(defconst bubbles--colors-2 '("orange" "violet")
  "Predefined bubbles color list with two colors.")

(defconst bubbles--colors-3 '("lightblue" "palegreen" "pink")
  "Predefined bubbles color list with three colors.")

(defconst bubbles--colors-4 '("firebrick" "sea green" "steel blue" "chocolate")
  "Predefined bubbles color list with four colors.")

(defconst bubbles--colors-5 '("firebrick" "sea green" "steel blue"
                              "sandy brown" "bisque3")
  "Predefined bubbles color list with five colors.")

(defcustom bubbles-colors
  bubbles--colors-3
  "List of bubble colors.
The length of this list determines how many different bubble
types are present."
  :type `(radio (const :tag "Red, darkgreen" ,bubbles--colors-2)
                (const :tag "Red, darkgreen, blue" ,bubbles--colors-3)
                (const :tag "Red, darkgreen, blue, orange" ,bubbles--colors-4)
                (const :tag "Red, darkgreen, blue, orange, violet"
                       ,bubbles--colors-5)
                (repeat :tag "User defined" color))
  :group 'bubbles)

(defcustom bubbles-chars
  '(?+ ?O ?# ?X ?. ?* ?& ?§)
  "Characters used for bubbles.
Note that the actual number of different bubbles is determined by
the number of colors, see `bubbles-colors'."
  :type '(repeat character)
  :group 'bubbles)

(defcustom bubbles-shift-mode
  'default
  "Shift mode.
Available modes are `shift-default' and `shift-always'."
  :type '(radio (const :tag "Default" default)
                (const :tag "Shifter" always)
                ;;(const :tag "Mega Shifter" 'mega)
                )
  :group 'bubbles)

(defcustom bubbles-mode-hook nil
  "Hook run by Bubbles mode."
  :group 'bubbles
  :type 'hook)

(defun bubbles-customize ()
  "Open customization buffer for bubbles."
  (interactive)
  (customize-group 'bubbles))

;; ======================================================================
;; internal variables

(defvar bubbles--score 0
  "Current Bubbles score.")

(defvar bubbles--neighbourhood-score 0
  "Score of active bubbles neighborhood.")

(defvar bubbles--faces nil
  "List of currently used faces.")

(defvar bubbles--playing nil
  "Play status indicator.")

(defvar bubbles--empty-image nil
  "Image used for removed bubbles (empty grid cells).")

(defvar bubbles--images nil
  "List of images for bubbles.")

(defvar bubbles--images-ok nil
  "Indicate whether images have been created successfully.")

(defvar bubbles--col-offset 0
  "Horizontal offset for centering the bubbles grid.")

(defvar bubbles--row-offset 0
  "Vertical offset for centering the bubbles grid.")

(defvar bubbles--save-data nil
  "List containing bubbles save data (SCORE BUFFERCONTENTS).")

(defconst bubbles--image-template-circle
  "/* XPM */
static char * dot_xpm[] = {
\"20 20 2 1\",
\" 	c None\",
\".	c #FFFFFF\",
\"       ......       \",
\"     ..........     \",
\"   ..............   \",
\"  ................  \",
\"  ................  \",
\" .................. \",
\" .................. \",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\" .................. \",
\" .................. \",
\"  ................  \",
\"  ................  \",
\"   ..............   \",
\"     ..........     \",
\"       ......       \"};")

(defconst bubbles--image-template-square
  "/* XPM */
static char * dot_xpm[] = {
\"20 20 2 1\",
\"0	c None\",
\"1	c #FFFFFF\",
\"00000000000000000000\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"01111111111111111110\",
\"00000000000000000000\"};")

(defconst bubbles--image-template-diamond
  "/* XPM */
static char * dot_xpm[] = {
\"20 20 2 1\",
\"0	c None\",
\"1	c #FFFFFF\",
\"00000000011000000000\",
\"00000000111100000000\",
\"00000001111110000000\",
\"00000011111111000000\",
\"00000111111111100000\",
\"00001111111111110000\",
\"00011111111111111000\",
\"00111111111111111100\",
\"01111111111111111110\",
\"11111111111111111111\",
\"01111111111111111110\",
\"00111111111111111100\",
\"00011111111111111000\",
\"00001111111111110000\",
\"00000111111111100000\",
\"00000011111111000000\",
\"00000001111110000000\",
\"00000000111100000000\",
\"00000000011000000000\",
\"00000000000000000000\"};")

(defconst bubbles--image-template-emacs
  "/* XPM */
static char * emacs_24_xpm[] = {
\"24 24 129 2\",
\"  	c None\",
\". 	c #837DA4\",
\"+ 	c #807AA0\",
\"@ 	c #9894B2\",
\"# 	c #CCCAD9\",
\"$ 	c #C2C0D2\",
\"% 	c #B6B3C9\",
\"& 	c #A19DB9\",
\"* 	c #8681A5\",
\"= 	c #7D779B\",
\"- 	c #B6B3C7\",
\"; 	c #ABA7BE\",
\"> 	c #9792AF\",
\", 	c #AAA6BD\",
\"' 	c #CBC9D7\",
\") 	c #AAA7BE\",
\"! 	c #908BAA\",
\"~ 	c #797397\",
\"{ 	c #948FAC\",
\"] 	c #9A95B1\",
\"^ 	c #EBEAEF\",
\"/ 	c #F1F1F5\",
\"( 	c #BCB9CB\",
\"_ 	c #A9A5BD\",
\": 	c #757093\",
\"< 	c #918DA9\",
\"[ 	c #DDDBE4\",
\"} 	c #FFFFFF\",
\"| 	c #EAE9EF\",
\"1 	c #A7A4BA\",
\"2 	c #716C8F\",
\"3 	c #8D89A5\",
\"4 	c #9C98B1\",
\"5 	c #DBDAE3\",
\"6 	c #A4A1B7\",
\"7 	c #6E698A\",
\"8 	c #8B87A1\",
\"9 	c #928EA7\",
\"0 	c #C5C3D1\",
\"a 	c #F8F8F9\",
\"b 	c #CCCAD6\",
\"c 	c #A29FB4\",
\"d 	c #6A6585\",
\"e 	c #88849D\",
\"f 	c #B5B2C2\",
\"g 	c #F0F0F3\",
\"h 	c #E1E0E6\",
\"i 	c #A5A2B5\",
\"j 	c #A09DB1\",
\"k 	c #676281\",
\"l 	c #85819A\",
\"m 	c #9591A7\",
\"n 	c #E1E0E5\",
\"o 	c #F0EFF2\",
\"p 	c #B3B0C0\",
\"q 	c #9D9AAE\",
\"r 	c #635F7C\",
\"s 	c #827F96\",
\"t 	c #9997AA\",
\"u 	c #F7F7F9\",
\"v 	c #C8C7D1\",
\"w 	c #89869D\",
\"x 	c #9B99AB\",
\"y 	c #5F5B78\",
\"z 	c #7F7C93\",
\"A 	c #CFCDD6\",
\"B 	c #B7B5C2\",
\"C 	c #9996A9\",
\"D 	c #5C5873\",
\"E 	c #7A778D\",
\"F 	c #F5F5F6\",
\"G 	c #8E8C9E\",
\"H 	c #7D798F\",
\"I 	c #58546F\",
\"J 	c #6C6981\",
\"K 	c #D5D4DB\",
\"L 	c #F5F4F6\",
\"M 	c #9794A5\",
\"N 	c #625F78\",
\"O 	c #79768C\",
\"P 	c #55516A\",
\"Q 	c #605C73\",
\"R 	c #CAC9D1\",
\"S 	c #EAE9EC\",
\"T 	c #B4B3BE\",
\"U 	c #777488\",
\"V 	c #514E66\",
\"W 	c #DEDEE2\",
\"X 	c #F4F4F5\",
\"Y 	c #9D9BA9\",
\"Z 	c #747185\",
\"` 	c #4E4B62\",
\" .	c #DEDDE1\",
\"..	c #A6A5B0\",
\"+.	c #716F81\",
\"@.	c #4A475D\",
\"#.	c #A4A3AE\",
\"$.	c #F4F3F5\",
\"%.	c #777586\",
\"&.	c #6E6C7D\",
\"*.	c #464358\",
\"=.	c #514E62\",
\"-.	c #B9B8C0\",
\";.	c #D1D0D5\",
\">.	c #747282\",
\",.	c #6B6979\",
\"'.	c #434054\",
\").	c #5A5769\",
\"!.	c #D0CFD4\",
\"~.	c #5B5869\",
\"{.	c #696676\",
\"].	c #403D50\",
\"^.	c #DBDADE\",
\"/.	c #F3F3F4\",
\"(.	c #646271\",
\"_.	c #666473\",
\":.	c #3D3A4C\",
\"<.	c #555362\",
\"[.	c #9E9DA6\",
\"}.	c #9E9CA5\",
\"|.	c #646170\",
\"1.	c #393647\",
\"2.	c #514E5D\",
\"3.	c #83818C\",
\"4.	c #A8A7AE\",
\"5.	c #E6E6E8\",
\"6.	c #DAD9DC\",
\"7.	c #353343\",
\"8.	c #32303E\",
\"      . . . . . . . . . . . . . . . . . .       \",
\"  + @ # $ % % % % % % % % % % % % % % & * + +   \",
\"  = - ; > > > > > > > > , ' ) > > > > > > ! =   \",
\"~ ~ { { { { { { { { { { { ] ^ / ( { { { { _ ~ ~ \",
\": : < < < < < < < < < < < < [ } } | < < < 1 : : \",
\"2 2 3 3 3 3 3 3 3 3 3 3 4 5 } } } 5 3 3 3 6 2 2 \",
\"7 7 8 8 8 8 8 8 8 8 9 0 a } } } b 8 8 8 8 c 7 7 \",
\"d d e e e e e e e f g } } } h i e e e e e j d d \",
\"k k l l l l l m n } } } o p l l l l l l l q k k \",
\"r r s s s s t u } } } v w s s s s s s s s x r r \",
\"y y z z z z A } } } B z z z z z z z z z z C y y \",
\"D D D D D D E F } } G D D D D D D D D D D H D D \",
\"I I I I I I I J K } L M N I I I I I I I I O I I \",
\"P P P P P P Q R } } } S T P P P P P P P P U P P \",
\"V V V V V V W } } X Y V V V V V V V V V V Z V V \",
\"` ` ` ` ` `  .} } ..` ` ` ` ` ` ` ` ` ` ` +.` ` \",
\"@.@.@.@.@.@.@.#.$.$.%.@.@.@.@.@.@.@.@.@.@.&.@.@.\",
\"*.*.*.*.*.*.*.*.=.-.} ;.>.*.*.*.*.*.*.*.*.,.*.*.\",
\"'.'.'.'.'.'.'.'.'.'.).!.} !.~.'.'.'.'.'.'.{.'.'.\",
\"].].].].].].].].].].].].^.} /.(.].].].].]._.].].\",
\":.:.:.:.:.:.:.:.:.:.<.[./.} } }.:.:.:.:.:.|.:.:.\",
\"  1.1.1.1.1.1.1.1.2.3.4.5.6.3.1.1.1.1.1.1.1.1.  \",
\"  7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.  \",
\"      8.8.8.8.8.8.8.8.8.8.8.8.8.8.8.8.8.8.      \"};")

(defconst bubbles--image-template-ball
  "/* XPM */
static char * dot3d_xpm[] = {
\"20 20 190 2\",
\"  	c None\",
\". 	c #F9F6F6\",
\"+ 	c #D6D0D0\",
\"@ 	c #BFBBBB\",
\"# 	c #AAA4A4\",
\"$ 	c #ABAAAB\",
\"% 	c #A8A8A8\",
\"& 	c #A29D9D\",
\"* 	c #B5B2B2\",
\"= 	c #CDC9C9\",
\"- 	c #D7D0D0\",
\"; 	c #B3AFAF\",
\"> 	c #B5B5B5\",
\", 	c #B7B7B7\",
\"' 	c #B8B8B8\",
\") 	c #B6B6B6\",
\"! 	c #B3B3B3\",
\"~ 	c #AFAFAF\",
\"{ 	c #A9A9A9\",
\"] 	c #A2A2A2\",
\"^ 	c #9C9A9A\",
\"/ 	c #C9C5C5\",
\"( 	c #FDFBFB\",
\"_ 	c #C3BCBC\",
\": 	c #BBBBBB\",
\"< 	c #C0C0C0\",
\"[ 	c #C3C2C2\",
\"} 	c #C3C3C3\",
\"| 	c #C2C2C2\",
\"1 	c #BEBEBE\",
\"2 	c #B9B9B9\",
\"3 	c #B2B2B2\",
\"4 	c #ABAAAA\",
\"5 	c #999999\",
\"6 	c #ACA7A7\",
\"7 	c #C2BBBB\",
\"8 	c #C5C5C5\",
\"9 	c #CACBCB\",
\"0 	c #CECECE\",
\"a 	c #CFCFCF\",
\"b 	c #CDCDCD\",
\"c 	c #C8C9C9\",
\"d 	c #9F9F9F\",
\"e 	c #959595\",
\"f 	c #A9A5A5\",
\"g 	c #D5CFCE\",
\"h 	c #BDBDBD\",
\"i 	c #C6C6C6\",
\"j 	c #D5D5D5\",
\"k 	c #D9D9D9\",
\"l 	c #DADADA\",
\"m 	c #D8D8D8\",
\"n 	c #D2D2D2\",
\"o 	c #CBCBCB\",
\"p 	c #A4A4A5\",
\"q 	c #9A9A9A\",
\"r 	c #8F8F8F\",
\"s 	c #C3BFBF\",
\"t 	c #AFACAB\",
\"u 	c #CCCCCC\",
\"v 	c #D6D6D6\",
\"w 	c #DEDEDE\",
\"x 	c #E4E4E4\",
\"y 	c #E5E5E5\",
\"z 	c #E2E2E2\",
\"A 	c #DBDBDB\",
\"B 	c #C9C8C8\",
\"C 	c #A8A9A8\",
\"D 	c #9D9E9D\",
\"E 	c #929292\",
\"F 	c #8A8888\",
\"G 	c #D3CECE\",
\"H 	c #B0B0B0\",
\"I 	c #D1D1D1\",
\"J 	c #DCDCDC\",
\"K 	c #E6E6E6\",
\"L 	c #EEEEEE\",
\"M 	c #F1F1F0\",
\"N 	c #EBEBEB\",
\"O 	c #D7D7D8\",
\"P 	c #ABABAB\",
\"Q 	c #A0A0A0\",
\"R 	c #949494\",
\"S 	c #898989\",
\"T 	c #C0BDBD\",
\"U 	c #B9B6B6\",
\"V 	c #B1B1B1\",
\"W 	c #BCBCBC\",
\"X 	c #C8C8C8\",
\"Y 	c #D3D3D3\",
\"Z 	c #DFDFDE\",
\"` 	c #EAEAEA\",
\" .	c #F5F5F5\",
\"..	c #FAFAFA\",
\"+.	c #F1F1F1\",
\"@.	c #CECFCF\",
\"#.	c #ACACAC\",
\"$.	c #A1A1A1\",
\"%.	c #8A8A8A\",
\"&.	c #9B9999\",
\"*.	c #C7C7C7\",
\"=.	c #DDDDDD\",
\"-.	c #E8E8E8\",
\";.	c #F2F2F2\",
\">.	c #898A89\",
\",.	c #7A7878\",
\"'.	c #AEAEAE\",
\").	c #C4C4C4\",
\"!.	c #CBCBCA\",
\"~.	c #AAAAAA\",
\"{.	c #939393\",
\"].	c #888888\",
\"^.	c #7C7C7C\",
\"/.	c #AAAAAB\",
\"(.	c #BFBFBF\",
\"_.	c #C9C9C9\",
\":.	c #DFDEDF\",
\"<.	c #A6A6A6\",
\"[.	c #9B9B9B\",
\"}.	c #909191\",
\"|.	c #858586\",
\"1.	c #797979\",
\"2.	c #989494\",
\"3.	c #A5A6A5\",
\"4.	c #B9B9B8\",
\"5.	c #C1C1C1\",
\"6.	c #CFCFCE\",
\"7.	c #979797\",
\"8.	c #8D8D8D\",
\"9.	c #828282\",
\"0.	c #747171\",
\"a.	c #ADAAAA\",
\"b.	c #A9A8A9\",
\"c.	c #B8B9B9\",
\"d.	c #A5A5A5\",
\"e.	c #9C9C9C\",
\"f.	c #7E7E7D\",
\"g.	c #929191\",
\"h.	c #C9C4C4\",
\"i.	c #989898\",
\"j.	c #ADADAD\",
\"k.	c #9D9D9D\",
\"l.	c #8C8C8C\",
\"m.	c #787878\",
\"n.	c #B8B6B6\",
\"o.	c #939191\",
\"p.	c #A5A5A6\",
\"q.	c #ABABAA\",
\"r.	c #A8A8A9\",
\"s.	c #A3A3A3\",
\"t.	c #858585\",
\"u.	c #757474\",
\"v.	c #C5C1C1\",
\"w.	c #969696\",
\"x.	c #9B9B9C\",
\"y.	c #A4A4A4\",
\"z.	c #9E9E9E\",
\"A.	c #939394\",
\"B.	c #7D7D7D\",
\"C.	c #747474\",
\"D.	c #B7B5B5\",
\"E.	c #A5A1A1\",
\"F.	c #919191\",
\"G.	c #9A9999\",
\"H.	c #838383\",
\"I.	c #757575\",
\"J.	c #939090\",
\"K.	c #A29E9E\",
\"L.	c #868686\",
\"M.	c #8D8D8C\",
\"N.	c #8E8E8E\",
\"O.	c #8D8D8E\",
\"P.	c #8B8C8C\",
\"Q.	c #848485\",
\"R.	c #7F7F80\",
\"S.	c #7A7A7A\",
\"T.	c #737373\",
\"U.	c #929090\",
\"V.	c #828080\",
\"W.	c #818181\",
\"X.	c #808080\",
\"Y.	c #7E7E7E\",
\"Z.	c #737272\",
\"`.	c #B7B4B4\",
\" +	c #BCBABA\",
\".+	c #959494\",
\"++	c #747172\",
\"@+	c #767676\",
\"#+	c #6F6D6D\",
\"$+	c #8F8E8E\",
\"          . + @ # $ % & * = .           \",
\"        - ; > , ' ) ! ~ { ] ^ /         \",
\"    ( _ > : < [ } | 1 2 3 4 ] 5 6 (     \",
\"    7 ) 1 8 9 0 a b c | : 3 { d e f     \",
\"  g ! h i 0 j k l m n o | 2 ~ p q r s   \",
\". t ' | u v w x y z A n B 1 ! C D E F . \",
\"G H : i I J K L M N z O b | ) P Q R S T \",
\"U V W X Y Z `  ...+.y l @.} ' #.$.e %.&.\",
\"& H W *.n =.-.;. .L x k 0 [ , #.Q e >.,.\",
\"] '.2 ).a k z -.` K w j !.< > ~.d {.].^.\",
\"d /.> (._.I k =.:.J v 0 8 : V <.[.}.|.1.\",
\"2.3.~ 4.5._.6.n Y I u i 1 > P $.7.8.9.0.\",
\"a.d b.V c.(.).*.X i | h ) '.d.e.E ].f.g.\",
\"h.i.$.C ~ > 2 W W : ' ! j.d.k.e l.9.m.n.\",
\". o.i.d p.q.'.H V H j.r.s.k.e 8.t.^.u.. \",
\"  v.r w.x.Q s.d.d.y.] z.5 A.8.t.B.C.D.  \",
\"    E.l.F.e i.G.q 5 7.{.r %.H.^.I.J.    \",
\"    ( K.L.%.M.N.N.O.P.S Q.R.S.T.U.(     \",
\"        @ V.W.H.H.9.X.Y.S.I.Z.`.        \",
\"          .  +.+++@+C.#+$+D..           \"};")

;; ======================================================================
;; Functions

(defsubst bubbles--grid-width ()
  "Return the grid width for the current game theme."
  (car (case bubbles-game-theme
         (easy
          bubbles--grid-small)
         (medium
          bubbles--grid-medium)
         (difficult
          bubbles--grid-large)
         (hard
          bubbles--grid-huge)
         (user-defined
          bubbles-grid-size))))

(defsubst bubbles--grid-height ()
  "Return the grid height for the current game theme."
  (cdr (case bubbles-game-theme
         (easy
          bubbles--grid-small)
         (medium
          bubbles--grid-medium)
         (difficult
          bubbles--grid-large)
         (hard
          bubbles--grid-huge)
         (user-defined
          bubbles-grid-size))))

(defsubst bubbles--colors ()
  "Return the color list for the current game theme."
  (case bubbles-game-theme
    (easy
     bubbles--colors-2)
    (medium
     bubbles--colors-3)
    (difficult
     bubbles--colors-4)
    (hard
     bubbles--colors-5)
    (user-defined
     bubbles-colors)))

(defsubst bubbles--shift-mode ()
  "Return the shift mode for the current game theme."
  (case bubbles-game-theme
    (easy
     'default)
    (medium
     'default)
    (difficult
     'always)
    (hard
     'always)
    (user-defined
     bubbles-shift-mode)))

(defun bubbles-save-settings ()
  "Save current customization settings."
  (interactive)
  (custom-set-variables
   (list 'bubbles-game-theme `(quote ,bubbles-game-theme) t)
   (list 'bubbles-graphics-theme `(quote ,bubbles-graphics-theme) t))
  (customize-save-customized))

(defsubst bubbles--empty-char ()
  "The character used for removed bubbles (empty grid cells)."
  ?\s)

(defun bubbles-set-graphics-theme-ascii ()
  "Set graphics theme to `ascii'."
  (interactive)
  (setq bubbles-graphics-theme 'ascii)
  (bubbles--update-faces-or-images))

(defun bubbles-set-graphics-theme-circles ()
  "Set graphics theme to `circles'."
  (interactive)
  (setq bubbles-graphics-theme 'circles)
  (bubbles--initialize-images)
  (bubbles--update-faces-or-images))

(defun bubbles-set-graphics-theme-squares ()
  "Set graphics theme to `squares'."
  (interactive)
  (setq bubbles-graphics-theme 'squares)
  (bubbles--initialize-images)
  (bubbles--update-faces-or-images))

(defun bubbles-set-graphics-theme-diamonds ()
  "Set graphics theme to `diamonds'."
  (interactive)
  (setq bubbles-graphics-theme 'diamonds)
  (bubbles--initialize-images)
  (bubbles--update-faces-or-images))

(defun bubbles-set-graphics-theme-balls ()
  "Set graphics theme to `balls'."
  (interactive)
  (setq bubbles-graphics-theme 'balls)
  (bubbles--initialize-images)
  (bubbles--update-faces-or-images))

(defun bubbles-set-graphics-theme-emacs ()
  "Set graphics theme to `emacs'."
  (interactive)
  (setq bubbles-graphics-theme 'emacs)
  (bubbles--initialize-images)
  (bubbles--update-faces-or-images))

;; game theme menu
(defvar bubbles-game-theme-menu
  (let ((menu (make-sparse-keymap "Game Theme")))
    (define-key menu [bubbles-set-game-userdefined]
      (list 'menu-item "User defined" 'bubbles-set-game-userdefined
            :button '(:radio . (eq bubbles-game-theme 'user-defined))))
    (define-key menu [bubbles-set-game-hard]
      (list 'menu-item "Hard" 'bubbles-set-game-hard
            :button '(:radio . (eq bubbles-game-theme 'hard))))
    (define-key menu [bubbles-set-game-difficult]
      (list 'menu-item "Difficult" 'bubbles-set-game-difficult
            :button '(:radio . (eq bubbles-game-theme 'difficult))))
    (define-key menu [bubbles-set-game-medium]
      (list 'menu-item "Medium" 'bubbles-set-game-medium
            :button '(:radio . (eq bubbles-game-theme 'medium))))
    (define-key menu [bubbles-set-game-easy]
      (list 'menu-item "Easy" 'bubbles-set-game-easy
            :button '(:radio . (eq bubbles-game-theme 'easy))))
    menu)
  "Map for bubbles game theme menu.")

;; graphics theme menu
(defvar bubbles-graphics-theme-menu
  (let ((menu (make-sparse-keymap "Graphics Theme")))
    (define-key menu [bubbles-set-graphics-theme-ascii]
      (list 'menu-item "ASCII" 'bubbles-set-graphics-theme-ascii
            :button '(:radio . (eq bubbles-graphics-theme 'ascii))))
    (define-key menu [bubbles-set-graphics-theme-emacs]
      (list 'menu-item "Emacs" 'bubbles-set-graphics-theme-emacs
            :button '(:radio . (eq bubbles-graphics-theme 'emacs))))
    (define-key menu [bubbles-set-graphics-theme-balls]
      (list 'menu-item "Balls" 'bubbles-set-graphics-theme-balls
            :button '(:radio . (eq bubbles-graphics-theme 'balls))))
    (define-key menu [bubbles-set-graphics-theme-diamonds]
      (list 'menu-item "Diamonds" 'bubbles-set-graphics-theme-diamonds
            :button '(:radio . (eq bubbles-graphics-theme 'diamonds))))
    (define-key menu [bubbles-set-graphics-theme-squares]
      (list 'menu-item "Squares" 'bubbles-set-graphics-theme-squares
            :button '(:radio . (eq bubbles-graphics-theme 'squares))))
    (define-key menu [bubbles-set-graphics-theme-circles]
      (list 'menu-item "Circles" 'bubbles-set-graphics-theme-circles
            :button '(:radio . (eq bubbles-graphics-theme 'circles))))
    menu)
    "Map for bubbles graphics theme menu.")

;; menu
(defvar bubbles-menu
  (let ((menu (make-sparse-keymap "Bubbles")))
    (define-key menu [bubbles-quit]
      (list 'menu-item "Quit" 'bubbles-quit))
    (define-key menu [bubbles]
      (list 'menu-item "New game" 'bubbles))
    (define-key menu [bubbles-separator-1]
      '("--"))
    (define-key menu [bubbles-save-settings]
      (list 'menu-item "Save all settings" 'bubbles-save-settings))
    (define-key menu [bubbles-customize]
      (list 'menu-item "Edit all settings" 'bubbles-customize))
    (define-key menu [bubbles-game-theme-menu]
      (list 'menu-item "Game Theme" bubbles-game-theme-menu))
    (define-key menu [bubbles-graphics-theme-menu]
      (list 'menu-item "Graphics Theme" bubbles-graphics-theme-menu
            :enable 'bubbles--playing))
    (define-key menu [bubbles-separator-2]
      '("--"))
    (define-key menu [bubbles-undo]
      (list 'menu-item "Undo last move" 'bubbles-undo
            :enable '(and bubbles--playing (listp buffer-undo-list))))
    menu)
  "Map for bubbles menu.")

;; bubbles mode map
(defvar bubbles-mode-map
  (let ((map (make-sparse-keymap 'bubbles-mode-map)))
;;    (suppress-keymap map t)
    (define-key map "q" 'bubbles-quit)
    (define-key map "\n" 'bubbles-plop)
    (define-key map " " 'bubbles-plop)
    (define-key map [double-down-mouse-1] 'bubbles-plop)
    (define-key map [mouse-2] 'bubbles-plop)
    (define-key map "\C-m" 'bubbles-plop)
    (define-key map "u" 'bubbles-undo)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "f" 'forward-char)
    (define-key map "b" 'backward-char)
    ;; bind menu to mouse
    (define-key map [down-mouse-3] bubbles-menu)
    ;; Put menu in menu-bar
    (define-key map [menu-bar Bubbles] (cons "Bubbles" bubbles-menu))
    map)
  "Mode map for bubbles.")

(define-derived-mode bubbles-mode nil "Bubbles"
  "Major mode for playing bubbles.
\\{bubbles-mode-map}"
  (setq buffer-read-only t
        show-trailing-whitespace nil)
  (buffer-disable-undo)
  (force-mode-line-update)
  (redisplay)
  (add-hook 'post-command-hook 'bubbles--mark-neighbourhood t t))

;;;###autoload
(defun bubbles ()
  "Play Bubbles game.
\\<bubbles-mode-map>
The goal is to remove all bubbles with as few moves as possible.
\\[bubbles-plop] on a bubble removes that bubble and all
connected bubbles of the same color.  Unsupported bubbles fall
down, and columns that do not contain any bubbles suck the
columns on its right towards the left.

\\[bubbles-set-game-easy] sets the difficulty to easy.
\\[bubbles-set-game-medium] sets the difficulty to medium.
\\[bubbles-set-game-difficult] sets the difficulty to difficult.
\\[bubbles-set-game-hard] sets the difficulty to hard."
  (interactive)
  (switch-to-buffer (get-buffer-create "*bubbles*"))
  (when (or (not bubbles--playing)
            (y-or-n-p "Start new game? "))
    (setq bubbles--save-data nil)
    (setq bubbles--playing t)
    (bubbles--initialize)))

(defun bubbles-quit ()
  "Quit Bubbles."
  (interactive)
  (message "bubbles-quit")
  (bury-buffer))

(declare-function image-size "image.c" (spec &optional pixels frame))

(defun bubbles--compute-offsets ()
  "Update horizontal and vertical offsets for centering the bubbles grid.
Set `bubbles--col-offset' and `bubbles--row-offset'."
  (cond ((and (display-images-p)
              bubbles--images-ok
              (not (eq bubbles-graphics-theme 'ascii))
              (fboundp 'window-inside-pixel-edges))
         ;; compute offset in units of pixels
         (let ((bubbles--image-size
                (car (image-size (car bubbles--images) t))))
           (setq bubbles--col-offset
                 (list
                  (max 0 (/ (- (nth 2 (window-inside-pixel-edges))
                               (nth 0 (window-inside-pixel-edges))
                               (* ( + bubbles--image-size 2) ;; margin
                                  (bubbles--grid-width))) 2))))
           (setq bubbles--row-offset
                 (list
                  (max 0 (/ (- (nth 3 (window-inside-pixel-edges))
                               (nth 1 (window-inside-pixel-edges))
                               (* (+ bubbles--image-size 1) ;; margin
                                  (bubbles--grid-height))) 2))))))
        (t
         ;; compute offset in units of chars
         (setq bubbles--col-offset
               (max 0 (/ (- (window-width)
                            (bubbles--grid-width)) 2)))
         (setq bubbles--row-offset
               (max 0 (/ (- (window-height)
                            (bubbles--grid-height) 2) 2))))))

(defun bubbles--remove-overlays ()
  "Remove all overlays."
  (if (fboundp 'remove-overlays)
      (remove-overlays)))

(defun bubbles--initialize ()
  "Initialize Bubbles game."
  (bubbles--initialize-faces)
  (bubbles--initialize-images)
  (bubbles--remove-overlays)

  (switch-to-buffer (get-buffer-create "*bubbles*"))
  (bubbles--compute-offsets)
  (let ((inhibit-read-only t))
    (set-buffer-modified-p nil)
    (erase-buffer)
    (insert " ")
    (add-text-properties
     (point-min) (point) (list 'intangible t 'display
                               (cons 'space
                                     (list :height bubbles--row-offset))))
    (insert "\n")
    (let ((max-char (length (bubbles--colors))))
      (dotimes (i (bubbles--grid-height))
        (let ((p (point)))
          (insert " ")
          (add-text-properties
           p (point) (list 'intangible t
                           'display (cons 'space
                                          (list :width
                                                bubbles--col-offset)))))
        (dotimes (j (bubbles--grid-width))
          (let* ((index (random max-char))
                 (char (nth index bubbles-chars)))
            (insert char)
            (add-text-properties (1- (point)) (point) (list 'index index))))
        (insert "\n"))
      (insert "\n ")
      (add-text-properties
       (1- (point)) (point) (list 'intangible t 'display
                                  (cons 'space
                                        (list :width bubbles--col-offset)))))
    (put-text-property (point-min) (point-max) 'pointer 'arrow))
  (bubbles-mode)
  (bubbles--reset-score)
  (bubbles--update-faces-or-images)
  (bubbles--goto 0 0)
  (setq buffer-undo-list t)
  (force-mode-line-update)
  (redisplay))

(defun bubbles--initialize-faces ()
  "Prepare faces for playing `bubbles'."
  (copy-face 'default 'bubbles--highlight-face)
  (set-face-background 'bubbles--highlight-face "#8080f4")
  (when (display-color-p)
    (setq bubbles--faces
          (mapcar (lambda (color)
                    (let ((fname (intern (format "bubbles--face-%s" color))))
                      (unless (facep fname)
                        (copy-face 'default fname)
                        (set-face-foreground fname color))
                      fname))
                  (bubbles--colors)))))

(defsubst bubbles--row (pos)
  "Return row of point POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (1- (count-lines (point-min) (point)))))

(defsubst bubbles--col (pos)
  "Return column of point POS."
  (save-excursion
    (goto-char pos)
    (1- (current-column))))

(defun bubbles--goto (row col)
  "Move point to bubble at coordinates ROW and COL."
  (if (or (< row 0)
          (< col 0)
          (>= row (bubbles--grid-height))
          (>= col (bubbles--grid-width)))
      ;; Error! return nil
      nil
    ;; go
    (goto-char (point-min))
    (forward-line (1+ row))
    (forward-char (1+ col))
    (point)))

(defun bubbles--char-at (row col)
  "Return character at bubble ROW and COL."
  (save-excursion
    (if (bubbles--goto row col)
        (char-after (point))
      nil)))

(defun bubbles--mark-direct-neighbours (row col char)
  "Mark direct neighbors of bubble at ROW COL with same CHAR."
  (save-excursion
    (let ((count 0))
      (when (and (bubbles--goto row col)
                 (eq char (char-after (point)))
                 (not (get-text-property (point) 'active)))
        (add-text-properties (point) (1+ (point))
                             '(active t face 'bubbles--highlight-face))
        (setq count (+ 1
                       (bubbles--mark-direct-neighbours row (1+ col) char)
                       (bubbles--mark-direct-neighbours row (1- col) char)
                       (bubbles--mark-direct-neighbours (1+ row) col char)
                       (bubbles--mark-direct-neighbours (1- row) col char))))
      count)))

(defun bubbles--mark-neighbourhood (&optional pos)
  "Mark neighborhood of point.
Use optional parameter POS instead of point if given."
  (when bubbles--playing
    (unless pos (setq pos (point)))
    (condition-case err
        (let ((char (char-after pos))
              (inhibit-read-only t)
              (row (bubbles--row (point)))
              (col (bubbles--col (point))))
          (add-text-properties (point-min) (point-max)
                               '(face default active nil))
          (let ((count 0))
            (when (and row col (not (eq char (bubbles--empty-char))))
              (setq count (bubbles--mark-direct-neighbours row col char))
              (unless (> count 1)
                (add-text-properties (point-min) (point-max)
                                     '(face default active nil))
                (setq count 0)))
            (bubbles--update-neighbourhood-score count))
          (put-text-property (point-min) (point-max) 'pointer 'arrow)
          (bubbles--update-faces-or-images)
          (sit-for 0))
      (error (message "Bubbles: Internal error %s" err)))))

(defun bubbles--neighbourhood-available ()
  "Return t if another valid neighborhood is available."
  (catch 'found
    (save-excursion
      (dotimes (i (bubbles--grid-height))
        (dotimes (j (bubbles--grid-width))
          (let ((c (bubbles--char-at i j)))
            (if (and (not (eq c (bubbles--empty-char)))
                     (or (eq c (bubbles--char-at (1+ i) j))
                         (eq c (bubbles--char-at i (1+ j)))))
                (throw 'found t)))))
      nil)))

(defun bubbles--count ()
  "Count remaining bubbles."
  (let ((count 0))
    (save-excursion
      (dotimes (i (bubbles--grid-height))
        (dotimes (j (bubbles--grid-width))
          (let ((c (bubbles--char-at i j)))
            (if (not (eq c (bubbles--empty-char)))
                (setq count (1+ count)))))))
    count))

(defun bubbles--reset-score ()
  "Reset bubbles score."
  (setq bubbles--neighbourhood-score 0
        bubbles--score 0)
  (bubbles--update-score))

(defun bubbles--update-score ()
  "Calculate and display new bubbles score."
  (setq bubbles--score (+ bubbles--score bubbles--neighbourhood-score))
  (bubbles--show-scores))

(defun bubbles--update-neighbourhood-score (size)
  "Calculate and display score of active neighborhood from its SIZE."
  (if (> size 1)
      (setq bubbles--neighbourhood-score (expt (- size 1) 2))
    (setq bubbles--neighbourhood-score 0))
  (bubbles--show-scores))

(defun bubbles--show-scores ()
  "Display current scores."
  (save-excursion
    (goto-char (or (next-single-property-change (point-min) 'status)
                   (point-max)))
    (let ((inhibit-read-only t)
          (pos (point)))
      (delete-region (point) (point-max))
      (insert (format "Selected: %4d\n" bubbles--neighbourhood-score))
      (insert " ")
      (add-text-properties (1- (point)) (point)
                           (list 'intangible t 'display
                                 (cons 'space
                                       (list :width bubbles--col-offset))))
      (insert (format "Score:    %4d" bubbles--score))
      (put-text-property pos (point) 'status t))))

(defun bubbles--game-over ()
  "Finish bubbles game."
  (bubbles--update-faces-or-images)
  (setq bubbles--playing nil
        bubbles--save-data nil)
  ;; add bonus if all bubbles were removed
  (when (= 0 (bubbles--count))
    (setq bubbles--score (+ bubbles--score (* (bubbles--grid-height)
                                              (bubbles--grid-width))))
    (bubbles--show-scores))
  ;; Game over message
  (goto-char (point-max))
  (let* ((inhibit-read-only t))
    (insert "\n ")
    (add-text-properties (1- (point)) (point)
                         (list 'intangible t 'display
                               (cons 'space
                                     (list :width bubbles--col-offset))))
    (insert "Game Over!"))
  ;; save score
  (gamegrid-add-score (format "bubbles-%s-%d-%d-%d-scores"
                              (symbol-name (bubbles--shift-mode))
                              (length (bubbles--colors))
                              (bubbles--grid-width) (bubbles--grid-height))
                      bubbles--score))

(defun bubbles-plop ()
  "Remove active bubbles region."
  (interactive)
  (when (and bubbles--playing
             (> bubbles--neighbourhood-score 0))
    (setq bubbles--save-data (list bubbles--score (buffer-string)))
    (let ((inhibit-read-only t))
      ;; blank out current neighbourhood
      (let ((row (bubbles--row (point)))
            (col (bubbles--col (point))))
        (goto-char (point-max))
        (while (not (bobp))
          (backward-char)
          (while (get-text-property (point) 'active)
            (delete-char 1)
            (insert (bubbles--empty-char))
            (add-text-properties (1- (point)) (point) (list 'removed t
                                                            'index -1))))
        (bubbles--goto row col))
      ;; show new score
      (bubbles--update-score)
      ;; update display and wait
      (bubbles--update-faces-or-images)
      (sit-for 0)
      (sleep-for 0.2)
      (discard-input)
      ;; drop down
      (let ((something-dropped nil))
        (save-excursion
          (dotimes (i (bubbles--grid-height))
            (dotimes (j (bubbles--grid-width))
              (bubbles--goto i j)
              (while (get-text-property (point) 'removed)
                (setq something-dropped (or (bubbles--shift 'top i j)
                                            something-dropped))))))
        ;; update display and wait
        (bubbles--update-faces-or-images)
        (when something-dropped
          (sit-for 0)))
      (discard-input)
      ;; shift to left
      (put-text-property (point-min) (point-max) 'removed nil)
      (save-excursion
        (goto-char (point-min))
        (let ((removed-string (format "%c" (bubbles--empty-char))))
          (while (search-forward removed-string nil t)
            (put-text-property (1- (point)) (point) 'removed t))))
      (let ((shifted nil))
        (cond ((eq (bubbles--shift-mode) 'always)
               (save-excursion
                 (dotimes (i (bubbles--grid-height))
                   (dotimes (j (bubbles--grid-width))
                     (bubbles--goto i j)
                     (while (get-text-property (point) 'removed)
                       (setq shifted (or (bubbles--shift 'right i j)
                                         shifted))))))
               (bubbles--update-faces-or-images)
               (sleep-for 0.5))
              (t ;; default shift-mode
               (save-excursion
                 (dotimes (j (bubbles--grid-width))
                   (bubbles--goto (1- (bubbles--grid-height)) j)
                   (let ((shifted-cols 0))
                     (while (get-text-property (point) 'removed)
                       (setq shifted-cols (1+ shifted-cols))
                       (bubbles--shift 'right (1- (bubbles--grid-height)) j))
                     (dotimes (k shifted-cols)
                       (let ((i (- (bubbles--grid-height) 2)))
                         (while (>= i 0)
                           (setq shifted (or (bubbles--shift 'right i j)
                                             shifted))
                           (setq i (1- i))))))))))
        (when shifted
          ;;(sleep-for 0.5)
          (bubbles--update-faces-or-images)
          (sit-for 0)))
      (put-text-property (point-min) (point-max) 'removed nil)
      (unless (bubbles--neighbourhood-available)
        (bubbles--game-over)))
    ;; undo
    (setq buffer-undo-list '((apply bubbles-undo . nil)))
    (force-mode-line-update)
    (redisplay)))

(defun bubbles-undo ()
  "Undo last move."
  (interactive)
  (when bubbles--save-data
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (insert (cadr bubbles--save-data))
      (bubbles--update-faces-or-images)
      (setq bubbles--score (car bubbles--save-data))
      (goto-char pos))
    (setq buffer-undo-list t)
    (force-mode-line-update)
    (redisplay)))

(defun bubbles--shift (from row col)
  "Move bubbles FROM one side to position ROW COL.
Return t if new char is non-empty."
  (save-excursion
    (when (bubbles--goto row col)
      (let ((char-new (bubbles--empty-char))
            (removed nil)
            (trow row)
            (tcol col)
            (index -1))
        (cond ((eq from 'top)
               (setq trow (1- row)))
              ((eq from 'left)
               (setq tcol (1- col)))
              ((eq from 'right)
               (setq tcol (1+ col))))
        (save-excursion
          (when (bubbles--goto trow tcol)
            (setq char-new (char-after (point)))
            (setq removed (get-text-property (point) 'removed))
            (setq index (get-text-property (point) 'index))
            (bubbles--shift from trow tcol)))
        (insert char-new)
        (delete-char 1)
        (add-text-properties (1- (point)) (point) (list 'index index
                                                        'removed removed))
        (not (eq char-new (bubbles--empty-char)))))))

(defun bubbles--initialize-images ()
  "Prepare images for playing `bubbles'."
  (when (and (display-images-p)
             (not (eq bubbles-graphics-theme 'ascii)))
    (let ((template (case bubbles-graphics-theme
                      (circles bubbles--image-template-circle)
                      (balls bubbles--image-template-ball)
                      (squares bubbles--image-template-square)
                      (diamonds bubbles--image-template-diamond)
                      (emacs bubbles--image-template-emacs))))
      (setq bubbles--empty-image
            (create-image (replace-regexp-in-string
                           "^\"\\(.*\\)\t.*c .*\",$"
                           "\"\\1\tc None\"," template)
                          'xpm t
                          ;;:mask 'heuristic
                          :margin '(2 . 1)))
      (setq bubbles--images
            (mapcar (lambda (color)
                      (let* ((rgb (color-values color))
                             (red (nth 0 rgb))
                             (green (nth 1 rgb))
                             (blue (nth 2 rgb)))
                        (with-temp-buffer
                          (insert template)
                          (goto-char (point-min))
                          (re-search-forward
                           "^\"[0-9]+ [0-9]+ \\(.*?\\) .*\",$" nil t)
                          (goto-char (point-min))
                          (while (re-search-forward
                                  "^\"\\(.*\\)\t.*c \\(#.*\\)\",$" nil t)
                            (let* ((crgb (color-values (match-string 2)))
                                   (r (nth 0 crgb))
                                   (g (nth 1 crgb))
                                   (b (nth 2 crgb))
                                   (brightness (/ (+ r g b) 3.0 256 256))
                                   (val (sin (* brightness (/ float-pi 2))))
                                   (rr (* red val))
                                   (gg (* green val))
                                   (bb (* blue val))
                                   ;;(rr (/ (+ red r) 2))
                                   ;;(gg (/ (+ green g) 2))
                                   ;;(bb (/ (+ blue b) 2))
                                   (color (format "#%02x%02x%02x"
                                                  (/ rr 256) (/ gg 256)
                                                  (/ bb 256))))
                              (replace-match (format "\"\\1\tc %s\","
                                                     (upcase color)))))
                          (create-image (buffer-string) 'xpm t
                                        :margin '(2 . 1)
                                        ;;:mask 'heuristic
                                        ))))
                    (bubbles--colors))))
    ;; check images
    (setq bubbles--images-ok bubbles--empty-image)
    (mapc (lambda (elt)
            (setq bubbles--images-ok (and bubbles--images-ok elt)))
          bubbles--images)))

(defun bubbles--update-faces-or-images ()
  "Update faces and/or images, depending on graphics mode."
  (bubbles--set-faces)
  (bubbles--show-images))

(defun bubbles--set-faces ()
  "Update faces in the bubbles buffer."
  (unless (and (display-images-p)
               bubbles--images-ok
               (not (eq bubbles-graphics-theme 'ascii)))
    (when (display-color-p)
      (save-excursion
        (let ((inhibit-read-only t))
          (dotimes (i (bubbles--grid-height))
            (dotimes (j (bubbles--grid-width))
              (bubbles--goto i j)
              (let ((face (nth (get-text-property (point) 'index)
                               bubbles--faces)))
                (when (get-text-property (point) 'active)
                  (set-face-foreground 'bubbles--highlight-face "#ff0000")
                  (setq face 'bubbles--highlight-face))
                (put-text-property (point) (1+ (point))
                                   'face face)))))))))

(defun bubbles--show-images ()
  "Update images in the bubbles buffer."
  (bubbles--remove-overlays)
  (if (and (display-images-p)
           bubbles--images-ok
           (not (eq bubbles-graphics-theme 'ascii)))
      (save-excursion
        (goto-char (point-min))
        (forward-line 1)
        (let ((inhibit-read-only t))
          (dotimes (i (bubbles--grid-height))
            (dotimes (j (bubbles--grid-width))
              (forward-char 1)
              (let ((index (or (get-text-property (point) 'index) -1)))
                (let ((img bubbles--empty-image))
                  (if (>= index 0)
                      (setq img (nth index bubbles--images)))
                  (put-text-property (point) (1+ (point))
                                     'display (cons img nil)))))
            (forward-line 1))))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((disp-prop (get-text-property (point) 'display)))
            (if (and (listp disp-prop)
                     (listp (car disp-prop))
                     (eq (caar disp-prop) 'image))
                (put-text-property (point) (1+ (point)) 'display nil))
            (forward-char 1)))
        (put-text-property (point-min) (point-max) 'pointer 'arrow)))))

(provide 'bubbles)

;;; bubbles.el ends here
