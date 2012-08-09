;;; table.el --- create and edit WYSIWYG text based embedded tables

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.

;; Keywords: wp, convenience
;; Author: Takaaki Ota <Takaaki.Ota@am.sony.com>
;; Created: Sat Jul 08 2000 13:28:45 (PST)

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

;; -------------
;; Introduction:
;; -------------
;;
;; This package provides text based table creation and editing
;; feature.  With this package Emacs is capable of editing tables that
;; are embedded inside a text document, the feature similar to the
;; ones seen in modern WYSIWYG word processors.  A table is a
;; rectangular text area consisting from a surrounding frame and
;; content inside the frame.  The content is usually subdivided into
;; multiple rectangular cells, see the actual tables used below in
;; this document.  Once a table is recognized, editing operation
;; inside a table cell is confined into that specific cell's
;; rectangular area.  This means that typing and deleting characters
;; inside a cell do not affect any outside text but introduces
;; appropriate formatting only to the cell contents.  If necessary for
;; accommodating added text in the cell, the cell automatically grows
;; vertically and/or horizontally.  The package uses no major mode nor
;; minor mode for its implementation because the subject text is
;; localized within a buffer.  Therefore the special behaviors inside
;; a table cells are implemented by using keymap text property
;; instead of buffer wide mode-map.
;;
;;
;; -----------
;; Background:
;; -----------
;;
;; Paul Georgief is one of my best friends.  He became an Emacs
;; convert after I recommended him trying it several years ago.  Now
;; we both are devoted disciples of Emacsism and elisp cult.  One day
;; in his Emacs exploration he asked me "Tak, what is a command to
;; edit tables in Emacs?".  This question started my journey of this
;; table package development.  May the code be with me!  In the
;; software world Emacs is probably one of the longest lifetime record
;; holders.  Amazingly there have been no direct support for WYSIWYG
;; table editing tasks in Emacs.  Many people must have experienced
;; manipulating existing overwrite-mode and picture-mode for this task
;; and only dreamed of having such a lisp package which supports this
;; specific task directly.  Certainly, I have been one of them.  The
;; most difficult part of dealing with table editing in Emacs probably
;; is how to realize localized rectangular editing effect.  Emacs has
;; no rectangular narrowing mechanism.  Existing rect package provides
;; basically kill, delete and yank operations of a rectangle, which
;; internally is a mere list of strings.  A simple approach for
;; realizing the localized virtual rectangular operation is combining
;; rect package capability with a temporary buffer.  Insertion and
;; deletion of a character to a table cell can be trapped by a
;; function that copies the cell rectangle to a temporary buffer then
;; apply the insertion/deletion to the temporary contents.  Then it
;; formats the contents by filling the paragraphs in order to fit it
;; into the original rectangular area and finally copy it back to the
;; original buffer.  This simplistic approach has to bear with
;; significant performance hit.  As cell grows larger the copying
;; rectangle back and forth between the original buffer and the
;; temporary buffer becomes expensive and unbearably slow.  It was
;; completely impractical and an obvious failure.  An idea has been
;; borrowed from the original Emacs design to overcome this
;; shortcoming.  When the terminal screen update was slow and
;; expensive Emacs employed a clever algorithm to reduce actual screen
;; update by removing redundant redrawing operations.  Also the actual
;; redrawing was done only when there was enough idling time.  This
;; technique significantly improved the previously mentioned
;; undesirable situation.  Now the original buffer's rectangle is
;; copied into a cache buffer only once.  Any cell editing operation
;; is done only to the cache contents.  When there is enough idling
;; time the original buffer's rectangle is updated with the current
;; cache contents.  This delayed operation is implemented by using
;; Emacs's timer function.  To reduce the visual awkwardness
;; introduced by the delayed effect the cursor location is updated in
;; real-time as a user types while the cell contents remains the same
;; until the next idling time.  A key to the success of this approach
;; is how to maintain cache coherency.  As a user moves point in and
;; out of a cell the table buffer contents and the cache buffer
;; contents must be synchronized without a mistake.  By observing user
;; action carefully this is possible however not easy.  Once this
;; mechanism is firmly implemented the rest of table features grew in
;; relatively painless progression.  Those users who are familiar with
;; Emacs internals appreciate this table package more.  Because it
;; demonstrates how extensible Emacs is by showing something that
;; appears like a magic.  It lets you re-discover the potential of
;; Emacs.
;;
;;
;; -------------
;; Entry Points:
;; -------------
;;
;; If this is the first time for you to try this package, go ahead and
;; load the package by M-x `load-file' RET.  Specify the package file
;; name "table.el".  Then switch to a new test buffer and issue the
;; command M-x `table-insert' RET.  It'll ask you number of columns,
;; number of rows, cell width and cell height.  Give some small
;; numbers for each of them.  Play with the resulted table for a
;; while.  If you have menu system find the item "Table" under "Tools"
;; and "Table" in the menu bar when the point is in a table cell.
;; Some of them are pretty intuitive and you can easily guess what
;; they do.  M-x `describe-function' and get the documentation of
;; `table-insert'.  The document includes a short tutorial.  When you
;; are tired of guessing how it works come back to this document
;; again.
;;
;; To use the package regularly place this file in the site library
;; directory and add the next expression in your .emacs file.  Make
;; sure that directory is included in the `load-path'.
;;
;;   (require 'table)
;;
;; Have the next expression also, if you want always be ready to edit
;; tables inside text files.  This mechanism is analogous to
;; fontification in a sense that tables are recognized at editing time
;; without having table information saved along with the text itself.
;;
;;   (add-hook 'text-mode-hook 'table-recognize)
;;
;; Following is a table of entry points and brief description of each
;; of them.  The tables below are of course generated and edited by
;; using this package.  Not all the commands are bound to keys.  Many
;; of them must be invoked by "M-x" (`execute-extended-command')
;; command.  Refer to the section "Keymap" below for the commands
;; available from keys.
;;
;; +------------------------------------------------------------------+
;; |                    User Visible Entry Points                     |
;; +-------------------------------+----------------------------------+
;; |           Function            |           Description            |
;; +-------------------------------+----------------------------------+
;; |`table-insert'                 |Insert a table consisting of grid |
;; |                               |of cells by specifying the number |
;; |                               |of COLUMNS, number of ROWS, cell  |
;; |                               |WIDTH and cell HEIGHT.            |
;; +-------------------------------+----------------------------------+
;; |`table-insert-row'             |Insert row(s) of cells before the |
;; |                               |current row that matches the      |
;; |                               |current row structure.            |
;; +-------------------------------+----------------------------------+
;; |`table-insert-column'          |Insert column(s) of cells before  |
;; |                               |the current column that matches   |
;; |                               |the current column structure.     |
;; +-------------------------------+----------------------------------+
;; |`table-delete-row'             |Delete row(s) of cells.  The row  |
;; |                               |must consist from cells of the    |
;; |                               |same height.                      |
;; +-------------------------------+----------------------------------+
;; |`table-delete-column'          |Delete column(s) of cells.  The   |
;; |                               |column must consist from cells of |
;; |                               |the same width.                   |
;; +-------------------------------+----------------------------------+
;; |`table-recognize'              |Recognize all tables in the       |
;; |`table-unrecognize'            |current buffer and                |
;; |                               |activate/deactivate them.         |
;; +-------------------------------+----------------------------------+
;; |`table-recognize-region'       |Recognize all the cells in a      |
;; |`table-unrecognize-region'     |region and activate/deactivate    |
;; |                               |them.                             |
;; +-------------------------------+----------------------------------+
;; |`table-recognize-table'        |Recognize all the cells in a      |
;; |`table-unrecognize-table'      |single table and                  |
;; |                               |activate/deactivate them.         |
;; +-------------------------------+----------------------------------+
;; |`table-recognize-cell'         |Recognize a cell.  Find a cell    |
;; |`table-unrecognize-cell'       |which contains the current point  |
;; |                               |and activate/deactivate that cell.|
;; +-------------------------------+----------------------------------+
;; |`table-forward-cell'           |Move point to the next Nth cell in|
;; |                               |a table.                          |
;; +-------------------------------+----------------------------------+
;; |`table-backward-cell'          |Move point to the previous Nth    |
;; |                               |cell in a table.                  |
;; +-------------------------------+----------------------------------+
;; |`table-span-cell'              |Span the current cell toward the  |
;; |                               |specified direction and merge it  |
;; |                               |with the adjacent cell.  The      |
;; |                               |direction is right, left, above or|
;; |                               |below.                            |
;; +-------------------------------+----------------------------------+
;; |`table-split-cell-vertically'  |Split the current cell vertically |
;; |                               |and create a cell above and a cell|
;; |                               |below the point location.         |
;; +-------------------------------+----------------------------------+
;; |`table-split-cell-horizontally'|Split the current cell            |
;; |                               |horizontally and create a cell on |
;; |                               |the left and a cell on the right  |
;; |                               |of the point location.            |
;; +-------------------------------+----------------------------------+
;; |`table-split-cell'             |Split the current cell vertically |
;; |                               |or horizontally.  This is a       |
;; |                               |wrapper command to the other two  |
;; |                               |orientation specific commands.    |
;; +-------------------------------+----------------------------------+
;; |`table-heighten-cell'          |Heighten the current cell.        |
;; +-------------------------------+----------------------------------+
;; |`table-shorten-cell'           |Shorten the current cell.         |
;; +-------------------------------+----------------------------------+
;; |`table-widen-cell'             |Widen the current cell.           |
;; +-------------------------------+----------------------------------+
;; |`table-narrow-cell'            |Narrow the current cell.          |
;; +-------------------------------+----------------------------------+
;; |`table-fixed-width-mode'       |Toggle fixed width mode.  In the  |
;; |                               |fixed width mode, typing inside a |
;; |                               |cell never changes the cell width,|
;; |                               |while in the normal mode the cell |
;; |                               |width expands automatically in    |
;; |                               |order to prevent a word being     |
;; |                               |folded into multiple lines.  Fixed|
;; |                               |width mode reverses video or      |
;; |                               |underline the cell contents for   |
;; |                               |its indication.                   |
;; +-------------------------------+----------------------------------+
;; |`table-query-dimension'        |Compute and report the current    |
;; |                               |cell dimension, current table     |
;; |                               |dimension and the number of       |
;; |                               |columns and rows in the table.    |
;; +-------------------------------+----------------------------------+
;; |`table-generate-source'        |Generate the source of the current|
;; |                               |table in the specified language   |
;; |                               |and insert it into a specified    |
;; |                               |buffer.                           |
;; +-------------------------------+----------------------------------+
;; |`table-insert-sequence'        |Travel cells forward while        |
;; |                               |inserting a specified sequence    |
;; |                               |string into each cell.            |
;; +-------------------------------+----------------------------------+
;; |`table-capture'                |Convert plain text into a table by|
;; |                               |capturing the text in the region. |
;; +-------------------------------+----------------------------------+
;; |`table-release'                |Convert a table into plain text by|
;; |                               |removing the frame from a table.  |
;; +-------------------------------+----------------------------------+
;; |`table-justify'                |Justify the contents of cell(s).  |
;; +-------------------------------+----------------------------------+
;;
;;
;; *Note*
;;
;; You may find that some of commonly expected table commands are
;; missing such as copying a row/column and yanking it.  Those
;; functions can be obtained through existing Emacs text editing
;; commands.  Rows are easily manipulated with region commands and
;; columns can be copied and pasted through rectangle commands.  After
;; all a table is still a part of text in the buffer.  Only the
;; special behaviors exist inside each cell through text properties.
;;
;; `table-generate-html' which appeared in earlier releases is
;; deprecated in favor of `table-generate-source'.  Now HTML is
;; treated as one of the languages used for describing the table's
;; logical structure.
;;
;;
;; -------
;; Keymap:
;; -------
;;
;; Although this package does not use a mode it does use its own
;; keymap inside a table cell by way of keymap text property.  Some of
;; the standard basic editing commands bound to certain keys are
;; replaced with the table specific version of corresponding commands.
;; This replacement combination is listed in the constant alist
;; `table-command-remap-alist' declared below.  This alist is
;; not meant to be user configurable but mentioned here for your
;; better understanding of using this package.  In addition, table
;; cells have some table specific bindings for cell navigation and
;; cell reformation.  You can find these additional bindings in the
;; constant `table-cell-bindings'.  Those key bound functions are
;; considered as internal functions instead of normal commands,
;; therefore they have special prefix, *table-- instead of table-, for
;; symbols.  The purpose of this is to make it easier for a user to
;; use command name completion.  There is a "normal hooks" variable
;; `table-cell-map-hook' prepared for users to override the default
;; table cell bindings.  Following is the table of predefined default
;; key bound commands inside a table cell.  Remember these bindings
;; exist only inside a table cell.  When your terminal is a tty, the
;; control modifier may not be available or applicable for those
;; special characters.  In this case use "C-cC-c", which is
;; customizable via `table-command-prefix', as the prefix key
;; sequence.  This should preceding the following special character
;; without the control modifier.  For example, use "C-cC-c|" instead
;; of "C-|".
;;
;; +------------------------------------------------------------------+
;; |                Default Bindings in a Table Cell                  |
;; +-------+----------------------------------------------------------+
;; |  Key  |                      Function                            |
;; +-------+----------------------------------------------------------+
;; |  TAB  |Move point forward to the beginning of the next cell.     |
;; +-------+----------------------------------------------------------+
;; | "C->" |Widen the current cell.                                   |
;; +-------+----------------------------------------------------------+
;; | "C-<" |Narrow the current cell.                                  |
;; +-------+----------------------------------------------------------+
;; | "C-}" |Heighten the current cell.                                |
;; +-------+----------------------------------------------------------+
;; | "C-{" |Shorten the current cell.                                 |
;; +-------+----------------------------------------------------------+
;; | "C--" |Split current cell vertically. (one above and one below)  |
;; +-------+----------------------------------------------------------+
;; | "C-|" |Split current cell horizontally. (one left and one right) |
;; +-------+----------------------------------------------------------+
;; | "C-*" |Span current cell into adjacent one.                      |
;; +-------+----------------------------------------------------------+
;; | "C-+" |Insert row(s)/column(s).                                  |
;; +-------+----------------------------------------------------------+
;; | "C-!" |Toggle between normal mode and fixed width mode.          |
;; +-------+----------------------------------------------------------+
;; | "C-#" |Report cell and table dimension.                          |
;; +-------+----------------------------------------------------------+
;; | "C-^" |Generate the source in a language from the current table. |
;; +-------+----------------------------------------------------------+
;; | "C-:" |Justify the contents of cell(s).                          |
;; +-------+----------------------------------------------------------+
;;
;; *Note*
;;
;; When using `table-cell-map-hook' do not use `local-set-key'.
;;
;;   (add-hook 'table-cell-map-hook
;;     (function (lambda ()
;;       (local-set-key [<key sequence>] '<function>))))
;;
;; Above code is well known ~/.emacs idiom for customizing a mode
;; specific keymap however it does not work for this package.  This is
;; because there is no table mode in effect.  This package does not
;; use a local map therefore you must modify `table-cell-map'
;; explicitly.  The correct way of achieving above task is:
;;
;;   (add-hook 'table-cell-map-hook
;;     (function (lambda ()
;;       (define-key table-cell-map [<key sequence>] '<function>))))
;;
;; -----
;; Menu:
;; -----
;;
;; If a menu system is available a group of table specific menu items,
;; "Table" under "Tools" section of the menu bar, is globally added
;; after this package is loaded.  The commands in this group are
;; limited to the ones that are related to creation and initialization
;; of tables, such as to insert a table, to insert rows and columns,
;; or recognize and unrecognize tables.  Once tables are created and
;; point is placed inside of a table cell a table specific menu item
;; "Table" appears directly on the menu bar.  The commands in this
;; menu give full control on table manipulation that include cell
;; navigation, insertion, splitting, spanning, shrinking, expansion
;; and unrecognizing.  In addition to above two types of menu there is
;; a pop-up menu available within a table cell.  The content of pop-up
;; menu is identical to the full table menu.  [mouse-3] is the default
;; button, defined in `table-cell-bindings', to bring up the pop-up
;; menu.  It can be reconfigured via `table-cell-map-hook'.  The
;; benefit of a pop-up menu is that it combines selection of the
;; location (which cell, where in the cell) and selection of the
;; desired operation into a single clicking action.
;;
;;
;; -------------------------------
;; Definition of tables and cells:
;; -------------------------------
;;
;; There is no artificial-intelligence magic in this package.  The
;; definition of a table and the cells inside the table is reasonably
;; limited in order to achieve acceptable performance in the
;; interactive operation under Emacs lisp implementation.  A valid
;; table is a rectangular text area completely filled with valid
;; cells.  A valid cell is a rectangle text area, which four borders
;; consist of valid border characters.  Cells can not be nested one to
;; another or overlapped to each other except sharing the border
;; lines.  A valid character of a cell's vertical border is either
;; table-cell-vertical-char `|' or table-cell-intersection-char `+'.
;; A valid character of a cell's horizontal border is either
;; one of table-cell-horizontal-chars (`-' or `=')
;; or table-cell-intersection-char `+'.
;; A valid character of the four corners of a cell must be
;; table-cell-intersection-char `+'.  A cell must contain at least one
;; character space inside.  There is no restriction about the contents
;; of a table cell, however it is advised if possible to avoid using
;; any of the border characters inside a table cell.  Normally a few
;; boarder characters inside a table cell are harmless.  But it is
;; possible that they accidentally align up to emulate a bogus cell
;; corner on which software relies on for cell recognition.  When this
;; happens the software may be fooled by it and fail to determine
;; correct cell dimension.
;;
;; Following are the examples of valid tables.
;;
;; +--+----+---+     +-+     +--+-----+
;; |  |    |   |     | |     |  |     |
;; +--+----+---+     +-+     |  +--+--+
;; |  |    |   |             |  |  |  |
;; +--+----+---+             +--+--+  |
;;                           |     |  |
;;                           +-----+--+
;;
;; The next five tables are the examples of invalid tables.  (From
;; left to right, 1. nested cells 2. overlapped cells and a
;; non-rectangle cell 3. non-rectangle table 4. zero width/height
;; cells 5. zero sized cell)
;;
;; +-----+    +-----+       +--+    +-++--+    ++
;; |     |    |     |       |  |    | ||  |    ++
;; | +-+ |    |     |       |  |    | ||  |
;; | | | |    +--+  |    +--+--+    +-++--+
;; | +-+ |    |  |  |    |  |  |    +-++--+
;; |     |    |  |  |    |  |  |    | ||  |
;; +-----+    +--+--+    +--+--+    +-++--+
;;
;; Although the program may recognizes some of these invalid tables,
;; results from the subsequent editing operations inside those cells
;; are not predictable and will most likely start destroying the table
;; structures.
;;
;; It is strongly recommended to have at least one blank line above
;; and below a table.  For a table to coexist peacefully with
;; surrounding environment table needs to be separated from unrelated
;; text.  This is necessary for the left table to grow or shrink
;; horizontally without breaking the right table in the following
;; example.
;;
;;                          +-----+-----+-----+
;;  +-----+-----+           |     |     |     |
;;  |     |     |           +-----+-----+-----+
;;  +-----+-----+           |     |     |     |
;;                          +-----+-----+-----+
;;
;;
;; -------------------------
;; Cell contents formatting:
;; -------------------------
;;
;; The cell contents are formatted by filling a paragraph immediately
;; after characters are inserted into or deleted from a cell.  Because
;; of this, cell contents always remain fit inside a cell neatly.  One
;; drawback of this is that users do not have full control over
;; spacing between words and line breaking.  Only one space can be
;; entered between words and up to two spaces between sentences.  For
;; a newline to be effective the new line must form a beginning of
;; paragraph, otherwise it'll automatically be merged with the
;; previous line in a same paragraph.  To form a new paragraph the
;; line must start with some space characters or immediately follow a
;; blank line.  Here is a typical example of how to list items within
;; a cell.  Without a space at the beginning of each line the items
;; can not stand on their own.
;;
;; +---------------------------------+
;; |Each one of the following three  |
;; |items starts with a space        |
;; |character thus forms a paragraph |
;; |of its own.  Limitations in cell |
;; |contents formatting are:         |
;; |                                 |
;; | 1. Only one space between words.|
;; | 2. Up to two spaces between     |
;; |sentences.                       |
;; | 3. A paragraph must start with  |
;; |spaces or follow a blank line.   |
;; |                                 |
;; |This paragraph stays away from   |
;; |the item 3 because there is a    |
;; |blank line between them.         |
;; +---------------------------------+
;;
;; In the normal operation table cell width grows automatically when
;; certain word has to be folded into the next line if the width had
;; not been increased.  This normal operation is useful and
;; appropriate for most of the time, however, it is sometimes useful
;; or necessary to fix the width of table and width of table cells.
;; For this purpose the package provides fixed width mode.  You can
;; toggle between fixed width mode and normal mode by "C-!".
;;
;; Here is a simple example of the fixed width mode.  Suppose we have
;; a table like this one.
;;
;; +-----+
;; |     |
;; +-----+
;;
;; In normal mode if you type a word "antidisestablishmentarianism" it
;; grows the cell horizontally like this.
;;
;; +----------------------------+
;; |antidisestablishmentarianism|
;; +----------------------------+
;;
;;  In the fixed width mode the same action produces the following
;;  result.  The folded locations are indicated by a continuation
;;  character (`\' is the default).  The continuation character is
;;  treated specially so it is recommended to choose a character that
;;  does not appear elsewhere in table cells.  This character is
;;  configurable via customization and is kept in the variable
;;  `table-word-continuation-char'.  The continuation character is
;;  treated specially only in the fixed width mode and has no special
;;  meaning in the normal mode however.
;;
;; +-----+
;; |anti\|
;; |dise\|
;; |stab\|
;; |lish\|
;; |ment\|
;; |aria\|
;; |nism |
;; +-----+
;;
;;
;; -------------------
;; Cell Justification:
;; -------------------
;;
;; By default the cell contents are filled with left justification and
;; no vertical justification.  A paragraph can be justified
;; individually but only horizontally.  Paragraph justification is for
;; appearance only and does not change any structural information
;; while cell justification affects table's structural information.
;; For cell justification a user can select horizontal justification
;; and vertical justification independently.  Horizontal justification
;; must be one of the three 'left, 'center or 'right.  Vertical
;; justification can be 'top, 'middle, 'bottom or 'none.  When a cell
;; is justified, that information is recorded as a part of text
;; property therefore the information is persistent as long as the
;; cell remains within the Emacs world.  Even copying tables by region
;; and rectangle manipulation commands preserve this information.
;; However, once the table text is saved as a file and the buffer is
;; killed the justification information vanishes permanently.  To
;; alleviate this shortcoming without forcing users to save and
;; maintain a separate attribute file, the table code detects
;; justification of each cell when recognizing a table.  This
;; detection is done by guessing the justification by looking at the
;; appearance of the cell contents.  Since it is a guessing work it
;; does not guarantee the perfectness but it is designed to be
;; practically good enough.  The guessing algorithm is implemented in
;; the function `table--detect-cell-alignment'.  If you have better
;; algorithm or idea any suggestion is welcome.
;;
;;
;; -----
;; Todo: (in the order of priority, some are just possibility)
;; -----
;;
;; Fix compatibilities with other input method than quail
;; Resolve conflict with flyspell
;; Use mouse for resizing cells
;; A mechanism to link cells internally
;; Consider the use of variable width font under Emacs 21
;; Consider the use of `:box' face attribute under Emacs 21
;; Consider the use of `modification-hooks' text property instead of
;; rebinding the keymap
;; Maybe provide complete XEmacs support in the future however the
;; "extent" is the single largest obstacle lying ahead, read the
;; document in Emacs info.
;; (eval '(progn (require 'info) (Info-find-node "elisp" "Not Intervals")))
;;
;;
;; ---------------
;; Acknowledgment:
;; ---------------
;;
;; Table would not have been possible without the help and
;; encouragement of the following spirited contributors.
;;
;; Paul Georgief <georgief@igpp.ucsd.edu> has been the best tester
;; of the code as well as the constructive criticizer.
;;
;; Gerd Moellmann <gerd@gnu.org> gave me useful suggestions from Emacs
;; 21 point of view.
;;
;; Richard Stallman <rms@gnu.org> showed the initial interest in this
;; attempt of implementing the table feature to Emacs.  This greatly
;; motivated me to follow through to its completion.
;;
;; Kenichi Handa <handa@etl.go.jp> kindly guided me through to
;; overcome many technical issues while I was struggling with quail
;; related internationalization problems.
;;
;; Christoph Conrad <christoph.conrad@gmx.de> suggested making symbol
;; names consistent as well as fixing several bugs.
;;
;; Paul Lew <paullew@cisco.com> suggested implementing fixed width
;; mode as well as multi column width (row height) input interface.
;;
;; Michael Smith <smith@xml-doc.org> a well-informed DocBook user
;; asked for CALS table source generation and helped me following
;; through the work by offering valuable suggestions and testing out
;; the code.  Jorge Godoy <godoy@conectiva.com> has also suggested
;; supporting for DocBook tables.
;;
;; And many other individuals who reported bugs and suggestions.

;;; Code:


(require 'regexp-opt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compatibility:
;;;

;; hush up the byte-compiler
(defvar quail-translating)
(defvar quail-converting)
(defvar flyspell-mode)
(defvar real-last-command)
(defvar delete-selection-mode)
;; This is evil!!
;; (eval-when-compile
;;   (unless (fboundp 'set-face-property)
;;     (defun set-face-property (face prop value)))
;;   (unless (fboundp 'unibyte-char-to-multibyte)
;;     (defun unibyte-char-to-multibyte (char)))
;;   (defun table--point-in-cell-p (&optional location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customization:
;;;

(defgroup table nil
  "Text based table manipulation utilities."
  :tag "Table"
  :prefix "table-"
  :group 'wp
  :version "22.1")

(defgroup table-hooks nil
  "Hooks for table manipulation utilities."
  :group 'table)

(defcustom table-time-before-update 0.2
  "Time in seconds before updating the cell contents after typing.
Updating the cell contents on the screen takes place only after this
specified amount of time has passed after the last modification to the
cell contents.  When the contents of a table cell changes repetitively
and frequently the updating the cell contents on the screen is
deferred until at least this specified amount of quiet time passes.  A
smaller number wastes more computation resource by unnecessarily
frequent screen update.  A large number presents noticeable and
annoying delay before the typed result start appearing on the screen."
  :tag "Time Before Cell Update"
  :type 'number
  :group 'table)

(defcustom table-time-before-reformat 0.2
  "Time in seconds before reformatting the table.
This many seconds must pass in addition to `table-time-before-update'
before the table is updated with newly widened width or heightened
height."
  :tag "Time Before Cell Reformat"
  :type 'number
  :group 'table)

(defcustom table-command-prefix [(control c) (control c)]
  "Key sequence to be used as prefix for table command key bindings."
  :type '(vector (repeat :inline t sexp))
  :tag "Table Command Prefix"
  :group 'table)

(defface table-cell
  '((((min-colors 88) (class color))
     (:foreground "gray90" :background "blue1"))
    (((class color))
     (:foreground "gray90" :background "blue"))
    (t (:bold t)))
  "Face used for table cell contents."
  :tag "Cell Face"
  :group 'table)

(defcustom table-cell-horizontal-chars "-="
  "Characters that may be used for table cell's horizontal border line."
  :tag "Cell Horizontal Boundary Characters"
  :type 'string
  :group 'table)

(defcustom table-cell-vertical-char ?\|
  "Character that forms table cell's vertical border line."
  :tag "Cell Vertical Boundary Character"
  :type 'character
  :group 'table)

(defcustom table-cell-intersection-char ?\+
  "Character that forms table cell's corner."
  :tag "Cell Intersection Character"
  :type 'character
  :group 'table)

(defcustom table-word-continuation-char ?\\
  "Character that indicates word continuation into the next line.
This character has a special meaning only in the fixed width mode,
that is when `table-fixed-width-mode' is non-nil .  In the fixed width
mode this character indicates that the location is continuing into the
next line.  Be careful about the choice of this character.  It is
treated substantially different manner than ordinary characters.  Try
select a character that is unlikely to appear in your document."
  :tag "Cell Word Continuation Character"
  :type 'character
  :group 'table)

(defun table-set-table-fixed-width-mode (variable value)
  (if (fboundp variable)
      (funcall variable (if value 1 -1))))

(defun table-initialize-table-fixed-width-mode (variable value)
  (set variable value))

(defcustom table-fixed-width-mode nil
  "Cell width is fixed when this is non-nil.
Normally it should be nil for allowing automatic cell width expansion
that widens a cell when it is necessary.  When non-nil, typing in a
cell does not automatically expand the cell width.  A word that is too
long to fit in a cell is chopped into multiple lines.  The chopped
location is indicated by `table-word-continuation-char'.  This
variable's value can be toggled by \\[table-fixed-width-mode] at
run-time."
  :tag "Fix Cell Width"
  :type 'boolean
  :initialize 'table-initialize-table-fixed-width-mode
  :set 'table-set-table-fixed-width-mode
  :group 'table)

(defcustom table-detect-cell-alignment t
  "Detect cell contents alignment automatically.
When non-nil cell alignment is automatically determined by the
appearance of the current cell contents when recognizing tables as a
whole.  This applies to `table-recognize', `table-recognize-region'
and `table-recognize-table' but not to `table-recognize-cell'."
  :tag "Detect Cell Alignment"
  :type 'boolean
  :group 'table)

(defcustom table-dest-buffer-name "table"
  "Default buffer name (without a suffix) for source generation."
  :tag "Source Buffer Name"
  :type 'string
  :group 'table)

(defcustom table-html-delegate-spacing-to-user-agent nil
  "Non-nil delegates cell contents spacing entirely to user agent.
Otherwise, when nil, it preserves the original spacing and line breaks."
  :tag "HTML delegate spacing"
  :type 'boolean
  :group 'table)

(defcustom table-html-th-rows 0
  "Number of top rows to become header cells automatically in HTML generation."
  :tag "HTML Header Rows"
  :type 'integer
  :group 'table)

(defcustom table-html-th-columns 0
  "Number of left columns to become header cells automatically in HTML generation."
  :tag "HTML Header Columns"
  :type 'integer
  :group 'table)

(defcustom table-html-table-attribute "border=\"1\""
  "Table attribute that applies to the table in HTML generation."
  :tag "HTML table attribute"
  :type 'string
  :group 'table)

(defcustom table-html-cell-attribute ""
  "Cell attribute that applies to all cells in HTML generation.
Do not specify \"align\" and \"valign\" because they are determined by
the cell contents dynamically."
  :tag "HTML cell attribute"
  :type 'string
  :group 'table)

(defcustom table-cals-thead-rows 1
  "Number of top rows to become header rows in CALS table."
  :tag "CALS Header Rows"
  :type 'integer
  :group 'table)

;;;###autoload
(defcustom table-cell-map-hook nil
  "Normal hooks run when finishing construction of `table-cell-map'.
User can modify `table-cell-map' by adding custom functions here."
  :tag "Cell Keymap Hooks"
  :type 'hook
  :group 'table-hooks)

(defcustom table-disable-incompatibility-warning nil
  "Disable compatibility warning notice.
When nil user is reminded of known incompatible issues."
  :tag "Disable Incompatibility Warning"
  :type 'boolean
  :group 'table)

(defcustom table-abort-recognition-when-input-pending t
  "Abort current recognition process when input pending.
Abort current recognition process when we are not sure that no input
is available.  When non-nil lengthy recognition process is aborted
simply by any key input."
  :tag "Abort Recognition When Input Pending"
  :type 'boolean
  :group 'table)

;;;###autoload
(defcustom table-load-hook nil
  "List of functions to be called after the table is first loaded."
  :type 'hook
  :group 'table-hooks)

;;;###autoload
(defcustom table-point-entered-cell-hook nil
  "List of functions to be called after point entered a table cell."
  :type 'hook
  :group 'table-hooks)

;;;###autoload
(defcustom table-point-left-cell-hook nil
  "List of functions to be called after point left a table cell."
  :type 'hook
  :group 'table-hooks)

(defvar table-yank-handler '(nil nil t nil)
  "Yank handler for tables.")

(setplist 'table-disable-incompatibility-warning nil)

(defvar table-disable-menu (null (and (locate-library "easymenu")
				      (require 'easymenu)
				      (fboundp 'easy-menu-add-item)))
  "*When non-nil, use of menu by table package is disabled.
It must be set before loading this package `table.el' for the first
time.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation:
;;;

;;; Internal variables and constants
;;; No need of user configuration

(defconst table-paragraph-start "[ \t\n\f]"
  "Regexp for beginning of a line that starts OR separates paragraphs.")
(defconst table-cache-buffer-name " *table cell cache*"
  "Cell cache buffer name.")
(defvar table-cell-info-lu-coordinate nil
  "Zero based coordinate of the cached cell's left upper corner.")
(defvar table-cell-info-rb-coordinate nil
  "Zero based coordinate of the cached cell's right bottom corner.")
(defvar table-cell-info-width nil
  "Number of characters per cached cell width.")
(defvar table-cell-info-height nil
  "Number of lines per cached cell height.")
(defvar table-cell-info-justify nil
  "Justification information of the cached cell.")
(defvar table-cell-info-valign nil
  "Vertical alignment information of the cached cell.")
(defvar table-cell-self-insert-command-count 0
  "Counter for undo control.")
(defvar table-cell-map nil
  "Keymap for table cell contents.")
(defvar table-cell-global-map-alist nil
  "Alist of copy of global maps that are substituted in `table-cell-map'.")
(defvar table-global-menu-map nil
  "Menu map created via `easy-menu-define'.")
(defvar table-cell-menu-map nil
  "Menu map created via `easy-menu-define'.")
(defvar table-cell-buffer nil
  "Buffer that contains the table cell.")
(defvar table-cell-cache-point-coordinate nil
  "Cache point coordinate based from the cell origin.")
(defvar table-cell-cache-mark-coordinate nil
  "Cache mark coordinate based from the cell origin.")
(defvar table-cell-entered-state nil
  "Records the state whether currently in a cell or nor.")
(defvar table-update-timer nil
  "Timer id for deferred cell update.")
(defvar table-widen-timer nil
  "Timer id for deferred cell update.")
(defvar table-heighten-timer nil
  "Timer id for deferred cell update.")
(defvar table-inhibit-update nil
  "Non-nil inhibits implicit cell and cache updates.
It inhibits `table-with-cache-buffer' to update data in both direction, cell to cache and cache to cell.")
(defvar table-inhibit-auto-fill-paragraph nil
  "Non-nil inhibits auto fill paragraph when `table-with-cache-buffer' exits.
This is always set to nil at the entry to `table-with-cache-buffer' before executing body forms.")
(defvar table-mode-indicator nil
  "For mode line indicator")
;; This is not a real minor-mode but placed in the minor-mode-alist
;; so that we can show the indicator on the mode line handy.
(make-variable-buffer-local 'table-mode-indicator)
(unless (assq table-mode-indicator minor-mode-alist)
  (push '(table-mode-indicator (table-fixed-width-mode " Fixed-Table" " Table"))
        minor-mode-alist))

(defconst table-source-languages '(html latex cals)
  "Supported source languages.")
(defvar table-source-info-plist nil
  "General storage for temporary information used while generating source.")

;; The following history containers not only keep the history of user
;; entries but also serve as the default value providers.  When an
;; interactive command is invoked it offers a user the latest entry
;; of the history as a default selection.  Therefore the values below
;; are the first default value when a command is invoked for the very
;; first time when there is no real history existing yet.
(defvar table-cell-span-direction-history '("right"))
(defvar table-cell-split-orientation-history '("horizontally"))
(defvar table-cell-split-contents-to-history '("split"))
(defvar table-insert-row-column-history '("row"))
(defvar table-justify-history '("center"))
(defvar table-columns-history '("3"))
(defvar table-rows-history '("3"))
(defvar table-cell-width-history '("5"))
(defvar table-cell-height-history '("1"))
(defvar table-source-caption-history '("Table"))
(defvar table-sequence-string-history '("0"))
(defvar table-sequence-count-history '("0"))
(defvar table-sequence-increment-history '("1"))
(defvar table-sequence-interval-history '("1"))
(defvar table-sequence-justify-history '("left"))
(defvar table-source-language-history '("html"))
(defvar table-col-delim-regexp-history '(""))
(defvar table-row-delim-regexp-history '(""))
(defvar table-capture-justify-history '("left"))
(defvar table-capture-min-cell-width-history '("5"))
(defvar table-capture-columns-history '(""))
(defvar table-target-history '("cell"))

;; Some entries in `table-cell-bindings' are duplicated in
;; `table-command-remap-alist'.  There is a good reason for
;; this.  Common key like return key may be taken by some other
;; function than normal `newline' function.  Thus binding return key
;; directly for `*table--cell-newline' ensures that the correct enter
;; operation in a table cell.  However
;; `table-command-remap-alist' has an additional role than
;; replacing commands.  It is also used to construct a table command
;; list.  This list is very important because it is used to check if
;; the previous command was one of them in this list or not.  If the
;; previous command is found in the list the current command will not
;; refill the table cache.  If the command were not listed fast
;; typing can cause unwanted cache refill.
(defconst table-cell-bindings
  '(([(control i)]	. table-forward-cell)
    ([(control I)]	. table-backward-cell)
    ([tab]		. table-forward-cell)
    ([(shift backtab)]	. table-backward-cell) ; for HPUX console keyboard
    ([(shift iso-lefttab)]    . table-backward-cell) ; shift-tab on a microsoft natural keyboard and redhat linux
    ([(shift tab)]	. table-backward-cell)
    ([return]		. *table--cell-newline)
    ([(control m)]	. *table--cell-newline)
    ([(control j)]	. *table--cell-newline-and-indent)
    ([mouse-3]		. *table--present-cell-popup-menu)
    ([(control ?>)]	. table-widen-cell)
    ([(control ?<)]	. table-narrow-cell)
    ([(control ?})]	. table-heighten-cell)
    ([(control ?{)]	. table-shorten-cell)
    ([(control ?-)]	. table-split-cell-vertically)
    ([(control ?|)]	. table-split-cell-horizontally)
    ([(control ?*)]	. table-span-cell)
    ([(control ?+)]	. table-insert-row-column)
    ([(control ?!)]	. table-fixed-width-mode)
    ([(control ?#)]	. table-query-dimension)
    ([(control ?^)]	. table-generate-source)
    ([(control ?:)]	. table-justify)
    )
  "Bindings for table cell commands.")

(defvar table-command-remap-alist
  '((self-insert-command . *table--cell-self-insert-command)
    (completion-separator-self-insert-autofilling . *table--cell-self-insert-command)
    (completion-separator-self-insert-command . *table--cell-self-insert-command)
    (delete-char . *table--cell-delete-char)
    (delete-backward-char . *table--cell-delete-backward-char)
    (backward-delete-char . *table--cell-delete-backward-char)
    (backward-delete-char-untabify . *table--cell-delete-backward-char)
    (newline . *table--cell-newline)
    (newline-and-indent . *table--cell-newline-and-indent)
    (open-line . *table--cell-open-line)
    (quoted-insert . *table--cell-quoted-insert)
    (describe-mode . *table--cell-describe-mode)
    (describe-bindings . *table--cell-describe-bindings)
    (dabbrev-expand . *table--cell-dabbrev-expand)
    (dabbrev-completion . *table--cell-dabbrev-completion))
  "List of cons cells consisting of (ORIGINAL-COMMAND . TABLE-VERSION-OF-THE-COMMAND).")

(defvar table-command-list nil
  "List of commands that override original commands.")
;; construct the real contents of the `table-command-list'
(let ((remap-alist table-command-remap-alist))
  (setq table-command-list nil)
  (while remap-alist
    (setq table-command-list (cons (cdar remap-alist) table-command-list))
    (setq remap-alist (cdr remap-alist))))

(defconst table-global-menu
  '("Table"
    ("Insert"
     ["a Table..." table-insert
      :active (and (not buffer-read-only) (not (table--probe-cell)))
      :help "Insert a text based table at point"]
     ["Row" table-insert-row
      :active (table--row-column-insertion-point-p)
      :help "Insert row(s) of cells in table"]
     ["Column" table-insert-column
      :active (table--row-column-insertion-point-p 'column)
      :help "Insert column(s) of cells in table"])
    "----"
    ("Recognize"
     ["in Buffer" table-recognize
      :active t
      :help "Recognize all tables in the current buffer"]
     ["in Region" table-recognize-region
      :active (and mark-active (not (eq (mark t) (point))))
      :help "Recognize all tables in the current region"]
     ["a Table" table-recognize-table
      :active (table--probe-cell)
      :help "Recognize a table at point"]
     ["a Cell" table-recognize-cell
      :active (let ((cell (table--probe-cell)))
		(and cell (null (table--at-cell-p (car cell)))))
      :help "Recognize a cell at point"])
    ("Unrecognize"
     ["in Buffer" table-unrecognize
      :active t
      :help "Unrecognize all tables in the current buffer"]
     ["in Region" table-unrecognize-region
      :active (and mark-active (not (eq (mark t) (point))))
      :help "Unrecognize all tables in the current region"]
     ["a Table" table-unrecognize-table
      :active (table--probe-cell)
      :help "Unrecognize the current table"]
     ["a Cell" table-unrecognize-cell
      :active (let ((cell (table--probe-cell)))
		(and cell (table--at-cell-p (car cell))))
      :help "Unrecognize the current cell"])
    "----"
    ["Capture Region" table-capture
     :active (and (not buffer-read-only) mark-active (not (eq (mark t) (point))) (not (table--probe-cell)))
     :help "Capture text in the current region as a table"]
    ["Release" table-release
     :active (table--editable-cell-p)
     :help "Release the current table as plain text"]))

(defconst table-cell-menu
  '("Table"
    ("Insert"
     ["Row" table-insert-row
      :active (table--row-column-insertion-point-p)
      :help "Insert row(s) of cells in table"]
     ["Column" table-insert-column
      :active (table--row-column-insertion-point-p 'column)
      :help "Insert column(s) of cells in table"])
    ("Delete"
     ["Row" table-delete-row
      :active (table--editable-cell-p)
      :help "Delete row(s) of cells in table"]
     ["Column" table-delete-column
      :active (table--editable-cell-p)
      :help "Delete column(s) of cells in table"])
    "----"
    ("Split a Cell"
     ["Horizontally" table-split-cell-horizontally
      :active (table--cell-can-split-horizontally-p)
      :help "Split the current cell horizontally at point"]
     ["Vertically"  table-split-cell-vertically
      :active (table--cell-can-split-vertically-p)
      :help "Split the current cell vertical at point"])
    ("Span a Cell to"
     ["Right" (table-span-cell 'right)
      :active (table--cell-can-span-p 'right)
      :help "Span the current cell into the right cell"]
     ["Left" (table-span-cell 'left)
      :active (table--cell-can-span-p 'left)
      :help "Span the current cell into the left cell"]
     ["Above" (table-span-cell 'above)
      :active (table--cell-can-span-p 'above)
      :help "Span the current cell into the cell above"]
     ["Below" (table-span-cell 'below)
      :active (table--cell-can-span-p 'below)
      :help "Span the current cell into the cell below"])
    "----"
    ("Shrink Cells"
     ["Horizontally" table-narrow-cell
      :active (table--editable-cell-p)
      :help "Shrink the current cell horizontally"]
     ["Vertically" table-shorten-cell
      :active (table--editable-cell-p)
      :help "Shrink the current cell vertically"])
    ("Expand Cells"
     ["Horizontally" table-widen-cell
      :active (table--editable-cell-p)
      :help "Expand the current cell horizontally"]
     ["Vertically" table-heighten-cell
      :active (table--editable-cell-p)
      :help "Expand the current cell vertically"])
    "----"
    ("Justify"
     ("a Cell"
      ["Left" (table-justify-cell 'left)
       :active (table--editable-cell-p)
       :help "Left justify the contents of the current cell"]
      ["Center" (table-justify-cell 'center)
       :active (table--editable-cell-p)
       :help "Center justify the contents of the current cell"]
      ["Right" (table-justify-cell 'right)
       :active (table--editable-cell-p)
       :help "Right justify the contents of the current cell"]
      "----"
      ["Top" (table-justify-cell 'top)
       :active (table--editable-cell-p)
       :help "Top align the contents of the current cell"]
      ["Middle" (table-justify-cell 'middle)
       :active (table--editable-cell-p)
       :help "Middle align the contents of the current cell"]
      ["Bottom" (table-justify-cell 'bottom)
       :active (table--editable-cell-p)
       :help "Bottom align the contents of the current cell"]
      ["None" (table-justify-cell 'none)
       :active (table--editable-cell-p)
       :help "Remove vertical alignment from the current cell"])
     ("a Row"
      ["Left" (table-justify-row 'left)
       :active (table--editable-cell-p)
       :help "Left justify the contents of all cells in the current row"]
      ["Center" (table-justify-row 'center)
       :active (table--editable-cell-p)
       :help "Center justify the contents of all cells in the current row"]
      ["Right" (table-justify-row 'right)
       :active (table--editable-cell-p)
       :help "Right justify the contents of all cells in the current row"]
      "----"
      ["Top" (table-justify-row 'top)
       :active (table--editable-cell-p)
       :help "Top align the contents of all cells in the current row"]
      ["Middle" (table-justify-row 'middle)
       :active (table--editable-cell-p)
       :help "Middle align the contents of all cells in the current row"]
      ["Bottom" (table-justify-row 'bottom)
       :active (table--editable-cell-p)
       :help "Bottom align the contents of all cells in the current row"]
      ["None" (table-justify-cell 'none)
       :active (table--editable-cell-p)
       :help "Remove vertical alignment from all cells in the current row"])
     ("a Column"
      ["Left" (table-justify-column 'left)
       :active (table--editable-cell-p)
       :help "Left justify the contents of all cells in the current column"]
      ["Center" (table-justify-column 'center)
       :active (table--editable-cell-p)
       :help "Center justify the contents of all cells in the current column"]
      ["Right" (table-justify-column 'right)
       :active (table--editable-cell-p)
       :help "Right justify the contents of all cells in the current column"]
      "----"
      ["Top" (table-justify-column 'top)
       :active (table--editable-cell-p)
       :help "Top align the contents of all cells in the current column"]
      ["Middle" (table-justify-column 'middle)
       :active (table--editable-cell-p)
       :help "Middle align the contents of all cells in the current column"]
      ["Bottom" (table-justify-column 'bottom)
       :active (table--editable-cell-p)
       :help "Bottom align the contents of all cells in the current column"]
      ["None" (table-justify-cell 'none)
       :active (table--editable-cell-p)
       :help "Remove vertical alignment from all cells in the current column"])
     ("a Paragraph"
      ["Left" (table-justify-cell 'left t)
       :active (table--editable-cell-p)
       :help "Left justify the current paragraph"]
      ["Center" (table-justify-cell 'center t)
       :active (table--editable-cell-p)
       :help "Center justify the current paragraph"]
      ["Right" (table-justify-cell 'right t)
       :active (table--editable-cell-p)
       :help "Right justify the current paragraph"]))
    "----"
    ["Query Dimension" table-query-dimension
     :active (table--probe-cell)
     :help "Get the dimension of the current cell and the current table"]
    ["Generate Source" table-generate-source
     :active (table--probe-cell)
     :help "Generate source of the current table in the specified language"]
    ["Insert Sequence" table-insert-sequence
     :active (table--editable-cell-p)
     :help "Travel cells forward while inserting a specified sequence string in each cell"]
    ("Unrecognize"
     ["a Table" table-unrecognize-table
      :active (table--probe-cell)
      :help "Unrecognize the current table"]
     ["a Cell" table-unrecognize-cell
      :active (let ((cell (table--probe-cell)))
		(and cell (table--at-cell-p (car cell))))
      :help "Unrecognize the current cell"])
    ["Release" table-release
     :active (table--editable-cell-p)
     :help "Release the current table as plain text"]
    ("Configure Width to"
     ["Auto Expand Mode" (table-fixed-width-mode -1)
      :active t
      :style radio
      :selected (not table-fixed-width-mode)
      :help "A mode that allows automatic horizontal cell expansion"]
     ["Fixed Width Mode" (table-fixed-width-mode 1)
      :active t
      :style radio
      :selected table-fixed-width-mode
      :help "A mode that does not allow automatic horizontal cell expansion"])
    ("Navigate"
     ["Forward Cell" table-forward-cell
      :active (table--probe-cell)
      :help "Move point forward by cell(s)"]
     ["Backward Cell" table-backward-cell
      :active (table--probe-cell)
      :help "Move point backward by cell(s)"])
    ))

;; XEmacs causes an error when encountering unknown keywords in the
;; menu definition.  Specifically the :help keyword is new in Emacs 21
;; and causes error for the XEmacs function `check-menu-syntax'.  IMHO
;; it is unwise to generate an error for unknown keywords because it
;; kills the nice backward compatible extensibility of keyword use.
;; Unknown keywords should be quietly ignore so that future extension
;; does not cause a problem in the old implementation.  Sigh...
(when (featurep 'xemacs)
  (mapcar
   (defun table--tweak-menu-for-xemacs (menu)
     (cond
      ((listp menu)
       (mapcar 'table--tweak-menu-for-xemacs menu))
      ((vectorp menu)
       (let ((i 0) (len (length menu)))
	 (while (< i len)
	   ;; replace :help with something harmless.
	   (if (eq (aref menu i) :help) (aset menu i :included))
	   (setq i (1+ i)))))))
   (list table-global-menu table-cell-menu))
  (defvar mark-active t))

;; register table menu under global tools menu
(unless table-disable-menu
  (easy-menu-define table-global-menu-map nil "Table global menu" table-global-menu)
  (if (featurep 'xemacs)
      (progn
	(easy-menu-add-item nil '("Tools") table-global-menu-map))
    (easy-menu-add-item (current-global-map) '("menu-bar" "tools") "--")
    (easy-menu-add-item (current-global-map) '("menu-bar" "tools") table-global-menu-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macros
;;

(defmacro table-with-cache-buffer (&rest body)
  "Execute the forms in BODY with table cache buffer as the current buffer.
This macro simplifies the rest of the work greatly by condensing the
common idiom used in many of the cell manipulation functions.  It does
not return any meaningful value.

Save the current buffer and set the cache buffer as the current
buffer.  Move the point to the cache buffer coordinate
`table-cell-cache-point-coordinate'.  After BODY forms are executed,
the paragraph is filled as long as `table-inhibit-auto-fill-paragraph'
remains nil.  BODY can set it to t when it does not want to fill the
paragraph.  If necessary the cell width and height are extended as the
consequence of cell content modification by the BODY.  Then the
current buffer is restored to the original one.  The last cache point
coordinate is stored in `table-cell-cache-point-coordinate'.  The
original buffer's point is moved to the location that corresponds to
the last cache point coordinate."
  (let ((height-expansion (make-symbol "height-expansion-var-symbol"))
	(width-expansion (make-symbol "width-expansion-var-symbol")))
    `(let (,height-expansion ,width-expansion)
       ;; make sure cache has valid data unless it is explicitly inhibited.
       (unless table-inhibit-update
	 (table-recognize-cell))
       (with-current-buffer (get-buffer-create table-cache-buffer-name)
	 ;; goto the cell coordinate based on `table-cell-cache-point-coordinate'.
	 (set-mark (table--goto-coordinate table-cell-cache-mark-coordinate))
	 (table--goto-coordinate table-cell-cache-point-coordinate)
	 (table--untabify-line)
	 ;; always reset before executing body forms because auto-fill behavior is the default.
	 (setq table-inhibit-auto-fill-paragraph nil)
	 ;; do the body
	 ,@body
	 ;; fill paragraph unless the body does not want to by setting `table-inhibit-auto-fill-paragraph'.
	 (unless table-inhibit-auto-fill-paragraph
	   (if (and table-cell-info-justify
		    (not (eq table-cell-info-justify 'left)))
	       (table--fill-region (point-min) (point-max))
	     (table--fill-region
	      (save-excursion (forward-paragraph -1) (point))
	      (save-excursion (forward-paragraph 1) (point)))))
	 ;; keep the updated cell coordinate.
	 (setq table-cell-cache-point-coordinate (table--get-coordinate))
	 ;; determine the cell width expansion.
	 (setq ,width-expansion (table--measure-max-width))
	 (if (<= ,width-expansion table-cell-info-width) nil
	   (table--fill-region (point-min) (point-max) ,width-expansion)
	   ;; keep the updated cell coordinate.
	   (setq table-cell-cache-point-coordinate (table--get-coordinate)))
	 (setq ,width-expansion (- ,width-expansion table-cell-info-width))
	 ;; determine the cell height expansion.
	 (if (looking-at "\\s *\\'") nil
	   (goto-char (point-min))
	   (if (re-search-forward "\\(\\s *\\)\\'" nil t)
	       (goto-char (match-beginning 1))))
	 (setq ,height-expansion (- (cdr (table--get-coordinate)) (1- table-cell-info-height))))
       ;; now back to the table buffer.
       ;; expand the cell width in the table buffer if necessary.
       (if (> ,width-expansion 0)
	   (table-widen-cell ,width-expansion 'no-copy 'no-update))
       ;; expand the cell height in the table buffer if necessary.
       (if (> ,height-expansion 0)
	   (table-heighten-cell ,height-expansion 'no-copy 'no-update))
       ;; do valign
       (with-current-buffer (get-buffer-create table-cache-buffer-name)
	 (table--goto-coordinate table-cell-cache-point-coordinate)
	 (setq table-cell-cache-point-coordinate (table--valign)))
       ;; move the point in the table buffer to the location that corresponds to
       ;; the location in the cell cache buffer
       (table--goto-coordinate (table--transcoord-cache-to-table table-cell-cache-point-coordinate))
       ;; set up the update timer unless it is explicitly inhibited.
       (unless table-inhibit-update
	 (table--update-cell)))))

;; for debugging the body form of the macro
(put 'table-with-cache-buffer 'edebug-form-spec '(body))
;; for neat presentation use the same indentation as `progn'
(put 'table-with-cache-buffer 'lisp-indent-function 0)
(if (or (featurep 'xemacs)
	(null (fboundp 'font-lock-add-keywords))) nil
  ;; color it as a keyword
  (font-lock-add-keywords
   'emacs-lisp-mode
   '("\\<table-with-cache-buffer\\>")))

(defmacro table-put-source-info (prop value)
  "Register source generation information."
  `(put 'table-source-info-plist ,prop ,value))

(defmacro table-get-source-info (prop)
  "Retrieve source generation information."
  `(get 'table-source-info-plist ,prop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modified commands for cell operation
;;

;; Point Motion Only Group
(mapc
 (lambda (command)
   (let ((func-symbol (intern (format "*table--cell-%s" command)))
	 (doc-string (format "Table remapped function for `%s'." command)))
     (fset func-symbol
	   `(lambda
	      (&rest args)
	      ,doc-string
	      (interactive)
	      (let ((table-inhibit-update t)
		    (deactivate-mark nil))
		(table--finish-delayed-tasks)
		(table-recognize-cell 'force)
		(table-with-cache-buffer
		  (call-interactively ',command)
		  (setq table-inhibit-auto-fill-paragraph t)))))
     (setq table-command-remap-alist
	   (cons (cons command func-symbol)
		 table-command-remap-alist))))
 '(move-beginning-of-line
   beginning-of-line
   move-end-of-line
   end-of-line
   beginning-of-buffer
   end-of-buffer
   forward-word
   backward-word
   forward-sentence
   backward-sentence
   forward-paragraph
   backward-paragraph))

;; Extraction Group
(mapc
 (lambda (command)
   (let ((func-symbol (intern (format "*table--cell-%s" command)))
	 (doc-string (format "Table remapped function for `%s'." command)))
     (fset func-symbol
	   `(lambda
	      (&rest args)
	      ,doc-string
	      (interactive)
	      (table--finish-delayed-tasks)
	      (table-recognize-cell 'force)
	      (table-with-cache-buffer
		(table--remove-cell-properties (point-min) (point-max))
		(table--remove-eol-spaces (point-min) (point-max))
		(call-interactively ',command))
	      (table--finish-delayed-tasks)))
     (setq table-command-remap-alist
	   (cons (cons command func-symbol)
		 table-command-remap-alist))))
 '(kill-region
   kill-ring-save
   delete-region
   copy-region-as-kill
   kill-line
   kill-word
   backward-kill-word
   kill-sentence
   backward-kill-sentence
   kill-paragraph
   backward-kill-paragraph
   kill-sexp
   backward-kill-sexp))

;; Pasting Group
(mapc
 (lambda (command)
   (let ((func-symbol (intern (format "*table--cell-%s" command)))
	 (doc-string (format "Table remapped function for `%s'." command)))
     (fset func-symbol
	   `(lambda
	      (&rest args)
	      ,doc-string
	      (interactive)
	      (table--finish-delayed-tasks)
	      (table-recognize-cell 'force)
	      (table-with-cache-buffer
		(call-interactively ',command)
		(table--untabify (point-min) (point-max))
		(table--fill-region (point-min) (point-max))
		(setq table-inhibit-auto-fill-paragraph t))
	      (table--finish-delayed-tasks)))
     (setq table-command-remap-alist
	   (cons (cons command func-symbol)
		 table-command-remap-alist))))
 '(yank
   clipboard-yank
   yank-clipboard-selection
   insert))

;; Formatting Group
(mapc
 (lambda (command)
   (let ((func-symbol (intern (format "*table--cell-%s" command)))
	 (doc-string (format "Table remapped function for `%s'." command)))
     (fset func-symbol
	   `(lambda
	      (&rest args)
	      ,doc-string
	      (interactive)
	      (table--finish-delayed-tasks)
	      (table-recognize-cell 'force)
	      (table-with-cache-buffer
		(let ((fill-column table-cell-info-width))
		  (call-interactively ',command))
		(setq table-inhibit-auto-fill-paragraph t))
	      (table--finish-delayed-tasks)))
     (setq table-command-remap-alist
	   (cons (cons command func-symbol)
		 table-command-remap-alist))))
 '(center-line
   center-region
   center-paragraph
   fill-paragraph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commands
;;

;;;###autoload
(defun table-insert (columns rows &optional cell-width cell-height)
  "Insert an editable text table.
Insert a table of specified number of COLUMNS and ROWS.  Optional
parameter CELL-WIDTH and CELL-HEIGHT can specify the size of each
cell.  The cell size is uniform across the table if the specified size
is a number.  They can be a list of numbers to specify different size
for each cell.  When called interactively, the list of number is
entered by simply listing all the numbers with space characters
delimiting them.

Examples:

\\[table-insert] inserts a table at the current point location.

Suppose we have the following situation where `-!-' indicates the
location of point.

    -!-

Type \\[table-insert] and hit ENTER key.  As it asks table
specification, provide 3 for number of columns, 1 for number of rows,
5 for cell width and 1 for cell height.  Now you shall see the next
table and the point is automatically moved to the beginning of the
first cell.

    +-----+-----+-----+
    |-!-  |     |     |
    +-----+-----+-----+

Inside a table cell, there are special key bindings. \\<table-cell-map>

M-9 \\[table-widen-cell] (or \\[universal-argument] 9 \\[table-widen-cell]) widens the first cell by 9 character
width, which results as

    +--------------+-----+-----+
    |-!-           |     |     |
    +--------------+-----+-----+

Type TAB \\[table-widen-cell] then type TAB M-2 M-7 \\[table-widen-cell] (or \\[universal-argument] 2 7 \\[table-widen-cell]).  Typing
TAB moves the point forward by a cell. The result now looks like this:

    +--------------+------+--------------------------------+
    |              |      |-!-                             |
    +--------------+------+--------------------------------+

If you knew each width of the columns prior to the table creation,
what you could have done better was to have had given the complete
width information to `table-insert'.

Cell width(s): 14 6 32

instead of

Cell width(s): 5

This would have eliminated the previously mentioned width adjustment
work all together.

If the point is in the last cell type S-TAB S-TAB to move it to the
first cell.  Now type \\[table-heighten-cell] which heighten the row by a line.

    +--------------+------+--------------------------------+
    |-!-           |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+

Type \\[table-insert-row-column] and tell it to insert a row.

    +--------------+------+--------------------------------+
    |-!-           |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+

Move the point under the table as shown below.

    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    -!-

Type M-x table-insert-row instead of \\[table-insert-row-column].  \\[table-insert-row-column] does not work
when the point is outside of the table.  This insertion at
outside of the table effectively appends a row at the end.

    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    |-!-           |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+

Text editing inside the table cell produces reasonably expected
results.

    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    |              |      |Text editing inside the table   |
    |              |      |cell produces reasonably        |
    |              |      |expected results.-!-            |
    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+

Inside a table cell has a special keymap.

\\{table-cell-map}
"
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (if (table--probe-cell)
	 (error "Can't insert a table inside a table"))
     (mapcar (function table--read-from-minibuffer)
	     '(("Number of columns" . table-columns-history)
	       ("Number of rows" . table-rows-history)
	       ("Cell width(s)" . table-cell-width-history)
	       ("Cell height(s)" . table-cell-height-history)))))
  (table--make-cell-map)
  ;; reform the arguments.
  (if (null cell-width) (setq cell-width (car table-cell-width-history)))
  (if (null cell-height) (setq cell-height (car table-cell-height-history)))
  (if (stringp columns) (setq columns (string-to-number columns)))
  (if (stringp rows) (setq rows (string-to-number rows)))
  (if (stringp cell-width) (setq cell-width (table--string-to-number-list cell-width)))
  (if (stringp cell-height) (setq cell-height (table--string-to-number-list cell-height)))
  (if (numberp cell-width) (setq cell-width (cons cell-width nil)))
  (if (numberp cell-height) (setq cell-height (cons cell-height nil)))
  ;; test validity of the arguments.
  (mapc (lambda (arg)
	  (let* ((value (symbol-value arg))
		 (error-handler
		  (function (lambda ()
		    (error "%s must be a positive integer%s" arg
			   (if (listp value) " or a list of positive integers" ""))))))
	    (if (null value) (funcall error-handler))
	    (mapcar (function (lambda (arg1)
		      (if (or (not (integerp arg1))
			      (< arg1 1))
			  (funcall error-handler))))
		    (if (listp value) value
		      (cons value nil)))))
	'(columns rows cell-width cell-height))
  (let ((orig-coord (table--get-coordinate))
	(coord (table--get-coordinate))
	r i cw ch cell-str border-str)
    ;; prefabricate the building blocks border-str and cell-str.
    (with-temp-buffer
      ;; construct border-str
      (insert table-cell-intersection-char)
      (setq cw cell-width)
      (setq i 0)
      (while (< i columns)
	(insert (make-string (car cw) (string-to-char table-cell-horizontal-chars)) table-cell-intersection-char)
	(if (cdr cw) (setq cw (cdr cw)))
	(setq i (1+ i)))
      (setq border-str (buffer-substring (point-min) (point-max)))
      ;; construct cell-str
      (erase-buffer)
      (insert table-cell-vertical-char)
      (setq cw cell-width)
      (setq i 0)
      (while (< i columns)
	(let ((beg (point)))
	  (insert (make-string (car cw) ?\s))
	  (insert table-cell-vertical-char)
	  (table--put-cell-line-property beg (1- (point))))
	(if (cdr cw) (setq cw (cdr cw)))
	(setq i (1+ i)))
      (setq cell-str (buffer-substring (point-min) (point-max))))
    ;; if the construction site has an empty border push that border down.
    (save-excursion
      (beginning-of-line)
      (if (looking-at "\\s *$")
	  (progn
	    (setq border-str (concat border-str "\n"))
	    (setq cell-str (concat cell-str "\n")))))
    ;; now build the table using the prefabricated building blocks
    (setq r 0)
    (setq ch cell-height)
    (while (< r rows)
      (if (> r 0) nil
	(table--goto-coordinate coord) (setcdr coord (1+ (cdr coord)))
	(table--untabify-line (point))
	(insert border-str))
      (setq i 0)
      (while (< i (car ch))
	(table--goto-coordinate coord) (setcdr coord (1+ (cdr coord)))
	(table--untabify-line (point))
	(insert cell-str)
	(setq i (1+ i)))
      (table--goto-coordinate coord) (setcdr coord (1+ (cdr coord)))
      (table--untabify-line (point))
      (insert border-str)
      (if (cdr ch) (setq ch (cdr ch)))
      (setq r (1+ r)))
    ;; stand by at the first cell
    (table--goto-coordinate (table--offset-coordinate orig-coord '(1 . 1)))
    (table-recognize-cell 'force)))

;;;###autoload
(defun table-insert-row (n)
  "Insert N table row(s).
When point is in a table the newly inserted row(s) are placed above
the current row.  When point is outside of the table it must be below
the table within the table width range, then the newly created row(s)
are appended at the bottom of the table."
  (interactive "*p")
  (if (< n 0) (setq n 1))
  (let* ((current-coordinate (table--get-coordinate))
	 (coord-list (table--cell-list-to-coord-list (table--horizontal-cell-list t nil 'top)))
	 (append-row (if coord-list nil (setq coord-list (table--find-row-column))))
	 (cell-height (cdr (table--min-coord-list coord-list)))
	 (left-list nil)
	 (this-list coord-list)
	 (right-list (cdr coord-list))
	 (bottom-border-y (1+ (cdr (table--get-coordinate (cdr (table--vertical-cell-list nil t))))))
	 (vertical-str (string table-cell-vertical-char))
	 (vertical-str-with-properties (let ((str (string table-cell-vertical-char)))
					 (table--put-cell-keymap-property 0 (length str) str)
					 (table--put-cell-rear-nonsticky 0 (length str) str) str))
	 (first-time t))
    ;; create the space below for the table to grow
    (table--create-growing-space-below (* n (+ 1 cell-height)) coord-list bottom-border-y)
    ;; vertically expand each cell from left to right
    (while this-list
      (let* ((left (prog1 (car left-list) (setq left-list (if left-list (cdr left-list) coord-list))))
	     (this (prog1 (car this-list) (setq this-list (cdr this-list))))
	     (right (prog1 (car right-list) (setq right-list (cdr right-list))))
	     (exclude-left (and left (< (cdar left) (cdar this))))
	     (exclude-right (and right (<= (cdar right) (cdar this))))
	     (beg (table--goto-coordinate
		   (cons (if exclude-left (caar this) (1- (caar this)))
			 (cdar this))))
	     (end (table--goto-coordinate
		   (cons (if exclude-right (cadr this) (1+ (cadr this)))
			 bottom-border-y)))
	     (rect (if append-row nil (extract-rectangle beg end))))
	;; prepend blank cell lines to the extracted rectangle
	(let ((i n))
	  (while (> i 0)
	    (setq rect (cons
			(concat (if exclude-left "" (char-to-string table-cell-intersection-char))
				(make-string (- (cadr this) (caar this)) (string-to-char table-cell-horizontal-chars))
				(if exclude-right "" (char-to-string table-cell-intersection-char)))
			rect))
	    (let ((j cell-height))
	      (while (> j 0)
		(setq rect (cons
			    (concat (if exclude-left ""
				      (if first-time vertical-str vertical-str-with-properties))
				    (table--cell-blank-str (- (cadr this) (caar this)))
				    (if exclude-right "" vertical-str-with-properties))
			    rect))
		(setq j (1- j))))
	    (setq i (1- i))))
	(setq first-time nil)
	(if append-row
	    (table--goto-coordinate (cons (if exclude-left (caar this) (1- (caar this)))
					  (1+ bottom-border-y)))
	  (delete-rectangle beg end)
	  (goto-char beg))
	(table--insert-rectangle rect)))
    ;; fix up the intersections
    (setq this-list (if append-row nil coord-list))
    (while this-list
      (let ((this (prog1 (car this-list) (setq this-list (cdr this-list))))
	    (i 0))
	(while (< i n)
	  (let ((y (1- (* i (+ 1 cell-height)))))
	    (table--goto-coordinate (table--offset-coordinate (car this) (cons -1  y)))
	    (delete-char 1) (insert table-cell-intersection-char)
	    (table--goto-coordinate (table--offset-coordinate (cons (cadr this) (cdar this)) (cons 0  y)))
	    (delete-char 1) (insert table-cell-intersection-char)
	    (setq i (1+ i))))))
    ;; move the point to the beginning of the first newly inserted cell.
    (if (table--goto-coordinate
	 (if append-row (cons (car (caar coord-list)) (1+ bottom-border-y))
	   (caar coord-list))) nil
      (table--goto-coordinate current-coordinate))
    ;; re-recognize the current cell's new dimension
    (table-recognize-cell 'force)))

;;;###autoload
(defun table-insert-column (n)
  "Insert N table column(s).
When point is in a table the newly inserted column(s) are placed left
of the current column.  When point is outside of the table it must be
right side of the table within the table height range, then the newly
created column(s) are appended at the right of the table."
  (interactive "*p")
  (if (< n 0) (setq n 1))
  (let* ((current-coordinate (table--get-coordinate))
	 (coord-list (table--cell-list-to-coord-list (table--vertical-cell-list t nil 'left)))
	 (append-column (if coord-list nil (setq coord-list (table--find-row-column 'column))))
	 (cell-width (car (table--min-coord-list coord-list)))
	 (border-str (table--multiply-string (concat (make-string cell-width (string-to-char table-cell-horizontal-chars))
						     (char-to-string table-cell-intersection-char)) n))
	 (cell-str (table--multiply-string (concat (table--cell-blank-str cell-width)
						   (let ((str (string table-cell-vertical-char)))
						     (table--put-cell-keymap-property 0 (length str) str)
						     (table--put-cell-rear-nonsticky 0 (length str) str) str)) n))
	 (columns-to-extend (* n (+ 1 cell-width)))
	 (above-list nil)
	 (this-list coord-list)
	 (below-list (cdr coord-list))
	 (right-border-x (car (table--get-coordinate (cdr (table--horizontal-cell-list nil t))))))
    ;; push back the affected area above and below this table
    (table--horizontally-shift-above-and-below columns-to-extend coord-list)
    ;; process each cell vertically from top to bottom
    (while this-list
      (let* ((above (prog1 (car above-list) (setq above-list (if above-list (cdr above-list) coord-list))))
	     (this (prog1 (car this-list) (setq this-list (cdr this-list))))
	     (below (prog1 (car below-list) (setq below-list (cdr below-list))))
	     (exclude-above (and above (<= (caar above) (caar this))))
	     (exclude-below (and below (< (caar below) (caar this))))
	     (beg-coord (cons (if append-column (1+ right-border-x) (caar this))
			      (if exclude-above (cdar this) (1- (cdar this)))))
	     (end-coord (cons (1+ right-border-x)
			      (if exclude-below (cddr this) (1+ (cddr this)))))
	     rect)
	;; untabify the area right of the bar that is about to be inserted
	(let ((coord (table--copy-coordinate beg-coord))
	      (i 0)
	      (len (length rect)))
	  (while (< i len)
	    (if (table--goto-coordinate coord 'no-extension)
		(table--untabify-line (point)))
	    (setcdr coord (1+ (cdr coord)))
	    (setq i (1+ i))))
	;; extract and delete the rectangle area including the current
	;; cell and to the right border of the table.
	(setq rect (extract-rectangle (table--goto-coordinate beg-coord)
				      (table--goto-coordinate end-coord)))
	(delete-rectangle (table--goto-coordinate beg-coord)
			  (table--goto-coordinate end-coord))
	;; prepend the empty column string at the beginning of each
	;; rectangle string extracted before.
	(let ((rect-str rect)
	      (first t))
	  (while rect-str
	    (if (and first (null exclude-above))
		(setcar rect-str (concat border-str (car rect-str)))
	      (if (and (null (cdr rect-str)) (null exclude-below))
		  (setcar rect-str (concat border-str (car rect-str)))
		(setcar rect-str (concat cell-str (car rect-str)))))
	    (setq first nil)
	    (setq rect-str (cdr rect-str))))
	;; insert the extended rectangle
	(table--goto-coordinate beg-coord)
	(table--insert-rectangle rect)))
    ;; fix up the intersections
    (setq this-list (if append-column nil coord-list))
    (while this-list
      (let ((this (prog1 (car this-list) (setq this-list (cdr this-list))))
	    (i 0))
	(while (< i n)
	  (let ((x (1- (* (1+ i) (+ 1 cell-width)))))
	    (table--goto-coordinate (table--offset-coordinate (car this) (cons x  -1)))
	    (delete-char 1) (insert table-cell-intersection-char)
	    (table--goto-coordinate (table--offset-coordinate (cons (caar this) (cddr this)) (cons x  1)))
	    (delete-char 1) (insert table-cell-intersection-char)
	    (setq i (1+ i))))))
    ;; move the point to the beginning of the first newly inserted cell.
    (if (table--goto-coordinate
	 (if append-column
	     (cons (1+ right-border-x)
		   (cdar (car coord-list)))
	   (caar coord-list))) nil
      (table--goto-coordinate current-coordinate))
    ;; re-recognize the current cell's new dimension
    (table-recognize-cell 'force)))

;;;###autoload
(defun table-insert-row-column (row-column n)
  "Insert row(s) or column(s).
See `table-insert-row' and `table-insert-column'."
  (interactive
   (let ((n (prefix-numeric-value current-prefix-arg)))
     (if (< n 0) (setq n 1))
     (list (intern (let ((completion-ignore-case t)
			 (default (car table-insert-row-column-history)))
		     (downcase (completing-read
				(format "Insert %s row%s/column%s (default %s): "
					(if (> n 1) (format "%d" n) "a")
					(if (> n 1) "s" "")
					(if (> n 1) "s" "")
					default)
				'(("row") ("column"))
				nil t nil 'table-insert-row-column-history default))))
	   n)))
  (cond ((eq row-column 'row)
	 (table-insert-row n))
	((eq row-column 'column)
	 (table-insert-column n))))

;;;###autoload
(defun table-recognize (&optional arg)
  "Recognize all tables within the current buffer and activate them.
Scans the entire buffer and recognizes valid table cells.  If the
optional numeric prefix argument ARG is negative the tables in the
buffer become inactive, meaning the tables become plain text and loses
all the table specific features."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let* ((inhibit-read-only t))
    (table-recognize-region (point-min) (point-max) -1)
    (if (>= arg 0)
	(save-excursion
	  (goto-char (point-min))
	  (let* ((border (format "[%s%c%c]"
				 table-cell-horizontal-chars
				 table-cell-vertical-char
				 table-cell-intersection-char))
		 (border3 (concat border border border))
		 (non-border (format "^[^%s%c%c]*$"
				     table-cell-horizontal-chars
				     table-cell-vertical-char
				     table-cell-intersection-char)))
	    ;; `table-recognize-region' is an expensive function so minimize
	    ;; the search area.  A minimum table at least consists of three consecutive
	    ;; table border characters to begin with such as
	    ;; +-+
	    ;; |A|
	    ;; +-+
	    ;; and any tables end with a line containing no table border characters
	    ;; or the end of buffer.
	    (while (and (re-search-forward border3 (point-max) t)
			(not (and (input-pending-p)
				  table-abort-recognition-when-input-pending)))
	      (message "Recognizing tables...(%d%%)" (/ (* 100 (match-beginning 0)) (- (point-max) (point-min))))
	      (let ((beg (match-beginning 0))
		    end)
		(if (re-search-forward non-border (point-max) t)
		    (setq end (match-beginning 0))
		  (setq end (goto-char (point-max))))
		(table-recognize-region beg end arg)))
	    (message "Recognizing tables...done"))))))

;;;###autoload
(defun table-unrecognize ()
  (interactive)
  (table-recognize -1))

;;;###autoload
(defun table-recognize-region (beg end &optional arg)
  "Recognize all tables within region.
BEG and END specify the region to work on.  If the optional numeric
prefix argument ARG is negative the tables in the region become
inactive, meaning the tables become plain text and lose all the table
specific features."
  (interactive "r\nP")
  (setq arg (prefix-numeric-value arg))
  (let ((inhibit-read-only t)
	(modified-flag (buffer-modified-p)))
    (if (< arg 0)
	(table--remove-cell-properties beg end)
      (save-excursion
	(goto-char beg)
	(let* ((border (format "[%s%c%c]"
			       table-cell-horizontal-chars
			       table-cell-vertical-char
			       table-cell-intersection-char))
	       (non-border (format "[^%s%c%c]"
				   table-cell-horizontal-chars
				   table-cell-vertical-char
				   table-cell-intersection-char))
	       (inhibit-read-only t))
	  (unwind-protect
	      (progn
		(remove-text-properties beg end '(table-cell nil))
		(while (and (< (point) end)
			    (not (and (input-pending-p)
				      table-abort-recognition-when-input-pending)))
		  (cond
		   ((looking-at "\n")
		    (forward-char 1))
		   ((looking-at border)
		    (if (re-search-forward non-border end t)
			(goto-char (match-beginning 0))
		      (goto-char end)))
		   ((table--at-cell-p (point))
		    (goto-char (next-single-property-change (point) 'table-cell nil end)))
		   (t
		    (let ((cell (table-recognize-cell 'force 'no-copy)))
		      (if (and cell table-detect-cell-alignment)
			  (table--detect-cell-alignment cell)))
		    (unless (re-search-forward border end t)
		      (goto-char end))))))))))
    (restore-buffer-modified-p modified-flag)))

;;;###autoload
(defun table-unrecognize-region (beg end)
  (interactive "r")
  (table-recognize-region beg end -1))

;;;###autoload
(defun table-recognize-table (&optional arg)
  "Recognize a table at point.
If the optional numeric prefix argument ARG is negative the table
becomes inactive, meaning the table becomes plain text and loses all
the table specific features."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let ((unrecognize (< arg 0))
	(origin-cell (table--probe-cell))
	(inhibit-read-only t))
    (if origin-cell
	(save-excursion
	  (while
	      (progn
		(table-forward-cell 1 nil unrecognize)
		(let ((cell (table--probe-cell)))
		  (if (and cell table-detect-cell-alignment)
		      (table--detect-cell-alignment cell))
		  (and cell (not (equal cell origin-cell))))))))))

;;;###autoload
(defun table-unrecognize-table ()
  (interactive)
  (table-recognize-table -1))

;;;###autoload
(defun table-recognize-cell (&optional force no-copy arg)
  "Recognize a table cell that contains current point.
Probe the cell dimension and prepare the cell information.  The
optional two arguments FORCE and NO-COPY are for internal use only and
must not be specified.  When the optional numeric prefix argument ARG
is negative the cell becomes inactive, meaning that the cell becomes
plain text and loses all the table specific features."
  (interactive "i\ni\np")
  (table--make-cell-map)
  (if (or force (not (memq (table--get-last-command) table-command-list)))
      (let* ((cell (table--probe-cell (called-interactively-p 'interactive)))
	     (cache-buffer (get-buffer-create table-cache-buffer-name))
	     (modified-flag (buffer-modified-p))
	     (inhibit-read-only t))
	(unwind-protect
	    (unless (null cell)
	      ;; initialize the cell info variables
	      (let ((lu-coordinate (table--get-coordinate (car cell)))
		    (rb-coordinate (table--get-coordinate (cdr cell))))
		;; update the previous cell if this cell is different from the previous one.
		;; care only lu but ignore rb since size change does not matter.
		(unless (equal table-cell-info-lu-coordinate lu-coordinate)
		  (table--finish-delayed-tasks))
		(setq table-cell-info-lu-coordinate lu-coordinate)
		(setq table-cell-info-rb-coordinate rb-coordinate)
		(setq table-cell-info-width (- (car table-cell-info-rb-coordinate)
					       (car table-cell-info-lu-coordinate)))
		(setq table-cell-info-height (+ (- (cdr table-cell-info-rb-coordinate)
						   (cdr table-cell-info-lu-coordinate)) 1))
		(setq table-cell-info-justify (table--get-cell-justify-property cell))
		(setq table-cell-info-valign (table--get-cell-valign-property cell)))
	      ;; set/remove table cell properties
	      (if (< (prefix-numeric-value arg) 0)
		  (let ((coord (table--get-coordinate (car cell)))
			(n table-cell-info-height))
		    (save-excursion
		      (while (> n 0)
			(table--remove-cell-properties
			 (table--goto-coordinate coord)
			 (table--goto-coordinate (cons (+ (car coord) table-cell-info-width 1) (cdr coord))))
			(setq n (1- n))
			(setcdr coord (1+ (cdr coord))))))
		(table--put-cell-property cell))
	      ;; copy the cell contents to the cache buffer
	      ;; only if no-copy is nil and timers are not set
	      (unless no-copy
		(setq table-cell-cache-point-coordinate (table--transcoord-table-to-cache))
		(setq table-cell-cache-mark-coordinate (table--transcoord-table-to-cache
							(table--get-coordinate (marker-position (mark-marker)))))
		(setq table-cell-buffer (current-buffer))
		(let ((rectangle (extract-rectangle (car cell)
						    (cdr cell))))
		  (save-current-buffer
		    (set-buffer cache-buffer)
		    (erase-buffer)
		    (table--insert-rectangle rectangle)))))
	  (restore-buffer-modified-p modified-flag))
	(if (featurep 'xemacs)
	    (table--warn-incompatibility))
	cell)))

;;;###autoload
(defun table-unrecognize-cell ()
  (interactive)
  (table-recognize-cell nil nil -1))

;;;###autoload
(defun table-heighten-cell (n &optional no-copy no-update)
  "Heighten the current cell by N lines by expanding the cell vertically.
Heightening is done by adding blank lines at the bottom of the current
cell.  Other cells aligned horizontally with the current one are also
heightened in order to keep the rectangular table structure.  The
optional argument NO-COPY is internal use only and must not be
specified."
  (interactive "*p")
  (if (< n 0) (setq n 1))
  (let* ((coord-list (table--cell-list-to-coord-list (table--horizontal-cell-list t)))
	 (left-list nil)
	 (this-list coord-list)
	 (right-list (cdr coord-list))
	 (bottom-border-y (1+ (cdr (table--get-coordinate (cdr (table--vertical-cell-list nil t))))))
	 (vertical-str (string table-cell-vertical-char))
	 (vertical-str-with-properties (string table-cell-vertical-char))
	 (first-time t)
	 (current-coordinate (table--get-coordinate)))
    ;; prepare the right vertical string with appropriate properties put
    (table--put-cell-keymap-property 0 (length vertical-str-with-properties) vertical-str-with-properties)
    ;; create the space below for the table to grow
    (table--create-growing-space-below n coord-list bottom-border-y)
    ;; vertically expand each cell from left to right
    (while this-list
      (let* ((left (prog1 (car left-list) (setq left-list (if left-list (cdr left-list) coord-list))))
	     (this (prog1 (car this-list) (setq this-list (cdr this-list))))
	     (right (prog1 (car right-list) (setq right-list (cdr right-list))))
	     (exclude-left (and left (< (cddr left) (cddr this))))
	     (exclude-right (and right (<= (cddr right) (cddr this))))
	     (beg (table--goto-coordinate
		   (cons (if exclude-left (caar this) (1- (caar this)))
			 (1+ (cddr this)))))
	     (end (table--goto-coordinate
		   (cons (if exclude-right (cadr this) (1+ (cadr this)))
			 bottom-border-y)))
	     (rect (extract-rectangle beg end)))
	;; prepend blank cell lines to the extracted rectangle
	(let ((i n))
	  (while (> i 0)
	    (setq rect (cons
			(concat (if exclude-left ""
				  (if first-time vertical-str vertical-str-with-properties))
				(table--cell-blank-str (- (cadr this) (caar this)))
				(if exclude-right "" vertical-str-with-properties))
			rect))
	    (setq i (1- i))))
	(setq first-time nil)
	(delete-rectangle beg end)
	(goto-char beg)
	(table--insert-rectangle rect)))
    (table--goto-coordinate current-coordinate)
    ;; re-recognize the current cell's new dimension
    (table-recognize-cell 'force no-copy)
    (unless no-update
      (table--update-cell-heightened))))

;;;###autoload
(defun table-shorten-cell (n)
  "Shorten the current cell by N lines by shrinking the cell vertically.
Shortening is done by removing blank lines from the bottom of the cell
and possibly from the top of the cell as well.  Therefore, the cell
must have some bottom/top blank lines to be shorten effectively.  This
is applicable to all the cells aligned horizontally with the current
one because they are also shortened in order to keep the rectangular
table structure."
  (interactive "*p")
  (if (< n 0) (setq n 1))
  (table--finish-delayed-tasks)
  (let* ((table-inhibit-update t)
	 (coord-list (table--cell-list-to-coord-list (table--horizontal-cell-list t)))
	 (left-list nil)
	 (this-list coord-list)
	 (right-list (cdr coord-list))
	 (bottom-budget-list nil)
	 (bottom-border-y (1+ (cdr (table--get-coordinate (cdr (table--vertical-cell-list nil t))))))
	 (current-coordinate (table--get-coordinate))
	 (current-cell-coordinate (table--cell-to-coord (table--probe-cell)))
	 (blank-line-regexp "\\s *$"))
    (message "Shortening...");; this operation may be lengthy
    ;; for each cell calculate the maximum number of blank lines we can delete
    ;; and adjust the argument n.  n is adjusted so that the total number of
    ;; blank lines from top and bottom of a cell do not exceed n, all cell has
    ;; at least one line height after blank line deletion.
    (while this-list
      (let ((this (prog1 (car this-list) (setq this-list (cdr this-list)))))
	(table--goto-coordinate (car this))
	(table-recognize-cell 'force)
	(table-with-cache-buffer
	  (catch 'end-count
	    (let ((blank-line-count 0))
	      (table--goto-coordinate (cons 0 (1- table-cell-info-height)))
	      ;; count bottom
	      (while (and (looking-at blank-line-regexp)
			  (setq blank-line-count (1+ blank-line-count))
			  ;; need to leave at least one blank line
			  (if (> blank-line-count n) (throw 'end-count nil) t)
			  (if (zerop (forward-line -1)) t
			    (setq n (if (zerop blank-line-count) 0
				      (1- blank-line-count)))
			    (throw 'end-count nil))))
	      (table--goto-coordinate (cons 0 0))
	      ;; count top
	      (while (and (looking-at blank-line-regexp)
			  (setq blank-line-count (1+ blank-line-count))
			  ;; can consume all blank lines
			  (if (>= blank-line-count n) (throw 'end-count nil) t)
			  (zerop (forward-line 1))))
	      (setq n blank-line-count))))))
    ;; construct the bottom-budget-list which is a list of numbers where each number
    ;; corresponds to how many lines to be deleted from the bottom of each cell.  If
    ;; this number, say bb, is smaller than n (bb < n) that means the difference (n - bb)
    ;; number of lines must be deleted from the top of the cell in addition to deleting
    ;; bb lines from the bottom of the cell.
    (setq this-list coord-list)
    (while this-list
      (let ((this (prog1 (car this-list) (setq this-list (cdr this-list)))))
	(table--goto-coordinate (car this))
	(table-recognize-cell 'force)
	(table-with-cache-buffer
	  (setq bottom-budget-list
		(cons
		 (let ((blank-line-count 0))
		   (table--goto-coordinate (cons 0 (1- table-cell-info-height)))
		   (while (and (looking-at blank-line-regexp)
			       (< blank-line-count n)
			       (setq blank-line-count (1+ blank-line-count))
			       (zerop (forward-line -1))))
		   blank-line-count)
		 bottom-budget-list)))))
    (setq bottom-budget-list (nreverse bottom-budget-list))
    ;; vertically shorten each cell from left to right
    (setq this-list coord-list)
    (while this-list
      (let* ((left (prog1 (car left-list) (setq left-list (if left-list (cdr left-list) coord-list))))
	     (this (prog1 (car this-list) (setq this-list (cdr this-list))))
	     (right (prog1 (car right-list) (setq right-list (cdr right-list))))
	     (bottom-budget (prog1 (car bottom-budget-list) (setq bottom-budget-list (cdr bottom-budget-list))))
	     (exclude-left (and left (< (cddr left) (cddr this))))
	     (exclude-right (and right (<= (cddr right) (cddr this))))
	     (beg (table--goto-coordinate (cons (caar this) (cdar this))))
	     (end (table--goto-coordinate (cons (cadr this) bottom-border-y)))
	     (rect (extract-rectangle beg end))
	     (height (+ (- (cddr this) (cdar this)) 1))
	     (blank-line (make-string (- (cadr this) (caar this)) ?\s)))
	;; delete lines from the bottom of the cell
	(setcdr (nthcdr (- height bottom-budget 1) rect) (nthcdr height rect))
	;; delete lines from the top of the cell
	(if (> n bottom-budget)
	    (let ((props (text-properties-at 0 (car rect))))
	      (setq rect (nthcdr (- n bottom-budget) rect))
	      (set-text-properties 0 1 props (car rect))))
	;; append blank lines below the table
	(setq rect (append rect (make-list n blank-line)))
	;; now swap the area with the prepared rect of the same size
	(delete-rectangle beg end)
	(goto-char beg)
	(table--insert-rectangle rect)
	;; for the left and right borders always delete lines from the bottom of the cell
	(unless exclude-left
	  (let* ((beg (table--goto-coordinate (cons (1- (caar this)) (cdar this))))
		 (end (table--goto-coordinate (cons (caar this) bottom-border-y)))
		 (rect (extract-rectangle beg end)))
	    (setcdr (nthcdr (- height n 1) rect) (nthcdr height rect))
	    (setq rect (append rect (make-list n " ")))
	    (delete-rectangle beg end)
	    (goto-char beg)
	    (table--insert-rectangle rect)))
	(unless exclude-right
	  (let* ((beg (table--goto-coordinate (cons (cadr this) (cdar this))))
		 (end (table--goto-coordinate (cons (1+ (cadr this)) bottom-border-y)))
		 (rect (extract-rectangle beg end)))
	    (setcdr (nthcdr (- height n 1) rect) (nthcdr height rect))
	    (setq rect (append rect (make-list n " ")))
	    (delete-rectangle beg end)
	    (goto-char beg)
	    (table--insert-rectangle rect)))
	;; if this is the cell where the original point was in, adjust the point location
	(if (null (equal this current-cell-coordinate)) nil
	  (let ((y (- (cdr current-coordinate) (cdar this))))
	    (if (< y (- n bottom-budget))
		(setcdr current-coordinate (cdar this))
	      (if (< (- y (- n bottom-budget)) (- height n))
		  (setcdr current-coordinate (+ (cdar this) (- y (- n bottom-budget))))
		(setcdr current-coordinate (+ (cdar this) (- height n 1)))))))))
    ;; remove the appended blank lines below the table if they are unnecessary
    (table--goto-coordinate (cons 0 (1+ (- bottom-border-y n))))
    (table--remove-blank-lines n)
    ;; re-recognize the current cell's new dimension
    (table--goto-coordinate current-coordinate)
    (table-recognize-cell 'force)
    (table--update-cell-heightened)
    (message "")))

;;;###autoload
(defun table-widen-cell (n &optional no-copy no-update)
  "Widen the current cell by N columns and expand the cell horizontally.
Some other cells in the same table are widen as well to keep the
table's rectangle structure."
  (interactive "*p")
  (if (< n 0) (setq n 1))
  (let* ((coord-list (table--cell-list-to-coord-list (table--vertical-cell-list)))
	 (below-list nil)
	 (this-list coord-list)
	 (above-list (cdr coord-list)))
    (save-excursion
      ;; push back the affected area above and below this table
      (table--horizontally-shift-above-and-below n (reverse coord-list))
      ;; now widen vertically for each cell
      (while this-list
	(let* ((below (prog1 (car below-list) (setq below-list (if below-list (cdr below-list) coord-list))))
	       (this (prog1 (car this-list) (setq this-list (cdr this-list))))
	       (above (prog1 (car above-list) (setq above-list (cdr above-list))))
	       (beg (table--goto-coordinate
		     (cons (car (cdr this))
			   (if (or (null above) (<= (car (cdr this)) (car (cdr above))))
			       (1- (cdr (car this)))
			     (cdr (car this))))))
	       (end (table--goto-coordinate
		     (cons (1+ (car (cdr this)))
			   (if (or (null below) (< (car (cdr this)) (car (cdr below))))
			       (1+ (cdr (cdr this)))
			     (cdr (cdr this))))))
	       (tmp (extract-rectangle (1- beg) end))
	       (border (format "[%s%c]\\%c"
			       table-cell-horizontal-chars
			       table-cell-intersection-char
			       table-cell-intersection-char))
	       (blank (table--cell-blank-str))
	       rectangle)
	  ;; create a single wide vertical bar of empty cell fragment
	  (while tmp
;        (message "tmp is %s" tmp)
	    (setq rectangle (cons
                         (if (string-match border (car tmp))
				      (substring (car tmp) 0 1)
				    blank)
				  rectangle))
;        (message "rectangle is %s" rectangle)
	    (setq tmp (cdr tmp)))
	  (setq rectangle (nreverse rectangle))
	  ;; untabify the area right of the bar that is about to be inserted
	  (let ((coord (table--get-coordinate beg))
		(i 0)
		(len (length rectangle)))
	    (while (< i len)
	      (if (table--goto-coordinate coord 'no-extension)
		  (table--untabify-line (point)))
	      (setcdr coord (1+ (cdr coord)))
	      (setq i (1+ i))))
	  ;; insert the bar n times
	  (goto-char beg)
	  (let ((i 0))
	    (while (< i n)
	      (save-excursion
		(table--insert-rectangle rectangle))
	      (setq i (1+ i)))))))
    (table-recognize-cell 'force no-copy)
    (unless no-update
      (table--update-cell-widened))))

;;;###autoload
(defun table-narrow-cell (n)
  "Narrow the current cell by N columns and shrink the cell horizontally.
Some other cells in the same table are narrowed as well to keep the
table's rectangle structure."
  (interactive "*p")
  (if (< n 0) (setq n 1))
  (table--finish-delayed-tasks)
  (let* ((coord-list (table--cell-list-to-coord-list (table--vertical-cell-list)))
	 (current-cell (table--cell-to-coord (table--probe-cell)))
	 (current-coordinate (table--get-coordinate))
	 tmp-list)
    (message "Narrowing...");; this operation may be lengthy
    ;; determine the doable n by try narrowing each cell.
    (setq tmp-list coord-list)
    (while tmp-list
      (let ((cell (prog1 (car tmp-list) (setq tmp-list (cdr tmp-list))))
	    (table-inhibit-update t)
	    cell-n)
	(table--goto-coordinate (car cell))
	(table-recognize-cell 'force)
	(table-with-cache-buffer
	  (table--fill-region (point-min) (point-max) (- table-cell-info-width n))
	  (if (< (setq cell-n (- table-cell-info-width (table--measure-max-width))) n)
	      (setq n cell-n))
	  (erase-buffer)
	  (setq table-inhibit-auto-fill-paragraph t))))
    (if (< n 1) nil
      ;; narrow only the contents of each cell but leave the cell frame as is because
      ;; we need to have valid frame structure in order for table-with-cache-buffer
      ;; to work correctly.
      (setq tmp-list coord-list)
      (while tmp-list
	(let* ((cell (prog1 (car tmp-list) (setq tmp-list (cdr tmp-list))))
	       (table-inhibit-update t)
	       (currentp (equal cell current-cell))
	       old-height)
	  (if currentp (table--goto-coordinate current-coordinate)
	    (table--goto-coordinate (car cell)))
	  (table-recognize-cell 'force)
	  (setq old-height table-cell-info-height)
	  (table-with-cache-buffer
	    (let ((out-of-bound (>= (- (car current-coordinate) (car table-cell-info-lu-coordinate))
				    (- table-cell-info-width n)))
		  (sticky (and currentp
			       (save-excursion
				 (unless (bolp) (forward-char -1))
				 (looking-at ".*\\S ")))))
	      (table--fill-region (point-min) (point-max) (- table-cell-info-width n))
	      (if (or sticky (and currentp (looking-at ".*\\S ")))
		  (setq current-coordinate (table--transcoord-cache-to-table))
		(if out-of-bound (setcar current-coordinate
					 (+ (car table-cell-info-lu-coordinate) (- table-cell-info-width n 1))))))
	    (setq table-inhibit-auto-fill-paragraph t))
	  (table--update-cell 'now)
	  ;; if this cell heightens and pushes the current cell below, move
	  ;; the current-coordinate (point location) down accordingly.
	  (if currentp (setq current-coordinate (table--get-coordinate))
	    (if (and (> table-cell-info-height old-height)
		     (> (cdr current-coordinate) (cdr table-cell-info-lu-coordinate)))
		(setcdr current-coordinate (+ (cdr current-coordinate)
					      (- table-cell-info-height old-height)))))
	  ))
      ;; coord-list is now possibly invalid since some cells may have already
      ;; been heightened so recompute them by table--vertical-cell-list.
      (table--goto-coordinate current-coordinate)
      (setq coord-list (table--cell-list-to-coord-list (table--vertical-cell-list)))
      ;; push in the affected area above and below this table so that things
      ;; on the right side of the table are shifted horizontally neatly.
      (table--horizontally-shift-above-and-below (- n) (reverse coord-list))
      ;; finally narrow the frames for each cell.
      (let* ((below-list nil)
	     (this-list coord-list)
	     (above-list (cdr coord-list)))
	(while this-list
	  (let* ((below (prog1 (car below-list) (setq below-list (if below-list (cdr below-list) coord-list))))
		 (this (prog1 (car this-list) (setq this-list (cdr this-list))))
		 (above (prog1 (car above-list) (setq above-list (cdr above-list)))))
	    (delete-rectangle
	     (table--goto-coordinate
	      (cons (- (cadr this) n)
		    (if (or (null above) (<= (cadr this) (cadr above)))
			(1- (cdar this))
		      (cdar this))))
	     (table--goto-coordinate
	      (cons (cadr this)
		    (if (or (null below) (< (cadr this) (cadr below)))
			(1+ (cddr this))
		      (cddr this)))))))))
    (table--goto-coordinate current-coordinate)
    ;; re-recognize the current cell's new dimension
    (table-recognize-cell 'force)
    (message "")))

;;;###autoload
(defun table-forward-cell (&optional arg no-recognize unrecognize)
  "Move point forward to the beginning of the next cell.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N cells.
Do not specify NO-RECOGNIZE and UNRECOGNIZE. They are for internal use only.

Sample Cell Traveling Order (In Irregular Table Cases)

You can actually try how it works in this buffer.  Press
\\[table-recognize] and go to cells in the following tables and press
\\[table-forward-cell] or TAB key.

+-----+--+  +--+-----+  +--+--+--+  +--+--+--+  +---------+  +--+---+--+
|0    |1 |  |0 |1    |  |0 |1 |2 |  |0 |1 |2 |  |0        |  |0 |1  |2 |
+--+--+  |  |  +--+--+  +--+  |  |  |  |  +--+  +----+----+  +--+-+-+--+
|2 |3 |  |  |  |2 |3 |  |3 +--+  |  |  +--+3 |  |1   |2   |  |3   |4   |
|  +--+--+  +--+--+  |  +--+4 |  |  |  |4 +--+  +--+-+-+--+  +----+----+
|  |4    |  |4    |  |  |5 |  |  |  |  |  |5 |  |3 |4  |5 |  |5        |
+--+-----+  +-----+--+  +--+--+--+  +--+--+--+  +--+---+--+  +---------+

+--+--+--+  +--+--+--+  +--+--+--+  +--+--+--+
|0 |1 |2 |  |0 |1 |2 |  |0 |1 |2 |  |0 |1 |2 |
|  |  |  |  |  +--+  |  |  |  |  |  +--+  +--+
+--+  +--+  +--+3 +--+  |  +--+  |  |3 +--+4 |
|3 |  |4 |  |4 +--+5 |  |  |3 |  |  +--+5 +--+
|  |  |  |  |  |6 |  |  |  |  |  |  |6 |  |7 |
+--+--+--+  +--+--+--+  +--+--+--+  +--+--+--+

+--+--+--+  +--+--+--+  +--+--+--+--+  +--+-----+--+  +--+--+--+--+
|0 |1 |2 |  |0 |1 |2 |	|0 |1 |2 |3 |  |0 |1    |2 |  |0 |1 |2 |3 |
|  +--+  |  |  +--+  |	|  +--+--+  |  |  |     |  |  |  +--+--+  |
|  |3 +--+  +--+3 |  |	+--+4    +--+  +--+     +--+  +--+4    +--+
+--+  |4 |  |4 |  +--+	|5 +--+--+6 |  |3 +--+--+4 |  |5 |     |6 |
|5 +--+  |  |  +--+5 |	|  |7 |8 |  |  |  |5 |6 |  |  |  |     |  |
|  |6 |  |  |  |6 |  |	+--+--+--+--+  +--+--+--+--+  +--+-----+--+
+--+--+--+  +--+--+--+
"
  ;; After modifying this function, test against the above tables in
  ;; the doc string.  It is quite tricky.  The tables above do not
  ;; mean to cover every possible cases of cell layout, of course.
  ;; They are examples of tricky cases from implementation point of
  ;; view and provided for simple regression test purpose.
  (interactive "p")
  (or arg (setq arg 1))
  (table--finish-delayed-tasks)
  (while (null (zerop arg))
    (let* ((pivot (table--probe-cell 'abort-on-error))
	   (cell pivot) edge tip)
      ;; go to the beginning of the first right/left cell with same height if exists
      (while (and (setq cell (table--goto-coordinate
			      (cons (if (> arg 0) (1+ (car (table--get-coordinate (cdr cell))))
				      (1- (car (table--get-coordinate (car cell)))))
				    (cdr (table--get-coordinate (car pivot)))) 'no-extension))
		  (setq cell (table--probe-cell))
		  (/= (cdr (table--get-coordinate (car cell)))
		      (cdr (table--get-coordinate (car pivot))))))
      (if cell (goto-char (car cell))	; done
	;; if the horizontal move fails search the most left/right edge cell below/above the pivot
	;; but first find the edge cell
	(setq edge pivot)
	(while (and (table--goto-coordinate
		     (cons (if (> arg 0) (1- (car (table--get-coordinate (car edge))))
			     (1+ (car (table--get-coordinate (cdr edge)))))
			   (cdr (table--get-coordinate (car pivot)))) 'no-extension)
		    (setq cell (table--probe-cell))
		    (setq edge cell)))
	(setq cell (if (> arg 0) edge
		     (or (and (table--goto-coordinate
			       (cons (car (table--get-coordinate (cdr edge)))
				     (1- (cdr (table--get-coordinate (car edge))))))
			      (table--probe-cell))
			 edge)))
	;; now search for the tip which is the highest/lowest below/above cell
	(while cell
	  (let (below/above)
	    (and (table--goto-coordinate
		  (cons (car (table--get-coordinate (if (> arg 0) (car cell)
						      (cdr cell))))
			(if (> arg 0) (+ 2 (cdr (table--get-coordinate (cdr cell))))
			  (1- (cdr (table--get-coordinate (car pivot)))))) 'no-extension)
		 (setq below/above (table--probe-cell))
		 (or (null tip)
		     (if (> arg 0)
			 (< (cdr (table--get-coordinate (car below/above)))
			    (cdr (table--get-coordinate (car tip))))
		       (> (cdr (table--get-coordinate (car below/above)))
			  (cdr (table--get-coordinate (car tip))))))
		 (setq tip below/above)))
	  (and (setq cell (table--goto-coordinate
			   (cons (if (> arg 0) (1+ (car (table--get-coordinate (cdr cell))))
				   (1- (car (table--get-coordinate (car cell)))))
				 (if (> arg 0) (cdr (table--get-coordinate (car pivot)))
				   (1- (cdr (table--get-coordinate (car pivot)))))) 'no-extension))
	       (setq cell (table--probe-cell))))
	(if tip (goto-char (car tip))	; done
	  ;; let's climb up/down to the top/bottom from the edge
	  (while (and (table--goto-coordinate
		       (cons (if (> arg 0) (car (table--get-coordinate (car edge)))
			       (car (table--get-coordinate (cdr edge))))
			     (if (> arg 0) (1- (cdr (table--get-coordinate (car edge))))
			       (+ 2 (cdr (table--get-coordinate (cdr edge)))))) 'no-extension)
		      (setq cell (table--probe-cell))
		      (setq edge cell)))
	  (if (< arg 0)
	      (progn
		(setq cell edge)
		(while (and (table--goto-coordinate
			     (cons (1- (car (table--get-coordinate (car cell))))
				   (cdr (table--get-coordinate (cdr cell)))) 'no-extension)
			    (setq cell (table--probe-cell)))
		  (if (> (cdr (table--get-coordinate (car cell)))
			 (cdr (table--get-coordinate (car edge))))
		      (setq edge cell)))))
	  (goto-char (car edge)))))	; the top left cell
    (setq arg (if (> arg 0) (1- arg) (1+ arg))))
  (unless no-recognize
    (table-recognize-cell 'force nil (if unrecognize -1 nil)))) ; refill the cache with new cell contents

;;;###autoload
(defun table-backward-cell (&optional arg)
  "Move backward to the beginning of the previous cell.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move forward N cells."
  (interactive "p")
  (or arg (setq arg 1))
  (table-forward-cell (- arg)))

;;;###autoload
(defun table-span-cell (direction)
  "Span current cell into adjacent cell in DIRECTION.
DIRECTION is one of symbols; right, left, above or below."
  (interactive
   (list
    (let* ((dummy (barf-if-buffer-read-only))
	   (direction-list
	    (let* ((tmp (delete nil
				(mapcar (lambda (d)
					  (if (table--cell-can-span-p d)
					      (list (symbol-name d))))
					'(right left above below)))))
	      (if (null tmp)
		  (error "Can't span this cell"))
	      tmp))
	   (default-direction  (if (member (list (car table-cell-span-direction-history)) direction-list)
				   (car table-cell-span-direction-history)
				 (caar direction-list)))
	   (completion-ignore-case t))
      (intern (downcase (completing-read
			 (format "Span into (default %s): " default-direction)
			 direction-list
			 nil t nil 'table-cell-span-direction-history default-direction))))))
  (unless (memq direction '(right left above below))
    (error "Invalid direction %s, must be right, left, above or below"
	   (symbol-name direction)))
  (table-recognize-cell 'force)
  (unless (table--cell-can-span-p direction)
    (error "Can't span %s" (symbol-name direction)))
  ;; prepare beginning and ending positions of the border bar to strike through
  (let ((beg (cond
	      ((eq direction 'right)
	       (save-excursion
		 (table--goto-coordinate
		  (cons (car table-cell-info-rb-coordinate)
			(1- (cdr table-cell-info-lu-coordinate))) 'no-extension)))
	      ((eq direction 'below)
	       (save-excursion
		 (table--goto-coordinate
		  (cons (1- (car table-cell-info-lu-coordinate))
			(1+ (cdr table-cell-info-rb-coordinate))) 'no-extension)))
	      (t
	       (save-excursion
		 (table--goto-coordinate
		  (cons (1- (car table-cell-info-lu-coordinate))
			(1- (cdr table-cell-info-lu-coordinate))) 'no-extension)))))
	(end (cond
	      ((eq direction 'left)
	       (save-excursion
		 (table--goto-coordinate
		  (cons (car table-cell-info-lu-coordinate)
			(1+ (cdr table-cell-info-rb-coordinate))) 'no-extension)))
	      ((eq direction 'above)
	       (save-excursion
		 (table--goto-coordinate
		  (cons (1+ (car table-cell-info-rb-coordinate))
			(1- (cdr table-cell-info-lu-coordinate))) 'no-extension)))
	      (t
	       (save-excursion
		 (table--goto-coordinate
		  (cons (1+ (car table-cell-info-rb-coordinate))
			(1+ (cdr table-cell-info-rb-coordinate))) 'no-extension))))))
    ;; replace the bar with blank space while taking care of edges to be border or intersection
    (save-excursion
      (goto-char beg)
      (if (memq direction '(left right))
	  (let* ((column (current-column))
		 rectangle
		 (n-element (- (length (extract-rectangle beg end)) 2))
		 (above-contp (and (goto-char beg)
				   (zerop (forward-line -1))
				   (= (move-to-column column) column)
				   (looking-at (regexp-quote (char-to-string table-cell-vertical-char)))))
		 (below-contp (and (goto-char end)
				   (progn (forward-char -1) t)
				   (zerop (forward-line 1))
				   (= (move-to-column column) column)
				   (looking-at (regexp-quote (char-to-string table-cell-vertical-char))))))
	    (setq rectangle
		  (cons (if below-contp
			    (char-to-string table-cell-intersection-char)
			  (substring table-cell-horizontal-chars 0 1))
			rectangle))
	    (while (> n-element 0)
	      (setq rectangle (cons (table--cell-blank-str 1) rectangle))
	      (setq n-element (1- n-element)))
	    (setq rectangle
		  (cons (if above-contp
			    (char-to-string table-cell-intersection-char)
			  (substring table-cell-horizontal-chars 0 1))
			rectangle))
	    (delete-rectangle beg end)
	    (goto-char beg)
	    (table--insert-rectangle rectangle))
	(delete-region beg end)
	(insert (if (and (> (point) (point-min))
			 (save-excursion
			   (forward-char -1)
			   (looking-at (regexp-opt-charset
					(string-to-list table-cell-horizontal-chars)))))
		    table-cell-intersection-char
		  table-cell-vertical-char)
		(table--cell-blank-str (- end beg 2))
		(if (looking-at (regexp-opt-charset
				 (string-to-list table-cell-horizontal-chars)))
		    table-cell-intersection-char
		  table-cell-vertical-char))))
    ;; recognize the newly created spanned cell
    (table-recognize-cell 'force)
    (if (member direction '(right left))
	(table-with-cache-buffer
	  (table--fill-region (point-min) (point-max))
	  (setq table-inhibit-auto-fill-paragraph t)))))

;;;###autoload
(defun table-split-cell-vertically ()
  "Split current cell vertically.
Creates a cell above and a cell below the current point location."
  (interactive "*")
  (table-recognize-cell 'force)
  (let ((point-y (cdr (table--get-coordinate))))
    (unless (table--cell-can-split-vertically-p)
      (error "Can't split here"))
    (let* ((old-coordinate (table--get-coordinate))
	   (column (current-column))
	   (beg (table--goto-coordinate
		 (cons (1- (car table-cell-info-lu-coordinate))
		       point-y)))
	   (end (table--goto-coordinate
		 (cons (1+ (car table-cell-info-rb-coordinate))
		       point-y)))
	   (line (buffer-substring (1+ beg) (1- end))))
      (when (= (cdr old-coordinate) (cdr table-cell-info-rb-coordinate))
	(table--goto-coordinate old-coordinate)
	(table-heighten-cell 1 'no-copy 'no-update))
      (goto-char beg)
      (delete-region beg end)
      (insert table-cell-intersection-char
	      (make-string table-cell-info-width (string-to-char table-cell-horizontal-chars))
	      table-cell-intersection-char)
      (table--goto-coordinate old-coordinate)
      (forward-line 1)
      (move-to-column column)
      (setq old-coordinate (table--get-coordinate))
      (table-recognize-cell 'force)
      (unless (string-match "^\\s *$" line)
	(table-with-cache-buffer
	  (goto-char (point-min))
	  (insert line ?\n)
	  (goto-char (point-min)) ;; don't heighten cell unnecessarily
	  (setq table-inhibit-auto-fill-paragraph t)))
      (table--update-cell 'now)	;; can't defer this operation
      (table--goto-coordinate old-coordinate)
      (move-to-column column)
      (table-recognize-cell 'force))))

;;;###autoload
(defun table-split-cell-horizontally ()
  "Split current cell horizontally.
Creates a cell on the left and a cell on the right of the current point location."
  (interactive "*")
  (table-recognize-cell 'force)
  (let* ((o-coordinate (table--get-coordinate))
	 (point-x (car o-coordinate))
	 cell-empty cell-contents cell-coordinate
	 contents-to beg end rectangle strip-rect
	 (right-edge (= (car o-coordinate) (1- (car table-cell-info-rb-coordinate)))))
    (unless (table--cell-can-split-horizontally-p)
      (error "Can't split here"))
    (let ((table-inhibit-update t))
      (table-with-cache-buffer
	(setq cell-coordinate (table--get-coordinate))
	(save-excursion
	  (goto-char (point-min))
	  (setq cell-empty (null (re-search-forward "\\S " nil t))))
	(setq cell-contents (buffer-substring (point-min) (point-max)))
	(setq table-inhibit-auto-fill-paragraph t)))
    (setq contents-to
	  (if cell-empty 'left
	    (let* ((completion-ignore-case t)
		   (default (car table-cell-split-contents-to-history)))
	      (intern
	       (if (member 'click (event-modifiers last-input-event))
		   (x-popup-menu last-input-event
				 '("Existing cell contents to:"
				   ("Title"
				    ("Split" . "split") ("Left" . "left") ("Right" . "right"))))
		 (downcase (completing-read
			    (format "Existing cell contents to (default %s): " default)
			    '(("split") ("left") ("right"))
			    nil t nil 'table-cell-split-contents-to-history default)))))))
    (unless (eq contents-to 'split)
      (table-with-cache-buffer
	(erase-buffer)
	(setq table-inhibit-auto-fill-paragraph t)))
    (table--update-cell 'now)
    (setq beg (table--goto-coordinate
	       (cons point-x
		     (1- (cdr table-cell-info-lu-coordinate)))))
    (setq end (table--goto-coordinate
	       (cons (1+ point-x)
		     (1+ (cdr table-cell-info-rb-coordinate)))))
    (setq rectangle (cons (char-to-string table-cell-intersection-char) nil))
    (let ((n table-cell-info-height))
      (while (prog1 (> n 0) (setq n (1- n)))
	(setq rectangle (cons (char-to-string table-cell-vertical-char) rectangle))))
    (setq rectangle (cons (char-to-string table-cell-intersection-char) rectangle))
    (if (eq contents-to 'split)
	(setq strip-rect (extract-rectangle beg end)))
    (delete-rectangle beg end)
    (goto-char beg)
    (table--insert-rectangle rectangle)
    (table--goto-coordinate o-coordinate)
    (if cell-empty
	(progn
	  (forward-char 1)
	  (if right-edge
	      (table-widen-cell 1)))
      (unless (eq contents-to 'left)
	(forward-char 1))
      (table-recognize-cell 'force)
      (table-with-cache-buffer
	(if (eq contents-to 'split)
	    ;; split inserts strip-rect after removing
	    ;; top and bottom borders
	    (let ((o-coord (table--get-coordinate))
		  (l (setq strip-rect (cdr strip-rect))))
	      (while (cddr l) (setq l (cdr l)))
	      (setcdr l nil)
	      ;; insert the strip only when it is not a completely blank one
	      (unless (let ((cl (mapcar (lambda (s) (string= s " ")) strip-rect)))
			(and (car cl)
			     (table--uniform-list-p cl)))
		(goto-char (point-min))
		(table--insert-rectangle strip-rect)
		(table--goto-coordinate o-coord)))
	  ;; left or right inserts original contents
	  (erase-buffer)
	  (insert cell-contents)
	  (table--goto-coordinate cell-coordinate)
	  (table--fill-region (point-min) (point-max))
	  ;; avoid unnecessary vertical cell expansion
	  (and (looking-at "\\s *\\'")
	       (re-search-backward "\\S \\(\\s *\\)\\=" nil t)
	       (goto-char (match-beginning 1))))
	;; in either case do not fill paragraph
	(setq table-inhibit-auto-fill-paragraph t))
      (table--update-cell 'now)) ;; can't defer this operation
    (table-recognize-cell 'force)))

;;;###autoload
(defun table-split-cell (orientation)
  "Split current cell in ORIENTATION.
ORIENTATION is a symbol either horizontally or vertically."
  (interactive
   (list
    (let* ((dummy (barf-if-buffer-read-only))
	   (completion-ignore-case t)
	   (default (car table-cell-split-orientation-history)))
      (intern (downcase (completing-read
			 (format "Split orientation (default %s): " default)
			 '(("horizontally") ("vertically"))
			 nil t nil 'table-cell-split-orientation-history default))))))
  (unless (memq orientation '(horizontally vertically))
    (error "Invalid orientation %s, must be horizontally or vertically"
	   (symbol-name orientation)))
  (if (eq orientation 'horizontally)
      (table-split-cell-horizontally)
    (table-split-cell-vertically)))

;;;###autoload
(defun table-justify (what justify)
  "Justify contents of a cell, a row of cells or a column of cells.
WHAT is a symbol 'cell, 'row or 'column.  JUSTIFY is a symbol 'left,
'center, 'right, 'top, 'middle, 'bottom or 'none."
  (interactive
   (list (let* ((dummy (barf-if-buffer-read-only))
		(completion-ignore-case t)
		(default (car table-target-history)))
	   (intern (downcase (completing-read
			      (format "Justify what (default %s): " default)
			      '(("cell") ("row") ("column"))
			      nil t nil 'table-target-history default))))
	 (table--query-justification)))
  (funcall (intern (concat "table-justify-" (symbol-name what))) justify))

;;;###autoload
(defun table-justify-cell (justify &optional paragraph)
  "Justify cell contents.
JUSTIFY is a symbol 'left, 'center or 'right for horizontal, or 'top,
'middle, 'bottom or 'none for vertical.  When optional PARAGRAPH is
non-nil the justify operation is limited to the current paragraph,
otherwise the entire cell contents is justified."
  (interactive
   (list (table--query-justification)))
  (table--finish-delayed-tasks)
  (table-recognize-cell 'force)
  (table--justify-cell-contents justify paragraph))

;;;###autoload
(defun table-justify-row (justify)
  "Justify cells of a row.
JUSTIFY is a symbol 'left, 'center or 'right for horizontal, or top,
'middle, 'bottom or 'none for vertical."
  (interactive
   (list (table--query-justification)))
  (let((cell-list (table--horizontal-cell-list nil nil 'top)))
    (table--finish-delayed-tasks)
    (save-excursion
      (while cell-list
	(let ((cell (car cell-list)))
	  (setq cell-list (cdr cell-list))
	  (goto-char (car cell))
	  (table-recognize-cell 'force)
	  (table--justify-cell-contents justify))))))

;;;###autoload
(defun table-justify-column (justify)
  "Justify cells of a column.
JUSTIFY is a symbol 'left, 'center or 'right for horizontal, or top,
'middle, 'bottom or 'none for vertical."
  (interactive
   (list (table--query-justification)))
  (let((cell-list (table--vertical-cell-list nil nil 'left)))
    (table--finish-delayed-tasks)
    (save-excursion
      (while cell-list
	(let ((cell (car cell-list)))
	  (setq cell-list (cdr cell-list))
	  (goto-char (car cell))
	  (table-recognize-cell 'force)
	  (table--justify-cell-contents justify))))))

;;;###autoload
(defun table-fixed-width-mode (&optional arg)
  "Toggle fixing width mode.
In the fixed width mode, typing inside a cell never changes the cell
width where in the normal mode the cell width expands automatically in
order to prevent a word being folded into multiple lines."
  (interactive "P")
  (table--finish-delayed-tasks)
  (setq table-fixed-width-mode
	(if (null arg)
	    (not table-fixed-width-mode)
	  (> (prefix-numeric-value arg) 0)))
  (table--update-cell-face))

;;;###autoload
(defun table-query-dimension (&optional where)
  "Return the dimension of the current cell and the current table.
The result is a list (cw ch tw th c r cells) where cw is the cell
width, ch is the cell height, tw is the table width, th is the table
height, c is the number of columns, r is the number of rows and cells
is the total number of cells.  The cell dimension excludes the cell
frame while the table dimension includes the table frame.  The columns
and the rows are counted by the number of cell boundaries.  Therefore
the number tends to be larger than it appears for the tables with
non-uniform cell structure (heavily spanned and split).  When optional
WHERE is provided the cell and table at that location is reported."
  (interactive)
  (save-excursion
    (if where (goto-char where))
    (let ((starting-cell (table--probe-cell))
	  cell table-lu table-rb col-list row-list (cells 0))
      (if (null starting-cell) nil
	(setq table-lu (car starting-cell))
	(setq table-rb (cdr starting-cell))
	(setq col-list (cons (car (table--get-coordinate (car starting-cell))) nil))
	(setq row-list (cons (cdr (table--get-coordinate (car starting-cell))) nil))
	(and (called-interactively-p 'interactive)
	     (message "Computing cell dimension..."))
	(while
	    (progn
	      (table-forward-cell 1 t)
	      (setq cells (1+ cells))
	      (and (setq cell (table--probe-cell))
		   (not (equal cell starting-cell))))
	  (if (< (car cell) table-lu)
	      (setq table-lu (car cell)))
	  (if (> (cdr cell) table-rb)
	      (setq table-rb (cdr cell)))
	  (let ((lu-coordinate (table--get-coordinate (car cell))))
	    (if (memq (car lu-coordinate) col-list) nil
	      (setq col-list (cons (car lu-coordinate) col-list)))
	    (if (memq (cdr lu-coordinate) row-list) nil
	      (setq row-list (cons (cdr lu-coordinate) row-list)))))
	(let* ((cell-lu-coordinate (table--get-coordinate (car starting-cell)))
	       (cell-rb-coordinate (table--get-coordinate (cdr starting-cell)))
	       (table-lu-coordinate (table--get-coordinate table-lu))
	       (table-rb-coordinate (table--get-coordinate table-rb))
	       (cw (- (car cell-rb-coordinate) (car cell-lu-coordinate)))
	       (ch (1+ (- (cdr cell-rb-coordinate) (cdr cell-lu-coordinate))))
	       (tw (+ 2 (- (car table-rb-coordinate) (car table-lu-coordinate))))
	       (th (+ 3 (- (cdr table-rb-coordinate) (cdr table-lu-coordinate))))
	       (c (length col-list))
	       (r (length row-list)))
	  (and (called-interactively-p 'interactive)
	       (message "Cell: (%dw, %dh), Table: (%dw, %dh), Dim: (%dc, %dr), Total Cells: %d" cw ch tw th c r cells))
	  (list cw ch tw th c r cells))))))

;;;###autoload
(defun table-generate-source (language &optional dest-buffer caption)
  "Generate source of the current table in the specified language.
LANGUAGE is a symbol that specifies the language to describe the
structure of the table.  It must be either 'html, 'latex or 'cals.
The resulted source text is inserted into DEST-BUFFER and the buffer
object is returned.  When DEST-BUFFER is omitted or nil the default
buffer specified in `table-dest-buffer-name' is used.  In this case
the content of the default buffer is erased prior to the generation.
When DEST-BUFFER is non-nil it is expected to be either a destination
buffer or a name of the destination buffer.  In this case the
generated result is inserted at the current point in the destination
buffer and the previously existing contents in the buffer are
untouched.

References used for this implementation:

HTML:
        URL `http://www.w3.org'

LaTeX:
        URL `http://www.maths.tcd.ie/~dwilkins/LaTeXPrimer/Tables.html'

CALS (DocBook DTD):
        URL `http://www.oasis-open.org/html/a502.htm'
        URL `http://www.oreilly.com/catalog/docbook/chapter/book/table.html#AEN114751'
"
  (interactive
   (let* ((dummy (unless (table--probe-cell) (error "Table not found here")))
	  (completion-ignore-case t)
	  (default (car table-source-language-history))
	  (language (downcase (completing-read
			       (format "Language (default %s): " default)
			       (mapcar (lambda (s) (list (symbol-name s)))
				       table-source-languages)
			       nil t nil 'table-source-language-history default))))
     (list
      (intern language)
      (read-buffer "Destination buffer: " (concat table-dest-buffer-name "." language))
      (table--read-from-minibuffer '("Table Caption" . table-source-caption-history)))))
  (let ((default-buffer-name (concat table-dest-buffer-name "." (symbol-name language))))
    (unless (or (called-interactively-p 'interactive) (table--probe-cell))
      (error "Table not found here"))
    (unless (bufferp dest-buffer)
      (setq dest-buffer (get-buffer-create (or dest-buffer default-buffer-name))))
    (if (string= (buffer-name dest-buffer) default-buffer-name)
	(with-current-buffer dest-buffer
	  (erase-buffer)))
    (save-excursion
      (let ((starting-cell (table--probe-cell))
	    cell origin-cell tail-cell col-list row-list (n 0) i)
	;; first analyze the table structure and prepare:
	;; 1. origin cell (left up corner cell)
	;; 2. tail cell (right bottom corner cell)
	;; 3. column boundary list
	;; 4. row boundary list
	(setq origin-cell starting-cell)
	(setq tail-cell starting-cell)
	(setq col-list (cons (car (table--get-coordinate (car starting-cell))) nil))
	(setq row-list (cons (cdr (table--get-coordinate (car starting-cell))) nil))
	(setq i 0)
	(let ((wheel [?- ?\\ ?| ?/]))
	  (while
	      (progn
		(if (called-interactively-p 'interactive)
		    (progn
		      (message "Analyzing table...%c" (aref wheel i))
		      (if (eq (setq i (1+ i)) (length wheel))
			  (setq i 0))
		      (setq n (1+ n))))
		(table-forward-cell 1 t)
		(and (setq cell (table--probe-cell))
		     (not (equal cell starting-cell))))
	    (if (< (car cell) (car origin-cell))
		(setq origin-cell cell))
	    (if (> (cdr cell) (cdr tail-cell))
		(setq tail-cell cell))
	    (let ((lu-coordinate (table--get-coordinate (car cell))))
	      (unless (memq (car lu-coordinate) col-list)
		(setq col-list (cons (car lu-coordinate) col-list)))
	      (unless (memq (cdr lu-coordinate) row-list)
		(setq row-list (cons (cdr lu-coordinate) row-list))))))
	(setq col-list (sort col-list '<))
	(setq row-list (sort row-list '<))
	(message "Generating source...")
	;; clear the source generation property list
	(setplist 'table-source-info-plist nil)
	;; prepare to start from the origin cell
	(goto-char (car origin-cell))
	;; first put some header information
	(table--generate-source-prologue dest-buffer language caption col-list row-list)
	(cond
	 ((eq language 'latex)
	  ;; scan by character lines
	  (table--generate-source-scan-lines dest-buffer language origin-cell tail-cell col-list row-list))
	 (t
	  ;; scan by table cells
	  (table--generate-source-scan-rows dest-buffer language origin-cell col-list row-list)))
	;; insert closing
	(table--generate-source-epilogue dest-buffer language col-list row-list))
      ;; lastly do some convenience work
      (if (called-interactively-p 'interactive)
	  (save-selected-window
	    (pop-to-buffer dest-buffer t)
	    (goto-char (point-min))
	    (and (string= (buffer-name dest-buffer) default-buffer-name)
		 (buffer-file-name dest-buffer)
		 (save-buffer))
	    (message "Generating source...done")
	    (let ((mode
		   (if (memq language '(cals)) 'sgml-mode
		     (intern (concat (symbol-name language) "-mode")))))
	      (if (fboundp mode)
		  (call-interactively mode)))
	    )))
    dest-buffer))

(defun table--generate-source-prologue (dest-buffer language caption col-list row-list)
  "Generate and insert source prologue into DEST-BUFFER."
  (with-current-buffer dest-buffer
    (cond
     ((eq language 'html)
      (insert (format "<!-- This HTML table template is generated by emacs %s -->\n" emacs-version)
	      (format "<table %s>\n" table-html-table-attribute)
	      (if (and (stringp caption)
		       (not (string= caption "")))
		  (format "  <caption>%s</caption>\n" caption)
		"")))
     ((eq language 'latex)
      (insert (format "%% This LaTeX table template is generated by emacs %s\n" emacs-version)
	      "\\begin{tabular}{|" (apply 'concat (make-list (length col-list) "l|")) "}\n"
	      "\\hline\n"))
     ((eq language 'cals)
      (insert (format "<!-- This CALS table template is generated by emacs %s -->\n" emacs-version)
	      "<table frame=\"all\">\n")
      (if (and (stringp caption)
	       (not (string= caption "")))
	  (insert "  <title>" caption "</title>\n"))
      (insert (format "  <tgroup cols=\"%d\" align=\"left\" colsep=\"1\" rowsep=\"1\">\n" (length col-list)))
      (table-put-source-info 'colspec-marker (point-marker))
      (table-put-source-info 'row-type (if (zerop table-cals-thead-rows) "tbody" "thead"))
      (set-marker-insertion-type (table-get-source-info 'colspec-marker) nil) ;; insert after
      (insert (format "    <%s valign=\"top\">\n" (table-get-source-info 'row-type))))
     )))

(defun table--generate-source-epilogue (dest-buffer language col-list row-list)
  "Generate and insert source epilogue into DEST-BUFFER."
  (with-current-buffer dest-buffer
    (cond
     ((eq language 'html)
      (insert "</table>\n"))
     ((eq language 'latex)
      (insert "\\end{tabular}\n"))
     ((eq language 'cals)
      (set-marker-insertion-type (table-get-source-info 'colspec-marker) t) ;; insert before
      (save-excursion
	(goto-char (table-get-source-info 'colspec-marker))
	(mapc
	 (lambda (col)
	   (insert (format "    <colspec colnum=\"%d\" colname=\"c%d\"/>\n" col col)))
	 (sort (table-get-source-info 'colnum-list) '<)))
      (insert (format "    </%s>\n  </tgroup>\n</table>\n" (table-get-source-info 'row-type))))
     )))

(defun table--generate-source-scan-rows (dest-buffer language origin-cell col-list row-list)
  "Generate and insert source rows into DEST-BUFFER."
  (table-put-source-info 'current-row 1)
  (while row-list
    (with-current-buffer dest-buffer
      (cond
       ((eq language 'html)
	(insert "  <tr>\n"))
       ((eq language 'cals)
	(insert "      <row>\n"))
       ))
    (table--generate-source-cells-in-a-row dest-buffer language col-list row-list)
    (with-current-buffer dest-buffer
      (cond
       ((eq language 'html)
	(insert "  </tr>\n"))
       ((eq language 'cals)
	(insert "      </row>\n")
	(unless (/= (table-get-source-info 'current-row) table-cals-thead-rows)
	  (insert (format "    </%s>\n" (table-get-source-info 'row-type)))
	  (insert (format "    <%s valign=\"top\">\n" (table-put-source-info 'row-type "tbody")))))))
    (table-put-source-info 'current-row (1+ (table-get-source-info 'current-row)))
    (setq row-list (cdr row-list))))

(defun table--generate-source-cells-in-a-row (dest-buffer language col-list row-list)
  "Generate and insert source cells into DEST-BUFFER."
  (table-put-source-info 'current-column 1)
  (while col-list
    (let* ((cell (table--probe-cell))
	   (lu (table--get-coordinate (car cell)))
	   (rb (table--get-coordinate (cdr cell)))
	   (alignment (table--get-cell-justify-property cell))
	   (valign (table--get-cell-valign-property cell))
	   (row-list row-list)
	   (colspan 1)
	   (rowspan 1))
	(if (< (car lu) (car col-list))
	    (setq col-list nil)
	  (while (and col-list
		      (> (car lu) (car col-list)))
	    (setq col-list (cdr col-list))
	    (table-put-source-info 'current-column (1+ (table-get-source-info 'current-column))))
	  (setq col-list (cdr col-list))
	  (table-put-source-info 'next-column (1+ (table-get-source-info 'current-column)))
	  (while (and col-list
		      (> (1+ (car rb)) (car col-list)))
	    (setq colspan (1+ colspan))
	    (setq col-list (cdr col-list))
	    (table-put-source-info 'next-column (1+ (table-get-source-info 'next-column))))
	  (setq row-list (cdr row-list))
	  (while (and row-list
		      (> (+ (cdr rb) 2) (car row-list)))
	    (setq rowspan (1+ rowspan))
	    (setq row-list (cdr row-list)))
	  (with-current-buffer dest-buffer
	    (cond
	     ((eq language 'html)
	      (insert (format "    <%s"
			      (table-put-source-info
			       'cell-type
			       (if (or (<= (table-get-source-info 'current-row) table-html-th-rows)
				       (<= (table-get-source-info 'current-column) table-html-th-columns))
				   "th" "td"))))
	      (if (and table-html-cell-attribute (not (string= table-html-cell-attribute "")))
		  (insert " " table-html-cell-attribute))
	      (if (> colspan 1) (insert (format " colspan=\"%d\"" colspan)))
	      (if (> rowspan 1) (insert (format " rowspan=\"%d\"" rowspan)))
	      (insert (format " align=\"%s\"" (if alignment (symbol-name alignment) "left")))
	      (insert (format " valign=\"%s\"" (if valign (symbol-name valign) "top")))
	      (insert ">\n"))
	     ((eq language 'cals)
	      (insert "        <entry")
	      (if (> colspan 1)
		  (let ((scol (table-get-source-info 'current-column))
			(ecol (+ (table-get-source-info 'current-column) colspan -1)))
		    (mapc (lambda (col)
			    (unless (memq col (table-get-source-info 'colnum-list))
			      (table-put-source-info 'colnum-list
						     (cons col (table-get-source-info 'colnum-list)))))
			  (list scol ecol))
		    (insert (format " namest=\"c%d\" nameend=\"c%d\"" scol ecol))))
	      (if (> rowspan 1) (insert (format " morerows=\"%d\"" (1- rowspan))))
	      (if (and alignment
		       (not (memq alignment '(left none))))
		  (insert " align=\"" (symbol-name alignment) "\""))
	      (if (and valign
		       (not (memq valign '(top none))))
		  (insert " valign=\"" (symbol-name valign) "\""))
	      (insert ">\n"))
	     ))
	  (table--generate-source-cell-contents dest-buffer language cell)
	  (with-current-buffer dest-buffer
	    (cond
	     ((eq language 'html)
	      (insert (format"    </%s>\n" (table-get-source-info 'cell-type))))
	     ((eq language 'cals)
	      (insert "        </entry>\n"))
	     ))
	  (table-forward-cell 1 t)
	  (table-put-source-info 'current-column (table-get-source-info 'next-column))
	  ))))

(defun table--generate-source-cell-contents (dest-buffer language cell)
  "Generate and insert source cell contents of a CELL into DEST-BUFFER."
  (let ((cell-contents (extract-rectangle (car cell) (cdr cell))))
    (with-temp-buffer
      (table--insert-rectangle cell-contents)
      (table--remove-cell-properties (point-min) (point-max))
      (goto-char (point-min))
      (cond
       ((eq language 'html)
	(if table-html-delegate-spacing-to-user-agent
	    (progn
	      (table--remove-eol-spaces (point-min) (point-max))
	      (if (re-search-forward "\\s +\\'" nil t)
		  (replace-match "")))
	  (while (search-forward " " nil t)
	    (replace-match "&nbsp;"))
	  (goto-char (point-min))
	  (while (and (re-search-forward "$" nil t)
		      (not (eobp)))
	    (insert "<br />")
	    (forward-char 1)))
	(unless (and table-html-delegate-spacing-to-user-agent
		     (progn
		       (goto-char (point-min))
		       (looking-at "\\s *\\'")))))
       ((eq language 'cals)
	(table--remove-eol-spaces (point-min) (point-max))
	(if (re-search-forward "\\s +\\'" nil t)
	    (replace-match "")))
       )
      (setq cell-contents (buffer-substring (point-min) (point-max))))
    (with-current-buffer dest-buffer
      (let ((beg (point)))
	(insert cell-contents)
	(indent-rigidly beg (point)
			(cond
			 ((eq language 'html) 6)
			 ((eq language 'cals) 10)))
	(insert ?\n)))))

(defun table--cell-horizontal-char-p (c)
  "Test if character C is one of the horizontal characters"
  (memq c (string-to-list table-cell-horizontal-chars)))

(defun table--generate-source-scan-lines (dest-buffer language origin-cell tail-cell col-list row-list)
  "Scan the table line by line.
Currently this method is for LaTeX only."
  (let* ((lu-coord (table--get-coordinate (car origin-cell)))
	 (rb-coord (table--get-coordinate (cdr tail-cell)))
	 (x0 (car lu-coord))
	 (x1 (car rb-coord))
	 (y  (cdr lu-coord))
	 (y1 (cdr rb-coord)))
    (while (<= y y1)
      (let* ((border-p (memq (1+ y) row-list))
	     (border-char-list
	      (mapcar (lambda (x)
			(if border-p (char-after (table--goto-coordinate (cons x y)))
			  (char-before (table--goto-coordinate (cons x y)))))
		      col-list))
	     start i c)
	(if border-p
	    ;; horizontal cell border processing
	    (if (and (table--cell-horizontal-char-p (car border-char-list))
		     (table--uniform-list-p border-char-list))
		(with-current-buffer dest-buffer
		  (insert "\\hline\n"))
	      (setq i 0)
	      (while (setq c (nth i border-char-list))
		(if (and start (not (table--cell-horizontal-char-p c)))
		    (progn
		      (with-current-buffer dest-buffer
			(insert (format "\\cline{%d-%d}\n" (1+ start) i)))
		      (setq start nil)))
		(if (and (not start) (table--cell-horizontal-char-p c))
		    (setq start i))
		(setq i (1+ i)))
	      (if start
		  (with-current-buffer dest-buffer
		    (insert (format "\\cline{%d-%d}\n" (1+ start) i)))))
	  ;; horizontal cell contents processing
	  (let* ((span 1) ;; spanning length
		 (first-p t) ;; first in a row
		 (insert-column ;; a function that processes one column/multicolumn
		  (function
		   (lambda (from to)
		     (let ((line (table--buffer-substring-and-trim
				  (table--goto-coordinate (cons from y))
				  (table--goto-coordinate (cons to y)))))
		       ;; escape special characters
		       (with-temp-buffer
			 (insert line)
			 (goto-char (point-min))
			 (while (re-search-forward "\\([#$~_^%{}]\\)\\|\\(\\\\\\)\\|\\([<>|]\\)" nil t)
			   (if (match-beginning 1)
			       (save-excursion
				 (goto-char (match-beginning 1))
				 (insert  "\\"))
			     (if (match-beginning 2)
				 (replace-match "$\\backslash$" t t)
			       (replace-match (concat "$" (match-string 3) "$")) t t)))
			 (setq line (buffer-substring (point-min) (point-max))))
		       ;; insert a column separator and column/multicolumn contents
		       (with-current-buffer dest-buffer
			 (unless first-p
			   (insert (if (eq (char-before) ?\s) "" " ") "& "))
			 (if (> span 1)
			     (insert (format "\\multicolumn{%d}{%sl|}{%s}" span (if first-p "|" "") line))
			   (insert line)))
		       (setq first-p nil)
		       (setq span 1)
		       (setq start (nth i col-list)))))))
	    (setq start x0)
	    (setq i 1)
	    (while (setq c (nth i border-char-list))
	      (if (eq c table-cell-vertical-char)
		  (funcall insert-column start (1- (nth i col-list)))
		(setq span (1+ span)))
	      (setq i (1+ i)))
	    (funcall insert-column start x1))
	  (with-current-buffer dest-buffer
	    (insert (if (eq (char-before) ?\s) "" " ") "\\\\\n"))))
      (setq y (1+ y)))
    (with-current-buffer dest-buffer
      (insert "\\hline\n"))
    ))

;;;###autoload
(defun table-insert-sequence (str n increment interval justify)
  "Travel cells forward while inserting a specified sequence string in each cell.
STR is the base string from which the sequence starts.  When STR is an
empty string then each cell content is erased.  When STR ends with
numerical characters (they may optionally be surrounded by a pair of
parentheses) they are incremented as a decimal number.  Otherwise the
last character in STR is incremented in ASCII code order.  N is the
number of sequence elements to insert.  When N is negative the cell
traveling direction is backward.  When N is zero it travels forward
entire table.  INCREMENT is the increment between adjacent sequence
elements and can be a negative number for effectively decrementing.
INTERVAL is the number of cells to travel between sequence element
insertion which is normally 1.  When zero or less is given for
INTERVAL it is interpreted as number of cells per row so that sequence
is placed straight down vertically as long as the table's cell
structure is uniform.  JUSTIFY is one of the symbol 'left, 'center or
'right, that specifies justification of the inserted string.

Example:

  (progn
    (table-insert 16 3 5 1)
    (table-forward-cell 15)
    (table-insert-sequence \"D0\" -16 1 1 'center)
    (table-forward-cell 16)
    (table-insert-sequence \"A[0]\" -16 1 1 'center)
    (table-forward-cell 1)
    (table-insert-sequence \"-\" 16 0 1 'center))

  (progn
    (table-insert 16 8 5 1)
    (table-insert-sequence \"@\" 0 1 2 'right)
    (table-forward-cell 1)
    (table-insert-sequence \"64\" 0 1 2 'left))
"
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (unless (table--probe-cell) (error "Table not found here"))
     (list (read-from-minibuffer
	    "Sequence base string: " (car table-sequence-string-history) nil nil 'table-sequence-string-history)
	   (string-to-number
	    (table--read-from-minibuffer
	     '("How many elements (0: maximum, negative: backward traveling)" . table-sequence-count-history)))
	   (string-to-number
	    (table--read-from-minibuffer
	     '("Increment element by" . table-sequence-increment-history)))
	   (string-to-number
	    (table--read-from-minibuffer
	     '("Cell interval (0: vertical, 1:horizontal)" . table-sequence-interval-history)))
	   (let* ((completion-ignore-case t)
		  (default (car table-sequence-justify-history)))
	     (intern (downcase (completing-read
				(format "Justify (default %s): " default)
				'(("left") ("center") ("right"))
				nil t nil 'table-sequence-justify-history default)))))))
  (unless (or (called-interactively-p 'interactive) (table--probe-cell))
    (error "Table not found here"))
  (string-match "\\([0-9]*\\)\\([]})>]*\\)\\'" str)
  (if (called-interactively-p 'interactive)
      (message "Sequencing..."))
  (let* ((prefix (substring str 0 (match-beginning 1)))
	 (index (match-string 1 str))
	 (fmt (format "%%%s%dd" (if (eq (string-to-char index) ?0) "0" "") (length index)))
	 (postfix (match-string 2 str))
	 (dim (table-query-dimension))
	 (cells (nth 6 dim))
	 (direction (if (< n 0) -1 1))
	 (interval-count 0))
    (if (string= index "")
	(progn
	  (setq index nil)
	  (if (string= prefix "")
	      (setq prefix nil)))
      (setq index (string-to-number index)))
    (if (< n 0) (setq n (- n)))
    (if (or (zerop n) (> n cells)) (setq n cells))
    (if (< interval 0) (setq interval (- interval)))
    (if (zerop interval) (setq interval (nth 4 dim)))
    (save-excursion
      (while (progn
	       (if (> interval-count 0) nil
		 (setq interval-count interval)
		 (table-with-cache-buffer
		   (goto-char (point-min))
		   (if (not (or prefix index))
		       (erase-buffer)
		     (insert prefix)
		     (if index (insert (format fmt index)))
		     (insert postfix)
		     (table--fill-region (point-min) (point) table-cell-info-width justify)
		     (setq table-cell-info-justify justify))
		   (setq table-inhibit-auto-fill-paragraph t))
		 (table--update-cell 'now)
		 (if index
		     (setq index (+ index increment))
		   (if (and prefix (string= postfix ""))
		       (let ((len-1 (1- (length prefix))))
			 (setq prefix (concat (substring prefix 0 len-1)
					      (char-to-string
					       (+ (string-to-char (substring prefix len-1)) increment)))))))
		 (setq n (1- n)))
	       (table-forward-cell direction t)
	       (setq interval-count (1- interval-count))
	       (setq cells (1- cells))
	       (and (> n 0) (> cells 0)))))
    (table-recognize-cell 'force)
    (if (called-interactively-p 'interactive)
	(message "Sequencing...done"))
    ))

;;;###autoload
(defun table-delete-row (n)
  "Delete N row(s) of cells.
Delete N rows of cells from current row.  The current row is the row
contains the current cell where point is located.  Each row must
consists from cells of same height."
  (interactive "*p")
  (let ((orig-coord (table--get-coordinate))
	(bt-coord (table--get-coordinate (cdr (table--vertical-cell-list nil 'first-only))))
	lu-coord rb-coord rect)
    ;; determine the area to delete while testing row height uniformity
    (while (> n 0)
      (setq n (1- n))
      (unless (table--probe-cell)
	(error "Table not found"))
      (let ((cell-list (table--horizontal-cell-list 'left-to-right)))
	(unless
	    (and (table--uniform-list-p
		  (mapcar (lambda (cell) (cdr (table--get-coordinate (car cell)))) cell-list))
		 (table--uniform-list-p
		  (mapcar (lambda (cell) (cdr (table--get-coordinate (cdr cell)))) cell-list)))
	  (error "Cells in this row are not in uniform height"))
	(unless lu-coord
	  (setq lu-coord (table--get-coordinate (caar cell-list))))
	(setq rb-coord (table--get-coordinate (cdar (last cell-list))))
	(table--goto-coordinate (cons (car orig-coord) (+ 2 (cdr rb-coord))))))
    ;; copy the remaining area (below the deleting area)
    (setq rect (extract-rectangle
		(table--goto-coordinate (cons (1- (car lu-coord)) (1+ (cdr rb-coord))))
		(table--goto-coordinate (cons (1+ (car rb-coord)) (1+ (cdr bt-coord))))))
    ;; delete the deleting area and below together
    (delete-rectangle
     (table--goto-coordinate (cons (1- (car lu-coord)) (1- (cdr lu-coord))))
     (table--goto-coordinate (cons (1+ (car rb-coord)) (1+ (cdr bt-coord)))))
    (table--goto-coordinate (cons (1- (car lu-coord)) (1- (cdr lu-coord))))
    ;; insert the remaining area while appending blank lines below it
    (table--insert-rectangle
     (append rect (make-list (+ 2 (- (cdr rb-coord) (cdr lu-coord)))
			     (make-string (+ 2 (- (car rb-coord) (car lu-coord))) ?\s))))
    ;; remove the appended blank lines below the table if they are unnecessary
    (table--goto-coordinate (cons 0 (- (cdr bt-coord) (- (cdr rb-coord) (cdr lu-coord)))))
    (table--remove-blank-lines (+ 2 (- (cdr rb-coord) (cdr lu-coord))))
    ;; fix up intersections
    (let ((coord (cons (car lu-coord) (1- (cdr lu-coord))))
	  (n (1+ (- (car rb-coord) (car lu-coord)))))
      (while (> n 0)
	(table--goto-coordinate coord)
	(if (save-excursion
	      (or (and (table--goto-coordinate (cons (car coord) (1- (cdr coord))) 'no-extension)
		       (looking-at (regexp-quote (char-to-string table-cell-vertical-char))))
		  (and (table--goto-coordinate (cons (car coord) (1+ (cdr coord))) 'no-extension)
		       (looking-at (regexp-quote (char-to-string table-cell-vertical-char))))))
	    (progn
	      (delete-char 1)
	      (insert table-cell-intersection-char))
	  (delete-char 1)
	  (insert (string-to-char table-cell-horizontal-chars)))
	(setq n (1- n))
	(setcar coord (1+ (car coord)))))
    ;; goto appropriate end point
    (table--goto-coordinate (cons (car orig-coord) (cdr lu-coord)))))

;;;###autoload
(defun table-delete-column (n)
  "Delete N column(s) of cells.
Delete N columns of cells from current column.  The current column is
the column contains the current cell where point is located.  Each
column must consists from cells of same width."
  (interactive "*p")
  (let ((orig-coord (table--get-coordinate))
	lu-coord rb-coord)
    ;; determine the area to delete while testing column width uniformity
    (while (> n 0)
      (setq n (1- n))
      (unless (table--probe-cell)
	(error "Table not found"))
      (let ((cell-list (table--vertical-cell-list 'top-to-bottom)))
	(unless
	    (and (table--uniform-list-p
		  (mapcar (function (lambda (cell) (car (table--get-coordinate (car cell))))) cell-list))
		 (table--uniform-list-p
		  (mapcar (function (lambda (cell) (car (table--get-coordinate (cdr cell))))) cell-list)))
	  (error "Cells in this column are not in uniform width"))
	(unless lu-coord
	  (setq lu-coord (table--get-coordinate (caar cell-list))))
	(setq rb-coord (table--get-coordinate (cdar (last cell-list))))
	(table--goto-coordinate (cons (1+ (car rb-coord)) (cdr orig-coord)))))
    ;; delete the area
    (delete-rectangle
     (table--goto-coordinate (cons (car lu-coord) (1- (cdr lu-coord))))
     (table--goto-coordinate (cons (1+ (car rb-coord)) (1+ (cdr rb-coord)))))
    ;; fix up the intersections
    (let ((coord (cons (1- (car lu-coord)) (cdr lu-coord)))
	  (n (1+ (- (cdr rb-coord) (cdr lu-coord)))))
      (while (> n 0)
	(table--goto-coordinate coord)
	(if (save-excursion
	      (or (and (table--goto-coordinate (cons (1- (car coord)) (cdr coord)) 'no-extension)
		       (looking-at (regexp-opt-charset
				    (string-to-list table-cell-horizontal-chars))))
		  (and (table--goto-coordinate (cons (1+ (car coord)) (cdr coord)) 'no-extension)
		       (looking-at (regexp-opt-charset
				    (string-to-list table-cell-horizontal-chars))))))
	    (progn
	      (delete-char 1)
	      (insert table-cell-intersection-char))
	  (delete-char 1)
	  (insert table-cell-vertical-char))
	(setq n (1- n))
	(setcdr coord (1+ (cdr coord)))))
    ;; goto appropriate end point
    (table--goto-coordinate (cons (car lu-coord) (cdr orig-coord)))))

;;;###autoload
(defun table-capture (beg end &optional col-delim-regexp row-delim-regexp justify min-cell-width columns)
  "Convert plain text into a table by capturing the text in the region.
Create a table with the text in region as cell contents.  BEG and END
specify the region.  The text in the region is replaced with a table.
The removed text is inserted in the table.  When optional
COL-DELIM-REGEXP and ROW-DELIM-REGEXP are provided the region contents
is parsed and separated into individual cell contents by using the
delimiter regular expressions.  This parsing determines the number of
columns and rows of the table automatically.  If COL-DELIM-REGEXP and
ROW-DELIM-REGEXP are omitted the result table has only one cell and
the entire region contents is placed in that cell.  Optional JUSTIFY
is one of 'left, 'center or 'right, which specifies the cell
justification.  Optional MIN-CELL-WIDTH specifies the minimum cell
width.  Optional COLUMNS specify the number of columns when
ROW-DELIM-REGEXP is not specified.


Example 1:

1, 2, 3, 4
5, 6, 7, 8
, 9, 10

Running `table-capture' on above 3 line region with COL-DELIM-REGEXP
\",\" and ROW-DELIM-REGEXP \"\\n\" creates the following table.  In
this example the cells are centered and minimum cell width is
specified as 5.

+-----+-----+-----+-----+
|  1  |  2  |  3  |  4  |
+-----+-----+-----+-----+
|  5  |  6  |  7  |  8  |
+-----+-----+-----+-----+
|     |  9  | 10  |     |
+-----+-----+-----+-----+

Note:

In case the function is called interactively user must use \\[quoted-insert] `quoted-insert'
in order to enter \"\\n\" successfully.  COL-DELIM-REGEXP at the end
of each row is optional.


Example 2:

This example shows how a table can be used for text layout editing.
Let `table-capture' capture the following region starting from
-!- and ending at -*-, that contains three paragraphs and two item
name headers.  This time specify empty string for both
COL-DELIM-REGEXP and ROW-DELIM-REGEXP.

-!-`table-capture' is a powerful command however mastering its power
requires some practice.  Here is a list of items what it can do.

Parse Cell Items      By using column delimiter regular
		      expression and raw delimiter regular
		      expression, it parses the specified text
		      area and extracts cell items from
		      non-table text and then forms a table out
		      of them.

Capture Text Area     When no delimiters are specified it
		      creates a single cell table.  The text in
		      the specified region is placed in that
		      cell.-*-

Now the entire content is captured in a cell which is itself a table
like this.

+-----------------------------------------------------------------+
|`table-capture' is a powerful command however mastering its power|
|requires some practice.  Here is a list of items what it can do. |
|                                                                 |
|Parse Cell Items      By using column delimiter regular          |
|                      expression and raw delimiter regular       |
|                      expression, it parses the specified text   |
|                      area and extracts cell items from          |
|                      non-table text and then forms a table out  |
|                      of them.                                   |
|                                                                 |
|Capture Text Area     When no delimiters are specified it        |
|                      creates a single cell table.  The text in  |
|                      the specified region is placed in that     |
|                      cell.                                      |
+-----------------------------------------------------------------+

By splitting the cell appropriately we now have a table consisting of
paragraphs occupying its own cell.  Each cell can now be edited
independently.

+-----------------------------------------------------------------+
|`table-capture' is a powerful command however mastering its power|
|requires some practice.  Here is a list of items what it can do. |
+---------------------+-------------------------------------------+
|Parse Cell Items     |By using column delimiter regular          |
|                     |expression and raw delimiter regular       |
|                     |expression, it parses the specified text   |
|                     |area and extracts cell items from          |
|                     |non-table text and then forms a table out  |
|                     |of them.                                   |
+---------------------+-------------------------------------------+
|Capture Text Area    |When no delimiters are specified it        |
|                     |creates a single cell table.  The text in  |
|                     |the specified region is placed in that     |
|                     |cell.                                      |
+---------------------+-------------------------------------------+

By applying `table-release', which does the opposite process, the
contents become once again plain text.  `table-release' works as
companion command to `table-capture' this way.
"
  (interactive
   (let ((col-delim-regexp)
	 (row-delim-regexp))
     (barf-if-buffer-read-only)
     (if (table--probe-cell)
	 (error "Can't insert a table inside a table"))
     (list
      (mark) (point)
      (setq col-delim-regexp
	    (read-from-minibuffer "Column delimiter regexp: "
				  (car table-col-delim-regexp-history) nil nil 'table-col-delim-regexp-history))
      (setq row-delim-regexp
	    (read-from-minibuffer "Row delimiter regexp: "
				  (car table-row-delim-regexp-history) nil nil 'table-row-delim-regexp-history))
      (let* ((completion-ignore-case t)
	     (default (car table-capture-justify-history)))
	(if (and (string= col-delim-regexp "") (string= row-delim-regexp "")) 'left
	  (intern
	   (downcase (completing-read
		      (format "Justify (default %s): " default)
		      '(("left") ("center") ("right"))
		      nil t nil 'table-capture-justify-history default)))))
      (if (and (string= col-delim-regexp "") (string= row-delim-regexp "")) "1"
	(table--read-from-minibuffer '("Minimum cell width" . table-capture-min-cell-width-history)))
      (if (and (not (string= col-delim-regexp "")) (string= row-delim-regexp ""))
	  (string-to-number
	   (table--read-from-minibuffer '("Number of columns" . table-capture-columns-history)))
	nil)
      )))
  (if (> beg end) (let ((tmp beg)) (setq beg end) (setq end tmp)))
  (if (string= col-delim-regexp "") (setq col-delim-regexp nil))
  (if (string= row-delim-regexp "") (setq row-delim-regexp nil))
  (if (and columns (< columns 1)) (setq columns nil))
  (unless min-cell-width (setq min-cell-width "5"))
  (let ((contents (buffer-substring beg end))
	(cols 0) (rows 0) c r cell-list
	(delim-pattern
	 (if (and col-delim-regexp row-delim-regexp)
	     (format "\\(\\(%s\\)?\\s *\\(%s\\)\\s *\\)\\|\\(\\(%s\\)\\s *\\)"
		     col-delim-regexp row-delim-regexp col-delim-regexp)
	   (if col-delim-regexp
	       (format "\\(\\)\\(\\)\\(\\)\\(\\(%s\\)\\s *\\)" col-delim-regexp))))
	(contents-list))
    ;; when delimiters are specified extract cells and determine the cell dimension
    (if delim-pattern
	(with-temp-buffer
	  (insert contents)
	  ;; make sure the contents ends with a newline
	  (goto-char (point-max))
	  (unless (zerop (current-column))
	    (insert ?\n))
	  ;; skip the preceding white spaces
	  (goto-char (point-min))
	  (if (looking-at "\\s +")
	      (goto-char (match-end 0)))
	  ;; extract cell contents
	  (let ((from (point)))
	    (setq cell-list nil)
	    (setq c 0)
	    (while (and (re-search-forward delim-pattern nil t)
			(cond
			 ;; row delimiter
			 ((and (match-string 1) (not (string= (match-string 1) "")))
			  (setq rows (1+ rows))
			  (setq cell-list
				(append cell-list (list (buffer-substring from (match-beginning 1)))))
			  (setq from (match-end 1))
			  (setq contents-list
				(append contents-list (list cell-list)))
			  (setq cell-list nil)
			  (setq c (1+ c))
			  (if (> c cols) (setq cols c))
			  (setq c 0)
			  t)
			 ;; column delimiter
			 ((and (match-string 4) (not (string= (match-string 4) "")))
			  (setq cell-list
				(append cell-list (list (buffer-substring from (match-beginning 4)))))
			  (setq from (match-end 4))
			  (setq c (1+ c))
			  (if (> c cols) (setq cols c))
			  t)
			 (t nil))))
	    ;; take care of the last element without a post delimiter
	    (unless (null (looking-at ".+$"))
	      (setq cell-list
		    (append cell-list (list (match-string 0))))
	      (setq cols (1+ cols)))
	    ;; take care of the last row without a terminating delimiter
	    (unless (null cell-list)
	      (setq rows (1+ rows))
	      (setq contents-list
		    (append contents-list (list cell-list)))))))
    ;; finalize the table dimension
    (if (and columns contents-list)
	;; when number of columns are specified and cells are parsed determine the dimension
	(progn
	  (setq cols columns)
	  (setq rows (/ (+ (length (car contents-list)) columns -1) columns)))
      ;; when dimensions are not specified default to a single cell table
      (if (zerop rows) (setq rows 1))
      (if (zerop cols) (setq cols 1)))
    ;; delete the region and reform line breaks
    (delete-region beg end)
    (goto-char beg)
    (unless (zerop (current-column))
      (insert ?\n))
    (unless (looking-at "\\s *$")
      (save-excursion
	(insert ?\n)))
    ;; insert the table
    ;; insert the cell contents
    (if (null contents-list)
	;; single cell
	(let ((width) (height))
	  (with-temp-buffer
	    (insert contents)
	    (table--remove-eol-spaces (point-min) (point-max))
	    (table--untabify (point-min) (point-max))
	    (setq width (table--measure-max-width))
	    (setq height (1+ (table--current-line (point-max))))
	    (setq contents (buffer-substring (point-min) (point-max))))
	  (table-insert cols rows width height)
	  (table-with-cache-buffer
	    (insert contents)
	    (setq table-inhibit-auto-fill-paragraph t)))
      ;; multi cells
      (table-insert cols rows min-cell-width 1)
      (setq r 0)
      (setq cell-list nil)
      (while (< r rows)
	(setq r (1+ r))
	(setq c 0)
	(unless cell-list
	  (setq cell-list (car contents-list))
	  (setq contents-list (cdr contents-list)))
	(while (< c cols)
	  (setq c (1+ c))
	  (if (car cell-list)
	      (table-with-cache-buffer
		(insert (car cell-list))
		(setq cell-list (cdr cell-list))
		(setq table-cell-info-justify justify)))
	  (table-forward-cell 1))))))

;;;###autoload
(defun table-release ()
  "Convert a table into plain text by removing the frame from a table.
Remove the frame from a table and deactivate the table.  This command
converts a table into plain text without frames.  It is a companion to
`table-capture' which does the opposite process."
  (interactive)
  (let ((origin-cell (table--probe-cell))
	table-lu table-rb)
    (if origin-cell
	(let ((old-point (point-marker)))
	  ;; save-excursion is not sufficient for this
	  ;; because untabify operation moves point
	  (set-marker-insertion-type old-point t)
	  (unwind-protect
	      (progn
		(while
		    (progn
		      (table-forward-cell 1 nil 'unrecognize)
		      (let ((cell (table--probe-cell)))
			(if (or (null table-lu)
				(< (car cell) table-lu))
			    (setq table-lu (car cell)))
			(if (or (null table-rb)
				(> (cdr cell) table-rb))
			    (setq table-rb (cdr cell)))
			(and cell (not (equal cell origin-cell))))))
		(let* ((lu-coord (table--get-coordinate table-lu))
		       (rb-coord (table--get-coordinate table-rb))
		       (lu (table--goto-coordinate (table--offset-coordinate lu-coord '(-1 . -1)))))
		  (table--spacify-frame)
		  (setcdr rb-coord (1+ (cdr rb-coord)))
		  (delete-rectangle lu (table--goto-coordinate (cons (car lu-coord) (cdr rb-coord))))
		  (table--remove-eol-spaces
		   (table--goto-coordinate (cons 0 (1- (cdr lu-coord))))
		   (table--goto-coordinate rb-coord) nil t)))
	    (goto-char old-point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Worker functions (executed implicitly)
;;

(defun table--make-cell-map ()
  "Make the table cell keymap if it does not exist yet."
  ;; this is irrelevant to keymap but good place to make sure to be executed
  (table--update-cell-face)
  (unless table-cell-map
    (let ((map (make-sparse-keymap))
	  (remap-alist table-command-remap-alist))
      ;; table-command-prefix mode specific bindings
      (if (vectorp table-command-prefix)
	  (mapc (lambda (binding)
		  (let ((seq (copy-sequence (car binding))))
		    (and (vectorp seq)
			 (listp (aref seq 0))
			 (eq (car (aref seq 0)) 'control)
			 (progn
			   (aset seq 0 (cadr (aref seq 0)))
			   (define-key map (vconcat table-command-prefix seq) (cdr binding))))))
		table-cell-bindings))
      ;; shorthand control bindings
      (mapc (lambda (binding)
	      (define-key map (car binding) (cdr binding)))
	    table-cell-bindings)
      ;; remap normal commands to table specific version
      (while remap-alist
	(define-key map (vector 'remap (caar remap-alist)) (cdar remap-alist))
	(setq remap-alist (cdr remap-alist)))
      ;;
      (setq table-cell-map map)
      (fset 'table-cell-map map)))
  ;; add menu for table cells
  (unless table-disable-menu
    (easy-menu-define table-cell-menu-map table-cell-map "Table cell menu" table-cell-menu)
    (if (featurep 'xemacs)
	(easy-menu-add table-cell-menu)))
  (run-hooks 'table-cell-map-hook))

;; Create the keymap after running the user init file so that the user
;; modification to the global-map is accounted.
(add-hook 'after-init-hook 'table--make-cell-map t)

(defun *table--cell-self-insert-command ()
  "Table cell version of `self-insert-command'."
  (interactive "*")
  (let ((char last-command-event))
    (if (eq buffer-undo-list t) nil
      (if (not (eq last-command this-command))
	  (setq table-cell-self-insert-command-count 0)
	(if (car buffer-undo-list) nil
	  (if (>= table-cell-self-insert-command-count 19)
	      (setq table-cell-self-insert-command-count 0)
	    (setq buffer-undo-list (cdr buffer-undo-list))
	    (setq table-cell-self-insert-command-count (1+ table-cell-self-insert-command-count))))))
    (table--cell-insert-char char overwrite-mode)))

(defun *table--cell-delete-backward-char (n)
  "Table cell version of `delete-backward-char'."
  (interactive "*p")
  (*table--cell-delete-char (- n)))

(defun *table--cell-newline (&optional indent)
  "Table cell version of `newline'."
  (interactive "*")
  (table-with-cache-buffer
    (let ((column (current-column)))
      (insert ?\n)
      (if indent (indent-to-column column))
      ;; fill only when at the beginning of paragraph
      (if (= (point)
	     (save-excursion
	       (forward-paragraph -1)
	       (if (looking-at "\\s *$")
		   (forward-line 1))
	       (point)))
	  nil		      ; yes, at the beginning of the paragraph
	(setq table-inhibit-auto-fill-paragraph t)))))

(defun *table--cell-open-line (n)
  "Table cell version of `open-line'."
  (interactive "*p")
  (table-with-cache-buffer
    (save-excursion
      (insert (make-string n ?\n))
      (table--fill-region (point) (point))
      (setq table-inhibit-auto-fill-paragraph t))))

(defun *table--cell-newline-and-indent ()
  "Table cell version of `newline-and-indent'."
  (interactive)
  (*table--cell-newline t))

(defun *table--cell-delete-char (n)
  "Table cell version of `delete-char'."
  (interactive "*p")
  (let ((overwrite overwrite-mode))
    (table-with-cache-buffer
      (if (and overwrite (< n 0))
	  (progn
	    (while (not (zerop n))
	      (let ((coordinate (table--get-coordinate)))
		(if (zerop (car coordinate))
		    (unless (zerop (cdr coordinate))
		      (table--goto-coordinate (cons (1- table-cell-info-width) (1- (cdr coordinate))))
		      (unless (eolp)
			(delete-char 1)))
		  (delete-char -1)
		  (insert ?\s)
		  (forward-char -1)))
	      (setq n (1+ n)))
	    (setq table-inhibit-auto-fill-paragraph t))
	(let ((coordinate (table--get-coordinate))
	      (end-marker (copy-marker (+ (point) n)))
	      (deleted))
	  (if (or (< end-marker (point-min))
		  (> end-marker (point-max))) nil
	    (table--remove-eol-spaces (point-min) (point-max))
	    (setq deleted (buffer-substring (point) end-marker))
	    (delete-char n)
	    ;; in fixed width mode when two lines are concatenated
	    ;; remove continuation character if there is one.
	    (and table-fixed-width-mode
		 (string-match "^\n" deleted)
		 (equal (char-before) table-word-continuation-char)
		 (delete-char -2))
	    ;; see if the point is placed at the right tip of the previous
	    ;; blank line, if so get rid of the preceding blanks.
	    (if (and (not (bolp))
		     (/= (cdr coordinate) (cdr (table--get-coordinate)))
		     (let ((end (point)))
		       (save-excursion
			 (beginning-of-line)
			 (re-search-forward "\\s +" end t)
			 (= (point) end))))
		(replace-match ""))
	    ;; do not fill the paragraph if the point is already at the end
	    ;; of this paragraph and is following a blank character
	    ;; (otherwise the filling squeezes the preceding blanks)
	    (if (and (looking-at "\\s *$")
		     (or (bobp)
			 (save-excursion
			   (backward-char)
			   (looking-at "\\s "))))
		(setq table-inhibit-auto-fill-paragraph t))
	    )
	  (set-marker end-marker nil))))))

(defun *table--cell-quoted-insert (arg)
  "Table cell version of `quoted-insert'."
  (interactive "*p")
  (let ((char (read-quoted-char)))
    (while (> arg 0)
      (table--cell-insert-char char nil)
      (setq arg (1- arg)))))

(defun *table--cell-describe-mode ()
  "Table cell version of `describe-mode'."
  (interactive)
  (if (not (table--point-in-cell-p))
      (call-interactively 'describe-mode)
    (with-output-to-temp-buffer "*Help*"
      (princ "Table mode: (in ")
      (princ (format-mode-line mode-name nil nil (current-buffer)))
      (princ " mode)

Table is not a mode technically.  You can regard it as a pseudo mode
which exists locally within a buffer.  It overrides some standard
editing behaviors.  Editing operations in a table produces confined
effects to the current cell.  It may grow the cell horizontally and/or
vertically depending on the newly entered or deleted contents of the
cell, and also depending on the current mode of cell.

In the normal mode the table preserves word continuity.  Which means
that a word never gets folded into multiple lines.  For this purpose
table will occasionally grow the cell width.  On the other hand, when
in a fixed width mode all cell width are fixed.  When a word can not
fit in the cell width the word is folded into the next line.  The
folded location is marked by a continuation character which is
specified in the variable `table-word-continuation-char'.
")
      (help-print-return-message))))

(defun *table--cell-describe-bindings ()
  "Table cell version of `describe-bindings'."
  (interactive)
  (if (not (table--point-in-cell-p))
      (call-interactively 'describe-bindings)
    (with-output-to-temp-buffer "*Help*"
      (princ "Table Bindings:
key             binding
---             -------

")
      (mapc (lambda (binding)
	      (princ (format "%-16s%s\n"
			     (key-description (car binding))
			     (cdr binding))))
	    table-cell-bindings)
      (help-print-return-message))))

(defun *table--cell-dabbrev-expand (arg)
  "Table cell version of `dabbrev-expand'."
  (interactive "*P")
  (let ((dabbrev-abbrev-char-regexp (concat "[^"
					    (char-to-string table-cell-vertical-char)
					    (char-to-string table-cell-intersection-char)
					    " \n]")))
    (table-with-cache-buffer
      (dabbrev-expand arg))))

(defun *table--cell-dabbrev-completion (&optional arg)
  "Table cell version of `dabbrev-completion'."
  (interactive "*P")
  (error "`dabbrev-completion' is incompatible with table")
  (let ((dabbrev-abbrev-char-regexp (concat "[^"
					    (char-to-string table-cell-vertical-char)
					    (char-to-string table-cell-intersection-char)
					    " \n]")))
    (table-with-cache-buffer
      (dabbrev-completion arg))))

(defun *table--present-cell-popup-menu (event)
  "Present and handle cell popup menu."
  (interactive "e")
  (unless table-disable-menu
    (select-window (posn-window (event-start event)))
    (goto-char (posn-point (event-start event)))
    (let ((item-list (x-popup-menu event table-cell-menu-map))
	  (func table-cell-menu-map))
      (while item-list
	(setq func (nth 3 (assoc (car item-list) func)))
	(setq item-list (cdr item-list)))
      (if (and (symbolp func) (fboundp func))
	  (call-interactively func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Cell updating functions
;;

(defun table--update-cell (&optional now)
  "Update the table cell contents.
When the optional parameter NOW is nil it only sets up the update
timer.  If it is non-nil the function copies the contents of the cell
cache buffer into the designated cell in the table buffer."
  (if (null table-update-timer) nil
    (table--cancel-timer table-update-timer)
    (setq table-update-timer nil))
  (if (or (not now)
	  (and (boundp 'quail-converting)
	       quail-converting) ;; defer operation while current quail work is not finished.
	  (and (boundp 'quail-translating)
	       quail-translating))
      (setq table-update-timer
	    (table--set-timer table-time-before-update
			      (function table--update-cell)
			      'now))
    (save-current-buffer
      (set-buffer table-cell-buffer)
      (let ((cache-buffer (get-buffer-create table-cache-buffer-name))
	    (org-coord (table--get-coordinate))
	    (in-cell (equal (table--cell-to-coord (table--probe-cell))
			    (cons table-cell-info-lu-coordinate table-cell-info-rb-coordinate)))
	    rectangle)
	(set-buffer cache-buffer)
	(setq rectangle
	      (extract-rectangle
	       1
	       (table--goto-coordinate (cons table-cell-info-width (1- table-cell-info-height)))))
	(set-buffer table-cell-buffer)
	(delete-rectangle (table--goto-coordinate table-cell-info-lu-coordinate)
			  (table--goto-coordinate table-cell-info-rb-coordinate))
	(table--goto-coordinate table-cell-info-lu-coordinate)
	(table--insert-rectangle rectangle)
	(let* ((cell (table--probe-cell))) ; must probe again in case of wide characters
	  (table--put-cell-property cell)
	  (table--put-cell-justify-property cell table-cell-info-justify)
	  (table--put-cell-valign-property cell table-cell-info-valign))
	(table--goto-coordinate
	 (if in-cell
	     (table--transcoord-cache-to-table table-cell-cache-point-coordinate)
	   org-coord))))
    ;; simulate undo behavior under overwrite-mode
    (if (and overwrite-mode (not (eq buffer-undo-list t)))
	(setq buffer-undo-list (cons nil buffer-undo-list)))))

(defun table--update-cell-widened (&optional now)
  "Update the contents of the cells that are affected by widening operation."
  (if (null table-widen-timer) nil
    (table--cancel-timer table-widen-timer)
    (setq table-widen-timer nil))
  (if (not now)
      (setq table-widen-timer
	    (table--set-timer (+ table-time-before-update table-time-before-reformat)
			      (function table--update-cell-widened)
			      'now))
    (save-current-buffer
      (if table-update-timer
	  (table--update-cell 'now))
      (set-buffer table-cell-buffer)
      (let* ((current-coordinate (table--get-coordinate))
	     (current-cell-coordinate (table--cell-to-coord (table--probe-cell)))
	     (cell-coord-list (progn
				(table--goto-coordinate table-cell-info-lu-coordinate)
				(table--cell-list-to-coord-list (table--vertical-cell-list)))))
	(while cell-coord-list
	  (let* ((cell-coord (prog1 (car cell-coord-list) (setq cell-coord-list (cdr cell-coord-list))))
		 (currentp (equal cell-coord current-cell-coordinate)))
	    (if currentp (table--goto-coordinate current-coordinate)
	      (table--goto-coordinate (car cell-coord)))
	    (table-recognize-cell 'froce)
	    (let ((table-inhibit-update t))
	      (table-with-cache-buffer
		(let ((sticky (and currentp
				   (save-excursion
				     (unless (bolp) (forward-char -1))
				     (looking-at ".*\\S ")))))
		  (table--fill-region (point-min) (point-max))
		  (if sticky
		      (setq current-coordinate (table--transcoord-cache-to-table))))))
	    (table--update-cell 'now)
	    ))
	(table--goto-coordinate current-coordinate)
	(table-recognize-cell 'froce)))))

(defun table--update-cell-heightened (&optional now)
  "Update the contents of the cells that are affected by heightening operation."
  (if (null table-heighten-timer) nil
    (table--cancel-timer table-heighten-timer)
    (setq table-heighten-timer nil))
  (if (not now)
      (setq table-heighten-timer
	    (table--set-timer (+ table-time-before-update table-time-before-reformat)
			      (function table--update-cell-heightened)
			      'now))
    (save-current-buffer
      (if table-update-timer
	  (table--update-cell 'now))
      (if table-widen-timer
	  (table--update-cell-widened 'now))
      (set-buffer table-cell-buffer)
      (let* ((current-coordinate (table--get-coordinate))
	     (current-cell-coordinate (table--cell-to-coord (table--probe-cell)))
	     (cell-coord-list (progn
				(table--goto-coordinate table-cell-info-lu-coordinate)
				(table--cell-list-to-coord-list (table--horizontal-cell-list)))))
	(while cell-coord-list
	  (let* ((cell-coord (prog1 (car cell-coord-list) (setq cell-coord-list (cdr cell-coord-list))))
		 (currentp (equal cell-coord current-cell-coordinate)))
	    (if currentp (table--goto-coordinate current-coordinate)
	      (table--goto-coordinate (car cell-coord)))
	    (table-recognize-cell 'froce)
	    (let ((table-inhibit-update t))
	      (table-with-cache-buffer
		(let ((sticky (and currentp
				   (save-excursion
				     (unless (bolp) (forward-char -1))
				     (looking-at ".*\\S ")))))
		  (table--valign)
		  (if sticky
		      (setq current-coordinate (table--transcoord-cache-to-table))))))
	    (table--update-cell 'now)
	    ))
	(table--goto-coordinate current-coordinate)
	(table-recognize-cell 'froce)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Service functions (for external packages)
;;

(defun table-goto-top-left-corner ()
  "Move point to top left corner of the current table and return the char position."
  (table--goto-coordinate
   (cons
    (1- (car (table--get-coordinate (car (table--horizontal-cell-list t t)))))
    (1- (cdr (table--get-coordinate (car (table--vertical-cell-list t t))))))))

(defun table-goto-top-right-corner ()
  "Move point to top right corner of the current table and return the char position."
  (table--goto-coordinate
   (cons
    (car (table--get-coordinate (cdr (table--horizontal-cell-list nil t))))
    (1- (cdr (table--get-coordinate (car (table--vertical-cell-list t t))))))))

(defun table-goto-bottom-left-corner ()
  "Move point to bottom left corner of the current table and return the char position."
  (table--goto-coordinate
   (cons
    (1- (car (table--get-coordinate (car (table--horizontal-cell-list t t)))))
    (1+ (cdr (table--get-coordinate (cdr (table--vertical-cell-list nil t))))))))

(defun table-goto-bottom-right-corner ()
  "Move point to bottom right corner of the current table and return the char position."
  (table--goto-coordinate
   (cons
    (car (table--get-coordinate (cdr (table--horizontal-cell-list nil t))))
    (1+ (cdr (table--get-coordinate (cdr (table--vertical-cell-list nil t))))))))

(defun table-call-interactively (function &optional record-flag keys)
  "Call FUNCTION, or a table version of it if applicable.
See `call-interactively' for full description of the arguments."
  (let ((table-func (intern-soft (format "*table--cell-%s" function))))
    (call-interactively
     (if (and table-func
	      (table--point-in-cell-p))
	 table-func
       function) record-flag keys)))

(defun table-funcall (function &rest arguments)
  "Call FUNCTION, or a table version of it if applicable.
See `funcall' for full description of the arguments."
  (let ((table-func (intern-soft (format "*table--cell-%s" function))))
    (apply
     (if (and table-func
	      (table--point-in-cell-p))
	 table-func
       function)
     arguments)))

(defmacro table-apply (function &rest arguments)
  "Call FUNCTION, or a table version of it if applicable.
See `apply' for full description of the arguments."
  (let ((table-func (make-symbol "table-func")))
    `(let ((,table-func (intern-soft (format "*table--cell-%s" ,function))))
       (apply
	(if (and ,table-func
		 (table--point-in-cell-p))
	    ,table-func
	  ,function)
	,@arguments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility functions
;;

(defun table--read-from-minibuffer (prompt-history)
  "A wrapper to `read-from-minibuffer'.
PROMPT-HISTORY is a cons cell which car is the prompt string and the
cdr is the history symbol."
  (let ((default (car (symbol-value (cdr prompt-history)))))
    (read-from-minibuffer
     (format "%s (default %s): " (car prompt-history) default)
     "" nil nil (cdr prompt-history) default))
  (and (featurep 'xemacs)
       (equal (car (symbol-value (cdr prompt-history))) "")
       (set (cdr prompt-history)
	    (cdr (symbol-value (cdr prompt-history)))))
  (car (symbol-value (cdr prompt-history))))

(defun table--buffer-substring-and-trim (beg end)
  "Extract buffer substring and remove blanks from front and the rear of it."
  (save-excursion
    (save-restriction
      (narrow-to-region (goto-char beg) end)
      (if (re-search-forward "\\s *")
	  (setq beg (match-end 0)))
      (if (re-search-forward "\\s *\\'" end t)
	  (setq end (match-beginning 0)))
      (table--remove-cell-properties
       0 (- end beg)
       (buffer-substring beg end)))))

(defun table--valign ()
  "Vertically align the cache cell contents.
Current buffer must be the cache buffer at the entry to this function.
Returns the coordinate of the final point location."
  (if (or (null table-cell-info-valign)
	  (eq table-cell-info-valign 'none))
      (table--get-coordinate)
    (let ((saved-point (point-marker)))
      ;;(set-marker-insertion-type saved-point t)
      (goto-char (point-min))
      (let* ((from (and (re-search-forward "^.*\\S " nil t)
			(table--current-line)))
	     (to (let ((tmp from))
		   (while (re-search-forward "^.*\\S " nil t)
		     (setq tmp (table--current-line)))
		   tmp))
	     (content-height (and from to (1+ (- to from)))))
	(unless (null content-height)
	  (goto-char (point-min))
	  (if (looking-at "\\s *\n")
	      (replace-match ""))
	  (cond ((eq table-cell-info-valign 'middle)
		 (insert (make-string (/ (- table-cell-info-height content-height) 2) ?\n)))
		((eq table-cell-info-valign 'bottom)
		 (insert (make-string (- table-cell-info-height content-height) ?\n))))
	  (table--goto-coordinate (cons table-cell-info-width (1- table-cell-info-height)))
	  (if (re-search-forward "\\s +\\'" nil t)
	      (replace-match ""))))
      (goto-char saved-point)
      (set-marker saved-point nil)
      (let ((coord (table--get-coordinate)))
	(unless (< (cdr coord) table-cell-info-height)
	  (setcdr coord (1- table-cell-info-height))
	  (table--goto-coordinate coord))
	coord))))

(defun table--query-justification ()
  (barf-if-buffer-read-only)
  (let* ((completion-ignore-case t)
	 (default (car table-justify-history)))
    (intern (downcase (completing-read
		       (format "Justify (default %s): " default)
		       '(("left") ("center") ("right") ("top") ("middle") ("bottom") ("none"))
		       nil t nil 'table-justify-history default)))))

(defun table--spacify-frame ()
  "Spacify table frame.
Replace frame characters with spaces."
  (let ((frame-char
         (append (string-to-list table-cell-horizontal-chars)
                 (list table-cell-intersection-char table-cell-vertical-char))))
    (while
	(progn
	  (cond
	   ((eq (char-after) table-cell-intersection-char)
	    (save-excursion
	      (let ((col (current-column)))
		(and (zerop (forward-line 1))
		     (zerop (current-column))
		     (move-to-column col)
		     (table--spacify-frame))))
	    (delete-char 1)
	    (insert-before-markers ?\s))
	   ((table--cell-horizontal-char-p (char-after))
	    (while (progn
		     (delete-char 1)
		     (insert-before-markers ?\s)
		     (table--cell-horizontal-char-p (char-after)))))
	   ((eq (char-after) table-cell-vertical-char)
	    (while (let ((col (current-column)))
		     (delete-char 1)
		     (insert-before-markers ?\s)
		     (and (zerop (forward-line 1))
			  (zerop (current-column))
			  (move-to-column col)
			  (eq (char-after) table-cell-vertical-char))))))
	  (memq (char-after) frame-char)))))

(defun table--remove-blank-lines (n)
  "Delete N blank lines from the current line.
For adjusting below area of the table when the table is shortened."
  (move-to-column 0)
  (let ((first-blank t))
    (while (> n 0)
      (setq n (1- n))
      (cond ((looking-at "\\s *\\'")
	     (delete-region (match-beginning 0) (match-end 0))
	     (setq n 0))
	    ((and (looking-at "\\([ \t]*\n[ \t]*\\)\n") first-blank)
	     (delete-region (match-beginning 1) (match-end 1)))
	    ((looking-at "[ \t]*$")
	     (delete-region (match-beginning 0) (match-end 0))
	     (forward-line 1))
	    (t
	     (setq first-blank nil)
	     (forward-line 1))))))

(defun table--uniform-list-p (l)
  "Return nil when LIST contains non equal elements.  Otherwise return t."
  (if (null l) t
    (catch 'end
      (while (cdr l)
	(if (not (equal (car l) (cadr l))) (throw 'end nil))
	(setq l (cdr l)))
      t)))

(defun table--detect-cell-alignment (cell)
  "Detect CELL contents alignment.
Guess CELL contents alignment both horizontally and vertically by
looking at the appearance of the CELL contents."
  (let ((cell-contents (extract-rectangle (car cell) (cdr cell)))
	(left-margin 0)
	(right-margin 0)
	(top-margin 0)
	(bottom-margin 0)
	(margin-diff 0)
	(margin-info-available nil)
	justify valign)
    (with-temp-buffer
      (table--insert-rectangle cell-contents)
      ;; determine the horizontal justification
      (goto-char (point-min))
      (while (re-search-forward "^\\( *\\).*[^ \n]\\( *\\)$" nil t)
	(setq margin-info-available t)
	(let* ((lm (- (match-end 1) (match-beginning 1)))
	       (rm (- (match-end 2) (match-beginning 2)))
	       (md (abs (- lm rm))))
	  (if (> lm left-margin)
	      (setq left-margin lm))
	  (if (> rm right-margin)
	      (setq right-margin rm))
	  (if (> md margin-diff)
	      (setq margin-diff md))))
      (setq justify
	    (cond
	     ((and margin-info-available
		   (<= margin-diff 1)
		   (> left-margin 0)) 'center)
	     ((and margin-info-available
		   (zerop right-margin)
		   (> left-margin 0)) 'right)
	     (t 'left)))
      ;; determine the vertical justification
      (goto-char (point-min))
      (if (and (re-search-forward "\\s *\\S " nil t)
	       (/= (match-beginning 0) (match-end 0)))
	  (setq top-margin (1- (count-lines (match-beginning 0) (match-end 0)))))
      (if (and (re-search-forward "\\s *\\'" nil t)
	       (/= (match-beginning 0) (match-end 0)))
	  (setq bottom-margin (1- (count-lines (match-beginning 0) (match-end 0)))))
      (setq valign
	    (cond
	     ((and (> top-margin 0)
		   (> bottom-margin 0)
		   (<= (abs (- top-margin bottom-margin)) 1)) 'middle)
	     ((and (> top-margin 0)
		   (zerop bottom-margin)) 'bottom)
	     (t nil))))
    (table--put-cell-justify-property cell justify)
    (table--put-cell-valign-property cell valign)))

(defun table--string-to-number-list (str)
  "Return a list of numbers in STR."
  (let ((idx 0)
	(nl nil))
    (while (string-match "[-0-9.]+" str idx)
      (setq idx (match-end 0))
      (setq nl (cons (string-to-number (match-string 0 str)) nl)))
    (nreverse nl)))

(defun table--justify-cell-contents (justify &optional paragraph)
  "Justify the current cell contents.
JUSTIFY is a symbol 'left, 'center or 'right for horizontal, or 'top,
'middle, 'bottom or 'none for vertical.  When PARAGRAPH is non-nil the
justify operation is limited to the current paragraph."
  (table-with-cache-buffer
    (let ((beg (point-min))
	  (end (point-max-marker))
	  (fill-column table-cell-info-width)
	  (adaptive-fill-mode nil)
	  (valign-symbols '(top middle bottom none)))
      (unless paragraph
	(if (memq justify valign-symbols)
	    (setq table-cell-info-valign
		  (if (eq justify 'none) nil justify))
	  (setq table-cell-info-justify justify)))
      (save-excursion
	(if paragraph
	    (let ((paragraph-start "\n"))
	      (forward-paragraph)
	      (or (bolp) (newline 1))
	      (set-marker end (point))
	      (setq beg (progn (forward-paragraph -1) (point)))))
	(if (memq justify valign-symbols)
	    (table--valign)
	  (table--remove-eol-spaces beg end 'bol)
	  (let ((paragraph-start table-paragraph-start))
	    (fill-region beg end table-cell-info-justify))))
      (setq table-inhibit-auto-fill-paragraph t)
      (set-marker end nil)))
  (table--update-cell 'now))

(defun table--horizontally-shift-above-and-below (columns-to-extend top-to-bottom-coord-list)
  "Horizontally shift outside contents right above and right below of the table.
This function moves the surrounding text outside of the table so that
they match the horizontal growth/shrink of the table.  It also
untabify the shift affected area including the right side of the table
so that tab related uneven shifting is avoided.  COLUMNS-TO-EXTEND
specifies the number of columns the table grows, or shrinks if
negative.  TOP-TO-BOTTOM-COORD-LIST is the vertical cell coordinate
list.  This list can be any vertical list within the table."
  (save-excursion
    (let (beg-coord end-coord)
      (table--goto-coordinate (caar top-to-bottom-coord-list))
      (let* ((cell (table--horizontal-cell-list nil 'first-only 'top))
	     (coord (cons (car (table--get-coordinate (cdr cell)))
			  (cdr (table--get-coordinate (car cell))))))
	(setcar coord (1+ (car coord)))
	(setcdr coord (- (cdr coord) 2))
	(setq beg-coord (cons (car coord) (1+ (cdr coord))))
	(while (and (table--goto-coordinate coord 'no-extension)
		    (not (looking-at "\\s *$")))
	  (if (< columns-to-extend 0)
	      (progn
		(table--untabify-line)
		(delete-char columns-to-extend))
	    (table--untabify-line (point))
	    (insert (make-string columns-to-extend ?\s)))
	  (setcdr coord (1- (cdr coord)))))
      (table--goto-coordinate (caar (last top-to-bottom-coord-list)))
      (let ((coord (table--get-coordinate (cdr (table--horizontal-cell-list nil 'first-only 'bottom)))))
	(setcar coord (1+ (car coord)))
	(setcdr coord (+ (cdr coord) 2))
	(setq end-coord (cons (car coord) (1- (cdr coord))))
	(while (and (table--goto-coordinate coord 'no-extension)
		    (not (looking-at "\\s *$")))
	  (if (< columns-to-extend 0)
	      (progn
		(table--untabify-line)
		(delete-char columns-to-extend))
	    (table--untabify-line (point))
	    (insert (make-string columns-to-extend ?\s)))
	  (setcdr coord (1+ (cdr coord)))))
      (while (<= (cdr beg-coord) (cdr end-coord))
	(table--untabify-line (table--goto-coordinate beg-coord 'no-extension))
	(setcdr beg-coord (1+ (cdr beg-coord)))))))

(defun table--create-growing-space-below (lines-to-extend left-to-right-coord-list bottom-border-y)
  "Create growing space below the table.
This function creates growing space below the table slightly
intelligent fashion.  Following is the cases it handles for each
growing line:
  1. When the first line below the table is a complete blank line it
inserts a blank line.
  2. When the line starts with a prefix that matches the prefix of the
bottom line of the table it inserts a line consisting of prefix alone.
  3. Otherwise it deletes the rectangular contents where table will
grow into."
  (save-excursion
    (let ((i 0)
	  (prefix (and (table--goto-coordinate (cons 0 bottom-border-y))
		       (re-search-forward
			".*\\S "
			(save-excursion
			  (table--goto-coordinate
			   (cons (1- (caar (car left-to-right-coord-list))) bottom-border-y)))
			t)
		       (buffer-substring (match-beginning 0) (match-end 0)))))
      (while (< i lines-to-extend)
	(let ((y (+ i bottom-border-y 1)))
	  (table--goto-coordinate (cons 0 y))
	  (cond
	   ((looking-at "\\s *$")
	    (insert ?\n))
	   ((and prefix (looking-at (concat (regexp-quote prefix) "\\s *$")))
	    (insert prefix ?\n))
	   (t
	    (delete-rectangle
	     (table--goto-coordinate (cons (1- (caar (car left-to-right-coord-list))) y))
	     (table--goto-coordinate (cons (1+ (cadr (car (last left-to-right-coord-list)))) y))))))
	(setq i (1+ i))))))

(defun table--untabify-line (&optional from)
  "Untabify current line.
Unlike save-excursion this guarantees preserving the cursor location
even when the point is on a tab character which is to be removed.
Optional FROM narrows the subject operation from this point to the end
of line."
  (let ((current-coordinate (table--get-coordinate)))
    (table--untabify (or from (progn (beginning-of-line) (point)))
		     (progn (end-of-line) (point)))
    (table--goto-coordinate current-coordinate)))

(defun table--untabify (beg end)
  "Wrapper to raw untabify."
  (untabify beg end)
  (if (featurep 'xemacs)
      ;; Cancel strange behavior of xemacs
      (message "")))

(defun table--multiply-string (string multiplier)
  "Multiply string and return it."
  (let ((ret-str ""))
    (while (> multiplier 0)
      (setq ret-str (concat ret-str string))
      (setq multiplier (1- multiplier)))
    ret-str))

(defun table--line-column-position (line column)
  "Return the location of LINE forward at COLUMN."
  (save-excursion
    (forward-line line)
    (move-to-column column)
    (point)))

(defun table--row-column-insertion-point-p (&optional columnp)
  "Return non-nil if it makes sense to insert a row or a column at point."
  (and (not buffer-read-only)
       (or (get-text-property (point) 'table-cell)
	   (let ((column (current-column)))
	     (if columnp
		 (or (text-property-any (line-beginning-position 0)
					(table--line-column-position -1 column)
					'table-cell t)
		     (text-property-any (line-beginning-position) (point) 'table-cell t)
		     (text-property-any (line-beginning-position 2)
					(table--line-column-position 1 column)
					'table-cell t))
	       (text-property-any (table--line-column-position -2 column)
				  (table--line-column-position -2 (+ 2 column))
				  'table-cell t))))))

(defun table--find-row-column (&optional columnp no-error)
  "Search table and return a cell coordinate list of row or column."
  (let ((current-coordinate (table--get-coordinate)))
    (catch 'end
      (catch 'error
	(let ((coord (table--get-coordinate)))
	  (while
	      (progn
		(if columnp (setcar coord (1- (car coord)))
		  (setcdr coord (1- (cdr coord))))
		(>= (if columnp (car coord) (cdr coord)) 0))
	    (while (progn
		     (table--goto-coordinate coord 'no-extension 'no-tab-expansion)
		     (not (looking-at (format "[%s%c%c]"
					      table-cell-horizontal-chars
					      table-cell-vertical-char
					      table-cell-intersection-char))))
	      (if columnp (setcar coord (1- (car coord)))
		(setcdr coord (1- (cdr coord))))
	      (if (< (if columnp (car coord) (cdr coord)) 0)
		  (throw 'error nil)))
	    (if (table--probe-cell)
		(throw 'end (table--cell-list-to-coord-list (if columnp
								(table--vertical-cell-list t nil 'left)
							      (table--horizontal-cell-list t nil 'top))))
	      (table--goto-coordinate (table--offset-coordinate coord (if columnp '(0 . 1) '(1 . 0)))
				      'no-extension 'no-tab-expansion)
	      (if (table--probe-cell)
		  (throw 'end (table--cell-list-to-coord-list (if columnp
								  (table--vertical-cell-list t nil 'left)
								(table--horizontal-cell-list t nil 'top)))))))))
      (table--goto-coordinate current-coordinate)
      (if no-error nil
	(error "Table not found")))))

(defun table--min-coord-list (coord-list)
  "Return minimum cell dimension of COORD-LIST.
COORD-LIST is a list of coordinate pairs (lu-coord . rb-coord), where
each pair in the list represents a cell.  lu-coord is the left upper
coordinate of a cell and rb-coord is the right bottom coordinate of a
cell.  A coordinate is a pair of x and y axis coordinate values.  The
return value is a cons cell (min-w . min-h), where min-w and min-h are
respectively the minimum width and the minimum height of all the cells
in the list."
  (if (null coord-list) nil
    (let ((min-width 134217727)
	  (min-height 134217727))
      (while coord-list
	(let* ((coord (prog1 (car coord-list) (setq coord-list (cdr coord-list))))
	       (width (- (cadr coord) (caar coord)))
	       (height (1+ (- (cddr coord) (cdar coord)))))
	  (if (< width min-width) (setq min-width width))
	  (if (< height min-height) (setq min-height height))))
      (cons min-width min-height))))

(defun table--cell-can-split-horizontally-p ()
  "Test if a cell can split at current location horizontally."
  (and (not buffer-read-only)
       (let ((point-x (car (table--get-coordinate))))
	 (table-recognize-cell 'force)
	 (and (> point-x (car table-cell-info-lu-coordinate))
	      (<= point-x (1- (car table-cell-info-rb-coordinate)))))))

(defun table--cell-can-split-vertically-p ()
  "Test if a cell can split at current location vertically."
  (and (not buffer-read-only)
       (let ((point-y (cdr (table--get-coordinate))))
	 (table-recognize-cell 'force)
	 (and (> point-y (cdr table-cell-info-lu-coordinate))
	      (<= point-y (cdr table-cell-info-rb-coordinate))))))

(defun table--cell-can-span-p (direction)
  "Test if the current cell can span to DIRECTION."
  (table-recognize-cell 'force)
  (and (not buffer-read-only)
       (table--probe-cell)
       ;; get two adjacent cells from each corner
       (let ((cell (save-excursion
		     (and
		      (table--goto-coordinate
		       (cons (cond ((eq direction 'right) (1+ (car table-cell-info-rb-coordinate)))
				   ((eq direction 'left)  (1- (car table-cell-info-lu-coordinate)))
				   (t (car table-cell-info-lu-coordinate)))
			     (cond ((eq direction 'above) (- (cdr table-cell-info-lu-coordinate) 2))
				   ((eq direction 'below) (+ (cdr table-cell-info-rb-coordinate) 2))
				   (t (cdr table-cell-info-lu-coordinate)))) 'no-extension)
		      (table--probe-cell))))
	     (cell2 (save-excursion
		      (and
		       (table--goto-coordinate
			(cons (cond ((eq direction 'right) (1+ (car table-cell-info-rb-coordinate)))
				    ((eq direction 'left)  (1- (car table-cell-info-lu-coordinate)))
				    (t (car table-cell-info-rb-coordinate)))
			      (cond ((eq direction 'above) (- (cdr table-cell-info-lu-coordinate) 2))
				    ((eq direction 'below) (+ (cdr table-cell-info-rb-coordinate) 2))
				    (t (cdr table-cell-info-rb-coordinate)))) 'no-extension)
		       (table--probe-cell)))))
	 ;; make sure the two cells exist, and they are identical, that cell's size matches the current one
	 (and cell
	      (equal cell cell2)
	      (if (or (eq direction 'right) (eq direction 'left))
		  (and (= (cdr (table--get-coordinate (car cell)))
			  (cdr table-cell-info-lu-coordinate))
		       (= (cdr (table--get-coordinate (cdr cell)))
			  (cdr table-cell-info-rb-coordinate)))
		(and (= (car (table--get-coordinate (car cell)))
			(car table-cell-info-lu-coordinate))
		     (= (car (table--get-coordinate (cdr cell)))
			(car table-cell-info-rb-coordinate))))))))

(defun table--cell-insert-char (char &optional overwrite)
  "Insert CHAR inside a table cell."
  (let ((delete-selection-p (and (boundp 'delete-selection-mode)
				 delete-selection-mode
				 transient-mark-mode mark-active
				 (not buffer-read-only)))
	(mark-coordinate (table--transcoord-table-to-cache (table--get-coordinate (mark t)))))
    (table-with-cache-buffer
      (and delete-selection-p
	   (>= (car mark-coordinate) 0)
	   (<= (car mark-coordinate) table-cell-info-width)
	   (>= (cdr mark-coordinate) 0)
	   (<= (cdr mark-coordinate) table-cell-info-height)
	   (save-excursion
	     (delete-region (point) (table--goto-coordinate mark-coordinate))))
      (if overwrite
	  (let ((coordinate (table--get-coordinate)))
	    (setq table-inhibit-auto-fill-paragraph t)
	    (if (>= (car coordinate) table-cell-info-width)
		(if (>= (cdr coordinate) (1- table-cell-info-height))
		    (insert "\n" char)
		  (forward-line 1)
		  (insert char)
		  (unless (eolp)
		    (delete-char 1)))
	      (insert char)
	      (unless (eolp)
		(delete-char 1))))
	(if (not (eq char ?\s))
	    (if char (insert char))
	  (if (not (looking-at "\\s *$"))
	      (if (and table-fixed-width-mode
		       (> (point) 2)
		       (save-excursion
			 (forward-char -2)
			 (looking-at (concat "\\("
					     (regexp-quote (char-to-string table-word-continuation-char))
					     "\\)\n"))))
		  (save-excursion
		    (replace-match " " nil nil nil 1))
		(insert char))
	    (let ((coordinate (table--get-coordinate)))
	      (if (< (car coordinate) table-cell-info-width)
		  (move-to-column (1+ (car coordinate)) t)
		(insert (make-string (forward-line 1) ?\n))
		(unless (bolp) (insert ?\n))))
	    (setq table-inhibit-auto-fill-paragraph t))
	  (save-excursion
	    (let ((o-point (point)))
	      (if (and (bolp)
		       (or (progn
			     (forward-paragraph)
			     (forward-paragraph -1)
			     (= o-point (point)))
			   (progn
			     (goto-char o-point)
			     (forward-line)
			     (setq o-point (point))
			     (forward-paragraph)
			     (forward-paragraph -1)
			     (= o-point (point)))))
		  (insert ?\n)))))))))

(defun table--finish-delayed-tasks ()
  "Finish all outstanding delayed tasks."
  (if table-update-timer
      (table--update-cell 'now))
  (if table-widen-timer
      (table--update-cell-widened 'now))
  (if table-heighten-timer
      (table--update-cell-heightened 'now)))

(defmacro table--log (&rest body)
  "Debug logging macro."
  `(with-current-buffer (get-buffer-create "log")
     (goto-char (point-min))
     (let ((standard-output (current-buffer)))
       ,@body)))

(defun table--measure-max-width (&optional unlimited)
  "Return maximum width of current buffer.
Normally the current buffer is expected to be already the cache
buffer.  The width excludes following spaces at the end of each line.
Unless UNLIMITED is non-nil minimum return value is 1."
  (save-excursion
    (let ((width 0))
      (goto-char (point-min))
      (while
	  (progn
	    ;; do not count the following white spaces
	    (re-search-forward "\\s *$")
	    (goto-char (match-beginning 0))
	    (if (> (current-column) width)
		(setq width (current-column)))
	    (forward-line)
	    (not (eobp))))
      (if unlimited width
	(max 1 width)))))

(defun table--cell-to-coord (cell)
  "Create a cell coordinate pair from cell location pair."
  (if cell
      (cons (table--get-coordinate (car cell))
	    (table--get-coordinate (cdr cell)))
    nil))

(defun table--cell-list-to-coord-list (cell-list)
  "Create and return a coordinate list that corresponds to CELL-LIST.
CELL-LIST is a list of location pairs (lu . rb), where each pair
represents a cell in the list.  lu is the left upper location and rb
is the right bottom location of a cell.  The return value is a list of
coordinate pairs (lu-coord . rb-coord), where lu-coord is the left
upper coordinate and rb-coord is the right bottom coordinate of a
cell."
  (let ((coord-list))
    (while cell-list
      (let ((cell (prog1 (car cell-list) (setq cell-list (cdr cell-list)))))
	(setq coord-list
	      (cons (table--cell-to-coord cell) coord-list))))
    (nreverse coord-list)))

(defun table--test-cell-list (&optional horizontal reverse first-only pivot)
  "For testing `table--vertical-cell-list' and `table--horizontal-cell-list'."
  (let* ((current-coordinate (table--get-coordinate))
	 (cell-list (if horizontal
			(table--horizontal-cell-list reverse first-only pivot)
		      (table--vertical-cell-list reverse first-only pivot)))
	 (count 0))
    (while cell-list
      (let* ((cell (if first-only (prog1 cell-list (setq cell-list nil))
		     (prog1 (car cell-list) (setq cell-list (cdr cell-list)))))
	     (dig1-str (format "%1d" (prog1 (% count 10) (setq count (1+ count))))))
	(goto-char (car cell))
	(table-with-cache-buffer
          (while (re-search-forward "." nil t)
            (replace-match dig1-str nil nil))
	  (setq table-inhibit-auto-fill-paragraph t))
	(table--finish-delayed-tasks)))
    (table--goto-coordinate current-coordinate)))

(defun table--vertical-cell-list (&optional top-to-bottom first-only pivot internal-dir internal-list internal-px)
  "Return a vertical cell list from the table.
The return value represents a list of cells including the current cell
that align vertically.  Each element of the list is a cons cell (lu
. rb) where lu is the cell's left upper location and rb is the cell's
right bottom location.  The cell order in the list is from bottom to
top of the table.  If optional argument TOP-TO-BOTTOM is non-nil the
order is reversed as from top to bottom of the table.  If optional
argument FIRST-ONLY is non-nil the return value is not a list of cells
but a single cons cell that is the first cell of the list, if the list
had been created.  If optional argument PIVOT is a symbol `left' the
vertical cell search is aligned with the left edge of the current
cell, otherwise aligned with the right edge of the current cell.  The
arguments INTERNAL-DIR, INTERNAL-LIST and INTERNAL-PX are internal use
only and must not be specified."
  (save-excursion
    (let* ((cell (table--probe-cell))
	   (lu-coordinate (table--get-coordinate (car cell)))
	   (rb-coordinate (table--get-coordinate (cdr cell)))
	   (px (or internal-px (car (if (eq pivot 'left) lu-coordinate rb-coordinate))))
	   (ty (- (cdr lu-coordinate) 2))
	   (by (+ (cdr rb-coordinate) 2)))
      ;; in case of finding the first cell, get the last adding item on the list
      (if (and (null internal-dir) first-only) (setq top-to-bottom (null top-to-bottom)))
      ;; travel up and process as recursion traces back (reverse order)
      (and cell
	   (or (eq internal-dir 'up) (null internal-dir))
	   (table--goto-coordinate (cons px (if top-to-bottom by ty)) 'no-extension 'no-tab-expansion)
	   (setq internal-list (table--vertical-cell-list top-to-bottom first-only nil 'up nil px)))
      ;; return the last cell or add this cell to the list
      (if first-only (or internal-list cell)
	(setq internal-list (if cell (cons cell internal-list) internal-list))
	;; travel down and process as entering each recursion (forward order)
	(and cell
	     (or (eq internal-dir 'down) (null internal-dir))
	     (table--goto-coordinate (cons px (if top-to-bottom ty by)) 'no-extension 'no-tab-expansion)
	     (setq internal-list (table--vertical-cell-list top-to-bottom nil nil 'down internal-list px)))
	;; return the result
	internal-list))))

(defun table--horizontal-cell-list (&optional left-to-right first-only pivot internal-dir internal-list internal-py)
  "Return a horizontal cell list from the table.
The return value represents a list of cells including the current cell
that align horizontally.  Each element of the list is a cons cells (lu
. rb) where lu is the cell's left upper location and rb is the cell's
right bottom location.  The cell order in the list is from right to
left of the table.  If optional argument LEFT-TO-RIGHT is non-nil the
order is reversed as from left to right of the table.  If optional
argument FIRST-ONLY is non-nil the return value is not a list of cells
but a single cons cell that is the first cell of the list, if the
list had been created.  If optional argument PIVOT is a symbol `top'
the horizontal cell search is aligned with the top edge of the current
cell, otherwise aligned with the bottom edge of the current cell.  The
arguments INTERNAL-DIR, INTERNAL-LIST and INTERNAL-PY are internal use
only and must not be specified."
  (save-excursion
    (let* ((cell (table--probe-cell))
	   (lu-coordinate (table--get-coordinate (car cell)))
	   (rb-coordinate (table--get-coordinate (cdr cell)))
	   (py (or internal-py (if (eq pivot 'top) (cdr lu-coordinate) (1+ (cdr rb-coordinate)))))
	   (lx (1- (car lu-coordinate)))
	   (rx (1+ (car rb-coordinate))))
      ;; in case of finding the first cell, get the last adding item on the list
      (if (and (null internal-dir) first-only) (setq left-to-right (null left-to-right)))
      ;; travel left and process as recursion traces back (reverse order)
      (and cell
	   (or (eq internal-dir 'left) (null internal-dir))
	   (table--goto-coordinate (cons (if left-to-right rx lx) py) 'no-extension 'no-tab-expansion)
	   (setq internal-list (table--horizontal-cell-list left-to-right first-only nil 'left nil py)))
      ;; return the last cell or add this cell to the list
      (if first-only (or internal-list cell)
	(setq internal-list (if cell (cons cell internal-list) internal-list))
	;; travel right and process as entering each recursion (forward order)
	(and cell
	     (or (eq internal-dir 'right) (null internal-dir))
	     (table--goto-coordinate (cons (if left-to-right lx rx) py) 'no-extension 'no-tab-expansion)
	     (setq internal-list (table--horizontal-cell-list left-to-right nil nil 'right internal-list py)))
	;; return the result
	internal-list))))

(defun table--point-in-cell-p (&optional location)
  "Return t when point is in a valid table cell in the current buffer.
When optional LOCATION is provided the test is performed at that location."
  (and (table--at-cell-p (or location (point)))
       (if location
	   (save-excursion
	     (goto-char location)
	     (table--probe-cell))
	 (table--probe-cell))))

(defun table--region-in-cell-p (beg end)
  "Return t when location BEG and END are in a valid table cell in the current buffer."
  (and (table--at-cell-p (min beg end))
       (save-excursion
	 (let ((cell-beg (progn (goto-char beg) (table--probe-cell))))
	   (and cell-beg
		(equal cell-beg (progn (goto-char end) (table--probe-cell))))))))

(defun table--at-cell-p (position &optional object at-column)
  "Returns non-nil if POSITION has table-cell property in OBJECT.
OBJECT is optional and defaults to the current buffer.
If POSITION is at the end of OBJECT, the value is nil."
  (if (and at-column (stringp object))
      (setq position (table--str-index-at-column object position)))
  (get-text-property position 'table-cell object))

(defun table--probe-cell-left-up ()
  "Probe left up corner pattern of a cell.
If it finds a valid corner returns a position otherwise returns nil.
The position is the location before the first cell character.
Focus only on the corner pattern.  Further cell validity check is required."
  (save-excursion
    (let ((vertical-str (regexp-quote (char-to-string table-cell-vertical-char)))
	  (intersection-str (regexp-quote (char-to-string table-cell-intersection-char)))
	  (v-border (format "[%c%c]" table-cell-vertical-char table-cell-intersection-char))
	  (h-border (format "[%s%c]" table-cell-horizontal-chars table-cell-intersection-char))
	  (limit (line-beginning-position)))
      (catch 'end
	(while t
	  (catch 'retry-horizontal
	    (if (not (search-backward-regexp v-border limit t))
		(throw 'end nil))
	    (save-excursion
	      (let ((column (current-column)))
		(while t
		  (catch 'retry-vertical
		    (if (zerop (forward-line -1)) nil (throw 'end nil))
		    (move-to-column column)
		    (while (and (looking-at vertical-str)
				(= column (current-column)))
		      (if (zerop (forward-line -1)) nil (throw 'end nil))
		      (move-to-column column))
		    (cond
		     ((/= column (current-column))
		      (throw 'end nil))
		     ((looking-at (concat intersection-str h-border))
		      (forward-line 1)
		      (move-to-column column)
		      (forward-char 1)
		      (throw 'end (point)))
		     ((looking-at intersection-str)
		      (throw 'retry-vertical nil))
		     (t (throw 'retry-horizontal nil)))))))))))))

(defun table--probe-cell-right-bottom ()
  "Probe right bottom corner pattern of a cell.
If it finds a valid corner returns a position otherwise returns nil.
The position is the location after the last cell character.
Focus only on the corner pattern.  Further cell validity check is required."
  (save-excursion
    (let ((vertical-str (regexp-quote (char-to-string table-cell-vertical-char)))
	  (intersection-str (regexp-quote (char-to-string table-cell-intersection-char)))
	  (v-border (format "[%c%c]" table-cell-vertical-char table-cell-intersection-char))
	  (h-border (format "[%s%c]" table-cell-horizontal-chars table-cell-intersection-char))
	  (limit (line-end-position)))
      (catch 'end
	(while t
	  (catch 'retry-horizontal
	    (if (not (search-forward-regexp v-border limit t))
		(throw 'end nil))
	    (save-excursion
	      (forward-char -1)
	      (let ((column (current-column)))
		(while t
		  (catch 'retry-vertical
		    (while (and (looking-at vertical-str)
				(= column (current-column)))
		      (if (and (zerop (forward-line 1)) (zerop (current-column))) nil (throw 'end nil))
		      (move-to-column column))
		    (cond
		     ((/= column (current-column))
		      (throw 'end nil))
		     ((save-excursion (forward-char -1) (looking-at (concat h-border intersection-str)))
		      (save-excursion
			(and (zerop (forward-line -1))
			     (move-to-column column)
			     (looking-at v-border)
			     (throw 'end (point))))
		      (forward-char 1)
		      (throw 'retry-horizontal nil))
		     ((looking-at intersection-str)
		      (if (and (zerop (forward-line 1)) (zerop (current-column))) nil (throw 'end nil))
		      (move-to-column column)
		      (throw 'retry-vertical nil))
		     (t (throw 'retry-horizontal nil)))))))))))))

(defun table--editable-cell-p (&optional abort-on-error)
  (and (not buffer-read-only)
       (get-text-property (point) 'table-cell)))

(defun table--probe-cell (&optional abort-on-error)
  "Probes a table cell around the point.
Searches for the left upper corner and the right bottom corner of a table
cell which contains the current point location.

The result is a cons cell (left-upper . right-bottom) where
the left-upper is the position before the cell's left upper corner character,
the right-bottom is the position after the cell's right bottom corner character.

When it fails to find either one of the cell corners it returns nil or
signals error if the optional ABORT-ON-ERROR is non-nil."
  (let (lu rb
	(border (format "^[%s%c%c]+$"
			table-cell-horizontal-chars
			table-cell-vertical-char
			table-cell-intersection-char)))
    (if (and (condition-case nil
		 (progn
		   (and (setq lu (table--probe-cell-left-up))
			(setq rb (table--probe-cell-right-bottom))))
	       (error nil))
	     (< lu rb)
	     (let ((lu-coordinate (table--get-coordinate lu))
		   (rb-coordinate (table--get-coordinate rb)))
	       ;; test for valid upper and lower borders
	       (and (string-match
		     border
		     (buffer-substring
		      (save-excursion
			(table--goto-coordinate
			 (cons (1- (car lu-coordinate))
			       (1- (cdr lu-coordinate)))))
		      (save-excursion
			(table--goto-coordinate
			 (cons (1+ (car rb-coordinate))
			       (1- (cdr lu-coordinate)))))))
		    (string-match
		     border
		     (buffer-substring
		      (save-excursion
			(table--goto-coordinate
			 (cons (1- (car lu-coordinate))
			       (1+ (cdr rb-coordinate)))))
		      (save-excursion
			(table--goto-coordinate
			 (cons (1+ (car rb-coordinate))
			       (1+ (cdr rb-coordinate))))))))))
	(cons lu rb)
      (if abort-on-error
	  (error "Table cell not found")
	nil))))

(defun table--insert-rectangle (rectangle)
  "Insert text of RECTANGLE with upper left corner at point.
Same as insert-rectangle except that mark operation is eliminated."
  (let ((lines rectangle)
	(insertcolumn (current-column))
	(first t))
    (while lines
      (or first
	  (progn
	    (forward-line 1)
	    (or (bolp) (insert ?\n))
	    (move-to-column insertcolumn t)))
      (setq first nil)
      (insert (car lines))
      (setq lines (cdr lines)))))

(defun table--put-cell-property (cell)
  "Put standard text properties to the CELL.
The CELL is a cons cell (left-upper . right-bottom) where the
left-upper is the position before the cell's left upper corner
character, the right-bottom is the position after the cell's right
bottom corner character."
  (let ((lu (table--get-coordinate (car cell)))
	(rb (table--get-coordinate (cdr cell))))
    (save-excursion
      (while (<= (cdr lu) (cdr rb))
	(let ((beg (table--goto-coordinate lu 'no-extension))
	      (end (table--goto-coordinate (cons (car rb) (cdr lu)))))
	  (table--put-cell-line-property beg end))
	(setcdr lu (1+ (cdr lu))))
      (table--put-cell-justify-property cell table-cell-info-justify)
      (table--put-cell-valign-property cell table-cell-info-valign))))

(defun table--put-cell-line-property (beg end &optional object)
  "Put standard text properties to a line of a cell.
BEG is the beginning of the line that is the location between left
cell border character and the first content character.  END is the end
of the line that is the location between the last content character
and the right cell border character."
  (table--put-cell-content-property beg end object)
  (table--put-cell-keymap-property end (1+ end) object)
  (table--put-cell-indicator-property end (1+ end) object)
  (table--put-cell-rear-nonsticky end (1+ end) object))

(defun table--put-cell-content-property (beg end &optional object)
  "Put cell content text properties."
  (table--put-cell-keymap-property beg end object)
  (table--put-cell-indicator-property beg end object)
  (table--put-cell-face-property beg end object)
  (table--put-cell-point-entered/left-property beg end object))

(defun table--put-cell-indicator-property (beg end &optional object)
  "Put cell property which indicates that the location is within a table cell."
  (put-text-property beg end 'table-cell t object)
  (put-text-property beg end 'yank-handler table-yank-handler object))

(defun table--put-cell-face-property (beg end &optional object)
  "Put cell face property."
  (put-text-property beg end 'face 'table-cell object))

(defun table--put-cell-keymap-property (beg end &optional object)
  "Put cell keymap property."
  (put-text-property beg end 'keymap 'table-cell-map object))

(defun table--put-cell-rear-nonsticky (beg end &optional object)
  "Put rear-nonsticky property."
  (put-text-property beg end 'rear-nonsticky t object))

(defun table--put-cell-point-entered/left-property (beg end &optional object)
  "Put point-entered/left property."
  (put-text-property beg end 'point-entered 'table--point-entered-cell-function object)
  (put-text-property beg end 'point-left 'table--point-left-cell-function object))

(defun table--remove-cell-properties (beg end &optional object)
  "Remove all cell properties.
If OBJECT is non-nil cell properties are removed from the OBJECT
instead of the current buffer and returns the OBJECT."
  (while (< beg end)
    (let ((next (next-single-property-change beg 'table-cell object end)))
      (if (get-text-property beg 'table-cell object)
	  (remove-text-properties beg next
				  (list
				   'table-cell nil
				   'table-justify nil
				   'table-valign nil
				   'face nil
				   'rear-nonsticky nil
				   'point-entered nil
				   'point-left nil
				   'keymap nil)
				  object))
      (setq beg next)))
  object)

(defun table--update-cell-face ()
  "Update cell face according to the current mode."
  (if (featurep 'xemacs)
      (set-face-property 'table-cell 'underline table-fixed-width-mode)
    (set-face-inverse-video-p 'table-cell table-fixed-width-mode)))

(table--update-cell-face)

(defun table--get-property (cell property)
  "Get CELL's PROPERTY."
  (or (get-text-property (car cell) property)
      (get-text-property (1- (cdr cell)) property)))

(defun table--get-cell-justify-property (cell)
  "Get cell's justify property."
  (table--get-property cell 'table-justify))

(defun table--get-cell-valign-property (cell)
  "Get cell's vertical alignment property."
  (table--get-property cell 'table-valign))

(defun table--put-property  (cell property value)
  "Put CELL's PROPERTY the VALUE."
  (let ((beg (car cell))
	(end (cdr cell)))
    (put-text-property beg (1+ beg) property value)
    (put-text-property (1- end) end property value)))

(defun table--put-cell-justify-property (cell justify)
  "Put cell's justify property."
  (table--put-property cell 'table-justify justify))

(defun table--put-cell-valign-property (cell valign)
  "Put cell's vertical alignment property."
  (table--put-property cell 'table-valign valign))

(defun table--point-entered-cell-function (&optional old-point new-point)
  "Point has entered a cell.
Refresh the menu bar."
  ;; Avoid calling point-motion-hooks recursively.
  (let ((inhibit-point-motion-hooks t))
    (unless table-cell-entered-state
      (setq table-cell-entered-state t)
      (setq table-mode-indicator t)
      (force-mode-line-update)
      (table--warn-incompatibility)
      (run-hooks 'table-point-entered-cell-hook))))

(defun table--point-left-cell-function (&optional old-point new-point)
  "Point has left a cell.
Refresh the menu bar."
  ;; Avoid calling point-motion-hooks recursively.
  (let ((inhibit-point-motion-hooks t))
    (when table-cell-entered-state
      (setq table-cell-entered-state nil)
      (setq table-mode-indicator nil)
      (force-mode-line-update)
      (run-hooks 'table-point-left-cell-hook))))

(defun table--warn-incompatibility ()
  "If called from interactive operation warn the know incompatibilities.
This feature is disabled when `table-disable-incompatibility-warning'
is non-nil.  The warning is done only once per session for each item."
  (unless (and table-disable-incompatibility-warning
	       (not (called-interactively-p 'interactive)))
    (cond ((and (featurep 'xemacs)
		(not (get 'table-disable-incompatibility-warning 'xemacs)))
	   (put 'table-disable-incompatibility-warning 'xemacs t)
	   (display-warning 'table
	    "
*** Warning ***

Table package mostly works fine under XEmacs, however, due to the
peculiar implementation of text property under XEmacs, cell splitting
and any undo operation of table exhibit some known strange problems,
such that a border characters dissolve into adjacent cells.  Please be
aware of this.

"
	    :warning))
	  ((and (boundp 'flyspell-mode)
		flyspell-mode
		(not (get 'table-disable-incompatibility-warning 'flyspell)))
	   (put 'table-disable-incompatibility-warning 'flyspell t)
	   (display-warning 'table
	    "
*** Warning ***

Flyspell minor mode is known to be incompatible with this table
package.  The flyspell version 1.5d at URL `http://kaolin.unice.fr/~serrano'
works better than the previous versions however not fully compatible.

"
	    :warning))
	  )))

(defun table--cell-blank-str (&optional n)
  "Return blank table cell string of length N."
  (let ((str (make-string (or n 1) ?\s)))
    (table--put-cell-content-property 0 (length str) str)
    str))

(defun table--remove-eol-spaces (beg end &optional bol force)
  "Remove spaces at the end of each line in the BEG END region of the current buffer.
When optional BOL is non-nil spaces at the beginning of line are
removed.  When optional FORCE is non-nil removal operation is enforced
even when point is within the removal area."
  (if (> beg end)
      (let ((tmp beg))
	(setq beg end)
	(setq end tmp)))
  (let ((saved-point (point-marker))
	(end-marker (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (if bol (re-search-forward "^\\( +\\)" end-marker t)
	       (re-search-forward "\\( +\\)$" end-marker t))
	;; avoid removal that causes the saved point to lose its location.
	(if (and (null bol)
		 (<= (match-beginning 1) saved-point)
		 (<= saved-point (match-end 1))
		 (not force))
	    (delete-region saved-point (match-end 1))
	  (delete-region (match-beginning 1) (match-end 1)))))
    (set-marker saved-point nil)
    (set-marker end-marker nil)))

(defun table--fill-region (beg end &optional col justify)
  "Fill paragraphs in table cell cache.
Current buffer must already be set to the cache buffer."
  (let ((fill-column (or col table-cell-info-width))
	(fill-prefix nil)
	(enable-kinsoku nil)
	(adaptive-fill-mode nil)
	(marker-beg (copy-marker beg))
	(marker-end (copy-marker end))
	(marker-point (point-marker)))
    (setq justify (or justify table-cell-info-justify))
    (and justify
	 (not (eq justify 'left))
	 (not (featurep 'xemacs))
	 (set-marker-insertion-type marker-point t))
    (table--remove-eol-spaces (point-min) (point-max))
    (if table-fixed-width-mode
	(table--fill-region-strictly marker-beg marker-end)
      (let ((paragraph-start table-paragraph-start))
	(fill-region marker-beg marker-end justify nil t)))
    (goto-char marker-point)
    (set-marker marker-beg nil)
    (set-marker marker-end nil)
    (set-marker marker-point nil)))

(defun table--fill-region-strictly (beg end)
  "Fill region strictly so that no line exceeds fill-column.
When a word exceeds fill-column the word is chopped into pieces.  The
chopped location is indicated with table-word-continuation-char."
  (or (and (markerp beg) (markerp end))
      (error "markerp"))
  (if (< fill-column 2)
      (setq fill-column 2))
  ;; first remove all continuation characters.
  (goto-char beg)
  (while (re-search-forward (concat
			     (format "[^%c ]\\(" table-word-continuation-char)
			     (regexp-quote (char-to-string table-word-continuation-char))
			     "\\s +\\)")
			    end t)
    (delete-region (match-beginning 1) (match-end 1)))
  ;; then fill as normal
  (let ((paragraph-start table-paragraph-start))
    (fill-region beg end nil nil t))
  ;; now fix up
  (goto-char beg)
  (while (let ((col (move-to-column fill-column t)))
	   (cond
	    ((and (<= col fill-column)
		  (looking-at " *$"))
	     (delete-region (match-beginning 0) (match-end 0))
	     (and (zerop (forward-line 1))
		  (< (point) end)))
	    (t (forward-char -1)
	       (insert-before-markers (if (equal (char-before) ?\s) ?\s table-word-continuation-char)
				      "\n")
	       t)))))

(defun table--goto-coordinate (coordinate &optional no-extension no-tab-expansion)
  "Move point to the given COORDINATE and return the location.
When optional NO-EXTENSION is non-nil and the specified coordinate is
not reachable returns nil otherwise the blanks are added if necessary
to achieve the goal coordinate and returns the goal point.  It
intentionally does not preserve the original point in case it fails
achieving the goal.  When optional NO-TAB-EXPANSION is non-nil and the
goad happens to be in a tab character the tab is not expanded but the
goal ends at the beginning of tab."
  (if (or (null coordinate)
	  (< (car coordinate) 0)
	  (< (cdr coordinate) 0)) nil
    (goto-char (point-min))
    (let ((x (car coordinate))
	  (more-lines (forward-line (cdr coordinate))))
      (catch 'exit
	(if (zerop (current-column)) nil
	  (if no-extension
	      (progn
		(move-to-column x)
		(throw 'exit nil))
	    (setq more-lines (1+ more-lines))))
	(if (zerop more-lines) nil
	  (newline more-lines))
	(if no-extension
	    (if (/= (move-to-column x) x)
		(if (> (move-to-column x) x)
		    (if no-tab-expansion
			(progn
			  (while (> (move-to-column x) x)
			    (setq x (1- x)))
			  (point))
		      (throw 'exit (move-to-column x t)))
		  (throw 'exit nil)))
	  (move-to-column x t))
	(point)))))

(defun table--copy-coordinate (coord)
  "Copy coordinate in a new cons cell."
  (cons (car coord) (cdr coord)))

(defun table--get-coordinate (&optional where)
  "Return the coordinate of point in current buffer.
When optional WHERE is given it returns the coordinate of that
location instead of point in the current buffer.  It does not move the
point"
  (save-excursion
    (if where (goto-char where))
    (cons (current-column)
	  (table--current-line))))

(defun table--current-line (&optional location)
  "Return zero based line count of current line or if non-nil LOCATION line."
  (save-excursion
    (if location (goto-char location))
    (beginning-of-line)
    (count-lines (point-min) (point))))

(defun table--transcoord-table-to-cache (&optional coordinate)
  "Transpose COORDINATE from table coordinate system to cache coordinate system.
When COORDINATE is omitted or nil the point in current buffer is assumed in place."
  (table--offset-coordinate
   (or coordinate (table--get-coordinate))
   table-cell-info-lu-coordinate
   'negative))

(defun table--transcoord-cache-to-table (&optional coordinate)
  "Transpose COORDINATE from cache coordinate system to table coordinate system.
When COORDINATE is omitted or nil the point in current buffer is assumed in place."
  (table--offset-coordinate
   (or coordinate (table--get-coordinate))
   table-cell-info-lu-coordinate))

(defun table--offset-coordinate (coordinate offset &optional negative)
  "Return the offset COORDINATE by OFFSET.
When optional NEGATIVE is non-nil offsetting direction is negative."
  (cons (if negative (- (car coordinate) (car offset))
	  (+ (car coordinate) (car offset)))
	(if negative (- (cdr coordinate) (cdr offset))
	  (+ (cdr coordinate) (cdr offset)))))

(defun table--char-in-str-at-column (str column)
  "Return the character in STR at COLUMN location.
When COLUMN is out of range it returns null character."
  (let ((idx (table--str-index-at-column str column)))
    (if idx (aref str idx)
      ?\0)))

(defun table--str-index-at-column (str column)
  "Return the character index in STR that corresponds to COLUMN location.
It returns COLUMN unless STR contains some wide characters."
  (let ((col 0)
	(idx 0)
	(len (length str)))
    (while (and (< col column) (< idx len))
      (setq col (+ col (char-width (aref str idx))))
      (setq idx (1+ idx)))
    (if (< idx len)
	idx
      nil)))

(defun table--set-timer (seconds func args)
  "Generic wrapper for setting up a timer."
  (if (featurep 'xemacs)
      ;; the picky xemacs refuses to accept zero
      (add-timeout (if (zerop seconds) 0.01 seconds) func args nil)
    ;;(run-at-time seconds nil func args)))
    ;; somehow run-at-time causes strange problem under Emacs 20.7
    ;; this problem does not show up under Emacs 21.0.90
    (run-with-idle-timer seconds nil func args)))

(defun table--cancel-timer (timer)
  "Generic wrapper for canceling a timer."
  (if (featurep 'xemacs)
      (disable-timeout timer)
    (cancel-timer timer)))

(defun table--get-last-command ()
  "Generic wrapper for getting the real last command."
  (if (boundp 'real-last-command)
      real-last-command
    last-command))

(run-hooks 'table-load-hook)

(provide 'table)

;;; table.el ends here
