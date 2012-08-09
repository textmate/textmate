/* Definitions for interface to indent.c
   Copyright (C) 1985-1986, 2001-2012  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/* We introduce new member `tab_offset'.  We need it because of the
   existence of wide-column characters.  There is a case that the
   line-break occurs at a wide-column character and the number of
   columns of the line gets less than width.

   Example (where W_ stands for a wide-column character):
	     ----------
	     abcdefgh\\
	     W_
	     ----------

   To handle this case, we should not calculate the tab offset by
  	tab_offset += width;

   Instead, we must remember tab_offset of the line.

 */

struct position
  {
    EMACS_INT bufpos;
    EMACS_INT bytepos;
    EMACS_INT hpos;
    EMACS_INT vpos;
    EMACS_INT prevhpos;
    EMACS_INT contin;
    /* Number of characters we have already handled
       from the before and after strings at this position.  */
    EMACS_INT ovstring_chars_done;
    EMACS_INT tab_offset;
  };

struct position *compute_motion (EMACS_INT from, EMACS_INT fromvpos,
                                 EMACS_INT fromhpos, int did_motion,
                                 EMACS_INT to, EMACS_INT tovpos,
                                 EMACS_INT tohpos,
                                 EMACS_INT width, EMACS_INT hscroll,
                                 EMACS_INT tab_offset, struct window *);
struct position *vmotion (EMACS_INT from, EMACS_INT vtarget,
                          struct window *);
EMACS_INT skip_invisible (EMACS_INT pos, EMACS_INT *next_boundary_p,
                          EMACS_INT to, Lisp_Object window);

/* Value of point when current_column was called */
extern EMACS_INT last_known_column_point;

/* Functions for dealing with the column cache.  */

/* Return true if the display table DISPTAB specifies the same widths
   for characters as WIDTHTAB.  We use this to decide when to
   invalidate the buffer's column_cache.  */
int disptab_matches_widthtab (struct Lisp_Char_Table *disptab,
                              struct Lisp_Vector *widthtab);

/* Recompute BUF's width table, using the display table DISPTAB.  */
void recompute_width_table (struct buffer *buf,
                            struct Lisp_Char_Table *disptab);
