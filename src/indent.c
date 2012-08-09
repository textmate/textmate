/* Indentation functions.
   Copyright (C) 1985-1988, 1993-1995, 1998, 2000-2012
                 Free Software Foundation, Inc.

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

#include <config.h>
#include <stdio.h>
#include <setjmp.h>

#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "category.h"
#include "composite.h"
#include "indent.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "termchar.h"
#include "termopts.h"
#include "disptab.h"
#include "intervals.h"
#include "dispextern.h"
#include "region-cache.h"

#define CR 015

/* These three values memorize the current column to avoid recalculation.  */

/* Last value returned by current_column.
   Some things in set last_known_column_point to -1
   to mark the memorized value as invalid.  */

static EMACS_INT last_known_column;

/* Value of point when current_column was called.  */

EMACS_INT last_known_column_point;

/* Value of MODIFF when current_column was called.  */

static int last_known_column_modified;

static EMACS_INT current_column_1 (void);
static EMACS_INT position_indentation (ptrdiff_t);

/* Cache of beginning of line found by the last call of
   current_column. */

static EMACS_INT current_column_bol_cache;

/* Get the display table to use for the current buffer.  */

struct Lisp_Char_Table *
buffer_display_table (void)
{
  Lisp_Object thisbuf;

  thisbuf = BVAR (current_buffer, display_table);
  if (DISP_TABLE_P (thisbuf))
    return XCHAR_TABLE (thisbuf);
  if (DISP_TABLE_P (Vstandard_display_table))
    return XCHAR_TABLE (Vstandard_display_table);
  return 0;
}

/* Width run cache considerations.  */

/* Return the width of character C under display table DP.  */

static int
character_width (int c, struct Lisp_Char_Table *dp)
{
  Lisp_Object elt;

  /* These width computations were determined by examining the cases
     in display_text_line.  */

  /* Everything can be handled by the display table, if it's
     present and the element is right.  */
  if (dp && (elt = DISP_CHAR_VECTOR (dp, c), VECTORP (elt)))
    return ASIZE (elt);

  /* Some characters are special.  */
  if (c == '\n' || c == '\t' || c == '\015')
    return 0;

  /* Printing characters have width 1.  */
  else if (c >= 040 && c < 0177)
    return 1;

  /* Everybody else (control characters, metacharacters) has other
     widths.  We could return their actual widths here, but they
     depend on things like ctl_arrow and crud like that, and they're
     not very common at all.  So we'll just claim we don't know their
     widths.  */
  else
    return 0;
}

/* Return true if the display table DISPTAB specifies the same widths
   for characters as WIDTHTAB.  We use this to decide when to
   invalidate the buffer's width_run_cache.  */

int
disptab_matches_widthtab (struct Lisp_Char_Table *disptab, struct Lisp_Vector *widthtab)
{
  int i;

  if (widthtab->header.size != 256)
    abort ();

  for (i = 0; i < 256; i++)
    if (character_width (i, disptab)
        != XFASTINT (widthtab->contents[i]))
      return 0;

  return 1;
}

/* Recompute BUF's width table, using the display table DISPTAB.  */

void
recompute_width_table (struct buffer *buf, struct Lisp_Char_Table *disptab)
{
  int i;
  struct Lisp_Vector *widthtab;

  if (!VECTORP (BVAR (buf, width_table)))
    BVAR (buf, width_table) = Fmake_vector (make_number (256), make_number (0));
  widthtab = XVECTOR (BVAR (buf, width_table));
  if (widthtab->header.size != 256)
    abort ();

  for (i = 0; i < 256; i++)
    XSETFASTINT (widthtab->contents[i], character_width (i, disptab));
}

/* Allocate or free the width run cache, as requested by the current
   state of current_buffer's cache_long_line_scans variable.  */

static void
width_run_cache_on_off (void)
{
  if (NILP (BVAR (current_buffer, cache_long_line_scans))
      /* And, for the moment, this feature doesn't work on multibyte
         characters.  */
      || !NILP (BVAR (current_buffer, enable_multibyte_characters)))
    {
      /* It should be off.  */
      if (current_buffer->width_run_cache)
        {
          free_region_cache (current_buffer->width_run_cache);
          current_buffer->width_run_cache = 0;
          BVAR (current_buffer, width_table) = Qnil;
        }
    }
  else
    {
      /* It should be on.  */
      if (current_buffer->width_run_cache == 0)
        {
          current_buffer->width_run_cache = new_region_cache ();
          recompute_width_table (current_buffer, buffer_display_table ());
        }
    }
}


/* Skip some invisible characters starting from POS.
   This includes characters invisible because of text properties
   and characters invisible because of overlays.

   If position POS is followed by invisible characters,
   skip some of them and return the position after them.
   Otherwise return POS itself.

   Set *NEXT_BOUNDARY_P to the next position at which
   it will be necessary to call this function again.

   Don't scan past TO, and don't set *NEXT_BOUNDARY_P
   to a value greater than TO.

   If WINDOW is non-nil, and this buffer is displayed in WINDOW,
   take account of overlays that apply only in WINDOW.

   We don't necessarily skip all the invisible characters after POS
   because that could take a long time.  We skip a reasonable number
   which can be skipped quickly.  If there might be more invisible
   characters immediately following, then *NEXT_BOUNDARY_P
   will equal the return value.  */

EMACS_INT
skip_invisible (EMACS_INT pos, EMACS_INT *next_boundary_p, EMACS_INT to, Lisp_Object window)
{
  Lisp_Object prop, position, overlay_limit, proplimit;
  Lisp_Object buffer, tmp;
  EMACS_INT end;
  int inv_p;

  XSETFASTINT (position, pos);
  XSETBUFFER (buffer, current_buffer);

  /* Give faster response for overlay lookup near POS.  */
  recenter_overlay_lists (current_buffer, pos);

  /* We must not advance farther than the next overlay change.
     The overlay change might change the invisible property;
     or there might be overlay strings to be displayed there.  */
  overlay_limit = Fnext_overlay_change (position);
  /* As for text properties, this gives a lower bound
     for where the invisible text property could change.  */
  proplimit = Fnext_property_change (position, buffer, Qt);
  if (XFASTINT (overlay_limit) < XFASTINT (proplimit))
    proplimit = overlay_limit;
  /* PROPLIMIT is now a lower bound for the next change
     in invisible status.  If that is plenty far away,
     use that lower bound.  */
  if (XFASTINT (proplimit) > pos + 100 || XFASTINT (proplimit) >= to)
    *next_boundary_p = XFASTINT (proplimit);
  /* Otherwise, scan for the next `invisible' property change.  */
  else
    {
      /* Don't scan terribly far.  */
      XSETFASTINT (proplimit, min (pos + 100, to));
      /* No matter what, don't go past next overlay change.  */
      if (XFASTINT (overlay_limit) < XFASTINT (proplimit))
	proplimit = overlay_limit;
      tmp = Fnext_single_property_change (position, Qinvisible,
					  buffer, proplimit);
      end = XFASTINT (tmp);
#if 0
      /* Don't put the boundary in the middle of multibyte form if
         there is no actual property change.  */
      if (end == pos + 100
	  && !NILP (current_buffer->enable_multibyte_characters)
	  && end < ZV)
	while (pos < end && !CHAR_HEAD_P (POS_ADDR (end)))
	  end--;
#endif
      *next_boundary_p = end;
    }
  /* if the `invisible' property is set, we can skip to
     the next property change */
  prop = Fget_char_property (position, Qinvisible,
			     (!NILP (window)
			      && EQ (XWINDOW (window)->buffer, buffer))
			     ? window : buffer);
  inv_p = TEXT_PROP_MEANS_INVISIBLE (prop);
  /* When counting columns (window == nil), don't skip over ellipsis text.  */
  if (NILP (window) ? inv_p == 1 : inv_p)
    return *next_boundary_p;
  return pos;
}

/* Set variables WIDTH and BYTES for a multibyte sequence starting at P.

   DP is a display table or NULL.

   This macro is used in scan_for_column and in
   compute_motion.  */

#define MULTIBYTE_BYTES_WIDTH(p, dp, bytes, width)			\
  do {									\
    int ch;								\
    									\
    ch = STRING_CHAR_AND_LENGTH (p, bytes);				\
    if (BYTES_BY_CHAR_HEAD (*p) != bytes)				\
      width = bytes * 4;						\
    else								\
      {									\
	if (dp != 0 && VECTORP (DISP_CHAR_VECTOR (dp, ch)))		\
	  width = sanitize_char_width (ASIZE (DISP_CHAR_VECTOR (dp, ch))); \
	else								\
	  width = CHAR_WIDTH (ch);					\
      }									\
  } while (0)


DEFUN ("current-column", Fcurrent_column, Scurrent_column, 0, 0, 0,
       doc: /* Return the horizontal position of point.  Beginning of line is column 0.
This is calculated by adding together the widths of all the displayed
representations of the character between the start of the previous line
and point (eg. control characters will have a width of 2 or 4, tabs
will have a variable width).
Ignores finite width of frame, which means that this function may return
values greater than (frame-width).
Whether the line is visible (if `selective-display' is t) has no effect;
however, ^M is treated as end of line when `selective-display' is t.
Text that has an invisible property is considered as having width 0, unless
`buffer-invisibility-spec' specifies that it is replaced by an ellipsis.  */)
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, current_column ());
  return temp;
}

/* Cancel any recorded value of the horizontal position.  */

void
invalidate_current_column (void)
{
  last_known_column_point = 0;
}

EMACS_INT
current_column (void)
{
  register EMACS_INT col;
  register unsigned char *ptr, *stop;
  register int tab_seen;
  EMACS_INT post_tab;
  register int c;
  int tab_width = SANE_TAB_WIDTH (current_buffer);
  int ctl_arrow = !NILP (BVAR (current_buffer, ctl_arrow));
  register struct Lisp_Char_Table *dp = buffer_display_table ();

  if (PT == last_known_column_point
      && MODIFF == last_known_column_modified)
    return last_known_column;

  /* If the buffer has overlays, text properties,
     or multibyte characters, use a more general algorithm.  */
  if (BUF_INTERVALS (current_buffer)
      || current_buffer->overlays_before
      || current_buffer->overlays_after
      || Z != Z_BYTE)
    return current_column_1 ();

  /* Scan backwards from point to the previous newline,
     counting width.  Tab characters are the only complicated case.  */

  /* Make a pointer for decrementing through the chars before point.  */
  ptr = BYTE_POS_ADDR (PT_BYTE - 1) + 1;
  /* Make a pointer to where consecutive chars leave off,
     going backwards from point.  */
  if (PT == BEGV)
    stop = ptr;
  else if (PT <= GPT || BEGV > GPT)
    stop = BEGV_ADDR;
  else
    stop = GAP_END_ADDR;

  col = 0, tab_seen = 0, post_tab = 0;

  while (1)
    {
      EMACS_INT i, n;
      Lisp_Object charvec;

      if (ptr == stop)
	{
	  /* We stopped either for the beginning of the buffer
	     or for the gap.  */
	  if (ptr == BEGV_ADDR)
	    break;

	  /* It was the gap.  Jump back over it.  */
	  stop = BEGV_ADDR;
	  ptr = GPT_ADDR;

	  /* Check whether that brings us to beginning of buffer.  */
	  if (BEGV >= GPT)
	    break;
	}

      c = *--ptr;

      if (dp && VECTORP (DISP_CHAR_VECTOR (dp, c)))
	{
	  charvec = DISP_CHAR_VECTOR (dp, c);
	  n = ASIZE (charvec);
	}
      else
	{
	  charvec = Qnil;
	  n = 1;
	}

      for (i = n - 1; i >= 0; --i)
	{
	  if (VECTORP (charvec))
	    {
	      /* This should be handled the same as
		 next_element_from_display_vector does it.  */
	      Lisp_Object entry = AREF (charvec, i);

	      if (GLYPH_CODE_P (entry)
		  && GLYPH_CODE_CHAR_VALID_P (entry))
		c = GLYPH_CODE_CHAR (entry);
	      else
		c = ' ';
	    }

	  if (c >= 040 && c < 0177)
	    col++;
	  else if (c == '\n'
		   || (c == '\r'
		       && EQ (BVAR (current_buffer, selective_display), Qt)))
	    {
	      ptr++;
	      goto start_of_line_found;
	    }
	  else if (c == '\t')
	    {
	      if (tab_seen)
		col = ((col + tab_width) / tab_width) * tab_width;

	      post_tab += col;
	      col = 0;
	      tab_seen = 1;
	    }
	  else if (VECTORP (charvec))
	    /* With a display table entry, C is displayed as is, and
	       not displayed as \NNN or as ^N.  If C is a single-byte
	       character, it takes one column.  If C is multi-byte in
	       an unibyte buffer, it's translated to unibyte, so it
	       also takes one column.  */
	    ++col;
	  else
	    col += (ctl_arrow && c < 0200) ? 2 : 4;
	}
    }

 start_of_line_found:

  if (tab_seen)
    {
      col = ((col + tab_width) / tab_width) * tab_width;
      col += post_tab;
    }

  if (ptr == BEGV_ADDR)
    current_column_bol_cache = BEGV;
  else
    current_column_bol_cache = BYTE_TO_CHAR (PTR_BYTE_POS (ptr));

  last_known_column = col;
  last_known_column_point = PT;
  last_known_column_modified = MODIFF;

  return col;
}


/* Check the presence of a display property and compute its width.
   If a property was found and its width was found as well, return
   its width (>= 0) and set the position of the end of the property
   in ENDPOS.
   Otherwise just return -1.  */
static int
check_display_width (EMACS_INT pos, EMACS_INT col, EMACS_INT *endpos)
{
  Lisp_Object val, overlay;

  if (CONSP (val = get_char_property_and_overlay
	     (make_number (pos), Qdisplay, Qnil, &overlay))
      && EQ (Qspace, XCAR (val)))
    { /* FIXME: Use calc_pixel_width_or_height.  */
      Lisp_Object plist = XCDR (val), prop;
      int width = -1;

      if ((prop = Fplist_get (plist, QCwidth), NATNUMP (prop)))
	width = XINT (prop);
      else if (FLOATP (prop))
	width = (int)(XFLOAT_DATA (prop) + 0.5);
      else if ((prop = Fplist_get (plist, QCalign_to), NATNUMP (prop)))
	width = XINT (prop) - col;
      else if (FLOATP (prop))
	width = (int)(XFLOAT_DATA (prop) + 0.5) - col;

      if (width >= 0)
	{
	  EMACS_INT start;
	  if (OVERLAYP (overlay))
	    *endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
	  else
	    get_property_and_range (pos, Qdisplay, &val, &start, endpos, Qnil);
	  return width;
	}
    }
  return -1;
}

/* Scanning from the beginning of the current line, stop at the buffer
   position ENDPOS or at the column GOALCOL or at the end of line, whichever
   comes first.
   Return the resulting buffer position and column in ENDPOS and GOALCOL.
   PREVCOL gets set to the column of the previous position (it's always
   strictly smaller than the goal column).  */
static void
scan_for_column (EMACS_INT *endpos, EMACS_INT *goalcol, EMACS_INT *prevcol)
{
  int tab_width = SANE_TAB_WIDTH (current_buffer);
  register int ctl_arrow = !NILP (BVAR (current_buffer, ctl_arrow));
  register struct Lisp_Char_Table *dp = buffer_display_table ();
  int multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  struct composition_it cmp_it;
  Lisp_Object window;
  struct window *w;

  /* Start the scan at the beginning of this line with column number 0.  */
  register EMACS_INT col = 0, prev_col = 0;
  EMACS_INT goal = goalcol ? *goalcol : MOST_POSITIVE_FIXNUM;
  EMACS_INT end = endpos ? *endpos : PT;
  EMACS_INT scan, scan_byte;
  EMACS_INT next_boundary;
  {
  EMACS_INT opoint = PT, opoint_byte = PT_BYTE;
  scan_newline (PT, PT_BYTE, BEGV, BEGV_BYTE, -1, 1);
  current_column_bol_cache = PT;
  scan = PT, scan_byte = PT_BYTE;
  SET_PT_BOTH (opoint, opoint_byte);
  next_boundary = scan;
  }

  window = Fget_buffer_window (Fcurrent_buffer (), Qnil);
  w = ! NILP (window) ? XWINDOW (window) : NULL;

  memset (&cmp_it, 0, sizeof cmp_it);
  cmp_it.id = -1;
  composition_compute_stop_pos (&cmp_it, scan, scan_byte, end, Qnil);

  /* Scan forward to the target position.  */
  while (scan < end)
    {
      int c;

      /* Occasionally we may need to skip invisible text.  */
      while (scan == next_boundary)
	{
	  EMACS_INT old_scan = scan;
	  /* This updates NEXT_BOUNDARY to the next place
	     where we might need to skip more invisible text.  */
	  scan = skip_invisible (scan, &next_boundary, end, Qnil);
	  if (scan != old_scan)
	    scan_byte = CHAR_TO_BYTE (scan);
	  if (scan >= end)
	    goto endloop;
	}

      /* Test reaching the goal column.  We do this after skipping
	 invisible characters, so that we put point before the
	 character on which the cursor will appear.  */
      if (col >= goal)
	break;
      prev_col = col;

      { /* Check display property.  */
	EMACS_INT endp;
	int width = check_display_width (scan, col, &endp);
	if (width >= 0)
	  {
	    col += width;
	    if (endp > scan) /* Avoid infinite loops with 0-width overlays.  */
	      {
		scan = endp; scan_byte = charpos_to_bytepos (scan);
		continue;
	      }
	  }
      }

      /* Check composition sequence.  */
      if (cmp_it.id >= 0
	  || (scan == cmp_it.stop_pos
	      && composition_reseat_it (&cmp_it, scan, scan_byte, end,
					w, NULL, Qnil)))
	composition_update_it (&cmp_it, scan, scan_byte, Qnil);
      if (cmp_it.id >= 0)
	{
	  scan += cmp_it.nchars;
	  scan_byte += cmp_it.nbytes;
	  if (scan <= end)
	    col += cmp_it.width;
	  if (cmp_it.to == cmp_it.nglyphs)
	    {
	      cmp_it.id = -1;
	      composition_compute_stop_pos (&cmp_it, scan, scan_byte, end,
					    Qnil);
	    }
	  else
	    cmp_it.from = cmp_it.to;
	  continue;
	}

      c = FETCH_BYTE (scan_byte);

      /* See if there is a display table and it relates
	 to this character.  */

      if (dp != 0
	  && ! (multibyte && LEADING_CODE_P (c))
	  && VECTORP (DISP_CHAR_VECTOR (dp, c)))
	{
	  Lisp_Object charvec;
	  EMACS_INT i, n;

	  /* This character is displayed using a vector of glyphs.
	     Update the column/position based on those glyphs.  */

	  charvec = DISP_CHAR_VECTOR (dp, c);
	  n = ASIZE (charvec);

	  for (i = 0; i < n; i++)
	    {
	      /* This should be handled the same as
		 next_element_from_display_vector does it.  */
	      Lisp_Object entry = AREF (charvec, i);

	      if (GLYPH_CODE_P (entry)
		  && GLYPH_CODE_CHAR_VALID_P (entry))
		c = GLYPH_CODE_CHAR (entry);
	      else
		c = ' ';

	      if (c == '\n')
		goto endloop;
	      if (c == '\r' && EQ (BVAR (current_buffer, selective_display), Qt))
		goto endloop;
	      if (c == '\t')
		{
		  col += tab_width;
		  col = col / tab_width * tab_width;
		}
	      else
		++col;
	    }
	}
      else
	{
	  /* The display table doesn't affect this character;
	     it displays as itself.  */

	  if (c == '\n')
	    goto endloop;
	  if (c == '\r' && EQ (BVAR (current_buffer, selective_display), Qt))
	    goto endloop;
	  if (c == '\t')
	    {
	      col += tab_width;
	      col = col / tab_width * tab_width;
	    }
	  else if (multibyte && LEADING_CODE_P (c))
	    {
	      /* Start of multi-byte form.  */
	      unsigned char *ptr;
	      int bytes, width;

	      ptr = BYTE_POS_ADDR (scan_byte);
	      MULTIBYTE_BYTES_WIDTH (ptr, dp, bytes, width);
	      /* Subtract one to compensate for the increment
		 that is going to happen below.  */
	      scan_byte += bytes - 1;
	      col += width;
	    }
	  else if (ctl_arrow && (c < 040 || c == 0177))
	    col += 2;
	  else if (c < 040 || c >= 0177)
	    col += 4;
	  else
	    col++;
	}
      scan++;
      scan_byte++;

    }
 endloop:

  last_known_column = col;
  last_known_column_point = PT;
  last_known_column_modified = MODIFF;

  if (goalcol)
    *goalcol = col;
  if (endpos)
    *endpos = scan;
  if (prevcol)
    *prevcol = prev_col;
}

/* Return the column number of position POS
   by scanning forward from the beginning of the line.
   This function handles characters that are invisible
   due to text properties or overlays.  */

static EMACS_INT
current_column_1 (void)
{
  EMACS_INT col = MOST_POSITIVE_FIXNUM;
  EMACS_INT opoint = PT;

  scan_for_column (&opoint, &col, NULL);
  return col;
}


#if 0 /* Not used.  */

/* Return the width in columns of the part of STRING from BEG to END.
   If BEG is nil, that stands for the beginning of STRING.
   If END is nil, that stands for the end of STRING.  */

static double
string_display_width (Lisp_Object string, Lisp_Object beg, Lisp_Object end)
{
  register int col;
  register unsigned char *ptr, *stop;
  register int tab_seen;
  int post_tab;
  register int c;
  int tab_width = SANE_TAB_WIDTH (current_buffer);
  int ctl_arrow = !NILP (current_buffer->ctl_arrow);
  register struct Lisp_Char_Table *dp = buffer_display_table ();
  int b, e;

  if (NILP (end))
    e = SCHARS (string);
  else
    {
      CHECK_NUMBER (end);
      e = XINT (end);
    }

  if (NILP (beg))
    b = 0;
  else
    {
      CHECK_NUMBER (beg);
      b = XINT (beg);
    }

  /* Make a pointer for decrementing through the chars before point.  */
  ptr = SDATA (string) + e;
  /* Make a pointer to where consecutive chars leave off,
     going backwards from point.  */
  stop = SDATA (string) + b;

  col = 0, tab_seen = 0, post_tab = 0;

  while (1)
    {
      if (ptr == stop)
	break;

      c = *--ptr;
      if (dp != 0 && VECTORP (DISP_CHAR_VECTOR (dp, c)))
	col += ASIZE (DISP_CHAR_VECTOR (dp, c));
      else if (c >= 040 && c < 0177)
	col++;
      else if (c == '\n')
	break;
      else if (c == '\t')
	{
	  if (tab_seen)
	    col = ((col + tab_width) / tab_width) * tab_width;

	  post_tab += col;
	  col = 0;
	  tab_seen = 1;
	}
      else
	col += (ctl_arrow && c < 0200) ? 2 : 4;
    }

  if (tab_seen)
    {
      col = ((col + tab_width) / tab_width) * tab_width;
      col += post_tab;
    }

  return col;
}

#endif /* 0 */


DEFUN ("indent-to", Findent_to, Sindent_to, 1, 2, "NIndent to column: ",
       doc: /* Indent from point with tabs and spaces until COLUMN is reached.
Optional second argument MINIMUM says always do at least MINIMUM spaces
even if that goes past COLUMN; by default, MINIMUM is zero.

The return value is COLUMN.  */)
  (Lisp_Object column, Lisp_Object minimum)
{
  EMACS_INT mincol;
  register EMACS_INT fromcol;
  int tab_width = SANE_TAB_WIDTH (current_buffer);

  CHECK_NUMBER (column);
  if (NILP (minimum))
    XSETFASTINT (minimum, 0);
  CHECK_NUMBER (minimum);

  fromcol = current_column ();
  mincol = fromcol + XINT (minimum);
  if (mincol < XINT (column)) mincol = XINT (column);

  if (fromcol == mincol)
    return make_number (mincol);

  if (indent_tabs_mode)
    {
      Lisp_Object n;
      XSETFASTINT (n, mincol / tab_width - fromcol / tab_width);
      if (XFASTINT (n) != 0)
	{
	  Finsert_char (make_number ('\t'), n, Qt);

	  fromcol = (mincol / tab_width) * tab_width;
	}
    }

  XSETFASTINT (column, mincol - fromcol);
  Finsert_char (make_number (' '), column, Qt);

  last_known_column = mincol;
  last_known_column_point = PT;
  last_known_column_modified = MODIFF;

  XSETINT (column, mincol);
  return column;
}


DEFUN ("current-indentation", Fcurrent_indentation, Scurrent_indentation,
       0, 0, 0,
       doc: /* Return the indentation of the current line.
This is the horizontal position of the character
following any initial whitespace.  */)
  (void)
{
  Lisp_Object val;
  EMACS_INT opoint = PT, opoint_byte = PT_BYTE;

  scan_newline (PT, PT_BYTE, BEGV, BEGV_BYTE, -1, 1);

  XSETFASTINT (val, position_indentation (PT_BYTE));
  SET_PT_BOTH (opoint, opoint_byte);
  return val;
}

static EMACS_INT
position_indentation (ptrdiff_t pos_byte)
{
  register EMACS_INT column = 0;
  int tab_width = SANE_TAB_WIDTH (current_buffer);
  register unsigned char *p;
  register unsigned char *stop;
  unsigned char *start;
  EMACS_INT next_boundary_byte = pos_byte;
  EMACS_INT ceiling = next_boundary_byte;

  p = BYTE_POS_ADDR (pos_byte);
  /* STOP records the value of P at which we will need
     to think about the gap, or about invisible text,
     or about the end of the buffer.  */
  stop = p;
  /* START records the starting value of P.  */
  start = p;
  while (1)
    {
      while (p == stop)
	{
	  EMACS_INT stop_pos_byte;

	  /* If we have updated P, set POS_BYTE to match.
	     The first time we enter the loop, POS_BYTE is already right.  */
	  if (p != start)
	    pos_byte = PTR_BYTE_POS (p);
	  /* Consider the various reasons STOP might have been set here.  */
	  if (pos_byte == ZV_BYTE)
	    return column;
	  if (pos_byte == next_boundary_byte)
	    {
	      EMACS_INT next_boundary;
	      EMACS_INT pos = BYTE_TO_CHAR (pos_byte);
	      pos = skip_invisible (pos, &next_boundary, ZV, Qnil);
	      pos_byte = CHAR_TO_BYTE (pos);
	      next_boundary_byte = CHAR_TO_BYTE (next_boundary);
	    }
	  if (pos_byte >= ceiling)
	    ceiling = BUFFER_CEILING_OF (pos_byte) + 1;
	  /* Compute the next place we need to stop and think,
	     and set STOP accordingly.  */
	  stop_pos_byte = min (ceiling, next_boundary_byte);
	  /* The -1 and +1 arrange to point at the first byte of gap
	     (if STOP_POS_BYTE is the position of the gap)
	     rather than at the data after the gap.  */

	  stop = BYTE_POS_ADDR (stop_pos_byte - 1) + 1;
	  p = BYTE_POS_ADDR (pos_byte);
	}
      switch (*p++)
	{
	case 0240:
	  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	    return column;
	case ' ':
	  column++;
	  break;
	case '\t':
	  column += tab_width - column % tab_width;
	  break;
	default:
	  if (ASCII_BYTE_P (p[-1])
	      || NILP (BVAR (current_buffer, enable_multibyte_characters)))
	    return column;
	  {
	    int c;
	    pos_byte = PTR_BYTE_POS (p - 1);
	    c = FETCH_MULTIBYTE_CHAR (pos_byte);
	    if (CHAR_HAS_CATEGORY (c, ' '))
	      {
		column++;
		INC_POS (pos_byte);
		p = BYTE_POS_ADDR (pos_byte);
	      }
	    else
	      return column;
	  }
	}
    }
}

/* Test whether the line beginning at POS is indented beyond COLUMN.
   Blank lines are treated as if they had the same indentation as the
   preceding line.  */

int
indented_beyond_p (EMACS_INT pos, EMACS_INT pos_byte, EMACS_INT column)
{
  EMACS_INT val;
  EMACS_INT opoint = PT, opoint_byte = PT_BYTE;

  SET_PT_BOTH (pos, pos_byte);
  while (PT > BEGV && FETCH_BYTE (PT_BYTE) == '\n')
    scan_newline (PT - 1, PT_BYTE - 1, BEGV, BEGV_BYTE, -1, 0);

  val = position_indentation (PT_BYTE);
  SET_PT_BOTH (opoint, opoint_byte);
  return val >= column;
}

DEFUN ("move-to-column", Fmove_to_column, Smove_to_column, 1, 2, "p",
       doc: /* Move point to column COLUMN in the current line.
Interactively, COLUMN is the value of prefix numeric argument.
The column of a character is calculated by adding together the widths
as displayed of the previous characters in the line.
This function ignores line-continuation;
there is no upper limit on the column number a character can have
and horizontal scrolling has no effect.

If specified column is within a character, point goes after that character.
If it's past end of line, point goes to end of line.

Optional second argument FORCE non-nil means if COLUMN is in the
middle of a tab character, change it to spaces.
In addition, if FORCE is t, and the line is too short to reach
COLUMN, add spaces/tabs to get there.

The return value is the current column.  */)
  (Lisp_Object column, Lisp_Object force)
{
  EMACS_INT pos;
  EMACS_INT col, prev_col;
  EMACS_INT goal;

  CHECK_NATNUM (column);
  goal = XINT (column);

  col = goal;
  pos = ZV;
  scan_for_column (&pos, &col, &prev_col);

  SET_PT (pos);

  /* If a tab char made us overshoot, change it to spaces
     and scan through it again.  */
  if (!NILP (force) && col > goal)
    {
      int c;
      EMACS_INT pos_byte = PT_BYTE;

      DEC_POS (pos_byte);
      c = FETCH_CHAR (pos_byte);
      if (c == '\t' && prev_col < goal)
	{
	  EMACS_INT goal_pt, goal_pt_byte;

	  /* Insert spaces in front of the tab to reach GOAL.  Do this
	     first so that a marker at the end of the tab gets
	     adjusted.  */
	  SET_PT_BOTH (PT - 1, PT_BYTE - 1);
	  Finsert_char (make_number (' '), make_number (goal - prev_col), Qt);

	  /* Now delete the tab, and indent to COL.  */
	  del_range (PT, PT + 1);
	  goal_pt = PT;
	  goal_pt_byte = PT_BYTE;
	  Findent_to (make_number (col), Qnil);
	  SET_PT_BOTH (goal_pt, goal_pt_byte);

	  /* Set the last_known... vars consistently.  */
	  col = goal;
	}
    }

  /* If line ends prematurely, add space to the end.  */
  if (col < goal && EQ (force, Qt))
    Findent_to (make_number (col = goal), Qnil);

  last_known_column = col;
  last_known_column_point = PT;
  last_known_column_modified = MODIFF;

  return make_number (col);
}

/* compute_motion: compute buffer posn given screen posn and vice versa */

static struct position val_compute_motion;

/* Scan the current buffer forward from offset FROM, pretending that
   this is at line FROMVPOS, column FROMHPOS, until reaching buffer
   offset TO or line TOVPOS, column TOHPOS (whichever comes first),
   and return the ending buffer position and screen location.  If we
   can't hit the requested column exactly (because of a tab or other
   multi-column character), overshoot.

   DID_MOTION is 1 if FROMHPOS has already accounted for overlay strings
   at FROM.  This is the case if FROMVPOS and FROMVPOS came from an
   earlier call to compute_motion.  The other common case is that FROMHPOS
   is zero and FROM is a position that "belongs" at column zero, but might
   be shifted by overlay strings; in this case DID_MOTION should be 0.

   WIDTH is the number of columns available to display text;
   compute_motion uses this to handle continuation lines and such.
   If WIDTH is -1, use width of window's text area adjusted for
   continuation glyph when needed.

   HSCROLL is the number of columns not being displayed at the left
   margin; this is usually taken from a window's hscroll member.
   TAB_OFFSET is the number of columns of the first tab that aren't
   being displayed, perhaps because of a continuation line or
   something.

   compute_motion returns a pointer to a struct position.  The bufpos
   member gives the buffer position at the end of the scan, and hpos
   and vpos give its cartesian location.  prevhpos is the column at
   which the character before bufpos started, and contin is non-zero
   if we reached the current line by continuing the previous.

   Note that FROMHPOS and TOHPOS should be expressed in real screen
   columns, taking HSCROLL and the truncation glyph at the left margin
   into account.  That is, beginning-of-line moves you to the hpos
   -HSCROLL + (HSCROLL > 0).

   For example, to find the buffer position of column COL of line LINE
   of a certain window, pass the window's starting location as FROM
   and the window's upper-left coordinates as FROMVPOS and FROMHPOS.
   Pass the buffer's ZV as TO, to limit the scan to the end of the
   visible section of the buffer, and pass LINE and COL as TOVPOS and
   TOHPOS.

   When displaying in window w, a typical formula for WIDTH is:

	window_width - 1
	 - (has_vertical_scroll_bars
	    ? WINDOW_CONFIG_SCROLL_BAR_COLS (window)
	    : (window_width + window_left != frame_cols))

	where
	  window_width is XFASTINT (w->total_cols),
	  window_left is XFASTINT (w->left_col),
	  has_vertical_scroll_bars is
	    WINDOW_HAS_VERTICAL_SCROLL_BAR (window)
	  and frame_cols = FRAME_COLS (XFRAME (window->frame))

   Or you can let window_body_cols do this all for you, and write:
	window_body_cols (w) - 1

   The `-1' accounts for the continuation-line backslashes; the rest
   accounts for window borders if the window is split horizontally, and
   the scroll bars if they are turned on.  */

struct position *
compute_motion (EMACS_INT from, EMACS_INT fromvpos, EMACS_INT fromhpos, int did_motion, EMACS_INT to, EMACS_INT tovpos, EMACS_INT tohpos, EMACS_INT width, EMACS_INT hscroll, EMACS_INT tab_offset, struct window *win)
{
  register EMACS_INT hpos = fromhpos;
  register EMACS_INT vpos = fromvpos;

  register EMACS_INT pos;
  EMACS_INT pos_byte;
  register int c = 0;
  int tab_width = SANE_TAB_WIDTH (current_buffer);
  register int ctl_arrow = !NILP (BVAR (current_buffer, ctl_arrow));
  register struct Lisp_Char_Table *dp = window_display_table (win);
  EMACS_INT selective
    = (INTEGERP (BVAR (current_buffer, selective_display))
       ? XINT (BVAR (current_buffer, selective_display))
       : !NILP (BVAR (current_buffer, selective_display)) ? -1 : 0);
  int selective_rlen
    = (selective && dp && VECTORP (DISP_INVIS_VECTOR (dp))
       ? ASIZE (DISP_INVIS_VECTOR (dp)) : 0);
  /* The next location where the `invisible' property changes, or an
     overlay starts or ends.  */
  EMACS_INT next_boundary = from;

  /* For computing runs of characters with similar widths.
     Invariant: width_run_width is zero, or all the characters
     from width_run_start to width_run_end have a fixed width of
     width_run_width.  */
  EMACS_INT width_run_start = from;
  EMACS_INT width_run_end   = from;
  EMACS_INT width_run_width = 0;
  Lisp_Object *width_table;
  Lisp_Object buffer;

  /* The next buffer pos where we should consult the width run cache. */
  EMACS_INT next_width_run = from;
  Lisp_Object window;

  int multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  /* If previous char scanned was a wide character,
     this is the column where it ended.  Otherwise, this is 0.  */
  EMACS_INT wide_column_end_hpos = 0;
  EMACS_INT prev_pos;		/* Previous buffer position.  */
  EMACS_INT prev_pos_byte;	/* Previous buffer position.  */
  EMACS_INT prev_hpos = 0;
  EMACS_INT prev_vpos = 0;
  EMACS_INT contin_hpos;	/* HPOS of last column of continued line.  */
  EMACS_INT prev_tab_offset;	/* Previous tab offset.  */
  EMACS_INT continuation_glyph_width;

  struct composition_it cmp_it;

  XSETBUFFER (buffer, current_buffer);
  XSETWINDOW (window, win);

  width_run_cache_on_off ();
  if (dp == buffer_display_table ())
    width_table = (VECTORP (BVAR (current_buffer, width_table))
                   ? XVECTOR (BVAR (current_buffer, width_table))->contents
                   : 0);
  else
    /* If the window has its own display table, we can't use the width
       run cache, because that's based on the buffer's display table.  */
    width_table = 0;

  /* Negative width means use all available text columns.  */
  if (width < 0)
    {
      width = window_body_cols (win);
      /* We must make room for continuation marks if we don't have fringes.  */
#ifdef HAVE_WINDOW_SYSTEM
      if (!FRAME_WINDOW_P (XFRAME (win->frame)))
#endif
	width -= 1;
    }

  continuation_glyph_width = 1;
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (win->frame)))
    continuation_glyph_width = 0;  /* In the fringe.  */
#endif

  immediate_quit = 1;
  QUIT;

  pos = prev_pos = from;
  pos_byte = prev_pos_byte = CHAR_TO_BYTE (from);
  contin_hpos = 0;
  prev_tab_offset = tab_offset;
  memset (&cmp_it, 0, sizeof cmp_it);
  cmp_it.id = -1;
  composition_compute_stop_pos (&cmp_it, pos, pos_byte, to, Qnil);

  while (1)
    {
      while (pos == next_boundary)
	{
	  EMACS_INT pos_here = pos;
	  EMACS_INT newpos;

	  /* Don't skip invisible if we are already at the margin.  */
	  if (vpos > tovpos || (vpos == tovpos && hpos >= tohpos))
	    {
	      if (contin_hpos && prev_hpos == 0
		  && hpos > tohpos
		  && (contin_hpos == width || wide_column_end_hpos > width))
		{ /* Line breaks because we can't put the character at the
		     previous line any more.  It is not the multi-column
		     character continued in middle.  Go back to previous
		     buffer position, screen position, and set tab offset
		     to previous value.  It's the beginning of the
		     line.  */
		  pos = prev_pos;
		  pos_byte = prev_pos_byte;
		  hpos = prev_hpos;
		  vpos = prev_vpos;
		  tab_offset = prev_tab_offset;
		}
	      break;
	    }

	  /* If the caller says that the screen position came from an earlier
	     call to compute_motion, then we've already accounted for the
	     overlay strings at point.  This is only true the first time
	     through, so clear the flag after testing it.  */
	  if (!did_motion)
	    /* We need to skip past the overlay strings.  Currently those
	       strings must not contain TAB;
	       if we want to relax that restriction, something will have
	       to be changed here.  */
	    {
	      unsigned char *ovstr;
	      EMACS_INT ovlen = overlay_strings (pos, win, &ovstr);
	      hpos += ((multibyte && ovlen > 0)
		       ? strwidth ((char *) ovstr, ovlen) : ovlen);
	    }
	  did_motion = 0;

	  if (pos >= to)
	    break;

	  /* Advance POS past invisible characters
	     (but not necessarily all that there are here),
	     and store in next_boundary the next position where
	     we need to call skip_invisible.  */
	  newpos = skip_invisible (pos, &next_boundary, to, window);

	  if (newpos >= to)
	    {
	      pos = min (to, newpos);
	      pos_byte = CHAR_TO_BYTE (pos);
	      goto after_loop;
	    }

	  if (newpos != pos_here)
	    {
	      pos = newpos;
	      pos_byte = CHAR_TO_BYTE (pos);
	    }
	}

      /* Handle right margin.  */
      /* Note on a wide-column character.

	 Characters are classified into the following three categories
	 according to the width (columns occupied on screen).

	 (1) single-column character: ex. `a'
	 (2) multi-column character: ex. `^A', TAB, `\033'
	 (3) wide-column character: ex. Japanese character, Chinese character
	     (In the following example, `W_' stands for them.)

	 Multi-column characters can be divided around the right margin,
	 but wide-column characters cannot.

	 NOTE:

	 (*) The cursor is placed on the next character after the point.

	     ----------
	     abcdefghi\
	     j        ^---- next after the point
	     ^---  next char. after the point.
	     ----------
	              In case of sigle-column character

	     ----------
	     abcdefgh\\
	     033     ^----  next after the point, next char. after the point.
	     ----------
	              In case of multi-column character

	     ----------
	     abcdefgh\\
	     W_      ^---- next after the point
	     ^----  next char. after the point.
	     ----------
	              In case of wide-column character

	 The problem here is continuation at a wide-column character.
	 In this case, the line may shorter less than WIDTH.
	 And we find the continuation AFTER it occurs.

       */

      if (hpos > width)
	{
	  int total_width = width + continuation_glyph_width;
	  int truncate = 0;

	  if (!NILP (Vtruncate_partial_width_windows)
	      && (total_width < FRAME_COLS (XFRAME (WINDOW_FRAME (win)))))
	    {
	      if (INTEGERP (Vtruncate_partial_width_windows))
		truncate
		  = total_width < XFASTINT (Vtruncate_partial_width_windows);
	      else
		truncate = 1;
	    }

	  if (hscroll || truncate
	      || !NILP (BVAR (current_buffer, truncate_lines)))
	    {
	      /* Truncating: skip to newline, unless we are already past
                 TO (we need to go back below).  */
	      if (pos <= to)
		{
		  pos = find_before_next_newline (pos, to, 1);
		  pos_byte = CHAR_TO_BYTE (pos);
		  hpos = width;
		  /* If we just skipped next_boundary,
		     loop around in the main while
		     and handle it.  */
		  if (pos >= next_boundary)
		    next_boundary = pos + 1;
		  prev_hpos = width;
		  prev_vpos = vpos;
		  prev_tab_offset = tab_offset;
		}
	    }
	  else
	    {
	      /* Continuing.  */
	      /* Remember the previous value.  */
	      prev_tab_offset = tab_offset;

	      if (wide_column_end_hpos > width)
		{
		  hpos -= prev_hpos;
		  tab_offset += prev_hpos;
		}
	      else
		{
		  tab_offset += width;
		  hpos -= width;
		}
	      vpos++;
	      contin_hpos = prev_hpos;
	      prev_hpos = 0;
	      prev_vpos = vpos;
	    }
	}

      /* Stop if past the target buffer position or screen position.  */
      if (pos > to)
	{
	  /* Go back to the previous position.  */
	  pos = prev_pos;
	  pos_byte = prev_pos_byte;
	  hpos = prev_hpos;
	  vpos = prev_vpos;
	  tab_offset = prev_tab_offset;

	  /* NOTE on contin_hpos, hpos, and prev_hpos.

	     ----------
	     abcdefgh\\
	     W_      ^----  contin_hpos
	     | ^-----  hpos
	     \---- prev_hpos
	     ----------
	   */

	  if (contin_hpos && prev_hpos == 0
	      && contin_hpos < width && !wide_column_end_hpos)
	    {
	      /* Line breaking occurs in the middle of multi-column
		 character.  Go back to previous line.  */
	      hpos = contin_hpos;
	      vpos = vpos - 1;
	    }
	  break;
	}

      if (vpos > tovpos || (vpos == tovpos && hpos >= tohpos))
	{
	  if (contin_hpos && prev_hpos == 0
	      && hpos > tohpos
	      && (contin_hpos == width || wide_column_end_hpos > width))
	    { /* Line breaks because we can't put the character at the
		 previous line any more.  It is not the multi-column
		 character continued in middle.  Go back to previous
		 buffer position, screen position, and set tab offset
		 to previous value.  It's the beginning of the
		 line.  */
	      pos = prev_pos;
	      pos_byte = prev_pos_byte;
	      hpos = prev_hpos;
	      vpos = prev_vpos;
	      tab_offset = prev_tab_offset;
	    }
	  break;
	}
      if (pos == ZV) /* We cannot go beyond ZV.  Stop here. */
	break;

      prev_hpos = hpos;
      prev_vpos = vpos;
      prev_pos = pos;
      prev_pos_byte = pos_byte;
      wide_column_end_hpos = 0;

      /* Consult the width run cache to see if we can avoid inspecting
         the text character-by-character.  */
      if (current_buffer->width_run_cache && pos >= next_width_run)
        {
          ptrdiff_t run_end;
          int common_width
            = region_cache_forward (current_buffer,
                                    current_buffer->width_run_cache,
                                    pos, &run_end);

          /* A width of zero means the character's width varies (like
             a tab), is meaningless (like a newline), or we just don't
             want to skip over it for some other reason.  */
          if (common_width != 0)
            {
              EMACS_INT run_end_hpos;

              /* Don't go past the final buffer posn the user
                 requested.  */
              if (run_end > to)
                run_end = to;

              run_end_hpos = hpos + (run_end - pos) * common_width;

              /* Don't go past the final horizontal position the user
                 requested.  */
              if (vpos == tovpos && run_end_hpos > tohpos)
                {
                  run_end      = pos + (tohpos - hpos) / common_width;
                  run_end_hpos = hpos + (run_end - pos) * common_width;
                }

              /* Don't go past the margin.  */
              if (run_end_hpos >= width)
                {
                  run_end      = pos + (width  - hpos) / common_width;
                  run_end_hpos = hpos + (run_end - pos) * common_width;
                }

              hpos = run_end_hpos;
              if (run_end > pos)
                prev_hpos = hpos - common_width;
	      if (pos != run_end)
		{
		  pos = run_end;
		  pos_byte = CHAR_TO_BYTE (pos);
		}
            }

          next_width_run = run_end + 1;
        }

      /* We have to scan the text character-by-character.  */
      else
	{
	  EMACS_INT i, n;
	  Lisp_Object charvec;

	  /* Check composition sequence.  */
	  if (cmp_it.id >= 0
	      || (pos == cmp_it.stop_pos
		  && composition_reseat_it (&cmp_it, pos, pos_byte, to, win,
					    NULL, Qnil)))
	    composition_update_it (&cmp_it, pos, pos_byte, Qnil);
	  if (cmp_it.id >= 0)
	    {
	      pos += cmp_it.nchars;
	      pos_byte += cmp_it.nbytes;
	      hpos += cmp_it.width;
	      if (cmp_it.to == cmp_it.nglyphs)
		{
		  cmp_it.id = -1;
		  composition_compute_stop_pos (&cmp_it, pos, pos_byte, to,
						Qnil);
		}
	      else
		cmp_it.from = cmp_it.to;
	      continue;
	    }

	  c = FETCH_BYTE (pos_byte);
	  pos++, pos_byte++;

	  /* Perhaps add some info to the width_run_cache.  */
	  if (current_buffer->width_run_cache)
	    {
	      /* Is this character part of the current run?  If so, extend
		 the run.  */
	      if (pos - 1 == width_run_end
		  && XFASTINT (width_table[c]) == width_run_width)
		width_run_end = pos;

	      /* The previous run is over, since this is a character at a
		 different position, or a different width.  */
	      else
		{
		  /* Have we accumulated a run to put in the cache?
		     (Currently, we only cache runs of width == 1).  */
		  if (width_run_start < width_run_end
		      && width_run_width == 1)
		    know_region_cache (current_buffer,
				       current_buffer->width_run_cache,
				       width_run_start, width_run_end);

		  /* Start recording a new width run.  */
		  width_run_width = XFASTINT (width_table[c]);
		  width_run_start = pos - 1;
		  width_run_end = pos;
		}
	    }

	  if (dp != 0
	      && ! (multibyte && LEADING_CODE_P (c))
	      && VECTORP (DISP_CHAR_VECTOR (dp, c)))
	    {
	      charvec = DISP_CHAR_VECTOR (dp, c);
	      n = ASIZE (charvec);
	    }
	  else
	    {
	      charvec = Qnil;
	      n = 1;
	    }

	  for (i = 0; i < n; ++i)
	    {
	      if (VECTORP (charvec))
		{
		  /* This should be handled the same as
		     next_element_from_display_vector does it.  */
		  Lisp_Object entry = AREF (charvec, i);

		  if (GLYPH_CODE_P (entry)
		      && GLYPH_CODE_CHAR_VALID_P (entry))
		    c = GLYPH_CODE_CHAR (entry);
		  else
		    c = ' ';
		}

	      if (c >= 040 && c < 0177)
		hpos++;
	      else if (c == '\t')
		{
		  int tem = ((hpos + tab_offset + hscroll - (hscroll > 0))
			     % tab_width);
		  if (tem < 0)
		    tem += tab_width;
		  hpos += tab_width - tem;
		}
	      else if (c == '\n')
		{
		  if (selective > 0
		      && indented_beyond_p (pos, pos_byte, selective))
		    {
		      /* If (pos == to), we don't have to take care of
			 selective display.  */
		      if (pos < to)
			{
			  /* Skip any number of invisible lines all at once */
			  do
			    {
			      pos = find_before_next_newline (pos, to, 1);
			      if (pos < to)
				pos++;
			      pos_byte = CHAR_TO_BYTE (pos);
			    }
			  while (pos < to
				 && indented_beyond_p (pos, pos_byte,
                                                       selective));
			  /* Allow for the " ..." that is displayed for them. */
			  if (selective_rlen)
			    {
			      hpos += selective_rlen;
			      if (hpos >= width)
				hpos = width;
			    }
			  DEC_BOTH (pos, pos_byte);
			  /* We have skipped the invis text, but not the
			     newline after.  */
			}
		    }
		  else
		    {
		      /* A visible line.  */
		      vpos++;
		      hpos = 0;
		      hpos -= hscroll;
		      /* Count the truncation glyph on column 0 */
		      if (hscroll > 0)
			hpos += continuation_glyph_width;
		      tab_offset = 0;
		    }
		  contin_hpos = 0;
		}
	      else if (c == CR && selective < 0)
		{
		  /* In selective display mode,
		     everything from a ^M to the end of the line is invisible.
		     Stop *before* the real newline.  */
		  if (pos < to)
		    {
		      pos = find_before_next_newline (pos, to, 1);
		      pos_byte = CHAR_TO_BYTE (pos);
		    }
		  /* If we just skipped next_boundary,
		     loop around in the main while
		     and handle it.  */
		  if (pos > next_boundary)
		    next_boundary = pos;
		  /* Allow for the " ..." that is displayed for them. */
		  if (selective_rlen)
		    {
		      hpos += selective_rlen;
		      if (hpos >= width)
			hpos = width;
		    }
		}
	      else if (multibyte && LEADING_CODE_P (c))
		{
		  /* Start of multi-byte form.  */
		  unsigned char *ptr;
		  int mb_bytes, mb_width;

		  pos_byte--;	/* rewind POS_BYTE */
		  ptr = BYTE_POS_ADDR (pos_byte);
		  MULTIBYTE_BYTES_WIDTH (ptr, dp, mb_bytes, mb_width);
		  pos_byte += mb_bytes;
		  if (mb_width > 1 && BYTES_BY_CHAR_HEAD (*ptr) == mb_bytes)
		    wide_column_end_hpos = hpos + mb_width;
		  hpos += mb_width;
		}
	      else if (VECTORP (charvec))
		++hpos;
	      else
		hpos += (ctl_arrow && c < 0200) ? 2 : 4;
	    }
	}
    }

 after_loop:

  /* Remember any final width run in the cache.  */
  if (current_buffer->width_run_cache
      && width_run_width == 1
      && width_run_start < width_run_end)
    know_region_cache (current_buffer, current_buffer->width_run_cache,
                       width_run_start, width_run_end);

  val_compute_motion.bufpos = pos;
  val_compute_motion.bytepos = pos_byte;
  val_compute_motion.hpos = hpos;
  val_compute_motion.vpos = vpos;
  if (contin_hpos && prev_hpos == 0)
    val_compute_motion.prevhpos = contin_hpos;
  else
    val_compute_motion.prevhpos = prev_hpos;
  /* We always handle all of them here; none of them remain to do.  */
  val_compute_motion.ovstring_chars_done = 0;

  /* Nonzero if have just continued a line */
  val_compute_motion.contin = (contin_hpos && prev_hpos == 0);

  immediate_quit = 0;
  return &val_compute_motion;
}


DEFUN ("compute-motion", Fcompute_motion, Scompute_motion, 7, 7, 0,
       doc: /* Scan through the current buffer, calculating screen position.
Scan the current buffer forward from offset FROM,
assuming it is at position FROMPOS--a cons of the form (HPOS . VPOS)--
to position TO or position TOPOS--another cons of the form (HPOS . VPOS)--
and return the ending buffer position and screen location.

If TOPOS is nil, the actual width and height of the window's
text area are used.

There are three additional arguments:

WIDTH is the number of columns available to display text;
this affects handling of continuation lines.  A value of nil
corresponds to the actual number of available text columns.

OFFSETS is either nil or a cons cell (HSCROLL . TAB-OFFSET).
HSCROLL is the number of columns not being displayed at the left
margin; this is usually taken from a window's hscroll member.
TAB-OFFSET is the number of columns of the first tab that aren't
being displayed, perhaps because the line was continued within it.
If OFFSETS is nil, HSCROLL and TAB-OFFSET are assumed to be zero.

WINDOW is the window to operate on.  It is used to choose the display table;
if it is showing the current buffer, it is used also for
deciding which overlay properties apply.
Note that `compute-motion' always operates on the current buffer.

The value is a list of five elements:
  (POS HPOS VPOS PREVHPOS CONTIN)
POS is the buffer position where the scan stopped.
VPOS is the vertical position where the scan stopped.
HPOS is the horizontal position where the scan stopped.

PREVHPOS is the horizontal position one character back from POS.
CONTIN is t if a line was continued after (or within) the previous character.

For example, to find the buffer position of column COL of line LINE
of a certain window, pass the window's starting location as FROM
and the window's upper-left coordinates as FROMPOS.
Pass the buffer's (point-max) as TO, to limit the scan to the end of the
visible section of the buffer, and pass LINE and COL as TOPOS.  */)
  (Lisp_Object from, Lisp_Object frompos, Lisp_Object to, Lisp_Object topos, Lisp_Object width, Lisp_Object offsets, Lisp_Object window)
{
  struct window *w;
  Lisp_Object bufpos, hpos, vpos, prevhpos;
  struct position *pos;
  EMACS_INT hscroll, tab_offset;

  CHECK_NUMBER_COERCE_MARKER (from);
  CHECK_CONS (frompos);
  CHECK_NUMBER_CAR (frompos);
  CHECK_NUMBER_CDR (frompos);
  CHECK_NUMBER_COERCE_MARKER (to);
  if (!NILP (topos))
    {
      CHECK_CONS (topos);
      CHECK_NUMBER_CAR (topos);
      CHECK_NUMBER_CDR (topos);
    }
  if (!NILP (width))
    CHECK_NUMBER (width);

  if (!NILP (offsets))
    {
      CHECK_CONS (offsets);
      CHECK_NUMBER_CAR (offsets);
      CHECK_NUMBER_CDR (offsets);
      hscroll = XINT (XCAR (offsets));
      tab_offset = XINT (XCDR (offsets));
    }
  else
    hscroll = tab_offset = 0;

  if (NILP (window))
    window = Fselected_window ();
  else
    CHECK_LIVE_WINDOW (window);
  w = XWINDOW (window);

  if (XINT (from) < BEGV || XINT (from) > ZV)
    args_out_of_range_3 (from, make_number (BEGV), make_number (ZV));
  if (XINT (to) < BEGV || XINT (to) > ZV)
    args_out_of_range_3 (to, make_number (BEGV), make_number (ZV));

  pos = compute_motion (XINT (from), XINT (XCDR (frompos)),
			XINT (XCAR (frompos)), 0,
			XINT (to),
			(NILP (topos)
			 ? window_internal_height (w)
			 : XINT (XCDR (topos))),
			(NILP (topos)
			 ? (window_body_cols (w)
			    - (
#ifdef HAVE_WINDOW_SYSTEM
			       FRAME_WINDOW_P (XFRAME (w->frame)) ? 0 :
#endif
			       1))
			 : XINT (XCAR (topos))),
			(NILP (width) ? -1 : XINT (width)),
			hscroll, tab_offset,
			XWINDOW (window));

  XSETFASTINT (bufpos, pos->bufpos);
  XSETINT (hpos, pos->hpos);
  XSETINT (vpos, pos->vpos);
  XSETINT (prevhpos, pos->prevhpos);

  return Fcons (bufpos,
		Fcons (hpos,
		       Fcons (vpos,
			      Fcons (prevhpos,
				     Fcons (pos->contin ? Qt : Qnil, Qnil)))));

}

/* Fvertical_motion and vmotion */

static struct position val_vmotion;

struct position *
vmotion (register EMACS_INT from, register EMACS_INT vtarget, struct window *w)
{
  EMACS_INT hscroll = XINT (w->hscroll);
  struct position pos;
  /* vpos is cumulative vertical position, changed as from is changed */
  register int vpos = 0;
  EMACS_INT prevline;
  register EMACS_INT first;
  EMACS_INT from_byte;
  EMACS_INT lmargin = hscroll > 0 ? 1 - hscroll : 0;
  EMACS_INT selective
    = (INTEGERP (BVAR (current_buffer, selective_display))
       ? XINT (BVAR (current_buffer, selective_display))
       : !NILP (BVAR (current_buffer, selective_display)) ? -1 : 0);
  Lisp_Object window;
  EMACS_INT start_hpos = 0;
  int did_motion;
  /* This is the object we use for fetching character properties.  */
  Lisp_Object text_prop_object;

  XSETWINDOW (window, w);

  /* If the window contains this buffer, use it for getting text properties.
     Otherwise use the current buffer as arg for doing that.  */
  if (EQ (w->buffer, Fcurrent_buffer ()))
    text_prop_object = window;
  else
    text_prop_object = Fcurrent_buffer ();

  if (vpos >= vtarget)
    {
      /* To move upward, go a line at a time until
	 we have gone at least far enough.  */

      first = 1;

      while ((vpos > vtarget || first) && from > BEGV)
	{
	  Lisp_Object propval;

	  prevline = find_next_newline_no_quit (from - 1, -1);
	  while (prevline > BEGV
		 && ((selective > 0
		      && indented_beyond_p (prevline,
					    CHAR_TO_BYTE (prevline),
					    selective))
		     /* Watch out for newlines with `invisible' property.
			When moving upward, check the newline before.  */
		     || (propval = Fget_char_property (make_number (prevline - 1),
						       Qinvisible,
						       text_prop_object),
			 TEXT_PROP_MEANS_INVISIBLE (propval))))
	    prevline = find_next_newline_no_quit (prevline - 1, -1);
	  pos = *compute_motion (prevline, 0,
				 lmargin + (prevline == BEG ? start_hpos : 0),
				 0,
				 from,
				 /* Don't care for VPOS...  */
				 1 << (BITS_PER_SHORT - 1),
				 /* ... nor HPOS.  */
				 1 << (BITS_PER_SHORT - 1),
				 -1, hscroll,
				 /* This compensates for start_hpos
				    so that a tab as first character
				    still occupies 8 columns.  */
				 (prevline == BEG ? -start_hpos : 0),
				 w);
	  vpos -= pos.vpos;
	  first = 0;
	  from = prevline;
	}

      /* If we made exactly the desired vertical distance,
	 or if we hit beginning of buffer,
	 return point found */
      if (vpos >= vtarget)
	{
	  val_vmotion.bufpos = from;
	  val_vmotion.bytepos = CHAR_TO_BYTE (from);
	  val_vmotion.vpos = vpos;
	  val_vmotion.hpos = lmargin;
	  val_vmotion.contin = 0;
	  val_vmotion.prevhpos = 0;
	  val_vmotion.ovstring_chars_done = 0;
	  val_vmotion.tab_offset = 0; /* For accumulating tab offset.  */
	  return &val_vmotion;
	}

      /* Otherwise find the correct spot by moving down */
    }
  /* Moving downward is simple, but must calculate from beg of line
     to determine hpos of starting point */
  from_byte = CHAR_TO_BYTE (from);
  if (from > BEGV && FETCH_BYTE (from_byte - 1) != '\n')
    {
      Lisp_Object propval;

      prevline = find_next_newline_no_quit (from, -1);
      while (prevline > BEGV
	     && ((selective > 0
		  && indented_beyond_p (prevline,
					CHAR_TO_BYTE (prevline),
					selective))
		 /* Watch out for newlines with `invisible' property.
		    When moving downward, check the newline after.  */
		 || (propval = Fget_char_property (make_number (prevline),
						   Qinvisible,
						   text_prop_object),
		     TEXT_PROP_MEANS_INVISIBLE (propval))))
	prevline = find_next_newline_no_quit (prevline - 1, -1);
      pos = *compute_motion (prevline, 0,
			     lmargin + (prevline == BEG
					? start_hpos : 0),
			     0,
			     from,
			     /* Don't care for VPOS...  */
			     1 << (BITS_PER_SHORT - 1),
			     /* ... nor HPOS.  */
			     1 << (BITS_PER_SHORT - 1),
			     -1, hscroll,
			     (prevline == BEG ? -start_hpos : 0),
			     w);
      did_motion = 1;
    }
  else
    {
      pos.hpos = lmargin + (from == BEG ? start_hpos : 0);
      pos.vpos = 0;
      pos.tab_offset = 0;
      did_motion = 0;
    }
  return compute_motion (from, vpos, pos.hpos, did_motion,
			 ZV, vtarget, - (1 << (BITS_PER_SHORT - 1)),
			 -1, hscroll,
			 pos.tab_offset - (from == BEG ? start_hpos : 0),
			 w);
}

DEFUN ("vertical-motion", Fvertical_motion, Svertical_motion, 1, 2, 0,
       doc: /* Move point to start of the screen line LINES lines down.
If LINES is negative, this means moving up.

This function is an ordinary cursor motion function
which calculates the new position based on how text would be displayed.
The new position may be the start of a line,
or just the start of a continuation line.
The function returns number of screen lines moved over;
that usually equals LINES, but may be closer to zero
if beginning or end of buffer was reached.

The optional second argument WINDOW specifies the window to use for
parameters such as width, horizontal scrolling, and so on.
The default is to use the selected window's parameters.

LINES can optionally take the form (COLS . LINES), in which case
the motion will not stop at the start of a screen line but on
its column COLS (if such exists on that line, that is).

`vertical-motion' always uses the current buffer,
regardless of which buffer is displayed in WINDOW.
This is consistent with other cursor motion functions
and makes it possible to use `vertical-motion' in any buffer,
whether or not it is currently displayed in some window.  */)
  (Lisp_Object lines, Lisp_Object window)
{
  struct it it;
  struct text_pos pt;
  struct window *w;
  Lisp_Object old_buffer;
  EMACS_INT old_charpos IF_LINT (= 0), old_bytepos IF_LINT (= 0);
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object lcols = Qnil;
  double cols IF_LINT (= 0);
  void *itdata = NULL;

  /* Allow LINES to be of the form (HPOS . VPOS) aka (COLUMNS . LINES).  */
  if (CONSP (lines) && (NUMBERP (XCAR (lines))))
    {
      lcols = XCAR (lines);
      cols = INTEGERP (lcols) ? (double) XINT (lcols) : XFLOAT_DATA (lcols);
      lines = XCDR (lines);
    }

  CHECK_NUMBER (lines);
  if (! NILP (window))
    CHECK_WINDOW (window);
  else
    window = selected_window;
  w = XWINDOW (window);

  old_buffer = Qnil;
  GCPRO3 (old_buffer, old_charpos, old_bytepos);
  if (XBUFFER (w->buffer) != current_buffer)
    {
      /* Set the window's buffer temporarily to the current buffer.  */
      old_buffer = w->buffer;
      old_charpos = XMARKER (w->pointm)->charpos;
      old_bytepos = XMARKER (w->pointm)->bytepos;
      XSETBUFFER (w->buffer, current_buffer);
      set_marker_both
	(w->pointm, w->buffer, BUF_PT (current_buffer), BUF_PT_BYTE (current_buffer));
    }

  if (noninteractive)
    {
      struct position pos;
      pos = *vmotion (PT, XINT (lines), w);
      SET_PT_BOTH (pos.bufpos, pos.bytepos);
    }
  else
    {
      EMACS_INT it_start;
      int first_x, it_overshoot_count = 0;
      int overshoot_handled = 0;

      itdata = bidi_shelve_cache ();
      SET_TEXT_POS (pt, PT, PT_BYTE);
      start_display (&it, w, pt);
      first_x = it.first_visible_x;
      it_start = IT_CHARPOS (it);

      /* See comments below for why we calculate this.  */
      if (it.cmp_it.id >= 0)
	it_overshoot_count = 0;
      else if (it.method == GET_FROM_STRING)
	{
	  const char *s = SSDATA (it.string);
	  const char *e = s + SBYTES (it.string);
	  while (s < e)
	    {
	      if (*s++ == '\n')
		it_overshoot_count++;
	    }
	  if (!it_overshoot_count)
	    it_overshoot_count = -1;
	}
      else
	it_overshoot_count =
	  !(it.method == GET_FROM_IMAGE || it.method == GET_FROM_STRETCH);

      /* Scan from the start of the line containing PT.  If we don't
	 do this, we start moving with IT->current_x == 0, while PT is
	 really at some x > 0.  */
      reseat_at_previous_visible_line_start (&it);
      it.current_x = it.hpos = 0;
      if (IT_CHARPOS (it) != PT)
	/* We used to temporarily disable selective display here; the
	   comment said this is "so we don't move too far" (2005-01-19
	   checkin by kfs).  But this does nothing useful that I can
	   tell, and it causes Bug#2694 .  -- cyd */
	move_it_to (&it, PT, -1, -1, -1, MOVE_TO_POS);

      /* IT may move too far if truncate-lines is on and PT lies
	 beyond the right margin.  IT may also move too far if the
	 starting point is on a Lisp string that has embedded
	 newlines.  In these cases, backtrack.  */
      if (IT_CHARPOS (it) > it_start)
	{
	  /* We need to backtrack also if the Lisp string contains no
	     newlines, but there is a newline right after it.  In this
	     case, IT overshoots if there is an after-string just
	     before the newline.  */
	  if (it_overshoot_count < 0
	      && it.method == GET_FROM_BUFFER
	      && it.c == '\n')
	    it_overshoot_count = 1;
	  if (it_overshoot_count > 0)
	    move_it_by_lines (&it, -it_overshoot_count);

	  overshoot_handled = 1;
	}
      if (XINT (lines) <= 0)
	{
	  it.vpos = 0;
	  /* Do this even if LINES is 0, so that we move back to the
	     beginning of the current line as we ought.  */
	  if (XINT (lines) == 0 || IT_CHARPOS (it) > 0)
	    move_it_by_lines (&it, max (INT_MIN, XINT (lines)));
	}
      else if (overshoot_handled)
	{
	  it.vpos = 0;
	  move_it_by_lines (&it, min (INT_MAX, XINT (lines)));
	}
      else
	{
	  /* Otherwise, we are at the first row occupied by PT, which
	     might span multiple screen lines (e.g., if it's on a
	     multi-line display string).  We want to start from the
	     last line that it occupies.  */
	  if (it_start < ZV)
	    {
	      while (IT_CHARPOS (it) <= it_start)
		{
		  it.vpos = 0;
		  move_it_by_lines (&it, 1);
		}
	      if (XINT (lines) > 1)
		move_it_by_lines (&it, min (INT_MAX, XINT (lines) - 1));
	    }
	  else
	    {
	      it.vpos = 0;
	      move_it_by_lines (&it, min (INT_MAX, XINT (lines)));
	    }
	}

      /* Move to the goal column, if one was specified.  */
      if (!NILP (lcols))
	{
	  /* If the window was originally hscrolled, move forward by
	     the hscrolled amount first.  */
	  if (first_x > 0)
	    {
	      move_it_in_display_line (&it, ZV, first_x, MOVE_TO_X);
	      it.current_x = 0;
	    }
	  move_it_in_display_line
	    (&it, ZV,
	     (int)(cols * FRAME_COLUMN_WIDTH (XFRAME (w->frame)) + 0.5),
	     MOVE_TO_X);
	}

      SET_PT_BOTH (IT_CHARPOS (it), IT_BYTEPOS (it));
      bidi_unshelve_cache (itdata, 0);
    }

  if (BUFFERP (old_buffer))
    {
      w->buffer = old_buffer;
      set_marker_both (w->pointm, w->buffer, old_charpos, old_bytepos);
    }

  RETURN_UNGCPRO (make_number (it.vpos));
}



/* File's initialization.  */

void
syms_of_indent (void)
{
  DEFVAR_BOOL ("indent-tabs-mode", indent_tabs_mode,
	       doc: /* *Indentation can insert tabs if this is non-nil.  */);
  indent_tabs_mode = 1;

  defsubr (&Scurrent_indentation);
  defsubr (&Sindent_to);
  defsubr (&Scurrent_column);
  defsubr (&Smove_to_column);
  defsubr (&Svertical_motion);
  defsubr (&Scompute_motion);
}
