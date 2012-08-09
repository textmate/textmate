/* Fringe handling (split from xdisp.c).
   Copyright (C) 1985-1988, 1993-1995, 1997-2012  Free Software Foundation, Inc.

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
#include "frame.h"
#include "window.h"
#include "dispextern.h"
#include "buffer.h"
#include "blockinput.h"
#include "termhooks.h"

#ifdef HAVE_WINDOW_SYSTEM

/* Fringe bitmaps are represented in three different ways:

   Logical bitmaps are used internally to denote things like
   'end-of-buffer', 'left-truncation', 'overlay-arrow', etc.

   Physical bitmaps specify the visual appearance of the bitmap,
   e.g. 'bottom-left-angle', 'left-arrow', 'left-triangle', etc.
   User defined bitmaps are physical bitmaps.

   Internally, fringe bitmaps for a specific display row are
   represented as a simple integer that is used as an index
   into the table of all defined bitmaps.  This index is stored
   in the `fringe' property of the physical bitmap symbol.

   Logical bitmaps are mapped to physical bitmaps through the
   buffer-local `fringe-indicator-alist' variable.

   Each element of this alist is a cons (LOGICAL . PHYSICAL)
   mapping a logical bitmap to a physical bitmap.
   PHYSICAL is either a symbol to use in both left and right fringe,
   or a cons of two symbols (LEFT . RIGHT) denoting different
   bitmaps to use in left and right fringe.

   LOGICAL is first looked up in the window's buffer's buffer-local
   value of the fringe-indicator-alist variable, and if not present,
   in the global value of fringe-indicator-alist.

   If LOGICAL is not present in either alist, or the PHYSICAL value
   found is nil, no bitmap is shown for the logical bitmap.

   The `left-fringe' and `right-fringe' display properties
   must specify physical bitmap symbols.
*/

static Lisp_Object Qtruncation, Qcontinuation, Qoverlay_arrow;
static Lisp_Object Qempty_line, Qtop_bottom;
static Lisp_Object Qhollow_small;

enum fringe_bitmap_align
{
  ALIGN_BITMAP_CENTER = 0,
  ALIGN_BITMAP_TOP,
  ALIGN_BITMAP_BOTTOM
};

struct fringe_bitmap
{
  unsigned short *bits;
  unsigned height : 8;
  unsigned width : 8;
  unsigned period : 8;
  unsigned align : 2;
  unsigned dynamic : 1;
};


/***********************************************************************
			       Fringe bitmaps
 ***********************************************************************/

/* Undefined bitmap.  A question mark.  */
/*
  ..xxxx..
  .xxxxxx.
  xx....xx
  xx....xx
  ....xx..
  ...xx...
  ...xx...
  ........
  ...xx...
  ...xx...
*/
static unsigned short question_mark_bits[] = {
  0x3c, 0x7e, 0x7e, 0x0c, 0x18, 0x18, 0x00, 0x18, 0x18};

/* An arrow like this: `<-'.  */
/*
  ...xx...
  ..xx....
  .xx.....
  xxxxxx..
  xxxxxx..
  .xx.....
  ..xx....
  ...xx...
*/
static unsigned short left_arrow_bits[] = {
   0x18, 0x30, 0x60, 0xfc, 0xfc, 0x60, 0x30, 0x18};


/* Right truncation arrow bitmap `->'.  */
/*
  ...xx...
  ....xx..
  .....xx.
  ..xxxxxx
  ..xxxxxx
  .....xx.
  ....xx..
  ...xx...
*/
static unsigned short right_arrow_bits[] = {
   0x18, 0x0c, 0x06, 0x3f, 0x3f, 0x06, 0x0c, 0x18};


/* Up arrow bitmap.  */
/*
  ...xx...
  ..xxxx..
  .xxxxxx.
  xxxxxxxx
  ...xx...
  ...xx...
  ...xx...
  ...xx...
*/
static unsigned short up_arrow_bits[] = {
   0x18, 0x3c, 0x7e, 0xff, 0x18, 0x18, 0x18, 0x18};


/* Down arrow bitmap.  */
/*
  ...xx...
  ...xx...
  ...xx...
  ...xx...
  xxxxxxxx
  .xxxxxx.
  ..xxxx..
  ...xx...
*/
static unsigned short down_arrow_bits[] = {
   0x18, 0x18, 0x18, 0x18, 0xff, 0x7e, 0x3c, 0x18};

/* Marker for continuation lines.  */
/*
  ..xxxx..
  .xxxxx..
  xx......
  xxx..x..
  xxxxxx..
  .xxxxx..
  ..xxxx..
  .xxxxx..
*/
static unsigned short left_curly_arrow_bits[] = {
   0x3c, 0x7c, 0xc0, 0xe4, 0xfc, 0x7c, 0x3c, 0x7c};

/* Marker for continued lines.  */
/*
  ..xxxx..
  ..xxxxx.
  ......xx
  ..x..xxx
  ..xxxxxx
  ..xxxxx.
  ..xxxx..
  ..xxxxx.
*/
static unsigned short right_curly_arrow_bits[] = {
   0x3c, 0x3e, 0x03, 0x27, 0x3f, 0x3e, 0x3c, 0x3e};

/* Reverse Overlay arrow bitmap.  A triangular arrow.  */
/*
  ......xx
  ....xxxx
  ...xxxxx
  ..xxxxxx
  ..xxxxxx
  ...xxxxx
  ....xxxx
  ......xx
*/
static unsigned short left_triangle_bits[] = {
   0x03, 0x0f, 0x1f, 0x3f, 0x3f, 0x1f, 0x0f, 0x03};

/* Overlay arrow bitmap.  A triangular arrow.  */
/*
  xx......
  xxxx....
  xxxxx...
  xxxxxx..
  xxxxxx..
  xxxxx...
  xxxx....
  xx......
*/
static unsigned short right_triangle_bits[] = {
   0xc0, 0xf0, 0xf8, 0xfc, 0xfc, 0xf8, 0xf0, 0xc0};

/* First line bitmap.  An top-left angle.  */
/*
  xxxxxx..
  xxxxxx..
  xx......
  xx......
  xx......
  xx......
  xx......
  ........
*/
static unsigned short top_left_angle_bits[] = {
   0xfc, 0xfc, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0x00};

/* First line bitmap.  An right-up angle.  */
/*
  ..xxxxxx
  ..xxxxxx
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ........
*/
static unsigned short top_right_angle_bits[] = {
   0x3f, 0x3f, 0x03, 0x03, 0x03, 0x03, 0x03, 0x00};

/* Last line bitmap.  An left-down angle.  */
/*
  ........
  xx......
  xx......
  xx......
  xx......
  xx......
  xxxxxx..
  xxxxxx..
*/
static unsigned short bottom_left_angle_bits[] = {
   0x00, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xfc, 0xfc};

/* Last line bitmap.  An right-down angle.  */
/*
  ........
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ..xxxxxx
  ..xxxxxx
*/
static unsigned short bottom_right_angle_bits[] = {
   0x00, 0x03, 0x03, 0x03, 0x03, 0x03, 0x3f, 0x3f};

/* First/last line bitmap.  An left bracket.  */
/*
  xxxxxx..
  xxxxxx..
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xxxxxx..
  xxxxxx..
*/
static unsigned short left_bracket_bits[] = {
   0xfc, 0xfc, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xfc, 0xfc};

/* First/last line bitmap.  An right bracket.  */
/*
  ..xxxxxx
  ..xxxxxx
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ..xxxxxx
  ..xxxxxx
*/
static unsigned short right_bracket_bits[] = {
  0x3f, 0x3f, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x3f, 0x3f};

/* Filled box cursor bitmap.  A filled box; max 13 pixels high.  */
/*
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
*/
static unsigned short filled_rectangle_bits[] = {
   0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe};

/* Hollow box cursor bitmap.  A hollow box; max 13 pixels high.  */
/*
  xxxxxxx.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  xxxxxxx.
*/
static unsigned short hollow_rectangle_bits[] = {
   0xfe, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0xfe};

/* Hollow square bitmap.  */
/*
  .xxxxxx.
  .x....x.
  .x....x.
  .x....x.
  .x....x.
  .xxxxxx.
*/
static unsigned short hollow_square_bits[] = {
   0x7e, 0x42, 0x42, 0x42, 0x42, 0x7e};

/* Filled square bitmap.  */
/*
  .xxxxxx.
  .xxxxxx.
  .xxxxxx.
  .xxxxxx.
  .xxxxxx.
  .xxxxxx.
*/
static unsigned short filled_square_bits[] = {
   0x7e, 0x7e, 0x7e, 0x7e, 0x7e, 0x7e};

/* Bar cursor bitmap.  A vertical bar; max 13 pixels high.  */
/*
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
*/
static unsigned short vertical_bar_bits[] = {
   0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0};

/* HBar cursor bitmap.  A horizontal bar; 2 pixels high.  */
/*
  xxxxxxx.
  xxxxxxx.
*/
static unsigned short horizontal_bar_bits[] = {
  0xfe, 0xfe};


/* Bitmap drawn to indicate lines not displaying text if
   `indicate-empty-lines' is non-nil.  */
/*
  ........
  ..xxxx..
  ........
  ........
  ..xxxx..
  ........
*/
static unsigned short empty_line_bits[] = {
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00};


#define BYTES_PER_BITMAP_ROW  (sizeof (unsigned short))
#define STANDARD_BITMAP_HEIGHT(bits) (sizeof (bits)/BYTES_PER_BITMAP_ROW)
#define FRBITS(bits)  bits, STANDARD_BITMAP_HEIGHT (bits)

/* NOTE:  The order of these bitmaps must match the sequence
   used in fringe.el to define the corresponding symbols.  */

static struct fringe_bitmap standard_bitmaps[] =
{
  { NULL, 0, 0, 0, 0, 0 }, /* NO_FRINGE_BITMAP */
  { FRBITS (question_mark_bits),      8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (left_arrow_bits),         8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (right_arrow_bits),        8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (up_arrow_bits),           8, 0, ALIGN_BITMAP_TOP,    0 },
  { FRBITS (down_arrow_bits),         8, 0, ALIGN_BITMAP_BOTTOM, 0 },
  { FRBITS (left_curly_arrow_bits),   8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (right_curly_arrow_bits),  8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (left_triangle_bits),      8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (right_triangle_bits),     8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (top_left_angle_bits),     8, 0, ALIGN_BITMAP_TOP,    0 },
  { FRBITS (top_right_angle_bits),    8, 0, ALIGN_BITMAP_TOP,    0 },
  { FRBITS (bottom_left_angle_bits),  8, 0, ALIGN_BITMAP_BOTTOM, 0 },
  { FRBITS (bottom_right_angle_bits), 8, 0, ALIGN_BITMAP_BOTTOM, 0 },
  { FRBITS (left_bracket_bits),       8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (right_bracket_bits),      8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (filled_rectangle_bits),   8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (hollow_rectangle_bits),   8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (filled_square_bits),      8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (hollow_square_bits),      8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (vertical_bar_bits),       8, 0, ALIGN_BITMAP_CENTER, 0 },
  { FRBITS (horizontal_bar_bits),     8, 0, ALIGN_BITMAP_BOTTOM, 0 },
  { FRBITS (empty_line_bits),         8, 3, ALIGN_BITMAP_TOP,    0 },
};

#define NO_FRINGE_BITMAP 0
#define UNDEF_FRINGE_BITMAP 1
#define MAX_STANDARD_FRINGE_BITMAPS (sizeof (standard_bitmaps)/sizeof (standard_bitmaps[0]))

static struct fringe_bitmap **fringe_bitmaps;
static Lisp_Object *fringe_faces;
static int max_fringe_bitmaps;

#ifndef HAVE_NS
static
#endif
int max_used_fringe_bitmap = MAX_STANDARD_FRINGE_BITMAPS;


/* Lookup bitmap number for symbol BITMAP.
   Return 0 if not a bitmap.  */

int
lookup_fringe_bitmap (Lisp_Object bitmap)
{
  int bn;

  bitmap = Fget (bitmap, Qfringe);
  if (!INTEGERP (bitmap))
    return 0;

  bn = XINT (bitmap);
  if (bn > NO_FRINGE_BITMAP
      && bn < max_used_fringe_bitmap
      && (bn < MAX_STANDARD_FRINGE_BITMAPS
	  || fringe_bitmaps[bn] != NULL))
    return bn;

  return 0;
}

/* Get fringe bitmap name for bitmap number BN.

   Found by traversing Vfringe_bitmaps comparing BN to the
   fringe property for each symbol.

   Return BN if not found in Vfringe_bitmaps.  */

static Lisp_Object
get_fringe_bitmap_name (int bn)
{
  Lisp_Object bitmaps;
  Lisp_Object num;

  /* Zero means no bitmap -- return nil.  */
  if (bn <= 0)
    return Qnil;

  bitmaps = Vfringe_bitmaps;
  num = make_number (bn);

  while (CONSP (bitmaps))
    {
      Lisp_Object bitmap = XCAR (bitmaps);
      if (EQ (num, Fget (bitmap, Qfringe)))
	return bitmap;
      bitmaps = XCDR (bitmaps);
    }

  return num;
}

/* Get fringe bitmap data for bitmap number BN.  */

static struct fringe_bitmap *
get_fringe_bitmap_data (int bn)
{
  struct fringe_bitmap *fb;

  fb = fringe_bitmaps[bn];
  if (fb == NULL)
    fb = &standard_bitmaps[bn < MAX_STANDARD_FRINGE_BITMAPS
			   ? bn : UNDEF_FRINGE_BITMAP];

  return fb;
}

/* Draw the bitmap WHICH in one of the left or right fringes of
   window W.  ROW is the glyph row for which to display the bitmap; it
   determines the vertical position at which the bitmap has to be
   drawn.
   LEFT_P is 1 for left fringe, 0 for right fringe.
*/

static void
draw_fringe_bitmap_1 (struct window *w, struct glyph_row *row, int left_p, int overlay, int which)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct draw_fringe_bitmap_params p;
  struct fringe_bitmap *fb;
  int period;
  int face_id = DEFAULT_FACE_ID;
  int offset, header_line_height;

  p.overlay_p = (overlay & 1) == 1;
  p.cursor_p = (overlay & 2) == 2;

  if (which != NO_FRINGE_BITMAP)
    {
      offset = 0;
    }
  else if (left_p)
    {
      which = row->left_fringe_bitmap;
      face_id = row->left_fringe_face_id;
      offset = row->left_fringe_offset;
    }
  else
    {
      which = row->right_fringe_bitmap;
      face_id = row->right_fringe_face_id;
      offset = row->right_fringe_offset;
    }

  if (face_id == DEFAULT_FACE_ID)
    {
      Lisp_Object face = fringe_faces[which];
      face_id = NILP (face) ? lookup_named_face (f, Qfringe, 0)
	: lookup_derived_face (f, face, FRINGE_FACE_ID, 0);
      if (face_id < 0)
	face_id = FRINGE_FACE_ID;
    }

  fb = get_fringe_bitmap_data (which);

  period = fb->period;

  /* Convert row to frame coordinates.  */
  p.y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y) + offset;

  p.which = which;
  p.bits = fb->bits;
  p.wd = fb->width;

  p.h = fb->height;
  p.dh = (period > 0 ? (p.y % period) : 0);
  p.h -= p.dh;

  /* Adjust y to the offset in the row to start drawing the bitmap.  */
  switch (fb->align)
    {
    case ALIGN_BITMAP_CENTER:
      p.y += (row->height - p.h) / 2;
      break;
    case ALIGN_BITMAP_BOTTOM:
      p.y += (row->visible_height - p.h);
      break;
    case ALIGN_BITMAP_TOP:
      break;
    }

  p.face = FACE_FROM_ID (f, face_id);

  if (p.face == NULL)
    {
      /* This could happen after clearing face cache.
	 But it shouldn't happen anymore.  ++kfs */
      return;
    }

  PREPARE_FACE_FOR_DISPLAY (f, p.face);

  /* Clear left fringe if no bitmap to draw or if bitmap doesn't fill
     the fringe.  */
  p.bx = -1;
  header_line_height = WINDOW_HEADER_LINE_HEIGHT (w);
  p.by = WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height, row->y));
  p.ny = row->visible_height;
  if (left_p)
    {
      int wd = WINDOW_LEFT_FRINGE_WIDTH (w);
      int x = window_box_left (w, (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
				   ? LEFT_MARGIN_AREA
				   : TEXT_AREA));
      if (p.wd > wd)
	p.wd = wd;
      p.x = x - p.wd - (wd - p.wd) / 2;

      if (p.wd < wd || p.y > p.by || p.y + p.h < p.by + p.ny)
	{
	  /* If W has a vertical border to its left, don't draw over it.  */
	  wd -= ((!WINDOW_LEFTMOST_P (w)
		  && !WINDOW_HAS_VERTICAL_SCROLL_BAR (w))
		 ? 1 : 0);
	  p.bx = x - wd;
	  p.nx = wd;
	}
    }
  else
    {
      int x = window_box_right (w,
				(WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
				 ? RIGHT_MARGIN_AREA
				 : TEXT_AREA));
      int wd = WINDOW_RIGHT_FRINGE_WIDTH (w);
      if (p.wd > wd)
	p.wd = wd;
      p.x = x + (wd - p.wd) / 2;
      /* Clear right fringe if no bitmap to draw of if bitmap doesn't fill
	 the fringe.  */
      if (p.wd < wd || p.y > p.by || p.y + p.h < p.by + p.ny)
	{
	  p.bx = x;
	  p.nx = wd;
	}
    }

  FRAME_RIF (f)->draw_fringe_bitmap (w, row, &p);
}

static int
get_logical_cursor_bitmap (struct window *w, Lisp_Object cursor)
{
  Lisp_Object cmap, bm = Qnil;

  if ((cmap = BVAR (XBUFFER (w->buffer), fringe_cursor_alist)), !NILP (cmap))
    {
      bm = Fassq (cursor, cmap);
      if (CONSP (bm))
	{
	  if ((bm = XCDR (bm)), NILP (bm))
	    return NO_FRINGE_BITMAP;
	  return lookup_fringe_bitmap (bm);
	}
    }
  if (EQ (cmap, BVAR (&buffer_defaults, fringe_cursor_alist)))
    return NO_FRINGE_BITMAP;
  bm = Fassq (cursor, BVAR (&buffer_defaults, fringe_cursor_alist));
  if (!CONSP (bm) || ((bm = XCDR (bm)), NILP (bm)))
    return NO_FRINGE_BITMAP;
  return lookup_fringe_bitmap (bm);
}

static int
get_logical_fringe_bitmap (struct window *w, Lisp_Object bitmap, int right_p, int partial_p)
{
  Lisp_Object cmap, bm1 = Qnil, bm2 = Qnil, bm;
  int ln1 = 0, ln2 = 0;
  int ix1 = right_p;
  int ix2 = ix1 + (partial_p ? 2 : 0);

  /* Lookup in buffer-local fringe-indicator-alist before global alist.

     Elements are:
	BITMAP		-- use for all
	(L R)		-- use for left right (whether partial or not)
	(L R PL PR)	-- use for left right partial-left partial-right
	If any value in local binding is not present or t, use global value.

     If partial, lookup partial bitmap in default value if not found here.
     If not partial, or no partial spec is present, use non-partial bitmap.  */

  if ((cmap = BVAR (XBUFFER (w->buffer), fringe_indicator_alist)), !NILP (cmap))
    {
      bm1 = Fassq (bitmap, cmap);
      if (CONSP (bm1))
	{
	  if ((bm1 = XCDR (bm1)), NILP (bm1))
	    return NO_FRINGE_BITMAP;
	  if (CONSP (bm1))
	    {
	      ln1 = XINT (Flength (bm1));
	      if (partial_p)
		{
		  if (ln1 > ix2)
		    {
		      bm = Fnth (make_number (ix2), bm1);
		      if (!EQ (bm, Qt))
			goto found;
		    }
		}
	      else
		{
		  if (ln1 > ix1)
		    {
		      bm = Fnth (make_number (ix1), bm1);
		      if (!EQ (bm, Qt))
			goto found;
		    }
		}
	    }
	  else if ((bm = bm1, !EQ (bm, Qt)))
	    goto found;
	}
    }

  if (!EQ (cmap, BVAR (&buffer_defaults, fringe_indicator_alist))
      && !NILP (BVAR (&buffer_defaults, fringe_indicator_alist)))
    {
      bm2 = Fassq (bitmap, BVAR (&buffer_defaults, fringe_indicator_alist));
      if (CONSP (bm2))
	{
	  if ((bm2 = XCDR (bm2)), !NILP (bm2))
	    {
	      if (CONSP (bm2))
		{
		  ln2 = XINT (Flength (bm2));
		  if (partial_p)
		    {
		      if (ln2 > ix2)
			{
			  bm = Fnth (make_number (ix2), bm2);
			  if (!EQ (bm, Qt))
			    goto found;
			}
		    }
		}
	    }
	}
    }

  if (ln1 > ix1)
    {
      bm = Fnth (make_number (ix1), bm1);
      if (!EQ (bm, Qt))
	goto found;
    }

  if (ln2 > ix1)
    {
      bm = Fnth (make_number (ix1), bm2);
      if (!EQ (bm, Qt))
	goto found;
      return NO_FRINGE_BITMAP;
    }
  else if ((bm = bm2, NILP (bm)))
    return NO_FRINGE_BITMAP;

 found:
  return lookup_fringe_bitmap (bm);
}


void
draw_fringe_bitmap (struct window *w, struct glyph_row *row, int left_p)
{
  int overlay = 0;

  if (left_p == row->reversed_p && row->cursor_in_fringe_p)
    {
      Lisp_Object cursor = Qnil;

      switch (w->phys_cursor_type)
	{
	case HOLLOW_BOX_CURSOR:
	  if (row->visible_height >= STANDARD_BITMAP_HEIGHT (hollow_rectangle_bits))
	    cursor = Qhollow;
	  else
	    cursor = Qhollow_small;
	  break;
	case FILLED_BOX_CURSOR:
	  cursor = Qbox;
	  break;
	case BAR_CURSOR:
	  cursor = Qbar;
	  break;
	case HBAR_CURSOR:
	  cursor = Qhbar;
	  break;
	case NO_CURSOR:
	default:
	  w->phys_cursor_on_p = 0;
	  row->cursor_in_fringe_p = 0;
	  break;
	}
      if (!NILP (cursor))
	{
	  int bm = get_logical_cursor_bitmap (w, cursor);
	  if (bm != NO_FRINGE_BITMAP)
	    {
	      draw_fringe_bitmap_1 (w, row, left_p, 2, bm);
	      overlay = EQ (cursor, Qbox) ? 3 : 1;
	    }
	}
    }

  draw_fringe_bitmap_1 (w, row, left_p, overlay, NO_FRINGE_BITMAP);

  if (left_p && row->overlay_arrow_bitmap != NO_FRINGE_BITMAP)
    draw_fringe_bitmap_1 (w, row, 1, 1, row->overlay_arrow_bitmap);
}


/* Draw fringe bitmaps for glyph row ROW on window W.  Call this
   function with input blocked.  */

void
draw_row_fringe_bitmaps (struct window *w, struct glyph_row *row)
{
  xassert (interrupt_input_blocked);

  /* If row is completely invisible, because of vscrolling, we
     don't have to draw anything.  */
  if (row->visible_height <= 0)
    return;

  if (WINDOW_LEFT_FRINGE_WIDTH (w) != 0)
    draw_fringe_bitmap (w, row, 1);

  if (WINDOW_RIGHT_FRINGE_WIDTH (w) != 0)
    draw_fringe_bitmap (w, row, 0);
}

/* Draw the fringes of window W.  Only fringes for rows marked for
   update in redraw_fringe_bitmaps_p are drawn.

   Return >0 if left or right fringe was redrawn in any way.

   If NO_FRINGE is non-zero, also return >0 if either fringe has zero width.

   A return value >0 indicates that the vertical line between windows
   needs update (as it may be drawn in the fringe).
*/

int
draw_window_fringes (struct window *w, int no_fringe)
{
  struct glyph_row *row;
  int yb = window_text_bottom_y (w);
  int nrows = w->current_matrix->nrows;
  int y, rn;
  int updated = 0;

  if (w->pseudo_window_p)
    return 0;

  /* Must draw line if no fringe */
  if (no_fringe
      && (WINDOW_LEFT_FRINGE_WIDTH (w) == 0
	  || WINDOW_RIGHT_FRINGE_WIDTH (w) == 0))
    updated++;

  for (y = w->vscroll, rn = 0, row = w->current_matrix->rows;
       y < yb && rn < nrows;
       y += row->height, ++row, ++rn)
    {
      if (!row->redraw_fringe_bitmaps_p)
	continue;
      draw_row_fringe_bitmaps (w, row);
      row->redraw_fringe_bitmaps_p = 0;
      updated++;
    }

  return updated;
}


/* Recalculate the bitmaps to show in the fringes of window W.
   Only mark rows with modified bitmaps for update in redraw_fringe_bitmaps_p.

   If KEEP_CURRENT_P is 0, update current_matrix too.  */

int
update_window_fringes (struct window *w, int keep_current_p)
{
  struct glyph_row *row, *cur = 0;
  int yb = window_text_bottom_y (w);
  int rn, nrows = w->current_matrix->nrows;
  int y;
  int redraw_p = 0;
  Lisp_Object boundary_top = Qnil, boundary_bot = Qnil;
  Lisp_Object arrow_top = Qnil, arrow_bot = Qnil;
  Lisp_Object empty_pos;
  Lisp_Object ind = Qnil;
#define MAX_BITMAP_CACHE (8*4)
  int bitmap_cache[MAX_BITMAP_CACHE];
  int top_ind_rn, bot_ind_rn;
  int top_ind_min_y, bot_ind_max_y;

  /* top_ind_rn is set to a nonnegative value whenever
     row->indicate_bob_p is set, so it's OK that top_row_ends_at_zv_p
     is not initialized here.  Similarly for bot_ind_rn,
     row->indicate_eob_p and bot_row_ends_at_zv_p.  */
  int top_row_ends_at_zv_p IF_LINT (= 0), bot_row_ends_at_zv_p IF_LINT (= 0);

  if (w->pseudo_window_p)
    return 0;

  if (!MINI_WINDOW_P (w)
      && (ind = BVAR (XBUFFER (w->buffer), indicate_buffer_boundaries), !NILP (ind)))
    {
      if (EQ (ind, Qleft) || EQ (ind, Qright))
	boundary_top = boundary_bot = arrow_top = arrow_bot = ind;
      else if (CONSP (ind) && CONSP (XCAR (ind)))
	{
	  Lisp_Object pos;
	  if (pos = Fassq (Qt, ind), !NILP (pos))
	    boundary_top = boundary_bot = arrow_top = arrow_bot = XCDR (pos);
	  if (pos = Fassq (Qtop, ind), !NILP (pos))
	    boundary_top = XCDR (pos);
	  if (pos = Fassq (Qbottom, ind), !NILP (pos))
	    boundary_bot = XCDR (pos);
	  if (pos = Fassq (Qup, ind), !NILP (pos))
	    arrow_top = XCDR (pos);
	  if (pos = Fassq (Qdown, ind), !NILP (pos))
	    arrow_bot = XCDR (pos);
	}
      else
	/* Anything else means boundary on left and no arrows.  */
	boundary_top = boundary_bot = Qleft;
    }

  top_ind_rn = bot_ind_rn = -1;
  if (!NILP (ind))
    {
      for (y = w->vscroll, rn = 0;
	   y < yb && rn < nrows;
	   y += row->height, ++rn)
	{
	  row = w->desired_matrix->rows + rn;
	  if (!row->enabled_p)
	    row = w->current_matrix->rows + rn;

	  row->indicate_bob_p = row->indicate_top_line_p = 0;
	  row->indicate_eob_p = row->indicate_bottom_line_p = 0;

	  if (!row->mode_line_p)
	    {
	      if (top_ind_rn < 0 && row->visible_height > 0)
		{
		  if (MATRIX_ROW_START_CHARPOS (row) <= BUF_BEGV (XBUFFER (w->buffer))
		      && !MATRIX_ROW_PARTIALLY_VISIBLE_AT_TOP_P (w, row))
		    row->indicate_bob_p = !NILP (boundary_top);
		  else
		    row->indicate_top_line_p = !NILP (arrow_top);
		  top_ind_rn = rn;
		}

	      if (bot_ind_rn < 0)
		{
		  if (MATRIX_ROW_END_CHARPOS (row) >= BUF_ZV (XBUFFER (w->buffer))
		      && !MATRIX_ROW_PARTIALLY_VISIBLE_AT_BOTTOM_P (w, row))
		    row->indicate_eob_p = !NILP (boundary_bot), bot_ind_rn = rn;
		  else if (y + row->height >= yb)
		    row->indicate_bottom_line_p = !NILP (arrow_bot), bot_ind_rn = rn;
		}
	    }
	}
    }

  empty_pos = BVAR (XBUFFER (w->buffer), indicate_empty_lines);
  if (!NILP (empty_pos) && !EQ (empty_pos, Qright))
    empty_pos = WINDOW_LEFT_FRINGE_WIDTH (w) == 0 ? Qright : Qleft;

  for (y = 0; y < MAX_BITMAP_CACHE; y++)
    bitmap_cache[y] = -1;

#define LEFT_FRINGE(cache, which, partial_p)			\
  (bitmap_cache[cache*4+partial_p] >= 0				\
   ? bitmap_cache[cache*4+partial_p]				\
   : (bitmap_cache[cache*4+partial_p] =				\
      get_logical_fringe_bitmap (w, which, 0, partial_p)))

#define RIGHT_FRINGE(cache, which, partial_p)			\
  (bitmap_cache[cache*4+2+partial_p] >= 0			\
   ? bitmap_cache[cache*4+2+partial_p]				\
   : (bitmap_cache[cache*4+2+partial_p] =			\
      get_logical_fringe_bitmap (w, which, 1, partial_p)))


  /* Extend top-aligned top indicator (or bottom-aligned bottom
     indicator) to adjacent rows if it doesn't fit in one row.  */
  top_ind_min_y = bot_ind_max_y = -1;
  if (top_ind_rn >= 0)
    {
      int bn = NO_FRINGE_BITMAP;

      row = w->desired_matrix->rows + top_ind_rn;
      if (!row->enabled_p)
	row = w->current_matrix->rows + top_ind_rn;

      top_row_ends_at_zv_p = row->ends_at_zv_p;
      if (row->indicate_bob_p)
	{
	  if (EQ (boundary_top, Qleft))
	    bn = ((row->indicate_eob_p && EQ (boundary_bot, Qleft))
		  ? LEFT_FRINGE (1, Qtop_bottom, row->ends_at_zv_p)
		  : LEFT_FRINGE (2, Qtop, 0));
	  else
	    bn = ((row->indicate_eob_p && EQ (boundary_bot, Qright))
		  ? RIGHT_FRINGE (1, Qtop_bottom, row->ends_at_zv_p)
		  : RIGHT_FRINGE (2, Qtop, 0));
	}
      else if (row->indicate_top_line_p)
	{
	  if (EQ (arrow_top, Qleft))
	    bn = LEFT_FRINGE (6, Qup, 0);
	  else
	    bn = RIGHT_FRINGE (6, Qup, 0);
	}

      if (bn != NO_FRINGE_BITMAP)
	{
	  struct fringe_bitmap *fb = get_fringe_bitmap_data (bn);

	  if (fb->align == ALIGN_BITMAP_TOP && fb->period == 0)
	    {
	      struct glyph_row *row1;
	      int top_ind_max_y;

	      top_ind_min_y = WINDOW_HEADER_LINE_HEIGHT (w);
	      top_ind_max_y = top_ind_min_y + fb->height;
	      if (top_ind_max_y > yb)
		top_ind_max_y = yb;

	      for (y = row->y + row->height, rn = top_ind_rn + 1;
		   y < top_ind_max_y && rn < nrows;
		   y += row1->height, rn++)
		{
		  if (bot_ind_rn >= 0 && rn >= bot_ind_rn)
		    break;

		  row1 = w->desired_matrix->rows + rn;
		  if (!row1->enabled_p)
		    row1 = w->current_matrix->rows + rn;

		  row1->indicate_bob_p = row->indicate_bob_p;
		  row1->indicate_top_line_p = row->indicate_top_line_p;
		}
	    }
	}
    }
  if (bot_ind_rn >= 0)
    {
      int bn = NO_FRINGE_BITMAP;

      row = w->desired_matrix->rows + bot_ind_rn;
      if (!row->enabled_p)
	row = w->current_matrix->rows + bot_ind_rn;

      bot_row_ends_at_zv_p = row->ends_at_zv_p;
      if (row->indicate_eob_p)
	{
	  if (EQ (boundary_bot, Qleft))
	    bn = LEFT_FRINGE (3, Qbottom, row->ends_at_zv_p);
	  else
	    bn = RIGHT_FRINGE (3, Qbottom, row->ends_at_zv_p);
	}
      else if (row->indicate_bottom_line_p)
	{
	  if (EQ (arrow_bot, Qleft))
	    bn = LEFT_FRINGE (7, Qdown, 0);
	  else
	    bn = RIGHT_FRINGE (7, Qdown, 0);
	}

      if (bn != NO_FRINGE_BITMAP)
	{
	  struct fringe_bitmap *fb = get_fringe_bitmap_data (bn);

	  if (fb->align == ALIGN_BITMAP_BOTTOM && fb->period == 0)
	    {
	      struct glyph_row *row1;
	      int bot_ind_min_y;

	      bot_ind_max_y = row->y + row->visible_height;
	      bot_ind_min_y = bot_ind_max_y - fb->height;
	      if (bot_ind_min_y < WINDOW_HEADER_LINE_HEIGHT (w))
		bot_ind_min_y = WINDOW_HEADER_LINE_HEIGHT (w);

	      for (y = row->y, rn = bot_ind_rn - 1;
		   y >= bot_ind_min_y && rn >= 0;
		   y -= row1->height, rn--)
		{
		  if (top_ind_rn >= 0 && rn <= top_ind_rn)
		    break;

		  row1 = w->desired_matrix->rows + rn;
		  if (!row1->enabled_p)
		    row1 = w->current_matrix->rows + rn;

		  row1->indicate_eob_p = row->indicate_eob_p;
		  row1->indicate_bottom_line_p = row->indicate_bottom_line_p;
		}
	    }
	}
    }

  for (y = w->vscroll, rn = 0;
       y < yb && rn < nrows;
       y += row->height, rn++)
    {
      int left, right;
      unsigned left_face_id, right_face_id;
      int left_offset, right_offset;
      int periodic_p;

      row = w->desired_matrix->rows + rn;
      cur = w->current_matrix->rows + rn;
      if (!row->enabled_p)
	row = cur;

      left_face_id = right_face_id = DEFAULT_FACE_ID;
      left_offset = right_offset = 0;
      periodic_p = 0;

      /* Decide which bitmap to draw in the left fringe.  */
      if (WINDOW_LEFT_FRINGE_WIDTH (w) == 0)
	left = NO_FRINGE_BITMAP;
      else if (row->left_user_fringe_bitmap != NO_FRINGE_BITMAP)
	{
	  left = row->left_user_fringe_bitmap;
	  left_face_id = row->left_user_fringe_face_id;
	}
      else if ((!row->reversed_p && row->truncated_on_left_p)
	       || (row->reversed_p && row->truncated_on_right_p))
	left = LEFT_FRINGE (0, Qtruncation, 0);
      else if (row->indicate_bob_p && EQ (boundary_top, Qleft))
	{
	  left = ((row->indicate_eob_p && EQ (boundary_bot, Qleft))
		  ? LEFT_FRINGE (1, Qtop_bottom, top_row_ends_at_zv_p)
		  : LEFT_FRINGE (2, Qtop, 0));
	  if (top_ind_min_y >= 0)
	    left_offset = top_ind_min_y - row->y;
	}
      else if (row->indicate_eob_p && EQ (boundary_bot, Qleft))
	{
	  left = LEFT_FRINGE (3, Qbottom, bot_row_ends_at_zv_p);
	  if (bot_ind_max_y >= 0)
	    left_offset = bot_ind_max_y - (row->y + row->visible_height);
	}
      else if ((!row->reversed_p && MATRIX_ROW_CONTINUATION_LINE_P (row))
	       || (row->reversed_p && row->continued_p))
	left = LEFT_FRINGE (4, Qcontinuation, 0);
      else if (row->indicate_empty_line_p && EQ (empty_pos, Qleft))
	left = LEFT_FRINGE (5, Qempty_line, 0);
      else if (row->indicate_top_line_p && EQ (arrow_top, Qleft))
	{
	  left = LEFT_FRINGE (6, Qup, 0);
	  if (top_ind_min_y >= 0)
	    left_offset = top_ind_min_y - row->y;
	}
      else if (row->indicate_bottom_line_p && EQ (arrow_bot, Qleft))
	{
	  left = LEFT_FRINGE (7, Qdown, 0);
	  if (bot_ind_max_y >= 0)
	    left_offset = bot_ind_max_y - (row->y + row->visible_height);
	}
      else
	left = NO_FRINGE_BITMAP;

      /* Decide which bitmap to draw in the right fringe.  */
      if (WINDOW_RIGHT_FRINGE_WIDTH (w) == 0)
	right = NO_FRINGE_BITMAP;
      else if (row->right_user_fringe_bitmap != NO_FRINGE_BITMAP)
	{
	  right = row->right_user_fringe_bitmap;
	  right_face_id = row->right_user_fringe_face_id;
	}
      else if ((!row->reversed_p && row->truncated_on_right_p)
	       || (row->reversed_p && row->truncated_on_left_p))
	right = RIGHT_FRINGE (0, Qtruncation, 0);
      else if (row->indicate_bob_p && EQ (boundary_top, Qright))
	{
	  right = ((row->indicate_eob_p && EQ (boundary_bot, Qright))
		   ? RIGHT_FRINGE (1, Qtop_bottom, top_row_ends_at_zv_p)
		   : RIGHT_FRINGE (2, Qtop, 0));
	  if (top_ind_min_y >= 0)
	    right_offset = top_ind_min_y - row->y;
	}
      else if (row->indicate_eob_p && EQ (boundary_bot, Qright))
	{
	  right = RIGHT_FRINGE (3, Qbottom, bot_row_ends_at_zv_p);
	  if (bot_ind_max_y >= 0)
	    right_offset = bot_ind_max_y - (row->y + row->visible_height);
	}
      else if ((!row->reversed_p && row->continued_p)
	       || (row->reversed_p && MATRIX_ROW_CONTINUATION_LINE_P (row)))
	right = RIGHT_FRINGE (4, Qcontinuation, 0);
      else if (row->indicate_top_line_p && EQ (arrow_top, Qright))
	{
	  right = RIGHT_FRINGE (6, Qup, 0);
	  if (top_ind_min_y >= 0)
	    right_offset = top_ind_min_y - row->y;
	}
      else if (row->indicate_bottom_line_p && EQ (arrow_bot, Qright))
	{
	  right = RIGHT_FRINGE (7, Qdown, 0);
	  if (bot_ind_max_y >= 0)
	    right_offset = bot_ind_max_y - (row->y + row->visible_height);
	}
      else if (row->indicate_empty_line_p && EQ (empty_pos, Qright))
	right = RIGHT_FRINGE (5, Qempty_line, 0);
      else
	right = NO_FRINGE_BITMAP;

      periodic_p = (get_fringe_bitmap_data (left)->period != 0
		    || get_fringe_bitmap_data (right)->period != 0);

      if (row->y != cur->y
	  || row->visible_height != cur->visible_height
	  || row->ends_at_zv_p != cur->ends_at_zv_p
	  || left != cur->left_fringe_bitmap
	  || right != cur->right_fringe_bitmap
	  || left_face_id != cur->left_fringe_face_id
	  || right_face_id != cur->right_fringe_face_id
	  || left_offset != cur->left_fringe_offset
	  || right_offset != cur->right_fringe_offset
	  || periodic_p != cur->fringe_bitmap_periodic_p
	  || cur->redraw_fringe_bitmaps_p)
	{
	  redraw_p = row->redraw_fringe_bitmaps_p = 1;
	  if (!keep_current_p)
	    {
	      cur->redraw_fringe_bitmaps_p = 1;
	      cur->left_fringe_bitmap = left;
	      cur->right_fringe_bitmap = right;
	      cur->left_fringe_face_id = left_face_id;
	      cur->right_fringe_face_id = right_face_id;
	      cur->left_fringe_offset = left_offset;
	      cur->right_fringe_offset = right_offset;
	      cur->fringe_bitmap_periodic_p = periodic_p;
	    }
	}

      if (row->overlay_arrow_bitmap < 0)
	row->overlay_arrow_bitmap = get_logical_fringe_bitmap (w, Qoverlay_arrow, 0, 0);

      if (row->overlay_arrow_bitmap != cur->overlay_arrow_bitmap)
	{
	  redraw_p = row->redraw_fringe_bitmaps_p = 1;
	  if (!keep_current_p)
	    {
	      cur->redraw_fringe_bitmaps_p = 1;
	      cur->overlay_arrow_bitmap = row->overlay_arrow_bitmap;
	    }
	}

      row->left_fringe_bitmap = left;
      row->right_fringe_bitmap = right;
      row->left_fringe_face_id = left_face_id;
      row->right_fringe_face_id = right_face_id;
      row->left_fringe_offset = left_offset;
      row->right_fringe_offset = right_offset;
      row->fringe_bitmap_periodic_p = periodic_p;
    }

  return redraw_p && !keep_current_p;
}


/* Compute actual fringe widths for frame F.

   If REDRAW is 1, redraw F if the fringe settings was actually
   modified and F is visible.

   Since the combined left and right fringe must occupy an integral
   number of columns, we may need to add some pixels to each fringe.
   Typically, we add an equal amount (+/- 1 pixel) to each fringe,
   but a negative width value is taken literally (after negating it).

   We never make the fringes narrower than specified.
*/

void
compute_fringe_widths (struct frame *f, int redraw)
{
  int o_left = FRAME_LEFT_FRINGE_WIDTH (f);
  int o_right = FRAME_RIGHT_FRINGE_WIDTH (f);
  int o_cols = FRAME_FRINGE_COLS (f);

  Lisp_Object left_fringe = Fassq (Qleft_fringe, f->param_alist);
  Lisp_Object right_fringe = Fassq (Qright_fringe, f->param_alist);
  int left_fringe_width, right_fringe_width;

  if (!NILP (left_fringe))
    left_fringe = Fcdr (left_fringe);
  if (!NILP (right_fringe))
    right_fringe = Fcdr (right_fringe);

  left_fringe_width = ((NILP (left_fringe) || !INTEGERP (left_fringe)) ? 8 :
		       XINT (left_fringe));
  right_fringe_width = ((NILP (right_fringe) || !INTEGERP (right_fringe)) ? 8 :
			XINT (right_fringe));

  if (left_fringe_width || right_fringe_width)
    {
      int left_wid = left_fringe_width >= 0 ? left_fringe_width : -left_fringe_width;
      int right_wid = right_fringe_width >= 0 ? right_fringe_width : -right_fringe_width;
      int conf_wid = left_wid + right_wid;
      int font_wid = FRAME_COLUMN_WIDTH (f);
      int cols = (left_wid + right_wid + font_wid-1) / font_wid;
      int real_wid = cols * font_wid;
      if (left_wid && right_wid)
	{
	  if (left_fringe_width < 0)
	    {
	      /* Left fringe width is fixed, adjust right fringe if necessary */
	      FRAME_LEFT_FRINGE_WIDTH (f) = left_wid;
	      FRAME_RIGHT_FRINGE_WIDTH (f) = real_wid - left_wid;
	    }
	  else if (right_fringe_width < 0)
	    {
	      /* Right fringe width is fixed, adjust left fringe if necessary */
	      FRAME_LEFT_FRINGE_WIDTH (f) = real_wid - right_wid;
	      FRAME_RIGHT_FRINGE_WIDTH (f) = right_wid;
	    }
	  else
	    {
	      /* Adjust both fringes with an equal amount.
		 Note that we are doing integer arithmetic here, so don't
		 lose a pixel if the total width is an odd number.  */
	      int fill = real_wid - conf_wid;
	      FRAME_LEFT_FRINGE_WIDTH (f) = left_wid + fill/2;
	      FRAME_RIGHT_FRINGE_WIDTH (f) = right_wid + fill - fill/2;
	    }
	}
      else if (left_fringe_width)
	{
	  FRAME_LEFT_FRINGE_WIDTH (f) = real_wid;
	  FRAME_RIGHT_FRINGE_WIDTH (f) = 0;
	}
      else
	{
	  FRAME_LEFT_FRINGE_WIDTH (f) = 0;
	  FRAME_RIGHT_FRINGE_WIDTH (f) = real_wid;
	}
      FRAME_FRINGE_COLS (f) = cols;
    }
  else
    {
      FRAME_LEFT_FRINGE_WIDTH (f) = 0;
      FRAME_RIGHT_FRINGE_WIDTH (f) = 0;
      FRAME_FRINGE_COLS (f) = 0;
    }

  if (redraw && FRAME_VISIBLE_P (f))
    if (o_left != FRAME_LEFT_FRINGE_WIDTH (f) ||
	o_right != FRAME_RIGHT_FRINGE_WIDTH (f) ||
	o_cols != FRAME_FRINGE_COLS (f))
      redraw_frame (f);
}


/* Free resources used by a user-defined bitmap.  */

static void
destroy_fringe_bitmap (int n)
{
  struct fringe_bitmap **fbp;

  fringe_faces[n] = Qnil;

  fbp = &fringe_bitmaps[n];
  if (*fbp && (*fbp)->dynamic)
    {
      /* XXX Is SELECTED_FRAME OK here? */
      struct redisplay_interface *rif = FRAME_RIF (SELECTED_FRAME ());
      if (rif && rif->destroy_fringe_bitmap)
	rif->destroy_fringe_bitmap (n);
      xfree (*fbp);
      *fbp = NULL;
    }

  while (max_used_fringe_bitmap > MAX_STANDARD_FRINGE_BITMAPS
	 && fringe_bitmaps[max_used_fringe_bitmap - 1] == NULL)
    max_used_fringe_bitmap--;
}


DEFUN ("destroy-fringe-bitmap", Fdestroy_fringe_bitmap, Sdestroy_fringe_bitmap,
       1, 1, 0,
       doc: /* Destroy fringe bitmap BITMAP.
If BITMAP overrides a standard fringe bitmap, the original bitmap is restored.  */)
  (Lisp_Object bitmap)
{
  int n;

  CHECK_SYMBOL (bitmap);
  n = lookup_fringe_bitmap (bitmap);
  if (!n)
    return Qnil;

  destroy_fringe_bitmap (n);

  if (n >= MAX_STANDARD_FRINGE_BITMAPS)
    {
      Vfringe_bitmaps = Fdelq (bitmap, Vfringe_bitmaps);
      /* It would be better to remove the fringe property.  */
      Fput (bitmap, Qfringe, Qnil);
    }

  return Qnil;
}


/* Initialize bitmap bit.

   On X, we bit-swap the built-in bitmaps and reduce bitmap
   from short to char array if width is <= 8 bits.

   On MAC with big-endian CPU, we need to byte-swap each short.

   On W32 and MAC (little endian), there's no need to do this.
*/

#if defined (HAVE_X_WINDOWS)
static const unsigned char swap_nibble[16] = {
  0x0, 0x8, 0x4, 0xc,           /* 0000 1000 0100 1100 */
  0x2, 0xa, 0x6, 0xe,           /* 0010 1010 0110 1110 */
  0x1, 0x9, 0x5, 0xd,           /* 0001 1001 0101 1101 */
  0x3, 0xb, 0x7, 0xf};          /* 0011 1011 0111 1111 */
#endif                          /* HAVE_X_WINDOWS */

static void
init_fringe_bitmap (int which, struct fringe_bitmap *fb, int once_p)
{
  if (once_p || fb->dynamic)
    {
#if defined (HAVE_X_WINDOWS)
      unsigned short *bits = fb->bits;
      int j;

      if (fb->width <= 8)
	{
	  unsigned char *cbits = (unsigned char *)fb->bits;
	  for (j = 0; j < fb->height; j++)
	    {
	      unsigned short b = *bits++;
	      unsigned char c;
	      c = (unsigned char)((swap_nibble[b & 0xf] << 4)
				  | (swap_nibble[(b>>4) & 0xf]));
	      *cbits++ = (c >> (8 - fb->width));
	    }
	}
      else
	{
	  for (j = 0; j < fb->height; j++)
	    {
	      unsigned short b = *bits;
	      b = (unsigned short)((swap_nibble[b & 0xf] << 12)
				   | (swap_nibble[(b>>4) & 0xf] << 8)
				   | (swap_nibble[(b>>8) & 0xf] << 4)
				   | (swap_nibble[(b>>12) & 0xf]));
	      b >>= (16 - fb->width);
#ifdef WORDS_BIGENDIAN
	      b = ((b >> 8) | (b << 8));
#endif
	      *bits++ = b;
	    }
	}
#endif /* HAVE_X_WINDOWS */

    }

  if (!once_p)
    {
      /* XXX Is SELECTED_FRAME OK here? */
      struct redisplay_interface *rif = FRAME_RIF (SELECTED_FRAME ());

      destroy_fringe_bitmap (which);

      if (rif && rif->define_fringe_bitmap)
	rif->define_fringe_bitmap (which, fb->bits, fb->height, fb->width);

      fringe_bitmaps[which] = fb;
      if (which >= max_used_fringe_bitmap)
	max_used_fringe_bitmap = which + 1;
    }
}


DEFUN ("define-fringe-bitmap", Fdefine_fringe_bitmap, Sdefine_fringe_bitmap,
       2, 5, 0,
       doc: /* Define fringe bitmap BITMAP from BITS of size HEIGHT x WIDTH.
BITMAP is a symbol identifying the new fringe bitmap.
BITS is either a string or a vector of integers.
HEIGHT is height of bitmap.  If HEIGHT is nil, use length of BITS.
WIDTH must be an integer between 1 and 16, or nil which defaults to 8.
Optional fifth arg ALIGN may be one of `top', `center', or `bottom',
indicating the positioning of the bitmap relative to the rows where it
is used; the default is to center the bitmap.  Fifth arg may also be a
list (ALIGN PERIODIC) where PERIODIC non-nil specifies that the bitmap
should be repeated.
If BITMAP already exists, the existing definition is replaced.  */)
  (Lisp_Object bitmap, Lisp_Object bits, Lisp_Object height, Lisp_Object width, Lisp_Object align)
{
  int n, h, i, j;
  unsigned short *b;
  struct fringe_bitmap fb, *xfb;
  int fill1 = 0, fill2 = 0;

  CHECK_SYMBOL (bitmap);

  if (STRINGP (bits))
    h = SCHARS (bits);
  else if (VECTORP (bits))
    h = ASIZE (bits);
  else
    wrong_type_argument (Qsequencep, bits);

  if (NILP (height))
    fb.height = h;
  else
    {
      CHECK_NUMBER (height);
      fb.height = min (XINT (height), 255);
      if (fb.height > h)
	{
	  fill1 = (fb.height - h) / 2;
	  fill2 = fb.height - h - fill1;
	}
    }

  if (NILP (width))
    fb.width = 8;
  else
    {
      CHECK_NUMBER (width);
      fb.width = min (XINT (width), 255);
    }

  fb.period = 0;
  fb.align = ALIGN_BITMAP_CENTER;

  if (CONSP (align))
    {
      Lisp_Object period = XCDR (align);
      if (CONSP (period))
	{
	  period = XCAR (period);
	  if (!NILP (period))
	    {
	      fb.period = fb.height;
	      fb.height = 255;
	    }
	}
      align = XCAR (align);
    }
  if (EQ (align, Qtop))
    fb.align = ALIGN_BITMAP_TOP;
  else if (EQ (align, Qbottom))
    fb.align = ALIGN_BITMAP_BOTTOM;
  else if (!NILP (align) && !EQ (align, Qcenter))
    error ("Bad align argument");

  n = lookup_fringe_bitmap (bitmap);
  if (!n)
    {
      if (max_used_fringe_bitmap < max_fringe_bitmaps)
	n = max_used_fringe_bitmap++;
      else
	{
	  for (n = MAX_STANDARD_FRINGE_BITMAPS;
	       n < max_fringe_bitmaps;
	       n++)
	    if (fringe_bitmaps[n] == NULL)
	      break;

	  if (n == max_fringe_bitmaps)
	    {
	      int bitmaps = max_fringe_bitmaps + 20;
	      if (MAX_FRINGE_BITMAPS < bitmaps)
		error ("No free fringe bitmap slots");

	      i = max_fringe_bitmaps;
	      fringe_bitmaps
		= ((struct fringe_bitmap **)
		   xrealloc (fringe_bitmaps, bitmaps * sizeof *fringe_bitmaps));
	      fringe_faces
		= (Lisp_Object *) xrealloc (fringe_faces,
					    bitmaps * sizeof *fringe_faces);

	      for (i = max_fringe_bitmaps; i < bitmaps; i++)
		{
		  fringe_bitmaps[i] = NULL;
		  fringe_faces[i] = Qnil;
		}

	      max_fringe_bitmaps = bitmaps;
	    }
	}

      Vfringe_bitmaps = Fcons (bitmap, Vfringe_bitmaps);
      Fput (bitmap, Qfringe, make_number (n));
    }

  fb.dynamic = 1;

  xfb = (struct fringe_bitmap *) xmalloc (sizeof fb
					  + fb.height * BYTES_PER_BITMAP_ROW);
  fb.bits = b = (unsigned short *) (xfb + 1);
  memset (b, 0, fb.height);

  j = 0;
  while (j < fb.height)
    {
      for (i = 0; i < fill1 && j < fb.height; i++)
	b[j++] = 0;
      for (i = 0; i < h && j < fb.height; i++)
	{
	  Lisp_Object elt = Faref (bits, make_number (i));
	  b[j++] = NUMBERP (elt) ? XINT (elt) : 0;
	}
      for (i = 0; i < fill2 && j < fb.height; i++)
	b[j++] = 0;
    }

  *xfb = fb;

  init_fringe_bitmap (n, xfb, 0);

  return bitmap;
}

DEFUN ("set-fringe-bitmap-face", Fset_fringe_bitmap_face, Sset_fringe_bitmap_face,
       1, 2, 0,
       doc: /* Set face for fringe bitmap BITMAP to FACE.
If FACE is nil, reset face to default fringe face.  */)
  (Lisp_Object bitmap, Lisp_Object face)
{
  int n;
  int face_id;

  CHECK_SYMBOL (bitmap);
  n = lookup_fringe_bitmap (bitmap);
  if (!n)
    error ("Undefined fringe bitmap");

  if (!NILP (face))
    {
      face_id = lookup_derived_face (SELECTED_FRAME (), face,
				     FRINGE_FACE_ID, 1);
      if (face_id < 0)
	error ("No such face");
    }

  fringe_faces[n] = face;

  return Qnil;
}

DEFUN ("fringe-bitmaps-at-pos", Ffringe_bitmaps_at_pos, Sfringe_bitmaps_at_pos,
       0, 2, 0,
       doc: /* Return fringe bitmaps of row containing position POS in window WINDOW.
If WINDOW is nil, use selected window.  If POS is nil, use value of point
in that window.  Return value is a list (LEFT RIGHT OV), where LEFT
is the symbol for the bitmap in the left fringe (or nil if no bitmap),
RIGHT is similar for the right fringe, and OV is non-nil if there is an
overlay arrow in the left fringe.
Return nil if POS is not visible in WINDOW.  */)
  (Lisp_Object pos, Lisp_Object window)
{
  struct window *w;
  struct glyph_row *row;
  int textpos;

  if (NILP (window))
    window = selected_window;
  CHECK_WINDOW (window);
  w = XWINDOW (window);

  if (!NILP (pos))
    {
      CHECK_NUMBER_COERCE_MARKER (pos);
      textpos = XINT (pos);
    }
  else if (w == XWINDOW (selected_window))
    textpos = PT;
  else
    textpos = XMARKER (w->pointm)->charpos;

  row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  row = row_containing_pos (w, textpos, row, NULL, 0);
  if (row)
    return list3 (get_fringe_bitmap_name (row->left_fringe_bitmap),
		  get_fringe_bitmap_name (row->right_fringe_bitmap),
		  (row->overlay_arrow_bitmap == 0 ? Qnil
		   : row->overlay_arrow_bitmap < 0 ? Qt
		   : get_fringe_bitmap_name (row->overlay_arrow_bitmap)));
  else
    return Qnil;
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_fringe (void)
{
  DEFSYM (Qtruncation, "truncation");
  DEFSYM (Qcontinuation, "continuation");
  DEFSYM (Qoverlay_arrow, "overlay-arrow");
  DEFSYM (Qempty_line, "empty-line");
  DEFSYM (Qtop_bottom, "top-bottom");
  DEFSYM (Qhollow_small, "hollow-small");

  defsubr (&Sdestroy_fringe_bitmap);
  defsubr (&Sdefine_fringe_bitmap);
  defsubr (&Sfringe_bitmaps_at_pos);
  defsubr (&Sset_fringe_bitmap_face);

  DEFVAR_LISP ("overflow-newline-into-fringe", Voverflow_newline_into_fringe,
    doc: /* *Non-nil means that newline may flow into the right fringe.
This means that display lines which are exactly as wide as the window
(not counting the final newline) will only occupy one screen line, by
showing (or hiding) the final newline in the right fringe; when point
is at the final newline, the cursor is shown in the right fringe.
If nil, also continue lines which are exactly as wide as the window.  */);
  Voverflow_newline_into_fringe = Qt;

  DEFVAR_LISP ("fringe-bitmaps", Vfringe_bitmaps,
    doc: /* List of fringe bitmap symbols.  */);
  Vfringe_bitmaps = Qnil;
}

/* Garbage collection hook */

void
mark_fringe_data (void)
{
  int i;

  for (i = 0; i < max_fringe_bitmaps; i++)
    if (!NILP (fringe_faces[i]))
      mark_object (fringe_faces[i]);
}

/* Initialize this module when Emacs starts.  */

void
init_fringe_once (void)
{
  int bt;

  for (bt = NO_FRINGE_BITMAP + 1; bt < MAX_STANDARD_FRINGE_BITMAPS; bt++)
    init_fringe_bitmap (bt, &standard_bitmaps[bt], 1);
}

void
init_fringe (void)
{
  int i;

  max_fringe_bitmaps = MAX_STANDARD_FRINGE_BITMAPS + 20;

  fringe_bitmaps
    = (struct fringe_bitmap **) xmalloc (max_fringe_bitmaps * sizeof (struct fringe_bitmap *));
  fringe_faces
    = (Lisp_Object *) xmalloc (max_fringe_bitmaps * sizeof (Lisp_Object));

  for (i = 0; i < max_fringe_bitmaps; i++)
    {
      fringe_bitmaps[i] = NULL;
      fringe_faces[i] = Qnil;
    }
}

#ifdef HAVE_NTGUI

void
w32_init_fringe (struct redisplay_interface *rif)
{
  int bt;

  if (!rif)
    return;

  for (bt = NO_FRINGE_BITMAP + 1; bt < MAX_STANDARD_FRINGE_BITMAPS; bt++)
    {
      struct fringe_bitmap *fb = &standard_bitmaps[bt];
      rif->define_fringe_bitmap (bt, fb->bits, fb->height, fb->width);
    }
}

void
w32_reset_fringes (void)
{
  /* Destroy row bitmaps.  */
  int bt;
  struct redisplay_interface *rif = FRAME_RIF (SELECTED_FRAME ());

  if (!rif)
    return;

  for (bt = NO_FRINGE_BITMAP + 1; bt < max_used_fringe_bitmap; bt++)
    rif->destroy_fringe_bitmap (bt);
}

#endif /* HAVE_NTGUI */

#endif /* HAVE_WINDOW_SYSTEM */
