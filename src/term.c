/* Terminal control module for terminals described by TERMCAP
   Copyright (C) 1985-1987, 1993-1995, 1998, 2000-2012
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

/* New redisplay, TTY faces by Gerd Moellmann <gerd@gnu.org>.  */

#include <config.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/file.h>
#include <unistd.h>
#include <signal.h>
#include <setjmp.h>

#include "lisp.h"
#include "termchar.h"
#include "termopts.h"
#include "tparam.h"
#include "buffer.h"
#include "character.h"
#include "charset.h"
#include "coding.h"
#include "composite.h"
#include "keyboard.h"
#include "frame.h"
#include "disptab.h"
#include "termhooks.h"
#include "dispextern.h"
#include "window.h"
#include "keymap.h"
#include "blockinput.h"
#include "syssignal.h"
#include "systty.h"
#include "intervals.h"
#ifdef MSDOS
#include "msdos.h"
static int been_here = -1;
#endif

#include "cm.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif

#ifndef O_RDWR
#define O_RDWR 2
#endif

#ifndef O_NOCTTY
#define O_NOCTTY 0
#endif

/* The name of the default console device.  */
#ifdef WINDOWSNT
#define DEV_TTY  "CONOUT$"
#else
#define DEV_TTY  "/dev/tty"
#endif

static void tty_set_scroll_region (struct frame *f, int start, int stop);
static void turn_on_face (struct frame *, int face_id);
static void turn_off_face (struct frame *, int face_id);
static void tty_turn_off_highlight (struct tty_display_info *);
static void tty_show_cursor (struct tty_display_info *);
static void tty_hide_cursor (struct tty_display_info *);
static void tty_background_highlight (struct tty_display_info *tty);
static struct terminal *get_tty_terminal (Lisp_Object, int);
static void clear_tty_hooks (struct terminal *terminal);
static void set_tty_hooks (struct terminal *terminal);
static void dissociate_if_controlling_tty (int fd);
static void delete_tty (struct terminal *);
static void maybe_fatal (int must_succeed, struct terminal *terminal,
			 const char *str1, const char *str2, ...)
  NO_RETURN ATTRIBUTE_FORMAT_PRINTF (3, 5) ATTRIBUTE_FORMAT_PRINTF (4, 5);
static void vfatal (const char *str, va_list ap)
  NO_RETURN ATTRIBUTE_FORMAT_PRINTF (1, 0);


#define OUTPUT(tty, a)                                          \
  emacs_tputs ((tty), a,                                        \
               (int) (FRAME_LINES (XFRAME (selected_frame))     \
                      - curY (tty)),                            \
               cmputc)

#define OUTPUT1(tty, a) emacs_tputs ((tty), a, 1, cmputc)
#define OUTPUTL(tty, a, lines) emacs_tputs ((tty), a, lines, cmputc)

#define OUTPUT_IF(tty, a)                                               \
  do {                                                                  \
    if (a)                                                              \
      OUTPUT (tty, a);							\
  } while (0)

#define OUTPUT1_IF(tty, a) do { if (a) emacs_tputs ((tty), a, 1, cmputc); } while (0)

/* Display space properties */

/* Chain of all tty device parameters. */
struct tty_display_info *tty_list;

/* Meaning of bits in no_color_video.  Each bit set means that the
   corresponding attribute cannot be combined with colors.  */

enum no_color_bit
{
  NC_STANDOUT	 = 1 << 0,
  NC_UNDERLINE	 = 1 << 1,
  NC_REVERSE	 = 1 << 2,
  NC_BLINK	 = 1 << 3,
  NC_DIM	 = 1 << 4,
  NC_BOLD	 = 1 << 5,
  NC_INVIS	 = 1 << 6,
  NC_PROTECT	 = 1 << 7,
  NC_ALT_CHARSET = 1 << 8
};

/* internal state */

/* The largest frame width in any call to calculate_costs.  */

static int max_frame_cols;

/* Non-zero if we have dropped our controlling tty and therefore
   should not open a frame on stdout. */
static int no_controlling_tty;



#ifdef HAVE_GPM
#include <sys/fcntl.h>

/* The device for which we have enabled gpm support (or NULL).  */
struct tty_display_info *gpm_tty = NULL;

/* Last recorded mouse coordinates.  */
static int last_mouse_x, last_mouse_y;
#endif /* HAVE_GPM */

/* Ring the bell on a tty. */

static void
tty_ring_bell (struct frame *f)
{
  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->output)
    {
      OUTPUT (tty, (tty->TS_visible_bell && visible_bell
                    ? tty->TS_visible_bell
                    : tty->TS_bell));
      fflush (tty->output);
    }
}

/* Set up termcap modes for Emacs. */

static void
tty_set_terminal_modes (struct terminal *terminal)
{
  struct tty_display_info *tty = terminal->display_info.tty;

  if (tty->output)
    {
      if (tty->TS_termcap_modes)
        OUTPUT (tty, tty->TS_termcap_modes);
      else
        {
          /* Output enough newlines to scroll all the old screen contents
             off the screen, so it won't be overwritten and lost.  */
          int i;
          current_tty = tty;
          for (i = 0; i < FRAME_LINES (XFRAME (selected_frame)); i++)
            cmputc ('\n');
        }

      OUTPUT_IF (tty, visible_cursor ? tty->TS_cursor_visible : tty->TS_cursor_normal);
      OUTPUT_IF (tty, tty->TS_keypad_mode);
      losecursor (tty);
      fflush (tty->output);
    }
}

/* Reset termcap modes before exiting Emacs. */

static void
tty_reset_terminal_modes (struct terminal *terminal)
{
  struct tty_display_info *tty = terminal->display_info.tty;

  if (tty->output)
    {
      tty_turn_off_highlight (tty);
      tty_turn_off_insert (tty);
      OUTPUT_IF (tty, tty->TS_end_keypad_mode);
      OUTPUT_IF (tty, tty->TS_cursor_normal);
      OUTPUT_IF (tty, tty->TS_end_termcap_modes);
      OUTPUT_IF (tty, tty->TS_orig_pair);
      /* Output raw CR so kernel can track the cursor hpos.  */
      current_tty = tty;
      cmputc ('\r');
      fflush (tty->output);
    }
}

/* Flag the end of a display update on a termcap terminal. */

static void
tty_update_end (struct frame *f)
{
  struct tty_display_info *tty = FRAME_TTY (f);

  if (!XWINDOW (selected_window)->cursor_off_p)
    tty_show_cursor (tty);
  tty_turn_off_insert (tty);
  tty_background_highlight (tty);
}

/* The implementation of set_terminal_window for termcap frames. */

static void
tty_set_terminal_window (struct frame *f, int size)
{
  struct tty_display_info *tty = FRAME_TTY (f);

  tty->specified_window = size ? size : FRAME_LINES (f);
  if (FRAME_SCROLL_REGION_OK (f))
    tty_set_scroll_region (f, 0, tty->specified_window);
}

static void
tty_set_scroll_region (struct frame *f, int start, int stop)
{
  char *buf;
  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->TS_set_scroll_region)
    buf = tparam (tty->TS_set_scroll_region, 0, 0, start, stop - 1, 0, 0);
  else if (tty->TS_set_scroll_region_1)
    buf = tparam (tty->TS_set_scroll_region_1, 0, 0,
		  FRAME_LINES (f), start,
		  FRAME_LINES (f) - stop,
		  FRAME_LINES (f));
  else
    buf = tparam (tty->TS_set_window, 0, 0, start, 0, stop, FRAME_COLS (f));

  OUTPUT (tty, buf);
  xfree (buf);
  losecursor (tty);
}


static void
tty_turn_on_insert (struct tty_display_info *tty)
{
  if (!tty->insert_mode)
    OUTPUT (tty, tty->TS_insert_mode);
  tty->insert_mode = 1;
}

void
tty_turn_off_insert (struct tty_display_info *tty)
{
  if (tty->insert_mode)
    OUTPUT (tty, tty->TS_end_insert_mode);
  tty->insert_mode = 0;
}

/* Handle highlighting.  */

static void
tty_turn_off_highlight (struct tty_display_info *tty)
{
  if (tty->standout_mode)
    OUTPUT_IF (tty, tty->TS_end_standout_mode);
  tty->standout_mode = 0;
}

static void
tty_turn_on_highlight (struct tty_display_info *tty)
{
  if (!tty->standout_mode)
    OUTPUT_IF (tty, tty->TS_standout_mode);
  tty->standout_mode = 1;
}

static void
tty_toggle_highlight (struct tty_display_info *tty)
{
  if (tty->standout_mode)
    tty_turn_off_highlight (tty);
  else
    tty_turn_on_highlight (tty);
}


/* Make cursor invisible.  */

static void
tty_hide_cursor (struct tty_display_info *tty)
{
  if (tty->cursor_hidden == 0)
    {
      tty->cursor_hidden = 1;
      OUTPUT_IF (tty, tty->TS_cursor_invisible);
    }
}


/* Ensure that cursor is visible.  */

static void
tty_show_cursor (struct tty_display_info *tty)
{
  if (tty->cursor_hidden)
    {
      tty->cursor_hidden = 0;
      OUTPUT_IF (tty, tty->TS_cursor_normal);
      if (visible_cursor)
        OUTPUT_IF (tty, tty->TS_cursor_visible);
    }
}


/* Set standout mode to the state it should be in for
   empty space inside windows.  What this is,
   depends on the user option inverse-video.  */

static void
tty_background_highlight (struct tty_display_info *tty)
{
  if (inverse_video)
    tty_turn_on_highlight (tty);
  else
    tty_turn_off_highlight (tty);
}

/* Set standout mode to the mode specified for the text to be output.  */

static void
tty_highlight_if_desired (struct tty_display_info *tty)
{
  if (inverse_video)
    tty_turn_on_highlight (tty);
  else
    tty_turn_off_highlight (tty);
}


/* Move cursor to row/column position VPOS/HPOS.  HPOS/VPOS are
   frame-relative coordinates.  */

static void
tty_cursor_to (struct frame *f, int vpos, int hpos)
{
  struct tty_display_info *tty = FRAME_TTY (f);

  /* Detect the case where we are called from reset_sys_modes
     and the costs have never been calculated.  Do nothing.  */
  if (! tty->costs_set)
    return;

  if (curY (tty) == vpos
      && curX (tty) == hpos)
    return;
  if (!tty->TF_standout_motion)
    tty_background_highlight (tty);
  if (!tty->TF_insmode_motion)
    tty_turn_off_insert (tty);
  cmgoto (tty, vpos, hpos);
}

/* Similar but don't take any account of the wasted characters.  */

static void
tty_raw_cursor_to (struct frame *f, int row, int col)
{
  struct tty_display_info *tty = FRAME_TTY (f);

  if (curY (tty) == row
      && curX (tty) == col)
    return;
  if (!tty->TF_standout_motion)
    tty_background_highlight (tty);
  if (!tty->TF_insmode_motion)
    tty_turn_off_insert (tty);
  cmgoto (tty, row, col);
}

/* Erase operations */

/* Clear from cursor to end of frame on a termcap device. */

static void
tty_clear_to_end (struct frame *f)
{
  register int i;
  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->TS_clr_to_bottom)
    {
      tty_background_highlight (tty);
      OUTPUT (tty, tty->TS_clr_to_bottom);
    }
  else
    {
      for (i = curY (tty); i < FRAME_LINES (f); i++)
	{
	  cursor_to (f, i, 0);
	  clear_end_of_line (f, FRAME_COLS (f));
	}
    }
}

/* Clear an entire termcap frame. */

static void
tty_clear_frame (struct frame *f)
{
  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->TS_clr_frame)
    {
      tty_background_highlight (tty);
      OUTPUT (tty, tty->TS_clr_frame);
      cmat (tty, 0, 0);
    }
  else
    {
      cursor_to (f, 0, 0);
      clear_to_end (f);
    }
}

/* An implementation of clear_end_of_line for termcap frames.

   Note that the cursor may be moved, on terminals lacking a `ce' string.  */

static void
tty_clear_end_of_line (struct frame *f, int first_unused_hpos)
{
  register int i;
  struct tty_display_info *tty = FRAME_TTY (f);

  /* Detect the case where we are called from reset_sys_modes
     and the costs have never been calculated.  Do nothing.  */
  if (! tty->costs_set)
    return;

  if (curX (tty) >= first_unused_hpos)
    return;
  tty_background_highlight (tty);
  if (tty->TS_clr_line)
    {
      OUTPUT1 (tty, tty->TS_clr_line);
    }
  else
    {			/* have to do it the hard way */
      tty_turn_off_insert (tty);

      /* Do not write in last row last col with Auto-wrap on. */
      if (AutoWrap (tty)
          && curY (tty) == FrameRows (tty) - 1
	  && first_unused_hpos == FrameCols (tty))
	first_unused_hpos--;

      for (i = curX (tty); i < first_unused_hpos; i++)
	{
	  if (tty->termscript)
	    fputc (' ', tty->termscript);
	  fputc (' ', tty->output);
	}
      cmplus (tty, first_unused_hpos - curX (tty));
    }
}

/* Buffers to store the source and result of code conversion for terminal.  */
static unsigned char *encode_terminal_src;
static unsigned char *encode_terminal_dst;
/* Allocated sizes of the above buffers.  */
static ptrdiff_t encode_terminal_src_size;
static ptrdiff_t encode_terminal_dst_size;

/* Encode SRC_LEN glyphs starting at SRC to terminal output codes.
   Set CODING->produced to the byte-length of the resulting byte
   sequence, and return a pointer to that byte sequence.  */

unsigned char *
encode_terminal_code (struct glyph *src, int src_len, struct coding_system *coding)
{
  struct glyph *src_end = src + src_len;
  unsigned char *buf;
  ptrdiff_t nchars, nbytes, required;
  ptrdiff_t tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;
  Lisp_Object charset_list;

  /* Allocate sufficient size of buffer to store all characters in
     multibyte-form.  But, it may be enlarged on demand if
     Vglyph_table contains a string or a composite glyph is
     encountered.  */
  if (min (PTRDIFF_MAX, SIZE_MAX) / MAX_MULTIBYTE_LENGTH < src_len)
    memory_full (SIZE_MAX);
  required = src_len;
  required *= MAX_MULTIBYTE_LENGTH;
  if (encode_terminal_src_size < required)
    {
      encode_terminal_src = xrealloc (encode_terminal_src, required);
      encode_terminal_src_size = required;
    }

  charset_list = coding_charset_list (coding);

  buf = encode_terminal_src;
  nchars = 0;
  while (src < src_end)
    {
      if (src->type == COMPOSITE_GLYPH)
	{
	  struct composition *cmp IF_LINT (= NULL);
	  Lisp_Object gstring IF_LINT (= Qnil);
	  int i;

	  nbytes = buf - encode_terminal_src;
	  if (src->u.cmp.automatic)
	    {
	      gstring = composition_gstring_from_id (src->u.cmp.id);
	      required = src->slice.cmp.to - src->slice.cmp.from + 1;
	    }
	  else
	    {
	      cmp = composition_table[src->u.cmp.id];
	      required = cmp->glyph_len;
	      required *= MAX_MULTIBYTE_LENGTH;
	    }

	  if (encode_terminal_src_size - nbytes < required)
	    {
	      encode_terminal_src =
		xpalloc (encode_terminal_src, &encode_terminal_src_size,
			 required - (encode_terminal_src_size - nbytes),
			 -1, 1);
	      buf = encode_terminal_src + nbytes;
	    }

	  if (src->u.cmp.automatic)
	    for (i = src->slice.cmp.from; i <= src->slice.cmp.to; i++)
	      {
		Lisp_Object g = LGSTRING_GLYPH (gstring, i);
		int c = LGLYPH_CHAR (g);

		if (! char_charset (c, charset_list, NULL))
		  c = '?';
		buf += CHAR_STRING (c, buf);
		nchars++;
	      }
	  else
	    for (i = 0; i < cmp->glyph_len; i++)
	      {
		int c = COMPOSITION_GLYPH (cmp, i);

		/* TAB in a composition means display glyphs with
		   padding space on the left or right.  */
		if (c == '\t')
		  continue;
		if (char_charset (c, charset_list, NULL))
		  {
		    if (CHAR_WIDTH (c) == 0
			&& i > 0 && COMPOSITION_GLYPH (cmp, i - 1) == '\t')
		      /* Should be left-padded */
		      {
			buf += CHAR_STRING (' ', buf);
			nchars++;
		      }
		  }
		else
		  c = '?';
		buf += CHAR_STRING (c, buf);
		nchars++;
	      }
	}
      /* We must skip glyphs to be padded for a wide character.  */
      else if (! CHAR_GLYPH_PADDING_P (*src))
	{
	  GLYPH g;
	  int c IF_LINT (= 0);
	  Lisp_Object string;

	  string = Qnil;
	  SET_GLYPH_FROM_CHAR_GLYPH (g, src[0]);

	  if (GLYPH_INVALID_P (g) || GLYPH_SIMPLE_P (tbase, tlen, g))
	    {
	      /* This glyph doesn't have an entry in Vglyph_table.  */
	      c = src->u.ch;
	    }
	  else
	    {
	      /* This glyph has an entry in Vglyph_table,
		 so process any alias before testing for simpleness.  */
	      GLYPH_FOLLOW_ALIASES (tbase, tlen, g);

	      if (GLYPH_SIMPLE_P (tbase, tlen, g))
		/* We set the multi-byte form of a character in G
		   (that should be an ASCII character) at WORKBUF.  */
		c = GLYPH_CHAR (g);
	      else
		/* We have a string in Vglyph_table.  */
		string = tbase[GLYPH_CHAR (g)];
	    }

	  if (NILP (string))
	    {
	      nbytes = buf - encode_terminal_src;
	      if (encode_terminal_src_size - nbytes < MAX_MULTIBYTE_LENGTH)
		{
		  encode_terminal_src =
		    xpalloc (encode_terminal_src, &encode_terminal_src_size,
			     MAX_MULTIBYTE_LENGTH, -1, 1);
		  buf = encode_terminal_src + nbytes;
		}
	      if (CHAR_BYTE8_P (c)
		  || char_charset (c, charset_list, NULL))
		{
		  /* Store the multibyte form of C at BUF.  */
		  buf += CHAR_STRING (c, buf);
		  nchars++;
		}
	      else
		{
		  /* C is not encodable.  */
		  *buf++ = '?';
		  nchars++;
		  while (src + 1 < src_end && CHAR_GLYPH_PADDING_P (src[1]))
		    {
		      *buf++ = '?';
		      nchars++;
		      src++;
		    }
		}
	    }
	  else
	    {
	      if (! STRING_MULTIBYTE (string))
		string = string_to_multibyte (string);
	      nbytes = buf - encode_terminal_src;
	      if (encode_terminal_src_size - nbytes < SBYTES (string))
		{
		  encode_terminal_src =
		    xpalloc (encode_terminal_src, &encode_terminal_src_size,
			     (SBYTES (string)
			      - (encode_terminal_src_size - nbytes)),
			     -1, 1);
		  buf = encode_terminal_src + nbytes;
		}
	      memcpy (buf, SDATA (string), SBYTES (string));
	      buf += SBYTES (string);
	      nchars += SCHARS (string);
	    }
	}
      src++;
    }

  if (nchars == 0)
    {
      coding->produced = 0;
      return NULL;
    }

  nbytes = buf - encode_terminal_src;
  coding->source = encode_terminal_src;
  if (encode_terminal_dst_size == 0)
    {
      encode_terminal_dst = xrealloc (encode_terminal_dst,
				      encode_terminal_src_size);
      encode_terminal_dst_size = encode_terminal_src_size;
    }
  coding->destination = encode_terminal_dst;
  coding->dst_bytes = encode_terminal_dst_size;
  encode_coding_object (coding, Qnil, 0, 0, nchars, nbytes, Qnil);
  /* coding->destination may have been reallocated.  */
  encode_terminal_dst = coding->destination;
  encode_terminal_dst_size = coding->dst_bytes;

  return (encode_terminal_dst);
}



/* An implementation of write_glyphs for termcap frames. */

static void
tty_write_glyphs (struct frame *f, struct glyph *string, int len)
{
  unsigned char *conversion_buffer;
  struct coding_system *coding;
  size_t n, stringlen;

  struct tty_display_info *tty = FRAME_TTY (f);

  tty_turn_off_insert (tty);
  tty_hide_cursor (tty);

  /* Don't dare write in last column of bottom line, if Auto-Wrap,
     since that would scroll the whole frame on some terminals.  */

  if (AutoWrap (tty)
      && curY (tty) + 1 == FRAME_LINES (f)
      && (curX (tty) + len) == FRAME_COLS (f))
    len --;
  if (len <= 0)
    return;

  cmplus (tty, len);

  /* If terminal_coding does any conversion, use it, otherwise use
     safe_terminal_coding.  We can't use CODING_REQUIRE_ENCODING here
     because it always return 1 if the member src_multibyte is 1.  */
  coding = (FRAME_TERMINAL_CODING (f)->common_flags & CODING_REQUIRE_ENCODING_MASK
	    ? FRAME_TERMINAL_CODING (f) : &safe_terminal_coding);
  /* The mode bit CODING_MODE_LAST_BLOCK should be set to 1 only at
     the tail.  */
  coding->mode &= ~CODING_MODE_LAST_BLOCK;

  for (stringlen = len; stringlen != 0; stringlen -= n)
    {
      /* Identify a run of glyphs with the same face.  */
      int face_id = string->face_id;

      for (n = 1; n < stringlen; ++n)
	if (string[n].face_id != face_id)
	  break;

      /* Turn appearance modes of the face of the run on.  */
      tty_highlight_if_desired (tty);
      turn_on_face (f, face_id);

      if (n == stringlen)
	/* This is the last run.  */
	coding->mode |= CODING_MODE_LAST_BLOCK;
      conversion_buffer = encode_terminal_code (string, n, coding);
      if (coding->produced > 0)
	{
	  BLOCK_INPUT;
	  fwrite (conversion_buffer, 1, coding->produced, tty->output);
	  if (ferror (tty->output))
	    clearerr (tty->output);
	  if (tty->termscript)
	    fwrite (conversion_buffer, 1, coding->produced, tty->termscript);
	  UNBLOCK_INPUT;
	}
      string += n;

      /* Turn appearance modes off.  */
      turn_off_face (f, face_id);
      tty_turn_off_highlight (tty);
    }

  cmcheckmagic (tty);
}

#ifdef HAVE_GPM			/* Only used by GPM code.  */

static void
tty_write_glyphs_with_face (register struct frame *f, register struct glyph *string,
			    register int len, register int face_id)
{
  unsigned char *conversion_buffer;
  struct coding_system *coding;

  struct tty_display_info *tty = FRAME_TTY (f);

  tty_turn_off_insert (tty);
  tty_hide_cursor (tty);

  /* Don't dare write in last column of bottom line, if Auto-Wrap,
     since that would scroll the whole frame on some terminals.  */

  if (AutoWrap (tty)
      && curY (tty) + 1 == FRAME_LINES (f)
      && (curX (tty) + len) == FRAME_COLS (f))
    len --;
  if (len <= 0)
    return;

  cmplus (tty, len);

  /* If terminal_coding does any conversion, use it, otherwise use
     safe_terminal_coding.  We can't use CODING_REQUIRE_ENCODING here
     because it always return 1 if the member src_multibyte is 1.  */
  coding = (FRAME_TERMINAL_CODING (f)->common_flags & CODING_REQUIRE_ENCODING_MASK
	    ? FRAME_TERMINAL_CODING (f) : &safe_terminal_coding);
  /* The mode bit CODING_MODE_LAST_BLOCK should be set to 1 only at
     the tail.  */
  coding->mode &= ~CODING_MODE_LAST_BLOCK;

  /* Turn appearance modes of the face.  */
  tty_highlight_if_desired (tty);
  turn_on_face (f, face_id);

  coding->mode |= CODING_MODE_LAST_BLOCK;
  conversion_buffer = encode_terminal_code (string, len, coding);
  if (coding->produced > 0)
    {
      BLOCK_INPUT;
      fwrite (conversion_buffer, 1, coding->produced, tty->output);
      if (ferror (tty->output))
	clearerr (tty->output);
      if (tty->termscript)
	fwrite (conversion_buffer, 1, coding->produced, tty->termscript);
      UNBLOCK_INPUT;
    }

  /* Turn appearance modes off.  */
  turn_off_face (f, face_id);
  tty_turn_off_highlight (tty);

  cmcheckmagic (tty);
}
#endif

/* An implementation of insert_glyphs for termcap frames. */

static void
tty_insert_glyphs (struct frame *f, struct glyph *start, int len)
{
  char *buf;
  struct glyph *glyph = NULL;
  unsigned char *conversion_buffer;
  unsigned char space[1];
  struct coding_system *coding;

  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->TS_ins_multi_chars)
    {
      buf = tparam (tty->TS_ins_multi_chars, 0, 0, len, 0, 0, 0);
      OUTPUT1 (tty, buf);
      xfree (buf);
      if (start)
	write_glyphs (f, start, len);
      return;
    }

  tty_turn_on_insert (tty);
  cmplus (tty, len);

  if (! start)
    space[0] = SPACEGLYPH;

  /* If terminal_coding does any conversion, use it, otherwise use
     safe_terminal_coding.  We can't use CODING_REQUIRE_ENCODING here
     because it always return 1 if the member src_multibyte is 1.  */
  coding = (FRAME_TERMINAL_CODING (f)->common_flags & CODING_REQUIRE_ENCODING_MASK
	    ? FRAME_TERMINAL_CODING (f) : &safe_terminal_coding);
  /* The mode bit CODING_MODE_LAST_BLOCK should be set to 1 only at
     the tail.  */
  coding->mode &= ~CODING_MODE_LAST_BLOCK;

  while (len-- > 0)
    {
      OUTPUT1_IF (tty, tty->TS_ins_char);
      if (!start)
	{
	  conversion_buffer = space;
	  coding->produced = 1;
	}
      else
	{
	  tty_highlight_if_desired (tty);
	  turn_on_face (f, start->face_id);
	  glyph = start;
	  ++start;
	  /* We must open sufficient space for a character which
	     occupies more than one column.  */
	  while (len && CHAR_GLYPH_PADDING_P (*start))
	    {
	      OUTPUT1_IF (tty, tty->TS_ins_char);
	      start++, len--;
	    }

	  if (len <= 0)
	    /* This is the last glyph.  */
	    coding->mode |= CODING_MODE_LAST_BLOCK;

          conversion_buffer = encode_terminal_code (glyph, 1, coding);
	}

      if (coding->produced > 0)
	{
	  BLOCK_INPUT;
	  fwrite (conversion_buffer, 1, coding->produced, tty->output);
	  if (ferror (tty->output))
	    clearerr (tty->output);
	  if (tty->termscript)
	    fwrite (conversion_buffer, 1, coding->produced, tty->termscript);
	  UNBLOCK_INPUT;
	}

      OUTPUT1_IF (tty, tty->TS_pad_inserted_char);
      if (start)
	{
	  turn_off_face (f, glyph->face_id);
	  tty_turn_off_highlight (tty);
	}
    }

  cmcheckmagic (tty);
}

/* An implementation of delete_glyphs for termcap frames. */

static void
tty_delete_glyphs (struct frame *f, int n)
{
  char *buf;
  register int i;

  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->delete_in_insert_mode)
    {
      tty_turn_on_insert (tty);
    }
  else
    {
      tty_turn_off_insert (tty);
      OUTPUT_IF (tty, tty->TS_delete_mode);
    }

  if (tty->TS_del_multi_chars)
    {
      buf = tparam (tty->TS_del_multi_chars, 0, 0, n, 0, 0, 0);
      OUTPUT1 (tty, buf);
      xfree (buf);
    }
  else
    for (i = 0; i < n; i++)
      OUTPUT1 (tty, tty->TS_del_char);
  if (!tty->delete_in_insert_mode)
    OUTPUT_IF (tty, tty->TS_end_delete_mode);
}

/* An implementation of ins_del_lines for termcap frames. */

static void
tty_ins_del_lines (struct frame *f, int vpos, int n)
{
  struct tty_display_info *tty = FRAME_TTY (f);
  const char *multi =
    n > 0 ? tty->TS_ins_multi_lines : tty->TS_del_multi_lines;
  const char *single = n > 0 ? tty->TS_ins_line : tty->TS_del_line;
  const char *scroll = n > 0 ? tty->TS_rev_scroll : tty->TS_fwd_scroll;

  register int i = n > 0 ? n : -n;
  register char *buf;

  /* If the lines below the insertion are being pushed
     into the end of the window, this is the same as clearing;
     and we know the lines are already clear, since the matching
     deletion has already been done.  So can ignore this.  */
  /* If the lines below the deletion are blank lines coming
     out of the end of the window, don't bother,
     as there will be a matching inslines later that will flush them. */
  if (FRAME_SCROLL_REGION_OK (f)
      && vpos + i >= tty->specified_window)
    return;
  if (!FRAME_MEMORY_BELOW_FRAME (f)
      && vpos + i >= FRAME_LINES (f))
    return;

  if (multi)
    {
      raw_cursor_to (f, vpos, 0);
      tty_background_highlight (tty);
      buf = tparam (multi, 0, 0, i, 0, 0, 0);
      OUTPUT (tty, buf);
      xfree (buf);
    }
  else if (single)
    {
      raw_cursor_to (f, vpos, 0);
      tty_background_highlight (tty);
      while (--i >= 0)
        OUTPUT (tty, single);
      if (tty->TF_teleray)
        curX (tty) = 0;
    }
  else
    {
      tty_set_scroll_region (f, vpos, tty->specified_window);
      if (n < 0)
        raw_cursor_to (f, tty->specified_window - 1, 0);
      else
        raw_cursor_to (f, vpos, 0);
      tty_background_highlight (tty);
      while (--i >= 0)
        OUTPUTL (tty, scroll, tty->specified_window - vpos);
      tty_set_scroll_region (f, 0, tty->specified_window);
    }

  if (!FRAME_SCROLL_REGION_OK (f)
      && FRAME_MEMORY_BELOW_FRAME (f)
      && n < 0)
    {
      cursor_to (f, FRAME_LINES (f) + n, 0);
      clear_to_end (f);
    }
}

/* Compute cost of sending "str", in characters,
   not counting any line-dependent padding.  */

int
string_cost (const char *str)
{
  cost = 0;
  if (str)
    tputs (str, 0, evalcost);
  return cost;
}

/* Compute cost of sending "str", in characters,
   counting any line-dependent padding at one line.  */

static int
string_cost_one_line (const char *str)
{
  cost = 0;
  if (str)
    tputs (str, 1, evalcost);
  return cost;
}

/* Compute per line amount of line-dependent padding,
   in tenths of characters.  */

int
per_line_cost (const char *str)
{
  cost = 0;
  if (str)
    tputs (str, 0, evalcost);
  cost = - cost;
  if (str)
    tputs (str, 10, evalcost);
  return cost;
}

/* char_ins_del_cost[n] is cost of inserting N characters.
   char_ins_del_cost[-n] is cost of deleting N characters.
   The length of this vector is based on max_frame_cols.  */

int *char_ins_del_vector;

#define char_ins_del_cost(f) (&char_ins_del_vector[FRAME_COLS ((f))])

/* ARGSUSED */
static void
calculate_ins_del_char_costs (struct frame *f)
{
  struct tty_display_info *tty = FRAME_TTY (f);
  int ins_startup_cost, del_startup_cost;
  int ins_cost_per_char, del_cost_per_char;
  register int i;
  register int *p;

  if (tty->TS_ins_multi_chars)
    {
      ins_cost_per_char = 0;
      ins_startup_cost = string_cost_one_line (tty->TS_ins_multi_chars);
    }
  else if (tty->TS_ins_char || tty->TS_pad_inserted_char
	   || (tty->TS_insert_mode && tty->TS_end_insert_mode))
    {
      ins_startup_cost = (30 * (string_cost (tty->TS_insert_mode)
				+ string_cost (tty->TS_end_insert_mode))) / 100;
      ins_cost_per_char = (string_cost_one_line (tty->TS_ins_char)
			   + string_cost_one_line (tty->TS_pad_inserted_char));
    }
  else
    {
      ins_startup_cost = 9999;
      ins_cost_per_char = 0;
    }

  if (tty->TS_del_multi_chars)
    {
      del_cost_per_char = 0;
      del_startup_cost = string_cost_one_line (tty->TS_del_multi_chars);
    }
  else if (tty->TS_del_char)
    {
      del_startup_cost = (string_cost (tty->TS_delete_mode)
			  + string_cost (tty->TS_end_delete_mode));
      if (tty->delete_in_insert_mode)
	del_startup_cost /= 2;
      del_cost_per_char = string_cost_one_line (tty->TS_del_char);
    }
  else
    {
      del_startup_cost = 9999;
      del_cost_per_char = 0;
    }

  /* Delete costs are at negative offsets */
  p = &char_ins_del_cost (f)[0];
  for (i = FRAME_COLS (f); --i >= 0;)
    *--p = (del_startup_cost += del_cost_per_char);

  /* Doing nothing is free */
  p = &char_ins_del_cost (f)[0];
  *p++ = 0;

  /* Insert costs are at positive offsets */
  for (i = FRAME_COLS (f); --i >= 0;)
    *p++ = (ins_startup_cost += ins_cost_per_char);
}

void
calculate_costs (struct frame *frame)
{
  FRAME_COST_BAUD_RATE (frame) = baud_rate;

  if (FRAME_TERMCAP_P (frame))
    {
      struct tty_display_info *tty = FRAME_TTY (frame);
      register const char *f = (tty->TS_set_scroll_region
				? tty->TS_set_scroll_region
				: tty->TS_set_scroll_region_1);

      FRAME_SCROLL_REGION_COST (frame) = string_cost (f);

      tty->costs_set = 1;

      /* These variables are only used for terminal stuff.  They are
         allocated once for the terminal frame of X-windows emacs, but not
         used afterwards.

         char_ins_del_vector (i.e., char_ins_del_cost) isn't used because
         X turns off char_ins_del_ok. */

      max_frame_cols = max (max_frame_cols, FRAME_COLS (frame));
      if ((min (PTRDIFF_MAX, SIZE_MAX) / sizeof (int) - 1) / 2
	  < max_frame_cols)
	memory_full (SIZE_MAX);

      char_ins_del_vector =
	xrealloc (char_ins_del_vector,
		  (sizeof (int) + 2 * sizeof (int) * max_frame_cols));

      memset (char_ins_del_vector, 0,
	      (sizeof (int) + 2 * sizeof (int) * max_frame_cols));


      if (f && (!tty->TS_ins_line && !tty->TS_del_line))
        do_line_insertion_deletion_costs (frame,
                                          tty->TS_rev_scroll, tty->TS_ins_multi_lines,
                                          tty->TS_fwd_scroll, tty->TS_del_multi_lines,
                                          f, f, 1);
      else
        do_line_insertion_deletion_costs (frame,
                                          tty->TS_ins_line, tty->TS_ins_multi_lines,
                                          tty->TS_del_line, tty->TS_del_multi_lines,
                                          0, 0, 1);

      calculate_ins_del_char_costs (frame);

      /* Don't use TS_repeat if its padding is worse than sending the chars */
      if (tty->TS_repeat && per_line_cost (tty->TS_repeat) * baud_rate < 9000)
        tty->RPov = string_cost (tty->TS_repeat);
      else
        tty->RPov = FRAME_COLS (frame) * 2;

      cmcostinit (FRAME_TTY (frame)); /* set up cursor motion costs */
    }
}

struct fkey_table {
  const char *cap, *name;
};

  /* Termcap capability names that correspond directly to X keysyms.
     Some of these (marked "terminfo") aren't supplied by old-style
     (Berkeley) termcap entries.  They're listed in X keysym order;
     except we put the keypad keys first, so that if they clash with
     other keys (as on the IBM PC keyboard) they get overridden.
  */

static const struct fkey_table keys[] =
{
  {"kh", "home"},	/* termcap */
  {"kl", "left"},	/* termcap */
  {"ku", "up"},		/* termcap */
  {"kr", "right"},	/* termcap */
  {"kd", "down"},	/* termcap */
  {"%8", "prior"},	/* terminfo */
  {"%5", "next"},	/* terminfo */
  {"@7", "end"},	/* terminfo */
  {"@1", "begin"},	/* terminfo */
  {"*6", "select"},	/* terminfo */
  {"%9", "print"},	/* terminfo */
  {"@4", "execute"},	/* terminfo --- actually the `command' key */
  /*
   * "insert" --- see below
   */
  {"&8", "undo"},	/* terminfo */
  {"%0", "redo"},	/* terminfo */
  {"%7", "menu"},	/* terminfo --- actually the `options' key */
  {"@0", "find"},	/* terminfo */
  {"@2", "cancel"},	/* terminfo */
  {"%1", "help"},	/* terminfo */
  /*
   * "break" goes here, but can't be reliably intercepted with termcap
   */
  {"&4", "reset"},	/* terminfo --- actually `restart' */
  /*
   * "system" and "user" --- no termcaps
   */
  {"kE", "clearline"},	/* terminfo */
  {"kA", "insertline"},	/* terminfo */
  {"kL", "deleteline"},	/* terminfo */
  {"kI", "insertchar"},	/* terminfo */
  {"kD", "deletechar"},	/* terminfo */
  {"kB", "backtab"},	/* terminfo */
  /*
   * "kp_backtab", "kp-space", "kp-tab" --- no termcaps
   */
  {"@8", "kp-enter"},	/* terminfo */
  /*
   * "kp-f1", "kp-f2", "kp-f3" "kp-f4",
   * "kp-multiply", "kp-add", "kp-separator",
   * "kp-subtract", "kp-decimal", "kp-divide", "kp-0";
   * --- no termcaps for any of these.
   */
  {"K4", "kp-1"},	/* terminfo */
  /*
   * "kp-2" --- no termcap
   */
  {"K5", "kp-3"},	/* terminfo */
  /*
   * "kp-4" --- no termcap
   */
  {"K2", "kp-5"},	/* terminfo */
  /*
   * "kp-6" --- no termcap
   */
  {"K1", "kp-7"},	/* terminfo */
  /*
   * "kp-8" --- no termcap
   */
  {"K3", "kp-9"},	/* terminfo */
  /*
   * "kp-equal" --- no termcap
   */
  {"k1", "f1"},
  {"k2", "f2"},
  {"k3", "f3"},
  {"k4", "f4"},
  {"k5", "f5"},
  {"k6", "f6"},
  {"k7", "f7"},
  {"k8", "f8"},
  {"k9", "f9"},

  {"&0", "S-cancel"},    /*shifted cancel key*/
  {"&9", "S-begin"},     /*shifted begin key*/
  {"*0", "S-find"},      /*shifted find key*/
  {"*1", "S-execute"},   /*shifted execute? actually shifted command key*/
  {"*4", "S-delete"},    /*shifted delete-character key*/
  {"*7", "S-end"},       /*shifted end key*/
  {"*8", "S-clearline"}, /*shifted clear-to end-of-line key*/
  {"#1", "S-help"},      /*shifted help key*/
  {"#2", "S-home"},      /*shifted home key*/
  {"#3", "S-insert"},    /*shifted insert-character key*/
  {"#4", "S-left"},      /*shifted left-arrow key*/
  {"%d", "S-menu"},      /*shifted menu? actually shifted options key*/
  {"%c", "S-next"},      /*shifted next key*/
  {"%e", "S-prior"},     /*shifted previous key*/
  {"%f", "S-print"},     /*shifted print key*/
  {"%g", "S-redo"},      /*shifted redo key*/
  {"%i", "S-right"},     /*shifted right-arrow key*/
  {"!3", "S-undo"}       /*shifted undo key*/
  };

#ifndef DOS_NT
static char **term_get_fkeys_address;
static KBOARD *term_get_fkeys_kboard;
static Lisp_Object term_get_fkeys_1 (void);

/* Find the escape codes sent by the function keys for Vinput_decode_map.
   This function scans the termcap function key sequence entries, and
   adds entries to Vinput_decode_map for each function key it finds.  */

static void
term_get_fkeys (char **address, KBOARD *kboard)
{
  /* We run the body of the function (term_get_fkeys_1) and ignore all Lisp
     errors during the call.  The only errors should be from Fdefine_key
     when given a key sequence containing an invalid prefix key.  If the
     termcap defines function keys which use a prefix that is already bound
     to a command by the default bindings, we should silently ignore that
     function key specification, rather than giving the user an error and
     refusing to run at all on such a terminal.  */

  term_get_fkeys_address = address;
  term_get_fkeys_kboard = kboard;
  internal_condition_case (term_get_fkeys_1, Qerror, Fidentity);
}

static Lisp_Object
term_get_fkeys_1 (void)
{
  int i;

  char **address = term_get_fkeys_address;
  KBOARD *kboard = term_get_fkeys_kboard;

  /* This can happen if CANNOT_DUMP or with strange options.  */
  if (!KEYMAPP (KVAR (kboard, Vinput_decode_map)))
    KVAR (kboard, Vinput_decode_map) = Fmake_sparse_keymap (Qnil);

  for (i = 0; i < (sizeof (keys)/sizeof (keys[0])); i++)
    {
      char *sequence = tgetstr (keys[i].cap, address);
      if (sequence)
	Fdefine_key (KVAR (kboard, Vinput_decode_map), build_string (sequence),
		     Fmake_vector (make_number (1),
				   intern (keys[i].name)));
    }

  /* The uses of the "k0" capability are inconsistent; sometimes it
     describes F10, whereas othertimes it describes F0 and "k;" describes F10.
     We will attempt to politely accommodate both systems by testing for
     "k;", and if it is present, assuming that "k0" denotes F0, otherwise F10.
     */
  {
    const char *k_semi  = tgetstr ("k;", address);
    const char *k0      = tgetstr ("k0", address);
    const char *k0_name = "f10";

    if (k_semi)
      {
	if (k0)
	  /* Define f0 first, so that f10 takes precedence in case the
	     key sequences happens to be the same.  */
	  Fdefine_key (KVAR (kboard, Vinput_decode_map), build_string (k0),
		       Fmake_vector (make_number (1), intern ("f0")));
	Fdefine_key (KVAR (kboard, Vinput_decode_map), build_string (k_semi),
		     Fmake_vector (make_number (1), intern ("f10")));
      }
    else if (k0)
      Fdefine_key (KVAR (kboard, Vinput_decode_map), build_string (k0),
		   Fmake_vector (make_number (1), intern (k0_name)));
  }

  /* Set up cookies for numbered function keys above f10. */
  {
    char fcap[3], fkey[4];

    fcap[0] = 'F'; fcap[2] = '\0';
    for (i = 11; i < 64; i++)
      {
	if (i <= 19)
	  fcap[1] = '1' + i - 11;
	else if (i <= 45)
	  fcap[1] = 'A' + i - 20;
	else
	  fcap[1] = 'a' + i - 46;

	{
	  char *sequence = tgetstr (fcap, address);
	  if (sequence)
	    {
	      sprintf (fkey, "f%d", i);
	      Fdefine_key (KVAR (kboard, Vinput_decode_map), build_string (sequence),
			   Fmake_vector (make_number (1),
					 intern (fkey)));
	    }
	}
      }
   }

  /*
   * Various mappings to try and get a better fit.
   */
  {
#define CONDITIONAL_REASSIGN(cap1, cap2, sym)				\
      if (!tgetstr (cap1, address))					\
	{								\
	  char *sequence = tgetstr (cap2, address);			\
	  if (sequence)                                                 \
	    Fdefine_key (KVAR (kboard, Vinput_decode_map), build_string (sequence), \
			 Fmake_vector (make_number (1),                 \
				       intern (sym)));                  \
	}

      /* if there's no key_next keycap, map key_npage to `next' keysym */
      CONDITIONAL_REASSIGN ("%5", "kN", "next");
      /* if there's no key_prev keycap, map key_ppage to `previous' keysym */
      CONDITIONAL_REASSIGN ("%8", "kP", "prior");
      /* if there's no key_dc keycap, map key_ic to `insert' keysym */
      CONDITIONAL_REASSIGN ("kD", "kI", "insert");
      /* if there's no key_end keycap, map key_ll to 'end' keysym */
      CONDITIONAL_REASSIGN ("@7", "kH", "end");

      /* IBM has their own non-standard dialect of terminfo.
	 If the standard name isn't found, try the IBM name.  */
      CONDITIONAL_REASSIGN ("kB", "KO", "backtab");
      CONDITIONAL_REASSIGN ("@4", "kJ", "execute"); /* actually "action" */
      CONDITIONAL_REASSIGN ("@4", "kc", "execute"); /* actually "command" */
      CONDITIONAL_REASSIGN ("%7", "ki", "menu");
      CONDITIONAL_REASSIGN ("@7", "kw", "end");
      CONDITIONAL_REASSIGN ("F1", "k<", "f11");
      CONDITIONAL_REASSIGN ("F2", "k>", "f12");
      CONDITIONAL_REASSIGN ("%1", "kq", "help");
      CONDITIONAL_REASSIGN ("*6", "kU", "select");
#undef CONDITIONAL_REASSIGN
  }

  return Qnil;
}
#endif /* not DOS_NT */


/***********************************************************************
		       Character Display Information
 ***********************************************************************/
static void append_glyph (struct it *);
static void append_composite_glyph (struct it *);
static void produce_composite_glyph (struct it *);
static void append_glyphless_glyph (struct it *, int, const char *);
static void produce_glyphless_glyph (struct it *, int, Lisp_Object);

/* Append glyphs to IT's glyph_row.  Called from produce_glyphs for
   terminal frames if IT->glyph_row != NULL.  IT->char_to_display is
   the character for which to produce glyphs; IT->face_id contains the
   character's face.  Padding glyphs are appended if IT->c has a
   IT->pixel_width > 1.  */

static void
append_glyph (struct it *it)
{
  struct glyph *glyph, *end;
  int i;

  xassert (it->glyph_row);
  glyph = (it->glyph_row->glyphs[it->area]
	   + it->glyph_row->used[it->area]);
  end = it->glyph_row->glyphs[1 + it->area];

  /* If the glyph row is reversed, we need to prepend the glyph rather
     than append it.  */
  if (it->glyph_row->reversed_p && it->area == TEXT_AREA)
    {
      struct glyph *g;
      int move_by = it->pixel_width;

      /* Make room for the new glyphs.  */
      if (move_by > end - glyph) /* don't overstep end of this area */
	move_by = end - glyph;
      for (g = glyph - 1; g >= it->glyph_row->glyphs[it->area]; g--)
	g[move_by] = *g;
      glyph = it->glyph_row->glyphs[it->area];
      end = glyph + move_by;
    }

  /* BIDI Note: we put the glyphs of a "multi-pixel" character left to
     right, even in the REVERSED_P case, since (a) all of its u.ch are
     identical, and (b) the PADDING_P flag needs to be set for the
     leftmost one, because we write to the terminal left-to-right.  */
  for (i = 0;
       i < it->pixel_width && glyph < end;
       ++i)
    {
      glyph->type = CHAR_GLYPH;
      glyph->pixel_width = 1;
      glyph->u.ch = it->char_to_display;
      glyph->face_id = it->face_id;
      glyph->padding_p = i > 0;
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      if (it->bidi_p)
	{
	  glyph->resolved_level = it->bidi_it.resolved_level;
	  if ((it->bidi_it.type & 7) != it->bidi_it.type)
	    abort ();
	  glyph->bidi_type = it->bidi_it.type;
	}
      else
	{
	  glyph->resolved_level = 0;
	  glyph->bidi_type = UNKNOWN_BT;
	}

      ++it->glyph_row->used[it->area];
      ++glyph;
    }
}

/* For external use.  */
void
tty_append_glyph (struct it *it)
{
  append_glyph (it);
}


/* Produce glyphs for the display element described by IT.  *IT
   specifies what we want to produce a glyph for (character, image, ...),
   and where in the glyph matrix we currently are (glyph row and hpos).
   produce_glyphs fills in output fields of *IT with information such as the
   pixel width and height of a character, and maybe output actual glyphs at
   the same time if IT->glyph_row is non-null.  For an overview, see
   the explanation in dispextern.h, before the definition of the
   display_element_type enumeration.

   produce_glyphs also stores the result of glyph width, ascent
   etc. computations in *IT.

   IT->glyph_row may be null, in which case produce_glyphs does not
   actually fill in the glyphs.  This is used in the move_* functions
   in xdisp.c for text width and height computations.

   Callers usually don't call produce_glyphs directly;
   instead they use the macro PRODUCE_GLYPHS.  */

void
produce_glyphs (struct it *it)
{
  /* If a hook is installed, let it do the work.  */

  /* Nothing but characters are supported on terminal frames.  */
  xassert (it->what == IT_CHARACTER
	   || it->what == IT_COMPOSITION
	   || it->what == IT_STRETCH
	   || it->what == IT_GLYPHLESS);

  if (it->what == IT_STRETCH)
    {
      produce_stretch_glyph (it);
      goto done;
    }

  if (it->what == IT_COMPOSITION)
    {
      produce_composite_glyph (it);
      goto done;
    }

  if (it->what == IT_GLYPHLESS)
    {
      produce_glyphless_glyph (it, 0, Qnil);
      goto done;
    }

  if (it->char_to_display >= 040 && it->char_to_display < 0177)
    {
      it->pixel_width = it->nglyphs = 1;
      if (it->glyph_row)
	append_glyph (it);
    }
  else if (it->char_to_display == '\n')
    it->pixel_width = it->nglyphs = 0;
  else if (it->char_to_display == '\t')
    {
      int absolute_x = (it->current_x
			+ it->continuation_lines_width);
      int next_tab_x
	= (((1 + absolute_x + it->tab_width - 1)
	    / it->tab_width)
	   * it->tab_width);
      int nspaces;

      /* If part of the TAB has been displayed on the previous line
	 which is continued now, continuation_lines_width will have
	 been incremented already by the part that fitted on the
	 continued line.  So, we will get the right number of spaces
	 here.  */
      nspaces = next_tab_x - absolute_x;

      if (it->glyph_row)
	{
	  int n = nspaces;

	  it->char_to_display = ' ';
	  it->pixel_width = it->len = 1;

	  while (n--)
	    append_glyph (it);
	}

      it->pixel_width = nspaces;
      it->nglyphs = nspaces;
    }
  else if (CHAR_BYTE8_P (it->char_to_display))
    {
      /* Coming here means that we must send the raw 8-bit byte as is
	 to the terminal.  Although there's no way to know how many
	 columns it occupies on a screen, it is a good assumption that
	 a single byte code has 1-column width.  */
      it->pixel_width = it->nglyphs = 1;
      if (it->glyph_row)
	append_glyph (it);
    }
  else
    {
      Lisp_Object charset_list = FRAME_TERMINAL (it->f)->charset_list;

      if (char_charset (it->char_to_display, charset_list, NULL))
	{
	  it->pixel_width = CHAR_WIDTH (it->char_to_display);
	  it->nglyphs = it->pixel_width;
	  if (it->glyph_row)
	    append_glyph (it);
	}
      else
	{
	  Lisp_Object acronym = lookup_glyphless_char_display (-1, it);

	  xassert (it->what == IT_GLYPHLESS);
	  produce_glyphless_glyph (it, 1, acronym);
	}
    }

 done:
  /* Advance current_x by the pixel width as a convenience for
     the caller.  */
  if (it->area == TEXT_AREA)
    it->current_x += it->pixel_width;
  it->ascent = it->max_ascent = it->phys_ascent = it->max_phys_ascent = 0;
  it->descent = it->max_descent = it->phys_descent = it->max_phys_descent = 1;
}

/* Append glyphs to IT's glyph_row for the composition IT->cmp_id.
   Called from produce_composite_glyph for terminal frames if
   IT->glyph_row != NULL.  IT->face_id contains the character's
   face.  */

static void
append_composite_glyph (struct it *it)
{
  struct glyph *glyph;

  xassert (it->glyph_row);
  glyph = it->glyph_row->glyphs[it->area] + it->glyph_row->used[it->area];
  if (glyph < it->glyph_row->glyphs[1 + it->area])
    {
      /* If the glyph row is reversed, we need to prepend the glyph
	 rather than append it.  */
      if (it->glyph_row->reversed_p && it->area == TEXT_AREA)
	{
	  struct glyph *g;

	  /* Make room for the new glyph.  */
	  for (g = glyph - 1; g >= it->glyph_row->glyphs[it->area]; g--)
	    g[1] = *g;
	  glyph = it->glyph_row->glyphs[it->area];
	}
      glyph->type = COMPOSITE_GLYPH;
      glyph->pixel_width = it->pixel_width;
      glyph->u.cmp.id = it->cmp_it.id;
      if (it->cmp_it.ch < 0)
	{
	  glyph->u.cmp.automatic = 0;
	  glyph->u.cmp.id = it->cmp_it.id;
	}
      else
	{
	  glyph->u.cmp.automatic = 1;
	  glyph->u.cmp.id = it->cmp_it.id;
	  glyph->slice.cmp.from = it->cmp_it.from;
	  glyph->slice.cmp.to = it->cmp_it.to - 1;
	}

      glyph->face_id = it->face_id;
      glyph->padding_p = 0;
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      if (it->bidi_p)
	{
	  glyph->resolved_level = it->bidi_it.resolved_level;
	  if ((it->bidi_it.type & 7) != it->bidi_it.type)
	    abort ();
	  glyph->bidi_type = it->bidi_it.type;
	}
      else
	{
	  glyph->resolved_level = 0;
	  glyph->bidi_type = UNKNOWN_BT;
	}

      ++it->glyph_row->used[it->area];
      ++glyph;
    }
}


/* Produce a composite glyph for iterator IT.  IT->cmp_id is the ID of
   the composition.  We simply produces components of the composition
   assuming that the terminal has a capability to layout/render it
   correctly.  */

static void
produce_composite_glyph (struct it *it)
{
  if (it->cmp_it.ch < 0)
    {
      struct composition *cmp = composition_table[it->cmp_it.id];

      it->pixel_width = cmp->width;
    }
  else
    {
      Lisp_Object gstring = composition_gstring_from_id (it->cmp_it.id);

      it->pixel_width = composition_gstring_width (gstring, it->cmp_it.from,
						   it->cmp_it.to, NULL);
    }
  it->nglyphs = 1;
  if (it->glyph_row)
    append_composite_glyph (it);
}


/* Append a glyph for a glyphless character to IT->glyph_row.  FACE_ID
   is a face ID to be used for the glyph.  What is actually appended
   are glyphs of type CHAR_GLYPH whose characters are in STR (which
   comes from it->nglyphs bytes).  */

static void
append_glyphless_glyph (struct it *it, int face_id, const char *str)
{
  struct glyph *glyph, *end;
  int i;

  xassert (it->glyph_row);
  glyph = it->glyph_row->glyphs[it->area] + it->glyph_row->used[it->area];
  end = it->glyph_row->glyphs[1 + it->area];

  /* If the glyph row is reversed, we need to prepend the glyph rather
     than append it.  */
  if (it->glyph_row->reversed_p && it->area == TEXT_AREA)
    {
      struct glyph *g;
      int move_by = it->pixel_width;

      /* Make room for the new glyphs.  */
      if (move_by > end - glyph) /* don't overstep end of this area */
	move_by = end - glyph;
      for (g = glyph - 1; g >= it->glyph_row->glyphs[it->area]; g--)
	g[move_by] = *g;
      glyph = it->glyph_row->glyphs[it->area];
      end = glyph + move_by;
    }

  if (glyph >= end)
    return;
  glyph->type = CHAR_GLYPH;
  glyph->pixel_width = 1;
  glyph->face_id = face_id;
  glyph->padding_p = 0;
  glyph->charpos = CHARPOS (it->position);
  glyph->object = it->object;
  if (it->bidi_p)
    {
      glyph->resolved_level = it->bidi_it.resolved_level;
      if ((it->bidi_it.type & 7) != it->bidi_it.type)
	abort ();
      glyph->bidi_type = it->bidi_it.type;
    }
  else
    {
      glyph->resolved_level = 0;
      glyph->bidi_type = UNKNOWN_BT;
    }

  /* BIDI Note: we put the glyphs of characters left to right, even in
     the REVERSED_P case because we write to the terminal
     left-to-right.  */
  for (i = 0; i < it->nglyphs && glyph < end; ++i)
    {
      if (i > 0)
	glyph[0] = glyph[-1];
      glyph->u.ch = str[i];
      ++it->glyph_row->used[it->area];
      ++glyph;
    }
}

/* Produce glyphs for a glyphless character for iterator IT.
   IT->glyphless_method specifies which method to use for displaying
   the character.  See the description of enum
   glyphless_display_method in dispextern.h for the details.

   FOR_NO_FONT is nonzero if and only if this is for a character that
   is not supported by the coding system of the terminal.  ACRONYM, if
   non-nil, is an acronym string for the character.

   The glyphs actually produced are of type CHAR_GLYPH.  */

static void
produce_glyphless_glyph (struct it *it, int for_no_font, Lisp_Object acronym)
{
  int face_id;
  int len;
  char buf[sizeof "\\x" + max (6, (sizeof it->c * CHAR_BIT + 3) / 4)];
  char const *str = "    ";

  /* Get a face ID for the glyph by utilizing a cache (the same way as
     done for `escape-glyph' in get_next_display_element).  */
  if (it->f == last_glyphless_glyph_frame
      && it->face_id == last_glyphless_glyph_face_id)
    {
      face_id = last_glyphless_glyph_merged_face_id;
    }
  else
    {
      /* Merge the `glyphless-char' face into the current face.  */
      face_id = merge_faces (it->f, Qglyphless_char, 0, it->face_id);
      last_glyphless_glyph_frame = it->f;
      last_glyphless_glyph_face_id = it->face_id;
      last_glyphless_glyph_merged_face_id = face_id;
    }

  if (it->glyphless_method == GLYPHLESS_DISPLAY_THIN_SPACE)
    {
      /* As there's no way to produce a thin space, we produce a space
	 of canonical width.  */
      len = 1;
    }
  else if (it->glyphless_method == GLYPHLESS_DISPLAY_EMPTY_BOX)
    {
      len = CHAR_WIDTH (it->c);
      if (len == 0)
	len = 1;
      else if (len > 4)
	len = 4;
      sprintf (buf, "[%.*s]", len, str);
      len += 2;
      str = buf;
    }
  else
    {
      if (it->glyphless_method == GLYPHLESS_DISPLAY_ACRONYM)
	{
	  if (! STRINGP (acronym) && CHAR_TABLE_P (Vglyphless_char_display))
	    acronym = CHAR_TABLE_REF (Vglyphless_char_display, it->c);
	  if (CONSP (acronym))
	    acronym = XCDR (acronym);
	  buf[0] = '[';
	  str = STRINGP (acronym) ? SSDATA (acronym) : "";
	  for (len = 0; len < 6 && str[len] && ASCII_BYTE_P (str[len]); len++)
	    buf[1 + len] = str[len];
	  buf[1 + len] = ']';
	  len += 2;
	}
      else
	{
	  xassert (it->glyphless_method == GLYPHLESS_DISPLAY_HEX_CODE);
	  len = (it->c < 0x10000 ? sprintf (buf, "\\u%04X", it->c)
		 : it->c <= MAX_UNICODE_CHAR ? sprintf (buf, "\\U%06X", it->c)
		 : sprintf (buf, "\\x%06X", it->c));
	}
      str = buf;
    }

  it->pixel_width = len;
  it->nglyphs = len;
  if (it->glyph_row)
    append_glyphless_glyph (it, face_id, str);
}


/* Get information about special display element WHAT in an
   environment described by IT.  WHAT is one of IT_TRUNCATION or
   IT_CONTINUATION.  Maybe produce glyphs for WHAT if IT has a
   non-null glyph_row member.  This function ensures that fields like
   face_id, c, len of IT are left untouched.  */

void
produce_special_glyphs (struct it *it, enum display_element_type what)
{
  struct it temp_it;
  Lisp_Object gc;
  GLYPH glyph;

  temp_it = *it;
  temp_it.dp = NULL;
  temp_it.what = IT_CHARACTER;
  temp_it.len = 1;
  temp_it.object = make_number (0);
  memset (&temp_it.current, 0, sizeof temp_it.current);

  if (what == IT_CONTINUATION)
    {
      /* Continuation glyph.  For R2L lines, we mirror it by hand.  */
      if (it->bidi_it.paragraph_dir == R2L)
	SET_GLYPH_FROM_CHAR (glyph, '/');
      else
	SET_GLYPH_FROM_CHAR (glyph, '\\');
      if (it->dp
	  && (gc = DISP_CONTINUE_GLYPH (it->dp), GLYPH_CODE_P (gc))
	  && GLYPH_CODE_CHAR_VALID_P (gc))
	{
	  /* FIXME: Should we mirror GC for R2L lines?  */
	  SET_GLYPH_FROM_GLYPH_CODE (glyph, gc);
	  spec_glyph_lookup_face (XWINDOW (it->window), &glyph);
	}
    }
  else if (what == IT_TRUNCATION)
    {
      /* Truncation glyph.  */
      SET_GLYPH_FROM_CHAR (glyph, '$');
      if (it->dp
	  && (gc = DISP_TRUNC_GLYPH (it->dp), GLYPH_CODE_P (gc))
	  && GLYPH_CODE_CHAR_VALID_P (gc))
	{
	  /* FIXME: Should we mirror GC for R2L lines?  */
	  SET_GLYPH_FROM_GLYPH_CODE (glyph, gc);
	  spec_glyph_lookup_face (XWINDOW (it->window), &glyph);
	}
    }
  else
    abort ();

  temp_it.c = temp_it.char_to_display = GLYPH_CHAR (glyph);
  temp_it.face_id = GLYPH_FACE (glyph);
  temp_it.len = CHAR_BYTES (temp_it.c);

  produce_glyphs (&temp_it);
  it->pixel_width = temp_it.pixel_width;
  it->nglyphs = temp_it.pixel_width;
}



/***********************************************************************
				Faces
 ***********************************************************************/

/* Value is non-zero if attribute ATTR may be used.  ATTR should be
   one of the enumerators from enum no_color_bit, or a bit set built
   from them.  Some display attributes may not be used together with
   color; the termcap capability `NC' specifies which ones.  */

#define MAY_USE_WITH_COLORS_P(tty, ATTR)                \
  (tty->TN_max_colors > 0				\
   ? (tty->TN_no_color_video & (ATTR)) == 0             \
   : 1)

/* Turn appearances of face FACE_ID on tty frame F on.
   FACE_ID is a realized face ID number, in the face cache.  */

static void
turn_on_face (struct frame *f, int face_id)
{
  struct face *face = FACE_FROM_ID (f, face_id);
  long fg = face->foreground;
  long bg = face->background;
  struct tty_display_info *tty = FRAME_TTY (f);

  /* Do this first because TS_end_standout_mode may be the same
     as TS_exit_attribute_mode, which turns all appearances off. */
  if (MAY_USE_WITH_COLORS_P (tty, NC_REVERSE))
    {
      if (tty->TN_max_colors > 0)
	{
	  if (fg >= 0 && bg >= 0)
	    {
	      /* If the terminal supports colors, we can set them
		 below without using reverse video.  The face's fg
		 and bg colors are set as they should appear on
		 the screen, i.e. they take the inverse-video'ness
		 of the face already into account.  */
	    }
	  else if (inverse_video)
	    {
	      if (fg == FACE_TTY_DEFAULT_FG_COLOR
		  || bg == FACE_TTY_DEFAULT_BG_COLOR)
		tty_toggle_highlight (tty);
	    }
	  else
	    {
	      if (fg == FACE_TTY_DEFAULT_BG_COLOR
		  || bg == FACE_TTY_DEFAULT_FG_COLOR)
		tty_toggle_highlight (tty);
	    }
	}
      else
	{
	  /* If we can't display colors, use reverse video
	     if the face specifies that.  */
	  if (inverse_video)
	    {
	      if (fg == FACE_TTY_DEFAULT_FG_COLOR
		  || bg == FACE_TTY_DEFAULT_BG_COLOR)
		tty_toggle_highlight (tty);
	    }
	  else
	    {
	      if (fg == FACE_TTY_DEFAULT_BG_COLOR
		  || bg == FACE_TTY_DEFAULT_FG_COLOR)
		tty_toggle_highlight (tty);
	    }
	}
    }

  if (face->tty_bold_p && MAY_USE_WITH_COLORS_P (tty, NC_BOLD))
    OUTPUT1_IF (tty, tty->TS_enter_bold_mode);

  if (face->tty_dim_p && MAY_USE_WITH_COLORS_P (tty, NC_DIM))
    OUTPUT1_IF (tty, tty->TS_enter_dim_mode);

  /* Alternate charset and blinking not yet used.  */
  if (face->tty_alt_charset_p
      && MAY_USE_WITH_COLORS_P (tty, NC_ALT_CHARSET))
    OUTPUT1_IF (tty, tty->TS_enter_alt_charset_mode);

  if (face->tty_blinking_p
      && MAY_USE_WITH_COLORS_P (tty, NC_BLINK))
    OUTPUT1_IF (tty, tty->TS_enter_blink_mode);

  if (face->tty_underline_p && MAY_USE_WITH_COLORS_P (tty, NC_UNDERLINE))
    OUTPUT1_IF (tty, tty->TS_enter_underline_mode);

  if (tty->TN_max_colors > 0)
    {
      const char *ts;
      char *p;

      ts = tty->standout_mode ? tty->TS_set_background : tty->TS_set_foreground;
      if (fg >= 0 && ts)
	{
          p = tparam (ts, NULL, 0, (int) fg, 0, 0, 0);
	  OUTPUT (tty, p);
	  xfree (p);
	}

      ts = tty->standout_mode ? tty->TS_set_foreground : tty->TS_set_background;
      if (bg >= 0 && ts)
	{
          p = tparam (ts, NULL, 0, (int) bg, 0, 0, 0);
	  OUTPUT (tty, p);
	  xfree (p);
	}
    }
}


/* Turn off appearances of face FACE_ID on tty frame F.  */

static void
turn_off_face (struct frame *f, int face_id)
{
  struct face *face = FACE_FROM_ID (f, face_id);
  struct tty_display_info *tty = FRAME_TTY (f);

  xassert (face != NULL);

  if (tty->TS_exit_attribute_mode)
    {
      /* Capability "me" will turn off appearance modes double-bright,
	 half-bright, reverse-video, standout, underline.  It may or
	 may not turn off alt-char-mode.  */
      if (face->tty_bold_p
	  || face->tty_dim_p
	  || face->tty_reverse_p
	  || face->tty_alt_charset_p
	  || face->tty_blinking_p
	  || face->tty_underline_p)
	{
	  OUTPUT1_IF (tty, tty->TS_exit_attribute_mode);
	  if (strcmp (tty->TS_exit_attribute_mode, tty->TS_end_standout_mode) == 0)
	    tty->standout_mode = 0;
	}

      if (face->tty_alt_charset_p)
	OUTPUT_IF (tty, tty->TS_exit_alt_charset_mode);
    }
  else
    {
      /* If we don't have "me" we can only have those appearances
	 that have exit sequences defined.  */
      if (face->tty_alt_charset_p)
	OUTPUT_IF (tty, tty->TS_exit_alt_charset_mode);

      if (face->tty_underline_p)
	OUTPUT_IF (tty, tty->TS_exit_underline_mode);
    }

  /* Switch back to default colors.  */
  if (tty->TN_max_colors > 0
      && ((face->foreground != FACE_TTY_DEFAULT_COLOR
	   && face->foreground != FACE_TTY_DEFAULT_FG_COLOR)
	  || (face->background != FACE_TTY_DEFAULT_COLOR
	      && face->background != FACE_TTY_DEFAULT_BG_COLOR)))
    OUTPUT1_IF (tty, tty->TS_orig_pair);
}


/* Return non-zero if the terminal on frame F supports all of the
   capabilities in CAPS simultaneously, with foreground and background
   colors FG and BG.  */

int
tty_capable_p (struct tty_display_info *tty, unsigned int caps,
	       unsigned long fg, unsigned long bg)
{
#define TTY_CAPABLE_P_TRY(tty, cap, TS, NC_bit)				\
  if ((caps & (cap)) && (!(TS) || !MAY_USE_WITH_COLORS_P(tty, NC_bit)))	\
    return 0;

  TTY_CAPABLE_P_TRY (tty, TTY_CAP_INVERSE,	tty->TS_standout_mode, 	 	NC_REVERSE);
  TTY_CAPABLE_P_TRY (tty, TTY_CAP_UNDERLINE, 	tty->TS_enter_underline_mode, 	NC_UNDERLINE);
  TTY_CAPABLE_P_TRY (tty, TTY_CAP_BOLD, 	tty->TS_enter_bold_mode, 	NC_BOLD);
  TTY_CAPABLE_P_TRY (tty, TTY_CAP_DIM, 		tty->TS_enter_dim_mode, 	NC_DIM);
  TTY_CAPABLE_P_TRY (tty, TTY_CAP_BLINK, 	tty->TS_enter_blink_mode, 	NC_BLINK);
  TTY_CAPABLE_P_TRY (tty, TTY_CAP_ALT_CHARSET, 	tty->TS_enter_alt_charset_mode, NC_ALT_CHARSET);

  /* We can do it!  */
  return 1;
}

/* Return non-zero if the terminal is capable to display colors.  */

DEFUN ("tty-display-color-p", Ftty_display_color_p, Stty_display_color_p,
       0, 1, 0,
       doc: /* Return non-nil if the tty device TERMINAL can display colors.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  This function always returns nil if
TERMINAL does not refer to a text-only terminal.  */)
  (Lisp_Object terminal)
{
  struct terminal *t = get_tty_terminal (terminal, 0);
  if (!t)
    return Qnil;
  else
    return t->display_info.tty->TN_max_colors > 0 ? Qt : Qnil;
}

/* Return the number of supported colors.  */
DEFUN ("tty-display-color-cells", Ftty_display_color_cells,
       Stty_display_color_cells, 0, 1, 0,
       doc: /* Return the number of colors supported by the tty device TERMINAL.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  This function always returns 0 if
TERMINAL does not refer to a text-only terminal.  */)
  (Lisp_Object terminal)
{
  struct terminal *t = get_tty_terminal (terminal, 0);
  if (!t)
    return make_number (0);
  else
    return make_number (t->display_info.tty->TN_max_colors);
}

#ifndef DOS_NT

/* Declare here rather than in the function, as in the rest of Emacs,
   to work around an HPUX compiler bug (?). See
   http://lists.gnu.org/archive/html/emacs-devel/2007-08/msg00410.html  */
static int default_max_colors;
static int default_max_pairs;
static int default_no_color_video;
static char *default_orig_pair;
static char *default_set_foreground;
static char *default_set_background;

/* Save or restore the default color-related capabilities of this
   terminal.  */
static void
tty_default_color_capabilities (struct tty_display_info *tty, int save)
{

  if (save)
    {
      xfree (default_orig_pair);
      default_orig_pair = tty->TS_orig_pair ? xstrdup (tty->TS_orig_pair) : NULL;

      xfree (default_set_foreground);
      default_set_foreground = tty->TS_set_foreground ? xstrdup (tty->TS_set_foreground)
			       : NULL;

      xfree (default_set_background);
      default_set_background = tty->TS_set_background ? xstrdup (tty->TS_set_background)
			       : NULL;

      default_max_colors = tty->TN_max_colors;
      default_max_pairs = tty->TN_max_pairs;
      default_no_color_video = tty->TN_no_color_video;
    }
  else
    {
      tty->TS_orig_pair = default_orig_pair;
      tty->TS_set_foreground = default_set_foreground;
      tty->TS_set_background = default_set_background;
      tty->TN_max_colors = default_max_colors;
      tty->TN_max_pairs = default_max_pairs;
      tty->TN_no_color_video = default_no_color_video;
    }
}

/* Setup one of the standard tty color schemes according to MODE.
   MODE's value is generally the number of colors which we want to
   support; zero means set up for the default capabilities, the ones
   we saw at init_tty time; -1 means turn off color support.  */
static void
tty_setup_colors (struct tty_display_info *tty, int mode)
{
  /* Canonicalize all negative values of MODE.  */
  if (mode < -1)
    mode = -1;

  switch (mode)
    {
      case -1:	 /* no colors at all */
	tty->TN_max_colors = 0;
	tty->TN_max_pairs = 0;
	tty->TN_no_color_video = 0;
	tty->TS_set_foreground = tty->TS_set_background = tty->TS_orig_pair = NULL;
	break;
      case 0:	 /* default colors, if any */
      default:
	tty_default_color_capabilities (tty, 0);
	break;
      case 8:	/* 8 standard ANSI colors */
	tty->TS_orig_pair = "\033[0m";
#ifdef TERMINFO
	tty->TS_set_foreground = "\033[3%p1%dm";
	tty->TS_set_background = "\033[4%p1%dm";
#else
	tty->TS_set_foreground = "\033[3%dm";
	tty->TS_set_background = "\033[4%dm";
#endif
	tty->TN_max_colors = 8;
	tty->TN_max_pairs = 64;
	tty->TN_no_color_video = 0;
	break;
    }
}

void
set_tty_color_mode (struct tty_display_info *tty, struct frame *f)
{
  Lisp_Object tem, val;
  Lisp_Object color_mode;
  int mode;
  Lisp_Object tty_color_mode_alist
    = Fintern_soft (build_string ("tty-color-mode-alist"), Qnil);

  tem = assq_no_quit (Qtty_color_mode, f->param_alist);
  val = CONSP (tem) ? XCDR (tem) : Qnil;

  if (INTEGERP (val))
    color_mode = val;
  else if (SYMBOLP (tty_color_mode_alist))
    {
      tem = Fassq (val, Fsymbol_value (tty_color_mode_alist));
      color_mode = CONSP (tem) ? XCDR (tem) : Qnil;
    }
  else
    color_mode = Qnil;

  mode = INTEGERP (color_mode) ? XINT (color_mode) : 0;

  if (mode != tty->previous_color_mode)
    {
      Lisp_Object funsym = intern ("tty-set-up-initial-frame-faces");
      tty->previous_color_mode = mode;
      tty_setup_colors (tty , mode);
      /*  This recomputes all the faces given the new color definitions.  */
      safe_call (1, &funsym);
    }
}

#endif /* !DOS_NT */



/* Return the tty display object specified by TERMINAL. */

static struct terminal *
get_tty_terminal (Lisp_Object terminal, int throw)
{
  struct terminal *t = get_terminal (terminal, throw);

  if (t && t->type != output_termcap && t->type != output_msdos_raw)
    {
      if (throw)
        error ("Device %d is not a termcap terminal device", t->id);
      else
        return NULL;
    }

  return t;
}

/* Return an active termcap device that uses the tty device with the
   given name.

   This function ignores suspended devices.

   Returns NULL if the named terminal device is not opened.  */

struct terminal *
get_named_tty (const char *name)
{
  struct terminal *t;

  if (!name)
    abort ();

  for (t = terminal_list; t; t = t->next_terminal)
    {
      if ((t->type == output_termcap || t->type == output_msdos_raw)
          && !strcmp (t->display_info.tty->name, name)
          && TERMINAL_ACTIVE_P (t))
        return t;
    }

  return 0;
}


DEFUN ("tty-type", Ftty_type, Stty_type, 0, 1, 0,
       doc: /* Return the type of the tty device that TERMINAL uses.
Returns nil if TERMINAL is not on a tty device.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  */)
  (Lisp_Object terminal)
{
  struct terminal *t = get_terminal (terminal, 1);

  if (t->type != output_termcap && t->type != output_msdos_raw)
    return Qnil;

  if (t->display_info.tty->type)
    return build_string (t->display_info.tty->type);
  else
    return Qnil;
}

DEFUN ("controlling-tty-p", Fcontrolling_tty_p, Scontrolling_tty_p, 0, 1, 0,
       doc: /* Return non-nil if TERMINAL is the controlling tty of the Emacs process.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  This function always returns nil if
TERMINAL is not on a tty device.  */)
  (Lisp_Object terminal)
{
  struct terminal *t = get_terminal (terminal, 1);

  if ((t->type != output_termcap && t->type != output_msdos_raw)
      || strcmp (t->display_info.tty->name, DEV_TTY) != 0)
    return Qnil;
  else
    return Qt;
}

DEFUN ("tty-no-underline", Ftty_no_underline, Stty_no_underline, 0, 1, 0,
       doc: /* Declare that the tty used by TERMINAL does not handle underlining.
This is used to override the terminfo data, for certain terminals that
do not really do underlining, but say that they do.  This function has
no effect if used on a non-tty terminal.

TERMINAL can be a terminal object, a frame or nil (meaning the
selected frame's terminal).  This function always returns nil if
TERMINAL does not refer to a text-only terminal.  */)
  (Lisp_Object terminal)
{
  struct terminal *t = get_terminal (terminal, 1);

  if (t->type == output_termcap)
    t->display_info.tty->TS_enter_underline_mode = 0;
  return Qnil;
}



DEFUN ("suspend-tty", Fsuspend_tty, Ssuspend_tty, 0, 1, 0,
       doc: /* Suspend the terminal device TTY.

The device is restored to its default state, and Emacs ceases all
access to the tty device.  Frames that use the device are not deleted,
but input is not read from them and if they change, their display is
not updated.

TTY may be a terminal object, a frame, or nil for the terminal device
of the currently selected frame.

This function runs `suspend-tty-functions' after suspending the
device.  The functions are run with one arg, the id of the suspended
terminal device.

`suspend-tty' does nothing if it is called on a device that is already
suspended.

A suspended tty may be resumed by calling `resume-tty' on it.  */)
  (Lisp_Object tty)
{
  struct terminal *t = get_tty_terminal (tty, 1);
  FILE *f;

  if (!t)
    error ("Unknown tty device");

  f = t->display_info.tty->input;

  if (f)
    {
      /* First run `suspend-tty-functions' and then clean up the tty
	 state because `suspend-tty-functions' might need to change
	 the tty state.  */
      Lisp_Object args[2];
      args[0] = intern ("suspend-tty-functions");
      XSETTERMINAL (args[1], t);
      Frun_hook_with_args (2, args);

      reset_sys_modes (t->display_info.tty);
      delete_keyboard_wait_descriptor (fileno (f));

#ifndef MSDOS
      fclose (f);
      if (f != t->display_info.tty->output)
        fclose (t->display_info.tty->output);
#endif

      t->display_info.tty->input = 0;
      t->display_info.tty->output = 0;

      if (FRAMEP (t->display_info.tty->top_frame))
        FRAME_SET_VISIBLE (XFRAME (t->display_info.tty->top_frame), 0);

    }

  /* Clear display hooks to prevent further output.  */
  clear_tty_hooks (t);

  return Qnil;
}

DEFUN ("resume-tty", Fresume_tty, Sresume_tty, 0, 1, 0,
       doc: /* Resume the previously suspended terminal device TTY.
The terminal is opened and reinitialized.  Frames that are on the
suspended terminal are revived.

It is an error to resume a terminal while another terminal is active
on the same device.

This function runs `resume-tty-functions' after resuming the terminal.
The functions are run with one arg, the id of the resumed terminal
device.

`resume-tty' does nothing if it is called on a device that is not
suspended.

TTY may be a terminal object, a frame, or nil (meaning the selected
frame's terminal). */)
  (Lisp_Object tty)
{
  struct terminal *t = get_tty_terminal (tty, 1);
  int fd;

  if (!t)
    error ("Unknown tty device");

  if (!t->display_info.tty->input)
    {
      if (get_named_tty (t->display_info.tty->name))
        error ("Cannot resume display while another display is active on the same device");

#ifdef MSDOS
      t->display_info.tty->output = stdout;
      t->display_info.tty->input  = stdin;
#else  /* !MSDOS */
      fd = emacs_open (t->display_info.tty->name, O_RDWR | O_NOCTTY, 0);

      if (fd == -1)
        error ("Can not reopen tty device %s: %s", t->display_info.tty->name, strerror (errno));

      if (strcmp (t->display_info.tty->name, DEV_TTY))
        dissociate_if_controlling_tty (fd);

      t->display_info.tty->output = fdopen (fd, "w+");
      t->display_info.tty->input = t->display_info.tty->output;
#endif

      add_keyboard_wait_descriptor (fd);

      if (FRAMEP (t->display_info.tty->top_frame))
	{
	  struct frame *f = XFRAME (t->display_info.tty->top_frame);
	  int width, height;
	  int old_height = FRAME_COLS (f);
	  int old_width = FRAME_LINES (f);

	  /* Check if terminal/window size has changed while the frame
	     was suspended.  */
	  get_tty_size (fileno (t->display_info.tty->input), &width, &height);
	  if (width != old_width || height != old_height)
	    change_frame_size (f, height, width, 0, 0, 0);
	  FRAME_SET_VISIBLE (XFRAME (t->display_info.tty->top_frame), 1);
	}

      set_tty_hooks (t);
      init_sys_modes (t->display_info.tty);

      {
        /* Run `resume-tty-functions'.  */
        Lisp_Object args[2];
        args[0] = intern ("resume-tty-functions");
        XSETTERMINAL (args[1], t);
        Frun_hook_with_args (2, args);
      }
    }

  set_tty_hooks (t);

  return Qnil;
}


/***********************************************************************
			       Mouse
 ***********************************************************************/

#ifdef HAVE_GPM

#ifndef HAVE_WINDOW_SYSTEM
void
term_mouse_moveto (int x, int y)
{
  /* TODO: how to set mouse position?
  const char *name;
  int fd;
  name = (const char *) ttyname (0);
  fd = open (name, O_WRONLY);
     SOME_FUNCTION (x, y, fd);
  close (fd);
  last_mouse_x = x;
  last_mouse_y = y;  */
}
#endif /* HAVE_WINDOW_SYSTEM */

/* Implementation of draw_row_with_mouse_face for TTY/GPM.  */
void
tty_draw_row_with_mouse_face (struct window *w, struct glyph_row *row,
			      int start_hpos, int end_hpos,
			      enum draw_glyphs_face draw)
{
  int nglyphs = end_hpos - start_hpos;
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct tty_display_info *tty = FRAME_TTY (f);
  int face_id = tty->mouse_highlight.mouse_face_face_id;
  int save_x, save_y, pos_x, pos_y;

  if (end_hpos >= row->used[TEXT_AREA])
    nglyphs = row->used[TEXT_AREA] - start_hpos;

  pos_y = row->y + WINDOW_TOP_EDGE_Y (w);
  pos_x = row->used[LEFT_MARGIN_AREA] + start_hpos + WINDOW_LEFT_EDGE_X (w);

  /* Save current cursor co-ordinates.  */
  save_y = curY (tty);
  save_x = curX (tty);
  cursor_to (f, pos_y, pos_x);

  if (draw == DRAW_MOUSE_FACE)
    tty_write_glyphs_with_face (f, row->glyphs[TEXT_AREA] + start_hpos,
				nglyphs, face_id);
  else if (draw == DRAW_NORMAL_TEXT)
    write_glyphs (f, row->glyphs[TEXT_AREA] + start_hpos, nglyphs);

  cursor_to (f, save_y, save_x);
}

static int
term_mouse_movement (FRAME_PTR frame, Gpm_Event *event)
{
  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  if (event->x != last_mouse_x || event->y != last_mouse_y)
    {
      frame->mouse_moved = 1;
      note_mouse_highlight (frame, event->x, event->y);
      /* Remember which glyph we're now on.  */
      last_mouse_x = event->x;
      last_mouse_y = event->y;
      return 1;
    }
  return 0;
}

/* Return the current position of the mouse.

   Set *f to the frame the mouse is in, or zero if the mouse is in no
   Emacs frame.  If it is set to zero, all the other arguments are
   garbage.

   Set *bar_window to Qnil, and *x and *y to the column and
   row of the character cell the mouse is over.

   Set *timeptr to the time the mouse was at the returned position.

   This clears mouse_moved until the next motion
   event arrives.  */
static void
term_mouse_position (FRAME_PTR *fp, int insist, Lisp_Object *bar_window,
		     enum scroll_bar_part *part, Lisp_Object *x,
		     Lisp_Object *y, Time *timeptr)
{
  struct timeval now;
  Time sec, usec;

  *fp = SELECTED_FRAME ();
  (*fp)->mouse_moved = 0;

  *bar_window = Qnil;
  *part = 0;

  XSETINT (*x, last_mouse_x);
  XSETINT (*y, last_mouse_y);
  gettimeofday(&now, 0);
  sec = now.tv_sec;
  usec = now.tv_usec;
  *timeptr = (sec * 1000) + (usec / 1000);
}

/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.  */

static Lisp_Object
term_mouse_click (struct input_event *result, Gpm_Event *event,
		  struct frame *f)
{
  struct timeval now;
  int i, j;

  result->kind = GPM_CLICK_EVENT;
  for (i = 0, j = GPM_B_LEFT; i < 3; i++, j >>= 1 )
    {
      if (event->buttons & j) {
	result->code = i; /* button number */
	break;
      }
    }
  gettimeofday(&now, 0);
  result->timestamp = (now.tv_sec * 1000) + (now.tv_usec / 1000);

  if (event->type & GPM_UP)
    result->modifiers = up_modifier;
  else if (event->type & GPM_DOWN)
    result->modifiers = down_modifier;
  else
    result->modifiers = 0;

  if (event->type & GPM_SINGLE)
    result->modifiers |= click_modifier;

  if (event->type & GPM_DOUBLE)
    result->modifiers |= double_modifier;

  if (event->type & GPM_TRIPLE)
    result->modifiers |= triple_modifier;

  if (event->type & GPM_DRAG)
    result->modifiers |= drag_modifier;

  if (!(event->type & (GPM_MOVE | GPM_DRAG))) {

    /* 1 << KG_SHIFT */
    if (event->modifiers & (1 << 0))
      result->modifiers |= shift_modifier;

    /* 1 << KG_CTRL */
    if (event->modifiers & (1 << 2))
      result->modifiers |= ctrl_modifier;

    /* 1 << KG_ALT || KG_ALTGR */
    if (event->modifiers & (1 << 3)
	|| event->modifiers & (1 << 1))
      result->modifiers |= meta_modifier;
  }

  XSETINT (result->x, event->x);
  XSETINT (result->y, event->y);
  XSETFRAME (result->frame_or_window, f);
  result->arg = Qnil;
  return Qnil;
}

int
handle_one_term_event (struct tty_display_info *tty, Gpm_Event *event, struct input_event* hold_quit)
{
  struct frame *f = XFRAME (tty->top_frame);
  struct input_event ie;
  int do_help = 0;
  int count = 0;

  EVENT_INIT (ie);
  ie.kind = NO_EVENT;
  ie.arg = Qnil;

  if (event->type & (GPM_MOVE | GPM_DRAG)) {
    previous_help_echo_string = help_echo_string;
    help_echo_string = Qnil;

    Gpm_DrawPointer (event->x, event->y, fileno (tty->output));

    if (!term_mouse_movement (f, event))
      help_echo_string = previous_help_echo_string;

    /* If the contents of the global variable help_echo_string
       has changed, generate a HELP_EVENT.  */
    if (!NILP (help_echo_string)
	|| !NILP (previous_help_echo_string))
      do_help = 1;

    goto done;
  }
  else {
    f->mouse_moved = 0;
    term_mouse_click (&ie, event, f);
  }

 done:
  if (ie.kind != NO_EVENT)
    {
      kbd_buffer_store_event_hold (&ie, hold_quit);
      count++;
    }

  if (do_help
      && !(hold_quit && hold_quit->kind != NO_EVENT))
    {
      Lisp_Object frame;

      if (f)
	XSETFRAME (frame, f);
      else
	frame = Qnil;

      gen_help_event (help_echo_string, frame, help_echo_window,
		      help_echo_object, help_echo_pos);
      count++;
    }

  return count;
}

DEFUN ("gpm-mouse-start", Fgpm_mouse_start, Sgpm_mouse_start,
       0, 0, 0,
       doc: /* Open a connection to Gpm.
Gpm-mouse can only be activated for one tty at a time.  */)
  (void)
{
  struct frame *f = SELECTED_FRAME ();
  struct tty_display_info *tty
    = ((f)->output_method == output_termcap
       ? (f)->terminal->display_info.tty : NULL);
  Gpm_Connect connection;

  if (!tty)
    error ("Gpm-mouse only works in the GNU/Linux console");
  if (gpm_tty == tty)
    return Qnil;		/* Already activated, nothing to do.  */
  if (gpm_tty)
    error ("Gpm-mouse can only be activated for one tty at a time");

  connection.eventMask = ~0;
  connection.defaultMask = ~GPM_HARD;
  connection.maxMod = ~0;
  connection.minMod = 0;
  gpm_zerobased = 1;

  if (Gpm_Open (&connection, 0) < 0)
    error ("Gpm-mouse failed to connect to the gpm daemon");
  else
    {
      gpm_tty = tty;
      /* `init_sys_modes' arranges for mouse movements sent through gpm_fd
	 to generate SIGIOs.  Apparently we need to call reset_sys_modes
	 before calling init_sys_modes.  */
      reset_sys_modes (tty);
      init_sys_modes (tty);
      add_gpm_wait_descriptor (gpm_fd);
      return Qnil;
    }
}

void
close_gpm (int fd)
{
  if (fd >= 0)
    delete_gpm_wait_descriptor (fd);
  while (Gpm_Close()); /* close all the stack */
  gpm_tty = NULL;
}

DEFUN ("gpm-mouse-stop", Fgpm_mouse_stop, Sgpm_mouse_stop,
       0, 0, 0,
       doc: /* Close a connection to Gpm.  */)
  (void)
{
  struct frame *f = SELECTED_FRAME ();
  struct tty_display_info *tty
    = ((f)->output_method == output_termcap
       ? (f)->terminal->display_info.tty : NULL);

  if (!tty || gpm_tty != tty)
    return Qnil;       /* Not activated on this terminal, nothing to do.  */

  close_gpm (gpm_fd);
  return Qnil;
}
#endif /* HAVE_GPM */


#ifndef MSDOS
/***********************************************************************
			    Initialization
 ***********************************************************************/

/* Initialize the tty-dependent part of frame F.  The frame must
   already have its device initialized. */

void
create_tty_output (struct frame *f)
{
  struct tty_output *t;

  if (! FRAME_TERMCAP_P (f))
    abort ();

  t = xmalloc (sizeof (struct tty_output));
  memset (t, 0, sizeof (struct tty_output));

  t->display_info = FRAME_TERMINAL (f)->display_info.tty;

  f->output_data.tty = t;
}

/* Delete frame F's face cache, and its tty-dependent part. */

static void
tty_free_frame_resources (struct frame *f)
{
  if (! FRAME_TERMCAP_P (f))
    abort ();

  if (FRAME_FACE_CACHE (f))
    free_frame_faces (f);

  xfree (f->output_data.tty);
}

#else  /* MSDOS */

/* Delete frame F's face cache. */

static void
tty_free_frame_resources (struct frame *f)
{
  if (! FRAME_TERMCAP_P (f) && ! FRAME_MSDOS_P (f))
    abort ();

  if (FRAME_FACE_CACHE (f))
    free_frame_faces (f);
}
#endif	/* MSDOS */

/* Reset the hooks in TERMINAL.  */

static void
clear_tty_hooks (struct terminal *terminal)
{
  terminal->rif = 0;
  terminal->cursor_to_hook = 0;
  terminal->raw_cursor_to_hook = 0;
  terminal->clear_to_end_hook = 0;
  terminal->clear_frame_hook = 0;
  terminal->clear_end_of_line_hook = 0;
  terminal->ins_del_lines_hook = 0;
  terminal->insert_glyphs_hook = 0;
  terminal->write_glyphs_hook = 0;
  terminal->delete_glyphs_hook = 0;
  terminal->ring_bell_hook = 0;
  terminal->reset_terminal_modes_hook = 0;
  terminal->set_terminal_modes_hook = 0;
  terminal->update_begin_hook = 0;
  terminal->update_end_hook = 0;
  terminal->set_terminal_window_hook = 0;
  terminal->mouse_position_hook = 0;
  terminal->frame_rehighlight_hook = 0;
  terminal->frame_raise_lower_hook = 0;
  terminal->fullscreen_hook = 0;
  terminal->set_vertical_scroll_bar_hook = 0;
  terminal->condemn_scroll_bars_hook = 0;
  terminal->redeem_scroll_bar_hook = 0;
  terminal->judge_scroll_bars_hook = 0;
  terminal->read_socket_hook = 0;
  terminal->frame_up_to_date_hook = 0;

  /* Leave these two set, or suspended frames are not deleted
     correctly.  */
  terminal->delete_frame_hook = &tty_free_frame_resources;
  terminal->delete_terminal_hook = &delete_tty;
}

/* Initialize hooks in TERMINAL with the values needed for a tty.  */

static void
set_tty_hooks (struct terminal *terminal)
{
  terminal->rif = 0; /* ttys don't support window-based redisplay. */

  terminal->cursor_to_hook = &tty_cursor_to;
  terminal->raw_cursor_to_hook = &tty_raw_cursor_to;

  terminal->clear_to_end_hook = &tty_clear_to_end;
  terminal->clear_frame_hook = &tty_clear_frame;
  terminal->clear_end_of_line_hook = &tty_clear_end_of_line;

  terminal->ins_del_lines_hook = &tty_ins_del_lines;

  terminal->insert_glyphs_hook = &tty_insert_glyphs;
  terminal->write_glyphs_hook = &tty_write_glyphs;
  terminal->delete_glyphs_hook = &tty_delete_glyphs;

  terminal->ring_bell_hook = &tty_ring_bell;

  terminal->reset_terminal_modes_hook = &tty_reset_terminal_modes;
  terminal->set_terminal_modes_hook = &tty_set_terminal_modes;
  terminal->update_begin_hook = 0; /* Not needed. */
  terminal->update_end_hook = &tty_update_end;
  terminal->set_terminal_window_hook = &tty_set_terminal_window;

  terminal->mouse_position_hook = 0; /* Not needed. */
  terminal->frame_rehighlight_hook = 0; /* Not needed. */
  terminal->frame_raise_lower_hook = 0; /* Not needed. */

  terminal->set_vertical_scroll_bar_hook = 0; /* Not needed. */
  terminal->condemn_scroll_bars_hook = 0; /* Not needed. */
  terminal->redeem_scroll_bar_hook = 0; /* Not needed. */
  terminal->judge_scroll_bars_hook = 0; /* Not needed. */

  terminal->read_socket_hook = &tty_read_avail_input; /* keyboard.c */
  terminal->frame_up_to_date_hook = 0; /* Not needed. */

  terminal->delete_frame_hook = &tty_free_frame_resources;
  terminal->delete_terminal_hook = &delete_tty;
}

/* Drop the controlling terminal if fd is the same device. */
static void
dissociate_if_controlling_tty (int fd)
{
#ifndef DOS_NT
  int pgid = tcgetpgrp (fd); /* If tcgetpgrp succeeds, fd is the ctty. */
  if (pgid != -1)
    {
#if defined (USG5)
      setpgrp ();
      no_controlling_tty = 1;
#elif defined (CYGWIN)
      setsid ();
      no_controlling_tty = 1;
#else
#ifdef TIOCNOTTY                /* Try BSD ioctls. */
      sigblock (sigmask (SIGTTOU));
      fd = emacs_open (DEV_TTY, O_RDWR, 0);
      if (fd != -1 && ioctl (fd, TIOCNOTTY, 0) != -1)
        {
          no_controlling_tty = 1;
        }
      if (fd != -1)
        emacs_close (fd);
      sigunblock (sigmask (SIGTTOU));
#else
      /* Unknown system. */
      croak ();
#endif  /* ! TIOCNOTTY */
#endif  /* ! USG */
    }
#endif	/* !DOS_NT */
}

/* Create a termcap display on the tty device with the given name and
   type.

   If NAME is NULL, then use the controlling tty, i.e., "/dev/tty".
   Otherwise NAME should be a path to the tty device file,
   e.g. "/dev/pts/7".

   TERMINAL_TYPE is the termcap type of the device, e.g. "vt100".

   If MUST_SUCCEED is true, then all errors are fatal. */

struct terminal *
init_tty (const char *name, const char *terminal_type, int must_succeed)
{
  char *area = NULL;
  char **address = &area;
  int buffer_size = 4096;
  int status;
  struct tty_display_info *tty = NULL;
  struct terminal *terminal = NULL;
  int ctty = 0;                 /* 1 if asked to open controlling tty. */

  if (!terminal_type)
    maybe_fatal (must_succeed, 0,
                 "Unknown terminal type",
                 "Unknown terminal type");

  if (name == NULL)
    name = DEV_TTY;
  if (!strcmp (name, DEV_TTY))
    ctty = 1;

  /* If we already have a terminal on the given device, use that.  If
     all such terminals are suspended, create a new one instead.  */
  /* XXX Perhaps this should be made explicit by having init_tty
     always create a new terminal and separating terminal and frame
     creation on Lisp level.  */
  terminal = get_named_tty (name);
  if (terminal)
    return terminal;

  terminal = create_terminal ();
#ifdef MSDOS
  if (been_here > 0)
    maybe_fatal (0, 0, "Attempt to create another terminal %s", "",
		 name, "");
  been_here = 1;
  tty = &the_only_display_info;
#else
  tty = (struct tty_display_info *) xmalloc (sizeof (struct tty_display_info));
#endif
  memset (tty, 0, sizeof (struct tty_display_info));
  tty->next = tty_list;
  tty_list = tty;

  terminal->type = output_termcap;
  terminal->display_info.tty = tty;
  tty->terminal = terminal;

  tty->Wcm = (struct cm *) xmalloc (sizeof (struct cm));
  Wcm_clear (tty);

  encode_terminal_src_size = 0;
  encode_terminal_dst_size = 0;


#ifndef DOS_NT
  set_tty_hooks (terminal);

  {
    int fd;
    FILE *file;

#ifdef O_IGNORE_CTTY
    if (!ctty)
      /* Open the terminal device.  Don't recognize it as our
         controlling terminal, and don't make it the controlling tty
         if we don't have one at the moment.  */
      fd = emacs_open (name, O_RDWR | O_IGNORE_CTTY | O_NOCTTY, 0);
    else
#endif /* O_IGNORE_CTTY */
      /* Alas, O_IGNORE_CTTY is a GNU extension that seems to be only
         defined on Hurd.  On other systems, we need to explicitly
         dissociate ourselves from the controlling tty when we want to
         open a frame on the same terminal.  */
      fd = emacs_open (name, O_RDWR | O_NOCTTY, 0);

    tty->name = xstrdup (name);
    terminal->name = xstrdup (name);

    if (fd < 0)
      maybe_fatal (must_succeed, terminal,
                   "Could not open file: %s",
                   "Could not open file: %s",
                   name);
    if (!isatty (fd))
      {
        close (fd);
        maybe_fatal (must_succeed, terminal,
                     "Not a tty device: %s",
                     "Not a tty device: %s",
                     name);
      }

#ifndef O_IGNORE_CTTY
    if (!ctty)
      dissociate_if_controlling_tty (fd);
#endif

    file = fdopen (fd, "w+");
    tty->input = file;
    tty->output = file;
  }

  tty->type = xstrdup (terminal_type);

  add_keyboard_wait_descriptor (fileno (tty->input));

  Wcm_clear (tty);

  tty->termcap_term_buffer = (char *) xmalloc (buffer_size);

  /* On some systems, tgetent tries to access the controlling
     terminal. */
  sigblock (sigmask (SIGTTOU));
  status = tgetent (tty->termcap_term_buffer, terminal_type);
  sigunblock (sigmask (SIGTTOU));

  if (status < 0)
    {
#ifdef TERMINFO
      maybe_fatal (must_succeed, terminal,
                   "Cannot open terminfo database file",
                   "Cannot open terminfo database file");
#else
      maybe_fatal (must_succeed, terminal,
                   "Cannot open termcap database file",
                   "Cannot open termcap database file");
#endif
    }
  if (status == 0)
    {
      maybe_fatal (must_succeed, terminal,
                   "Terminal type %s is not defined",
                   "Terminal type %s is not defined.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n"
#ifdef TERMINFO
"to do `unset TERMINFO' (C-shell: `unsetenv TERMINFO') as well.",
#else
"to do `unset TERMCAP' (C-shell: `unsetenv TERMCAP') as well.",
#endif
                   terminal_type);
    }

#ifndef TERMINFO
  if (strlen (tty->termcap_term_buffer) >= buffer_size)
    abort ();
  buffer_size = strlen (tty->termcap_term_buffer);
#endif
  tty->termcap_strings_buffer = area = (char *) xmalloc (buffer_size);
  tty->TS_ins_line = tgetstr ("al", address);
  tty->TS_ins_multi_lines = tgetstr ("AL", address);
  tty->TS_bell = tgetstr ("bl", address);
  BackTab (tty) = tgetstr ("bt", address);
  tty->TS_clr_to_bottom = tgetstr ("cd", address);
  tty->TS_clr_line = tgetstr ("ce", address);
  tty->TS_clr_frame = tgetstr ("cl", address);
  ColPosition (tty) = NULL; /* tgetstr ("ch", address); */
  AbsPosition (tty) = tgetstr ("cm", address);
  CR (tty) = tgetstr ("cr", address);
  tty->TS_set_scroll_region = tgetstr ("cs", address);
  tty->TS_set_scroll_region_1 = tgetstr ("cS", address);
  RowPosition (tty) = tgetstr ("cv", address);
  tty->TS_del_char = tgetstr ("dc", address);
  tty->TS_del_multi_chars = tgetstr ("DC", address);
  tty->TS_del_line = tgetstr ("dl", address);
  tty->TS_del_multi_lines = tgetstr ("DL", address);
  tty->TS_delete_mode = tgetstr ("dm", address);
  tty->TS_end_delete_mode = tgetstr ("ed", address);
  tty->TS_end_insert_mode = tgetstr ("ei", address);
  Home (tty) = tgetstr ("ho", address);
  tty->TS_ins_char = tgetstr ("ic", address);
  tty->TS_ins_multi_chars = tgetstr ("IC", address);
  tty->TS_insert_mode = tgetstr ("im", address);
  tty->TS_pad_inserted_char = tgetstr ("ip", address);
  tty->TS_end_keypad_mode = tgetstr ("ke", address);
  tty->TS_keypad_mode = tgetstr ("ks", address);
  LastLine (tty) = tgetstr ("ll", address);
  Right (tty) = tgetstr ("nd", address);
  Down (tty) = tgetstr ("do", address);
  if (!Down (tty))
    Down (tty) = tgetstr ("nl", address); /* Obsolete name for "do" */
  if (tgetflag ("bs"))
    Left (tty) = "\b";		  /* can't possibly be longer! */
  else				  /* (Actually, "bs" is obsolete...) */
    Left (tty) = tgetstr ("le", address);
  if (!Left (tty))
    Left (tty) = tgetstr ("bc", address); /* Obsolete name for "le" */
  tty->TS_pad_char = tgetstr ("pc", address);
  tty->TS_repeat = tgetstr ("rp", address);
  tty->TS_end_standout_mode = tgetstr ("se", address);
  tty->TS_fwd_scroll = tgetstr ("sf", address);
  tty->TS_standout_mode = tgetstr ("so", address);
  tty->TS_rev_scroll = tgetstr ("sr", address);
  tty->Wcm->cm_tab = tgetstr ("ta", address);
  tty->TS_end_termcap_modes = tgetstr ("te", address);
  tty->TS_termcap_modes = tgetstr ("ti", address);
  Up (tty) = tgetstr ("up", address);
  tty->TS_visible_bell = tgetstr ("vb", address);
  tty->TS_cursor_normal = tgetstr ("ve", address);
  tty->TS_cursor_visible = tgetstr ("vs", address);
  tty->TS_cursor_invisible = tgetstr ("vi", address);
  tty->TS_set_window = tgetstr ("wi", address);

  tty->TS_enter_underline_mode = tgetstr ("us", address);
  tty->TS_exit_underline_mode = tgetstr ("ue", address);
  tty->TS_enter_bold_mode = tgetstr ("md", address);
  tty->TS_enter_dim_mode = tgetstr ("mh", address);
  tty->TS_enter_blink_mode = tgetstr ("mb", address);
  tty->TS_enter_reverse_mode = tgetstr ("mr", address);
  tty->TS_enter_alt_charset_mode = tgetstr ("as", address);
  tty->TS_exit_alt_charset_mode = tgetstr ("ae", address);
  tty->TS_exit_attribute_mode = tgetstr ("me", address);

  MultiUp (tty) = tgetstr ("UP", address);
  MultiDown (tty) = tgetstr ("DO", address);
  MultiLeft (tty) = tgetstr ("LE", address);
  MultiRight (tty) = tgetstr ("RI", address);

  /* SVr4/ANSI color support.  If "op" isn't available, don't support
     color because we can't switch back to the default foreground and
     background.  */
  tty->TS_orig_pair = tgetstr ("op", address);
  if (tty->TS_orig_pair)
    {
      tty->TS_set_foreground = tgetstr ("AF", address);
      tty->TS_set_background = tgetstr ("AB", address);
      if (!tty->TS_set_foreground)
	{
	  /* SVr4.  */
	  tty->TS_set_foreground = tgetstr ("Sf", address);
	  tty->TS_set_background = tgetstr ("Sb", address);
	}

      tty->TN_max_colors = tgetnum ("Co");
      tty->TN_max_pairs = tgetnum ("pa");

      tty->TN_no_color_video = tgetnum ("NC");
      if (tty->TN_no_color_video == -1)
        tty->TN_no_color_video = 0;
    }

  tty_default_color_capabilities (tty, 1);

  MagicWrap (tty) = tgetflag ("xn");
  /* Since we make MagicWrap terminals look like AutoWrap, we need to have
     the former flag imply the latter.  */
  AutoWrap (tty) = MagicWrap (tty) || tgetflag ("am");
  terminal->memory_below_frame = tgetflag ("db");
  tty->TF_hazeltine = tgetflag ("hz");
  terminal->must_write_spaces = tgetflag ("in");
  tty->meta_key = tgetflag ("km") || tgetflag ("MT");
  tty->TF_insmode_motion = tgetflag ("mi");
  tty->TF_standout_motion = tgetflag ("ms");
  tty->TF_underscore = tgetflag ("ul");
  tty->TF_teleray = tgetflag ("xt");

#else /* DOS_NT */
#ifdef WINDOWSNT
  {
    struct frame *f = XFRAME (selected_frame);

    initialize_w32_display (terminal);

    FrameRows (tty) = FRAME_LINES (f);
    FrameCols (tty) = FRAME_COLS (f);
    tty->specified_window = FRAME_LINES (f);

    FRAME_CAN_HAVE_SCROLL_BARS (f) = 0;
    FRAME_VERTICAL_SCROLL_BAR_TYPE (f) = vertical_scroll_bar_none;
    terminal->char_ins_del_ok = 1;
    baud_rate = 19200;
  }
#else  /* MSDOS */
  {
    int height, width;
    if (strcmp (terminal_type, "internal") == 0)
      terminal->type = output_msdos_raw;
    initialize_msdos_display (terminal);

    get_tty_size (fileno (tty->input), &width, &height);
    FrameCols (tty) = width;
    FrameRows (tty) = height;
    terminal->char_ins_del_ok = 0;
    init_baud_rate (fileno (tty->input));
  }
#endif	/* MSDOS */
  tty->output = stdout;
  tty->input = stdin;
  /* The following two are inaccessible from w32console.c.  */
  terminal->delete_frame_hook = &tty_free_frame_resources;
  terminal->delete_terminal_hook = &delete_tty;

  tty->name = xstrdup (name);
  terminal->name = xstrdup (name);
  tty->type = xstrdup (terminal_type);

  add_keyboard_wait_descriptor (0);

  tty->delete_in_insert_mode = 1;

  UseTabs (tty) = 0;
  terminal->scroll_region_ok = 0;

  /* Seems to insert lines when it's not supposed to, messing up the
     display.  In doing a trace, it didn't seem to be called much, so I
     don't think we're losing anything by turning it off.  */
  terminal->line_ins_del_ok = 0;

  tty->TN_max_colors = 16;  /* Required to be non-zero for tty-display-color-p */
#endif	/* DOS_NT */

#ifdef HAVE_GPM
  terminal->mouse_position_hook = term_mouse_position;
  tty->mouse_highlight.mouse_face_window = Qnil;
#endif

  terminal->kboard = (KBOARD *) xmalloc (sizeof (KBOARD));
  init_kboard (terminal->kboard);
  KVAR (terminal->kboard, Vwindow_system) = Qnil;
  terminal->kboard->next_kboard = all_kboards;
  all_kboards = terminal->kboard;
  terminal->kboard->reference_count++;
  /* Don't let the initial kboard remain current longer than necessary.
     That would cause problems if a file loaded on startup tries to
     prompt in the mini-buffer.  */
  if (current_kboard == initial_kboard)
    current_kboard = terminal->kboard;
#ifndef DOS_NT
  term_get_fkeys (address, terminal->kboard);

  /* Get frame size from system, or else from termcap.  */
  {
    int height, width;
    get_tty_size (fileno (tty->input), &width, &height);
    FrameCols (tty) = width;
    FrameRows (tty) = height;
  }

  if (FrameCols (tty) <= 0)
    FrameCols (tty) = tgetnum ("co");
  if (FrameRows (tty) <= 0)
    FrameRows (tty) = tgetnum ("li");

  if (FrameRows (tty) < 3 || FrameCols (tty) < 3)
    maybe_fatal (must_succeed, terminal,
                 "Screen size %dx%d is too small",
                 "Screen size %dx%d is too small",
                 FrameCols (tty), FrameRows (tty));

  TabWidth (tty) = tgetnum ("tw");

  if (!tty->TS_bell)
    tty->TS_bell = "\07";

  if (!tty->TS_fwd_scroll)
    tty->TS_fwd_scroll = Down (tty);

  PC = tty->TS_pad_char ? *tty->TS_pad_char : 0;

  if (TabWidth (tty) < 0)
    TabWidth (tty) = 8;

/* Turned off since /etc/termcap seems to have :ta= for most terminals
   and newer termcap doc does not seem to say there is a default.
  if (!tty->Wcm->cm_tab)
    tty->Wcm->cm_tab = "\t";
*/

  /* We don't support standout modes that use `magic cookies', so
     turn off any that do.  */
  if (tty->TS_standout_mode && tgetnum ("sg") >= 0)
    {
      tty->TS_standout_mode = 0;
      tty->TS_end_standout_mode = 0;
    }
  if (tty->TS_enter_underline_mode && tgetnum ("ug") >= 0)
    {
      tty->TS_enter_underline_mode = 0;
      tty->TS_exit_underline_mode = 0;
    }

  /* If there's no standout mode, try to use underlining instead.  */
  if (tty->TS_standout_mode == 0)
    {
      tty->TS_standout_mode = tty->TS_enter_underline_mode;
      tty->TS_end_standout_mode = tty->TS_exit_underline_mode;
    }

  /* If no `se' string, try using a `me' string instead.
     If that fails, we can't use standout mode at all.  */
  if (tty->TS_end_standout_mode == 0)
    {
      char *s = tgetstr ("me", address);
      if (s != 0)
        tty->TS_end_standout_mode = s;
      else
        tty->TS_standout_mode = 0;
    }

  if (tty->TF_teleray)
    {
      tty->Wcm->cm_tab = 0;
      /* We can't support standout mode, because it uses magic cookies.  */
      tty->TS_standout_mode = 0;
      /* But that means we cannot rely on ^M to go to column zero! */
      CR (tty) = 0;
      /* LF can't be trusted either -- can alter hpos */
      /* if move at column 0 thru a line with TS_standout_mode */
      Down (tty) = 0;
    }

  tty->specified_window = FrameRows (tty);

  if (Wcm_init (tty) == -1)	/* can't do cursor motion */
    {
      maybe_fatal (must_succeed, terminal,
                   "Terminal type \"%s\" is not powerful enough to run Emacs",
                   "Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n"
# ifdef TERMINFO
"to do `unset TERMINFO' (C-shell: `unsetenv TERMINFO') as well.",
# else /* TERMCAP */
"to do `unset TERMCAP' (C-shell: `unsetenv TERMCAP') as well.",
# endif /* TERMINFO */
                   terminal_type);
    }

  if (FrameRows (tty) <= 0 || FrameCols (tty) <= 0)
    maybe_fatal (must_succeed, terminal,
                 "Could not determine the frame size",
                 "Could not determine the frame size");

  tty->delete_in_insert_mode
    = tty->TS_delete_mode && tty->TS_insert_mode
    && !strcmp (tty->TS_delete_mode, tty->TS_insert_mode);

  tty->se_is_so = (tty->TS_standout_mode
              && tty->TS_end_standout_mode
              && !strcmp (tty->TS_standout_mode, tty->TS_end_standout_mode));

  UseTabs (tty) = tabs_safe_p (fileno (tty->input)) && TabWidth (tty) == 8;

  terminal->scroll_region_ok
    = (tty->Wcm->cm_abs
       && (tty->TS_set_window || tty->TS_set_scroll_region || tty->TS_set_scroll_region_1));

  terminal->line_ins_del_ok
    = (((tty->TS_ins_line || tty->TS_ins_multi_lines)
        && (tty->TS_del_line || tty->TS_del_multi_lines))
       || (terminal->scroll_region_ok
           && tty->TS_fwd_scroll && tty->TS_rev_scroll));

  terminal->char_ins_del_ok
    = ((tty->TS_ins_char || tty->TS_insert_mode
        || tty->TS_pad_inserted_char || tty->TS_ins_multi_chars)
       && (tty->TS_del_char || tty->TS_del_multi_chars));

  terminal->fast_clear_end_of_line = tty->TS_clr_line != 0;

  init_baud_rate (fileno (tty->input));

#endif /* not DOS_NT */

  /* Init system terminal modes (RAW or CBREAK, etc.).  */
  init_sys_modes (tty);

  return terminal;
}


static void
vfatal (const char *str, va_list ap)
{
  fprintf (stderr, "emacs: ");
  vfprintf (stderr, str, ap);
  if (!(strlen (str) > 0 && str[strlen (str) - 1] == '\n'))
    fprintf (stderr, "\n");
  fflush (stderr);
  exit (1);
}


/* Auxiliary error-handling function for init_tty.
   Delete TERMINAL, then call error or fatal with str1 or str2,
   respectively, according to whether MUST_SUCCEED is zero or not.  */

static void
maybe_fatal (int must_succeed, struct terminal *terminal,
	     const char *str1, const char *str2, ...)
{
  va_list ap;
  va_start (ap, str2);
  if (terminal)
    delete_tty (terminal);

  if (must_succeed)
    vfatal (str2, ap);
  else
    verror (str1, ap);

  va_end (ap);
  abort ();
}

void
fatal (const char *str, ...)
{
  va_list ap;
  va_start (ap, str);
  vfatal (str, ap);
  va_end (ap);
}



/* Delete the given tty terminal, closing all frames on it. */

static void
delete_tty (struct terminal *terminal)
{
  struct tty_display_info *tty;

  /* Protect against recursive calls.  delete_frame in
     delete_terminal calls us back when it deletes our last frame.  */
  if (!terminal->name)
    return;

  if (terminal->type != output_termcap)
    abort ();

  tty = terminal->display_info.tty;

  if (tty == tty_list)
    tty_list = tty->next;
  else
    {
      struct tty_display_info *p;
      for (p = tty_list; p && p->next != tty; p = p->next)
        ;

      if (! p)
        /* This should not happen. */
        abort ();

      p->next = tty->next;
      tty->next = 0;
    }

  /* reset_sys_modes needs a valid device, so this call needs to be
     before delete_terminal. */
  reset_sys_modes (tty);

  delete_terminal (terminal);

  xfree (tty->name);
  xfree (tty->type);

  if (tty->input)
    {
      delete_keyboard_wait_descriptor (fileno (tty->input));
      if (tty->input != stdin)
        fclose (tty->input);
    }
  if (tty->output && tty->output != stdout && tty->output != tty->input)
    fclose (tty->output);
  if (tty->termscript)
    fclose (tty->termscript);

  xfree (tty->old_tty);
  xfree (tty->Wcm);
  xfree (tty->termcap_strings_buffer);
  xfree (tty->termcap_term_buffer);

  memset (tty, 0, sizeof (struct tty_display_info));
  xfree (tty);
}



/* Mark the pointers in the tty_display_info objects.
   Called by the Fgarbage_collector.  */

void
mark_ttys (void)
{
  struct tty_display_info *tty;

  for (tty = tty_list; tty; tty = tty->next)
    mark_object (tty->top_frame);
}



void
syms_of_term (void)
{
  DEFVAR_BOOL ("system-uses-terminfo", system_uses_terminfo,
    doc: /* Non-nil means the system uses terminfo rather than termcap.
This variable can be used by terminal emulator packages.  */);
#ifdef TERMINFO
  system_uses_terminfo = 1;
#else
  system_uses_terminfo = 0;
#endif

  DEFVAR_LISP ("suspend-tty-functions", Vsuspend_tty_functions,
    doc: /* Functions to be run after suspending a tty.
The functions are run with one argument, the terminal object to be suspended.
See `suspend-tty'.  */);
  Vsuspend_tty_functions = Qnil;


  DEFVAR_LISP ("resume-tty-functions", Vresume_tty_functions,
    doc: /* Functions to be run after resuming a tty.
The functions are run with one argument, the terminal object that was revived.
See `resume-tty'.  */);
  Vresume_tty_functions = Qnil;

  DEFVAR_BOOL ("visible-cursor", visible_cursor,
	       doc: /* Non-nil means to make the cursor very visible.
This only has an effect when running in a text terminal.
What means \"very visible\" is up to your terminal.  It may make the cursor
bigger, or it may make it blink, or it may do nothing at all.  */);
  visible_cursor = 1;

  defsubr (&Stty_display_color_p);
  defsubr (&Stty_display_color_cells);
  defsubr (&Stty_no_underline);
  defsubr (&Stty_type);
  defsubr (&Scontrolling_tty_p);
  defsubr (&Ssuspend_tty);
  defsubr (&Sresume_tty);
#ifdef HAVE_GPM
  defsubr (&Sgpm_mouse_start);
  defsubr (&Sgpm_mouse_stop);
#endif /* HAVE_GPM */

#ifndef DOS_NT
  default_orig_pair = NULL;
  default_set_foreground = NULL;
  default_set_background = NULL;
#endif /* !DOS_NT */

  encode_terminal_src = NULL;
  encode_terminal_dst = NULL;
}
