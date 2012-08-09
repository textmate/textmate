/* Terminal hooks for GNU Emacs on the Microsoft W32 API.
   Copyright (C) 1992, 1999, 2001-2012  Free Software Foundation, Inc.

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

/*
   Tim Fleehart (apollo@online.com)		1-17-92
   Geoff Voelker (voelker@cs.washington.edu)	9-12-93
*/


#include <config.h>

#include <stdio.h>
#include <windows.h>
#include <setjmp.h>

#include "lisp.h"
#include "character.h"
#include "coding.h"
#include "disptab.h"
#include "frame.h"
#include "termhooks.h"
#include "termchar.h"
#include "dispextern.h"
#include "w32inevt.h"

/* from window.c */
extern Lisp_Object Frecenter (Lisp_Object);

static void w32con_move_cursor (struct frame *f, int row, int col);
static void w32con_clear_to_end (struct frame *f);
static void w32con_clear_frame (struct frame *f);
static void w32con_clear_end_of_line (struct frame *f, int);
static void w32con_ins_del_lines (struct frame *f, int vpos, int n);
static void w32con_insert_glyphs (struct frame *f, struct glyph *start, int len);
static void w32con_write_glyphs (struct frame *f, struct glyph *string, int len);
static void w32con_delete_glyphs (struct frame *f, int n);
static void w32con_reset_terminal_modes (struct terminal *t);
static void w32con_set_terminal_modes (struct terminal *t);
static void w32con_set_terminal_window (struct frame *f, int size);
static void w32con_update_begin (struct frame * f);
static void w32con_update_end (struct frame * f);
static WORD w32_face_attributes (struct frame *f, int face_id);

static COORD	cursor_coords;
static HANDLE	prev_screen, cur_screen;
static WORD	char_attr_normal;
static DWORD	prev_console_mode;

#ifndef USE_SEPARATE_SCREEN
static CONSOLE_CURSOR_INFO prev_console_cursor;
#endif

HANDLE  keyboard_handle;


/* Setting this as the ctrl handler prevents emacs from being killed when
   someone hits ^C in a 'suspended' session (child shell).
   Also ignore Ctrl-Break signals.  */

BOOL
ctrl_c_handler (unsigned long type)
{
  /* Only ignore "interrupt" events when running interactively.  */
  return (!noninteractive
	  && (type == CTRL_C_EVENT || type == CTRL_BREAK_EVENT));
}


/* Move the cursor to (ROW, COL) on FRAME.  */
static void
w32con_move_cursor (struct frame *f, int row, int col)
{
  cursor_coords.X = col;
  cursor_coords.Y = row;

  /* TODO: for multi-tty support, cur_screen should be replaced with a
     reference to the terminal for this frame.  */
  SetConsoleCursorPosition (cur_screen, cursor_coords);
}

/* Clear from cursor to end of screen.  */
static void
w32con_clear_to_end (struct frame *f)
{
  w32con_clear_end_of_line (f, FRAME_COLS (f) - 1);
  w32con_ins_del_lines (f, cursor_coords.Y, FRAME_LINES (f) - cursor_coords.Y - 1);
}

/* Clear the frame.  */
static void
w32con_clear_frame (struct frame *f)
{
  COORD	     dest;
  int        n;
  DWORD      r;
  CONSOLE_SCREEN_BUFFER_INFO info;

  GetConsoleScreenBufferInfo (GetStdHandle (STD_OUTPUT_HANDLE), &info);

  /* Remember that the screen buffer might be wider than the window.  */
  n = FRAME_LINES (f) * info.dwSize.X;
  dest.X = dest.Y = 0;

  FillConsoleOutputAttribute (cur_screen, char_attr_normal, n, dest, &r);
  FillConsoleOutputCharacter (cur_screen, ' ', n, dest, &r);

  w32con_move_cursor (f, 0, 0);
}


static struct glyph glyph_base[256];
static BOOL  ceol_initialized = FALSE;

/* Clear from Cursor to end (what's "standout marker"?).  */
static void
w32con_clear_end_of_line (struct frame *f, int end)
{
  if (!ceol_initialized)
    {
      int i;
      for (i = 0; i < 256; i++)
        {
	  memcpy (&glyph_base[i], &space_glyph, sizeof (struct glyph));
        }
      ceol_initialized = TRUE;
    }
  w32con_write_glyphs (f, glyph_base, end - cursor_coords.X);	/* fencepost ?	*/
}

/* Insert n lines at vpos. if n is negative delete -n lines.  */
static void
w32con_ins_del_lines (struct frame *f, int vpos, int n)
{
  int	     i, nb;
  SMALL_RECT scroll;
  SMALL_RECT clip;
  COORD	     dest;
  CHAR_INFO  fill;

  if (n < 0)
    {
      scroll.Top = vpos - n;
      scroll.Bottom = FRAME_LINES (f);
      dest.Y = vpos;
    }
  else
    {
      scroll.Top = vpos;
      scroll.Bottom = FRAME_LINES (f) - n;
      dest.Y = vpos + n;
    }
  clip.Top = clip.Left = scroll.Left = 0;
  clip.Right = scroll.Right = FRAME_COLS (f);
  clip.Bottom = FRAME_LINES (f);

  dest.X = 0;

  fill.Char.AsciiChar = 0x20;
  fill.Attributes = char_attr_normal;

  ScrollConsoleScreenBuffer (cur_screen, &scroll, &clip, dest, &fill);

  /* Here we have to deal with a w32 console flake: If the scroll
     region looks like abc and we scroll c to a and fill with d we get
     cbd... if we scroll block c one line at a time to a, we get cdd...
     Emacs expects cdd consistently... So we have to deal with that
     here... (this also occurs scrolling the same way in the other
     direction.  */

  if (n > 0)
    {
      if (scroll.Bottom < dest.Y)
        {
	  for (i = scroll.Bottom; i < dest.Y; i++)
            {
	      w32con_move_cursor (f, i, 0);
	      w32con_clear_end_of_line (f, FRAME_COLS (f));
            }
        }
    }
  else
    {
      nb = dest.Y + (scroll.Bottom - scroll.Top) + 1;

      if (nb < scroll.Top)
        {
	  for (i = nb; i < scroll.Top; i++)
            {
	      w32con_move_cursor (f, i, 0);
	      w32con_clear_end_of_line (f, FRAME_COLS (f));
            }
        }
    }

  cursor_coords.X = 0;
  cursor_coords.Y = vpos;
}

#undef	LEFT
#undef	RIGHT
#define	LEFT	1
#define	RIGHT	0

static void
scroll_line (struct frame *f, int dist, int direction)
{
  /* The idea here is to implement a horizontal scroll in one line to
     implement delete and half of insert.  */
  SMALL_RECT scroll, clip;
  COORD	     dest;
  CHAR_INFO  fill;

  clip.Top = scroll.Top = clip.Bottom = scroll.Bottom = cursor_coords.Y;
  clip.Left = 0;
  clip.Right = FRAME_COLS (f);

  if (direction == LEFT)
    {
      scroll.Left = cursor_coords.X + dist;
      scroll.Right = FRAME_COLS (f) - 1;
    }
  else
    {
      scroll.Left = cursor_coords.X;
      scroll.Right = FRAME_COLS (f) - dist - 1;
    }

  dest.X = cursor_coords.X;
  dest.Y = cursor_coords.Y;

  fill.Char.AsciiChar = 0x20;
  fill.Attributes = char_attr_normal;

  ScrollConsoleScreenBuffer (cur_screen, &scroll, &clip, dest, &fill);
}


/* If start is zero insert blanks instead of a string at start ?. */
static void
w32con_insert_glyphs (struct frame *f, register struct glyph *start,
		      register int len)
{
  scroll_line (f, len, RIGHT);

  /* Move len chars to the right starting at cursor_coords, fill with blanks */
  if (start)
    {
      /* Print the first len characters of start, cursor_coords.X adjusted
	 by write_glyphs.  */

      w32con_write_glyphs (f, start, len);
    }
  else
    {
      w32con_clear_end_of_line (f, cursor_coords.X + len);
    }
}

static void
w32con_write_glyphs (struct frame *f, register struct glyph *string,
                     register int len)
{
  DWORD r;
  WORD char_attr;
  unsigned char *conversion_buffer;
  struct coding_system *coding;

  if (len <= 0)
    return;

  /* If terminal_coding does any conversion, use it, otherwise use
     safe_terminal_coding.  We can't use CODING_REQUIRE_ENCODING here
     because it always return 1 if the member src_multibyte is 1.  */
  coding = (FRAME_TERMINAL_CODING (f)->common_flags & CODING_REQUIRE_ENCODING_MASK
	    ? FRAME_TERMINAL_CODING (f) : &safe_terminal_coding);
  /* The mode bit CODING_MODE_LAST_BLOCK should be set to 1 only at
     the tail.  */
  coding->mode &= ~CODING_MODE_LAST_BLOCK;

  while (len > 0)
    {
      /* Identify a run of glyphs with the same face.  */
      int face_id = string->face_id;
      int n;

      for (n = 1; n < len; ++n)
	if (string[n].face_id != face_id)
	  break;

      /* Turn appearance modes of the face of the run on.  */
      char_attr = w32_face_attributes (f, face_id);

      if (n == len)
	/* This is the last run.  */
	coding->mode |= CODING_MODE_LAST_BLOCK;
      conversion_buffer = encode_terminal_code (string, n, coding);
      if (coding->produced > 0)
	{
	  /* Set the attribute for these characters.  */
	  if (!FillConsoleOutputAttribute (cur_screen, char_attr,
					   coding->produced, cursor_coords,
					   &r))
	    {
	      printf ("Failed writing console attributes: %d\n",
		      GetLastError ());
	      fflush (stdout);
	    }

	  /* Write the characters.  */
	  if (!WriteConsoleOutputCharacter (cur_screen, conversion_buffer,
					    coding->produced, cursor_coords,
					    &r))
	    {
	      printf ("Failed writing console characters: %d\n",
		      GetLastError ());
	      fflush (stdout);
	    }

	  cursor_coords.X += coding->produced;
	  w32con_move_cursor (f, cursor_coords.Y, cursor_coords.X);
	}
      len -= n;
      string += n;
    }
}


static void
w32con_delete_glyphs (struct frame *f, int n)
{
  /* delete chars means scroll chars from cursor_coords.X + n to
     cursor_coords.X, anything beyond the edge of the screen should
     come out empty...  */

  scroll_line (f, n, LEFT);
}

static unsigned int sound_type = 0xFFFFFFFF;
#define MB_EMACS_SILENT (0xFFFFFFFF - 1)

void
w32_sys_ring_bell (struct frame *f)
{
  if (sound_type == 0xFFFFFFFF)
    {
      Beep (666, 100);
    }
  else if (sound_type == MB_EMACS_SILENT)
    {
      /* Do nothing.  */
    }
  else
    MessageBeep (sound_type);
}

DEFUN ("set-message-beep", Fset_message_beep, Sset_message_beep, 1, 1, 0,
       doc: /* Set the sound generated when the bell is rung.
SOUND is 'asterisk, 'exclamation, 'hand, 'question, 'ok, or 'silent
to use the corresponding system sound for the bell.  The 'silent sound
prevents Emacs from making any sound at all.
SOUND is nil to use the normal beep.  */)
  (Lisp_Object sound)
{
  CHECK_SYMBOL (sound);

  if (NILP (sound))
      sound_type = 0xFFFFFFFF;
  else if (EQ (sound, intern ("asterisk")))
      sound_type = MB_ICONASTERISK;
  else if (EQ (sound, intern ("exclamation")))
      sound_type = MB_ICONEXCLAMATION;
  else if (EQ (sound, intern ("hand")))
      sound_type = MB_ICONHAND;
  else if (EQ (sound, intern ("question")))
      sound_type = MB_ICONQUESTION;
  else if (EQ (sound, intern ("ok")))
      sound_type = MB_OK;
  else if (EQ (sound, intern ("silent")))
      sound_type = MB_EMACS_SILENT;
  else
      sound_type = 0xFFFFFFFF;

  return sound;
}

static void
w32con_reset_terminal_modes (struct terminal *t)
{
  COORD dest;
  CONSOLE_SCREEN_BUFFER_INFO info;
  int n;
  DWORD r;

  /* Clear the complete screen buffer.  This is required because Emacs
     sets the cursor position to the top of the buffer, but there might
     be other output below the bottom of the Emacs frame if the screen buffer
     is larger than the window size.  */
  GetConsoleScreenBufferInfo (cur_screen, &info);
  dest.X = 0;
  dest.Y = 0;
  n = info.dwSize.X * info.dwSize.Y;

  FillConsoleOutputAttribute (cur_screen, char_attr_normal, n, dest, &r);
  FillConsoleOutputCharacter (cur_screen, ' ', n, dest, &r);
  /* Now that the screen is clear, put the cursor at the top.  */
  SetConsoleCursorPosition (cur_screen, dest);

#ifdef USE_SEPARATE_SCREEN
  SetConsoleActiveScreenBuffer (prev_screen);
#else
  SetConsoleCursorInfo (prev_screen, &prev_console_cursor);
#endif

  SetConsoleMode (keyboard_handle, prev_console_mode);
}

static void
w32con_set_terminal_modes (struct terminal *t)
{
  CONSOLE_CURSOR_INFO cci;

  /* make cursor big and visible (100 on Win95 makes it disappear)  */
  cci.dwSize = 99;
  cci.bVisible = TRUE;
  (void) SetConsoleCursorInfo (cur_screen, &cci);

  SetConsoleActiveScreenBuffer (cur_screen);

  SetConsoleMode (keyboard_handle, ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT);

  /* Initialize input mode: interrupt_input off, no flow control, allow
     8 bit character input, standard quit char.  */
  Fset_input_mode (Qnil, Qnil, make_number (2), Qnil);
}

/* hmmm... perhaps these let us bracket screen changes so that we can flush
   clumps rather than one-character-at-a-time...

   we'll start with not moving the cursor while an update is in progress.  */
static void
w32con_update_begin (struct frame * f)
{
}

static void
w32con_update_end (struct frame * f)
{
  SetConsoleCursorPosition (cur_screen, cursor_coords);
}

static void
w32con_set_terminal_window (struct frame *f, int size)
{
}

/***********************************************************************
			stubs from termcap.c
 ***********************************************************************/

void
sys_tputs (char *str, int nlines, int (*outfun) (int))
{
}

char *
sys_tgetstr (char *cap, char **area)
{
  return NULL;
}


/***********************************************************************
			stubs from cm.c
 ***********************************************************************/

struct tty_display_info *current_tty = NULL;
int cost = 0;

int
evalcost (int c)
{
  return c;
}

int
cmputc (int c)
{
  return c;
}

void
cmcheckmagic (struct tty_display_info *tty)
{
}

void
cmcostinit (struct tty_display_info *tty)
{
}

void
cmgoto (struct tty_display_info *tty, int row, int col)
{
}

void
Wcm_clear (struct tty_display_info *tty)
{
}


/***********************************************************************
				Faces
 ***********************************************************************/


/* Turn appearances of face FACE_ID on tty frame F on.  */

static WORD
w32_face_attributes (struct frame *f, int face_id)
{
  WORD char_attr;
  struct face *face = FACE_FROM_ID (f, face_id);

  xassert (face != NULL);

  char_attr = char_attr_normal;

  /* Reverse the default color if requested. If background and
     foreground are specified, then they have been reversed already.  */
  if (face->tty_reverse_p)
    char_attr = (char_attr & 0xff00) + ((char_attr & 0x000f) << 4)
      + ((char_attr & 0x00f0) >> 4);

  /* Before the terminal is properly initialized, all colors map to 0.
     Don't try to resolve them.  */
  if (NILP (Vtty_defined_color_alist))
    return char_attr;

  /* Colors should be in the range 0...15 unless they are one of
     FACE_TTY_DEFAULT_COLOR, FACE_TTY_DEFAULT_FG_COLOR or
     FACE_TTY_DEFAULT_BG_COLOR.  Other out of range colors are
     invalid, so it is better to use the default color if they ever
     get through to here.  */
  if (face->foreground >= 0 && face->foreground < 16)
    char_attr = (char_attr & 0xfff0) + face->foreground;

  if (face->background >= 0 && face->background < 16)
    char_attr = (char_attr & 0xff0f) + (face->background << 4);

  return char_attr;
}

void
initialize_w32_display (struct terminal *term)
{
  CONSOLE_SCREEN_BUFFER_INFO	info;

  term->rif = 0; /* No window based redisplay on the console.  */
  term->cursor_to_hook		= w32con_move_cursor;
  term->raw_cursor_to_hook	= w32con_move_cursor;
  term->clear_to_end_hook	= w32con_clear_to_end;
  term->clear_frame_hook	= w32con_clear_frame;
  term->clear_end_of_line_hook	= w32con_clear_end_of_line;
  term->ins_del_lines_hook	= w32con_ins_del_lines;
  term->insert_glyphs_hook	= w32con_insert_glyphs;
  term->write_glyphs_hook	= w32con_write_glyphs;
  term->delete_glyphs_hook	= w32con_delete_glyphs;
  term->ring_bell_hook		= w32_sys_ring_bell;
  term->reset_terminal_modes_hook = w32con_reset_terminal_modes;
  term->set_terminal_modes_hook	= w32con_set_terminal_modes;
  term->set_terminal_window_hook = w32con_set_terminal_window;
  term->update_begin_hook	= w32con_update_begin;
  term->update_end_hook		= w32con_update_end;

  term->read_socket_hook = w32_console_read_socket;
  term->mouse_position_hook = w32_console_mouse_position;

  /* The following are not used on the console.  */
  term->frame_rehighlight_hook = 0;
  term->frame_raise_lower_hook = 0;
  term->set_vertical_scroll_bar_hook = 0;
  term->condemn_scroll_bars_hook = 0;
  term->redeem_scroll_bar_hook = 0;
  term->judge_scroll_bars_hook = 0;
  term->frame_up_to_date_hook = 0;

  /* Initialize interrupt_handle.  */
  init_crit ();

  /* Remember original console settings.  */
  keyboard_handle = GetStdHandle (STD_INPUT_HANDLE);
  GetConsoleMode (keyboard_handle, &prev_console_mode);

  prev_screen = GetStdHandle (STD_OUTPUT_HANDLE);

#ifdef USE_SEPARATE_SCREEN
  cur_screen = CreateConsoleScreenBuffer (GENERIC_READ | GENERIC_WRITE,
					  0, NULL,
					  CONSOLE_TEXTMODE_BUFFER,
					  NULL);

  if (cur_screen == INVALID_HANDLE_VALUE)
    {
      printf ("CreateConsoleScreenBuffer failed in ResetTerm\n");
      printf ("LastError = 0x%lx\n", GetLastError ());
      fflush (stdout);
      exit (0);
    }
#else
  cur_screen = prev_screen;
  GetConsoleCursorInfo (prev_screen, &prev_console_cursor);
#endif

  /* Respect setting of LINES and COLUMNS environment variables.  */
  {
    char * lines = getenv ("LINES");
    char * columns = getenv ("COLUMNS");

    if (lines != NULL && columns != NULL)
      {
	SMALL_RECT new_win_dims;
	COORD new_size;

	new_size.X = atoi (columns);
	new_size.Y = atoi (lines);

	GetConsoleScreenBufferInfo (cur_screen, &info);

	/* Shrink the window first, so the buffer dimensions can be
           reduced if necessary.  */
	new_win_dims.Top = 0;
	new_win_dims.Left = 0;
	new_win_dims.Bottom = min (new_size.Y, info.dwSize.Y) - 1;
	new_win_dims.Right = min (new_size.X, info.dwSize.X) - 1;
	SetConsoleWindowInfo (cur_screen, TRUE, &new_win_dims);

	SetConsoleScreenBufferSize (cur_screen, new_size);

	/* Set the window size to match the buffer dimension.  */
	new_win_dims.Top = 0;
	new_win_dims.Left = 0;
	new_win_dims.Bottom = new_size.Y - 1;
	new_win_dims.Right = new_size.X - 1;
	SetConsoleWindowInfo (cur_screen, TRUE, &new_win_dims);
      }
  }

  GetConsoleScreenBufferInfo (cur_screen, &info);

  char_attr_normal = info.wAttributes;

  /* Determine if the info returned by GetConsoleScreenBufferInfo
     is realistic.  Old MS Telnet servers used to only fill out
     the dwSize portion, even modern one fill the whole struct with
     garbage when using non-MS telnet clients.  */
  if ((w32_use_full_screen_buffer
       && (info.dwSize.Y < 20 || info.dwSize.Y > 100
	   || info.dwSize.X < 40 || info.dwSize.X > 200))
      || (!w32_use_full_screen_buffer
	  && (info.srWindow.Bottom - info.srWindow.Top < 20
	      || info.srWindow.Bottom - info.srWindow.Top > 100
	      || info.srWindow.Right - info.srWindow.Left < 40
	      || info.srWindow.Right - info.srWindow.Left > 100)))
    {
      FRAME_LINES (SELECTED_FRAME ()) = 25;
      SET_FRAME_COLS (SELECTED_FRAME (), 80);
    }

  else if (w32_use_full_screen_buffer)
    {
      FRAME_LINES (SELECTED_FRAME ()) = info.dwSize.Y;	/* lines per page */
      SET_FRAME_COLS (SELECTED_FRAME (), info.dwSize.X);  /* characters per line */
    }
  else
    {
      /* Lines per page.  Use buffer coords instead of buffer size.  */
      FRAME_LINES (SELECTED_FRAME ()) = 1 + info.srWindow.Bottom -
	info.srWindow.Top;
      /* Characters per line.  Use buffer coords instead of buffer size.  */
      SET_FRAME_COLS (SELECTED_FRAME (), 1 + info.srWindow.Right -
		       info.srWindow.Left);
    }

  /* Setup w32_display_info structure for this frame. */

  w32_initialize_display_info (build_string ("Console"));

}


DEFUN ("set-screen-color", Fset_screen_color, Sset_screen_color, 2, 2, 0,
       doc: /* Set screen foreground and background colors.

Arguments should be indices between 0 and 15, see w32console.el.  */)
  (Lisp_Object foreground, Lisp_Object background)
{
  char_attr_normal = XFASTINT (foreground) + (XFASTINT (background) << 4);

  Frecenter (Qnil);
  return Qt;
}

DEFUN ("get-screen-color", Fget_screen_color, Sget_screen_color, 0, 0, 0,
       doc: /* Get color indices of the current screen foreground and background.

The colors are returned as a list of 2 indices (FOREGROUND BACKGROUND).
See w32console.el and `tty-defined-color-alist' for mapping of indices
to colors.  */)
  (void)
{
  return Fcons (make_number (char_attr_normal & 0x000f),
		Fcons (make_number ((char_attr_normal >> 4) & 0x000f), Qnil));
}

DEFUN ("set-cursor-size", Fset_cursor_size, Sset_cursor_size, 1, 1, 0,
       doc: /* Set cursor size.  */)
  (Lisp_Object size)
{
  CONSOLE_CURSOR_INFO cci;
  cci.dwSize = XFASTINT (size);
  cci.bVisible = TRUE;
  (void) SetConsoleCursorInfo (cur_screen, &cci);

  return Qt;
}

void
syms_of_ntterm (void)
{
  DEFVAR_BOOL ("w32-use-full-screen-buffer",
               w32_use_full_screen_buffer,
	       doc: /* Non-nil means make terminal frames use the full screen buffer dimensions.
This is desirable when running Emacs over telnet.
A value of nil means use the current console window dimensions; this
may be preferable when working directly at the console with a large
scroll-back buffer.  */);
  w32_use_full_screen_buffer = 0;

  defsubr (&Sset_screen_color);
  defsubr (&Sget_screen_color);
  defsubr (&Sset_cursor_size);
  defsubr (&Sset_message_beep);
}
