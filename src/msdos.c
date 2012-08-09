/* MS-DOS specific C utilities.          -*- coding: raw-text -*-

Copyright (C) 1993-1997, 1999-2012  Free Software Foundation, Inc.

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

/* Contributed by Morten Welinder */
/* New display, keyboard, and mouse control by Kim F. Storm */

/* Note: some of the stuff here was taken from end of sysdep.c in demacs. */

#include <config.h>

#ifdef MSDOS
#include <setjmp.h>
#include "lisp.h"
#include <stdio.h>
#include <time.h>
#include <sys/param.h>
#include <sys/time.h>
#include <dos.h>
#include <errno.h>
#include <sys/stat.h>    /* for _fixpath */
#include <unistd.h>	 /* for chdir, dup, dup2, etc. */
#include <dir.h>	 /* for getdisk */
#pragma pack(0)		 /* dir.h does a pack(4), which isn't GCC's default */
#include <fcntl.h>
#include <io.h>		 /* for setmode */
#include <dpmi.h>	 /* for __dpmi_xxx stuff */
#include <sys/farptr.h>	 /* for _farsetsel, _farnspokeb */
#include <libc/dosio.h>  /* for _USE_LFN */
#include <conio.h>	 /* for cputs */

#include "msdos.h"
#include "systime.h"
#include "frame.h"
#include "termhooks.h"
#include "termchar.h"
#include "dispextern.h"
#include "dosfns.h"
#include "termopts.h"
#include "character.h"
#include "coding.h"
#include "disptab.h"
#include "window.h"
#include "buffer.h"
#include "commands.h"
#include "blockinput.h"
#include "keyboard.h"
#include "intervals.h"
#include <go32.h>
#include <pc.h>
#include <ctype.h>
/* #include <process.h> */
/* Damn that local process.h!  Instead we can define P_WAIT and
   spawnve ourselves.  */
#define P_WAIT 1
extern int spawnve (int, const char *, char *const [], char *const []);

#ifndef _USE_LFN
#define _USE_LFN 0
#endif

#ifndef _dos_ds
#define _dos_ds _go32_info_block.selector_for_linear_memory
#endif

#include <signal.h>
#include "syssignal.h"

#include "careadlinkat.h"
#include "allocator.h"

#ifndef SYSTEM_MALLOC

#ifdef GNU_MALLOC

/* If other `malloc' than ours is used, force our `sbrk' behave like
   Unix programs expect (resize memory blocks to keep them contiguous).
   If `sbrk' from `ralloc.c' is NOT used, also zero-out sbrk'ed memory,
   because that's what `gmalloc' expects to get.  */
#include <crt0.h>

#ifdef REL_ALLOC
int _crt0_startup_flags = _CRT0_FLAG_UNIX_SBRK;
#else  /* not REL_ALLOC */
int _crt0_startup_flags = (_CRT0_FLAG_UNIX_SBRK | _CRT0_FLAG_FILL_SBRK_MEMORY);
#endif /* not REL_ALLOC */
#endif /* GNU_MALLOC */

#endif /* not SYSTEM_MALLOC */

static unsigned long
event_timestamp (void)
{
  struct time t;
  unsigned long s;

  gettime (&t);
  s = t.ti_min;
  s *= 60;
  s += t.ti_sec;
  s *= 1000;
  s += t.ti_hund * 10;

  return s;
}


/* ------------------------ Mouse control ---------------------------
 *
 * Coordinates are in screen positions and zero based.
 * Mouse buttons are numbered from left to right and also zero based.
 */

/* This used to be in termhooks.h, but mainstream Emacs code no longer
   uses it, and it was removed...  */
#define NUM_MOUSE_BUTTONS (5)

int have_mouse;          /* 0: no, 1: enabled, -1: disabled */
static int mouse_visible;

static int mouse_last_x;
static int mouse_last_y;

static int mouse_button_translate[NUM_MOUSE_BUTTONS];
static int mouse_button_count;

void
mouse_on (void)
{
  union REGS regs;

  if (have_mouse > 0 && !mouse_visible)
    {
      struct tty_display_info *tty = CURTTY ();

      if (tty->termscript)
	fprintf (tty->termscript, "<M_ON>");
      regs.x.ax = 0x0001;
      int86 (0x33, &regs, &regs);
      mouse_visible = 1;
    }
}

void
mouse_off (void)
{
  union REGS regs;

  if (have_mouse > 0 && mouse_visible)
    {
      struct tty_display_info *tty = CURTTY ();

      if (tty->termscript)
	fprintf (tty->termscript, "<M_OFF>");
      regs.x.ax = 0x0002;
      int86 (0x33, &regs, &regs);
      mouse_visible = 0;
    }
}

static void
mouse_setup_buttons (int n_buttons)
{
  if (n_buttons == 3)
    {
      mouse_button_count = 3;
      mouse_button_translate[0] = 0; /* Left */
      mouse_button_translate[1] = 2; /* Middle */
      mouse_button_translate[2] = 1; /* Right */
    }
  else	/* two, what else? */
    {
      mouse_button_count = 2;
      mouse_button_translate[0] = 0;
      mouse_button_translate[1] = 1;
    }
}

DEFUN ("msdos-set-mouse-buttons", Fmsdos_set_mouse_buttons, Smsdos_set_mouse_buttons,
       1, 1, "NSet number of mouse buttons to: ",
       doc: /* Set the number of mouse buttons to use by Emacs.
This is useful with mice that report the number of buttons inconsistently,
e.g., if the number of buttons is reported as 3, but Emacs only sees 2 of
them.  This happens with wheeled mice on Windows 9X, for example.  */)
  (Lisp_Object nbuttons)
{
  int n;

  CHECK_NUMBER (nbuttons);
  n = XINT (nbuttons);
  if (n < 2 || n > 3)
    xsignal2 (Qargs_out_of_range,
	      build_string ("only 2 or 3 mouse buttons are supported"),
	      nbuttons);
  mouse_setup_buttons (n);
  return Qnil;
}

static void
mouse_get_xy (int *x, int *y)
{
  union REGS regs;

  regs.x.ax = 0x0003;
  int86 (0x33, &regs, &regs);
  *x = regs.x.cx / 8;
  *y = regs.x.dx / 8;
}

void
mouse_moveto (int x, int y)
{
  union REGS regs;
  struct tty_display_info *tty = CURTTY ();

  if (tty->termscript)
    fprintf (tty->termscript, "<M_XY=%dx%d>", x, y);
  regs.x.ax = 0x0004;
  mouse_last_x = regs.x.cx = x * 8;
  mouse_last_y = regs.x.dx = y * 8;
  int86 (0x33, &regs, &regs);
}

static int
mouse_pressed (int b, int *xp, int *yp)
{
  union REGS regs;

  if (b >= mouse_button_count)
    return 0;
  regs.x.ax = 0x0005;
  regs.x.bx = mouse_button_translate[b];
  int86 (0x33, &regs, &regs);
  if (regs.x.bx)
    *xp = regs.x.cx / 8, *yp = regs.x.dx / 8;
  return (regs.x.bx != 0);
}

static int
mouse_released (int b, int *xp, int *yp)
{
  union REGS regs;

  if (b >= mouse_button_count)
    return 0;
  regs.x.ax = 0x0006;
  regs.x.bx = mouse_button_translate[b];
  int86 (0x33, &regs, &regs);
  if (regs.x.bx)
    *xp = regs.x.cx / 8, *yp = regs.x.dx / 8;
  return (regs.x.bx != 0);
}

static int
mouse_button_depressed (int b, int *xp, int *yp)
{
  union REGS regs;

  if (b >= mouse_button_count)
    return 0;
  regs.x.ax = 0x0003;
  int86 (0x33, &regs, &regs);
  if ((regs.x.bx & (1 << mouse_button_translate[b])) != 0)
    {
      *xp = regs.x.cx / 8;
      *yp = regs.x.dx / 8;
      return 1;
    }
  return 0;
}

void
mouse_get_pos (FRAME_PTR *f, int insist, Lisp_Object *bar_window,
	       enum scroll_bar_part *part, Lisp_Object *x, Lisp_Object *y,
	       Time *time)
{
  int ix, iy;
  Lisp_Object frame, tail;

  /* Clear the mouse-moved flag for every frame on this display.  */
  FOR_EACH_FRAME (tail, frame)
    XFRAME (frame)->mouse_moved = 0;

  *f = SELECTED_FRAME ();
  *bar_window = Qnil;
  mouse_get_xy (&ix, &iy);
  *time = event_timestamp ();
  *x = make_number (mouse_last_x = ix);
  *y = make_number (mouse_last_y = iy);
}

static void
mouse_check_moved (void)
{
  int x, y;

  mouse_get_xy (&x, &y);
  SELECTED_FRAME ()->mouse_moved |= (x != mouse_last_x || y != mouse_last_y);
  mouse_last_x = x;
  mouse_last_y = y;
}

/* Force the mouse driver to ``forget'' about any button clicks until
   now.  */
static void
mouse_clear_clicks (void)
{
  int b;

  for (b = 0; b < mouse_button_count; b++)
    {
      int dummy_x, dummy_y;

      (void) mouse_pressed (b, &dummy_x, &dummy_y);
      (void) mouse_released (b, &dummy_x, &dummy_y);
    }
}

void
mouse_init (void)
{
  union REGS regs;
  struct tty_display_info *tty = CURTTY ();

  if (tty->termscript)
    fprintf (tty->termscript, "<M_INIT>");

  regs.x.ax = 0x0021;
  int86 (0x33, &regs, &regs);

  /* Reset the mouse last press/release info.  It seems that Windows
     doesn't do that automatically when function 21h is called, which
     causes Emacs to ``remember'' the click that switched focus to the
     window just before Emacs was started from that window.  */
  mouse_clear_clicks ();

  regs.x.ax = 0x0007;
  regs.x.cx = 0;
  regs.x.dx = 8 * (ScreenCols () - 1);
  int86 (0x33, &regs, &regs);

  regs.x.ax = 0x0008;
  regs.x.cx = 0;
  regs.x.dx = 8 * (ScreenRows () - 1);
  int86 (0x33, &regs, &regs);

  mouse_moveto (0, 0);
  mouse_visible = 0;
}

/* ------------------------- Screen control ----------------------
 *
 */

static int internal_terminal = 0;

#ifndef HAVE_X_WINDOWS
extern unsigned char ScreenAttrib;
static int screen_face;

static int screen_size_X;
static int screen_size_Y;
static int screen_size;

static int current_pos_X;
static int current_pos_Y;
static int new_pos_X;
static int new_pos_Y;

static void *startup_screen_buffer;
static int startup_screen_size_X;
static int startup_screen_size_Y;
static int startup_pos_X;
static int startup_pos_Y;
static unsigned char startup_screen_attrib;

static clock_t startup_time;

static int term_setup_done;

static unsigned short outside_cursor;

/* Similar to the_only_frame.  */
struct tty_display_info the_only_display_info;

/* Support for DOS/V (allows Japanese characters to be displayed on
   standard, non-Japanese, ATs).  Only supported for DJGPP v2 and later.  */

/* Holds the address of the text-mode screen buffer.  */
static unsigned long screen_old_address = 0;
/* Segment and offset of the virtual screen.  If 0, DOS/V is NOT loaded.  */
static unsigned short screen_virtual_segment = 0;
static unsigned short screen_virtual_offset = 0;
extern Lisp_Object Qcursor_type;
extern Lisp_Object Qbar, Qhbar;

/* The screen colors of the current frame, which serve as the default
   colors for newly-created frames.  */
static int initial_screen_colors[2];

/* Update the screen from a part of relocated DOS/V screen buffer which
   begins at OFFSET and includes COUNT characters.  */
static void
dosv_refresh_virtual_screen (int offset, int count)
{
  __dpmi_regs regs;

  if (offset < 0 || count < 0)	/* paranoia; invalid values crash DOS/V */
    return;

  regs.h.ah = 0xff;	/* update relocated screen */
  regs.x.es = screen_virtual_segment;
  regs.x.di = screen_virtual_offset + offset;
  regs.x.cx = count;
  __dpmi_int (0x10, &regs);
}

static void
dos_direct_output (int y, int x, char *buf, int len)
{
  int t0 = 2 * (x + y * screen_size_X);
  int t = t0 + (int) ScreenPrimary;
  int l0 = len;

  /* This is faster.  */
  for (_farsetsel (_dos_ds); --len >= 0; t += 2, buf++)
    _farnspokeb (t, *buf);

  if (screen_virtual_segment)
    dosv_refresh_virtual_screen (t0, l0);
}
#endif

#ifndef HAVE_X_WINDOWS

static int blink_bit = -1;	/* the state of the blink bit at startup */

/* Enable bright background colors.  */
static void
bright_bg (void)
{
  union REGS regs;

  /* Remember the original state of the blink/bright-background bit.
     It is stored at 0040:0065h in the BIOS data area.  */
  if (blink_bit == -1)
    blink_bit = (_farpeekb (_dos_ds, 0x465) & 0x20) == 0x20;

  regs.h.bl = 0;
  regs.x.ax = 0x1003;
  int86 (0x10, &regs, &regs);
}

/* Disable bright background colors (and enable blinking) if we found
   the video system in that state at startup.  */
static void
maybe_enable_blinking (void)
{
  if (blink_bit == 1)
    {
      union REGS regs;

      regs.h.bl = 1;
      regs.x.ax = 0x1003;
      int86 (0x10, &regs, &regs);
    }
}

/* Return non-zero if the system has a VGA adapter.  */
static int
vga_installed (void)
{
  union REGS regs;

  regs.x.ax = 0x1a00;
  int86 (0x10, &regs, &regs);
  if (regs.h.al == 0x1a && regs.h.bl > 5 && regs.h.bl < 13)
    return 1;
  return 0;
}

/* Set the screen dimensions so that it can show no less than
   ROWS x COLS frame.  */

void
dos_set_window_size (int *rows, int *cols)
{
  char video_name[30];
  union REGS regs;
  Lisp_Object video_mode;
  int video_mode_value, have_vga = 0;
  int current_rows = ScreenRows (), current_cols = ScreenCols ();

  if (*rows == current_rows && *cols == current_cols)
    return;

  mouse_off ();
  have_vga = vga_installed ();

  /* If the user specified a special video mode for these dimensions,
     use that mode.  */
  sprintf (video_name, "screen-dimensions-%dx%d", *rows, *cols);
  video_mode = Fsymbol_value (Fintern_soft (build_string (video_name), Qnil));

  if (INTEGERP (video_mode)
      && (video_mode_value = XINT (video_mode)) > 0)
    {
      regs.x.ax = video_mode_value;
      int86 (0x10, &regs, &regs);

      if (have_mouse)
	{
	  /* Must hardware-reset the mouse, or else it won't update
	     its notion of screen dimensions for some non-standard
	     video modes.  This is *painfully* slow...  */
	  regs.x.ax = 0;
	  int86 (0x33, &regs, &regs);
	}
    }

  /* Find one of the dimensions supported by standard EGA/VGA
     which gives us at least the required dimensions.  */
  else
    {
      static struct {
	int rows, need_vga;
      }	std_dimension[] = {
	  {25, 0},
	  {28, 1},
	  {35, 0},
	  {40, 1},
	  {43, 0},
	  {50, 1}
      };
      int i = 0;

      while (i < sizeof (std_dimension) / sizeof (std_dimension[0]))
	{
	 if (std_dimension[i].need_vga <= have_vga
	     && std_dimension[i].rows >= *rows)
	   {
	     if (std_dimension[i].rows != current_rows
		 || *cols != current_cols)
	       _set_screen_lines (std_dimension[i].rows);
	     break;
	   }
	 i++;
	}
    }


  if (have_mouse)
    {
      mouse_init ();
      mouse_on ();
    }

  /* Tell the caller what dimensions have been REALLY set.  */
  *rows = ScreenRows ();
  *cols = ScreenCols ();

  /* Update Emacs' notion of screen dimensions.  */
  screen_size_X = *cols;
  screen_size_Y = *rows;
  screen_size = *cols * *rows;

  /* If the dimensions changed, the mouse highlight info is invalid.  */
  if (current_rows != *rows || current_cols != *cols)
    {
      struct frame *f = SELECTED_FRAME ();
      Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
      Lisp_Object window = hlinfo->mouse_face_window;

      if (! NILP (window) && XFRAME (XWINDOW (window)->frame) == f)
	{
	  hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
	  hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
	  hlinfo->mouse_face_window = Qnil;
	}
    }

  /* Enable bright background colors.  */
  bright_bg ();

  /* FIXME: I'm not sure the above will run at all on DOS/V.  But let's
     be defensive anyway.  */
  if (screen_virtual_segment)
    dosv_refresh_virtual_screen (0, *cols * *rows);
}

/* If we write a character in the position where the mouse is,
   the mouse cursor may need to be refreshed.  */

static void
mouse_off_maybe (void)
{
  int x, y;

  if (!mouse_visible)
    return;

  mouse_get_xy (&x, &y);
  if (y != new_pos_Y || x < new_pos_X)
    return;

  mouse_off ();
}

#define DEFAULT_CURSOR_START (-1)
#define DEFAULT_CURSOR_WIDTH (-1)
#define BOX_CURSOR_WIDTH     (-32)

/* Set cursor to begin at scan line START_LINE in the character cell
   and extend for WIDTH scan lines.  Scan lines are counted from top
   of the character cell, starting from zero.  */
static void
msdos_set_cursor_shape (struct frame *f, int start_line, int width)
{
  unsigned desired_cursor;
  __dpmi_regs regs;
  int max_line, top_line, bot_line;
  struct tty_display_info *tty = FRAME_TTY (f);

  /* Avoid the costly BIOS call if F isn't the currently selected
     frame.  Allow for NULL as unconditionally meaning the selected
     frame.  */
  if (f && f != SELECTED_FRAME ())
    return;

  if (tty->termscript)
    fprintf (tty->termscript, "\nCURSOR SHAPE=(%d,%d)", start_line, width);

  /* The character cell size in scan lines is stored at 40:85 in the
     BIOS data area.  */
  max_line = _farpeekw (_dos_ds, 0x485) - 1;
  switch (max_line)
    {
      default:	/* this relies on CGA cursor emulation being ON! */
      case 7:
	bot_line = 7;
	break;
      case 9:
	bot_line = 9;
	break;
      case 13:
	bot_line = 12;
	break;
      case 15:
	bot_line = 14;
	break;
    }

  if (width < 0)
    {
      if (width == BOX_CURSOR_WIDTH)
	{
	  top_line = 0;
	  bot_line = max_line;
	}
      else if (start_line != DEFAULT_CURSOR_START)
	{
	  top_line = start_line;
	  bot_line = top_line - width - 1;
	}
      else if (width != DEFAULT_CURSOR_WIDTH)
	{
	  top_line = 0;
	  bot_line = -1 - width;
	}
      else
	top_line = bot_line + 1;
    }
  else if (width == 0)
    {
      /* [31, 0] seems to DTRT for all screen sizes.  */
      top_line = 31;
      bot_line = 0;
    }
  else	/* WIDTH is positive */
    {
      if (start_line != DEFAULT_CURSOR_START)
	bot_line = start_line;
      top_line = bot_line - (width - 1);
    }

  /* If the current cursor shape is already what they want, we are
     history here.  */
  desired_cursor = ((top_line & 0x1f) << 8) | (bot_line & 0x1f);
  if (desired_cursor == _farpeekw (_dos_ds, 0x460))
    return;

  regs.h.ah = 1;
  regs.x.cx = desired_cursor;
  __dpmi_int (0x10, &regs);
}

static void
IT_set_cursor_type (struct frame *f, Lisp_Object cursor_type)
{
  if (EQ (cursor_type, Qbar) || EQ (cursor_type, Qhbar))
    {
      /* Just BAR means the normal EGA/VGA cursor.  */
      msdos_set_cursor_shape (f, DEFAULT_CURSOR_START, DEFAULT_CURSOR_WIDTH);
    }
  else if (CONSP (cursor_type)
	   && (EQ (XCAR (cursor_type), Qbar)
	       || EQ (XCAR (cursor_type), Qhbar)))
    {
      Lisp_Object bar_parms = XCDR (cursor_type);
      int width;

      if (INTEGERP (bar_parms))
	{
	  /* Feature: negative WIDTH means cursor at the top
	     of the character cell, zero means invisible cursor.  */
	  width = XINT (bar_parms);
	  msdos_set_cursor_shape (f, width >= 0 ? DEFAULT_CURSOR_START : 0,
				  width);
	}
      else if (CONSP (bar_parms)
	       && INTEGERP (XCAR (bar_parms))
	       && INTEGERP (XCDR (bar_parms)))
	{
	  int start_line = XINT (XCDR (bar_parms));

	  width = XINT (XCAR (bar_parms));
	  msdos_set_cursor_shape (f, start_line, width);
	}
    }
  else
    {
      /* Treat anything unknown as "box cursor".  This includes nil, so
	 that a frame which doesn't specify a cursor type gets a box,
	 which is the default in Emacs.  */
      msdos_set_cursor_shape (f, 0, BOX_CURSOR_WIDTH);
    }
}

static void
IT_ring_bell (struct frame *f)
{
  if (visible_bell)
    {
      mouse_off ();
      ScreenVisualBell ();
    }
  else
    {
      union REGS inregs, outregs;
      inregs.h.ah = 2;
      inregs.h.dl = 7;
      intdos (&inregs, &outregs);
    }
}

/* Given a face id FACE, extract the face parameters to be used for
   display until the face changes.  The face parameters (actually, its
   color) are used to construct the video attribute byte for each
   glyph during the construction of the buffer that is then blitted to
   the video RAM.  */
static void
IT_set_face (int face)
{
  struct frame *sf = SELECTED_FRAME ();
  struct face *fp  = FACE_FROM_ID (sf, face);
  struct face *dfp = FACE_FROM_ID (sf, DEFAULT_FACE_ID);
  unsigned long fg, bg, dflt_fg, dflt_bg;
  struct tty_display_info *tty = FRAME_TTY (sf);

  if (!fp)
    {
      fp = dfp;
      /* The default face for the frame should always be realized and
	 cached.  */
      if (!fp)
	abort ();
    }
  screen_face = face;
  fg = fp->foreground;
  bg = fp->background;
  dflt_fg = dfp->foreground;
  dflt_bg = dfp->background;

  /* Don't use invalid colors.  In particular, FACE_TTY_DEFAULT_* colors
     mean use the colors of the default face.  Note that we assume all
     16 colors to be available for the background, since Emacs switches
     on this mode (and loses the blinking attribute) at startup.  */
  if (fg == FACE_TTY_DEFAULT_COLOR || fg == FACE_TTY_DEFAULT_FG_COLOR)
    fg = FRAME_FOREGROUND_PIXEL (sf);
  else if (fg == FACE_TTY_DEFAULT_BG_COLOR)
    fg = FRAME_BACKGROUND_PIXEL (sf);
  if (bg == FACE_TTY_DEFAULT_COLOR || bg == FACE_TTY_DEFAULT_BG_COLOR)
    bg = FRAME_BACKGROUND_PIXEL (sf);
  else if (bg == FACE_TTY_DEFAULT_FG_COLOR)
    bg = FRAME_FOREGROUND_PIXEL (sf);

  /* Make sure highlighted lines really stand out, come what may.  */
  if (fp->tty_reverse_p && (fg == dflt_fg && bg == dflt_bg))
    {
      unsigned long tem = fg;

      fg = bg;
      bg = tem;
    }
  /* If the user requested inverse video, obey.  */
  if (inverse_video)
    {
      unsigned long tem2 = fg;

      fg = bg;
      bg = tem2;
    }
  if (tty->termscript)
    fprintf (tty->termscript, "<FACE %d: %lu/%lu[FG:%lu/BG:%lu]>", face,
	     fp->foreground, fp->background, fg, bg);
  if (fg >= 0 && fg < 16)
    {
      ScreenAttrib &= 0xf0;
      ScreenAttrib |= fg;
    }
  if (bg >= 0 && bg < 16)
    {
      ScreenAttrib &= 0x0f;
      ScreenAttrib |= ((bg & 0x0f) << 4);
    }
}

/* According to RBIL (INTERRUP.A, V-1000), 160 is the maximum possible
   width of a DOS display in any known text mode.  We multiply by 2 to
   accommodate the screen attribute byte.  */
#define MAX_SCREEN_BUF 160*2

extern unsigned char *encode_terminal_code (struct glyph *, int,
					    struct coding_system *);

static void
IT_write_glyphs (struct frame *f, struct glyph *str, int str_len)
{
  unsigned char screen_buf[MAX_SCREEN_BUF], *screen_bp, *bp;
  int offset = 2 * (new_pos_X + screen_size_X * new_pos_Y);
  register int sl = str_len;
  struct tty_display_info *tty = FRAME_TTY (f);
  struct frame *sf;
  unsigned char *conversion_buffer;

  /* If terminal_coding does any conversion, use it, otherwise use
     safe_terminal_coding.  We can't use CODING_REQUIRE_ENCODING here
     because it always returns 1 if terminal_coding.src_multibyte is 1.  */
  struct coding_system *coding = FRAME_TERMINAL_CODING (f);

  if (!(coding->common_flags & CODING_REQUIRE_ENCODING_MASK))
    coding = &safe_terminal_coding;

  if (str_len <= 0) return;

  sf = SELECTED_FRAME ();

  /* Since faces get cached and uncached behind our back, we can't
     rely on their indices in the cache being consistent across
     invocations.  So always reset the screen face to the default
     face of the frame, before writing glyphs, and let the glyphs
     set the right face if it's different from the default.  */
  IT_set_face (DEFAULT_FACE_ID);

  /* The mode bit CODING_MODE_LAST_BLOCK should be set to 1 only at
     the tail.  */
  coding->mode &= ~CODING_MODE_LAST_BLOCK;
  screen_bp = &screen_buf[0];
  while (sl > 0)
    {
      int cf;
      int n;

      /* If the face of this glyph is different from the current
	 screen face, update the screen attribute byte.  */
      cf = str->face_id;
      if (cf != screen_face)
	IT_set_face (cf);	/* handles invalid faces gracefully */

      /* Identify a run of glyphs with the same face.  */
      for (n = 1; n < sl; ++n)
	if (str[n].face_id != cf)
	  break;

      if (n >= sl)
	/* This is the last glyph.  */
	coding->mode |= CODING_MODE_LAST_BLOCK;

      conversion_buffer = encode_terminal_code (str, n, coding);
      if (coding->produced > 0)
	{
	  /* Copy the encoded bytes to the screen buffer.  */
	  for (bp = conversion_buffer; coding->produced--; bp++)
	    {
	      /* Paranoia: discard bytes that would overrun the end of
		 the screen buffer.  */
	      if (screen_bp - screen_buf <= MAX_SCREEN_BUF - 2)
		{
		  *screen_bp++ = (unsigned char)*bp;
		  *screen_bp++ = ScreenAttrib;
		}
	      if (tty->termscript)
		fputc (*bp, tty->termscript);
	    }
	}
      /* Update STR and its remaining length.  */
      str += n;
      sl -= n;
    }

  /* Dump whatever we have in the screen buffer.  */
  mouse_off_maybe ();
  dosmemput (screen_buf, screen_bp - screen_buf, (int)ScreenPrimary + offset);
  if (screen_virtual_segment)
    dosv_refresh_virtual_screen (offset, (screen_bp - screen_buf) / 2);
  new_pos_X += (screen_bp - screen_buf) / 2;
}

/************************************************************************
			  Mouse Highlight (and friends..)
 ************************************************************************/

/* Last window where we saw the mouse.  Used by mouse-autoselect-window.  */
static Lisp_Object last_mouse_window;

static int mouse_preempted = 0;	/* non-zero when XMenu gobbles mouse events */

int
popup_activated (void)
{
  return mouse_preempted;
}

/* Draw TEXT_AREA glyphs between START and END of glyph row ROW on
   window W.  X is relative to TEXT_AREA in W.  HL is a face override
   for drawing the glyphs.  */
void
tty_draw_row_with_mouse_face (struct window *w, struct glyph_row *row,
			      int start_hpos, int end_hpos,
			      enum draw_glyphs_face hl)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct tty_display_info *tty = FRAME_TTY (f);
  Mouse_HLInfo *hlinfo = &tty->mouse_highlight;

  if (hl == DRAW_MOUSE_FACE)
    {
      int vpos = row->y + WINDOW_TOP_EDGE_Y (w);
      int kstart = start_hpos + WINDOW_LEFT_EDGE_X (w);
      int nglyphs = end_hpos - start_hpos;
      int offset = ScreenPrimary + 2*(vpos*screen_size_X + kstart) + 1;
      int start_offset = offset;

      if (tty->termscript)
	fprintf (tty->termscript, "\n<MH+ %d-%d:%d>",
		 kstart, kstart + nglyphs - 1, vpos);

      mouse_off ();
      IT_set_face (hlinfo->mouse_face_face_id);
      /* Since we are going to change only the _colors_ of already
	 displayed text, there's no need to go through all the pain of
	 generating and encoding the text from the glyphs.  Instead,
	 we simply poke the attribute byte of each affected position
	 in video memory with the colors computed by IT_set_face!  */
      _farsetsel (_dos_ds);
      while (nglyphs--)
	{
	  _farnspokeb (offset, ScreenAttrib);
	  offset += 2;
	}
      if (screen_virtual_segment)
	dosv_refresh_virtual_screen (start_offset, end_hpos - start_hpos);
      mouse_on ();
    }
  else if (hl == DRAW_NORMAL_TEXT)
    {
      /* We are removing a previously-drawn mouse highlight.  The
	 safest way to do so is to redraw the glyphs anew, since all
	 kinds of faces and display tables could have changed behind
	 our back.  */
      int nglyphs = end_hpos - start_hpos;
      int save_x = new_pos_X, save_y = new_pos_Y;

      if (end_hpos >= row->used[TEXT_AREA])
	nglyphs = row->used[TEXT_AREA] - start_hpos;

      /* IT_write_glyphs writes at cursor position, so we need to
	 temporarily move cursor coordinates to the beginning of
	 the highlight region.  */
      new_pos_X = start_hpos + WINDOW_LEFT_EDGE_X (w);
      new_pos_Y = row->y + WINDOW_TOP_EDGE_Y (w);

      if (tty->termscript)
	fprintf (tty->termscript, "<MH- %d-%d:%d>",
		 new_pos_X, new_pos_X + nglyphs - 1, new_pos_Y);
      IT_write_glyphs (f, row->glyphs[TEXT_AREA] + start_hpos, nglyphs);
      if (tty->termscript)
	fputs ("\n", tty->termscript);
      new_pos_X = save_x;
      new_pos_Y = save_y;
    }
}

static void
IT_clear_end_of_line (struct frame *f, int first_unused)
{
  char *spaces, *sp;
  int i, j, offset = 2 * (new_pos_X + screen_size_X * new_pos_Y);
  extern int fatal_error_in_progress;
  struct tty_display_info *tty = FRAME_TTY (f);

  if (new_pos_X >= first_unused || fatal_error_in_progress)
    return;

  IT_set_face (0);
  i = (j = first_unused - new_pos_X) * 2;
  if (tty->termscript)
    fprintf (tty->termscript, "<CLR:EOL[%d..%d)>", new_pos_X, first_unused);
  spaces = sp = alloca (i);

  while (--j >= 0)
    {
      *sp++ = ' ';
      *sp++ = ScreenAttrib;
    }

  mouse_off_maybe ();
  dosmemput (spaces, i, (int)ScreenPrimary + offset);
  if (screen_virtual_segment)
    dosv_refresh_virtual_screen (offset, i / 2);

  /* clear_end_of_line_raw on term.c leaves the cursor at first_unused.
     Let's follow their lead, in case someone relies on this.  */
  new_pos_X = first_unused;
}

static void
IT_clear_screen (struct frame *f)
{
  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->termscript)
    fprintf (tty->termscript, "<CLR:SCR>");
  /* We are sometimes called (from clear_garbaged_frames) when a new
     frame is being created, but its faces are not yet realized.  In
     such a case we cannot call IT_set_face, since it will fail to find
     any valid faces and will abort.  Instead, use the initial screen
     colors; that should mimic what a Unix tty does, which simply clears
     the screen with whatever default colors are in use.  */
  if (FACE_FROM_ID (SELECTED_FRAME (), DEFAULT_FACE_ID) == NULL)
    ScreenAttrib = (initial_screen_colors[0] << 4) | initial_screen_colors[1];
  else
    IT_set_face (0);
  mouse_off ();
  ScreenClear ();
  if (screen_virtual_segment)
    dosv_refresh_virtual_screen (0, screen_size);
  new_pos_X = new_pos_Y = 0;
}

static void
IT_clear_to_end (struct frame *f)
{
  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->termscript)
    fprintf (tty->termscript, "<CLR:EOS>");

  while (new_pos_Y < screen_size_Y) {
    new_pos_X = 0;
    IT_clear_end_of_line (f, screen_size_X);
    new_pos_Y++;
  }
}

static void
IT_cursor_to (struct frame *f, int y, int x)
{
  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->termscript)
    fprintf (tty->termscript, "\n<XY=%dx%d>", x, y);
  new_pos_X = x;
  new_pos_Y = y;
}

static int cursor_cleared;

static void
IT_display_cursor (int on)
{
  struct tty_display_info *tty = CURTTY ();

  if (on && cursor_cleared)
    {
      ScreenSetCursor (current_pos_Y, current_pos_X);
      cursor_cleared = 0;
      if (tty->termscript)
	fprintf (tty->termscript, "\nCURSOR ON (%dx%d)",
		 current_pos_Y, current_pos_X);
    }
  else if (!on && !cursor_cleared)
    {
      ScreenSetCursor (-1, -1);
      cursor_cleared = 1;
      if (tty->termscript)
	fprintf (tty->termscript, "\nCURSOR OFF (%dx%d)",
		 current_pos_Y, current_pos_X);
    }
}

/* Emacs calls cursor-movement functions a lot when it updates the
   display (probably a legacy of old terminals where you cannot
   update a screen line without first moving the cursor there).
   However, cursor movement is expensive on MSDOS (it calls a slow
   BIOS function and requires 2 mode switches), while actual screen
   updates access the video memory directly and don't depend on
   cursor position.  To avoid slowing down the redisplay, we cheat:
   all functions that move the cursor only set internal variables
   which record the cursor position, whereas the cursor is only
   moved to its final position whenever screen update is complete.

   `IT_cmgoto' is called from the keyboard reading loop and when the
   frame update is complete.  This means that we are ready for user
   input, so we update the cursor position to show where the point is,
   and also make the mouse pointer visible.

   Special treatment is required when the cursor is in the echo area,
   to put the cursor at the end of the text displayed there.  */

static void
IT_cmgoto (FRAME_PTR f)
{
  /* Only set the cursor to where it should be if the display is
     already in sync with the window contents.  */
  int update_cursor_pos = 1; /* MODIFF == unchanged_modified; */
  struct tty_display_info *tty = FRAME_TTY (f);

  /* FIXME: This needs to be rewritten for the new redisplay, or
     removed.  */
#if 0
  static int previous_pos_X = -1;

  update_cursor_pos = 1;	/* temporary!!! */

  /* If the display is in sync, forget any previous knowledge about
     cursor position.  This is primarily for unexpected events like
     C-g in the minibuffer.  */
  if (update_cursor_pos && previous_pos_X >= 0)
    previous_pos_X = -1;
  /* If we are in the echo area, put the cursor at the
     end of the echo area message.  */
  if (!update_cursor_pos
      && WINDOW_TOP_EDGE_LINE (XWINDOW (FRAME_MINIBUF_WINDOW (f))) <= new_pos_Y)
    {
      int tem_X = current_pos_X, dummy;

      if (echo_area_glyphs)
	{
	  tem_X = echo_area_glyphs_length;
	  /* Save current cursor position, to be restored after the
	     echo area message is erased.  Only remember one level
	     of previous cursor position.  */
	  if (previous_pos_X == -1)
	    ScreenGetCursor (&dummy, &previous_pos_X);
	}
      else if (previous_pos_X >= 0)
	{
	  /* We wind up here after the echo area message is erased.
	     Restore the cursor position we remembered above.  */
	  tem_X = previous_pos_X;
	  previous_pos_X = -1;
	}

      if (current_pos_X != tem_X)
	{
	  new_pos_X = tem_X;
	  update_cursor_pos = 1;
	}
    }
#endif

  if (update_cursor_pos
      && (current_pos_X != new_pos_X || current_pos_Y != new_pos_Y))
    {
      ScreenSetCursor (current_pos_Y = new_pos_Y, current_pos_X = new_pos_X);
      if (tty->termscript)
	fprintf (tty->termscript, "\n<CURSOR:%dx%d>", current_pos_X, current_pos_Y);
    }

  /* Maybe cursor is invisible, so make it visible.  */
  IT_display_cursor (1);

  /* Mouse pointer should be always visible if we are waiting for
     keyboard input.  */
  if (!mouse_visible)
    mouse_on ();
}

static void
IT_update_begin (struct frame *f)
{
  struct tty_display_info *display_info = FRAME_X_DISPLAY_INFO (f);
  Mouse_HLInfo *hlinfo = &display_info->mouse_highlight;
  struct frame *mouse_face_frame = hlinfo->mouse_face_mouse_frame;

  if (display_info->termscript)
    fprintf (display_info->termscript, "\n\n<UPDATE_BEGIN");

  BLOCK_INPUT;

  if (f && f == mouse_face_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      hlinfo->mouse_face_defer = 1;

      /* If F needs to be redrawn, simply forget about any prior mouse
	 highlighting.  */
      if (FRAME_GARBAGED_P (f))
	hlinfo->mouse_face_window = Qnil;

      /* Can we tell that this update does not affect the window
	 where the mouse highlight is?  If so, no need to turn off.
	 Likewise, don't do anything if none of the enabled rows
	 contains glyphs highlighted in mouse face.  */
      if (!NILP (hlinfo->mouse_face_window)
	  && WINDOWP (hlinfo->mouse_face_window))
	{
	  struct window *w = XWINDOW (hlinfo->mouse_face_window);
	  int i;

	  /* If the mouse highlight is in the window that was deleted
	     (e.g., if it was popped by completion), clear highlight
	     unconditionally.  */
	  if (NILP (w->buffer))
	    hlinfo->mouse_face_window = Qnil;
	  else
	    {
	      for (i = 0; i < w->desired_matrix->nrows; ++i)
		if (MATRIX_ROW_ENABLED_P (w->desired_matrix, i)
		    && MATRIX_ROW (w->current_matrix, i)->mouse_face_p)
		  break;
	    }

	  if (NILP (w->buffer) || i < w->desired_matrix->nrows)
	    clear_mouse_face (hlinfo);
	}
    }
  else if (mouse_face_frame && !FRAME_LIVE_P (mouse_face_frame))
    {
      /* If the frame with mouse highlight was deleted, invalidate the
	 highlight info.  */
      hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
      hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
      hlinfo->mouse_face_window = Qnil;
      hlinfo->mouse_face_deferred_gc = 0;
      hlinfo->mouse_face_mouse_frame = NULL;
    }

  UNBLOCK_INPUT;
}

static void
IT_update_end (struct frame *f)
{
  struct tty_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  if (dpyinfo->termscript)
    fprintf (dpyinfo->termscript, "\n<UPDATE_END\n");
  dpyinfo->mouse_highlight.mouse_face_defer = 0;
}

static void
IT_frame_up_to_date (struct frame *f)
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
  Lisp_Object new_cursor, frame_desired_cursor;
  struct window *sw;

  if (hlinfo->mouse_face_deferred_gc
      || (f && f == hlinfo->mouse_face_mouse_frame))
    {
      BLOCK_INPUT;
      if (hlinfo->mouse_face_mouse_frame)
	note_mouse_highlight (hlinfo->mouse_face_mouse_frame,
			      hlinfo->mouse_face_mouse_x,
			      hlinfo->mouse_face_mouse_y);
      hlinfo->mouse_face_deferred_gc = 0;
      UNBLOCK_INPUT;
    }

  /* Set the cursor type to whatever they wanted.  In a minibuffer
     window, we want the cursor to appear only if we are reading input
     from this window, and we want the cursor to be taken from the
     frame parameters.  For the selected window, we use either its
     buffer-local value or the value from the frame parameters if the
     buffer doesn't define its local value for the cursor type.  */
  sw = XWINDOW (f->selected_window);
  frame_desired_cursor = Fcdr (Fassq (Qcursor_type, f->param_alist));
  if (cursor_in_echo_area
      && FRAME_HAS_MINIBUF_P (f)
      && EQ (FRAME_MINIBUF_WINDOW (f), echo_area_window)
      && sw == XWINDOW (echo_area_window))
    new_cursor = frame_desired_cursor;
  else
    {
      struct buffer *b = XBUFFER (sw->buffer);

      if (EQ (BVAR (b,cursor_type), Qt))
	new_cursor = frame_desired_cursor;
      else if (NILP (BVAR (b, cursor_type))) /* nil means no cursor */
	new_cursor = Fcons (Qbar, make_number (0));
      else
	new_cursor = BVAR (b, cursor_type);
    }

  IT_set_cursor_type (f, new_cursor);

  IT_cmgoto (f);  /* position cursor when update is done */
}

/* Copy LEN glyphs displayed on a single line whose vertical position
   is YPOS, beginning at horizontal position XFROM to horizontal
   position XTO, by moving blocks in the video memory.  Used by
   functions that insert and delete glyphs.  */
static void
IT_copy_glyphs (int xfrom, int xto, size_t len, int ypos)
{
  /* The offsets of source and destination relative to the
     conventional memory selector.  */
  int from = 2 * (xfrom + screen_size_X * ypos) + ScreenPrimary;
  int to = 2 * (xto + screen_size_X * ypos) + ScreenPrimary;

  if (from == to || len <= 0)
    return;

  _farsetsel (_dos_ds);

  /* The source and destination might overlap, so we need to move
     glyphs non-destructively.  */
  if (from > to)
    {
      for ( ; len; from += 2, to += 2, len--)
	_farnspokew (to, _farnspeekw (from));
    }
  else
    {
      from += (len - 1) * 2;
      to += (len - 1) * 2;
      for ( ; len; from -= 2, to -= 2, len--)
	_farnspokew (to, _farnspeekw (from));
    }
  if (screen_virtual_segment)
    dosv_refresh_virtual_screen (ypos * screen_size_X * 2, screen_size_X);
}

/* Insert and delete glyphs.  */
static void
IT_insert_glyphs (struct frame *f, struct glyph *start, int len)
{
  int shift_by_width = screen_size_X - (new_pos_X + len);

  /* Shift right the glyphs from the nominal cursor position to the
     end of this line.  */
  IT_copy_glyphs (new_pos_X, new_pos_X + len, shift_by_width, new_pos_Y);

  /* Now write the glyphs to be inserted.  */
  IT_write_glyphs (f, start, len);
}

static void
IT_delete_glyphs (struct frame *f, int n)
{
  abort ();
}

/* set-window-configuration on window.c needs this.  */
void
x_set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  set_menu_bar_lines (f, value, oldval);
}

/* This was copied from xfaces.c  */

extern Lisp_Object Qbackground_color;
extern Lisp_Object Qforeground_color;
Lisp_Object Qreverse;
extern Lisp_Object Qtitle;

/* IT_set_terminal_modes is called when emacs is started,
   resumed, and whenever the screen is redrawn!  */

static void
IT_set_terminal_modes (struct terminal *term)
{
  struct tty_display_info *tty;

  /* If called with initial terminal, it's too early to do anything
     useful.  */
  if (term->type == output_initial)
    return;

  tty = term->display_info.tty;

  if (tty->termscript)
    fprintf (tty->termscript, "\n<SET_TERM>");

  screen_size_X = ScreenCols ();
  screen_size_Y = ScreenRows ();
  screen_size = screen_size_X * screen_size_Y;

  new_pos_X = new_pos_Y = 0;
  current_pos_X = current_pos_Y = -1;

  if (term_setup_done)
    return;
  term_setup_done = 1;

  startup_screen_size_X = screen_size_X;
  startup_screen_size_Y = screen_size_Y;
  startup_screen_attrib = ScreenAttrib;

  /* Is DOS/V (or any other RSIS software which relocates
     the screen) installed?  */
  {
    unsigned short es_value;
    __dpmi_regs regs;

    regs.h.ah = 0xfe;	/* get relocated screen address */
    if (ScreenPrimary == 0xb0000UL || ScreenPrimary == 0xb8000UL)
      regs.x.es = (ScreenPrimary >> 4) & 0xffff;
    else if (screen_old_address) /* already switched to Japanese mode once */
      regs.x.es = (screen_old_address >> 4) & 0xffff;
    else
      regs.x.es = ScreenMode () == 7 ? 0xb000 : 0xb800;
    regs.x.di = 0;
    es_value = regs.x.es;
    __dpmi_int (0x10, &regs);

    if (regs.x.es != es_value)
      {
	/* screen_old_address is only set if ScreenPrimary does NOT
	   already point to the relocated buffer address returned by
	   the Int 10h/AX=FEh call above.  DJGPP v2.02 and later sets
	   ScreenPrimary to that address at startup under DOS/V.  */
	if (regs.x.es != ((ScreenPrimary >> 4) & 0xffff))
	  screen_old_address = ScreenPrimary;
	screen_virtual_segment = regs.x.es;
	screen_virtual_offset  = regs.x.di;
	ScreenPrimary = (screen_virtual_segment << 4) + screen_virtual_offset;
      }
  }

  ScreenGetCursor (&startup_pos_Y, &startup_pos_X);
  ScreenRetrieve (startup_screen_buffer = xmalloc (screen_size * 2));

  bright_bg ();
}

/* IT_reset_terminal_modes is called when emacs is
   suspended or killed.  */

static void
IT_reset_terminal_modes (struct terminal *term)
{
  int display_row_start = (int) ScreenPrimary;
  int saved_row_len     = startup_screen_size_X * 2;
  int update_row_len    = ScreenCols () * 2, current_rows = ScreenRows ();
  int to_next_row       = update_row_len;
  unsigned char *saved_row = startup_screen_buffer;
  int cursor_pos_X = ScreenCols () - 1, cursor_pos_Y = ScreenRows () - 1;
  struct tty_display_info *tty = term->display_info.tty;

  if (tty->termscript)
    fprintf (tty->termscript, "\n<RESET_TERM>");

  if (!term_setup_done)
    return;

  mouse_off ();

  /* Leave the video system in the same state as we found it,
     as far as the blink/bright-background bit is concerned.  */
  maybe_enable_blinking ();

  /* We have a situation here.
     We cannot just do ScreenUpdate(startup_screen_buffer) because
     the luser could have changed screen dimensions inside Emacs
     and failed (or didn't want) to restore them before killing
     Emacs.  ScreenUpdate() uses the *current* screen dimensions and
     thus will happily use memory outside what was allocated for
     `startup_screen_buffer'.
     Thus we only restore as much as the current screen dimensions
     can hold, and clear the rest (if the saved screen is smaller than
     the current) with the color attribute saved at startup.  The cursor
     is also restored within the visible dimensions.  */

  ScreenAttrib = startup_screen_attrib;

  /* Don't restore the screen if we are exiting less than 2 seconds
     after startup: we might be crashing, and the screen might show
     some vital clues to what's wrong.  */
  if (clock () - startup_time >= 2*CLOCKS_PER_SEC)
    {
      ScreenClear ();
      if (screen_virtual_segment)
	dosv_refresh_virtual_screen (0, screen_size);

      if (update_row_len > saved_row_len)
	update_row_len = saved_row_len;
      if (current_rows > startup_screen_size_Y)
	current_rows = startup_screen_size_Y;

      if (tty->termscript)
	fprintf (tty->termscript, "<SCREEN RESTORED (dimensions=%dx%d)>\n",
		 update_row_len / 2, current_rows);

      while (current_rows--)
	{
	  dosmemput (saved_row, update_row_len, display_row_start);
	  if (screen_virtual_segment)
	    dosv_refresh_virtual_screen (display_row_start - ScreenPrimary,
					 update_row_len / 2);
	  saved_row         += saved_row_len;
	  display_row_start += to_next_row;
	}
    }
  if (startup_pos_X < cursor_pos_X)
    cursor_pos_X = startup_pos_X;
  if (startup_pos_Y < cursor_pos_Y)
    cursor_pos_Y = startup_pos_Y;

  ScreenSetCursor (cursor_pos_Y, cursor_pos_X);
  xfree (startup_screen_buffer);
  startup_screen_buffer = NULL;

  term_setup_done = 0;
}

static void
IT_set_terminal_window (struct frame *f, int foo)
{
}

/* Remember the screen colors of the current frame, to serve as the
   default colors for newly-created frames.  */
DEFUN ("msdos-remember-default-colors", Fmsdos_remember_default_colors,
       Smsdos_remember_default_colors, 1, 1, 0,
       doc: /* Remember the screen colors of the current frame.  */)
  (Lisp_Object frame)
{
  struct frame *f;

  CHECK_FRAME (frame);
  f = XFRAME (frame);

  /* This function is called after applying default-frame-alist to the
     initial frame.  At that time, if reverse-colors option was
     specified in default-frame-alist, it was already applied, and
     frame colors are reversed.  */
  initial_screen_colors[0] = FRAME_FOREGROUND_PIXEL (f);
  initial_screen_colors[1] = FRAME_BACKGROUND_PIXEL (f);

  return Qnil;
}

void
IT_set_frame_parameters (struct frame *f, Lisp_Object alist)
{
  Lisp_Object tail;
  int i, j, length = XINT (Flength (alist));
  Lisp_Object *parms
    = (Lisp_Object *) alloca (length * sizeof (Lisp_Object));
  Lisp_Object *values
    = (Lisp_Object *) alloca (length * sizeof (Lisp_Object));
  /* Do we have to reverse the foreground and background colors?  */
  int reverse = EQ (Fcdr (Fassq (Qreverse, f->param_alist)), Qt);
  int redraw = 0, fg_set = 0, bg_set = 0;
  unsigned long orig_fg, orig_bg;
  struct tty_display_info *tty = FRAME_TTY (f);

  /* If we are creating a new frame, begin with the original screen colors
     used for the initial frame.  */
  if (!f->default_face_done_p
      && initial_screen_colors[0] != -1 && initial_screen_colors[1] != -1)
    {
      FRAME_FOREGROUND_PIXEL (f) = initial_screen_colors[0];
      FRAME_BACKGROUND_PIXEL (f) = initial_screen_colors[1];
      init_frame_faces (f);
      f->default_face_done_p = 1;
    }
  orig_fg = reverse ? FRAME_BACKGROUND_PIXEL (f) : FRAME_FOREGROUND_PIXEL (f);
  orig_bg = reverse ? FRAME_FOREGROUND_PIXEL (f) : FRAME_BACKGROUND_PIXEL (f);

  /* Extract parm names and values into those vectors.  */
  i = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    {
      Lisp_Object elt;

      elt = Fcar (tail);
      parms[i] = Fcar (elt);
      CHECK_SYMBOL (parms[i]);
      values[i] = Fcdr (elt);
      i++;
    }

  j = i;

  for (i = 0; i < j; i++)
    {
      Lisp_Object prop, val;

      prop = parms[i];
      val  = values[i];

      if (EQ (prop, Qreverse))
	reverse = EQ (val, Qt);
    }

  if (tty->termscript && reverse)
    fprintf (tty->termscript, "<INVERSE-VIDEO>\n");

  /* Now process the alist elements in reverse of specified order.  */
  for (i--; i >= 0; i--)
    {
      Lisp_Object prop, val;

      prop = parms[i];
      val  = values[i];

      if (EQ (prop, Qforeground_color))
	{
	  unsigned long new_color = load_color (f, NULL, val, reverse
						? LFACE_BACKGROUND_INDEX
						: LFACE_FOREGROUND_INDEX);
	  if (new_color !=  FACE_TTY_DEFAULT_COLOR
	      && new_color != FACE_TTY_DEFAULT_FG_COLOR
	      && new_color != FACE_TTY_DEFAULT_BG_COLOR)
	    {
	      if (!reverse)
		{
		  FRAME_FOREGROUND_PIXEL (f) = new_color;
		  /* Make sure the foreground of the default face for
		     this frame is changed as well.  */
		  update_face_from_frame_parameter (f, Qforeground_color, val);
		  fg_set = 1;
		  if (tty->termscript)
		    fprintf (tty->termscript, "<FGCOLOR %lu>\n", new_color);
		}
	      else
		{
		  FRAME_BACKGROUND_PIXEL (f) = new_color;
		  update_face_from_frame_parameter (f, Qbackground_color, val);
		  bg_set = 1;
		  if (tty->termscript)
		    fprintf (tty->termscript, "<BGCOLOR %lu>\n", new_color);
		}
	      redraw = 1;
	    }
	}
      else if (EQ (prop, Qbackground_color))
	{
	  unsigned long new_color = load_color (f, NULL, val, reverse
						? LFACE_FOREGROUND_INDEX
						: LFACE_BACKGROUND_INDEX);
	  if (new_color != FACE_TTY_DEFAULT_COLOR
	      && new_color != FACE_TTY_DEFAULT_FG_COLOR
	      && new_color != FACE_TTY_DEFAULT_BG_COLOR)
	    {
	      if (!reverse)
		{
		  FRAME_BACKGROUND_PIXEL (f) = new_color;
		  /* Make sure the background of the default face for
		     this frame is changed as well.  */
		  bg_set = 1;
		  update_face_from_frame_parameter (f, Qbackground_color, val);
		  if (tty->termscript)
		    fprintf (tty->termscript, "<BGCOLOR %lu>\n", new_color);
		}
	      else
		{
		  FRAME_FOREGROUND_PIXEL (f) = new_color;
		  fg_set = 1;
		  update_face_from_frame_parameter (f, Qforeground_color, val);
		  if (tty->termscript)
		    fprintf (tty->termscript, "<FGCOLOR %lu>\n", new_color);
		}
	      redraw = 1;
	    }
	}
      else if (EQ (prop, Qtitle))
	{
	  x_set_title (f, val);
	  if (tty->termscript)
	    fprintf (tty->termscript, "<TITLE: %s>\n", SDATA (val));
	}
      else if (EQ (prop, Qcursor_type))
	{
	  IT_set_cursor_type (f, val);
	  if (tty->termscript)
	    fprintf (tty->termscript, "<CTYPE: %s>\n",
		     EQ (val, Qbar)
		     || EQ (val, Qhbar)
		     || (CONSP (val) && (EQ (XCAR (val), Qbar)
					 || EQ (XCAR (val), Qhbar)))
		     ? "bar" : "box");
	}
      else if (EQ (prop, Qtty_type))
	{
	  internal_terminal_init ();
	  if (tty->termscript)
	    fprintf (tty->termscript, "<TERM_INIT done, TTY_TYPE: %.*s>\n",
		     SBYTES (val), SDATA (val));
	}
      store_frame_param (f, prop, val);
    }

  /* If they specified "reverse", but not the colors, we need to swap
     the current frame colors.  */
  if (reverse)
    {
      if (!fg_set)
	{
	  FRAME_FOREGROUND_PIXEL (f) = orig_bg;
	  update_face_from_frame_parameter (f, Qforeground_color,
					    tty_color_name (f, orig_bg));
	  redraw = 1;
	}
      if (!bg_set)
	{
	  FRAME_BACKGROUND_PIXEL (f) = orig_fg;
	  update_face_from_frame_parameter (f, Qbackground_color,
					    tty_color_name (f, orig_fg));
	  redraw = 1;
	}
    }

  if (redraw)
    {
      face_change_count++;	/* forces xdisp.c to recompute basic faces */
      if (f == SELECTED_FRAME ())
	redraw_frame (f);
    }
}

extern void init_frame_faces (FRAME_PTR);

#endif /* !HAVE_X_WINDOWS */


/* Do we need the internal terminal?  */

void
internal_terminal_init (void)
{
  static int init_needed = 1;
  char *term = getenv ("TERM"), *colors;
  struct frame *sf = SELECTED_FRAME ();
  struct tty_display_info *tty;

#ifdef HAVE_X_WINDOWS
  if (!inhibit_window_system)
    return;
#endif

  /* If this is the initial terminal, we are done here.  */
  if (sf->output_method == output_initial)
    return;

  internal_terminal
    = (!noninteractive) && term && !strcmp (term, "internal");

#ifndef HAVE_X_WINDOWS
  if (!internal_terminal || inhibit_window_system)
    {
      sf->output_method = output_termcap;
      return;
    }

  tty = FRAME_TTY (sf);
  KVAR (current_kboard, Vwindow_system) = Qpc;
  sf->output_method = output_msdos_raw;
  if (init_needed)
    {
      if (!tty->termscript && getenv ("EMACSTEST"))
	tty->termscript = fopen (getenv ("EMACSTEST"), "wt");
      if (tty->termscript)
	{
	  time_t now = time (NULL);
	  struct tm *tnow = localtime (&now);
	  char tbuf[100];

	  strftime (tbuf, sizeof (tbuf) - 1, "%a %b %e %Y %H:%M:%S %Z", tnow);
	  fprintf (tty->termscript, "\nEmacs session started at %s\n", tbuf);
	  fprintf (tty->termscript,   "=====================\n\n");
	}

      Vinitial_window_system = Qpc;
      Vwindow_system_version = make_number (24); /* RE Emacs version */
      tty->terminal->type = output_msdos_raw;

      /* If Emacs was dumped on DOS/V machine, forget the stale VRAM
	 address.  */
      screen_old_address = 0;

      /* Forget the stale screen colors as well.  */
      initial_screen_colors[0] = initial_screen_colors[1] = -1;

      FRAME_BACKGROUND_PIXEL (SELECTED_FRAME ()) = 7; /* White */
      FRAME_FOREGROUND_PIXEL (SELECTED_FRAME ()) = 0; /* Black */
      bright_bg ();
      colors = getenv ("EMACSCOLORS");
      if (colors && strlen (colors) >= 2)
	{
	  /* The colors use 4 bits each (we enable bright background).  */
	  if (isdigit (colors[0]))
	    colors[0] -= '0';
	  else if (isxdigit (colors[0]))
	    colors[0] -= (isupper (colors[0]) ? 'A' : 'a') - 10;
	  if (colors[0] >= 0 && colors[0] < 16)
	    FRAME_FOREGROUND_PIXEL (SELECTED_FRAME ()) = colors[0];
	  if (isdigit (colors[1]))
	    colors[1] -= '0';
	  else if (isxdigit (colors[1]))
	    colors[1] -= (isupper (colors[1]) ? 'A' : 'a') - 10;
	  if (colors[1] >= 0 && colors[1] < 16)
	    FRAME_BACKGROUND_PIXEL (SELECTED_FRAME ()) = colors[1];
	}
      the_only_display_info.mouse_highlight.mouse_face_mouse_frame = NULL;
      the_only_display_info.mouse_highlight.mouse_face_deferred_gc = 0;
      the_only_display_info.mouse_highlight.mouse_face_beg_row =
	the_only_display_info.mouse_highlight.mouse_face_beg_col = -1;
      the_only_display_info.mouse_highlight.mouse_face_end_row =
	the_only_display_info.mouse_highlight.mouse_face_end_col = -1;
      the_only_display_info.mouse_highlight.mouse_face_face_id = DEFAULT_FACE_ID;
      the_only_display_info.mouse_highlight.mouse_face_window = Qnil;
      the_only_display_info.mouse_highlight.mouse_face_mouse_x =
	the_only_display_info.mouse_highlight.mouse_face_mouse_y = 0;
      the_only_display_info.mouse_highlight.mouse_face_defer = 0;
      the_only_display_info.mouse_highlight.mouse_face_hidden = 0;

      if (have_mouse)	/* detected in dos_ttraw, which see */
	{
	  have_mouse = 1;	/* enable mouse */
	  mouse_visible = 0;
	  mouse_setup_buttons (mouse_button_count);
	  tty->terminal->mouse_position_hook = &mouse_get_pos;
	  mouse_init ();
	}

      if (tty->termscript && screen_size)
	fprintf (tty->termscript, "<SCREEN SAVED (dimensions=%dx%d)>\n",
		 screen_size_X, screen_size_Y);

      init_frame_faces (sf);
      init_needed = 0;
    }
#endif
}

void
initialize_msdos_display (struct terminal *term)
{
  term->rif = 0;		/* we don't support window-based display */
  term->cursor_to_hook = term->raw_cursor_to_hook = IT_cursor_to;
  term->clear_to_end_hook = IT_clear_to_end;
  term->clear_frame_hook = IT_clear_screen;
  term->clear_end_of_line_hook = IT_clear_end_of_line;
  term->ins_del_lines_hook = 0;
  term->insert_glyphs_hook = IT_insert_glyphs;
  term->write_glyphs_hook = IT_write_glyphs;
  term->delete_glyphs_hook = IT_delete_glyphs;
  term->ring_bell_hook = IT_ring_bell;
  term->reset_terminal_modes_hook = IT_reset_terminal_modes;
  term->set_terminal_modes_hook = IT_set_terminal_modes;
  term->set_terminal_window_hook = IT_set_terminal_window;
  term->update_begin_hook = IT_update_begin;
  term->update_end_hook = IT_update_end;
  term->frame_up_to_date_hook = IT_frame_up_to_date;
  term->mouse_position_hook = 0; /* set later by dos_ttraw */
  term->frame_rehighlight_hook = 0;
  term->frame_raise_lower_hook = 0;
  term->set_vertical_scroll_bar_hook = 0;
  term->condemn_scroll_bars_hook = 0;
  term->redeem_scroll_bar_hook = 0;
  term->judge_scroll_bars_hook = 0;
  term->read_socket_hook = &tty_read_avail_input; /* from keyboard.c */
}

int
dos_get_saved_screen (char **screen, int *rows, int *cols)
{
#ifndef HAVE_X_WINDOWS
  *screen = startup_screen_buffer;
  *cols = startup_screen_size_X;
  *rows = startup_screen_size_Y;
  return *screen != (char *)0;
#else
  return 0;
#endif
}

#ifndef HAVE_X_WINDOWS

/* We are not X, but we can emulate it well enough for our needs... */
void
check_x (void)
{
  if (! FRAME_MSDOS_P (SELECTED_FRAME ()))
    error ("Not running under a window system");
}

#endif


/* ----------------------- Keyboard control ----------------------
 *
 * Keymaps reflect the following keyboard layout:
 *
 *    0  1  2  3  4  5  6  7  8  9  10 11 12  BS
 *    TAB 15 16 17 18 19 20 21 22 23 24 25 26 (41)
 *    CLOK 30 31 32 33 34 35 36 37 38 39 40 (41) RET
 *    SH () 45 46 47 48 49 50 51 52 53 54  SHIFT
 *                    SPACE
 */

#define Ignore	0x0000
#define Normal	0x0000	/* normal key - alt changes scan-code */
#define FctKey	0x1000	/* func key if c == 0, else c */
#define Special	0x2000	/* func key even if c != 0 */
#define ModFct	0x3000	/* special if mod-keys, else 'c' */
#define Map	0x4000	/* alt scan-code, map to unshift/shift key */
#define KeyPad	0x5000	/* map to insert/kp-0 depending on c == 0xe0 */
#define Grey	0x6000	/* Grey keypad key */

#define Alt	0x0100	/* alt scan-code */
#define Ctrl	0x0200	/* ctrl scan-code */
#define Shift	0x0400	/* shift scan-code */

static int extended_kbd; /* 101 (102) keyboard present. */

struct kbd_translate {
  unsigned char  sc;
  unsigned char  ch;
  unsigned short code;
};

struct dos_keyboard_map
{
  char *unshifted;
  char *shifted;
  char *alt_gr;
  struct kbd_translate *translate_table;
};


static struct dos_keyboard_map us_keyboard = {
/* 0         1         2         3         4         5      */
/* 01234567890123456789012345678901234567890 12345678901234 */
  "`1234567890-=  qwertyuiop[]   asdfghjkl;'\\   zxcvbnm,./  ",
/* 0123456789012345678901234567890123456789 012345678901234 */
  "~!@#$%^&*()_+  QWERTYUIOP{}   ASDFGHJKL:\"|   ZXCVBNM<>?  ",
  0,				/* no Alt-Gr key */
  0				/* no translate table */
};

static struct dos_keyboard_map fr_keyboard = {
/* 0         1         2         3         4         5      */
/* 012 3456789012345678901234567890123456789012345678901234 */
  "˝&Ç\",(-ä_ÄÖ)=  azertyuiop^$   qsdfghjklmó*   wxcvbnm;:!  ",
/* 0123456789012345678901234567890123456789012345678901234 */
  " 1234567890¯+  AZERTYUIOP˘ú   QSDFGHJKLM%Ê   WXCVBN?./ı  ",
/* 01234567 89012345678901234567890123456789012345678901234 */
  "  ~#{[|`\\^@]}             œ                              ",
  0				/* no translate table */
};

/*
 * Italian keyboard support, country code 39.
 * '<' 56:3c*0000
 * '>' 56:3e*0000
 * added also {,},` as, respectively, AltGr-8, AltGr-9, AltGr-'
 * Donated by Stefano Brozzi <brozzis@mag00.cedi.unipr.it>
 */

static struct kbd_translate it_kbd_translate_table[] = {
  { 0x56, 0x3c, Normal | 13 },
  { 0x56, 0x3e, Normal | 27 },
  { 0, 0, 0 }
};
static struct dos_keyboard_map it_keyboard = {
/* 0          1         2         3         4         5     */
/* 0 123456789012345678901234567890123456789012345678901234 */
  "\\1234567890'ç< qwertyuiopä+>  asdfghjklïÖó   zxcvbnm,.-  ",
/* 01 23456789012345678901234567890123456789012345678901234 */
  "|!\"ú$%&/()=?^> QWERTYUIOPÇ*   ASDFGHJKLá¯ı   ZXCVBNM;:_  ",
/* 0123456789012345678901234567890123456789012345678901234 */
  "        {}~`             []             @#               ",
  it_kbd_translate_table
};

static struct dos_keyboard_map dk_keyboard = {
/* 0         1         2         3         4         5      */
/* 0123456789012345678901234567890123456789012345678901234 */
  "´1234567890+|  qwertyuiopÜ~   asdfghjklëõ'   zxcvbnm,.-  ",
/* 01 23456789012345678901234567890123456789012345678901234 */
  "ı!\"#$%&/()=?`  QWERTYUIOPè^   ASDFGHJKLíù*   ZXCVBNM;:_  ",
/* 0123456789012345678901234567890123456789012345678901234 */
  "  @ú$  {[]} |                                             ",
  0				/* no translate table */
};

static struct kbd_translate jp_kbd_translate_table[] = {
  { 0x73, 0x5c, Normal | 0 },
  { 0x73, 0x5f, Normal | 0 },
  { 0x73, 0x1c, Map | 0 },
  { 0x7d, 0x5c, Normal | 13 },
  { 0x7d, 0x7c, Normal | 13 },
  { 0x7d, 0x1c, Map | 13 },
  { 0, 0, 0 }
};
static struct dos_keyboard_map jp_keyboard = {
/*  0         1          2         3         4         5     */
/*  0123456789012 345678901234567890123456789012345678901234 */
  "\\1234567890-^\\ qwertyuiop@[   asdfghjkl;:]   zxcvbnm,./  ",
/*  01 23456789012345678901234567890123456789012345678901234 */
   "_!\"#$%&'()~=~| QWERTYUIOP`{   ASDFGHJKL+*}   ZXCVBNM<>?  ",
  0,				/* no Alt-Gr key */
  jp_kbd_translate_table
};

static struct keyboard_layout_list
{
  int country_code;
  struct dos_keyboard_map *keyboard_map;
} keyboard_layout_list[] =
{
  { 1, &us_keyboard },
  { 33, &fr_keyboard },
  { 39, &it_keyboard },
  { 45, &dk_keyboard },
  { 81, &jp_keyboard }
};

static struct dos_keyboard_map *keyboard;
static int keyboard_map_all;
static int international_keyboard;

int
dos_set_keyboard (int code, int always)
{
  int i;
  _go32_dpmi_registers regs;

  /* See if Keyb.Com is installed (for international keyboard support).
     Note: calling Int 2Fh via int86 wedges the DOS box on some versions
     of Windows 9X!  So don't do that!  */
  regs.x.ax = 0xad80;
  regs.x.ss = regs.x.sp = regs.x.flags = 0;
  _go32_dpmi_simulate_int (0x2f, &regs);
  if (regs.h.al == 0xff)
    international_keyboard = 1;

  /* Initialize to US settings, for countries that don't have their own.  */
  keyboard = keyboard_layout_list[0].keyboard_map;
  keyboard_map_all = always;
  dos_keyboard_layout = 1;

  for (i = 0; i < (sizeof (keyboard_layout_list)/sizeof (struct keyboard_layout_list)); i++)
    if (code == keyboard_layout_list[i].country_code)
      {
	keyboard = keyboard_layout_list[i].keyboard_map;
	keyboard_map_all = always;
	dos_keyboard_layout = code;
	return 1;
      }
  return 0;
}

static struct
{
  unsigned char char_code;	/* normal code	*/
  unsigned char meta_code;	/* M- code	*/
  unsigned char keypad_code;	/* keypad code	*/
  unsigned char editkey_code;	/* edit key	*/
} keypad_translate_map[] = {
  { '0',  '0',  0xb0, /* kp-0 */		0x63 /* insert */ },
  { '1',  '1',  0xb1, /* kp-1 */		0x57 /* end */    },
  { '2',  '2',  0xb2, /* kp-2 */		0x54 /* down */   },
  { '3',  '3',  0xb3, /* kp-3 */		0x56 /* next */   },
  { '4',  '4',  0xb4, /* kp-4 */		0x51 /* left */   },
  { '5',  '5',  0xb5, /* kp-5 */		0xb5 /* kp-5 */   },
  { '6',  '6',  0xb6, /* kp-6 */		0x53 /* right */  },
  { '7',  '7',  0xb7, /* kp-7 */		0x50 /* home */   },
  { '8',  '8',  0xb8, /* kp-8 */		0x52 /* up */     },
  { '9',  '9',  0xb9, /* kp-9 */		0x55 /* prior */  },
  { '.',  '-',  0xae, /* kp-decimal */		0xff  /* delete */}
};

static struct
{
  unsigned char char_code;	/* normal code	*/
  unsigned char keypad_code;	/* keypad code	*/
} grey_key_translate_map[] = {
  { '/',  0xaf /* kp-decimal */  },
  { '*',  0xaa /* kp-multiply */ },
  { '-',  0xad /* kp-subtract */ },
  { '+',  0xab /* kp-add */      },
  { '\r', 0x8d  /* kp-enter */   }
};

static unsigned short
ibmpc_translate_map[] =
{
  /* --------------- 00 to 0f --------------- */
  Normal | 0xff,	/* Ctrl Break + Alt-NNN */
  Alt | ModFct | 0x1b,		/* Escape */
  Normal | 1,			/* '1' */
  Normal | 2,			/* '2' */
  Normal | 3,			/* '3' */
  Normal | 4,			/* '4' */
  Normal | 5,			/* '5' */
  Normal | 6,			/* '6' */
  Normal | 7,			/* '7' */
  Normal | 8,			/* '8' */
  Normal | 9,			/* '9' */
  Normal | 10,			/* '0' */
  Normal | 11,			/* '-' */
  Normal | 12,			/* '=' */
  Special | 0x08,		/* Backspace */
  ModFct | 0x74,		/* Tab/Backtab */

  /* --------------- 10 to 1f --------------- */
  Map | 15,			/* 'q' */
  Map | 16,			/* 'w' */
  Map | 17,			/* 'e' */
  Map | 18,			/* 'r' */
  Map | 19,			/* 't' */
  Map | 20,			/* 'y' */
  Map | 21,			/* 'u' */
  Map | 22,			/* 'i' */
  Map | 23,			/* 'o' */
  Map | 24,			/* 'p' */
  Map | 25,			/* '[' */
  Map | 26,			/* ']' */
  ModFct | 0x0d,		/* Return */
  Ignore,			/* Ctrl */
  Map | 30,			/* 'a' */
  Map | 31,			/* 's' */

  /* --------------- 20 to 2f --------------- */
  Map | 32,			/* 'd' */
  Map | 33,			/* 'f' */
  Map | 34,			/* 'g' */
  Map | 35,			/* 'h' */
  Map | 36,			/* 'j' */
  Map | 37,			/* 'k' */
  Map | 38,			/* 'l' */
  Map | 39,			/* ';' */
  Map | 40,			/* '\'' */
  Map |  0,			/* '`' */
  Ignore,			/* Left shift */
  Map | 41,			/* '\\' */
  Map | 45,			/* 'z' */
  Map | 46,			/* 'x' */
  Map | 47,			/* 'c' */
  Map | 48,			/* 'v' */

  /* --------------- 30 to 3f --------------- */
  Map | 49,			/* 'b' */
  Map | 50,			/* 'n' */
  Map | 51,			/* 'm' */
  Map | 52,			/* ',' */
  Map | 53,			/* '.' */
  Map | 54,			/* '/' */
  Ignore,			/* Right shift */
  Grey | 1,			/* Grey * */
  Ignore,			/* Alt */
  Normal | 55,			/* ' ' */
  Ignore,			/* Caps Lock */
  FctKey | 0xbe,		/* F1 */
  FctKey | 0xbf,		/* F2 */
  FctKey | 0xc0,		/* F3 */
  FctKey | 0xc1,		/* F4 */
  FctKey | 0xc2,		/* F5 */

  /* --------------- 40 to 4f --------------- */
  FctKey | 0xc3,		/* F6 */
  FctKey | 0xc4,		/* F7 */
  FctKey | 0xc5,		/* F8 */
  FctKey | 0xc6,		/* F9 */
  FctKey | 0xc7,		/* F10 */
  Ignore,			/* Num Lock */
  Ignore,			/* Scroll Lock */
  KeyPad | 7,			/* Home */
  KeyPad | 8,			/* Up */
  KeyPad | 9,			/* Page Up */
  Grey | 2,			/* Grey - */
  KeyPad | 4,			/* Left */
  KeyPad | 5,			/* Keypad 5 */
  KeyPad | 6,			/* Right */
  Grey | 3,			/* Grey + */
  KeyPad | 1,			/* End */

  /* --------------- 50 to 5f --------------- */
  KeyPad | 2,			/* Down */
  KeyPad | 3,			/* Page Down */
  KeyPad | 0,			/* Insert */
  KeyPad | 10,			/* Delete */
  Shift | FctKey | 0xbe,	/* (Shift) F1 */
  Shift | FctKey | 0xbf,	/* (Shift) F2 */
  Shift | FctKey | 0xc0,	/* (Shift) F3 */
  Shift | FctKey | 0xc1,	/* (Shift) F4 */
  Shift | FctKey | 0xc2,	/* (Shift) F5 */
  Shift | FctKey | 0xc3,	/* (Shift) F6 */
  Shift | FctKey | 0xc4,	/* (Shift) F7 */
  Shift | FctKey | 0xc5,	/* (Shift) F8 */
  Shift | FctKey | 0xc6,	/* (Shift) F9 */
  Shift | FctKey | 0xc7,	/* (Shift) F10 */
  Ctrl | FctKey | 0xbe,		/* (Ctrl) F1 */
  Ctrl | FctKey | 0xbf,		/* (Ctrl) F2 */

  /* --------------- 60 to 6f --------------- */
  Ctrl | FctKey | 0xc0,		/* (Ctrl) F3 */
  Ctrl | FctKey | 0xc1,		/* (Ctrl) F4 */
  Ctrl | FctKey | 0xc2,		/* (Ctrl) F5 */
  Ctrl | FctKey | 0xc3,		/* (Ctrl) F6 */
  Ctrl | FctKey | 0xc4,		/* (Ctrl) F7 */
  Ctrl | FctKey | 0xc5,		/* (Ctrl) F8 */
  Ctrl | FctKey | 0xc6,		/* (Ctrl) F9 */
  Ctrl | FctKey | 0xc7,		/* (Ctrl) F10 */
  Alt | FctKey | 0xbe,		/* (Alt) F1 */
  Alt | FctKey | 0xbf,		/* (Alt) F2 */
  Alt | FctKey | 0xc0,		/* (Alt) F3 */
  Alt | FctKey | 0xc1,		/* (Alt) F4 */
  Alt | FctKey | 0xc2,		/* (Alt) F5 */
  Alt | FctKey | 0xc3,		/* (Alt) F6 */
  Alt | FctKey | 0xc4,		/* (Alt) F7 */
  Alt | FctKey | 0xc5,		/* (Alt) F8 */

  /* --------------- 70 to 7f --------------- */
  Alt | FctKey | 0xc6,		/* (Alt) F9 */
  Alt | FctKey | 0xc7,		/* (Alt) F10 */
  Ctrl | FctKey | 0x6d,		/* (Ctrl) Sys Rq */
  Ctrl | KeyPad | 4,		/* (Ctrl) Left */
  Ctrl | KeyPad | 6,		/* (Ctrl) Right */
  Ctrl | KeyPad | 1,		/* (Ctrl) End */
  Ctrl | KeyPad | 3,		/* (Ctrl) Page Down */
  Ctrl | KeyPad | 7,		/* (Ctrl) Home */
  Alt | Map | 1,		/* '1' */
  Alt | Map | 2,		/* '2' */
  Alt | Map | 3,		/* '3' */
  Alt | Map | 4,		/* '4' */
  Alt | Map | 5,		/* '5' */
  Alt | Map | 6,		/* '6' */
  Alt | Map | 7,		/* '7' */
  Alt | Map | 8,		/* '8' */

  /* --------------- 80 to 8f --------------- */
  Alt | Map | 9,		/* '9' */
  Alt | Map | 10,		/* '0' */
  Alt | Map | 11,		/* '-' */
  Alt | Map | 12,		/* '=' */
  Ctrl | KeyPad | 9,		/* (Ctrl) Page Up */
  FctKey | 0xc8,		/* F11 */
  FctKey | 0xc9,		/* F12 */
  Shift | FctKey | 0xc8,	/* (Shift) F11 */
  Shift | FctKey | 0xc9,	/* (Shift) F12 */
  Ctrl | FctKey | 0xc8,		/* (Ctrl) F11 */
  Ctrl | FctKey | 0xc9,		/* (Ctrl) F12 */
  Alt | FctKey | 0xc8,		/* (Alt) F11 */
  Alt | FctKey | 0xc9,		/* (Alt) F12 */
  Ctrl | KeyPad | 8,		/* (Ctrl) Up */
  Ctrl | Grey | 2,		/* (Ctrl) Grey - */
  Ctrl | KeyPad | 5,		/* (Ctrl) Keypad 5 */

  /* --------------- 90 to 9f --------------- */
  Ctrl | Grey | 3,		/* (Ctrl) Grey + */
  Ctrl | KeyPad | 2,		/* (Ctrl) Down */
  Ctrl | KeyPad | 0,		/* (Ctrl) Insert */
  Ctrl | KeyPad | 10,		/* (Ctrl) Delete */
  Ctrl | FctKey | 0x09,		/* (Ctrl) Tab */
  Ctrl | Grey | 0,		/* (Ctrl) Grey / */
  Ctrl | Grey | 1,		/* (Ctrl) Grey * */
  Alt | FctKey | 0x50,		/* (Alt) Home */
  Alt | FctKey | 0x52,		/* (Alt) Up */
  Alt | FctKey | 0x55,		/* (Alt) Page Up */
  Ignore,			/* NO KEY */
  Alt | FctKey | 0x51,		/* (Alt) Left */
  Ignore,			/* NO KEY */
  Alt | FctKey | 0x53,		/* (Alt) Right */
  Ignore,			/* NO KEY */
  Alt | FctKey | 0x57,		/* (Alt) End */

  /* --------------- a0 to af --------------- */
  Alt | KeyPad | 2,		/* (Alt) Down */
  Alt | KeyPad | 3,		/* (Alt) Page Down */
  Alt | KeyPad | 0,		/* (Alt) Insert */
  Alt | KeyPad | 10,		/* (Alt) Delete */
  Alt | Grey | 0,		/* (Alt) Grey / */
  Alt | FctKey | 0x09,		/* (Alt) Tab */
  Alt | Grey | 4		/* (Alt) Keypad Enter */
};

/* These bit-positions corresponds to values returned by BIOS */
#define SHIFT_P		0x0003	/* two bits! */
#define CTRL_P		0x0004
#define ALT_P		0x0008
#define SCRLOCK_P	0x0010
#define NUMLOCK_P	0x0020
#define CAPSLOCK_P	0x0040
#define ALT_GR_P	0x0800
#define SUPER_P		0x4000	/* pseudo */
#define HYPER_P		0x8000	/* pseudo */

static int
dos_get_modifiers (int *keymask)
{
  union REGS regs;
  int mask, modifiers = 0;

  /* Calculate modifier bits */
  regs.h.ah = extended_kbd ? 0x12 : 0x02;
  int86 (0x16, &regs, &regs);

  if (!extended_kbd)
    {
      mask = regs.h.al & (SHIFT_P | CTRL_P | ALT_P |
			  SCRLOCK_P | NUMLOCK_P | CAPSLOCK_P);
    }
  else
    {
      mask = regs.h.al & (SHIFT_P |
			  SCRLOCK_P | NUMLOCK_P | CAPSLOCK_P);

      /* Do not break international keyboard support.   */
      /* When Keyb.Com is loaded, the right Alt key is  */
      /* used for accessing characters like { and } 	  */
      if (regs.h.ah & 2)		/* Left ALT pressed ? */
	mask |= ALT_P;

      if ((regs.h.ah & 8) != 0)		/* Right ALT pressed ? */
	{
	  mask |= ALT_GR_P;
	  if (dos_hyper_key == 1)
	    {
	      mask |= HYPER_P;
	      modifiers |= hyper_modifier;
	    }
	  else if (dos_super_key == 1)
	    {
	      mask |= SUPER_P;
	      modifiers |= super_modifier;
	    }
	  else if (!international_keyboard)
	    {
	      /* If Keyb.Com is NOT installed, let Right Alt behave
		 like the Left Alt.  */
	      mask &= ~ALT_GR_P;
	      mask |= ALT_P;
	    }
	}

      if (regs.h.ah & 1)		/* Left CTRL pressed ? */
	mask |= CTRL_P;

      if (regs.h.ah & 4)	 	/* Right CTRL pressed ? */
	{
	  if (dos_hyper_key == 2)
	    {
	      mask |= HYPER_P;
	      modifiers |= hyper_modifier;
	    }
	  else if (dos_super_key == 2)
	    {
	      mask |= SUPER_P;
	      modifiers |= super_modifier;
	    }
	  else
	    mask |= CTRL_P;
	}
    }

  if (mask & SHIFT_P)
    modifiers |= shift_modifier;
  if (mask & CTRL_P)
    modifiers |= ctrl_modifier;
  if (mask & ALT_P)
    modifiers |= meta_modifier;

  if (keymask)
    *keymask = mask;
  return modifiers;
}

#define NUM_RECENT_DOSKEYS (100)
int recent_doskeys_index;	/* Index for storing next element into recent_doskeys */
int total_doskeys;		/* Total number of elements stored into recent_doskeys */
Lisp_Object recent_doskeys;	/* A vector, holding the last 100 keystrokes */

DEFUN ("recent-doskeys", Frecent_doskeys, Srecent_doskeys, 0, 0, 0,
       doc: /* Return vector of last 100 keyboard input values seen in dos_rawgetc.
Each input key receives two values in this vector: first the ASCII code,
and then the scan code.  */)
  (void)
{
  Lisp_Object val, *keys = XVECTOR (recent_doskeys)->contents;

  if (total_doskeys < NUM_RECENT_DOSKEYS)
    return Fvector (total_doskeys, keys);
  else
    {
      val = Fvector (NUM_RECENT_DOSKEYS, keys);
      memcpy (XVECTOR (val)->contents, keys + recent_doskeys_index,
	      (NUM_RECENT_DOSKEYS - recent_doskeys_index) * sizeof (Lisp_Object));
      memcpy (XVECTOR (val)->contents + NUM_RECENT_DOSKEYS - recent_doskeys_index,
	      keys, recent_doskeys_index * sizeof (Lisp_Object));
      return val;
    }
}

/* Get a char from keyboard.  Function keys are put into the event queue.  */
static int
dos_rawgetc (void)
{
  struct input_event event;
  union REGS regs;
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (SELECTED_FRAME ());
  EVENT_INIT (event);

#ifndef HAVE_X_WINDOWS
  /* Maybe put the cursor where it should be.  */
  IT_cmgoto (SELECTED_FRAME ());
#endif

  /* The following condition is equivalent to `kbhit ()', except that
     it uses the bios to do its job.  This pleases DESQview/X.  */
  while ((regs.h.ah = extended_kbd ? 0x11 : 0x01),
	 int86 (0x16, &regs, &regs),
	 (regs.x.flags & 0x40) == 0)
    {
      union REGS regs;
      register unsigned char c;
      int modifiers, sc, code = -1, mask, kp_mode;

      regs.h.ah = extended_kbd ? 0x10 : 0x00;
      int86 (0x16, &regs, &regs);
      c = regs.h.al;
      sc = regs.h.ah;

      total_doskeys += 2;
      XVECTOR (recent_doskeys)->contents[recent_doskeys_index++]
	= make_number (c);
      if (recent_doskeys_index == NUM_RECENT_DOSKEYS)
	recent_doskeys_index = 0;
      XVECTOR (recent_doskeys)->contents[recent_doskeys_index++]
	= make_number (sc);
      if (recent_doskeys_index == NUM_RECENT_DOSKEYS)
	recent_doskeys_index = 0;

      modifiers = dos_get_modifiers (&mask);

#ifndef HAVE_X_WINDOWS
      if (!NILP (Vdos_display_scancodes))
	{
	  char buf[11];
	  sprintf (buf, "%02x:%02x*%04x",
		   (unsigned) (sc&0xff), (unsigned) c, mask);
	  dos_direct_output (screen_size_Y - 2, screen_size_X - 12, buf, 10);
	}
#endif

      if (sc == 0xe0)
	{
	  switch (c)
	    {
	    case 10:		/* Ctrl Grey Enter */
	      code = Ctrl | Grey | 4;
	      break;
	    case 13:		/* Grey Enter */
	      code = Grey | 4;
	      break;
	    case '/':		/* Grey / */
	      code = Grey | 0;
	      break;
	    default:
	      continue;
	    };
	  c = 0;
	}
      else
	{
	  /* Try the keyboard-private translation table first.  */
	  if (keyboard->translate_table)
	    {
	      struct kbd_translate *p = keyboard->translate_table;

	      while (p->sc)
		{
		  if (p->sc == sc && p->ch == c)
		    {
		      code = p->code;
		      break;
		    }
		  p++;
		}
	    }
	  /* If the private table didn't translate it, use the general
             one.  */
	  if (code == -1)
	    {
	      if (sc >= (sizeof (ibmpc_translate_map) / sizeof (short)))
		continue;
	      if ((code = ibmpc_translate_map[sc]) == Ignore)
		continue;
	    }
	}

      if (c == 0)
	{
        /* We only look at the keyboard Ctrl/Shift/Alt keys when
           Emacs is ready to read a key.  Therefore, if they press
           `Alt-x' when Emacs is busy, by the time we get to
           `dos_get_modifiers', they might have already released the
           Alt key, and Emacs gets just `x', which is BAD.
           However, for keys with the `Map' property set, the ASCII
           code returns zero only if Alt is pressed.  So, when we DON'T
           have to support international_keyboard, we don't have to
           distinguish between the left and  right Alt keys, and we
           can set the META modifier for any keys with the `Map'
           property if they return zero ASCII code (c = 0).  */
        if ( (code & Alt)
             || ( (code & 0xf000) == Map && !international_keyboard))
	    modifiers |= meta_modifier;
	  if (code & Ctrl)
	    modifiers |= ctrl_modifier;
	  if (code & Shift)
	    modifiers |= shift_modifier;
	}

      switch (code & 0xf000)
	{
	case ModFct:
	  if (c && !(mask & (SHIFT_P | ALT_P | CTRL_P | HYPER_P | SUPER_P)))
	    return c;
	  c = 0;		/* Special */

	case FctKey:
	  if (c != 0)
	    return c;

	case Special:
	  code |= 0xff00;
	  break;

	case Normal:
	  if (sc == 0)
	    {
	      if (c == 0)	/* ctrl-break */
		continue;
	      return c;		/* ALT-nnn */
	    }
	  if (!keyboard_map_all)
	    {
	      if (c != ' ')
		return c;
	      code = c;
	      break;
	    }

	case Map:
	  if (c && !(mask & ALT_P) && !((mask & SHIFT_P) && (mask & CTRL_P)))
	    if (!keyboard_map_all)
	      return c;

	  code &= 0xff;
	  if (mask & ALT_P && code <= 10 && code > 0 && dos_keypad_mode & 0x200)
	    mask |= SHIFT_P;	/* ALT-1 => M-! etc. */

	  if (mask & SHIFT_P)
	    {
	      code = keyboard->shifted[code];
	      mask -= SHIFT_P;
	      modifiers &= ~shift_modifier;
	    }
	  else
	    if ((mask & ALT_GR_P) && keyboard->alt_gr && keyboard->alt_gr[code] != ' ')
	      code = keyboard->alt_gr[code];
	    else
	      code = keyboard->unshifted[code];
	  break;

	case KeyPad:
	  code &= 0xff;
	  if (c == 0xe0)	/* edit key */
	    kp_mode = 3;
	  else
	    if ((mask & (NUMLOCK_P|CTRL_P|SHIFT_P|ALT_P)) == NUMLOCK_P) /* numlock on */
	      kp_mode = dos_keypad_mode & 0x03;
	    else
	      kp_mode = (dos_keypad_mode >> 4) & 0x03;

	  switch (kp_mode)
	    {
	    case 0:
	      if (code == 10 && dos_decimal_point)
		return dos_decimal_point;
	      return keypad_translate_map[code].char_code;

	    case 1:
	      code = 0xff00 | keypad_translate_map[code].keypad_code;
	      break;

	    case 2:
	      code = keypad_translate_map[code].meta_code;
	      modifiers = meta_modifier;
	      break;

	    case 3:
	      code = 0xff00 | keypad_translate_map[code].editkey_code;
	      break;
	    }
	  break;

	case Grey:
	  code &= 0xff;
	  kp_mode = ((mask & (NUMLOCK_P|CTRL_P|SHIFT_P|ALT_P)) == NUMLOCK_P) ? 0x04 : 0x40;
	  if (dos_keypad_mode & kp_mode)
	    code = 0xff00 | grey_key_translate_map[code].keypad_code;
	  else
	    code = grey_key_translate_map[code].char_code;
	  break;
	}

      if (code == 0)
	continue;

      if (!hlinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight))
	{
	  clear_mouse_face (hlinfo);
	  hlinfo->mouse_face_hidden = 1;
	}

      if (code >= 0x100)
	event.kind = NON_ASCII_KEYSTROKE_EVENT;
      else
	event.kind = ASCII_KEYSTROKE_EVENT;
      event.code = code;
      event.modifiers =	modifiers;
      event.frame_or_window = selected_frame;
      event.arg = Qnil;
      event.timestamp = event_timestamp ();
      kbd_buffer_store_event (&event);
    }

  if (have_mouse > 0 && !mouse_preempted)
    {
      int but, press, x, y, ok;
      int mouse_prev_x = mouse_last_x, mouse_prev_y = mouse_last_y;
      Lisp_Object mouse_window = Qnil;

      /* Check for mouse movement *before* buttons.  */
      mouse_check_moved ();

      /* If the mouse moved from the spot of its last sighting, we
         might need to update mouse highlight.  */
      if (mouse_last_x != mouse_prev_x || mouse_last_y != mouse_prev_y)
	{
	  if (hlinfo->mouse_face_hidden)
	    {
	      hlinfo->mouse_face_hidden = 0;
	      clear_mouse_face (hlinfo);
	    }

	  /* Generate SELECT_WINDOW_EVENTs when needed.  */
	  if (!NILP (Vmouse_autoselect_window))
	    {
	      mouse_window = window_from_coordinates (SELECTED_FRAME (),
						      mouse_last_x,
						      mouse_last_y,
						      0, 0);
	      /* A window will be selected only when it is not
		 selected now, and the last mouse movement event was
		 not in it.  A minibuffer window will be selected iff
		 it is active.  */
	      if (WINDOWP (mouse_window)
		  && !EQ (mouse_window, last_mouse_window)
		  && !EQ (mouse_window, selected_window))
		{
		  event.kind = SELECT_WINDOW_EVENT;
		  event.frame_or_window = mouse_window;
		  event.arg = Qnil;
		  event.timestamp = event_timestamp ();
		  kbd_buffer_store_event (&event);
		}
	      last_mouse_window = mouse_window;
	    }
	  else
	    last_mouse_window = Qnil;

	  previous_help_echo_string = help_echo_string;
	  help_echo_string = help_echo_object = help_echo_window = Qnil;
	  help_echo_pos = -1;
	  note_mouse_highlight (SELECTED_FRAME (), mouse_last_x, mouse_last_y);
	  /* If the contents of the global variable help_echo has
	     changed, generate a HELP_EVENT.  */
	  if (!NILP (help_echo_string) || !NILP (previous_help_echo_string))
	    gen_help_event (help_echo_string, selected_frame, help_echo_window,
			    help_echo_object, help_echo_pos);
	}

      for (but = 0; but < NUM_MOUSE_BUTTONS; but++)
	for (press = 0; press < 2; press++)
	  {
	    int button_num = but;

	    if (press)
	      ok = mouse_pressed (but, &x, &y);
	    else
	      ok = mouse_released (but, &x, &y);
	    if (ok)
	      {
		/* Allow a simultaneous press/release of Mouse-1 and
		   Mouse-2 to simulate Mouse-3 on two-button mice.  */
		if (mouse_button_count == 2 && but < 2)
		  {
		    int x2, y2;	/* don't clobber original coordinates */

		    /* If only one button is pressed, wait 100 msec and
		       check again.  This way, Speedy Gonzales isn't
		       punished, while the slow get their chance.  */
		    if ((press && mouse_pressed (1-but, &x2, &y2))
			|| (!press && mouse_released (1-but, &x2, &y2)))
		      button_num = 2;
		    else
		      {
			delay (100);
			if ((press && mouse_pressed (1-but, &x2, &y2))
			    || (!press && mouse_released (1-but, &x2, &y2)))
			  button_num = 2;
		      }
		  }

		event.kind = MOUSE_CLICK_EVENT;
		event.code = button_num;
		event.modifiers = dos_get_modifiers (0)
		  | (press ? down_modifier : up_modifier);
		event.x = make_number (x);
		event.y = make_number (y);
		event.frame_or_window = selected_frame;
		event.arg = Qnil;
		event.timestamp = event_timestamp ();
		kbd_buffer_store_event (&event);
	      }
	  }
    }

  return -1;
}

static int prev_get_char = -1;

/* Return 1 if a key is ready to be read without suspending execution.  */
int
dos_keysns (void)
{
  if (prev_get_char != -1)
    return 1;
  else
    return ((prev_get_char = dos_rawgetc ()) != -1);
}

/* Read a key.  Return -1 if no key is ready.  */
int
dos_keyread (void)
{
  if (prev_get_char != -1)
    {
      int c = prev_get_char;
      prev_get_char = -1;
      return c;
    }
  else
    return dos_rawgetc ();
}

#ifndef HAVE_X_WINDOWS

/* Simulation of X's menus.  Nothing too fancy here -- just make it work
   for now.

   Actually, I don't know the meaning of all the parameters of the functions
   here -- I only know how they are called by xmenu.c.  I could of course
   grab the nearest Xlib manual (down the hall, second-to-last door on the
   left), but I don't think it's worth the effort.  */

/* These hold text of the current and the previous menu help messages.  */
static const char *menu_help_message, *prev_menu_help_message;
/* Pane number and item number of the menu item which generated the
   last menu help message.  */
static int menu_help_paneno, menu_help_itemno;

static XMenu *
IT_menu_create (void)
{
  XMenu *menu;

  menu = (XMenu *) xmalloc (sizeof (XMenu));
  menu->allocated = menu->count = menu->panecount = menu->width = 0;
  return menu;
}

/* Allocate some (more) memory for MENU ensuring that there is room for one
   for item.  */

static void
IT_menu_make_room (XMenu *menu)
{
  if (menu->allocated == 0)
    {
      int count = menu->allocated = 10;
      menu->text = (char **) xmalloc (count * sizeof (char *));
      menu->submenu = (XMenu **) xmalloc (count * sizeof (XMenu *));
      menu->panenumber = (int *) xmalloc (count * sizeof (int));
      menu->help_text = (const char **) xmalloc (count * sizeof (char *));
    }
  else if (menu->allocated == menu->count)
    {
      int count = menu->allocated = menu->allocated + 10;
      menu->text
	= (char **) xrealloc (menu->text, count * sizeof (char *));
      menu->submenu
	= (XMenu **) xrealloc (menu->submenu, count * sizeof (XMenu *));
      menu->panenumber
	= (int *) xrealloc (menu->panenumber, count * sizeof (int));
      menu->help_text
	= (const char **) xrealloc (menu->help_text, count * sizeof (char *));
    }
}

/* Search the given menu structure for a given pane number.  */

static XMenu *
IT_menu_search_pane (XMenu *menu, int pane)
{
  int i;
  XMenu *try;

  for (i = 0; i < menu->count; i++)
    if (menu->submenu[i])
      {
	if (pane == menu->panenumber[i])
	  return menu->submenu[i];
	if ((try = IT_menu_search_pane (menu->submenu[i], pane)))
	  return try;
      }
  return (XMenu *) 0;
}

/* Determine how much screen space a given menu needs.  */

static void
IT_menu_calc_size (XMenu *menu, int *width, int *height)
{
  int i, h2, w2, maxsubwidth, maxheight;

  maxsubwidth = 0;
  maxheight = menu->count;
  for (i = 0; i < menu->count; i++)
    {
      if (menu->submenu[i])
	{
	  IT_menu_calc_size (menu->submenu[i], &w2, &h2);
	  if (w2 > maxsubwidth) maxsubwidth = w2;
	  if (i + h2 > maxheight) maxheight = i + h2;
	}
    }
  *width = menu->width + maxsubwidth;
  *height = maxheight;
}

/* Display MENU at (X,Y) using FACES.  */

#define BUILD_CHAR_GLYPH(GLYPH, CODE, FACE_ID, PADDING_P)  \
  do							   \
    {							   \
      (GLYPH).type = CHAR_GLYPH;			   \
      SET_CHAR_GLYPH ((GLYPH), CODE, FACE_ID, PADDING_P);  \
      (GLYPH).charpos = -1;				   \
    }							   \
  while (0)

static void
IT_menu_display (XMenu *menu, int y, int x, int pn, int *faces, int disp_help)
{
  int i, j, face, width,  mx, my, enabled, mousehere, row, col;
  struct glyph *text, *p;
  const unsigned char *q;
  struct frame *sf = SELECTED_FRAME ();

  menu_help_message = NULL;

  width = menu->width;
  /* We multiply width by 2 to account for possible control characters.
     FIXME: cater to non-ASCII characters in menus.  */
  text = (struct glyph *) xmalloc ((width * 2 + 2) * sizeof (struct glyph));
  ScreenGetCursor (&row, &col);
  mouse_get_xy (&mx, &my);
  IT_update_begin (sf);
  for (i = 0; i < menu->count; i++)
    {
      int max_width = width + 2;

      IT_cursor_to (sf, y + i, x);
      enabled
	= (!menu->submenu[i] && menu->panenumber[i]) || (menu->submenu[i]);
      mousehere = (y + i == my && x <= mx && mx < x + max_width);
      face = faces[enabled + mousehere * 2];
      /* The following if clause means that we display the menu help
	 strings even if the menu item is currently disabled.  */
      if (disp_help && enabled + mousehere * 2 >= 2)
	{
	  menu_help_message = menu->help_text[i];
	  menu_help_paneno = pn - 1;
	  menu_help_itemno = i;
	}
      p = text;
      BUILD_CHAR_GLYPH (*p, ' ', face, 0);
      p++;
      for (j = 0, q = menu->text[i]; *q; j++)
	{
	  unsigned c = STRING_CHAR_ADVANCE (q);

	  if (c > 26)
	    {
	      BUILD_CHAR_GLYPH (*p, c, face, 0);
	      p++;
	    }
	  else	/* make '^x' */
	    {
	      BUILD_CHAR_GLYPH (*p, '^', face, 0);
	      p++;
	      j++;
	      BUILD_CHAR_GLYPH (*p, c + 64, face, 0);
	      p++;
	    }
	}
      /* Don't let the menu text overflow into the next screen row.  */
      if (x + max_width > screen_size_X)
	{
	  max_width = screen_size_X - x;
	  text[max_width - 1].u.ch = '$'; /* indicate it's truncated */
	}
      for (; j < max_width - 2; j++, p++)
	BUILD_CHAR_GLYPH (*p, ' ', face, 0);

      /* 16 is the character code of a character that on DOS terminal
	 produces a nice-looking right-pointing arrow glyph.  */
      BUILD_CHAR_GLYPH (*p, menu->submenu[i] ? 16 : ' ', face, 0);
      p++;
      IT_write_glyphs (sf, text, max_width);
    }
  IT_update_end (sf);
  IT_cursor_to (sf, row, col);
  xfree (text);
}

/* --------------------------- X Menu emulation ---------------------- */

/* Report availability of menus.  */

int
have_menus_p (void) {  return 1; }

/* Create a brand new menu structure.  */

XMenu *
XMenuCreate (Display *foo1, Window foo2, char *foo3)
{
  return IT_menu_create ();
}

/* Create a new pane and place it on the outer-most level.  It is not
   clear that it should be placed out there, but I don't know what else
   to do.  */

int
XMenuAddPane (Display *foo, XMenu *menu, const char *txt, int enable)
{
  int len;
  const char *p;

  if (!enable)
    abort ();

  IT_menu_make_room (menu);
  menu->submenu[menu->count] = IT_menu_create ();
  menu->text[menu->count] = (char *)txt;
  menu->panenumber[menu->count] = ++menu->panecount;
  menu->help_text[menu->count] = NULL;
  menu->count++;

  /* Adjust length for possible control characters (which will
     be written as ^x).  */
  for (len = strlen (txt), p = txt; *p; p++)
    if (*p < 27)
      len++;

  if (len > menu->width)
    menu->width = len;

  return menu->panecount;
}

/* Create a new item in a menu pane.  */

int
XMenuAddSelection (Display *bar, XMenu *menu, int pane,
		   int foo, char *txt, int enable, char const *help_text)
{
  int len;
  char *p;

  if (pane)
    if (!(menu = IT_menu_search_pane (menu, pane)))
      return XM_FAILURE;
  IT_menu_make_room (menu);
  menu->submenu[menu->count] = (XMenu *) 0;
  menu->text[menu->count] = txt;
  menu->panenumber[menu->count] = enable;
  menu->help_text[menu->count] = help_text;
  menu->count++;

  /* Adjust length for possible control characters (which will
     be written as ^x).  */
  for (len = strlen (txt), p = txt; *p; p++)
    if (*p < 27)
      len++;

  if (len > menu->width)
    menu->width = len;

  return XM_SUCCESS;
}

/* Decide where the menu would be placed if requested at (X,Y).  */

void
XMenuLocate (Display *foo0, XMenu *menu, int foo1, int foo2, int x, int y,
	     int *ulx, int *uly, int *width, int *height)
{
  IT_menu_calc_size (menu, width, height);
  *ulx = x + 1;
  *uly = y;
  *width += 2;
}

struct IT_menu_state
{
  void *screen_behind;
  XMenu *menu;
  int pane;
  int x, y;
};


/* Display menu, wait for user's response, and return that response.  */

int
XMenuActivate (Display *foo, XMenu *menu, int *pane, int *selidx,
	       int x0, int y0, unsigned ButtonMask, char **txt,
	       void (*help_callback)(char const *, int, int))
{
  struct IT_menu_state *state;
  int statecount, x, y, i, b, screensize, leave, result, onepane;
  int title_faces[4];		/* face to display the menu title */
  int faces[4], buffers_num_deleted = 0;
  struct frame *sf = SELECTED_FRAME ();
  Lisp_Object saved_echo_area_message, selectface;

  /* Just in case we got here without a mouse present...  */
  if (have_mouse <= 0)
    return XM_IA_SELECT;
  /* Don't allow non-positive x0 and y0, lest the menu will wrap
     around the display.  */
  if (x0 <= 0)
    x0 = 1;
  if (y0 <= 0)
    y0 = 1;

  /* We will process all the mouse events directly, so we had
     better prevent dos_rawgetc from stealing them from us.  */
  mouse_preempted++;

  state = alloca (menu->panecount * sizeof (struct IT_menu_state));
  screensize = screen_size * 2;
  faces[0]
    = lookup_derived_face (sf, intern ("msdos-menu-passive-face"),
			   DEFAULT_FACE_ID, 1);
  faces[1]
    = lookup_derived_face (sf, intern ("msdos-menu-active-face"),
			   DEFAULT_FACE_ID, 1);
  selectface = intern ("msdos-menu-select-face");
  faces[2] = lookup_derived_face (sf, selectface,
				  faces[0], 1);
  faces[3] = lookup_derived_face (sf, selectface,
				  faces[1], 1);

  /* Make sure the menu title is always displayed with
     `msdos-menu-active-face', no matter where the mouse pointer is.  */
  for (i = 0; i < 4; i++)
    title_faces[i] = faces[3];

  statecount = 1;

  /* Don't let the title for the "Buffers" popup menu include a
     digit (which is ugly).

     This is a terrible kludge, but I think the "Buffers" case is
     the only one where the title includes a number, so it doesn't
     seem to be necessary to make this more general.  */
  if (strncmp (menu->text[0], "Buffers 1", 9) == 0)
    {
      menu->text[0][7] = '\0';
      buffers_num_deleted = 1;
    }

  /* We need to save the current echo area message, so that we could
     restore it below, before we exit.  See the commentary below,
     before the call to message_with_string.  */
  saved_echo_area_message = Fcurrent_message ();
  state[0].menu = menu;
  mouse_off ();
  ScreenRetrieve (state[0].screen_behind = xmalloc (screensize));

  /* Turn off the cursor.  Otherwise it shows through the menu
     panes, which is ugly.  */
  IT_display_cursor (0);

  /* Display the menu title.  */
  IT_menu_display (menu, y0 - 1, x0 - 1, 1, title_faces, 0);
  if (buffers_num_deleted)
    menu->text[0][7] = ' ';
  if ((onepane = menu->count == 1 && menu->submenu[0]))
    {
      menu->width = menu->submenu[0]->width;
      state[0].menu = menu->submenu[0];
    }
  else
    {
      state[0].menu = menu;
    }
  state[0].x = x0 - 1;
  state[0].y = y0;
  state[0].pane = onepane;

  mouse_last_x = -1;  /* A hack that forces display.  */
  leave = 0;
  while (!leave)
    {
      if (!mouse_visible) mouse_on ();
      mouse_check_moved ();
      if (sf->mouse_moved)
	{
	  sf->mouse_moved = 0;
	  result = XM_IA_SELECT;
	  mouse_get_xy (&x, &y);
	  for (i = 0; i < statecount; i++)
	    if (state[i].x <= x && x < state[i].x + state[i].menu->width + 2)
	      {
		int dy = y - state[i].y;
		if (0 <= dy && dy < state[i].menu->count)
		  {
		    if (!state[i].menu->submenu[dy])
		      {
			if (state[i].menu->panenumber[dy])
			  result = XM_SUCCESS;
			else
			  result = XM_IA_SELECT;
		      }
		    *pane = state[i].pane - 1;
		    *selidx = dy;
		    /* We hit some part of a menu, so drop extra menus that
		       have been opened.  That does not include an open and
		       active submenu.  */
		    if (i != statecount - 2
			|| state[i].menu->submenu[dy] != state[i+1].menu)
		      while (i != statecount - 1)
			{
			  statecount--;
			  mouse_off ();
			  ScreenUpdate (state[statecount].screen_behind);
			  if (screen_virtual_segment)
			    dosv_refresh_virtual_screen (0, screen_size);
			  xfree (state[statecount].screen_behind);
			}
		    if (i == statecount - 1 && state[i].menu->submenu[dy])
		      {
			IT_menu_display (state[i].menu,
					 state[i].y,
					 state[i].x,
					 state[i].pane,
					 faces, 1);
			state[statecount].menu = state[i].menu->submenu[dy];
			state[statecount].pane = state[i].menu->panenumber[dy];
			mouse_off ();
			ScreenRetrieve (state[statecount].screen_behind
					= xmalloc (screensize));
			state[statecount].x
			  = state[i].x + state[i].menu->width + 2;
			state[statecount].y = y;
			statecount++;
		      }
		  }
	      }
	  IT_menu_display (state[statecount - 1].menu,
			   state[statecount - 1].y,
			   state[statecount - 1].x,
			   state[statecount - 1].pane,
			   faces, 1);
	}
      else
	{
	  if ((menu_help_message || prev_menu_help_message)
	      && menu_help_message != prev_menu_help_message)
	    {
	      help_callback (menu_help_message,
			     menu_help_paneno, menu_help_itemno);
	      IT_display_cursor (0);
	      prev_menu_help_message = menu_help_message;
	    }
	  /* We are busy-waiting for the mouse to move, so let's be nice
	     to other Windows applications by releasing our time slice.  */
	  __dpmi_yield ();
	}
      for (b = 0; b < mouse_button_count && !leave; b++)
	{
	  /* Only leave if user both pressed and released the mouse, and in
	     that order.  This avoids popping down the menu pane unless
	     the user is really done with it.  */
	  if (mouse_pressed (b, &x, &y))
	    {
	      while (mouse_button_depressed (b, &x, &y))
		__dpmi_yield ();
	      leave = 1;
	    }
	  (void) mouse_released (b, &x, &y);
	}
    }

  mouse_off ();
  ScreenUpdate (state[0].screen_behind);
  if (screen_virtual_segment)
    dosv_refresh_virtual_screen (0, screen_size);

  /* We have a situation here.  ScreenUpdate has just restored the
     screen contents as it was before we started drawing this menu.
     That includes any echo area message that could have been
     displayed back then.  (In reality, that echo area message will
     almost always be the ``keystroke echo'' that echoes the sequence
     of menu items chosen by the user.)  However, if the menu had some
     help messages, then displaying those messages caused Emacs to
     forget about the original echo area message.  So when
     ScreenUpdate restored it, it created a discrepancy between the
     actual screen contents and what Emacs internal data structures
     know about it.

     To avoid this conflict, we force Emacs to restore the original
     echo area message as we found it when we entered this function.
     The irony of this is that we then erase the restored message
     right away, so the only purpose of restoring it is so that
     erasing it works correctly...  */
  if (! NILP (saved_echo_area_message))
    message_with_string ("%s", saved_echo_area_message, 0);
  message (0);
  while (statecount--)
    xfree (state[statecount].screen_behind);
  IT_display_cursor (1);	/* turn cursor back on */
  /* Clean up any mouse events that are waiting inside Emacs event queue.
     These events are likely to be generated before the menu was even
     displayed, probably because the user pressed and released the button
     (which invoked the menu) too quickly.  If we don't remove these events,
     Emacs will process them after we return and surprise the user.  */
  discard_mouse_events ();
  mouse_clear_clicks ();
  if (!kbd_buffer_events_waiting (1))
    clear_input_pending ();
  /* Allow mouse events generation by dos_rawgetc.  */
  mouse_preempted--;
  return result;
}

/* Dispose of a menu.  */

void
XMenuDestroy (Display *foo, XMenu *menu)
{
  int i;
  if (menu->allocated)
    {
      for (i = 0; i < menu->count; i++)
	if (menu->submenu[i])
	  XMenuDestroy (foo, menu->submenu[i]);
      xfree (menu->text);
      xfree (menu->submenu);
      xfree (menu->panenumber);
      xfree (menu->help_text);
    }
  xfree (menu);
  menu_help_message = prev_menu_help_message = NULL;
}

int
x_pixel_width (struct frame *f)
{
  return FRAME_COLS (f);
}

int
x_pixel_height (struct frame *f)
{
  return FRAME_LINES (f);
}
#endif /* !HAVE_X_WINDOWS */

/* ----------------------- DOS / UNIX conversion --------------------- */

void msdos_downcase_filename (unsigned char *);

/* Destructively turn backslashes into slashes.  */

void
dostounix_filename (char *p)
{
  msdos_downcase_filename (p);

  while (*p)
    {
      if (*p == '\\')
	*p = '/';
      p++;
    }
}

/* Destructively turn slashes into backslashes.  */

void
unixtodos_filename (char *p)
{
  if (p[1] == ':' && *p >= 'A' && *p <= 'Z')
    {
      *p += 'a' - 'A';
      p += 2;
    }

  while (*p)
    {
      if (*p == '/')
	*p = '\\';
      p++;
    }
}

/* Get the default directory for a given drive.  0=def, 1=A, 2=B, ...  */

int
getdefdir (int drive, char *dst)
{
  char in_path[4], *p = in_path, e = errno;

  /* Generate "X:." (when drive is X) or "." (when drive is 0).  */
  if (drive != 0)
    {
      *p++ = drive + 'A' - 1;
      *p++ = ':';
    }

  *p++ = '.';
  *p = '\0';
  errno = 0;
  _fixpath (in_path, dst);
    /* _fixpath can set errno to ENOSYS on non-LFN systems because
       it queries the LFN support, so ignore that error.  */
  if ((errno && errno != ENOSYS) || *dst == '\0')
    return 0;

  msdos_downcase_filename (dst);

  errno = e;
  return 1;
}

char *
emacs_root_dir (void)
{
  static char root_dir[4];

  sprintf (root_dir, "%c:/", 'A' + getdisk ());
  root_dir[0] = tolower (root_dir[0]);
  return root_dir;
}

/* Remove all CR's that are followed by a LF.  */

int
crlf_to_lf (int n, unsigned char *buf)
{
  unsigned char *np = buf, *startp = buf, *endp = buf + n;

  if (n == 0)
    return n;
  while (buf < endp - 1)
    {
      if (*buf == 0x0d)
	{
	  if (*(++buf) != 0x0a)
	    *np++ = 0x0d;
	}
      else
	*np++ = *buf++;
    }
  if (buf < endp)
    *np++ = *buf++;
  return np - startp;
}

DEFUN ("msdos-long-file-names", Fmsdos_long_file_names, Smsdos_long_file_names,
       0, 0, 0,
       doc: /* Return non-nil if long file names are supported on MS-DOS.  */)
  (void)
{
  return (_USE_LFN ? Qt : Qnil);
}

/* Convert alphabetic characters in a filename to lower-case.  */

void
msdos_downcase_filename (unsigned char *p)
{
  /* Always lower-case drive letters a-z, even if the filesystem
     preserves case in filenames.
     This is so MSDOS filenames could be compared by string comparison
     functions that are case-sensitive.  Even case-preserving filesystems
     do not distinguish case in drive letters.  */
  if (p[1] == ':' && *p >= 'A' && *p <= 'Z')
    {
      *p += 'a' - 'A';
      p += 2;
    }

  /* Under LFN we expect to get pathnames in their true case.  */
  if (NILP (Fmsdos_long_file_names ()))
    for ( ; *p; p++)
      if (*p >= 'A' && *p <= 'Z')
	*p += 'a' - 'A';
}

DEFUN ("msdos-downcase-filename", Fmsdos_downcase_filename, Smsdos_downcase_filename,
       1, 1, 0,
       doc: /* Convert alphabetic characters in FILENAME to lower case and return that.
When long filenames are supported, doesn't change FILENAME.
If FILENAME is not a string, returns nil.
The argument object is never altered--the value is a copy.  */)
  (Lisp_Object filename)
{
  Lisp_Object tem;

  if (! STRINGP (filename))
    return Qnil;

  tem = Fcopy_sequence (filename);
  msdos_downcase_filename (SDATA (tem));
  return tem;
}

/* The Emacs root directory as determined by init_environment.  */

static char emacsroot[MAXPATHLEN];

char *
rootrelativepath (char *rel)
{
  static char result[MAXPATHLEN + 10];

  strcpy (result, emacsroot);
  strcat (result, "/");
  strcat (result, rel);
  return result;
}

/* Define a lot of environment variables if not already defined.  Don't
   remove anything unless you know what you're doing -- lots of code will
   break if one or more of these are missing.  */

void
init_environment (int argc, char **argv, int skip_args)
{
  char *s, *t, *root;
  int len, i;
  static const char * const tempdirs[] = {
    "$TMPDIR", "$TEMP", "$TMP", "c:/"
  };
  const int imax = sizeof (tempdirs) / sizeof (tempdirs[0]);

  /* Make sure they have a usable $TMPDIR.  Many Emacs functions use
     temporary files and assume "/tmp" if $TMPDIR is unset, which
     will break on DOS/Windows.  Refuse to work if we cannot find
     a directory, not even "c:/", usable for that purpose.  */
  for (i = 0; i < imax ; i++)
    {
      const char *tmp = tempdirs[i];
      char buf[FILENAME_MAX];

      if (*tmp == '$')
	{
	  int tmp_len;

	  tmp = getenv (tmp + 1);
	  if (!tmp)
	    continue;

	  /* Some lusers set TMPDIR=e:, probably because some losing
	     programs cannot handle multiple slashes if they use e:/.
	     e: fails in `access' below, so we interpret e: as e:/.  */
	  tmp_len = strlen (tmp);
	  if (tmp[tmp_len - 1] != '/' && tmp[tmp_len - 1] != '\\')
	    {
	      strcpy (buf, tmp);
	      buf[tmp_len++] = '/', buf[tmp_len] = 0;
	      tmp = buf;
	    }
	}

      /* Note that `access' can lie to us if the directory resides on a
	 read-only filesystem, like CD-ROM or a write-protected floppy.
	 The only way to be really sure is to actually create a file and
	 see if it succeeds.  But I think that's too much to ask.  */
      if (tmp && access (tmp, D_OK) == 0)
	{
	  setenv ("TMPDIR", tmp, 1);
	  break;
	}
    }
  if (i >= imax)
    cmd_error_internal
      (Fcons (Qerror,
	      Fcons (build_string ("no usable temporary directories found!!"),
		     Qnil)),
       "While setting TMPDIR: ");

  /* Note the startup time, so we know not to clear the screen if we
     exit immediately; see IT_reset_terminal_modes.
     (Yes, I know `clock' returns zero the first time it's called, but
     I do this anyway, in case some wiseguy changes that at some point.)  */
  startup_time = clock ();

  /* Find our root from argv[0].  Assuming argv[0] is, say,
     "c:/emacs/bin/emacs.exe" our root will be "c:/emacs".  */
  root = alloca (MAXPATHLEN + 20);
  _fixpath (argv[0], root);
  msdos_downcase_filename (root);
  len = strlen (root);
  while (len > 0 && root[len] != '/' && root[len] != ':')
    len--;
  root[len] = '\0';
  if (len > 4
      && (strcmp (root + len - 4, "/bin") == 0
	  || strcmp (root + len - 4, "/src") == 0)) /* under a debugger */
    root[len - 4] = '\0';
  else
    strcpy (root, "c:/emacs");  /* let's be defensive */
  len = strlen (root);
  strcpy (emacsroot, root);

  /* We default HOME to our root.  */
  setenv ("HOME", root, 0);

  /* We default EMACSPATH to root + "/bin".  */
  strcpy (root + len, "/bin");
  setenv ("EMACSPATH", root, 0);

  /* I don't expect anybody to ever use other terminals so the internal
     terminal is the default.  */
  setenv ("TERM", "internal", 0);

#ifdef HAVE_X_WINDOWS
  /* Emacs expects DISPLAY to be set.  */
  setenv ("DISPLAY", "unix:0.0", 0);
#endif

  /* SHELL is a bit tricky -- COMSPEC is the closest we come, but we must
     downcase it and mirror the backslashes.  */
  s = getenv ("COMSPEC");
  if (!s) s = "c:/command.com";
  t = alloca (strlen (s) + 1);
  strcpy (t, s);
  dostounix_filename (t);
  setenv ("SHELL", t, 0);

  /* PATH is also downcased and backslashes mirrored.  */
  s = getenv ("PATH");
  if (!s) s = "";
  t = alloca (strlen (s) + 3);
  /* Current directory is always considered part of MsDos's path but it is
     not normally mentioned.  Now it is.  */
  strcat (strcpy (t, ".;"), s);
  dostounix_filename (t); /* Not a single file name, but this should work.  */
  setenv ("PATH", t, 1);

  /* In some sense all dos users have root privileges, so...  */
  setenv ("USER", "root", 0);
  setenv ("NAME", getenv ("USER"), 0);

  /* Time zone determined from country code.  To make this possible, the
     country code may not span more than one time zone.  In other words,
     in the USA, you lose.  */
  if (!getenv ("TZ"))
    switch (dos_country_code)
      {
      case 31:			/* Belgium */
      case 32:			/* The Netherlands */
      case 33:			/* France */
      case 34:			/* Spain */
      case 36:			/* Hungary */
      case 38:			/* Yugoslavia (or what's left of it?) */
      case 39:			/* Italy */
      case 41:			/* Switzerland */
      case 42:			/* Tjekia */
      case 45:			/* Denmark */
      case 46:			/* Sweden */
      case 47:			/* Norway */
      case 48:			/* Poland */
      case 49:			/* Germany */
	/* Daylight saving from last Sunday in March to last Sunday in
	   September, both at 2AM.  */
	setenv ("TZ", "MET-01METDST-02,M3.5.0/02:00,M9.5.0/02:00", 0);
	break;
      case 44:			/* United Kingdom */
      case 351:			/* Portugal */
      case 354:			/* Iceland */
	setenv ("TZ", "GMT+00", 0);
	break;
      case 81:			/* Japan */
      case 82:			/* Korea */
	setenv ("TZ", "JST-09", 0);
	break;
      case 90:			/* Turkey */
      case 358:			/* Finland */
	setenv ("TZ", "EET-02", 0);
	break;
      case 972:			/* Israel */
	/* This is an approximation.  (For exact rules, use the
	   `zoneinfo/israel' file which comes with DJGPP, but you need
	   to install it in `/usr/share/zoneinfo/' directory first.)  */
	setenv ("TZ", "IST-02IDT-03,M4.1.6/00:00,M9.5.6/01:00", 0);
	break;
      }
  tzset ();
}



static int break_stat;	 /* BREAK check mode status.	*/
static int stdin_stat;	 /* stdin IOCTL status.		*/

/* Turn off Dos' Ctrl-C checking and inhibit interpretation of
   control chars by DOS.   Determine the keyboard type.  */

int
dos_ttraw (struct tty_display_info *tty)
{
  union REGS inregs, outregs;
  static int first_time = 1;

  /* If we are called for the initial terminal, it's too early to do
     anything, and termscript isn't set up.  */
  if (tty->terminal->type == output_initial)
    return 2;

  break_stat = getcbrk ();
  setcbrk (0);

  if (first_time)
    {
      inregs.h.ah = 0xc0;
      int86 (0x15, &inregs, &outregs);
      extended_kbd = (!outregs.x.cflag) && (outregs.h.ah == 0);

      have_mouse = 0;

      if (1
#ifdef HAVE_X_WINDOWS
	  && inhibit_window_system
#endif
	  )
	{
	  inregs.x.ax = 0x0021;
	  int86 (0x33, &inregs, &outregs);
	  have_mouse = (outregs.x.ax & 0xffff) == 0xffff;
	  if (!have_mouse)
	    {
	      /* Reportedly, the above doesn't work for some mouse drivers.  There
		 is an additional detection method that should work, but might be
		 a little slower.  Use that as an alternative.  */
	      inregs.x.ax = 0x0000;
	      int86 (0x33, &inregs, &outregs);
	      have_mouse = (outregs.x.ax & 0xffff) == 0xffff;
	    }
	  if (have_mouse)
	    mouse_button_count = outregs.x.bx;

#ifndef HAVE_X_WINDOWS
	  /* Save the cursor shape used outside Emacs.  */
	  outside_cursor = _farpeekw (_dos_ds, 0x460);
#endif
	}

      first_time = 0;

      stdin_stat = setmode (fileno (stdin), O_BINARY);
      return (stdin_stat != -1);
    }
  else
    return (setmode (fileno (stdin), O_BINARY) != -1);
}

/*  Restore status of standard input and Ctrl-C checking.  */

int
dos_ttcooked (void)
{
  union REGS inregs, outregs;

  setcbrk (break_stat);
  mouse_off ();

#ifndef HAVE_X_WINDOWS
  /* Restore the cursor shape we found on startup.  */
  if (outside_cursor)
    {
      inregs.h.ah = 1;
      inregs.x.cx = outside_cursor;
      int86 (0x10, &inregs, &outregs);
    }
#endif

  return (setmode (fileno (stdin), stdin_stat) != -1);
}


/* Run command as specified by ARGV in directory DIR.
   The command is run with input from TEMPIN, output to
   file TEMPOUT and stderr to TEMPERR.  */

int
run_msdos_command (unsigned char **argv, const char *working_dir,
		   int tempin, int tempout, int temperr, char **envv)
{
  char *saveargv1, *saveargv2, *lowcase_argv0, *pa, *pl;
  char oldwd[MAXPATHLEN + 1]; /* Fixed size is safe on MSDOS.  */
  int msshell, result = -1, inbak, outbak, errbak, x, y;
  Lisp_Object cmd;

  /* Get current directory as MSDOS cwd is not per-process.  */
  getwd (oldwd);

  /* If argv[0] is the shell, it might come in any lettercase.
     Since `Fmember' is case-sensitive, we need to downcase
     argv[0], even if we are on case-preserving filesystems.  */
  lowcase_argv0 = alloca (strlen (argv[0]) + 1);
  for (pa = argv[0], pl = lowcase_argv0; *pa; pl++)
    {
      *pl = *pa++;
      if (*pl >= 'A' && *pl <= 'Z')
	*pl += 'a' - 'A';
    }
  *pl = '\0';

  cmd = Ffile_name_nondirectory (build_string (lowcase_argv0));
  msshell = !NILP (Fmember (cmd, Fsymbol_value (intern ("msdos-shells"))))
    && !strcmp ("-c", argv[1]);
  if (msshell)
    {
      saveargv1 = argv[1];
      saveargv2 = argv[2];
      argv[1] = "/c";
      /* We only need to mirror slashes if a DOS shell will be invoked
	 not via `system' (which does the mirroring itself).  Yes, that
	 means DJGPP v1.x will lose here.  */
      if (argv[2] && argv[3])
	{
	  char *p = alloca (strlen (argv[2]) + 1);

	  strcpy (argv[2] = p, saveargv2);
	  while (*p && isspace (*p))
	    p++;
	  while (*p)
	    {
	      if (*p == '/')
		*p++ = '\\';
	      else
		p++;
	    }
	}
    }

  chdir (working_dir);
  inbak = dup (0);
  outbak = dup (1);
  errbak = dup (2);
  if (inbak < 0 || outbak < 0 || errbak < 0)
    goto done; /* Allocation might fail due to lack of descriptors.  */

  if (have_mouse > 0)
    mouse_get_xy (&x, &y);

  if (!noninteractive)
    dos_ttcooked ();	/* do it here while 0 = stdin */

  dup2 (tempin, 0);
  dup2 (tempout, 1);
  dup2 (temperr, 2);

  if (msshell && !argv[3])
    {
      /* MS-DOS native shells are too restrictive.  For starters, they
	 cannot grok commands longer than 126 characters.  In DJGPP v2
	 and later, `system' is much smarter, so we'll call it instead.  */

      const char *cmnd;

      /* A shell gets a single argument--its full command
	 line--whose original was saved in `saveargv2'.  */

      /* Don't let them pass empty command lines to `system', since
	 with some shells it will try to invoke an interactive shell,
	 which will hang Emacs.  */
      for (cmnd = saveargv2; *cmnd && isspace (*cmnd); cmnd++)
	;
      if (*cmnd)
	{
	  extern char **environ;
	  char **save_env = environ;
	  int save_system_flags = __system_flags;

	  /* Request the most powerful version of `system'.  We need
	     all the help we can get to avoid calling stock DOS shells.  */
	  __system_flags =  (__system_redirect
			     | __system_use_shell
			     | __system_allow_multiple_cmds
			     | __system_allow_long_cmds
			     | __system_handle_null_commands
			     | __system_emulate_chdir);

	  environ = envv;
	  result = system (cmnd);
	  __system_flags = save_system_flags;
	  environ = save_env;
	}
      else
	result = 0;	/* emulate Unixy shell behavior with empty cmd line */
    }
  else
    result = spawnve (P_WAIT, argv[0], (char **)argv, envv);

  dup2 (inbak, 0);
  dup2 (outbak, 1);
  dup2 (errbak, 2);
  emacs_close (inbak);
  emacs_close (outbak);
  emacs_close (errbak);

  if (!noninteractive)
    dos_ttraw (CURTTY ());
  if (have_mouse > 0)
    {
      mouse_init ();
      mouse_moveto (x, y);
    }

  /* Some programs might change the meaning of the highest bit of the
     text attribute byte, so we get blinking characters instead of the
     bright background colors.  Restore that.  */
  if (!noninteractive)
    bright_bg ();

 done:
  chdir (oldwd);
  if (msshell)
    {
      argv[1] = saveargv1;
      argv[2] = saveargv2;
    }
  return result;
}

void
croak (char *badfunc)
{
  fprintf (stderr, "%s not yet implemented\r\n", badfunc);
  reset_all_sys_modes ();
  exit (1);
}

/*
 * A few unimplemented functions that we silently ignore.
 */
int setpgrp (void) {return 0; }
int setpriority (int x, int y, int z) { return 0; }

#if __DJGPP__ == 2 && __DJGPP_MINOR__ < 4
ssize_t
readlink (const char *name, char *dummy1, size_t dummy2)
{
  /* `access' is much faster than `stat' on MS-DOS.  */
  if (access (name, F_OK) == 0)
    errno = EINVAL;
  return -1;
}
#endif

char *
careadlinkat (int fd, char const *filename,
              char *buffer, size_t buffer_size,
              struct allocator const *alloc,
              ssize_t (*preadlinkat) (int, char const *, char *, size_t))
{
  if (!buffer)
    {
      /* We don't support the fancy auto-allocation feature.  */
      if (!buffer_size)
	errno = ENOSYS;
      else
	errno = EINVAL;
      buffer = NULL;
    }
  else
    {
      ssize_t len = preadlinkat (fd, filename, buffer, buffer_size);

      if (len < 0 || len == buffer_size)
	buffer = NULL;
      else
	buffer[len + 1] = '\0';
    }
  return buffer;
}

ssize_t
careadlinkatcwd (int fd, char const *filename, char *buffer,
                 size_t buffer_size)
{
  (void) fd;
  return readlink (filename, buffer, buffer_size);
}


#if __DJGPP__ == 2 && __DJGPP_MINOR__ < 2

/* Augment DJGPP library POSIX signal functions.  This is needed
   as of DJGPP v2.01, but might be in the library in later releases. */

#include <libc/bss.h>

/* A counter to know when to re-initialize the static sets.  */
static int sigprocmask_count = -1;

/* Which signals are currently blocked (initially none).  */
static sigset_t current_mask;

/* Which signals are pending (initially none).  */
static sigset_t msdos_pending_signals;

/* Previous handlers to restore when the blocked signals are unblocked.  */
typedef void (*sighandler_t)(int);
static sighandler_t prev_handlers[320];

/* A signal handler which just records that a signal occurred
   (it will be raised later, if and when the signal is unblocked).  */
static void
sig_suspender (int signo)
{
  sigaddset (&msdos_pending_signals, signo);
}

int
sigprocmask (int how, const sigset_t *new_set, sigset_t *old_set)
{
  int signo;
  sigset_t new_mask;

  /* If called for the first time, initialize.  */
  if (sigprocmask_count != __bss_count)
    {
      sigprocmask_count = __bss_count;
      sigemptyset (&msdos_pending_signals);
      sigemptyset (&current_mask);
      for (signo = 0; signo < 320; signo++)
	prev_handlers[signo] = SIG_ERR;
    }

  if (old_set)
    *old_set = current_mask;

  if (new_set == 0)
    return 0;

  if (how != SIG_BLOCK && how != SIG_UNBLOCK && how != SIG_SETMASK)
    {
      errno = EINVAL;
      return -1;
    }

  sigemptyset (&new_mask);

  /* DJGPP supports upto 320 signals.  */
  for (signo = 0; signo < 320; signo++)
    {
      if (sigismember (&current_mask, signo))
	sigaddset (&new_mask, signo);
      else if (sigismember (new_set, signo) && how != SIG_UNBLOCK)
	{
	  sigaddset (&new_mask, signo);

	  /* SIGKILL is silently ignored, as on other platforms.  */
	  if (signo != SIGKILL && prev_handlers[signo] == SIG_ERR)
	    prev_handlers[signo] = signal (signo, sig_suspender);
	}
      if ((   how == SIG_UNBLOCK
	      && sigismember (&new_mask, signo)
	      && sigismember (new_set, signo))
	  || (how == SIG_SETMASK
	      && sigismember (&new_mask, signo)
	      && !sigismember (new_set, signo)))
	{
	  sigdelset (&new_mask, signo);
	  if (prev_handlers[signo] != SIG_ERR)
	    {
	      signal (signo, prev_handlers[signo]);
	      prev_handlers[signo] = SIG_ERR;
	    }
	  if (sigismember (&msdos_pending_signals, signo))
	    {
	      sigdelset (&msdos_pending_signals, signo);
	      raise (signo);
	    }
	}
    }
  current_mask = new_mask;
  return 0;
}

#endif /* not __DJGPP_MINOR__ < 2 */

#ifndef HAVE_SELECT
#include "sysselect.h"

#ifndef EMACS_TIME_ZERO_OR_NEG_P
#define EMACS_TIME_ZERO_OR_NEG_P(time)	\
  ((long)(time).tv_sec < 0		\
   || ((time).tv_sec == 0		\
       && (long)(time).tv_usec <= 0))
#endif

/* This yields the rest of the current time slice to the task manager.
   It should be called by any code which knows that it has nothing
   useful to do except idle.

   I don't use __dpmi_yield here, since versions of library before 2.02
   called Int 2Fh/AX=1680h there in a way that would wedge the DOS box
   on some versions of Windows 9X.  */

void
dos_yield_time_slice (void)
{
  _go32_dpmi_registers r;

  r.x.ax = 0x1680;
  r.x.ss = r.x.sp = r.x.flags = 0;
  _go32_dpmi_simulate_int (0x2f, &r);
  if (r.h.al == 0x80)
    errno = ENOSYS;
}

/* Only event queue is checked.  */
/* We don't have to call timer_check here
   because wait_reading_process_output takes care of that.  */
int
sys_select (int nfds, SELECT_TYPE *rfds, SELECT_TYPE *wfds, SELECT_TYPE *efds,
	    EMACS_TIME *timeout)
{
  int check_input;
  struct time t;

  check_input = 0;
  if (rfds)
    {
      check_input = FD_ISSET (0, rfds);
      FD_ZERO (rfds);
    }
  if (wfds)
    FD_ZERO (wfds);
  if (efds)
    FD_ZERO (efds);

  if (nfds != 1)
    abort ();

  /* If we are looking only for the terminal, with no timeout,
     just read it and wait -- that's more efficient.  */
  if (!timeout)
    {
      while (!detect_input_pending ())
	{
	  dos_yield_time_slice ();
	}
    }
  else
    {
      EMACS_TIME clnow, cllast, cldiff;

      gettime (&t);
      EMACS_SET_SECS_USECS (cllast, t.ti_sec, t.ti_hund * 10000L);

      while (!check_input || !detect_input_pending ())
	{
	  gettime (&t);
	  EMACS_SET_SECS_USECS (clnow, t.ti_sec, t.ti_hund * 10000L);
	  EMACS_SUB_TIME (cldiff, clnow, cllast);

	  /* When seconds wrap around, we assume that no more than
	     1 minute passed since last `gettime'.  */
	  if (EMACS_TIME_NEG_P (cldiff))
	    EMACS_SET_SECS (cldiff, EMACS_SECS (cldiff) + 60);
	  EMACS_SUB_TIME (*timeout, *timeout, cldiff);

	  /* Stop when timeout value crosses zero.  */
	  if (EMACS_TIME_ZERO_OR_NEG_P (*timeout))
	    return 0;
	  cllast = clnow;
	  dos_yield_time_slice ();
	}
    }

  FD_SET (0, rfds);
  return 1;
}
#endif

/*
 * Define overlaid functions:
 *
 *	chdir -> sys_chdir
 *	tzset -> init_gettimeofday
 *	abort -> dos_abort
 */

#ifdef chdir
#undef chdir
extern int chdir (const char *);

int
sys_chdir (const char *path)
{
  int len = strlen (path);
  char *tmp = (char *)path;

  if (*tmp && tmp[1] == ':')
    {
      if (getdisk () != tolower (tmp[0]) - 'a')
	setdisk (tolower (tmp[0]) - 'a');
      tmp += 2;	/* strip drive: KFS 1995-07-06 */
      len -= 2;
    }

  if (len > 1 && (tmp[len - 1] == '/'))
    {
      char *tmp1 = (char *) alloca (len + 1);
      strcpy (tmp1, tmp);
      tmp1[len - 1] = 0;
      tmp = tmp1;
    }
  return chdir (tmp);
}
#endif

#ifdef tzset
#undef tzset
extern void tzset (void);

void
init_gettimeofday (void)
{
  time_t ltm, gtm;
  struct tm *lstm;

  tzset ();
  ltm = gtm = time (NULL);
  ltm = mktime (lstm = localtime (&ltm));
  gtm = mktime (gmtime (&gtm));
  time_rec.tm_hour = 99;	/* force gettimeofday to get date */
  time_rec.tm_isdst = lstm->tm_isdst;
  dos_timezone_offset = time_rec.tm_gmtoff = (int)(gtm - ltm) / 60;
}
#endif

#ifdef abort
#undef abort
void
dos_abort (char *file, int line)
{
  char buffer1[200], buffer2[400];
  int i, j;

  sprintf (buffer1, "<EMACS FATAL ERROR IN %s LINE %d>", file, line);
  for (i = j = 0; buffer1[i]; i++) {
    buffer2[j++] = buffer1[i];
    buffer2[j++] = 0x70;
  }
  dosmemput (buffer2, j, (int)ScreenPrimary);
  ScreenSetCursor (2, 0);
  abort ();
}
#else
void
abort (void)
{
  dos_ttcooked ();
  ScreenSetCursor (10, 0);
  cputs ("\r\n\nEmacs aborted!\r\n");
#if __DJGPP__ == 2 && __DJGPP_MINOR__ < 2
  if (screen_virtual_segment)
    dosv_refresh_virtual_screen (2 * 10 * screen_size_X, 4 * screen_size_X);
  /* Generate traceback, so we could tell whodunit.  */
  signal (SIGINT, SIG_DFL);
  __asm__ __volatile__ ("movb $0x1b,%al;call ___djgpp_hw_exception");
#else  /* __DJGPP_MINOR__ >= 2 */
  raise (SIGABRT);
#endif /* __DJGPP_MINOR__ >= 2 */
  exit (2);
}
#endif

void
syms_of_msdos (void)
{
  recent_doskeys = Fmake_vector (make_number (NUM_RECENT_DOSKEYS), Qnil);
  staticpro (&recent_doskeys);

#ifndef HAVE_X_WINDOWS

  /* The following two are from xfns.c:  */
  DEFSYM (Qreverse, "reverse");

  DEFVAR_LISP ("dos-unsupported-char-glyph", Vdos_unsupported_char_glyph,
	       doc: /* *Glyph to display instead of chars not supported by current codepage.
This variable is used only by MS-DOS terminals.  */);
  Vdos_unsupported_char_glyph = make_number ('\177');

#endif

  defsubr (&Srecent_doskeys);
  defsubr (&Smsdos_long_file_names);
  defsubr (&Smsdos_downcase_filename);
  defsubr (&Smsdos_remember_default_colors);
  defsubr (&Smsdos_set_mouse_buttons);
}

#endif /* MSDOS */
