/* Implementation of GUI terminal on the Microsoft W32 API.

Copyright (C) 1989, 1993-2012 Free Software Foundation, Inc.

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
#include <signal.h>
#include <stdio.h>
#include <setjmp.h>
#include "lisp.h"
#include "blockinput.h"
#include "w32term.h"

#include "systty.h"
#include "systime.h"

#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <imm.h>

#include "charset.h"
#include "character.h"
#include "coding.h"
#include "ccl.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "disptab.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "intervals.h"
#include "process.h"
#include "atimer.h"
#include "keymap.h"

#include "w32heap.h"
#include <shellapi.h>

#include "font.h"
#include "w32font.h"

/* Fringe bitmaps.  */

static int max_fringe_bmp = 0;
static HBITMAP *fringe_bmp = 0;

/* Temporary variables for w32_read_socket.  */

static int last_mousemove_x = 0;
static int last_mousemove_y = 0;

/* Define GET_WHEEL_DELTA_WPARAM macro if system headers don't.  */
#ifndef GET_WHEEL_DELTA_WPARAM
#define GET_WHEEL_DELTA_WPARAM(wparam) ((short)HIWORD (wparam))
#endif

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static int any_help_event_p;

/* Last window where we saw the mouse.  Used by mouse-autoselect-window.  */
static Lisp_Object last_window;

extern unsigned int msh_mousewheel;

extern void free_frame_menubar (struct frame *);

extern int w32_codepage_for_font (char *fontname);
extern Cursor w32_load_cursor (LPCTSTR name);

#define x_any_window_to_frame x_window_to_frame
#define x_top_window_to_frame x_window_to_frame


/* This is display since w32 does not support multiple ones.  */
struct w32_display_info one_w32_display_info;
struct w32_display_info *x_display_list;

/* This is a list of cons cells, each of the form (NAME . FONT-LIST-CACHE),
   one for each element of w32_display_list and in the same order.
   NAME is the name of the frame.
   FONT-LIST-CACHE records previous values returned by x-list-fonts.  */
Lisp_Object w32_display_name_list;


#ifndef GLYPHSET
/* Pre Windows 2000, this was not available, but define it here so
   that Emacs compiled on such a platform will run on newer versions.  */

typedef struct tagWCRANGE
{
  WCHAR wcLow;
  USHORT cGlyphs;
} WCRANGE;

typedef struct tagGLYPHSET
{
  DWORD cbThis;
  DWORD flAccel;
  DWORD cGlyphsSupported;
  DWORD cRanges;
  WCRANGE ranges[1];
} GLYPHSET;

#endif

/* Dynamic linking to SetLayeredWindowAttribute (only since 2000).  */
BOOL (WINAPI *pfnSetLayeredWindowAttributes) (HWND, COLORREF, BYTE, DWORD);

#ifndef LWA_ALPHA
#define LWA_ALPHA 0x02
#endif
/* WS_EX_LAYERED is defined unconditionally by MingW, but only for W2K and
   later targets by MSVC headers.  */
#ifndef WS_EX_LAYERED
#define WS_EX_LAYERED 0x80000
#endif

/* This is a frame waiting to be autoraised, within w32_read_socket.  */
struct frame *pending_autoraise_frame;

/* The handle of the frame that currently owns the system caret.  */
HWND w32_system_caret_hwnd;
int w32_system_caret_height;
int w32_system_caret_x;
int w32_system_caret_y;
DWORD dwWindowsThreadId = 0;
HANDLE hWindowsThread = NULL;
DWORD dwMainThreadId = 0;
HANDLE hMainThread = NULL;

int vertical_scroll_bar_min_handle;
int vertical_scroll_bar_top_border;
int vertical_scroll_bar_bottom_border;

int last_scroll_bar_drag_pos;

/* Mouse movement. */

/* Where the mouse was last time we reported a mouse event.  */
static RECT last_mouse_glyph;
static FRAME_PTR last_mouse_glyph_frame;

/* The scroll bar in which the last motion event occurred.

   If the last motion event occurred in a scroll bar, we set this
   so w32_mouse_position can know whether to report a scroll bar motion or
   an ordinary motion.

   If the last motion event didn't occur in a scroll bar, we set this
   to Qnil, to tell w32_mouse_position to return an ordinary motion event.  */
static Lisp_Object last_mouse_scroll_bar;
static int last_mouse_scroll_bar_pos;

/* This is a hack.  We would really prefer that w32_mouse_position would
   return the time associated with the position it returns, but there
   doesn't seem to be any way to wrest the time-stamp from the server
   along with the position query.  So, we just keep track of the time
   of the last movement we received, and return that in hopes that
   it's somewhat accurate.  */
static Time last_mouse_movement_time;

/* Incremented by w32_read_socket whenever it really tries to read
   events.  */
#ifdef __STDC__
static int volatile input_signal_count;
#else
static int input_signal_count;
#endif

/* Keyboard code page - may be changed by language-change events.  */
static int keyboard_codepage;

static void x_update_window_end (struct window *, int, int);
static void w32_handle_tool_bar_click (struct frame *,
                                       struct input_event *);
static void w32_define_cursor (Window, Cursor);

void x_lower_frame (struct frame *);
void x_scroll_bar_clear (struct frame *);
void x_wm_set_size_hint (struct frame *, long, int);
void x_raise_frame (struct frame *);
void x_set_window_size (struct frame *, int, int, int);
void x_wm_set_window_state (struct frame *, int);
void x_wm_set_icon_pixmap (struct frame *, int);
static void w32_initialize (void);
static void x_update_end (struct frame *);
static void w32_frame_up_to_date (struct frame *);
static void w32_set_terminal_modes (struct terminal *);
static void w32_reset_terminal_modes (struct terminal *);
static void x_clear_frame (struct frame *);
static void frame_highlight (struct frame *);
static void frame_unhighlight (struct frame *);
static void x_new_focus_frame (struct w32_display_info *,
                               struct frame *);
static void x_focus_changed (int, int, struct w32_display_info *,
                             struct frame *, struct input_event *);
static void w32_detect_focus_change (struct w32_display_info *,
                                     W32Msg *, struct input_event *);
static void w32_frame_rehighlight (struct frame *);
static void x_frame_rehighlight (struct w32_display_info *);
static void x_draw_hollow_cursor (struct window *, struct glyph_row *);
static void x_draw_bar_cursor (struct window *, struct glyph_row *, int,
                               enum text_cursor_kinds);
static void w32_clip_to_row (struct window *, struct glyph_row *, int, HDC);
static BOOL my_show_window (struct frame *, HWND, int);
static void my_set_window_pos (HWND, HWND, int, int, int, int, UINT);
#if 0
static void my_set_focus (struct frame *, HWND);
#endif
static void my_set_foreground_window (HWND);
static void my_destroy_window (struct frame *, HWND);

static Lisp_Object Qvendor_specific_keysyms;


/***********************************************************************
			      Debugging
 ***********************************************************************/

#if 0

/* This is a function useful for recording debugging information about
   the sequence of occurrences in this file.  */

struct record
{
  char *locus;
  int type;
};

struct record event_record[100];

int event_record_index;

record_event (char *locus, int type)
{
  if (event_record_index == sizeof (event_record) / sizeof (struct record))
    event_record_index = 0;

  event_record[event_record_index].locus = locus;
  event_record[event_record_index].type = type;
  event_record_index++;
}

#endif /* 0 */


void
XChangeGC (void *ignore, XGCValues *gc, unsigned long mask,
	   XGCValues *xgcv)
{
  if (mask & GCForeground)
    gc->foreground = xgcv->foreground;
  if (mask & GCBackground)
    gc->background = xgcv->background;
  if (mask & GCFont)
    gc->font = xgcv->font;
}

XGCValues *
XCreateGC (void *ignore, Window window, unsigned long mask, XGCValues *xgcv)
{
  XGCValues *gc = (XGCValues *) xmalloc (sizeof (XGCValues));
  memset (gc, 0, sizeof (XGCValues));

  XChangeGC (ignore, gc, mask, xgcv);

  return gc;
}

void
XGetGCValues (void *ignore, XGCValues *gc,
	      unsigned long mask, XGCValues *xgcv)
{
  XChangeGC (ignore, xgcv, mask, gc);
}

static void
w32_set_clip_rectangle (HDC hdc, RECT *rect)
{
  if (rect)
    {
      HRGN clip_region = CreateRectRgnIndirect (rect);
      SelectClipRgn (hdc, clip_region);
      DeleteObject (clip_region);
    }
  else
    SelectClipRgn (hdc, NULL);
}


/* Draw a hollow rectangle at the specified position.  */
void
w32_draw_rectangle (HDC hdc, XGCValues *gc, int x, int y,
                    int width, int height)
{
  HBRUSH hb, oldhb;
  HPEN hp, oldhp;

  hb = CreateSolidBrush (gc->background);
  hp = CreatePen (PS_SOLID, 0, gc->foreground);
  oldhb = SelectObject (hdc, hb);
  oldhp = SelectObject (hdc, hp);

  Rectangle (hdc, x, y, x + width, y + height);

  SelectObject (hdc, oldhb);
  SelectObject (hdc, oldhp);
  DeleteObject (hb);
  DeleteObject (hp);
}

/* Draw a filled rectangle at the specified position. */
void
w32_fill_rect (FRAME_PTR f, HDC hdc, COLORREF pix, RECT *lprect)
{
  HBRUSH hb;

  hb = CreateSolidBrush (pix);
  FillRect (hdc, lprect, hb);
  DeleteObject (hb);
}

void
w32_clear_window (FRAME_PTR f)
{
  RECT rect;
  HDC hdc = get_frame_dc (f);

  /* Under certain conditions, this can be called at startup with
     a console frame pointer before the GUI frame is created. An HDC
     of 0 indicates this. */
  if (hdc)
    {
      GetClientRect (FRAME_W32_WINDOW (f), &rect);
      w32_clear_rect (f, hdc, &rect);
    }

  release_frame_dc (f, hdc);
}

#define OPAQUE_FRAME 255

void
x_set_frame_alpha (struct frame *f)
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  double alpha = 1.0;
  double alpha_min = 1.0;
  BYTE opac;
  LONG ex_style;
  HWND window = FRAME_W32_WINDOW (f);

  /* Older versions of Windows do not support transparency.  */
  if (!pfnSetLayeredWindowAttributes)
    return;

  if (dpyinfo->x_highlight_frame == f)
    alpha = f->alpha[0];
  else
    alpha = f->alpha[1];

  if (FLOATP (Vframe_alpha_lower_limit))
    alpha_min = XFLOAT_DATA (Vframe_alpha_lower_limit);
  else if (INTEGERP (Vframe_alpha_lower_limit))
    alpha_min = (XINT (Vframe_alpha_lower_limit)) / 100.0;

  if (alpha < 0.0)
    return;
  else if (alpha > 1.0)
    alpha = 1.0;
  else if (alpha < alpha_min && alpha_min <= 1.0)
    alpha = alpha_min;

  opac = alpha * OPAQUE_FRAME;

  ex_style = GetWindowLong (window, GWL_EXSTYLE);

  if (opac == OPAQUE_FRAME)
    ex_style &= ~WS_EX_LAYERED;
  else
    ex_style |= WS_EX_LAYERED;

  SetWindowLong (window, GWL_EXSTYLE, ex_style);

  if (opac != OPAQUE_FRAME)
    pfnSetLayeredWindowAttributes (window, 0, opac, LWA_ALPHA);
}

int
x_display_pixel_height (struct w32_display_info *dpyinfo)
{
  HDC dc = GetDC (NULL);
  int pixels = GetDeviceCaps (dc, VERTRES);
  ReleaseDC (NULL, dc);
  return pixels;
}

int
x_display_pixel_width (struct w32_display_info *dpyinfo)
{
  HDC dc = GetDC (NULL);
  int pixels = GetDeviceCaps (dc, HORZRES);
  ReleaseDC (NULL, dc);
  return pixels;
}


/***********************************************************************
		    Starting and ending an update
 ***********************************************************************/

/* Start an update of frame F.  This function is installed as a hook
   for update_begin, i.e. it is called when update_begin is called.
   This function is called prior to calls to x_update_window_begin for
   each window being updated.  */

static void
x_update_begin (struct frame *f)
{
  struct w32_display_info *display_info = FRAME_W32_DISPLAY_INFO (f);

  if (! FRAME_W32_P (f))
    return;

  /* Regenerate display palette before drawing if list of requested
     colors has changed. */
  if (display_info->regen_palette)
  {
    w32_regenerate_palette (f);
    display_info->regen_palette = FALSE;
  }
}


/* Start update of window W.  Set the global variable updated_window
   to the window being updated and set output_cursor to the cursor
   position of W.  */

static void
x_update_window_begin (struct window *w)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);

  /* Hide the system caret during an update.  */
  if (w32_use_visible_system_caret && w32_system_caret_hwnd)
    {
      SendMessage (w32_system_caret_hwnd, WM_EMACS_HIDE_CARET, 0, 0);
    }

  updated_window = w;
  set_output_cursor (&w->cursor);

  BLOCK_INPUT;

  if (f == hlinfo->mouse_face_mouse_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      hlinfo->mouse_face_defer = 1;

      /* If F needs to be redrawn, simply forget about any prior mouse
	 highlighting.  */
      if (FRAME_GARBAGED_P (f))
	hlinfo->mouse_face_window = Qnil;

#if 0 /* Rows in a current matrix containing glyphs in mouse-face have
	 their mouse_face_p flag set, which means that they are always
	 unequal to rows in a desired matrix which never have that
	 flag set.  So, rows containing mouse-face glyphs are never
	 scrolled, and we don't have to switch the mouse highlight off
	 here to prevent it from being scrolled.  */

      /* Can we tell that this update does not affect the window
	 where the mouse highlight is?  If so, no need to turn off.
	 Likewise, don't do anything if the frame is garbaged;
	 in that case, the frame's current matrix that we would use
	 is all wrong, and we will redisplay that line anyway.  */
      if (!NILP (hlinfo->mouse_face_window)
	  && w == XWINDOW (hlinfo->mouse_face_window))
	{
	  int i;

          for (i = 0; i < w->desired_matrix->nrows; ++i)
	    if (MATRIX_ROW_ENABLED_P (w->desired_matrix, i))
	      break;

	  if (i < w->desired_matrix->nrows)
	    clear_mouse_face (hlinfo);
	}
#endif /* 0 */
    }

  UNBLOCK_INPUT;
}

/* Draw a vertical window border from (x,y0) to (x,y1)  */

static void
w32_draw_vertical_window_border (struct window *w, int x, int y0, int y1)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  RECT r;
  HDC hdc;
  struct face *face;

  r.left = x;
  r.right = x + 1;
  r.top = y0;
  r.bottom = y1;

  hdc = get_frame_dc (f);
  face = FACE_FROM_ID (f, VERTICAL_BORDER_FACE_ID);
  if (face)
    w32_fill_rect (f, hdc, face->foreground, &r);
  else
    w32_fill_rect (f, hdc, FRAME_FOREGROUND_PIXEL (f), &r);

  release_frame_dc (f, hdc);
}


/* End update of window W (which is equal to updated_window).

   Draw vertical borders between horizontally adjacent windows, and
   display W's cursor if CURSOR_ON_P is non-zero.

   MOUSE_FACE_OVERWRITTEN_P non-zero means that some row containing
   glyphs in mouse-face were overwritten.  In that case we have to
   make sure that the mouse-highlight is properly redrawn.

   W may be a menu bar pseudo-window in case we don't have X toolkit
   support. Such windows don't have a cursor, so don't display it
   here. */

static void
x_update_window_end (struct window *w, int cursor_on_p,
		     int mouse_face_overwritten_p)
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (XFRAME (w->frame));

  if (!w->pseudo_window_p)
    {
      BLOCK_INPUT;

      if (cursor_on_p)
	display_and_set_cursor (w, 1, output_cursor.hpos,
				output_cursor.vpos,
				output_cursor.x, output_cursor.y);

      if (draw_window_fringes (w, 1))
	x_draw_vertical_border (w);

      UNBLOCK_INPUT;
    }

  /* If a row with mouse-face was overwritten, arrange for
     XTframe_up_to_date to redisplay the mouse highlight.  */
  if (mouse_face_overwritten_p)
    {
      hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
      hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
      hlinfo->mouse_face_window = Qnil;
    }

  /* Unhide the caret.  This won't actually show the cursor, unless it
     was visible before the corresponding call to HideCaret in
     x_update_window_begin.  */
  if (w32_use_visible_system_caret && w32_system_caret_hwnd)
    {
      SendMessage (w32_system_caret_hwnd, WM_EMACS_SHOW_CARET, 0, 0);
    }

  updated_window = NULL;
}


/* End update of frame F.  This function is installed as a hook in
   update_end.  */

static void
x_update_end (struct frame *f)
{
  if (! FRAME_W32_P (f))
    return;

  /* Mouse highlight may be displayed again.  */
  MOUSE_HL_INFO (f)->mouse_face_defer = 0;
}


/* This function is called from various places in xdisp.c whenever a
   complete update has been performed.  The global variable
   updated_window is not available here.  */

static void
w32_frame_up_to_date (struct frame *f)
{
  if (FRAME_W32_P (f))
    {
      Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);

      if (hlinfo->mouse_face_deferred_gc
	  || f == hlinfo->mouse_face_mouse_frame)
	{
	  BLOCK_INPUT;
	  if (hlinfo->mouse_face_mouse_frame)
	    note_mouse_highlight (hlinfo->mouse_face_mouse_frame,
				  hlinfo->mouse_face_mouse_x,
				  hlinfo->mouse_face_mouse_y);
	  hlinfo->mouse_face_deferred_gc = 0;
	  UNBLOCK_INPUT;
	}
    }
}


/* Draw truncation mark bitmaps, continuation mark bitmaps, overlay
   arrow bitmaps, or clear the fringes if no bitmaps are required
   before DESIRED_ROW is made current.  The window being updated is
   found in updated_window.  This function is called from
   update_window_line only if it is known that there are differences
   between bitmaps to be drawn between current row and DESIRED_ROW.  */

static void
x_after_update_window_line (struct glyph_row *desired_row)
{
  struct window *w = updated_window;
  struct frame *f;
  int width, height;

  xassert (w);

  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    desired_row->redraw_fringe_bitmaps_p = 1;

  /* When a window has disappeared, make sure that no rest of
     full-width rows stays visible in the internal border.  Could
     check here if updated_window is the leftmost/rightmost window,
     but I guess it's not worth doing since vertically split windows
     are almost never used, internal border is rarely set, and the
     overhead is very small.  */
  if (windows_or_buffers_changed
      && desired_row->full_width_p
      && (f = XFRAME (w->frame),
	  width = FRAME_INTERNAL_BORDER_WIDTH (f),
	  width != 0)
      && (height = desired_row->visible_height,
	  height > 0))
    {
      int y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, desired_row->y));

      BLOCK_INPUT;
      {
	HDC hdc = get_frame_dc (f);
	w32_clear_area (f, hdc, 0, y, width, height);
	w32_clear_area (f, hdc, FRAME_PIXEL_WIDTH (f) - width,
			y, width, height);
	release_frame_dc (f, hdc);
      }
      UNBLOCK_INPUT;
    }
}


/* Draw the bitmap WHICH in one of the left or right fringes of
   window W.  ROW is the glyph row for which to display the bitmap; it
   determines the vertical position at which the bitmap has to be
   drawn.  */

static void
w32_draw_fringe_bitmap (struct window *w, struct glyph_row *row,
			struct draw_fringe_bitmap_params *p)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  HDC hdc;
  struct face *face = p->face;

  hdc = get_frame_dc (f);

  if (!p->overlay_p)
    {
      int bx = p->bx, by = p->by, nx = p->nx, ny = p->ny;

      /* If the fringe is adjacent to the left (right) scroll bar of a
	 leftmost (rightmost, respectively) window, then extend its
	 background to the gap between the fringe and the bar.  */
      if ((WINDOW_LEFTMOST_P (w)
	   && WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w))
	  || (WINDOW_RIGHTMOST_P (w)
	      && WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w)))
	{
	  int sb_width = WINDOW_CONFIG_SCROLL_BAR_WIDTH (w);

	  if (sb_width > 0)
	    {
	      int bar_area_x = WINDOW_SCROLL_BAR_AREA_X (w);
	      int bar_area_width = (WINDOW_CONFIG_SCROLL_BAR_COLS (w)
				    * FRAME_COLUMN_WIDTH (f));

	      if (bx < 0)
		{
		  /* Bitmap fills the fringe.  */
		  if (bar_area_x + bar_area_width == p->x)
		    bx = bar_area_x + sb_width;
		  else if (p->x + p->wd == bar_area_x)
		    bx = bar_area_x;
		  if (bx >= 0)
		    {
		      int header_line_height = WINDOW_HEADER_LINE_HEIGHT (w);

		      nx = bar_area_width - sb_width;
		      by = WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height,
							    row->y));
		      ny = row->visible_height;
		    }
		}
	      else
		{
		  if (bar_area_x + bar_area_width == bx)
		    {
		      bx = bar_area_x + sb_width;
		      nx += bar_area_width - sb_width;
		    }
		  else if (bx + nx == bar_area_x)
		    nx += bar_area_width - sb_width;
		}
	    }
	}

      if (bx >= 0 && nx > 0)
	w32_fill_area (f, hdc, face->background, bx, by, nx, ny);
    }

  /* Must clip because of partially visible lines.  */
  w32_clip_to_row (w, row, -1, hdc);

  if (p->which && p->which < max_fringe_bmp)
    {
      HBITMAP pixmap = fringe_bmp[p->which];
      HDC compat_hdc;
      HANDLE horig_obj;

      compat_hdc = CreateCompatibleDC (hdc);

      SaveDC (hdc);

      horig_obj = SelectObject (compat_hdc, pixmap);

      /* Paint overlays transparently.  */
      if (p->overlay_p)
	{
	  HBRUSH h_brush, h_orig_brush;

	  SetTextColor (hdc, BLACK_PIX_DEFAULT (f));
	  SetBkColor (hdc, WHITE_PIX_DEFAULT (f));
	  h_brush = CreateSolidBrush (face->foreground);
	  h_orig_brush = SelectObject (hdc, h_brush);

	  BitBlt (hdc, p->x, p->y, p->wd, p->h,
		  compat_hdc, 0, p->dh,
		  DSTINVERT);
	  BitBlt (hdc, p->x, p->y, p->wd, p->h,
		  compat_hdc, 0, p->dh,
		  0x2E064A);
	  BitBlt (hdc, p->x, p->y, p->wd, p->h,
		  compat_hdc, 0, p->dh,
		  DSTINVERT);

	  SelectObject (hdc, h_orig_brush);
	  DeleteObject (h_brush);
	}
      else
	{
	  SetTextColor (hdc, face->background);
	  SetBkColor (hdc, (p->cursor_p
			    ? f->output_data.w32->cursor_pixel
			    : face->foreground));

	  BitBlt (hdc, p->x, p->y, p->wd, p->h,
		  compat_hdc, 0, p->dh,
		  SRCCOPY);
	}

      SelectObject (compat_hdc, horig_obj);
      DeleteDC (compat_hdc);
      RestoreDC (hdc, -1);
    }

  w32_set_clip_rectangle (hdc, NULL);

  release_frame_dc (f, hdc);
}

static void
w32_define_fringe_bitmap (int which, unsigned short *bits, int h, int wd)
{
  if (which >= max_fringe_bmp)
    {
      int i = max_fringe_bmp;
      max_fringe_bmp = which + 20;
      fringe_bmp = (HBITMAP *) xrealloc (fringe_bmp, max_fringe_bmp * sizeof (HBITMAP));
      while (i < max_fringe_bmp)
	fringe_bmp[i++] = 0;
    }

  fringe_bmp[which] = CreateBitmap (wd, h, 1, 1, bits);
}

static void
w32_destroy_fringe_bitmap (int which)
{
  if (which >= max_fringe_bmp)
    return;

  if (fringe_bmp[which])
    DeleteObject (fringe_bmp[which]);
  fringe_bmp[which] = 0;
}



/* This is called when starting Emacs and when restarting after
   suspend.  When starting Emacs, no window is mapped.  And nothing
   must be done to Emacs's own window if it is suspended (though that
   rarely happens).  */

static void
w32_set_terminal_modes (struct terminal *term)
{
}

/* This is called when exiting or suspending Emacs.  Exiting will make
   the W32 windows go away, and suspending requires no action. */

static void
w32_reset_terminal_modes (struct terminal *term)
{
}



/***********************************************************************
			   Display Iterator
 ***********************************************************************/

/* Function prototypes of this page.  */

static void x_set_glyph_string_clipping (struct glyph_string *);
static void x_set_glyph_string_gc (struct glyph_string *);
static void x_draw_glyph_string_background (struct glyph_string *,
                                            int);
static void x_draw_glyph_string_foreground (struct glyph_string *);
static void x_draw_composite_glyph_string_foreground (struct glyph_string *);
static void x_draw_glyph_string_box (struct glyph_string *);
static void x_draw_glyph_string  (struct glyph_string *);
static void x_set_cursor_gc (struct glyph_string *);
static void x_set_mode_line_face_gc (struct glyph_string *);
static void x_set_mouse_face_gc (struct glyph_string *);
static int w32_alloc_lighter_color (struct frame *, COLORREF *, double, int);
static void w32_setup_relief_color (struct frame *, struct relief *,
                                    double, int, COLORREF);
static void x_setup_relief_colors (struct glyph_string *);
static void x_draw_image_glyph_string (struct glyph_string *);
static void x_draw_image_relief (struct glyph_string *);
static void x_draw_image_foreground (struct glyph_string *);
static void w32_draw_image_foreground_1 (struct glyph_string *, HBITMAP);
static void x_clear_glyph_string_rect (struct glyph_string *, int,
                                       int, int, int);
static void w32_draw_relief_rect (struct frame *, int, int, int, int,
                                  int, int, int, int, int, int,
                                  RECT *);
static void w32_draw_box_rect (struct glyph_string *, int, int, int, int,
                               int, int, int, RECT *);


/* Set S->gc to a suitable GC for drawing glyph string S in cursor
   face.  */

static void
x_set_cursor_gc (struct glyph_string *s)
{
  if (s->font == FRAME_FONT (s->f)
      && s->face->background == FRAME_BACKGROUND_PIXEL (s->f)
      && s->face->foreground == FRAME_FOREGROUND_PIXEL (s->f)
      && !s->cmp)
    s->gc = s->f->output_data.w32->cursor_gc;
  else
    {
      /* Cursor on non-default face: must merge.  */
      XGCValues xgcv;
      unsigned long mask;

      xgcv.background = s->f->output_data.w32->cursor_pixel;
      xgcv.foreground = s->face->background;

      /* If the glyph would be invisible, try a different foreground.  */
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->f->output_data.w32->cursor_foreground_pixel;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;

      /* Make sure the cursor is distinct from text in this face.  */
      if (xgcv.background == s->face->background
	  && xgcv.foreground == s->face->foreground)
	{
	  xgcv.background = s->face->foreground;
	  xgcv.foreground = s->face->background;
	}

      IF_DEBUG (x_check_font (s->f, s->font));
      xgcv.font = s->font;
      mask = GCForeground | GCBackground | GCFont;

      if (FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	XChangeGC (NULL, FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc,
		   mask, &xgcv);
      else
	FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc
	  = XCreateGC (NULL, s->window, mask, &xgcv);

      s->gc = FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }
}


/* Set up S->gc of glyph string S for drawing text in mouse face.  */

static void
x_set_mouse_face_gc (struct glyph_string *s)
{
  int face_id;
  struct face *face;

  /* What face has to be used last for the mouse face?  */
  face_id = MOUSE_HL_INFO (s->f)->mouse_face_face_id;
  face = FACE_FROM_ID (s->f, face_id);
  if (face == NULL)
    face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);

  if (s->first_glyph->type == CHAR_GLYPH)
    face_id = FACE_FOR_CHAR (s->f, face, s->first_glyph->u.ch, -1, Qnil);
  else
    face_id = FACE_FOR_CHAR (s->f, face, 0, -1, Qnil);
  s->face = FACE_FROM_ID (s->f, face_id);
  PREPARE_FACE_FOR_DISPLAY (s->f, s->face);

  /* If font in this face is same as S->font, use it.  */
  if (s->font == s->face->font)
    s->gc = s->face->gc;
  else
    {
      /* Otherwise construct scratch_cursor_gc with values from FACE
	 but font FONT.  */
      XGCValues xgcv;
      unsigned long mask;

      xgcv.background = s->face->background;
      xgcv.foreground = s->face->foreground;
      IF_DEBUG (x_check_font (s->f, s->font));
      xgcv.font = s->font;
      mask = GCForeground | GCBackground | GCFont;

      if (FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	XChangeGC (NULL, FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc,
		   mask, &xgcv);
      else
	FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc
	  = XCreateGC (NULL, s->window, mask, &xgcv);

      s->gc = FRAME_W32_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }

  xassert (s->gc != 0);
}


/* Set S->gc of glyph string S to a GC suitable for drawing a mode line.
   Faces to use in the mode line have already been computed when the
   matrix was built, so there isn't much to do, here.  */

static inline void
x_set_mode_line_face_gc (struct glyph_string *s)
{
  s->gc = s->face->gc;
}


/* Set S->gc of glyph string S for drawing that glyph string.  Set
   S->stippled_p to a non-zero value if the face of S has a stipple
   pattern.  */

static inline void
x_set_glyph_string_gc (struct glyph_string *s)
{
  PREPARE_FACE_FOR_DISPLAY (s->f, s->face);

  if (s->hl == DRAW_NORMAL_TEXT)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_INVERSE_VIDEO)
    {
      x_set_mode_line_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_CURSOR)
    {
      x_set_cursor_gc (s);
      s->stippled_p = 0;
    }
  else if (s->hl == DRAW_MOUSE_FACE)
    {
      x_set_mouse_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_IMAGE_RAISED
	   || s->hl == DRAW_IMAGE_SUNKEN)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }

  /* GC must have been set.  */
  xassert (s->gc != 0);
}


/* Set clipping for output of glyph string S.  S may be part of a mode
   line or menu if we don't have X toolkit support.  */

static inline void
x_set_glyph_string_clipping (struct glyph_string *s)
{
  RECT *r = s->clip;
  int n = get_glyph_string_clip_rects (s, r, 2);

  if (n == 1)
    w32_set_clip_rectangle (s->hdc, r);
  else if (n > 1)
    {
      HRGN clip1 = CreateRectRgnIndirect (r);
      HRGN clip2 = CreateRectRgnIndirect (r + 1);
      if (CombineRgn (clip1, clip1, clip2, RGN_OR) != ERROR)
        SelectClipRgn (s->hdc, clip1);
      DeleteObject (clip1);
      DeleteObject (clip2);
    }
    s->num_clips = n;
}

/* Set SRC's clipping for output of glyph string DST.  This is called
   when we are drawing DST's left_overhang or right_overhang only in
   the area of SRC.  */

static void
x_set_glyph_string_clipping_exactly (struct glyph_string *src,
				     struct glyph_string *dst)
{
  RECT r;

  r.left = src->x;
  r.right = r.left + src->width;
  r.top = src->y;
  r.bottom = r.top + src->height;
  dst->clip[0] = r;
  dst->num_clips = 1;
  w32_set_clip_rectangle (dst->hdc, &r);
}

/* RIF:
   Compute left and right overhang of glyph string S.  */

static void
w32_compute_glyph_string_overhangs (struct glyph_string *s)
{
  if (s->cmp == NULL
      && s->first_glyph->type == CHAR_GLYPH
      && !s->font_not_found_p)
    {
      unsigned *code = alloca (sizeof (unsigned) * s->nchars);
      struct font *font = s->font;
      struct font_metrics metrics;
      int i;

      for (i = 0; i < s->nchars; i++)
	code[i] = s->char2b[i];
      font->driver->text_extents (font, code, s->nchars, &metrics);
      s->right_overhang = (metrics.rbearing > metrics.width
			   ? metrics.rbearing - metrics.width : 0);
      s->left_overhang = metrics.lbearing < 0 ? -metrics.lbearing : 0;
    }
  else if (s->cmp)
    {
      s->right_overhang = s->cmp->rbearing - s->cmp->pixel_width;
      s->left_overhang = -s->cmp->lbearing;
    }
}

/* Fill rectangle X, Y, W, H with background color of glyph string S.  */

static inline void
x_clear_glyph_string_rect (struct glyph_string *s,
			   int x, int y, int w, int h)
{
  int real_x = x;
  int real_y = y;
  int real_w = w;
  int real_h = h;
#if 0
  /* Take clipping into account.  */
  if (s->gc->clip_mask == Rect)
    {
      real_x = max (real_x, s->gc->clip_rectangle.left);
      real_y = max (real_y, s->gc->clip_rectangle.top);
      real_w = min (real_w, s->gc->clip_rectangle.right
                    - s->gc->clip_rectangle.left);
      real_h = min (real_h, s->gc->clip_rectangle.bottom
                    - s->gc->clip_rectangle.top);
    }
#endif
  w32_fill_area (s->f, s->hdc, s->gc->background, real_x, real_y,
                 real_w, real_h);
}


/* Draw the background of glyph_string S.  If S->background_filled_p
   is non-zero don't draw it.  FORCE_P non-zero means draw the
   background even if it wouldn't be drawn normally.  This is used
   when a string preceding S draws into the background of S, or S
   contains the first component of a composition.  */

static void
x_draw_glyph_string_background (struct glyph_string *s, int force_p)
{
  /* Nothing to do if background has already been drawn or if it
     shouldn't be drawn in the first place.  */
  if (!s->background_filled_p)
    {
      int box_line_width = max (s->face->box_line_width, 0);

#if 0 /* TODO: stipple */
      if (s->stippled_p)
	{
	  /* Fill background with a stipple pattern.  */
	  XSetFillStyle (s->display, s->gc, FillOpaqueStippled);
	  XFillRectangle (s->display, s->window, s->gc, s->x,
			  s->y + box_line_width,
			  s->background_width,
			  s->height - 2 * box_line_width);
	  XSetFillStyle (s->display, s->gc, FillSolid);
	  s->background_filled_p = 1;
	}
      else
#endif
        if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
	       || s->font_not_found_p
	       || s->extends_to_end_of_line_p
	       || force_p)
	{
	  x_clear_glyph_string_rect (s, s->x, s->y + box_line_width,
				     s->background_width,
				     s->height - 2 * box_line_width);
	  s->background_filled_p = 1;
	}
    }
}


/* Draw the foreground of glyph string S.  */

static void
x_draw_glyph_string_foreground (struct glyph_string *s)
{
  int i, x;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + eabs (s->face->box_line_width);
  else
    x = s->x;

  SetTextColor (s->hdc, s->gc->foreground);
  SetBkColor (s->hdc, s->gc->background);
  SetTextAlign (s->hdc, TA_BASELINE | TA_LEFT);

  /* Draw characters of S as rectangles if S's font could not be
     loaded. */
  if (s->font_not_found_p)
    {
      for (i = 0; i < s->nchars; ++i)
        {
          struct glyph *g = s->first_glyph + i;

          w32_draw_rectangle (s->hdc, s->gc, x, s->y, g->pixel_width - 1,
                              s->height - 1);
          x += g->pixel_width;
        }
    }
  else
    {
      struct font *font = s->font;
      int boff = font->baseline_offset;
      int y;
      HFONT old_font;

      old_font = SelectObject (s->hdc, FONT_HANDLE (font));

      if (font->vertical_centering)
	boff = VCENTER_BASELINE_OFFSET (font, s->f) - boff;

      y = s->ybase - boff;
      if (s->for_overlaps
	  || (s->background_filled_p && s->hl != DRAW_CURSOR))
	font->driver->draw (s, 0, s->nchars, x, y, 0);
      else
	font->driver->draw (s, 0, s->nchars, x, y, 1);
      if (s->face->overstrike)
	font->driver->draw (s, 0, s->nchars, x + 1, y, 0);

      SelectObject (s->hdc, old_font);
    }
}

/* Draw the foreground of composite glyph string S.  */

static void
x_draw_composite_glyph_string_foreground (struct glyph_string *s)
{
  int i, j, x;
  struct font *font = s->font;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + eabs (s->face->box_line_width);
  else
    x = s->x;

  /* S is a glyph string for a composition.  S->cmp_from is the index
     of the first character drawn for glyphs of this composition.
     S->cmp_from == 0 means we are drawing the very first character of
     this composition.  */

  SetTextColor (s->hdc, s->gc->foreground);
  SetBkColor (s->hdc, s->gc->background);
  SetTextAlign (s->hdc, TA_BASELINE | TA_LEFT);

  /* Draw a rectangle for the composition if the font for the very
     first character of the composition could not be loaded.  */
  if (s->font_not_found_p)
    {
      if (s->cmp_from == 0)
        w32_draw_rectangle (s->hdc, s->gc, x, s->y, s->width - 1,
                            s->height - 1);
    }
  else if (! s->first_glyph->u.cmp.automatic)
    {
      int y = s->ybase;
      HFONT old_font;

      old_font = SelectObject (s->hdc, FONT_HANDLE (font));

      for (i = 0, j = s->cmp_from; i < s->nchars; i++, j++)
	/* TAB in a composition means display glyphs with padding
	   space on the left or right.  */
	if (COMPOSITION_GLYPH (s->cmp, j) != '\t')
	  {
	    int xx = x + s->cmp->offsets[j * 2];
	    int yy = y - s->cmp->offsets[j * 2 + 1];

	    font->driver->draw (s, j, j + 1, xx, yy, 0);
	    if (s->face->overstrike)
	      font->driver->draw (s, j, j + 1, xx + 1, yy, 0);
	  }
      SelectObject (s->hdc, old_font);
    }
  else
    {
      Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);
      Lisp_Object glyph;
      int y = s->ybase;
      int width = 0;
      HFONT old_font;

      old_font = SelectObject (s->hdc, FONT_HANDLE (font));

      for (i = j = s->cmp_from; i < s->cmp_to; i++)
	{
	  glyph = LGSTRING_GLYPH (gstring, i);
	  if (NILP (LGLYPH_ADJUSTMENT (glyph)))
	    width += LGLYPH_WIDTH (glyph);
	  else
	    {
	      int xoff, yoff, wadjust;

	      if (j < i)
		{
		  font->driver->draw (s, j, i, x, y, 0);
		  x += width;
		}
	      xoff = LGLYPH_XOFF (glyph);
	      yoff = LGLYPH_YOFF (glyph);
	      wadjust = LGLYPH_WADJUST (glyph);
	      font->driver->draw (s, i, i + 1, x + xoff, y + yoff, 0);
	      x += wadjust;
	      j = i + 1;
	      width = 0;
	    }
	}
      if (j < i)
	font->driver->draw (s, j, i, x, y, 0);

      SelectObject (s->hdc, old_font);
    }
}


/* Draw the foreground of glyph string S for glyphless characters.  */

static void
x_draw_glyphless_glyph_string_foreground (struct glyph_string *s)
{
  struct glyph *glyph = s->first_glyph;
  XChar2b char2b[8];
  int x, i, j;
  int with_background;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + eabs (s->face->box_line_width);
  else
    x = s->x;

  SetTextColor (s->hdc, s->gc->foreground);
  SetBkColor (s->hdc, s->gc->background);
  SetTextAlign (s->hdc, TA_BASELINE | TA_LEFT);

  s->char2b = char2b;
  with_background = ! (s->for_overlaps
		       || (s->background_filled_p && s->hl != DRAW_CURSOR));
  for (i = 0; i < s->nchars; i++, glyph++)
    {
      char buf[7], *str = NULL;
      int len = glyph->u.glyphless.len;

      if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_ACRONYM)
	{
	  if (len > 1
	      && CHAR_TABLE_P (Vglyphless_char_display)
	      && (CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (Vglyphless_char_display))
		  >= 1))
	    {
	      Lisp_Object acronym
		= (! glyph->u.glyphless.for_no_font
		   ? CHAR_TABLE_REF (Vglyphless_char_display,
				     glyph->u.glyphless.ch)
		   : XCHAR_TABLE (Vglyphless_char_display)->extras[0]);
	      if (STRINGP (acronym))
		str = SSDATA (acronym);
	    }
	}
      else if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_HEX_CODE)
	{
	  sprintf ((char *) buf, "%0*X",
		   glyph->u.glyphless.ch < 0x10000 ? 4 : 6,
		   glyph->u.glyphless.ch);
	  str = buf;
	}

      if (glyph->u.glyphless.method != GLYPHLESS_DISPLAY_THIN_SPACE)
	w32_draw_rectangle (s->hdc, s->gc,
			    x, s->ybase - glyph->ascent,
			    glyph->pixel_width - 1,
			    glyph->ascent + glyph->descent - 1);
      if (str)
	{
	  struct font *font = s->font;
	  int upper_len = (len + 1) / 2;
	  unsigned code;
	  HFONT old_font;

	  old_font = SelectObject (s->hdc, FONT_HANDLE (font));
	  /* It is certain that all LEN characters in STR are ASCII.  */
	  for (j = 0; j < len; j++)
	    {
	      code = font->driver->encode_char (font, str[j]);
	      STORE_XCHAR2B (char2b + j, code >> 8, code & 0xFF);
	    }
	  font->driver->draw (s, 0, upper_len,
			      x + glyph->slice.glyphless.upper_xoff,
			      s->ybase + glyph->slice.glyphless.upper_yoff,
			      with_background);
	  font->driver->draw (s, upper_len, len,
			      x + glyph->slice.glyphless.lower_xoff,
			      s->ybase + glyph->slice.glyphless.lower_yoff,
			      with_background);
	  SelectObject (s->hdc, old_font);
	}
      x += glyph->pixel_width;
   }
}


/* Brightness beyond which a color won't have its highlight brightness
   boosted.

   Nominally, highlight colors for `3d' faces are calculated by
   brightening an object's color by a constant scale factor, but this
   doesn't yield good results for dark colors, so for colors whose
   brightness is less than this value (on a scale of 0-255) have to
   use an additional additive factor.

   The value here is set so that the default menu-bar/mode-line color
   (grey75) will not have its highlights changed at all.  */
#define HIGHLIGHT_COLOR_DARK_BOOST_LIMIT 187


/* Allocate a color which is lighter or darker than *COLOR by FACTOR
   or DELTA.  Try a color with RGB values multiplied by FACTOR first.
   If this produces the same color as COLOR, try a color where all RGB
   values have DELTA added.  Return the allocated color in *COLOR.
   DISPLAY is the X display, CMAP is the colormap to operate on.
   Value is non-zero if successful.  */

static int
w32_alloc_lighter_color (struct frame *f, COLORREF *color,
			 double factor, int delta)
{
  COLORREF new;
  long bright;

  /* On Windows, RGB values are 0-255, not 0-65535, so scale delta. */
  delta /= 256;

  /* Change RGB values by specified FACTOR.  Avoid overflow!  */
  xassert (factor >= 0);
  new = PALETTERGB (min (0xff, factor * GetRValue (*color)),
                    min (0xff, factor * GetGValue (*color)),
                    min (0xff, factor * GetBValue (*color)));

  /* Calculate brightness of COLOR.  */
  bright = (2 * GetRValue (*color) + 3 * GetGValue (*color)
            + GetBValue (*color)) / 6;

  /* We only boost colors that are darker than
     HIGHLIGHT_COLOR_DARK_BOOST_LIMIT.  */
  if (bright < HIGHLIGHT_COLOR_DARK_BOOST_LIMIT)
    /* Make an additive adjustment to NEW, because it's dark enough so
       that scaling by FACTOR alone isn't enough.  */
    {
      /* How far below the limit this color is (0 - 1, 1 being darker).  */
      double dimness = 1 - (double)bright / HIGHLIGHT_COLOR_DARK_BOOST_LIMIT;
      /* The additive adjustment.  */
      int min_delta = delta * dimness * factor / 2;

      if (factor < 1)
        new = PALETTERGB (max (0, min (0xff, min_delta - GetRValue (*color))),
                          max (0, min (0xff, min_delta - GetGValue (*color))),
                          max (0, min (0xff, min_delta - GetBValue (*color))));
      else
        new = PALETTERGB (max (0, min (0xff, min_delta + GetRValue (*color))),
                          max (0, min (0xff, min_delta + GetGValue (*color))),
                          max (0, min (0xff, min_delta + GetBValue (*color))));
    }

  if (new == *color)
    new = PALETTERGB (max (0, min (0xff, delta + GetRValue (*color))),
                      max (0, min (0xff, delta + GetGValue (*color))),
                      max (0, min (0xff, delta + GetBValue (*color))));

  /* TODO: Map to palette and retry with delta if same? */
  /* TODO: Free colors (if using palette)? */

  if (new == *color)
    return 0;

  *color = new;

  return 1;
}

/* On frame F, translate pixel colors to RGB values for the NCOLORS
   colors in COLORS.  On W32, we no longer try to map colors to
   a palette.  */
void
x_query_colors (struct frame *f, XColor *colors, int ncolors)
{
  int i;

  for (i = 0; i < ncolors; i++)
    {
      DWORD pixel = colors[i].pixel;
      /* Convert to a 16 bit value in range 0 - 0xffff. */
      colors[i].red = GetRValue (pixel) * 257;
      colors[i].green = GetGValue (pixel) * 257;
      colors[i].blue = GetBValue (pixel) * 257;
    }
}

void
x_query_color (struct frame *f, XColor *color)
{
  x_query_colors (f, color, 1);
}


/* Set up the foreground color for drawing relief lines of glyph
   string S.  RELIEF is a pointer to a struct relief containing the GC
   with which lines will be drawn.  Use a color that is FACTOR or
   DELTA lighter or darker than the relief's background which is found
   in S->f->output_data.x->relief_background.  If such a color cannot
   be allocated, use DEFAULT_PIXEL, instead.  */

static void
w32_setup_relief_color (struct frame *f, struct relief *relief, double factor,
			int delta, COLORREF default_pixel)
{
  XGCValues xgcv;
  struct w32_output *di = f->output_data.w32;
  unsigned long mask = GCForeground;
  COLORREF pixel;
  COLORREF background = di->relief_background;
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  /* TODO: Free colors (if using palette)? */

  /* Allocate new color.  */
  xgcv.foreground = default_pixel;
  pixel = background;
  if (w32_alloc_lighter_color (f, &pixel, factor, delta))
    {
      relief->allocated_p = 1;
      xgcv.foreground = relief->pixel = pixel;
    }

  if (relief->gc == 0)
    {
#if 0 /* TODO: stipple */
      xgcv.stipple = dpyinfo->gray;
      mask |= GCStipple;
#endif
      relief->gc = XCreateGC (NULL, FRAME_W32_WINDOW (f), mask, &xgcv);
    }
  else
    XChangeGC (NULL, relief->gc, mask, &xgcv);
}


/* Set up colors for the relief lines around glyph string S.  */

static void
x_setup_relief_colors (struct glyph_string *s)
{
  struct w32_output *di = s->f->output_data.w32;
  COLORREF color;

  if (s->face->use_box_color_for_shadows_p)
    color = s->face->box_color;
  else if (s->first_glyph->type == IMAGE_GLYPH
	   && s->img->pixmap
	   && !IMAGE_BACKGROUND_TRANSPARENT (s->img, s->f, 0))
    color = IMAGE_BACKGROUND  (s->img, s->f, 0);
  else
    color = s->gc->background;

  if (di->white_relief.gc == 0
      || color != di->relief_background)
    {
      di->relief_background = color;
      w32_setup_relief_color (s->f, &di->white_relief, 1.2, 0x8000,
                              WHITE_PIX_DEFAULT (s->f));
      w32_setup_relief_color (s->f, &di->black_relief, 0.6, 0x4000,
                              BLACK_PIX_DEFAULT (s->f));
    }
}


/* Draw a relief on frame F inside the rectangle given by LEFT_X,
   TOP_Y, RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the relief
   to draw, it must be >= 0.  RAISED_P non-zero means draw a raised
   relief.  LEFT_P non-zero means draw a relief on the left side of
   the rectangle.  RIGHT_P non-zero means draw a relief on the right
   side of the rectangle.  CLIP_RECT is the clipping rectangle to use
   when drawing.  */

static void
w32_draw_relief_rect (struct frame *f,
		      int left_x, int top_y, int right_x, int bottom_y,
		      int width, int raised_p,
		      int top_p, int bot_p, int left_p, int right_p,
		      RECT *clip_rect)
{
  int i;
  XGCValues gc;
  HDC hdc = get_frame_dc (f);

  if (raised_p)
    gc.foreground = f->output_data.w32->white_relief.gc->foreground;
  else
    gc.foreground = f->output_data.w32->black_relief.gc->foreground;

  w32_set_clip_rectangle (hdc, clip_rect);

  /* Top.  */
  if (top_p)
    for (i = 0; i < width; ++i)
      w32_fill_area (f, hdc, gc.foreground,
		     left_x + i * left_p, top_y + i,
		     right_x - left_x - i * (left_p + right_p ) + 1, 1);

  /* Left.  */
  if (left_p)
    for (i = 0; i < width; ++i)
      w32_fill_area (f, hdc, gc.foreground,
		     left_x + i, top_y + i, 1,
		     bottom_y - top_y - 2 * i + 1);

  if (raised_p)
    gc.foreground = f->output_data.w32->black_relief.gc->foreground;
  else
    gc.foreground = f->output_data.w32->white_relief.gc->foreground;

  /* Bottom.  */
  if (bot_p)
    for (i = 0; i < width; ++i)
      w32_fill_area (f, hdc, gc.foreground,
		     left_x + i * left_p, bottom_y - i,
		     right_x - left_x - i * (left_p + right_p) + 1, 1);

  /* Right.  */
  if (right_p)
    for (i = 0; i < width; ++i)
      w32_fill_area (f, hdc, gc.foreground,
		     right_x - i, top_y + i + 1, 1,
		     bottom_y - top_y - 2 * i - 1);

  w32_set_clip_rectangle (hdc, NULL);

  release_frame_dc (f, hdc);
}


/* Draw a box on frame F inside the rectangle given by LEFT_X, TOP_Y,
   RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the lines to
   draw, it must be >= 0.  LEFT_P non-zero means draw a line on the
   left side of the rectangle.  RIGHT_P non-zero means draw a line
   on the right side of the rectangle.  CLIP_RECT is the clipping
   rectangle to use when drawing.  */

static void
w32_draw_box_rect (struct glyph_string *s,
		   int left_x, int top_y, int right_x, int bottom_y, int width,
                   int left_p, int right_p, RECT *clip_rect)
{
  w32_set_clip_rectangle (s->hdc, clip_rect);

  /* Top.  */
  w32_fill_area (s->f, s->hdc, s->face->box_color,
		  left_x, top_y, right_x - left_x + 1, width);

  /* Left.  */
  if (left_p)
    {
      w32_fill_area (s->f, s->hdc, s->face->box_color,
                     left_x, top_y, width, bottom_y - top_y + 1);
    }

  /* Bottom.  */
  w32_fill_area (s->f, s->hdc, s->face->box_color,
                 left_x, bottom_y - width + 1, right_x - left_x + 1, width);

  /* Right.  */
  if (right_p)
    {
      w32_fill_area (s->f, s->hdc, s->face->box_color,
                     right_x - width + 1, top_y, width, bottom_y - top_y + 1);
    }

  w32_set_clip_rectangle (s->hdc, NULL);
}


/* Draw a box around glyph string S.  */

static void
x_draw_glyph_string_box (struct glyph_string *s)
{
  int width, left_x, right_x, top_y, bottom_y, last_x, raised_p;
  int left_p, right_p;
  struct glyph *last_glyph;
  RECT clip_rect;

  last_x = ((s->row->full_width_p && !s->w->pseudo_window_p)
	    ? WINDOW_RIGHT_EDGE_X (s->w)
	    : window_box_right (s->w, s->area));

  /* The glyph that may have a right box line.  */
  last_glyph = (s->cmp || s->img
		? s->first_glyph
		: s->first_glyph + s->nchars - 1);

  width = eabs (s->face->box_line_width);
  raised_p = s->face->box == FACE_RAISED_BOX;
  left_x = s->x;
  right_x = ((s->row->full_width_p && s->extends_to_end_of_line_p
	      ? last_x - 1
	      : min (last_x, s->x + s->background_width) - 1));
  top_y = s->y;
  bottom_y = top_y + s->height - 1;

  left_p = (s->first_glyph->left_box_line_p
	    || (s->hl == DRAW_MOUSE_FACE
		&& (s->prev == NULL
		    || s->prev->hl != s->hl)));
  right_p = (last_glyph->right_box_line_p
	     || (s->hl == DRAW_MOUSE_FACE
		 && (s->next == NULL
		     || s->next->hl != s->hl)));

  get_glyph_string_clip_rect (s, &clip_rect);

  if (s->face->box == FACE_SIMPLE_BOX)
    w32_draw_box_rect (s, left_x, top_y, right_x, bottom_y, width,
                       left_p, right_p, &clip_rect);
  else
    {
      x_setup_relief_colors (s);
      w32_draw_relief_rect (s->f, left_x, top_y, right_x, bottom_y,
                            width, raised_p, 1, 1, left_p, right_p, &clip_rect);
    }
}


/* Draw foreground of image glyph string S.  */

static void
x_draw_image_foreground (struct glyph_string *s)
{
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += eabs (s->face->box_line_width);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  SaveDC (s->hdc);

  if (s->img->pixmap)
    {
      HDC compat_hdc = CreateCompatibleDC (s->hdc);
      HBRUSH fg_brush = CreateSolidBrush (s->gc->foreground);
      HBRUSH orig_brush = SelectObject (s->hdc, fg_brush);
      HGDIOBJ orig_obj = SelectObject (compat_hdc, s->img->pixmap);
      SetBkColor (compat_hdc, RGB (255, 255, 255));
      SetTextColor (s->hdc, RGB (0, 0, 0));
      x_set_glyph_string_clipping (s);

      if (s->img->mask)
	{
	  HDC mask_dc = CreateCompatibleDC (s->hdc);
	  HGDIOBJ mask_orig_obj = SelectObject (mask_dc, s->img->mask);

	  SetTextColor (s->hdc, RGB (255, 255, 255));
	  SetBkColor (s->hdc, RGB (0, 0, 0));

	  BitBlt (s->hdc, x, y, s->slice.width, s->slice.height,
		  compat_hdc, s->slice.x, s->slice.y, SRCINVERT);
	  BitBlt (s->hdc, x, y, s->slice.width, s->slice.height,
		  mask_dc, s->slice.x, s->slice.y, SRCAND);
	  BitBlt (s->hdc, x, y, s->slice.width, s->slice.height,
		  compat_hdc, s->slice.x, s->slice.y, SRCINVERT);

	  SelectObject (mask_dc, mask_orig_obj);
	  DeleteDC (mask_dc);
	}
      else
	{
	  SetTextColor (s->hdc, s->gc->foreground);
	  SetBkColor (s->hdc, s->gc->background);

          BitBlt (s->hdc, x, y, s->slice.width, s->slice.height,
                  compat_hdc, s->slice.x, s->slice.y, SRCCOPY);

	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    {
	      int r = s->img->relief;
	      if (r < 0) r = -r;
	      w32_draw_rectangle (s->hdc, s->gc, x - r, y - r ,
				  s->slice.width + r*2 - 1,
				  s->slice.height + r*2 - 1);
	    }
	}

      w32_set_clip_rectangle (s->hdc, NULL);
      SelectObject (s->hdc, orig_brush);
      DeleteObject (fg_brush);
      SelectObject (compat_hdc, orig_obj);
      DeleteDC (compat_hdc);
    }
  else
    w32_draw_rectangle (s->hdc, s->gc, x, y,
			s->slice.width - 1, s->slice.height - 1);

  RestoreDC (s->hdc ,-1);
}


/* Draw a relief around the image glyph string S.  */

static void
x_draw_image_relief (struct glyph_string *s)
{
  int x0, y0, x1, y1, thick, raised_p;
  RECT r;
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += eabs (s->face->box_line_width);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->hl == DRAW_IMAGE_SUNKEN
      || s->hl == DRAW_IMAGE_RAISED)
    {
      thick = tool_bar_button_relief >= 0 ? tool_bar_button_relief
	: DEFAULT_TOOL_BAR_BUTTON_RELIEF;
      raised_p = s->hl == DRAW_IMAGE_RAISED;
    }
  else
    {
      thick = eabs (s->img->relief);
      raised_p = s->img->relief > 0;
    }

  x0 = x - thick;
  y0 = y - thick;
  x1 = x + s->slice.width + thick - 1;
  y1 = y + s->slice.height + thick - 1;

  x_setup_relief_colors (s);
  get_glyph_string_clip_rect (s, &r);
  w32_draw_relief_rect (s->f, x0, y0, x1, y1, thick, raised_p,
			s->slice.y == 0,
			s->slice.y + s->slice.height == s->img->height,
			s->slice.x == 0,
			s->slice.x + s->slice.width == s->img->width,
			&r);
}


/* Draw the foreground of image glyph string S to PIXMAP.  */

static void
w32_draw_image_foreground_1 (struct glyph_string *s, HBITMAP pixmap)
{
  HDC hdc = CreateCompatibleDC (s->hdc);
  HGDIOBJ orig_hdc_obj = SelectObject (hdc, pixmap);
  int x = 0;
  int y = s->ybase - s->y - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += eabs (s->face->box_line_width);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->img->pixmap)
    {
      HDC compat_hdc = CreateCompatibleDC (hdc);
      HBRUSH fg_brush = CreateSolidBrush (s->gc->foreground);
      HBRUSH orig_brush = SelectObject (hdc, fg_brush);
      HGDIOBJ orig_obj = SelectObject (compat_hdc, s->img->pixmap);

      if (s->img->mask)
	{
	  HDC mask_dc = CreateCompatibleDC (hdc);
	  HGDIOBJ mask_orig_obj = SelectObject (mask_dc, s->img->mask);

	  SetTextColor (hdc, RGB (0, 0, 0));
	  SetBkColor (hdc, RGB (255, 255, 255));
	  BitBlt (hdc, x, y, s->slice.width, s->slice.height,
		  compat_hdc, s->slice.x, s->slice.y, SRCINVERT);
	  BitBlt (hdc, x, y, s->slice.width, s->slice.height,
		  mask_dc, s->slice.x, s->slice.y, SRCAND);
	  BitBlt (hdc, x, y, s->slice.width, s->slice.height,
		  compat_hdc, s->slice.x, s->slice.y, SRCINVERT);

	  SelectObject (mask_dc, mask_orig_obj);
	  DeleteDC (mask_dc);
	}
      else
	{
	  SetTextColor (hdc, s->gc->foreground);
	  SetBkColor (hdc, s->gc->background);

          BitBlt (hdc, x, y, s->slice.width, s->slice.height,
                  compat_hdc, s->slice.x, s->slice.y, SRCCOPY);

	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    {
	      int r = s->img->relief;
	      if (r < 0) r = -r;
	      w32_draw_rectangle (hdc, s->gc, x - r, y - r,
				  s->slice.width + r*2 - 1,
				  s->slice.height + r*2 - 1);
	    }
	}

      SelectObject (hdc, orig_brush);
      DeleteObject (fg_brush);
      SelectObject (compat_hdc, orig_obj);
      DeleteDC (compat_hdc);
    }
  else
    w32_draw_rectangle (hdc, s->gc, x, y,
			s->slice.width - 1, s->slice.height - 1);

  SelectObject (hdc, orig_hdc_obj);
  DeleteDC (hdc);
}


/* Draw part of the background of glyph string S.  X, Y, W, and H
   give the rectangle to draw.  */

static void
x_draw_glyph_string_bg_rect (struct glyph_string *s, int x, int y, int w, int h)
{
#if 0 /* TODO: stipple */
  if (s->stippled_p)
    {
      /* Fill background with a stipple pattern.  */
      XSetFillStyle (s->display, s->gc, FillOpaqueStippled);
      XFillRectangle (s->display, s->window, s->gc, x, y, w, h);
      XSetFillStyle (s->display, s->gc, FillSolid);
    }
  else
#endif
    x_clear_glyph_string_rect (s, x, y, w, h);
}


/* Draw image glyph string S.

            s->y
   s->x      +-------------------------
	     |   s->face->box
	     |
	     |     +-------------------------
	     |     |  s->img->vmargin
	     |     |
	     |     |       +-------------------
	     |     |       |  the image

 */

static void
x_draw_image_glyph_string (struct glyph_string *s)
{
  int x, y;
  int box_line_hwidth = eabs (s->face->box_line_width);
  int box_line_vwidth = max (s->face->box_line_width, 0);
  int height;
  HBITMAP pixmap = 0;

  height = s->height - 2 * box_line_vwidth;

  /* Fill background with face under the image.  Do it only if row is
     taller than image or if image has a clip mask to reduce
     flickering.  */
  s->stippled_p = s->face->stipple != 0;
  if (height > s->slice.height
      || s->img->hmargin
      || s->img->vmargin
      || s->img->mask
      || s->img->pixmap == 0
      || s->width != s->background_width)
    {
      x = s->x;
      if (s->first_glyph->left_box_line_p
	  && s->slice.x == 0)
	x += box_line_hwidth;

      y = s->y;
      if (s->slice.y == 0)
	y += box_line_vwidth;

#if 0 /* TODO: figure out if we need to do this on Windows.  */
      if (s->img->mask)
	{
	  /* Create a pixmap as large as the glyph string.  Fill it
	     with the background color.  Copy the image to it, using
	     its mask.  Copy the temporary pixmap to the display.  */
	  Screen *screen = FRAME_X_SCREEN (s->f);
	  int depth = DefaultDepthOfScreen (screen);

	  /* Create a pixmap as large as the glyph string.  */
 	  pixmap = XCreatePixmap (s->display, s->window,
				  s->background_width,
				  s->height, depth);

	  /* Don't clip in the following because we're working on the
	     pixmap.  */
	  XSetClipMask (s->display, s->gc, None);

	  /* Fill the pixmap with the background color/stipple.  */
	  if (s->stippled_p)
	    {
	      /* Fill background with a stipple pattern.  */
	      XSetFillStyle (s->display, s->gc, FillOpaqueStippled);
	      XFillRectangle (s->display, pixmap, s->gc,
			      0, 0, s->background_width, s->height);
	      XSetFillStyle (s->display, s->gc, FillSolid);
	    }
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground | GCBackground,
			    &xgcv);
	      XSetForeground (s->display, s->gc, xgcv.background);
	      XFillRectangle (s->display, pixmap, s->gc,
			      0, 0, s->background_width, s->height);
	      XSetForeground (s->display, s->gc, xgcv.foreground);
	    }
	}
      else
#endif
	x_draw_glyph_string_bg_rect (s, x, y, s->background_width, height);

      s->background_filled_p = 1;
    }

  /* Draw the foreground.  */
  if (pixmap != 0)
    {
      w32_draw_image_foreground_1 (s, pixmap);
      x_set_glyph_string_clipping (s);
      {
        HDC compat_hdc = CreateCompatibleDC (s->hdc);
        HBRUSH fg_brush = CreateSolidBrush (s->gc->foreground);
        HBRUSH orig_brush = SelectObject (s->hdc, fg_brush);
        HGDIOBJ orig_obj = SelectObject (compat_hdc, pixmap);

        SetTextColor (s->hdc, s->gc->foreground);
        SetBkColor (s->hdc, s->gc->background);
        BitBlt (s->hdc, s->x, s->y, s->background_width, s->height,
                compat_hdc, 0, 0, SRCCOPY);

        SelectObject (s->hdc, orig_brush);
        DeleteObject (fg_brush);
        SelectObject (compat_hdc, orig_obj);
        DeleteDC (compat_hdc);
      }
      DeleteObject (pixmap);
      pixmap = 0;
    }
  else
    x_draw_image_foreground (s);

  /* If we must draw a relief around the image, do it.  */
  if (s->img->relief
      || s->hl == DRAW_IMAGE_RAISED
      || s->hl == DRAW_IMAGE_SUNKEN)
    x_draw_image_relief (s);
}


/* Draw stretch glyph string S.  */

static void
x_draw_stretch_glyph_string (struct glyph_string *s)
{
  xassert (s->first_glyph->type == STRETCH_GLYPH);

  if (s->hl == DRAW_CURSOR
      && !x_stretch_cursor_p)
    {
      /* If `x-stretch-cursor' is nil, don't draw a block cursor as
	 wide as the stretch glyph.  */
      int width, background_width = s->background_width;
      int x = s->x;

      if (!s->row->reversed_p)
	{
	  int left_x = window_box_left_offset (s->w, TEXT_AREA);

	  if (x < left_x)
	    {
	      background_width -= left_x - x;
	      x = left_x;
	    }
	}
      else
	{
	  /* In R2L rows, draw the cursor on the right edge of the
	     stretch glyph.  */
	  int right_x = window_box_right_offset (s->w, TEXT_AREA);

	  if (x + background_width > right_x)
	    background_width -= x - right_x;
	  x += background_width;
	}
      width = min (FRAME_COLUMN_WIDTH (s->f), background_width);
      if (s->row->reversed_p)
	x -= width;

      /* Draw cursor.  */
      x_draw_glyph_string_bg_rect (s, x, s->y, width, s->height);

      /* Clear rest using the GC of the original non-cursor face.  */
      if (width < background_width)
	{
	  XGCValues *gc = s->face->gc;
	  int y = s->y;
	  int w = background_width - width, h = s->height;
	  RECT r;
          HDC hdc = s->hdc;

	  if (!s->row->reversed_p)
	    x += width;
	  else
	    x = s->x;
	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    {
	      x_set_mouse_face_gc (s);
	      gc = s->gc;
	    }
	  else
	    gc = s->face->gc;

	  get_glyph_string_clip_rect (s, &r);
	  w32_set_clip_rectangle (hdc, &r);

#if 0 /* TODO: stipple */
	  if (s->face->stipple)
	    {
	      /* Fill background with a stipple pattern.  */
	      XSetFillStyle (s->display, gc, FillOpaqueStippled);
	      XFillRectangle (s->display, s->window, gc, x, y, w, h);
	      XSetFillStyle (s->display, gc, FillSolid);
	    }
	  else
#endif
            {
              w32_fill_area (s->f, s->hdc, gc->background, x, y, w, h);
            }
        }
    }
  else if (!s->background_filled_p)
    {
      int background_width = s->background_width;
      int x = s->x, left_x = window_box_left_offset (s->w, TEXT_AREA);

      /* Don't draw into left margin, fringe or scrollbar area
         except for header line and mode line.  */
      if (x < left_x && !s->row->mode_line_p)
	{
	  background_width -= left_x - x;
	  x = left_x;
	}
      if (background_width > 0)
	x_draw_glyph_string_bg_rect (s, x, s->y, background_width, s->height);
    }

  s->background_filled_p = 1;
}


/* Draw glyph string S.  */

static void
x_draw_glyph_string (struct glyph_string *s)
{
  int relief_drawn_p = 0;

  /* If S draws into the background of its successor, draw the
     background of the successor first so that S can draw into it.
     This makes S->next use XDrawString instead of XDrawImageString.  */
  if (s->next && s->right_overhang && !s->for_overlaps)
    {
      int width;
      struct glyph_string *next;
      for (width = 0, next = s->next;
	   next && width < s->right_overhang;
           width += next->width, next = next->next)
        if (next->first_glyph->type != IMAGE_GLYPH)
          {
            x_set_glyph_string_gc (next);
            x_set_glyph_string_clipping (next);
	    if (next->first_glyph->type == STRETCH_GLYPH)
	      x_draw_stretch_glyph_string (next);
	    else
	      x_draw_glyph_string_background (next, 1);
            next->num_clips = 0;
          }
    }

  /* Set up S->gc, set clipping and draw S.  */
  x_set_glyph_string_gc (s);

  /* Draw relief (if any) in advance for char/composition so that the
     glyph string can be drawn over it.  */
  if (!s->for_overlaps
      && s->face->box != FACE_NO_BOX
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))

    {
      x_set_glyph_string_clipping (s);
      x_draw_glyph_string_background (s, 1);
      x_draw_glyph_string_box (s);
      x_set_glyph_string_clipping (s);
      relief_drawn_p = 1;
    }
  else if (!s->clip_head /* draw_glyphs didn't specify a clip mask.  */
           && !s->clip_tail
           && ((s->prev && s->prev->hl != s->hl && s->left_overhang)
               || (s->next && s->next->hl != s->hl && s->right_overhang)))
    /* We must clip just this glyph.  left_overhang part has already
       drawn when s->prev was drawn, and right_overhang part will be
       drawn later when s->next is drawn. */
    x_set_glyph_string_clipping_exactly (s, s);
  else
    x_set_glyph_string_clipping (s);

  switch (s->first_glyph->type)
    {
    case IMAGE_GLYPH:
      x_draw_image_glyph_string (s);
      break;

    case STRETCH_GLYPH:
      x_draw_stretch_glyph_string (s);
      break;

    case CHAR_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = 1;
      else
        x_draw_glyph_string_background (s, 0);
      x_draw_glyph_string_foreground (s);
      break;

    case COMPOSITE_GLYPH:
      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
	s->background_filled_p = 1;
      else
	x_draw_glyph_string_background (s, 1);
      x_draw_composite_glyph_string_foreground (s);
      break;

    case GLYPHLESS_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = 1;
      else
	x_draw_glyph_string_background (s, 0);
      x_draw_glyphless_glyph_string_foreground (s);
      break;

    default:
      abort ();
    }

  if (!s->for_overlaps)
    {
      /* Draw underline.  */
      if (s->face->underline_p)
        {
          unsigned long thickness, position;
          int y;

          if (s->prev && s->prev->face->underline_p)
            {
              /* We use the same underline style as the previous one.  */
              thickness = s->prev->underline_thickness;
              position = s->prev->underline_position;
            }
          else
            {
              /* Get the underline thickness.  Default is 1 pixel.  */
              if (s->font && s->font->underline_thickness > 0)
                thickness = s->font->underline_thickness;
              else
                thickness = 1;
              if (x_underline_at_descent_line)
                position = (s->height - thickness) - (s->ybase - s->y);
              else
                {
                /* Get the underline position.  This is the recommended
                   vertical offset in pixels from the baseline to the top of
                   the underline.  This is a signed value according to the
                   specs, and its default is

                   ROUND ((maximum_descent) / 2), with
                   ROUND (x) = floor (x + 0.5)  */

                if (x_use_underline_position_properties
                    && s->font && s->font->underline_position >= 0)
                  position = s->font->underline_position;
                else if (s->font)
                  position = (s->font->descent + 1) / 2;
                }
	      position = max (position, underline_minimum_offset);
            }
	  /* Check the sanity of thickness and position.  We should
	     avoid drawing underline out of the current line area.  */
	  if (s->y + s->height <= s->ybase + position)
	    position = (s->height - 1) - (s->ybase - s->y);
	  if (s->y + s->height < s->ybase + position + thickness)
	    thickness = (s->y + s->height) - (s->ybase + position);
	  s->underline_thickness = thickness;
	  s->underline_position =position;
          y = s->ybase + position;
          if (s->face->underline_defaulted_p)
            {
              w32_fill_area (s->f, s->hdc, s->gc->foreground, s->x,
                             y, s->width, 1);
            }
          else
            {
              w32_fill_area (s->f, s->hdc, s->face->underline_color, s->x,
                             y, s->width, 1);
            }
        }
      /* Draw overline.  */
      if (s->face->overline_p)
        {
          unsigned long dy = 0, h = 1;

          if (s->face->overline_color_defaulted_p)
            {
              w32_fill_area (s->f, s->hdc, s->gc->foreground, s->x,
                             s->y + dy, s->width, h);
            }
          else
            {
              w32_fill_area (s->f, s->hdc, s->face->overline_color, s->x,
                             s->y + dy, s->width, h);
            }
        }

      /* Draw strike-through.  */
      if (s->face->strike_through_p
          && !FONT_TEXTMETRIC (s->font).tmStruckOut)
        {
          unsigned long h = 1;
          unsigned long dy = (s->height - h) / 2;

          if (s->face->strike_through_color_defaulted_p)
            {
              w32_fill_area (s->f, s->hdc, s->gc->foreground, s->x, s->y + dy,
                             s->width, h);
            }
          else
            {
              w32_fill_area (s->f, s->hdc, s->face->strike_through_color, s->x,
                             s->y + dy, s->width, h);
            }
        }

      /* Draw relief if not yet drawn.  */
      if (!relief_drawn_p && s->face->box != FACE_NO_BOX)
        x_draw_glyph_string_box (s);

      if (s->prev)
        {
          struct glyph_string *prev;

          for (prev = s->prev; prev; prev = prev->prev)
            if (prev->hl != s->hl
                && prev->x + prev->width + prev->right_overhang > s->x)
              {
                /* As prev was drawn while clipped to its own area, we
                   must draw the right_overhang part using s->hl now.  */
		enum draw_glyphs_face save = prev->hl;

		prev->hl = s->hl;
		x_set_glyph_string_gc (prev);
		x_set_glyph_string_clipping_exactly (s, prev);
		if (prev->first_glyph->type == CHAR_GLYPH)
		  x_draw_glyph_string_foreground (prev);
		else
		  x_draw_composite_glyph_string_foreground (prev);
                w32_set_clip_rectangle (prev->hdc, NULL);
		prev->hl = save;
		prev->num_clips = 0;
	      }
	}

      if (s->next)
	{
	  struct glyph_string *next;

	  for (next = s->next; next; next = next->next)
	    if (next->hl != s->hl
		&& next->x - next->left_overhang < s->x + s->width)
	      {
		/* As next will be drawn while clipped to its own area,
		   we must draw the left_overhang part using s->hl now.  */
		enum draw_glyphs_face save = next->hl;

		next->hl = s->hl;
		x_set_glyph_string_gc (next);
		x_set_glyph_string_clipping_exactly (s, next);
		if (next->first_glyph->type == CHAR_GLYPH)
		  x_draw_glyph_string_foreground (next);
		else
		  x_draw_composite_glyph_string_foreground (next);
                w32_set_clip_rectangle (next->hdc, NULL);
		next->hl = save;
		next->num_clips = 0;
	      }
	}
    }

  /* Reset clipping.  */
  w32_set_clip_rectangle (s->hdc, NULL);
  s->num_clips = 0;
}


/* Shift display to make room for inserted glyphs.   */

void
w32_shift_glyphs_for_insert (struct frame *f, int x, int y,
			     int width, int height, int shift_by)
{
  HDC hdc;

  hdc = get_frame_dc (f);
  BitBlt (hdc, x + shift_by, y, width, height,
          hdc, x, y, SRCCOPY);

  release_frame_dc (f, hdc);
}


/* Delete N glyphs at the nominal cursor position.  Not implemented
   for X frames.  */

static void
x_delete_glyphs (struct frame *f, register int n)
{
  if (! FRAME_W32_P (f))
    return;

  abort ();
}


/* Clear entire frame.  */

static void
x_clear_frame (struct frame *f)
{
  if (! FRAME_W32_P (f))
    return;

  /* Clearing the frame will erase any cursor, so mark them all as no
     longer visible.  */
  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));
  output_cursor.hpos = output_cursor.vpos = 0;
  output_cursor.x = -1;

  /* We don't set the output cursor here because there will always
     follow an explicit cursor_to.  */
  BLOCK_INPUT;

  w32_clear_window (f);

  /* We have to clear the scroll bars, too.  If we have changed
     colors or something like that, then they should be notified.  */
  x_scroll_bar_clear (f);

  UNBLOCK_INPUT;
}


/* Make audible bell.  */

static void
w32_ring_bell (struct frame *f)
{
  BLOCK_INPUT;

  if (FRAME_W32_P (f) && visible_bell)
    {
      int i;
      HWND hwnd = FRAME_W32_WINDOW (f);

      for (i = 0; i < 5; i++)
	{
	  FlashWindow (hwnd, TRUE);
	  Sleep (10);
	}
      FlashWindow (hwnd, FALSE);
    }
  else
      w32_sys_ring_bell (f);

  UNBLOCK_INPUT;
}


/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to x_update_begin and x_update_end.  */

static void
w32_set_terminal_window (struct frame *f, int n)
{
  /* This function intentionally left blank.  */
}


/***********************************************************************
			      Line Dance
 ***********************************************************************/

/* Perform an insert-lines or delete-lines operation, inserting N
   lines or deleting -N lines at vertical position VPOS.  */

static void
x_ins_del_lines (struct frame *f, int vpos, int n)
{
  if (! FRAME_W32_P (f))
    return;

  abort ();
}


/* Scroll part of the display as described by RUN.  */

static void
x_scroll_run (struct window *w, struct run *run)
{
  struct frame *f = XFRAME (w->frame);
  int x, y, width, height, from_y, to_y, bottom_y;
  HWND hwnd = FRAME_W32_WINDOW (f);
  HRGN expect_dirty;

  /* Get frame-relative bounding box of the text display area of W,
     without mode lines.  Include in this box the left and right
     fringes of W.  */
  window_box (w, -1, &x, &y, &width, &height);

  /* If the fringe is adjacent to the left (right) scroll bar of a
     leftmost (rightmost, respectively) window, then extend its
     background to the gap between the fringe and the bar.  */
  if ((WINDOW_LEFTMOST_P (w)
       && WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w))
      || (WINDOW_RIGHTMOST_P (w)
	  && WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w)))
    {
      int sb_width = WINDOW_CONFIG_SCROLL_BAR_WIDTH (w);

      if (sb_width > 0)
	{
	  int bar_area_x = WINDOW_SCROLL_BAR_AREA_X (w);
	  int bar_area_width = (WINDOW_CONFIG_SCROLL_BAR_COLS (w)
				* FRAME_COLUMN_WIDTH (f));

	  if (bar_area_x + bar_area_width == x)
	    {
	      x = bar_area_x + sb_width;
	      width += bar_area_width - sb_width;
	    }
	  else if (x + width == bar_area_x)
	    width += bar_area_width - sb_width;
	}
    }

  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->current_y);
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->desired_y);
  bottom_y = y + height;

  if (to_y < from_y)
    {
      /* Scrolling up.  Make sure we don't copy part of the mode
	 line at the bottom.  */
      if (from_y + run->height > bottom_y)
	height = bottom_y - from_y;
      else
	height = run->height;
      expect_dirty = CreateRectRgn (x, y + height, x + width, bottom_y);
    }
  else
    {
      /* Scrolling down.  Make sure we don't copy over the mode line.
	 at the bottom.  */
      if (to_y + run->height > bottom_y)
	height = bottom_y - to_y;
      else
	height = run->height;
      expect_dirty = CreateRectRgn (x, y, x + width, to_y);
    }

  BLOCK_INPUT;

  /* Cursor off.  Will be switched on again in x_update_window_end.  */
  updated_window = w;
  x_clear_cursor (w);

  {
    RECT from;
    RECT to;
    HRGN dirty = CreateRectRgn (0, 0, 0, 0);
    HRGN combined = CreateRectRgn (0, 0, 0, 0);

    from.left = to.left = x;
    from.right = to.right = x + width;
    from.top = from_y;
    from.bottom = from_y + height;
    to.top = y;
    to.bottom = bottom_y;

    ScrollWindowEx (hwnd, 0, to_y - from_y, &from, &to, dirty,
		    NULL, SW_INVALIDATE);

    /* Combine this with what we expect to be dirty. This covers the
       case where not all of the region we expect is actually dirty.  */
    CombineRgn (combined, dirty, expect_dirty, RGN_OR);

    /* If the dirty region is not what we expected, redraw the entire frame.  */
    if (!EqualRgn (combined, expect_dirty))
      SET_FRAME_GARBAGED (f);

    DeleteObject (dirty);
    DeleteObject (combined);
  }

  UNBLOCK_INPUT;
  DeleteObject (expect_dirty);
}



/***********************************************************************
			   Exposure Events
 ***********************************************************************/

static void
frame_highlight (struct frame *f)
{
  x_update_cursor (f, 1);
  x_set_frame_alpha (f);
}

static void
frame_unhighlight (struct frame *f)
{
  x_update_cursor (f, 1);
  x_set_frame_alpha (f);
}

/* The focus has changed.  Update the frames as necessary to reflect
   the new situation.  Note that we can't change the selected frame
   here, because the Lisp code we are interrupting might become confused.
   Each event gets marked with the frame in which it occurred, so the
   Lisp code can tell when the switch took place by examining the events.  */

static void
x_new_focus_frame (struct w32_display_info *dpyinfo, struct frame *frame)
{
  struct frame *old_focus = dpyinfo->w32_focus_frame;

  if (frame != dpyinfo->w32_focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of w32_focus_frame.  */
      dpyinfo->w32_focus_frame = frame;

      if (old_focus && old_focus->auto_lower)
	x_lower_frame (old_focus);

      if (dpyinfo->w32_focus_frame && dpyinfo->w32_focus_frame->auto_raise)
	pending_autoraise_frame = dpyinfo->w32_focus_frame;
      else
	pending_autoraise_frame = 0;
    }

  x_frame_rehighlight (dpyinfo);
}


/* Handle FocusIn and FocusOut state changes for FRAME.
   If FRAME has focus and there exists more than one frame, puts
   a FOCUS_IN_EVENT into *BUFP.  */

static void
x_focus_changed (int type, int state, struct w32_display_info *dpyinfo,
		 struct frame *frame, struct input_event *bufp)
{
  if (type == WM_SETFOCUS)
    {
      if (dpyinfo->w32_focus_event_frame != frame)
        {
          x_new_focus_frame (dpyinfo, frame);
          dpyinfo->w32_focus_event_frame = frame;

          /* Don't stop displaying the initial startup message
             for a switch-frame event we don't need.  */
          if (NILP (Vterminal_frame)
              && CONSP (Vframe_list)
              && !NILP (XCDR (Vframe_list)))
            {
              bufp->kind = FOCUS_IN_EVENT;
              XSETFRAME (bufp->frame_or_window, frame);
            }
        }

      frame->output_data.x->focus_state |= state;

      /* TODO: IME focus?  */
    }
  else if (type == WM_KILLFOCUS)
    {
      frame->output_data.x->focus_state &= ~state;

      if (dpyinfo->w32_focus_event_frame == frame)
        {
          dpyinfo->w32_focus_event_frame = 0;
          x_new_focus_frame (dpyinfo, 0);
        }

      /* TODO: IME focus?  */
    }
}


/* The focus may have changed.  Figure out if it is a real focus change,
   by checking both FocusIn/Out and Enter/LeaveNotify events.

   Returns FOCUS_IN_EVENT event in *BUFP. */

static void
w32_detect_focus_change (struct w32_display_info *dpyinfo, W32Msg *event,
			 struct input_event *bufp)
{
  struct frame *frame;

  frame = x_any_window_to_frame (dpyinfo, event->msg.hwnd);
  if (! frame)
    return;

  /* On w32, this is only called from focus events, so no switch needed.  */
  x_focus_changed (event->msg.message,
		   (event->msg.message == WM_KILLFOCUS ?
		    FOCUS_IMPLICIT : FOCUS_EXPLICIT),
		   dpyinfo, frame, bufp);
}


/* Handle an event saying the mouse has moved out of an Emacs frame.  */

void
x_mouse_leave (struct w32_display_info *dpyinfo)
{
  x_new_focus_frame (dpyinfo, dpyinfo->w32_focus_event_frame);
}

/* The focus has changed, or we have redirected a frame's focus to
   another frame (this happens when a frame uses a surrogate
   mini-buffer frame).  Shift the highlight as appropriate.

   The FRAME argument doesn't necessarily have anything to do with which
   frame is being highlighted or un-highlighted; we only use it to find
   the appropriate X display info.  */

static void
w32_frame_rehighlight (struct frame *frame)
{
  if (! FRAME_W32_P (frame))
    return;
  x_frame_rehighlight (FRAME_W32_DISPLAY_INFO (frame));
}

static void
x_frame_rehighlight (struct w32_display_info *dpyinfo)
{
  struct frame *old_highlight = dpyinfo->x_highlight_frame;

  if (dpyinfo->w32_focus_frame)
    {
      dpyinfo->x_highlight_frame
	= ((FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame))
	   : dpyinfo->w32_focus_frame);
      if (! FRAME_LIVE_P (dpyinfo->x_highlight_frame))
	{
	  FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame) = Qnil;
	  dpyinfo->x_highlight_frame = dpyinfo->w32_focus_frame;
	}
    }
  else
    dpyinfo->x_highlight_frame = 0;

  if (dpyinfo->x_highlight_frame != old_highlight)
    {
      if (old_highlight)
	frame_unhighlight (old_highlight);
      if (dpyinfo->x_highlight_frame)
	frame_highlight (dpyinfo->x_highlight_frame);
    }
}

/* Keyboard processing - modifier keys, etc. */

/* Convert a keysym to its name.  */

char *
x_get_keysym_name (int keysym)
{
  /* Make static so we can always return it */
  static char value[100];

  BLOCK_INPUT;
  GetKeyNameText (keysym, value, 100);
  UNBLOCK_INPUT;

  return value;
}

static int
codepage_for_locale (LCID locale)
{
  char cp[20];

  if (GetLocaleInfo (locale, LOCALE_IDEFAULTANSICODEPAGE, cp, 20) > 0)
    return atoi (cp);
  else
    return CP_ACP;
}


/* Mouse clicks and mouse movement.  Rah.  */

/* Parse a button MESSAGE. The button index is returned in PBUTTON, and
   the state in PUP. XBUTTON provides extra information for extended mouse
   button messages. Returns FALSE if unable to parse the message.  */
BOOL
parse_button (int message, int xbutton, int * pbutton, int * pup)
{
  int button = 0;
  int up = 0;

  switch (message)
    {
    case WM_LBUTTONDOWN:
      button = 0;
      up = 0;
      break;
    case WM_LBUTTONUP:
      button = 0;
      up = 1;
      break;
    case WM_MBUTTONDOWN:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 1;
      else
	button = 2;
      up = 0;
      break;
    case WM_MBUTTONUP:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 1;
      else
	button = 2;
      up = 1;
      break;
    case WM_RBUTTONDOWN:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 2;
      else
	button = 1;
      up = 0;
      break;
    case WM_RBUTTONUP:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 2;
      else
	button = 1;
      up = 1;
      break;
    case WM_XBUTTONDOWN:
      button = xbutton + 2;
      up = 0;
      break;
    case WM_XBUTTONUP:
      button = xbutton + 2;
      up = 1;
      break;
    default:
      return (FALSE);
    }

  if (pup) *pup = up;
  if (pbutton) *pbutton = button;

  return (TRUE);
}


/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.  */

static Lisp_Object
construct_mouse_click (struct input_event *result, W32Msg *msg, struct frame *f)
{
  int button;
  int up;

  parse_button (msg->msg.message, HIWORD (msg->msg.wParam),
		&button, &up);

  /* Make the event type NO_EVENT; we'll change that when we decide
     otherwise.  */
  result->kind = MOUSE_CLICK_EVENT;
  result->code = button;
  result->timestamp = msg->msg.time;
  result->modifiers = (msg->dwModifiers
		       | (up
			  ? up_modifier
			  : down_modifier));

  XSETINT (result->x, LOWORD (msg->msg.lParam));
  XSETINT (result->y, HIWORD (msg->msg.lParam));
  XSETFRAME (result->frame_or_window, f);
  result->arg = Qnil;
  return Qnil;
}

static Lisp_Object
construct_mouse_wheel (struct input_event *result, W32Msg *msg, struct frame *f)
{
  POINT p;
  int delta;

  result->kind = msg->msg.message == WM_MOUSEHWHEEL ? HORIZ_WHEEL_EVENT
                                                    : WHEEL_EVENT;
  result->code = 0;
  result->timestamp = msg->msg.time;

  /* A WHEEL_DELTA positive value indicates that the wheel was rotated
     forward, away from the user (up); a negative value indicates that
     the wheel was rotated backward, toward the user (down).  */
  delta = GET_WHEEL_DELTA_WPARAM (msg->msg.wParam);

  /* The up and down modifiers indicate if the wheel was rotated up or
     down based on WHEEL_DELTA value.  */
  result->modifiers = (msg->dwModifiers
                       | ((delta < 0 ) ? down_modifier : up_modifier));

  /* With multiple monitors, we can legitimately get negative
     coordinates, so cast to short to interpret them correctly.  */
  p.x = (short) LOWORD (msg->msg.lParam);
  p.y = (short) HIWORD (msg->msg.lParam);
  ScreenToClient (msg->msg.hwnd, &p);
  XSETINT (result->x, p.x);
  XSETINT (result->y, p.y);
  XSETFRAME (result->frame_or_window, f);
  result->arg = Qnil;
  return Qnil;
}

static Lisp_Object
construct_drag_n_drop (struct input_event *result, W32Msg *msg, struct frame *f)
{
  Lisp_Object files;
  Lisp_Object frame;
  HDROP hdrop;
  POINT p;
  WORD num_files;
  char *name;
  int i, len;

  result->kind = DRAG_N_DROP_EVENT;
  result->code = 0;
  result->timestamp = msg->msg.time;
  result->modifiers = msg->dwModifiers;

  hdrop = (HDROP) msg->msg.wParam;
  DragQueryPoint (hdrop, &p);

#if 0
  p.x = LOWORD (msg->msg.lParam);
  p.y = HIWORD (msg->msg.lParam);
  ScreenToClient (msg->msg.hwnd, &p);
#endif

  XSETINT (result->x, p.x);
  XSETINT (result->y, p.y);

  num_files = DragQueryFile (hdrop, 0xFFFFFFFF, NULL, 0);
  files = Qnil;

  for (i = 0; i < num_files; i++)
    {
      len = DragQueryFile (hdrop, i, NULL, 0);
      if (len <= 0)
	continue;
      name = alloca (len + 1);
      DragQueryFile (hdrop, i, name, len + 1);
      files = Fcons (DECODE_FILE (build_string (name)), files);
    }

  DragFinish (hdrop);

  XSETFRAME (frame, f);
  result->frame_or_window = frame;
  result->arg = files;
  return Qnil;
}


/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */

static MSG last_mouse_motion_event;
static Lisp_Object last_mouse_motion_frame;

static int
note_mouse_movement (FRAME_PTR frame, MSG *msg)
{
  int mouse_x = LOWORD (msg->lParam);
  int mouse_y = HIWORD (msg->lParam);

  last_mouse_movement_time = msg->time;
  memcpy (&last_mouse_motion_event, msg, sizeof (last_mouse_motion_event));
  XSETFRAME (last_mouse_motion_frame, frame);

  if (!FRAME_X_OUTPUT (frame))
    return 0;

  if (msg->hwnd != FRAME_W32_WINDOW (frame))
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, -1, -1);
      last_mouse_glyph_frame = 0;
      return 1;
    }

  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  if (frame != last_mouse_glyph_frame
      || mouse_x < last_mouse_glyph.left
      || mouse_x >= last_mouse_glyph.right
      || mouse_y < last_mouse_glyph.top
      || mouse_y >= last_mouse_glyph.bottom)
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, mouse_x, mouse_y);
      /* Remember the mouse position here, as w32_mouse_position only
	 gets called when mouse tracking is enabled but we also need
	 to keep track of the mouse for help_echo and highlighting at
	 other times.  */
      remember_mouse_glyph (frame, mouse_x, mouse_y, &last_mouse_glyph);
      last_mouse_glyph_frame = frame;
      return 1;
    }

  return 0;
}


/************************************************************************
			      Mouse Face
 ************************************************************************/

static struct scroll_bar *x_window_to_scroll_bar (Window);
static void x_scroll_bar_report_motion (FRAME_PTR *, Lisp_Object *,
					enum scroll_bar_part *,
					Lisp_Object *, Lisp_Object *,
					unsigned long *);
static void x_check_fullscreen (struct frame *);

static void
redo_mouse_highlight (void)
{
  if (!NILP (last_mouse_motion_frame)
      && FRAME_LIVE_P (XFRAME (last_mouse_motion_frame)))
    note_mouse_highlight (XFRAME (last_mouse_motion_frame),
			  LOWORD (last_mouse_motion_event.lParam),
			  HIWORD (last_mouse_motion_event.lParam));
}

static void
w32_define_cursor (Window window, Cursor cursor)
{
  PostMessage (window, WM_EMACS_SETCURSOR, (WPARAM) cursor, 0);
}
/* Return the current position of the mouse.
   *fp should be a frame which indicates which display to ask about.

   If the mouse movement started in a scroll bar, set *fp, *bar_window,
   and *part to the frame, window, and scroll bar part that the mouse
   is over.  Set *x and *y to the portion and whole of the mouse's
   position on the scroll bar.

   If the mouse movement started elsewhere, set *fp to the frame the
   mouse is on, *bar_window to nil, and *x and *y to the character cell
   the mouse is over.

   Set *time to the server time-stamp for the time at which the mouse
   was at this position.

   Don't store anything if we don't have a valid set of values to report.

   This clears the mouse_moved flag, so we can wait for the next mouse
   movement.  */

static void
w32_mouse_position (FRAME_PTR *fp, int insist, Lisp_Object *bar_window,
		    enum scroll_bar_part *part, Lisp_Object *x, Lisp_Object *y,
		    unsigned long *time)
{
  FRAME_PTR f1;

  BLOCK_INPUT;

  if (! NILP (last_mouse_scroll_bar) && insist == 0)
    x_scroll_bar_report_motion (fp, bar_window, part, x, y, time);
  else
    {
      POINT pt;

      Lisp_Object frame, tail;

      /* Clear the mouse-moved flag for every frame on this display.  */
      FOR_EACH_FRAME (tail, frame)
	XFRAME (frame)->mouse_moved = 0;

      last_mouse_scroll_bar = Qnil;

      GetCursorPos (&pt);

      /* Now we have a position on the root; find the innermost window
	 containing the pointer.  */
      {
	if (FRAME_W32_DISPLAY_INFO (*fp)->grabbed && last_mouse_frame
	    && FRAME_LIVE_P (last_mouse_frame))
	  {
	    /* If mouse was grabbed on a frame, give coords for that frame
	       even if the mouse is now outside it.  */
	    f1 = last_mouse_frame;
	  }
	else
	  {
	    /* Is window under mouse one of our frames?  */
	    f1 = x_any_window_to_frame (FRAME_W32_DISPLAY_INFO (*fp),
                                    WindowFromPoint (pt));
	  }

	/* If not, is it one of our scroll bars?  */
	if (! f1)
	  {
	    struct scroll_bar *bar
              = x_window_to_scroll_bar (WindowFromPoint (pt));

	    if (bar)
	      {
		f1 = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
	      }
	  }

	if (f1 == 0 && insist > 0)
	  f1 = SELECTED_FRAME ();

	if (f1)
	  {
	    /* Ok, we found a frame.  Store all the values.
	       last_mouse_glyph is a rectangle used to reduce the
	       generation of mouse events.  To not miss any motion
	       events, we must divide the frame into rectangles of the
	       size of the smallest character that could be displayed
	       on it, i.e. into the same rectangles that matrices on
	       the frame are divided into.  */

	    ScreenToClient (FRAME_W32_WINDOW (f1), &pt);
	    remember_mouse_glyph (f1, pt.x, pt.y, &last_mouse_glyph);
	    last_mouse_glyph_frame = f1;

	    *bar_window = Qnil;
	    *part = 0;
	    *fp = f1;
	    XSETINT (*x, pt.x);
	    XSETINT (*y, pt.y);
	    *time = last_mouse_movement_time;
	  }
      }
    }

  UNBLOCK_INPUT;
}


/***********************************************************************
			       Tool-bars
 ***********************************************************************/

/* Handle mouse button event on the tool-bar of frame F, at
   frame-relative coordinates X/Y.  EVENT_TYPE is either ButtonPress
   or ButtonRelease.  */

static void
w32_handle_tool_bar_click (struct frame *f, struct input_event *button_event)
{
  int x = XFASTINT (button_event->x);
  int y = XFASTINT (button_event->y);

  if (button_event->modifiers & down_modifier)
    handle_tool_bar_click (f, x, y, 1, 0);
  else
    handle_tool_bar_click (f, x, y, 0,
			   button_event->modifiers & ~up_modifier);
}



/***********************************************************************
			       Scroll bars
 ***********************************************************************/

/* Scroll bar support.  */

/* Given a window ID, find the struct scroll_bar which manages it.
   This can be called in GC, so we have to make sure to strip off mark
   bits.  */

static struct scroll_bar *
x_window_to_scroll_bar (Window window_id)
{
  Lisp_Object tail;

  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object frame, bar, condemned;

      frame = XCAR (tail);
      /* All elements of Vframe_list should be frames.  */
      if (! FRAMEP (frame))
	abort ();

      /* Scan this frame's scroll bar list for a scroll bar with the
	 right window ID.  */
      condemned = FRAME_CONDEMNED_SCROLL_BARS (XFRAME (frame));
      for (bar = FRAME_SCROLL_BARS (XFRAME (frame));
	   /* This trick allows us to search both the ordinary and
	      condemned scroll bar lists with one loop.  */
	   ! NILP (bar) || (bar = condemned,
			       condemned = Qnil,
			       ! NILP (bar));
	   bar = XSCROLL_BAR (bar)->next)
	if (SCROLL_BAR_W32_WINDOW (XSCROLL_BAR (bar)) == window_id)
	  return XSCROLL_BAR (bar);
    }

  return 0;
}



/* Set the thumb size and position of scroll bar BAR.  We are currently
   displaying PORTION out of a whole WHOLE, and our position POSITION.  */

static void
w32_set_scroll_bar_thumb (struct scroll_bar *bar,
			  int portion, int position, int whole)
{
  Window w = SCROLL_BAR_W32_WINDOW (bar);
  /* We use the whole scroll-bar height in the calculations below, to
     avoid strange effects like scrolling backwards when just clicking
     on the handle (without moving it).  */
  double range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height))
                 + VERTICAL_SCROLL_BAR_MIN_HANDLE;
  int sb_page, sb_pos;
  BOOL draggingp = !NILP (bar->dragging) ? TRUE : FALSE;
  SCROLLINFO si;

  /* We used to change the nPage setting while dragging the handle,
     but that had very strange effects (such as scrolling backwards
     while dragging downwards).

     Now, we don't change the nPage setting while dragging unless we
     get near to the end of the buffer, in which case we often have to
     resize the handle to "go all the way".  */

  if (draggingp)
    {
      int near_bottom_p;
      BLOCK_INPUT;
      si.cbSize = sizeof (si);
      si.fMask = SIF_POS | SIF_PAGE;
      GetScrollInfo (w, SB_CTL, &si);
      near_bottom_p = si.nPos + si.nPage >= range;
      UNBLOCK_INPUT;
      if (!near_bottom_p)
	return;
    }

  if (whole)
    {
      /* Position scroll bar at rock bottom if the bottom of the
         buffer is visible. This avoids shrinking the thumb away
         to nothing if it is held at the bottom of the buffer.  */
      if (position + portion >= whole && !draggingp)
	{
	  sb_page = range * (whole - position) / whole;
	  sb_pos = range;
	}
      else
	{
	  sb_pos = position * range / whole;
	  sb_page = (min (portion, (whole - position)) * range) / whole;
	}
    }
  else
    {
      sb_page = range;
      sb_pos = 0;
    }

  sb_page = max (sb_page, VERTICAL_SCROLL_BAR_MIN_HANDLE);

  BLOCK_INPUT;

  si.cbSize = sizeof (si);
  si.fMask = SIF_PAGE | SIF_POS;
  si.nPage = sb_page;
  si.nPos = sb_pos;

  SetScrollInfo (w, SB_CTL, &si, TRUE);

  UNBLOCK_INPUT;
}


/************************************************************************
			 Scroll bars, general
 ************************************************************************/

static HWND
my_create_scrollbar (struct frame * f, struct scroll_bar * bar)
{
  return (HWND) SendMessage (FRAME_W32_WINDOW (f),
			     WM_EMACS_CREATESCROLLBAR, (WPARAM) f,
			     (LPARAM) bar);
}

/*#define ATTACH_THREADS*/

static BOOL
my_show_window (FRAME_PTR f, HWND hwnd, int how)
{
#ifndef ATTACH_THREADS
  return SendMessage (FRAME_W32_WINDOW (f), WM_EMACS_SHOWWINDOW,
		      (WPARAM) hwnd, (LPARAM) how);
#else
  return ShowWindow (hwnd, how);
#endif
}

static void
my_set_window_pos (HWND hwnd, HWND hwndAfter,
		   int x, int y, int cx, int cy, UINT flags)
{
#ifndef ATTACH_THREADS
  WINDOWPOS pos;
  pos.hwndInsertAfter = hwndAfter;
  pos.x = x;
  pos.y = y;
  pos.cx = cx;
  pos.cy = cy;
  pos.flags = flags;
  SendMessage (hwnd, WM_EMACS_SETWINDOWPOS, (WPARAM) &pos, 0);
#else
  SetWindowPos (hwnd, hwndAfter, x, y, cx, cy, flags);
#endif
}

#if 0
static void
my_set_focus (struct frame * f, HWND hwnd)
{
  SendMessage (FRAME_W32_WINDOW (f), WM_EMACS_SETFOCUS,
	       (WPARAM) hwnd, 0);
}
#endif

static void
my_set_foreground_window (HWND hwnd)
{
  SendMessage (hwnd, WM_EMACS_SETFOREGROUND, (WPARAM) hwnd, 0);
}


static void
my_destroy_window (struct frame * f, HWND hwnd)
{
  SendMessage (FRAME_W32_WINDOW (f), WM_EMACS_DESTROYWINDOW,
	       (WPARAM) hwnd, 0);
}

/* Create a scroll bar and return the scroll bar vector for it.  W is
   the Emacs window on which to create the scroll bar. TOP, LEFT,
   WIDTH and HEIGHT are the pixel coordinates and dimensions of the
   scroll bar. */

static struct scroll_bar *
x_scroll_bar_create (struct window *w, int top, int left, int width, int height)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  HWND hwnd;
  SCROLLINFO si;
  struct scroll_bar *bar
    = XSCROLL_BAR (Fmake_vector (make_number (SCROLL_BAR_VEC_SIZE), Qnil));

  BLOCK_INPUT;

  XSETWINDOW (bar->window, w);
  XSETINT (bar->top, top);
  XSETINT (bar->left, left);
  XSETINT (bar->width, width);
  XSETINT (bar->height, height);
  XSETINT (bar->start, 0);
  XSETINT (bar->end, 0);
  bar->dragging = Qnil;
  bar->fringe_extended_p = Qnil;

  /* Requires geometry to be set before call to create the real window */

  hwnd = my_create_scrollbar (f, bar);

  si.cbSize = sizeof (si);
  si.fMask = SIF_ALL;
  si.nMin = 0;
  si.nMax = VERTICAL_SCROLL_BAR_TOP_RANGE (f, height)
    + VERTICAL_SCROLL_BAR_MIN_HANDLE;
  si.nPage = si.nMax;
  si.nPos = 0;

  SetScrollInfo (hwnd, SB_CTL, &si, FALSE);

  SET_SCROLL_BAR_W32_WINDOW (bar, hwnd);

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
  if (! NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  UNBLOCK_INPUT;

  return bar;
}


/* Destroy scroll bar BAR, and set its Emacs window's scroll bar to
   nil. */

static void
x_scroll_bar_remove (struct scroll_bar *bar)
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  BLOCK_INPUT;

  /* Destroy the window.  */
  my_destroy_window (f, SCROLL_BAR_W32_WINDOW (bar));

  /* Dissociate this scroll bar from its window.  */
  XWINDOW (bar->window)->vertical_scroll_bar = Qnil;

  UNBLOCK_INPUT;
}

/* Set the handle of the vertical scroll bar for WINDOW to indicate
   that we are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  If WINDOW has no scroll bar,
   create one.  */
static void
w32_set_vertical_scroll_bar (struct window *w,
			     int portion, int whole, int position)
{
  struct frame *f = XFRAME (w->frame);
  struct scroll_bar *bar;
  int top, height, left, sb_left, width, sb_width;
  int window_y, window_height;
  int fringe_extended_p;

  /* Get window dimensions.  */
  window_box (w, -1, 0, &window_y, 0, &window_height);
  top  = window_y;
  width = WINDOW_CONFIG_SCROLL_BAR_COLS (w) * FRAME_COLUMN_WIDTH (f);
  height = window_height;

  /* Compute the left edge of the scroll bar area.  */
  left = WINDOW_SCROLL_BAR_AREA_X (w);

  /* Compute the width of the scroll bar which might be less than
     the width of the area reserved for the scroll bar.  */
  if (WINDOW_CONFIG_SCROLL_BAR_WIDTH (w) > 0)
    sb_width = WINDOW_CONFIG_SCROLL_BAR_WIDTH (w);
  else
    sb_width = width;

  /* Compute the left edge of the scroll bar.  */
  if (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w))
    sb_left = left + (WINDOW_RIGHTMOST_P (w) ? width - sb_width : 0);
  else
    sb_left = left + (WINDOW_LEFTMOST_P (w) ? 0 : width - sb_width);

  if (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w))
    fringe_extended_p = (WINDOW_LEFTMOST_P (w)
			 && WINDOW_LEFT_FRINGE_WIDTH (w)
			 && (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
			     || WINDOW_LEFT_MARGIN_COLS (w) == 0));
  else
    fringe_extended_p = (WINDOW_RIGHTMOST_P (w)
			 && WINDOW_RIGHT_FRINGE_WIDTH (w)
			 && (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
			     || WINDOW_RIGHT_MARGIN_COLS (w) == 0));

  /* Does the scroll bar exist yet?  */
  if (NILP (w->vertical_scroll_bar))
    {
      HDC hdc;
      BLOCK_INPUT;
      if (width > 0 && height > 0)
	{
	  hdc = get_frame_dc (f);
	  if (fringe_extended_p)
	    w32_clear_area (f, hdc, sb_left, top, sb_width, height);
	  else
	    w32_clear_area (f, hdc, left, top, width, height);
	  release_frame_dc (f, hdc);
	}
      UNBLOCK_INPUT;

      bar = x_scroll_bar_create (w, top, sb_left, sb_width, height);
    }
  else
    {
      /* It may just need to be moved and resized.  */
      HWND hwnd;

      bar = XSCROLL_BAR (w->vertical_scroll_bar);
      hwnd = SCROLL_BAR_W32_WINDOW (bar);

      /* If already correctly positioned, do nothing.  */
      if ( XINT (bar->left) == sb_left
           && XINT (bar->top) == top
           && XINT (bar->width) ==  sb_width
           && XINT (bar->height) == height
	   && !NILP (bar->fringe_extended_p) == fringe_extended_p )
        {
          /* Redraw after clear_frame. */
          if (!my_show_window (f, hwnd, SW_NORMAL))
            InvalidateRect (hwnd, NULL, FALSE);
        }
      else
        {
          HDC hdc;
	  SCROLLINFO si;

          BLOCK_INPUT;
	  if (width && height)
	    {
	      hdc = get_frame_dc (f);
	      /* Since Windows scroll bars are smaller than the space reserved
		 for them on the frame, we have to clear "under" them.  */
	      if (fringe_extended_p)
		w32_clear_area (f, hdc, sb_left, top, sb_width, height);
	      else
		w32_clear_area (f, hdc, left, top, width, height);
	      release_frame_dc (f, hdc);
	    }
          /* Make sure scroll bar is "visible" before moving, to ensure the
             area of the parent window now exposed will be refreshed.  */
          my_show_window (f, hwnd, SW_HIDE);
          MoveWindow (hwnd, sb_left + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
		      top, sb_width - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
		      max (height, 1), TRUE);

	  si.cbSize = sizeof (si);
	  si.fMask = SIF_RANGE;
	  si.nMin = 0;
	  si.nMax = VERTICAL_SCROLL_BAR_TOP_RANGE (f, height)
	    + VERTICAL_SCROLL_BAR_MIN_HANDLE;

	  SetScrollInfo (hwnd, SB_CTL, &si, FALSE);

          my_show_window (f, hwnd, SW_NORMAL);
          /* InvalidateRect (w, NULL, FALSE);  */

          /* Remember new settings.  */
          XSETINT (bar->left, sb_left);
          XSETINT (bar->top, top);
          XSETINT (bar->width, sb_width);
          XSETINT (bar->height, height);

          UNBLOCK_INPUT;
        }
    }
  bar->fringe_extended_p = fringe_extended_p ? Qt : Qnil;

  w32_set_scroll_bar_thumb (bar, portion, position, whole);

  XSETVECTOR (w->vertical_scroll_bar, bar);
}


/* The following three hooks are used when we're doing a thorough
   redisplay of the frame.  We don't explicitly know which scroll bars
   are going to be deleted, because keeping track of when windows go
   away is a real pain - "Can you say set-window-configuration, boys
   and girls?"  Instead, we just assert at the beginning of redisplay
   that *all* scroll bars are to be removed, and then save a scroll bar
   from the fiery pit when we actually redisplay its window.  */

/* Arrange for all scroll bars on FRAME to be removed at the next call
   to `*judge_scroll_bars_hook'.  A scroll bar may be spared if
   `*redeem_scroll_bar_hook' is applied to its window before the judgment.  */

static void
w32_condemn_scroll_bars (FRAME_PTR frame)
{
  /* Transfer all the scroll bars to FRAME_CONDEMNED_SCROLL_BARS.  */
  while (! NILP (FRAME_SCROLL_BARS (frame)))
    {
      Lisp_Object bar;
      bar = FRAME_SCROLL_BARS (frame);
      FRAME_SCROLL_BARS (frame) = XSCROLL_BAR (bar)->next;
      XSCROLL_BAR (bar)->next = FRAME_CONDEMNED_SCROLL_BARS (frame);
      XSCROLL_BAR (bar)->prev = Qnil;
      if (! NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
	XSCROLL_BAR (FRAME_CONDEMNED_SCROLL_BARS (frame))->prev = bar;
      FRAME_CONDEMNED_SCROLL_BARS (frame) = bar;
    }
}


/* Un-mark WINDOW's scroll bar for deletion in this judgment cycle.
   Note that WINDOW isn't necessarily condemned at all.  */

static void
w32_redeem_scroll_bar (struct window *window)
{
  struct scroll_bar *bar;
  struct frame *f;

  /* We can't redeem this window's scroll bar if it doesn't have one.  */
  if (NILP (window->vertical_scroll_bar))
    abort ();

  bar = XSCROLL_BAR (window->vertical_scroll_bar);

  /* Unlink it from the condemned list.  */
  f = XFRAME (WINDOW_FRAME (window));
  if (NILP (bar->prev))
    {
      /* If the prev pointer is nil, it must be the first in one of
         the lists.  */
      if (EQ (FRAME_SCROLL_BARS (f), window->vertical_scroll_bar))
        /* It's not condemned.  Everything's fine.  */
        return;
      else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
                   window->vertical_scroll_bar))
        FRAME_CONDEMNED_SCROLL_BARS (f) = bar->next;
      else
        /* If its prev pointer is nil, it must be at the front of
           one or the other!  */
        abort ();
    }
  else
    XSCROLL_BAR (bar->prev)->next = bar->next;

  if (! NILP (bar->next))
    XSCROLL_BAR (bar->next)->prev = bar->prev;

  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
  if (! NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
}

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  */

static void
w32_judge_scroll_bars (FRAME_PTR f)
{
  Lisp_Object bar, next;

  bar = FRAME_CONDEMNED_SCROLL_BARS (f);

  /* Clear out the condemned list now so we won't try to process any
     more events on the hapless scroll bars.  */
  FRAME_CONDEMNED_SCROLL_BARS (f) = Qnil;

  for (; ! NILP (bar); bar = next)
    {
      struct scroll_bar *b = XSCROLL_BAR (bar);

      x_scroll_bar_remove (b);

      next = b->next;
      b->next = b->prev = Qnil;
    }

  /* Now there should be no references to the condemned scroll bars,
     and they should get garbage-collected.  */
}

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than NO_EVENT, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static int
w32_scroll_bar_handle_click (struct scroll_bar *bar, W32Msg *msg,
			     struct input_event *emacs_event)
{
  if (! WINDOWP (bar->window))
    abort ();

  emacs_event->kind = SCROLL_BAR_CLICK_EVENT;
  emacs_event->code = 0;
  /* not really meaningful to distinguish up/down */
  emacs_event->modifiers = msg->dwModifiers;
  emacs_event->frame_or_window = bar->window;
  emacs_event->arg = Qnil;
  emacs_event->timestamp = msg->msg.time;

  {
    int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));
    int y;
    int dragging = !NILP (bar->dragging);
    SCROLLINFO si;

    si.cbSize = sizeof (si);
    si.fMask = SIF_POS;

    GetScrollInfo ((HWND) msg->msg.lParam, SB_CTL, &si);
    y = si.nPos;

    bar->dragging = Qnil;


    last_mouse_scroll_bar_pos = msg->msg.wParam;

    switch (LOWORD (msg->msg.wParam))
      {
      case SB_LINEDOWN:
	emacs_event->part = scroll_bar_down_arrow;
	break;
      case SB_LINEUP:
	emacs_event->part = scroll_bar_up_arrow;
	break;
      case SB_PAGEUP:
	emacs_event->part = scroll_bar_above_handle;
	break;
      case SB_PAGEDOWN:
	emacs_event->part = scroll_bar_below_handle;
	break;
      case SB_TOP:
	emacs_event->part = scroll_bar_handle;
	y = 0;
	break;
      case SB_BOTTOM:
	emacs_event->part = scroll_bar_handle;
	y = top_range;
	break;
      case SB_THUMBTRACK:
      case SB_THUMBPOSITION:
	if (VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height)) <= 0xffff)
          y = HIWORD (msg->msg.wParam);
	bar->dragging = Qt;
	emacs_event->part = scroll_bar_handle;

	/* "Silently" update current position.  */
	{
	  SCROLLINFO si;

	  si.cbSize = sizeof (si);
	  si.fMask = SIF_POS;
	  si.nPos = y;
	  /* Remember apparent position (we actually lag behind the real
	     position, so don't set that directly).  */
	  last_scroll_bar_drag_pos = y;

	  SetScrollInfo (SCROLL_BAR_W32_WINDOW (bar), SB_CTL, &si, FALSE);
	}
	break;
      case SB_ENDSCROLL:
	/* If this is the end of a drag sequence, then reset the scroll
	   handle size to normal and do a final redraw.  Otherwise do
	   nothing.  */
	if (dragging)
	  {
	    SCROLLINFO si;
	    int start = XINT (bar->start);
	    int end = XINT (bar->end);

	    si.cbSize = sizeof (si);
	    si.fMask = SIF_PAGE | SIF_POS;
	    si.nPage = end - start + VERTICAL_SCROLL_BAR_MIN_HANDLE;
	    si.nPos = last_scroll_bar_drag_pos;
	    SetScrollInfo (SCROLL_BAR_W32_WINDOW (bar), SB_CTL, &si, TRUE);
	  }
	/* fall through */
      default:
	emacs_event->kind = NO_EVENT;
	return FALSE;
      }

    XSETINT (emacs_event->x, y);
    XSETINT (emacs_event->y, top_range);

    return TRUE;
  }
}

/* Return information to the user about the current position of the mouse
   on the scroll bar.  */

static void
x_scroll_bar_report_motion (FRAME_PTR *fp, Lisp_Object *bar_window,
			    enum scroll_bar_part *part,
			    Lisp_Object *x, Lisp_Object *y,
			    unsigned long *time)
{
  struct scroll_bar *bar = XSCROLL_BAR (last_mouse_scroll_bar);
  Window w = SCROLL_BAR_W32_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  int pos;
  int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));
  SCROLLINFO si;

  BLOCK_INPUT;

  *fp = f;
  *bar_window = bar->window;

  si.cbSize = sizeof (si);
  si.fMask = SIF_POS | SIF_PAGE | SIF_RANGE;

  GetScrollInfo (w, SB_CTL, &si);
  pos = si.nPos;
  top_range = si.nMax - si.nPage + 1;

  switch (LOWORD (last_mouse_scroll_bar_pos))
  {
  case SB_THUMBPOSITION:
  case SB_THUMBTRACK:
      *part = scroll_bar_handle;
      if (VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height)) <= 0xffff)
	  pos = HIWORD (last_mouse_scroll_bar_pos);
      break;
  case SB_LINEDOWN:
      *part = scroll_bar_handle;
      pos++;
      break;
  default:
      *part = scroll_bar_handle;
      break;
  }

  XSETINT (*x, pos);
  XSETINT (*y, top_range);

  f->mouse_moved = 0;
  last_mouse_scroll_bar = Qnil;

  *time = last_mouse_movement_time;

  UNBLOCK_INPUT;
}


/* The screen has been cleared so we may have changed foreground or
   background colors, and the scroll bars may need to be redrawn.
   Clear out the scroll bars, and ask for expose events, so we can
   redraw them.  */

void
x_scroll_bar_clear (FRAME_PTR f)
{
  Lisp_Object bar;

  /* We can have scroll bars even if this is 0,
     if we just turned off scroll bar mode.
     But in that case we should not clear them.  */
  if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    for (bar = FRAME_SCROLL_BARS (f); VECTORP (bar);
         bar = XSCROLL_BAR (bar)->next)
      {
        HWND window = SCROLL_BAR_W32_WINDOW (XSCROLL_BAR (bar));
        HDC hdc = GetDC (window);
        RECT rect;

        /* Hide scroll bar until ready to repaint.  x_scroll_bar_move
           arranges to refresh the scroll bar if hidden.  */
        my_show_window (f, window, SW_HIDE);

        GetClientRect (window, &rect);
        select_palette (f, hdc);
        w32_clear_rect (f, hdc, &rect);
        deselect_palette (f, hdc);

        ReleaseDC (window, hdc);
      }
}


/* The main W32 event-reading loop - w32_read_socket.  */

/* Record the last 100 characters stored
   to help debug the loss-of-chars-during-GC problem.  */

static int temp_index;
static short temp_buffer[100];

/* Temporarily store lead byte of DBCS input sequences.  */
static char dbcs_lead = 0;

/* Read events coming from the W32 shell.
   This routine is called by the SIGIO handler.
   We return as soon as there are no more events to be read.

   We return the number of characters stored into the buffer,
   thus pretending to be `read'.

   EXPECTED is nonzero if the caller knows input is available.

   Some of these messages are reposted back to the message queue since the
   system calls the windows proc directly in a context where we cannot return
   the data nor can we guarantee the state we are in.  So if we dispatch them
   we will get into an infinite loop.  To prevent this from ever happening we
   will set a variable to indicate we are in the read_socket call and indicate
   which message we are processing since the windows proc gets called
   recursively with different messages by the system.
*/

static int
w32_read_socket (struct terminal *terminal, int expected,
		 struct input_event *hold_quit)
{
  int count = 0;
  int check_visibility = 0;
  W32Msg msg;
  struct frame *f;
  struct w32_display_info *dpyinfo = &one_w32_display_info;
  Mouse_HLInfo *hlinfo = &dpyinfo->mouse_highlight;

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }

  interrupt_input_pending = 0;
  BLOCK_INPUT;

  /* So people can tell when we have read the available input.  */
  input_signal_count++;

  /* TODO: ghostscript integration. */
  while (get_next_msg (&msg, FALSE))
    {
      struct input_event inev;
      int do_help = 0;

      EVENT_INIT (inev);
      inev.kind = NO_EVENT;
      inev.arg = Qnil;

      switch (msg.msg.message)
	{
	case WM_EMACS_PAINT:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      if (msg.rect.right == msg.rect.left ||
		  msg.rect.bottom == msg.rect.top)
		{
		  /* We may get paint messages even though the client
		     area is clipped - these are not expose events. */
		  DebPrint (("clipped frame %p (%s) got WM_PAINT - ignored\n", f,
			     SDATA (f->name)));
		}
	      else if (f->async_visible != 1)
		{
		  /* Definitely not obscured, so mark as visible.  */
		  f->async_visible = 1;
		  f->async_iconified = 0;
		  SET_FRAME_GARBAGED (f);
		  DebPrint (("frame %p (%s) reexposed by WM_PAINT\n", f,
			     SDATA (f->name)));

		  /* WM_PAINT serves as MapNotify as well, so report
		     visibility changes properly.  */
		  if (f->iconified)
		    {
		      inev.kind = DEICONIFY_EVENT;
		      XSETFRAME (inev.frame_or_window, f);
		    }
		  else if (! NILP (Vframe_list)
			   && ! NILP (XCDR (Vframe_list)))
		    /* Force a redisplay sooner or later to update the
		       frame titles in case this is the second frame.  */
		    record_asynch_buffer_change ();
		}
	      else
		{
		  HDC hdc = get_frame_dc (f);

		  /* Erase background again for safety.  */
		  w32_clear_rect (f, hdc, &msg.rect);
		  release_frame_dc (f, hdc);
		  expose_frame (f,
				msg.rect.left,
				msg.rect.top,
				msg.rect.right - msg.rect.left,
				msg.rect.bottom - msg.rect.top);
		}
	    }
	  break;

	case WM_INPUTLANGCHANGE:
	  /* Generate a language change event.  */
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  /* lParam contains the input lang ID.  Use it to update our
	     record of the keyboard codepage.  */
	  keyboard_codepage = codepage_for_locale ((LCID)(msg.msg.lParam
							  & 0xffff));

	  if (f)
	    {
	      inev.kind = LANGUAGE_CHANGE_EVENT;
	      XSETFRAME (inev.frame_or_window, f);
	      inev.code = msg.msg.wParam;
	      inev.modifiers = msg.msg.lParam & 0xffff;
	    }
	  break;

	case WM_KEYDOWN:
	case WM_SYSKEYDOWN:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f && !f->iconified)
	    {
	      if (!hlinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight)
		  && !EQ (f->tool_bar_window, hlinfo->mouse_face_window))
		{
		  clear_mouse_face (hlinfo);
		  hlinfo->mouse_face_hidden = 1;
		}

	      if (temp_index == sizeof temp_buffer / sizeof (short))
		temp_index = 0;
	      temp_buffer[temp_index++] = msg.msg.wParam;
	      inev.kind = NON_ASCII_KEYSTROKE_EVENT;
	      inev.code = msg.msg.wParam;
	      inev.modifiers = msg.dwModifiers;
	      XSETFRAME (inev.frame_or_window, f);
	      inev.timestamp = msg.msg.time;
	    }
	  break;

        case WM_UNICHAR:
	case WM_SYSCHAR:
	case WM_CHAR:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f && !f->iconified)
	    {
	      if (!hlinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight)
		  && !EQ (f->tool_bar_window, hlinfo->mouse_face_window))
		{
		  clear_mouse_face (hlinfo);
		  hlinfo->mouse_face_hidden = 1;
		}

	      if (temp_index == sizeof temp_buffer / sizeof (short))
		temp_index = 0;
	      temp_buffer[temp_index++] = msg.msg.wParam;

	      inev.modifiers = msg.dwModifiers;
	      XSETFRAME (inev.frame_or_window, f);
	      inev.timestamp = msg.msg.time;

              if (msg.msg.message == WM_UNICHAR)
                {
                  inev.code = msg.msg.wParam;
                }
              else if (msg.msg.wParam < 256)
                {
                  wchar_t code;
                  char dbcs[2];
                  dbcs[0] = 0;
                  dbcs[1] = (char) msg.msg.wParam;

                  if (dbcs_lead)
                    {
                      dbcs[0] = dbcs_lead;
                      dbcs_lead = 0;
                      if (!MultiByteToWideChar (keyboard_codepage, 0,
						dbcs, 2, &code, 1))
                        {
                          /* Garbage */
                          DebPrint (("Invalid DBCS sequence: %d %d\n",
                                     dbcs[0], dbcs[1]));
                          inev.kind = NO_EVENT;
                          break;
                        }
                    }
                  else if (IsDBCSLeadByteEx (keyboard_codepage,
					     (BYTE) msg.msg.wParam))
                    {
                      dbcs_lead = (char) msg.msg.wParam;
                      inev.kind = NO_EVENT;
                      break;
                    }
                  else
                    {
                      if (!MultiByteToWideChar (keyboard_codepage, 0,
						&dbcs[1], 1, &code, 1))
                        {
                          /* What to do with garbage? */
                          DebPrint (("Invalid character: %d\n", dbcs[1]));
                          inev.kind = NO_EVENT;
                          break;
                        }
                    }
                  inev.code = code;
                }
              else
                {
                  /* Windows shouldn't generate WM_CHAR events above 0xFF
                     in non-Unicode message handlers.  */
                  DebPrint (("Non-byte WM_CHAR: %d\n", msg.msg.wParam));
                  inev.kind = NO_EVENT;
                  break;
                }
              inev.kind = inev.code < 128 ? ASCII_KEYSTROKE_EVENT
                                          : MULTIBYTE_CHAR_KEYSTROKE_EVENT;
	    }
	  break;

        case WM_APPCOMMAND:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f && !f->iconified)
	    {
	      if (!hlinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight)
		  && !EQ (f->tool_bar_window, hlinfo->mouse_face_window))
		{
		  clear_mouse_face (hlinfo);
		  hlinfo->mouse_face_hidden = 1;
		}

	      if (temp_index == sizeof temp_buffer / sizeof (short))
		temp_index = 0;
	      temp_buffer[temp_index++] = msg.msg.wParam;
	      inev.kind = MULTIMEDIA_KEY_EVENT;
	      inev.code = GET_APPCOMMAND_LPARAM (msg.msg.lParam);
	      inev.modifiers = msg.dwModifiers;
	      XSETFRAME (inev.frame_or_window, f);
	      inev.timestamp = msg.msg.time;
	    }
	  break;

	case WM_MOUSEMOVE:
	  /* Ignore non-movement.  */
	  {
	    int x = LOWORD (msg.msg.lParam);
	    int y = HIWORD (msg.msg.lParam);
	    if (x == last_mousemove_x && y == last_mousemove_y)
	      break;
	    last_mousemove_x = x;
	    last_mousemove_y = y;
	  }

          previous_help_echo_string = help_echo_string;
	  help_echo_string = Qnil;

	  if (dpyinfo->grabbed && last_mouse_frame
	      && FRAME_LIVE_P (last_mouse_frame))
	    f = last_mouse_frame;
	  else
	    f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (hlinfo->mouse_face_hidden)
	    {
	      hlinfo->mouse_face_hidden = 0;
	      clear_mouse_face (hlinfo);
	    }

	  if (f)
	    {
	      /* Generate SELECT_WINDOW_EVENTs when needed.  */
	      if (!NILP (Vmouse_autoselect_window))
		{
		  Lisp_Object window;
		  int x = LOWORD (msg.msg.lParam);
		  int y = HIWORD (msg.msg.lParam);

		  window = window_from_coordinates (f, x, y, 0, 0);

		  /* Window will be selected only when it is not
		     selected now and last mouse movement event was
		     not in it.  Minibuffer window will be selected
		     only when it is active.  */
		  if (WINDOWP (window)
		      && !EQ (window, last_window)
		      && !EQ (window, selected_window)
		      /* For click-to-focus window managers
			 create event iff we don't leave the
			 selected frame.  */
		      && (focus_follows_mouse
			  || (EQ (XWINDOW (window)->frame,
				  XWINDOW (selected_window)->frame))))
		    {
		      inev.kind = SELECT_WINDOW_EVENT;
		      inev.frame_or_window = window;
		    }

		  last_window = window;
		}
	      if (!note_mouse_movement (f, &msg.msg))
		help_echo_string = previous_help_echo_string;
	    }
	  else
            {
              /* If we move outside the frame, then we're
                 certainly no longer on any text in the frame.  */
              clear_mouse_face (hlinfo);
            }

          /* If the contents of the global variable help_echo_string
             has changed, generate a HELP_EVENT.  */
#if 0 /* The below is an invalid comparison when USE_LISP_UNION_TYPE.
	 But it was originally changed to this to fix a bug, so I have
	 not removed it completely in case the bug is still there.  */
          if (help_echo_string != previous_help_echo_string ||
	      (!NILP (help_echo_string) && !STRINGP (help_echo_string) && f->mouse_moved))
#else /* This is what xterm.c does.  */
	    if (!NILP (help_echo_string)
		|| !NILP (previous_help_echo_string))
	    do_help = 1;
#endif
          break;

	case WM_LBUTTONDOWN:
	case WM_LBUTTONUP:
	case WM_MBUTTONDOWN:
	case WM_MBUTTONUP:
	case WM_RBUTTONDOWN:
	case WM_RBUTTONUP:
	case WM_XBUTTONDOWN:
	case WM_XBUTTONUP:
	  {
            /* If we decide we want to generate an event to be seen
               by the rest of Emacs, we put it here.  */
            int tool_bar_p = 0;
	    int button;
	    int up;

	    if (dpyinfo->grabbed && last_mouse_frame
		&& FRAME_LIVE_P (last_mouse_frame))
	      f = last_mouse_frame;
	    else
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	    if (f)
	      {
                construct_mouse_click (&inev, &msg, f);

                /* Is this in the tool-bar?  */
                if (WINDOWP (f->tool_bar_window)
                    && WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)))
                  {
                    Lisp_Object window;
		    int x = XFASTINT (inev.x);
		    int y = XFASTINT (inev.y);

                    window = window_from_coordinates (f, x, y, 0, 1);

                    if (EQ (window, f->tool_bar_window))
                      {
                        w32_handle_tool_bar_click (f, &inev);
                        tool_bar_p = 1;
                      }
                  }

                if (tool_bar_p
		    || (dpyinfo->w32_focus_frame
			&& f != dpyinfo->w32_focus_frame))
		  inev.kind = NO_EVENT;
	      }

	    parse_button (msg.msg.message, HIWORD (msg.msg.wParam),
			  &button, &up);

	    if (up)
	      {
		dpyinfo->grabbed &= ~ (1 << button);
	      }
	    else
	      {
		dpyinfo->grabbed |= (1 << button);
		last_mouse_frame = f;
                /* Ignore any mouse motion that happened
                   before this event; any subsequent mouse-movement
                   Emacs events should reflect only motion after
                   the ButtonPress.  */
                if (f != 0)
                  f->mouse_moved = 0;

                if (!tool_bar_p)
                  last_tool_bar_item = -1;
	      }
	    break;
	  }

	case WM_MOUSEWHEEL:
        case WM_MOUSEHWHEEL:
	  {
	    if (dpyinfo->grabbed && last_mouse_frame
		&& FRAME_LIVE_P (last_mouse_frame))
	      f = last_mouse_frame;
	    else
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	    if (f)
	      {

		if (!dpyinfo->w32_focus_frame
		    || f == dpyinfo->w32_focus_frame)
		  {
		    /* Emit an Emacs wheel-up/down event.  */
		    construct_mouse_wheel (&inev, &msg, f);
		  }
		/* Ignore any mouse motion that happened before this
		   event; any subsequent mouse-movement Emacs events
		   should reflect only motion after the
		   ButtonPress.  */
		f->mouse_moved = 0;
	      }
	    last_mouse_frame = f;
	    last_tool_bar_item = -1;
	  }
	  break;

	case WM_DROPFILES:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    construct_drag_n_drop (&inev, &msg, f);
	  break;

	case WM_VSCROLL:
	  {
	    struct scroll_bar *bar =
	      x_window_to_scroll_bar ((HWND)msg.msg.lParam);

	    if (bar)
	      w32_scroll_bar_handle_click (bar, &msg, &inev);
	    break;
	  }

	case WM_WINDOWPOSCHANGED:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  if (f)
	    {
	      if (f->want_fullscreen & FULLSCREEN_WAIT)
		f->want_fullscreen &= ~(FULLSCREEN_WAIT|FULLSCREEN_BOTH);
	    }
	  check_visibility = 1;
	  break;

	case WM_ACTIVATE:
	case WM_ACTIVATEAPP:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  if (f)
	    x_check_fullscreen (f);
	  check_visibility = 1;
	  break;

	case WM_MOVE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f && !f->async_iconified)
	    {
	      int x, y;

	      x_real_positions (f, &x, &y);
	      f->left_pos = x;
	      f->top_pos = y;
	    }

	  check_visibility = 1;
	  break;

	case WM_SHOWWINDOW:
	  /* wParam non-zero means Window is about to be shown, 0 means
	     about to be hidden.  */
	  /* Redo the mouse-highlight after the tooltip has gone.  */
	  if (!msg.msg.wParam && msg.msg.hwnd == tip_window)
	    {
	      tip_window = NULL;
	      redo_mouse_highlight ();
	    }

	  /* If window has been obscured or exposed by another window
	     being maximized or minimized/restored, then recheck
	     visibility of all frames.  Direct changes to our own
	     windows get handled by WM_SIZE.  */
#if 0
	  if (msg.msg.lParam != 0)
	    check_visibility = 1;
	  else
	    {
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	      f->async_visible = msg.msg.wParam;
	    }
#endif

	  check_visibility = 1;
	  break;

	case WM_SIZE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  /* Inform lisp of whether frame has been iconified etc. */
	  if (f)
	    {
	      switch (msg.msg.wParam)
		{
		case SIZE_MINIMIZED:
		  f->async_visible = 0;
		  f->async_iconified = 1;

		  inev.kind = ICONIFY_EVENT;
		  XSETFRAME (inev.frame_or_window, f);
		  break;

		case SIZE_MAXIMIZED:
		case SIZE_RESTORED:
		  f->async_visible = 1;
		  f->async_iconified = 0;

		  /* wait_reading_process_output will notice this and update
		     the frame's display structures.  */
		  SET_FRAME_GARBAGED (f);

		  if (f->iconified)
		    {
                      int x, y;

                      /* Reset top and left positions of the Window
                         here since Windows sends a WM_MOVE message
                         BEFORE telling us the Window is minimized
                         when the Window is iconified, with 3000,3000
                         as the co-ords. */
                      x_real_positions (f, &x, &y);
                      f->left_pos = x;
                      f->top_pos = y;

		      inev.kind = DEICONIFY_EVENT;
		      XSETFRAME (inev.frame_or_window, f);
		    }
		  else if (! NILP (Vframe_list)
			   && ! NILP (XCDR (Vframe_list)))
		    /* Force a redisplay sooner or later
		       to update the frame titles
		       in case this is the second frame.  */
		    record_asynch_buffer_change ();
		  break;
		}
	    }

	  if (f && !f->async_iconified && msg.msg.wParam != SIZE_MINIMIZED)
	    {
	      RECT rect;
	      int rows;
	      int columns;
	      int width;
	      int height;

	      GetClientRect (msg.msg.hwnd, &rect);

	      height = rect.bottom - rect.top;
	      width = rect.right - rect.left;

	      rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, height);
	      columns = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, width);

	      /* TODO: Clip size to the screen dimensions.  */

	      /* Even if the number of character rows and columns has
		 not changed, the font size may have changed, so we need
		 to check the pixel dimensions as well.  */

	      if (columns != FRAME_COLS (f)
		  || rows != FRAME_LINES (f)
		  || width != FRAME_PIXEL_WIDTH (f)
		  || height != FRAME_PIXEL_HEIGHT (f))
		{
		  change_frame_size (f, rows, columns, 0, 1, 0);
		  SET_FRAME_GARBAGED (f);
		  cancel_mouse_face (f);
		  FRAME_PIXEL_WIDTH (f) = width;
		  FRAME_PIXEL_HEIGHT (f) = height;
		  f->win_gravity = NorthWestGravity;
		}
	    }

	  check_visibility = 1;
	  break;

	case WM_MOUSELEAVE:
	  f = x_any_window_to_frame (dpyinfo, msg.msg.hwnd);
	  if (f)
	    {
	      if (f == hlinfo->mouse_face_mouse_frame)
		{
		  /* If we move outside the frame, then we're
		     certainly no longer on any text in the frame.  */
		  clear_mouse_face (hlinfo);
		  hlinfo->mouse_face_mouse_frame = 0;
		}

	      /* Generate a nil HELP_EVENT to cancel a help-echo.
		 Do it only if there's something to cancel.
		 Otherwise, the startup message is cleared when
		 the mouse leaves the frame.  */
	      if (any_help_event_p)
		do_help = -1;
	    }
	  break;

	case WM_SETFOCUS:
	  w32_detect_focus_change (dpyinfo, &msg, &inev);

	  dpyinfo->grabbed = 0;
	  check_visibility = 1;
	  break;

	case WM_KILLFOCUS:
	  f = x_top_window_to_frame (dpyinfo, msg.msg.hwnd);

          if (f)
            {
              if (f == dpyinfo->w32_focus_event_frame)
                dpyinfo->w32_focus_event_frame = 0;

              if (f == dpyinfo->w32_focus_frame)
                x_new_focus_frame (dpyinfo, 0);

              if (f == hlinfo->mouse_face_mouse_frame)
                {
                  /* If we move outside the frame, then we're
                     certainly no longer on any text in the frame.  */
                  clear_mouse_face (hlinfo);
                  hlinfo->mouse_face_mouse_frame = 0;
                }

              /* Generate a nil HELP_EVENT to cancel a help-echo.
                 Do it only if there's something to cancel.
                 Otherwise, the startup message is cleared when
                 the mouse leaves the frame.  */
              if (any_help_event_p)
		do_help = -1;
            }

	  dpyinfo->grabbed = 0;
	  check_visibility = 1;
	  break;

	case WM_CLOSE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      inev.kind = DELETE_WINDOW_EVENT;
	      XSETFRAME (inev.frame_or_window, f);
	    }
	  break;

	case WM_INITMENU:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      inev.kind = MENU_BAR_ACTIVATE_EVENT;
	      XSETFRAME (inev.frame_or_window, f);
	    }
	  break;

	case WM_COMMAND:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      extern void menubar_selection_callback
		(FRAME_PTR f, void * client_data);
	      menubar_selection_callback (f, (void *)msg.msg.wParam);
	    }

	  check_visibility = 1;
	  break;

	case WM_DISPLAYCHANGE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      dpyinfo->n_cbits = msg.msg.wParam;
	      DebPrint (("display change: %d %d\n",
			 (short) LOWORD (msg.msg.lParam),
			 (short) HIWORD (msg.msg.lParam)));
	    }

	  check_visibility = 1;
	  break;

	default:
	  /* Check for messages registered at runtime.  */
	  if (msg.msg.message == msh_mousewheel)
	    {
	      /* Forward MSH_MOUSEWHEEL as WM_MOUSEWHEEL.  */
	      msg.msg.message = WM_MOUSEWHEEL;
	      prepend_msg (&msg);
	    }
	  break;
	}

      if (inev.kind != NO_EVENT)
	{
	  kbd_buffer_store_event_hold (&inev, hold_quit);
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

	  if (do_help > 0)
	    {
	      if (NILP (help_echo_string))
		{
		  help_echo_object = help_echo_window = Qnil;
		  help_echo_pos = -1;
		}

	      any_help_event_p = 1;
	      gen_help_event (help_echo_string, frame, help_echo_window,
			      help_echo_object, help_echo_pos);
	    }
	  else
	    {
	      help_echo_string = Qnil;
	      gen_help_event (Qnil, frame, Qnil, Qnil, 0);
	    }
	  count++;
	}
    }

  /* If the focus was just given to an autoraising frame,
     raise it now.  */
  /* ??? This ought to be able to handle more than one such frame.  */
  if (pending_autoraise_frame)
    {
      x_raise_frame (pending_autoraise_frame);
      pending_autoraise_frame = 0;
    }

  /* Check which frames are still visible, if we have enqueued any user
     events or been notified of events that may affect visibility.  We
     do this here because there doesn't seem to be any direct
     notification from Windows that the visibility of a window has
     changed (at least, not in all cases).  */
  if (count > 0 || check_visibility)
    {
      Lisp_Object tail, frame;

      FOR_EACH_FRAME (tail, frame)
      {
	FRAME_PTR f = XFRAME (frame);
	/* The tooltip has been drawn already.  Avoid the
	   SET_FRAME_GARBAGED below.  */
	if (EQ (frame, tip_frame))
	  continue;

	/* Check "visible" frames and mark each as obscured or not.
	   Note that async_visible is nonzero for unobscured and
	   obscured frames, but zero for hidden and iconified frames.  */
	if (FRAME_W32_P (f) && f->async_visible)
	  {
	    RECT clipbox;
	    HDC  hdc;

	    enter_crit ();
	    /* Query clipping rectangle for the entire window area
	       (GetWindowDC), not just the client portion (GetDC).
	       Otherwise, the scrollbars and menubar aren't counted as
	       part of the visible area of the frame, and we may think
	       the frame is obscured when really a scrollbar is still
	       visible and gets WM_PAINT messages above.  */
	    hdc = GetWindowDC (FRAME_W32_WINDOW (f));
	    GetClipBox (hdc, &clipbox);
	    ReleaseDC (FRAME_W32_WINDOW (f), hdc);
	    leave_crit ();

	    if (clipbox.right == clipbox.left
		|| clipbox.bottom == clipbox.top)
	      {
		/* Frame has become completely obscured so mark as
		   such (we do this by setting async_visible to 2 so
		   that FRAME_VISIBLE_P is still true, but redisplay
		   will skip it).  */
		f->async_visible = 2;

		if (!FRAME_OBSCURED_P (f))
		  {
		    DebPrint (("frame %p (%s) obscured\n", f,
			       SDATA (f->name)));
		  }
	      }
	    else
	      {
		/* Frame is not obscured, so mark it as such.  */
		f->async_visible = 1;

		if (FRAME_OBSCURED_P (f))
		  {
		    SET_FRAME_GARBAGED (f);
		    DebPrint (("obscured frame %p (%s) found to be visible\n", f,
			       SDATA (f->name)));

		    /* Force a redisplay sooner or later.  */
		    record_asynch_buffer_change ();
		  }
	      }
	  }
      }
    }

  UNBLOCK_INPUT;
  return count;
}



/***********************************************************************
			     Text Cursor
 ***********************************************************************/

/* Set clipping for output in glyph row ROW.  W is the window in which
   we operate.  GC is the graphics context to set clipping in.

   ROW may be a text row or, e.g., a mode line.  Text rows must be
   clipped to the interior of the window dedicated to text display,
   mode lines must be clipped to the whole window.  */

static void
w32_clip_to_row (struct window *w, struct glyph_row *row, int area, HDC hdc)
{
  RECT clip_rect;
  int window_x, window_y, window_width;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  clip_rect.left = window_x;
  clip_rect.top = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);
  clip_rect.top = max (clip_rect.top, window_y);
  clip_rect.right = clip_rect.left + window_width;
  clip_rect.bottom = clip_rect.top + row->visible_height;

  w32_set_clip_rectangle (hdc, &clip_rect);
}


/* Draw a hollow box cursor on window W in glyph row ROW.  */

static void
x_draw_hollow_cursor (struct window *w, struct glyph_row *row)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  HDC hdc;
  RECT rect;
  int left, top, h;
  struct glyph *cursor_glyph;
  HBRUSH hb = CreateSolidBrush (f->output_data.w32->cursor_pixel);

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute frame-relative coordinates for phys cursor.  */
  get_phys_cursor_geometry (w, row, cursor_glyph, &left, &top, &h);
  rect.left = left;
  rect.top = top;
  rect.bottom = rect.top + h;
  rect.right = rect.left + w->phys_cursor_width;

  hdc = get_frame_dc (f);
  /* Set clipping, draw the rectangle, and reset clipping again.  */
  w32_clip_to_row (w, row, TEXT_AREA, hdc);
  FrameRect (hdc, &rect, hb);
  DeleteObject (hb);
  w32_set_clip_rectangle (hdc, NULL);
  release_frame_dc (f, hdc);
}


/* Draw a bar cursor on window W in glyph row ROW.

   Implementation note: One would like to draw a bar cursor with an
   angle equal to the one given by the font property XA_ITALIC_ANGLE.
   Unfortunately, I didn't find a font yet that has this property set.
   --gerd.  */

static void
x_draw_bar_cursor (struct window *w, struct glyph_row *row,
		   int width, enum text_cursor_kinds kind)
{
  struct frame *f = XFRAME (w->frame);
  struct glyph *cursor_glyph;

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* If on an image, draw like a normal cursor.  That's usually better
     visible than drawing a bar, esp. if the image is large so that
     the bar might not be in the window.  */
  if (cursor_glyph->type == IMAGE_GLYPH)
    {
      struct glyph_row *row;
      row = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
      draw_phys_cursor_glyph (w, row, DRAW_CURSOR);
    }
  else
    {
      COLORREF cursor_color = f->output_data.w32->cursor_pixel;
      struct face *face = FACE_FROM_ID (f, cursor_glyph->face_id);
      int x;
      HDC hdc;

      /* If the glyph's background equals the color we normally draw
	 the bar cursor in, the bar cursor in its normal color is
	 invisible.  Use the glyph's foreground color instead in this
	 case, on the assumption that the glyph's colors are chosen so
	 that the glyph is legible.  */
      if (face->background == cursor_color)
	cursor_color = face->foreground;

      x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

      hdc = get_frame_dc (f);
      w32_clip_to_row (w, row, TEXT_AREA, hdc);

      if (kind == BAR_CURSOR)
	{
	  if (width < 0)
	    width = FRAME_CURSOR_WIDTH (f);
	  width = min (cursor_glyph->pixel_width, width);

	  w->phys_cursor_width = width;

	  /* If the character under cursor is R2L, draw the bar cursor
	     on the right of its glyph, rather than on the left.  */
	  if ((cursor_glyph->resolved_level & 1) != 0)
	    x += cursor_glyph->pixel_width - width;

	  w32_fill_area (f, hdc, cursor_color, x,
			 WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y),
			 width, row->height);
	}
      else
	{
	  int dummy_x, dummy_y, dummy_h;

	  if (width < 0)
	    width = row->height;

	  width = min (row->height, width);

	  get_phys_cursor_geometry (w, row, cursor_glyph, &dummy_x,
				    &dummy_y, &dummy_h);
	  w32_fill_area (f, hdc, cursor_color, x,
			 WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y +
						  row->height - width),
			 w->phys_cursor_width, width);
	}

      w32_set_clip_rectangle (hdc, NULL);
      release_frame_dc (f, hdc);
    }
}


/* RIF: Define cursor CURSOR on frame F.  */

static void
w32_define_frame_cursor (struct frame *f, Cursor cursor)
{
  w32_define_cursor (FRAME_W32_WINDOW (f), cursor);
}


/* RIF: Clear area on frame F.  */

static void
w32_clear_frame_area (struct frame *f, int x, int y, int width, int height)
{
  HDC hdc;

  hdc = get_frame_dc (f);
  w32_clear_area (f, hdc, x, y, width, height);
  release_frame_dc (f, hdc);
}

/* RIF: Draw or clear cursor on window W.  */

static void
w32_draw_window_cursor (struct window *w, struct glyph_row *glyph_row,
			int x, int y, int cursor_type, int cursor_width,
			int on_p, int active_p)
{
  if (on_p)
    {
      /* If the user wants to use the system caret, make sure our own
	 cursor remains invisible.  */
      if (w32_use_visible_system_caret)
	{
	  /* Call to erase_phys_cursor here seems to use the
	     wrong values of w->phys_cursor, as they have been
	     overwritten before this function was called. */
	  if (w->phys_cursor_type != NO_CURSOR)
	    erase_phys_cursor (w);

	  cursor_type = w->phys_cursor_type = NO_CURSOR;
	  w->phys_cursor_width = -1;
	}
      else
	{
	  w->phys_cursor_type = cursor_type;
	}

      w->phys_cursor_on_p = 1;

      /* If this is the active cursor, we need to track it with the
	 system caret, so third party software like screen magnifiers
	 and speech synthesizers can follow the cursor.  */
      if (active_p)
	{
	  struct frame *f = XFRAME (WINDOW_FRAME (w));
	  HWND hwnd = FRAME_W32_WINDOW (f);

	  w32_system_caret_x
	    = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
	  w32_system_caret_y
	    = (WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y)
	       + glyph_row->ascent - w->phys_cursor_ascent);

	  PostMessage (hwnd, WM_IME_STARTCOMPOSITION, 0, 0);

	  /* If the size of the active cursor changed, destroy the old
	     system caret.  */
	  if (w32_system_caret_hwnd
	      && (w32_system_caret_height != w->phys_cursor_height))
	    PostMessage (hwnd, WM_EMACS_DESTROY_CARET, 0, 0);

	  w32_system_caret_height = w->phys_cursor_height;

	  /* Move the system caret.  */
	  PostMessage (hwnd, WM_EMACS_TRACK_CARET, 0, 0);
	}

      if (glyph_row->exact_window_width_line_p
	  && (glyph_row->reversed_p
	      ? (w->phys_cursor.hpos < 0)
	      : (w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])))
	{
	  glyph_row->cursor_in_fringe_p = 1;
	  draw_fringe_bitmap (w, glyph_row, glyph_row->reversed_p);
	  return;
	}

      switch (cursor_type)
	{
	case HOLLOW_BOX_CURSOR:
	  x_draw_hollow_cursor (w, glyph_row);
	  break;

	case FILLED_BOX_CURSOR:
	  draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
	  break;

	case BAR_CURSOR:
	  x_draw_bar_cursor (w, glyph_row, cursor_width, BAR_CURSOR);
	  break;

	case HBAR_CURSOR:
	  x_draw_bar_cursor (w, glyph_row, cursor_width, HBAR_CURSOR);
	  break;

	case NO_CURSOR:
	  w->phys_cursor_width = 0;
	  break;

	default:
	  abort ();
	}
    }
}



/* Icons.  */

int
x_bitmap_icon (struct frame *f, Lisp_Object icon)
{
  HANDLE main_icon;
  HANDLE small_icon = NULL;

  if (FRAME_W32_WINDOW (f) == 0)
    return 1;

  if (NILP (icon))
    main_icon = LoadIcon (hinst, EMACS_CLASS);
  else if (STRINGP (icon))
    {
      /* Load the main icon from the named file.  */
      main_icon = LoadImage (NULL, (LPCTSTR) SDATA (icon), IMAGE_ICON, 0, 0,
			     LR_DEFAULTSIZE | LR_LOADFROMFILE);
      /* Try to load a small icon to go with it.  */
      small_icon = LoadImage (NULL, (LPCSTR) SDATA (icon), IMAGE_ICON,
			      GetSystemMetrics (SM_CXSMICON),
			      GetSystemMetrics (SM_CYSMICON),
			      LR_LOADFROMFILE);
    }
  else if (SYMBOLP (icon))
    {
      LPCTSTR name;

      if (EQ (icon, intern ("application")))
	name = (LPCTSTR) IDI_APPLICATION;
      else if (EQ (icon, intern ("hand")))
	name = (LPCTSTR) IDI_HAND;
      else if (EQ (icon, intern ("question")))
	name = (LPCTSTR) IDI_QUESTION;
      else if (EQ (icon, intern ("exclamation")))
	name = (LPCTSTR) IDI_EXCLAMATION;
      else if (EQ (icon, intern ("asterisk")))
	name = (LPCTSTR) IDI_ASTERISK;
      else if (EQ (icon, intern ("winlogo")))
	name = (LPCTSTR) IDI_WINLOGO;
      else
	return 1;

      main_icon = LoadIcon (NULL, name);
    }
  else
    return 1;

  if (main_icon == NULL)
    return 1;

  PostMessage (FRAME_W32_WINDOW (f), WM_SETICON, (WPARAM) ICON_BIG,
               (LPARAM) main_icon);

  /* If there is a small icon that goes with it, set that too.  */
  if (small_icon)
    PostMessage (FRAME_W32_WINDOW (f), WM_SETICON, (WPARAM) ICON_SMALL,
		 (LPARAM) small_icon);

  return 0;
}


/************************************************************************
			  Handling X errors
 ************************************************************************/

/* Display Error Handling functions not used on W32. Listing them here
   helps diff stay in step when comparing w32term.c with xterm.c.

x_error_catcher (display, error)
x_catch_errors (dpy)
x_catch_errors_unwind (old_val)
x_check_errors (dpy, format)
x_fully_uncatch_errors ()
x_had_errors_p (dpy)
x_clear_errors (dpy)
x_uncatch_errors (dpy, count)
x_trace_wire ()
x_connection_signal (signalnum)
x_connection_closed (dpy, error_message)
x_error_quitter (display, error)
x_error_handler (display, error)
x_io_error_quitter (display)

 */


/* Changing the font of the frame.  */

Lisp_Object
x_new_font (struct frame *f, Lisp_Object font_object, int fontset)
{
  struct font *font = XFONT_OBJECT (font_object);

  if (fontset < 0)
    fontset = fontset_from_font (font_object);
  FRAME_FONTSET (f) = fontset;
  if (FRAME_FONT (f) == font)
    /* This font is already set in frame F.  There's nothing more to
       do.  */
    return font_object;

  FRAME_FONT (f) = font;
  FRAME_BASELINE_OFFSET (f) = font->baseline_offset;
  FRAME_COLUMN_WIDTH (f) = font->average_width;
  FRAME_SPACE_WIDTH (f) = font->space_width;
  FRAME_LINE_HEIGHT (f) = font->height;

  compute_fringe_widths (f, 1);

  /* Compute the scroll bar width in character columns.  */
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f)
	= (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + wid - 1) / wid;
    }
  else
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;
    }

  /* Now make the frame display the given font.  */
  if (FRAME_X_WINDOW (f) != 0)
    {
      /* Don't change the size of a tip frame; there's no point in
	 doing it because it's done in Fx_show_tip, and it leads to
	 problems because the tip frame has no widget.  */
      if (NILP (tip_frame) || XFRAME (tip_frame) != f)
	x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
    }

  /* X version sets font of input methods here also.  */

  return font_object;
}


/***********************************************************************
	TODO: W32 Input Methods
 ***********************************************************************/
/* Listing missing functions from xterm.c helps diff stay in step.

xim_destroy_callback (xim, client_data, call_data)
xim_open_dpy (dpyinfo, resource_name)
struct xim_inst_t
xim_instantiate_callback (display, client_data, call_data)
xim_initialize (dpyinfo, resource_name)
xim_close_dpy (dpyinfo)

 */


/* Calculate the absolute position in frame F
   from its current recorded position values and gravity.  */

void
x_calc_absolute_position (struct frame *f)
{
  int flags = f->size_hint_flags;

  /* The sum of the widths of the frame's left and right borders, and
     the sum of the heights of the frame's top and bottom borders (in
     pixels) drawn by Windows.  */
  unsigned int left_right_borders_width, top_bottom_borders_height;

  /* Try to get the actual values of these two variables.  We compute
     the border width (height) by subtracting the width (height) of
     the frame's client area from the width (height) of the frame's
     entire window.  */
  WINDOWPLACEMENT wp = { 0 };
  RECT client_rect = { 0 };

  if (GetWindowPlacement (FRAME_W32_WINDOW (f), &wp)
      && GetClientRect (FRAME_W32_WINDOW (f), &client_rect))
    {
      left_right_borders_width =
	(wp.rcNormalPosition.right - wp.rcNormalPosition.left) -
	(client_rect.right - client_rect.left);

      top_bottom_borders_height =
	(wp.rcNormalPosition.bottom - wp.rcNormalPosition.top) -
	(client_rect.bottom - client_rect.top);
    }
  else
    {
      /* Use sensible default values.  */
      left_right_borders_width = 8;
      top_bottom_borders_height = 32;
    }

  /* Treat negative positions as relative to the rightmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    f->left_pos = (x_display_pixel_width (FRAME_W32_DISPLAY_INFO (f))
		   - FRAME_PIXEL_WIDTH (f)
		   + f->left_pos
		   - (left_right_borders_width - 1));

  if (flags & YNegative)
    f->top_pos = (x_display_pixel_height (FRAME_W32_DISPLAY_INFO (f))
		  - FRAME_PIXEL_HEIGHT (f)
		  + f->top_pos
		  - (top_bottom_borders_height - 1));

  /* The left_pos and top_pos are now relative to the top and left
     screen edges, so the flags should correspond.  */
  f->size_hint_flags &= ~ (XNegative | YNegative);
}

/* CHANGE_GRAVITY is 1 when calling from Fset_frame_position,
   to really change the position, and 0 when calling from
   x_make_frame_visible (in that case, XOFF and YOFF are the current
   position values).  It is -1 when calling from x_set_frame_parameters,
   which means, do adjust for borders but don't change the gravity.  */

void
x_set_offset (struct frame *f, register int xoff, register int yoff,
	      int change_gravity)
{
  int modified_top, modified_left;

  if (change_gravity > 0)
    {
      f->top_pos = yoff;
      f->left_pos = xoff;
      f->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->size_hint_flags |= YNegative;
      f->win_gravity = NorthWestGravity;
    }
  x_calc_absolute_position (f);

  BLOCK_INPUT;
  x_wm_set_size_hint (f, (long) 0, 0);

  modified_left = f->left_pos;
  modified_top = f->top_pos;

  my_set_window_pos (FRAME_W32_WINDOW (f),
		     NULL,
		     modified_left, modified_top,
		     0, 0,
		     SWP_NOZORDER | SWP_NOSIZE | SWP_NOACTIVATE);
  UNBLOCK_INPUT;
}


/* Check if we need to resize the frame due to a fullscreen request.
   If so needed, resize the frame. */
static void
x_check_fullscreen (struct frame *f)
{
  if (f->want_fullscreen & FULLSCREEN_BOTH)
    {
      int width, height, ign;

      x_real_positions (f, &f->left_pos, &f->top_pos);

      x_fullscreen_adjust (f, &width, &height, &ign, &ign);

      /* We do not need to move the window, it shall be taken care of
         when setting WM manager hints.  */
      if (FRAME_COLS (f) != width || FRAME_LINES (f) != height)
        {
          change_frame_size (f, height, width, 0, 1, 0);
          SET_FRAME_GARBAGED (f);
          cancel_mouse_face (f);

          /* Wait for the change of frame size to occur */
          f->want_fullscreen |= FULLSCREEN_WAIT;
        }
    }
}

/* Call this to change the size of frame F's x-window.
   If CHANGE_GRAVITY is 1, we change to top-left-corner window gravity
   for this size change and subsequent size changes.
   Otherwise we leave the window gravity unchanged.  */

void
x_set_window_size (struct frame *f, int change_gravity, int cols, int rows)
{
  int pixelwidth, pixelheight;

  BLOCK_INPUT;

  check_frame_size (f, &rows, &cols);
  f->scroll_bar_actual_width
    = FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f);

  compute_fringe_widths (f, 0);

  pixelwidth = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, cols);
  pixelheight = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, rows);

  f->win_gravity = NorthWestGravity;
  x_wm_set_size_hint (f, (long) 0, 0);

  {
    RECT rect;

    rect.left = rect.top = 0;
    rect.right = pixelwidth;
    rect.bottom = pixelheight;

    AdjustWindowRect (&rect, f->output_data.w32->dwStyle,
		      FRAME_EXTERNAL_MENU_BAR (f));

    my_set_window_pos (FRAME_W32_WINDOW (f),
		       NULL,
		       0, 0,
		       rect.right - rect.left,
		       rect.bottom - rect.top,
		       SWP_NOZORDER | SWP_NOMOVE | SWP_NOACTIVATE);
  }

#if 0
  /* The following mirrors what is done in xterm.c. It appears to be
     for informing lisp of the new size immediately, while the actual
     resize will happen asynchronously. But on Windows, the menu bar
     automatically wraps when the frame is too narrow to contain it,
     and that causes any calculations made here to come out wrong. The
     end is some nasty buggy behavior, including the potential loss
     of the minibuffer.

     Disabling this code is either not sufficient to fix the problems
     completely, or it causes fresh problems, but at least it removes
     the most problematic symptom of the minibuffer becoming unusable.

     -----------------------------------------------------------------

     Now, strictly speaking, we can't be sure that this is accurate,
     but the window manager will get around to dealing with the size
     change request eventually, and we'll hear how it went when the
     ConfigureNotify event gets here.

     We could just not bother storing any of this information here,
     and let the ConfigureNotify event set everything up, but that
     might be kind of confusing to the Lisp code, since size changes
     wouldn't be reported in the frame parameters until some random
     point in the future when the ConfigureNotify event arrives.

     We pass 1 for DELAY since we can't run Lisp code inside of
     a BLOCK_INPUT.  */
  change_frame_size (f, rows, cols, 0, 1, 0);
  FRAME_PIXEL_WIDTH (f) = pixelwidth;
  FRAME_PIXEL_HEIGHT (f) = pixelheight;

  /* We've set {FRAME,PIXEL}_{WIDTH,HEIGHT} to the values we hope to
     receive in the ConfigureNotify event; if we get what we asked
     for, then the event won't cause the screen to become garbaged, so
     we have to make sure to do it here.  */
  SET_FRAME_GARBAGED (f);

  /* If cursor was outside the new size, mark it as off.  */
  mark_window_cursors_off (XWINDOW (f->root_window));

  /* Clear out any recollection of where the mouse highlighting was,
     since it might be in a place that's outside the new frame size.
     Actually checking whether it is outside is a pain in the neck,
     so don't try--just let the highlighting be done afresh with new size.  */
  cancel_mouse_face (f);
#endif

  UNBLOCK_INPUT;
}

/* Mouse warping.  */

void x_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y);

void
x_set_mouse_position (struct frame *f, int x, int y)
{
  int pix_x, pix_y;

  pix_x = FRAME_COL_TO_PIXEL_X (f, x) + FRAME_COLUMN_WIDTH (f) / 2;
  pix_y = FRAME_LINE_TO_PIXEL_Y (f, y) + FRAME_LINE_HEIGHT (f) / 2;

  if (pix_x < 0) pix_x = 0;
  if (pix_x > FRAME_PIXEL_WIDTH (f)) pix_x = FRAME_PIXEL_WIDTH (f);

  if (pix_y < 0) pix_y = 0;
  if (pix_y > FRAME_PIXEL_HEIGHT (f)) pix_y = FRAME_PIXEL_HEIGHT (f);

  x_set_mouse_pixel_position (f, pix_x, pix_y);
}

void
x_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y)
{
  RECT rect;
  POINT pt;

  BLOCK_INPUT;

  GetClientRect (FRAME_W32_WINDOW (f), &rect);
  pt.x = rect.left + pix_x;
  pt.y = rect.top + pix_y;
  ClientToScreen (FRAME_W32_WINDOW (f), &pt);

  SetCursorPos (pt.x, pt.y);

  UNBLOCK_INPUT;
}


/* focus shifting, raising and lowering.  */

void
x_focus_on_frame (struct frame *f)
{
  struct w32_display_info *dpyinfo = &one_w32_display_info;

  /* Give input focus to frame.  */
  BLOCK_INPUT;
#if 0
  /* Try not to change its Z-order if possible.  */
  if (x_window_to_frame (dpyinfo, GetForegroundWindow ()))
    my_set_focus (f, FRAME_W32_WINDOW (f));
  else
#endif
    my_set_foreground_window (FRAME_W32_WINDOW (f));
  UNBLOCK_INPUT;
}

void
x_unfocus_frame (struct frame *f)
{
}

/* Raise frame F.  */
void
x_raise_frame (struct frame *f)
{
  BLOCK_INPUT;

  /* Strictly speaking, raise-frame should only change the frame's Z
     order, leaving input focus unchanged.  This is reasonable behavior
     on X where the usual policy is point-to-focus.  However, this
     behavior would be very odd on Windows where the usual policy is
     click-to-focus.

     On X, if the mouse happens to be over the raised frame, it gets
     input focus anyway (so the window with focus will never be
     completely obscured) - if not, then just moving the mouse over it
     is sufficient to give it focus.  On Windows, the user must actually
     click on the frame (preferably the title bar so as not to move
     point), which is more awkward.  Also, no other Windows program
     raises a window to the top but leaves another window (possibly now
     completely obscured) with input focus.

     Because there is a system setting on Windows that allows the user
     to choose the point to focus policy, we make the strict semantics
     optional, but by default we grab focus when raising.  */

  if (NILP (Vw32_grab_focus_on_raise))
    {
      /* The obvious call to my_set_window_pos doesn't work if Emacs is
	 not already the foreground application: the frame is raised
	 above all other frames belonging to us, but not above the
	 current top window.  To achieve that, we have to resort to this
	 more cumbersome method.  */

      HDWP handle = BeginDeferWindowPos (2);
      if (handle)
	{
	  DeferWindowPos (handle,
			  FRAME_W32_WINDOW (f),
  			  HWND_TOP,
  			  0, 0, 0, 0,
  			  SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

	  DeferWindowPos (handle,
			  GetForegroundWindow (),
			  FRAME_W32_WINDOW (f),
			  0, 0, 0, 0,
			  SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

	  EndDeferWindowPos (handle);
	}
    }
  else
    {
      my_set_foreground_window (FRAME_W32_WINDOW (f));
    }

  UNBLOCK_INPUT;
}

/* Lower frame F.  */
void
x_lower_frame (struct frame *f)
{
  BLOCK_INPUT;
  my_set_window_pos (FRAME_W32_WINDOW (f),
		     HWND_BOTTOM,
		     0, 0, 0, 0,
		     SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);
  UNBLOCK_INPUT;
}

static void
w32_frame_raise_lower (FRAME_PTR f, int raise_flag)
{
  if (! FRAME_W32_P (f))
    return;

  if (raise_flag)
    x_raise_frame (f);
  else
    x_lower_frame (f);
}

/* Change of visibility.  */

/* This tries to wait until the frame is really visible.
   However, if the window manager asks the user where to position
   the frame, this will return before the user finishes doing that.
   The frame will not actually be visible at that time,
   but it will become visible later when the window manager
   finishes with it.  */

void
x_make_frame_visible (struct frame *f)
{
  Lisp_Object type;

  BLOCK_INPUT;

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

  if (! FRAME_VISIBLE_P (f))
    {
      /* We test FRAME_GARBAGED_P here to make sure we don't
	 call x_set_offset a second time
	 if we get to x_make_frame_visible a second time
	 before the window gets really visible.  */
      if (! FRAME_ICONIFIED_P (f)
	  && ! f->output_data.w32->asked_for_visible)
	{
	  RECT workarea_rect;
	  RECT window_rect;

	  /* Adjust vertical window position in order to avoid being
	     covered by a task bar placed at the bottom of the desktop. */
	  SystemParametersInfo (SPI_GETWORKAREA, 0, &workarea_rect, 0);
	  GetWindowRect (FRAME_W32_WINDOW (f), &window_rect);
	  if (window_rect.bottom > workarea_rect.bottom
	      && window_rect.top > workarea_rect.top)
	    f->top_pos = max (window_rect.top
			      - window_rect.bottom + workarea_rect.bottom,
			      workarea_rect.top);

	  x_set_offset (f, f->left_pos, f->top_pos, 0);
	}

      f->output_data.w32->asked_for_visible = 1;

      /* According to a report in emacs-devel 2008-06-03, SW_SHOWNORMAL
	 causes unexpected behavior when unminimizing frames that were
	 previously maximized.  But only SW_SHOWNORMAL works properly for
	 frames that were truely hidden (using make-frame-invisible), so
	 we need it to avoid Bug#5482.  It seems that async_iconified
	 is only set for minimized windows that are still visible, so
         use that to determine the appropriate flag to pass ShowWindow.  */
      my_show_window (f, FRAME_W32_WINDOW (f),
                      f->async_iconified ? SW_RESTORE : SW_SHOWNORMAL);
    }

  /* Synchronize to ensure Emacs knows the frame is visible
     before we do anything else.  We do this loop with input not blocked
     so that incoming events are handled.  */
  {
    Lisp_Object frame;
    int count;

    /* This must come after we set COUNT.  */
    UNBLOCK_INPUT;

    XSETFRAME (frame, f);

    /* Wait until the frame is visible.  Process X events until a
       MapNotify event has been seen, or until we think we won't get a
       MapNotify at all..  */
    for (count = input_signal_count + 10;
	 input_signal_count < count && !FRAME_VISIBLE_P (f);)
      {
	/* Force processing of queued events.  */
        /* TODO: x_sync equivalent?  */

	/* Machines that do polling rather than SIGIO have been observed
	   to go into a busy-wait here.  So we'll fake an alarm signal
	   to let the handler know that there's something to be read.
	   We used to raise a real alarm, but it seems that the handler
	   isn't always enabled here.  This is probably a bug.  */
	if (input_polling_used ())
	  {
	    /* It could be confusing if a real alarm arrives while processing
	       the fake one.  Turn it off and let the handler reset it.  */
	    int old_poll_suppress_count = poll_suppress_count;
	    poll_suppress_count = 1;
	    poll_for_input_1 ();
	    poll_suppress_count = old_poll_suppress_count;
	  }
      }
    FRAME_SAMPLE_VISIBILITY (f);
  }
}

/* Change from mapped state to withdrawn state. */

/* Make the frame visible (mapped and not iconified).  */

void
x_make_frame_invisible (struct frame *f)
{
  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_W32_DISPLAY_INFO (f)->x_highlight_frame == f)
    FRAME_W32_DISPLAY_INFO (f)->x_highlight_frame = 0;

  BLOCK_INPUT;

  my_show_window (f, FRAME_W32_WINDOW (f), SW_HIDE);

  /* We can't distinguish this from iconification
     just by the event that we get from the server.
     So we can't win using the usual strategy of letting
     FRAME_SAMPLE_VISIBILITY set this.  So do it by hand,
     and synchronize with the server to make sure we agree.  */
  f->visible = 0;
  FRAME_ICONIFIED_P (f) = 0;
  f->async_visible = 0;
  f->async_iconified = 0;

  UNBLOCK_INPUT;
}

/* Change window state from mapped to iconified. */

void
x_iconify_frame (struct frame *f)
{
  Lisp_Object type;

  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_W32_DISPLAY_INFO (f)->x_highlight_frame == f)
    FRAME_W32_DISPLAY_INFO (f)->x_highlight_frame = 0;

  if (f->async_iconified)
    return;

  BLOCK_INPUT;

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

  /* Simulate the user minimizing the frame.  */
  SendMessage (FRAME_W32_WINDOW (f), WM_SYSCOMMAND, SC_MINIMIZE, 0);

  UNBLOCK_INPUT;
}


/* Free X resources of frame F.  */

void
x_free_frame_resources (struct frame *f)
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);

  BLOCK_INPUT;

  /* We must free faces before destroying windows because some
     font-driver (e.g. xft) access a window while finishing a
     face.  */
  if (FRAME_FACE_CACHE (f))
    free_frame_faces (f);

  if (FRAME_W32_WINDOW (f))
    my_destroy_window (f, FRAME_W32_WINDOW (f));

  free_frame_menubar (f);

  unload_color (f, FRAME_FOREGROUND_PIXEL (f));
  unload_color (f, FRAME_BACKGROUND_PIXEL (f));
  unload_color (f, f->output_data.w32->cursor_pixel);
  unload_color (f, f->output_data.w32->cursor_foreground_pixel);
  unload_color (f, f->output_data.w32->border_pixel);
  unload_color (f, f->output_data.w32->mouse_pixel);
  if (f->output_data.w32->white_relief.allocated_p)
    unload_color (f, f->output_data.w32->white_relief.pixel);
  if (f->output_data.w32->black_relief.allocated_p)
    unload_color (f, f->output_data.w32->black_relief.pixel);

  if (FRAME_FACE_CACHE (f))
    free_frame_faces (f);

  xfree (f->output_data.w32);
  f->output_data.w32 = NULL;

  if (f == dpyinfo->w32_focus_frame)
    dpyinfo->w32_focus_frame = 0;
  if (f == dpyinfo->w32_focus_event_frame)
    dpyinfo->w32_focus_event_frame = 0;
  if (f == dpyinfo->x_highlight_frame)
    dpyinfo->x_highlight_frame = 0;

  if (f == hlinfo->mouse_face_mouse_frame)
    {
      hlinfo->mouse_face_beg_row
	= hlinfo->mouse_face_beg_col = -1;
      hlinfo->mouse_face_end_row
	= hlinfo->mouse_face_end_col = -1;
      hlinfo->mouse_face_window = Qnil;
      hlinfo->mouse_face_deferred_gc = 0;
      hlinfo->mouse_face_mouse_frame = 0;
    }

  UNBLOCK_INPUT;
}


/* Destroy the window of frame F.  */
void
x_destroy_window (struct frame *f)
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  x_free_frame_resources (f);
  dpyinfo->reference_count--;
}


/* Setting window manager hints.  */

/* Set the normal size hints for the window manager, for frame F.
   FLAGS is the flags word to use--or 0 meaning preserve the flags
   that the window now has.
   If USER_POSITION is nonzero, we set the USPosition
   flag (this is useful when FLAGS is 0).  */
void
x_wm_set_size_hint (struct frame *f, long flags, int user_position)
{
  Window window = FRAME_W32_WINDOW (f);

  enter_crit ();

  SetWindowLong (window, WND_FONTWIDTH_INDEX, FRAME_COLUMN_WIDTH (f));
  SetWindowLong (window, WND_LINEHEIGHT_INDEX, FRAME_LINE_HEIGHT (f));
  SetWindowLong (window, WND_BORDER_INDEX, FRAME_INTERNAL_BORDER_WIDTH (f));
  SetWindowLong (window, WND_SCROLLBAR_INDEX, f->scroll_bar_actual_width);

  leave_crit ();
}

/* Window manager things */
void
x_wm_set_icon_position (struct frame *f, int icon_x, int icon_y)
{
#if 0
  Window window = FRAME_W32_WINDOW (f);

  f->display.x->wm_hints.flags |= IconPositionHint;
  f->display.x->wm_hints.icon_x = icon_x;
  f->display.x->wm_hints.icon_y = icon_y;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->display.x->wm_hints);
#endif
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

static int w32_initialized = 0;

void
w32_initialize_display_info (Lisp_Object display_name)
{
  struct w32_display_info *dpyinfo = &one_w32_display_info;
  Mouse_HLInfo *hlinfo = &dpyinfo->mouse_highlight;

  memset (dpyinfo, 0, sizeof (*dpyinfo));

  /* Put it on w32_display_name_list.  */
  w32_display_name_list = Fcons (Fcons (display_name, Qnil),
                                 w32_display_name_list);
  dpyinfo->name_list_element = XCAR (w32_display_name_list);

  dpyinfo->w32_id_name
    = (char *) xmalloc (SCHARS (Vinvocation_name)
			+ SCHARS (Vsystem_name)
			+ 2);
  sprintf (dpyinfo->w32_id_name, "%s@%s",
	   SDATA (Vinvocation_name), SDATA (Vsystem_name));

  /* Default Console mode values - overridden when running in GUI mode
     with values obtained from system metrics.  */
  dpyinfo->resx = 1;
  dpyinfo->resy = 1;
  dpyinfo->n_planes = 1;
  dpyinfo->n_cbits = 4;
  dpyinfo->n_fonts = 0;
  dpyinfo->smallest_font_height = 1;
  dpyinfo->smallest_char_width = 1;

  hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
  hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
  hlinfo->mouse_face_face_id = DEFAULT_FACE_ID;
  hlinfo->mouse_face_window = Qnil;
  hlinfo->mouse_face_overlay = Qnil;
  hlinfo->mouse_face_hidden = 0;

  dpyinfo->vertical_scroll_bar_cursor = w32_load_cursor (IDC_ARROW);
  /* TODO: dpyinfo->gray */

}

/* Create an xrdb-style database of resources to supersede registry settings.
   The database is just a concatenation of C strings, finished by an additional
   \0.  The strings are submitted to some basic normalization, so

     [ *]option[ *]:[ *]value...

   becomes

     option:value...

   but any whitespace following value is not removed.  */

static char *
w32_make_rdb (char *xrm_option)
{
  char *buffer = xmalloc (strlen (xrm_option) + 2);
  char *current = buffer;
  char ch;
  int in_option = 1;
  int before_value = 0;

  do {
    ch = *xrm_option++;

    if (ch == '\n')
      {
        *current++ = '\0';
        in_option = 1;
        before_value = 0;
      }
    else if (ch != ' ')
      {
        *current++ = ch;
        if (in_option && (ch == ':'))
          {
            in_option = 0;
            before_value = 1;
          }
        else if (before_value)
          {
            before_value = 0;
          }
      }
    else if (!(in_option || before_value))
      {
        *current++ = ch;
      }
  } while (ch);

  *current = '\0';

  return buffer;
}

void
x_flush (struct frame * f)
{ /* Nothing to do */ }


extern frame_parm_handler w32_frame_parm_handlers[];

static struct redisplay_interface w32_redisplay_interface =
{
  w32_frame_parm_handlers,
  x_produce_glyphs,
  x_write_glyphs,
  x_insert_glyphs,
  x_clear_end_of_line,
  x_scroll_run,
  x_after_update_window_line,
  x_update_window_begin,
  x_update_window_end,
  x_cursor_to,
  x_flush,
  0,  /* flush_display_optional */
  x_clear_window_mouse_face,
  x_get_glyph_overhangs,
  x_fix_overlapping_area,
  w32_draw_fringe_bitmap,
  w32_define_fringe_bitmap,
  w32_destroy_fringe_bitmap,
  w32_compute_glyph_string_overhangs,
  x_draw_glyph_string,
  w32_define_frame_cursor,
  w32_clear_frame_area,
  w32_draw_window_cursor,
  w32_draw_vertical_window_border,
  w32_shift_glyphs_for_insert
};

static void x_delete_terminal (struct terminal *term);

static struct terminal *
w32_create_terminal (struct w32_display_info *dpyinfo)
{
  struct terminal *terminal;

  terminal = create_terminal ();

  terminal->type = output_w32;
  terminal->display_info.w32 = dpyinfo;
  dpyinfo->terminal = terminal;

  /* MSVC does not type K&R functions with no arguments correctly, and
     so we must explicitly cast them.  */
  terminal->clear_frame_hook = x_clear_frame;
  terminal->ins_del_lines_hook = x_ins_del_lines;
  terminal->delete_glyphs_hook = x_delete_glyphs;
  terminal->ring_bell_hook = w32_ring_bell;
  terminal->reset_terminal_modes_hook = w32_reset_terminal_modes;
  terminal->set_terminal_modes_hook = w32_set_terminal_modes;
  terminal->update_begin_hook = x_update_begin;
  terminal->update_end_hook = x_update_end;
  terminal->set_terminal_window_hook = w32_set_terminal_window;
  terminal->read_socket_hook = w32_read_socket;
  terminal->frame_up_to_date_hook = w32_frame_up_to_date;
  terminal->mouse_position_hook = w32_mouse_position;
  terminal->frame_rehighlight_hook = w32_frame_rehighlight;
  terminal->frame_raise_lower_hook = w32_frame_raise_lower;
  /* terminal->fullscreen_hook = XTfullscreen_hook; */
  terminal->set_vertical_scroll_bar_hook = w32_set_vertical_scroll_bar;
  terminal->condemn_scroll_bars_hook = w32_condemn_scroll_bars;
  terminal->redeem_scroll_bar_hook = w32_redeem_scroll_bar;
  terminal->judge_scroll_bars_hook = w32_judge_scroll_bars;

  terminal->delete_frame_hook = x_destroy_window;
  terminal->delete_terminal_hook = x_delete_terminal;

  terminal->rif = &w32_redisplay_interface;
  terminal->scroll_region_ok = 1;    /* We'll scroll partial frames. */
  terminal->char_ins_del_ok = 1;
  terminal->line_ins_del_ok = 1;         /* We'll just blt 'em. */
  terminal->fast_clear_end_of_line = 1;  /* X does this well. */
  terminal->memory_below_frame = 0;   /* We don't remember what scrolls
                                        off the bottom. */

  /* We don't yet support separate terminals on W32, so don't try to share
     keyboards between virtual terminals that are on the same physical
     terminal like X does.  */
  terminal->kboard = (KBOARD *) xmalloc (sizeof (KBOARD));
  init_kboard (terminal->kboard);
  KVAR (terminal->kboard, Vwindow_system) = intern ("w32");
  terminal->kboard->next_kboard = all_kboards;
  all_kboards = terminal->kboard;
  /* Don't let the initial kboard remain current longer than necessary.
     That would cause problems if a file loaded on startup tries to
     prompt in the mini-buffer.  */
  if (current_kboard == initial_kboard)
    current_kboard = terminal->kboard;
  terminal->kboard->reference_count++;

  return terminal;
}

static void
x_delete_terminal (struct terminal *terminal)
{
  struct w32_display_info *dpyinfo = terminal->display_info.w32;

  /* Protect against recursive calls.  delete_frame in
     delete_terminal calls us back when it deletes our last frame.  */
  if (!terminal->name)
    return;

  BLOCK_INPUT;

  x_delete_display (dpyinfo);
  UNBLOCK_INPUT;
}

struct w32_display_info *
w32_term_init (Lisp_Object display_name, char *xrm_option, char *resource_name)
{
  struct w32_display_info *dpyinfo;
  struct terminal *terminal;
  HDC hdc;

  BLOCK_INPUT;

  if (!w32_initialized)
    {
      w32_initialize ();
      w32_initialized = 1;
    }

  w32_initialize_display_info (display_name);

  dpyinfo = &one_w32_display_info;
  terminal = w32_create_terminal (dpyinfo);

  /* Set the name of the terminal. */
  terminal->name = (char *) xmalloc (SBYTES (display_name) + 1);
  strncpy (terminal->name, SDATA (display_name), SBYTES (display_name));
  terminal->name[SBYTES (display_name)] = 0;

  dpyinfo->xrdb = xrm_option ? w32_make_rdb (xrm_option) : NULL;

  /* Put this display on the chain.  */
  dpyinfo->next = x_display_list;
  x_display_list = dpyinfo;

  hdc = GetDC (NULL);

  dpyinfo->root_window = GetDesktopWindow ();
  dpyinfo->n_planes = GetDeviceCaps (hdc, PLANES);
  dpyinfo->n_cbits = GetDeviceCaps (hdc, BITSPIXEL);
  dpyinfo->resx = GetDeviceCaps (hdc, LOGPIXELSX);
  dpyinfo->resy = GetDeviceCaps (hdc, LOGPIXELSY);
  dpyinfo->has_palette = GetDeviceCaps (hdc, RASTERCAPS) & RC_PALETTE;
  ReleaseDC (NULL, hdc);

  /* initialize palette with white and black */
  {
    XColor color;
    w32_defined_color (0, "white", &color, 1);
    w32_defined_color (0, "black", &color, 1);
  }

  /* Add the default keyboard.  */
  add_keyboard_wait_descriptor (0);

  /* Create Fringe Bitmaps and store them for later use.

     On W32, bitmaps are all unsigned short, as Windows requires
     bitmap data to be Word aligned.  For some reason they are
     horizontally reflected compared to how they appear on X, so we
     need to bitswap and convert to unsigned shorts before creating
     the bitmaps.  */
  w32_init_fringe (terminal->rif);

#ifdef F_SETOWN
  fcntl (connection, F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN) */

#ifdef SIGIO
  if (interrupt_input)
    init_sigio (connection);
#endif /* ! defined (SIGIO) */

  UNBLOCK_INPUT;

  return dpyinfo;
}

/* Get rid of display DPYINFO, assuming all frames are already gone.  */
void
x_delete_display (struct w32_display_info *dpyinfo)
{
  /* Discard this display from w32_display_name_list and w32_display_list.
     We can't use Fdelq because that can quit.  */
  if (! NILP (w32_display_name_list)
      && EQ (XCAR (w32_display_name_list), dpyinfo->name_list_element))
    w32_display_name_list = XCDR (w32_display_name_list);
  else
    {
      Lisp_Object tail;

      tail = w32_display_name_list;
      while (CONSP (tail) && CONSP (XCDR (tail)))
	{
	  if (EQ (XCAR (XCDR (tail)), dpyinfo->name_list_element))
	    {
	      XSETCDR (tail, XCDR (XCDR (tail)));
	      break;
	    }
	  tail = XCDR (tail);
	}
    }

  /* free palette table */
  {
    struct w32_palette_entry * plist;

    plist = dpyinfo->color_list;
    while (plist)
    {
      struct w32_palette_entry * pentry = plist;
      plist = plist->next;
      xfree (pentry);
    }
    dpyinfo->color_list = NULL;
    if (dpyinfo->palette)
      DeleteObject (dpyinfo->palette);
  }
  xfree (dpyinfo->w32_id_name);

  w32_reset_fringes ();
}

/* Set up use of W32.  */

DWORD WINAPI w32_msg_worker (void * arg);

static void
w32_initialize (void)
{
  HANDLE shell;
  HRESULT (WINAPI * set_user_model) (wchar_t * id);

  baud_rate = 19200;

  w32_system_caret_hwnd = NULL;
  w32_system_caret_height = 0;
  w32_system_caret_x = 0;
  w32_system_caret_y = 0;

  /* On Windows 7 and later, we need to set the user model ID
     to associate emacsclient launched files with Emacs frames
     in the UI.  */
  shell = GetModuleHandle ("shell32.dll");
  if (shell)
    {
      set_user_model
	= (void *) GetProcAddress (shell,
				   "SetCurrentProcessExplicitAppUserModelID");

      /* If the function is defined, then we are running on Windows 7
	 or newer, and the UI uses this to group related windows
	 together.  Since emacs, runemacs, emacsclient are related, we
	 want them grouped even though the executables are different,
	 so we need to set a consistent ID between them.  */
      if (set_user_model)
	set_user_model (L"GNU.Emacs");
    }

  /* Initialize w32_use_visible_system_caret based on whether a screen
     reader is in use.  */
  if (!SystemParametersInfo (SPI_GETSCREENREADER, 0,
			     &w32_use_visible_system_caret, 0))
    w32_use_visible_system_caret = 0;

  last_tool_bar_item = -1;
  any_help_event_p = 0;

  /* Initialize input mode: interrupt_input off, no flow control, allow
     8 bit character input, standard quit char.  */
  Fset_input_mode (Qnil, Qnil, make_number (2), Qnil);

  {
    DWORD input_locale_id = (DWORD) GetKeyboardLayout (0);
    keyboard_codepage = codepage_for_locale ((LCID) (input_locale_id & 0xffff));
  }

  /* Create the window thread - it will terminate itself when the app
     terminates */
  init_crit ();

  dwMainThreadId = GetCurrentThreadId ();
  DuplicateHandle (GetCurrentProcess (), GetCurrentThread (),
		   GetCurrentProcess (), &hMainThread, 0, TRUE, DUPLICATE_SAME_ACCESS);

  /* Wait for thread to start */
  {
    MSG msg;

    PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE);

    hWindowsThread = CreateThread (NULL, 0,
                                   w32_msg_worker,
                                   0, 0, &dwWindowsThreadId);

    GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);
  }

  /* It is desirable that mainThread should have the same notion of
     focus window and active window as windowsThread.  Unfortunately, the
     following call to AttachThreadInput, which should do precisely what
     we need, causes major problems when Emacs is linked as a console
     program.  Unfortunately, we have good reasons for doing that, so
     instead we need to send messages to windowsThread to make some API
     calls for us (ones that affect, or depend on, the active/focus
     window state.)  */
#ifdef ATTACH_THREADS
  AttachThreadInput (dwMainThreadId, dwWindowsThreadId, TRUE);
#endif

  /* Dynamically link to optional system components.  */
  {
    HMODULE user_lib = GetModuleHandle ("user32.dll");

#define LOAD_PROC(lib, fn) pfn##fn = (void *) GetProcAddress (lib, #fn)

    LOAD_PROC (user_lib, SetLayeredWindowAttributes);

#undef LOAD_PROC

    /* Ensure scrollbar handle is at least 5 pixels.  */
    vertical_scroll_bar_min_handle = 5;

    /* For either kind of scroll bar, take account of the arrows; these
       effectively form the border of the main scroll bar range.  */
    vertical_scroll_bar_top_border = vertical_scroll_bar_bottom_border
      = GetSystemMetrics (SM_CYVSCROLL);
  }
}

void
syms_of_w32term (void)
{
  staticpro (&w32_display_name_list);
  w32_display_name_list = Qnil;

  staticpro (&last_mouse_scroll_bar);
  last_mouse_scroll_bar = Qnil;

  DEFSYM (Qvendor_specific_keysyms, "vendor-specific-keysyms");

  DEFVAR_INT ("w32-num-mouse-buttons",
	      w32_num_mouse_buttons,
	      doc: /* Number of physical mouse buttons.  */);
  w32_num_mouse_buttons = 2;

  DEFVAR_LISP ("w32-swap-mouse-buttons",
	      Vw32_swap_mouse_buttons,
	       doc: /* Swap the mapping of middle and right mouse buttons.
When nil, middle button is mouse-2 and right button is mouse-3.  */);
  Vw32_swap_mouse_buttons = Qnil;

  DEFVAR_LISP ("w32-grab-focus-on-raise",
	       Vw32_grab_focus_on_raise,
	       doc: /* Raised frame grabs input focus.
When t, `raise-frame' grabs input focus as well.  This fits well
with the normal Windows click-to-focus policy, but might not be
desirable when using a point-to-focus policy.  */);
  Vw32_grab_focus_on_raise = Qt;

  DEFVAR_LISP ("w32-capslock-is-shiftlock",
	       Vw32_capslock_is_shiftlock,
	       doc: /* Apply CapsLock state to non character input keys.
When nil, CapsLock only affects normal character input keys.  */);
  Vw32_capslock_is_shiftlock = Qnil;

  DEFVAR_LISP ("w32-recognize-altgr",
	       Vw32_recognize_altgr,
	       doc: /* Recognize right-alt and left-ctrl as AltGr.
When nil, the right-alt and left-ctrl key combination is
interpreted normally.  */);
  Vw32_recognize_altgr = Qt;

  DEFVAR_BOOL ("w32-use-visible-system-caret",
	       w32_use_visible_system_caret,
	       doc: /* Flag to make the system caret visible.
When this is non-nil, Emacs will indicate the position of point by
using the system caret instead of drawing its own cursor.  Some screen
reader software does not track the system cursor properly when it is
invisible, and gets confused by Emacs drawing its own cursor, so this
variable is initialized to t when Emacs detects that screen reader
software is running as it starts up.

When this variable is set, other variables affecting the appearance of
the cursor have no effect.  */);

  w32_use_visible_system_caret = 0;

  /* We don't yet support this, but defining this here avoids whining
     from cus-start.el and other places, like "M-x set-variable".  */
  DEFVAR_BOOL ("x-use-underline-position-properties",
	       x_use_underline_position_properties,
     doc: /* *Non-nil means make use of UNDERLINE_POSITION font properties.
A value of nil means ignore them.  If you encounter fonts with bogus
UNDERLINE_POSITION font properties, for example 7x13 on XFree prior
to 4.1, set this to nil.  You can also use `underline-minimum-offset'
to override the font's UNDERLINE_POSITION for small font display
sizes.  */);
  x_use_underline_position_properties = 0;

  DEFVAR_BOOL ("x-underline-at-descent-line",
	       x_underline_at_descent_line,
     doc: /* *Non-nil means to draw the underline at the same place as the descent line.
A value of nil means to draw the underline according to the value of the
variable `x-use-underline-position-properties', which is usually at the
baseline level.  The default value is nil.  */);
  x_underline_at_descent_line = 0;

  DEFVAR_LISP ("x-toolkit-scroll-bars", Vx_toolkit_scroll_bars,
	       doc: /* Which toolkit scroll bars Emacs uses, if any.
A value of nil means Emacs doesn't use toolkit scroll bars.
With the X Window system, the value is a symbol describing the
X toolkit.  Possible values are: gtk, motif, xaw, or xaw3d.
With MS Windows or Nextstep, the value is t.  */);
  Vx_toolkit_scroll_bars = Qt;

  staticpro (&last_mouse_motion_frame);
  last_mouse_motion_frame = Qnil;
}
