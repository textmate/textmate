/* Window creation, deletion and examination for GNU Emacs.
   Does not include redisplay.
   Copyright (C) 1985-1987, 1993-1998, 2000-2012
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
#include "keyboard.h"
#include "keymap.h"
#include "frame.h"
#include "window.h"
#include "commands.h"
#include "indent.h"
#include "termchar.h"
#include "disptab.h"
#include "dispextern.h"
#include "blockinput.h"
#include "intervals.h"
#include "termhooks.h"		/* For FRAME_TERMINAL.  */

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif	/* HAVE_X_WINDOWS */
#ifdef WINDOWSNT
#include "w32term.h"
#endif
#ifdef MSDOS
#include "msdos.h"
#endif
#ifdef HAVE_NS
#include "nsterm.h"
#endif

Lisp_Object Qwindowp, Qwindow_live_p;
static Lisp_Object Qwindow_configuration_p, Qrecord_window_buffer;
static Lisp_Object Qwindow_deletable_p, Qdelete_window, Qdisplay_buffer;
static Lisp_Object Qreplace_buffer_in_windows, Qget_mru_window;
static Lisp_Object Qwindow_resize_root_window, Qwindow_resize_root_window_vertically;
static Lisp_Object Qscroll_up, Qscroll_down, Qscroll_command;
static Lisp_Object Qsafe, Qabove, Qbelow;
static Lisp_Object Qauto_buffer_name, Qclone_of;

static int displayed_window_lines (struct window *);
static struct window *decode_window (Lisp_Object);
static int count_windows (struct window *);
static int get_leaf_windows (struct window *, struct window **, int);
static void window_scroll (Lisp_Object, int, int, int);
static void window_scroll_pixel_based (Lisp_Object, int, int, int);
static void window_scroll_line_based (Lisp_Object, int, int, int);
static int freeze_window_start (struct window *, void *);
static Lisp_Object window_list (void);
static int add_window_to_list (struct window *, void *);
static int candidate_window_p (Lisp_Object, Lisp_Object, Lisp_Object,
                               Lisp_Object);
static Lisp_Object next_window (Lisp_Object, Lisp_Object,
                                Lisp_Object, int);
static void decode_next_window_args (Lisp_Object *, Lisp_Object *,
                                     Lisp_Object *);
static void foreach_window (struct frame *,
				 int (* fn) (struct window *, void *),
                            void *);
static int foreach_window_1 (struct window *,
                             int (* fn) (struct window *, void *),
                             void *);
static Lisp_Object window_list_1 (Lisp_Object, Lisp_Object, Lisp_Object);
static int window_resize_check (struct window *, int);
static void window_resize_apply (struct window *, int);
static Lisp_Object select_window (Lisp_Object, Lisp_Object, int);

/* This is the window in which the terminal's cursor should
   be left when nothing is being done with it.  This must
   always be a leaf window, and its buffer is selected by
   the top level editing loop at the end of each command.

   This value is always the same as
   FRAME_SELECTED_WINDOW (selected_frame).  */
Lisp_Object selected_window;

/* A list of all windows for use by next_window and Fwindow_list.
   Functions creating or deleting windows should invalidate this cache
   by setting it to nil.  */
Lisp_Object Vwindow_list;

/* The mini-buffer window of the selected frame.
   Note that you cannot test for mini-bufferness of an arbitrary window
   by comparing against this; but you can test for mini-bufferness of
   the selected window.  */
Lisp_Object minibuf_window;

/* Non-nil means it is the window whose mode line should be
   shown as the selected window when the minibuffer is selected.  */
Lisp_Object minibuf_selected_window;

/* Hook run at end of temp_output_buffer_show.  */
static Lisp_Object Qtemp_buffer_show_hook;

/* Incremented for each window created.  */
static int sequence_number;

/* Nonzero after init_window_once has finished.  */
static int window_initialized;

/* Hook to run when window config changes.  */
static Lisp_Object Qwindow_configuration_change_hook;

/* Used by the function window_scroll_pixel_based */
static int window_scroll_pixel_based_preserve_x;
static int window_scroll_pixel_based_preserve_y;

/* Same for window_scroll_line_based.  */
static int window_scroll_preserve_hpos;
static int window_scroll_preserve_vpos;

static struct window *
decode_window (register Lisp_Object window)
{
  if (NILP (window))
    return XWINDOW (selected_window);

  CHECK_LIVE_WINDOW (window);
  return XWINDOW (window);
}

static struct window *
decode_any_window (register Lisp_Object window)
{
  if (NILP (window))
    return XWINDOW (selected_window);

  CHECK_WINDOW (window);
  return XWINDOW (window);
}

DEFUN ("windowp", Fwindowp, Swindowp, 1, 1, 0,
       doc: /* Return t if OBJECT is a window and nil otherwise.  */)
  (Lisp_Object object)
{
  return WINDOWP (object) ? Qt : Qnil;
}

DEFUN ("window-live-p", Fwindow_live_p, Swindow_live_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a live window and nil otherwise.
A live window is a window that displays a buffer.
Internal windows and deleted windows are not live.  */)
  (Lisp_Object object)
{
  return WINDOW_LIVE_P (object) ? Qt : Qnil;
}

/* Frames and windows.  */
DEFUN ("window-frame", Fwindow_frame, Swindow_frame, 1, 1, 0,
       doc: /* Return the frame that window WINDOW is on.
If WINDOW is omitted or nil, it defaults to the selected window.  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->frame;
}

DEFUN ("frame-root-window", Fframe_root_window, Sframe_root_window, 0, 1, 0,
       doc: /* Return the root window of FRAME-OR-WINDOW.
If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
With a frame argument, return that frame's root window.
With a window argument, return the root window of that window's frame.  */)
  (Lisp_Object frame_or_window)
{
  Lisp_Object window;

  if (NILP (frame_or_window))
    window = SELECTED_FRAME ()->root_window;
  else if (WINDOWP (frame_or_window))
    window = XFRAME (WINDOW_FRAME (XWINDOW (frame_or_window)))->root_window;
  else
    {
      CHECK_LIVE_FRAME (frame_or_window);
      window = XFRAME (frame_or_window)->root_window;
    }

  return window;
}

DEFUN ("minibuffer-window", Fminibuffer_window, Sminibuffer_window, 0, 1, 0,
       doc: /* Return the minibuffer window for frame FRAME.
If FRAME is omitted or nil, it defaults to the selected frame.  */)
  (Lisp_Object frame)
{
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  return FRAME_MINIBUF_WINDOW (XFRAME (frame));
}

DEFUN ("window-minibuffer-p", Fwindow_minibuffer_p,
       Swindow_minibuffer_p, 0, 1, 0,
       doc: /* Return non-nil if WINDOW is a minibuffer window.
If WINDOW is omitted or nil, it defaults to the selected window.  */)
  (Lisp_Object window)
{
  return MINI_WINDOW_P (decode_any_window (window)) ? Qt : Qnil;
}

/* Don't move this to window.el - this must be a safe routine.  */
DEFUN ("frame-first-window", Fframe_first_window, Sframe_first_window, 0, 1, 0,
       doc: /* Return the topmost, leftmost live window on FRAME-OR-WINDOW.
If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
Else if FRAME-OR-WINDOW denotes any window, return the first window of
that window's frame.  If FRAME-OR-WINDOW denotes a live frame, return
the first window of that frame.  */)
  (Lisp_Object frame_or_window)
{
  Lisp_Object window;

  if (NILP (frame_or_window))
    window = SELECTED_FRAME ()->root_window;
  else if (WINDOWP (frame_or_window))
    window = XFRAME (WINDOW_FRAME (XWINDOW (frame_or_window)))->root_window;
  else
    {
      CHECK_LIVE_FRAME (frame_or_window);
      window = XFRAME (frame_or_window)->root_window;
    }

  while (NILP (XWINDOW (window)->buffer))
    {
      if (! NILP (XWINDOW (window)->hchild))
	window = XWINDOW (window)->hchild;
      else if (! NILP (XWINDOW (window)->vchild))
	window = XWINDOW (window)->vchild;
      else
	abort ();
    }

  return window;
}

DEFUN ("frame-selected-window", Fframe_selected_window,
       Sframe_selected_window, 0, 1, 0,
       doc: /* Return the selected window of FRAME-OR-WINDOW.
If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
Else if FRAME-OR-WINDOW denotes any window, return the selected window
of that window's frame.  If FRAME-OR-WINDOW denotes a live frame, return
the selected window of that frame.  */)
  (Lisp_Object frame_or_window)
{
  Lisp_Object window;

  if (NILP (frame_or_window))
    window = SELECTED_FRAME ()->selected_window;
  else if (WINDOWP (frame_or_window))
    window = XFRAME (WINDOW_FRAME (XWINDOW (frame_or_window)))->selected_window;
  else
    {
      CHECK_LIVE_FRAME (frame_or_window);
      window = XFRAME (frame_or_window)->selected_window;
    }

  return window;
}

DEFUN ("set-frame-selected-window", Fset_frame_selected_window,
       Sset_frame_selected_window, 2, 3, 0,
       doc: /* Set selected window of FRAME to WINDOW.
FRAME must be a live frame and defaults to the selected one.  If FRAME
is the selected frame, this makes WINDOW the selected window.  Optional
argument NORECORD non-nil means to neither change the order of recently
selected windows nor the buffer list.  WINDOW must denote a live window.
Return WINDOW.  */)
  (Lisp_Object frame, Lisp_Object window, Lisp_Object norecord)
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);
  CHECK_LIVE_WINDOW (window);

  if (! EQ (frame, WINDOW_FRAME (XWINDOW (window))))
    error ("In `set-frame-selected-window', WINDOW is not on FRAME");

  if (EQ (frame, selected_frame))
    return Fselect_window (window, norecord);
  else
    return XFRAME (frame)->selected_window = window;
}

DEFUN ("selected-window", Fselected_window, Sselected_window, 0, 0, 0,
       doc: /* Return the selected window.
The selected window is the window in which the standard cursor for
selected windows appears and to which many commands apply.  */)
  (void)
{
  return selected_window;
}

int window_select_count;

/* If select_window is called with inhibit_point_swap non-zero it will
   not store point of the old selected window's buffer back into that
   window's pointm slot.  This is needed by Fset_window_configuration to
   avoid that the display routine is called with selected_window set to
   Qnil causing a subsequent crash.  */
static Lisp_Object
select_window (Lisp_Object window, Lisp_Object norecord, int inhibit_point_swap)
{
  register struct window *w;
  register struct window *ow;
  struct frame *sf;

  CHECK_LIVE_WINDOW (window);

  w = XWINDOW (window);
  w->frozen_window_start_p = 0;

  if (NILP (norecord))
    {
      ++window_select_count;
      XSETFASTINT (w->use_time, window_select_count);
      record_buffer (w->buffer);
    }

  if (EQ (window, selected_window) && !inhibit_point_swap)
    return window;

  sf = SELECTED_FRAME ();
  if (XFRAME (WINDOW_FRAME (w)) != sf)
    {
      XFRAME (WINDOW_FRAME (w))->selected_window = window;
      /* Use this rather than Fhandle_switch_frame
	 so that FRAME_FOCUS_FRAME is moved appropriately as we
	 move around in the state where a minibuffer in a separate
	 frame is active.  */
      Fselect_frame (WINDOW_FRAME (w), norecord);
      /* Fselect_frame called us back so we've done all the work already.  */
      eassert (EQ (window, selected_window));
      return window;
    }
  else
    sf->selected_window = window;

  /* Store the current buffer's actual point into the
     old selected window.  It belongs to that window,
     and when the window is not selected, must be in the window.  */
  if (!inhibit_point_swap)
    {
      ow = XWINDOW (selected_window);
      if (! NILP (ow->buffer))
	set_marker_both (ow->pointm, ow->buffer,
			 BUF_PT (XBUFFER (ow->buffer)),
			 BUF_PT_BYTE (XBUFFER (ow->buffer)));
    }

  selected_window = window;

  Fset_buffer (w->buffer);

  BVAR (XBUFFER (w->buffer), last_selected_window) = window;

  /* Go to the point recorded in the window.
     This is important when the buffer is in more
     than one window.  It also matters when
     redisplay_window has altered point after scrolling,
     because it makes the change only in the window.  */
  {
    register EMACS_INT new_point = marker_position (w->pointm);
    if (new_point < BEGV)
      SET_PT (BEGV);
    else if (new_point > ZV)
      SET_PT (ZV);
    else
      SET_PT (new_point);
  }

  windows_or_buffers_changed++;
  return window;
}

DEFUN ("select-window", Fselect_window, Sselect_window, 1, 2, 0,
       doc: /* Select WINDOW.  Most editing will apply to WINDOW's buffer.
Also make WINDOW's buffer current and make WINDOW the frame's selected
window.  Return WINDOW.

Optional second arg NORECORD non-nil means do not put this buffer at the
front of the buffer list and do not make this window the most recently
selected one.

Note that the main editor command loop sets the current buffer to the
buffer of the selected window before each command.  */)
  (register Lisp_Object window, Lisp_Object norecord)
{
  return select_window (window, norecord, 0);
}

DEFUN ("window-buffer", Fwindow_buffer, Swindow_buffer, 0, 1, 0,
       doc: /* Return the buffer displayed in window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.
Return nil for an internal window or a deleted window.  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->buffer;
}

DEFUN ("window-parent", Fwindow_parent, Swindow_parent, 0, 1, 0,
       doc: /* Return the parent window of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.
Return nil for a window with no parent (e.g. a root window).  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->parent;
}

DEFUN ("window-top-child", Fwindow_top_child, Swindow_top_child, 1, 1, 0,
       doc: /* Return the topmost child window of window WINDOW.
Return nil if WINDOW is a live window (live windows have no children).
Return nil if WINDOW is an internal window whose children form a
horizontal combination.  */)
  (Lisp_Object window)
{
  CHECK_WINDOW (window);
  return decode_any_window (window)->vchild;
}

DEFUN ("window-left-child", Fwindow_left_child, Swindow_left_child, 1, 1, 0,
       doc: /* Return the leftmost child window of window WINDOW.
Return nil if WINDOW is a live window (live windows have no children).
Return nil if WINDOW is an internal window whose children form a
vertical combination.  */)
  (Lisp_Object window)
{
  CHECK_WINDOW (window);
  return decode_any_window (window)->hchild;
}

DEFUN ("window-next-sibling", Fwindow_next_sibling, Swindow_next_sibling, 0, 1, 0,
       doc: /* Return the next sibling window of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.
Return nil if WINDOW has no next sibling.  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->next;
}

DEFUN ("window-prev-sibling", Fwindow_prev_sibling, Swindow_prev_sibling, 0, 1, 0,
       doc: /* Return the previous sibling window of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.
Return nil if WINDOW has no previous sibling.  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->prev;
}

DEFUN ("window-combination-limit", Fwindow_combination_limit, Swindow_combination_limit, 1, 1, 0,
       doc: /* Return combination limit of window WINDOW.
If the return value is nil, child windows of WINDOW can be recombined with
WINDOW's siblings.  A return value of t means that child windows of
WINDOW are never \(re-)combined with WINDOW's siblings.  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->combination_limit;
}

DEFUN ("set-window-combination-limit", Fset_window_combination_limit, Sset_window_combination_limit, 2, 2, 0,
       doc: /* Set combination limit of window WINDOW to LIMIT; return LIMIT.
If LIMIT is nil, child windows of WINDOW can be recombined with
WINDOW's siblings.  LIMIT t means that child windows of WINDOW are
never \(re-)combined with WINDOW's siblings.  Other values are reserved
for future use.  */)
  (Lisp_Object window, Lisp_Object limit)
{
  register struct window *w = decode_any_window (window);

  w->combination_limit = limit;

  return w->combination_limit;
}

DEFUN ("window-use-time", Fwindow_use_time, Swindow_use_time, 0, 1, 0,
       doc: /* Return the use time of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.
The window with the highest use time is the most recently selected
one.  The window with the lowest use time is the least recently
selected one.  */)
  (Lisp_Object window)
{
  return decode_window (window)->use_time;
}

DEFUN ("window-total-height", Fwindow_total_height, Swindow_total_height, 0, 1, 0,
       doc: /* Return the total height, in lines, of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.

The return value includes the mode line and header line, if any.
If WINDOW is an internal window, the total height is the height
of the screen areas spanned by its children.

On a graphical display, this total height is reported as an
integer multiple of the default character height.  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->total_lines;
}

DEFUN ("window-total-width", Fwindow_total_width, Swindow_total_width, 0, 1, 0,
       doc: /* Return the total width, in columns, of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.

The return value includes any vertical dividers or scroll bars
belonging to WINDOW.  If WINDOW is an internal window, the total width
is the width of the screen areas spanned by its children.

On a graphical display, this total width is reported as an
integer multiple of the default character width.  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->total_cols;
}

DEFUN ("window-new-total", Fwindow_new_total, Swindow_new_total, 0, 1, 0,
       doc: /* Return the new total size of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.   */)
  (Lisp_Object window)
{
  return decode_any_window (window)->new_total;
}

DEFUN ("window-normal-size", Fwindow_normal_size, Swindow_normal_size, 0, 2, 0,
       doc: /* Return the normal height of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.
If HORIZONTAL is non-nil, return the normal width of WINDOW.  */)
  (Lisp_Object window, Lisp_Object horizontal)
{
  if (NILP (horizontal))
    return decode_any_window (window)->normal_lines;
  else
    return decode_any_window (window)->normal_cols;
}

DEFUN ("window-new-normal", Fwindow_new_normal, Swindow_new_normal, 0, 1, 0,
       doc: /* Return new normal size of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->new_normal;
}

DEFUN ("window-left-column", Fwindow_left_column, Swindow_left_column, 0, 1, 0,
       doc: /* Return left column of window WINDOW.
This is the distance, in columns, between the left edge of WINDOW and
the left edge of the frame's window area.  For instance, the return
value is 0 if there is no window to the left of WINDOW.

If WINDOW is omitted or nil, it defaults to the selected window.  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->left_col;
}

DEFUN ("window-top-line", Fwindow_top_line, Swindow_top_line, 0, 1, 0,
       doc: /* Return top line of window WINDOW.
This is the distance, in lines, between the top of WINDOW and the top
of the frame's window area.  For instance, the return value is 0 if
there is no window above WINDOW.

If WINDOW is omitted or nil, it defaults to the selected window.  */)
  (Lisp_Object window)
{
  return decode_any_window (window)->top_line;
}

/* Return the number of lines of W's body.  Don't count any mode or
   header line of W.  */

static int
window_body_lines (struct window *w)
{
  int height = XFASTINT (w->total_lines);

  if (!MINI_WINDOW_P (w))
    {
      if (WINDOW_WANTS_MODELINE_P (w))
	--height;
      if (WINDOW_WANTS_HEADER_LINE_P (w))
	--height;
    }

  return height;
}

/* Return the number of columns of W's body.  Don't count columns
   occupied by the scroll bar or the vertical bar separating W from its
   right sibling.  On window-systems don't count fringes or display
   margins either.  */

int
window_body_cols (struct window *w)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  int width = XINT (w->total_cols);

  if (WINDOW_HAS_VERTICAL_SCROLL_BAR (w))
    /* Scroll bars occupy a few columns.  */
    width -= WINDOW_CONFIG_SCROLL_BAR_COLS (w);
  else if (!FRAME_WINDOW_P (f)
	   && !WINDOW_RIGHTMOST_P (w) && !WINDOW_FULL_WIDTH_P (w))
    /* The column of `|' characters separating side-by-side windows
       occupies one column only.  */
    width -= 1;

  if (FRAME_WINDOW_P (f))
    /* On window-systems, fringes and display margins cannot be
       used for normal text.  */
    width -= (WINDOW_FRINGE_COLS (w)
	      + WINDOW_LEFT_MARGIN_COLS (w)
	      + WINDOW_RIGHT_MARGIN_COLS (w));

  return width;
}

DEFUN ("window-body-height", Fwindow_body_height, Swindow_body_height, 0, 1, 0,
       doc: /* Return the height, in lines, of WINDOW's text area.
If WINDOW is omitted or nil, it defaults to the selected window.
Signal an error if the window is not live.

The returned height does not include the mode line or header line.
On a graphical display, the height is expressed as an integer multiple
of the default character height.  If a line at the bottom of the text
area is only partially visible, that counts as a whole line; to
exclude partially-visible lines, use `window-text-height'.  */)
  (Lisp_Object window)
{
  struct window *w = decode_window (window);
  return make_number (window_body_lines (w));
}

DEFUN ("window-body-width", Fwindow_body_width, Swindow_body_width, 0, 1, 0,
       doc: /* Return the width, in columns, of WINDOW's text area.
If WINDOW is omitted or nil, it defaults to the selected window.
Signal an error if the window is not live.

The return value does not include any vertical dividers, fringe or
marginal areas, or scroll bars.  On a graphical display, the width is
expressed as an integer multiple of the default character width.  */)
  (Lisp_Object window)
{
  struct window *w = decode_window (window);
  return make_number (window_body_cols (w));
}

DEFUN ("window-hscroll", Fwindow_hscroll, Swindow_hscroll, 0, 1, 0,
       doc: /* Return the number of columns by which WINDOW is scrolled from left margin.
WINDOW must be a live window and defaults to the selected one.  */)
  (Lisp_Object window)
{
  return decode_window (window)->hscroll;
}

DEFUN ("set-window-hscroll", Fset_window_hscroll, Sset_window_hscroll, 2, 2, 0,
       doc: /* Set number of columns WINDOW is scrolled from left margin to NCOL.
If WINDOW is nil, the selected window is used.
Return NCOL.  NCOL should be zero or positive.

Note that if `automatic-hscrolling' is non-nil, you cannot scroll the
window so that the location of point moves off-window.  */)
  (Lisp_Object window, Lisp_Object ncol)
{
  struct window *w = decode_window (window);
  int hscroll;

  CHECK_NUMBER (ncol);
  hscroll = max (0, XINT (ncol));

  /* Prevent redisplay shortcuts when changing the hscroll.  */
  if (XINT (w->hscroll) != hscroll)
    XBUFFER (w->buffer)->prevent_redisplay_optimizations_p = 1;

  w->hscroll = make_number (hscroll);
  return ncol;
}

DEFUN ("window-redisplay-end-trigger", Fwindow_redisplay_end_trigger,
       Swindow_redisplay_end_trigger, 0, 1, 0,
       doc: /* Return WINDOW's redisplay end trigger value.
WINDOW defaults to the selected window.
See `set-window-redisplay-end-trigger' for more information.  */)
  (Lisp_Object window)
{
  return decode_window (window)->redisplay_end_trigger;
}

DEFUN ("set-window-redisplay-end-trigger", Fset_window_redisplay_end_trigger,
       Sset_window_redisplay_end_trigger, 2, 2, 0,
       doc: /* Set WINDOW's redisplay end trigger value to VALUE.
VALUE should be a buffer position (typically a marker) or nil.
If it is a buffer position, then if redisplay in WINDOW reaches a position
beyond VALUE, the functions in `redisplay-end-trigger-functions' are called
with two arguments: WINDOW, and the end trigger value.
Afterwards the end-trigger value is reset to nil.  */)
  (register Lisp_Object window, Lisp_Object value)
{
  register struct window *w;

  w = decode_window (window);
  w->redisplay_end_trigger = value;
  return value;
}

DEFUN ("window-edges", Fwindow_edges, Swindow_edges, 0, 1, 0,
       doc: /* Return a list of the edge coordinates of WINDOW.
The list has the form (LEFT TOP RIGHT BOTTOM).
TOP and BOTTOM count by lines, and LEFT and RIGHT count by columns,
all relative to 0, 0 at top left corner of frame.

RIGHT is one more than the rightmost column occupied by WINDOW.
BOTTOM is one more than the bottommost row occupied by WINDOW.
The edges include the space used by WINDOW's scroll bar, display
margins, fringes, header line, and/or mode line.  For the edges of
just the text area, use `window-inside-edges'.  */)
  (Lisp_Object window)
{
  register struct window *w = decode_any_window (window);

  return Fcons (make_number (WINDOW_LEFT_EDGE_COL (w)),
	 Fcons (make_number (WINDOW_TOP_EDGE_LINE (w)),
	 Fcons (make_number (WINDOW_RIGHT_EDGE_COL (w)),
	 Fcons (make_number (WINDOW_BOTTOM_EDGE_LINE (w)),
		Qnil))));
}

DEFUN ("window-pixel-edges", Fwindow_pixel_edges, Swindow_pixel_edges, 0, 1, 0,
       doc: /* Return a list of the edge pixel coordinates of WINDOW.
The list has the form (LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at
the top left corner of the frame.

RIGHT is one more than the rightmost x position occupied by WINDOW.
BOTTOM is one more than the bottommost y position occupied by WINDOW.
The pixel edges include the space used by WINDOW's scroll bar, display
margins, fringes, header line, and/or mode line.  For the pixel edges
of just the text area, use `window-inside-pixel-edges'.  */)
  (Lisp_Object window)
{
  register struct window *w = decode_any_window (window);

  return Fcons (make_number (WINDOW_LEFT_EDGE_X (w)),
	 Fcons (make_number (WINDOW_TOP_EDGE_Y (w)),
	 Fcons (make_number (WINDOW_RIGHT_EDGE_X (w)),
	 Fcons (make_number (WINDOW_BOTTOM_EDGE_Y (w)),
		Qnil))));
}

static void
calc_absolute_offset (struct window *w, int *add_x, int *add_y)
{
  struct frame *f = XFRAME (w->frame);
  *add_y = f->top_pos;
#ifdef FRAME_MENUBAR_HEIGHT
  *add_y += FRAME_MENUBAR_HEIGHT (f);
#endif
#ifdef FRAME_TOOLBAR_TOP_HEIGHT
  *add_y += FRAME_TOOLBAR_TOP_HEIGHT (f);
#elif FRAME_TOOLBAR_HEIGHT
  *add_y += FRAME_TOOLBAR_HEIGHT (f);
#endif
#ifdef FRAME_NS_TITLEBAR_HEIGHT
  *add_y += FRAME_NS_TITLEBAR_HEIGHT (f);
#endif
  *add_x = f->left_pos;
#ifdef FRAME_TOOLBAR_LEFT_WIDTH
  *add_x += FRAME_TOOLBAR_LEFT_WIDTH (f);
#endif
}

DEFUN ("window-absolute-pixel-edges", Fwindow_absolute_pixel_edges,
       Swindow_absolute_pixel_edges, 0, 1, 0,
       doc: /* Return a list of the edge pixel coordinates of WINDOW.
The list has the form (LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at
the top left corner of the display.

RIGHT is one more than the rightmost x position occupied by WINDOW.
BOTTOM is one more than the bottommost y position occupied by WINDOW.
The pixel edges include the space used by WINDOW's scroll bar, display
margins, fringes, header line, and/or mode line.  For the pixel edges
of just the text area, use `window-inside-absolute-pixel-edges'.  */)
  (Lisp_Object window)
{
  register struct window *w = decode_any_window (window);
  int add_x, add_y;
  calc_absolute_offset (w, &add_x, &add_y);

  return Fcons (make_number (WINDOW_LEFT_EDGE_X (w) + add_x),
         Fcons (make_number (WINDOW_TOP_EDGE_Y (w) + add_y),
	 Fcons (make_number (WINDOW_RIGHT_EDGE_X (w) + add_x),
	 Fcons (make_number (WINDOW_BOTTOM_EDGE_Y (w) + add_y),
		Qnil))));
}

DEFUN ("window-inside-edges", Fwindow_inside_edges, Swindow_inside_edges, 0, 1, 0,
       doc: /* Return a list of the edge coordinates of WINDOW.
The list has the form (LEFT TOP RIGHT BOTTOM).
TOP and BOTTOM count by lines, and LEFT and RIGHT count by columns,
all relative to 0, 0 at top left corner of frame.

RIGHT is one more than the rightmost column of WINDOW's text area.
BOTTOM is one more than the bottommost row of WINDOW's text area.
The inside edges do not include the space used by the WINDOW's scroll
bar, display margins, fringes, header line, and/or mode line.  */)
  (Lisp_Object window)
{
  register struct window *w = decode_window (window);

  return list4 (make_number (WINDOW_BOX_LEFT_EDGE_COL (w)
			     + WINDOW_LEFT_MARGIN_COLS (w)
			     + WINDOW_LEFT_FRINGE_COLS (w)),
		make_number (WINDOW_TOP_EDGE_LINE (w)
			     + WINDOW_HEADER_LINE_LINES (w)),
		make_number (WINDOW_BOX_RIGHT_EDGE_COL (w)
			     - WINDOW_RIGHT_MARGIN_COLS (w)
			     - WINDOW_RIGHT_FRINGE_COLS (w)),
		make_number (WINDOW_BOTTOM_EDGE_LINE (w)
			     - WINDOW_MODE_LINE_LINES (w)));
}

DEFUN ("window-inside-pixel-edges", Fwindow_inside_pixel_edges, Swindow_inside_pixel_edges, 0, 1, 0,
       doc: /* Return a list of the edge pixel coordinates of WINDOW's text area.
The list has the form (LEFT TOP RIGHT BOTTOM), all relative to (0,0)
at the top left corner of the frame's window area.

RIGHT is one more than the rightmost x position of WINDOW's text area.
BOTTOM is one more than the bottommost y position of WINDOW's text area.
The inside edges do not include the space used by WINDOW's scroll bar,
display margins, fringes, header line, and/or mode line.  */)
  (Lisp_Object window)
{
  register struct window *w = decode_window (window);

  return list4 (make_number (WINDOW_BOX_LEFT_EDGE_X (w)
			     + WINDOW_LEFT_MARGIN_WIDTH (w)
			     + WINDOW_LEFT_FRINGE_WIDTH (w)),
		make_number (WINDOW_TOP_EDGE_Y (w)
			     + WINDOW_HEADER_LINE_HEIGHT (w)),
		make_number (WINDOW_BOX_RIGHT_EDGE_X (w)
			     - WINDOW_RIGHT_MARGIN_WIDTH (w)
			     - WINDOW_RIGHT_FRINGE_WIDTH (w)),
		make_number (WINDOW_BOTTOM_EDGE_Y (w)
			     - WINDOW_MODE_LINE_HEIGHT (w)));
}

DEFUN ("window-inside-absolute-pixel-edges",
       Fwindow_inside_absolute_pixel_edges,
       Swindow_inside_absolute_pixel_edges, 0, 1, 0,
       doc: /* Return a list of the edge pixel coordinates of WINDOW's text area.
The list has the form (LEFT TOP RIGHT BOTTOM), all relative to (0,0)
at the top left corner of the frame's window area.

RIGHT is one more than the rightmost x position of WINDOW's text area.
BOTTOM is one more than the bottommost y position of WINDOW's text area.
The inside edges do not include the space used by WINDOW's scroll bar,
display margins, fringes, header line, and/or mode line.  */)
  (Lisp_Object window)
{
  register struct window *w = decode_window (window);
  int add_x, add_y;
  calc_absolute_offset (w, &add_x, &add_y);

  return list4 (make_number (WINDOW_BOX_LEFT_EDGE_X (w)
			     + WINDOW_LEFT_MARGIN_WIDTH (w)
			     + WINDOW_LEFT_FRINGE_WIDTH (w) + add_x),
		make_number (WINDOW_TOP_EDGE_Y (w)
			     + WINDOW_HEADER_LINE_HEIGHT (w) + add_y),
		make_number (WINDOW_BOX_RIGHT_EDGE_X (w)
			     - WINDOW_RIGHT_MARGIN_WIDTH (w)
			     - WINDOW_RIGHT_FRINGE_WIDTH (w) + add_x),
		make_number (WINDOW_BOTTOM_EDGE_Y (w)
			     - WINDOW_MODE_LINE_HEIGHT (w) + add_y));
}

/* Test if the character at column X, row Y is within window W.
   If it is not, return ON_NOTHING;
   if it is in the window's text area, return ON_TEXT;
   if it is on the window's modeline, return ON_MODE_LINE;
   if it is on the border between the window and its right sibling,
      return ON_VERTICAL_BORDER.
   if it is on a scroll bar, return ON_SCROLL_BAR.
   if it is on the window's top line, return ON_HEADER_LINE;
   if it is in left or right fringe of the window,
      return ON_LEFT_FRINGE or ON_RIGHT_FRINGE;
   if it is in the marginal area to the left/right of the window,
      return ON_LEFT_MARGIN or ON_RIGHT_MARGIN.

   X and Y are frame relative pixel coordinates.  */

static enum window_part
coordinates_in_window (register struct window *w, int x, int y)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  enum window_part part;
  int ux = FRAME_COLUMN_WIDTH (f);
  int left_x = WINDOW_LEFT_EDGE_X (w);
  int right_x = WINDOW_RIGHT_EDGE_X (w);
  int top_y = WINDOW_TOP_EDGE_Y (w);
  int bottom_y = WINDOW_BOTTOM_EDGE_Y (w);
  /* The width of the area where the vertical line can be dragged.
     (Between mode lines for instance.  */
  int grabbable_width = ux;
  int lmargin_width, rmargin_width, text_left, text_right;

  /* Outside any interesting row or column?  */
  if (y < top_y || y >= bottom_y || x < left_x || x >= right_x)
    return ON_NOTHING;

  /* On the mode line or header line?   */
  if ((WINDOW_WANTS_MODELINE_P (w)
       && y >= bottom_y - CURRENT_MODE_LINE_HEIGHT (w)
       && (part = ON_MODE_LINE))
      || (WINDOW_WANTS_HEADER_LINE_P (w)
	  && y < top_y + CURRENT_HEADER_LINE_HEIGHT (w)
	  && (part = ON_HEADER_LINE)))
    {
      /* If it's under/over the scroll bar portion of the mode/header
	 line, say it's on the vertical line.  That's to be able to
	 resize windows horizontally in case we're using toolkit scroll
	 bars.  Note: If scrollbars are on the left, the window that
	 must be eventually resized is that on the left of WINDOW.  */
      if ((WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w)
	   && !WINDOW_LEFTMOST_P (w)
	   && eabs (x - left_x) < grabbable_width)
	  || (!WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w)
	      && !WINDOW_RIGHTMOST_P (w)
	      && eabs (x - right_x) < grabbable_width))
	return ON_VERTICAL_BORDER;
      else
	return part;
    }

  /* In what's below, we subtract 1 when computing right_x because we
     want the rightmost pixel, which is given by left_pixel+width-1.  */
  if (w->pseudo_window_p)
    {
      left_x = 0;
      right_x = WINDOW_TOTAL_WIDTH (w) - 1;
    }
  else
    {
      left_x = WINDOW_BOX_LEFT_EDGE_X (w);
      right_x = WINDOW_BOX_RIGHT_EDGE_X (w) - 1;
    }

  /* Outside any interesting column?  */
  if (x < left_x || x > right_x)
    return ON_SCROLL_BAR;

  lmargin_width = window_box_width (w, LEFT_MARGIN_AREA);
  rmargin_width = window_box_width (w, RIGHT_MARGIN_AREA);

  text_left = window_box_left (w, TEXT_AREA);
  text_right = text_left + window_box_width (w, TEXT_AREA);

  if (FRAME_WINDOW_P (f))
    {
      if (!w->pseudo_window_p
	  && !WINDOW_HAS_VERTICAL_SCROLL_BAR (w)
	  && !WINDOW_RIGHTMOST_P (w)
	  && (eabs (x - right_x) < grabbable_width))
	return ON_VERTICAL_BORDER;
    }
  /* Need to say "x > right_x" rather than >=, since on character
     terminals, the vertical line's x coordinate is right_x.  */
  else if (!w->pseudo_window_p
	   && !WINDOW_RIGHTMOST_P (w)
	   && x > right_x - ux)
    return ON_VERTICAL_BORDER;

  if (x < text_left)
    {
      if (lmargin_width > 0
	  && (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
	      ? (x >= left_x + WINDOW_LEFT_FRINGE_WIDTH (w))
	      : (x < left_x + lmargin_width)))
	return ON_LEFT_MARGIN;

      return ON_LEFT_FRINGE;
    }

  if (x >= text_right)
    {
      if (rmargin_width > 0
	  && (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
	      ? (x < right_x - WINDOW_RIGHT_FRINGE_WIDTH (w))
	      : (x >= right_x - rmargin_width)))
	return ON_RIGHT_MARGIN;

      return ON_RIGHT_FRINGE;
    }

  /* Everything special ruled out - must be on text area */
  return ON_TEXT;
}

/* Take X is the frame-relative pixel x-coordinate, and return the
   x-coordinate relative to part PART of window W. */
int
window_relative_x_coord (struct window *w, enum window_part part, int x)
{
  int left_x = (w->pseudo_window_p) ? 0 : WINDOW_BOX_LEFT_EDGE_X (w);

  switch (part)
    {
    case ON_TEXT:
      return x - window_box_left (w, TEXT_AREA);

    case ON_LEFT_FRINGE:
      return x - left_x;

    case ON_RIGHT_FRINGE:
      return x - left_x - WINDOW_LEFT_FRINGE_WIDTH (w);

    case ON_LEFT_MARGIN:
      return (x - left_x
	      - ((WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w))
		 ? WINDOW_LEFT_FRINGE_WIDTH (w) : 0));

    case ON_RIGHT_MARGIN:
      return (x + 1
	      - ((w->pseudo_window_p)
		 ? WINDOW_TOTAL_WIDTH (w)
		 : WINDOW_BOX_RIGHT_EDGE_X (w))
	      + window_box_width (w, RIGHT_MARGIN_AREA)
	      + ((WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w))
		 ? WINDOW_RIGHT_FRINGE_WIDTH (w) : 0));
    }

  /* ON_SCROLL_BAR, ON_NOTHING, and ON_VERTICAL_BORDER:  */
  return 0;
}


DEFUN ("coordinates-in-window-p", Fcoordinates_in_window_p,
       Scoordinates_in_window_p, 2, 2, 0,
       doc: /* Return non-nil if COORDINATES are in WINDOW.
WINDOW must be a live window.
COORDINATES is a cons of the form (X . Y), X and Y being distances
measured in characters from the upper-left corner of the frame.
\(0 . 0) denotes the character in the upper left corner of the
frame.
If COORDINATES are in the text portion of WINDOW,
   the coordinates relative to the window are returned.
If they are in the mode line of WINDOW, `mode-line' is returned.
If they are in the top mode line of WINDOW, `header-line' is returned.
If they are in the left fringe of WINDOW, `left-fringe' is returned.
If they are in the right fringe of WINDOW, `right-fringe' is returned.
If they are on the border between WINDOW and its right sibling,
  `vertical-line' is returned.
If they are in the windows's left or right marginal areas, `left-margin'\n\
  or `right-margin' is returned.  */)
  (register Lisp_Object coordinates, Lisp_Object window)
{
  struct window *w;
  struct frame *f;
  int x, y;
  Lisp_Object lx, ly;

  CHECK_LIVE_WINDOW (window);
  w = XWINDOW (window);
  f = XFRAME (w->frame);
  CHECK_CONS (coordinates);
  lx = Fcar (coordinates);
  ly = Fcdr (coordinates);
  CHECK_NUMBER_OR_FLOAT (lx);
  CHECK_NUMBER_OR_FLOAT (ly);
  x = FRAME_PIXEL_X_FROM_CANON_X (f, lx) + FRAME_INTERNAL_BORDER_WIDTH (f);
  y = FRAME_PIXEL_Y_FROM_CANON_Y (f, ly) + FRAME_INTERNAL_BORDER_WIDTH (f);

  switch (coordinates_in_window (w, x, y))
    {
    case ON_NOTHING:
      return Qnil;

    case ON_TEXT:
      /* Convert X and Y to window relative pixel coordinates, and
	 return the canonical char units.  */
      x -= window_box_left (w, TEXT_AREA);
      y -= WINDOW_TOP_EDGE_Y (w);
      return Fcons (FRAME_CANON_X_FROM_PIXEL_X (f, x),
		    FRAME_CANON_Y_FROM_PIXEL_Y (f, y));

    case ON_MODE_LINE:
      return Qmode_line;

    case ON_VERTICAL_BORDER:
      return Qvertical_line;

    case ON_HEADER_LINE:
      return Qheader_line;

    case ON_LEFT_FRINGE:
      return Qleft_fringe;

    case ON_RIGHT_FRINGE:
      return Qright_fringe;

    case ON_LEFT_MARGIN:
      return Qleft_margin;

    case ON_RIGHT_MARGIN:
      return Qright_margin;

    case ON_SCROLL_BAR:
      /* Historically we are supposed to return nil in this case.  */
      return Qnil;

    default:
      abort ();
    }
}


/* Callback for foreach_window, used in window_from_coordinates.
   Check if window W contains coordinates specified by USER_DATA which
   is actually a pointer to a struct check_window_data CW.

   Check if window W contains coordinates *CW->x and *CW->y.  If it
   does, return W in *CW->window, as Lisp_Object, and return in
   *CW->part the part of the window under coordinates *X,*Y.  Return
   zero from this function to stop iterating over windows.  */

struct check_window_data
{
  Lisp_Object *window;
  int x, y;
  enum window_part *part;
};

static int
check_window_containing (struct window *w, void *user_data)
{
  struct check_window_data *cw = (struct check_window_data *) user_data;
  enum window_part found;
  int continue_p = 1;

  found = coordinates_in_window (w, cw->x, cw->y);
  if (found != ON_NOTHING)
    {
      *cw->part = found;
      XSETWINDOW (*cw->window, w);
      continue_p = 0;
    }

  return continue_p;
}


/* Find the window containing frame-relative pixel position X/Y and
   return it as a Lisp_Object.

   If X, Y is on one of the window's special `window_part' elements,
   set *PART to the id of that element.

   If there is no window under X, Y return nil and leave *PART
   unmodified.  TOOL_BAR_P non-zero means detect tool-bar windows.

   This function was previously implemented with a loop cycling over
   windows with Fnext_window, and starting with the frame's selected
   window.  It turned out that this doesn't work with an
   implementation of next_window using Vwindow_list, because
   FRAME_SELECTED_WINDOW (F) is not always contained in the window
   tree of F when this function is called asynchronously from
   note_mouse_highlight.  The original loop didn't terminate in this
   case.  */

Lisp_Object
window_from_coordinates (struct frame *f, int x, int y,
			 enum window_part *part, int tool_bar_p)
{
  Lisp_Object window;
  struct check_window_data cw;
  enum window_part dummy;

  if (part == 0)
    part = &dummy;

  window = Qnil;
  cw.window = &window, cw.x = x, cw.y = y; cw.part = part;
  foreach_window (f, check_window_containing, &cw);

  /* If not found above, see if it's in the tool bar window, if a tool
     bar exists.  */
  if (NILP (window)
      && tool_bar_p
      && WINDOWP (f->tool_bar_window)
      && WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)) > 0
      && (coordinates_in_window (XWINDOW (f->tool_bar_window), x, y)
	  != ON_NOTHING))
    {
      *part = ON_TEXT;
      window = f->tool_bar_window;
    }

  return window;
}

DEFUN ("window-at", Fwindow_at, Swindow_at, 2, 3, 0,
       doc: /* Return window containing coordinates X and Y on FRAME.
FRAME must be a live frame and defaults to the selected one.
The top left corner of the frame is considered to be row 0,
column 0.  */)
  (Lisp_Object x, Lisp_Object y, Lisp_Object frame)
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* Check that arguments are integers or floats.  */
  CHECK_NUMBER_OR_FLOAT (x);
  CHECK_NUMBER_OR_FLOAT (y);

  return window_from_coordinates (f,
				  (FRAME_PIXEL_X_FROM_CANON_X (f, x)
				   + FRAME_INTERNAL_BORDER_WIDTH (f)),
				  (FRAME_PIXEL_Y_FROM_CANON_Y (f, y)
				   + FRAME_INTERNAL_BORDER_WIDTH (f)),
				  0, 0);
}

DEFUN ("window-point", Fwindow_point, Swindow_point, 0, 1, 0,
       doc: /* Return current value of point in WINDOW.
WINDOW must be a live window and defaults to the selected one.

For a nonselected window, this is the value point would have
if that window were selected.

Note that, when WINDOW is the selected window and its buffer
is also currently selected, the value returned is the same as (point).
It would be more strictly correct to return the `top-level' value
of point, outside of any save-excursion forms.
But that is hard to define.  */)
  (Lisp_Object window)
{
  register struct window *w = decode_window (window);

  if (w == XWINDOW (selected_window)
      && current_buffer == XBUFFER (w->buffer))
    return Fpoint ();
  return Fmarker_position (w->pointm);
}

DEFUN ("window-start", Fwindow_start, Swindow_start, 0, 1, 0,
       doc: /* Return position at which display currently starts in WINDOW.
WINDOW must be a live window and defaults to the selected one.
This is updated by redisplay or by calling `set-window-start'.  */)
  (Lisp_Object window)
{
  return Fmarker_position (decode_window (window)->start);
}

/* This is text temporarily removed from the doc string below.

This function returns nil if the position is not currently known.
That happens when redisplay is preempted and doesn't finish.
If in that case you want to compute where the end of the window would
have been if redisplay had finished, do this:
    (save-excursion
      (goto-char (window-start window))
      (vertical-motion (1- (window-height window)) window)
      (point))")  */

DEFUN ("window-end", Fwindow_end, Swindow_end, 0, 2, 0,
       doc: /* Return position at which display currently ends in WINDOW.
WINDOW must be a live window and defaults to the selected one.
This is updated by redisplay, when it runs to completion.
Simply changing the buffer text or setting `window-start'
does not update this value.
Return nil if there is no recorded value.  (This can happen if the
last redisplay of WINDOW was preempted, and did not finish.)
If UPDATE is non-nil, compute the up-to-date position
if it isn't already recorded.  */)
  (Lisp_Object window, Lisp_Object update)
{
  Lisp_Object value;
  struct window *w = decode_window (window);
  Lisp_Object buf;
  struct buffer *b;

  buf = w->buffer;
  CHECK_BUFFER (buf);
  b = XBUFFER (buf);

#if 0 /* This change broke some things.  We should make it later.  */
  /* If we don't know the end position, return nil.
     The user can compute it with vertical-motion if he wants to.
     It would be nicer to do it automatically,
     but that's so slow that it would probably bother people.  */
  if (NILP (w->window_end_valid))
    return Qnil;
#endif

  if (! NILP (update)
      && ! (! NILP (w->window_end_valid)
	    && XFASTINT (w->last_modified) >= BUF_MODIFF (b)
	    && XFASTINT (w->last_overlay_modified) >= BUF_OVERLAY_MODIFF (b))
      && !noninteractive)
    {
      struct text_pos startp;
      struct it it;
      struct buffer *old_buffer = NULL;
      void *itdata = NULL;

      /* Cannot use Fvertical_motion because that function doesn't
	 cope with variable-height lines.  */
      if (b != current_buffer)
	{
	  old_buffer = current_buffer;
	  set_buffer_internal (b);
	}

      /* In case W->start is out of the range, use something
         reasonable.  This situation occurred when loading a file with
         `-l' containing a call to `rmail' with subsequent other
         commands.  At the end, W->start happened to be BEG, while
         rmail had already narrowed the buffer.  */
      if (XMARKER (w->start)->charpos < BEGV)
	SET_TEXT_POS (startp, BEGV, BEGV_BYTE);
      else if (XMARKER (w->start)->charpos > ZV)
	SET_TEXT_POS (startp, ZV, ZV_BYTE);
      else
	SET_TEXT_POS_FROM_MARKER (startp, w->start);

      itdata = bidi_shelve_cache ();
      start_display (&it, w, startp);
      move_it_vertically (&it, window_box_height (w));
      if (it.current_y < it.last_visible_y)
	move_it_past_eol (&it);
      value = make_number (IT_CHARPOS (it));
      bidi_unshelve_cache (itdata, 0);

      if (old_buffer)
	set_buffer_internal (old_buffer);
    }
  else
    XSETINT (value, BUF_Z (b) - XFASTINT (w->window_end_pos));

  return value;
}

DEFUN ("set-window-point", Fset_window_point, Sset_window_point, 2, 2, 0,
       doc: /* Make point value in WINDOW be at position POS in WINDOW's buffer.
Return POS.  */)
  (Lisp_Object window, Lisp_Object pos)
{
  register struct window *w = decode_window (window);

  CHECK_NUMBER_COERCE_MARKER (pos);
  if (w == XWINDOW (selected_window)
      && XBUFFER (w->buffer) == current_buffer)
    Fgoto_char (pos);
  else
    set_marker_restricted (w->pointm, pos, w->buffer);

  /* We have to make sure that redisplay updates the window to show
     the new value of point.  */
  if (!EQ (window, selected_window))
    ++windows_or_buffers_changed;

  return pos;
}

DEFUN ("set-window-start", Fset_window_start, Sset_window_start, 2, 3, 0,
       doc: /* Make display in WINDOW start at position POS in WINDOW's buffer.
If WINDOW is nil, the selected window is used.  Return POS.
Optional third arg NOFORCE non-nil inhibits next redisplay from
overriding motion of point in order to display at this exact start.  */)
  (Lisp_Object window, Lisp_Object pos, Lisp_Object noforce)
{
  register struct window *w = decode_window (window);

  CHECK_NUMBER_COERCE_MARKER (pos);
  set_marker_restricted (w->start, pos, w->buffer);
  /* this is not right, but much easier than doing what is right. */
  w->start_at_line_beg = Qnil;
  if (NILP (noforce))
    w->force_start = Qt;
  w->update_mode_line = Qt;
  XSETFASTINT (w->last_modified, 0);
  XSETFASTINT (w->last_overlay_modified, 0);
  if (!EQ (window, selected_window))
    windows_or_buffers_changed++;

  return pos;
}

DEFUN ("pos-visible-in-window-p", Fpos_visible_in_window_p,
       Spos_visible_in_window_p, 0, 3, 0,
       doc: /* Return non-nil if position POS is currently on the frame in WINDOW.
Return nil if that position is scrolled vertically out of view.
If a character is only partially visible, nil is returned, unless the
optional argument PARTIALLY is non-nil.
If POS is only out of view because of horizontal scrolling, return non-nil.
If POS is t, it specifies the position of the last visible glyph in WINDOW.
POS defaults to point in WINDOW; WINDOW defaults to the selected window.

If POS is visible, return t if PARTIALLY is nil; if PARTIALLY is non-nil,
return value is a list of 2 or 6 elements (X Y [RTOP RBOT ROWH VPOS]),
where X and Y are the pixel coordinates relative to the top left corner
of the window.  The remaining elements are omitted if the character after
POS is fully visible; otherwise, RTOP and RBOT are the number of pixels
off-window at the top and bottom of the row, ROWH is the height of the
display row, and VPOS is the row number (0-based) containing POS.  */)
  (Lisp_Object pos, Lisp_Object window, Lisp_Object partially)
{
  register struct window *w;
  register EMACS_INT posint;
  register struct buffer *buf;
  struct text_pos top;
  Lisp_Object in_window = Qnil;
  int rtop, rbot, rowh, vpos, fully_p = 1;
  int x, y;

  w = decode_window (window);
  buf = XBUFFER (w->buffer);
  SET_TEXT_POS_FROM_MARKER (top, w->start);

  if (EQ (pos, Qt))
    posint = -1;
  else if (!NILP (pos))
    {
      CHECK_NUMBER_COERCE_MARKER (pos);
      posint = XINT (pos);
    }
  else if (w == XWINDOW (selected_window))
    posint = PT;
  else
    posint = XMARKER (w->pointm)->charpos;

  /* If position is above window start or outside buffer boundaries,
     or if window start is out of range, position is not visible.  */
  if ((EQ (pos, Qt)
       || (posint >= CHARPOS (top) && posint <= BUF_ZV (buf)))
      && CHARPOS (top) >= BUF_BEGV (buf)
      && CHARPOS (top) <= BUF_ZV (buf)
      && pos_visible_p (w, posint, &x, &y, &rtop, &rbot, &rowh, &vpos)
      && (fully_p = !rtop && !rbot, (!NILP (partially) || fully_p)))
    in_window = Qt;

  if (!NILP (in_window) && !NILP (partially))
    {
      Lisp_Object part = Qnil;
      if (!fully_p)
	part = list4 (make_number (rtop), make_number (rbot),
			make_number (rowh), make_number (vpos));
      in_window = Fcons (make_number (x),
			 Fcons (make_number (y), part));
    }

  return in_window;
}

DEFUN ("window-line-height", Fwindow_line_height,
       Swindow_line_height, 0, 2, 0,
       doc: /* Return height in pixels of text line LINE in window WINDOW.
WINDOW defaults to the selected window.

Return height of current line if LINE is omitted or nil.  Return height of
header or mode line if LINE is `header-line' or `mode-line'.
Otherwise, LINE is a text line number starting from 0.  A negative number
counts from the end of the window.

Value is a list (HEIGHT VPOS YPOS OFFBOT), where HEIGHT is the height
in pixels of the visible part of the line, VPOS and YPOS are the
vertical position in lines and pixels of the line, relative to the top
of the first text line, and OFFBOT is the number of off-window pixels at
the bottom of the text line.  If there are off-window pixels at the top
of the (first) text line, YPOS is negative.

Return nil if window display is not up-to-date.  In that case, use
`pos-visible-in-window-p' to obtain the information.  */)
  (Lisp_Object line, Lisp_Object window)
{
  register struct window *w;
  register struct buffer *b;
  struct glyph_row *row, *end_row;
  int max_y, crop, i, n;

  w = decode_window (window);

  if (noninteractive || w->pseudo_window_p)
    return Qnil;

  CHECK_BUFFER (w->buffer);
  b = XBUFFER (w->buffer);

  /* Fail if current matrix is not up-to-date.  */
  if (NILP (w->window_end_valid)
      || current_buffer->clip_changed
      || current_buffer->prevent_redisplay_optimizations_p
      || XFASTINT (w->last_modified) < BUF_MODIFF (b)
      || XFASTINT (w->last_overlay_modified) < BUF_OVERLAY_MODIFF (b))
    return Qnil;

  if (NILP (line))
    {
      i = w->cursor.vpos;
      if (i < 0 || i >= w->current_matrix->nrows
	  || (row = MATRIX_ROW (w->current_matrix, i), !row->enabled_p))
	return Qnil;
      max_y = window_text_bottom_y (w);
      goto found_row;
    }

  if (EQ (line, Qheader_line))
    {
      if (!WINDOW_WANTS_HEADER_LINE_P (w))
	return Qnil;
      row = MATRIX_HEADER_LINE_ROW (w->current_matrix);
      if (!row->enabled_p)
	return Qnil;
      return list4 (make_number (row->height),
		    make_number (0), make_number (0),
		    make_number (0));
    }

  if (EQ (line, Qmode_line))
    {
      row = MATRIX_MODE_LINE_ROW (w->current_matrix);
      if (!row->enabled_p)
	return Qnil;
      return list4 (make_number (row->height),
		    make_number (0), /* not accurate */
		    make_number (WINDOW_HEADER_LINE_HEIGHT (w)
				 + window_text_bottom_y (w)),
		    make_number (0));
    }

  CHECK_NUMBER (line);
  n = XINT (line);

  row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  end_row = MATRIX_BOTTOM_TEXT_ROW (w->current_matrix, w);
  max_y = window_text_bottom_y (w);
  i = 0;

  while ((n < 0 || i < n)
	 && row <= end_row && row->enabled_p
	 && row->y + row->height < max_y)
    row++, i++;

  if (row > end_row || !row->enabled_p)
    return Qnil;

  if (++n < 0)
    {
      if (-n > i)
	return Qnil;
      row += n;
      i += n;
    }

 found_row:
  crop = max (0, (row->y + row->height) - max_y);
  return list4 (make_number (row->height + min (0, row->y) - crop),
		make_number (i),
		make_number (row->y),
		make_number (crop));
}

DEFUN ("window-dedicated-p", Fwindow_dedicated_p, Swindow_dedicated_p,
       0, 1, 0,
       doc: /* Return non-nil when WINDOW is dedicated to its buffer.
More precisely, return the value assigned by the last call of
`set-window-dedicated-p' for WINDOW.  Return nil if that function was
never called with WINDOW as its argument, or the value set by that
function was internally reset since its last call.  WINDOW defaults to
the selected window.

When a window is dedicated to its buffer, `display-buffer' will refrain
from displaying another buffer in it.  `get-lru-window' and
`get-largest-window' treat dedicated windows specially.
`delete-windows-on', `replace-buffer-in-windows', `quit-window' and
`kill-buffer' can delete a dedicated window and the containing frame.

Functions like `set-window-buffer' may change the buffer displayed by a
window, unless that window is "strongly" dedicated to its buffer, that
is the value returned by `window-dedicated-p' is t.  */)
  (Lisp_Object window)
{
  return decode_window (window)->dedicated;
}

DEFUN ("set-window-dedicated-p", Fset_window_dedicated_p,
       Sset_window_dedicated_p, 2, 2, 0,
       doc: /* Mark WINDOW as dedicated according to FLAG.
WINDOW must be a live window and defaults to the selected one.  FLAG
non-nil means mark WINDOW as dedicated to its buffer.  FLAG nil means
mark WINDOW as non-dedicated.  Return FLAG.

When a window is dedicated to its buffer, `display-buffer' will refrain
from displaying another buffer in it.  `get-lru-window' and
`get-largest-window' treat dedicated windows specially.
`delete-windows-on', `replace-buffer-in-windows', `quit-window',
`quit-restore-window' and `kill-buffer' can delete a dedicated window
and the containing frame.

As a special case, if FLAG is t, mark WINDOW as "strongly" dedicated to
its buffer.  Functions like `set-window-buffer' may change the buffer
displayed by a window, unless that window is strongly dedicated to its
buffer.  If and when `set-window-buffer' displays another buffer in a
window, it also makes sure that the window is no more dedicated.  */)
  (Lisp_Object window, Lisp_Object flag)
{
  register struct window *w = decode_window (window);

  w->dedicated = flag;
  return w->dedicated;
}

DEFUN ("window-prev-buffers", Fwindow_prev_buffers, Swindow_prev_buffers,
       0, 1, 0,
       doc:  /* Return buffers previously shown in WINDOW.
WINDOW must be a live window and defaults to the selected one.

The return value is a list of elements (BUFFER WINDOW-START POS),
where BUFFER is a buffer, WINDOW-START is the start position of the
window for that buffer, and POS is a window-specific point value.  */)
  (Lisp_Object window)
{
  return decode_window (window)->prev_buffers;
}

DEFUN ("set-window-prev-buffers", Fset_window_prev_buffers,
       Sset_window_prev_buffers, 2, 2, 0,
       doc: /* Set WINDOW's previous buffers to PREV-BUFFERS.
WINDOW must be a live window and defaults to the selected one.

PREV-BUFFERS should be a list of elements (BUFFER WINDOW-START POS),
where BUFFER is a buffer, WINDOW-START is the start position of the
window for that buffer, and POS is a window-specific point value.  */)
     (Lisp_Object window, Lisp_Object prev_buffers)
{
  return decode_window (window)->prev_buffers = prev_buffers;
}

DEFUN ("window-next-buffers", Fwindow_next_buffers, Swindow_next_buffers,
       0, 1, 0,
       doc:  /* Return list of buffers recently re-shown in WINDOW.
WINDOW must be a live window and defaults to the selected one.  */)
     (Lisp_Object window)
{
  return decode_window (window)->next_buffers;
}

DEFUN ("set-window-next-buffers", Fset_window_next_buffers,
       Sset_window_next_buffers, 2, 2, 0,
       doc: /* Set WINDOW's next buffers to NEXT-BUFFERS.
WINDOW must be a live window and defaults to the selected one.
NEXT-BUFFERS should be a list of buffers.  */)
     (Lisp_Object window, Lisp_Object next_buffers)
{
  return decode_window (window)->next_buffers = next_buffers;
}

DEFUN ("window-parameters", Fwindow_parameters, Swindow_parameters,
       0, 1, 0,
       doc: /* Return the parameters of WINDOW and their values.
WINDOW defaults to the selected window.  The return value is a list of
elements of the form (PARAMETER . VALUE).  */)
  (Lisp_Object window)
{
  return Fcopy_alist (decode_any_window (window)->window_parameters);
}

DEFUN ("window-parameter", Fwindow_parameter, Swindow_parameter,
       2, 2, 0,
       doc:  /* Return WINDOW's value for PARAMETER.
WINDOW defaults to the selected window.  */)
  (Lisp_Object window, Lisp_Object parameter)
{
  Lisp_Object result;

  result = Fassq (parameter, decode_any_window (window)->window_parameters);
  return CDR_SAFE (result);
}

DEFUN ("set-window-parameter", Fset_window_parameter,
       Sset_window_parameter, 3, 3, 0,
       doc: /* Set WINDOW's value of PARAMETER to VALUE.
WINDOW defaults to the selected window.  Return VALUE.  */)
  (Lisp_Object window, Lisp_Object parameter, Lisp_Object value)
{
  register struct window *w = decode_any_window (window);
  Lisp_Object old_alist_elt;

  old_alist_elt = Fassq (parameter, w->window_parameters);
  if (NILP (old_alist_elt))
    w->window_parameters = Fcons (Fcons (parameter, value), w->window_parameters);
  else
    Fsetcdr (old_alist_elt, value);
  return value;
}

DEFUN ("window-display-table", Fwindow_display_table, Swindow_display_table,
       0, 1, 0,
       doc: /* Return the display-table that WINDOW is using.
WINDOW defaults to the selected window.  */)
  (Lisp_Object window)
{
  return decode_window (window)->display_table;
}

/* Get the display table for use on window W.  This is either W's
   display table or W's buffer's display table.  Ignore the specified
   tables if they are not valid; if no valid table is specified,
   return 0.  */

struct Lisp_Char_Table *
window_display_table (struct window *w)
{
  struct Lisp_Char_Table *dp = NULL;

  if (DISP_TABLE_P (w->display_table))
    dp = XCHAR_TABLE (w->display_table);
  else if (BUFFERP (w->buffer))
    {
      struct buffer *b = XBUFFER (w->buffer);

      if (DISP_TABLE_P (BVAR (b, display_table)))
	dp = XCHAR_TABLE (BVAR (b, display_table));
      else if (DISP_TABLE_P (Vstandard_display_table))
	dp = XCHAR_TABLE (Vstandard_display_table);
    }

  return dp;
}

DEFUN ("set-window-display-table", Fset_window_display_table, Sset_window_display_table, 2, 2, 0,
       doc: /* Set WINDOW's display-table to TABLE.  */)
  (register Lisp_Object window, Lisp_Object table)
{
  register struct window *w;

  w = decode_window (window);
  w->display_table = table;
  return table;
}

/* Record info on buffer window W is displaying
   when it is about to cease to display that buffer.  */
static void
unshow_buffer (register struct window *w)
{
  Lisp_Object buf;
  struct buffer *b;

  buf = w->buffer;
  b = XBUFFER (buf);
  if (b != XMARKER (w->pointm)->buffer)
    abort ();

#if 0
  if (w == XWINDOW (selected_window)
      || ! EQ (buf, XWINDOW (selected_window)->buffer))
    /* Do this except when the selected window's buffer
       is being removed from some other window.  */
#endif
    /* last_window_start records the start position that this buffer
       had in the last window to be disconnected from it.
       Now that this statement is unconditional,
       it is possible for the buffer to be displayed in the
       selected window, while last_window_start reflects another
       window which was recently showing the same buffer.
       Some people might say that might be a good thing.  Let's see.  */
    b->last_window_start = marker_position (w->start);

  /* Point in the selected window's buffer
     is actually stored in that buffer, and the window's pointm isn't used.
     So don't clobber point in that buffer.  */
  if (! EQ (buf, XWINDOW (selected_window)->buffer)
      /* This line helps to fix Horsley's testbug.el bug.  */
      && !(WINDOWP (BVAR (b, last_selected_window))
	   && w != XWINDOW (BVAR (b, last_selected_window))
	   && EQ (buf, XWINDOW (BVAR (b, last_selected_window))->buffer)))
    temp_set_point_both (b,
			 clip_to_bounds (BUF_BEGV (b),
					 XMARKER (w->pointm)->charpos,
					 BUF_ZV (b)),
			 clip_to_bounds (BUF_BEGV_BYTE (b),
					 marker_byte_position (w->pointm),
					 BUF_ZV_BYTE (b)));

  if (WINDOWP (BVAR (b, last_selected_window))
      && w == XWINDOW (BVAR (b, last_selected_window)))
    BVAR (b, last_selected_window) = Qnil;
}

/* Put NEW into the window structure in place of OLD.  SETFLAG zero
   means change window structure only.  Otherwise store geometry and
   other settings as well.  */
static void
replace_window (Lisp_Object old, Lisp_Object new, int setflag)
{
  register Lisp_Object tem;
  register struct window *o = XWINDOW (old), *n = XWINDOW (new);

  /* If OLD is its frame's root window, then NEW is the new
     root window for that frame.  */
  if (EQ (old, FRAME_ROOT_WINDOW (XFRAME (o->frame))))
    FRAME_ROOT_WINDOW (XFRAME (o->frame)) = new;

  if (setflag)
    {
      n->left_col = o->left_col;
      n->top_line = o->top_line;
      n->total_cols = o->total_cols;
      n->total_lines = o->total_lines;
      n->normal_cols = o->normal_cols;
      o->normal_cols = make_float (1.0);
      n->normal_lines = o->normal_lines;
      o->normal_lines = make_float (1.0);
      n->desired_matrix = n->current_matrix = 0;
      n->vscroll = 0;
      memset (&n->cursor, 0, sizeof (n->cursor));
      memset (&n->last_cursor, 0, sizeof (n->last_cursor));
      memset (&n->phys_cursor, 0, sizeof (n->phys_cursor));
      n->phys_cursor_type = -1;
      n->phys_cursor_width = -1;
      n->must_be_updated_p = 0;
      n->pseudo_window_p = 0;
      XSETFASTINT (n->window_end_vpos, 0);
      XSETFASTINT (n->window_end_pos, 0);
      n->window_end_valid = Qnil;
      n->frozen_window_start_p = 0;
    }

  n->next = tem = o->next;
  if (!NILP (tem))
    XWINDOW (tem)->prev = new;

  n->prev = tem = o->prev;
  if (!NILP (tem))
    XWINDOW (tem)->next = new;

  n->parent = tem = o->parent;
  if (!NILP (tem))
    {
      if (EQ (XWINDOW (tem)->vchild, old))
	XWINDOW (tem)->vchild = new;
      if (EQ (XWINDOW (tem)->hchild, old))
	XWINDOW (tem)->hchild = new;
    }
}

/* If window WINDOW and its parent window are iso-combined, merge
   WINDOW's children into those of its parent window and mark WINDOW as
   deleted.  */

static void
recombine_windows (Lisp_Object window)
{
  struct window *w, *p, *c;
  Lisp_Object parent, child;
  int horflag;

  w = XWINDOW (window);
  parent = w->parent;
  if (!NILP (parent) && NILP (w->combination_limit))
    {
      p = XWINDOW (parent);
      if (((!NILP (p->vchild) && !NILP (w->vchild))
	   || (!NILP (p->hchild) && !NILP (w->hchild))))
	/* WINDOW and PARENT are both either a vertical or a horizontal
	   combination.  */
	{
	  horflag = NILP (w->vchild);
	  child = horflag ? w->hchild : w->vchild;
	  c = XWINDOW (child);

	  /* Splice WINDOW's children into its parent's children and
	     assign new normal sizes.  */
	  if (NILP (w->prev))
	    if (horflag)
	      p->hchild = child;
	    else
	      p->vchild = child;
	  else
	    {
	      c->prev = w->prev;
	      XWINDOW (w->prev)->next = child;
	    }

	  while (c)
	    {
	      c->parent = parent;

	      if (horflag)
		c->normal_cols
		  = make_float (XFLOATINT (c->total_cols)
				/ XFLOATINT (p->total_cols));
	      else
		c->normal_lines
		  = make_float (XFLOATINT (c->total_lines)
				/ XFLOATINT (p->total_lines));

	      if (NILP (c->next))
		{
		  if (!NILP (w->next))
		    {
		      c->next = w->next;
		      XWINDOW (c->next)->prev = child;
		    }

		  c = 0;
		}
	      else
		{
		  child = c->next;
		  c = XWINDOW (child);
		}
	    }

	  /* WINDOW can be deleted now.  */
	  w->vchild = w->hchild = Qnil;
	}
    }
}

/* If WINDOW can be deleted, delete it.  */
static void
delete_deletable_window (Lisp_Object window)
{
  if (!NILP (call1 (Qwindow_deletable_p, window)))
    call1 (Qdelete_window, window);
}

/***********************************************************************
			     Window List
 ***********************************************************************/

/* Add window W to *USER_DATA.  USER_DATA is actually a Lisp_Object
   pointer.  This is a callback function for foreach_window, used in
   the window_list function.  */

static int
add_window_to_list (struct window *w, void *user_data)
{
  Lisp_Object *list = (Lisp_Object *) user_data;
  Lisp_Object window;
  XSETWINDOW (window, w);
  *list = Fcons (window, *list);
  return 1;
}


/* Return a list of all windows, for use by next_window.  If
   Vwindow_list is a list, return that list.  Otherwise, build a new
   list, cache it in Vwindow_list, and return that.  */

static Lisp_Object
window_list (void)
{
  if (!CONSP (Vwindow_list))
    {
      Lisp_Object tail;

      Vwindow_list = Qnil;
      for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object args[2];

	  /* We are visiting windows in canonical order, and add
	     new windows at the front of args[1], which means we
	     have to reverse this list at the end.  */
	  args[1] = Qnil;
	  foreach_window (XFRAME (XCAR (tail)), add_window_to_list, &args[1]);
	  args[0] = Vwindow_list;
	  args[1] = Fnreverse (args[1]);
	  Vwindow_list = Fnconc (2, args);
	}
    }

  return Vwindow_list;
}


/* Value is non-zero if WINDOW satisfies the constraints given by
   OWINDOW, MINIBUF and ALL_FRAMES.

   MINIBUF	t means WINDOW may be minibuffer windows.
		`lambda' means WINDOW may not be a minibuffer window.
		a window means a specific minibuffer window

   ALL_FRAMES	t means search all frames,
		nil means search just current frame,
		`visible' means search just visible frames on the
                current terminal,
		0 means search visible and iconified frames on the
                current terminal,
		a window means search the frame that window belongs to,
		a frame means consider windows on that frame, only.  */

static int
candidate_window_p (Lisp_Object window, Lisp_Object owindow, Lisp_Object minibuf, Lisp_Object all_frames)
{
  struct window *w = XWINDOW (window);
  struct frame *f = XFRAME (w->frame);
  int candidate_p = 1;

  if (!BUFFERP (w->buffer))
    candidate_p = 0;
  else if (MINI_WINDOW_P (w)
           && (EQ (minibuf, Qlambda)
	       || (WINDOWP (minibuf) && !EQ (minibuf, window))))
    {
      /* If MINIBUF is `lambda' don't consider any mini-windows.
         If it is a window, consider only that one.  */
      candidate_p = 0;
    }
  else if (EQ (all_frames, Qt))
    candidate_p = 1;
  else if (NILP (all_frames))
    {
      xassert (WINDOWP (owindow));
      candidate_p = EQ (w->frame, XWINDOW (owindow)->frame);
    }
  else if (EQ (all_frames, Qvisible))
    {
      FRAME_SAMPLE_VISIBILITY (f);
      candidate_p = FRAME_VISIBLE_P (f)
	&& (FRAME_TERMINAL (XFRAME (w->frame))
	    == FRAME_TERMINAL (XFRAME (selected_frame)));

    }
  else if (INTEGERP (all_frames) && XINT (all_frames) == 0)
    {
      FRAME_SAMPLE_VISIBILITY (f);
      candidate_p = (FRAME_VISIBLE_P (f) || FRAME_ICONIFIED_P (f)
#ifdef HAVE_X_WINDOWS
		     /* Yuck!!  If we've just created the frame and the
			window-manager requested the user to place it
			manually, the window may still not be considered
			`visible'.  I'd argue it should be at least
			something like `iconified', but don't know how to do
			that yet.  --Stef  */
		     || (FRAME_X_P (f) && f->output_data.x->asked_for_visible
			 && !f->output_data.x->has_been_visible)
#endif
		     )
	&& (FRAME_TERMINAL (XFRAME (w->frame))
	    == FRAME_TERMINAL (XFRAME (selected_frame)));
    }
  else if (WINDOWP (all_frames))
    candidate_p = (EQ (FRAME_MINIBUF_WINDOW (f), all_frames)
		   || EQ (XWINDOW (all_frames)->frame, w->frame)
		   || EQ (XWINDOW (all_frames)->frame, FRAME_FOCUS_FRAME (f)));
  else if (FRAMEP (all_frames))
    candidate_p = EQ (all_frames, w->frame);

  return candidate_p;
}


/* Decode arguments as allowed by Fnext_window, Fprevious_window, and
   Fwindow_list.  See candidate_window_p for the meaning of WINDOW,
   MINIBUF, and ALL_FRAMES.  */

static void
decode_next_window_args (Lisp_Object *window, Lisp_Object *minibuf, Lisp_Object *all_frames)
{
  if (NILP (*window))
    *window = selected_window;
  else
    CHECK_LIVE_WINDOW (*window);

  /* MINIBUF nil may or may not include minibuffers.  Decide if it
     does.  */
  if (NILP (*minibuf))
    *minibuf = minibuf_level ? minibuf_window : Qlambda;
  else if (!EQ (*minibuf, Qt))
    *minibuf = Qlambda;

  /* Now *MINIBUF can be t => count all minibuffer windows, `lambda'
     => count none of them, or a specific minibuffer window (the
     active one) to count.  */

  /* ALL_FRAMES nil doesn't specify which frames to include.  */
  if (NILP (*all_frames))
    *all_frames = (!EQ (*minibuf, Qlambda)
		   ? FRAME_MINIBUF_WINDOW (XFRAME (XWINDOW (*window)->frame))
		   : Qnil);
  else if (EQ (*all_frames, Qvisible))
    ;
  else if (EQ (*all_frames, make_number (0)))
    ;
  else if (FRAMEP (*all_frames))
    ;
  else if (!EQ (*all_frames, Qt))
    *all_frames = Qnil;
}


/* Return the next or previous window of WINDOW in cyclic ordering
   of windows.  NEXT_P non-zero means return the next window.  See the
   documentation string of next-window for the meaning of MINIBUF and
   ALL_FRAMES.  */

static Lisp_Object
next_window (Lisp_Object window, Lisp_Object minibuf, Lisp_Object all_frames, int next_p)
{
  decode_next_window_args (&window, &minibuf, &all_frames);

  /* If ALL_FRAMES is a frame, and WINDOW isn't on that frame, just
     return the first window on the frame.  */
  if (FRAMEP (all_frames)
      && !EQ (all_frames, XWINDOW (window)->frame))
    return Fframe_first_window (all_frames);

  if (next_p)
    {
      Lisp_Object list;

      /* Find WINDOW in the list of all windows.  */
      list = Fmemq (window, window_list ());

      /* Scan forward from WINDOW to the end of the window list.  */
      if (CONSP (list))
	for (list = XCDR (list); CONSP (list); list = XCDR (list))
	  if (candidate_window_p (XCAR (list), window, minibuf, all_frames))
	    break;

      /* Scan from the start of the window list up to WINDOW.  */
      if (!CONSP (list))
	for (list = Vwindow_list;
	     CONSP (list) && !EQ (XCAR (list), window);
	     list = XCDR (list))
	  if (candidate_window_p (XCAR (list), window, minibuf, all_frames))
	    break;

      if (CONSP (list))
	window = XCAR (list);
    }
  else
    {
      Lisp_Object candidate, list;

      /* Scan through the list of windows for candidates.  If there are
	 candidate windows in front of WINDOW, the last one of these
	 is the one we want.  If there are candidates following WINDOW
	 in the list, again the last one of these is the one we want.  */
      candidate = Qnil;
      for (list = window_list (); CONSP (list); list = XCDR (list))
	{
	  if (EQ (XCAR (list), window))
	    {
	      if (WINDOWP (candidate))
		break;
	    }
	  else if (candidate_window_p (XCAR (list), window, minibuf,
				       all_frames))
	    candidate = XCAR (list);
	}

      if (WINDOWP (candidate))
	window = candidate;
    }

  return window;
}


DEFUN ("next-window", Fnext_window, Snext_window, 0, 3, 0,
       doc: /* Return live window after WINDOW in the cyclic ordering of windows.
WINDOW must be a live window and defaults to the selected one.  The
optional arguments MINIBUF and ALL-FRAMES specify the set of windows to
consider.

MINIBUF nil or omitted means consider the minibuffer window only if the
minibuffer is active.  MINIBUF t means consider the minibuffer window
even if the minibuffer is not active.  Any other value means do not
consider the minibuffer window even if the minibuffer is active.

ALL-FRAMES nil or omitted means consider all windows on WINDOW's frame,
plus the minibuffer window if specified by the MINIBUF argument.  If the
minibuffer counts, consider all windows on all frames that share that
minibuffer too.  The following non-nil values of ALL-FRAMES have special
meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible and
  iconified frames.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on WINDOW's frame and no
others.

If you use consistent values for MINIBUF and ALL-FRAMES, you can use
`next-window' to iterate through the entire cycle of acceptable
windows, eventually ending up back at the window you started with.
`previous-window' traverses the same cycle, in the reverse order.  */)
  (Lisp_Object window, Lisp_Object minibuf, Lisp_Object all_frames)
{
  return next_window (window, minibuf, all_frames, 1);
}


DEFUN ("previous-window", Fprevious_window, Sprevious_window, 0, 3, 0,
       doc: /* Return live window before WINDOW in the cyclic ordering of windows.
WINDOW must be a live window and defaults to the selected one.  The
optional arguments MINIBUF and ALL-FRAMES specify the set of windows to
consider.

MINIBUF nil or omitted means consider the minibuffer window only if the
minibuffer is active.  MINIBUF t means consider the minibuffer window
even if the minibuffer is not active.  Any other value means do not
consider the minibuffer window even if the minibuffer is active.

ALL-FRAMES nil or omitted means consider all windows on WINDOW's frame,
plus the minibuffer window if specified by the MINIBUF argument.  If the
minibuffer counts, consider all windows on all frames that share that
minibuffer too.  The following non-nil values of ALL-FRAMES have special
meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible and
  iconified frames.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on WINDOW's frame and no
others.

If you use consistent values for MINIBUF and ALL-FRAMES, you can
use `previous-window' to iterate through the entire cycle of
acceptable windows, eventually ending up back at the window you
started with.  `next-window' traverses the same cycle, in the
reverse order.  */)
  (Lisp_Object window, Lisp_Object minibuf, Lisp_Object all_frames)
{
  return next_window (window, minibuf, all_frames, 0);
}


/* Return a list of windows in cyclic ordering.  Arguments are like
   for `next-window'.  */

static Lisp_Object
window_list_1 (Lisp_Object window, Lisp_Object minibuf, Lisp_Object all_frames)
{
  Lisp_Object tail, list, rest;

  decode_next_window_args (&window, &minibuf, &all_frames);
  list = Qnil;

  for (tail = window_list (); CONSP (tail); tail = XCDR (tail))
    if (candidate_window_p (XCAR (tail), window, minibuf, all_frames))
      list = Fcons (XCAR (tail), list);

  /* Rotate the list to start with WINDOW.  */
  list = Fnreverse (list);
  rest = Fmemq (window, list);
  if (!NILP (rest) && !EQ (rest, list))
    {
      for (tail = list; !EQ (XCDR (tail), rest); tail = XCDR (tail))
	;
      XSETCDR (tail, Qnil);
      list = nconc2 (rest, list);
    }
  return list;
}


DEFUN ("window-list", Fwindow_list, Swindow_list, 0, 3, 0,
       doc: /* Return a list of windows on FRAME, starting with WINDOW.
FRAME nil or omitted means use the selected frame.
WINDOW nil or omitted means use the window selected within FRAME.
MINIBUF t means include the minibuffer window, even if it isn't active.
MINIBUF nil or omitted means include the minibuffer window only
if it's active.
MINIBUF neither nil nor t means never include the minibuffer window.  */)
  (Lisp_Object frame, Lisp_Object minibuf, Lisp_Object window)
{
  if (NILP (window))
    window = FRAMEP (frame) ? XFRAME (frame)->selected_window : selected_window;
  CHECK_WINDOW (window);
  if (NILP (frame))
    frame = selected_frame;

  if (!EQ (frame, XWINDOW (window)->frame))
    error ("Window is on a different frame");

  return window_list_1 (window, minibuf, frame);
}


DEFUN ("window-list-1", Fwindow_list_1, Swindow_list_1, 0, 3, 0,
       doc: /* Return a list of all live windows.
WINDOW specifies the first window to list and defaults to the selected
window.

Optional argument MINIBUF nil or omitted means consider the minibuffer
window only if the minibuffer is active.  MINIBUF t means consider the
minibuffer window even if the minibuffer is not active.  Any other value
means do not consider the minibuffer window even if the minibuffer is
active.

Optional argument ALL-FRAMES nil or omitted means consider all windows
on WINDOW's frame, plus the minibuffer window if specified by the
MINIBUF argument.  If the minibuffer counts, consider all windows on all
frames that share that minibuffer too.  The following non-nil values of
ALL-FRAMES have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible and
  iconified frames.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on WINDOW's frame and no
others.

If WINDOW is not on the list of windows returned, some other window will
be listed first but no error is signaled.  */)
  (Lisp_Object window, Lisp_Object minibuf, Lisp_Object all_frames)
{
  return window_list_1 (window, minibuf, all_frames);
}

/* Look at all windows, performing an operation specified by TYPE
   with argument OBJ.
   If FRAMES is Qt, look at all frames;
                Qnil, look at just the selected frame;
		Qvisible, look at visible frames;
	        a frame, just look at windows on that frame.
   If MINI is non-zero, perform the operation on minibuffer windows too.  */

enum window_loop
{
  WINDOW_LOOP_UNUSED,
  GET_BUFFER_WINDOW,		    /* Arg is buffer */
  REPLACE_BUFFER_IN_WINDOWS_SAFELY, /* Arg is buffer */
  REDISPLAY_BUFFER_WINDOWS,	    /* Arg is buffer */
  CHECK_ALL_WINDOWS
};

static Lisp_Object
window_loop (enum window_loop type, Lisp_Object obj, int mini, Lisp_Object frames)
{
  Lisp_Object window, windows, best_window, frame_arg;
  int frame_best_window_flag = 0;
  struct frame *f;
  struct gcpro gcpro1;

  /* If we're only looping through windows on a particular frame,
     frame points to that frame.  If we're looping through windows
     on all frames, frame is 0.  */
  if (FRAMEP (frames))
    f = XFRAME (frames);
  else if (NILP (frames))
    f = SELECTED_FRAME ();
  else
    f = NULL;

  if (f)
    frame_arg = Qlambda;
  else if (EQ (frames, make_number (0)))
    frame_arg = frames;
  else if (EQ (frames, Qvisible))
    frame_arg = frames;
  else
    frame_arg = Qt;

  /* frame_arg is Qlambda to stick to one frame,
     Qvisible to consider all visible frames,
     or Qt otherwise.  */

  /* Pick a window to start with.  */
  if (WINDOWP (obj))
    window = obj;
  else if (f)
    window = FRAME_SELECTED_WINDOW (f);
  else
    window = FRAME_SELECTED_WINDOW (SELECTED_FRAME ());

  windows = window_list_1 (window, mini ? Qt : Qnil, frame_arg);
  GCPRO1 (windows);
  best_window = Qnil;

  for (; CONSP (windows); windows = XCDR (windows))
    {
      struct window *w;

      window = XCAR (windows);
      w = XWINDOW (window);

      /* Note that we do not pay attention here to whether the frame
	 is visible, since Fwindow_list skips non-visible frames if
	 that is desired, under the control of frame_arg.  */
      if (!MINI_WINDOW_P (w)
	  /* For REPLACE_BUFFER_IN_WINDOWS_SAFELY, we must always
	     consider all windows.  */
	  || type == REPLACE_BUFFER_IN_WINDOWS_SAFELY
	  || (mini && minibuf_level > 0))
	switch (type)
	  {
	  case GET_BUFFER_WINDOW:
	    if (EQ (w->buffer, obj)
		/* Don't find any minibuffer window except the one that
		   is currently in use.  */
		&& (MINI_WINDOW_P (w) ? EQ (window, minibuf_window) : 1))
	      {
		if (EQ (window, selected_window))
		  /* Preferably return the selected window.  */
		  RETURN_UNGCPRO (window);
		else if (EQ (XWINDOW (window)->frame, selected_frame)
			 && !frame_best_window_flag)
		  /* Prefer windows on the current frame (but don't
		     choose another one if we have one already).  */
		  {
		    best_window = window;
		    frame_best_window_flag = 1;
		  }
		else if (NILP (best_window))
		  best_window = window;
	      }
	    break;

	  case REPLACE_BUFFER_IN_WINDOWS_SAFELY:
	    /* We could simply check whether the buffer shown by window
	       is live, and show another buffer in case it isn't.  */
	    if (EQ (w->buffer, obj))
	      {
		/* Undedicate WINDOW.  */
		w->dedicated = Qnil;
		/* Make WINDOW show the buffer returned by
		   other_buffer_safely, don't run any hooks.  */
		set_window_buffer
		  (window, other_buffer_safely (w->buffer), 0, 0);
		/* If WINDOW is the selected window, make its buffer
		   current.  But do so only if the window shows the
		   current buffer (Bug#6454).  */
		if (EQ (window, selected_window)
		    && XBUFFER (w->buffer) == current_buffer)
		  Fset_buffer (w->buffer);
	      }
	    break;

	  case REDISPLAY_BUFFER_WINDOWS:
	    if (EQ (w->buffer, obj))
	      {
		mark_window_display_accurate (window, 0);
		w->update_mode_line = Qt;
		XBUFFER (obj)->prevent_redisplay_optimizations_p = 1;
		++update_mode_lines;
		best_window = window;
	      }
	    break;

	    /* Check for a window that has a killed buffer.  */
	  case CHECK_ALL_WINDOWS:
	    if (! NILP (w->buffer)
		&& NILP (BVAR (XBUFFER (w->buffer), name)))
	      abort ();
	    break;

	  case WINDOW_LOOP_UNUSED:
	    break;
	  }
    }

  UNGCPRO;
  return best_window;
}

/* Used for debugging.  Abort if any window has a dead buffer.  */

extern void check_all_windows (void) EXTERNALLY_VISIBLE;
void
check_all_windows (void)
{
  window_loop (CHECK_ALL_WINDOWS, Qnil, 1, Qt);
}

DEFUN ("get-buffer-window", Fget_buffer_window, Sget_buffer_window, 0, 2, 0,
       doc: /* Return a window currently displaying BUFFER-OR-NAME, or nil if none.
BUFFER-OR-NAME may be a buffer or a buffer name and defaults to
the current buffer.

The optional argument ALL-FRAMES specifies the frames to consider:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible
    and iconified frames.

- A frame means consider all windows on that frame only.

Any other value of ALL-FRAMES means consider all windows on the
selected frame and no others.  */)
     (Lisp_Object buffer_or_name, Lisp_Object all_frames)
{
  Lisp_Object buffer;

  if (NILP (buffer_or_name))
    buffer = Fcurrent_buffer ();
  else
    buffer = Fget_buffer (buffer_or_name);

  if (BUFFERP (buffer))
    return window_loop (GET_BUFFER_WINDOW, buffer, 1, all_frames);
  else
    return Qnil;
}

static Lisp_Object
resize_root_window (Lisp_Object window, Lisp_Object delta, Lisp_Object horizontal, Lisp_Object ignore)
{
  return call4 (Qwindow_resize_root_window, window, delta, horizontal, ignore);
}


DEFUN ("delete-other-windows-internal", Fdelete_other_windows_internal,
       Sdelete_other_windows_internal, 0, 2, "",
       doc: /* Make WINDOW fill its frame.
Only the frame WINDOW is on is affected.  WINDOW may be any window and
defaults to the selected one.

Optional argument ROOT, if non-nil, must specify an internal window such
that WINDOW is in its window subtree.  If this is the case, replace ROOT
by WINDOW and leave alone any windows not part of ROOT's subtree.

When WINDOW is live try to reduce display jumps by keeping the text
previously visible in WINDOW in the same place on the frame.  Doing this
depends on the value of (window-start WINDOW), so if calling this
function in a program gives strange scrolling, make sure the
window-start value is reasonable when this function is called.  */)
     (Lisp_Object window, Lisp_Object root)
{
  struct window *w, *r, *s;
  struct frame *f;
  Lisp_Object sibling, pwindow, swindow IF_LINT (= Qnil), delta;
  EMACS_INT startpos IF_LINT (= 0);
  int top IF_LINT (= 0), new_top, resize_failed;
  Mouse_HLInfo *hlinfo;

  w = decode_any_window (window);
  XSETWINDOW (window, w);
  f = XFRAME (w->frame);

  if (NILP (root))
    /* ROOT is the frame's root window.  */
    {
      root = FRAME_ROOT_WINDOW (f);
      r = XWINDOW (root);
    }
  else
    /* ROOT must be an ancestor of WINDOW.  */
    {
      r = decode_any_window (root);
      pwindow = XWINDOW (window)->parent;
      while (!NILP (pwindow))
	if (EQ (pwindow, root))
	  break;
	else
	  pwindow = XWINDOW (pwindow)->parent;
      if (!EQ (pwindow, root))
	error ("Specified root is not an ancestor of specified window");
    }

  if (EQ (window, root))
    /* A noop.  */
    return Qnil;
  /* I don't understand the "top > 0" part below.  If we deal with a
     standalone minibuffer it would have been caught by the preceding
     test.  */
  else if (MINI_WINDOW_P (w)) /* && top > 0) */
    error ("Can't expand minibuffer to full frame");

  if (!NILP (w->buffer))
    {
      startpos = marker_position (w->start);
      top = WINDOW_TOP_EDGE_LINE (w)
	- FRAME_TOP_MARGIN (XFRAME (WINDOW_FRAME (w)));
      /* Make sure WINDOW is the frame's selected window.  */
      if (!EQ (window, FRAME_SELECTED_WINDOW (f)))
	{
	  if (EQ (selected_frame, w->frame))
	    Fselect_window (window, Qnil);
	  else
	    FRAME_SELECTED_WINDOW (f) = window;
	}
    }
  else
    {
      /* See if the frame's selected window is a part of the window
	 subtree rooted at WINDOW, by finding all the selected window's
	 parents and comparing each one with WINDOW.  If it isn't we
	 need a new selected window for this frame.  */
      swindow = FRAME_SELECTED_WINDOW (f);
      while (1)
	{
	  pwindow = swindow;
	  while (!NILP (pwindow) && !EQ (window, pwindow))
	    pwindow = XWINDOW (pwindow)->parent;

	  if (EQ (window, pwindow))
	    /* If WINDOW is an ancestor of SWINDOW, then SWINDOW is ok
	       as the new selected window.  */
	    break;
	  else
	    /* Else try the previous window of SWINDOW.  */
	    swindow = Fprevious_window (swindow, Qlambda, Qnil);
	}

      if (!EQ (swindow, FRAME_SELECTED_WINDOW (f)))
	{
	  if (EQ (selected_frame, w->frame))
	    Fselect_window (swindow, Qnil);
	  else
	    FRAME_SELECTED_WINDOW (f) = swindow;
	}
    }

  BLOCK_INPUT;
  hlinfo = MOUSE_HL_INFO (f);
  /* We are going to free the glyph matrices of WINDOW, and with that
     we might lose any information about glyph rows that have some of
     their glyphs highlighted in mouse face.  (These rows are marked
     with a non-zero mouse_face_p flag.)  If WINDOW indeed has some
     glyphs highlighted in mouse face, signal to frame's up-to-date
     hook that mouse highlight was overwritten, so that it will
     arrange for redisplaying the highlight.  */
  if (EQ (hlinfo->mouse_face_window, window))
    {
      hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
      hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
      hlinfo->mouse_face_window = Qnil;
    }
  free_window_matrices (r);

  windows_or_buffers_changed++;
  Vwindow_list = Qnil;
  FRAME_WINDOW_SIZES_CHANGED (f) = 1;
  resize_failed = 0;

  if (NILP (w->buffer))
    {
      /* Resize child windows vertically.  */
      XSETINT (delta, XINT (r->total_lines) - XINT (w->total_lines));
      w->top_line = r->top_line;
      resize_root_window (window, delta, Qnil, Qnil);
      if (window_resize_check (w, 0))
	window_resize_apply (w, 0);
      else
	{
	  resize_root_window (window, delta, Qnil, Qt);
	  if (window_resize_check (w, 0))
	    window_resize_apply (w, 0);
	  else
	    resize_failed = 1;
	}

      /* Resize child windows horizontally.  */
      if (!resize_failed)
	{
	  w->left_col = r->left_col;
	  XSETINT (delta, XINT (r->total_cols) - XINT (w->total_cols));
	  w->left_col = r->left_col;
	  resize_root_window (window, delta, Qt, Qnil);
	  if (window_resize_check (w, 1))
	    window_resize_apply (w, 1);
	  else
	    {
	      resize_root_window (window, delta, Qt, Qt);
	      if (window_resize_check (w, 1))
		window_resize_apply (w, 1);
	      else
		resize_failed = 1;
	    }
	}

      if (resize_failed)
	/* Play safe, if we still can ...  */
	{
	  window = swindow;
	  w = XWINDOW (window);
	}
    }

  /* Cleanly unlink WINDOW from window-tree.  */
  if (!NILP (w->prev))
    /* Get SIBLING above (on the left of) WINDOW.  */
    {
      sibling = w->prev;
      s = XWINDOW (sibling);
      s->next = w->next;
      if (!NILP (s->next))
	XWINDOW (s->next)->prev = sibling;
    }
  else
    /* Get SIBLING below (on the right of) WINDOW.  */
    {
      sibling = w->next;
      s = XWINDOW (sibling);
      s->prev = Qnil;
      if (!NILP (XWINDOW (w->parent)->vchild))
	XWINDOW (w->parent)->vchild = sibling;
      else
	XWINDOW (w->parent)->hchild = sibling;
    }

  /* Delete ROOT and all child windows of ROOT.  */
  if (!NILP (r->vchild))
    {
      delete_all_child_windows (r->vchild);
      r->vchild = Qnil;
    }
  else if (!NILP (r->hchild))
    {
      delete_all_child_windows (r->hchild);
      r->hchild = Qnil;
    }

  replace_window (root, window, 1);

  /* This must become SWINDOW anyway ....... */
  if (!NILP (w->buffer) && !resize_failed)
    {
      /* Try to minimize scrolling, by setting the window start to the
	 point will cause the text at the old window start to be at the
	 same place on the frame.  But don't try to do this if the
	 window start is outside the visible portion (as might happen
	 when the display is not current, due to typeahead).  */
      new_top = WINDOW_TOP_EDGE_LINE (w) - FRAME_TOP_MARGIN (XFRAME (WINDOW_FRAME (w)));
      if (new_top != top
	  && startpos >= BUF_BEGV (XBUFFER (w->buffer))
	  && startpos <= BUF_ZV (XBUFFER (w->buffer)))
	{
	  struct position pos;
	  struct buffer *obuf = current_buffer;

	  Fset_buffer (w->buffer);
	  /* This computation used to temporarily move point, but that
	     can have unwanted side effects due to text properties.  */
	  pos = *vmotion (startpos, -top, w);

	  set_marker_both (w->start, w->buffer, pos.bufpos, pos.bytepos);
	  w->window_end_valid = Qnil;
	  w->start_at_line_beg = ((pos.bytepos == BEGV_BYTE
				   || FETCH_BYTE (pos.bytepos - 1) == '\n') ? Qt
				  : Qnil);
	  /* We need to do this, so that the window-scroll-functions
	     get called.  */
	  w->optional_new_start = Qt;

	  set_buffer_internal (obuf);
	}
    }

  adjust_glyphs (f);
  UNBLOCK_INPUT;

  run_window_configuration_change_hook (f);

  return Qnil;
}


void
replace_buffer_in_windows (Lisp_Object buffer)
{
  call1 (Qreplace_buffer_in_windows, buffer);
}


/* Safely replace BUFFER with some other buffer in all windows of all
   frames, even those on other keyboards.  */

void
replace_buffer_in_windows_safely (Lisp_Object buffer)
{
  Lisp_Object tail, frame;

  /* A single call to window_loop won't do the job because it only
     considers frames on the current keyboard.  So loop manually over
     frames, and handle each one.  */
  FOR_EACH_FRAME (tail, frame)
    window_loop (REPLACE_BUFFER_IN_WINDOWS_SAFELY, buffer, 1, frame);
}

/* If *ROWS or *COLS are too small a size for FRAME, set them to the
   minimum allowable size.  */

void
check_frame_size (FRAME_PTR frame, int *rows, int *cols)
{
  /* For height, we have to see:
     how many windows the frame has at minimum (one or two),
     and whether it has a menu bar or other special stuff at the top.  */
  int min_height
    = ((FRAME_MINIBUF_ONLY_P (frame) || ! FRAME_HAS_MINIBUF_P (frame))
       ? MIN_SAFE_WINDOW_HEIGHT
       : 2 * MIN_SAFE_WINDOW_HEIGHT);

  if (FRAME_TOP_MARGIN (frame) > 0)
    min_height += FRAME_TOP_MARGIN (frame);

  if (*rows < min_height)
    *rows = min_height;
  if (*cols  < MIN_SAFE_WINDOW_WIDTH)
    *cols = MIN_SAFE_WINDOW_WIDTH;
}

/* Adjust the margins of window W if text area is too small.
   Return 1 if window width is ok after adjustment; 0 if window
   is still too narrow.  */

static int
adjust_window_margins (struct window *w)
{
  int box_cols = (WINDOW_TOTAL_COLS (w)
		  - WINDOW_FRINGE_COLS (w)
		  - WINDOW_SCROLL_BAR_COLS (w));
  int margin_cols = (WINDOW_LEFT_MARGIN_COLS (w)
		     + WINDOW_RIGHT_MARGIN_COLS (w));

  if (box_cols - margin_cols >= MIN_SAFE_WINDOW_WIDTH)
    return 1;

  if (margin_cols < 0 || box_cols < MIN_SAFE_WINDOW_WIDTH)
    return 0;

  /* Window's text area is too narrow, but reducing the window
     margins will fix that.  */
  margin_cols = box_cols - MIN_SAFE_WINDOW_WIDTH;
  if (WINDOW_RIGHT_MARGIN_COLS (w) > 0)
    {
      if (WINDOW_LEFT_MARGIN_COLS (w) > 0)
	w->left_margin_cols = w->right_margin_cols
	  = make_number (margin_cols/2);
      else
	w->right_margin_cols = make_number (margin_cols);
    }
  else
    w->left_margin_cols = make_number (margin_cols);
  return 1;
}

static Lisp_Object Fset_window_margins (Lisp_Object, Lisp_Object, Lisp_Object);
static Lisp_Object Fset_window_fringes (Lisp_Object, Lisp_Object, Lisp_Object,
					Lisp_Object);
static Lisp_Object Fset_window_scroll_bars (Lisp_Object, Lisp_Object,
					    Lisp_Object, Lisp_Object);
static Lisp_Object Fset_window_vscroll (Lisp_Object, Lisp_Object, Lisp_Object);

/* The following three routines are needed for running a window's
   configuration change hook.  */
static void
run_funs (Lisp_Object funs)
{
  for (; CONSP (funs); funs = XCDR (funs))
    if (!EQ (XCAR (funs), Qt))
      call0 (XCAR (funs));
}

static Lisp_Object
select_window_norecord (Lisp_Object window)
{
  return WINDOW_LIVE_P (window)
    ? Fselect_window (window, Qt) : selected_window;
}

static Lisp_Object
select_frame_norecord (Lisp_Object frame)
{
  return FRAME_LIVE_P (XFRAME (frame))
    ? Fselect_frame (frame, Qt) : selected_frame;
}

void
run_window_configuration_change_hook (struct frame *f)
{
  int count = SPECPDL_INDEX ();
  Lisp_Object frame, global_wcch
    = Fdefault_value (Qwindow_configuration_change_hook);
  XSETFRAME (frame, f);

  if (NILP (Vrun_hooks) || !NILP (inhibit_lisp_code))
    return;

  /* Use the right buffer.  Matters when running the local hooks.  */
  if (current_buffer != XBUFFER (Fwindow_buffer (Qnil)))
    {
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      Fset_buffer (Fwindow_buffer (Qnil));
    }

  if (SELECTED_FRAME () != f)
    {
      record_unwind_protect (select_frame_norecord, Fselected_frame ());
      select_frame_norecord (frame);
    }

  /* Look for buffer-local values.  */
  {
    Lisp_Object windows = Fwindow_list (frame, Qlambda, Qnil);
    for (; CONSP (windows); windows = XCDR (windows))
      {
	Lisp_Object window = XCAR (windows);
	Lisp_Object buffer = Fwindow_buffer (window);
	if (!NILP (Flocal_variable_p (Qwindow_configuration_change_hook,
				      buffer)))
	  {
	    int inner_count = SPECPDL_INDEX ();
	    record_unwind_protect (select_window_norecord, Fselected_window ());
	    select_window_norecord (window);
	    run_funs (Fbuffer_local_value (Qwindow_configuration_change_hook,
					   buffer));
	    unbind_to (inner_count, Qnil);
	  }
      }
  }

  run_funs (global_wcch);
  unbind_to (count, Qnil);
}

DEFUN ("run-window-configuration-change-hook", Frun_window_configuration_change_hook,
       Srun_window_configuration_change_hook, 1, 1, 0,
       doc: /* Run `window-configuration-change-hook' for FRAME.  */)
     (Lisp_Object frame)
{
  CHECK_LIVE_FRAME (frame);
  run_window_configuration_change_hook (XFRAME (frame));
  return Qnil;
}

/* Make WINDOW display BUFFER as its contents.  RUN_HOOKS_P non-zero
   means it's allowed to run hooks.  See make_frame for a case where
   it's not allowed.  KEEP_MARGINS_P non-zero means that the current
   margins, fringes, and scroll-bar settings of the window are not
   reset from the buffer's local settings.  */

void
set_window_buffer (Lisp_Object window, Lisp_Object buffer, int run_hooks_p, int keep_margins_p)
{
  struct window *w = XWINDOW (window);
  struct buffer *b = XBUFFER (buffer);
  int count = SPECPDL_INDEX ();
  int samebuf = EQ (buffer, w->buffer);

  w->buffer = buffer;

  if (EQ (window, selected_window))
    BVAR (b, last_selected_window) = window;

  /* Let redisplay errors through.  */
  b->display_error_modiff = 0;

  /* Update time stamps of buffer display.  */
  if (INTEGERP (BVAR (b, display_count)))
    XSETINT (BVAR (b, display_count), XINT (BVAR (b, display_count)) + 1);
  BVAR (b, display_time) = Fcurrent_time ();

  XSETFASTINT (w->window_end_pos, 0);
  XSETFASTINT (w->window_end_vpos, 0);
  memset (&w->last_cursor, 0, sizeof w->last_cursor);
  w->window_end_valid = Qnil;
  if (!(keep_margins_p && samebuf))
    { /* If we're not actually changing the buffer, don't reset hscroll and
	 vscroll.  This case happens for example when called from
	 change_frame_size_1, where we use a dummy call to
	 Fset_window_buffer on the frame's selected window (and no other)
	 just in order to run window-configuration-change-hook.
	 Resetting hscroll and vscroll here is problematic for things like
	 image-mode and doc-view-mode since it resets the image's position
	 whenever we resize the frame.  */
      w->hscroll = w->min_hscroll = make_number (0);
      w->vscroll = 0;
      set_marker_both (w->pointm, buffer, BUF_PT (b), BUF_PT_BYTE (b));
      set_marker_restricted (w->start,
			     make_number (b->last_window_start),
			     buffer);
      w->start_at_line_beg = Qnil;
      w->force_start = Qnil;
      XSETFASTINT (w->last_modified, 0);
      XSETFASTINT (w->last_overlay_modified, 0);
    }
  /* Maybe we could move this into the `if' but it's not obviously safe and
     I doubt it's worth the trouble.  */
  windows_or_buffers_changed++;

  /* We must select BUFFER for running the window-scroll-functions.  */
  /* We can't check ! NILP (Vwindow_scroll_functions) here
     because that might itself be a local variable.  */
  if (window_initialized)
    {
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      Fset_buffer (buffer);
    }

  XMARKER (w->pointm)->insertion_type = !NILP (Vwindow_point_insertion_type);

  if (!keep_margins_p)
    {
      /* Set left and right marginal area width etc. from buffer.  */

      /* This may call adjust_window_margins three times, so
	 temporarily disable window margins.  */
      Lisp_Object save_left = w->left_margin_cols;
      Lisp_Object save_right = w->right_margin_cols;

      w->left_margin_cols = w->right_margin_cols = Qnil;

      Fset_window_fringes (window,
			   BVAR (b, left_fringe_width), BVAR (b, right_fringe_width),
			   BVAR (b, fringes_outside_margins));

      Fset_window_scroll_bars (window,
			       BVAR (b, scroll_bar_width),
			       BVAR (b, vertical_scroll_bar_type), Qnil);

      w->left_margin_cols = save_left;
      w->right_margin_cols = save_right;

      Fset_window_margins (window,
			   BVAR (b, left_margin_cols), BVAR (b, right_margin_cols));
    }

  if (run_hooks_p)
    {
      if (! NILP (Vwindow_scroll_functions))
	run_hook_with_args_2 (Qwindow_scroll_functions, window,
			      Fmarker_position (w->start));
      run_window_configuration_change_hook (XFRAME (WINDOW_FRAME (w)));
    }

  unbind_to (count, Qnil);
}

DEFUN ("set-window-buffer", Fset_window_buffer, Sset_window_buffer, 2, 3, 0,
       doc: /* Make WINDOW display BUFFER-OR-NAME as its contents.
WINDOW has to be a live window and defaults to the selected one.
BUFFER-OR-NAME must be a buffer or the name of an existing buffer.

Optional third argument KEEP-MARGINS non-nil means that WINDOW's current
display margins, fringe widths, and scroll bar settings are preserved;
the default is to reset these from the local settings for BUFFER-OR-NAME
or the frame defaults.  Return nil.

This function throws an error when WINDOW is strongly dedicated to its
buffer (that is `window-dedicated-p' returns t for WINDOW) and does not
already display BUFFER-OR-NAME.

This function runs `window-scroll-functions' before running
`window-configuration-change-hook'.  */)
  (register Lisp_Object window, Lisp_Object buffer_or_name, Lisp_Object keep_margins)
{
  register Lisp_Object tem, buffer;
  register struct window *w = decode_window (window);

  XSETWINDOW (window, w);
  buffer = Fget_buffer (buffer_or_name);
  CHECK_BUFFER (buffer);
  if (NILP (BVAR (XBUFFER (buffer), name)))
    error ("Attempt to display deleted buffer");

  tem = w->buffer;
  if (NILP (tem))
    error ("Window is deleted");
  else if (!EQ (tem, Qt))
    /* w->buffer is t when the window is first being set up.  */
    {
      if (!EQ (tem, buffer))
	{
	  if (EQ (w->dedicated, Qt))
	    /* WINDOW is strongly dedicated to its buffer, signal an
	       error.  */
	    error ("Window is dedicated to `%s'", SDATA (BVAR (XBUFFER (tem), name)));
	  else
	    /* WINDOW is weakly dedicated to its buffer, reset
	       dedication.  */
	    w->dedicated = Qnil;

	  call1 (Qrecord_window_buffer, window);
	}

      unshow_buffer (w);
    }

  set_window_buffer (window, buffer, 1, !NILP (keep_margins));

  return Qnil;
}

static Lisp_Object
display_buffer (Lisp_Object buffer, Lisp_Object not_this_window_p, Lisp_Object override_frame)
{
  return call3 (Qdisplay_buffer, buffer, not_this_window_p, override_frame);
}

DEFUN ("force-window-update", Fforce_window_update, Sforce_window_update,
       0, 1, 0,
       doc: /* Force all windows to be updated on next redisplay.
If optional arg OBJECT is a window, force redisplay of that window only.
If OBJECT is a buffer or buffer name, force redisplay of all windows
displaying that buffer.  */)
  (Lisp_Object object)
{
  if (NILP (object))
    {
      windows_or_buffers_changed++;
      update_mode_lines++;
      return Qt;
    }

  if (WINDOWP (object))
    {
      struct window *w = XWINDOW (object);
      mark_window_display_accurate (object, 0);
      w->update_mode_line = Qt;
      if (BUFFERP (w->buffer))
	XBUFFER (w->buffer)->prevent_redisplay_optimizations_p = 1;
      ++update_mode_lines;
      return Qt;
    }

  if (STRINGP (object))
    object = Fget_buffer (object);
  if (BUFFERP (object) && !NILP (BVAR (XBUFFER (object), name)))
    {
      /* Walk all windows looking for buffer, and force update
	 of each of those windows.  */

      object = window_loop (REDISPLAY_BUFFER_WINDOWS, object, 0, Qvisible);
      return NILP (object) ? Qnil : Qt;
    }

  /* If nothing suitable was found, just return.
     We could signal an error, but this feature will typically be used
     asynchronously in timers or process sentinels, so we don't.  */
  return Qnil;
}


void
temp_output_buffer_show (register Lisp_Object buf)
{
  register struct buffer *old = current_buffer;
  register Lisp_Object window;
  register struct window *w;

  BVAR (XBUFFER (buf), directory) = BVAR (current_buffer, directory);

  Fset_buffer (buf);
  BUF_SAVE_MODIFF (XBUFFER (buf)) = MODIFF;
  BEGV = BEG;
  ZV = Z;
  SET_PT (BEG);
  set_buffer_internal (old);

  if (!NILP (Vtemp_buffer_show_function))
    call1 (Vtemp_buffer_show_function, buf);
  else
    {
      window = display_buffer (buf, Qnil, Qnil);

      if (!EQ (XWINDOW (window)->frame, selected_frame))
	Fmake_frame_visible (WINDOW_FRAME (XWINDOW (window)));
      Vminibuf_scroll_window = window;
      w = XWINDOW (window);
      XSETFASTINT (w->hscroll, 0);
      XSETFASTINT (w->min_hscroll, 0);
      set_marker_restricted_both (w->start, buf, BEG, BEG);
      set_marker_restricted_both (w->pointm, buf, BEG, BEG);

      /* Run temp-buffer-show-hook, with the chosen window selected
	 and its buffer current.  */
      {
        int count = SPECPDL_INDEX ();
        Lisp_Object prev_window, prev_buffer;
        prev_window = selected_window;
        XSETBUFFER (prev_buffer, old);

        /* Select the window that was chosen, for running the hook.
           Note: Both Fselect_window and select_window_norecord may
           set-buffer to the buffer displayed in the window,
           so we need to save the current buffer.  --stef  */
        record_unwind_protect (Fset_buffer, prev_buffer);
        record_unwind_protect (select_window_norecord, prev_window);
        Fselect_window (window, Qt);
        Fset_buffer (w->buffer);
        Frun_hooks (1, &Qtemp_buffer_show_hook);
        unbind_to (count, Qnil);
      }
    }
}

DEFUN ("internal-temp-output-buffer-show",
       Ftemp_output_buffer_show, Stemp_output_buffer_show,
       1, 1, 0,
       doc: /* Internal function for `with-output-to-temp-buffer'.  */)
     (Lisp_Object buf)
{
  temp_output_buffer_show (buf);
  return Qnil;
}

/* Make new window, have it replace WINDOW in window-tree, and make
   WINDOW its only vertical child (HORFLAG 1 means make WINDOW its only
   horizontal child).   */
static void
make_parent_window (Lisp_Object window, int horflag)
{
  Lisp_Object parent;
  register struct window *o, *p;
  int i;

  o = XWINDOW (window);
  p = allocate_window ();
  for (i = 0; i < VECSIZE (struct window); ++i)
    ((struct Lisp_Vector *) p)->contents[i]
      = ((struct Lisp_Vector *) o)->contents[i];
  XSETWINDOW (parent, p);

  ++sequence_number;
  XSETFASTINT (p->sequence_number, sequence_number);

  replace_window (window, parent, 1);

  o->next = Qnil;
  o->prev = Qnil;
  o->parent = parent;

  p->hchild = horflag ? window : Qnil;
  p->vchild = horflag ? Qnil : window;
  p->start = Qnil;
  p->pointm = Qnil;
  p->buffer = Qnil;
  p->combination_limit = Qnil;
  p->window_parameters = Qnil;
}

/* Make new window from scratch.  */
Lisp_Object
make_window (void)
{
  Lisp_Object window;
  register struct window *w;

  w = allocate_window ();
  /* Initialize all Lisp data.  */
  w->frame = w->mini_p = Qnil;
  w->next = w->prev = w->hchild = w->vchild = w->parent = Qnil;
  XSETFASTINT (w->left_col, 0);
  XSETFASTINT (w->top_line, 0);
  XSETFASTINT (w->total_lines, 0);
  XSETFASTINT (w->total_cols, 0);
  w->normal_lines = make_float (1.0);
  w->normal_cols = make_float (1.0);
  XSETFASTINT (w->new_total, 0);
  XSETFASTINT (w->new_normal, 0);
  w->buffer = Qnil;
  w->start = Fmake_marker ();
  w->pointm = Fmake_marker ();
  w->force_start = w->optional_new_start = Qnil;
  XSETFASTINT (w->hscroll, 0);
  XSETFASTINT (w->min_hscroll, 0);
  XSETFASTINT (w->use_time, 0);
  ++sequence_number;
  XSETFASTINT (w->sequence_number, sequence_number);
  w->temslot = w->last_modified = w->last_overlay_modified = Qnil;
  XSETFASTINT (w->last_point, 0);
  w->last_had_star = w->vertical_scroll_bar = Qnil;
  w->left_margin_cols = w->right_margin_cols = Qnil;
  w->left_fringe_width = w->right_fringe_width = Qnil;
  w->fringes_outside_margins = Qnil;
  w->scroll_bar_width = Qnil;
  w->vertical_scroll_bar_type = Qt;
  w->last_mark_x = w->last_mark_y = Qnil;
  XSETFASTINT (w->window_end_pos, 0);
  XSETFASTINT (w->window_end_vpos, 0);
  w->window_end_valid = w->update_mode_line = Qnil;
  w->start_at_line_beg = w->display_table = w->dedicated = Qnil;
  w->base_line_number = w->base_line_pos = w->region_showing = Qnil;
  w->column_number_displayed = w->redisplay_end_trigger = Qnil;
  w->combination_limit = w->window_parameters = Qnil;
  w->prev_buffers = w->next_buffers = Qnil;
  /* Initialize non-Lisp data.  */
  w->desired_matrix = w->current_matrix = 0;
  w->nrows_scale_factor = w->ncols_scale_factor = 1;
  memset (&w->cursor, 0, sizeof (w->cursor));
  memset (&w->last_cursor, 0, sizeof (w->last_cursor));
  memset (&w->phys_cursor, 0, sizeof (w->phys_cursor));
  w->phys_cursor_type = -1;
  w->phys_cursor_width = -1;
  w->phys_cursor_on_p = 0;
  w->last_cursor_off_p = w->cursor_off_p = 0;
  w->must_be_updated_p = 0;
  w->pseudo_window_p = 0;
  w->frozen_window_start_p = 0;
  w->vscroll = 0;
  /* Reset window_list.  */
  Vwindow_list = Qnil;
  /* Return window.  */
  XSETWINDOW (window, w);
  return window;
}

DEFUN ("set-window-new-total", Fset_window_new_total, Sset_window_new_total, 2, 3, 0,
       doc: /* Set new total size of WINDOW to SIZE.
Return SIZE.

Optional argument ADD non-nil means add SIZE to the new total size of
WINDOW and return the sum.

Note: This function does not operate on any child windows of WINDOW.  */)
     (Lisp_Object window, Lisp_Object size, Lisp_Object add)
{
  struct window *w = decode_any_window (window);

  CHECK_NUMBER (size);
  if (NILP (add))
    XSETINT (w->new_total, XINT (size));
  else
    XSETINT (w->new_total, XINT (w->new_total) + XINT (size));

  return w->new_total;
}

DEFUN ("set-window-new-normal", Fset_window_new_normal, Sset_window_new_normal, 1, 2, 0,
       doc: /* Set new normal size of WINDOW to SIZE.
Return SIZE.

Note: This function does not operate on any child windows of WINDOW.  */)
     (Lisp_Object window, Lisp_Object size)
{
  struct window *w = decode_any_window (window);

  w->new_normal = size;
  return w->new_normal;
}

/* Return 1 if setting w->total_lines (w->total_cols if HORFLAG is
   non-zero) to w->new_total would result in correct heights (widths)
   for window W and recursively all child windows of W.

   Note: This function does not check any of `window-fixed-size-p',
   `window-min-height' or `window-min-width'.  It does check that window
   sizes do not drop below one line (two columns). */
static int
window_resize_check (struct window *w, int horflag)
{
  struct window *c;

  if (!NILP (w->vchild))
    /* W is a vertical combination.  */
    {
      c = XWINDOW (w->vchild);
      if (horflag)
	/* All child windows of W must have the same width as W.  */
	{
	  while (c)
	    {
	      if ((XINT (c->new_total) != XINT (w->new_total))
		  || !window_resize_check (c, horflag))
		return 0;
	      c = NILP (c->next) ? 0 : XWINDOW (c->next);
	    }
	  return 1;
	}
      else
	/* The sum of the heights of the child windows of W must equal
	   W's height.  */
	{
	  int sum_of_sizes = 0;
	  while (c)
	    {
	      if (!window_resize_check (c, horflag))
		return 0;
	      sum_of_sizes = sum_of_sizes + XINT (c->new_total);
	      c = NILP (c->next) ? 0 : XWINDOW (c->next);
	    }
	  return (sum_of_sizes == XINT (w->new_total));
	}
    }
  else if (!NILP (w->hchild))
    /* W is a horizontal combination.  */
    {
      c = XWINDOW (w->hchild);
      if (horflag)
	/* The sum of the widths of the child windows of W must equal W's
	   width.  */
	{
	  int sum_of_sizes = 0;
	  while (c)
	    {
	      if (!window_resize_check (c, horflag))
		return 0;
	      sum_of_sizes = sum_of_sizes + XINT (c->new_total);
	      c = NILP (c->next) ? 0 : XWINDOW (c->next);
	    }
	  return (sum_of_sizes == XINT (w->new_total));
	}
      else
	/* All child windows of W must have the same height as W.  */
	{
	  while (c)
	    {
	      if ((XINT (c->new_total) != XINT (w->new_total))
		  || !window_resize_check (c, horflag))
		return 0;
	      c = NILP (c->next) ? 0 : XWINDOW (c->next);
	    }
	  return 1;
	}
    }
  else
    /* A leaf window.  Make sure it's not too small.  The following
       hardcodes the values of `window-safe-min-width' (2) and
       `window-safe-min-height' (1) which are defined in window.el.  */
    return XINT (w->new_total) >= (horflag ? 2 : 1);
}

/* Set w->total_lines (w->total_cols if HORIZONTAL is non-zero) to
   w->new_total for window W and recursively all child windows of W.
   Also calculate and assign the new vertical (horizontal) start
   positions of each of these windows.

   This function does not perform any error checks.  Make sure you have
   run window_resize_check on W before applying this function.  */
static void
window_resize_apply (struct window *w, int horflag)
{
  struct window *c;
  int pos;

  /* Note: Assigning new_normal requires that the new total size of the
     parent window has been set *before*.  */
  if (horflag)
    {
      w->total_cols = w->new_total;
      if (NUMBERP (w->new_normal))
	w->normal_cols = w->new_normal;

      pos = XINT (w->left_col);
    }
  else
    {
      w->total_lines = w->new_total;
      if (NUMBERP (w->new_normal))
	w->normal_lines = w->new_normal;

      pos = XINT (w->top_line);
    }

  if (!NILP (w->vchild))
    /* W is a vertical combination.  */
    {
      c = XWINDOW (w->vchild);
      while (c)
	{
	  if (horflag)
	    XSETFASTINT (c->left_col, pos);
	  else
	    XSETFASTINT (c->top_line, pos);
	  window_resize_apply (c, horflag);
	  if (!horflag)
	    pos = pos + XINT (c->total_lines);
	  c = NILP (c->next) ? 0 : XWINDOW (c->next);
	}
    }
  else if (!NILP (w->hchild))
    /* W is a horizontal combination.  */
    {
      c = XWINDOW (w->hchild);
      while (c)
	{
	  if (horflag)
	    XSETFASTINT (c->left_col, pos);
	  else
	    XSETFASTINT (c->top_line, pos);
	  window_resize_apply (c, horflag);
	  if (horflag)
	    pos = pos + XINT (c->total_cols);
	  c = NILP (c->next) ? 0 : XWINDOW (c->next);
	}
    }

  /* Clear out some redisplay caches.  */
  XSETFASTINT (w->last_modified, 0);
  XSETFASTINT (w->last_overlay_modified, 0);
}


DEFUN ("window-resize-apply", Fwindow_resize_apply, Swindow_resize_apply, 1, 2, 0,
       doc: /* Apply requested size values for window-tree of FRAME.
Optional argument HORIZONTAL omitted or nil means apply requested height
values.  HORIZONTAL non-nil means apply requested width values.

This function checks whether the requested values sum up to a valid
window layout, recursively assigns the new sizes of all child windows
and calculates and assigns the new start positions of these windows.

Note: This function does not check any of `window-fixed-size-p',
`window-min-height' or `window-min-width'.  All these checks have to
be applied on the Elisp level.  */)
     (Lisp_Object frame, Lisp_Object horizontal)
{
  struct frame *f;
  struct window *r;
  int horflag = !NILP (horizontal);

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);

  f = XFRAME (frame);
  r = XWINDOW (FRAME_ROOT_WINDOW (f));

  if (!window_resize_check (r, horflag)
      || ! EQ (r->new_total, (horflag ? r->total_cols : r->total_lines)))
    return Qnil;

  BLOCK_INPUT;
  window_resize_apply (r, horflag);

  windows_or_buffers_changed++;
  FRAME_WINDOW_SIZES_CHANGED (f) = 1;

  adjust_glyphs (f);
  UNBLOCK_INPUT;

  run_window_configuration_change_hook (f);

  return Qt;
}


/* Resize frame F's windows when number of lines of F is set to SIZE.
   HORFLAG 1 means resize windows when number of columns of F is set to
   SIZE.

   This function can delete all windows but the selected one in order to
   satisfy the request.  The result will be meaningful if and only if
   F's windows have meaningful sizes when you call this.  */
void
resize_frame_windows (struct frame *f, int size, int horflag)
{
  Lisp_Object root = f->root_window;
  struct window *r = XWINDOW (root);
  Lisp_Object mini = f->minibuffer_window;
  struct window *m;
  /* new_size is the new size of the frame's root window.  */
  int new_size = (horflag
		  ? size
		  : (size
		     - FRAME_TOP_MARGIN (f)
		     - ((FRAME_HAS_MINIBUF_P (f) && !FRAME_MINIBUF_ONLY_P (f))
			? 1 : 0)));

  XSETFASTINT (r->top_line, FRAME_TOP_MARGIN (f));
  if (NILP (r->vchild) && NILP (r->hchild))
    /* For a leaf root window just set the size.  */
    if (horflag)
      XSETFASTINT (r->total_cols, new_size);
    else
      XSETFASTINT (r->total_lines, new_size);
  else
    {
      /* old_size is the old size of the frame's root window.  */
      int old_size = XFASTINT (horflag ? r->total_cols : r->total_lines);
      Lisp_Object delta;

      XSETINT (delta, new_size - old_size);
      /* Try a "normal" resize first.  */
      resize_root_window (root, delta, horflag ? Qt : Qnil, Qnil);
      if (window_resize_check (r, horflag) && new_size == XINT (r->new_total))
	window_resize_apply (r, horflag);
      else
	{
	  /* Try with "reasonable" minimum sizes next.  */
	  resize_root_window (root, delta, horflag ? Qt : Qnil, Qt);
	  if (window_resize_check (r, horflag)
	      && new_size == XINT (r->new_total))
	    window_resize_apply (r, horflag);
	  else
	    {
	      /* Finally, try with "safe" minimum sizes.  */
	      resize_root_window (root, delta, horflag ? Qt : Qnil, Qsafe);
	      if (window_resize_check (r, horflag)
		  && new_size == XINT (r->new_total))
		window_resize_apply (r, horflag);
	      else
		{
		  /* We lost.  Delete all windows but the frame's
		     selected one.  */
		  root = f->selected_window;
		  Fdelete_other_windows_internal (root, Qnil);
		  if (horflag)
		    XSETFASTINT (XWINDOW (root)->total_cols, new_size);
		  else
		    XSETFASTINT (XWINDOW (root)->total_lines, new_size);
		}
	    }
	}
    }

  if (FRAME_HAS_MINIBUF_P (f) && !FRAME_MINIBUF_ONLY_P (f))
    {
      m = XWINDOW (mini);
      if (horflag)
	XSETFASTINT (m->total_cols, size);
      else
	{
	  /* Are we sure we always want 1 line here?  */
	  XSETFASTINT (m->total_lines, 1);
	  XSETFASTINT (m->top_line, XINT (r->top_line) + XINT (r->total_lines));
	}
    }
}


DEFUN ("split-window-internal", Fsplit_window_internal, Ssplit_window_internal, 4, 4, 0,
       doc: /* Split window OLD.
Second argument TOTAL-SIZE specifies the number of lines or columns of the
new window.  In any case TOTAL-SIZE must be a positive integer.

Third argument SIDE nil (or `below') specifies that the new window shall
be located below WINDOW.  SIDE `above' means the new window shall be
located above WINDOW.  In both cases TOTAL-SIZE specifies the number of
lines of the new window including space reserved for the mode and/or
header line.

SIDE t (or `right') specifies that the new window shall be located on
the right side of WINDOW.  SIDE `left' means the new window shall be
located on the left of WINDOW.  In both cases TOTAL-SIZE specifies the
number of columns of the new window including space reserved for fringes
and the scrollbar or a divider column.

Fourth argument NORMAL-SIZE specifies the normal size of the new window
according to the SIDE argument.

The new total and normal sizes of all involved windows must have been
set correctly.  See the code of `split-window' for how this is done.  */)
  (Lisp_Object old, Lisp_Object total_size, Lisp_Object side, Lisp_Object normal_size)
{
  /* OLD (*o) is the window we have to split.  (*p) is either OLD's
     parent window or an internal window we have to install as OLD's new
     parent.  REFERENCE (*r) must denote a live window, or is set to OLD
     provided OLD is a leaf window, or to the frame's selected window.
     NEW (*n) is the new window created with some parameters taken from
     REFERENCE (*r).  */
  register Lisp_Object new, frame, reference;
  register struct window *o, *p, *n, *r;
  struct frame *f;
  int horflag
    /* HORFLAG is 1 when we split side-by-side, 0 otherwise.  */
    = EQ (side, Qt) || EQ (side, Qleft) || EQ (side, Qright);
  int combination_limit = 0;

  CHECK_WINDOW (old);
  o = XWINDOW (old);
  frame = WINDOW_FRAME (o);
  f = XFRAME (frame);

  CHECK_NUMBER (total_size);

  /* Set combination_limit to 1 if we have to make a new parent window.
     We do that if either `window-combination-limit' is t, or OLD has no
     parent, or OLD is ortho-combined.  */
  combination_limit =
    !NILP (Vwindow_combination_limit)
    || NILP (o->parent)
    || NILP (horflag
	     ? (XWINDOW (o->parent)->hchild)
	     : (XWINDOW (o->parent)->vchild));

  /* We need a live reference window to initialize some parameters.  */
  if (WINDOW_LIVE_P (old))
    /* OLD is live, use it as reference window.  */
    reference = old;
  else
    /* Use the frame's selected window as reference window.  */
    reference = FRAME_SELECTED_WINDOW (f);
  r = XWINDOW (reference);

  /* The following bugs are caught by `split-window'.  */
  if (MINI_WINDOW_P (o))
    error ("Attempt to split minibuffer window");
  else if (XINT (total_size) < (horflag ? 2 : 1))
    error ("Size of new window too small (after split)");
  else if (!combination_limit && !NILP (Vwindow_combination_resize))
    /* `window-combination-resize' non-nil means try to resize OLD's siblings
       proportionally.  */
    {
      p = XWINDOW (o->parent);
      /* Temporarily pretend we split the parent window.  */
      XSETINT (p->new_total,
	       XINT (horflag ? p->total_cols : p->total_lines)
	       - XINT (total_size));
      if (!window_resize_check (p, horflag))
	error ("Window sizes don't fit");
      else
	/* Undo the temporary pretension.  */
	p->new_total = horflag ? p->total_cols : p->total_lines;
    }
  else
    {
      if (!window_resize_check (o, horflag))
	error ("Resizing old window failed");
      else if (XINT (total_size) + XINT (o->new_total)
	       != XINT (horflag ? o->total_cols : o->total_lines))
	error ("Sum of sizes of old and new window don't fit");
    }

  /* This is our point of no return. */
  if (combination_limit)
    {
      /* Save the old value of o->normal_cols/lines.  It gets corrupted
	 by make_parent_window and we need it below for assigning it to
	 p->new_normal.  */
      Lisp_Object new_normal = horflag ? o->normal_cols : o->normal_lines;

      make_parent_window (old, horflag);
      p = XWINDOW (o->parent);
      /* Store value of `window-combination-limit' in new parent's
	 combination_limit slot.  */
      p->combination_limit = Vwindow_combination_limit;
      /* These get applied below.  */
      p->new_total = horflag ? o->total_cols : o->total_lines;
      p->new_normal = new_normal;
    }
  else
    p = XWINDOW (o->parent);

  windows_or_buffers_changed++;
  FRAME_WINDOW_SIZES_CHANGED (f) = 1;
  new = make_window ();
  n = XWINDOW (new);
  n->frame = frame;
  n->parent = o->parent;
  n->vchild = n->hchild = Qnil;

  if (EQ (side, Qabove) || EQ (side, Qleft))
    {
      n->prev = o->prev;
      if (NILP (n->prev))
	if (horflag)
	  p->hchild = new;
	else
	  p->vchild = new;
      else
	XWINDOW (n->prev)->next = new;
      n->next = old;
      o->prev = new;
    }
  else
    {
      n->next = o->next;
      if (!NILP (n->next))
	XWINDOW (n->next)->prev = new;
      n->prev = old;
      o->next = new;
    }

  n->buffer = Qt;
  n->window_end_valid = Qnil;
  memset (&n->last_cursor, 0, sizeof n->last_cursor);

  /* Get special geometry settings from reference window.  */
  n->left_margin_cols = r->left_margin_cols;
  n->right_margin_cols = r->right_margin_cols;
  n->left_fringe_width = r->left_fringe_width;
  n->right_fringe_width = r->right_fringe_width;
  n->fringes_outside_margins = r->fringes_outside_margins;
  n->scroll_bar_width = r->scroll_bar_width;
  n->vertical_scroll_bar_type = r->vertical_scroll_bar_type;

  /* Directly assign orthogonal coordinates and sizes.  */
  if (horflag)
    {
      n->top_line = o->top_line;
      n->total_lines = o->total_lines;
    }
  else
    {
      n->left_col = o->left_col;
      n->total_cols = o->total_cols;
    }

  /* Iso-coordinates and sizes are assigned by window_resize_apply,
     get them ready here.  */
  n->new_total = total_size;
  n->new_normal = normal_size;

  BLOCK_INPUT;
  window_resize_apply (p, horflag);
  adjust_glyphs (f);
  /* Set buffer of NEW to buffer of reference window.  Don't run
     any hooks.  */
  set_window_buffer (new, r->buffer, 0, 1);
  UNBLOCK_INPUT;

  /* Maybe we should run the scroll functions in Elisp (which already
     runs the configuration change hook).  */
  if (! NILP (Vwindow_scroll_functions))
    run_hook_with_args_2 (Qwindow_scroll_functions, new,
			  Fmarker_position (n->start));
  /* Return NEW.  */
  return new;
}


DEFUN ("delete-window-internal", Fdelete_window_internal, Sdelete_window_internal, 1, 1, 0,
       doc: /* Remove WINDOW from its frame.
WINDOW defaults to the selected window.  Return nil.
Signal an error when WINDOW is the only window on its frame.  */)
     (register Lisp_Object window)
{
  register Lisp_Object parent, sibling, frame, root;
  struct window *w, *p, *s, *r;
  struct frame *f;
  int horflag;
  int before_sibling = 0;

  w = decode_any_window (window);
  XSETWINDOW (window, w);
  if (NILP (w->buffer) && NILP (w->hchild) && NILP (w->vchild))
    /* It's a no-op to delete an already deleted window.  */
    return Qnil;

  parent = w->parent;
  if (NILP (parent))
    /* Never delete a minibuffer or frame root window.  */
    error ("Attempt to delete minibuffer or sole ordinary window");
  else if (NILP (w->prev) && NILP (w->next))
    /* Rather bow out here, this case should be handled on the Elisp
       level.  */
    error ("Attempt to delete sole window of parent");

  p = XWINDOW (parent);
  horflag = NILP (p->vchild);

  frame = WINDOW_FRAME (w);
  f = XFRAME (frame);

  root = FRAME_ROOT_WINDOW (f);
  r = XWINDOW (root);

  /* Unlink WINDOW from window tree.  */
  if (NILP (w->prev))
    /* Get SIBLING below (on the right of) WINDOW.  */
    {
      /* before_sibling 1 means WINDOW is the first child of its
	 parent and thus before the sibling.  */
      before_sibling = 1;
      sibling = w->next;
      s = XWINDOW (sibling);
      s->prev = Qnil;
      if (horflag)
	p->hchild = sibling;
      else
	p->vchild = sibling;
    }
  else
    /* Get SIBLING above (on the left of) WINDOW.  */
    {
      sibling = w->prev;
      s = XWINDOW (sibling);
      s->next = w->next;
      if (!NILP (s->next))
	XWINDOW (s->next)->prev = sibling;
    }

  if (window_resize_check (r, horflag)
      && EQ (r->new_total, (horflag ? r->total_cols : r->total_lines)))
    /* We can delete WINDOW now.  */
    {
      Mouse_HLInfo *hlinfo;

      /* Block input.  */
      BLOCK_INPUT;
      window_resize_apply (p, horflag);

      /* If this window is referred to by the dpyinfo's mouse
	 highlight, invalidate that slot to be safe (Bug#9904).  */
      hlinfo = MOUSE_HL_INFO (XFRAME (w->frame));
      if (EQ (hlinfo->mouse_face_window, window))
	hlinfo->mouse_face_window = Qnil;

      windows_or_buffers_changed++;
      Vwindow_list = Qnil;
      FRAME_WINDOW_SIZES_CHANGED (f) = 1;

      w->next = Qnil;  /* Don't delete w->next too.  */
      free_window_matrices (w);

      if (!NILP (w->vchild))
	{
	  delete_all_child_windows (w->vchild);
	  w->vchild = Qnil;
	}
      else if (!NILP (w->hchild))
	{
	  delete_all_child_windows (w->hchild);
	  w->hchild = Qnil;
	}
      else if (!NILP (w->buffer))
	{
	  unshow_buffer (w);
	  unchain_marker (XMARKER (w->pointm));
	  unchain_marker (XMARKER (w->start));
	  w->buffer = Qnil;
	}

      if (NILP (s->prev) && NILP (s->next))
	  /* A matrjoshka where SIBLING has become the only child of
	     PARENT.  */
	{
	  /* Put SIBLING into PARENT's place.  */
	  replace_window (parent, sibling, 0);
	  /* Have SIBLING inherit the following three slot values from
	     PARENT (the combination_limit slot is not inherited).  */
	  s->normal_cols = p->normal_cols;
	  s->normal_lines = p->normal_lines;
	  /* Mark PARENT as deleted.  */
	  p->vchild = p->hchild = Qnil;
	  /* Try to merge SIBLING into its new parent.  */
	  recombine_windows (sibling);
	}

      adjust_glyphs (f);

      if (!WINDOW_LIVE_P (FRAME_SELECTED_WINDOW (f)))
	/* We deleted the frame's selected window.  */
	{
	  /* Use the frame's first window as fallback ...  */
	  Lisp_Object new_selected_window = Fframe_first_window (frame);
	  /* ... but preferably use its most recently used window.  */
	  Lisp_Object mru_window;

	  /* `get-mru-window' might fail for some reason so play it safe
	  - promote the first window _without recording it_ first.  */
	  if (EQ (FRAME_SELECTED_WINDOW (f), selected_window))
	    Fselect_window (new_selected_window, Qt);
	  else
	    FRAME_SELECTED_WINDOW (f) = new_selected_window;

	  UNBLOCK_INPUT;

	  /* Now look whether `get-mru-window' gets us something.  */
	  mru_window = call1 (Qget_mru_window, frame);
	  if (WINDOW_LIVE_P (mru_window)
	      && EQ (XWINDOW (mru_window)->frame, frame))
	    new_selected_window = mru_window;

	  /* If all ended up well, we now promote the mru window.  */
	  if (EQ (FRAME_SELECTED_WINDOW (f), selected_window))
	    Fselect_window (new_selected_window, Qnil);
	  else
	    FRAME_SELECTED_WINDOW (f) = new_selected_window;
	}
      else
	UNBLOCK_INPUT;

      /* Must be run by the caller:
	 run_window_configuration_change_hook (f);  */
    }
  else
    /* We failed: Relink WINDOW into window tree.  */
    {
      if (before_sibling)
	{
	  s->prev = window;
	  if (horflag)
	    p->hchild = window;
	  else
	    p->vchild = window;
	}
      else
	{
	  s->next = window;
	  if (!NILP (w->next))
	    XWINDOW (w->next)->prev = window;
	}
      error ("Deletion failed");
    }

  return Qnil;
}

/***********************************************************************
			Resizing Mini-Windows
 ***********************************************************************/

/* Grow mini-window W by DELTA lines, DELTA >= 0, or as much as we
   can.  */
void
grow_mini_window (struct window *w, int delta)
{
  struct frame *f = XFRAME (w->frame);
  struct window *r;
  Lisp_Object root, value;

  xassert (MINI_WINDOW_P (w));
  xassert (delta >= 0);

  root = FRAME_ROOT_WINDOW (f);
  r = XWINDOW (root);
  value = call2 (Qwindow_resize_root_window_vertically,
		 root, make_number (- delta));
  if (INTEGERP (value) && window_resize_check (r, 0))
    {
      BLOCK_INPUT;
      window_resize_apply (r, 0);

      /* Grow the mini-window.  */
      XSETFASTINT (w->top_line, XFASTINT (r->top_line) + XFASTINT (r->total_lines));
      XSETFASTINT (w->total_lines, XFASTINT (w->total_lines) - XINT (value));
      XSETFASTINT (w->last_modified, 0);
      XSETFASTINT (w->last_overlay_modified, 0);

      adjust_glyphs (f);
      UNBLOCK_INPUT;
    }
}


/* Shrink mini-window W.  */
void
shrink_mini_window (struct window *w)
{
  struct frame *f = XFRAME (w->frame);
  struct window *r;
  Lisp_Object root, value;
  EMACS_INT size;

  xassert (MINI_WINDOW_P (w));

  size = XINT (w->total_lines);
  if (size > 1)
    {
      root = FRAME_ROOT_WINDOW (f);
      r = XWINDOW (root);
      value = call2 (Qwindow_resize_root_window_vertically,
		     root, make_number (size - 1));
      if (INTEGERP (value) && window_resize_check (r, 0))
	{
	  BLOCK_INPUT;
	  window_resize_apply (r, 0);

	  /* Shrink the mini-window.  */
	  XSETFASTINT (w->top_line, XFASTINT (r->top_line) + XFASTINT (r->total_lines));
	  XSETFASTINT (w->total_lines, 1);

	  XSETFASTINT (w->last_modified, 0);
	  XSETFASTINT (w->last_overlay_modified, 0);

	  adjust_glyphs (f);
	  UNBLOCK_INPUT;
	}
      /* If the above failed for whatever strange reason we must make a
	 one window frame here.  The same routine will be needed when
	 shrinking the frame (and probably when making the initial
	 *scratch* window).  For the moment leave things as they are.  */
    }
}

DEFUN ("resize-mini-window-internal", Fresize_mini_window_internal, Sresize_mini_window_internal, 1, 1, 0,
       doc: /* Resize minibuffer window WINDOW.  */)
     (Lisp_Object window)
{
  struct window *w = XWINDOW (window);
  struct window *r;
  struct frame *f;
  int height;

  CHECK_WINDOW (window);
  f = XFRAME (w->frame);

  if (!EQ (FRAME_MINIBUF_WINDOW (XFRAME (w->frame)), window))
    error ("Not a valid minibuffer window");
  else if (FRAME_MINIBUF_ONLY_P (f))
    error ("Cannot resize a minibuffer-only frame");

  r = XWINDOW (FRAME_ROOT_WINDOW (f));
  height = XINT (r->total_lines) + XINT (w->total_lines);
  if (window_resize_check (r, 0)
      && XINT (w->new_total) > 0
      && height == XINT (r->new_total) + XINT (w->new_total))
    {
      BLOCK_INPUT;
      window_resize_apply (r, 0);

      w->total_lines = w->new_total;
      XSETFASTINT (w->top_line, XINT (r->top_line) + XINT (r->total_lines));

      windows_or_buffers_changed++;
      FRAME_WINDOW_SIZES_CHANGED (f) = 1;
      adjust_glyphs (f);
      UNBLOCK_INPUT;

      run_window_configuration_change_hook (f);
      return Qt;
    }
  else error ("Failed to resize minibuffer window");
}

/* Mark window cursors off for all windows in the window tree rooted
   at W by setting their phys_cursor_on_p flag to zero.  Called from
   xterm.c, e.g. when a frame is cleared and thereby all cursors on
   the frame are cleared.  */

void
mark_window_cursors_off (struct window *w)
{
  while (w)
    {
      if (!NILP (w->hchild))
	mark_window_cursors_off (XWINDOW (w->hchild));
      else if (!NILP (w->vchild))
	mark_window_cursors_off (XWINDOW (w->vchild));
      else
	w->phys_cursor_on_p = 0;

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}


/* Return number of lines of text (not counting mode lines) in W.  */

int
window_internal_height (struct window *w)
{
  int ht = XFASTINT (w->total_lines);

  if (!MINI_WINDOW_P (w))
    {
      if (!NILP (w->parent)
	  || !NILP (w->vchild)
	  || !NILP (w->hchild)
	  || !NILP (w->next)
	  || !NILP (w->prev)
	  || WINDOW_WANTS_MODELINE_P (w))
	--ht;

      if (WINDOW_WANTS_HEADER_LINE_P (w))
	--ht;
    }

  return ht;
}

/************************************************************************
			   Window Scrolling
 ***********************************************************************/

/* Scroll contents of window WINDOW up.  If WHOLE is non-zero, scroll
   N screen-fulls, which is defined as the height of the window minus
   next_screen_context_lines.  If WHOLE is zero, scroll up N lines
   instead.  Negative values of N mean scroll down.  NOERROR non-zero
   means don't signal an error if we try to move over BEGV or ZV,
   respectively.  */

static void
window_scroll (Lisp_Object window, int n, int whole, int noerror)
{
  immediate_quit = 1;

  /* If we must, use the pixel-based version which is much slower than
     the line-based one but can handle varying line heights.  */
  if (FRAME_WINDOW_P (XFRAME (XWINDOW (window)->frame)))
    window_scroll_pixel_based (window, n, whole, noerror);
  else
    window_scroll_line_based (window, n, whole, noerror);

  immediate_quit = 0;
}


/* Implementation of window_scroll that works based on pixel line
   heights.  See the comment of window_scroll for parameter
   descriptions.  */

static void
window_scroll_pixel_based (Lisp_Object window, int n, int whole, int noerror)
{
  struct it it;
  struct window *w = XWINDOW (window);
  struct text_pos start;
  int this_scroll_margin;
  /* True if we fiddled the window vscroll field without really scrolling.  */
  int vscrolled = 0;
  int x, y, rtop, rbot, rowh, vpos;
  void *itdata = NULL;

  SET_TEXT_POS_FROM_MARKER (start, w->start);
  /* Scrolling a minibuffer window via scroll bar when the echo area
     shows long text sometimes resets the minibuffer contents behind
     our backs.  */
  if (CHARPOS (start) > ZV)
    SET_TEXT_POS (start, BEGV, BEGV_BYTE);

  /* If PT is not visible in WINDOW, move back one half of
     the screen.  Allow PT to be partially visible, otherwise
     something like (scroll-down 1) with PT in the line before
     the partially visible one would recenter. */

  if (!pos_visible_p (w, PT, &x, &y, &rtop, &rbot, &rowh, &vpos))
    {
      itdata = bidi_shelve_cache ();
      /* Move backward half the height of the window.  Performance note:
	 vmotion used here is about 10% faster, but would give wrong
	 results for variable height lines.  */
      init_iterator (&it, w, PT, PT_BYTE, NULL, DEFAULT_FACE_ID);
      it.current_y = it.last_visible_y;
      move_it_vertically_backward (&it, window_box_height (w) / 2);

      /* The function move_iterator_vertically may move over more than
	 the specified y-distance.  If it->w is small, e.g. a
	 mini-buffer window, we may end up in front of the window's
	 display area.  This is the case when Start displaying at the
	 start of the line containing PT in this case.  */
      if (it.current_y <= 0)
	{
	  init_iterator (&it, w, PT, PT_BYTE, NULL, DEFAULT_FACE_ID);
	  move_it_vertically_backward (&it, 0);
	  it.current_y = 0;
	}

      start = it.current.pos;
      bidi_unshelve_cache (itdata, 0);
    }
  else if (auto_window_vscroll_p)
    {
      if (rtop || rbot)		/* partially visible */
	{
	  int px;
	  int dy = WINDOW_FRAME_LINE_HEIGHT (w);
	  if (whole)
	    dy = max ((window_box_height (w)
		       - next_screen_context_lines * dy),
		      dy);
	  dy *= n;

	  if (n < 0)
	    {
	      /* Only vscroll backwards if already vscrolled forwards.  */
	      if (w->vscroll < 0 && rtop > 0)
		{
		  px = max (0, -w->vscroll - min (rtop, -dy));
		  Fset_window_vscroll (window, make_number (px), Qt);
		  return;
		}
	    }
	  if (n > 0)
	    {
	      /* Do vscroll if already vscrolled or only display line.  */
	      if (rbot > 0 && (w->vscroll < 0 || vpos == 0))
		{
		  px = max (0, -w->vscroll + min (rbot, dy));
		  Fset_window_vscroll (window, make_number (px), Qt);
		  return;
		}

	      /* Maybe modify window start instead of scrolling.  */
	      if (rbot > 0 || w->vscroll < 0)
		{
		  EMACS_INT spos;

		  Fset_window_vscroll (window, make_number (0), Qt);
		  /* If there are other text lines above the current row,
		     move window start to current row.  Else to next row. */
		  if (rbot > 0)
		    spos = XINT (Fline_beginning_position (Qnil));
		  else
		    spos = min (XINT (Fline_end_position (Qnil)) + 1, ZV);
		  set_marker_restricted (w->start, make_number (spos),
					 w->buffer);
		  w->start_at_line_beg = Qt;
		  w->update_mode_line = Qt;
		  XSETFASTINT (w->last_modified, 0);
		  XSETFASTINT (w->last_overlay_modified, 0);
		  /* Set force_start so that redisplay_window will run the
		     window-scroll-functions.  */
		  w->force_start = Qt;
		  return;
		}
	    }
	}
      /* Cancel previous vscroll.  */
      Fset_window_vscroll (window, make_number (0), Qt);
    }

  itdata = bidi_shelve_cache ();
  /* If scroll_preserve_screen_position is non-nil, we try to set
     point in the same window line as it is now, so get that line.  */
  if (!NILP (Vscroll_preserve_screen_position))
    {
      /* We preserve the goal pixel coordinate across consecutive
	 calls to scroll-up, scroll-down and other commands that
	 have the `scroll-command' property.  This avoids the
	 possibility of point becoming "stuck" on a tall line when
	 scrolling by one line.  */
      if (window_scroll_pixel_based_preserve_y < 0
	  || !SYMBOLP (KVAR (current_kboard, Vlast_command))
	  || NILP (Fget (KVAR (current_kboard, Vlast_command), Qscroll_command)))
	{
	  start_display (&it, w, start);
	  move_it_to (&it, PT, -1, -1, -1, MOVE_TO_POS);
	  window_scroll_pixel_based_preserve_y = it.current_y;
	  window_scroll_pixel_based_preserve_x = it.current_x;
	}
    }
  else
    window_scroll_pixel_based_preserve_y
      = window_scroll_pixel_based_preserve_x = -1;

  /* Move iterator it from start the specified distance forward or
     backward.  The result is the new window start.  */
  start_display (&it, w, start);
  if (whole)
    {
      EMACS_INT start_pos = IT_CHARPOS (it);
      int dy = WINDOW_FRAME_LINE_HEIGHT (w);
      dy = max ((window_box_height (w)
		 - next_screen_context_lines * dy),
		dy) * n;

      /* Note that move_it_vertically always moves the iterator to the
         start of a line.  So, if the last line doesn't have a newline,
	 we would end up at the start of the line ending at ZV.  */
      if (dy <= 0)
	{
	  move_it_vertically_backward (&it, -dy);
	  /* Ensure we actually do move, e.g. in case we are currently
	     looking at an image that is taller that the window height.  */
	  while (start_pos == IT_CHARPOS (it)
		 && start_pos > BEGV)
	    move_it_by_lines (&it, -1);
	}
      else if (dy > 0)
	{
	  move_it_to (&it, ZV, -1, it.current_y + dy, -1,
		      MOVE_TO_POS | MOVE_TO_Y);
	  /* Ensure we actually do move, e.g. in case we are currently
	     looking at an image that is taller that the window height.  */
	  while (start_pos == IT_CHARPOS (it)
		 && start_pos < ZV)
	    move_it_by_lines (&it, 1);
	}
    }
  else
    move_it_by_lines (&it, n);

  /* We failed if we find ZV is already on the screen (scrolling up,
     means there's nothing past the end), or if we can't start any
     earlier (scrolling down, means there's nothing past the top).  */
  if ((n > 0 && IT_CHARPOS (it) == ZV)
      || (n < 0 && IT_CHARPOS (it) == CHARPOS (start)))
    {
      if (IT_CHARPOS (it) == ZV)
	{
	  if (it.current_y < it.last_visible_y
	      && (it.current_y + it.max_ascent + it.max_descent
		  > it.last_visible_y))
	    {
	      /* The last line was only partially visible, make it fully
		 visible.  */
	      w->vscroll = (it.last_visible_y
			    - it.current_y + it.max_ascent + it.max_descent);
	      adjust_glyphs (it.f);
	    }
	  else
	    {
	      bidi_unshelve_cache (itdata, 0);
	      if (noerror)
		return;
	      else if (n < 0)	/* could happen with empty buffers */
		xsignal0 (Qbeginning_of_buffer);
	      else
		xsignal0 (Qend_of_buffer);
	    }
	}
      else
	{
	  if (w->vscroll != 0)
	    /* The first line was only partially visible, make it fully
	       visible. */
	    w->vscroll = 0;
	  else
	    {
	      bidi_unshelve_cache (itdata, 0);
	      if (noerror)
		return;
	      else
		xsignal0 (Qbeginning_of_buffer);
	    }
	}

      /* If control gets here, then we vscrolled.  */

      XBUFFER (w->buffer)->prevent_redisplay_optimizations_p = 1;

      /* Don't try to change the window start below.  */
      vscrolled = 1;
    }

  if (! vscrolled)
    {
      EMACS_INT pos = IT_CHARPOS (it);
      EMACS_INT bytepos;

      /* If in the middle of a multi-glyph character move forward to
	 the next character.  */
      if (in_display_vector_p (&it))
	{
	  ++pos;
	  move_it_to (&it, pos, -1, -1, -1, MOVE_TO_POS);
	}

      /* Set the window start, and set up the window for redisplay.  */
      set_marker_restricted (w->start, make_number (pos),
			     w->buffer);
      bytepos = XMARKER (w->start)->bytepos;
      w->start_at_line_beg = ((pos == BEGV || FETCH_BYTE (bytepos - 1) == '\n')
			      ? Qt : Qnil);
      w->update_mode_line = Qt;
      XSETFASTINT (w->last_modified, 0);
      XSETFASTINT (w->last_overlay_modified, 0);
      /* Set force_start so that redisplay_window will run the
	 window-scroll-functions.  */
      w->force_start = Qt;
    }

  /* The rest of this function uses current_y in a nonstandard way,
     not including the height of the header line if any.  */
  it.current_y = it.vpos = 0;

  /* Move PT out of scroll margins.
     This code wants current_y to be zero at the window start position
     even if there is a header line.  */
  this_scroll_margin = max (0, scroll_margin);
  this_scroll_margin = min (this_scroll_margin, XFASTINT (w->total_lines) / 4);
  this_scroll_margin *= FRAME_LINE_HEIGHT (it.f);

  if (n > 0)
    {
      /* We moved the window start towards ZV, so PT may be now
	 in the scroll margin at the top.  */
      move_it_to (&it, PT, -1, -1, -1, MOVE_TO_POS);
      if (IT_CHARPOS (it) == PT && it.current_y >= this_scroll_margin
          && (NILP (Vscroll_preserve_screen_position)
	      || EQ (Vscroll_preserve_screen_position, Qt)))
	/* We found PT at a legitimate height.  Leave it alone.  */
	;
      else if (window_scroll_pixel_based_preserve_y >= 0)
	{
	  /* If we have a header line, take account of it.
	     This is necessary because we set it.current_y to 0, above.  */
	  move_it_to (&it, -1,
		      window_scroll_pixel_based_preserve_x,
		      window_scroll_pixel_based_preserve_y
		      - (WINDOW_WANTS_HEADER_LINE_P (w) ? 1 : 0 ),
		      -1, MOVE_TO_Y | MOVE_TO_X);
	  SET_PT_BOTH (IT_CHARPOS (it), IT_BYTEPOS (it));
	}
      else
	{
	  while (it.current_y < this_scroll_margin)
	    {
	      int prev = it.current_y;
	      move_it_by_lines (&it, 1);
	      if (prev == it.current_y)
		break;
	    }
	  SET_PT_BOTH (IT_CHARPOS (it), IT_BYTEPOS (it));
	}
    }
  else if (n < 0)
    {
      EMACS_INT charpos, bytepos;
      int partial_p;

      /* Save our position, for the
	 window_scroll_pixel_based_preserve_y case.  */
      charpos = IT_CHARPOS (it);
      bytepos = IT_BYTEPOS (it);

      /* We moved the window start towards BEGV, so PT may be now
	 in the scroll margin at the bottom.  */
      move_it_to (&it, PT, -1,
		  (it.last_visible_y - CURRENT_HEADER_LINE_HEIGHT (w)
		   - this_scroll_margin - 1),
		  -1,
		  MOVE_TO_POS | MOVE_TO_Y);

      /* Save our position, in case it's correct.  */
      charpos = IT_CHARPOS (it);
      bytepos = IT_BYTEPOS (it);

      /* See if point is on a partially visible line at the end.  */
      if (it.what == IT_EOB)
	partial_p = it.current_y + it.ascent + it.descent > it.last_visible_y;
      else
	{
	  move_it_by_lines (&it, 1);
	  partial_p = it.current_y > it.last_visible_y;
	}

      if (charpos == PT && !partial_p
          && (NILP (Vscroll_preserve_screen_position)
	      || EQ (Vscroll_preserve_screen_position, Qt)))
	/* We found PT before we found the display margin, so PT is ok.  */
	;
      else if (window_scroll_pixel_based_preserve_y >= 0)
	{
	  SET_TEXT_POS_FROM_MARKER (start, w->start);
	  start_display (&it, w, start);
	  /* It would be wrong to subtract CURRENT_HEADER_LINE_HEIGHT
	     here because we called start_display again and did not
	     alter it.current_y this time.  */
	  move_it_to (&it, -1, window_scroll_pixel_based_preserve_x,
		      window_scroll_pixel_based_preserve_y, -1,
		      MOVE_TO_Y | MOVE_TO_X);
	  SET_PT_BOTH (IT_CHARPOS (it), IT_BYTEPOS (it));
	}
      else
	{
	  if (partial_p)
	    /* The last line was only partially visible, so back up two
	       lines to make sure we're on a fully visible line.  */
	    {
	      move_it_by_lines (&it, -2);
	      SET_PT_BOTH (IT_CHARPOS (it), IT_BYTEPOS (it));
	    }
	  else
	    /* No, the position we saved is OK, so use it.  */
	    SET_PT_BOTH (charpos, bytepos);
	}
    }
  bidi_unshelve_cache (itdata, 0);
}


/* Implementation of window_scroll that works based on screen lines.
   See the comment of window_scroll for parameter descriptions.  */

static void
window_scroll_line_based (Lisp_Object window, int n, int whole, int noerror)
{
  register struct window *w = XWINDOW (window);
  /* Fvertical_motion enters redisplay, which can trigger
     fontification, which in turn can modify buffer text (e.g., if the
     fontification functions replace escape sequences with faces, as
     in `grep-mode-font-lock-keywords').  So we use a marker to record
     the old point position, to prevent crashes in SET_PT_BOTH.  */
  Lisp_Object opoint_marker = Fpoint_marker ();
  register EMACS_INT pos, pos_byte;
  register int ht = window_internal_height (w);
  register Lisp_Object tem;
  int lose;
  Lisp_Object bolp;
  EMACS_INT startpos;
  Lisp_Object original_pos = Qnil;

  /* If scrolling screen-fulls, compute the number of lines to
     scroll from the window's height.  */
  if (whole)
    n *= max (1, ht - next_screen_context_lines);

  startpos = marker_position (w->start);

  if (!NILP (Vscroll_preserve_screen_position))
    {
      if (window_scroll_preserve_vpos <= 0
	  || !SYMBOLP (KVAR (current_kboard, Vlast_command))
	  || NILP (Fget (KVAR (current_kboard, Vlast_command), Qscroll_command)))
	{
	  struct position posit
	    = *compute_motion (startpos, 0, 0, 0,
			       PT, ht, 0,
			       -1, XINT (w->hscroll),
			       0, w);
	  window_scroll_preserve_vpos = posit.vpos;
	  window_scroll_preserve_hpos = posit.hpos + XINT (w->hscroll);
	}

      original_pos = Fcons (make_number (window_scroll_preserve_hpos),
			    make_number (window_scroll_preserve_vpos));
    }

  XSETFASTINT (tem, PT);
  tem = Fpos_visible_in_window_p (tem, window, Qnil);

  if (NILP (tem))
    {
      Fvertical_motion (make_number (- (ht / 2)), window);
      startpos = PT;
    }

  SET_PT (startpos);
  lose = n < 0 && PT == BEGV;
  Fvertical_motion (make_number (n), window);
  pos = PT;
  pos_byte = PT_BYTE;
  bolp = Fbolp ();
  SET_PT_BOTH (marker_position (opoint_marker),
	       marker_byte_position (opoint_marker));

  if (lose)
    {
      if (noerror)
	return;
      else
	xsignal0 (Qbeginning_of_buffer);
    }

  if (pos < ZV)
    {
      /* Don't use a scroll margin that is negative or too large.  */
      int this_scroll_margin =
	max (0, min (scroll_margin, XINT (w->total_lines) / 4));

      set_marker_restricted_both (w->start, w->buffer, pos, pos_byte);
      w->start_at_line_beg = bolp;
      w->update_mode_line = Qt;
      XSETFASTINT (w->last_modified, 0);
      XSETFASTINT (w->last_overlay_modified, 0);
      /* Set force_start so that redisplay_window will run
	 the window-scroll-functions.  */
      w->force_start = Qt;

      if (!NILP (Vscroll_preserve_screen_position)
	  && (whole || !EQ (Vscroll_preserve_screen_position, Qt)))
	{
	  SET_PT_BOTH (pos, pos_byte);
	  Fvertical_motion (original_pos, window);
	}
      /* If we scrolled forward, put point enough lines down
	 that it is outside the scroll margin.  */
      else if (n > 0)
	{
	  int top_margin;

	  if (this_scroll_margin > 0)
	    {
	      SET_PT_BOTH (pos, pos_byte);
	      Fvertical_motion (make_number (this_scroll_margin), window);
	      top_margin = PT;
	    }
	  else
	    top_margin = pos;

	  if (top_margin <= marker_position (opoint_marker))
	    SET_PT_BOTH (marker_position (opoint_marker),
			 marker_byte_position (opoint_marker));
	  else if (!NILP (Vscroll_preserve_screen_position))
	    {
	      SET_PT_BOTH (pos, pos_byte);
	      Fvertical_motion (original_pos, window);
	    }
	  else
	    SET_PT (top_margin);
	}
      else if (n < 0)
	{
	  int bottom_margin;

	  /* If we scrolled backward, put point near the end of the window
	     but not within the scroll margin.  */
	  SET_PT_BOTH (pos, pos_byte);
	  tem = Fvertical_motion (make_number (ht - this_scroll_margin), window);
	  if (XFASTINT (tem) == ht - this_scroll_margin)
	    bottom_margin = PT;
	  else
	    bottom_margin = PT + 1;

	  if (bottom_margin > marker_position (opoint_marker))
	    SET_PT_BOTH (marker_position (opoint_marker),
			 marker_byte_position (opoint_marker));
	  else
	    {
	      if (!NILP (Vscroll_preserve_screen_position))
		{
		  SET_PT_BOTH (pos, pos_byte);
		  Fvertical_motion (original_pos, window);
		}
	      else
		Fvertical_motion (make_number (-1), window);
	    }
	}
    }
  else
    {
      if (noerror)
	return;
      else
	xsignal0 (Qend_of_buffer);
    }
}


/* Scroll selected_window up or down.  If N is nil, scroll a
   screen-full which is defined as the height of the window minus
   next_screen_context_lines.  If N is the symbol `-', scroll.
   DIRECTION may be 1 meaning to scroll down, or -1 meaning to scroll
   up.  This is the guts of Fscroll_up and Fscroll_down.  */

static void
scroll_command (Lisp_Object n, int direction)
{
  int count = SPECPDL_INDEX ();

  xassert (eabs (direction) == 1);

  /* If selected window's buffer isn't current, make it current for
     the moment.  But don't screw up if window_scroll gets an error.  */
  if (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer)
    {
      record_unwind_protect (save_excursion_restore, save_excursion_save ());
      Fset_buffer (XWINDOW (selected_window)->buffer);

      /* Make redisplay consider other windows than just selected_window.  */
      ++windows_or_buffers_changed;
    }

  if (NILP (n))
    window_scroll (selected_window, direction, 1, 0);
  else if (EQ (n, Qminus))
    window_scroll (selected_window, -direction, 1, 0);
  else
    {
      n = Fprefix_numeric_value (n);
      window_scroll (selected_window, XINT (n) * direction, 0, 0);
    }

  unbind_to (count, Qnil);
}

DEFUN ("scroll-up", Fscroll_up, Sscroll_up, 0, 1, "^P",
       doc: /* Scroll text of selected window upward ARG lines.
If ARG is omitted or nil, scroll upward by a near full screen.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'.  */)
  (Lisp_Object arg)
{
  scroll_command (arg, 1);
  return Qnil;
}

DEFUN ("scroll-down", Fscroll_down, Sscroll_down, 0, 1, "^P",
       doc: /* Scroll text of selected window down ARG lines.
If ARG is omitted or nil, scroll down by a near full screen.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'.  */)
  (Lisp_Object arg)
{
  scroll_command (arg, -1);
  return Qnil;
}

DEFUN ("other-window-for-scrolling", Fother_window_for_scrolling, Sother_window_for_scrolling, 0, 0, 0,
       doc: /* Return the other window for \"other window scroll\" commands.
If `other-window-scroll-buffer' is non-nil, a window
showing that buffer is used.
If in the minibuffer, `minibuffer-scroll-window' if non-nil
specifies the window.  This takes precedence over
`other-window-scroll-buffer'.  */)
  (void)
{
  Lisp_Object window;

  if (MINI_WINDOW_P (XWINDOW (selected_window))
      && !NILP (Vminibuf_scroll_window))
    window = Vminibuf_scroll_window;
  /* If buffer is specified, scroll that buffer.  */
  else if (!NILP (Vother_window_scroll_buffer))
    {
      window = Fget_buffer_window (Vother_window_scroll_buffer, Qnil);
      if (NILP (window))
	window = display_buffer (Vother_window_scroll_buffer, Qt, Qnil);
    }
  else
    {
      /* Nothing specified; look for a neighboring window on the same
	 frame.  */
      window = Fnext_window (selected_window, Qnil, Qnil);

      if (EQ (window, selected_window))
	/* That didn't get us anywhere; look for a window on another
           visible frame.  */
	do
	  window = Fnext_window (window, Qnil, Qt);
	while (! FRAME_VISIBLE_P (XFRAME (WINDOW_FRAME (XWINDOW (window))))
	       && ! EQ (window, selected_window));
    }

  CHECK_LIVE_WINDOW (window);

  if (EQ (window, selected_window))
    error ("There is no other window");

  return window;
}

DEFUN ("scroll-other-window", Fscroll_other_window, Sscroll_other_window, 0, 1, "P",
       doc: /* Scroll next window upward ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
The next window is the one below the current one; or the one at the top
if the current one is at the bottom.  Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'.

If `other-window-scroll-buffer' is non-nil, scroll the window
showing that buffer, popping the buffer up if necessary.
If in the minibuffer, `minibuffer-scroll-window' if non-nil
specifies the window to scroll.  This takes precedence over
`other-window-scroll-buffer'.  */)
  (Lisp_Object arg)
{
  Lisp_Object window;
  struct window *w;
  int count = SPECPDL_INDEX ();

  window = Fother_window_for_scrolling ();
  w = XWINDOW (window);

  /* Don't screw up if window_scroll gets an error.  */
  record_unwind_protect (save_excursion_restore, save_excursion_save ());
  ++windows_or_buffers_changed;

  Fset_buffer (w->buffer);
  SET_PT (marker_position (w->pointm));

  if (NILP (arg))
    window_scroll (window, 1, 1, 1);
  else if (EQ (arg, Qminus))
    window_scroll (window, -1, 1, 1);
  else
    {
      if (CONSP (arg))
	arg = Fcar (arg);
      CHECK_NUMBER (arg);
      window_scroll (window, XINT (arg), 0, 1);
    }

  set_marker_both (w->pointm, Qnil, PT, PT_BYTE);
  unbind_to (count, Qnil);

  return Qnil;
}

DEFUN ("scroll-left", Fscroll_left, Sscroll_left, 0, 2, "^P\np",
       doc: /* Scroll selected window display ARG columns left.
Default for ARG is window width minus 2.
Value is the total amount of leftward horizontal scrolling in
effect after the change.
If SET-MINIMUM is non-nil, the new scroll amount becomes the
lower bound for automatic scrolling, i.e. automatic scrolling
will not scroll a window to a column less than the value returned
by this function.  This happens in an interactive call.  */)
  (register Lisp_Object arg, Lisp_Object set_minimum)
{
  Lisp_Object result;
  int hscroll;
  struct window *w = XWINDOW (selected_window);

  if (NILP (arg))
    XSETFASTINT (arg, window_body_cols (w) - 2);
  else
    arg = Fprefix_numeric_value (arg);

  hscroll = XINT (w->hscroll) + XINT (arg);
  result = Fset_window_hscroll (selected_window, make_number (hscroll));

  if (!NILP (set_minimum))
    w->min_hscroll = w->hscroll;

  return result;
}

DEFUN ("scroll-right", Fscroll_right, Sscroll_right, 0, 2, "^P\np",
       doc: /* Scroll selected window display ARG columns right.
Default for ARG is window width minus 2.
Value is the total amount of leftward horizontal scrolling in
effect after the change.
If SET-MINIMUM is non-nil, the new scroll amount becomes the
lower bound for automatic scrolling, i.e. automatic scrolling
will not scroll a window to a column less than the value returned
by this function.  This happens in an interactive call.  */)
  (register Lisp_Object arg, Lisp_Object set_minimum)
{
  Lisp_Object result;
  int hscroll;
  struct window *w = XWINDOW (selected_window);

  if (NILP (arg))
    XSETFASTINT (arg, window_body_cols (w) - 2);
  else
    arg = Fprefix_numeric_value (arg);

  hscroll = XINT (w->hscroll) - XINT (arg);
  result = Fset_window_hscroll (selected_window, make_number (hscroll));

  if (!NILP (set_minimum))
    w->min_hscroll = w->hscroll;

  return result;
}

DEFUN ("minibuffer-selected-window", Fminibuffer_selected_window, Sminibuffer_selected_window, 0, 0, 0,
       doc: /* Return the window which was selected when entering the minibuffer.
Returns nil, if selected window is not a minibuffer window.  */)
  (void)
{
  if (minibuf_level > 0
      && MINI_WINDOW_P (XWINDOW (selected_window))
      && WINDOW_LIVE_P (minibuf_selected_window))
    return minibuf_selected_window;

  return Qnil;
}

/* Value is the number of lines actually displayed in window W,
   as opposed to its height.  */

static int
displayed_window_lines (struct window *w)
{
  struct it it;
  struct text_pos start;
  int height = window_box_height (w);
  struct buffer *old_buffer;
  int bottom_y;
  void *itdata = NULL;

  if (XBUFFER (w->buffer) != current_buffer)
    {
      old_buffer = current_buffer;
      set_buffer_internal (XBUFFER (w->buffer));
    }
  else
    old_buffer = NULL;

  /* In case W->start is out of the accessible range, do something
     reasonable.  This happens in Info mode when Info-scroll-down
     calls (recenter -1) while W->start is 1.  */
  if (XMARKER (w->start)->charpos < BEGV)
    SET_TEXT_POS (start, BEGV, BEGV_BYTE);
  else if (XMARKER (w->start)->charpos > ZV)
    SET_TEXT_POS (start, ZV, ZV_BYTE);
  else
    SET_TEXT_POS_FROM_MARKER (start, w->start);

  itdata = bidi_shelve_cache ();
  start_display (&it, w, start);
  move_it_vertically (&it, height);
  bottom_y = line_bottom_y (&it);
  bidi_unshelve_cache (itdata, 0);

  /* rms: On a non-window display,
     the value of it.vpos at the bottom of the screen
     seems to be 1 larger than window_box_height (w).
     This kludge fixes a bug whereby (move-to-window-line -1)
     when ZV is on the last screen line
     moves to the previous screen line instead of the last one.  */
  if (! FRAME_WINDOW_P (XFRAME (w->frame)))
    height++;

  /* Add in empty lines at the bottom of the window.  */
  if (bottom_y < height)
    {
      int uy = FRAME_LINE_HEIGHT (it.f);
      it.vpos += (height - bottom_y + uy - 1) / uy;
    }

  if (old_buffer)
    set_buffer_internal (old_buffer);

  return it.vpos;
}


DEFUN ("recenter", Frecenter, Srecenter, 0, 1, "P",
       doc: /* Center point in selected window and maybe redisplay frame.
With prefix argument ARG, recenter putting point on screen line ARG
relative to the selected window.  If ARG is negative, it counts up from the
bottom of the window.  (ARG should be less than the height of the window.)

If ARG is omitted or nil, then recenter with point on the middle line of
the selected window; if the variable `recenter-redisplay' is non-nil,
also erase the entire frame and redraw it (when `auto-resize-tool-bars'
is set to `grow-only', this resets the tool-bar's height to the minimum
height needed); if `recenter-redisplay' has the special value `tty',
then only tty frames are redrawn.

Just C-u as prefix means put point in the center of the window
and redisplay normally--don't erase and redraw the frame.  */)
  (register Lisp_Object arg)
{
  struct window *w = XWINDOW (selected_window);
  struct buffer *buf = XBUFFER (w->buffer);
  struct buffer *obuf = current_buffer;
  int center_p = 0;
  EMACS_INT charpos, bytepos;
  EMACS_INT iarg IF_LINT (= 0);
  int this_scroll_margin;

  /* If redisplay is suppressed due to an error, try again.  */
  obuf->display_error_modiff = 0;

  if (NILP (arg))
    {
      if (!NILP (Vrecenter_redisplay)
	  && (!EQ (Vrecenter_redisplay, Qtty)
	      || !NILP (Ftty_type (selected_frame))))
	{
	  ptrdiff_t i;

	  /* Invalidate pixel data calculated for all compositions.  */
	  for (i = 0; i < n_compositions; i++)
	    composition_table[i]->font = NULL;

	  WINDOW_XFRAME (w)->minimize_tool_bar_window_p = 1;

	  Fredraw_frame (WINDOW_FRAME (w));
	  SET_FRAME_GARBAGED (WINDOW_XFRAME (w));
	}

      center_p = 1;
    }
  else if (CONSP (arg)) /* Just C-u. */
    center_p = 1;
  else
    {
      arg = Fprefix_numeric_value (arg);
      CHECK_NUMBER (arg);
      iarg = XINT (arg);
    }

  set_buffer_internal (buf);

  /* Do this after making BUF current
     in case scroll_margin is buffer-local.  */
  this_scroll_margin =
    max (0, min (scroll_margin, XFASTINT (w->total_lines) / 4));

  /* Handle centering on a graphical frame specially.  Such frames can
     have variable-height lines and centering point on the basis of
     line counts would lead to strange effects.  */
  if (FRAME_WINDOW_P (XFRAME (w->frame)))
    {
      if (center_p)
	{
	  struct it it;
	  struct text_pos pt;
	  void *itdata = bidi_shelve_cache ();

	  SET_TEXT_POS (pt, PT, PT_BYTE);
	  start_display (&it, w, pt);
	  move_it_vertically_backward (&it, window_box_height (w) / 2);
	  charpos = IT_CHARPOS (it);
	  bytepos = IT_BYTEPOS (it);
	  bidi_unshelve_cache (itdata, 0);
	}
      else if (iarg < 0)
	{
	  struct it it;
	  struct text_pos pt;
	  int nlines = min (INT_MAX, -iarg);
	  int extra_line_spacing;
	  int h = window_box_height (w);
	  void *itdata = bidi_shelve_cache ();

	  iarg = - max (-iarg, this_scroll_margin);

	  SET_TEXT_POS (pt, PT, PT_BYTE);
	  start_display (&it, w, pt);

	  /* Be sure we have the exact height of the full line containing PT.  */
	  move_it_by_lines (&it, 0);

	  /* The amount of pixels we have to move back is the window
	     height minus what's displayed in the line containing PT,
	     and the lines below.  */
	  it.current_y = 0;
	  it.vpos = 0;
	  move_it_by_lines (&it, nlines);

	  if (it.vpos == nlines)
	    h -= it.current_y;
	  else
	    {
	      /* Last line has no newline */
	      h -= line_bottom_y (&it);
	      it.vpos++;
	    }

	  /* Don't reserve space for extra line spacing of last line.  */
	  extra_line_spacing = it.max_extra_line_spacing;

	  /* If we can't move down NLINES lines because we hit
	     the end of the buffer, count in some empty lines.  */
	  if (it.vpos < nlines)
	    {
	      nlines -= it.vpos;
	      extra_line_spacing = it.extra_line_spacing;
	      h -= nlines * (FRAME_LINE_HEIGHT (it.f) + extra_line_spacing);
	    }
	  if (h <= 0)
	    {
	      bidi_unshelve_cache (itdata, 0);
	      return Qnil;
	    }

	  /* Now find the new top line (starting position) of the window.  */
	  start_display (&it, w, pt);
	  it.current_y = 0;
	  move_it_vertically_backward (&it, h);

	  /* If extra line spacing is present, we may move too far
	     back.  This causes the last line to be only partially
	     visible (which triggers redisplay to recenter that line
	     in the middle), so move forward.
	     But ignore extra line spacing on last line, as it is not
	     considered to be part of the visible height of the line.
	  */
	  h += extra_line_spacing;
	  while (-it.current_y > h)
	    move_it_by_lines (&it, 1);

	  charpos = IT_CHARPOS (it);
	  bytepos = IT_BYTEPOS (it);

	  bidi_unshelve_cache (itdata, 0);
	}
      else
	{
	  struct position pos;

	  iarg = max (iarg, this_scroll_margin);

	  pos = *vmotion (PT, -iarg, w);
	  charpos = pos.bufpos;
	  bytepos = pos.bytepos;
	}
    }
  else
    {
      struct position pos;
      int ht = window_internal_height (w);

      if (center_p)
	iarg = ht / 2;
      else if (iarg < 0)
	iarg += ht;

      /* Don't let it get into the margin at either top or bottom.  */
      iarg = max (iarg, this_scroll_margin);
      iarg = min (iarg, ht - this_scroll_margin - 1);

      pos = *vmotion (PT, - iarg, w);
      charpos = pos.bufpos;
      bytepos = pos.bytepos;
    }

  /* Set the new window start.  */
  set_marker_both (w->start, w->buffer, charpos, bytepos);
  w->window_end_valid = Qnil;

  w->optional_new_start = Qt;

  if (bytepos == BEGV_BYTE || FETCH_BYTE (bytepos - 1) == '\n')
    w->start_at_line_beg = Qt;
  else
    w->start_at_line_beg = Qnil;

  set_buffer_internal (obuf);
  return Qnil;
}

DEFUN ("window-text-height", Fwindow_text_height, Swindow_text_height,
       0, 1, 0,
       doc: /* Return the height in lines of the text display area of WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.

The returned height does not include the mode line, any header line,
nor any partial-height lines at the bottom of the text area.  */)
  (Lisp_Object window)
{
  struct window *w = decode_window (window);
  int pixel_height = window_box_height (w);
  int line_height = pixel_height / FRAME_LINE_HEIGHT (XFRAME (w->frame));
  return make_number (line_height);
}



DEFUN ("move-to-window-line", Fmove_to_window_line, Smove_to_window_line,
       1, 1, "P",
       doc: /* Position point relative to window.
ARG nil means position point at center of window.
Else, ARG specifies vertical position within the window;
zero means top of window, negative means relative to bottom of window.  */)
  (Lisp_Object arg)
{
  struct window *w = XWINDOW (selected_window);
  int lines, start;
  Lisp_Object window;
#if 0
  int this_scroll_margin;
#endif

  if (!(BUFFERP (w->buffer)
	&& XBUFFER (w->buffer) == current_buffer))
    /* This test is needed to make sure PT/PT_BYTE make sense in w->buffer
       when passed below to set_marker_both.  */
    error ("move-to-window-line called from unrelated buffer");

  window = selected_window;
  start = marker_position (w->start);
  if (start < BEGV || start > ZV)
    {
      int height = window_internal_height (w);
      Fvertical_motion (make_number (- (height / 2)), window);
      set_marker_both (w->start, w->buffer, PT, PT_BYTE);
      w->start_at_line_beg = Fbolp ();
      w->force_start = Qt;
    }
  else
    Fgoto_char (w->start);

  lines = displayed_window_lines (w);

#if 0
  this_scroll_margin = max (0, min (scroll_margin, lines / 4));
#endif

  if (NILP (arg))
    XSETFASTINT (arg, lines / 2);
  else
    {
      EMACS_INT iarg = XINT (Fprefix_numeric_value (arg));

      if (iarg < 0)
	iarg = iarg + lines;

#if 0  /* This code would prevent move-to-window-line from moving point
	  to a place inside the scroll margins (which would cause the
	  next redisplay to scroll).  I wrote this code, but then concluded
	  it is probably better not to install it.  However, it is here
	  inside #if 0 so as not to lose it.  -- rms.  */

      /* Don't let it get into the margin at either top or bottom.  */
      iarg = max (iarg, this_scroll_margin);
      iarg = min (iarg, lines - this_scroll_margin - 1);
#endif

      arg = make_number (iarg);
    }

  /* Skip past a partially visible first line.  */
  if (w->vscroll)
    XSETINT (arg, XINT (arg) + 1);

  return Fvertical_motion (arg, window);
}



/***********************************************************************
			 Window Configuration
 ***********************************************************************/

struct save_window_data
  {
    struct vectorlike_header header;
    Lisp_Object selected_frame;
    Lisp_Object current_window;
    Lisp_Object current_buffer;
    Lisp_Object minibuf_scroll_window;
    Lisp_Object minibuf_selected_window;
    Lisp_Object root_window;
    Lisp_Object focus_frame;
    /* A vector, each of whose elements is a struct saved_window
       for one window.  */
    Lisp_Object saved_windows;

    /* All fields above are traced by the GC.
       From `fame-cols' down, the fields are ignored by the GC.  */

    int frame_cols, frame_lines, frame_menu_bar_lines;
    int frame_tool_bar_lines;
  };

/* This is saved as a Lisp_Vector  */
struct saved_window
{
  struct vectorlike_header header;

  Lisp_Object window, buffer, start, pointm, mark;
  Lisp_Object left_col, top_line, total_cols, total_lines;
  Lisp_Object normal_cols, normal_lines;
  Lisp_Object hscroll, min_hscroll;
  Lisp_Object parent, prev;
  Lisp_Object start_at_line_beg;
  Lisp_Object display_table;
  Lisp_Object left_margin_cols, right_margin_cols;
  Lisp_Object left_fringe_width, right_fringe_width, fringes_outside_margins;
  Lisp_Object scroll_bar_width, vertical_scroll_bar_type, dedicated;
  Lisp_Object combination_limit, window_parameters;
};

#define SAVED_WINDOW_N(swv,n) \
  ((struct saved_window *) (XVECTOR ((swv)->contents[(n)])))

DEFUN ("window-configuration-p", Fwindow_configuration_p, Swindow_configuration_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a window-configuration object.  */)
  (Lisp_Object object)
{
  return WINDOW_CONFIGURATIONP (object) ? Qt : Qnil;
}

DEFUN ("window-configuration-frame", Fwindow_configuration_frame, Swindow_configuration_frame, 1, 1, 0,
       doc: /* Return the frame that CONFIG, a window-configuration object, is about.  */)
  (Lisp_Object config)
{
  register struct save_window_data *data;
  struct Lisp_Vector *saved_windows;

  CHECK_WINDOW_CONFIGURATION (config);

  data = (struct save_window_data *) XVECTOR (config);
  saved_windows = XVECTOR (data->saved_windows);
  return XWINDOW (SAVED_WINDOW_N (saved_windows, 0)->window)->frame;
}

DEFUN ("set-window-configuration", Fset_window_configuration,
       Sset_window_configuration, 1, 1, 0,
       doc: /* Set the configuration of windows and buffers as specified by CONFIGURATION.
CONFIGURATION must be a value previously returned
by `current-window-configuration' (which see).
If CONFIGURATION was made from a frame that is now deleted,
only frame-independent values can be restored.  In this case,
the return value is nil.  Otherwise the value is t.  */)
  (Lisp_Object configuration)
{
  register struct save_window_data *data;
  struct Lisp_Vector *saved_windows;
  Lisp_Object new_current_buffer;
  Lisp_Object frame;
  Lisp_Object auto_buffer_name;
  FRAME_PTR f;
  EMACS_INT old_point = -1;

  CHECK_WINDOW_CONFIGURATION (configuration);

  data = (struct save_window_data *) XVECTOR (configuration);
  saved_windows = XVECTOR (data->saved_windows);

  new_current_buffer = data->current_buffer;
  if (NILP (BVAR (XBUFFER (new_current_buffer), name)))
    new_current_buffer = Qnil;
  else
    {
      if (XBUFFER (new_current_buffer) == current_buffer)
	/* The code further down "preserves point" by saving here PT in
	   old_point and then setting it later back into PT.  When the
	   current-selected-window and the final-selected-window both show
	   the current buffer, this suffers from the problem that the
	   current PT is the window-point of the current-selected-window,
	   while the final PT is the point of the final-selected-window, so
	   this copy from one PT to the other would end up moving the
	   window-point of the final-selected-window to the window-point of
	   the current-selected-window.  So we have to be careful which
	   point of the current-buffer we copy into old_point.  */
	if (EQ (XWINDOW (data->current_window)->buffer, new_current_buffer)
	    && WINDOWP (selected_window)
	    && EQ (XWINDOW (selected_window)->buffer, new_current_buffer)
	    && !EQ (selected_window, data->current_window))
	  old_point = XMARKER (XWINDOW (data->current_window)->pointm)->charpos;
	else
	  old_point = PT;
      else
	/* BUF_PT (XBUFFER (new_current_buffer)) gives us the position of
	   point in new_current_buffer as of the last time this buffer was
	   used.  This can be non-deterministic since it can be changed by
	   things like jit-lock by mere temporary selection of some random
	   window that happens to show this buffer.
	   So if possible we want this arbitrary choice of "which point" to
	   be the one from the to-be-selected-window so as to prevent this
	   window's cursor from being copied from another window.  */
	if (EQ (XWINDOW (data->current_window)->buffer, new_current_buffer)
	    /* If current_window = selected_window, its point is in BUF_PT.  */
	    && !EQ (selected_window, data->current_window))
	  old_point = XMARKER (XWINDOW (data->current_window)->pointm)->charpos;
	else
	  old_point = BUF_PT (XBUFFER (new_current_buffer));
    }

  frame = XWINDOW (SAVED_WINDOW_N (saved_windows, 0)->window)->frame;
  f = XFRAME (frame);

  /* If f is a dead frame, don't bother rebuilding its window tree.
     However, there is other stuff we should still try to do below.  */
  if (FRAME_LIVE_P (f))
    {
      Lisp_Object window;
      Lisp_Object dead_windows = Qnil;
      register Lisp_Object tem, par, pers;
      register struct window *w;
      register struct saved_window *p;
      struct window *root_window;
      struct window **leaf_windows;
      int n_leaf_windows;
      ptrdiff_t k;
      int i, n;

      /* If the frame has been resized since this window configuration was
	 made, we change the frame to the size specified in the
	 configuration, restore the configuration, and then resize it
	 back.  We keep track of the prevailing height in these variables.  */
      int previous_frame_lines = FRAME_LINES (f);
      int previous_frame_cols =  FRAME_COLS  (f);
      int previous_frame_menu_bar_lines = FRAME_MENU_BAR_LINES (f);
      int previous_frame_tool_bar_lines = FRAME_TOOL_BAR_LINES (f);

      /* The mouse highlighting code could get screwed up
	 if it runs during this.  */
      BLOCK_INPUT;

      if (data->frame_lines != previous_frame_lines
	  || data->frame_cols != previous_frame_cols)
	change_frame_size (f, data->frame_lines,
			   data->frame_cols, 0, 0, 0);
#if defined (HAVE_WINDOW_SYSTEM) || defined (MSDOS)
      if (data->frame_menu_bar_lines
	  != previous_frame_menu_bar_lines)
	x_set_menu_bar_lines (f, make_number (data->frame_menu_bar_lines),
			      make_number (0));
#ifdef HAVE_WINDOW_SYSTEM
      if (data->frame_tool_bar_lines
	  != previous_frame_tool_bar_lines)
	x_set_tool_bar_lines (f, make_number (data->frame_tool_bar_lines),
			      make_number (0));
#endif
#endif

      /* "Swap out" point from the selected window's buffer
	 into the window itself.  (Normally the pointm of the selected
	 window holds garbage.)  We do this now, before
	 restoring the window contents, and prevent it from
	 being done later on when we select a new window.  */
      if (! NILP (XWINDOW (selected_window)->buffer))
	{
	  w = XWINDOW (selected_window);
	  set_marker_both (w->pointm,
			   w->buffer,
			   BUF_PT (XBUFFER (w->buffer)),
			   BUF_PT_BYTE (XBUFFER (w->buffer)));
	}

      windows_or_buffers_changed++;
      FRAME_WINDOW_SIZES_CHANGED (f) = 1;

      /* Problem: Freeing all matrices and later allocating them again
	 is a serious redisplay flickering problem.  What we would
	 really like to do is to free only those matrices not reused
	 below.  */
      root_window = XWINDOW (FRAME_ROOT_WINDOW (f));
      leaf_windows
	= (struct window **) alloca (count_windows (root_window)
				     * sizeof (struct window *));
      n_leaf_windows = get_leaf_windows (root_window, leaf_windows, 0);

      /* Kludge Alert!
	 Mark all windows now on frame as "deleted".
	 Restoring the new configuration "undeletes" any that are in it.

	 Save their current buffers in their height fields, since we may
	 need it later, if a buffer saved in the configuration is now
	 dead.  */
      delete_all_child_windows (FRAME_ROOT_WINDOW (f));

      for (k = 0; k < saved_windows->header.size; k++)
	{
	  p = SAVED_WINDOW_N (saved_windows, k);
	  window = p->window;
	  w = XWINDOW (window);
	  w->next = Qnil;

	  if (!NILP (p->parent))
	    w->parent = SAVED_WINDOW_N (saved_windows,
					XFASTINT (p->parent))->window;
	  else
	    w->parent = Qnil;

	  if (!NILP (p->prev))
	    {
	      w->prev = SAVED_WINDOW_N (saved_windows,
					XFASTINT (p->prev))->window;
	      XWINDOW (w->prev)->next = p->window;
	    }
	  else
	    {
	      w->prev = Qnil;
	      if (!NILP (w->parent))
		{
		  if (EQ (p->total_cols, XWINDOW (w->parent)->total_cols))
		    {
		      XWINDOW (w->parent)->vchild = p->window;
		      XWINDOW (w->parent)->hchild = Qnil;
		    }
		  else
		    {
		      XWINDOW (w->parent)->hchild = p->window;
		      XWINDOW (w->parent)->vchild = Qnil;
		    }
		}
	    }

	  /* If we squirreled away the buffer in the window's height,
	     restore it now.  */
	  if (BUFFERP (w->total_lines))
	    w->buffer = w->total_lines;
	  w->left_col = p->left_col;
	  w->top_line = p->top_line;
	  w->total_cols = p->total_cols;
	  w->total_lines = p->total_lines;
	  w->normal_cols = p->normal_cols;
	  w->normal_lines = p->normal_lines;
	  w->hscroll = p->hscroll;
	  w->min_hscroll = p->min_hscroll;
	  w->display_table = p->display_table;
	  w->left_margin_cols = p->left_margin_cols;
	  w->right_margin_cols = p->right_margin_cols;
	  w->left_fringe_width = p->left_fringe_width;
	  w->right_fringe_width = p->right_fringe_width;
	  w->fringes_outside_margins = p->fringes_outside_margins;
	  w->scroll_bar_width = p->scroll_bar_width;
	  w->vertical_scroll_bar_type = p->vertical_scroll_bar_type;
	  w->dedicated = p->dedicated;
	  w->combination_limit = p->combination_limit;
	  /* Restore any window parameters that have been saved.
	     Parameters that have not been saved are left alone.  */
	  for (tem = p->window_parameters; CONSP (tem); tem = XCDR (tem))
	    {
	      pers = XCAR (tem);
	      if (CONSP (pers))
		{
		  if (NILP (XCDR (pers)))
		    {
		      par = Fassq (XCAR (pers), w->window_parameters);
		      if (CONSP (par) && !NILP (XCDR (par)))
			/* Reset a parameter to nil if and only if it
			   has a non-nil association.  Don't make new
			   associations.  */
			Fsetcdr (par, Qnil);
		    }
		  else
		    /* Always restore a non-nil value.  */
		    Fset_window_parameter (window, XCAR (pers), XCDR (pers));
		}
	    }

	  XSETFASTINT (w->last_modified, 0);
	  XSETFASTINT (w->last_overlay_modified, 0);

	  /* Reinstall the saved buffer and pointers into it.  */
	  if (NILP (p->buffer))
	    /* An internal window.  */
	    w->buffer = p->buffer;
	  else if (!NILP (BVAR (XBUFFER (p->buffer), name)))
	    /* If saved buffer is alive, install it.  */
	    {
	      w->buffer = p->buffer;
	      w->start_at_line_beg = p->start_at_line_beg;
	      set_marker_restricted (w->start, p->start, w->buffer);
	      set_marker_restricted (w->pointm, p->pointm, w->buffer);
	      Fset_marker (BVAR (XBUFFER (w->buffer), mark),
			   p->mark, w->buffer);

	      /* As documented in Fcurrent_window_configuration, don't
		 restore the location of point in the buffer which was
		 current when the window configuration was recorded.  */
	      if (!EQ (p->buffer, new_current_buffer)
		  && XBUFFER (p->buffer) == current_buffer)
		Fgoto_char (w->pointm);
	    }
	  else if (!NILP (w->buffer) && !NILP (BVAR (XBUFFER (w->buffer), name)))
	    /* Keep window's old buffer; make sure the markers are
	       real.  */
	    {
	      /* Set window markers at start of visible range.  */
	      if (XMARKER (w->start)->buffer == 0)
		set_marker_restricted (w->start, make_number (0),
				       w->buffer);
	      if (XMARKER (w->pointm)->buffer == 0)
		set_marker_restricted_both (w->pointm, w->buffer,
					    BUF_PT (XBUFFER (w->buffer)),
					    BUF_PT_BYTE (XBUFFER (w->buffer)));
	      w->start_at_line_beg = Qt;
	    }
	  else if (STRINGP (auto_buffer_name =
			    Fwindow_parameter (window, Qauto_buffer_name))
		   && SCHARS (auto_buffer_name) != 0
		   && !NILP (w->buffer = Fget_buffer_create (auto_buffer_name)))
	    {
	      set_marker_restricted (w->start, make_number (0), w->buffer);
	      set_marker_restricted (w->pointm, make_number (0), w->buffer);
	      w->start_at_line_beg = Qt;
	    }
	  else
	    /* Window has no live buffer, get one.  */
	    {
	      /* Get the buffer via other_buffer_safely in order to
	      avoid showing an unimportant buffer and, if necessary, to
	      recreate *scratch* in the course (part of Juanma's bs-show
	      scenario from March 2011).  */
	      w->buffer = other_buffer_safely (Fcurrent_buffer ());
	      /* This will set the markers to beginning of visible
		 range.  */
	      set_marker_restricted (w->start, make_number (0), w->buffer);
	      set_marker_restricted (w->pointm, make_number (0), w->buffer);
	      w->start_at_line_beg = Qt;
	      if (!NILP (w->dedicated))
		/* Record this window as dead.  */
		dead_windows = Fcons (window, dead_windows);
	      /* Make sure window is no more dedicated.  */
	      w->dedicated = Qnil;
	    }
	}

      FRAME_ROOT_WINDOW (f) = data->root_window;
      /* Arrange *not* to restore point in the buffer that was
	 current when the window configuration was saved.  */
      if (EQ (XWINDOW (data->current_window)->buffer, new_current_buffer))
	set_marker_restricted (XWINDOW (data->current_window)->pointm,
			       make_number (old_point),
			       XWINDOW (data->current_window)->buffer);

      /* In the following call to `select-window', prevent "swapping out
	 point" in the old selected window using the buffer that has
	 been restored into it.  We already swapped out that point from
	 that window's old buffer.  */
      select_window (data->current_window, Qnil, 1);
      BVAR (XBUFFER (XWINDOW (selected_window)->buffer), last_selected_window)
	= selected_window;

      if (NILP (data->focus_frame)
	  || (FRAMEP (data->focus_frame)
	      && FRAME_LIVE_P (XFRAME (data->focus_frame))))
	Fredirect_frame_focus (frame, data->focus_frame);

      /* Set the screen height to the value it had before this function.  */
      if (previous_frame_lines != FRAME_LINES (f)
	  || previous_frame_cols != FRAME_COLS (f))
	change_frame_size (f, previous_frame_lines, previous_frame_cols,
			   0, 0, 0);
#if defined (HAVE_WINDOW_SYSTEM) || defined (MSDOS)
      if (previous_frame_menu_bar_lines != FRAME_MENU_BAR_LINES (f))
	x_set_menu_bar_lines (f, make_number (previous_frame_menu_bar_lines),
			      make_number (0));
#ifdef HAVE_WINDOW_SYSTEM
      if (previous_frame_tool_bar_lines != FRAME_TOOL_BAR_LINES (f))
	x_set_tool_bar_lines (f, make_number (previous_frame_tool_bar_lines),
			      make_number (0));
#endif
#endif

      /* Now, free glyph matrices in windows that were not reused.  */
      for (i = n = 0; i < n_leaf_windows; ++i)
	{
	  if (NILP (leaf_windows[i]->buffer))
	    {
	      /* Assert it's not reused as a combination.  */
	      xassert (NILP (leaf_windows[i]->hchild)
		       && NILP (leaf_windows[i]->vchild));
	      free_window_matrices (leaf_windows[i]);
	    }
	  else if (EQ (leaf_windows[i]->buffer, new_current_buffer))
	    ++n;
	}

      adjust_glyphs (f);
      UNBLOCK_INPUT;

      /* Scan dead buffer windows.  */
      for (; CONSP (dead_windows); dead_windows = XCDR (dead_windows))
	{
	  window = XCAR (dead_windows);
	  if (WINDOW_LIVE_P (window) && !EQ (window, FRAME_ROOT_WINDOW (f)))
	    delete_deletable_window (window);
	}

      /* Fselect_window will have made f the selected frame, so we
	 reselect the proper frame here.  Fhandle_switch_frame will change the
	 selected window too, but that doesn't make the call to
	 Fselect_window above totally superfluous; it still sets f's
	 selected window.  */
      if (FRAME_LIVE_P (XFRAME (data->selected_frame)))
	do_switch_frame (data->selected_frame, 0, 0, Qnil);

      run_window_configuration_change_hook (f);
    }

  if (!NILP (new_current_buffer))
    Fset_buffer (new_current_buffer);

  Vminibuf_scroll_window = data->minibuf_scroll_window;
  minibuf_selected_window = data->minibuf_selected_window;

  return (FRAME_LIVE_P (f) ? Qt : Qnil);
}


/* Recursively delete all child windows reachable via the next, vchild,
   and hchild slots of WINDOW.  */
void
delete_all_child_windows (Lisp_Object window)
{
  register struct window *w;

  w = XWINDOW (window);

  if (!NILP (w->next))
    /* Delete WINDOW's siblings (we traverse postorderly).  */
    delete_all_child_windows (w->next);

  w->total_lines = w->buffer;       /* See Fset_window_configuration for excuse.  */

  if (!NILP (w->vchild))
    {
      delete_all_child_windows (w->vchild);
      w->vchild = Qnil;
    }
  else if (!NILP (w->hchild))
    {
      delete_all_child_windows (w->hchild);
      w->hchild = Qnil;
    }
  else if (!NILP (w->buffer))
    {
      unshow_buffer (w);
      unchain_marker (XMARKER (w->pointm));
      unchain_marker (XMARKER (w->start));
      w->buffer = Qnil;
    }

  Vwindow_list = Qnil;
}

static int
count_windows (register struct window *window)
{
  register int count = 1;
  if (!NILP (window->next))
    count += count_windows (XWINDOW (window->next));
  if (!NILP (window->vchild))
    count += count_windows (XWINDOW (window->vchild));
  if (!NILP (window->hchild))
    count += count_windows (XWINDOW (window->hchild));
  return count;
}


/* Fill vector FLAT with leaf windows under W, starting at index I.
   Value is last index + 1.  */
static int
get_leaf_windows (struct window *w, struct window **flat, int i)
{
  while (w)
    {
      if (!NILP (w->hchild))
	i = get_leaf_windows (XWINDOW (w->hchild), flat, i);
      else if (!NILP (w->vchild))
	i = get_leaf_windows (XWINDOW (w->vchild), flat, i);
      else
	flat[i++] = w;

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }

  return i;
}


/* Return a pointer to the glyph W's physical cursor is on.  Value is
   null if W's current matrix is invalid, so that no meaningful glyph
   can be returned.  */
struct glyph *
get_phys_cursor_glyph (struct window *w)
{
  struct glyph_row *row;
  struct glyph *glyph;
  int hpos = w->phys_cursor.hpos;

  if (!(w->phys_cursor.vpos >= 0
	&& w->phys_cursor.vpos < w->current_matrix->nrows))
    return NULL;

  row = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
  if (!row->enabled_p)
    return NULL;

  if (XINT (w->hscroll))
    {
      /* When the window is hscrolled, cursor hpos can legitimately be
	 out of bounds, but we draw the cursor at the corresponding
	 window margin in that case.  */
      if (!row->reversed_p && hpos < 0)
	hpos = 0;
      if (row->reversed_p && hpos >= row->used[TEXT_AREA])
	hpos = row->used[TEXT_AREA] - 1;
    }

  if (row->used[TEXT_AREA] > hpos
      && 0 <= hpos)
    glyph = row->glyphs[TEXT_AREA] + hpos;
  else
    glyph = NULL;

  return glyph;
}


static int
save_window_save (Lisp_Object window, struct Lisp_Vector *vector, int i)
{
  register struct saved_window *p;
  register struct window *w;
  register Lisp_Object tem, pers, par;

  for (;!NILP (window); window = w->next)
    {
      p = SAVED_WINDOW_N (vector, i);
      w = XWINDOW (window);

      XSETFASTINT (w->temslot, i); i++;
      p->window = window;
      p->buffer = w->buffer;
      p->left_col = w->left_col;
      p->top_line = w->top_line;
      p->total_cols = w->total_cols;
      p->total_lines = w->total_lines;
      p->normal_cols = w->normal_cols;
      p->normal_lines = w->normal_lines;
      p->hscroll = w->hscroll;
      p->min_hscroll = w->min_hscroll;
      p->display_table = w->display_table;
      p->left_margin_cols = w->left_margin_cols;
      p->right_margin_cols = w->right_margin_cols;
      p->left_fringe_width = w->left_fringe_width;
      p->right_fringe_width = w->right_fringe_width;
      p->fringes_outside_margins = w->fringes_outside_margins;
      p->scroll_bar_width = w->scroll_bar_width;
      p->vertical_scroll_bar_type = w->vertical_scroll_bar_type;
      p->dedicated = w->dedicated;
      p->combination_limit = w->combination_limit;
      p->window_parameters = Qnil;

      if (!NILP (Vwindow_persistent_parameters))
	{
	  /* Run cycle detection on Vwindow_persistent_parameters.  */
	  Lisp_Object tortoise, hare;

	  hare = tortoise = Vwindow_persistent_parameters;
	  while (CONSP (hare))
	    {
	      hare = XCDR (hare);
	      if (!CONSP (hare))
		break;

	      hare = XCDR (hare);
	      tortoise = XCDR (tortoise);

	      if (EQ (hare, tortoise))
		/* Reset Vwindow_persistent_parameters to Qnil.  */
		{
		  Vwindow_persistent_parameters = Qnil;
		  break;
		}
	    }

	  for (tem = Vwindow_persistent_parameters; CONSP (tem);
	       tem = XCDR (tem))
	    {
	      pers = XCAR (tem);
	      /* Save values for persistent window parameters. */
	      if (CONSP (pers) && !NILP (XCDR (pers)))
		{
		  par = Fassq (XCAR (pers), w->window_parameters);
		  if (NILP (par))
		    /* If the window has no value for the parameter,
		       make one.  */
		    p->window_parameters = Fcons (Fcons (XCAR (pers), Qnil),
						  p->window_parameters);
		  else
		    /* If the window has a value for the parameter,
		       save it.  */
		    p->window_parameters = Fcons (Fcons (XCAR (par),
							 XCDR (par)),
						  p->window_parameters);
		}
	    }
	}

      if (!NILP (w->buffer))
	{
	  /* Save w's value of point in the window configuration.  If w
	     is the selected window, then get the value of point from
	     the buffer; pointm is garbage in the selected window.  */
	  if (EQ (window, selected_window))
	    {
	      p->pointm = Fmake_marker ();
	      set_marker_both (p->pointm, w->buffer,
			       BUF_PT (XBUFFER (w->buffer)),
			       BUF_PT_BYTE (XBUFFER (w->buffer)));
	    }
	  else
	    p->pointm = Fcopy_marker (w->pointm, Qnil);

	  p->start = Fcopy_marker (w->start, Qnil);
	  p->start_at_line_beg = w->start_at_line_beg;

	  tem = BVAR (XBUFFER (w->buffer), mark);
	  p->mark = Fcopy_marker (tem, Qnil);
	}
      else
	{
	  p->pointm = Qnil;
	  p->start = Qnil;
	  p->mark = Qnil;
	  p->start_at_line_beg = Qnil;
	}

      if (NILP (w->parent))
	p->parent = Qnil;
      else
	p->parent = XWINDOW (w->parent)->temslot;

      if (NILP (w->prev))
	p->prev = Qnil;
      else
	p->prev = XWINDOW (w->prev)->temslot;

      if (!NILP (w->vchild))
	i = save_window_save (w->vchild, vector, i);
      if (!NILP (w->hchild))
	i = save_window_save (w->hchild, vector, i);
    }

  return i;
}

DEFUN ("current-window-configuration", Fcurrent_window_configuration,
       Scurrent_window_configuration, 0, 1, 0,
       doc: /* Return an object representing the current window configuration of FRAME.
If FRAME is nil or omitted, use the selected frame.
This describes the number of windows, their sizes and current buffers,
and for each displayed buffer, where display starts, and the positions of
point and mark.  An exception is made for point in the current buffer:
its value is -not- saved.
This also records the currently selected frame, and FRAME's focus
redirection (see `redirect-frame-focus').  The variable
`window-persistent-parameters' specifies which window parameters are
saved by this function.  */)
  (Lisp_Object frame)
{
  register Lisp_Object tem;
  register int n_windows;
  register struct save_window_data *data;
  register int i;
  FRAME_PTR f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  n_windows = count_windows (XWINDOW (FRAME_ROOT_WINDOW (f)));
  data = ALLOCATE_PSEUDOVECTOR (struct save_window_data, frame_cols,
			       PVEC_WINDOW_CONFIGURATION);

  data->frame_cols = FRAME_COLS (f);
  data->frame_lines = FRAME_LINES (f);
  data->frame_menu_bar_lines = FRAME_MENU_BAR_LINES (f);
  data->frame_tool_bar_lines = FRAME_TOOL_BAR_LINES (f);
  data->selected_frame = selected_frame;
  data->current_window = FRAME_SELECTED_WINDOW (f);
  XSETBUFFER (data->current_buffer, current_buffer);
  data->minibuf_scroll_window = minibuf_level > 0 ? Vminibuf_scroll_window : Qnil;
  data->minibuf_selected_window = minibuf_level > 0 ? minibuf_selected_window : Qnil;
  data->root_window = FRAME_ROOT_WINDOW (f);
  data->focus_frame = FRAME_FOCUS_FRAME (f);
  tem = Fmake_vector (make_number (n_windows), Qnil);
  data->saved_windows = tem;
  for (i = 0; i < n_windows; i++)
    XVECTOR (tem)->contents[i]
      = Fmake_vector (make_number (VECSIZE (struct saved_window)), Qnil);
  save_window_save (FRAME_ROOT_WINDOW (f), XVECTOR (tem), 0);
  XSETWINDOW_CONFIGURATION (tem, data);
  return (tem);
}

/***********************************************************************
			    Marginal Areas
 ***********************************************************************/

DEFUN ("set-window-margins", Fset_window_margins, Sset_window_margins,
       2, 3, 0,
       doc: /* Set width of marginal areas of window WINDOW.
If WINDOW is nil, set margins of the currently selected window.
Second arg LEFT-WIDTH specifies the number of character cells to
reserve for the left marginal area.  Optional third arg RIGHT-WIDTH
does the same for the right marginal area.  A nil width parameter
means no margin.  */)
  (Lisp_Object window, Lisp_Object left_width, Lisp_Object right_width)
{
  struct window *w = decode_window (window);

  /* Translate negative or zero widths to nil.
     Margins that are too wide have to be checked elsewhere.  */

  if (!NILP (left_width))
    {
      CHECK_NUMBER (left_width);
      if (XINT (left_width) <= 0)
	left_width = Qnil;
    }

  if (!NILP (right_width))
    {
      CHECK_NUMBER (right_width);
      if (XINT (right_width) <= 0)
	right_width = Qnil;
    }

  if (!EQ (w->left_margin_cols, left_width)
      || !EQ (w->right_margin_cols, right_width))
    {
      w->left_margin_cols = left_width;
      w->right_margin_cols = right_width;

      adjust_window_margins (w);

      ++windows_or_buffers_changed;
      adjust_glyphs (XFRAME (WINDOW_FRAME (w)));
    }

  return Qnil;
}


DEFUN ("window-margins", Fwindow_margins, Swindow_margins,
       0, 1, 0,
       doc: /* Get width of marginal areas of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.
Value is a cons of the form (LEFT-WIDTH . RIGHT-WIDTH).
If a marginal area does not exist, its width will be returned
as nil.  */)
  (Lisp_Object window)
{
  struct window *w = decode_window (window);
  return Fcons (w->left_margin_cols, w->right_margin_cols);
}



/***********************************************************************
			    Fringes
 ***********************************************************************/

DEFUN ("set-window-fringes", Fset_window_fringes, Sset_window_fringes,
       2, 4, 0,
       doc: /* Set the fringe widths of window WINDOW.
If WINDOW is nil, set the fringe widths of the currently selected
window.
Second arg LEFT-WIDTH specifies the number of pixels to reserve for
the left fringe.  Optional third arg RIGHT-WIDTH specifies the right
fringe width.  If a fringe width arg is nil, that means to use the
frame's default fringe width.  Default fringe widths can be set with
the command `set-fringe-style'.
If optional fourth arg OUTSIDE-MARGINS is non-nil, draw the fringes
outside of the display margins.  By default, fringes are drawn between
display marginal areas and the text area.  */)
  (Lisp_Object window, Lisp_Object left_width, Lisp_Object right_width, Lisp_Object outside_margins)
{
  struct window *w = decode_window (window);

  if (!NILP (left_width))
    CHECK_NATNUM (left_width);
  if (!NILP (right_width))
    CHECK_NATNUM (right_width);

  /* Do nothing on a tty.  */
  if (FRAME_WINDOW_P (WINDOW_XFRAME (w))
      && (!EQ (w->left_fringe_width, left_width)
	  || !EQ (w->right_fringe_width, right_width)
	  || !EQ (w->fringes_outside_margins, outside_margins)))
    {
      w->left_fringe_width = left_width;
      w->right_fringe_width = right_width;
      w->fringes_outside_margins = outside_margins;

      adjust_window_margins (w);

      clear_glyph_matrix (w->current_matrix);
      w->window_end_valid = Qnil;

      ++windows_or_buffers_changed;
      adjust_glyphs (XFRAME (WINDOW_FRAME (w)));
    }

  return Qnil;
}


DEFUN ("window-fringes", Fwindow_fringes, Swindow_fringes,
       0, 1, 0,
       doc: /* Get width of fringes of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.
Value is a list of the form (LEFT-WIDTH RIGHT-WIDTH OUTSIDE-MARGINS).  */)
  (Lisp_Object window)
{
  struct window *w = decode_window (window);

  return Fcons (make_number (WINDOW_LEFT_FRINGE_WIDTH (w)),
		Fcons (make_number (WINDOW_RIGHT_FRINGE_WIDTH (w)),
		       Fcons ((WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
			       ? Qt : Qnil), Qnil)));
}



/***********************************************************************
			    Scroll bars
 ***********************************************************************/

DEFUN ("set-window-scroll-bars", Fset_window_scroll_bars,
       Sset_window_scroll_bars, 2, 4, 0,
       doc: /* Set width and type of scroll bars of window WINDOW.
If window is nil, set scroll bars of the currently selected window.
Second parameter WIDTH specifies the pixel width for the scroll bar;
this is automatically adjusted to a multiple of the frame column width.
Third parameter VERTICAL-TYPE specifies the type of the vertical scroll
bar: left, right, or nil.
If WIDTH is nil, use the frame's scroll-bar width.
If VERTICAL-TYPE is t, use the frame's scroll-bar type.
Fourth parameter HORIZONTAL-TYPE is currently unused.  */)
  (Lisp_Object window, Lisp_Object width, Lisp_Object vertical_type, Lisp_Object horizontal_type)
{
  struct window *w = decode_window (window);

  if (!NILP (width))
    {
      CHECK_NATNUM (width);

      if (XINT (width) == 0)
	vertical_type = Qnil;
    }

  if (!(NILP (vertical_type)
	|| EQ (vertical_type, Qleft)
	|| EQ (vertical_type, Qright)
	|| EQ (vertical_type, Qt)))
    error ("Invalid type of vertical scroll bar");

  if (!EQ (w->scroll_bar_width, width)
      || !EQ (w->vertical_scroll_bar_type, vertical_type))
    {
      w->scroll_bar_width = width;
      w->vertical_scroll_bar_type = vertical_type;

      adjust_window_margins (w);

      clear_glyph_matrix (w->current_matrix);
      w->window_end_valid = Qnil;

      ++windows_or_buffers_changed;
      adjust_glyphs (XFRAME (WINDOW_FRAME (w)));
    }

  return Qnil;
}


DEFUN ("window-scroll-bars", Fwindow_scroll_bars, Swindow_scroll_bars,
       0, 1, 0,
       doc: /* Get width and type of scroll bars of window WINDOW.
If WINDOW is omitted or nil, it defaults to the selected window.
Value is a list of the form (WIDTH COLS VERTICAL-TYPE HORIZONTAL-TYPE).
If WIDTH is nil or TYPE is t, the window is using the frame's corresponding
value.  */)
  (Lisp_Object window)
{
  struct window *w = decode_window (window);
  return Fcons (make_number ((WINDOW_CONFIG_SCROLL_BAR_WIDTH (w)
			      ? WINDOW_CONFIG_SCROLL_BAR_WIDTH (w)
			      : WINDOW_SCROLL_BAR_AREA_WIDTH (w))),
		Fcons (make_number (WINDOW_SCROLL_BAR_COLS (w)),
		       Fcons (w->vertical_scroll_bar_type,
			      Fcons (Qnil, Qnil))));
}



/***********************************************************************
			   Smooth scrolling
 ***********************************************************************/

DEFUN ("window-vscroll", Fwindow_vscroll, Swindow_vscroll, 0, 2, 0,
       doc: /* Return the amount by which WINDOW is scrolled vertically.
If WINDOW is omitted or nil, it defaults to the selected window.
Normally, value is a multiple of the canonical character height of WINDOW;
optional second arg PIXELS-P means value is measured in pixels.  */)
  (Lisp_Object window, Lisp_Object pixels_p)
{
  Lisp_Object result;
  struct frame *f;
  struct window *w;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window);
  w = XWINDOW (window);
  f = XFRAME (w->frame);

  if (FRAME_WINDOW_P (f))
    result = (NILP (pixels_p)
	      ? FRAME_CANON_Y_FROM_PIXEL_Y (f, -w->vscroll)
	      : make_number (-w->vscroll));
  else
    result = make_number (0);
  return result;
}


DEFUN ("set-window-vscroll", Fset_window_vscroll, Sset_window_vscroll,
       2, 3, 0,
       doc: /* Set amount by which WINDOW should be scrolled vertically to VSCROLL.
WINDOW nil means use the selected window.  Normally, VSCROLL is a
non-negative multiple of the canonical character height of WINDOW;
optional third arg PIXELS-P non-nil means that VSCROLL is in pixels.
If PIXELS-P is nil, VSCROLL may have to be rounded so that it
corresponds to an integral number of pixels.  The return value is the
result of this rounding.
If PIXELS-P is non-nil, the return value is VSCROLL.  */)
  (Lisp_Object window, Lisp_Object vscroll, Lisp_Object pixels_p)
{
  struct window *w;
  struct frame *f;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window);
  CHECK_NUMBER_OR_FLOAT (vscroll);

  w = XWINDOW (window);
  f = XFRAME (w->frame);

  if (FRAME_WINDOW_P (f))
    {
      int old_dy = w->vscroll;

      w->vscroll = - (NILP (pixels_p)
		      ? FRAME_LINE_HEIGHT (f) * XFLOATINT (vscroll)
		      : XFLOATINT (vscroll));
      w->vscroll = min (w->vscroll, 0);

      if (w->vscroll != old_dy)
	{
	  /* Adjust glyph matrix of the frame if the virtual display
	     area becomes larger than before.  */
	  if (w->vscroll < 0 && w->vscroll < old_dy)
	    adjust_glyphs (f);

	  /* Prevent redisplay shortcuts.  */
	  XBUFFER (w->buffer)->prevent_redisplay_optimizations_p = 1;
	}
    }

  return Fwindow_vscroll (window, pixels_p);
}


/* Call FN for all leaf windows on frame F.  FN is called with the
   first argument being a pointer to the leaf window, and with
   additional argument USER_DATA.  Stops when FN returns 0.  */

static void
foreach_window (struct frame *f, int (*fn) (struct window *, void *),
		void *user_data)
{
  /* delete_frame may set FRAME_ROOT_WINDOW (f) to Qnil.  */
  if (WINDOWP (FRAME_ROOT_WINDOW (f)))
    foreach_window_1 (XWINDOW (FRAME_ROOT_WINDOW (f)), fn, user_data);
}


/* Helper function for foreach_window.  Call FN for all leaf windows
   reachable from W.  FN is called with the first argument being a
   pointer to the leaf window, and with additional argument USER_DATA.
   Stop when FN returns 0.  Value is 0 if stopped by FN.  */

static int
foreach_window_1 (struct window *w, int (*fn) (struct window *, void *), void *user_data)
{
  int cont;

  for (cont = 1; w && cont;)
    {
      if (!NILP (w->hchild))
 	cont = foreach_window_1 (XWINDOW (w->hchild), fn, user_data);
      else if (!NILP (w->vchild))
 	cont = foreach_window_1 (XWINDOW (w->vchild), fn, user_data);
      else
	cont = fn (w, user_data);

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }

  return cont;
}


/* Freeze or unfreeze the window start of W unless it is a
   mini-window or the selected window.  FREEZE_P non-null means freeze
   the window start.  */

static int
freeze_window_start (struct window *w, void *freeze_p)
{
  if (MINI_WINDOW_P (w)
      || (WINDOWP (selected_window) /* Can be nil in corner cases.  */
         && (w == XWINDOW (selected_window)
             || (MINI_WINDOW_P (XWINDOW (selected_window))
                 && ! NILP (Vminibuf_scroll_window)
                 && w == XWINDOW (Vminibuf_scroll_window)))))
    freeze_p = NULL;

  w->frozen_window_start_p = freeze_p != NULL;
  return 1;
}


/* Freeze or unfreeze the window starts of all leaf windows on frame
   F, except the selected window and a mini-window.  FREEZE_P non-zero
   means freeze the window start.  */

void
freeze_window_starts (struct frame *f, int freeze_p)
{
  foreach_window (f, freeze_window_start, (void *) (freeze_p ? f : 0));
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

/* Return 1 if window configurations CONFIGURATION1 and CONFIGURATION2
   describe the same state of affairs.  This is used by Fequal.

   ignore_positions non-zero means ignore non-matching scroll positions
   and the like.

   This ignores a couple of things like the dedication status of
   window, combination_limit and the like.  This might have to be
   fixed.  */

int
compare_window_configurations (Lisp_Object configuration1, Lisp_Object configuration2, int ignore_positions)
{
  register struct save_window_data *d1, *d2;
  struct Lisp_Vector *sws1, *sws2;
  ptrdiff_t i;

  CHECK_WINDOW_CONFIGURATION (configuration1);
  CHECK_WINDOW_CONFIGURATION (configuration2);

  d1 = (struct save_window_data *) XVECTOR (configuration1);
  d2 = (struct save_window_data *) XVECTOR (configuration2);
  sws1 = XVECTOR (d1->saved_windows);
  sws2 = XVECTOR (d2->saved_windows);

  /* Frame settings must match.  */
  if (d1->frame_cols != d2->frame_cols
      || d1->frame_lines != d2->frame_lines
      || d1->frame_menu_bar_lines != d2->frame_menu_bar_lines
      || !EQ (d1->selected_frame, d2->selected_frame)
      || !EQ (d1->current_buffer, d2->current_buffer)
      || (!ignore_positions
	  && (!EQ (d1->minibuf_scroll_window, d2->minibuf_scroll_window)
	      || !EQ (d1->minibuf_selected_window, d2->minibuf_selected_window)))
      || !EQ (d1->focus_frame, d2->focus_frame)
      /* Verify that the two configurations have the same number of windows.  */
      || sws1->header.size != sws2->header.size)
    return 0;

  for (i = 0; i < sws1->header.size; i++)
    {
      struct saved_window *sw1, *sw2;

      sw1 = SAVED_WINDOW_N (sws1, i);
      sw2 = SAVED_WINDOW_N (sws2, i);

      if (
	   /* The "current" windows in the two configurations must
	      correspond to each other.  */
	  EQ (d1->current_window, sw1->window)
	  != EQ (d2->current_window, sw2->window)
	  /* Windows' buffers must match.  */
	  || !EQ (sw1->buffer, sw2->buffer)
	  || !EQ (sw1->left_col, sw2->left_col)
	  || !EQ (sw1->top_line, sw2->top_line)
	  || !EQ (sw1->total_cols, sw2->total_cols)
	  || !EQ (sw1->total_lines, sw2->total_lines)
	  || !EQ (sw1->display_table, sw2->display_table)
	  /* The next two disjuncts check the window structure for
	     equality.  */
	  || !EQ (sw1->parent, sw2->parent)
	  || !EQ (sw1->prev, sw2->prev)
	  || (!ignore_positions
	      && (!EQ (sw1->hscroll, sw2->hscroll)
		  || !EQ (sw1->min_hscroll, sw2->min_hscroll)
		  || !EQ (sw1->start_at_line_beg, sw2->start_at_line_beg)
		  || NILP (Fequal (sw1->start, sw2->start))
		  || NILP (Fequal (sw1->pointm, sw2->pointm))
		  || NILP (Fequal (sw1->mark, sw2->mark))))
	  || !EQ (sw1->left_margin_cols, sw2->left_margin_cols)
	  || !EQ (sw1->right_margin_cols, sw2->right_margin_cols)
	  || !EQ (sw1->left_fringe_width, sw2->left_fringe_width)
	  || !EQ (sw1->right_fringe_width, sw2->right_fringe_width)
	  || !EQ (sw1->fringes_outside_margins, sw2->fringes_outside_margins)
	  || !EQ (sw1->scroll_bar_width, sw2->scroll_bar_width)
	  || !EQ (sw1->vertical_scroll_bar_type, sw2->vertical_scroll_bar_type))
	return 0;
    }

  return 1;
}

DEFUN ("compare-window-configurations", Fcompare_window_configurations,
       Scompare_window_configurations, 2, 2, 0,
       doc: /* Compare two window configurations as regards the structure of windows.
This function ignores details such as the values of point and mark
and scrolling positions.  */)
  (Lisp_Object x, Lisp_Object y)
{
  if (compare_window_configurations (x, y, 1))
    return Qt;
  return Qnil;
}

void
init_window_once (void)
{
  struct frame *f = make_initial_frame ();
  XSETFRAME (selected_frame, f);
  Vterminal_frame = selected_frame;
  minibuf_window = f->minibuffer_window;
  selected_window = f->selected_window;
  last_nonminibuf_frame = f;

  window_initialized = 1;
}

void
init_window (void)
{
  Vwindow_list = Qnil;
}

void
syms_of_window (void)
{
  DEFSYM (Qscroll_up, "scroll-up");
  DEFSYM (Qscroll_down, "scroll-down");
  DEFSYM (Qscroll_command, "scroll-command");

  Fput (Qscroll_up, Qscroll_command, Qt);
  Fput (Qscroll_down, Qscroll_command, Qt);

  DEFSYM (Qwindow_configuration_change_hook, "window-configuration-change-hook");
  DEFSYM (Qwindowp, "windowp");
  DEFSYM (Qwindow_configuration_p, "window-configuration-p");
  DEFSYM (Qwindow_live_p, "window-live-p");
  DEFSYM (Qwindow_deletable_p, "window-deletable-p");
  DEFSYM (Qdelete_window, "delete-window");
  DEFSYM (Qwindow_resize_root_window, "window--resize-root-window");
  DEFSYM (Qwindow_resize_root_window_vertically, "window--resize-root-window-vertically");
  DEFSYM (Qsafe, "safe");
  DEFSYM (Qdisplay_buffer, "display-buffer");
  DEFSYM (Qreplace_buffer_in_windows, "replace-buffer-in-windows");
  DEFSYM (Qrecord_window_buffer, "record-window-buffer");
  DEFSYM (Qget_mru_window, "get-mru-window");
  DEFSYM (Qtemp_buffer_show_hook, "temp-buffer-show-hook");
  DEFSYM (Qabove, "above");
  DEFSYM (Qbelow, "below");
  DEFSYM (Qauto_buffer_name, "auto-buffer-name");
  DEFSYM (Qclone_of, "clone-of");

  staticpro (&Vwindow_list);

  minibuf_selected_window = Qnil;
  staticpro (&minibuf_selected_window);

  window_scroll_pixel_based_preserve_x = -1;
  window_scroll_pixel_based_preserve_y = -1;
  window_scroll_preserve_hpos = -1;
  window_scroll_preserve_vpos = -1;

  DEFVAR_LISP ("temp-buffer-show-function", Vtemp_buffer_show_function,
	       doc: /* Non-nil means call as function to display a help buffer.
The function is called with one argument, the buffer to be displayed.
Used by `with-output-to-temp-buffer'.
If this function is used, then it must do the entire job of showing
the buffer; `temp-buffer-show-hook' is not run unless this function runs it.  */);
  Vtemp_buffer_show_function = Qnil;

  DEFVAR_LISP ("minibuffer-scroll-window", Vminibuf_scroll_window,
	       doc: /* Non-nil means it is the window that C-M-v in minibuffer should scroll.  */);
  Vminibuf_scroll_window = Qnil;

  DEFVAR_BOOL ("mode-line-in-non-selected-windows", mode_line_in_non_selected_windows,
	       doc: /* Non-nil means to use `mode-line-inactive' face in non-selected windows.
If the minibuffer is active, the `minibuffer-scroll-window' mode line
is displayed in the `mode-line' face.  */);
  mode_line_in_non_selected_windows = 1;

  DEFVAR_LISP ("other-window-scroll-buffer", Vother_window_scroll_buffer,
	       doc: /* If non-nil, this is a buffer and \\[scroll-other-window] should scroll its window.  */);
  Vother_window_scroll_buffer = Qnil;

  DEFVAR_BOOL ("auto-window-vscroll", auto_window_vscroll_p,
	       doc: /* Non-nil means to automatically adjust `window-vscroll' to view tall lines.  */);
  auto_window_vscroll_p = 1;

  DEFVAR_INT ("next-screen-context-lines", next_screen_context_lines,
	      doc: /* Number of lines of continuity when scrolling by screenfuls.  */);
  next_screen_context_lines = 2;

  DEFVAR_LISP ("scroll-preserve-screen-position",
	       Vscroll_preserve_screen_position,
	       doc: /* Controls if scroll commands move point to keep its screen position unchanged.
A value of nil means point does not keep its screen position except
at the scroll margin or window boundary respectively.
A value of t means point keeps its screen position if the scroll
command moved it vertically out of the window, e.g. when scrolling
by full screens.
Any other value means point always keeps its screen position.
Scroll commands should have the `scroll-command' property
on their symbols to be controlled by this variable.  */);
  Vscroll_preserve_screen_position = Qnil;

  DEFVAR_LISP ("window-point-insertion-type", Vwindow_point_insertion_type,
	       doc: /* Type of marker to use for `window-point'.  */);
  Vwindow_point_insertion_type = Qnil;

  DEFVAR_LISP ("window-configuration-change-hook",
	       Vwindow_configuration_change_hook,
	       doc: /* Functions to call when window configuration changes.
The buffer-local part is run once per window, with the relevant window
selected; while the global part is run only once for the modified frame,
with the relevant frame selected.  */);
  Vwindow_configuration_change_hook = Qnil;

  DEFVAR_LISP ("recenter-redisplay", Vrecenter_redisplay,
	       doc: /* Non-nil means `recenter' redraws entire frame.
If this option is non-nil, then the `recenter' command with a nil
argument will redraw the entire frame; the special value `tty' causes
the frame to be redrawn only if it is a tty frame.  */);
  Vrecenter_redisplay = Qtty;

  DEFVAR_LISP ("window-combination-resize", Vwindow_combination_resize,
	       doc: /* If t, resize window combinations proportionally.
If this variable is nil, splitting a window gets the entire screen space
for displaying the new window from the window to split.  Deleting and
resizing a window preferably resizes one adjacent window only.

If this variable is t, splitting a window tries to get the space
proportionally from all windows in the same combination.  This also
allows to split a window that is otherwise too small or of fixed size.
Resizing and deleting a window proportionally resize all windows in the
same combination.

Other values are reserved for future use.

This variable takes no effect if `window-combination-limit' is non-nil.  */);
  Vwindow_combination_resize = Qnil;

  DEFVAR_LISP ("window-combination-limit", Vwindow_combination_limit,
	       doc: /* If t, splitting a window makes a new parent window.
If this variable is nil, splitting a window will create a new parent
window only if the window has no parent window or the window shall
become a combination orthogonal to the one it is part of.

If this variable is t, splitting a window always creates a new parent
window.  If all splits behave this way, each frame's window tree is a
binary tree and every window but the frame's root window has exactly one
sibling.

Other values are reserved for future use.

The value of this variable is also assigned to the combination limit of
the new parent window.  The combination limit of a window can be
retrieved via the function `window-combination-limit' and altered by the
function `set-window-combination-limit'.  */);
  Vwindow_combination_limit = Qnil;

  DEFVAR_LISP ("window-persistent-parameters", Vwindow_persistent_parameters,
	       doc: /* Alist of persistent window parameters.
This alist specifies which window parameters shall get saved by
`current-window-configuration' and `window-state-get' and subsequently
restored to their previous values by `set-window-configuration' and
`window-state-put'.

The car of each entry of this alist is the symbol specifying the
parameter.  The cdr is one of the following:

nil means the parameter is neither saved by `window-state-get' nor by
`current-window-configuration'.

t means the parameter is saved by `current-window-configuration' and,
provided its WRITABLE argument is nil, by `window-state-get'.

The symbol `writable' means the parameter is saved unconditionally by
both `current-window-configuration' and `window-state-get'.  Do not use
this value for parameters without read syntax (like windows or frames).

Parameters not saved by `current-window-configuration' or
`window-state-get' are left alone by `set-window-configuration'
respectively are not installed by `window-state-put'.  */);
  Vwindow_persistent_parameters = list1 (Fcons (Qclone_of, Qt));

  defsubr (&Sselected_window);
  defsubr (&Sminibuffer_window);
  defsubr (&Swindow_minibuffer_p);
  defsubr (&Swindowp);
  defsubr (&Swindow_live_p);
  defsubr (&Swindow_frame);
  defsubr (&Sframe_root_window);
  defsubr (&Sframe_first_window);
  defsubr (&Sframe_selected_window);
  defsubr (&Sset_frame_selected_window);
  defsubr (&Spos_visible_in_window_p);
  defsubr (&Swindow_line_height);
  defsubr (&Swindow_buffer);
  defsubr (&Swindow_parent);
  defsubr (&Swindow_top_child);
  defsubr (&Swindow_left_child);
  defsubr (&Swindow_next_sibling);
  defsubr (&Swindow_prev_sibling);
  defsubr (&Swindow_combination_limit);
  defsubr (&Sset_window_combination_limit);
  defsubr (&Swindow_use_time);
  defsubr (&Swindow_top_line);
  defsubr (&Swindow_left_column);
  defsubr (&Swindow_total_height);
  defsubr (&Swindow_total_width);
  defsubr (&Swindow_normal_size);
  defsubr (&Swindow_new_total);
  defsubr (&Swindow_new_normal);
  defsubr (&Sset_window_new_total);
  defsubr (&Sset_window_new_normal);
  defsubr (&Swindow_resize_apply);
  defsubr (&Swindow_body_height);
  defsubr (&Swindow_body_width);
  defsubr (&Swindow_hscroll);
  defsubr (&Sset_window_hscroll);
  defsubr (&Swindow_redisplay_end_trigger);
  defsubr (&Sset_window_redisplay_end_trigger);
  defsubr (&Swindow_edges);
  defsubr (&Swindow_pixel_edges);
  defsubr (&Swindow_absolute_pixel_edges);
  defsubr (&Swindow_inside_edges);
  defsubr (&Swindow_inside_pixel_edges);
  defsubr (&Swindow_inside_absolute_pixel_edges);
  defsubr (&Scoordinates_in_window_p);
  defsubr (&Swindow_at);
  defsubr (&Swindow_point);
  defsubr (&Swindow_start);
  defsubr (&Swindow_end);
  defsubr (&Sset_window_point);
  defsubr (&Sset_window_start);
  defsubr (&Swindow_dedicated_p);
  defsubr (&Sset_window_dedicated_p);
  defsubr (&Swindow_display_table);
  defsubr (&Sset_window_display_table);
  defsubr (&Snext_window);
  defsubr (&Sprevious_window);
  defsubr (&Sget_buffer_window);
  defsubr (&Sdelete_other_windows_internal);
  defsubr (&Sdelete_window_internal);
  defsubr (&Sresize_mini_window_internal);
  defsubr (&Sset_window_buffer);
  defsubr (&Srun_window_configuration_change_hook);
  defsubr (&Sselect_window);
  defsubr (&Sforce_window_update);
  defsubr (&Stemp_output_buffer_show);
  defsubr (&Ssplit_window_internal);
  defsubr (&Sscroll_up);
  defsubr (&Sscroll_down);
  defsubr (&Sscroll_left);
  defsubr (&Sscroll_right);
  defsubr (&Sother_window_for_scrolling);
  defsubr (&Sscroll_other_window);
  defsubr (&Sminibuffer_selected_window);
  defsubr (&Srecenter);
  defsubr (&Swindow_text_height);
  defsubr (&Smove_to_window_line);
  defsubr (&Swindow_configuration_p);
  defsubr (&Swindow_configuration_frame);
  defsubr (&Sset_window_configuration);
  defsubr (&Scurrent_window_configuration);
  defsubr (&Sset_window_margins);
  defsubr (&Swindow_margins);
  defsubr (&Sset_window_fringes);
  defsubr (&Swindow_fringes);
  defsubr (&Sset_window_scroll_bars);
  defsubr (&Swindow_scroll_bars);
  defsubr (&Swindow_vscroll);
  defsubr (&Sset_window_vscroll);
  defsubr (&Scompare_window_configurations);
  defsubr (&Swindow_list);
  defsubr (&Swindow_list_1);
  defsubr (&Swindow_prev_buffers);
  defsubr (&Sset_window_prev_buffers);
  defsubr (&Swindow_next_buffers);
  defsubr (&Sset_window_next_buffers);
  defsubr (&Swindow_parameters);
  defsubr (&Swindow_parameter);
  defsubr (&Sset_window_parameter);
}

void
keys_of_window (void)
{
  initial_define_key (control_x_map, '<', "scroll-left");
  initial_define_key (control_x_map, '>', "scroll-right");

  initial_define_key (global_map, Ctl ('V'), "scroll-up-command");
  initial_define_key (meta_map, Ctl ('V'), "scroll-other-window");
  initial_define_key (meta_map, 'v', "scroll-down-command");
}
