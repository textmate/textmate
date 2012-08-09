/* Generic frame functions.

Copyright (C) 1993-1995, 1997, 1999-2012  Free Software Foundation, Inc.

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
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <setjmp.h>
#include "lisp.h"
#include "character.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif
#ifdef WINDOWSNT
#include "w32term.h"
#endif
#ifdef HAVE_NS
#include "nsterm.h"
#endif
#include "buffer.h"
/* These help us bind and responding to switch-frame events.  */
#include "commands.h"
#include "keyboard.h"
#include "frame.h"
#include "blockinput.h"
#include "termchar.h"
#include "termhooks.h"
#include "dispextern.h"
#include "window.h"
#include "font.h"
#ifdef HAVE_WINDOW_SYSTEM
#include "fontset.h"
#endif
#ifdef MSDOS
#include "msdos.h"
#include "dosfns.h"
#endif


#ifdef HAVE_WINDOW_SYSTEM

#endif

#ifdef HAVE_NS
Lisp_Object Qns_parse_geometry;
#endif

Lisp_Object Qframep, Qframe_live_p;
Lisp_Object Qicon, Qmodeline;
Lisp_Object Qonly;
Lisp_Object Qx, Qw32, Qmac, Qpc, Qns;
Lisp_Object Qvisible;
Lisp_Object Qdisplay_type;
static Lisp_Object Qbackground_mode;
Lisp_Object Qnoelisp;

static Lisp_Object Qx_frame_parameter;
Lisp_Object Qx_resource_name;
Lisp_Object Qterminal;
Lisp_Object Qterminal_live_p;

/* Frame parameters (set or reported).  */

Lisp_Object Qauto_raise, Qauto_lower;
Lisp_Object Qborder_color, Qborder_width;
Lisp_Object Qcursor_color, Qcursor_type;
static Lisp_Object Qgeometry;  /* Not used */
Lisp_Object Qheight, Qwidth;
Lisp_Object Qleft, Qright;
Lisp_Object Qicon_left, Qicon_top, Qicon_type, Qicon_name;
Lisp_Object Qtooltip;
Lisp_Object Qinternal_border_width;
Lisp_Object Qmouse_color;
Lisp_Object Qminibuffer;
Lisp_Object Qscroll_bar_width, Qvertical_scroll_bars;
Lisp_Object Qvisibility;
Lisp_Object Qscroll_bar_foreground, Qscroll_bar_background;
Lisp_Object Qscreen_gamma;
Lisp_Object Qline_spacing;
static Lisp_Object Quser_position, Quser_size;
Lisp_Object Qwait_for_wm;
static Lisp_Object Qwindow_id;
#ifdef HAVE_X_WINDOWS
static Lisp_Object Qouter_window_id;
#endif
Lisp_Object Qparent_id;
Lisp_Object Qtitle, Qname;
static Lisp_Object Qexplicit_name;
Lisp_Object Qunsplittable;
Lisp_Object Qmenu_bar_lines, Qtool_bar_lines, Qtool_bar_position;
Lisp_Object Qleft_fringe, Qright_fringe;
Lisp_Object Qbuffer_predicate;
static Lisp_Object Qbuffer_list, Qburied_buffer_list;
Lisp_Object Qtty_color_mode;
Lisp_Object Qtty, Qtty_type;

Lisp_Object Qfullscreen, Qfullwidth, Qfullheight, Qfullboth, Qmaximized;
Lisp_Object Qsticky;
Lisp_Object Qfont_backend;
Lisp_Object Qalpha;

Lisp_Object Qface_set_after_frame_default;

static Lisp_Object Qdelete_frame_functions;

#ifdef HAVE_WINDOW_SYSTEM
static void x_report_frame_params (struct frame *, Lisp_Object *);
#endif


static void
set_menu_bar_lines_1 (Lisp_Object window, int n)
{
  struct window *w = XWINDOW (window);

  XSETFASTINT (w->last_modified, 0);
  XSETFASTINT (w->top_line, XFASTINT (w->top_line) + n);
  XSETFASTINT (w->total_lines, XFASTINT (w->total_lines) - n);

  /* Handle just the top child in a vertical split.  */
  if (!NILP (w->vchild))
    set_menu_bar_lines_1 (w->vchild, n);

  /* Adjust all children in a horizontal split.  */
  for (window = w->hchild; !NILP (window); window = w->next)
    {
      w = XWINDOW (window);
      set_menu_bar_lines_1 (window, n);
    }
}

void
set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;
  int olines = FRAME_MENU_BAR_LINES (f);

  /* Right now, menu bars don't work properly in minibuf-only frames;
     most of the commands try to apply themselves to the minibuffer
     frame itself, and get an error because you can't switch buffers
     in or split the minibuffer window.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (TYPE_RANGED_INTEGERP (int, value))
    nlines = XINT (value);
  else
    nlines = 0;

  if (nlines != olines)
    {
      windows_or_buffers_changed++;
      FRAME_WINDOW_SIZES_CHANGED (f) = 1;
      FRAME_MENU_BAR_LINES (f) = nlines;
      set_menu_bar_lines_1 (f->root_window, nlines - olines);
      adjust_glyphs (f);
    }
}

Lisp_Object Vframe_list;


DEFUN ("framep", Fframep, Sframep, 1, 1, 0,
       doc: /* Return non-nil if OBJECT is a frame.
Value is:
  t for a termcap frame (a character-only terminal),
 'x' for an Emacs frame that is really an X window,
 'w32' for an Emacs frame that is a window on MS-Windows display,
 'ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
 'pc' for a direct-write MS-DOS frame.
See also `frame-live-p'.  */)
  (Lisp_Object object)
{
  if (!FRAMEP (object))
    return Qnil;
  switch (XFRAME (object)->output_method)
    {
    case output_initial: /* The initial frame is like a termcap frame. */
    case output_termcap:
      return Qt;
    case output_x_window:
      return Qx;
    case output_w32:
      return Qw32;
    case output_msdos_raw:
      return Qpc;
    case output_mac:
      return Qmac;
    case output_ns:
      return Qns;
    default:
      abort ();
    }
}

DEFUN ("frame-live-p", Fframe_live_p, Sframe_live_p, 1, 1, 0,
       doc: /* Return non-nil if OBJECT is a frame which has not been deleted.
Value is nil if OBJECT is not a live frame.  If object is a live
frame, the return value indicates what sort of terminal device it is
displayed on.  See the documentation of `framep' for possible
return values.  */)
  (Lisp_Object object)
{
  return ((FRAMEP (object)
	   && FRAME_LIVE_P (XFRAME (object)))
	  ? Fframep (object)
	  : Qnil);
}

DEFUN ("window-system", Fwindow_system, Swindow_system, 0, 1, 0,
       doc: /* The name of the window system that FRAME is displaying through.
The value is a symbol:
 nil for a termcap frame (a character-only terminal),
 'x' for an Emacs frame that is really an X window,
 'w32' for an Emacs frame that is a window on MS-Windows display,
 'ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
 'pc' for a direct-write MS-DOS frame.

FRAME defaults to the currently selected frame.

Use of this function as a predicate is deprecated.  Instead,
use `display-graphic-p' or any of the other `display-*-p'
predicates which report frame's specific UI-related capabilities.  */)
  (Lisp_Object frame)
{
  Lisp_Object type;
  if (NILP (frame))
    frame = selected_frame;

  type = Fframep (frame);

  if (NILP (type))
    wrong_type_argument (Qframep, frame);

  if (EQ (type, Qt))
    return Qnil;
  else
    return type;
}

struct frame *
make_frame (int mini_p)
{
  Lisp_Object frame;
  register struct frame *f;
  register Lisp_Object root_window;
  register Lisp_Object mini_window;

  f = allocate_frame ();
  XSETFRAME (frame, f);

  f->desired_matrix = 0;
  f->current_matrix = 0;
  f->desired_pool = 0;
  f->current_pool = 0;
  f->glyphs_initialized_p = 0;
  f->decode_mode_spec_buffer = 0;
  f->visible = 0;
  f->async_visible = 0;
  f->output_data.nothing = 0;
  f->iconified = 0;
  f->async_iconified = 0;
  f->wants_modeline = 1;
  f->auto_raise = 0;
  f->auto_lower = 0;
  f->no_split = 0;
  f->garbaged = 1;
  f->has_minibuffer = mini_p;
  f->focus_frame = Qnil;
  f->explicit_name = 0;
  f->can_have_scroll_bars = 0;
  f->vertical_scroll_bar_type = vertical_scroll_bar_none;
  f->param_alist = Qnil;
  f->scroll_bars = Qnil;
  f->condemned_scroll_bars = Qnil;
  f->face_alist = Qnil;
  f->face_cache = NULL;
  f->menu_bar_items = Qnil;
  f->menu_bar_vector = Qnil;
  f->menu_bar_items_used = 0;
  f->buffer_predicate = Qnil;
  f->buffer_list = Qnil;
  f->buried_buffer_list = Qnil;
  f->namebuf = 0;
  f->title = Qnil;
  f->menu_bar_window = Qnil;
  f->tool_bar_window = Qnil;
  f->tool_bar_items = Qnil;
  f->tool_bar_position = Qtop;
  f->desired_tool_bar_string = f->current_tool_bar_string = Qnil;
  f->n_tool_bar_items = 0;
  f->left_fringe_width = f->right_fringe_width = 0;
  f->fringe_cols = 0;
  f->menu_bar_lines = 0;
  f->tool_bar_lines = 0;
  f->scroll_bar_actual_width = 0;
  f->border_width = 0;
  f->internal_border_width = 0;
  f->column_width = 1;  /* !FRAME_WINDOW_P value */
  f->line_height = 1;  /* !FRAME_WINDOW_P value */
  f->x_pixels_diff = f->y_pixels_diff = 0;
#ifdef HAVE_WINDOW_SYSTEM
  f->want_fullscreen = FULLSCREEN_NONE;
#endif
  f->size_hint_flags = 0;
  f->win_gravity = 0;
  f->font_driver_list = NULL;
  f->font_data_list = NULL;

  root_window = make_window ();
  if (mini_p)
    {
      mini_window = make_window ();
      XWINDOW (root_window)->next = mini_window;
      XWINDOW (mini_window)->prev = root_window;
      XWINDOW (mini_window)->mini_p = Qt;
      XWINDOW (mini_window)->frame = frame;
      f->minibuffer_window = mini_window;
    }
  else
    {
      mini_window = Qnil;
      XWINDOW (root_window)->next = Qnil;
      f->minibuffer_window = Qnil;
    }

  XWINDOW (root_window)->frame = frame;

  /* 10 is arbitrary,
     just so that there is "something there."
     Correct size will be set up later with change_frame_size.  */

  SET_FRAME_COLS (f, 10);
  FRAME_LINES (f) = 10;

  XSETFASTINT (XWINDOW (root_window)->total_cols, 10);
  XSETFASTINT (XWINDOW (root_window)->total_lines, (mini_p ? 9 : 10));

  if (mini_p)
    {
      XSETFASTINT (XWINDOW (mini_window)->total_cols, 10);
      XSETFASTINT (XWINDOW (mini_window)->top_line, 9);
      XSETFASTINT (XWINDOW (mini_window)->total_lines, 1);
    }

  /* Choose a buffer for the frame's root window.  */
  {
    Lisp_Object buf;

    XWINDOW (root_window)->buffer = Qt;
    buf = Fcurrent_buffer ();
    /* If buf is a 'hidden' buffer (i.e. one whose name starts with
       a space), try to find another one.  */
    if (SREF (Fbuffer_name (buf), 0) == ' ')
      buf = other_buffer_safely (buf);

    /* Use set_window_buffer, not Fset_window_buffer, and don't let
       hooks be run by it.  The reason is that the whole frame/window
       arrangement is not yet fully initialized at this point.  Windows
       don't have the right size, glyph matrices aren't initialized
       etc.  Running Lisp functions at this point surely ends in a
       SEGV.  */
    set_window_buffer (root_window, buf, 0, 0);
    f->buffer_list = Fcons (buf, Qnil);
  }

  if (mini_p)
    {
      XWINDOW (mini_window)->buffer = Qt;
      set_window_buffer (mini_window,
			 (NILP (Vminibuffer_list)
			  ? get_minibuffer (0)
			  : Fcar (Vminibuffer_list)),
			 0, 0);
    }

  f->root_window = root_window;
  f->selected_window = root_window;
  /* Make sure this window seems more recently used than
     a newly-created, never-selected window.  */
  ++window_select_count;
  XSETFASTINT (XWINDOW (f->selected_window)->use_time, window_select_count);

  f->default_face_done_p = 0;

  return f;
}

#ifdef HAVE_WINDOW_SYSTEM
/* Make a frame using a separate minibuffer window on another frame.
   MINI_WINDOW is the minibuffer window to use.  nil means use the
   default (the global minibuffer).  */

struct frame *
make_frame_without_minibuffer (register Lisp_Object mini_window, KBOARD *kb, Lisp_Object display)
{
  register struct frame *f;
  struct gcpro gcpro1;

  if (!NILP (mini_window))
    CHECK_LIVE_WINDOW (mini_window);

  if (!NILP (mini_window)
      && FRAME_KBOARD (XFRAME (XWINDOW (mini_window)->frame)) != kb)
    error ("Frame and minibuffer must be on the same terminal");

  /* Make a frame containing just a root window.  */
  f = make_frame (0);

  if (NILP (mini_window))
    {
      /* Use default-minibuffer-frame if possible.  */
      if (!FRAMEP (KVAR (kb, Vdefault_minibuffer_frame))
	  || ! FRAME_LIVE_P (XFRAME (KVAR (kb, Vdefault_minibuffer_frame))))
	{
          Lisp_Object frame_dummy;

          XSETFRAME (frame_dummy, f);
          GCPRO1 (frame_dummy);
	  /* If there's no minibuffer frame to use, create one.  */
	  KVAR (kb, Vdefault_minibuffer_frame) =
	    call1 (intern ("make-initial-minibuffer-frame"), display);
          UNGCPRO;
	}

      mini_window = XFRAME (KVAR (kb, Vdefault_minibuffer_frame))->minibuffer_window;
    }

  f->minibuffer_window = mini_window;

  /* Make the chosen minibuffer window display the proper minibuffer,
     unless it is already showing a minibuffer.  */
  if (NILP (Fmemq (XWINDOW (mini_window)->buffer, Vminibuffer_list)))
    Fset_window_buffer (mini_window,
			(NILP (Vminibuffer_list)
			 ? get_minibuffer (0)
			 : Fcar (Vminibuffer_list)), Qnil);
  return f;
}

/* Make a frame containing only a minibuffer window.  */

struct frame *
make_minibuffer_frame (void)
{
  /* First make a frame containing just a root window, no minibuffer.  */

  register struct frame *f = make_frame (0);
  register Lisp_Object mini_window;
  register Lisp_Object frame;

  XSETFRAME (frame, f);

  f->auto_raise = 0;
  f->auto_lower = 0;
  f->no_split = 1;
  f->wants_modeline = 0;
  f->has_minibuffer = 1;

  /* Now label the root window as also being the minibuffer.
     Avoid infinite looping on the window chain by marking next pointer
     as nil. */

  mini_window = f->minibuffer_window = f->root_window;
  XWINDOW (mini_window)->mini_p = Qt;
  XWINDOW (mini_window)->next = Qnil;
  XWINDOW (mini_window)->prev = Qnil;
  XWINDOW (mini_window)->frame = frame;

  /* Put the proper buffer in that window.  */

  Fset_window_buffer (mini_window,
		      (NILP (Vminibuffer_list)
		       ? get_minibuffer (0)
		       : Fcar (Vminibuffer_list)), Qnil);
  return f;
}
#endif /* HAVE_WINDOW_SYSTEM */

/* Construct a frame that refers to a terminal.  */

static printmax_t tty_frame_count;

struct frame *
make_initial_frame (void)
{
  struct frame *f;
  struct terminal *terminal;
  Lisp_Object frame;

  eassert (initial_kboard);

  /* The first call must initialize Vframe_list.  */
  if (! (NILP (Vframe_list) || CONSP (Vframe_list)))
    Vframe_list = Qnil;

  terminal = init_initial_terminal ();

  f = make_frame (1);
  XSETFRAME (frame, f);

  Vframe_list = Fcons (frame, Vframe_list);

  tty_frame_count = 1;
  f->name = make_pure_c_string ("F1");

  f->visible = 1;
  f->async_visible = 1;

  f->output_method = terminal->type;
  f->terminal = terminal;
  f->terminal->reference_count++;
  f->output_data.nothing = 0;

  FRAME_FOREGROUND_PIXEL (f) = FACE_TTY_DEFAULT_FG_COLOR;
  FRAME_BACKGROUND_PIXEL (f) = FACE_TTY_DEFAULT_BG_COLOR;

  FRAME_CAN_HAVE_SCROLL_BARS (f) = 0;
  FRAME_VERTICAL_SCROLL_BAR_TYPE (f) = vertical_scroll_bar_none;

  /* The default value of menu-bar-mode is t.  */
  set_menu_bar_lines (f, make_number (1), Qnil);

  if (!noninteractive)
    init_frame_faces (f);

  return f;
}


static struct frame *
make_terminal_frame (struct terminal *terminal)
{
  register struct frame *f;
  Lisp_Object frame;
  char name[sizeof "F" + INT_STRLEN_BOUND (printmax_t)];

  if (!terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  f = make_frame (1);

  XSETFRAME (frame, f);
  Vframe_list = Fcons (frame, Vframe_list);

  tty_frame_count++;
  sprintf (name, "F%"pMd, tty_frame_count);
  f->name = build_string (name);

  f->visible = 1;		/* FRAME_SET_VISIBLE wd set frame_garbaged. */
  f->async_visible = 1;		/* Don't let visible be cleared later. */
  f->terminal = terminal;
  f->terminal->reference_count++;
#ifdef MSDOS
  f->output_data.tty->display_info = &the_only_display_info;
  if (!inhibit_window_system
      && (!FRAMEP (selected_frame) || !FRAME_LIVE_P (XFRAME (selected_frame))
	  || XFRAME (selected_frame)->output_method == output_msdos_raw))
    f->output_method = output_msdos_raw;
  else
    f->output_method = output_termcap;
#else /* not MSDOS */
  f->output_method = output_termcap;
  create_tty_output (f);
  FRAME_FOREGROUND_PIXEL (f) = FACE_TTY_DEFAULT_FG_COLOR;
  FRAME_BACKGROUND_PIXEL (f) = FACE_TTY_DEFAULT_BG_COLOR;
#endif /* not MSDOS */

  FRAME_CAN_HAVE_SCROLL_BARS (f) = 0;
  FRAME_VERTICAL_SCROLL_BAR_TYPE (f) = vertical_scroll_bar_none;
  FRAME_MENU_BAR_LINES(f) = NILP (Vmenu_bar_mode) ? 0 : 1;

  /* Set the top frame to the newly created frame. */
  if (FRAMEP (FRAME_TTY (f)->top_frame)
      && FRAME_LIVE_P (XFRAME (FRAME_TTY (f)->top_frame)))
    XFRAME (FRAME_TTY (f)->top_frame)->async_visible = 2; /* obscured */

  FRAME_TTY (f)->top_frame = frame;

  if (!noninteractive)
    init_frame_faces (f);

  return f;
}

/* Get a suitable value for frame parameter PARAMETER for a newly
   created frame, based on (1) the user-supplied frame parameter
   alist SUPPLIED_PARMS, and (2) CURRENT_VALUE.  */

static Lisp_Object
get_future_frame_param (Lisp_Object parameter,
                        Lisp_Object supplied_parms,
                        char *current_value)
{
  Lisp_Object result;

  result = Fassq (parameter, supplied_parms);
  if (NILP (result))
    result = Fassq (parameter, XFRAME (selected_frame)->param_alist);
  if (NILP (result) && current_value != NULL)
    result = build_string (current_value);
  if (!NILP (result) && !STRINGP (result))
    result = XCDR (result);
  if (NILP (result) || !STRINGP (result))
    result = Qnil;

  return result;
}

DEFUN ("make-terminal-frame", Fmake_terminal_frame, Smake_terminal_frame,
       1, 1, 0,
       doc: /* Create an additional terminal frame, possibly on another terminal.
This function takes one argument, an alist specifying frame parameters.

You can create multiple frames on a single text-only terminal, but
only one of them (the selected terminal frame) is actually displayed.

In practice, generally you don't need to specify any parameters,
except when you want to create a new frame on another terminal.
In that case, the `tty' parameter specifies the device file to open,
and the `tty-type' parameter specifies the terminal type.  Example:

   (make-terminal-frame '((tty . "/dev/pts/5") (tty-type . "xterm")))

Note that changing the size of one terminal frame automatically
affects all frames on the same terminal device.  */)
  (Lisp_Object parms)
{
  struct frame *f;
  struct terminal *t = NULL;
  Lisp_Object frame, tem;
  struct frame *sf = SELECTED_FRAME ();

#ifdef MSDOS
  if (sf->output_method != output_msdos_raw
      && sf->output_method != output_termcap)
    abort ();
#else /* not MSDOS */

#ifdef WINDOWSNT                           /* This should work now! */
  if (sf->output_method != output_termcap)
    error ("Not using an ASCII terminal now; cannot make a new ASCII frame");
#endif
#endif /* not MSDOS */

  {
    Lisp_Object terminal;

    terminal = Fassq (Qterminal, parms);
    if (!NILP (terminal))
      {
        terminal = XCDR (terminal);
        t = get_terminal (terminal, 1);
      }
#ifdef MSDOS
    if (t && t != the_only_display_info.terminal)
      /* msdos.c assumes a single tty_display_info object.  */
      error ("Multiple terminals are not supported on this platform");
    if (!t)
      t = the_only_display_info.terminal;
#endif
  }

  if (!t)
    {
      char *name = 0, *type = 0;
      Lisp_Object tty, tty_type;

      tty = get_future_frame_param
        (Qtty, parms, (FRAME_TERMCAP_P (XFRAME (selected_frame))
                       ? FRAME_TTY (XFRAME (selected_frame))->name
                       : NULL));
      if (!NILP (tty))
        {
          name = (char *) alloca (SBYTES (tty) + 1);
          strncpy (name, SSDATA (tty), SBYTES (tty));
          name[SBYTES (tty)] = 0;
        }

      tty_type = get_future_frame_param
        (Qtty_type, parms, (FRAME_TERMCAP_P (XFRAME (selected_frame))
                            ? FRAME_TTY (XFRAME (selected_frame))->type
                            : NULL));
      if (!NILP (tty_type))
        {
          type = (char *) alloca (SBYTES (tty_type) + 1);
          strncpy (type, SSDATA (tty_type), SBYTES (tty_type));
          type[SBYTES (tty_type)] = 0;
        }

      t = init_tty (name, type, 0); /* Errors are not fatal. */
    }

  f = make_terminal_frame (t);

  {
    int width, height;
    get_tty_size (fileno (FRAME_TTY (f)->input), &width, &height);
    change_frame_size (f, height, width, 0, 0, 0);
  }

  adjust_glyphs (f);
  calculate_costs (f);
  XSETFRAME (frame, f);
  Fmodify_frame_parameters (frame, parms);
  Fmodify_frame_parameters (frame, Fcons (Fcons (Qtty_type,
                                                 build_string (t->display_info.tty->type)),
                                          Qnil));
  if (t->display_info.tty->name != NULL)
    Fmodify_frame_parameters (frame, Fcons (Fcons (Qtty,
                                                   build_string (t->display_info.tty->name)),
                                            Qnil));
  else
    Fmodify_frame_parameters (frame, Fcons (Fcons (Qtty, Qnil), Qnil));

  /* Make the frame face alist be frame-specific, so that each
     frame could change its face definitions independently.  */
  f->face_alist = Fcopy_alist (sf->face_alist);
  /* Simple Fcopy_alist isn't enough, because we need the contents of
     the vectors which are the CDRs of associations in face_alist to
     be copied as well.  */
  for (tem = f->face_alist; CONSP (tem); tem = XCDR (tem))
    XSETCDR (XCAR (tem), Fcopy_sequence (XCDR (XCAR (tem))));
  return frame;
}


/* Perform the switch to frame FRAME.

   If FRAME is a switch-frame event `(switch-frame FRAME1)', use
   FRAME1 as frame.

   If TRACK is non-zero and the frame that currently has the focus
   redirects its focus to the selected frame, redirect that focused
   frame's focus to FRAME instead.

   FOR_DELETION non-zero means that the selected frame is being
   deleted, which includes the possibility that the frame's terminal
   is dead.

   The value of NORECORD is passed as argument to Fselect_window.  */

Lisp_Object
do_switch_frame (Lisp_Object frame, int track, int for_deletion, Lisp_Object norecord)
{
  struct frame *sf = SELECTED_FRAME ();

  /* If FRAME is a switch-frame event, extract the frame we should
     switch to.  */
  if (CONSP (frame)
      && EQ (XCAR (frame), Qswitch_frame)
      && CONSP (XCDR (frame)))
    frame = XCAR (XCDR (frame));

  /* This used to say CHECK_LIVE_FRAME, but apparently it's possible for
     a switch-frame event to arrive after a frame is no longer live,
     especially when deleting the initial frame during startup.  */
  CHECK_FRAME (frame);
  if (! FRAME_LIVE_P (XFRAME (frame)))
    return Qnil;

  if (sf == XFRAME (frame))
    return frame;

  /* This is too greedy; it causes inappropriate focus redirection
     that's hard to get rid of.  */
#if 0
  /* If a frame's focus has been redirected toward the currently
     selected frame, we should change the redirection to point to the
     newly selected frame.  This means that if the focus is redirected
     from a minibufferless frame to a surrogate minibuffer frame, we
     can use `other-window' to switch between all the frames using
     that minibuffer frame, and the focus redirection will follow us
     around.  */
  if (track)
    {
      Lisp_Object tail;

      for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object focus;

	  if (!FRAMEP (XCAR (tail)))
	    abort ();

	  focus = FRAME_FOCUS_FRAME (XFRAME (XCAR (tail)));

	  if (FRAMEP (focus) && XFRAME (focus) == SELECTED_FRAME ())
	    Fredirect_frame_focus (XCAR (tail), frame);
	}
    }
#else /* ! 0 */
  /* Instead, apply it only to the frame we're pointing to.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (track && FRAME_WINDOW_P (XFRAME (frame)))
    {
      Lisp_Object focus, xfocus;

      xfocus = x_get_focus_frame (XFRAME (frame));
      if (FRAMEP (xfocus))
	{
	  focus = FRAME_FOCUS_FRAME (XFRAME (xfocus));
	  if (FRAMEP (focus) && XFRAME (focus) == SELECTED_FRAME ())
	    Fredirect_frame_focus (xfocus, frame);
	}
    }
#endif /* HAVE_X_WINDOWS */
#endif /* ! 0 */

  if (!for_deletion && FRAME_HAS_MINIBUF_P (sf))
    resize_mini_window (XWINDOW (FRAME_MINIBUF_WINDOW (sf)), 1);

  if (FRAME_TERMCAP_P (XFRAME (frame)) || FRAME_MSDOS_P (XFRAME (frame)))
    {
      if (FRAMEP (FRAME_TTY (XFRAME (frame))->top_frame))
	/* Mark previously displayed frame as now obscured.  */
	XFRAME (FRAME_TTY (XFRAME (frame))->top_frame)->async_visible = 2;
      XFRAME (frame)->async_visible = 1;
      FRAME_TTY (XFRAME (frame))->top_frame = frame;
    }

  selected_frame = frame;
  if (! FRAME_MINIBUF_ONLY_P (XFRAME (selected_frame)))
    last_nonminibuf_frame = XFRAME (selected_frame);

  Fselect_window (XFRAME (frame)->selected_window, norecord);

  /* We want to make sure that the next event generates a frame-switch
     event to the appropriate frame.  This seems kludgy to me, but
     before you take it out, make sure that evaluating something like
     (select-window (frame-root-window (new-frame))) doesn't end up
     with your typing being interpreted in the new frame instead of
     the one you're actually typing in.  */
  internal_last_event_frame = Qnil;

  return frame;
}

DEFUN ("select-frame", Fselect_frame, Sselect_frame, 1, 2, "e",
       doc: /* Select FRAME.
Subsequent editing commands apply to its selected window.
Optional argument NORECORD means to neither change the order of
recently selected windows nor the buffer list.

The selection of FRAME lasts until the next time the user does
something to select a different frame, or until the next time
this function is called.  If you are using a window system, the
previously selected frame may be restored as the selected frame
when returning to the command loop, because it still may have
the window system's input focus.  On a text-only terminal, the
next redisplay will display FRAME.

This function returns FRAME, or nil if FRAME has been deleted.  */)
  (Lisp_Object frame, Lisp_Object norecord)
{
  return do_switch_frame (frame, 1, 0, norecord);
}


DEFUN ("handle-switch-frame", Fhandle_switch_frame, Shandle_switch_frame, 1, 1, "e",
       doc: /* Handle a switch-frame event EVENT.
Switch-frame events are usually bound to this function.
A switch-frame event tells Emacs that the window manager has requested
that the user's events be directed to the frame mentioned in the event.
This function selects the selected window of the frame of EVENT.

If EVENT is frame object, handle it as if it were a switch-frame event
to that frame.  */)
  (Lisp_Object event)
{
  /* Preserve prefix arg that the command loop just cleared.  */
  KVAR (current_kboard, Vprefix_arg) = Vcurrent_prefix_arg;
  Frun_hooks (1, &Qmouse_leave_buffer_hook);
  return do_switch_frame (event, 0, 0, Qnil);
}

DEFUN ("selected-frame", Fselected_frame, Sselected_frame, 0, 0, 0,
       doc: /* Return the frame that is now selected.  */)
  (void)
{
  return selected_frame;
}

DEFUN ("frame-list", Fframe_list, Sframe_list,
       0, 0, 0,
       doc: /* Return a list of all live frames.  */)
  (void)
{
  Lisp_Object frames;
  frames = Fcopy_sequence (Vframe_list);
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAMEP (tip_frame))
    frames = Fdelq (tip_frame, frames);
#endif
  return frames;
}

/* Return the next frame in the frame list after FRAME.
   If MINIBUF is nil, exclude minibuffer-only frames.
   If MINIBUF is a window, include only its own frame
   and any frame now using that window as the minibuffer.
   If MINIBUF is `visible', include all visible frames.
   If MINIBUF is 0, include all visible and iconified frames.
   Otherwise, include all frames.  */

static Lisp_Object
next_frame (Lisp_Object frame, Lisp_Object minibuf)
{
  Lisp_Object tail;
  int passed = 0;

  /* There must always be at least one frame in Vframe_list.  */
  if (! CONSP (Vframe_list))
    abort ();

  /* If this frame is dead, it won't be in Vframe_list, and we'll loop
     forever.  Forestall that.  */
  CHECK_LIVE_FRAME (frame);

  while (1)
    for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
      {
	Lisp_Object f;

	f = XCAR (tail);

	if (passed
	    && ((!FRAME_TERMCAP_P (XFRAME (f)) && !FRAME_TERMCAP_P (XFRAME (frame))
                 && FRAME_KBOARD (XFRAME (f)) == FRAME_KBOARD (XFRAME (frame)))
                || (FRAME_TERMCAP_P (XFRAME (f)) && FRAME_TERMCAP_P (XFRAME (frame))
                    && FRAME_TTY (XFRAME (f)) == FRAME_TTY (XFRAME (frame)))))
	  {
	    /* Decide whether this frame is eligible to be returned.  */

	    /* If we've looped all the way around without finding any
	       eligible frames, return the original frame.  */
	    if (EQ (f, frame))
	      return f;

	    /* Let minibuf decide if this frame is acceptable.  */
	    if (NILP (minibuf))
	      {
		if (! FRAME_MINIBUF_ONLY_P (XFRAME (f)))
		  return f;
	      }
	    else if (EQ (minibuf, Qvisible))
	      {
		FRAME_SAMPLE_VISIBILITY (XFRAME (f));
		if (FRAME_VISIBLE_P (XFRAME (f)))
		  return f;
	      }
	    else if (INTEGERP (minibuf) && XINT (minibuf) == 0)
	      {
		FRAME_SAMPLE_VISIBILITY (XFRAME (f));
		if (FRAME_VISIBLE_P (XFRAME (f))
		    || FRAME_ICONIFIED_P (XFRAME (f)))
		  return f;
	      }
	    else if (WINDOWP (minibuf))
	      {
		if (EQ (FRAME_MINIBUF_WINDOW (XFRAME (f)), minibuf)
		    || EQ (WINDOW_FRAME (XWINDOW (minibuf)), f)
		    || EQ (WINDOW_FRAME (XWINDOW (minibuf)),
			   FRAME_FOCUS_FRAME (XFRAME (f))))
		  return f;
	      }
	    else
	      return f;
	  }

	if (EQ (frame, f))
	  passed++;
      }
}

/* Return the previous frame in the frame list before FRAME.
   If MINIBUF is nil, exclude minibuffer-only frames.
   If MINIBUF is a window, include only its own frame
   and any frame now using that window as the minibuffer.
   If MINIBUF is `visible', include all visible frames.
   If MINIBUF is 0, include all visible and iconified frames.
   Otherwise, include all frames.  */

static Lisp_Object
prev_frame (Lisp_Object frame, Lisp_Object minibuf)
{
  Lisp_Object tail;
  Lisp_Object prev;

  /* There must always be at least one frame in Vframe_list.  */
  if (! CONSP (Vframe_list))
    abort ();

  prev = Qnil;
  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object f;

      f = XCAR (tail);
      if (!FRAMEP (f))
	abort ();

      if (EQ (frame, f) && !NILP (prev))
	return prev;

      if ((!FRAME_TERMCAP_P (XFRAME (f)) && !FRAME_TERMCAP_P (XFRAME (frame))
           && FRAME_KBOARD (XFRAME (f)) == FRAME_KBOARD (XFRAME (frame)))
          || (FRAME_TERMCAP_P (XFRAME (f)) && FRAME_TERMCAP_P (XFRAME (frame))
              && FRAME_TTY (XFRAME (f)) == FRAME_TTY (XFRAME (frame))))
	{
	  /* Decide whether this frame is eligible to be returned,
	     according to minibuf.  */
	  if (NILP (minibuf))
	    {
	      if (! FRAME_MINIBUF_ONLY_P (XFRAME (f)))
		prev = f;
	    }
	  else if (WINDOWP (minibuf))
	    {
	      if (EQ (FRAME_MINIBUF_WINDOW (XFRAME (f)), minibuf)
		  || EQ (WINDOW_FRAME (XWINDOW (minibuf)), f)
		  || EQ (WINDOW_FRAME (XWINDOW (minibuf)),
			 FRAME_FOCUS_FRAME (XFRAME (f))))
		prev = f;
	    }
	  else if (EQ (minibuf, Qvisible))
	    {
	      FRAME_SAMPLE_VISIBILITY (XFRAME (f));
	      if (FRAME_VISIBLE_P (XFRAME (f)))
		prev = f;
	    }
	  else if (XFASTINT (minibuf) == 0)
	    {
	      FRAME_SAMPLE_VISIBILITY (XFRAME (f));
	      if (FRAME_VISIBLE_P (XFRAME (f))
		  || FRAME_ICONIFIED_P (XFRAME (f)))
		prev = f;
	    }
	  else
	    prev = f;
	}
    }

  /* We've scanned the entire list.  */
  if (NILP (prev))
    /* We went through the whole frame list without finding a single
       acceptable frame.  Return the original frame.  */
    return frame;
  else
    /* There were no acceptable frames in the list before FRAME; otherwise,
       we would have returned directly from the loop.  Since PREV is the last
       acceptable frame in the list, return it.  */
    return prev;
}


DEFUN ("next-frame", Fnext_frame, Snext_frame, 0, 2, 0,
       doc: /* Return the next frame in the frame list after FRAME.
It considers only frames on the same terminal as FRAME.
By default, skip minibuffer-only frames.
If omitted, FRAME defaults to the selected frame.
If optional argument MINIFRAME is nil, exclude minibuffer-only frames.
If MINIFRAME is a window, include only its own frame
and any frame now using that window as the minibuffer.
If MINIFRAME is `visible', include all visible frames.
If MINIFRAME is 0, include all visible and iconified frames.
Otherwise, include all frames.  */)
  (Lisp_Object frame, Lisp_Object miniframe)
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);
  return next_frame (frame, miniframe);
}

DEFUN ("previous-frame", Fprevious_frame, Sprevious_frame, 0, 2, 0,
       doc: /* Return the previous frame in the frame list before FRAME.
It considers only frames on the same terminal as FRAME.
By default, skip minibuffer-only frames.
If omitted, FRAME defaults to the selected frame.
If optional argument MINIFRAME is nil, exclude minibuffer-only frames.
If MINIFRAME is a window, include only its own frame
and any frame now using that window as the minibuffer.
If MINIFRAME is `visible', include all visible frames.
If MINIFRAME is 0, include all visible and iconified frames.
Otherwise, include all frames.  */)
  (Lisp_Object frame, Lisp_Object miniframe)
{
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  return prev_frame (frame, miniframe);
}

/* Return 1 if it is ok to delete frame F;
   0 if all frames aside from F are invisible.
   (Exception: if F is the terminal frame, and we are using X, return 1.)  */

static int
other_visible_frames (FRAME_PTR f)
{
  Lisp_Object frames;

  for (frames = Vframe_list; CONSP (frames); frames = XCDR (frames))
    {
      Lisp_Object this = XCAR (frames);
      if (f == XFRAME (this))
	continue;

      /* Verify that we can still talk to the frame's X window,
	 and note any recent change in visibility.  */
#ifdef HAVE_WINDOW_SYSTEM
      if (FRAME_WINDOW_P (XFRAME (this)))
	{
	  x_sync (XFRAME (this));
	  FRAME_SAMPLE_VISIBILITY (XFRAME (this));
	}
#endif

      if (FRAME_VISIBLE_P (XFRAME (this))
	  || FRAME_ICONIFIED_P (XFRAME (this))
	  /* Allow deleting the terminal frame when at least one X
	     frame exists.  */
	  || (FRAME_WINDOW_P (XFRAME (this)) && !FRAME_WINDOW_P (f)))
	return 1;
    }
  return 0;
}

/* Delete FRAME.  When FORCE equals Qnoelisp, delete FRAME
  unconditionally.  x_connection_closed and delete_terminal use
  this.  Any other value of FORCE implements the semantics
  described for Fdelete_frame.  */
Lisp_Object
delete_frame (Lisp_Object frame, Lisp_Object force)
     /* If we use `register' here, gcc-4.0.2 on amd64 using
	-DUSE_LISP_UNION_TYPE complains further down that we're getting the
	address of `force'.  Go figure.  */

{
  struct frame *f;
  struct frame *sf = SELECTED_FRAME ();
  struct kboard *kb;

  int minibuffer_selected, tooltip_frame;

  if (EQ (frame, Qnil))
    {
      f = sf;
      XSETFRAME (frame, f);
    }
  else
    {
      CHECK_FRAME (frame);
      f = XFRAME (frame);
    }

  if (! FRAME_LIVE_P (f))
    return Qnil;

  if (NILP (force) && !other_visible_frames (f))
    error ("Attempt to delete the sole visible or iconified frame");

  /* x_connection_closed must have set FORCE to `noelisp' in order
     to delete the last frame, if it is gone.  */
  if (NILP (XCDR (Vframe_list)) && !EQ (force, Qnoelisp))
    error ("Attempt to delete the only frame");

  /* Does this frame have a minibuffer, and is it the surrogate
     minibuffer for any other frame?  */
  if (FRAME_HAS_MINIBUF_P (XFRAME (frame)))
    {
      Lisp_Object frames;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCDR (frames))
	{
	  Lisp_Object this;
	  this = XCAR (frames);

	  if (! EQ (this, frame)
	      && EQ (frame,
		     WINDOW_FRAME (XWINDOW
				   (FRAME_MINIBUF_WINDOW (XFRAME (this))))))
	    {
	      /* If we MUST delete this frame, delete the other first.
		 But do this only if FORCE equals `noelisp'.  */
	      if (EQ (force, Qnoelisp))
		delete_frame (this, Qnoelisp);
	      else
		error ("Attempt to delete a surrogate minibuffer frame");
	    }
	}
    }

  tooltip_frame = !NILP (Fframe_parameter (frame, intern ("tooltip")));

  /* Run `delete-frame-functions' unless FORCE is `noelisp' or
     frame is a tooltip.  FORCE is set to `noelisp' when handling
     a disconnect from the terminal, so we don't dare call Lisp
     code.  */
  if (NILP (Vrun_hooks) || tooltip_frame)
    ;
  else if (EQ (force, Qnoelisp))
    pending_funcalls
      = Fcons (list3 (Qrun_hook_with_args, Qdelete_frame_functions, frame),
	       pending_funcalls);
  else
    {
#ifdef HAVE_X_WINDOWS
      /* Also, save clipboard to the clipboard manager.  */
      x_clipboard_manager_save_frame (frame);
#endif

      safe_call2 (Qrun_hook_with_args, Qdelete_frame_functions, frame);
    }

  /* The hook may sometimes (indirectly) cause the frame to be deleted.  */
  if (! FRAME_LIVE_P (f))
    return Qnil;

  /* At this point, we are committed to deleting the frame.
     There is no more chance for errors to prevent it.  */

  minibuffer_selected = EQ (minibuf_window, selected_window);

  /* Don't let the frame remain selected.  */
  if (f == sf)
    {
      Lisp_Object tail, frame1;

      /* Look for another visible frame on the same terminal.  */
      frame1 = next_frame (frame, Qvisible);

      /* If there is none, find *some* other frame.  */
      if (NILP (frame1) || EQ (frame1, frame))
	{
	  FOR_EACH_FRAME (tail, frame1)
	    {
	      if (! EQ (frame, frame1) && FRAME_LIVE_P (XFRAME (frame1)))
		break;
	    }
	}
#ifdef NS_IMPL_COCOA
      else
	/* Under NS, there is no system mechanism for choosing a new
	   window to get focus -- it is left to application code.
	   So the portion of THIS application interfacing with NS
	   needs to know about it.  We call Fraise_frame, but the
	   purpose is really to transfer focus.  */
	Fraise_frame (frame1);
#endif

      do_switch_frame (frame1, 0, 1, Qnil);
      sf = SELECTED_FRAME ();
    }

  /* Don't allow minibuf_window to remain on a deleted frame.  */
  if (EQ (f->minibuffer_window, minibuf_window))
    {
      Fset_window_buffer (sf->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer, Qnil);
      minibuf_window = sf->minibuffer_window;

      /* If the dying minibuffer window was selected,
	 select the new one.  */
      if (minibuffer_selected)
	Fselect_window (minibuf_window, Qnil);
    }

  /* Don't let echo_area_window to remain on a deleted frame.  */
  if (EQ (f->minibuffer_window, echo_area_window))
    echo_area_window = sf->minibuffer_window;

  /* Clear any X selections for this frame.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    x_clear_frame_selections (f);
#endif

  /* Free glyphs.
     This function must be called before the window tree of the
     frame is deleted because windows contain dynamically allocated
     memory. */
  free_glyphs (f);

#ifdef HAVE_WINDOW_SYSTEM
  /* Give chance to each font driver to free a frame specific data.  */
  font_update_drivers (f, Qnil);
#endif

  /* Mark all the windows that used to be on FRAME as deleted, and then
     remove the reference to them.  */
  delete_all_child_windows (f->root_window);
  f->root_window = Qnil;

  Vframe_list = Fdelq (frame, Vframe_list);
  FRAME_SET_VISIBLE (f, 0);

  /* Allow the vector of menu bar contents to be freed in the next
     garbage collection.  The frame object itself may not be garbage
     collected until much later, because recent_keys and other data
     structures can still refer to it.  */
  f->menu_bar_vector = Qnil;

  free_font_driver_list (f);
  xfree (f->namebuf);
  xfree (f->decode_mode_spec_buffer);
  xfree (FRAME_INSERT_COST (f));
  xfree (FRAME_DELETEN_COST (f));
  xfree (FRAME_INSERTN_COST (f));
  xfree (FRAME_DELETE_COST (f));
  xfree (FRAME_MESSAGE_BUF (f));

  /* Since some events are handled at the interrupt level, we may get
     an event for f at any time; if we zero out the frame's terminal
     now, then we may trip up the event-handling code.  Instead, we'll
     promise that the terminal of the frame must be valid until we
     have called the window-system-dependent frame destruction
     routine.  */

  if (FRAME_TERMINAL (f)->delete_frame_hook)
    (*FRAME_TERMINAL (f)->delete_frame_hook) (f);

  {
    struct terminal *terminal = FRAME_TERMINAL (f);
    f->output_data.nothing = 0;
    f->terminal = 0;             /* Now the frame is dead. */

    /* If needed, delete the terminal that this frame was on.
       (This must be done after the frame is killed.) */
    terminal->reference_count--;
#ifdef USE_GTK
    /* FIXME: Deleting the terminal crashes emacs because of a GTK
       bug.
       http://lists.gnu.org/archive/html/emacs-devel/2011-10/msg00363.html */
    if (terminal->reference_count == 0 && terminal->type == output_x_window)
      terminal->reference_count = 1;
#endif /* USE_GTK */
    if (terminal->reference_count == 0)
      {
	Lisp_Object tmp;
	XSETTERMINAL (tmp, terminal);

        kb = NULL;
	Fdelete_terminal (tmp, NILP (force) ? Qt : force);
      }
    else
      kb = terminal->kboard;
  }

  /* If we've deleted the last_nonminibuf_frame, then try to find
     another one.  */
  if (f == last_nonminibuf_frame)
    {
      Lisp_Object frames;

      last_nonminibuf_frame = 0;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCDR (frames))
	{
	  f = XFRAME (XCAR (frames));
	  if (!FRAME_MINIBUF_ONLY_P (f))
	    {
	      last_nonminibuf_frame = f;
	      break;
	    }
	}
    }

  /* If there's no other frame on the same kboard, get out of
     single-kboard state if we're in it for this kboard.  */
  if (kb != NULL)
    {
      Lisp_Object frames;
      /* Some frame we found on the same kboard, or nil if there are none.  */
      Lisp_Object frame_on_same_kboard;

      frame_on_same_kboard = Qnil;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCDR (frames))
	{
	  Lisp_Object this;
	  struct frame *f1;

	  this = XCAR (frames);
	  if (!FRAMEP (this))
	    abort ();
	  f1 = XFRAME (this);

	  if (kb == FRAME_KBOARD (f1))
	    frame_on_same_kboard = this;
	}

      if (NILP (frame_on_same_kboard))
	not_single_kboard_state (kb);
    }


  /* If we've deleted this keyboard's default_minibuffer_frame, try to
     find another one.  Prefer minibuffer-only frames, but also notice
     frames with other windows.  */
  if (kb != NULL && EQ (frame, KVAR (kb, Vdefault_minibuffer_frame)))
    {
      Lisp_Object frames;

      /* The last frame we saw with a minibuffer, minibuffer-only or not.  */
      Lisp_Object frame_with_minibuf;
      /* Some frame we found on the same kboard, or nil if there are none.  */
      Lisp_Object frame_on_same_kboard;

      frame_on_same_kboard = Qnil;
      frame_with_minibuf = Qnil;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCDR (frames))
	{
	  Lisp_Object this;
	  struct frame *f1;

	  this = XCAR (frames);
	  if (!FRAMEP (this))
	    abort ();
	  f1 = XFRAME (this);

	  /* Consider only frames on the same kboard
	     and only those with minibuffers.  */
	  if (kb == FRAME_KBOARD (f1)
	      && FRAME_HAS_MINIBUF_P (f1))
	    {
	      frame_with_minibuf = this;
	      if (FRAME_MINIBUF_ONLY_P (f1))
		break;
	    }

	  if (kb == FRAME_KBOARD (f1))
	    frame_on_same_kboard = this;
	}

      if (!NILP (frame_on_same_kboard))
	{
	  /* We know that there must be some frame with a minibuffer out
	     there.  If this were not true, all of the frames present
	     would have to be minibufferless, which implies that at some
	     point their minibuffer frames must have been deleted, but
	     that is prohibited at the top; you can't delete surrogate
	     minibuffer frames.  */
	  if (NILP (frame_with_minibuf))
	    abort ();

	  KVAR (kb, Vdefault_minibuffer_frame) = frame_with_minibuf;
	}
      else
	/* No frames left on this kboard--say no minibuffer either.  */
	KVAR (kb, Vdefault_minibuffer_frame) = Qnil;
    }

  /* Cause frame titles to update--necessary if we now have just one frame.  */
  if (!tooltip_frame)
    update_mode_lines = 1;

  return Qnil;
}

DEFUN ("delete-frame", Fdelete_frame, Sdelete_frame, 0, 2, "",
       doc: /* Delete FRAME, permanently eliminating it from use.
FRAME defaults to the selected frame.

A frame may not be deleted if its minibuffer is used by other frames.
Normally, you may not delete a frame if all other frames are invisible,
but if the second optional argument FORCE is non-nil, you may do so.

This function runs `delete-frame-functions' before actually
deleting the frame, unless the frame is a tooltip.
The functions are run with one argument, the frame to be deleted.  */)
  (Lisp_Object frame, Lisp_Object force)
{
  return delete_frame (frame, !NILP (force) ? Qt : Qnil);
}


/* Return mouse position in character cell units.  */

DEFUN ("mouse-position", Fmouse_position, Smouse_position, 0, 0, 0,
       doc: /* Return a list (FRAME X . Y) giving the current mouse frame and position.
The position is given in character cells, where (0, 0) is the
upper-left corner of the frame, X is the horizontal offset, and Y is
the vertical offset.
If Emacs is running on a mouseless terminal or hasn't been programmed
to read the mouse position, it returns the selected frame for FRAME
and nil for X and Y.
If `mouse-position-function' is non-nil, `mouse-position' calls it,
passing the normal return value to that function as an argument,
and returns whatever that function returns.  */)
  (void)
{
  FRAME_PTR f;
  Lisp_Object lispy_dummy;
  enum scroll_bar_part party_dummy;
  Lisp_Object x, y, retval;
  int col, row;
  Time long_dummy;
  struct gcpro gcpro1;

  f = SELECTED_FRAME ();
  x = y = Qnil;

#if defined (HAVE_MOUSE) || defined (HAVE_GPM)
  /* It's okay for the hook to refrain from storing anything.  */
  if (FRAME_TERMINAL (f)->mouse_position_hook)
    (*FRAME_TERMINAL (f)->mouse_position_hook) (&f, -1,
                                                &lispy_dummy, &party_dummy,
                                                &x, &y,
                                                &long_dummy);
  if (! NILP (x))
    {
      col = XINT (x);
      row = XINT (y);
      pixel_to_glyph_coords (f, col, row, &col, &row, NULL, 1);
      XSETINT (x, col);
      XSETINT (y, row);
    }
#endif
  XSETFRAME (lispy_dummy, f);
  retval = Fcons (lispy_dummy, Fcons (x, y));
  GCPRO1 (retval);
  if (!NILP (Vmouse_position_function))
    retval = call1 (Vmouse_position_function, retval);
  RETURN_UNGCPRO (retval);
}

DEFUN ("mouse-pixel-position", Fmouse_pixel_position,
       Smouse_pixel_position, 0, 0, 0,
       doc: /* Return a list (FRAME X . Y) giving the current mouse frame and position.
The position is given in pixel units, where (0, 0) is the
upper-left corner of the frame, X is the horizontal offset, and Y is
the vertical offset.
If Emacs is running on a mouseless terminal or hasn't been programmed
to read the mouse position, it returns the selected frame for FRAME
and nil for X and Y.  */)
  (void)
{
  FRAME_PTR f;
  Lisp_Object lispy_dummy;
  enum scroll_bar_part party_dummy;
  Lisp_Object x, y;
  Time long_dummy;

  f = SELECTED_FRAME ();
  x = y = Qnil;

#if defined (HAVE_MOUSE) || defined (HAVE_GPM)
  /* It's okay for the hook to refrain from storing anything.  */
  if (FRAME_TERMINAL (f)->mouse_position_hook)
    (*FRAME_TERMINAL (f)->mouse_position_hook) (&f, -1,
                                                &lispy_dummy, &party_dummy,
                                                &x, &y,
                                                &long_dummy);
#endif
  XSETFRAME (lispy_dummy, f);
  return Fcons (lispy_dummy, Fcons (x, y));
}

DEFUN ("set-mouse-position", Fset_mouse_position, Sset_mouse_position, 3, 3, 0,
       doc: /* Move the mouse pointer to the center of character cell (X,Y) in FRAME.
Coordinates are relative to the frame, not a window,
so the coordinates of the top left character in the frame
may be nonzero due to left-hand scroll bars or the menu bar.

The position is given in character cells, where (0, 0) is the
upper-left corner of the frame, X is the horizontal offset, and Y is
the vertical offset.

This function is a no-op for an X frame that is not visible.
If you have just created a frame, you must wait for it to become visible
before calling this function on it, like this.
  (while (not (frame-visible-p frame)) (sleep-for .5))  */)
  (Lisp_Object frame, Lisp_Object x, Lisp_Object y)
{
  CHECK_LIVE_FRAME (frame);
  CHECK_NUMBER (x);
  CHECK_NUMBER (y);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (frame)))
    /* Warping the mouse will cause enternotify and focus events.  */
    x_set_mouse_position (XFRAME (frame), XINT (x), XINT (y));
#else
#if defined (MSDOS) && defined (HAVE_MOUSE)
  if (FRAME_MSDOS_P (XFRAME (frame)))
    {
      Fselect_frame (frame, Qnil);
      mouse_moveto (XINT (x), XINT (y));
    }
#else
#ifdef HAVE_GPM
    {
      Fselect_frame (frame, Qnil);
      term_mouse_moveto (XINT (x), XINT (y));
    }
#endif
#endif
#endif

  return Qnil;
}

DEFUN ("set-mouse-pixel-position", Fset_mouse_pixel_position,
       Sset_mouse_pixel_position, 3, 3, 0,
       doc: /* Move the mouse pointer to pixel position (X,Y) in FRAME.
The position is given in pixels, where (0, 0) is the upper-left corner
of the frame, X is the horizontal offset, and Y is the vertical offset.

Note, this is a no-op for an X frame that is not visible.
If you have just created a frame, you must wait for it to become visible
before calling this function on it, like this.
  (while (not (frame-visible-p frame)) (sleep-for .5))  */)
  (Lisp_Object frame, Lisp_Object x, Lisp_Object y)
{
  CHECK_LIVE_FRAME (frame);
  CHECK_NUMBER (x);
  CHECK_NUMBER (y);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (frame)))
    /* Warping the mouse will cause enternotify and focus events.  */
    x_set_mouse_pixel_position (XFRAME (frame), XINT (x), XINT (y));
#else
#if defined (MSDOS) && defined (HAVE_MOUSE)
  if (FRAME_MSDOS_P (XFRAME (frame)))
    {
      Fselect_frame (frame, Qnil);
      mouse_moveto (XINT (x), XINT (y));
    }
#else
#ifdef HAVE_GPM
    {
      Fselect_frame (frame, Qnil);
      term_mouse_moveto (XINT (x), XINT (y));
    }
#endif
#endif
#endif

  return Qnil;
}

static void make_frame_visible_1 (Lisp_Object);

DEFUN ("make-frame-visible", Fmake_frame_visible, Smake_frame_visible,
       0, 1, "",
       doc: /* Make the frame FRAME visible (assuming it is an X window).
If omitted, FRAME defaults to the currently selected frame.  */)
  (Lisp_Object frame)
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (frame)))
    {
      FRAME_SAMPLE_VISIBILITY (XFRAME (frame));
      x_make_frame_visible (XFRAME (frame));
    }
#endif

  make_frame_visible_1 (XFRAME (frame)->root_window);

  /* Make menu bar update for the Buffers and Frames menus.  */
  windows_or_buffers_changed++;

  return frame;
}

/* Update the display_time slot of the buffers shown in WINDOW
   and all its descendants.  */

static void
make_frame_visible_1 (Lisp_Object window)
{
  struct window *w;

  for (;!NILP (window); window = w->next)
    {
      w = XWINDOW (window);

      if (!NILP (w->buffer))
	BVAR (XBUFFER (w->buffer), display_time) = Fcurrent_time ();

      if (!NILP (w->vchild))
	make_frame_visible_1 (w->vchild);
      if (!NILP (w->hchild))
	make_frame_visible_1 (w->hchild);
    }
}

DEFUN ("make-frame-invisible", Fmake_frame_invisible, Smake_frame_invisible,
       0, 2, "",
       doc: /* Make the frame FRAME invisible.
If omitted, FRAME defaults to the currently selected frame.
On graphical displays, invisible frames are not updated and are
usually not displayed at all, even in a window system's \"taskbar\".

Normally you may not make FRAME invisible if all other frames are invisible,
but if the second optional argument FORCE is non-nil, you may do so.

This function has no effect on text-only terminal frames.  Such frames
are always considered visible, whether or not they are currently being
displayed in the terminal.  */)
  (Lisp_Object frame, Lisp_Object force)
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

  if (NILP (force) && !other_visible_frames (XFRAME (frame)))
    error ("Attempt to make invisible the sole visible or iconified frame");

#if 0 /* This isn't logically necessary, and it can do GC.  */
  /* Don't let the frame remain selected.  */
  if (EQ (frame, selected_frame))
    do_switch_frame (next_frame (frame, Qt), 0, 0, Qnil)
#endif

  /* Don't allow minibuf_window to remain on a deleted frame.  */
  if (EQ (XFRAME (frame)->minibuffer_window, minibuf_window))
    {
      struct frame *sf = XFRAME (selected_frame);
      Fset_window_buffer (sf->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer, Qnil);
      minibuf_window = sf->minibuffer_window;
    }

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (frame)))
    x_make_frame_invisible (XFRAME (frame));
#endif

  /* Make menu bar update for the Buffers and Frames menus.  */
  windows_or_buffers_changed++;

  return Qnil;
}

DEFUN ("iconify-frame", Ficonify_frame, Siconify_frame,
       0, 1, "",
       doc: /* Make the frame FRAME into an icon.
If omitted, FRAME defaults to the currently selected frame.  */)
  (Lisp_Object frame)
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

#if 0 /* This isn't logically necessary, and it can do GC.  */
  /* Don't let the frame remain selected.  */
  if (EQ (frame, selected_frame))
    Fhandle_switch_frame (next_frame (frame, Qt));
#endif

  /* Don't allow minibuf_window to remain on a deleted frame.  */
  if (EQ (XFRAME (frame)->minibuffer_window, minibuf_window))
    {
      struct frame *sf = XFRAME (selected_frame);
      Fset_window_buffer (sf->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer, Qnil);
      minibuf_window = sf->minibuffer_window;
    }

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (frame)))
      x_iconify_frame (XFRAME (frame));
#endif

  /* Make menu bar update for the Buffers and Frames menus.  */
  windows_or_buffers_changed++;

  return Qnil;
}

DEFUN ("frame-visible-p", Fframe_visible_p, Sframe_visible_p,
       1, 1, 0,
       doc: /* Return t if FRAME is \"visible\" (actually in use for display).
Return the symbol `icon' if FRAME is iconified or \"minimized\".
Return nil if FRAME was made invisible, via `make-frame-invisible'.
On graphical displays, invisible frames are not updated and are
usually not displayed at all, even in a window system's \"taskbar\".

If FRAME is a text-only terminal frame, this always returns t.
Such frames are always considered visible, whether or not they are
currently being displayed on the terminal.  */)
  (Lisp_Object frame)
{
  CHECK_LIVE_FRAME (frame);

  FRAME_SAMPLE_VISIBILITY (XFRAME (frame));

  if (FRAME_VISIBLE_P (XFRAME (frame)))
    return Qt;
  if (FRAME_ICONIFIED_P (XFRAME (frame)))
    return Qicon;
  return Qnil;
}

DEFUN ("visible-frame-list", Fvisible_frame_list, Svisible_frame_list,
       0, 0, 0,
       doc: /* Return a list of all frames now \"visible\" (being updated).  */)
  (void)
{
  Lisp_Object tail, frame;
  struct frame *f;
  Lisp_Object value;

  value = Qnil;
  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
	continue;
      f = XFRAME (frame);
      if (FRAME_VISIBLE_P (f))
	value = Fcons (frame, value);
    }
  return value;
}


DEFUN ("raise-frame", Fraise_frame, Sraise_frame, 0, 1, "",
       doc: /* Bring FRAME to the front, so it occludes any frames it overlaps.
If FRAME is invisible or iconified, make it visible.
If you don't specify a frame, the selected frame is used.
If Emacs is displaying on an ordinary terminal or some other device which
doesn't support multiple overlapping frames, this function selects FRAME.  */)
  (Lisp_Object frame)
{
  struct frame *f;
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

  f = XFRAME (frame);

  if (FRAME_TERMCAP_P (f))
    /* On a text-only terminal select FRAME.  */
    Fselect_frame (frame, Qnil);
  else
    /* Do like the documentation says. */
    Fmake_frame_visible (frame);

  if (FRAME_TERMINAL (f)->frame_raise_lower_hook)
    (*FRAME_TERMINAL (f)->frame_raise_lower_hook) (f, 1);

  return Qnil;
}

/* Should we have a corresponding function called Flower_Power?  */
DEFUN ("lower-frame", Flower_frame, Slower_frame, 0, 1, "",
       doc: /* Send FRAME to the back, so it is occluded by any frames that overlap it.
If you don't specify a frame, the selected frame is used.
If Emacs is displaying on an ordinary terminal or some other device which
doesn't support multiple overlapping frames, this function does nothing.  */)
  (Lisp_Object frame)
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

  f = XFRAME (frame);

  if (FRAME_TERMINAL (f)->frame_raise_lower_hook)
    (*FRAME_TERMINAL (f)->frame_raise_lower_hook) (f, 0);

  return Qnil;
}


DEFUN ("redirect-frame-focus", Fredirect_frame_focus, Sredirect_frame_focus,
       1, 2, 0,
       doc: /* Arrange for keystrokes typed at FRAME to be sent to FOCUS-FRAME.
In other words, switch-frame events caused by events in FRAME will
request a switch to FOCUS-FRAME, and `last-event-frame' will be
FOCUS-FRAME after reading an event typed at FRAME.

If FOCUS-FRAME is omitted or nil, any existing redirection is
canceled, and the frame again receives its own keystrokes.

Focus redirection is useful for temporarily redirecting keystrokes to
a surrogate minibuffer frame when a frame doesn't have its own
minibuffer window.

A frame's focus redirection can be changed by `select-frame'.  If frame
FOO is selected, and then a different frame BAR is selected, any
frames redirecting their focus to FOO are shifted to redirect their
focus to BAR.  This allows focus redirection to work properly when the
user switches from one frame to another using `select-window'.

This means that a frame whose focus is redirected to itself is treated
differently from a frame whose focus is redirected to nil; the former
is affected by `select-frame', while the latter is not.

The redirection lasts until `redirect-frame-focus' is called to change it.  */)
  (Lisp_Object frame, Lisp_Object focus_frame)
{
  struct frame *f;

  /* Note that we don't check for a live frame here.  It's reasonable
     to redirect the focus of a frame you're about to delete, if you
     know what other frame should receive those keystrokes.  */
  CHECK_FRAME (frame);

  if (! NILP (focus_frame))
    CHECK_LIVE_FRAME (focus_frame);

  f = XFRAME (frame);

  f->focus_frame = focus_frame;

  if (FRAME_TERMINAL (f)->frame_rehighlight_hook)
    (*FRAME_TERMINAL (f)->frame_rehighlight_hook) (f);

  return Qnil;
}


DEFUN ("frame-focus", Fframe_focus, Sframe_focus, 1, 1, 0,
       doc: /* Return the frame to which FRAME's keystrokes are currently being sent.
This returns nil if FRAME's focus is not redirected.
See `redirect-frame-focus'.  */)
  (Lisp_Object frame)
{
  CHECK_LIVE_FRAME (frame);

  return FRAME_FOCUS_FRAME (XFRAME (frame));
}



/* Return the value of frame parameter PROP in frame FRAME.  */

#if !HAVE_NS
static
#endif
Lisp_Object
get_frame_param (register struct frame *frame, Lisp_Object prop)
{
  register Lisp_Object tem;

  tem = Fassq (prop, frame->param_alist);
  if (EQ (tem, Qnil))
    return tem;
  return Fcdr (tem);
}

/* Return the buffer-predicate of the selected frame.  */

Lisp_Object
frame_buffer_predicate (Lisp_Object frame)
{
  return XFRAME (frame)->buffer_predicate;
}

/* Return the buffer-list of the selected frame.  */

static Lisp_Object
frame_buffer_list (Lisp_Object frame)
{
  return XFRAME (frame)->buffer_list;
}

/* Discard BUFFER from the buffer-list and buried-buffer-list of each frame.  */

void
frames_discard_buffer (Lisp_Object buffer)
{
  Lisp_Object frame, tail;

  FOR_EACH_FRAME (tail, frame)
    {
      XFRAME (frame)->buffer_list
	= Fdelq (buffer, XFRAME (frame)->buffer_list);
      XFRAME (frame)->buried_buffer_list
        = Fdelq (buffer, XFRAME (frame)->buried_buffer_list);
    }
}

/* Modify the alist in *ALISTPTR to associate PROP with VAL.
   If the alist already has an element for PROP, we change it.  */

void
store_in_alist (Lisp_Object *alistptr, Lisp_Object prop, Lisp_Object val)
{
  register Lisp_Object tem;

  tem = Fassq (prop, *alistptr);
  if (EQ (tem, Qnil))
    *alistptr = Fcons (Fcons (prop, val), *alistptr);
  else
    Fsetcdr (tem, val);
}

static int
frame_name_fnn_p (char *str, EMACS_INT len)
{
  if (len > 1 && str[0] == 'F' && '0' <= str[1] && str[1] <= '9')
    {
      char *p = str + 2;
      while ('0' <= *p && *p <= '9')
	p++;
      if (p == str + len)
	return 1;
    }
  return 0;
}

/* Set the name of the terminal frame.  Also used by MSDOS frames.
   Modeled after x_set_name which is used for WINDOW frames.  */

static void
set_term_frame_name (struct frame *f, Lisp_Object name)
{
  f->explicit_name = ! NILP (name);

  /* If NAME is nil, set the name to F<num>.  */
  if (NILP (name))
    {
      char namebuf[sizeof "F" + INT_STRLEN_BOUND (printmax_t)];

      /* Check for no change needed in this very common case
	 before we do any consing.  */
      if (frame_name_fnn_p (SSDATA (f->name),
			    SBYTES (f->name)))
	return;

      tty_frame_count++;
      sprintf (namebuf, "F%"pMd, tty_frame_count);
      name = build_string (namebuf);
    }
  else
    {
      CHECK_STRING (name);

      /* Don't change the name if it's already NAME.  */
      if (! NILP (Fstring_equal (name, f->name)))
	return;

      /* Don't allow the user to set the frame name to F<num>, so it
	 doesn't clash with the names we generate for terminal frames.  */
      if (frame_name_fnn_p (SSDATA (name), SBYTES (name)))
	error ("Frame names of the form F<num> are usurped by Emacs");
    }

  f->name = name;
  update_mode_lines = 1;
}

void
store_frame_param (struct frame *f, Lisp_Object prop, Lisp_Object val)
{
  register Lisp_Object old_alist_elt;

  /* The buffer-list parameters are stored in a special place and not
     in the alist.  All buffers must be live.  */
  if (EQ (prop, Qbuffer_list))
    {
      Lisp_Object list = Qnil;
      for (; CONSP (val); val = XCDR (val))
	if (!NILP (Fbuffer_live_p (XCAR (val))))
	  list = Fcons (XCAR (val), list);
      f->buffer_list = Fnreverse (list);
      return;
    }
  if (EQ (prop, Qburied_buffer_list))
    {
      Lisp_Object list = Qnil;
      for (; CONSP (val); val = XCDR (val))
	if (!NILP (Fbuffer_live_p (XCAR (val))))
	  list = Fcons (XCAR (val), list);
      f->buried_buffer_list = Fnreverse (list);
      return;
    }

  /* If PROP is a symbol which is supposed to have frame-local values,
     and it is set up based on this frame, switch to the global
     binding.  That way, we can create or alter the frame-local binding
     without messing up the symbol's status.  */
  if (SYMBOLP (prop))
    {
      struct Lisp_Symbol *sym = XSYMBOL (prop);
    start:
      switch (sym->redirect)
	{
	case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
	case SYMBOL_PLAINVAL: case SYMBOL_FORWARDED: break;
	case SYMBOL_LOCALIZED:
	  { struct Lisp_Buffer_Local_Value *blv = sym->val.blv;
	    if (blv->frame_local && BLV_FOUND (blv) && XFRAME (blv->where) == f)
	      swap_in_global_binding (sym);
	    break;
	  }
	default: abort ();
	}
    }

  /* The tty color needed to be set before the frame's parameter
     alist was updated with the new value.  This is not true any more,
     but we still do this test early on.  */
  if (FRAME_TERMCAP_P (f) && EQ (prop, Qtty_color_mode)
      && f == FRAME_TTY (f)->previous_frame)
    /* Force redisplay of this tty.  */
    FRAME_TTY (f)->previous_frame = NULL;

  /* Update the frame parameter alist.  */
  old_alist_elt = Fassq (prop, f->param_alist);
  if (EQ (old_alist_elt, Qnil))
    f->param_alist = Fcons (Fcons (prop, val), f->param_alist);
  else
    Fsetcdr (old_alist_elt, val);

  /* Update some other special parameters in their special places
     in addition to the alist.  */

  if (EQ (prop, Qbuffer_predicate))
    f->buffer_predicate = val;

  if (! FRAME_WINDOW_P (f))
    {
      if (EQ (prop, Qmenu_bar_lines))
	set_menu_bar_lines (f, val, make_number (FRAME_MENU_BAR_LINES (f)));
      else if (EQ (prop, Qname))
	set_term_frame_name (f, val);
    }

  if (EQ (prop, Qminibuffer) && WINDOWP (val))
    {
      if (! MINI_WINDOW_P (XWINDOW (val)))
	error ("Surrogate minibuffer windows must be minibuffer windows");

      if ((FRAME_HAS_MINIBUF_P (f) || FRAME_MINIBUF_ONLY_P (f))
	  && !EQ (val, f->minibuffer_window))
	error ("Can't change the surrogate minibuffer of a frame with its own minibuffer");

      /* Install the chosen minibuffer window, with proper buffer.  */
      f->minibuffer_window = val;
    }
}

DEFUN ("frame-parameters", Fframe_parameters, Sframe_parameters, 0, 1, 0,
       doc: /* Return the parameters-alist of frame FRAME.
It is a list of elements of the form (PARM . VALUE), where PARM is a symbol.
The meaningful PARMs depend on the kind of frame.
If FRAME is omitted, return information on the currently selected frame.  */)
  (Lisp_Object frame)
{
  Lisp_Object alist;
  FRAME_PTR f;
  int height, width;
  struct gcpro gcpro1;

  if (NILP (frame))
    frame = selected_frame;

  CHECK_FRAME (frame);
  f = XFRAME (frame);

  if (!FRAME_LIVE_P (f))
    return Qnil;

  alist = Fcopy_alist (f->param_alist);
  GCPRO1 (alist);

  if (!FRAME_WINDOW_P (f))
    {
      int fg = FRAME_FOREGROUND_PIXEL (f);
      int bg = FRAME_BACKGROUND_PIXEL (f);
      Lisp_Object elt;

      /* If the frame's parameter alist says the colors are
	 unspecified and reversed, take the frame's background pixel
	 for foreground and vice versa.  */
      elt = Fassq (Qforeground_color, alist);
      if (CONSP (elt) && STRINGP (XCDR (elt)))
	{
	  if (strncmp (SSDATA (XCDR (elt)),
		       unspecified_bg,
		       SCHARS (XCDR (elt))) == 0)
	    store_in_alist (&alist, Qforeground_color, tty_color_name (f, bg));
	  else if (strncmp (SSDATA (XCDR (elt)),
			    unspecified_fg,
			    SCHARS (XCDR (elt))) == 0)
	    store_in_alist (&alist, Qforeground_color, tty_color_name (f, fg));
	}
      else
	store_in_alist (&alist, Qforeground_color, tty_color_name (f, fg));
      elt = Fassq (Qbackground_color, alist);
      if (CONSP (elt) && STRINGP (XCDR (elt)))
	{
	  if (strncmp (SSDATA (XCDR (elt)),
		       unspecified_fg,
		       SCHARS (XCDR (elt))) == 0)
	    store_in_alist (&alist, Qbackground_color, tty_color_name (f, fg));
	  else if (strncmp (SSDATA (XCDR (elt)),
			    unspecified_bg,
			    SCHARS (XCDR (elt))) == 0)
	    store_in_alist (&alist, Qbackground_color, tty_color_name (f, bg));
	}
      else
	store_in_alist (&alist, Qbackground_color, tty_color_name (f, bg));
      store_in_alist (&alist, intern ("font"),
		      build_string (FRAME_MSDOS_P (f)
				    ? "ms-dos"
				    : FRAME_W32_P (f) ? "w32term"
				    :"tty"));
    }
  store_in_alist (&alist, Qname, f->name);
  height = (f->new_text_lines ? f->new_text_lines : FRAME_LINES (f));
  store_in_alist (&alist, Qheight, make_number (height));
  width = (f->new_text_cols ? f->new_text_cols : FRAME_COLS (f));
  store_in_alist (&alist, Qwidth, make_number (width));
  store_in_alist (&alist, Qmodeline, (FRAME_WANTS_MODELINE_P (f) ? Qt : Qnil));
  store_in_alist (&alist, Qminibuffer,
		  (! FRAME_HAS_MINIBUF_P (f) ? Qnil
		   : FRAME_MINIBUF_ONLY_P (f) ? Qonly
		   : FRAME_MINIBUF_WINDOW (f)));
  store_in_alist (&alist, Qunsplittable, (FRAME_NO_SPLIT_P (f) ? Qt : Qnil));
  store_in_alist (&alist, Qbuffer_list, frame_buffer_list (frame));
  store_in_alist (&alist, Qburied_buffer_list, XFRAME (frame)->buried_buffer_list);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    x_report_frame_params (f, &alist);
  else
#endif
    {
      /* This ought to be correct in f->param_alist for an X frame.  */
      Lisp_Object lines;
      XSETFASTINT (lines, FRAME_MENU_BAR_LINES (f));
      store_in_alist (&alist, Qmenu_bar_lines, lines);
    }

  UNGCPRO;
  return alist;
}


DEFUN ("frame-parameter", Fframe_parameter, Sframe_parameter, 2, 2, 0,
       doc: /* Return FRAME's value for parameter PARAMETER.
If FRAME is nil, describe the currently selected frame.  */)
  (Lisp_Object frame, Lisp_Object parameter)
{
  struct frame *f;
  Lisp_Object value;

  if (NILP (frame))
    frame = selected_frame;
  else
    CHECK_FRAME (frame);
  CHECK_SYMBOL (parameter);

  f = XFRAME (frame);
  value = Qnil;

  if (FRAME_LIVE_P (f))
    {
      /* Avoid consing in frequent cases.  */
      if (EQ (parameter, Qname))
	value = f->name;
#ifdef HAVE_X_WINDOWS
      else if (EQ (parameter, Qdisplay) && FRAME_X_P (f))
	value = XCAR (FRAME_X_DISPLAY_INFO (f)->name_list_element);
#endif /* HAVE_X_WINDOWS */
      else if (EQ (parameter, Qbackground_color)
	       || EQ (parameter, Qforeground_color))
	{
	  value = Fassq (parameter, f->param_alist);
	  if (CONSP (value))
	    {
	      value = XCDR (value);
	      /* Fframe_parameters puts the actual fg/bg color names,
		 even if f->param_alist says otherwise.  This is
		 important when param_alist's notion of colors is
		 "unspecified".  We need to do the same here.  */
	      if (STRINGP (value) && !FRAME_WINDOW_P (f))
		{
		  const char *color_name;
		  EMACS_INT csz;

		  if (EQ (parameter, Qbackground_color))
		    {
		      color_name = SSDATA (value);
		      csz = SCHARS (value);
		      if (strncmp (color_name, unspecified_bg, csz) == 0)
			value = tty_color_name (f, FRAME_BACKGROUND_PIXEL (f));
		      else if (strncmp (color_name, unspecified_fg, csz) == 0)
			value = tty_color_name (f, FRAME_FOREGROUND_PIXEL (f));
		    }
		  else if (EQ (parameter, Qforeground_color))
		    {
		      color_name = SSDATA (value);
		      csz = SCHARS (value);
		      if (strncmp (color_name, unspecified_fg, csz) == 0)
			value = tty_color_name (f, FRAME_FOREGROUND_PIXEL (f));
		      else if (strncmp (color_name, unspecified_bg, csz) == 0)
			value = tty_color_name (f, FRAME_BACKGROUND_PIXEL (f));
		    }
		}
	    }
	  else
	    value = Fcdr (Fassq (parameter, Fframe_parameters (frame)));
	}
      else if (EQ (parameter, Qdisplay_type)
	       || EQ (parameter, Qbackground_mode))
	value = Fcdr (Fassq (parameter, f->param_alist));
      else
	/* FIXME: Avoid this code path at all (as well as code duplication)
	   by sharing more code with Fframe_parameters.  */
	value = Fcdr (Fassq (parameter, Fframe_parameters (frame)));
    }

  return value;
}


DEFUN ("modify-frame-parameters", Fmodify_frame_parameters,
       Smodify_frame_parameters, 2, 2, 0,
       doc: /* Modify the parameters of frame FRAME according to ALIST.
If FRAME is nil, it defaults to the selected frame.
ALIST is an alist of parameters to change and their new values.
Each element of ALIST has the form (PARM . VALUE), where PARM is a symbol.
The meaningful PARMs depend on the kind of frame.
Undefined PARMs are ignored, but stored in the frame's parameter list
so that `frame-parameters' will return them.

The value of frame parameter FOO can also be accessed
as a frame-local binding for the variable FOO, if you have
enabled such bindings for that variable with `make-variable-frame-local'.
Note that this functionality is obsolete as of Emacs 22.2, and its
use is not recommended.  Explicitly check for a frame-parameter instead.  */)
  (Lisp_Object frame, Lisp_Object alist)
{
  FRAME_PTR f;
  register Lisp_Object tail, prop, val;

  if (EQ (frame, Qnil))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    x_set_frame_parameters (f, alist);
  else
#endif
#ifdef MSDOS
  if (FRAME_MSDOS_P (f))
    IT_set_frame_parameters (f, alist);
  else
#endif

    {
      int length = XINT (Flength (alist));
      int i;
      Lisp_Object *parms
	= (Lisp_Object *) alloca (length * sizeof (Lisp_Object));
      Lisp_Object *values
	= (Lisp_Object *) alloca (length * sizeof (Lisp_Object));

      /* Extract parm names and values into those vectors.  */

      i = 0;
      for (tail = alist; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object elt;

	  elt = XCAR (tail);
	  parms[i] = Fcar (elt);
	  values[i] = Fcdr (elt);
	  i++;
	}

      /* Now process them in reverse of specified order.  */
      while (--i >= 0)
	{
	  prop = parms[i];
	  val = values[i];
	  store_frame_param (f, prop, val);

	  if (EQ (prop, Qforeground_color)
	      || EQ (prop, Qbackground_color))
	    update_face_from_frame_parameter (f, prop, val);
	}
    }
  return Qnil;
}

DEFUN ("frame-char-height", Fframe_char_height, Sframe_char_height,
       0, 1, 0,
       doc: /* Height in pixels of a line in the font in frame FRAME.
If FRAME is omitted, the selected frame is used.
For a terminal frame, the value is always 1.  */)
  (Lisp_Object frame)
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_FRAME (frame);
  f = XFRAME (frame);

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    return make_number (x_char_height (f));
  else
#endif
    return make_number (1);
}


DEFUN ("frame-char-width", Fframe_char_width, Sframe_char_width,
       0, 1, 0,
       doc: /* Width in pixels of characters in the font in frame FRAME.
If FRAME is omitted, the selected frame is used.
On a graphical screen, the width is the standard width of the default font.
For a terminal screen, the value is always 1.  */)
  (Lisp_Object frame)
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_FRAME (frame);
  f = XFRAME (frame);

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    return make_number (x_char_width (f));
  else
#endif
    return make_number (1);
}

DEFUN ("frame-pixel-height", Fframe_pixel_height,
       Sframe_pixel_height, 0, 1, 0,
       doc: /* Return a FRAME's height in pixels.
If FRAME is omitted, the selected frame is used.  The exact value
of the result depends on the window-system and toolkit in use:

In the Gtk+ version of Emacs, it includes only any window (including
the minibuffer or echo area), mode line, and header line.  It does not
include the tool bar or menu bar.

With the Motif or Lucid toolkits, it also includes the tool bar (but
not the menu bar).

In a graphical version with no toolkit, it includes both the tool bar
and menu bar.

For a text-only terminal, it includes the menu bar.  In this case, the
result is really in characters rather than pixels (i.e., is identical
to `frame-height'). */)
  (Lisp_Object frame)
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_FRAME (frame);
  f = XFRAME (frame);

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    return make_number (x_pixel_height (f));
  else
#endif
    return make_number (FRAME_LINES (f));
}

DEFUN ("frame-pixel-width", Fframe_pixel_width,
       Sframe_pixel_width, 0, 1, 0,
       doc: /* Return FRAME's width in pixels.
For a terminal frame, the result really gives the width in characters.
If FRAME is omitted, the selected frame is used.  */)
  (Lisp_Object frame)
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_FRAME (frame);
  f = XFRAME (frame);

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    return make_number (x_pixel_width (f));
  else
#endif
    return make_number (FRAME_COLS (f));
}

DEFUN ("tool-bar-pixel-width", Ftool_bar_pixel_width,
       Stool_bar_pixel_width, 0, 1, 0,
       doc: /* Return width in pixels of FRAME's tool bar.
The result is greater than zero only when the tool bar is on the left
or right side of FRAME.  If FRAME is omitted, the selected frame is
used.  */)
  (Lisp_Object frame)
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_FRAME (frame);
  f = XFRAME (frame);

#ifdef FRAME_TOOLBAR_WIDTH
  if (FRAME_WINDOW_P (f))
    return make_number (FRAME_TOOLBAR_WIDTH (f));
#endif
  return make_number (0);
}

DEFUN ("set-frame-height", Fset_frame_height, Sset_frame_height, 2, 3, 0,
       doc: /* Specify that the frame FRAME has LINES lines.
Optional third arg non-nil means that redisplay should use LINES lines
but that the idea of the actual height of the frame should not be changed.  */)
  (Lisp_Object frame, Lisp_Object lines, Lisp_Object pretend)
{
  register struct frame *f;

  CHECK_NUMBER (lines);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      if (XINT (lines) != FRAME_LINES (f))
	x_set_window_size (f, 1, FRAME_COLS (f), XINT (lines));
      do_pending_window_change (0);
    }
  else
#endif
    change_frame_size (f, XINT (lines), 0, !NILP (pretend), 0, 0);
  return Qnil;
}

DEFUN ("set-frame-width", Fset_frame_width, Sset_frame_width, 2, 3, 0,
       doc: /* Specify that the frame FRAME has COLS columns.
Optional third arg non-nil means that redisplay should use COLS columns
but that the idea of the actual width of the frame should not be changed.  */)
  (Lisp_Object frame, Lisp_Object cols, Lisp_Object pretend)
{
  register struct frame *f;
  CHECK_NUMBER (cols);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      if (XINT (cols) != FRAME_COLS (f))
	x_set_window_size (f, 1, XINT (cols), FRAME_LINES (f));
      do_pending_window_change (0);
    }
  else
#endif
    change_frame_size (f, 0, XINT (cols), !NILP (pretend), 0, 0);
  return Qnil;
}

DEFUN ("set-frame-size", Fset_frame_size, Sset_frame_size, 3, 3, 0,
       doc: /* Sets size of FRAME to COLS by ROWS, measured in characters.  */)
  (Lisp_Object frame, Lisp_Object cols, Lisp_Object rows)
{
  register struct frame *f;

  CHECK_LIVE_FRAME (frame);
  CHECK_NUMBER (cols);
  CHECK_NUMBER (rows);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      if (XINT (rows) != FRAME_LINES (f)
	  || XINT (cols) != FRAME_COLS (f)
	  || f->new_text_lines || f->new_text_cols)
	x_set_window_size (f, 1, XINT (cols), XINT (rows));
      do_pending_window_change (0);
    }
  else
#endif
    change_frame_size (f, XINT (rows), XINT (cols), 0, 0, 0);

  return Qnil;
}

DEFUN ("set-frame-position", Fset_frame_position,
       Sset_frame_position, 3, 3, 0,
       doc: /* Sets position of FRAME in pixels to XOFFSET by YOFFSET.
This is actually the position of the upper left corner of the frame.
Negative values for XOFFSET or YOFFSET are interpreted relative to
the rightmost or bottommost possible position (that stays within the screen).  */)
  (Lisp_Object frame, Lisp_Object xoffset, Lisp_Object yoffset)
{
  register struct frame *f;

  CHECK_LIVE_FRAME (frame);
  CHECK_NUMBER (xoffset);
  CHECK_NUMBER (yoffset);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    x_set_offset (f, XINT (xoffset), XINT (yoffset), 1);
#endif

  return Qt;
}


/***********************************************************************
				Frame Parameters
 ***********************************************************************/

/* Connect the frame-parameter names for X frames
   to the ways of passing the parameter values to the window system.

   The name of a parameter, as a Lisp symbol,
   has an `x-frame-parameter' property which is an integer in Lisp
   that is an index in this table.  */

struct frame_parm_table {
  const char *name;
  Lisp_Object *variable;
};

static const struct frame_parm_table frame_parms[] =
{
  {"auto-raise",		&Qauto_raise},
  {"auto-lower",		&Qauto_lower},
  {"background-color",		0},
  {"border-color",		&Qborder_color},
  {"border-width",		&Qborder_width},
  {"cursor-color",		&Qcursor_color},
  {"cursor-type",		&Qcursor_type},
  {"font",			0},
  {"foreground-color",		0},
  {"icon-name",			&Qicon_name},
  {"icon-type",			&Qicon_type},
  {"internal-border-width",	&Qinternal_border_width},
  {"menu-bar-lines",		&Qmenu_bar_lines},
  {"mouse-color",		&Qmouse_color},
  {"name",			&Qname},
  {"scroll-bar-width",		&Qscroll_bar_width},
  {"title",			&Qtitle},
  {"unsplittable",		&Qunsplittable},
  {"vertical-scroll-bars",	&Qvertical_scroll_bars},
  {"visibility",		&Qvisibility},
  {"tool-bar-lines",		&Qtool_bar_lines},
  {"scroll-bar-foreground",	&Qscroll_bar_foreground},
  {"scroll-bar-background",	&Qscroll_bar_background},
  {"screen-gamma",		&Qscreen_gamma},
  {"line-spacing",		&Qline_spacing},
  {"left-fringe",		&Qleft_fringe},
  {"right-fringe",		&Qright_fringe},
  {"wait-for-wm",		&Qwait_for_wm},
  {"fullscreen",                &Qfullscreen},
  {"font-backend",		&Qfont_backend},
  {"alpha",			&Qalpha},
  {"sticky",			&Qsticky},
  {"tool-bar-position",		&Qtool_bar_position},
};

#ifdef WINDOWSNT

/* Calculate fullscreen size.  Return in *TOP_POS and *LEFT_POS the
   wanted positions of the WM window (not Emacs window).
   Return in *WIDTH and *HEIGHT the wanted width and height of Emacs
   window (FRAME_X_WINDOW).
 */

void
x_fullscreen_adjust (struct frame *f, int *width, int *height, int *top_pos, int *left_pos)
{
  int newwidth = FRAME_COLS (f);
  int newheight = FRAME_LINES (f);
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  *top_pos = f->top_pos;
  *left_pos = f->left_pos;

  if (f->want_fullscreen & FULLSCREEN_HEIGHT)
    {
      int ph;

      ph = x_display_pixel_height (dpyinfo);
      newheight = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, ph);
      ph = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, newheight) - f->y_pixels_diff;
      newheight = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, ph);
      *top_pos = 0;
    }

  if (f->want_fullscreen & FULLSCREEN_WIDTH)
    {
      int pw;

      pw = x_display_pixel_width (dpyinfo);
      newwidth = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, pw);
      pw = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, newwidth) - f->x_pixels_diff;
      newwidth = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, pw);
      *left_pos = 0;
    }

  *width = newwidth;
  *height = newheight;
}

#endif /* WINDOWSNT */

#ifdef HAVE_WINDOW_SYSTEM

/* Change the parameters of frame F as specified by ALIST.
   If a parameter is not specially recognized, do nothing special;
   otherwise call the `x_set_...' function for that parameter.
   Except for certain geometry properties, always call store_frame_param
   to store the new value in the parameter alist.  */

void
x_set_frame_parameters (FRAME_PTR f, Lisp_Object alist)
{
  Lisp_Object tail;

  /* If both of these parameters are present, it's more efficient to
     set them both at once.  So we wait until we've looked at the
     entire list before we set them.  */
  int width, height;

  /* Same here.  */
  Lisp_Object left, top;

  /* Same with these.  */
  Lisp_Object icon_left, icon_top;

  /* Record in these vectors all the parms specified.  */
  Lisp_Object *parms;
  Lisp_Object *values;
  ptrdiff_t i, p;
  int left_no_change = 0, top_no_change = 0;
  int icon_left_no_change = 0, icon_top_no_change = 0;
  int size_changed = 0;
  struct gcpro gcpro1, gcpro2;

  i = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    i++;

  parms = (Lisp_Object *) alloca (i * sizeof (Lisp_Object));
  values = (Lisp_Object *) alloca (i * sizeof (Lisp_Object));

  /* Extract parm names and values into those vectors.  */

  i = 0;
  for (tail = alist; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt;

      elt = XCAR (tail);
      parms[i] = Fcar (elt);
      values[i] = Fcdr (elt);
      i++;
    }
  /* TAIL and ALIST are not used again below here.  */
  alist = tail = Qnil;

  GCPRO2 (*parms, *values);
  gcpro1.nvars = i;
  gcpro2.nvars = i;

  /* There is no need to gcpro LEFT, TOP, ICON_LEFT, or ICON_TOP,
     because their values appear in VALUES and strings are not valid.  */
  top = left = Qunbound;
  icon_left = icon_top = Qunbound;

  /* Provide default values for HEIGHT and WIDTH.  */
  width = (f->new_text_cols ? f->new_text_cols : FRAME_COLS (f));
  height = (f->new_text_lines ? f->new_text_lines : FRAME_LINES (f));

  /* Process foreground_color and background_color before anything else.
     They are independent of other properties, but other properties (e.g.,
     cursor_color) are dependent upon them.  */
  /* Process default font as well, since fringe widths depends on it.  */
  for (p = 0; p < i; p++)
    {
      Lisp_Object prop, val;

      prop = parms[p];
      val = values[p];
      if (EQ (prop, Qforeground_color)
	  || EQ (prop, Qbackground_color)
	  || EQ (prop, Qfont))
	{
	  register Lisp_Object param_index, old_value;

	  old_value = get_frame_param (f, prop);
	  if (NILP (Fequal (val, old_value)))
	    {
	      store_frame_param (f, prop, val);

	      param_index = Fget (prop, Qx_frame_parameter);
	      if (NATNUMP (param_index)
		  && (XFASTINT (param_index)
		      < sizeof (frame_parms)/sizeof (frame_parms[0]))
                  && FRAME_RIF (f)->frame_parm_handlers[XINT (param_index)])
                (*(FRAME_RIF (f)->frame_parm_handlers[XINT (param_index)])) (f, val, old_value);
	    }
	}
    }

  /* Now process them in reverse of specified order.  */
  while (i-- != 0)
    {
      Lisp_Object prop, val;

      prop = parms[i];
      val = values[i];

      if (EQ (prop, Qwidth) && NATNUMP (val))
        {
          size_changed = 1;
          width = XFASTINT (val);
        }
      else if (EQ (prop, Qheight) && NATNUMP (val))
        {
          size_changed = 1;
          height = XFASTINT (val);
        }
      else if (EQ (prop, Qtop))
	top = val;
      else if (EQ (prop, Qleft))
	left = val;
      else if (EQ (prop, Qicon_top))
	icon_top = val;
      else if (EQ (prop, Qicon_left))
	icon_left = val;
      else if (EQ (prop, Qforeground_color)
	       || EQ (prop, Qbackground_color)
	       || EQ (prop, Qfont))
	/* Processed above.  */
	continue;
      else
	{
	  register Lisp_Object param_index, old_value;

	  old_value = get_frame_param (f, prop);

	  store_frame_param (f, prop, val);

	  param_index = Fget (prop, Qx_frame_parameter);
	  if (NATNUMP (param_index)
	      && (XFASTINT (param_index)
		  < sizeof (frame_parms)/sizeof (frame_parms[0]))
	      && FRAME_RIF (f)->frame_parm_handlers[XINT (param_index)])
	    (*(FRAME_RIF (f)->frame_parm_handlers[XINT (param_index)])) (f, val, old_value);
	}
    }

  /* Don't die if just one of these was set.  */
  if (EQ (left, Qunbound))
    {
      left_no_change = 1;
      if (f->left_pos < 0)
	left = Fcons (Qplus, Fcons (make_number (f->left_pos), Qnil));
      else
	XSETINT (left, f->left_pos);
    }
  if (EQ (top, Qunbound))
    {
      top_no_change = 1;
      if (f->top_pos < 0)
	top = Fcons (Qplus, Fcons (make_number (f->top_pos), Qnil));
      else
	XSETINT (top, f->top_pos);
    }

  /* If one of the icon positions was not set, preserve or default it.  */
  if (EQ (icon_left, Qunbound) || ! INTEGERP (icon_left))
    {
      icon_left_no_change = 1;
      icon_left = Fcdr (Fassq (Qicon_left, f->param_alist));
      if (NILP (icon_left))
	XSETINT (icon_left, 0);
    }
  if (EQ (icon_top, Qunbound) || ! INTEGERP (icon_top))
    {
      icon_top_no_change = 1;
      icon_top = Fcdr (Fassq (Qicon_top, f->param_alist));
      if (NILP (icon_top))
	XSETINT (icon_top, 0);
    }

  /* Don't set these parameters unless they've been explicitly
     specified.  The window might be mapped or resized while we're in
     this function, and we don't want to override that unless the lisp
     code has asked for it.

     Don't set these parameters unless they actually differ from the
     window's current parameters; the window may not actually exist
     yet.  */
  {
    Lisp_Object frame;

    check_frame_size (f, &height, &width);

    XSETFRAME (frame, f);

    if (size_changed
        && (width != FRAME_COLS (f)
            || height != FRAME_LINES (f)
            || f->new_text_lines || f->new_text_cols))
        Fset_frame_size (frame, make_number (width), make_number (height));

    if ((!NILP (left) || !NILP (top))
	&& ! (left_no_change && top_no_change)
	&& ! (NUMBERP (left) && XINT (left) == f->left_pos
	      && NUMBERP (top) && XINT (top) == f->top_pos))
      {
	int leftpos = 0;
	int toppos = 0;

	/* Record the signs.  */
	f->size_hint_flags &= ~ (XNegative | YNegative);
	if (EQ (left, Qminus))
	  f->size_hint_flags |= XNegative;
	else if (TYPE_RANGED_INTEGERP (int, left))
	  {
	    leftpos = XINT (left);
	    if (leftpos < 0)
	      f->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qminus)
		 && CONSP (XCDR (left))
		 && RANGED_INTEGERP (-INT_MAX, XCAR (XCDR (left)), INT_MAX))
	  {
	    leftpos = - XINT (XCAR (XCDR (left)));
	    f->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qplus)
		 && CONSP (XCDR (left))
		 && TYPE_RANGED_INTEGERP (int, XCAR (XCDR (left))))
	  {
	    leftpos = XINT (XCAR (XCDR (left)));
	  }

	if (EQ (top, Qminus))
	  f->size_hint_flags |= YNegative;
	else if (TYPE_RANGED_INTEGERP (int, top))
	  {
	    toppos = XINT (top);
	    if (toppos < 0)
	      f->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qminus)
		 && CONSP (XCDR (top))
		 && RANGED_INTEGERP (-INT_MAX, XCAR (XCDR (top)), INT_MAX))
	  {
	    toppos = - XINT (XCAR (XCDR (top)));
	    f->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qplus)
		 && CONSP (XCDR (top))
		 && TYPE_RANGED_INTEGERP (int, XCAR (XCDR (top))))
	  {
	    toppos = XINT (XCAR (XCDR (top)));
	  }


	/* Store the numeric value of the position.  */
	f->top_pos = toppos;
	f->left_pos = leftpos;

	f->win_gravity = NorthWestGravity;

	/* Actually set that position, and convert to absolute.  */
	x_set_offset (f, leftpos, toppos, -1);
      }

    if ((!NILP (icon_left) || !NILP (icon_top))
	&& ! (icon_left_no_change && icon_top_no_change))
      x_wm_set_icon_position (f, XINT (icon_left), XINT (icon_top));
  }

  UNGCPRO;
}


/* Insert a description of internally-recorded parameters of frame X
   into the parameter alist *ALISTPTR that is to be given to the user.
   Only parameters that are specific to the X window system
   and whose values are not correctly recorded in the frame's
   param_alist need to be considered here.  */

void
x_report_frame_params (struct frame *f, Lisp_Object *alistptr)
{
  char buf[16];
  Lisp_Object tem;
  unsigned long w;

  /* Represent negative positions (off the top or left screen edge)
     in a way that Fmodify_frame_parameters will understand correctly.  */
  XSETINT (tem, f->left_pos);
  if (f->left_pos >= 0)
    store_in_alist (alistptr, Qleft, tem);
  else
    store_in_alist (alistptr, Qleft, Fcons (Qplus, Fcons (tem, Qnil)));

  XSETINT (tem, f->top_pos);
  if (f->top_pos >= 0)
    store_in_alist (alistptr, Qtop, tem);
  else
    store_in_alist (alistptr, Qtop, Fcons (Qplus, Fcons (tem, Qnil)));

  store_in_alist (alistptr, Qborder_width,
		  make_number (f->border_width));
  store_in_alist (alistptr, Qinternal_border_width,
		  make_number (FRAME_INTERNAL_BORDER_WIDTH (f)));
  store_in_alist (alistptr, Qleft_fringe,
		  make_number (FRAME_LEFT_FRINGE_WIDTH (f)));
  store_in_alist (alistptr, Qright_fringe,
		  make_number (FRAME_RIGHT_FRINGE_WIDTH (f)));
  store_in_alist (alistptr, Qscroll_bar_width,
		  (! FRAME_HAS_VERTICAL_SCROLL_BARS (f)
		   ? make_number (0)
		   : FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0
		   ? make_number (FRAME_CONFIG_SCROLL_BAR_WIDTH (f))
		   /* nil means "use default width"
		      for non-toolkit scroll bar.
		      ruler-mode.el depends on this.  */
		   : Qnil));
  /* FRAME_X_WINDOW is not guaranteed to return an integer.  E.g., on
     MS-Windows it returns a value whose type is HANDLE, which is
     actually a pointer.  Explicit casting avoids compiler
     warnings.  */
  w = (unsigned long) FRAME_X_WINDOW (f);
  sprintf (buf, "%lu", w);
  store_in_alist (alistptr, Qwindow_id,
		  build_string (buf));
#ifdef HAVE_X_WINDOWS
#ifdef USE_X_TOOLKIT
  /* Tooltip frame may not have this widget.  */
  if (FRAME_X_OUTPUT (f)->widget)
#endif
    {
      w = (unsigned long) FRAME_OUTER_WINDOW (f);
      sprintf (buf, "%lu", w);
    }
  store_in_alist (alistptr, Qouter_window_id,
		  build_string (buf));
#endif
  store_in_alist (alistptr, Qicon_name, f->icon_name);
  FRAME_SAMPLE_VISIBILITY (f);
  store_in_alist (alistptr, Qvisibility,
		  (FRAME_VISIBLE_P (f) ? Qt
		   : FRAME_ICONIFIED_P (f) ? Qicon : Qnil));
  store_in_alist (alistptr, Qdisplay,
		  XCAR (FRAME_X_DISPLAY_INFO (f)->name_list_element));

  if (FRAME_X_OUTPUT (f)->parent_desc == FRAME_X_DISPLAY_INFO (f)->root_window)
    tem = Qnil;
  else
    XSETFASTINT (tem, FRAME_X_OUTPUT (f)->parent_desc);
  store_in_alist (alistptr, Qexplicit_name, (f->explicit_name ? Qt : Qnil));
  store_in_alist (alistptr, Qparent_id, tem);
  store_in_alist (alistptr, Qtool_bar_position, f->tool_bar_position);
}


/* Change the `fullscreen' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value. */

void
x_set_fullscreen (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (NILP (new_value))
    f->want_fullscreen = FULLSCREEN_NONE;
  else if (EQ (new_value, Qfullboth) || EQ (new_value, Qfullscreen))
    f->want_fullscreen = FULLSCREEN_BOTH;
  else if (EQ (new_value, Qfullwidth))
    f->want_fullscreen = FULLSCREEN_WIDTH;
  else if (EQ (new_value, Qfullheight))
    f->want_fullscreen = FULLSCREEN_HEIGHT;
  else if (EQ (new_value, Qmaximized))
    f->want_fullscreen = FULLSCREEN_MAXIMIZED;

  if (FRAME_TERMINAL (f)->fullscreen_hook != NULL)
    FRAME_TERMINAL (f)->fullscreen_hook (f);
}


/* Change the `line-spacing' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value.  */

void
x_set_line_spacing (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (NILP (new_value))
    f->extra_line_spacing = 0;
  else if (NATNUMP (new_value))
    f->extra_line_spacing = XFASTINT (new_value);
  else
    signal_error ("Invalid line-spacing", new_value);
  if (FRAME_VISIBLE_P (f))
    redraw_frame (f);
}


/* Change the `screen-gamma' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value.  */

void
x_set_screen_gamma (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  Lisp_Object bgcolor;

  if (NILP (new_value))
    f->gamma = 0;
  else if (NUMBERP (new_value) && XFLOATINT (new_value) > 0)
    /* The value 0.4545 is the normal viewing gamma.  */
    f->gamma = 1.0 / (0.4545 * XFLOATINT (new_value));
  else
    signal_error ("Invalid screen-gamma", new_value);

  /* Apply the new gamma value to the frame background.  */
  bgcolor = Fassq (Qbackground_color, f->param_alist);
  if (CONSP (bgcolor) && (bgcolor = XCDR (bgcolor), STRINGP (bgcolor)))
    {
      Lisp_Object parm_index = Fget (Qbackground_color, Qx_frame_parameter);
      if (NATNUMP (parm_index)
	  && (XFASTINT (parm_index)
	      < sizeof (frame_parms)/sizeof (frame_parms[0]))
	  && FRAME_RIF (f)->frame_parm_handlers[XFASTINT (parm_index)])
	  (*FRAME_RIF (f)->frame_parm_handlers[XFASTINT (parm_index)])
	    (f, bgcolor, Qnil);
    }

  Fclear_face_cache (Qnil);
}


void
x_set_font (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  Lisp_Object font_object, font_param = Qnil;
  int fontset = -1;

  /* Set the frame parameter back to the old value because we may
     fail to use ARG as the new parameter value.  */
  store_frame_param (f, Qfont, oldval);

  /* ARG is a fontset name, a font name, a cons of fontset name and a
     font object, or a font object.  In the last case, this function
     never fail.  */
  if (STRINGP (arg))
    {
      font_param = arg;
      fontset = fs_query_fontset (arg, 0);
      if (fontset < 0)
	{
	  font_object = font_open_by_name (f, SSDATA (arg));
	  if (NILP (font_object))
	    error ("Font `%s' is not defined", SSDATA (arg));
	  arg = AREF (font_object, FONT_NAME_INDEX);
	}
      else if (fontset > 0)
	{
	  Lisp_Object ascii_font = fontset_ascii (fontset);

	  font_object = font_open_by_name (f, SSDATA (ascii_font));
	  if (NILP (font_object))
	    error ("Font `%s' is not defined", SDATA (arg));
	  arg = AREF (font_object, FONT_NAME_INDEX);
	}
      else
	error ("The default fontset can't be used for a frame font");
    }
  else if (CONSP (arg) && STRINGP (XCAR (arg)) && FONT_OBJECT_P (XCDR (arg)))
    {
      /* This is the case that the ASCII font of F's fontset XCAR
	 (arg) is changed to the font XCDR (arg) by
	 `set-fontset-font'.  */
      fontset = fs_query_fontset (XCAR (arg), 0);
      if (fontset < 0)
	error ("Unknown fontset: %s", SDATA (XCAR (arg)));
      font_object = XCDR (arg);
      arg = AREF (font_object, FONT_NAME_INDEX);
      font_param = Ffont_get (font_object, QCname);
    }
  else if (FONT_OBJECT_P (arg))
    {
      font_object = arg;
      font_param = Ffont_get (font_object, QCname);
      /* This is to store the XLFD font name in the frame parameter for
	 backward compatibility.  We should store the font-object
	 itself in the future.  */
      arg = AREF (font_object, FONT_NAME_INDEX);
      fontset = FRAME_FONTSET (f);
      /* Check if we can use the current fontset.  If not, set FONTSET
	 to -1 to generate a new fontset from FONT-OBJECT.  */
      if (fontset >= 0)
	{
	  Lisp_Object ascii_font = fontset_ascii (fontset);
	  Lisp_Object spec = font_spec_from_name (ascii_font);

	  if (! font_match_p (spec, font_object))
	    fontset = -1;
	}
    }
  else
    signal_error ("Invalid font", arg);

  if (! NILP (Fequal (font_object, oldval)))
    return;

  x_new_font (f, font_object, fontset);
  store_frame_param (f, Qfont, arg);
#ifdef HAVE_X_WINDOWS
  store_frame_param (f, Qfont_param, font_param);
#endif
  /* Recalculate toolbar height.  */
  f->n_tool_bar_rows = 0;
  /* Ensure we redraw it.  */
  clear_current_matrices (f);

  recompute_basic_faces (f);

  do_pending_window_change (0);

  /* We used to call face-set-after-frame-default here, but it leads to
     recursive calls (since that function can set the `default' face's
     font which in turns changes the frame's `font' parameter).
     Also I don't know what this call is meant to do, but it seems the
     wrong way to do it anyway (it does a lot more work than what seems
     reasonable in response to a change to `font').  */
}


void
x_set_font_backend (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (! NILP (new_value)
      && !CONSP (new_value))
    {
      char *p0, *p1;

      CHECK_STRING (new_value);
      p0 = p1 = SSDATA (new_value);
      new_value = Qnil;
      while (*p0)
	{
	  while (*p1 && ! isspace (*p1) && *p1 != ',') p1++;
	  if (p0 < p1)
	    new_value = Fcons (Fintern (make_string (p0, p1 - p0), Qnil),
			       new_value);
	  if (*p1)
	    {
	      int c;

	      while ((c = *++p1) && isspace (c));
	    }
	  p0 = p1;
	}
      new_value = Fnreverse (new_value);
    }

  if (! NILP (old_value) && ! NILP (Fequal (old_value, new_value)))
    return;

  if (FRAME_FONT (f))
    free_all_realized_faces (Qnil);

  new_value = font_update_drivers (f, NILP (new_value) ? Qt : new_value);
  if (NILP (new_value))
    {
      if (NILP (old_value))
	error ("No font backend available");
      font_update_drivers (f, old_value);
      error ("None of specified font backends are available");
    }
  store_frame_param (f, Qfont_backend, new_value);

  if (FRAME_FONT (f))
    {
      Lisp_Object frame;

      XSETFRAME (frame, f);
      x_set_font (f, Fframe_parameter (frame, Qfont), Qnil);
      ++face_change_count;
      ++windows_or_buffers_changed;
    }
}


void
x_set_fringe_width (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  compute_fringe_widths (f, 1);
#ifdef HAVE_X_WINDOWS
  /* Must adjust this so window managers report correct number of columns.  */
  if (FRAME_X_WINDOW (f) != 0)
    x_wm_set_size_hint (f, 0, 0);
#endif
}

void
x_set_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  CHECK_NUMBER (arg);

  if (XINT (arg) == f->border_width)
    return;

  if (FRAME_X_WINDOW (f) != 0)
    error ("Cannot change the border width of a frame");

  f->border_width = XINT (arg);
}

void
x_set_internal_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int old = FRAME_INTERNAL_BORDER_WIDTH (f);

  CHECK_NUMBER (arg);
  FRAME_INTERNAL_BORDER_WIDTH (f) = XINT (arg);
  if (FRAME_INTERNAL_BORDER_WIDTH (f) < 0)
    FRAME_INTERNAL_BORDER_WIDTH (f) = 0;

#ifdef USE_X_TOOLKIT
  if (FRAME_X_OUTPUT (f)->edit_widget)
    widget_store_internal_border (FRAME_X_OUTPUT (f)->edit_widget);
#endif

  if (FRAME_INTERNAL_BORDER_WIDTH (f) == old)
    return;

  if (FRAME_X_WINDOW (f) != 0)
    {
      x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
      SET_FRAME_GARBAGED (f);
      do_pending_window_change (0);
    }
  else
    SET_FRAME_GARBAGED (f);
}

void
x_set_visibility (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  Lisp_Object frame;
  XSETFRAME (frame, f);

  if (NILP (value))
    Fmake_frame_invisible (frame, Qt);
  else if (EQ (value, Qicon))
    Ficonify_frame (frame);
  else
    Fmake_frame_visible (frame);
}

void
x_set_autoraise (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  f->auto_raise = !EQ (Qnil, arg);
}

void
x_set_autolower (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  f->auto_lower = !EQ (Qnil, arg);
}

void
x_set_unsplittable (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  f->no_split = !NILP (arg);
}

void
x_set_vertical_scroll_bars (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  if ((EQ (arg, Qleft) && FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
      || (EQ (arg, Qright) && FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f))
      || (NILP (arg) && FRAME_HAS_VERTICAL_SCROLL_BARS (f))
      || (!NILP (arg) && ! FRAME_HAS_VERTICAL_SCROLL_BARS (f)))
    {
      FRAME_VERTICAL_SCROLL_BAR_TYPE (f)
	= (NILP (arg)
	   ? vertical_scroll_bar_none
	   : EQ (Qleft, arg)
	   ? vertical_scroll_bar_left
	   : EQ (Qright, arg)
	   ? vertical_scroll_bar_right
	   : EQ (Qleft, Vdefault_frame_scroll_bars)
	   ? vertical_scroll_bar_left
	   : EQ (Qright, Vdefault_frame_scroll_bars)
	   ? vertical_scroll_bar_right
	   : vertical_scroll_bar_none);

      /* We set this parameter before creating the X window for the
	 frame, so we can get the geometry right from the start.
	 However, if the window hasn't been created yet, we shouldn't
	 call x_set_window_size.  */
      if (FRAME_X_WINDOW (f))
	x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
      do_pending_window_change (0);
    }
}

void
x_set_scroll_bar_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int wid = FRAME_COLUMN_WIDTH (f);

  if (NILP (arg))
    {
      x_set_scroll_bar_default_width (f);

      if (FRAME_X_WINDOW (f))
        x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
      do_pending_window_change (0);
    }
  else if (RANGED_INTEGERP (1, arg, INT_MAX)
	   && XFASTINT (arg) != FRAME_CONFIG_SCROLL_BAR_WIDTH (f))
    {
      if (XFASTINT (arg) <= 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM)
	XSETINT (arg, 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM + 1);

      FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = XFASTINT (arg);
      FRAME_CONFIG_SCROLL_BAR_COLS (f) = (XFASTINT (arg) + wid-1) / wid;
      if (FRAME_X_WINDOW (f))
	x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
      do_pending_window_change (0);
    }

  change_frame_size (f, 0, FRAME_COLS (f), 0, 0, 0);
  XWINDOW (FRAME_SELECTED_WINDOW (f))->cursor.hpos = 0;
  XWINDOW (FRAME_SELECTED_WINDOW (f))->cursor.x = 0;
}



/* Return non-nil if frame F wants a bitmap icon.  */

Lisp_Object
x_icon_type (FRAME_PTR f)
{
  Lisp_Object tem;

  tem = assq_no_quit (Qicon_type, f->param_alist);
  if (CONSP (tem))
    return XCDR (tem);
  else
    return Qnil;
}

void
x_set_alpha (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  double alpha = 1.0;
  double newval[2];
  int i;
  Lisp_Object item;

  for (i = 0; i < 2; i++)
    {
      newval[i] = 1.0;
      if (CONSP (arg))
        {
          item = CAR (arg);
          arg  = CDR (arg);
        }
      else
        item = arg;

      if (NILP (item))
	alpha = - 1.0;
      else if (FLOATP (item))
	{
	  alpha = XFLOAT_DATA (item);
	  if (alpha < 0.0 || 1.0 < alpha)
	    args_out_of_range (make_float (0.0), make_float (1.0));
	}
      else if (INTEGERP (item))
	{
	  EMACS_INT ialpha = XINT (item);
	  if (ialpha < 0 || 100 < ialpha)
	    args_out_of_range (make_number (0), make_number (100));
	  else
	    alpha = ialpha / 100.0;
	}
      else
	wrong_type_argument (Qnumberp, item);
      newval[i] = alpha;
    }

  for (i = 0; i < 2; i++)
    f->alpha[i] = newval[i];

#if defined (HAVE_X_WINDOWS) || defined (HAVE_NTGUI) || defined (NS_IMPL_COCOA)
  BLOCK_INPUT;
  x_set_frame_alpha (f);
  UNBLOCK_INPUT;
#endif

  return;
}


/* Subroutines of creating an X frame.  */

/* Make sure that Vx_resource_name is set to a reasonable value.
   Fix it up, or set it to `emacs' if it is too hopeless.  */

void
validate_x_resource_name (void)
{
  ptrdiff_t len = 0;
  /* Number of valid characters in the resource name.  */
  ptrdiff_t good_count = 0;
  /* Number of invalid characters in the resource name.  */
  ptrdiff_t bad_count = 0;
  Lisp_Object new;
  ptrdiff_t i;

  if (!STRINGP (Vx_resource_class))
    Vx_resource_class = build_string (EMACS_CLASS);

  if (STRINGP (Vx_resource_name))
    {
      unsigned char *p = SDATA (Vx_resource_name);

      len = SBYTES (Vx_resource_name);

      /* Only letters, digits, - and _ are valid in resource names.
	 Count the valid characters and count the invalid ones.  */
      for (i = 0; i < len; i++)
	{
	  int c = p[i];
	  if (! ((c >= 'a' && c <= 'z')
		 || (c >= 'A' && c <= 'Z')
		 || (c >= '0' && c <= '9')
		 || c == '-' || c == '_'))
	    bad_count++;
	  else
	    good_count++;
	}
    }
  else
    /* Not a string => completely invalid.  */
    bad_count = 5, good_count = 0;

  /* If name is valid already, return.  */
  if (bad_count == 0)
    return;

  /* If name is entirely invalid, or nearly so, or is so implausibly
     large that alloca might not work, use `emacs'.  */
  if (good_count < 2 || MAX_ALLOCA - sizeof ".customization" < len)
    {
      Vx_resource_name = build_string ("emacs");
      return;
    }

  /* Name is partly valid.  Copy it and replace the invalid characters
     with underscores.  */

  Vx_resource_name = new = Fcopy_sequence (Vx_resource_name);

  for (i = 0; i < len; i++)
    {
      int c = SREF (new, i);
      if (! ((c >= 'a' && c <= 'z')
	     || (c >= 'A' && c <= 'Z')
	     || (c >= '0' && c <= '9')
	     || c == '-' || c == '_'))
	SSET (new, i, '_');
    }
}


extern char *x_get_string_resource (XrmDatabase, const char *, const char *);
extern Display_Info *check_x_display_info (Lisp_Object);


/* Get specified attribute from resource database RDB.
   See Fx_get_resource below for other parameters.  */

static Lisp_Object
xrdb_get_resource (XrmDatabase rdb, Lisp_Object attribute, Lisp_Object class, Lisp_Object component, Lisp_Object subclass)
{
  register char *value;
  char *name_key;
  char *class_key;

  CHECK_STRING (attribute);
  CHECK_STRING (class);

  if (!NILP (component))
    CHECK_STRING (component);
  if (!NILP (subclass))
    CHECK_STRING (subclass);
  if (NILP (component) != NILP (subclass))
    error ("x-get-resource: must specify both COMPONENT and SUBCLASS or neither");

  validate_x_resource_name ();

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  Make them big enough for the worst case.  */
  name_key = (char *) alloca (SBYTES (Vx_resource_name)
			      + (STRINGP (component)
				 ? SBYTES (component) : 0)
			      + SBYTES (attribute)
			      + 3);

  class_key = (char *) alloca (SBYTES (Vx_resource_class)
			       + SBYTES (class)
			       + (STRINGP (subclass)
				  ? SBYTES (subclass) : 0)
			       + 3);

  /* Start with emacs.FRAMENAME for the name (the specific one)
     and with `Emacs' for the class key (the general one).  */
  strcpy (name_key, SSDATA (Vx_resource_name));
  strcpy (class_key, SSDATA (Vx_resource_class));

  strcat (class_key, ".");
  strcat (class_key, SSDATA (class));

  if (!NILP (component))
    {
      strcat (class_key, ".");
      strcat (class_key, SSDATA (subclass));

      strcat (name_key, ".");
      strcat (name_key, SSDATA (component));
    }

  strcat (name_key, ".");
  strcat (name_key, SSDATA (attribute));

  value = x_get_string_resource (rdb, name_key, class_key);

  if (value != (char *) 0 && *value)
    return build_string (value);
  else
    return Qnil;
}


DEFUN ("x-get-resource", Fx_get_resource, Sx_get_resource, 2, 4, 0,
       doc: /* Return the value of ATTRIBUTE, of class CLASS, from the X defaults database.
This uses `INSTANCE.ATTRIBUTE' as the key and `Emacs.CLASS' as the
class, where INSTANCE is the name under which Emacs was invoked, or
the name specified by the `-name' or `-rn' command-line arguments.

The optional arguments COMPONENT and SUBCLASS add to the key and the
class, respectively.  You must specify both of them or neither.
If you specify them, the key is `INSTANCE.COMPONENT.ATTRIBUTE'
and the class is `Emacs.CLASS.SUBCLASS'.  */)
  (Lisp_Object attribute, Lisp_Object class, Lisp_Object component, Lisp_Object subclass)
{
#ifdef HAVE_X_WINDOWS
  check_x ();
#endif

  return xrdb_get_resource (check_x_display_info (Qnil)->xrdb,
			    attribute, class, component, subclass);
}

/* Get an X resource, like Fx_get_resource, but for display DPYINFO.  */

Lisp_Object
display_x_get_resource (Display_Info *dpyinfo, Lisp_Object attribute, Lisp_Object class, Lisp_Object component, Lisp_Object subclass)
{
  return xrdb_get_resource (dpyinfo->xrdb,
			    attribute, class, component, subclass);
}

#if defined HAVE_X_WINDOWS && !defined USE_X_TOOLKIT
/* Used when C code wants a resource value.  */
/* Called from oldXMenu/Create.c.  */
char *
x_get_resource_string (const char *attribute, const char *class)
{
  char *name_key;
  char *class_key;
  char *result;
  struct frame *sf = SELECTED_FRAME ();
  ptrdiff_t invocation_namelen = SBYTES (Vinvocation_name);
  USE_SAFE_ALLOCA;

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  */
  SAFE_ALLOCA (name_key, char *, invocation_namelen + strlen (attribute) + 2);
  class_key = (char *) alloca ((sizeof (EMACS_CLASS) - 1)
			       + strlen (class) + 2);

  esprintf (name_key, "%s.%s", SSDATA (Vinvocation_name), attribute);
  sprintf (class_key, "%s.%s", EMACS_CLASS, class);

  result = x_get_string_resource (FRAME_X_DISPLAY_INFO (sf)->xrdb,
				  name_key, class_key);
  SAFE_FREE ();
  return result;
}
#endif

/* Return the value of parameter PARAM.

   First search ALIST, then Vdefault_frame_alist, then the X defaults
   database, using ATTRIBUTE as the attribute name and CLASS as its class.

   Convert the resource to the type specified by desired_type.

   If no default is specified, return Qunbound.  If you call
   x_get_arg, make sure you deal with Qunbound in a reasonable way,
   and don't let it get stored in any Lisp-visible variables!  */

Lisp_Object
x_get_arg (Display_Info *dpyinfo, Lisp_Object alist, Lisp_Object param,
	   const char *attribute, const char *class, enum resource_types type)
{
  register Lisp_Object tem;

  tem = Fassq (param, alist);

  if (!NILP (tem))
    {
      /* If we find this parm in ALIST, clear it out
	 so that it won't be "left over" at the end.  */
      Lisp_Object tail;
      XSETCAR (tem, Qnil);
      /* In case the parameter appears more than once in the alist,
	 clear it out.  */
      for (tail = alist; CONSP (tail); tail = XCDR (tail))
	if (CONSP (XCAR (tail))
	    && EQ (XCAR (XCAR (tail)), param))
	  XSETCAR (XCAR (tail), Qnil);
    }
  else
    tem = Fassq (param, Vdefault_frame_alist);

  /* If it wasn't specified in ALIST or the Lisp-level defaults,
     look in the X resources.  */
  if (EQ (tem, Qnil))
    {
      if (attribute && dpyinfo)
	{
	  tem = display_x_get_resource (dpyinfo,
					build_string (attribute),
					build_string (class),
					Qnil, Qnil);

	  if (NILP (tem))
	    return Qunbound;

	  switch (type)
	    {
	    case RES_TYPE_NUMBER:
	      return make_number (atoi (SSDATA (tem)));

	    case RES_TYPE_BOOLEAN_NUMBER:
	      if (!strcmp (SSDATA (tem), "on")
		  || !strcmp (SSDATA (tem), "true"))
		return make_number (1);
	      return make_number (atoi (SSDATA (tem)));
              break;

	    case RES_TYPE_FLOAT:
	      return make_float (atof (SSDATA (tem)));

	    case RES_TYPE_BOOLEAN:
	      tem = Fdowncase (tem);
	      if (!strcmp (SSDATA (tem), "on")
#ifdef HAVE_NS
                  || !strcmp (SSDATA (tem), "yes")
#endif
		  || !strcmp (SSDATA (tem), "true"))
		return Qt;
	      else
		return Qnil;

	    case RES_TYPE_STRING:
	      return tem;

	    case RES_TYPE_SYMBOL:
	      /* As a special case, we map the values `true' and `on'
		 to Qt, and `false' and `off' to Qnil.  */
	      {
		Lisp_Object lower;
		lower = Fdowncase (tem);
		if (!strcmp (SSDATA (lower), "on")
#ifdef HAVE_NS
                    || !strcmp (SSDATA (lower), "yes")
#endif
		    || !strcmp (SSDATA (lower), "true"))
		  return Qt;
		else if (!strcmp (SSDATA (lower), "off")
#ifdef HAVE_NS
                      || !strcmp (SSDATA (lower), "no")
#endif
		      || !strcmp (SSDATA (lower), "false"))
		  return Qnil;
		else
		  return Fintern (tem, Qnil);
	      }

	    default:
	      abort ();
	    }
	}
      else
	return Qunbound;
    }
  return Fcdr (tem);
}

static Lisp_Object
x_frame_get_arg (struct frame *f, Lisp_Object alist, Lisp_Object param,
		 const char *attribute, const char *class,
		 enum resource_types type)
{
  return x_get_arg (FRAME_X_DISPLAY_INFO (f),
		    alist, param, attribute, class, type);
}

/* Like x_frame_get_arg, but also record the value in f->param_alist.  */

Lisp_Object
x_frame_get_and_record_arg (struct frame *f, Lisp_Object alist,
			    Lisp_Object param,
			    const char *attribute, const char *class,
			    enum resource_types type)
{
  Lisp_Object value;

  value = x_get_arg (FRAME_X_DISPLAY_INFO (f), alist, param,
		     attribute, class, type);
  if (! NILP (value) && ! EQ (value, Qunbound))
    store_frame_param (f, param, value);

  return value;
}


/* Record in frame F the specified or default value according to ALIST
   of the parameter named PROP (a Lisp symbol).
   If no value is specified for PROP, look for an X default for XPROP
   on the frame named NAME.
   If that is not found either, use the value DEFLT.  */

Lisp_Object
x_default_parameter (struct frame *f, Lisp_Object alist, Lisp_Object prop,
		     Lisp_Object deflt, const char *xprop, const char *xclass,
		     enum resource_types type)
{
  Lisp_Object tem;

  tem = x_frame_get_arg (f, alist, prop, xprop, xclass, type);
  if (EQ (tem, Qunbound))
    tem = deflt;
  x_set_frame_parameters (f, Fcons (Fcons (prop, tem), Qnil));
  return tem;
}




/* NS used to define x-parse-geometry in ns-win.el, but that confused
   make-docfile: the documentation string in ns-win.el was used for
   x-parse-geometry even in non-NS builds.

   With two definitions of x-parse-geometry in this file, various
   things still get confused (eg M-x apropos documentation), so that
   it is best if the two definitions just share the same doc-string.
*/
DEFUN ("x-parse-geometry", Fx_parse_geometry, Sx_parse_geometry, 1, 1, 0,
       doc: /* Parse a display geometry string STRING.
Returns an alist of the form ((top . TOP), (left . LEFT) ... ).
The properties returned may include `top', `left', `height', and `width'.
For X, the value of `left' or `top' may be an integer,
or a list (+ N) meaning N pixels relative to top/left corner,
or a list (- N) meaning -N pixels relative to bottom/right corner.
On Nextstep, this just calls `ns-parse-geometry'.  */)
  (Lisp_Object string)
{
#ifdef HAVE_NS
  call1 (Qns_parse_geometry, string);
#else
  int geometry, x, y;
  unsigned int width, height;
  Lisp_Object result;

  CHECK_STRING (string);

  geometry = XParseGeometry (SSDATA (string),
			     &x, &y, &width, &height);
  result = Qnil;
  if (geometry & XValue)
    {
      Lisp_Object element;

      if (x >= 0 && (geometry & XNegative))
	element = Fcons (Qleft, Fcons (Qminus, Fcons (make_number (-x), Qnil)));
      else if (x < 0 && ! (geometry & XNegative))
	element = Fcons (Qleft, Fcons (Qplus, Fcons (make_number (x), Qnil)));
      else
	element = Fcons (Qleft, make_number (x));
      result = Fcons (element, result);
    }

  if (geometry & YValue)
    {
      Lisp_Object element;

      if (y >= 0 && (geometry & YNegative))
	element = Fcons (Qtop, Fcons (Qminus, Fcons (make_number (-y), Qnil)));
      else if (y < 0 && ! (geometry & YNegative))
	element = Fcons (Qtop, Fcons (Qplus, Fcons (make_number (y), Qnil)));
      else
	element = Fcons (Qtop, make_number (y));
      result = Fcons (element, result);
    }

  if (geometry & WidthValue)
    result = Fcons (Fcons (Qwidth, make_number (width)), result);
  if (geometry & HeightValue)
    result = Fcons (Fcons (Qheight, make_number (height)), result);

  return result;
#endif /* HAVE_NS */
}


/* Calculate the desired size and position of frame F.
   Return the flags saying which aspects were specified.

   Also set the win_gravity and size_hint_flags of F.

   Adjust height for toolbar if TOOLBAR_P is 1.

   This function does not make the coordinates positive.  */

#define DEFAULT_ROWS 35
#define DEFAULT_COLS 80

int
x_figure_window_size (struct frame *f, Lisp_Object parms, int toolbar_p)
{
  register Lisp_Object tem0, tem1, tem2;
  long window_prompting = 0;
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  /* Default values if we fall through.
     Actually, if that happens we should get
     window manager prompting.  */
  SET_FRAME_COLS (f, DEFAULT_COLS);
  FRAME_LINES (f) = DEFAULT_ROWS;
  /* Window managers expect that if program-specified
     positions are not (0,0), they're intentional, not defaults.  */
  f->top_pos = 0;
  f->left_pos = 0;

  /* Ensure that old new_text_cols and new_text_lines will not override the
     values set here.  */
  /* ++KFS: This was specific to W32, but seems ok for all platforms */
  f->new_text_cols = f->new_text_lines = 0;

  tem0 = x_get_arg (dpyinfo, parms, Qheight, 0, 0, RES_TYPE_NUMBER);
  tem1 = x_get_arg (dpyinfo, parms, Qwidth, 0, 0, RES_TYPE_NUMBER);
  tem2 = x_get_arg (dpyinfo, parms, Quser_size, 0, 0, RES_TYPE_NUMBER);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (!EQ (tem0, Qunbound))
	{
	  CHECK_NUMBER (tem0);
	  if (! (0 <= XINT (tem0) && XINT (tem0) <= INT_MAX))
	    xsignal1 (Qargs_out_of_range, tem0);
	  FRAME_LINES (f) = XINT (tem0);
	}
      if (!EQ (tem1, Qunbound))
	{
	  CHECK_NUMBER (tem1);
	  if (! (0 <= XINT (tem1) && XINT (tem1) <= INT_MAX))
	    xsignal1 (Qargs_out_of_range, tem1);
	  SET_FRAME_COLS (f, XINT (tem1));
	}
      if (!NILP (tem2) && !EQ (tem2, Qunbound))
	window_prompting |= USSize;
      else
	window_prompting |= PSize;
    }

  f->scroll_bar_actual_width
    = FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f);

  /* This used to be done _before_ calling x_figure_window_size, but
     since the height is reset here, this was really a no-op.  I
     assume that moving it here does what Gerd intended (although he
     no longer can remember what that was...  ++KFS, 2003-03-25.  */

  /* Add the tool-bar height to the initial frame height so that the
     user gets a text display area of the size he specified with -g or
     via .Xdefaults.  Later changes of the tool-bar height don't
     change the frame size.  This is done so that users can create
     tall Emacs frames without having to guess how tall the tool-bar
     will get.  */
  if (toolbar_p && FRAME_TOOL_BAR_LINES (f))
    {
      int margin, relief, bar_height;

      relief = (tool_bar_button_relief >= 0
		? tool_bar_button_relief
		: DEFAULT_TOOL_BAR_BUTTON_RELIEF);

      if (RANGED_INTEGERP (1, Vtool_bar_button_margin, INT_MAX))
	margin = XFASTINT (Vtool_bar_button_margin);
      else if (CONSP (Vtool_bar_button_margin)
	       && RANGED_INTEGERP (1, XCDR (Vtool_bar_button_margin), INT_MAX))
	margin = XFASTINT (XCDR (Vtool_bar_button_margin));
      else
	margin = 0;

      bar_height = DEFAULT_TOOL_BAR_IMAGE_HEIGHT + 2 * margin + 2 * relief;
      FRAME_LINES (f) += (bar_height + FRAME_LINE_HEIGHT (f) - 1) / FRAME_LINE_HEIGHT (f);
    }

  compute_fringe_widths (f, 0);

  FRAME_PIXEL_WIDTH (f) = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, FRAME_COLS (f));
  FRAME_PIXEL_HEIGHT (f) = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, FRAME_LINES (f));

  tem0 = x_get_arg (dpyinfo, parms, Qtop, 0, 0, RES_TYPE_NUMBER);
  tem1 = x_get_arg (dpyinfo, parms, Qleft, 0, 0, RES_TYPE_NUMBER);
  tem2 = x_get_arg (dpyinfo, parms, Quser_position, 0, 0, RES_TYPE_NUMBER);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (EQ (tem0, Qminus))
	{
	  f->top_pos = 0;
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCAR (tem0), Qminus)
	       && CONSP (XCDR (tem0))
	       && RANGED_INTEGERP (-INT_MAX, XCAR (XCDR (tem0)), INT_MAX))
	{
	  f->top_pos = - XINT (XCAR (XCDR (tem0)));
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCAR (tem0), Qplus)
	       && CONSP (XCDR (tem0))
	       && TYPE_RANGED_INTEGERP (int, XCAR (XCDR (tem0))))
	{
	  f->top_pos = XINT (XCAR (XCDR (tem0)));
	}
      else if (EQ (tem0, Qunbound))
	f->top_pos = 0;
      else
	{
	  CHECK_NUMBER (tem0);
	  f->top_pos = XINT (tem0);
	  if (f->top_pos < 0)
	    window_prompting |= YNegative;
	}

      if (EQ (tem1, Qminus))
	{
	  f->left_pos = 0;
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCAR (tem1), Qminus)
	       && CONSP (XCDR (tem1))
	       && RANGED_INTEGERP (-INT_MAX, XCAR (XCDR (tem1)), INT_MAX))
	{
	  f->left_pos = - XINT (XCAR (XCDR (tem1)));
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCAR (tem1), Qplus)
	       && CONSP (XCDR (tem1))
	       && TYPE_RANGED_INTEGERP (int, XCAR (XCDR (tem1))))
	{
	  f->left_pos = XINT (XCAR (XCDR (tem1)));
	}
      else if (EQ (tem1, Qunbound))
	f->left_pos = 0;
      else
	{
	  CHECK_NUMBER (tem1);
	  f->left_pos = XINT (tem1);
	  if (f->left_pos < 0)
	    window_prompting |= XNegative;
	}

      if (!NILP (tem2) && ! EQ (tem2, Qunbound))
	window_prompting |= USPosition;
      else
	window_prompting |= PPosition;
    }

  if (window_prompting & XNegative)
    {
      if (window_prompting & YNegative)
	f->win_gravity = SouthEastGravity;
      else
	f->win_gravity = NorthEastGravity;
    }
  else
    {
      if (window_prompting & YNegative)
	f->win_gravity = SouthWestGravity;
      else
	f->win_gravity = NorthWestGravity;
    }

  f->size_hint_flags = window_prompting;

  return window_prompting;
}



#endif /* HAVE_WINDOW_SYSTEM */

void
frame_make_pointer_invisible (void)
{
  if (! NILP (Vmake_pointer_invisible))
    {
      struct frame *f;
      if (!FRAMEP (selected_frame) || !FRAME_LIVE_P (XFRAME (selected_frame)))
        return;

      f = SELECTED_FRAME ();
      if (f && !f->pointer_invisible
          && FRAME_TERMINAL (f)->toggle_invisible_pointer_hook)
        {
          f->mouse_moved = 0;
          FRAME_TERMINAL (f)->toggle_invisible_pointer_hook (f, 1);
          f->pointer_invisible = 1;
        }
    }
}

void
frame_make_pointer_visible (void)
{
  /* We don't check Vmake_pointer_invisible here in case the
     pointer was invisible when Vmake_pointer_invisible was set to nil.  */
  struct frame *f;

  if (!FRAMEP (selected_frame) || !FRAME_LIVE_P (XFRAME (selected_frame)))
    return;

  f = SELECTED_FRAME ();
  if (f && f->pointer_invisible && f->mouse_moved
      && FRAME_TERMINAL (f)->toggle_invisible_pointer_hook)
    {
      FRAME_TERMINAL (f)->toggle_invisible_pointer_hook (f, 0);
      f->pointer_invisible = 0;
    }
}

DEFUN ("frame-pointer-visible-p", Fframe_pointer_visible_p,
       Sframe_pointer_visible_p, 0, 1, 0,
       doc: /* Return t if the mouse pointer displayed on FRAME is visible.
Otherwise it returns nil.  FRAME omitted or nil means the
selected frame.  This is useful when `make-pointer-invisible' is set.  */)
  (Lisp_Object frame)
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_FRAME (frame);

  return (XFRAME (frame)->pointer_invisible ? Qnil : Qt);
}


/***********************************************************************
				Initialization
 ***********************************************************************/

void
syms_of_frame (void)
{
  DEFSYM (Qframep, "framep");
  DEFSYM (Qframe_live_p, "frame-live-p");
  DEFSYM (Qexplicit_name, "explicit-name");
  DEFSYM (Qheight, "height");
  DEFSYM (Qicon, "icon");
  DEFSYM (Qminibuffer, "minibuffer");
  DEFSYM (Qmodeline, "modeline");
  DEFSYM (Qonly, "only");
  DEFSYM (Qwidth, "width");
  DEFSYM (Qgeometry, "geometry");
  DEFSYM (Qicon_left, "icon-left");
  DEFSYM (Qicon_top, "icon-top");
  DEFSYM (Qtooltip, "tooltip");
  DEFSYM (Qleft, "left");
  DEFSYM (Qright, "right");
  DEFSYM (Quser_position, "user-position");
  DEFSYM (Quser_size, "user-size");
  DEFSYM (Qwindow_id, "window-id");
#ifdef HAVE_X_WINDOWS
  DEFSYM (Qouter_window_id, "outer-window-id");
#endif
  DEFSYM (Qparent_id, "parent-id");
  DEFSYM (Qx, "x");
  DEFSYM (Qw32, "w32");
  DEFSYM (Qpc, "pc");
  DEFSYM (Qmac, "mac");
  DEFSYM (Qns, "ns");
  DEFSYM (Qvisible, "visible");
  DEFSYM (Qbuffer_predicate, "buffer-predicate");
  DEFSYM (Qbuffer_list, "buffer-list");
  DEFSYM (Qburied_buffer_list, "buried-buffer-list");
  DEFSYM (Qdisplay_type, "display-type");
  DEFSYM (Qbackground_mode, "background-mode");
  DEFSYM (Qnoelisp, "noelisp");
  DEFSYM (Qtty_color_mode, "tty-color-mode");
  DEFSYM (Qtty, "tty");
  DEFSYM (Qtty_type, "tty-type");

  DEFSYM (Qface_set_after_frame_default, "face-set-after-frame-default");

  DEFSYM (Qfullwidth, "fullwidth");
  DEFSYM (Qfullheight, "fullheight");
  DEFSYM (Qfullboth, "fullboth");
  DEFSYM (Qmaximized, "maximized");
  DEFSYM (Qx_resource_name, "x-resource-name");
  DEFSYM (Qx_frame_parameter, "x-frame-parameter");

  DEFSYM (Qterminal, "terminal");
  DEFSYM (Qterminal_live_p, "terminal-live-p");

#ifdef HAVE_NS
  DEFSYM (Qns_parse_geometry, "ns-parse-geometry");
#endif

  {
    int i;

    for (i = 0; i < sizeof (frame_parms) / sizeof (frame_parms[0]); i++)
      {
	Lisp_Object v = intern_c_string (frame_parms[i].name);
	if (frame_parms[i].variable)
	  {
	    *frame_parms[i].variable = v;
	    staticpro (frame_parms[i].variable);
	  }
	Fput (v, Qx_frame_parameter, make_number (i));
      }
  }

#ifdef HAVE_WINDOW_SYSTEM
  DEFVAR_LISP ("x-resource-name", Vx_resource_name,
    doc: /* The name Emacs uses to look up X resources.
`x-get-resource' uses this as the first component of the instance name
when requesting resource values.
Emacs initially sets `x-resource-name' to the name under which Emacs
was invoked, or to the value specified with the `-name' or `-rn'
switches, if present.

It may be useful to bind this variable locally around a call
to `x-get-resource'.  See also the variable `x-resource-class'.  */);
  Vx_resource_name = Qnil;

  DEFVAR_LISP ("x-resource-class", Vx_resource_class,
    doc: /* The class Emacs uses to look up X resources.
`x-get-resource' uses this as the first component of the instance class
when requesting resource values.

Emacs initially sets `x-resource-class' to "Emacs".

Setting this variable permanently is not a reasonable thing to do,
but binding this variable locally around a call to `x-get-resource'
is a reasonable practice.  See also the variable `x-resource-name'.  */);
  Vx_resource_class = build_string (EMACS_CLASS);

  DEFVAR_LISP ("frame-alpha-lower-limit", Vframe_alpha_lower_limit,
    doc: /* The lower limit of the frame opacity (alpha transparency).
The value should range from 0 (invisible) to 100 (completely opaque).
You can also use a floating number between 0.0 and 1.0.
The default is 20.  */);
  Vframe_alpha_lower_limit = make_number (20);
#endif

  DEFVAR_LISP ("default-frame-alist", Vdefault_frame_alist,
	       doc: /* Alist of default values for frame creation.
These may be set in your init file, like this:
  (setq default-frame-alist '((width . 80) (height . 55) (menu-bar-lines . 1)))
These override values given in window system configuration data,
 including X Windows' defaults database.
For values specific to the first Emacs frame, see `initial-frame-alist'.
For window-system specific values, see `window-system-default-frame-alist'.
For values specific to the separate minibuffer frame, see
 `minibuffer-frame-alist'.
The `menu-bar-lines' element of the list controls whether new frames
 have menu bars; `menu-bar-mode' works by altering this element.
Setting this variable does not affect existing frames, only new ones.  */);
  Vdefault_frame_alist = Qnil;

  DEFVAR_LISP ("default-frame-scroll-bars", Vdefault_frame_scroll_bars,
	       doc: /* Default position of scroll bars on this window-system.  */);
#ifdef HAVE_WINDOW_SYSTEM
#if defined (HAVE_NTGUI) || defined (NS_IMPL_COCOA) || (defined (USE_GTK) && defined (USE_TOOLKIT_SCROLL_BARS))
  /* MS-Windows, Mac OS X, and GTK have scroll bars on the right by
     default.  */
  Vdefault_frame_scroll_bars = Qright;
#else
  Vdefault_frame_scroll_bars = Qleft;
#endif
#else
  Vdefault_frame_scroll_bars = Qnil;
#endif

  DEFVAR_LISP ("terminal-frame", Vterminal_frame,
               doc: /* The initial frame-object, which represents Emacs's stdout.  */);

  DEFVAR_LISP ("mouse-position-function", Vmouse_position_function,
	       doc: /* If non-nil, function to transform normal value of `mouse-position'.
`mouse-position' calls this function, passing its usual return value as
argument, and returns whatever this function returns.
This abnormal hook exists for the benefit of packages like `xt-mouse.el'
which need to do mouse handling at the Lisp level.  */);
  Vmouse_position_function = Qnil;

  DEFVAR_LISP ("mouse-highlight", Vmouse_highlight,
	       doc: /* If non-nil, clickable text is highlighted when mouse is over it.
If the value is an integer, highlighting is only shown after moving the
mouse, while keyboard input turns off the highlight even when the mouse
is over the clickable text.  However, the mouse shape still indicates
when the mouse is over clickable text.  */);
  Vmouse_highlight = Qt;

  DEFVAR_LISP ("make-pointer-invisible", Vmake_pointer_invisible,
               doc: /* If non-nil, make pointer invisible while typing.
The pointer becomes visible again when the mouse is moved.  */);
  Vmake_pointer_invisible = Qt;

  DEFVAR_LISP ("delete-frame-functions", Vdelete_frame_functions,
	       doc: /* Functions to be run before deleting a frame.
The functions are run with one arg, the frame to be deleted.
See `delete-frame'.

Note that functions in this list may be called just before the frame is
actually deleted, or some time later (or even both when an earlier function
in `delete-frame-functions' (indirectly) calls `delete-frame'
recursively).  */);
  Vdelete_frame_functions = Qnil;
  DEFSYM (Qdelete_frame_functions, "delete-frame-functions");

  DEFVAR_LISP ("menu-bar-mode", Vmenu_bar_mode,
               doc: /* Non-nil if Menu-Bar mode is enabled.
See the command `menu-bar-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `menu-bar-mode'.  */);
  Vmenu_bar_mode = Qt;

  DEFVAR_LISP ("tool-bar-mode", Vtool_bar_mode,
               doc: /* Non-nil if Tool-Bar mode is enabled.
See the command `tool-bar-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tool-bar-mode'.  */);
#ifdef HAVE_WINDOW_SYSTEM
  Vtool_bar_mode = Qt;
#else
  Vtool_bar_mode = Qnil;
#endif

  DEFVAR_KBOARD ("default-minibuffer-frame", Vdefault_minibuffer_frame,
		 doc: /* Minibufferless frames use this frame's minibuffer.

Emacs cannot create minibufferless frames unless this is set to an
appropriate surrogate.

Emacs consults this variable only when creating minibufferless
frames; once the frame is created, it sticks with its assigned
minibuffer, no matter what this variable is set to.  This means that
this variable doesn't necessarily say anything meaningful about the
current set of frames, or where the minibuffer is currently being
displayed.

This variable is local to the current terminal and cannot be buffer-local.  */);

  DEFVAR_BOOL ("focus-follows-mouse", focus_follows_mouse,
	       doc: /* Non-nil if window system changes focus when you move the mouse.
You should set this variable to tell Emacs how your window manager
handles focus, since there is no way in general for Emacs to find out
automatically.  See also `mouse-autoselect-window'.  */);
  focus_follows_mouse = 0;

  staticpro (&Vframe_list);

  defsubr (&Sframep);
  defsubr (&Sframe_live_p);
  defsubr (&Swindow_system);
  defsubr (&Smake_terminal_frame);
  defsubr (&Shandle_switch_frame);
  defsubr (&Sselect_frame);
  defsubr (&Sselected_frame);
  defsubr (&Sframe_list);
  defsubr (&Snext_frame);
  defsubr (&Sprevious_frame);
  defsubr (&Sdelete_frame);
  defsubr (&Smouse_position);
  defsubr (&Smouse_pixel_position);
  defsubr (&Sset_mouse_position);
  defsubr (&Sset_mouse_pixel_position);
#if 0
  defsubr (&Sframe_configuration);
  defsubr (&Srestore_frame_configuration);
#endif
  defsubr (&Smake_frame_visible);
  defsubr (&Smake_frame_invisible);
  defsubr (&Siconify_frame);
  defsubr (&Sframe_visible_p);
  defsubr (&Svisible_frame_list);
  defsubr (&Sraise_frame);
  defsubr (&Slower_frame);
  defsubr (&Sredirect_frame_focus);
  defsubr (&Sframe_focus);
  defsubr (&Sframe_parameters);
  defsubr (&Sframe_parameter);
  defsubr (&Smodify_frame_parameters);
  defsubr (&Sframe_char_height);
  defsubr (&Sframe_char_width);
  defsubr (&Sframe_pixel_height);
  defsubr (&Sframe_pixel_width);
  defsubr (&Stool_bar_pixel_width);
  defsubr (&Sset_frame_height);
  defsubr (&Sset_frame_width);
  defsubr (&Sset_frame_size);
  defsubr (&Sset_frame_position);
  defsubr (&Sframe_pointer_visible_p);

#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Sx_get_resource);
  defsubr (&Sx_parse_geometry);
#endif

}
