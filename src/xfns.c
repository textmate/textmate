/* Functions for the X window system.

Copyright (C) 1989, 1992-2012  Free Software Foundation, Inc.

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
#include <math.h>
#include <setjmp.h>
#include <ctype.h>
#include <unistd.h>

/* This makes the fields of a Display accessible, in Xlib header files.  */

#define XLIB_ILLEGAL_ACCESS

#include "lisp.h"
#include "xterm.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "intervals.h"
#include "dispextern.h"
#include "keyboard.h"
#include "blockinput.h"
#include <epaths.h>
#include "character.h"
#include "charset.h"
#include "coding.h"
#include "fontset.h"
#include "systime.h"
#include "termhooks.h"
#include "atimer.h"
#include "termchar.h"
#include "font.h"

#ifdef HAVE_X_WINDOWS

#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#if 1 /* Used to be #ifdef EMACS_BITMAP_FILES, but this should always work.  */
#include "bitmaps/gray.xbm"
#else
#include <X11/bitmaps/gray>
#endif

#include "xsettings.h"

#ifdef USE_GTK
#include "gtkutil.h"
#endif

#ifdef USE_X_TOOLKIT
#include <X11/Shell.h>

#ifndef USE_MOTIF
#ifdef HAVE_XAW3D
#include <X11/Xaw3d/Paned.h>
#include <X11/Xaw3d/Label.h>
#else /* !HAVE_XAW3D */
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Label.h>
#endif /* HAVE_XAW3D */
#endif /* USE_MOTIF */

#ifdef USG
#undef USG	/* ####KLUDGE for Solaris 2.2 and up */
#include <X11/Xos.h>
#define USG
#ifdef USG /* Pacify gcc -Wunused-macros.  */
#endif
#else
#include <X11/Xos.h>
#endif

#include "widget.h"

#include "../lwlib/lwlib.h"

#ifdef USE_MOTIF
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/FileSB.h>
#include <Xm/List.h>
#include <Xm/TextF.h>
#endif

#ifdef USE_LUCID
#include "../lwlib/xlwmenu.h"
#endif

#if !defined (NO_EDITRES)
#define HACK_EDITRES
extern void _XEditResCheckMessages (Widget, XtPointer, XEvent *, Boolean *);
#endif /* not defined NO_EDITRES */

/* Unique id counter for widgets created by the Lucid Widget Library.  */

extern LWLIB_ID widget_id_tick;

#ifdef USE_MOTIF

#endif /* USE_MOTIF */

#endif /* USE_X_TOOLKIT */

#ifdef USE_GTK

#endif /* USE_GTK */

#define MAXREQUEST(dpy) (XMaxRequestSize (dpy))

/* The gray bitmap `bitmaps/gray'.  This is done because xterm.c uses
   it, and including `bitmaps/gray' more than once is a problem when
   config.h defines `static' as an empty replacement string.  */

int gray_bitmap_width = gray_width;
int gray_bitmap_height = gray_height;
char *gray_bitmap_bits = gray_bits;

/* Nonzero if using X.  */

static int x_in_use;

static Lisp_Object Qnone;
static Lisp_Object Qsuppress_icon;
static Lisp_Object Qundefined_color;
static Lisp_Object Qcompound_text, Qcancel_timer;
Lisp_Object Qfont_param;

#if GLYPH_DEBUG
static ptrdiff_t image_cache_refcount;
static int dpyinfo_refcount;
#endif

#if defined (USE_GTK) && defined (HAVE_FREETYPE)
static char *x_last_font_name;
#endif

static struct x_display_info *x_display_info_for_name (Lisp_Object);


/* Error if we are not connected to X.  */

void
check_x (void)
{
  if (! x_in_use)
    error ("X windows are not in use or not initialized");
}

/* Nonzero if we can use mouse menus.
   You should not call this unless HAVE_MENUS is defined.  */

int
have_menus_p (void)
{
  return x_in_use;
}

/* Extract a frame as a FRAME_PTR, defaulting to the selected frame
   and checking validity for X.  */

FRAME_PTR
check_x_frame (Lisp_Object frame)
{
  FRAME_PTR f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);
  if (! FRAME_X_P (f))
    error ("Non-X frame used");
  return f;
}

/* Let the user specify an X display with a Lisp object.
   OBJECT may be nil, a frame or a terminal object.
   nil stands for the selected frame--or, if that is not an X frame,
   the first X display on the list.  */

struct x_display_info *
check_x_display_info (Lisp_Object object)
{
  struct x_display_info *dpyinfo = NULL;

  if (NILP (object))
    {
      struct frame *sf = XFRAME (selected_frame);

      if (FRAME_X_P (sf) && FRAME_LIVE_P (sf))
	dpyinfo = FRAME_X_DISPLAY_INFO (sf);
      else if (x_display_list != 0)
	dpyinfo = x_display_list;
      else
	error ("X windows are not in use or not initialized");
    }
  else if (TERMINALP (object))
    {
      struct terminal *t = get_terminal (object, 1);

      if (t->type != output_x_window)
        error ("Terminal %"pI"d is not an X display", XINT (object));

      dpyinfo = t->display_info.x;
    }
  else if (STRINGP (object))
    dpyinfo = x_display_info_for_name (object);
  else
    {
      FRAME_PTR f = check_x_frame (object);
      dpyinfo = FRAME_X_DISPLAY_INFO (f);
    }

  return dpyinfo;
}


/* Return the Emacs frame-object corresponding to an X window.
   It could be the frame's main window or an icon window.  */

/* This function can be called during GC, so use GC_xxx type test macros.  */

struct frame *
x_window_to_frame (struct x_display_info *dpyinfo, int wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f;

  if (wdesc == None) return 0;

  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_X_P (f) || FRAME_X_DISPLAY_INFO (f) != dpyinfo)
	continue;
      if (f->output_data.x->hourglass_window == wdesc)
	return f;
#ifdef USE_X_TOOLKIT
      if ((f->output_data.x->edit_widget
	   && XtWindow (f->output_data.x->edit_widget) == wdesc)
	  /* A tooltip frame?  */
	  || (!f->output_data.x->edit_widget
	      && FRAME_X_WINDOW (f) == wdesc)
          || f->output_data.x->icon_desc == wdesc)
        return f;
#else /* not USE_X_TOOLKIT */
#ifdef USE_GTK
      if (f->output_data.x->edit_widget)
      {
        GtkWidget *gwdesc = xg_win_to_widget (dpyinfo->display, wdesc);
        struct x_output *x = f->output_data.x;
        if (gwdesc != 0 && gwdesc == x->edit_widget)
          return f;
      }
#endif /* USE_GTK */
      if (FRAME_X_WINDOW (f) == wdesc
          || f->output_data.x->icon_desc == wdesc)
        return f;
#endif /* not USE_X_TOOLKIT */
    }
  return 0;
}

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
/* Like x_window_to_frame but also compares the window with the widget's
   windows.  */

struct frame *
x_any_window_to_frame (struct x_display_info *dpyinfo, int wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f, *found;
  struct x_output *x;

  if (wdesc == None) return NULL;

  found = NULL;
  for (tail = Vframe_list; CONSP (tail) && !found; tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
        continue;

      f = XFRAME (frame);
      if (FRAME_X_P (f) && FRAME_X_DISPLAY_INFO (f) == dpyinfo)
	{
	  /* This frame matches if the window is any of its widgets.  */
	  x = f->output_data.x;
	  if (x->hourglass_window == wdesc)
	    found = f;
	  else if (x->widget)
	    {
#ifdef USE_GTK
              GtkWidget *gwdesc = xg_win_to_widget (dpyinfo->display, wdesc);
              if (gwdesc != 0
                  && gtk_widget_get_toplevel (gwdesc) == x->widget)
                found = f;
#else
	      if (wdesc == XtWindow (x->widget)
		  || wdesc == XtWindow (x->column_widget)
		  || wdesc == XtWindow (x->edit_widget))
		found = f;
	      /* Match if the window is this frame's menubar.  */
	      else if (lw_window_is_in_menubar (wdesc, x->menubar_widget))
		found = f;
#endif
	    }
	  else if (FRAME_X_WINDOW (f) == wdesc)
	    /* A tooltip frame.  */
	    found = f;
	}
    }

  return found;
}

/* Likewise, but consider only the menu bar widget.  */

struct frame *
x_menubar_window_to_frame (struct x_display_info *dpyinfo, XEvent *event)
{
  Window wdesc = event->xany.window;
  Lisp_Object tail, frame;
  struct frame *f;
  struct x_output *x;

  if (wdesc == None) return 0;

  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_X_P (f) || FRAME_X_DISPLAY_INFO (f) != dpyinfo)
	continue;
      x = f->output_data.x;
#ifdef USE_GTK
      if (x->menubar_widget && xg_event_is_for_menubar (f, event))
        return f;
#else
      /* Match if the window is this frame's menubar.  */
      if (x->menubar_widget
	  && lw_window_is_in_menubar (wdesc, x->menubar_widget))
	return f;
#endif
    }
  return 0;
}

/* Return the frame whose principal (outermost) window is WDESC.
   If WDESC is some other (smaller) window, we return 0.  */

struct frame *
x_top_window_to_frame (struct x_display_info *dpyinfo, int wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f;
  struct x_output *x;

  if (wdesc == None) return 0;

  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_X_P (f) || FRAME_X_DISPLAY_INFO (f) != dpyinfo)
	continue;
      x = f->output_data.x;

      if (x->widget)
	{
	  /* This frame matches if the window is its topmost widget.  */
#ifdef USE_GTK
          GtkWidget *gwdesc = xg_win_to_widget (dpyinfo->display, wdesc);
          if (gwdesc == x->widget)
            return f;
#else
	  if (wdesc == XtWindow (x->widget))
	    return f;
#if 0 /* I don't know why it did this,
	 but it seems logically wrong,
	 and it causes trouble for MapNotify events.  */
	  /* Match if the window is this frame's menubar.  */
	  if (x->menubar_widget
	      && wdesc == XtWindow (x->menubar_widget))
	    return f;
#endif
#endif
	}
      else if (FRAME_X_WINDOW (f) == wdesc)
	/* Tooltip frame.  */
	return f;
    }
  return 0;
}
#endif /* USE_X_TOOLKIT || USE_GTK */



/* Store the screen positions of frame F into XPTR and YPTR.
   These are the positions of the containing window manager window,
   not Emacs's own window.  */

void
x_real_positions (FRAME_PTR f, int *xptr, int *yptr)
{
  int win_x, win_y, outer_x IF_LINT (= 0), outer_y IF_LINT (= 0);
  int real_x = 0, real_y = 0;
  int had_errors = 0;
  Window win = f->output_data.x->parent_desc;
  Atom actual_type;
  unsigned long actual_size, bytes_remaining;
  int rc, actual_format;
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  long max_len = 400;
  Display *dpy = FRAME_X_DISPLAY (f);
  unsigned char *tmp_data = NULL;
  Atom target_type = XA_CARDINAL;

  BLOCK_INPUT;

  x_catch_errors (dpy);

  if (win == dpyinfo->root_window)
    win = FRAME_OUTER_WINDOW (f);

  /* This loop traverses up the containment tree until we hit the root
     window.  Window managers may intersect many windows between our window
     and the root window.  The window we find just before the root window
     should be the outer WM window. */
  for (;;)
    {
      Window wm_window, rootw;
      Window *tmp_children;
      unsigned int tmp_nchildren;
      int success;

      success = XQueryTree (FRAME_X_DISPLAY (f), win, &rootw,
			    &wm_window, &tmp_children, &tmp_nchildren);

      had_errors = x_had_errors_p (FRAME_X_DISPLAY (f));

      /* Don't free tmp_children if XQueryTree failed.  */
      if (! success)
	break;

      XFree ((char *) tmp_children);

      if (wm_window == rootw || had_errors)
        break;

      win = wm_window;
    }

  if (! had_errors)
    {
      unsigned int ign;
      Window child, rootw;

      /* Get the real coordinates for the WM window upper left corner */
      XGetGeometry (FRAME_X_DISPLAY (f), win,
                    &rootw, &real_x, &real_y, &ign, &ign, &ign, &ign);

      /* Translate real coordinates to coordinates relative to our
         window.  For our window, the upper left corner is 0, 0.
         Since the upper left corner of the WM window is outside
         our window, win_x and win_y will be negative:

         ------------------          ---> x
         |      title                |
         | -----------------         v y
         | |  our window
      */
      XTranslateCoordinates (FRAME_X_DISPLAY (f),

			     /* From-window, to-window.  */
			     FRAME_X_DISPLAY_INFO (f)->root_window,
                             FRAME_X_WINDOW (f),

			     /* From-position, to-position.  */
                             real_x, real_y, &win_x, &win_y,

			     /* Child of win.  */
			     &child);

      if (FRAME_X_WINDOW (f) == FRAME_OUTER_WINDOW (f))
	{
          outer_x = win_x;
          outer_y = win_y;
	}
      else
        {
          XTranslateCoordinates (FRAME_X_DISPLAY (f),

                                 /* From-window, to-window.  */
                                 FRAME_X_DISPLAY_INFO (f)->root_window,
                                 FRAME_OUTER_WINDOW (f),

                                 /* From-position, to-position.  */
                                 real_x, real_y, &outer_x, &outer_y,

                                 /* Child of win.  */
                                 &child);
	}

      had_errors = x_had_errors_p (FRAME_X_DISPLAY (f));
    }


  if (dpyinfo->root_window == f->output_data.x->parent_desc)
    {
      /* Try _NET_FRAME_EXTENTS if our parent is the root window.  */
      rc = XGetWindowProperty (dpy, win, dpyinfo->Xatom_net_frame_extents,
                               0, max_len, False, target_type,
                               &actual_type, &actual_format, &actual_size,
                               &bytes_remaining, &tmp_data);

      if (rc == Success && actual_type == target_type && !x_had_errors_p (dpy)
          && actual_size == 4 && actual_format == 32)
        {
          unsigned int ign;
          Window rootw;
          long *fe = (long *)tmp_data;

          XGetGeometry (FRAME_X_DISPLAY (f), win,
                        &rootw, &real_x, &real_y, &ign, &ign, &ign, &ign);
          outer_x = -fe[0];
          outer_y = -fe[2];
          real_x -= fe[0];
          real_y -= fe[2];
        }
    }

  if (tmp_data) XFree (tmp_data);

  x_uncatch_errors ();

  UNBLOCK_INPUT;

  if (had_errors) return;

  f->x_pixels_diff = -win_x;
  f->y_pixels_diff = -win_y;

  FRAME_X_OUTPUT (f)->x_pixels_outer_diff = -outer_x;
  FRAME_X_OUTPUT (f)->y_pixels_outer_diff = -outer_y;

  *xptr = real_x;
  *yptr = real_y;
}




/* Gamma-correct COLOR on frame F.  */

void
gamma_correct (struct frame *f, XColor *color)
{
  if (f->gamma)
    {
      color->red = pow (color->red / 65535.0, f->gamma) * 65535.0 + 0.5;
      color->green = pow (color->green / 65535.0, f->gamma) * 65535.0 + 0.5;
      color->blue = pow (color->blue / 65535.0, f->gamma) * 65535.0 + 0.5;
    }
}


/* Decide if color named COLOR_NAME is valid for use on frame F.  If
   so, return the RGB values in COLOR.  If ALLOC_P is non-zero,
   allocate the color.  Value is zero if COLOR_NAME is invalid, or
   no color could be allocated.  */

int
x_defined_color (struct frame *f, const char *color_name,
		 XColor *color, int alloc_p)
{
  int success_p = 0;
  Display *dpy = FRAME_X_DISPLAY (f);
  Colormap cmap = FRAME_X_COLORMAP (f);

  BLOCK_INPUT;
#ifdef USE_GTK
  success_p = xg_check_special_colors (f, color_name, color);
#endif
  if (!success_p)
    success_p = XParseColor (dpy, cmap, color_name, color);
  if (success_p && alloc_p)
    success_p = x_alloc_nearest_color (f, cmap, color);
  UNBLOCK_INPUT;

  return success_p;
}


/* Return the pixel color value for color COLOR_NAME on frame F.  If F
   is a monochrome frame, return MONO_COLOR regardless of what ARG says.
   Signal an error if color can't be allocated.  */

static int
x_decode_color (FRAME_PTR f, Lisp_Object color_name, int mono_color)
{
  XColor cdef;

  CHECK_STRING (color_name);

#if 0 /* Don't do this.  It's wrong when we're not using the default
	 colormap, it makes freeing difficult, and it's probably not
	 an important optimization.  */
  if (strcmp (SDATA (color_name), "black") == 0)
    return BLACK_PIX_DEFAULT (f);
  else if (strcmp (SDATA (color_name), "white") == 0)
    return WHITE_PIX_DEFAULT (f);
#endif

  /* Return MONO_COLOR for monochrome frames.  */
  if (FRAME_X_DISPLAY_INFO (f)->n_planes == 1)
    return mono_color;

  /* x_defined_color is responsible for coping with failures
     by looking for a near-miss.  */
  if (x_defined_color (f, SSDATA (color_name), &cdef, 1))
    return cdef.pixel;

  signal_error ("Undefined color", color_name);
}



/* Change the `wait-for-wm' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value.
   See also the comment of wait_for_wm in struct x_output.  */

static void
x_set_wait_for_wm (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  f->output_data.x->wait_for_wm = !NILP (new_value);
}

static void
x_set_tool_bar_position (struct frame *f,
                         Lisp_Object new_value,
                         Lisp_Object old_value)
{
  if (! EQ (new_value, Qleft) && ! EQ (new_value, Qright)
      && ! EQ (new_value, Qbottom) && ! EQ (new_value, Qtop))
    return;
  if (EQ (new_value, old_value)) return;

#ifdef USE_GTK
  if (xg_change_toolbar_position (f, new_value))
    f->tool_bar_position = new_value;
#endif
}

#ifdef USE_GTK

/* Set icon from FILE for frame F.  By using GTK functions the icon
   may be any format that GdkPixbuf knows about, i.e. not just bitmaps.  */

int
xg_set_icon (FRAME_PTR f, Lisp_Object file)
{
  int result = 0;
  Lisp_Object found;

  found = x_find_image_file (file);

  if (! NILP (found))
    {
      GdkPixbuf *pixbuf;
      GError *err = NULL;
      char *filename = SSDATA (found);
      BLOCK_INPUT;

      pixbuf = gdk_pixbuf_new_from_file (filename, &err);

      if (pixbuf)
	{
	  gtk_window_set_icon (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
			       pixbuf);
	  g_object_unref (pixbuf);

	  result = 1;
	}
      else
	g_error_free (err);

      UNBLOCK_INPUT;
    }

  return result;
}

int
xg_set_icon_from_xpm_data (FRAME_PTR f, const char **data)
{
  GdkPixbuf *pixbuf = gdk_pixbuf_new_from_xpm_data (data);

  if (!pixbuf)
    return 0;

  gtk_window_set_icon (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), pixbuf);
  g_object_unref (pixbuf);
  return 1;
}
#endif /* USE_GTK */


/* Functions called only from `x_set_frame_param'
   to set individual parameters.

   If FRAME_X_WINDOW (f) is 0,
   the frame is being created and its X-window does not exist yet.
   In that case, just record the parameter's new value
   in the standard place; do not attempt to change the window.  */

static void
x_set_foreground_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  struct x_output *x = f->output_data.x;
  unsigned long fg, old_fg;

  fg = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  old_fg = FRAME_FOREGROUND_PIXEL (f);
  FRAME_FOREGROUND_PIXEL (f) = fg;

  if (FRAME_X_WINDOW (f) != 0)
    {
      Display *dpy = FRAME_X_DISPLAY (f);

      BLOCK_INPUT;
      XSetForeground (dpy, x->normal_gc, fg);
      XSetBackground (dpy, x->reverse_gc, fg);

      if (x->cursor_pixel == old_fg)
	{
	  unload_color (f, x->cursor_pixel);
	  x->cursor_pixel = x_copy_color (f, fg);
	  XSetBackground (dpy, x->cursor_gc, x->cursor_pixel);
	}

      UNBLOCK_INPUT;

      update_face_from_frame_parameter (f, Qforeground_color, arg);

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }

  unload_color (f, old_fg);
}

static void
x_set_background_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  struct x_output *x = f->output_data.x;
  unsigned long bg;

  bg = x_decode_color (f, arg, WHITE_PIX_DEFAULT (f));
  unload_color (f, FRAME_BACKGROUND_PIXEL (f));
  FRAME_BACKGROUND_PIXEL (f) = bg;

  if (FRAME_X_WINDOW (f) != 0)
    {
      Display *dpy = FRAME_X_DISPLAY (f);

      BLOCK_INPUT;
      XSetBackground (dpy, x->normal_gc, bg);
      XSetForeground (dpy, x->reverse_gc, bg);
      XSetWindowBackground (dpy, FRAME_X_WINDOW (f), bg);
      XSetForeground (dpy, x->cursor_gc, bg);

#ifdef USE_GTK
      xg_set_background_color (f, bg);
#endif

#ifndef USE_TOOLKIT_SCROLL_BARS /* Turns out to be annoying with
				   toolkit scroll bars.  */
      {
	Lisp_Object bar;
	for (bar = FRAME_SCROLL_BARS (f);
	     !NILP (bar);
	     bar = XSCROLL_BAR (bar)->next)
	  {
	    Window window = XSCROLL_BAR (bar)->x_window;
	    XSetWindowBackground (dpy, window, bg);
	  }
      }
#endif /* USE_TOOLKIT_SCROLL_BARS */

      UNBLOCK_INPUT;
      update_face_from_frame_parameter (f, Qbackground_color, arg);

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

static Cursor
make_invisible_cursor (struct frame *f)
{
  Display *dpy = FRAME_X_DISPLAY (f);
  static char const no_data[] = { 0 };
  Pixmap pix;
  XColor col;
  Cursor c = 0;

  x_catch_errors (dpy);
  pix = XCreateBitmapFromData (dpy, FRAME_X_DISPLAY_INFO (f)->root_window,
                               no_data, 1, 1);
  if (! x_had_errors_p (dpy) && pix != None)
    {
      Cursor pixc;
      col.pixel = 0;
      col.red = col.green = col.blue = 0;
      col.flags = DoRed | DoGreen | DoBlue;
      pixc = XCreatePixmapCursor (dpy, pix, pix, &col, &col, 0, 0);
      if (! x_had_errors_p (dpy) && pixc != None)
        c = pixc;
      XFreePixmap (dpy, pix);
    }

  x_uncatch_errors ();

  return c;
}

static void
x_set_mouse_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  struct x_output *x = f->output_data.x;
  Display *dpy = FRAME_X_DISPLAY (f);
  Cursor cursor, nontext_cursor, mode_cursor, hand_cursor;
  Cursor hourglass_cursor, horizontal_drag_cursor;
  unsigned long pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  unsigned long mask_color = FRAME_BACKGROUND_PIXEL (f);

  /* Don't let pointers be invisible.  */
  if (mask_color == pixel)
    {
      x_free_colors (f, &pixel, 1);
      pixel = x_copy_color (f, FRAME_FOREGROUND_PIXEL (f));
    }

  unload_color (f, x->mouse_pixel);
  x->mouse_pixel = pixel;

  BLOCK_INPUT;

  /* It's not okay to crash if the user selects a screwy cursor.  */
  x_catch_errors (dpy);

  if (!NILP (Vx_pointer_shape))
    {
      CHECK_NUMBER (Vx_pointer_shape);
      cursor = XCreateFontCursor (dpy, XINT (Vx_pointer_shape));
    }
  else
    cursor = XCreateFontCursor (dpy, XC_xterm);
  x_check_errors (dpy, "bad text pointer cursor: %s");

  if (!NILP (Vx_nontext_pointer_shape))
    {
      CHECK_NUMBER (Vx_nontext_pointer_shape);
      nontext_cursor
	= XCreateFontCursor (dpy, XINT (Vx_nontext_pointer_shape));
    }
  else
    nontext_cursor = XCreateFontCursor (dpy, XC_left_ptr);
  x_check_errors (dpy, "bad nontext pointer cursor: %s");

  if (!NILP (Vx_hourglass_pointer_shape))
    {
      CHECK_NUMBER (Vx_hourglass_pointer_shape);
      hourglass_cursor
	= XCreateFontCursor (dpy, XINT (Vx_hourglass_pointer_shape));
    }
  else
    hourglass_cursor = XCreateFontCursor (dpy, XC_watch);
  x_check_errors (dpy, "bad hourglass pointer cursor: %s");

  if (!NILP (Vx_mode_pointer_shape))
    {
      CHECK_NUMBER (Vx_mode_pointer_shape);
      mode_cursor = XCreateFontCursor (dpy, XINT (Vx_mode_pointer_shape));
    }
  else
    mode_cursor = XCreateFontCursor (dpy, XC_xterm);
  x_check_errors (dpy, "bad modeline pointer cursor: %s");

  if (!NILP (Vx_sensitive_text_pointer_shape))
    {
      CHECK_NUMBER (Vx_sensitive_text_pointer_shape);
      hand_cursor
	= XCreateFontCursor (dpy, XINT (Vx_sensitive_text_pointer_shape));
    }
  else
    hand_cursor = XCreateFontCursor (dpy, XC_hand2);

  if (!NILP (Vx_window_horizontal_drag_shape))
    {
      CHECK_NUMBER (Vx_window_horizontal_drag_shape);
      horizontal_drag_cursor
	= XCreateFontCursor (dpy, XINT (Vx_window_horizontal_drag_shape));
    }
  else
    horizontal_drag_cursor
      = XCreateFontCursor (dpy, XC_sb_h_double_arrow);

  /* Check and report errors with the above calls.  */
  x_check_errors (dpy, "can't set cursor shape: %s");
  x_uncatch_errors ();

  {
    XColor fore_color, back_color;

    fore_color.pixel = x->mouse_pixel;
    x_query_color (f, &fore_color);
    back_color.pixel = mask_color;
    x_query_color (f, &back_color);

    XRecolorCursor (dpy, cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, nontext_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, mode_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, hand_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, hourglass_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, horizontal_drag_cursor, &fore_color, &back_color);
  }

  if (FRAME_X_WINDOW (f) != 0)
    XDefineCursor (dpy, FRAME_X_WINDOW (f),
                   f->output_data.x->current_cursor = cursor);

  if (FRAME_X_DISPLAY_INFO (f)->invisible_cursor == 0)
    FRAME_X_DISPLAY_INFO (f)->invisible_cursor = make_invisible_cursor (f);

  if (cursor != x->text_cursor
      && x->text_cursor != 0)
    XFreeCursor (dpy, x->text_cursor);
  x->text_cursor = cursor;

  if (nontext_cursor != x->nontext_cursor
      && x->nontext_cursor != 0)
    XFreeCursor (dpy, x->nontext_cursor);
  x->nontext_cursor = nontext_cursor;

  if (hourglass_cursor != x->hourglass_cursor
      && x->hourglass_cursor != 0)
    XFreeCursor (dpy, x->hourglass_cursor);
  x->hourglass_cursor = hourglass_cursor;

  if (mode_cursor != x->modeline_cursor
      && x->modeline_cursor != 0)
    XFreeCursor (dpy, f->output_data.x->modeline_cursor);
  x->modeline_cursor = mode_cursor;

  if (hand_cursor != x->hand_cursor
      && x->hand_cursor != 0)
    XFreeCursor (dpy, x->hand_cursor);
  x->hand_cursor = hand_cursor;

  if (horizontal_drag_cursor != x->horizontal_drag_cursor
      && x->horizontal_drag_cursor != 0)
    XFreeCursor (dpy, x->horizontal_drag_cursor);
  x->horizontal_drag_cursor = horizontal_drag_cursor;

  XFlush (dpy);
  UNBLOCK_INPUT;

  update_face_from_frame_parameter (f, Qmouse_color, arg);
}

static void
x_set_cursor_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  unsigned long fore_pixel, pixel;
  int fore_pixel_allocated_p = 0, pixel_allocated_p = 0;
  struct x_output *x = f->output_data.x;

  if (!NILP (Vx_cursor_fore_pixel))
    {
      fore_pixel = x_decode_color (f, Vx_cursor_fore_pixel,
				   WHITE_PIX_DEFAULT (f));
      fore_pixel_allocated_p = 1;
    }
  else
    fore_pixel = FRAME_BACKGROUND_PIXEL (f);

  pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  pixel_allocated_p = 1;

  /* Make sure that the cursor color differs from the background color.  */
  if (pixel == FRAME_BACKGROUND_PIXEL (f))
    {
      if (pixel_allocated_p)
	{
	  x_free_colors (f, &pixel, 1);
	  pixel_allocated_p = 0;
	}

      pixel = x->mouse_pixel;
      if (pixel == fore_pixel)
	{
	  if (fore_pixel_allocated_p)
	    {
	      x_free_colors (f, &fore_pixel, 1);
	      fore_pixel_allocated_p = 0;
	    }
	  fore_pixel = FRAME_BACKGROUND_PIXEL (f);
	}
    }

  unload_color (f, x->cursor_foreground_pixel);
  if (!fore_pixel_allocated_p)
    fore_pixel = x_copy_color (f, fore_pixel);
  x->cursor_foreground_pixel = fore_pixel;

  unload_color (f, x->cursor_pixel);
  if (!pixel_allocated_p)
    pixel = x_copy_color (f, pixel);
  x->cursor_pixel = pixel;

  if (FRAME_X_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
      XSetBackground (FRAME_X_DISPLAY (f), x->cursor_gc, x->cursor_pixel);
      XSetForeground (FRAME_X_DISPLAY (f), x->cursor_gc, fore_pixel);
      UNBLOCK_INPUT;

      if (FRAME_VISIBLE_P (f))
	{
	  x_update_cursor (f, 0);
	  x_update_cursor (f, 1);
	}
    }

  update_face_from_frame_parameter (f, Qcursor_color, arg);
}

/* Set the border-color of frame F to pixel value PIX.
   Note that this does not fully take effect if done before
   F has an x-window.  */

static void
x_set_border_pixel (struct frame *f, int pix)
{
  unload_color (f, f->output_data.x->border_pixel);
  f->output_data.x->border_pixel = pix;

  if (FRAME_X_WINDOW (f) != 0 && f->border_width > 0)
    {
      BLOCK_INPUT;
      XSetWindowBorder (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), pix);
      UNBLOCK_INPUT;

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

/* Set the border-color of frame F to value described by ARG.
   ARG can be a string naming a color.
   The border-color is used for the border that is drawn by the X server.
   Note that this does not fully take effect if done before
   F has an x-window; it must be redone when the window is created.

   Note: this is done in two routines because of the way X10 works.

   Note: under X11, this is normally the province of the window manager,
   and so emacs' border colors may be overridden.  */

static void
x_set_border_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int pix;

  CHECK_STRING (arg);
  pix = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  x_set_border_pixel (f, pix);
  update_face_from_frame_parameter (f, Qborder_color, arg);
}


static void
x_set_cursor_type (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  set_frame_cursor_types (f, arg);

  /* Make sure the cursor gets redrawn.  */
  cursor_type_changed = 1;
}

static void
x_set_icon_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int result;

  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && EQ (Fstring_equal (oldval, arg), Qt))
	return;
    }
  else if (!STRINGP (oldval) && EQ (oldval, Qnil) == EQ (arg, Qnil))
    return;

  BLOCK_INPUT;
  if (NILP (arg))
    result = x_text_icon (f,
			  SSDATA ((!NILP (f->icon_name)
				   ? f->icon_name
				   : f->name)));
  else
    result = x_bitmap_icon (f, arg);

  if (result)
    {
      UNBLOCK_INPUT;
      error ("No icon window available");
    }

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
}

static void
x_set_icon_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int result;

  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && EQ (Fstring_equal (oldval, arg), Qt))
	return;
    }
  else if (!NILP (arg) || NILP (oldval))
    return;

  f->icon_name = arg;

  if (f->output_data.x->icon_bitmap != 0)
    return;

  BLOCK_INPUT;

  result = x_text_icon (f,
			SSDATA ((!NILP (f->icon_name)
				 ? f->icon_name
				 : !NILP (f->title)
				 ? f->title
				 : f->name)));

  if (result)
    {
      UNBLOCK_INPUT;
      error ("No icon window available");
    }

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
}


void
x_set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;
#if ! defined (USE_X_TOOLKIT) && ! defined (USE_GTK)
  int olines = FRAME_MENU_BAR_LINES (f);
#endif

  /* Right now, menu bars don't work properly in minibuf-only frames;
     most of the commands try to apply themselves to the minibuffer
     frame itself, and get an error because you can't switch buffers
     in or split the minibuffer window.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (INTEGERP (value))
    nlines = XINT (value);
  else
    nlines = 0;

  /* Make sure we redisplay all windows in this frame.  */
  windows_or_buffers_changed++;

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
  FRAME_MENU_BAR_LINES (f) = 0;
  if (nlines)
    {
      FRAME_EXTERNAL_MENU_BAR (f) = 1;
      if (FRAME_X_P (f) && f->output_data.x->menubar_widget == 0)
	/* Make sure next redisplay shows the menu bar.  */
	XWINDOW (FRAME_SELECTED_WINDOW (f))->update_mode_line = Qt;
    }
  else
    {
      if (FRAME_EXTERNAL_MENU_BAR (f) == 1)
	free_frame_menubar (f);
      FRAME_EXTERNAL_MENU_BAR (f) = 0;
      if (FRAME_X_P (f))
	f->output_data.x->menubar_widget = 0;
    }
#else /* not USE_X_TOOLKIT && not USE_GTK */
  FRAME_MENU_BAR_LINES (f) = nlines;
  resize_frame_windows (f, FRAME_LINES (f), 0);

  /* If the menu bar height gets changed, the internal border below
     the top margin has to be cleared.  Also, if the menu bar gets
     larger, the area for the added lines has to be cleared except for
     the first menu bar line that is to be drawn later.  */
  if (nlines != olines)
    {
      int height = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = FRAME_PIXEL_WIDTH (f);
      int y;

      /* height can be zero here. */
      if (height > 0 && width > 0)
	{
	  y = FRAME_TOP_MARGIN_HEIGHT (f);

	  BLOCK_INPUT;
	  x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			0, y, width, height, False);
	  UNBLOCK_INPUT;
	}

      if (nlines > 1 && nlines > olines)
	{
	  y = (olines == 0 ? 1 : olines) * FRAME_LINE_HEIGHT (f);
	  height = nlines * FRAME_LINE_HEIGHT (f) - y;

	  BLOCK_INPUT;
	  x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			0, y, width, height, False);
	  UNBLOCK_INPUT;
	}

      if (nlines == 0 && WINDOWP (f->menu_bar_window))
	clear_glyph_matrix (XWINDOW (f->menu_bar_window)->current_matrix);
    }
#endif /* not USE_X_TOOLKIT && not USE_GTK */
  adjust_glyphs (f);
  run_window_configuration_change_hook (f);
}


/* Set the number of lines used for the tool bar of frame F to VALUE.
   VALUE not an integer, or < 0 means set the lines to zero.  OLDVAL
   is the old number of tool bar lines.  This function changes the
   height of all windows on frame F to match the new tool bar height.
   The frame's height doesn't change.  */

void
x_set_tool_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int delta, nlines, root_height;
  Lisp_Object root_window;

  /* Treat tool bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an integer >= 0.  */
  if (INTEGERP (value) && XINT (value) >= 0)
    nlines = XFASTINT (value);
  else
    nlines = 0;

#ifdef USE_GTK
  FRAME_TOOL_BAR_LINES (f) = 0;
  if (nlines)
    {
      FRAME_EXTERNAL_TOOL_BAR (f) = 1;
      if (FRAME_X_P (f) && f->output_data.x->toolbar_widget == 0)
	/* Make sure next redisplay shows the tool bar.  */
	XWINDOW (FRAME_SELECTED_WINDOW (f))->update_mode_line = Qt;
      update_frame_tool_bar (f);
    }
  else
    {
      if (FRAME_EXTERNAL_TOOL_BAR (f))
        free_frame_tool_bar (f);
      FRAME_EXTERNAL_TOOL_BAR (f) = 0;
    }

  return;
#endif

     /* Make sure we redisplay all windows in this frame.  */
  ++windows_or_buffers_changed;

  delta = nlines - FRAME_TOOL_BAR_LINES (f);

  /* Don't resize the tool-bar to more than we have room for.  */
  root_window = FRAME_ROOT_WINDOW (f);
  root_height = WINDOW_TOTAL_LINES (XWINDOW (root_window));
  if (root_height - delta < 1)
    {
      delta = root_height - 1;
      nlines = FRAME_TOOL_BAR_LINES (f) + delta;
    }

  FRAME_TOOL_BAR_LINES (f) = nlines;
  resize_frame_windows (f, FRAME_LINES (f), 0);
  adjust_glyphs (f);

  /* We also have to make sure that the internal border at the top of
     the frame, below the menu bar or tool bar, is redrawn when the
     tool bar disappears.  This is so because the internal border is
     below the tool bar if one is displayed, but is below the menu bar
     if there isn't a tool bar.  The tool bar draws into the area
     below the menu bar.  */
  if (FRAME_X_WINDOW (f) && FRAME_TOOL_BAR_LINES (f) == 0)
    {
      clear_frame (f);
      clear_current_matrices (f);
    }

  /* If the tool bar gets smaller, the internal border below it
     has to be cleared.  It was formerly part of the display
     of the larger tool bar, and updating windows won't clear it.  */
  if (delta < 0)
    {
      int height = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = FRAME_PIXEL_WIDTH (f);
      int y = (FRAME_MENU_BAR_LINES (f) + nlines) * FRAME_LINE_HEIGHT (f);

      /* height can be zero here. */
      if (height > 0 && width > 0)
	{
          BLOCK_INPUT;
          x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
                        0, y, width, height, False);
          UNBLOCK_INPUT;
        }

      if (WINDOWP (f->tool_bar_window))
	clear_glyph_matrix (XWINDOW (f->tool_bar_window)->current_matrix);
    }

    run_window_configuration_change_hook (f);

}


/* Set the foreground color for scroll bars on frame F to VALUE.
   VALUE should be a string, a color name.  If it isn't a string or
   isn't a valid color name, do nothing.  OLDVAL is the old value of
   the frame parameter.  */

static void
x_set_scroll_bar_foreground (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  unsigned long pixel;

  if (STRINGP (value))
    pixel = x_decode_color (f, value, BLACK_PIX_DEFAULT (f));
  else
    pixel = -1;

  if (f->output_data.x->scroll_bar_foreground_pixel != -1)
    unload_color (f, f->output_data.x->scroll_bar_foreground_pixel);

  f->output_data.x->scroll_bar_foreground_pixel = pixel;
  if (FRAME_X_WINDOW (f) && FRAME_VISIBLE_P (f))
    {
      /* Remove all scroll bars because they have wrong colors.  */
      if (FRAME_TERMINAL (f)->condemn_scroll_bars_hook)
	(*FRAME_TERMINAL (f)->condemn_scroll_bars_hook) (f);
      if (FRAME_TERMINAL (f)->judge_scroll_bars_hook)
	(*FRAME_TERMINAL (f)->judge_scroll_bars_hook) (f);

      update_face_from_frame_parameter (f, Qscroll_bar_foreground, value);
      redraw_frame (f);
    }
}


/* Set the background color for scroll bars on frame F to VALUE VALUE
   should be a string, a color name.  If it isn't a string or isn't a
   valid color name, do nothing.  OLDVAL is the old value of the frame
   parameter.  */

static void
x_set_scroll_bar_background (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  unsigned long pixel;

  if (STRINGP (value))
    pixel = x_decode_color (f, value, WHITE_PIX_DEFAULT (f));
  else
    pixel = -1;

  if (f->output_data.x->scroll_bar_background_pixel != -1)
    unload_color (f, f->output_data.x->scroll_bar_background_pixel);

#ifdef USE_TOOLKIT_SCROLL_BARS
  /* Scrollbar shadow colors.  */
  if (f->output_data.x->scroll_bar_top_shadow_pixel != -1)
    {
      unload_color (f, f->output_data.x->scroll_bar_top_shadow_pixel);
      f->output_data.x->scroll_bar_top_shadow_pixel = -1;
    }
  if (f->output_data.x->scroll_bar_bottom_shadow_pixel != -1)
    {
      unload_color (f, f->output_data.x->scroll_bar_bottom_shadow_pixel);
      f->output_data.x->scroll_bar_bottom_shadow_pixel = -1;
    }
#endif /* USE_TOOLKIT_SCROLL_BARS */

  f->output_data.x->scroll_bar_background_pixel = pixel;
  if (FRAME_X_WINDOW (f) && FRAME_VISIBLE_P (f))
    {
      /* Remove all scroll bars because they have wrong colors.  */
      if (FRAME_TERMINAL (f)->condemn_scroll_bars_hook)
	(*FRAME_TERMINAL (f)->condemn_scroll_bars_hook) (f);
      if (FRAME_TERMINAL (f)->judge_scroll_bars_hook)
	(*FRAME_TERMINAL (f)->judge_scroll_bars_hook) (f);

      update_face_from_frame_parameter (f, Qscroll_bar_background, value);
      redraw_frame (f);
    }
}


/* Encode Lisp string STRING as a text in a format appropriate for
   XICCC (X Inter Client Communication Conventions).

   This can call Lisp code, so callers must GCPRO.

   If STRING contains only ASCII characters, do no conversion and
   return the string data of STRING.  Otherwise, encode the text by
   CODING_SYSTEM, and return a newly allocated memory area which
   should be freed by `xfree' by a caller.

   SELECTIONP non-zero means the string is being encoded for an X
   selection, so it is safe to run pre-write conversions (which
   may run Lisp code).

   Store the byte length of resulting text in *TEXT_BYTES.

   If the text contains only ASCII and Latin-1, store 1 in *STRING_P,
   which means that the `encoding' of the result can be `STRING'.
   Otherwise store 0 in *STRINGP, which means that the `encoding' of
   the result should be `COMPOUND_TEXT'.  */

static unsigned char *
x_encode_text (Lisp_Object string, Lisp_Object coding_system, int selectionp,
	       ptrdiff_t *text_bytes, int *stringp, int *freep)
{
  int result = string_xstring_p (string);
  struct coding_system coding;

  if (result == 0)
    {
      /* No multibyte character in OBJ.  We need not encode it.  */
      *text_bytes = SBYTES (string);
      *stringp = 1;
      *freep = 0;
      return SDATA (string);
    }

  setup_coding_system (coding_system, &coding);
  coding.mode |= (CODING_MODE_SAFE_ENCODING | CODING_MODE_LAST_BLOCK);
  /* We suppress producing escape sequences for composition.  */
  coding.common_flags &= ~CODING_ANNOTATION_MASK;
  coding.destination = xnmalloc (SCHARS (string), 2);
  coding.dst_bytes = SCHARS (string) * 2;
  encode_coding_object (&coding, string, 0, 0,
			SCHARS (string), SBYTES (string), Qnil);
  *text_bytes = coding.produced;
  *stringp = (result == 1 || !EQ (coding_system, Qcompound_text));
  *freep = 1;
  return coding.destination;
}


/* Set the WM name to NAME for frame F. Also set the icon name.
   If the frame already has an icon name, use that, otherwise set the
   icon name to NAME.  */

static void
x_set_name_internal (FRAME_PTR f, Lisp_Object name)
{
  if (FRAME_X_WINDOW (f))
    {
      BLOCK_INPUT;
      {
	XTextProperty text, icon;
	ptrdiff_t bytes;
	int stringp;
        int do_free_icon_value = 0, do_free_text_value = 0;
	Lisp_Object coding_system;
	Lisp_Object encoded_name;
	Lisp_Object encoded_icon_name;
	struct gcpro gcpro1;

	/* As ENCODE_UTF_8 may cause GC and relocation of string data,
	   we use it before x_encode_text that may return string data.  */
	GCPRO1 (name);
	encoded_name = ENCODE_UTF_8 (name);
	UNGCPRO;

	coding_system = Qcompound_text;
	/* Note: Encoding strategy

	   We encode NAME by compound-text and use "COMPOUND-TEXT" in
	   text.encoding.  But, there are non-internationalized window
	   managers which don't support that encoding.  So, if NAME
	   contains only ASCII and 8859-1 characters, encode it by
	   iso-latin-1, and use "STRING" in text.encoding hoping that
	   such window managers at least analyze this format correctly,
	   i.e. treat 8-bit bytes as 8859-1 characters.

	   We may also be able to use "UTF8_STRING" in text.encoding
	   in the future which can encode all Unicode characters.
	   But, for the moment, there's no way to know that the
	   current window manager supports it or not.

	   Either way, we also set the _NET_WM_NAME and _NET_WM_ICON_NAME
	   properties.  Per the EWMH specification, those two properties
	   are always UTF8_STRING.  This matches what gtk_window_set_title()
	   does in the USE_GTK case. */
	text.value = x_encode_text (name, coding_system, 0, &bytes, &stringp,
				    &do_free_text_value);
	text.encoding = (stringp ? XA_STRING
			 : FRAME_X_DISPLAY_INFO (f)->Xatom_COMPOUND_TEXT);
	text.format = 8;
	text.nitems = bytes;
	if (text.nitems != bytes)
	  error ("Window name too large");

	if (!STRINGP (f->icon_name))
	  {
	    icon = text;
	    encoded_icon_name = encoded_name;
	  }
	else
	  {
	    /* See the above comment "Note: Encoding strategy".  */
	    icon.value = x_encode_text (f->icon_name, coding_system, 0,
					&bytes, &stringp, &do_free_icon_value);
	    icon.encoding = (stringp ? XA_STRING
			     : FRAME_X_DISPLAY_INFO (f)->Xatom_COMPOUND_TEXT);
	    icon.format = 8;
	    icon.nitems = bytes;
	    if (icon.nitems != bytes)
	      error ("Icon name too large");

	    encoded_icon_name = ENCODE_UTF_8 (f->icon_name);
	  }

#ifdef USE_GTK
        gtk_window_set_title (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
                              SSDATA (encoded_name));
#else /* not USE_GTK */
	XSetWMName (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f), &text);
	XChangeProperty (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
			 FRAME_X_DISPLAY_INFO (f)->Xatom_net_wm_name,
			 FRAME_X_DISPLAY_INFO (f)->Xatom_UTF8_STRING,
			 8, PropModeReplace,
			 SDATA (encoded_name),
			 SBYTES (encoded_name));
#endif /* not USE_GTK */

	XSetWMIconName (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f), &icon);
	XChangeProperty (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
			 FRAME_X_DISPLAY_INFO (f)->Xatom_net_wm_icon_name,
			 FRAME_X_DISPLAY_INFO (f)->Xatom_UTF8_STRING,
			 8, PropModeReplace,
			 SDATA (encoded_icon_name),
			 SBYTES (encoded_icon_name));

	if (do_free_icon_value)
	  xfree (icon.value);
	if (do_free_text_value)
	  xfree (text.value);
      }
      UNBLOCK_INPUT;
    }
}

/* Change the name of frame F to NAME.  If NAME is nil, set F's name to
       x_id_name.

   If EXPLICIT is non-zero, that indicates that lisp code is setting the
       name; if NAME is a string, set F's name to NAME and set
       F->explicit_name; if NAME is Qnil, then clear F->explicit_name.

   If EXPLICIT is zero, that indicates that Emacs redisplay code is
       suggesting a new name, which lisp code should override; if
       F->explicit_name is set, ignore the new name; otherwise, set it.  */

static void
x_set_name (struct frame *f, Lisp_Object name, int explicit)
{
  /* Make sure that requests from lisp code override requests from
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
	 update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
	update_mode_lines = 1;

      f->explicit_name = ! NILP (name);
    }
  else if (f->explicit_name)
    return;

  /* If NAME is nil, set the name to the x_id_name.  */
  if (NILP (name))
    {
      /* Check for no change needed in this very common case
	 before we do any consing.  */
      if (!strcmp (FRAME_X_DISPLAY_INFO (f)->x_id_name,
		   SSDATA (f->name)))
	return;
      name = build_string (FRAME_X_DISPLAY_INFO (f)->x_id_name);
    }
  else
    CHECK_STRING (name);

  /* Don't change the name if it's already NAME.  */
  if (! NILP (Fstring_equal (name, f->name)))
    return;

  f->name = name;

  /* For setting the frame title, the title parameter should override
     the name parameter.  */
  if (! NILP (f->title))
    name = f->title;

  x_set_name_internal (f, name);
}

/* This function should be called when the user's lisp code has
   specified a name for the frame; the name will override any set by the
   redisplay code.  */
static void
x_explicitly_set_name (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  x_set_name (f, arg, 1);
}

/* This function should be called by Emacs redisplay code to set the
   name; names set this way will never override names set by the user's
   lisp code.  */
void
x_implicitly_set_name (FRAME_PTR f, Lisp_Object arg, Lisp_Object oldval)
{
  x_set_name (f, arg, 0);
}

/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.  */

static void
x_set_title (struct frame *f, Lisp_Object name, Lisp_Object old_name)
{
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 1;

  f->title = name;

  if (NILP (name))
    name = f->name;
  else
    CHECK_STRING (name);

  x_set_name_internal (f, name);
}

void
x_set_scroll_bar_default_width (struct frame *f)
{
  int wid = FRAME_COLUMN_WIDTH (f);
#ifdef USE_TOOLKIT_SCROLL_BARS
#ifdef USE_GTK
  int minw = xg_get_default_scrollbar_width ();
#else
  int minw = 16;
#endif
  /* A minimum width of 14 doesn't look good for toolkit scroll bars.  */
  int width = minw + 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM;
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (width + wid - 1) / wid;
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = width;
#else
  /* Make the actual width at least 14 pixels and a multiple of a
     character width.  */
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;

  /* Use all of that space (aside from required margins) for the
     scroll bar.  */
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = 0;
#endif
}


/* Record in frame F the specified or default value according to ALIST
   of the parameter named PROP (a Lisp symbol).  If no value is
   specified for PROP, look for an X default for XPROP on the frame
   named NAME.  If that is not found either, use the value DEFLT.  */

static Lisp_Object
x_default_scroll_bar_color_parameter (struct frame *f,
				      Lisp_Object alist, Lisp_Object prop,
				      const char *xprop, const char *xclass,
				      int foreground_p)
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Lisp_Object tem;

  tem = x_get_arg (dpyinfo, alist, prop, xprop, xclass, RES_TYPE_STRING);
  if (EQ (tem, Qunbound))
    {
#ifdef USE_TOOLKIT_SCROLL_BARS

      /* See if an X resource for the scroll bar color has been
	 specified.  */
      tem = display_x_get_resource (dpyinfo,
				    build_string (foreground_p
						  ? "foreground"
						  : "background"),
				    empty_unibyte_string,
				    build_string ("verticalScrollBar"),
				    empty_unibyte_string);
      if (!STRINGP (tem))
	{
	  /* If nothing has been specified, scroll bars will use a
	     toolkit-dependent default.  Because these defaults are
	     difficult to get at without actually creating a scroll
	     bar, use nil to indicate that no color has been
	     specified.  */
	  tem = Qnil;
	}

#else /* not USE_TOOLKIT_SCROLL_BARS */

      tem = Qnil;

#endif /* not USE_TOOLKIT_SCROLL_BARS */
    }

  x_set_frame_parameters (f, Fcons (Fcons (prop, tem), Qnil));
  return tem;
}




#ifdef USE_X_TOOLKIT

/* If the WM_PROTOCOLS property does not already contain WM_TAKE_FOCUS,
   WM_DELETE_WINDOW, and WM_SAVE_YOURSELF, then add them.  (They may
   already be present because of the toolkit (Motif adds some of them,
   for example, but Xt doesn't).  */

static void
hack_wm_protocols (FRAME_PTR f, Widget widget)
{
  Display *dpy = XtDisplay (widget);
  Window w = XtWindow (widget);
  int need_delete = 1;
  int need_focus = 1;
  int need_save = 1;

  BLOCK_INPUT;
  {
    Atom type;
    unsigned char *catoms;
    int format = 0;
    unsigned long nitems = 0;
    unsigned long bytes_after;

    if ((XGetWindowProperty (dpy, w,
			     FRAME_X_DISPLAY_INFO (f)->Xatom_wm_protocols,
			     (long)0, (long)100, False, XA_ATOM,
			     &type, &format, &nitems, &bytes_after,
			     &catoms)
	 == Success)
	&& format == 32 && type == XA_ATOM)
      {
	Atom *atoms = (Atom *) catoms;
	while (nitems > 0)
	  {
	    nitems--;
	    if (atoms[nitems]
		== FRAME_X_DISPLAY_INFO (f)->Xatom_wm_delete_window)
	      need_delete = 0;
	    else if (atoms[nitems]
		     == FRAME_X_DISPLAY_INFO (f)->Xatom_wm_take_focus)
	      need_focus = 0;
	    else if (atoms[nitems]
		     == FRAME_X_DISPLAY_INFO (f)->Xatom_wm_save_yourself)
	      need_save = 0;
	  }
      }
    if (catoms)
      XFree (catoms);
  }
  {
    Atom props [10];
    int count = 0;
    if (need_delete)
      props[count++] = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_delete_window;
    if (need_focus)
      props[count++] = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_take_focus;
    if (need_save)
      props[count++] = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_save_yourself;
    if (count)
      XChangeProperty (dpy, w, FRAME_X_DISPLAY_INFO (f)->Xatom_wm_protocols,
		       XA_ATOM, 32, PropModeAppend,
		       (unsigned char *) props, count);
  }
  UNBLOCK_INPUT;
}
#endif



/* Support routines for XIC (X Input Context).  */

#ifdef HAVE_X_I18N

static XFontSet xic_create_xfontset (struct frame *);
static XIMStyle best_xim_style (XIMStyles *, XIMStyles *);


/* Supported XIM styles, ordered by preference.  */

static XIMStyle supported_xim_styles[] =
{
  XIMPreeditPosition | XIMStatusArea,
  XIMPreeditPosition | XIMStatusNothing,
  XIMPreeditPosition | XIMStatusNone,
  XIMPreeditNothing | XIMStatusArea,
  XIMPreeditNothing | XIMStatusNothing,
  XIMPreeditNothing | XIMStatusNone,
  XIMPreeditNone | XIMStatusArea,
  XIMPreeditNone | XIMStatusNothing,
  XIMPreeditNone | XIMStatusNone,
  0,
};


#if defined HAVE_X_WINDOWS && defined USE_X_TOOLKIT
/* Create an X fontset on frame F with base font name BASE_FONTNAME.  */

static const char xic_default_fontset[] = "-*-*-*-r-normal--14-*-*-*-*-*-*-*";

/* Create an Xt fontset spec from the name of a base font.
   If `motif' is True use the Motif syntax.  */
char *
xic_create_fontsetname (const char *base_fontname, int motif)
{
  const char *sep = motif ? ";" : ",";
  char *fontsetname;

  /* Make a fontset name from the base font name.  */
  if (xic_default_fontset == base_fontname)
    { /* There is no base font name, use the default.  */
      ptrdiff_t len = strlen (base_fontname) + 2;
      fontsetname = xmalloc (len);
      memset (fontsetname, 0, len);
      strcpy (fontsetname, base_fontname);
    }
  else
    {
      /* Make a fontset name from the base font name.
	 The font set will be made of the following elements:
	 - the base font.
	 - the base font where the charset spec is replaced by -*-*.
	 - the same but with the family also replaced with -*-*-.  */
      const char *p = base_fontname;
      ptrdiff_t i;

      for (i = 0; *p; p++)
	if (*p == '-') i++;
      if (i != 14)
	{ /* As the font name doesn't conform to XLFD, we can't
	     modify it to generalize it to allcs and allfamilies.
	     Use the specified font plus the default.  */
	  ptrdiff_t len =
	    strlen (base_fontname) + strlen (xic_default_fontset) + 3;
	  fontsetname = xmalloc (len);
	  memset (fontsetname, 0, len);
	  strcpy (fontsetname, base_fontname);
	  strcat (fontsetname, sep);
	  strcat (fontsetname, xic_default_fontset);
	}
      else
	{
	  ptrdiff_t len;
	  const char *p1 = NULL, *p2 = NULL, *p3 = NULL;
	  char *font_allcs = NULL;
	  char *font_allfamilies = NULL;
	  char *font_all = NULL;
	  const char *allcs = "*-*-*-*-*-*-*";
	  const char *allfamilies = "-*-*-";
	  const char *all = "*-*-*-*-";
	  char *base;

	  for (i = 0, p = base_fontname; i < 8; p++)
	    {
	      if (*p == '-')
		{
		  i++;
		  if (i == 3)
		    p1 = p + 1;
		  else if (i == 7)
		    p2 = p + 1;
		  else if (i == 6)
		    p3 = p + 1;
		}
	    }
	  /* If base_fontname specifies ADSTYLE, make it a
	     wildcard.  */
	  if (*p3 != '*')
	    {
	      ptrdiff_t diff = (p2 - p3) - 2;

	      base = alloca (strlen (base_fontname) + 1);
	      memcpy (base, base_fontname, p3 - base_fontname);
	      base[p3 - base_fontname] = '*';
	      base[(p3 - base_fontname) + 1] = '-';
	      strcpy (base + (p3 - base_fontname) + 2, p2);
	      p = base + (p - base_fontname) - diff;
	      p1 = base + (p1 - base_fontname);
	      p2 = base + (p2 - base_fontname) - diff;
	      base_fontname = base;
	    }

	  /* Build the font spec that matches all charsets.  */
	  len = p - base_fontname + strlen (allcs) + 1;
	  font_allcs = (char *) alloca (len);
	  memset (font_allcs, 0, len);
	  memcpy (font_allcs, base_fontname, p - base_fontname);
	  strcat (font_allcs, allcs);

	  /* Build the font spec that matches all families and
	     add-styles.  */
	  len = p - p1 + strlen (allcs) + strlen (allfamilies) + 1;
	  font_allfamilies = (char *) alloca (len);
	  memset (font_allfamilies, 0, len);
	  strcpy (font_allfamilies, allfamilies);
	  memcpy (font_allfamilies + strlen (allfamilies), p1, p - p1);
	  strcat (font_allfamilies, allcs);

	  /* Build the font spec that matches all.  */
	  len = p - p2 + strlen (allcs) + strlen (all) + strlen (allfamilies) + 1;
	  font_all = (char *) alloca (len);
	  memset (font_all, 0, len);
	  strcpy (font_all, allfamilies);
	  strcat (font_all, all);
	  memcpy (font_all + strlen (all) + strlen (allfamilies), p2, p - p2);
	  strcat (font_all, allcs);

	  /* Build the actual font set name.  */
	  len = strlen (base_fontname) + strlen (font_allcs)
	    + strlen (font_allfamilies) + strlen (font_all) + 5;
	  fontsetname = xmalloc (len);
	  memset (fontsetname, 0, len);
	  strcpy (fontsetname, base_fontname);
	  strcat (fontsetname, sep);
	  strcat (fontsetname, font_allcs);
	  strcat (fontsetname, sep);
	  strcat (fontsetname, font_allfamilies);
	  strcat (fontsetname, sep);
	  strcat (fontsetname, font_all);
	}
    }
  if (motif)
    strcat (fontsetname, ":");
  return fontsetname;
}
#endif /* HAVE_X_WINDOWS && USE_X_TOOLKIT */

#ifdef DEBUG_XIC_FONTSET
static void
print_fontset_result (XFontSet xfs, char *name, char **missing_list,
		      int missing_count)
{
  if (xfs)
    fprintf (stderr, "XIC Fontset created: %s\n", name);
  else
    {
      fprintf (stderr, "XIC Fontset failed: %s\n", name);
      while (missing_count-- > 0)
	{
	  fprintf (stderr, "  missing: %s\n", *missing_list);
	  missing_list++;
	}
    }

}
#endif

static XFontSet
xic_create_xfontset (struct frame *f)
{
  XFontSet xfs = NULL;
  struct font *font = FRAME_FONT (f);
  int pixel_size = font->pixel_size;
  Lisp_Object rest, frame;

  /* See if there is another frame already using same fontset.  */
  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *cf = XFRAME (frame);

      if (cf != f && FRAME_LIVE_P (f) && FRAME_X_P (cf)
          && FRAME_X_DISPLAY_INFO (cf) == FRAME_X_DISPLAY_INFO (f)
	  && FRAME_FONT (f)
	  && FRAME_FONT (f)->pixel_size == pixel_size)
        {
          xfs = FRAME_XIC_FONTSET (cf);
          break;
        }
    }

  if (! xfs)
    {
      char buf[256];
      char **missing_list;
      int missing_count;
      char *def_string;
      const char *xlfd_format = "-*-*-medium-r-normal--%d-*-*-*-*-*";

      sprintf (buf, xlfd_format, pixel_size);
      missing_list = NULL;
      xfs = XCreateFontSet (FRAME_X_DISPLAY (f), buf,
			    &missing_list, &missing_count, &def_string);
#ifdef DEBUG_XIC_FONTSET
      print_fontset_result (xfs, buf, missing_list, missing_count);
#endif
      if (missing_list)
	XFreeStringList (missing_list);
      if (! xfs)
	{
	  /* List of pixel sizes most likely available.  Find one that
	     is closest to pixel_size.  */
	  int sizes[] = {0, 8, 10, 11, 12, 14, 17, 18, 20, 24, 26, 34, 0};
	  int *smaller, *larger;

	  for (smaller = sizes; smaller[1]; smaller++)
	    if (smaller[1] >= pixel_size)
	      break;
	  larger = smaller + 1;
	  if (*larger == pixel_size)
	    larger++;
	  while (*smaller || *larger)
	    {
	      int this_size;

	      if (! *larger)
		this_size = *smaller--;
	      else if (! *smaller)
		this_size = *larger++;
	      else if (pixel_size - *smaller < *larger - pixel_size)
		this_size = *smaller--;
	      else
		this_size = *larger++;
	      sprintf (buf, xlfd_format, this_size);
	      missing_list = NULL;
	      xfs = XCreateFontSet (FRAME_X_DISPLAY (f), buf,
				    &missing_list, &missing_count, &def_string);
#ifdef DEBUG_XIC_FONTSET
	      print_fontset_result (xfs, buf, missing_list, missing_count);
#endif
	      if (missing_list)
		XFreeStringList (missing_list);
	      if (xfs)
		break;
	    }
	}
      if (! xfs)
	{
	  const char *last_resort = "-*-*-*-r-normal--*-*-*-*-*-*";

	  missing_list = NULL;
	  xfs = XCreateFontSet (FRAME_X_DISPLAY (f), last_resort,
				&missing_list, &missing_count, &def_string);
#ifdef DEBUG_XIC_FONTSET
	  print_fontset_result (xfs, last_resort, missing_list, missing_count);
#endif
	  if (missing_list)
	    XFreeStringList (missing_list);
	}

    }

  return xfs;
}

/* Free the X fontset of frame F if it is the last frame using it.  */

void
xic_free_xfontset (struct frame *f)
{
  Lisp_Object rest, frame;
  int shared_p = 0;

  if (!FRAME_XIC_FONTSET (f))
    return;

  /* See if there is another frame sharing the same fontset.  */
  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *cf = XFRAME (frame);
      if (cf != f && FRAME_LIVE_P (f) && FRAME_X_P (cf)
          && FRAME_X_DISPLAY_INFO (cf) == FRAME_X_DISPLAY_INFO (f)
          && FRAME_XIC_FONTSET (cf) == FRAME_XIC_FONTSET (f))
        {
          shared_p = 1;
          break;
        }
    }

  if (!shared_p)
    /* The fontset is not used anymore.  It is safe to free it.  */
    XFreeFontSet (FRAME_X_DISPLAY (f), FRAME_XIC_FONTSET (f));

  if (FRAME_XIC_BASE_FONTNAME (f))
    xfree (FRAME_XIC_BASE_FONTNAME (f));
  FRAME_XIC_BASE_FONTNAME (f) = NULL;
  FRAME_XIC_FONTSET (f) = NULL;
}


/* Value is the best input style, given user preferences USER (already
   checked to be supported by Emacs), and styles supported by the
   input method XIM.  */

static XIMStyle
best_xim_style (XIMStyles *user, XIMStyles *xim)
{
  int i, j;

  for (i = 0; i < user->count_styles; ++i)
    for (j = 0; j < xim->count_styles; ++j)
      if (user->supported_styles[i] == xim->supported_styles[j])
	return user->supported_styles[i];

  /* Return the default style.  */
  return XIMPreeditNothing | XIMStatusNothing;
}

/* Create XIC for frame F. */

static XIMStyle xic_style;

void
create_frame_xic (struct frame *f)
{
  XIM xim;
  XIC xic = NULL;
  XFontSet xfs = NULL;

  if (FRAME_XIC (f))
    return;

  /* Create X fontset. */
  xfs = xic_create_xfontset (f);
  xim = FRAME_X_XIM (f);
  if (xim)
    {
      XRectangle s_area;
      XPoint spot;
      XVaNestedList preedit_attr;
      XVaNestedList status_attr;

      s_area.x = 0; s_area.y = 0; s_area.width = 1; s_area.height = 1;
      spot.x = 0; spot.y = 1;

      /* Determine XIC style.  */
      if (xic_style == 0)
	{
	  XIMStyles supported_list;
	  supported_list.count_styles = (sizeof supported_xim_styles
					 / sizeof supported_xim_styles[0]);
	  supported_list.supported_styles = supported_xim_styles;
	  xic_style = best_xim_style (&supported_list,
				      FRAME_X_XIM_STYLES (f));
	}

      preedit_attr = XVaCreateNestedList (0,
					  XNFontSet, xfs,
					  XNForeground,
					  FRAME_FOREGROUND_PIXEL (f),
					  XNBackground,
					  FRAME_BACKGROUND_PIXEL (f),
					  (xic_style & XIMPreeditPosition
					   ? XNSpotLocation
					   : NULL),
					  &spot,
					  NULL);
      status_attr = XVaCreateNestedList (0,
					 XNArea,
					 &s_area,
					 XNFontSet,
					 xfs,
					 XNForeground,
					 FRAME_FOREGROUND_PIXEL (f),
					 XNBackground,
					 FRAME_BACKGROUND_PIXEL (f),
					 NULL);

      xic = XCreateIC (xim,
		       XNInputStyle, xic_style,
		       XNClientWindow, FRAME_X_WINDOW (f),
		       XNFocusWindow, FRAME_X_WINDOW (f),
		       XNStatusAttributes, status_attr,
		       XNPreeditAttributes, preedit_attr,
		       NULL);
      XFree (preedit_attr);
      XFree (status_attr);
    }

  FRAME_XIC (f) = xic;
  FRAME_XIC_STYLE (f) = xic_style;
  FRAME_XIC_FONTSET (f) = xfs;
}


/* Destroy XIC and free XIC fontset of frame F, if any. */

void
free_frame_xic (struct frame *f)
{
  if (FRAME_XIC (f) == NULL)
    return;

  XDestroyIC (FRAME_XIC (f));
  xic_free_xfontset (f);

  FRAME_XIC (f) = NULL;
}


/* Place preedit area for XIC of window W's frame to specified
   pixel position X/Y.  X and Y are relative to window W.  */

void
xic_set_preeditarea (struct window *w, int x, int y)
{
  struct frame *f = XFRAME (w->frame);
  XVaNestedList attr;
  XPoint spot;

  spot.x = WINDOW_TO_FRAME_PIXEL_X (w, x) + WINDOW_LEFT_FRINGE_WIDTH (w);
  spot.y = WINDOW_TO_FRAME_PIXEL_Y (w, y) + FONT_BASE (FRAME_FONT (f));
  attr = XVaCreateNestedList (0, XNSpotLocation, &spot, NULL);
  XSetICValues (FRAME_XIC (f), XNPreeditAttributes, attr, NULL);
  XFree (attr);
}


/* Place status area for XIC in bottom right corner of frame F.. */

void
xic_set_statusarea (struct frame *f)
{
  XIC xic = FRAME_XIC (f);
  XVaNestedList attr;
  XRectangle area;
  XRectangle *needed;

  /* Negotiate geometry of status area.  If input method has existing
     status area, use its current size.  */
  area.x = area.y = area.width = area.height = 0;
  attr = XVaCreateNestedList (0, XNAreaNeeded, &area, NULL);
  XSetICValues (xic, XNStatusAttributes, attr, NULL);
  XFree (attr);

  attr = XVaCreateNestedList (0, XNAreaNeeded, &needed, NULL);
  XGetICValues (xic, XNStatusAttributes, attr, NULL);
  XFree (attr);

  if (needed->width == 0) /* Use XNArea instead of XNAreaNeeded */
    {
      attr = XVaCreateNestedList (0, XNArea, &needed, NULL);
      XGetICValues (xic, XNStatusAttributes, attr, NULL);
      XFree (attr);
    }

  area.width  = needed->width;
  area.height = needed->height;
  area.x = FRAME_PIXEL_WIDTH (f) - area.width - FRAME_INTERNAL_BORDER_WIDTH (f);
  area.y = (FRAME_PIXEL_HEIGHT (f) - area.height
	    - FRAME_MENUBAR_HEIGHT (f)
	    - FRAME_TOOLBAR_TOP_HEIGHT (f)
            - FRAME_INTERNAL_BORDER_WIDTH (f));
  XFree (needed);

  attr = XVaCreateNestedList (0, XNArea, &area, NULL);
  XSetICValues (xic, XNStatusAttributes, attr, NULL);
  XFree (attr);
}


/* Set X fontset for XIC of frame F, using base font name
   BASE_FONTNAME.  Called when a new Emacs fontset is chosen.  */

void
xic_set_xfontset (struct frame *f, const char *base_fontname)
{
  XVaNestedList attr;
  XFontSet xfs;

  xic_free_xfontset (f);

  xfs = xic_create_xfontset (f);

  attr = XVaCreateNestedList (0, XNFontSet, xfs, NULL);
  if (FRAME_XIC_STYLE (f) & XIMPreeditPosition)
    XSetICValues (FRAME_XIC (f), XNPreeditAttributes, attr, NULL);
  if (FRAME_XIC_STYLE (f) & XIMStatusArea)
    XSetICValues (FRAME_XIC (f), XNStatusAttributes, attr, NULL);
  XFree (attr);

  FRAME_XIC_FONTSET (f) = xfs;
}

#endif /* HAVE_X_I18N */



#ifdef USE_X_TOOLKIT

/* Create and set up the X widget for frame F.  */

static void
x_window (struct frame *f, long window_prompting, int minibuffer_only)
{
  XClassHint class_hints;
  XSetWindowAttributes attributes;
  unsigned long attribute_mask;
  Widget shell_widget;
  Widget pane_widget;
  Widget frame_widget;
  Arg al [25];
  int ac;

  BLOCK_INPUT;

  /* Use the resource name as the top-level widget name
     for looking up resources.  Make a non-Lisp copy
     for the window manager, so GC relocation won't bother it.

     Elsewhere we specify the window name for the window manager.  */

  {
    char *str = SSDATA (Vx_resource_name);
    f->namebuf = (char *) xmalloc (strlen (str) + 1);
    strcpy (f->namebuf, str);
  }

  ac = 0;
  XtSetArg (al[ac], XtNallowShellResize, 1); ac++;
  XtSetArg (al[ac], XtNinput, 1); ac++;
  XtSetArg (al[ac], XtNmappedWhenManaged, 0); ac++;
  XtSetArg (al[ac], XtNborderWidth, f->border_width); ac++;
  XtSetArg (al[ac], XtNvisual, FRAME_X_VISUAL (f)); ac++;
  XtSetArg (al[ac], XtNdepth, FRAME_X_DISPLAY_INFO (f)->n_planes); ac++;
  XtSetArg (al[ac], XtNcolormap, FRAME_X_COLORMAP (f)); ac++;
  shell_widget = XtAppCreateShell (f->namebuf, EMACS_CLASS,
				   applicationShellWidgetClass,
				   FRAME_X_DISPLAY (f), al, ac);

  f->output_data.x->widget = shell_widget;
  /* maybe_set_screen_title_format (shell_widget); */

  pane_widget = lw_create_widget ("main", "pane", widget_id_tick++,
				  (widget_value *) NULL,
				  shell_widget, False,
				  (lw_callback) NULL,
				  (lw_callback) NULL,
				  (lw_callback) NULL,
				  (lw_callback) NULL);

  ac = 0;
  XtSetArg (al[ac], XtNvisual, FRAME_X_VISUAL (f)); ac++;
  XtSetArg (al[ac], XtNdepth, FRAME_X_DISPLAY_INFO (f)->n_planes); ac++;
  XtSetArg (al[ac], XtNcolormap, FRAME_X_COLORMAP (f)); ac++;
  XtSetArg (al[ac], XtNborderWidth, 0); ac++;
  XtSetValues (pane_widget, al, ac);
  f->output_data.x->column_widget = pane_widget;

  /* mappedWhenManaged to false tells to the paned window to not map/unmap
     the emacs screen when changing menubar.  This reduces flickering.  */

  ac = 0;
  XtSetArg (al[ac], XtNmappedWhenManaged, 0); ac++;
  XtSetArg (al[ac], XtNshowGrip, 0); ac++;
  XtSetArg (al[ac], XtNallowResize, 1); ac++;
  XtSetArg (al[ac], XtNresizeToPreferred, 1); ac++;
  XtSetArg (al[ac], XtNemacsFrame, f); ac++;
  XtSetArg (al[ac], XtNvisual, FRAME_X_VISUAL (f)); ac++;
  XtSetArg (al[ac], XtNdepth, FRAME_X_DISPLAY_INFO (f)->n_planes); ac++;
  XtSetArg (al[ac], XtNcolormap, FRAME_X_COLORMAP (f)); ac++;
  XtSetArg (al[ac], XtNborderWidth, 0); ac++;
  frame_widget = XtCreateWidget (f->namebuf, emacsFrameClass, pane_widget,
				 al, ac);

  f->output_data.x->edit_widget = frame_widget;

  XtManageChild (frame_widget);

  /* Do some needed geometry management.  */
  {
    ptrdiff_t len;
    char *tem, shell_position[sizeof "=x++" + 4 * INT_STRLEN_BOUND (int)];
    Arg gal[10];
    int gac = 0;
    int extra_borders = 0;
    int menubar_size
      = (f->output_data.x->menubar_widget
	 ? (f->output_data.x->menubar_widget->core.height
	    + f->output_data.x->menubar_widget->core.border_width)
	 : 0);

#if 0 /* Experimentally, we now get the right results
	 for -geometry -0-0 without this.  24 Aug 96, rms.  */
    if (FRAME_EXTERNAL_MENU_BAR (f))
      {
        Dimension ibw = 0;
        XtVaGetValues (pane_widget, XtNinternalBorderWidth, &ibw, NULL);
        menubar_size += ibw;
      }
#endif

    f->output_data.x->menubar_height = menubar_size;

#ifndef USE_LUCID
    /* Motif seems to need this amount added to the sizes
       specified for the shell widget.  The Athena/Lucid widgets don't.
       Both conclusions reached experimentally.  -- rms.  */
    XtVaGetValues (f->output_data.x->edit_widget, XtNinternalBorderWidth,
		   &extra_borders, NULL);
    extra_borders *= 2;
#endif

    /* Convert our geometry parameters into a geometry string
       and specify it.
       Note that we do not specify here whether the position
       is a user-specified or program-specified one.
       We pass that information later, in x_wm_set_size_hints.  */
    {
      int left = f->left_pos;
      int xneg = window_prompting & XNegative;
      int top = f->top_pos;
      int yneg = window_prompting & YNegative;
      if (xneg)
	left = -left;
      if (yneg)
	top = -top;

      if (window_prompting & USPosition)
	sprintf (shell_position, "=%dx%d%c%d%c%d",
		 FRAME_PIXEL_WIDTH (f) + extra_borders,
		 FRAME_PIXEL_HEIGHT (f) + menubar_size + extra_borders,
		 (xneg ? '-' : '+'), left,
		 (yneg ? '-' : '+'), top);
      else
        {
          sprintf (shell_position, "=%dx%d",
                   FRAME_PIXEL_WIDTH (f) + extra_borders,
                   FRAME_PIXEL_HEIGHT (f) + menubar_size + extra_borders);

          /* Setting x and y when the position is not specified in
             the geometry string will set program position in the WM hints.
             If Emacs had just one program position, we could set it in
             fallback resources, but since each make-frame call can specify
             different program positions, this is easier.  */
          XtSetArg (gal[gac], XtNx, left); gac++;
          XtSetArg (gal[gac], XtNy, top); gac++;
        }
    }

    len = strlen (shell_position) + 1;
    /* We don't free this because we don't know whether
       it is safe to free it while the frame exists.
       It isn't worth the trouble of arranging to free it
       when the frame is deleted.  */
    tem = (char *) xmalloc (len);
    strncpy (tem, shell_position, len);
    XtSetArg (gal[gac], XtNgeometry, tem); gac++;
    XtSetValues (shell_widget, gal, gac);
  }

  XtManageChild (pane_widget);
  XtRealizeWidget (shell_widget);

  if (FRAME_X_EMBEDDED_P (f))
    XReparentWindow (FRAME_X_DISPLAY (f), XtWindow (shell_widget),
		     f->output_data.x->parent_desc, 0, 0);

  FRAME_X_WINDOW (f) = XtWindow (frame_widget);

  validate_x_resource_name ();

  class_hints.res_name = SSDATA (Vx_resource_name);
  class_hints.res_class = SSDATA (Vx_resource_class);
  XSetClassHint (FRAME_X_DISPLAY (f), XtWindow (shell_widget), &class_hints);

#ifdef HAVE_X_I18N
  FRAME_XIC (f) = NULL;
  if (use_xim)
    create_frame_xic (f);
#endif

  f->output_data.x->wm_hints.input = True;
  f->output_data.x->wm_hints.flags |= InputHint;
  XSetWMHints (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
	       &f->output_data.x->wm_hints);

  hack_wm_protocols (f, shell_widget);

#ifdef HACK_EDITRES
  XtAddEventHandler (shell_widget, 0, True, _XEditResCheckMessages, 0);
#endif

  /* Do a stupid property change to force the server to generate a
     PropertyNotify event so that the event_stream server timestamp will
     be initialized to something relevant to the time we created the window.
     */
  XChangeProperty (XtDisplay (frame_widget), XtWindow (frame_widget),
		   FRAME_X_DISPLAY_INFO (f)->Xatom_wm_protocols,
		   XA_ATOM, 32, PropModeAppend,
		   (unsigned char*) NULL, 0);

  /* Make all the standard events reach the Emacs frame.  */
  attributes.event_mask = STANDARD_EVENT_SET;

#ifdef HAVE_X_I18N
  if (FRAME_XIC (f))
    {
      /* XIM server might require some X events. */
      unsigned long fevent = NoEventMask;
      XGetICValues (FRAME_XIC (f), XNFilterEvents, &fevent, NULL);
      attributes.event_mask |= fevent;
    }
#endif /* HAVE_X_I18N */

  attribute_mask = CWEventMask;
  XChangeWindowAttributes (XtDisplay (shell_widget), XtWindow (shell_widget),
			   attribute_mask, &attributes);

  XtMapWidget (frame_widget);

  /* x_set_name normally ignores requests to set the name if the
     requested name is the same as the current name.  This is the one
     place where that assumption isn't correct; f->name is set, but
     the X server hasn't been told.  */
  {
    Lisp_Object name;
    int explicit = f->explicit_name;

    f->explicit_name = 0;
    name = f->name;
    f->name = Qnil;
    x_set_name (f, name, explicit);
  }

  XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 f->output_data.x->current_cursor
                 = f->output_data.x->text_cursor);

  UNBLOCK_INPUT;

  /* This is a no-op, except under Motif.  Make sure main areas are
     set to something reasonable, in case we get an error later.  */
  lw_set_main_areas (pane_widget, 0, frame_widget);
}

#else /* not USE_X_TOOLKIT */
#ifdef USE_GTK
static void
x_window (FRAME_PTR f)
{
  if (! xg_create_frame_widgets (f))
    error ("Unable to create window");

#ifdef HAVE_X_I18N
  FRAME_XIC (f) = NULL;
  if (use_xim)
  {
    BLOCK_INPUT;
    create_frame_xic (f);
    if (FRAME_XIC (f))
      {
	/* XIM server might require some X events. */
	unsigned long fevent = NoEventMask;
	XGetICValues (FRAME_XIC (f), XNFilterEvents, &fevent, NULL);

	if (fevent != NoEventMask)
	  {
	    XSetWindowAttributes attributes;
	    XWindowAttributes wattr;
	    unsigned long attribute_mask;

	    XGetWindowAttributes (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				  &wattr);
	    attributes.event_mask = wattr.your_event_mask | fevent;
	    attribute_mask = CWEventMask;
	    XChangeWindowAttributes (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				     attribute_mask, &attributes);
	  }
      }
    UNBLOCK_INPUT;
  }
#endif
}

#else /*! USE_GTK */
/* Create and set up the X window for frame F.  */

static void
x_window (struct frame *f)
{
  XClassHint class_hints;
  XSetWindowAttributes attributes;
  unsigned long attribute_mask;

  attributes.background_pixel = FRAME_BACKGROUND_PIXEL (f);
  attributes.border_pixel = f->output_data.x->border_pixel;
  attributes.bit_gravity = StaticGravity;
  attributes.backing_store = NotUseful;
  attributes.save_under = True;
  attributes.event_mask = STANDARD_EVENT_SET;
  attributes.colormap = FRAME_X_COLORMAP (f);
  attribute_mask = (CWBackPixel | CWBorderPixel | CWBitGravity | CWEventMask
		    | CWColormap);

  BLOCK_INPUT;
  FRAME_X_WINDOW (f)
    = XCreateWindow (FRAME_X_DISPLAY (f),
		     f->output_data.x->parent_desc,
		     f->left_pos,
		     f->top_pos,
		     FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f),
		     f->border_width,
		     CopyFromParent, /* depth */
		     InputOutput, /* class */
		     FRAME_X_VISUAL (f),
		     attribute_mask, &attributes);

#ifdef HAVE_X_I18N
  if (use_xim)
    {
      create_frame_xic (f);
      if (FRAME_XIC (f))
	{
	  /* XIM server might require some X events. */
	  unsigned long fevent = NoEventMask;
	  XGetICValues (FRAME_XIC (f), XNFilterEvents, &fevent, NULL);
	  attributes.event_mask |= fevent;
	  attribute_mask = CWEventMask;
	  XChangeWindowAttributes (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				   attribute_mask, &attributes);
	}
    }
#endif /* HAVE_X_I18N */

  validate_x_resource_name ();

  class_hints.res_name = SSDATA (Vx_resource_name);
  class_hints.res_class = SSDATA (Vx_resource_class);
  XSetClassHint (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &class_hints);

  /* The menubar is part of the ordinary display;
     it does not count in addition to the height of the window.  */
  f->output_data.x->menubar_height = 0;

  /* This indicates that we use the "Passive Input" input model.
     Unless we do this, we don't get the Focus{In,Out} events that we
     need to draw the cursor correctly.  Accursed bureaucrats.
   XWhipsAndChains (FRAME_X_DISPLAY (f), IronMaiden, &TheRack);  */

  f->output_data.x->wm_hints.input = True;
  f->output_data.x->wm_hints.flags |= InputHint;
  XSetWMHints (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
	       &f->output_data.x->wm_hints);
  f->output_data.x->wm_hints.icon_pixmap = None;

  /* Request "save yourself" and "delete window" commands from wm.  */
  {
    Atom protocols[2];
    protocols[0] = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_delete_window;
    protocols[1] = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_save_yourself;
    XSetWMProtocols (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), protocols, 2);
  }

  /* x_set_name normally ignores requests to set the name if the
     requested name is the same as the current name.  This is the one
     place where that assumption isn't correct; f->name is set, but
     the X server hasn't been told.  */
  {
    Lisp_Object name;
    int explicit = f->explicit_name;

    f->explicit_name = 0;
    name = f->name;
    f->name = Qnil;
    x_set_name (f, name, explicit);
  }

  XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 f->output_data.x->current_cursor
                 = f->output_data.x->text_cursor);

  UNBLOCK_INPUT;

  if (FRAME_X_WINDOW (f) == 0)
    error ("Unable to create window");
}

#endif /* not USE_GTK */
#endif /* not USE_X_TOOLKIT */

/* Verify that the icon position args for this window are valid.  */

static void
x_icon_verify (struct frame *f, Lisp_Object parms)
{
  Lisp_Object icon_x, icon_y;

  /* Set the position of the icon.  Note that twm groups all
     icons in an icon window.  */
  icon_x = x_frame_get_and_record_arg (f, parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  icon_y = x_frame_get_and_record_arg (f, parms, Qicon_top, 0, 0, RES_TYPE_NUMBER);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_NUMBER (icon_x);
      CHECK_NUMBER (icon_y);
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");
}

/* Handle the icon stuff for this window.  Perhaps later we might
   want an x_set_icon_position which can be called interactively as
   well.  */

static void
x_icon (struct frame *f, Lisp_Object parms)
{
  Lisp_Object icon_x, icon_y;
#if 0
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
#endif

  /* Set the position of the icon.  Note that twm groups all
     icons in an icon window.  */
  icon_x = x_frame_get_and_record_arg (f, parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  icon_y = x_frame_get_and_record_arg (f, parms, Qicon_top, 0, 0, RES_TYPE_NUMBER);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_NUMBER (icon_x);
      CHECK_NUMBER (icon_y);
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");

  BLOCK_INPUT;

  if (! EQ (icon_x, Qunbound))
    x_wm_set_icon_position (f, XINT (icon_x), XINT (icon_y));

#if 0 /* x_get_arg removes the visibility parameter as a side effect,
         but x_create_frame still needs it.  */
  /* Start up iconic or window? */
  x_wm_set_window_state
    (f, (EQ (x_get_arg (dpyinfo, parms, Qvisibility, 0, 0, RES_TYPE_SYMBOL),
	     Qicon)
	 ? IconicState
	 : NormalState));
#endif

  x_text_icon (f, SSDATA ((!NILP (f->icon_name)
			   ? f->icon_name
			   : f->name)));

  UNBLOCK_INPUT;
}

/* Make the GCs needed for this window, setting the
   background, border and mouse colors; also create the
   mouse cursor and the gray border tile.  */

static void
x_make_gc (struct frame *f)
{
  XGCValues gc_values;

  BLOCK_INPUT;

  /* Create the GCs of this frame.
     Note that many default values are used.  */

  gc_values.foreground = FRAME_FOREGROUND_PIXEL (f);
  gc_values.background = FRAME_BACKGROUND_PIXEL (f);
  gc_values.line_width = 0;	/* Means 1 using fast algorithm.  */
  f->output_data.x->normal_gc
    = XCreateGC (FRAME_X_DISPLAY (f),
		 FRAME_X_WINDOW (f),
		 GCLineWidth | GCForeground | GCBackground,
		 &gc_values);

  /* Reverse video style.  */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL (f);
  gc_values.background = FRAME_FOREGROUND_PIXEL (f);
  f->output_data.x->reverse_gc
    = XCreateGC (FRAME_X_DISPLAY (f),
		 FRAME_X_WINDOW (f),
		 GCForeground | GCBackground | GCLineWidth,
		 &gc_values);

  /* Cursor has cursor-color background, background-color foreground.  */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL (f);
  gc_values.background = f->output_data.x->cursor_pixel;
  gc_values.fill_style = FillOpaqueStippled;
  f->output_data.x->cursor_gc
    = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 (GCForeground | GCBackground
		  | GCFillStyle | GCLineWidth),
		 &gc_values);

  /* Reliefs.  */
  f->output_data.x->white_relief.gc = 0;
  f->output_data.x->black_relief.gc = 0;

  /* Create the gray border tile used when the pointer is not in
     the frame.  Since this depends on the frame's pixel values,
     this must be done on a per-frame basis.  */
  f->output_data.x->border_tile
    = (XCreatePixmapFromBitmapData
       (FRAME_X_DISPLAY (f), FRAME_X_DISPLAY_INFO (f)->root_window,
	gray_bits, gray_width, gray_height,
	FRAME_FOREGROUND_PIXEL (f),
	FRAME_BACKGROUND_PIXEL (f),
	DefaultDepth (FRAME_X_DISPLAY (f), FRAME_X_SCREEN_NUMBER (f))));

  UNBLOCK_INPUT;
}


/* Free what was allocated in x_make_gc.  */

void
x_free_gcs (struct frame *f)
{
  Display *dpy = FRAME_X_DISPLAY (f);

  BLOCK_INPUT;

  if (f->output_data.x->normal_gc)
    {
      XFreeGC (dpy, f->output_data.x->normal_gc);
      f->output_data.x->normal_gc = 0;
    }

  if (f->output_data.x->reverse_gc)
    {
      XFreeGC (dpy, f->output_data.x->reverse_gc);
      f->output_data.x->reverse_gc = 0;
    }

  if (f->output_data.x->cursor_gc)
    {
      XFreeGC (dpy, f->output_data.x->cursor_gc);
      f->output_data.x->cursor_gc = 0;
    }

  if (f->output_data.x->border_tile)
    {
      XFreePixmap (dpy, f->output_data.x->border_tile);
      f->output_data.x->border_tile = 0;
    }

  UNBLOCK_INPUT;
}


/* Handler for signals raised during x_create_frame and
   x_create_tip_frame.  FRAME is the frame which is partially
   constructed.  */

static Lisp_Object
unwind_create_frame (Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);

  /* If frame is already dead, nothing to do.  This can happen if the
     display is disconnected after the frame has become official, but
     before x_create_frame removes the unwind protect.  */
  if (!FRAME_LIVE_P (f))
    return Qnil;

  /* If frame is ``official'', nothing to do.  */
  if (NILP (Fmemq (frame, Vframe_list)))
    {
#if GLYPH_DEBUG && XASSERTS
      struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
#endif

      x_free_frame_resources (f);
      free_glyphs (f);

#if GLYPH_DEBUG
      /* Check that reference counts are indeed correct.  */
      xassert (dpyinfo->reference_count == dpyinfo_refcount);
      xassert (dpyinfo->terminal->image_cache->refcount == image_cache_refcount);
#endif
      return Qt;
    }

  return Qnil;
}

static Lisp_Object
unwind_create_frame_1 (Lisp_Object val)
{
  inhibit_lisp_code = val;
  return Qnil;
}

static void
x_default_font_parameter (struct frame *f, Lisp_Object parms)
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Lisp_Object font_param = x_get_arg (dpyinfo, parms, Qfont, NULL, NULL,
                                      RES_TYPE_STRING);
  Lisp_Object font = Qnil;
  if (EQ (font_param, Qunbound))
    font_param = Qnil;

  if (NILP (font_param))
    {
      /* System font should take precedence over X resources.  We suggest this
         regardless of font-use-system-font because .emacs may not have been
         read yet.  */
      const char *system_font = xsettings_get_system_font ();
      if (system_font)
        {
          char *name = xstrdup (system_font);
          font = font_open_by_name (f, name);
          xfree (name);
        }
    }

  if (NILP (font))
      font = !NILP (font_param) ? font_param
      : x_get_arg (dpyinfo, parms, Qfont, "font", "Font", RES_TYPE_STRING);

  if (! FONTP (font) && ! STRINGP (font))
    {
      const char *names[]
	= {
#ifdef HAVE_XFT
	    /* This will find the normal Xft font.  */
 	    "monospace-10",
#endif
	    "-adobe-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-1",
	    "-misc-fixed-medium-r-normal-*-*-140-*-*-c-*-iso8859-1",
	    "-*-*-medium-r-normal-*-*-140-*-*-c-*-iso8859-1",
	    /* This was formerly the first thing tried, but it finds
	       too many fonts and takes too long.  */
	    "-*-*-medium-r-*-*-*-*-*-*-c-*-iso8859-1",
	    /* If those didn't work, look for something which will
	       at least work.  */
	    "-*-fixed-*-*-*-*-*-140-*-*-c-*-iso8859-1",
	    "fixed",
	    NULL };
      int i;

      for (i = 0; names[i]; i++)
	{
	  font = font_open_by_name (f, names[i]);
	  if (! NILP (font))
	    break;
	}
      if (NILP (font))
	error ("No suitable font was found");
    }
  else if (!NILP (font_param))
    {
      /* Remember the explicit font parameter, so we can re-apply it after
	 we've applied the `default' face settings.  */
      x_set_frame_parameters (f, Fcons (Fcons (Qfont_param, font_param), Qnil));
    }

  /* This call will make X resources override any system font setting.  */
  x_default_parameter (f, parms, Qfont, font, "font", "Font", RES_TYPE_STRING);
}


DEFUN ("x-wm-set-size-hint", Fx_wm_set_size_hint, Sx_wm_set_size_hint,
       0, 1, 0,
       doc: /* Send the size hints for frame FRAME to the window manager.
If FRAME is nil, use the selected frame.  */)
  (Lisp_Object frame)
{
  struct frame *f;
  if (NILP (frame))
    frame = selected_frame;
  f = XFRAME (frame);
  BLOCK_INPUT;
  if (FRAME_X_P (f))
    x_wm_set_size_hint (f, 0, 0);
  UNBLOCK_INPUT;
  return Qnil;
}

static void
set_machine_and_pid_properties (struct frame *f)
{
  long pid = (long) getpid ();

  /* This will set WM_CLIENT_MACHINE and WM_LOCALE_NAME.  */
  XSetWMProperties (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f), NULL, NULL,
                    NULL, 0, NULL, NULL, NULL);
  XChangeProperty (FRAME_X_DISPLAY (f),
                   FRAME_OUTER_WINDOW (f),
                   XInternAtom (FRAME_X_DISPLAY (f),
                                "_NET_WM_PID",
                                False),
                   XA_CARDINAL, 32, PropModeReplace,
                   (unsigned char *) &pid, 1);
}

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* Make a new X window, which is called a "frame" in Emacs terms.
Return an Emacs frame object.
PARMS is an alist of frame parameters.
If the parameters specify that the frame should not have a minibuffer,
and do not specify a specific minibuffer window to use,
then `default-minibuffer-frame' must be a frame whose minibuffer can
be shared by the new frame.

This function is an internal primitive--use `make-frame' instead.  */)
  (Lisp_Object parms)
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  int minibuffer_only = 0;
  long window_prompting = 0;
  int width, height;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object display;
  struct x_display_info *dpyinfo = NULL;
  Lisp_Object parent;
  struct kboard *kb;

  parms = Fcopy_alist (parms);

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = x_get_arg (dpyinfo, parms, Qterminal, 0, 0, RES_TYPE_NUMBER);
  if (EQ (display, Qunbound))
    display = x_get_arg (dpyinfo, parms, Qdisplay, 0, 0, RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_x_display_info (display);
  kb = dpyinfo->terminal->kboard;

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  name = x_get_arg (dpyinfo, parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent = x_get_arg (dpyinfo, parms, Qparent_id, NULL, NULL, RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_NUMBER (parent);

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  GCPRO4 (parms, parent, name, frame);
  tem = x_get_arg (dpyinfo, parms, Qminibuffer, "minibuffer", "Minibuffer",
		   RES_TYPE_SYMBOL);
  if (EQ (tem, Qnone) || NILP (tem))
    f = make_frame_without_minibuffer (Qnil, kb, display);
  else if (EQ (tem, Qonly))
    {
      f = make_minibuffer_frame ();
      minibuffer_only = 1;
    }
  else if (WINDOWP (tem))
    f = make_frame_without_minibuffer (tem, kb, display);
  else
    f = make_frame (1);

  XSETFRAME (frame, f);

  /* Note that X Windows does support scroll bars.  */
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 1;

  f->terminal = dpyinfo->terminal;

  f->output_method = output_x_window;
  f->output_data.x = (struct x_output *) xmalloc (sizeof (struct x_output));
  memset (f->output_data.x, 0, sizeof (struct x_output));
  f->output_data.x->icon_bitmap = -1;
  FRAME_FONTSET (f) = -1;
  f->output_data.x->scroll_bar_foreground_pixel = -1;
  f->output_data.x->scroll_bar_background_pixel = -1;
#ifdef USE_TOOLKIT_SCROLL_BARS
  f->output_data.x->scroll_bar_top_shadow_pixel = -1;
  f->output_data.x->scroll_bar_bottom_shadow_pixel = -1;
#endif /* USE_TOOLKIT_SCROLL_BARS */

  f->icon_name
    = x_get_arg (dpyinfo, parms, Qicon_name, "iconName", "Title",
		 RES_TYPE_STRING);
  if (! STRINGP (f->icon_name))
    f->icon_name = Qnil;

  FRAME_X_DISPLAY_INFO (f) = dpyinfo;

  /* With FRAME_X_DISPLAY_INFO set up, this unwind-protect is safe.  */
  record_unwind_protect (unwind_create_frame, frame);

  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!  */
  {
    Lisp_Object black;
    struct gcpro gcpro1;

    /* Function x_decode_color can signal an error.  Make
       sure to initialize color slots so that we won't try
       to free colors we haven't allocated.  */
    FRAME_FOREGROUND_PIXEL (f) = -1;
    FRAME_BACKGROUND_PIXEL (f) = -1;
    f->output_data.x->cursor_pixel = -1;
    f->output_data.x->cursor_foreground_pixel = -1;
    f->output_data.x->border_pixel = -1;
    f->output_data.x->mouse_pixel = -1;

    black = build_string ("black");
    GCPRO1 (black);
    FRAME_FOREGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_BACKGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_foreground_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->border_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->mouse_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    UNGCPRO;
  }

  /* Specify the parent under which to make this X window.  */

  if (!NILP (parent))
    {
      f->output_data.x->parent_desc = (Window) XFASTINT (parent);
      f->output_data.x->explicit_parent = 1;
    }
  else
    {
      f->output_data.x->parent_desc = FRAME_X_DISPLAY_INFO (f)->root_window;
      f->output_data.x->explicit_parent = 0;
    }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      f->name = build_string (dpyinfo->x_id_name);
      f->explicit_name = 0;
    }
  else
    {
      f->name = name;
      f->explicit_name = 1;
      /* use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  f->resx = dpyinfo->resx;
  f->resy = dpyinfo->resy;

#ifdef HAVE_FREETYPE
#ifdef HAVE_XFT
  register_font_driver (&xftfont_driver, f);
#else	/* not HAVE_XFT */
  register_font_driver (&ftxfont_driver, f);
#endif	/* not HAVE_XFT */
#endif	/* HAVE_FREETYPE */
  register_font_driver (&xfont_driver, f);

  x_default_parameter (f, parms, Qfont_backend, Qnil,
		       "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  x_default_font_parameter (f, parms);
  if (!FRAME_FONT (f))
    {
      delete_frame (frame, Qnoelisp);
      error ("Invalid frame font");
    }

  /* Frame contents get displaced if an embedded X window has a border.  */
  if (! FRAME_X_EMBEDDED_P (f))
    x_default_parameter (f, parms, Qborder_width, make_number (0),
			 "borderWidth", "BorderWidth", RES_TYPE_NUMBER);

  /* This defaults to 1 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = x_get_arg (dpyinfo, parms, Qinternal_border_width,
			 "internalBorder", "internalBorder", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }
  x_default_parameter (f, parms, Qinternal_border_width,
#ifdef USE_GTK /* We used to impose 0 in xg_create_frame_widgets.  */
		       make_number (0),
#else
		       make_number (1),
#endif
		       "internalBorderWidth", "internalBorderWidth",
		       RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qvertical_scroll_bars,
#if defined (USE_GTK) && defined (USE_TOOLKIT_SCROLL_BARS)
		       Qright,
#else
		       Qleft,
#endif
		       "verticalScrollBars", "ScrollBars",
		       RES_TYPE_SYMBOL);

  /* Also do the stuff which must be set before the window exists.  */
  x_default_parameter (f, parms, Qforeground_color, build_string ("black"),
		       "foreground", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qbackground_color, build_string ("white"),
		       "background", "Background", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qmouse_color, build_string ("black"),
		       "pointerColor", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qborder_color, build_string ("black"),
		       "borderColor", "BorderColor", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qscreen_gamma, Qnil,
		       "screenGamma", "ScreenGamma", RES_TYPE_FLOAT);
  x_default_parameter (f, parms, Qline_spacing, Qnil,
		       "lineSpacing", "LineSpacing", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qleft_fringe, Qnil,
		       "leftFringe", "LeftFringe", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qright_fringe, Qnil,
		       "rightFringe", "RightFringe", RES_TYPE_NUMBER);

  x_default_scroll_bar_color_parameter (f, parms, Qscroll_bar_foreground,
					"scrollBarForeground",
					"ScrollBarForeground", 1);
  x_default_scroll_bar_color_parameter (f, parms, Qscroll_bar_background,
					"scrollBarBackground",
					"ScrollBarBackground", 0);

#if GLYPH_DEBUG
  image_cache_refcount =
    FRAME_IMAGE_CACHE (f) ? FRAME_IMAGE_CACHE (f)->refcount : 0;
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */

  /* Init faces before x_default_parameter is called for scroll-bar
     parameters because that function calls x_set_scroll_bar_width,
     which calls change_frame_size, which calls Fset_window_buffer,
     which runs hooks, which call Fvertical_motion.  At the end, we
     end up in init_iterator with a null face cache, which should not
     happen.  */
  init_frame_faces (f);

  /* Set the menu-bar-lines and tool-bar-lines parameters.  We don't
     look up the X resources controlling the menu-bar and tool-bar
     here; they are processed specially at startup, and reflected in
     the values of the mode variables.

     Avoid calling window-configuration-change-hook; otherwise we
     could get an infloop in next_frame since the frame is not yet in
     Vframe_list.  */
  {
    int count2 = SPECPDL_INDEX ();
    record_unwind_protect (unwind_create_frame_1, inhibit_lisp_code);
    inhibit_lisp_code = Qt;

    x_default_parameter (f, parms, Qmenu_bar_lines,
			 NILP (Vmenu_bar_mode)
			 ? make_number (0) : make_number (1),
			 NULL, NULL, RES_TYPE_NUMBER);
    x_default_parameter (f, parms, Qtool_bar_lines,
			 NILP (Vtool_bar_mode)
			 ? make_number (0) : make_number (1),
			 NULL, NULL, RES_TYPE_NUMBER);

    unbind_to (count2, Qnil);
  }

  x_default_parameter (f, parms, Qbuffer_predicate, Qnil,
		       "bufferPredicate", "BufferPredicate",
		       RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qtitle, Qnil,
		       "title", "Title", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qwait_for_wm, Qt,
		       "waitForWM", "WaitForWM", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qfullscreen, Qnil,
                       "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qtool_bar_position,
                       f->tool_bar_position, 0, 0, RES_TYPE_SYMBOL);

  /* Compute the size of the X window.  */
  window_prompting = x_figure_window_size (f, parms, 1);

  tem = x_get_arg (dpyinfo, parms, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  x_icon_verify (f, parms);

  /* Create the X widget or window.  */
#ifdef USE_X_TOOLKIT
  x_window (f, window_prompting, minibuffer_only);
#else
  x_window (f);
#endif

  x_icon (f, parms);
  x_make_gc (f);

  /* Now consider the frame official.  */
  f->terminal->reference_count++;
  FRAME_X_DISPLAY_INFO (f)->reference_count++;
  Vframe_list = Fcons (frame, Vframe_list);

  /* We need to do this after creating the X window, so that the
     icon-creation functions can say whose icon they're describing.  */
  x_default_parameter (f, parms, Qicon_type, Qt,
		       "bitmapIcon", "BitmapIcon", RES_TYPE_BOOLEAN);

  x_default_parameter (f, parms, Qauto_raise, Qnil,
		       "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qauto_lower, Qnil,
		       "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qcursor_type, Qbox,
		       "cursorType", "CursorType", RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qscroll_bar_width, Qnil,
		       "scrollBarWidth", "ScrollBarWidth",
		       RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qalpha, Qnil,
		       "alpha", "Alpha", RES_TYPE_NUMBER);

  /* Dimensions, especially FRAME_LINES (f), must be done via change_frame_size.
     Change will not be effected unless different from the current
     FRAME_LINES (f).  */
  width = FRAME_COLS (f);
  height = FRAME_LINES (f);

  SET_FRAME_COLS (f, 0);
  FRAME_LINES (f) = 0;
  change_frame_size (f, height, width, 1, 0, 0);

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
  /* Create the menu bar.  */
  if (!minibuffer_only && FRAME_EXTERNAL_MENU_BAR (f))
    {
      /* If this signals an error, we haven't set size hints for the
	 frame and we didn't make it visible.  */
      initialize_frame_menubar (f);

#ifndef USE_GTK
      /* This is a no-op, except under Motif where it arranges the
	 main window for the widgets on it.  */
      lw_set_main_areas (f->output_data.x->column_widget,
			 f->output_data.x->menubar_widget,
			 f->output_data.x->edit_widget);
#endif /* not USE_GTK */
    }
#endif /* USE_X_TOOLKIT || USE_GTK */

  /* Tell the server what size and position, etc, we want, and how
     badly we want them.  This should be done after we have the menu
     bar so that its size can be taken into account.  */
  BLOCK_INPUT;
  x_wm_set_size_hint (f, window_prompting, 0);
  UNBLOCK_INPUT;

  /* Make the window appear on the frame and enable display, unless
     the caller says not to.  However, with explicit parent, Emacs
     cannot control visibility, so don't try.  */
  if (! f->output_data.x->explicit_parent)
    {
      Lisp_Object visibility;

      visibility = x_get_arg (dpyinfo, parms, Qvisibility, 0, 0,
			      RES_TYPE_SYMBOL);
      if (EQ (visibility, Qunbound))
	visibility = Qt;

      if (EQ (visibility, Qicon))
	x_iconify_frame (f);
      else if (! NILP (visibility))
	x_make_frame_visible (f);
      else
	{
	  /* Must have been Qnil.  */
	}
    }

  BLOCK_INPUT;

  /* Set machine name and pid for the purpose of window managers.  */
  set_machine_and_pid_properties (f);

  /* Set the WM leader property.  GTK does this itself, so this is not
     needed when using GTK.  */
  if (dpyinfo->client_leader_window != 0)
    {
      XChangeProperty (FRAME_X_DISPLAY (f),
                       FRAME_OUTER_WINDOW (f),
                       dpyinfo->Xatom_wm_client_leader,
                       XA_WINDOW, 32, PropModeReplace,
                       (unsigned char *) &dpyinfo->client_leader_window, 1);
    }

  UNBLOCK_INPUT;

  /* Initialize `default-minibuffer-frame' in case this is the first
     frame on this terminal.  */
  if (FRAME_HAS_MINIBUF_P (f)
      && (!FRAMEP (KVAR (kb, Vdefault_minibuffer_frame))
          || !FRAME_LIVE_P (XFRAME (KVAR (kb, Vdefault_minibuffer_frame)))))
    KVAR (kb, Vdefault_minibuffer_frame) = frame;

  /* All remaining specified parameters, which have not been "used"
     by x_get_arg and friends, now go in the misc. alist of the frame.  */
  for (tem = parms; CONSP (tem); tem = XCDR (tem))
    if (CONSP (XCAR (tem)) && !NILP (XCAR (XCAR (tem))))
      f->param_alist = Fcons (XCAR (tem), f->param_alist);

  UNGCPRO;

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;

  return unbind_to (count, frame);
}


/* FRAME is used only to get a handle on the X display.  We don't pass the
   display info directly because we're called from frame.c, which doesn't
   know about that structure.  */

Lisp_Object
x_get_focus_frame (struct frame *frame)
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (frame);
  Lisp_Object xfocus;
  if (! dpyinfo->x_focus_frame)
    return Qnil;

  XSETFRAME (xfocus, dpyinfo->x_focus_frame);
  return xfocus;
}


/* In certain situations, when the window manager follows a
   click-to-focus policy, there seems to be no way around calling
   XSetInputFocus to give another frame the input focus .

   In an ideal world, XSetInputFocus should generally be avoided so
   that applications don't interfere with the window manager's focus
   policy.  But I think it's okay to use when it's clearly done
   following a user-command.  */

DEFUN ("x-focus-frame", Fx_focus_frame, Sx_focus_frame, 1, 1, 0,
       doc: /* Set the input focus to FRAME.
FRAME nil means use the selected frame.  */)
  (Lisp_Object frame)
{
  struct frame *f = check_x_frame (frame);
  Display *dpy = FRAME_X_DISPLAY (f);

  BLOCK_INPUT;
  x_catch_errors (dpy);

  if (FRAME_X_EMBEDDED_P (f))
    {
      /* For Xembedded frames, normally the embedder forwards key
	 events.  See XEmbed Protocol Specification at
	 http://freedesktop.org/wiki/Specifications/xembed-spec  */
      xembed_request_focus (f);
    }
  else
    {
      XSetInputFocus (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		      RevertToParent, CurrentTime);
      x_ewmh_activate_frame (f);
    }

  x_uncatch_errors ();
  UNBLOCK_INPUT;

  return Qnil;
}


DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* Internal function called by `color-defined-p', which see
.\(Note that the Nextstep version of this function ignores FRAME.)  */)
  (Lisp_Object color, Lisp_Object frame)
{
  XColor foo;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_STRING (color);

  if (x_defined_color (f, SSDATA (color), &foo, 0))
    return Qt;
  else
    return Qnil;
}

DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
       doc: /* Internal function called by `color-values', which see.  */)
  (Lisp_Object color, Lisp_Object frame)
{
  XColor foo;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_STRING (color);

  if (x_defined_color (f, SSDATA (color), &foo, 0))
    return list3 (make_number (foo.red),
		  make_number (foo.green),
		  make_number (foo.blue));
  else
    return Qnil;
}

DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* Internal function called by `display-color-p', which see.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  if (dpyinfo->n_planes <= 2)
    return Qnil;

  switch (dpyinfo->visual->class)
    {
    case StaticColor:
    case PseudoColor:
    case TrueColor:
    case DirectColor:
      return Qt;

    default:
      return Qnil;
    }
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p, Sx_display_grayscale_p,
       0, 1, 0,
       doc: /* Return t if the X display supports shades of gray.
Note that color displays do support shades of gray.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  if (dpyinfo->n_planes <= 1)
    return Qnil;

  switch (dpyinfo->visual->class)
    {
    case StaticColor:
    case PseudoColor:
    case TrueColor:
    case DirectColor:
    case StaticGray:
    case GrayScale:
      return Qt;

    default:
      return Qnil;
    }
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
       0, 1, 0,
       doc: /* Return the width in pixels of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_number (x_display_pixel_width (dpyinfo));
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height, 0, 1, 0,
       doc: /* Return the height in pixels of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_number (x_display_pixel_height (dpyinfo));
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* Return the number of bitplanes of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_number (dpyinfo->n_planes);
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* Return the number of color cells of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  int nr_planes = DisplayPlanes (dpyinfo->display,
                                 XScreenNumberOfScreen (dpyinfo->screen));

  /* Truncate nr_planes to 24 to avoid integer overflow.
     Some displays says 32, but only 24 bits are actually significant.
     There are only very few and rare video cards that have more than
     24 significant bits.  Also 24 bits is more than 16 million colors,
     it "should be enough for everyone".  */
  if (nr_planes > 24) nr_planes = 24;

  return make_number (1 << nr_planes);
}

DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* Return the maximum request size of the X server of display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_number (MAXREQUEST (dpyinfo->display));
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* Return the "vendor ID" string of the X server of display TERMINAL.
\(Labeling every distributor as a "vendor" embodies the false assumption
that operating systems cannot be developed and distributed noncommercially.)
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);
  const char *vendor = ServerVendor (dpyinfo->display);

  if (! vendor) vendor = "";
  return build_string (vendor);
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* Return the version numbers of the X server of display TERMINAL.
The value is a list of three integers: the major and minor
version numbers of the X Protocol in use, and the distributor-specific release
number.  See also the function `x-server-vendor'.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);
  Display *dpy = dpyinfo->display;

  return Fcons (make_number (ProtocolVersion (dpy)),
		Fcons (make_number (ProtocolRevision (dpy)),
		       Fcons (make_number (VendorRelease (dpy)), Qnil)));
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* Return the number of screens on the X server of display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_number (ScreenCount (dpyinfo->display));
}

DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
       doc: /* Return the height in millimeters of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_number (HeightMMOfScreen (dpyinfo->screen));
}

DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* Return the width in millimeters of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_number (WidthMMOfScreen (dpyinfo->screen));
}

DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* Return an indication of whether X display TERMINAL does backing store.
The value may be `always', `when-mapped', or `not-useful'.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);
  Lisp_Object result;

  switch (DoesBackingStore (dpyinfo->screen))
    {
    case Always:
      result = intern ("always");
      break;

    case WhenMapped:
      result = intern ("when-mapped");
      break;

    case NotUseful:
      result = intern ("not-useful");
      break;

    default:
      error ("Strange value for BackingStore parameter of screen");
      result = Qnil;
    }

  return result;
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* Return the visual class of the X display TERMINAL.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);
  Lisp_Object result;

  switch (dpyinfo->visual->class)
    {
    case StaticGray:
      result = intern ("static-gray");
      break;
    case GrayScale:
      result = intern ("gray-scale");
      break;
    case StaticColor:
      result = intern ("static-color");
      break;
    case PseudoColor:
      result = intern ("pseudo-color");
      break;
    case TrueColor:
      result = intern ("true-color");
      break;
    case DirectColor:
      result = intern ("direct-color");
      break;
    default:
      error ("Display has an unknown visual class");
      result = Qnil;
    }

  return result;
}

DEFUN ("x-display-save-under", Fx_display_save_under,
       Sx_display_save_under, 0, 1, 0,
       doc: /* Return t if the X display TERMINAL supports the save-under feature.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  if (DoesSaveUnders (dpyinfo->screen) == True)
    return Qt;
  else
    return Qnil;
}

int
x_pixel_width (register struct frame *f)
{
  return FRAME_PIXEL_WIDTH (f);
}

int
x_pixel_height (register struct frame *f)
{
  return FRAME_PIXEL_HEIGHT (f);
}

int
x_char_width (register struct frame *f)
{
  return FRAME_COLUMN_WIDTH (f);
}

int
x_char_height (register struct frame *f)
{
  return FRAME_LINE_HEIGHT (f);
}



/************************************************************************
			      X Displays
 ************************************************************************/


/* Mapping visual names to visuals.  */

static struct visual_class
{
  const char *name;
  int class;
}
visual_classes[] =
{
  {"StaticGray",	StaticGray},
  {"GrayScale",		GrayScale},
  {"StaticColor",	StaticColor},
  {"PseudoColor",	PseudoColor},
  {"TrueColor",		TrueColor},
  {"DirectColor",	DirectColor},
  {NULL, 0}
};


#ifndef HAVE_XSCREENNUMBEROFSCREEN

/* Value is the screen number of screen SCR.  This is a substitute for
   the X function with the same name when that doesn't exist.  */

int
XScreenNumberOfScreen (scr)
    register Screen *scr;
{
  Display *dpy = scr->display;
  int i;

  for (i = 0; i < dpy->nscreens; ++i)
    if (scr == dpy->screens + i)
      break;

  return i;
}

#endif /* not HAVE_XSCREENNUMBEROFSCREEN */


/* Select the visual that should be used on display DPYINFO.  Set
   members of DPYINFO appropriately.  Called from x_term_init.  */

void
select_visual (struct x_display_info *dpyinfo)
{
  Display *dpy = dpyinfo->display;
  Screen *screen = dpyinfo->screen;
  Lisp_Object value;

  /* See if a visual is specified.  */
  value = display_x_get_resource (dpyinfo,
				  build_string ("visualClass"),
				  build_string ("VisualClass"),
				  Qnil, Qnil);
  if (STRINGP (value))
    {
      /* VALUE should be of the form CLASS-DEPTH, where CLASS is one
	 of `PseudoColor', `TrueColor' etc. and DEPTH is the color
	 depth, a decimal number.  NAME is compared with case ignored.  */
      char *s = (char *) alloca (SBYTES (value) + 1);
      char *dash;
      int i, class = -1;
      XVisualInfo vinfo;

      strcpy (s, SSDATA (value));
      dash = strchr (s, '-');
      if (dash)
	{
	  dpyinfo->n_planes = atoi (dash + 1);
	  *dash = '\0';
	}
      else
	/* We won't find a matching visual with depth 0, so that
	   an error will be printed below.  */
	dpyinfo->n_planes = 0;

      /* Determine the visual class.  */
      for (i = 0; visual_classes[i].name; ++i)
	if (xstrcasecmp (s, visual_classes[i].name) == 0)
	  {
	    class = visual_classes[i].class;
	    break;
	  }

      /* Look up a matching visual for the specified class.  */
      if (class == -1
	  || !XMatchVisualInfo (dpy, XScreenNumberOfScreen (screen),
				dpyinfo->n_planes, class, &vinfo))
	fatal ("Invalid visual specification `%s'", SDATA (value));

      dpyinfo->visual = vinfo.visual;
    }
  else
    {
      int n_visuals;
      XVisualInfo *vinfo, vinfo_template;

      dpyinfo->visual = DefaultVisualOfScreen (screen);

      vinfo_template.visualid = XVisualIDFromVisual (dpyinfo->visual);
      vinfo_template.screen = XScreenNumberOfScreen (screen);
      vinfo = XGetVisualInfo (dpy, VisualIDMask | VisualScreenMask,
			      &vinfo_template, &n_visuals);
      if (n_visuals <= 0)
	fatal ("Can't get proper X visual info");

      dpyinfo->n_planes = vinfo->depth;
      XFree ((char *) vinfo);
    }
}


/* Return the X display structure for the display named NAME.
   Open a new connection if necessary.  */

static struct x_display_info *
x_display_info_for_name (Lisp_Object name)
{
  Lisp_Object names;
  struct x_display_info *dpyinfo;

  CHECK_STRING (name);

#if 0
  if (! EQ (Vinitial_window_system, intern ("x")))
    error ("Not using X Windows"); /* That doesn't stop us anymore. */
#endif

  for (dpyinfo = x_display_list, names = x_display_name_list;
       dpyinfo;
       dpyinfo = dpyinfo->next, names = XCDR (names))
    {
      Lisp_Object tem;
      tem = Fstring_equal (XCAR (XCAR (names)), name);
      if (!NILP (tem))
	return dpyinfo;
    }

  /* Use this general default value to start with.  */
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  dpyinfo = x_term_init (name, (char *)0,
			 SSDATA (Vx_resource_name));

  if (dpyinfo == 0)
    error ("Cannot connect to X server %s", SDATA (name));

  x_in_use = 1;
  XSETFASTINT (Vwindow_system_version, 11);

  return dpyinfo;
}


DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0,
       doc: /* Open a connection to a display server.
DISPLAY is the name of the display to connect to.
Optional second arg XRM-STRING is a string of resources in xrdb format.
If the optional third arg MUST-SUCCEED is non-nil,
terminate Emacs if we can't open the connection.
\(In the Nextstep version, the last two arguments are currently ignored.)  */)
  (Lisp_Object display, Lisp_Object xrm_string, Lisp_Object must_succeed)
{
  char *xrm_option;
  struct x_display_info *dpyinfo;

  CHECK_STRING (display);
  if (! NILP (xrm_string))
    CHECK_STRING (xrm_string);

#if 0
  if (! EQ (Vinitial_window_system, intern ("x")))
    error ("Not using X Windows"); /* That doesn't stop us anymore. */
#endif

  if (! NILP (xrm_string))
    xrm_option = SSDATA (xrm_string);
  else
    xrm_option = (char *) 0;

  validate_x_resource_name ();

  /* This is what opens the connection and sets x_current_display.
     This also initializes many symbols, such as those used for input.  */
  dpyinfo = x_term_init (display, xrm_option,
			 SSDATA (Vx_resource_name));

  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
	fatal ("Cannot connect to X server %s.\n\
Check the DISPLAY environment variable or use `-d'.\n\
Also use the `xauth' program to verify that you have the proper\n\
authorization information needed to connect the X server.\n\
An insecure way to solve the problem may be to use `xhost'.\n",
	       SDATA (display));
      else
	error ("Cannot connect to X server %s", SDATA (display));
    }

  x_in_use = 1;

  XSETFASTINT (Vwindow_system_version, 11);
  return Qnil;
}

DEFUN ("x-close-connection", Fx_close_connection,
       Sx_close_connection, 1, 1, 0,
       doc: /* Close the connection to TERMINAL's X server.
For TERMINAL, specify a terminal object, a frame or a display name (a
string).  If TERMINAL is nil, that stands for the selected frame's
terminal.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  if (dpyinfo->reference_count > 0)
    error ("Display still has frames on it");

  x_delete_terminal (dpyinfo->terminal);

  return Qnil;
}

DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* Return the list of display names that Emacs has connections to.  */)
  (void)
{
  Lisp_Object tail, result;

  result = Qnil;
  for (tail = x_display_name_list; CONSP (tail); tail = XCDR (tail))
    result = Fcons (XCAR (XCAR (tail)), result);

  return result;
}

DEFUN ("x-synchronize", Fx_synchronize, Sx_synchronize, 1, 2, 0,
       doc: /* If ON is non-nil, report X errors as soon as the erring request is made.
This function only has an effect on X Windows.  With MS Windows, it is
defined but does nothing.

If ON is nil, allow buffering of requests.
Turning on synchronization prohibits the Xlib routines from buffering
requests and seriously degrades performance, but makes debugging much
easier.
The optional second argument TERMINAL specifies which display to act on.
TERMINAL should be a terminal object, a frame or a display name (a string).
If TERMINAL is omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object on, Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  XSynchronize (dpyinfo->display, !EQ (on, Qnil));

  return Qnil;
}

/* Wait for responses to all X commands issued so far for frame F.  */

void
x_sync (FRAME_PTR f)
{
  BLOCK_INPUT;
  XSync (FRAME_X_DISPLAY (f), False);
  UNBLOCK_INPUT;
}


/***********************************************************************
                           Window properties
 ***********************************************************************/

DEFUN ("x-change-window-property", Fx_change_window_property,
       Sx_change_window_property, 2, 6, 0,
       doc: /* Change window property PROP to VALUE on the X window of FRAME.
PROP must be a string.  VALUE may be a string or a list of conses,
numbers and/or strings.  If an element in the list is a string, it is
converted to an atom and the value of the atom is used.  If an element
is a cons, it is converted to a 32 bit number where the car is the 16
top bits and the cdr is the lower 16 bits.

FRAME nil or omitted means use the selected frame.
If TYPE is given and non-nil, it is the name of the type of VALUE.
If TYPE is not given or nil, the type is STRING.
FORMAT gives the size in bits of each element if VALUE is a list.
It must be one of 8, 16 or 32.
If VALUE is a string or FORMAT is nil or not given, FORMAT defaults to 8.
If OUTER_P is non-nil, the property is changed for the outer X window of
FRAME.  Default is to change on the edit X window.  */)
  (Lisp_Object prop, Lisp_Object value, Lisp_Object frame, Lisp_Object type, Lisp_Object format, Lisp_Object outer_p)
{
  struct frame *f = check_x_frame (frame);
  Atom prop_atom;
  Atom target_type = XA_STRING;
  int element_format = 8;
  unsigned char *data;
  int nelements;
  Window w;

  CHECK_STRING (prop);

  if (! NILP (format))
    {
      CHECK_NUMBER (format);
      element_format = XFASTINT (format);

      if (element_format != 8 && element_format != 16
          && element_format != 32)
        error ("FORMAT must be one of 8, 16 or 32");
    }

  if (CONSP (value))
    {
      ptrdiff_t elsize;

      nelements = x_check_property_data (value);
      if (nelements == -1)
        error ("Bad data in VALUE, must be number, string or cons");

      /* The man page for XChangeProperty:
	     "If the specified format is 32, the property data must be a
	      long array."
	 This applies even if long is more than 32 bits.  The X library
	 converts to 32 bits before sending to the X server.  */
      elsize = element_format == 32 ? sizeof (long) : element_format >> 3;
      data = xnmalloc (nelements, elsize);

      x_fill_property_data (FRAME_X_DISPLAY (f), value, data, element_format);
    }
  else
    {
      CHECK_STRING (value);
      data = SDATA (value);
      if (INT_MAX < SBYTES (value))
	error ("VALUE too long");
      nelements = SBYTES (value);
    }

  BLOCK_INPUT;
  prop_atom = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (prop), False);
  if (! NILP (type))
    {
      CHECK_STRING (type);
      target_type = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (type), False);
    }

  if (! NILP (outer_p)) w = FRAME_OUTER_WINDOW (f);
  else w = FRAME_X_WINDOW (f);

  XChangeProperty (FRAME_X_DISPLAY (f), w,
		   prop_atom, target_type, element_format, PropModeReplace,
		   data, nelements);

  if (CONSP (value)) xfree (data);

  /* Make sure the property is set when we return.  */
  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;

  return value;
}


DEFUN ("x-delete-window-property", Fx_delete_window_property,
       Sx_delete_window_property, 1, 2, 0,
       doc: /* Remove window property PROP from X window of FRAME.
FRAME nil or omitted means use the selected frame.  Value is PROP.  */)
  (Lisp_Object prop, Lisp_Object frame)
{
  struct frame *f = check_x_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop);
  BLOCK_INPUT;
  prop_atom = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (prop), False);
  XDeleteProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), prop_atom);

  /* Make sure the property is removed when we return.  */
  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;

  return prop;
}


DEFUN ("x-window-property", Fx_window_property, Sx_window_property,
       1, 6, 0,
       doc: /* Value is the value of window property PROP on FRAME.
If FRAME is nil or omitted, use the selected frame.

On MS Windows, this function only accepts the PROP and FRAME arguments.

On X Windows, the following optional arguments are also accepted:
If TYPE is nil or omitted, get the property as a string.
Otherwise TYPE is the name of the atom that denotes the type expected.
If SOURCE is non-nil, get the property on that window instead of from
FRAME.  The number 0 denotes the root window.
If DELETE_P is non-nil, delete the property after retrieving it.
If VECTOR_RET_P is non-nil, don't return a string but a vector of values.

Value is nil if FRAME hasn't a property with name PROP or if PROP has
no value of TYPE (always string in the MS Windows case).  */)
  (Lisp_Object prop, Lisp_Object frame, Lisp_Object type, Lisp_Object source, Lisp_Object delete_p, Lisp_Object vector_ret_p)
{
  struct frame *f = check_x_frame (frame);
  Atom prop_atom;
  int rc;
  Lisp_Object prop_value = Qnil;
  unsigned char *tmp_data = NULL;
  Atom actual_type;
  Atom target_type = XA_STRING;
  int actual_format;
  unsigned long actual_size, bytes_remaining;
  Window target_window = FRAME_X_WINDOW (f);
  struct gcpro gcpro1;

  GCPRO1 (prop_value);
  CHECK_STRING (prop);

  if (! NILP (source))
    {
      CONS_TO_INTEGER (source, Window, target_window);
      if (! target_window)
	target_window = FRAME_X_DISPLAY_INFO (f)->root_window;
    }

  BLOCK_INPUT;
  if (STRINGP (type))
    {
      if (strcmp ("AnyPropertyType", SSDATA (type)) == 0)
        target_type = AnyPropertyType;
      else
        target_type = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (type), False);
    }

  prop_atom = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (prop), False);
  rc = XGetWindowProperty (FRAME_X_DISPLAY (f), target_window,
			   prop_atom, 0, 0, False, target_type,
			   &actual_type, &actual_format, &actual_size,
			   &bytes_remaining, &tmp_data);
  if (rc == Success)
    {
      int size = bytes_remaining;

      XFree (tmp_data);
      tmp_data = NULL;

      rc = XGetWindowProperty (FRAME_X_DISPLAY (f), target_window,
			       prop_atom, 0, bytes_remaining,
			       ! NILP (delete_p), target_type,
			       &actual_type, &actual_format,
			       &actual_size, &bytes_remaining,
			       &tmp_data);
      if (rc == Success && tmp_data)
        {
          /* The man page for XGetWindowProperty says:
             "If the returned format is 32, the returned data is represented
             as a long array and should be cast to that type to obtain the
             elements."
             This applies even if long is more than 32 bits, the X library
             converts from 32 bit elements received from the X server to long
             and passes the long array to us.  Thus, for that case memcpy can not
             be used.  We convert to a 32 bit type here, because so much code
             assume on that.

             The bytes and offsets passed to XGetWindowProperty refers to the
             property and those are indeed in 32 bit quantities if format is
             32.  */

          if (32 < BITS_PER_LONG && actual_format == 32)
            {
              unsigned long i;
              int  *idata = (int *) tmp_data;
              long *ldata = (long *) tmp_data;

              for (i = 0; i < actual_size; ++i)
                idata[i] = (int) ldata[i];
            }

          if (NILP (vector_ret_p))
            prop_value = make_string ((char *) tmp_data, size);
          else
            prop_value = x_property_data_to_lisp (f,
                                                  tmp_data,
                                                  actual_type,
                                                  actual_format,
                                                  actual_size);
        }

      if (tmp_data) XFree (tmp_data);
    }

  UNBLOCK_INPUT;
  UNGCPRO;
  return prop_value;
}



/***********************************************************************
				Busy cursor
 ***********************************************************************/

/* Timer function of hourglass_atimer.  TIMER is equal to
   hourglass_atimer.

   Display an hourglass pointer on all frames by mapping the frames'
   hourglass_window.  Set the hourglass_p flag in the frames'
   output_data.x structure to indicate that an hourglass cursor is
   shown on the frames.  */

void
show_hourglass (struct atimer *timer)
{
  /* The timer implementation will cancel this timer automatically
     after this function has run.  Set hourglass_atimer to null
     so that we know the timer doesn't have to be canceled.  */
  hourglass_atimer = NULL;

  if (!hourglass_shown_p)
    {
      Lisp_Object rest, frame;

      BLOCK_INPUT;

      FOR_EACH_FRAME (rest, frame)
	{
	  struct frame *f = XFRAME (frame);

	  if (FRAME_LIVE_P (f) && FRAME_X_P (f) && FRAME_X_DISPLAY (f))
	    {
	      Display *dpy = FRAME_X_DISPLAY (f);

#ifdef USE_X_TOOLKIT
	      if (f->output_data.x->widget)
#else
	      if (FRAME_OUTER_WINDOW (f))
#endif
		{
		  f->output_data.x->hourglass_p = 1;

		  if (!f->output_data.x->hourglass_window)
		    {
		      unsigned long mask = CWCursor;
		      XSetWindowAttributes attrs;
#ifdef USE_GTK
                      Window parent = FRAME_X_WINDOW (f);
#else
                      Window parent = FRAME_OUTER_WINDOW (f);
#endif
		      attrs.cursor = f->output_data.x->hourglass_cursor;

		      f->output_data.x->hourglass_window
			= XCreateWindow (dpy, parent,
					 0, 0, 32000, 32000, 0, 0,
					 InputOnly,
					 CopyFromParent,
					 mask, &attrs);
		    }

		  XMapRaised (dpy, f->output_data.x->hourglass_window);
		  XFlush (dpy);
		}
	    }
	}

      hourglass_shown_p = 1;
      UNBLOCK_INPUT;
    }
}


/* Hide the hourglass pointer on all frames, if it is currently
   shown.  */

void
hide_hourglass (void)
{
  if (hourglass_shown_p)
    {
      Lisp_Object rest, frame;

      BLOCK_INPUT;
      FOR_EACH_FRAME (rest, frame)
	{
	  struct frame *f = XFRAME (frame);

	  if (FRAME_X_P (f)
	      /* Watch out for newly created frames.  */
	      && f->output_data.x->hourglass_window)
	    {
	      XUnmapWindow (FRAME_X_DISPLAY (f),
			    f->output_data.x->hourglass_window);
	      /* Sync here because XTread_socket looks at the
		 hourglass_p flag that is reset to zero below.  */
	      XSync (FRAME_X_DISPLAY (f), False);
	      f->output_data.x->hourglass_p = 0;
	    }
	}

      hourglass_shown_p = 0;
      UNBLOCK_INPUT;
    }
}



/***********************************************************************
				Tool tips
 ***********************************************************************/

static Lisp_Object x_create_tip_frame (struct x_display_info *,
                                       Lisp_Object, Lisp_Object);
static void compute_tip_xy (struct frame *, Lisp_Object, Lisp_Object,
                            Lisp_Object, int, int, int *, int *);

/* The frame of a currently visible tooltip.  */

Lisp_Object tip_frame;

/* If non-nil, a timer started that hides the last tooltip when it
   fires.  */

static Lisp_Object tip_timer;
Window tip_window;

/* If non-nil, a vector of 3 elements containing the last args
   with which x-show-tip was called.  See there.  */

static Lisp_Object last_show_tip_args;


static Lisp_Object
unwind_create_tip_frame (Lisp_Object frame)
{
  Lisp_Object deleted;

  deleted = unwind_create_frame (frame);
  if (EQ (deleted, Qt))
    {
      tip_window = None;
      tip_frame = Qnil;
    }

  return deleted;
}


/* Create a frame for a tooltip on the display described by DPYINFO.
   PARMS is a list of frame parameters.  TEXT is the string to
   display in the tip frame.  Value is the frame.

   Note that functions called here, esp. x_default_parameter can
   signal errors, for instance when a specified color name is
   undefined.  We have to make sure that we're in a consistent state
   when this happens.  */

static Lisp_Object
x_create_tip_frame (struct x_display_info *dpyinfo,
                    Lisp_Object parms,
                    Lisp_Object text)
{
  struct frame *f;
  Lisp_Object frame;
  Lisp_Object name;
  int width, height;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3;
  int face_change_count_before = face_change_count;
  Lisp_Object buffer;
  struct buffer *old_buffer;

  check_x ();

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  parms = Fcopy_alist (parms);

  /* Get the name of the frame to use for resource lookup.  */
  name = x_get_arg (dpyinfo, parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && !EQ (name, Qunbound)
      && !NILP (name))
    error ("Invalid frame name--not a string or nil");

  frame = Qnil;
  GCPRO3 (parms, name, frame);
  f = make_frame (1);
  XSETFRAME (frame, f);

  buffer = Fget_buffer_create (build_string (" *tip*"));
  Fset_window_buffer (FRAME_ROOT_WINDOW (f), buffer, Qnil);
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (buffer));
  BVAR (current_buffer, truncate_lines) = Qnil;
  specbind (Qinhibit_read_only, Qt);
  specbind (Qinhibit_modification_hooks, Qt);
  Ferase_buffer ();
  Finsert (1, &text);
  set_buffer_internal_1 (old_buffer);

  FRAME_CAN_HAVE_SCROLL_BARS (f) = 0;
  record_unwind_protect (unwind_create_tip_frame, frame);

  f->terminal = dpyinfo->terminal;

  /* By setting the output method, we're essentially saying that
     the frame is live, as per FRAME_LIVE_P.  If we get a signal
     from this point on, x_destroy_window might screw up reference
     counts etc.  */
  f->output_method = output_x_window;
  f->output_data.x = (struct x_output *) xmalloc (sizeof (struct x_output));
  memset (f->output_data.x, 0, sizeof (struct x_output));
  f->output_data.x->icon_bitmap = -1;
  FRAME_FONTSET (f) = -1;
  f->output_data.x->scroll_bar_foreground_pixel = -1;
  f->output_data.x->scroll_bar_background_pixel = -1;
#ifdef USE_TOOLKIT_SCROLL_BARS
  f->output_data.x->scroll_bar_top_shadow_pixel = -1;
  f->output_data.x->scroll_bar_bottom_shadow_pixel = -1;
#endif /* USE_TOOLKIT_SCROLL_BARS */
  f->icon_name = Qnil;
  FRAME_X_DISPLAY_INFO (f) = dpyinfo;
  f->output_data.x->parent_desc = FRAME_X_DISPLAY_INFO (f)->root_window;
  f->output_data.x->explicit_parent = 0;

  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!  */
  {
    Lisp_Object black;
    struct gcpro gcpro1;

    /* Function x_decode_color can signal an error.  Make
       sure to initialize color slots so that we won't try
       to free colors we haven't allocated.  */
    FRAME_FOREGROUND_PIXEL (f) = -1;
    FRAME_BACKGROUND_PIXEL (f) = -1;
    f->output_data.x->cursor_pixel = -1;
    f->output_data.x->cursor_foreground_pixel = -1;
    f->output_data.x->border_pixel = -1;
    f->output_data.x->mouse_pixel = -1;

    black = build_string ("black");
    GCPRO1 (black);
    FRAME_FOREGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_BACKGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_foreground_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->border_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->mouse_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    UNGCPRO;
  }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      f->name = build_string (dpyinfo->x_id_name);
      f->explicit_name = 0;
    }
  else
    {
      f->name = name;
      f->explicit_name = 1;
      /* use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  f->resx = dpyinfo->resx;
  f->resy = dpyinfo->resy;

  register_font_driver (&xfont_driver, f);
#ifdef HAVE_FREETYPE
#ifdef HAVE_XFT
  register_font_driver (&xftfont_driver, f);
#else	/* not HAVE_XFT */
  register_font_driver (&ftxfont_driver, f);
#endif	/* not HAVE_XFT */
#endif	/* HAVE_FREETYPE */

  x_default_parameter (f, parms, Qfont_backend, Qnil,
		       "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values that are
     needed to determine window geometry.  */
  x_default_font_parameter (f, parms);

  x_default_parameter (f, parms, Qborder_width, make_number (0),
		       "borderWidth", "BorderWidth", RES_TYPE_NUMBER);

  /* This defaults to 2 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = x_get_arg (dpyinfo, parms, Qinternal_border_width,
			 "internalBorder", "internalBorder", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }

  x_default_parameter (f, parms, Qinternal_border_width, make_number (1),
		       "internalBorderWidth", "internalBorderWidth",
		       RES_TYPE_NUMBER);

  /* Also do the stuff which must be set before the window exists.  */
  x_default_parameter (f, parms, Qforeground_color, build_string ("black"),
		       "foreground", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qbackground_color, build_string ("white"),
		       "background", "Background", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qmouse_color, build_string ("black"),
		       "pointerColor", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qcursor_color, build_string ("black"),
		       "cursorColor", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qborder_color, build_string ("black"),
		       "borderColor", "BorderColor", RES_TYPE_STRING);

#if GLYPH_DEBUG
  image_cache_refcount =
    FRAME_IMAGE_CACHE (f) ? FRAME_IMAGE_CACHE (f)->refcount : 0;
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */

  /* Init faces before x_default_parameter is called for scroll-bar
     parameters because that function calls x_set_scroll_bar_width,
     which calls change_frame_size, which calls Fset_window_buffer,
     which runs hooks, which call Fvertical_motion.  At the end, we
     end up in init_iterator with a null face cache, which should not
     happen.  */
  init_frame_faces (f);

  f->output_data.x->parent_desc = FRAME_X_DISPLAY_INFO (f)->root_window;

  x_figure_window_size (f, parms, 0);

  {
    XSetWindowAttributes attrs;
    unsigned long mask;
    Atom type = FRAME_X_DISPLAY_INFO (f)->Xatom_net_window_type_tooltip;

    BLOCK_INPUT;
    mask = CWBackPixel | CWOverrideRedirect | CWEventMask;
    if (DoesSaveUnders (dpyinfo->screen))
      mask |= CWSaveUnder;

    /* Window managers look at the override-redirect flag to determine
       whether or net to give windows a decoration (Xlib spec, chapter
       3.2.8).  */
    attrs.override_redirect = True;
    attrs.save_under = True;
    attrs.background_pixel = FRAME_BACKGROUND_PIXEL (f);
    /* Arrange for getting MapNotify and UnmapNotify events.  */
    attrs.event_mask = StructureNotifyMask;
    tip_window
      = FRAME_X_WINDOW (f)
      = XCreateWindow (FRAME_X_DISPLAY (f),
		       FRAME_X_DISPLAY_INFO (f)->root_window,
		       /* x, y, width, height */
		       0, 0, 1, 1,
		       /* Border.  */
		       f->border_width,
		       CopyFromParent, InputOutput, CopyFromParent,
		       mask, &attrs);
    XChangeProperty (FRAME_X_DISPLAY (f), tip_window,
                     FRAME_X_DISPLAY_INFO (f)->Xatom_net_window_type,
                     XA_ATOM, 32, PropModeReplace,
                     (unsigned char *)&type, 1);
    UNBLOCK_INPUT;
  }

  x_make_gc (f);

  x_default_parameter (f, parms, Qauto_raise, Qnil,
		       "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qauto_lower, Qnil,
		       "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qcursor_type, Qbox,
		       "cursorType", "CursorType", RES_TYPE_SYMBOL);

  /* Dimensions, especially FRAME_LINES (f), must be done via change_frame_size.
     Change will not be effected unless different from the current
     FRAME_LINES (f).  */
  width = FRAME_COLS (f);
  height = FRAME_LINES (f);
  SET_FRAME_COLS (f, 0);
  FRAME_LINES (f) = 0;
  change_frame_size (f, height, width, 1, 0, 0);

  /* Add `tooltip' frame parameter's default value. */
  if (NILP (Fframe_parameter (frame, Qtooltip)))
    Fmodify_frame_parameters (frame, Fcons (Fcons (Qtooltip, Qt), Qnil));

  /* FIXME - can this be done in a similar way to normal frames?
     http://lists.gnu.org/archive/html/emacs-devel/2007-10/msg00641.html */

  /* Set the `display-type' frame parameter before setting up faces. */
  {
    Lisp_Object disptype;

    if (FRAME_X_DISPLAY_INFO (f)->n_planes == 1)
      disptype = intern ("mono");
    else if (FRAME_X_DISPLAY_INFO (f)->visual->class == GrayScale
             || FRAME_X_DISPLAY_INFO (f)->visual->class == StaticGray)
      disptype = intern ("grayscale");
    else
      disptype = intern ("color");

    if (NILP (Fframe_parameter (frame, Qdisplay_type)))
      Fmodify_frame_parameters (frame, Fcons (Fcons (Qdisplay_type, disptype),
                                              Qnil));
  }

  /* Set up faces after all frame parameters are known.  This call
     also merges in face attributes specified for new frames.

     Frame parameters may be changed if .Xdefaults contains
     specifications for the default font.  For example, if there is an
     `Emacs.default.attributeBackground: pink', the `background-color'
     attribute of the frame get's set, which let's the internal border
     of the tooltip frame appear in pink.  Prevent this.  */
  {
    Lisp_Object bg = Fframe_parameter (frame, Qbackground_color);

    /* Set tip_frame here, so that */
    tip_frame = frame;
    call2 (Qface_set_after_frame_default, frame, Qnil);

    if (!EQ (bg, Fframe_parameter (frame, Qbackground_color)))
      Fmodify_frame_parameters (frame, Fcons (Fcons (Qbackground_color, bg),
					      Qnil));
  }

  f->no_split = 1;

  UNGCPRO;

  /* Now that the frame will be official, it counts as a reference to
     its display and terminal.  */
  FRAME_X_DISPLAY_INFO (f)->reference_count++;
  f->terminal->reference_count++;

  /* It is now ok to make the frame official even if we get an error
     below.  And the frame needs to be on Vframe_list or making it
     visible won't work.  */
  Vframe_list = Fcons (frame, Vframe_list);


  /* Setting attributes of faces of the tooltip frame from resources
     and similar will increment face_change_count, which leads to the
     clearing of all current matrices.  Since this isn't necessary
     here, avoid it by resetting face_change_count to the value it
     had before we created the tip frame.  */
  face_change_count = face_change_count_before;

  /* Discard the unwind_protect.  */
  return unbind_to (count, frame);
}


/* Compute where to display tip frame F.  PARMS is the list of frame
   parameters for F.  DX and DY are specified offsets from the current
   location of the mouse.  WIDTH and HEIGHT are the width and height
   of the tooltip.  Return coordinates relative to the root window of
   the display in *ROOT_X, and *ROOT_Y.  */

static void
compute_tip_xy (struct frame *f, Lisp_Object parms, Lisp_Object dx, Lisp_Object dy, int width, int height, int *root_x, int *root_y)
{
  Lisp_Object left, top;
  int win_x, win_y;
  Window root, child;
  unsigned pmask;

  /* User-specified position?  */
  left = Fcdr (Fassq (Qleft, parms));
  top  = Fcdr (Fassq (Qtop, parms));

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  if (!INTEGERP (left) || !INTEGERP (top))
    {
      BLOCK_INPUT;
      XQueryPointer (FRAME_X_DISPLAY (f), FRAME_X_DISPLAY_INFO (f)->root_window,
		     &root, &child, root_x, root_y, &win_x, &win_y, &pmask);
      UNBLOCK_INPUT;
    }

  if (INTEGERP (top))
    *root_y = XINT (top);
  else if (*root_y + XINT (dy) <= 0)
    *root_y = 0; /* Can happen for negative dy */
  else if (*root_y + XINT (dy) + height
	   <= x_display_pixel_height (FRAME_X_DISPLAY_INFO (f)))
    /* It fits below the pointer */
    *root_y += XINT (dy);
  else if (height + XINT (dy) <= *root_y)
    /* It fits above the pointer.  */
    *root_y -= height + XINT (dy);
  else
    /* Put it on the top.  */
    *root_y = 0;

  if (INTEGERP (left))
    *root_x = XINT (left);
  else if (*root_x + XINT (dx) <= 0)
    *root_x = 0; /* Can happen for negative dx */
  else if (*root_x + XINT (dx) + width
	   <= x_display_pixel_width (FRAME_X_DISPLAY_INFO (f)))
    /* It fits to the right of the pointer.  */
    *root_x += XINT (dx);
  else if (width + XINT (dx) <= *root_x)
    /* It fits to the left of the pointer.  */
    *root_x -= width + XINT (dx);
  else
    /* Put it left-justified on the screen--it ought to fit that way.  */
    *root_x = 0;
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* Show STRING in a "tooltip" window on frame FRAME.
A tooltip window is a small X window displaying a string.

This is an internal function; Lisp code should call `tooltip-show'.

FRAME nil or omitted means use the selected frame.

PARMS is an optional list of frame parameters which can be used to
change the tooltip's appearance.

Automatically hide the tooltip after TIMEOUT seconds.  TIMEOUT nil
means use the default timeout of 5 seconds.

If the list of frame parameters PARMS contains a `left' parameters,
the tooltip is displayed at that x-position.  Otherwise it is
displayed at the mouse position, with offset DX added (default is 5 if
DX isn't specified).  Likewise for the y-position; if a `top' frame
parameter is specified, it determines the y-position of the tooltip
window, otherwise it is displayed at the mouse position, with offset
DY added (default is -10).

A tooltip's maximum size is specified by `x-max-tooltip-size'.
Text larger than the specified size is clipped.  */)
  (Lisp_Object string, Lisp_Object frame, Lisp_Object parms, Lisp_Object timeout, Lisp_Object dx, Lisp_Object dy)
{
  struct frame *f;
  struct window *w;
  int root_x, root_y;
  struct buffer *old_buffer;
  struct text_pos pos;
  int i, width, height, seen_reversed_p;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  int old_windows_or_buffers_changed = windows_or_buffers_changed;
  int count = SPECPDL_INDEX ();

  specbind (Qinhibit_redisplay, Qt);

  GCPRO4 (string, parms, frame, timeout);

  CHECK_STRING (string);
  if (SCHARS (string) == 0)
    string = make_unibyte_string (" ", 1);

  f = check_x_frame (frame);
  if (NILP (timeout))
    timeout = make_number (5);
  else
    CHECK_NATNUM (timeout);

  if (NILP (dx))
    dx = make_number (5);
  else
    CHECK_NUMBER (dx);

  if (NILP (dy))
    dy = make_number (-10);
  else
    CHECK_NUMBER (dy);

#ifdef USE_GTK
  if (x_gtk_use_system_tooltips)
    {
      int ok;

      /* Hide a previous tip, if any.  */
      Fx_hide_tip ();

      BLOCK_INPUT;
      if ((ok = xg_prepare_tooltip (f, string, &width, &height)) != 0)
        {
	  compute_tip_xy (f, parms, dx, dy, width, height, &root_x, &root_y);
          xg_show_tooltip (f, root_x, root_y);
          /* This is used in Fx_hide_tip.  */
          XSETFRAME (tip_frame, f);
        }
      UNBLOCK_INPUT;
      if (ok) goto start_timer;
    }
#endif /* USE_GTK */

  if (NILP (last_show_tip_args))
    last_show_tip_args = Fmake_vector (make_number (3), Qnil);

  if (!NILP (tip_frame))
    {
      Lisp_Object last_string = AREF (last_show_tip_args, 0);
      Lisp_Object last_frame = AREF (last_show_tip_args, 1);
      Lisp_Object last_parms = AREF (last_show_tip_args, 2);

      if (EQ (frame, last_frame)
	  && !NILP (Fequal (last_string, string))
	  && !NILP (Fequal (last_parms, parms)))
	{
	  struct frame *tip_f = XFRAME (tip_frame);

	  /* Only DX and DY have changed.  */
	  if (!NILP (tip_timer))
	    {
	      Lisp_Object timer = tip_timer;
	      tip_timer = Qnil;
	      call1 (Qcancel_timer, timer);
	    }

	  BLOCK_INPUT;
	  compute_tip_xy (tip_f, parms, dx, dy, FRAME_PIXEL_WIDTH (tip_f),
			  FRAME_PIXEL_HEIGHT (tip_f), &root_x, &root_y);
	  XMoveWindow (FRAME_X_DISPLAY (tip_f), FRAME_X_WINDOW (tip_f),
		       root_x, root_y);
	  UNBLOCK_INPUT;
	  goto start_timer;
	}
    }

  /* Hide a previous tip, if any.  */
  Fx_hide_tip ();

  ASET (last_show_tip_args, 0, string);
  ASET (last_show_tip_args, 1, frame);
  ASET (last_show_tip_args, 2, parms);

  /* Add default values to frame parameters.  */
  if (NILP (Fassq (Qname, parms)))
    parms = Fcons (Fcons (Qname, build_string ("tooltip")), parms);
  if (NILP (Fassq (Qinternal_border_width, parms)))
    parms = Fcons (Fcons (Qinternal_border_width, make_number (3)), parms);
  if (NILP (Fassq (Qborder_width, parms)))
    parms = Fcons (Fcons (Qborder_width, make_number (1)), parms);
  if (NILP (Fassq (Qborder_color, parms)))
    parms = Fcons (Fcons (Qborder_color, build_string ("lightyellow")), parms);
  if (NILP (Fassq (Qbackground_color, parms)))
    parms = Fcons (Fcons (Qbackground_color, build_string ("lightyellow")),
		   parms);

  /* Create a frame for the tooltip, and record it in the global
     variable tip_frame.  */
  frame = x_create_tip_frame (FRAME_X_DISPLAY_INFO (f), parms, string);
  f = XFRAME (frame);

  /* Set up the frame's root window.  */
  w = XWINDOW (FRAME_ROOT_WINDOW (f));
  w->left_col = w->top_line = make_number (0);

  if (CONSP (Vx_max_tooltip_size)
      && INTEGERP (XCAR (Vx_max_tooltip_size))
      && XINT (XCAR (Vx_max_tooltip_size)) > 0
      && INTEGERP (XCDR (Vx_max_tooltip_size))
      && XINT (XCDR (Vx_max_tooltip_size)) > 0)
    {
      w->total_cols = XCAR (Vx_max_tooltip_size);
      w->total_lines = XCDR (Vx_max_tooltip_size);
    }
  else
    {
      w->total_cols = make_number (80);
      w->total_lines = make_number (40);
    }

  FRAME_TOTAL_COLS (f) = XINT (w->total_cols);
  adjust_glyphs (f);
  w->pseudo_window_p = 1;

  /* Display the tooltip text in a temporary buffer.  */
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (XWINDOW (FRAME_ROOT_WINDOW (f))->buffer));
  BVAR (current_buffer, truncate_lines) = Qnil;
  clear_glyph_matrix (w->desired_matrix);
  clear_glyph_matrix (w->current_matrix);
  SET_TEXT_POS (pos, BEGV, BEGV_BYTE);
  try_window (FRAME_ROOT_WINDOW (f), pos, TRY_WINDOW_IGNORE_FONTS_CHANGE);

  /* Compute width and height of the tooltip.  */
  width = height = seen_reversed_p = 0;
  for (i = 0; i < w->desired_matrix->nrows; ++i)
    {
      struct glyph_row *row = &w->desired_matrix->rows[i];
      struct glyph *last;
      int row_width;

      /* Stop at the first empty row at the end.  */
      if (!row->enabled_p || !row->displays_text_p)
	break;

      /* Let the row go over the full width of the frame.  */
      row->full_width_p = 1;

      row_width = row->pixel_width;
      if (row->used[TEXT_AREA])
	{
	  /* There's a glyph at the end of rows that is used to place
	     the cursor there.  Don't include the width of this glyph.  */
	  if (!row->reversed_p)
	    {
	      last = &row->glyphs[TEXT_AREA][row->used[TEXT_AREA] - 1];
	      if (INTEGERP (last->object))
		row_width -= last->pixel_width;
	    }
	  else
	    {
	      /* There could be a stretch glyph at the beginning of R2L
		 rows that is produced by extend_face_to_end_of_line.
		 Don't count that glyph.  */
	      struct glyph *g = row->glyphs[TEXT_AREA];

	      if (g->type == STRETCH_GLYPH && INTEGERP (g->object))
		{
		  row_width -= g->pixel_width;
		  seen_reversed_p = 1;
		}
	    }
	}

      height += row->height;
      width = max (width, row_width);
    }

  /* If we've seen partial-length R2L rows, we need to re-adjust the
     tool-tip frame width and redisplay it again, to avoid over-wide
     tips due to the stretch glyph that extends R2L lines to full
     width of the frame.  */
  if (seen_reversed_p)
    {
      /* w->total_cols and FRAME_TOTAL_COLS want the width in columns,
	 not in pixels.  */
      width /= WINDOW_FRAME_COLUMN_WIDTH (w);
      w->total_cols = make_number (width);
      FRAME_TOTAL_COLS (f) = width;
      adjust_glyphs (f);
      clear_glyph_matrix (w->desired_matrix);
      clear_glyph_matrix (w->current_matrix);
      try_window (FRAME_ROOT_WINDOW (f), pos, 0);
      width = height = 0;
      /* Recompute width and height of the tooltip.  */
      for (i = 0; i < w->desired_matrix->nrows; ++i)
	{
	  struct glyph_row *row = &w->desired_matrix->rows[i];
	  struct glyph *last;
	  int row_width;

	  if (!row->enabled_p || !row->displays_text_p)
	    break;
	  row->full_width_p = 1;
	  row_width = row->pixel_width;
	  if (row->used[TEXT_AREA] && !row->reversed_p)
	    {
	      last = &row->glyphs[TEXT_AREA][row->used[TEXT_AREA] - 1];
	      if (INTEGERP (last->object))
		row_width -= last->pixel_width;
	    }

	  height += row->height;
	  width = max (width, row_width);
	}
    }

  /* Add the frame's internal border to the width and height the X
     window should have.  */
  height += 2 * FRAME_INTERNAL_BORDER_WIDTH (f);
  width += 2 * FRAME_INTERNAL_BORDER_WIDTH (f);

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  compute_tip_xy (f, parms, dx, dy, width, height, &root_x, &root_y);

  BLOCK_INPUT;
  XMoveResizeWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		     root_x, root_y, width, height);
  XMapRaised (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
  UNBLOCK_INPUT;

  /* Draw into the window.  */
  w->must_be_updated_p = 1;
  update_single_window (w, 1);

  /* Restore original current buffer.  */
  set_buffer_internal_1 (old_buffer);
  windows_or_buffers_changed = old_windows_or_buffers_changed;

 start_timer:
  /* Let the tip disappear after timeout seconds.  */
  tip_timer = call3 (intern ("run-at-time"), timeout, Qnil,
		     intern ("x-hide-tip"));

  UNGCPRO;
  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* Hide the current tooltip window, if there is any.
Value is t if tooltip was open, nil otherwise.  */)
  (void)
{
  int count;
  Lisp_Object deleted, frame, timer;
  struct gcpro gcpro1, gcpro2;

  /* Return quickly if nothing to do.  */
  if (NILP (tip_timer) && NILP (tip_frame))
    return Qnil;

  frame = tip_frame;
  timer = tip_timer;
  GCPRO2 (frame, timer);
  tip_frame = tip_timer = deleted = Qnil;

  count = SPECPDL_INDEX ();
  specbind (Qinhibit_redisplay, Qt);
  specbind (Qinhibit_quit, Qt);

  if (!NILP (timer))
    call1 (Qcancel_timer, timer);

#ifdef USE_GTK
  {
    /* When using system tooltip, tip_frame is the Emacs frame on which
       the tip is shown.  */
    struct frame *f = XFRAME (frame);
    if (FRAME_LIVE_P (f) && xg_hide_tooltip (f))
      frame = Qnil;
  }
#endif

  if (FRAMEP (frame))
    {
      delete_frame (frame, Qnil);
      deleted = Qt;

#ifdef USE_LUCID
      /* Bloodcurdling hack alert: The Lucid menu bar widget's
	 redisplay procedure is not called when a tip frame over menu
	 items is unmapped.  Redisplay the menu manually...  */
      {
        Widget w;
	struct frame *f = SELECTED_FRAME ();
	w = f->output_data.x->menubar_widget;

	if (!DoesSaveUnders (FRAME_X_DISPLAY_INFO (f)->screen)
	    && w != NULL)
	  {
	    BLOCK_INPUT;
	    xlwmenu_redisplay (w);
	    UNBLOCK_INPUT;
	  }
      }
#endif /* USE_LUCID */
    }

  UNGCPRO;
  return unbind_to (count, deleted);
}



/***********************************************************************
			File selection dialog
 ***********************************************************************/

DEFUN ("x-uses-old-gtk-dialog", Fx_uses_old_gtk_dialog,
       Sx_uses_old_gtk_dialog,
       0, 0, 0,
       doc: /* Return t if the old Gtk+ file selection dialog is used.  */)
  (void)
{
#ifdef USE_GTK
  if (use_dialog_box
      && use_file_dialog
      && have_menus_p ()
      && xg_uses_old_file_dialog ())
    return Qt;
#endif
  return Qnil;
}


#ifdef USE_MOTIF
/* Callback for "OK" and "Cancel" on file selection dialog.  */

static void
file_dialog_cb (Widget widget, XtPointer client_data, XtPointer call_data)
{
  int *result = (int *) client_data;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *) call_data;
  *result = cb->reason;
}


/* Callback for unmapping a file selection dialog.  This is used to
   capture the case where a dialog is closed via a window manager's
   closer button, for example. Using a XmNdestroyCallback didn't work
   in this case.  */

static void
file_dialog_unmap_cb (Widget widget, XtPointer client_data, XtPointer call_data)
{
  int *result = (int *) client_data;
  *result = XmCR_CANCEL;
}

static Lisp_Object
clean_up_file_dialog (Lisp_Object arg)
{
  struct Lisp_Save_Value *p = XSAVE_VALUE (arg);
  Widget dialog = (Widget) p->pointer;

  /* Clean up.  */
  BLOCK_INPUT;
  XtUnmanageChild (dialog);
  XtDestroyWidget (dialog);
  x_menu_set_in_use (0);
  UNBLOCK_INPUT;

  return Qnil;
}


DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 5, 0,
       doc: /* Read file name, prompting with PROMPT in directory DIR.
Use a file selection dialog.  Select DEFAULT-FILENAME in the dialog's file
selection box, if specified.  If MUSTMATCH is non-nil, the returned file
or directory must exist.

This function is only defined on MS Windows, and X Windows with the
Motif or Gtk toolkits.  With the Motif toolkit, ONLY-DIR-P is ignored.
Otherwise, if ONLY-DIR-P is non-nil, the user can only select directories.  */)
  (Lisp_Object prompt, Lisp_Object dir, Lisp_Object default_filename, Lisp_Object mustmatch, Lisp_Object only_dir_p)
{
  int result;
  struct frame *f = SELECTED_FRAME ();
  Lisp_Object file = Qnil;
  Lisp_Object decoded_file;
  Widget dialog, text, help;
  Arg al[10];
  int ac = 0;
  XmString dir_xmstring, pattern_xmstring;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;

  check_x ();

  GCPRO6 (prompt, dir, default_filename, mustmatch, only_dir_p, file);

  if (popup_activated ())
    error ("Trying to use a menu from within a menu-entry");

  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);

  BLOCK_INPUT;

  /* Create the dialog with PROMPT as title, using DIR as initial
     directory and using "*" as pattern.  */
  dir = Fexpand_file_name (dir, Qnil);
  dir_xmstring = XmStringCreateLocalized (SDATA (dir));
  pattern_xmstring = XmStringCreateLocalized ("*");

  XtSetArg (al[ac], XmNtitle, SDATA (prompt)); ++ac;
  XtSetArg (al[ac], XmNdirectory, dir_xmstring); ++ac;
  XtSetArg (al[ac], XmNpattern, pattern_xmstring); ++ac;
  XtSetArg (al[ac], XmNresizePolicy, XmRESIZE_GROW); ++ac;
  XtSetArg (al[ac], XmNdialogStyle, XmDIALOG_APPLICATION_MODAL); ++ac;
  dialog = XmCreateFileSelectionDialog (f->output_data.x->widget,
					"fsb", al, ac);
  XmStringFree (dir_xmstring);
  XmStringFree (pattern_xmstring);

  /* Add callbacks for OK and Cancel.  */
  XtAddCallback (dialog, XmNokCallback, file_dialog_cb,
		 (XtPointer) &result);
  XtAddCallback (dialog, XmNcancelCallback, file_dialog_cb,
		 (XtPointer) &result);
  XtAddCallback (dialog, XmNunmapCallback, file_dialog_unmap_cb,
		 (XtPointer) &result);

  /* Remove the help button since we can't display help.  */
  help = XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON);
  XtUnmanageChild (help);

  /* Mark OK button as default.  */
  XtVaSetValues (XmFileSelectionBoxGetChild (dialog, XmDIALOG_OK_BUTTON),
		 XmNshowAsDefault, True, NULL);

  /* If MUSTMATCH is non-nil, disable the file entry field of the
     dialog, so that the user must select a file from the files list
     box.  We can't remove it because we wouldn't have a way to get at
     the result file name, then.  */
  text = XmFileSelectionBoxGetChild (dialog, XmDIALOG_TEXT);
  if (!NILP (mustmatch))
    {
      Widget label;
      label = XmFileSelectionBoxGetChild (dialog, XmDIALOG_SELECTION_LABEL);
      XtSetSensitive (text, False);
      XtSetSensitive (label, False);
    }

  /* Manage the dialog, so that list boxes get filled.  */
  XtManageChild (dialog);

  if (STRINGP (default_filename))
    {
      XmString default_xmstring;
      Widget wtext = XmFileSelectionBoxGetChild (dialog, XmDIALOG_TEXT);
      Widget list = XmFileSelectionBoxGetChild (dialog, XmDIALOG_LIST);

      XmTextPosition last_pos = XmTextFieldGetLastPosition (wtext);
      XmTextFieldReplace (wtext, 0, last_pos,
                          (SDATA (Ffile_name_nondirectory (default_filename))));

      /* Select DEFAULT_FILENAME in the files list box.  DEFAULT_FILENAME
         must include the path for this to work.  */

      default_xmstring = XmStringCreateLocalized (SDATA (default_filename));

      if (XmListItemExists (list, default_xmstring))
        {
          int item_pos = XmListItemPos (list, default_xmstring);
          /* Select the item and scroll it into view.  */
          XmListSelectPos (list, item_pos, True);
          XmListSetPos (list, item_pos);
        }

      XmStringFree (default_xmstring);
    }

  record_unwind_protect (clean_up_file_dialog, make_save_value (dialog, 0));

  /* Process events until the user presses Cancel or OK.  */
  x_menu_set_in_use (1);
  result = 0;
  while (result == 0)
    {
      XEvent event;
      x_menu_wait_for_event (0);
      XtAppNextEvent (Xt_app_con, &event);
      if (event.type == KeyPress
          && FRAME_X_DISPLAY (f) == event.xkey.display)
        {
          KeySym keysym = XLookupKeysym (&event.xkey, 0);

          /* Pop down on C-g.  */
          if (keysym == XK_g && (event.xkey.state & ControlMask) != 0)
            XtUnmanageChild (dialog);
        }

      (void) x_dispatch_event (&event, FRAME_X_DISPLAY (f));
    }

  /* Get the result.  */
  if (result == XmCR_OK)
    {
      XmString text_string;
      String data;

      XtVaGetValues (dialog, XmNtextString, &text_string, NULL);
      XmStringGetLtoR (text_string, XmFONTLIST_DEFAULT_TAG, &data);
      XmStringFree (text_string);
      file = build_string (data);
      XtFree (data);
    }
  else
    file = Qnil;

  UNBLOCK_INPUT;
  UNGCPRO;

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    Fsignal (Qquit, Qnil);

  decoded_file = DECODE_FILE (file);

  return unbind_to (count, decoded_file);
}

#endif /* USE_MOTIF */

#ifdef USE_GTK

static Lisp_Object
clean_up_dialog (Lisp_Object arg)
{
  x_menu_set_in_use (0);

  return Qnil;
}

DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 5, 0,
       doc: /* Read file name, prompting with PROMPT in directory DIR.
Use a file selection dialog.  Select DEFAULT-FILENAME in the dialog's file
selection box, if specified.  If MUSTMATCH is non-nil, the returned file
or directory must exist.

This function is only defined on MS Windows, and X Windows with the
Motif or Gtk toolkits.  With the Motif toolkit, ONLY-DIR-P is ignored.
Otherwise, if ONLY-DIR-P is non-nil, the user can only select directories.  */)
  (Lisp_Object prompt, Lisp_Object dir, Lisp_Object default_filename, Lisp_Object mustmatch, Lisp_Object only_dir_p)
{
  FRAME_PTR f = SELECTED_FRAME ();
  char *fn;
  Lisp_Object file = Qnil;
  Lisp_Object decoded_file;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;
  char *cdef_file;

  check_x ();

  GCPRO6 (prompt, dir, default_filename, mustmatch, only_dir_p, file);

  if (popup_activated ())
    error ("Trying to use a menu from within a menu-entry");

  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);
  record_unwind_protect (clean_up_dialog, Qnil);

  BLOCK_INPUT;

  if (STRINGP (default_filename))
    cdef_file = SSDATA (default_filename);
  else
    cdef_file = SSDATA (dir);

  fn = xg_get_file_name (f, SSDATA (prompt), cdef_file,
                         ! NILP (mustmatch),
                         ! NILP (only_dir_p));

  if (fn)
    {
      file = build_string (fn);
      xfree (fn);
    }

  UNBLOCK_INPUT;
  UNGCPRO;

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    Fsignal (Qquit, Qnil);

  decoded_file = DECODE_FILE (file);

  return unbind_to (count, decoded_file);
}


#ifdef HAVE_FREETYPE

DEFUN ("x-select-font", Fx_select_font, Sx_select_font, 0, 2, 0,
       doc: /* Read a font name using a GTK font selection dialog.
Return a GTK-style font string corresponding to the selection.

If FRAME is omitted or nil, it defaults to the selected frame. */)
  (Lisp_Object frame, Lisp_Object ignored)
{
  FRAME_PTR f = check_x_frame (frame);
  char *name;
  Lisp_Object font;
  Lisp_Object font_param;
  char *default_name = NULL;
  struct gcpro gcpro1, gcpro2;
  int count = SPECPDL_INDEX ();

  check_x ();

  if (popup_activated ())
    error ("Trying to use a menu from within a menu-entry");

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);
  record_unwind_protect (clean_up_dialog, Qnil);

  BLOCK_INPUT;

  GCPRO2 (font_param, font);

  XSETFONT (font, FRAME_FONT (f));
  font_param = Ffont_get (font, intern (":name"));
  if (STRINGP (font_param))
    default_name = xstrdup (SSDATA (font_param));
  else
    {
      font_param = Fframe_parameter (frame, Qfont_param);
      if (STRINGP (font_param))
        default_name = xstrdup (SSDATA (font_param));
    }

  if (default_name == NULL && x_last_font_name != NULL)
    default_name = xstrdup (x_last_font_name);

  /* Convert fontconfig names to Gtk names, i.e. remove - before number */
  if (default_name)
    {
      char *p = strrchr (default_name, '-');
      if (p)
        {
          char *ep = p+1;
          while (isdigit (*ep))
            ++ep;
          if (*ep == '\0') *p = ' ';
        }
    }

  name = xg_get_font_name (f, default_name);
  xfree (default_name);

  if (name)
    {
      font = build_string (name);
      g_free (x_last_font_name);
      x_last_font_name = name;
    }

  UNBLOCK_INPUT;

  if (NILP (font))
    Fsignal (Qquit, Qnil);

  return unbind_to (count, font);
}
#endif /* HAVE_FREETYPE */

#endif /* USE_GTK */


/***********************************************************************
			       Keyboard
 ***********************************************************************/

#ifdef HAVE_XKBGETKEYBOARD
#include <X11/XKBlib.h>
#include <X11/keysym.h>
#endif

DEFUN ("x-backspace-delete-keys-p", Fx_backspace_delete_keys_p,
       Sx_backspace_delete_keys_p, 0, 1, 0,
       doc: /* Check if both Backspace and Delete keys are on the keyboard of FRAME.
FRAME nil means use the selected frame.
Value is t if we know that both keys are present, and are mapped to the
usual X keysyms.  Value is `lambda' if we cannot determine if both keys are
present and mapped to the usual X keysyms.  */)
  (Lisp_Object frame)
{
#ifdef HAVE_XKBGETKEYBOARD
  XkbDescPtr kb;
  struct frame *f = check_x_frame (frame);
  Display *dpy = FRAME_X_DISPLAY (f);
  Lisp_Object have_keys;
  int major, minor, op, event, error_code;

  BLOCK_INPUT;

  /* Check library version in case we're dynamically linked.  */
  major = XkbMajorVersion;
  minor = XkbMinorVersion;
  if (!XkbLibraryVersion (&major, &minor))
    {
      UNBLOCK_INPUT;
      return Qlambda;
    }

  /* Check that the server supports XKB.  */
  major = XkbMajorVersion;
  minor = XkbMinorVersion;
  if (!XkbQueryExtension (dpy, &op, &event, &error_code, &major, &minor))
    {
      UNBLOCK_INPUT;
      return Qlambda;
    }

  /* In this code we check that the keyboard has physical keys with names
     that start with BKSP (Backspace) and DELE (Delete), and that they
     generate keysym XK_BackSpace and XK_Delete respectively.
     This function is used to test if normal-erase-is-backspace should be
     turned on.
     An alternative approach would be to just check if XK_BackSpace and
     XK_Delete are mapped to any key.  But if any of those are mapped to
     some non-intuitive key combination (Meta-Shift-Ctrl-whatever) and the
     user doesn't know about it, it is better to return false here.
     It is more obvious to the user what to do if she/he has two keys
     clearly marked with names/symbols and one key does something not
     expected (i.e. she/he then tries the other).
     The cases where Backspace/Delete is mapped to some other key combination
     are rare, and in those cases, normal-erase-is-backspace can be turned on
     manually.  */

  have_keys = Qnil;
  kb = XkbGetMap (dpy, XkbAllMapComponentsMask, XkbUseCoreKbd);
  if (kb)
    {
      int delete_keycode = 0, backspace_keycode = 0, i;

      if (XkbGetNames (dpy, XkbAllNamesMask, kb) == Success)
	{
	  for (i = kb->min_key_code;
	       (i < kb->max_key_code
		&& (delete_keycode == 0 || backspace_keycode == 0));
	       ++i)
	    {
	      /* The XKB symbolic key names can be seen most easily in
		 the PS file generated by `xkbprint -label name
		 $DISPLAY'.  */
	      if (memcmp ("DELE", kb->names->keys[i].name, 4) == 0)
		delete_keycode = i;
	      else if (memcmp ("BKSP", kb->names->keys[i].name, 4) == 0)
		backspace_keycode = i;
	    }

	  XkbFreeNames (kb, 0, True);
	}

      XkbFreeClientMap (kb, 0, True);

      if (delete_keycode
	  && backspace_keycode
	  && XKeysymToKeycode (dpy, XK_Delete) == delete_keycode
	  && XKeysymToKeycode (dpy, XK_BackSpace) == backspace_keycode)
	have_keys = Qt;
    }
  UNBLOCK_INPUT;
  return have_keys;
#else /* not HAVE_XKBGETKEYBOARD */
  return Qlambda;
#endif /* not HAVE_XKBGETKEYBOARD */
}



/***********************************************************************
			    Initialization
 ***********************************************************************/

/* Keep this list in the same order as frame_parms in frame.c.
   Use 0 for unsupported frame parameters.  */

frame_parm_handler x_frame_parm_handlers[] =
{
  x_set_autoraise,
  x_set_autolower,
  x_set_background_color,
  x_set_border_color,
  x_set_border_width,
  x_set_cursor_color,
  x_set_cursor_type,
  x_set_font,
  x_set_foreground_color,
  x_set_icon_name,
  x_set_icon_type,
  x_set_internal_border_width,
  x_set_menu_bar_lines,
  x_set_mouse_color,
  x_explicitly_set_name,
  x_set_scroll_bar_width,
  x_set_title,
  x_set_unsplittable,
  x_set_vertical_scroll_bars,
  x_set_visibility,
  x_set_tool_bar_lines,
  x_set_scroll_bar_foreground,
  x_set_scroll_bar_background,
  x_set_screen_gamma,
  x_set_line_spacing,
  x_set_fringe_width,
  x_set_fringe_width,
  x_set_wait_for_wm,
  x_set_fullscreen,
  x_set_font_backend,
  x_set_alpha,
  x_set_sticky,
  x_set_tool_bar_position,
};

void
syms_of_xfns (void)
{
  /* This is zero if not using X windows.  */
  x_in_use = 0;

  /* The section below is built by the lisp expression at the top of the file,
     just above where these variables are declared.  */
  /*&&& init symbols here &&&*/
  DEFSYM (Qnone, "none");
  DEFSYM (Qsuppress_icon, "suppress-icon");
  DEFSYM (Qundefined_color, "undefined-color");
  DEFSYM (Qcompound_text, "compound-text");
  DEFSYM (Qcancel_timer, "cancel-timer");
  DEFSYM (Qfont_param, "font-parameter");
  /* This is the end of symbol initialization.  */

  Fput (Qundefined_color, Qerror_conditions,
	pure_cons (Qundefined_color, pure_cons (Qerror, Qnil)));
  Fput (Qundefined_color, Qerror_message,
	make_pure_c_string ("Undefined color"));

  DEFVAR_LISP ("x-pointer-shape", Vx_pointer_shape,
    doc: /* The shape of the pointer when over text.
Changing the value does not affect existing frames
unless you set the mouse color.  */);
  Vx_pointer_shape = Qnil;

#if 0 /* This doesn't really do anything.  */
  DEFVAR_LISP ("x-nontext-pointer-shape", Vx_nontext_pointer_shape,
    doc: /* The shape of the pointer when not over text.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
#endif
  Vx_nontext_pointer_shape = Qnil;

  DEFVAR_LISP ("x-hourglass-pointer-shape", Vx_hourglass_pointer_shape,
    doc: /* The shape of the pointer when Emacs is busy.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_hourglass_pointer_shape = Qnil;

#if 0 /* This doesn't really do anything.  */
  DEFVAR_LISP ("x-mode-pointer-shape", Vx_mode_pointer_shape,
    doc: /* The shape of the pointer when over the mode line.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
#endif
  Vx_mode_pointer_shape = Qnil;

  DEFVAR_LISP ("x-sensitive-text-pointer-shape",
	      Vx_sensitive_text_pointer_shape,
	       doc: /* The shape of the pointer when over mouse-sensitive text.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_sensitive_text_pointer_shape = Qnil;

  DEFVAR_LISP ("x-window-horizontal-drag-cursor",
	      Vx_window_horizontal_drag_shape,
  doc: /* Pointer shape to use for indicating a window can be dragged horizontally.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_horizontal_drag_shape = Qnil;

  DEFVAR_LISP ("x-cursor-fore-pixel", Vx_cursor_fore_pixel,
    doc: /* A string indicating the foreground color of the cursor box.  */);
  Vx_cursor_fore_pixel = Qnil;

  DEFVAR_LISP ("x-max-tooltip-size", Vx_max_tooltip_size,
    doc: /* Maximum size for tooltips.
Value is a pair (COLUMNS . ROWS).  Text larger than this is clipped.  */);
  Vx_max_tooltip_size = Fcons (make_number (80), make_number (40));

  DEFVAR_LISP ("x-no-window-manager", Vx_no_window_manager,
    doc: /* Non-nil if no X window manager is in use.
Emacs doesn't try to figure this out; this is always nil
unless you set it to something else.  */);
  /* We don't have any way to find this out, so set it to nil
     and maybe the user would like to set it to t.  */
  Vx_no_window_manager = Qnil;

  DEFVAR_LISP ("x-pixel-size-width-font-regexp",
	       Vx_pixel_size_width_font_regexp,
    doc: /* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.

Since Emacs gets width of a font matching with this regexp from
PIXEL_SIZE field of the name, font finding mechanism gets faster for
such a font.  This is especially effective for such large fonts as
Chinese, Japanese, and Korean.  */);
  Vx_pixel_size_width_font_regexp = Qnil;

/* This is not ifdef:ed, so other builds than GTK can customize it.  */
  DEFVAR_BOOL ("x-gtk-use-old-file-dialog", x_gtk_use_old_file_dialog,
    doc: /* *Non-nil means prompt with the old GTK file selection dialog.
If nil or if the file selection dialog is not available, the new GTK file
chooser is used instead.  To turn off all file dialogs set the
variable `use-file-dialog'.  */);
  x_gtk_use_old_file_dialog = 0;

  DEFVAR_BOOL ("x-gtk-show-hidden-files", x_gtk_show_hidden_files,
    doc: /* *If non-nil, the GTK file chooser will by default show hidden files.
Note that this is just the default, there is a toggle button on the file
chooser to show or not show hidden files on a case by case basis.  */);
  x_gtk_show_hidden_files = 0;

  DEFVAR_BOOL ("x-gtk-file-dialog-help-text", x_gtk_file_dialog_help_text,
    doc: /* *If non-nil, the GTK file chooser will show additional help text.
If more space for files in the file chooser dialog is wanted, set this to nil
to turn the additional text off.  */);
  x_gtk_file_dialog_help_text = 1;

  DEFVAR_BOOL ("x-gtk-whole-detached-tool-bar", x_gtk_whole_detached_tool_bar,
    doc: /* *If non-nil, a detached tool bar is shown in full.
The default is to just show an arrow and pressing on that arrow shows
the tool bar buttons.  */);
  x_gtk_whole_detached_tool_bar = 0;

  DEFVAR_BOOL ("x-gtk-use-system-tooltips", x_gtk_use_system_tooltips,
    doc: /* *If non-nil with a Gtk+ built Emacs, the Gtk+ tooltip is used.
Otherwise use Emacs own tooltip implementation.
When using Gtk+ tooltips, the tooltip face is not used.  */);
  x_gtk_use_system_tooltips = 1;

  Fprovide (intern_c_string ("x"), Qnil);

#ifdef USE_X_TOOLKIT
  Fprovide (intern_c_string ("x-toolkit"), Qnil);
#ifdef USE_MOTIF
  Fprovide (intern_c_string ("motif"), Qnil);

  DEFVAR_LISP ("motif-version-string", Vmotif_version_string,
	       doc: /* Version info for LessTif/Motif.  */);
  Vmotif_version_string = build_string (XmVERSION_STRING);
#endif /* USE_MOTIF */
#endif /* USE_X_TOOLKIT */

#ifdef USE_GTK
  /* Provide x-toolkit also for GTK.  Internally GTK does not use Xt so it
     is not an X toolkit in that sense (USE_X_TOOLKIT is not defined).
     But for a user it is a toolkit for X, and indeed, configure
     accepts --with-x-toolkit=gtk.  */
  Fprovide (intern_c_string ("x-toolkit"), Qnil);
  Fprovide (intern_c_string ("gtk"), Qnil);
  Fprovide (intern_c_string ("move-toolbar"), Qnil);

  DEFVAR_LISP ("gtk-version-string", Vgtk_version_string,
               doc: /* Version info for GTK+.  */);
  {
    char gtk_version[40];
    g_snprintf (gtk_version, sizeof (gtk_version), "%u.%u.%u",
                GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
    Vgtk_version_string = make_pure_string (gtk_version, strlen (gtk_version), strlen (gtk_version), 0);
  }
#endif /* USE_GTK */

  /* X window properties.  */
  defsubr (&Sx_change_window_property);
  defsubr (&Sx_delete_window_property);
  defsubr (&Sx_window_property);

  defsubr (&Sxw_display_color_p);
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Sxw_color_defined_p);
  defsubr (&Sxw_color_values);
  defsubr (&Sx_server_max_request_size);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Sx_display_mm_width);
  defsubr (&Sx_display_mm_height);
  defsubr (&Sx_display_screens);
  defsubr (&Sx_display_planes);
  defsubr (&Sx_display_color_cells);
  defsubr (&Sx_display_visual_class);
  defsubr (&Sx_display_backing_store);
  defsubr (&Sx_display_save_under);
  defsubr (&Sx_wm_set_size_hint);
  defsubr (&Sx_create_frame);
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_connection);
  defsubr (&Sx_display_list);
  defsubr (&Sx_synchronize);
  defsubr (&Sx_focus_frame);
  defsubr (&Sx_backspace_delete_keys_p);

  /* Setting callback functions for fontset handler.  */
  check_window_system_func = check_x;

  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);
  tip_timer = Qnil;
  staticpro (&tip_timer);
  tip_frame = Qnil;
  staticpro (&tip_frame);

  last_show_tip_args = Qnil;
  staticpro (&last_show_tip_args);

  defsubr (&Sx_uses_old_gtk_dialog);
#if defined (USE_MOTIF) || defined (USE_GTK)
  defsubr (&Sx_file_dialog);
#endif

#if defined (USE_GTK) && defined (HAVE_FREETYPE)
  defsubr (&Sx_select_font);
  x_last_font_name = NULL;
#endif
}

#endif /* HAVE_X_WINDOWS */
