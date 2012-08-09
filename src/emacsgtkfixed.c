/* A Gtk Widget that inherits GtkFixed, but can be shrunk.
This file is only use when compiling with Gtk+ 3.

Copyright (C) 2011-2012  Free Software Foundation, Inc.

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

#include "emacsgtkfixed.h"
#include <signal.h>
#include <stdio.h>
#include <setjmp.h>
#include "lisp.h"
#include "frame.h"
#include "xterm.h"

struct _EmacsFixedPrivate
{
  struct frame *f;
};


static void emacs_fixed_get_preferred_width  (GtkWidget *widget,
                                              gint      *minimum,
                                              gint      *natural);
static void emacs_fixed_get_preferred_height (GtkWidget *widget,
                                              gint      *minimum,
                                              gint      *natural);
G_DEFINE_TYPE (EmacsFixed, emacs_fixed, GTK_TYPE_FIXED)

static void
emacs_fixed_class_init (EmacsFixedClass *klass)
{
  GtkWidgetClass *widget_class;
  GtkFixedClass *fixed_class;

  widget_class = (GtkWidgetClass*) klass;
  fixed_class = (GtkFixedClass*) klass;

  widget_class->get_preferred_width = emacs_fixed_get_preferred_width;
  widget_class->get_preferred_height = emacs_fixed_get_preferred_height;
  g_type_class_add_private (klass, sizeof (EmacsFixedPrivate));
}

static GType
emacs_fixed_child_type (GtkFixed *container)
{
  return GTK_TYPE_WIDGET;
}

static void
emacs_fixed_init (EmacsFixed *fixed)
{
  fixed->priv = G_TYPE_INSTANCE_GET_PRIVATE (fixed, EMACS_TYPE_FIXED,
                                             EmacsFixedPrivate);
  fixed->priv->f = 0;
}

/**
 * emacs_fixed_new:
 *
 * Creates a new #EmacsFixed.
 *
 * Returns: a new #EmacsFixed.
 */
GtkWidget*
emacs_fixed_new (struct frame *f)
{
  EmacsFixed *fixed = g_object_new (EMACS_TYPE_FIXED, NULL);
  EmacsFixedPrivate *priv = fixed->priv;
  priv->f = f;
  return GTK_WIDGET (fixed);
}

static void
emacs_fixed_get_preferred_width (GtkWidget *widget,
                                 gint      *minimum,
                                 gint      *natural)
{
  EmacsFixed *fixed = EMACS_FIXED (widget);
  EmacsFixedPrivate *priv = fixed->priv;
  int w = priv->f->output_data.x->size_hints.min_width;
  if (minimum) *minimum = w;
  if (natural) *natural = w;
}

static void
emacs_fixed_get_preferred_height (GtkWidget *widget,
                                  gint      *minimum,
                                  gint      *natural)
{
  EmacsFixed *fixed = EMACS_FIXED (widget);
  EmacsFixedPrivate *priv = fixed->priv;
  int h = priv->f->output_data.x->size_hints.min_height;
  if (minimum) *minimum = h;
  if (natural) *natural = h;
}


/* Override the X function so we can intercept Gtk+ 3 calls.
   Use our values for min_width/height so that KDE don't freak out
   (Bug#8919), and so users can resize our frames as they wish.  */

void
XSetWMSizeHints (Display* d,
                 Window w,
                 XSizeHints* hints,
                 Atom prop)
{
  struct x_display_info *dpyinfo = x_display_info_for_display (d);
  struct frame *f = x_top_window_to_frame (dpyinfo, w);
  long data[18];
  data[0] = hints->flags;
  data[1] = hints->x;
  data[2] = hints->y;
  data[3] = hints->width;
  data[4] = hints->height;
  data[5] = hints->min_width;
  data[6] = hints->min_height;
  data[7] = hints->max_width;
  data[8] = hints->max_height;
  data[9] = hints->width_inc;
  data[10] = hints->height_inc;
  data[11] = hints->min_aspect.x;
  data[12] = hints->min_aspect.y;
  data[13] = hints->max_aspect.x;
  data[14] = hints->max_aspect.y;
  data[15] = hints->base_width;
  data[16] = hints->base_height;
  data[17] = hints->win_gravity;

  if ((hints->flags & PMinSize) && f)
    {
      int w = f->output_data.x->size_hints.min_width;
      int h = f->output_data.x->size_hints.min_height;
      data[5] = w;
      data[6] = h;
    }

  XChangeProperty (d, w, prop, XA_WM_SIZE_HINTS, 32, PropModeReplace,
		   (unsigned char *) data, 18);
}

/* Override this X11 function.
   This function is in the same X11 file as the one above.  So we must
   provide it also.  */

void
XSetWMNormalHints (Display *d, Window w, XSizeHints *hints)
{
  XSetWMSizeHints (d, w, hints, XA_WM_NORMAL_HINTS);
}
