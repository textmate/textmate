/* Defines some widget utility functions.

Copyright (C) 1992 Lucid, Inc.
Copyright (C) 1994, 2001-2012 Free Software Foundation, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <setjmp.h>
#include <lisp.h>

#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include <X11/ObjectP.h>
#include "lwlib-utils.h"
#include "lwlib.h"

/* Redisplay the contents of the widget, without first clearing it. */
void
XtNoClearRefreshWidget (Widget widget)
{
  XEvent event;

  event.type = Expose;
  event.xexpose.serial = 0;
  event.xexpose.send_event = 0;
  event.xexpose.display = XtDisplay (widget);
  event.xexpose.window = XtWindow (widget);
  event.xexpose.x = 0;
  event.xexpose.y = 0;
  event.xexpose.width = widget->core.width;
  event.xexpose.height = widget->core.height;
  event.xexpose.count = 0;

  (*widget->core.widget_class->core_class.expose)
    (widget, &event, (Region)NULL);
}


/*
 * Apply a function to all the subwidgets of a given widget recursively.
*/
void
XtApplyToWidgets (Widget w, XtApplyToWidgetsProc proc, XtPointer arg)
{
  if (XtIsComposite (w))
    {
      CompositeWidget cw = (CompositeWidget) w;
      /* We have to copy the children list before mapping over it, because
	 the procedure might add/delete elements, which would lose badly.
	 */
      int nkids = cw->composite.num_children;
      Widget *kids = (Widget *) xmalloc (sizeof (Widget) * nkids);
      int i;
      memcpy ((char *) kids, (char *) cw->composite.children,
	      sizeof (Widget) * nkids);
      for (i = 0; i < nkids; i++)
/* This prevent us from using gadgets, why is it here? */
/*	if (XtIsWidget (kids [i])) */
	  {
	    /* do the kiddies first in case we're destroying */
	    XtApplyToWidgets (kids [i], proc, arg);
	    proc (kids [i], arg);
	  }
      xfree (kids);
    }
}


/*
 * Apply a function to all the subwidgets of a given widget recursively.
 * Stop as soon as the function returns non NULL and returns this as a value.
 */
void *
XtApplyUntilToWidgets (Widget w, XtApplyUntilToWidgetsProc proc, XtPointer arg)
{
  void* result;
  if (XtIsComposite (w))
    {
      CompositeWidget cw = (CompositeWidget)w;
      int i;
      for (i = 0; i < cw->composite.num_children; i++)
	if (XtIsWidget (cw->composite.children [i])){
	  result = proc (cw->composite.children [i], arg);
	  if (result)
	    return result;
	  result = XtApplyUntilToWidgets (cw->composite.children [i], proc,
					  arg);
	  if (result)
	    return result;
	}
    }
  return NULL;
}


/*
 * Returns a copy of the list of all children of a composite widget
 */
Widget *
XtCompositeChildren (Widget widget, unsigned int *number)
{
  CompositeWidget cw = (CompositeWidget)widget;
  Widget* result;
  int n;
  int i;

  if (!XtIsComposite (widget))
    {
      *number = 0;
      return NULL;
    }
  n = cw->composite.num_children;
  result = (Widget*)(void*)XtMalloc (n * sizeof (Widget));
  *number = n;
  for (i = 0; i < n; i++)
    result [i] = cw->composite.children [i];
  return result;
}

Boolean
XtWidgetBeingDestroyedP (Widget widget)
{
  return widget->core.being_destroyed;
}
