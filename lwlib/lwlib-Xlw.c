/* The lwlib interface to "xlwmenu" menus.

Copyright (C) 1992 Lucid, Inc.
Copyright (C) 1994, 2000-2012 Free Software Foundation, Inc.

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

#include "lwlib-Xlw.h"
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/ObjectP.h>
#include <X11/CompositeP.h>
#include <X11/Shell.h>
#include "xlwmenu.h"

#if 0

#include <stdio.h>

/* Print the complete X resource name of widget WIDGET to stderr.
   This is sometimes handy to have available.  */

void
x_print_complete_resource_name (Widget widget)
{
  int i;
  String names[100];

  for (i = 0; i < 100 && widget != NULL; ++i)
    {
      names[i] = XtName (widget);
      widget = XtParent (widget);
    }

  for (--i; i >= 1; --i)
    fprintf (stderr, "%s.", names[i]);
  fprintf (stderr, "%s\n", names[0]);
}

#endif /* 0 */


/* Menu callbacks */

/* Callback XtNhighlightCallback for Lucid menus.  W is the menu
   widget, CLIENT_DATA contains a pointer to the widget_instance
   for the menu, CALL_DATA contains a pointer to the widget_value
   structure for the highlighted menu item.  The latter may be null
   if there isn't any highlighted menu item.  */

static void
highlight_hook (Widget w, XtPointer client_data, XtPointer call_data)
{
  widget_instance *instance = (widget_instance *) client_data;

  if (instance->info->highlight_cb
      && !w->core.being_destroyed)
    instance->info->highlight_cb (w, instance->info->id, call_data);
}

static void
enter_hook (Widget w, XtPointer client_data, XtPointer call_data)
{
  highlight_hook (w, client_data, call_data);
}

static void
leave_hook (Widget w, XtPointer client_data, XtPointer call_data)
{
  highlight_hook (w, client_data, NULL);
}


static void
pre_hook (Widget w, XtPointer client_data, XtPointer call_data)
{
  widget_instance* instance = (widget_instance*)client_data;
  widget_value* val;

  if (w->core.being_destroyed)
    return;

  val = lw_get_widget_value_for_widget (instance, w);
  if (instance->info->pre_activate_cb)
    instance->info->pre_activate_cb (w, instance->info->id,
				     val ? val->call_data : NULL);
}

static void
pick_hook (Widget w, XtPointer client_data, XtPointer call_data)
{
  widget_instance* instance = (widget_instance*)client_data;
  widget_value* contents_val = (widget_value*)call_data;
  widget_value* widget_val;
  XtPointer widget_arg;

  if (w->core.being_destroyed)
    return;

  if (instance->info->selection_cb && contents_val && contents_val->enabled
      && !contents_val->contents)
    instance->info->selection_cb (w, instance->info->id,
				  contents_val->call_data);

  widget_val = lw_get_widget_value_for_widget (instance, w);
  widget_arg = widget_val ? widget_val->call_data : NULL;
  if (instance->info->post_activate_cb)
    instance->info->post_activate_cb (w, instance->info->id, widget_arg);

}

/* creation functions */

static Widget
xlw_create_menubar (widget_instance *instance)
{
  Widget widget;
  Arg al[5];
  int ac = 0;

  XtSetArg (al[ac], XtNmenu, instance->info->val); ac++;
#ifdef emacs
  XtSetArg (al[ac], XtNshowGrip, 0); ac++;
  XtSetArg (al[ac], XtNresizeToPreferred, 1); ac++;
  XtSetArg (al[ac], XtNallowResize, 1); ac++;
#endif

  /* This used to use XtVaCreateWidget, but an old Xt version
     has a bug in XtVaCreateWidget that frees instance->info->name.  */
  widget
    = XtCreateWidget (instance->info->name, xlwMenuWidgetClass,
		      instance->parent, al, ac);

  XtAddCallback (widget, XtNopen, pre_hook, (XtPointer)instance);
  XtAddCallback (widget, XtNselect, pick_hook, (XtPointer)instance);
  XtAddCallback (widget, XtNleaveCallback, leave_hook, (XtPointer)instance);
  XtAddCallback (widget, XtNenterCallback, enter_hook, (XtPointer)instance);
  return widget;
}

static Widget
xlw_create_popup_menu (widget_instance *instance)
{
  Widget popup_shell
    = XtCreatePopupShell (instance->info->name, overrideShellWidgetClass,
			  instance->parent, NULL, 0);

  Widget widget;
  Arg al[2];
  int ac = 0;

  XtSetArg (al[ac], XtNmenu, instance->info->val); ac++;
  XtSetArg (al[ac], XtNhorizontal, False); ac++;

  /* This used to use XtVaManagedCreateWidget, but an old Xt version
     has a bug in XtVaManagedCreateWidget that frees instance->info->name.  */
  widget
    = XtCreateManagedWidget ("popup", xlwMenuWidgetClass,
			     popup_shell, al, ac);

  XtAddCallback (widget, XtNselect, pick_hook, (XtPointer)instance);
  XtAddCallback (widget, XtNleaveCallback, leave_hook, (XtPointer)instance);
  XtAddCallback (widget, XtNenterCallback, enter_hook, (XtPointer)instance);

  return popup_shell;
}

widget_creation_entry
xlw_creation_table [] =
{
  {"menubar", xlw_create_menubar},
  {"popup", xlw_create_popup_menu},
  {NULL, NULL}
};

Boolean
lw_lucid_widget_p (Widget widget)
{
  WidgetClass the_class = XtClass (widget);

  if (the_class == xlwMenuWidgetClass)
    return True;
  if (the_class == overrideShellWidgetClass)
    return (XtClass (((CompositeWidget)widget)->composite.children [0])
	    == xlwMenuWidgetClass);
  return False;
}

void
xlw_update_one_widget (widget_instance* instance, Widget widget,
		       widget_value* val, Boolean deep_p)
{
  Arg al[1];

  /* This used to use XtVaSetValues, but some old Xt versions
     that have a bug in XtVaCreateWidget might have it here too.  */
  XtSetArg (al[0], XtNmenu, instance->info->val);

  XtSetValues (widget, al, 1);
}

void
xlw_update_one_value (widget_instance *instance,
                      Widget widget,
                      widget_value *val)
{
  return;
}

void
xlw_pop_instance (widget_instance* instance, Boolean up)
{
}

void
xlw_popup_menu (Widget widget, XEvent *event)
{
  XlwMenuWidget mw;

  if (!XtIsShell (widget))
    return;

  mw = (XlwMenuWidget)((CompositeWidget)widget)->composite.children [0];

  if (event)
    XtCallActionProc ((Widget) mw, "start", event, NULL, 0);
  else
    {
      XEvent dummy;
      XButtonPressedEvent *bd = &dummy.xbutton;

      bd->type = ButtonPress;
      bd->serial = 0;
      bd->send_event = 0;
      bd->display = XtDisplay (widget);
      bd->window = XtWindow (XtParent (widget));
      bd->time = CurrentTime;
      bd->button = 0;
      XQueryPointer (bd->display, bd->window, &bd->root,
		     &bd->subwindow, &bd->x_root, &bd->y_root,
		     &bd->x, &bd->y, &bd->state);

      XtCallActionProc ((Widget) mw, "start", &dummy, NULL, 0);
    }
}

/* Destruction of instances */
void
xlw_destroy_instance (widget_instance *instance)
{
  if (instance->widget)
    XtDestroyWidget (instance->widget);
}
