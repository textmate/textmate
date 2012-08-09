/* The lwlib interface to Motif widgets.

Copyright (C) 1994-1997, 1999-2012  Free Software Foundation, Inc.
Copyright (C) 1992 Lucid, Inc.

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

#include <unistd.h>
#include <stdio.h>
#include <setjmp.h>

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/ObjectP.h>
#include <X11/CoreP.h>
#include <X11/CompositeP.h>

#include <lisp.h>

#include "lwlib-Xm.h"
#include "lwlib-utils.h"

#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/DrawingA.h>
#include <Xm/FileSB.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/ArrowB.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>

enum do_call_type { pre_activate, selection, no_selection, post_activate };


/* Structures to keep destroyed instances */
typedef struct _destroyed_instance
{
  char*		name;
  char*		type;
  Widget 	widget;
  Widget	parent;
  Boolean	pop_up_p;
  struct _destroyed_instance*	next;
} destroyed_instance;

static destroyed_instance *make_destroyed_instance (char *, char *,
                                                    Widget, Widget,
                                                    Boolean);
static void free_destroyed_instance (destroyed_instance*);
Widget first_child (Widget);
Boolean lw_motif_widget_p (Widget);
static XmString resource_motif_string (Widget, char *);
static void destroy_all_children (Widget, int);
static void xm_update_label (widget_instance *, Widget, widget_value *);
static void xm_update_list (widget_instance *, Widget, widget_value *);
static void xm_update_pushbutton (widget_instance *, Widget,
                                  widget_value *);
static void xm_update_cascadebutton (widget_instance *, Widget,
                                     widget_value *);
static void xm_update_toggle (widget_instance *, Widget, widget_value *);
static void xm_update_radiobox (widget_instance *, Widget, widget_value *);
static void make_menu_in_widget (widget_instance *, Widget,
                                 widget_value *, int);
static void update_one_menu_entry (widget_instance *, Widget,
                                   widget_value *, Boolean);
static void xm_update_menu (widget_instance *, Widget, widget_value *,
                            Boolean);
static void xm_update_text (widget_instance *, Widget, widget_value *);
static void xm_update_text_field (widget_instance *, Widget,
                                  widget_value *);
void xm_update_one_value (widget_instance *, Widget, widget_value *);
static void activate_button (Widget, XtPointer, XtPointer);
static Widget make_dialog (char *, Widget, Boolean, char *, char *,
                           Boolean, Boolean, Boolean, int, int);
static destroyed_instance* find_matching_instance (widget_instance*);
static void mark_dead_instance_destroyed (Widget, XtPointer, XtPointer);
static void recenter_widget (Widget);
static Widget recycle_instance (destroyed_instance*);
Widget xm_create_dialog (widget_instance*);
static Widget make_menubar (widget_instance*);
static void remove_grabs (Widget, XtPointer, XtPointer);
static Widget make_popup_menu (widget_instance*);
static Widget make_main (widget_instance*);
void xm_destroy_instance (widget_instance*);
void xm_popup_menu (Widget, XEvent *);
static void set_min_dialog_size (Widget);
static void do_call (Widget, XtPointer, enum do_call_type);
static void xm_generic_callback (Widget, XtPointer, XtPointer);
static void xm_nosel_callback (Widget, XtPointer, XtPointer);
static void xm_pull_down_callback (Widget, XtPointer, XtPointer);
static void xm_pop_down_callback (Widget, XtPointer, XtPointer);
void xm_set_keyboard_focus (Widget, Widget);
void xm_set_main_areas (Widget, Widget, Widget);
static void xm_internal_update_other_instances (Widget, XtPointer,
                                                XtPointer);
static void xm_arm_callback (Widget, XtPointer, XtPointer);

#if 0
void xm_update_one_widget (widget_instance *, Widget, widget_value *,
                           Boolean);
void xm_pop_instance (widget_instance*, Boolean);
void xm_manage_resizing (Widget, Boolean);
#endif


#if 0

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


static destroyed_instance *all_destroyed_instances = NULL;

static destroyed_instance*
make_destroyed_instance (char* name,
                         char* type,
                         Widget widget,
                         Widget parent,
                         Boolean pop_up_p)
{
  destroyed_instance* instance =
    (destroyed_instance*) xmalloc (sizeof (destroyed_instance));
  instance->name = safe_strdup (name);
  instance->type = safe_strdup (type);
  instance->widget = widget;
  instance->parent = parent;
  instance->pop_up_p = pop_up_p;
  instance->next = NULL;
  return instance;
}

static void
free_destroyed_instance (destroyed_instance* instance)
{
  xfree (instance->name);
  xfree (instance->type);
  xfree (instance);
}

/* motif utility functions */
Widget
first_child (Widget widget)
{
  return ((CompositeWidget)widget)->composite.children [0];
}

Boolean
lw_motif_widget_p (Widget widget)
{
  return
    XtClass (widget) == xmDialogShellWidgetClass
      || XmIsPrimitive (widget) || XmIsManager (widget) || XmIsGadget (widget);
}

static XmString
resource_motif_string (Widget widget,
                       char* name)
{
  XtResource resource;
  XmString result = 0;

  resource.resource_name = name;
  resource.resource_class = XmCXmString;
  resource.resource_type = XmRXmString;
  resource.resource_size = sizeof (XmString);
  resource.resource_offset = 0;
  resource.default_type = XtRImmediate;
  resource.default_addr = 0;

  XtGetSubresources (widget, (XtPointer)&result, "dialogString",
		     "DialogString", &resource, 1, NULL, 0);
  return result;
}

/* Destroy all of the children of WIDGET
   starting with number FIRST_CHILD_TO_DESTROY.  */

static void
destroy_all_children (Widget widget,
                      int first_child_to_destroy)
{
  Widget* children;
  unsigned int number;
  int i;

  children = XtCompositeChildren (widget, &number);
  if (children)
    {
      XtUnmanageChildren (children + first_child_to_destroy,
			  number - first_child_to_destroy);

      /* Unmanage all children and destroy them.  They will only be
	 really destroyed when we get out of DispatchEvent.  */
      for (i = first_child_to_destroy; i < number; i++)
	{
	  Arg al[2];
	  Widget submenu = 0;
	  /* Cascade buttons have submenus,and these submenus
	     need to be freed.  But they are not included in
	     XtCompositeChildren.  So get it out of the cascade button
	     and free it.  If this child is not a cascade button,
	     then submenu should remain unchanged.  */
	  XtSetArg (al[0], XmNsubMenuId, &submenu);
  	  XtGetValues (children[i], al, 1);
	  if (submenu)
            {
              destroy_all_children (submenu, 0);
              XtDestroyWidget (submenu);
            }
	  XtDestroyWidget (children[i]);
	}

      XtFree ((char *) children);
    }
}



/* Callback XmNarmCallback and XmNdisarmCallback for buttons in a
   menu.  CLIENT_DATA contains a pointer to the widget_value
   corresponding to widget W.  CALL_DATA contains a
   XmPushButtonCallbackStruct containing the reason why the callback
   is called.  */

static void
xm_arm_callback (Widget w, XtPointer client_data, XtPointer call_data)
{
  XmPushButtonCallbackStruct *cbs = (XmPushButtonCallbackStruct *) call_data;
  widget_value *wv = (widget_value *) client_data;
  widget_instance *instance;

  /* Get the id of the menu bar or popup menu this widget is in.  */
  while (w != NULL)
    {
      if (XmIsRowColumn (w))
	{
	  unsigned char type = 0xff;

	  XtVaGetValues (w, XmNrowColumnType, &type, NULL);
	  if (type == XmMENU_BAR || type == XmMENU_POPUP)
	    break;
	}

      w = XtParent (w);
    }

  if (w != NULL)
    {
      instance = lw_get_widget_instance (w);
      if (instance && instance->info->highlight_cb)
	{
	  call_data = cbs->reason == XmCR_DISARM ? NULL : wv;
	  instance->info->highlight_cb (w, instance->info->id, call_data);
	}
    }
}



/* Update the label of widget WIDGET.  WIDGET must be a Label widget
   or a subclass of Label.  WIDGET_INSTANCE is unused.  VAL contains
   the value to update.

   Menus:

   Emacs fills VAL->name with the text to display in the menu, and
   sets VAL->value to null.  Function make_menu_in_widget creates
   widgets with VAL->name as resource name.  This works because the
   Label widget uses its resource name for display if no
   XmNlabelString is set.

   Dialogs:

   VAL->name is again set to the resource name, but VAL->value is
   not null, and contains the label string to display.  */

static void
xm_update_label (widget_instance* instance,
                 Widget widget,
                 widget_value* val)
{
  XmString res_string = 0;
  XmString built_string = 0;
  XmString key_string = 0;
  Arg al [256];
  int ac;

  ac = 0;

  if (val->value)
    {
      /* A label string is specified, i.e. we are in a dialog.  First
	 see if it is overridden by something from the resource file.  */
      res_string = resource_motif_string (widget, val->value);

      if (res_string)
	{
	  XtSetArg (al [ac], XmNlabelString, res_string); ac++;
	}
      else
	{
	  built_string =
	    XmStringCreateLocalized (val->value);
	  XtSetArg (al [ac], XmNlabelString, built_string); ac++;
	}

      XtSetArg (al [ac], XmNlabelType, XmSTRING); ac++;
    }

  if (val->key)
    {
      key_string = XmStringCreateLocalized (val->key);
      XtSetArg (al [ac], XmNacceleratorText, key_string); ac++;
    }

  if (ac)
    XtSetValues (widget, al, ac);

  if (built_string)
    XmStringFree (built_string);

  if (key_string)
    XmStringFree (key_string);
}

/* update of list */
static void
xm_update_list (widget_instance* instance,
                Widget widget,
                widget_value* val)
{
  widget_value* cur;
  int i;
  XtRemoveAllCallbacks (widget, XmNsingleSelectionCallback);
  XtAddCallback (widget, XmNsingleSelectionCallback, xm_generic_callback,
		 instance);
  for (cur = val->contents, i = 0; cur; cur = cur->next)
    if (cur->value)
      {
	XmString xmstr = XmStringCreateLocalized (cur->value);
	i += 1;
	XmListAddItem (widget, xmstr, 0);
	if (cur->selected)
	  XmListSelectPos (widget, i, False);
	XmStringFree (xmstr);
      }
}

/* update of buttons */
static void
xm_update_pushbutton (widget_instance* instance,
                      Widget widget,
                      widget_value* val)
{
  XtVaSetValues (widget, XmNalignment, XmALIGNMENT_CENTER, NULL);
  XtRemoveAllCallbacks (widget, XmNactivateCallback);
  XtAddCallback (widget, XmNactivateCallback, xm_generic_callback, instance);
}

static void
xm_update_cascadebutton (widget_instance* instance,
                         Widget widget,
                         widget_value* val)
{
  /* Should also rebuild the menu by calling ...update_menu... */
  XtRemoveAllCallbacks (widget, XmNcascadingCallback);
  XtAddCallback (widget, XmNcascadingCallback, xm_pull_down_callback,
		 instance);
}

/* update toggle and radiobox */
static void
xm_update_toggle (widget_instance* instance,
                  Widget widget,
                  widget_value* val)
{
  XtRemoveAllCallbacks (widget, XmNvalueChangedCallback);
  XtAddCallback (widget, XmNvalueChangedCallback,
		 xm_generic_callback, instance);
  XtVaSetValues (widget, XmNset, val->selected,
		 XmNalignment, XmALIGNMENT_BEGINNING, NULL);
}

static void
xm_update_radiobox (widget_instance* instance,
                    Widget widget,
                    widget_value* val)

{
  Widget toggle;
  widget_value* cur;

  /* update the callback */
  XtRemoveAllCallbacks (widget, XmNentryCallback);
  XtAddCallback (widget, XmNentryCallback, xm_generic_callback, instance);

  /* first update all the toggles */
  /* Energize kernel interface is currently bad.  It sets the selected widget
     with the selected flag but returns it by its name.  So we currently
     have to support both setting the selection with the selected slot
     of val contents and setting it with the "value" slot of val.  The latter
     has a higher priority.  This to be removed when the kernel is fixed. */
  for (cur = val->contents; cur; cur = cur->next)
    {
      toggle = XtNameToWidget (widget, cur->value);
      if (toggle)
	{
	  XtSetSensitive (toggle, cur->enabled);
	  if (!val->value && cur->selected)
	    XtVaSetValues (toggle, XmNset, cur->selected, NULL);
	  if (val->value && strcmp (val->value, cur->value))
	    XtVaSetValues (toggle, XmNset, False, NULL);
	}
    }

  /* The selected was specified by the value slot */
  if (val->value)
    {
      toggle = XtNameToWidget (widget, val->value);
      if (toggle)
	XtVaSetValues (toggle, XmNset, True, NULL);
    }
}


/* update a popup menu, pulldown menu or a menubar */

/* KEEP_FIRST_CHILDREN gives the number of initial children to keep.  */

static void
make_menu_in_widget (widget_instance* instance,
                     Widget widget,
                     widget_value* val,
                     int keep_first_children)
{
  Widget* children = 0;
  int num_children;
  int child_index;
  widget_value* cur;
  Widget button = 0;
  Widget title = 0;
  Widget menu;
  Arg al [256];
  int ac;
  Boolean menubar_p;
  unsigned char type;

  Widget* old_children;
  unsigned int old_num_children;

  /* Disable drag and drop for labels in menu bar.  */
  static char overrideTrans[] = "<Btn2Down>: Noop()";
  XtTranslations override = XtParseTranslationTable (overrideTrans);

  old_children = XtCompositeChildren (widget, &old_num_children);

  /* Allocate the children array */
  for (num_children = 0, cur = val; cur; num_children++, cur = cur->next)
    ;
  children = (Widget*)(void*)XtMalloc (num_children * sizeof (Widget));

  /* WIDGET should be a RowColumn.  */
  if (!XmIsRowColumn (widget))
    abort ();

  /* Determine whether WIDGET is a menu bar.  */
  type = -1;
  XtSetArg (al[0], XmNrowColumnType, &type);
  XtGetValues (widget, al, 1);
  if (type != XmMENU_BAR && type != XmMENU_PULLDOWN && type != XmMENU_POPUP)
    abort ();
  menubar_p = type == XmMENU_BAR;

  /* Add a callback to popups and pulldowns that is called when
     it is made invisible again.  */
  if (!menubar_p)
    XtAddCallback (XtParent (widget), XmNpopdownCallback,
		   xm_pop_down_callback, (XtPointer)instance);

  /* Preserve the first KEEP_FIRST_CHILDREN old children.  */
  for (child_index = 0, cur = val; child_index < keep_first_children;
       child_index++, cur = cur->next)
    children[child_index] = old_children[child_index];

  /* Check that those are all we have
     (the caller should have deleted the rest).  */
  if (old_num_children != keep_first_children)
    abort ();

  /* Create the rest.  */
  for (child_index = keep_first_children; cur; child_index++, cur = cur->next)
    {
      enum menu_separator separator;

      ac = 0;
      XtSetArg (al[ac], XmNsensitive, cur->enabled); ac++;
      XtSetArg (al[ac], XmNalignment, XmALIGNMENT_BEGINNING); ac++;
      XtSetArg (al[ac], XmNuserData, cur->call_data); ac++;

      if (instance->pop_up_p && !cur->contents && !cur->call_data
	  && !lw_separator_p (cur->name, &separator, 1))
	{
	  ac = 0;
	  XtSetArg (al[ac], XmNalignment, XmALIGNMENT_CENTER); ac++;
	  title = button = XmCreateLabel (widget, cur->name, al, ac);
	}
      else if (lw_separator_p (cur->name, &separator, 1))
	{
	  ac = 0;
	  XtSetArg (al[ac], XmNseparatorType, separator); ++ac;
	  button = XmCreateSeparator (widget, cur->name, al, ac);
	}
      else if (!cur->contents)
	{
	  if (menubar_p)
	    button = XmCreateCascadeButton (widget, cur->name, al, ac);
	  else if (!cur->call_data)
	    button = XmCreateLabel (widget, cur->name, al, ac);
	  else if (cur->button_type == BUTTON_TYPE_TOGGLE
		   || cur->button_type == BUTTON_TYPE_RADIO)
	    {
	      XtSetArg (al[ac], XmNset, cur->selected); ++ac;
	      XtSetArg (al[ac], XmNvisibleWhenOff, True); ++ac;
	      XtSetArg (al[ac], XmNindicatorType,
			(cur->button_type == BUTTON_TYPE_TOGGLE
			 ? XmN_OF_MANY : XmONE_OF_MANY));
	      ++ac;
	      button = XmCreateToggleButton (widget, cur->name, al, ac);
	      XtAddCallback (button, XmNarmCallback, xm_arm_callback, cur);
	      XtAddCallback (button, XmNdisarmCallback, xm_arm_callback, cur);
	    }
	  else
	    {
	      button = XmCreatePushButton (widget, cur->name, al, ac);
	      XtAddCallback (button, XmNarmCallback, xm_arm_callback, cur);
	      XtAddCallback (button, XmNdisarmCallback, xm_arm_callback, cur);
	    }

	  xm_update_label (instance, button, cur);

	  /* Add a callback that is called when the button is
	     selected.  Toggle buttons don't support
	     XmNactivateCallback, we use XmNvalueChangedCallback in
	     that case.  Don't add a callback to a simple label.  */
	  if (cur->button_type)
	    xm_update_toggle (instance, button, cur);
	  else if (cur->call_data)
	    XtAddCallback (button, XmNactivateCallback, xm_generic_callback,
			   (XtPointer)instance);
	}
      else
	{
	  menu = XmCreatePulldownMenu (widget, cur->name, NULL, 0);

	  make_menu_in_widget (instance, menu, cur->contents, 0);
          XtSetArg (al[ac], XmNsubMenuId, menu); ac++;
	  button = XmCreateCascadeButton (widget, cur->name, al, ac);

	  xm_update_label (instance, button, cur);

	  XtAddCallback (button, XmNcascadingCallback, xm_pull_down_callback,
			 (XtPointer)instance);
          XtOverrideTranslations (button, override);

	}

      children[child_index] = button;
    }

  /* Last entry is the help button.  The original comment read "Has to
     be done after managing the buttons otherwise the menubar is only
     4 pixels high."  This is no longer true, and to make
     XmNmenuHelpWidget work, we need to set it before managing the
     children.. --gerd.  */
  if (button)
    XtVaSetValues (widget, XmNmenuHelpWidget, button, NULL);

  if (num_children)
    XtManageChildren (children, num_children);

  XtFree ((char *) children);
  if (old_children)
    XtFree ((char *) old_children);
}

static void
update_one_menu_entry (widget_instance* instance,
                       Widget widget,
                       widget_value* val,
                       Boolean deep_p)
{
  Arg al [256];
  int ac;
  Widget menu;
  widget_value* contents;

  if (val->this_one_change == NO_CHANGE)
    return;

  /* update the sensitivity and userdata */
  /* Common to all widget types */
  XtSetSensitive (widget, val->enabled);
  XtVaSetValues (widget, XmNuserData, val->call_data, NULL);

  /* update the menu button as a label. */
  if (val->this_one_change >= VISIBLE_CHANGE)
    {
      xm_update_label (instance, widget, val);
      if (val->button_type)
	xm_update_toggle (instance, widget, val);
    }

  /* update the pulldown/pullaside as needed */
  ac = 0;
  menu = NULL;
  XtSetArg (al [ac], XmNsubMenuId, &menu); ac++;
  XtGetValues (widget, al, ac);

  contents = val->contents;

  if (!menu)
    {
      if (contents)
	{
	  unsigned int old_num_children, i;
	  Widget parent;
	  Widget *widget_list;

	  parent = XtParent (widget);
	  widget_list = XtCompositeChildren (parent, &old_num_children);

	  /* Find the widget position within the parent's widget list.  */
	  for (i = 0; i < old_num_children; i++)
	    if (strcmp (XtName (widget_list[i]), XtName (widget)) == 0)
	      break;
	  if (i == old_num_children)
	    abort ();
	  if (XmIsCascadeButton (widget_list[i]))
	    {
	      menu = XmCreatePulldownMenu (parent, XtName(widget), NULL, 0);
	      make_menu_in_widget (instance, menu, contents, 0);
	      ac = 0;
	      XtSetArg (al [ac], XmNsubMenuId, menu); ac++;
	      XtSetValues (widget, al, ac);
	    }
	  else
	    {
	      Widget button;

	      /* The current menuitem is a XmPushButtonGadget, it
		 needs to be replaced by a CascadeButtonGadget */
	      XtDestroyWidget (widget_list[i]);
	      menu = XmCreatePulldownMenu (parent, val->name, NULL, 0);
	      make_menu_in_widget (instance, menu, contents, 0);
	      ac = 0;
	      XtSetArg (al [ac], XmNsubMenuId, menu); ac++;
	      /* Non-zero values don't work reliably in
		 conjunction with Emacs' event loop */
	      XtSetArg (al [ac], XmNmappingDelay, 0); ac++;
#ifdef XmNpositionIndex /* This is undefined on SCO ODT 2.0.  */
	      /* Tell Motif to put it in the right place */
	      XtSetArg (al [ac], XmNpositionIndex , i); ac++;
#endif
	      button = XmCreateCascadeButton (parent, val->name, al, ac);
	      xm_update_label (instance, button, val);

	      XtAddCallback (button, XmNcascadingCallback, xm_pull_down_callback,
			     (XtPointer)instance);
	      XtManageChild (button);
	    }

          if (widget_list)
            XtFree ((char*) widget_list);
	}
    }
  else if (!contents)
    {
      ac = 0;
      XtSetArg (al [ac], XmNsubMenuId, NULL); ac++;
      XtSetValues (widget, al, ac);
      XtDestroyWidget (menu);
    }
  else if (deep_p && contents->change != NO_CHANGE)
    xm_update_menu (instance, menu, val, 1);
}

static void
xm_update_menu (widget_instance* instance,
                Widget widget,
                widget_value* val,
                Boolean deep_p)
{
  Widget* children;
  unsigned int num_children;
  int num_children_to_keep = 0;
  int i;
  widget_value* cur;

  children = XtCompositeChildren (widget, &num_children);

  /* Widget is a RowColumn widget whose contents have to be updated
   * to reflect the list of items in val->contents */

  /* See how many buttons we can keep, and how many we
     must completely replace.  */
  if (val->contents == 0)
    num_children_to_keep = 0;
  else if (val->contents->change == STRUCTURAL_CHANGE)
    {
      if (children)
	{
	  for (i = 0, cur = val->contents;
               (i < num_children
		&& cur); /* how else to ditch unwanted children ?? - mgd */
	       i++, cur = cur->next)
	    {
	      if (cur->this_one_change == STRUCTURAL_CHANGE)
		break;
	    }

	  num_children_to_keep = i;
	}
    }
  else
    num_children_to_keep = num_children;

  /* Update all the buttons of the RowColumn, in order,
     except for those we are going to replace entirely.  */
  if (children)
    {
      for (i = 0, cur = val->contents; i < num_children_to_keep; i++)
	{
	  if (!cur)
	    {
	      num_children_to_keep = i;
	      break;
	    }
	  if (children [i]->core.being_destroyed
	      || strcmp (XtName (children [i]), cur->name))
	    continue;
	  update_one_menu_entry (instance, children [i], cur, deep_p);
	  cur = cur->next;
	}
    }

  /* Now replace from scratch all the buttons after the last
     place that the top-level structure changed.  */
  if (val->contents && val->contents->change == STRUCTURAL_CHANGE)
    {
      destroy_all_children (widget, num_children_to_keep);
      make_menu_in_widget (instance, widget, val->contents,
                           num_children_to_keep);
    }

  XtFree ((char *) children);
}


/* update text widgets */

static void
xm_update_text (widget_instance* instance,
                Widget widget,
                widget_value* val)
{
  XmTextSetString (widget, val->value ? val->value : "");
  XtRemoveAllCallbacks (widget, XmNactivateCallback);
  XtAddCallback (widget, XmNactivateCallback, xm_generic_callback, instance);
  XtRemoveAllCallbacks (widget, XmNvalueChangedCallback);
  XtAddCallback (widget, XmNvalueChangedCallback,
		 xm_internal_update_other_instances, instance);
}

static void
xm_update_text_field (widget_instance* instance,
                      Widget widget,
                      widget_value* val)
{
  XmTextFieldSetString (widget, val->value ? val->value : "");
  XtRemoveAllCallbacks (widget, XmNactivateCallback);
  XtAddCallback (widget, XmNactivateCallback, xm_generic_callback, instance);
  XtRemoveAllCallbacks (widget, XmNvalueChangedCallback);
  XtAddCallback (widget, XmNvalueChangedCallback,
		 xm_internal_update_other_instances, instance);
}


/* update a motif widget */

void
xm_update_one_widget (widget_instance* instance,
                      Widget widget,
                      widget_value* val,
                      Boolean deep_p)
{
  WidgetClass class;

  /* Mark as not edited */
  val->edited = False;

  /* Common to all widget types */
  XtSetSensitive (widget, val->enabled);
  XtVaSetValues (widget, XmNuserData, val->call_data, NULL);

  /* Common to all label like widgets */
  if (XtIsSubclass (widget, xmLabelWidgetClass))
    xm_update_label (instance, widget, val);

  class = XtClass (widget);
  /* Class specific things */
  if (class == xmPushButtonWidgetClass ||
      class == xmArrowButtonWidgetClass)
    {
      xm_update_pushbutton (instance, widget, val);
    }
  else if (class == xmCascadeButtonWidgetClass)
    {
      xm_update_cascadebutton (instance, widget, val);
    }
  else if (class == xmToggleButtonWidgetClass
	   || class == xmToggleButtonGadgetClass)
    {
      xm_update_toggle (instance, widget, val);
    }
  else if (class == xmRowColumnWidgetClass)
    {
      Boolean radiobox = 0;
      int ac = 0;
      Arg al [1];

      XtSetArg (al [ac], XmNradioBehavior, &radiobox); ac++;
      XtGetValues (widget, al, ac);

      if (radiobox)
	xm_update_radiobox (instance, widget, val);
      else
	xm_update_menu (instance, widget, val, deep_p);
    }
  else if (class == xmTextWidgetClass)
    {
      xm_update_text (instance, widget, val);
    }
  else if (class == xmTextFieldWidgetClass)
    {
      xm_update_text_field (instance, widget, val);
    }
  else if (class == xmListWidgetClass)
    {
      xm_update_list (instance, widget, val);
    }
}

/* getting the value back */
void
xm_update_one_value (widget_instance* instance,
                     Widget widget,
                     widget_value* val)
{
  WidgetClass class = XtClass (widget);
  widget_value *old_wv;

  /* copy the call_data slot into the "return" widget_value */
  for (old_wv = instance->info->val->contents; old_wv; old_wv = old_wv->next)
    if (!strcmp (val->name, old_wv->name))
      {
	val->call_data = old_wv->call_data;
	break;
      }

  if (class == xmToggleButtonWidgetClass || class == xmToggleButtonGadgetClass)
    {
      XtVaGetValues (widget, XmNset, &val->selected, NULL);
      val->edited = True;
    }
  else if (class == xmTextWidgetClass)
    {
      xfree (val->value);
      val->value = XmTextGetString (widget);
      val->edited = True;
    }
  else if (class == xmTextFieldWidgetClass)
    {
      xfree (val->value);
      val->value = XmTextFieldGetString (widget);
      val->edited = True;
    }
  else if (class == xmRowColumnWidgetClass)
    {
      Boolean radiobox = 0;
      int ac = 0;
      Arg al [1];

      XtSetArg (al [ac], XmNradioBehavior, &radiobox); ac++;
      XtGetValues (widget, al, ac);

      if (radiobox)
	{
	  CompositeWidget radio = (CompositeWidget)widget;
	  int i;
	  for (i = 0; i < radio->composite.num_children; i++)
	    {
	      int set = False;
	      Widget toggle = radio->composite.children [i];

	      XtVaGetValues (toggle, XmNset, &set, NULL);
	      if (set)
		{
		  xfree (val->value);
		  val->value = safe_strdup (XtName (toggle));
		}
	    }
	  val->edited = True;
	}
    }
  else if (class == xmListWidgetClass)
    {
      int pos_cnt;
      int* pos_list;
      if (XmListGetSelectedPos (widget, &pos_list, &pos_cnt))
	{
	  int i;
	  widget_value* cur;
	  for (cur = val->contents, i = 0; cur; cur = cur->next)
	    if (cur->value)
	      {
		int j;
		cur->selected = False;
		i += 1;
		for (j = 0; j < pos_cnt; j++)
		  if (pos_list [j] == i)
		    {
		      cur->selected = True;
		      val->value = safe_strdup (cur->name);
		    }
	      }
	  val->edited = 1;
	  XtFree ((char *) pos_list);
	}
    }
}


/* This function is for activating a button from a program.  It's wrong because
   we pass a NULL argument in the call_data which is not Motif compatible.
   This is used from the XmNdefaultAction callback of the List widgets to
   have a double-click put down a dialog box like the button would do.
   I could not find a way to do that with accelerators.
 */
static void
activate_button (Widget widget,
                 XtPointer closure,
                 XtPointer call_data)
{
  Widget button = (Widget)closure;
  XtCallCallbacks (button, XmNactivateCallback, NULL);
}

/* creation functions */

/* Called for key press in dialogs.  Used to pop down dialog on ESC.  */
static void
dialog_key_cb (Widget widget,
               XtPointer closure,
               XEvent *event,
               Boolean *continue_to_dispatch)
{
  KeySym sym = 0;
  Modifiers modif_ret;

  XtTranslateKeycode (event->xkey.display, event->xkey.keycode, 0,
                      &modif_ret, &sym);

  if (sym == osfXK_Cancel)
    {
      Widget w = *((Widget *) closure);

      while (w && ! XtIsShell (w))
        w = XtParent (w);

      if (XtIsShell (w)) XtPopdown (w);
    }

  *continue_to_dispatch = TRUE;
}

/* dialogs */
static Widget
make_dialog (char* name,
             Widget parent,
             Boolean pop_up_p,
             char* shell_title,
             char* icon_name,
             Boolean text_input_slot,
             Boolean radio_box,
             Boolean list,
             int left_buttons,
             int right_buttons)
{
  Widget result;
  Widget form;
  Widget row;
  Widget icon;
  Widget icon_separator;
  Widget message_label;
  Widget value = 0;
  Widget separator;
  Widget button = 0;
  Widget children [16];		/* for the final XtManageChildren */
  int	 n_children;
  Arg 	al[64];			/* Arg List */
  int 	ac;			/* Arg Count */
  int 	i;

  if (pop_up_p)
    {
      ac = 0;
      XtSetArg(al[ac], XmNtitle, shell_title); ac++;
      XtSetArg(al[ac], XtNallowShellResize, True); ac++;
      XtSetArg(al[ac], XmNdeleteResponse, XmUNMAP); ac++;
      result = XmCreateDialogShell (parent, "dialog", al, ac);
      ac = 0;
      XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
/*      XtSetArg(al[ac], XmNautoUnmanage, TRUE); ac++; */ /* ####is this ok? */
      XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP); ac++;
      form = XmCreateForm (result, shell_title, al, ac);
    }
  else
    {
      ac = 0;
      XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
      XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP); ac++;
      form = XmCreateForm (parent, shell_title, al, ac);
      result = form;
    }

  n_children = left_buttons + right_buttons + 1;
  ac = 0;
  XtSetArg(al[ac], XmNpacking, n_children == 3?
	   XmPACK_COLUMN: XmPACK_TIGHT); ac++;
  XtSetArg(al[ac], XmNorientation, n_children == 3?
	   XmVERTICAL: XmHORIZONTAL); ac++;
  XtSetArg(al[ac], XmNnumColumns, left_buttons + right_buttons + 1); ac++;
  XtSetArg(al[ac], XmNmarginWidth, 0); ac++;
  XtSetArg(al[ac], XmNmarginHeight, 0); ac++;
  XtSetArg(al[ac], XmNspacing, 13); ac++;
  XtSetArg(al[ac], XmNadjustLast, False); ac++;
  XtSetArg(al[ac], XmNalignment, XmALIGNMENT_CENTER); ac++;
  XtSetArg(al[ac], XmNisAligned, True); ac++;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNleftOffset, 13); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNrightOffset, 13); ac++;
  row = XmCreateRowColumn (form, "row", al, ac);

  n_children = 0;
  for (i = 0; i < left_buttons; i++)
    {
      char button_name [16];
      sprintf (button_name, "button%d", i + 1);
      ac = 0;
      if (i == 0)
	{
	  XtSetArg(al[ac], XmNhighlightThickness, 1); ac++;
	  XtSetArg(al[ac], XmNshowAsDefault, TRUE); ac++;
	}
      XtSetArg(al[ac], XmNmarginWidth, 10); ac++;
      XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP); ac++;
      children [n_children] = XmCreatePushButton (row, button_name, al, ac);
      XtAddEventHandler (children [n_children],
                         KeyPressMask, False, dialog_key_cb, result);

      if (i == 0)
	{
	  button = children [n_children];
	  ac = 0;
	  XtSetArg(al[ac], XmNdefaultButton, button); ac++;
	  XtSetValues (row, al, ac);
	}

      n_children++;
    }

  /* invisible separator button */
  ac = 0;
  XtSetArg (al[ac], XmNmappedWhenManaged, FALSE); ac++;
  children [n_children] = XmCreateLabel (row, "separator_button", al, ac);
  n_children++;

  for (i = 0; i < right_buttons; i++)
    {
      char button_name [16];
      sprintf (button_name, "button%d", left_buttons + i + 1);
      ac = 0;
      XtSetArg(al[ac], XmNmarginWidth, 10); ac++;
      XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP); ac++;
      children [n_children] = XmCreatePushButton (row, button_name, al, ac);
      XtAddEventHandler (children [n_children],
                         KeyPressMask, False, dialog_key_cb, result);

      if (! button) button = children [n_children];
      n_children++;
    }

  XtManageChildren (children, n_children);

  ac = 0;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
  XtSetArg(al[ac], XmNbottomWidget, row); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNleftOffset, 0); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNrightOffset, 0); ac++;
  separator = XmCreateSeparator (form, "", al, ac);

  ac = 0;
  XtSetArg(al[ac], XmNlabelType, XmPIXMAP); ac++;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNtopOffset, 13); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNleftOffset, 13); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
  icon = XmCreateLabel (form, icon_name, al, ac);

  ac = 0;
  XtSetArg(al[ac], XmNmappedWhenManaged, FALSE); ac++;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNtopOffset, 6); ac++;
  XtSetArg(al[ac], XmNtopWidget, icon); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 6); ac++;
  XtSetArg(al[ac], XmNbottomWidget, separator); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_NONE); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
  icon_separator = XmCreateLabel (form, "", al, ac);

  if (text_input_slot)
    {
      ac = 0;
      XtSetArg(al[ac], XmNcolumns, 50); ac++;
      XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
      XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
      XtSetArg(al[ac], XmNbottomWidget, separator); ac++;
      XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNleftOffset, 13); ac++;
      XtSetArg(al[ac], XmNleftWidget, icon); ac++;
      XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNrightOffset, 13); ac++;
      value = XmCreateTextField (form, "value", al, ac);
    }
  else if (radio_box)
    {
      Widget radio_butt;
      ac = 0;
      XtSetArg(al[ac], XmNmarginWidth, 0); ac++;
      XtSetArg(al[ac], XmNmarginHeight, 0); ac++;
      XtSetArg(al[ac], XmNspacing, 13); ac++;
      XtSetArg(al[ac], XmNalignment, XmALIGNMENT_CENTER); ac++;
      XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
      XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
      XtSetArg(al[ac], XmNbottomWidget, separator); ac++;
      XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNleftOffset, 13); ac++;
      XtSetArg(al[ac], XmNleftWidget, icon); ac++;
      XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNrightOffset, 13); ac++;
      value = XmCreateRadioBox (form, "radiobutton1", al, ac);
      ac = 0;
      i = 0;
      radio_butt = XmCreateToggleButtonGadget (value, "radio1", al, ac);
      children [i++] = radio_butt;
      radio_butt = XmCreateToggleButtonGadget (value, "radio2", al, ac);
      children [i++] = radio_butt;
      radio_butt = XmCreateToggleButtonGadget (value, "radio3", al, ac);
      children [i++] = radio_butt;
      XtManageChildren (children, i);
    }
  else if (list)
    {
      ac = 0;
      XtSetArg(al[ac], XmNvisibleItemCount, 5); ac++;
      XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
      XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
      XtSetArg(al[ac], XmNbottomWidget, separator); ac++;
      XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNleftOffset, 13); ac++;
      XtSetArg(al[ac], XmNleftWidget, icon); ac++;
      XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNrightOffset, 13); ac++;
      value = XmCreateScrolledList (form, "list", al, ac);

      /* this is the easiest way I found to have the dble click in the
	 list activate the default button */
      XtAddCallback (value, XmNdefaultActionCallback, activate_button, button);
    }

  ac = 0;
  XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING); ac++;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNtopOffset, 13); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
  XtSetArg(al[ac], XmNbottomWidget,
	   text_input_slot || radio_box || list ? value : separator); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNleftOffset, 13); ac++;
  XtSetArg(al[ac], XmNleftWidget, icon); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNrightOffset, 13); ac++;
  message_label = XmCreateLabel (form, "message", al, ac);

  if (list)
    XtManageChild (value);

  i = 0;
  children [i] = row; i++;
  children [i] = separator; i++;
  if (text_input_slot || radio_box)
    {
      children [i] = value; i++;
    }
  children [i] = message_label; i++;
  children [i] = icon; i++;
  children [i] = icon_separator; i++;
  XtManageChildren (children, i);

  if (text_input_slot || list)
    {
      XtInstallAccelerators (value, button);
      XtSetKeyboardFocus (result, value);
    }
  else
    {
      XtInstallAccelerators (form, button);
      XtSetKeyboardFocus (result, button);
    }

  return result;
}

static destroyed_instance*
find_matching_instance (widget_instance* instance)
{
  destroyed_instance*	cur;
  destroyed_instance*	prev;
  char*	type = instance->info->type;
  char*	name = instance->info->name;

  for (prev = NULL, cur = all_destroyed_instances;
       cur;
       prev = cur, cur = cur->next)
    {
      if (!strcmp (cur->name, name)
	  && !strcmp (cur->type, type)
	  && cur->parent == instance->parent
	  && cur->pop_up_p == instance->pop_up_p)
	{
	  if (prev)
	    prev->next = cur->next;
	  else
	    all_destroyed_instances = cur->next;
	  return cur;
	}
      /* do some cleanup */
      else if (!cur->widget)
	{
	  if (prev)
	    prev->next = cur->next;
	  else
	    all_destroyed_instances = cur->next;
	  free_destroyed_instance (cur);
	  cur = prev ? prev : all_destroyed_instances;
	}
    }
  return NULL;
}

static void
mark_dead_instance_destroyed (Widget widget,
                              XtPointer closure,
                              XtPointer call_data)
{
  destroyed_instance* instance = (destroyed_instance*)closure;
  instance->widget = NULL;
}

static void
recenter_widget (Widget widget)
{
  Widget parent = XtParent (widget);
  Screen* screen = XtScreen (widget);
  Dimension screen_width = WidthOfScreen (screen);
  Dimension screen_height = HeightOfScreen (screen);
  Dimension parent_width = 0;
  Dimension parent_height = 0;
  Dimension child_width = 0;
  Dimension child_height = 0;
  Position x;
  Position y;

  XtVaGetValues (widget, XtNwidth, &child_width, XtNheight, &child_height, NULL);
  XtVaGetValues (parent, XtNwidth, &parent_width, XtNheight, &parent_height,
		 NULL);

  x = (((Position)parent_width) - ((Position)child_width)) / 2;
  y = (((Position)parent_height) - ((Position)child_height)) / 2;

  XtTranslateCoords (parent, x, y, &x, &y);

  if (x + child_width > screen_width)
    x = screen_width - child_width;
  if (x < 0)
    x = 0;

  if (y + child_height > screen_height)
    y = screen_height - child_height;
  if (y < 0)
    y = 0;

  XtVaSetValues (widget, XtNx, x, XtNy, y, NULL);
}

static Widget
recycle_instance (destroyed_instance* instance)
{
  Widget widget = instance->widget;

  /* widget is NULL if the parent was destroyed. */
  if (widget)
    {
      Widget focus;
      Widget separator;

      /* Remove the destroy callback as the instance is not in the list
	 anymore */
      XtRemoveCallback (instance->parent, XtNdestroyCallback,
			mark_dead_instance_destroyed,
			(XtPointer)instance);

      /* Give the focus to the initial item */
      focus = XtNameToWidget (widget, "*value");
      if (!focus)
	focus = XtNameToWidget (widget, "*button1");
      if (focus)
	XtSetKeyboardFocus (widget, focus);

      /* shrink the separator label back to their original size */
      separator = XtNameToWidget (widget, "*separator_button");
      if (separator)
	XtVaSetValues (separator, XtNwidth, 5, XtNheight, 5, NULL);

      /* Center the dialog in its parent */
      recenter_widget (widget);
    }
  free_destroyed_instance (instance);
  return widget;
}

Widget
xm_create_dialog (widget_instance* instance)
{
  char* 	name = instance->info->type;
  Widget 	parent = instance->parent;
  Widget	widget;
  Boolean 	pop_up_p = instance->pop_up_p;
  char*		shell_name = 0;
  char* 	icon_name = 0;
  Boolean	text_input_slot = False;
  Boolean	radio_box = False;
  Boolean	list = False;
  int		total_buttons;
  int		left_buttons = 0;
  int		right_buttons = 1;
  destroyed_instance*	dead_one;

  /* try to find a widget to recycle */
  dead_one = find_matching_instance (instance);
  if (dead_one)
    {
      Widget recycled_widget = recycle_instance (dead_one);
      if (recycled_widget)
	return recycled_widget;
    }

  switch (name [0]){
  case 'E': case 'e':
    icon_name = "dbox-error";
    shell_name = "Error";
    break;

  case 'I': case 'i':
    icon_name = "dbox-info";
    shell_name = "Information";
    break;

  case 'L': case 'l':
    list = True;
    icon_name = "dbox-question";
    shell_name = "Prompt";
    break;

  case 'P': case 'p':
    text_input_slot = True;
    icon_name = "dbox-question";
    shell_name = "Prompt";
    break;

  case 'Q': case 'q':
    icon_name = "dbox-question";
    shell_name = "Question";
    break;
  }

  total_buttons = name [1] - '0';

  if (name [3] == 'T' || name [3] == 't')
    {
      text_input_slot = False;
      radio_box = True;
    }
  else if (name [3])
    right_buttons = name [4] - '0';

  left_buttons = total_buttons - right_buttons;

  widget = make_dialog (name, parent, pop_up_p,
			shell_name, icon_name, text_input_slot, radio_box,
			list, left_buttons, right_buttons);

  XtAddCallback (widget, XmNpopdownCallback, xm_nosel_callback,
		 (XtPointer) instance);

  return widget;
}

/* Create a menu bar.  We turn off the f10 key
   because we have not yet managed to make it work right in Motif.  */

static Widget
make_menubar (widget_instance* instance)
{
  Arg al[3];
  int ac;

  ac = 0;
  XtSetArg(al[ac], XmNmenuAccelerator, 0); ++ac;
  return XmCreateMenuBar (instance->parent, instance->info->name, al, ac);
}

static void
remove_grabs (Widget shell,
              XtPointer closure,
              XtPointer call_data)
{
  Widget menu = (Widget) closure;
  XmRemoveFromPostFromList (menu, XtParent (XtParent (menu)));
}

static Widget
make_popup_menu (widget_instance* instance)
{
  Widget parent = instance->parent;
  Window parent_window = parent->core.window;
  Widget result;

  /* sets the parent window to 0 to fool Motif into not generating a grab */
  parent->core.window = 0;
  result = XmCreatePopupMenu (parent, instance->info->name, NULL, 0);
  XtAddCallback (XtParent (result), XmNpopdownCallback, remove_grabs,
		 (XtPointer)result);
  parent->core.window = parent_window;
  return result;
}

static Widget
make_main (widget_instance* instance)
{
  Widget parent = instance->parent;
  Widget result;
  Arg al[2];
  int ac;

  ac = 0;
  XtSetArg (al[ac], XtNborderWidth, 0); ac++;
  XtSetArg (al[ac], XmNspacing, 0); ac++;
  result = XmCreateMainWindow (parent, instance->info->name, al, ac);
  return result;
}

/* Table of functions to create widgets */

#ifdef ENERGIZE

/* interface with the XDesigner generated functions */
typedef Widget (*widget_maker) (Widget);
extern Widget create_project_p_sheet (Widget parent);
extern Widget create_debugger_p_sheet (Widget parent);
extern Widget create_breaklist_p_sheet (Widget parent);
extern Widget create_le_browser_p_sheet (Widget parent);
extern Widget create_class_browser_p_sheet (Widget parent);
extern Widget create_call_browser_p_sheet (Widget parent);
extern Widget create_build_dialog (Widget parent);
extern Widget create_editmode_dialog (Widget parent);
extern Widget create_search_dialog (Widget parent);
extern Widget create_project_display_dialog (Widget parent);

static Widget
make_one (widget_instance* instance, widget_maker fn)
{
  Widget result;
  Arg 	al [64];
  int 	ac = 0;

  if (instance->pop_up_p)
    {
      XtSetArg (al [ac], XmNallowShellResize, TRUE); ac++;
      result = XmCreateDialogShell (instance->parent, "dialog", NULL, 0);
      XtAddCallback (result, XmNpopdownCallback, &xm_nosel_callback,
		     (XtPointer) instance);
      (*fn) (result);
    }
  else
    {
      result = (*fn) (instance->parent);
      XtRealizeWidget (result);
    }
  return result;
}

static Widget
make_project_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_project_p_sheet);
}

static Widget
make_debugger_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_debugger_p_sheet);
}

static Widget
make_breaklist_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_breaklist_p_sheet);
}

static Widget
make_le_browser_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_le_browser_p_sheet);
}

static Widget
make_class_browser_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_class_browser_p_sheet);
}

static Widget
make_call_browser_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_call_browser_p_sheet);
}

static Widget
make_build_dialog (widget_instance* instance)
{
  return make_one (instance, create_build_dialog);
}

static Widget
make_editmode_dialog (widget_instance* instance)
{
  return make_one (instance, create_editmode_dialog);
}

static Widget
make_search_dialog (widget_instance* instance)
{
  return make_one (instance, create_search_dialog);
}

static Widget
make_project_display_dialog (widget_instance* instance)
{
  return make_one (instance, create_project_display_dialog);
}

#endif /* ENERGIZE */

widget_creation_entry
xm_creation_table [] =
{
  {"menubar", 			make_menubar},
  {"popup",			make_popup_menu},
  {"main",			make_main},
#ifdef ENERGIZE
  {"project_p_sheet",		make_project_p_sheet},
  {"debugger_p_sheet",		make_debugger_p_sheet},
  {"breaklist_psheet",		make_breaklist_p_sheet},
  {"leb_psheet",       		make_le_browser_p_sheet},
  {"class_browser_psheet",	make_class_browser_p_sheet},
  {"ctree_browser_psheet",	make_call_browser_p_sheet},
  {"build",			make_build_dialog},
  {"editmode",			make_editmode_dialog},
  {"search",			make_search_dialog},
  {"project_display",		make_project_display_dialog},
#endif /* ENERGIZE */
  {NULL, NULL}
};

/* Destruction of instances */
void
xm_destroy_instance ( widget_instance* instance)
{
  Widget widget = instance->widget;
  /* recycle the dialog boxes */
  /* Disable the recycling until we can find a way to have the dialog box
     get reasonable layout after we modify its contents. */
  if (0
      && XtClass (widget) == xmDialogShellWidgetClass)
    {
      destroyed_instance* dead_instance =
	make_destroyed_instance (instance->info->name,
				 instance->info->type,
				 instance->widget,
				 instance->parent,
				 instance->pop_up_p);
      dead_instance->next = all_destroyed_instances;
      all_destroyed_instances = dead_instance;
      XtUnmanageChild (first_child (instance->widget));
      XFlush (XtDisplay (instance->widget));
      XtAddCallback (instance->parent, XtNdestroyCallback,
		     mark_dead_instance_destroyed, (XtPointer)dead_instance);
    }
  else
    {
      /* This might not be necessary now that the nosel is attached to
	 popdown instead of destroy, but it can't hurt. */
      XtRemoveCallback (instance->widget, XtNdestroyCallback,
			xm_nosel_callback, (XtPointer)instance);
      XtDestroyWidget (instance->widget);
    }
}

/* popup utility */
void
xm_popup_menu (Widget widget, XEvent *event)
{
  XButtonPressedEvent dummy;

  if (event == 0)
    {
      dummy.type = ButtonPress;
      dummy.serial = 0;
      dummy.send_event = 0;
      dummy.display = XtDisplay (widget);
      dummy.window = XtWindow (XtParent (widget));
      dummy.time = 0;
      dummy.button = 0;
      XQueryPointer (dummy.display, dummy.window, &dummy.root,
		     &dummy.subwindow, &dummy.x_root, &dummy.y_root,
		     &dummy.x, &dummy.y, &dummy.state);
      event = (XEvent *) &dummy;
    }

  if (event->type == ButtonPress || event->type == ButtonRelease)
    {
      /* Setting the menuPost resource only required by Motif 1.1 and
	 LessTif 0.84 and earlier.  With later versions of LessTif,
	 setting menuPost is unnecessary and may cause problems, so
	 don't do it.  */
#if XmVersion < 1002 || (defined LESSTIF_VERSION && LESSTIF_VERSION < 84)
	{
	  /* This is so totally ridiculous: there's NO WAY to tell Motif
	     that *any* button can select a menu item.  Only one button
	     can have that honor.  */

	  char *trans = 0;
	  if      (event->xbutton.state & Button5Mask) trans = "<Btn5Down>";
	  else if (event->xbutton.state & Button4Mask) trans = "<Btn4Down>";
	  else if (event->xbutton.state & Button3Mask) trans = "<Btn3Down>";
	  else if (event->xbutton.state & Button2Mask) trans = "<Btn2Down>";
	  else if (event->xbutton.state & Button1Mask) trans = "<Btn1Down>";
	  if (trans) XtVaSetValues (widget, XmNmenuPost, trans, NULL);
	}
#endif

      XmMenuPosition (widget, (XButtonPressedEvent *) event);
    }

  XtManageChild (widget);
}

static void
set_min_dialog_size (Widget w)
{
  short width;
  short height;
  XtVaGetValues (w, XmNwidth, &width, XmNheight, &height, NULL);
  XtVaSetValues (w, XmNminWidth, width, XmNminHeight, height, NULL);
}

void
xm_pop_instance (widget_instance* instance, Boolean up)
{
  Widget widget = instance->widget;

  if (XtClass (widget) == xmDialogShellWidgetClass)
    {
      Widget widget_to_manage = first_child (widget);
      if (up)
	{
	  XtManageChild (widget_to_manage);
	  set_min_dialog_size (widget);
	  XtSetKeyboardFocus (instance->parent, widget);
	}
      else
	XtUnmanageChild (widget_to_manage);
    }
  else
    {
      if (up)
	XtManageChild (widget);
      else
	XtUnmanageChild (widget);
    }
}


/* motif callback */

static void
do_call (Widget widget,
         XtPointer closure,
         enum do_call_type type)
{
  Arg al [256];
  int ac;
  XtPointer user_data;
  widget_instance* instance = (widget_instance*)closure;
  Widget instance_widget;
  LWLIB_ID id;

  if (!instance)
    return;
  if (widget->core.being_destroyed)
    return;

  instance_widget = instance->widget;
  if (!instance_widget)
    return;

  id = instance->info->id;
  ac = 0;
  user_data = NULL;
  XtSetArg (al [ac], XmNuserData, &user_data); ac++;
  XtGetValues (widget, al, ac);

  switch (type)
    {
    case pre_activate:
      if (instance->info->pre_activate_cb)
	instance->info->pre_activate_cb (widget, id, user_data);
      break;

    case selection:
      if (instance->info->selection_cb)
	instance->info->selection_cb (widget, id, user_data);
      break;

    case no_selection:
      if (instance->info->selection_cb)
	instance->info->selection_cb (widget, id, (XtPointer) -1);
      break;

    case post_activate:
      if (instance->info->post_activate_cb)
	instance->info->post_activate_cb (widget, id, user_data);
      break;

    default:
      abort ();
    }
}

/* Like lw_internal_update_other_instances except that it does not do
   anything if its shell parent is not managed.  This is to protect
   lw_internal_update_other_instances to dereference freed memory
   if the widget was ``destroyed'' by caching it in the all_destroyed_instances
   list */
static void
xm_internal_update_other_instances (Widget widget,
                                    XtPointer closure,
                                    XtPointer call_data)
{
  Widget parent;
  for (parent = widget; parent; parent = XtParent (parent))
    if (XtIsShell (parent))
      break;
    else if (!XtIsManaged (parent))
      return;
   lw_internal_update_other_instances (widget, closure, call_data);
}

static void
xm_generic_callback (Widget widget,
                     XtPointer closure,
                     XtPointer call_data)
{
  lw_internal_update_other_instances (widget, closure, call_data);
  do_call (widget, closure, selection);
}

static void
xm_nosel_callback (Widget widget,
                   XtPointer closure,
                   XtPointer call_data)
{
  /* This callback is only called when a dialog box is dismissed with
     the wm's destroy button (WM_DELETE_WINDOW.)  We want the dialog
     box to be destroyed in that case, not just unmapped, so that it
     releases its keyboard grabs.  But there are problems with running
     our callbacks while the widget is in the process of being
     destroyed, so we set XmNdeleteResponse to XmUNMAP instead of
     XmDESTROY and then destroy it ourself after having run the
     callback.  */
  do_call (widget, closure, no_selection);
  XtDestroyWidget (widget);
}

static void
xm_pull_down_callback (Widget widget,
                       XtPointer closure,
                       XtPointer call_data)
{
  Widget parent = XtParent (widget);

  if (XmIsRowColumn (parent))
    {
      unsigned char type = 0xff;
      XtVaGetValues (parent, XmNrowColumnType, &type, NULL);
      if (type == XmMENU_BAR)
	do_call (widget, closure, pre_activate);
    }
}


/* XmNpopdownCallback for MenuShell widgets.  WIDGET is the MenuShell,
   CLOSURE is a pointer to the widget_instance of the shell,

   Note that this callback is called for each cascade button in a
   menu, whether or not its submenu is visible.  */

static void
xm_pop_down_callback (Widget widget,
                      XtPointer closure,
                      XtPointer call_data)
{
  widget_instance *instance = (widget_instance *) closure;

  if ((!instance->pop_up_p && XtParent (widget) == instance->widget)
      || XtParent (widget) == instance->parent)
    do_call (widget, closure, post_activate);
}


/* set the keyboard focus */
void
xm_set_keyboard_focus (Widget parent, Widget w)
{
  XmProcessTraversal (w, 0);
  XtSetKeyboardFocus (parent, w);
}

/* Motif hack to set the main window areas. */
void
xm_set_main_areas (Widget parent,
                   Widget menubar,
                   Widget work_area)
{
  XmMainWindowSetAreas (parent,
			menubar,	/* menubar (maybe 0) */
			0,		/* command area (psheets) */
			0,		/* horizontal scroll */
			0,              /* vertical scroll */
			work_area);	/* work area */
}

/* Motif hack to control resizing on the menubar. */
void
xm_manage_resizing (Widget w, Boolean flag)
{
  XtVaSetValues (w, XtNallowShellResize, flag, NULL);
}
