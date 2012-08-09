/*
Copyright (C) 1992, 1993 Lucid, Inc.
Copyright (C) 1994, 1999-2012  Free Software Foundation, Inc.

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


#ifndef LWLIB_H
#define LWLIB_H

#include <X11/Intrinsic.h>

/*
** Widget values depend on the Widget type:
**
** widget:   (name value key enabled data contents/selected)
**
** label:    ("name" "string" NULL NULL NULL NULL)
** button:   ("name" "string" "key" T/F data <default-button-p>)
** button w/menu:
**           ("name" "string" "key" T/F data (label|button|button w/menu...))
** menubar:  ("name" NULL NULL T/F data (button w/menu))
** selectable thing:
**           ("name" "string" "key" T/F data T/F)
** checkbox: selectable thing
** radio:    ("name" NULL NULL T/F data (selectable thing...))
** strings:  ("name" NULL NULL T/F data (selectable thing...))
** text:     ("name" "string" <ign> T/F data)
** main:     ("name")
*/

typedef unsigned long LWLIB_ID;

typedef enum _change_type
{
  NO_CHANGE = 0,
  INVISIBLE_CHANGE = 1,
  VISIBLE_CHANGE = 2,
  STRUCTURAL_CHANGE = 3
} change_type;

enum button_type
{
  BUTTON_TYPE_NONE,
  BUTTON_TYPE_TOGGLE,
  BUTTON_TYPE_RADIO
};

/* Menu separator types.  */

enum menu_separator
{
  /* These values are Motif compatible.  */
  SEPARATOR_NO_LINE,
  SEPARATOR_SINGLE_LINE,
  SEPARATOR_DOUBLE_LINE,
  SEPARATOR_SINGLE_DASHED_LINE,
  SEPARATOR_DOUBLE_DASHED_LINE,
  SEPARATOR_SHADOW_ETCHED_IN,
  SEPARATOR_SHADOW_ETCHED_OUT,
  SEPARATOR_SHADOW_ETCHED_IN_DASH,
  SEPARATOR_SHADOW_ETCHED_OUT_DASH,

  /* The following are supported by Lucid menus.  */
  SEPARATOR_SHADOW_DOUBLE_ETCHED_IN,
  SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT,
  SEPARATOR_SHADOW_DOUBLE_ETCHED_IN_DASH,
  SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT_DASH
};

typedef struct _widget_value
{
  /* name of widget */
  Lisp_Object   lname;
  char*		name;
  /* value (meaning depend on widget type) */
  char*		value;
  /* keyboard equivalent. no implications for XtTranslations */
  Lisp_Object   lkey;
  char*		key;
  /* Help string or nil if none.
     GC finds this string through the frame's menu_bar_vector
     or through menu_items.  */
  Lisp_Object	help;
  /* true if enabled */
  Boolean	enabled;
  /* true if selected */
  Boolean	selected;
  /* true if was edited (maintained by get_value) */
  Boolean	edited;
  /* The type of a button.  */
  enum button_type button_type;
  /* true if has changed (maintained by lw library) */
  change_type	change;
  /* true if this widget itself has changed,
     but not counting the other widgets found in the `next' field.  */
  change_type   this_one_change;
  /* Contents of the sub-widgets, also selected slot for checkbox */
  struct _widget_value*	contents;
  /* data passed to callback */
  XtPointer	call_data;
  /* next one in the list */
  struct _widget_value*	next;
  /* slot for the toolkit dependent part.  Always initialize to NULL. */
  void* toolkit_data;
  /* tell us if we should free the toolkit data slot when freeing the
     widget_value itself. */
  Boolean free_toolkit_data;

  /* we resource the widget_value structures; this points to the next
     one on the free list if this one has been deallocated.
   */
  struct _widget_value *free_list;
} widget_value;


typedef void (*lw_callback) (Widget w, LWLIB_ID id, void* data);

void  lw_register_widget (const char* type, const char* name, LWLIB_ID id,
                          widget_value* val, lw_callback pre_activate_cb,
                          lw_callback selection_cb,
                          lw_callback post_activate_cb,
                          lw_callback highlight_cb);
Widget lw_get_widget (LWLIB_ID id, Widget parent, Boolean pop_up_p);
Widget lw_make_widget (LWLIB_ID id, Widget parent, Boolean pop_up_p);
Widget lw_create_widget (const char* type, const char* name, LWLIB_ID id,
                         widget_value* val, Widget parent, Boolean pop_up_p,
                         lw_callback pre_activate_cb,
                         lw_callback selection_cb,
                         lw_callback post_activate_cb,
                         lw_callback highlight_cb);
LWLIB_ID lw_get_widget_id (Widget w);
int lw_modify_all_widgets (LWLIB_ID id, widget_value* val, Boolean deep_p);
void lw_destroy_widget (Widget w);
void lw_destroy_all_widgets (LWLIB_ID id);
void lw_destroy_everything (void);
void lw_destroy_all_pop_ups (void);
Widget lw_raise_all_pop_up_widgets (void);
widget_value* lw_get_all_values (LWLIB_ID id);
Boolean lw_get_some_values (LWLIB_ID id, widget_value* val);
void lw_pop_up_all_widgets (LWLIB_ID id);
void lw_pop_down_all_widgets (LWLIB_ID id);
widget_value *malloc_widget_value (void);
void free_widget_value (widget_value *);
void lw_popup_menu (Widget, XEvent *);

/* Toolkit independent way of focusing on a Widget at the Xt level. */
void lw_set_keyboard_focus (Widget parent, Widget w);

/* Silly Energize hack to invert the "sheet" button */
void lw_show_busy (Widget w, Boolean busy);

/* Silly hack to assist with Lucid/Athena geometry management. */
void lw_refigure_widget (Widget w, Boolean doit);

/* Toolkit independent way of determining if an event occurred on a
   menubar. */
Boolean lw_window_is_in_menubar (Window win, Widget menubar_widget);

/* Manage resizing: TRUE permits resizing widget w; FALSE disallows it. */
void lw_allow_resizing (Widget w, Boolean flag);

/* Set up the main window. */
void lw_set_main_areas (Widget parent,
                        Widget menubar,
                        Widget work_area);

/* Value is non-zero if LABEL is a menu separator.  If it is, *TYPE is
   set to an appropriate enumerator of type enum menu_separator.
   MOTIF_P non-zero means map separator types not supported by Motif
   to similar ones that are supported.  */

int lw_separator_p (const char *label, enum menu_separator *type,
                    int motif_p);

#endif /* LWLIB_H */

