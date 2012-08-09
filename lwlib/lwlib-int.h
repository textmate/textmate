/*
Copyright (C) 1992 Lucid, Inc.
Copyright (C) 2000-2012 Free Software Foundation, Inc.

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


#ifndef LWLIB_INTERNAL_H
#define LWLIB_INTERNAL_H

#include "lwlib.h"

extern char *safe_strdup (const char *);

struct widget_xft_data;

typedef struct _widget_instance
{
  Widget		widget;
  Widget		parent;
  Boolean		pop_up_p;
#ifdef HAVE_XFT
  struct widget_xft_data *xft_data;
  int                   nr_xft_data;
#endif
  struct _widget_info*		info;
  struct _widget_instance*	next;
} widget_instance;

typedef struct _widget_info
{
  char*			type;
  char*			name;
  LWLIB_ID		id;
  widget_value*		val;
  Boolean		busy;
  lw_callback		pre_activate_cb;
  lw_callback		selection_cb;
  lw_callback		post_activate_cb;
  lw_callback		highlight_cb;
  struct _widget_instance*	instances;
  struct _widget_info*		next;
} widget_info;

typedef Widget (*widget_creation_function) (widget_instance *instance);

typedef struct _widget_creation_entry
{
  char*				type;
  widget_creation_function	function;
} widget_creation_entry;

/* update all other instances of a widget.  Can be used in a callback when
   a widget has been used by the user */
void
lw_internal_update_other_instances (Widget, XtPointer, XtPointer);

/* get the widget_value for a widget in a given instance */
widget_value*
lw_get_widget_value_for_widget (widget_instance *, Widget);

widget_info *lw_get_widget_info (LWLIB_ID);
widget_instance * lw_get_widget_instance (Widget);

#endif /* LWLIB_INTERNAL_H */

