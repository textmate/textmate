/* The lwlib interface to Motif widgets.
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

#ifndef LWLIB_XM_H
#define LWLIB_XM_H

#include "lwlib-int.h"

extern widget_creation_entry xm_creation_table [];

Widget
xm_create_dialog (widget_instance* instance);

Boolean
lw_motif_widget_p (Widget widget);

void
xm_update_one_widget (widget_instance* instance, Widget widget,
                      widget_value* val, Boolean deep_p);

void
xm_update_one_value (widget_instance* instance, Widget widget,
                     widget_value* val);

void
xm_destroy_instance (widget_instance* instance);

void
xm_set_keyboard_focus (Widget parent, Widget w);

void
xm_popup_menu (Widget widget, XEvent *event);

void
xm_pop_instance (widget_instance* instance, Boolean up);

void
xm_set_main_areas (Widget parent, Widget menubar, Widget work_area);

void
xm_manage_resizing (Widget w, Boolean flag);

#endif /* LWLIB_XM_H */

