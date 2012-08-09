/* Functions to manipulate menus.
   Copyright (C) 2008-2012 Free Software Foundation, Inc.

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

#ifndef MENU_H
#define MENU_H

#include "systime.h" /* for Time */

extern void x_set_menu_bar_lines (struct frame *f,
                                  Lisp_Object value,
                                  Lisp_Object oldval);

extern void init_menu_items (void);
extern void finish_menu_items (void);
extern void discard_menu_items (void);
extern void save_menu_items (void);
extern int parse_single_submenu (Lisp_Object, Lisp_Object, Lisp_Object);
extern void list_of_panes (Lisp_Object);
#if defined (USE_X_TOOLKIT) || defined (USE_GTK) || defined (HAVE_NTGUI) \
  || defined (HAVE_NS)
extern void free_menubar_widget_value_tree (widget_value *);
extern void update_submenu_strings (widget_value *);
extern void find_and_call_menu_selection (FRAME_PTR, int,
                                          Lisp_Object, void *);
extern widget_value *xmalloc_widget_value (void);
extern widget_value *digest_single_submenu (int, int, int);
#endif

#ifdef HAVE_X_WINDOWS
extern void mouse_position_for_popup (FRAME_PTR f, int *x, int *y);
#endif

extern Lisp_Object w32_menu_show (FRAME_PTR, int, int, int, int,
				  Lisp_Object, const char **);
extern Lisp_Object ns_menu_show (FRAME_PTR, int, int, int, int,
				 Lisp_Object, const char **);
extern Lisp_Object xmenu_show (FRAME_PTR, int, int, int, int,
			       Lisp_Object, const char **, Time);
#endif /* MENU_H */
