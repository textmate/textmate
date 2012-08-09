/* Definitions and headers for GTK widgets.

Copyright (C) 2003-2012  Free Software Foundation, Inc.

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

#ifndef GTKUTIL_H
#define GTKUTIL_H


#ifdef USE_GTK

#include <gtk/gtk.h>
#include "frame.h"

/* Minimum and maximum values used for GTK scroll bars  */

#define XG_SB_MIN 1
#define XG_SB_MAX 10000000
#define XG_SB_RANGE (XG_SB_MAX-XG_SB_MIN)

/* Key for data that is valid for menus and scroll bars in a frame  */
#define XG_FRAME_DATA "emacs_frame"

/* Key for data that menu items hold.  */
#define XG_ITEM_DATA "emacs_menuitem"

/* This is a list node in a generic list implementation.  */
typedef struct xg_list_node_
{
  struct xg_list_node_ *prev;
  struct xg_list_node_ *next;
} xg_list_node;

/* This structure is the callback data that is shared for menu items.
   We need to keep it separate from the frame structure due to
   detachable menus.  The data in the frame structure is only valid while
   the menu is popped up.  This structure is kept around as long as
   the menu is.  */
typedef struct xg_menu_cb_data_
{
  xg_list_node  ptrs;

  FRAME_PTR     f;
  Lisp_Object   menu_bar_vector;
  int           menu_bar_items_used;
  GCallback     highlight_cb;
  int           ref_count;
} xg_menu_cb_data;

/* This structure holds callback information for each individual menu item.  */
typedef struct xg_menu_item_cb_data_
{
  xg_list_node  ptrs;

  gulong        select_id;
  Lisp_Object   help;
  gpointer	call_data;
  xg_menu_cb_data *cl_data;

} xg_menu_item_cb_data;


#ifdef HAVE_GTK_FILE_SELECTION_NEW
extern int use_old_gtk_file_dialog;
#endif
struct _widget_value;

extern struct _widget_value *malloc_widget_value (void);
extern void free_widget_value (struct _widget_value *);

extern int xg_uses_old_file_dialog (void);

extern char *xg_get_file_name (FRAME_PTR f,
                               char *prompt,
                               char *default_filename,
                               int mustmatch_p,
                               int only_dir_p);

extern char *xg_get_font_name (FRAME_PTR f, const char *);

extern GtkWidget *xg_create_widget (const char *type,
                                    const char *name,
                                    FRAME_PTR f,
                                    struct _widget_value *val,
                                    GCallback select_cb,
                                    GCallback deactivate_cb,
                                    GCallback highlight_cb);

extern void xg_modify_menubar_widgets (GtkWidget *menubar,
                                       FRAME_PTR f,
                                       struct _widget_value *val,
                                       int deep_p,
                                       GCallback select_cb,
                                       GCallback deactivate_cb,
                                       GCallback highlight_cb);

extern int xg_update_frame_menubar (FRAME_PTR f);

extern int xg_event_is_for_menubar (FRAME_PTR f, XEvent *event);

extern int xg_have_tear_offs (void);

extern ptrdiff_t xg_get_scroll_id_for_window (Display *dpy, Window wid);

extern void xg_create_scroll_bar (FRAME_PTR f,
                                  struct scroll_bar *bar,
                                  GCallback scroll_callback,
                                  GCallback end_callback,
                                  const char *scroll_bar_name);
extern void xg_remove_scroll_bar (FRAME_PTR f, ptrdiff_t scrollbar_id);

extern void xg_update_scrollbar_pos (FRAME_PTR f,
                                     ptrdiff_t scrollbar_id,
                                     int top,
                                     int left,
                                     int width,
                                     int height);

extern void xg_set_toolkit_scroll_bar_thumb (struct scroll_bar *bar,
                                             int portion,
                                             int position,
                                             int whole);
extern int xg_event_is_for_scrollbar (FRAME_PTR f, XEvent *event);
extern int xg_get_default_scrollbar_width (void);

extern void update_frame_tool_bar (FRAME_PTR f);
extern void free_frame_tool_bar (FRAME_PTR f);
extern int xg_change_toolbar_position (FRAME_PTR f, Lisp_Object pos);

extern void xg_frame_resized (FRAME_PTR f,
                              int pixelwidth,
                              int pixelheight);
extern void xg_frame_set_char_size (FRAME_PTR f, int cols, int rows);
extern GtkWidget * xg_win_to_widget (Display *dpy, Window wdesc);

extern void xg_display_open (char *display_name, Display **dpy);
extern void xg_display_close (Display *dpy);
extern GdkCursor * xg_create_default_cursor (Display *dpy);

extern int xg_create_frame_widgets (FRAME_PTR f);
extern void xg_free_frame_widgets (FRAME_PTR f);
extern void x_wm_set_size_hint (FRAME_PTR f,
                                long flags,
                                int user_position);
extern void xg_set_background_color (FRAME_PTR f, unsigned long bg);
extern int xg_check_special_colors (struct frame *f,
                                    const char *color_name,
                                    XColor *color);

extern void xg_set_frame_icon (FRAME_PTR f,
                               Pixmap icon_pixmap,
                               Pixmap icon_mask);

extern int xg_prepare_tooltip (FRAME_PTR f,
                               Lisp_Object string,
                               int *width,
                               int *height);
extern void xg_show_tooltip (FRAME_PTR f, int root_x, int root_y);
extern int xg_hide_tooltip (FRAME_PTR f);


/* Mark all callback data that are Lisp_object:s during GC.  */
extern void xg_mark_data (void);

/* Initialize GTK specific parts.  */
extern void xg_initialize (void);

/* Setting scrollbar values invokes the callback.  Use this variable
   to indicate that the callback should do nothing.  */
extern int xg_ignore_gtk_scrollbar;

#endif /* USE_GTK */
#endif /* GTKUTIL_H */
