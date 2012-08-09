/* Internals of a lightweight menubar widget.

Copyright (C) 2002-2012  Free Software Foundation, Inc.
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
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef _XlwMenuP_h
#define _XlwMenuP_h

#include "xlwmenu.h"
#include <X11/CoreP.h>
#ifdef HAVE_XFT
#include <X11/Xft/Xft.h>
#endif

/* Elements in the stack arrays. */
typedef struct _window_state
{
  Widget        w;
  Window	window;
  Pixmap        pixmap;
  Position	x;
  Position	y;
  Dimension	width;
  Dimension	height;
  Dimension	label_width;
  int           max_rest_width;

  /* Width of toggle buttons or radio buttons.  */
  Dimension     button_width;
#ifdef HAVE_XFT
  XftDraw*      xft_draw;
#endif
} window_state;


/* New fields for the XlwMenu widget instance record */
typedef struct _XlwMenu_part
{
  /* slots set by the resources */
#ifdef HAVE_X_I18N
  XFontSet	fontSet;
  XFontSetExtents *font_extents;
#endif
#ifdef HAVE_XFT
  int           default_face;
  XftFont*      xft_font;
  XftColor      xft_fg, xft_bg, xft_disabled_fg;
#endif
  String	fontName;
  XFontStruct*	font;
  Pixel		foreground;
  Pixel		disabled_foreground;
  Pixel		button_foreground;
  Dimension	margin;
  Dimension	horizontal_spacing;
  Dimension	vertical_spacing;
  Dimension	arrow_spacing;
  Dimension	shadow_thickness;
  Pixel 	top_shadow_color;
  Pixel 	bottom_shadow_color;
  Pixmap	top_shadow_pixmap;
  Pixmap	bottom_shadow_pixmap;
  Cursor	cursor_shape;
  XtCallbackList	open;
  XtCallbackList	select, highlight;
  XtCallbackList        enter, leave;
  widget_value*	contents;
  int		horizontal;

  /* True means top_shadow_color and/or bottom_shadow_color must be freed.  */
  unsigned free_top_shadow_color_p : 1;
  unsigned free_bottom_shadow_color_p : 1;

  /* State of the XlwMenu */
  int                   top_depth;
  int			old_depth;
  widget_value**	old_stack;
  int			old_stack_length;
  widget_value*         inside_entry;

  /* New state after the user moved */
  int			new_depth;
  widget_value**	new_stack;
  int			new_stack_length;

  /* Window resources */
  window_state*		windows;
  int			windows_length;

  /* Internal part, set by the XlwMenu */
  GC			foreground_gc;
  GC			button_gc;
  GC			background_gc;
  GC			disabled_gc;
  GC			inactive_button_gc;
  GC			shadow_top_gc;
  GC			shadow_bottom_gc;
  Cursor		cursor;
  Boolean		popped_up;
  Pixmap		gray_pixmap;
} XlwMenuPart;

/* Full instance record declaration */
typedef struct _XlwMenuRec
{
  CorePart	core;
  XlwMenuPart	menu;
} XlwMenuRec;

/* New fields for the XlwMenu widget class record */
typedef struct
{
  int	dummy;
} XlwMenuClassPart;

/* Full class record declaration. */
typedef struct _XlwMenuClassRec
{
  CoreClassPart		core_class;
  XlwMenuClassPart	menu_class;
} XlwMenuClassRec;

/* Class pointer. */
extern XlwMenuClassRec xlwMenuClassRec;

#endif /* _XlwMenuP_h */

