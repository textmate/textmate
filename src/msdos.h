/* MS-DOS specific C utilities, interface.
   Copyright (C) 1993, 2001-2012 Free Software Foundation, Inc.

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

#ifndef EMACS_MSDOS_H
#define EMACS_MSDOS_H

#include <dpmi.h>

int dos_ttraw (struct tty_display_info *);
int dos_ttcooked (void);
int dos_get_saved_screen (char **, int *, int *);
int dos_set_keyboard (int, int);
void dos_set_window_size (int *, int *);

int getdefdir (int, char*);
void unixtodos_filename (char *);
void dostounix_filename (char *);
char *rootrelativepath (char *);
void init_environment (int, char **, int);
void internal_terminal_init (void);
void initialize_msdos_display (struct terminal *);

extern int have_mouse;
void mouse_init (void);
void mouse_on (void);
void mouse_off (void);
void mouse_moveto (int, int);

#if __DJGPP__ == 2 && __DJGPP_MINOR__ < 4
int readlink (const char *, char *, size_t);
#endif


#ifndef HAVE_X_WINDOWS
/* Dummy types.  */
typedef int XFontStruct;
typedef int GC;
typedef int Pixmap;
typedef int Display;
typedef int Window;
typedef int XRectangle;
#define PIX_TYPE unsigned long
#define XDISPLAY

typedef struct tty_display_info Display_Info;

extern struct tty_display_info the_only_display_info;

#define FRAME_X_DISPLAY(f) ((Display *) 0)
#define FRAME_FONT(f) ((f)->output_data.tty->font)
#define FRAME_X_DISPLAY_INFO(f) (&the_only_display_info)

/* Prototypes.  */

/* Forward declarations for prototypes.  */
struct frame;
struct window;

/* Defined in xfns.c; emulated on msdos.c */

extern void x_set_menu_bar_lines (struct frame *, Lisp_Object, Lisp_Object);
extern int x_pixel_width (struct frame *);
extern int x_pixel_height (struct frame *);

#define XFreeGC (void)
#define x_destroy_bitmap(p1,p2)
#define XGetGeometry(p1,p2,p3,p4,p5,p6,p7,p8,p9)
#define DisplayWidth(p1,p2) (SELECTED_FRAME()->text_cols)
#define DisplayHeight(p1,p2) (SELECTED_FRAME()->text_lines)
#define XMenuSetAEQ (void)
#define XMenuSetFreeze (void)
#define XMenuRecompute (void)
#define FONT_WIDTH(foo) 1
#define XM_FAILURE -1
#define XM_SUCCESS 1
#define XM_NO_SELECT 2
#define XM_IA_SELECT 3
#define ButtonReleaseMask 0

typedef struct x_menu_struct
{
  int count;
  char **text;
  struct x_menu_struct **submenu;
  int *panenumber; /* Also used as enable.  */
  int allocated;
  int panecount;
  int width;
  const char **help_text;
} XMenu;

XMenu *XMenuCreate (Display *, Window, char *);
int XMenuAddPane (Display *, XMenu *, char const *, int);
int XMenuAddSelection (Display *, XMenu *, int, int, char *, int, char const *);
void XMenuLocate (Display *, XMenu *, int, int, int, int,
		  int *, int *, int *, int *);
int XMenuActivate (Display *, XMenu *, int *, int *, int, int, unsigned,
		   char **, void (*callback)(char const *, int, int));
void XMenuDestroy (Display *, XMenu *);

#endif /* not HAVE_X_WINDOWS */

#endif /* not EMACS_MSDOS_H */

