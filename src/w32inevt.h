/* Input routines for GNU Emacs on the Microsoft W32 API.
   Copyright (C) 1995, 2001-2012  Free Software Foundation, Inc.

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

#ifndef EMACS_W32INEVT_H
#define EMACS_W32INEVT_H

extern int w32_console_read_socket (struct terminal *term, int numchars,
				    struct input_event *hold_quit);
extern void w32_console_mouse_position (FRAME_PTR *f, int insist,
					Lisp_Object *bar_window,
					enum scroll_bar_part *part,
					Lisp_Object *x, Lisp_Object *y,
					unsigned long *time);

#endif /* EMACS_W32INEVT_H */

