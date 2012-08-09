/* Header for fontset handler.
   Copyright (C) 1998, 2001-2012  Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
     2005, 2006, 2007, 2008, 2009, 2010, 2011
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003, 2006
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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

#ifndef EMACS_FONTSET_H
#define EMACS_FONTSET_H

/* Check if any window system is used now.  */
extern void (*check_window_system_func) (void);

struct face;

extern void free_face_fontset (FRAME_PTR, struct face *);
extern int face_for_char (FRAME_PTR, struct face *, int,
                          int, Lisp_Object);
extern Lisp_Object font_for_char (struct face *, int, int, Lisp_Object);

extern int make_fontset_for_ascii_face (FRAME_PTR, int, struct face *);
extern int fontset_from_font (Lisp_Object);
extern int fs_query_fontset (Lisp_Object, int);
EXFUN (Fquery_fontset, 2);
extern Lisp_Object list_fontsets (struct frame *, Lisp_Object, int);

extern Lisp_Object Qlatin;
extern Lisp_Object fontset_name (int);
extern Lisp_Object fontset_ascii (int);

struct font;
extern int face_for_font (struct frame *, Lisp_Object, struct face *);

#endif /* EMACS_FONTSET_H */
