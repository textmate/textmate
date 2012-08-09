/* The emacs frame widget private header file.
   Copyright (C) 1993, 2001-2012  Free Software Foundation, Inc.

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

/* Emacs 19 face widget ported by Fred Pierresteguy */

#ifndef _EmacsFrameP_h
#define _EmacsFrameP_h

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include "widget.h"

typedef struct {
  struct frame*	frame;		/* the *emacs* frame object */

  /* Resources that can't be done from lisp.
   */
  char*		geometry;		/* geometry spec of this frame */
  Boolean	iconic;			/* whether this frame is iconic */

  /* The rest of this is crap and should be deleted.
   */
  int		minibuffer;	/* 0: normal frames with minibuffers.
				 * 1: frames without minibuffers
				 * 2: minibuffer only. */
  Boolean	unsplittable;	/* frame can only have one window */

  int		internal_border_width;	/* internal borders */
  int		interline;		/* skips between lines */

  struct font*	font;			/* font */
  Pixel		foreground_pixel;	/* foreground */

  Pixel		cursor_color;		/* text cursor color */
  Boolean	bar_cursor;		/* 1 if bar, 0 if block */

  Boolean	visual_bell;		/* flash instead of beep */
  int		bell_volume;		/* how loud is beep */

  /* private state */

} EmacsFramePart;

typedef struct _EmacsFrameRec {	/* full instance record */
    CorePart		core;
    EmacsFramePart	emacs_frame;
} EmacsFrameRec;

typedef struct {			/* new fields for EmacsFrame class */
    int dummy;
} EmacsFrameClassPart;

typedef struct _EmacsFrameClassRec {	/* full class record declaration */
    CoreClassPart		core_class;
    EmacsFrameClassPart	emacs_frame_class;
} EmacsFrameClassRec;

#endif /* _EmacsFrameP_h */
