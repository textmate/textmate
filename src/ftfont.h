/* ftfont.h -- Interface definition for Freetype font backend.
   Copyright (C) 2007, 2008, 2009, 2010, 2011
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


#ifndef EMACS_FTFONT_H
#define EMACS_FTFONT_H

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_SIZES_H
#ifdef FT_BDF_H
#include FT_BDF_H
#endif

#ifdef HAVE_LIBOTF
#include <otf.h>
#ifdef HAVE_M17N_FLT
#include <m17n-flt.h>
#endif	/* HAVE_M17N_FLT */
#endif	/* HAVE_LIBOTF */

extern Lisp_Object ftfont_font_format (FcPattern *, Lisp_Object);
extern FcCharSet *ftfont_get_fc_charset (Lisp_Object);

#endif	/* EMACS_FTFONT_H */

