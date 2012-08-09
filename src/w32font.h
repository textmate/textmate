/* Shared GDI and Uniscribe Font backend declarations for the W32 API.
   Copyright (C) 2007-2012 Free Software Foundation, Inc.

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

#ifndef EMACS_W32FONT_H
#define EMACS_W32FONT_H


/* Bit 17 of ntmFlags in NEWTEXTMETRIC is set for PostScript OpenType fonts,
   bit 18 for TrueType OpenType fonts, bit 20 for Type1 fonts.  */
#ifndef NTM_PS_OPENTYPE
#define NTM_PS_OPENTYPE 0x00020000
#endif
#ifndef NTM_TT_OPENTYPE
#define NTM_TT_OPENTYPE 0x00040000
#endif
#ifndef NTM_TYPE1
#define NTM_TYPE1 0x00100000
#endif

#define NTMFLAGS_OPENTYPE (NTM_PS_OPENTYPE | NTM_TT_OPENTYPE)

struct w32_metric_cache
{
  short lbearing, rbearing, width;
  unsigned char status;
};

#define W32METRIC_NO_ATTEMPT 0
#define W32METRIC_SUCCESS 1
#define W32METRIC_FAIL 2

/* The actual structure for a w32 font, that can be cast to struct font.
   The Uniscribe backend extends this.  */
struct w32font_info
{
  struct font font;
  TEXTMETRICW metrics;
  unsigned int glyph_idx;
  struct w32_metric_cache **cached_metrics;
  int n_cache_blocks;
  HFONT hfont;
};

/* Macros for getting OS specific information from a font struct.  */
#define FONT_HANDLE(f) (((struct w32font_info *)(f))->hfont)
#define FONT_TEXTMETRIC(f) (((struct w32font_info *)(f))->metrics)

#define CACHE_BLOCKSIZE 128

Lisp_Object w32font_get_cache (FRAME_PTR fe);
Lisp_Object w32font_list_internal (Lisp_Object frame,
                                   Lisp_Object font_spec,
                                   int opentype_only);
Lisp_Object w32font_match_internal (Lisp_Object frame,
                                    Lisp_Object font_spec,
                                    int opentype_only);
int w32font_open_internal (FRAME_PTR f, Lisp_Object font_entity,
                           int pixel_size, Lisp_Object font_object);
void w32font_close (FRAME_PTR f, struct font *font);
int w32font_has_char (Lisp_Object entity, int c);
int w32font_text_extents (struct font *font, unsigned *code, int nglyphs,
                          struct font_metrics *metrics);
int w32font_draw (struct glyph_string *s, int from, int to,
                  int x, int y, int with_background);


int uniscribe_check_otf (LOGFONT *font, Lisp_Object otf_spec);

Lisp_Object intern_font_name (char *);

#endif
