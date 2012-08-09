/* xftfont.c -- XFT font driver.
   Copyright (C) 2006-2012 Free Software Foundation, Inc.
   Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011
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

#include <config.h>
#include <stdio.h>
#include <setjmp.h>
#include <X11/Xlib.h>
#include <X11/Xft/Xft.h>

#include "lisp.h"
#include "dispextern.h"
#include "xterm.h"
#include "frame.h"
#include "blockinput.h"
#include "character.h"
#include "charset.h"
#include "composite.h"
#include "fontset.h"
#include "font.h"
#include "ftfont.h"

/* Xft font driver.  */

static Lisp_Object Qxft;
static Lisp_Object QChinting, QCautohint, QChintstyle, QCrgba, QCembolden,
  QClcdfilter;

/* The actual structure for Xft font that can be casted to struct
   font.  */

struct xftfont_info
{
  struct font font;
  /* The following five members must be here in this order to be
     compatible with struct ftfont_info (in ftfont.c).  */
#ifdef HAVE_LIBOTF
  int maybe_otf;	  /* Flag to tell if this may be OTF or not.  */
  OTF *otf;
#endif	/* HAVE_LIBOTF */
  FT_Size ft_size;
  int index;
  FT_Matrix matrix;
  Display *display;
  int screen;
  XftFont *xftfont;
};

/* Structure pointed by (struct face *)->extra  */

struct xftface_info
{
  XftColor xft_fg;		/* color for face->foreground */
  XftColor xft_bg;		/* color for face->background */
};

static void xftfont_get_colors (FRAME_PTR, struct face *, GC gc,
                                struct xftface_info *,
                                XftColor *fg, XftColor *bg);


/* Setup foreground and background colors of GC into FG and BG.  If
   XFTFACE_INFO is not NULL, reuse the colors in it if possible.  BG
   may be NULL.  */

static void
xftfont_get_colors (FRAME_PTR f, struct face *face, GC gc, struct xftface_info *xftface_info, XftColor *fg, XftColor *bg)
{
  if (xftface_info && face->gc == gc)
    {
      *fg = xftface_info->xft_fg;
      if (bg)
	*bg = xftface_info->xft_bg;
    }
  else
    {
      XGCValues xgcv;
      int fg_done = 0, bg_done = 0;

      BLOCK_INPUT;
      XGetGCValues (FRAME_X_DISPLAY (f), gc,
		    GCForeground | GCBackground, &xgcv);
      if (xftface_info)
	{
	  if (xgcv.foreground == face->foreground)
	    *fg = xftface_info->xft_fg, fg_done = 1;
	  else if (xgcv.foreground == face->background)
	    *fg = xftface_info->xft_bg, fg_done = 1;
	  if (! bg)
	    bg_done = 1;
	  else if (xgcv.background == face->background)
	    *bg = xftface_info->xft_bg, bg_done = 1;
	  else if (xgcv.background == face->foreground)
	    *bg = xftface_info->xft_fg, bg_done = 1;
	}

      if (fg_done + bg_done < 2)
	{
	  XColor colors[2];

	  colors[0].pixel = fg->pixel = xgcv.foreground;
	  if (bg)
	    colors[1].pixel = bg->pixel = xgcv.background;
	  XQueryColors (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), colors,
			bg ? 2 : 1);
	  fg->color.alpha = 0xFFFF;
	  fg->color.red = colors[0].red;
	  fg->color.green = colors[0].green;
	  fg->color.blue = colors[0].blue;
	  if (bg)
	    {
	      bg->color.alpha = 0xFFFF;
	      bg->color.red = colors[1].red;
	      bg->color.green = colors[1].green;
	      bg->color.blue = colors[1].blue;
	    }
	}
      UNBLOCK_INPUT;
    }
}


static Lisp_Object xftfont_list (Lisp_Object, Lisp_Object);
static Lisp_Object xftfont_match (Lisp_Object, Lisp_Object);
static Lisp_Object xftfont_open (FRAME_PTR, Lisp_Object, int);
static void xftfont_close (FRAME_PTR, struct font *);
static int xftfont_prepare_face (FRAME_PTR, struct face *);
static void xftfont_done_face (FRAME_PTR, struct face *);
static int xftfont_has_char (Lisp_Object, int);
static unsigned xftfont_encode_char (struct font *, int);
static int xftfont_text_extents (struct font *, unsigned *, int,
                                 struct font_metrics *);
static int xftfont_draw (struct glyph_string *, int, int, int, int, int);
static int xftfont_end_for_frame (FRAME_PTR f);

struct font_driver xftfont_driver;

static Lisp_Object
xftfont_list (Lisp_Object frame, Lisp_Object spec)
{
  Lisp_Object list = ftfont_driver.list (frame, spec), tail;

  for (tail = list; CONSP (tail); tail = XCDR (tail))
    ASET (XCAR (tail), FONT_TYPE_INDEX, Qxft);
  return list;
}

static Lisp_Object
xftfont_match (Lisp_Object frame, Lisp_Object spec)
{
  Lisp_Object entity = ftfont_driver.match (frame, spec);

  if (! NILP (entity))
    ASET (entity, FONT_TYPE_INDEX, Qxft);
  return entity;
}

static FcChar8 ascii_printable[95];

static void
xftfont_fix_match (FcPattern *pat, FcPattern *match)
{
  /*  These values are not used for matching (except antialias), but for
      rendering, so make sure they are carried over to the match.
      We also put antialias here because most fonts are antialiased, so
      the match will have antialias true.  */

  FcBool b = FcTrue;
  int i;
  double dpi;

  FcPatternGetBool (pat, FC_ANTIALIAS, 0, &b);
  if (! b)
    {
      FcPatternDel (match, FC_ANTIALIAS);
      FcPatternAddBool (match, FC_ANTIALIAS, FcFalse);
    }
  FcPatternGetBool (pat, FC_HINTING, 0, &b);
  if (! b)
    {
      FcPatternDel (match, FC_HINTING);
      FcPatternAddBool (match, FC_HINTING, FcFalse);
    }
#ifndef FC_HINT_STYLE
# define FC_HINT_STYLE "hintstyle"
#endif
  if (FcResultMatch == FcPatternGetInteger (pat, FC_HINT_STYLE, 0, &i))
    {
      FcPatternDel (match, FC_HINT_STYLE);
      FcPatternAddInteger (match, FC_HINT_STYLE, i);
    }
#ifndef FC_LCD_FILTER
  /* Older fontconfig versions don't have FC_LCD_FILTER. */
#define FC_LCD_FILTER "lcdfilter"
#endif
  if (FcResultMatch == FcPatternGetInteger (pat, FC_LCD_FILTER, 0, &i))
    {
      FcPatternDel (match, FC_LCD_FILTER);
      FcPatternAddInteger (match, FC_LCD_FILTER, i);
    }
  if (FcResultMatch == FcPatternGetInteger (pat, FC_RGBA, 0, &i))
    {
      FcPatternDel (match, FC_RGBA);
      FcPatternAddInteger (match, FC_RGBA, i);
    }
  if (FcResultMatch == FcPatternGetDouble (pat, FC_DPI, 0, &dpi))
    {
      FcPatternDel (match, FC_DPI);
      FcPatternAddDouble (match, FC_DPI, dpi);
    }
}

static void
xftfont_add_rendering_parameters (FcPattern *pat, Lisp_Object entity)
{
  Lisp_Object tail;
  int ival;

  for (tail = AREF (entity, FONT_EXTRA_INDEX); CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object key = XCAR (XCAR (tail));
      Lisp_Object val = XCDR (XCAR (tail));

      if (EQ (key, QCantialias))
          FcPatternAddBool (pat, FC_ANTIALIAS, NILP (val) ? FcFalse : FcTrue);
      else if (EQ (key, QChinting))
	FcPatternAddBool (pat, FC_HINTING, NILP (val) ? FcFalse : FcTrue);
      else if (EQ (key, QCautohint))
	FcPatternAddBool (pat, FC_AUTOHINT, NILP (val) ? FcFalse : FcTrue);
      else if (EQ (key, QChintstyle))
	{
	  if (INTEGERP (val))
	    FcPatternAddInteger (pat, FC_HINT_STYLE, XINT (val));
          else if (SYMBOLP (val)
                   && FcNameConstant (SDATA (SYMBOL_NAME (val)), &ival))
	    FcPatternAddInteger (pat, FC_HINT_STYLE, ival);
	}
      else if (EQ (key, QCrgba))
	{
	  if (INTEGERP (val))
	    FcPatternAddInteger (pat, FC_RGBA, XINT (val));
          else if (SYMBOLP (val)
                   && FcNameConstant (SDATA (SYMBOL_NAME (val)), &ival))
	    FcPatternAddInteger (pat, FC_RGBA, ival);
	}
      else if (EQ (key, QClcdfilter))
	{
	  if (INTEGERP (val))
	    FcPatternAddInteger (pat, FC_LCD_FILTER, ival = XINT (val));
          else if (SYMBOLP (val)
                   && FcNameConstant (SDATA (SYMBOL_NAME (val)), &ival))
	    FcPatternAddInteger (pat, FC_LCD_FILTER, ival);
	}
#ifdef FC_EMBOLDEN
      else if (EQ (key, QCembolden))
	FcPatternAddBool (pat, FC_EMBOLDEN, NILP (val) ? FcFalse : FcTrue);
#endif
    }
}

static Lisp_Object
xftfont_open (FRAME_PTR f, Lisp_Object entity, int pixel_size)
{
  FcResult result;
  Display *display = FRAME_X_DISPLAY (f);
  Lisp_Object val, filename, idx, font_object;
  FcPattern *pat = NULL, *match;
  struct xftfont_info *xftfont_info = NULL;
  struct font *font;
  double size = 0;
  XftFont *xftfont = NULL;
  int spacing;
  char name[256];
  int len, i;
  XGlyphInfo extents;
  FT_Face ft_face;
  FcMatrix *matrix;

  val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
  if (! CONSP (val))
    return Qnil;
  val = XCDR (val);
  filename = XCAR (val);
  idx = XCDR (val);
  size = XINT (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;
  pat = FcPatternCreate ();
  FcPatternAddInteger (pat, FC_WEIGHT, FONT_WEIGHT_NUMERIC (entity));
  i = FONT_SLANT_NUMERIC (entity) - 100;
  if (i < 0) i = 0;
  FcPatternAddInteger (pat, FC_SLANT, i);
  FcPatternAddInteger (pat, FC_WIDTH, FONT_WIDTH_NUMERIC (entity));
  FcPatternAddDouble (pat, FC_PIXEL_SIZE, pixel_size);
  val = AREF (entity, FONT_FAMILY_INDEX);
  if (! NILP (val))
    FcPatternAddString (pat, FC_FAMILY, (FcChar8 *) SDATA (SYMBOL_NAME (val)));
  val = AREF (entity, FONT_FOUNDRY_INDEX);
  if (! NILP (val))
    FcPatternAddString (pat, FC_FOUNDRY, (FcChar8 *) SDATA (SYMBOL_NAME (val)));
  val = AREF (entity, FONT_SPACING_INDEX);
  if (! NILP (val))
    FcPatternAddInteger (pat, FC_SPACING, XINT (val));
  val = AREF (entity, FONT_DPI_INDEX);
  if (! NILP (val))
    {
      double dbl = XINT (val);

      FcPatternAddDouble (pat, FC_DPI, dbl);
    }
  val = AREF (entity, FONT_AVGWIDTH_INDEX);
  if (INTEGERP (val) && XINT (val) == 0)
    FcPatternAddBool (pat, FC_SCALABLE, FcTrue);
  /* This is necessary to identify the exact font (e.g. 10x20.pcf.gz
     over 10x20-ISO8859-1.pcf.gz).  */
  FcPatternAddCharSet (pat, FC_CHARSET, ftfont_get_fc_charset (entity));

  xftfont_add_rendering_parameters (pat, entity);

  FcPatternAddString (pat, FC_FILE, (FcChar8 *) SDATA (filename));
  FcPatternAddInteger (pat, FC_INDEX, XINT (idx));


  BLOCK_INPUT;
  /* Make sure that the Xrender extension is added before the Xft one.
     Otherwise, the close-display hook set by Xft is called after the
     one for Xrender, and the former tries to re-add the latter.  This
     results in inconsistency of internal states and leads to X
     protocol error when one reconnects to the same X server.
     (Bug#1696)  */
  {
    int event_base, error_base;
    XRenderQueryExtension (display, &event_base, &error_base);
  }

  /* Substitute in values from X resources and XftDefaultSet.  */
  XftDefaultSubstitute (display, FRAME_X_SCREEN_NUMBER (f), pat);
  match = XftFontMatch (display, FRAME_X_SCREEN_NUMBER (f), pat, &result);
  xftfont_fix_match (pat, match);

  FcPatternDestroy (pat);
  xftfont = XftFontOpenPattern (display, match);
  if (!xftfont)
    {
      UNBLOCK_INPUT;
      XftPatternDestroy (match);
      return Qnil;
    }
  ft_face = XftLockFace (xftfont);
  UNBLOCK_INPUT;

  /* We should not destroy PAT here because it is kept in XFTFONT and
     destroyed automatically when XFTFONT is closed.  */
  font_object = font_make_object (VECSIZE (struct xftfont_info), entity, size);
  ASET (font_object, FONT_TYPE_INDEX, Qxft);
  len = font_unparse_xlfd (entity, size, name, 256);
  if (len > 0)
    ASET (font_object, FONT_NAME_INDEX, make_string (name, len));
  len = font_unparse_fcname (entity, size, name, 256);
  if (len > 0)
    ASET (font_object, FONT_FULLNAME_INDEX, make_string (name, len));
  else
    ASET (font_object, FONT_FULLNAME_INDEX,
	  AREF (font_object, FONT_NAME_INDEX));
  ASET (font_object, FONT_FILE_INDEX, filename);
  ASET (font_object, FONT_FORMAT_INDEX,
	ftfont_font_format (xftfont->pattern, filename));
  font = XFONT_OBJECT (font_object);
  font->pixel_size = pixel_size;
  font->driver = &xftfont_driver;
  font->encoding_charset = font->repertory_charset = -1;

  xftfont_info = (struct xftfont_info *) font;
  xftfont_info->display = display;
  xftfont_info->screen = FRAME_X_SCREEN_NUMBER (f);
  xftfont_info->xftfont = xftfont;
  /* This means that there's no need of transformation.  */
  xftfont_info->matrix.xx = 0;
  if (FcPatternGetMatrix (xftfont->pattern, FC_MATRIX, 0, &matrix)
      == FcResultMatch)
    {
      xftfont_info->matrix.xx = 0x10000L * matrix->xx;
      xftfont_info->matrix.yy = 0x10000L * matrix->yy;
      xftfont_info->matrix.xy = 0x10000L * matrix->xy;
      xftfont_info->matrix.yx = 0x10000L * matrix->yx;
    }
  font->pixel_size = size;
  font->driver = &xftfont_driver;
  if (INTEGERP (AREF (entity, FONT_SPACING_INDEX)))
    spacing = XINT (AREF (entity, FONT_SPACING_INDEX));
  else
    spacing = FC_PROPORTIONAL;
  if (! ascii_printable[0])
    {
      int ch;
      for (ch = 0; ch < 95; ch++)
	ascii_printable[ch] = ' ' + ch;
    }
  BLOCK_INPUT;
  if (spacing != FC_PROPORTIONAL
#ifdef FC_DUAL
      && spacing != FC_DUAL
#endif	/* FC_DUAL */
      )
    {
      font->min_width = font->average_width = font->space_width
	= xftfont->max_advance_width;
      XftTextExtents8 (display, xftfont, ascii_printable + 1, 94, &extents);
    }
  else
    {
      XftTextExtents8 (display, xftfont, ascii_printable, 1, &extents);
      font->space_width = extents.xOff;
      if (font->space_width <= 0)
	/* dirty workaround */
	font->space_width = pixel_size;
      XftTextExtents8 (display, xftfont, ascii_printable + 1, 94, &extents);
      font->average_width = (font->space_width + extents.xOff) / 95;
    }
  UNBLOCK_INPUT;

  font->ascent = xftfont->ascent;
  font->descent = xftfont->descent;
  if (pixel_size >= 5)
    {
      /* The above condition is a dirty workaround because
	 XftTextExtents8 behaves strangely for some fonts
	 (e.g. "Dejavu Sans Mono") when pixel_size is less than 5. */
      if (font->ascent < extents.y)
	font->ascent = extents.y;
      if (font->descent < extents.height - extents.y)
	font->descent = extents.height - extents.y;
    }
  font->height = font->ascent + font->descent;

  if (XINT (AREF (entity, FONT_SIZE_INDEX)) == 0)
    {
      int upEM = ft_face->units_per_EM;

      font->underline_position = -ft_face->underline_position * size / upEM;
      font->underline_thickness = ft_face->underline_thickness * size / upEM;
      if (font->underline_thickness > 2)
	font->underline_position -= font->underline_thickness / 2;
    }
  else
    {
      font->underline_position = -1;
      font->underline_thickness = 0;
    }
#ifdef HAVE_LIBOTF
  xftfont_info->maybe_otf = ft_face->face_flags & FT_FACE_FLAG_SFNT;
  xftfont_info->otf = NULL;
#endif	/* HAVE_LIBOTF */
  xftfont_info->ft_size = ft_face->size;

  /* Unfortunately Xft doesn't provide a way to get minimum char
     width.  So, we use space_width instead.  */
  font->min_width = font->space_width;

  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->default_ascent = 0;
  font->vertical_centering = 0;
#ifdef FT_BDF_H
  if (! (ft_face->face_flags & FT_FACE_FLAG_SFNT))
    {
      BDF_PropertyRec rec;

      if (FT_Get_BDF_Property (ft_face, "_MULE_BASELINE_OFFSET", &rec) == 0
	  && rec.type == BDF_PROPERTY_TYPE_INTEGER)
	font->baseline_offset = rec.u.integer;
      if (FT_Get_BDF_Property (ft_face, "_MULE_RELATIVE_COMPOSE", &rec) == 0
	  && rec.type == BDF_PROPERTY_TYPE_INTEGER)
	font->relative_compose = rec.u.integer;
      if (FT_Get_BDF_Property (ft_face, "_MULE_DEFAULT_ASCENT", &rec) == 0
	  && rec.type == BDF_PROPERTY_TYPE_INTEGER)
	font->default_ascent = rec.u.integer;
    }
#endif

  return font_object;
}

static void
xftfont_close (FRAME_PTR f, struct font *font)
{
  struct xftfont_info *xftfont_info = (struct xftfont_info *) font;

#ifdef HAVE_LIBOTF
  if (xftfont_info->otf)
    OTF_close (xftfont_info->otf);
#endif
  BLOCK_INPUT;
  XftUnlockFace (xftfont_info->xftfont);
  XftFontClose (xftfont_info->display, xftfont_info->xftfont);
  UNBLOCK_INPUT;
}

static int
xftfont_prepare_face (FRAME_PTR f, struct face *face)
{
  struct xftface_info *xftface_info;

#if 0
  /* This doesn't work if face->ascii_face doesn't use an Xft font. */
  if (face != face->ascii_face)
    {
      face->extra = face->ascii_face->extra;
      return 0;
    }
#endif

  xftface_info = malloc (sizeof (struct xftface_info));
  if (! xftface_info)
    return -1;
  xftfont_get_colors (f, face, face->gc, NULL,
		      &xftface_info->xft_fg, &xftface_info->xft_bg);
  face->extra = xftface_info;
  return 0;
}

static void
xftfont_done_face (FRAME_PTR f, struct face *face)
{
  struct xftface_info *xftface_info;

#if 0
  /* This doesn't work if face->ascii_face doesn't use an Xft font. */
  if (face != face->ascii_face
      || ! face->extra)
    return;
#endif

  xftface_info = (struct xftface_info *) face->extra;
  if (xftface_info)
    {
      free (xftface_info);
      face->extra = NULL;
    }
}

static int
xftfont_has_char (Lisp_Object font, int c)
{
  struct xftfont_info *xftfont_info;
  struct charset *cs = NULL;

  if (EQ (AREF (font, FONT_ADSTYLE_INDEX), Qja)
      && charset_jisx0208 >= 0)
    cs = CHARSET_FROM_ID (charset_jisx0208);
  else if (EQ (AREF (font, FONT_ADSTYLE_INDEX), Qko)
      && charset_ksc5601 >= 0)
    cs = CHARSET_FROM_ID (charset_ksc5601);
  if (cs)
    return (ENCODE_CHAR (cs, c) != CHARSET_INVALID_CODE (cs));

  if (FONT_ENTITY_P (font))
    return ftfont_driver.has_char (font, c);
  xftfont_info = (struct xftfont_info *) XFONT_OBJECT (font);
  return (XftCharExists (xftfont_info->display, xftfont_info->xftfont,
			 (FcChar32) c) == FcTrue);
}

static unsigned
xftfont_encode_char (struct font *font, int c)
{
  struct xftfont_info *xftfont_info = (struct xftfont_info *) font;
  unsigned code = XftCharIndex (xftfont_info->display, xftfont_info->xftfont,
				(FcChar32) c);

  return (code ? code : FONT_INVALID_CODE);
}

static int
xftfont_text_extents (struct font *font, unsigned int *code, int nglyphs, struct font_metrics *metrics)
{
  struct xftfont_info *xftfont_info = (struct xftfont_info *) font;
  XGlyphInfo extents;

  BLOCK_INPUT;
  XftGlyphExtents (xftfont_info->display, xftfont_info->xftfont, code, nglyphs,
		   &extents);
  UNBLOCK_INPUT;
  if (metrics)
    {
      metrics->lbearing = - extents.x;
      metrics->rbearing = - extents.x + extents.width;
      metrics->width = extents.xOff;
      metrics->ascent = extents.y;
      metrics->descent = extents.height - extents.y;
    }
  return extents.xOff;
}

static XftDraw *
xftfont_get_xft_draw (FRAME_PTR f)
{
  XftDraw *xft_draw = font_get_frame_data (f, &xftfont_driver);

  if (! xft_draw)
    {
      BLOCK_INPUT;
      xft_draw= XftDrawCreate (FRAME_X_DISPLAY (f),
			       FRAME_X_WINDOW (f),
			       FRAME_X_VISUAL (f),
			       FRAME_X_COLORMAP (f));
      UNBLOCK_INPUT;
      if (! xft_draw)
	abort ();
      font_put_frame_data (f, &xftfont_driver, xft_draw);
    }
  return xft_draw;
}

static int
xftfont_draw (struct glyph_string *s, int from, int to, int x, int y, int with_background)
{
  FRAME_PTR f = s->f;
  struct face *face = s->face;
  struct xftfont_info *xftfont_info = (struct xftfont_info *) s->font;
  struct xftface_info *xftface_info = NULL;
  XftDraw *xft_draw = xftfont_get_xft_draw (f);
  FT_UInt *code;
  XftColor fg, bg;
  int len = to - from;
  int i;

  if (s->font == face->font)
    xftface_info = (struct xftface_info *) face->extra;
  xftfont_get_colors (f, face, s->gc, xftface_info,
		      &fg, with_background ? &bg : NULL);
  BLOCK_INPUT;
  if (s->num_clips > 0)
    XftDrawSetClipRectangles (xft_draw, 0, 0, s->clip, s->num_clips);
  else
    XftDrawSetClip (xft_draw, NULL);

  if (with_background)
    XftDrawRect (xft_draw, &bg,
		 x, y - s->font->ascent, s->width, s->font->height);
  code = alloca (sizeof (FT_UInt) * len);
  for (i = 0; i < len; i++)
    code[i] = ((XCHAR2B_BYTE1 (s->char2b + from + i) << 8)
	       | XCHAR2B_BYTE2 (s->char2b + from + i));

  if (s->padding_p)
    for (i = 0; i < len; i++)
      XftDrawGlyphs (xft_draw, &fg, xftfont_info->xftfont,
		     x + i, y, code + i, 1);
  else
    XftDrawGlyphs (xft_draw, &fg, xftfont_info->xftfont,
		   x, y, code, len);
  UNBLOCK_INPUT;

  return len;
}

#if defined HAVE_M17N_FLT && defined HAVE_LIBOTF
static Lisp_Object
xftfont_shape (Lisp_Object lgstring)
{
  struct font *font;
  struct xftfont_info *xftfont_info;
  FT_Face ft_face;
  Lisp_Object val;

  CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring), font);
  xftfont_info = (struct xftfont_info *) font;
  ft_face = XftLockFace (xftfont_info->xftfont);
  xftfont_info->ft_size = ft_face->size;
  val = ftfont_driver.shape (lgstring);
  XftUnlockFace (xftfont_info->xftfont);
  return val;
}
#endif

static int
xftfont_end_for_frame (FRAME_PTR f)
{
  XftDraw *xft_draw;

  /* Don't do anything if display is dead */
  if (FRAME_X_DISPLAY (f) == NULL) return 0;

  xft_draw = font_get_frame_data (f, &xftfont_driver);

  if (xft_draw)
    {
      BLOCK_INPUT;
      XftDrawDestroy (xft_draw);
      UNBLOCK_INPUT;
      font_put_frame_data (f, &xftfont_driver, NULL);
    }
  return 0;
}

static int
xftfont_cached_font_ok (struct frame *f, Lisp_Object font_object, Lisp_Object entity)
{
  struct xftfont_info *info = (struct xftfont_info *) XFONT_OBJECT (font_object);
  FcPattern *oldpat = info->xftfont->pattern;
  Display *display = FRAME_X_DISPLAY (f);
  FcPattern *pat = FcPatternCreate ();
  FcBool b1, b2;
  int ok = 0, i1, i2, r1, r2;

  xftfont_add_rendering_parameters (pat, entity);
  XftDefaultSubstitute (display, FRAME_X_SCREEN_NUMBER (f), pat);

  r1 = FcPatternGetBool (pat, FC_ANTIALIAS, 0, &b1);
  r2 = FcPatternGetBool (oldpat, FC_ANTIALIAS, 0, &b2);
  if (r1 != r2 || b1 != b2) goto out;
  r1 = FcPatternGetBool (pat, FC_HINTING, 0, &b1);
  r2 = FcPatternGetBool (oldpat, FC_HINTING, 0, &b2);
  if (r1 != r2 || b1 != b2) goto out;
  r1 = FcPatternGetBool (pat, FC_AUTOHINT, 0, &b1);
  r2 = FcPatternGetBool (oldpat, FC_AUTOHINT, 0, &b2);
  if (r1 != r2 || b1 != b2) goto out;
#ifdef FC_EMBOLDEN
  r1 = FcPatternGetBool (pat, FC_EMBOLDEN, 0, &b1);
  r2 = FcPatternGetBool (oldpat, FC_EMBOLDEN, 0, &b2);
  if (r1 != r2 || b1 != b2) goto out;
#endif
  r1 = FcPatternGetInteger (pat, FC_HINT_STYLE, 0, &i1);
  r2 = FcPatternGetInteger (oldpat, FC_HINT_STYLE, 0, &i2);
  if (r1 != r2 || i1 != i2) goto out;
  r1 = FcPatternGetInteger (pat, FC_LCD_FILTER, 0, &i1);
  r2 = FcPatternGetInteger (oldpat, FC_LCD_FILTER, 0, &i2);
  if (r1 != r2 || i1 != i2) goto out;
  r1 = FcPatternGetInteger (pat, FC_RGBA, 0, &i1);
  r2 = FcPatternGetInteger (oldpat, FC_RGBA, 0, &i2);
  if (r1 != r2 || i1 != i2) goto out;

  ok = 1;
 out:
  FcPatternDestroy (pat);
  return ok;
}

void
syms_of_xftfont (void)
{
  DEFSYM (Qxft, "xft");
  DEFSYM (QChinting, ":hinting");
  DEFSYM (QCautohint, ":autohint");
  DEFSYM (QChintstyle, ":hintstyle");
  DEFSYM (QCrgba, ":rgba");
  DEFSYM (QCembolden, ":embolden");
  DEFSYM (QClcdfilter, ":lcdfilter");

  xftfont_driver = ftfont_driver;
  xftfont_driver.type = Qxft;
  xftfont_driver.get_cache = xfont_driver.get_cache;
  xftfont_driver.list = xftfont_list;
  xftfont_driver.match = xftfont_match;
  xftfont_driver.open = xftfont_open;
  xftfont_driver.close = xftfont_close;
  xftfont_driver.prepare_face = xftfont_prepare_face;
  xftfont_driver.done_face = xftfont_done_face;
  xftfont_driver.has_char = xftfont_has_char;
  xftfont_driver.encode_char = xftfont_encode_char;
  xftfont_driver.text_extents = xftfont_text_extents;
  xftfont_driver.draw = xftfont_draw;
  xftfont_driver.end_for_frame = xftfont_end_for_frame;
  xftfont_driver.cached_font_ok = xftfont_cached_font_ok;
#if defined (HAVE_M17N_FLT) && defined (HAVE_LIBOTF)
  xftfont_driver.shape = xftfont_shape;
#endif

  register_font_driver (&xftfont_driver, NULL);
}
