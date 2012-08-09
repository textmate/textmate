/* ftfont.c -- FreeType font driver.
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

#include <fontconfig/fontconfig.h>
#include <fontconfig/fcfreetype.h>

#include "lisp.h"
#include "dispextern.h"
#include "frame.h"
#include "blockinput.h"
#include "character.h"
#include "charset.h"
#include "coding.h"
#include "composite.h"
#include "fontset.h"
#include "font.h"
#include "ftfont.h"

/* Symbolic type of this font-driver.  */
static Lisp_Object Qfreetype;

/* Fontconfig's generic families and their aliases.  */
static Lisp_Object Qmonospace, Qsans_serif, Qserif, Qmono, Qsans, Qsans__serif;

/* Flag to tell if FcInit is already called or not.  */
static int fc_initialized;

/* Handle to a FreeType library instance.  */
static FT_Library ft_library;

/* Cache for FreeType fonts.  */
static Lisp_Object freetype_font_cache;

/* Cache for FT_Face and FcCharSet. */
static Lisp_Object ft_face_cache;

/* The actual structure for FreeType font that can be casted to struct
   font.  */

struct ftfont_info
{
  struct font font;
#ifdef HAVE_LIBOTF
  /* The following four members must be here in this order to be
     compatible with struct xftfont_info (in xftfont.c).  */
  int maybe_otf;	/* Flag to tell if this may be OTF or not.  */
  OTF *otf;
#endif	/* HAVE_LIBOTF */
  FT_Size ft_size;
  int index;
  FT_Matrix matrix;
};

enum ftfont_cache_for
  {
    FTFONT_CACHE_FOR_FACE,
    FTFONT_CACHE_FOR_CHARSET,
    FTFONT_CACHE_FOR_ENTITY
  };

static Lisp_Object ftfont_pattern_entity (FcPattern *, Lisp_Object);

static Lisp_Object ftfont_resolve_generic_family (Lisp_Object,
                                                  FcPattern *);
static Lisp_Object ftfont_lookup_cache (Lisp_Object,
                                        enum ftfont_cache_for);

static void ftfont_filter_properties (Lisp_Object font, Lisp_Object alist);

Lisp_Object ftfont_font_format (FcPattern *, Lisp_Object);

#define SYMBOL_FcChar8(SYM) (FcChar8 *) SDATA (SYMBOL_NAME (SYM))

static struct
{
  /* registry name */
  const char *name;
  /* characters to distinguish the charset from the others */
  int uniquifier[6];
  /* additional constraint by language */
  const char *lang;
  /* set on demand */
  FcCharSet *fc_charset;
} fc_charset_table[] =
  { { "iso8859-1", { 0x00A0, 0x00A1, 0x00B4, 0x00BC, 0x00D0 } },
    { "iso8859-2", { 0x00A0, 0x010E }},
    { "iso8859-3", { 0x00A0, 0x0108 }},
    { "iso8859-4", { 0x00A0, 0x00AF, 0x0128, 0x0156, 0x02C7 }},
    { "iso8859-5", { 0x00A0, 0x0401 }},
    { "iso8859-6", { 0x00A0, 0x060C }},
    { "iso8859-7", { 0x00A0, 0x0384 }},
    { "iso8859-8", { 0x00A0, 0x05D0 }},
    { "iso8859-9", { 0x00A0, 0x00A1, 0x00BC, 0x011E }},
    { "iso8859-10", { 0x00A0, 0x00D0, 0x0128, 0x2015 }},
    { "iso8859-11", { 0x00A0, 0x0E01 }},
    { "iso8859-13", { 0x00A0, 0x201C }},
    { "iso8859-14", { 0x00A0, 0x0174 }},
    { "iso8859-15", { 0x00A0, 0x00A1, 0x00D0, 0x0152 }},
    { "iso8859-16", { 0x00A0, 0x0218}},
    { "gb2312.1980-0", { 0x4E13 }, "zh-cn"},
    { "big5-0", { 0xF6B1 }, "zh-tw" },
    { "jisx0208.1983-0", { 0x4E55 }, "ja"},
    { "ksc5601.1985-0", { 0xAC00 }, "ko"},
    { "cns11643.1992-1", { 0xFE32 }, "zh-tw"},
    { "cns11643.1992-2", { 0x4E33, 0x7934 }},
    { "cns11643.1992-3", { 0x201A9 }},
    { "cns11643.1992-4", { 0x20057 }},
    { "cns11643.1992-5", { 0x20000 }},
    { "cns11643.1992-6", { 0x20003 }},
    { "cns11643.1992-7", { 0x20055 }},
    { "gbk-0", { 0x4E06 }, "zh-cn"},
    { "jisx0212.1990-0", { 0x4E44 }},
    { "jisx0213.2000-1", { 0xFA10 }, "ja"},
    { "jisx0213.2000-2", { 0xFA49 }},
    { "jisx0213.2004-1", { 0x20B9F }},
    { "viscii1.1-1", { 0x1EA0, 0x1EAE, 0x1ED2 }, "vi"},
    { "tis620.2529-1", { 0x0E01 }, "th"},
    { "windows-1251", { 0x0401, 0x0490 }, "ru"},
    { "koi8-r", { 0x0401, 0x2219 }, "ru"},
    { "mulelao-1", { 0x0E81 }, "lo"},
    { "unicode-sip", { 0x20000 }},
    { NULL }
  };

/* Dirty hack for handing ADSTYLE property.

   Fontconfig (actually the underlying FreeType) gives such ADSTYLE
   font property of PCF/BDF fonts in FC_STYLE.  And, "Bold",
   "Oblique", "Italic", or any non-normal SWIDTH property names
   (e.g. SemiCondensed) are appended.  In addition, if there's no
   ADSTYLE property nor non-normal WEIGHT/SLANT/SWIDTH properties,
   "Regular" is used for FC_STYLE (see the function
   pcf_interpret_style in src/pcf/pcfread.c of FreeType).

   Unfortunately this behavior is not documented, so the following
   code may fail if FreeType changes the behavior in the future.  */

static Lisp_Object
get_adstyle_property (FcPattern *p)
{
  FcChar8 *fcstr;
  char *str, *end;
  Lisp_Object adstyle;

#ifdef FC_FONTFORMAT
  if ((FcPatternGetString (p, FC_FONTFORMAT, 0, &fcstr) == FcResultMatch)
      && xstrcasecmp ((char *) fcstr, "bdf") != 0
      && xstrcasecmp ((char *) fcstr, "pcf") != 0)
    /* Not a BDF nor PCF font.  */
    return Qnil;
#endif
  if (FcPatternGetString (p, FC_STYLE, 0, &fcstr) != FcResultMatch)
    return Qnil;
  str = (char *) fcstr;
  for (end = str; *end && *end != ' '; end++);
  if (*end)
    {
      char *newstr = alloca (end - str + 1);
      memcpy (newstr, str, end - str);
      newstr[end - str] = '\0';
      end = newstr + (end - str);
      str = newstr;
    }
  if (xstrcasecmp (str, "Regular") == 0
      || xstrcasecmp (str, "Bold") == 0
      || xstrcasecmp (str, "Oblique") == 0
      || xstrcasecmp (str, "Italic") == 0)
    return Qnil;
  adstyle = font_intern_prop (str, end - str, 1);
  if (font_style_to_value (FONT_WIDTH_INDEX, adstyle, 0) >= 0)
    return Qnil;
  return adstyle;
}

static Lisp_Object
ftfont_pattern_entity (FcPattern *p, Lisp_Object extra)
{
  Lisp_Object key, cache, entity;
  FcChar8 *str;
  char *file;
  int idx;
  int numeric;
  double dbl;
  FcBool b;

  if (FcPatternGetString (p, FC_FILE, 0, &str) != FcResultMatch)
    return Qnil;
  if (FcPatternGetInteger (p, FC_INDEX, 0, &idx) != FcResultMatch)
    return Qnil;

  file = (char *) str;
  key = Fcons (make_unibyte_string (file, strlen (file)), make_number (idx));
  cache = ftfont_lookup_cache (key, FTFONT_CACHE_FOR_ENTITY);
  entity = XCAR (cache);
  if (! NILP (entity))
    {
      Lisp_Object val = font_make_entity ();
      int i;

      for (i = 0; i < FONT_OBJLIST_INDEX; i++)
	ASET (val, i, AREF (entity, i));

      ASET (val, FONT_EXTRA_INDEX, Fcopy_sequence (extra));
      font_put_extra (val, QCfont_entity, key);

      return val;
    }
  entity = font_make_entity ();
  XSETCAR (cache, entity);

  ASET (entity, FONT_TYPE_INDEX, Qfreetype);
  ASET (entity, FONT_REGISTRY_INDEX, Qiso10646_1);

  if (FcPatternGetString (p, FC_FOUNDRY, 0, &str) == FcResultMatch)
    {
      char *s = (char *) str;
      ASET (entity, FONT_FOUNDRY_INDEX, font_intern_prop (s, strlen (s), 1));
    }
  if (FcPatternGetString (p, FC_FAMILY, 0, &str) == FcResultMatch)
    {
      char *s = (char *) str;
      ASET (entity, FONT_FAMILY_INDEX, font_intern_prop (s, strlen (s), 1));
    }
  if (FcPatternGetInteger (p, FC_WEIGHT, 0, &numeric) == FcResultMatch)
    {
      if (numeric >= FC_WEIGHT_REGULAR && numeric < FC_WEIGHT_MEDIUM)
	numeric = FC_WEIGHT_MEDIUM;
      FONT_SET_STYLE (entity, FONT_WEIGHT_INDEX, make_number (numeric));
    }
  if (FcPatternGetInteger (p, FC_SLANT, 0, &numeric) == FcResultMatch)
    {
      numeric += 100;
      FONT_SET_STYLE (entity, FONT_SLANT_INDEX, make_number (numeric));
    }
  if (FcPatternGetInteger (p, FC_WIDTH, 0, &numeric) == FcResultMatch)
    {
      FONT_SET_STYLE (entity, FONT_WIDTH_INDEX, make_number (numeric));
    }
  if (FcPatternGetDouble (p, FC_PIXEL_SIZE, 0, &dbl) == FcResultMatch)
    {
      ASET (entity, FONT_SIZE_INDEX, make_number (dbl));
    }
  else
    ASET (entity, FONT_SIZE_INDEX, make_number (0));
  if (FcPatternGetInteger (p, FC_SPACING, 0, &numeric) == FcResultMatch)
    ASET (entity, FONT_SPACING_INDEX, make_number (numeric));
  if (FcPatternGetDouble (p, FC_DPI, 0, &dbl) == FcResultMatch)
    {
      int dpi = dbl;
      ASET (entity, FONT_DPI_INDEX, make_number (dpi));
    }
  if (FcPatternGetBool (p, FC_SCALABLE, 0, &b) == FcResultMatch
      && b == FcTrue)
    {
      ASET (entity, FONT_SIZE_INDEX, make_number (0));
      ASET (entity, FONT_AVGWIDTH_INDEX, make_number (0));
    }
  else
    {
      /* As this font is not scalable, perhaps this is a BDF or PCF
	 font. */
      FT_Face ft_face;

      ASET (entity, FONT_ADSTYLE_INDEX, get_adstyle_property (p));
      if ((ft_library || FT_Init_FreeType (&ft_library) == 0)
	  && FT_New_Face (ft_library, file, idx, &ft_face) == 0)
	{
	  BDF_PropertyRec rec;

	  if (FT_Get_BDF_Property (ft_face, "AVERAGE_WIDTH", &rec) == 0
	      && rec.type == BDF_PROPERTY_TYPE_INTEGER)
	    ASET (entity, FONT_AVGWIDTH_INDEX, make_number (rec.u.integer));
	  FT_Done_Face (ft_face);
	}
    }

  ASET (entity, FONT_EXTRA_INDEX, Fcopy_sequence (extra));
  font_put_extra (entity, QCfont_entity, key);
  return entity;
}


static Lisp_Object ftfont_generic_family_list;

static Lisp_Object
ftfont_resolve_generic_family (Lisp_Object family, FcPattern *pattern)
{
  Lisp_Object slot;
  FcPattern *match;
  FcResult result;
  FcLangSet *langset;

  family = Fintern (Fdowncase (SYMBOL_NAME (family)), Qnil);
  if (EQ (family, Qmono))
    family = Qmonospace;
  else if (EQ (family, Qsans) || EQ (family, Qsans__serif))
    family = Qsans_serif;
  slot = assq_no_quit (family, ftfont_generic_family_list);
  if (! CONSP (slot))
    return Qnil;
  if (! EQ (XCDR (slot), Qt))
    return XCDR (slot);
  pattern = FcPatternDuplicate (pattern);
  if (! pattern)
    goto err;
  FcPatternDel (pattern, FC_FOUNDRY);
  FcPatternDel (pattern, FC_FAMILY);
  FcPatternAddString (pattern, FC_FAMILY, SYMBOL_FcChar8 (family));
  if (FcPatternGetLangSet (pattern, FC_LANG, 0, &langset) != FcResultMatch)
    {
      /* This is to avoid the effect of locale.  */
      static const FcChar8 lang[] = "en";
      langset = FcLangSetCreate ();
      FcLangSetAdd (langset, lang);
      FcPatternAddLangSet (pattern, FC_LANG, langset);
      FcLangSetDestroy (langset);
    }
  FcConfigSubstitute (NULL, pattern, FcMatchPattern);
  FcDefaultSubstitute (pattern);
  match = FcFontMatch (NULL, pattern, &result);
  if (match)
    {
      FcChar8 *fam;

      if (FcPatternGetString (match, FC_FAMILY, 0, &fam) == FcResultMatch)
	family = intern ((char *) fam);
    }
  else
    family = Qnil;
  XSETCDR (slot, family);
  if (match) FcPatternDestroy (match);
 err:
  if (pattern) FcPatternDestroy (pattern);
  return family;
}

struct ftfont_cache_data
{
  FT_Face ft_face;
  FcCharSet *fc_charset;
};

static Lisp_Object
ftfont_lookup_cache (Lisp_Object key, enum ftfont_cache_for cache_for)
{
  Lisp_Object cache, val, entity;
  struct ftfont_cache_data *cache_data;

  if (FONT_ENTITY_P (key))
    {
      entity = key;
      val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
      xassert (CONSP (val));
      key = XCDR (val);
    }
  else
    entity = Qnil;

  if (NILP (ft_face_cache))
    cache = Qnil;
  else
    cache = Fgethash (key, ft_face_cache, Qnil);
  if (NILP (cache))
    {
      if (NILP (ft_face_cache))
	{
	  Lisp_Object args[2];

	  args[0] = QCtest;
	  args[1] = Qequal;
	  ft_face_cache = Fmake_hash_table (2, args);
	}
      cache_data = xmalloc (sizeof (struct ftfont_cache_data));
      cache_data->ft_face = NULL;
      cache_data->fc_charset = NULL;
      val = make_save_value (NULL, 0);
      XSAVE_VALUE (val)->integer = 0;
      XSAVE_VALUE (val)->pointer = cache_data;
      cache = Fcons (Qnil, val);
      Fputhash (key, cache, ft_face_cache);
    }
  else
    {
      val = XCDR (cache);
      cache_data = XSAVE_VALUE (val)->pointer;
    }

  if (cache_for == FTFONT_CACHE_FOR_ENTITY)
    return cache;

  if (cache_for == FTFONT_CACHE_FOR_FACE
      ? ! cache_data->ft_face : ! cache_data->fc_charset)
    {
      char *filename = SSDATA (XCAR (key));
      int idx = XINT (XCDR (key));

      if (cache_for == FTFONT_CACHE_FOR_FACE)
	{
	  if (! ft_library
	      && FT_Init_FreeType (&ft_library) != 0)
	    return Qnil;
	  if (FT_New_Face (ft_library, filename, idx, &cache_data->ft_face)
	      != 0)
	    return Qnil;
	}
      else
	{
	  FcPattern *pat = NULL;
	  FcFontSet *fontset = NULL;
	  FcObjectSet *objset = NULL;
	  FcCharSet *charset = NULL;

	  pat = FcPatternBuild (0, FC_FILE, FcTypeString, (FcChar8 *) filename,
				FC_INDEX, FcTypeInteger, idx, NULL);
	  if (! pat)
	    goto finish;
	  objset = FcObjectSetBuild (FC_CHARSET, FC_STYLE, NULL);
	  if (! objset)
	    goto finish;
	  fontset = FcFontList (NULL, pat, objset);
	  if (! fontset)
	    goto finish;
	  if (fontset && fontset->nfont > 0
	      && (FcPatternGetCharSet (fontset->fonts[0], FC_CHARSET, 0,
				       &charset)
		  == FcResultMatch))
	    cache_data->fc_charset = FcCharSetCopy (charset);
	  else
	    cache_data->fc_charset = FcCharSetCreate ();

	finish:
	  if (fontset)
	    FcFontSetDestroy (fontset);
	  if (objset)
	    FcObjectSetDestroy (objset);
	  if (pat)
	    FcPatternDestroy (pat);
	}
    }
  return cache;
}

FcCharSet *
ftfont_get_fc_charset (Lisp_Object entity)
{
  Lisp_Object val, cache;
  struct ftfont_cache_data *cache_data;

  cache = ftfont_lookup_cache (entity, FTFONT_CACHE_FOR_CHARSET);
  val = XCDR (cache);
  cache_data = XSAVE_VALUE (val)->pointer;
  return cache_data->fc_charset;
}

#ifdef HAVE_LIBOTF
static OTF *
ftfont_get_otf (struct ftfont_info *ftfont_info)
{
  OTF *otf;

  if (ftfont_info->otf)
    return ftfont_info->otf;
  if (! ftfont_info->maybe_otf)
    return NULL;
  otf = OTF_open_ft_face (ftfont_info->ft_size->face);
  if (! otf || OTF_get_table (otf, "head") < 0)
    {
      if (otf)
	OTF_close (otf);
      ftfont_info->maybe_otf = 0;
      return NULL;
    }
  ftfont_info->otf = otf;
  return otf;
}
#endif	/* HAVE_LIBOTF */

static Lisp_Object ftfont_get_cache (FRAME_PTR);
static Lisp_Object ftfont_list (Lisp_Object, Lisp_Object);
static Lisp_Object ftfont_match (Lisp_Object, Lisp_Object);
static Lisp_Object ftfont_list_family (Lisp_Object);
static Lisp_Object ftfont_open (FRAME_PTR, Lisp_Object, int);
static void ftfont_close (FRAME_PTR, struct font *);
static int ftfont_has_char (Lisp_Object, int);
static unsigned ftfont_encode_char (struct font *, int);
static int ftfont_text_extents (struct font *, unsigned *, int,
                                struct font_metrics *);
static int ftfont_get_bitmap (struct font *, unsigned,
                              struct font_bitmap *, int);
static int ftfont_anchor_point (struct font *, unsigned, int,
                                int *, int *);
#ifdef HAVE_LIBOTF
static Lisp_Object ftfont_otf_capability (struct font *);
# ifdef HAVE_M17N_FLT
static Lisp_Object ftfont_shape (Lisp_Object);
# endif
#endif

#ifdef HAVE_OTF_GET_VARIATION_GLYPHS
static int ftfont_variation_glyphs (struct font *, int c,
                                    unsigned variations[256]);
#endif /* HAVE_OTF_GET_VARIATION_GLYPHS */

struct font_driver ftfont_driver =
  {
    0,				/* Qfreetype */
    0,				/* case insensitive */
    ftfont_get_cache,
    ftfont_list,
    ftfont_match,
    ftfont_list_family,
    NULL,			/* free_entity */
    ftfont_open,
    ftfont_close,
    /* We can't draw a text without device dependent functions.  */
    NULL,			/* prepare_face */
    NULL,			/* done_face */
    ftfont_has_char,
    ftfont_encode_char,
    ftfont_text_extents,
    /* We can't draw a text without device dependent functions.  */
    NULL,			/* draw */
    ftfont_get_bitmap,
    NULL,			/* get_bitmap */
    NULL,			/* free_bitmap */
    NULL,			/* get_outline */
    ftfont_anchor_point,
#ifdef HAVE_LIBOTF
    ftfont_otf_capability,
#else  /* not HAVE_LIBOTF */
    NULL,
#endif	/* not HAVE_LIBOTF */
    NULL,			/* otf_drive */
    NULL,			/* start_for_frame */
    NULL,			/* end_for_frame */
#if defined (HAVE_M17N_FLT) && defined (HAVE_LIBOTF)
    ftfont_shape,
#else  /* not (HAVE_M17N_FLT && HAVE_LIBOTF) */
    NULL,
#endif	/* not (HAVE_M17N_FLT && HAVE_LIBOTF) */
    NULL,			/* check */

#ifdef HAVE_OTF_GET_VARIATION_GLYPHS
    ftfont_variation_glyphs,
#else
    NULL,
#endif

    ftfont_filter_properties, /* filter_properties */
  };

static Lisp_Object
ftfont_get_cache (FRAME_PTR f)
{
  return freetype_font_cache;
}

static int
ftfont_get_charset (Lisp_Object registry)
{
  char *str = SSDATA (SYMBOL_NAME (registry));
  char *re = alloca (SBYTES (SYMBOL_NAME (registry)) * 2 + 1);
  Lisp_Object regexp;
  int i, j;

  for (i = j = 0; i < SBYTES (SYMBOL_NAME (registry)); i++, j++)
    {
      if (str[i] == '.')
	re[j++] = '\\';
      else if (str[i] == '*')
	re[j++] = '.';
      re[j] = str[i];
      if (re[j] == '?')
	re[j] = '.';
    }
  re[j] = '\0';
  regexp = make_unibyte_string (re, j);
  for (i = 0; fc_charset_table[i].name; i++)
    if (fast_c_string_match_ignore_case (regexp, fc_charset_table[i].name) >= 0)
      break;
  if (! fc_charset_table[i].name)
    return -1;
  if (! fc_charset_table[i].fc_charset)
    {
      FcCharSet *charset = FcCharSetCreate ();
      int *uniquifier = fc_charset_table[i].uniquifier;

      if (! charset)
	return -1;
      for (j = 0; uniquifier[j]; j++)
	if (! FcCharSetAddChar (charset, uniquifier[j]))
	  {
	    FcCharSetDestroy (charset);
	    return -1;
	  }
      fc_charset_table[i].fc_charset = charset;
    }
  return i;
}

struct OpenTypeSpec
{
  Lisp_Object script;
  unsigned int script_tag, langsys_tag;
  int nfeatures[2];
  unsigned int *features[2];
};

#define OTF_SYM_TAG(SYM, TAG)					\
  do {								\
    unsigned char *p = SDATA (SYMBOL_NAME (SYM));		\
    TAG = (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];	\
  } while (0)

#define OTF_TAG_STR(TAG, P)			\
  do {						\
    (P)[0] = (char) (TAG >> 24);		\
    (P)[1] = (char) ((TAG >> 16) & 0xFF);	\
    (P)[2] = (char) ((TAG >> 8) & 0xFF);	\
    (P)[3] = (char) (TAG & 0xFF);		\
    (P)[4] = '\0';				\
  } while (0)

#ifdef HAVE_LIBOTF
#define OTF_TAG_SYM(SYM, TAG)			\
  do {						\
    char str[5];				\
    						\
    OTF_TAG_STR (TAG, str);			\
    (SYM) = font_intern_prop (str, 4, 1);	\
  } while (0)
#endif


static struct OpenTypeSpec *
ftfont_get_open_type_spec (Lisp_Object otf_spec)
{
  struct OpenTypeSpec *spec = malloc (sizeof (struct OpenTypeSpec));
  Lisp_Object val;
  int i, j, negative;

  if (! spec)
    return NULL;
  spec->script = XCAR (otf_spec);
  if (! NILP (spec->script))
    {
      OTF_SYM_TAG (spec->script, spec->script_tag);
      val = assq_no_quit (spec->script, Votf_script_alist);
      if (CONSP (val) && SYMBOLP (XCDR (val)))
	spec->script = XCDR (val);
      else
	spec->script = Qnil;
    }
  else
    spec->script_tag = 0x44464C54; 	/* "DFLT" */
  otf_spec = XCDR (otf_spec);
  spec->langsys_tag = 0;
  if (! NILP (otf_spec))
    {
      val = XCAR (otf_spec);
      if (! NILP (val))
	OTF_SYM_TAG (val, spec->langsys_tag);
      otf_spec = XCDR (otf_spec);
    }
  spec->nfeatures[0] = spec->nfeatures[1] = 0;
  for (i = 0; i < 2 && ! NILP (otf_spec); i++, otf_spec = XCDR (otf_spec))
    {
      Lisp_Object len;

      val = XCAR (otf_spec);
      if (NILP (val))
	continue;
      len = Flength (val);
      spec->features[i] =
	(min (PTRDIFF_MAX, SIZE_MAX) / sizeof (int) < XINT (len)
	 ? 0
	 : malloc (sizeof (int) * XINT (len)));
      if (! spec->features[i])
	{
	  if (i > 0 && spec->features[0])
	    free (spec->features[0]);
	  free (spec);
	  return NULL;
	}
      for (j = 0, negative = 0; CONSP (val); val = XCDR (val))
	{
	  if (NILP (XCAR (val)))
	    negative = 1;
	  else
	    {
	      unsigned int tag;

	      OTF_SYM_TAG (XCAR (val), tag);
	      spec->features[i][j++] = negative ? tag & 0x80000000 : tag;
	    }
	}
      spec->nfeatures[i] = j;
    }
  return spec;
}

static FcPattern *
ftfont_spec_pattern (Lisp_Object spec, char *otlayout, struct OpenTypeSpec **otspec, const char **langname)
{
  Lisp_Object tmp, extra;
  FcPattern *pattern = NULL;
  FcCharSet *charset = NULL;
  FcLangSet *langset = NULL;
  int n;
  int dpi = -1;
  int scalable = -1;
  Lisp_Object script = Qnil;
  Lisp_Object registry;
  int fc_charset_idx;

  if ((n = FONT_SLANT_NUMERIC (spec)) >= 0
      && n < 100)
    /* Fontconfig doesn't support reverse-italic/oblique.  */
    return NULL;

  if (INTEGERP (AREF (spec, FONT_DPI_INDEX)))
    dpi = XINT (AREF (spec, FONT_DPI_INDEX));
  if (INTEGERP (AREF (spec, FONT_AVGWIDTH_INDEX))
      && XINT (AREF (spec, FONT_AVGWIDTH_INDEX)) == 0)
    scalable = 1;

  registry = AREF (spec, FONT_REGISTRY_INDEX);
  if (NILP (registry)
      || EQ (registry, Qascii_0)
      || EQ (registry, Qiso10646_1)
      || EQ (registry, Qunicode_bmp))
    fc_charset_idx = -1;
  else
    {
      FcChar8 *lang;

      fc_charset_idx = ftfont_get_charset (registry);
      if (fc_charset_idx < 0)
	return NULL;
      charset = fc_charset_table[fc_charset_idx].fc_charset;
      *langname = fc_charset_table[fc_charset_idx].lang;
      lang = (FcChar8 *) *langname;
      if (lang)
	{
	  langset = FcLangSetCreate ();
	  if (! langset)
	    goto err;
	  FcLangSetAdd (langset, lang);
	}
    }

  otlayout[0] = '\0';
  for (extra = AREF (spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      Lisp_Object key, val;

      key = XCAR (XCAR (extra)), val = XCDR (XCAR (extra));
      if (EQ (key, QCdpi))
	{
	  if (INTEGERP (val))
	    dpi = XINT (val);
	}
      else if (EQ (key, QClang))
	{
	  if (! langset)
	    langset = FcLangSetCreate ();
	  if (! langset)
	    goto err;
	  if (SYMBOLP (val))
	    {
	      if (! FcLangSetAdd (langset, SYMBOL_FcChar8 (val)))
		goto err;
	    }
	  else
	    for (; CONSP (val); val = XCDR (val))
	      if (SYMBOLP (XCAR (val))
		  && ! FcLangSetAdd (langset, SYMBOL_FcChar8 (XCAR (val))))
		goto err;
	}
      else if (EQ (key, QCotf))
	{
	  if (CONSP (val))
	    {
	      *otspec = ftfont_get_open_type_spec (val);
	      if (! *otspec)
		return NULL;
	      strcat (otlayout, "otlayout:");
	      OTF_TAG_STR ((*otspec)->script_tag, otlayout + 9);
	      script = (*otspec)->script;
	    }
	}
      else if (EQ (key, QCscript))
	script = val;
      else if (EQ (key, QCscalable))
	scalable = ! NILP (val);
    }

  if (! NILP (script) && ! charset)
    {
      Lisp_Object chars = assq_no_quit (script, Vscript_representative_chars);

      if (CONSP (chars) && CONSP (CDR (chars)))
	{
	  charset = FcCharSetCreate ();
	  if (! charset)
	    goto err;
	  for (chars = XCDR (chars); CONSP (chars); chars = XCDR (chars))
	    if (CHARACTERP (XCAR (chars))
		&& ! FcCharSetAddChar (charset, XFASTINT (XCAR (chars))))
	      goto err;
	}
    }

  pattern = FcPatternCreate ();
  if (! pattern)
    goto err;
  tmp = AREF (spec, FONT_FOUNDRY_INDEX);
  if (! NILP (tmp)
      && ! FcPatternAddString (pattern, FC_FOUNDRY, SYMBOL_FcChar8 (tmp)))
    goto err;
  tmp = AREF (spec, FONT_FAMILY_INDEX);
  if (! NILP (tmp)
      && ! FcPatternAddString (pattern, FC_FAMILY, SYMBOL_FcChar8 (tmp)))
    goto err;
  if (charset
      && ! FcPatternAddCharSet (pattern, FC_CHARSET, charset))
    goto err;
  if (langset
      && ! FcPatternAddLangSet (pattern, FC_LANG, langset))
    goto err;
  if (dpi >= 0
      && ! FcPatternAddDouble (pattern, FC_DPI, dpi))
    goto err;
  if (scalable >= 0
      && ! FcPatternAddBool (pattern, FC_SCALABLE, scalable ? FcTrue : FcFalse))
    goto err;

  goto finish;

 err:
  /* We come here because of unexpected error in fontconfig API call
     (usually insufficient memory).  */
  if (pattern)
    {
      FcPatternDestroy (pattern);
      pattern = NULL;
    }
  if (*otspec)
    {
      if ((*otspec)->nfeatures[0] > 0)
	free ((*otspec)->features[0]);
      if ((*otspec)->nfeatures[1] > 0)
	free ((*otspec)->features[1]);
      free (*otspec);
      *otspec = NULL;
    }

 finish:
  if (langset) FcLangSetDestroy (langset);
  if (charset && fc_charset_idx < 0) FcCharSetDestroy (charset);
  return pattern;
}

static Lisp_Object
ftfont_list (Lisp_Object frame, Lisp_Object spec)
{
  Lisp_Object val = Qnil, family, adstyle;
  int i;
  FcPattern *pattern;
  FcFontSet *fontset = NULL;
  FcObjectSet *objset = NULL;
  FcCharSet *charset;
  Lisp_Object chars = Qnil;
  char otlayout[15];		/* For "otlayout:XXXX" */
  struct OpenTypeSpec *otspec = NULL;
  int spacing = -1;
  const char *langname = NULL;

  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  pattern = ftfont_spec_pattern (spec, otlayout, &otspec, &langname);
  if (! pattern)
    return Qnil;
  if (FcPatternGetCharSet (pattern, FC_CHARSET, 0, &charset) != FcResultMatch)
    {
      val = assq_no_quit (QCscript, AREF (spec, FONT_EXTRA_INDEX));
      if (! NILP (val))
	{
	  val = assq_no_quit (XCDR (val), Vscript_representative_chars);
	  if (CONSP (val) && VECTORP (XCDR (val)))
	    chars = XCDR (val);
	}
      val = Qnil;
    }
  if (INTEGERP (AREF (spec, FONT_SPACING_INDEX)))
    spacing = XINT (AREF (spec, FONT_SPACING_INDEX));
  family = AREF (spec, FONT_FAMILY_INDEX);
  if (! NILP (family))
    {
      Lisp_Object resolved;

      resolved = ftfont_resolve_generic_family (family, pattern);
      if (! NILP (resolved))
	{
	  FcPatternDel (pattern, FC_FAMILY);
	  if (! FcPatternAddString (pattern, FC_FAMILY,
				    SYMBOL_FcChar8 (resolved)))
	    goto err;
	}
    }
  adstyle = AREF (spec, FONT_ADSTYLE_INDEX);
  if (! NILP (adstyle) && SBYTES (SYMBOL_NAME (adstyle)) == 0)
    adstyle = Qnil;
  objset = FcObjectSetBuild (FC_FOUNDRY, FC_FAMILY, FC_WEIGHT, FC_SLANT,
			     FC_WIDTH, FC_PIXEL_SIZE, FC_SPACING, FC_SCALABLE,
			     FC_STYLE, FC_FILE, FC_INDEX,
#ifdef FC_CAPABILITY
			     FC_CAPABILITY,
#endif	/* FC_CAPABILITY */
#ifdef FC_FONTFORMAT
			     FC_FONTFORMAT,
#endif
			     NULL);
  if (! objset)
    goto err;
  if (! NILP (chars))
    FcObjectSetAdd (objset, FC_CHARSET);

  fontset = FcFontList (NULL, pattern, objset);
  if (! fontset || fontset->nfont == 0)
    goto finish;
#if 0
  /* Need fix because this finds any fonts.  */
  if (fontset->nfont == 0 && ! NILP (family))
    {
      /* Try matching with configuration.  For instance, the
	 configuration may specify "Nimbus Mono L" as an alias of
	 "Courier".  */
      FcPattern *pat = FcPatternBuild (0, FC_FAMILY, FcTypeString,
				       SYMBOL_FcChar8 (family), NULL);
      FcChar8 *fam;

      if (FcConfigSubstitute (NULL, pat, FcMatchPattern) == FcTrue)
	{
	  for (i = 0;
	       FcPatternGetString (pat, FC_FAMILY, i, &fam) == FcResultMatch;
	       i++)
	    {
	      FcPatternDel (pattern, FC_FAMILY);
	      FcPatternAddString (pattern, FC_FAMILY, fam);
	      FcFontSetDestroy (fontset);
	      fontset = FcFontList (NULL, pattern, objset);
	      if (fontset && fontset->nfont > 0)
		break;
	    }
	}
    }
#endif
  for (i = 0; i < fontset->nfont; i++)
    {
      Lisp_Object entity;

      if (spacing >= 0)
	{
	  int this;

	  if ((FcPatternGetInteger (fontset->fonts[i], FC_SPACING, 0, &this)
	       == FcResultMatch)
	      && spacing != this)
	    continue;
	}

#ifdef FC_CAPABILITY
      if (otlayout[0])
	{
	  FcChar8 *this;

	  if (FcPatternGetString (fontset->fonts[i], FC_CAPABILITY, 0, &this)
	      != FcResultMatch
	      || ! strstr ((char *) this, otlayout))
	    continue;
	}
#endif	/* FC_CAPABILITY */
#ifdef HAVE_LIBOTF
      if (otspec)
	{
	  FcChar8 *file;
	  OTF *otf;

	  if (FcPatternGetString (fontset->fonts[i], FC_FILE, 0, &file)
	      != FcResultMatch)
	    continue;
	  otf = OTF_open ((char *) file);
	  if (! otf)
	    continue;
	  if (OTF_check_features (otf, 1,
				  otspec->script_tag, otspec->langsys_tag,
				  otspec->features[0],
				  otspec->nfeatures[0]) != 1
	      || OTF_check_features (otf, 0,
				     otspec->script_tag, otspec->langsys_tag,
				     otspec->features[1],
				     otspec->nfeatures[1]) != 1)
	    continue;
	}
#endif	/* HAVE_LIBOTF */
      if (VECTORP (chars))
	{
	  int j;

	  if (FcPatternGetCharSet (fontset->fonts[i], FC_CHARSET, 0, &charset)
	      != FcResultMatch)
	    continue;
	  for (j = 0; j < ASIZE (chars); j++)
	    if (NATNUMP (AREF (chars, j))
		&& FcCharSetHasChar (charset, XFASTINT (AREF (chars, j))))
	      break;
	  if (j == ASIZE (chars))
	    continue;
	}
      if (! NILP (adstyle) || langname)
	{
	  Lisp_Object this_adstyle = get_adstyle_property (fontset->fonts[i]);

	  if (! NILP (adstyle)
	      && (NILP (this_adstyle)
		  || xstrcasecmp (SSDATA (SYMBOL_NAME (adstyle)),
				  SSDATA (SYMBOL_NAME (this_adstyle))) != 0))
	    continue;
	  if (langname
	      && ! NILP (this_adstyle)
	      && xstrcasecmp (langname, SSDATA (SYMBOL_NAME (this_adstyle))))
	    continue;
	}
      entity = ftfont_pattern_entity (fontset->fonts[i],
				      AREF (spec, FONT_EXTRA_INDEX));
      if (! NILP (entity))
	val = Fcons (entity, val);
    }
  val = Fnreverse (val);
  goto finish;

 err:
  /* We come here because of unexpected error in fontconfig API call
     (usually insufficient memory).  */
  val = Qnil;

 finish:
  FONT_ADD_LOG ("ftfont-list", spec, val);
  if (objset) FcObjectSetDestroy (objset);
  if (fontset) FcFontSetDestroy (fontset);
  if (pattern) FcPatternDestroy (pattern);
  return val;
}

static Lisp_Object
ftfont_match (Lisp_Object frame, Lisp_Object spec)
{
  Lisp_Object entity = Qnil;
  FcPattern *pattern, *match = NULL;
  FcResult result;
  char otlayout[15];		/* For "otlayout:XXXX" */
  struct OpenTypeSpec *otspec = NULL;
  const char *langname = NULL;

  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  pattern = ftfont_spec_pattern (spec, otlayout, &otspec, &langname);
  if (! pattern)
    return Qnil;

  if (INTEGERP (AREF (spec, FONT_SIZE_INDEX)))
    {
      FcValue value;

      value.type = FcTypeDouble;
      value.u.d = XINT (AREF (spec, FONT_SIZE_INDEX));
      FcPatternAdd (pattern, FC_PIXEL_SIZE, value, FcFalse);
    }
  if (FcConfigSubstitute (NULL, pattern, FcMatchPattern) == FcTrue)
    {
      FcDefaultSubstitute (pattern);
      match = FcFontMatch (NULL, pattern, &result);
      if (match)
	{
	  entity = ftfont_pattern_entity (match, AREF (spec, FONT_EXTRA_INDEX));
	  FcPatternDestroy (match);
	  if (! NILP (AREF (spec, FONT_FAMILY_INDEX))
	      && NILP (assq_no_quit (AREF (spec, FONT_FAMILY_INDEX),
				     ftfont_generic_family_list))
	      && NILP (Fstring_equal (AREF (spec, FONT_FAMILY_INDEX),
				      AREF (entity, FONT_FAMILY_INDEX))))
	    entity = Qnil;
	}
    }
  FcPatternDestroy (pattern);

  FONT_ADD_LOG ("ftfont-match", spec, entity);
  return entity;
}

static Lisp_Object
ftfont_list_family (Lisp_Object frame)
{
  Lisp_Object list = Qnil;
  FcPattern *pattern = NULL;
  FcFontSet *fontset = NULL;
  FcObjectSet *objset = NULL;
  int i;

  if (! fc_initialized)
    {
      FcInit ();
      fc_initialized = 1;
    }

  pattern = FcPatternCreate ();
  if (! pattern)
    goto finish;
  objset = FcObjectSetBuild (FC_FAMILY, NULL);
  if (! objset)
    goto finish;
  fontset = FcFontList (NULL, pattern, objset);
  if (! fontset)
    goto finish;

  for (i = 0; i < fontset->nfont; i++)
    {
      FcPattern *pat = fontset->fonts[i];
      FcChar8 *str;

      if (FcPatternGetString (pat, FC_FAMILY, 0, &str) == FcResultMatch)
	list = Fcons (intern ((char *) str), list);
    }

 finish:
  if (objset) FcObjectSetDestroy (objset);
  if (fontset) FcFontSetDestroy (fontset);
  if (pattern) FcPatternDestroy (pattern);

  return list;
}


static Lisp_Object
ftfont_open (FRAME_PTR f, Lisp_Object entity, int pixel_size)
{
  struct ftfont_info *ftfont_info;
  struct font *font;
  struct ftfont_cache_data *cache_data;
  FT_Face ft_face;
  FT_Size ft_size;
  FT_UInt size;
  Lisp_Object val, filename, idx, cache, font_object;
  int scalable;
  int spacing;
  char name[256];
  int i, len;
  int upEM;

  val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
  if (! CONSP (val))
    return Qnil;
  val = XCDR (val);
  cache = ftfont_lookup_cache (entity, FTFONT_CACHE_FOR_FACE);
  if (NILP (cache))
    return Qnil;
  filename = XCAR (val);
  idx = XCDR (val);
  val = XCDR (cache);
  cache_data = XSAVE_VALUE (XCDR (cache))->pointer;
  ft_face = cache_data->ft_face;
  if (XSAVE_VALUE (val)->integer > 0)
    {
      /* FT_Face in this cache is already used by the different size.  */
      if (FT_New_Size (ft_face, &ft_size) != 0)
	return Qnil;
      if (FT_Activate_Size (ft_size) != 0)
	{
	  FT_Done_Size (ft_size);
	  return Qnil;
	}
    }
  XSAVE_VALUE (val)->integer++;
  size = XINT (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;
  if (FT_Set_Pixel_Sizes (ft_face, size, size) != 0)
    {
      if (XSAVE_VALUE (val)->integer == 0)
	FT_Done_Face (ft_face);
      return Qnil;
    }

  font_object = font_make_object (VECSIZE (struct ftfont_info), entity, size);
  ASET (font_object, FONT_TYPE_INDEX, Qfreetype);
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
  ASET (font_object, FONT_FORMAT_INDEX, ftfont_font_format (NULL, filename));
  font = XFONT_OBJECT (font_object);
  ftfont_info = (struct ftfont_info *) font;
  ftfont_info->ft_size = ft_face->size;
  ftfont_info->index = XINT (idx);
#ifdef HAVE_LIBOTF
  ftfont_info->maybe_otf = ft_face->face_flags & FT_FACE_FLAG_SFNT;
  ftfont_info->otf = NULL;
#endif	/* HAVE_LIBOTF */
  /* This means that there's no need of transformation.  */
  ftfont_info->matrix.xx = 0;
  font->pixel_size = size;
  font->driver = &ftfont_driver;
  font->encoding_charset = font->repertory_charset = -1;

  upEM = ft_face->units_per_EM;
  scalable = (INTEGERP (AREF (entity, FONT_AVGWIDTH_INDEX))
	      && XINT (AREF (entity, FONT_AVGWIDTH_INDEX)) == 0);
  if (scalable)
    {
      font->ascent = ft_face->ascender * size / upEM;
      font->descent = - ft_face->descender * size / upEM;
      font->height = ft_face->height * size / upEM;
    }
  else
    {
      font->ascent = ft_face->size->metrics.ascender >> 6;
      font->descent = - ft_face->size->metrics.descender >> 6;
      font->height = ft_face->size->metrics.height >> 6;
    }
  if (INTEGERP (AREF (entity, FONT_SPACING_INDEX)))
    spacing = XINT (AREF (entity, FONT_SPACING_INDEX));
  else
    spacing = FC_PROPORTIONAL;
  if (spacing != FC_PROPORTIONAL
#ifdef FC_DUAL
      && spacing != FC_DUAL
#endif	/* FC_DUAL */
      )
    font->min_width = font->average_width = font->space_width
      = (scalable ? ft_face->max_advance_width * size / upEM
	 : ft_face->size->metrics.max_advance >> 6);
  else
    {
      int n;

      font->min_width = font->average_width = font->space_width = 0;
      for (i = 32, n = 0; i < 127; i++)
	if (FT_Load_Char (ft_face, i, FT_LOAD_DEFAULT) == 0)
	  {
	    int this_width = ft_face->glyph->metrics.horiAdvance >> 6;

	    if (this_width > 0
		&& (! font->min_width || font->min_width > this_width))
	      font->min_width = this_width;
	    if (i == 32)
	      font->space_width = this_width;
	    font->average_width += this_width;
	    n++;
	  }
      if (n > 0)
	font->average_width /= n;
    }

  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->default_ascent = 0;
  font->vertical_centering = 0;
  if (scalable)
    {
      font->underline_position = -ft_face->underline_position * size / upEM;
      font->underline_thickness = ft_face->underline_thickness * size / upEM;
    }
  else
    {
      font->underline_position = -1;
      font->underline_thickness = 0;
    }

  return font_object;
}

static void
ftfont_close (FRAME_PTR f, struct font *font)
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  Lisp_Object val, cache;

  val = Fcons (font->props[FONT_FILE_INDEX], make_number (ftfont_info->index));
  cache = ftfont_lookup_cache (val, FTFONT_CACHE_FOR_FACE);
  xassert (CONSP (cache));
  val = XCDR (cache);
  (XSAVE_VALUE (val)->integer)--;
  if (XSAVE_VALUE (val)->integer == 0)
    {
      struct ftfont_cache_data *cache_data = XSAVE_VALUE (val)->pointer;

      FT_Done_Face (cache_data->ft_face);
#ifdef HAVE_LIBOTF
      if (ftfont_info->otf)
	OTF_close (ftfont_info->otf);
#endif
      cache_data->ft_face = NULL;
    }
  else
    FT_Done_Size (ftfont_info->ft_size);
}

static int
ftfont_has_char (Lisp_Object font, int c)
{
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
    {
      FcCharSet *charset = ftfont_get_fc_charset (font);

      return (FcCharSetHasChar (charset, c) == FcTrue);
    }
  else
    {
      struct ftfont_info *ftfont_info;

      ftfont_info = (struct ftfont_info *) XFONT_OBJECT (font);
      return (FT_Get_Char_Index (ftfont_info->ft_size->face, (FT_ULong) c)
	      != 0);
    }
}

static unsigned
ftfont_encode_char (struct font *font, int c)
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;
  FT_ULong charcode = c;
  FT_UInt code = FT_Get_Char_Index (ft_face, charcode);

  return (code > 0 ? code : FONT_INVALID_CODE);
}

static int
ftfont_text_extents (struct font *font, unsigned int *code, int nglyphs, struct font_metrics *metrics)
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;
  int width = 0;
  int i, first;

  if (ftfont_info->ft_size != ft_face->size)
    FT_Activate_Size (ftfont_info->ft_size);
  if (metrics)
    memset (metrics, 0, sizeof (struct font_metrics));
  for (i = 0, first = 1; i < nglyphs; i++)
    {
      if (FT_Load_Glyph (ft_face, code[i], FT_LOAD_DEFAULT) == 0)
	{
	  FT_Glyph_Metrics *m = &ft_face->glyph->metrics;

	  if (first)
	    {
	      if (metrics)
		{
		  metrics->lbearing = m->horiBearingX >> 6;
		  metrics->rbearing = (m->horiBearingX + m->width) >> 6;
		  metrics->ascent = m->horiBearingY >> 6;
		  metrics->descent = (m->height - m->horiBearingY) >> 6;
		}
	      first = 0;
	    }
	  if (metrics)
	    {
	      if (metrics->lbearing > width + (m->horiBearingX >> 6))
		metrics->lbearing = width + (m->horiBearingX >> 6);
	      if (metrics->rbearing
		  < width + ((m->horiBearingX + m->width) >> 6))
		metrics->rbearing
		  = width + ((m->horiBearingX + m->width) >> 6);
	      if (metrics->ascent < (m->horiBearingY >> 6))
		metrics->ascent = m->horiBearingY >> 6;
	      if (metrics->descent > ((m->height - m->horiBearingY) >> 6))
		metrics->descent = (m->height - m->horiBearingY) >> 6;
	    }
	  width += m->horiAdvance >> 6;
	}
      else
	{
	  width += font->space_width;
	}
    }
  if (metrics)
    metrics->width = width;

  return width;
}

static int
ftfont_get_bitmap (struct font *font, unsigned int code, struct font_bitmap *bitmap, int bits_per_pixel)
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;
  FT_Int32 load_flags = FT_LOAD_RENDER;

  if (ftfont_info->ft_size != ft_face->size)
    FT_Activate_Size (ftfont_info->ft_size);
  if (bits_per_pixel == 1)
    {
#ifdef FT_LOAD_TARGET_MONO
      load_flags |= FT_LOAD_TARGET_MONO;
#else
      load_flags |= FT_LOAD_MONOCHROME;
#endif
    }
  else if (bits_per_pixel != 8)
    /* We don't support such a rendering.  */
    return -1;

  if (FT_Load_Glyph (ft_face, code, load_flags) != 0)
    return -1;
  bitmap->bits_per_pixel
    = (ft_face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_MONO ? 1
       : ft_face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_GRAY ? 8
       : ft_face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_LCD ? 8
       : ft_face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_LCD_V ? 8
       : -1);
  if (bitmap->bits_per_pixel < 0)
    /* We don't support that kind of pixel mode.  */
    return -1;
  bitmap->rows = ft_face->glyph->bitmap.rows;
  bitmap->width = ft_face->glyph->bitmap.width;
  bitmap->pitch = ft_face->glyph->bitmap.pitch;
  bitmap->buffer = ft_face->glyph->bitmap.buffer;
  bitmap->left = ft_face->glyph->bitmap_left;
  bitmap->top = ft_face->glyph->bitmap_top;
  bitmap->advance = ft_face->glyph->metrics.horiAdvance >> 6;
  bitmap->extra = NULL;

  return 0;
}

static int
ftfont_anchor_point (struct font *font, unsigned int code, int idx,
		     int *x, int *y)
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  FT_Face ft_face = ftfont_info->ft_size->face;

  if (ftfont_info->ft_size != ft_face->size)
    FT_Activate_Size (ftfont_info->ft_size);
  if (FT_Load_Glyph (ft_face, code, FT_LOAD_DEFAULT) != 0)
    return -1;
  if (ft_face->glyph->format != FT_GLYPH_FORMAT_OUTLINE)
    return -1;
  if (idx >= ft_face->glyph->outline.n_points)
    return -1;
  *x = ft_face->glyph->outline.points[idx].x;
  *y = ft_face->glyph->outline.points[idx].y;
  return 0;
}

#ifdef HAVE_LIBOTF

static Lisp_Object
ftfont_otf_features (OTF_GSUB_GPOS *gsub_gpos)
{
  Lisp_Object scripts, langsyses, features, sym;
  int i, j, k, l;

  for (scripts = Qnil, i = gsub_gpos->ScriptList.ScriptCount - 1; i >= 0; i--)
    {
      OTF_Script *otf_script = gsub_gpos->ScriptList.Script + i;

      for (langsyses = Qnil, j = otf_script->LangSysCount - 1; j >= -1; j--)
	{
	  OTF_LangSys *otf_langsys;

	  if (j >= 0)
	    otf_langsys = otf_script->LangSys + j;
	  else if (otf_script->DefaultLangSysOffset)
	    otf_langsys = &otf_script->DefaultLangSys;
	  else
	    break;

	  for (features = Qnil, k = otf_langsys->FeatureCount - 1; k >= 0; k--)
	    {
	      l = otf_langsys->FeatureIndex[k];
	      if (l >= gsub_gpos->FeatureList.FeatureCount)
		continue;
	      OTF_TAG_SYM (sym, gsub_gpos->FeatureList.Feature[l].FeatureTag);
	      features = Fcons (sym, features);
	    }
	  if (j >= 0)
	    OTF_TAG_SYM (sym, otf_script->LangSysRecord[j].LangSysTag);
	  else
	    sym = Qnil;
	  langsyses = Fcons (Fcons (sym, features), langsyses);
	}

      OTF_TAG_SYM (sym, gsub_gpos->ScriptList.Script[i].ScriptTag);
      scripts = Fcons (Fcons (sym, langsyses), scripts);
    }
  return scripts;

}


static Lisp_Object
ftfont_otf_capability (struct font *font)
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  OTF *otf = ftfont_get_otf (ftfont_info);
  Lisp_Object gsub_gpos;

  if (! otf)
    return Qnil;
  gsub_gpos = Fcons (Qnil, Qnil);
  if (OTF_get_table (otf, "GSUB") == 0
      && otf->gsub->FeatureList.FeatureCount > 0)
    XSETCAR (gsub_gpos, ftfont_otf_features (otf->gsub));
  if (OTF_get_table (otf, "GPOS") == 0
      && otf->gpos->FeatureList.FeatureCount > 0)
    XSETCDR (gsub_gpos, ftfont_otf_features (otf->gpos));
  return gsub_gpos;
}

#ifdef HAVE_M17N_FLT

#if (((LIBOTF_MAJOR_VERSION > 1) || (LIBOTF_RELEASE_NUMBER >= 10))	\
     && ((M17NLIB_MAJOR_VERSION > 1) || (M17NLIB_MINOR_VERSION >= 6)))
/* We can use the new feature of libotf and m17n-flt to handle the
   character encoding scheme introduced in Unicode 5.1 and 5.2 for
   some Agian scripts.  */
#define M17N_FLT_USE_NEW_FEATURE
#endif

struct MFLTFontFT
{
  MFLTFont flt_font;
  struct font *font;
  FT_Face ft_face;
  OTF *otf;
  FT_Matrix *matrix;
};

static int
ftfont_get_glyph_id (MFLTFont *font, MFLTGlyphString *gstring,
		     int from, int to)
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  FT_Face ft_face = flt_font_ft->ft_face;
  MFLTGlyph *g;

  for (g = gstring->glyphs + from; from < to; g++, from++)
    if (! g->encoded)
      {
	FT_UInt code = FT_Get_Char_Index (ft_face, g->code);

	g->code = code > 0 ? code : FONT_INVALID_CODE;
	g->encoded = 1;
      }
  return 0;
}

/* Operators for 26.6 fixed fractional pixel format */

#define FLOOR(x)    ((x) & -64)
#define CEIL(x)	    (((x)+63) & -64)
#define ROUND(x)    (((x)+32) & -64)

static int
ftfont_get_metrics (MFLTFont *font, MFLTGlyphString *gstring,
		    int from, int to)
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  FT_Face ft_face = flt_font_ft->ft_face;
  MFLTGlyph *g;

  for (g = gstring->glyphs + from; from < to; g++, from++)
    if (! g->measured)
      {
	if (g->code != FONT_INVALID_CODE)
	  {
	    FT_Glyph_Metrics *m;

	    if (FT_Load_Glyph (ft_face, g->code, FT_LOAD_DEFAULT) != 0)
	      abort ();
	    m = &ft_face->glyph->metrics;
	    if (flt_font_ft->matrix)
	      {
		FT_Vector v[4];
		int i;

		v[0].x = v[1].x = m->horiBearingX;
		v[2].x = v[3].x = m->horiBearingX + m->width;
		v[0].y = v[2].y = m->horiBearingY;
		v[1].y = v[3].y = m->horiBearingY - m->height;
		for (i = 0; i < 4; i++)
		  FT_Vector_Transform (v + i, flt_font_ft->matrix);
		g->lbearing = v[0].x < v[1].x ? FLOOR (v[0].x) : FLOOR (v[1].x);
		g->rbearing = v[2].x > v[3].x ? CEIL (v[2].x) : CEIL (v[3].x);
		g->ascent = v[0].y > v[2].y ? CEIL (v[0].y) : CEIL (v[2].y);
		g->descent = v[1].y < v[3].y ? - FLOOR (v[1].y) : - FLOOR (v[3].y);
	      }
	    else
	      {
		g->lbearing = FLOOR (m->horiBearingX);
		g->rbearing = CEIL (m->horiBearingX + m->width);
		g->ascent = CEIL (m->horiBearingY);
		g->descent = - FLOOR (m->horiBearingY - m->height);
	      }
	    g->xadv = ROUND (ft_face->glyph->advance.x);
	  }
	else
	  {
	    g->lbearing = 0;
	    g->rbearing = g->xadv = flt_font_ft->font->space_width << 6;
	    g->ascent = flt_font_ft->font->ascent << 6;
	    g->descent = flt_font_ft->font->descent << 6;
	  }
	g->yadv = 0;
	g->measured = 1;
      }
  return 0;
}

static int
ftfont_check_otf (MFLTFont *font, MFLTOtfSpec *spec)
{
#define FEATURE_NONE(IDX) (! spec->features[IDX])

#define FEATURE_ANY(IDX)	\
  (spec->features[IDX]		\
   && spec->features[IDX][0] == 0xFFFFFFFF && spec->features[IDX][1] == 0)

  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  OTF *otf = flt_font_ft->otf;
  OTF_Tag *tags;
  int i, n, negative;

  if (FEATURE_ANY (0) && FEATURE_ANY (1))
    /* Return 1 iff any of GSUB or GPOS support the script (and language).  */
    return (otf
	    && (OTF_check_features (otf, 0, spec->script, spec->langsys,
				    NULL, 0) > 0
		|| OTF_check_features (otf, 1, spec->script, spec->langsys,
				       NULL, 0) > 0));

  for (i = 0; i < 2; i++)
    if (! FEATURE_ANY (i))
      {
	if (FEATURE_NONE (i))
	  {
	    if (otf
		&& OTF_check_features (otf, i == 0, spec->script, spec->langsys,
				       NULL, 0) > 0)
	      return 0;
	    continue;
	  }
	if (spec->features[i][0] == 0xFFFFFFFF)
	  {
	    if (! otf
		|| OTF_check_features (otf, i == 0, spec->script, spec->langsys,
				       NULL, 0) <= 0)
	      continue;
	  }
	else if (! otf)
	  return 0;
	for (n = 1; spec->features[i][n]; n++);
	tags = alloca (sizeof (OTF_Tag) * n);
	for (n = 0, negative = 0; spec->features[i][n]; n++)
	  {
	    if (spec->features[i][n] == 0xFFFFFFFF)
	      negative = 1;
	    else if (negative)
	      tags[n - 1] = spec->features[i][n] | 0x80000000;
	    else
	      tags[n] = spec->features[i][n];
	  }
#ifdef M17N_FLT_USE_NEW_FEATURE
	if (OTF_check_features (otf, i == 0, spec->script, spec->langsys,
				tags, n - negative) != 1)
	  return 0;
#else  /* not M17N_FLT_USE_NEW_FEATURE */
	if (n - negative > 0
	    && OTF_check_features (otf, i == 0, spec->script, spec->langsys,
				   tags, n - negative) != 1)
	  return 0;
#endif	/* not M17N_FLT_USE_NEW_FEATURE */
      }
  return 1;
#undef FEATURE_NONE
#undef FEATURE_ANY
}

#define DEVICE_DELTA(table, size)				\
  (((size) >= (table).StartSize && (size) <= (table).EndSize)	\
   ? (table).DeltaValue[(size) - (table).StartSize] << 6	\
   : 0)

static void
adjust_anchor (FT_Face ft_face, OTF_Anchor *anchor,
	       unsigned code, int x_ppem, int y_ppem, int *x, int *y)
{
  if (anchor->AnchorFormat == 2)
    {
      FT_Outline *outline;
      int ap = anchor->f.f1.AnchorPoint;

      FT_Load_Glyph (ft_face, (FT_UInt) code, FT_LOAD_MONOCHROME);
      outline = &ft_face->glyph->outline;
      if (ap < outline->n_points)
	{
	  *x = outline->points[ap].x << 6;
	  *y = outline->points[ap].y << 6;
	}
    }
  else if (anchor->AnchorFormat == 3)
    {
      if (anchor->f.f2.XDeviceTable.offset
	  && anchor->f.f2.XDeviceTable.DeltaValue)
	*x += DEVICE_DELTA (anchor->f.f2.XDeviceTable, x_ppem);
      if (anchor->f.f2.YDeviceTable.offset
	  && anchor->f.f2.YDeviceTable.DeltaValue)
	*y += DEVICE_DELTA (anchor->f.f2.YDeviceTable, y_ppem);
    }
}

static OTF_GlyphString otf_gstring;

static void
setup_otf_gstring (int size)
{
  if (otf_gstring.size < size)
    {
      otf_gstring.glyphs = xnrealloc (otf_gstring.glyphs,
				      size, sizeof (OTF_Glyph));
      otf_gstring.size = size;
    }
  otf_gstring.used = size;
  memset (otf_gstring.glyphs, 0, sizeof (OTF_Glyph) * size);
}

#ifdef M17N_FLT_USE_NEW_FEATURE

/* Pack 32-bit OTF tag (0x7F7F7F7F) into 28-bit (0x0FFFFFFF).  */
#define PACK_OTF_TAG(TAG)	\
  ((((TAG) & 0x7F000000) >> 3)	\
    | (((TAG) & 0x7F0000) >> 2)	\
    | (((TAG) & 0x7F00) >> 1)	\
    | ((TAG) & 0x7F))

/* Assuming that FONT is an OpenType font, apply OpenType features
   specified in SPEC on glyphs between FROM and TO of IN, and record
   the lastly applied feature in each glyph of IN.  If OUT is not
   NULL, append the resulting glyphs to OUT while storing glyph
   position adjustment information in ADJUSTMENT.  */

static int
ftfont_drive_otf (MFLTFont *font,
		  MFLTOtfSpec *spec,
		  MFLTGlyphString *in,
		  int from,
		  int to,
		  MFLTGlyphString *out,
		  MFLTGlyphAdjustment *adjustment)
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  FT_Face ft_face = flt_font_ft->ft_face;
  OTF *otf = flt_font_ft->otf;
  int len = to - from;
  int i, j, gidx;
  OTF_Glyph *otfg;
  char script[5], *langsys = NULL;
  char *gsub_features = NULL, *gpos_features = NULL;
  OTF_Feature *features;

  if (len == 0)
    return from;
  OTF_tag_name (spec->script, script);
  if (spec->langsys)
    {
      langsys = alloca (5);
      OTF_tag_name (spec->langsys, langsys);
    }
  for (i = 0; i < 2; i++)
    {
      char *p;

      if (spec->features[i] && spec->features[i][1] != 0xFFFFFFFF)
	{
	  for (j = 0; spec->features[i][j]; j++);
	  if (i == 0)
	    p = gsub_features = alloca (6 * j);
	  else
	    p = gpos_features = alloca (6 * j);
	  for (j = 0; spec->features[i][j]; j++)
	    {
	      if (spec->features[i][j] == 0xFFFFFFFF)
		*p++ = '*', *p++ = ',';
	      else
		{
		  OTF_tag_name (spec->features[i][j], p);
		  p[4] = ',';
		  p += 5;
		}
	    }
	  *--p = '\0';
	}
    }

  setup_otf_gstring (len);
  for (i = 0; i < len; i++)
    {
      otf_gstring.glyphs[i].c = in->glyphs[from + i].c & 0x11FFFF;
      otf_gstring.glyphs[i].glyph_id = in->glyphs[from + i].code;
    }

  OTF_drive_gdef (otf, &otf_gstring);
  gidx = out ? out->used : from;

  if (gsub_features && out)
    {
      if (OTF_drive_gsub_with_log (otf, &otf_gstring, script, langsys,
				   gsub_features) < 0)
	goto simple_copy;
      if (out->allocated < out->used + otf_gstring.used)
	return -2;
      features = otf->gsub->FeatureList.Feature;
      for (i = 0, otfg = otf_gstring.glyphs; i < otf_gstring.used; )
	{
	  MFLTGlyph *g;
	  int min_from, max_to;
	  int feature_idx = otfg->positioning_type >> 4;

	  g = out->glyphs + out->used;
	  *g = in->glyphs[from + otfg->f.index.from];
	  if (g->code != otfg->glyph_id)
	    {
	      g->c = 0;
	      g->code = otfg->glyph_id;
	      g->measured = 0;
	    }
	  out->used++;
	  min_from = g->from;
	  max_to = g->to;
	  if (otfg->f.index.from < otfg->f.index.to)
	    {
	      /* OTFG substitutes multiple glyphs in IN.  */
	      for (j = from + otfg->f.index.from + 1;
		   j <= from + otfg->f.index.to; j++)
		{
		  if (min_from > in->glyphs[j].from)
		    min_from = in->glyphs[j].from;
		  if (max_to < in->glyphs[j].to)
		    max_to = in->glyphs[j].to;
		}
	      g->from = min_from;
	      g->to = max_to;
	    }
	  if (feature_idx)
	    {
	      unsigned int tag = features[feature_idx - 1].FeatureTag;
	      tag = PACK_OTF_TAG (tag);
	      g->internal = (g->internal & ~0x1FFFFFFF) | tag;
	    }
	  for (i++, otfg++; (i < otf_gstring.used
			     && otfg->f.index.from == otfg[-1].f.index.from);
	       i++, otfg++)
	    {
	      g = out->glyphs + out->used;
	      *g = in->glyphs[from + otfg->f.index.to];
	      if (g->code != otfg->glyph_id)
		{
		  g->c = 0;
		  g->code = otfg->glyph_id;
		  g->measured = 0;
		}
	      feature_idx = otfg->positioning_type >> 4;
	      if (feature_idx)
		{
		  unsigned int tag = features[feature_idx - 1].FeatureTag;
		  tag = PACK_OTF_TAG (tag);
		  g->internal = (g->internal & ~0x1FFFFFFF) | tag;
		}
	      out->used++;
	    }
	}
    }
  else if (gsub_features)
    {
      /* Just for checking which features will be applied.  */
      if (OTF_drive_gsub_with_log (otf, &otf_gstring, script, langsys,
				   gsub_features) < 0)
	goto simple_copy;
      features = otf->gsub->FeatureList.Feature;
      for (i = 0, otfg = otf_gstring.glyphs; i < otf_gstring.used; i++,
	     otfg++)
	{
	  int feature_idx = otfg->positioning_type >> 4;

	  if (feature_idx)
	    {
	      unsigned int tag = features[feature_idx - 1].FeatureTag;
	      tag = PACK_OTF_TAG (tag);
	      for (j = otfg->f.index.from; j <= otfg->f.index.to; j++)
		{
		  MFLTGlyph *g = in->glyphs + (from + j);
		  g->internal = (g->internal & ~0x1FFFFFFF) | tag;
		}
	    }
	}
    }
  else if (out)
    {
      if (out->allocated < out->used + len)
	return -2;
      for (i = 0; i < len; i++)
	out->glyphs[out->used++] = in->glyphs[from + i];
    }

  if (gpos_features && out)
    {
      MFLTGlyph *base = NULL, *mark = NULL, *g;
      int x_ppem, y_ppem, x_scale, y_scale;

      if (OTF_drive_gpos_with_log (otf, &otf_gstring, script, langsys,
				   gpos_features) < 0)
	return to;
      features = otf->gpos->FeatureList.Feature;
      x_ppem = ft_face->size->metrics.x_ppem;
      y_ppem = ft_face->size->metrics.y_ppem;
      x_scale = ft_face->size->metrics.x_scale;
      y_scale = ft_face->size->metrics.y_scale;

      for (i = 0, otfg = otf_gstring.glyphs, g = out->glyphs + gidx;
	   i < otf_gstring.used; i++, otfg++, g++)
	{
	  MFLTGlyph *prev;
	  int feature_idx = otfg->positioning_type >> 4;

	  if (feature_idx)
	    {
	      unsigned int tag = features[feature_idx - 1].FeatureTag;
	      tag = PACK_OTF_TAG (tag);
	      g->internal = (g->internal & ~0x1FFFFFFF) | tag;
	    }

	  if (! otfg->glyph_id)
	    continue;
	  switch (otfg->positioning_type & 0xF)
	    {
	    case 0:
	      break;
	    case 1: 		/* Single */
	    case 2: 		/* Pair */
	      {
		int format = otfg->f.f1.format;

		if (format & OTF_XPlacement)
		  adjustment[i].xoff
		    = otfg->f.f1.value->XPlacement * x_scale / 0x10000;
		if (format & OTF_XPlaDevice)
		  adjustment[i].xoff
		    += DEVICE_DELTA (otfg->f.f1.value->XPlaDevice, x_ppem);
		if (format & OTF_YPlacement)
		  adjustment[i].yoff
		    = - (otfg->f.f1.value->YPlacement * y_scale / 0x10000);
		if (format & OTF_YPlaDevice)
		  adjustment[i].yoff
		    -= DEVICE_DELTA (otfg->f.f1.value->YPlaDevice, y_ppem);
		if (format & OTF_XAdvance)
		  adjustment[i].xadv
		    += otfg->f.f1.value->XAdvance * x_scale / 0x10000;
		if (format & OTF_XAdvDevice)
		  adjustment[i].xadv
		    += DEVICE_DELTA (otfg->f.f1.value->XAdvDevice, x_ppem);
		if (format & OTF_YAdvance)
		  adjustment[i].yadv
		    += otfg->f.f1.value->YAdvance * y_scale / 0x10000;
		if (format & OTF_YAdvDevice)
		  adjustment[i].yadv
		    += DEVICE_DELTA (otfg->f.f1.value->YAdvDevice, y_ppem);
		adjustment[i].set = 1;
	      }
	      break;
	    case 3:		/* Cursive */
	      /* Not yet supported.  */
	      break;
	    case 4:		/* Mark-to-Base */
	    case 5:		/* Mark-to-Ligature */
	      if (! base)
		break;
	      prev = base;
	      goto label_adjust_anchor;
	    default:		/* i.e. case 6 Mark-to-Mark */
	      if (! mark)
		break;
	      prev = mark;

	    label_adjust_anchor:
	      {
		int base_x, base_y, mark_x, mark_y;
		int this_from, this_to;

		base_x = otfg->f.f4.base_anchor->XCoordinate * x_scale / 0x10000;
		base_y = otfg->f.f4.base_anchor->YCoordinate * y_scale / 0x10000;
		mark_x = otfg->f.f4.mark_anchor->XCoordinate * x_scale / 0x10000;
		mark_y = otfg->f.f4.mark_anchor->YCoordinate * y_scale / 0x10000;

		if (otfg->f.f4.base_anchor->AnchorFormat != 1)
		  adjust_anchor (ft_face, otfg->f.f4.base_anchor,
				 prev->code, x_ppem, y_ppem, &base_x, &base_y);
		if (otfg->f.f4.mark_anchor->AnchorFormat != 1)
		  adjust_anchor (ft_face, otfg->f.f4.mark_anchor, g->code,
				 x_ppem, y_ppem, &mark_x, &mark_y);
		adjustment[i].xoff = (base_x - mark_x);
		adjustment[i].yoff = - (base_y - mark_y);
		adjustment[i].back = (g - prev);
		adjustment[i].xadv = 0;
		adjustment[i].advance_is_absolute = 1;
		adjustment[i].set = 1;
		this_from = g->from;
		this_to = g->to;
		for (j = 0; prev + j < g; j++)
		  {
		    if (this_from > prev[j].from)
		      this_from = prev[j].from;
		    if (this_to < prev[j].to)
		      this_to = prev[j].to;
		  }
		for (; prev <= g; prev++)
		  {
		    prev->from = this_from;
		    prev->to = this_to;
		  }
	      }
	    }
	  if (otfg->GlyphClass == OTF_GlyphClass0)
	    base = mark = g;
	  else if (otfg->GlyphClass == OTF_GlyphClassMark)
	    mark = g;
	  else
	    base = g;
	}
    }
  else if (gpos_features)
    {
      if (OTF_drive_gpos_with_log (otf, &otf_gstring, script, langsys,
				   gpos_features) < 0)
	return to;
      features = otf->gpos->FeatureList.Feature;
      for (i = 0, otfg = otf_gstring.glyphs; i < otf_gstring.used;
	   i++, otfg++)
	if (otfg->positioning_type & 0xF)
	  {
	    int feature_idx = otfg->positioning_type >> 4;

	    if (feature_idx)
	      {
		unsigned int tag = features[feature_idx - 1].FeatureTag;
		tag = PACK_OTF_TAG (tag);
		for (j = otfg->f.index.from; j <= otfg->f.index.to; j++)
		  {
		    MFLTGlyph *g = in->glyphs + (from + j);
		    g->internal = (g->internal & ~0x1FFFFFFF) | tag;
		  }
	      }
	  }
    }
  return to;

 simple_copy:
  if (! out)
    return to;
  if (out->allocated < out->used + len)
    return -2;
  font->get_metrics (font, in, from, to);
  memcpy (out->glyphs + out->used, in->glyphs + from,
	  sizeof (MFLTGlyph) * len);
  out->used += len;
  return to;
}

static int
ftfont_try_otf (MFLTFont *font, MFLTOtfSpec *spec,
		MFLTGlyphString *in, int from, int to)
{
  return ftfont_drive_otf (font, spec, in, from, to, NULL, NULL);
}

#else  /* not M17N_FLT_USE_NEW_FEATURE */

static int
ftfont_drive_otf (MFLTFont *font, MFLTOtfSpec *spec, MFLTGlyphString *in,
		  int from, int to,
		  MFLTGlyphString *out, MFLTGlyphAdjustment *adjustment)
{
  struct MFLTFontFT *flt_font_ft = (struct MFLTFontFT *) font;
  FT_Face ft_face = flt_font_ft->ft_face;
  OTF *otf = flt_font_ft->otf;
  int len = to - from;
  int i, j, gidx;
  OTF_Glyph *otfg;
  char script[5], *langsys = NULL;
  char *gsub_features = NULL, *gpos_features = NULL;

  if (len == 0)
    return from;
  OTF_tag_name (spec->script, script);
  if (spec->langsys)
    {
      langsys = alloca (5);
      OTF_tag_name (spec->langsys, langsys);
    }
  for (i = 0; i < 2; i++)
    {
      char *p;

      if (spec->features[i] && spec->features[i][1] != 0xFFFFFFFF)
	{
	  for (j = 0; spec->features[i][j]; j++);
	  if (i == 0)
	    p = gsub_features = alloca (6 * j);
	  else
	    p = gpos_features = alloca (6 * j);
	  for (j = 0; spec->features[i][j]; j++)
	    {
	      if (spec->features[i][j] == 0xFFFFFFFF)
		*p++ = '*', *p++ = ',';
	      else
		{
		  OTF_tag_name (spec->features[i][j], p);
		  p[4] = ',';
		  p += 5;
		}
	    }
	  *--p = '\0';
	}
    }

  setup_otf_gstring (len);
  for (i = 0; i < len; i++)
    {
      otf_gstring.glyphs[i].c = in->glyphs[from + i].c;
      otf_gstring.glyphs[i].glyph_id = in->glyphs[from + i].code;
    }

  OTF_drive_gdef (otf, &otf_gstring);
  gidx = out->used;

  if (gsub_features)
    {
      if (OTF_drive_gsub (otf, &otf_gstring, script, langsys, gsub_features)
	  < 0)
	goto simple_copy;
      if (out->allocated < out->used + otf_gstring.used)
	return -2;
      for (i = 0, otfg = otf_gstring.glyphs; i < otf_gstring.used; )
	{
	  MFLTGlyph *g;
	  int min_from, max_to;
	  int j;

	  g = out->glyphs + out->used;
	  *g = in->glyphs[from + otfg->f.index.from];
	  if (g->code != otfg->glyph_id)
	    {
	      g->c = 0;
	      g->code = otfg->glyph_id;
	      g->measured = 0;
	    }
	  out->used++;
	  min_from = g->from;
	  max_to = g->to;
	  if (otfg->f.index.from < otfg->f.index.to)
	    {
	      /* OTFG substitutes multiple glyphs in IN.  */
	      for (j = from + otfg->f.index.from + 1;
		   j <= from + otfg->f.index.to; j++)
		{
		  if (min_from > in->glyphs[j].from)
		    min_from = in->glyphs[j].from;
		  if (max_to < in->glyphs[j].to)
		    max_to = in->glyphs[j].to;
		}
	      g->from = min_from;
	      g->to = max_to;
	    }
	  for (i++, otfg++; (i < otf_gstring.used
			     && otfg->f.index.from == otfg[-1].f.index.from);
	       i++, otfg++)
	    {
	      g = out->glyphs + out->used;
	      *g = in->glyphs[from + otfg->f.index.to];
	      if (g->code != otfg->glyph_id)
		{
		  g->c = 0;
		  g->code = otfg->glyph_id;
		  g->measured = 0;
		}
	      out->used++;
	    }
	}
    }
  else
    {
      if (out->allocated < out->used + len)
	return -2;
      for (i = 0; i < len; i++)
	out->glyphs[out->used++] = in->glyphs[from + i];
    }

  if (gpos_features)
    {
      MFLTGlyph *base = NULL, *mark = NULL, *g;
      int x_ppem, y_ppem, x_scale, y_scale;

      if (OTF_drive_gpos (otf, &otf_gstring, script, langsys, gpos_features)
	  < 0)
	return to;

      x_ppem = ft_face->size->metrics.x_ppem;
      y_ppem = ft_face->size->metrics.y_ppem;
      x_scale = ft_face->size->metrics.x_scale;
      y_scale = ft_face->size->metrics.y_scale;

      for (i = 0, otfg = otf_gstring.glyphs, g = out->glyphs + gidx;
	   i < otf_gstring.used; i++, otfg++, g++)
	{
	  MFLTGlyph *prev;

	  if (! otfg->glyph_id)
	    continue;
	  switch (otfg->positioning_type)
	    {
	    case 0:
	      break;
	    case 1: 		/* Single */
	    case 2: 		/* Pair */
	      {
		int format = otfg->f.f1.format;

		if (format & OTF_XPlacement)
		  adjustment[i].xoff
		    = otfg->f.f1.value->XPlacement * x_scale / 0x10000;
		if (format & OTF_XPlaDevice)
		  adjustment[i].xoff
		    += DEVICE_DELTA (otfg->f.f1.value->XPlaDevice, x_ppem);
		if (format & OTF_YPlacement)
		  adjustment[i].yoff
		    = - (otfg->f.f1.value->YPlacement * y_scale / 0x10000);
		if (format & OTF_YPlaDevice)
		  adjustment[i].yoff
		    -= DEVICE_DELTA (otfg->f.f1.value->YPlaDevice, y_ppem);
		if (format & OTF_XAdvance)
		  adjustment[i].xadv
		    += otfg->f.f1.value->XAdvance * x_scale / 0x10000;
		if (format & OTF_XAdvDevice)
		  adjustment[i].xadv
		    += DEVICE_DELTA (otfg->f.f1.value->XAdvDevice, x_ppem);
		if (format & OTF_YAdvance)
		  adjustment[i].yadv
		    += otfg->f.f1.value->YAdvance * y_scale / 0x10000;
		if (format & OTF_YAdvDevice)
		  adjustment[i].yadv
		    += DEVICE_DELTA (otfg->f.f1.value->YAdvDevice, y_ppem);
		adjustment[i].set = 1;
	      }
	      break;
	    case 3:		/* Cursive */
	      /* Not yet supported.  */
	      break;
	    case 4:		/* Mark-to-Base */
	    case 5:		/* Mark-to-Ligature */
	      if (! base)
		break;
	      prev = base;
	      goto label_adjust_anchor;
	    default:		/* i.e. case 6 Mark-to-Mark */
	      if (! mark)
		break;
	      prev = mark;

	    label_adjust_anchor:
	      {
		int base_x, base_y, mark_x, mark_y;
		int this_from, this_to;

		base_x = otfg->f.f4.base_anchor->XCoordinate * x_scale / 0x10000;
		base_y = otfg->f.f4.base_anchor->YCoordinate * y_scale / 0x10000;
		mark_x = otfg->f.f4.mark_anchor->XCoordinate * x_scale / 0x10000;
		mark_y = otfg->f.f4.mark_anchor->YCoordinate * y_scale / 0x10000;

		if (otfg->f.f4.base_anchor->AnchorFormat != 1)
		  adjust_anchor (ft_face, otfg->f.f4.base_anchor,
				 prev->code, x_ppem, y_ppem, &base_x, &base_y);
		if (otfg->f.f4.mark_anchor->AnchorFormat != 1)
		  adjust_anchor (ft_face, otfg->f.f4.mark_anchor, g->code,
				 x_ppem, y_ppem, &mark_x, &mark_y);
		adjustment[i].xoff = (base_x - mark_x);
		adjustment[i].yoff = - (base_y - mark_y);
		adjustment[i].back = (g - prev);
		adjustment[i].xadv = 0;
		adjustment[i].advance_is_absolute = 1;
		adjustment[i].set = 1;
		this_from = g->from;
		this_to = g->to;
		for (j = 0; prev + j < g; j++)
		  {
		    if (this_from > prev[j].from)
		      this_from = prev[j].from;
		    if (this_to < prev[j].to)
		      this_to = prev[j].to;
		  }
		for (; prev <= g; prev++)
		  {
		    prev->from = this_from;
		    prev->to = this_to;
		  }
	      }
	    }
	  if (otfg->GlyphClass == OTF_GlyphClass0)
	    base = mark = g;
	  else if (otfg->GlyphClass == OTF_GlyphClassMark)
	    mark = g;
	  else
	    base = g;
	}
    }
  return to;

 simple_copy:
  if (out->allocated < out->used + len)
    return -2;
  font->get_metrics (font, in, from, to);
  memcpy (out->glyphs + out->used, in->glyphs + from,
	  sizeof (MFLTGlyph) * len);
  out->used += len;
  return to;
}

#endif	/* not M17N_FLT_USE_NEW_FEATURE */

static MFLTGlyphString gstring;

static int m17n_flt_initialized;

static Lisp_Object
ftfont_shape_by_flt (Lisp_Object lgstring, struct font *font,
		     FT_Face ft_face, OTF *otf, FT_Matrix *matrix)
{
  EMACS_INT len = LGSTRING_GLYPH_LEN (lgstring);
  EMACS_INT i;
  struct MFLTFontFT flt_font_ft;
  MFLT *flt = NULL;
  int with_variation_selector = 0;

  if (! m17n_flt_initialized)
    {
      M17N_INIT ();
#ifdef M17N_FLT_USE_NEW_FEATURE
      mflt_enable_new_feature = 1;
      mflt_try_otf = ftfont_try_otf;
#endif	/* M17N_FLT_USE_NEW_FEATURE */
      m17n_flt_initialized = 1;
    }

  for (i = 0; i < len; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (lgstring, i);
      int c;

      if (NILP (g))
	break;
      c = LGLYPH_CHAR (g);
      if (CHAR_VARIATION_SELECTOR_P (c))
	with_variation_selector++;
    }

  len = i;
  lint_assume (len <= TYPE_MAXIMUM (EMACS_INT) - 2);

  if (with_variation_selector)
    {
      setup_otf_gstring (len);
      for (i = 0; i < len; i++)
	{
	  Lisp_Object g = LGSTRING_GLYPH (lgstring, i);

	  otf_gstring.glyphs[i].c = LGLYPH_CHAR (g);
	  otf_gstring.glyphs[i].f.index.from = LGLYPH_FROM (g);
	  otf_gstring.glyphs[i].f.index.to = LGLYPH_TO (g);
	}
      OTF_drive_cmap (otf, &otf_gstring);
      for (i = 0; i < otf_gstring.used; i++)
	{
	  OTF_Glyph *otfg = otf_gstring.glyphs + i;
	  Lisp_Object g0 = LGSTRING_GLYPH (lgstring, otfg->f.index.from);
	  Lisp_Object g1 = LGSTRING_GLYPH (lgstring, otfg->f.index.to);

	  LGLYPH_SET_CODE (g0, otfg->glyph_id);
	  LGLYPH_SET_TO (g0, LGLYPH_TO (g1));
	  LGSTRING_SET_GLYPH (lgstring, i, g0);
	}
      if (len > otf_gstring.used)
	{
	  len = otf_gstring.used;
	  LGSTRING_SET_GLYPH (lgstring, len, Qnil);
	}
    }

  if (INT_MAX / 2 < len)
    memory_full (SIZE_MAX);

  if (gstring.allocated == 0)
    {
      gstring.glyph_size = sizeof (MFLTGlyph);
      gstring.glyphs = xnmalloc (len * 2, sizeof (MFLTGlyph));
      gstring.allocated = len * 2;
    }
  else if (gstring.allocated < len * 2)
    {
      gstring.glyphs = xnrealloc (gstring.glyphs, len * 2, sizeof (MFLTGlyph));
      gstring.allocated = len * 2;
    }
  memset (gstring.glyphs, 0, sizeof (MFLTGlyph) * len);
  for (i = 0; i < len; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (lgstring, i);

      gstring.glyphs[i].c = LGLYPH_CHAR (g);
      if (with_variation_selector)
	{
	  gstring.glyphs[i].code = LGLYPH_CODE (g);
	  gstring.glyphs[i].encoded = 1;
	}
    }

  gstring.used = len;
  gstring.r2l = 0;

  {
    Lisp_Object family = Ffont_get (LGSTRING_FONT (lgstring), QCfamily);

    if (NILP (family))
      flt_font_ft.flt_font.family = Mnil;
    else
      flt_font_ft.flt_font.family
	= msymbol (SSDATA (Fdowncase (SYMBOL_NAME (family))));
  }
  flt_font_ft.flt_font.x_ppem = ft_face->size->metrics.x_ppem;
  flt_font_ft.flt_font.y_ppem = ft_face->size->metrics.y_ppem;
  flt_font_ft.flt_font.get_glyph_id = ftfont_get_glyph_id;
  flt_font_ft.flt_font.get_metrics = ftfont_get_metrics;
  flt_font_ft.flt_font.check_otf = ftfont_check_otf;
  flt_font_ft.flt_font.drive_otf = ftfont_drive_otf;
  flt_font_ft.flt_font.internal = NULL;
  flt_font_ft.font = font;
  flt_font_ft.ft_face = ft_face;
  flt_font_ft.otf = otf;
  flt_font_ft.matrix = matrix->xx != 0 ? matrix : 0;
  if (len > 1
      && gstring.glyphs[1].c >= 0x300 && gstring.glyphs[1].c <= 0x36F)
    /* A little bit ad hoc.  Perhaps, shaper must get script and
       language information, and select a proper flt for them
       here.  */
    flt = mflt_get (msymbol ("combining"));
  for (i = 0; i < 3; i++)
    {
      int result = mflt_run (&gstring, 0, len, &flt_font_ft.flt_font, flt);
      if (result != -2)
	break;
      if (INT_MAX / 2 < gstring.allocated)
	memory_full (SIZE_MAX);
      gstring.glyphs = xnrealloc (gstring.glyphs,
				  gstring.allocated, 2 * sizeof (MFLTGlyph));
      gstring.allocated *= 2;
    }
  if (gstring.used > LGSTRING_GLYPH_LEN (lgstring))
    return Qnil;
  for (i = 0; i < gstring.used; i++)
    {
      MFLTGlyph *g = gstring.glyphs + i;

      g->from = LGLYPH_FROM (LGSTRING_GLYPH (lgstring, g->from));
      g->to = LGLYPH_TO (LGSTRING_GLYPH (lgstring, g->to));
    }

  for (i = 0; i < gstring.used; i++)
    {
      Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, i);
      MFLTGlyph *g = gstring.glyphs + i;

      if (NILP (lglyph))
	{
	  lglyph = Fmake_vector (make_number (LGLYPH_SIZE), Qnil);
	  LGSTRING_SET_GLYPH (lgstring, i, lglyph);
	}
      LGLYPH_SET_FROM (lglyph, g->from);
      LGLYPH_SET_TO (lglyph, g->to);
      LGLYPH_SET_CHAR (lglyph, g->c);
      LGLYPH_SET_CODE (lglyph, g->code);
      LGLYPH_SET_WIDTH (lglyph, g->xadv >> 6);
      LGLYPH_SET_LBEARING (lglyph, g->lbearing >> 6);
      LGLYPH_SET_RBEARING (lglyph, g->rbearing >> 6);
      LGLYPH_SET_ASCENT (lglyph, g->ascent >> 6);
      LGLYPH_SET_DESCENT (lglyph, g->descent >> 6);
      if (g->adjusted)
	{
	  Lisp_Object vec;

	  vec = Fmake_vector (make_number (3), Qnil);
	  ASET (vec, 0, make_number (g->xoff >> 6));
	  ASET (vec, 1, make_number (g->yoff >> 6));
	  ASET (vec, 2, make_number (g->xadv >> 6));
	  LGLYPH_SET_ADJUSTMENT (lglyph, vec);
	}
    }
  return make_number (i);
}

Lisp_Object
ftfont_shape (Lisp_Object lgstring)
{
  struct font *font;
  struct ftfont_info *ftfont_info;
  OTF *otf;

  CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring), font);
  ftfont_info = (struct ftfont_info *) font;
  otf = ftfont_get_otf (ftfont_info);
  if (! otf)
    return make_number (0);
  return ftfont_shape_by_flt (lgstring, font, ftfont_info->ft_size->face, otf,
			      &ftfont_info->matrix);
}

#endif	/* HAVE_M17N_FLT */

#ifdef HAVE_OTF_GET_VARIATION_GLYPHS

static int
ftfont_variation_glyphs (struct font *font, int c, unsigned variations[256])
{
  struct ftfont_info *ftfont_info = (struct ftfont_info *) font;
  OTF *otf = ftfont_get_otf (ftfont_info);

  if (! otf)
    return 0;
  return OTF_get_variation_glyphs (otf, c, variations);
}

#endif	/* HAVE_OTF_GET_VARIATION_GLYPHS */
#endif	/* HAVE_LIBOTF */

Lisp_Object
ftfont_font_format (FcPattern *pattern, Lisp_Object filename)
{
  FcChar8 *str;

#ifdef FC_FONTFORMAT
  if (pattern)
    {
      if (FcPatternGetString (pattern, FC_FONTFORMAT, 0, &str) != FcResultMatch)
	return Qnil;
      if (strcmp ((char *) str, "TrueType") == 0)
	return intern ("truetype");
      if (strcmp ((char *) str, "Type 1") == 0)
	return intern ("type1");
      if (strcmp ((char *) str, "PCF") == 0)
	return intern ("pcf");
      if (strcmp ((char *) str, "BDF") == 0)
	return intern ("bdf");
    }
#endif  /* FC_FONTFORMAT */
  if (STRINGP (filename))
    {
      int len = SBYTES (filename);

      if (len >= 4)
	{
	  str = (FcChar8 *) (SDATA (filename) + len - 4);
	  if (xstrcasecmp ((char *) str, ".ttf") == 0)
	    return intern ("truetype");
	  if (xstrcasecmp ((char *) str, ".pfb") == 0)
	    return intern ("type1");
	  if (xstrcasecmp ((char *) str, ".pcf") == 0)
	    return intern ("pcf");
	  if (xstrcasecmp ((char *) str, ".bdf") == 0)
	    return intern ("bdf");
	}
    }
  return intern ("unknown");
}

static const char *const ftfont_booleans [] = {
  ":antialias",
  ":hinting",
  ":verticallayout",
  ":autohint",
  ":globaladvance",
  ":outline",
  ":scalable",
  ":minspace",
  ":embolden",
  NULL,
};

static const char *const ftfont_non_booleans [] = {
  ":family",
  ":familylang",
  ":style",
  ":stylelang",
  ":fullname",
  ":fullnamelang",
  ":slant",
  ":weight",
  ":size",
  ":width",
  ":aspect",
  ":pixelsize",
  ":spacing",
  ":foundry",
  ":hintstyle",
  ":file",
  ":index",
  ":ftface",
  ":rasterizer",
  ":scale",
  ":dpi",
  ":rgba",
  ":lcdfilter",
  ":charset",
  ":lang",
  ":fontversion",
  ":capability",
  NULL,
};

static void
ftfont_filter_properties (Lisp_Object font, Lisp_Object alist)
{
  font_filter_properties (font, alist, ftfont_booleans, ftfont_non_booleans);
}


void
syms_of_ftfont (void)
{
  DEFSYM (Qfreetype, "freetype");
  DEFSYM (Qmonospace, "monospace");
  DEFSYM (Qsans_serif, "sans-serif");
  DEFSYM (Qserif, "serif");
  DEFSYM (Qmono, "mono");
  DEFSYM (Qsans, "sans");
  DEFSYM (Qsans__serif, "sans serif");

  staticpro (&freetype_font_cache);
  freetype_font_cache = Fcons (Qt, Qnil);

  staticpro (&ftfont_generic_family_list);
  ftfont_generic_family_list
    = Fcons (Fcons (Qmonospace, Qt),
	     Fcons (Fcons (Qsans_serif, Qt),
		    Fcons (Fcons (Qsans, Qt), Qnil)));

  staticpro (&ft_face_cache);
  ft_face_cache = Qnil;

  ftfont_driver.type = Qfreetype;
  register_font_driver (&ftfont_driver, NULL);
}
