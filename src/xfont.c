/* xfont.c -- X core font driver.
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

#include "lisp.h"
#include "dispextern.h"
#include "xterm.h"
#include "frame.h"
#include "blockinput.h"
#include "character.h"
#include "charset.h"
#include "fontset.h"
#include "font.h"
#include "ccl.h"


/* X core font driver.  */

struct xfont_info
{
  struct font font;
  Display *display;
  XFontStruct *xfont;
};

/* Prototypes of support functions.  */
extern void x_clear_errors (Display *);

static XCharStruct *xfont_get_pcm (XFontStruct *, XChar2b *);

/* Get metrics of character CHAR2B in XFONT.  Value is null if CHAR2B
   is not contained in the font.  */

static XCharStruct *
xfont_get_pcm (XFontStruct *xfont, XChar2b *char2b)
{
  /* The result metric information.  */
  XCharStruct *pcm = NULL;

  font_assert (xfont && char2b);

  if (xfont->per_char != NULL)
    {
      if (xfont->min_byte1 == 0 && xfont->max_byte1 == 0)
	{
	  /* min_char_or_byte2 specifies the linear character index
	     corresponding to the first element of the per_char array,
	     max_char_or_byte2 is the index of the last character.  A
	     character with non-zero CHAR2B->byte1 is not in the font.
	     A character with byte2 less than min_char_or_byte2 or
	     greater max_char_or_byte2 is not in the font.  */
	  if (char2b->byte1 == 0
	      && char2b->byte2 >= xfont->min_char_or_byte2
	      && char2b->byte2 <= xfont->max_char_or_byte2)
	    pcm = xfont->per_char + char2b->byte2 - xfont->min_char_or_byte2;
	}
      else
	{
	  /* If either min_byte1 or max_byte1 are nonzero, both
	     min_char_or_byte2 and max_char_or_byte2 are less than
	     256, and the 2-byte character index values corresponding
	     to the per_char array element N (counting from 0) are:

	     byte1 = N/D + min_byte1
	     byte2 = N\D + min_char_or_byte2

	     where:

	     D = max_char_or_byte2 - min_char_or_byte2 + 1
	     / = integer division
	     \ = integer modulus  */
	  if (char2b->byte1 >= xfont->min_byte1
	      && char2b->byte1 <= xfont->max_byte1
	      && char2b->byte2 >= xfont->min_char_or_byte2
	      && char2b->byte2 <= xfont->max_char_or_byte2)
	    pcm = (xfont->per_char
		   + ((xfont->max_char_or_byte2 - xfont->min_char_or_byte2 + 1)
		      * (char2b->byte1 - xfont->min_byte1))
		   + (char2b->byte2 - xfont->min_char_or_byte2));
	}
    }
  else
    {
      /* If the per_char pointer is null, all glyphs between the first
	 and last character indexes inclusive have the same
	 information, as given by both min_bounds and max_bounds.  */
      if (char2b->byte2 >= xfont->min_char_or_byte2
	  && char2b->byte2 <= xfont->max_char_or_byte2)
	pcm = &xfont->max_bounds;
    }

  return ((pcm == NULL
	   || (pcm->width == 0 && (pcm->rbearing - pcm->lbearing) == 0))
	  ? NULL : pcm);
}

static Lisp_Object xfont_get_cache (FRAME_PTR);
static Lisp_Object xfont_list (Lisp_Object, Lisp_Object);
static Lisp_Object xfont_match (Lisp_Object, Lisp_Object);
static Lisp_Object xfont_list_family (Lisp_Object);
static Lisp_Object xfont_open (FRAME_PTR, Lisp_Object, int);
static void xfont_close (FRAME_PTR, struct font *);
static int xfont_prepare_face (FRAME_PTR, struct face *);
static int xfont_has_char (Lisp_Object, int);
static unsigned xfont_encode_char (struct font *, int);
static int xfont_text_extents (struct font *, unsigned *, int,
                               struct font_metrics *);
static int xfont_draw (struct glyph_string *, int, int, int, int, int);
static int xfont_check (FRAME_PTR, struct font *);

struct font_driver xfont_driver =
  {
    0,				/* Qx */
    0,				/* case insensitive */
    xfont_get_cache,
    xfont_list,
    xfont_match,
    xfont_list_family,
    NULL,
    xfont_open,
    xfont_close,
    xfont_prepare_face,
    NULL,
    xfont_has_char,
    xfont_encode_char,
    xfont_text_extents,
    xfont_draw,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    xfont_check,
    NULL, /* get_variation_glyphs */
    NULL, /* filter_properties */
  };

static Lisp_Object
xfont_get_cache (FRAME_PTR f)
{
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  return (dpyinfo->name_list_element);
}

static int
compare_font_names (const void *name1, const void *name2)
{
  return xstrcasecmp (*(const char **) name1,
		      *(const char **) name2);
}

/* Decode XLFD as iso-8859-1 into OUTPUT, and return the byte length
   of the decoding result.  LEN is the byte length of XLFD, or -1 if
   XLFD is NULL terminated.  The caller must assure that OUTPUT is at
   least twice (plus 1) as large as XLFD.  */

static int
xfont_decode_coding_xlfd (char *xlfd, int len, char *output)
{
  char *p0 = xlfd, *p1 = output;
  int c;

  while (*p0)
    {
      c = *(unsigned char *) p0++;
      p1 += CHAR_STRING (c, (unsigned char *) p1);
      if (--len == 0)
	break;
    }
  *p1 = 0;
  return (p1 - output);
}

/* Encode XLFD from UTF-8 to iso-8859-1 destructively, and return the
   resulting byte length.  If XLFD contains unencodable character,
   return -1.  */

static int
xfont_encode_coding_xlfd (char *xlfd)
{
  const unsigned char *p0 = (unsigned char *) xlfd;
  unsigned char *p1 = (unsigned char *) xlfd;
  int len = 0;

  while (*p0)
    {
      int c = STRING_CHAR_ADVANCE (p0);

      if (c >= 0x100)
	return -1;
      *p1++ = c;
      len++;
    }
  *p1 = 0;
  return len;
}

/* Check if CHARS (cons or vector) is supported by XFONT whose
   encoding charset is ENCODING (XFONT is NULL) or by a font whose
   registry corresponds to ENCODING and REPERTORY.
   Return 1 if supported, return 0 otherwise.  */

static int
xfont_chars_supported (Lisp_Object chars, XFontStruct *xfont,
		       struct charset *encoding, struct charset *repertory)
{
  struct charset *charset = repertory ? repertory : encoding;

  if (CONSP (chars))
    {
      for (; CONSP (chars); chars = XCDR (chars))
	{
	  int c = XINT (XCAR (chars));
	  unsigned code = ENCODE_CHAR (charset, c);
	  XChar2b char2b;

	  if (code == CHARSET_INVALID_CODE (charset))
	    break;
	  if (! xfont)
	    continue;
	  if (code >= 0x10000)
	    break;
	  char2b.byte1 = code >> 8;
	  char2b.byte2 = code & 0xFF;
	  if (! xfont_get_pcm (xfont, &char2b))
	    break;
	}
      return (NILP (chars));
    }
  else if (VECTORP (chars))
    {
      int i;

      for (i = ASIZE (chars) - 1; i >= 0; i--)
	{
	  int c = XINT (AREF (chars, i));
	  unsigned code = ENCODE_CHAR (charset, c);
	  XChar2b char2b;

	  if (code == CHARSET_INVALID_CODE (charset))
	    continue;
	  if (! xfont)
	    break;
	  if (code >= 0x10000)
	    continue;
	  char2b.byte1 = code >> 8;
	  char2b.byte2 = code & 0xFF;
	  if (xfont_get_pcm (xfont, &char2b))
	    break;
	}
      return (i >= 0);
    }
  return 0;
}

/* A hash table recoding which font supports which scripts.  Each key
   is a vector of characteristic font properties FOUNDRY to WIDTH and
   ADDSTYLE, and each value is a list of script symbols.

   We assume that fonts that have the same value in the above
   properties supports the same set of characters on all displays.  */

static Lisp_Object xfont_scripts_cache;

/* Re-usable vector to store characteristic font properties.   */
static Lisp_Object xfont_scratch_props;

/* Return a list of scripts supported by the font of FONTNAME whose
   characteristic properties are in PROPS and whose encoding charset
   is ENCODING.  A caller must call BLOCK_INPUT in advance.  */

static Lisp_Object
xfont_supported_scripts (Display *display, char *fontname, Lisp_Object props,
			 struct charset *encoding)
{
  Lisp_Object scripts;

  /* Two special cases to avoid opening rather big fonts.  */
  if (EQ (AREF (props, 2), Qja))
    return Fcons (intern ("kana"), Fcons (intern ("han"), Qnil));
  if (EQ (AREF (props, 2), Qko))
    return Fcons (intern ("hangul"), Qnil);
  scripts = Fgethash (props, xfont_scripts_cache, Qt);
  if (EQ (scripts, Qt))
    {
      XFontStruct *xfont;
      Lisp_Object val;

      scripts = Qnil;
      xfont = XLoadQueryFont (display, fontname);
      if (xfont)
	{
	  if (xfont->per_char)
	    {
	      for (val = Vscript_representative_chars; CONSP (val);
		   val = XCDR (val))
		if (CONSP (XCAR (val)) && SYMBOLP (XCAR (XCAR (val))))
		  {
		    Lisp_Object script = XCAR (XCAR (val));
		    Lisp_Object chars = XCDR (XCAR (val));

		    if (xfont_chars_supported (chars, xfont, encoding, NULL))
		      scripts = Fcons (script, scripts);
		  }
	    }
	  XFreeFont (display, xfont);
	}
      if (EQ (AREF (props, 3), Qiso10646_1)
	  && NILP (Fmemq (Qlatin, scripts)))
	scripts = Fcons (Qlatin, scripts);
      Fputhash (Fcopy_sequence (props), scripts, xfont_scripts_cache);
    }
  return scripts;
}

static Lisp_Object
xfont_list_pattern (Display *display, const char *pattern,
		    Lisp_Object registry, Lisp_Object script)
{
  Lisp_Object list = Qnil;
  Lisp_Object chars = Qnil;
  struct charset *encoding, *repertory = NULL;
  int i, limit, num_fonts;
  char **names;
  /* Large enough to decode the longest XLFD (255 bytes). */
  char buf[512];

  if (! NILP (registry)
      && font_registry_charsets (registry, &encoding, &repertory) < 0)
    /* Unknown REGISTRY, not supported.  */
    return Qnil;
  if (! NILP (script))
    {
      chars = assq_no_quit (script, Vscript_representative_chars);
      if (NILP (chars))
	/* We can't tell whether or not a font supports SCRIPT.  */
	return Qnil;
      chars = XCDR (chars);
      if (repertory)
	{
	  if (! xfont_chars_supported (chars, NULL, encoding, repertory))
	    return Qnil;
	  script = Qnil;
	}
    }

  BLOCK_INPUT;
  x_catch_errors (display);

  for (limit = 512; ; limit *= 2)
    {
      names = XListFonts (display, pattern, limit, &num_fonts);
      if (x_had_errors_p (display))
	{
	  /* This error is perhaps due to insufficient memory on X
	     server.  Let's just ignore it.  */
	  x_clear_errors (display);
	  num_fonts = 0;
	  break;
	}
      if (num_fonts < limit)
	break;
      XFreeFontNames (names);
    }

  if (num_fonts > 0)
    {
      char **indices = alloca (sizeof (char *) * num_fonts);
      Lisp_Object *props = XVECTOR (xfont_scratch_props)->contents;
      Lisp_Object scripts = Qnil;

      for (i = 0; i < ASIZE (xfont_scratch_props); i++)
	props[i] = Qnil;
      for (i = 0; i < num_fonts; i++)
	indices[i] = names[i];
      qsort (indices, num_fonts, sizeof (char *), compare_font_names);

      for (i = 0; i < num_fonts; i++)
	{
	  Lisp_Object entity;

	  if (i > 0 && xstrcasecmp (indices[i - 1], indices[i]) == 0)
	    continue;
	  entity = font_make_entity ();
	  xfont_decode_coding_xlfd (indices[i], -1, buf);
	  if (font_parse_xlfd (buf, entity) < 0)
	    continue;
	  ASET (entity, FONT_TYPE_INDEX, Qx);
	  /* Avoid auto-scaled fonts.  */
	  if (INTEGERP (AREF (entity, FONT_DPI_INDEX))
	      && INTEGERP (AREF (entity, FONT_AVGWIDTH_INDEX))
	      && XINT (AREF (entity, FONT_DPI_INDEX)) != 0
	      && XINT (AREF (entity, FONT_AVGWIDTH_INDEX)) == 0)
	    continue;
	  /* Avoid not-allowed scalable fonts.  */
	  if (NILP (Vscalable_fonts_allowed))
	    {
	      int size = 0;

	      if (INTEGERP (AREF (entity, FONT_SIZE_INDEX)))
		size = XINT (AREF (entity, FONT_SIZE_INDEX));
	      else if (FLOATP (AREF (entity, FONT_SIZE_INDEX)))
		size = XFLOAT_DATA (AREF (entity, FONT_SIZE_INDEX));
	      if (size == 0)
		continue;
	    }
	  else if (CONSP (Vscalable_fonts_allowed))
	    {
	      Lisp_Object tail, elt;

	      for (tail = Vscalable_fonts_allowed; CONSP (tail);
		   tail = XCDR (tail))
		{
		  elt = XCAR (tail);
		  if (STRINGP (elt)
		      && fast_c_string_match_ignore_case (elt, indices[i]) >= 0)
		    break;
		}
	      if (! CONSP (tail))
		continue;
	    }

	  /* Avoid fonts of invalid registry.  */
	  if (NILP (AREF (entity, FONT_REGISTRY_INDEX)))
	    continue;

	  /* Update encoding and repertory if necessary.  */
	  if (! EQ (registry, AREF (entity, FONT_REGISTRY_INDEX)))
	    {
	      registry = AREF (entity, FONT_REGISTRY_INDEX);
	      if (font_registry_charsets (registry, &encoding, &repertory) < 0)
		encoding = NULL;
	    }
	  if (! encoding)
	    /* Unknown REGISTRY, not supported.  */
	    continue;
	  if (repertory)
	    {
	      if (NILP (script)
		  || xfont_chars_supported (chars, NULL, encoding, repertory))
		list = Fcons (entity, list);
	      continue;
	    }
	  if (memcmp (props, &(AREF (entity, FONT_FOUNDRY_INDEX)),
		      sizeof (Lisp_Object) * 7)
	      || ! EQ (AREF (entity, FONT_SPACING_INDEX), props[7]))
	    {
	      memcpy (props, &(AREF (entity, FONT_FOUNDRY_INDEX)),
		      sizeof (Lisp_Object) * 7);
	      props[7] = AREF (entity, FONT_SPACING_INDEX);
	      scripts = xfont_supported_scripts (display, indices[i],
						 xfont_scratch_props, encoding);
	    }
	  if (NILP (script)
	      || ! NILP (Fmemq (script, scripts)))
	    list = Fcons (entity, list);
	}
      XFreeFontNames (names);
    }

  x_uncatch_errors ();
  UNBLOCK_INPUT;

  FONT_ADD_LOG ("xfont-list", build_string (pattern), list);
  return list;
}

static Lisp_Object
xfont_list (Lisp_Object frame, Lisp_Object spec)
{
  FRAME_PTR f = XFRAME (frame);
  Display *display = FRAME_X_DISPLAY_INFO (f)->display;
  Lisp_Object registry, list, val, extra, script;
  int len;
  /* Large enough to contain the longest XLFD (255 bytes) in UTF-8.  */
  char name[512];

  extra = AREF (spec, FONT_EXTRA_INDEX);
  if (CONSP (extra))
    {
      val = assq_no_quit (QCotf, extra);
      if (! NILP (val))
	return Qnil;
      val = assq_no_quit (QClang, extra);
      if (! NILP (val))
	return Qnil;
    }

  registry = AREF (spec, FONT_REGISTRY_INDEX);
  len = font_unparse_xlfd (spec, 0, name, 512);
  if (len < 0 || (len = xfont_encode_coding_xlfd (name)) < 0)
    return Qnil;

  val = assq_no_quit (QCscript, extra);
  script = CDR (val);
  list = xfont_list_pattern (display, name, registry, script);
  if (NILP (list) && NILP (registry))
    {
      /* Try iso10646-1 */
      char *r = name + len - 9;	/* 9 == strlen (iso8859-1) */

      if (r - name + 10 < 256)	/* 10 == strlen (iso10646-1) */
	{
	  strcpy (r, "iso10646-1");
	  list = xfont_list_pattern (display, name, Qiso10646_1, script);
	}
    }
  if (NILP (list) && ! NILP (registry))
    {
      /* Try alternate registries.  */
      Lisp_Object alter;

      if ((alter = Fassoc (SYMBOL_NAME (registry),
			   Vface_alternative_font_registry_alist),
	   CONSP (alter)))
	{
	  /* Pointer to REGISTRY-ENCODING field.  */
	  char *r = name + len - SBYTES (SYMBOL_NAME (registry));

	  for (alter = XCDR (alter); CONSP (alter); alter = XCDR (alter))
	    if (STRINGP (XCAR (alter))
		&& ((r - name) + SBYTES (XCAR (alter))) < 256)
	      {
		strcpy (r, SSDATA (XCAR (alter)));
		list = xfont_list_pattern (display, name, registry, script);
		if (! NILP (list))
		  break;
	      }
	}
    }
  if (NILP (list))
    {
      /* Try alias.  */
      val = assq_no_quit (QCname, AREF (spec, FONT_EXTRA_INDEX));
      if (CONSP (val) && STRINGP (XCDR (val)) && SBYTES (XCDR (val)) < 512)
	{
	  memcpy (name, SDATA (XCDR (val)), SBYTES (XCDR (val)) + 1);
	  if (xfont_encode_coding_xlfd (name) < 0)
	    return Qnil;
	  list = xfont_list_pattern (display, name, registry, script);
	}
    }

  return list;
}

static Lisp_Object
xfont_match (Lisp_Object frame, Lisp_Object spec)
{
  FRAME_PTR f = XFRAME (frame);
  Display *display = FRAME_X_DISPLAY_INFO (f)->display;
  Lisp_Object extra, val, entity;
  char name[512];
  XFontStruct *xfont;
  unsigned long value;

  extra = AREF (spec, FONT_EXTRA_INDEX);
  val = assq_no_quit (QCname, extra);
  if (! CONSP (val) || ! STRINGP (XCDR (val)))
    {
      if (font_unparse_xlfd (spec, 0, name, 512) < 0)
	return Qnil;
    }
  else if (SBYTES (XCDR (val)) < 512)
    memcpy (name, SDATA (XCDR (val)), SBYTES (XCDR (val)) + 1);
  else
    return Qnil;
  if (xfont_encode_coding_xlfd (name) < 0)
    return Qnil;

  BLOCK_INPUT;
  entity = Qnil;
  xfont = XLoadQueryFont (display, name);
  if (xfont)
    {
      if (XGetFontProperty (xfont, XA_FONT, &value))
	{
	  char *s;

	  s = (char *) XGetAtomName (display, (Atom) value);

	  /* If DXPC (a Differential X Protocol Compressor)
	     Ver.3.7 is running, XGetAtomName will return null
	     string.  We must avoid such a name.  */
	  if (*s)
	    {
	      entity = font_make_entity ();
	      ASET (entity, FONT_TYPE_INDEX, Qx);
	      xfont_decode_coding_xlfd (s, -1, name);
	      if (font_parse_xlfd (name, entity) < 0)
		entity = Qnil;
	    }
	  XFree (s);
	}
      XFreeFont (display, xfont);
    }
  UNBLOCK_INPUT;

  FONT_ADD_LOG ("xfont-match", spec, entity);
  return entity;
}

static Lisp_Object
xfont_list_family (Lisp_Object frame)
{
  FRAME_PTR f = XFRAME (frame);
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  char **names;
  int num_fonts, i;
  Lisp_Object list;
  char *last_family IF_LINT (= 0);
  int last_len;

  BLOCK_INPUT;
  x_catch_errors (dpyinfo->display);
  names = XListFonts (dpyinfo->display, "-*-*-*-*-*-*-*-*-*-*-*-*-*-*",
		      0x8000, &num_fonts);
  if (x_had_errors_p (dpyinfo->display))
    {
      /* This error is perhaps due to insufficient memory on X server.
	 Let's just ignore it.  */
      x_clear_errors (dpyinfo->display);
      num_fonts = 0;
    }

  list = Qnil;
  for (i = 0, last_len = 0; i < num_fonts; i++)
    {
      char *p0 = names[i], *p1, buf[512];
      Lisp_Object family;
      int decoded_len;

      p0++;			/* skip the leading '-' */
      while (*p0 && *p0 != '-') p0++; /* skip foundry */
      if (! *p0)
	continue;
      p1 = ++p0;
      while (*p1 && *p1 != '-') p1++; /* find the end of family */
      if (! *p1 || p1 == p0)
	continue;
      if (last_len == p1 - p0
	  && memcmp (last_family, p0, last_len) == 0)
	continue;
      last_len = p1 - p0;
      last_family = p0;

      decoded_len = xfont_decode_coding_xlfd (p0, last_len, buf);
      family = font_intern_prop (p0, decoded_len, 1);
      if (NILP (assq_no_quit (family, list)))
	list = Fcons (family, list);
    }

  XFreeFontNames (names);
  x_uncatch_errors ();
  UNBLOCK_INPUT;

  return list;
}

static Lisp_Object
xfont_open (FRAME_PTR f, Lisp_Object entity, int pixel_size)
{
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Display *display = dpyinfo->display;
  char name[512];
  int len;
  unsigned long value;
  Lisp_Object registry;
  struct charset *encoding, *repertory;
  Lisp_Object font_object, fullname;
  struct font *font;
  XFontStruct *xfont;

  /* At first, check if we know how to encode characters for this
     font.  */
  registry = AREF (entity, FONT_REGISTRY_INDEX);
  if (font_registry_charsets (registry, &encoding, &repertory) < 0)
    {
      FONT_ADD_LOG ("  x:unknown registry", registry, Qnil);
      return Qnil;
    }

  if (XINT (AREF (entity, FONT_SIZE_INDEX)) != 0)
    pixel_size = XINT (AREF (entity, FONT_SIZE_INDEX));
  else if (pixel_size == 0)
    {
      if (FRAME_FONT (f))
	pixel_size = FRAME_FONT (f)->pixel_size;
      else
	pixel_size = 14;
    }
  len = font_unparse_xlfd (entity, pixel_size, name, 512);
  if (len <= 0 || (len = xfont_encode_coding_xlfd (name)) < 0)
    {
      FONT_ADD_LOG ("  x:unparse failed", entity, Qnil);
      return Qnil;
    }

  BLOCK_INPUT;
  x_catch_errors (display);
  xfont = XLoadQueryFont (display, name);
  if (x_had_errors_p (display))
    {
      /* This error is perhaps due to insufficient memory on X server.
	 Let's just ignore it.  */
      x_clear_errors (display);
      xfont = NULL;
    }
  else if (! xfont)
    {
      /* Some version of X lists:
	   -misc-fixed-medium-r-normal--20-*-75-75-c-100-iso8859-1
	   -misc-fixed-medium-r-normal--20-*-100-100-c-100-iso8859-1
	 but can open only:
	   -misc-fixed-medium-r-normal--20-*-100-100-c-100-iso8859-1
	 and
	   -misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1
	 So, we try again with wildcards in RESX and RESY.  */
      Lisp_Object temp;

      temp = copy_font_spec (entity);
      ASET (temp, FONT_DPI_INDEX, Qnil);
      len = font_unparse_xlfd (temp, pixel_size, name, 512);
      if (len <= 0 || (len = xfont_encode_coding_xlfd (name)) < 0)
	{
	  FONT_ADD_LOG ("  x:unparse failed", temp, Qnil);
	  return Qnil;
	}
      xfont = XLoadQueryFont (display, name);
      if (x_had_errors_p (display))
	{
	  /* This error is perhaps due to insufficient memory on X server.
	     Let's just ignore it.  */
	  x_clear_errors (display);
	  xfont = NULL;
	}
    }
  fullname = Qnil;
  /* Try to get the full name of FONT.  */
  if (xfont && XGetFontProperty (xfont, XA_FONT, &value))
    {
      char *p0, *p;
      int dashes = 0;

      p0 = p = (char *) XGetAtomName (FRAME_X_DISPLAY (f), (Atom) value);
      /* Count the number of dashes in the "full name".
	 If it is too few, this isn't really the font's full name,
	 so don't use it.
	 In X11R4, the fonts did not come with their canonical names
	 stored in them.  */
      while (*p)
	{
	  if (*p == '-')
	    dashes++;
	  p++;
	}

      if (dashes >= 13)
	{
	  len = xfont_decode_coding_xlfd (p0, -1, name);
	  fullname = Fdowncase (make_string (name, len));
	}
      XFree (p0);
    }
  x_uncatch_errors ();
  UNBLOCK_INPUT;

  if (! xfont)
    {
      FONT_ADD_LOG ("  x:open failed", build_string (name), Qnil);
      return Qnil;
    }

  font_object = font_make_object (VECSIZE (struct xfont_info),
				  entity, pixel_size);
  ASET (font_object, FONT_TYPE_INDEX, Qx);
  if (STRINGP (fullname))
    {
      font_parse_xlfd (SSDATA (fullname), font_object);
      ASET (font_object, FONT_NAME_INDEX, fullname);
    }
  else
    {
      char buf[512];

      len = xfont_decode_coding_xlfd (name, -1, buf);
      ASET (font_object, FONT_NAME_INDEX, make_string (buf, len));
    }
  ASET (font_object, FONT_FULLNAME_INDEX, fullname);
  ASET (font_object, FONT_FILE_INDEX, Qnil);
  ASET (font_object, FONT_FORMAT_INDEX, Qx);
  font = XFONT_OBJECT (font_object);
  ((struct xfont_info *) font)->xfont = xfont;
  ((struct xfont_info *) font)->display = FRAME_X_DISPLAY (f);
  font->pixel_size = pixel_size;
  font->driver = &xfont_driver;
  font->encoding_charset = encoding->id;
  font->repertory_charset = repertory ? repertory->id : -1;
  font->ascent = xfont->ascent;
  font->descent = xfont->descent;
  font->height = font->ascent + font->descent;
  font->min_width = xfont->min_bounds.width;
  if (xfont->min_bounds.width == xfont->max_bounds.width)
    {
      /* Fixed width font.  */
      font->average_width = font->space_width = xfont->min_bounds.width;
    }
  else
    {
      XCharStruct *pcm;
      XChar2b char2b;
      Lisp_Object val;

      char2b.byte1 = 0x00, char2b.byte2 = 0x20;
      pcm = xfont_get_pcm (xfont, &char2b);
      if (pcm)
	font->space_width = pcm->width;
      else
	font->space_width = 0;

      val = Ffont_get (font_object, QCavgwidth);
      if (INTEGERP (val))
	font->average_width = XINT (val) / 10;
      if (font->average_width < 0)
	font->average_width = - font->average_width;
      else
	{
	  if (font->average_width == 0
	      && encoding->ascii_compatible_p)
	    {
	      int width = font->space_width, n = pcm != NULL;

	      for (char2b.byte2 = 33; char2b.byte2 <= 126; char2b.byte2++)
		if ((pcm = xfont_get_pcm (xfont, &char2b)) != NULL)
		  width += pcm->width, n++;
	      if (n > 0)
		font->average_width = width / n;
	    }
	  if (font->average_width == 0)
	    /* No easy way other than this to get a reasonable
	       average_width.  */
	    font->average_width
	      = (xfont->min_bounds.width + xfont->max_bounds.width) / 2;
	}
    }

  BLOCK_INPUT;
  font->underline_thickness
    = (XGetFontProperty (xfont, XA_UNDERLINE_THICKNESS, &value)
       ? (long) value : 0);
  font->underline_position
    = (XGetFontProperty (xfont, XA_UNDERLINE_POSITION, &value)
       ? (long) value : -1);
  font->baseline_offset
    = (XGetFontProperty (xfont, dpyinfo->Xatom_MULE_BASELINE_OFFSET, &value)
       ? (long) value : 0);
  font->relative_compose
    = (XGetFontProperty (xfont, dpyinfo->Xatom_MULE_RELATIVE_COMPOSE, &value)
       ? (long) value : 0);
  font->default_ascent
    = (XGetFontProperty (xfont, dpyinfo->Xatom_MULE_DEFAULT_ASCENT, &value)
       ? (long) value : 0);
  UNBLOCK_INPUT;

  if (NILP (fullname))
    fullname = AREF (font_object, FONT_NAME_INDEX);
  font->vertical_centering
    = (STRINGP (Vvertical_centering_font_regexp)
       && (fast_string_match_ignore_case
	   (Vvertical_centering_font_regexp, fullname) >= 0));

  return font_object;
}

static void
xfont_close (FRAME_PTR f, struct font *font)
{
  BLOCK_INPUT;
  XFreeFont (FRAME_X_DISPLAY (f), ((struct xfont_info *) font)->xfont);
  UNBLOCK_INPUT;
}

static int
xfont_prepare_face (FRAME_PTR f, struct face *face)
{
  BLOCK_INPUT;
  XSetFont (FRAME_X_DISPLAY (f), face->gc,
	    ((struct xfont_info *) face->font)->xfont->fid);
  UNBLOCK_INPUT;

  return 0;
}

static int
xfont_has_char (Lisp_Object font, int c)
{
  Lisp_Object registry = AREF (font, FONT_REGISTRY_INDEX);
  struct charset *encoding;
  struct charset *repertory = NULL;

  if (EQ (registry, Qiso10646_1))
    {
      encoding = CHARSET_FROM_ID (charset_unicode);
      /* We use a font of `ja' and `ko' adstyle only for a character
	 in JISX0208 and KSC5601 charsets respectively.  */
      if (EQ (AREF (font, FONT_ADSTYLE_INDEX), Qja)
	  && charset_jisx0208 >= 0)
	repertory = CHARSET_FROM_ID (charset_jisx0208);
      else if (EQ (AREF (font, FONT_ADSTYLE_INDEX), Qko)
	       && charset_ksc5601 >= 0)
	repertory = CHARSET_FROM_ID (charset_ksc5601);
    }
  else if (font_registry_charsets (registry, &encoding, &repertory) < 0)
    /* Unknown REGISTRY, not usable.  */
    return 0;
  if (ASCII_CHAR_P (c) && encoding->ascii_compatible_p)
    return 1;
  if (! repertory)
    return -1;
  return (ENCODE_CHAR (repertory, c) != CHARSET_INVALID_CODE (repertory));
}

static unsigned
xfont_encode_char (struct font *font, int c)
{
  XFontStruct *xfont = ((struct xfont_info *) font)->xfont;
  struct charset *charset;
  unsigned code;
  XChar2b char2b;

  charset = CHARSET_FROM_ID (font->encoding_charset);
  code = ENCODE_CHAR (charset, c);
  if (code == CHARSET_INVALID_CODE (charset))
    return FONT_INVALID_CODE;
  if (font->repertory_charset >= 0)
    {
      charset = CHARSET_FROM_ID (font->repertory_charset);
      return (ENCODE_CHAR (charset, c) != CHARSET_INVALID_CODE (charset)
	      ? code : FONT_INVALID_CODE);
    }
  char2b.byte1 = code >> 8;
  char2b.byte2 = code & 0xFF;
  return (xfont_get_pcm (xfont, &char2b) ? code : FONT_INVALID_CODE);
}

static int
xfont_text_extents (struct font *font, unsigned int *code, int nglyphs, struct font_metrics *metrics)
{
  XFontStruct *xfont = ((struct xfont_info *) font)->xfont;
  int width = 0;
  int i, first;

  if (metrics)
    memset (metrics, 0, sizeof (struct font_metrics));
  for (i = 0, first = 1; i < nglyphs; i++)
    {
      XChar2b char2b;
      static XCharStruct *pcm;

      if (code[i] >= 0x10000)
	continue;
      char2b.byte1 = code[i] >> 8, char2b.byte2 = code[i] & 0xFF;
      pcm = xfont_get_pcm (xfont, &char2b);
      if (! pcm)
	continue;
      if (first)
	{
	  if (metrics)
	    {
	      metrics->lbearing = pcm->lbearing;
	      metrics->rbearing = pcm->rbearing;
	      metrics->ascent = pcm->ascent;
	      metrics->descent = pcm->descent;
	    }
	  first = 0;
	}
      else
	{
	  if (metrics)
	    {
	      if (metrics->lbearing > width + pcm->lbearing)
		metrics->lbearing = width + pcm->lbearing;
	      if (metrics->rbearing < width + pcm->rbearing)
		metrics->rbearing = width + pcm->rbearing;
	      if (metrics->ascent < pcm->ascent)
		metrics->ascent = pcm->ascent;
	      if (metrics->descent < pcm->descent)
		metrics->descent = pcm->descent;
	    }
	}
      width += pcm->width;
    }
  if (metrics)
    metrics->width = width;
  return width;
}

static int
xfont_draw (struct glyph_string *s, int from, int to, int x, int y, int with_background)
{
  XFontStruct *xfont = ((struct xfont_info *) s->font)->xfont;
  int len = to - from;
  GC gc = s->gc;
  int i;

  if (s->gc != s->face->gc)
    {
      BLOCK_INPUT;
      XSetFont (s->display, gc, xfont->fid);
      UNBLOCK_INPUT;
    }

  if (xfont->min_byte1 == 0 && xfont->max_byte1 == 0)
    {
      char *str;
      USE_SAFE_ALLOCA;

      SAFE_ALLOCA (str, char *, len);
      for (i = 0; i < len ; i++)
	str[i] = XCHAR2B_BYTE2 (s->char2b + from + i);
      BLOCK_INPUT;
      if (with_background > 0)
	{
	  if (s->padding_p)
	    for (i = 0; i < len; i++)
	      XDrawImageString (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
				gc, x + i, y, str + i, 1);
	  else
	    XDrawImageString (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			      gc, x, y, str, len);
	}
      else
	{
	  if (s->padding_p)
	    for (i = 0; i < len; i++)
	      XDrawString (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			   gc, x + i, y, str + i, 1);
	  else
	    XDrawString (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			 gc, x, y, str, len);
	}
      UNBLOCK_INPUT;
      SAFE_FREE ();
      return s->nchars;
    }

  BLOCK_INPUT;
  if (with_background > 0)
    {
      if (s->padding_p)
	for (i = 0; i < len; i++)
	  XDrawImageString16 (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			      gc, x + i, y, s->char2b + from + i, 1);
      else
	XDrawImageString16 (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			    gc, x, y, s->char2b + from, len);
    }
  else
    {
      if (s->padding_p)
	for (i = 0; i < len; i++)
	  XDrawString16 (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			 gc, x + i, y, s->char2b + from + i, 1);
      else
	XDrawString16 (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
		       gc, x, y, s->char2b + from, len);
    }
  UNBLOCK_INPUT;

  return len;
}

static int
xfont_check (FRAME_PTR f, struct font *font)
{
  struct xfont_info *xfont = (struct xfont_info *) font;

  return (FRAME_X_DISPLAY (f) == xfont->display ? 0 : -1);
}


void
syms_of_xfont (void)
{
  staticpro (&xfont_scripts_cache);
  { /* Here we rely on the fact that syms_of_xfont (via syms_of_font)
       is called fairly late, when QCtest and Qequal are known to be set.  */
    Lisp_Object args[2];
    args[0] = QCtest;
    args[1] = Qequal;
    xfont_scripts_cache = Fmake_hash_table (2, args);
  }
  staticpro (&xfont_scratch_props);
  xfont_scratch_props = Fmake_vector (make_number (8), Qnil);
  xfont_driver.type = Qx;
  register_font_driver (&xfont_driver, NULL);
}
