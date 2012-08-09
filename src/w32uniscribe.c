/* Font backend for the Microsoft W32 Uniscribe API.
   Copyright (C) 2008-2012 Free Software Foundation, Inc.

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
/* Override API version - Uniscribe is only available as standard since
   Windows 2000, though most users of older systems will have it
   since it installs with Internet Explorer 5.0 and other software.
   We only enable the feature if it is available, so there is no chance
   of calling non-existent functions.  */
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x500
#include <windows.h>
#include <usp10.h>
#include <setjmp.h>

#include "lisp.h"
#include "w32term.h"
#include "frame.h"
#include "dispextern.h"
#include "character.h"
#include "charset.h"
#include "composite.h"
#include "fontset.h"
#include "font.h"
#include "w32font.h"

struct uniscribe_font_info
{
  struct w32font_info w32_font;
  SCRIPT_CACHE cache;
};

int uniscribe_available = 0;

/* Defined in w32font.c, since it is required there as well.  */
extern Lisp_Object Quniscribe;
extern Lisp_Object Qopentype;

/* EnumFontFamiliesEx callback.  */
static int CALLBACK add_opentype_font_name_to_list (ENUMLOGFONTEX *,
                                                    NEWTEXTMETRICEX *,
                                                    DWORD, LPARAM);
/* Used by uniscribe_otf_capability.  */
static Lisp_Object otf_features (HDC context, char *table);

static int
memq_no_quit (Lisp_Object elt, Lisp_Object list)
{
  while (CONSP (list) && ! EQ (XCAR (list), elt))
    list = XCDR (list);
  return (CONSP (list));
}


/* Font backend interface implementation.  */
static Lisp_Object
uniscribe_list (Lisp_Object frame, Lisp_Object font_spec)
{
  Lisp_Object fonts = w32font_list_internal (frame, font_spec, 1);
  FONT_ADD_LOG ("uniscribe-list", font_spec, fonts);
  return fonts;
}

static Lisp_Object
uniscribe_match (Lisp_Object frame, Lisp_Object font_spec)
{
  Lisp_Object entity = w32font_match_internal (frame, font_spec, 1);
  FONT_ADD_LOG ("uniscribe-match", font_spec, entity);
  return entity;
}

static Lisp_Object
uniscribe_list_family (Lisp_Object frame)
{
  Lisp_Object list = Qnil;
  LOGFONT font_match_pattern;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  memset (&font_match_pattern, 0, sizeof (font_match_pattern));
  /* Limit enumerated fonts to outline fonts to save time.  */
  font_match_pattern.lfOutPrecision = OUT_OUTLINE_PRECIS;

  dc = get_frame_dc (f);

  EnumFontFamiliesEx (dc, &font_match_pattern,
                      (FONTENUMPROC) add_opentype_font_name_to_list,
                      (LPARAM) &list, 0);
  release_frame_dc (f, dc);

  return list;
}

static Lisp_Object
uniscribe_open (FRAME_PTR f, Lisp_Object font_entity, int pixel_size)
{
  Lisp_Object font_object
    = font_make_object (VECSIZE (struct uniscribe_font_info),
			font_entity, pixel_size);
  struct uniscribe_font_info *uniscribe_font
    = (struct uniscribe_font_info *) XFONT_OBJECT (font_object);

  ASET (font_object, FONT_TYPE_INDEX, Quniscribe);

  if (!w32font_open_internal (f, font_entity, pixel_size, font_object))
    {
      return Qnil;
    }

  /* Initialize the cache for this font.  */
  uniscribe_font->cache = NULL;

  /* Uniscribe backend uses glyph indices.  */
  uniscribe_font->w32_font.glyph_idx = ETO_GLYPH_INDEX;

  /* Mark the format as opentype  */
  uniscribe_font->w32_font.font.props[FONT_FORMAT_INDEX] = Qopentype;
  uniscribe_font->w32_font.font.driver = &uniscribe_font_driver;

  return font_object;
}

static void
uniscribe_close (FRAME_PTR f, struct font *font)
{
  struct uniscribe_font_info *uniscribe_font
    = (struct uniscribe_font_info *) font;

  if (uniscribe_font->cache)
    ScriptFreeCache (&(uniscribe_font->cache));

  w32font_close (f, font);
}

/* Return a list describing which scripts/languages FONT supports by
   which GSUB/GPOS features of OpenType tables.  */
static Lisp_Object
uniscribe_otf_capability (struct font *font)
{
  HDC context;
  HFONT old_font;
  struct frame *f;
  Lisp_Object capability = Fcons (Qnil, Qnil);
  Lisp_Object features;

  f = XFRAME (selected_frame);
  context = get_frame_dc (f);
  old_font = SelectObject (context, FONT_HANDLE (font));

  features = otf_features (context, "GSUB");
  XSETCAR (capability, features);
  features = otf_features (context, "GPOS");
  XSETCDR (capability, features);

  SelectObject (context, old_font);
  release_frame_dc (f, context);

  return capability;
}

/* Uniscribe implementation of shape for font backend.

   Shape text in LGSTRING.  See the docstring of
   `composition-get-gstring' for the format of LGSTRING.  If the
   (N+1)th element of LGSTRING is nil, input of shaping is from the
   1st to (N)th elements.  In each input glyph, FROM, TO, CHAR, and
   CODE are already set.

   This function updates all fields of the input glyphs.  If the
   output glyphs (M) are more than the input glyphs (N), (N+1)th
   through (M)th elements of LGSTRING are updated possibly by making
   a new glyph object and storing it in LGSTRING.  If (M) is greater
   than the length of LGSTRING, nil should be returned.  In that case,
   this function is called again with a larger LGSTRING.  */
static Lisp_Object
uniscribe_shape (Lisp_Object lgstring)
{
  struct font * font;
  struct uniscribe_font_info * uniscribe_font;
  EMACS_UINT nchars;
  int nitems, max_items, i, max_glyphs, done_glyphs;
  wchar_t *chars;
  WORD *glyphs, *clusters;
  SCRIPT_ITEM *items;
  SCRIPT_VISATTR *attributes;
  int *advances;
  GOFFSET *offsets;
  ABC overall_metrics;
  HRESULT result;
  struct frame * f = NULL;
  HDC context = NULL;
  HFONT old_font = NULL;

  CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring), font);
  uniscribe_font = (struct uniscribe_font_info *) font;

  /* Get the chars from lgstring in a form we can use with uniscribe.  */
  max_glyphs = nchars = LGSTRING_GLYPH_LEN (lgstring);
  done_glyphs = 0;
  chars = (wchar_t *) alloca (nchars * sizeof (wchar_t));
  /* FIXME: This loop assumes that characters in the input LGSTRING
     are all inside the BMP.  Need to encode characters beyond the BMP
     as UTF-16.  */
  for (i = 0; i < nchars; i++)
    {
      /* lgstring can be bigger than the number of characters in it, in
	 the case where more glyphs are required to display those characters.
         If that is the case, note the real number of characters.  */
      if (NILP (LGSTRING_GLYPH (lgstring, i)))
	nchars = i;
      else
	chars[i] = LGLYPH_CHAR (LGSTRING_GLYPH (lgstring, i));
    }

  /* First we need to break up the glyph string into runs of glyphs that
     can be treated together.  First try a single run.  */
  max_items = 2;
  items = (SCRIPT_ITEM *) xmalloc (sizeof (SCRIPT_ITEM) * max_items + 1);

  while ((result = ScriptItemize (chars, nchars, max_items, NULL, NULL,
				  items, &nitems)) == E_OUTOFMEMORY)
    {
      /* If that wasn't enough, keep trying with one more run.  */
      max_items++;
      items = (SCRIPT_ITEM *) xrealloc (items,
					sizeof (SCRIPT_ITEM) * max_items + 1);
    }

  if (FAILED (result))
    {
      xfree (items);
      return Qnil;
    }

  glyphs = alloca (max_glyphs * sizeof (WORD));
  clusters = alloca (nchars * sizeof (WORD));
  attributes = alloca (max_glyphs * sizeof (SCRIPT_VISATTR));
  advances = alloca (max_glyphs * sizeof (int));
  offsets = alloca (max_glyphs * sizeof (GOFFSET));

  for (i = 0; i < nitems; i++)
    {
      int nglyphs, nchars_in_run;
      nchars_in_run = items[i+1].iCharPos - items[i].iCharPos;
      /* Force ScriptShape to generate glyphs in the same order as
	 they are in the input LGSTRING, which is in the logical
	 order.  */
      items[i].a.fLogicalOrder = 1;

      /* Context may be NULL here, in which case the cache should be
         used without needing to select the font.  */
      result = ScriptShape (context, &(uniscribe_font->cache),
			    chars + items[i].iCharPos, nchars_in_run,
			    max_glyphs - done_glyphs, &(items[i].a),
			    glyphs, clusters, attributes, &nglyphs);

      if (result == E_PENDING && !context)
	{
	  /* This assumes the selected frame is on the same display as the
	     one we are drawing.  It would be better for the frame to be
	     passed in.  */
	  f = XFRAME (selected_frame);
	  context = get_frame_dc (f);
	  old_font = SelectObject (context, FONT_HANDLE (font));

	  result = ScriptShape (context, &(uniscribe_font->cache),
				chars + items[i].iCharPos, nchars_in_run,
				max_glyphs - done_glyphs, &(items[i].a),
				glyphs, clusters, attributes, &nglyphs);
	}

      if (result == E_OUTOFMEMORY)
	{
	  /* Need a bigger lgstring.  */
	  lgstring = Qnil;
	  break;
	}
      else if (FAILED (result))
	{
	  /* Can't shape this run - return results so far if any.  */
	  break;
	}
      else if (items[i].a.fNoGlyphIndex)
	{
	  /* Glyph indices not supported by this font (or OS), means we
	     can't really do any meaningful shaping.  */
	  break;
	}
      else
	{
	  result = ScriptPlace (context, &(uniscribe_font->cache),
				glyphs, nglyphs, attributes, &(items[i].a),
				advances, offsets, &overall_metrics);
	  if (result == E_PENDING && !context)
	    {
	      /* Cache not complete...  */
	      f = XFRAME (selected_frame);
	      context = get_frame_dc (f);
	      old_font = SelectObject (context, FONT_HANDLE (font));

	      result = ScriptPlace (context, &(uniscribe_font->cache),
				    glyphs, nglyphs, attributes, &(items[i].a),
				    advances, offsets, &overall_metrics);
	    }
          if (SUCCEEDED (result))
	    {
	      int j, from, to;

	      from = 0;
	      to = from;

	      for (j = 0; j < nglyphs; j++)
		{
		  int lglyph_index = j + done_glyphs;
		  Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, lglyph_index);
		  ABC char_metric;
		  unsigned gl;

		  if (NILP (lglyph))
		    {
		      lglyph = Fmake_vector (make_number (LGLYPH_SIZE), Qnil);
		      LGSTRING_SET_GLYPH (lgstring, lglyph_index, lglyph);
		    }
		  /* Copy to a 32-bit data type to shut up the
		     compiler warning in LGLYPH_SET_CODE about
		     comparison being always false.  */
		  gl = glyphs[j];
		  LGLYPH_SET_CODE (lglyph, gl);

		  /* Detect clusters, for linking codes back to
		     characters.  */
		  if (attributes[j].fClusterStart)
		    {
		      while (from < nchars_in_run && clusters[from] < j)
			from++;
		      if (from >= nchars_in_run)
			from = to = nchars_in_run - 1;
		      else
			{
			  int k;
			  to = nchars_in_run - 1;
			  for (k = from + 1; k < nchars_in_run; k++)
			    {
			      if (clusters[k] > j)
				{
				  to = k - 1;
				  break;
				}
			    }
			}
		    }

		  LGLYPH_SET_CHAR (lglyph, chars[items[i].iCharPos
						 + from]);
		  LGLYPH_SET_FROM (lglyph, items[i].iCharPos + from);
		  LGLYPH_SET_TO (lglyph, items[i].iCharPos + to);

		  /* Metrics.  */
		  LGLYPH_SET_WIDTH (lglyph, advances[j]);
		  LGLYPH_SET_ASCENT (lglyph, font->ascent);
		  LGLYPH_SET_DESCENT (lglyph, font->descent);

		  result = ScriptGetGlyphABCWidth (context,
						   &(uniscribe_font->cache),
						   glyphs[j], &char_metric);
		  if (result == E_PENDING && !context)
		    {
		      /* Cache incomplete... */
		      f = XFRAME (selected_frame);
		      context = get_frame_dc (f);
		      old_font = SelectObject (context, FONT_HANDLE (font));
		      result = ScriptGetGlyphABCWidth (context,
						       &(uniscribe_font->cache),
						       glyphs[j], &char_metric);
		    }

		  if (SUCCEEDED (result))
		    {
		      LGLYPH_SET_LBEARING (lglyph, char_metric.abcA);
		      LGLYPH_SET_RBEARING (lglyph, (char_metric.abcA
						    + char_metric.abcB));
		    }
		  else
		    {
		      LGLYPH_SET_LBEARING (lglyph, 0);
		      LGLYPH_SET_RBEARING (lglyph, advances[j]);
		    }

		  if (offsets[j].du || offsets[j].dv)
		    {
		      Lisp_Object vec;
		      vec = Fmake_vector (make_number (3), Qnil);
		      ASET (vec, 0, make_number (offsets[j].du));
		      ASET (vec, 1, make_number (offsets[j].dv));
		      /* Based on what ftfont.c does... */
		      ASET (vec, 2, make_number (advances[j]));
		      LGLYPH_SET_ADJUSTMENT (lglyph, vec);
		    }
		  else
		    LGLYPH_SET_ADJUSTMENT (lglyph, Qnil);
		}
	    }
	}
      done_glyphs += nglyphs;
    }

  xfree (items);

  if (context)
    {
      SelectObject (context, old_font);
      release_frame_dc (f, context);
    }

  if (NILP (lgstring))
    return Qnil;
  else
    return make_number (done_glyphs);
}

/* Uniscribe implementation of encode_char for font backend.
   Return a glyph code of FONT for character C (Unicode code point).
   If FONT doesn't have such a glyph, return FONT_INVALID_CODE.  */
static unsigned
uniscribe_encode_char (struct font *font, int c)
{
  HDC context = NULL;
  struct frame *f = NULL;
  HFONT old_font = NULL;
  unsigned code = FONT_INVALID_CODE;
  wchar_t ch[2];
  int len;
  SCRIPT_ITEM* items;
  int nitems;
  struct uniscribe_font_info *uniscribe_font
    = (struct uniscribe_font_info *)font;

  if (c < 0x10000)
    {
      ch[0] = (wchar_t) c;
      len = 1;
    }
  else
    {
      DWORD surrogate = c - 0x10000;

      /* High surrogate: U+D800 - U+DBFF.  */
      ch[0] = 0xD800 + ((surrogate >> 10) & 0x03FF);
      /* Low surrogate: U+DC00 - U+DFFF.  */
      ch[1] = 0xDC00 + (surrogate & 0x03FF);
      len = 2;
    }

  /* Non BMP characters must be handled by the uniscribe shaping
     engine as GDI functions (except blindly displaying lines of
     Unicode text) and the promising looking ScriptGetCMap do not
     convert surrogate pairs to glyph indexes correctly.  */
    {
      items = (SCRIPT_ITEM *) alloca (sizeof (SCRIPT_ITEM) * 2 + 1);
      if (SUCCEEDED (ScriptItemize (ch, len, 2, NULL, NULL, items, &nitems)))
	{
	  HRESULT result;
          /* Surrogates seem to need 2 here, even though only one glyph is
	     returned.  Indic characters can also produce 2 or more glyphs for
	     a single code point, but they need to use uniscribe_shape
	     above for correct display.  */
          WORD glyphs[2], clusters[2];
          SCRIPT_VISATTR attrs[2];
          int nglyphs;

	  /* Force ScriptShape to generate glyphs in the logical
	     order.  */
	  items[0].a.fLogicalOrder = 1;

          result = ScriptShape (context, &(uniscribe_font->cache),
                                ch, len, 2, &(items[0].a),
                                glyphs, clusters, attrs, &nglyphs);

          if (result == E_PENDING)
            {
              /* Use selected frame until API is updated to pass
                 the frame.  */
              f = XFRAME (selected_frame);
              context = get_frame_dc (f);
              old_font = SelectObject (context, FONT_HANDLE (font));
              result = ScriptShape (context, &(uniscribe_font->cache),
                                    ch, len, 2, &(items[0].a),
                                    glyphs, clusters, attrs, &nglyphs);
            }

          if (SUCCEEDED (result) && nglyphs == 1)
            {
	      /* Some fonts return .notdef glyphs instead of failing.
	         (TrueType spec reserves glyph code 0 for .notdef)  */
	      if (glyphs[0])
		code = glyphs[0];
            }
          else if (SUCCEEDED (result) || result == E_OUTOFMEMORY)
            {
              /* This character produces zero or more than one glyph
                 when shaped. But we still need the return from here
                 to be valid for the shaping engine to be invoked
                 later.  */
              result = ScriptGetCMap (context, &(uniscribe_font->cache),
                                      ch, len, 0, glyphs);
              if (SUCCEEDED (result) && glyphs[0])
                code = glyphs[0];
            }
	}
    }
    if (context)
      {
	SelectObject (context, old_font);
	release_frame_dc (f, context);
      }

    return code;
}

/*
   Shared with w32font:
   Lisp_Object uniscribe_get_cache (Lisp_Object frame);
   void uniscribe_free_entity (Lisp_Object font_entity);
   int uniscribe_has_char (Lisp_Object entity, int c);
   int uniscribe_text_extents (struct font *font, unsigned *code,
                               int nglyphs, struct font_metrics *metrics);
   int uniscribe_draw (struct glyph_string *s, int from, int to,
                       int x, int y, int with_background);

   Unused:
   int uniscribe_prepare_face (FRAME_PTR f, struct face *face);
   void uniscribe_done_face (FRAME_PTR f, struct face *face);
   int uniscribe_get_bitmap (struct font *font, unsigned code,
                             struct font_bitmap *bitmap, int bits_per_pixel);
   void uniscribe_free_bitmap (struct font *font, struct font_bitmap *bitmap);
   void * uniscribe_get_outline (struct font *font, unsigned code);
   void uniscribe_free_outline (struct font *font, void *outline);
   int uniscribe_anchor_point (struct font *font, unsigned code,
                               int index, int *x, int *y);
   int uniscribe_start_for_frame (FRAME_PTR f);
   int uniscribe_end_for_frame (FRAME_PTR f);

*/


/* Callback function for EnumFontFamiliesEx.
   Adds the name of opentype fonts to a Lisp list (passed in as the
   lParam arg). */
static int CALLBACK
add_opentype_font_name_to_list (ENUMLOGFONTEX *logical_font,
				NEWTEXTMETRICEX *physical_font,
				DWORD font_type, LPARAM list_object)
{
  Lisp_Object* list = (Lisp_Object *) list_object;
  Lisp_Object family;

  /* Skip vertical fonts (intended only for printing)  */
  if (logical_font->elfLogFont.lfFaceName[0] == '@')
    return 1;

  /* Skip non opentype fonts.  Count old truetype fonts as opentype,
     as some of them do contain GPOS and GSUB data that Uniscribe
     can make use of.  */
  if (!(physical_font->ntmTm.ntmFlags & NTMFLAGS_OPENTYPE)
      && font_type != TRUETYPE_FONTTYPE)
    return 1;

  /* Skip fonts that have no Unicode coverage.  */
  if (!physical_font->ntmFontSig.fsUsb[3]
      && !physical_font->ntmFontSig.fsUsb[2]
      && !physical_font->ntmFontSig.fsUsb[1]
      && !(physical_font->ntmFontSig.fsUsb[0] & 0x3fffffff))
    return 1;

  family = intern_font_name (logical_font->elfLogFont.lfFaceName);
  if (! memq_no_quit (family, *list))
    *list = Fcons (family, *list);

  return 1;
}


/* :otf property handling.
   Since the necessary Uniscribe APIs for getting font tag information
   are only available in Vista, we need to parse the font data directly
   according to the OpenType Specification.  */

/* Push into DWORD backwards to cope with endianness.  */
#define OTF_TAG(STR)                                          \
  ((STR[3] << 24) | (STR[2] << 16) | (STR[1] << 8) | STR[0])

#define OTF_INT16_VAL(TABLE, OFFSET, PTR)		     \
  do {							     \
    BYTE temp, data[2];					     \
    if (GetFontData (context, TABLE, OFFSET, data, 2) != 2)  \
      goto font_table_error;				     \
    temp = data[0], data[0] = data[1], data[1] = temp;	     \
    memcpy (PTR, data, 2);				     \
  } while (0)

/* Do not reverse the bytes, because we will compare with a OTF_TAG value
   that has them reversed already.  */
#define OTF_DWORDTAG_VAL(TABLE, OFFSET, PTR)                    \
  do {								\
    if (GetFontData (context, TABLE, OFFSET, PTR, 4) != 4)	\
      goto font_table_error;					\
  } while (0)

#define OTF_TAG_VAL(TABLE, OFFSET, STR)			     \
  do {							     \
    if (GetFontData (context, TABLE, OFFSET, STR, 4) != 4)   \
      goto font_table_error;				     \
    STR[4] = '\0';                                           \
  } while (0)

#define SNAME(VAL) SDATA (SYMBOL_NAME (VAL))

/* Check if font supports the otf script/language/features specified.
   OTF_SPEC is in the format
     (script lang [(gsub_feature ...)|nil] [(gpos_feature ...)]?) */
int
uniscribe_check_otf (LOGFONT *font, Lisp_Object otf_spec)
{
  Lisp_Object script, lang, rest;
  Lisp_Object features[2];
  DWORD feature_tables[2];
  DWORD script_tag, default_script, lang_tag = 0;
  struct frame * f;
  HDC context;
  HFONT check_font, old_font;
  int i, retval = 0;
  struct gcpro gcpro1;

  /* Check the spec is in the right format.  */
  if (!CONSP (otf_spec) || XINT (Flength (otf_spec)) < 3)
    return 0;

  /* Break otf_spec into its components.  */
  script = XCAR (otf_spec);
  rest = XCDR (otf_spec);

  lang = XCAR (rest);
  rest = XCDR (rest);

  features[0] = XCAR (rest);
  rest = XCDR (rest);
  if (NILP (rest))
    features[1] = Qnil;
  else
    features[1] = XCAR (rest);

  /* Set up tags we will use in the search.  */
  feature_tables[0] = OTF_TAG ("GSUB");
  feature_tables[1] = OTF_TAG ("GPOS");
  default_script = OTF_TAG ("DFLT");
  if (NILP (script))
    script_tag = default_script;
  else
    script_tag = OTF_TAG (SNAME (script));
  if (!NILP (lang))
    lang_tag = OTF_TAG (SNAME (lang));

  /* Set up graphics context so we can use the font.  */
  f = XFRAME (selected_frame);
  context = get_frame_dc (f);
  check_font = CreateFontIndirect (font);
  old_font = SelectObject (context, check_font);

  /* Everything else is contained within otf_spec so should get
     marked along with it.  */
  GCPRO1 (otf_spec);

  /* Scan GSUB and GPOS tables.  */
  for (i = 0; i < 2; i++)
    {
      int j, n_match_features;
      unsigned short scriptlist_table, feature_table, n_scripts;
      unsigned short script_table, langsys_table, n_langs;
      unsigned short feature_index, n_features;
      DWORD tbl = feature_tables[i];

      /* Skip if no features requested from this table.  */
      if (NILP (features[i]))
	continue;

      /* If features is not a cons, this font spec is messed up.  */
      if (!CONSP (features[i]))
	goto no_support;

      /* Read GPOS/GSUB header.  */
      OTF_INT16_VAL (tbl, 4, &scriptlist_table);
      OTF_INT16_VAL (tbl, 6, &feature_table);
      OTF_INT16_VAL (tbl, scriptlist_table, &n_scripts);

      /* Find the appropriate script table.  */
      script_table = 0;
      for (j = 0; j < n_scripts; j++)
	{
	  DWORD script_id;
	  OTF_DWORDTAG_VAL (tbl, scriptlist_table + 2 + j * 6, &script_id);
	  if (script_id == script_tag)
	    {
	      OTF_INT16_VAL (tbl, scriptlist_table + 6 + j * 6, &script_table);
	      break;
	    }
#if 0	  /* Causes false positives.  */
	  /* If there is a DFLT script defined in the font, use it
	     if the specified script is not found.  */
	  else if (script_id == default_script)
	    OTF_INT16_VAL (tbl, scriptlist_table + 6 + j * 6, &script_table);
#endif
	}
      /* If no specific or default script table was found, then this font
	 does not support the script.  */
      if (!script_table)
	goto no_support;

      /* Offset is from beginning of scriptlist_table.  */
      script_table += scriptlist_table;

      /* Get default langsys table.  */
      OTF_INT16_VAL (tbl, script_table, &langsys_table);

      /* If lang was specified, see if font contains a specific entry.  */
      if (!NILP (lang))
	{
	  OTF_INT16_VAL (tbl, script_table + 2, &n_langs);

	  for (j = 0; j < n_langs; j++)
	    {
	      DWORD lang_id;
	      OTF_DWORDTAG_VAL (tbl, script_table + 4 + j * 6, &lang_id);
	      if (lang_id == lang_tag)
		{
		  OTF_INT16_VAL (tbl, script_table + 8 + j * 6, &langsys_table);
		  break;
		}
	    }
	}

      if (!langsys_table)
	goto no_support;

      /* Offset is from beginning of script table.  */
      langsys_table += script_table;

      /* Check the features.  Features may contain nil according to
	 documentation in font_prop_validate_otf, so count them.  */
      n_match_features = 0;
      for (rest = features[i]; CONSP (rest); rest = XCDR (rest))
	{
	  Lisp_Object feature = XCAR (rest);
	  if (!NILP (feature))
	    n_match_features++;
	}

      /* If there are no features to check, skip checking.  */
      if (!n_match_features)
	continue;

      /* First check required feature (if any).  */
      OTF_INT16_VAL (tbl, langsys_table + 2, &feature_index);
      if (feature_index != 0xFFFF)
	{
	  char feature_id[5];
	  OTF_TAG_VAL (tbl, feature_table + 2 + feature_index * 6, feature_id);
	  OTF_TAG_VAL (tbl, feature_table + 2 + feature_index * 6, feature_id);
	  /* Assume no duplicates in the font table. This allows us to mark
	     the features off by simply decrementing a counter.  */
	  if (!NILP (Fmemq (intern (feature_id), features[i])))
	    n_match_features--;
	}
      /* Now check all the other features.  */
      OTF_INT16_VAL (tbl, langsys_table + 4, &n_features);
      for (j = 0; j < n_features; j++)
	{
	  char feature_id[5];
	  OTF_INT16_VAL (tbl, langsys_table + 6 + j * 2, &feature_index);
	  OTF_TAG_VAL (tbl, feature_table + 2 + feature_index * 6, feature_id);
	  /* Assume no duplicates in the font table. This allows us to mark
	     the features off by simply decrementing a counter.  */
	  if (!NILP (Fmemq (intern (feature_id), features[i])))
	    n_match_features--;
	}

      if (n_match_features > 0)
	goto no_support;
    }

  retval = 1;

 no_support:
 font_table_error:
  /* restore graphics context.  */
  SelectObject (context, old_font);
  DeleteObject (check_font);
  release_frame_dc (f, context);

  return retval;
}

static Lisp_Object
otf_features (HDC context, char *table)
{
  Lisp_Object script_list = Qnil;
  unsigned short scriptlist_table, n_scripts, feature_table;
  DWORD tbl = OTF_TAG (table);
  int i, j, k;

  /* Look for scripts in the table.  */
  OTF_INT16_VAL (tbl, 4, &scriptlist_table);
  OTF_INT16_VAL (tbl, 6, &feature_table);
  OTF_INT16_VAL (tbl, scriptlist_table, &n_scripts);

  for (i = 0; i < n_scripts; i++)
    {
      char script[5], lang[5];
      unsigned short script_table, lang_count, langsys_table, feature_count;
      Lisp_Object script_tag, langsys_list, langsys_tag, feature_list;
      unsigned short record_offset = scriptlist_table + 2 + i * 6;
      OTF_TAG_VAL (tbl, record_offset, script);
      OTF_INT16_VAL (tbl, record_offset + 4, &script_table);

      /* Offset is from beginning of script table.  */
      script_table += scriptlist_table;

      script_tag = intern (script);
      langsys_list = Qnil;

      /* Optional default lang.  */
      OTF_INT16_VAL (tbl, script_table, &langsys_table);
      if (langsys_table)
	{
	  /* Offset is from beginning of script table.  */
	  langsys_table += script_table;

	  langsys_tag = Qnil;
	  feature_list = Qnil;
	  OTF_INT16_VAL (tbl, langsys_table + 4, &feature_count);
	  for (k = 0; k < feature_count; k++)
	    {
	      char feature[5];
	      unsigned short index;
	      OTF_INT16_VAL (tbl, langsys_table + 6 + k * 2, &index);
	      OTF_TAG_VAL (tbl, feature_table + 2 + index * 6, feature);
	      feature_list = Fcons (intern (feature), feature_list);
	    }
	  langsys_list = Fcons (Fcons (langsys_tag, feature_list),
				langsys_list);
	}

      /* List of supported languages.  */
      OTF_INT16_VAL (tbl, script_table + 2, &lang_count);

      for (j = 0; j < lang_count; j++)
	{
	  record_offset = script_table + 4 + j * 6;
	  OTF_TAG_VAL (tbl, record_offset, lang);
	  OTF_INT16_VAL (tbl, record_offset + 4, &langsys_table);

	  /* Offset is from beginning of script table.  */
	  langsys_table += script_table;

	  langsys_tag = intern (lang);
	  feature_list = Qnil;
	  OTF_INT16_VAL (tbl, langsys_table + 4, &feature_count);
	  for (k = 0; k < feature_count; k++)
	    {
	      char feature[5];
	      unsigned short index;
	      OTF_INT16_VAL (tbl, langsys_table + 6 + k * 2, &index);
	      OTF_TAG_VAL (tbl, feature_table + 2 + index * 6, feature);
	      feature_list = Fcons (intern (feature), feature_list);
	    }
	  langsys_list = Fcons (Fcons (langsys_tag, feature_list),
				langsys_list);

	}

      script_list = Fcons (Fcons (script_tag, langsys_list), script_list);
    }

  return script_list;

font_table_error:
  return Qnil;
}

#undef OTF_INT16_VAL
#undef OTF_TAG_VAL
#undef OTF_TAG


struct font_driver uniscribe_font_driver =
  {
    0, /* Quniscribe */
    0, /* case insensitive */
    w32font_get_cache,
    uniscribe_list,
    uniscribe_match,
    uniscribe_list_family,
    NULL, /* free_entity */
    uniscribe_open,
    uniscribe_close,
    NULL, /* prepare_face */
    NULL, /* done_face */
    w32font_has_char,
    uniscribe_encode_char,
    w32font_text_extents,
    w32font_draw,
    NULL, /* get_bitmap */
    NULL, /* free_bitmap */
    NULL, /* get_outline */
    NULL, /* free_outline */
    NULL, /* anchor_point */
    uniscribe_otf_capability, /* Defined so (font-get FONTOBJ :otf) works.  */
    NULL, /* otf_drive - use shape instead.  */
    NULL, /* start_for_frame */
    NULL, /* end_for_frame */
    uniscribe_shape,
    NULL, /* check */
    NULL, /* get_variation_glyphs */
    NULL, /* filter_properties */
    NULL, /* cached_font_ok */
  };

/* Note that this should be called at every startup, not just when dumping,
   as it needs to test for the existence of the Uniscribe library.  */
void
syms_of_w32uniscribe (void)
{
  HMODULE uniscribe;

  /* Don't init uniscribe when dumping */
  if (!initialized)
    return;

  /* Don't register if uniscribe is not available.  */
  uniscribe = GetModuleHandle ("usp10");
  if (!uniscribe)
    return;

  uniscribe_font_driver.type = Quniscribe;
  uniscribe_available = 1;

  register_font_driver (&uniscribe_font_driver, NULL);
}
