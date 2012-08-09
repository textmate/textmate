/* Font backend for the Microsoft W32 API.
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

#include <config.h>
#include <windows.h>
#include <math.h>
#include <ctype.h>
#include <commdlg.h>
#include <setjmp.h>

#include "lisp.h"
#include "w32term.h"
#include "frame.h"
#include "dispextern.h"
#include "character.h"
#include "charset.h"
#include "coding.h"
#include "fontset.h"
#include "font.h"
#include "w32font.h"

/* Cleartype available on Windows XP, cleartype_natural from XP SP1.
   The latter does not try to fit cleartype smoothed fonts into the
   same bounding box as the non-antialiased version of the font.
 */
#ifndef CLEARTYPE_QUALITY
#define CLEARTYPE_QUALITY 5
#endif
#ifndef CLEARTYPE_NATURAL_QUALITY
#define CLEARTYPE_NATURAL_QUALITY 6
#endif

/* VIETNAMESE_CHARSET and JOHAB_CHARSET are not defined in some versions
   of MSVC headers.  */
#ifndef VIETNAMESE_CHARSET
#define VIETNAMESE_CHARSET 163
#endif
#ifndef JOHAB_CHARSET
#define JOHAB_CHARSET 130
#endif

Lisp_Object Qgdi;
Lisp_Object Quniscribe;
static Lisp_Object QCformat;
static Lisp_Object Qmonospace, Qsansserif, Qmono, Qsans, Qsans_serif;
static Lisp_Object Qserif, Qscript, Qdecorative;
static Lisp_Object Qraster, Qoutline, Qunknown;

/* antialiasing  */
extern Lisp_Object Qnone; /* reuse from w32fns.c  */
static Lisp_Object Qstandard, Qsubpixel, Qnatural;

/* languages */
static Lisp_Object Qzh;

/* scripts */
static Lisp_Object Qgreek, Qcoptic, Qcyrillic, Qarmenian, Qhebrew;
static Lisp_Object Qarabic, Qsyriac, Qnko, Qthaana, Qdevanagari, Qbengali;
static Lisp_Object Qgurmukhi, Qgujarati, Qoriya, Qtamil, Qtelugu;
static Lisp_Object Qkannada, Qmalayalam, Qsinhala, Qthai, Qlao;
static Lisp_Object Qtibetan, Qmyanmar, Qgeorgian, Qhangul, Qethiopic;
static Lisp_Object Qcherokee, Qcanadian_aboriginal, Qogham, Qrunic;
static Lisp_Object Qkhmer, Qmongolian, Qsymbol, Qbraille, Qhan;
static Lisp_Object Qideographic_description, Qcjk_misc, Qkana, Qbopomofo;
static Lisp_Object Qkanbun, Qyi, Qbyzantine_musical_symbol;
static Lisp_Object Qmusical_symbol, Qmathematical, Qcham, Qphonetic;
/* Not defined in characters.el, but referenced in fontset.el.  */
static Lisp_Object Qbalinese, Qbuginese, Qbuhid, Qcuneiform, Qcypriot;
static Lisp_Object Qdeseret, Qglagolitic, Qgothic, Qhanunoo, Qkharoshthi;
static Lisp_Object Qlimbu, Qlinear_b, Qold_italic, Qold_persian, Qosmanya;
static Lisp_Object Qphags_pa, Qphoenician, Qshavian, Qsyloti_nagri;
static Lisp_Object Qtagalog, Qtagbanwa, Qtai_le, Qtifinagh, Qugaritic;

/* W32 charsets: for use in Vw32_charset_info_alist.  */
static Lisp_Object Qw32_charset_ansi, Qw32_charset_default;
static Lisp_Object Qw32_charset_symbol, Qw32_charset_shiftjis;
static Lisp_Object Qw32_charset_hangeul, Qw32_charset_gb2312;
static Lisp_Object Qw32_charset_chinesebig5, Qw32_charset_oem;
static Lisp_Object Qw32_charset_easteurope, Qw32_charset_turkish;
static Lisp_Object Qw32_charset_baltic, Qw32_charset_russian;
static Lisp_Object Qw32_charset_arabic, Qw32_charset_greek;
static Lisp_Object Qw32_charset_hebrew, Qw32_charset_vietnamese;
static Lisp_Object Qw32_charset_thai, Qw32_charset_johab, Qw32_charset_mac;

/* Font spacing symbols - defined in font.c.  */
extern Lisp_Object Qc, Qp, Qm;

static void fill_in_logfont (FRAME_PTR, LOGFONT *, Lisp_Object);

static BYTE w32_antialias_type (Lisp_Object);
static Lisp_Object lispy_antialias_type (BYTE);

static Lisp_Object font_supported_scripts (FONTSIGNATURE *);
static int w32font_full_name (LOGFONT *, Lisp_Object, int, char *, int);
static void compute_metrics (HDC, struct w32font_info *, unsigned int,
                             struct w32_metric_cache *);

static Lisp_Object w32_registry (LONG, DWORD);

/* EnumFontFamiliesEx callbacks.  */
static int CALLBACK add_font_entity_to_list (ENUMLOGFONTEX *,
                                             NEWTEXTMETRICEX *,
                                             DWORD, LPARAM);
static int CALLBACK add_one_font_entity_to_list (ENUMLOGFONTEX *,
                                                 NEWTEXTMETRICEX *,
                                                 DWORD, LPARAM);
static int CALLBACK add_font_name_to_list (ENUMLOGFONTEX *,
                                           NEWTEXTMETRICEX *,
                                           DWORD, LPARAM);

/* struct passed in as LPARAM arg to EnumFontFamiliesEx, for keeping track
   of what we really want.  */
struct font_callback_data
{
  /* The logfont we are matching against. EnumFontFamiliesEx only matches
     face name and charset, so we need to manually match everything else
     in the callback function.  */
  LOGFONT pattern;
  /* The original font spec or entity.  */
  Lisp_Object orig_font_spec;
  /* The frame the font is being loaded on.  */
  Lisp_Object frame;
  /* The list to add matches to.  */
  Lisp_Object list;
  /* Whether to match only opentype fonts.  */
  int opentype_only;
};

/* Handles the problem that EnumFontFamiliesEx will not return all
   style variations if the font name is not specified.  */
static void list_all_matching_fonts (struct font_callback_data *);

static BOOL g_b_init_is_w9x;
static BOOL g_b_init_get_outline_metrics_w;
static BOOL g_b_init_get_text_metrics_w;
static BOOL g_b_init_get_glyph_outline_w;
static BOOL g_b_init_get_glyph_outline_w;

typedef UINT (WINAPI * GetOutlineTextMetricsW_Proc) (
   HDC hdc,
   UINT cbData,
   LPOUTLINETEXTMETRICW lpotmw);
typedef BOOL (WINAPI * GetTextMetricsW_Proc) (
   HDC hdc,
   LPTEXTMETRICW lptmw);
typedef DWORD (WINAPI * GetGlyphOutlineW_Proc) (
   HDC hdc,
   UINT uChar,
   UINT uFormat,
   LPGLYPHMETRICS lpgm,
   DWORD cbBuffer,
   LPVOID lpvBuffer,
   const MAT2 *lpmat2);

/* Several "wide" functions we use to support the font backends are
   unavailable on Windows 9X, unless UNICOWS.DLL is installed (their
   versions in the default libraries are non-functional stubs).  On NT
   and later systems, these functions are in GDI32.DLL.  The following
   helper function attempts to load UNICOWS.DLL on Windows 9X, and
   refuses to let Emacs start up if that library is not found.  On NT
   and later versions, it simply loads GDI32.DLL, which should always
   be available.  */
static HMODULE
w32_load_unicows_or_gdi32 (void)
{
  static BOOL is_9x = 0;
  OSVERSIONINFO os_ver;
  HMODULE ret;
  if (g_b_init_is_w9x == 0)
    {
      g_b_init_is_w9x = 1;
      ZeroMemory (&os_ver, sizeof (OSVERSIONINFO));
      os_ver.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
      if (GetVersionEx (&os_ver))
	is_9x = (os_ver.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS);
    }
  if (is_9x)
    {
      ret = LoadLibrary ("Unicows.dll");
      if (!ret)
	{
	  int button;

	  button = MessageBox (NULL,
			       "Emacs cannot load the UNICOWS.DLL library.\n"
			       "This library is essential for using Emacs\n"
			       "on this system.  You need to install it.\n\n"
			       "However, you can still use Emacs by invoking\n"
			       "it with the '-nw' command-line option.\n\n"
			       "Emacs will exit when you click OK.",
			       "Emacs cannot load UNICOWS.DLL",
			       MB_ICONERROR | MB_TASKMODAL
			       | MB_SETFOREGROUND | MB_OK);
	  switch (button)
	    {
	    case IDOK:
	    default:
	      exit (1);
	    }
	}
    }
  else
    ret = LoadLibrary ("Gdi32.dll");
  return ret;
}

/* The following 3 functions call the problematic "wide" APIs via
   function pointers, to avoid linking against the non-standard
   libunicows on W9X.  */
static UINT WINAPI
get_outline_metrics_w(HDC hdc, UINT cbData, LPOUTLINETEXTMETRICW lpotmw)
{
  static GetOutlineTextMetricsW_Proc s_pfn_Get_Outline_Text_MetricsW = NULL;
  HMODULE hm_unicows = NULL;
  if (g_b_init_get_outline_metrics_w == 0)
    {
      g_b_init_get_outline_metrics_w = 1;
      hm_unicows = w32_load_unicows_or_gdi32 ();
      if (hm_unicows)
	s_pfn_Get_Outline_Text_MetricsW = (GetOutlineTextMetricsW_Proc)
	  GetProcAddress (hm_unicows, "GetOutlineTextMetricsW");
    }
  if (s_pfn_Get_Outline_Text_MetricsW == NULL)
    abort ();	/* cannot happen */
  return s_pfn_Get_Outline_Text_MetricsW (hdc, cbData, lpotmw);
}

static BOOL WINAPI
get_text_metrics_w(HDC hdc, LPTEXTMETRICW lptmw)
{
  static GetTextMetricsW_Proc s_pfn_Get_Text_MetricsW = NULL;
  HMODULE hm_unicows = NULL;
  if (g_b_init_get_text_metrics_w == 0)
    {
      g_b_init_get_text_metrics_w = 1;
      hm_unicows = w32_load_unicows_or_gdi32 ();
      if (hm_unicows)
	s_pfn_Get_Text_MetricsW = (GetTextMetricsW_Proc)
	  GetProcAddress (hm_unicows, "GetTextMetricsW");
    }
  if (s_pfn_Get_Text_MetricsW == NULL)
    abort ();	/* cannot happen */
  return s_pfn_Get_Text_MetricsW (hdc, lptmw);
}

static DWORD WINAPI
get_glyph_outline_w (HDC hdc, UINT uChar, UINT uFormat, LPGLYPHMETRICS lpgm,
		     DWORD cbBuffer, LPVOID lpvBuffer, const MAT2 *lpmat2)
{
  static GetGlyphOutlineW_Proc s_pfn_Get_Glyph_OutlineW = NULL;
  HMODULE hm_unicows = NULL;
  if (g_b_init_get_glyph_outline_w == 0)
    {
      g_b_init_get_glyph_outline_w = 1;
      hm_unicows = w32_load_unicows_or_gdi32 ();
      if (hm_unicows)
	s_pfn_Get_Glyph_OutlineW = (GetGlyphOutlineW_Proc)
	  GetProcAddress (hm_unicows, "GetGlyphOutlineW");
    }
  if (s_pfn_Get_Glyph_OutlineW == NULL)
    abort ();	/* cannot happen */
  return s_pfn_Get_Glyph_OutlineW (hdc, uChar, uFormat, lpgm, cbBuffer,
				   lpvBuffer, lpmat2);
}

static int
memq_no_quit (Lisp_Object elt, Lisp_Object list)
{
  while (CONSP (list) && ! EQ (XCAR (list), elt))
    list = XCDR (list);
  return (CONSP (list));
}

Lisp_Object
intern_font_name (char * string)
{
  Lisp_Object obarray, tem, str;
  int len;

  str = DECODE_SYSTEM (build_string (string));
  len = SCHARS (str);

  /* The following code is copied from the function intern (in lread.c).  */
  obarray = Vobarray;
  if (!VECTORP (obarray) || ASIZE (obarray) == 0)
    obarray = check_obarray (obarray);
  tem = oblookup (obarray, SDATA (str), len, len);
  if (SYMBOLP (tem))
    return tem;
  return Fintern (str, obarray);
}

/* w32 implementation of get_cache for font backend.
   Return a cache of font-entities on FRAME.  The cache must be a
   cons whose cdr part is the actual cache area.  */
Lisp_Object
w32font_get_cache (FRAME_PTR f)
{
  struct w32_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  return (dpyinfo->name_list_element);
}

/* w32 implementation of list for font backend.
   List fonts exactly matching with FONT_SPEC on FRAME.  The value
   is a vector of font-entities.  This is the sole API that
   allocates font-entities.  */
static Lisp_Object
w32font_list (Lisp_Object frame, Lisp_Object font_spec)
{
  Lisp_Object fonts = w32font_list_internal (frame, font_spec, 0);
  FONT_ADD_LOG ("w32font-list", font_spec, fonts);
  return fonts;
}

/* w32 implementation of match for font backend.
   Return a font entity most closely matching with FONT_SPEC on
   FRAME.  The closeness is determined by the font backend, thus
   `face-font-selection-order' is ignored here.  */
static Lisp_Object
w32font_match (Lisp_Object frame, Lisp_Object font_spec)
{
  Lisp_Object entity = w32font_match_internal (frame, font_spec, 0);
  FONT_ADD_LOG ("w32font-match", font_spec, entity);
  return entity;
}

/* w32 implementation of list_family for font backend.
   List available families.  The value is a list of family names
   (symbols).  */
static Lisp_Object
w32font_list_family (Lisp_Object frame)
{
  Lisp_Object list = Qnil;
  LOGFONT font_match_pattern;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  memset (&font_match_pattern, 0, sizeof (font_match_pattern));
  font_match_pattern.lfCharSet = DEFAULT_CHARSET;

  dc = get_frame_dc (f);

  EnumFontFamiliesEx (dc, &font_match_pattern,
                      (FONTENUMPROC) add_font_name_to_list,
                      (LPARAM) &list, 0);
  release_frame_dc (f, dc);

  return list;
}

/* w32 implementation of open for font backend.
   Open a font specified by FONT_ENTITY on frame F.
   If the font is scalable, open it with PIXEL_SIZE.  */
static Lisp_Object
w32font_open (FRAME_PTR f, Lisp_Object font_entity, int pixel_size)
{
  Lisp_Object font_object
    = font_make_object (VECSIZE (struct w32font_info),
                        font_entity, pixel_size);
  struct w32font_info *w32_font
    = (struct w32font_info *) XFONT_OBJECT (font_object);

  ASET (font_object, FONT_TYPE_INDEX, Qgdi);

  if (!w32font_open_internal (f, font_entity, pixel_size, font_object))
    {
      return Qnil;
    }

  /* GDI backend does not use glyph indices.  */
  w32_font->glyph_idx = 0;

  return font_object;
}

/* w32 implementation of close for font_backend.
   Close FONT on frame F.  */
void
w32font_close (FRAME_PTR f, struct font *font)
{
  int i;
  struct w32font_info *w32_font = (struct w32font_info *) font;

  /* Delete the GDI font object.  */
  DeleteObject (w32_font->hfont);

  /* Free all the cached metrics.  */
  if (w32_font->cached_metrics)
    {
      for (i = 0; i < w32_font->n_cache_blocks; i++)
        {
          xfree (w32_font->cached_metrics[i]);
        }
      xfree (w32_font->cached_metrics);
      w32_font->cached_metrics = NULL;
    }
}

/* w32 implementation of has_char for font backend.
   Optional.
   If FONT_ENTITY has a glyph for character C (Unicode code point),
   return 1.  If not, return 0.  If a font must be opened to check
   it, return -1.  */
int
w32font_has_char (Lisp_Object entity, int c)
{
  /* We can't be certain about which characters a font will support until
     we open it.  Checking the scripts that the font supports turns out
     to not be reliable.  */
  return -1;

#if 0
  Lisp_Object supported_scripts, extra, script;
  DWORD mask;

  extra = AREF (entity, FONT_EXTRA_INDEX);
  if (!CONSP (extra))
    return -1;

  supported_scripts = assq_no_quit (QCscript, extra);
  /* If font doesn't claim to support any scripts, then we can't be certain
     until we open it.  */
  if (!CONSP (supported_scripts))
    return -1;

  supported_scripts = XCDR (supported_scripts);

  script = CHAR_TABLE_REF (Vchar_script_table, c);

  /* If we don't know what script the character is from, then we can't be
     certain until we open it.  Also if the font claims support for the script
     the character is from, it may only have partial coverage, so we still
     can't be certain until we open the font.  */
  if (NILP (script) || memq_no_quit (script, supported_scripts))
    return -1;

  /* Font reports what scripts it supports, and none of them are the script
     the character is from. But we still can't be certain, as some fonts
     will contain some/most/all of the characters in that script without
     claiming support for it.  */
  return -1;
#endif
}

/* w32 implementation of encode_char for font backend.
   Return a glyph code of FONT for character C (Unicode code point).
   If FONT doesn't have such a glyph, return FONT_INVALID_CODE.

   For speed, the gdi backend uses Unicode (Emacs calls encode_char
   far too often for it to be efficient). But we still need to detect
   which characters are not supported by the font.
  */
static unsigned
w32font_encode_char (struct font *font, int c)
{
  struct w32font_info * w32_font = (struct w32font_info *)font;

  if (c < w32_font->metrics.tmFirstChar
      || c > w32_font->metrics.tmLastChar)
    return FONT_INVALID_CODE;
  else
    return c;
}

/* w32 implementation of text_extents for font backend.
   Perform the size computation of glyphs of FONT and fillin members
   of METRICS.  The glyphs are specified by their glyph codes in
   CODE (length NGLYPHS).  Apparently metrics can be NULL, in this
   case just return the overall width.  */
int
w32font_text_extents (struct font *font, unsigned *code,
		      int nglyphs, struct font_metrics *metrics)
{
  int i;
  HFONT old_font = NULL;
  HDC dc = NULL;
  struct frame * f;
  int total_width = 0;
  WORD *wcode;
  SIZE size;

  struct w32font_info *w32_font = (struct w32font_info *) font;

  if (metrics)
    {
      memset (metrics, 0, sizeof (struct font_metrics));
      metrics->ascent = font->ascent;
      metrics->descent = font->descent;

      for (i = 0; i < nglyphs; i++)
        {
	  struct w32_metric_cache *char_metric;
	  int block = *(code + i) / CACHE_BLOCKSIZE;
	  int pos_in_block = *(code + i) % CACHE_BLOCKSIZE;

	  if (block >= w32_font->n_cache_blocks)
	    {
	      if (!w32_font->cached_metrics)
		w32_font->cached_metrics
		  = xmalloc ((block + 1)
			     * sizeof (struct w32_metric_cache *));
	      else
		w32_font->cached_metrics
		  = xrealloc (w32_font->cached_metrics,
			      (block + 1)
			      * sizeof (struct w32_metric_cache *));
	      memset (w32_font->cached_metrics + w32_font->n_cache_blocks, 0,
		      ((block + 1 - w32_font->n_cache_blocks)
		       * sizeof (struct w32_metric_cache *)));
	      w32_font->n_cache_blocks = block + 1;
	    }

	  if (!w32_font->cached_metrics[block])
	    {
	      w32_font->cached_metrics[block]
		= xmalloc (CACHE_BLOCKSIZE * sizeof (struct w32_metric_cache));
	      memset (w32_font->cached_metrics[block], 0,
		      CACHE_BLOCKSIZE * sizeof (struct w32_metric_cache));
	    }

	  char_metric = w32_font->cached_metrics[block] + pos_in_block;

	  if (char_metric->status == W32METRIC_NO_ATTEMPT)
	    {
	      if (dc == NULL)
		{
		  /* TODO: Frames can come and go, and their fonts
		     outlive them. So we can't cache the frame in the
		     font structure.  Use selected_frame until the API
		     is updated to pass in a frame.  */
		  f = XFRAME (selected_frame);

                  dc = get_frame_dc (f);
                  old_font = SelectObject (dc, w32_font->hfont);
		}
	      compute_metrics (dc, w32_font, *(code + i), char_metric);
	    }

	  if (char_metric->status == W32METRIC_SUCCESS)
	    {
	      metrics->lbearing = min (metrics->lbearing,
				       metrics->width + char_metric->lbearing);
	      metrics->rbearing = max (metrics->rbearing,
				       metrics->width + char_metric->rbearing);
	      metrics->width += char_metric->width;
	    }
	  else
	    /* If we couldn't get metrics for a char,
	       use alternative method.  */
	    break;
	}
      /* If we got through everything, return.  */
      if (i == nglyphs)
        {
          if (dc != NULL)
            {
              /* Restore state and release DC.  */
              SelectObject (dc, old_font);
              release_frame_dc (f, dc);
            }

          return metrics->width;
        }
    }

  /* For non-truetype fonts, GetGlyphOutlineW is not supported, so
     fallback on other methods that will at least give some of the metric
     information.  */

  /* Make array big enough to hold surrogates.  */
  wcode = alloca (nglyphs * sizeof (WORD) * 2);
  for (i = 0; i < nglyphs; i++)
    {
      if (code[i] < 0x10000)
        wcode[i] = code[i];
      else
        {
          DWORD surrogate = code[i] - 0x10000;

          /* High surrogate: U+D800 - U+DBFF.  */
          wcode[i++] = 0xD800 + ((surrogate >> 10) & 0x03FF);
          /* Low surrogate: U+DC00 - U+DFFF.  */
          wcode[i] = 0xDC00 + (surrogate & 0x03FF);
          /* An extra glyph. wcode is already double the size of code to
             cope with this.  */
          nglyphs++;
        }
    }

  if (dc == NULL)
    {
      /* TODO: Frames can come and go, and their fonts outlive
	 them. So we can't cache the frame in the font structure.  Use
	 selected_frame until the API is updated to pass in a
	 frame.  */
      f = XFRAME (selected_frame);

      dc = get_frame_dc (f);
      old_font = SelectObject (dc, w32_font->hfont);
    }

  if (GetTextExtentPoint32W (dc, wcode, nglyphs, &size))
    {
      total_width = size.cx;
    }

  /* On 95/98/ME, only some Unicode functions are available, so fallback
     on doing a dummy draw to find the total width.  */
  if (!total_width)
    {
      RECT rect;
      rect.top = 0; rect.bottom = font->height; rect.left = 0; rect.right = 1;
      DrawTextW (dc, wcode, nglyphs, &rect,
                 DT_CALCRECT | DT_NOPREFIX | DT_SINGLELINE);
      total_width = rect.right;
    }

  /* Give our best estimate of the metrics, based on what we know.  */
  if (metrics)
    {
      metrics->width = total_width - w32_font->metrics.tmOverhang;
      metrics->lbearing = 0;
      metrics->rbearing = total_width;
    }

  /* Restore state and release DC.  */
  SelectObject (dc, old_font);
  release_frame_dc (f, dc);

  return total_width;
}

/* w32 implementation of draw for font backend.
   Optional.
   Draw glyphs between FROM and TO of S->char2b at (X Y) pixel
   position of frame F with S->FACE and S->GC.  If WITH_BACKGROUND
   is nonzero, fill the background in advance.  It is assured that
   WITH_BACKGROUND is zero when (FROM > 0 || TO < S->nchars).

   TODO: Currently this assumes that the colors and fonts are already
   set in the DC. This seems to be true now, but maybe only due to
   the old font code setting it up. It may be safer to resolve faces
   and fonts in here and set them explicitly
*/

int
w32font_draw (struct glyph_string *s, int from, int to,
	      int x, int y, int with_background)
{
  UINT options;
  HRGN orig_clip = NULL;
  int len = to - from;
  struct w32font_info *w32font = (struct w32font_info *) s->font;

  options = w32font->glyph_idx;

  if (s->num_clips > 0)
    {
      HRGN new_clip = CreateRectRgnIndirect (s->clip);

      /* Save clip region for later restoration.  */
      orig_clip = CreateRectRgn (0, 0, 0, 0);
      if (!GetClipRgn (s->hdc, orig_clip))
	{
	  DeleteObject (orig_clip);
	  orig_clip = NULL;
	}

      if (s->num_clips > 1)
        {
          HRGN clip2 = CreateRectRgnIndirect (s->clip + 1);

          CombineRgn (new_clip, new_clip, clip2, RGN_OR);
          DeleteObject (clip2);
        }

      SelectClipRgn (s->hdc, new_clip);
      DeleteObject (new_clip);
    }

  /* Using OPAQUE background mode can clear more background than expected
     when Cleartype is used.  Draw the background manually to avoid this.  */
  SetBkMode (s->hdc, TRANSPARENT);
  if (with_background)
    {
      HBRUSH brush;
      RECT rect;
      struct font *font = s->font;

      brush = CreateSolidBrush (s->gc->background);
      rect.left = x;
      rect.top = y - font->ascent;
      rect.right = x + s->width;
      rect.bottom = y + font->descent;
      FillRect (s->hdc, &rect, brush);
      DeleteObject (brush);
    }

  if (s->padding_p)
    {
      int i;

      for (i = 0; i < len; i++)
	ExtTextOutW (s->hdc, x + i, y, options, NULL,
		     s->char2b + from + i, 1, NULL);
    }
  else
    ExtTextOutW (s->hdc, x, y, options, NULL, s->char2b + from, len, NULL);

  /* Restore clip region.  */
  if (s->num_clips > 0)
    SelectClipRgn (s->hdc, orig_clip);

  if (orig_clip)
    DeleteObject (orig_clip);

  return len;
}

/* w32 implementation of free_entity for font backend.
   Optional (if FONT_EXTRA_INDEX is not Lisp_Save_Value).
   Free FONT_EXTRA_INDEX field of FONT_ENTITY.
static void
w32font_free_entity (Lisp_Object entity);
  */

/* w32 implementation of prepare_face for font backend.
   Optional (if FACE->extra is not used).
   Prepare FACE for displaying characters by FONT on frame F by
   storing some data in FACE->extra.  If successful, return 0.
   Otherwise, return -1.
static int
w32font_prepare_face (FRAME_PTR f, struct face *face);
  */
/* w32 implementation of done_face for font backend.
   Optional.
   Done FACE for displaying characters by FACE->font on frame F.
static void
w32font_done_face (FRAME_PTR f, struct face *face);  */

/* w32 implementation of get_bitmap for font backend.
   Optional.
   Store bitmap data for glyph-code CODE of FONT in BITMAP.  It is
   intended that this method is called from the other font-driver
   for actual drawing.
static int
w32font_get_bitmap (struct font *font, unsigned code,
                    struct font_bitmap *bitmap, int bits_per_pixel);
  */
/* w32 implementation of free_bitmap for font backend.
   Optional.
   Free bitmap data in BITMAP.
static void
w32font_free_bitmap (struct font *font, struct font_bitmap *bitmap);
  */
/* w32 implementation of get_outline for font backend.
   Optional.
   Return an outline data for glyph-code CODE of FONT.  The format
   of the outline data depends on the font-driver.
static void *
w32font_get_outline (struct font *font, unsigned code);
  */
/* w32 implementation of free_outline for font backend.
   Optional.
   Free OUTLINE (that is obtained by the above method).
static void
w32font_free_outline (struct font *font, void *outline);
  */
/* w32 implementation of anchor_point for font backend.
   Optional.
   Get coordinates of the INDEXth anchor point of the glyph whose
   code is CODE.  Store the coordinates in *X and *Y.  Return 0 if
   the operations was successful.  Otherwise return -1.
static int
w32font_anchor_point (struct font *font, unsigned code,
                                 int index, int *x, int *y);
  */
/* w32 implementation of otf_capability for font backend.
   Optional.
   Return a list describing which scripts/languages FONT
   supports by which GSUB/GPOS features of OpenType tables.
static Lisp_Object
w32font_otf_capability (struct font *font);
  */
/* w32 implementation of otf_drive for font backend.
   Optional.
   Apply FONT's OTF-FEATURES to the glyph string.

   FEATURES specifies which OTF features to apply in this format:
      (SCRIPT LANGSYS GSUB-FEATURE GPOS-FEATURE)
   See the documentation of `font-drive-otf' for the detail.

   This method applies the specified features to the codes in the
   elements of GSTRING-IN (between FROMth and TOth).  The output
   codes are stored in GSTRING-OUT at the IDXth element and the
   following elements.

   Return the number of output codes.  If none of the features are
   applicable to the input data, return 0.  If GSTRING-OUT is too
   short, return -1.
static int
w32font_otf_drive (struct font *font, Lisp_Object features,
                   Lisp_Object gstring_in, int from, int to,
                   Lisp_Object gstring_out, int idx,
                   int alternate_subst);
  */

/* Internal implementation of w32font_list.
   Additional parameter opentype_only restricts the returned fonts to
   opentype fonts, which can be used with the Uniscribe backend.  */
Lisp_Object
w32font_list_internal (Lisp_Object frame, Lisp_Object font_spec, int opentype_only)
{
  struct font_callback_data match_data;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  match_data.orig_font_spec = font_spec;
  match_data.list = Qnil;
  match_data.frame = frame;

  memset (&match_data.pattern, 0, sizeof (LOGFONT));
  fill_in_logfont (f, &match_data.pattern, font_spec);

  /* If the charset is unrecognized, then we won't find a font, so don't
     waste time looking for one.  */
  if (match_data.pattern.lfCharSet == DEFAULT_CHARSET)
    {
      Lisp_Object spec_charset = AREF (font_spec, FONT_REGISTRY_INDEX);
      if (!NILP (spec_charset)
	  && !EQ (spec_charset, Qiso10646_1)
	  && !EQ (spec_charset, Qunicode_bmp)
	  && !EQ (spec_charset, Qunicode_sip)
	  && !EQ (spec_charset, Qunknown))
	return Qnil;
    }

  match_data.opentype_only = opentype_only;
  if (opentype_only)
    match_data.pattern.lfOutPrecision = OUT_OUTLINE_PRECIS;

  if (match_data.pattern.lfFaceName[0] == '\0')
    {
      /* EnumFontFamiliesEx does not take other fields into account if
         font name is blank, so need to use two passes.  */
      list_all_matching_fonts (&match_data);
    }
  else
    {
      dc = get_frame_dc (f);

      EnumFontFamiliesEx (dc, &match_data.pattern,
                          (FONTENUMPROC) add_font_entity_to_list,
                          (LPARAM) &match_data, 0);
      release_frame_dc (f, dc);
    }

  return match_data.list;
}

/* Internal implementation of w32font_match.
   Additional parameter opentype_only restricts the returned fonts to
   opentype fonts, which can be used with the Uniscribe backend.  */
Lisp_Object
w32font_match_internal (Lisp_Object frame, Lisp_Object font_spec, int opentype_only)
{
  struct font_callback_data match_data;
  HDC dc;
  FRAME_PTR f = XFRAME (frame);

  match_data.orig_font_spec = font_spec;
  match_data.frame = frame;
  match_data.list = Qnil;

  memset (&match_data.pattern, 0, sizeof (LOGFONT));
  fill_in_logfont (f, &match_data.pattern, font_spec);

  match_data.opentype_only = opentype_only;
  if (opentype_only)
    match_data.pattern.lfOutPrecision = OUT_OUTLINE_PRECIS;

  dc = get_frame_dc (f);

  EnumFontFamiliesEx (dc, &match_data.pattern,
                      (FONTENUMPROC) add_one_font_entity_to_list,
                      (LPARAM) &match_data, 0);
  release_frame_dc (f, dc);

  return NILP (match_data.list) ? Qnil : XCAR (match_data.list);
}

int
w32font_open_internal (FRAME_PTR f, Lisp_Object font_entity,
		       int pixel_size, Lisp_Object font_object)
{
  int len, size;
  LOGFONT logfont;
  HDC dc;
  HFONT hfont, old_font;
  Lisp_Object val, extra;
  struct w32font_info *w32_font;
  struct font * font;
  OUTLINETEXTMETRICW* metrics = NULL;

  w32_font = (struct w32font_info *) XFONT_OBJECT (font_object);
  font = (struct font *) w32_font;

  if (!font)
    return 0;

  memset (&logfont, 0, sizeof (logfont));
  fill_in_logfont (f, &logfont, font_entity);

  /* Prefer truetype fonts, to avoid known problems with type1 fonts, and
     limitations in bitmap fonts.  */
  val = AREF (font_entity, FONT_FOUNDRY_INDEX);
  if (!EQ (val, Qraster))
    logfont.lfOutPrecision = OUT_TT_PRECIS;

  size = XINT (AREF (font_entity, FONT_SIZE_INDEX));
  if (!size)
    size = pixel_size;

  logfont.lfHeight = -size;
  hfont = CreateFontIndirect (&logfont);

  if (hfont == NULL)
    return 0;

  /* Get the metrics for this font.  */
  dc = get_frame_dc (f);
  old_font = SelectObject (dc, hfont);

  /* Try getting the outline metrics (only works for truetype fonts).  */
  len = get_outline_metrics_w (dc, 0, NULL);
  if (len)
    {
      metrics = (OUTLINETEXTMETRICW *) alloca (len);
      if (get_outline_metrics_w (dc, len, metrics))
        memcpy (&w32_font->metrics, &metrics->otmTextMetrics,
		sizeof (TEXTMETRICW));
      else
        metrics = NULL;
    }

  if (!metrics)
    get_text_metrics_w (dc, &w32_font->metrics);

  w32_font->cached_metrics = NULL;
  w32_font->n_cache_blocks = 0;

  SelectObject (dc, old_font);
  release_frame_dc (f, dc);

  w32_font->hfont = hfont;

  {
    char *name;

    /* We don't know how much space we need for the full name, so start with
       96 bytes and go up in steps of 32.  */
    len = 96;
    name = alloca (len);
    while (name && w32font_full_name (&logfont, font_entity, pixel_size,
                                      name, len) < 0)
      {
        len += 32;
        name = alloca (len);
      }
    if (name)
      font->props[FONT_FULLNAME_INDEX]
        = DECODE_SYSTEM (build_string (name));
    else
      font->props[FONT_FULLNAME_INDEX]
	= DECODE_SYSTEM (build_string (logfont.lfFaceName));
  }

  font->max_width = w32_font->metrics.tmMaxCharWidth;
  /* Parts of Emacs display assume that height = ascent + descent...
     so height is defined later, after ascent and descent.
  font->height = w32_font->metrics.tmHeight
    + w32_font->metrics.tmExternalLeading;
  */

  font->space_width = font->average_width = w32_font->metrics.tmAveCharWidth;

  font->vertical_centering = 0;
  font->encoding_type = 0;
  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->default_ascent = w32_font->metrics.tmAscent;
  font->font_encoder = NULL;
  font->pixel_size = size;
  font->driver = &w32font_driver;
  /* Use format cached during list, as the information we have access to
     here is incomplete.  */
  extra = AREF (font_entity, FONT_EXTRA_INDEX);
  if (CONSP (extra))
    {
      val = assq_no_quit (QCformat, extra);
      if (CONSP (val))
        font->props[FONT_FORMAT_INDEX] = XCDR (val);
      else
        font->props[FONT_FORMAT_INDEX] = Qunknown;
    }
  else
    font->props[FONT_FORMAT_INDEX] = Qunknown;

  font->props[FONT_FILE_INDEX] = Qnil;
  font->encoding_charset = -1;
  font->repertory_charset = -1;
  /* TODO: do we really want the minimum width here, which could be negative? */
  font->min_width = font->space_width;
  font->ascent = w32_font->metrics.tmAscent;
  font->descent = w32_font->metrics.tmDescent;
  font->height = font->ascent + font->descent;

  if (metrics)
    {
      font->underline_thickness = metrics->otmsUnderscoreSize;
      font->underline_position = -metrics->otmsUnderscorePosition;
    }
  else
    {
      font->underline_thickness = 0;
      font->underline_position = -1;
    }

  /* For temporary compatibility with legacy code that expects the
     name to be usable in x-list-fonts. Eventually we expect to change
     x-list-fonts and other places that use fonts so that this can be
     an fcname or similar.  */
  font->props[FONT_NAME_INDEX] = Ffont_xlfd_name (font_object, Qnil);

  return 1;
}

/* Callback function for EnumFontFamiliesEx.
 * Adds the name of a font to a Lisp list (passed in as the lParam arg).  */
static int CALLBACK
add_font_name_to_list (ENUMLOGFONTEX *logical_font,
		       NEWTEXTMETRICEX *physical_font,
		       DWORD font_type, LPARAM list_object)
{
  Lisp_Object* list = (Lisp_Object *) list_object;
  Lisp_Object family;

  /* Skip vertical fonts (intended only for printing)  */
  if (logical_font->elfLogFont.lfFaceName[0] == '@')
    return 1;

  family = intern_font_name (logical_font->elfLogFont.lfFaceName);
  if (! memq_no_quit (family, *list))
    *list = Fcons (family, *list);

  return 1;
}

static int w32_decode_weight (int);
static int w32_encode_weight (int);

/* Convert an enumerated Windows font to an Emacs font entity.  */
static Lisp_Object
w32_enumfont_pattern_entity (Lisp_Object frame,
			     ENUMLOGFONTEX *logical_font,
			     NEWTEXTMETRICEX *physical_font,
			     DWORD font_type,
			     LOGFONT *requested_font,
			     Lisp_Object backend)
{
  Lisp_Object entity, tem;
  LOGFONT *lf = (LOGFONT*) logical_font;
  BYTE generic_type;
  DWORD full_type = physical_font->ntmTm.ntmFlags;

  entity = font_make_entity ();

  ASET (entity, FONT_TYPE_INDEX, backend);
  ASET (entity, FONT_REGISTRY_INDEX, w32_registry (lf->lfCharSet, font_type));
  ASET (entity, FONT_OBJLIST_INDEX, Qnil);

  /* Foundry is difficult to get in readable form on Windows.
     But Emacs crashes if it is not set, so set it to something more
     generic.  These values make xlfds compatible with Emacs 22. */
  if (lf->lfOutPrecision == OUT_STRING_PRECIS)
    tem = Qraster;
  else if (lf->lfOutPrecision == OUT_STROKE_PRECIS)
    tem = Qoutline;
  else
    tem = Qunknown;

  ASET (entity, FONT_FOUNDRY_INDEX, tem);

  /* Save the generic family in the extra info, as it is likely to be
     useful to users looking for a close match.  */
  generic_type = physical_font->ntmTm.tmPitchAndFamily & 0xF0;
  if (generic_type == FF_DECORATIVE)
    tem = Qdecorative;
  else if (generic_type == FF_MODERN)
    tem = Qmono;
  else if (generic_type == FF_ROMAN)
    tem = Qserif;
  else if (generic_type == FF_SCRIPT)
    tem = Qscript;
  else if (generic_type == FF_SWISS)
    tem = Qsans;
  else
    tem = Qnil;

  ASET (entity, FONT_ADSTYLE_INDEX, tem);

  if (physical_font->ntmTm.tmPitchAndFamily & 0x01)
    ASET (entity, FONT_SPACING_INDEX, make_number (FONT_SPACING_PROPORTIONAL));
  else
    ASET (entity, FONT_SPACING_INDEX, make_number (FONT_SPACING_CHARCELL));

  if (requested_font->lfQuality != DEFAULT_QUALITY)
    {
      font_put_extra (entity, QCantialias,
                      lispy_antialias_type (requested_font->lfQuality));
    }
  ASET (entity, FONT_FAMILY_INDEX,
	intern_font_name (lf->lfFaceName));

  FONT_SET_STYLE (entity, FONT_WEIGHT_INDEX,
		  make_number (w32_decode_weight (lf->lfWeight)));
  FONT_SET_STYLE (entity, FONT_SLANT_INDEX,
		  make_number (lf->lfItalic ? 200 : 100));
  /* TODO: PANOSE struct has this info, but need to call GetOutlineTextMetrics
     to get it.  */
  FONT_SET_STYLE (entity, FONT_WIDTH_INDEX, make_number (100));

  if (font_type & RASTER_FONTTYPE)
    ASET (entity, FONT_SIZE_INDEX,
          make_number (physical_font->ntmTm.tmHeight
                       + physical_font->ntmTm.tmExternalLeading));
  else
    ASET (entity, FONT_SIZE_INDEX, make_number (0));

  /* Cache Unicode codepoints covered by this font, as there is no other way
     of getting this information easily.  */
  if (font_type & TRUETYPE_FONTTYPE)
    {
      tem = font_supported_scripts (&physical_font->ntmFontSig);
      if (!NILP (tem))
        font_put_extra (entity, QCscript, tem);
    }

  /* This information is not fully available when opening fonts, so
     save it here.  Only Windows 2000 and later return information
     about opentype and type1 fonts, so need a fallback for detecting
     truetype so that this information is not any worse than we could
     have obtained later.  */
  if (EQ (backend, Quniscribe) && (full_type & NTMFLAGS_OPENTYPE))
    tem = intern ("opentype");
  else if (font_type & TRUETYPE_FONTTYPE)
    tem = intern ("truetype");
  else if (full_type & NTM_PS_OPENTYPE)
    tem = intern ("postscript");
  else if (full_type & NTM_TYPE1)
    tem = intern ("type1");
  else if (font_type & RASTER_FONTTYPE)
    tem = intern ("w32bitmap");
  else
    tem = intern ("w32vector");

  font_put_extra (entity, QCformat, tem);

  return entity;
}


/* Convert generic families to the family portion of lfPitchAndFamily.  */
static BYTE
w32_generic_family (Lisp_Object name)
{
  /* Generic families.  */
  if (EQ (name, Qmonospace) || EQ (name, Qmono))
    return FF_MODERN;
  else if (EQ (name, Qsans) || EQ (name, Qsans_serif) || EQ (name, Qsansserif))
    return FF_SWISS;
  else if (EQ (name, Qserif))
    return FF_ROMAN;
  else if (EQ (name, Qdecorative))
    return FF_DECORATIVE;
  else if (EQ (name, Qscript))
    return FF_SCRIPT;
  else
    return FF_DONTCARE;
}

static int
logfonts_match (LOGFONT *font, LOGFONT *pattern)
{
  /* Only check height for raster fonts.  */
  if (pattern->lfHeight && font->lfOutPrecision == OUT_STRING_PRECIS
      && font->lfHeight != pattern->lfHeight)
    return 0;

  /* Have some flexibility with weights.  */
  if (pattern->lfWeight
      && ((font->lfWeight < (pattern->lfWeight - 150))
          || font->lfWeight > (pattern->lfWeight + 150)))
      return 0;

  /* Charset and face should be OK.  Italic has to be checked
     against the original spec, in case we don't have any preference.  */
  return 1;
}

/* Codepage Bitfields in FONTSIGNATURE struct.  */
#define CSB_JAPANESE (1 << 17)
#define CSB_KOREAN ((1 << 19) | (1 << 21))
#define CSB_CHINESE ((1 << 18) | (1 << 20))

static int
font_matches_spec (DWORD type, NEWTEXTMETRICEX *font,
		   Lisp_Object spec, Lisp_Object backend,
		   LOGFONT *logfont)
{
  Lisp_Object extra, val;

  /* Check italic. Can't check logfonts, since it is a boolean field,
     so there is no difference between "non-italic" and "don't care".  */
  {
    int slant = FONT_SLANT_NUMERIC (spec);

    if (slant >= 0
	&& ((slant > 150 && !font->ntmTm.tmItalic)
	    || (slant <= 150 && font->ntmTm.tmItalic)))
	  return 0;
  }

  /* Check adstyle against generic family.  */
  val = AREF (spec, FONT_ADSTYLE_INDEX);
  if (!NILP (val))
    {
      BYTE family = w32_generic_family (val);
      if (family != FF_DONTCARE
          && family != (font->ntmTm.tmPitchAndFamily & 0xF0))
        return 0;
    }

  /* Check spacing */
  val = AREF (spec, FONT_SPACING_INDEX);
  if (INTEGERP (val))
    {
      int spacing = XINT (val);
      int proportional = (spacing < FONT_SPACING_MONO);

      if ((proportional && !(font->ntmTm.tmPitchAndFamily & 0x01))
	  || (!proportional && (font->ntmTm.tmPitchAndFamily & 0x01)))
	return 0;
    }

  /* Check extra parameters.  */
  for (extra = AREF (spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      Lisp_Object extra_entry;
      extra_entry = XCAR (extra);
      if (CONSP (extra_entry))
        {
          Lisp_Object key = XCAR (extra_entry);

          val = XCDR (extra_entry);
          if (EQ (key, QCscript) && SYMBOLP (val))
            {
              /* Only truetype fonts will have information about what
                 scripts they support.  This probably means the user
                 will have to force Emacs to use raster, PostScript
                 or ATM fonts for non-ASCII text.  */
              if (type & TRUETYPE_FONTTYPE)
                {
                  Lisp_Object support
                    = font_supported_scripts (&font->ntmFontSig);
                  if (! memq_no_quit (val, support))
                    return 0;

		  /* Avoid using non-Japanese fonts for Japanese, even
		     if they claim they are capable, due to known
		     breakage in Vista and Windows 7 fonts
		     (bug#6029).  */
		  if (EQ (val, Qkana)
		      && (font->ntmTm.tmCharSet != SHIFTJIS_CHARSET
			  || !(font->ntmFontSig.fsCsb[0] & CSB_JAPANESE)))
		    return 0;
                }
              else
                {
                  /* Return specific matches, but play it safe. Fonts
                     that cover more than their charset would suggest
                     are likely to be truetype or opentype fonts,
                     covered above.  */
                  if (EQ (val, Qlatin))
                    {
                      /* Although every charset but symbol, thai and
                         arabic contains the basic ASCII set of latin
                         characters, Emacs expects much more.  */
                      if (font->ntmTm.tmCharSet != ANSI_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qsymbol))
                    {
                      if (font->ntmTm.tmCharSet != SYMBOL_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qcyrillic))
                    {
                      if (font->ntmTm.tmCharSet != RUSSIAN_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qgreek))
                    {
                      if (font->ntmTm.tmCharSet != GREEK_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qarabic))
                    {
                      if (font->ntmTm.tmCharSet != ARABIC_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qhebrew))
                    {
                      if (font->ntmTm.tmCharSet != HEBREW_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qthai))
                    {
                      if (font->ntmTm.tmCharSet != THAI_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qkana))
                    {
                      if (font->ntmTm.tmCharSet != SHIFTJIS_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qbopomofo))
                    {
                      if (font->ntmTm.tmCharSet != CHINESEBIG5_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qhangul))
                    {
                      if (font->ntmTm.tmCharSet != HANGUL_CHARSET
                          && font->ntmTm.tmCharSet != JOHAB_CHARSET)
                        return 0;
                    }
                  else if (EQ (val, Qhan))
                    {
                      if (font->ntmTm.tmCharSet != CHINESEBIG5_CHARSET
                          && font->ntmTm.tmCharSet != GB2312_CHARSET
                          && font->ntmTm.tmCharSet != HANGUL_CHARSET
                          && font->ntmTm.tmCharSet != JOHAB_CHARSET
                          && font->ntmTm.tmCharSet != SHIFTJIS_CHARSET)
                        return 0;
                    }
                  else
                    /* Other scripts unlikely to be handled by non-truetype
		       fonts.  */
                    return 0;
                }
            }
	  else if (EQ (key, QClang) && SYMBOLP (val))
	    {
	      /* Just handle the CJK languages here, as the lang
		 parameter is used to select a font with appropriate
		 glyphs in the cjk unified ideographs block. Other fonts
	         support for a language can be solely determined by
	         its character coverage.  */
	      if (EQ (val, Qja))
		{
		  if (!(font->ntmFontSig.fsCsb[0] & CSB_JAPANESE))
		    return 0;
		}
	      else if (EQ (val, Qko))
		{
		  if (!(font->ntmFontSig.fsCsb[0] & CSB_KOREAN))
		    return 0;
		}
	      else if (EQ (val, Qzh))
		{
		  if (!(font->ntmFontSig.fsCsb[0] & CSB_CHINESE))
                    return 0;
		}
	      else
		/* Any other language, we don't recognize it. Only the above
                   currently appear in fontset.el, so it isn't worth
                   creating a mapping table of codepages/scripts to languages
                   or opening the font to see if there are any language tags
                   in it that the W32 API does not expose. Fontset
		   spec should have a fallback, as some backends do
		   not recognize language at all.  */
		return 0;
	    }
          else if (EQ (key, QCotf) && CONSP (val))
	    {
	      /* OTF features only supported by the uniscribe backend.  */
	      if (EQ (backend, Quniscribe))
		{
		  if (!uniscribe_check_otf (logfont, val))
		    return 0;
		}
	      else
		return 0;
	    }
        }
    }
  return 1;
}

static int
w32font_coverage_ok (FONTSIGNATURE * coverage, BYTE charset)
{
  DWORD subrange1 = coverage->fsUsb[1];

#define SUBRANGE1_HAN_MASK 0x08000000
#define SUBRANGE1_HANGEUL_MASK 0x01000000
#define SUBRANGE1_JAPANESE_MASK (0x00060000 | SUBRANGE1_HAN_MASK)

  if (charset == GB2312_CHARSET || charset == CHINESEBIG5_CHARSET)
    {
      return (subrange1 & SUBRANGE1_HAN_MASK) == SUBRANGE1_HAN_MASK;
    }
  else if (charset == SHIFTJIS_CHARSET)
    {
      return (subrange1 & SUBRANGE1_JAPANESE_MASK) == SUBRANGE1_JAPANESE_MASK;
    }
  else if (charset == HANGEUL_CHARSET)
    {
      return (subrange1 & SUBRANGE1_HANGEUL_MASK) == SUBRANGE1_HANGEUL_MASK;
    }

  return 1;
}


static int
check_face_name (LOGFONT *font, char *full_name)
{
  char full_iname[LF_FULLFACESIZE+1];

  /* Just check for names known to cause problems, since the full name
     can contain expanded abbreviations, prefixed foundry, postfixed
     style, the latter of which sometimes differs from the style indicated
     in the shorter name (eg Lt becomes Light or even Extra Light)  */

  /* Helvetica is mapped to Arial in Windows, but if a Type-1 Helvetica is
     installed, we run into problems with the Uniscribe backend which tries
     to avoid non-truetype fonts, and ends up mixing the Type-1 Helvetica
     with Arial's characteristics, since that attempt to use TrueType works
     some places, but not others.  */
  if (!xstrcasecmp (font->lfFaceName, "helvetica"))
    {
      strncpy (full_iname, full_name, LF_FULLFACESIZE);
      full_iname[LF_FULLFACESIZE] = 0;
      _strlwr (full_iname);
      return strstr ("helvetica", full_iname) != NULL;
    }
  /* Same for Helv.  */
  if (!xstrcasecmp (font->lfFaceName, "helv"))
    {
      strncpy (full_iname, full_name, LF_FULLFACESIZE);
      full_iname[LF_FULLFACESIZE] = 0;
      _strlwr (full_iname);
      return strstr ("helv", full_iname) != NULL;
    }

  /* Since Times is mapped to Times New Roman, a substring
     match is not sufficient to filter out the bogus match.  */
  else if (!xstrcasecmp (font->lfFaceName, "times"))
    return xstrcasecmp (full_name, "times") == 0;

  return 1;
}


/* Callback function for EnumFontFamiliesEx.
 * Checks if a font matches everything we are trying to check against,
 * and if so, adds it to a list. Both the data we are checking against
 * and the list to which the fonts are added are passed in via the
 * lparam argument, in the form of a font_callback_data struct. */
static int CALLBACK
add_font_entity_to_list (ENUMLOGFONTEX *logical_font,
			 NEWTEXTMETRICEX *physical_font,
			 DWORD font_type, LPARAM lParam)
{
  struct font_callback_data *match_data
    = (struct font_callback_data *) lParam;
  Lisp_Object backend = match_data->opentype_only ? Quniscribe : Qgdi;
  Lisp_Object entity;

  int is_unicode = physical_font->ntmFontSig.fsUsb[3]
    || physical_font->ntmFontSig.fsUsb[2]
    || physical_font->ntmFontSig.fsUsb[1]
    || physical_font->ntmFontSig.fsUsb[0] & 0x3fffffff;

  /* Skip non matching fonts.  */

  /* For uniscribe backend, consider only truetype or opentype fonts
     that have some Unicode coverage.  */
  if (match_data->opentype_only
      && ((!(physical_font->ntmTm.ntmFlags & NTMFLAGS_OPENTYPE)
	   && !(font_type & TRUETYPE_FONTTYPE))
	  || !is_unicode))
    return 1;

  /* Ensure a match.  */
  if (!logfonts_match (&logical_font->elfLogFont, &match_data->pattern)
      || !font_matches_spec (font_type, physical_font,
			     match_data->orig_font_spec, backend,
			     &logical_font->elfLogFont)
      || !w32font_coverage_ok (&physical_font->ntmFontSig,
			       match_data->pattern.lfCharSet))
    return 1;

  /* Avoid substitutions involving raster fonts (eg Helv -> MS Sans Serif)
     We limit this to raster fonts, because the test can catch some
     genuine fonts (eg the full name of DejaVu Sans Mono Light is actually
     DejaVu Sans Mono ExtraLight). Helvetica -> Arial substitution will
     therefore get through this test.  Since full names can be prefixed
     by a foundry, we accept raster fonts if the font name is found
     anywhere within the full name.  */
  if ((logical_font->elfLogFont.lfOutPrecision == OUT_STRING_PRECIS
       && !strstr (logical_font->elfFullName,
		   logical_font->elfLogFont.lfFaceName))
      /* Check for well known substitutions that mess things up in the
	 presence of Type-1 fonts of the same name.  */
      || (!check_face_name (&logical_font->elfLogFont,
			    logical_font->elfFullName)))
    return 1;

  /* Make a font entity for the font.  */
  entity = w32_enumfont_pattern_entity (match_data->frame, logical_font,
					physical_font, font_type,
					&match_data->pattern,
					backend);

  if (!NILP (entity))
    {
      Lisp_Object spec_charset = AREF (match_data->orig_font_spec,
				       FONT_REGISTRY_INDEX);

      /* iso10646-1 fonts must contain Unicode mapping tables.  */
      if (EQ (spec_charset, Qiso10646_1))
	{
	  if (!is_unicode)
	    return 1;
	}
      /* unicode-bmp fonts must contain characters from the BMP.  */
      else if (EQ (spec_charset, Qunicode_bmp))
	{
	  if (!physical_font->ntmFontSig.fsUsb[3]
	      && !(physical_font->ntmFontSig.fsUsb[2] & 0xFFFFFF9E)
	      && !(physical_font->ntmFontSig.fsUsb[1] & 0xE81FFFFF)
	      && !(physical_font->ntmFontSig.fsUsb[0] & 0x007F001F))
	    return 1;
	}
      /* unicode-sip fonts must contain characters in Unicode plane 2.
	 so look for bit 57 (surrogates) in the Unicode subranges, plus
	 the bits for CJK ranges that include those characters.  */
      else if (EQ (spec_charset, Qunicode_sip))
	{
	  if (!(physical_font->ntmFontSig.fsUsb[1] & 0x02000000)
	      || !(physical_font->ntmFontSig.fsUsb[1] & 0x28000000))
	    return 1;
	}

      /* This font matches.  */

      /* If registry was specified, ensure it is reported as the same.  */
      if (!NILP (spec_charset))
	{
	  /* Avoid using non-Japanese fonts for Japanese, even if they
	     claim they are capable, due to known breakage in Vista
	     and Windows 7 fonts (bug#6029).  */
	  if (logical_font->elfLogFont.lfCharSet == SHIFTJIS_CHARSET
	      && !(physical_font->ntmFontSig.fsCsb[0] & CSB_JAPANESE))
	    return 1;
	  else
	    ASET (entity, FONT_REGISTRY_INDEX, spec_charset);
	}
      /* Otherwise if using the uniscribe backend, report ANSI and DEFAULT
	 fonts as Unicode and skip other charsets.  */
      else if (match_data->opentype_only)
	{
	  if (logical_font->elfLogFont.lfCharSet == ANSI_CHARSET
	      || logical_font->elfLogFont.lfCharSet == DEFAULT_CHARSET)
	    ASET (entity, FONT_REGISTRY_INDEX, Qiso10646_1);
	  else
	    return 1;
	}

      /* Add this font to the list.  */
      match_data->list = Fcons (entity, match_data->list);
    }
  return 1;
}

/* Callback function for EnumFontFamiliesEx.
 * Terminates the search once we have a match. */
static int CALLBACK
add_one_font_entity_to_list (ENUMLOGFONTEX *logical_font,
			     NEWTEXTMETRICEX *physical_font,
			     DWORD font_type, LPARAM lParam)
{
  struct font_callback_data *match_data
    = (struct font_callback_data *) lParam;
  add_font_entity_to_list (logical_font, physical_font, font_type, lParam);

  /* If we have a font in the list, terminate the search.  */
  return NILP (match_data->list);
}

/* Old function to convert from x to w32 charset, from w32fns.c.  */
static LONG
x_to_w32_charset (char * lpcs)
{
  Lisp_Object this_entry, w32_charset;
  char *charset;
  int len = strlen (lpcs);

  /* Support "*-#nnn" format for unknown charsets.  */
  if (strncmp (lpcs, "*-#", 3) == 0)
    return atoi (lpcs + 3);

  /* All Windows fonts qualify as Unicode.  */
  if (!strncmp (lpcs, "iso10646", 8))
    return DEFAULT_CHARSET;

  /* Handle wildcards by ignoring them; eg. treat "big5*-*" as "big5".  */
  charset = alloca (len + 1);
  strcpy (charset, lpcs);
  lpcs = strchr (charset, '*');
  if (lpcs)
    *lpcs = '\0';

  /* Look through w32-charset-info-alist for the character set.
     Format of each entry is
       (CHARSET_NAME . (WINDOWS_CHARSET . CODEPAGE)).
  */
  this_entry = Fassoc (build_string (charset), Vw32_charset_info_alist);

  if (NILP (this_entry))
    {
      /* At startup, we want iso8859-1 fonts to come up properly. */
      if (xstrcasecmp (charset, "iso8859-1") == 0)
        return ANSI_CHARSET;
      else
        return DEFAULT_CHARSET;
    }

  w32_charset = Fcar (Fcdr (this_entry));

  /* Translate Lisp symbol to number.  */
  if (EQ (w32_charset, Qw32_charset_ansi))
    return ANSI_CHARSET;
  if (EQ (w32_charset, Qw32_charset_symbol))
    return SYMBOL_CHARSET;
  if (EQ (w32_charset, Qw32_charset_shiftjis))
    return SHIFTJIS_CHARSET;
  if (EQ (w32_charset, Qw32_charset_hangeul))
    return HANGEUL_CHARSET;
  if (EQ (w32_charset, Qw32_charset_chinesebig5))
    return CHINESEBIG5_CHARSET;
  if (EQ (w32_charset, Qw32_charset_gb2312))
    return GB2312_CHARSET;
  if (EQ (w32_charset, Qw32_charset_oem))
    return OEM_CHARSET;
  if (EQ (w32_charset, Qw32_charset_johab))
    return JOHAB_CHARSET;
  if (EQ (w32_charset, Qw32_charset_easteurope))
    return EASTEUROPE_CHARSET;
  if (EQ (w32_charset, Qw32_charset_turkish))
    return TURKISH_CHARSET;
  if (EQ (w32_charset, Qw32_charset_baltic))
    return BALTIC_CHARSET;
  if (EQ (w32_charset, Qw32_charset_russian))
    return RUSSIAN_CHARSET;
  if (EQ (w32_charset, Qw32_charset_arabic))
    return ARABIC_CHARSET;
  if (EQ (w32_charset, Qw32_charset_greek))
    return GREEK_CHARSET;
  if (EQ (w32_charset, Qw32_charset_hebrew))
    return HEBREW_CHARSET;
  if (EQ (w32_charset, Qw32_charset_vietnamese))
    return VIETNAMESE_CHARSET;
  if (EQ (w32_charset, Qw32_charset_thai))
    return THAI_CHARSET;
  if (EQ (w32_charset, Qw32_charset_mac))
    return MAC_CHARSET;

  return DEFAULT_CHARSET;
}


/* Convert a Lisp font registry (symbol) to a windows charset.  */
static LONG
registry_to_w32_charset (Lisp_Object charset)
{
  if (EQ (charset, Qiso10646_1) || EQ (charset, Qunicode_bmp)
      || EQ (charset, Qunicode_sip))
    return DEFAULT_CHARSET; /* UNICODE_CHARSET not defined in MingW32 */
  else if (EQ (charset, Qiso8859_1))
    return ANSI_CHARSET;
  else if (SYMBOLP (charset))
    return x_to_w32_charset (SDATA (SYMBOL_NAME (charset)));
  else
    return DEFAULT_CHARSET;
}

/* Old function to convert from w32 to x charset, from w32fns.c.  */
static char *
w32_to_x_charset (int fncharset, char *matching)
{
  static char buf[32];
  Lisp_Object charset_type;
  int match_len = 0;

  if (matching)
    {
      /* If fully specified, accept it as it is.  Otherwise use a
	 substring match. */
      char *wildcard = strchr (matching, '*');
      if (wildcard)
	*wildcard = '\0';
      else if (strchr (matching, '-'))
	return matching;

      match_len = strlen (matching);
    }

  switch (fncharset)
    {
    case ANSI_CHARSET:
      /* Handle startup case of w32-charset-info-alist not
         being set up yet. */
      if (NILP (Vw32_charset_info_alist))
        return "iso8859-1";
      charset_type = Qw32_charset_ansi;
      break;
    case DEFAULT_CHARSET:
      charset_type = Qw32_charset_default;
      break;
    case SYMBOL_CHARSET:
      charset_type = Qw32_charset_symbol;
      break;
    case SHIFTJIS_CHARSET:
      charset_type = Qw32_charset_shiftjis;
      break;
    case HANGEUL_CHARSET:
      charset_type = Qw32_charset_hangeul;
      break;
    case GB2312_CHARSET:
      charset_type = Qw32_charset_gb2312;
      break;
    case CHINESEBIG5_CHARSET:
      charset_type = Qw32_charset_chinesebig5;
      break;
    case OEM_CHARSET:
      charset_type = Qw32_charset_oem;
      break;
    case EASTEUROPE_CHARSET:
      charset_type = Qw32_charset_easteurope;
      break;
    case TURKISH_CHARSET:
      charset_type = Qw32_charset_turkish;
      break;
    case BALTIC_CHARSET:
      charset_type = Qw32_charset_baltic;
      break;
    case RUSSIAN_CHARSET:
      charset_type = Qw32_charset_russian;
      break;
    case ARABIC_CHARSET:
      charset_type = Qw32_charset_arabic;
      break;
    case GREEK_CHARSET:
      charset_type = Qw32_charset_greek;
      break;
    case HEBREW_CHARSET:
      charset_type = Qw32_charset_hebrew;
      break;
    case VIETNAMESE_CHARSET:
      charset_type = Qw32_charset_vietnamese;
      break;
    case THAI_CHARSET:
      charset_type = Qw32_charset_thai;
      break;
    case MAC_CHARSET:
      charset_type = Qw32_charset_mac;
      break;
    case JOHAB_CHARSET:
      charset_type = Qw32_charset_johab;
      break;

    default:
      /* Encode numerical value of unknown charset.  */
      sprintf (buf, "*-#%u", fncharset);
      return buf;
    }

  {
    Lisp_Object rest;
    char * best_match = NULL;
    int matching_found = 0;

    /* Look through w32-charset-info-alist for the character set.
       Prefer ISO codepages, and prefer lower numbers in the ISO
       range. Only return charsets for codepages which are installed.

       Format of each entry is
         (CHARSET_NAME . (WINDOWS_CHARSET . CODEPAGE)).
    */
    for (rest = Vw32_charset_info_alist; CONSP (rest); rest = XCDR (rest))
      {
        char * x_charset;
        Lisp_Object w32_charset;
        Lisp_Object codepage;

        Lisp_Object this_entry = XCAR (rest);

        /* Skip invalid entries in alist. */
        if (!CONSP (this_entry) || !STRINGP (XCAR (this_entry))
            || !CONSP (XCDR (this_entry))
            || !SYMBOLP (XCAR (XCDR (this_entry))))
          continue;

        x_charset = SDATA (XCAR (this_entry));
        w32_charset = XCAR (XCDR (this_entry));
        codepage = XCDR (XCDR (this_entry));

        /* Look for Same charset and a valid codepage (or non-int
           which means ignore).  */
        if (EQ (w32_charset, charset_type)
            && (!INTEGERP (codepage) || XINT (codepage) == CP_DEFAULT
                || IsValidCodePage (XINT (codepage))))
          {
            /* If we don't have a match already, then this is the
               best.  */
            if (!best_match)
	      {
		best_match = x_charset;
		if (matching && !strnicmp (x_charset, matching, match_len))
		  matching_found = 1;
	      }
	    /* If we already found a match for MATCHING, then
	       only consider other matches.  */
	    else if (matching_found
		     && strnicmp (x_charset, matching, match_len))
	      continue;
	    /* If this matches what we want, and the best so far doesn't,
	       then this is better.  */
	    else if (!matching_found && matching
		     && !strnicmp (x_charset, matching, match_len))
	      {
		best_match = x_charset;
		matching_found = 1;
	      }
	    /* If this is fully specified, and the best so far isn't,
	       then this is better.  */
	    else if ((!strchr (best_match, '-') && strchr (x_charset, '-'))
	    /* If this is an ISO codepage, and the best so far isn't,
	       then this is better, but only if it fully specifies the
	       encoding.  */
		|| (strnicmp (best_match, "iso", 3) != 0
		    && strnicmp (x_charset, "iso", 3) == 0
		    && strchr (x_charset, '-')))
		best_match = x_charset;
            /* If both are ISO8859 codepages, choose the one with the
               lowest number in the encoding field.  */
            else if (strnicmp (best_match, "iso8859-", 8) == 0
                     && strnicmp (x_charset, "iso8859-", 8) == 0)
              {
                int best_enc = atoi (best_match + 8);
                int this_enc = atoi (x_charset + 8);
                if (this_enc > 0 && this_enc < best_enc)
                  best_match = x_charset;
              }
          }
      }

    /* If no match, encode the numeric value. */
    if (!best_match)
      {
        sprintf (buf, "*-#%u", fncharset);
        return buf;
      }

    strncpy (buf, best_match, 31);
    /* If the charset is not fully specified, put -0 on the end.  */
    if (!strchr (best_match, '-'))
      {
	int pos = strlen (best_match);
	/* Charset specifiers shouldn't be very long.  If it is a made
	   up one, truncating it should not do any harm since it isn't
	   recognized anyway.  */
	if (pos > 29)
	  pos = 29;
	strcpy (buf + pos, "-0");
      }
    buf[31] = '\0';
    return buf;
  }
}

static Lisp_Object
w32_registry (LONG w32_charset, DWORD font_type)
{
  char *charset;

  /* If charset is defaulted, charset is Unicode or unknown, depending on
     font type.  */
  if (w32_charset == DEFAULT_CHARSET)
    return font_type == TRUETYPE_FONTTYPE ? Qiso10646_1 : Qunknown;

  charset = w32_to_x_charset (w32_charset, NULL);
  return font_intern_prop (charset, strlen (charset), 1);
}

static int
w32_decode_weight (int fnweight)
{
  if (fnweight >= FW_HEAVY)      return 210;
  if (fnweight >= FW_EXTRABOLD)  return 205;
  if (fnweight >= FW_BOLD)       return 200;
  if (fnweight >= FW_SEMIBOLD)   return 180;
  if (fnweight >= FW_NORMAL)     return 100;
  if (fnweight >= FW_LIGHT)      return 50;
  if (fnweight >= FW_EXTRALIGHT) return 40;
  if (fnweight >  FW_THIN)       return 20;
  return 0;
}

static int
w32_encode_weight (int n)
{
  if (n >= 210) return FW_HEAVY;
  if (n >= 205) return FW_EXTRABOLD;
  if (n >= 200) return FW_BOLD;
  if (n >= 180) return FW_SEMIBOLD;
  if (n >= 100) return FW_NORMAL;
  if (n >= 50)  return FW_LIGHT;
  if (n >= 40)  return FW_EXTRALIGHT;
  if (n >= 20)  return FW_THIN;
  return 0;
}

/* Convert a Windows font weight into one of the weights supported
   by fontconfig (see font.c:font_parse_fcname).  */
static Lisp_Object
w32_to_fc_weight (int n)
{
  if (n >= FW_EXTRABOLD) return intern ("black");
  if (n >= FW_BOLD)      return intern ("bold");
  if (n >= FW_SEMIBOLD)  return intern ("demibold");
  if (n >= FW_NORMAL)    return intern ("medium");
  return intern ("light");
}

/* Fill in all the available details of LOGFONT from FONT_SPEC.  */
static void
fill_in_logfont (FRAME_PTR f, LOGFONT *logfont, Lisp_Object font_spec)
{
  Lisp_Object tmp, extra;
  int dpi = FRAME_W32_DISPLAY_INFO (f)->resy;

  tmp = AREF (font_spec, FONT_DPI_INDEX);
  if (INTEGERP (tmp))
    {
      dpi = XINT (tmp);
    }
  else if (FLOATP (tmp))
    {
      dpi = (int) (XFLOAT_DATA (tmp) + 0.5);
    }

  /* Height  */
  tmp = AREF (font_spec, FONT_SIZE_INDEX);
  if (INTEGERP (tmp))
    logfont->lfHeight = -1 * XINT (tmp);
  else if (FLOATP (tmp))
    logfont->lfHeight = (int) (-1.0 *  dpi * XFLOAT_DATA (tmp) / 72.27 + 0.5);

  /* Escapement  */

  /* Orientation  */

  /* Weight  */
  tmp = AREF (font_spec, FONT_WEIGHT_INDEX);
  if (INTEGERP (tmp))
    logfont->lfWeight = w32_encode_weight (FONT_WEIGHT_NUMERIC (font_spec));

  /* Italic  */
  tmp = AREF (font_spec, FONT_SLANT_INDEX);
  if (INTEGERP (tmp))
    {
      int slant = FONT_SLANT_NUMERIC (font_spec);
      logfont->lfItalic = slant > 150 ? 1 : 0;
    }

  /* Underline  */

  /* Strikeout  */

  /* Charset  */
  tmp = AREF (font_spec, FONT_REGISTRY_INDEX);
  if (! NILP (tmp))
    logfont->lfCharSet = registry_to_w32_charset (tmp);
  else
    logfont->lfCharSet = DEFAULT_CHARSET;

  /* Out Precision  */

  /* Clip Precision  */

  /* Quality */
  logfont->lfQuality = DEFAULT_QUALITY;

  /* Generic Family and Face Name  */
  logfont->lfPitchAndFamily = FF_DONTCARE | DEFAULT_PITCH;

  tmp = AREF (font_spec, FONT_FAMILY_INDEX);
  if (! NILP (tmp))
    {
      logfont->lfPitchAndFamily = w32_generic_family (tmp) | DEFAULT_PITCH;
      if ((logfont->lfPitchAndFamily & 0xF0) != FF_DONTCARE)
        ; /* Font name was generic, don't fill in font name.  */
        /* Font families are interned, but allow for strings also in case of
           user input.  */
      else if (SYMBOLP (tmp))
	{
	  strncpy (logfont->lfFaceName,
		   SDATA (ENCODE_SYSTEM (SYMBOL_NAME (tmp))), LF_FACESIZE);
	  logfont->lfFaceName[LF_FACESIZE-1] = '\0';
	}
    }

  tmp = AREF (font_spec, FONT_ADSTYLE_INDEX);
  if (!NILP (tmp))
    {
      /* Override generic family.  */
      BYTE family = w32_generic_family (tmp);
      if (family != FF_DONTCARE)
        logfont->lfPitchAndFamily = family | DEFAULT_PITCH;
    }

  /* Set pitch based on the spacing property.  */
  tmp = AREF (font_spec, FONT_SPACING_INDEX);
  if (INTEGERP (tmp))
    {
      int spacing = XINT (tmp);
      if (spacing < FONT_SPACING_MONO)
	logfont->lfPitchAndFamily
	  = (logfont->lfPitchAndFamily & 0xF0) | VARIABLE_PITCH;
      else
	logfont->lfPitchAndFamily
	  = (logfont->lfPitchAndFamily & 0xF0) | FIXED_PITCH;
    }

  /* Process EXTRA info.  */
  for (extra = AREF (font_spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      tmp = XCAR (extra);
      if (CONSP (tmp))
        {
          Lisp_Object key, val;
          key = XCAR (tmp), val = XCDR (tmp);
          /* Only use QCscript if charset is not provided, or is Unicode
             and a single script is specified.  This is rather crude,
             and is only used to narrow down the fonts returned where
             there is a definite match.  Some scripts, such as latin, han,
             cjk-misc match multiple lfCharSet values, so we can't pre-filter
             them.  */
	  if (EQ (key, QCscript)
                   && logfont->lfCharSet == DEFAULT_CHARSET
                   && SYMBOLP (val))
            {
              if (EQ (val, Qgreek))
                logfont->lfCharSet = GREEK_CHARSET;
              else if (EQ (val, Qhangul))
                logfont->lfCharSet = HANGUL_CHARSET;
              else if (EQ (val, Qkana) || EQ (val, Qkanbun))
                logfont->lfCharSet = SHIFTJIS_CHARSET;
              else if (EQ (val, Qbopomofo))
                logfont->lfCharSet = CHINESEBIG5_CHARSET;
              /* GB 18030 supports tibetan, yi, mongolian,
                 fonts that support it should show up if we ask for
                 GB2312 fonts. */
              else if (EQ (val, Qtibetan) || EQ (val, Qyi)
                       || EQ (val, Qmongolian))
                logfont->lfCharSet = GB2312_CHARSET;
              else if (EQ (val, Qhebrew))
                logfont->lfCharSet = HEBREW_CHARSET;
              else if (EQ (val, Qarabic))
                logfont->lfCharSet = ARABIC_CHARSET;
              else if (EQ (val, Qthai))
                logfont->lfCharSet = THAI_CHARSET;
            }
          else if (EQ (key, QCantialias) && SYMBOLP (val))
            {
              logfont->lfQuality = w32_antialias_type (val);
            }
        }
    }
}

static void
list_all_matching_fonts (struct font_callback_data *match_data)
{
  HDC dc;
  Lisp_Object families = w32font_list_family (match_data->frame);
  struct frame *f = XFRAME (match_data->frame);

  dc = get_frame_dc (f);

  while (!NILP (families))
    {
      /* Only fonts from the current locale are given localized names
	 on Windows, so we can keep backwards compatibility with
	 Windows 9x/ME by using non-Unicode font enumeration without
	 sacrificing internationalization here.  */
      char *name;
      Lisp_Object family = CAR (families);
      families = CDR (families);
      if (NILP (family))
        continue;
      else if (SYMBOLP (family))
        name = SDATA (ENCODE_SYSTEM (SYMBOL_NAME (family)));
      else
	continue;

      strncpy (match_data->pattern.lfFaceName, name, LF_FACESIZE);
      match_data->pattern.lfFaceName[LF_FACESIZE - 1] = '\0';

      EnumFontFamiliesEx (dc, &match_data->pattern,
                          (FONTENUMPROC) add_font_entity_to_list,
                          (LPARAM) match_data, 0);
    }

  release_frame_dc (f, dc);
}

static Lisp_Object
lispy_antialias_type (BYTE type)
{
  Lisp_Object lispy;

  switch (type)
    {
    case NONANTIALIASED_QUALITY:
      lispy = Qnone;
      break;
    case ANTIALIASED_QUALITY:
      lispy = Qstandard;
      break;
    case CLEARTYPE_QUALITY:
      lispy = Qsubpixel;
      break;
    case CLEARTYPE_NATURAL_QUALITY:
      lispy = Qnatural;
      break;
    default:
      lispy = Qnil;
      break;
    }
  return lispy;
}

/* Convert antialiasing symbols to lfQuality  */
static BYTE
w32_antialias_type (Lisp_Object type)
{
  if (EQ (type, Qnone))
    return NONANTIALIASED_QUALITY;
  else if (EQ (type, Qstandard))
    return ANTIALIASED_QUALITY;
  else if (EQ (type, Qsubpixel))
    return CLEARTYPE_QUALITY;
  else if (EQ (type, Qnatural))
    return CLEARTYPE_NATURAL_QUALITY;
  else
    return DEFAULT_QUALITY;
}

/* Return a list of all the scripts that the font supports.  */
static Lisp_Object
font_supported_scripts (FONTSIGNATURE * sig)
{
  DWORD * subranges = sig->fsUsb;
  Lisp_Object supported = Qnil;

  /* Match a single subrange. SYM is set if bit N is set in subranges.  */
#define SUBRANGE(n,sym) \
  if (subranges[(n) / 32] & (1 << ((n) % 32))) \
    supported = Fcons ((sym), supported)

  /* Match multiple subranges. SYM is set if any MASK bit is set in
     subranges[0 - 3].  */
#define MASK_ANY(mask0,mask1,mask2,mask3,sym)      \
  if ((subranges[0] & (mask0)) || (subranges[1] & (mask1))     \
      || (subranges[2] & (mask2)) || (subranges[3] & (mask3))) \
    supported = Fcons ((sym), supported)

  SUBRANGE (0, Qlatin);
  /* The following count as latin too, ASCII should be present in these fonts,
     so don't need to mark them separately.  */
  /* 1: Latin-1 supplement, 2: Latin Extended A, 3: Latin Extended B.  */
  SUBRANGE (4, Qphonetic);
  /* 5: Spacing and tone modifiers, 6: Combining Diacritical Marks.  */
  SUBRANGE (7, Qgreek);
  SUBRANGE (8, Qcoptic);
  SUBRANGE (9, Qcyrillic);
  SUBRANGE (10, Qarmenian);
  SUBRANGE (11, Qhebrew);
  /* 12: Vai.  */
  SUBRANGE (13, Qarabic);
  SUBRANGE (14, Qnko);
  SUBRANGE (15, Qdevanagari);
  SUBRANGE (16, Qbengali);
  SUBRANGE (17, Qgurmukhi);
  SUBRANGE (18, Qgujarati);
  SUBRANGE (19, Qoriya);
  SUBRANGE (20, Qtamil);
  SUBRANGE (21, Qtelugu);
  SUBRANGE (22, Qkannada);
  SUBRANGE (23, Qmalayalam);
  SUBRANGE (24, Qthai);
  SUBRANGE (25, Qlao);
  SUBRANGE (26, Qgeorgian);
  SUBRANGE (27, Qbalinese);
  /* 28: Hangul Jamo.  */
  /* 29: Latin Extended, 30: Greek Extended, 31: Punctuation.  */
  /* 32-47: Symbols (defined below).  */
  SUBRANGE (48, Qcjk_misc);
  /* Match either 49: katakana or 50: hiragana for kana.  */
  MASK_ANY (0, 0x00060000, 0, 0, Qkana);
  SUBRANGE (51, Qbopomofo);
  /* 52: Compatibility Jamo */
  SUBRANGE (53, Qphags_pa);
  /* 54: Enclosed CJK letters and months, 55: CJK Compatibility.  */
  SUBRANGE (56, Qhangul);
  /* 57: Surrogates.  */
  SUBRANGE (58, Qphoenician);
  SUBRANGE (59, Qhan); /* There are others, but this is the main one.  */
  SUBRANGE (59, Qideographic_description); /* Windows lumps this in.  */
  SUBRANGE (59, Qkanbun); /* And this.  */
  /* 60: Private use, 61: CJK strokes and compatibility.  */
  /* 62: Alphabetic Presentation, 63: Arabic Presentation A.  */
  /* 64: Combining half marks, 65: Vertical and CJK compatibility.  */
  /* 66: Small forms, 67: Arabic Presentation B, 68: Half and Full width.  */
  /* 69: Specials.  */
  SUBRANGE (70, Qtibetan);
  SUBRANGE (71, Qsyriac);
  SUBRANGE (72, Qthaana);
  SUBRANGE (73, Qsinhala);
  SUBRANGE (74, Qmyanmar);
  SUBRANGE (75, Qethiopic);
  SUBRANGE (76, Qcherokee);
  SUBRANGE (77, Qcanadian_aboriginal);
  SUBRANGE (78, Qogham);
  SUBRANGE (79, Qrunic);
  SUBRANGE (80, Qkhmer);
  SUBRANGE (81, Qmongolian);
  SUBRANGE (82, Qbraille);
  SUBRANGE (83, Qyi);
  SUBRANGE (84, Qbuhid);
  SUBRANGE (84, Qhanunoo);
  SUBRANGE (84, Qtagalog);
  SUBRANGE (84, Qtagbanwa);
  SUBRANGE (85, Qold_italic);
  SUBRANGE (86, Qgothic);
  SUBRANGE (87, Qdeseret);
  SUBRANGE (88, Qbyzantine_musical_symbol);
  SUBRANGE (88, Qmusical_symbol); /* Windows doesn't distinguish these.  */
  SUBRANGE (89, Qmathematical);
  /* 90: Private use, 91: Variation selectors, 92: Tags.  */
  SUBRANGE (93, Qlimbu);
  SUBRANGE (94, Qtai_le);
  /* 95: New Tai Le */
  SUBRANGE (90, Qbuginese);
  SUBRANGE (97, Qglagolitic);
  SUBRANGE (98, Qtifinagh);
  /* 99: Yijing Hexagrams.  */
  SUBRANGE (100, Qsyloti_nagri);
  SUBRANGE (101, Qlinear_b);
  /* 102: Ancient Greek Numbers.  */
  SUBRANGE (103, Qugaritic);
  SUBRANGE (104, Qold_persian);
  SUBRANGE (105, Qshavian);
  SUBRANGE (106, Qosmanya);
  SUBRANGE (107, Qcypriot);
  SUBRANGE (108, Qkharoshthi);
  /* 109: Tai Xuan Jing.  */
  SUBRANGE (110, Qcuneiform);
  /* 111: Counting Rods, 112: Sundanese, 113: Lepcha, 114: Ol Chiki.  */
  /* 115: Saurashtra, 116: Kayah Li, 117: Rejang.  */
  SUBRANGE (118, Qcham);
  /* 119: Ancient symbols, 120: Phaistos Disc.  */
  /* 121: Carian, Lycian, Lydian, 122: Dominoes, Mahjong tiles.  */
  /* 123-127: Reserved.  */

  /* There isn't really a main symbol range, so include symbol if any
     relevant range is set.  */
  MASK_ANY (0x8000000, 0x0000FFFF, 0, 0, Qsymbol);

  /* Missing: Tai Viet (U+AA80-U+AADF).  */
#undef SUBRANGE
#undef MASK_ANY

  return supported;
}

/* Generate a full name for a Windows font.
   The full name is in fcname format, with weight, slant and antialiasing
   specified if they are not "normal".  */
static int
w32font_full_name (LOGFONT * font, Lisp_Object font_obj,
		   int pixel_size, char *name, int nbytes)
{
  int len, height, outline;
  char *p;
  Lisp_Object antialiasing, weight = Qnil;

  len = strlen (font->lfFaceName);

  outline = EQ (AREF (font_obj, FONT_FOUNDRY_INDEX), Qoutline);

  /* Represent size of scalable fonts by point size. But use pixelsize for
     raster fonts to indicate that they are exactly that size.  */
  if (outline)
    len += 11; /* -SIZE */
  else
    len += 21;

  if (font->lfItalic)
    len += 7; /* :italic */

  if (font->lfWeight && font->lfWeight != FW_NORMAL)
    {
      weight = w32_to_fc_weight (font->lfWeight);
      len += 1 + SBYTES (SYMBOL_NAME (weight)); /* :WEIGHT */
    }

  antialiasing = lispy_antialias_type (font->lfQuality);
  if (! NILP (antialiasing))
    len += 11 + SBYTES (SYMBOL_NAME (antialiasing)); /* :antialias=NAME */

  /* Check that the buffer is big enough  */
  if (len > nbytes)
    return -1;

  p = name;
  p += sprintf (p, "%s", font->lfFaceName);

  height = font->lfHeight ? eabs (font->lfHeight) : pixel_size;

  if (height > 0)
    {
      if (outline)
        {
          float pointsize = height * 72.0 / one_w32_display_info.resy;
          /* Round to nearest half point.  floor is used, since round is not
	     supported in MS library.  */
          pointsize = floor (pointsize * 2 + 0.5) / 2;
          p += sprintf (p, "-%1.1f", pointsize);
        }
      else
        p += sprintf (p, ":pixelsize=%d", height);
    }

  if (SYMBOLP (weight) && ! NILP (weight))
    p += sprintf (p, ":%s", SDATA (SYMBOL_NAME (weight)));

  if (font->lfItalic)
    p += sprintf (p, ":italic");

  if (SYMBOLP (antialiasing) && ! NILP (antialiasing))
    p += sprintf (p, ":antialias=%s", SDATA (SYMBOL_NAME (antialiasing)));

  return (p - name);
}

/* Convert a logfont and point size into a fontconfig style font name.
   POINTSIZE is in tenths of points.
   If SIZE indicates the size of buffer FCNAME, into which the font name
   is written.  If the buffer is not large enough to contain the name,
   the function returns -1, otherwise it returns the number of bytes
   written to FCNAME.  */
static int
logfont_to_fcname (LOGFONT* font, int pointsize, char *fcname, int size)
{
  int len, height;
  char *p = fcname;
  Lisp_Object weight = Qnil;

  len = strlen (font->lfFaceName) + 2;
  height = pointsize / 10;
  while (height /= 10)
    len++;

  if (pointsize % 10)
    len += 2;

  if (font->lfItalic)
    len += 7; /* :italic */
  if (font->lfWeight && font->lfWeight != FW_NORMAL)
    {
      weight = w32_to_fc_weight (font->lfWeight);
      len += SBYTES (SYMBOL_NAME (weight)) + 1;
    }

  if (len > size)
    return -1;

  p += sprintf (p, "%s-%d", font->lfFaceName, pointsize / 10);
  if (pointsize % 10)
    p += sprintf (p, ".%d", pointsize % 10);

  if (SYMBOLP (weight) && !NILP (weight))
    p += sprintf (p, ":%s", SDATA (SYMBOL_NAME (weight)));

  if (font->lfItalic)
    p += sprintf (p, ":italic");

  return (p - fcname);
}

static void
compute_metrics (HDC dc, struct w32font_info *w32_font, unsigned int code,
		 struct w32_metric_cache *metrics)
{
  GLYPHMETRICS gm;
  MAT2 transform;
  unsigned int options = GGO_METRICS;

  if (w32_font->glyph_idx)
    options |= GGO_GLYPH_INDEX;

  memset (&transform, 0, sizeof (transform));
  transform.eM11.value = 1;
  transform.eM22.value = 1;

  if (get_glyph_outline_w (dc, code, options, &gm, 0, NULL, &transform)
      != GDI_ERROR)
    {
      metrics->lbearing = gm.gmptGlyphOrigin.x;
      metrics->rbearing = gm.gmptGlyphOrigin.x + gm.gmBlackBoxX;
      metrics->width = gm.gmCellIncX;
      metrics->status = W32METRIC_SUCCESS;
    }
  else
    metrics->status = W32METRIC_FAIL;
}

DEFUN ("x-select-font", Fx_select_font, Sx_select_font, 0, 2, 0,
       doc: /* Read a font name using a W32 font selection dialog.
Return fontconfig style font string corresponding to the selection.

If FRAME is omitted or nil, it defaults to the selected frame.
If EXCLUDE-PROPORTIONAL is non-nil, exclude proportional fonts
in the font selection dialog. */)
  (Lisp_Object frame, Lisp_Object exclude_proportional)
{
  FRAME_PTR f = check_x_frame (frame);
  CHOOSEFONT cf;
  LOGFONT lf;
  TEXTMETRIC tm;
  HDC hdc;
  HANDLE oldobj;
  char buf[100];

  memset (&cf, 0, sizeof (cf));
  memset (&lf, 0, sizeof (lf));

  cf.lStructSize = sizeof (cf);
  cf.hwndOwner = FRAME_W32_WINDOW (f);
  cf.Flags = CF_FORCEFONTEXIST | CF_SCREENFONTS | CF_NOVERTFONTS;

  /* If exclude_proportional is non-nil, limit the selection to
     monospaced fonts.  */
  if (!NILP (exclude_proportional))
    cf.Flags |= CF_FIXEDPITCHONLY;

  cf.lpLogFont = &lf;

  /* Initialize as much of the font details as we can from the current
     default font.  */
  hdc = GetDC (FRAME_W32_WINDOW (f));
  oldobj = SelectObject (hdc, FONT_HANDLE (FRAME_FONT (f)));
  GetTextFace (hdc, LF_FACESIZE, lf.lfFaceName);
  if (GetTextMetrics (hdc, &tm))
    {
      lf.lfHeight = tm.tmInternalLeading - tm.tmHeight;
      lf.lfWeight = tm.tmWeight;
      lf.lfItalic = tm.tmItalic;
      lf.lfUnderline = tm.tmUnderlined;
      lf.lfStrikeOut = tm.tmStruckOut;
      lf.lfCharSet = tm.tmCharSet;
      cf.Flags |= CF_INITTOLOGFONTSTRUCT;
    }
  SelectObject (hdc, oldobj);
  ReleaseDC (FRAME_W32_WINDOW (f), hdc);

  if (!ChooseFont (&cf)
      || logfont_to_fcname (&lf, cf.iPointSize, buf, 100) < 0)
    return Qnil;

  return DECODE_SYSTEM (build_string (buf));
}

static const char *const w32font_booleans [] = {
  NULL,
};

static const char *const w32font_non_booleans [] = {
  ":script",
  ":antialias",
  ":style",
  NULL,
};

static void
w32font_filter_properties (Lisp_Object font, Lisp_Object alist)
{
  font_filter_properties (font, alist, w32font_booleans, w32font_non_booleans);
}

struct font_driver w32font_driver =
  {
    0, /* Qgdi */
    0, /* case insensitive */
    w32font_get_cache,
    w32font_list,
    w32font_match,
    w32font_list_family,
    NULL, /* free_entity */
    w32font_open,
    w32font_close,
    NULL, /* prepare_face */
    NULL, /* done_face */
    w32font_has_char,
    w32font_encode_char,
    w32font_text_extents,
    w32font_draw,
    NULL, /* get_bitmap */
    NULL, /* free_bitmap */
    NULL, /* get_outline */
    NULL, /* free_outline */
    NULL, /* anchor_point */
    NULL, /* otf_capability */
    NULL, /* otf_drive */
    NULL, /* start_for_frame */
    NULL, /* end_for_frame */
    NULL, /* shape */
    NULL, /* check */
    NULL, /* get_variation_glyphs */
    w32font_filter_properties,
    NULL, /* cached_font_ok */
  };


/* Initialize state that does not change between invocations. This is only
   called when Emacs is dumped.  */
void
syms_of_w32font (void)
{
  DEFSYM (Qgdi, "gdi");
  DEFSYM (Quniscribe, "uniscribe");
  DEFSYM (QCformat, ":format");

  /* Generic font families.  */
  DEFSYM (Qmonospace, "monospace");
  DEFSYM (Qserif, "serif");
  DEFSYM (Qsansserif, "sansserif");
  DEFSYM (Qscript, "script");
  DEFSYM (Qdecorative, "decorative");
  /* Aliases.  */
  DEFSYM (Qsans_serif, "sans_serif");
  DEFSYM (Qsans, "sans");
  DEFSYM (Qmono, "mono");

  /* Fake foundries.  */
  DEFSYM (Qraster, "raster");
  DEFSYM (Qoutline, "outline");
  DEFSYM (Qunknown, "unknown");

  /* Antialiasing.  */
  DEFSYM (Qstandard, "standard");
  DEFSYM (Qsubpixel, "subpixel");
  DEFSYM (Qnatural, "natural");

  /* Languages  */
  DEFSYM (Qzh, "zh");

  /* Scripts  */
  DEFSYM (Qlatin, "latin");
  DEFSYM (Qgreek, "greek");
  DEFSYM (Qcoptic, "coptic");
  DEFSYM (Qcyrillic, "cyrillic");
  DEFSYM (Qarmenian, "armenian");
  DEFSYM (Qhebrew, "hebrew");
  DEFSYM (Qarabic, "arabic");
  DEFSYM (Qsyriac, "syriac");
  DEFSYM (Qnko, "nko");
  DEFSYM (Qthaana, "thaana");
  DEFSYM (Qdevanagari, "devanagari");
  DEFSYM (Qbengali, "bengali");
  DEFSYM (Qgurmukhi, "gurmukhi");
  DEFSYM (Qgujarati, "gujarati");
  DEFSYM (Qoriya, "oriya");
  DEFSYM (Qtamil, "tamil");
  DEFSYM (Qtelugu, "telugu");
  DEFSYM (Qkannada, "kannada");
  DEFSYM (Qmalayalam, "malayalam");
  DEFSYM (Qsinhala, "sinhala");
  DEFSYM (Qthai, "thai");
  DEFSYM (Qlao, "lao");
  DEFSYM (Qtibetan, "tibetan");
  DEFSYM (Qmyanmar, "myanmar");
  DEFSYM (Qgeorgian, "georgian");
  DEFSYM (Qhangul, "hangul");
  DEFSYM (Qethiopic, "ethiopic");
  DEFSYM (Qcherokee, "cherokee");
  DEFSYM (Qcanadian_aboriginal, "canadian-aboriginal");
  DEFSYM (Qogham, "ogham");
  DEFSYM (Qrunic, "runic");
  DEFSYM (Qkhmer, "khmer");
  DEFSYM (Qmongolian, "mongolian");
  DEFSYM (Qsymbol, "symbol");
  DEFSYM (Qbraille, "braille");
  DEFSYM (Qhan, "han");
  DEFSYM (Qideographic_description, "ideographic-description");
  DEFSYM (Qcjk_misc, "cjk-misc");
  DEFSYM (Qkana, "kana");
  DEFSYM (Qbopomofo, "bopomofo");
  DEFSYM (Qkanbun, "kanbun");
  DEFSYM (Qyi, "yi");
  DEFSYM (Qbyzantine_musical_symbol, "byzantine-musical-symbol");
  DEFSYM (Qmusical_symbol, "musical-symbol");
  DEFSYM (Qmathematical, "mathematical");
  DEFSYM (Qcham, "cham");
  DEFSYM (Qphonetic, "phonetic");
  DEFSYM (Qbalinese, "balinese");
  DEFSYM (Qbuginese, "buginese");
  DEFSYM (Qbuhid, "buhid");
  DEFSYM (Qcuneiform, "cuneiform");
  DEFSYM (Qcypriot, "cypriot");
  DEFSYM (Qdeseret, "deseret");
  DEFSYM (Qglagolitic, "glagolitic");
  DEFSYM (Qgothic, "gothic");
  DEFSYM (Qhanunoo, "hanunoo");
  DEFSYM (Qkharoshthi, "kharoshthi");
  DEFSYM (Qlimbu, "limbu");
  DEFSYM (Qlinear_b, "linear_b");
  DEFSYM (Qold_italic, "old_italic");
  DEFSYM (Qold_persian, "old_persian");
  DEFSYM (Qosmanya, "osmanya");
  DEFSYM (Qphags_pa, "phags-pa");
  DEFSYM (Qphoenician, "phoenician");
  DEFSYM (Qshavian, "shavian");
  DEFSYM (Qsyloti_nagri, "syloti_nagri");
  DEFSYM (Qtagalog, "tagalog");
  DEFSYM (Qtagbanwa, "tagbanwa");
  DEFSYM (Qtai_le, "tai_le");
  DEFSYM (Qtifinagh, "tifinagh");
  DEFSYM (Qugaritic, "ugaritic");

  /* W32 font encodings.  */
  DEFVAR_LISP ("w32-charset-info-alist",
               Vw32_charset_info_alist,
               doc: /* Alist linking Emacs character sets to Windows fonts and codepages.
Each entry should be of the form:

   (CHARSET_NAME . (WINDOWS_CHARSET . CODEPAGE))

where CHARSET_NAME is a string used in font names to identify the charset,
WINDOWS_CHARSET is a symbol that can be one of:

  w32-charset-ansi, w32-charset-default, w32-charset-symbol,
  w32-charset-shiftjis, w32-charset-hangeul, w32-charset-gb2312,
  w32-charset-chinesebig5, w32-charset-johab, w32-charset-hebrew,
  w32-charset-arabic, w32-charset-greek, w32-charset-turkish,
  w32-charset-vietnamese, w32-charset-thai, w32-charset-easteurope,
  w32-charset-russian, w32-charset-mac, w32-charset-baltic,
  or w32-charset-oem.

CODEPAGE should be an integer specifying the codepage that should be used
to display the character set, t to do no translation and output as Unicode,
or nil to do no translation and output as 8 bit (or multibyte on far-east
versions of Windows) characters.  */);
  Vw32_charset_info_alist = Qnil;

  DEFSYM (Qw32_charset_ansi, "w32-charset-ansi");
  DEFSYM (Qw32_charset_symbol, "w32-charset-symbol");
  DEFSYM (Qw32_charset_default, "w32-charset-default");
  DEFSYM (Qw32_charset_shiftjis, "w32-charset-shiftjis");
  DEFSYM (Qw32_charset_hangeul, "w32-charset-hangeul");
  DEFSYM (Qw32_charset_chinesebig5, "w32-charset-chinesebig5");
  DEFSYM (Qw32_charset_gb2312, "w32-charset-gb2312");
  DEFSYM (Qw32_charset_oem, "w32-charset-oem");
  DEFSYM (Qw32_charset_johab, "w32-charset-johab");
  DEFSYM (Qw32_charset_easteurope, "w32-charset-easteurope");
  DEFSYM (Qw32_charset_turkish, "w32-charset-turkish");
  DEFSYM (Qw32_charset_baltic, "w32-charset-baltic");
  DEFSYM (Qw32_charset_russian, "w32-charset-russian");
  DEFSYM (Qw32_charset_arabic, "w32-charset-arabic");
  DEFSYM (Qw32_charset_greek, "w32-charset-greek");
  DEFSYM (Qw32_charset_hebrew, "w32-charset-hebrew");
  DEFSYM (Qw32_charset_vietnamese, "w32-charset-vietnamese");
  DEFSYM (Qw32_charset_thai, "w32-charset-thai");
  DEFSYM (Qw32_charset_mac, "w32-charset-mac");

  defsubr (&Sx_select_font);

  w32font_driver.type = Qgdi;
  register_font_driver (&w32font_driver, NULL);
}

void
globals_of_w32font (void)
{
  g_b_init_is_w9x = 0;
  g_b_init_get_outline_metrics_w = 0;
  g_b_init_get_text_metrics_w = 0;
  g_b_init_get_glyph_outline_w = 0;
}
