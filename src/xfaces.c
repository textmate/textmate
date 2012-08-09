/* xfaces.c -- "Face" primitives.

Copyright (C) 1993-1994, 1998-2012  Free Software Foundation, Inc.

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

/* New face implementation by Gerd Moellmann <gerd@gnu.org>.  */

/* Faces.

   When using Emacs with X, the display style of characters can be
   changed by defining `faces'.  Each face can specify the following
   display attributes:

   1. Font family name.

   2. Font foundry name.

   3. Relative proportionate width, aka character set width or set
   width (swidth), e.g. `semi-compressed'.

   4. Font height in 1/10pt.

   5. Font weight, e.g. `bold'.

   6. Font slant, e.g. `italic'.

   7. Foreground color.

   8. Background color.

   9. Whether or not characters should be underlined, and in what color.

   10. Whether or not characters should be displayed in inverse video.

   11. A background stipple, a bitmap.

   12. Whether or not characters should be overlined, and in what color.

   13. Whether or not characters should be strike-through, and in what
   color.

   14. Whether or not a box should be drawn around characters, the box
   type, and, for simple boxes, in what color.

   15. Font-spec, or nil.  This is a special attribute.

   A font-spec is a collection of font attributes (specs).

   When this attribute is specified, the face uses a font matching
   with the specs as is except for what overwritten by the specs in
   the fontset (see below).  In addition, the other font-related
   attributes (1st thru 5th) are updated from the spec.

   On the other hand, if one of the other font-related attributes are
   specified, the corresponding specs in this attribute is set to nil.

   15. A face name or list of face names from which to inherit attributes.

   16. A specified average font width, which is invisible from Lisp,
   and is used to ensure that a font specified on the command line,
   for example, can be matched exactly.

   17. A fontset name.  This is another special attribute.

   A fontset is a mappings from characters to font-specs, and the
   specs overwrite the font-spec in the 14th attribute.


   Faces are frame-local by nature because Emacs allows to define the
   same named face (face names are symbols) differently for different
   frames.  Each frame has an alist of face definitions for all named
   faces.  The value of a named face in such an alist is a Lisp vector
   with the symbol `face' in slot 0, and a slot for each of the face
   attributes mentioned above.

   There is also a global face alist `Vface_new_frame_defaults'.  Face
   definitions from this list are used to initialize faces of newly
   created frames.

   A face doesn't have to specify all attributes.  Those not specified
   have a value of `unspecified'.  Faces specifying all attributes but
   the 14th are called `fully-specified'.


   Face merging.

   The display style of a given character in the text is determined by
   combining several faces.  This process is called `face merging'.
   Any aspect of the display style that isn't specified by overlays or
   text properties is taken from the `default' face.  Since it is made
   sure that the default face is always fully-specified, face merging
   always results in a fully-specified face.


   Face realization.

   After all face attributes for a character have been determined by
   merging faces of that character, that face is `realized'.  The
   realization process maps face attributes to what is physically
   available on the system where Emacs runs.  The result is a
   `realized face' in form of a struct face which is stored in the
   face cache of the frame on which it was realized.

   Face realization is done in the context of the character to display
   because different fonts may be used for different characters.  In
   other words, for characters that have different font
   specifications, different realized faces are needed to display
   them.

   Font specification is done by fontsets.  See the comment in
   fontset.c for the details.  In the current implementation, all ASCII
   characters share the same font in a fontset.

   Faces are at first realized for ASCII characters, and, at that
   time, assigned a specific realized fontset.  Hereafter, we call
   such a face as `ASCII face'.  When a face for a multibyte character
   is realized, it inherits (thus shares) a fontset of an ASCII face
   that has the same attributes other than font-related ones.

   Thus, all realized faces have a realized fontset.


   Unibyte text.

   Unibyte text (i.e. raw 8-bit characters) is displayed with the same
   font as ASCII characters.  That is because it is expected that
   unibyte text users specify a font that is suitable both for ASCII
   and raw 8-bit characters.


   Font selection.

   Font selection tries to find the best available matching font for a
   given (character, face) combination.

   If the face specifies a fontset name, that fontset determines a
   pattern for fonts of the given character.  If the face specifies a
   font name or the other font-related attributes, a fontset is
   realized from the default fontset.  In that case, that
   specification determines a pattern for ASCII characters and the
   default fontset determines a pattern for multibyte characters.

   Available fonts on the system on which Emacs runs are then matched
   against the font pattern.  The result of font selection is the best
   match for the given face attributes in this font list.

   Font selection can be influenced by the user.

   1. The user can specify the relative importance he gives the face
   attributes width, height, weight, and slant by setting
   face-font-selection-order (faces.el) to a list of face attribute
   names.  The default is '(:width :height :weight :slant), and means
   that font selection first tries to find a good match for the font
   width specified by a face, then---within fonts with that
   width---tries to find a best match for the specified font height,
   etc.

   2. Setting face-font-family-alternatives allows the user to
   specify alternative font families to try if a family specified by a
   face doesn't exist.

   3. Setting face-font-registry-alternatives allows the user to
   specify all alternative font registries to try for a face
   specifying a registry.

   4. Setting face-ignored-fonts allows the user to ignore specific
   fonts.


   Character composition.

   Usually, the realization process is already finished when Emacs
   actually reflects the desired glyph matrix on the screen.  However,
   on displaying a composition (sequence of characters to be composed
   on the screen), a suitable font for the components of the
   composition is selected and realized while drawing them on the
   screen, i.e.  the realization process is delayed but in principle
   the same.


   Initialization of basic faces.

   The faces `default', `modeline' are considered `basic faces'.
   When redisplay happens the first time for a newly created frame,
   basic faces are realized for CHARSET_ASCII.  Frame parameters are
   used to fill in unspecified attributes of the default face.  */

#include <config.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>              /* This needs to be before termchar.h */
#include <setjmp.h>

#include "lisp.h"
#include "character.h"
#include "charset.h"
#include "keyboard.h"
#include "frame.h"
#include "termhooks.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#ifdef USE_MOTIF
#include <Xm/Xm.h>
#include <Xm/XmStrDefs.h>
#endif /* USE_MOTIF */
#endif /* HAVE_X_WINDOWS */

#ifdef MSDOS
#include "dosfns.h"
#endif

#ifdef WINDOWSNT
#include "w32term.h"
#include "fontset.h"
/* Redefine X specifics to W32 equivalents to avoid cluttering the
   code with #ifdef blocks. */
#undef FRAME_X_DISPLAY_INFO
#define FRAME_X_DISPLAY_INFO FRAME_W32_DISPLAY_INFO
#define x_display_info w32_display_info
#define FRAME_X_FONT_TABLE FRAME_W32_FONT_TABLE
#define check_x check_w32
#define GCGraphicsExposures 0
#endif /* WINDOWSNT */

#ifdef HAVE_NS
#include "nsterm.h"
#undef FRAME_X_DISPLAY_INFO
#define FRAME_X_DISPLAY_INFO FRAME_NS_DISPLAY_INFO
#define x_display_info ns_display_info
#define FRAME_X_FONT_TABLE FRAME_NS_FONT_TABLE
#define check_x check_ns
#define GCGraphicsExposures 0
#endif /* HAVE_NS */

#include "buffer.h"
#include "dispextern.h"
#include "blockinput.h"
#include "window.h"
#include "intervals.h"
#include "termchar.h"

#include "font.h"
#ifdef HAVE_WINDOW_SYSTEM
#include "fontset.h"
#endif /* HAVE_WINDOW_SYSTEM */

#ifdef HAVE_X_WINDOWS

/* Compensate for a bug in Xos.h on some systems, on which it requires
   time.h.  On some such systems, Xos.h tries to redefine struct
   timeval and struct timezone if USG is #defined while it is
   #included.  */

#ifdef XOS_NEEDS_TIME_H
#include <time.h>
#undef USG
#include <X11/Xos.h>
#define USG
#define __TIMEVAL__
#if defined USG || defined __TIMEVAL__ /* Don't warn about unused macros.  */
#endif
#else /* not XOS_NEEDS_TIME_H */
#include <X11/Xos.h>
#endif /* not XOS_NEEDS_TIME_H */

#endif /* HAVE_X_WINDOWS */

#include <ctype.h>

/* Number of pt per inch (from the TeXbook).  */

#define PT_PER_INCH 72.27

/* Non-zero if face attribute ATTR is unspecified.  */

#define UNSPECIFIEDP(ATTR) EQ ((ATTR), Qunspecified)

/* Non-zero if face attribute ATTR is `ignore-defface'.  */

#define IGNORE_DEFFACE_P(ATTR) EQ ((ATTR), QCignore_defface)

/* Value is the number of elements of VECTOR.  */

#define DIM(VECTOR) (sizeof (VECTOR) / sizeof *(VECTOR))

/* Size of hash table of realized faces in face caches (should be a
   prime number).  */

#define FACE_CACHE_BUCKETS_SIZE 1001

/* Keyword symbols used for face attribute names.  */

Lisp_Object QCfamily, QCheight, QCweight, QCslant;
static Lisp_Object QCunderline;
static Lisp_Object QCinverse_video, QCstipple;
Lisp_Object QCforeground, QCbackground;
Lisp_Object QCwidth;
static Lisp_Object QCfont, QCbold, QCitalic;
static Lisp_Object QCreverse_video;
static Lisp_Object QCoverline, QCstrike_through, QCbox, QCinherit;
static Lisp_Object QCfontset;

/* Symbols used for attribute values.  */

Lisp_Object Qnormal;
Lisp_Object Qbold;
static Lisp_Object Qultra_light, Qextra_light, Qlight;
static Lisp_Object Qsemi_light, Qsemi_bold, Qextra_bold, Qultra_bold;
static Lisp_Object Qoblique, Qreverse_oblique, Qreverse_italic;
Lisp_Object Qitalic;
static Lisp_Object Qultra_condensed, Qextra_condensed;
Lisp_Object Qcondensed;
static Lisp_Object Qsemi_condensed, Qsemi_expanded, Qextra_expanded;
Lisp_Object Qexpanded;
static Lisp_Object Qultra_expanded;
static Lisp_Object Qreleased_button, Qpressed_button;
static Lisp_Object QCstyle, QCcolor, QCline_width;
Lisp_Object Qunspecified;	/* used in dosfns.c */
static Lisp_Object QCignore_defface;

char unspecified_fg[] = "unspecified-fg", unspecified_bg[] = "unspecified-bg";

/* The name of the function to call when the background of the frame
   has changed, frame_set_background_mode.  */

static Lisp_Object Qframe_set_background_mode;

/* Names of basic faces.  */

Lisp_Object Qdefault, Qtool_bar, Qfringe;
static Lisp_Object Qregion;
Lisp_Object Qheader_line, Qscroll_bar, Qcursor;
static Lisp_Object Qborder, Qmouse, Qmenu;
Lisp_Object Qmode_line_inactive;
static Lisp_Object Qvertical_border;

/* The symbol `face-alias'.  A symbols having that property is an
   alias for another face.  Value of the property is the name of
   the aliased face.  */

static Lisp_Object Qface_alias;

/* Alist of alternative font families.  Each element is of the form
   (FAMILY FAMILY1 FAMILY2 ...).  If fonts of FAMILY can't be loaded,
   try FAMILY1, then FAMILY2, ...  */

Lisp_Object Vface_alternative_font_family_alist;

/* Alist of alternative font registries.  Each element is of the form
   (REGISTRY REGISTRY1 REGISTRY2...).  If fonts of REGISTRY can't be
   loaded, try REGISTRY1, then REGISTRY2, ...  */

Lisp_Object Vface_alternative_font_registry_alist;

/* Allowed scalable fonts.  A value of nil means don't allow any
   scalable fonts.  A value of t means allow the use of any scalable
   font.  Otherwise, value must be a list of regular expressions.  A
   font may be scaled if its name matches a regular expression in the
   list.  */

static Lisp_Object Qscalable_fonts_allowed;

#define DEFAULT_FONT_LIST_LIMIT 100

/* The symbols `foreground-color' and `background-color' which can be
   used as part of a `face' property.  This is for compatibility with
   Emacs 20.2.  */

Lisp_Object Qforeground_color, Qbackground_color;

/* The symbols `face' and `mouse-face' used as text properties.  */

Lisp_Object Qface;

/* Property for basic faces which other faces cannot inherit.  */

static Lisp_Object Qface_no_inherit;

/* Error symbol for wrong_type_argument in load_pixmap.  */

static Lisp_Object Qbitmap_spec_p;

/* The next ID to assign to Lisp faces.  */

static int next_lface_id;

/* A vector mapping Lisp face Id's to face names.  */

static Lisp_Object *lface_id_to_name;
static ptrdiff_t lface_id_to_name_size;

/* TTY color-related functions (defined in tty-colors.el).  */

static Lisp_Object Qtty_color_desc, Qtty_color_by_index, Qtty_color_standard_values;

/* The name of the function used to compute colors on TTYs.  */

static Lisp_Object Qtty_color_alist;

/* Counter for calls to clear_face_cache.  If this counter reaches
   CLEAR_FONT_TABLE_COUNT, and a frame has more than
   CLEAR_FONT_TABLE_NFONTS load, unused fonts are freed.  */

static int clear_font_table_count;
#define CLEAR_FONT_TABLE_COUNT	100
#define CLEAR_FONT_TABLE_NFONTS	10

/* Non-zero means face attributes have been changed since the last
   redisplay.  Used in redisplay_internal.  */

int face_change_count;

/* Non-zero means don't display bold text if a face's foreground
   and background colors are the inverse of the default colors of the
   display.   This is a kluge to suppress `bold black' foreground text
   which is hard to read on an LCD monitor.  */

static int tty_suppress_bold_inverse_default_colors_p;

/* A list of the form `((x . y))' used to avoid consing in
   Finternal_set_lisp_face_attribute.  */

static Lisp_Object Vparam_value_alist;

/* The total number of colors currently allocated.  */

#if GLYPH_DEBUG
static int ncolors_allocated;
static int npixmaps_allocated;
static int ngcs;
#endif

/* Non-zero means the definition of the `menu' face for new frames has
   been changed.  */

static int menu_face_changed_default;


/* Function prototypes.  */

struct table_entry;
struct named_merge_point;

static void map_tty_color (struct frame *, struct face *,
			   enum lface_attribute_index, int *);
static Lisp_Object resolve_face_name (Lisp_Object, int);
static void set_font_frame_param (Lisp_Object, Lisp_Object);
static int get_lface_attributes (struct frame *, Lisp_Object, Lisp_Object *,
				 int, struct named_merge_point *);
static ptrdiff_t load_pixmap (struct frame *, Lisp_Object,
			      unsigned *, unsigned *);
static struct frame *frame_or_selected_frame (Lisp_Object, int);
static void load_face_colors (struct frame *, struct face *, Lisp_Object *);
static void free_face_colors (struct frame *, struct face *);
static int face_color_gray_p (struct frame *, const char *);
static struct face *realize_face (struct face_cache *, Lisp_Object *,
				  int);
static struct face *realize_non_ascii_face (struct frame *, Lisp_Object,
					    struct face *);
static struct face *realize_x_face (struct face_cache *, Lisp_Object *);
static struct face *realize_tty_face (struct face_cache *, Lisp_Object *);
static int realize_basic_faces (struct frame *);
static int realize_default_face (struct frame *);
static void realize_named_face (struct frame *, Lisp_Object, int);
static int lface_fully_specified_p (Lisp_Object *);
static int lface_equal_p (Lisp_Object *, Lisp_Object *);
static unsigned hash_string_case_insensitive (Lisp_Object);
static unsigned lface_hash (Lisp_Object *);
static int lface_same_font_attributes_p (Lisp_Object *, Lisp_Object *);
static struct face_cache *make_face_cache (struct frame *);
static void clear_face_gcs (struct face_cache *);
static void free_face_cache (struct face_cache *);
static int face_fontset (Lisp_Object *);
static void merge_face_vectors (struct frame *, Lisp_Object *, Lisp_Object*,
				struct named_merge_point *);
static int merge_face_ref (struct frame *, Lisp_Object, Lisp_Object *,
			   int, struct named_merge_point *);
static int set_lface_from_font (struct frame *, Lisp_Object, Lisp_Object,
				int);
static Lisp_Object lface_from_face_name (struct frame *, Lisp_Object, int);
static struct face *make_realized_face (Lisp_Object *);
static void cache_face (struct face_cache *, struct face *, unsigned);
static void uncache_face (struct face_cache *, struct face *);

#ifdef HAVE_WINDOW_SYSTEM

static GC x_create_gc (struct frame *, unsigned long, XGCValues *);
static void x_free_gc (struct frame *, GC);

#ifdef USE_X_TOOLKIT
static void x_update_menu_appearance (struct frame *);

extern void free_frame_menubar (struct frame *);
#endif /* USE_X_TOOLKIT */

#endif /* HAVE_WINDOW_SYSTEM */


/***********************************************************************
			      Utilities
 ***********************************************************************/

#ifdef HAVE_X_WINDOWS

#ifdef DEBUG_X_COLORS

/* The following is a poor mans infrastructure for debugging X color
   allocation problems on displays with PseudoColor-8.  Some X servers
   like 3.3.5 XF86_SVGA with Matrox cards apparently don't implement
   color reference counts completely so that they don't signal an
   error when a color is freed whose reference count is already 0.
   Other X servers do.  To help me debug this, the following code
   implements a simple reference counting schema of its own, for a
   single display/screen.  --gerd.  */

/* Reference counts for pixel colors.  */

int color_count[256];

/* Register color PIXEL as allocated.  */

void
register_color (unsigned long pixel)
{
  xassert (pixel < 256);
  ++color_count[pixel];
}


/* Register color PIXEL as deallocated.  */

void
unregister_color (unsigned long pixel)
{
  xassert (pixel < 256);
  if (color_count[pixel] > 0)
    --color_count[pixel];
  else
    abort ();
}


/* Register N colors from PIXELS as deallocated.  */

void
unregister_colors (unsigned long *pixels, int n)
{
  int i;
  for (i = 0; i < n; ++i)
    unregister_color (pixels[i]);
}


DEFUN ("dump-colors", Fdump_colors, Sdump_colors, 0, 0, 0,
       doc: /* Dump currently allocated colors to stderr.  */)
  (void)
{
  int i, n;

  fputc ('\n', stderr);

  for (i = n = 0; i < sizeof color_count / sizeof color_count[0]; ++i)
    if (color_count[i])
      {
	fprintf (stderr, "%3d: %5d", i, color_count[i]);
	++n;
	if (n % 5 == 0)
	  fputc ('\n', stderr);
	else
	  fputc ('\t', stderr);
      }

  if (n % 5 != 0)
    fputc ('\n', stderr);
  return Qnil;
}

#endif /* DEBUG_X_COLORS */


/* Free colors used on frame F.  PIXELS is an array of NPIXELS pixel
   color values.  Interrupt input must be blocked when this function
   is called.  */

void
x_free_colors (struct frame *f, long unsigned int *pixels, int npixels)
{
  int class = FRAME_X_DISPLAY_INFO (f)->visual->class;

  /* If display has an immutable color map, freeing colors is not
     necessary and some servers don't allow it.  So don't do it.  */
  if (class != StaticColor && class != StaticGray && class != TrueColor)
    {
#ifdef DEBUG_X_COLORS
      unregister_colors (pixels, npixels);
#endif
      XFreeColors (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f),
		   pixels, npixels, 0);
    }
}


#ifdef USE_X_TOOLKIT

/* Free colors used on frame F.  PIXELS is an array of NPIXELS pixel
   color values.  Interrupt input must be blocked when this function
   is called.  */

void
x_free_dpy_colors (Display *dpy, Screen *screen, Colormap cmap,
		   long unsigned int *pixels, int npixels)
{
  struct x_display_info *dpyinfo = x_display_info_for_display (dpy);
  int class = dpyinfo->visual->class;

  /* If display has an immutable color map, freeing colors is not
     necessary and some servers don't allow it.  So don't do it.  */
  if (class != StaticColor && class != StaticGray && class != TrueColor)
    {
#ifdef DEBUG_X_COLORS
      unregister_colors (pixels, npixels);
#endif
      XFreeColors (dpy, cmap, pixels, npixels, 0);
    }
}
#endif /* USE_X_TOOLKIT */

/* Create and return a GC for use on frame F.  GC values and mask
   are given by XGCV and MASK.  */

static inline GC
x_create_gc (struct frame *f, long unsigned int mask, XGCValues *xgcv)
{
  GC gc;
  BLOCK_INPUT;
  gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), mask, xgcv);
  UNBLOCK_INPUT;
  IF_DEBUG (++ngcs);
  return gc;
}


/* Free GC which was used on frame F.  */

static inline void
x_free_gc (struct frame *f, GC gc)
{
  eassert (interrupt_input_blocked);
  IF_DEBUG (xassert (--ngcs >= 0));
  XFreeGC (FRAME_X_DISPLAY (f), gc);
}

#endif /* HAVE_X_WINDOWS */

#ifdef WINDOWSNT
/* W32 emulation of GCs */

static inline GC
x_create_gc (struct frame *f, unsigned long mask, XGCValues *xgcv)
{
  GC gc;
  BLOCK_INPUT;
  gc = XCreateGC (NULL, FRAME_W32_WINDOW (f), mask, xgcv);
  UNBLOCK_INPUT;
  IF_DEBUG (++ngcs);
  return gc;
}


/* Free GC which was used on frame F.  */

static inline void
x_free_gc (struct frame *f, GC gc)
{
  IF_DEBUG (xassert (--ngcs >= 0));
  xfree (gc);
}

#endif  /* WINDOWSNT */

#ifdef HAVE_NS
/* NS emulation of GCs */

static inline GC
x_create_gc (struct frame *f,
	     unsigned long mask,
	     XGCValues *xgcv)
{
  GC gc = xmalloc (sizeof (*gc));
  if (gc)
    memcpy (gc, xgcv, sizeof (XGCValues));
  return gc;
}

static inline void
x_free_gc (struct frame *f, GC gc)
{
  xfree (gc);
}
#endif  /* HAVE_NS */

/* Like strcasecmp/stricmp.  Used to compare parts of font names which
   are in ISO8859-1.  */

int
xstrcasecmp (const char *s1, const char *s2)
{
  while (*s1 && *s2)
    {
      unsigned char b1 = *s1;
      unsigned char b2 = *s2;
      unsigned char c1 = tolower (b1);
      unsigned char c2 = tolower (b2);
      if (c1 != c2)
	return c1 < c2 ? -1 : 1;
      ++s1, ++s2;
    }

  if (*s1 == 0)
    return *s2 == 0 ? 0 : -1;
  return 1;
}


/* If FRAME is nil, return a pointer to the selected frame.
   Otherwise, check that FRAME is a live frame, and return a pointer
   to it.  NPARAM is the parameter number of FRAME, for
   CHECK_LIVE_FRAME.  This is here because it's a frequent pattern in
   Lisp function definitions.  */

static inline struct frame *
frame_or_selected_frame (Lisp_Object frame, int nparam)
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);
  return XFRAME (frame);
}


/***********************************************************************
			   Frames and faces
 ***********************************************************************/

/* Initialize face cache and basic faces for frame F.  */

void
init_frame_faces (struct frame *f)
{
  /* Make a face cache, if F doesn't have one.  */
  if (FRAME_FACE_CACHE (f) == NULL)
    FRAME_FACE_CACHE (f) = make_face_cache (f);

#ifdef HAVE_WINDOW_SYSTEM
  /* Make the image cache.  */
  if (FRAME_WINDOW_P (f))
    {
      /* We initialize the image cache when creating the first frame
	 on a terminal, and not during terminal creation.  This way,
	 `x-open-connection' on a tty won't create an image cache.  */
      if (FRAME_IMAGE_CACHE (f) == NULL)
	FRAME_IMAGE_CACHE (f) = make_image_cache ();
      ++FRAME_IMAGE_CACHE (f)->refcount;
    }
#endif /* HAVE_WINDOW_SYSTEM */

  /* Realize basic faces.  Must have enough information in frame
     parameters to realize basic faces at this point.  */
#ifdef HAVE_X_WINDOWS
  if (!FRAME_X_P (f) || FRAME_X_WINDOW (f))
#endif
#ifdef WINDOWSNT
  if (!FRAME_WINDOW_P (f) || FRAME_W32_WINDOW (f))
#endif
#ifdef HAVE_NS
  if (!FRAME_NS_P (f) || FRAME_NS_WINDOW (f))
#endif
    if (!realize_basic_faces (f))
	abort ();
}


/* Free face cache of frame F.  Called from delete_frame.  */

void
free_frame_faces (struct frame *f)
{
  struct face_cache *face_cache = FRAME_FACE_CACHE (f);

  if (face_cache)
    {
      free_face_cache (face_cache);
      FRAME_FACE_CACHE (f) = NULL;
    }

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      struct image_cache *image_cache = FRAME_IMAGE_CACHE (f);
      if (image_cache)
	{
	  --image_cache->refcount;
	  if (image_cache->refcount == 0)
	    free_image_cache (f);
	}
    }
#endif /* HAVE_WINDOW_SYSTEM */
}


/* Clear face caches, and recompute basic faces for frame F.  Call
   this after changing frame parameters on which those faces depend,
   or when realized faces have been freed due to changing attributes
   of named faces. */

void
recompute_basic_faces (struct frame *f)
{
  if (FRAME_FACE_CACHE (f))
    {
      clear_face_cache (0);
      if (!realize_basic_faces (f))
	abort ();
    }
}


/* Clear the face caches of all frames.  CLEAR_FONTS_P non-zero means
   try to free unused fonts, too.  */

void
clear_face_cache (int clear_fonts_p)
{
#ifdef HAVE_WINDOW_SYSTEM
  Lisp_Object tail, frame;

  if (clear_fonts_p
      || ++clear_font_table_count == CLEAR_FONT_TABLE_COUNT)
    {
#if 0
      /* Not yet implemented.  */
      clear_font_cache (frame);
#endif

      /* From time to time see if we can unload some fonts.  This also
	 frees all realized faces on all frames.  Fonts needed by
	 faces will be loaded again when faces are realized again.  */
      clear_font_table_count = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);
	  if (FRAME_WINDOW_P (f)
	      && FRAME_X_DISPLAY_INFO (f)->n_fonts > CLEAR_FONT_TABLE_NFONTS)
	    free_all_realized_faces (frame);
	}
    }
  else
    {
      /* Clear GCs of realized faces.  */
      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);
	  if (FRAME_WINDOW_P (f))
	      clear_face_gcs (FRAME_FACE_CACHE (f));
	}
      clear_image_caches (Qnil);
    }
#endif /* HAVE_WINDOW_SYSTEM */
}


DEFUN ("clear-face-cache", Fclear_face_cache, Sclear_face_cache, 0, 1, 0,
       doc: /* Clear face caches on all frames.
Optional THOROUGHLY non-nil means try to free unused fonts, too.  */)
  (Lisp_Object thoroughly)
{
  clear_face_cache (!NILP (thoroughly));
  ++face_change_count;
  ++windows_or_buffers_changed;
  return Qnil;
}


/***********************************************************************
			      X Pixmaps
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

DEFUN ("bitmap-spec-p", Fbitmap_spec_p, Sbitmap_spec_p, 1, 1, 0,
       doc: /* Value is non-nil if OBJECT is a valid bitmap specification.
A bitmap specification is either a string, a file name, or a list
\(WIDTH HEIGHT DATA) where WIDTH is the pixel width of the bitmap,
HEIGHT is its height, and DATA is a string containing the bits of
the pixmap.  Bits are stored row by row, each row occupies
\(WIDTH + 7)/8 bytes.  */)
  (Lisp_Object object)
{
  int pixmap_p = 0;

  if (STRINGP (object))
    /* If OBJECT is a string, it's a file name.  */
    pixmap_p = 1;
  else if (CONSP (object))
    {
      /* Otherwise OBJECT must be (WIDTH HEIGHT DATA), WIDTH and
	 HEIGHT must be integers > 0, and DATA must be string large
	 enough to hold a bitmap of the specified size.  */
      Lisp_Object width, height, data;

      height = width = data = Qnil;

      if (CONSP (object))
	{
	  width = XCAR (object);
	  object = XCDR (object);
	  if (CONSP (object))
	    {
	      height = XCAR (object);
	      object = XCDR (object);
	      if (CONSP (object))
		data = XCAR (object);
	    }
	}

      if (STRINGP (data)
	  && INTEGERP (width) && 0 < XINT (width)
	  && INTEGERP (height) && 0 < XINT (height))
	{
	  EMACS_INT bytes_per_row = ((XINT (width) + BITS_PER_CHAR - 1)
				     / BITS_PER_CHAR);
	  if (XINT (height) <= SBYTES (data) / bytes_per_row)
	    pixmap_p = 1;
	}
    }

  return pixmap_p ? Qt : Qnil;
}


/* Load a bitmap according to NAME (which is either a file name or a
   pixmap spec) for use on frame F.  Value is the bitmap_id (see
   xfns.c).  If NAME is nil, return with a bitmap id of zero.  If
   bitmap cannot be loaded, display a message saying so, and return
   zero.  Store the bitmap width in *W_PTR and its height in *H_PTR,
   if these pointers are not null.  */

static ptrdiff_t
load_pixmap (FRAME_PTR f, Lisp_Object name, unsigned int *w_ptr,
	     unsigned int *h_ptr)
{
  ptrdiff_t bitmap_id;

  if (NILP (name))
    return 0;

  CHECK_TYPE (!NILP (Fbitmap_spec_p (name)), Qbitmap_spec_p, name);

  BLOCK_INPUT;
  if (CONSP (name))
    {
      /* Decode a bitmap spec into a bitmap.  */

      int h, w;
      Lisp_Object bits;

      w = XINT (Fcar (name));
      h = XINT (Fcar (Fcdr (name)));
      bits = Fcar (Fcdr (Fcdr (name)));

      bitmap_id = x_create_bitmap_from_data (f, SSDATA (bits),
					     w, h);
    }
  else
    {
      /* It must be a string -- a file name.  */
      bitmap_id = x_create_bitmap_from_file (f, name);
    }
  UNBLOCK_INPUT;

  if (bitmap_id < 0)
    {
      add_to_log ("Invalid or undefined bitmap `%s'", name, Qnil);
      bitmap_id = 0;

      if (w_ptr)
	*w_ptr = 0;
      if (h_ptr)
	*h_ptr = 0;
    }
  else
    {
#if GLYPH_DEBUG
      ++npixmaps_allocated;
#endif
      if (w_ptr)
	*w_ptr = x_bitmap_width (f, bitmap_id);

      if (h_ptr)
	*h_ptr = x_bitmap_height (f, bitmap_id);
    }

  return bitmap_id;
}

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
				X Colors
 ***********************************************************************/

/* Parse RGB_LIST, and fill in the RGB fields of COLOR.
   RGB_LIST should contain (at least) 3 lisp integers.
   Return 0 if there's a problem with RGB_LIST, otherwise return 1.  */

static int
parse_rgb_list (Lisp_Object rgb_list, XColor *color)
{
#define PARSE_RGB_LIST_FIELD(field)					\
  if (CONSP (rgb_list) && INTEGERP (XCAR (rgb_list)))			\
    {									\
      color->field = XINT (XCAR (rgb_list));				\
      rgb_list = XCDR (rgb_list);					\
    }									\
  else									\
    return 0;

  PARSE_RGB_LIST_FIELD (red);
  PARSE_RGB_LIST_FIELD (green);
  PARSE_RGB_LIST_FIELD (blue);

  return 1;
}


/* Lookup on frame F the color described by the lisp string COLOR.
   The resulting tty color is returned in TTY_COLOR; if STD_COLOR is
   non-zero, then the `standard' definition of the same color is
   returned in it.  */

static int
tty_lookup_color (struct frame *f, Lisp_Object color, XColor *tty_color,
		  XColor *std_color)
{
  Lisp_Object frame, color_desc;

  if (!STRINGP (color) || NILP (Ffboundp (Qtty_color_desc)))
    return 0;

  XSETFRAME (frame, f);

  color_desc = call2 (Qtty_color_desc, color, frame);
  if (CONSP (color_desc) && CONSP (XCDR (color_desc)))
    {
      Lisp_Object rgb;

      if (! INTEGERP (XCAR (XCDR (color_desc))))
	return 0;

      tty_color->pixel = XINT (XCAR (XCDR (color_desc)));

      rgb = XCDR (XCDR (color_desc));
      if (! parse_rgb_list (rgb, tty_color))
	return 0;

      /* Should we fill in STD_COLOR too?  */
      if (std_color)
	{
	  /* Default STD_COLOR to the same as TTY_COLOR.  */
	  *std_color = *tty_color;

	  /* Do a quick check to see if the returned descriptor is
	     actually _exactly_ equal to COLOR, otherwise we have to
	     lookup STD_COLOR separately.  If it's impossible to lookup
	     a standard color, we just give up and use TTY_COLOR.  */
	  if ((!STRINGP (XCAR (color_desc))
	       || NILP (Fstring_equal (color, XCAR (color_desc))))
	      && !NILP (Ffboundp (Qtty_color_standard_values)))
	    {
	      /* Look up STD_COLOR separately.  */
	      rgb = call1 (Qtty_color_standard_values, color);
	      if (! parse_rgb_list (rgb, std_color))
		return 0;
	    }
	}

      return 1;
    }
  else if (NILP (Fsymbol_value (intern ("tty-defined-color-alist"))))
    /* We were called early during startup, and the colors are not
       yet set up in tty-defined-color-alist.  Don't return a failure
       indication, since this produces the annoying "Unable to
       load color" messages in the *Messages* buffer.  */
    return 1;
  else
    /* tty-color-desc seems to have returned a bad value.  */
    return 0;
}

/* A version of defined_color for non-X frames.  */

static int
tty_defined_color (struct frame *f, const char *color_name,
		   XColor *color_def, int alloc)
{
  int status = 1;

  /* Defaults.  */
  color_def->pixel = FACE_TTY_DEFAULT_COLOR;
  color_def->red = 0;
  color_def->blue = 0;
  color_def->green = 0;

  if (*color_name)
    status = tty_lookup_color (f, build_string (color_name), color_def, NULL);

  if (color_def->pixel == FACE_TTY_DEFAULT_COLOR && *color_name)
    {
      if (strcmp (color_name, "unspecified-fg") == 0)
	color_def->pixel = FACE_TTY_DEFAULT_FG_COLOR;
      else if (strcmp (color_name, "unspecified-bg") == 0)
	color_def->pixel = FACE_TTY_DEFAULT_BG_COLOR;
    }

  if (color_def->pixel != FACE_TTY_DEFAULT_COLOR)
    status = 1;

  return status;
}


/* Decide if color named COLOR_NAME is valid for the display
   associated with the frame F; if so, return the rgb values in
   COLOR_DEF.  If ALLOC is nonzero, allocate a new colormap cell.

   This does the right thing for any type of frame.  */

static int
defined_color (struct frame *f, const char *color_name, XColor *color_def,
	       int alloc)
{
  if (!FRAME_WINDOW_P (f))
    return tty_defined_color (f, color_name, color_def, alloc);
#ifdef HAVE_X_WINDOWS
  else if (FRAME_X_P (f))
    return x_defined_color (f, color_name, color_def, alloc);
#endif
#ifdef WINDOWSNT
  else if (FRAME_W32_P (f))
    return w32_defined_color (f, color_name, color_def, alloc);
#endif
#ifdef HAVE_NS
  else if (FRAME_NS_P (f))
    return ns_defined_color (f, color_name, color_def, alloc, 1);
#endif
  else
    abort ();
}


/* Given the index IDX of a tty color on frame F, return its name, a
   Lisp string.  */

Lisp_Object
tty_color_name (struct frame *f, int idx)
{
  if (idx >= 0 && !NILP (Ffboundp (Qtty_color_by_index)))
    {
      Lisp_Object frame;
      Lisp_Object coldesc;

      XSETFRAME (frame, f);
      coldesc = call2 (Qtty_color_by_index, make_number (idx), frame);

      if (!NILP (coldesc))
	return XCAR (coldesc);
    }
#ifdef MSDOS
  /* We can have an MSDOG frame under -nw for a short window of
     opportunity before internal_terminal_init is called.  DTRT.  */
  if (FRAME_MSDOS_P (f) && !inhibit_window_system)
    return msdos_stdcolor_name (idx);
#endif

  if (idx == FACE_TTY_DEFAULT_FG_COLOR)
    return build_string (unspecified_fg);
  if (idx == FACE_TTY_DEFAULT_BG_COLOR)
    return build_string (unspecified_bg);

  return Qunspecified;
}


/* Return non-zero if COLOR_NAME is a shade of gray (or white or
   black) on frame F.

   The criterion implemented here is not a terribly sophisticated one.  */

static int
face_color_gray_p (struct frame *f, const char *color_name)
{
  XColor color;
  int gray_p;

  if (defined_color (f, color_name, &color, 0))
    gray_p = (/* Any color sufficiently close to black counts as gray.  */
	      (color.red < 5000 && color.green < 5000 && color.blue < 5000)
	      ||
	      ((eabs (color.red - color.green)
		< max (color.red, color.green) / 20)
	       && (eabs (color.green - color.blue)
		   < max (color.green, color.blue) / 20)
	       && (eabs (color.blue - color.red)
		   < max (color.blue, color.red) / 20)));
  else
    gray_p = 0;

  return gray_p;
}


/* Return non-zero if color COLOR_NAME can be displayed on frame F.
   BACKGROUND_P non-zero means the color will be used as background
   color.  */

static int
face_color_supported_p (struct frame *f, const char *color_name,
			int background_p)
{
  Lisp_Object frame;
  XColor not_used;

  XSETFRAME (frame, f);
  return
#ifdef HAVE_WINDOW_SYSTEM
    FRAME_WINDOW_P (f)
    ? (!NILP (Fxw_display_color_p (frame))
       || xstrcasecmp (color_name, "black") == 0
       || xstrcasecmp (color_name, "white") == 0
       || (background_p
	   && face_color_gray_p (f, color_name))
       || (!NILP (Fx_display_grayscale_p (frame))
	   && face_color_gray_p (f, color_name)))
    :
#endif
    tty_defined_color (f, color_name, &not_used, 0);
}


DEFUN ("color-gray-p", Fcolor_gray_p, Scolor_gray_p, 1, 2, 0,
       doc: /* Return non-nil if COLOR is a shade of gray (or white or black).
FRAME specifies the frame and thus the display for interpreting COLOR.
If FRAME is nil or omitted, use the selected frame.  */)
  (Lisp_Object color, Lisp_Object frame)
{
  struct frame *f;

  CHECK_STRING (color);
  if (NILP (frame))
    frame = selected_frame;
  else
    CHECK_FRAME (frame);
  f = XFRAME (frame);
  return face_color_gray_p (f, SSDATA (color)) ? Qt : Qnil;
}


DEFUN ("color-supported-p", Fcolor_supported_p,
       Scolor_supported_p, 1, 3, 0,
       doc: /* Return non-nil if COLOR can be displayed on FRAME.
BACKGROUND-P non-nil means COLOR is used as a background.
Otherwise, this function tells whether it can be used as a foreground.
If FRAME is nil or omitted, use the selected frame.
COLOR must be a valid color name.  */)
  (Lisp_Object color, Lisp_Object frame, Lisp_Object background_p)
{
  struct frame *f;

  CHECK_STRING (color);
  if (NILP (frame))
    frame = selected_frame;
  else
    CHECK_FRAME (frame);
  f = XFRAME (frame);
  if (face_color_supported_p (f, SSDATA (color), !NILP (background_p)))
    return Qt;
  return Qnil;
}


/* Load color with name NAME for use by face FACE on frame F.
   TARGET_INDEX must be one of LFACE_FOREGROUND_INDEX,
   LFACE_BACKGROUND_INDEX, LFACE_UNDERLINE_INDEX, LFACE_OVERLINE_INDEX,
   LFACE_STRIKE_THROUGH_INDEX, or LFACE_BOX_INDEX.  Value is the
   pixel color.  If color cannot be loaded, display a message, and
   return the foreground, background or underline color of F, but
   record that fact in flags of the face so that we don't try to free
   these colors.  */

unsigned long
load_color (struct frame *f, struct face *face, Lisp_Object name,
	    enum lface_attribute_index target_index)
{
  XColor color;

  xassert (STRINGP (name));
  xassert (target_index == LFACE_FOREGROUND_INDEX
	   || target_index == LFACE_BACKGROUND_INDEX
	   || target_index == LFACE_UNDERLINE_INDEX
	   || target_index == LFACE_OVERLINE_INDEX
	   || target_index == LFACE_STRIKE_THROUGH_INDEX
	   || target_index == LFACE_BOX_INDEX);

  /* if the color map is full, defined_color will return a best match
     to the values in an existing cell. */
  if (!defined_color (f, SSDATA (name), &color, 1))
    {
      add_to_log ("Unable to load color \"%s\"", name, Qnil);

      switch (target_index)
	{
	case LFACE_FOREGROUND_INDEX:
	  face->foreground_defaulted_p = 1;
	  color.pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_BACKGROUND_INDEX:
	  face->background_defaulted_p = 1;
	  color.pixel = FRAME_BACKGROUND_PIXEL (f);
	  break;

	case LFACE_UNDERLINE_INDEX:
	  face->underline_defaulted_p = 1;
	  color.pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_OVERLINE_INDEX:
	  face->overline_color_defaulted_p = 1;
	  color.pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_STRIKE_THROUGH_INDEX:
	  face->strike_through_color_defaulted_p = 1;
	  color.pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	case LFACE_BOX_INDEX:
	  face->box_color_defaulted_p = 1;
	  color.pixel = FRAME_FOREGROUND_PIXEL (f);
	  break;

	default:
	  abort ();
	}
    }
#if GLYPH_DEBUG
  else
    ++ncolors_allocated;
#endif

  return color.pixel;
}


#ifdef HAVE_WINDOW_SYSTEM

/* Load colors for face FACE which is used on frame F.  Colors are
   specified by slots LFACE_BACKGROUND_INDEX and LFACE_FOREGROUND_INDEX
   of ATTRS.  If the background color specified is not supported on F,
   try to emulate gray colors with a stipple from Vface_default_stipple.  */

static void
load_face_colors (struct frame *f, struct face *face, Lisp_Object *attrs)
{
  Lisp_Object fg, bg;

  bg = attrs[LFACE_BACKGROUND_INDEX];
  fg = attrs[LFACE_FOREGROUND_INDEX];

  /* Swap colors if face is inverse-video.  */
  if (EQ (attrs[LFACE_INVERSE_INDEX], Qt))
    {
      Lisp_Object tmp;
      tmp = fg;
      fg = bg;
      bg = tmp;
    }

  /* Check for support for foreground, not for background because
     face_color_supported_p is smart enough to know that grays are
     "supported" as background because we are supposed to use stipple
     for them.  */
  if (!face_color_supported_p (f, SSDATA (bg), 0)
      && !NILP (Fbitmap_spec_p (Vface_default_stipple)))
    {
      x_destroy_bitmap (f, face->stipple);
      face->stipple = load_pixmap (f, Vface_default_stipple,
				   &face->pixmap_w, &face->pixmap_h);
    }

  face->background = load_color (f, face, bg, LFACE_BACKGROUND_INDEX);
  face->foreground = load_color (f, face, fg, LFACE_FOREGROUND_INDEX);
}


/* Free color PIXEL on frame F.  */

void
unload_color (struct frame *f, long unsigned int pixel)
{
#ifdef HAVE_X_WINDOWS
  if (pixel != -1)
    {
      BLOCK_INPUT;
      x_free_colors (f, &pixel, 1);
      UNBLOCK_INPUT;
    }
#endif
}


/* Free colors allocated for FACE.  */

static void
free_face_colors (struct frame *f, struct face *face)
{
/* PENDING(NS): need to do something here? */
#ifdef HAVE_X_WINDOWS
  if (face->colors_copied_bitwise_p)
    return;

  BLOCK_INPUT;

  if (!face->foreground_defaulted_p)
    {
      x_free_colors (f, &face->foreground, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  if (!face->background_defaulted_p)
    {
      x_free_colors (f, &face->background, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  if (face->underline_p
      && !face->underline_defaulted_p)
    {
      x_free_colors (f, &face->underline_color, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  if (face->overline_p
      && !face->overline_color_defaulted_p)
    {
      x_free_colors (f, &face->overline_color, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  if (face->strike_through_p
      && !face->strike_through_color_defaulted_p)
    {
      x_free_colors (f, &face->strike_through_color, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  if (face->box != FACE_NO_BOX
      && !face->box_color_defaulted_p)
    {
      x_free_colors (f, &face->box_color, 1);
      IF_DEBUG (--ncolors_allocated);
    }

  UNBLOCK_INPUT;
#endif /* HAVE_X_WINDOWS */
}

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
			   XLFD Font Names
 ***********************************************************************/

/* An enumerator for each field of an XLFD font name.  */

enum xlfd_field
{
  XLFD_FOUNDRY,
  XLFD_FAMILY,
  XLFD_WEIGHT,
  XLFD_SLANT,
  XLFD_SWIDTH,
  XLFD_ADSTYLE,
  XLFD_PIXEL_SIZE,
  XLFD_POINT_SIZE,
  XLFD_RESX,
  XLFD_RESY,
  XLFD_SPACING,
  XLFD_AVGWIDTH,
  XLFD_REGISTRY,
  XLFD_ENCODING,
  XLFD_LAST
};

/* An enumerator for each possible slant value of a font.  Taken from
   the XLFD specification.  */

enum xlfd_slant
{
  XLFD_SLANT_UNKNOWN,
  XLFD_SLANT_ROMAN,
  XLFD_SLANT_ITALIC,
  XLFD_SLANT_OBLIQUE,
  XLFD_SLANT_REVERSE_ITALIC,
  XLFD_SLANT_REVERSE_OBLIQUE,
  XLFD_SLANT_OTHER
};

/* Relative font weight according to XLFD documentation.  */

enum xlfd_weight
{
  XLFD_WEIGHT_UNKNOWN,
  XLFD_WEIGHT_ULTRA_LIGHT,	/* 10 */
  XLFD_WEIGHT_EXTRA_LIGHT,	/* 20 */
  XLFD_WEIGHT_LIGHT,		/* 30 */
  XLFD_WEIGHT_SEMI_LIGHT,	/* 40: SemiLight, Book, ...  */
  XLFD_WEIGHT_MEDIUM,		/* 50: Medium, Normal, Regular, ...  */
  XLFD_WEIGHT_SEMI_BOLD,	/* 60: SemiBold, DemiBold, ...  */
  XLFD_WEIGHT_BOLD,		/* 70: Bold, ... */
  XLFD_WEIGHT_EXTRA_BOLD,	/* 80: ExtraBold, Heavy, ...  */
  XLFD_WEIGHT_ULTRA_BOLD	/* 90: UltraBold, Black, ...  */
};

/* Relative proportionate width.  */

enum xlfd_swidth
{
  XLFD_SWIDTH_UNKNOWN,
  XLFD_SWIDTH_ULTRA_CONDENSED,	/* 10 */
  XLFD_SWIDTH_EXTRA_CONDENSED,	/* 20 */
  XLFD_SWIDTH_CONDENSED,	/* 30: Condensed, Narrow, Compressed, ... */
  XLFD_SWIDTH_SEMI_CONDENSED,	/* 40: semicondensed */
  XLFD_SWIDTH_MEDIUM,		/* 50: Medium, Normal, Regular, ... */
  XLFD_SWIDTH_SEMI_EXPANDED,	/* 60: SemiExpanded, DemiExpanded, ... */
  XLFD_SWIDTH_EXPANDED,		/* 70: Expanded... */
  XLFD_SWIDTH_EXTRA_EXPANDED,	/* 80: ExtraExpanded, Wide...  */
  XLFD_SWIDTH_ULTRA_EXPANDED	/* 90: UltraExpanded... */
};

/* Order by which font selection chooses fonts.  The default values
   mean `first, find a best match for the font width, then for the
   font height, then for weight, then for slant.'  This variable can be
   set via set-face-font-sort-order.  */

static int font_sort_order[4];

#ifdef HAVE_WINDOW_SYSTEM

static enum font_property_index font_props_for_sorting[FONT_SIZE_INDEX];

static int
compare_fonts_by_sort_order (const void *v1, const void *v2)
{
  Lisp_Object font1 = *(Lisp_Object *) v1;
  Lisp_Object font2 = *(Lisp_Object *) v2;
  int i;

  for (i = 0; i < FONT_SIZE_INDEX; i++)
    {
      enum font_property_index idx = font_props_for_sorting[i];
      Lisp_Object val1 = AREF (font1, idx), val2 = AREF (font2, idx);
      int result;

      if (idx <= FONT_REGISTRY_INDEX)
	{
	  if (STRINGP (val1))
	    result = STRINGP (val2) ? strcmp (SSDATA (val1), SSDATA (val2)) : -1;
	  else
	    result = STRINGP (val2) ? 1 : 0;
	}
      else
	{
	  if (INTEGERP (val1))
	    result = INTEGERP (val2) ? XINT (val1) - XINT (val2) : -1;
	  else
	    result = INTEGERP (val2) ? 1 : 0;
	}
      if (result)
	return result;
    }
  return 0;
}

DEFUN ("x-family-fonts", Fx_family_fonts, Sx_family_fonts, 0, 2, 0,
       doc: /* Return a list of available fonts of family FAMILY on FRAME.
If FAMILY is omitted or nil, list all families.
Otherwise, FAMILY must be a string, possibly containing wildcards
`?' and `*'.
If FRAME is omitted or nil, use the selected frame.
Each element of the result is a vector [FAMILY WIDTH POINT-SIZE WEIGHT
SLANT FIXED-P FULL REGISTRY-AND-ENCODING].
FAMILY is the font family name.  POINT-SIZE is the size of the
font in 1/10 pt.  WIDTH, WEIGHT, and SLANT are symbols describing the
width, weight and slant of the font.  These symbols are the same as for
face attributes.  FIXED-P is non-nil if the font is fixed-pitch.
FULL is the full name of the font, and REGISTRY-AND-ENCODING is a string
giving the registry and encoding of the font.
The result list is sorted according to the current setting of
the face font sort order.  */)
  (Lisp_Object family, Lisp_Object frame)
{
  Lisp_Object font_spec, list, *drivers, vec;
  int i, nfonts, ndrivers;
  Lisp_Object result;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);

  font_spec = Ffont_spec (0, NULL);
  if (!NILP (family))
    {
      CHECK_STRING (family);
      font_parse_family_registry (family, Qnil, font_spec);
    }

  list = font_list_entities (frame, font_spec);
  if (NILP (list))
    return Qnil;

  /* Sort the font entities.  */
  for (i = 0; i < 4; i++)
    switch (font_sort_order[i])
      {
      case XLFD_SWIDTH:
	font_props_for_sorting[i] = FONT_WIDTH_INDEX; break;
      case XLFD_POINT_SIZE:
	font_props_for_sorting[i] = FONT_SIZE_INDEX; break;
      case XLFD_WEIGHT:
	font_props_for_sorting[i] = FONT_WEIGHT_INDEX; break;
      default:
	font_props_for_sorting[i] = FONT_SLANT_INDEX; break;
      }
  font_props_for_sorting[i++] = FONT_FAMILY_INDEX;
  font_props_for_sorting[i++] = FONT_FOUNDRY_INDEX;
  font_props_for_sorting[i++] = FONT_ADSTYLE_INDEX;
  font_props_for_sorting[i++] = FONT_REGISTRY_INDEX;

  ndrivers = XINT (Flength (list));
  drivers  = alloca (sizeof (Lisp_Object) * ndrivers);
  for (i = 0; i < ndrivers; i++, list = XCDR (list))
    drivers[i] = XCAR (list);
  vec = Fvconcat (ndrivers, drivers);
  nfonts = ASIZE (vec);

  qsort (XVECTOR (vec)->contents, nfonts, sizeof (Lisp_Object),
	 compare_fonts_by_sort_order);

  result = Qnil;
  for (i = nfonts - 1; i >= 0; --i)
    {
      Lisp_Object font = AREF (vec, i);
      Lisp_Object v = Fmake_vector (make_number (8), Qnil);
      int point;
      Lisp_Object spacing;

      ASET (v, 0, AREF (font, FONT_FAMILY_INDEX));
      ASET (v, 1, FONT_WIDTH_SYMBOLIC (font));
      point = PIXEL_TO_POINT (XINT (AREF (font, FONT_SIZE_INDEX)) * 10,
			      XFRAME (frame)->resy);
      ASET (v, 2, make_number (point));
      ASET (v, 3, FONT_WEIGHT_SYMBOLIC (font));
      ASET (v, 4, FONT_SLANT_SYMBOLIC (font));
      spacing = Ffont_get (font, QCspacing);
      ASET (v, 5, (NILP (spacing) || EQ (spacing, Qp)) ? Qnil : Qt);
      ASET (v, 6, Ffont_xlfd_name (font, Qnil));
      ASET (v, 7, AREF (font, FONT_REGISTRY_INDEX));

      result = Fcons (v, result);
    }

  return result;
}

DEFUN ("x-list-fonts", Fx_list_fonts, Sx_list_fonts, 1, 5, 0,
       doc: /* Return a list of the names of available fonts matching PATTERN.
If optional arguments FACE and FRAME are specified, return only fonts
the same size as FACE on FRAME.

PATTERN should be a string containing a font name in the XLFD,
Fontconfig, or GTK format.  A font name given in the XLFD format may
contain wildcard characters:
  the * character matches any substring, and
  the ? character matches any single character.
  PATTERN is case-insensitive.

The return value is a list of strings, suitable as arguments to
`set-face-font'.

Fonts Emacs can't use may or may not be excluded
even if they match PATTERN and FACE.
The optional fourth argument MAXIMUM sets a limit on how many
fonts to match.  The first MAXIMUM fonts are reported.
The optional fifth argument WIDTH, if specified, is a number of columns
occupied by a character of a font.  In that case, return only fonts
the WIDTH times as wide as FACE on FRAME.  */)
  (Lisp_Object pattern, Lisp_Object face, Lisp_Object frame,
   Lisp_Object maximum, Lisp_Object width)
{
  struct frame *f;
  int size, avgwidth IF_LINT (= 0);

  check_x ();
  CHECK_STRING (pattern);

  if (! NILP (maximum))
    CHECK_NATNUM (maximum);

  if (!NILP (width))
    CHECK_NUMBER (width);

  /* We can't simply call check_x_frame because this function may be
     called before any frame is created.  */
  if (NILP (frame))
    frame = selected_frame;
  f = frame_or_selected_frame (frame, 2);
  if (! FRAME_WINDOW_P (f))
    {
      /* Perhaps we have not yet created any frame.  */
      f = NULL;
      frame = Qnil;
      face = Qnil;
    }

  /* Determine the width standard for comparison with the fonts we find.  */

  if (NILP (face))
    size = 0;
  else
    {
      /* This is of limited utility since it works with character
	 widths.  Keep it for compatibility.  --gerd.  */
      int face_id = lookup_named_face (f, face, 0);
      struct face *width_face = (face_id < 0
				 ? NULL
				 : FACE_FROM_ID (f, face_id));

      if (width_face && width_face->font)
	{
	  size = width_face->font->pixel_size;
	  avgwidth = width_face->font->average_width;
	}
      else
	{
	  size = FRAME_FONT (f)->pixel_size;
	  avgwidth = FRAME_FONT (f)->average_width;
	}
      if (!NILP (width))
	avgwidth *= XINT (width);
    }

  {
    Lisp_Object font_spec;
    Lisp_Object args[2], tail;

    font_spec = font_spec_from_name (pattern);
    if (!FONTP (font_spec))
      signal_error ("Invalid font name", pattern);

    if (size)
      {
	Ffont_put (font_spec, QCsize, make_number (size));
	Ffont_put (font_spec, QCavgwidth, make_number (avgwidth));
      }
    args[0] = Flist_fonts (font_spec, frame, maximum, font_spec);
    for (tail = args[0]; CONSP (tail); tail = XCDR (tail))
      {
	Lisp_Object font_entity;

	font_entity = XCAR (tail);
	if ((NILP (AREF (font_entity, FONT_SIZE_INDEX))
	     || XINT (AREF (font_entity, FONT_SIZE_INDEX)) == 0)
	    && ! NILP (AREF (font_spec, FONT_SIZE_INDEX)))
	  {
	    /* This is a scalable font.  For backward compatibility,
	       we set the specified size. */
	    font_entity = copy_font_spec (font_entity);
	    ASET (font_entity, FONT_SIZE_INDEX,
		  AREF (font_spec, FONT_SIZE_INDEX));
	  }
	XSETCAR (tail, Ffont_xlfd_name (font_entity, Qnil));
      }
    if (NILP (frame))
      /* We don't have to check fontsets.  */
      return args[0];
    args[1] = list_fontsets (f, pattern, size);
    return Fnconc (2, args);
  }
}

#endif /* HAVE_WINDOW_SYSTEM */


/***********************************************************************
			      Lisp Faces
 ***********************************************************************/

/* Access face attributes of face LFACE, a Lisp vector.  */

#define LFACE_FAMILY(LFACE)	    AREF ((LFACE), LFACE_FAMILY_INDEX)
#define LFACE_FOUNDRY(LFACE)	    AREF ((LFACE), LFACE_FOUNDRY_INDEX)
#define LFACE_HEIGHT(LFACE)	    AREF ((LFACE), LFACE_HEIGHT_INDEX)
#define LFACE_WEIGHT(LFACE)	    AREF ((LFACE), LFACE_WEIGHT_INDEX)
#define LFACE_SLANT(LFACE)	    AREF ((LFACE), LFACE_SLANT_INDEX)
#define LFACE_UNDERLINE(LFACE)      AREF ((LFACE), LFACE_UNDERLINE_INDEX)
#define LFACE_INVERSE(LFACE)	    AREF ((LFACE), LFACE_INVERSE_INDEX)
#define LFACE_FOREGROUND(LFACE)     AREF ((LFACE), LFACE_FOREGROUND_INDEX)
#define LFACE_BACKGROUND(LFACE)     AREF ((LFACE), LFACE_BACKGROUND_INDEX)
#define LFACE_STIPPLE(LFACE)	    AREF ((LFACE), LFACE_STIPPLE_INDEX)
#define LFACE_SWIDTH(LFACE)	    AREF ((LFACE), LFACE_SWIDTH_INDEX)
#define LFACE_OVERLINE(LFACE)	    AREF ((LFACE), LFACE_OVERLINE_INDEX)
#define LFACE_STRIKE_THROUGH(LFACE) AREF ((LFACE), LFACE_STRIKE_THROUGH_INDEX)
#define LFACE_BOX(LFACE)	    AREF ((LFACE), LFACE_BOX_INDEX)
#define LFACE_FONT(LFACE)	    AREF ((LFACE), LFACE_FONT_INDEX)
#define LFACE_INHERIT(LFACE)	    AREF ((LFACE), LFACE_INHERIT_INDEX)
#define LFACE_FONTSET(LFACE)	    AREF ((LFACE), LFACE_FONTSET_INDEX)

#if XASSERTS
/* Non-zero if LFACE is a Lisp face.  A Lisp face is a vector of size
   LFACE_VECTOR_SIZE which has the symbol `face' in slot 0.  */

#define LFACEP(LFACE)					\
     (VECTORP (LFACE)					\
      && ASIZE (LFACE) == LFACE_VECTOR_SIZE		\
      && EQ (AREF (LFACE, 0), Qface))
#endif


#if GLYPH_DEBUG

/* Check consistency of Lisp face attribute vector ATTRS.  */

static void
check_lface_attrs (Lisp_Object *attrs)
{
  xassert (UNSPECIFIEDP (attrs[LFACE_FAMILY_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_FAMILY_INDEX])
	   || STRINGP (attrs[LFACE_FAMILY_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_FOUNDRY_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_FOUNDRY_INDEX])
	   || STRINGP (attrs[LFACE_FOUNDRY_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_SWIDTH_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_SWIDTH_INDEX])
	   || SYMBOLP (attrs[LFACE_SWIDTH_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_HEIGHT_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_HEIGHT_INDEX])
	   || INTEGERP (attrs[LFACE_HEIGHT_INDEX])
	   || FLOATP (attrs[LFACE_HEIGHT_INDEX])
	   || FUNCTIONP (attrs[LFACE_HEIGHT_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_WEIGHT_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_WEIGHT_INDEX])
	   || SYMBOLP (attrs[LFACE_WEIGHT_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_SLANT_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_SLANT_INDEX])
	   || SYMBOLP (attrs[LFACE_SLANT_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_UNDERLINE_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_UNDERLINE_INDEX])
	   || SYMBOLP (attrs[LFACE_UNDERLINE_INDEX])
	   || STRINGP (attrs[LFACE_UNDERLINE_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_OVERLINE_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_OVERLINE_INDEX])
	   || SYMBOLP (attrs[LFACE_OVERLINE_INDEX])
	   || STRINGP (attrs[LFACE_OVERLINE_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_STRIKE_THROUGH_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_STRIKE_THROUGH_INDEX])
	   || SYMBOLP (attrs[LFACE_STRIKE_THROUGH_INDEX])
	   || STRINGP (attrs[LFACE_STRIKE_THROUGH_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_BOX_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_BOX_INDEX])
	   || SYMBOLP (attrs[LFACE_BOX_INDEX])
	   || STRINGP (attrs[LFACE_BOX_INDEX])
	   || INTEGERP (attrs[LFACE_BOX_INDEX])
	   || CONSP (attrs[LFACE_BOX_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_INVERSE_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_INVERSE_INDEX])
	   || SYMBOLP (attrs[LFACE_INVERSE_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_FOREGROUND_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_FOREGROUND_INDEX])
	   || STRINGP (attrs[LFACE_FOREGROUND_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_BACKGROUND_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_BACKGROUND_INDEX])
	   || STRINGP (attrs[LFACE_BACKGROUND_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_INHERIT_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_INHERIT_INDEX])
	   || NILP (attrs[LFACE_INHERIT_INDEX])
	   || SYMBOLP (attrs[LFACE_INHERIT_INDEX])
	   || CONSP (attrs[LFACE_INHERIT_INDEX]));
#ifdef HAVE_WINDOW_SYSTEM
  xassert (UNSPECIFIEDP (attrs[LFACE_STIPPLE_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_STIPPLE_INDEX])
	   || SYMBOLP (attrs[LFACE_STIPPLE_INDEX])
	   || !NILP (Fbitmap_spec_p (attrs[LFACE_STIPPLE_INDEX])));
  xassert (UNSPECIFIEDP (attrs[LFACE_FONT_INDEX])
	   || IGNORE_DEFFACE_P (attrs[LFACE_FONT_INDEX])
	   || FONTP (attrs[LFACE_FONT_INDEX]));
  xassert (UNSPECIFIEDP (attrs[LFACE_FONTSET_INDEX])
	   || STRINGP (attrs[LFACE_FONTSET_INDEX])
	   || NILP (attrs[LFACE_FONTSET_INDEX]));
#endif
}


/* Check consistency of attributes of Lisp face LFACE (a Lisp vector).  */

static void
check_lface (Lisp_Object lface)
{
  if (!NILP (lface))
    {
      xassert (LFACEP (lface));
      check_lface_attrs (XVECTOR (lface)->contents);
    }
}

#else /* GLYPH_DEBUG == 0 */

#define check_lface_attrs(attrs)	(void) 0
#define check_lface(lface)		(void) 0

#endif /* GLYPH_DEBUG == 0 */



/* Face-merge cycle checking.  */

enum named_merge_point_kind
{
  NAMED_MERGE_POINT_NORMAL,
  NAMED_MERGE_POINT_REMAP
};

/* A `named merge point' is simply a point during face-merging where we
   look up a face by name.  We keep a stack of which named lookups we're
   currently processing so that we can easily detect cycles, using a
   linked- list of struct named_merge_point structures, typically
   allocated on the stack frame of the named lookup functions which are
   active (so no consing is required).  */
struct named_merge_point
{
  Lisp_Object face_name;
  enum named_merge_point_kind named_merge_point_kind;
  struct named_merge_point *prev;
};


/* If a face merging cycle is detected for FACE_NAME, return 0,
   otherwise add NEW_NAMED_MERGE_POINT, which is initialized using
   FACE_NAME and NAMED_MERGE_POINT_KIND, as the head of the linked list
   pointed to by NAMED_MERGE_POINTS, and return 1.  */

static inline int
push_named_merge_point (struct named_merge_point *new_named_merge_point,
			Lisp_Object face_name,
			enum named_merge_point_kind named_merge_point_kind,
			struct named_merge_point **named_merge_points)
{
  struct named_merge_point *prev;

  for (prev = *named_merge_points; prev; prev = prev->prev)
    if (EQ (face_name, prev->face_name))
      {
	if (prev->named_merge_point_kind == named_merge_point_kind)
	  /* A cycle, so fail.  */
	  return 0;
	else if (prev->named_merge_point_kind == NAMED_MERGE_POINT_REMAP)
	  /* A remap `hides ' any previous normal merge points
	     (because the remap means that it's actually different face),
	     so as we know the current merge point must be normal, we
	     can just assume it's OK.  */
	  break;
      }

  new_named_merge_point->face_name = face_name;
  new_named_merge_point->named_merge_point_kind = named_merge_point_kind;
  new_named_merge_point->prev = *named_merge_points;

  *named_merge_points = new_named_merge_point;

  return 1;
}


/* Resolve face name FACE_NAME.  If FACE_NAME is a string, intern it
   to make it a symbol.  If FACE_NAME is an alias for another face,
   return that face's name.

   Return default face in case of errors.  */

static Lisp_Object
resolve_face_name (Lisp_Object face_name, int signal_p)
{
  Lisp_Object orig_face;
  Lisp_Object tortoise, hare;

  if (STRINGP (face_name))
    face_name = intern (SSDATA (face_name));

  if (NILP (face_name) || !SYMBOLP (face_name))
    return face_name;

  orig_face = face_name;
  tortoise = hare = face_name;

  while (1)
    {
      face_name = hare;
      hare = Fget (hare, Qface_alias);
      if (NILP (hare) || !SYMBOLP (hare))
	break;

      face_name = hare;
      hare = Fget (hare, Qface_alias);
      if (NILP (hare) || !SYMBOLP (hare))
	break;

      tortoise = Fget (tortoise, Qface_alias);
      if (EQ (hare, tortoise))
	{
	  if (signal_p)
	    xsignal1 (Qcircular_list, orig_face);
	  return Qdefault;
	}
    }

  return face_name;
}


/* Return the face definition of FACE_NAME on frame F.  F null means
   return the definition for new frames.  FACE_NAME may be a string or
   a symbol (apparently Emacs 20.2 allowed strings as face names in
   face text properties; Ediff uses that).  If SIGNAL_P is non-zero,
   signal an error if FACE_NAME is not a valid face name.  If SIGNAL_P
   is zero, value is nil if FACE_NAME is not a valid face name.  */
static inline Lisp_Object
lface_from_face_name_no_resolve (struct frame *f, Lisp_Object face_name,
				 int signal_p)
{
  Lisp_Object lface;

  if (f)
    lface = assq_no_quit (face_name, f->face_alist);
  else
    lface = assq_no_quit (face_name, Vface_new_frame_defaults);

  if (CONSP (lface))
    lface = XCDR (lface);
  else if (signal_p)
    signal_error ("Invalid face", face_name);

  check_lface (lface);

  return lface;
}

/* Return the face definition of FACE_NAME on frame F.  F null means
   return the definition for new frames.  FACE_NAME may be a string or
   a symbol (apparently Emacs 20.2 allowed strings as face names in
   face text properties; Ediff uses that).  If FACE_NAME is an alias
   for another face, return that face's definition.  If SIGNAL_P is
   non-zero, signal an error if FACE_NAME is not a valid face name.
   If SIGNAL_P is zero, value is nil if FACE_NAME is not a valid face
   name.  */
static inline Lisp_Object
lface_from_face_name (struct frame *f, Lisp_Object face_name, int signal_p)
{
  face_name = resolve_face_name (face_name, signal_p);
  return lface_from_face_name_no_resolve (f, face_name, signal_p);
}


/* Get face attributes of face FACE_NAME from frame-local faces on
   frame F.  Store the resulting attributes in ATTRS which must point
   to a vector of Lisp_Objects of size LFACE_VECTOR_SIZE.  If SIGNAL_P
   is non-zero, signal an error if FACE_NAME does not name a face.
   Otherwise, value is zero if FACE_NAME is not a face.  */

static inline int
get_lface_attributes_no_remap (struct frame *f, Lisp_Object face_name,
			       Lisp_Object *attrs, int signal_p)
{
  Lisp_Object lface;

  lface = lface_from_face_name_no_resolve (f, face_name, signal_p);

  if (! NILP (lface))
    memcpy (attrs, XVECTOR (lface)->contents,
	    LFACE_VECTOR_SIZE * sizeof *attrs);

  return !NILP (lface);
}

/* Get face attributes of face FACE_NAME from frame-local faces on frame
   F.  Store the resulting attributes in ATTRS which must point to a
   vector of Lisp_Objects of size LFACE_VECTOR_SIZE.  If FACE_NAME is an
   alias for another face, use that face's definition.  If SIGNAL_P is
   non-zero, signal an error if FACE_NAME does not name a face.
   Otherwise, value is zero if FACE_NAME is not a face.  */

static inline int
get_lface_attributes (struct frame *f, Lisp_Object face_name,
		      Lisp_Object *attrs, int signal_p,
		      struct named_merge_point *named_merge_points)
{
  Lisp_Object face_remapping;

  face_name = resolve_face_name (face_name, signal_p);

  /* See if SYMBOL has been remapped to some other face (usually this
     is done buffer-locally).  */
  face_remapping = assq_no_quit (face_name, Vface_remapping_alist);
  if (CONSP (face_remapping))
    {
      struct named_merge_point named_merge_point;

      if (push_named_merge_point (&named_merge_point,
				  face_name, NAMED_MERGE_POINT_REMAP,
				  &named_merge_points))
	{
	  int i;

	  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
	    attrs[i] = Qunspecified;

	  return merge_face_ref (f, XCDR (face_remapping), attrs,
				 signal_p, named_merge_points);
	}
    }

  /* Default case, no remapping.  */
  return get_lface_attributes_no_remap (f, face_name, attrs, signal_p);
}


/* Non-zero if all attributes in face attribute vector ATTRS are
   specified, i.e. are non-nil.  */

static int
lface_fully_specified_p (Lisp_Object *attrs)
{
  int i;

  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (i != LFACE_FONT_INDEX && i != LFACE_INHERIT_INDEX)
      if ((UNSPECIFIEDP (attrs[i]) || IGNORE_DEFFACE_P (attrs[i])))
	break;

  return i == LFACE_VECTOR_SIZE;
}

#ifdef HAVE_WINDOW_SYSTEM

/* Set font-related attributes of Lisp face LFACE from FONT-OBJECT.
   If FORCE_P is zero, set only unspecified attributes of LFACE.  The
   exception is `font' attribute.  It is set to FONT_OBJECT regardless
   of FORCE_P.  */

static int
set_lface_from_font (struct frame *f, Lisp_Object lface,
		     Lisp_Object font_object, int force_p)
{
  Lisp_Object val;
  struct font *font = XFONT_OBJECT (font_object);

  /* Set attributes only if unspecified, otherwise face defaults for
     new frames would never take effect.  If the font doesn't have a
     specific property, set a normal value for that.  */

  if (force_p || UNSPECIFIEDP (LFACE_FAMILY (lface)))
    {
      Lisp_Object family = AREF (font_object, FONT_FAMILY_INDEX);

      LFACE_FAMILY (lface) = SYMBOL_NAME (family);
    }

  if (force_p || UNSPECIFIEDP (LFACE_FOUNDRY (lface)))
    {
      Lisp_Object foundry = AREF (font_object, FONT_FOUNDRY_INDEX);

      LFACE_FOUNDRY (lface) = SYMBOL_NAME (foundry);
    }

  if (force_p || UNSPECIFIEDP (LFACE_HEIGHT (lface)))
    {
      int pt = PIXEL_TO_POINT (font->pixel_size * 10, f->resy);

      xassert (pt > 0);
      LFACE_HEIGHT (lface) = make_number (pt);
    }

  if (force_p || UNSPECIFIEDP (LFACE_WEIGHT (lface)))
    {
      val = FONT_WEIGHT_FOR_FACE (font_object);
      LFACE_WEIGHT (lface) = ! NILP (val) ? val :Qnormal;
    }
  if (force_p || UNSPECIFIEDP (LFACE_SLANT (lface)))
    {
      val = FONT_SLANT_FOR_FACE (font_object);
      LFACE_SLANT (lface) = ! NILP (val) ? val : Qnormal;
    }
  if (force_p || UNSPECIFIEDP (LFACE_SWIDTH (lface)))
    {
      val = FONT_WIDTH_FOR_FACE (font_object);
      LFACE_SWIDTH (lface) = ! NILP (val) ? val : Qnormal;
    }

  LFACE_FONT (lface) = font_object;
  return 1;
}

#endif /* HAVE_WINDOW_SYSTEM */


/* Merges the face height FROM with the face height TO, and returns the
   merged height.  If FROM is an invalid height, then INVALID is
   returned instead.  FROM and TO may be either absolute face heights or
   `relative' heights; the returned value is always an absolute height
   unless both FROM and TO are relative.  */

static Lisp_Object
merge_face_heights (Lisp_Object from, Lisp_Object to, Lisp_Object invalid)
{
  Lisp_Object result = invalid;

  if (INTEGERP (from))
    /* FROM is absolute, just use it as is.  */
    result = from;
  else if (FLOATP (from))
    /* FROM is a scale, use it to adjust TO.  */
    {
      if (INTEGERP (to))
	/* relative X absolute => absolute */
	result = make_number ((EMACS_INT)(XFLOAT_DATA (from) * XINT (to)));
      else if (FLOATP (to))
	/* relative X relative => relative */
	result = make_float (XFLOAT_DATA (from) * XFLOAT_DATA (to));
      else if (UNSPECIFIEDP (to))
	result = from;
    }
  else if (FUNCTIONP (from))
    /* FROM is a function, which use to adjust TO.  */
    {
      /* Call function with current height as argument.
	 From is the new height.  */
      Lisp_Object args[2];

      args[0] = from;
      args[1] = to;
      result = safe_call (2, args);

      /* Ensure that if TO was absolute, so is the result.  */
      if (INTEGERP (to) && !INTEGERP (result))
	result = invalid;
    }

  return result;
}


/* Merge two Lisp face attribute vectors on frame F, FROM and TO, and
   store the resulting attributes in TO, which must be already be
   completely specified and contain only absolute attributes.  Every
   specified attribute of FROM overrides the corresponding attribute of
   TO; relative attributes in FROM are merged with the absolute value in
   TO and replace it.  NAMED_MERGE_POINTS is used internally to detect
   loops in face inheritance/remapping; it should be 0 when called from
   other places.  */

static inline void
merge_face_vectors (struct frame *f, Lisp_Object *from, Lisp_Object *to,
		    struct named_merge_point *named_merge_points)
{
  int i;

  /* If FROM inherits from some other faces, merge their attributes into
     TO before merging FROM's direct attributes.  Note that an :inherit
     attribute of `unspecified' is the same as one of nil; we never
     merge :inherit attributes, so nil is more correct, but lots of
     other code uses `unspecified' as a generic value for face attributes. */
  if (!UNSPECIFIEDP (from[LFACE_INHERIT_INDEX])
      && !NILP (from[LFACE_INHERIT_INDEX]))
    merge_face_ref (f, from[LFACE_INHERIT_INDEX], to, 0, named_merge_points);

  i = LFACE_FONT_INDEX;
  if (!UNSPECIFIEDP (from[i]))
    {
      if (!UNSPECIFIEDP (to[i]))
	to[i] = merge_font_spec (from[i], to[i]);
      else
	to[i] = copy_font_spec (from[i]);
      if (! NILP (AREF (to[i], FONT_FOUNDRY_INDEX)))
	to[LFACE_FOUNDRY_INDEX] = SYMBOL_NAME (AREF (to[i], FONT_FOUNDRY_INDEX));
      if (! NILP (AREF (to[i], FONT_FAMILY_INDEX)))
	to[LFACE_FAMILY_INDEX] = SYMBOL_NAME (AREF (to[i], FONT_FAMILY_INDEX));
      if (! NILP (AREF (to[i], FONT_WEIGHT_INDEX)))
	to[LFACE_WEIGHT_INDEX] = FONT_WEIGHT_FOR_FACE (to[i]);
      if (! NILP (AREF (to[i], FONT_SLANT_INDEX)))
	to[LFACE_SLANT_INDEX] = FONT_SLANT_FOR_FACE (to[i]);
      if (! NILP (AREF (to[i], FONT_WIDTH_INDEX)))
	to[LFACE_SWIDTH_INDEX] = FONT_WIDTH_FOR_FACE (to[i]);
      ASET (to[i], FONT_SIZE_INDEX, Qnil);
    }

  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (!UNSPECIFIEDP (from[i]))
      {
	if (i == LFACE_HEIGHT_INDEX && !INTEGERP (from[i]))
	  {
	    to[i] = merge_face_heights (from[i], to[i], to[i]);
	    font_clear_prop (to, FONT_SIZE_INDEX);
	  }
	else if (i != LFACE_FONT_INDEX
		 && ! EQ (to[i], from[i]))
	  {
	    to[i] = from[i];
	    if (i >= LFACE_FAMILY_INDEX && i <=LFACE_SLANT_INDEX)
	      font_clear_prop (to,
			       (i == LFACE_FAMILY_INDEX ? FONT_FAMILY_INDEX
				: i == LFACE_FOUNDRY_INDEX ? FONT_FOUNDRY_INDEX
				: i == LFACE_SWIDTH_INDEX ? FONT_WIDTH_INDEX
				: i == LFACE_HEIGHT_INDEX ? FONT_SIZE_INDEX
				: i == LFACE_WEIGHT_INDEX ? FONT_WEIGHT_INDEX
				: FONT_SLANT_INDEX));
	  }
      }

  /* TO is always an absolute face, which should inherit from nothing.
     We blindly copy the :inherit attribute above and fix it up here.  */
  to[LFACE_INHERIT_INDEX] = Qnil;
}

/* Merge the named face FACE_NAME on frame F, into the vector of face
   attributes TO.  NAMED_MERGE_POINTS is used to detect loops in face
   inheritance.  Returns true if FACE_NAME is a valid face name and
   merging succeeded.  */

static int
merge_named_face (struct frame *f, Lisp_Object face_name, Lisp_Object *to,
		  struct named_merge_point *named_merge_points)
{
  struct named_merge_point named_merge_point;

  if (push_named_merge_point (&named_merge_point,
			      face_name, NAMED_MERGE_POINT_NORMAL,
			      &named_merge_points))
    {
      struct gcpro gcpro1;
      Lisp_Object from[LFACE_VECTOR_SIZE];
      int ok = get_lface_attributes (f, face_name, from, 0, named_merge_points);

      if (ok)
	{
	  GCPRO1 (named_merge_point.face_name);
	  merge_face_vectors (f, from, to, named_merge_points);
	  UNGCPRO;
	}

      return ok;
    }
  else
    return 0;
}


/* Merge face attributes from the lisp `face reference' FACE_REF on
   frame F into the face attribute vector TO.  If ERR_MSGS is non-zero,
   problems with FACE_REF cause an error message to be shown.  Return
   non-zero if no errors occurred (regardless of the value of ERR_MSGS).
   NAMED_MERGE_POINTS is used to detect loops in face inheritance or
   list structure; it may be 0 for most callers.

   FACE_REF may be a single face specification or a list of such
   specifications.  Each face specification can be:

   1. A symbol or string naming a Lisp face.

   2. A property list of the form (KEYWORD VALUE ...) where each
   KEYWORD is a face attribute name, and value is an appropriate value
   for that attribute.

   3. Conses or the form (FOREGROUND-COLOR . COLOR) or
   (BACKGROUND-COLOR . COLOR) where COLOR is a color name.  This is
   for compatibility with 20.2.

   Face specifications earlier in lists take precedence over later
   specifications.  */

static int
merge_face_ref (struct frame *f, Lisp_Object face_ref, Lisp_Object *to,
		int err_msgs, struct named_merge_point *named_merge_points)
{
  int ok = 1;			/* Succeed without an error? */

  if (CONSP (face_ref))
    {
      Lisp_Object first = XCAR (face_ref);

      if (EQ (first, Qforeground_color)
	  || EQ (first, Qbackground_color))
	{
	  /* One of (FOREGROUND-COLOR . COLOR) or (BACKGROUND-COLOR
	     . COLOR).  COLOR must be a string.  */
	  Lisp_Object color_name = XCDR (face_ref);
	  Lisp_Object color = first;

	  if (STRINGP (color_name))
	    {
	      if (EQ (color, Qforeground_color))
		to[LFACE_FOREGROUND_INDEX] = color_name;
	      else
		to[LFACE_BACKGROUND_INDEX] = color_name;
	    }
	  else
	    {
	      if (err_msgs)
		add_to_log ("Invalid face color", color_name, Qnil);
	      ok = 0;
	    }
	}
      else if (SYMBOLP (first)
	       && *SDATA (SYMBOL_NAME (first)) == ':')
	{
	  /* Assume this is the property list form.  */
	  while (CONSP (face_ref) && CONSP (XCDR (face_ref)))
	    {
	      Lisp_Object keyword = XCAR (face_ref);
	      Lisp_Object value = XCAR (XCDR (face_ref));
	      int err = 0;

	      /* Specifying `unspecified' is a no-op.  */
	      if (EQ (value, Qunspecified))
		;
	      else if (EQ (keyword, QCfamily))
		{
		  if (STRINGP (value))
		    {
		      to[LFACE_FAMILY_INDEX] = value;
		      font_clear_prop (to, FONT_FAMILY_INDEX);
		    }
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCfoundry))
		{
		  if (STRINGP (value))
		    {
		      to[LFACE_FOUNDRY_INDEX] = value;
		      font_clear_prop (to, FONT_FOUNDRY_INDEX);
		    }
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCheight))
		{
		  Lisp_Object new_height =
		    merge_face_heights (value, to[LFACE_HEIGHT_INDEX], Qnil);

		  if (! NILP (new_height))
		    {
		      to[LFACE_HEIGHT_INDEX] = new_height;
		      font_clear_prop (to, FONT_SIZE_INDEX);
		    }
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCweight))
		{
		  if (SYMBOLP (value) && FONT_WEIGHT_NAME_NUMERIC (value) >= 0)
		    {
		      to[LFACE_WEIGHT_INDEX] = value;
		      font_clear_prop (to, FONT_WEIGHT_INDEX);
		    }
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCslant))
		{
		  if (SYMBOLP (value) && FONT_SLANT_NAME_NUMERIC (value) >= 0)
		    {
		      to[LFACE_SLANT_INDEX] = value;
		      font_clear_prop (to, FONT_SLANT_INDEX);
		    }
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCunderline))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_UNDERLINE_INDEX] = value;
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCoverline))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_OVERLINE_INDEX] = value;
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCstrike_through))
		{
		  if (EQ (value, Qt)
		      || NILP (value)
		      || STRINGP (value))
		    to[LFACE_STRIKE_THROUGH_INDEX] = value;
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCbox))
		{
		  if (EQ (value, Qt))
		    value = make_number (1);
		  if (INTEGERP (value)
		      || STRINGP (value)
		      || CONSP (value)
		      || NILP (value))
		    to[LFACE_BOX_INDEX] = value;
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCinverse_video)
		       || EQ (keyword, QCreverse_video))
		{
		  if (EQ (value, Qt) || NILP (value))
		    to[LFACE_INVERSE_INDEX] = value;
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCforeground))
		{
		  if (STRINGP (value))
		    to[LFACE_FOREGROUND_INDEX] = value;
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCbackground))
		{
		  if (STRINGP (value))
		    to[LFACE_BACKGROUND_INDEX] = value;
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCstipple))
		{
#if defined (HAVE_X_WINDOWS) || defined (HAVE_NS)
		  Lisp_Object pixmap_p = Fbitmap_spec_p (value);
		  if (!NILP (pixmap_p))
		    to[LFACE_STIPPLE_INDEX] = value;
		  else
		    err = 1;
#endif
		}
	      else if (EQ (keyword, QCwidth))
		{
		  if (SYMBOLP (value) && FONT_WIDTH_NAME_NUMERIC (value) >= 0)
		    {
		      to[LFACE_SWIDTH_INDEX] = value;
		      font_clear_prop (to, FONT_WIDTH_INDEX);
		    }
		  else
		    err = 1;
		}
	      else if (EQ (keyword, QCinherit))
		{
		  /* This is not really very useful; it's just like a
		     normal face reference.  */
		  if (! merge_face_ref (f, value, to,
					err_msgs, named_merge_points))
		    err = 1;
		}
	      else
		err = 1;

	      if (err)
		{
		  add_to_log ("Invalid face attribute %S %S", keyword, value);
		  ok = 0;
		}

	      face_ref = XCDR (XCDR (face_ref));
	    }
	}
      else
	{
	  /* This is a list of face refs.  Those at the beginning of the
	     list take precedence over what follows, so we have to merge
	     from the end backwards.  */
	  Lisp_Object next = XCDR (face_ref);

	  if (! NILP (next))
	    ok = merge_face_ref (f, next, to, err_msgs, named_merge_points);

	  if (! merge_face_ref (f, first, to, err_msgs, named_merge_points))
	    ok = 0;
	}
    }
  else
    {
      /* FACE_REF ought to be a face name.  */
      ok = merge_named_face (f, face_ref, to, named_merge_points);
      if (!ok && err_msgs)
	add_to_log ("Invalid face reference: %s", face_ref, Qnil);
    }

  return ok;
}


DEFUN ("internal-make-lisp-face", Finternal_make_lisp_face,
       Sinternal_make_lisp_face, 1, 2, 0,
       doc: /* Make FACE, a symbol, a Lisp face with all attributes nil.
If FACE was not known as a face before, create a new one.
If optional argument FRAME is specified, make a frame-local face
for that frame.  Otherwise operate on the global face definition.
Value is a vector of face attributes.  */)
  (Lisp_Object face, Lisp_Object frame)
{
  Lisp_Object global_lface, lface;
  struct frame *f;
  int i;

  CHECK_SYMBOL (face);
  global_lface = lface_from_face_name (NULL, face, 0);

  if (!NILP (frame))
    {
      CHECK_LIVE_FRAME (frame);
      f = XFRAME (frame);
      lface = lface_from_face_name (f, face, 0);
    }
  else
    f = NULL, lface = Qnil;

  /* Add a global definition if there is none.  */
  if (NILP (global_lface))
    {
      global_lface = Fmake_vector (make_number (LFACE_VECTOR_SIZE),
				   Qunspecified);
      ASET (global_lface, 0, Qface);
      Vface_new_frame_defaults = Fcons (Fcons (face, global_lface),
					Vface_new_frame_defaults);

      /* Assign the new Lisp face a unique ID.  The mapping from Lisp
	 face id to Lisp face is given by the vector lface_id_to_name.
	 The mapping from Lisp face to Lisp face id is given by the
	 property `face' of the Lisp face name.  */
      if (next_lface_id == lface_id_to_name_size)
	lface_id_to_name =
	  xpalloc (lface_id_to_name, &lface_id_to_name_size, 1,
		   min (INT_MAX, MOST_POSITIVE_FIXNUM),
		   sizeof *lface_id_to_name);

      lface_id_to_name[next_lface_id] = face;
      Fput (face, Qface, make_number (next_lface_id));
      ++next_lface_id;
    }
  else if (f == NULL)
    for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
      ASET (global_lface, i, Qunspecified);

  /* Add a frame-local definition.  */
  if (f)
    {
      if (NILP (lface))
	{
	  lface = Fmake_vector (make_number (LFACE_VECTOR_SIZE),
				Qunspecified);
	  ASET (lface, 0, Qface);
	  f->face_alist = Fcons (Fcons (face, lface), f->face_alist);
	}
      else
	for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
	  ASET (lface, i, Qunspecified);
    }
  else
    lface = global_lface;

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by incrementing face_change_count.  The next call to
     init_iterator will then free realized faces.  */
  if (NILP (Fget (face, Qface_no_inherit)))
    {
      ++face_change_count;
      ++windows_or_buffers_changed;
    }

  xassert (LFACEP (lface));
  check_lface (lface);
  return lface;
}


DEFUN ("internal-lisp-face-p", Finternal_lisp_face_p,
       Sinternal_lisp_face_p, 1, 2, 0,
       doc: /* Return non-nil if FACE names a face.
FACE should be a symbol or string.
If optional second argument FRAME is non-nil, check for the
existence of a frame-local face with name FACE on that frame.
Otherwise check for the existence of a global face.  */)
  (Lisp_Object face, Lisp_Object frame)
{
  Lisp_Object lface;

  face = resolve_face_name (face, 1);

  if (!NILP (frame))
    {
      CHECK_LIVE_FRAME (frame);
      lface = lface_from_face_name (XFRAME (frame), face, 0);
    }
  else
    lface = lface_from_face_name (NULL, face, 0);

  return lface;
}


DEFUN ("internal-copy-lisp-face", Finternal_copy_lisp_face,
       Sinternal_copy_lisp_face, 4, 4, 0,
       doc: /* Copy face FROM to TO.
If FRAME is t, copy the global face definition of FROM.
Otherwise, copy the frame-local definition of FROM on FRAME.
If NEW-FRAME is a frame, copy that data into the frame-local
definition of TO on NEW-FRAME.  If NEW-FRAME is nil,
FRAME controls where the data is copied to.

The value is TO.  */)
  (Lisp_Object from, Lisp_Object to, Lisp_Object frame, Lisp_Object new_frame)
{
  Lisp_Object lface, copy;

  CHECK_SYMBOL (from);
  CHECK_SYMBOL (to);

  if (EQ (frame, Qt))
    {
      /* Copy global definition of FROM.  We don't make copies of
	 strings etc. because 20.2 didn't do it either.  */
      lface = lface_from_face_name (NULL, from, 1);
      copy = Finternal_make_lisp_face (to, Qnil);
    }
  else
    {
      /* Copy frame-local definition of FROM.  */
      if (NILP (new_frame))
	new_frame = frame;
      CHECK_LIVE_FRAME (frame);
      CHECK_LIVE_FRAME (new_frame);
      lface = lface_from_face_name (XFRAME (frame), from, 1);
      copy = Finternal_make_lisp_face (to, new_frame);
    }

  memcpy (XVECTOR (copy)->contents, XVECTOR (lface)->contents,
	  LFACE_VECTOR_SIZE * sizeof (Lisp_Object));

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by incrementing face_change_count.  The next call to
     init_iterator will then free realized faces.  */
  if (NILP (Fget (to, Qface_no_inherit)))
    {
      ++face_change_count;
      ++windows_or_buffers_changed;
    }

  return to;
}


DEFUN ("internal-set-lisp-face-attribute", Finternal_set_lisp_face_attribute,
       Sinternal_set_lisp_face_attribute, 3, 4, 0,
       doc: /* Set attribute ATTR of FACE to VALUE.
FRAME being a frame means change the face on that frame.
FRAME nil means change the face of the selected frame.
FRAME t means change the default for new frames.
FRAME 0 means change the face on all frames, and change the default
  for new frames.  */)
  (Lisp_Object face, Lisp_Object attr, Lisp_Object value, Lisp_Object frame)
{
  Lisp_Object lface;
  Lisp_Object old_value = Qnil;
  /* Set one of enum font_property_index (> 0) if ATTR is one of
     font-related attributes other than QCfont and QCfontset.  */
  enum font_property_index prop_index = 0;

  CHECK_SYMBOL (face);
  CHECK_SYMBOL (attr);

  face = resolve_face_name (face, 1);

  /* If FRAME is 0, change face on all frames, and change the
     default for new frames.  */
  if (INTEGERP (frame) && XINT (frame) == 0)
    {
      Lisp_Object tail;
      Finternal_set_lisp_face_attribute (face, attr, value, Qt);
      FOR_EACH_FRAME (tail, frame)
	Finternal_set_lisp_face_attribute (face, attr, value, frame);
      return face;
    }

  /* Set lface to the Lisp attribute vector of FACE.  */
  if (EQ (frame, Qt))
    {
      lface = lface_from_face_name (NULL, face, 1);

      /* When updating face-new-frame-defaults, we put :ignore-defface
	 where the caller wants `unspecified'.  This forces the frame
	 defaults to ignore the defface value.  Otherwise, the defface
	 will take effect, which is generally not what is intended.
	 The value of that attribute will be inherited from some other
	 face during face merging.  See internal_merge_in_global_face. */
      if (UNSPECIFIEDP (value))
	value = QCignore_defface;
    }
  else
    {
      if (NILP (frame))
	frame = selected_frame;

      CHECK_LIVE_FRAME (frame);
      lface = lface_from_face_name (XFRAME (frame), face, 0);

      /* If a frame-local face doesn't exist yet, create one.  */
      if (NILP (lface))
	lface = Finternal_make_lisp_face (face, frame);
    }

  if (EQ (attr, QCfamily))
    {
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	{
	  CHECK_STRING (value);
	  if (SCHARS (value) == 0)
	    signal_error ("Invalid face family", value);
	}
      old_value = LFACE_FAMILY (lface);
      LFACE_FAMILY (lface) = value;
      prop_index = FONT_FAMILY_INDEX;
    }
  else if (EQ (attr, QCfoundry))
    {
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	{
	  CHECK_STRING (value);
	  if (SCHARS (value) == 0)
	    signal_error ("Invalid face foundry", value);
	}
      old_value = LFACE_FOUNDRY (lface);
      LFACE_FOUNDRY (lface) = value;
      prop_index = FONT_FOUNDRY_INDEX;
    }
  else if (EQ (attr, QCheight))
    {
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	{
	  if (EQ (face, Qdefault))
	    {
	      /* The default face must have an absolute size.  */
	      if (!INTEGERP (value) || XINT (value) <= 0)
		signal_error ("Default face height not absolute and positive",
			      value);
	    }
	  else
	    {
	      /* For non-default faces, do a test merge with a random
		 height to see if VALUE's ok. */
	      Lisp_Object test = merge_face_heights (value,
						     make_number (10),
						     Qnil);
	      if (!INTEGERP (test) || XINT (test) <= 0)
		signal_error ("Face height does not produce a positive integer",
			      value);
	    }
	}

      old_value = LFACE_HEIGHT (lface);
      LFACE_HEIGHT (lface) = value;
      prop_index = FONT_SIZE_INDEX;
    }
  else if (EQ (attr, QCweight))
    {
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	{
	  CHECK_SYMBOL (value);
	  if (FONT_WEIGHT_NAME_NUMERIC (value) < 0)
	    signal_error ("Invalid face weight", value);
	}
      old_value = LFACE_WEIGHT (lface);
      LFACE_WEIGHT (lface) = value;
      prop_index = FONT_WEIGHT_INDEX;
    }
  else if (EQ (attr, QCslant))
    {
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	{
	  CHECK_SYMBOL (value);
	  if (FONT_SLANT_NAME_NUMERIC (value) < 0)
	    signal_error ("Invalid face slant", value);
	}
      old_value = LFACE_SLANT (lface);
      LFACE_SLANT (lface) = value;
      prop_index = FONT_SLANT_INDEX;
    }
  else if (EQ (attr, QCunderline))
    {
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	if ((SYMBOLP (value)
	     && !EQ (value, Qt)
	     && !EQ (value, Qnil))
	    /* Underline color.  */
	    || (STRINGP (value)
		&& SCHARS (value) == 0))
	  signal_error ("Invalid face underline", value);

      old_value = LFACE_UNDERLINE (lface);
      LFACE_UNDERLINE (lface) = value;
    }
  else if (EQ (attr, QCoverline))
    {
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	if ((SYMBOLP (value)
	     && !EQ (value, Qt)
	     && !EQ (value, Qnil))
	    /* Overline color.  */
	    || (STRINGP (value)
		&& SCHARS (value) == 0))
	  signal_error ("Invalid face overline", value);

      old_value = LFACE_OVERLINE (lface);
      LFACE_OVERLINE (lface) = value;
    }
  else if (EQ (attr, QCstrike_through))
    {
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	if ((SYMBOLP (value)
	     && !EQ (value, Qt)
	     && !EQ (value, Qnil))
	    /* Strike-through color.  */
	    || (STRINGP (value)
		&& SCHARS (value) == 0))
	  signal_error ("Invalid face strike-through", value);

      old_value = LFACE_STRIKE_THROUGH (lface);
      LFACE_STRIKE_THROUGH (lface) = value;
    }
  else if (EQ (attr, QCbox))
    {
      int valid_p;

      /* Allow t meaning a simple box of width 1 in foreground color
	 of the face.  */
      if (EQ (value, Qt))
	value = make_number (1);

      if (UNSPECIFIEDP (value) || IGNORE_DEFFACE_P (value))
	valid_p = 1;
      else if (NILP (value))
	valid_p = 1;
      else if (INTEGERP (value))
	valid_p = XINT (value) != 0;
      else if (STRINGP (value))
	valid_p = SCHARS (value) > 0;
      else if (CONSP (value))
	{
	  Lisp_Object tem;

	  tem = value;
	  while (CONSP (tem))
	    {
	      Lisp_Object k, v;

	      k = XCAR (tem);
	      tem = XCDR (tem);
	      if (!CONSP (tem))
		break;
	      v = XCAR (tem);
	      tem = XCDR (tem);

	      if (EQ (k, QCline_width))
		{
		  if (!INTEGERP (v) || XINT (v) == 0)
		    break;
		}
	      else if (EQ (k, QCcolor))
		{
		  if (!NILP (v) && (!STRINGP (v) || SCHARS (v) == 0))
		    break;
		}
	      else if (EQ (k, QCstyle))
		{
		  if (!EQ (v, Qpressed_button) && !EQ (v, Qreleased_button))
		    break;
		}
	      else
		break;
	    }

	  valid_p = NILP (tem);
	}
      else
	valid_p = 0;

      if (!valid_p)
	signal_error ("Invalid face box", value);

      old_value = LFACE_BOX (lface);
      LFACE_BOX (lface) = value;
    }
  else if (EQ (attr, QCinverse_video)
	   || EQ (attr, QCreverse_video))
    {
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	{
	  CHECK_SYMBOL (value);
	  if (!EQ (value, Qt) && !NILP (value))
	    signal_error ("Invalid inverse-video face attribute value", value);
	}
      old_value = LFACE_INVERSE (lface);
      LFACE_INVERSE (lface) = value;
    }
  else if (EQ (attr, QCforeground))
    {
      /* Compatibility with 20.x.  */
      if (NILP (value))
	value = Qunspecified;
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	{
	  /* Don't check for valid color names here because it depends
	     on the frame (display) whether the color will be valid
	     when the face is realized.  */
	  CHECK_STRING (value);
	  if (SCHARS (value) == 0)
	    signal_error ("Empty foreground color value", value);
	}
      old_value = LFACE_FOREGROUND (lface);
      LFACE_FOREGROUND (lface) = value;
    }
  else if (EQ (attr, QCbackground))
    {
      /* Compatibility with 20.x.  */
      if (NILP (value))
	value = Qunspecified;
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	{
	  /* Don't check for valid color names here because it depends
	     on the frame (display) whether the color will be valid
	     when the face is realized.  */
	  CHECK_STRING (value);
	  if (SCHARS (value) == 0)
	    signal_error ("Empty background color value", value);
	}
      old_value = LFACE_BACKGROUND (lface);
      LFACE_BACKGROUND (lface) = value;
    }
  else if (EQ (attr, QCstipple))
    {
#if defined (HAVE_X_WINDOWS) || defined (HAVE_NS)
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value)
	  && !NILP (value)
	  && NILP (Fbitmap_spec_p (value)))
	signal_error ("Invalid stipple attribute", value);
      old_value = LFACE_STIPPLE (lface);
      LFACE_STIPPLE (lface) = value;
#endif /* HAVE_X_WINDOWS || HAVE_NS */
    }
  else if (EQ (attr, QCwidth))
    {
      if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	{
	  CHECK_SYMBOL (value);
	  if (FONT_WIDTH_NAME_NUMERIC (value) < 0)
	    signal_error ("Invalid face width", value);
	}
      old_value = LFACE_SWIDTH (lface);
      LFACE_SWIDTH (lface) = value;
      prop_index = FONT_WIDTH_INDEX;
    }
  else if (EQ (attr, QCfont))
    {
#ifdef HAVE_WINDOW_SYSTEM
      if (EQ (frame, Qt) || FRAME_WINDOW_P (XFRAME (frame)))
	{
	  if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value))
	    {
	      FRAME_PTR f;

	      old_value = LFACE_FONT (lface);
	      if (! FONTP (value))
		{
		  if (STRINGP (value))
		    {
		      Lisp_Object name = value;
		      int fontset = fs_query_fontset (name, 0);

		      if (fontset >= 0)
			name = fontset_ascii (fontset);
		      value = font_spec_from_name (name);
		      if (!FONTP (value))
			signal_error ("Invalid font name", name);
		    }
		  else
		    signal_error ("Invalid font or font-spec", value);
		}
	      if (EQ (frame, Qt))
		f = XFRAME (selected_frame);
	      else
		f = XFRAME (frame);
	      if (! FONT_OBJECT_P (value))
		{
		  Lisp_Object *attrs = XVECTOR (lface)->contents;
		  Lisp_Object font_object;

		  font_object = font_load_for_lface (f, attrs, value);
		  if (NILP (font_object))
		    signal_error ("Font not available", value);
		  value = font_object;
		}
	      set_lface_from_font (f, lface, value, 1);
	    }
	  else
	    LFACE_FONT (lface) = value;
	}
#endif /* HAVE_WINDOW_SYSTEM */
    }
  else if (EQ (attr, QCfontset))
    {
#ifdef HAVE_WINDOW_SYSTEM
      if (EQ (frame, Qt) || FRAME_WINDOW_P (XFRAME (frame)))
	{
	  Lisp_Object tmp;

	  old_value = LFACE_FONTSET (lface);
	  tmp = Fquery_fontset (value, Qnil);
	  if (NILP (tmp))
	    signal_error ("Invalid fontset name", value);
	  LFACE_FONTSET (lface) = value = tmp;
	}
#endif /* HAVE_WINDOW_SYSTEM */
    }
  else if (EQ (attr, QCinherit))
    {
      Lisp_Object tail;
      if (SYMBOLP (value))
	tail = Qnil;
      else
	for (tail = value; CONSP (tail); tail = XCDR (tail))
	  if (!SYMBOLP (XCAR (tail)))
	    break;
      if (NILP (tail))
	LFACE_INHERIT (lface) = value;
      else
	signal_error ("Invalid face inheritance", value);
    }
  else if (EQ (attr, QCbold))
    {
      old_value = LFACE_WEIGHT (lface);
      LFACE_WEIGHT (lface) = NILP (value) ? Qnormal : Qbold;
      prop_index = FONT_WEIGHT_INDEX;
    }
  else if (EQ (attr, QCitalic))
    {
      attr = QCslant;
      old_value = LFACE_SLANT (lface);
      LFACE_SLANT (lface) = NILP (value) ? Qnormal : Qitalic;
      prop_index = FONT_SLANT_INDEX;
    }
  else
    signal_error ("Invalid face attribute name", attr);

  if (prop_index)
    {
      /* If a font-related attribute other than QCfont and QCfontset
	 is specified, and if the original QCfont attribute has a font
	 (font-spec or font-object), set the corresponding property in
	 the font to nil so that the font selector doesn't think that
	 the attribute is mandatory.  Also, clear the average
	 width.  */
      font_clear_prop (XVECTOR (lface)->contents, prop_index);
    }

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by incrementing face_change_count.  The next call to
     init_iterator will then free realized faces.  */
  if (!EQ (frame, Qt)
      && NILP (Fget (face, Qface_no_inherit))
      && NILP (Fequal (old_value, value)))
    {
      ++face_change_count;
      ++windows_or_buffers_changed;
    }

  if (!UNSPECIFIEDP (value) && !IGNORE_DEFFACE_P (value)
      && NILP (Fequal (old_value, value)))
    {
      Lisp_Object param;

      param = Qnil;

      if (EQ (face, Qdefault))
	{
#ifdef HAVE_WINDOW_SYSTEM
	  /* Changed font-related attributes of the `default' face are
	     reflected in changed `font' frame parameters. */
	  if (FRAMEP (frame)
	      && (prop_index || EQ (attr, QCfont))
	      && lface_fully_specified_p (XVECTOR (lface)->contents))
	    set_font_frame_param (frame, lface);
	  else
#endif /* HAVE_WINDOW_SYSTEM */

	  if (EQ (attr, QCforeground))
	    param = Qforeground_color;
	  else if (EQ (attr, QCbackground))
	    param = Qbackground_color;
	}
#ifdef HAVE_WINDOW_SYSTEM
#ifndef WINDOWSNT
      else if (EQ (face, Qscroll_bar))
	{
	  /* Changing the colors of `scroll-bar' sets frame parameters
	     `scroll-bar-foreground' and `scroll-bar-background'. */
	  if (EQ (attr, QCforeground))
	    param = Qscroll_bar_foreground;
	  else if (EQ (attr, QCbackground))
	    param = Qscroll_bar_background;
	}
#endif /* not WINDOWSNT */
      else if (EQ (face, Qborder))
	{
	  /* Changing background color of `border' sets frame parameter
	     `border-color'.  */
	  if (EQ (attr, QCbackground))
	    param = Qborder_color;
	}
      else if (EQ (face, Qcursor))
	{
	  /* Changing background color of `cursor' sets frame parameter
	     `cursor-color'.  */
	  if (EQ (attr, QCbackground))
	    param = Qcursor_color;
	}
      else if (EQ (face, Qmouse))
	{
	  /* Changing background color of `mouse' sets frame parameter
	     `mouse-color'.  */
	  if (EQ (attr, QCbackground))
	    param = Qmouse_color;
	}
#endif /* HAVE_WINDOW_SYSTEM */
      else if (EQ (face, Qmenu))
	{
	  /* Indicate that we have to update the menu bar when
	     realizing faces on FRAME.  FRAME t change the
	     default for new frames.  We do this by setting
	     setting the flag in new face caches   */
	  if (FRAMEP (frame))
	    {
	      struct frame *f = XFRAME (frame);
	      if (FRAME_FACE_CACHE (f) == NULL)
		FRAME_FACE_CACHE (f) = make_face_cache (f);
	      FRAME_FACE_CACHE (f)->menu_face_changed_p = 1;
	    }
	  else
	    menu_face_changed_default = 1;
	}

      if (!NILP (param))
	{
	  if (EQ (frame, Qt))
	    /* Update `default-frame-alist', which is used for new frames.  */
	    {
	      store_in_alist (&Vdefault_frame_alist, param, value);
	    }
	  else
	    /* Update the current frame's parameters.  */
	    {
	      Lisp_Object cons;
	      cons = XCAR (Vparam_value_alist);
	      XSETCAR (cons, param);
	      XSETCDR (cons, value);
	      Fmodify_frame_parameters (frame, Vparam_value_alist);
	    }
	}
    }

  return face;
}


/* Update the corresponding face when frame parameter PARAM on frame F
   has been assigned the value NEW_VALUE.  */

void
update_face_from_frame_parameter (struct frame *f, Lisp_Object param,
				  Lisp_Object new_value)
{
  Lisp_Object face = Qnil;
  Lisp_Object lface;

  /* If there are no faces yet, give up.  This is the case when called
     from Fx_create_frame, and we do the necessary things later in
     face-set-after-frame-defaults.  */
  if (NILP (f->face_alist))
    return;

  if (EQ (param, Qforeground_color))
    {
      face = Qdefault;
      lface = lface_from_face_name (f, face, 1);
      LFACE_FOREGROUND (lface) = (STRINGP (new_value)
				  ? new_value : Qunspecified);
      realize_basic_faces (f);
    }
  else if (EQ (param, Qbackground_color))
    {
      Lisp_Object frame;

      /* Changing the background color might change the background
	 mode, so that we have to load new defface specs.
	 Call frame-set-background-mode to do that.  */
      XSETFRAME (frame, f);
      call1 (Qframe_set_background_mode, frame);

      face = Qdefault;
      lface = lface_from_face_name (f, face, 1);
      LFACE_BACKGROUND (lface) = (STRINGP (new_value)
				  ? new_value : Qunspecified);
      realize_basic_faces (f);
    }
#ifdef HAVE_WINDOW_SYSTEM
  else if (EQ (param, Qborder_color))
    {
      face = Qborder;
      lface = lface_from_face_name (f, face, 1);
      LFACE_BACKGROUND (lface) = (STRINGP (new_value)
				  ? new_value : Qunspecified);
    }
  else if (EQ (param, Qcursor_color))
    {
      face = Qcursor;
      lface = lface_from_face_name (f, face, 1);
      LFACE_BACKGROUND (lface) = (STRINGP (new_value)
				  ? new_value : Qunspecified);
    }
  else if (EQ (param, Qmouse_color))
    {
      face = Qmouse;
      lface = lface_from_face_name (f, face, 1);
      LFACE_BACKGROUND (lface) = (STRINGP (new_value)
				  ? new_value : Qunspecified);
    }
#endif

  /* Changing a named face means that all realized faces depending on
     that face are invalid.  Since we cannot tell which realized faces
     depend on the face, make sure they are all removed.  This is done
     by incrementing face_change_count.  The next call to
     init_iterator will then free realized faces.  */
  if (!NILP (face)
      && NILP (Fget (face, Qface_no_inherit)))
    {
      ++face_change_count;
      ++windows_or_buffers_changed;
    }
}


#ifdef HAVE_WINDOW_SYSTEM

/* Set the `font' frame parameter of FRAME determined from the
   font-object set in `default' face attributes LFACE.  */

static void
set_font_frame_param (Lisp_Object frame, Lisp_Object lface)
{
  struct frame *f = XFRAME (frame);
  Lisp_Object font;

  if (FRAME_WINDOW_P (f)
      /* Don't do anything if the font is `unspecified'.  This can
	 happen during frame creation.  */
      && (font = LFACE_FONT (lface),
	  ! UNSPECIFIEDP (font)))
    {
      if (FONT_SPEC_P (font))
	{
	  font = font_load_for_lface (f, XVECTOR (lface)->contents, font);
	  if (NILP (font))
	    return;
	  LFACE_FONT (lface) = font;
	}
      f->default_face_done_p = 0;
      Fmodify_frame_parameters (frame, Fcons (Fcons (Qfont, font), Qnil));
    }
}


/* Get the value of X resource RESOURCE, class CLASS for the display
   of frame FRAME.  This is here because ordinary `x-get-resource'
   doesn't take a frame argument.  */

DEFUN ("internal-face-x-get-resource", Finternal_face_x_get_resource,
       Sinternal_face_x_get_resource, 3, 3, 0, doc: /* */)
  (Lisp_Object resource, Lisp_Object class, Lisp_Object frame)
{
  Lisp_Object value = Qnil;
  CHECK_STRING (resource);
  CHECK_STRING (class);
  CHECK_LIVE_FRAME (frame);
  BLOCK_INPUT;
  value = display_x_get_resource (FRAME_X_DISPLAY_INFO (XFRAME (frame)),
				  resource, class, Qnil, Qnil);
  UNBLOCK_INPUT;
  return value;
}


/* Return resource string VALUE as a boolean value, i.e. nil, or t.
   If VALUE is "on" or "true", return t.  If VALUE is "off" or
   "false", return nil.  Otherwise, if SIGNAL_P is non-zero, signal an
   error; if SIGNAL_P is zero, return 0.  */

static Lisp_Object
face_boolean_x_resource_value (Lisp_Object value, int signal_p)
{
  Lisp_Object result = make_number (0);

  xassert (STRINGP (value));

  if (xstrcasecmp (SSDATA (value), "on") == 0
      || xstrcasecmp (SSDATA (value), "true") == 0)
    result = Qt;
  else if (xstrcasecmp (SSDATA (value), "off") == 0
	   || xstrcasecmp (SSDATA (value), "false") == 0)
    result = Qnil;
  else if (xstrcasecmp (SSDATA (value), "unspecified") == 0)
    result = Qunspecified;
  else if (signal_p)
    signal_error ("Invalid face attribute value from X resource", value);

  return result;
}


DEFUN ("internal-set-lisp-face-attribute-from-resource",
       Finternal_set_lisp_face_attribute_from_resource,
       Sinternal_set_lisp_face_attribute_from_resource,
       3, 4, 0, doc: /* */)
  (Lisp_Object face, Lisp_Object attr, Lisp_Object value, Lisp_Object frame)
{
  CHECK_SYMBOL (face);
  CHECK_SYMBOL (attr);
  CHECK_STRING (value);

  if (xstrcasecmp (SSDATA (value), "unspecified") == 0)
    value = Qunspecified;
  else if (EQ (attr, QCheight))
    {
      value = Fstring_to_number (value, make_number (10));
      if (XINT (value) <= 0)
	signal_error ("Invalid face height from X resource", value);
    }
  else if (EQ (attr, QCbold) || EQ (attr, QCitalic))
    value = face_boolean_x_resource_value (value, 1);
  else if (EQ (attr, QCweight) || EQ (attr, QCslant) || EQ (attr, QCwidth))
    value = intern (SSDATA (value));
  else if (EQ (attr, QCreverse_video) || EQ (attr, QCinverse_video))
    value = face_boolean_x_resource_value (value, 1);
  else if (EQ (attr, QCunderline)
	   || EQ (attr, QCoverline)
	   || EQ (attr, QCstrike_through))
    {
      Lisp_Object boolean_value;

      /* If the result of face_boolean_x_resource_value is t or nil,
	 VALUE does NOT specify a color. */
      boolean_value = face_boolean_x_resource_value (value, 0);
      if (SYMBOLP (boolean_value))
	value = boolean_value;
    }
  else if (EQ (attr, QCbox) || EQ (attr, QCinherit))
    value = Fcar (Fread_from_string (value, Qnil, Qnil));

  return Finternal_set_lisp_face_attribute (face, attr, value, frame);
}

#endif /* HAVE_WINDOW_SYSTEM */


/***********************************************************************
			      Menu face
 ***********************************************************************/

#if defined HAVE_X_WINDOWS && defined USE_X_TOOLKIT

/* Make menus on frame F appear as specified by the `menu' face.  */

static void
x_update_menu_appearance (struct frame *f)
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  XrmDatabase rdb;

  if (dpyinfo
      && (rdb = XrmGetDatabase (FRAME_X_DISPLAY (f)),
	  rdb != NULL))
    {
      char line[512];
      char *buf = line;
      ptrdiff_t bufsize = sizeof line;
      Lisp_Object lface = lface_from_face_name (f, Qmenu, 1);
      struct face *face = FACE_FROM_ID (f, MENU_FACE_ID);
      const char *myname = SSDATA (Vx_resource_name);
      int changed_p = 0;
#ifdef USE_MOTIF
      const char *popup_path = "popup_menu";
#else
      const char *popup_path = "menu.popup";
#endif

      if (STRINGP (LFACE_FOREGROUND (lface)))
	{
	  exprintf (&buf, &bufsize, line, -1, "%s.%s*foreground: %s",
		    myname, popup_path,
		    SDATA (LFACE_FOREGROUND (lface)));
	  XrmPutLineResource (&rdb, line);
	  exprintf (&buf, &bufsize, line, -1, "%s.pane.menubar*foreground: %s",
		    myname, SDATA (LFACE_FOREGROUND (lface)));
	  XrmPutLineResource (&rdb, line);
	  changed_p = 1;
	}

      if (STRINGP (LFACE_BACKGROUND (lface)))
	{
	  exprintf (&buf, &bufsize, line, -1, "%s.%s*background: %s",
		    myname, popup_path,
		    SDATA (LFACE_BACKGROUND (lface)));
	  XrmPutLineResource (&rdb, line);

	  exprintf (&buf, &bufsize, line, -1, "%s.pane.menubar*background: %s",
		    myname, SDATA (LFACE_BACKGROUND (lface)));
	  XrmPutLineResource (&rdb, line);
	  changed_p = 1;
	}

      if (face->font
	  /* On Solaris 5.8, it's been reported that the `menu' face
	     can be unspecified here, during startup.  Why this
	     happens remains unknown.  -- cyd  */
	  && FONTP (LFACE_FONT (lface))
	  && (!UNSPECIFIEDP (LFACE_FAMILY (lface))
	      || !UNSPECIFIEDP (LFACE_FOUNDRY (lface))
	      || !UNSPECIFIEDP (LFACE_SWIDTH (lface))
	      || !UNSPECIFIEDP (LFACE_WEIGHT (lface))
	      || !UNSPECIFIEDP (LFACE_SLANT (lface))
	      || !UNSPECIFIEDP (LFACE_HEIGHT (lface))))
	{
	  Lisp_Object xlfd = Ffont_xlfd_name (LFACE_FONT (lface), Qnil);
#ifdef USE_MOTIF
	  const char *suffix = "List";
	  Bool motif = True;
#else
#if defined HAVE_X_I18N

	  const char *suffix = "Set";
#else
	  const char *suffix = "";
#endif
	  Bool motif = False;
#endif

	  if (! NILP (xlfd))
	    {
#if defined HAVE_X_I18N
	      char *fontsetname = xic_create_fontsetname (SSDATA (xlfd), motif);
#else
	      char *fontsetname = SSDATA (xlfd);
#endif
	      exprintf (&buf, &bufsize, line, -1, "%s.pane.menubar*font%s: %s",
			myname, suffix, fontsetname);
	      XrmPutLineResource (&rdb, line);

	      exprintf (&buf, &bufsize, line, -1, "%s.%s*font%s: %s",
			myname, popup_path, suffix, fontsetname);
	      XrmPutLineResource (&rdb, line);
	      changed_p = 1;
	      if (fontsetname != SSDATA (xlfd))
		xfree (fontsetname);
	    }
	}

      if (changed_p && f->output_data.x->menubar_widget)
	free_frame_menubar (f);

      if (buf != line)
	xfree (buf);
    }
}

#endif /* HAVE_X_WINDOWS && USE_X_TOOLKIT */


DEFUN ("face-attribute-relative-p", Fface_attribute_relative_p,
       Sface_attribute_relative_p,
       2, 2, 0,
       doc: /* Check whether a face attribute value is relative.
Specifically, this function returns t if the attribute ATTRIBUTE
with the value VALUE is relative.

A relative value is one that doesn't entirely override whatever is
inherited from another face.  For most possible attributes,
the only relative value that users see is `unspecified'.
However, for :height, floating point values are also relative.  */)
  (Lisp_Object attribute, Lisp_Object value)
{
  if (EQ (value, Qunspecified) || (EQ (value, QCignore_defface)))
    return Qt;
  else if (EQ (attribute, QCheight))
    return INTEGERP (value) ? Qnil : Qt;
  else
    return Qnil;
}

DEFUN ("merge-face-attribute", Fmerge_face_attribute, Smerge_face_attribute,
       3, 3, 0,
       doc: /* Return face ATTRIBUTE VALUE1 merged with VALUE2.
If VALUE1 or VALUE2 are absolute (see `face-attribute-relative-p'), then
the result will be absolute, otherwise it will be relative.  */)
  (Lisp_Object attribute, Lisp_Object value1, Lisp_Object value2)
{
  if (EQ (value1, Qunspecified) || EQ (value1, QCignore_defface))
    return value2;
  else if (EQ (attribute, QCheight))
    return merge_face_heights (value1, value2, value1);
  else
    return value1;
}


DEFUN ("internal-get-lisp-face-attribute", Finternal_get_lisp_face_attribute,
       Sinternal_get_lisp_face_attribute,
       2, 3, 0,
       doc: /* Return face attribute KEYWORD of face SYMBOL.
If SYMBOL does not name a valid Lisp face or KEYWORD isn't a valid
face attribute name, signal an error.
If the optional argument FRAME is given, report on face SYMBOL in that
frame.  If FRAME is t, report on the defaults for face SYMBOL (for new
frames).  If FRAME is omitted or nil, use the selected frame.  */)
  (Lisp_Object symbol, Lisp_Object keyword, Lisp_Object frame)
{
  Lisp_Object lface, value = Qnil;

  CHECK_SYMBOL (symbol);
  CHECK_SYMBOL (keyword);

  if (EQ (frame, Qt))
    lface = lface_from_face_name (NULL, symbol, 1);
  else
    {
      if (NILP (frame))
	frame = selected_frame;
      CHECK_LIVE_FRAME (frame);
      lface = lface_from_face_name (XFRAME (frame), symbol, 1);
    }

  if (EQ (keyword, QCfamily))
    value = LFACE_FAMILY (lface);
  else if (EQ (keyword, QCfoundry))
    value = LFACE_FOUNDRY (lface);
  else if (EQ (keyword, QCheight))
    value = LFACE_HEIGHT (lface);
  else if (EQ (keyword, QCweight))
    value = LFACE_WEIGHT (lface);
  else if (EQ (keyword, QCslant))
    value = LFACE_SLANT (lface);
  else if (EQ (keyword, QCunderline))
    value = LFACE_UNDERLINE (lface);
  else if (EQ (keyword, QCoverline))
    value = LFACE_OVERLINE (lface);
  else if (EQ (keyword, QCstrike_through))
    value = LFACE_STRIKE_THROUGH (lface);
  else if (EQ (keyword, QCbox))
    value = LFACE_BOX (lface);
  else if (EQ (keyword, QCinverse_video)
	   || EQ (keyword, QCreverse_video))
    value = LFACE_INVERSE (lface);
  else if (EQ (keyword, QCforeground))
    value = LFACE_FOREGROUND (lface);
  else if (EQ (keyword, QCbackground))
    value = LFACE_BACKGROUND (lface);
  else if (EQ (keyword, QCstipple))
    value = LFACE_STIPPLE (lface);
  else if (EQ (keyword, QCwidth))
    value = LFACE_SWIDTH (lface);
  else if (EQ (keyword, QCinherit))
    value = LFACE_INHERIT (lface);
  else if (EQ (keyword, QCfont))
    value = LFACE_FONT (lface);
  else if (EQ (keyword, QCfontset))
    value = LFACE_FONTSET (lface);
  else
    signal_error ("Invalid face attribute name", keyword);

  if (IGNORE_DEFFACE_P (value))
    return Qunspecified;

  return value;
}


DEFUN ("internal-lisp-face-attribute-values",
       Finternal_lisp_face_attribute_values,
       Sinternal_lisp_face_attribute_values, 1, 1, 0,
       doc: /* Return a list of valid discrete values for face attribute ATTR.
Value is nil if ATTR doesn't have a discrete set of valid values.  */)
  (Lisp_Object attr)
{
  Lisp_Object result = Qnil;

  CHECK_SYMBOL (attr);

  if (EQ (attr, QCunderline))
    result = Fcons (Qt, Fcons (Qnil, Qnil));
  else if (EQ (attr, QCoverline))
    result = Fcons (Qt, Fcons (Qnil, Qnil));
  else if (EQ (attr, QCstrike_through))
    result = Fcons (Qt, Fcons (Qnil, Qnil));
  else if (EQ (attr, QCinverse_video) || EQ (attr, QCreverse_video))
    result = Fcons (Qt, Fcons (Qnil, Qnil));

  return result;
}


DEFUN ("internal-merge-in-global-face", Finternal_merge_in_global_face,
       Sinternal_merge_in_global_face, 2, 2, 0,
       doc: /* Add attributes from frame-default definition of FACE to FACE on FRAME.
Default face attributes override any local face attributes.  */)
  (Lisp_Object face, Lisp_Object frame)
{
  int i;
  Lisp_Object global_lface, local_lface, *gvec, *lvec;
  struct frame *f = XFRAME (frame);

  CHECK_LIVE_FRAME (frame);
  global_lface = lface_from_face_name (NULL, face, 1);
  local_lface = lface_from_face_name (f, face, 0);
  if (NILP (local_lface))
    local_lface = Finternal_make_lisp_face (face, frame);

  /* Make every specified global attribute override the local one.
     BEWARE!! This is only used from `face-set-after-frame-default' where
     the local frame is defined from default specs in `face-defface-spec'
     and those should be overridden by global settings.  Hence the strange
     "global before local" priority.  */
  lvec = XVECTOR (local_lface)->contents;
  gvec = XVECTOR (global_lface)->contents;
  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (IGNORE_DEFFACE_P (gvec[i]))
      lvec[i] = Qunspecified;
    else if (! UNSPECIFIEDP (gvec[i]))
      lvec[i] = gvec[i];

  /* If the default face was changed, update the face cache and the
     `font' frame parameter.  */
  if (EQ (face, Qdefault))
    {
      struct face_cache *c = FRAME_FACE_CACHE (f);
      struct face *newface, *oldface = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      Lisp_Object attrs[LFACE_VECTOR_SIZE];

      /* This can be NULL (e.g., in batch mode).  */
      if (oldface)
	{
	  /* Ensure that the face vector is fully specified by merging
	     the previously-cached vector.  */
	  memcpy (attrs, oldface->lface, sizeof attrs);
	  merge_face_vectors (f, lvec, attrs, 0);
	  memcpy (lvec, attrs, sizeof attrs);
	  newface = realize_face (c, lvec, DEFAULT_FACE_ID);

	  if ((! UNSPECIFIEDP (gvec[LFACE_FAMILY_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_FOUNDRY_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_HEIGHT_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_WEIGHT_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_SLANT_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_SWIDTH_INDEX])
	       || ! UNSPECIFIEDP (gvec[LFACE_FONT_INDEX]))
	      && newface->font)
	    {
	      Lisp_Object name = newface->font->props[FONT_NAME_INDEX];
	      Fmodify_frame_parameters (frame, Fcons (Fcons (Qfont, name),
						      Qnil));
	    }

	  if (STRINGP (gvec[LFACE_FOREGROUND_INDEX]))
	    Fmodify_frame_parameters (frame,
				      Fcons (Fcons (Qforeground_color,
						    gvec[LFACE_FOREGROUND_INDEX]),
					     Qnil));

	  if (STRINGP (gvec[LFACE_BACKGROUND_INDEX]))
	    Fmodify_frame_parameters (frame,
				      Fcons (Fcons (Qbackground_color,
						    gvec[LFACE_BACKGROUND_INDEX]),
					     Qnil));
	}
    }

  return Qnil;
}


/* The following function is implemented for compatibility with 20.2.
   The function is used in x-resolve-fonts when it is asked to
   return fonts with the same size as the font of a face.  This is
   done in fontset.el.  */

DEFUN ("face-font", Fface_font, Sface_font, 1, 3, 0,
       doc: /* Return the font name of face FACE, or nil if it is unspecified.
The font name is, by default, for ASCII characters.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
  The font default for a face is either nil, or a list
  of the form (bold), (italic) or (bold italic).
If FRAME is omitted or nil, use the selected frame.  And, in this case,
if the optional third argument CHARACTER is given,
return the font name used for CHARACTER.  */)
  (Lisp_Object face, Lisp_Object frame, Lisp_Object character)
{
  if (EQ (frame, Qt))
    {
      Lisp_Object result = Qnil;
      Lisp_Object lface = lface_from_face_name (NULL, face, 1);

      if (!UNSPECIFIEDP (LFACE_WEIGHT (lface))
	  && !EQ (LFACE_WEIGHT (lface), Qnormal))
	result = Fcons (Qbold, result);

      if (!UNSPECIFIEDP (LFACE_SLANT (lface))
	  && !EQ (LFACE_SLANT (lface), Qnormal))
	result = Fcons (Qitalic, result);

      return result;
    }
  else
    {
      struct frame *f = frame_or_selected_frame (frame, 1);
      int face_id = lookup_named_face (f, face, 1);
      struct face *fface = FACE_FROM_ID (f, face_id);

      if (! fface)
	return Qnil;
#ifdef HAVE_WINDOW_SYSTEM
      if (FRAME_WINDOW_P (f) && !NILP (character))
	{
	  CHECK_CHARACTER (character);
	  face_id = FACE_FOR_CHAR (f, fface, XINT (character), -1, Qnil);
	  fface = FACE_FROM_ID (f, face_id);
	}
      return (fface->font
	      ? fface->font->props[FONT_NAME_INDEX]
	      : Qnil);
#else  /* !HAVE_WINDOW_SYSTEM */
      return build_string (FRAME_MSDOS_P (f)
			   ? "ms-dos"
			   : FRAME_W32_P (f) ? "w32term"
			   :"tty");
#endif
    }
}


/* Compare face-attribute values v1 and v2 for equality.  Value is non-zero if
   all attributes are `equal'.  Tries to be fast because this function
   is called quite often.  */

static inline int
face_attr_equal_p (Lisp_Object v1, Lisp_Object v2)
{
  /* Type can differ, e.g. when one attribute is unspecified, i.e. nil,
     and the other is specified.  */
  if (XTYPE (v1) != XTYPE (v2))
    return 0;

  if (EQ (v1, v2))
    return 1;

  switch (XTYPE (v1))
    {
    case Lisp_String:
      if (SBYTES (v1) != SBYTES (v2))
	return 0;

      return memcmp (SDATA (v1), SDATA (v2), SBYTES (v1)) == 0;

    case_Lisp_Int:
    case Lisp_Symbol:
      return 0;

    default:
      return !NILP (Fequal (v1, v2));
    }
}


/* Compare face vectors V1 and V2 for equality.  Value is non-zero if
   all attributes are `equal'.  Tries to be fast because this function
   is called quite often.  */

static inline int
lface_equal_p (Lisp_Object *v1, Lisp_Object *v2)
{
  int i, equal_p = 1;

  for (i = 1; i < LFACE_VECTOR_SIZE && equal_p; ++i)
    equal_p = face_attr_equal_p (v1[i], v2[i]);

  return equal_p;
}


DEFUN ("internal-lisp-face-equal-p", Finternal_lisp_face_equal_p,
       Sinternal_lisp_face_equal_p, 2, 3, 0,
       doc: /* True if FACE1 and FACE2 are equal.
If the optional argument FRAME is given, report on FACE1 and FACE2 in that frame.
If FRAME is t, report on the defaults for FACE1 and FACE2 (for new frames).
If FRAME is omitted or nil, use the selected frame.  */)
  (Lisp_Object face1, Lisp_Object face2, Lisp_Object frame)
{
  int equal_p;
  struct frame *f;
  Lisp_Object lface1, lface2;

  if (EQ (frame, Qt))
    f = NULL;
  else
    /* Don't use check_x_frame here because this function is called
       before X frames exist.  At that time, if FRAME is nil,
       selected_frame will be used which is the frame dumped with
       Emacs.  That frame is not an X frame.  */
    f = frame_or_selected_frame (frame, 2);

  lface1 = lface_from_face_name (f, face1, 1);
  lface2 = lface_from_face_name (f, face2, 1);
  equal_p = lface_equal_p (XVECTOR (lface1)->contents,
			   XVECTOR (lface2)->contents);
  return equal_p ? Qt : Qnil;
}


DEFUN ("internal-lisp-face-empty-p", Finternal_lisp_face_empty_p,
       Sinternal_lisp_face_empty_p, 1, 2, 0,
       doc: /* True if FACE has no attribute specified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame.  */)
  (Lisp_Object face, Lisp_Object frame)
{
  struct frame *f;
  Lisp_Object lface;
  int i;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  if (EQ (frame, Qt))
    lface = lface_from_face_name (NULL, face, 1);
  else
    lface = lface_from_face_name (f, face, 1);

  for (i = 1; i < LFACE_VECTOR_SIZE; ++i)
    if (!UNSPECIFIEDP (AREF (lface, i)))
      break;

  return i == LFACE_VECTOR_SIZE ? Qt : Qnil;
}


DEFUN ("frame-face-alist", Fframe_face_alist, Sframe_face_alist,
       0, 1, 0,
       doc: /* Return an alist of frame-local faces defined on FRAME.
For internal use only.  */)
  (Lisp_Object frame)
{
  struct frame *f = frame_or_selected_frame (frame, 0);
  return f->face_alist;
}


/* Return a hash code for Lisp string STRING with case ignored.  Used
   below in computing a hash value for a Lisp face.  */

static inline unsigned
hash_string_case_insensitive (Lisp_Object string)
{
  const unsigned char *s;
  unsigned hash = 0;
  xassert (STRINGP (string));
  for (s = SDATA (string); *s; ++s)
    hash = (hash << 1) ^ tolower (*s);
  return hash;
}


/* Return a hash code for face attribute vector V.  */

static inline unsigned
lface_hash (Lisp_Object *v)
{
  return (hash_string_case_insensitive (v[LFACE_FAMILY_INDEX])
	  ^ hash_string_case_insensitive (v[LFACE_FOUNDRY_INDEX])
	  ^ hash_string_case_insensitive (v[LFACE_FOREGROUND_INDEX])
	  ^ hash_string_case_insensitive (v[LFACE_BACKGROUND_INDEX])
	  ^ XHASH (v[LFACE_WEIGHT_INDEX])
	  ^ XHASH (v[LFACE_SLANT_INDEX])
	  ^ XHASH (v[LFACE_SWIDTH_INDEX])
	  ^ XHASH (v[LFACE_HEIGHT_INDEX]));
}


/* Return non-zero if LFACE1 and LFACE2 specify the same font (without
   considering charsets/registries).  They do if they specify the same
   family, point size, weight, width, slant, and font.  Both
   LFACE1 and LFACE2 must be fully-specified.  */

static inline int
lface_same_font_attributes_p (Lisp_Object *lface1, Lisp_Object *lface2)
{
  xassert (lface_fully_specified_p (lface1)
	   && lface_fully_specified_p (lface2));
  return (xstrcasecmp (SSDATA (lface1[LFACE_FAMILY_INDEX]),
		       SSDATA (lface2[LFACE_FAMILY_INDEX])) == 0
	  && xstrcasecmp (SSDATA (lface1[LFACE_FOUNDRY_INDEX]),
			  SSDATA (lface2[LFACE_FOUNDRY_INDEX])) == 0
	  && EQ (lface1[LFACE_HEIGHT_INDEX], lface2[LFACE_HEIGHT_INDEX])
	  && EQ (lface1[LFACE_SWIDTH_INDEX], lface2[LFACE_SWIDTH_INDEX])
	  && EQ (lface1[LFACE_WEIGHT_INDEX], lface2[LFACE_WEIGHT_INDEX])
	  && EQ (lface1[LFACE_SLANT_INDEX], lface2[LFACE_SLANT_INDEX])
	  && EQ (lface1[LFACE_FONT_INDEX], lface2[LFACE_FONT_INDEX])
	  && (EQ (lface1[LFACE_FONTSET_INDEX], lface2[LFACE_FONTSET_INDEX])
	      || (STRINGP (lface1[LFACE_FONTSET_INDEX])
		  && STRINGP (lface2[LFACE_FONTSET_INDEX])
		  && ! xstrcasecmp (SSDATA (lface1[LFACE_FONTSET_INDEX]),
				    SSDATA (lface2[LFACE_FONTSET_INDEX]))))
	  );
}



/***********************************************************************
			    Realized Faces
 ***********************************************************************/

/* Allocate and return a new realized face for Lisp face attribute
   vector ATTR.  */

static struct face *
make_realized_face (Lisp_Object *attr)
{
  struct face *face = (struct face *) xmalloc (sizeof *face);
  memset (face, 0, sizeof *face);
  face->ascii_face = face;
  memcpy (face->lface, attr, sizeof face->lface);
  return face;
}


/* Free realized face FACE, including its X resources.  FACE may
   be null.  */

static void
free_realized_face (struct frame *f, struct face *face)
{
  if (face)
    {
#ifdef HAVE_WINDOW_SYSTEM
      if (FRAME_WINDOW_P (f))
	{
	  /* Free fontset of FACE if it is ASCII face.  */
	  if (face->fontset >= 0 && face == face->ascii_face)
	    free_face_fontset (f, face);
	  if (face->gc)
	    {
	      BLOCK_INPUT;
	      if (face->font)
		font_done_for_face (f, face);
	      x_free_gc (f, face->gc);
	      face->gc = 0;
	      UNBLOCK_INPUT;
	    }

	  free_face_colors (f, face);
	  x_destroy_bitmap (f, face->stipple);
	}
#endif /* HAVE_WINDOW_SYSTEM */

      xfree (face);
    }
}


/* Prepare face FACE for subsequent display on frame F.  This
   allocated GCs if they haven't been allocated yet or have been freed
   by clearing the face cache.  */

void
prepare_face_for_display (struct frame *f, struct face *face)
{
#ifdef HAVE_WINDOW_SYSTEM
  xassert (FRAME_WINDOW_P (f));

  if (face->gc == 0)
    {
      XGCValues xgcv;
      unsigned long mask = GCForeground | GCBackground | GCGraphicsExposures;

      xgcv.foreground = face->foreground;
      xgcv.background = face->background;
#ifdef HAVE_X_WINDOWS
      xgcv.graphics_exposures = False;
#endif

      BLOCK_INPUT;
#ifdef HAVE_X_WINDOWS
      if (face->stipple)
	{
	  xgcv.fill_style = FillOpaqueStippled;
	  xgcv.stipple = x_bitmap_pixmap (f, face->stipple);
	  mask |= GCFillStyle | GCStipple;
	}
#endif
      face->gc = x_create_gc (f, mask, &xgcv);
      if (face->font)
	font_prepare_for_face (f, face);
      UNBLOCK_INPUT;
    }
#endif /* HAVE_WINDOW_SYSTEM */
}


/* Returns the `distance' between the colors X and Y.  */

static int
color_distance (XColor *x, XColor *y)
{
  /* This formula is from a paper titled `Colour metric' by Thiadmer Riemersma.
     Quoting from that paper:

	 This formula has results that are very close to L*u*v* (with the
	 modified lightness curve) and, more importantly, it is a more even
	 algorithm: it does not have a range of colors where it suddenly
	 gives far from optimal results.

     See <http://www.compuphase.com/cmetric.htm> for more info.  */

  long r = (x->red   - y->red)   >> 8;
  long g = (x->green - y->green) >> 8;
  long b = (x->blue  - y->blue)  >> 8;
  long r_mean = (x->red + y->red) >> 9;

  return
    (((512 + r_mean) * r * r) >> 8)
    + 4 * g * g
    + (((767 - r_mean) * b * b) >> 8);
}


DEFUN ("color-distance", Fcolor_distance, Scolor_distance, 2, 3, 0,
       doc: /* Return an integer distance between COLOR1 and COLOR2 on FRAME.
COLOR1 and COLOR2 may be either strings containing the color name,
or lists of the form (RED GREEN BLUE).
If FRAME is unspecified or nil, the current frame is used.  */)
  (Lisp_Object color1, Lisp_Object color2, Lisp_Object frame)
{
  struct frame *f;
  XColor cdef1, cdef2;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  if (!(CONSP (color1) && parse_rgb_list (color1, &cdef1))
      && !(STRINGP (color1) && defined_color (f, SSDATA (color1), &cdef1, 0)))
    signal_error ("Invalid color", color1);
  if (!(CONSP (color2) && parse_rgb_list (color2, &cdef2))
      && !(STRINGP (color2) && defined_color (f, SSDATA (color2), &cdef2, 0)))
    signal_error ("Invalid color", color2);

  return make_number (color_distance (&cdef1, &cdef2));
}


/***********************************************************************
			      Face Cache
 ***********************************************************************/

/* Return a new face cache for frame F.  */

static struct face_cache *
make_face_cache (struct frame *f)
{
  struct face_cache *c;
  int size;

  c = (struct face_cache *) xmalloc (sizeof *c);
  memset (c, 0, sizeof *c);
  size = FACE_CACHE_BUCKETS_SIZE * sizeof *c->buckets;
  c->buckets = (struct face **) xmalloc (size);
  memset (c->buckets, 0, size);
  c->size = 50;
  c->faces_by_id = (struct face **) xmalloc (c->size * sizeof *c->faces_by_id);
  c->f = f;
  c->menu_face_changed_p = menu_face_changed_default;
  return c;
}


/* Clear out all graphics contexts for all realized faces, except for
   the basic faces.  This should be done from time to time just to avoid
   keeping too many graphics contexts that are no longer needed.  */

static void
clear_face_gcs (struct face_cache *c)
{
  if (c && FRAME_WINDOW_P (c->f))
    {
#ifdef HAVE_WINDOW_SYSTEM
      int i;
      for (i = BASIC_FACE_ID_SENTINEL; i < c->used; ++i)
	{
	  struct face *face = c->faces_by_id[i];
	  if (face && face->gc)
	    {
	      BLOCK_INPUT;
	      if (face->font)
		font_done_for_face (c->f, face);
	      x_free_gc (c->f, face->gc);
	      face->gc = 0;
	      UNBLOCK_INPUT;
	    }
	}
#endif /* HAVE_WINDOW_SYSTEM */
    }
}


/* Free all realized faces in face cache C, including basic faces.
   C may be null.  If faces are freed, make sure the frame's current
   matrix is marked invalid, so that a display caused by an expose
   event doesn't try to use faces we destroyed.  */

static void
free_realized_faces (struct face_cache *c)
{
  if (c && c->used)
    {
      int i, size;
      struct frame *f = c->f;

      /* We must block input here because we can't process X events
	 safely while only some faces are freed, or when the frame's
	 current matrix still references freed faces.  */
      BLOCK_INPUT;

      for (i = 0; i < c->used; ++i)
	{
	  free_realized_face (f, c->faces_by_id[i]);
	  c->faces_by_id[i] = NULL;
	}

      c->used = 0;
      size = FACE_CACHE_BUCKETS_SIZE * sizeof *c->buckets;
      memset (c->buckets, 0, size);

      /* Must do a thorough redisplay the next time.  Mark current
	 matrices as invalid because they will reference faces freed
	 above.  This function is also called when a frame is
	 destroyed.  In this case, the root window of F is nil.  */
      if (WINDOWP (f->root_window))
	{
	  clear_current_matrices (f);
	  ++windows_or_buffers_changed;
	}

      UNBLOCK_INPUT;
    }
}


/* Free all realized faces on FRAME or on all frames if FRAME is nil.
   This is done after attributes of a named face have been changed,
   because we can't tell which realized faces depend on that face.  */

void
free_all_realized_faces (Lisp_Object frame)
{
  if (NILP (frame))
    {
      Lisp_Object rest;
      FOR_EACH_FRAME (rest, frame)
	free_realized_faces (FRAME_FACE_CACHE (XFRAME (frame)));
    }
  else
    free_realized_faces (FRAME_FACE_CACHE (XFRAME (frame)));
}


/* Free face cache C and faces in it, including their X resources.  */

static void
free_face_cache (struct face_cache *c)
{
  if (c)
    {
      free_realized_faces (c);
      xfree (c->buckets);
      xfree (c->faces_by_id);
      xfree (c);
    }
}


/* Cache realized face FACE in face cache C.  HASH is the hash value
   of FACE.  If FACE is for ASCII characters (i.e. FACE->ascii_face ==
   FACE), insert the new face to the beginning of the collision list
   of the face hash table of C.  Otherwise, add the new face to the
   end of the collision list.  This way, lookup_face can quickly find
   that a requested face is not cached.  */

static void
cache_face (struct face_cache *c, struct face *face, unsigned int hash)
{
  int i = hash % FACE_CACHE_BUCKETS_SIZE;

  face->hash = hash;

  if (face->ascii_face != face)
    {
      struct face *last = c->buckets[i];
      if (last)
	{
	  while (last->next)
	    last = last->next;
	  last->next = face;
	  face->prev = last;
	  face->next = NULL;
	}
      else
	{
	  c->buckets[i] = face;
	  face->prev = face->next = NULL;
	}
    }
  else
    {
      face->prev = NULL;
      face->next = c->buckets[i];
      if (face->next)
	face->next->prev = face;
      c->buckets[i] = face;
    }

  /* Find a free slot in C->faces_by_id and use the index of the free
     slot as FACE->id.  */
  for (i = 0; i < c->used; ++i)
    if (c->faces_by_id[i] == NULL)
      break;
  face->id = i;

#if GLYPH_DEBUG
  /* Check that FACE got a unique id.  */
  {
    int j, n;
    struct face *face1;

    for (j = n = 0; j < FACE_CACHE_BUCKETS_SIZE; ++j)
      for (face1 = c->buckets[j]; face1; face1 = face1->next)
	if (face1->id == i)
	  ++n;

    xassert (n == 1);
  }
#endif /* GLYPH_DEBUG */

  /* Maybe enlarge C->faces_by_id.  */
  if (i == c->used)
    {
      if (c->used == c->size)
	c->faces_by_id = xpalloc (c->faces_by_id, &c->size, 1, MAX_FACE_ID,
				  sizeof *c->faces_by_id);
      c->used++;
    }

  c->faces_by_id[i] = face;
}


/* Remove face FACE from cache C.  */

static void
uncache_face (struct face_cache *c, struct face *face)
{
  int i = face->hash % FACE_CACHE_BUCKETS_SIZE;

  if (face->prev)
    face->prev->next = face->next;
  else
    c->buckets[i] = face->next;

  if (face->next)
    face->next->prev = face->prev;

  c->faces_by_id[face->id] = NULL;
  if (face->id == c->used)
    --c->used;
}


/* Look up a realized face with face attributes ATTR in the face cache
   of frame F.  The face will be used to display ASCII characters.
   Value is the ID of the face found.  If no suitable face is found,
   realize a new one.  */

static inline int
lookup_face (struct frame *f, Lisp_Object *attr)
{
  struct face_cache *cache = FRAME_FACE_CACHE (f);
  unsigned hash;
  int i;
  struct face *face;

  xassert (cache != NULL);
  check_lface_attrs (attr);

  /* Look up ATTR in the face cache.  */
  hash = lface_hash (attr);
  i = hash % FACE_CACHE_BUCKETS_SIZE;

  for (face = cache->buckets[i]; face; face = face->next)
    {
      if (face->ascii_face != face)
	{
	  /* There's no more ASCII face.  */
	  face = NULL;
	  break;
	}
      if (face->hash == hash
	  && lface_equal_p (face->lface, attr))
	break;
    }

  /* If not found, realize a new face.  */
  if (face == NULL)
    face = realize_face (cache, attr, -1);

#if GLYPH_DEBUG
  xassert (face == FACE_FROM_ID (f, face->id));
#endif /* GLYPH_DEBUG */

  return face->id;
}

#ifdef HAVE_WINDOW_SYSTEM
/* Look up a realized face that has the same attributes as BASE_FACE
   except for the font in the face cache of frame F.  If FONT-OBJECT
   is not nil, it is an already opened font.  If FONT-OBJECT is nil,
   the face has no font.  Value is the ID of the face found.  If no
   suitable face is found, realize a new one.  */

int
face_for_font (struct frame *f, Lisp_Object font_object, struct face *base_face)
{
  struct face_cache *cache = FRAME_FACE_CACHE (f);
  unsigned hash;
  int i;
  struct face *face;

  xassert (cache != NULL);
  base_face = base_face->ascii_face;
  hash = lface_hash (base_face->lface);
  i = hash % FACE_CACHE_BUCKETS_SIZE;

  for (face = cache->buckets[i]; face; face = face->next)
    {
      if (face->ascii_face == face)
	continue;
      if (face->ascii_face == base_face
	  && face->font == (NILP (font_object) ? NULL
			    : XFONT_OBJECT (font_object))
	  && lface_equal_p (face->lface, base_face->lface))
	return face->id;
    }

  /* If not found, realize a new face.  */
  face = realize_non_ascii_face (f, font_object, base_face);
  return face->id;
}
#endif	/* HAVE_WINDOW_SYSTEM */

/* Return the face id of the realized face for named face SYMBOL on
   frame F suitable for displaying ASCII characters.  Value is -1 if
   the face couldn't be determined, which might happen if the default
   face isn't realized and cannot be realized.  */

int
lookup_named_face (struct frame *f, Lisp_Object symbol, int signal_p)
{
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];
  struct face *default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);

  if (default_face == NULL)
    {
      if (!realize_basic_faces (f))
	return -1;
      default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      if (default_face == NULL)
	abort ();  /* realize_basic_faces must have set it up  */
    }

  if (! get_lface_attributes (f, symbol, symbol_attrs, signal_p, 0))
    return -1;

  memcpy (attrs, default_face->lface, sizeof attrs);
  merge_face_vectors (f, symbol_attrs, attrs, 0);

  return lookup_face (f, attrs);
}


/* Return the display face-id of the basic face who's canonical face-id
   is FACE_ID.  The return value will usually simply be FACE_ID, unless that
   basic face has bee remapped via Vface_remapping_alist.  This function is
   conservative: if something goes wrong, it will simply return FACE_ID
   rather than signal an error.   */

int
lookup_basic_face (struct frame *f, int face_id)
{
  Lisp_Object name, mapping;
  int remapped_face_id;

  if (NILP (Vface_remapping_alist))
    return face_id;		/* Nothing to do.  */

  switch (face_id)
    {
    case DEFAULT_FACE_ID:		name = Qdefault;		break;
    case MODE_LINE_FACE_ID:		name = Qmode_line;		break;
    case MODE_LINE_INACTIVE_FACE_ID:	name = Qmode_line_inactive;	break;
    case HEADER_LINE_FACE_ID:		name = Qheader_line;		break;
    case TOOL_BAR_FACE_ID:		name = Qtool_bar;		break;
    case FRINGE_FACE_ID:		name = Qfringe;			break;
    case SCROLL_BAR_FACE_ID:		name = Qscroll_bar;		break;
    case BORDER_FACE_ID:		name = Qborder;			break;
    case CURSOR_FACE_ID:		name = Qcursor;			break;
    case MOUSE_FACE_ID:			name = Qmouse;			break;
    case MENU_FACE_ID:			name = Qmenu;			break;

    default:
      abort ();	    /* the caller is supposed to pass us a basic face id */
    }

  /* Do a quick scan through Vface_remapping_alist, and return immediately
     if there is no remapping for face NAME.  This is just an optimization
     for the very common no-remapping case.  */
  mapping = assq_no_quit (name, Vface_remapping_alist);
  if (NILP (mapping))
    return face_id;		/* Give up.  */

  /* If there is a remapping entry, lookup the face using NAME, which will
     handle the remapping too.  */
  remapped_face_id = lookup_named_face (f, name, 0);
  if (remapped_face_id < 0)
    return face_id;		/* Give up. */

  return remapped_face_id;
}


/* Return a face for charset ASCII that is like the face with id
   FACE_ID on frame F, but has a font that is STEPS steps smaller.
   STEPS < 0 means larger.  Value is the id of the face.  */

int
smaller_face (struct frame *f, int face_id, int steps)
{
#ifdef HAVE_WINDOW_SYSTEM
  struct face *face;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  int pt, last_pt, last_height;
  int delta;
  int new_face_id;
  struct face *new_face;

  /* If not called for an X frame, just return the original face.  */
  if (FRAME_TERMCAP_P (f))
    return face_id;

  /* Try in increments of 1/2 pt.  */
  delta = steps < 0 ? 5 : -5;
  steps = eabs (steps);

  face = FACE_FROM_ID (f, face_id);
  memcpy (attrs, face->lface, sizeof attrs);
  pt = last_pt = XFASTINT (attrs[LFACE_HEIGHT_INDEX]);
  new_face_id = face_id;
  last_height = FONT_HEIGHT (face->font);

  while (steps
	 && pt + delta > 0
	 /* Give up if we cannot find a font within 10pt.  */
	 && eabs (last_pt - pt) < 100)
    {
      /* Look up a face for a slightly smaller/larger font.  */
      pt += delta;
      attrs[LFACE_HEIGHT_INDEX] = make_number (pt);
      new_face_id = lookup_face (f, attrs);
      new_face = FACE_FROM_ID (f, new_face_id);

      /* If height changes, count that as one step.  */
      if ((delta < 0 && FONT_HEIGHT (new_face->font) < last_height)
	  || (delta > 0 && FONT_HEIGHT (new_face->font) > last_height))
	{
	  --steps;
	  last_height = FONT_HEIGHT (new_face->font);
	  last_pt = pt;
	}
    }

  return new_face_id;

#else /* not HAVE_WINDOW_SYSTEM */

  return face_id;

#endif /* not HAVE_WINDOW_SYSTEM */
}


/* Return a face for charset ASCII that is like the face with id
   FACE_ID on frame F, but has height HEIGHT.  */

int
face_with_height (struct frame *f, int face_id, int height)
{
#ifdef HAVE_WINDOW_SYSTEM
  struct face *face;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];

  if (FRAME_TERMCAP_P (f)
      || height <= 0)
    return face_id;

  face = FACE_FROM_ID (f, face_id);
  memcpy (attrs, face->lface, sizeof attrs);
  attrs[LFACE_HEIGHT_INDEX] = make_number (height);
  font_clear_prop (attrs, FONT_SIZE_INDEX);
  face_id = lookup_face (f, attrs);
#endif /* HAVE_WINDOW_SYSTEM */

  return face_id;
}


/* Return the face id of the realized face for named face SYMBOL on
   frame F suitable for displaying ASCII characters, and use
   attributes of the face FACE_ID for attributes that aren't
   completely specified by SYMBOL.  This is like lookup_named_face,
   except that the default attributes come from FACE_ID, not from the
   default face.  FACE_ID is assumed to be already realized.  */

int
lookup_derived_face (struct frame *f, Lisp_Object symbol, int face_id,
		     int signal_p)
{
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];
  struct face *default_face = FACE_FROM_ID (f, face_id);

  if (!default_face)
    abort ();

  if (!get_lface_attributes (f, symbol, symbol_attrs, signal_p, 0))
    return -1;

  memcpy (attrs, default_face->lface, sizeof attrs);
  merge_face_vectors (f, symbol_attrs, attrs, 0);
  return lookup_face (f, attrs);
}

DEFUN ("face-attributes-as-vector", Fface_attributes_as_vector,
       Sface_attributes_as_vector, 1, 1, 0,
       doc: /* Return a vector of face attributes corresponding to PLIST.  */)
  (Lisp_Object plist)
{
  Lisp_Object lface;
  lface = Fmake_vector (make_number (LFACE_VECTOR_SIZE),
			Qunspecified);
  merge_face_ref (XFRAME (selected_frame), plist, XVECTOR (lface)->contents,
		  1, 0);
  return lface;
}



/***********************************************************************
			Face capability testing
 ***********************************************************************/


/* If the distance (as returned by color_distance) between two colors is
   less than this, then they are considered the same, for determining
   whether a color is supported or not.  The range of values is 0-65535.  */

#define TTY_SAME_COLOR_THRESHOLD  10000

#ifdef HAVE_WINDOW_SYSTEM

/* Return non-zero if all the face attributes in ATTRS are supported
   on the window-system frame F.

   The definition of `supported' is somewhat heuristic, but basically means
   that a face containing all the attributes in ATTRS, when merged with the
   default face for display, can be represented in a way that's

    \(1) different in appearance than the default face, and
    \(2) `close in spirit' to what the attributes specify, if not exact.  */

static int
x_supports_face_attributes_p (struct frame *f, Lisp_Object *attrs,
			      struct face *def_face)
{
  Lisp_Object *def_attrs = def_face->lface;

  /* Check that other specified attributes are different that the default
     face.  */
  if ((!UNSPECIFIEDP (attrs[LFACE_UNDERLINE_INDEX])
       && face_attr_equal_p (attrs[LFACE_UNDERLINE_INDEX],
			     def_attrs[LFACE_UNDERLINE_INDEX]))
      || (!UNSPECIFIEDP (attrs[LFACE_INVERSE_INDEX])
	  && face_attr_equal_p (attrs[LFACE_INVERSE_INDEX],
				def_attrs[LFACE_INVERSE_INDEX]))
      || (!UNSPECIFIEDP (attrs[LFACE_FOREGROUND_INDEX])
	  && face_attr_equal_p (attrs[LFACE_FOREGROUND_INDEX],
				def_attrs[LFACE_FOREGROUND_INDEX]))
      || (!UNSPECIFIEDP (attrs[LFACE_BACKGROUND_INDEX])
	  && face_attr_equal_p (attrs[LFACE_BACKGROUND_INDEX],
				def_attrs[LFACE_BACKGROUND_INDEX]))
      || (!UNSPECIFIEDP (attrs[LFACE_STIPPLE_INDEX])
	  && face_attr_equal_p (attrs[LFACE_STIPPLE_INDEX],
				def_attrs[LFACE_STIPPLE_INDEX]))
      || (!UNSPECIFIEDP (attrs[LFACE_OVERLINE_INDEX])
	  && face_attr_equal_p (attrs[LFACE_OVERLINE_INDEX],
				def_attrs[LFACE_OVERLINE_INDEX]))
      || (!UNSPECIFIEDP (attrs[LFACE_STRIKE_THROUGH_INDEX])
	  && face_attr_equal_p (attrs[LFACE_STRIKE_THROUGH_INDEX],
				def_attrs[LFACE_STRIKE_THROUGH_INDEX]))
      || (!UNSPECIFIEDP (attrs[LFACE_BOX_INDEX])
	  && face_attr_equal_p (attrs[LFACE_BOX_INDEX],
				def_attrs[LFACE_BOX_INDEX])))
    return 0;

  /* Check font-related attributes, as those are the most commonly
     "unsupported" on a window-system (because of missing fonts).  */
  if (!UNSPECIFIEDP (attrs[LFACE_FAMILY_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_FOUNDRY_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_HEIGHT_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_WEIGHT_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_SLANT_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_SWIDTH_INDEX]))
    {
      int face_id;
      struct face *face;
      Lisp_Object merged_attrs[LFACE_VECTOR_SIZE];
      int i;

      memcpy (merged_attrs, def_attrs, sizeof merged_attrs);

      merge_face_vectors (f, attrs, merged_attrs, 0);

      face_id = lookup_face (f, merged_attrs);
      face = FACE_FROM_ID (f, face_id);

      if (! face)
	error ("Cannot make face");

      /* If the font is the same, or no font is found, then not
	 supported.  */
      if (face->font == def_face->font
	  || ! face->font)
	return 0;
      for (i = FONT_TYPE_INDEX; i <= FONT_SIZE_INDEX; i++)
	if (! EQ (face->font->props[i], def_face->font->props[i]))
	  {
	    Lisp_Object s1, s2;

	    if (i < FONT_FOUNDRY_INDEX || i > FONT_REGISTRY_INDEX
		|| face->font->driver->case_sensitive)
	      return 1;
	    s1 = SYMBOL_NAME (face->font->props[i]);
	    s2 = SYMBOL_NAME (def_face->font->props[i]);
	    if (! EQ (Fcompare_strings (s1, make_number (0), Qnil,
					s2, make_number (0), Qnil, Qt), Qt))
	      return 1;
	  }
      return 0;
    }

  /* Everything checks out, this face is supported.  */
  return 1;
}

#endif	/* HAVE_WINDOW_SYSTEM */

/* Return non-zero if all the face attributes in ATTRS are supported
   on the tty frame F.

   The definition of `supported' is somewhat heuristic, but basically means
   that a face containing all the attributes in ATTRS, when merged
   with the default face for display, can be represented in a way that's

    \(1) different in appearance than the default face, and
    \(2) `close in spirit' to what the attributes specify, if not exact.

   Point (2) implies that a `:weight black' attribute will be satisfied
   by any terminal that can display bold, and a `:foreground "yellow"' as
   long as the terminal can display a yellowish color, but `:slant italic'
   will _not_ be satisfied by the tty display code's automatic
   substitution of a `dim' face for italic.  */

static int
tty_supports_face_attributes_p (struct frame *f, Lisp_Object *attrs,
				struct face *def_face)
{
  int weight;
  Lisp_Object val, fg, bg;
  XColor fg_tty_color, fg_std_color;
  XColor bg_tty_color, bg_std_color;
  unsigned test_caps = 0;
  Lisp_Object *def_attrs = def_face->lface;


  /* First check some easy-to-check stuff; ttys support none of the
     following attributes, so we can just return false if any are requested
     (even if `nominal' values are specified, we should still return false,
     as that will be the same value that the default face uses).  We
     consider :slant unsupportable on ttys, even though the face code
     actually `fakes' them using a dim attribute if possible.  This is
     because the faked result is too different from what the face
     specifies.  */
  if (!UNSPECIFIEDP (attrs[LFACE_FAMILY_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_FOUNDRY_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_STIPPLE_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_HEIGHT_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_SWIDTH_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_OVERLINE_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_STRIKE_THROUGH_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_BOX_INDEX])
      || !UNSPECIFIEDP (attrs[LFACE_SLANT_INDEX]))
    return 0;


  /* Test for terminal `capabilities' (non-color character attributes).  */

  /* font weight (bold/dim) */
  val = attrs[LFACE_WEIGHT_INDEX];
  if (!UNSPECIFIEDP (val)
      && (weight = FONT_WEIGHT_NAME_NUMERIC (val), weight >= 0))
    {
      int def_weight = FONT_WEIGHT_NAME_NUMERIC (def_attrs[LFACE_WEIGHT_INDEX]);

      if (weight > 100)
	{
	  if (def_weight > 100)
	    return 0;		/* same as default */
	  test_caps = TTY_CAP_BOLD;
	}
      else if (weight < 100)
	{
	  if (def_weight < 100)
	    return 0;		/* same as default */
	  test_caps = TTY_CAP_DIM;
	}
      else if (def_weight == 100)
	return 0;		/* same as default */
    }

  /* underlining */
  val = attrs[LFACE_UNDERLINE_INDEX];
  if (!UNSPECIFIEDP (val))
    {
      if (STRINGP (val))
	return 0;		/* ttys can't use colored underlines */
      else if (face_attr_equal_p (val, def_attrs[LFACE_UNDERLINE_INDEX]))
	return 0;		/* same as default */
      else
	test_caps |= TTY_CAP_UNDERLINE;
    }

  /* inverse video */
  val = attrs[LFACE_INVERSE_INDEX];
  if (!UNSPECIFIEDP (val))
    {
      if (face_attr_equal_p (val, def_attrs[LFACE_INVERSE_INDEX]))
	return 0;		/* same as default */
      else
	test_caps |= TTY_CAP_INVERSE;
    }


  /* Color testing.  */

  /* Default the color indices in FG_TTY_COLOR and BG_TTY_COLOR, since
     we use them when calling `tty_capable_p' below, even if the face
     specifies no colors.  */
  fg_tty_color.pixel = FACE_TTY_DEFAULT_FG_COLOR;
  bg_tty_color.pixel = FACE_TTY_DEFAULT_BG_COLOR;

  /* Check if foreground color is close enough.  */
  fg = attrs[LFACE_FOREGROUND_INDEX];
  if (STRINGP (fg))
    {
      Lisp_Object def_fg = def_attrs[LFACE_FOREGROUND_INDEX];

      if (face_attr_equal_p (fg, def_fg))
	return 0;		/* same as default */
      else if (! tty_lookup_color (f, fg, &fg_tty_color, &fg_std_color))
	return 0;		/* not a valid color */
      else if (color_distance (&fg_tty_color, &fg_std_color)
	       > TTY_SAME_COLOR_THRESHOLD)
	return 0;		/* displayed color is too different */
      else
	/* Make sure the color is really different than the default.  */
	{
	  XColor def_fg_color;
	  if (tty_lookup_color (f, def_fg, &def_fg_color, 0)
	      && (color_distance (&fg_tty_color, &def_fg_color)
		  <= TTY_SAME_COLOR_THRESHOLD))
	    return 0;
	}
    }

  /* Check if background color is close enough.  */
  bg = attrs[LFACE_BACKGROUND_INDEX];
  if (STRINGP (bg))
    {
      Lisp_Object def_bg = def_attrs[LFACE_BACKGROUND_INDEX];

      if (face_attr_equal_p (bg, def_bg))
	return 0;		/* same as default */
      else if (! tty_lookup_color (f, bg, &bg_tty_color, &bg_std_color))
	return 0;		/* not a valid color */
      else if (color_distance (&bg_tty_color, &bg_std_color)
	       > TTY_SAME_COLOR_THRESHOLD)
	return 0;		/* displayed color is too different */
      else
	/* Make sure the color is really different than the default.  */
	{
	  XColor def_bg_color;
	  if (tty_lookup_color (f, def_bg, &def_bg_color, 0)
	      && (color_distance (&bg_tty_color, &def_bg_color)
		  <= TTY_SAME_COLOR_THRESHOLD))
	    return 0;
	}
    }

  /* If both foreground and background are requested, see if the
     distance between them is OK.  We just check to see if the distance
     between the tty's foreground and background is close enough to the
     distance between the standard foreground and background.  */
  if (STRINGP (fg) && STRINGP (bg))
    {
      int delta_delta
	= (color_distance (&fg_std_color, &bg_std_color)
	   - color_distance (&fg_tty_color, &bg_tty_color));
      if (delta_delta > TTY_SAME_COLOR_THRESHOLD
	  || delta_delta < -TTY_SAME_COLOR_THRESHOLD)
	return 0;
    }


  /* See if the capabilities we selected above are supported, with the
     given colors.  */
  if (test_caps != 0 &&
      ! tty_capable_p (FRAME_TTY (f), test_caps, fg_tty_color.pixel,
		       bg_tty_color.pixel))
    return 0;


  /* Hmmm, everything checks out, this terminal must support this face.  */
  return 1;
}


DEFUN ("display-supports-face-attributes-p",
       Fdisplay_supports_face_attributes_p, Sdisplay_supports_face_attributes_p,
       1, 2, 0,
       doc: /* Return non-nil if all the face attributes in ATTRIBUTES are supported.
The optional argument DISPLAY can be a display name, a frame, or
nil (meaning the selected frame's display).

The definition of `supported' is somewhat heuristic, but basically means
that a face containing all the attributes in ATTRIBUTES, when merged
with the default face for display, can be represented in a way that's

 \(1) different in appearance than the default face, and
 \(2) `close in spirit' to what the attributes specify, if not exact.

Point (2) implies that a `:weight black' attribute will be satisfied by
any display that can display bold, and a `:foreground \"yellow\"' as long
as it can display a yellowish color, but `:slant italic' will _not_ be
satisfied by the tty display code's automatic substitution of a `dim'
face for italic.  */)
  (Lisp_Object attributes, Lisp_Object display)
{
  int supports = 0, i;
  Lisp_Object frame;
  struct frame *f;
  struct face *def_face;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];

  if (noninteractive || !initialized)
    /* We may not be able to access low-level face information in batch
       mode, or before being dumped, and this function is not going to
       be very useful in those cases anyway, so just give up.  */
    return Qnil;

  if (NILP (display))
    frame = selected_frame;
  else if (FRAMEP (display))
    frame = display;
  else
    {
      /* Find any frame on DISPLAY.  */
      Lisp_Object fl_tail;

      frame = Qnil;
      for (fl_tail = Vframe_list; CONSP (fl_tail); fl_tail = XCDR (fl_tail))
	{
	  frame = XCAR (fl_tail);
	  if (!NILP (Fequal (Fcdr (Fassq (Qdisplay,
					  XFRAME (frame)->param_alist)),
			     display)))
	    break;
	}
    }

  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  for (i = 0; i < LFACE_VECTOR_SIZE; i++)
    attrs[i] = Qunspecified;
  merge_face_ref (f, attributes, attrs, 1, 0);

  def_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
  if (def_face == NULL)
    {
      if (! realize_basic_faces (f))
	error ("Cannot realize default face");
      def_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      if (def_face == NULL)
	abort ();  /* realize_basic_faces must have set it up  */
    }

  /* Dispatch to the appropriate handler.  */
  if (FRAME_TERMCAP_P (f) || FRAME_MSDOS_P (f))
    supports = tty_supports_face_attributes_p (f, attrs, def_face);
#ifdef HAVE_WINDOW_SYSTEM
  else
    supports = x_supports_face_attributes_p (f, attrs, def_face);
#endif

  return supports ? Qt : Qnil;
}


/***********************************************************************
			    Font selection
 ***********************************************************************/

DEFUN ("internal-set-font-selection-order",
       Finternal_set_font_selection_order,
       Sinternal_set_font_selection_order, 1, 1, 0,
       doc: /* Set font selection order for face font selection to ORDER.
ORDER must be a list of length 4 containing the symbols `:width',
`:height', `:weight', and `:slant'.  Face attributes appearing
first in ORDER are matched first, e.g. if `:height' appears before
`:weight' in ORDER, font selection first tries to find a font with
a suitable height, and then tries to match the font weight.
Value is ORDER.  */)
  (Lisp_Object order)
{
  Lisp_Object list;
  int i;
  int indices[DIM (font_sort_order)];

  CHECK_LIST (order);
  memset (indices, 0, sizeof indices);
  i = 0;

  for (list = order;
       CONSP (list) && i < DIM (indices);
       list = XCDR (list), ++i)
    {
      Lisp_Object attr = XCAR (list);
      int xlfd;

      if (EQ (attr, QCwidth))
	xlfd = XLFD_SWIDTH;
      else if (EQ (attr, QCheight))
	xlfd = XLFD_POINT_SIZE;
      else if (EQ (attr, QCweight))
	xlfd = XLFD_WEIGHT;
      else if (EQ (attr, QCslant))
	xlfd = XLFD_SLANT;
      else
	break;

      if (indices[i] != 0)
	break;
      indices[i] = xlfd;
    }

  if (!NILP (list) || i != DIM (indices))
    signal_error ("Invalid font sort order", order);
  for (i = 0; i < DIM (font_sort_order); ++i)
    if (indices[i] == 0)
      signal_error ("Invalid font sort order", order);

  if (memcmp (indices, font_sort_order, sizeof indices) != 0)
    {
      memcpy (font_sort_order, indices, sizeof font_sort_order);
      free_all_realized_faces (Qnil);
    }

  font_update_sort_order (font_sort_order);

  return Qnil;
}


DEFUN ("internal-set-alternative-font-family-alist",
       Finternal_set_alternative_font_family_alist,
       Sinternal_set_alternative_font_family_alist, 1, 1, 0,
       doc: /* Define alternative font families to try in face font selection.
ALIST is an alist of (FAMILY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
Each ALTERNATIVE is tried in order if no fonts of font family FAMILY can
be found.  Value is ALIST.  */)
  (Lisp_Object alist)
{
  Lisp_Object entry, tail, tail2;

  CHECK_LIST (alist);
  alist = Fcopy_sequence (alist);
  for (tail = alist; CONSP (tail); tail = XCDR (tail))
    {
      entry = XCAR (tail);
      CHECK_LIST (entry);
      entry = Fcopy_sequence (entry);
      XSETCAR (tail, entry);
      for (tail2 = entry; CONSP (tail2); tail2 = XCDR (tail2))
	XSETCAR (tail2, Fintern (XCAR (tail2), Qnil));
    }

  Vface_alternative_font_family_alist = alist;
  free_all_realized_faces (Qnil);
  return alist;
}


DEFUN ("internal-set-alternative-font-registry-alist",
       Finternal_set_alternative_font_registry_alist,
       Sinternal_set_alternative_font_registry_alist, 1, 1, 0,
       doc: /* Define alternative font registries to try in face font selection.
ALIST is an alist of (REGISTRY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
Each ALTERNATIVE is tried in order if no fonts of font registry REGISTRY can
be found.  Value is ALIST.  */)
  (Lisp_Object alist)
{
  Lisp_Object entry, tail, tail2;

  CHECK_LIST (alist);
  alist = Fcopy_sequence (alist);
  for (tail = alist; CONSP (tail); tail = XCDR (tail))
    {
      entry = XCAR (tail);
      CHECK_LIST (entry);
      entry = Fcopy_sequence (entry);
      XSETCAR (tail, entry);
      for (tail2 = entry; CONSP (tail2); tail2 = XCDR (tail2))
	XSETCAR (tail2, Fdowncase (XCAR (tail2)));
    }
  Vface_alternative_font_registry_alist = alist;
  free_all_realized_faces (Qnil);
  return alist;
}


#ifdef HAVE_WINDOW_SYSTEM

/* Return the fontset id of the base fontset name or alias name given
   by the fontset attribute of ATTRS.  Value is -1 if the fontset
   attribute of ATTRS doesn't name a fontset.  */

static int
face_fontset (Lisp_Object *attrs)
{
  Lisp_Object name;

  name = attrs[LFACE_FONTSET_INDEX];
  if (!STRINGP (name))
    return -1;
  return fs_query_fontset (name, 0);
}

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
			   Face Realization
 ***********************************************************************/

/* Realize basic faces on frame F.  Value is zero if frame parameters
   of F don't contain enough information needed to realize the default
   face.  */

static int
realize_basic_faces (struct frame *f)
{
  int success_p = 0;
  int count = SPECPDL_INDEX ();

  /* Block input here so that we won't be surprised by an X expose
     event, for instance, without having the faces set up.  */
  BLOCK_INPUT;
  specbind (Qscalable_fonts_allowed, Qt);

  if (realize_default_face (f))
    {
      realize_named_face (f, Qmode_line, MODE_LINE_FACE_ID);
      realize_named_face (f, Qmode_line_inactive, MODE_LINE_INACTIVE_FACE_ID);
      realize_named_face (f, Qtool_bar, TOOL_BAR_FACE_ID);
      realize_named_face (f, Qfringe, FRINGE_FACE_ID);
      realize_named_face (f, Qheader_line, HEADER_LINE_FACE_ID);
      realize_named_face (f, Qscroll_bar, SCROLL_BAR_FACE_ID);
      realize_named_face (f, Qborder, BORDER_FACE_ID);
      realize_named_face (f, Qcursor, CURSOR_FACE_ID);
      realize_named_face (f, Qmouse, MOUSE_FACE_ID);
      realize_named_face (f, Qmenu, MENU_FACE_ID);
      realize_named_face (f, Qvertical_border, VERTICAL_BORDER_FACE_ID);

      /* Reflect changes in the `menu' face in menu bars.  */
      if (FRAME_FACE_CACHE (f)->menu_face_changed_p)
	{
	  FRAME_FACE_CACHE (f)->menu_face_changed_p = 0;
#ifdef USE_X_TOOLKIT
	  if (FRAME_WINDOW_P (f))
	    x_update_menu_appearance (f);
#endif
	}

      success_p = 1;
    }

  unbind_to (count, Qnil);
  UNBLOCK_INPUT;
  return success_p;
}


/* Realize the default face on frame F.  If the face is not fully
   specified, make it fully-specified.  Attributes of the default face
   that are not explicitly specified are taken from frame parameters.  */

static int
realize_default_face (struct frame *f)
{
  struct face_cache *c = FRAME_FACE_CACHE (f);
  Lisp_Object lface;
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  struct face *face;

  /* If the `default' face is not yet known, create it.  */
  lface = lface_from_face_name (f, Qdefault, 0);
  if (NILP (lface))
  {
       Lisp_Object frame;
       XSETFRAME (frame, f);
       lface = Finternal_make_lisp_face (Qdefault, frame);
  }

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      Lisp_Object font_object;

      XSETFONT (font_object, FRAME_FONT (f));
      set_lface_from_font (f, lface, font_object, f->default_face_done_p);
      LFACE_FONTSET (lface) = fontset_name (FRAME_FONTSET (f));
      f->default_face_done_p = 1;
    }
#endif /* HAVE_WINDOW_SYSTEM */

  if (!FRAME_WINDOW_P (f))
    {
      LFACE_FAMILY (lface) = build_string ("default");
      LFACE_FOUNDRY (lface) = LFACE_FAMILY (lface);
      LFACE_SWIDTH (lface) = Qnormal;
      LFACE_HEIGHT (lface) = make_number (1);
      if (UNSPECIFIEDP (LFACE_WEIGHT (lface)))
	LFACE_WEIGHT (lface) = Qnormal;
      if (UNSPECIFIEDP (LFACE_SLANT (lface)))
	LFACE_SLANT (lface) = Qnormal;
      if (UNSPECIFIEDP (LFACE_FONTSET (lface)))
	LFACE_FONTSET (lface) = Qnil;
    }

  if (UNSPECIFIEDP (LFACE_UNDERLINE (lface)))
    LFACE_UNDERLINE (lface) = Qnil;

  if (UNSPECIFIEDP (LFACE_OVERLINE (lface)))
    LFACE_OVERLINE (lface) = Qnil;

  if (UNSPECIFIEDP (LFACE_STRIKE_THROUGH (lface)))
    LFACE_STRIKE_THROUGH (lface) = Qnil;

  if (UNSPECIFIEDP (LFACE_BOX (lface)))
    LFACE_BOX (lface) = Qnil;

  if (UNSPECIFIEDP (LFACE_INVERSE (lface)))
    LFACE_INVERSE (lface) = Qnil;

  if (UNSPECIFIEDP (LFACE_FOREGROUND (lface)))
    {
      /* This function is called so early that colors are not yet
	 set in the frame parameter list.  */
      Lisp_Object color = Fassq (Qforeground_color, f->param_alist);

      if (CONSP (color) && STRINGP (XCDR (color)))
	LFACE_FOREGROUND (lface) = XCDR (color);
      else if (FRAME_WINDOW_P (f))
	return 0;
      else if (FRAME_INITIAL_P (f) || FRAME_TERMCAP_P (f) || FRAME_MSDOS_P (f))
	LFACE_FOREGROUND (lface) = build_string (unspecified_fg);
      else
	abort ();
    }

  if (UNSPECIFIEDP (LFACE_BACKGROUND (lface)))
    {
      /* This function is called so early that colors are not yet
	 set in the frame parameter list.  */
      Lisp_Object color = Fassq (Qbackground_color, f->param_alist);
      if (CONSP (color) && STRINGP (XCDR (color)))
	LFACE_BACKGROUND (lface) = XCDR (color);
      else if (FRAME_WINDOW_P (f))
	return 0;
      else if (FRAME_INITIAL_P (f) || FRAME_TERMCAP_P (f) || FRAME_MSDOS_P (f))
	LFACE_BACKGROUND (lface) = build_string (unspecified_bg);
      else
	abort ();
    }

  if (UNSPECIFIEDP (LFACE_STIPPLE (lface)))
    LFACE_STIPPLE (lface) = Qnil;

  /* Realize the face; it must be fully-specified now.  */
  xassert (lface_fully_specified_p (XVECTOR (lface)->contents));
  check_lface (lface);
  memcpy (attrs, XVECTOR (lface)->contents, sizeof attrs);
  face = realize_face (c, attrs, DEFAULT_FACE_ID);

#ifdef HAVE_WINDOW_SYSTEM
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f) && face->font != FRAME_FONT (f))
    {
      /* This can happen when making a frame on a display that does
	 not support the default font.  */
      if (!face->font)
	return 0;

      /* Otherwise, the font specified for the frame was not
	 acceptable as a font for the default face (perhaps because
	 auto-scaled fonts are rejected), so we must adjust the frame
	 font.  */
      x_set_font (f, LFACE_FONT (lface), Qnil);
    }
#endif	/* HAVE_X_WINDOWS */
#endif	/* HAVE_WINDOW_SYSTEM */
  return 1;
}


/* Realize basic faces other than the default face in face cache C.
   SYMBOL is the face name, ID is the face id the realized face must
   have.  The default face must have been realized already.  */

static void
realize_named_face (struct frame *f, Lisp_Object symbol, int id)
{
  struct face_cache *c = FRAME_FACE_CACHE (f);
  Lisp_Object lface = lface_from_face_name (f, symbol, 0);
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object symbol_attrs[LFACE_VECTOR_SIZE];

  /* The default face must exist and be fully specified.  */
  get_lface_attributes_no_remap (f, Qdefault, attrs, 1);
  check_lface_attrs (attrs);
  xassert (lface_fully_specified_p (attrs));

  /* If SYMBOL isn't know as a face, create it.  */
  if (NILP (lface))
    {
      Lisp_Object frame;
      XSETFRAME (frame, f);
      lface = Finternal_make_lisp_face (symbol, frame);
    }

  /* Merge SYMBOL's face with the default face.  */
  get_lface_attributes_no_remap (f, symbol, symbol_attrs, 1);
  merge_face_vectors (f, symbol_attrs, attrs, 0);

  /* Realize the face.  */
  realize_face (c, attrs, id);
}


/* Realize the fully-specified face with attributes ATTRS in face
   cache CACHE for ASCII characters.  If FORMER_FACE_ID is
   non-negative, it is an ID of face to remove before caching the new
   face.  Value is a pointer to the newly created realized face.  */

static struct face *
realize_face (struct face_cache *cache, Lisp_Object *attrs, int former_face_id)
{
  struct face *face;

  /* LFACE must be fully specified.  */
  xassert (cache != NULL);
  check_lface_attrs (attrs);

  if (former_face_id >= 0 && cache->used > former_face_id)
    {
      /* Remove the former face.  */
      struct face *former_face = cache->faces_by_id[former_face_id];
      uncache_face (cache, former_face);
      free_realized_face (cache->f, former_face);
      SET_FRAME_GARBAGED (cache->f);
    }

  if (FRAME_WINDOW_P (cache->f))
    face = realize_x_face (cache, attrs);
  else if (FRAME_TERMCAP_P (cache->f) || FRAME_MSDOS_P (cache->f))
    face = realize_tty_face (cache, attrs);
  else if (FRAME_INITIAL_P (cache->f))
    {
      /* Create a dummy face. */
      face = make_realized_face (attrs);
    }
  else
    abort ();

  /* Insert the new face.  */
  cache_face (cache, face, lface_hash (attrs));
  return face;
}


#ifdef HAVE_WINDOW_SYSTEM
/* Realize the fully-specified face that uses FONT-OBJECT and has the
   same attributes as BASE_FACE except for the font on frame F.
   FONT-OBJECT may be nil, in which case, realized a face of
   no-font.  */

static struct face *
realize_non_ascii_face (struct frame *f, Lisp_Object font_object,
			struct face *base_face)
{
  struct face_cache *cache = FRAME_FACE_CACHE (f);
  struct face *face;

  face = (struct face *) xmalloc (sizeof *face);
  *face = *base_face;
  face->gc = 0;
  face->extra = NULL;
  face->overstrike
    = (! NILP (font_object)
       && FONT_WEIGHT_NAME_NUMERIC (face->lface[LFACE_WEIGHT_INDEX]) > 100
       && FONT_WEIGHT_NUMERIC (font_object) <= 100);

  /* Don't try to free the colors copied bitwise from BASE_FACE.  */
  face->colors_copied_bitwise_p = 1;
  face->font = NILP (font_object) ? NULL : XFONT_OBJECT (font_object);
  face->gc = 0;

  cache_face (cache, face, face->hash);

  return face;
}
#endif	/* HAVE_WINDOW_SYSTEM */


/* Realize the fully-specified face with attributes ATTRS in face
   cache CACHE for ASCII characters.  Do it for X frame CACHE->f.  If
   the new face doesn't share font with the default face, a fontname
   is allocated from the heap and set in `font_name' of the new face,
   but it is not yet loaded here.  Value is a pointer to the newly
   created realized face.  */

static struct face *
realize_x_face (struct face_cache *cache, Lisp_Object *attrs)
{
  struct face *face = NULL;
#ifdef HAVE_WINDOW_SYSTEM
  struct face *default_face;
  struct frame *f;
  Lisp_Object stipple, overline, strike_through, box;

  xassert (FRAME_WINDOW_P (cache->f));

  /* Allocate a new realized face.  */
  face = make_realized_face (attrs);
  face->ascii_face = face;

  f = cache->f;

  /* Determine the font to use.  Most of the time, the font will be
     the same as the font of the default face, so try that first.  */
  default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
  if (default_face
      && lface_same_font_attributes_p (default_face->lface, attrs))
    {
      face->font = default_face->font;
      face->fontset
	= make_fontset_for_ascii_face (f, default_face->fontset, face);
    }
  else
    {
      /* If the face attribute ATTRS specifies a fontset, use it as
	 the base of a new realized fontset.  Otherwise, use the same
	 base fontset as of the default face.  The base determines
	 registry and encoding of a font.  It may also determine
	 foundry and family.  The other fields of font name pattern
	 are constructed from ATTRS.  */
      int fontset = face_fontset (attrs);

      /* If we are realizing the default face, ATTRS should specify a
	 fontset.  In other words, if FONTSET is -1, we are not
	 realizing the default face, thus the default face should have
	 already been realized.  */
      if (fontset == -1)
	{
	  if (default_face)
	    fontset = default_face->fontset;
	  if (fontset == -1)
	    abort ();
	}
      if (! FONT_OBJECT_P (attrs[LFACE_FONT_INDEX]))
	attrs[LFACE_FONT_INDEX]
	  = font_load_for_lface (f, attrs, attrs[LFACE_FONT_INDEX]);
      if (FONT_OBJECT_P (attrs[LFACE_FONT_INDEX]))
	{
	  face->font = XFONT_OBJECT (attrs[LFACE_FONT_INDEX]);
	  face->fontset = make_fontset_for_ascii_face (f, fontset, face);
	}
      else
	{
	  face->font = NULL;
	  face->fontset = -1;
	}
    }

  if (face->font
      && FONT_WEIGHT_NAME_NUMERIC (attrs[LFACE_WEIGHT_INDEX]) > 100
      && FONT_WEIGHT_NUMERIC (attrs[LFACE_FONT_INDEX]) <= 100)
    face->overstrike = 1;

  /* Load colors, and set remaining attributes.  */

  load_face_colors (f, face, attrs);

  /* Set up box.  */
  box = attrs[LFACE_BOX_INDEX];
  if (STRINGP (box))
    {
      /* A simple box of line width 1 drawn in color given by
	 the string.  */
      face->box_color = load_color (f, face, attrs[LFACE_BOX_INDEX],
				    LFACE_BOX_INDEX);
      face->box = FACE_SIMPLE_BOX;
      face->box_line_width = 1;
    }
  else if (INTEGERP (box))
    {
      /* Simple box of specified line width in foreground color of the
	 face.  */
      xassert (XINT (box) != 0);
      face->box = FACE_SIMPLE_BOX;
      face->box_line_width = XINT (box);
      face->box_color = face->foreground;
      face->box_color_defaulted_p = 1;
    }
  else if (CONSP (box))
    {
      /* `(:width WIDTH :color COLOR :shadow SHADOW)'.  SHADOW
	 being one of `raised' or `sunken'.  */
      face->box = FACE_SIMPLE_BOX;
      face->box_color = face->foreground;
      face->box_color_defaulted_p = 1;
      face->box_line_width = 1;

      while (CONSP (box))
	{
	  Lisp_Object keyword, value;

	  keyword = XCAR (box);
	  box = XCDR (box);

	  if (!CONSP (box))
	    break;
	  value = XCAR (box);
	  box = XCDR (box);

	  if (EQ (keyword, QCline_width))
	    {
	      if (INTEGERP (value) && XINT (value) != 0)
		face->box_line_width = XINT (value);
	    }
	  else if (EQ (keyword, QCcolor))
	    {
	      if (STRINGP (value))
		{
		  face->box_color = load_color (f, face, value,
						LFACE_BOX_INDEX);
		  face->use_box_color_for_shadows_p = 1;
		}
	    }
	  else if (EQ (keyword, QCstyle))
	    {
	      if (EQ (value, Qreleased_button))
		face->box = FACE_RAISED_BOX;
	      else if (EQ (value, Qpressed_button))
		face->box = FACE_SUNKEN_BOX;
	    }
	}
    }

  /* Text underline, overline, strike-through.  */

  if (EQ (attrs[LFACE_UNDERLINE_INDEX], Qt))
    {
      /* Use default color (same as foreground color).  */
      face->underline_p = 1;
      face->underline_defaulted_p = 1;
      face->underline_color = 0;
    }
  else if (STRINGP (attrs[LFACE_UNDERLINE_INDEX]))
    {
      /* Use specified color.  */
      face->underline_p = 1;
      face->underline_defaulted_p = 0;
      face->underline_color
	= load_color (f, face, attrs[LFACE_UNDERLINE_INDEX],
		      LFACE_UNDERLINE_INDEX);
    }
  else if (NILP (attrs[LFACE_UNDERLINE_INDEX]))
    {
      face->underline_p = 0;
      face->underline_defaulted_p = 0;
      face->underline_color = 0;
    }

  overline = attrs[LFACE_OVERLINE_INDEX];
  if (STRINGP (overline))
    {
      face->overline_color
	= load_color (f, face, attrs[LFACE_OVERLINE_INDEX],
		      LFACE_OVERLINE_INDEX);
      face->overline_p = 1;
    }
  else if (EQ (overline, Qt))
    {
      face->overline_color = face->foreground;
      face->overline_color_defaulted_p = 1;
      face->overline_p = 1;
    }

  strike_through = attrs[LFACE_STRIKE_THROUGH_INDEX];
  if (STRINGP (strike_through))
    {
      face->strike_through_color
	= load_color (f, face, attrs[LFACE_STRIKE_THROUGH_INDEX],
		      LFACE_STRIKE_THROUGH_INDEX);
      face->strike_through_p = 1;
    }
  else if (EQ (strike_through, Qt))
    {
      face->strike_through_color = face->foreground;
      face->strike_through_color_defaulted_p = 1;
      face->strike_through_p = 1;
    }

  stipple = attrs[LFACE_STIPPLE_INDEX];
  if (!NILP (stipple))
    face->stipple = load_pixmap (f, stipple, &face->pixmap_w, &face->pixmap_h);
#endif /* HAVE_WINDOW_SYSTEM */

  return face;
}


/* Map a specified color of face FACE on frame F to a tty color index.
   IDX is either LFACE_FOREGROUND_INDEX or LFACE_BACKGROUND_INDEX, and
   specifies which color to map.  Set *DEFAULTED to 1 if mapping to the
   default foreground/background colors.  */

static void
map_tty_color (struct frame *f, struct face *face,
	       enum lface_attribute_index idx, int *defaulted)
{
  Lisp_Object frame, color, def;
  int foreground_p = idx == LFACE_FOREGROUND_INDEX;
  unsigned long default_pixel =
    foreground_p ? FACE_TTY_DEFAULT_FG_COLOR : FACE_TTY_DEFAULT_BG_COLOR;
  unsigned long pixel = default_pixel;
#ifdef MSDOS
  unsigned long default_other_pixel =
    foreground_p ? FACE_TTY_DEFAULT_BG_COLOR : FACE_TTY_DEFAULT_FG_COLOR;
#endif

  xassert (idx == LFACE_FOREGROUND_INDEX || idx == LFACE_BACKGROUND_INDEX);

  XSETFRAME (frame, f);
  color = face->lface[idx];

  if (STRINGP (color)
      && SCHARS (color)
      && CONSP (Vtty_defined_color_alist)
      && (def = assq_no_quit (color, call1 (Qtty_color_alist, frame)),
	  CONSP (def)))
    {
      /* Associations in tty-defined-color-alist are of the form
	 (NAME INDEX R G B).  We need the INDEX part.  */
      pixel = XINT (XCAR (XCDR (def)));
    }

  if (pixel == default_pixel && STRINGP (color))
    {
      pixel = load_color (f, face, color, idx);

#ifdef MSDOS
      /* If the foreground of the default face is the default color,
	 use the foreground color defined by the frame.  */
      if (FRAME_MSDOS_P (f))
	{
	  if (pixel == default_pixel
	      || pixel == FACE_TTY_DEFAULT_COLOR)
	    {
	      if (foreground_p)
		pixel = FRAME_FOREGROUND_PIXEL (f);
	      else
		pixel = FRAME_BACKGROUND_PIXEL (f);
	      face->lface[idx] = tty_color_name (f, pixel);
	      *defaulted = 1;
	    }
	  else if (pixel == default_other_pixel)
	    {
	      if (foreground_p)
		pixel = FRAME_BACKGROUND_PIXEL (f);
	      else
		pixel = FRAME_FOREGROUND_PIXEL (f);
	      face->lface[idx] = tty_color_name (f, pixel);
	      *defaulted = 1;
	    }
	}
#endif /* MSDOS */
    }

  if (foreground_p)
    face->foreground = pixel;
  else
    face->background = pixel;
}


/* Realize the fully-specified face with attributes ATTRS in face
   cache CACHE for ASCII characters.  Do it for TTY frame CACHE->f.
   Value is a pointer to the newly created realized face.  */

static struct face *
realize_tty_face (struct face_cache *cache, Lisp_Object *attrs)
{
  struct face *face;
  int weight, slant;
  int face_colors_defaulted = 0;
  struct frame *f = cache->f;

  /* Frame must be a termcap frame.  */
  xassert (FRAME_TERMCAP_P (cache->f) || FRAME_MSDOS_P (cache->f));

  /* Allocate a new realized face.  */
  face = make_realized_face (attrs);
#if 0
  face->font_name = FRAME_MSDOS_P (cache->f) ? "ms-dos" : "tty";
#endif

  /* Map face attributes to TTY appearances.  We map slant to
     dimmed text because we want italic text to appear differently
     and because dimmed text is probably used infrequently.  */
  weight = FONT_WEIGHT_NAME_NUMERIC (attrs[LFACE_WEIGHT_INDEX]);
  slant = FONT_SLANT_NAME_NUMERIC (attrs[LFACE_SLANT_INDEX]);
  if (weight > 100)
    face->tty_bold_p = 1;
  if (weight < 100 || slant != 100)
    face->tty_dim_p = 1;
  if (!NILP (attrs[LFACE_UNDERLINE_INDEX]))
    face->tty_underline_p = 1;
  if (!NILP (attrs[LFACE_INVERSE_INDEX]))
    face->tty_reverse_p = 1;

  /* Map color names to color indices.  */
  map_tty_color (f, face, LFACE_FOREGROUND_INDEX, &face_colors_defaulted);
  map_tty_color (f, face, LFACE_BACKGROUND_INDEX, &face_colors_defaulted);

  /* Swap colors if face is inverse-video.  If the colors are taken
     from the frame colors, they are already inverted, since the
     frame-creation function calls x-handle-reverse-video.  */
  if (face->tty_reverse_p && !face_colors_defaulted)
    {
      unsigned long tem = face->foreground;
      face->foreground = face->background;
      face->background = tem;
    }

  if (tty_suppress_bold_inverse_default_colors_p
      && face->tty_bold_p
      && face->background == FACE_TTY_DEFAULT_FG_COLOR
      && face->foreground == FACE_TTY_DEFAULT_BG_COLOR)
    face->tty_bold_p = 0;

  return face;
}


DEFUN ("tty-suppress-bold-inverse-default-colors",
       Ftty_suppress_bold_inverse_default_colors,
       Stty_suppress_bold_inverse_default_colors, 1, 1, 0,
       doc: /* Suppress/allow boldness of faces with inverse default colors.
SUPPRESS non-nil means suppress it.
This affects bold faces on TTYs whose foreground is the default background
color of the display and whose background is the default foreground color.
For such faces, the bold face attribute is ignored if this variable
is non-nil.  */)
  (Lisp_Object suppress)
{
  tty_suppress_bold_inverse_default_colors_p = !NILP (suppress);
  ++face_change_count;
  return suppress;
}



/***********************************************************************
			   Computing Faces
 ***********************************************************************/

/* Return the ID of the face to use to display character CH with face
   property PROP on frame F in current_buffer.  */

int
compute_char_face (struct frame *f, int ch, Lisp_Object prop)
{
  int face_id;

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    ch = 0;

  if (NILP (prop))
    {
      struct face *face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      face_id = FACE_FOR_CHAR (f, face, ch, -1, Qnil);
    }
  else
    {
      Lisp_Object attrs[LFACE_VECTOR_SIZE];
      struct face *default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);
      memcpy (attrs, default_face->lface, sizeof attrs);
      merge_face_ref (f, prop, attrs, 1, 0);
      face_id = lookup_face (f, attrs);
    }

  return face_id;
}

/* Return the face ID associated with buffer position POS for
   displaying ASCII characters.  Return in *ENDPTR the position at
   which a different face is needed, as far as text properties and
   overlays are concerned.  W is a window displaying current_buffer.

   REGION_BEG, REGION_END delimit the region, so it can be
   highlighted.

   LIMIT is a position not to scan beyond.  That is to limit the time
   this function can take.

   If MOUSE is non-zero, use the character's mouse-face, not its face.

   BASE_FACE_ID, if non-negative, specifies a base face id to use
   instead of DEFAULT_FACE_ID.

   The face returned is suitable for displaying ASCII characters.  */

int
face_at_buffer_position (struct window *w, EMACS_INT pos,
			 EMACS_INT region_beg, EMACS_INT region_end,
			 EMACS_INT *endptr, EMACS_INT limit,
			 int mouse, int base_face_id)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object prop, position;
  ptrdiff_t i, noverlays;
  Lisp_Object *overlay_vec;
  Lisp_Object frame;
  EMACS_INT endpos;
  Lisp_Object propname = mouse ? Qmouse_face : Qface;
  Lisp_Object limit1, end;
  struct face *default_face;

  /* W must display the current buffer.  We could write this function
     to use the frame and buffer of W, but right now it doesn't.  */
  /* xassert (XBUFFER (w->buffer) == current_buffer); */

  XSETFRAME (frame, f);
  XSETFASTINT (position, pos);

  endpos = ZV;
  if (pos < region_beg && region_beg < endpos)
    endpos = region_beg;

  /* Get the `face' or `mouse_face' text property at POS, and
     determine the next position at which the property changes.  */
  prop = Fget_text_property (position, propname, w->buffer);
  XSETFASTINT (limit1, (limit < endpos ? limit : endpos));
  end = Fnext_single_property_change (position, propname, w->buffer, limit1);
  if (INTEGERP (end))
    endpos = XINT (end);

  /* Look at properties from overlays.  */
  {
    EMACS_INT next_overlay;

    GET_OVERLAYS_AT (pos, overlay_vec, noverlays, &next_overlay, 0);
    if (next_overlay < endpos)
      endpos = next_overlay;
  }

  *endptr = endpos;

  {
    int face_id;

    if (base_face_id >= 0)
      face_id = base_face_id;
    else if (NILP (Vface_remapping_alist))
      face_id = DEFAULT_FACE_ID;
    else
      face_id = lookup_basic_face (f, DEFAULT_FACE_ID);

    default_face = FACE_FROM_ID (f, face_id);
  }

  /* Optimize common cases where we can use the default face.  */
  if (noverlays == 0
      && NILP (prop)
      && !(pos >= region_beg && pos < region_end))
    return default_face->id;

  /* Begin with attributes from the default face.  */
  memcpy (attrs, default_face->lface, sizeof attrs);

  /* Merge in attributes specified via text properties.  */
  if (!NILP (prop))
    merge_face_ref (f, prop, attrs, 1, 0);

  /* Now merge the overlay data.  */
  noverlays = sort_overlays (overlay_vec, noverlays, w);
  for (i = 0; i < noverlays; i++)
    {
      Lisp_Object oend;
      int oendpos;

      prop = Foverlay_get (overlay_vec[i], propname);
      if (!NILP (prop))
	merge_face_ref (f, prop, attrs, 1, 0);

      oend = OVERLAY_END (overlay_vec[i]);
      oendpos = OVERLAY_POSITION (oend);
      if (oendpos < endpos)
	endpos = oendpos;
    }

  /* If in the region, merge in the region face.  */
  if (pos >= region_beg && pos < region_end)
    {
      merge_named_face (f, Qregion, attrs, 0);

      if (region_end < endpos)
	endpos = region_end;
    }

  *endptr = endpos;

  /* Look up a realized face with the given face attributes,
     or realize a new one for ASCII characters.  */
  return lookup_face (f, attrs);
}

/* Return the face ID at buffer position POS for displaying ASCII
   characters associated with overlay strings for overlay OVERLAY.

   Like face_at_buffer_position except for OVERLAY.  Currently it
   simply disregards the `face' properties of all overlays.  */

int
face_for_overlay_string (struct window *w, EMACS_INT pos,
			 EMACS_INT region_beg, EMACS_INT region_end,
			 EMACS_INT *endptr, EMACS_INT limit,
			 int mouse, Lisp_Object overlay)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  Lisp_Object prop, position;
  Lisp_Object frame;
  int endpos;
  Lisp_Object propname = mouse ? Qmouse_face : Qface;
  Lisp_Object limit1, end;
  struct face *default_face;

  /* W must display the current buffer.  We could write this function
     to use the frame and buffer of W, but right now it doesn't.  */
  /* xassert (XBUFFER (w->buffer) == current_buffer); */

  XSETFRAME (frame, f);
  XSETFASTINT (position, pos);

  endpos = ZV;
  if (pos < region_beg && region_beg < endpos)
    endpos = region_beg;

  /* Get the `face' or `mouse_face' text property at POS, and
     determine the next position at which the property changes.  */
  prop = Fget_text_property (position, propname, w->buffer);
  XSETFASTINT (limit1, (limit < endpos ? limit : endpos));
  end = Fnext_single_property_change (position, propname, w->buffer, limit1);
  if (INTEGERP (end))
    endpos = XINT (end);

  *endptr = endpos;

  default_face = FACE_FROM_ID (f, DEFAULT_FACE_ID);

  /* Optimize common cases where we can use the default face.  */
  if (NILP (prop)
      && !(pos >= region_beg && pos < region_end))
    return DEFAULT_FACE_ID;

  /* Begin with attributes from the default face.  */
  memcpy (attrs, default_face->lface, sizeof attrs);

  /* Merge in attributes specified via text properties.  */
  if (!NILP (prop))
    merge_face_ref (f, prop, attrs, 1, 0);

  /* If in the region, merge in the region face.  */
  if (pos >= region_beg && pos < region_end)
    {
      merge_named_face (f, Qregion, attrs, 0);

      if (region_end < endpos)
	endpos = region_end;
    }

  *endptr = endpos;

  /* Look up a realized face with the given face attributes,
     or realize a new one for ASCII characters.  */
  return lookup_face (f, attrs);
}


/* Compute the face at character position POS in Lisp string STRING on
   window W, for ASCII characters.

   If STRING is an overlay string, it comes from position BUFPOS in
   current_buffer, otherwise BUFPOS is zero to indicate that STRING is
   not an overlay string.  W must display the current buffer.
   REGION_BEG and REGION_END give the start and end positions of the
   region; both are -1 if no region is visible.

   BASE_FACE_ID is the id of a face to merge with.  For strings coming
   from overlays or the `display' property it is the face at BUFPOS.

   If MOUSE_P is non-zero, use the character's mouse-face, not its face.

   Set *ENDPTR to the next position where to check for faces in
   STRING; -1 if the face is constant from POS to the end of the
   string.

   Value is the id of the face to use.  The face returned is suitable
   for displaying ASCII characters.  */

int
face_at_string_position (struct window *w, Lisp_Object string,
			 EMACS_INT pos, EMACS_INT bufpos,
			 EMACS_INT region_beg, EMACS_INT region_end,
			 EMACS_INT *endptr, enum face_id base_face_id,
			 int mouse_p)
{
  Lisp_Object prop, position, end, limit;
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  struct face *base_face;
  int multibyte_p = STRING_MULTIBYTE (string);
  Lisp_Object prop_name = mouse_p ? Qmouse_face : Qface;

  /* Get the value of the face property at the current position within
     STRING.  Value is nil if there is no face property.  */
  XSETFASTINT (position, pos);
  prop = Fget_text_property (position, prop_name, string);

  /* Get the next position at which to check for faces.  Value of end
     is nil if face is constant all the way to the end of the string.
     Otherwise it is a string position where to check faces next.
     Limit is the maximum position up to which to check for property
     changes in Fnext_single_property_change.  Strings are usually
     short, so set the limit to the end of the string.  */
  XSETFASTINT (limit, SCHARS (string));
  end = Fnext_single_property_change (position, prop_name, string, limit);
  if (INTEGERP (end))
    *endptr = XFASTINT (end);
  else
    *endptr = -1;

  base_face = FACE_FROM_ID (f, base_face_id);
  xassert (base_face);

  /* Optimize the default case that there is no face property and we
     are not in the region.  */
  if (NILP (prop)
      && (base_face_id != DEFAULT_FACE_ID
	  /* BUFPOS <= 0 means STRING is not an overlay string, so
	     that the region doesn't have to be taken into account.  */
	  || bufpos <= 0
	  || bufpos < region_beg
	  || bufpos >= region_end)
      && (multibyte_p
	  /* We can't realize faces for different charsets differently
	     if we don't have fonts, so we can stop here if not working
	     on a window-system frame.  */
	  || !FRAME_WINDOW_P (f)
	  || FACE_SUITABLE_FOR_ASCII_CHAR_P (base_face, 0)))
    return base_face->id;

  /* Begin with attributes from the base face.  */
  memcpy (attrs, base_face->lface, sizeof attrs);

  /* Merge in attributes specified via text properties.  */
  if (!NILP (prop))
    merge_face_ref (f, prop, attrs, 1, 0);

  /* If in the region, merge in the region face.  */
  if (bufpos
      && bufpos >= region_beg
      && bufpos < region_end)
    merge_named_face (f, Qregion, attrs, 0);

  /* Look up a realized face with the given face attributes,
     or realize a new one for ASCII characters.  */
  return lookup_face (f, attrs);
}


/* Merge a face into a realized face.

   F is frame where faces are (to be) realized.

   FACE_NAME is named face to merge.

   If FACE_NAME is nil, FACE_ID is face_id of realized face to merge.

   If FACE_NAME is t, FACE_ID is lface_id of face to merge.

   BASE_FACE_ID is realized face to merge into.

   Return new face id.
*/

int
merge_faces (struct frame *f, Lisp_Object face_name, EMACS_INT face_id,
	     int base_face_id)
{
  Lisp_Object attrs[LFACE_VECTOR_SIZE];
  struct face *base_face;

  base_face = FACE_FROM_ID (f, base_face_id);
  if (!base_face)
    return base_face_id;

  if (EQ (face_name, Qt))
    {
      if (face_id < 0 || face_id >= lface_id_to_name_size)
	return base_face_id;
      face_name = lface_id_to_name[face_id];
      /* When called during make-frame, lookup_derived_face may fail
	 if the faces are uninitialized.  Don't signal an error.  */
      face_id = lookup_derived_face (f, face_name, base_face_id, 0);
      return (face_id >= 0 ? face_id : base_face_id);
    }

  /* Begin with attributes from the base face.  */
  memcpy (attrs, base_face->lface, sizeof attrs);

  if (!NILP (face_name))
    {
      if (!merge_named_face (f, face_name, attrs, 0))
	return base_face_id;
    }
  else
    {
      struct face *face;
      if (face_id < 0)
	return base_face_id;
      face = FACE_FROM_ID (f, face_id);
      if (!face)
	return base_face_id;
      merge_face_vectors (f, face->lface, attrs, 0);
    }

  /* Look up a realized face with the given face attributes,
     or realize a new one for ASCII characters.  */
  return lookup_face (f, attrs);
}



#ifndef HAVE_X_WINDOWS
DEFUN ("x-load-color-file", Fx_load_color_file,
       Sx_load_color_file, 1, 1, 0,
       doc: /* Create an alist of color entries from an external file.

The file should define one named RGB color per line like so:
  R G B   name
where R,G,B are numbers between 0 and 255 and name is an arbitrary string.  */)
  (Lisp_Object filename)
{
  FILE *fp;
  Lisp_Object cmap = Qnil;
  Lisp_Object abspath;

  CHECK_STRING (filename);
  abspath = Fexpand_file_name (filename, Qnil);

  fp = fopen (SDATA (abspath), "rt");
  if (fp)
    {
      char buf[512];
      int red, green, blue;
      int num;

      BLOCK_INPUT;

      while (fgets (buf, sizeof (buf), fp) != NULL) {
	if (sscanf (buf, "%u %u %u %n", &red, &green, &blue, &num) == 3)
	  {
	    char *name = buf + num;
	    num = strlen (name) - 1;
	    if (num >= 0 && name[num] == '\n')
	      name[num] = 0;
	    cmap = Fcons (Fcons (build_string (name),
#ifdef WINDOWSNT
				 make_number (RGB (red, green, blue))),
#else
				 make_number ((red << 16) | (green << 8) | blue)),
#endif
			  cmap);
	  }
      }
      fclose (fp);

      UNBLOCK_INPUT;
    }

  return cmap;
}
#endif


/***********************************************************************
				Tests
 ***********************************************************************/

#if GLYPH_DEBUG

/* Print the contents of the realized face FACE to stderr.  */

static void
dump_realized_face (struct face *face)
{
  fprintf (stderr, "ID: %d\n", face->id);
#ifdef HAVE_X_WINDOWS
  fprintf (stderr, "gc: %ld\n", (long) face->gc);
#endif
  fprintf (stderr, "foreground: 0x%lx (%s)\n",
	   face->foreground,
	   SDATA (face->lface[LFACE_FOREGROUND_INDEX]));
  fprintf (stderr, "background: 0x%lx (%s)\n",
	   face->background,
	   SDATA (face->lface[LFACE_BACKGROUND_INDEX]));
  if (face->font)
    fprintf (stderr, "font_name: %s (%s)\n",
	     SDATA (face->font->props[FONT_NAME_INDEX]),
	     SDATA (face->lface[LFACE_FAMILY_INDEX]));
#ifdef HAVE_X_WINDOWS
  fprintf (stderr, "font = %p\n", face->font);
#endif
  fprintf (stderr, "fontset: %d\n", face->fontset);
  fprintf (stderr, "underline: %d (%s)\n",
	   face->underline_p,
	   SDATA (Fsymbol_name (face->lface[LFACE_UNDERLINE_INDEX])));
  fprintf (stderr, "hash: %d\n", face->hash);
}


DEFUN ("dump-face", Fdump_face, Sdump_face, 0, 1, 0, doc: /* */)
  (Lisp_Object n)
{
  if (NILP (n))
    {
      int i;

      fprintf (stderr, "font selection order: ");
      for (i = 0; i < DIM (font_sort_order); ++i)
	fprintf (stderr, "%d ", font_sort_order[i]);
      fprintf (stderr, "\n");

      fprintf (stderr, "alternative fonts: ");
      debug_print (Vface_alternative_font_family_alist);
      fprintf (stderr, "\n");

      for (i = 0; i < FRAME_FACE_CACHE (SELECTED_FRAME ())->used; ++i)
	Fdump_face (make_number (i));
    }
  else
    {
      struct face *face;
      CHECK_NUMBER (n);
      face = FACE_FROM_ID (SELECTED_FRAME (), XINT (n));
      if (face == NULL)
	error ("Not a valid face");
      dump_realized_face (face);
    }

  return Qnil;
}


DEFUN ("show-face-resources", Fshow_face_resources, Sshow_face_resources,
       0, 0, 0, doc: /* */)
  (void)
{
  fprintf (stderr, "number of colors = %d\n", ncolors_allocated);
  fprintf (stderr, "number of pixmaps = %d\n", npixmaps_allocated);
  fprintf (stderr, "number of GCs = %d\n", ngcs);
  return Qnil;
}

#endif /* GLYPH_DEBUG != 0 */



/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_xfaces (void)
{
  DEFSYM (Qface, "face");
  DEFSYM (Qface_no_inherit, "face-no-inherit");
  DEFSYM (Qbitmap_spec_p, "bitmap-spec-p");
  DEFSYM (Qframe_set_background_mode, "frame-set-background-mode");

  /* Lisp face attribute keywords.  */
  DEFSYM (QCfamily, ":family");
  DEFSYM (QCheight, ":height");
  DEFSYM (QCweight, ":weight");
  DEFSYM (QCslant, ":slant");
  DEFSYM (QCunderline, ":underline");
  DEFSYM (QCinverse_video, ":inverse-video");
  DEFSYM (QCreverse_video, ":reverse-video");
  DEFSYM (QCforeground, ":foreground");
  DEFSYM (QCbackground, ":background");
  DEFSYM (QCstipple, ":stipple");
  DEFSYM (QCwidth, ":width");
  DEFSYM (QCfont, ":font");
  DEFSYM (QCfontset, ":fontset");
  DEFSYM (QCbold, ":bold");
  DEFSYM (QCitalic, ":italic");
  DEFSYM (QCoverline, ":overline");
  DEFSYM (QCstrike_through, ":strike-through");
  DEFSYM (QCbox, ":box");
  DEFSYM (QCinherit, ":inherit");

  /* Symbols used for Lisp face attribute values.  */
  DEFSYM (QCcolor, ":color");
  DEFSYM (QCline_width, ":line-width");
  DEFSYM (QCstyle, ":style");
  DEFSYM (Qreleased_button, "released-button");
  DEFSYM (Qpressed_button, "pressed-button");
  DEFSYM (Qnormal, "normal");
  DEFSYM (Qultra_light, "ultra-light");
  DEFSYM (Qextra_light, "extra-light");
  DEFSYM (Qlight, "light");
  DEFSYM (Qsemi_light, "semi-light");
  DEFSYM (Qsemi_bold, "semi-bold");
  DEFSYM (Qbold, "bold");
  DEFSYM (Qextra_bold, "extra-bold");
  DEFSYM (Qultra_bold, "ultra-bold");
  DEFSYM (Qoblique, "oblique");
  DEFSYM (Qitalic, "italic");
  DEFSYM (Qreverse_oblique, "reverse-oblique");
  DEFSYM (Qreverse_italic, "reverse-italic");
  DEFSYM (Qultra_condensed, "ultra-condensed");
  DEFSYM (Qextra_condensed, "extra-condensed");
  DEFSYM (Qcondensed, "condensed");
  DEFSYM (Qsemi_condensed, "semi-condensed");
  DEFSYM (Qsemi_expanded, "semi-expanded");
  DEFSYM (Qexpanded, "expanded");
  DEFSYM (Qextra_expanded, "extra-expanded");
  DEFSYM (Qultra_expanded, "ultra-expanded");
  DEFSYM (Qbackground_color, "background-color");
  DEFSYM (Qforeground_color, "foreground-color");
  DEFSYM (Qunspecified, "unspecified");
  DEFSYM (QCignore_defface, ":ignore-defface");

  DEFSYM (Qface_alias, "face-alias");
  DEFSYM (Qdefault, "default");
  DEFSYM (Qtool_bar, "tool-bar");
  DEFSYM (Qregion, "region");
  DEFSYM (Qfringe, "fringe");
  DEFSYM (Qheader_line, "header-line");
  DEFSYM (Qscroll_bar, "scroll-bar");
  DEFSYM (Qmenu, "menu");
  DEFSYM (Qcursor, "cursor");
  DEFSYM (Qborder, "border");
  DEFSYM (Qmouse, "mouse");
  DEFSYM (Qmode_line_inactive, "mode-line-inactive");
  DEFSYM (Qvertical_border, "vertical-border");
  DEFSYM (Qtty_color_desc, "tty-color-desc");
  DEFSYM (Qtty_color_standard_values, "tty-color-standard-values");
  DEFSYM (Qtty_color_by_index, "tty-color-by-index");
  DEFSYM (Qtty_color_alist, "tty-color-alist");
  DEFSYM (Qscalable_fonts_allowed, "scalable-fonts-allowed");

  Vparam_value_alist = Fcons (Fcons (Qnil, Qnil), Qnil);
  staticpro (&Vparam_value_alist);
  Vface_alternative_font_family_alist = Qnil;
  staticpro (&Vface_alternative_font_family_alist);
  Vface_alternative_font_registry_alist = Qnil;
  staticpro (&Vface_alternative_font_registry_alist);

  defsubr (&Sinternal_make_lisp_face);
  defsubr (&Sinternal_lisp_face_p);
  defsubr (&Sinternal_set_lisp_face_attribute);
#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Sinternal_set_lisp_face_attribute_from_resource);
#endif
  defsubr (&Scolor_gray_p);
  defsubr (&Scolor_supported_p);
#ifndef HAVE_X_WINDOWS
  defsubr (&Sx_load_color_file);
#endif
  defsubr (&Sface_attribute_relative_p);
  defsubr (&Smerge_face_attribute);
  defsubr (&Sinternal_get_lisp_face_attribute);
  defsubr (&Sinternal_lisp_face_attribute_values);
  defsubr (&Sinternal_lisp_face_equal_p);
  defsubr (&Sinternal_lisp_face_empty_p);
  defsubr (&Sinternal_copy_lisp_face);
  defsubr (&Sinternal_merge_in_global_face);
  defsubr (&Sface_font);
  defsubr (&Sframe_face_alist);
  defsubr (&Sdisplay_supports_face_attributes_p);
  defsubr (&Scolor_distance);
  defsubr (&Sinternal_set_font_selection_order);
  defsubr (&Sinternal_set_alternative_font_family_alist);
  defsubr (&Sinternal_set_alternative_font_registry_alist);
  defsubr (&Sface_attributes_as_vector);
#if GLYPH_DEBUG
  defsubr (&Sdump_face);
  defsubr (&Sshow_face_resources);
#endif /* GLYPH_DEBUG */
  defsubr (&Sclear_face_cache);
  defsubr (&Stty_suppress_bold_inverse_default_colors);

#if defined DEBUG_X_COLORS && defined HAVE_X_WINDOWS
  defsubr (&Sdump_colors);
#endif

  DEFVAR_LISP ("font-list-limit", Vfont_list_limit,
	       doc: /* *Limit for font matching.
If an integer > 0, font matching functions won't load more than
that number of fonts when searching for a matching font.  */);
  Vfont_list_limit = make_number (DEFAULT_FONT_LIST_LIMIT);

  DEFVAR_LISP ("face-new-frame-defaults", Vface_new_frame_defaults,
    doc: /* List of global face definitions (for internal use only.)  */);
  Vface_new_frame_defaults = Qnil;

  DEFVAR_LISP ("face-default-stipple", Vface_default_stipple,
    doc: /* *Default stipple pattern used on monochrome displays.
This stipple pattern is used on monochrome displays
instead of shades of gray for a face background color.
See `set-face-stipple' for possible values for this variable.  */);
  Vface_default_stipple = make_pure_c_string ("gray3");

  DEFVAR_LISP ("tty-defined-color-alist", Vtty_defined_color_alist,
   doc: /* An alist of defined terminal colors and their RGB values.
See the docstring of `tty-color-alist' for the details.  */);
  Vtty_defined_color_alist = Qnil;

  DEFVAR_LISP ("scalable-fonts-allowed", Vscalable_fonts_allowed,
	       doc: /* Allowed scalable fonts.
A value of nil means don't allow any scalable fonts.
A value of t means allow any scalable font.
Otherwise, value must be a list of regular expressions.  A font may be
scaled if its name matches a regular expression in the list.
Note that if value is nil, a scalable font might still be used, if no
other font of the appropriate family and registry is available.  */);
  Vscalable_fonts_allowed = Qnil;

  DEFVAR_LISP ("face-ignored-fonts", Vface_ignored_fonts,
	       doc: /* List of ignored fonts.
Each element is a regular expression that matches names of fonts to
ignore.  */);
  Vface_ignored_fonts = Qnil;

  DEFVAR_LISP ("face-remapping-alist", Vface_remapping_alist,
	       doc: /* Alist of face remappings.
Each element is of the form:

   (FACE . REPLACEMENT),

which causes display of the face FACE to use REPLACEMENT instead.
REPLACEMENT is a face specification, i.e. one of the following:

  (1) a face name
  (2) a property list of attribute/value pairs, or
  (3) a list in which each element has the form of (1) or (2).

List values for REPLACEMENT are merged to form the final face
specification, with earlier entries taking precedence, in the same as
as in the `face' text property.

Face-name remapping cycles are suppressed; recursive references use
the underlying face instead of the remapped face.  So a remapping of
the form:

   (FACE EXTRA-FACE... FACE)

or:

   (FACE (FACE-ATTR VAL ...) FACE)

causes EXTRA-FACE... or (FACE-ATTR VAL ...) to be _merged_ with the
existing definition of FACE.  Note that this isn't necessary for the
default face, since every face inherits from the default face.

If this variable is made buffer-local, the face remapping takes effect
only in that buffer.  For instance, the mode my-mode could define a
face `my-mode-default', and then in the mode setup function, do:

   (set (make-local-variable 'face-remapping-alist)
	'((default my-mode-default)))).

Because Emacs normally only redraws screen areas when the underlying
buffer contents change, you may need to call `redraw-display' after
changing this variable for it to take effect.  */);
  Vface_remapping_alist = Qnil;

  DEFVAR_LISP ("face-font-rescale-alist", Vface_font_rescale_alist,
	       doc: /* Alist of fonts vs the rescaling factors.
Each element is a cons (FONT-PATTERN . RESCALE-RATIO), where
FONT-PATTERN is a font-spec or a regular expression matching a font name, and
RESCALE-RATIO is a floating point number to specify how much larger
\(or smaller) font we should use.  For instance, if a face requests
a font of 10 point, we actually use a font of 10 * RESCALE-RATIO point.  */);
  Vface_font_rescale_alist = Qnil;

#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Sbitmap_spec_p);
  defsubr (&Sx_list_fonts);
  defsubr (&Sinternal_face_x_get_resource);
  defsubr (&Sx_family_fonts);
#endif
}
