/* font.h -- Interface definition for font handling.
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

#ifndef EMACS_FONT_H
#define EMACS_FONT_H

#include "ccl.h"

/* We have three types of Lisp objects related to font.

   FONT-SPEC

	Pseudo vector (length FONT_SPEC_MAX) of font properties.  Some
	properties can be left unspecified (i.e. nil).  Emacs asks
	font-drivers to find a font by FONT-SPEC.  A fontset entry
	specifies requisite properties whereas a face specifies just
	preferable properties.

   FONT-ENTITY

	Pseudo vector (length FONT_ENTITY_MAX) of fully instantiated
	font properties that a font-driver returns upon a request of
	FONT-SPEC.

	Note: Only the method `list' and `match' of a font-driver can
	create this object, and it should never be modified by Lisp.

   FONT-OBJECT

	Pseudo vector (length FONT_OBJECT_MAX) of an opened font.

	Lisp object encapsulating "struct font".  This corresponds to
	an opened font.

	Note: Only the method `open' of a font-driver can create this
	object, and it should never be modified by Lisp.  */

extern Lisp_Object Qfont_spec, Qfont_entity, Qfont_object;


struct font_driver;
struct font;
struct glyph_string;

/* An enumerator for each font property.  This is used as an index to
   the vector of FONT-SPEC and FONT-ENTITY.

   Note: The order is important and should not be changed.  */

enum font_property_index
  {
    /* FONT-TYPE is a symbol indicating a font backend; currently `x',
       `xft', and `ftx' are available on X, `uniscribe' and `gdi' on
       Windows, and `ns' under Cocoa / GNUstep.  */
    FONT_TYPE_INDEX,

    /* FONT-FOUNDRY is a foundry name (symbol).  */
    FONT_FOUNDRY_INDEX,

    /* FONT-FAMILY is a family name (symbol).  */
    FONT_FAMILY_INDEX,

    /* FONT-ADSTYLE is an additional style name (symbol).  */
    FONT_ADSTYLE_INDEX,

    /* FONT-REGISTRY is a combination of a charset-registry and
       charset-encoding name (symbol).  */
    FONT_REGISTRY_INDEX,

    /* FONT-WEIGHT is a numeric value of weight (e.g. medium, bold) of
       the font.  The lowest 8 bits is an index determining the
       symbolic name, and the higher bits is the actual numeric value
       defined in `font-weight-table'. */
    FONT_WEIGHT_INDEX,

    /* FONT-SLANT is a numeric value of slant (e.g. r, i, o) of the
       font.  The lowest 8 bits is an index determining the symbolic
       name, and the higher bits is the actual numeric value defined
       in `font-slant-table'.  */
    FONT_SLANT_INDEX,

    /* FONT-WIDTH is a numeric value of setwidth (e.g. normal) of the
       font.  The lowest 8 bits is an index determining the symbolic
       name, and the higher bits is the actual numeric value defined
       `font-width-table'.  */
    FONT_WIDTH_INDEX,

    /* FONT-SIZE is a size of the font.  If integer, it is a pixel
       size.  For a font-spec, the value can be a float specifying
       the point size.  The value zero means that the font is
       scalable.  */
    FONT_SIZE_INDEX,

    /* FONT-DPI is a resolution (dot per inch) for which the font is
       designed. */
    FONT_DPI_INDEX,

    /* FONT-SPACING is a spacing (mono, proportional, charcell) of the
       font (integer; one of enum font_spacing).  */
    FONT_SPACING_INDEX,

    /* FONT-AVGWIDTH is an average width (1/10 pixel unit) of the
       font.  */
    FONT_AVGWIDTH_INDEX,

#if 0
    /* The following two members are to substitute for the above 6
       members (FONT_WEIGHT_INDEX to FONT_AVGWIDTH_INDEX excluding
       FONT_SIZE_INDEX) if it is found that font-entities consumes too
       much memory.  */

    /* FONT-STYLE is a 24-bit integer containing indices for
       style-related properties WEIGHT, SLANT, and WIDTH.  The lowest
       8 bits is an index to the weight table AREF (font_style_table,
       0), the next 8 bits is an index to the slant table AREF
       (font_style_table, 1), the highest 8 bits is an index to the
       slant table AREF (font_style_table, 2).  The index 0 indicates
       that the corresponding style is not specified.  This way, we
       can represent at most 255 different names for each style, which
       is surely sufficient.  */
    FONT_STYLE_INDEX,

    /* FONT-METRICS is a 27-bit integer containing metrics-related
       properties DPI, AVGWIDTH, SPACING.  The lowest 12 bits is for
       DPI, the next 12 bits is for AVGWIDTH, the highest 3 bits is for
       SPACING.  In each bit field, the highest bit indicates that the
       corresponding value is set or not.  This way, we can represent
       DPI by 11-bit (0 to 2047), AVGWIDTH by 11-bit (0 to 2047),
       SPACING by 3-bit (0 for proportional, 1 for dual, 2 for mono, 3
       for charcell), which is surely sufficient.  */
    FONT_METRICS_INDEX,
#endif

    /* In a font-spec, the value is an alist of extra information of a
       font such as name, OpenType features, and language coverage.
       In addition, in a font-entity, the value may contain a pair
       (font-entity . INFO) where INFO is extra information to identify
       a font (font-driver dependent).  */
    FONT_EXTRA_INDEX,		/* alist		alist */

    /* This value is the length of font-spec vector.  */
    FONT_SPEC_MAX,

    /* The followings are used only for a font-entity and a font-object.  */

    /* List of font-objects opened from the font-entity.  */
    FONT_OBJLIST_INDEX = FONT_SPEC_MAX,

    /* Font-entity from which the font-object is opened.  */
    FONT_ENTITY_INDEX = FONT_SPEC_MAX,

    /* This value is the length of font-entity vector.  */
    FONT_ENTITY_MAX,

    /* The followings are used only for a font-object.  */

    /* XLFD name of the font (string). */
    FONT_NAME_INDEX = FONT_ENTITY_MAX,

    /* Full name of the font (string).  It is the name extracted from
       the opened font, and may be different from the above.  It may be
       nil if the opened font doesn't give a name.  */
    FONT_FULLNAME_INDEX,

    /* File name of the font or nil if a file associated with the font
       is not available.  */
    FONT_FILE_INDEX,

    /* Format of the font (symbol) or nil if unknown.  */
    FONT_FORMAT_INDEX,

    /* This value is the length of font-object vector.  */
    FONT_OBJECT_MAX
  };

/* Return the numeric weight value of FONT.  */
#define FONT_WEIGHT_NUMERIC(font)		\
  (INTEGERP (AREF ((font), FONT_WEIGHT_INDEX))	\
   ? (XINT (AREF ((font), FONT_WEIGHT_INDEX)) >> 8) : -1)
/* Return the numeric slant value of FONT.  */
#define FONT_SLANT_NUMERIC(font)		\
  (INTEGERP (AREF ((font), FONT_SLANT_INDEX))	\
   ? (XINT (AREF ((font), FONT_SLANT_INDEX)) >> 8) : -1)
/* Return the numeric width value of FONT.  */
#define FONT_WIDTH_NUMERIC(font)		\
  (INTEGERP (AREF ((font), FONT_WIDTH_INDEX))	\
   ? (XINT (AREF ((font), FONT_WIDTH_INDEX)) >> 8) : -1)
/* Return the symbolic weight value of FONT.  */
#define FONT_WEIGHT_SYMBOLIC(font)	\
  font_style_symbolic (font, FONT_WEIGHT_INDEX, 0)
/* Return the symbolic slant value of FONT.  */
#define FONT_SLANT_SYMBOLIC(font)	\
  font_style_symbolic (font, FONT_SLANT_INDEX, 0)
/* Return the symbolic width value of FONT.  */
#define FONT_WIDTH_SYMBOLIC(font)	\
  font_style_symbolic (font, FONT_WIDTH_INDEX, 0)
/* Return the face-weight corresponding to the weight of FONT.  */
#define FONT_WEIGHT_FOR_FACE(font)	\
  font_style_symbolic (font, FONT_WEIGHT_INDEX, 1)
/* Return the face-slant corresponding to the slant of FONT.  */
#define FONT_SLANT_FOR_FACE(font)	\
  font_style_symbolic (font, FONT_SLANT_INDEX, 1)
/* Return the face-swidth corresponding to the slant of FONT.  */
#define FONT_WIDTH_FOR_FACE(font)	\
  font_style_symbolic (font, FONT_WIDTH_INDEX, 1)

/* Return the numeric weight value corresponding ot the symbol NAME.  */
#define FONT_WEIGHT_NAME_NUMERIC(name)	\
  (font_style_to_value (FONT_WEIGHT_INDEX, (name), 0) >> 8)
/* Return the numeric slant value corresponding ot the symbol NAME.  */
#define FONT_SLANT_NAME_NUMERIC(name)	\
  (font_style_to_value (FONT_SLANT_INDEX, (name), 0) >> 8)
/* Return the numeric width value corresponding ot the symbol NAME.  */
#define FONT_WIDTH_NAME_NUMERIC(name)	\
  (font_style_to_value (FONT_WIDTH_INDEX, (name), 0) >> 8)

/* Set the font property PROP of FONT to VAL.  PROP is one of
   style-related font property index (FONT_WEIGHT/SLANT/WIDTH_INDEX).
   VAL (integer or symbol) is the numeric or symbolic style value.  */
#define FONT_SET_STYLE(font, prop, val)	\
  ASET ((font), prop, make_number (font_style_to_value (prop, val, 1)))

extern Lisp_Object QCspacing, QCdpi, QCscalable, QCotf, QClang, QCscript;
extern Lisp_Object QCavgwidth, QCantialias, QCfont_entity;
extern Lisp_Object Qp;


/* Important character set symbols.  */
extern Lisp_Object Qascii_0;
extern Lisp_Object Qiso8859_1, Qiso10646_1, Qunicode_bmp, Qunicode_sip;

/* Special ADSTYLE properties to avoid fonts used for Latin characters.  */
extern Lisp_Object Qja, Qko;

/* Structure for a font-spec.  */

struct font_spec
{
  struct vectorlike_header header;
  Lisp_Object props[FONT_SPEC_MAX];
};

/* Structure for a font-entity.  */

struct font_entity
{
  struct vectorlike_header header;
  Lisp_Object props[FONT_ENTITY_MAX];
};

/* A value which may appear in the member `encoding' of struct font
   indicating that a font itself doesn't tell which encoding to be
   used.  */
#define FONT_ENCODING_NOT_DECIDED 255

/* Structure for a font-object.  */

struct font
{
  struct vectorlike_header header;

  /* All Lisp_Object components must come first.
     That ensures they are all aligned normally.  */

  Lisp_Object props[FONT_OBJECT_MAX];

  /* Beyond here, there should be no more Lisp_Object components.  */

  /* Maximum bound width over all existing characters of the font.  On
     X window, this is same as (font->max_bounds.width).  */
  int max_width;

  /* By which pixel size the font is opened.  */
  int pixel_size;

  /* Height of the font.  On X window, this is the same as
     (font->ascent + font->descent).  */
  int height;

  /* Width of the space glyph of the font.  If the font doesn't have a
     SPACE glyph, the value is 0.  */
  int space_width;

  /* Average width of glyphs in the font.  If the font itself doesn't
     have that information but has glyphs of ASCII characters, the
     value is the average with of those glyphs.  Otherwise, the value
     is 0.  */
  int average_width;

  /* Minimum glyph width (in pixels).  */
  int min_width;

  /* Ascent and descent of the font (in pixels).  */
  int ascent, descent;

  /* Vertical pixel width of the underline.  If is zero if that
     information is not in the font.  */
  int underline_thickness;

  /* Vertical pixel position (relative to the baseline) of the
     underline.  If it is positive, it is below the baseline.  It is
     negative if that information is not in the font.  */
  int underline_position;

  /* 1 if `vertical-centering-font-regexp' matches this font name.
     In this case, we render characters at vertical center positions
     of lines.  */
  int vertical_centering;

  /* Encoding type of the font.  The value is one of
     0, 1, 2, or 3:
	0: code points 0x20..0x7F or 0x2020..0x7F7F are used
	1: code points 0xA0..0xFF or 0xA0A0..0xFFFF are used
	2: code points 0x20A0..0x7FFF are used
	3: code points 0xA020..0xFF7F are used
     If the member `font_encoder' is not NULL, this member is ignored.  */
  unsigned char encoding_type;

  /* The baseline position of a font is normally `ascent' value of the
     font.  However, there exist many fonts which don't set `ascent' to
     an appropriate value to be used as baseline position.  This is
     typical in such ASCII fonts which are designed to be used with
     Chinese, Japanese, Korean characters.  When we use mixture of
     such fonts and normal fonts (having correct `ascent' value), a
     display line gets very ugly.  Since we have no way to fix it
     automatically, it is user's responsibility to supply well designed
     fonts or correct `ascent' value of fonts.  But, the latter
     requires heavy work (modifying all bitmap data in BDF files).
     So, Emacs accepts a private font property
     `_MULE_BASELINE_OFFSET'.  If a font has this property, we
     calculate the baseline position by subtracting the value from
     `ascent'.  In other words, the value indicates how many pixels
     higher than normal ASCII text we should draw a character of the
     font for better appearance.

     We also have to consider the fact that the concept of `baseline'
     differs among scripts to which each character belongs.  For
     instance, baseline should be at the bottom-most position of all
     glyphs for Chinese, Japanese, and Korean.  But, many of existing
     fonts for those characters don't have correct `ascent' values
     because they are designed to be used with ASCII fonts.  To
     display characters of different language on the same line, the
     best way will be to arrange them in the middle of the line.  So,
     in such a case, again, we utilize the font property
     `_MULE_BASELINE_OFFSET'.  If the value is larger than `ascent' we
     calculate baseline so that a character is arranged in the middle
     of a line.  */
  int baseline_offset;

  /* Non-zero means a character should be composed at a position
     relative to the height (or depth) of previous glyphs in the
     following cases:
	(1) The bottom of the character is higher than this value.  In
	this case, the character is drawn above the previous glyphs.
	(2) The top of the character is lower than 0 (i.e. baseline
	height).  In this case, the character is drawn below the
	previous glyphs.

     This value is taken from a private font property
     `_MULE_RELATIVE_COMPOSE' which is introduced by Emacs.  */
  int relative_compose;

  /* Non-zero means an ascent value to be used for a character
     registered in char-table `use-default-ascent'.  */
  int default_ascent;

  /* CCL program to calculate code points of the font.  */
  struct ccl_program *font_encoder;

  /* Font-driver for the font.  */
  struct font_driver *driver;

  /* Charset to encode a character code into a glyph code of the font.
     -1 means that the font doesn't require this information to encode
     a character.  */
  int encoding_charset;

  /* Charset to check if a character code is supported by the font.
     -1 means that the contents of the font must be looked up to
     determine it.  */
  int repertory_charset;

  /* There are more members in this structure, but they are private
     to the font-driver.  */
};

enum font_spacing
  {
    FONT_SPACING_PROPORTIONAL = 0,
    FONT_SPACING_DUAL = 90,
    FONT_SPACING_MONO = 100,
    FONT_SPACING_CHARCELL = 110
  };

struct font_metrics
{
  short lbearing, rbearing, width, ascent, descent;
};

struct font_bitmap
{
  int bits_per_pixel;
  int rows;
  int width;
  int pitch;
  unsigned char *buffer;
  int left;
  int top;
  int advance;
  void *extra;
};

/* Predicates to check various font-related objects.  */

/* 1 iff X is one of font-spec, font-entity, and font-object.  */
#define FONTP(x) PSEUDOVECTORP (x, PVEC_FONT)
/* 1 iff X is font-spec.  */
#define FONT_SPEC_P(x)	\
  (FONTP (x) && (ASIZE (x) & PSEUDOVECTOR_SIZE_MASK) == FONT_SPEC_MAX)
/* 1 iff X is font-entity.  */
#define FONT_ENTITY_P(x)	\
  (FONTP (x) && (ASIZE (x) & PSEUDOVECTOR_SIZE_MASK) == FONT_ENTITY_MAX)
/* 1 iff X is font-object.  */
#define FONT_OBJECT_P(x)	\
  (FONTP (x) && (ASIZE (x) & PSEUDOVECTOR_SIZE_MASK) == FONT_OBJECT_MAX)

/* 1 iff ENTITY can't be loaded.  */
#define FONT_ENTITY_NOT_LOADABLE(entity)	\
  EQ (AREF (entity, FONT_OBJLIST_INDEX), Qt)

/* Flag ENTITY not loadable.  */
#define FONT_ENTITY_SET_NOT_LOADABLE(entity)	\
  ASET (entity, FONT_OBJLIST_INDEX, Qt)


/* Check macros for various font-related objects.  */

#define CHECK_FONT(x)	\
  do { if (! FONTP (x)) wrong_type_argument (Qfont, x); } while (0)
#define CHECK_FONT_SPEC(x)	\
  do { if (! FONT_SPEC_P (x)) wrong_type_argument (Qfont_spec, x); } while (0)
#define CHECK_FONT_ENTITY(x)	\
  do { if (! FONT_ENTITY_P (x)) wrong_type_argument (Qfont_entity, x); } while (0)
#define CHECK_FONT_OBJECT(x)	\
  do { if (! FONT_OBJECT_P (x)) wrong_type_argument (Qfont_object, x); } while (0)

#define CHECK_FONT_GET_OBJECT(x, font)	\
  do {					\
    CHECK_FONT_OBJECT (x);		\
    font = XFONT_OBJECT (x);		\
  } while (0)

#define XFONT_SPEC(p)	\
  (eassert (FONT_SPEC_P(p)), (struct font_spec *) XPNTR (p))
#define XFONT_ENTITY(p)	\
  (eassert (FONT_ENTITY_P(p)), (struct font_entity *) XPNTR (p))
#define XFONT_OBJECT(p)	\
  (eassert (FONT_OBJECT_P(p)), (struct font *) XPNTR (p))
#define XSETFONT(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_FONT))

/* Number of pt per inch (from the TeXbook).  */
#define PT_PER_INCH 72.27

/* Return a pixel size (integer) corresponding to POINT size (double)
   on resolution DPI.  */
#define POINT_TO_PIXEL(POINT, DPI) ((POINT) * (DPI) / PT_PER_INCH + 0.5)

/* Return a point size corresponding to POINT size (integer)
   on resolution DPI.  Note that though point size is a double, we expect
   it to be rounded to an int, so we add 0.5 here.  If the desired value
   is tenths of points (as in xfld specs), then the pixel size should
   be multiplied BEFORE the conversion to avoid magnifying the error.  */
#define PIXEL_TO_POINT(PIXEL, DPI) ((PIXEL) * PT_PER_INCH / (DPI) + 0.5)

/* Ignore the difference of font pixel sizes less than or equal to
   this value.  */
#define FONT_PIXEL_SIZE_QUANTUM 1

struct face;

#define FONT_INVALID_CODE 0xFFFFFFFF

/* Font driver.  Members specified as "optional" can be NULL.  */

struct font_driver
{
  /* Symbol indicating the type of the font-driver.  */
  Lisp_Object type;

  /* 1 iff the font's foundry, family, and adstyle names are case
     sensitive.  */
  int case_sensitive;

  /* Return a cache of font-entities on frame F.  The cache must be a
     cons whose cdr part is the actual cache area.  */
  Lisp_Object (*get_cache) (FRAME_PTR F);

  /* List fonts exactly matching with FONT_SPEC on FRAME.  The value
     is a list of font-entities.  The font properties to be considered
     are: :foundry, :family, :adstyle, :registry, :script, :lang, and
     :otf.  See the function `font-spec' for their meanings.  Note
     that the last three properties are stored in FONT_EXTRA_INDEX
     slot of FONT_SPEC.

     The returned value is a list of font-entities.  Each font-entity
     has :type property whose value is the same as the above `type'.
     It also has these properties if they are available from the
     corresponding font; :foundry, :family, :adstyle, :registry,
     :weight, :slant, :width, :size, :dpi, :spacing, :avgwidth.  If
     the font is scalable, :size and :avgwidth must be 0.

     The `open' method of the same font-backend is called with one of
     the returned font-entities.  If the backend needs additional
     information to be used in `open' method, this method can add any
     Lispy value by the property :font-entity to the entities.

     This and the following `match' are the only APIs that allocate
     font-entities.  */
  Lisp_Object (*list) (Lisp_Object frame, Lisp_Object font_spec);

  /* Return a font-entity most closely matching with FONT_SPEC on
     FRAME.  Which font property to consider, and how to calculate the
     closeness is determined by the font backend, thus
     `face-font-selection-order' is ignored here.

     The properties that the font-entity has is the same as `list'
     method.  */
  Lisp_Object (*match) (Lisp_Object frame, Lisp_Object font_spec);

  /* Optional.
     List available families.  The value is a list of family names
     (symbols).  */
  Lisp_Object (*list_family) (Lisp_Object frame);

  /* Optional (if FONT_EXTRA_INDEX is not Lisp_Save_Value).
     Free FONT_EXTRA_INDEX field of FONT_ENTITY.  */
  void (*free_entity) (Lisp_Object font_entity);

  /* Open a font specified by FONT_ENTITY on frame F.  If the font is
     scalable, open it with PIXEL_SIZE.  */
  Lisp_Object (*open) (FRAME_PTR f, Lisp_Object font_entity,
                       int pixel_size);

  /* Close FONT on frame F.  */
  void (*close) (FRAME_PTR f, struct font *font);

  /* Optional (if FACE->extra is not used).
     Prepare FACE for displaying characters by FONT on frame F by
     storing some data in FACE->extra.  If successful, return 0.
     Otherwise, return -1.  */
  int (*prepare_face) (FRAME_PTR f, struct face *face);

  /* Optional.
     Done FACE for displaying characters by FACE->font on frame F.  */
  void (*done_face) (FRAME_PTR f, struct face *face);

  /* Optional.
     If FONT (FONT-ENTITY or FONT-OBJECT) has a glyph for character C
     (Unicode code point), return 1.  If not, return 0.  If FONT is
     FONT-ENTITY and it must be opened to check it, return -1.  */
  int (*has_char) (Lisp_Object font, int c);

  /* Return a glyph code of FONT for character C (Unicode code point).
     If FONT doesn't have such a glyph, return FONT_INVALID_CODE.  */
  unsigned (*encode_char) (struct font *font, int c);

  /* Compute the total metrics of the NGLYPHS glyphs specified by
     the font FONT and the sequence of glyph codes CODE, and store the
     result in METRICS.  */
  int (*text_extents) (struct font *font,
                       unsigned *code, int nglyphs,
                       struct font_metrics *metrics);

  /* Optional.
     Draw glyphs between FROM and TO of S->char2b at (X Y) pixel
     position of frame F with S->FACE and S->GC.  If WITH_BACKGROUND
     is nonzero, fill the background in advance.  It is assured that
     WITH_BACKGROUND is zero when (FROM > 0 || TO < S->nchars).  */
  int (*draw) (struct glyph_string *s, int from, int to,
               int x, int y, int with_background);

  /* Optional.
     Store bitmap data for glyph-code CODE of FONT in BITMAP.  It is
     intended that this method is called from the other font-driver
     for actual drawing.  */
  int (*get_bitmap) (struct font *font, unsigned code,
                     struct font_bitmap *bitmap,
                     int bits_per_pixel);

  /* Optional.
     Free bitmap data in BITMAP.  */
  void (*free_bitmap) (struct font *font, struct font_bitmap *bitmap);

  /* Optional.
     Return an outline data for glyph-code CODE of FONT.  The format
     of the outline data depends on the font-driver.  */
  void *(*get_outline) (struct font *font, unsigned code);

  /* Optional.
     Free OUTLINE (that is obtained by the above method).  */
  void (*free_outline) (struct font *font, void *outline);

  /* Optional.
     Get coordinates of the INDEXth anchor point of the glyph whose
     code is CODE.  Store the coordinates in *X and *Y.  Return 0 if
     the operations was successful.  Otherwise return -1.  */
  int (*anchor_point) (struct font *font, unsigned code, int index,
                       int *x, int *y);

  /* Optional.
     Return a list describing which scripts/languages FONT
     supports by which GSUB/GPOS features of OpenType tables.  */
  Lisp_Object (*otf_capability) (struct font *font);

  /* Optional.
     Apply FONT's OTF-FEATURES to the glyph string.

     FEATURES specifies which OTF features to apply in this format:
	(SCRIPT LANGSYS GSUB-FEATURE GPOS-FEATURE)
     See the documentation of `font-drive-otf' for the details.

     This method applies the specified features to the codes in the
     elements of GSTRING-IN (between FROMth and TOth).  The output
     codes are stored in GSTRING-OUT at the IDXth element and the
     following elements.

     Return the number of output codes.  If none of the features are
     applicable to the input data, return 0.  If GSTRING-OUT is too
     short, return -1.  */
  int (*otf_drive) (struct font *font, Lisp_Object features,
                    Lisp_Object gstring_in, int from, int to,
                    Lisp_Object gstring_out, int idx, int alternate_subst);

  /* Optional.
     Make the font driver ready for frame F.  Usually this function
     makes some data specific to F and stores it in F by calling
     font_put_frame_data ().  */
  int (*start_for_frame) (FRAME_PTR f);

  /* Optional.
     End using the driver for frame F.  Usually this function free
     some data stored for F.  */
  int (*end_for_frame) (FRAME_PTR f);

  /* Optional.

     Shape text in GSTRING.  See the docstring of
     `composition-get-gstring' for the format of GSTRING.  If the
     (N+1)th element of GSTRING is nil, input of shaping is from the
     1st to (N)th elements.  In each input glyph, FROM, TO, CHAR, and
     CODE are already set.

     This function updates all fields of the input glyphs.  If the
     output glyphs (M) are more than the input glyphs (N), (N+1)th
     through (M)th elements of GSTRING are updated possibly by making
     a new glyph object and storing it in GSTRING.  If (M) is greater
     than the length of GSTRING, nil should be return.  In that case,
     this function is called again with the larger GSTRING.  */
  Lisp_Object (*shape) (Lisp_Object lgstring);

  /* Optional.

     If FONT is usable on frame F, return 0.  Otherwise return -1.
     This method is used only for debugging.  If this method is NULL,
     Emacs assumes that the font is usable on any frame.  */
  int (*check) (FRAME_PTR F, struct font *font);

  /* Optional.

     Return the number of variation glyphs of character C supported by
     FONT.  VARIATIONS is an array of 256 elements.  If the variation
     selector N (1..256) defines a glyph, that glyph code is stored in
     the (N-1)th element of VARIATIONS.  */
  int (*get_variation_glyphs) (struct font *font,
                               int c, unsigned variations[256]);

  void (*filter_properties) (Lisp_Object font, Lisp_Object properties);

  /* Optional.

     Return non-zero if FONT_OBJECT can be used as a (cached) font
     for ENTITY on frame F.  */
  int (*cached_font_ok) (struct frame *f,
                         Lisp_Object font_object,
                         Lisp_Object entity);
};


/* Chain of font drivers.  There's one global font driver list
   (font_driver_list in font.c).  In addition, each frame has its own
   font driver list at FRAME_PTR->font_driver_list.  */

struct font_driver_list
{
  /* 1 iff this driver is currently used.  It is ignored in the global
     font driver list.*/
  int on;
  /* Pointer to the font driver.  */
  struct font_driver *driver;
  /* Pointer to the next element of the chain.  */
  struct font_driver_list *next;
};


/* Chain of arbitrary data specific to each font driver.  Each frame
   has its own font data list at FRAME_PTR->font_data_list.  */

struct font_data_list
{
  /* Pointer to the font driver.  */
  struct font_driver *driver;
  /* Data specific to the font driver.  */
  void *data;
  /* Pointer to the next element of the chain.  */
  struct font_data_list *next;
};

EXFUN (Ffont_spec, MANY);
extern Lisp_Object copy_font_spec (Lisp_Object);
extern Lisp_Object merge_font_spec (Lisp_Object, Lisp_Object);
EXFUN (Ffont_get, 2);
EXFUN (Ffont_put, 3);
EXFUN (Flist_fonts, 4);
EXFUN (Ffont_xlfd_name, 2);

extern Lisp_Object font_make_entity (void);
extern Lisp_Object font_make_object (int, Lisp_Object, int);

extern Lisp_Object find_font_encoding (Lisp_Object);
extern int font_registry_charsets (Lisp_Object, struct charset **,
                                   struct charset **);
extern int font_style_to_value (enum font_property_index prop,
                                Lisp_Object name, int noerror);
extern Lisp_Object font_style_symbolic (Lisp_Object font,
                                        enum font_property_index prop,
                                        int for_face);

extern int font_match_p (Lisp_Object spec, Lisp_Object font);
extern Lisp_Object font_list_entities (Lisp_Object frame,
                                       Lisp_Object spec);

extern Lisp_Object font_get_name (Lisp_Object font_object);
extern Lisp_Object font_spec_from_name (Lisp_Object font_name);
extern Lisp_Object font_get_frame (Lisp_Object font_object);
extern int font_has_char (FRAME_PTR, Lisp_Object, int);

extern void font_clear_prop (Lisp_Object *attrs,
                             enum font_property_index prop);
extern Lisp_Object font_find_for_lface (FRAME_PTR f, Lisp_Object *lface,
                                        Lisp_Object spec, int c);
extern Lisp_Object font_open_for_lface (FRAME_PTR f, Lisp_Object entity,
                                        Lisp_Object *lface,
                                        Lisp_Object spec);
extern Lisp_Object font_load_for_lface (FRAME_PTR f, Lisp_Object *lface,
                                        Lisp_Object spec);
extern void font_prepare_for_face (FRAME_PTR f, struct face *face);
extern void font_done_for_face (FRAME_PTR f, struct face *face);

extern Lisp_Object font_open_by_spec (FRAME_PTR f, Lisp_Object spec);
extern Lisp_Object font_open_by_name (FRAME_PTR f, const char *name);

extern Lisp_Object font_intern_prop (const char *str, ptrdiff_t len,
				     int force_symbol);
extern void font_update_sort_order (int *order);

extern void font_parse_family_registry (Lisp_Object family,
                                        Lisp_Object registry,
                                        Lisp_Object spec);

extern int font_parse_xlfd (char *name, Lisp_Object font);
extern int font_unparse_xlfd (Lisp_Object font, int pixel_size,
                              char *name, int bytes);
extern int font_unparse_fcname (Lisp_Object font, int pixel_size,
                                char *name, int bytes);
extern void register_font_driver (struct font_driver *driver, FRAME_PTR f);
extern void free_font_driver_list (FRAME_PTR f);
extern Lisp_Object font_update_drivers (FRAME_PTR f, Lisp_Object list);
extern Lisp_Object font_range (EMACS_INT, EMACS_INT *,
			       struct window *, struct face *,
			       Lisp_Object);
extern void font_fill_lglyph_metrics (Lisp_Object, Lisp_Object);

extern Lisp_Object font_put_extra (Lisp_Object font, Lisp_Object prop,
                                   Lisp_Object val);

extern int font_put_frame_data (FRAME_PTR f,
                                struct font_driver *driver,
                                void *data);
extern void *font_get_frame_data (FRAME_PTR f,
                                  struct font_driver *driver);

extern void font_filter_properties (Lisp_Object font,
				    Lisp_Object alist,
				    const char *const boolean_properties[],
				    const char *const non_boolean_properties[]);

#ifdef HAVE_FREETYPE
extern struct font_driver ftfont_driver;
extern void syms_of_ftfont (void);
#endif	/* HAVE_FREETYPE */
#ifdef HAVE_X_WINDOWS
extern struct font_driver xfont_driver;
extern void syms_of_xfont (void);
extern void syms_of_ftxfont (void);
#ifdef HAVE_XFT
extern struct font_driver xftfont_driver;
extern void syms_of_xftfont (void);
#elif defined HAVE_FREETYPE
extern struct font_driver ftxfont_driver;
#endif
#ifdef HAVE_BDFFONT
extern void syms_of_bdffont (void);
#endif	/* HAVE_BDFFONT */
#endif	/* HAVE_X_WINDOWS */
#ifdef WINDOWSNT
extern struct font_driver w32font_driver;
extern struct font_driver uniscribe_font_driver;
extern void syms_of_w32font (void);
#endif	/* WINDOWSNT */
#ifdef HAVE_NS
extern Lisp_Object Qfontsize;
extern struct font_driver nsfont_driver;
extern void syms_of_nsfont (void);
#endif	/* HAVE_NS */

#ifndef FONT_DEBUG
#define FONT_DEBUG
#endif

extern Lisp_Object QCfoundry;

extern void font_add_log (const char *, Lisp_Object, Lisp_Object);
extern void font_deferred_log (const char *, Lisp_Object, Lisp_Object);

#define FONT_ADD_LOG(ACTION, ARG, RESULT)	\
  do {						\
    if (! EQ (Vfont_log, Qt))			\
      font_add_log ((ACTION), (ARG), (RESULT));	\
  } while (0)

#define FONT_DEFERRED_LOG(ACTION, ARG, RESULT)		\
  do {							\
    if (! EQ (Vfont_log, Qt))				\
      font_deferred_log ((ACTION), (ARG), (RESULT));	\
  } while (0)

#ifdef FONT_DEBUG
#define font_assert(X)	do {if (!(X)) abort ();} while (0)
#else  /* not FONT_DEBUG */
#define font_assert(X)	(void) 0
#endif	/* not FONT_DEBUG */

#endif	/* not EMACS_FONT_H */
