/* Header for coding system handler.
   Copyright (C) 2001-2012  Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
     2005, 2006, 2007, 2008, 2009, 2010, 2011
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003
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

#ifndef EMACS_CODING_H
#define EMACS_CODING_H

/* Index to arguments of Fdefine_coding_system_internal.  */

enum define_coding_system_arg_index
  {
    coding_arg_name,
    coding_arg_mnemonic,
    coding_arg_coding_type,
    coding_arg_charset_list,
    coding_arg_ascii_compatible_p,
    coding_arg_decode_translation_table,
    coding_arg_encode_translation_table,
    coding_arg_post_read_conversion,
    coding_arg_pre_write_conversion,
    coding_arg_default_char,
    coding_arg_for_unibyte,
    coding_arg_plist,
    coding_arg_eol_type,
    coding_arg_max
  };

enum define_coding_iso2022_arg_index
  {
    coding_arg_iso2022_initial = coding_arg_max,
    coding_arg_iso2022_reg_usage,
    coding_arg_iso2022_request,
    coding_arg_iso2022_flags,
    coding_arg_iso2022_max
  };

enum define_coding_utf8_arg_index
  {
    coding_arg_utf8_bom = coding_arg_max,
    coding_arg_utf8_max
  };

enum define_coding_utf16_arg_index
  {
    coding_arg_utf16_bom = coding_arg_max,
    coding_arg_utf16_endian,
    coding_arg_utf16_max
  };

enum define_coding_ccl_arg_index
  {
    coding_arg_ccl_decoder = coding_arg_max,
    coding_arg_ccl_encoder,
    coding_arg_ccl_valids,
    coding_arg_ccl_max
  };

/* Hash table for all coding systems.  Keys are coding system symbols
   and values are spec vectors of the corresponding coding system.  A
   spec vector has the form [ ATTRS ALIASES EOL-TYPE ].  ATTRS is a
   vector of attribute of the coding system.  ALIASES is a list of
   aliases (symbols) of the coding system.  EOL-TYPE is `unix', `dos',
   `mac' or a vector of coding systems (symbols).  */

extern Lisp_Object Vcoding_system_hash_table;


/* Enumeration of coding system type.  */

enum coding_system_type
  {
    coding_type_charset,
    coding_type_utf_8,
    coding_type_utf_16,
    coding_type_iso_2022,
    coding_type_emacs_mule,
    coding_type_sjis,
    coding_type_ccl,
    coding_type_raw_text,
    coding_type_undecided,
    coding_type_max
  };


/* Enumeration of end-of-line format type.  */

enum end_of_line_type
  {
    eol_lf,		/* Line-feed only, same as Emacs' internal
			   format.  */
    eol_crlf,		/* Sequence of carriage-return and
			   line-feed.  */
    eol_cr,		/* Carriage-return only.  */
    eol_any,		/* Accept any of above.  Produce line-feed
			   only.  */
    eol_undecided,	/* This value is used to denote that the
			   eol-type is not yet undecided.  */
    eol_type_max
  };

/* Enumeration of index to an attribute vector of a coding system.  */

enum coding_attr_index
  {
    coding_attr_base_name,
    coding_attr_docstring,
    coding_attr_mnemonic,
    coding_attr_type,
    coding_attr_charset_list,
    coding_attr_ascii_compat,
    coding_attr_decode_tbl,
    coding_attr_encode_tbl,
    coding_attr_trans_tbl,
    coding_attr_post_read,
    coding_attr_pre_write,
    coding_attr_default_char,
    coding_attr_for_unibyte,
    coding_attr_plist,

    coding_attr_category,
    coding_attr_safe_charsets,

    /* The followings are extra attributes for each type.  */
    coding_attr_charset_valids,

    coding_attr_ccl_decoder,
    coding_attr_ccl_encoder,
    coding_attr_ccl_valids,

    coding_attr_iso_initial,
    coding_attr_iso_usage,
    coding_attr_iso_request,
    coding_attr_iso_flags,

    coding_attr_utf_bom,
    coding_attr_utf_16_endian,

    coding_attr_emacs_mule_full,

    coding_attr_last_index
  };


/* Macros to access an element of an attribute vector.  */

#define CODING_ATTR_BASE_NAME(attrs)	AREF (attrs, coding_attr_base_name)
#define CODING_ATTR_TYPE(attrs)		AREF (attrs, coding_attr_type)
#define CODING_ATTR_CHARSET_LIST(attrs)	AREF (attrs, coding_attr_charset_list)
#define CODING_ATTR_MNEMONIC(attrs)	AREF (attrs, coding_attr_mnemonic)
#define CODING_ATTR_DOCSTRING(attrs)	AREF (attrs, coding_attr_docstring)
#define CODING_ATTR_ASCII_COMPAT(attrs)	AREF (attrs, coding_attr_ascii_compat)
#define CODING_ATTR_DECODE_TBL(attrs)	AREF (attrs, coding_attr_decode_tbl)
#define CODING_ATTR_ENCODE_TBL(attrs)	AREF (attrs, coding_attr_encode_tbl)
#define CODING_ATTR_TRANS_TBL(attrs)	AREF (attrs, coding_attr_trans_tbl)
#define CODING_ATTR_POST_READ(attrs)	AREF (attrs, coding_attr_post_read)
#define CODING_ATTR_PRE_WRITE(attrs)	AREF (attrs, coding_attr_pre_write)
#define CODING_ATTR_DEFAULT_CHAR(attrs)	AREF (attrs, coding_attr_default_char)
#define CODING_ATTR_FOR_UNIBYTE(attrs)	AREF (attrs, coding_attr_for_unibyte)
#define CODING_ATTR_FLUSHING(attrs)	AREF (attrs, coding_attr_flushing)
#define CODING_ATTR_PLIST(attrs)	AREF (attrs, coding_attr_plist)
#define CODING_ATTR_CATEGORY(attrs)	AREF (attrs, coding_attr_category)
#define CODING_ATTR_SAFE_CHARSETS(attrs)AREF (attrs, coding_attr_safe_charsets)


/* Return the name of a coding system specified by ID.  */
#define CODING_ID_NAME(id) \
  (HASH_KEY (XHASH_TABLE (Vcoding_system_hash_table), id))

/* Return the attribute vector of a coding system specified by ID.  */

#define CODING_ID_ATTRS(id)	\
  (AREF (HASH_VALUE (XHASH_TABLE (Vcoding_system_hash_table), id), 0))

/* Return the list of aliases of a coding system specified by ID.  */

#define CODING_ID_ALIASES(id)	\
  (AREF (HASH_VALUE (XHASH_TABLE (Vcoding_system_hash_table), id), 1))

/* Return the eol-type of a coding system specified by ID.  */

#define CODING_ID_EOL_TYPE(id)	\
  (AREF (HASH_VALUE (XHASH_TABLE (Vcoding_system_hash_table), id), 2))


/* Return the spec vector of CODING_SYSTEM_SYMBOL.  */

#define CODING_SYSTEM_SPEC(coding_system_symbol)	\
  (Fgethash (coding_system_symbol, Vcoding_system_hash_table, Qnil))


/* Return the ID of CODING_SYSTEM_SYMBOL.  */

#define CODING_SYSTEM_ID(coding_system_symbol)			\
  hash_lookup (XHASH_TABLE (Vcoding_system_hash_table),		\
	       coding_system_symbol, NULL)

/* Return 1 if CODING_SYSTEM_SYMBOL is a coding system.  */

#define CODING_SYSTEM_P(coding_system_symbol)		\
  (CODING_SYSTEM_ID (coding_system_symbol) >= 0		\
   || (! NILP (coding_system_symbol)			\
       && ! NILP (Fcoding_system_p (coding_system_symbol))))

/* Check if X is a coding system or not.  */

#define CHECK_CODING_SYSTEM(x)				\
  do {							\
    if (CODING_SYSTEM_ID (x) < 0			\
	&& NILP (Fcheck_coding_system (x)))		\
      wrong_type_argument (Qcoding_system_p, (x));	\
  } while (0)


/* Check if X is a coding system or not.  If it is, set SEPC to the
   spec vector of the coding system.  */

#define CHECK_CODING_SYSTEM_GET_SPEC(x, spec)		\
  do {							\
    spec = CODING_SYSTEM_SPEC (x);			\
    if (NILP (spec))					\
      {							\
	Fcheck_coding_system (x);			\
	spec = CODING_SYSTEM_SPEC (x);			\
      }							\
    if (NILP (spec))					\
      wrong_type_argument (Qcoding_system_p, (x));	\
  } while (0)


/* Check if X is a coding system or not.  If it is, set ID to the
   ID of the coding system.  */

#define CHECK_CODING_SYSTEM_GET_ID(x, id)			\
  do								\
    {								\
      id = CODING_SYSTEM_ID (x);				\
      if (id < 0)						\
	{							\
	  Fcheck_coding_system (x);				\
	  id = CODING_SYSTEM_ID (x);				\
	}							\
      if (id < 0)						\
	wrong_type_argument (Qcoding_system_p, (x));	\
    } while (0)


/*** GENERAL section ***/

/* Enumeration of result code of code conversion.  */
enum coding_result_code
  {
    CODING_RESULT_SUCCESS,
    CODING_RESULT_INSUFFICIENT_SRC,
    CODING_RESULT_INSUFFICIENT_DST,
    CODING_RESULT_INCONSISTENT_EOL,
    CODING_RESULT_INVALID_SRC,
    CODING_RESULT_INTERRUPT,
    CODING_RESULT_INSUFFICIENT_MEM
  };


/* Macros used for the member `mode' of the struct coding_system.  */

/* If set, recover the original CR or LF of the already decoded text
   when the decoding routine encounters an inconsistent eol format.  */
#define CODING_MODE_INHIBIT_INCONSISTENT_EOL	0x01

/* If set, the decoding/encoding routines treat the current data as
   the last block of the whole text to be converted, and do the
   appropriate finishing job.  */
#define CODING_MODE_LAST_BLOCK			0x02

/* If set, it means that the current source text is in a buffer which
   enables selective display.  */
#define CODING_MODE_SELECTIVE_DISPLAY		0x04

/* This flag is used by the decoding/encoding routines on the fly.  If
   set, it means that right-to-left text is being processed.  */
#define CODING_MODE_DIRECTION			0x08

#define CODING_MODE_FIXED_DESTINATION		0x10

/* If set, it means that the encoding routines produces some safe
   ASCII characters (usually '?') for unsupported characters.  */
#define CODING_MODE_SAFE_ENCODING		0x20

  /* For handling composition sequence.  */
#include "composite.h"

enum composition_state
  {
    COMPOSING_NO,
    COMPOSING_CHAR,
    COMPOSING_RULE,
    COMPOSING_COMPONENT_CHAR,
    COMPOSING_COMPONENT_RULE
  };

/* Structure for the current composition status.  */
struct composition_status
{
  enum composition_state state;
  enum composition_method method;
  int old_form;		  /* 0:pre-21 form, 1:post-21 form */
  int length;		  /* number of elements produced in charbuf */
  int nchars;		  /* number of characters composed */
  int ncomps;		  /* number of composition components */
  /* Maximum carryover is for the case of COMPOSITION_WITH_RULE_ALTCHARS.
     See the comment in coding.c.  */
  int carryover[4 		/* annotation header */
		+ MAX_COMPOSITION_COMPONENTS * 3 - 2 /* ALTs and RULEs */
		+ 2				     /* intermediate -1 -1 */
		+ MAX_COMPOSITION_COMPONENTS	     /* CHARs */
		];
};


/* Structure of the field `spec.iso_2022' in the structure
   `coding_system'.  */
struct iso_2022_spec
{
  /* Bit-wise-or of CODING_ISO_FLAG_XXX.  */
  unsigned flags;

  /* The current graphic register invoked to each graphic plane.  */
  int current_invocation[2];

  /* The current charset designated to each graphic register.  The
     value -1 means that not charset is designated, -2 means that
     there was an invalid designation previously.  */
  int current_designation[4];

  /* Set to 1 temporarily only when graphic register 2 or 3 is invoked
     by single-shift while encoding.  */
  int single_shifting;

  /* Set to 1 temporarily only when processing at beginning of line.  */
  int bol;

  /* If positive, we are now scanning CTEXT extended segment.  */
  int ctext_extended_segment_len;

  /* If nonzero, we are now scanning embedded UTF-8 sequence.  */
  int embedded_utf_8;

  /* The current composition.  */
  struct composition_status cmp_status;
};

struct emacs_mule_spec
{
  int full_support;
  struct composition_status cmp_status;
};

struct ccl_spec;

enum utf_bom_type
  {
    utf_detect_bom,
    utf_without_bom,
    utf_with_bom
  };

enum utf_16_endian_type
  {
    utf_16_big_endian,
    utf_16_little_endian
  };

struct utf_16_spec
{
  enum utf_bom_type bom;
  enum utf_16_endian_type endian;
  int surrogate;
};

struct coding_detection_info
{
  /* Values of these members are bitwise-OR of CATEGORY_MASK_XXXs.  */
  /* Which categories are already checked.  */
  int checked;
  /* Which categories are strongly found.  */
  int found;
  /* Which categories are rejected.  */
  int rejected;
};


struct coding_system
{
  /* ID number of the coding system.  This is an index to
     Vcoding_system_hash_table.  This value is set by
     setup_coding_system.  At the early stage of building time, this
     value is -1 in the array coding_categories to indicate that no
     coding-system of that category is yet defined.  */
  ptrdiff_t id;

  /* Flag bits of the coding system.  The meaning of each bit is common
     to all types of coding systems.  */
  int common_flags;

  /* Mode bits of the coding system.  See the comments of the macros
     CODING_MODE_XXX.  */
  unsigned int mode;

  /* Detailed information specific to each type of coding system.  */
  union
    {
      struct iso_2022_spec iso_2022;
      struct ccl_spec *ccl;	/* Defined in ccl.h.  */
      struct utf_16_spec utf_16;
      enum utf_bom_type utf_8_bom;
      struct emacs_mule_spec emacs_mule;
    } spec;

  int max_charset_id;
  unsigned char *safe_charsets;

  /* The following two members specify how binary 8-bit code 128..255
     are represented in source and destination text respectively.  1
     means they are represented by 2-byte sequence, 0 means they are
     represented by 1-byte as is (see the comment in character.h).  */
  unsigned src_multibyte : 1;
  unsigned dst_multibyte : 1;

  /* How may heading bytes we can skip for decoding.  This is set to
     -1 in setup_coding_system, and updated by detect_coding.  So,
     when this is equal to the byte length of the text being
     converted, we can skip the actual conversion process.  */
  EMACS_INT head_ascii;

  /* The following members are set by encoding/decoding routine.  */
  EMACS_INT produced, produced_char, consumed, consumed_char;

  /* Number of error source data found in a decoding routine.  */
  int errors;

  /* Store the positions of error source data.  */
  EMACS_INT *error_positions;

  /* Finish status of code conversion.  */
  enum coding_result_code result;

  EMACS_INT src_pos, src_pos_byte, src_chars, src_bytes;
  Lisp_Object src_object;
  const unsigned char *source;

  EMACS_INT dst_pos, dst_pos_byte, dst_bytes;
  Lisp_Object dst_object;
  unsigned char *destination;

  /* Set to 1 if the source of conversion is not in the member
     `charbuf', but at `src_object'.  */
  int chars_at_source;

  /* If an element is non-negative, it is a character code.

     If it is in the range -128..-1, it is a 8-bit character code
     minus 256.

     If it is less than -128, it specifies the start of an annotation
     chunk.  The length of the chunk is -128 minus the value of the
     element.  The following elements are OFFSET, ANNOTATION-TYPE, and
     a sequence of actual data for the annotation.  OFFSET is a
     character position offset from dst_pos or src_pos,
     ANNOTATION-TYPE specifies the meaning of the annotation and how to
     handle the following data..  */
  int *charbuf;
  int charbuf_size, charbuf_used;

  /* Set to 1 if charbuf contains an annotation.  */
  int annotated;

  unsigned char carryover[64];
  int carryover_bytes;

  int default_char;

  int (*detector) (struct coding_system *,
                   struct coding_detection_info *);
  void (*decoder) (struct coding_system *);
  int (*encoder) (struct coding_system *);
};

/* Meanings of bits in the member `common_flags' of the structure
   coding_system.  The lowest 8 bits are reserved for various kind of
   annotations (currently two of them are used).  */
#define CODING_ANNOTATION_MASK			0x00FF
#define CODING_ANNOTATE_COMPOSITION_MASK	0x0001
#define CODING_ANNOTATE_DIRECTION_MASK		0x0002
#define CODING_ANNOTATE_CHARSET_MASK		0x0003
#define CODING_FOR_UNIBYTE_MASK			0x0100
#define CODING_REQUIRE_FLUSHING_MASK		0x0200
#define CODING_REQUIRE_DECODING_MASK		0x0400
#define CODING_REQUIRE_ENCODING_MASK		0x0800
#define CODING_REQUIRE_DETECTION_MASK		0x1000
#define CODING_RESET_AT_BOL_MASK		0x2000

/* Return 1 if the coding context CODING requires annotation
   handling.  */
#define CODING_REQUIRE_ANNOTATION(coding) \
  ((coding)->common_flags & CODING_ANNOTATION_MASK)

/* Return 1 if the coding context CODING prefers decoding into unibyte.  */
#define CODING_FOR_UNIBYTE(coding) \
  ((coding)->common_flags & CODING_FOR_UNIBYTE_MASK)

/* Return 1 if the coding context CODING requires specific code to be
   attached at the tail of converted text.  */
#define CODING_REQUIRE_FLUSHING(coding) \
  ((coding)->common_flags & CODING_REQUIRE_FLUSHING_MASK)

/* Return 1 if the coding context CODING requires code conversion on
   decoding.  */
#define CODING_REQUIRE_DECODING(coding)	\
  ((coding)->dst_multibyte		\
   || (coding)->common_flags & CODING_REQUIRE_DECODING_MASK)


/* Return 1 if the coding context CODING requires code conversion on
   encoding.
   The non-multibyte part of the condition is to support encoding of
   unibyte strings/buffers generated by string-as-unibyte or
   (set-buffer-multibyte nil) from multibyte strings/buffers.  */
#define CODING_REQUIRE_ENCODING(coding)				\
  ((coding)->src_multibyte					\
   || (coding)->common_flags & CODING_REQUIRE_ENCODING_MASK	\
   || (coding)->mode & CODING_MODE_SELECTIVE_DISPLAY)


/* Return 1 if the coding context CODING requires some kind of code
   detection.  */
#define CODING_REQUIRE_DETECTION(coding) \
  ((coding)->common_flags & CODING_REQUIRE_DETECTION_MASK)

/* Return 1 if the coding context CODING requires code conversion on
   decoding or some kind of code detection.  */
#define CODING_MAY_REQUIRE_DECODING(coding)	\
  (CODING_REQUIRE_DECODING (coding)		\
   || CODING_REQUIRE_DETECTION (coding))

/* Macros to decode or encode a character of JISX0208 in SJIS.  S1 and
   S2 are the 1st and 2nd position-codes of JISX0208 in SJIS coding
   system.  C1 and C2 are the 1st and 2nd position codes of Emacs'
   internal format.  */

#define SJIS_TO_JIS(code)				\
  do {							\
    int s1, s2, j1, j2;					\
							\
    s1 = (code) >> 8, s2 = (code) & 0xFF;		\
							\
    if (s2 >= 0x9F)					\
      (j1 = s1 * 2 - (s1 >= 0xE0 ? 0x160 : 0xE0),	\
       j2 = s2 - 0x7E);					\
    else						\
      (j1 = s1 * 2 - ((s1 >= 0xE0) ? 0x161 : 0xE1),	\
       j2 = s2 - ((s2 >= 0x7F) ? 0x20 : 0x1F));		\
    (code) = (j1 << 8) | j2;				\
  } while (0)

#define SJIS_TO_JIS2(code)				\
  do {							\
    int s1, s2, j1, j2;					\
							\
    s1 = (code) >> 8, s2 = (code) & 0xFF;		\
							\
    if (s2 >= 0x9F)					\
      {							\
	j1 = (s1 == 0xF0 ? 0x28				\
	      : s1 == 0xF1 ? 0x24			\
	      : s1 == 0xF2 ? 0x2C			\
	      : s1 == 0xF3 ? 0x2E			\
	      : 0x6E + (s1 - 0xF4) * 2);		\
	j2 = s2 - 0x7E;					\
      }							\
    else						\
      {							\
	j1 = (s1 <= 0xF2 ? 0x21 + (s1 - 0xF0) * 2	\
	      : s1 <= 0xF4 ? 0x2D + (s1 - 0xF3) * 2	\
	      : 0x6F + (s1 - 0xF5) * 2);		\
	j2 = s2 - ((s2 >= 0x7F ? 0x20 : 0x1F));		\
      }							\
    (code) = (j1 << 8) | j2;				\
  } while (0)


#define JIS_TO_SJIS(code)				\
  do {							\
    int s1, s2, j1, j2;					\
							\
    j1 = (code) >> 8, j2 = (code) & 0xFF;		\
    if (j1 & 1)						\
      (s1 = j1 / 2 + ((j1 < 0x5F) ? 0x71 : 0xB1),	\
       s2 = j2 + ((j2 >= 0x60) ? 0x20 : 0x1F));		\
    else						\
      (s1 = j1 / 2 + ((j1 < 0x5F) ? 0x70 : 0xB0),	\
       s2 = j2 + 0x7E);					\
    (code) = (s1 << 8) | s2;				\
  } while (0)

#define JIS_TO_SJIS2(code)				\
  do {							\
    int s1, s2, j1, j2;					\
							\
    j1 = (code) >> 8, j2 = (code) & 0xFF;		\
    if (j1 & 1)						\
      {							\
	s1 = (j1 <= 0x25 ? 0xF0 + (j1 - 0x21) / 2	\
	      : j1 <= 0x2F ? 0xF3 + (j1 - 0x2D) / 2	\
	      : 0xF5 + (j1 - 0x6F) / 2);		\
	s2 = j2 + ((j2 >= 0x60) ? 0x20 : 0x1F);		\
      }							\
    else						\
      {							\
	s1 = (j1 == 0x28 ? 0xF0				\
	      : j1 == 0x24 ? 0xF1			\
	      : j1 == 0x2C ? 0xF2			\
	      : j1 == 0x2E ? 0xF3			\
	      : 0xF4 + (j1 - 0x6E) / 2);		\
	s2 = j2 + 0x7E;					\
      }							\
    (code) = (s1 << 8) | s2;				\
  } while (0)

/* Encode the file name NAME using the specified coding system
   for file names, if any.  */
#define ENCODE_FILE(name)						   \
  (! NILP (Vfile_name_coding_system)					   \
   && !EQ (Vfile_name_coding_system, make_number (0))			   \
   ? code_convert_string_norecord (name, Vfile_name_coding_system, 1)	   \
   : (! NILP (Vdefault_file_name_coding_system)				   \
      && !EQ (Vdefault_file_name_coding_system, make_number (0))	   \
      ? code_convert_string_norecord (name, Vdefault_file_name_coding_system, 1) \
      : name))


/* Decode the file name NAME using the specified coding system
   for file names, if any.  */
#define DECODE_FILE(name)						   \
  (! NILP (Vfile_name_coding_system)					   \
   && !EQ (Vfile_name_coding_system, make_number (0))			   \
   ? code_convert_string_norecord (name, Vfile_name_coding_system, 0)	   \
   : (! NILP (Vdefault_file_name_coding_system)				   \
      && !EQ (Vdefault_file_name_coding_system, make_number (0))	   \
      ? code_convert_string_norecord (name, Vdefault_file_name_coding_system, 0) \
      : name))


/* Encode the string STR using the specified coding system
   for system functions, if any.  */
#define ENCODE_SYSTEM(str)						   \
  (! NILP (Vlocale_coding_system)					   \
   && !EQ (Vlocale_coding_system, make_number (0))			   \
   ? code_convert_string_norecord (str, Vlocale_coding_system, 1)	   \
   : str)

/* Decode the string STR using the specified coding system
   for system functions, if any.  */
#define DECODE_SYSTEM(str)						   \
  (! NILP (Vlocale_coding_system)					   \
   && !EQ (Vlocale_coding_system, make_number (0))			   \
   ? code_convert_string_norecord (str, Vlocale_coding_system, 0)	   \
   : str)

/* Note that this encodes utf-8, not utf-8-emacs, so it's not a no-op.  */
#define ENCODE_UTF_8(str) code_convert_string_norecord (str, Qutf_8, 1)

/* Extern declarations.  */
extern Lisp_Object code_conversion_save (int, int);
extern int decoding_buffer_size (struct coding_system *, int);
extern int encoding_buffer_size (struct coding_system *, int);
extern void setup_coding_system (Lisp_Object, struct coding_system *);
extern Lisp_Object coding_charset_list (struct coding_system *);
extern Lisp_Object coding_system_charset_list (Lisp_Object);
extern Lisp_Object code_convert_string (Lisp_Object, Lisp_Object,
                                        Lisp_Object, int, int, int);
extern Lisp_Object code_convert_string_norecord (Lisp_Object, Lisp_Object,
                                                 int);
extern Lisp_Object raw_text_coding_system (Lisp_Object);
extern Lisp_Object coding_inherit_eol_type (Lisp_Object, Lisp_Object);
extern Lisp_Object complement_process_encoding_system (Lisp_Object);

extern int decode_coding_gap (struct coding_system *,
                              EMACS_INT, EMACS_INT);
extern void decode_coding_object (struct coding_system *,
                                  Lisp_Object, EMACS_INT, EMACS_INT,
                                  EMACS_INT, EMACS_INT, Lisp_Object);
extern void encode_coding_object (struct coding_system *,
                                  Lisp_Object, EMACS_INT, EMACS_INT,
                                  EMACS_INT, EMACS_INT, Lisp_Object);

/* Macros for backward compatibility.  */

#define decode_coding_region(coding, from, to)		\
  decode_coding_object (coding, Fcurrent_buffer (),	\
			from, CHAR_TO_BYTE (from),	\
			to, CHAR_TO_BYTE (to), Fcurrent_buffer ())


#define encode_coding_region(coding, from, to)		\
  encode_coding_object (coding, Fcurrent_buffer (),	\
			from, CHAR_TO_BYTE (from),	\
			to, CHAR_TO_BYTE (to), Fcurrent_buffer ())


#define decode_coding_string(coding, string, nocopy)			\
  decode_coding_object (coding, string, 0, 0, SCHARS (string),		\
			SBYTES (string), Qt)

#define encode_coding_string(coding, string, nocopy)			\
  (STRING_MULTIBYTE(string) ?						\
    (encode_coding_object (coding, string, 0, 0, SCHARS (string),	\
			   SBYTES (string), Qt),			\
     (coding)->dst_object) : (string))


#define decode_coding_c_string(coding, src, bytes, dst_object)		\
  do {									\
    (coding)->source = (src);						\
    (coding)->src_chars = (coding)->src_bytes = (bytes);		\
    decode_coding_object ((coding), Qnil, 0, 0, (bytes), (bytes),	\
			  (dst_object));				\
  } while (0)


extern Lisp_Object preferred_coding_system (void);


extern Lisp_Object Qutf_8, Qutf_8_emacs;

extern Lisp_Object Qcoding_category_index;
extern Lisp_Object Qcoding_system_p;
extern Lisp_Object Qraw_text, Qemacs_mule, Qno_conversion, Qundecided;
extern Lisp_Object Qbuffer_file_coding_system;

extern Lisp_Object Qunix, Qdos, Qmac;

extern Lisp_Object Qtranslation_table;
extern Lisp_Object Qtranslation_table_id;

#ifdef emacs
extern Lisp_Object Qfile_coding_system;
extern Lisp_Object Qcall_process, Qcall_process_region;
extern Lisp_Object Qstart_process, Qopen_network_stream;
extern Lisp_Object Qwrite_region;

extern char *emacs_strerror (int);

/* Coding system to be used to encode text for terminal display when
   terminal coding system is nil.  */
extern struct coding_system safe_terminal_coding;

#endif

/* Error signaled when there's a problem with detecting coding system */
extern Lisp_Object Qcoding_system_error;

extern char emacs_mule_bytes[256];
extern int emacs_mule_string_char (unsigned char *);

#endif /* EMACS_CODING_H */
