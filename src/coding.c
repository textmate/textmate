/* Coding system handler (conversion, detection, etc).
   Copyright (C) 2001-2012 Free Software Foundation, Inc.
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

/*** TABLE OF CONTENTS ***

  0. General comments
  1. Preamble
  2. Emacs' internal format (emacs-utf-8) handlers
  3. UTF-8 handlers
  4. UTF-16 handlers
  5. Charset-base coding systems handlers
  6. emacs-mule (old Emacs' internal format) handlers
  7. ISO2022 handlers
  8. Shift-JIS and BIG5 handlers
  9. CCL handlers
  10. C library functions
  11. Emacs Lisp library functions
  12. Postamble

*/

/*** 0. General comments ***


CODING SYSTEM

  A coding system is an object for an encoding mechanism that contains
  information about how to convert byte sequences to character
  sequences and vice versa.  When we say "decode", it means converting
  a byte sequence of a specific coding system into a character
  sequence that is represented by Emacs' internal coding system
  `emacs-utf-8', and when we say "encode", it means converting a
  character sequence of emacs-utf-8 to a byte sequence of a specific
  coding system.

  In Emacs Lisp, a coding system is represented by a Lisp symbol.  On
  the C level, a coding system is represented by a vector of attributes
  stored in the hash table Vcharset_hash_table.  The conversion from
  coding system symbol to attributes vector is done by looking up
  Vcharset_hash_table by the symbol.

  Coding systems are classified into the following types depending on
  the encoding mechanism.  Here's a brief description of the types.

  o UTF-8

  o UTF-16

  o Charset-base coding system

  A coding system defined by one or more (coded) character sets.
  Decoding and encoding are done by a code converter defined for each
  character set.

  o Old Emacs internal format (emacs-mule)

  The coding system adopted by old versions of Emacs (20 and 21).

  o ISO2022-base coding system

  The most famous coding system for multiple character sets.  X's
  Compound Text, various EUCs (Extended Unix Code), and coding systems
  used in the Internet communication such as ISO-2022-JP are all
  variants of ISO2022.

  o SJIS (or Shift-JIS or MS-Kanji-Code)

  A coding system to encode character sets: ASCII, JISX0201, and
  JISX0208.  Widely used for PC's in Japan.  Details are described in
  section 8.

  o BIG5

  A coding system to encode character sets: ASCII and Big5.  Widely
  used for Chinese (mainly in Taiwan and Hong Kong).  Details are
  described in section 8.  In this file, when we write "big5" (all
  lowercase), we mean the coding system, and when we write "Big5"
  (capitalized), we mean the character set.

  o CCL

  If a user wants to decode/encode text encoded in a coding system
  not listed above, he can supply a decoder and an encoder for it in
  CCL (Code Conversion Language) programs.  Emacs executes the CCL
  program while decoding/encoding.

  o Raw-text

  A coding system for text containing raw eight-bit data.  Emacs
  treats each byte of source text as a character (except for
  end-of-line conversion).

  o No-conversion

  Like raw text, but don't do end-of-line conversion.


END-OF-LINE FORMAT

  How text end-of-line is encoded depends on operating system.  For
  instance, Unix's format is just one byte of LF (line-feed) code,
  whereas DOS's format is two-byte sequence of `carriage-return' and
  `line-feed' codes.  MacOS's format is usually one byte of
  `carriage-return'.

  Since text character encoding and end-of-line encoding are
  independent, any coding system described above can take any format
  of end-of-line (except for no-conversion).

STRUCT CODING_SYSTEM

  Before using a coding system for code conversion (i.e. decoding and
  encoding), we setup a structure of type `struct coding_system'.
  This structure keeps various information about a specific code
  conversion (e.g. the location of source and destination data).

*/

/* COMMON MACROS */


/*** GENERAL NOTES on `detect_coding_XXX ()' functions ***

  These functions check if a byte sequence specified as a source in
  CODING conforms to the format of XXX, and update the members of
  DETECT_INFO.

  Return 1 if the byte sequence conforms to XXX, otherwise return 0.

  Below is the template of these functions.  */

#if 0
static int
detect_coding_XXX (struct coding_system *coding,
		   struct coding_detection_info *detect_info)
{
  const unsigned char *src = coding->source;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  EMACS_INT consumed_chars = 0;
  int found = 0;
  ...;

  while (1)
    {
      /* Get one byte from the source.  If the source is exhausted, jump
	 to no_more_source:.  */
      ONE_MORE_BYTE (c);

      if (! __C_conforms_to_XXX___ (c))
	break;
      if (! __C_strongly_suggests_XXX__ (c))
	found = CATEGORY_MASK_XXX;
    }
  /* The byte sequence is invalid for XXX.  */
  detect_info->rejected |= CATEGORY_MASK_XXX;
  return 0;

 no_more_source:
  /* The source exhausted successfully.  */
  detect_info->found |= found;
  return 1;
}
#endif

/*** GENERAL NOTES on `decode_coding_XXX ()' functions ***

  These functions decode a byte sequence specified as a source by
  CODING.  The resulting multibyte text goes to a place pointed to by
  CODING->charbuf, the length of which should not exceed
  CODING->charbuf_size;

  These functions set the information of original and decoded texts in
  CODING->consumed, CODING->consumed_char, and CODING->charbuf_used.
  They also set CODING->result to one of CODING_RESULT_XXX indicating
  how the decoding is finished.

  Below is the template of these functions.  */

#if 0
static void
decode_coding_XXXX (struct coding_system *coding)
{
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  /* SRC_BASE remembers the start position in source in each loop.
     The loop will be exited when there's not enough source code, or
     when there's no room in CHARBUF for a decoded character.  */
  const unsigned char *src_base;
  /* A buffer to produce decoded characters.  */
  int *charbuf = coding->charbuf + coding->charbuf_used;
  int *charbuf_end = coding->charbuf + coding->charbuf_size;
  int multibytep = coding->src_multibyte;

  while (1)
    {
      src_base = src;
      if (charbuf < charbuf_end)
	/* No more room to produce a decoded character.  */
	break;
      ONE_MORE_BYTE (c);
      /* Decode it. */
    }

 no_more_source:
  if (src_base < src_end
      && coding->mode & CODING_MODE_LAST_BLOCK)
    /* If the source ends by partial bytes to construct a character,
       treat them as eight-bit raw data.  */
    while (src_base < src_end && charbuf < charbuf_end)
      *charbuf++ = *src_base++;
  /* Remember how many bytes and characters we consumed.  If the
     source is multibyte, the bytes and chars are not identical.  */
  coding->consumed = coding->consumed_char = src_base - coding->source;
  /* Remember how many characters we produced.  */
  coding->charbuf_used = charbuf - coding->charbuf;
}
#endif

/*** GENERAL NOTES on `encode_coding_XXX ()' functions ***

  These functions encode SRC_BYTES length text at SOURCE of Emacs'
  internal multibyte format by CODING.  The resulting byte sequence
  goes to a place pointed to by DESTINATION, the length of which
  should not exceed DST_BYTES.

  These functions set the information of original and encoded texts in
  the members produced, produced_char, consumed, and consumed_char of
  the structure *CODING.  They also set the member result to one of
  CODING_RESULT_XXX indicating how the encoding finished.

  DST_BYTES zero means that source area and destination area are
  overlapped, which means that we can produce a encoded text until it
  reaches at the head of not-yet-encoded source text.

  Below is a template of these functions.  */
#if 0
static void
encode_coding_XXX (struct coding_system *coding)
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf->charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  unsigned char *adjusted_dst_end = dst_end - _MAX_BYTES_PRODUCED_IN_LOOP_;
  EMACS_INT produced_chars = 0;

  for (; charbuf < charbuf_end && dst < adjusted_dst_end; charbuf++)
    {
      int c = *charbuf;
      /* Encode C into DST, and increment DST.  */
    }
 label_no_more_destination:
  /* How many chars and bytes we produced.  */
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
}
#endif


/*** 1. Preamble ***/

#include <config.h>
#include <stdio.h>
#include <setjmp.h>

#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "charset.h"
#include "ccl.h"
#include "composite.h"
#include "coding.h"
#include "window.h"
#include "frame.h"
#include "termhooks.h"

Lisp_Object Vcoding_system_hash_table;

static Lisp_Object Qcoding_system, Qeol_type;
static Lisp_Object Qcoding_aliases;
Lisp_Object Qunix, Qdos;
Lisp_Object Qbuffer_file_coding_system;
static Lisp_Object Qpost_read_conversion, Qpre_write_conversion;
static Lisp_Object Qdefault_char;
Lisp_Object Qno_conversion, Qundecided;
Lisp_Object Qcharset, Qutf_8;
static Lisp_Object Qiso_2022;
static Lisp_Object Qutf_16, Qshift_jis, Qbig5;
static Lisp_Object Qbig, Qlittle;
static Lisp_Object Qcoding_system_history;
static Lisp_Object Qvalid_codes;
static Lisp_Object QCcategory, QCmnemonic, QCdefault_char;
static Lisp_Object QCdecode_translation_table, QCencode_translation_table;
static Lisp_Object QCpost_read_conversion, QCpre_write_conversion;
static Lisp_Object QCascii_compatible_p;

Lisp_Object Qcall_process, Qcall_process_region;
Lisp_Object Qstart_process, Qopen_network_stream;
static Lisp_Object Qtarget_idx;

static Lisp_Object Qinsufficient_source, Qinconsistent_eol, Qinvalid_source;
static Lisp_Object Qinterrupted, Qinsufficient_memory;

/* If a symbol has this property, evaluate the value to define the
   symbol as a coding system.  */
static Lisp_Object Qcoding_system_define_form;

/* Format of end-of-line decided by system.  This is Qunix on
   Unix and Mac, Qdos on DOS/Windows.
   This has an effect only for external encoding (i.e. for output to
   file and process), not for in-buffer or Lisp string encoding.  */
static Lisp_Object system_eol_type;

#ifdef emacs

Lisp_Object Qcoding_system_p, Qcoding_system_error;

/* Coding system emacs-mule and raw-text are for converting only
   end-of-line format.  */
Lisp_Object Qemacs_mule, Qraw_text;
Lisp_Object Qutf_8_emacs;

/* Coding-systems are handed between Emacs Lisp programs and C internal
   routines by the following three variables.  */
/* Coding system to be used to encode text for terminal display when
   terminal coding system is nil.  */
struct coding_system safe_terminal_coding;

#endif /* emacs */

Lisp_Object Qtranslation_table;
Lisp_Object Qtranslation_table_id;
static Lisp_Object Qtranslation_table_for_decode;
static Lisp_Object Qtranslation_table_for_encode;

/* Two special coding systems.  */
static Lisp_Object Vsjis_coding_system;
static Lisp_Object Vbig5_coding_system;

/* ISO2022 section */

#define CODING_ISO_INITIAL(coding, reg)			\
  (XINT (AREF (AREF (CODING_ID_ATTRS ((coding)->id),	\
		     coding_attr_iso_initial),		\
	       reg)))


#define CODING_ISO_REQUEST(coding, charset_id)		\
  (((charset_id) <= (coding)->max_charset_id		\
    ? ((coding)->safe_charsets[charset_id] != 255	\
       ? (coding)->safe_charsets[charset_id]		\
       : -1)						\
    : -1))


#define CODING_ISO_FLAGS(coding)	\
  ((coding)->spec.iso_2022.flags)
#define CODING_ISO_DESIGNATION(coding, reg)	\
  ((coding)->spec.iso_2022.current_designation[reg])
#define CODING_ISO_INVOCATION(coding, plane)	\
  ((coding)->spec.iso_2022.current_invocation[plane])
#define CODING_ISO_SINGLE_SHIFTING(coding)	\
  ((coding)->spec.iso_2022.single_shifting)
#define CODING_ISO_BOL(coding)	\
  ((coding)->spec.iso_2022.bol)
#define CODING_ISO_INVOKED_CHARSET(coding, plane)	\
  CODING_ISO_DESIGNATION ((coding), CODING_ISO_INVOCATION ((coding), (plane)))
#define CODING_ISO_CMP_STATUS(coding)	\
  (&(coding)->spec.iso_2022.cmp_status)
#define CODING_ISO_EXTSEGMENT_LEN(coding)	\
  ((coding)->spec.iso_2022.ctext_extended_segment_len)
#define CODING_ISO_EMBEDDED_UTF_8(coding)	\
  ((coding)->spec.iso_2022.embedded_utf_8)

/* Control characters of ISO2022.  */
			/* code */	/* function */
#define ISO_CODE_SO	0x0E		/* shift-out */
#define ISO_CODE_SI	0x0F		/* shift-in */
#define ISO_CODE_SS2_7	0x19		/* single-shift-2 for 7-bit code */
#define ISO_CODE_ESC	0x1B		/* escape */
#define ISO_CODE_SS2	0x8E		/* single-shift-2 */
#define ISO_CODE_SS3	0x8F		/* single-shift-3 */
#define ISO_CODE_CSI	0x9B		/* control-sequence-introducer */

/* All code (1-byte) of ISO2022 is classified into one of the
   followings.  */
enum iso_code_class_type
  {
    ISO_control_0,		/* Control codes in the range
				   0x00..0x1F and 0x7F, except for the
				   following 5 codes.  */
    ISO_shift_out,		/* ISO_CODE_SO (0x0E) */
    ISO_shift_in,		/* ISO_CODE_SI (0x0F) */
    ISO_single_shift_2_7,	/* ISO_CODE_SS2_7 (0x19) */
    ISO_escape,			/* ISO_CODE_SO (0x1B) */
    ISO_control_1,		/* Control codes in the range
				   0x80..0x9F, except for the
				   following 3 codes.  */
    ISO_single_shift_2,		/* ISO_CODE_SS2 (0x8E) */
    ISO_single_shift_3,		/* ISO_CODE_SS3 (0x8F) */
    ISO_control_sequence_introducer, /* ISO_CODE_CSI (0x9B) */
    ISO_0x20_or_0x7F,		/* Codes of the values 0x20 or 0x7F.  */
    ISO_graphic_plane_0,	/* Graphic codes in the range 0x21..0x7E.  */
    ISO_0xA0_or_0xFF,		/* Codes of the values 0xA0 or 0xFF.  */
    ISO_graphic_plane_1		/* Graphic codes in the range 0xA1..0xFE.  */
  };

/** The macros CODING_ISO_FLAG_XXX defines a flag bit of the
    `iso-flags' attribute of an iso2022 coding system.  */

/* If set, produce long-form designation sequence (e.g. ESC $ ( A)
   instead of the correct short-form sequence (e.g. ESC $ A).  */
#define CODING_ISO_FLAG_LONG_FORM	0x0001

/* If set, reset graphic planes and registers at end-of-line to the
   initial state.  */
#define CODING_ISO_FLAG_RESET_AT_EOL	0x0002

/* If set, reset graphic planes and registers before any control
   characters to the initial state.  */
#define CODING_ISO_FLAG_RESET_AT_CNTL	0x0004

/* If set, encode by 7-bit environment.  */
#define CODING_ISO_FLAG_SEVEN_BITS	0x0008

/* If set, use locking-shift function.  */
#define CODING_ISO_FLAG_LOCKING_SHIFT	0x0010

/* If set, use single-shift function.  Overwrite
   CODING_ISO_FLAG_LOCKING_SHIFT.  */
#define CODING_ISO_FLAG_SINGLE_SHIFT	0x0020

/* If set, use designation escape sequence.  */
#define CODING_ISO_FLAG_DESIGNATION	0x0040

/* If set, produce revision number sequence.  */
#define CODING_ISO_FLAG_REVISION	0x0080

/* If set, produce ISO6429's direction specifying sequence.  */
#define CODING_ISO_FLAG_DIRECTION	0x0100

/* If set, assume designation states are reset at beginning of line on
   output.  */
#define CODING_ISO_FLAG_INIT_AT_BOL	0x0200

/* If set, designation sequence should be placed at beginning of line
   on output.  */
#define CODING_ISO_FLAG_DESIGNATE_AT_BOL 0x0400

/* If set, do not encode unsafe characters on output.  */
#define CODING_ISO_FLAG_SAFE		0x0800

/* If set, extra latin codes (128..159) are accepted as a valid code
   on input.  */
#define CODING_ISO_FLAG_LATIN_EXTRA	0x1000

#define CODING_ISO_FLAG_COMPOSITION	0x2000

/* #define CODING_ISO_FLAG_EUC_TW_SHIFT	0x4000 */

#define CODING_ISO_FLAG_USE_ROMAN	0x8000

#define CODING_ISO_FLAG_USE_OLDJIS	0x10000

#define CODING_ISO_FLAG_FULL_SUPPORT	0x100000

/* A character to be produced on output if encoding of the original
   character is prohibited by CODING_ISO_FLAG_SAFE.  */
#define CODING_INHIBIT_CHARACTER_SUBSTITUTION  '?'

/* UTF-8 section */
#define CODING_UTF_8_BOM(coding)	\
  ((coding)->spec.utf_8_bom)

/* UTF-16 section */
#define CODING_UTF_16_BOM(coding)	\
  ((coding)->spec.utf_16.bom)

#define CODING_UTF_16_ENDIAN(coding)	\
  ((coding)->spec.utf_16.endian)

#define CODING_UTF_16_SURROGATE(coding)	\
  ((coding)->spec.utf_16.surrogate)


/* CCL section */
#define CODING_CCL_DECODER(coding)	\
  AREF (CODING_ID_ATTRS ((coding)->id), coding_attr_ccl_decoder)
#define CODING_CCL_ENCODER(coding)	\
  AREF (CODING_ID_ATTRS ((coding)->id), coding_attr_ccl_encoder)
#define CODING_CCL_VALIDS(coding)					   \
  (SDATA (AREF (CODING_ID_ATTRS ((coding)->id), coding_attr_ccl_valids)))

/* Index for each coding category in `coding_categories' */

enum coding_category
  {
    coding_category_iso_7,
    coding_category_iso_7_tight,
    coding_category_iso_8_1,
    coding_category_iso_8_2,
    coding_category_iso_7_else,
    coding_category_iso_8_else,
    coding_category_utf_8_auto,
    coding_category_utf_8_nosig,
    coding_category_utf_8_sig,
    coding_category_utf_16_auto,
    coding_category_utf_16_be,
    coding_category_utf_16_le,
    coding_category_utf_16_be_nosig,
    coding_category_utf_16_le_nosig,
    coding_category_charset,
    coding_category_sjis,
    coding_category_big5,
    coding_category_ccl,
    coding_category_emacs_mule,
    /* All above are targets of code detection.  */
    coding_category_raw_text,
    coding_category_undecided,
    coding_category_max
  };

/* Definitions of flag bits used in detect_coding_XXXX.  */
#define CATEGORY_MASK_ISO_7		(1 << coding_category_iso_7)
#define CATEGORY_MASK_ISO_7_TIGHT	(1 << coding_category_iso_7_tight)
#define CATEGORY_MASK_ISO_8_1		(1 << coding_category_iso_8_1)
#define CATEGORY_MASK_ISO_8_2		(1 << coding_category_iso_8_2)
#define CATEGORY_MASK_ISO_7_ELSE	(1 << coding_category_iso_7_else)
#define CATEGORY_MASK_ISO_8_ELSE	(1 << coding_category_iso_8_else)
#define CATEGORY_MASK_UTF_8_AUTO	(1 << coding_category_utf_8_auto)
#define CATEGORY_MASK_UTF_8_NOSIG	(1 << coding_category_utf_8_nosig)
#define CATEGORY_MASK_UTF_8_SIG		(1 << coding_category_utf_8_sig)
#define CATEGORY_MASK_UTF_16_AUTO	(1 << coding_category_utf_16_auto)
#define CATEGORY_MASK_UTF_16_BE		(1 << coding_category_utf_16_be)
#define CATEGORY_MASK_UTF_16_LE		(1 << coding_category_utf_16_le)
#define CATEGORY_MASK_UTF_16_BE_NOSIG	(1 << coding_category_utf_16_be_nosig)
#define CATEGORY_MASK_UTF_16_LE_NOSIG	(1 << coding_category_utf_16_le_nosig)
#define CATEGORY_MASK_CHARSET		(1 << coding_category_charset)
#define CATEGORY_MASK_SJIS		(1 << coding_category_sjis)
#define CATEGORY_MASK_BIG5		(1 << coding_category_big5)
#define CATEGORY_MASK_CCL		(1 << coding_category_ccl)
#define CATEGORY_MASK_EMACS_MULE	(1 << coding_category_emacs_mule)
#define CATEGORY_MASK_RAW_TEXT		(1 << coding_category_raw_text)

/* This value is returned if detect_coding_mask () find nothing other
   than ASCII characters.  */
#define CATEGORY_MASK_ANY		\
  (CATEGORY_MASK_ISO_7			\
   | CATEGORY_MASK_ISO_7_TIGHT		\
   | CATEGORY_MASK_ISO_8_1		\
   | CATEGORY_MASK_ISO_8_2		\
   | CATEGORY_MASK_ISO_7_ELSE		\
   | CATEGORY_MASK_ISO_8_ELSE		\
   | CATEGORY_MASK_UTF_8_AUTO		\
   | CATEGORY_MASK_UTF_8_NOSIG		\
   | CATEGORY_MASK_UTF_8_SIG		\
   | CATEGORY_MASK_UTF_16_AUTO		\
   | CATEGORY_MASK_UTF_16_BE		\
   | CATEGORY_MASK_UTF_16_LE		\
   | CATEGORY_MASK_UTF_16_BE_NOSIG	\
   | CATEGORY_MASK_UTF_16_LE_NOSIG	\
   | CATEGORY_MASK_CHARSET		\
   | CATEGORY_MASK_SJIS			\
   | CATEGORY_MASK_BIG5			\
   | CATEGORY_MASK_CCL			\
   | CATEGORY_MASK_EMACS_MULE)


#define CATEGORY_MASK_ISO_7BIT \
  (CATEGORY_MASK_ISO_7 | CATEGORY_MASK_ISO_7_TIGHT)

#define CATEGORY_MASK_ISO_8BIT \
  (CATEGORY_MASK_ISO_8_1 | CATEGORY_MASK_ISO_8_2)

#define CATEGORY_MASK_ISO_ELSE \
  (CATEGORY_MASK_ISO_7_ELSE | CATEGORY_MASK_ISO_8_ELSE)

#define CATEGORY_MASK_ISO_ESCAPE	\
  (CATEGORY_MASK_ISO_7			\
   | CATEGORY_MASK_ISO_7_TIGHT		\
   | CATEGORY_MASK_ISO_7_ELSE		\
   | CATEGORY_MASK_ISO_8_ELSE)

#define CATEGORY_MASK_ISO	\
  (  CATEGORY_MASK_ISO_7BIT	\
     | CATEGORY_MASK_ISO_8BIT	\
     | CATEGORY_MASK_ISO_ELSE)

#define CATEGORY_MASK_UTF_16		\
  (CATEGORY_MASK_UTF_16_AUTO		\
   | CATEGORY_MASK_UTF_16_BE		\
   | CATEGORY_MASK_UTF_16_LE		\
   | CATEGORY_MASK_UTF_16_BE_NOSIG	\
   | CATEGORY_MASK_UTF_16_LE_NOSIG)

#define CATEGORY_MASK_UTF_8	\
  (CATEGORY_MASK_UTF_8_AUTO	\
   | CATEGORY_MASK_UTF_8_NOSIG	\
   | CATEGORY_MASK_UTF_8_SIG)

/* Table of coding categories (Lisp symbols).  This variable is for
   internal use only.  */
static Lisp_Object Vcoding_category_table;

/* Table of coding-categories ordered by priority.  */
static enum coding_category coding_priorities[coding_category_max];

/* Nth element is a coding context for the coding system bound to the
   Nth coding category.  */
static struct coding_system coding_categories[coding_category_max];

/*** Commonly used macros and functions ***/

#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif
#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif

#define CODING_GET_INFO(coding, attrs, charset_list)	\
  do {							\
    (attrs) = CODING_ID_ATTRS ((coding)->id);		\
    (charset_list) = CODING_ATTR_CHARSET_LIST (attrs);	\
  } while (0)


/* Safely get one byte from the source text pointed by SRC which ends
   at SRC_END, and set C to that byte.  If there are not enough bytes
   in the source, it jumps to `no_more_source'.  If multibytep is
   nonzero, and a multibyte character is found at SRC, set C to the
   negative value of the character code.  The caller should declare
   and set these variables appropriately in advance:
	src, src_end, multibytep */

#define ONE_MORE_BYTE(c)				\
  do {							\
    if (src == src_end)					\
      {							\
	if (src_base < src)				\
	  record_conversion_result			\
	    (coding, CODING_RESULT_INSUFFICIENT_SRC);	\
	goto no_more_source;				\
      }							\
    c = *src++;						\
    if (multibytep && (c & 0x80))			\
      {							\
	if ((c & 0xFE) == 0xC0)				\
	  c = ((c & 1) << 6) | *src++;			\
	else						\
	  {						\
	    src--;					\
	    c = - string_char (src, &src, NULL);	\
	    record_conversion_result			\
	      (coding, CODING_RESULT_INVALID_SRC);	\
	  }						\
      }							\
    consumed_chars++;					\
  } while (0)

/* Safely get two bytes from the source text pointed by SRC which ends
   at SRC_END, and set C1 and C2 to those bytes while skipping the
   heading multibyte characters.  If there are not enough bytes in the
   source, it jumps to `no_more_source'.  If multibytep is nonzero and
   a multibyte character is found for C2, set C2 to the negative value
   of the character code.  The caller should declare and set these
   variables appropriately in advance:
	src, src_end, multibytep
   It is intended that this macro is used in detect_coding_utf_16.  */

#define TWO_MORE_BYTES(c1, c2)				\
  do {							\
    do {						\
      if (src == src_end)				\
	goto no_more_source;				\
      c1 = *src++;					\
      if (multibytep && (c1 & 0x80))			\
	{						\
	  if ((c1 & 0xFE) == 0xC0)			\
	    c1 = ((c1 & 1) << 6) | *src++;		\
	  else						\
	    {						\
	      src += BYTES_BY_CHAR_HEAD (c1) - 1;	\
	      c1 = -1;					\
	    }						\
	}						\
    } while (c1 < 0);					\
    if (src == src_end)					\
      goto no_more_source;				\
    c2 = *src++;					\
    if (multibytep && (c2 & 0x80))			\
      {							\
	if ((c2 & 0xFE) == 0xC0)			\
	  c2 = ((c2 & 1) << 6) | *src++;		\
	else						\
	  c2 = -1;					\
      }							\
  } while (0)


/* Store a byte C in the place pointed by DST and increment DST to the
   next free point, and increment PRODUCED_CHARS.  The caller should
   assure that C is 0..127, and declare and set the variable `dst'
   appropriately in advance.
*/


#define EMIT_ONE_ASCII_BYTE(c)	\
  do {				\
    produced_chars++;		\
    *dst++ = (c);		\
  } while (0)


/* Like EMIT_ONE_ASCII_BYTE but store two bytes; C1 and C2.  */

#define EMIT_TWO_ASCII_BYTES(c1, c2)	\
  do {					\
    produced_chars += 2;		\
    *dst++ = (c1), *dst++ = (c2);	\
  } while (0)


/* Store a byte C in the place pointed by DST and increment DST to the
   next free point, and increment PRODUCED_CHARS.  If MULTIBYTEP is
   nonzero, store in an appropriate multibyte from.  The caller should
   declare and set the variables `dst' and `multibytep' appropriately
   in advance.  */

#define EMIT_ONE_BYTE(c)		\
  do {					\
    produced_chars++;			\
    if (multibytep)			\
      {					\
	unsigned ch = (c);		\
	if (ch >= 0x80)			\
	  ch = BYTE8_TO_CHAR (ch);	\
	CHAR_STRING_ADVANCE (ch, dst);	\
      }					\
    else				\
      *dst++ = (c);			\
  } while (0)


/* Like EMIT_ONE_BYTE, but emit two bytes; C1 and C2.  */

#define EMIT_TWO_BYTES(c1, c2)		\
  do {					\
    produced_chars += 2;		\
    if (multibytep)			\
      {					\
	unsigned ch;			\
					\
	ch = (c1);			\
	if (ch >= 0x80)			\
	  ch = BYTE8_TO_CHAR (ch);	\
	CHAR_STRING_ADVANCE (ch, dst);	\
	ch = (c2);			\
	if (ch >= 0x80)			\
	  ch = BYTE8_TO_CHAR (ch);	\
	CHAR_STRING_ADVANCE (ch, dst);	\
      }					\
    else				\
      {					\
	*dst++ = (c1);			\
	*dst++ = (c2);			\
      }					\
  } while (0)


#define EMIT_THREE_BYTES(c1, c2, c3)	\
  do {					\
    EMIT_ONE_BYTE (c1);			\
    EMIT_TWO_BYTES (c2, c3);		\
  } while (0)


#define EMIT_FOUR_BYTES(c1, c2, c3, c4)		\
  do {						\
    EMIT_TWO_BYTES (c1, c2);			\
    EMIT_TWO_BYTES (c3, c4);			\
  } while (0)


/* Prototypes for static functions.  */
static void record_conversion_result (struct coding_system *coding,
                                      enum coding_result_code result);
static int detect_coding_utf_8 (struct coding_system *,
                                struct coding_detection_info *info);
static void decode_coding_utf_8 (struct coding_system *);
static int encode_coding_utf_8 (struct coding_system *);

static int detect_coding_utf_16 (struct coding_system *,
                                 struct coding_detection_info *info);
static void decode_coding_utf_16 (struct coding_system *);
static int encode_coding_utf_16 (struct coding_system *);

static int detect_coding_iso_2022 (struct coding_system *,
                                   struct coding_detection_info *info);
static void decode_coding_iso_2022 (struct coding_system *);
static int encode_coding_iso_2022 (struct coding_system *);

static int detect_coding_emacs_mule (struct coding_system *,
                                     struct coding_detection_info *info);
static void decode_coding_emacs_mule (struct coding_system *);
static int encode_coding_emacs_mule (struct coding_system *);

static int detect_coding_sjis (struct coding_system *,
                               struct coding_detection_info *info);
static void decode_coding_sjis (struct coding_system *);
static int encode_coding_sjis (struct coding_system *);

static int detect_coding_big5 (struct coding_system *,
                               struct coding_detection_info *info);
static void decode_coding_big5 (struct coding_system *);
static int encode_coding_big5 (struct coding_system *);

static int detect_coding_ccl (struct coding_system *,
                              struct coding_detection_info *info);
static void decode_coding_ccl (struct coding_system *);
static int encode_coding_ccl (struct coding_system *);

static void decode_coding_raw_text (struct coding_system *);
static int encode_coding_raw_text (struct coding_system *);

static EMACS_INT coding_set_source (struct coding_system *);
static EMACS_INT coding_set_destination (struct coding_system *);
static void coding_alloc_by_realloc (struct coding_system *, EMACS_INT);
static void coding_alloc_by_making_gap (struct coding_system *,
                                        EMACS_INT, EMACS_INT);
static unsigned char *alloc_destination (struct coding_system *,
                                         EMACS_INT, unsigned char *);
static void setup_iso_safe_charsets (Lisp_Object);
static EMACS_INT encode_designation_at_bol (struct coding_system *,
				      int *, int *, unsigned char *);
static int detect_eol (const unsigned char *,
                       EMACS_INT, enum coding_category);
static Lisp_Object adjust_coding_eol_type (struct coding_system *, int);
static void decode_eol (struct coding_system *);
static Lisp_Object get_translation_table (Lisp_Object, int, int *);
static Lisp_Object get_translation (Lisp_Object, int *, int *);
static int produce_chars (struct coding_system *, Lisp_Object, int);
static inline void produce_charset (struct coding_system *, int *,
                                    EMACS_INT);
static void produce_annotation (struct coding_system *, EMACS_INT);
static int decode_coding (struct coding_system *);
static inline int *handle_composition_annotation (EMACS_INT, EMACS_INT,
                                                  struct coding_system *,
                                                  int *, EMACS_INT *);
static inline int *handle_charset_annotation (EMACS_INT, EMACS_INT,
                                              struct coding_system *,
                                              int *, EMACS_INT *);
static void consume_chars (struct coding_system *, Lisp_Object, int);
static int encode_coding (struct coding_system *);
static Lisp_Object make_conversion_work_buffer (int);
static Lisp_Object code_conversion_restore (Lisp_Object);
static inline int char_encodable_p (int, Lisp_Object);
static Lisp_Object make_subsidiaries (Lisp_Object);

static void
record_conversion_result (struct coding_system *coding,
			  enum coding_result_code result)
{
  coding->result = result;
  switch (result)
    {
    case CODING_RESULT_INSUFFICIENT_SRC:
      Vlast_code_conversion_error = Qinsufficient_source;
      break;
    case CODING_RESULT_INCONSISTENT_EOL:
      Vlast_code_conversion_error = Qinconsistent_eol;
      break;
    case CODING_RESULT_INVALID_SRC:
      Vlast_code_conversion_error = Qinvalid_source;
      break;
    case CODING_RESULT_INTERRUPT:
      Vlast_code_conversion_error = Qinterrupted;
      break;
    case CODING_RESULT_INSUFFICIENT_MEM:
      Vlast_code_conversion_error = Qinsufficient_memory;
      break;
    case CODING_RESULT_INSUFFICIENT_DST:
      /* Don't record this error in Vlast_code_conversion_error
	 because it happens just temporarily and is resolved when the
	 whole conversion is finished.  */
      break;
    case CODING_RESULT_SUCCESS:
      break;
    default:
      Vlast_code_conversion_error = intern ("Unknown error");
    }
}

/* These wrapper macros are used to preserve validity of pointers into
   buffer text across calls to decode_char, encode_char, etc, which
   could cause relocation of buffers if it loads a charset map,
   because loading a charset map allocates large structures.  */

#define CODING_DECODE_CHAR(coding, src, src_base, src_end, charset, code, c) \
  do {									     \
    EMACS_INT offset;							     \
									     \
    charset_map_loaded = 0;						     \
    c = DECODE_CHAR (charset, code);					     \
    if (charset_map_loaded						     \
	&& (offset = coding_set_source (coding)))			     \
      {									     \
	src += offset;							     \
	src_base += offset;						     \
	src_end += offset;						     \
      }									     \
  } while (0)

#define CODING_ENCODE_CHAR(coding, dst, dst_end, charset, c, code)	\
  do {									\
    EMACS_INT offset;							\
									\
    charset_map_loaded = 0;						\
    code = ENCODE_CHAR (charset, c);					\
    if (charset_map_loaded						\
	&& (offset = coding_set_destination (coding)))			\
      {									\
	dst += offset;							\
	dst_end += offset;						\
      }									\
  } while (0)

#define CODING_CHAR_CHARSET(coding, dst, dst_end, c, charset_list, code_return, charset) \
  do {									\
    EMACS_INT offset;							\
									\
    charset_map_loaded = 0;						\
    charset = char_charset (c, charset_list, code_return);		\
    if (charset_map_loaded						\
	&& (offset = coding_set_destination (coding)))			\
      {									\
	dst += offset;							\
	dst_end += offset;						\
      }									\
  } while (0)

#define CODING_CHAR_CHARSET_P(coding, dst, dst_end, c, charset, result)	\
  do {									\
    EMACS_INT offset;							\
									\
    charset_map_loaded = 0;						\
    result = CHAR_CHARSET_P (c, charset);				\
    if (charset_map_loaded						\
	&& (offset = coding_set_destination (coding)))			\
      {									\
	dst += offset;							\
	dst_end += offset;						\
      }									\
  } while (0)


/* If there are at least BYTES length of room at dst, allocate memory
   for coding->destination and update dst and dst_end.  We don't have
   to take care of coding->source which will be relocated.  It is
   handled by calling coding_set_source in encode_coding.  */

#define ASSURE_DESTINATION(bytes)				\
  do {								\
    if (dst + (bytes) >= dst_end)				\
      {								\
	EMACS_INT more_bytes = charbuf_end - charbuf + (bytes);	\
								\
	dst = alloc_destination (coding, more_bytes, dst);	\
	dst_end = coding->destination + coding->dst_bytes;	\
      }								\
  } while (0)


/* Store multibyte form of the character C in P, and advance P to the
   end of the multibyte form.  This is like CHAR_STRING_ADVANCE but it
   never calls MAYBE_UNIFY_CHAR.  */

#define CHAR_STRING_ADVANCE_NO_UNIFY(c, p)	\
  do {						\
    if ((c) <= MAX_1_BYTE_CHAR)			\
      *(p)++ = (c);				\
    else if ((c) <= MAX_2_BYTE_CHAR)		\
      *(p)++ = (0xC0 | ((c) >> 6)),		\
	*(p)++ = (0x80 | ((c) & 0x3F));		\
    else if ((c) <= MAX_3_BYTE_CHAR)		\
      *(p)++ = (0xE0 | ((c) >> 12)),		\
	*(p)++ = (0x80 | (((c) >> 6) & 0x3F)),	\
	*(p)++ = (0x80 | ((c) & 0x3F));		\
    else if ((c) <= MAX_4_BYTE_CHAR)		\
      *(p)++ = (0xF0 | (c >> 18)),		\
	*(p)++ = (0x80 | ((c >> 12) & 0x3F)),	\
	*(p)++ = (0x80 | ((c >> 6) & 0x3F)),	\
	*(p)++ = (0x80 | (c & 0x3F));		\
    else if ((c) <= MAX_5_BYTE_CHAR)		\
      *(p)++ = 0xF8,				\
	*(p)++ = (0x80 | ((c >> 18) & 0x0F)),	\
	*(p)++ = (0x80 | ((c >> 12) & 0x3F)),	\
	*(p)++ = (0x80 | ((c >> 6) & 0x3F)),	\
	*(p)++ = (0x80 | (c & 0x3F));		\
    else					\
      (p) += BYTE8_STRING ((c) - 0x3FFF80, p);	\
  } while (0)


/* Return the character code of character whose multibyte form is at
   P, and advance P to the end of the multibyte form.  This is like
   STRING_CHAR_ADVANCE, but it never calls MAYBE_UNIFY_CHAR.  */

#define STRING_CHAR_ADVANCE_NO_UNIFY(p)				\
  (!((p)[0] & 0x80)						\
   ? *(p)++							\
   : ! ((p)[0] & 0x20)						\
   ? ((p) += 2,							\
      ((((p)[-2] & 0x1F) << 6)					\
       | ((p)[-1] & 0x3F)					\
       | ((unsigned char) ((p)[-2]) < 0xC2 ? 0x3FFF80 : 0)))	\
   : ! ((p)[0] & 0x10)						\
   ? ((p) += 3,							\
      ((((p)[-3] & 0x0F) << 12)					\
       | (((p)[-2] & 0x3F) << 6)				\
       | ((p)[-1] & 0x3F)))					\
   : ! ((p)[0] & 0x08)						\
   ? ((p) += 4,							\
      ((((p)[-4] & 0xF) << 18)					\
       | (((p)[-3] & 0x3F) << 12)				\
       | (((p)[-2] & 0x3F) << 6)				\
       | ((p)[-1] & 0x3F)))					\
   : ((p) += 5,							\
      ((((p)[-4] & 0x3F) << 18)					\
       | (((p)[-3] & 0x3F) << 12)				\
       | (((p)[-2] & 0x3F) << 6)				\
       | ((p)[-1] & 0x3F))))


/* Update coding->source from coding->src_object, and return how many
   bytes coding->source was changed.  */

static EMACS_INT
coding_set_source (struct coding_system *coding)
{
  const unsigned char *orig = coding->source;

  if (BUFFERP (coding->src_object))
    {
      struct buffer *buf = XBUFFER (coding->src_object);

      if (coding->src_pos < 0)
	coding->source = BUF_GAP_END_ADDR (buf) + coding->src_pos_byte;
      else
	coding->source = BUF_BYTE_ADDRESS (buf, coding->src_pos_byte);
    }
  else if (STRINGP (coding->src_object))
    {
      coding->source = SDATA (coding->src_object) + coding->src_pos_byte;
    }
  else
    {
      /* Otherwise, the source is C string and is never relocated
	 automatically.  Thus we don't have to update anything.  */
    }
  return coding->source - orig;
}


/* Update coding->destination from coding->dst_object, and return how
   many bytes coding->destination was changed.  */

static EMACS_INT
coding_set_destination (struct coding_system *coding)
{
  const unsigned char *orig = coding->destination;

  if (BUFFERP (coding->dst_object))
    {
      if (BUFFERP (coding->src_object) && coding->src_pos < 0)
	{
	  coding->destination = BEG_ADDR + coding->dst_pos_byte - BEG_BYTE;
	  coding->dst_bytes = (GAP_END_ADDR
			       - (coding->src_bytes - coding->consumed)
			       - coding->destination);
	}
      else
	{
	  /* We are sure that coding->dst_pos_byte is before the gap
	     of the buffer. */
	  coding->destination = (BUF_BEG_ADDR (XBUFFER (coding->dst_object))
				 + coding->dst_pos_byte - BEG_BYTE);
	  coding->dst_bytes = (BUF_GAP_END_ADDR (XBUFFER (coding->dst_object))
			       - coding->destination);
	}
    }
  else
    {
      /* Otherwise, the destination is C string and is never relocated
	 automatically.  Thus we don't have to update anything.  */
    }
  return coding->destination - orig;
}


static void
coding_alloc_by_realloc (struct coding_system *coding, EMACS_INT bytes)
{
  if (STRING_BYTES_BOUND - coding->dst_bytes < bytes)
    string_overflow ();
  coding->destination = (unsigned char *) xrealloc (coding->destination,
						    coding->dst_bytes + bytes);
  coding->dst_bytes += bytes;
}

static void
coding_alloc_by_making_gap (struct coding_system *coding,
			    EMACS_INT gap_head_used, EMACS_INT bytes)
{
  if (EQ (coding->src_object, coding->dst_object))
    {
      /* The gap may contain the produced data at the head and not-yet
	 consumed data at the tail.  To preserve those data, we at
	 first make the gap size to zero, then increase the gap
	 size.  */
      EMACS_INT add = GAP_SIZE;

      GPT += gap_head_used, GPT_BYTE += gap_head_used;
      GAP_SIZE = 0; ZV += add; Z += add; ZV_BYTE += add; Z_BYTE += add;
      make_gap (bytes);
      GAP_SIZE += add; ZV -= add; Z -= add; ZV_BYTE -= add; Z_BYTE -= add;
      GPT -= gap_head_used, GPT_BYTE -= gap_head_used;
    }
  else
    {
      Lisp_Object this_buffer;

      this_buffer = Fcurrent_buffer ();
      set_buffer_internal (XBUFFER (coding->dst_object));
      make_gap (bytes);
      set_buffer_internal (XBUFFER (this_buffer));
    }
}


static unsigned char *
alloc_destination (struct coding_system *coding, EMACS_INT nbytes,
		   unsigned char *dst)
{
  EMACS_INT offset = dst - coding->destination;

  if (BUFFERP (coding->dst_object))
    {
      struct buffer *buf = XBUFFER (coding->dst_object);

      coding_alloc_by_making_gap (coding, dst - BUF_GPT_ADDR (buf), nbytes);
    }
  else
    coding_alloc_by_realloc (coding, nbytes);
  coding_set_destination (coding);
  dst = coding->destination + offset;
  return dst;
}

/** Macros for annotations.  */

/* An annotation data is stored in the array coding->charbuf in this
   format:
     [ -LENGTH ANNOTATION_MASK NCHARS ... ]
   LENGTH is the number of elements in the annotation.
   ANNOTATION_MASK is one of CODING_ANNOTATE_XXX_MASK.
   NCHARS is the number of characters in the text annotated.

   The format of the following elements depend on ANNOTATION_MASK.

   In the case of CODING_ANNOTATE_COMPOSITION_MASK, these elements
   follows:
     ... NBYTES METHOD [ COMPOSITION-COMPONENTS ... ]

   NBYTES is the number of bytes specified in the header part of
   old-style emacs-mule encoding, or 0 for the other kind of
   composition.

   METHOD is one of enum composition_method.

   Optional COMPOSITION-COMPONENTS are characters and composition
   rules.

   In the case of CODING_ANNOTATE_CHARSET_MASK, one element CHARSET-ID
   follows.

   If ANNOTATION_MASK is 0, this annotation is just a space holder to
   recover from an invalid annotation, and should be skipped by
   produce_annotation.  */

/* Maximum length of the header of annotation data.  */
#define MAX_ANNOTATION_LENGTH 5

#define ADD_ANNOTATION_DATA(buf, len, mask, nchars)	\
  do {							\
    *(buf)++ = -(len);					\
    *(buf)++ = (mask);					\
    *(buf)++ = (nchars);				\
    coding->annotated = 1;				\
  } while (0);

#define ADD_COMPOSITION_DATA(buf, nchars, nbytes, method)		    \
  do {									    \
    ADD_ANNOTATION_DATA (buf, 5, CODING_ANNOTATE_COMPOSITION_MASK, nchars); \
    *buf++ = nbytes;							    \
    *buf++ = method;							    \
  } while (0)


#define ADD_CHARSET_DATA(buf, nchars, id)				\
  do {									\
    ADD_ANNOTATION_DATA (buf, 4, CODING_ANNOTATE_CHARSET_MASK, nchars);	\
    *buf++ = id;							\
  } while (0)


/*** 2. Emacs' internal format (emacs-utf-8) ***/




/*** 3. UTF-8 ***/

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in UTF-8.  If it is, return 1, else
   return 0.  */

#define UTF_8_1_OCTET_P(c)         ((c) < 0x80)
#define UTF_8_EXTRA_OCTET_P(c)     (((c) & 0xC0) == 0x80)
#define UTF_8_2_OCTET_LEADING_P(c) (((c) & 0xE0) == 0xC0)
#define UTF_8_3_OCTET_LEADING_P(c) (((c) & 0xF0) == 0xE0)
#define UTF_8_4_OCTET_LEADING_P(c) (((c) & 0xF8) == 0xF0)
#define UTF_8_5_OCTET_LEADING_P(c) (((c) & 0xFC) == 0xF8)

#define UTF_8_BOM_1 0xEF
#define UTF_8_BOM_2 0xBB
#define UTF_8_BOM_3 0xBF

static int
detect_coding_utf_8 (struct coding_system *coding,
		     struct coding_detection_info *detect_info)
{
  const unsigned char *src = coding->source, *src_base;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  EMACS_INT consumed_chars = 0;
  int bom_found = 0;
  int found = 0;

  detect_info->checked |= CATEGORY_MASK_UTF_8;
  /* A coding system of this category is always ASCII compatible.  */
  src += coding->head_ascii;

  while (1)
    {
      int c, c1, c2, c3, c4;

      src_base = src;
      ONE_MORE_BYTE (c);
      if (c < 0 || UTF_8_1_OCTET_P (c))
	continue;
      ONE_MORE_BYTE (c1);
      if (c1 < 0 || ! UTF_8_EXTRA_OCTET_P (c1))
	break;
      if (UTF_8_2_OCTET_LEADING_P (c))
	{
	  found = 1;
	  continue;
	}
      ONE_MORE_BYTE (c2);
      if (c2 < 0 || ! UTF_8_EXTRA_OCTET_P (c2))
	break;
      if (UTF_8_3_OCTET_LEADING_P (c))
	{
	  found = 1;
	  if (src_base == coding->source
	      && c == UTF_8_BOM_1 && c1 == UTF_8_BOM_2 && c2 == UTF_8_BOM_3)
	    bom_found = 1;
	  continue;
	}
      ONE_MORE_BYTE (c3);
      if (c3 < 0 || ! UTF_8_EXTRA_OCTET_P (c3))
	break;
      if (UTF_8_4_OCTET_LEADING_P (c))
	{
	  found = 1;
	  continue;
	}
      ONE_MORE_BYTE (c4);
      if (c4 < 0 || ! UTF_8_EXTRA_OCTET_P (c4))
	break;
      if (UTF_8_5_OCTET_LEADING_P (c))
	{
	  found = 1;
	  continue;
	}
      break;
    }
  detect_info->rejected |= CATEGORY_MASK_UTF_8;
  return 0;

 no_more_source:
  if (src_base < src && coding->mode & CODING_MODE_LAST_BLOCK)
    {
      detect_info->rejected |= CATEGORY_MASK_UTF_8;
      return 0;
    }
  if (bom_found)
    {
      /* The first character 0xFFFE doesn't necessarily mean a BOM.  */
      detect_info->found |= CATEGORY_MASK_UTF_8_SIG | CATEGORY_MASK_UTF_8_NOSIG;
    }
  else
    {
      detect_info->rejected |= CATEGORY_MASK_UTF_8_SIG;
      if (found)
	detect_info->found |= CATEGORY_MASK_UTF_8_NOSIG;
    }
  return 1;
}


static void
decode_coding_utf_8 (struct coding_system *coding)
{
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  const unsigned char *src_base;
  int *charbuf = coding->charbuf + coding->charbuf_used;
  int *charbuf_end = coding->charbuf + coding->charbuf_size;
  EMACS_INT consumed_chars = 0, consumed_chars_base = 0;
  int multibytep = coding->src_multibyte;
  enum utf_bom_type bom = CODING_UTF_8_BOM (coding);
  int eol_dos =
    !inhibit_eol_conversion && EQ (CODING_ID_EOL_TYPE (coding->id), Qdos);
  int byte_after_cr = -1;

  if (bom != utf_without_bom)
    {
      int c1, c2, c3;

      src_base = src;
      ONE_MORE_BYTE (c1);
      if (! UTF_8_3_OCTET_LEADING_P (c1))
	src = src_base;
      else
	{
	  ONE_MORE_BYTE (c2);
	  if (! UTF_8_EXTRA_OCTET_P (c2))
	    src = src_base;
	  else
	    {
	      ONE_MORE_BYTE (c3);
	      if (! UTF_8_EXTRA_OCTET_P (c3))
		src = src_base;
	      else
		{
		  if ((c1 != UTF_8_BOM_1)
		      || (c2 != UTF_8_BOM_2) || (c3 != UTF_8_BOM_3))
		    src = src_base;
		  else
		    CODING_UTF_8_BOM (coding) = utf_without_bom;
		}
	    }
	}
    }
  CODING_UTF_8_BOM (coding) = utf_without_bom;

  while (1)
    {
      int c, c1, c2, c3, c4, c5;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	{
	  if (byte_after_cr >= 0)
	    src_base--;
	  break;
	}

      if (byte_after_cr >= 0)
	c1 = byte_after_cr, byte_after_cr = -1;
      else
	ONE_MORE_BYTE (c1);
      if (c1 < 0)
	{
	  c = - c1;
	}
      else if (UTF_8_1_OCTET_P (c1))
	{
	  if (eol_dos && c1 == '\r')
	    ONE_MORE_BYTE (byte_after_cr);
	  c = c1;
	}
      else
	{
	  ONE_MORE_BYTE (c2);
	  if (c2 < 0 || ! UTF_8_EXTRA_OCTET_P (c2))
	    goto invalid_code;
	  if (UTF_8_2_OCTET_LEADING_P (c1))
	    {
	      c = ((c1 & 0x1F) << 6) | (c2 & 0x3F);
	      /* Reject overlong sequences here and below.  Encoders
		 producing them are incorrect, they can be misleading,
		 and they mess up read/write invariance.  */
	      if (c < 128)
		goto invalid_code;
	    }
	  else
	    {
	      ONE_MORE_BYTE (c3);
	      if (c3 < 0 || ! UTF_8_EXTRA_OCTET_P (c3))
		goto invalid_code;
	      if (UTF_8_3_OCTET_LEADING_P (c1))
		{
		  c = (((c1 & 0xF) << 12)
		       | ((c2 & 0x3F) << 6) | (c3 & 0x3F));
		  if (c < 0x800
		      || (c >= 0xd800 && c < 0xe000)) /* surrogates (invalid) */
		    goto invalid_code;
		}
	      else
		{
		  ONE_MORE_BYTE (c4);
		  if (c4 < 0 || ! UTF_8_EXTRA_OCTET_P (c4))
		    goto invalid_code;
		  if (UTF_8_4_OCTET_LEADING_P (c1))
		    {
		    c = (((c1 & 0x7) << 18) | ((c2 & 0x3F) << 12)
			 | ((c3 & 0x3F) << 6) | (c4 & 0x3F));
		    if (c < 0x10000)
		      goto invalid_code;
		    }
		  else
		    {
		      ONE_MORE_BYTE (c5);
		      if (c5 < 0 || ! UTF_8_EXTRA_OCTET_P (c5))
			goto invalid_code;
		      if (UTF_8_5_OCTET_LEADING_P (c1))
			{
			  c = (((c1 & 0x3) << 24) | ((c2 & 0x3F) << 18)
			       | ((c3 & 0x3F) << 12) | ((c4 & 0x3F) << 6)
			       | (c5 & 0x3F));
			  if ((c > MAX_CHAR) || (c < 0x200000))
			    goto invalid_code;
			}
		      else
			goto invalid_code;
		    }
		}
	    }
	}

      *charbuf++ = c;
      continue;

    invalid_code:
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = ASCII_BYTE_P (c) ? c : BYTE8_TO_CHAR (c);
      coding->errors++;
    }

 no_more_source:
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}


static int
encode_coding_utf_8 (struct coding_system *coding)
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  EMACS_INT produced_chars = 0;
  int c;

  if (CODING_UTF_8_BOM (coding) == utf_with_bom)
    {
      ASSURE_DESTINATION (3);
      EMIT_THREE_BYTES (UTF_8_BOM_1, UTF_8_BOM_2, UTF_8_BOM_3);
      CODING_UTF_8_BOM (coding) = utf_without_bom;
    }

  if (multibytep)
    {
      int safe_room = MAX_MULTIBYTE_LENGTH * 2;

      while (charbuf < charbuf_end)
	{
	  unsigned char str[MAX_MULTIBYTE_LENGTH], *p, *pend = str;

	  ASSURE_DESTINATION (safe_room);
	  c = *charbuf++;
	  if (CHAR_BYTE8_P (c))
	    {
	      c = CHAR_TO_BYTE8 (c);
	      EMIT_ONE_BYTE (c);
	    }
	  else
	    {
	      CHAR_STRING_ADVANCE_NO_UNIFY (c, pend);
	      for (p = str; p < pend; p++)
		EMIT_ONE_BYTE (*p);
	    }
	}
    }
  else
    {
      int safe_room = MAX_MULTIBYTE_LENGTH;

      while (charbuf < charbuf_end)
	{
	  ASSURE_DESTINATION (safe_room);
	  c = *charbuf++;
	  if (CHAR_BYTE8_P (c))
	    *dst++ = CHAR_TO_BYTE8 (c);
	  else
	    CHAR_STRING_ADVANCE_NO_UNIFY (c, dst);
	  produced_chars++;
	}
    }
  record_conversion_result (coding, CODING_RESULT_SUCCESS);
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}


/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in one of UTF-16 based coding systems.
   If it is, return 1, else return 0.  */

#define UTF_16_HIGH_SURROGATE_P(val) \
  (((val) & 0xFC00) == 0xD800)

#define UTF_16_LOW_SURROGATE_P(val) \
  (((val) & 0xFC00) == 0xDC00)


static int
detect_coding_utf_16 (struct coding_system *coding,
		      struct coding_detection_info *detect_info)
{
  const unsigned char *src = coding->source;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int c1, c2;

  detect_info->checked |= CATEGORY_MASK_UTF_16;
  if (coding->mode & CODING_MODE_LAST_BLOCK
      && (coding->src_chars & 1))
    {
      detect_info->rejected |= CATEGORY_MASK_UTF_16;
      return 0;
    }

  TWO_MORE_BYTES (c1, c2);
  if ((c1 == 0xFF) && (c2 == 0xFE))
    {
      detect_info->found |= (CATEGORY_MASK_UTF_16_LE
			     | CATEGORY_MASK_UTF_16_AUTO);
      detect_info->rejected |= (CATEGORY_MASK_UTF_16_BE
				| CATEGORY_MASK_UTF_16_BE_NOSIG
				| CATEGORY_MASK_UTF_16_LE_NOSIG);
    }
  else if ((c1 == 0xFE) && (c2 == 0xFF))
    {
      detect_info->found |= (CATEGORY_MASK_UTF_16_BE
			     | CATEGORY_MASK_UTF_16_AUTO);
      detect_info->rejected |= (CATEGORY_MASK_UTF_16_LE
				| CATEGORY_MASK_UTF_16_BE_NOSIG
				| CATEGORY_MASK_UTF_16_LE_NOSIG);
    }
  else if (c2 < 0)
    {
      detect_info->rejected |= CATEGORY_MASK_UTF_16;
      return 0;
    }
  else
    {
      /* We check the dispersion of Eth and Oth bytes where E is even and
	 O is odd.  If both are high, we assume binary data.*/
      unsigned char e[256], o[256];
      unsigned e_num = 1, o_num = 1;

      memset (e, 0, 256);
      memset (o, 0, 256);
      e[c1] = 1;
      o[c2] = 1;

      detect_info->rejected |= (CATEGORY_MASK_UTF_16_AUTO
				|CATEGORY_MASK_UTF_16_BE
				| CATEGORY_MASK_UTF_16_LE);

      while ((detect_info->rejected & CATEGORY_MASK_UTF_16)
	     != CATEGORY_MASK_UTF_16)
	{
	  TWO_MORE_BYTES (c1, c2);
	  if (c2 < 0)
	    break;
	  if (! e[c1])
	    {
	      e[c1] = 1;
	      e_num++;
	      if (e_num >= 128)
		detect_info->rejected |= CATEGORY_MASK_UTF_16_BE_NOSIG;
	    }
	  if (! o[c2])
	    {
	      o[c2] = 1;
	      o_num++;
	      if (o_num >= 128)
		detect_info->rejected |= CATEGORY_MASK_UTF_16_LE_NOSIG;
	    }
	}
      return 0;
    }

 no_more_source:
  return 1;
}

static void
decode_coding_utf_16 (struct coding_system *coding)
{
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  const unsigned char *src_base;
  int *charbuf = coding->charbuf + coding->charbuf_used;
  /* We may produces at most 3 chars in one loop.  */
  int *charbuf_end = coding->charbuf + coding->charbuf_size - 2;
  EMACS_INT consumed_chars = 0, consumed_chars_base = 0;
  int multibytep = coding->src_multibyte;
  enum utf_bom_type bom = CODING_UTF_16_BOM (coding);
  enum utf_16_endian_type endian = CODING_UTF_16_ENDIAN (coding);
  int surrogate = CODING_UTF_16_SURROGATE (coding);
  int eol_dos =
    !inhibit_eol_conversion && EQ (CODING_ID_EOL_TYPE (coding->id), Qdos);
  int byte_after_cr1 = -1, byte_after_cr2 = -1;

  if (bom == utf_with_bom)
    {
      int c, c1, c2;

      src_base = src;
      ONE_MORE_BYTE (c1);
      ONE_MORE_BYTE (c2);
      c = (c1 << 8) | c2;

      if (endian == utf_16_big_endian
	  ? c != 0xFEFF : c != 0xFFFE)
	{
	  /* The first two bytes are not BOM.  Treat them as bytes
	     for a normal character.  */
	  src = src_base;
	  coding->errors++;
	}
      CODING_UTF_16_BOM (coding) = utf_without_bom;
    }
  else if (bom == utf_detect_bom)
    {
      /* We have already tried to detect BOM and failed in
	 detect_coding.  */
      CODING_UTF_16_BOM (coding) = utf_without_bom;
    }

  while (1)
    {
      int c, c1, c2;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	{
	  if (byte_after_cr1 >= 0)
	    src_base -= 2;
	  break;
	}

      if (byte_after_cr1 >= 0)
	c1 = byte_after_cr1, byte_after_cr1 = -1;
      else
	ONE_MORE_BYTE (c1);
      if (c1 < 0)
	{
	  *charbuf++ = -c1;
	  continue;
	}
      if (byte_after_cr2 >= 0)
	c2 = byte_after_cr2, byte_after_cr2 = -1;
      else
	ONE_MORE_BYTE (c2);
      if (c2 < 0)
	{
	  *charbuf++ = ASCII_BYTE_P (c1) ? c1 : BYTE8_TO_CHAR (c1);
	  *charbuf++ = -c2;
	  continue;
	}
      c = (endian == utf_16_big_endian
	   ? ((c1 << 8) | c2) : ((c2 << 8) | c1));

      if (surrogate)
	{
	  if (! UTF_16_LOW_SURROGATE_P (c))
	    {
	      if (endian == utf_16_big_endian)
		c1 = surrogate >> 8, c2 = surrogate & 0xFF;
	      else
		c1 = surrogate & 0xFF, c2 = surrogate >> 8;
	      *charbuf++ = c1;
	      *charbuf++ = c2;
	      coding->errors++;
	      if (UTF_16_HIGH_SURROGATE_P (c))
		CODING_UTF_16_SURROGATE (coding) = surrogate = c;
	      else
		*charbuf++ = c;
	    }
	  else
	    {
	      c = ((surrogate - 0xD800) << 10) | (c - 0xDC00);
	      CODING_UTF_16_SURROGATE (coding) = surrogate = 0;
	      *charbuf++ = 0x10000 + c;
	    }
	}
      else
	{
	  if (UTF_16_HIGH_SURROGATE_P (c))
	    CODING_UTF_16_SURROGATE (coding) = surrogate = c;
	  else
	    {
	      if (eol_dos && c == '\r')
		{
		  ONE_MORE_BYTE (byte_after_cr1);
		  ONE_MORE_BYTE (byte_after_cr2);
		}
	      *charbuf++ = c;
	    }
	}
    }

 no_more_source:
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}

static int
encode_coding_utf_16 (struct coding_system *coding)
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = 8;
  enum utf_bom_type bom = CODING_UTF_16_BOM (coding);
  int big_endian = CODING_UTF_16_ENDIAN (coding) == utf_16_big_endian;
  EMACS_INT produced_chars = 0;
  int c;

  if (bom != utf_without_bom)
    {
      ASSURE_DESTINATION (safe_room);
      if (big_endian)
	EMIT_TWO_BYTES (0xFE, 0xFF);
      else
	EMIT_TWO_BYTES (0xFF, 0xFE);
      CODING_UTF_16_BOM (coding) = utf_without_bom;
    }

  while (charbuf < charbuf_end)
    {
      ASSURE_DESTINATION (safe_room);
      c = *charbuf++;
      if (c > MAX_UNICODE_CHAR)
	c = coding->default_char;

      if (c < 0x10000)
	{
	  if (big_endian)
	    EMIT_TWO_BYTES (c >> 8, c & 0xFF);
	  else
	    EMIT_TWO_BYTES (c & 0xFF, c >> 8);
	}
      else
	{
	  int c1, c2;

	  c -= 0x10000;
	  c1 = (c >> 10) + 0xD800;
	  c2 = (c & 0x3FF) + 0xDC00;
	  if (big_endian)
	    EMIT_FOUR_BYTES (c1 >> 8, c1 & 0xFF, c2 >> 8, c2 & 0xFF);
	  else
	    EMIT_FOUR_BYTES (c1 & 0xFF, c1 >> 8, c2 & 0xFF, c2 >> 8);
	}
    }
  record_conversion_result (coding, CODING_RESULT_SUCCESS);
  coding->produced = dst - coding->destination;
  coding->produced_char += produced_chars;
  return 0;
}


/*** 6. Old Emacs' internal format (emacs-mule) ***/

/* Emacs' internal format for representation of multiple character
   sets is a kind of multi-byte encoding, i.e. characters are
   represented by variable-length sequences of one-byte codes.

   ASCII characters and control characters (e.g. `tab', `newline') are
   represented by one-byte sequences which are their ASCII codes, in
   the range 0x00 through 0x7F.

   8-bit characters of the range 0x80..0x9F are represented by
   two-byte sequences of LEADING_CODE_8_BIT_CONTROL and (their 8-bit
   code + 0x20).

   8-bit characters of the range 0xA0..0xFF are represented by
   one-byte sequences which are their 8-bit code.

   The other characters are represented by a sequence of `base
   leading-code', optional `extended leading-code', and one or two
   `position-code's.  The length of the sequence is determined by the
   base leading-code.  Leading-code takes the range 0x81 through 0x9D,
   whereas extended leading-code and position-code take the range 0xA0
   through 0xFF.  See `charset.h' for more details about leading-code
   and position-code.

   --- CODE RANGE of Emacs' internal format ---
   character set	range
   -------------	-----
   ascii		0x00..0x7F
   eight-bit-control	LEADING_CODE_8_BIT_CONTROL + 0xA0..0xBF
   eight-bit-graphic	0xA0..0xBF
   ELSE			0x81..0x9D + [0xA0..0xFF]+
   ---------------------------------------------

   As this is the internal character representation, the format is
   usually not used externally (i.e. in a file or in a data sent to a
   process).  But, it is possible to have a text externally in this
   format (i.e. by encoding by the coding system `emacs-mule').

   In that case, a sequence of one-byte codes has a slightly different
   form.

   At first, all characters in eight-bit-control are represented by
   one-byte sequences which are their 8-bit code.

   Next, character composition data are represented by the byte
   sequence of the form: 0x80 METHOD BYTES CHARS COMPONENT ...,
   where,
	METHOD is 0xF2 plus one of composition method (enum
	composition_method),

	BYTES is 0xA0 plus a byte length of this composition data,

	CHARS is 0xA0 plus a number of characters composed by this
	data,

	COMPONENTs are characters of multibyte form or composition
	rules encoded by two-byte of ASCII codes.

   In addition, for backward compatibility, the following formats are
   also recognized as composition data on decoding.

   0x80 MSEQ ...
   0x80 0xFF MSEQ RULE MSEQ RULE ... MSEQ

   Here,
	MSEQ is a multibyte form but in these special format:
	  ASCII: 0xA0 ASCII_CODE+0x80,
	  other: LEADING_CODE+0x20 FOLLOWING-BYTE ...,
	RULE is a one byte code of the range 0xA0..0xF0 that
	represents a composition rule.
  */

char emacs_mule_bytes[256];


/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in `emacs-mule'.  If it is, return 1,
   else return 0.  */

static int
detect_coding_emacs_mule (struct coding_system *coding,
			  struct coding_detection_info *detect_info)
{
  const unsigned char *src = coding->source, *src_base;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  EMACS_INT consumed_chars = 0;
  int c;
  int found = 0;

  detect_info->checked |= CATEGORY_MASK_EMACS_MULE;
  /* A coding system of this category is always ASCII compatible.  */
  src += coding->head_ascii;

  while (1)
    {
      src_base = src;
      ONE_MORE_BYTE (c);
      if (c < 0)
	continue;
      if (c == 0x80)
	{
	  /* Perhaps the start of composite character.  We simply skip
	     it because analyzing it is too heavy for detecting.  But,
	     at least, we check that the composite character
	     constitutes of more than 4 bytes.  */
	  const unsigned char *src_start;

	repeat:
	  src_start = src;
	  do
	    {
	      ONE_MORE_BYTE (c);
	    }
	  while (c >= 0xA0);

	  if (src - src_start <= 4)
	    break;
	  found = CATEGORY_MASK_EMACS_MULE;
	  if (c == 0x80)
	    goto repeat;
	}

      if (c < 0x80)
	{
	  if (c < 0x20
	      && (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO))
	    break;
	}
      else
	{
	  int more_bytes = emacs_mule_bytes[c] - 1;

	  while (more_bytes > 0)
	    {
	      ONE_MORE_BYTE (c);
	      if (c < 0xA0)
		{
		  src--;	/* Unread the last byte.  */
		  break;
		}
	      more_bytes--;
	    }
	  if (more_bytes != 0)
	    break;
	  found = CATEGORY_MASK_EMACS_MULE;
	}
    }
  detect_info->rejected |= CATEGORY_MASK_EMACS_MULE;
  return 0;

 no_more_source:
  if (src_base < src && coding->mode & CODING_MODE_LAST_BLOCK)
    {
      detect_info->rejected |= CATEGORY_MASK_EMACS_MULE;
      return 0;
    }
  detect_info->found |= found;
  return 1;
}


/* Parse emacs-mule multibyte sequence at SRC and return the decoded
   character.  If CMP_STATUS indicates that we must expect MSEQ or
   RULE described above, decode it and return the negative value of
   the decoded character or rule.  If an invalid byte is found, return
   -1.  If SRC is too short, return -2.  */

static int
emacs_mule_char (struct coding_system *coding, const unsigned char *src,
		 int *nbytes, int *nchars, int *id,
		 struct composition_status *cmp_status)
{
  const unsigned char *src_end = coding->source + coding->src_bytes;
  const unsigned char *src_base = src;
  int multibytep = coding->src_multibyte;
  int charset_ID;
  unsigned code;
  int c;
  int consumed_chars = 0;
  int mseq_found = 0;

  ONE_MORE_BYTE (c);
  if (c < 0)
    {
      c = -c;
      charset_ID = emacs_mule_charset[0];
    }
  else
    {
      if (c >= 0xA0)
	{
	  if (cmp_status->state != COMPOSING_NO
	      && cmp_status->old_form)
	    {
	      if (cmp_status->state == COMPOSING_CHAR)
		{
		  if (c == 0xA0)
		    {
		      ONE_MORE_BYTE (c);
		      c -= 0x80;
		      if (c < 0)
			goto invalid_code;
		    }
		  else
		    c -= 0x20;
		  mseq_found = 1;
		}
	      else
		{
		  *nbytes = src - src_base;
		  *nchars = consumed_chars;
		  return -c;
		}
	    }
	  else
	    goto invalid_code;
	}

      switch (emacs_mule_bytes[c])
	{
	case 2:
	  if ((charset_ID = emacs_mule_charset[c]) < 0)
	    goto invalid_code;
	  ONE_MORE_BYTE (c);
	  if (c < 0xA0)
	    goto invalid_code;
	  code = c & 0x7F;
	  break;

	case 3:
	  if (c == EMACS_MULE_LEADING_CODE_PRIVATE_11
	      || c == EMACS_MULE_LEADING_CODE_PRIVATE_12)
	    {
	      ONE_MORE_BYTE (c);
	      if (c < 0xA0 || (charset_ID = emacs_mule_charset[c]) < 0)
		goto invalid_code;
	      ONE_MORE_BYTE (c);
	      if (c < 0xA0)
		goto invalid_code;
	      code = c & 0x7F;
	    }
	  else
	    {
	      if ((charset_ID = emacs_mule_charset[c]) < 0)
		goto invalid_code;
	      ONE_MORE_BYTE (c);
	      if (c < 0xA0)
		goto invalid_code;
	      code = (c & 0x7F) << 8;
	      ONE_MORE_BYTE (c);
	      if (c < 0xA0)
		goto invalid_code;
	      code |= c & 0x7F;
	    }
	  break;

	case 4:
	  ONE_MORE_BYTE (c);
	  if (c < 0 || (charset_ID = emacs_mule_charset[c]) < 0)
	    goto invalid_code;
	  ONE_MORE_BYTE (c);
	  if (c < 0xA0)
	    goto invalid_code;
	  code = (c & 0x7F) << 8;
	  ONE_MORE_BYTE (c);
	  if (c < 0xA0)
	    goto invalid_code;
	  code |= c & 0x7F;
	  break;

	case 1:
	  code = c;
	  charset_ID = ASCII_BYTE_P (code) ? charset_ascii : charset_eight_bit;
	  break;

	default:
	  abort ();
	}
      CODING_DECODE_CHAR (coding, src, src_base, src_end,
			  CHARSET_FROM_ID (charset_ID), code, c);
      if (c < 0)
	goto invalid_code;
    }
  *nbytes = src - src_base;
  *nchars = consumed_chars;
  if (id)
    *id = charset_ID;
  return (mseq_found ? -c : c);

 no_more_source:
  return -2;

 invalid_code:
  return -1;
}


/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".  */

/* Handle these composition sequence ('|': the end of header elements,
   BYTES and CHARS >= 0xA0):

   (1) relative composition: 0x80 0xF2 BYTES CHARS | CHAR ...
   (2) altchar composition:  0x80 0xF4 BYTES CHARS | ALT ... ALT CHAR ...
   (3) alt&rule composition: 0x80 0xF5 BYTES CHARS | ALT RULE ... ALT CHAR ...

   and these old form:

   (4) relative composition: 0x80 | MSEQ ... MSEQ
   (5) rulebase composition: 0x80 0xFF | MSEQ MRULE ... MSEQ

   When the starter 0x80 and the following header elements are found,
   this annotation header is produced.

	[ -LENGTH(==-5) CODING_ANNOTATE_COMPOSITION_MASK NCHARS NBYTES METHOD ]

   NCHARS is CHARS - 0xA0 for (1), (2), (3), and 0 for (4), (5).
   NBYTES is BYTES - 0xA0 for (1), (2), (3), and 0 for (4), (5).

   Then, upon reading the following elements, these codes are produced
   until the composition end is found:

   (1) CHAR ... CHAR
   (2) ALT ... ALT CHAR ... CHAR
   (3) ALT -2 DECODED-RULE ALT -2 DECODED-RULE ... ALT CHAR ... CHAR
   (4) CHAR ... CHAR
   (5) CHAR -2 DECODED-RULE CHAR -2 DECODED-RULE ... CHAR

   When the composition end is found, LENGTH and NCHARS in the
   annotation header is updated as below:

   (1) LENGTH: unchanged, NCHARS: unchanged
   (2) LENGTH: length of the whole sequence minus NCHARS, NCHARS: unchanged
   (3) LENGTH: length of the whole sequence minus NCHARS, NCHARS: unchanged
   (4) LENGTH: unchanged,  NCHARS: number of CHARs
   (5) LENGTH: unchanged,  NCHARS: number of CHARs

   If an error is found while composing, the annotation header is
   changed to the original composition header (plus filler -1s) as
   below:

   (1),(2),(3)  [ 0x80 0xF2+METHOD BYTES CHARS -1 ]
   (5)          [ 0x80 0xFF -1 -1- -1 ]

   and the sequence [ -2 DECODED-RULE ] is changed to the original
   byte sequence as below:
	o the original byte sequence is B: [ B -1 ]
	o the original byte sequence is B1 B2: [ B1 B2 ]

   Most of the routines are implemented by macros because many
   variables and labels in the caller decode_coding_emacs_mule must be
   accessible, and they are usually called just once (thus doesn't
   increase the size of compiled object).  */

/* Decode a composition rule represented by C as a component of
   composition sequence of Emacs 20 style.  Set RULE to the decoded
   rule. */

#define DECODE_EMACS_MULE_COMPOSITION_RULE_20(c, rule)	\
  do {							\
    int gref, nref;					\
    							\
    c -= 0xA0;						\
    if (c < 0 || c >= 81)				\
      goto invalid_code;				\
    gref = c / 9, nref = c % 9;				\
    if (gref == 4) gref = 10;				\
    if (nref == 4) nref = 10;				\
    rule = COMPOSITION_ENCODE_RULE (gref, nref);	\
  } while (0)


/* Decode a composition rule represented by C and the following byte
   at SRC as a component of composition sequence of Emacs 21 style.
   Set RULE to the decoded rule.  */

#define DECODE_EMACS_MULE_COMPOSITION_RULE_21(c, rule)	\
  do {							\
    int gref, nref;					\
    							\
    gref = c - 0x20;					\
    if (gref < 0 || gref >= 81)				\
      goto invalid_code;				\
    ONE_MORE_BYTE (c);					\
    nref = c - 0x20;					\
    if (nref < 0 || nref >= 81)				\
      goto invalid_code;				\
    rule = COMPOSITION_ENCODE_RULE (gref, nref);	\
  } while (0)


/* Start of Emacs 21 style format.  The first three bytes at SRC are
   (METHOD - 0xF2), (BYTES - 0xA0), (CHARS - 0xA0), where BYTES is the
   byte length of this composition information, CHARS is the number of
   characters composed by this composition.  */

#define DECODE_EMACS_MULE_21_COMPOSITION()				\
  do {									\
    enum composition_method method = c - 0xF2;				\
    int nbytes, nchars;							\
    									\
    ONE_MORE_BYTE (c);							\
    if (c < 0)								\
      goto invalid_code;						\
    nbytes = c - 0xA0;							\
    if (nbytes < 3 || (method == COMPOSITION_RELATIVE && nbytes != 4))	\
      goto invalid_code;						\
    ONE_MORE_BYTE (c);							\
    nchars = c - 0xA0;							\
    if (nchars <= 0 || nchars >= MAX_COMPOSITION_COMPONENTS)		\
      goto invalid_code;						\
    cmp_status->old_form = 0;						\
    cmp_status->method = method;					\
    if (method == COMPOSITION_RELATIVE)					\
      cmp_status->state = COMPOSING_CHAR;				\
    else								\
      cmp_status->state = COMPOSING_COMPONENT_CHAR;			\
    cmp_status->length = MAX_ANNOTATION_LENGTH;				\
    cmp_status->nchars = nchars;					\
    cmp_status->ncomps = nbytes - 4;					\
    ADD_COMPOSITION_DATA (charbuf, nchars, nbytes, method);		\
  } while (0)


/* Start of Emacs 20 style format for relative composition.  */

#define DECODE_EMACS_MULE_20_RELATIVE_COMPOSITION()		\
  do {								\
    cmp_status->old_form = 1;					\
    cmp_status->method = COMPOSITION_RELATIVE;			\
    cmp_status->state = COMPOSING_CHAR;				\
    cmp_status->length = MAX_ANNOTATION_LENGTH;			\
    cmp_status->nchars = cmp_status->ncomps = 0;		\
    ADD_COMPOSITION_DATA (charbuf, 0, 0, cmp_status->method);	\
  } while (0)


/* Start of Emacs 20 style format for rule-base composition.  */

#define DECODE_EMACS_MULE_20_RULEBASE_COMPOSITION()		\
  do {								\
    cmp_status->old_form = 1;					\
    cmp_status->method = COMPOSITION_WITH_RULE;			\
    cmp_status->state = COMPOSING_CHAR;				\
    cmp_status->length = MAX_ANNOTATION_LENGTH;			\
    cmp_status->nchars = cmp_status->ncomps = 0;		\
    ADD_COMPOSITION_DATA (charbuf, 0, 0, cmp_status->method);	\
  } while (0)


#define DECODE_EMACS_MULE_COMPOSITION_START()		\
  do {							\
    const unsigned char *current_src = src;		\
    							\
    ONE_MORE_BYTE (c);					\
    if (c < 0)						\
      goto invalid_code;				\
    if (c - 0xF2 >= COMPOSITION_RELATIVE		\
	&& c - 0xF2 <= COMPOSITION_WITH_RULE_ALTCHARS)	\
      DECODE_EMACS_MULE_21_COMPOSITION ();		\
    else if (c < 0xA0)					\
      goto invalid_code;				\
    else if (c < 0xC0)					\
      {							\
	DECODE_EMACS_MULE_20_RELATIVE_COMPOSITION ();	\
	/* Re-read C as a composition component.  */	\
	src = current_src;				\
      }							\
    else if (c == 0xFF)					\
      DECODE_EMACS_MULE_20_RULEBASE_COMPOSITION ();	\
    else						\
      goto invalid_code;				\
  } while (0)

#define EMACS_MULE_COMPOSITION_END()				\
  do {								\
    int idx = - cmp_status->length;				\
    								\
    if (cmp_status->old_form)					\
      charbuf[idx + 2] = cmp_status->nchars;			\
    else if (cmp_status->method > COMPOSITION_RELATIVE)		\
      charbuf[idx] = charbuf[idx + 2] - cmp_status->length;	\
    cmp_status->state = COMPOSING_NO;				\
  } while (0)


static int
emacs_mule_finish_composition (int *charbuf,
			       struct composition_status *cmp_status)
{
  int idx = - cmp_status->length;
  int new_chars;

  if (cmp_status->old_form && cmp_status->nchars > 0)
    {
      charbuf[idx + 2] = cmp_status->nchars;
      new_chars = 0;
      if (cmp_status->method == COMPOSITION_WITH_RULE
	  && cmp_status->state == COMPOSING_CHAR)
	{
	  /* The last rule was invalid.  */
	  int rule = charbuf[-1] + 0xA0;

	  charbuf[-2] = BYTE8_TO_CHAR (rule);
	  charbuf[-1] = -1;
	  new_chars = 1;
	}
    }
  else
    {
      charbuf[idx++] = BYTE8_TO_CHAR (0x80);

      if (cmp_status->method == COMPOSITION_WITH_RULE)
	{
	  charbuf[idx++] = BYTE8_TO_CHAR (0xFF);
	  charbuf[idx++] = -3;
	  charbuf[idx++] = 0;
	  new_chars = 1;
	}
      else
	{
	  int nchars = charbuf[idx + 1] + 0xA0;
	  int nbytes = charbuf[idx + 2] + 0xA0;

	  charbuf[idx++] = BYTE8_TO_CHAR (0xF2 + cmp_status->method);
	  charbuf[idx++] = BYTE8_TO_CHAR (nbytes);
	  charbuf[idx++] = BYTE8_TO_CHAR (nchars);
	  charbuf[idx++] = -1;
	  new_chars = 4;
	}
    }
  cmp_status->state = COMPOSING_NO;
  return new_chars;
}

#define EMACS_MULE_MAYBE_FINISH_COMPOSITION()				  \
  do {									  \
    if (cmp_status->state != COMPOSING_NO)				  \
      char_offset += emacs_mule_finish_composition (charbuf, cmp_status); \
  } while (0)


static void
decode_coding_emacs_mule (struct coding_system *coding)
{
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  const unsigned char *src_base;
  int *charbuf = coding->charbuf + coding->charbuf_used;
  /* We may produce two annotations (charset and composition) in one
     loop and one more charset annotation at the end.  */
  int *charbuf_end
    = coding->charbuf + coding->charbuf_size - (MAX_ANNOTATION_LENGTH * 3)
      /* We can produce up to 2 characters in a loop.  */
      - 1;
  EMACS_INT consumed_chars = 0, consumed_chars_base;
  int multibytep = coding->src_multibyte;
  EMACS_INT char_offset = coding->produced_char;
  EMACS_INT last_offset = char_offset;
  int last_id = charset_ascii;
  int eol_dos =
    !inhibit_eol_conversion && EQ (CODING_ID_EOL_TYPE (coding->id), Qdos);
  int byte_after_cr = -1;
  struct composition_status *cmp_status = &coding->spec.emacs_mule.cmp_status;

  if (cmp_status->state != COMPOSING_NO)
    {
      int i;

      if (charbuf_end - charbuf < cmp_status->length)
	abort ();
      for (i = 0; i < cmp_status->length; i++)
	*charbuf++ = cmp_status->carryover[i];
      coding->annotated = 1;
    }

  while (1)
    {
      int c, id IF_LINT (= 0);

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	{
	  if (byte_after_cr >= 0)
	    src_base--;
	  break;
	}

      if (byte_after_cr >= 0)
	c = byte_after_cr, byte_after_cr = -1;
      else
	ONE_MORE_BYTE (c);

      if (c < 0 || c == 0x80)
	{
	  EMACS_MULE_MAYBE_FINISH_COMPOSITION ();
	  if (c < 0)
	    {
	      *charbuf++ = -c;
	      char_offset++;
	    }
	  else
	    DECODE_EMACS_MULE_COMPOSITION_START ();
	  continue;
	}

      if (c < 0x80)
	{
	  if (eol_dos && c == '\r')
	    ONE_MORE_BYTE (byte_after_cr);
	  id = charset_ascii;
	  if (cmp_status->state != COMPOSING_NO)
	    {
	      if (cmp_status->old_form)
		EMACS_MULE_MAYBE_FINISH_COMPOSITION ();
	      else if (cmp_status->state >= COMPOSING_COMPONENT_CHAR)
		cmp_status->ncomps--;
	    }
	}
      else
	{
	  int nchars IF_LINT (= 0), nbytes IF_LINT (= 0);
	  /* emacs_mule_char can load a charset map from a file, which
	     allocates a large structure and might cause buffer text
	     to be relocated as result.  Thus, we need to remember the
	     original pointer to buffer text, and fix up all related
	     pointers after the call.  */
	  const unsigned char *orig = coding->source;
	  EMACS_INT offset;

	  c = emacs_mule_char (coding, src_base, &nbytes, &nchars, &id,
			       cmp_status);
	  offset = coding->source - orig;
	  if (offset)
	    {
	      src += offset;
	      src_base += offset;
	      src_end += offset;
	    }
	  if (c < 0)
	    {
	      if (c == -1)
		goto invalid_code;
	      if (c == -2)
		break;
	    }
	  src = src_base + nbytes;
	  consumed_chars = consumed_chars_base + nchars;
	  if (cmp_status->state >= COMPOSING_COMPONENT_CHAR)
	    cmp_status->ncomps -= nchars;
	}

      /* Now if C >= 0, we found a normally encoded character, if C <
	 0, we found an old-style composition component character or
	 rule.  */

      if (cmp_status->state == COMPOSING_NO)
	{
	  if (last_id != id)
	    {
	      if (last_id != charset_ascii)
		ADD_CHARSET_DATA (charbuf, char_offset - last_offset,
				  last_id);
	      last_id = id;
	      last_offset = char_offset;
	    }
	  *charbuf++ = c;
	  char_offset++;
	}
      else if (cmp_status->state == COMPOSING_CHAR)
	{
	  if (cmp_status->old_form)
	    {
	      if (c >= 0)
		{
		  EMACS_MULE_MAYBE_FINISH_COMPOSITION ();
		  *charbuf++ = c;
		  char_offset++;
		}
	      else
		{
		  *charbuf++ = -c;
		  cmp_status->nchars++;
		  cmp_status->length++;
		  if (cmp_status->nchars == MAX_COMPOSITION_COMPONENTS)
		    EMACS_MULE_COMPOSITION_END ();
		  else if (cmp_status->method == COMPOSITION_WITH_RULE)
		    cmp_status->state = COMPOSING_RULE;
		}
	    }
	  else
	    {
	      *charbuf++ = c;
	      cmp_status->length++;
	      cmp_status->nchars--;
	      if (cmp_status->nchars == 0)
		EMACS_MULE_COMPOSITION_END ();
	    }
	}
      else if (cmp_status->state == COMPOSING_RULE)
	{
	  int rule;

	  if (c >= 0)
	    {
	      EMACS_MULE_COMPOSITION_END ();
	      *charbuf++ = c;
	      char_offset++;
	    }
	  else
	    {
	      c = -c;
	      DECODE_EMACS_MULE_COMPOSITION_RULE_20 (c, rule);
	      if (rule < 0)
		goto invalid_code;
	      *charbuf++ = -2;
	      *charbuf++ = rule;
	      cmp_status->length += 2;
	      cmp_status->state = COMPOSING_CHAR;
	    }
	}
      else if (cmp_status->state == COMPOSING_COMPONENT_CHAR)
	{
	  *charbuf++ = c;
	  cmp_status->length++;
	  if (cmp_status->ncomps == 0)
	    cmp_status->state = COMPOSING_CHAR;
	  else if (cmp_status->ncomps > 0)
	    {
	      if (cmp_status->method == COMPOSITION_WITH_RULE_ALTCHARS)
		cmp_status->state = COMPOSING_COMPONENT_RULE;
	    }
	  else
	    EMACS_MULE_MAYBE_FINISH_COMPOSITION ();
	}
      else			/* COMPOSING_COMPONENT_RULE */
	{
	  int rule;

	  DECODE_EMACS_MULE_COMPOSITION_RULE_21 (c, rule);
	  if (rule < 0)
	    goto invalid_code;
	  *charbuf++ = -2;
	  *charbuf++ = rule;
	  cmp_status->length += 2;
	  cmp_status->ncomps--;
	  if (cmp_status->ncomps > 0)
	    cmp_status->state = COMPOSING_COMPONENT_CHAR;
	  else
	    EMACS_MULE_MAYBE_FINISH_COMPOSITION ();
	}
      continue;

    invalid_code:
      EMACS_MULE_MAYBE_FINISH_COMPOSITION ();
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = ASCII_BYTE_P (c) ? c : BYTE8_TO_CHAR (c);
      char_offset++;
      coding->errors++;
    }

 no_more_source:
  if (cmp_status->state != COMPOSING_NO)
    {
      if (coding->mode & CODING_MODE_LAST_BLOCK)
	EMACS_MULE_MAYBE_FINISH_COMPOSITION ();
      else
	{
	  int i;

	  charbuf -= cmp_status->length;
	  for (i = 0; i < cmp_status->length; i++)
	    cmp_status->carryover[i] = charbuf[i];
	}
    }
  if (last_id != charset_ascii)
    ADD_CHARSET_DATA (charbuf, char_offset - last_offset, last_id);
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}


#define EMACS_MULE_LEADING_CODES(id, codes)	\
  do {						\
    if (id < 0xA0)				\
      codes[0] = id, codes[1] = 0;		\
    else if (id < 0xE0)				\
      codes[0] = 0x9A, codes[1] = id;		\
    else if (id < 0xF0)				\
      codes[0] = 0x9B, codes[1] = id;		\
    else if (id < 0xF5)				\
      codes[0] = 0x9C, codes[1] = id;		\
    else					\
      codes[0] = 0x9D, codes[1] = id;		\
  } while (0);


static int
encode_coding_emacs_mule (struct coding_system *coding)
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = 8;
  EMACS_INT produced_chars = 0;
  Lisp_Object attrs, charset_list;
  int c;
  int preferred_charset_id = -1;

  CODING_GET_INFO (coding, attrs, charset_list);
  if (! EQ (charset_list, Vemacs_mule_charset_list))
    {
      CODING_ATTR_CHARSET_LIST (attrs)
	= charset_list = Vemacs_mule_charset_list;
    }

  while (charbuf < charbuf_end)
    {
      ASSURE_DESTINATION (safe_room);
      c = *charbuf++;

      if (c < 0)
	{
	  /* Handle an annotation.  */
	  switch (*charbuf)
	    {
	    case CODING_ANNOTATE_COMPOSITION_MASK:
	      /* Not yet implemented.  */
	      break;
	    case CODING_ANNOTATE_CHARSET_MASK:
	      preferred_charset_id = charbuf[3];
	      if (preferred_charset_id >= 0
		  && NILP (Fmemq (make_number (preferred_charset_id),
				  charset_list)))
		preferred_charset_id = -1;
	      break;
	    default:
	      abort ();
	    }
	  charbuf += -c - 1;
	  continue;
	}

      if (ASCII_CHAR_P (c))
	EMIT_ONE_ASCII_BYTE (c);
      else if (CHAR_BYTE8_P (c))
	{
	  c = CHAR_TO_BYTE8 (c);
	  EMIT_ONE_BYTE (c);
	}
      else
	{
	  struct charset *charset;
	  unsigned code;
	  int dimension;
	  int emacs_mule_id;
	  unsigned char leading_codes[2];

	  if (preferred_charset_id >= 0)
	    {
	      int result;

	      charset = CHARSET_FROM_ID (preferred_charset_id);
	      CODING_CHAR_CHARSET_P (coding, dst, dst_end, c, charset, result);
	      if (result)
		code = ENCODE_CHAR (charset, c);
	      else
		CODING_CHAR_CHARSET (coding, dst, dst_end, c, charset_list,
				     &code, charset);
	    }
	  else
	    CODING_CHAR_CHARSET (coding, dst, dst_end, c, charset_list,
				 &code, charset);
	  if (! charset)
	    {
	      c = coding->default_char;
	      if (ASCII_CHAR_P (c))
		{
		  EMIT_ONE_ASCII_BYTE (c);
		  continue;
		}
	      CODING_CHAR_CHARSET (coding, dst, dst_end, c, charset_list,
				   &code, charset);
	    }
	  dimension = CHARSET_DIMENSION (charset);
	  emacs_mule_id = CHARSET_EMACS_MULE_ID (charset);
	  EMACS_MULE_LEADING_CODES (emacs_mule_id, leading_codes);
	  EMIT_ONE_BYTE (leading_codes[0]);
	  if (leading_codes[1])
	    EMIT_ONE_BYTE (leading_codes[1]);
	  if (dimension == 1)
	    EMIT_ONE_BYTE (code | 0x80);
	  else
	    {
	      code |= 0x8080;
	      EMIT_ONE_BYTE (code >> 8);
	      EMIT_ONE_BYTE (code & 0xFF);
	    }
	}
    }
  record_conversion_result (coding, CODING_RESULT_SUCCESS);
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}


/*** 7. ISO2022 handlers ***/

/* The following note describes the coding system ISO2022 briefly.
   Since the intention of this note is to help understand the
   functions in this file, some parts are NOT ACCURATE or are OVERLY
   SIMPLIFIED.  For thorough understanding, please refer to the
   original document of ISO2022.  This is equivalent to the standard
   ECMA-35, obtainable from <URL:http://www.ecma.ch/> (*).

   ISO2022 provides many mechanisms to encode several character sets
   in 7-bit and 8-bit environments.  For 7-bit environments, all text
   is encoded using bytes less than 128.  This may make the encoded
   text a little bit longer, but the text passes more easily through
   several types of gateway, some of which strip off the MSB (Most
   Significant Bit).

   There are two kinds of character sets: control character sets and
   graphic character sets.  The former contain control characters such
   as `newline' and `escape' to provide control functions (control
   functions are also provided by escape sequences).  The latter
   contain graphic characters such as 'A' and '-'.  Emacs recognizes
   two control character sets and many graphic character sets.

   Graphic character sets are classified into one of the following
   four classes, according to the number of bytes (DIMENSION) and
   number of characters in one dimension (CHARS) of the set:
   - DIMENSION1_CHARS94
   - DIMENSION1_CHARS96
   - DIMENSION2_CHARS94
   - DIMENSION2_CHARS96

   In addition, each character set is assigned an identification tag,
   unique for each set, called the "final character" (denoted as <F>
   hereafter).  The <F> of each character set is decided by ECMA(*)
   when it is registered in ISO.  The code range of <F> is 0x30..0x7F
   (0x30..0x3F are for private use only).

   Note (*): ECMA = European Computer Manufacturers Association

   Here are examples of graphic character sets [NAME(<F>)]:
	o DIMENSION1_CHARS94 -- ASCII('B'), right-half-of-JISX0201('I'), ...
	o DIMENSION1_CHARS96 -- right-half-of-ISO8859-1('A'), ...
	o DIMENSION2_CHARS94 -- GB2312('A'), JISX0208('B'), ...
	o DIMENSION2_CHARS96 -- none for the moment

   A code area (1 byte=8 bits) is divided into 4 areas, C0, GL, C1, and GR.
	C0 [0x00..0x1F] -- control character plane 0
	GL [0x20..0x7F] -- graphic character plane 0
	C1 [0x80..0x9F] -- control character plane 1
	GR [0xA0..0xFF] -- graphic character plane 1

   A control character set is directly designated and invoked to C0 or
   C1 by an escape sequence.  The most common case is that:
   - ISO646's  control character set is designated/invoked to C0, and
   - ISO6429's control character set is designated/invoked to C1,
   and usually these designations/invocations are omitted in encoded
   text.  In a 7-bit environment, only C0 can be used, and a control
   character for C1 is encoded by an appropriate escape sequence to
   fit into the environment.  All control characters for C1 are
   defined to have corresponding escape sequences.

   A graphic character set is at first designated to one of four
   graphic registers (G0 through G3), then these graphic registers are
   invoked to GL or GR.  These designations and invocations can be
   done independently.  The most common case is that G0 is invoked to
   GL, G1 is invoked to GR, and ASCII is designated to G0.  Usually
   these invocations and designations are omitted in encoded text.
   In a 7-bit environment, only GL can be used.

   When a graphic character set of CHARS94 is invoked to GL, codes
   0x20 and 0x7F of the GL area work as control characters SPACE and
   DEL respectively, and codes 0xA0 and 0xFF of the GR area should not
   be used.

   There are two ways of invocation: locking-shift and single-shift.
   With locking-shift, the invocation lasts until the next different
   invocation, whereas with single-shift, the invocation affects the
   following character only and doesn't affect the locking-shift
   state.  Invocations are done by the following control characters or
   escape sequences:

   ----------------------------------------------------------------------
   abbrev  function	             cntrl escape seq	description
   ----------------------------------------------------------------------
   SI/LS0  (shift-in)		     0x0F  none		invoke G0 into GL
   SO/LS1  (shift-out)		     0x0E  none		invoke G1 into GL
   LS2     (locking-shift-2)	     none  ESC 'n'	invoke G2 into GL
   LS3     (locking-shift-3)	     none  ESC 'o'	invoke G3 into GL
   LS1R    (locking-shift-1 right)   none  ESC '~'      invoke G1 into GR (*)
   LS2R    (locking-shift-2 right)   none  ESC '}'      invoke G2 into GR (*)
   LS3R    (locking-shift 3 right)   none  ESC '|'      invoke G3 into GR (*)
   SS2     (single-shift-2)	     0x8E  ESC 'N'	invoke G2 for one char
   SS3     (single-shift-3)	     0x8F  ESC 'O'	invoke G3 for one char
   ----------------------------------------------------------------------
   (*) These are not used by any known coding system.

   Control characters for these functions are defined by macros
   ISO_CODE_XXX in `coding.h'.

   Designations are done by the following escape sequences:
   ----------------------------------------------------------------------
   escape sequence	description
   ----------------------------------------------------------------------
   ESC '(' <F>		designate DIMENSION1_CHARS94<F> to G0
   ESC ')' <F>		designate DIMENSION1_CHARS94<F> to G1
   ESC '*' <F>		designate DIMENSION1_CHARS94<F> to G2
   ESC '+' <F>		designate DIMENSION1_CHARS94<F> to G3
   ESC ',' <F>		designate DIMENSION1_CHARS96<F> to G0 (*)
   ESC '-' <F>		designate DIMENSION1_CHARS96<F> to G1
   ESC '.' <F>		designate DIMENSION1_CHARS96<F> to G2
   ESC '/' <F>		designate DIMENSION1_CHARS96<F> to G3
   ESC '$' '(' <F>	designate DIMENSION2_CHARS94<F> to G0 (**)
   ESC '$' ')' <F>	designate DIMENSION2_CHARS94<F> to G1
   ESC '$' '*' <F>	designate DIMENSION2_CHARS94<F> to G2
   ESC '$' '+' <F>	designate DIMENSION2_CHARS94<F> to G3
   ESC '$' ',' <F>	designate DIMENSION2_CHARS96<F> to G0 (*)
   ESC '$' '-' <F>	designate DIMENSION2_CHARS96<F> to G1
   ESC '$' '.' <F>	designate DIMENSION2_CHARS96<F> to G2
   ESC '$' '/' <F>	designate DIMENSION2_CHARS96<F> to G3
   ----------------------------------------------------------------------

   In this list, "DIMENSION1_CHARS94<F>" means a graphic character set
   of dimension 1, chars 94, and final character <F>, etc...

   Note (*): Although these designations are not allowed in ISO2022,
   Emacs accepts them on decoding, and produces them on encoding
   CHARS96 character sets in a coding system which is characterized as
   7-bit environment, non-locking-shift, and non-single-shift.

   Note (**): If <F> is '@', 'A', or 'B', the intermediate character
   '(' must be omitted.  We refer to this as "short-form" hereafter.

   Now you may notice that there are a lot of ways of encoding the
   same multilingual text in ISO2022.  Actually, there exist many
   coding systems such as Compound Text (used in X11's inter client
   communication, ISO-2022-JP (used in Japanese Internet), ISO-2022-KR
   (used in Korean Internet), EUC (Extended UNIX Code, used in Asian
   localized platforms), and all of these are variants of ISO2022.

   In addition to the above, Emacs handles two more kinds of escape
   sequences: ISO6429's direction specification and Emacs' private
   sequence for specifying character composition.

   ISO6429's direction specification takes the following form:
	o CSI ']'      -- end of the current direction
	o CSI '0' ']'  -- end of the current direction
	o CSI '1' ']'  -- start of left-to-right text
	o CSI '2' ']'  -- start of right-to-left text
   The control character CSI (0x9B: control sequence introducer) is
   abbreviated to the escape sequence ESC '[' in a 7-bit environment.

   Character composition specification takes the following form:
	o ESC '0' -- start relative composition
	o ESC '1' -- end composition
	o ESC '2' -- start rule-base composition (*)
	o ESC '3' -- start relative composition with alternate chars  (**)
	o ESC '4' -- start rule-base composition with alternate chars  (**)
  Since these are not standard escape sequences of any ISO standard,
  the use of them with these meanings is restricted to Emacs only.

  (*) This form is used only in Emacs 20.7 and older versions,
  but newer versions can safely decode it.
  (**) This form is used only in Emacs 21.1 and newer versions,
  and older versions can't decode it.

  Here's a list of example usages of these composition escape
  sequences (categorized by `enum composition_method').

  COMPOSITION_RELATIVE:
	ESC 0 CHAR [ CHAR ] ESC 1
  COMPOSITION_WITH_RULE:
	ESC 2 CHAR [ RULE CHAR ] ESC 1
  COMPOSITION_WITH_ALTCHARS:
	ESC 3 ALTCHAR [ ALTCHAR ] ESC 0 CHAR [ CHAR ] ESC 1
  COMPOSITION_WITH_RULE_ALTCHARS:
	ESC 4 ALTCHAR [ RULE ALTCHAR ] ESC 0 CHAR [ CHAR ] ESC 1 */

static enum iso_code_class_type iso_code_class[256];

#define SAFE_CHARSET_P(coding, id)	\
  ((id) <= (coding)->max_charset_id	\
   && (coding)->safe_charsets[id] != 255)

static void
setup_iso_safe_charsets (Lisp_Object attrs)
{
  Lisp_Object charset_list, safe_charsets;
  Lisp_Object request;
  Lisp_Object reg_usage;
  Lisp_Object tail;
  int reg94, reg96;
  int flags = XINT (AREF (attrs, coding_attr_iso_flags));
  int max_charset_id;

  charset_list = CODING_ATTR_CHARSET_LIST (attrs);
  if ((flags & CODING_ISO_FLAG_FULL_SUPPORT)
      && ! EQ (charset_list, Viso_2022_charset_list))
    {
      CODING_ATTR_CHARSET_LIST (attrs)
	= charset_list = Viso_2022_charset_list;
      ASET (attrs, coding_attr_safe_charsets, Qnil);
    }

  if (STRINGP (AREF (attrs, coding_attr_safe_charsets)))
    return;

  max_charset_id = 0;
  for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
    {
      int id = XINT (XCAR (tail));
      if (max_charset_id < id)
	max_charset_id = id;
    }

  safe_charsets = make_uninit_string (max_charset_id + 1);
  memset (SDATA (safe_charsets), 255, max_charset_id + 1);
  request = AREF (attrs, coding_attr_iso_request);
  reg_usage = AREF (attrs, coding_attr_iso_usage);
  reg94 = XINT (XCAR (reg_usage));
  reg96 = XINT (XCDR (reg_usage));

  for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object id;
      Lisp_Object reg;
      struct charset *charset;

      id = XCAR (tail);
      charset = CHARSET_FROM_ID (XINT (id));
      reg = Fcdr (Fassq (id, request));
      if (! NILP (reg))
	SSET (safe_charsets, XINT (id), XINT (reg));
      else if (charset->iso_chars_96)
	{
	  if (reg96 < 4)
	    SSET (safe_charsets, XINT (id), reg96);
	}
      else
	{
	  if (reg94 < 4)
	    SSET (safe_charsets, XINT (id), reg94);
	}
    }
  ASET (attrs, coding_attr_safe_charsets, safe_charsets);
}


/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in one of ISO-2022 based coding systems.
   If it is, return 1, else return 0.  */

static int
detect_coding_iso_2022 (struct coding_system *coding,
			struct coding_detection_info *detect_info)
{
  const unsigned char *src = coding->source, *src_base = src;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  int single_shifting = 0;
  int id;
  int c, c1;
  EMACS_INT consumed_chars = 0;
  int i;
  int rejected = 0;
  int found = 0;
  int composition_count = -1;

  detect_info->checked |= CATEGORY_MASK_ISO;

  for (i = coding_category_iso_7; i <= coding_category_iso_8_else; i++)
    {
      struct coding_system *this = &(coding_categories[i]);
      Lisp_Object attrs, val;

      if (this->id < 0)
	continue;
      attrs = CODING_ID_ATTRS (this->id);
      if (CODING_ISO_FLAGS (this) & CODING_ISO_FLAG_FULL_SUPPORT
	  && ! EQ (CODING_ATTR_CHARSET_LIST (attrs), Viso_2022_charset_list))
	setup_iso_safe_charsets (attrs);
      val = CODING_ATTR_SAFE_CHARSETS (attrs);
      this->max_charset_id = SCHARS (val) - 1;
      this->safe_charsets = SDATA (val);
    }

  /* A coding system of this category is always ASCII compatible.  */
  src += coding->head_ascii;

  while (rejected != CATEGORY_MASK_ISO)
    {
      src_base = src;
      ONE_MORE_BYTE (c);
      switch (c)
	{
	case ISO_CODE_ESC:
	  if (inhibit_iso_escape_detection)
	    break;
	  single_shifting = 0;
	  ONE_MORE_BYTE (c);
	  if (c == 'N' || c == 'O')
	    {
	      /* ESC <Fe> for SS2 or SS3.  */
	      single_shifting = 1;
	      rejected |= CATEGORY_MASK_ISO_7BIT | CATEGORY_MASK_ISO_8BIT;
	    }
	  else if (c == '1')
	    {
	      /* End of composition.  */
	      if (composition_count < 0
		  || composition_count > MAX_COMPOSITION_COMPONENTS)
		/* Invalid */
		break;
	      composition_count = -1;
	      found |= CATEGORY_MASK_ISO;
	    }
	  else if (c >= '0' && c <= '4')
	    {
	      /* ESC <Fp> for start/end composition.  */
	      composition_count = 0;
	    }
	  else
	    {
	      if (c >= '(' && c <= '/')
		{
		  /* Designation sequence for a charset of dimension 1.  */
		  ONE_MORE_BYTE (c1);
		  if (c1 < ' ' || c1 >= 0x80
		      || (id = iso_charset_table[0][c >= ','][c1]) < 0)
		    /* Invalid designation sequence.  Just ignore.  */
		    break;
		}
	      else if (c == '$')
		{
		  /* Designation sequence for a charset of dimension 2.  */
		  ONE_MORE_BYTE (c);
		  if (c >= '@' && c <= 'B')
		    /* Designation for JISX0208.1978, GB2312, or JISX0208.  */
		    id = iso_charset_table[1][0][c];
		  else if (c >= '(' && c <= '/')
		    {
		      ONE_MORE_BYTE (c1);
		      if (c1 < ' ' || c1 >= 0x80
			  || (id = iso_charset_table[1][c >= ','][c1]) < 0)
			/* Invalid designation sequence.  Just ignore.  */
			break;
		    }
		  else
		    /* Invalid designation sequence.  Just ignore it.  */
		    break;
		}
	      else
		{
		  /* Invalid escape sequence.  Just ignore it.  */
		  break;
		}

	      /* We found a valid designation sequence for CHARSET.  */
	      rejected |= CATEGORY_MASK_ISO_8BIT;
	      if (SAFE_CHARSET_P (&coding_categories[coding_category_iso_7],
				  id))
		found |= CATEGORY_MASK_ISO_7;
	      else
		rejected |= CATEGORY_MASK_ISO_7;
	      if (SAFE_CHARSET_P (&coding_categories[coding_category_iso_7_tight],
				  id))
		found |= CATEGORY_MASK_ISO_7_TIGHT;
	      else
		rejected |= CATEGORY_MASK_ISO_7_TIGHT;
	      if (SAFE_CHARSET_P (&coding_categories[coding_category_iso_7_else],
				  id))
		found |= CATEGORY_MASK_ISO_7_ELSE;
	      else
		rejected |= CATEGORY_MASK_ISO_7_ELSE;
	      if (SAFE_CHARSET_P (&coding_categories[coding_category_iso_8_else],
				  id))
		found |= CATEGORY_MASK_ISO_8_ELSE;
	      else
		rejected |= CATEGORY_MASK_ISO_8_ELSE;
	    }
	  break;

	case ISO_CODE_SO:
	case ISO_CODE_SI:
	  /* Locking shift out/in.  */
	  if (inhibit_iso_escape_detection)
	    break;
	  single_shifting = 0;
	  rejected |= CATEGORY_MASK_ISO_7BIT | CATEGORY_MASK_ISO_8BIT;
	  break;

	case ISO_CODE_CSI:
	  /* Control sequence introducer.  */
	  single_shifting = 0;
	  rejected |= CATEGORY_MASK_ISO_7BIT | CATEGORY_MASK_ISO_7_ELSE;
	  found |= CATEGORY_MASK_ISO_8_ELSE;
	  goto check_extra_latin;

	case ISO_CODE_SS2:
	case ISO_CODE_SS3:
	  /* Single shift.   */
	  if (inhibit_iso_escape_detection)
	    break;
	  single_shifting = 0;
	  rejected |= CATEGORY_MASK_ISO_7BIT;
	  if (CODING_ISO_FLAGS (&coding_categories[coding_category_iso_8_1])
	      & CODING_ISO_FLAG_SINGLE_SHIFT)
	    {
	      found |= CATEGORY_MASK_ISO_8_1;
	      single_shifting = 1;
	    }
	  if (CODING_ISO_FLAGS (&coding_categories[coding_category_iso_8_2])
	      & CODING_ISO_FLAG_SINGLE_SHIFT)
	    {
	      found |= CATEGORY_MASK_ISO_8_2;
	      single_shifting = 1;
	    }
	  if (single_shifting)
	    break;
	check_extra_latin:
	  if (! VECTORP (Vlatin_extra_code_table)
	      || NILP (XVECTOR (Vlatin_extra_code_table)->contents[c]))
	    {
	      rejected = CATEGORY_MASK_ISO;
	      break;
	    }
	  if (CODING_ISO_FLAGS (&coding_categories[coding_category_iso_8_1])
	      & CODING_ISO_FLAG_LATIN_EXTRA)
	    found |= CATEGORY_MASK_ISO_8_1;
	  else
	    rejected |= CATEGORY_MASK_ISO_8_1;
	  rejected |= CATEGORY_MASK_ISO_8_2;
	  break;

	default:
	  if (c < 0)
	    continue;
	  if (c < 0x80)
	    {
	      if (composition_count >= 0)
		composition_count++;
	      single_shifting = 0;
	      break;
	    }
	  if (c >= 0xA0)
	    {
	      rejected |= CATEGORY_MASK_ISO_7BIT | CATEGORY_MASK_ISO_7_ELSE;
	      found |= CATEGORY_MASK_ISO_8_1;
	      /* Check the length of succeeding codes of the range
                 0xA0..0FF.  If the byte length is even, we include
                 CATEGORY_MASK_ISO_8_2 in `found'.  We can check this
                 only when we are not single shifting.  */
	      if (! single_shifting
		  && ! (rejected & CATEGORY_MASK_ISO_8_2))
		{
		  int len = 1;
		  while (src < src_end)
		    {
		      src_base = src;
		      ONE_MORE_BYTE (c);
		      if (c < 0xA0)
			{
			  src = src_base;
			  break;
			}
		      len++;
		    }

		  if (len & 1 && src < src_end)
		    {
		      rejected |= CATEGORY_MASK_ISO_8_2;
		      if (composition_count >= 0)
			composition_count += len;
		    }
		  else
		    {
		      found |= CATEGORY_MASK_ISO_8_2;
		      if (composition_count >= 0)
			composition_count += len / 2;
		    }
		}
	      break;
	    }
	}
    }
  detect_info->rejected |= CATEGORY_MASK_ISO;
  return 0;

 no_more_source:
  detect_info->rejected |= rejected;
  detect_info->found |= (found & ~rejected);
  return 1;
}


/* Set designation state into CODING.  Set CHARS_96 to -1 if the
   escape sequence should be kept.  */
#define DECODE_DESIGNATION(reg, dim, chars_96, final)			\
  do {									\
    int id, prev;							\
									\
    if (final < '0' || final >= 128					\
	|| ((id = ISO_CHARSET_TABLE (dim, chars_96, final)) < 0)	\
	|| !SAFE_CHARSET_P (coding, id))				\
      {									\
	CODING_ISO_DESIGNATION (coding, reg) = -2;			\
	chars_96 = -1;							\
	break;								\
      }									\
    prev = CODING_ISO_DESIGNATION (coding, reg);			\
    if (id == charset_jisx0201_roman)					\
      {									\
	if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_USE_ROMAN)	\
	  id = charset_ascii;						\
      }									\
    else if (id == charset_jisx0208_1978)				\
      {									\
	if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_USE_OLDJIS)	\
	  id = charset_jisx0208;					\
      }									\
    CODING_ISO_DESIGNATION (coding, reg) = id;				\
    /* If there was an invalid designation to REG previously, and this	\
       designation is ASCII to REG, we should keep this designation	\
       sequence.  */							\
    if (prev == -2 && id == charset_ascii)				\
      chars_96 = -1;							\
  } while (0)


/* Handle these composition sequence (ALT: alternate char):

   (1) relative composition: ESC 0 CHAR ... ESC 1
   (2) rulebase composition: ESC 2 CHAR RULE CHAR RULE ... CHAR ESC 1
   (3) altchar composition:  ESC 3 ALT ... ALT ESC 0 CHAR ... ESC 1
   (4) alt&rule composition: ESC 4 ALT RULE ... ALT ESC 0 CHAR ... ESC 1

   When the start sequence (ESC 0/2/3/4) is found, this annotation
   header is produced.

	[ -LENGTH(==-5) CODING_ANNOTATE_COMPOSITION_MASK NCHARS(==0) 0 METHOD ]

   Then, upon reading CHAR or RULE (one or two bytes), these codes are
   produced until the end sequence (ESC 1) is found:

   (1) CHAR ... CHAR
   (2) CHAR -2 DECODED-RULE CHAR -2 DECODED-RULE ... CHAR
   (3) ALT ... ALT -1 -1 CHAR ... CHAR
   (4) ALT -2 DECODED-RULE ALT -2 DECODED-RULE ... ALT -1 -1 CHAR ... CHAR

   When the end sequence (ESC 1) is found, LENGTH and NCHARS in the
   annotation header is updated as below:

   (1) LENGTH: unchanged,  NCHARS: number of CHARs
   (2) LENGTH: unchanged,  NCHARS: number of CHARs
   (3) LENGTH: += number of ALTs + 2,  NCHARS: number of CHARs
   (4) LENGTH: += number of ALTs * 3,  NCHARS: number of CHARs

   If an error is found while composing, the annotation header is
   changed to:

	[ ESC '0'/'2'/'3'/'4' -2 0 ]

   and the sequence [ -2 DECODED-RULE ] is changed to the original
   byte sequence as below:
	o the original byte sequence is B: [ B -1 ]
	o the original byte sequence is B1 B2: [ B1 B2 ]
   and the sequence [ -1 -1 ] is changed to the original byte
   sequence:
	[ ESC '0' ]
*/

/* Decode a composition rule C1 and maybe one more byte from the
   source, and set RULE to the encoded composition rule.  If the rule
   is invalid, goto invalid_code.  */

#define DECODE_COMPOSITION_RULE(rule)					\
  do {									\
    rule = c1 - 32;							\
    if (rule < 0)							\
      goto invalid_code;						\
    if (rule < 81)		/* old format (before ver.21) */	\
      {									\
	int gref = (rule) / 9;						\
	int nref = (rule) % 9;						\
	if (gref == 4) gref = 10;					\
	if (nref == 4) nref = 10;					\
	rule = COMPOSITION_ENCODE_RULE (gref, nref);			\
      }									\
    else			/* new format (after ver.21) */		\
      {									\
	int b;								\
									\
	ONE_MORE_BYTE (b);						\
	if (! COMPOSITION_ENCODE_RULE_VALID (rule - 81, b - 32))	\
	  goto invalid_code;						\
	rule = COMPOSITION_ENCODE_RULE (rule - 81, b - 32);		\
	rule += 0x100;   /* Distinguish it from the old format.  */	\
      }									\
  } while (0)

#define ENCODE_COMPOSITION_RULE(rule)				\
  do {								\
    int gref = (rule % 0x100) / 12, nref = (rule % 0x100) % 12;	\
    								\
    if (rule < 0x100)		/* old format */		\
      {								\
	if (gref == 10) gref = 4;				\
	if (nref == 10) nref = 4;				\
	charbuf[idx] = 32 + gref * 9 + nref;			\
	charbuf[idx + 1] = -1;					\
	new_chars++;						\
      }								\
    else				/* new format */	\
      {								\
	charbuf[idx] = 32 + 81 + gref;				\
	charbuf[idx + 1] = 32 + nref;				\
	new_chars += 2;						\
      }								\
  } while (0)

/* Finish the current composition as invalid.  */

static int finish_composition (int *, struct composition_status *);

static int
finish_composition (int *charbuf, struct composition_status *cmp_status)
{
  int idx = - cmp_status->length;
  int new_chars;

  /* Recover the original ESC sequence */
  charbuf[idx++] = ISO_CODE_ESC;
  charbuf[idx++] = (cmp_status->method == COMPOSITION_RELATIVE ? '0'
		    : cmp_status->method == COMPOSITION_WITH_RULE ? '2'
		    : cmp_status->method == COMPOSITION_WITH_ALTCHARS ? '3'
		    /* cmp_status->method == COMPOSITION_WITH_RULE_ALTCHARS */
		    : '4');
  charbuf[idx++] = -2;
  charbuf[idx++] = 0;
  charbuf[idx++] = -1;
  new_chars = cmp_status->nchars;
  if (cmp_status->method >= COMPOSITION_WITH_RULE)
    for (; idx < 0; idx++)
      {
	int elt = charbuf[idx];

	if (elt == -2)
	  {
	    ENCODE_COMPOSITION_RULE (charbuf[idx + 1]);
	    idx++;
	  }
	else if (elt == -1)
	  {
	    charbuf[idx++] = ISO_CODE_ESC;
	    charbuf[idx] = '0';
	    new_chars += 2;
	  }
      }
  cmp_status->state = COMPOSING_NO;
  return new_chars;
}

/* If characters are under composition, finish the composition.  */
#define MAYBE_FINISH_COMPOSITION()				\
  do {								\
    if (cmp_status->state != COMPOSING_NO)			\
      char_offset += finish_composition (charbuf, cmp_status);	\
  } while (0)

/* Handle composition start sequence ESC 0, ESC 2, ESC 3, or ESC 4.

   ESC 0 : relative composition : ESC 0 CHAR ... ESC 1
   ESC 2 : rulebase composition : ESC 2 CHAR RULE CHAR RULE ... CHAR ESC 1
   ESC 3 : altchar composition :  ESC 3 CHAR ... ESC 0 CHAR ... ESC 1
   ESC 4 : alt&rule composition : ESC 4 CHAR RULE ... CHAR ESC 0 CHAR ... ESC 1

   Produce this annotation sequence now:

   [ -LENGTH(==-4) CODING_ANNOTATE_COMPOSITION_MASK NCHARS(==0) METHOD ]
*/

#define DECODE_COMPOSITION_START(c1)					   \
  do {									   \
    if (c1 == '0'							   \
	&& ((cmp_status->state == COMPOSING_COMPONENT_CHAR		   \
	     && cmp_status->method == COMPOSITION_WITH_ALTCHARS)	   \
	    || (cmp_status->state == COMPOSING_COMPONENT_RULE		   \
		&& cmp_status->method == COMPOSITION_WITH_RULE_ALTCHARS))) \
      {									   \
	*charbuf++ = -1;						   \
	*charbuf++= -1;							   \
	cmp_status->state = COMPOSING_CHAR;				   \
	cmp_status->length += 2;					   \
      }									   \
    else								   \
      {									   \
	MAYBE_FINISH_COMPOSITION ();					   \
	cmp_status->method = (c1 == '0' ? COMPOSITION_RELATIVE		   \
			      : c1 == '2' ? COMPOSITION_WITH_RULE	   \
			      : c1 == '3' ? COMPOSITION_WITH_ALTCHARS	   \
			      : COMPOSITION_WITH_RULE_ALTCHARS);	   \
	cmp_status->state						   \
	  = (c1 <= '2' ? COMPOSING_CHAR : COMPOSING_COMPONENT_CHAR);	   \
	ADD_COMPOSITION_DATA (charbuf, 0, 0, cmp_status->method);	   \
	cmp_status->length = MAX_ANNOTATION_LENGTH;			   \
	cmp_status->nchars = cmp_status->ncomps = 0;			   \
	coding->annotated = 1;						   \
      }									   \
  } while (0)


/* Handle composition end sequence ESC 1.  */

#define DECODE_COMPOSITION_END()					\
  do {									\
    if (cmp_status->nchars == 0						\
	|| ((cmp_status->state == COMPOSING_CHAR)			\
	    == (cmp_status->method == COMPOSITION_WITH_RULE)))		\
      {									\
	MAYBE_FINISH_COMPOSITION ();					\
	goto invalid_code;						\
      }									\
    if (cmp_status->method == COMPOSITION_WITH_ALTCHARS)		\
      charbuf[- cmp_status->length] -= cmp_status->ncomps + 2;		\
    else if (cmp_status->method == COMPOSITION_WITH_RULE_ALTCHARS)	\
      charbuf[- cmp_status->length] -= cmp_status->ncomps * 3;		\
    charbuf[- cmp_status->length + 2] = cmp_status->nchars;		\
    char_offset += cmp_status->nchars;					\
    cmp_status->state = COMPOSING_NO;					\
  } while (0)

/* Store a composition rule RULE in charbuf, and update cmp_status.  */

#define STORE_COMPOSITION_RULE(rule)	\
  do {					\
    *charbuf++ = -2;			\
    *charbuf++ = rule;			\
    cmp_status->length += 2;		\
    cmp_status->state--;		\
  } while (0)

/* Store a composed char or a component char C in charbuf, and update
   cmp_status.  */

#define STORE_COMPOSITION_CHAR(c)					\
  do {									\
    *charbuf++ = (c);							\
    cmp_status->length++;						\
    if (cmp_status->state == COMPOSING_CHAR)				\
      cmp_status->nchars++;						\
    else								\
      cmp_status->ncomps++;						\
    if (cmp_status->method == COMPOSITION_WITH_RULE			\
	|| (cmp_status->method == COMPOSITION_WITH_RULE_ALTCHARS	\
	    && cmp_status->state == COMPOSING_COMPONENT_CHAR))		\
      cmp_status->state++;						\
  } while (0)


/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".  */

static void
decode_coding_iso_2022 (struct coding_system *coding)
{
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  const unsigned char *src_base;
  int *charbuf = coding->charbuf + coding->charbuf_used;
  /* We may produce two annotations (charset and composition) in one
     loop and one more charset annotation at the end.  */
  int *charbuf_end
    = coding->charbuf + coding->charbuf_size - (MAX_ANNOTATION_LENGTH * 3);
  EMACS_INT consumed_chars = 0, consumed_chars_base;
  int multibytep = coding->src_multibyte;
  /* Charsets invoked to graphic plane 0 and 1 respectively.  */
  int charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
  int charset_id_1 = CODING_ISO_INVOKED_CHARSET (coding, 1);
  int charset_id_2, charset_id_3;
  struct charset *charset;
  int c;
  struct composition_status *cmp_status = CODING_ISO_CMP_STATUS (coding);
  Lisp_Object attrs = CODING_ID_ATTRS (coding->id);
  EMACS_INT char_offset = coding->produced_char;
  EMACS_INT last_offset = char_offset;
  int last_id = charset_ascii;
  int eol_dos =
    !inhibit_eol_conversion && EQ (CODING_ID_EOL_TYPE (coding->id), Qdos);
  int byte_after_cr = -1;
  int i;

  setup_iso_safe_charsets (attrs);
  coding->safe_charsets = SDATA (CODING_ATTR_SAFE_CHARSETS (attrs));

  if (cmp_status->state != COMPOSING_NO)
    {
      if (charbuf_end - charbuf < cmp_status->length)
	abort ();
      for (i = 0; i < cmp_status->length; i++)
	*charbuf++ = cmp_status->carryover[i];
      coding->annotated = 1;
    }

  while (1)
    {
      int c1, c2, c3;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	{
	  if (byte_after_cr >= 0)
	    src_base--;
	  break;
	}

      if (byte_after_cr >= 0)
	c1 = byte_after_cr, byte_after_cr = -1;
      else
	ONE_MORE_BYTE (c1);
      if (c1 < 0)
	goto invalid_code;

      if (CODING_ISO_EXTSEGMENT_LEN (coding) > 0)
	{
	  *charbuf++ = ASCII_BYTE_P (c1) ? c1 : BYTE8_TO_CHAR (c1);
	  char_offset++;
	  CODING_ISO_EXTSEGMENT_LEN (coding)--;
	  continue;
	}

      if (CODING_ISO_EMBEDDED_UTF_8 (coding))
	{
	  if (c1 == ISO_CODE_ESC)
	    {
	      if (src + 1 >= src_end)
		goto no_more_source;
	      *charbuf++ = ISO_CODE_ESC;
	      char_offset++;
	      if (src[0] == '%' && src[1] == '@')
		{
		  src += 2;
		  consumed_chars += 2;
		  char_offset += 2;
		  /* We are sure charbuf can contain two more chars. */
		  *charbuf++ = '%';
		  *charbuf++ = '@';
		  CODING_ISO_EMBEDDED_UTF_8 (coding) = 0;
		}
	    }
	  else
	    {
	      *charbuf++ = ASCII_BYTE_P (c1) ? c1 : BYTE8_TO_CHAR (c1);
	      char_offset++;
	    }
	  continue;
	}

      if ((cmp_status->state == COMPOSING_RULE
	   || cmp_status->state == COMPOSING_COMPONENT_RULE)
	  && c1 != ISO_CODE_ESC)
	{
	  int rule;

	  DECODE_COMPOSITION_RULE (rule);
	  STORE_COMPOSITION_RULE (rule);
	  continue;
	}

      /* We produce at most one character.  */
      switch (iso_code_class [c1])
	{
	case ISO_0x20_or_0x7F:
	  if (charset_id_0 < 0
	      || ! CHARSET_ISO_CHARS_96 (CHARSET_FROM_ID (charset_id_0)))
	    /* This is SPACE or DEL.  */
	    charset = CHARSET_FROM_ID (charset_ascii);
	  else
	    charset = CHARSET_FROM_ID (charset_id_0);
	  break;

	case ISO_graphic_plane_0:
	  if (charset_id_0 < 0)
	    charset = CHARSET_FROM_ID (charset_ascii);
	  else
	    charset = CHARSET_FROM_ID (charset_id_0);
	  break;

	case ISO_0xA0_or_0xFF:
	  if (charset_id_1 < 0
	      || ! CHARSET_ISO_CHARS_96 (CHARSET_FROM_ID (charset_id_1))
	      || CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS)
	    goto invalid_code;
	  /* This is a graphic character, we fall down ... */

	case ISO_graphic_plane_1:
	  if (charset_id_1 < 0)
	    goto invalid_code;
	  charset = CHARSET_FROM_ID (charset_id_1);
	  break;

	case ISO_control_0:
	  if (eol_dos && c1 == '\r')
	    ONE_MORE_BYTE (byte_after_cr);
	  MAYBE_FINISH_COMPOSITION ();
	  charset = CHARSET_FROM_ID (charset_ascii);
	  break;

	case ISO_control_1:
	  goto invalid_code;

	case ISO_shift_out:
	  if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_LOCKING_SHIFT)
	      || CODING_ISO_DESIGNATION (coding, 1) < 0)
	    goto invalid_code;
	  CODING_ISO_INVOCATION (coding, 0) = 1;
	  charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
	  continue;

	case ISO_shift_in:
	  if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_LOCKING_SHIFT))
	    goto invalid_code;
	  CODING_ISO_INVOCATION (coding, 0) = 0;
	  charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
	  continue;

	case ISO_single_shift_2_7:
	  if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS))
	    goto invalid_code;
	case ISO_single_shift_2:
	  if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT))
	    goto invalid_code;
	  /* SS2 is handled as an escape sequence of ESC 'N' */
	  c1 = 'N';
	  goto label_escape_sequence;

	case ISO_single_shift_3:
	  if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT))
	    goto invalid_code;
	  /* SS2 is handled as an escape sequence of ESC 'O' */
	  c1 = 'O';
	  goto label_escape_sequence;

	case ISO_control_sequence_introducer:
	  /* CSI is handled as an escape sequence of ESC '[' ...  */
	  c1 = '[';
	  goto label_escape_sequence;

	case ISO_escape:
	  ONE_MORE_BYTE (c1);
	label_escape_sequence:
	  /* Escape sequences handled here are invocation,
	     designation, direction specification, and character
	     composition specification.  */
	  switch (c1)
	    {
	    case '&':		/* revision of following character set */
	      ONE_MORE_BYTE (c1);
	      if (!(c1 >= '@' && c1 <= '~'))
		goto invalid_code;
	      ONE_MORE_BYTE (c1);
	      if (c1 != ISO_CODE_ESC)
		goto invalid_code;
	      ONE_MORE_BYTE (c1);
	      goto label_escape_sequence;

	    case '$':		/* designation of 2-byte character set */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_DESIGNATION))
		goto invalid_code;
	      {
		int reg, chars96;

		ONE_MORE_BYTE (c1);
		if (c1 >= '@' && c1 <= 'B')
		  {	/* designation of JISX0208.1978, GB2312.1980,
			   or JISX0208.1980 */
		    reg = 0, chars96 = 0;
		  }
		else if (c1 >= 0x28 && c1 <= 0x2B)
		  { /* designation of DIMENSION2_CHARS94 character set */
		    reg = c1 - 0x28, chars96 = 0;
		    ONE_MORE_BYTE (c1);
		  }
		else if (c1 >= 0x2C && c1 <= 0x2F)
		  { /* designation of DIMENSION2_CHARS96 character set */
		    reg = c1 - 0x2C, chars96 = 1;
		    ONE_MORE_BYTE (c1);
		  }
		else
		  goto invalid_code;
		DECODE_DESIGNATION (reg, 2, chars96, c1);
		/* We must update these variables now.  */
		if (reg == 0)
		  charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
		else if (reg == 1)
		  charset_id_1 = CODING_ISO_INVOKED_CHARSET (coding, 1);
		if (chars96 < 0)
		  goto invalid_code;
	      }
	      continue;

	    case 'n':		/* invocation of locking-shift-2 */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_LOCKING_SHIFT)
		  || CODING_ISO_DESIGNATION (coding, 2) < 0)
		goto invalid_code;
	      CODING_ISO_INVOCATION (coding, 0) = 2;
	      charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
	      continue;

	    case 'o':		/* invocation of locking-shift-3 */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_LOCKING_SHIFT)
		  || CODING_ISO_DESIGNATION (coding, 3) < 0)
		goto invalid_code;
	      CODING_ISO_INVOCATION (coding, 0) = 3;
	      charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
	      continue;

	    case 'N':		/* invocation of single-shift-2 */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT)
		  || CODING_ISO_DESIGNATION (coding, 2) < 0)
		goto invalid_code;
	      charset_id_2 = CODING_ISO_DESIGNATION (coding, 2);
	      if (charset_id_2 < 0)
		charset = CHARSET_FROM_ID (charset_ascii);
	      else
		charset = CHARSET_FROM_ID (charset_id_2);
	      ONE_MORE_BYTE (c1);
	      if (c1 < 0x20 || (c1 >= 0x80 && c1 < 0xA0))
		goto invalid_code;
	      break;

	    case 'O':		/* invocation of single-shift-3 */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT)
		  || CODING_ISO_DESIGNATION (coding, 3) < 0)
		goto invalid_code;
	      charset_id_3 = CODING_ISO_DESIGNATION (coding, 3);
	      if (charset_id_3 < 0)
		charset = CHARSET_FROM_ID (charset_ascii);
	      else
		charset = CHARSET_FROM_ID (charset_id_3);
	      ONE_MORE_BYTE (c1);
	      if (c1 < 0x20 || (c1 >= 0x80 && c1 < 0xA0))
		goto invalid_code;
	      break;

	    case '0': case '2':	case '3': case '4': /* start composition */
	      if (! (coding->common_flags & CODING_ANNOTATE_COMPOSITION_MASK))
		goto invalid_code;
	      if (last_id != charset_ascii)
		{
		  ADD_CHARSET_DATA (charbuf, char_offset- last_offset, last_id);
		  last_id = charset_ascii;
		  last_offset = char_offset;
		}
	      DECODE_COMPOSITION_START (c1);
	      continue;

	    case '1':		/* end composition */
	      if (cmp_status->state == COMPOSING_NO)
		goto invalid_code;
	      DECODE_COMPOSITION_END ();
	      continue;

	    case '[':		/* specification of direction */
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_DIRECTION))
		goto invalid_code;
	      /* For the moment, nested direction is not supported.
		 So, `coding->mode & CODING_MODE_DIRECTION' zero means
		 left-to-right, and nonzero means right-to-left.  */
	      ONE_MORE_BYTE (c1);
	      switch (c1)
		{
		case ']':	/* end of the current direction */
		  coding->mode &= ~CODING_MODE_DIRECTION;

		case '0':	/* end of the current direction */
		case '1':	/* start of left-to-right direction */
		  ONE_MORE_BYTE (c1);
		  if (c1 == ']')
		    coding->mode &= ~CODING_MODE_DIRECTION;
		  else
		    goto invalid_code;
		  break;

		case '2':	/* start of right-to-left direction */
		  ONE_MORE_BYTE (c1);
		  if (c1 == ']')
		    coding->mode |= CODING_MODE_DIRECTION;
		  else
		    goto invalid_code;
		  break;

		default:
		  goto invalid_code;
		}
	      continue;

	    case '%':
	      ONE_MORE_BYTE (c1);
	      if (c1 == '/')
		{
		  /* CTEXT extended segment:
		     ESC % / [0-4] M L --ENCODING-NAME-- \002 --BYTES--
		     We keep these bytes as is for the moment.
		     They may be decoded by post-read-conversion.  */
		  int dim, M, L;
		  int size;

		  ONE_MORE_BYTE (dim);
		  if (dim < '0' || dim > '4')
		    goto invalid_code;
		  ONE_MORE_BYTE (M);
		  if (M < 128)
		    goto invalid_code;
		  ONE_MORE_BYTE (L);
		  if (L < 128)
		    goto invalid_code;
		  size = ((M - 128) * 128) + (L - 128);
		  if (charbuf + 6 > charbuf_end)
		    goto break_loop;
		  *charbuf++ = ISO_CODE_ESC;
		  *charbuf++ = '%';
		  *charbuf++ = '/';
		  *charbuf++ = dim;
		  *charbuf++ = BYTE8_TO_CHAR (M);
		  *charbuf++ = BYTE8_TO_CHAR (L);
		  CODING_ISO_EXTSEGMENT_LEN (coding) = size;
		}
	      else if (c1 == 'G')
		{
		  /* XFree86 extension for embedding UTF-8 in CTEXT:
		     ESC % G --UTF-8-BYTES-- ESC % @
		     We keep these bytes as is for the moment.
		     They may be decoded by post-read-conversion.  */
		  if (charbuf + 3 > charbuf_end)
		    goto break_loop;
		  *charbuf++ = ISO_CODE_ESC;
		  *charbuf++ = '%';
		  *charbuf++ = 'G';
		  CODING_ISO_EMBEDDED_UTF_8 (coding) = 1;
		}
	      else
		goto invalid_code;
	      continue;
	      break;

	    default:
	      if (! (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_DESIGNATION))
		goto invalid_code;
	      {
		int reg, chars96;

		if (c1 >= 0x28 && c1 <= 0x2B)
		  { /* designation of DIMENSION1_CHARS94 character set */
		    reg = c1 - 0x28, chars96 = 0;
		    ONE_MORE_BYTE (c1);
		  }
		else if (c1 >= 0x2C && c1 <= 0x2F)
		  { /* designation of DIMENSION1_CHARS96 character set */
		    reg = c1 - 0x2C, chars96 = 1;
		    ONE_MORE_BYTE (c1);
		  }
		else
		  goto invalid_code;
		DECODE_DESIGNATION (reg, 1, chars96, c1);
		/* We must update these variables now.  */
		if (reg == 0)
		  charset_id_0 = CODING_ISO_INVOKED_CHARSET (coding, 0);
		else if (reg == 1)
		  charset_id_1 = CODING_ISO_INVOKED_CHARSET (coding, 1);
		if (chars96 < 0)
		  goto invalid_code;
	      }
	      continue;
	    }
	  break;

	default:
	  abort ();
	}

      if (cmp_status->state == COMPOSING_NO
	  && charset->id != charset_ascii
	  && last_id != charset->id)
	{
	  if (last_id != charset_ascii)
	    ADD_CHARSET_DATA (charbuf, char_offset - last_offset, last_id);
	  last_id = charset->id;
	  last_offset = char_offset;
	}

      /* Now we know CHARSET and 1st position code C1 of a character.
         Produce a decoded character while getting 2nd and 3rd
         position codes C2, C3 if necessary.  */
      if (CHARSET_DIMENSION (charset) > 1)
	{
	  ONE_MORE_BYTE (c2);
	  if (c2 < 0x20 || (c2 >= 0x80 && c2 < 0xA0)
	      || ((c1 & 0x80) != (c2 & 0x80)))
	    /* C2 is not in a valid range.  */
	    goto invalid_code;
	  if (CHARSET_DIMENSION (charset) == 2)
	    c1 = (c1 << 8) | c2;
	  else
	    {
	      ONE_MORE_BYTE (c3);
	      if (c3 < 0x20 || (c3 >= 0x80 && c3 < 0xA0)
		  || ((c1 & 0x80) != (c3 & 0x80)))
		/* C3 is not in a valid range.  */
		goto invalid_code;
	      c1 = (c1 << 16) | (c2 << 8) | c2;
	    }
	}
      c1 &= 0x7F7F7F;
      CODING_DECODE_CHAR (coding, src, src_base, src_end, charset, c1, c);
      if (c < 0)
	{
	  MAYBE_FINISH_COMPOSITION ();
	  for (; src_base < src; src_base++, char_offset++)
	    {
	      if (ASCII_BYTE_P (*src_base))
		*charbuf++ = *src_base;
	      else
		*charbuf++ = BYTE8_TO_CHAR (*src_base);
	    }
	}
      else if (cmp_status->state == COMPOSING_NO)
	{
	  *charbuf++ = c;
	  char_offset++;
	}
      else if ((cmp_status->state == COMPOSING_CHAR
		? cmp_status->nchars
		: cmp_status->ncomps)
	       >= MAX_COMPOSITION_COMPONENTS)
	{
	  /* Too long composition.  */
	  MAYBE_FINISH_COMPOSITION ();
	  *charbuf++ = c;
	  char_offset++;
	}
      else
	STORE_COMPOSITION_CHAR (c);
      continue;

    invalid_code:
      MAYBE_FINISH_COMPOSITION ();
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = c < 0 ? -c : ASCII_BYTE_P (c) ? c : BYTE8_TO_CHAR (c);
      char_offset++;
      coding->errors++;
      continue;

    break_loop:
      break;
    }

 no_more_source:
  if (cmp_status->state != COMPOSING_NO)
    {
      if (coding->mode & CODING_MODE_LAST_BLOCK)
	MAYBE_FINISH_COMPOSITION ();
      else
	{
	  charbuf -= cmp_status->length;
	  for (i = 0; i < cmp_status->length; i++)
	    cmp_status->carryover[i] = charbuf[i];
	}
    }
  else if (last_id != charset_ascii)
    ADD_CHARSET_DATA (charbuf, char_offset - last_offset, last_id);
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}


/* ISO2022 encoding stuff.  */

/*
   It is not enough to say just "ISO2022" on encoding, we have to
   specify more details.  In Emacs, each coding system of ISO2022
   variant has the following specifications:
	1. Initial designation to G0 thru G3.
	2. Allows short-form designation?
	3. ASCII should be designated to G0 before control characters?
	4. ASCII should be designated to G0 at end of line?
	5. 7-bit environment or 8-bit environment?
	6. Use locking-shift?
	7. Use Single-shift?
   And the following two are only for Japanese:
	8. Use ASCII in place of JIS0201-1976-Roman?
	9. Use JISX0208-1983 in place of JISX0208-1978?
   These specifications are encoded in CODING_ISO_FLAGS (coding) as flag bits
   defined by macros CODING_ISO_FLAG_XXX.  See `coding.h' for more
   details.
*/

/* Produce codes (escape sequence) for designating CHARSET to graphic
   register REG at DST, and increment DST.  If <final-char> of CHARSET is
   '@', 'A', or 'B' and the coding system CODING allows, produce
   designation sequence of short-form.  */

#define ENCODE_DESIGNATION(charset, reg, coding)			\
  do {									\
    unsigned char final_char = CHARSET_ISO_FINAL (charset);		\
    const char *intermediate_char_94 = "()*+";				\
    const char *intermediate_char_96 = ",-./";				\
    int revision = -1;							\
									\
    if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_REVISION)		\
      revision = CHARSET_ISO_REVISION (charset);			\
									\
    if (revision >= 0)							\
      {									\
	EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, '&');			\
	EMIT_ONE_BYTE ('@' + revision);					\
      }									\
    EMIT_ONE_ASCII_BYTE (ISO_CODE_ESC);					\
    if (CHARSET_DIMENSION (charset) == 1)				\
      {									\
	int b;								\
	if (! CHARSET_ISO_CHARS_96 (charset))				\
	  b = intermediate_char_94[reg];				\
	else								\
	  b = intermediate_char_96[reg];				\
	EMIT_ONE_ASCII_BYTE (b);					\
      }									\
    else								\
      {									\
	EMIT_ONE_ASCII_BYTE ('$');					\
	if (! CHARSET_ISO_CHARS_96 (charset))				\
	  {								\
	    if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_LONG_FORM	\
		|| reg != 0						\
		|| final_char < '@' || final_char > 'B')		\
	      EMIT_ONE_ASCII_BYTE (intermediate_char_94[reg]);		\
	  }								\
	else								\
	  EMIT_ONE_ASCII_BYTE (intermediate_char_96[reg]);		\
      }									\
    EMIT_ONE_ASCII_BYTE (final_char);					\
									\
    CODING_ISO_DESIGNATION (coding, reg) = CHARSET_ID (charset);	\
  } while (0)


/* The following two macros produce codes (control character or escape
   sequence) for ISO2022 single-shift functions (single-shift-2 and
   single-shift-3).  */

#define ENCODE_SINGLE_SHIFT_2						\
  do {									\
    if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS)		\
      EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, 'N');				\
    else								\
      EMIT_ONE_BYTE (ISO_CODE_SS2);					\
    CODING_ISO_SINGLE_SHIFTING (coding) = 1;				\
  } while (0)


#define ENCODE_SINGLE_SHIFT_3						\
  do {									\
    if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS)		\
      EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, 'O');				\
    else								\
      EMIT_ONE_BYTE (ISO_CODE_SS3);					\
    CODING_ISO_SINGLE_SHIFTING (coding) = 1;				\
  } while (0)


/* The following four macros produce codes (control character or
   escape sequence) for ISO2022 locking-shift functions (shift-in,
   shift-out, locking-shift-2, and locking-shift-3).  */

#define ENCODE_SHIFT_IN					\
  do {							\
    EMIT_ONE_ASCII_BYTE (ISO_CODE_SI);			\
    CODING_ISO_INVOCATION (coding, 0) = 0;		\
  } while (0)


#define ENCODE_SHIFT_OUT				\
  do {							\
    EMIT_ONE_ASCII_BYTE (ISO_CODE_SO);			\
    CODING_ISO_INVOCATION (coding, 0) = 1;		\
  } while (0)


#define ENCODE_LOCKING_SHIFT_2				\
  do {							\
    EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, 'n');		\
    CODING_ISO_INVOCATION (coding, 0) = 2;		\
  } while (0)


#define ENCODE_LOCKING_SHIFT_3				\
  do {							\
    EMIT_TWO_ASCII_BYTES (ISO_CODE_ESC, 'n');		\
    CODING_ISO_INVOCATION (coding, 0) = 3;		\
  } while (0)


/* Produce codes for a DIMENSION1 character whose character set is
   CHARSET and whose position-code is C1.  Designation and invocation
   sequences are also produced in advance if necessary.  */

#define ENCODE_ISO_CHARACTER_DIMENSION1(charset, c1)			\
  do {									\
    int id = CHARSET_ID (charset);					\
									\
    if ((CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_USE_ROMAN)		\
	&& id == charset_ascii)						\
      {									\
	id = charset_jisx0201_roman;					\
	charset = CHARSET_FROM_ID (id);					\
      }									\
									\
    if (CODING_ISO_SINGLE_SHIFTING (coding))				\
      {									\
	if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS)	\
	  EMIT_ONE_ASCII_BYTE (c1 & 0x7F);				\
	else								\
	  EMIT_ONE_BYTE (c1 | 0x80);					\
	CODING_ISO_SINGLE_SHIFTING (coding) = 0;			\
	break;								\
      }									\
    else if (id == CODING_ISO_INVOKED_CHARSET (coding, 0))		\
      {									\
	EMIT_ONE_ASCII_BYTE (c1 & 0x7F);				\
	break;								\
      }									\
    else if (id == CODING_ISO_INVOKED_CHARSET (coding, 1))		\
      {									\
	EMIT_ONE_BYTE (c1 | 0x80);					\
	break;								\
      }									\
    else								\
      /* Since CHARSET is not yet invoked to any graphic planes, we	\
	 must invoke it, or, at first, designate it to some graphic	\
	 register.  Then repeat the loop to actually produce the	\
	 character.  */							\
      dst = encode_invocation_designation (charset, coding, dst,	\
					   &produced_chars);		\
  } while (1)


/* Produce codes for a DIMENSION2 character whose character set is
   CHARSET and whose position-codes are C1 and C2.  Designation and
   invocation codes are also produced in advance if necessary.  */

#define ENCODE_ISO_CHARACTER_DIMENSION2(charset, c1, c2)		\
  do {									\
    int id = CHARSET_ID (charset);					\
									\
    if ((CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_USE_OLDJIS)	\
	&& id == charset_jisx0208)					\
      {									\
	id = charset_jisx0208_1978;					\
	charset = CHARSET_FROM_ID (id);					\
      }									\
									\
    if (CODING_ISO_SINGLE_SHIFTING (coding))				\
      {									\
	if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SEVEN_BITS)	\
	  EMIT_TWO_ASCII_BYTES ((c1) & 0x7F, (c2) & 0x7F);		\
	else								\
	  EMIT_TWO_BYTES ((c1) | 0x80, (c2) | 0x80);			\
	CODING_ISO_SINGLE_SHIFTING (coding) = 0;			\
	break;								\
      }									\
    else if (id == CODING_ISO_INVOKED_CHARSET (coding, 0))		\
      {									\
	EMIT_TWO_ASCII_BYTES ((c1) & 0x7F, (c2) & 0x7F);		\
	break;								\
      }									\
    else if (id == CODING_ISO_INVOKED_CHARSET (coding, 1))		\
      {									\
	EMIT_TWO_BYTES ((c1) | 0x80, (c2) | 0x80);			\
	break;								\
      }									\
    else								\
      /* Since CHARSET is not yet invoked to any graphic planes, we	\
	 must invoke it, or, at first, designate it to some graphic	\
	 register.  Then repeat the loop to actually produce the	\
	 character.  */							\
      dst = encode_invocation_designation (charset, coding, dst,	\
					   &produced_chars);		\
  } while (1)


#define ENCODE_ISO_CHARACTER(charset, c)				   \
  do {									   \
    int code;								   \
    CODING_ENCODE_CHAR (coding, dst, dst_end, (charset), (c), code);	   \
									   \
    if (CHARSET_DIMENSION (charset) == 1)				   \
      ENCODE_ISO_CHARACTER_DIMENSION1 ((charset), code);		   \
    else								   \
      ENCODE_ISO_CHARACTER_DIMENSION2 ((charset), code >> 8, code & 0xFF); \
  } while (0)


/* Produce designation and invocation codes at a place pointed by DST
   to use CHARSET.  The element `spec.iso_2022' of *CODING is updated.
   Return new DST.  */

static unsigned char *
encode_invocation_designation (struct charset *charset,
			       struct coding_system *coding,
			       unsigned char *dst, EMACS_INT *p_nchars)
{
  int multibytep = coding->dst_multibyte;
  EMACS_INT produced_chars = *p_nchars;
  int reg;			/* graphic register number */
  int id = CHARSET_ID (charset);

  /* At first, check designations.  */
  for (reg = 0; reg < 4; reg++)
    if (id == CODING_ISO_DESIGNATION (coding, reg))
      break;

  if (reg >= 4)
    {
      /* CHARSET is not yet designated to any graphic registers.  */
      /* At first check the requested designation.  */
      reg = CODING_ISO_REQUEST (coding, id);
      if (reg < 0)
	/* Since CHARSET requests no special designation, designate it
	   to graphic register 0.  */
	reg = 0;

      ENCODE_DESIGNATION (charset, reg, coding);
    }

  if (CODING_ISO_INVOCATION (coding, 0) != reg
      && CODING_ISO_INVOCATION (coding, 1) != reg)
    {
      /* Since the graphic register REG is not invoked to any graphic
	 planes, invoke it to graphic plane 0.  */
      switch (reg)
	{
	case 0:			/* graphic register 0 */
	  ENCODE_SHIFT_IN;
	  break;

	case 1:			/* graphic register 1 */
	  ENCODE_SHIFT_OUT;
	  break;

	case 2:			/* graphic register 2 */
	  if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT)
	    ENCODE_SINGLE_SHIFT_2;
	  else
	    ENCODE_LOCKING_SHIFT_2;
	  break;

	case 3:			/* graphic register 3 */
	  if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_SINGLE_SHIFT)
	    ENCODE_SINGLE_SHIFT_3;
	  else
	    ENCODE_LOCKING_SHIFT_3;
	  break;
	}
    }

  *p_nchars = produced_chars;
  return dst;
}


/* Produce codes for designation and invocation to reset the graphic
   planes and registers to initial state.  */
#define ENCODE_RESET_PLANE_AND_REGISTER()				\
  do {									\
    int reg;								\
    struct charset *charset;						\
									\
    if (CODING_ISO_INVOCATION (coding, 0) != 0)				\
      ENCODE_SHIFT_IN;							\
    for (reg = 0; reg < 4; reg++)					\
      if (CODING_ISO_INITIAL (coding, reg) >= 0				\
	  && (CODING_ISO_DESIGNATION (coding, reg)			\
	      != CODING_ISO_INITIAL (coding, reg)))			\
	{								\
	  charset = CHARSET_FROM_ID (CODING_ISO_INITIAL (coding, reg));	\
	  ENCODE_DESIGNATION (charset, reg, coding);			\
	}								\
  } while (0)


/* Produce designation sequences of charsets in the line started from
   CHARBUF to a place pointed by DST, and return the number of
   produced bytes.  DST should not directly point a buffer text area
   which may be relocated by char_charset call.

   If the current block ends before any end-of-line, we may fail to
   find all the necessary designations.  */

static EMACS_INT
encode_designation_at_bol (struct coding_system *coding,
			   int *charbuf, int *charbuf_end,
			   unsigned char *dst)
{
  unsigned char *orig = dst;
  struct charset *charset;
  /* Table of charsets to be designated to each graphic register.  */
  int r[4];
  int c, found = 0, reg;
  EMACS_INT produced_chars = 0;
  int multibytep = coding->dst_multibyte;
  Lisp_Object attrs;
  Lisp_Object charset_list;

  attrs = CODING_ID_ATTRS (coding->id);
  charset_list = CODING_ATTR_CHARSET_LIST (attrs);
  if (EQ (charset_list, Qiso_2022))
    charset_list = Viso_2022_charset_list;

  for (reg = 0; reg < 4; reg++)
    r[reg] = -1;

  while (charbuf < charbuf_end && found < 4)
    {
      int id;

      c = *charbuf++;
      if (c == '\n')
	break;
      charset = char_charset (c, charset_list, NULL);
      id = CHARSET_ID (charset);
      reg = CODING_ISO_REQUEST (coding, id);
      if (reg >= 0 && r[reg] < 0)
	{
	  found++;
	  r[reg] = id;
	}
    }

  if (found)
    {
      for (reg = 0; reg < 4; reg++)
	if (r[reg] >= 0
	    && CODING_ISO_DESIGNATION (coding, reg) != r[reg])
	  ENCODE_DESIGNATION (CHARSET_FROM_ID (r[reg]), reg, coding);
    }

  return dst - orig;
}

/* See the above "GENERAL NOTES on `encode_coding_XXX ()' functions".  */

static int
encode_coding_iso_2022 (struct coding_system *coding)
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = 16;
  int bol_designation
    = (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_DESIGNATE_AT_BOL
       && CODING_ISO_BOL (coding));
  EMACS_INT produced_chars = 0;
  Lisp_Object attrs, eol_type, charset_list;
  int ascii_compatible;
  int c;
  int preferred_charset_id = -1;

  CODING_GET_INFO (coding, attrs, charset_list);
  eol_type = inhibit_eol_conversion ? Qunix : CODING_ID_EOL_TYPE (coding->id);
  if (VECTORP (eol_type))
    eol_type = Qunix;

  setup_iso_safe_charsets (attrs);
  /* Charset list may have been changed.  */
  charset_list = CODING_ATTR_CHARSET_LIST (attrs);
  coding->safe_charsets = SDATA (CODING_ATTR_SAFE_CHARSETS (attrs));

  ascii_compatible
    = (! NILP (CODING_ATTR_ASCII_COMPAT (attrs))
       && ! (CODING_ISO_FLAGS (coding) & (CODING_ISO_FLAG_DESIGNATION
					  | CODING_ISO_FLAG_LOCKING_SHIFT)));

  while (charbuf < charbuf_end)
    {
      ASSURE_DESTINATION (safe_room);

      if (bol_designation)
	{
	  /* We have to produce designation sequences if any now.  */
	  unsigned char desig_buf[16];
	  int nbytes;
	  EMACS_INT offset;

	  charset_map_loaded = 0;
	  nbytes = encode_designation_at_bol (coding, charbuf, charbuf_end,
					      desig_buf);
	  if (charset_map_loaded
	      && (offset = coding_set_destination (coding)))
	    {
	      dst += offset;
	      dst_end += offset;
	    }
	  memcpy (dst, desig_buf, nbytes);
	  dst += nbytes;
	  /* We are sure that designation sequences are all ASCII bytes.  */
	  produced_chars += nbytes;
	  bol_designation = 0;
	  ASSURE_DESTINATION (safe_room);
	}

      c = *charbuf++;

      if (c < 0)
	{
	  /* Handle an annotation.  */
	  switch (*charbuf)
	    {
	    case CODING_ANNOTATE_COMPOSITION_MASK:
	      /* Not yet implemented.  */
	      break;
	    case CODING_ANNOTATE_CHARSET_MASK:
	      preferred_charset_id = charbuf[2];
	      if (preferred_charset_id >= 0
		  && NILP (Fmemq (make_number (preferred_charset_id),
				  charset_list)))
		preferred_charset_id = -1;
	      break;
	    default:
	      abort ();
	    }
	  charbuf += -c - 1;
	  continue;
	}

      /* Now encode the character C.  */
      if (c < 0x20 || c == 0x7F)
	{
	  if (c == '\n'
	      || (c == '\r' && EQ (eol_type, Qmac)))
	    {
	      if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_RESET_AT_EOL)
		ENCODE_RESET_PLANE_AND_REGISTER ();
	      if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_INIT_AT_BOL)
		{
		  int i;

		  for (i = 0; i < 4; i++)
		    CODING_ISO_DESIGNATION (coding, i)
		      = CODING_ISO_INITIAL (coding, i);
		}
	      bol_designation
		= CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_DESIGNATE_AT_BOL;
	    }
	  else if (CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_RESET_AT_CNTL)
	    ENCODE_RESET_PLANE_AND_REGISTER ();
	  EMIT_ONE_ASCII_BYTE (c);
	}
      else if (ASCII_CHAR_P (c))
	{
	  if (ascii_compatible)
	    EMIT_ONE_ASCII_BYTE (c);
	  else
	    {
	      struct charset *charset = CHARSET_FROM_ID (charset_ascii);
	      ENCODE_ISO_CHARACTER (charset, c);
	    }
	}
      else if (CHAR_BYTE8_P (c))
	{
	  c = CHAR_TO_BYTE8 (c);
	  EMIT_ONE_BYTE (c);
	}
      else
	{
	  struct charset *charset;

	  if (preferred_charset_id >= 0)
	    {
	      int result;

	      charset = CHARSET_FROM_ID (preferred_charset_id);
	      CODING_CHAR_CHARSET_P (coding, dst, dst_end, c, charset, result);
	      if (! result)
		CODING_CHAR_CHARSET (coding, dst, dst_end, c, charset_list,
				     NULL, charset);
	    }
	  else
	    CODING_CHAR_CHARSET (coding, dst, dst_end, c, charset_list,
				 NULL, charset);
	  if (!charset)
	    {
	      if (coding->mode & CODING_MODE_SAFE_ENCODING)
		{
		  c = CODING_INHIBIT_CHARACTER_SUBSTITUTION;
		  charset = CHARSET_FROM_ID (charset_ascii);
		}
	      else
		{
		  c = coding->default_char;
		  CODING_CHAR_CHARSET (coding, dst, dst_end, c,
				       charset_list, NULL, charset);
		}
	    }
	  ENCODE_ISO_CHARACTER (charset, c);
	}
    }

  if (coding->mode & CODING_MODE_LAST_BLOCK
      && CODING_ISO_FLAGS (coding) & CODING_ISO_FLAG_RESET_AT_EOL)
    {
      ASSURE_DESTINATION (safe_room);
      ENCODE_RESET_PLANE_AND_REGISTER ();
    }
  record_conversion_result (coding, CODING_RESULT_SUCCESS);
  CODING_ISO_BOL (coding) = bol_designation;
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}


/*** 8,9. SJIS and BIG5 handlers ***/

/* Although SJIS and BIG5 are not ISO's coding system, they are used
   quite widely.  So, for the moment, Emacs supports them in the bare
   C code.  But, in the future, they may be supported only by CCL.  */

/* SJIS is a coding system encoding three character sets: ASCII, right
   half of JISX0201-Kana, and JISX0208.  An ASCII character is encoded
   as is.  A character of charset katakana-jisx0201 is encoded by
   "position-code + 0x80".  A character of charset japanese-jisx0208
   is encoded in 2-byte but two position-codes are divided and shifted
   so that it fit in the range below.

   --- CODE RANGE of SJIS ---
   (character set)	(range)
   ASCII		0x00 .. 0x7F
   KATAKANA-JISX0201	0xA0 .. 0xDF
   JISX0208 (1st byte)	0x81 .. 0x9F and 0xE0 .. 0xEF
	    (2nd byte)	0x40 .. 0x7E and 0x80 .. 0xFC
   -------------------------------

*/

/* BIG5 is a coding system encoding two character sets: ASCII and
   Big5.  An ASCII character is encoded as is.  Big5 is a two-byte
   character set and is encoded in two-byte.

   --- CODE RANGE of BIG5 ---
   (character set)	(range)
   ASCII		0x00 .. 0x7F
   Big5 (1st byte)	0xA1 .. 0xFE
	(2nd byte)	0x40 .. 0x7E and 0xA1 .. 0xFE
   --------------------------

  */

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in SJIS.  If it is, return
   CATEGORY_MASK_SJIS, else return 0.  */

static int
detect_coding_sjis (struct coding_system *coding,
		    struct coding_detection_info *detect_info)
{
  const unsigned char *src = coding->source, *src_base;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  EMACS_INT consumed_chars = 0;
  int found = 0;
  int c;
  Lisp_Object attrs, charset_list;
  int max_first_byte_of_2_byte_code;

  CODING_GET_INFO (coding, attrs, charset_list);
  max_first_byte_of_2_byte_code
    = (XINT (Flength (charset_list)) > 3 ? 0xFC : 0xEF);

  detect_info->checked |= CATEGORY_MASK_SJIS;
  /* A coding system of this category is always ASCII compatible.  */
  src += coding->head_ascii;

  while (1)
    {
      src_base = src;
      ONE_MORE_BYTE (c);
      if (c < 0x80)
	continue;
      if ((c >= 0x81 && c <= 0x9F)
	  || (c >= 0xE0 && c <= max_first_byte_of_2_byte_code))
	{
	  ONE_MORE_BYTE (c);
	  if (c < 0x40 || c == 0x7F || c > 0xFC)
	    break;
	  found = CATEGORY_MASK_SJIS;
	}
      else if (c >= 0xA0 && c < 0xE0)
	found = CATEGORY_MASK_SJIS;
      else
	break;
    }
  detect_info->rejected |= CATEGORY_MASK_SJIS;
  return 0;

 no_more_source:
  if (src_base < src && coding->mode & CODING_MODE_LAST_BLOCK)
    {
      detect_info->rejected |= CATEGORY_MASK_SJIS;
      return 0;
    }
  detect_info->found |= found;
  return 1;
}

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in BIG5.  If it is, return
   CATEGORY_MASK_BIG5, else return 0.  */

static int
detect_coding_big5 (struct coding_system *coding,
		    struct coding_detection_info *detect_info)
{
  const unsigned char *src = coding->source, *src_base;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  EMACS_INT consumed_chars = 0;
  int found = 0;
  int c;

  detect_info->checked |= CATEGORY_MASK_BIG5;
  /* A coding system of this category is always ASCII compatible.  */
  src += coding->head_ascii;

  while (1)
    {
      src_base = src;
      ONE_MORE_BYTE (c);
      if (c < 0x80)
	continue;
      if (c >= 0xA1)
	{
	  ONE_MORE_BYTE (c);
	  if (c < 0x40 || (c >= 0x7F && c <= 0xA0))
	    return 0;
	  found = CATEGORY_MASK_BIG5;
	}
      else
	break;
    }
  detect_info->rejected |= CATEGORY_MASK_BIG5;
  return 0;

 no_more_source:
  if (src_base < src && coding->mode & CODING_MODE_LAST_BLOCK)
    {
      detect_info->rejected |= CATEGORY_MASK_BIG5;
      return 0;
    }
  detect_info->found |= found;
  return 1;
}

/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".
   If SJIS_P is 1, decode SJIS text, else decode BIG5 test.  */

static void
decode_coding_sjis (struct coding_system *coding)
{
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  const unsigned char *src_base;
  int *charbuf = coding->charbuf + coding->charbuf_used;
  /* We may produce one charset annotation in one loop and one more at
     the end.  */
  int *charbuf_end
    = coding->charbuf + coding->charbuf_size - (MAX_ANNOTATION_LENGTH * 2);
  EMACS_INT consumed_chars = 0, consumed_chars_base;
  int multibytep = coding->src_multibyte;
  struct charset *charset_roman, *charset_kanji, *charset_kana;
  struct charset *charset_kanji2;
  Lisp_Object attrs, charset_list, val;
  EMACS_INT char_offset = coding->produced_char;
  EMACS_INT last_offset = char_offset;
  int last_id = charset_ascii;
  int eol_dos =
    !inhibit_eol_conversion && EQ (CODING_ID_EOL_TYPE (coding->id), Qdos);
  int byte_after_cr = -1;

  CODING_GET_INFO (coding, attrs, charset_list);

  val = charset_list;
  charset_roman = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kana = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kanji = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kanji2 = NILP (val) ? NULL : CHARSET_FROM_ID (XINT (XCAR (val)));

  while (1)
    {
      int c, c1;
      struct charset *charset;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	{
	  if (byte_after_cr >= 0)
	    src_base--;
	  break;
	}

      if (byte_after_cr >= 0)
	c = byte_after_cr, byte_after_cr = -1;
      else
	ONE_MORE_BYTE (c);
      if (c < 0)
	goto invalid_code;
      if (c < 0x80)
	{
	  if (eol_dos && c == '\r')
	    ONE_MORE_BYTE (byte_after_cr);
	  charset = charset_roman;
	}
      else if (c == 0x80 || c == 0xA0)
	goto invalid_code;
      else if (c >= 0xA1 && c <= 0xDF)
	{
	  /* SJIS -> JISX0201-Kana */
	  c &= 0x7F;
	  charset = charset_kana;
	}
      else if (c <= 0xEF)
	{
	  /* SJIS -> JISX0208 */
	  ONE_MORE_BYTE (c1);
	  if (c1 < 0x40 || c1 == 0x7F || c1 > 0xFC)
	    goto invalid_code;
	  c = (c << 8) | c1;
	  SJIS_TO_JIS (c);
	  charset = charset_kanji;
	}
      else if (c <= 0xFC && charset_kanji2)
	{
	  /* SJIS -> JISX0213-2 */
	  ONE_MORE_BYTE (c1);
	  if (c1 < 0x40 || c1 == 0x7F || c1 > 0xFC)
	    goto invalid_code;
	  c = (c << 8) | c1;
	  SJIS_TO_JIS2 (c);
	  charset = charset_kanji2;
	}
      else
	goto invalid_code;
      if (charset->id != charset_ascii
	  && last_id != charset->id)
	{
	  if (last_id != charset_ascii)
	    ADD_CHARSET_DATA (charbuf, char_offset - last_offset, last_id);
	  last_id = charset->id;
	  last_offset = char_offset;
	}
      CODING_DECODE_CHAR (coding, src, src_base, src_end, charset, c, c);
      *charbuf++ = c;
      char_offset++;
      continue;

    invalid_code:
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = c < 0 ? -c : BYTE8_TO_CHAR (c);
      char_offset++;
      coding->errors++;
    }

 no_more_source:
  if (last_id != charset_ascii)
    ADD_CHARSET_DATA (charbuf, char_offset - last_offset, last_id);
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}

static void
decode_coding_big5 (struct coding_system *coding)
{
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  const unsigned char *src_base;
  int *charbuf = coding->charbuf + coding->charbuf_used;
  /* We may produce one charset annotation in one loop and one more at
     the end.  */
  int *charbuf_end
    = coding->charbuf + coding->charbuf_size - (MAX_ANNOTATION_LENGTH * 2);
  EMACS_INT consumed_chars = 0, consumed_chars_base;
  int multibytep = coding->src_multibyte;
  struct charset *charset_roman, *charset_big5;
  Lisp_Object attrs, charset_list, val;
  EMACS_INT char_offset = coding->produced_char;
  EMACS_INT last_offset = char_offset;
  int last_id = charset_ascii;
  int eol_dos =
    !inhibit_eol_conversion && EQ (CODING_ID_EOL_TYPE (coding->id), Qdos);
  int byte_after_cr = -1;

  CODING_GET_INFO (coding, attrs, charset_list);
  val = charset_list;
  charset_roman = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_big5 = CHARSET_FROM_ID (XINT (XCAR (val)));

  while (1)
    {
      int c, c1;
      struct charset *charset;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	{
	  if (byte_after_cr >= 0)
	    src_base--;
	  break;
	}

      if (byte_after_cr >= 0)
	c = byte_after_cr, byte_after_cr = -1;
      else
	ONE_MORE_BYTE (c);

      if (c < 0)
	goto invalid_code;
      if (c < 0x80)
	{
	  if (eol_dos && c == '\r')
	    ONE_MORE_BYTE (byte_after_cr);
	  charset = charset_roman;
	}
      else
	{
	  /* BIG5 -> Big5 */
	  if (c < 0xA1 || c > 0xFE)
	    goto invalid_code;
	  ONE_MORE_BYTE (c1);
	  if (c1 < 0x40 || (c1 > 0x7E && c1 < 0xA1) || c1 > 0xFE)
	    goto invalid_code;
	  c = c << 8 | c1;
	  charset = charset_big5;
	}
      if (charset->id != charset_ascii
	  && last_id != charset->id)
	{
	  if (last_id != charset_ascii)
	    ADD_CHARSET_DATA (charbuf, char_offset - last_offset, last_id);
	  last_id = charset->id;
	  last_offset = char_offset;
	}
      CODING_DECODE_CHAR (coding, src, src_base, src_end, charset, c, c);
      *charbuf++ = c;
      char_offset++;
      continue;

    invalid_code:
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = c < 0 ? -c : BYTE8_TO_CHAR (c);
      char_offset++;
      coding->errors++;
    }

 no_more_source:
  if (last_id != charset_ascii)
    ADD_CHARSET_DATA (charbuf, char_offset - last_offset, last_id);
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}

/* See the above "GENERAL NOTES on `encode_coding_XXX ()' functions".
   This function can encode charsets `ascii', `katakana-jisx0201',
   `japanese-jisx0208', `chinese-big5-1', and `chinese-big5-2'.  We
   are sure that all these charsets are registered as official charset
   (i.e. do not have extended leading-codes).  Characters of other
   charsets are produced without any encoding.  If SJIS_P is 1, encode
   SJIS text, else encode BIG5 text.  */

static int
encode_coding_sjis (struct coding_system *coding)
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = 4;
  EMACS_INT produced_chars = 0;
  Lisp_Object attrs, charset_list, val;
  int ascii_compatible;
  struct charset *charset_kanji, *charset_kana;
  struct charset *charset_kanji2;
  int c;

  CODING_GET_INFO (coding, attrs, charset_list);
  val = XCDR (charset_list);
  charset_kana = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kanji = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kanji2 = NILP (val) ? NULL : CHARSET_FROM_ID (XINT (XCAR (val)));

  ascii_compatible = ! NILP (CODING_ATTR_ASCII_COMPAT (attrs));

  while (charbuf < charbuf_end)
    {
      ASSURE_DESTINATION (safe_room);
      c = *charbuf++;
      /* Now encode the character C.  */
      if (ASCII_CHAR_P (c) && ascii_compatible)
	EMIT_ONE_ASCII_BYTE (c);
      else if (CHAR_BYTE8_P (c))
	{
	  c = CHAR_TO_BYTE8 (c);
	  EMIT_ONE_BYTE (c);
	}
      else
	{
	  unsigned code;
	  struct charset *charset;
	  CODING_CHAR_CHARSET (coding, dst, dst_end, c, charset_list,
			       &code, charset);

	  if (!charset)
	    {
	      if (coding->mode & CODING_MODE_SAFE_ENCODING)
		{
		  code = CODING_INHIBIT_CHARACTER_SUBSTITUTION;
		  charset = CHARSET_FROM_ID (charset_ascii);
		}
	      else
		{
		  c = coding->default_char;
		  CODING_CHAR_CHARSET (coding, dst, dst_end, c,
				       charset_list, &code, charset);
		}
	    }
	  if (code == CHARSET_INVALID_CODE (charset))
	    abort ();
	  if (charset == charset_kanji)
	    {
	      int c1, c2;
	      JIS_TO_SJIS (code);
	      c1 = code >> 8, c2 = code & 0xFF;
	      EMIT_TWO_BYTES (c1, c2);
	    }
	  else if (charset == charset_kana)
	    EMIT_ONE_BYTE (code | 0x80);
	  else if (charset_kanji2 && charset == charset_kanji2)
	    {
	      int c1, c2;

	      c1 = code >> 8;
	      if (c1 == 0x21 || (c1 >= 0x23 && c1 <= 0x25)
		  || c1 == 0x28
		  || (c1 >= 0x2C && c1 <= 0x2F) || c1 >= 0x6E)
		{
		  JIS_TO_SJIS2 (code);
		  c1 = code >> 8, c2 = code & 0xFF;
		  EMIT_TWO_BYTES (c1, c2);
		}
	      else
		EMIT_ONE_ASCII_BYTE (code & 0x7F);
	    }
	  else
	    EMIT_ONE_ASCII_BYTE (code & 0x7F);
	}
    }
  record_conversion_result (coding, CODING_RESULT_SUCCESS);
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}

static int
encode_coding_big5 (struct coding_system *coding)
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = 4;
  EMACS_INT produced_chars = 0;
  Lisp_Object attrs, charset_list, val;
  int ascii_compatible;
  struct charset *charset_big5;
  int c;

  CODING_GET_INFO (coding, attrs, charset_list);
  val = XCDR (charset_list);
  charset_big5 = CHARSET_FROM_ID (XINT (XCAR (val)));
  ascii_compatible = ! NILP (CODING_ATTR_ASCII_COMPAT (attrs));

  while (charbuf < charbuf_end)
    {
      ASSURE_DESTINATION (safe_room);
      c = *charbuf++;
      /* Now encode the character C.  */
      if (ASCII_CHAR_P (c) && ascii_compatible)
	EMIT_ONE_ASCII_BYTE (c);
      else if (CHAR_BYTE8_P (c))
	{
	  c = CHAR_TO_BYTE8 (c);
	  EMIT_ONE_BYTE (c);
	}
      else
	{
	  unsigned code;
	  struct charset *charset;
	  CODING_CHAR_CHARSET (coding, dst, dst_end, c, charset_list,
			       &code, charset);

	  if (! charset)
	    {
	      if (coding->mode & CODING_MODE_SAFE_ENCODING)
		{
		  code = CODING_INHIBIT_CHARACTER_SUBSTITUTION;
		  charset = CHARSET_FROM_ID (charset_ascii);
		}
	      else
		{
		  c = coding->default_char;
		  CODING_CHAR_CHARSET (coding, dst, dst_end, c,
				       charset_list, &code, charset);
		}
	    }
	  if (code == CHARSET_INVALID_CODE (charset))
	    abort ();
	  if (charset == charset_big5)
	    {
	      int c1, c2;

	      c1 = code >> 8, c2 = code & 0xFF;
	      EMIT_TWO_BYTES (c1, c2);
	    }
	  else
	    EMIT_ONE_ASCII_BYTE (code & 0x7F);
	}
    }
  record_conversion_result (coding, CODING_RESULT_SUCCESS);
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}


/*** 10. CCL handlers ***/

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in a coding system of which
   encoder/decoder are written in CCL program.  If it is, return
   CATEGORY_MASK_CCL, else return 0.  */

static int
detect_coding_ccl (struct coding_system *coding,
		   struct coding_detection_info *detect_info)
{
  const unsigned char *src = coding->source, *src_base;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  EMACS_INT consumed_chars = 0;
  int found = 0;
  unsigned char *valids;
  EMACS_INT head_ascii = coding->head_ascii;
  Lisp_Object attrs;

  detect_info->checked |= CATEGORY_MASK_CCL;

  coding = &coding_categories[coding_category_ccl];
  valids = CODING_CCL_VALIDS (coding);
  attrs = CODING_ID_ATTRS (coding->id);
  if (! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    src += head_ascii;

  while (1)
    {
      int c;

      src_base = src;
      ONE_MORE_BYTE (c);
      if (c < 0 || ! valids[c])
	break;
      if ((valids[c] > 1))
	found = CATEGORY_MASK_CCL;
    }
  detect_info->rejected |= CATEGORY_MASK_CCL;
  return 0;

 no_more_source:
  detect_info->found |= found;
  return 1;
}

static void
decode_coding_ccl (struct coding_system *coding)
{
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  int *charbuf = coding->charbuf + coding->charbuf_used;
  int *charbuf_end = coding->charbuf + coding->charbuf_size;
  EMACS_INT consumed_chars = 0;
  int multibytep = coding->src_multibyte;
  struct ccl_program *ccl = &coding->spec.ccl->ccl;
  int source_charbuf[1024];
  int source_byteidx[1025];
  Lisp_Object attrs, charset_list;

  CODING_GET_INFO (coding, attrs, charset_list);

  while (1)
    {
      const unsigned char *p = src;
      int i = 0;

      if (multibytep)
	{
	  while (i < 1024 && p < src_end)
	    {
	      source_byteidx[i] = p - src;
	      source_charbuf[i++] = STRING_CHAR_ADVANCE (p);
	    }
	  source_byteidx[i] = p - src;
	}
      else
	while (i < 1024 && p < src_end)
	  source_charbuf[i++] = *p++;

      if (p == src_end && coding->mode & CODING_MODE_LAST_BLOCK)
	ccl->last_block = 1;
      ccl_driver (ccl, source_charbuf, charbuf, i, charbuf_end - charbuf,
		  charset_list);
      charbuf += ccl->produced;
      if (multibytep)
	src += source_byteidx[ccl->consumed];
      else
	src += ccl->consumed;
      consumed_chars += ccl->consumed;
      if (p == src_end || ccl->status != CCL_STAT_SUSPEND_BY_SRC)
	break;
    }

  switch (ccl->status)
    {
    case CCL_STAT_SUSPEND_BY_SRC:
      record_conversion_result (coding, CODING_RESULT_INSUFFICIENT_SRC);
      break;
    case CCL_STAT_SUSPEND_BY_DST:
      record_conversion_result (coding, CODING_RESULT_INSUFFICIENT_DST);
      break;
    case CCL_STAT_QUIT:
    case CCL_STAT_INVALID_CMD:
      record_conversion_result (coding, CODING_RESULT_INTERRUPT);
      break;
    default:
      record_conversion_result (coding, CODING_RESULT_SUCCESS);
      break;
    }
  coding->consumed_char += consumed_chars;
  coding->consumed = src - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}

static int
encode_coding_ccl (struct coding_system *coding)
{
  struct ccl_program *ccl = &coding->spec.ccl->ccl;
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int destination_charbuf[1024];
  EMACS_INT produced_chars = 0;
  int i;
  Lisp_Object attrs, charset_list;

  CODING_GET_INFO (coding, attrs, charset_list);
  if (coding->consumed_char == coding->src_chars
      && coding->mode & CODING_MODE_LAST_BLOCK)
    ccl->last_block = 1;

  do
    {
      ccl_driver (ccl, charbuf, destination_charbuf,
		  charbuf_end - charbuf, 1024, charset_list);
      if (multibytep)
	{
	  ASSURE_DESTINATION (ccl->produced * 2);
	  for (i = 0; i < ccl->produced; i++)
	    EMIT_ONE_BYTE (destination_charbuf[i] & 0xFF);
	}
      else
	{
	  ASSURE_DESTINATION (ccl->produced);
	  for (i = 0; i < ccl->produced; i++)
	    *dst++ = destination_charbuf[i] & 0xFF;
	  produced_chars += ccl->produced;
	}
      charbuf += ccl->consumed;
      if (ccl->status == CCL_STAT_QUIT
	  || ccl->status == CCL_STAT_INVALID_CMD)
	break;
    }
  while (charbuf < charbuf_end);

  switch (ccl->status)
    {
    case CCL_STAT_SUSPEND_BY_SRC:
      record_conversion_result (coding, CODING_RESULT_INSUFFICIENT_SRC);
      break;
    case CCL_STAT_SUSPEND_BY_DST:
      record_conversion_result (coding, CODING_RESULT_INSUFFICIENT_DST);
      break;
    case CCL_STAT_QUIT:
    case CCL_STAT_INVALID_CMD:
      record_conversion_result (coding, CODING_RESULT_INTERRUPT);
      break;
    default:
      record_conversion_result (coding, CODING_RESULT_SUCCESS);
      break;
    }

  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}



/*** 10, 11. no-conversion handlers ***/

/* See the above "GENERAL NOTES on `decode_coding_XXX ()' functions".  */

static void
decode_coding_raw_text (struct coding_system *coding)
{
  int eol_dos =
    !inhibit_eol_conversion && EQ (CODING_ID_EOL_TYPE (coding->id), Qdos);

  coding->chars_at_source = 1;
  coding->consumed_char = coding->src_chars;
  coding->consumed = coding->src_bytes;
  if (eol_dos && coding->source[coding->src_bytes - 1] == '\r')
    {
      coding->consumed_char--;
      coding->consumed--;
      record_conversion_result (coding, CODING_RESULT_INSUFFICIENT_SRC);
    }
  else
    record_conversion_result (coding, CODING_RESULT_SUCCESS);
}

static int
encode_coding_raw_text (struct coding_system *coding)
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = coding->charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  EMACS_INT produced_chars = 0;
  int c;

  if (multibytep)
    {
      int safe_room = MAX_MULTIBYTE_LENGTH * 2;

      if (coding->src_multibyte)
	while (charbuf < charbuf_end)
	  {
	    ASSURE_DESTINATION (safe_room);
	    c = *charbuf++;
	    if (ASCII_CHAR_P (c))
	      EMIT_ONE_ASCII_BYTE (c);
	    else if (CHAR_BYTE8_P (c))
	      {
		c = CHAR_TO_BYTE8 (c);
		EMIT_ONE_BYTE (c);
	      }
	    else
	      {
		unsigned char str[MAX_MULTIBYTE_LENGTH], *p0 = str, *p1 = str;

		CHAR_STRING_ADVANCE (c, p1);
		do
		  {
		    EMIT_ONE_BYTE (*p0);
		    p0++;
		  }
		while (p0 < p1);
	      }
	  }
      else
	while (charbuf < charbuf_end)
	  {
	    ASSURE_DESTINATION (safe_room);
	    c = *charbuf++;
	    EMIT_ONE_BYTE (c);
	  }
    }
  else
    {
      if (coding->src_multibyte)
	{
	  int safe_room = MAX_MULTIBYTE_LENGTH;

	  while (charbuf < charbuf_end)
	    {
	      ASSURE_DESTINATION (safe_room);
	      c = *charbuf++;
	      if (ASCII_CHAR_P (c))
		*dst++ = c;
	      else if (CHAR_BYTE8_P (c))
		*dst++ = CHAR_TO_BYTE8 (c);
	      else
		CHAR_STRING_ADVANCE (c, dst);
	    }
	}
      else
	{
	  ASSURE_DESTINATION (charbuf_end - charbuf);
	  while (charbuf < charbuf_end && dst < dst_end)
	    *dst++ = *charbuf++;
	}
      produced_chars = dst - (coding->destination + coding->produced);
    }
  record_conversion_result (coding, CODING_RESULT_SUCCESS);
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}

/* See the above "GENERAL NOTES on `detect_coding_XXX ()' functions".
   Check if a text is encoded in a charset-based coding system.  If it
   is, return 1, else return 0.  */

static int
detect_coding_charset (struct coding_system *coding,
		       struct coding_detection_info *detect_info)
{
  const unsigned char *src = coding->source, *src_base;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  int multibytep = coding->src_multibyte;
  EMACS_INT consumed_chars = 0;
  Lisp_Object attrs, valids, name;
  int found = 0;
  EMACS_INT head_ascii = coding->head_ascii;
  int check_latin_extra = 0;

  detect_info->checked |= CATEGORY_MASK_CHARSET;

  coding = &coding_categories[coding_category_charset];
  attrs = CODING_ID_ATTRS (coding->id);
  valids = AREF (attrs, coding_attr_charset_valids);
  name = CODING_ID_NAME (coding->id);
  if (strncmp (SSDATA (SYMBOL_NAME (name)),
	       "iso-8859-", sizeof ("iso-8859-") - 1) == 0
      || strncmp (SSDATA (SYMBOL_NAME (name)),
		  "iso-latin-", sizeof ("iso-latin-") - 1) == 0)
    check_latin_extra = 1;

  if (! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    src += head_ascii;

  while (1)
    {
      int c;
      Lisp_Object val;
      struct charset *charset;
      int dim, idx;

      src_base = src;
      ONE_MORE_BYTE (c);
      if (c < 0)
	continue;
      val = AREF (valids, c);
      if (NILP (val))
	break;
      if (c >= 0x80)
	{
	  if (c < 0xA0
	      && check_latin_extra
	      && (!VECTORP (Vlatin_extra_code_table)
		  || NILP (XVECTOR (Vlatin_extra_code_table)->contents[c])))
	    break;
	  found = CATEGORY_MASK_CHARSET;
	}
      if (INTEGERP (val))
	{
	  charset = CHARSET_FROM_ID (XFASTINT (val));
	  dim = CHARSET_DIMENSION (charset);
	  for (idx = 1; idx < dim; idx++)
	    {
	      if (src == src_end)
		goto too_short;
	      ONE_MORE_BYTE (c);
	      if (c < charset->code_space[(dim - 1 - idx) * 4]
		  || c > charset->code_space[(dim - 1 - idx) * 4 + 1])
		break;
	    }
	  if (idx < dim)
	    break;
	}
      else
	{
	  idx = 1;
	  for (; CONSP (val); val = XCDR (val))
	    {
	      charset = CHARSET_FROM_ID (XFASTINT (XCAR (val)));
	      dim = CHARSET_DIMENSION (charset);
	      while (idx < dim)
		{
		  if (src == src_end)
		    goto too_short;
		  ONE_MORE_BYTE (c);
		  if (c < charset->code_space[(dim - 1 - idx) * 4]
		      || c > charset->code_space[(dim - 1 - idx) * 4 + 1])
		    break;
		  idx++;
		}
	      if (idx == dim)
		{
		  val = Qnil;
		  break;
		}
	    }
	  if (CONSP (val))
	    break;
	}
    }
 too_short:
  detect_info->rejected |= CATEGORY_MASK_CHARSET;
  return 0;

 no_more_source:
  detect_info->found |= found;
  return 1;
}

static void
decode_coding_charset (struct coding_system *coding)
{
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  const unsigned char *src_base;
  int *charbuf = coding->charbuf + coding->charbuf_used;
  /* We may produce one charset annotation in one loop and one more at
     the end.  */
  int *charbuf_end
    = coding->charbuf + coding->charbuf_size - (MAX_ANNOTATION_LENGTH * 2);
  EMACS_INT consumed_chars = 0, consumed_chars_base;
  int multibytep = coding->src_multibyte;
  Lisp_Object attrs = CODING_ID_ATTRS (coding->id);
  Lisp_Object valids;
  EMACS_INT char_offset = coding->produced_char;
  EMACS_INT last_offset = char_offset;
  int last_id = charset_ascii;
  int eol_dos =
    !inhibit_eol_conversion && EQ (CODING_ID_EOL_TYPE (coding->id), Qdos);
  int byte_after_cr = -1;

  valids = AREF (attrs, coding_attr_charset_valids);

  while (1)
    {
      int c;
      Lisp_Object val;
      struct charset *charset;
      int dim;
      int len = 1;
      unsigned code;

      src_base = src;
      consumed_chars_base = consumed_chars;

      if (charbuf >= charbuf_end)
	{
	  if (byte_after_cr >= 0)
	    src_base--;
	  break;
	}

      if (byte_after_cr >= 0)
	{
	  c = byte_after_cr;
	  byte_after_cr = -1;
	}
      else
	{
	  ONE_MORE_BYTE (c);
	  if (eol_dos && c == '\r')
	    ONE_MORE_BYTE (byte_after_cr);
	}
      if (c < 0)
	goto invalid_code;
      code = c;

      val = AREF (valids, c);
      if (! INTEGERP (val) && ! CONSP (val))
	goto invalid_code;
      if (INTEGERP (val))
	{
	  charset = CHARSET_FROM_ID (XFASTINT (val));
	  dim = CHARSET_DIMENSION (charset);
	  while (len < dim)
	    {
	      ONE_MORE_BYTE (c);
	      code = (code << 8) | c;
	      len++;
	    }
	  CODING_DECODE_CHAR (coding, src, src_base, src_end,
			      charset, code, c);
	}
      else
	{
	  /* VAL is a list of charset IDs.  It is assured that the
	     list is sorted by charset dimensions (smaller one
	     comes first).  */
	  while (CONSP (val))
	    {
	      charset = CHARSET_FROM_ID (XFASTINT (XCAR (val)));
	      dim = CHARSET_DIMENSION (charset);
	      while (len < dim)
		{
		  ONE_MORE_BYTE (c);
		  code = (code << 8) | c;
		  len++;
		}
	      CODING_DECODE_CHAR (coding, src, src_base,
				  src_end, charset, code, c);
	      if (c >= 0)
		break;
	      val = XCDR (val);
	    }
	}
      if (c < 0)
	goto invalid_code;
      if (charset->id != charset_ascii
	  && last_id != charset->id)
	{
	  if (last_id != charset_ascii)
	    ADD_CHARSET_DATA (charbuf, char_offset - last_offset, last_id);
	  last_id = charset->id;
	  last_offset = char_offset;
	}

      *charbuf++ = c;
      char_offset++;
      continue;

    invalid_code:
      src = src_base;
      consumed_chars = consumed_chars_base;
      ONE_MORE_BYTE (c);
      *charbuf++ = c < 0 ? -c : ASCII_BYTE_P (c) ? c : BYTE8_TO_CHAR (c);
      char_offset++;
      coding->errors++;
    }

 no_more_source:
  if (last_id != charset_ascii)
    ADD_CHARSET_DATA (charbuf, char_offset - last_offset, last_id);
  coding->consumed_char += consumed_chars_base;
  coding->consumed = src_base - coding->source;
  coding->charbuf_used = charbuf - coding->charbuf;
}

static int
encode_coding_charset (struct coding_system *coding)
{
  int multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  int safe_room = MAX_MULTIBYTE_LENGTH;
  EMACS_INT produced_chars = 0;
  Lisp_Object attrs, charset_list;
  int ascii_compatible;
  int c;

  CODING_GET_INFO (coding, attrs, charset_list);
  ascii_compatible = ! NILP (CODING_ATTR_ASCII_COMPAT (attrs));

  while (charbuf < charbuf_end)
    {
      struct charset *charset;
      unsigned code;

      ASSURE_DESTINATION (safe_room);
      c = *charbuf++;
      if (ascii_compatible && ASCII_CHAR_P (c))
	EMIT_ONE_ASCII_BYTE (c);
      else if (CHAR_BYTE8_P (c))
	{
	  c = CHAR_TO_BYTE8 (c);
	  EMIT_ONE_BYTE (c);
	}
      else
	{
	  CODING_CHAR_CHARSET (coding, dst, dst_end, c, charset_list,
			       &code, charset);

	  if (charset)
	    {
	      if (CHARSET_DIMENSION (charset) == 1)
		EMIT_ONE_BYTE (code);
	      else if (CHARSET_DIMENSION (charset) == 2)
		EMIT_TWO_BYTES (code >> 8, code & 0xFF);
	      else if (CHARSET_DIMENSION (charset) == 3)
		EMIT_THREE_BYTES (code >> 16, (code >> 8) & 0xFF, code & 0xFF);
	      else
		EMIT_FOUR_BYTES (code >> 24, (code >> 16) & 0xFF,
				 (code >> 8) & 0xFF, code & 0xFF);
	    }
	  else
	    {
	      if (coding->mode & CODING_MODE_SAFE_ENCODING)
		c = CODING_INHIBIT_CHARACTER_SUBSTITUTION;
	      else
		c = coding->default_char;
	      EMIT_ONE_BYTE (c);
	    }
	}
    }

  record_conversion_result (coding, CODING_RESULT_SUCCESS);
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
  return 0;
}


/*** 7. C library functions ***/

/* Setup coding context CODING from information about CODING_SYSTEM.
   If CODING_SYSTEM is nil, `no-conversion' is assumed.  If
   CODING_SYSTEM is invalid, signal an error.  */

void
setup_coding_system (Lisp_Object coding_system, struct coding_system *coding)
{
  Lisp_Object attrs;
  Lisp_Object eol_type;
  Lisp_Object coding_type;
  Lisp_Object val;

  if (NILP (coding_system))
    coding_system = Qundecided;

  CHECK_CODING_SYSTEM_GET_ID (coding_system, coding->id);

  attrs = CODING_ID_ATTRS (coding->id);
  eol_type = inhibit_eol_conversion ? Qunix : CODING_ID_EOL_TYPE (coding->id);

  coding->mode = 0;
  coding->head_ascii = -1;
  if (VECTORP (eol_type))
    coding->common_flags = (CODING_REQUIRE_DECODING_MASK
			    | CODING_REQUIRE_DETECTION_MASK);
  else if (! EQ (eol_type, Qunix))
    coding->common_flags = (CODING_REQUIRE_DECODING_MASK
			    | CODING_REQUIRE_ENCODING_MASK);
  else
    coding->common_flags = 0;
  if (! NILP (CODING_ATTR_POST_READ (attrs)))
    coding->common_flags |= CODING_REQUIRE_DECODING_MASK;
  if (! NILP (CODING_ATTR_PRE_WRITE (attrs)))
    coding->common_flags |= CODING_REQUIRE_ENCODING_MASK;
  if (! NILP (CODING_ATTR_FOR_UNIBYTE (attrs)))
    coding->common_flags |= CODING_FOR_UNIBYTE_MASK;

  val = CODING_ATTR_SAFE_CHARSETS (attrs);
  coding->max_charset_id = SCHARS (val) - 1;
  coding->safe_charsets = SDATA (val);
  coding->default_char = XINT (CODING_ATTR_DEFAULT_CHAR (attrs));
  coding->carryover_bytes = 0;

  coding_type = CODING_ATTR_TYPE (attrs);
  if (EQ (coding_type, Qundecided))
    {
      coding->detector = NULL;
      coding->decoder = decode_coding_raw_text;
      coding->encoder = encode_coding_raw_text;
      coding->common_flags |= CODING_REQUIRE_DETECTION_MASK;
    }
  else if (EQ (coding_type, Qiso_2022))
    {
      int i;
      int flags = XINT (AREF (attrs, coding_attr_iso_flags));

      /* Invoke graphic register 0 to plane 0.  */
      CODING_ISO_INVOCATION (coding, 0) = 0;
      /* Invoke graphic register 1 to plane 1 if we can use 8-bit.  */
      CODING_ISO_INVOCATION (coding, 1)
	= (flags & CODING_ISO_FLAG_SEVEN_BITS ? -1 : 1);
      /* Setup the initial status of designation.  */
      for (i = 0; i < 4; i++)
	CODING_ISO_DESIGNATION (coding, i) = CODING_ISO_INITIAL (coding, i);
      /* Not single shifting initially.  */
      CODING_ISO_SINGLE_SHIFTING (coding) = 0;
      /* Beginning of buffer should also be regarded as bol. */
      CODING_ISO_BOL (coding) = 1;
      coding->detector = detect_coding_iso_2022;
      coding->decoder = decode_coding_iso_2022;
      coding->encoder = encode_coding_iso_2022;
      if (flags & CODING_ISO_FLAG_SAFE)
	coding->mode |= CODING_MODE_SAFE_ENCODING;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK
	    | CODING_REQUIRE_FLUSHING_MASK);
      if (flags & CODING_ISO_FLAG_COMPOSITION)
	coding->common_flags |= CODING_ANNOTATE_COMPOSITION_MASK;
      if (flags & CODING_ISO_FLAG_DESIGNATION)
	coding->common_flags |= CODING_ANNOTATE_CHARSET_MASK;
      if (flags & CODING_ISO_FLAG_FULL_SUPPORT)
	{
	  setup_iso_safe_charsets (attrs);
	  val = CODING_ATTR_SAFE_CHARSETS (attrs);
	  coding->max_charset_id = SCHARS (val) - 1;
	  coding->safe_charsets = SDATA (val);
	}
      CODING_ISO_FLAGS (coding) = flags;
      CODING_ISO_CMP_STATUS (coding)->state = COMPOSING_NO;
      CODING_ISO_CMP_STATUS (coding)->method = COMPOSITION_NO;
      CODING_ISO_EXTSEGMENT_LEN (coding) = 0;
      CODING_ISO_EMBEDDED_UTF_8 (coding) = 0;
    }
  else if (EQ (coding_type, Qcharset))
    {
      coding->detector = detect_coding_charset;
      coding->decoder = decode_coding_charset;
      coding->encoder = encode_coding_charset;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
    }
  else if (EQ (coding_type, Qutf_8))
    {
      val = AREF (attrs, coding_attr_utf_bom);
      CODING_UTF_8_BOM (coding) = (CONSP (val) ? utf_detect_bom
				   : EQ (val, Qt) ? utf_with_bom
				   : utf_without_bom);
      coding->detector = detect_coding_utf_8;
      coding->decoder = decode_coding_utf_8;
      coding->encoder = encode_coding_utf_8;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
      if (CODING_UTF_8_BOM (coding) == utf_detect_bom)
	coding->common_flags |= CODING_REQUIRE_DETECTION_MASK;
    }
  else if (EQ (coding_type, Qutf_16))
    {
      val = AREF (attrs, coding_attr_utf_bom);
      CODING_UTF_16_BOM (coding) = (CONSP (val) ? utf_detect_bom
				    : EQ (val, Qt) ? utf_with_bom
				    : utf_without_bom);
      val = AREF (attrs, coding_attr_utf_16_endian);
      CODING_UTF_16_ENDIAN (coding) = (EQ (val, Qbig) ? utf_16_big_endian
				       : utf_16_little_endian);
      CODING_UTF_16_SURROGATE (coding) = 0;
      coding->detector = detect_coding_utf_16;
      coding->decoder = decode_coding_utf_16;
      coding->encoder = encode_coding_utf_16;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
      if (CODING_UTF_16_BOM (coding) == utf_detect_bom)
	coding->common_flags |= CODING_REQUIRE_DETECTION_MASK;
    }
  else if (EQ (coding_type, Qccl))
    {
      coding->detector = detect_coding_ccl;
      coding->decoder = decode_coding_ccl;
      coding->encoder = encode_coding_ccl;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK
	    | CODING_REQUIRE_FLUSHING_MASK);
    }
  else if (EQ (coding_type, Qemacs_mule))
    {
      coding->detector = detect_coding_emacs_mule;
      coding->decoder = decode_coding_emacs_mule;
      coding->encoder = encode_coding_emacs_mule;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
      coding->spec.emacs_mule.full_support = 1;
      if (! NILP (AREF (attrs, coding_attr_emacs_mule_full))
	  && ! EQ (CODING_ATTR_CHARSET_LIST (attrs), Vemacs_mule_charset_list))
	{
	  Lisp_Object tail, safe_charsets;
	  int max_charset_id = 0;

	  for (tail = Vemacs_mule_charset_list; CONSP (tail);
	       tail = XCDR (tail))
	    if (max_charset_id < XFASTINT (XCAR (tail)))
	      max_charset_id = XFASTINT (XCAR (tail));
	  safe_charsets = make_uninit_string (max_charset_id + 1);
	  memset (SDATA (safe_charsets), 255, max_charset_id + 1);
	  for (tail = Vemacs_mule_charset_list; CONSP (tail);
	       tail = XCDR (tail))
	    SSET (safe_charsets, XFASTINT (XCAR (tail)), 0);
	  coding->max_charset_id = max_charset_id;
	  coding->safe_charsets = SDATA (safe_charsets);
	  coding->spec.emacs_mule.full_support = 1;
	}
      coding->spec.emacs_mule.cmp_status.state = COMPOSING_NO;
      coding->spec.emacs_mule.cmp_status.method = COMPOSITION_NO;
    }
  else if (EQ (coding_type, Qshift_jis))
    {
      coding->detector = detect_coding_sjis;
      coding->decoder = decode_coding_sjis;
      coding->encoder = encode_coding_sjis;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
    }
  else if (EQ (coding_type, Qbig5))
    {
      coding->detector = detect_coding_big5;
      coding->decoder = decode_coding_big5;
      coding->encoder = encode_coding_big5;
      coding->common_flags
	|= (CODING_REQUIRE_DECODING_MASK | CODING_REQUIRE_ENCODING_MASK);
    }
  else				/* EQ (coding_type, Qraw_text) */
    {
      coding->detector = NULL;
      coding->decoder = decode_coding_raw_text;
      coding->encoder = encode_coding_raw_text;
      if (! EQ (eol_type, Qunix))
	{
	  coding->common_flags |= CODING_REQUIRE_DECODING_MASK;
	  if (! VECTORP (eol_type))
	    coding->common_flags |= CODING_REQUIRE_ENCODING_MASK;
	}

    }

  return;
}

/* Return a list of charsets supported by CODING.  */

Lisp_Object
coding_charset_list (struct coding_system *coding)
{
  Lisp_Object attrs, charset_list;

  CODING_GET_INFO (coding, attrs, charset_list);
  if (EQ (CODING_ATTR_TYPE (attrs), Qiso_2022))
    {
      int flags = XINT (AREF (attrs, coding_attr_iso_flags));

      if (flags & CODING_ISO_FLAG_FULL_SUPPORT)
	charset_list = Viso_2022_charset_list;
    }
  else if (EQ (CODING_ATTR_TYPE (attrs), Qemacs_mule))
    {
      charset_list = Vemacs_mule_charset_list;
    }
  return charset_list;
}


/* Return a list of charsets supported by CODING-SYSTEM.  */

Lisp_Object
coding_system_charset_list (Lisp_Object coding_system)
{
  ptrdiff_t id;
  Lisp_Object attrs, charset_list;

  CHECK_CODING_SYSTEM_GET_ID (coding_system, id);
  attrs = CODING_ID_ATTRS (id);

  if (EQ (CODING_ATTR_TYPE (attrs), Qiso_2022))
    {
      int flags = XINT (AREF (attrs, coding_attr_iso_flags));

      if (flags & CODING_ISO_FLAG_FULL_SUPPORT)
	charset_list = Viso_2022_charset_list;
      else
	charset_list = CODING_ATTR_CHARSET_LIST (attrs);
    }
  else if (EQ (CODING_ATTR_TYPE (attrs), Qemacs_mule))
    {
      charset_list = Vemacs_mule_charset_list;
    }
  else
    {
      charset_list = CODING_ATTR_CHARSET_LIST (attrs);
    }
  return charset_list;
}


/* Return raw-text or one of its subsidiaries that has the same
   eol_type as CODING-SYSTEM.  */

Lisp_Object
raw_text_coding_system (Lisp_Object coding_system)
{
  Lisp_Object spec, attrs;
  Lisp_Object eol_type, raw_text_eol_type;

  if (NILP (coding_system))
    return Qraw_text;
  spec = CODING_SYSTEM_SPEC (coding_system);
  attrs = AREF (spec, 0);

  if (EQ (CODING_ATTR_TYPE (attrs), Qraw_text))
    return coding_system;

  eol_type = AREF (spec, 2);
  if (VECTORP (eol_type))
    return Qraw_text;
  spec = CODING_SYSTEM_SPEC (Qraw_text);
  raw_text_eol_type = AREF (spec, 2);
  return (EQ (eol_type, Qunix) ? AREF (raw_text_eol_type, 0)
	  : EQ (eol_type, Qdos) ? AREF (raw_text_eol_type, 1)
	  : AREF (raw_text_eol_type, 2));
}


/* If CODING_SYSTEM doesn't specify end-of-line format, return one of
   the subsidiary that has the same eol-spec as PARENT (if it is not
   nil and specifies end-of-line format) or the system's setting
   (system_eol_type).  */

Lisp_Object
coding_inherit_eol_type (Lisp_Object coding_system, Lisp_Object parent)
{
  Lisp_Object spec, eol_type;

  if (NILP (coding_system))
    coding_system = Qraw_text;
  spec = CODING_SYSTEM_SPEC (coding_system);
  eol_type = AREF (spec, 2);
  if (VECTORP (eol_type))
    {
      Lisp_Object parent_eol_type;

      if (! NILP (parent))
	{
	  Lisp_Object parent_spec;

	  parent_spec = CODING_SYSTEM_SPEC (parent);
	  parent_eol_type = AREF (parent_spec, 2);
	  if (VECTORP (parent_eol_type))
	    parent_eol_type = system_eol_type;
	}
      else
	parent_eol_type = system_eol_type;
      if (EQ (parent_eol_type, Qunix))
	coding_system = AREF (eol_type, 0);
      else if (EQ (parent_eol_type, Qdos))
	coding_system = AREF (eol_type, 1);
      else if (EQ (parent_eol_type, Qmac))
	coding_system = AREF (eol_type, 2);
    }
  return coding_system;
}


/* Check if text-conversion and eol-conversion of CODING_SYSTEM are
   decided for writing to a process.  If not, complement them, and
   return a new coding system.  */

Lisp_Object
complement_process_encoding_system (Lisp_Object coding_system)
{
  Lisp_Object coding_base = Qnil, eol_base = Qnil;
  Lisp_Object spec, attrs;
  int i;

  for (i = 0; i < 3; i++)
    {
      if (i == 1)
	coding_system = CDR_SAFE (Vdefault_process_coding_system);
      else if (i == 2)
	coding_system = preferred_coding_system ();
      spec = CODING_SYSTEM_SPEC (coding_system);
      if (NILP (spec))
	continue;
      attrs = AREF (spec, 0);
      if (NILP (coding_base) && ! EQ (CODING_ATTR_TYPE (attrs), Qundecided))
	coding_base = CODING_ATTR_BASE_NAME (attrs);
      if (NILP (eol_base) && ! VECTORP (AREF (spec, 2)))
	eol_base = coding_system;
      if (! NILP (coding_base) && ! NILP (eol_base))
	break;
    }

  if (i > 0)
    /* The original CODING_SYSTEM didn't specify text-conversion or
       eol-conversion.  Be sure that we return a fully complemented
       coding system.  */
    coding_system = coding_inherit_eol_type (coding_base, eol_base);
  return coding_system;
}


/* Emacs has a mechanism to automatically detect a coding system if it
   is one of Emacs' internal format, ISO2022, SJIS, and BIG5.  But,
   it's impossible to distinguish some coding systems accurately
   because they use the same range of codes.  So, at first, coding
   systems are categorized into 7, those are:

   o coding-category-emacs-mule

   	The category for a coding system which has the same code range
	as Emacs' internal format.  Assigned the coding-system (Lisp
	symbol) `emacs-mule' by default.

   o coding-category-sjis

	The category for a coding system which has the same code range
	as SJIS.  Assigned the coding-system (Lisp
	symbol) `japanese-shift-jis' by default.

   o coding-category-iso-7

   	The category for a coding system which has the same code range
	as ISO2022 of 7-bit environment.  This doesn't use any locking
	shift and single shift functions.  This can encode/decode all
	charsets.  Assigned the coding-system (Lisp symbol)
	`iso-2022-7bit' by default.

   o coding-category-iso-7-tight

	Same as coding-category-iso-7 except that this can
	encode/decode only the specified charsets.

   o coding-category-iso-8-1

   	The category for a coding system which has the same code range
	as ISO2022 of 8-bit environment and graphic plane 1 used only
	for DIMENSION1 charset.  This doesn't use any locking shift
	and single shift functions.  Assigned the coding-system (Lisp
	symbol) `iso-latin-1' by default.

   o coding-category-iso-8-2

   	The category for a coding system which has the same code range
	as ISO2022 of 8-bit environment and graphic plane 1 used only
	for DIMENSION2 charset.  This doesn't use any locking shift
	and single shift functions.  Assigned the coding-system (Lisp
	symbol) `japanese-iso-8bit' by default.

   o coding-category-iso-7-else

   	The category for a coding system which has the same code range
	as ISO2022 of 7-bit environment but uses locking shift or
	single shift functions.  Assigned the coding-system (Lisp
	symbol) `iso-2022-7bit-lock' by default.

   o coding-category-iso-8-else

   	The category for a coding system which has the same code range
	as ISO2022 of 8-bit environment but uses locking shift or
	single shift functions.  Assigned the coding-system (Lisp
	symbol) `iso-2022-8bit-ss2' by default.

   o coding-category-big5

   	The category for a coding system which has the same code range
	as BIG5.  Assigned the coding-system (Lisp symbol)
	`cn-big5' by default.

   o coding-category-utf-8

	The category for a coding system which has the same code range
	as UTF-8 (cf. RFC3629).  Assigned the coding-system (Lisp
	symbol) `utf-8' by default.

   o coding-category-utf-16-be

	The category for a coding system in which a text has an
	Unicode signature (cf. Unicode Standard) in the order of BIG
	endian at the head.  Assigned the coding-system (Lisp symbol)
	`utf-16-be' by default.

   o coding-category-utf-16-le

	The category for a coding system in which a text has an
	Unicode signature (cf. Unicode Standard) in the order of
	LITTLE endian at the head.  Assigned the coding-system (Lisp
	symbol) `utf-16-le' by default.

   o coding-category-ccl

	The category for a coding system of which encoder/decoder is
	written in CCL programs.  The default value is nil, i.e., no
	coding system is assigned.

   o coding-category-binary

   	The category for a coding system not categorized in any of the
	above.  Assigned the coding-system (Lisp symbol)
	`no-conversion' by default.

   Each of them is a Lisp symbol and the value is an actual
   `coding-system's (this is also a Lisp symbol) assigned by a user.
   What Emacs does actually is to detect a category of coding system.
   Then, it uses a `coding-system' assigned to it.  If Emacs can't
   decide only one possible category, it selects a category of the
   highest priority.  Priorities of categories are also specified by a
   user in a Lisp variable `coding-category-list'.

*/

#define EOL_SEEN_NONE	0
#define EOL_SEEN_LF	1
#define EOL_SEEN_CR	2
#define EOL_SEEN_CRLF	4

/* Detect how end-of-line of a text of length SRC_BYTES pointed by
   SOURCE is encoded.  If CATEGORY is one of
   coding_category_utf_16_XXXX, assume that CR and LF are encoded by
   two-byte, else they are encoded by one-byte.

   Return one of EOL_SEEN_XXX.  */

#define MAX_EOL_CHECK_COUNT 3

static int
detect_eol (const unsigned char *source, EMACS_INT src_bytes,
	    enum coding_category category)
{
  const unsigned char *src = source, *src_end = src + src_bytes;
  unsigned char c;
  int total  = 0;
  int eol_seen = EOL_SEEN_NONE;

  if ((1 << category) & CATEGORY_MASK_UTF_16)
    {
      int msb, lsb;

      msb = category == (coding_category_utf_16_le
			 | coding_category_utf_16_le_nosig);
      lsb = 1 - msb;

      while (src + 1 < src_end)
	{
	  c = src[lsb];
	  if (src[msb] == 0 && (c == '\n' || c == '\r'))
	    {
	      int this_eol;

	      if (c == '\n')
		this_eol = EOL_SEEN_LF;
	      else if (src + 3 >= src_end
		       || src[msb + 2] != 0
		       || src[lsb + 2] != '\n')
		this_eol = EOL_SEEN_CR;
	      else
		{
		  this_eol = EOL_SEEN_CRLF;
		  src += 2;
		}

	      if (eol_seen == EOL_SEEN_NONE)
		/* This is the first end-of-line.  */
		eol_seen = this_eol;
	      else if (eol_seen != this_eol)
		{
		  /* The found type is different from what found before.
		     Allow for stray ^M characters in DOS EOL files.  */
		  if ((eol_seen == EOL_SEEN_CR && this_eol == EOL_SEEN_CRLF)
		      || (eol_seen == EOL_SEEN_CRLF
			  && this_eol == EOL_SEEN_CR))
		    eol_seen = EOL_SEEN_CRLF;
		  else
		    {
		      eol_seen = EOL_SEEN_LF;
		      break;
		    }
		}
	      if (++total == MAX_EOL_CHECK_COUNT)
		break;
	    }
	  src += 2;
	}
    }
  else
    while (src < src_end)
      {
	c = *src++;
	if (c == '\n' || c == '\r')
	  {
	    int this_eol;

	    if (c == '\n')
	      this_eol = EOL_SEEN_LF;
	    else if (src >= src_end || *src != '\n')
	      this_eol = EOL_SEEN_CR;
	    else
	      this_eol = EOL_SEEN_CRLF, src++;

	    if (eol_seen == EOL_SEEN_NONE)
	      /* This is the first end-of-line.  */
	      eol_seen = this_eol;
	    else if (eol_seen != this_eol)
	      {
		/* The found type is different from what found before.
		   Allow for stray ^M characters in DOS EOL files.  */
		if ((eol_seen == EOL_SEEN_CR && this_eol == EOL_SEEN_CRLF)
		    || (eol_seen == EOL_SEEN_CRLF && this_eol == EOL_SEEN_CR))
		  eol_seen = EOL_SEEN_CRLF;
		else
		  {
		    eol_seen = EOL_SEEN_LF;
		    break;
		  }
	      }
	    if (++total == MAX_EOL_CHECK_COUNT)
	      break;
	  }
      }
  return eol_seen;
}


static Lisp_Object
adjust_coding_eol_type (struct coding_system *coding, int eol_seen)
{
  Lisp_Object eol_type;

  eol_type = CODING_ID_EOL_TYPE (coding->id);
  if (eol_seen & EOL_SEEN_LF)
    {
      coding->id = CODING_SYSTEM_ID (AREF (eol_type, 0));
      eol_type = Qunix;
    }
  else if (eol_seen & EOL_SEEN_CRLF)
    {
      coding->id = CODING_SYSTEM_ID (AREF (eol_type, 1));
      eol_type = Qdos;
    }
  else if (eol_seen & EOL_SEEN_CR)
    {
      coding->id = CODING_SYSTEM_ID (AREF (eol_type, 2));
      eol_type = Qmac;
    }
  return eol_type;
}

/* Detect how a text specified in CODING is encoded.  If a coding
   system is detected, update fields of CODING by the detected coding
   system.  */

static void
detect_coding (struct coding_system *coding)
{
  const unsigned char *src, *src_end;
  int saved_mode = coding->mode;

  coding->consumed = coding->consumed_char = 0;
  coding->produced = coding->produced_char = 0;
  coding_set_source (coding);

  src_end = coding->source + coding->src_bytes;
  coding->head_ascii = 0;

  /* If we have not yet decided the text encoding type, detect it
     now.  */
  if (EQ (CODING_ATTR_TYPE (CODING_ID_ATTRS (coding->id)), Qundecided))
    {
      int c, i;
      struct coding_detection_info detect_info;
      int null_byte_found = 0, eight_bit_found = 0;

      detect_info.checked = detect_info.found = detect_info.rejected = 0;
      for (src = coding->source; src < src_end; src++)
	{
	  c = *src;
	  if (c & 0x80)
	    {
	      eight_bit_found = 1;
	      if (null_byte_found)
		break;
	    }
	  else if (c < 0x20)
	    {
	      if ((c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO)
		  && ! inhibit_iso_escape_detection
		  && ! detect_info.checked)
		{
		  if (detect_coding_iso_2022 (coding, &detect_info))
		    {
		      /* We have scanned the whole data.  */
		      if (! (detect_info.rejected & CATEGORY_MASK_ISO_7_ELSE))
			{
			  /* We didn't find an 8-bit code.  We may
			     have found a null-byte, but it's very
			     rare that a binary file conforms to
			     ISO-2022.  */
			  src = src_end;
			  coding->head_ascii = src - coding->source;
			}
		      detect_info.rejected |= ~CATEGORY_MASK_ISO_ESCAPE;
		      break;
		    }
		}
	      else if (! c && !inhibit_null_byte_detection)
		{
		  null_byte_found = 1;
		  if (eight_bit_found)
		    break;
		}
	      if (! eight_bit_found)
		coding->head_ascii++;
	    }
	  else if (! eight_bit_found)
	    coding->head_ascii++;
	}

      if (null_byte_found || eight_bit_found
	  || coding->head_ascii < coding->src_bytes
	  || detect_info.found)
	{
	  enum coding_category category;
	  struct coding_system *this;

	  if (coding->head_ascii == coding->src_bytes)
	    /* As all bytes are 7-bit, we can ignore non-ISO-2022 codings.  */
	    for (i = 0; i < coding_category_raw_text; i++)
	      {
		category = coding_priorities[i];
		this = coding_categories + category;
		if (detect_info.found & (1 << category))
		  break;
	      }
	  else
	    {
	      if (null_byte_found)
		{
		  detect_info.checked |= ~CATEGORY_MASK_UTF_16;
		  detect_info.rejected |= ~CATEGORY_MASK_UTF_16;
		}
	      for (i = 0; i < coding_category_raw_text; i++)
		{
		  category = coding_priorities[i];
		  this = coding_categories + category;
		  if (this->id < 0)
		    {
		      /* No coding system of this category is defined.  */
		      detect_info.rejected |= (1 << category);
		    }
		  else if (category >= coding_category_raw_text)
		    continue;
		  else if (detect_info.checked & (1 << category))
		    {
		      if (detect_info.found & (1 << category))
			break;
		    }
		  else if ((*(this->detector)) (coding, &detect_info)
			   && detect_info.found & (1 << category))
		    {
		      if (category == coding_category_utf_16_auto)
			{
			  if (detect_info.found & CATEGORY_MASK_UTF_16_LE)
			    category = coding_category_utf_16_le;
			  else
			    category = coding_category_utf_16_be;
			}
		      break;
		    }
		}
	    }

	  if (i < coding_category_raw_text)
	    setup_coding_system (CODING_ID_NAME (this->id), coding);
	  else if (null_byte_found)
	    setup_coding_system (Qno_conversion, coding);
	  else if ((detect_info.rejected & CATEGORY_MASK_ANY)
		   == CATEGORY_MASK_ANY)
	    setup_coding_system (Qraw_text, coding);
	  else if (detect_info.rejected)
	    for (i = 0; i < coding_category_raw_text; i++)
	      if (! (detect_info.rejected & (1 << coding_priorities[i])))
		{
		  this = coding_categories + coding_priorities[i];
		  setup_coding_system (CODING_ID_NAME (this->id), coding);
		  break;
		}
	}
    }
  else if (XINT (CODING_ATTR_CATEGORY (CODING_ID_ATTRS (coding->id)))
	   == coding_category_utf_8_auto)
    {
      Lisp_Object coding_systems;
      struct coding_detection_info detect_info;

      coding_systems
	= AREF (CODING_ID_ATTRS (coding->id), coding_attr_utf_bom);
      detect_info.found = detect_info.rejected = 0;
      coding->head_ascii = 0;
      if (CONSP (coding_systems)
	  && detect_coding_utf_8 (coding, &detect_info))
	{
	  if (detect_info.found & CATEGORY_MASK_UTF_8_SIG)
	    setup_coding_system (XCAR (coding_systems), coding);
	  else
	    setup_coding_system (XCDR (coding_systems), coding);
	}
    }
  else if (XINT (CODING_ATTR_CATEGORY (CODING_ID_ATTRS (coding->id)))
	   == coding_category_utf_16_auto)
    {
      Lisp_Object coding_systems;
      struct coding_detection_info detect_info;

      coding_systems
	= AREF (CODING_ID_ATTRS (coding->id), coding_attr_utf_bom);
      detect_info.found = detect_info.rejected = 0;
      coding->head_ascii = 0;
      if (CONSP (coding_systems)
	  && detect_coding_utf_16 (coding, &detect_info))
	{
	  if (detect_info.found & CATEGORY_MASK_UTF_16_LE)
	    setup_coding_system (XCAR (coding_systems), coding);
	  else if (detect_info.found & CATEGORY_MASK_UTF_16_BE)
	    setup_coding_system (XCDR (coding_systems), coding);
	}
    }
  coding->mode = saved_mode;
}


static void
decode_eol (struct coding_system *coding)
{
  Lisp_Object eol_type;
  unsigned char *p, *pbeg, *pend;

  eol_type = CODING_ID_EOL_TYPE (coding->id);
  if (EQ (eol_type, Qunix) || inhibit_eol_conversion)
    return;

  if (NILP (coding->dst_object))
    pbeg = coding->destination;
  else
    pbeg = BYTE_POS_ADDR (coding->dst_pos_byte);
  pend = pbeg + coding->produced;

  if (VECTORP (eol_type))
    {
      int eol_seen = EOL_SEEN_NONE;

      for (p = pbeg; p < pend; p++)
	{
	  if (*p == '\n')
	    eol_seen |= EOL_SEEN_LF;
	  else if (*p == '\r')
	    {
	      if (p + 1 < pend && *(p + 1) == '\n')
		{
		  eol_seen |= EOL_SEEN_CRLF;
		  p++;
		}
	      else
		eol_seen |= EOL_SEEN_CR;
	    }
	}
      /* Handle DOS-style EOLs in a file with stray ^M characters.  */
      if ((eol_seen & EOL_SEEN_CRLF) != 0
	  && (eol_seen & EOL_SEEN_CR) != 0
	  && (eol_seen & EOL_SEEN_LF) == 0)
	eol_seen = EOL_SEEN_CRLF;
      else if (eol_seen != EOL_SEEN_NONE
	  && eol_seen != EOL_SEEN_LF
	  && eol_seen != EOL_SEEN_CRLF
	  && eol_seen != EOL_SEEN_CR)
	eol_seen = EOL_SEEN_LF;
      if (eol_seen != EOL_SEEN_NONE)
	eol_type = adjust_coding_eol_type (coding, eol_seen);
    }

  if (EQ (eol_type, Qmac))
    {
      for (p = pbeg; p < pend; p++)
	if (*p == '\r')
	  *p = '\n';
    }
  else if (EQ (eol_type, Qdos))
    {
      EMACS_INT n = 0;

      if (NILP (coding->dst_object))
	{
	  /* Start deleting '\r' from the tail to minimize the memory
	     movement.  */
	  for (p = pend - 2; p >= pbeg; p--)
	    if (*p == '\r')
	      {
		memmove (p, p + 1, pend-- - p - 1);
		n++;
	      }
	}
      else
	{
	  EMACS_INT pos_byte = coding->dst_pos_byte;
	  EMACS_INT pos = coding->dst_pos;
	  EMACS_INT pos_end = pos + coding->produced_char - 1;

	  while (pos < pos_end)
	    {
	      p = BYTE_POS_ADDR (pos_byte);
	      if (*p == '\r' && p[1] == '\n')
		{
		  del_range_2 (pos, pos_byte, pos + 1, pos_byte + 1, 0);
		  n++;
		  pos_end--;
		}
	      pos++;
	      if (coding->dst_multibyte)
		pos_byte += BYTES_BY_CHAR_HEAD (*p);
	      else
		pos_byte++;
	    }
	}
      coding->produced -= n;
      coding->produced_char -= n;
    }
}


/* Return a translation table (or list of them) from coding system
   attribute vector ATTRS for encoding (ENCODEP is nonzero) or
   decoding (ENCODEP is zero). */

static Lisp_Object
get_translation_table (Lisp_Object attrs, int encodep, int *max_lookup)
{
  Lisp_Object standard, translation_table;
  Lisp_Object val;

  if (NILP (Venable_character_translation))
    {
      if (max_lookup)
	*max_lookup = 0;
      return Qnil;
    }
  if (encodep)
    translation_table = CODING_ATTR_ENCODE_TBL (attrs),
      standard = Vstandard_translation_table_for_encode;
  else
    translation_table = CODING_ATTR_DECODE_TBL (attrs),
      standard = Vstandard_translation_table_for_decode;
  if (NILP (translation_table))
    translation_table = standard;
  else
    {
      if (SYMBOLP (translation_table))
	translation_table = Fget (translation_table, Qtranslation_table);
      else if (CONSP (translation_table))
	{
	  translation_table = Fcopy_sequence (translation_table);
	  for (val = translation_table; CONSP (val); val = XCDR (val))
	    if (SYMBOLP (XCAR (val)))
	      XSETCAR (val, Fget (XCAR (val), Qtranslation_table));
	}
      if (CHAR_TABLE_P (standard))
	{
	  if (CONSP (translation_table))
	    translation_table = nconc2 (translation_table,
					Fcons (standard, Qnil));
	  else
	    translation_table = Fcons (translation_table,
				       Fcons (standard, Qnil));
	}
    }

  if (max_lookup)
    {
      *max_lookup = 1;
      if (CHAR_TABLE_P (translation_table)
	  && CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (translation_table)) > 1)
	{
	  val = XCHAR_TABLE (translation_table)->extras[1];
	  if (NATNUMP (val) && *max_lookup < XFASTINT (val))
	    *max_lookup = XFASTINT (val);
	}
      else if (CONSP (translation_table))
	{
	  Lisp_Object tail;

	  for (tail = translation_table; CONSP (tail); tail = XCDR (tail))
	    if (CHAR_TABLE_P (XCAR (tail))
		&& CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (XCAR (tail))) > 1)
	      {
		Lisp_Object tailval = XCHAR_TABLE (XCAR (tail))->extras[1];
		if (NATNUMP (tailval) && *max_lookup < XFASTINT (tailval))
		  *max_lookup = XFASTINT (tailval);
	      }
	}
    }
  return translation_table;
}

#define LOOKUP_TRANSLATION_TABLE(table, c, trans)		\
  do {								\
    trans = Qnil;						\
    if (CHAR_TABLE_P (table))					\
      {								\
	trans = CHAR_TABLE_REF (table, c);			\
	if (CHARACTERP (trans))					\
	  c = XFASTINT (trans), trans = Qnil;			\
      }								\
    else if (CONSP (table))					\
      {								\
	Lisp_Object tail;					\
								\
	for (tail = table; CONSP (tail); tail = XCDR (tail))	\
	  if (CHAR_TABLE_P (XCAR (tail)))			\
	    {							\
	      trans = CHAR_TABLE_REF (XCAR (tail), c);		\
	      if (CHARACTERP (trans))				\
		c = XFASTINT (trans), trans = Qnil;		\
	      else if (! NILP (trans))				\
		break;						\
	    }							\
      }								\
  } while (0)


/* Return a translation of character(s) at BUF according to TRANS.
   TRANS is TO-CHAR or ((FROM .  TO) ...) where
   FROM = [FROM-CHAR ...], TO is TO-CHAR or [TO-CHAR ...].
   The return value is TO-CHAR or ([FROM-CHAR ...] . TO) if a
   translation is found, and Qnil if not found..
   If BUF is too short to lookup characters in FROM, return Qt.  */

static Lisp_Object
get_translation (Lisp_Object trans, int *buf, int *buf_end)
{

  if (INTEGERP (trans))
    return trans;
  for (; CONSP (trans); trans = XCDR (trans))
    {
      Lisp_Object val = XCAR (trans);
      Lisp_Object from = XCAR (val);
      int len = ASIZE (from);
      int i;

      for (i = 0; i < len; i++)
	{
	  if (buf + i == buf_end)
	    return Qt;
	  if (XINT (AREF (from, i)) != buf[i])
	    break;
	}
      if (i == len)
	return val;
    }
  return Qnil;
}


static int
produce_chars (struct coding_system *coding, Lisp_Object translation_table,
	       int last_block)
{
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  EMACS_INT produced;
  EMACS_INT produced_chars = 0;
  int carryover = 0;

  if (! coding->chars_at_source)
    {
      /* Source characters are in coding->charbuf.  */
      int *buf = coding->charbuf;
      int *buf_end = buf + coding->charbuf_used;

      if (EQ (coding->src_object, coding->dst_object))
	{
	  coding_set_source (coding);
	  dst_end = ((unsigned char *) coding->source) + coding->consumed;
	}

      while (buf < buf_end)
	{
	  int c = *buf, i;

	  if (c >= 0)
	    {
	      EMACS_INT from_nchars = 1, to_nchars = 1;
	      Lisp_Object trans = Qnil;

	      LOOKUP_TRANSLATION_TABLE (translation_table, c, trans);
	      if (! NILP (trans))
		{
		  trans = get_translation (trans, buf, buf_end);
		  if (INTEGERP (trans))
		    c = XINT (trans);
		  else if (CONSP (trans))
		    {
		      from_nchars = ASIZE (XCAR (trans));
		      trans = XCDR (trans);
		      if (INTEGERP (trans))
			c = XINT (trans);
		      else
			{
			  to_nchars = ASIZE (trans);
			  c = XINT (AREF (trans, 0));
			}
		    }
		  else if (EQ (trans, Qt) && ! last_block)
		    break;
		}

	      if ((dst_end - dst) / MAX_MULTIBYTE_LENGTH < to_nchars)
		{
		  if (((min (PTRDIFF_MAX, SIZE_MAX) - (buf_end - buf))
		       / MAX_MULTIBYTE_LENGTH)
		      < to_nchars)
		    memory_full (SIZE_MAX);
		  dst = alloc_destination (coding,
					   buf_end - buf
					   + MAX_MULTIBYTE_LENGTH * to_nchars,
					   dst);
		  if (EQ (coding->src_object, coding->dst_object))
		    {
		      coding_set_source (coding);
		      dst_end = (((unsigned char *) coding->source)
				 + coding->consumed);
		    }
		  else
		    dst_end = coding->destination + coding->dst_bytes;
		}

	      for (i = 0; i < to_nchars; i++)
		{
		  if (i > 0)
		    c = XINT (AREF (trans, i));
		  if (coding->dst_multibyte
		      || ! CHAR_BYTE8_P (c))
		    CHAR_STRING_ADVANCE_NO_UNIFY (c, dst);
		  else
		    *dst++ = CHAR_TO_BYTE8 (c);
		}
	      produced_chars += to_nchars;
	      buf += from_nchars;
	    }
	  else
	    /* This is an annotation datum.  (-C) is the length.  */
	    buf += -c;
	}
      carryover = buf_end - buf;
    }
  else
    {
      /* Source characters are at coding->source.  */
      const unsigned char *src = coding->source;
      const unsigned char *src_end = src + coding->consumed;

      if (EQ (coding->dst_object, coding->src_object))
	dst_end = (unsigned char *) src;
      if (coding->src_multibyte != coding->dst_multibyte)
	{
	  if (coding->src_multibyte)
	    {
	      int multibytep = 1;
	      EMACS_INT consumed_chars = 0;

	      while (1)
		{
		  const unsigned char *src_base = src;
		  int c;

		  ONE_MORE_BYTE (c);
		  if (dst == dst_end)
		    {
		      if (EQ (coding->src_object, coding->dst_object))
			dst_end = (unsigned char *) src;
		      if (dst == dst_end)
			{
			  EMACS_INT offset = src - coding->source;

			  dst = alloc_destination (coding, src_end - src + 1,
						   dst);
			  dst_end = coding->destination + coding->dst_bytes;
			  coding_set_source (coding);
			  src = coding->source + offset;
			  src_end = coding->source + coding->consumed;
			  if (EQ (coding->src_object, coding->dst_object))
			    dst_end = (unsigned char *) src;
			}
		    }
		  *dst++ = c;
		  produced_chars++;
		}
	    no_more_source:
	      ;
	    }
	  else
	    while (src < src_end)
	      {
		int multibytep = 1;
		int c = *src++;

		if (dst >= dst_end - 1)
		  {
		    if (EQ (coding->src_object, coding->dst_object))
		      dst_end = (unsigned char *) src;
		    if (dst >= dst_end - 1)
		      {
			EMACS_INT offset = src - coding->source;
			EMACS_INT more_bytes;

			if (EQ (coding->src_object, coding->dst_object))
			  more_bytes = ((src_end - src) / 2) + 2;
			else
			  more_bytes = src_end - src + 2;
			dst = alloc_destination (coding, more_bytes, dst);
			dst_end = coding->destination + coding->dst_bytes;
			coding_set_source (coding);
			src = coding->source + offset;
			src_end = coding->source + coding->consumed;
			if (EQ (coding->src_object, coding->dst_object))
			  dst_end = (unsigned char *) src;
		      }
		  }
		EMIT_ONE_BYTE (c);
	      }
	}
      else
	{
	  if (!EQ (coding->src_object, coding->dst_object))
	    {
	      EMACS_INT require = coding->src_bytes - coding->dst_bytes;

	      if (require > 0)
		{
		  EMACS_INT offset = src - coding->source;

		  dst = alloc_destination (coding, require, dst);
		  coding_set_source (coding);
		  src = coding->source + offset;
		  src_end = coding->source + coding->consumed;
		}
	    }
	  produced_chars = coding->consumed_char;
	  while (src < src_end)
	    *dst++ = *src++;
	}
    }

  produced = dst - (coding->destination + coding->produced);
  if (BUFFERP (coding->dst_object) && produced_chars > 0)
    insert_from_gap (produced_chars, produced);
  coding->produced += produced;
  coding->produced_char += produced_chars;
  return carryover;
}

/* Compose text in CODING->object according to the annotation data at
   CHARBUF.  CHARBUF is an array:
     [ -LENGTH ANNOTATION_MASK NCHARS NBYTES METHOD [ COMPONENTS... ] ]
 */

static inline void
produce_composition (struct coding_system *coding, int *charbuf, EMACS_INT pos)
{
  int len;
  EMACS_INT to;
  enum composition_method method;
  Lisp_Object components;

  len = -charbuf[0] - MAX_ANNOTATION_LENGTH;
  to = pos + charbuf[2];
  method = (enum composition_method) (charbuf[4]);

  if (method == COMPOSITION_RELATIVE)
    components = Qnil;
  else
    {
      Lisp_Object args[MAX_COMPOSITION_COMPONENTS * 2 - 1];
      int i, j;

      if (method == COMPOSITION_WITH_RULE)
	len = charbuf[2] * 3 - 2;
      charbuf += MAX_ANNOTATION_LENGTH;
      /* charbuf = [ CHRA ... CHAR] or [ CHAR -2 RULE ... CHAR ] */
      for (i = j = 0; i < len && charbuf[i] != -1; i++, j++)
	{
	  if (charbuf[i] >= 0)
	    args[j] = make_number (charbuf[i]);
	  else
	    {
	      i++;
	      args[j] = make_number (charbuf[i] % 0x100);
	    }
	}
      components = (i == j ? Fstring (j, args) : Fvector (j, args));
    }
  compose_text (pos, to, components, Qnil, coding->dst_object);
}


/* Put `charset' property on text in CODING->object according to
   the annotation data at CHARBUF.  CHARBUF is an array:
     [ -LENGTH ANNOTATION_MASK NCHARS CHARSET-ID ]
 */

static inline void
produce_charset (struct coding_system *coding, int *charbuf, EMACS_INT pos)
{
  EMACS_INT from = pos - charbuf[2];
  struct charset *charset = CHARSET_FROM_ID (charbuf[3]);

  Fput_text_property (make_number (from), make_number (pos),
		      Qcharset, CHARSET_NAME (charset),
		      coding->dst_object);
}


#define CHARBUF_SIZE 0x4000

#define ALLOC_CONVERSION_WORK_AREA(coding)				\
  do {									\
    int size = CHARBUF_SIZE;						\
    									\
    coding->charbuf = NULL;						\
    while (size > 1024)							\
      {									\
	coding->charbuf = (int *) alloca (sizeof (int) * size);		\
	if (coding->charbuf)						\
	  break;							\
	size >>= 1;							\
      }									\
    if (! coding->charbuf)						\
      {									\
	record_conversion_result (coding, CODING_RESULT_INSUFFICIENT_MEM); \
	return coding->result;						\
      }									\
    coding->charbuf_size = size;					\
  } while (0)


static void
produce_annotation (struct coding_system *coding, EMACS_INT pos)
{
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf + coding->charbuf_used;

  if (NILP (coding->dst_object))
    return;

  while (charbuf < charbuf_end)
    {
      if (*charbuf >= 0)
	pos++, charbuf++;
      else
	{
	  int len = -*charbuf;

	  if (len > 2)
	    switch (charbuf[1])
	      {
	      case CODING_ANNOTATE_COMPOSITION_MASK:
		produce_composition (coding, charbuf, pos);
		break;
	      case CODING_ANNOTATE_CHARSET_MASK:
		produce_charset (coding, charbuf, pos);
		break;
	      }
	  charbuf += len;
	}
    }
}

/* Decode the data at CODING->src_object into CODING->dst_object.
   CODING->src_object is a buffer, a string, or nil.
   CODING->dst_object is a buffer.

   If CODING->src_object is a buffer, it must be the current buffer.
   In this case, if CODING->src_pos is positive, it is a position of
   the source text in the buffer, otherwise, the source text is in the
   gap area of the buffer, and CODING->src_pos specifies the offset of
   the text from GPT (which must be the same as PT).  If this is the
   same buffer as CODING->dst_object, CODING->src_pos must be
   negative.

   If CODING->src_object is a string, CODING->src_pos is an index to
   that string.

   If CODING->src_object is nil, CODING->source must already point to
   the non-relocatable memory area.  In this case, CODING->src_pos is
   an offset from CODING->source.

   The decoded data is inserted at the current point of the buffer
   CODING->dst_object.
*/

static int
decode_coding (struct coding_system *coding)
{
  Lisp_Object attrs;
  Lisp_Object undo_list;
  Lisp_Object translation_table;
  struct ccl_spec cclspec;
  int carryover;
  int i;

  if (BUFFERP (coding->src_object)
      && coding->src_pos > 0
      && coding->src_pos < GPT
      && coding->src_pos + coding->src_chars > GPT)
    move_gap_both (coding->src_pos, coding->src_pos_byte);

  undo_list = Qt;
  if (BUFFERP (coding->dst_object))
    {
      if (current_buffer != XBUFFER (coding->dst_object))
	set_buffer_internal (XBUFFER (coding->dst_object));
      if (GPT != PT)
	move_gap_both (PT, PT_BYTE);
      undo_list = BVAR (current_buffer, undo_list);
      BVAR (current_buffer, undo_list) = Qt;
    }

  coding->consumed = coding->consumed_char = 0;
  coding->produced = coding->produced_char = 0;
  coding->chars_at_source = 0;
  record_conversion_result (coding, CODING_RESULT_SUCCESS);
  coding->errors = 0;

  ALLOC_CONVERSION_WORK_AREA (coding);

  attrs = CODING_ID_ATTRS (coding->id);
  translation_table = get_translation_table (attrs, 0, NULL);

  carryover = 0;
  if (coding->decoder == decode_coding_ccl)
    {
      coding->spec.ccl = &cclspec;
      setup_ccl_program (&cclspec.ccl, CODING_CCL_DECODER (coding));
    }
  do
    {
      EMACS_INT pos = coding->dst_pos + coding->produced_char;

      coding_set_source (coding);
      coding->annotated = 0;
      coding->charbuf_used = carryover;
      (*(coding->decoder)) (coding);
      coding_set_destination (coding);
      carryover = produce_chars (coding, translation_table, 0);
      if (coding->annotated)
	produce_annotation (coding, pos);
      for (i = 0; i < carryover; i++)
	coding->charbuf[i]
	  = coding->charbuf[coding->charbuf_used - carryover + i];
    }
  while (coding->result == CODING_RESULT_INSUFFICIENT_DST
	 || (coding->consumed < coding->src_bytes
	     && (coding->result == CODING_RESULT_SUCCESS
		 || coding->result == CODING_RESULT_INVALID_SRC)));

  if (carryover > 0)
    {
      coding_set_destination (coding);
      coding->charbuf_used = carryover;
      produce_chars (coding, translation_table, 1);
    }

  coding->carryover_bytes = 0;
  if (coding->consumed < coding->src_bytes)
    {
      int nbytes = coding->src_bytes - coding->consumed;
      const unsigned char *src;

      coding_set_source (coding);
      coding_set_destination (coding);
      src = coding->source + coding->consumed;

      if (coding->mode & CODING_MODE_LAST_BLOCK)
	{
	  /* Flush out unprocessed data as binary chars.  We are sure
	     that the number of data is less than the size of
	     coding->charbuf.  */
	  coding->charbuf_used = 0;
	  coding->chars_at_source = 0;

	  while (nbytes-- > 0)
	    {
	      int c = *src++;

	      if (c & 0x80)
		c = BYTE8_TO_CHAR (c);
	      coding->charbuf[coding->charbuf_used++] = c;
	    }
	  produce_chars (coding, Qnil, 1);
	}
      else
	{
	  /* Record unprocessed bytes in coding->carryover.  We are
	     sure that the number of data is less than the size of
	     coding->carryover.  */
	  unsigned char *p = coding->carryover;

	  if (nbytes > sizeof coding->carryover)
	    nbytes = sizeof coding->carryover;
	  coding->carryover_bytes = nbytes;
	  while (nbytes-- > 0)
	    *p++ = *src++;
	}
      coding->consumed = coding->src_bytes;
    }

  if (! EQ (CODING_ID_EOL_TYPE (coding->id), Qunix)
      && !inhibit_eol_conversion)
    decode_eol (coding);
  if (BUFFERP (coding->dst_object))
    {
      BVAR (current_buffer, undo_list) = undo_list;
      record_insert (coding->dst_pos, coding->produced_char);
    }
  return coding->result;
}


/* Extract an annotation datum from a composition starting at POS and
   ending before LIMIT of CODING->src_object (buffer or string), store
   the data in BUF, set *STOP to a starting position of the next
   composition (if any) or to LIMIT, and return the address of the
   next element of BUF.

   If such an annotation is not found, set *STOP to a starting
   position of a composition after POS (if any) or to LIMIT, and
   return BUF.  */

static inline int *
handle_composition_annotation (EMACS_INT pos, EMACS_INT limit,
			       struct coding_system *coding, int *buf,
			       EMACS_INT *stop)
{
  EMACS_INT start, end;
  Lisp_Object prop;

  if (! find_composition (pos, limit, &start, &end, &prop, coding->src_object)
      || end > limit)
    *stop = limit;
  else if (start > pos)
    *stop = start;
  else
    {
      if (start == pos)
	{
	  /* We found a composition.  Store the corresponding
	     annotation data in BUF.  */
	  int *head = buf;
	  enum composition_method method = COMPOSITION_METHOD (prop);
	  int nchars = COMPOSITION_LENGTH (prop);

	  ADD_COMPOSITION_DATA (buf, nchars, 0, method);
	  if (method != COMPOSITION_RELATIVE)
	    {
	      Lisp_Object components;
	      int len, i, i_byte;

	      components = COMPOSITION_COMPONENTS (prop);
	      if (VECTORP (components))
		{
		  len = ASIZE (components);
		  for (i = 0; i < len; i++)
		    *buf++ = XINT (AREF (components, i));
		}
	      else if (STRINGP (components))
		{
		  len = SCHARS (components);
		  i = i_byte = 0;
		  while (i < len)
		    {
		      FETCH_STRING_CHAR_ADVANCE (*buf, components, i, i_byte);
		      buf++;
		    }
		}
	      else if (INTEGERP (components))
		{
		  len = 1;
		  *buf++ = XINT (components);
		}
	      else if (CONSP (components))
		{
		  for (len = 0; CONSP (components);
		       len++, components = XCDR (components))
		    *buf++ = XINT (XCAR (components));
		}
	      else
		abort ();
	      *head -= len;
	    }
	}

      if (find_composition (end, limit, &start, &end, &prop,
			    coding->src_object)
	  && end <= limit)
	*stop = start;
      else
	*stop = limit;
    }
  return buf;
}


/* Extract an annotation datum from a text property `charset' at POS of
   CODING->src_object (buffer of string), store the data in BUF, set
   *STOP to the position where the value of `charset' property changes
   (limiting by LIMIT), and return the address of the next element of
   BUF.

   If the property value is nil, set *STOP to the position where the
   property value is non-nil (limiting by LIMIT), and return BUF.  */

static inline int *
handle_charset_annotation (EMACS_INT pos, EMACS_INT limit,
			   struct coding_system *coding, int *buf,
			   EMACS_INT *stop)
{
  Lisp_Object val, next;
  int id;

  val = Fget_text_property (make_number (pos), Qcharset, coding->src_object);
  if (! NILP (val) && CHARSETP (val))
    id = XINT (CHARSET_SYMBOL_ID (val));
  else
    id = -1;
  ADD_CHARSET_DATA (buf, 0, id);
  next = Fnext_single_property_change (make_number (pos), Qcharset,
				       coding->src_object,
				       make_number (limit));
  *stop = XINT (next);
  return buf;
}


static void
consume_chars (struct coding_system *coding, Lisp_Object translation_table,
	       int max_lookup)
{
  int *buf = coding->charbuf;
  int *buf_end = coding->charbuf + coding->charbuf_size;
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  EMACS_INT pos = coding->src_pos + coding->consumed_char;
  EMACS_INT end_pos = coding->src_pos + coding->src_chars;
  int multibytep = coding->src_multibyte;
  Lisp_Object eol_type;
  int c;
  EMACS_INT stop, stop_composition, stop_charset;
  int *lookup_buf = NULL;

  if (! NILP (translation_table))
    lookup_buf = alloca (sizeof (int) * max_lookup);

  eol_type = inhibit_eol_conversion ? Qunix : CODING_ID_EOL_TYPE (coding->id);
  if (VECTORP (eol_type))
    eol_type = Qunix;

  /* Note: composition handling is not yet implemented.  */
  coding->common_flags &= ~CODING_ANNOTATE_COMPOSITION_MASK;

  if (NILP (coding->src_object))
    stop = stop_composition = stop_charset = end_pos;
  else
    {
      if (coding->common_flags & CODING_ANNOTATE_COMPOSITION_MASK)
	stop = stop_composition = pos;
      else
	stop = stop_composition = end_pos;
      if (coding->common_flags & CODING_ANNOTATE_CHARSET_MASK)
	stop = stop_charset = pos;
      else
	stop_charset = end_pos;
    }

  /* Compensate for CRLF and conversion.  */
  buf_end -= 1 + MAX_ANNOTATION_LENGTH;
  while (buf < buf_end)
    {
      Lisp_Object trans;

      if (pos == stop)
	{
	  if (pos == end_pos)
	    break;
	  if (pos == stop_composition)
	    buf = handle_composition_annotation (pos, end_pos, coding,
						 buf, &stop_composition);
	  if (pos == stop_charset)
	    buf = handle_charset_annotation (pos, end_pos, coding,
					     buf, &stop_charset);
	  stop = (stop_composition < stop_charset
		  ? stop_composition : stop_charset);
	}

      if (! multibytep)
	{
	  EMACS_INT bytes;

	  if (coding->encoder == encode_coding_raw_text
	      || coding->encoder == encode_coding_ccl)
	    c = *src++, pos++;
	  else if ((bytes = MULTIBYTE_LENGTH (src, src_end)) > 0)
	    c = STRING_CHAR_ADVANCE_NO_UNIFY (src), pos += bytes;
	  else
	    c = BYTE8_TO_CHAR (*src), src++, pos++;
	}
      else
	c = STRING_CHAR_ADVANCE_NO_UNIFY (src), pos++;
      if ((c == '\r') && (coding->mode & CODING_MODE_SELECTIVE_DISPLAY))
	c = '\n';
      if (! EQ (eol_type, Qunix))
	{
	  if (c == '\n')
	    {
	      if (EQ (eol_type, Qdos))
		*buf++ = '\r';
	      else
		c = '\r';
	    }
	}

      trans = Qnil;
      LOOKUP_TRANSLATION_TABLE (translation_table, c, trans);
      if (NILP (trans))
	*buf++ = c;
      else
	{
	  int from_nchars = 1, to_nchars = 1;
	  int *lookup_buf_end;
	  const unsigned char *p = src;
	  int i;

	  lookup_buf[0] = c;
	  for (i = 1; i < max_lookup && p < src_end; i++)
	    lookup_buf[i] = STRING_CHAR_ADVANCE (p);
	  lookup_buf_end = lookup_buf + i;
	  trans = get_translation (trans, lookup_buf, lookup_buf_end);
	  if (INTEGERP (trans))
	    c = XINT (trans);
	  else if (CONSP (trans))
	    {
	      from_nchars = ASIZE (XCAR (trans));
	      trans = XCDR (trans);
	      if (INTEGERP (trans))
		c = XINT (trans);
	      else
		{
		  to_nchars = ASIZE (trans);
		  if (buf + to_nchars > buf_end)
		    break;
		  c = XINT (AREF (trans, 0));
		}
	    }
	  else
	    break;
	  *buf++ = c;
	  for (i = 1; i < to_nchars; i++)
	    *buf++ = XINT (AREF (trans, i));
	  for (i = 1; i < from_nchars; i++, pos++)
	    src += MULTIBYTE_LENGTH_NO_CHECK (src);
	}
    }

  coding->consumed = src - coding->source;
  coding->consumed_char = pos - coding->src_pos;
  coding->charbuf_used = buf - coding->charbuf;
  coding->chars_at_source = 0;
}


/* Encode the text at CODING->src_object into CODING->dst_object.
   CODING->src_object is a buffer or a string.
   CODING->dst_object is a buffer or nil.

   If CODING->src_object is a buffer, it must be the current buffer.
   In this case, if CODING->src_pos is positive, it is a position of
   the source text in the buffer, otherwise. the source text is in the
   gap area of the buffer, and coding->src_pos specifies the offset of
   the text from GPT (which must be the same as PT).  If this is the
   same buffer as CODING->dst_object, CODING->src_pos must be
   negative and CODING should not have `pre-write-conversion'.

   If CODING->src_object is a string, CODING should not have
   `pre-write-conversion'.

   If CODING->dst_object is a buffer, the encoded data is inserted at
   the current point of that buffer.

   If CODING->dst_object is nil, the encoded data is placed at the
   memory area specified by CODING->destination.  */

static int
encode_coding (struct coding_system *coding)
{
  Lisp_Object attrs;
  Lisp_Object translation_table;
  int max_lookup;
  struct ccl_spec cclspec;

  attrs = CODING_ID_ATTRS (coding->id);
  if (coding->encoder == encode_coding_raw_text)
    translation_table = Qnil, max_lookup = 0;
  else
    translation_table = get_translation_table (attrs, 1, &max_lookup);

  if (BUFFERP (coding->dst_object))
    {
      set_buffer_internal (XBUFFER (coding->dst_object));
      coding->dst_multibyte
	= ! NILP (BVAR (current_buffer, enable_multibyte_characters));
    }

  coding->consumed = coding->consumed_char = 0;
  coding->produced = coding->produced_char = 0;
  record_conversion_result (coding, CODING_RESULT_SUCCESS);
  coding->errors = 0;

  ALLOC_CONVERSION_WORK_AREA (coding);

  if (coding->encoder == encode_coding_ccl)
    {
      coding->spec.ccl = &cclspec;
      setup_ccl_program (&cclspec.ccl, CODING_CCL_ENCODER (coding));
    }
  do {
    coding_set_source (coding);
    consume_chars (coding, translation_table, max_lookup);
    coding_set_destination (coding);
    (*(coding->encoder)) (coding);
  } while (coding->consumed_char < coding->src_chars);

  if (BUFFERP (coding->dst_object) && coding->produced_char > 0)
    insert_from_gap (coding->produced_char, coding->produced);

  return (coding->result);
}


/* Name (or base name) of work buffer for code conversion.  */
static Lisp_Object Vcode_conversion_workbuf_name;

/* A working buffer used by the top level conversion.  Once it is
   created, it is never destroyed.  It has the name
   Vcode_conversion_workbuf_name.  The other working buffers are
   destroyed after the use is finished, and their names are modified
   versions of Vcode_conversion_workbuf_name.  */
static Lisp_Object Vcode_conversion_reused_workbuf;

/* 1 iff Vcode_conversion_reused_workbuf is already in use.  */
static int reused_workbuf_in_use;


/* Return a working buffer of code conversion.  MULTIBYTE specifies the
   multibyteness of returning buffer.  */

static Lisp_Object
make_conversion_work_buffer (int multibyte)
{
  Lisp_Object name, workbuf;
  struct buffer *current;

  if (reused_workbuf_in_use++)
    {
      name = Fgenerate_new_buffer_name (Vcode_conversion_workbuf_name, Qnil);
      workbuf = Fget_buffer_create (name);
    }
  else
    {
      if (NILP (Fbuffer_live_p (Vcode_conversion_reused_workbuf)))
	Vcode_conversion_reused_workbuf
	  = Fget_buffer_create (Vcode_conversion_workbuf_name);
      workbuf = Vcode_conversion_reused_workbuf;
    }
  current = current_buffer;
  set_buffer_internal (XBUFFER (workbuf));
  /* We can't allow modification hooks to run in the work buffer.  For
     instance, directory_files_internal assumes that file decoding
     doesn't compile new regexps.  */
  Fset (Fmake_local_variable (Qinhibit_modification_hooks), Qt);
  Ferase_buffer ();
  BVAR (current_buffer, undo_list) = Qt;
  BVAR (current_buffer, enable_multibyte_characters) = multibyte ? Qt : Qnil;
  set_buffer_internal (current);
  return workbuf;
}


static Lisp_Object
code_conversion_restore (Lisp_Object arg)
{
  Lisp_Object current, workbuf;
  struct gcpro gcpro1;

  GCPRO1 (arg);
  current = XCAR (arg);
  workbuf = XCDR (arg);
  if (! NILP (workbuf))
    {
      if (EQ (workbuf, Vcode_conversion_reused_workbuf))
	reused_workbuf_in_use = 0;
      else if (! NILP (Fbuffer_live_p (workbuf)))
	Fkill_buffer (workbuf);
    }
  set_buffer_internal (XBUFFER (current));
  UNGCPRO;
  return Qnil;
}

Lisp_Object
code_conversion_save (int with_work_buf, int multibyte)
{
  Lisp_Object workbuf = Qnil;

  if (with_work_buf)
    workbuf = make_conversion_work_buffer (multibyte);
  record_unwind_protect (code_conversion_restore,
			 Fcons (Fcurrent_buffer (), workbuf));
  return workbuf;
}

int
decode_coding_gap (struct coding_system *coding,
		   EMACS_INT chars, EMACS_INT bytes)
{
  int count = SPECPDL_INDEX ();
  Lisp_Object attrs;

  code_conversion_save (0, 0);

  coding->src_object = Fcurrent_buffer ();
  coding->src_chars = chars;
  coding->src_bytes = bytes;
  coding->src_pos = -chars;
  coding->src_pos_byte = -bytes;
  coding->src_multibyte = chars < bytes;
  coding->dst_object = coding->src_object;
  coding->dst_pos = PT;
  coding->dst_pos_byte = PT_BYTE;
  coding->dst_multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));

  if (CODING_REQUIRE_DETECTION (coding))
    detect_coding (coding);

  coding->mode |= CODING_MODE_LAST_BLOCK;
  current_buffer->text->inhibit_shrinking = 1;
  decode_coding (coding);
  current_buffer->text->inhibit_shrinking = 0;

  attrs = CODING_ID_ATTRS (coding->id);
  if (! NILP (CODING_ATTR_POST_READ (attrs)))
    {
      EMACS_INT prev_Z = Z, prev_Z_BYTE = Z_BYTE;
      Lisp_Object val;

      TEMP_SET_PT_BOTH (coding->dst_pos, coding->dst_pos_byte);
      val = call1 (CODING_ATTR_POST_READ (attrs),
		   make_number (coding->produced_char));
      CHECK_NATNUM (val);
      coding->produced_char += Z - prev_Z;
      coding->produced += Z_BYTE - prev_Z_BYTE;
    }

  unbind_to (count, Qnil);
  return coding->result;
}


/* Decode the text in the range FROM/FROM_BYTE and TO/TO_BYTE in
   SRC_OBJECT into DST_OBJECT by coding context CODING.

   SRC_OBJECT is a buffer, a string, or Qnil.

   If it is a buffer, the text is at point of the buffer.  FROM and TO
   are positions in the buffer.

   If it is a string, the text is at the beginning of the string.
   FROM and TO are indices to the string.

   If it is nil, the text is at coding->source.  FROM and TO are
   indices to coding->source.

   DST_OBJECT is a buffer, Qt, or Qnil.

   If it is a buffer, the decoded text is inserted at point of the
   buffer.  If the buffer is the same as SRC_OBJECT, the source text
   is deleted.

   If it is Qt, a string is made from the decoded text, and
   set in CODING->dst_object.

   If it is Qnil, the decoded text is stored at CODING->destination.
   The caller must allocate CODING->dst_bytes bytes at
   CODING->destination by xmalloc.  If the decoded text is longer than
   CODING->dst_bytes, CODING->destination is relocated by xrealloc.
 */

void
decode_coding_object (struct coding_system *coding,
		      Lisp_Object src_object,
		      EMACS_INT from, EMACS_INT from_byte,
		      EMACS_INT to, EMACS_INT to_byte,
		      Lisp_Object dst_object)
{
  int count = SPECPDL_INDEX ();
  unsigned char *destination IF_LINT (= NULL);
  EMACS_INT dst_bytes IF_LINT (= 0);
  EMACS_INT chars = to - from;
  EMACS_INT bytes = to_byte - from_byte;
  Lisp_Object attrs;
  int saved_pt = -1, saved_pt_byte IF_LINT (= 0);
  int need_marker_adjustment = 0;
  Lisp_Object old_deactivate_mark;

  old_deactivate_mark = Vdeactivate_mark;

  if (NILP (dst_object))
    {
      destination = coding->destination;
      dst_bytes = coding->dst_bytes;
    }

  coding->src_object = src_object;
  coding->src_chars = chars;
  coding->src_bytes = bytes;
  coding->src_multibyte = chars < bytes;

  if (STRINGP (src_object))
    {
      coding->src_pos = from;
      coding->src_pos_byte = from_byte;
    }
  else if (BUFFERP (src_object))
    {
      set_buffer_internal (XBUFFER (src_object));
      if (from != GPT)
	move_gap_both (from, from_byte);
      if (EQ (src_object, dst_object))
	{
	  struct Lisp_Marker *tail;

	  for (tail = BUF_MARKERS (current_buffer); tail; tail = tail->next)
	    {
	      tail->need_adjustment
		= tail->charpos == (tail->insertion_type ? from : to);
	      need_marker_adjustment |= tail->need_adjustment;
	    }
	  saved_pt = PT, saved_pt_byte = PT_BYTE;
	  TEMP_SET_PT_BOTH (from, from_byte);
	  current_buffer->text->inhibit_shrinking = 1;
	  del_range_both (from, from_byte, to, to_byte, 1);
	  coding->src_pos = -chars;
	  coding->src_pos_byte = -bytes;
	}
      else
	{
	  coding->src_pos = from;
	  coding->src_pos_byte = from_byte;
	}
    }

  if (CODING_REQUIRE_DETECTION (coding))
    detect_coding (coding);
  attrs = CODING_ID_ATTRS (coding->id);

  if (EQ (dst_object, Qt)
      || (! NILP (CODING_ATTR_POST_READ (attrs))
	  && NILP (dst_object)))
    {
      coding->dst_multibyte = !CODING_FOR_UNIBYTE (coding);
      coding->dst_object = code_conversion_save (1, coding->dst_multibyte);
      coding->dst_pos = BEG;
      coding->dst_pos_byte = BEG_BYTE;
    }
  else if (BUFFERP (dst_object))
    {
      code_conversion_save (0, 0);
      coding->dst_object = dst_object;
      coding->dst_pos = BUF_PT (XBUFFER (dst_object));
      coding->dst_pos_byte = BUF_PT_BYTE (XBUFFER (dst_object));
      coding->dst_multibyte
	= ! NILP (BVAR (XBUFFER (dst_object), enable_multibyte_characters));
    }
  else
    {
      code_conversion_save (0, 0);
      coding->dst_object = Qnil;
      /* Most callers presume this will return a multibyte result, and they
	 won't use `binary' or `raw-text' anyway, so let's not worry about
	 CODING_FOR_UNIBYTE.  */
      coding->dst_multibyte = 1;
    }

  decode_coding (coding);

  if (BUFFERP (coding->dst_object))
    set_buffer_internal (XBUFFER (coding->dst_object));

  if (! NILP (CODING_ATTR_POST_READ (attrs)))
    {
      struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
      EMACS_INT prev_Z = Z, prev_Z_BYTE = Z_BYTE;
      Lisp_Object val;

      TEMP_SET_PT_BOTH (coding->dst_pos, coding->dst_pos_byte);
      GCPRO5 (coding->src_object, coding->dst_object, src_object, dst_object,
	      old_deactivate_mark);
      val = safe_call1 (CODING_ATTR_POST_READ (attrs),
			make_number (coding->produced_char));
      UNGCPRO;
      CHECK_NATNUM (val);
      coding->produced_char += Z - prev_Z;
      coding->produced += Z_BYTE - prev_Z_BYTE;
    }

  if (EQ (dst_object, Qt))
    {
      coding->dst_object = Fbuffer_string ();
    }
  else if (NILP (dst_object) && BUFFERP (coding->dst_object))
    {
      set_buffer_internal (XBUFFER (coding->dst_object));
      if (dst_bytes < coding->produced)
	{
	  destination = xrealloc (destination, coding->produced);
	  if (! destination)
	    {
	      record_conversion_result (coding,
					CODING_RESULT_INSUFFICIENT_MEM);
	      unbind_to (count, Qnil);
	      return;
	    }
	  if (BEGV < GPT && GPT < BEGV + coding->produced_char)
	    move_gap_both (BEGV, BEGV_BYTE);
	  memcpy (destination, BEGV_ADDR, coding->produced);
	  coding->destination = destination;
	}
    }

  if (saved_pt >= 0)
    {
      /* This is the case of:
	 (BUFFERP (src_object) && EQ (src_object, dst_object))
	 As we have moved PT while replacing the original buffer
	 contents, we must recover it now.  */
      set_buffer_internal (XBUFFER (src_object));
      current_buffer->text->inhibit_shrinking = 0;
      if (saved_pt < from)
	TEMP_SET_PT_BOTH (saved_pt, saved_pt_byte);
      else if (saved_pt < from + chars)
	TEMP_SET_PT_BOTH (from, from_byte);
      else if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	TEMP_SET_PT_BOTH (saved_pt + (coding->produced_char - chars),
			  saved_pt_byte + (coding->produced - bytes));
      else
	TEMP_SET_PT_BOTH (saved_pt + (coding->produced - bytes),
			  saved_pt_byte + (coding->produced - bytes));

      if (need_marker_adjustment)
	{
	  struct Lisp_Marker *tail;

	  for (tail = BUF_MARKERS (current_buffer); tail; tail = tail->next)
	    if (tail->need_adjustment)
	      {
		tail->need_adjustment = 0;
		if (tail->insertion_type)
		  {
		    tail->bytepos = from_byte;
		    tail->charpos = from;
		  }
		else
		  {
		    tail->bytepos = from_byte + coding->produced;
		    tail->charpos
		      = (NILP (BVAR (current_buffer, enable_multibyte_characters))
			 ? tail->bytepos : from + coding->produced_char);
		  }
	      }
	}
    }

  Vdeactivate_mark = old_deactivate_mark;
  unbind_to (count, coding->dst_object);
}


void
encode_coding_object (struct coding_system *coding,
		      Lisp_Object src_object,
		      EMACS_INT from, EMACS_INT from_byte,
		      EMACS_INT to, EMACS_INT to_byte,
		      Lisp_Object dst_object)
{
  int count = SPECPDL_INDEX ();
  EMACS_INT chars = to - from;
  EMACS_INT bytes = to_byte - from_byte;
  Lisp_Object attrs;
  int saved_pt = -1, saved_pt_byte IF_LINT (= 0);
  int need_marker_adjustment = 0;
  int kill_src_buffer = 0;
  Lisp_Object old_deactivate_mark;

  old_deactivate_mark = Vdeactivate_mark;

  coding->src_object = src_object;
  coding->src_chars = chars;
  coding->src_bytes = bytes;
  coding->src_multibyte = chars < bytes;

  attrs = CODING_ID_ATTRS (coding->id);

  if (EQ (src_object, dst_object))
    {
      struct Lisp_Marker *tail;

      for (tail = BUF_MARKERS (current_buffer); tail; tail = tail->next)
	{
	  tail->need_adjustment
	    = tail->charpos == (tail->insertion_type ? from : to);
	  need_marker_adjustment |= tail->need_adjustment;
	}
    }

  if (! NILP (CODING_ATTR_PRE_WRITE (attrs)))
    {
      coding->src_object = code_conversion_save (1, coding->src_multibyte);
      set_buffer_internal (XBUFFER (coding->src_object));
      if (STRINGP (src_object))
	insert_from_string (src_object, from, from_byte, chars, bytes, 0);
      else if (BUFFERP (src_object))
	insert_from_buffer (XBUFFER (src_object), from, chars, 0);
      else
	insert_1_both ((char *) coding->source + from, chars, bytes, 0, 0, 0);

      if (EQ (src_object, dst_object))
	{
	  set_buffer_internal (XBUFFER (src_object));
	  saved_pt = PT, saved_pt_byte = PT_BYTE;
	  del_range_both (from, from_byte, to, to_byte, 1);
	  set_buffer_internal (XBUFFER (coding->src_object));
	}

      {
	Lisp_Object args[3];
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

	GCPRO5 (coding->src_object, coding->dst_object, src_object, dst_object,
		old_deactivate_mark);
	args[0] = CODING_ATTR_PRE_WRITE (attrs);
	args[1] = make_number (BEG);
	args[2] = make_number (Z);
	safe_call (3, args);
	UNGCPRO;
      }
      if (XBUFFER (coding->src_object) != current_buffer)
	kill_src_buffer = 1;
      coding->src_object = Fcurrent_buffer ();
      if (BEG != GPT)
	move_gap_both (BEG, BEG_BYTE);
      coding->src_chars = Z - BEG;
      coding->src_bytes = Z_BYTE - BEG_BYTE;
      coding->src_pos = BEG;
      coding->src_pos_byte = BEG_BYTE;
      coding->src_multibyte = Z < Z_BYTE;
    }
  else if (STRINGP (src_object))
    {
      code_conversion_save (0, 0);
      coding->src_pos = from;
      coding->src_pos_byte = from_byte;
    }
  else if (BUFFERP (src_object))
    {
      code_conversion_save (0, 0);
      set_buffer_internal (XBUFFER (src_object));
      if (EQ (src_object, dst_object))
	{
	  saved_pt = PT, saved_pt_byte = PT_BYTE;
	  coding->src_object = del_range_1 (from, to, 1, 1);
	  coding->src_pos = 0;
	  coding->src_pos_byte = 0;
	}
      else
	{
	  if (from < GPT && to >= GPT)
	    move_gap_both (from, from_byte);
	  coding->src_pos = from;
	  coding->src_pos_byte = from_byte;
	}
    }
  else
    code_conversion_save (0, 0);

  if (BUFFERP (dst_object))
    {
      coding->dst_object = dst_object;
      if (EQ (src_object, dst_object))
	{
	  coding->dst_pos = from;
	  coding->dst_pos_byte = from_byte;
	}
      else
	{
	  struct buffer *current = current_buffer;

	  set_buffer_temp (XBUFFER (dst_object));
	  coding->dst_pos = PT;
	  coding->dst_pos_byte = PT_BYTE;
	  move_gap_both (coding->dst_pos, coding->dst_pos_byte);
	  set_buffer_temp (current);
	}
      coding->dst_multibyte
	= ! NILP (BVAR (XBUFFER (dst_object), enable_multibyte_characters));
    }
  else if (EQ (dst_object, Qt))
    {
      ptrdiff_t dst_bytes = max (1, coding->src_chars);
      coding->dst_object = Qnil;
      coding->destination = (unsigned char *) xmalloc (dst_bytes);
      coding->dst_bytes = dst_bytes;
      coding->dst_multibyte = 0;
    }
  else
    {
      coding->dst_object = Qnil;
      coding->dst_multibyte = 0;
    }

  encode_coding (coding);

  if (EQ (dst_object, Qt))
    {
      if (BUFFERP (coding->dst_object))
	coding->dst_object = Fbuffer_string ();
      else
	{
	  coding->dst_object
	    = make_unibyte_string ((char *) coding->destination,
				   coding->produced);
	  xfree (coding->destination);
	}
    }

  if (saved_pt >= 0)
    {
      /* This is the case of:
	 (BUFFERP (src_object) && EQ (src_object, dst_object))
	 As we have moved PT while replacing the original buffer
	 contents, we must recover it now.  */
      set_buffer_internal (XBUFFER (src_object));
      if (saved_pt < from)
	TEMP_SET_PT_BOTH (saved_pt, saved_pt_byte);
      else if (saved_pt < from + chars)
	TEMP_SET_PT_BOTH (from, from_byte);
      else if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	TEMP_SET_PT_BOTH (saved_pt + (coding->produced_char - chars),
			  saved_pt_byte + (coding->produced - bytes));
      else
	TEMP_SET_PT_BOTH (saved_pt + (coding->produced - bytes),
			  saved_pt_byte + (coding->produced - bytes));

      if (need_marker_adjustment)
	{
	  struct Lisp_Marker *tail;

	  for (tail = BUF_MARKERS (current_buffer); tail; tail = tail->next)
	    if (tail->need_adjustment)
	      {
		tail->need_adjustment = 0;
		if (tail->insertion_type)
		  {
		    tail->bytepos = from_byte;
		    tail->charpos = from;
		  }
		else
		  {
		    tail->bytepos = from_byte + coding->produced;
		    tail->charpos
		      = (NILP (BVAR (current_buffer, enable_multibyte_characters))
			 ? tail->bytepos : from + coding->produced_char);
		  }
	      }
	}
    }

  if (kill_src_buffer)
    Fkill_buffer (coding->src_object);

  Vdeactivate_mark = old_deactivate_mark;
  unbind_to (count, Qnil);
}


Lisp_Object
preferred_coding_system (void)
{
  int id = coding_categories[coding_priorities[0]].id;

  return CODING_ID_NAME (id);
}


#ifdef emacs
/*** 8. Emacs Lisp library functions ***/

DEFUN ("coding-system-p", Fcoding_system_p, Scoding_system_p, 1, 1, 0,
       doc: /* Return t if OBJECT is nil or a coding-system.
See the documentation of `define-coding-system' for information
about coding-system objects.  */)
  (Lisp_Object object)
{
  if (NILP (object)
      || CODING_SYSTEM_ID (object) >= 0)
    return Qt;
  if (! SYMBOLP (object)
      || NILP (Fget (object, Qcoding_system_define_form)))
    return Qnil;
  return Qt;
}

DEFUN ("read-non-nil-coding-system", Fread_non_nil_coding_system,
       Sread_non_nil_coding_system, 1, 1, 0,
       doc: /* Read a coding system from the minibuffer, prompting with string PROMPT.  */)
  (Lisp_Object prompt)
{
  Lisp_Object val;
  do
    {
      val = Fcompleting_read (prompt, Vcoding_system_alist, Qnil,
			      Qt, Qnil, Qcoding_system_history, Qnil, Qnil);
    }
  while (SCHARS (val) == 0);
  return (Fintern (val, Qnil));
}

DEFUN ("read-coding-system", Fread_coding_system, Sread_coding_system, 1, 2, 0,
       doc: /* Read a coding system from the minibuffer, prompting with string PROMPT.
If the user enters null input, return second argument DEFAULT-CODING-SYSTEM.
Ignores case when completing coding systems (all Emacs coding systems
are lower-case).  */)
  (Lisp_Object prompt, Lisp_Object default_coding_system)
{
  Lisp_Object val;
  int count = SPECPDL_INDEX ();

  if (SYMBOLP (default_coding_system))
    default_coding_system = SYMBOL_NAME (default_coding_system);
  specbind (Qcompletion_ignore_case, Qt);
  val = Fcompleting_read (prompt, Vcoding_system_alist, Qnil,
			  Qt, Qnil, Qcoding_system_history,
			  default_coding_system, Qnil);
  unbind_to (count, Qnil);
  return (SCHARS (val) == 0 ? Qnil : Fintern (val, Qnil));
}

DEFUN ("check-coding-system", Fcheck_coding_system, Scheck_coding_system,
       1, 1, 0,
       doc: /* Check validity of CODING-SYSTEM.
If valid, return CODING-SYSTEM, else signal a `coding-system-error' error.
It is valid if it is nil or a symbol defined as a coding system by the
function `define-coding-system'.  */)
  (Lisp_Object coding_system)
{
  Lisp_Object define_form;

  define_form = Fget (coding_system, Qcoding_system_define_form);
  if (! NILP (define_form))
    {
      Fput (coding_system, Qcoding_system_define_form, Qnil);
      safe_eval (define_form);
    }
  if (!NILP (Fcoding_system_p (coding_system)))
    return coding_system;
  xsignal1 (Qcoding_system_error, coding_system);
}


/* Detect how the bytes at SRC of length SRC_BYTES are encoded.  If
   HIGHEST is nonzero, return the coding system of the highest
   priority among the detected coding systems.  Otherwise return a
   list of detected coding systems sorted by their priorities.  If
   MULTIBYTEP is nonzero, it is assumed that the bytes are in correct
   multibyte form but contains only ASCII and eight-bit chars.
   Otherwise, the bytes are raw bytes.

   CODING-SYSTEM controls the detection as below:

   If it is nil, detect both text-format and eol-format.  If the
   text-format part of CODING-SYSTEM is already specified
   (e.g. `iso-latin-1'), detect only eol-format.  If the eol-format
   part of CODING-SYSTEM is already specified (e.g. `undecided-unix'),
   detect only text-format.  */

Lisp_Object
detect_coding_system (const unsigned char *src,
		      EMACS_INT src_chars, EMACS_INT src_bytes,
		      int highest, int multibytep,
		      Lisp_Object coding_system)
{
  const unsigned char *src_end = src + src_bytes;
  Lisp_Object attrs, eol_type;
  Lisp_Object val = Qnil;
  struct coding_system coding;
  ptrdiff_t id;
  struct coding_detection_info detect_info;
  enum coding_category base_category;
  int null_byte_found = 0, eight_bit_found = 0;

  if (NILP (coding_system))
    coding_system = Qundecided;
  setup_coding_system (coding_system, &coding);
  attrs = CODING_ID_ATTRS (coding.id);
  eol_type = CODING_ID_EOL_TYPE (coding.id);
  coding_system = CODING_ATTR_BASE_NAME (attrs);

  coding.source = src;
  coding.src_chars = src_chars;
  coding.src_bytes = src_bytes;
  coding.src_multibyte = multibytep;
  coding.consumed = 0;
  coding.mode |= CODING_MODE_LAST_BLOCK;
  coding.head_ascii = 0;

  detect_info.checked = detect_info.found = detect_info.rejected = 0;

  /* At first, detect text-format if necessary.  */
  base_category = XINT (CODING_ATTR_CATEGORY (attrs));
  if (base_category == coding_category_undecided)
    {
      enum coding_category category IF_LINT (= 0);
      struct coding_system *this IF_LINT (= NULL);
      int c, i;

      /* Skip all ASCII bytes except for a few ISO2022 controls.  */
      for (; src < src_end; src++)
	{
	  c = *src;
	  if (c & 0x80)
	    {
	      eight_bit_found = 1;
	      if (null_byte_found)
		break;
	    }
	  else if (c < 0x20)
	    {
	      if ((c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO)
		  && ! inhibit_iso_escape_detection
		  && ! detect_info.checked)
		{
		  if (detect_coding_iso_2022 (&coding, &detect_info))
		    {
		      /* We have scanned the whole data.  */
		      if (! (detect_info.rejected & CATEGORY_MASK_ISO_7_ELSE))
			{
			  /* We didn't find an 8-bit code.  We may
			     have found a null-byte, but it's very
			     rare that a binary file confirm to
			     ISO-2022.  */
			  src = src_end;
			  coding.head_ascii = src - coding.source;
			}
		      detect_info.rejected |= ~CATEGORY_MASK_ISO_ESCAPE;
		      break;
		    }
		}
	      else if (! c && !inhibit_null_byte_detection)
		{
		  null_byte_found = 1;
		  if (eight_bit_found)
		    break;
		}
	      if (! eight_bit_found)
		coding.head_ascii++;
	    }
	  else if (! eight_bit_found)
	    coding.head_ascii++;
	}

      if (null_byte_found || eight_bit_found
	  || coding.head_ascii < coding.src_bytes
	  || detect_info.found)
	{
	  if (coding.head_ascii == coding.src_bytes)
	    /* As all bytes are 7-bit, we can ignore non-ISO-2022 codings.  */
	    for (i = 0; i < coding_category_raw_text; i++)
	      {
		category = coding_priorities[i];
		this = coding_categories + category;
		if (detect_info.found & (1 << category))
		  break;
	      }
	  else
	    {
	      if (null_byte_found)
		{
		  detect_info.checked |= ~CATEGORY_MASK_UTF_16;
		  detect_info.rejected |= ~CATEGORY_MASK_UTF_16;
		}
	      for (i = 0; i < coding_category_raw_text; i++)
		{
		  category = coding_priorities[i];
		  this = coding_categories + category;

		  if (this->id < 0)
		    {
		      /* No coding system of this category is defined.  */
		      detect_info.rejected |= (1 << category);
		    }
		  else if (category >= coding_category_raw_text)
		    continue;
		  else if (detect_info.checked & (1 << category))
		    {
		      if (highest
			  && (detect_info.found & (1 << category)))
			break;
		    }
		  else if ((*(this->detector)) (&coding, &detect_info)
			   && highest
			   && (detect_info.found & (1 << category)))
		    {
		      if (category == coding_category_utf_16_auto)
			{
			  if (detect_info.found & CATEGORY_MASK_UTF_16_LE)
			    category = coding_category_utf_16_le;
			  else
			    category = coding_category_utf_16_be;
			}
		      break;
		    }
		}
	    }
	}

      if ((detect_info.rejected & CATEGORY_MASK_ANY) == CATEGORY_MASK_ANY
	  || null_byte_found)
	{
	  detect_info.found = CATEGORY_MASK_RAW_TEXT;
	  id = CODING_SYSTEM_ID (Qno_conversion);
	  val = Fcons (make_number (id), Qnil);
	}
      else if (! detect_info.rejected && ! detect_info.found)
	{
	  detect_info.found = CATEGORY_MASK_ANY;
	  id = coding_categories[coding_category_undecided].id;
	  val = Fcons (make_number (id), Qnil);
	}
      else if (highest)
	{
	  if (detect_info.found)
	    {
	      detect_info.found = 1 << category;
	      val = Fcons (make_number (this->id), Qnil);
	    }
	  else
	    for (i = 0; i < coding_category_raw_text; i++)
	      if (! (detect_info.rejected & (1 << coding_priorities[i])))
		{
		  detect_info.found = 1 << coding_priorities[i];
		  id = coding_categories[coding_priorities[i]].id;
		  val = Fcons (make_number (id), Qnil);
		  break;
		}
	}
      else
	{
	  int mask = detect_info.rejected | detect_info.found;
	  int found = 0;

	  for (i = coding_category_raw_text - 1; i >= 0; i--)
	    {
	      category = coding_priorities[i];
	      if (! (mask & (1 << category)))
		{
		  found |= 1 << category;
		  id = coding_categories[category].id;
		  if (id >= 0)
		    val = Fcons (make_number (id), val);
		}
	    }
	  for (i = coding_category_raw_text - 1; i >= 0; i--)
	    {
	      category = coding_priorities[i];
	      if (detect_info.found & (1 << category))
		{
		  id = coding_categories[category].id;
		  val = Fcons (make_number (id), val);
		}
	    }
	  detect_info.found |= found;
	}
    }
  else if (base_category == coding_category_utf_8_auto)
    {
      if (detect_coding_utf_8 (&coding, &detect_info))
	{
	  struct coding_system *this;

	  if (detect_info.found & CATEGORY_MASK_UTF_8_SIG)
	    this = coding_categories + coding_category_utf_8_sig;
	  else
	    this = coding_categories + coding_category_utf_8_nosig;
	  val = Fcons (make_number (this->id), Qnil);
	}
    }
  else if (base_category == coding_category_utf_16_auto)
    {
      if (detect_coding_utf_16 (&coding, &detect_info))
	{
	  struct coding_system *this;

	  if (detect_info.found & CATEGORY_MASK_UTF_16_LE)
	    this = coding_categories + coding_category_utf_16_le;
	  else if (detect_info.found & CATEGORY_MASK_UTF_16_BE)
	    this = coding_categories + coding_category_utf_16_be;
	  else if (detect_info.rejected & CATEGORY_MASK_UTF_16_LE_NOSIG)
	    this = coding_categories + coding_category_utf_16_be_nosig;
	  else
	    this = coding_categories + coding_category_utf_16_le_nosig;
	  val = Fcons (make_number (this->id), Qnil);
	}
    }
  else
    {
      detect_info.found = 1 << XINT (CODING_ATTR_CATEGORY (attrs));
      val = Fcons (make_number (coding.id), Qnil);
    }

  /* Then, detect eol-format if necessary.  */
  {
    int normal_eol = -1, utf_16_be_eol = -1, utf_16_le_eol = -1;
    Lisp_Object tail;

    if (VECTORP (eol_type))
      {
	if (detect_info.found & ~CATEGORY_MASK_UTF_16)
	  {
	    if (null_byte_found)
	      normal_eol = EOL_SEEN_LF;
	    else
	      normal_eol = detect_eol (coding.source, src_bytes,
				       coding_category_raw_text);
	  }
	if (detect_info.found & (CATEGORY_MASK_UTF_16_BE
				 | CATEGORY_MASK_UTF_16_BE_NOSIG))
	  utf_16_be_eol = detect_eol (coding.source, src_bytes,
				      coding_category_utf_16_be);
	if (detect_info.found & (CATEGORY_MASK_UTF_16_LE
				 | CATEGORY_MASK_UTF_16_LE_NOSIG))
	  utf_16_le_eol = detect_eol (coding.source, src_bytes,
				      coding_category_utf_16_le);
      }
    else
      {
	if (EQ (eol_type, Qunix))
	  normal_eol = utf_16_be_eol = utf_16_le_eol = EOL_SEEN_LF;
	else if (EQ (eol_type, Qdos))
	  normal_eol = utf_16_be_eol = utf_16_le_eol = EOL_SEEN_CRLF;
	else
	  normal_eol = utf_16_be_eol = utf_16_le_eol = EOL_SEEN_CR;
      }

    for (tail = val; CONSP (tail); tail = XCDR (tail))
      {
	enum coding_category category;
	int this_eol;

	id = XINT (XCAR (tail));
	attrs = CODING_ID_ATTRS (id);
	category = XINT (CODING_ATTR_CATEGORY (attrs));
	eol_type = CODING_ID_EOL_TYPE (id);
	if (VECTORP (eol_type))
	  {
	    if (category == coding_category_utf_16_be
		|| category == coding_category_utf_16_be_nosig)
	      this_eol = utf_16_be_eol;
	    else if (category == coding_category_utf_16_le
		     || category == coding_category_utf_16_le_nosig)
	      this_eol = utf_16_le_eol;
	    else
	      this_eol = normal_eol;

	    if (this_eol == EOL_SEEN_LF)
	      XSETCAR (tail, AREF (eol_type, 0));
	    else if (this_eol == EOL_SEEN_CRLF)
	      XSETCAR (tail, AREF (eol_type, 1));
	    else if (this_eol == EOL_SEEN_CR)
	      XSETCAR (tail, AREF (eol_type, 2));
	    else
	      XSETCAR (tail, CODING_ID_NAME (id));
	  }
	else
	  XSETCAR (tail, CODING_ID_NAME (id));
      }
  }

  return (highest ? (CONSP (val) ? XCAR (val) : Qnil) : val);
}


DEFUN ("detect-coding-region", Fdetect_coding_region, Sdetect_coding_region,
       2, 3, 0,
       doc: /* Detect coding system of the text in the region between START and END.
Return a list of possible coding systems ordered by priority.
The coding systems to try and their priorities follows what
the function `coding-system-priority-list' (which see) returns.

If only ASCII characters are found (except for such ISO-2022 control
characters as ESC), it returns a list of single element `undecided'
or its subsidiary coding system according to a detected end-of-line
format.

If optional argument HIGHEST is non-nil, return the coding system of
highest priority.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object highest)
{
  int from, to;
  int from_byte, to_byte;

  CHECK_NUMBER_COERCE_MARKER (start);
  CHECK_NUMBER_COERCE_MARKER (end);

  validate_region (&start, &end);
  from = XINT (start), to = XINT (end);
  from_byte = CHAR_TO_BYTE (from);
  to_byte = CHAR_TO_BYTE (to);

  if (from < GPT && to >= GPT)
    move_gap_both (to, to_byte);

  return detect_coding_system (BYTE_POS_ADDR (from_byte),
			       to - from, to_byte - from_byte,
			       !NILP (highest),
			       !NILP (BVAR (current_buffer
				      , enable_multibyte_characters)),
			       Qnil);
}

DEFUN ("detect-coding-string", Fdetect_coding_string, Sdetect_coding_string,
       1, 2, 0,
       doc: /* Detect coding system of the text in STRING.
Return a list of possible coding systems ordered by priority.
The coding systems to try and their priorities follows what
the function `coding-system-priority-list' (which see) returns.

If only ASCII characters are found (except for such ISO-2022 control
characters as ESC), it returns a list of single element `undecided'
or its subsidiary coding system according to a detected end-of-line
format.

If optional argument HIGHEST is non-nil, return the coding system of
highest priority.  */)
  (Lisp_Object string, Lisp_Object highest)
{
  CHECK_STRING (string);

  return detect_coding_system (SDATA (string),
			       SCHARS (string), SBYTES (string),
			       !NILP (highest), STRING_MULTIBYTE (string),
			       Qnil);
}


static inline int
char_encodable_p (int c, Lisp_Object attrs)
{
  Lisp_Object tail;
  struct charset *charset;
  Lisp_Object translation_table;

  translation_table = CODING_ATTR_TRANS_TBL (attrs);
  if (! NILP (translation_table))
    c = translate_char (translation_table, c);
  for (tail = CODING_ATTR_CHARSET_LIST (attrs);
       CONSP (tail); tail = XCDR (tail))
    {
      charset = CHARSET_FROM_ID (XINT (XCAR (tail)));
      if (CHAR_CHARSET_P (c, charset))
	break;
    }
  return (! NILP (tail));
}


/* Return a list of coding systems that safely encode the text between
   START and END.  If EXCLUDE is non-nil, it is a list of coding
   systems not to check.  The returned list doesn't contain any such
   coding systems.  In any case, if the text contains only ASCII or is
   unibyte, return t.  */

DEFUN ("find-coding-systems-region-internal",
       Ffind_coding_systems_region_internal,
       Sfind_coding_systems_region_internal, 2, 3, 0,
       doc: /* Internal use only.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object exclude)
{
  Lisp_Object coding_attrs_list, safe_codings;
  EMACS_INT start_byte, end_byte;
  const unsigned char *p, *pbeg, *pend;
  int c;
  Lisp_Object tail, elt, work_table;

  if (STRINGP (start))
    {
      if (!STRING_MULTIBYTE (start)
	  || SCHARS (start) == SBYTES (start))
	return Qt;
      start_byte = 0;
      end_byte = SBYTES (start);
    }
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start);
      CHECK_NUMBER_COERCE_MARKER (end);
      if (XINT (start) < BEG || XINT (end) > Z || XINT (start) > XINT (end))
	args_out_of_range (start, end);
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	return Qt;
      start_byte = CHAR_TO_BYTE (XINT (start));
      end_byte = CHAR_TO_BYTE (XINT (end));
      if (XINT (end) - XINT (start) == end_byte - start_byte)
	return Qt;

      if (XINT (start) < GPT && XINT (end) > GPT)
	{
	  if ((GPT - XINT (start)) < (XINT (end) - GPT))
	    move_gap_both (XINT (start), start_byte);
	  else
	    move_gap_both (XINT (end), end_byte);
	}
    }

  coding_attrs_list = Qnil;
  for (tail = Vcoding_system_list; CONSP (tail); tail = XCDR (tail))
    if (NILP (exclude)
	|| NILP (Fmemq (XCAR (tail), exclude)))
      {
	Lisp_Object attrs;

	attrs = AREF (CODING_SYSTEM_SPEC (XCAR (tail)), 0);
	if (EQ (XCAR (tail), CODING_ATTR_BASE_NAME (attrs))
	    && ! EQ (CODING_ATTR_TYPE (attrs), Qundecided))
	  {
	    ASET (attrs, coding_attr_trans_tbl,
		  get_translation_table (attrs, 1, NULL));
	    coding_attrs_list = Fcons (attrs, coding_attrs_list);
	  }
      }

  if (STRINGP (start))
    p = pbeg = SDATA (start);
  else
    p = pbeg = BYTE_POS_ADDR (start_byte);
  pend = p + (end_byte - start_byte);

  while (p < pend && ASCII_BYTE_P (*p)) p++;
  while (p < pend && ASCII_BYTE_P (*(pend - 1))) pend--;

  work_table = Fmake_char_table (Qnil, Qnil);
  while (p < pend)
    {
      if (ASCII_BYTE_P (*p))
	p++;
      else
	{
	  c = STRING_CHAR_ADVANCE (p);
	  if (!NILP (char_table_ref (work_table, c)))
	    /* This character was already checked.  Ignore it.  */
	    continue;

	  charset_map_loaded = 0;
	  for (tail = coding_attrs_list; CONSP (tail);)
	    {
	      elt = XCAR (tail);
	      if (NILP (elt))
		tail = XCDR (tail);
	      else if (char_encodable_p (c, elt))
		tail = XCDR (tail);
	      else if (CONSP (XCDR (tail)))
		{
		  XSETCAR (tail, XCAR (XCDR (tail)));
		  XSETCDR (tail, XCDR (XCDR (tail)));
		}
	      else
		{
		  XSETCAR (tail, Qnil);
		  tail = XCDR (tail);
		}
	    }
	  if (charset_map_loaded)
	    {
	      EMACS_INT p_offset = p - pbeg, pend_offset = pend - pbeg;

	      if (STRINGP (start))
		pbeg = SDATA (start);
	      else
		pbeg = BYTE_POS_ADDR (start_byte);
	      p = pbeg + p_offset;
	      pend = pbeg + pend_offset;
	    }
	  char_table_set (work_table, c, Qt);
	}
    }

  safe_codings = list2 (Qraw_text, Qno_conversion);
  for (tail = coding_attrs_list; CONSP (tail); tail = XCDR (tail))
    if (! NILP (XCAR (tail)))
      safe_codings = Fcons (CODING_ATTR_BASE_NAME (XCAR (tail)), safe_codings);

  return safe_codings;
}


DEFUN ("unencodable-char-position", Funencodable_char_position,
       Sunencodable_char_position, 3, 5, 0,
       doc: /*
Return position of first un-encodable character in a region.
START and END specify the region and CODING-SYSTEM specifies the
encoding to check.  Return nil if CODING-SYSTEM does encode the region.

If optional 4th argument COUNT is non-nil, it specifies at most how
many un-encodable characters to search.  In this case, the value is a
list of positions.

If optional 5th argument STRING is non-nil, it is a string to search
for un-encodable characters.  In that case, START and END are indexes
to the string.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object coding_system, Lisp_Object count, Lisp_Object string)
{
  int n;
  struct coding_system coding;
  Lisp_Object attrs, charset_list, translation_table;
  Lisp_Object positions;
  int from, to;
  const unsigned char *p, *stop, *pend;
  int ascii_compatible;

  setup_coding_system (Fcheck_coding_system (coding_system), &coding);
  attrs = CODING_ID_ATTRS (coding.id);
  if (EQ (CODING_ATTR_TYPE (attrs), Qraw_text))
    return Qnil;
  ascii_compatible = ! NILP (CODING_ATTR_ASCII_COMPAT (attrs));
  charset_list = CODING_ATTR_CHARSET_LIST (attrs);
  translation_table = get_translation_table (attrs, 1, NULL);

  if (NILP (string))
    {
      validate_region (&start, &end);
      from = XINT (start);
      to = XINT (end);
      if (NILP (BVAR (current_buffer, enable_multibyte_characters))
	  || (ascii_compatible
	      && (to - from) == (CHAR_TO_BYTE (to) - (CHAR_TO_BYTE (from)))))
	return Qnil;
      p = CHAR_POS_ADDR (from);
      pend = CHAR_POS_ADDR (to);
      if (from < GPT && to >= GPT)
	stop = GPT_ADDR;
      else
	stop = pend;
    }
  else
    {
      CHECK_STRING (string);
      CHECK_NATNUM (start);
      CHECK_NATNUM (end);
      from = XINT (start);
      to = XINT (end);
      if (from > to
	  || to > SCHARS (string))
	args_out_of_range_3 (string, start, end);
      if (! STRING_MULTIBYTE (string))
	return Qnil;
      p = SDATA (string) + string_char_to_byte (string, from);
      stop = pend = SDATA (string) + string_char_to_byte (string, to);
      if (ascii_compatible && (to - from) == (pend - p))
	return Qnil;
    }

  if (NILP (count))
    n = 1;
  else
    {
      CHECK_NATNUM (count);
      n = XINT (count);
    }

  positions = Qnil;
  charset_map_loaded = 0;
  while (1)
    {
      int c;

      if (ascii_compatible)
	while (p < stop && ASCII_BYTE_P (*p))
	  p++, from++;
      if (p >= stop)
	{
	  if (p >= pend)
	    break;
	  stop = pend;
	  p = GAP_END_ADDR;
	}

      c = STRING_CHAR_ADVANCE (p);
      if (! (ASCII_CHAR_P (c) && ascii_compatible)
	  && ! char_charset (translate_char (translation_table, c),
			     charset_list, NULL))
	{
	  positions = Fcons (make_number (from), positions);
	  n--;
	  if (n == 0)
	    break;
	}

      from++;
      if (charset_map_loaded && NILP (string))
	{
	  p = CHAR_POS_ADDR (from);
	  pend = CHAR_POS_ADDR (to);
	  if (from < GPT && to >= GPT)
	    stop = GPT_ADDR;
	  else
	    stop = pend;
	  charset_map_loaded = 0;
	}
    }

  return (NILP (count) ? Fcar (positions) : Fnreverse (positions));
}


DEFUN ("check-coding-systems-region", Fcheck_coding_systems_region,
       Scheck_coding_systems_region, 3, 3, 0,
       doc: /* Check if the region is encodable by coding systems.

START and END are buffer positions specifying the region.
CODING-SYSTEM-LIST is a list of coding systems to check.

The value is an alist ((CODING-SYSTEM POS0 POS1 ...) ...), where
CODING-SYSTEM is a member of CODING-SYSTEM-LIST and can't encode the
whole region, POS0, POS1, ... are buffer positions where non-encodable
characters are found.

If all coding systems in CODING-SYSTEM-LIST can encode the region, the
value is nil.

START may be a string.  In that case, check if the string is
encodable, and the value contains indices to the string instead of
buffer positions.  END is ignored.

If the current buffer (or START if it is a string) is unibyte, the value
is nil.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object coding_system_list)
{
  Lisp_Object list;
  EMACS_INT start_byte, end_byte;
  int pos;
  const unsigned char *p, *pbeg, *pend;
  int c;
  Lisp_Object tail, elt, attrs;

  if (STRINGP (start))
    {
      if (!STRING_MULTIBYTE (start)
	  || SCHARS (start) == SBYTES (start))
	return Qnil;
      start_byte = 0;
      end_byte = SBYTES (start);
      pos = 0;
    }
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start);
      CHECK_NUMBER_COERCE_MARKER (end);
      if (XINT (start) < BEG || XINT (end) > Z || XINT (start) > XINT (end))
	args_out_of_range (start, end);
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	return Qnil;
      start_byte = CHAR_TO_BYTE (XINT (start));
      end_byte = CHAR_TO_BYTE (XINT (end));
      if (XINT (end) - XINT (start) == end_byte - start_byte)
	return Qnil;

      if (XINT (start) < GPT && XINT (end) > GPT)
	{
	  if ((GPT - XINT (start)) < (XINT (end) - GPT))
	    move_gap_both (XINT (start), start_byte);
	  else
	    move_gap_both (XINT (end), end_byte);
	}
      pos = XINT (start);
    }

  list = Qnil;
  for (tail = coding_system_list; CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      attrs = AREF (CODING_SYSTEM_SPEC (elt), 0);
      ASET (attrs, coding_attr_trans_tbl,
	    get_translation_table (attrs, 1, NULL));
      list = Fcons (Fcons (elt, Fcons (attrs, Qnil)), list);
    }

  if (STRINGP (start))
    p = pbeg = SDATA (start);
  else
    p = pbeg = BYTE_POS_ADDR (start_byte);
  pend = p + (end_byte - start_byte);

  while (p < pend && ASCII_BYTE_P (*p)) p++, pos++;
  while (p < pend && ASCII_BYTE_P (*(pend - 1))) pend--;

  while (p < pend)
    {
      if (ASCII_BYTE_P (*p))
	p++;
      else
	{
	  c = STRING_CHAR_ADVANCE (p);

	  charset_map_loaded = 0;
	  for (tail = list; CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCDR (XCAR (tail));
	      if (! char_encodable_p (c, XCAR (elt)))
		XSETCDR (elt, Fcons (make_number (pos), XCDR (elt)));
	    }
	  if (charset_map_loaded)
	    {
	      EMACS_INT p_offset = p - pbeg, pend_offset = pend - pbeg;

	      if (STRINGP (start))
		pbeg = SDATA (start);
	      else
		pbeg = BYTE_POS_ADDR (start_byte);
	      p = pbeg + p_offset;
	      pend = pbeg + pend_offset;
	    }
	}
      pos++;
    }

  tail = list;
  list = Qnil;
  for (; CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      if (CONSP (XCDR (XCDR (elt))))
	list = Fcons (Fcons (XCAR (elt), Fnreverse (XCDR (XCDR (elt)))),
		      list);
    }

  return list;
}


static Lisp_Object
code_convert_region (Lisp_Object start, Lisp_Object end,
		     Lisp_Object coding_system, Lisp_Object dst_object,
		     int encodep, int norecord)
{
  struct coding_system coding;
  EMACS_INT from, from_byte, to, to_byte;
  Lisp_Object src_object;

  CHECK_NUMBER_COERCE_MARKER (start);
  CHECK_NUMBER_COERCE_MARKER (end);
  if (NILP (coding_system))
    coding_system = Qno_conversion;
  else
    CHECK_CODING_SYSTEM (coding_system);
  src_object = Fcurrent_buffer ();
  if (NILP (dst_object))
    dst_object = src_object;
  else if (! EQ (dst_object, Qt))
    CHECK_BUFFER (dst_object);

  validate_region (&start, &end);
  from = XFASTINT (start);
  from_byte = CHAR_TO_BYTE (from);
  to = XFASTINT (end);
  to_byte = CHAR_TO_BYTE (to);

  setup_coding_system (coding_system, &coding);
  coding.mode |= CODING_MODE_LAST_BLOCK;

  if (encodep)
    encode_coding_object (&coding, src_object, from, from_byte, to, to_byte,
			  dst_object);
  else
    decode_coding_object (&coding, src_object, from, from_byte, to, to_byte,
			  dst_object);
  if (! norecord)
    Vlast_coding_system_used = CODING_ID_NAME (coding.id);

  return (BUFFERP (dst_object)
	  ? make_number (coding.produced_char)
	  : coding.dst_object);
}


DEFUN ("decode-coding-region", Fdecode_coding_region, Sdecode_coding_region,
       3, 4, "r\nzCoding system: ",
       doc: /* Decode the current region from the specified coding system.
When called from a program, takes four arguments:
	START, END, CODING-SYSTEM, and DESTINATION.
START and END are buffer positions.

Optional 4th arguments DESTINATION specifies where the decoded text goes.
If nil, the region between START and END is replaced by the decoded text.
If buffer, the decoded text is inserted in that buffer after point (point
does not move).
In those cases, the length of the decoded text is returned.
If DESTINATION is t, the decoded text is returned.

This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object coding_system, Lisp_Object destination)
{
  return code_convert_region (start, end, coding_system, destination, 0, 0);
}

DEFUN ("encode-coding-region", Fencode_coding_region, Sencode_coding_region,
       3, 4, "r\nzCoding system: ",
       doc: /* Encode the current region by specified coding system.
When called from a program, takes four arguments:
        START, END, CODING-SYSTEM and DESTINATION.
START and END are buffer positions.

Optional 4th arguments DESTINATION specifies where the encoded text goes.
If nil, the region between START and END is replace by the encoded text.
If buffer, the encoded text is inserted in that buffer after point (point
does not move).
In those cases, the length of the encoded text is returned.
If DESTINATION is t, the encoded text is returned.

This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object coding_system, Lisp_Object destination)
{
  return code_convert_region (start, end, coding_system, destination, 1, 0);
}

Lisp_Object
code_convert_string (Lisp_Object string, Lisp_Object coding_system,
		     Lisp_Object dst_object, int encodep, int nocopy, int norecord)
{
  struct coding_system coding;
  EMACS_INT chars, bytes;

  CHECK_STRING (string);
  if (NILP (coding_system))
    {
      if (! norecord)
	Vlast_coding_system_used = Qno_conversion;
      if (NILP (dst_object))
	return (nocopy ? Fcopy_sequence (string) : string);
    }

  if (NILP (coding_system))
    coding_system = Qno_conversion;
  else
    CHECK_CODING_SYSTEM (coding_system);
  if (NILP (dst_object))
    dst_object = Qt;
  else if (! EQ (dst_object, Qt))
    CHECK_BUFFER (dst_object);

  setup_coding_system (coding_system, &coding);
  coding.mode |= CODING_MODE_LAST_BLOCK;
  chars = SCHARS (string);
  bytes = SBYTES (string);
  if (encodep)
    encode_coding_object (&coding, string, 0, 0, chars, bytes, dst_object);
  else
    decode_coding_object (&coding, string, 0, 0, chars, bytes, dst_object);
  if (! norecord)
    Vlast_coding_system_used = CODING_ID_NAME (coding.id);

  return (BUFFERP (dst_object)
	  ? make_number (coding.produced_char)
	  : coding.dst_object);
}


/* Encode or decode STRING according to CODING_SYSTEM.
   Do not set Vlast_coding_system_used.

   This function is called only from macros DECODE_FILE and
   ENCODE_FILE, thus we ignore character composition.  */

Lisp_Object
code_convert_string_norecord (Lisp_Object string, Lisp_Object coding_system,
			      int encodep)
{
  return code_convert_string (string, coding_system, Qt, encodep, 0, 1);
}


DEFUN ("decode-coding-string", Fdecode_coding_string, Sdecode_coding_string,
       2, 4, 0,
       doc: /* Decode STRING which is encoded in CODING-SYSTEM, and return the result.

Optional third arg NOCOPY non-nil means it is OK to return STRING itself
if the decoding operation is trivial.

Optional fourth arg BUFFER non-nil means that the decoded text is
inserted in that buffer after point (point does not move).  In this
case, the return value is the length of the decoded text.

This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)  */)
  (Lisp_Object string, Lisp_Object coding_system, Lisp_Object nocopy, Lisp_Object buffer)
{
  return code_convert_string (string, coding_system, buffer,
			      0, ! NILP (nocopy), 0);
}

DEFUN ("encode-coding-string", Fencode_coding_string, Sencode_coding_string,
       2, 4, 0,
       doc: /* Encode STRING to CODING-SYSTEM, and return the result.

Optional third arg NOCOPY non-nil means it is OK to return STRING
itself if the encoding operation is trivial.

Optional fourth arg BUFFER non-nil means that the encoded text is
inserted in that buffer after point (point does not move).  In this
case, the return value is the length of the encoded text.

This function sets `last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)  */)
  (Lisp_Object string, Lisp_Object coding_system, Lisp_Object nocopy, Lisp_Object buffer)
{
  return code_convert_string (string, coding_system, buffer,
			      1, ! NILP (nocopy), 0);
}


DEFUN ("decode-sjis-char", Fdecode_sjis_char, Sdecode_sjis_char, 1, 1, 0,
       doc: /* Decode a Japanese character which has CODE in shift_jis encoding.
Return the corresponding character.  */)
  (Lisp_Object code)
{
  Lisp_Object spec, attrs, val;
  struct charset *charset_roman, *charset_kanji, *charset_kana, *charset;
  EMACS_INT ch;
  int c;

  CHECK_NATNUM (code);
  ch = XFASTINT (code);
  CHECK_CODING_SYSTEM_GET_SPEC (Vsjis_coding_system, spec);
  attrs = AREF (spec, 0);

  if (ASCII_BYTE_P (ch)
      && ! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    return code;

  val = CODING_ATTR_CHARSET_LIST (attrs);
  charset_roman = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kana = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_kanji = CHARSET_FROM_ID (XINT (XCAR (val)));

  if (ch <= 0x7F)
    {
      c = ch;
      charset = charset_roman;
    }
  else if (ch >= 0xA0 && ch < 0xDF)
    {
      c = ch - 0x80;
      charset = charset_kana;
    }
  else
    {
      EMACS_INT c1 = ch >> 8;
      int c2 = ch & 0xFF;

      if (c1 < 0x81 || (c1 > 0x9F && c1 < 0xE0) || c1 > 0xEF
	  || c2 < 0x40 || c2 == 0x7F || c2 > 0xFC)
	error ("Invalid code: %"pI"d", ch);
      c = ch;
      SJIS_TO_JIS (c);
      charset = charset_kanji;
    }
  c = DECODE_CHAR (charset, c);
  if (c < 0)
    error ("Invalid code: %"pI"d", ch);
  return make_number (c);
}


DEFUN ("encode-sjis-char", Fencode_sjis_char, Sencode_sjis_char, 1, 1, 0,
       doc: /* Encode a Japanese character CH to shift_jis encoding.
Return the corresponding code in SJIS.  */)
  (Lisp_Object ch)
{
  Lisp_Object spec, attrs, charset_list;
  int c;
  struct charset *charset;
  unsigned code;

  CHECK_CHARACTER (ch);
  c = XFASTINT (ch);
  CHECK_CODING_SYSTEM_GET_SPEC (Vsjis_coding_system, spec);
  attrs = AREF (spec, 0);

  if (ASCII_CHAR_P (c)
      && ! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    return ch;

  charset_list = CODING_ATTR_CHARSET_LIST (attrs);
  charset = char_charset (c, charset_list, &code);
  if (code == CHARSET_INVALID_CODE (charset))
    error ("Can't encode by shift_jis encoding: %c", c);
  JIS_TO_SJIS (code);

  return make_number (code);
}

DEFUN ("decode-big5-char", Fdecode_big5_char, Sdecode_big5_char, 1, 1, 0,
       doc: /* Decode a Big5 character which has CODE in BIG5 coding system.
Return the corresponding character.  */)
  (Lisp_Object code)
{
  Lisp_Object spec, attrs, val;
  struct charset *charset_roman, *charset_big5, *charset;
  EMACS_INT ch;
  int c;

  CHECK_NATNUM (code);
  ch = XFASTINT (code);
  CHECK_CODING_SYSTEM_GET_SPEC (Vbig5_coding_system, spec);
  attrs = AREF (spec, 0);

  if (ASCII_BYTE_P (ch)
      && ! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    return code;

  val = CODING_ATTR_CHARSET_LIST (attrs);
  charset_roman = CHARSET_FROM_ID (XINT (XCAR (val))), val = XCDR (val);
  charset_big5 = CHARSET_FROM_ID (XINT (XCAR (val)));

  if (ch <= 0x7F)
    {
      c = ch;
      charset = charset_roman;
    }
  else
    {
      EMACS_INT b1 = ch >> 8;
      int b2 = ch & 0x7F;
      if (b1 < 0xA1 || b1 > 0xFE
	  || b2 < 0x40 || (b2 > 0x7E && b2 < 0xA1) || b2 > 0xFE)
	error ("Invalid code: %"pI"d", ch);
      c = ch;
      charset = charset_big5;
    }
  c = DECODE_CHAR (charset, c);
  if (c < 0)
    error ("Invalid code: %"pI"d", ch);
  return make_number (c);
}

DEFUN ("encode-big5-char", Fencode_big5_char, Sencode_big5_char, 1, 1, 0,
       doc: /* Encode the Big5 character CH to BIG5 coding system.
Return the corresponding character code in Big5.  */)
  (Lisp_Object ch)
{
  Lisp_Object spec, attrs, charset_list;
  struct charset *charset;
  int c;
  unsigned code;

  CHECK_CHARACTER (ch);
  c = XFASTINT (ch);
  CHECK_CODING_SYSTEM_GET_SPEC (Vbig5_coding_system, spec);
  attrs = AREF (spec, 0);
  if (ASCII_CHAR_P (c)
      && ! NILP (CODING_ATTR_ASCII_COMPAT (attrs)))
    return ch;

  charset_list = CODING_ATTR_CHARSET_LIST (attrs);
  charset = char_charset (c, charset_list, &code);
  if (code == CHARSET_INVALID_CODE (charset))
    error ("Can't encode by Big5 encoding: %c", c);

  return make_number (code);
}


DEFUN ("set-terminal-coding-system-internal", Fset_terminal_coding_system_internal,
       Sset_terminal_coding_system_internal, 1, 2, 0,
       doc: /* Internal use only.  */)
  (Lisp_Object coding_system, Lisp_Object terminal)
{
  struct terminal *term = get_terminal (terminal, 1);
  struct coding_system *terminal_coding = TERMINAL_TERMINAL_CODING (term);
  CHECK_SYMBOL (coding_system);
  setup_coding_system (Fcheck_coding_system (coding_system), terminal_coding);
  /* We had better not send unsafe characters to terminal.  */
  terminal_coding->mode |= CODING_MODE_SAFE_ENCODING;
  /* Character composition should be disabled.  */
  terminal_coding->common_flags &= ~CODING_ANNOTATE_COMPOSITION_MASK;
  terminal_coding->src_multibyte = 1;
  terminal_coding->dst_multibyte = 0;
  if (terminal_coding->common_flags & CODING_REQUIRE_ENCODING_MASK)
    term->charset_list = coding_charset_list (terminal_coding);
  else
    term->charset_list = Fcons (make_number (charset_ascii), Qnil);
  return Qnil;
}

DEFUN ("set-safe-terminal-coding-system-internal",
       Fset_safe_terminal_coding_system_internal,
       Sset_safe_terminal_coding_system_internal, 1, 1, 0,
       doc: /* Internal use only.  */)
  (Lisp_Object coding_system)
{
  CHECK_SYMBOL (coding_system);
  setup_coding_system (Fcheck_coding_system (coding_system),
		       &safe_terminal_coding);
  /* Character composition should be disabled.  */
  safe_terminal_coding.common_flags &= ~CODING_ANNOTATE_COMPOSITION_MASK;
  safe_terminal_coding.src_multibyte = 1;
  safe_terminal_coding.dst_multibyte = 0;
  return Qnil;
}

DEFUN ("terminal-coding-system", Fterminal_coding_system,
       Sterminal_coding_system, 0, 1, 0,
       doc: /* Return coding system specified for terminal output on the given terminal.
TERMINAL may be a terminal object, a frame, or nil for the selected
frame's terminal device.  */)
  (Lisp_Object terminal)
{
  struct coding_system *terminal_coding
    = TERMINAL_TERMINAL_CODING (get_terminal (terminal, 1));
  Lisp_Object coding_system = CODING_ID_NAME (terminal_coding->id);

  /* For backward compatibility, return nil if it is `undecided'.  */
  return (! EQ (coding_system, Qundecided) ? coding_system : Qnil);
}

DEFUN ("set-keyboard-coding-system-internal", Fset_keyboard_coding_system_internal,
       Sset_keyboard_coding_system_internal, 1, 2, 0,
       doc: /* Internal use only.  */)
  (Lisp_Object coding_system, Lisp_Object terminal)
{
  struct terminal *t = get_terminal (terminal, 1);
  CHECK_SYMBOL (coding_system);
  if (NILP (coding_system))
    coding_system = Qno_conversion;
  else
    Fcheck_coding_system (coding_system);
  setup_coding_system (coding_system, TERMINAL_KEYBOARD_CODING (t));
  /* Character composition should be disabled.  */
  TERMINAL_KEYBOARD_CODING (t)->common_flags
    &= ~CODING_ANNOTATE_COMPOSITION_MASK;
  return Qnil;
}

DEFUN ("keyboard-coding-system",
       Fkeyboard_coding_system, Skeyboard_coding_system, 0, 1, 0,
       doc: /* Return coding system specified for decoding keyboard input.  */)
  (Lisp_Object terminal)
{
  return CODING_ID_NAME (TERMINAL_KEYBOARD_CODING
			 (get_terminal (terminal, 1))->id);
}


DEFUN ("find-operation-coding-system", Ffind_operation_coding_system,
       Sfind_operation_coding_system,  1, MANY, 0,
       doc: /* Choose a coding system for an operation based on the target name.
The value names a pair of coding systems: (DECODING-SYSTEM . ENCODING-SYSTEM).
DECODING-SYSTEM is the coding system to use for decoding
\(in case OPERATION does decoding), and ENCODING-SYSTEM is the coding system
for encoding (in case OPERATION does encoding).

The first argument OPERATION specifies an I/O primitive:
  For file I/O, `insert-file-contents' or `write-region'.
  For process I/O, `call-process', `call-process-region', or `start-process'.
  For network I/O, `open-network-stream'.

The remaining arguments should be the same arguments that were passed
to the primitive.  Depending on which primitive, one of those arguments
is selected as the TARGET.  For example, if OPERATION does file I/O,
whichever argument specifies the file name is TARGET.

TARGET has a meaning which depends on OPERATION:
  For file I/O, TARGET is a file name (except for the special case below).
  For process I/O, TARGET is a process name.
  For network I/O, TARGET is a service name or a port number.

This function looks up what is specified for TARGET in
`file-coding-system-alist', `process-coding-system-alist',
or `network-coding-system-alist' depending on OPERATION.
They may specify a coding system, a cons of coding systems,
or a function symbol to call.
In the last case, we call the function with one argument,
which is a list of all the arguments given to this function.
If the function can't decide a coding system, it can return
`undecided' so that the normal code-detection is performed.

If OPERATION is `insert-file-contents', the argument corresponding to
TARGET may be a cons (FILENAME . BUFFER).  In that case, FILENAME is a
file name to look up, and BUFFER is a buffer that contains the file's
contents (not yet decoded).  If `file-coding-system-alist' specifies a
function to call for FILENAME, that function should examine the
contents of BUFFER instead of reading the file.

usage: (find-operation-coding-system OPERATION ARGUMENTS...)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object operation, target_idx, target, val;
  register Lisp_Object chain;

  if (nargs < 2)
    error ("Too few arguments");
  operation = args[0];
  if (!SYMBOLP (operation)
      || !NATNUMP (target_idx = Fget (operation, Qtarget_idx)))
    error ("Invalid first argument");
  if (nargs < 1 + XFASTINT (target_idx))
    error ("Too few arguments for operation `%s'",
	   SDATA (SYMBOL_NAME (operation)));
  target = args[XFASTINT (target_idx) + 1];
  if (!(STRINGP (target)
	|| (EQ (operation, Qinsert_file_contents) && CONSP (target)
	    && STRINGP (XCAR (target)) && BUFFERP (XCDR (target)))
	|| (EQ (operation, Qopen_network_stream) && INTEGERP (target))))
    error ("Invalid argument %"pI"d of operation `%s'",
	   XFASTINT (target_idx) + 1, SDATA (SYMBOL_NAME (operation)));
  if (CONSP (target))
    target = XCAR (target);

  chain = ((EQ (operation, Qinsert_file_contents)
	    || EQ (operation, Qwrite_region))
	   ? Vfile_coding_system_alist
	   : (EQ (operation, Qopen_network_stream)
	      ? Vnetwork_coding_system_alist
	      : Vprocess_coding_system_alist));
  if (NILP (chain))
    return Qnil;

  for (; CONSP (chain); chain = XCDR (chain))
    {
      Lisp_Object elt;

      elt = XCAR (chain);
      if (CONSP (elt)
	  && ((STRINGP (target)
	       && STRINGP (XCAR (elt))
	       && fast_string_match (XCAR (elt), target) >= 0)
	      || (INTEGERP (target) && EQ (target, XCAR (elt)))))
	{
	  val = XCDR (elt);
	  /* Here, if VAL is both a valid coding system and a valid
             function symbol, we return VAL as a coding system.  */
	  if (CONSP (val))
	    return val;
	  if (! SYMBOLP (val))
	    return Qnil;
	  if (! NILP (Fcoding_system_p (val)))
	    return Fcons (val, val);
	  if (! NILP (Ffboundp (val)))
	    {
	      /* We use call1 rather than safe_call1
		 so as to get bug reports about functions called here
		 which don't handle the current interface.  */
	      val = call1 (val, Flist (nargs, args));
	      if (CONSP (val))
		return val;
	      if (SYMBOLP (val) && ! NILP (Fcoding_system_p (val)))
		return Fcons (val, val);
	    }
	  return Qnil;
	}
    }
  return Qnil;
}

DEFUN ("set-coding-system-priority", Fset_coding_system_priority,
       Sset_coding_system_priority, 0, MANY, 0,
       doc: /* Assign higher priority to the coding systems given as arguments.
If multiple coding systems belong to the same category,
all but the first one are ignored.

usage: (set-coding-system-priority &rest coding-systems)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i, j;
  int changed[coding_category_max];
  enum coding_category priorities[coding_category_max];

  memset (changed, 0, sizeof changed);

  for (i = j = 0; i < nargs; i++)
    {
      enum coding_category category;
      Lisp_Object spec, attrs;

      CHECK_CODING_SYSTEM_GET_SPEC (args[i], spec);
      attrs = AREF (spec, 0);
      category = XINT (CODING_ATTR_CATEGORY (attrs));
      if (changed[category])
	/* Ignore this coding system because a coding system of the
	   same category already had a higher priority.  */
	continue;
      changed[category] = 1;
      priorities[j++] = category;
      if (coding_categories[category].id >= 0
	  && ! EQ (args[i], CODING_ID_NAME (coding_categories[category].id)))
	setup_coding_system (args[i], &coding_categories[category]);
      Fset (AREF (Vcoding_category_table, category), args[i]);
    }

  /* Now we have decided top J priorities.  Reflect the order of the
     original priorities to the remaining priorities.  */

  for (i = j, j = 0; i < coding_category_max; i++, j++)
    {
      while (j < coding_category_max
	     && changed[coding_priorities[j]])
	j++;
      if (j == coding_category_max)
	abort ();
      priorities[i] = coding_priorities[j];
    }

  memcpy (coding_priorities, priorities, sizeof priorities);

  /* Update `coding-category-list'.  */
  Vcoding_category_list = Qnil;
  for (i = coding_category_max; i-- > 0; )
    Vcoding_category_list
      = Fcons (AREF (Vcoding_category_table, priorities[i]),
	       Vcoding_category_list);

  return Qnil;
}

DEFUN ("coding-system-priority-list", Fcoding_system_priority_list,
       Scoding_system_priority_list, 0, 1, 0,
       doc: /* Return a list of coding systems ordered by their priorities.
The list contains a subset of coding systems; i.e. coding systems
assigned to each coding category (see `coding-category-list').

HIGHESTP non-nil means just return the highest priority one.  */)
  (Lisp_Object highestp)
{
  int i;
  Lisp_Object val;

  for (i = 0, val = Qnil; i < coding_category_max; i++)
    {
      enum coding_category category = coding_priorities[i];
      int id = coding_categories[category].id;
      Lisp_Object attrs;

      if (id < 0)
	continue;
      attrs = CODING_ID_ATTRS (id);
      if (! NILP (highestp))
	return CODING_ATTR_BASE_NAME (attrs);
      val = Fcons (CODING_ATTR_BASE_NAME (attrs), val);
    }
  return Fnreverse (val);
}

static const char *const suffixes[] = { "-unix", "-dos", "-mac" };

static Lisp_Object
make_subsidiaries (Lisp_Object base)
{
  Lisp_Object subsidiaries;
  ptrdiff_t base_name_len = SBYTES (SYMBOL_NAME (base));
  char *buf = (char *) alloca (base_name_len + 6);
  int i;

  memcpy (buf, SDATA (SYMBOL_NAME (base)), base_name_len);
  subsidiaries = Fmake_vector (make_number (3), Qnil);
  for (i = 0; i < 3; i++)
    {
      strcpy (buf + base_name_len, suffixes[i]);
      ASET (subsidiaries, i, intern (buf));
    }
  return subsidiaries;
}


DEFUN ("define-coding-system-internal", Fdefine_coding_system_internal,
       Sdefine_coding_system_internal, coding_arg_max, MANY, 0,
       doc: /* For internal use only.
usage: (define-coding-system-internal ...)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object name;
  Lisp_Object spec_vec;		/* [ ATTRS ALIASE EOL_TYPE ] */
  Lisp_Object attrs;		/* Vector of attributes.  */
  Lisp_Object eol_type;
  Lisp_Object aliases;
  Lisp_Object coding_type, charset_list, safe_charsets;
  enum coding_category category;
  Lisp_Object tail, val;
  int max_charset_id = 0;
  int i;

  if (nargs < coding_arg_max)
    goto short_args;

  attrs = Fmake_vector (make_number (coding_attr_last_index), Qnil);

  name = args[coding_arg_name];
  CHECK_SYMBOL (name);
  CODING_ATTR_BASE_NAME (attrs) = name;

  val = args[coding_arg_mnemonic];
  if (! STRINGP (val))
    CHECK_CHARACTER (val);
  CODING_ATTR_MNEMONIC (attrs) = val;

  coding_type = args[coding_arg_coding_type];
  CHECK_SYMBOL (coding_type);
  CODING_ATTR_TYPE (attrs) = coding_type;

  charset_list = args[coding_arg_charset_list];
  if (SYMBOLP (charset_list))
    {
      if (EQ (charset_list, Qiso_2022))
	{
	  if (! EQ (coding_type, Qiso_2022))
	    error ("Invalid charset-list");
	  charset_list = Viso_2022_charset_list;
	}
      else if (EQ (charset_list, Qemacs_mule))
	{
	  if (! EQ (coding_type, Qemacs_mule))
	    error ("Invalid charset-list");
	  charset_list = Vemacs_mule_charset_list;
	}
      for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
	if (max_charset_id < XFASTINT (XCAR (tail)))
	  max_charset_id = XFASTINT (XCAR (tail));
    }
  else
    {
      charset_list = Fcopy_sequence (charset_list);
      for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
	{
	  struct charset *charset;

	  val = XCAR (tail);
	  CHECK_CHARSET_GET_CHARSET (val, charset);
	  if (EQ (coding_type, Qiso_2022)
	      ? CHARSET_ISO_FINAL (charset) < 0
	      : EQ (coding_type, Qemacs_mule)
	      ? CHARSET_EMACS_MULE_ID (charset) < 0
	      : 0)
	    error ("Can't handle charset `%s'",
		   SDATA (SYMBOL_NAME (CHARSET_NAME (charset))));

	  XSETCAR (tail, make_number (charset->id));
	  if (max_charset_id < charset->id)
	    max_charset_id = charset->id;
	}
    }
  CODING_ATTR_CHARSET_LIST (attrs) = charset_list;

  safe_charsets = make_uninit_string (max_charset_id + 1);
  memset (SDATA (safe_charsets), 255, max_charset_id + 1);
  for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
    SSET (safe_charsets, XFASTINT (XCAR (tail)), 0);
  CODING_ATTR_SAFE_CHARSETS (attrs) = safe_charsets;

  CODING_ATTR_ASCII_COMPAT (attrs) = args[coding_arg_ascii_compatible_p];

  val = args[coding_arg_decode_translation_table];
  if (! CHAR_TABLE_P (val) && ! CONSP (val))
    CHECK_SYMBOL (val);
  CODING_ATTR_DECODE_TBL (attrs) = val;

  val = args[coding_arg_encode_translation_table];
  if (! CHAR_TABLE_P (val) && ! CONSP (val))
    CHECK_SYMBOL (val);
  CODING_ATTR_ENCODE_TBL (attrs) = val;

  val = args[coding_arg_post_read_conversion];
  CHECK_SYMBOL (val);
  CODING_ATTR_POST_READ (attrs) = val;

  val = args[coding_arg_pre_write_conversion];
  CHECK_SYMBOL (val);
  CODING_ATTR_PRE_WRITE (attrs) = val;

  val = args[coding_arg_default_char];
  if (NILP (val))
    CODING_ATTR_DEFAULT_CHAR (attrs) = make_number (' ');
  else
    {
      CHECK_CHARACTER (val);
      CODING_ATTR_DEFAULT_CHAR (attrs) = val;
    }

  val = args[coding_arg_for_unibyte];
  CODING_ATTR_FOR_UNIBYTE (attrs) = NILP (val) ? Qnil : Qt;

  val = args[coding_arg_plist];
  CHECK_LIST (val);
  CODING_ATTR_PLIST (attrs) = val;

  if (EQ (coding_type, Qcharset))
    {
      /* Generate a lisp vector of 256 elements.  Each element is nil,
	 integer, or a list of charset IDs.

	 If Nth element is nil, the byte code N is invalid in this
	 coding system.

	 If Nth element is a number NUM, N is the first byte of a
	 charset whose ID is NUM.

	 If Nth element is a list of charset IDs, N is the first byte
	 of one of them.  The list is sorted by dimensions of the
	 charsets.  A charset of smaller dimension comes first. */
      val = Fmake_vector (make_number (256), Qnil);

      for (tail = charset_list; CONSP (tail); tail = XCDR (tail))
	{
	  struct charset *charset = CHARSET_FROM_ID (XFASTINT (XCAR (tail)));
	  int dim = CHARSET_DIMENSION (charset);
	  int idx = (dim - 1) * 4;

	  if (CHARSET_ASCII_COMPATIBLE_P (charset))
	    CODING_ATTR_ASCII_COMPAT (attrs) = Qt;

	  for (i = charset->code_space[idx];
	       i <= charset->code_space[idx + 1]; i++)
	    {
	      Lisp_Object tmp, tmp2;
	      int dim2;

	      tmp = AREF (val, i);
	      if (NILP (tmp))
		tmp = XCAR (tail);
	      else if (NUMBERP (tmp))
		{
		  dim2 = CHARSET_DIMENSION (CHARSET_FROM_ID (XFASTINT (tmp)));
		  if (dim < dim2)
		    tmp = Fcons (XCAR (tail), Fcons (tmp, Qnil));
		  else
		    tmp = Fcons (tmp, Fcons (XCAR (tail), Qnil));
		}
	      else
		{
		  for (tmp2 = tmp; CONSP (tmp2); tmp2 = XCDR (tmp2))
		    {
		      dim2 = CHARSET_DIMENSION (CHARSET_FROM_ID (XFASTINT (XCAR (tmp2))));
		      if (dim < dim2)
			break;
		    }
		  if (NILP (tmp2))
		    tmp = nconc2 (tmp, Fcons (XCAR (tail), Qnil));
		  else
		    {
		      XSETCDR (tmp2, Fcons (XCAR (tmp2), XCDR (tmp2)));
		      XSETCAR (tmp2, XCAR (tail));
		    }
		}
	      ASET (val, i, tmp);
	    }
	}
      ASET (attrs, coding_attr_charset_valids, val);
      category = coding_category_charset;
    }
  else if (EQ (coding_type, Qccl))
    {
      Lisp_Object valids;

      if (nargs < coding_arg_ccl_max)
	goto short_args;

      val = args[coding_arg_ccl_decoder];
      CHECK_CCL_PROGRAM (val);
      if (VECTORP (val))
	val = Fcopy_sequence (val);
      ASET (attrs, coding_attr_ccl_decoder, val);

      val = args[coding_arg_ccl_encoder];
      CHECK_CCL_PROGRAM (val);
      if (VECTORP (val))
	val = Fcopy_sequence (val);
      ASET (attrs, coding_attr_ccl_encoder, val);

      val = args[coding_arg_ccl_valids];
      valids = Fmake_string (make_number (256), make_number (0));
      for (tail = val; !NILP (tail); tail = Fcdr (tail))
	{
	  int from, to;

	  val = Fcar (tail);
	  if (INTEGERP (val))
	    {
	      from = to = XINT (val);
	      if (from < 0 || from > 255)
		args_out_of_range_3 (val, make_number (0), make_number (255));
	    }
	  else
	    {
	      CHECK_CONS (val);
	      CHECK_NATNUM_CAR (val);
	      CHECK_NATNUM_CDR (val);
	      from = XINT (XCAR (val));
	      if (from > 255)
		args_out_of_range_3 (XCAR (val),
				     make_number (0), make_number (255));
	      to = XINT (XCDR (val));
	      if (to < from || to > 255)
		args_out_of_range_3 (XCDR (val),
				     XCAR (val), make_number (255));
	    }
	  for (i = from; i <= to; i++)
	    SSET (valids, i, 1);
	}
      ASET (attrs, coding_attr_ccl_valids, valids);

      category = coding_category_ccl;
    }
  else if (EQ (coding_type, Qutf_16))
    {
      Lisp_Object bom, endian;

      CODING_ATTR_ASCII_COMPAT (attrs) = Qnil;

      if (nargs < coding_arg_utf16_max)
	goto short_args;

      bom = args[coding_arg_utf16_bom];
      if (! NILP (bom) && ! EQ (bom, Qt))
	{
	  CHECK_CONS (bom);
	  val = XCAR (bom);
	  CHECK_CODING_SYSTEM (val);
	  val = XCDR (bom);
	  CHECK_CODING_SYSTEM (val);
	}
      ASET (attrs, coding_attr_utf_bom, bom);

      endian = args[coding_arg_utf16_endian];
      CHECK_SYMBOL (endian);
      if (NILP (endian))
	endian = Qbig;
      else if (! EQ (endian, Qbig) && ! EQ (endian, Qlittle))
	error ("Invalid endian: %s", SDATA (SYMBOL_NAME (endian)));
      ASET (attrs, coding_attr_utf_16_endian, endian);

      category = (CONSP (bom)
		  ? coding_category_utf_16_auto
		  : NILP (bom)
		  ? (EQ (endian, Qbig)
		     ? coding_category_utf_16_be_nosig
		     : coding_category_utf_16_le_nosig)
		  : (EQ (endian, Qbig)
		     ? coding_category_utf_16_be
		     : coding_category_utf_16_le));
    }
  else if (EQ (coding_type, Qiso_2022))
    {
      Lisp_Object initial, reg_usage, request, flags;

      if (nargs < coding_arg_iso2022_max)
	goto short_args;

      initial = Fcopy_sequence (args[coding_arg_iso2022_initial]);
      CHECK_VECTOR (initial);
      for (i = 0; i < 4; i++)
	{
	  val = Faref (initial, make_number (i));
	  if (! NILP (val))
	    {
	      struct charset *charset;

	      CHECK_CHARSET_GET_CHARSET (val, charset);
	      ASET (initial, i, make_number (CHARSET_ID (charset)));
	      if (i == 0 && CHARSET_ASCII_COMPATIBLE_P (charset))
		CODING_ATTR_ASCII_COMPAT (attrs) = Qt;
	    }
	  else
	    ASET (initial, i, make_number (-1));
	}

      reg_usage = args[coding_arg_iso2022_reg_usage];
      CHECK_CONS (reg_usage);
      CHECK_NUMBER_CAR (reg_usage);
      CHECK_NUMBER_CDR (reg_usage);

      request = Fcopy_sequence (args[coding_arg_iso2022_request]);
      for (tail = request; ! NILP (tail); tail = Fcdr (tail))
	{
	  int id;
	  Lisp_Object tmp1;

	  val = Fcar (tail);
	  CHECK_CONS (val);
	  tmp1 = XCAR (val);
	  CHECK_CHARSET_GET_ID (tmp1, id);
	  CHECK_NATNUM_CDR (val);
	  if (XINT (XCDR (val)) >= 4)
	    error ("Invalid graphic register number: %"pI"d", XINT (XCDR (val)));
	  XSETCAR (val, make_number (id));
	}

      flags = args[coding_arg_iso2022_flags];
      CHECK_NATNUM (flags);
      i = XINT (flags);
      if (EQ (args[coding_arg_charset_list], Qiso_2022))
	flags = make_number (i | CODING_ISO_FLAG_FULL_SUPPORT);

      ASET (attrs, coding_attr_iso_initial, initial);
      ASET (attrs, coding_attr_iso_usage, reg_usage);
      ASET (attrs, coding_attr_iso_request, request);
      ASET (attrs, coding_attr_iso_flags, flags);
      setup_iso_safe_charsets (attrs);

      if (i & CODING_ISO_FLAG_SEVEN_BITS)
	category = ((i & (CODING_ISO_FLAG_LOCKING_SHIFT
			  | CODING_ISO_FLAG_SINGLE_SHIFT))
		    ? coding_category_iso_7_else
		    : EQ (args[coding_arg_charset_list], Qiso_2022)
		    ? coding_category_iso_7
		    : coding_category_iso_7_tight);
      else
	{
	  int id = XINT (AREF (initial, 1));

	  category = (((i & CODING_ISO_FLAG_LOCKING_SHIFT)
		       || EQ (args[coding_arg_charset_list], Qiso_2022)
		       || id < 0)
		      ? coding_category_iso_8_else
		      : (CHARSET_DIMENSION (CHARSET_FROM_ID (id)) == 1)
		      ? coding_category_iso_8_1
		      : coding_category_iso_8_2);
	}
      if (category != coding_category_iso_8_1
	  && category != coding_category_iso_8_2)
	CODING_ATTR_ASCII_COMPAT (attrs) = Qnil;
    }
  else if (EQ (coding_type, Qemacs_mule))
    {
      if (EQ (args[coding_arg_charset_list], Qemacs_mule))
	ASET (attrs, coding_attr_emacs_mule_full, Qt);
      CODING_ATTR_ASCII_COMPAT (attrs) = Qt;
      category = coding_category_emacs_mule;
    }
  else if (EQ (coding_type, Qshift_jis))
    {

      struct charset *charset;

      if (XINT (Flength (charset_list)) != 3
	  && XINT (Flength (charset_list)) != 4)
	error ("There should be three or four charsets");

      charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      if (CHARSET_DIMENSION (charset) != 1)
	error ("Dimension of charset %s is not one",
	       SDATA (SYMBOL_NAME (CHARSET_NAME (charset))));
      if (CHARSET_ASCII_COMPATIBLE_P (charset))
	CODING_ATTR_ASCII_COMPAT (attrs) = Qt;

      charset_list = XCDR (charset_list);
      charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      if (CHARSET_DIMENSION (charset) != 1)
	error ("Dimension of charset %s is not one",
	       SDATA (SYMBOL_NAME (CHARSET_NAME (charset))));

      charset_list = XCDR (charset_list);
      charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      if (CHARSET_DIMENSION (charset) != 2)
	error ("Dimension of charset %s is not two",
	       SDATA (SYMBOL_NAME (CHARSET_NAME (charset))));

      charset_list = XCDR (charset_list);
      if (! NILP (charset_list))
	{
	  charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
	  if (CHARSET_DIMENSION (charset) != 2)
	    error ("Dimension of charset %s is not two",
		   SDATA (SYMBOL_NAME (CHARSET_NAME (charset))));
	}

      category = coding_category_sjis;
      Vsjis_coding_system = name;
    }
  else if (EQ (coding_type, Qbig5))
    {
      struct charset *charset;

      if (XINT (Flength (charset_list)) != 2)
	error ("There should be just two charsets");

      charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      if (CHARSET_DIMENSION (charset) != 1)
	error ("Dimension of charset %s is not one",
	       SDATA (SYMBOL_NAME (CHARSET_NAME (charset))));
      if (CHARSET_ASCII_COMPATIBLE_P (charset))
	CODING_ATTR_ASCII_COMPAT (attrs) = Qt;

      charset_list = XCDR (charset_list);
      charset = CHARSET_FROM_ID (XINT (XCAR (charset_list)));
      if (CHARSET_DIMENSION (charset) != 2)
	error ("Dimension of charset %s is not two",
	       SDATA (SYMBOL_NAME (CHARSET_NAME (charset))));

      category = coding_category_big5;
      Vbig5_coding_system = name;
    }
  else if (EQ (coding_type, Qraw_text))
    {
      category = coding_category_raw_text;
      CODING_ATTR_ASCII_COMPAT (attrs) = Qt;
    }
  else if (EQ (coding_type, Qutf_8))
    {
      Lisp_Object bom;

      if (nargs < coding_arg_utf8_max)
	goto short_args;

      bom = args[coding_arg_utf8_bom];
      if (! NILP (bom) && ! EQ (bom, Qt))
	{
	  CHECK_CONS (bom);
	  val = XCAR (bom);
	  CHECK_CODING_SYSTEM (val);
	  val = XCDR (bom);
	  CHECK_CODING_SYSTEM (val);
	}
      ASET (attrs, coding_attr_utf_bom, bom);
      if (NILP (bom))
	CODING_ATTR_ASCII_COMPAT (attrs) = Qt;

      category = (CONSP (bom) ? coding_category_utf_8_auto
		  : NILP (bom) ? coding_category_utf_8_nosig
		  : coding_category_utf_8_sig);
    }
  else if (EQ (coding_type, Qundecided))
    category = coding_category_undecided;
  else
    error ("Invalid coding system type: %s",
	   SDATA (SYMBOL_NAME (coding_type)));

  CODING_ATTR_CATEGORY (attrs) = make_number (category);
  CODING_ATTR_PLIST (attrs)
    = Fcons (QCcategory, Fcons (AREF (Vcoding_category_table, category),
				CODING_ATTR_PLIST (attrs)));
  CODING_ATTR_PLIST (attrs)
    = Fcons (QCascii_compatible_p,
	     Fcons (CODING_ATTR_ASCII_COMPAT (attrs),
		    CODING_ATTR_PLIST (attrs)));

  eol_type = args[coding_arg_eol_type];
  if (! NILP (eol_type)
      && ! EQ (eol_type, Qunix)
      && ! EQ (eol_type, Qdos)
      && ! EQ (eol_type, Qmac))
    error ("Invalid eol-type");

  aliases = Fcons (name, Qnil);

  if (NILP (eol_type))
    {
      eol_type = make_subsidiaries (name);
      for (i = 0; i < 3; i++)
	{
	  Lisp_Object this_spec, this_name, this_aliases, this_eol_type;

	  this_name = AREF (eol_type, i);
	  this_aliases = Fcons (this_name, Qnil);
	  this_eol_type = (i == 0 ? Qunix : i == 1 ? Qdos : Qmac);
	  this_spec = Fmake_vector (make_number (3), attrs);
	  ASET (this_spec, 1, this_aliases);
	  ASET (this_spec, 2, this_eol_type);
	  Fputhash (this_name, this_spec, Vcoding_system_hash_table);
	  Vcoding_system_list = Fcons (this_name, Vcoding_system_list);
	  val = Fassoc (Fsymbol_name (this_name), Vcoding_system_alist);
	  if (NILP (val))
	    Vcoding_system_alist
	      = Fcons (Fcons (Fsymbol_name (this_name), Qnil),
		       Vcoding_system_alist);
	}
    }

  spec_vec = Fmake_vector (make_number (3), attrs);
  ASET (spec_vec, 1, aliases);
  ASET (spec_vec, 2, eol_type);

  Fputhash (name, spec_vec, Vcoding_system_hash_table);
  Vcoding_system_list = Fcons (name, Vcoding_system_list);
  val = Fassoc (Fsymbol_name (name), Vcoding_system_alist);
  if (NILP (val))
    Vcoding_system_alist = Fcons (Fcons (Fsymbol_name (name), Qnil),
				  Vcoding_system_alist);

  {
    int id = coding_categories[category].id;

    if (id < 0 || EQ (name, CODING_ID_NAME (id)))
      setup_coding_system (name, &coding_categories[category]);
  }

  return Qnil;

 short_args:
  return Fsignal (Qwrong_number_of_arguments,
		  Fcons (intern ("define-coding-system-internal"),
			 make_number (nargs)));
}


DEFUN ("coding-system-put", Fcoding_system_put, Scoding_system_put,
       3, 3, 0,
       doc: /* Change value in CODING-SYSTEM's property list PROP to VAL.  */)
  (Lisp_Object coding_system, Lisp_Object prop, Lisp_Object val)
{
  Lisp_Object spec, attrs;

  CHECK_CODING_SYSTEM_GET_SPEC (coding_system, spec);
  attrs = AREF (spec, 0);
  if (EQ (prop, QCmnemonic))
    {
      if (! STRINGP (val))
	CHECK_CHARACTER (val);
      CODING_ATTR_MNEMONIC (attrs) = val;
    }
  else if (EQ (prop, QCdefault_char))
    {
      if (NILP (val))
	val = make_number (' ');
      else
	CHECK_CHARACTER (val);
      CODING_ATTR_DEFAULT_CHAR (attrs) = val;
    }
  else if (EQ (prop, QCdecode_translation_table))
    {
      if (! CHAR_TABLE_P (val) && ! CONSP (val))
	CHECK_SYMBOL (val);
      CODING_ATTR_DECODE_TBL (attrs) = val;
    }
  else if (EQ (prop, QCencode_translation_table))
    {
      if (! CHAR_TABLE_P (val) && ! CONSP (val))
	CHECK_SYMBOL (val);
      CODING_ATTR_ENCODE_TBL (attrs) = val;
    }
  else if (EQ (prop, QCpost_read_conversion))
    {
      CHECK_SYMBOL (val);
      CODING_ATTR_POST_READ (attrs) = val;
    }
  else if (EQ (prop, QCpre_write_conversion))
    {
      CHECK_SYMBOL (val);
      CODING_ATTR_PRE_WRITE (attrs) = val;
    }
  else if (EQ (prop, QCascii_compatible_p))
    {
      CODING_ATTR_ASCII_COMPAT (attrs) = val;
    }

  CODING_ATTR_PLIST (attrs)
    = Fplist_put (CODING_ATTR_PLIST (attrs), prop, val);
  return val;
}


DEFUN ("define-coding-system-alias", Fdefine_coding_system_alias,
       Sdefine_coding_system_alias, 2, 2, 0,
       doc: /* Define ALIAS as an alias for CODING-SYSTEM.  */)
  (Lisp_Object alias, Lisp_Object coding_system)
{
  Lisp_Object spec, aliases, eol_type, val;

  CHECK_SYMBOL (alias);
  CHECK_CODING_SYSTEM_GET_SPEC (coding_system, spec);
  aliases = AREF (spec, 1);
  /* ALIASES should be a list of length more than zero, and the first
     element is a base coding system.  Append ALIAS at the tail of the
     list.  */
  while (!NILP (XCDR (aliases)))
    aliases = XCDR (aliases);
  XSETCDR (aliases, Fcons (alias, Qnil));

  eol_type = AREF (spec, 2);
  if (VECTORP (eol_type))
    {
      Lisp_Object subsidiaries;
      int i;

      subsidiaries = make_subsidiaries (alias);
      for (i = 0; i < 3; i++)
	Fdefine_coding_system_alias (AREF (subsidiaries, i),
				     AREF (eol_type, i));
    }

  Fputhash (alias, spec, Vcoding_system_hash_table);
  Vcoding_system_list = Fcons (alias, Vcoding_system_list);
  val = Fassoc (Fsymbol_name (alias), Vcoding_system_alist);
  if (NILP (val))
    Vcoding_system_alist = Fcons (Fcons (Fsymbol_name (alias), Qnil),
				  Vcoding_system_alist);

  return Qnil;
}

DEFUN ("coding-system-base", Fcoding_system_base, Scoding_system_base,
       1, 1, 0,
       doc: /* Return the base of CODING-SYSTEM.
Any alias or subsidiary coding system is not a base coding system.  */)
  (Lisp_Object coding_system)
{
  Lisp_Object spec, attrs;

  if (NILP (coding_system))
    return (Qno_conversion);
  CHECK_CODING_SYSTEM_GET_SPEC (coding_system, spec);
  attrs = AREF (spec, 0);
  return CODING_ATTR_BASE_NAME (attrs);
}

DEFUN ("coding-system-plist", Fcoding_system_plist, Scoding_system_plist,
       1, 1, 0,
       doc: "Return the property list of CODING-SYSTEM.")
  (Lisp_Object coding_system)
{
  Lisp_Object spec, attrs;

  if (NILP (coding_system))
    coding_system = Qno_conversion;
  CHECK_CODING_SYSTEM_GET_SPEC (coding_system, spec);
  attrs = AREF (spec, 0);
  return CODING_ATTR_PLIST (attrs);
}


DEFUN ("coding-system-aliases", Fcoding_system_aliases, Scoding_system_aliases,
       1, 1, 0,
       doc: /* Return the list of aliases of CODING-SYSTEM.  */)
  (Lisp_Object coding_system)
{
  Lisp_Object spec;

  if (NILP (coding_system))
    coding_system = Qno_conversion;
  CHECK_CODING_SYSTEM_GET_SPEC (coding_system, spec);
  return AREF (spec, 1);
}

DEFUN ("coding-system-eol-type", Fcoding_system_eol_type,
       Scoding_system_eol_type, 1, 1, 0,
       doc: /* Return eol-type of CODING-SYSTEM.
An eol-type is an integer 0, 1, 2, or a vector of coding systems.

Integer values 0, 1, and 2 indicate a format of end-of-line; LF, CRLF,
and CR respectively.

A vector value indicates that a format of end-of-line should be
detected automatically.  Nth element of the vector is the subsidiary
coding system whose eol-type is N.  */)
  (Lisp_Object coding_system)
{
  Lisp_Object spec, eol_type;
  int n;

  if (NILP (coding_system))
    coding_system = Qno_conversion;
  if (! CODING_SYSTEM_P (coding_system))
    return Qnil;
  spec = CODING_SYSTEM_SPEC (coding_system);
  eol_type = AREF (spec, 2);
  if (VECTORP (eol_type))
    return Fcopy_sequence (eol_type);
  n = EQ (eol_type, Qunix) ? 0 : EQ (eol_type, Qdos) ? 1 : 2;
  return make_number (n);
}

#endif /* emacs */


/*** 9. Post-amble ***/

void
init_coding_once (void)
{
  int i;

  for (i = 0; i < coding_category_max; i++)
    {
      coding_categories[i].id = -1;
      coding_priorities[i] = i;
    }

  /* ISO2022 specific initialize routine.  */
  for (i = 0; i < 0x20; i++)
    iso_code_class[i] = ISO_control_0;
  for (i = 0x21; i < 0x7F; i++)
    iso_code_class[i] = ISO_graphic_plane_0;
  for (i = 0x80; i < 0xA0; i++)
    iso_code_class[i] = ISO_control_1;
  for (i = 0xA1; i < 0xFF; i++)
    iso_code_class[i] = ISO_graphic_plane_1;
  iso_code_class[0x20] = iso_code_class[0x7F] = ISO_0x20_or_0x7F;
  iso_code_class[0xA0] = iso_code_class[0xFF] = ISO_0xA0_or_0xFF;
  iso_code_class[ISO_CODE_SO] = ISO_shift_out;
  iso_code_class[ISO_CODE_SI] = ISO_shift_in;
  iso_code_class[ISO_CODE_SS2_7] = ISO_single_shift_2_7;
  iso_code_class[ISO_CODE_ESC] = ISO_escape;
  iso_code_class[ISO_CODE_SS2] = ISO_single_shift_2;
  iso_code_class[ISO_CODE_SS3] = ISO_single_shift_3;
  iso_code_class[ISO_CODE_CSI] = ISO_control_sequence_introducer;

  for (i = 0; i < 256; i++)
    {
      emacs_mule_bytes[i] = 1;
    }
  emacs_mule_bytes[EMACS_MULE_LEADING_CODE_PRIVATE_11] = 3;
  emacs_mule_bytes[EMACS_MULE_LEADING_CODE_PRIVATE_12] = 3;
  emacs_mule_bytes[EMACS_MULE_LEADING_CODE_PRIVATE_21] = 4;
  emacs_mule_bytes[EMACS_MULE_LEADING_CODE_PRIVATE_22] = 4;
}

#ifdef emacs

void
syms_of_coding (void)
{
  staticpro (&Vcoding_system_hash_table);
  {
    Lisp_Object args[2];
    args[0] = QCtest;
    args[1] = Qeq;
    Vcoding_system_hash_table = Fmake_hash_table (2, args);
  }

  staticpro (&Vsjis_coding_system);
  Vsjis_coding_system = Qnil;

  staticpro (&Vbig5_coding_system);
  Vbig5_coding_system = Qnil;

  staticpro (&Vcode_conversion_reused_workbuf);
  Vcode_conversion_reused_workbuf = Qnil;

  staticpro (&Vcode_conversion_workbuf_name);
  Vcode_conversion_workbuf_name = make_pure_c_string (" *code-conversion-work*");

  reused_workbuf_in_use = 0;

  DEFSYM (Qcharset, "charset");
  DEFSYM (Qtarget_idx, "target-idx");
  DEFSYM (Qcoding_system_history, "coding-system-history");
  Fset (Qcoding_system_history, Qnil);

  /* Target FILENAME is the first argument.  */
  Fput (Qinsert_file_contents, Qtarget_idx, make_number (0));
  /* Target FILENAME is the third argument.  */
  Fput (Qwrite_region, Qtarget_idx, make_number (2));

  DEFSYM (Qcall_process, "call-process");
  /* Target PROGRAM is the first argument.  */
  Fput (Qcall_process, Qtarget_idx, make_number (0));

  DEFSYM (Qcall_process_region, "call-process-region");
  /* Target PROGRAM is the third argument.  */
  Fput (Qcall_process_region, Qtarget_idx, make_number (2));

  DEFSYM (Qstart_process, "start-process");
  /* Target PROGRAM is the third argument.  */
  Fput (Qstart_process, Qtarget_idx, make_number (2));

  DEFSYM (Qopen_network_stream, "open-network-stream");
  /* Target SERVICE is the fourth argument.  */
  Fput (Qopen_network_stream, Qtarget_idx, make_number (3));

  DEFSYM (Qcoding_system, "coding-system");
  DEFSYM (Qcoding_aliases, "coding-aliases");

  DEFSYM (Qeol_type, "eol-type");
  DEFSYM (Qunix, "unix");
  DEFSYM (Qdos, "dos");

  DEFSYM (Qbuffer_file_coding_system, "buffer-file-coding-system");
  DEFSYM (Qpost_read_conversion, "post-read-conversion");
  DEFSYM (Qpre_write_conversion, "pre-write-conversion");
  DEFSYM (Qdefault_char, "default-char");
  DEFSYM (Qundecided, "undecided");
  DEFSYM (Qno_conversion, "no-conversion");
  DEFSYM (Qraw_text, "raw-text");

  DEFSYM (Qiso_2022, "iso-2022");

  DEFSYM (Qutf_8, "utf-8");
  DEFSYM (Qutf_8_emacs, "utf-8-emacs");

  DEFSYM (Qutf_16, "utf-16");
  DEFSYM (Qbig, "big");
  DEFSYM (Qlittle, "little");

  DEFSYM (Qshift_jis, "shift-jis");
  DEFSYM (Qbig5, "big5");

  DEFSYM (Qcoding_system_p, "coding-system-p");

  DEFSYM (Qcoding_system_error, "coding-system-error");
  Fput (Qcoding_system_error, Qerror_conditions,
	pure_cons (Qcoding_system_error, pure_cons (Qerror, Qnil)));
  Fput (Qcoding_system_error, Qerror_message,
	make_pure_c_string ("Invalid coding system"));

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern_c_string ("char-table-extra-slots");

  DEFSYM (Qtranslation_table, "translation-table");
  Fput (Qtranslation_table, Qchar_table_extra_slots, make_number (2));
  DEFSYM (Qtranslation_table_id, "translation-table-id");
  DEFSYM (Qtranslation_table_for_decode, "translation-table-for-decode");
  DEFSYM (Qtranslation_table_for_encode, "translation-table-for-encode");

  DEFSYM (Qvalid_codes, "valid-codes");

  DEFSYM (Qemacs_mule, "emacs-mule");

  DEFSYM (QCcategory, ":category");
  DEFSYM (QCmnemonic, ":mnemonic");
  DEFSYM (QCdefault_char, ":default-char");
  DEFSYM (QCdecode_translation_table, ":decode-translation-table");
  DEFSYM (QCencode_translation_table, ":encode-translation-table");
  DEFSYM (QCpost_read_conversion, ":post-read-conversion");
  DEFSYM (QCpre_write_conversion, ":pre-write-conversion");
  DEFSYM (QCascii_compatible_p, ":ascii-compatible-p");

  Vcoding_category_table
    = Fmake_vector (make_number (coding_category_max), Qnil);
  staticpro (&Vcoding_category_table);
  /* Followings are target of code detection.  */
  ASET (Vcoding_category_table, coding_category_iso_7,
	intern_c_string ("coding-category-iso-7"));
  ASET (Vcoding_category_table, coding_category_iso_7_tight,
	intern_c_string ("coding-category-iso-7-tight"));
  ASET (Vcoding_category_table, coding_category_iso_8_1,
	intern_c_string ("coding-category-iso-8-1"));
  ASET (Vcoding_category_table, coding_category_iso_8_2,
	intern_c_string ("coding-category-iso-8-2"));
  ASET (Vcoding_category_table, coding_category_iso_7_else,
	intern_c_string ("coding-category-iso-7-else"));
  ASET (Vcoding_category_table, coding_category_iso_8_else,
	intern_c_string ("coding-category-iso-8-else"));
  ASET (Vcoding_category_table, coding_category_utf_8_auto,
	intern_c_string ("coding-category-utf-8-auto"));
  ASET (Vcoding_category_table, coding_category_utf_8_nosig,
	intern_c_string ("coding-category-utf-8"));
  ASET (Vcoding_category_table, coding_category_utf_8_sig,
	intern_c_string ("coding-category-utf-8-sig"));
  ASET (Vcoding_category_table, coding_category_utf_16_be,
	intern_c_string ("coding-category-utf-16-be"));
  ASET (Vcoding_category_table, coding_category_utf_16_auto,
	intern_c_string ("coding-category-utf-16-auto"));
  ASET (Vcoding_category_table, coding_category_utf_16_le,
	intern_c_string ("coding-category-utf-16-le"));
  ASET (Vcoding_category_table, coding_category_utf_16_be_nosig,
	intern_c_string ("coding-category-utf-16-be-nosig"));
  ASET (Vcoding_category_table, coding_category_utf_16_le_nosig,
	intern_c_string ("coding-category-utf-16-le-nosig"));
  ASET (Vcoding_category_table, coding_category_charset,
	intern_c_string ("coding-category-charset"));
  ASET (Vcoding_category_table, coding_category_sjis,
	intern_c_string ("coding-category-sjis"));
  ASET (Vcoding_category_table, coding_category_big5,
	intern_c_string ("coding-category-big5"));
  ASET (Vcoding_category_table, coding_category_ccl,
	intern_c_string ("coding-category-ccl"));
  ASET (Vcoding_category_table, coding_category_emacs_mule,
	intern_c_string ("coding-category-emacs-mule"));
  /* Followings are NOT target of code detection.  */
  ASET (Vcoding_category_table, coding_category_raw_text,
	intern_c_string ("coding-category-raw-text"));
  ASET (Vcoding_category_table, coding_category_undecided,
	intern_c_string ("coding-category-undecided"));

  DEFSYM (Qinsufficient_source, "insufficient-source");
  DEFSYM (Qinconsistent_eol, "inconsistent-eol");
  DEFSYM (Qinvalid_source, "invalid-source");
  DEFSYM (Qinterrupted, "interrupted");
  DEFSYM (Qinsufficient_memory, "insufficient-memory");
  DEFSYM (Qcoding_system_define_form, "coding-system-define-form");

  defsubr (&Scoding_system_p);
  defsubr (&Sread_coding_system);
  defsubr (&Sread_non_nil_coding_system);
  defsubr (&Scheck_coding_system);
  defsubr (&Sdetect_coding_region);
  defsubr (&Sdetect_coding_string);
  defsubr (&Sfind_coding_systems_region_internal);
  defsubr (&Sunencodable_char_position);
  defsubr (&Scheck_coding_systems_region);
  defsubr (&Sdecode_coding_region);
  defsubr (&Sencode_coding_region);
  defsubr (&Sdecode_coding_string);
  defsubr (&Sencode_coding_string);
  defsubr (&Sdecode_sjis_char);
  defsubr (&Sencode_sjis_char);
  defsubr (&Sdecode_big5_char);
  defsubr (&Sencode_big5_char);
  defsubr (&Sset_terminal_coding_system_internal);
  defsubr (&Sset_safe_terminal_coding_system_internal);
  defsubr (&Sterminal_coding_system);
  defsubr (&Sset_keyboard_coding_system_internal);
  defsubr (&Skeyboard_coding_system);
  defsubr (&Sfind_operation_coding_system);
  defsubr (&Sset_coding_system_priority);
  defsubr (&Sdefine_coding_system_internal);
  defsubr (&Sdefine_coding_system_alias);
  defsubr (&Scoding_system_put);
  defsubr (&Scoding_system_base);
  defsubr (&Scoding_system_plist);
  defsubr (&Scoding_system_aliases);
  defsubr (&Scoding_system_eol_type);
  defsubr (&Scoding_system_priority_list);

  DEFVAR_LISP ("coding-system-list", Vcoding_system_list,
	       doc: /* List of coding systems.

Do not alter the value of this variable manually.  This variable should be
updated by the functions `define-coding-system' and
`define-coding-system-alias'.  */);
  Vcoding_system_list = Qnil;

  DEFVAR_LISP ("coding-system-alist", Vcoding_system_alist,
	       doc: /* Alist of coding system names.
Each element is one element list of coding system name.
This variable is given to `completing-read' as COLLECTION argument.

Do not alter the value of this variable manually.  This variable should be
updated by the functions `make-coding-system' and
`define-coding-system-alias'.  */);
  Vcoding_system_alist = Qnil;

  DEFVAR_LISP ("coding-category-list", Vcoding_category_list,
	       doc: /* List of coding-categories (symbols) ordered by priority.

On detecting a coding system, Emacs tries code detection algorithms
associated with each coding-category one by one in this order.  When
one algorithm agrees with a byte sequence of source text, the coding
system bound to the corresponding coding-category is selected.

Don't modify this variable directly, but use `set-coding-system-priority'.  */);
  {
    int i;

    Vcoding_category_list = Qnil;
    for (i = coding_category_max - 1; i >= 0; i--)
      Vcoding_category_list
	= Fcons (XVECTOR (Vcoding_category_table)->contents[i],
		 Vcoding_category_list);
  }

  DEFVAR_LISP ("coding-system-for-read", Vcoding_system_for_read,
	       doc: /* Specify the coding system for read operations.
It is useful to bind this variable with `let', but do not set it globally.
If the value is a coding system, it is used for decoding on read operation.
If not, an appropriate element is used from one of the coding system alists.
There are three such tables: `file-coding-system-alist',
`process-coding-system-alist', and `network-coding-system-alist'.  */);
  Vcoding_system_for_read = Qnil;

  DEFVAR_LISP ("coding-system-for-write", Vcoding_system_for_write,
	       doc: /* Specify the coding system for write operations.
Programs bind this variable with `let', but you should not set it globally.
If the value is a coding system, it is used for encoding of output,
when writing it to a file and when sending it to a file or subprocess.

If this does not specify a coding system, an appropriate element
is used from one of the coding system alists.
There are three such tables: `file-coding-system-alist',
`process-coding-system-alist', and `network-coding-system-alist'.
For output to files, if the above procedure does not specify a coding system,
the value of `buffer-file-coding-system' is used.  */);
  Vcoding_system_for_write = Qnil;

  DEFVAR_LISP ("last-coding-system-used", Vlast_coding_system_used,
	       doc: /*
Coding system used in the latest file or process I/O.  */);
  Vlast_coding_system_used = Qnil;

  DEFVAR_LISP ("last-code-conversion-error", Vlast_code_conversion_error,
	       doc: /*
Error status of the last code conversion.

When an error was detected in the last code conversion, this variable
is set to one of the following symbols.
  `insufficient-source'
  `inconsistent-eol'
  `invalid-source'
  `interrupted'
  `insufficient-memory'
When no error was detected, the value doesn't change.  So, to check
the error status of a code conversion by this variable, you must
explicitly set this variable to nil before performing code
conversion.  */);
  Vlast_code_conversion_error = Qnil;

  DEFVAR_BOOL ("inhibit-eol-conversion", inhibit_eol_conversion,
	       doc: /*
*Non-nil means always inhibit code conversion of end-of-line format.
See info node `Coding Systems' and info node `Text and Binary' concerning
such conversion.  */);
  inhibit_eol_conversion = 0;

  DEFVAR_BOOL ("inherit-process-coding-system", inherit_process_coding_system,
	       doc: /*
Non-nil means process buffer inherits coding system of process output.
Bind it to t if the process output is to be treated as if it were a file
read from some filesystem.  */);
  inherit_process_coding_system = 0;

  DEFVAR_LISP ("file-coding-system-alist", Vfile_coding_system_alist,
	       doc: /*
Alist to decide a coding system to use for a file I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a file name,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding and encoding
the file contents.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, the function must return a coding system
or a cons of coding systems which are used as above.  The function is
called with an argument that is a list of the arguments with which
`find-operation-coding-system' was called.  If the function can't decide
a coding system, it can return `undecided' so that the normal
code-detection is performed.

See also the function `find-operation-coding-system'
and the variable `auto-coding-alist'.  */);
  Vfile_coding_system_alist = Qnil;

  DEFVAR_LISP ("process-coding-system-alist", Vprocess_coding_system_alist,
	       doc: /*
Alist to decide a coding system to use for a process I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a program name,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding what received
from the program and encoding what sent to the program.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, the function must return a coding system
or a cons of coding systems which are used as above.

See also the function `find-operation-coding-system'.  */);
  Vprocess_coding_system_alist = Qnil;

  DEFVAR_LISP ("network-coding-system-alist", Vnetwork_coding_system_alist,
	       doc: /*
Alist to decide a coding system to use for a network I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a network service name
or is a port number to connect to,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding what received
from the network stream and encoding what sent to the network stream.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, the function must return a coding system
or a cons of coding systems which are used as above.

See also the function `find-operation-coding-system'.  */);
  Vnetwork_coding_system_alist = Qnil;

  DEFVAR_LISP ("locale-coding-system", Vlocale_coding_system,
	       doc: /* Coding system to use with system messages.
Also used for decoding keyboard input on X Window system.  */);
  Vlocale_coding_system = Qnil;

  /* The eol mnemonics are reset in startup.el system-dependently.  */
  DEFVAR_LISP ("eol-mnemonic-unix", eol_mnemonic_unix,
	       doc: /*
*String displayed in mode line for UNIX-like (LF) end-of-line format.  */);
  eol_mnemonic_unix = make_pure_c_string (":");

  DEFVAR_LISP ("eol-mnemonic-dos", eol_mnemonic_dos,
	       doc: /*
*String displayed in mode line for DOS-like (CRLF) end-of-line format.  */);
  eol_mnemonic_dos = make_pure_c_string ("\\");

  DEFVAR_LISP ("eol-mnemonic-mac", eol_mnemonic_mac,
	       doc: /*
*String displayed in mode line for MAC-like (CR) end-of-line format.  */);
  eol_mnemonic_mac = make_pure_c_string ("/");

  DEFVAR_LISP ("eol-mnemonic-undecided", eol_mnemonic_undecided,
	       doc: /*
*String displayed in mode line when end-of-line format is not yet determined.  */);
  eol_mnemonic_undecided = make_pure_c_string (":");

  DEFVAR_LISP ("enable-character-translation", Venable_character_translation,
	       doc: /*
*Non-nil enables character translation while encoding and decoding.  */);
  Venable_character_translation = Qt;

  DEFVAR_LISP ("standard-translation-table-for-decode",
	       Vstandard_translation_table_for_decode,
	       doc: /* Table for translating characters while decoding.  */);
  Vstandard_translation_table_for_decode = Qnil;

  DEFVAR_LISP ("standard-translation-table-for-encode",
	       Vstandard_translation_table_for_encode,
	       doc: /* Table for translating characters while encoding.  */);
  Vstandard_translation_table_for_encode = Qnil;

  DEFVAR_LISP ("charset-revision-table", Vcharset_revision_table,
	       doc: /* Alist of charsets vs revision numbers.
While encoding, if a charset (car part of an element) is found,
designate it with the escape sequence identifying revision (cdr part
of the element).  */);
  Vcharset_revision_table = Qnil;

  DEFVAR_LISP ("default-process-coding-system",
	       Vdefault_process_coding_system,
	       doc: /* Cons of coding systems used for process I/O by default.
The car part is used for decoding a process output,
the cdr part is used for encoding a text to be sent to a process.  */);
  Vdefault_process_coding_system = Qnil;

  DEFVAR_LISP ("latin-extra-code-table", Vlatin_extra_code_table,
	       doc: /*
Table of extra Latin codes in the range 128..159 (inclusive).
This is a vector of length 256.
If Nth element is non-nil, the existence of code N in a file
\(or output of subprocess) doesn't prevent it to be detected as
a coding system of ISO 2022 variant which has a flag
`accept-latin-extra-code' t (e.g. iso-latin-1) on reading a file
or reading output of a subprocess.
Only 128th through 159th elements have a meaning.  */);
  Vlatin_extra_code_table = Fmake_vector (make_number (256), Qnil);

  DEFVAR_LISP ("select-safe-coding-system-function",
	       Vselect_safe_coding_system_function,
	       doc: /*
Function to call to select safe coding system for encoding a text.

If set, this function is called to force a user to select a proper
coding system which can encode the text in the case that a default
coding system used in each operation can't encode the text.  The
function should take care that the buffer is not modified while
the coding system is being selected.

The default value is `select-safe-coding-system' (which see).  */);
  Vselect_safe_coding_system_function = Qnil;

  DEFVAR_BOOL ("coding-system-require-warning",
	       coding_system_require_warning,
	       doc: /* Internal use only.
If non-nil, on writing a file, `select-safe-coding-system-function' is
called even if `coding-system-for-write' is non-nil.  The command
`universal-coding-system-argument' binds this variable to t temporarily.  */);
  coding_system_require_warning = 0;


  DEFVAR_BOOL ("inhibit-iso-escape-detection",
	       inhibit_iso_escape_detection,
	       doc: /*
If non-nil, Emacs ignores ISO-2022 escape sequences during code detection.

When Emacs reads text, it tries to detect how the text is encoded.
This code detection is sensitive to escape sequences.  If Emacs sees
a valid ISO-2022 escape sequence, it assumes the text is encoded in one
of the ISO2022 encodings, and decodes text by the corresponding coding
system (e.g. `iso-2022-7bit').

However, there may be a case that you want to read escape sequences in
a file as is.  In such a case, you can set this variable to non-nil.
Then the code detection will ignore any escape sequences, and no text is
detected as encoded in some ISO-2022 encoding.  The result is that all
escape sequences become visible in a buffer.

The default value is nil, and it is strongly recommended not to change
it.  That is because many Emacs Lisp source files that contain
non-ASCII characters are encoded by the coding system `iso-2022-7bit'
in Emacs's distribution, and they won't be decoded correctly on
reading if you suppress escape sequence detection.

The other way to read escape sequences in a file without decoding is
to explicitly specify some coding system that doesn't use ISO-2022
escape sequence (e.g `latin-1') on reading by \\[universal-coding-system-argument].  */);
  inhibit_iso_escape_detection = 0;

  DEFVAR_BOOL ("inhibit-null-byte-detection",
	       inhibit_null_byte_detection,
	       doc: /* If non-nil, Emacs ignores null bytes on code detection.
By default, Emacs treats it as binary data, and does not attempt to
decode it.  The effect is as if you specified `no-conversion' for
reading that text.

Set this to non-nil when a regular text happens to include null bytes.
Examples are Index nodes of Info files and null-byte delimited output
from GNU Find and GNU Grep.  Emacs will then ignore the null bytes and
decode text as usual.  */);
  inhibit_null_byte_detection = 0;

  DEFVAR_LISP ("translation-table-for-input", Vtranslation_table_for_input,
	       doc: /* Char table for translating self-inserting characters.
This is applied to the result of input methods, not their input.
See also `keyboard-translate-table'.

Use of this variable for character code unification was rendered
obsolete in Emacs 23.1 and later, since Unicode is now the basis of
internal character representation.  */);
    Vtranslation_table_for_input = Qnil;

  {
    Lisp_Object args[coding_arg_max];
    Lisp_Object plist[16];
    int i;

    for (i = 0; i < coding_arg_max; i++)
      args[i] = Qnil;

    plist[0] = intern_c_string (":name");
    plist[1] = args[coding_arg_name] = Qno_conversion;
    plist[2] = intern_c_string (":mnemonic");
    plist[3] = args[coding_arg_mnemonic] = make_number ('=');
    plist[4] = intern_c_string (":coding-type");
    plist[5] = args[coding_arg_coding_type] = Qraw_text;
    plist[6] = intern_c_string (":ascii-compatible-p");
    plist[7] = args[coding_arg_ascii_compatible_p] = Qt;
    plist[8] = intern_c_string (":default-char");
    plist[9] = args[coding_arg_default_char] = make_number (0);
    plist[10] = intern_c_string (":for-unibyte");
    plist[11] = args[coding_arg_for_unibyte] = Qt;
    plist[12] = intern_c_string (":docstring");
    plist[13] = make_pure_c_string ("Do no conversion.\n\
\n\
When you visit a file with this coding, the file is read into a\n\
unibyte buffer as is, thus each byte of a file is treated as a\n\
character.");
    plist[14] = intern_c_string (":eol-type");
    plist[15] = args[coding_arg_eol_type] = Qunix;
    args[coding_arg_plist] = Flist (16, plist);
    Fdefine_coding_system_internal (coding_arg_max, args);

    plist[1] = args[coding_arg_name] = Qundecided;
    plist[3] = args[coding_arg_mnemonic] = make_number ('-');
    plist[5] = args[coding_arg_coding_type] = Qundecided;
    /* This is already set.
       plist[7] = args[coding_arg_ascii_compatible_p] = Qt; */
    plist[8] = intern_c_string (":charset-list");
    plist[9] = args[coding_arg_charset_list] = Fcons (Qascii, Qnil);
    plist[11] = args[coding_arg_for_unibyte] = Qnil;
    plist[13] = make_pure_c_string ("No conversion on encoding, automatic conversion on decoding.");
    plist[15] = args[coding_arg_eol_type] = Qnil;
    args[coding_arg_plist] = Flist (16, plist);
    Fdefine_coding_system_internal (coding_arg_max, args);
  }

  setup_coding_system (Qno_conversion, &safe_terminal_coding);

  {
    int i;

    for (i = 0; i < coding_category_max; i++)
      Fset (AREF (Vcoding_category_table, i), Qno_conversion);
  }
#if defined (DOS_NT)
  system_eol_type = Qdos;
#else
  system_eol_type = Qunix;
#endif
  staticpro (&system_eol_type);
}

char *
emacs_strerror (int error_number)
{
  char *str;

  synchronize_system_messages_locale ();
  str = strerror (error_number);

  if (! NILP (Vlocale_coding_system))
    {
      Lisp_Object dec = code_convert_string_norecord (build_string (str),
						      Vlocale_coding_system,
						      0);
      str = SSDATA (dec);
    }

  return str;
}

#endif /* emacs */
