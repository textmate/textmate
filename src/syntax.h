/* Declarations having to do with GNU Emacs syntax tables.

Copyright (C) 1985, 1993-1994, 1997-1998, 2001-2012
  Free Software Foundation, Inc.

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


extern void update_syntax_table (EMACS_INT, EMACS_INT, int, Lisp_Object);

/* The standard syntax table is stored where it will automatically
   be used in all new buffers.  */
#define Vstandard_syntax_table BVAR (&buffer_defaults, syntax_table)

/* A syntax table is a chartable whose elements are cons cells
   (CODE+FLAGS . MATCHING-CHAR).  MATCHING-CHAR can be nil if the char
   is not a kind of parenthesis.

   The low 8 bits of CODE+FLAGS is a code, as follows:  */

enum syntaxcode
  {
    Swhitespace, /* for a whitespace character */
    Spunct,	 /* for random punctuation characters */
    Sword,	 /* for a word constituent */
    Ssymbol,	 /* symbol constituent but not word constituent */
    Sopen,	 /* for a beginning delimiter */
    Sclose,      /* for an ending delimiter */
    Squote,	 /* for a prefix character like Lisp ' */
    Sstring,	 /* for a string-grouping character like Lisp " */
    Smath,	 /* for delimiters like $ in Tex.  */
    Sescape,	 /* for a character that begins a C-style escape */
    Scharquote,  /* for a character that quotes the following character */
    Scomment,    /* for a comment-starting character */
    Sendcomment, /* for a comment-ending character */
    Sinherit,    /* use the standard syntax table for this character */
    Scomment_fence, /* Starts/ends comment which is delimited on the
		       other side by any char with the same syntaxcode.  */
    Sstring_fence,  /* Starts/ends string which is delimited on the
		       other side by any char with the same syntaxcode.  */
    Smax	 /* Upper bound on codes that are meaningful */
  };

/* Set the syntax entry VAL for char C in table TABLE.  */

#define SET_RAW_SYNTAX_ENTRY(table, c, val)	\
  CHAR_TABLE_SET ((table), c, (val))

/* Set the syntax entry VAL for char-range RANGE in table TABLE.
   RANGE is a cons (FROM . TO) specifying the range of characters.  */

#define SET_RAW_SYNTAX_ENTRY_RANGE(table, range, val)	\
  Fset_char_table_range ((table), (range), (val))

/* SYNTAX_ENTRY fetches the information from the entry for character C
   in syntax table TABLE, or from globally kept data (gl_state).
   Does inheritance.  */
/* CURRENT_SYNTAX_TABLE gives the syntax table valid for current
   position, it is either the buffer's syntax table, or syntax table
   found in text properties.  */

#ifdef SYNTAX_ENTRY_VIA_PROPERTY
#  define SYNTAX_ENTRY(c)                                             \
    (gl_state.use_global ? gl_state.global_code : SYNTAX_ENTRY_INT (c))
#  define CURRENT_SYNTAX_TABLE gl_state.current_syntax_table
#else
#  define SYNTAX_ENTRY SYNTAX_ENTRY_INT
#  define CURRENT_SYNTAX_TABLE BVAR (current_buffer, syntax_table)
#endif

#define SYNTAX_ENTRY_INT(c) CHAR_TABLE_REF (CURRENT_SYNTAX_TABLE, (c))

/* Extract the information from the entry for character C
   in the current syntax table.  */

#ifdef __GNUC__
#define SYNTAX(c)							\
  ({ Lisp_Object _syntax_temp;						\
     _syntax_temp = SYNTAX_ENTRY (c);					\
     (CONSP (_syntax_temp)						\
      ? (enum syntaxcode) (XINT (XCAR (_syntax_temp)) & 0xff)		\
      : Swhitespace); })

#define SYNTAX_WITH_FLAGS(c)						\
  ({ Lisp_Object _syntax_temp;						\
     _syntax_temp = SYNTAX_ENTRY (c);					\
     (CONSP (_syntax_temp)						\
      ? XINT (XCAR (_syntax_temp))					\
      : (int) Swhitespace); })

#define SYNTAX_MATCH(c)							\
  ({ Lisp_Object _syntax_temp;						\
     _syntax_temp = SYNTAX_ENTRY (c);					\
     (CONSP (_syntax_temp)						\
      ? XCDR (_syntax_temp)						\
      : Qnil); })
#else
extern Lisp_Object syntax_temp;
#define SYNTAX(c)							\
  (syntax_temp = SYNTAX_ENTRY ((c)),					\
   (CONSP (syntax_temp)							\
    ? (enum syntaxcode) (XINT (XCAR (syntax_temp)) & 0xff)	\
    : Swhitespace))

#define SYNTAX_WITH_FLAGS(c)						\
  (syntax_temp = SYNTAX_ENTRY ((c)),					\
   (CONSP (syntax_temp)							\
    ? XINT (XCAR (syntax_temp))					\
    : (int) Swhitespace))

#define SYNTAX_MATCH(c)							\
  (syntax_temp = SYNTAX_ENTRY ((c)),					\
   (CONSP (syntax_temp)							\
    ? XCDR (syntax_temp)						\
    : Qnil))
#endif


/* Whether the syntax of the character C has the prefix flag set.  */
extern int syntax_prefix_flag_p (int c);

/* This array, indexed by a character, contains the syntax code which that
 character signifies (as a char).  For example,
 (enum syntaxcode) syntax_spec_code['w'] is Sword.  */

extern unsigned char syntax_spec_code[0400];

/* Indexed by syntax code, give the letter that describes it.  */

extern char syntax_code_spec[16];

/* Convert the byte offset BYTEPOS into a character position,
   for the object recorded in gl_state with SETUP_SYNTAX_TABLE_FOR_OBJECT.

   The value is meant for use in the UPDATE_SYNTAX_TABLE... macros.
   These macros do nothing when parse_sexp_lookup_properties is 0,
   so we return 0 in that case, for speed.  */

#define SYNTAX_TABLE_BYTE_TO_CHAR(bytepos)				\
  (! parse_sexp_lookup_properties					\
   ? 0									\
   : STRINGP (gl_state.object)						\
   ? string_byte_to_char (gl_state.object, (bytepos))			\
   : BUFFERP (gl_state.object)						\
   ? buf_bytepos_to_charpos (XBUFFER (gl_state.object),			\
			     (bytepos) + BUF_BEGV_BYTE (XBUFFER (gl_state.object)) - 1) - BUF_BEGV (XBUFFER (gl_state.object)) + 1	\
   : NILP (gl_state.object)						\
   ? BYTE_TO_CHAR ((bytepos) + BEGV_BYTE - 1) - BEGV + 1		\
   : (bytepos))

/* Make syntax table state (gl_state) good for CHARPOS, assuming it is
   currently good for a position before CHARPOS.  */

#define UPDATE_SYNTAX_TABLE_FORWARD(charpos)			\
  (parse_sexp_lookup_properties					\
   && (charpos) >= gl_state.e_property				\
   ? (update_syntax_table ((charpos) + gl_state.offset, 1, 0,	\
			   gl_state.object),			\
      1)							\
   : 0)

/* Make syntax table state (gl_state) good for CHARPOS, assuming it is
   currently good for a position after CHARPOS.  */

#define UPDATE_SYNTAX_TABLE_BACKWARD(charpos)			\
  (parse_sexp_lookup_properties					\
   && (charpos) < gl_state.b_property				\
   ? (update_syntax_table ((charpos) + gl_state.offset, -1, 0,	\
			   gl_state.object),			\
      1)							\
   : 0)

/* Make syntax table good for CHARPOS.  */

#define UPDATE_SYNTAX_TABLE(charpos)				\
  (parse_sexp_lookup_properties					\
   && (charpos) < gl_state.b_property				\
   ? (update_syntax_table ((charpos) + gl_state.offset, -1, 0,	\
			   gl_state.object),			\
      1)							\
   : (parse_sexp_lookup_properties				\
      && (charpos) >= gl_state.e_property			\
      ? (update_syntax_table ((charpos) + gl_state.offset, 1, 0,\
			      gl_state.object),			\
	 1)							\
      : 0))

/* This macro sets up the buffer-global syntax table.  */
#define SETUP_BUFFER_SYNTAX_TABLE()					\
do									\
  {									\
    gl_state.use_global = 0;						\
    gl_state.current_syntax_table = BVAR (current_buffer, syntax_table);\
  } while (0)

/* This macro should be called with FROM at the start of forward
   search, or after the last position of the backward search.  It
   makes sure that the first char is picked up with correct table, so
   one does not need to call UPDATE_SYNTAX_TABLE immediately after the
   call.
   Sign of COUNT gives the direction of the search.
 */

#define SETUP_SYNTAX_TABLE(FROM, COUNT)					\
do									\
  {									\
    SETUP_BUFFER_SYNTAX_TABLE ();					\
    gl_state.b_property = BEGV;						\
    gl_state.e_property = ZV + 1;					\
    gl_state.object = Qnil;						\
    gl_state.offset = 0;						\
    if (parse_sexp_lookup_properties)					\
      if ((COUNT) > 0 || (FROM) > BEGV)					\
        update_syntax_table ((COUNT) > 0 ? (FROM) : (FROM) - 1, (COUNT),\
			     1, Qnil);					\
  }									\
while (0)

/* Same as above, but in OBJECT.  If OBJECT is nil, use current buffer.
   If it is t (which is only used in fast_c_string_match_ignore_case),
   ignore properties altogether.

   This is meant for regex.c to use.  For buffers, regex.c passes arguments
   to the UPDATE_SYNTAX_TABLE macros which are relative to BEGV.
   So if it is a buffer, we set the offset field to BEGV.  */

#define SETUP_SYNTAX_TABLE_FOR_OBJECT(OBJECT, FROM, COUNT)		\
do									\
  {									\
    SETUP_BUFFER_SYNTAX_TABLE ();					\
    gl_state.object = (OBJECT);						\
    if (BUFFERP (gl_state.object))					\
      {									\
	struct buffer *buf = XBUFFER (gl_state.object);			\
	gl_state.b_property = 1;					\
	gl_state.e_property = BUF_ZV (buf) - BUF_BEGV (buf) + 1;	\
	gl_state.offset = BUF_BEGV (buf) - 1;				\
      }									\
    else if (NILP (gl_state.object))					\
      {									\
	gl_state.b_property = 1;					\
	gl_state.e_property = ZV - BEGV + 1;				\
	gl_state.offset = BEGV - 1;					\
      }									\
    else if (EQ (gl_state.object, Qt))					\
      {									\
	gl_state.b_property = 0;					\
	gl_state.e_property = MOST_POSITIVE_FIXNUM;			\
	gl_state.offset = 0;						\
      }									\
    else								\
      {									\
	gl_state.b_property = 0;					\
	gl_state.e_property = 1 + SCHARS (gl_state.object);		\
	gl_state.offset = 0;						\
      }									\
    if (parse_sexp_lookup_properties)					\
      update_syntax_table (((FROM) + gl_state.offset			\
			    + (COUNT > 0 ? 0 :  -1)),			\
			   COUNT, 1, gl_state.object);			\
  }									\
while (0)

struct gl_state_s
{
  Lisp_Object object;			/* The object we are scanning. */
  EMACS_INT start;			/* Where to stop. */
  EMACS_INT stop;			/* Where to stop. */
  int use_global;			/* Whether to use global_code
					   or c_s_t. */
  Lisp_Object global_code;		/* Syntax code of current char. */
  Lisp_Object current_syntax_table;	/* Syntax table for current pos. */
  Lisp_Object old_prop;			/* Syntax-table prop at prev pos. */
  EMACS_INT b_property;			/* First index where c_s_t is valid. */
  EMACS_INT e_property;			/* First index where c_s_t is
					   not valid. */
  INTERVAL forward_i;			/* Where to start lookup on forward */
  INTERVAL backward_i;			/* or backward movement.  The
					   data in c_s_t is valid
					   between these intervals,
					   and possibly at the
					   intervals too, depending
					   on: */
  /* Offset for positions specified to UPDATE_SYNTAX_TABLE.  */
  EMACS_INT offset;
};

extern struct gl_state_s gl_state;
extern EMACS_INT scan_words (EMACS_INT, EMACS_INT);
