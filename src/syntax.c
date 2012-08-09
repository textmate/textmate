/* GNU Emacs routines to deal with syntax tables; also word and list parsing.
   Copyright (C) 1985, 1987, 1993-1995, 1997-1999, 2001-2012
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


#include <config.h>

#include <ctype.h>
#include <sys/types.h>
#include <setjmp.h>
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "character.h"
#include "keymap.h"
#include "regex.h"

/* Make syntax table lookup grant data in gl_state.  */
#define SYNTAX_ENTRY_VIA_PROPERTY

#include "syntax.h"
#include "intervals.h"
#include "category.h"

/* Then there are seven single-bit flags that have the following meanings:
  1. This character is the first of a two-character comment-start sequence.
  2. This character is the second of a two-character comment-start sequence.
  3. This character is the first of a two-character comment-end sequence.
  4. This character is the second of a two-character comment-end sequence.
  5. This character is a prefix, for backward-prefix-chars.
  6. The char is part of a delimiter for comments of style "b".
  7. This character is part of a nestable comment sequence.
  8. The char is part of a delimiter for comments of style "c".
  Note that any two-character sequence whose first character has flag 1
  and whose second character has flag 2 will be interpreted as a comment start.

  bit 6 and 8 are used to discriminate between different comment styles.
  Languages such as C++ allow two orthogonal syntax start/end pairs
  and bit 6 is used to determine whether a comment-end or Scommentend
  ends style a or b.  Comment markers can start style a, b, c, or bc.
  Style a is always the default.
  For 2-char comment markers, the style b flag is only looked up on the second
  char of the comment marker and on the first char of the comment ender.
  For style c (like to for the nested flag), the flag can be placed on any
  one of the chars.
  */

/* These macros extract specific flags from an integer
   that holds the syntax code and the flags.  */

#define SYNTAX_FLAGS_COMSTART_FIRST(flags) (((flags) >> 16) & 1)

#define SYNTAX_FLAGS_COMSTART_SECOND(flags) (((flags) >> 17) & 1)

#define SYNTAX_FLAGS_COMEND_FIRST(flags) (((flags) >> 18) & 1)

#define SYNTAX_FLAGS_COMEND_SECOND(flags) (((flags) >> 19) & 1)

#define SYNTAX_FLAGS_PREFIX(flags) (((flags) >> 20) & 1)

#define SYNTAX_FLAGS_COMMENT_STYLEB(flags) (((flags) >> 21) & 1)
#define SYNTAX_FLAGS_COMMENT_STYLEC(flags) (((flags) >> 22) & 2)
/* FLAGS should be the flags of the main char of the comment marker, e.g.
   the second for comstart and the first for comend.  */
#define SYNTAX_FLAGS_COMMENT_STYLE(flags, other_flags) \
  (SYNTAX_FLAGS_COMMENT_STYLEB (flags) \
   | SYNTAX_FLAGS_COMMENT_STYLEC (flags) \
   | SYNTAX_FLAGS_COMMENT_STYLEC (other_flags))

#define SYNTAX_FLAGS_COMMENT_NESTED(flags) (((flags) >> 22) & 1)

/* These macros extract a particular flag for a given character.  */

#define SYNTAX_COMEND_FIRST(c) \
  (SYNTAX_FLAGS_COMEND_FIRST (SYNTAX_WITH_FLAGS (c)))
#define SYNTAX_PREFIX(c) (SYNTAX_FLAGS_PREFIX (SYNTAX_WITH_FLAGS (c)))

/* We use these constants in place for comment-style and
   string-ender-char to distinguish  comments/strings started by
   comment_fence and string_fence codes.  */

#define ST_COMMENT_STYLE (256 + 1)
#define ST_STRING_STYLE (256 + 2)

static Lisp_Object Qsyntax_table_p;
static Lisp_Object Qsyntax_table, Qscan_error;

#ifndef __GNUC__
/* Used as a temporary in SYNTAX_ENTRY and other macros in syntax.h,
   if not compiled with GCC.  No need to mark it, since it is used
   only very temporarily.  */
Lisp_Object syntax_temp;
#endif

/* This is the internal form of the parse state used in parse-partial-sexp.  */

struct lisp_parse_state
  {
    int depth;	   /* Depth at end of parsing.  */
    int instring;  /* -1 if not within string, else desired terminator.  */
    int incomment; /* -1 if in unnestable comment else comment nesting */
    int comstyle;  /* comment style a=0, or b=1, or ST_COMMENT_STYLE.  */
    int quoted;	   /* Nonzero if just after an escape char at end of parsing */
    int mindepth;  /* Minimum depth seen while scanning.  */
    /* Char number of most recent start-of-expression at current level */
    EMACS_INT thislevelstart;
    /* Char number of start of containing expression */
    EMACS_INT prevlevelstart;
    EMACS_INT location;	     /* Char number at which parsing stopped.  */
    EMACS_INT comstr_start;  /* Position of last comment/string starter.  */
    Lisp_Object levelstarts; /* Char numbers of starts-of-expression
				of levels (starting from outermost).  */
  };

/* These variables are a cache for finding the start of a defun.
   find_start_pos is the place for which the defun start was found.
   find_start_value is the defun start position found for it.
   find_start_value_byte is the corresponding byte position.
   find_start_buffer is the buffer it was found in.
   find_start_begv is the BEGV value when it was found.
   find_start_modiff is the value of MODIFF when it was found.  */

static EMACS_INT find_start_pos;
static EMACS_INT find_start_value;
static EMACS_INT find_start_value_byte;
static struct buffer *find_start_buffer;
static EMACS_INT find_start_begv;
static int find_start_modiff;


static Lisp_Object Fsyntax_table_p (Lisp_Object);
static Lisp_Object skip_chars (int, Lisp_Object, Lisp_Object, int);
static Lisp_Object skip_syntaxes (int, Lisp_Object, Lisp_Object);
static Lisp_Object scan_lists (EMACS_INT, EMACS_INT, EMACS_INT, int);
static void scan_sexps_forward (struct lisp_parse_state *,
                                EMACS_INT, EMACS_INT, EMACS_INT, int,
                                int, Lisp_Object, int);
static int in_classes (int, Lisp_Object);

/* Whether the syntax of the character C has the prefix flag set.  */
int syntax_prefix_flag_p (int c)
{
  return SYNTAX_PREFIX (c);
}

struct gl_state_s gl_state;		/* Global state of syntax parser.  */

#define INTERVALS_AT_ONCE 10		/* 1 + max-number of intervals
					   to scan to property-change.  */

/* Update gl_state to an appropriate interval which contains CHARPOS.  The
   sign of COUNT give the relative position of CHARPOS wrt the previously
   valid interval.  If INIT, only [be]_property fields of gl_state are
   valid at start, the rest is filled basing on OBJECT.

   `gl_state.*_i' are the intervals, and CHARPOS is further in the search
   direction than the intervals - or in an interval.  We update the
   current syntax-table basing on the property of this interval, and
   update the interval to start further than CHARPOS - or be
   NULL_INTERVAL.  We also update lim_property to be the next value of
   charpos to call this subroutine again - or be before/after the
   start/end of OBJECT.  */

void
update_syntax_table (EMACS_INT charpos, EMACS_INT count, int init,
		     Lisp_Object object)
{
  Lisp_Object tmp_table;
  unsigned cnt = 0;
  int invalidate = 1;
  INTERVAL i;

  if (init)
    {
      gl_state.old_prop = Qnil;
      gl_state.start = gl_state.b_property;
      gl_state.stop = gl_state.e_property;
      i = interval_of (charpos, object);
      gl_state.backward_i = gl_state.forward_i = i;
      invalidate = 0;
      if (NULL_INTERVAL_P (i))
	return;
      /* interval_of updates only ->position of the return value, so
	 update the parents manually to speed up update_interval.  */
      while (!NULL_PARENT (i))
	{
	  if (AM_RIGHT_CHILD (i))
	    INTERVAL_PARENT (i)->position = i->position
	      - LEFT_TOTAL_LENGTH (i) + TOTAL_LENGTH (i) /* right end */
	      - TOTAL_LENGTH (INTERVAL_PARENT (i))
	      + LEFT_TOTAL_LENGTH (INTERVAL_PARENT (i));
	  else
	    INTERVAL_PARENT (i)->position = i->position - LEFT_TOTAL_LENGTH (i)
	      + TOTAL_LENGTH (i);
	  i = INTERVAL_PARENT (i);
	}
      i = gl_state.forward_i;
      gl_state.b_property = i->position - gl_state.offset;
      gl_state.e_property = INTERVAL_LAST_POS (i) - gl_state.offset;
      goto update;
    }
  i = count > 0 ? gl_state.forward_i : gl_state.backward_i;

  /* We are guaranteed to be called with CHARPOS either in i,
     or further off.  */
  if (NULL_INTERVAL_P (i))
    error ("Error in syntax_table logic for to-the-end intervals");
  else if (charpos < i->position)		/* Move left.  */
    {
      if (count > 0)
	error ("Error in syntax_table logic for intervals <-");
      /* Update the interval.  */
      i = update_interval (i, charpos);
      if (INTERVAL_LAST_POS (i) != gl_state.b_property)
	{
	  invalidate = 0;
	  gl_state.forward_i = i;
	  gl_state.e_property = INTERVAL_LAST_POS (i) - gl_state.offset;
	}
    }
  else if (charpos >= INTERVAL_LAST_POS (i)) /* Move right.  */
    {
      if (count < 0)
	error ("Error in syntax_table logic for intervals ->");
      /* Update the interval.  */
      i = update_interval (i, charpos);
      if (i->position != gl_state.e_property)
	{
	  invalidate = 0;
	  gl_state.backward_i = i;
	  gl_state.b_property = i->position - gl_state.offset;
	}
    }

  update:
  tmp_table = textget (i->plist, Qsyntax_table);

  if (invalidate)
    invalidate = !EQ (tmp_table, gl_state.old_prop); /* Need to invalidate? */

  if (invalidate)		/* Did not get to adjacent interval.  */
    {				/* with the same table => */
				/* invalidate the old range.  */
      if (count > 0)
	{
	  gl_state.backward_i = i;
	  gl_state.b_property = i->position - gl_state.offset;
	}
      else
	{
	  gl_state.forward_i = i;
	  gl_state.e_property = INTERVAL_LAST_POS (i) - gl_state.offset;
	}
    }

  if (!EQ (tmp_table, gl_state.old_prop))
    {
      gl_state.current_syntax_table = tmp_table;
      gl_state.old_prop = tmp_table;
      if (EQ (Fsyntax_table_p (tmp_table), Qt))
	{
	  gl_state.use_global = 0;
	}
      else if (CONSP (tmp_table))
	{
	  gl_state.use_global = 1;
	  gl_state.global_code = tmp_table;
	}
      else
	{
	  gl_state.use_global = 0;
	  gl_state.current_syntax_table = BVAR (current_buffer, syntax_table);
	}
    }

  while (!NULL_INTERVAL_P (i))
    {
      if (cnt && !EQ (tmp_table, textget (i->plist, Qsyntax_table)))
	{
	  if (count > 0)
	    {
	      gl_state.e_property = i->position - gl_state.offset;
	      gl_state.forward_i = i;
	    }
	  else
	    {
	      gl_state.b_property
		= i->position + LENGTH (i) - gl_state.offset;
	      gl_state.backward_i = i;
	    }
	  return;
	}
      else if (cnt == INTERVALS_AT_ONCE)
	{
	  if (count > 0)
	    {
	      gl_state.e_property
		= i->position + LENGTH (i) - gl_state.offset
		/* e_property at EOB is not set to ZV but to ZV+1, so that
		   we can do INC(from);UPDATE_SYNTAX_TABLE_FORWARD without
		   having to check eob between the two.  */
		+ (NULL_INTERVAL_P (next_interval (i)) ? 1 : 0);
	      gl_state.forward_i = i;
	    }
	  else
	    {
	      gl_state.b_property = i->position - gl_state.offset;
	      gl_state.backward_i = i;
	    }
	  return;
	}
      cnt++;
      i = count > 0 ? next_interval (i) : previous_interval (i);
    }
  eassert (NULL_INTERVAL_P (i)); /* This property goes to the end.  */
  if (count > 0)
    gl_state.e_property = gl_state.stop;
  else
    gl_state.b_property = gl_state.start;
}

/* Returns TRUE if char at CHARPOS is quoted.
   Global syntax-table data should be set up already to be good at CHARPOS
   or after.  On return global syntax data is good for lookup at CHARPOS. */

static int
char_quoted (EMACS_INT charpos, EMACS_INT bytepos)
{
  register enum syntaxcode code;
  register EMACS_INT beg = BEGV;
  register int quoted = 0;
  EMACS_INT orig = charpos;

  while (charpos > beg)
    {
      int c;
      DEC_BOTH (charpos, bytepos);

      UPDATE_SYNTAX_TABLE_BACKWARD (charpos);
      c = FETCH_CHAR_AS_MULTIBYTE (bytepos);
      code = SYNTAX (c);
      if (! (code == Scharquote || code == Sescape))
	break;

      quoted = !quoted;
    }

  UPDATE_SYNTAX_TABLE (orig);
  return quoted;
}

/* Return the bytepos one character before BYTEPOS.
   We assume that BYTEPOS is not at the start of the buffer.  */

static inline EMACS_INT
dec_bytepos (EMACS_INT bytepos)
{
  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    return bytepos - 1;

  DEC_POS (bytepos);
  return bytepos;
}

/* Return a defun-start position before POS and not too far before.
   It should be the last one before POS, or nearly the last.

   When open_paren_in_column_0_is_defun_start is nonzero,
   only the beginning of the buffer is treated as a defun-start.

   We record the information about where the scan started
   and what its result was, so that another call in the same area
   can return the same value very quickly.

   There is no promise at which position the global syntax data is
   valid on return from the subroutine, so the caller should explicitly
   update the global data.  */

static EMACS_INT
find_defun_start (EMACS_INT pos, EMACS_INT pos_byte)
{
  EMACS_INT opoint = PT, opoint_byte = PT_BYTE;

  if (!open_paren_in_column_0_is_defun_start)
    {
      find_start_value = BEGV;
      find_start_value_byte = BEGV_BYTE;
      find_start_buffer = current_buffer;
      find_start_modiff = MODIFF;
      find_start_begv = BEGV;
      find_start_pos = pos;
      return BEGV;
    }

  /* Use previous finding, if it's valid and applies to this inquiry.  */
  if (current_buffer == find_start_buffer
      /* Reuse the defun-start even if POS is a little farther on.
	 POS might be in the next defun, but that's ok.
	 Our value may not be the best possible, but will still be usable.  */
      && pos <= find_start_pos + 1000
      && pos >= find_start_value
      && BEGV == find_start_begv
      && MODIFF == find_start_modiff)
    return find_start_value;

  /* Back up to start of line.  */
  scan_newline (pos, pos_byte, BEGV, BEGV_BYTE, -1, 1);

  /* We optimize syntax-table lookup for rare updates.  Thus we accept
     only those `^\s(' which are good in global _and_ text-property
     syntax-tables.  */
  SETUP_BUFFER_SYNTAX_TABLE ();
  while (PT > BEGV)
    {
      int c;

      /* Open-paren at start of line means we may have found our
	 defun-start.  */
      c = FETCH_CHAR_AS_MULTIBYTE (PT_BYTE);
      if (SYNTAX (c) == Sopen)
	{
	  SETUP_SYNTAX_TABLE (PT + 1, -1);	/* Try again... */
	  c = FETCH_CHAR_AS_MULTIBYTE (PT_BYTE);
	  if (SYNTAX (c) == Sopen)
	    break;
	  /* Now fallback to the default value.  */
	  SETUP_BUFFER_SYNTAX_TABLE ();
	}
      /* Move to beg of previous line.  */
      scan_newline (PT, PT_BYTE, BEGV, BEGV_BYTE, -2, 1);
    }

  /* Record what we found, for the next try.  */
  find_start_value = PT;
  find_start_value_byte = PT_BYTE;
  find_start_buffer = current_buffer;
  find_start_modiff = MODIFF;
  find_start_begv = BEGV;
  find_start_pos = pos;

  TEMP_SET_PT_BOTH (opoint, opoint_byte);

  return find_start_value;
}

/* Return the SYNTAX_COMEND_FIRST of the character before POS, POS_BYTE.  */

static int
prev_char_comend_first (EMACS_INT pos, EMACS_INT pos_byte)
{
  int c, val;

  DEC_BOTH (pos, pos_byte);
  UPDATE_SYNTAX_TABLE_BACKWARD (pos);
  c = FETCH_CHAR (pos_byte);
  val = SYNTAX_COMEND_FIRST (c);
  UPDATE_SYNTAX_TABLE_FORWARD (pos + 1);
  return val;
}

/* Return the SYNTAX_COMSTART_FIRST of the character before POS, POS_BYTE.  */

/* static int
 * prev_char_comstart_first (pos, pos_byte)
 *      int pos, pos_byte;
 * {
 *   int c, val;
 *
 *   DEC_BOTH (pos, pos_byte);
 *   UPDATE_SYNTAX_TABLE_BACKWARD (pos);
 *   c = FETCH_CHAR (pos_byte);
 *   val = SYNTAX_COMSTART_FIRST (c);
 *   UPDATE_SYNTAX_TABLE_FORWARD (pos + 1);
 *   return val;
 * } */

/* Checks whether charpos FROM is at the end of a comment.
   FROM_BYTE is the bytepos corresponding to FROM.
   Do not move back before STOP.

   Return a positive value if we find a comment ending at FROM/FROM_BYTE;
   return -1 otherwise.

   If successful, store the charpos of the comment's beginning
   into *CHARPOS_PTR, and the bytepos into *BYTEPOS_PTR.

   Global syntax data remains valid for backward search starting at
   the returned value (or at FROM, if the search was not successful).  */

static int
back_comment (EMACS_INT from, EMACS_INT from_byte, EMACS_INT stop, int comnested, int comstyle, EMACS_INT *charpos_ptr, EMACS_INT *bytepos_ptr)
{
  /* Look back, counting the parity of string-quotes,
     and recording the comment-starters seen.
     When we reach a safe place, assume that's not in a string;
     then step the main scan to the earliest comment-starter seen
     an even number of string quotes away from the safe place.

     OFROM[I] is position of the earliest comment-starter seen
     which is I+2X quotes from the comment-end.
     PARITY is current parity of quotes from the comment end.  */
  int string_style = -1;	/* Presumed outside of any string. */
  int string_lossage = 0;
  /* Not a real lossage: indicates that we have passed a matching comment
     starter plus a non-matching comment-ender, meaning that any matching
     comment-starter we might see later could be a false positive (hidden
     inside another comment).
     Test case:  { a (* b } c (* d *) */
  int comment_lossage = 0;
  EMACS_INT comment_end = from;
  EMACS_INT comment_end_byte = from_byte;
  EMACS_INT comstart_pos = 0;
  EMACS_INT comstart_byte IF_LINT (= 0);
  /* Place where the containing defun starts,
     or 0 if we didn't come across it yet.  */
  EMACS_INT defun_start = 0;
  EMACS_INT defun_start_byte = 0;
  register enum syntaxcode code;
  int nesting = 1;		/* current comment nesting */
  int c;
  int syntax = 0;

  /* FIXME: A }} comment-ender style leads to incorrect behavior
     in the case of {{ c }}} because we ignore the last two chars which are
     assumed to be comment-enders although they aren't.  */

  /* At beginning of range to scan, we're outside of strings;
     that determines quote parity to the comment-end.  */
  while (from != stop)
    {
      EMACS_INT temp_byte;
      int prev_syntax, com2start, com2end;
      int comstart;

      /* Move back and examine a character.  */
      DEC_BOTH (from, from_byte);
      UPDATE_SYNTAX_TABLE_BACKWARD (from);

      prev_syntax = syntax;
      c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
      syntax = SYNTAX_WITH_FLAGS (c);
      code = SYNTAX (c);

      /* Check for 2-char comment markers.  */
      com2start = (SYNTAX_FLAGS_COMSTART_FIRST (syntax)
		   && SYNTAX_FLAGS_COMSTART_SECOND (prev_syntax)
		   && (comstyle
		       == SYNTAX_FLAGS_COMMENT_STYLE (prev_syntax, syntax))
		   && (SYNTAX_FLAGS_COMMENT_NESTED (prev_syntax)
		       || SYNTAX_FLAGS_COMMENT_NESTED (syntax)) == comnested);
      com2end = (SYNTAX_FLAGS_COMEND_FIRST (syntax)
		 && SYNTAX_FLAGS_COMEND_SECOND (prev_syntax));
      comstart = (com2start || code == Scomment);

      /* Nasty cases with overlapping 2-char comment markers:
	 - snmp-mode: -- c -- foo -- c --
	              --- c --
		      ------ c --
	 - c-mode:    *||*
		      |* *|* *|
		      |*| |* |*|
		      ///   */

      /* If a 2-char comment sequence partly overlaps with another,
	 we don't try to be clever.  E.g. |*| in C, or }% in modes that
	 have %..\n and %{..}%.  */
      if (from > stop && (com2end || comstart))
	{
	  EMACS_INT next = from, next_byte = from_byte;
	  int next_c, next_syntax;
	  DEC_BOTH (next, next_byte);
	  UPDATE_SYNTAX_TABLE_BACKWARD (next);
	  next_c = FETCH_CHAR_AS_MULTIBYTE (next_byte);
	  next_syntax = SYNTAX_WITH_FLAGS (next_c);
	  if (((comstart || comnested)
	       && SYNTAX_FLAGS_COMEND_SECOND (syntax)
	       && SYNTAX_FLAGS_COMEND_FIRST (next_syntax))
	      || ((com2end || comnested)
		  && SYNTAX_FLAGS_COMSTART_SECOND (syntax)
		  && (comstyle
		      == SYNTAX_FLAGS_COMMENT_STYLE (syntax, prev_syntax))
		  && SYNTAX_FLAGS_COMSTART_FIRST (next_syntax)))
	    goto lossage;
	  /* UPDATE_SYNTAX_TABLE_FORWARD (next + 1); */
	}

      if (com2start && comstart_pos == 0)
	/* We're looking at a comment starter.  But it might be a comment
	   ender as well (see snmp-mode).  The first time we see one, we
	   need to consider it as a comment starter,
	   and the subsequent times as a comment ender.  */
	com2end = 0;

      /* Turn a 2-char comment sequences into the appropriate syntax.  */
      if (com2end)
	code = Sendcomment;
      else if (com2start)
	code = Scomment;
      /* Ignore comment starters of a different style.  */
      else if (code == Scomment
	       && (comstyle != SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0)
		   || SYNTAX_FLAGS_COMMENT_NESTED (syntax) != comnested))
	continue;

      /* Ignore escaped characters, except comment-enders.  */
      if (code != Sendcomment && char_quoted (from, from_byte))
	continue;

      switch (code)
	{
	case Sstring_fence:
	case Scomment_fence:
	  c = (code == Sstring_fence ? ST_STRING_STYLE : ST_COMMENT_STYLE);
	case Sstring:
	  /* Track parity of quotes.  */
	  if (string_style == -1)
	    /* Entering a string.  */
	    string_style = c;
	  else if (string_style == c)
	    /* Leaving the string.  */
	    string_style = -1;
	  else
	    /* If we have two kinds of string delimiters.
	       There's no way to grok this scanning backwards.  */
	    string_lossage = 1;
	  break;

	case Scomment:
	  /* We've already checked that it is the relevant comstyle.  */
	  if (string_style != -1 || comment_lossage || string_lossage)
	    /* There are odd string quotes involved, so let's be careful.
	       Test case in Pascal: " { " a { " } */
	    goto lossage;

	  if (!comnested)
	    {
	      /* Record best comment-starter so far.  */
	      comstart_pos = from;
	      comstart_byte = from_byte;
	    }
	  else if (--nesting <= 0)
	    /* nested comments have to be balanced, so we don't need to
	       keep looking for earlier ones.  We use here the same (slightly
	       incorrect) reasoning as below:  since it is followed by uniform
	       paired string quotes, this comment-start has to be outside of
	       strings, else the comment-end itself would be inside a string. */
	    goto done;
	  break;

	case Sendcomment:
	  if (SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0) == comstyle
	      && ((com2end && SYNTAX_FLAGS_COMMENT_NESTED (prev_syntax))
		  || SYNTAX_FLAGS_COMMENT_NESTED (syntax)) == comnested)
	    /* This is the same style of comment ender as ours. */
	    {
	      if (comnested)
		nesting++;
	      else
		/* Anything before that can't count because it would match
		   this comment-ender rather than ours.  */
		from = stop;	/* Break out of the loop.  */
	    }
	  else if (comstart_pos != 0 || c != '\n')
	    /* We're mixing comment styles here, so we'd better be careful.
	       The (comstart_pos != 0 || c != '\n') check is not quite correct
	       (we should just always set comment_lossage), but removing it
	       would imply that any multiline comment in C would go through
	       lossage, which seems overkill.
	       The failure should only happen in the rare cases such as
	         { (* } *)   */
	    comment_lossage = 1;
	  break;

	case Sopen:
	  /* Assume a defun-start point is outside of strings.  */
	  if (open_paren_in_column_0_is_defun_start
	      && (from == stop
		  || (temp_byte = dec_bytepos (from_byte),
		      FETCH_CHAR (temp_byte) == '\n')))
	    {
	      defun_start = from;
	      defun_start_byte = from_byte;
	      from = stop;	/* Break out of the loop.  */
	    }
	  break;

	default:
	  break;
	}
    }

  if (comstart_pos == 0)
    {
      from = comment_end;
      from_byte = comment_end_byte;
      UPDATE_SYNTAX_TABLE_FORWARD (comment_end - 1);
    }
  /* If comstart_pos is set and we get here (ie. didn't jump to `lossage'
     or `done'), then we've found the beginning of the non-nested comment.  */
  else if (1)	/* !comnested */
    {
      from = comstart_pos;
      from_byte = comstart_byte;
      UPDATE_SYNTAX_TABLE_FORWARD (from - 1);
    }
  else
    {
      struct lisp_parse_state state;
    lossage:
      /* We had two kinds of string delimiters mixed up
	 together.  Decode this going forwards.
	 Scan fwd from a known safe place (beginning-of-defun)
	 to the one in question; this records where we
	 last passed a comment starter.  */
      /* If we did not already find the defun start, find it now.  */
      if (defun_start == 0)
	{
	  defun_start = find_defun_start (comment_end, comment_end_byte);
	  defun_start_byte = find_start_value_byte;
	}
      do
	{
	  scan_sexps_forward (&state,
			      defun_start, defun_start_byte,
			      comment_end, -10000, 0, Qnil, 0);
	  defun_start = comment_end;
	  if (state.incomment == (comnested ? 1 : -1)
	      && state.comstyle == comstyle)
	    from = state.comstr_start;
	  else
	    {
	      from = comment_end;
	      if (state.incomment)
		/* If comment_end is inside some other comment, maybe ours
		   is nested, so we need to try again from within the
		   surrounding comment.  Example: { a (* " *)  */
		{
		  /* FIXME: We should advance by one or two chars. */
		  defun_start = state.comstr_start + 2;
		  defun_start_byte = CHAR_TO_BYTE (defun_start);
		}
	    }
	} while (defun_start < comment_end);

      from_byte = CHAR_TO_BYTE (from);
      UPDATE_SYNTAX_TABLE_FORWARD (from - 1);
    }

 done:
  *charpos_ptr = from;
  *bytepos_ptr = from_byte;

  return (from == comment_end) ? -1 : from;
}

DEFUN ("syntax-table-p", Fsyntax_table_p, Ssyntax_table_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a syntax table.
Currently, any char-table counts as a syntax table.  */)
  (Lisp_Object object)
{
  if (CHAR_TABLE_P (object)
      && EQ (XCHAR_TABLE (object)->purpose, Qsyntax_table))
    return Qt;
  return Qnil;
}

static void
check_syntax_table (Lisp_Object obj)
{
  CHECK_TYPE (CHAR_TABLE_P (obj) && EQ (XCHAR_TABLE (obj)->purpose, Qsyntax_table),
	      Qsyntax_table_p, obj);
}

DEFUN ("syntax-table", Fsyntax_table, Ssyntax_table, 0, 0, 0,
       doc: /* Return the current syntax table.
This is the one specified by the current buffer.  */)
  (void)
{
  return BVAR (current_buffer, syntax_table);
}

DEFUN ("standard-syntax-table", Fstandard_syntax_table,
   Sstandard_syntax_table, 0, 0, 0,
       doc: /* Return the standard syntax table.
This is the one used for new buffers.  */)
  (void)
{
  return Vstandard_syntax_table;
}

DEFUN ("copy-syntax-table", Fcopy_syntax_table, Scopy_syntax_table, 0, 1, 0,
       doc: /* Construct a new syntax table and return it.
It is a copy of the TABLE, which defaults to the standard syntax table.  */)
  (Lisp_Object table)
{
  Lisp_Object copy;

  if (!NILP (table))
    check_syntax_table (table);
  else
    table = Vstandard_syntax_table;

  copy = Fcopy_sequence (table);

  /* Only the standard syntax table should have a default element.
     Other syntax tables should inherit from parents instead.  */
  XCHAR_TABLE (copy)->defalt = Qnil;

  /* Copied syntax tables should all have parents.
     If we copied one with no parent, such as the standard syntax table,
     use the standard syntax table as the copy's parent.  */
  if (NILP (XCHAR_TABLE (copy)->parent))
    Fset_char_table_parent (copy, Vstandard_syntax_table);
  return copy;
}

DEFUN ("set-syntax-table", Fset_syntax_table, Sset_syntax_table, 1, 1, 0,
       doc: /* Select a new syntax table for the current buffer.
One argument, a syntax table.  */)
  (Lisp_Object table)
{
  int idx;
  check_syntax_table (table);
  BVAR (current_buffer, syntax_table) = table;
  /* Indicate that this buffer now has a specified syntax table.  */
  idx = PER_BUFFER_VAR_IDX (syntax_table);
  SET_PER_BUFFER_VALUE_P (current_buffer, idx, 1);
  return table;
}

/* Convert a letter which signifies a syntax code
 into the code it signifies.
 This is used by modify-syntax-entry, and other things.  */

unsigned char syntax_spec_code[0400] =
  { 0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    (char) Swhitespace, (char) Scomment_fence, (char) Sstring, 0377,
        (char) Smath, 0377, 0377, (char) Squote,
    (char) Sopen, (char) Sclose, 0377, 0377,
	0377, (char) Swhitespace, (char) Spunct, (char) Scharquote,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377,
	(char) Scomment, 0377, (char) Sendcomment, 0377,
    (char) Sinherit, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* @, A ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
    0377, 0377, 0377, 0377, (char) Sescape, 0377, 0377, (char) Ssymbol,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* `, a, ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
    0377, 0377, 0377, 0377, (char) Sstring_fence, 0377, 0377, 0377
  };

/* Indexed by syntax code, give the letter that describes it.  */

char syntax_code_spec[16] =
  {
    ' ', '.', 'w', '_', '(', ')', '\'', '\"', '$', '\\', '/', '<', '>', '@',
    '!', '|'
  };

/* Indexed by syntax code, give the object (cons of syntax code and
   nil) to be stored in syntax table.  Since these objects can be
   shared among syntax tables, we generate them in advance.  By
   sharing objects, the function `describe-syntax' can give a more
   compact listing.  */
static Lisp_Object Vsyntax_code_object;


DEFUN ("char-syntax", Fchar_syntax, Schar_syntax, 1, 1, 0,
       doc: /* Return the syntax code of CHARACTER, described by a character.
For example, if CHARACTER is a word constituent, the
character `w' (119) is returned.
The characters that correspond to various syntax codes
are listed in the documentation of `modify-syntax-entry'.  */)
  (Lisp_Object character)
{
  int char_int;
  CHECK_CHARACTER (character);
  char_int = XINT (character);
  SETUP_BUFFER_SYNTAX_TABLE ();
  return make_number (syntax_code_spec[(int) SYNTAX (char_int)]);
}

DEFUN ("matching-paren", Fmatching_paren, Smatching_paren, 1, 1, 0,
       doc: /* Return the matching parenthesis of CHARACTER, or nil if none.  */)
  (Lisp_Object character)
{
  int char_int, code;
  CHECK_NUMBER (character);
  char_int = XINT (character);
  SETUP_BUFFER_SYNTAX_TABLE ();
  code = SYNTAX (char_int);
  if (code == Sopen || code == Sclose)
    return SYNTAX_MATCH (char_int);
  return Qnil;
}

DEFUN ("string-to-syntax", Fstring_to_syntax, Sstring_to_syntax, 1, 1, 0,
       doc: /* Convert a syntax specification STRING into syntax cell form.
STRING should be a string as it is allowed as argument of
`modify-syntax-entry'.  Value is the equivalent cons cell
\(CODE . MATCHING-CHAR) that can be used as value of a `syntax-table'
text property.  */)
  (Lisp_Object string)
{
  register const unsigned char *p;
  register enum syntaxcode code;
  int val;
  Lisp_Object match;

  CHECK_STRING (string);

  p = SDATA (string);
  code = (enum syntaxcode) syntax_spec_code[*p++];
  if (((int) code & 0377) == 0377)
    error ("Invalid syntax description letter: %c", p[-1]);

  if (code == Sinherit)
    return Qnil;

  if (*p)
    {
      int len;
      int character = STRING_CHAR_AND_LENGTH (p, len);
      XSETINT (match, character);
      if (XFASTINT (match) == ' ')
	match = Qnil;
      p += len;
    }
  else
    match = Qnil;

  val = (int) code;
  while (*p)
    switch (*p++)
      {
      case '1':
	val |= 1 << 16;
	break;

      case '2':
	val |= 1 << 17;
	break;

      case '3':
	val |= 1 << 18;
	break;

      case '4':
	val |= 1 << 19;
	break;

      case 'p':
	val |= 1 << 20;
	break;

      case 'b':
	val |= 1 << 21;
	break;

      case 'n':
	val |= 1 << 22;
	break;

      case 'c':
	val |= 1 << 23;
	break;
      }

  if (val < ASIZE (Vsyntax_code_object) && NILP (match))
    return XVECTOR (Vsyntax_code_object)->contents[val];
  else
    /* Since we can't use a shared object, let's make a new one.  */
    return Fcons (make_number (val), match);
}

/* I really don't know why this is interactive
   help-form should at least be made useful whilst reading the second arg.  */
DEFUN ("modify-syntax-entry", Fmodify_syntax_entry, Smodify_syntax_entry, 2, 3,
  "cSet syntax for character: \nsSet syntax for %s to: ",
       doc: /* Set syntax for character CHAR according to string NEWENTRY.
The syntax is changed only for table SYNTAX-TABLE, which defaults to
 the current buffer's syntax table.
CHAR may be a cons (MIN . MAX), in which case, syntaxes of all characters
in the range MIN to MAX are changed.
The first character of NEWENTRY should be one of the following:
  Space or -  whitespace syntax.    w   word constituent.
  _           symbol constituent.   .   punctuation.
  (           open-parenthesis.     )   close-parenthesis.
  "           string quote.         \\   escape.
  $           paired delimiter.     '   expression quote or prefix operator.
  <           comment starter.      >   comment ender.
  /           character-quote.      @   inherit from `standard-syntax-table'.
  |           generic string fence. !   generic comment fence.

Only single-character comment start and end sequences are represented thus.
Two-character sequences are represented as described below.
The second character of NEWENTRY is the matching parenthesis,
 used only if the first character is `(' or `)'.
Any additional characters are flags.
Defined flags are the characters 1, 2, 3, 4, b, p, and n.
 1 means CHAR is the start of a two-char comment start sequence.
 2 means CHAR is the second character of such a sequence.
 3 means CHAR is the start of a two-char comment end sequence.
 4 means CHAR is the second character of such a sequence.

There can be several orthogonal comment sequences.  This is to support
language modes such as C++.  By default, all comment sequences are of style
a, but you can set the comment sequence style to b (on the second character
of a comment-start, and the first character of a comment-end sequence) and/or
c (on any of its chars) using this flag:
 b means CHAR is part of comment sequence b.
 c means CHAR is part of comment sequence c.
 n means CHAR is part of a nestable comment sequence.

 p means CHAR is a prefix character for `backward-prefix-chars';
   such characters are treated as whitespace when they occur
   between expressions.
usage: (modify-syntax-entry CHAR NEWENTRY &optional SYNTAX-TABLE)  */)
  (Lisp_Object c, Lisp_Object newentry, Lisp_Object syntax_table)
{
  if (CONSP (c))
    {
      CHECK_CHARACTER_CAR (c);
      CHECK_CHARACTER_CDR (c);
    }
  else
    CHECK_CHARACTER (c);

  if (NILP (syntax_table))
    syntax_table = BVAR (current_buffer, syntax_table);
  else
    check_syntax_table (syntax_table);

  newentry = Fstring_to_syntax (newentry);
  if (CONSP (c))
    SET_RAW_SYNTAX_ENTRY_RANGE (syntax_table, c, newentry);
  else
    SET_RAW_SYNTAX_ENTRY (syntax_table, XINT (c), newentry);

  /* We clear the regexp cache, since character classes can now have
     different values from those in the compiled regexps.*/
  clear_regexp_cache ();

  return Qnil;
}

/* Dump syntax table to buffer in human-readable format */

DEFUN ("internal-describe-syntax-value", Finternal_describe_syntax_value,
       Sinternal_describe_syntax_value, 1, 1, 0,
       doc: /* Insert a description of the internal syntax description SYNTAX at point.  */)
  (Lisp_Object syntax)
{
  register enum syntaxcode code;
  int syntax_code;
  char desc, start1, start2, end1, end2, prefix,
    comstyleb, comstylec, comnested;
  char str[2];
  Lisp_Object first, match_lisp, value = syntax;

  if (NILP (value))
    {
      insert_string ("default");
      return syntax;
    }

  if (CHAR_TABLE_P (value))
    {
      insert_string ("deeper char-table ...");
      return syntax;
    }

  if (!CONSP (value))
    {
      insert_string ("invalid");
      return syntax;
    }

  first = XCAR (value);
  match_lisp = XCDR (value);

  if (!INTEGERP (first) || !(NILP (match_lisp) || INTEGERP (match_lisp)))
    {
      insert_string ("invalid");
      return syntax;
    }

  syntax_code = XINT (first);
  code = (enum syntaxcode) (syntax_code & 0377);
  start1 = SYNTAX_FLAGS_COMSTART_FIRST (syntax_code);
  start2 = SYNTAX_FLAGS_COMSTART_SECOND (syntax_code);;
  end1 = SYNTAX_FLAGS_COMEND_FIRST (syntax_code);
  end2 = SYNTAX_FLAGS_COMEND_SECOND (syntax_code);
  prefix = SYNTAX_FLAGS_PREFIX (syntax_code);
  comstyleb = SYNTAX_FLAGS_COMMENT_STYLEB (syntax_code);
  comstylec = SYNTAX_FLAGS_COMMENT_STYLEC (syntax_code);
  comnested = SYNTAX_FLAGS_COMMENT_NESTED (syntax_code);

  if ((int) code < 0 || (int) code >= (int) Smax)
    {
      insert_string ("invalid");
      return syntax;
    }
  desc = syntax_code_spec[(int) code];

  str[0] = desc, str[1] = 0;
  insert (str, 1);

  if (NILP (match_lisp))
    insert (" ", 1);
  else
    insert_char (XINT (match_lisp));

  if (start1)
    insert ("1", 1);
  if (start2)
    insert ("2", 1);

  if (end1)
    insert ("3", 1);
  if (end2)
    insert ("4", 1);

  if (prefix)
    insert ("p", 1);
  if (comstyleb)
    insert ("b", 1);
  if (comstylec)
    insert ("c", 1);
  if (comnested)
    insert ("n", 1);

  insert_string ("\twhich means: ");

  switch (SWITCH_ENUM_CAST (code))
    {
    case Swhitespace:
      insert_string ("whitespace"); break;
    case Spunct:
      insert_string ("punctuation"); break;
    case Sword:
      insert_string ("word"); break;
    case Ssymbol:
      insert_string ("symbol"); break;
    case Sopen:
      insert_string ("open"); break;
    case Sclose:
      insert_string ("close"); break;
    case Squote:
      insert_string ("prefix"); break;
    case Sstring:
      insert_string ("string"); break;
    case Smath:
      insert_string ("math"); break;
    case Sescape:
      insert_string ("escape"); break;
    case Scharquote:
      insert_string ("charquote"); break;
    case Scomment:
      insert_string ("comment"); break;
    case Sendcomment:
      insert_string ("endcomment"); break;
    case Sinherit:
      insert_string ("inherit"); break;
    case Scomment_fence:
      insert_string ("comment fence"); break;
    case Sstring_fence:
      insert_string ("string fence"); break;
    default:
      insert_string ("invalid");
      return syntax;
    }

  if (!NILP (match_lisp))
    {
      insert_string (", matches ");
      insert_char (XINT (match_lisp));
    }

  if (start1)
    insert_string (",\n\t  is the first character of a comment-start sequence");
  if (start2)
    insert_string (",\n\t  is the second character of a comment-start sequence");

  if (end1)
    insert_string (",\n\t  is the first character of a comment-end sequence");
  if (end2)
    insert_string (",\n\t  is the second character of a comment-end sequence");
  if (comstyleb)
    insert_string (" (comment style b)");
  if (comstylec)
    insert_string (" (comment style c)");
  if (comnested)
    insert_string (" (nestable)");

  if (prefix)
    insert_string (",\n\t  is a prefix character for `backward-prefix-chars'");

  return syntax;
}

/* Return the position across COUNT words from FROM.
   If that many words cannot be found before the end of the buffer, return 0.
   COUNT negative means scan backward and stop at word beginning.  */

EMACS_INT
scan_words (register EMACS_INT from, register EMACS_INT count)
{
  register EMACS_INT beg = BEGV;
  register EMACS_INT end = ZV;
  register EMACS_INT from_byte = CHAR_TO_BYTE (from);
  register enum syntaxcode code;
  int ch0, ch1;
  Lisp_Object func, pos;

  immediate_quit = 1;
  QUIT;

  SETUP_SYNTAX_TABLE (from, count);

  while (count > 0)
    {
      while (1)
	{
	  if (from == end)
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  ch0 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  code = SYNTAX (ch0);
	  INC_BOTH (from, from_byte);
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	}
      /* Now CH0 is a character which begins a word and FROM is the
         position of the next character.  */
      func = CHAR_TABLE_REF (Vfind_word_boundary_function_table, ch0);
      if (! NILP (Ffboundp (func)))
	{
	  pos = call2 (func, make_number (from - 1), make_number (end));
	  if (INTEGERP (pos) && XINT (pos) > from)
	    {
	      from = XINT (pos);
	      from_byte = CHAR_TO_BYTE (from);
	    }
	}
      else
	{
	  while (1)
	    {
	      if (from == end) break;
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	      ch1 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      code = SYNTAX (ch1);
	      if ((code != Sword
		   && (! words_include_escapes
		       || (code != Sescape && code != Scharquote)))
		  || word_boundary_p (ch0, ch1))
		break;
	      INC_BOTH (from, from_byte);
	      ch0 = ch1;
	    }
	}
      count--;
    }
  while (count < 0)
    {
      while (1)
	{
	  if (from == beg)
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	  DEC_BOTH (from, from_byte);
	  UPDATE_SYNTAX_TABLE_BACKWARD (from);
	  ch1 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  code = SYNTAX (ch1);
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	}
      /* Now CH1 is a character which ends a word and FROM is the
         position of it.  */
      func = CHAR_TABLE_REF (Vfind_word_boundary_function_table, ch1);
      if (! NILP (Ffboundp (func)))
 	{
	  pos = call2 (func, make_number (from), make_number (beg));
	  if (INTEGERP (pos) && XINT (pos) < from)
	    {
	      from = XINT (pos);
	      from_byte = CHAR_TO_BYTE (from);
	    }
	}
      else
	{
	  while (1)
	    {
	      if (from == beg)
		break;
	      DEC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_BACKWARD (from);
	      ch0 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      code = SYNTAX (ch0);
	      if ((code != Sword
		   && (! words_include_escapes
		       || (code != Sescape && code != Scharquote)))
		  || word_boundary_p (ch0, ch1))
		{
		  INC_BOTH (from, from_byte);
		  break;
		}
	      ch1 = ch0;
	    }
	}
      count++;
    }

  immediate_quit = 0;

  return from;
}

DEFUN ("forward-word", Fforward_word, Sforward_word, 0, 1, "^p",
       doc: /* Move point forward ARG words (backward if ARG is negative).
Normally returns t.
If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed if
`inhibit-field-text-motion' is non-nil.  */)
  (Lisp_Object arg)
{
  Lisp_Object tmp;
  int orig_val, val;

  if (NILP (arg))
    XSETFASTINT (arg, 1);
  else
    CHECK_NUMBER (arg);

  val = orig_val = scan_words (PT, XINT (arg));
  if (! orig_val)
    val = XINT (arg) > 0 ? ZV : BEGV;

  /* Avoid jumping out of an input field.  */
  tmp = Fconstrain_to_field (make_number (val), make_number (PT),
			     Qt, Qnil, Qnil);
  val = XFASTINT (tmp);

  SET_PT (val);
  return val == orig_val ? Qt : Qnil;
}

DEFUN ("skip-chars-forward", Fskip_chars_forward, Sskip_chars_forward, 1, 2, 0,
       doc: /* Move point forward, stopping before a char not in STRING, or at pos LIM.
STRING is like the inside of a `[...]' in a regular expression
except that `]' is never special and `\\' quotes `^', `-' or `\\'
 (but not at the end of a range; quoting is never needed there).
Thus, with arg "a-zA-Z", this skips letters stopping before first nonletter.
With arg "^a-zA-Z", skips nonletters stopping before first letter.
Char classes, e.g. `[:alpha:]', are supported.

Returns the distance traveled, either zero or positive.  */)
  (Lisp_Object string, Lisp_Object lim)
{
  return skip_chars (1, string, lim, 1);
}

DEFUN ("skip-chars-backward", Fskip_chars_backward, Sskip_chars_backward, 1, 2, 0,
       doc: /* Move point backward, stopping after a char not in STRING, or at pos LIM.
See `skip-chars-forward' for details.
Returns the distance traveled, either zero or negative.  */)
  (Lisp_Object string, Lisp_Object lim)
{
  return skip_chars (0, string, lim, 1);
}

DEFUN ("skip-syntax-forward", Fskip_syntax_forward, Sskip_syntax_forward, 1, 2, 0,
       doc: /* Move point forward across chars in specified syntax classes.
SYNTAX is a string of syntax code characters.
Stop before a char whose syntax is not in SYNTAX, or at position LIM.
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
This function returns the distance traveled, either zero or positive.  */)
  (Lisp_Object syntax, Lisp_Object lim)
{
  return skip_syntaxes (1, syntax, lim);
}

DEFUN ("skip-syntax-backward", Fskip_syntax_backward, Sskip_syntax_backward, 1, 2, 0,
       doc: /* Move point backward across chars in specified syntax classes.
SYNTAX is a string of syntax code characters.
Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
This function returns the distance traveled, either zero or negative.  */)
  (Lisp_Object syntax, Lisp_Object lim)
{
  return skip_syntaxes (0, syntax, lim);
}

static Lisp_Object
skip_chars (int forwardp, Lisp_Object string, Lisp_Object lim, int handle_iso_classes)
{
  register unsigned int c;
  unsigned char fastmap[0400];
  /* Store the ranges of non-ASCII characters.  */
  int *char_ranges IF_LINT (= NULL);
  int n_char_ranges = 0;
  int negate = 0;
  register EMACS_INT i, i_byte;
  /* Set to 1 if the current buffer is multibyte and the region
     contains non-ASCII chars.  */
  int multibyte;
  /* Set to 1 if STRING is multibyte and it contains non-ASCII
     chars.  */
  int string_multibyte;
  EMACS_INT size_byte;
  const unsigned char *str;
  int len;
  Lisp_Object iso_classes;

  CHECK_STRING (string);
  iso_classes = Qnil;

  if (NILP (lim))
    XSETINT (lim, forwardp ? ZV : BEGV);
  else
    CHECK_NUMBER_COERCE_MARKER (lim);

  /* In any case, don't allow scan outside bounds of buffer.  */
  if (XINT (lim) > ZV)
    XSETFASTINT (lim, ZV);
  if (XINT (lim) < BEGV)
    XSETFASTINT (lim, BEGV);

  multibyte = (!NILP (BVAR (current_buffer, enable_multibyte_characters))
	       && (XINT (lim) - PT != CHAR_TO_BYTE (XINT (lim)) - PT_BYTE));
  string_multibyte = SBYTES (string) > SCHARS (string);

  memset (fastmap, 0, sizeof fastmap);

  str = SDATA (string);
  size_byte = SBYTES (string);

  i_byte = 0;
  if (i_byte < size_byte
      && SREF (string, 0) == '^')
    {
      negate = 1; i_byte++;
    }

  /* Find the characters specified and set their elements of fastmap.
     Handle backslashes and ranges specially.

     If STRING contains non-ASCII characters, setup char_ranges for
     them and use fastmap only for their leading codes.  */

  if (! string_multibyte)
    {
      int string_has_eight_bit = 0;

      /* At first setup fastmap.  */
      while (i_byte < size_byte)
	{
	  c = str[i_byte++];

	  if (handle_iso_classes && c == '['
	      && i_byte < size_byte
	      && str[i_byte] == ':')
	    {
	      const unsigned char *class_beg = str + i_byte + 1;
	      const unsigned char *class_end = class_beg;
	      const unsigned char *class_limit = str + size_byte - 2;
	      /* Leave room for the null.  */
	      unsigned char class_name[CHAR_CLASS_MAX_LENGTH + 1];
	      re_wctype_t cc;

	      if (class_limit - class_beg > CHAR_CLASS_MAX_LENGTH)
		class_limit = class_beg + CHAR_CLASS_MAX_LENGTH;

	      while (class_end < class_limit
		     && *class_end >= 'a' && *class_end <= 'z')
		class_end++;

	      if (class_end == class_beg
		  || *class_end != ':' || class_end[1] != ']')
		goto not_a_class_name;

	      memcpy (class_name, class_beg, class_end - class_beg);
	      class_name[class_end - class_beg] = 0;

	      cc = re_wctype (class_name);
	      if (cc == 0)
		error ("Invalid ISO C character class");

	      iso_classes = Fcons (make_number (cc), iso_classes);

	      i_byte = class_end + 2 - str;
	      continue;
	    }

	not_a_class_name:
	  if (c == '\\')
	    {
	      if (i_byte == size_byte)
		break;

	      c = str[i_byte++];
	    }
	  /* Treat `-' as range character only if another character
	     follows.  */
	  if (i_byte + 1 < size_byte
	      && str[i_byte] == '-')
	    {
	      unsigned int c2;

	      /* Skip over the dash.  */
	      i_byte++;

	      /* Get the end of the range.  */
	      c2 = str[i_byte++];
	      if (c2 == '\\'
		  && i_byte < size_byte)
		c2 = str[i_byte++];

	      if (c <= c2)
		{
		  unsigned lim2 = c2 + 1;
		  while (c < lim2)
		    fastmap[c++] = 1;
		  if (! ASCII_CHAR_P (c2))
		    string_has_eight_bit = 1;
		}
	    }
	  else
	    {
	      fastmap[c] = 1;
	      if (! ASCII_CHAR_P (c))
		string_has_eight_bit = 1;
	    }
	}

      /* If the current range is multibyte and STRING contains
	 eight-bit chars, arrange fastmap and setup char_ranges for
	 the corresponding multibyte chars.  */
      if (multibyte && string_has_eight_bit)
	{
	  unsigned char fastmap2[0400];
	  int range_start_byte, range_start_char;

	  memcpy (fastmap + 0200, fastmap2 + 0200, 0200);
	  memset (fastmap + 0200, 0, 0200);
	  /* We are sure that this loop stops.  */
	  for (i = 0200; ! fastmap2[i]; i++);
	  c = BYTE8_TO_CHAR (i);
	  fastmap[CHAR_LEADING_CODE (c)] = 1;
	  range_start_byte = i;
	  range_start_char = c;
	  char_ranges = (int *) alloca (sizeof (int) * 128 * 2);
	  for (i = 129; i < 0400; i++)
	    {
	      c = BYTE8_TO_CHAR (i);
	      fastmap[CHAR_LEADING_CODE (c)] = 1;
	      if (i - range_start_byte != c - range_start_char)
		{
		  char_ranges[n_char_ranges++] = range_start_char;
		  char_ranges[n_char_ranges++] = ((i - 1 - range_start_byte)
						  + range_start_char);
		  range_start_byte = i;
		  range_start_char = c;
		}
	    }
	  char_ranges[n_char_ranges++] = range_start_char;
	  char_ranges[n_char_ranges++] = ((i - 1 - range_start_byte)
					  + range_start_char);
	}
    }
  else				/* STRING is multibyte */
    {
      char_ranges = (int *) alloca (sizeof (int) * SCHARS (string) * 2);

      while (i_byte < size_byte)
	{
	  unsigned char leading_code;

	  leading_code = str[i_byte];
	  c = STRING_CHAR_AND_LENGTH (str + i_byte, len);
	  i_byte += len;

	  if (handle_iso_classes && c == '['
	      && i_byte < size_byte
	      && STRING_CHAR (str + i_byte) == ':')
	    {
	      const unsigned char *class_beg = str + i_byte + 1;
	      const unsigned char *class_end = class_beg;
	      const unsigned char *class_limit = str + size_byte - 2;
	      /* Leave room for the null.	 */
	      unsigned char class_name[CHAR_CLASS_MAX_LENGTH + 1];
	      re_wctype_t cc;

	      if (class_limit - class_beg > CHAR_CLASS_MAX_LENGTH)
		class_limit = class_beg + CHAR_CLASS_MAX_LENGTH;

	      while (class_end < class_limit
		     && *class_end >= 'a' && *class_end <= 'z')
		class_end++;

	      if (class_end == class_beg
		  || *class_end != ':' || class_end[1] != ']')
		goto not_a_class_name_multibyte;

	      memcpy (class_name, class_beg, class_end - class_beg);
	      class_name[class_end - class_beg] = 0;

	      cc = re_wctype (class_name);
	      if (cc == 0)
		error ("Invalid ISO C character class");

	      iso_classes = Fcons (make_number (cc), iso_classes);

	      i_byte = class_end + 2 - str;
	      continue;
	    }

	not_a_class_name_multibyte:
	  if (c == '\\')
	    {
	      if (i_byte == size_byte)
		break;

	      leading_code = str[i_byte];
	      c = STRING_CHAR_AND_LENGTH (str + i_byte, len);
	      i_byte += len;
	    }
	  /* Treat `-' as range character only if another character
	     follows.  */
	  if (i_byte + 1 < size_byte
	      && str[i_byte] == '-')
	    {
	      unsigned int c2;
	      unsigned char leading_code2;

	      /* Skip over the dash.  */
	      i_byte++;

	      /* Get the end of the range.  */
	      leading_code2 = str[i_byte];
	      c2 = STRING_CHAR_AND_LENGTH (str + i_byte, len);
	      i_byte += len;

	      if (c2 == '\\'
		  && i_byte < size_byte)
		{
		  leading_code2 = str[i_byte];
		  c2 =STRING_CHAR_AND_LENGTH (str + i_byte, len);
		  i_byte += len;
		}

	      if (c > c2)
		continue;
	      if (ASCII_CHAR_P (c))
		{
		  while (c <= c2 && c < 0x80)
		    fastmap[c++] = 1;
		  leading_code = CHAR_LEADING_CODE (c);
		}
	      if (! ASCII_CHAR_P (c))
		{
		  unsigned lim2 = leading_code2 + 1;
		  while (leading_code < lim2)
		    fastmap[leading_code++] = 1;
		  if (c <= c2)
		    {
		      char_ranges[n_char_ranges++] = c;
		      char_ranges[n_char_ranges++] = c2;
		    }
		}
	    }
	  else
	    {
	      if (ASCII_CHAR_P (c))
		fastmap[c] = 1;
	      else
		{
		  fastmap[leading_code] = 1;
		  char_ranges[n_char_ranges++] = c;
		  char_ranges[n_char_ranges++] = c;
		}
	    }
	}

      /* If the current range is unibyte and STRING contains non-ASCII
	 chars, arrange fastmap for the corresponding unibyte
	 chars.  */

      if (! multibyte && n_char_ranges > 0)
	{
	  memset (fastmap + 0200, 0, 0200);
	  for (i = 0; i < n_char_ranges; i += 2)
	    {
	      int c1 = char_ranges[i];
	      unsigned lim2 = char_ranges[i + 1] + 1;

	      for (; c1 < lim2; c1++)
		{
		  int b = CHAR_TO_BYTE_SAFE (c1);
		  if (b >= 0)
		    fastmap[b] = 1;
		}
	    }
	}
    }

  /* If ^ was the first character, complement the fastmap.  */
  if (negate)
    {
      if (! multibyte)
	for (i = 0; i < sizeof fastmap; i++)
	  fastmap[i] ^= 1;
      else
	{
	  for (i = 0; i < 0200; i++)
	    fastmap[i] ^= 1;
	  /* All non-ASCII chars possibly match.  */
	  for (; i < sizeof fastmap; i++)
	    fastmap[i] = 1;
	}
    }

  {
    EMACS_INT start_point = PT;
    EMACS_INT pos = PT;
    EMACS_INT pos_byte = PT_BYTE;
    unsigned char *p = PT_ADDR, *endp, *stop;

    if (forwardp)
      {
	endp = (XINT (lim) == GPT) ? GPT_ADDR : CHAR_POS_ADDR (XINT (lim));
	stop = (pos < GPT && GPT < XINT (lim)) ? GPT_ADDR : endp;
      }
    else
      {
	endp = CHAR_POS_ADDR (XINT (lim));
	stop = (pos >= GPT && GPT > XINT (lim)) ? GAP_END_ADDR : endp;
      }

    immediate_quit = 1;
    /* This code may look up syntax tables using macros that rely on the
       gl_state object.  To make sure this object is not out of date,
       let's initialize it manually.
       We ignore syntax-table text-properties for now, since that's
       what we've done in the past.  */
    SETUP_BUFFER_SYNTAX_TABLE ();
    if (forwardp)
      {
	if (multibyte)
	  while (1)
	    {
	      int nbytes;

	      if (p >= stop)
		{
		  if (p >= endp)
		    break;
		  p = GAP_END_ADDR;
		  stop = endp;
		}
	      c = STRING_CHAR_AND_LENGTH (p, nbytes);
	      if (! NILP (iso_classes) && in_classes (c, iso_classes))
		{
		  if (negate)
		    break;
		  else
		    goto fwd_ok;
		}

	      if (! fastmap[*p])
		break;
	      if (! ASCII_CHAR_P (c))
		{
		  /* As we are looking at a multibyte character, we
		     must look up the character in the table
		     CHAR_RANGES.  If there's no data in the table,
		     that character is not what we want to skip.  */

		  /* The following code do the right thing even if
		     n_char_ranges is zero (i.e. no data in
		     CHAR_RANGES).  */
		  for (i = 0; i < n_char_ranges; i += 2)
		    if (c >= char_ranges[i] && c <= char_ranges[i + 1])
		      break;
		  if (!(negate ^ (i < n_char_ranges)))
		    break;
		}
	    fwd_ok:
	      p += nbytes, pos++, pos_byte += nbytes;
	    }
	else
	  while (1)
	    {
	      if (p >= stop)
		{
		  if (p >= endp)
		    break;
		  p = GAP_END_ADDR;
		  stop = endp;
		}

	      if (!NILP (iso_classes) && in_classes (*p, iso_classes))
		{
		  if (negate)
		    break;
		  else
		    goto fwd_unibyte_ok;
		}

	      if (!fastmap[*p])
		break;
	    fwd_unibyte_ok:
	      p++, pos++, pos_byte++;
	    }
      }
    else
      {
	if (multibyte)
	  while (1)
	    {
	      unsigned char *prev_p;

	      if (p <= stop)
		{
		  if (p <= endp)
		    break;
		  p = GPT_ADDR;
		  stop = endp;
		}
	      prev_p = p;
	      while (--p >= stop && ! CHAR_HEAD_P (*p));
	      c = STRING_CHAR (p);

	      if (! NILP (iso_classes) && in_classes (c, iso_classes))
		{
		  if (negate)
		    break;
		  else
		    goto back_ok;
		}

	      if (! fastmap[*p])
		break;
	      if (! ASCII_CHAR_P (c))
		{
		  /* See the comment in the previous similar code.  */
		  for (i = 0; i < n_char_ranges; i += 2)
		    if (c >= char_ranges[i] && c <= char_ranges[i + 1])
		      break;
		  if (!(negate ^ (i < n_char_ranges)))
		    break;
		}
	    back_ok:
	      pos--, pos_byte -= prev_p - p;
	    }
	else
	  while (1)
	    {
	      if (p <= stop)
		{
		  if (p <= endp)
		    break;
		  p = GPT_ADDR;
		  stop = endp;
		}

	      if (! NILP (iso_classes) && in_classes (p[-1], iso_classes))
		{
		  if (negate)
		    break;
		  else
		    goto back_unibyte_ok;
		}

	      if (!fastmap[p[-1]])
		break;
	    back_unibyte_ok:
	      p--, pos--, pos_byte--;
	    }
      }

    SET_PT_BOTH (pos, pos_byte);
    immediate_quit = 0;

    return make_number (PT - start_point);
  }
}


static Lisp_Object
skip_syntaxes (int forwardp, Lisp_Object string, Lisp_Object lim)
{
  register unsigned int c;
  unsigned char fastmap[0400];
  int negate = 0;
  register EMACS_INT i, i_byte;
  int multibyte;
  EMACS_INT size_byte;
  unsigned char *str;

  CHECK_STRING (string);

  if (NILP (lim))
    XSETINT (lim, forwardp ? ZV : BEGV);
  else
    CHECK_NUMBER_COERCE_MARKER (lim);

  /* In any case, don't allow scan outside bounds of buffer.  */
  if (XINT (lim) > ZV)
    XSETFASTINT (lim, ZV);
  if (XINT (lim) < BEGV)
    XSETFASTINT (lim, BEGV);

  if (forwardp ? (PT >= XFASTINT (lim)) : (PT <= XFASTINT (lim)))
    return make_number (0);

  multibyte = (!NILP (BVAR (current_buffer, enable_multibyte_characters))
	       && (XINT (lim) - PT != CHAR_TO_BYTE (XINT (lim)) - PT_BYTE));

  memset (fastmap, 0, sizeof fastmap);

  if (SBYTES (string) > SCHARS (string))
    /* As this is very rare case (syntax spec is ASCII only), don't
       consider efficiency.  */
    string = string_make_unibyte (string);

  str = SDATA (string);
  size_byte = SBYTES (string);

  i_byte = 0;
  if (i_byte < size_byte
      && SREF (string, 0) == '^')
    {
      negate = 1; i_byte++;
    }

  /* Find the syntaxes specified and set their elements of fastmap.  */

  while (i_byte < size_byte)
    {
      c = str[i_byte++];
      fastmap[syntax_spec_code[c]] = 1;
    }

  /* If ^ was the first character, complement the fastmap.  */
  if (negate)
    for (i = 0; i < sizeof fastmap; i++)
      fastmap[i] ^= 1;

  {
    EMACS_INT start_point = PT;
    EMACS_INT pos = PT;
    EMACS_INT pos_byte = PT_BYTE;
    unsigned char *p = PT_ADDR, *endp, *stop;

    if (forwardp)
      {
	endp = (XINT (lim) == GPT) ? GPT_ADDR : CHAR_POS_ADDR (XINT (lim));
	stop = (pos < GPT && GPT < XINT (lim)) ? GPT_ADDR : endp;
      }
    else
      {
	endp = CHAR_POS_ADDR (XINT (lim));
	stop = (pos >= GPT && GPT > XINT (lim)) ? GAP_END_ADDR : endp;
      }

    immediate_quit = 1;
    SETUP_SYNTAX_TABLE (pos, forwardp ? 1 : -1);
    if (forwardp)
      {
	if (multibyte)
	  {
	    while (1)
	      {
		int nbytes;

		if (p >= stop)
		  {
		    if (p >= endp)
		      break;
		    p = GAP_END_ADDR;
		    stop = endp;
		  }
		c = STRING_CHAR_AND_LENGTH (p, nbytes);
		if (! fastmap[(int) SYNTAX (c)])
		  break;
		p += nbytes, pos++, pos_byte += nbytes;
		UPDATE_SYNTAX_TABLE_FORWARD (pos);
	      }
	  }
	else
	  {
	    while (1)
	      {
		if (p >= stop)
		  {
		    if (p >= endp)
		      break;
		    p = GAP_END_ADDR;
		    stop = endp;
		  }
		if (! fastmap[(int) SYNTAX (*p)])
		  break;
		p++, pos++, pos_byte++;
		UPDATE_SYNTAX_TABLE_FORWARD (pos);
	      }
	  }
      }
    else
      {
	if (multibyte)
	  {
	    while (1)
	      {
		unsigned char *prev_p;

		if (p <= stop)
		  {
		    if (p <= endp)
		      break;
		    p = GPT_ADDR;
		    stop = endp;
		  }
		UPDATE_SYNTAX_TABLE_BACKWARD (pos - 1);
		prev_p = p;
		while (--p >= stop && ! CHAR_HEAD_P (*p));
		c = STRING_CHAR (p);
		if (! fastmap[(int) SYNTAX (c)])
		  break;
		pos--, pos_byte -= prev_p - p;
	      }
	  }
	else
	  {
	    while (1)
	      {
		if (p <= stop)
		  {
		    if (p <= endp)
		      break;
		    p = GPT_ADDR;
		    stop = endp;
		  }
		UPDATE_SYNTAX_TABLE_BACKWARD (pos - 1);
		if (! fastmap[(int) SYNTAX (p[-1])])
		  break;
		p--, pos--, pos_byte--;
	      }
	  }
      }

    SET_PT_BOTH (pos, pos_byte);
    immediate_quit = 0;

    return make_number (PT - start_point);
  }
}

/* Return 1 if character C belongs to one of the ISO classes
   in the list ISO_CLASSES.  Each class is represented by an
   integer which is its type according to re_wctype.  */

static int
in_classes (int c, Lisp_Object iso_classes)
{
  int fits_class = 0;

  while (CONSP (iso_classes))
    {
      Lisp_Object elt;
      elt = XCAR (iso_classes);
      iso_classes = XCDR (iso_classes);

      if (re_iswctype (c, XFASTINT (elt)))
	fits_class = 1;
    }

  return fits_class;
}

/* Jump over a comment, assuming we are at the beginning of one.
   FROM is the current position.
   FROM_BYTE is the bytepos corresponding to FROM.
   Do not move past STOP (a charpos).
   The comment over which we have to jump is of style STYLE
     (either SYNTAX_FLAGS_COMMENT_STYLE(foo) or ST_COMMENT_STYLE).
   NESTING should be positive to indicate the nesting at the beginning
     for nested comments and should be zero or negative else.
     ST_COMMENT_STYLE cannot be nested.
   PREV_SYNTAX is the SYNTAX_WITH_FLAGS of the previous character
     (or 0 If the search cannot start in the middle of a two-character).

   If successful, return 1 and store the charpos of the comment's end
   into *CHARPOS_PTR and the corresponding bytepos into *BYTEPOS_PTR.
   Else, return 0 and store the charpos STOP into *CHARPOS_PTR, the
   corresponding bytepos into *BYTEPOS_PTR and the current nesting
   (as defined for state.incomment) in *INCOMMENT_PTR.

   The comment end is the last character of the comment rather than the
     character just after the comment.

   Global syntax data is assumed to initially be valid for FROM and
   remains valid for forward search starting at the returned position. */

static int
forw_comment (EMACS_INT from, EMACS_INT from_byte, EMACS_INT stop,
	      int nesting, int style, int prev_syntax,
	      EMACS_INT *charpos_ptr, EMACS_INT *bytepos_ptr,
	      int *incomment_ptr)
{
  register int c, c1;
  register enum syntaxcode code;
  register int syntax, other_syntax;

  if (nesting <= 0) nesting = -1;

  /* Enter the loop in the middle so that we find
     a 2-char comment ender if we start in the middle of it.  */
  syntax = prev_syntax;
  if (syntax != 0) goto forw_incomment;

  while (1)
    {
      if (from == stop)
	{
	  *incomment_ptr = nesting;
	  *charpos_ptr = from;
	  *bytepos_ptr = from_byte;
	  return 0;
	}
      c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
      syntax = SYNTAX_WITH_FLAGS (c);
      code = syntax & 0xff;
      if (code == Sendcomment
	  && SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0) == style
	  && (SYNTAX_FLAGS_COMMENT_NESTED (syntax) ?
	      (nesting > 0 && --nesting == 0) : nesting < 0))
	/* we have encountered a comment end of the same style
	   as the comment sequence which began this comment
	   section */
	break;
      if (code == Scomment_fence
	  && style == ST_COMMENT_STYLE)
	/* we have encountered a comment end of the same style
	   as the comment sequence which began this comment
	   section.  */
	break;
      if (nesting > 0
	  && code == Scomment
	  && SYNTAX_FLAGS_COMMENT_NESTED (syntax)
	  && SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0) == style)
	/* we have encountered a nested comment of the same style
	   as the comment sequence which began this comment section */
	nesting++;
      INC_BOTH (from, from_byte);
      UPDATE_SYNTAX_TABLE_FORWARD (from);

    forw_incomment:
      if (from < stop && SYNTAX_FLAGS_COMEND_FIRST (syntax)
	  && (c1 = FETCH_CHAR_AS_MULTIBYTE (from_byte),
	      other_syntax = SYNTAX_WITH_FLAGS (c1),
	      SYNTAX_FLAGS_COMEND_SECOND (other_syntax))
	  && SYNTAX_FLAGS_COMMENT_STYLE (syntax, other_syntax) == style
	  && ((SYNTAX_FLAGS_COMMENT_NESTED (syntax) ||
	       SYNTAX_FLAGS_COMMENT_NESTED (other_syntax))
	      ? nesting > 0 : nesting < 0))
	{
	  if (--nesting <= 0)
	    /* we have encountered a comment end of the same style
	       as the comment sequence which began this comment
	       section */
	    break;
	  else
	    {
	      INC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	    }
	}
      if (nesting > 0
	  && from < stop
	  && SYNTAX_FLAGS_COMSTART_FIRST (syntax)
	  && (c1 = FETCH_CHAR_AS_MULTIBYTE (from_byte),
	      other_syntax = SYNTAX_WITH_FLAGS (c1),
	      SYNTAX_FLAGS_COMMENT_STYLE (other_syntax, syntax) == style
	      && SYNTAX_FLAGS_COMSTART_SECOND (other_syntax))
	  && (SYNTAX_FLAGS_COMMENT_NESTED (syntax) ||
	      SYNTAX_FLAGS_COMMENT_NESTED (other_syntax)))
	/* we have encountered a nested comment of the same style
	   as the comment sequence which began this comment
	   section */
	{
	  INC_BOTH (from, from_byte);
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  nesting++;
	}
    }
  *charpos_ptr = from;
  *bytepos_ptr = from_byte;
  return 1;
}

DEFUN ("forward-comment", Fforward_comment, Sforward_comment, 1, 1, 0,
       doc: /*
Move forward across up to COUNT comments.  If COUNT is negative, move backward.
Stop scanning if we find something other than a comment or whitespace.
Set point to where scanning stops.
If COUNT comments are found as expected, with nothing except whitespace
between them, return t; otherwise return nil.  */)
  (Lisp_Object count)
{
  register EMACS_INT from;
  EMACS_INT from_byte;
  register EMACS_INT stop;
  register int c, c1;
  register enum syntaxcode code;
  int comstyle = 0;	    /* style of comment encountered */
  int comnested = 0;	    /* whether the comment is nestable or not */
  int found;
  EMACS_INT count1;
  EMACS_INT out_charpos, out_bytepos;
  int dummy;

  CHECK_NUMBER (count);
  count1 = XINT (count);
  stop = count1 > 0 ? ZV : BEGV;

  immediate_quit = 1;
  QUIT;

  from = PT;
  from_byte = PT_BYTE;

  SETUP_SYNTAX_TABLE (from, count1);
  while (count1 > 0)
    {
      do
	{
	  int comstart_first, syntax, other_syntax;

	  if (from == stop)
	    {
	      SET_PT_BOTH (from, from_byte);
	      immediate_quit = 0;
	      return Qnil;
	    }
	  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  syntax = SYNTAX_WITH_FLAGS (c);
	  code = SYNTAX (c);
	  comstart_first = SYNTAX_FLAGS_COMSTART_FIRST (syntax);
	  comnested = SYNTAX_FLAGS_COMMENT_NESTED (syntax);
	  comstyle = SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0);
	  INC_BOTH (from, from_byte);
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  if (from < stop && comstart_first
	      && (c1 = FETCH_CHAR_AS_MULTIBYTE (from_byte),
		  other_syntax = SYNTAX_WITH_FLAGS (c1),
		  SYNTAX_FLAGS_COMSTART_SECOND (other_syntax)))
	    {
	      /* We have encountered a comment start sequence and we
		 are ignoring all text inside comments.  We must record
		 the comment style this sequence begins so that later,
		 only a comment end of the same style actually ends
		 the comment section.  */
	      code = Scomment;
	      comstyle = SYNTAX_FLAGS_COMMENT_STYLE (other_syntax, syntax);
	      comnested
		= comnested || SYNTAX_FLAGS_COMMENT_NESTED (other_syntax);
	      INC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	    }
	}
      while (code == Swhitespace || (code == Sendcomment && c == '\n'));

      if (code == Scomment_fence)
	comstyle = ST_COMMENT_STYLE;
      else if (code != Scomment)
	{
	  immediate_quit = 0;
	  DEC_BOTH (from, from_byte);
	  SET_PT_BOTH (from, from_byte);
	  return Qnil;
	}
      /* We're at the start of a comment.  */
      found = forw_comment (from, from_byte, stop, comnested, comstyle, 0,
			    &out_charpos, &out_bytepos, &dummy);
      from = out_charpos; from_byte = out_bytepos;
      if (!found)
	{
	  immediate_quit = 0;
	  SET_PT_BOTH (from, from_byte);
	  return Qnil;
	}
      INC_BOTH (from, from_byte);
      UPDATE_SYNTAX_TABLE_FORWARD (from);
      /* We have skipped one comment.  */
      count1--;
    }

  while (count1 < 0)
    {
      while (1)
	{
	  int quoted, syntax;

	  if (from <= stop)
	    {
	      SET_PT_BOTH (BEGV, BEGV_BYTE);
	      immediate_quit = 0;
	      return Qnil;
	    }

	  DEC_BOTH (from, from_byte);
	  /* char_quoted does UPDATE_SYNTAX_TABLE_BACKWARD (from).  */
	  quoted = char_quoted (from, from_byte);
	  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  syntax = SYNTAX_WITH_FLAGS (c);
	  code = SYNTAX (c);
	  comstyle = 0;
	  comnested = SYNTAX_FLAGS_COMMENT_NESTED (syntax);
	  if (code == Sendcomment)
	    comstyle = SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0);
	  if (from > stop && SYNTAX_FLAGS_COMEND_SECOND (syntax)
	      && prev_char_comend_first (from, from_byte)
	      && !char_quoted (from - 1, dec_bytepos (from_byte)))
	    {
	      int other_syntax;
	      /* We must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style.  */
	      DEC_BOTH (from, from_byte);
	      code = Sendcomment;
	      /* Calling char_quoted, above, set up global syntax position
		 at the new value of FROM.  */
	      c1 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      other_syntax = SYNTAX_WITH_FLAGS (c1);
	      comstyle = SYNTAX_FLAGS_COMMENT_STYLE (other_syntax, syntax);
	      comnested
		= comnested || SYNTAX_FLAGS_COMMENT_NESTED (other_syntax);
	    }

	  if (code == Scomment_fence)
	    {
	      /* Skip until first preceding unquoted comment_fence.  */
	      int fence_found = 0;
	      EMACS_INT ini = from, ini_byte = from_byte;

	      while (1)
		{
		  DEC_BOTH (from, from_byte);
		  UPDATE_SYNTAX_TABLE_BACKWARD (from);
		  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
		  if (SYNTAX (c) == Scomment_fence
		      && !char_quoted (from, from_byte))
		    {
		      fence_found = 1;
		      break;
		    }
		  else if (from == stop)
		    break;
		}
	      if (fence_found == 0)
		{
		  from = ini;		/* Set point to ini + 1.  */
		  from_byte = ini_byte;
		  goto leave;
		}
 	      else
		/* We have skipped one comment.  */
		break;
	    }
	  else if (code == Sendcomment)
	    {
	      found = back_comment (from, from_byte, stop, comnested, comstyle,
				    &out_charpos, &out_bytepos);
	      if (found == -1)
		{
		  if (c == '\n')
		    /* This end-of-line is not an end-of-comment.
		       Treat it like a whitespace.
		       CC-mode (and maybe others) relies on this behavior.  */
		    ;
		  else
		    {
		      /* Failure: we should go back to the end of this
			 not-quite-endcomment.  */
		      if (SYNTAX (c) != code)
			/* It was a two-char Sendcomment.  */
			INC_BOTH (from, from_byte);
		      goto leave;
		    }
		}
	      else
		{
		  /* We have skipped one comment.  */
		  from = out_charpos, from_byte = out_bytepos;
		  break;
		}
	    }
	  else if (code != Swhitespace || quoted)
	    {
	    leave:
	      immediate_quit = 0;
	      INC_BOTH (from, from_byte);
	      SET_PT_BOTH (from, from_byte);
	      return Qnil;
	    }
	}

      count1++;
    }

  SET_PT_BOTH (from, from_byte);
  immediate_quit = 0;
  return Qt;
}

/* Return syntax code of character C if C is an ASCII character
   or `multibyte_symbol_p' is zero.  Otherwise, return Ssymbol.  */

#define SYNTAX_WITH_MULTIBYTE_CHECK(c)		\
  ((ASCII_CHAR_P (c) || !multibyte_symbol_p)	\
   ? SYNTAX (c) : Ssymbol)

static Lisp_Object
scan_lists (register EMACS_INT from, EMACS_INT count, EMACS_INT depth, int sexpflag)
{
  Lisp_Object val;
  register EMACS_INT stop = count > 0 ? ZV : BEGV;
  register int c, c1;
  int stringterm;
  int quoted;
  int mathexit = 0;
  register enum syntaxcode code, temp_code;
  int min_depth = depth;    /* Err out if depth gets less than this.  */
  int comstyle = 0;	    /* style of comment encountered */
  int comnested = 0;	    /* whether the comment is nestable or not */
  EMACS_INT temp_pos;
  EMACS_INT last_good = from;
  int found;
  EMACS_INT from_byte;
  EMACS_INT out_bytepos, out_charpos;
  int temp, dummy;
  int multibyte_symbol_p = sexpflag && multibyte_syntax_as_symbol;

  if (depth > 0) min_depth = 0;

  if (from > ZV) from = ZV;
  if (from < BEGV) from = BEGV;

  from_byte = CHAR_TO_BYTE (from);

  immediate_quit = 1;
  QUIT;

  SETUP_SYNTAX_TABLE (from, count);
  while (count > 0)
    {
      while (from < stop)
	{
	  int comstart_first, prefix, syntax, other_syntax;
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  syntax = SYNTAX_WITH_FLAGS (c);
	  code = SYNTAX_WITH_MULTIBYTE_CHECK (c);
	  comstart_first = SYNTAX_FLAGS_COMSTART_FIRST (syntax);
	  comnested = SYNTAX_FLAGS_COMMENT_NESTED (syntax);
	  comstyle = SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0);
	  prefix = SYNTAX_FLAGS_PREFIX (syntax);
	  if (depth == min_depth)
	    last_good = from;
	  INC_BOTH (from, from_byte);
	  UPDATE_SYNTAX_TABLE_FORWARD (from);
	  if (from < stop && comstart_first
	      && (c = FETCH_CHAR_AS_MULTIBYTE (from_byte),
		  other_syntax = SYNTAX_WITH_FLAGS (c),
		  SYNTAX_FLAGS_COMSTART_SECOND (other_syntax))
	      && parse_sexp_ignore_comments)
	    {
	      /* we have encountered a comment start sequence and we
		 are ignoring all text inside comments.  We must record
		 the comment style this sequence begins so that later,
		 only a comment end of the same style actually ends
		 the comment section */
	      code = Scomment;
	      comstyle = SYNTAX_FLAGS_COMMENT_STYLE (other_syntax, syntax);
	      comnested
		= comnested || SYNTAX_FLAGS_COMMENT_NESTED (other_syntax);
	      INC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	    }

	  if (prefix)
	    continue;

	  switch (SWITCH_ENUM_CAST (code))
	    {
	    case Sescape:
	    case Scharquote:
	      if (from == stop)
		goto lose;
	      INC_BOTH (from, from_byte);
	      /* treat following character as a word constituent */
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; return at end of it.  */
	      while (from < stop)
		{
		  UPDATE_SYNTAX_TABLE_FORWARD (from);

		  /* Some compilers can't handle this inside the switch.  */
		  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
		  temp = SYNTAX_WITH_MULTIBYTE_CHECK (c);
		  switch (temp)
		    {
		    case Scharquote:
		    case Sescape:
		      INC_BOTH (from, from_byte);
		      if (from == stop)
			goto lose;
		      break;
		    case Sword:
		    case Ssymbol:
		    case Squote:
		      break;
		    default:
		      goto done;
		    }
		  INC_BOTH (from, from_byte);
		}
	      goto done;

	    case Scomment_fence:
	      comstyle = ST_COMMENT_STYLE;
	      /* FALLTHROUGH */
	    case Scomment:
	      if (!parse_sexp_ignore_comments) break;
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	      found = forw_comment (from, from_byte, stop,
				    comnested, comstyle, 0,
				    &out_charpos, &out_bytepos, &dummy);
	      from = out_charpos, from_byte = out_bytepos;
	      if (!found)
		{
		  if (depth == 0)
		    goto done;
		  goto lose;
		}
	      INC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_FORWARD (from);
	      break;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == FETCH_CHAR_AS_MULTIBYTE (from_byte))
		{
		  INC_BOTH (from, from_byte);
		}
	      if (mathexit)
		{
		  mathexit = 0;
		  goto close1;
		}
	      mathexit = 1;

	    case Sopen:
	      if (!++depth) goto done;
	      break;

	    case Sclose:
	    close1:
	      if (!--depth) goto done;
	      if (depth < min_depth)
		xsignal3 (Qscan_error,
			  build_string ("Containing expression ends prematurely"),
			  make_number (last_good), make_number (from));
	      break;

	    case Sstring:
	    case Sstring_fence:
	      temp_pos = dec_bytepos (from_byte);
	      stringterm = FETCH_CHAR_AS_MULTIBYTE (temp_pos);
	      while (1)
		{
		  if (from >= stop)
		    goto lose;
		  UPDATE_SYNTAX_TABLE_FORWARD (from);
		  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
		  if (code == Sstring
		      ? (c == stringterm
			 && SYNTAX_WITH_MULTIBYTE_CHECK (c) == Sstring)
		      : SYNTAX_WITH_MULTIBYTE_CHECK (c) == Sstring_fence)
		    break;

		  /* Some compilers can't handle this inside the switch.  */
		  temp = SYNTAX_WITH_MULTIBYTE_CHECK (c);
		  switch (temp)
		    {
		    case Scharquote:
		    case Sescape:
		      INC_BOTH (from, from_byte);
		    }
		  INC_BOTH (from, from_byte);
		}
	      INC_BOTH (from, from_byte);
	      if (!depth && sexpflag) goto done;
	      break;
	    default:
	      /* Ignore whitespace, punctuation, quote, endcomment.  */
	      break;
	    }
	}

      /* Reached end of buffer.  Error if within object, return nil if between */
      if (depth)
	goto lose;

      immediate_quit = 0;
      return Qnil;

      /* End of object reached */
    done:
      count--;
    }

  while (count < 0)
    {
      while (from > stop)
	{
	  int syntax;
	  DEC_BOTH (from, from_byte);
	  UPDATE_SYNTAX_TABLE_BACKWARD (from);
	  c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	  syntax= SYNTAX_WITH_FLAGS (c);
	  code = SYNTAX_WITH_MULTIBYTE_CHECK (c);
	  if (depth == min_depth)
	    last_good = from;
	  comstyle = 0;
	  comnested = SYNTAX_FLAGS_COMMENT_NESTED (syntax);
	  if (code == Sendcomment)
	    comstyle = SYNTAX_FLAGS_COMMENT_STYLE (syntax, 0);
	  if (from > stop && SYNTAX_FLAGS_COMEND_SECOND (syntax)
	      && prev_char_comend_first (from, from_byte)
	      && parse_sexp_ignore_comments)
	    {
	      /* We must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style.  */
	      int c2, other_syntax;
	      DEC_BOTH (from, from_byte);
	      UPDATE_SYNTAX_TABLE_BACKWARD (from);
	      code = Sendcomment;
	      c2 = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      other_syntax = SYNTAX_WITH_FLAGS (c2);
	      comstyle = SYNTAX_FLAGS_COMMENT_STYLE (other_syntax, syntax);
	      comnested
		= comnested || SYNTAX_FLAGS_COMMENT_NESTED (other_syntax);
	    }

	  /* Quoting turns anything except a comment-ender
	     into a word character.  Note that this cannot be true
	     if we decremented FROM in the if-statement above.  */
	  if (code != Sendcomment && char_quoted (from, from_byte))
	    {
	      DEC_BOTH (from, from_byte);
	      code = Sword;
	    }
	  else if (SYNTAX_FLAGS_PREFIX (syntax))
	    continue;

	  switch (SWITCH_ENUM_CAST (code))
	    {
	    case Sword:
	    case Ssymbol:
	    case Sescape:
	    case Scharquote:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; count object finished
		 after passing it.  */
	      while (from > stop)
		{
		  temp_pos = from_byte;
		  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
		    DEC_POS (temp_pos);
		  else
		    temp_pos--;
		  UPDATE_SYNTAX_TABLE_BACKWARD (from - 1);
		  c1 = FETCH_CHAR_AS_MULTIBYTE (temp_pos);
		  temp_code = SYNTAX_WITH_MULTIBYTE_CHECK (c1);
		  /* Don't allow comment-end to be quoted.  */
		  if (temp_code == Sendcomment)
		    goto done2;
		  quoted = char_quoted (from - 1, temp_pos);
		  if (quoted)
		    {
		      DEC_BOTH (from, from_byte);
		      temp_pos = dec_bytepos (temp_pos);
		      UPDATE_SYNTAX_TABLE_BACKWARD (from - 1);
		    }
		  c1 = FETCH_CHAR_AS_MULTIBYTE (temp_pos);
		  temp_code = SYNTAX_WITH_MULTIBYTE_CHECK (c1);
		  if (! (quoted || temp_code == Sword
			 || temp_code == Ssymbol
			 || temp_code == Squote))
            	    goto done2;
		  DEC_BOTH (from, from_byte);
		}
	      goto done2;

	    case Smath:
	      if (!sexpflag)
		break;
	      temp_pos = dec_bytepos (from_byte);
	      UPDATE_SYNTAX_TABLE_BACKWARD (from - 1);
	      if (from != stop && c == FETCH_CHAR_AS_MULTIBYTE (temp_pos))
		DEC_BOTH (from, from_byte);
	      if (mathexit)
		{
		  mathexit = 0;
		  goto open2;
		}
	      mathexit = 1;

	    case Sclose:
	      if (!++depth) goto done2;
	      break;

	    case Sopen:
	    open2:
	      if (!--depth) goto done2;
	      if (depth < min_depth)
		xsignal3 (Qscan_error,
			  build_string ("Containing expression ends prematurely"),
			  make_number (last_good), make_number (from));
	      break;

	    case Sendcomment:
	      if (!parse_sexp_ignore_comments)
		break;
	      found = back_comment (from, from_byte, stop, comnested, comstyle,
				    &out_charpos, &out_bytepos);
	      /* FIXME:  if found == -1, then it really wasn't a comment-end.
		 For single-char Sendcomment, we can't do much about it apart
		 from skipping the char.
		 For 2-char endcomments, we could try again, taking both
		 chars as separate entities, but it's a lot of trouble
		 for very little gain, so we don't bother either.  -sm */
	      if (found != -1)
		from = out_charpos, from_byte = out_bytepos;
	      break;

	    case Scomment_fence:
	    case Sstring_fence:
	      while (1)
		{
		  if (from == stop)
		    goto lose;
		  DEC_BOTH (from, from_byte);
		  UPDATE_SYNTAX_TABLE_BACKWARD (from);
		  if (!char_quoted (from, from_byte)
		      && (c = FETCH_CHAR_AS_MULTIBYTE (from_byte),
			  SYNTAX_WITH_MULTIBYTE_CHECK (c) == code))
		    break;
		}
	      if (code == Sstring_fence && !depth && sexpflag) goto done2;
	      break;

	    case Sstring:
	      stringterm = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      while (1)
		{
		  if (from == stop)
		    goto lose;
		  DEC_BOTH (from, from_byte);
		  UPDATE_SYNTAX_TABLE_BACKWARD (from);
		  if (!char_quoted (from, from_byte)
		      && (stringterm
			  == (c = FETCH_CHAR_AS_MULTIBYTE (from_byte)))
		      && SYNTAX_WITH_MULTIBYTE_CHECK (c) == Sstring)
		    break;
		}
	      if (!depth && sexpflag) goto done2;
	      break;
	    default:
	      /* Ignore whitespace, punctuation, quote, endcomment.  */
	      break;
	    }
	}

      /* Reached start of buffer.  Error if within object, return nil if between */
      if (depth)
	goto lose;

      immediate_quit = 0;
      return Qnil;

    done2:
      count++;
    }


  immediate_quit = 0;
  XSETFASTINT (val, from);
  return val;

 lose:
  xsignal3 (Qscan_error,
	    build_string ("Unbalanced parentheses"),
	    make_number (last_good), make_number (from));
}

DEFUN ("scan-lists", Fscan_lists, Sscan_lists, 3, 3, 0,
       doc: /* Scan from character number FROM by COUNT lists.
Scan forward if COUNT is positive, backward if COUNT is negative.
Return the character number of the position thus found.

A \"list", in this context, refers to a balanced parenthetical
grouping, as determined by the syntax table.

If DEPTH is nonzero, treat that as the nesting depth of the starting
point (i.e. the starting point is DEPTH parentheses deep).  This
function scans over parentheses until the depth goes to zero COUNT
times.  Hence, positive DEPTH moves out that number of levels of
parentheses, while negative DEPTH moves to a deeper level.

Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

If we reach the beginning or end of the accessible part of the buffer
before we have scanned over COUNT lists, return nil if the depth at
that point is zero, and signal a error if the depth is nonzero.  */)
  (Lisp_Object from, Lisp_Object count, Lisp_Object depth)
{
  CHECK_NUMBER (from);
  CHECK_NUMBER (count);
  CHECK_NUMBER (depth);

  return scan_lists (XINT (from), XINT (count), XINT (depth), 0);
}

DEFUN ("scan-sexps", Fscan_sexps, Sscan_sexps, 2, 2, 0,
       doc: /* Scan from character number FROM by COUNT balanced expressions.
If COUNT is negative, scan backwards.
Returns the character number of the position thus found.

Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

If the beginning or end of (the accessible part of) the buffer is reached
in the middle of a parenthetical grouping, an error is signaled.
If the beginning or end is reached between groupings
but before count is used up, nil is returned.  */)
  (Lisp_Object from, Lisp_Object count)
{
  CHECK_NUMBER (from);
  CHECK_NUMBER (count);

  return scan_lists (XINT (from), XINT (count), 0, 1);
}

DEFUN ("backward-prefix-chars", Fbackward_prefix_chars, Sbackward_prefix_chars,
       0, 0, 0,
       doc: /* Move point backward over any number of chars with prefix syntax.
This includes chars with "quote" or "prefix" syntax (' or p).  */)
  (void)
{
  EMACS_INT beg = BEGV;
  EMACS_INT opoint = PT;
  EMACS_INT opoint_byte = PT_BYTE;
  EMACS_INT pos = PT;
  EMACS_INT pos_byte = PT_BYTE;
  int c;

  if (pos <= beg)
    {
      SET_PT_BOTH (opoint, opoint_byte);

      return Qnil;
    }

  SETUP_SYNTAX_TABLE (pos, -1);

  DEC_BOTH (pos, pos_byte);

  while (!char_quoted (pos, pos_byte)
	 /* Previous statement updates syntax table.  */
	 && ((c = FETCH_CHAR_AS_MULTIBYTE (pos_byte), SYNTAX (c) == Squote)
	     || SYNTAX_PREFIX (c)))
    {
      opoint = pos;
      opoint_byte = pos_byte;

      if (pos + 1 > beg)
	DEC_BOTH (pos, pos_byte);
    }

  SET_PT_BOTH (opoint, opoint_byte);

  return Qnil;
}

/* Parse forward from FROM / FROM_BYTE to END,
   assuming that FROM has state OLDSTATE (nil means FROM is start of function),
   and return a description of the state of the parse at END.
   If STOPBEFORE is nonzero, stop at the start of an atom.
   If COMMENTSTOP is 1, stop at the start of a comment.
   If COMMENTSTOP is -1, stop at the start or end of a comment,
   after the beginning of a string, or after the end of a string.  */

static void
scan_sexps_forward (struct lisp_parse_state *stateptr,
		    EMACS_INT from, EMACS_INT from_byte, EMACS_INT end,
		    int targetdepth, int stopbefore,
		    Lisp_Object oldstate, int commentstop)
{
  struct lisp_parse_state state;

  register enum syntaxcode code;
  int c1;
  int comnested;
  struct level { int last, prev; };
  struct level levelstart[100];
  register struct level *curlevel = levelstart;
  struct level *endlevel = levelstart + 100;
  register int depth;	/* Paren depth of current scanning location.
			   level - levelstart equals this except
			   when the depth becomes negative.  */
  int mindepth;		/* Lowest DEPTH value seen.  */
  int start_quoted = 0;		/* Nonzero means starting after a char quote */
  Lisp_Object tem;
  EMACS_INT prev_from;		/* Keep one character before FROM.  */
  EMACS_INT prev_from_byte;
  int prev_from_syntax;
  int boundary_stop = commentstop == -1;
  int nofence;
  int found;
  EMACS_INT out_bytepos, out_charpos;
  int temp;

  prev_from = from;
  prev_from_byte = from_byte;
  if (from != BEGV)
    DEC_BOTH (prev_from, prev_from_byte);

  /* Use this macro instead of `from++'.  */
#define INC_FROM				\
do { prev_from = from;				\
     prev_from_byte = from_byte; 		\
     temp = FETCH_CHAR_AS_MULTIBYTE (prev_from_byte);	\
     prev_from_syntax = SYNTAX_WITH_FLAGS (temp); \
     INC_BOTH (from, from_byte);		\
     if (from < end)				\
       UPDATE_SYNTAX_TABLE_FORWARD (from);	\
  } while (0)

  immediate_quit = 1;
  QUIT;

  if (NILP (oldstate))
    {
      depth = 0;
      state.instring = -1;
      state.incomment = 0;
      state.comstyle = 0;	/* comment style a by default.  */
      state.comstr_start = -1;	/* no comment/string seen.  */
    }
  else
    {
      tem = Fcar (oldstate);
      if (!NILP (tem))
	depth = XINT (tem);
      else
	depth = 0;

      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      /* Check whether we are inside string_fence-style string: */
      state.instring = (!NILP (tem)
			? (INTEGERP (tem) ? XINT (tem) : ST_STRING_STYLE)
			: -1);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.incomment = (!NILP (tem)
			 ? (INTEGERP (tem) ? XINT (tem) : -1)
			 : 0);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      start_quoted = !NILP (tem);

      /* if the eighth element of the list is nil, we are in comment
	 style a.  If it is non-nil, we are in comment style b */
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.comstyle = (NILP (tem)
			? 0
			: (EQ (tem, Qsyntax_table)
			   ? ST_COMMENT_STYLE
			   : INTEGERP (tem) ? XINT (tem) : 1));

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.comstr_start = NILP (tem) ? -1 : XINT (tem) ;
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      while (!NILP (tem))		/* >= second enclosing sexps.  */
	{
	  /* curlevel++->last ran into compiler bug on Apollo */
	  curlevel->last = XINT (Fcar (tem));
	  if (++curlevel == endlevel)
	    curlevel--; /* error ("Nesting too deep for parser"); */
	  curlevel->prev = -1;
	  curlevel->last = -1;
	  tem = Fcdr (tem);
	}
    }
  state.quoted = 0;
  mindepth = depth;

  curlevel->prev = -1;
  curlevel->last = -1;

  SETUP_SYNTAX_TABLE (prev_from, 1);
  temp = FETCH_CHAR (prev_from_byte);
  prev_from_syntax = SYNTAX_WITH_FLAGS (temp);
  UPDATE_SYNTAX_TABLE_FORWARD (from);

  /* Enter the loop at a place appropriate for initial state.  */

  if (state.incomment)
    goto startincomment;
  if (state.instring >= 0)
    {
      nofence = state.instring != ST_STRING_STYLE;
      if (start_quoted)
	goto startquotedinstring;
      goto startinstring;
    }
  else if (start_quoted)
    goto startquoted;

  while (from < end)
    {
      int syntax;
      INC_FROM;
      code = prev_from_syntax & 0xff;

      if (from < end
	  && SYNTAX_FLAGS_COMSTART_FIRST (prev_from_syntax)
	  && (c1 = FETCH_CHAR (from_byte),
	      syntax = SYNTAX_WITH_FLAGS (c1),
	      SYNTAX_FLAGS_COMSTART_SECOND (syntax)))
	/* Duplicate code to avoid a complex if-expression
	   which causes trouble for the SGI compiler.  */
	{
	  /* Record the comment style we have entered so that only
	     the comment-end sequence of the same style actually
	     terminates the comment section.  */
	  state.comstyle
	    = SYNTAX_FLAGS_COMMENT_STYLE (syntax, prev_from_syntax);
	  comnested = SYNTAX_FLAGS_COMMENT_NESTED (prev_from_syntax);
	  comnested = comnested || SYNTAX_FLAGS_COMMENT_NESTED (syntax);
	  state.incomment = comnested ? 1 : -1;
	  state.comstr_start = prev_from;
	  INC_FROM;
	  code = Scomment;
	}
      else if (code == Scomment_fence)
	{
	  /* Record the comment style we have entered so that only
	     the comment-end sequence of the same style actually
	     terminates the comment section.  */
	  state.comstyle = ST_COMMENT_STYLE;
	  state.incomment = -1;
	  state.comstr_start = prev_from;
	  code = Scomment;
	}
      else if (code == Scomment)
	{
	  state.comstyle = SYNTAX_FLAGS_COMMENT_STYLE (prev_from_syntax, 0);
	  state.incomment = (SYNTAX_FLAGS_COMMENT_NESTED (prev_from_syntax) ?
			     1 : -1);
	  state.comstr_start = prev_from;
	}

      if (SYNTAX_FLAGS_PREFIX (prev_from_syntax))
	continue;
      switch (SWITCH_ENUM_CAST (code))
	{
	case Sescape:
	case Scharquote:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = prev_from;
	startquoted:
	  if (from == end) goto endquoted;
	  INC_FROM;
	  goto symstarted;
	  /* treat following character as a word constituent */
	case Sword:
	case Ssymbol:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = prev_from;
	symstarted:
	  while (from < end)
	    {
	      /* Some compilers can't handle this inside the switch.  */
	      temp = FETCH_CHAR_AS_MULTIBYTE (from_byte);
	      temp = SYNTAX (temp);
	      switch (temp)
		{
		case Scharquote:
		case Sescape:
		  INC_FROM;
		  if (from == end) goto endquoted;
		  break;
		case Sword:
		case Ssymbol:
		case Squote:
		  break;
		default:
		  goto symdone;
		}
	      INC_FROM;
	    }
	symdone:
	  curlevel->prev = curlevel->last;
	  break;

	case Scomment_fence: /* Can't happen because it's handled above.  */
	case Scomment:
	  if (commentstop || boundary_stop) goto done;
	startincomment:
	  /* The (from == BEGV) test was to enter the loop in the middle so
	     that we find a 2-char comment ender even if we start in the
	     middle of it.  We don't want to do that if we're just at the
	     beginning of the comment (think of (*) ... (*)).  */
	  found = forw_comment (from, from_byte, end,
				state.incomment, state.comstyle,
				(from == BEGV || from < state.comstr_start + 3)
				? 0 : prev_from_syntax,
				&out_charpos, &out_bytepos, &state.incomment);
	  from = out_charpos; from_byte = out_bytepos;
	  /* Beware!  prev_from and friends are invalid now.
	     Luckily, the `done' doesn't use them and the INC_FROM
	     sets them to a sane value without looking at them. */
	  if (!found) goto done;
	  INC_FROM;
	  state.incomment = 0;
	  state.comstyle = 0;	/* reset the comment style */
	  if (boundary_stop) goto done;
	  break;

	case Sopen:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  depth++;
	  /* curlevel++->last ran into compiler bug on Apollo */
	  curlevel->last = prev_from;
	  if (++curlevel == endlevel)
	    curlevel--; /* error ("Nesting too deep for parser"); */
	  curlevel->prev = -1;
	  curlevel->last = -1;
	  if (targetdepth == depth) goto done;
	  break;

	case Sclose:
	  depth--;
	  if (depth < mindepth)
	    mindepth = depth;
	  if (curlevel != levelstart)
	    curlevel--;
	  curlevel->prev = curlevel->last;
	  if (targetdepth == depth) goto done;
	  break;

	case Sstring:
	case Sstring_fence:
	  state.comstr_start = from - 1;
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = prev_from;
	  state.instring = (code == Sstring
			    ? (FETCH_CHAR_AS_MULTIBYTE (prev_from_byte))
			    : ST_STRING_STYLE);
	  if (boundary_stop) goto done;
	startinstring:
	  {
	    nofence = state.instring != ST_STRING_STYLE;

	    while (1)
	      {
		int c;

		if (from >= end) goto done;
		c = FETCH_CHAR_AS_MULTIBYTE (from_byte);
		/* Some compilers can't handle this inside the switch.  */
		temp = SYNTAX (c);

		/* Check TEMP here so that if the char has
		   a syntax-table property which says it is NOT
		   a string character, it does not end the string.  */
		if (nofence && c == state.instring && temp == Sstring)
		  break;

		switch (temp)
		  {
		  case Sstring_fence:
		    if (!nofence) goto string_end;
		    break;
		  case Scharquote:
		  case Sescape:
		    INC_FROM;
		  startquotedinstring:
		    if (from >= end) goto endquoted;
		  }
		INC_FROM;
	      }
	  }
	string_end:
	  state.instring = -1;
	  curlevel->prev = curlevel->last;
	  INC_FROM;
	  if (boundary_stop) goto done;
	  break;

	case Smath:
	  /* FIXME: We should do something with it.  */
	  break;
	default:
	  /* Ignore whitespace, punctuation, quote, endcomment.  */
	  break;
	}
    }
  goto done;

 stop:   /* Here if stopping before start of sexp. */
  from = prev_from;    /* We have just fetched the char that starts it; */
  goto done; /* but return the position before it. */

 endquoted:
  state.quoted = 1;
 done:
  state.depth = depth;
  state.mindepth = mindepth;
  state.thislevelstart = curlevel->prev;
  state.prevlevelstart
    = (curlevel == levelstart) ? -1 : (curlevel - 1)->last;
  state.location = from;
  state.levelstarts = Qnil;
  while (curlevel > levelstart)
    state.levelstarts = Fcons (make_number ((--curlevel)->last),
			       state.levelstarts);
  immediate_quit = 0;

  *stateptr = state;
}

DEFUN ("parse-partial-sexp", Fparse_partial_sexp, Sparse_partial_sexp, 2, 6, 0,
       doc: /* Parse Lisp syntax starting at FROM until TO; return status of parse at TO.
Parsing stops at TO or when certain criteria are met;
 point is set to where parsing stops.
If fifth arg OLDSTATE is omitted or nil,
 parsing assumes that FROM is the beginning of a function.
Value is a list of elements describing final state of parsing:
 0. depth in parens.
 1. character address of start of innermost containing list; nil if none.
 2. character address of start of last complete sexp terminated.
 3. non-nil if inside a string.
    (it is the character that will terminate the string,
     or t if the string should be terminated by a generic string delimiter.)
 4. nil if outside a comment, t if inside a non-nestable comment,
    else an integer (the current comment nesting).
 5. t if following a quote character.
 6. the minimum paren-depth encountered during this scan.
 7. style of comment, if any.
 8. character address of start of comment or string; nil if not in one.
 9. Intermediate data for continuation of parsing (subject to change).
If third arg TARGETDEPTH is non-nil, parsing stops if the depth
in parentheses becomes equal to TARGETDEPTH.
Fourth arg STOPBEFORE non-nil means stop when come to
 any character that starts a sexp.
Fifth arg OLDSTATE is a list like what this function returns.
 It is used to initialize the state of the parse.  Elements number 1, 2, 6
 and 8 are ignored.
Sixth arg COMMENTSTOP non-nil means stop at the start of a comment.
 If it is symbol `syntax-table', stop after the start of a comment or a
 string, or after end of a comment or a string.  */)
  (Lisp_Object from, Lisp_Object to, Lisp_Object targetdepth, Lisp_Object stopbefore, Lisp_Object oldstate, Lisp_Object commentstop)
{
  struct lisp_parse_state state;
  int target;

  if (!NILP (targetdepth))
    {
      CHECK_NUMBER (targetdepth);
      target = XINT (targetdepth);
    }
  else
    target = -100000;		/* We won't reach this depth */

  validate_region (&from, &to);
  scan_sexps_forward (&state, XINT (from), CHAR_TO_BYTE (XINT (from)),
		      XINT (to),
		      target, !NILP (stopbefore), oldstate,
		      (NILP (commentstop)
		       ? 0 : (EQ (commentstop, Qsyntax_table) ? -1 : 1)));

  SET_PT (state.location);

  return Fcons (make_number (state.depth),
	   Fcons (state.prevlevelstart < 0
		  ? Qnil : make_number (state.prevlevelstart),
	     Fcons (state.thislevelstart < 0
		    ? Qnil : make_number (state.thislevelstart),
	       Fcons (state.instring >= 0
		      ? (state.instring == ST_STRING_STYLE
			 ? Qt : make_number (state.instring)) : Qnil,
		 Fcons (state.incomment < 0 ? Qt :
			(state.incomment == 0 ? Qnil :
			 make_number (state.incomment)),
		   Fcons (state.quoted ? Qt : Qnil,
		     Fcons (make_number (state.mindepth),
		       Fcons ((state.comstyle
			       ? (state.comstyle == ST_COMMENT_STYLE
				  ? Qsyntax_table
				  : make_number (state.comstyle))
			       : Qnil),
			      Fcons (((state.incomment
				       || (state.instring >= 0))
				      ? make_number (state.comstr_start)
				      : Qnil),
				     Fcons (state.levelstarts, Qnil))))))))));
}

void
init_syntax_once (void)
{
  register int i, c;
  Lisp_Object temp;

  /* This has to be done here, before we call Fmake_char_table.  */
  DEFSYM (Qsyntax_table, "syntax-table");

  /* Intern_C_String this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern_c_string ("char-table-extra-slots");

  /* Create objects which can be shared among syntax tables.  */
  Vsyntax_code_object = Fmake_vector (make_number (Smax), Qnil);
  for (i = 0; i < ASIZE (Vsyntax_code_object); i++)
    XVECTOR (Vsyntax_code_object)->contents[i]
      = Fcons (make_number (i), Qnil);

  /* Now we are ready to set up this property, so we can
     create syntax tables.  */
  Fput (Qsyntax_table, Qchar_table_extra_slots, make_number (0));

  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Swhitespace];

  Vstandard_syntax_table = Fmake_char_table (Qsyntax_table, temp);

  /* Control characters should not be whitespace.  */
  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Spunct];
  for (i = 0; i <= ' ' - 1; i++)
    SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, i, temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, 0177, temp);

  /* Except that a few really are whitespace.  */
  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Swhitespace];
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, ' ', temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '\t', temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '\n', temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, 015, temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, 014, temp);

  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Sword];
  for (i = 'a'; i <= 'z'; i++)
    SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, i, temp);
  for (i = 'A'; i <= 'Z'; i++)
    SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, i, temp);
  for (i = '0'; i <= '9'; i++)
    SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, i, temp);

  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '$', temp);
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '%', temp);

  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '(',
			Fcons (make_number (Sopen), make_number (')')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, ')',
			Fcons (make_number (Sclose), make_number ('(')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '[',
			Fcons (make_number (Sopen), make_number (']')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, ']',
			Fcons (make_number (Sclose), make_number ('[')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '{',
			Fcons (make_number (Sopen), make_number ('}')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '}',
			Fcons (make_number (Sclose), make_number ('{')));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '"',
			Fcons (make_number ((int) Sstring), Qnil));
  SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, '\\',
			Fcons (make_number ((int) Sescape), Qnil));

  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Ssymbol];
  for (i = 0; i < 10; i++)
    {
      c = "_-+*/&|<>="[i];
      SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, c, temp);
    }

  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Spunct];
  for (i = 0; i < 12; i++)
    {
      c = ".,;:?!#@~^'`"[i];
      SET_RAW_SYNTAX_ENTRY (Vstandard_syntax_table, c, temp);
    }

  /* All multibyte characters have syntax `word' by default.  */
  temp = XVECTOR (Vsyntax_code_object)->contents[(int) Sword];
  char_table_set_range (Vstandard_syntax_table, 0x80, MAX_CHAR, temp);
}

void
syms_of_syntax (void)
{
  DEFSYM (Qsyntax_table_p, "syntax-table-p");

  staticpro (&Vsyntax_code_object);

  staticpro (&gl_state.object);
  staticpro (&gl_state.global_code);
  staticpro (&gl_state.current_syntax_table);
  staticpro (&gl_state.old_prop);

  /* Defined in regex.c */
  staticpro (&re_match_object);

  DEFSYM (Qscan_error, "scan-error");
  Fput (Qscan_error, Qerror_conditions,
	pure_cons (Qscan_error, pure_cons (Qerror, Qnil)));
  Fput (Qscan_error, Qerror_message,
	make_pure_c_string ("Scan error"));

  DEFVAR_BOOL ("parse-sexp-ignore-comments", parse_sexp_ignore_comments,
	       doc: /* Non-nil means `forward-sexp', etc., should treat comments as whitespace.  */);

  DEFVAR_BOOL ("parse-sexp-lookup-properties", parse_sexp_lookup_properties,
	       doc: /* Non-nil means `forward-sexp', etc., obey `syntax-table' property.
Otherwise, that text property is simply ignored.
See the info node `(elisp)Syntax Properties' for a description of the
`syntax-table' property.  */);

  words_include_escapes = 0;
  DEFVAR_BOOL ("words-include-escapes", words_include_escapes,
	       doc: /* Non-nil means `forward-word', etc., should treat escape chars part of words.  */);

  DEFVAR_BOOL ("multibyte-syntax-as-symbol", multibyte_syntax_as_symbol,
	       doc: /* Non-nil means `scan-sexps' treats all multibyte characters as symbol.  */);
  multibyte_syntax_as_symbol = 0;

  DEFVAR_BOOL ("open-paren-in-column-0-is-defun-start",
	       open_paren_in_column_0_is_defun_start,
	       doc: /* *Non-nil means an open paren in column 0 denotes the start of a defun.  */);
  open_paren_in_column_0_is_defun_start = 1;


  DEFVAR_LISP ("find-word-boundary-function-table",
	       Vfind_word_boundary_function_table,
	       doc: /*
Char table of functions to search for the word boundary.
Each function is called with two arguments; POS and LIMIT.
POS and LIMIT are character positions in the current buffer.

If POS is less than LIMIT, POS is at the first character of a word,
and the return value of a function is a position after the last
character of that word.

If POS is not less than LIMIT, POS is at the last character of a word,
and the return value of a function is a position at the first
character of that word.

In both cases, LIMIT bounds the search. */);
  Vfind_word_boundary_function_table = Fmake_char_table (Qnil, Qnil);

  defsubr (&Ssyntax_table_p);
  defsubr (&Ssyntax_table);
  defsubr (&Sstandard_syntax_table);
  defsubr (&Scopy_syntax_table);
  defsubr (&Sset_syntax_table);
  defsubr (&Schar_syntax);
  defsubr (&Smatching_paren);
  defsubr (&Sstring_to_syntax);
  defsubr (&Smodify_syntax_entry);
  defsubr (&Sinternal_describe_syntax_value);

  defsubr (&Sforward_word);

  defsubr (&Sskip_chars_forward);
  defsubr (&Sskip_chars_backward);
  defsubr (&Sskip_syntax_forward);
  defsubr (&Sskip_syntax_backward);

  defsubr (&Sforward_comment);
  defsubr (&Sscan_lists);
  defsubr (&Sscan_sexps);
  defsubr (&Sbackward_prefix_chars);
  defsubr (&Sparse_partial_sexp);
}
