/* String search routines for GNU Emacs.

Copyright (C) 1985-1987, 1993-1994, 1997-1999, 2001-2012
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
#include <setjmp.h>
#include "lisp.h"
#include "syntax.h"
#include "category.h"
#include "buffer.h"
#include "character.h"
#include "charset.h"
#include "region-cache.h"
#include "commands.h"
#include "blockinput.h"
#include "intervals.h"

#include <sys/types.h>
#include "regex.h"

#define REGEXP_CACHE_SIZE 20

/* If the regexp is non-nil, then the buffer contains the compiled form
   of that regexp, suitable for searching.  */
struct regexp_cache
{
  struct regexp_cache *next;
  Lisp_Object regexp, whitespace_regexp;
  /* Syntax table for which the regexp applies.  We need this because
     of character classes.  If this is t, then the compiled pattern is valid
     for any syntax-table.  */
  Lisp_Object syntax_table;
  struct re_pattern_buffer buf;
  char fastmap[0400];
  /* Nonzero means regexp was compiled to do full POSIX backtracking.  */
  char posix;
};

/* The instances of that struct.  */
static struct regexp_cache searchbufs[REGEXP_CACHE_SIZE];

/* The head of the linked list; points to the most recently used buffer.  */
static struct regexp_cache *searchbuf_head;


/* Every call to re_match, etc., must pass &search_regs as the regs
   argument unless you can show it is unnecessary (i.e., if re_match
   is certainly going to be called again before region-around-match
   can be called).

   Since the registers are now dynamically allocated, we need to make
   sure not to refer to the Nth register before checking that it has
   been allocated by checking search_regs.num_regs.

   The regex code keeps track of whether it has allocated the search
   buffer using bits in the re_pattern_buffer.  This means that whenever
   you compile a new pattern, it completely forgets whether it has
   allocated any registers, and will allocate new registers the next
   time you call a searching or matching function.  Therefore, we need
   to call re_set_registers after compiling a new pattern or after
   setting the match registers, so that the regex functions will be
   able to free or re-allocate it properly.  */
static struct re_registers search_regs;

/* The buffer in which the last search was performed, or
   Qt if the last search was done in a string;
   Qnil if no searching has been done yet.  */
static Lisp_Object last_thing_searched;

/* Error condition signaled when regexp compile_pattern fails.  */
static Lisp_Object Qinvalid_regexp;

/* Error condition used for failing searches.  */
static Lisp_Object Qsearch_failed;

static void set_search_regs (EMACS_INT, EMACS_INT);
static void save_search_regs (void);
static EMACS_INT simple_search (EMACS_INT, unsigned char *, EMACS_INT,
				EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT,
                                EMACS_INT, EMACS_INT);
static EMACS_INT boyer_moore (EMACS_INT, unsigned char *, EMACS_INT,
                              Lisp_Object, Lisp_Object, EMACS_INT,
                              EMACS_INT, int);
static EMACS_INT search_buffer (Lisp_Object, EMACS_INT, EMACS_INT,
                                EMACS_INT, EMACS_INT, EMACS_INT, int,
                                Lisp_Object, Lisp_Object, int);
static void matcher_overflow (void) NO_RETURN;

static void
matcher_overflow (void)
{
  error ("Stack overflow in regexp matcher");
}

/* Compile a regexp and signal a Lisp error if anything goes wrong.
   PATTERN is the pattern to compile.
   CP is the place to put the result.
   TRANSLATE is a translation table for ignoring case, or nil for none.
   POSIX is nonzero if we want full backtracking (POSIX style)
   for this pattern.  0 means backtrack only enough to get a valid match.

   The behavior also depends on Vsearch_spaces_regexp.  */

static void
compile_pattern_1 (struct regexp_cache *cp, Lisp_Object pattern, Lisp_Object translate, int posix)
{
  char *val;
  reg_syntax_t old;

  cp->regexp = Qnil;
  cp->buf.translate = (! NILP (translate) ? translate : make_number (0));
  cp->posix = posix;
  cp->buf.multibyte = STRING_MULTIBYTE (pattern);
  cp->buf.charset_unibyte = charset_unibyte;
  if (STRINGP (Vsearch_spaces_regexp))
    cp->whitespace_regexp = Vsearch_spaces_regexp;
  else
    cp->whitespace_regexp = Qnil;

  /* rms: I think BLOCK_INPUT is not needed here any more,
     because regex.c defines malloc to call xmalloc.
     Using BLOCK_INPUT here means the debugger won't run if an error occurs.
     So let's turn it off.  */
  /*  BLOCK_INPUT;  */
  old = re_set_syntax (RE_SYNTAX_EMACS
		       | (posix ? 0 : RE_NO_POSIX_BACKTRACKING));

  if (STRINGP (Vsearch_spaces_regexp))
    re_set_whitespace_regexp (SSDATA (Vsearch_spaces_regexp));
  else
    re_set_whitespace_regexp (NULL);

  val = (char *) re_compile_pattern (SSDATA (pattern),
				     SBYTES (pattern), &cp->buf);

  /* If the compiled pattern hard codes some of the contents of the
     syntax-table, it can only be reused with *this* syntax table.  */
  cp->syntax_table = cp->buf.used_syntax ? BVAR (current_buffer, syntax_table) : Qt;

  re_set_whitespace_regexp (NULL);

  re_set_syntax (old);
  /* UNBLOCK_INPUT;  */
  if (val)
    xsignal1 (Qinvalid_regexp, build_string (val));

  cp->regexp = Fcopy_sequence (pattern);
}

/* Shrink each compiled regexp buffer in the cache
   to the size actually used right now.
   This is called from garbage collection.  */

void
shrink_regexp_cache (void)
{
  struct regexp_cache *cp;

  for (cp = searchbuf_head; cp != 0; cp = cp->next)
    {
      cp->buf.allocated = cp->buf.used;
      cp->buf.buffer
	= (unsigned char *) xrealloc (cp->buf.buffer, cp->buf.used);
    }
}

/* Clear the regexp cache w.r.t. a particular syntax table,
   because it was changed.
   There is no danger of memory leak here because re_compile_pattern
   automagically manages the memory in each re_pattern_buffer struct,
   based on its `allocated' and `buffer' values.  */
void
clear_regexp_cache (void)
{
  int i;

  for (i = 0; i < REGEXP_CACHE_SIZE; ++i)
    /* It's tempting to compare with the syntax-table we've actually changed,
       but it's not sufficient because char-table inheritance means that
       modifying one syntax-table can change others at the same time.  */
    if (!EQ (searchbufs[i].syntax_table, Qt))
      searchbufs[i].regexp = Qnil;
}

/* Compile a regexp if necessary, but first check to see if there's one in
   the cache.
   PATTERN is the pattern to compile.
   TRANSLATE is a translation table for ignoring case, or nil for none.
   REGP is the structure that says where to store the "register"
   values that will result from matching this pattern.
   If it is 0, we should compile the pattern not to record any
   subexpression bounds.
   POSIX is nonzero if we want full backtracking (POSIX style)
   for this pattern.  0 means backtrack only enough to get a valid match.  */

struct re_pattern_buffer *
compile_pattern (Lisp_Object pattern, struct re_registers *regp, Lisp_Object translate, int posix, int multibyte)
{
  struct regexp_cache *cp, **cpp;

  for (cpp = &searchbuf_head; ; cpp = &cp->next)
    {
      cp = *cpp;
      /* Entries are initialized to nil, and may be set to nil by
	 compile_pattern_1 if the pattern isn't valid.  Don't apply
	 string accessors in those cases.  However, compile_pattern_1
	 is only applied to the cache entry we pick here to reuse.  So
	 nil should never appear before a non-nil entry.  */
      if (NILP (cp->regexp))
	goto compile_it;
      if (SCHARS (cp->regexp) == SCHARS (pattern)
	  && STRING_MULTIBYTE (cp->regexp) == STRING_MULTIBYTE (pattern)
	  && !NILP (Fstring_equal (cp->regexp, pattern))
	  && EQ (cp->buf.translate, (! NILP (translate) ? translate : make_number (0)))
	  && cp->posix == posix
	  && (EQ (cp->syntax_table, Qt)
	      || EQ (cp->syntax_table, BVAR (current_buffer, syntax_table)))
	  && !NILP (Fequal (cp->whitespace_regexp, Vsearch_spaces_regexp))
	  && cp->buf.charset_unibyte == charset_unibyte)
	break;

      /* If we're at the end of the cache, compile into the nil cell
	 we found, or the last (least recently used) cell with a
	 string value.  */
      if (cp->next == 0)
	{
	compile_it:
	  compile_pattern_1 (cp, pattern, translate, posix);
	  break;
	}
    }

  /* When we get here, cp (aka *cpp) contains the compiled pattern,
     either because we found it in the cache or because we just compiled it.
     Move it to the front of the queue to mark it as most recently used.  */
  *cpp = cp->next;
  cp->next = searchbuf_head;
  searchbuf_head = cp;

  /* Advise the searching functions about the space we have allocated
     for register data.  */
  if (regp)
    re_set_registers (&cp->buf, regp, regp->num_regs, regp->start, regp->end);

  /* The compiled pattern can be used both for multibyte and unibyte
     target.  But, we have to tell which the pattern is used for. */
  cp->buf.target_multibyte = multibyte;

  return &cp->buf;
}


static Lisp_Object
looking_at_1 (Lisp_Object string, int posix)
{
  Lisp_Object val;
  unsigned char *p1, *p2;
  EMACS_INT s1, s2;
  register EMACS_INT i;
  struct re_pattern_buffer *bufp;

  if (running_asynch_code)
    save_search_regs ();

  /* This is so set_image_of_range_1 in regex.c can find the EQV table.  */
  XCHAR_TABLE (BVAR (current_buffer, case_canon_table))->extras[2]
    = BVAR (current_buffer, case_eqv_table);

  CHECK_STRING (string);
  bufp = compile_pattern (string,
			  (NILP (Vinhibit_changing_match_data)
			   ? &search_regs : NULL),
			  (!NILP (BVAR (current_buffer, case_fold_search))
			   ? BVAR (current_buffer, case_canon_table) : Qnil),
			  posix,
			  !NILP (BVAR (current_buffer, enable_multibyte_characters)));

  immediate_quit = 1;
  QUIT;			/* Do a pending quit right away, to avoid paradoxical behavior */

  /* Get pointers and sizes of the two strings
     that make up the visible portion of the buffer. */

  p1 = BEGV_ADDR;
  s1 = GPT_BYTE - BEGV_BYTE;
  p2 = GAP_END_ADDR;
  s2 = ZV_BYTE - GPT_BYTE;
  if (s1 < 0)
    {
      p2 = p1;
      s2 = ZV_BYTE - BEGV_BYTE;
      s1 = 0;
    }
  if (s2 < 0)
    {
      s1 = ZV_BYTE - BEGV_BYTE;
      s2 = 0;
    }

  re_match_object = Qnil;

  i = re_match_2 (bufp, (char *) p1, s1, (char *) p2, s2,
		  PT_BYTE - BEGV_BYTE,
		  (NILP (Vinhibit_changing_match_data)
		   ? &search_regs : NULL),
		  ZV_BYTE - BEGV_BYTE);
  immediate_quit = 0;

  if (i == -2)
    matcher_overflow ();

  val = (0 <= i ? Qt : Qnil);
  if (NILP (Vinhibit_changing_match_data) && i >= 0)
    for (i = 0; i < search_regs.num_regs; i++)
      if (search_regs.start[i] >= 0)
	{
	  search_regs.start[i]
	    = BYTE_TO_CHAR (search_regs.start[i] + BEGV_BYTE);
	  search_regs.end[i]
	    = BYTE_TO_CHAR (search_regs.end[i] + BEGV_BYTE);
	}

  /* Set last_thing_searched only when match data is changed.  */
  if (NILP (Vinhibit_changing_match_data))
    XSETBUFFER (last_thing_searched, current_buffer);

  return val;
}

DEFUN ("looking-at", Flooking_at, Slooking_at, 1, 1, 0,
       doc: /* Return t if text after point matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them.  */)
  (Lisp_Object regexp)
{
  return looking_at_1 (regexp, 0);
}

DEFUN ("posix-looking-at", Fposix_looking_at, Sposix_looking_at, 1, 1, 0,
       doc: /* Return t if text after point matches regular expression REGEXP.
Find the longest match, in accord with Posix regular expression rules.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them.  */)
  (Lisp_Object regexp)
{
  return looking_at_1 (regexp, 1);
}

static Lisp_Object
string_match_1 (Lisp_Object regexp, Lisp_Object string, Lisp_Object start, int posix)
{
  EMACS_INT val;
  struct re_pattern_buffer *bufp;
  EMACS_INT pos, pos_byte;
  int i;

  if (running_asynch_code)
    save_search_regs ();

  CHECK_STRING (regexp);
  CHECK_STRING (string);

  if (NILP (start))
    pos = 0, pos_byte = 0;
  else
    {
      EMACS_INT len = SCHARS (string);

      CHECK_NUMBER (start);
      pos = XINT (start);
      if (pos < 0 && -pos <= len)
	pos = len + pos;
      else if (0 > pos || pos > len)
	args_out_of_range (string, start);
      pos_byte = string_char_to_byte (string, pos);
    }

  /* This is so set_image_of_range_1 in regex.c can find the EQV table.  */
  XCHAR_TABLE (BVAR (current_buffer, case_canon_table))->extras[2]
    = BVAR (current_buffer, case_eqv_table);

  bufp = compile_pattern (regexp,
			  (NILP (Vinhibit_changing_match_data)
			   ? &search_regs : NULL),
			  (!NILP (BVAR (current_buffer, case_fold_search))
			   ? BVAR (current_buffer, case_canon_table) : Qnil),
			  posix,
			  STRING_MULTIBYTE (string));
  immediate_quit = 1;
  re_match_object = string;

  val = re_search (bufp, SSDATA (string),
		   SBYTES (string), pos_byte,
		   SBYTES (string) - pos_byte,
		   (NILP (Vinhibit_changing_match_data)
		    ? &search_regs : NULL));
  immediate_quit = 0;

  /* Set last_thing_searched only when match data is changed.  */
  if (NILP (Vinhibit_changing_match_data))
    last_thing_searched = Qt;

  if (val == -2)
    matcher_overflow ();
  if (val < 0) return Qnil;

  if (NILP (Vinhibit_changing_match_data))
    for (i = 0; i < search_regs.num_regs; i++)
      if (search_regs.start[i] >= 0)
	{
	  search_regs.start[i]
	    = string_byte_to_char (string, search_regs.start[i]);
	  search_regs.end[i]
	    = string_byte_to_char (string, search_regs.end[i]);
	}

  return make_number (string_byte_to_char (string, val));
}

DEFUN ("string-match", Fstring_match, Sstring_match, 2, 3, 0,
       doc: /* Return index of start of first match for REGEXP in STRING, or nil.
Matching ignores case if `case-fold-search' is non-nil.
If third arg START is non-nil, start search at that index in STRING.
For index of first char beyond the match, do (match-end 0).
`match-end' and `match-beginning' also give indices of substrings
matched by parenthesis constructs in the pattern.

You can use the function `match-string' to extract the substrings
matched by the parenthesis constructions in REGEXP. */)
  (Lisp_Object regexp, Lisp_Object string, Lisp_Object start)
{
  return string_match_1 (regexp, string, start, 0);
}

DEFUN ("posix-string-match", Fposix_string_match, Sposix_string_match, 2, 3, 0,
       doc: /* Return index of start of first match for REGEXP in STRING, or nil.
Find the longest match, in accord with Posix regular expression rules.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
If third arg START is non-nil, start search at that index in STRING.
For index of first char beyond the match, do (match-end 0).
`match-end' and `match-beginning' also give indices of substrings
matched by parenthesis constructs in the pattern.  */)
  (Lisp_Object regexp, Lisp_Object string, Lisp_Object start)
{
  return string_match_1 (regexp, string, start, 1);
}

/* Match REGEXP against STRING, searching all of STRING,
   and return the index of the match, or negative on failure.
   This does not clobber the match data.  */

EMACS_INT
fast_string_match (Lisp_Object regexp, Lisp_Object string)
{
  EMACS_INT val;
  struct re_pattern_buffer *bufp;

  bufp = compile_pattern (regexp, 0, Qnil,
			  0, STRING_MULTIBYTE (string));
  immediate_quit = 1;
  re_match_object = string;

  val = re_search (bufp, SSDATA (string),
		   SBYTES (string), 0,
		   SBYTES (string), 0);
  immediate_quit = 0;
  return val;
}

/* Match REGEXP against STRING, searching all of STRING ignoring case,
   and return the index of the match, or negative on failure.
   This does not clobber the match data.
   We assume that STRING contains single-byte characters.  */

EMACS_INT
fast_c_string_match_ignore_case (Lisp_Object regexp, const char *string)
{
  EMACS_INT val;
  struct re_pattern_buffer *bufp;
  size_t len = strlen (string);

  regexp = string_make_unibyte (regexp);
  re_match_object = Qt;
  bufp = compile_pattern (regexp, 0,
			  Vascii_canon_table, 0,
			  0);
  immediate_quit = 1;
  val = re_search (bufp, string, len, 0, len, 0);
  immediate_quit = 0;
  return val;
}

/* Like fast_string_match but ignore case.  */

EMACS_INT
fast_string_match_ignore_case (Lisp_Object regexp, Lisp_Object string)
{
  EMACS_INT val;
  struct re_pattern_buffer *bufp;

  bufp = compile_pattern (regexp, 0, Vascii_canon_table,
			  0, STRING_MULTIBYTE (string));
  immediate_quit = 1;
  re_match_object = string;

  val = re_search (bufp, SSDATA (string),
		   SBYTES (string), 0,
		   SBYTES (string), 0);
  immediate_quit = 0;
  return val;
}

/* Match REGEXP against the characters after POS to LIMIT, and return
   the number of matched characters.  If STRING is non-nil, match
   against the characters in it.  In that case, POS and LIMIT are
   indices into the string.  This function doesn't modify the match
   data.  */

EMACS_INT
fast_looking_at (Lisp_Object regexp, EMACS_INT pos, EMACS_INT pos_byte, EMACS_INT limit, EMACS_INT limit_byte, Lisp_Object string)
{
  int multibyte;
  struct re_pattern_buffer *buf;
  unsigned char *p1, *p2;
  EMACS_INT s1, s2;
  EMACS_INT len;

  if (STRINGP (string))
    {
      if (pos_byte < 0)
	pos_byte = string_char_to_byte (string, pos);
      if (limit_byte < 0)
	limit_byte = string_char_to_byte (string, limit);
      p1 = NULL;
      s1 = 0;
      p2 = SDATA (string);
      s2 = SBYTES (string);
      re_match_object = string;
      multibyte = STRING_MULTIBYTE (string);
    }
  else
    {
      if (pos_byte < 0)
	pos_byte = CHAR_TO_BYTE (pos);
      if (limit_byte < 0)
	limit_byte = CHAR_TO_BYTE (limit);
      pos_byte -= BEGV_BYTE;
      limit_byte -= BEGV_BYTE;
      p1 = BEGV_ADDR;
      s1 = GPT_BYTE - BEGV_BYTE;
      p2 = GAP_END_ADDR;
      s2 = ZV_BYTE - GPT_BYTE;
      if (s1 < 0)
	{
	  p2 = p1;
	  s2 = ZV_BYTE - BEGV_BYTE;
	  s1 = 0;
	}
      if (s2 < 0)
	{
	  s1 = ZV_BYTE - BEGV_BYTE;
	  s2 = 0;
	}
      re_match_object = Qnil;
      multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));
    }

  buf = compile_pattern (regexp, 0, Qnil, 0, multibyte);
  immediate_quit = 1;
  len = re_match_2 (buf, (char *) p1, s1, (char *) p2, s2,
		    pos_byte, NULL, limit_byte);
  immediate_quit = 0;

  return len;
}


/* The newline cache: remembering which sections of text have no newlines.  */

/* If the user has requested newline caching, make sure it's on.
   Otherwise, make sure it's off.
   This is our cheezy way of associating an action with the change of
   state of a buffer-local variable.  */
static void
newline_cache_on_off (struct buffer *buf)
{
  if (NILP (BVAR (buf, cache_long_line_scans)))
    {
      /* It should be off.  */
      if (buf->newline_cache)
        {
          free_region_cache (buf->newline_cache);
          buf->newline_cache = 0;
        }
    }
  else
    {
      /* It should be on.  */
      if (buf->newline_cache == 0)
        buf->newline_cache = new_region_cache ();
    }
}


/* Search for COUNT instances of the character TARGET between START and END.

   If COUNT is positive, search forwards; END must be >= START.
   If COUNT is negative, search backwards for the -COUNTth instance;
      END must be <= START.
   If COUNT is zero, do anything you please; run rogue, for all I care.

   If END is zero, use BEGV or ZV instead, as appropriate for the
   direction indicated by COUNT.

   If we find COUNT instances, set *SHORTAGE to zero, and return the
   position past the COUNTth match.  Note that for reverse motion
   this is not the same as the usual convention for Emacs motion commands.

   If we don't find COUNT instances before reaching END, set *SHORTAGE
   to the number of TARGETs left unfound, and return END.

   If ALLOW_QUIT is non-zero, set immediate_quit.  That's good to do
   except when inside redisplay.  */

EMACS_INT
scan_buffer (register int target, EMACS_INT start, EMACS_INT end,
	     EMACS_INT count, EMACS_INT *shortage, int allow_quit)
{
  struct region_cache *newline_cache;
  int direction;

  if (count > 0)
    {
      direction = 1;
      if (! end) end = ZV;
    }
  else
    {
      direction = -1;
      if (! end) end = BEGV;
    }

  newline_cache_on_off (current_buffer);
  newline_cache = current_buffer->newline_cache;

  if (shortage != 0)
    *shortage = 0;

  immediate_quit = allow_quit;

  if (count > 0)
    while (start != end)
      {
        /* Our innermost scanning loop is very simple; it doesn't know
           about gaps, buffer ends, or the newline cache.  ceiling is
           the position of the last character before the next such
           obstacle --- the last character the dumb search loop should
           examine.  */
	EMACS_INT ceiling_byte = CHAR_TO_BYTE (end) - 1;
	EMACS_INT start_byte = CHAR_TO_BYTE (start);
	EMACS_INT tem;

        /* If we're looking for a newline, consult the newline cache
           to see where we can avoid some scanning.  */
        if (target == '\n' && newline_cache)
          {
            ptrdiff_t next_change;
            immediate_quit = 0;
            while (region_cache_forward
                   (current_buffer, newline_cache, start_byte, &next_change))
              start_byte = next_change;
            immediate_quit = allow_quit;

            /* START should never be after END.  */
            if (start_byte > ceiling_byte)
              start_byte = ceiling_byte;

            /* Now the text after start is an unknown region, and
               next_change is the position of the next known region. */
            ceiling_byte = min (next_change - 1, ceiling_byte);
          }

        /* The dumb loop can only scan text stored in contiguous
           bytes. BUFFER_CEILING_OF returns the last character
           position that is contiguous, so the ceiling is the
           position after that.  */
	tem = BUFFER_CEILING_OF (start_byte);
	ceiling_byte = min (tem, ceiling_byte);

        {
          /* The termination address of the dumb loop.  */
          register unsigned char *ceiling_addr
	    = BYTE_POS_ADDR (ceiling_byte) + 1;
          register unsigned char *cursor
	    = BYTE_POS_ADDR (start_byte);
          unsigned char *base = cursor;

          while (cursor < ceiling_addr)
            {
              unsigned char *scan_start = cursor;

              /* The dumb loop.  */
              while (*cursor != target && ++cursor < ceiling_addr)
                ;

              /* If we're looking for newlines, cache the fact that
                 the region from start to cursor is free of them. */
              if (target == '\n' && newline_cache)
                know_region_cache (current_buffer, newline_cache,
                                   BYTE_TO_CHAR (start_byte + scan_start - base),
                                   BYTE_TO_CHAR (start_byte + cursor - base));

              /* Did we find the target character?  */
              if (cursor < ceiling_addr)
                {
                  if (--count == 0)
                    {
                      immediate_quit = 0;
                      return BYTE_TO_CHAR (start_byte + cursor - base + 1);
                    }
                  cursor++;
                }
            }

          start = BYTE_TO_CHAR (start_byte + cursor - base);
        }
      }
  else
    while (start > end)
      {
        /* The last character to check before the next obstacle.  */
	EMACS_INT ceiling_byte = CHAR_TO_BYTE (end);
	EMACS_INT start_byte = CHAR_TO_BYTE (start);
	EMACS_INT tem;

        /* Consult the newline cache, if appropriate.  */
        if (target == '\n' && newline_cache)
          {
            ptrdiff_t next_change;
            immediate_quit = 0;
            while (region_cache_backward
                   (current_buffer, newline_cache, start_byte, &next_change))
              start_byte = next_change;
            immediate_quit = allow_quit;

            /* Start should never be at or before end.  */
            if (start_byte <= ceiling_byte)
              start_byte = ceiling_byte + 1;

            /* Now the text before start is an unknown region, and
               next_change is the position of the next known region. */
            ceiling_byte = max (next_change, ceiling_byte);
          }

        /* Stop scanning before the gap.  */
	tem = BUFFER_FLOOR_OF (start_byte - 1);
	ceiling_byte = max (tem, ceiling_byte);

        {
          /* The termination address of the dumb loop.  */
          register unsigned char *ceiling_addr = BYTE_POS_ADDR (ceiling_byte);
          register unsigned char *cursor = BYTE_POS_ADDR (start_byte - 1);
          unsigned char *base = cursor;

          while (cursor >= ceiling_addr)
            {
              unsigned char *scan_start = cursor;

              while (*cursor != target && --cursor >= ceiling_addr)
                ;

              /* If we're looking for newlines, cache the fact that
                 the region from after the cursor to start is free of them.  */
              if (target == '\n' && newline_cache)
                know_region_cache (current_buffer, newline_cache,
                                   BYTE_TO_CHAR (start_byte + cursor - base),
                                   BYTE_TO_CHAR (start_byte + scan_start - base));

              /* Did we find the target character?  */
              if (cursor >= ceiling_addr)
                {
                  if (++count >= 0)
                    {
                      immediate_quit = 0;
                      return BYTE_TO_CHAR (start_byte + cursor - base);
                    }
                  cursor--;
                }
            }

	  start = BYTE_TO_CHAR (start_byte + cursor - base);
        }
      }

  immediate_quit = 0;
  if (shortage != 0)
    *shortage = count * direction;
  return start;
}

/* Search for COUNT instances of a line boundary, which means either a
   newline or (if selective display enabled) a carriage return.
   Start at START.  If COUNT is negative, search backwards.

   We report the resulting position by calling TEMP_SET_PT_BOTH.

   If we find COUNT instances. we position after (always after,
   even if scanning backwards) the COUNTth match, and return 0.

   If we don't find COUNT instances before reaching the end of the
   buffer (or the beginning, if scanning backwards), we return
   the number of line boundaries left unfound, and position at
   the limit we bumped up against.

   If ALLOW_QUIT is non-zero, set immediate_quit.  That's good to do
   except in special cases.  */

EMACS_INT
scan_newline (EMACS_INT start, EMACS_INT start_byte,
	      EMACS_INT limit, EMACS_INT limit_byte,
	      register EMACS_INT count, int allow_quit)
{
  int direction = ((count > 0) ? 1 : -1);

  register unsigned char *cursor;
  unsigned char *base;

  EMACS_INT ceiling;
  register unsigned char *ceiling_addr;

  int old_immediate_quit = immediate_quit;

  /* The code that follows is like scan_buffer
     but checks for either newline or carriage return.  */

  if (allow_quit)
    immediate_quit++;

  start_byte = CHAR_TO_BYTE (start);

  if (count > 0)
    {
      while (start_byte < limit_byte)
	{
	  ceiling =  BUFFER_CEILING_OF (start_byte);
	  ceiling = min (limit_byte - 1, ceiling);
	  ceiling_addr = BYTE_POS_ADDR (ceiling) + 1;
	  base = (cursor = BYTE_POS_ADDR (start_byte));
	  while (1)
	    {
	      while (*cursor != '\n' && ++cursor != ceiling_addr)
		;

	      if (cursor != ceiling_addr)
		{
		  if (--count == 0)
		    {
		      immediate_quit = old_immediate_quit;
		      start_byte = start_byte + cursor - base + 1;
		      start = BYTE_TO_CHAR (start_byte);
		      TEMP_SET_PT_BOTH (start, start_byte);
		      return 0;
		    }
		  else
		    if (++cursor == ceiling_addr)
		      break;
		}
	      else
		break;
	    }
	  start_byte += cursor - base;
	}
    }
  else
    {
      while (start_byte > limit_byte)
	{
	  ceiling = BUFFER_FLOOR_OF (start_byte - 1);
	  ceiling = max (limit_byte, ceiling);
	  ceiling_addr = BYTE_POS_ADDR (ceiling) - 1;
	  base = (cursor = BYTE_POS_ADDR (start_byte - 1) + 1);
	  while (1)
	    {
	      while (--cursor != ceiling_addr && *cursor != '\n')
		;

	      if (cursor != ceiling_addr)
		{
		  if (++count == 0)
		    {
		      immediate_quit = old_immediate_quit;
		      /* Return the position AFTER the match we found.  */
		      start_byte = start_byte + cursor - base + 1;
		      start = BYTE_TO_CHAR (start_byte);
		      TEMP_SET_PT_BOTH (start, start_byte);
		      return 0;
		    }
		}
	      else
		break;
	    }
	  /* Here we add 1 to compensate for the last decrement
	     of CURSOR, which took it past the valid range.  */
	  start_byte += cursor - base + 1;
	}
    }

  TEMP_SET_PT_BOTH (limit, limit_byte);
  immediate_quit = old_immediate_quit;

  return count * direction;
}

EMACS_INT
find_next_newline_no_quit (EMACS_INT from, EMACS_INT cnt)
{
  return scan_buffer ('\n', from, 0, cnt, (EMACS_INT *) 0, 0);
}

/* Like find_next_newline, but returns position before the newline,
   not after, and only search up to TO.  This isn't just
   find_next_newline (...)-1, because you might hit TO.  */

EMACS_INT
find_before_next_newline (EMACS_INT from, EMACS_INT to, EMACS_INT cnt)
{
  EMACS_INT shortage;
  EMACS_INT pos = scan_buffer ('\n', from, to, cnt, &shortage, 1);

  if (shortage == 0)
    pos--;

  return pos;
}

/* Subroutines of Lisp buffer search functions. */

static Lisp_Object
search_command (Lisp_Object string, Lisp_Object bound, Lisp_Object noerror,
		Lisp_Object count, int direction, int RE, int posix)
{
  register EMACS_INT np;
  EMACS_INT lim, lim_byte;
  EMACS_INT n = direction;

  if (!NILP (count))
    {
      CHECK_NUMBER (count);
      n *= XINT (count);
    }

  CHECK_STRING (string);
  if (NILP (bound))
    {
      if (n > 0)
	lim = ZV, lim_byte = ZV_BYTE;
      else
	lim = BEGV, lim_byte = BEGV_BYTE;
    }
  else
    {
      CHECK_NUMBER_COERCE_MARKER (bound);
      lim = XINT (bound);
      if (n > 0 ? lim < PT : lim > PT)
	error ("Invalid search bound (wrong side of point)");
      if (lim > ZV)
	lim = ZV, lim_byte = ZV_BYTE;
      else if (lim < BEGV)
	lim = BEGV, lim_byte = BEGV_BYTE;
      else
	lim_byte = CHAR_TO_BYTE (lim);
    }

  /* This is so set_image_of_range_1 in regex.c can find the EQV table.  */
  XCHAR_TABLE (BVAR (current_buffer, case_canon_table))->extras[2]
    = BVAR (current_buffer, case_eqv_table);

  np = search_buffer (string, PT, PT_BYTE, lim, lim_byte, n, RE,
		      (!NILP (BVAR (current_buffer, case_fold_search))
		       ? BVAR (current_buffer, case_canon_table)
		       : Qnil),
		      (!NILP (BVAR (current_buffer, case_fold_search))
		       ? BVAR (current_buffer, case_eqv_table)
		       : Qnil),
		      posix);
  if (np <= 0)
    {
      if (NILP (noerror))
	xsignal1 (Qsearch_failed, string);

      if (!EQ (noerror, Qt))
	{
	  if (lim < BEGV || lim > ZV)
	    abort ();
	  SET_PT_BOTH (lim, lim_byte);
	  return Qnil;
#if 0 /* This would be clean, but maybe programs depend on
	 a value of nil here.  */
	  np = lim;
#endif
	}
      else
	return Qnil;
    }

  if (np < BEGV || np > ZV)
    abort ();

  SET_PT (np);

  return make_number (np);
}

/* Return 1 if REGEXP it matches just one constant string.  */

static int
trivial_regexp_p (Lisp_Object regexp)
{
  EMACS_INT len = SBYTES (regexp);
  unsigned char *s = SDATA (regexp);
  while (--len >= 0)
    {
      switch (*s++)
	{
	case '.': case '*': case '+': case '?': case '[': case '^': case '$':
	  return 0;
	case '\\':
	  if (--len < 0)
	    return 0;
	  switch (*s++)
	    {
	    case '|': case '(': case ')': case '`': case '\'': case 'b':
	    case 'B': case '<': case '>': case 'w': case 'W': case 's':
	    case 'S': case '=': case '{': case '}': case '_':
	    case 'c': case 'C':	/* for categoryspec and notcategoryspec */
	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      return 0;
	    }
	}
    }
  return 1;
}

/* Search for the n'th occurrence of STRING in the current buffer,
   starting at position POS and stopping at position LIM,
   treating STRING as a literal string if RE is false or as
   a regular expression if RE is true.

   If N is positive, searching is forward and LIM must be greater than POS.
   If N is negative, searching is backward and LIM must be less than POS.

   Returns -x if x occurrences remain to be found (x > 0),
   or else the position at the beginning of the Nth occurrence
   (if searching backward) or the end (if searching forward).

   POSIX is nonzero if we want full backtracking (POSIX style)
   for this pattern.  0 means backtrack only enough to get a valid match.  */

#define TRANSLATE(out, trt, d)			\
do						\
  {						\
    if (! NILP (trt))				\
      {						\
	Lisp_Object temp;			\
	temp = Faref (trt, make_number (d));	\
	if (INTEGERP (temp))			\
	  out = XINT (temp);			\
	else					\
	  out = d;				\
      }						\
    else					\
      out = d;					\
  }						\
while (0)

/* Only used in search_buffer, to record the end position of the match
   when searching regexps and SEARCH_REGS should not be changed
   (i.e. Vinhibit_changing_match_data is non-nil).  */
static struct re_registers search_regs_1;

static EMACS_INT
search_buffer (Lisp_Object string, EMACS_INT pos, EMACS_INT pos_byte,
	       EMACS_INT lim, EMACS_INT lim_byte, EMACS_INT n,
	       int RE, Lisp_Object trt, Lisp_Object inverse_trt, int posix)
{
  EMACS_INT len = SCHARS (string);
  EMACS_INT len_byte = SBYTES (string);
  register int i;

  if (running_asynch_code)
    save_search_regs ();

  /* Searching 0 times means don't move.  */
  /* Null string is found at starting position.  */
  if (len == 0 || n == 0)
    {
      set_search_regs (pos_byte, 0);
      return pos;
    }

  if (RE && !(trivial_regexp_p (string) && NILP (Vsearch_spaces_regexp)))
    {
      unsigned char *p1, *p2;
      EMACS_INT s1, s2;
      struct re_pattern_buffer *bufp;

      bufp = compile_pattern (string,
			      (NILP (Vinhibit_changing_match_data)
			       ? &search_regs : &search_regs_1),
			      trt, posix,
			      !NILP (BVAR (current_buffer, enable_multibyte_characters)));

      immediate_quit = 1;	/* Quit immediately if user types ^G,
				   because letting this function finish
				   can take too long. */
      QUIT;			/* Do a pending quit right away,
				   to avoid paradoxical behavior */
      /* Get pointers and sizes of the two strings
	 that make up the visible portion of the buffer. */

      p1 = BEGV_ADDR;
      s1 = GPT_BYTE - BEGV_BYTE;
      p2 = GAP_END_ADDR;
      s2 = ZV_BYTE - GPT_BYTE;
      if (s1 < 0)
	{
	  p2 = p1;
	  s2 = ZV_BYTE - BEGV_BYTE;
	  s1 = 0;
	}
      if (s2 < 0)
	{
	  s1 = ZV_BYTE - BEGV_BYTE;
	  s2 = 0;
	}
      re_match_object = Qnil;

      while (n < 0)
	{
	  EMACS_INT val;

	  val = re_search_2 (bufp, (char *) p1, s1, (char *) p2, s2,
			     pos_byte - BEGV_BYTE, lim_byte - pos_byte,
			     (NILP (Vinhibit_changing_match_data)
			      ? &search_regs : &search_regs_1),
			     /* Don't allow match past current point */
			     pos_byte - BEGV_BYTE);
	  if (val == -2)
	    {
	      matcher_overflow ();
	    }
	  if (val >= 0)
	    {
	      if (NILP (Vinhibit_changing_match_data))
		{
		  pos_byte = search_regs.start[0] + BEGV_BYTE;
		  for (i = 0; i < search_regs.num_regs; i++)
		    if (search_regs.start[i] >= 0)
		      {
			search_regs.start[i]
			  = BYTE_TO_CHAR (search_regs.start[i] + BEGV_BYTE);
			search_regs.end[i]
			  = BYTE_TO_CHAR (search_regs.end[i] + BEGV_BYTE);
		      }
		  XSETBUFFER (last_thing_searched, current_buffer);
		  /* Set pos to the new position. */
		  pos = search_regs.start[0];
		}
	      else
		{
		  pos_byte = search_regs_1.start[0] + BEGV_BYTE;
		  /* Set pos to the new position.  */
		  pos = BYTE_TO_CHAR (search_regs_1.start[0] + BEGV_BYTE);
		}
	    }
	  else
	    {
	      immediate_quit = 0;
	      return (n);
	    }
	  n++;
	}
      while (n > 0)
	{
	  EMACS_INT val;

	  val = re_search_2 (bufp, (char *) p1, s1, (char *) p2, s2,
			     pos_byte - BEGV_BYTE, lim_byte - pos_byte,
			     (NILP (Vinhibit_changing_match_data)
			      ? &search_regs : &search_regs_1),
			     lim_byte - BEGV_BYTE);
	  if (val == -2)
	    {
	      matcher_overflow ();
	    }
	  if (val >= 0)
	    {
	      if (NILP (Vinhibit_changing_match_data))
		{
		  pos_byte = search_regs.end[0] + BEGV_BYTE;
		  for (i = 0; i < search_regs.num_regs; i++)
		    if (search_regs.start[i] >= 0)
		      {
			search_regs.start[i]
			  = BYTE_TO_CHAR (search_regs.start[i] + BEGV_BYTE);
			search_regs.end[i]
			  = BYTE_TO_CHAR (search_regs.end[i] + BEGV_BYTE);
		      }
		  XSETBUFFER (last_thing_searched, current_buffer);
		  pos = search_regs.end[0];
		}
	      else
		{
		  pos_byte = search_regs_1.end[0] + BEGV_BYTE;
		  pos = BYTE_TO_CHAR (search_regs_1.end[0] + BEGV_BYTE);
		}
	    }
	  else
	    {
	      immediate_quit = 0;
	      return (0 - n);
	    }
	  n--;
	}
      immediate_quit = 0;
      return (pos);
    }
  else				/* non-RE case */
    {
      unsigned char *raw_pattern, *pat;
      EMACS_INT raw_pattern_size;
      EMACS_INT raw_pattern_size_byte;
      unsigned char *patbuf;
      int multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
      unsigned char *base_pat;
      /* Set to positive if we find a non-ASCII char that need
	 translation.  Otherwise set to zero later.  */
      int char_base = -1;
      int boyer_moore_ok = 1;

      /* MULTIBYTE says whether the text to be searched is multibyte.
	 We must convert PATTERN to match that, or we will not really
	 find things right.  */

      if (multibyte == STRING_MULTIBYTE (string))
	{
	  raw_pattern = SDATA (string);
	  raw_pattern_size = SCHARS (string);
	  raw_pattern_size_byte = SBYTES (string);
	}
      else if (multibyte)
	{
	  raw_pattern_size = SCHARS (string);
	  raw_pattern_size_byte
	    = count_size_as_multibyte (SDATA (string),
				       raw_pattern_size);
	  raw_pattern = (unsigned char *) alloca (raw_pattern_size_byte + 1);
	  copy_text (SDATA (string), raw_pattern,
		     SCHARS (string), 0, 1);
	}
      else
	{
	  /* Converting multibyte to single-byte.

	     ??? Perhaps this conversion should be done in a special way
	     by subtracting nonascii-insert-offset from each non-ASCII char,
	     so that only the multibyte chars which really correspond to
	     the chosen single-byte character set can possibly match.  */
	  raw_pattern_size = SCHARS (string);
	  raw_pattern_size_byte = SCHARS (string);
	  raw_pattern = (unsigned char *) alloca (raw_pattern_size + 1);
	  copy_text (SDATA (string), raw_pattern,
		     SBYTES (string), 1, 0);
	}

      /* Copy and optionally translate the pattern.  */
      len = raw_pattern_size;
      len_byte = raw_pattern_size_byte;
      patbuf = (unsigned char *) alloca (len * MAX_MULTIBYTE_LENGTH);
      pat = patbuf;
      base_pat = raw_pattern;
      if (multibyte)
	{
	  /* Fill patbuf by translated characters in STRING while
	     checking if we can use boyer-moore search.  If TRT is
	     non-nil, we can use boyer-moore search only if TRT can be
	     represented by the byte array of 256 elements.  For that,
	     all non-ASCII case-equivalents of all case-sensitive
	     characters in STRING must belong to the same charset and
	     row.  */

	  while (--len >= 0)
	    {
	      unsigned char str_base[MAX_MULTIBYTE_LENGTH], *str;
	      int c, translated, inverse;
	      int in_charlen, charlen;

	      /* If we got here and the RE flag is set, it's because we're
		 dealing with a regexp known to be trivial, so the backslash
		 just quotes the next character.  */
	      if (RE && *base_pat == '\\')
		{
		  len--;
		  raw_pattern_size--;
		  len_byte--;
		  base_pat++;
		}

	      c = STRING_CHAR_AND_LENGTH (base_pat, in_charlen);

	      if (NILP (trt))
		{
		  str = base_pat;
		  charlen = in_charlen;
		}
	      else
		{
		  /* Translate the character.  */
		  TRANSLATE (translated, trt, c);
		  charlen = CHAR_STRING (translated, str_base);
		  str = str_base;

		  /* Check if C has any other case-equivalents.  */
		  TRANSLATE (inverse, inverse_trt, c);
		  /* If so, check if we can use boyer-moore.  */
		  if (c != inverse && boyer_moore_ok)
		    {
		      /* Check if all equivalents belong to the same
			 group of characters.  Note that the check of C
			 itself is done by the last iteration.  */
		      int this_char_base = -1;

		      while (boyer_moore_ok)
			{
			  if (ASCII_BYTE_P (inverse))
			    {
			      if (this_char_base > 0)
				boyer_moore_ok = 0;
			      else
				this_char_base = 0;
			    }
			  else if (CHAR_BYTE8_P (inverse))
			    /* Boyer-moore search can't handle a
			       translation of an eight-bit
			       character.  */
			    boyer_moore_ok = 0;
			  else if (this_char_base < 0)
			    {
			      this_char_base = inverse & ~0x3F;
			      if (char_base < 0)
				char_base = this_char_base;
			      else if (this_char_base != char_base)
				boyer_moore_ok = 0;
			    }
			  else if ((inverse & ~0x3F) != this_char_base)
			    boyer_moore_ok = 0;
			  if (c == inverse)
			    break;
			  TRANSLATE (inverse, inverse_trt, inverse);
			}
		    }
		}

	      /* Store this character into the translated pattern.  */
	      memcpy (pat, str, charlen);
	      pat += charlen;
	      base_pat += in_charlen;
	      len_byte -= in_charlen;
	    }

	  /* If char_base is still negative we didn't find any translated
	     non-ASCII characters.  */
	  if (char_base < 0)
	    char_base = 0;
	}
      else
	{
	  /* Unibyte buffer.  */
	  char_base = 0;
	  while (--len >= 0)
	    {
	      int c, translated;

	      /* If we got here and the RE flag is set, it's because we're
		 dealing with a regexp known to be trivial, so the backslash
		 just quotes the next character.  */
	      if (RE && *base_pat == '\\')
		{
		  len--;
		  raw_pattern_size--;
		  base_pat++;
		}
	      c = *base_pat++;
	      TRANSLATE (translated, trt, c);
	      *pat++ = translated;
	    }
	}

      len_byte = pat - patbuf;
      pat = base_pat = patbuf;

      if (boyer_moore_ok)
	return boyer_moore (n, pat, len_byte, trt, inverse_trt,
			    pos_byte, lim_byte,
			    char_base);
      else
	return simple_search (n, pat, raw_pattern_size, len_byte, trt,
			      pos, pos_byte, lim, lim_byte);
    }
}

/* Do a simple string search N times for the string PAT,
   whose length is LEN/LEN_BYTE,
   from buffer position POS/POS_BYTE until LIM/LIM_BYTE.
   TRT is the translation table.

   Return the character position where the match is found.
   Otherwise, if M matches remained to be found, return -M.

   This kind of search works regardless of what is in PAT and
   regardless of what is in TRT.  It is used in cases where
   boyer_moore cannot work.  */

static EMACS_INT
simple_search (EMACS_INT n, unsigned char *pat,
	       EMACS_INT len, EMACS_INT len_byte, Lisp_Object trt,
	       EMACS_INT pos, EMACS_INT pos_byte,
	       EMACS_INT lim, EMACS_INT lim_byte)
{
  int multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));
  int forward = n > 0;
  /* Number of buffer bytes matched.  Note that this may be different
     from len_byte in a multibyte buffer.  */
  EMACS_INT match_byte;

  if (lim > pos && multibyte)
    while (n > 0)
      {
	while (1)
	  {
	    /* Try matching at position POS.  */
	    EMACS_INT this_pos = pos;
	    EMACS_INT this_pos_byte = pos_byte;
	    EMACS_INT this_len = len;
	    unsigned char *p = pat;
	    if (pos + len > lim || pos_byte + len_byte > lim_byte)
	      goto stop;

	    while (this_len > 0)
	      {
		int charlen, buf_charlen;
		int pat_ch, buf_ch;

		pat_ch = STRING_CHAR_AND_LENGTH (p, charlen);
		buf_ch = STRING_CHAR_AND_LENGTH (BYTE_POS_ADDR (this_pos_byte),
						 buf_charlen);
		TRANSLATE (buf_ch, trt, buf_ch);

		if (buf_ch != pat_ch)
		  break;

		this_len--;
		p += charlen;

		this_pos_byte += buf_charlen;
		this_pos++;
	      }

	    if (this_len == 0)
	      {
		match_byte = this_pos_byte - pos_byte;
		pos += len;
		pos_byte += match_byte;
		break;
	      }

	    INC_BOTH (pos, pos_byte);
	  }

	n--;
      }
  else if (lim > pos)
    while (n > 0)
      {
	while (1)
	  {
	    /* Try matching at position POS.  */
	    EMACS_INT this_pos = pos;
	    EMACS_INT this_len = len;
	    unsigned char *p = pat;

	    if (pos + len > lim)
	      goto stop;

	    while (this_len > 0)
	      {
		int pat_ch = *p++;
		int buf_ch = FETCH_BYTE (this_pos);
		TRANSLATE (buf_ch, trt, buf_ch);

		if (buf_ch != pat_ch)
		  break;

		this_len--;
		this_pos++;
	      }

	    if (this_len == 0)
	      {
		match_byte = len;
		pos += len;
		break;
	      }

	    pos++;
	  }

	n--;
      }
  /* Backwards search.  */
  else if (lim < pos && multibyte)
    while (n < 0)
      {
	while (1)
	  {
	    /* Try matching at position POS.  */
	    EMACS_INT this_pos = pos;
	    EMACS_INT this_pos_byte = pos_byte;
	    EMACS_INT this_len = len;
	    const unsigned char *p = pat + len_byte;

	    if (this_pos - len < lim || (pos_byte - len_byte) < lim_byte)
	      goto stop;

	    while (this_len > 0)
	      {
		int pat_ch, buf_ch;

		DEC_BOTH (this_pos, this_pos_byte);
		PREV_CHAR_BOUNDARY (p, pat);
		pat_ch = STRING_CHAR (p);
		buf_ch = STRING_CHAR (BYTE_POS_ADDR (this_pos_byte));
		TRANSLATE (buf_ch, trt, buf_ch);

		if (buf_ch != pat_ch)
		  break;

		this_len--;
	      }

	    if (this_len == 0)
	      {
		match_byte = pos_byte - this_pos_byte;
		pos = this_pos;
		pos_byte = this_pos_byte;
		break;
	      }

	    DEC_BOTH (pos, pos_byte);
	  }

	n++;
      }
  else if (lim < pos)
    while (n < 0)
      {
	while (1)
	  {
	    /* Try matching at position POS.  */
	    EMACS_INT this_pos = pos - len;
	    EMACS_INT this_len = len;
	    unsigned char *p = pat;

	    if (this_pos < lim)
	      goto stop;

	    while (this_len > 0)
	      {
		int pat_ch = *p++;
		int buf_ch = FETCH_BYTE (this_pos);
		TRANSLATE (buf_ch, trt, buf_ch);

		if (buf_ch != pat_ch)
		  break;
		this_len--;
		this_pos++;
	      }

	    if (this_len == 0)
	      {
		match_byte = len;
		pos -= len;
		break;
	      }

	    pos--;
	  }

	n++;
      }

 stop:
  if (n == 0)
    {
      if (forward)
	set_search_regs ((multibyte ? pos_byte : pos) - match_byte, match_byte);
      else
	set_search_regs (multibyte ? pos_byte : pos, match_byte);

      return pos;
    }
  else if (n > 0)
    return -n;
  else
    return n;
}

/* Do Boyer-Moore search N times for the string BASE_PAT,
   whose length is LEN_BYTE,
   from buffer position POS_BYTE until LIM_BYTE.
   DIRECTION says which direction we search in.
   TRT and INVERSE_TRT are translation tables.
   Characters in PAT are already translated by TRT.

   This kind of search works if all the characters in BASE_PAT that
   have nontrivial translation are the same aside from the last byte.
   This makes it possible to translate just the last byte of a
   character, and do so after just a simple test of the context.
   CHAR_BASE is nonzero if there is such a non-ASCII character.

   If that criterion is not satisfied, do not call this function.  */

static EMACS_INT
boyer_moore (EMACS_INT n, unsigned char *base_pat,
	     EMACS_INT len_byte,
	     Lisp_Object trt, Lisp_Object inverse_trt,
	     EMACS_INT pos_byte, EMACS_INT lim_byte,
             int char_base)
{
  int direction = ((n > 0) ? 1 : -1);
  register EMACS_INT dirlen;
  EMACS_INT limit;
  int stride_for_teases = 0;
  int BM_tab[0400];
  register unsigned char *cursor, *p_limit;
  register EMACS_INT i;
  register int j;
  unsigned char *pat, *pat_end;
  int multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));

  unsigned char simple_translate[0400];
  /* These are set to the preceding bytes of a byte to be translated
     if char_base is nonzero.  As the maximum byte length of a
     multibyte character is 5, we have to check at most four previous
     bytes.  */
  int translate_prev_byte1 = 0;
  int translate_prev_byte2 = 0;
  int translate_prev_byte3 = 0;

  /* The general approach is that we are going to maintain that we know
     the first (closest to the present position, in whatever direction
     we're searching) character that could possibly be the last
     (furthest from present position) character of a valid match.  We
     advance the state of our knowledge by looking at that character
     and seeing whether it indeed matches the last character of the
     pattern.  If it does, we take a closer look.  If it does not, we
     move our pointer (to putative last characters) as far as is
     logically possible.  This amount of movement, which I call a
     stride, will be the length of the pattern if the actual character
     appears nowhere in the pattern, otherwise it will be the distance
     from the last occurrence of that character to the end of the
     pattern.  If the amount is zero we have a possible match.  */

  /* Here we make a "mickey mouse" BM table.  The stride of the search
     is determined only by the last character of the putative match.
     If that character does not match, we will stride the proper
     distance to propose a match that superimposes it on the last
     instance of a character that matches it (per trt), or misses
     it entirely if there is none. */

  dirlen = len_byte * direction;

  /* Record position after the end of the pattern.  */
  pat_end = base_pat + len_byte;
  /* BASE_PAT points to a character that we start scanning from.
     It is the first character in a forward search,
     the last character in a backward search.  */
  if (direction < 0)
    base_pat = pat_end - 1;

  /* A character that does not appear in the pattern induces a
     stride equal to the pattern length.  */
  for (i = 0; i < 0400; i++)
    BM_tab[i] = dirlen;

  /* We use this for translation, instead of TRT itself.
     We fill this in to handle the characters that actually
     occur in the pattern.  Others don't matter anyway!  */
  for (i = 0; i < 0400; i++)
    simple_translate[i] = i;

  if (char_base)
    {
      /* Setup translate_prev_byte1/2/3/4 from CHAR_BASE.  Only a
	 byte following them are the target of translation.  */
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      int cblen = CHAR_STRING (char_base, str);

      translate_prev_byte1 = str[cblen - 2];
      if (cblen > 2)
	{
	  translate_prev_byte2 = str[cblen - 3];
	  if (cblen > 3)
	    translate_prev_byte3 = str[cblen - 4];
	}
    }

  i = 0;
  while (i != dirlen)
    {
      unsigned char *ptr = base_pat + i;
      i += direction;
      if (! NILP (trt))
	{
	  /* If the byte currently looking at is the last of a
	     character to check case-equivalents, set CH to that
	     character.  An ASCII character and a non-ASCII character
	     matching with CHAR_BASE are to be checked.  */
	  int ch = -1;

	  if (ASCII_BYTE_P (*ptr) || ! multibyte)
	    ch = *ptr;
	  else if (char_base
		   && ((pat_end - ptr) == 1 || CHAR_HEAD_P (ptr[1])))
	    {
	      unsigned char *charstart = ptr - 1;

	      while (! (CHAR_HEAD_P (*charstart)))
		charstart--;
	      ch = STRING_CHAR (charstart);
	      if (char_base != (ch & ~0x3F))
		ch = -1;
	    }

	  if (ch >= 0200 && multibyte)
	    j = (ch & 0x3F) | 0200;
	  else
	    j = *ptr;

	  if (i == dirlen)
	    stride_for_teases = BM_tab[j];

	  BM_tab[j] = dirlen - i;
	  /* A translation table is accompanied by its inverse -- see
	     comment following downcase_table for details.  */
	  if (ch >= 0)
	    {
	      int starting_ch = ch;
	      int starting_j = j;

	      while (1)
		{
		  TRANSLATE (ch, inverse_trt, ch);
		  if (ch >= 0200 && multibyte)
		    j = (ch & 0x3F) | 0200;
		  else
		    j = ch;

		  /* For all the characters that map into CH,
		     set up simple_translate to map the last byte
		     into STARTING_J.  */
		  simple_translate[j] = starting_j;
		  if (ch == starting_ch)
		    break;
		  BM_tab[j] = dirlen - i;
		}
	    }
	}
      else
	{
	  j = *ptr;

	  if (i == dirlen)
	    stride_for_teases = BM_tab[j];
	  BM_tab[j] = dirlen - i;
	}
      /* stride_for_teases tells how much to stride if we get a
	 match on the far character but are subsequently
	 disappointed, by recording what the stride would have been
	 for that character if the last character had been
	 different.  */
    }
  pos_byte += dirlen - ((direction > 0) ? direction : 0);
  /* loop invariant - POS_BYTE points at where last char (first
     char if reverse) of pattern would align in a possible match.  */
  while (n != 0)
    {
      EMACS_INT tail_end;
      unsigned char *tail_end_ptr;

      /* It's been reported that some (broken) compiler thinks that
	 Boolean expressions in an arithmetic context are unsigned.
	 Using an explicit ?1:0 prevents this.  */
      if ((lim_byte - pos_byte - ((direction > 0) ? 1 : 0)) * direction
	  < 0)
	return (n * (0 - direction));
      /* First we do the part we can by pointers (maybe nothing) */
      QUIT;
      pat = base_pat;
      limit = pos_byte - dirlen + direction;
      if (direction > 0)
	{
	  limit = BUFFER_CEILING_OF (limit);
	  /* LIMIT is now the last (not beyond-last!) value POS_BYTE
	     can take on without hitting edge of buffer or the gap.  */
	  limit = min (limit, pos_byte + 20000);
	  limit = min (limit, lim_byte - 1);
	}
      else
	{
	  limit = BUFFER_FLOOR_OF (limit);
	  /* LIMIT is now the last (not beyond-last!) value POS_BYTE
	     can take on without hitting edge of buffer or the gap.  */
	  limit = max (limit, pos_byte - 20000);
	  limit = max (limit, lim_byte);
	}
      tail_end = BUFFER_CEILING_OF (pos_byte) + 1;
      tail_end_ptr = BYTE_POS_ADDR (tail_end);

      if ((limit - pos_byte) * direction > 20)
	{
	  unsigned char *p2;

	  p_limit = BYTE_POS_ADDR (limit);
	  p2 = (cursor = BYTE_POS_ADDR (pos_byte));
	  /* In this loop, pos + cursor - p2 is the surrogate for pos.  */
	  while (1)		/* use one cursor setting as long as i can */
	    {
	      if (direction > 0) /* worth duplicating */
		{
		  while (cursor <= p_limit)
		    {
		      if (BM_tab[*cursor] == 0)
			goto hit;
		      cursor += BM_tab[*cursor];
		    }
		}
	      else
		{
		  while (cursor >= p_limit)
		    {
		      if (BM_tab[*cursor] == 0)
			goto hit;
		      cursor += BM_tab[*cursor];
		    }
		}
	      /* If you are here, cursor is beyond the end of the
		 searched region.  You fail to match within the
		 permitted region and would otherwise try a character
		 beyond that region.  */
	      break;

	    hit:
	      i = dirlen - direction;
	      if (! NILP (trt))
		{
		  while ((i -= direction) + direction != 0)
		    {
		      int ch;
		      cursor -= direction;
		      /* Translate only the last byte of a character.  */
		      if (! multibyte
			  || ((cursor == tail_end_ptr
			       || CHAR_HEAD_P (cursor[1]))
			      && (CHAR_HEAD_P (cursor[0])
				  /* Check if this is the last byte of
				     a translatable character.  */
				  || (translate_prev_byte1 == cursor[-1]
				      && (CHAR_HEAD_P (translate_prev_byte1)
					  || (translate_prev_byte2 == cursor[-2]
					      && (CHAR_HEAD_P (translate_prev_byte2)
						  || (translate_prev_byte3 == cursor[-3]))))))))
			ch = simple_translate[*cursor];
		      else
			ch = *cursor;
		      if (pat[i] != ch)
			break;
		    }
		}
	      else
		{
		  while ((i -= direction) + direction != 0)
		    {
		      cursor -= direction;
		      if (pat[i] != *cursor)
			break;
		    }
		}
	      cursor += dirlen - i - direction;	/* fix cursor */
	      if (i + direction == 0)
		{
		  EMACS_INT position, start, end;

		  cursor -= direction;

		  position = pos_byte + cursor - p2 + ((direction > 0)
						       ? 1 - len_byte : 0);
		  set_search_regs (position, len_byte);

		  if (NILP (Vinhibit_changing_match_data))
		    {
		      start = search_regs.start[0];
		      end = search_regs.end[0];
		    }
		  else
		    /* If Vinhibit_changing_match_data is non-nil,
		       search_regs will not be changed.  So let's
		       compute start and end here.  */
		    {
		      start = BYTE_TO_CHAR (position);
		      end = BYTE_TO_CHAR (position + len_byte);
		    }

		  if ((n -= direction) != 0)
		    cursor += dirlen; /* to resume search */
		  else
		    return direction > 0 ? end : start;
		}
	      else
		cursor += stride_for_teases; /* <sigh> we lose -  */
	    }
	  pos_byte += cursor - p2;
	}
      else
	/* Now we'll pick up a clump that has to be done the hard
	   way because it covers a discontinuity.  */
	{
	  limit = ((direction > 0)
		   ? BUFFER_CEILING_OF (pos_byte - dirlen + 1)
		   : BUFFER_FLOOR_OF (pos_byte - dirlen - 1));
	  limit = ((direction > 0)
		   ? min (limit + len_byte, lim_byte - 1)
		   : max (limit - len_byte, lim_byte));
	  /* LIMIT is now the last value POS_BYTE can have
	     and still be valid for a possible match.  */
	  while (1)
	    {
	      /* This loop can be coded for space rather than
		 speed because it will usually run only once.
		 (the reach is at most len + 21, and typically
		 does not exceed len).  */
	      while ((limit - pos_byte) * direction >= 0)
		{
		  int ch = FETCH_BYTE (pos_byte);
		  if (BM_tab[ch] == 0)
		    goto hit2;
		  pos_byte += BM_tab[ch];
		}
	      break;	/* ran off the end */

	    hit2:
	      /* Found what might be a match.  */
	      i = dirlen - direction;
	      while ((i -= direction) + direction != 0)
		{
		  int ch;
		  unsigned char *ptr;
		  pos_byte -= direction;
		  ptr = BYTE_POS_ADDR (pos_byte);
		  /* Translate only the last byte of a character.  */
		  if (! multibyte
		      || ((ptr == tail_end_ptr
			   || CHAR_HEAD_P (ptr[1]))
			  && (CHAR_HEAD_P (ptr[0])
			      /* Check if this is the last byte of a
				 translatable character.  */
			      || (translate_prev_byte1 == ptr[-1]
				  && (CHAR_HEAD_P (translate_prev_byte1)
				      || (translate_prev_byte2 == ptr[-2]
					  && (CHAR_HEAD_P (translate_prev_byte2)
					      || translate_prev_byte3 == ptr[-3])))))))
		    ch = simple_translate[*ptr];
		  else
		    ch = *ptr;
		  if (pat[i] != ch)
		    break;
		}
	      /* Above loop has moved POS_BYTE part or all the way
		 back to the first pos (last pos if reverse).
		 Set it once again at the last (first if reverse) char.  */
	      pos_byte += dirlen - i - direction;
	      if (i + direction == 0)
		{
		  EMACS_INT position, start, end;
		  pos_byte -= direction;

		  position = pos_byte + ((direction > 0) ? 1 - len_byte : 0);
		  set_search_regs (position, len_byte);

		  if (NILP (Vinhibit_changing_match_data))
		    {
		      start = search_regs.start[0];
		      end = search_regs.end[0];
		    }
		  else
		    /* If Vinhibit_changing_match_data is non-nil,
		       search_regs will not be changed.  So let's
		       compute start and end here.  */
		    {
		      start = BYTE_TO_CHAR (position);
		      end = BYTE_TO_CHAR (position + len_byte);
		    }

		  if ((n -= direction) != 0)
		    pos_byte += dirlen; /* to resume search */
		  else
		    return direction > 0 ? end : start;
		}
	      else
		pos_byte += stride_for_teases;
	    }
	  }
      /* We have done one clump.  Can we continue? */
      if ((lim_byte - pos_byte) * direction < 0)
	return ((0 - n) * direction);
    }
  return BYTE_TO_CHAR (pos_byte);
}

/* Record beginning BEG_BYTE and end BEG_BYTE + NBYTES
   for the overall match just found in the current buffer.
   Also clear out the match data for registers 1 and up.  */

static void
set_search_regs (EMACS_INT beg_byte, EMACS_INT nbytes)
{
  int i;

  if (!NILP (Vinhibit_changing_match_data))
    return;

  /* Make sure we have registers in which to store
     the match position.  */
  if (search_regs.num_regs == 0)
    {
      search_regs.start = (regoff_t *) xmalloc (2 * sizeof (regoff_t));
      search_regs.end = (regoff_t *) xmalloc (2 * sizeof (regoff_t));
      search_regs.num_regs = 2;
    }

  /* Clear out the other registers.  */
  for (i = 1; i < search_regs.num_regs; i++)
    {
      search_regs.start[i] = -1;
      search_regs.end[i] = -1;
    }

  search_regs.start[0] = BYTE_TO_CHAR (beg_byte);
  search_regs.end[0] = BYTE_TO_CHAR (beg_byte + nbytes);
  XSETBUFFER (last_thing_searched, current_buffer);
}

DEFUN ("word-search-regexp", Fword_search_regexp, Sword_search_regexp, 1, 2, 0,
       doc: /* Return a regexp which matches words, ignoring punctuation.
Given STRING, a string of words separated by word delimiters,
compute a regexp that matches those exact words separated by
arbitrary punctuation.  If LAX is non-nil, the end of the string
need not match a word boundary unless it ends in whitespace.

Used in `word-search-forward', `word-search-backward',
`word-search-forward-lax', `word-search-backward-lax'.  */)
  (Lisp_Object string, Lisp_Object lax)
{
  register unsigned char *o;
  register EMACS_INT i, i_byte, len, punct_count = 0, word_count = 0;
  Lisp_Object val;
  int prev_c = 0;
  EMACS_INT adjust;
  int whitespace_at_end;

  CHECK_STRING (string);
  len = SCHARS (string);

  for (i = 0, i_byte = 0; i < len; )
    {
      int c;

      FETCH_STRING_CHAR_AS_MULTIBYTE_ADVANCE (c, string, i, i_byte);

      if (SYNTAX (c) != Sword)
	{
	  punct_count++;
	  if (SYNTAX (prev_c) == Sword)
	    word_count++;
	}

      prev_c = c;
    }

  if (SYNTAX (prev_c) == Sword)
    {
      word_count++;
      whitespace_at_end = 0;
    }
  else
    {
      whitespace_at_end = 1;
      if (!word_count)
	return empty_unibyte_string;
    }

  adjust = - punct_count + 5 * (word_count - 1)
    + ((!NILP (lax) && !whitespace_at_end) ? 2 : 4);
  if (STRING_MULTIBYTE (string))
    val = make_uninit_multibyte_string (len + adjust,
					SBYTES (string)
					+ adjust);
  else
    val = make_uninit_string (len + adjust);

  o = SDATA (val);
  *o++ = '\\';
  *o++ = 'b';
  prev_c = 0;

  for (i = 0, i_byte = 0; i < len; )
    {
      int c;
      EMACS_INT i_byte_orig = i_byte;

      FETCH_STRING_CHAR_AS_MULTIBYTE_ADVANCE (c, string, i, i_byte);

      if (SYNTAX (c) == Sword)
	{
	  memcpy (o, SDATA (string) + i_byte_orig, i_byte - i_byte_orig);
	  o += i_byte - i_byte_orig;
	}
      else if (SYNTAX (prev_c) == Sword && --word_count)
	{
	  *o++ = '\\';
	  *o++ = 'W';
	  *o++ = '\\';
	  *o++ = 'W';
	  *o++ = '*';
	}

      prev_c = c;
    }

  if (NILP (lax) || whitespace_at_end)
    {
      *o++ = '\\';
      *o++ = 'b';
    }

  return val;
}

DEFUN ("search-backward", Fsearch_backward, Ssearch_backward, 1, 4,
       "MSearch backward: ",
       doc: /* Search backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend before that position.
Optional third argument, if t, means if fail just return nil (no error).
 If not nil and not t, position at limit of search and return nil.
Optional fourth argument COUNT, if non-nil, means to search for COUNT
 successive occurrences.  If COUNT is negative, search forward,
 instead of backward, for -COUNT occurrences.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end' and `replace-match'.  */)
  (Lisp_Object string, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (string, bound, noerror, count, -1, 0, 0);
}

DEFUN ("search-forward", Fsearch_forward, Ssearch_forward, 1, 4, "MSearch: ",
       doc: /* Search forward from point for STRING.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  A value of nil is
  equivalent to (point-max).
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument COUNT, if non-nil, means to search for COUNT
 successive occurrences.  If COUNT is negative, search backward,
 instead of forward, for -COUNT occurrences.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end' and `replace-match'.  */)
  (Lisp_Object string, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (string, bound, noerror, count, 1, 0, 0);
}

DEFUN ("word-search-backward", Fword_search_backward, Sword_search_backward, 1, 4,
       "sWord search backward: ",
       doc: /* Search backward from point for STRING, ignoring differences in punctuation.
Set point to the beginning of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend before that position.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.

Relies on the function `word-search-regexp' to convert a sequence
of words in STRING to a regexp used to search words without regard
to punctuation.  */)
  (Lisp_Object string, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (Fword_search_regexp (string, Qnil), bound, noerror, count, -1, 1, 0);
}

DEFUN ("word-search-forward", Fword_search_forward, Sword_search_forward, 1, 4,
       "sWord search: ",
       doc: /* Search forward from point for STRING, ignoring differences in punctuation.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.

Relies on the function `word-search-regexp' to convert a sequence
of words in STRING to a regexp used to search words without regard
to punctuation.  */)
  (Lisp_Object string, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (Fword_search_regexp (string, Qnil), bound, noerror, count, 1, 1, 0);
}

DEFUN ("word-search-backward-lax", Fword_search_backward_lax, Sword_search_backward_lax, 1, 4,
       "sWord search backward: ",
       doc: /* Search backward from point for STRING, ignoring differences in punctuation.
Set point to the beginning of the occurrence found, and return point.

Unlike `word-search-backward', the end of STRING need not match a word
boundary, unless STRING ends in whitespace.

An optional second argument bounds the search; it is a buffer position.
The match found must not extend before that position.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.

Relies on the function `word-search-regexp' to convert a sequence
of words in STRING to a regexp used to search words without regard
to punctuation.  */)
  (Lisp_Object string, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (Fword_search_regexp (string, Qt), bound, noerror, count, -1, 1, 0);
}

DEFUN ("word-search-forward-lax", Fword_search_forward_lax, Sword_search_forward_lax, 1, 4,
       "sWord search: ",
       doc: /* Search forward from point for STRING, ignoring differences in punctuation.
Set point to the end of the occurrence found, and return point.

Unlike `word-search-forward', the end of STRING need not match a word
boundary, unless STRING ends in whitespace.

An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.

Relies on the function `word-search-regexp' to convert a sequence
of words in STRING to a regexp used to search words without regard
to punctuation.  */)
  (Lisp_Object string, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (Fword_search_regexp (string, Qt), bound, noerror, count, 1, 1, 0);
}

DEFUN ("re-search-backward", Fre_search_backward, Sre_search_backward, 1, 4,
       "sRE search backward: ",
       doc: /* Search backward from point for match for regular expression REGEXP.
Set point to the beginning of the match, and return point.
The match found is the one starting last in the buffer
and yet ending before the origin of the search.
An optional second argument bounds the search; it is a buffer position.
The match found must start at or after that position.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end', `match-string',
and `replace-match'.  */)
  (Lisp_Object regexp, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (regexp, bound, noerror, count, -1, 1, 0);
}

DEFUN ("re-search-forward", Fre_search_forward, Sre_search_forward, 1, 4,
       "sRE search: ",
       doc: /* Search forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end', `match-string',
and `replace-match'.  */)
  (Lisp_Object regexp, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (regexp, bound, noerror, count, 1, 1, 0);
}

DEFUN ("posix-search-backward", Fposix_search_backward, Sposix_search_backward, 1, 4,
       "sPosix search backward: ",
       doc: /* Search backward from point for match for regular expression REGEXP.
Find the longest match in accord with Posix regular expression rules.
Set point to the beginning of the match, and return point.
The match found is the one starting last in the buffer
and yet ending before the origin of the search.
An optional second argument bounds the search; it is a buffer position.
The match found must start at or after that position.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end', `match-string',
and `replace-match'.  */)
  (Lisp_Object regexp, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (regexp, bound, noerror, count, -1, 1, 1);
}

DEFUN ("posix-search-forward", Fposix_search_forward, Sposix_search_forward, 1, 4,
       "sPosix search: ",
       doc: /* Search forward from point for regular expression REGEXP.
Find the longest match in accord with Posix regular expression rules.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end', `match-string',
and `replace-match'.  */)
  (Lisp_Object regexp, Lisp_Object bound, Lisp_Object noerror, Lisp_Object count)
{
  return search_command (regexp, bound, noerror, count, 1, 1, 1);
}

DEFUN ("replace-match", Freplace_match, Sreplace_match, 1, 5, 0,
       doc: /* Replace text matched by last search with NEWTEXT.
Leave point at the end of the replacement text.

If second arg FIXEDCASE is non-nil, do not alter case of replacement text.
Otherwise maybe capitalize the whole text, or maybe just word initials,
based on the replaced text.
If the replaced text has only capital letters
and has at least one multiletter word, convert NEWTEXT to all caps.
Otherwise if all words are capitalized in the replaced text,
capitalize each word in NEWTEXT.

If third arg LITERAL is non-nil, insert NEWTEXT literally.
Otherwise treat `\\' as special:
  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
Case conversion does not apply to these substitutions.

FIXEDCASE and LITERAL are optional arguments.

The optional fourth argument STRING can be a string to modify.
This is meaningful when the previous match was done against STRING,
using `string-match'.  When used this way, `replace-match'
creates and returns a new string made by copying STRING and replacing
the part of STRING that was matched.

The optional fifth argument SUBEXP specifies a subexpression;
it says to replace just that subexpression with NEWTEXT,
rather than replacing the entire matched text.
This is, in a vague sense, the inverse of using `\\N' in NEWTEXT;
`\\N' copies subexp N into NEWTEXT, but using N as SUBEXP puts
NEWTEXT in place of subexp N.
This is useful only after a regular expression search or match,
since only regular expressions have distinguished subexpressions.  */)
  (Lisp_Object newtext, Lisp_Object fixedcase, Lisp_Object literal, Lisp_Object string, Lisp_Object subexp)
{
  enum { nochange, all_caps, cap_initial } case_action;
  register EMACS_INT pos, pos_byte;
  int some_multiletter_word;
  int some_lowercase;
  int some_uppercase;
  int some_nonuppercase_initial;
  register int c, prevc;
  ptrdiff_t sub;
  EMACS_INT opoint, newpoint;

  CHECK_STRING (newtext);

  if (! NILP (string))
    CHECK_STRING (string);

  case_action = nochange;	/* We tried an initialization */
				/* but some C compilers blew it */

  if (search_regs.num_regs <= 0)
    error ("`replace-match' called before any match found");

  if (NILP (subexp))
    sub = 0;
  else
    {
      CHECK_NUMBER (subexp);
      if (! (0 <= XINT (subexp) && XINT (subexp) < search_regs.num_regs))
	args_out_of_range (subexp, make_number (search_regs.num_regs));
      sub = XINT (subexp);
    }

  if (NILP (string))
    {
      if (search_regs.start[sub] < BEGV
	  || search_regs.start[sub] > search_regs.end[sub]
	  || search_regs.end[sub] > ZV)
	args_out_of_range (make_number (search_regs.start[sub]),
			   make_number (search_regs.end[sub]));
    }
  else
    {
      if (search_regs.start[sub] < 0
	  || search_regs.start[sub] > search_regs.end[sub]
	  || search_regs.end[sub] > SCHARS (string))
	args_out_of_range (make_number (search_regs.start[sub]),
			   make_number (search_regs.end[sub]));
    }

  if (NILP (fixedcase))
    {
      /* Decide how to casify by examining the matched text. */
      EMACS_INT last;

      pos = search_regs.start[sub];
      last = search_regs.end[sub];

      if (NILP (string))
	pos_byte = CHAR_TO_BYTE (pos);
      else
	pos_byte = string_char_to_byte (string, pos);

      prevc = '\n';
      case_action = all_caps;

      /* some_multiletter_word is set nonzero if any original word
	 is more than one letter long. */
      some_multiletter_word = 0;
      some_lowercase = 0;
      some_nonuppercase_initial = 0;
      some_uppercase = 0;

      while (pos < last)
	{
	  if (NILP (string))
	    {
	      c = FETCH_CHAR_AS_MULTIBYTE (pos_byte);
	      INC_BOTH (pos, pos_byte);
	    }
	  else
	    FETCH_STRING_CHAR_AS_MULTIBYTE_ADVANCE (c, string, pos, pos_byte);

	  if (lowercasep (c))
	    {
	      /* Cannot be all caps if any original char is lower case */

	      some_lowercase = 1;
	      if (SYNTAX (prevc) != Sword)
		some_nonuppercase_initial = 1;
	      else
		some_multiletter_word = 1;
	    }
	  else if (uppercasep (c))
	    {
	      some_uppercase = 1;
	      if (SYNTAX (prevc) != Sword)
		;
	      else
		some_multiletter_word = 1;
	    }
	  else
	    {
	      /* If the initial is a caseless word constituent,
		 treat that like a lowercase initial.  */
	      if (SYNTAX (prevc) != Sword)
		some_nonuppercase_initial = 1;
	    }

	  prevc = c;
	}

      /* Convert to all caps if the old text is all caps
	 and has at least one multiletter word.  */
      if (! some_lowercase && some_multiletter_word)
	case_action = all_caps;
      /* Capitalize each word, if the old text has all capitalized words.  */
      else if (!some_nonuppercase_initial && some_multiletter_word)
	case_action = cap_initial;
      else if (!some_nonuppercase_initial && some_uppercase)
	/* Should x -> yz, operating on X, give Yz or YZ?
	   We'll assume the latter.  */
	case_action = all_caps;
      else
	case_action = nochange;
    }

  /* Do replacement in a string.  */
  if (!NILP (string))
    {
      Lisp_Object before, after;

      before = Fsubstring (string, make_number (0),
			   make_number (search_regs.start[sub]));
      after = Fsubstring (string, make_number (search_regs.end[sub]), Qnil);

      /* Substitute parts of the match into NEWTEXT
	 if desired.  */
      if (NILP (literal))
	{
	  EMACS_INT lastpos = 0;
	  EMACS_INT lastpos_byte = 0;
	  /* We build up the substituted string in ACCUM.  */
	  Lisp_Object accum;
	  Lisp_Object middle;
	  EMACS_INT length = SBYTES (newtext);

	  accum = Qnil;

	  for (pos_byte = 0, pos = 0; pos_byte < length;)
	    {
	      EMACS_INT substart = -1;
	      EMACS_INT subend = 0;
	      int delbackslash = 0;

	      FETCH_STRING_CHAR_ADVANCE (c, newtext, pos, pos_byte);

	      if (c == '\\')
		{
		  FETCH_STRING_CHAR_ADVANCE (c, newtext, pos, pos_byte);

		  if (c == '&')
		    {
		      substart = search_regs.start[sub];
		      subend = search_regs.end[sub];
		    }
		  else if (c >= '1' && c <= '9')
		    {
		      if (search_regs.start[c - '0'] >= 0
			  && c <= search_regs.num_regs + '0')
			{
			  substart = search_regs.start[c - '0'];
			  subend = search_regs.end[c - '0'];
			}
		      else
			{
			  /* If that subexp did not match,
			     replace \\N with nothing.  */
			  substart = 0;
			  subend = 0;
			}
		    }
		  else if (c == '\\')
		    delbackslash = 1;
		  else
		    error ("Invalid use of `\\' in replacement text");
		}
	      if (substart >= 0)
		{
		  if (pos - 2 != lastpos)
		    middle = substring_both (newtext, lastpos,
					     lastpos_byte,
					     pos - 2, pos_byte - 2);
		  else
		    middle = Qnil;
		  accum = concat3 (accum, middle,
				   Fsubstring (string,
					       make_number (substart),
					       make_number (subend)));
		  lastpos = pos;
		  lastpos_byte = pos_byte;
		}
	      else if (delbackslash)
		{
		  middle = substring_both (newtext, lastpos,
					   lastpos_byte,
					   pos - 1, pos_byte - 1);

		  accum = concat2 (accum, middle);
		  lastpos = pos;
		  lastpos_byte = pos_byte;
		}
	    }

	  if (pos != lastpos)
	    middle = substring_both (newtext, lastpos,
				     lastpos_byte,
				     pos, pos_byte);
	  else
	    middle = Qnil;

	  newtext = concat2 (accum, middle);
	}

      /* Do case substitution in NEWTEXT if desired.  */
      if (case_action == all_caps)
	newtext = Fupcase (newtext);
      else if (case_action == cap_initial)
	newtext = Fupcase_initials (newtext);

      return concat3 (before, newtext, after);
    }

  /* Record point, then move (quietly) to the start of the match.  */
  if (PT >= search_regs.end[sub])
    opoint = PT - ZV;
  else if (PT > search_regs.start[sub])
    opoint = search_regs.end[sub] - ZV;
  else
    opoint = PT;

  /* If we want non-literal replacement,
     perform substitution on the replacement string.  */
  if (NILP (literal))
    {
      ptrdiff_t length = SBYTES (newtext);
      unsigned char *substed;
      ptrdiff_t substed_alloc_size, substed_len;
      int buf_multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
      int str_multibyte = STRING_MULTIBYTE (newtext);
      int really_changed = 0;

      substed_alloc_size = ((STRING_BYTES_BOUND - 100) / 2 < length
			    ? STRING_BYTES_BOUND
			    : length * 2 + 100);
      substed = (unsigned char *) xmalloc (substed_alloc_size);
      substed_len = 0;

      /* Go thru NEWTEXT, producing the actual text to insert in
	 SUBSTED while adjusting multibyteness to that of the current
	 buffer.  */

      for (pos_byte = 0, pos = 0; pos_byte < length;)
	{
	  unsigned char str[MAX_MULTIBYTE_LENGTH];
	  const unsigned char *add_stuff = NULL;
	  ptrdiff_t add_len = 0;
	  ptrdiff_t idx = -1;

	  if (str_multibyte)
	    {
	      FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, newtext, pos, pos_byte);
	      if (!buf_multibyte)
		c = multibyte_char_to_unibyte (c);
	    }
	  else
	    {
	      /* Note that we don't have to increment POS.  */
	      c = SREF (newtext, pos_byte++);
	      if (buf_multibyte)
		MAKE_CHAR_MULTIBYTE (c);
	    }

	  /* Either set ADD_STUFF and ADD_LEN to the text to put in SUBSTED,
	     or set IDX to a match index, which means put that part
	     of the buffer text into SUBSTED.  */

	  if (c == '\\')
	    {
	      really_changed = 1;

	      if (str_multibyte)
		{
		  FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, newtext,
						      pos, pos_byte);
		  if (!buf_multibyte && !ASCII_CHAR_P (c))
		    c = multibyte_char_to_unibyte (c);
		}
	      else
		{
		  c = SREF (newtext, pos_byte++);
		  if (buf_multibyte)
		    MAKE_CHAR_MULTIBYTE (c);
		}

	      if (c == '&')
		idx = sub;
	      else if (c >= '1' && c <= '9' && c <= search_regs.num_regs + '0')
		{
		  if (search_regs.start[c - '0'] >= 1)
		    idx = c - '0';
		}
	      else if (c == '\\')
		add_len = 1, add_stuff = (unsigned char *) "\\";
	      else
		{
		  xfree (substed);
		  error ("Invalid use of `\\' in replacement text");
		}
	    }
	  else
	    {
	      add_len = CHAR_STRING (c, str);
	      add_stuff = str;
	    }

	  /* If we want to copy part of a previous match,
	     set up ADD_STUFF and ADD_LEN to point to it.  */
	  if (idx >= 0)
	    {
	      ptrdiff_t begbyte = CHAR_TO_BYTE (search_regs.start[idx]);
	      add_len = CHAR_TO_BYTE (search_regs.end[idx]) - begbyte;
	      if (search_regs.start[idx] < GPT && GPT < search_regs.end[idx])
		move_gap (search_regs.start[idx]);
	      add_stuff = BYTE_POS_ADDR (begbyte);
	    }

	  /* Now the stuff we want to add to SUBSTED
	     is invariably ADD_LEN bytes starting at ADD_STUFF.  */

	  /* Make sure SUBSTED is big enough.  */
	  if (substed_alloc_size - substed_len < add_len)
	    substed =
	      xpalloc (substed, &substed_alloc_size,
		       add_len - (substed_alloc_size - substed_len),
		       STRING_BYTES_BOUND, 1);

	  /* Now add to the end of SUBSTED.  */
	  if (add_stuff)
	    {
	      memcpy (substed + substed_len, add_stuff, add_len);
	      substed_len += add_len;
	    }
	}

      if (really_changed)
	{
	  if (buf_multibyte)
	    {
	      EMACS_INT nchars =
		multibyte_chars_in_text (substed, substed_len);

	      newtext = make_multibyte_string ((char *) substed, nchars,
					       substed_len);
	    }
	  else
	    newtext = make_unibyte_string ((char *) substed, substed_len);
	}
      xfree (substed);
    }

  /* Replace the old text with the new in the cleanest possible way.  */
  replace_range (search_regs.start[sub], search_regs.end[sub],
		 newtext, 1, 0, 1);
  newpoint = search_regs.start[sub] + SCHARS (newtext);

  if (case_action == all_caps)
    Fupcase_region (make_number (search_regs.start[sub]),
		    make_number (newpoint));
  else if (case_action == cap_initial)
    Fupcase_initials_region (make_number (search_regs.start[sub]),
			     make_number (newpoint));

  /* Adjust search data for this change.  */
  {
    EMACS_INT oldend = search_regs.end[sub];
    EMACS_INT oldstart = search_regs.start[sub];
    EMACS_INT change = newpoint - search_regs.end[sub];
    int i;

    for (i = 0; i < search_regs.num_regs; i++)
      {
	if (search_regs.start[i] >= oldend)
	  search_regs.start[i] += change;
	else if (search_regs.start[i] > oldstart)
	  search_regs.start[i] = oldstart;
	if (search_regs.end[i] >= oldend)
	  search_regs.end[i] += change;
	else if (search_regs.end[i] > oldstart)
	  search_regs.end[i] = oldstart;
      }
  }

  /* Put point back where it was in the text.  */
  if (opoint <= 0)
    TEMP_SET_PT (opoint + ZV);
  else
    TEMP_SET_PT (opoint);

  /* Now move point "officially" to the start of the inserted replacement.  */
  move_if_not_intangible (newpoint);

  return Qnil;
}

static Lisp_Object
match_limit (Lisp_Object num, int beginningp)
{
  EMACS_INT n;

  CHECK_NUMBER (num);
  n = XINT (num);
  if (n < 0)
    args_out_of_range (num, make_number (0));
  if (search_regs.num_regs <= 0)
    error ("No match data, because no search succeeded");
  if (n >= search_regs.num_regs
      || search_regs.start[n] < 0)
    return Qnil;
  return (make_number ((beginningp) ? search_regs.start[n]
		                    : search_regs.end[n]));
}

DEFUN ("match-beginning", Fmatch_beginning, Smatch_beginning, 1, 1, 0,
       doc: /* Return position of start of text matched by last search.
SUBEXP, a number, specifies which parenthesized expression in the last
  regexp.
Value is nil if SUBEXPth pair didn't match, or there were less than
  SUBEXP pairs.
Zero means the entire text matched by the whole regexp or whole string.  */)
  (Lisp_Object subexp)
{
  return match_limit (subexp, 1);
}

DEFUN ("match-end", Fmatch_end, Smatch_end, 1, 1, 0,
       doc: /* Return position of end of text matched by last search.
SUBEXP, a number, specifies which parenthesized expression in the last
  regexp.
Value is nil if SUBEXPth pair didn't match, or there were less than
  SUBEXP pairs.
Zero means the entire text matched by the whole regexp or whole string.  */)
  (Lisp_Object subexp)
{
  return match_limit (subexp, 0);
}

DEFUN ("match-data", Fmatch_data, Smatch_data, 0, 3, 0,
       doc: /* Return a list containing all info on what the last search matched.
Element 2N is `(match-beginning N)'; element 2N + 1 is `(match-end N)'.
All the elements are markers or nil (nil if the Nth pair didn't match)
if the last match was on a buffer; integers or nil if a string was matched.
Use `set-match-data' to reinstate the data in this list.

If INTEGERS (the optional first argument) is non-nil, always use
integers \(rather than markers) to represent buffer positions.  In
this case, and if the last match was in a buffer, the buffer will get
stored as one additional element at the end of the list.

If REUSE is a list, reuse it as part of the value.  If REUSE is long
enough to hold all the values, and if INTEGERS is non-nil, no consing
is done.

If optional third arg RESEAT is non-nil, any previous markers on the
REUSE list will be modified to point to nowhere.

Return value is undefined if the last search failed.  */)
  (Lisp_Object integers, Lisp_Object reuse, Lisp_Object reseat)
{
  Lisp_Object tail, prev;
  Lisp_Object *data;
  int i, len;

  if (!NILP (reseat))
    for (tail = reuse; CONSP (tail); tail = XCDR (tail))
      if (MARKERP (XCAR (tail)))
	{
	  unchain_marker (XMARKER (XCAR (tail)));
	  XSETCAR (tail, Qnil);
	}

  if (NILP (last_thing_searched))
    return Qnil;

  prev = Qnil;

  data = (Lisp_Object *) alloca ((2 * search_regs.num_regs + 1)
				 * sizeof (Lisp_Object));

  len = 0;
  for (i = 0; i < search_regs.num_regs; i++)
    {
      EMACS_INT start = search_regs.start[i];
      if (start >= 0)
	{
	  if (EQ (last_thing_searched, Qt)
	      || ! NILP (integers))
	    {
	      XSETFASTINT (data[2 * i], start);
	      XSETFASTINT (data[2 * i + 1], search_regs.end[i]);
	    }
	  else if (BUFFERP (last_thing_searched))
	    {
	      data[2 * i] = Fmake_marker ();
	      Fset_marker (data[2 * i],
			   make_number (start),
			   last_thing_searched);
	      data[2 * i + 1] = Fmake_marker ();
	      Fset_marker (data[2 * i + 1],
			   make_number (search_regs.end[i]),
			   last_thing_searched);
	    }
	  else
	    /* last_thing_searched must always be Qt, a buffer, or Qnil.  */
	    abort ();

	  len = 2 * i + 2;
	}
      else
	data[2 * i] = data[2 * i + 1] = Qnil;
    }

  if (BUFFERP (last_thing_searched) && !NILP (integers))
    {
      data[len] = last_thing_searched;
      len++;
    }

  /* If REUSE is not usable, cons up the values and return them.  */
  if (! CONSP (reuse))
    return Flist (len, data);

  /* If REUSE is a list, store as many value elements as will fit
     into the elements of REUSE.  */
  for (i = 0, tail = reuse; CONSP (tail);
       i++, tail = XCDR (tail))
    {
      if (i < len)
	XSETCAR (tail, data[i]);
      else
	XSETCAR (tail, Qnil);
      prev = tail;
    }

  /* If we couldn't fit all value elements into REUSE,
     cons up the rest of them and add them to the end of REUSE.  */
  if (i < len)
    XSETCDR (prev, Flist (len - i, data + i));

  return reuse;
}

/* We used to have an internal use variant of `reseat' described as:

      If RESEAT is `evaporate', put the markers back on the free list
      immediately.  No other references to the markers must exist in this
      case, so it is used only internally on the unwind stack and
      save-match-data from Lisp.

   But it was ill-conceived: those supposedly-internal markers get exposed via
   the undo-list, so freeing them here is unsafe.  */

DEFUN ("set-match-data", Fset_match_data, Sset_match_data, 1, 2, 0,
       doc: /* Set internal data on last search match from elements of LIST.
LIST should have been created by calling `match-data' previously.

If optional arg RESEAT is non-nil, make markers on LIST point nowhere.  */)
  (register Lisp_Object list, Lisp_Object reseat)
{
  ptrdiff_t i;
  register Lisp_Object marker;

  if (running_asynch_code)
    save_search_regs ();

  CHECK_LIST (list);

  /* Unless we find a marker with a buffer or an explicit buffer
     in LIST, assume that this match data came from a string.  */
  last_thing_searched = Qt;

  /* Allocate registers if they don't already exist.  */
  {
    ptrdiff_t length = XFASTINT (Flength (list)) / 2;

    if (length > search_regs.num_regs)
      {
	ptrdiff_t num_regs = search_regs.num_regs;
	search_regs.start =
	  xpalloc (search_regs.start, &num_regs, length - num_regs,
		   min (PTRDIFF_MAX, UINT_MAX), sizeof (regoff_t));
	search_regs.end =
	  xrealloc (search_regs.end, num_regs * sizeof (regoff_t));

	for (i = search_regs.num_regs; i < num_regs; i++)
	  search_regs.start[i] = -1;

	search_regs.num_regs = num_regs;
      }

    for (i = 0; CONSP (list); i++)
      {
	marker = XCAR (list);
	if (BUFFERP (marker))
	  {
	    last_thing_searched = marker;
	    break;
	  }
	if (i >= length)
	  break;
	if (NILP (marker))
	  {
	    search_regs.start[i] = -1;
	    list = XCDR (list);
	  }
	else
	  {
	    EMACS_INT from;
	    Lisp_Object m;

	    m = marker;
	    if (MARKERP (marker))
	      {
		if (XMARKER (marker)->buffer == 0)
		  XSETFASTINT (marker, 0);
		else
		  XSETBUFFER (last_thing_searched, XMARKER (marker)->buffer);
	      }

	    CHECK_NUMBER_COERCE_MARKER (marker);
	    from = XINT (marker);

	    if (!NILP (reseat) && MARKERP (m))
	      {
		unchain_marker (XMARKER (m));
		XSETCAR (list, Qnil);
	      }

	    if ((list = XCDR (list), !CONSP (list)))
	      break;

	    m = marker = XCAR (list);

	    if (MARKERP (marker) && XMARKER (marker)->buffer == 0)
	      XSETFASTINT (marker, 0);

	    CHECK_NUMBER_COERCE_MARKER (marker);
	    search_regs.start[i] = from;
	    search_regs.end[i] = XINT (marker);

	    if (!NILP (reseat) && MARKERP (m))
	      {
		unchain_marker (XMARKER (m));
		XSETCAR (list, Qnil);
	      }
	  }
	list = XCDR (list);
      }

    for (; i < search_regs.num_regs; i++)
      search_regs.start[i] = -1;
  }

  return Qnil;
}

/* If non-zero the match data have been saved in saved_search_regs
   during the execution of a sentinel or filter. */
static int search_regs_saved;
static struct re_registers saved_search_regs;
static Lisp_Object saved_last_thing_searched;

/* Called from Flooking_at, Fstring_match, search_buffer, Fstore_match_data
   if asynchronous code (filter or sentinel) is running. */
static void
save_search_regs (void)
{
  if (!search_regs_saved)
    {
      saved_search_regs.num_regs = search_regs.num_regs;
      saved_search_regs.start = search_regs.start;
      saved_search_regs.end = search_regs.end;
      saved_last_thing_searched = last_thing_searched;
      last_thing_searched = Qnil;
      search_regs.num_regs = 0;
      search_regs.start = 0;
      search_regs.end = 0;

      search_regs_saved = 1;
    }
}

/* Called upon exit from filters and sentinels. */
void
restore_search_regs (void)
{
  if (search_regs_saved)
    {
      if (search_regs.num_regs > 0)
	{
	  xfree (search_regs.start);
	  xfree (search_regs.end);
	}
      search_regs.num_regs = saved_search_regs.num_regs;
      search_regs.start = saved_search_regs.start;
      search_regs.end = saved_search_regs.end;
      last_thing_searched = saved_last_thing_searched;
      saved_last_thing_searched = Qnil;
      search_regs_saved = 0;
    }
}

static Lisp_Object
unwind_set_match_data (Lisp_Object list)
{
  /* It is NOT ALWAYS safe to free (evaporate) the markers immediately.  */
  return Fset_match_data (list, Qt);
}

/* Called to unwind protect the match data.  */
void
record_unwind_save_match_data (void)
{
  record_unwind_protect (unwind_set_match_data,
			 Fmatch_data (Qnil, Qnil, Qnil));
}

/* Quote a string to deactivate reg-expr chars */

DEFUN ("regexp-quote", Fregexp_quote, Sregexp_quote, 1, 1, 0,
       doc: /* Return a regexp string which matches exactly STRING and nothing else.  */)
  (Lisp_Object string)
{
  register char *in, *out, *end;
  register char *temp;
  int backslashes_added = 0;

  CHECK_STRING (string);

  temp = (char *) alloca (SBYTES (string) * 2);

  /* Now copy the data into the new string, inserting escapes. */

  in = SSDATA (string);
  end = in + SBYTES (string);
  out = temp;

  for (; in != end; in++)
    {
      if (*in == '['
	  || *in == '*' || *in == '.' || *in == '\\'
	  || *in == '?' || *in == '+'
	  || *in == '^' || *in == '$')
	*out++ = '\\', backslashes_added++;
      *out++ = *in;
    }

  return make_specified_string (temp,
				SCHARS (string) + backslashes_added,
				out - temp,
				STRING_MULTIBYTE (string));
}

void
syms_of_search (void)
{
  register int i;

  for (i = 0; i < REGEXP_CACHE_SIZE; ++i)
    {
      searchbufs[i].buf.allocated = 100;
      searchbufs[i].buf.buffer = (unsigned char *) xmalloc (100);
      searchbufs[i].buf.fastmap = searchbufs[i].fastmap;
      searchbufs[i].regexp = Qnil;
      searchbufs[i].whitespace_regexp = Qnil;
      searchbufs[i].syntax_table = Qnil;
      staticpro (&searchbufs[i].regexp);
      staticpro (&searchbufs[i].whitespace_regexp);
      staticpro (&searchbufs[i].syntax_table);
      searchbufs[i].next = (i == REGEXP_CACHE_SIZE-1 ? 0 : &searchbufs[i+1]);
    }
  searchbuf_head = &searchbufs[0];

  DEFSYM (Qsearch_failed, "search-failed");
  DEFSYM (Qinvalid_regexp, "invalid-regexp");

  Fput (Qsearch_failed, Qerror_conditions,
	pure_cons (Qsearch_failed, pure_cons (Qerror, Qnil)));
  Fput (Qsearch_failed, Qerror_message,
	make_pure_c_string ("Search failed"));

  Fput (Qinvalid_regexp, Qerror_conditions,
	pure_cons (Qinvalid_regexp, pure_cons (Qerror, Qnil)));
  Fput (Qinvalid_regexp, Qerror_message,
	make_pure_c_string ("Invalid regexp"));

  last_thing_searched = Qnil;
  staticpro (&last_thing_searched);

  saved_last_thing_searched = Qnil;
  staticpro (&saved_last_thing_searched);

  DEFVAR_LISP ("search-spaces-regexp", Vsearch_spaces_regexp,
      doc: /* Regexp to substitute for bunches of spaces in regexp search.
Some commands use this for user-specified regexps.
Spaces that occur inside character classes or repetition operators
or other such regexp constructs are not replaced with this.
A value of nil (which is the normal value) means treat spaces literally.  */);
  Vsearch_spaces_regexp = Qnil;

  DEFVAR_LISP ("inhibit-changing-match-data", Vinhibit_changing_match_data,
      doc: /* Internal use only.
If non-nil, the primitive searching and matching functions
such as `looking-at', `string-match', `re-search-forward', etc.,
do not set the match data.  The proper way to use this variable
is to bind it with `let' around a small expression.  */);
  Vinhibit_changing_match_data = Qnil;

  defsubr (&Slooking_at);
  defsubr (&Sposix_looking_at);
  defsubr (&Sstring_match);
  defsubr (&Sposix_string_match);
  defsubr (&Ssearch_forward);
  defsubr (&Ssearch_backward);
  defsubr (&Sword_search_regexp);
  defsubr (&Sword_search_forward);
  defsubr (&Sword_search_backward);
  defsubr (&Sword_search_forward_lax);
  defsubr (&Sword_search_backward_lax);
  defsubr (&Sre_search_forward);
  defsubr (&Sre_search_backward);
  defsubr (&Sposix_search_forward);
  defsubr (&Sposix_search_backward);
  defsubr (&Sreplace_match);
  defsubr (&Smatch_beginning);
  defsubr (&Smatch_end);
  defsubr (&Smatch_data);
  defsubr (&Sset_match_data);
  defsubr (&Sregexp_quote);
}
