/* Header file: Caching facts about regions of the buffer, for optimization.

Copyright (C) 1985-1986, 1993, 1995, 2001-2012
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


/* This code was written by Jim Blandy <jimb@cs.oberlin.edu> to help
   GNU Emacs better support the gene editor written for the University
   of Illinois at Urbana-Champagne's Ribosome Database Project (RDP).

   Emacs implements line operations (finding the beginning/end of the
   line, vertical motion, all the redisplay stuff) by searching for
   newlines in the buffer.  Usually, this is a good design; it's very
   clean to just represent the buffer as an unstructured string of
   characters, and the lines in most files are very short (less than
   eighty characters), meaning that scanning usually costs about the
   same as the overhead of maintaining some more complicated data
   structure.

   However, some applications, like gene editing, make use of very
   long lines --- on the order of tens of kilobytes.  In such cases,
   it may well be worthwhile to try to avoid scanning, because the
   scans have become two orders of magnitude more expensive.  It would
   be nice if this speedup could preserve the simplicity of the
   existing data structure, and disturb as little of the existing code
   as possible.

   So here's the tack.  We add some caching to the scan_buffer
   function, so that when it searches for a newline, it notes that the
   region between the start and end of the search contained no
   newlines; then, the next time around, it consults this cache to see
   if there are regions of text it can skip over completely.  The
   buffer modification primitives invalidate this cache.

   (Note: Since the redisplay code needs similar information on
   modified regions of the buffer, we can use the code that helps out
   redisplay as a guide to where we need to add our own code to
   invalidate our cache.  prepare_to_modify_buffer seems to be the
   central spot.)

   Note that the cache code itself never mentions newlines
   specifically, so if you wanted to cache other properties of regions
   of the buffer, you could use this code pretty much unchanged.  So
   this cache really holds "known/unknown" information --- "I know
   this region has property P" vs. "I don't know if this region has
   property P or not."  */


/* Allocate, initialize and return a new, empty region cache.  */
struct region_cache *new_region_cache (void);

/* Free a region cache.  */
void free_region_cache (struct region_cache *);

/* Assert that the region of BUF between START and END (absolute
   buffer positions) is "known," for the purposes of CACHE (e.g. "has
   no newlines", in the case of the line cache).  */
extern void know_region_cache (struct buffer *BUF,
                               struct region_cache *CACHE,
                               ptrdiff_t START, ptrdiff_t END);

/* Indicate that a section of BUF has changed, to invalidate CACHE.
   HEAD is the number of chars unchanged at the beginning of the buffer.
   TAIL is the number of chars unchanged at the end of the buffer.
      NOTE: this is *not* the same as the ending position of modified
      region.
   (This way of specifying regions makes more sense than absolute
   buffer positions in the presence of insertions and deletions; the
   args to pass are the same before and after such an operation.)  */
extern void invalidate_region_cache (struct buffer *BUF,
                                     struct region_cache *CACHE,
                                     ptrdiff_t HEAD, ptrdiff_t TAIL);

/* The scanning functions.

   Basically, if you're scanning forward/backward from position POS,
   and region_cache_forward/backward returns true, you can skip all
   the text between POS and *NEXT.  And if the function returns false,
   you should examine all the text from POS to *NEXT, and call
   know_region_cache depending on what you find there; this way, you
   might be able to avoid scanning it again.  */

/* Return true if the text immediately after POS in BUF is known, for
   the purposes of CACHE.  If NEXT is non-zero, set *NEXT to the nearest
   position after POS where the knowledge changes.  */
extern int region_cache_forward (struct buffer *BUF,
                                 struct region_cache *CACHE,
                                 ptrdiff_t POS,
                                 ptrdiff_t *NEXT);

/* Return true if the text immediately before POS in BUF is known, for
   the purposes of CACHE.  If NEXT is non-zero, set *NEXT to the nearest
   position before POS where the knowledge changes.  */
extern int region_cache_backward (struct buffer *BUF,
                                  struct region_cache *CACHE,
                                  ptrdiff_t POS,
                                  ptrdiff_t *NEXT);
