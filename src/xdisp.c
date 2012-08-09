/* Display generation from window structure and buffer text.

Copyright (C) 1985-1988, 1993-1995, 1997-2012  Free Software Foundation, Inc.

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

/* New redisplay written by Gerd Moellmann <gerd@gnu.org>.

   Redisplay.

   Emacs separates the task of updating the display from code
   modifying global state, e.g. buffer text.  This way functions
   operating on buffers don't also have to be concerned with updating
   the display.

   Updating the display is triggered by the Lisp interpreter when it
   decides it's time to do it.  This is done either automatically for
   you as part of the interpreter's command loop or as the result of
   calling Lisp functions like `sit-for'.  The C function `redisplay'
   in xdisp.c is the only entry into the inner redisplay code.

   The following diagram shows how redisplay code is invoked.  As you
   can see, Lisp calls redisplay and vice versa.  Under window systems
   like X, some portions of the redisplay code are also called
   asynchronously during mouse movement or expose events.  It is very
   important that these code parts do NOT use the C library (malloc,
   free) because many C libraries under Unix are not reentrant.  They
   may also NOT call functions of the Lisp interpreter which could
   change the interpreter's state.  If you don't follow these rules,
   you will encounter bugs which are very hard to explain.

   +--------------+   redisplay     +----------------+
   | Lisp machine |---------------->| Redisplay code |<--+
   +--------------+   (xdisp.c)     +----------------+   |
	  ^				     |		 |
	  +----------------------------------+           |
	    Don't use this path when called		 |
	    asynchronously!				 |
                                                         |
                           expose_window (asynchronous)  |
                                                         |
			           X expose events  -----+

   What does redisplay do?  Obviously, it has to figure out somehow what
   has been changed since the last time the display has been updated,
   and to make these changes visible.  Preferably it would do that in
   a moderately intelligent way, i.e. fast.

   Changes in buffer text can be deduced from window and buffer
   structures, and from some global variables like `beg_unchanged' and
   `end_unchanged'.  The contents of the display are additionally
   recorded in a `glyph matrix', a two-dimensional matrix of glyph
   structures.  Each row in such a matrix corresponds to a line on the
   display, and each glyph in a row corresponds to a column displaying
   a character, an image, or what else.  This matrix is called the
   `current glyph matrix' or `current matrix' in redisplay
   terminology.

   For buffer parts that have been changed since the last update, a
   second glyph matrix is constructed, the so called `desired glyph
   matrix' or short `desired matrix'.  Current and desired matrix are
   then compared to find a cheap way to update the display, e.g. by
   reusing part of the display by scrolling lines.

   You will find a lot of redisplay optimizations when you start
   looking at the innards of redisplay.  The overall goal of all these
   optimizations is to make redisplay fast because it is done
   frequently.  Some of these optimizations are implemented by the
   following functions:

    . try_cursor_movement

      This function tries to update the display if the text in the
      window did not change and did not scroll, only point moved, and
      it did not move off the displayed portion of the text.

    . try_window_reusing_current_matrix

      This function reuses the current matrix of a window when text
      has not changed, but the window start changed (e.g., due to
      scrolling).

    . try_window_id

      This function attempts to redisplay a window by reusing parts of
      its existing display.  It finds and reuses the part that was not
      changed, and redraws the rest.

    . try_window

      This function performs the full redisplay of a single window
      assuming that its fonts were not changed and that the cursor
      will not end up in the scroll margins.  (Loading fonts requires
      re-adjustment of dimensions of glyph matrices, which makes this
      method impossible to use.)

   These optimizations are tried in sequence (some can be skipped if
   it is known that they are not applicable).  If none of the
   optimizations were successful, redisplay calls redisplay_windows,
   which performs a full redisplay of all windows.

   Desired matrices.

   Desired matrices are always built per Emacs window.  The function
   `display_line' is the central function to look at if you are
   interested.  It constructs one row in a desired matrix given an
   iterator structure containing both a buffer position and a
   description of the environment in which the text is to be
   displayed.  But this is too early, read on.

   Characters and pixmaps displayed for a range of buffer text depend
   on various settings of buffers and windows, on overlays and text
   properties, on display tables, on selective display.  The good news
   is that all this hairy stuff is hidden behind a small set of
   interface functions taking an iterator structure (struct it)
   argument.

   Iteration over things to be displayed is then simple.  It is
   started by initializing an iterator with a call to init_iterator,
   passing it the buffer position where to start iteration.  For
   iteration over strings, pass -1 as the position to init_iterator,
   and call reseat_to_string when the string is ready, to initialize
   the iterator for that string.  Thereafter, calls to
   get_next_display_element fill the iterator structure with relevant
   information about the next thing to display.  Calls to
   set_iterator_to_next move the iterator to the next thing.

   Besides this, an iterator also contains information about the
   display environment in which glyphs for display elements are to be
   produced.  It has fields for the width and height of the display,
   the information whether long lines are truncated or continued, a
   current X and Y position, and lots of other stuff you can better
   see in dispextern.h.

   Glyphs in a desired matrix are normally constructed in a loop
   calling get_next_display_element and then PRODUCE_GLYPHS.  The call
   to PRODUCE_GLYPHS will fill the iterator structure with pixel
   information about the element being displayed and at the same time
   produce glyphs for it.  If the display element fits on the line
   being displayed, set_iterator_to_next is called next, otherwise the
   glyphs produced are discarded.  The function display_line is the
   workhorse of filling glyph rows in the desired matrix with glyphs.
   In addition to producing glyphs, it also handles line truncation
   and continuation, word wrap, and cursor positioning (for the
   latter, see also set_cursor_from_row).

   Frame matrices.

   That just couldn't be all, could it?  What about terminal types not
   supporting operations on sub-windows of the screen?  To update the
   display on such a terminal, window-based glyph matrices are not
   well suited.  To be able to reuse part of the display (scrolling
   lines up and down), we must instead have a view of the whole
   screen.  This is what `frame matrices' are for.  They are a trick.

   Frames on terminals like above have a glyph pool.  Windows on such
   a frame sub-allocate their glyph memory from their frame's glyph
   pool.  The frame itself is given its own glyph matrices.  By
   coincidence---or maybe something else---rows in window glyph
   matrices are slices of corresponding rows in frame matrices.  Thus
   writing to window matrices implicitly updates a frame matrix which
   provides us with the view of the whole screen that we originally
   wanted to have without having to move many bytes around.  To be
   honest, there is a little bit more done, but not much more.  If you
   plan to extend that code, take a look at dispnew.c.  The function
   build_frame_matrix is a good starting point.

   Bidirectional display.

   Bidirectional display adds quite some hair to this already complex
   design.  The good news are that a large portion of that hairy stuff
   is hidden in bidi.c behind only 3 interfaces.  bidi.c implements a
   reordering engine which is called by set_iterator_to_next and
   returns the next character to display in the visual order.  See
   commentary on bidi.c for more details.  As far as redisplay is
   concerned, the effect of calling bidi_move_to_visually_next, the
   main interface of the reordering engine, is that the iterator gets
   magically placed on the buffer or string position that is to be
   displayed next.  In other words, a linear iteration through the
   buffer/string is replaced with a non-linear one.  All the rest of
   the redisplay is oblivious to the bidi reordering.

   Well, almost oblivious---there are still complications, most of
   them due to the fact that buffer and string positions no longer
   change monotonously with glyph indices in a glyph row.  Moreover,
   for continued lines, the buffer positions may not even be
   monotonously changing with vertical positions.  Also, accounting
   for face changes, overlays, etc. becomes more complex because
   non-linear iteration could potentially skip many positions with
   changes, and then cross them again on the way back...

   One other prominent effect of bidirectional display is that some
   paragraphs of text need to be displayed starting at the right
   margin of the window---the so-called right-to-left, or R2L
   paragraphs.  R2L paragraphs are displayed with R2L glyph rows,
   which have their reversed_p flag set.  The bidi reordering engine
   produces characters in such rows starting from the character which
   should be the rightmost on display.  PRODUCE_GLYPHS then reverses
   the order, when it fills up the glyph row whose reversed_p flag is
   set, by prepending each new glyph to what is already there, instead
   of appending it.  When the glyph row is complete, the function
   extend_face_to_end_of_line fills the empty space to the left of the
   leftmost character with special glyphs, which will display as,
   well, empty.  On text terminals, these special glyphs are simply
   blank characters.  On graphics terminals, there's a single stretch
   glyph of a suitably computed width.  Both the blanks and the
   stretch glyph are given the face of the background of the line.
   This way, the terminal-specific back-end can still draw the glyphs
   left to right, even for R2L lines.

   Bidirectional display and character compositions

   Some scripts cannot be displayed by drawing each character
   individually, because adjacent characters change each other's shape
   on display.  For example, Arabic and Indic scripts belong to this
   category.

   Emacs display supports this by providing "character compositions",
   most of which is implemented in composite.c.  During the buffer
   scan that delivers characters to PRODUCE_GLYPHS, if the next
   character to be delivered is a composed character, the iteration
   calls composition_reseat_it and next_element_from_composition.  If
   they succeed to compose the character with one or more of the
   following characters, the whole sequence of characters that where
   composed is recorded in the `struct composition_it' object that is
   part of the buffer iterator.  The composed sequence could produce
   one or more font glyphs (called "grapheme clusters") on the screen.
   Each of these grapheme clusters is then delivered to PRODUCE_GLYPHS
   in the direction corresponding to the current bidi scan direction
   (recorded in the scan_dir member of the `struct bidi_it' object
   that is part of the buffer iterator).  In particular, if the bidi
   iterator currently scans the buffer backwards, the grapheme
   clusters are delivered back to front.  This reorders the grapheme
   clusters as appropriate for the current bidi context.  Note that
   this means that the grapheme clusters are always stored in the
   LGSTRING object (see composite.c) in the logical order.

   Moving an iterator in bidirectional text
   without producing glyphs

   Note one important detail mentioned above: that the bidi reordering
   engine, driven by the iterator, produces characters in R2L rows
   starting at the character that will be the rightmost on display.
   As far as the iterator is concerned, the geometry of such rows is
   still left to right, i.e. the iterator "thinks" the first character
   is at the leftmost pixel position.  The iterator does not know that
   PRODUCE_GLYPHS reverses the order of the glyphs that the iterator
   delivers.  This is important when functions from the move_it_*
   family are used to get to certain screen position or to match
   screen coordinates with buffer coordinates: these functions use the
   iterator geometry, which is left to right even in R2L paragraphs.
   This works well with most callers of move_it_*, because they need
   to get to a specific column, and columns are still numbered in the
   reading order, i.e. the rightmost character in a R2L paragraph is
   still column zero.  But some callers do not get well with this; a
   notable example is mouse clicks that need to find the character
   that corresponds to certain pixel coordinates.  See
   buffer_posn_from_coords in dispnew.c for how this is handled.  */

#include <config.h>
#include <stdio.h>
#include <limits.h>
#include <setjmp.h>

#include "lisp.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "termchar.h"
#include "dispextern.h"
#include "buffer.h"
#include "character.h"
#include "charset.h"
#include "indent.h"
#include "commands.h"
#include "keymap.h"
#include "macros.h"
#include "disptab.h"
#include "termhooks.h"
#include "termopts.h"
#include "intervals.h"
#include "coding.h"
#include "process.h"
#include "region-cache.h"
#include "font.h"
#include "fontset.h"
#include "blockinput.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif
#ifdef WINDOWSNT
#include "w32term.h"
#endif
#ifdef HAVE_NS
#include "nsterm.h"
#endif
#ifdef USE_GTK
#include "gtkutil.h"
#endif

#include "font.h"

#ifndef FRAME_X_OUTPUT
#define FRAME_X_OUTPUT(f) ((f)->output_data.x)
#endif

#define INFINITY 10000000

Lisp_Object Qoverriding_local_map, Qoverriding_terminal_local_map;
Lisp_Object Qwindow_scroll_functions;
static Lisp_Object Qwindow_text_change_functions;
static Lisp_Object Qredisplay_end_trigger_functions;
Lisp_Object Qinhibit_point_motion_hooks;
static Lisp_Object QCeval, QCpropertize;
Lisp_Object QCfile, QCdata;
static Lisp_Object Qfontified;
static Lisp_Object Qgrow_only;
static Lisp_Object Qinhibit_eval_during_redisplay;
static Lisp_Object Qbuffer_position, Qposition, Qobject;
static Lisp_Object Qright_to_left, Qleft_to_right;

/* Cursor shapes */
Lisp_Object Qbar, Qhbar, Qbox, Qhollow;

/* Pointer shapes */
static Lisp_Object Qarrow, Qhand;
Lisp_Object Qtext;

/* Holds the list (error).  */
static Lisp_Object list_of_error;

static Lisp_Object Qfontification_functions;

static Lisp_Object Qwrap_prefix;
static Lisp_Object Qline_prefix;

/* Non-nil means don't actually do any redisplay.  */

Lisp_Object Qinhibit_redisplay;

/* Names of text properties relevant for redisplay.  */

Lisp_Object Qdisplay;

Lisp_Object Qspace, QCalign_to;
static Lisp_Object QCrelative_width, QCrelative_height;
Lisp_Object Qleft_margin, Qright_margin;
static Lisp_Object Qspace_width, Qraise;
static Lisp_Object Qslice;
Lisp_Object Qcenter;
static Lisp_Object Qmargin, Qpointer;
static Lisp_Object Qline_height;

#ifdef HAVE_WINDOW_SYSTEM

/* Test if overflow newline into fringe.  Called with iterator IT
   at or past right window margin, and with IT->current_x set.  */

#define IT_OVERFLOW_NEWLINE_INTO_FRINGE(IT)		\
  (!NILP (Voverflow_newline_into_fringe)		\
   && FRAME_WINDOW_P ((IT)->f)				\
   && ((IT)->bidi_it.paragraph_dir == R2L		\
       ? (WINDOW_LEFT_FRINGE_WIDTH ((IT)->w) > 0)	\
       : (WINDOW_RIGHT_FRINGE_WIDTH ((IT)->w) > 0))	\
   && (IT)->current_x == (IT)->last_visible_x		\
   && (IT)->line_wrap != WORD_WRAP)

#else /* !HAVE_WINDOW_SYSTEM */
#define IT_OVERFLOW_NEWLINE_INTO_FRINGE(it) 0
#endif /* HAVE_WINDOW_SYSTEM */

/* Test if the display element loaded in IT is a space or tab
   character.  This is used to determine word wrapping.  */

#define IT_DISPLAYING_WHITESPACE(it)				\
  (it->what == IT_CHARACTER && (it->c == ' ' || it->c == '\t'))

/* Name of the face used to highlight trailing whitespace.  */

static Lisp_Object Qtrailing_whitespace;

/* Name and number of the face used to highlight escape glyphs.  */

static Lisp_Object Qescape_glyph;

/* Name and number of the face used to highlight non-breaking spaces.  */

static Lisp_Object Qnobreak_space;

/* The symbol `image' which is the car of the lists used to represent
   images in Lisp.  Also a tool bar style.  */

Lisp_Object Qimage;

/* The image map types.  */
Lisp_Object QCmap;
static Lisp_Object QCpointer;
static Lisp_Object Qrect, Qcircle, Qpoly;

/* Tool bar styles */
Lisp_Object Qboth, Qboth_horiz, Qtext_image_horiz;

/* Non-zero means print newline to stdout before next mini-buffer
   message.  */

int noninteractive_need_newline;

/* Non-zero means print newline to message log before next message.  */

static int message_log_need_newline;

/* Three markers that message_dolog uses.
   It could allocate them itself, but that causes trouble
   in handling memory-full errors.  */
static Lisp_Object message_dolog_marker1;
static Lisp_Object message_dolog_marker2;
static Lisp_Object message_dolog_marker3;

/* The buffer position of the first character appearing entirely or
   partially on the line of the selected window which contains the
   cursor; <= 0 if not known.  Set by set_cursor_from_row, used for
   redisplay optimization in redisplay_internal.  */

static struct text_pos this_line_start_pos;

/* Number of characters past the end of the line above, including the
   terminating newline.  */

static struct text_pos this_line_end_pos;

/* The vertical positions and the height of this line.  */

static int this_line_vpos;
static int this_line_y;
static int this_line_pixel_height;

/* X position at which this display line starts.  Usually zero;
   negative if first character is partially visible.  */

static int this_line_start_x;

/* The smallest character position seen by move_it_* functions as they
   move across display lines.  Used to set MATRIX_ROW_START_CHARPOS of
   hscrolled lines, see display_line.  */

static struct text_pos this_line_min_pos;

/* Buffer that this_line_.* variables are referring to.  */

static struct buffer *this_line_buffer;


/* Values of those variables at last redisplay are stored as
   properties on `overlay-arrow-position' symbol.  However, if
   Voverlay_arrow_position is a marker, last-arrow-position is its
   numerical position.  */

static Lisp_Object Qlast_arrow_position, Qlast_arrow_string;

/* Alternative overlay-arrow-string and overlay-arrow-bitmap
   properties on a symbol in overlay-arrow-variable-list.  */

static Lisp_Object Qoverlay_arrow_string, Qoverlay_arrow_bitmap;

Lisp_Object Qmenu_bar_update_hook;

/* Nonzero if an overlay arrow has been displayed in this window.  */

static int overlay_arrow_seen;

/* Number of windows showing the buffer of the selected window (or
   another buffer with the same base buffer).  keyboard.c refers to
   this.  */

int buffer_shared;

/* Vector containing glyphs for an ellipsis `...'.  */

static Lisp_Object default_invis_vector[3];

/* This is the window where the echo area message was displayed.  It
   is always a mini-buffer window, but it may not be the same window
   currently active as a mini-buffer.  */

Lisp_Object echo_area_window;

/* List of pairs (MESSAGE . MULTIBYTE).  The function save_message
   pushes the current message and the value of
   message_enable_multibyte on the stack, the function restore_message
   pops the stack and displays MESSAGE again.  */

static Lisp_Object Vmessage_stack;

/* Nonzero means multibyte characters were enabled when the echo area
   message was specified.  */

static int message_enable_multibyte;

/* Nonzero if we should redraw the mode lines on the next redisplay.  */

int update_mode_lines;

/* Nonzero if window sizes or contents have changed since last
   redisplay that finished.  */

int windows_or_buffers_changed;

/* Nonzero means a frame's cursor type has been changed.  */

int cursor_type_changed;

/* Nonzero after display_mode_line if %l was used and it displayed a
   line number.  */

static int line_number_displayed;

/* The name of the *Messages* buffer, a string.  */

static Lisp_Object Vmessages_buffer_name;

/* Current, index 0, and last displayed echo area message.  Either
   buffers from echo_buffers, or nil to indicate no message.  */

Lisp_Object echo_area_buffer[2];

/* The buffers referenced from echo_area_buffer.  */

static Lisp_Object echo_buffer[2];

/* A vector saved used in with_area_buffer to reduce consing.  */

static Lisp_Object Vwith_echo_area_save_vector;

/* Non-zero means display_echo_area should display the last echo area
   message again.  Set by redisplay_preserve_echo_area.  */

static int display_last_displayed_message_p;

/* Nonzero if echo area is being used by print; zero if being used by
   message.  */

static int message_buf_print;

/* The symbol `inhibit-menubar-update' and its DEFVAR_BOOL variable.  */

static Lisp_Object Qinhibit_menubar_update;
static Lisp_Object Qmessage_truncate_lines;

/* Set to 1 in clear_message to make redisplay_internal aware
   of an emptied echo area.  */

static int message_cleared_p;

/* A scratch glyph row with contents used for generating truncation
   glyphs.  Also used in direct_output_for_insert.  */

#define MAX_SCRATCH_GLYPHS 100
static struct glyph_row scratch_glyph_row;
static struct glyph scratch_glyphs[MAX_SCRATCH_GLYPHS];

/* Ascent and height of the last line processed by move_it_to.  */

static int last_max_ascent, last_height;

/* Non-zero if there's a help-echo in the echo area.  */

int help_echo_showing_p;

/* If >= 0, computed, exact values of mode-line and header-line height
   to use in the macros CURRENT_MODE_LINE_HEIGHT and
   CURRENT_HEADER_LINE_HEIGHT.  */

int current_mode_line_height, current_header_line_height;

/* The maximum distance to look ahead for text properties.  Values
   that are too small let us call compute_char_face and similar
   functions too often which is expensive.  Values that are too large
   let us call compute_char_face and alike too often because we
   might not be interested in text properties that far away.  */

#define TEXT_PROP_DISTANCE_LIMIT 100

/* SAVE_IT and RESTORE_IT are called when we save a snapshot of the
   iterator state and later restore it.  This is needed because the
   bidi iterator on bidi.c keeps a stacked cache of its states, which
   is really a singleton.  When we use scratch iterator objects to
   move around the buffer, we can cause the bidi cache to be pushed or
   popped, and therefore we need to restore the cache state when we
   return to the original iterator.  */
#define SAVE_IT(ITCOPY,ITORIG,CACHE)		\
  do {						\
    if (CACHE)					\
      bidi_unshelve_cache (CACHE, 1);		\
    ITCOPY = ITORIG;				\
    CACHE = bidi_shelve_cache ();		\
  } while (0)

#define RESTORE_IT(pITORIG,pITCOPY,CACHE)	\
  do {						\
    if (pITORIG != pITCOPY)			\
      *(pITORIG) = *(pITCOPY);			\
    bidi_unshelve_cache (CACHE, 0);		\
    CACHE = NULL;				\
  } while (0)

#if GLYPH_DEBUG

/* Non-zero means print traces of redisplay if compiled with
   GLYPH_DEBUG != 0.  */

int trace_redisplay_p;

#endif /* GLYPH_DEBUG */

#ifdef DEBUG_TRACE_MOVE
/* Non-zero means trace with TRACE_MOVE to stderr.  */
int trace_move;

#define TRACE_MOVE(x)	if (trace_move) fprintf x; else (void) 0
#else
#define TRACE_MOVE(x)	(void) 0
#endif

static Lisp_Object Qauto_hscroll_mode;

/* Buffer being redisplayed -- for redisplay_window_error.  */

static struct buffer *displayed_buffer;

/* Value returned from text property handlers (see below).  */

enum prop_handled
{
  HANDLED_NORMALLY,
  HANDLED_RECOMPUTE_PROPS,
  HANDLED_OVERLAY_STRING_CONSUMED,
  HANDLED_RETURN
};

/* A description of text properties that redisplay is interested
   in.  */

struct props
{
  /* The name of the property.  */
  Lisp_Object *name;

  /* A unique index for the property.  */
  enum prop_idx idx;

  /* A handler function called to set up iterator IT from the property
     at IT's current position.  Value is used to steer handle_stop.  */
  enum prop_handled (*handler) (struct it *it);
};

static enum prop_handled handle_face_prop (struct it *);
static enum prop_handled handle_invisible_prop (struct it *);
static enum prop_handled handle_display_prop (struct it *);
static enum prop_handled handle_composition_prop (struct it *);
static enum prop_handled handle_overlay_change (struct it *);
static enum prop_handled handle_fontified_prop (struct it *);

/* Properties handled by iterators.  */

static struct props it_props[] =
{
  {&Qfontified,		FONTIFIED_PROP_IDX,	handle_fontified_prop},
  /* Handle `face' before `display' because some sub-properties of
     `display' need to know the face.  */
  {&Qface,		FACE_PROP_IDX,		handle_face_prop},
  {&Qdisplay,		DISPLAY_PROP_IDX,	handle_display_prop},
  {&Qinvisible,		INVISIBLE_PROP_IDX,	handle_invisible_prop},
  {&Qcomposition,	COMPOSITION_PROP_IDX,	handle_composition_prop},
  {NULL,		0,			NULL}
};

/* Value is the position described by X.  If X is a marker, value is
   the marker_position of X.  Otherwise, value is X.  */

#define COERCE_MARKER(X) (MARKERP ((X)) ? Fmarker_position (X) : (X))

/* Enumeration returned by some move_it_.* functions internally.  */

enum move_it_result
{
  /* Not used.  Undefined value.  */
  MOVE_UNDEFINED,

  /* Move ended at the requested buffer position or ZV.  */
  MOVE_POS_MATCH_OR_ZV,

  /* Move ended at the requested X pixel position.  */
  MOVE_X_REACHED,

  /* Move within a line ended at the end of a line that must be
     continued.  */
  MOVE_LINE_CONTINUED,

  /* Move within a line ended at the end of a line that would
     be displayed truncated.  */
  MOVE_LINE_TRUNCATED,

  /* Move within a line ended at a line end.  */
  MOVE_NEWLINE_OR_CR
};

/* This counter is used to clear the face cache every once in a while
   in redisplay_internal.  It is incremented for each redisplay.
   Every CLEAR_FACE_CACHE_COUNT full redisplays, the face cache is
   cleared.  */

#define CLEAR_FACE_CACHE_COUNT	500
static int clear_face_cache_count;

/* Similarly for the image cache.  */

#ifdef HAVE_WINDOW_SYSTEM
#define CLEAR_IMAGE_CACHE_COUNT	101
static int clear_image_cache_count;

/* Null glyph slice */
static struct glyph_slice null_glyph_slice = { 0, 0, 0, 0 };
#endif

/* Non-zero while redisplay_internal is in progress.  */

int redisplaying_p;

static Lisp_Object Qinhibit_free_realized_faces;

/* If a string, XTread_socket generates an event to display that string.
   (The display is done in read_char.)  */

Lisp_Object help_echo_string;
Lisp_Object help_echo_window;
Lisp_Object help_echo_object;
EMACS_INT help_echo_pos;

/* Temporary variable for XTread_socket.  */

Lisp_Object previous_help_echo_string;

/* Platform-independent portion of hourglass implementation. */

/* Non-zero means an hourglass cursor is currently shown.  */
int hourglass_shown_p;

/* If non-null, an asynchronous timer that, when it expires, displays
   an hourglass cursor on all frames.  */
struct atimer *hourglass_atimer;

/* Name of the face used to display glyphless characters.  */
Lisp_Object Qglyphless_char;

/* Symbol for the purpose of Vglyphless_char_display.  */
static Lisp_Object Qglyphless_char_display;

/* Method symbols for Vglyphless_char_display.  */
static Lisp_Object Qhex_code, Qempty_box, Qthin_space, Qzero_width;

/* Default pixel width of `thin-space' display method.  */
#define THIN_SPACE_WIDTH 1

/* Default number of seconds to wait before displaying an hourglass
   cursor.  */
#define DEFAULT_HOURGLASS_DELAY 1


/* Function prototypes.  */

static void setup_for_ellipsis (struct it *, int);
static void set_iterator_to_next (struct it *, int);
static void mark_window_display_accurate_1 (struct window *, int);
static int single_display_spec_string_p (Lisp_Object, Lisp_Object);
static int display_prop_string_p (Lisp_Object, Lisp_Object);
static int cursor_row_p (struct glyph_row *);
static int redisplay_mode_lines (Lisp_Object, int);
static char *decode_mode_spec_coding (Lisp_Object, char *, int);

static Lisp_Object get_it_property (struct it *it, Lisp_Object prop);

static void handle_line_prefix (struct it *);

static void pint2str (char *, int, EMACS_INT);
static void pint2hrstr (char *, int, EMACS_INT);
static struct text_pos run_window_scroll_functions (Lisp_Object,
                                                    struct text_pos);
static void reconsider_clip_changes (struct window *, struct buffer *);
static int text_outside_line_unchanged_p (struct window *,
					  EMACS_INT, EMACS_INT);
static void store_mode_line_noprop_char (char);
static int store_mode_line_noprop (const char *, int, int);
static void handle_stop (struct it *);
static void handle_stop_backwards (struct it *, EMACS_INT);
static void vmessage (const char *, va_list) ATTRIBUTE_FORMAT_PRINTF (1, 0);
static void ensure_echo_area_buffers (void);
static Lisp_Object unwind_with_echo_area_buffer (Lisp_Object);
static Lisp_Object with_echo_area_buffer_unwind_data (struct window *);
static int with_echo_area_buffer (struct window *, int,
                                  int (*) (EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT),
                                  EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT);
static void clear_garbaged_frames (void);
static int current_message_1 (EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT);
static void pop_message (void);
static int truncate_message_1 (EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT);
static void set_message (const char *, Lisp_Object, EMACS_INT, int);
static int set_message_1 (EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT);
static int display_echo_area (struct window *);
static int display_echo_area_1 (EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT);
static int resize_mini_window_1 (EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT);
static Lisp_Object unwind_redisplay (Lisp_Object);
static int string_char_and_length (const unsigned char *, int *);
static struct text_pos display_prop_end (struct it *, Lisp_Object,
                                         struct text_pos);
static int compute_window_start_on_continuation_line (struct window *);
static Lisp_Object safe_eval_handler (Lisp_Object);
static void insert_left_trunc_glyphs (struct it *);
static struct glyph_row *get_overlay_arrow_glyph_row (struct window *,
                                                      Lisp_Object);
static void extend_face_to_end_of_line (struct it *);
static int append_space_for_newline (struct it *, int);
static int cursor_row_fully_visible_p (struct window *, int, int);
static int try_scrolling (Lisp_Object, int, EMACS_INT, EMACS_INT, int, int);
static int try_cursor_movement (Lisp_Object, struct text_pos, int *);
static int trailing_whitespace_p (EMACS_INT);
static intmax_t message_log_check_duplicate (EMACS_INT, EMACS_INT);
static void push_it (struct it *, struct text_pos *);
static void iterate_out_of_display_property (struct it *);
static void pop_it (struct it *);
static void sync_frame_with_window_matrix_rows (struct window *);
static void select_frame_for_redisplay (Lisp_Object);
static void redisplay_internal (void);
static int echo_area_display (int);
static void redisplay_windows (Lisp_Object);
static void redisplay_window (Lisp_Object, int);
static Lisp_Object redisplay_window_error (Lisp_Object);
static Lisp_Object redisplay_window_0 (Lisp_Object);
static Lisp_Object redisplay_window_1 (Lisp_Object);
static int set_cursor_from_row (struct window *, struct glyph_row *,
				struct glyph_matrix *, EMACS_INT, EMACS_INT,
				int, int);
static int update_menu_bar (struct frame *, int, int);
static int try_window_reusing_current_matrix (struct window *);
static int try_window_id (struct window *);
static int display_line (struct it *);
static int display_mode_lines (struct window *);
static int display_mode_line (struct window *, enum face_id, Lisp_Object);
static int display_mode_element (struct it *, int, int, int, Lisp_Object, Lisp_Object, int);
static int store_mode_line_string (const char *, Lisp_Object, int, int, int, Lisp_Object);
static const char *decode_mode_spec (struct window *, int, int, Lisp_Object *);
static void display_menu_bar (struct window *);
static EMACS_INT display_count_lines (EMACS_INT, EMACS_INT, EMACS_INT,
				      EMACS_INT *);
static int display_string (const char *, Lisp_Object, Lisp_Object,
                           EMACS_INT, EMACS_INT, struct it *, int, int, int, int);
static void compute_line_metrics (struct it *);
static void run_redisplay_end_trigger_hook (struct it *);
static int get_overlay_strings (struct it *, EMACS_INT);
static int get_overlay_strings_1 (struct it *, EMACS_INT, int);
static void next_overlay_string (struct it *);
static void reseat (struct it *, struct text_pos, int);
static void reseat_1 (struct it *, struct text_pos, int);
static void back_to_previous_visible_line_start (struct it *);
void reseat_at_previous_visible_line_start (struct it *);
static void reseat_at_next_visible_line_start (struct it *, int);
static int next_element_from_ellipsis (struct it *);
static int next_element_from_display_vector (struct it *);
static int next_element_from_string (struct it *);
static int next_element_from_c_string (struct it *);
static int next_element_from_buffer (struct it *);
static int next_element_from_composition (struct it *);
static int next_element_from_image (struct it *);
static int next_element_from_stretch (struct it *);
static void load_overlay_strings (struct it *, EMACS_INT);
static int init_from_display_pos (struct it *, struct window *,
                                  struct display_pos *);
static void reseat_to_string (struct it *, const char *,
                              Lisp_Object, EMACS_INT, EMACS_INT, int, int);
static int get_next_display_element (struct it *);
static enum move_it_result
       move_it_in_display_line_to (struct it *, EMACS_INT, int,
				   enum move_operation_enum);
void move_it_vertically_backward (struct it *, int);
static void init_to_row_start (struct it *, struct window *,
                               struct glyph_row *);
static int init_to_row_end (struct it *, struct window *,
                            struct glyph_row *);
static void back_to_previous_line_start (struct it *);
static int forward_to_next_line_start (struct it *, int *, struct bidi_it *);
static struct text_pos string_pos_nchars_ahead (struct text_pos,
                                                Lisp_Object, EMACS_INT);
static struct text_pos string_pos (EMACS_INT, Lisp_Object);
static struct text_pos c_string_pos (EMACS_INT, const char *, int);
static EMACS_INT number_of_chars (const char *, int);
static void compute_stop_pos (struct it *);
static void compute_string_pos (struct text_pos *, struct text_pos,
                                Lisp_Object);
static int face_before_or_after_it_pos (struct it *, int);
static EMACS_INT next_overlay_change (EMACS_INT);
static int handle_display_spec (struct it *, Lisp_Object, Lisp_Object,
				Lisp_Object, struct text_pos *, EMACS_INT, int);
static int handle_single_display_spec (struct it *, Lisp_Object,
                                       Lisp_Object, Lisp_Object,
                                       struct text_pos *, EMACS_INT, int, int);
static int underlying_face_id (struct it *);
static int in_ellipses_for_invisible_text_p (struct display_pos *,
                                             struct window *);

#define face_before_it_pos(IT) face_before_or_after_it_pos ((IT), 1)
#define face_after_it_pos(IT)  face_before_or_after_it_pos ((IT), 0)

#ifdef HAVE_WINDOW_SYSTEM

static void x_consider_frame_title (Lisp_Object);
static int tool_bar_lines_needed (struct frame *, int *);
static void update_tool_bar (struct frame *, int);
static void build_desired_tool_bar_string (struct frame *f);
static int redisplay_tool_bar (struct frame *);
static void display_tool_bar_line (struct it *, int);
static void notice_overwritten_cursor (struct window *,
                                       enum glyph_row_area,
                                       int, int, int, int);
static void append_stretch_glyph (struct it *, Lisp_Object,
                                  int, int, int);


#endif /* HAVE_WINDOW_SYSTEM */

static void show_mouse_face (Mouse_HLInfo *, enum draw_glyphs_face);
static int coords_in_mouse_face_p (struct window *, int, int);



/***********************************************************************
		      Window display dimensions
 ***********************************************************************/

/* Return the bottom boundary y-position for text lines in window W.
   This is the first y position at which a line cannot start.
   It is relative to the top of the window.

   This is the height of W minus the height of a mode line, if any.  */

int
window_text_bottom_y (struct window *w)
{
  int height = WINDOW_TOTAL_HEIGHT (w);

  if (WINDOW_WANTS_MODELINE_P (w))
    height -= CURRENT_MODE_LINE_HEIGHT (w);
  return height;
}

/* Return the pixel width of display area AREA of window W.  AREA < 0
   means return the total width of W, not including fringes to
   the left and right of the window.  */

int
window_box_width (struct window *w, int area)
{
  int cols = XFASTINT (w->total_cols);
  int pixels = 0;

  if (!w->pseudo_window_p)
    {
      cols -= WINDOW_SCROLL_BAR_COLS (w);

      if (area == TEXT_AREA)
	{
	  if (INTEGERP (w->left_margin_cols))
	    cols -= XFASTINT (w->left_margin_cols);
	  if (INTEGERP (w->right_margin_cols))
	    cols -= XFASTINT (w->right_margin_cols);
	  pixels = -WINDOW_TOTAL_FRINGE_WIDTH (w);
	}
      else if (area == LEFT_MARGIN_AREA)
	{
	  cols = (INTEGERP (w->left_margin_cols)
		   ? XFASTINT (w->left_margin_cols) : 0);
	  pixels = 0;
	}
      else if (area == RIGHT_MARGIN_AREA)
	{
	  cols = (INTEGERP (w->right_margin_cols)
		   ? XFASTINT (w->right_margin_cols) : 0);
	  pixels = 0;
	}
    }

  return cols * WINDOW_FRAME_COLUMN_WIDTH (w) + pixels;
}


/* Return the pixel height of the display area of window W, not
   including mode lines of W, if any.  */

int
window_box_height (struct window *w)
{
  struct frame *f = XFRAME (w->frame);
  int height = WINDOW_TOTAL_HEIGHT (w);

  xassert (height >= 0);

  /* Note: the code below that determines the mode-line/header-line
     height is essentially the same as that contained in the macro
     CURRENT_{MODE,HEADER}_LINE_HEIGHT, except that it checks whether
     the appropriate glyph row has its `mode_line_p' flag set,
     and if it doesn't, uses estimate_mode_line_height instead.  */

  if (WINDOW_WANTS_MODELINE_P (w))
    {
      struct glyph_row *ml_row
	= (w->current_matrix && w->current_matrix->rows
	   ? MATRIX_MODE_LINE_ROW (w->current_matrix)
	   : 0);
      if (ml_row && ml_row->mode_line_p)
	height -= ml_row->height;
      else
	height -= estimate_mode_line_height (f, CURRENT_MODE_LINE_FACE_ID (w));
    }

  if (WINDOW_WANTS_HEADER_LINE_P (w))
    {
      struct glyph_row *hl_row
	= (w->current_matrix && w->current_matrix->rows
	   ? MATRIX_HEADER_LINE_ROW (w->current_matrix)
	   : 0);
      if (hl_row && hl_row->mode_line_p)
	height -= hl_row->height;
      else
	height -= estimate_mode_line_height (f, HEADER_LINE_FACE_ID);
    }

  /* With a very small font and a mode-line that's taller than
     default, we might end up with a negative height.  */
  return max (0, height);
}

/* Return the window-relative coordinate of the left edge of display
   area AREA of window W.  AREA < 0 means return the left edge of the
   whole window, to the right of the left fringe of W.  */

int
window_box_left_offset (struct window *w, int area)
{
  int x;

  if (w->pseudo_window_p)
    return 0;

  x = WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH (w);

  if (area == TEXT_AREA)
    x += (WINDOW_LEFT_FRINGE_WIDTH (w)
	  + window_box_width (w, LEFT_MARGIN_AREA));
  else if (area == RIGHT_MARGIN_AREA)
    x += (WINDOW_LEFT_FRINGE_WIDTH (w)
	  + window_box_width (w, LEFT_MARGIN_AREA)
	  + window_box_width (w, TEXT_AREA)
	  + (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
	     ? 0
	     : WINDOW_RIGHT_FRINGE_WIDTH (w)));
  else if (area == LEFT_MARGIN_AREA
	   && WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w))
    x += WINDOW_LEFT_FRINGE_WIDTH (w);

  return x;
}


/* Return the window-relative coordinate of the right edge of display
   area AREA of window W.  AREA < 0 means return the right edge of the
   whole window, to the left of the right fringe of W.  */

int
window_box_right_offset (struct window *w, int area)
{
  return window_box_left_offset (w, area) + window_box_width (w, area);
}

/* Return the frame-relative coordinate of the left edge of display
   area AREA of window W.  AREA < 0 means return the left edge of the
   whole window, to the right of the left fringe of W.  */

int
window_box_left (struct window *w, int area)
{
  struct frame *f = XFRAME (w->frame);
  int x;

  if (w->pseudo_window_p)
    return FRAME_INTERNAL_BORDER_WIDTH (f);

  x = (WINDOW_LEFT_EDGE_X (w)
       + window_box_left_offset (w, area));

  return x;
}


/* Return the frame-relative coordinate of the right edge of display
   area AREA of window W.  AREA < 0 means return the right edge of the
   whole window, to the left of the right fringe of W.  */

int
window_box_right (struct window *w, int area)
{
  return window_box_left (w, area) + window_box_width (w, area);
}

/* Get the bounding box of the display area AREA of window W, without
   mode lines, in frame-relative coordinates.  AREA < 0 means the
   whole window, not including the left and right fringes of
   the window.  Return in *BOX_X and *BOX_Y the frame-relative pixel
   coordinates of the upper-left corner of the box.  Return in
   *BOX_WIDTH, and *BOX_HEIGHT the pixel width and height of the box.  */

void
window_box (struct window *w, int area, int *box_x, int *box_y,
	    int *box_width, int *box_height)
{
  if (box_width)
    *box_width = window_box_width (w, area);
  if (box_height)
    *box_height = window_box_height (w);
  if (box_x)
    *box_x = window_box_left (w, area);
  if (box_y)
    {
      *box_y = WINDOW_TOP_EDGE_Y (w);
      if (WINDOW_WANTS_HEADER_LINE_P (w))
	*box_y += CURRENT_HEADER_LINE_HEIGHT (w);
    }
}


/* Get the bounding box of the display area AREA of window W, without
   mode lines.  AREA < 0 means the whole window, not including the
   left and right fringe of the window.  Return in *TOP_LEFT_X
   and TOP_LEFT_Y the frame-relative pixel coordinates of the
   upper-left corner of the box.  Return in *BOTTOM_RIGHT_X, and
   *BOTTOM_RIGHT_Y the coordinates of the bottom-right corner of the
   box.  */

static inline void
window_box_edges (struct window *w, int area, int *top_left_x, int *top_left_y,
		   int *bottom_right_x, int *bottom_right_y)
{
  window_box (w, area, top_left_x, top_left_y, bottom_right_x,
	      bottom_right_y);
  *bottom_right_x += *top_left_x;
  *bottom_right_y += *top_left_y;
}



/***********************************************************************
			      Utilities
 ***********************************************************************/

/* Return the bottom y-position of the line the iterator IT is in.
   This can modify IT's settings.  */

int
line_bottom_y (struct it *it)
{
  int line_height = it->max_ascent + it->max_descent;
  int line_top_y = it->current_y;

  if (line_height == 0)
    {
      if (last_height)
	line_height = last_height;
      else if (IT_CHARPOS (*it) < ZV)
	{
	  move_it_by_lines (it, 1);
	  line_height = (it->max_ascent || it->max_descent
			 ? it->max_ascent + it->max_descent
			 : last_height);
	}
      else
	{
	  struct glyph_row *row = it->glyph_row;

	  /* Use the default character height.  */
	  it->glyph_row = NULL;
	  it->what = IT_CHARACTER;
	  it->c = ' ';
	  it->len = 1;
	  PRODUCE_GLYPHS (it);
	  line_height = it->ascent + it->descent;
	  it->glyph_row = row;
	}
    }

  return line_top_y + line_height;
}

/* Subroutine of pos_visible_p below.  Extracts a display string, if
   any, from the display spec given as its argument.  */
static Lisp_Object
string_from_display_spec (Lisp_Object spec)
{
  if (CONSP (spec))
    {
      while (CONSP (spec))
	{
	  if (STRINGP (XCAR (spec)))
	    return XCAR (spec);
	  spec = XCDR (spec);
	}
    }
  else if (VECTORP (spec))
    {
      ptrdiff_t i;

      for (i = 0; i < ASIZE (spec); i++)
	{
	  if (STRINGP (AREF (spec, i)))
	    return AREF (spec, i);
	}
      return Qnil;
    }

  return spec;
}

/* Return 1 if position CHARPOS is visible in window W.
   CHARPOS < 0 means return info about WINDOW_END position.
   If visible, set *X and *Y to pixel coordinates of top left corner.
   Set *RTOP and *RBOT to pixel height of an invisible area of glyph at POS.
   Set *ROWH and *VPOS to row's visible height and VPOS (row number).  */

int
pos_visible_p (struct window *w, EMACS_INT charpos, int *x, int *y,
	       int *rtop, int *rbot, int *rowh, int *vpos)
{
  struct it it;
  void *itdata = bidi_shelve_cache ();
  struct text_pos top;
  int visible_p = 0;
  struct buffer *old_buffer = NULL;

  if (FRAME_INITIAL_P (XFRAME (WINDOW_FRAME (w))))
    return visible_p;

  if (XBUFFER (w->buffer) != current_buffer)
    {
      old_buffer = current_buffer;
      set_buffer_internal_1 (XBUFFER (w->buffer));
    }

  SET_TEXT_POS_FROM_MARKER (top, w->start);
  /* Scrolling a minibuffer window via scroll bar when the echo area
     shows long text sometimes resets the minibuffer contents behind
     our backs.  */
  if (CHARPOS (top) > ZV)
    SET_TEXT_POS (top, BEGV, BEGV_BYTE);

  /* Compute exact mode line heights.  */
  if (WINDOW_WANTS_MODELINE_P (w))
    current_mode_line_height
      = display_mode_line (w, CURRENT_MODE_LINE_FACE_ID (w),
			   BVAR (current_buffer, mode_line_format));

  if (WINDOW_WANTS_HEADER_LINE_P (w))
    current_header_line_height
      = display_mode_line (w, HEADER_LINE_FACE_ID,
			       BVAR (current_buffer, header_line_format));

  start_display (&it, w, top);
  move_it_to (&it, charpos, -1, it.last_visible_y-1, -1,
	      (charpos >= 0 ? MOVE_TO_POS : 0) | MOVE_TO_Y);

  if (charpos >= 0
      && (((!it.bidi_p || it.bidi_it.scan_dir == 1)
	   && IT_CHARPOS (it) >= charpos)
	  /* When scanning backwards under bidi iteration, move_it_to
	     stops at or _before_ CHARPOS, because it stops at or to
	     the _right_ of the character at CHARPOS. */
	  || (it.bidi_p && it.bidi_it.scan_dir == -1
	      && IT_CHARPOS (it) <= charpos)))
    {
      /* We have reached CHARPOS, or passed it.  How the call to
	 move_it_to can overshoot: (i) If CHARPOS is on invisible text
	 or covered by a display property, move_it_to stops at the end
	 of the invisible text, to the right of CHARPOS.  (ii) If
	 CHARPOS is in a display vector, move_it_to stops on its last
	 glyph.  */
      int top_x = it.current_x;
      int top_y = it.current_y;
      /* Calling line_bottom_y may change it.method, it.position, etc.  */
      enum it_method it_method = it.method;
      int bottom_y = (last_height = 0, line_bottom_y (&it));
      int window_top_y = WINDOW_HEADER_LINE_HEIGHT (w);

      if (top_y < window_top_y)
	visible_p = bottom_y > window_top_y;
      else if (top_y < it.last_visible_y)
	visible_p = 1;
      if (bottom_y >= it.last_visible_y
	  && it.bidi_p && it.bidi_it.scan_dir == -1
	  && IT_CHARPOS (it) < charpos)
	{
	  /* When the last line of the window is scanned backwards
	     under bidi iteration, we could be duped into thinking
	     that we have passed CHARPOS, when in fact move_it_to
	     simply stopped short of CHARPOS because it reached
	     last_visible_y.  To see if that's what happened, we call
	     move_it_to again with a slightly larger vertical limit,
	     and see if it actually moved vertically; if it did, we
	     didn't really reach CHARPOS, which is beyond window end.  */
	  struct it save_it = it;
	  /* Why 10? because we don't know how many canonical lines
	     will the height of the next line(s) be.  So we guess.  */
	  int ten_more_lines =
	    10 * FRAME_LINE_HEIGHT (XFRAME (WINDOW_FRAME (w)));

	  move_it_to (&it, charpos, -1, bottom_y + ten_more_lines, -1,
		      MOVE_TO_POS | MOVE_TO_Y);
	  if (it.current_y > top_y)
	    visible_p = 0;

	  it = save_it;
	}
      if (visible_p)
	{
	  if (it_method == GET_FROM_DISPLAY_VECTOR)
	    {
	      /* We stopped on the last glyph of a display vector.
		 Try and recompute.  Hack alert!  */
	      if (charpos < 2 || top.charpos >= charpos)
		top_x = it.glyph_row->x;
	      else
		{
		  struct it it2;
		  start_display (&it2, w, top);
		  move_it_to (&it2, charpos - 1, -1, -1, -1, MOVE_TO_POS);
		  get_next_display_element (&it2);
		  PRODUCE_GLYPHS (&it2);
		  if (ITERATOR_AT_END_OF_LINE_P (&it2)
		      || it2.current_x > it2.last_visible_x)
		    top_x = it.glyph_row->x;
		  else
		    {
		      top_x = it2.current_x;
		      top_y = it2.current_y;
		    }
		}
	    }
	  else if (IT_CHARPOS (it) != charpos)
	    {
	      Lisp_Object cpos = make_number (charpos);
	      Lisp_Object spec = Fget_char_property (cpos, Qdisplay, Qnil);
	      Lisp_Object string = string_from_display_spec (spec);
	      int newline_in_string = 0;

	      if (STRINGP (string))
		{
		  const char *s = SSDATA (string);
		  const char *e = s + SBYTES (string);
		  while (s < e)
		    {
		      if (*s++ == '\n')
			{
			  newline_in_string = 1;
			  break;
			}
		    }
		}
	      /* The tricky code below is needed because there's a
		 discrepancy between move_it_to and how we set cursor
		 when the display line ends in a newline from a
		 display string.  move_it_to will stop _after_ such
		 display strings, whereas set_cursor_from_row
		 conspires with cursor_row_p to place the cursor on
		 the first glyph produced from the display string.  */

	      /* We have overshoot PT because it is covered by a
		 display property whose value is a string.  If the
		 string includes embedded newlines, we are also in the
		 wrong display line.  Backtrack to the correct line,
		 where the display string begins.  */
	      if (newline_in_string)
		{
		  Lisp_Object startpos, endpos;
		  EMACS_INT start, end;
		  struct it it3;
		  int it3_moved;

		  /* Find the first and the last buffer positions
		     covered by the display string.  */
		  endpos =
		    Fnext_single_char_property_change (cpos, Qdisplay,
						       Qnil, Qnil);
		  startpos =
		    Fprevious_single_char_property_change (endpos, Qdisplay,
							   Qnil, Qnil);
		  start = XFASTINT (startpos);
		  end = XFASTINT (endpos);
		  /* Move to the last buffer position before the
		     display property.  */
		  start_display (&it3, w, top);
		  move_it_to (&it3, start - 1, -1, -1, -1, MOVE_TO_POS);
		  /* Move forward one more line if the position before
		     the display string is a newline or if it is the
		     rightmost character on a line that is
		     continued or word-wrapped.  */
		  if (it3.method == GET_FROM_BUFFER
		      && it3.c == '\n')
		    move_it_by_lines (&it3, 1);
		  else if (move_it_in_display_line_to (&it3, -1,
						       it3.current_x
						       + it3.pixel_width,
						       MOVE_TO_X)
			   == MOVE_LINE_CONTINUED)
		    {
		      move_it_by_lines (&it3, 1);
		      /* When we are under word-wrap, the #$@%!
			 move_it_by_lines moves 2 lines, so we need to
			 fix that up.  */
		      if (it3.line_wrap == WORD_WRAP)
			move_it_by_lines (&it3, -1);
		    }

		  /* Record the vertical coordinate of the display
		     line where we wound up.  */
		  top_y = it3.current_y;
		  if (it3.bidi_p)
		    {
		      /* When characters are reordered for display,
			 the character displayed to the left of the
			 display string could be _after_ the display
			 property in the logical order.  Use the
			 smallest vertical position of these two.  */
		      start_display (&it3, w, top);
		      move_it_to (&it3, end + 1, -1, -1, -1, MOVE_TO_POS);
		      if (it3.current_y < top_y)
			top_y = it3.current_y;
		    }
		  /* Move from the top of the window to the beginning
		     of the display line where the display string
		     begins.  */
		  start_display (&it3, w, top);
		  move_it_to (&it3, -1, 0, top_y, -1, MOVE_TO_X | MOVE_TO_Y);
		  /* If it3_moved stays zero after the 'while' loop
		     below, that means we already were at a newline
		     before the loop (e.g., the display string begins
		     with a newline), so we don't need to (and cannot)
		     inspect the glyphs of it3.glyph_row, because
		     PRODUCE_GLYPHS will not produce anything for a
		     newline, and thus it3.glyph_row stays at its
		     stale content it got at top of the window.  */
		  it3_moved = 0;
		  /* Finally, advance the iterator until we hit the
		     first display element whose character position is
		     CHARPOS, or until the first newline from the
		     display string, which signals the end of the
		     display line.  */
		  while (get_next_display_element (&it3))
		    {
		      PRODUCE_GLYPHS (&it3);
		      if (IT_CHARPOS (it3) == charpos
			  || ITERATOR_AT_END_OF_LINE_P (&it3))
			break;
		      it3_moved = 1;
		      set_iterator_to_next (&it3, 0);
		    }
		  top_x = it3.current_x - it3.pixel_width;
		  /* Normally, we would exit the above loop because we
		     found the display element whose character
		     position is CHARPOS.  For the contingency that we
		     didn't, and stopped at the first newline from the
		     display string, move back over the glyphs
		     produced from the string, until we find the
		     rightmost glyph not from the string.  */
		  if (it3_moved
		      && IT_CHARPOS (it3) != charpos && EQ (it3.object, string))
		    {
		      struct glyph *g = it3.glyph_row->glyphs[TEXT_AREA]
					+ it3.glyph_row->used[TEXT_AREA];

		      while (EQ ((g - 1)->object, string))
			{
			  --g;
			  top_x -= g->pixel_width;
			}
		      xassert (g < it3.glyph_row->glyphs[TEXT_AREA]
				    + it3.glyph_row->used[TEXT_AREA]);
		    }
		}
	    }

	  *x = top_x;
	  *y = max (top_y + max (0, it.max_ascent - it.ascent), window_top_y);
	  *rtop = max (0, window_top_y - top_y);
	  *rbot = max (0, bottom_y - it.last_visible_y);
	  *rowh = max (0, (min (bottom_y, it.last_visible_y)
			   - max (top_y, window_top_y)));
	  *vpos = it.vpos;
	}
    }
  else
    {
      /* We were asked to provide info about WINDOW_END.  */
      struct it it2;
      void *it2data = NULL;

      SAVE_IT (it2, it, it2data);
      if (IT_CHARPOS (it) < ZV && FETCH_BYTE (IT_BYTEPOS (it)) != '\n')
	move_it_by_lines (&it, 1);
      if (charpos < IT_CHARPOS (it)
	  || (it.what == IT_EOB && charpos == IT_CHARPOS (it)))
	{
	  visible_p = 1;
	  RESTORE_IT (&it2, &it2, it2data);
	  move_it_to (&it2, charpos, -1, -1, -1, MOVE_TO_POS);
	  *x = it2.current_x;
	  *y = it2.current_y + it2.max_ascent - it2.ascent;
	  *rtop = max (0, -it2.current_y);
	  *rbot = max (0, ((it2.current_y + it2.max_ascent + it2.max_descent)
			   - it.last_visible_y));
	  *rowh = max (0, (min (it2.current_y + it2.max_ascent + it2.max_descent,
				it.last_visible_y)
			   - max (it2.current_y,
				  WINDOW_HEADER_LINE_HEIGHT (w))));
	  *vpos = it2.vpos;
	}
      else
	bidi_unshelve_cache (it2data, 1);
    }
  bidi_unshelve_cache (itdata, 0);

  if (old_buffer)
    set_buffer_internal_1 (old_buffer);

  current_header_line_height = current_mode_line_height = -1;

  if (visible_p && XFASTINT (w->hscroll) > 0)
    *x -= XFASTINT (w->hscroll) * WINDOW_FRAME_COLUMN_WIDTH (w);

#if 0
  /* Debugging code.  */
  if (visible_p)
    fprintf (stderr, "+pv pt=%d vs=%d --> x=%d y=%d rt=%d rb=%d rh=%d vp=%d\n",
	     charpos, w->vscroll, *x, *y, *rtop, *rbot, *rowh, *vpos);
  else
    fprintf (stderr, "-pv pt=%d vs=%d\n", charpos, w->vscroll);
#endif

  return visible_p;
}


/* Return the next character from STR.  Return in *LEN the length of
   the character.  This is like STRING_CHAR_AND_LENGTH but never
   returns an invalid character.  If we find one, we return a `?', but
   with the length of the invalid character.  */

static inline int
string_char_and_length (const unsigned char *str, int *len)
{
  int c;

  c = STRING_CHAR_AND_LENGTH (str, *len);
  if (!CHAR_VALID_P (c))
    /* We may not change the length here because other places in Emacs
       don't use this function, i.e. they silently accept invalid
       characters.  */
    c = '?';

  return c;
}



/* Given a position POS containing a valid character and byte position
   in STRING, return the position NCHARS ahead (NCHARS >= 0).  */

static struct text_pos
string_pos_nchars_ahead (struct text_pos pos, Lisp_Object string, EMACS_INT nchars)
{
  xassert (STRINGP (string) && nchars >= 0);

  if (STRING_MULTIBYTE (string))
    {
      const unsigned char *p = SDATA (string) + BYTEPOS (pos);
      int len;

      while (nchars--)
	{
	  string_char_and_length (p, &len);
	  p += len;
	  CHARPOS (pos) += 1;
	  BYTEPOS (pos) += len;
	}
    }
  else
    SET_TEXT_POS (pos, CHARPOS (pos) + nchars, BYTEPOS (pos) + nchars);

  return pos;
}


/* Value is the text position, i.e. character and byte position,
   for character position CHARPOS in STRING.  */

static inline struct text_pos
string_pos (EMACS_INT charpos, Lisp_Object string)
{
  struct text_pos pos;
  xassert (STRINGP (string));
  xassert (charpos >= 0);
  SET_TEXT_POS (pos, charpos, string_char_to_byte (string, charpos));
  return pos;
}


/* Value is a text position, i.e. character and byte position, for
   character position CHARPOS in C string S.  MULTIBYTE_P non-zero
   means recognize multibyte characters.  */

static struct text_pos
c_string_pos (EMACS_INT charpos, const char *s, int multibyte_p)
{
  struct text_pos pos;

  xassert (s != NULL);
  xassert (charpos >= 0);

  if (multibyte_p)
    {
      int len;

      SET_TEXT_POS (pos, 0, 0);
      while (charpos--)
	{
	  string_char_and_length ((const unsigned char *) s, &len);
	  s += len;
	  CHARPOS (pos) += 1;
	  BYTEPOS (pos) += len;
	}
    }
  else
    SET_TEXT_POS (pos, charpos, charpos);

  return pos;
}


/* Value is the number of characters in C string S.  MULTIBYTE_P
   non-zero means recognize multibyte characters.  */

static EMACS_INT
number_of_chars (const char *s, int multibyte_p)
{
  EMACS_INT nchars;

  if (multibyte_p)
    {
      EMACS_INT rest = strlen (s);
      int len;
      const unsigned char *p = (const unsigned char *) s;

      for (nchars = 0; rest > 0; ++nchars)
	{
	  string_char_and_length (p, &len);
	  rest -= len, p += len;
	}
    }
  else
    nchars = strlen (s);

  return nchars;
}


/* Compute byte position NEWPOS->bytepos corresponding to
   NEWPOS->charpos.  POS is a known position in string STRING.
   NEWPOS->charpos must be >= POS.charpos.  */

static void
compute_string_pos (struct text_pos *newpos, struct text_pos pos, Lisp_Object string)
{
  xassert (STRINGP (string));
  xassert (CHARPOS (*newpos) >= CHARPOS (pos));

  if (STRING_MULTIBYTE (string))
    *newpos = string_pos_nchars_ahead (pos, string,
				       CHARPOS (*newpos) - CHARPOS (pos));
  else
    BYTEPOS (*newpos) = CHARPOS (*newpos);
}

/* EXPORT:
   Return an estimation of the pixel height of mode or header lines on
   frame F.  FACE_ID specifies what line's height to estimate.  */

int
estimate_mode_line_height (struct frame *f, enum face_id face_id)
{
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      int height = FONT_HEIGHT (FRAME_FONT (f));

      /* This function is called so early when Emacs starts that the face
	 cache and mode line face are not yet initialized.  */
      if (FRAME_FACE_CACHE (f))
	{
	  struct face *face = FACE_FROM_ID (f, face_id);
	  if (face)
	    {
	      if (face->font)
		height = FONT_HEIGHT (face->font);
	      if (face->box_line_width > 0)
		height += 2 * face->box_line_width;
	    }
	}

      return height;
    }
#endif

  return 1;
}

/* Given a pixel position (PIX_X, PIX_Y) on frame F, return glyph
   co-ordinates in (*X, *Y).  Set *BOUNDS to the rectangle that the
   glyph at X, Y occupies, if BOUNDS != 0.  If NOCLIP is non-zero, do
   not force the value into range.  */

void
pixel_to_glyph_coords (FRAME_PTR f, register int pix_x, register int pix_y,
		       int *x, int *y, NativeRectangle *bounds, int noclip)
{

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      /* Arrange for the division in FRAME_PIXEL_X_TO_COL etc. to round down
	 even for negative values.  */
      if (pix_x < 0)
	pix_x -= FRAME_COLUMN_WIDTH (f) - 1;
      if (pix_y < 0)
	pix_y -= FRAME_LINE_HEIGHT (f) - 1;

      pix_x = FRAME_PIXEL_X_TO_COL (f, pix_x);
      pix_y = FRAME_PIXEL_Y_TO_LINE (f, pix_y);

      if (bounds)
	STORE_NATIVE_RECT (*bounds,
			   FRAME_COL_TO_PIXEL_X (f, pix_x),
			   FRAME_LINE_TO_PIXEL_Y (f, pix_y),
			   FRAME_COLUMN_WIDTH (f) - 1,
			   FRAME_LINE_HEIGHT (f) - 1);

      if (!noclip)
	{
	  if (pix_x < 0)
	    pix_x = 0;
	  else if (pix_x > FRAME_TOTAL_COLS (f))
	    pix_x = FRAME_TOTAL_COLS (f);

	  if (pix_y < 0)
	    pix_y = 0;
	  else if (pix_y > FRAME_LINES (f))
	    pix_y = FRAME_LINES (f);
	}
    }
#endif

  *x = pix_x;
  *y = pix_y;
}


/* Find the glyph under window-relative coordinates X/Y in window W.
   Consider only glyphs from buffer text, i.e. no glyphs from overlay
   strings.  Return in *HPOS and *VPOS the row and column number of
   the glyph found.  Return in *AREA the glyph area containing X.
   Value is a pointer to the glyph found or null if X/Y is not on
   text, or we can't tell because W's current matrix is not up to
   date.  */

static
struct glyph *
x_y_to_hpos_vpos (struct window *w, int x, int y, int *hpos, int *vpos,
		  int *dx, int *dy, int *area)
{
  struct glyph *glyph, *end;
  struct glyph_row *row = NULL;
  int x0, i;

  /* Find row containing Y.  Give up if some row is not enabled.  */
  for (i = 0; i < w->current_matrix->nrows; ++i)
    {
      row = MATRIX_ROW (w->current_matrix, i);
      if (!row->enabled_p)
	return NULL;
      if (y >= row->y && y < MATRIX_ROW_BOTTOM_Y (row))
	break;
    }

  *vpos = i;
  *hpos = 0;

  /* Give up if Y is not in the window.  */
  if (i == w->current_matrix->nrows)
    return NULL;

  /* Get the glyph area containing X.  */
  if (w->pseudo_window_p)
    {
      *area = TEXT_AREA;
      x0 = 0;
    }
  else
    {
      if (x < window_box_left_offset (w, TEXT_AREA))
	{
	  *area = LEFT_MARGIN_AREA;
	  x0 = window_box_left_offset (w, LEFT_MARGIN_AREA);
	}
      else if (x < window_box_right_offset (w, TEXT_AREA))
	{
	  *area = TEXT_AREA;
	  x0 = window_box_left_offset (w, TEXT_AREA) + min (row->x, 0);
	}
      else
	{
	  *area = RIGHT_MARGIN_AREA;
	  x0 = window_box_left_offset (w, RIGHT_MARGIN_AREA);
	}
    }

  /* Find glyph containing X.  */
  glyph = row->glyphs[*area];
  end = glyph + row->used[*area];
  x -= x0;
  while (glyph < end && x >= glyph->pixel_width)
    {
      x -= glyph->pixel_width;
      ++glyph;
    }

  if (glyph == end)
    return NULL;

  if (dx)
    {
      *dx = x;
      *dy = y - (row->y + row->ascent - glyph->ascent);
    }

  *hpos = glyph - row->glyphs[*area];
  return glyph;
}

/* Convert frame-relative x/y to coordinates relative to window W.
   Takes pseudo-windows into account.  */

static void
frame_to_window_pixel_xy (struct window *w, int *x, int *y)
{
  if (w->pseudo_window_p)
    {
      /* A pseudo-window is always full-width, and starts at the
	 left edge of the frame, plus a frame border.  */
      struct frame *f = XFRAME (w->frame);
      *x -= FRAME_INTERNAL_BORDER_WIDTH (f);
      *y = FRAME_TO_WINDOW_PIXEL_Y (w, *y);
    }
  else
    {
      *x -= WINDOW_LEFT_EDGE_X (w);
      *y = FRAME_TO_WINDOW_PIXEL_Y (w, *y);
    }
}

#ifdef HAVE_WINDOW_SYSTEM

/* EXPORT:
   Return in RECTS[] at most N clipping rectangles for glyph string S.
   Return the number of stored rectangles.  */

int
get_glyph_string_clip_rects (struct glyph_string *s, NativeRectangle *rects, int n)
{
  XRectangle r;

  if (n <= 0)
    return 0;

  if (s->row->full_width_p)
    {
      /* Draw full-width.  X coordinates are relative to S->w->left_col.  */
      r.x = WINDOW_LEFT_EDGE_X (s->w);
      r.width = WINDOW_TOTAL_WIDTH (s->w);

      /* Unless displaying a mode or menu bar line, which are always
	 fully visible, clip to the visible part of the row.  */
      if (s->w->pseudo_window_p)
	r.height = s->row->visible_height;
      else
	r.height = s->height;
    }
  else
    {
      /* This is a text line that may be partially visible.  */
      r.x = window_box_left (s->w, s->area);
      r.width = window_box_width (s->w, s->area);
      r.height = s->row->visible_height;
    }

  if (s->clip_head)
    if (r.x < s->clip_head->x)
      {
	if (r.width >= s->clip_head->x - r.x)
	  r.width -= s->clip_head->x - r.x;
	else
	  r.width = 0;
	r.x = s->clip_head->x;
      }
  if (s->clip_tail)
    if (r.x + r.width > s->clip_tail->x + s->clip_tail->background_width)
      {
	if (s->clip_tail->x + s->clip_tail->background_width >= r.x)
	  r.width = s->clip_tail->x + s->clip_tail->background_width - r.x;
	else
	  r.width = 0;
      }

  /* If S draws overlapping rows, it's sufficient to use the top and
     bottom of the window for clipping because this glyph string
     intentionally draws over other lines.  */
  if (s->for_overlaps)
    {
      r.y = WINDOW_HEADER_LINE_HEIGHT (s->w);
      r.height = window_text_bottom_y (s->w) - r.y;

      /* Alas, the above simple strategy does not work for the
	 environments with anti-aliased text: if the same text is
	 drawn onto the same place multiple times, it gets thicker.
	 If the overlap we are processing is for the erased cursor, we
	 take the intersection with the rectangle of the cursor.  */
      if (s->for_overlaps & OVERLAPS_ERASED_CURSOR)
	{
	  XRectangle rc, r_save = r;

	  rc.x = WINDOW_TEXT_TO_FRAME_PIXEL_X (s->w, s->w->phys_cursor.x);
	  rc.y = s->w->phys_cursor.y;
	  rc.width = s->w->phys_cursor_width;
	  rc.height = s->w->phys_cursor_height;

	  x_intersect_rectangles (&r_save, &rc, &r);
	}
    }
  else
    {
      /* Don't use S->y for clipping because it doesn't take partially
	 visible lines into account.  For example, it can be negative for
	 partially visible lines at the top of a window.  */
      if (!s->row->full_width_p
	  && MATRIX_ROW_PARTIALLY_VISIBLE_AT_TOP_P (s->w, s->row))
	r.y = WINDOW_HEADER_LINE_HEIGHT (s->w);
      else
	r.y = max (0, s->row->y);
    }

  r.y = WINDOW_TO_FRAME_PIXEL_Y (s->w, r.y);

  /* If drawing the cursor, don't let glyph draw outside its
     advertised boundaries. Cleartype does this under some circumstances.  */
  if (s->hl == DRAW_CURSOR)
    {
      struct glyph *glyph = s->first_glyph;
      int height, max_y;

      if (s->x > r.x)
	{
	  r.width -= s->x - r.x;
	  r.x = s->x;
	}
      r.width = min (r.width, glyph->pixel_width);

      /* If r.y is below window bottom, ensure that we still see a cursor.  */
      height = min (glyph->ascent + glyph->descent,
		    min (FRAME_LINE_HEIGHT (s->f), s->row->visible_height));
      max_y = window_text_bottom_y (s->w) - height;
      max_y = WINDOW_TO_FRAME_PIXEL_Y (s->w, max_y);
      if (s->ybase - glyph->ascent > max_y)
	{
	  r.y = max_y;
	  r.height = height;
	}
      else
	{
	  /* Don't draw cursor glyph taller than our actual glyph.  */
	  height = max (FRAME_LINE_HEIGHT (s->f), glyph->ascent + glyph->descent);
	  if (height < r.height)
	    {
	      max_y = r.y + r.height;
	      r.y = min (max_y, max (r.y, s->ybase + glyph->descent - height));
	      r.height = min (max_y - r.y, height);
	    }
	}
    }

  if (s->row->clip)
    {
      XRectangle r_save = r;

      if (! x_intersect_rectangles (&r_save, s->row->clip, &r))
	r.width = 0;
    }

  if ((s->for_overlaps & OVERLAPS_BOTH) == 0
      || ((s->for_overlaps & OVERLAPS_BOTH) == OVERLAPS_BOTH && n == 1))
    {
#ifdef CONVERT_FROM_XRECT
      CONVERT_FROM_XRECT (r, *rects);
#else
      *rects = r;
#endif
      return 1;
    }
  else
    {
      /* If we are processing overlapping and allowed to return
	 multiple clipping rectangles, we exclude the row of the glyph
	 string from the clipping rectangle.  This is to avoid drawing
	 the same text on the environment with anti-aliasing.  */
#ifdef CONVERT_FROM_XRECT
      XRectangle rs[2];
#else
      XRectangle *rs = rects;
#endif
      int i = 0, row_y = WINDOW_TO_FRAME_PIXEL_Y (s->w, s->row->y);

      if (s->for_overlaps & OVERLAPS_PRED)
	{
	  rs[i] = r;
	  if (r.y + r.height > row_y)
	    {
	      if (r.y < row_y)
		rs[i].height = row_y - r.y;
	      else
		rs[i].height = 0;
	    }
	  i++;
	}
      if (s->for_overlaps & OVERLAPS_SUCC)
	{
	  rs[i] = r;
	  if (r.y < row_y + s->row->visible_height)
	    {
	      if (r.y + r.height > row_y + s->row->visible_height)
		{
		  rs[i].y = row_y + s->row->visible_height;
		  rs[i].height = r.y + r.height - rs[i].y;
		}
	      else
		rs[i].height = 0;
	    }
	  i++;
	}

      n = i;
#ifdef CONVERT_FROM_XRECT
      for (i = 0; i < n; i++)
	CONVERT_FROM_XRECT (rs[i], rects[i]);
#endif
      return n;
    }
}

/* EXPORT:
   Return in *NR the clipping rectangle for glyph string S.  */

void
get_glyph_string_clip_rect (struct glyph_string *s, NativeRectangle *nr)
{
  get_glyph_string_clip_rects (s, nr, 1);
}


/* EXPORT:
   Return the position and height of the phys cursor in window W.
   Set w->phys_cursor_width to width of phys cursor.
*/

void
get_phys_cursor_geometry (struct window *w, struct glyph_row *row,
			  struct glyph *glyph, int *xp, int *yp, int *heightp)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  int x, y, wd, h, h0, y0;

  /* Compute the width of the rectangle to draw.  If on a stretch
     glyph, and `x-stretch-block-cursor' is nil, don't draw a
     rectangle as wide as the glyph, but use a canonical character
     width instead.  */
  wd = glyph->pixel_width - 1;
#if defined (HAVE_NTGUI) || defined (HAVE_NS)
  wd++; /* Why? */
#endif

  x = w->phys_cursor.x;
  if (x < 0)
    {
      wd += x;
      x = 0;
    }

  if (glyph->type == STRETCH_GLYPH
      && !x_stretch_cursor_p)
    wd = min (FRAME_COLUMN_WIDTH (f), wd);
  w->phys_cursor_width = wd;

  y = w->phys_cursor.y + row->ascent - glyph->ascent;

  /* If y is below window bottom, ensure that we still see a cursor.  */
  h0 = min (FRAME_LINE_HEIGHT (f), row->visible_height);

  h = max (h0, glyph->ascent + glyph->descent);
  h0 = min (h0, glyph->ascent + glyph->descent);

  y0 = WINDOW_HEADER_LINE_HEIGHT (w);
  if (y < y0)
    {
      h = max (h - (y0 - y) + 1, h0);
      y = y0 - 1;
    }
  else
    {
      y0 = window_text_bottom_y (w) - h0;
      if (y > y0)
	{
	  h += y - y0;
	  y = y0;
	}
    }

  *xp = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, x);
  *yp = WINDOW_TO_FRAME_PIXEL_Y (w, y);
  *heightp = h;
}

/*
 * Remember which glyph the mouse is over.
 */

void
remember_mouse_glyph (struct frame *f, int gx, int gy, NativeRectangle *rect)
{
  Lisp_Object window;
  struct window *w;
  struct glyph_row *r, *gr, *end_row;
  enum window_part part;
  enum glyph_row_area area;
  int x, y, width, height;

  /* Try to determine frame pixel position and size of the glyph under
     frame pixel coordinates X/Y on frame F.  */

  if (!f->glyphs_initialized_p
      || (window = window_from_coordinates (f, gx, gy, &part, 0),
	  NILP (window)))
    {
      width = FRAME_SMALLEST_CHAR_WIDTH (f);
      height = FRAME_SMALLEST_FONT_HEIGHT (f);
      goto virtual_glyph;
    }

  w = XWINDOW (window);
  width = WINDOW_FRAME_COLUMN_WIDTH (w);
  height = WINDOW_FRAME_LINE_HEIGHT (w);

  x = window_relative_x_coord (w, part, gx);
  y = gy - WINDOW_TOP_EDGE_Y (w);

  r = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  end_row = MATRIX_BOTTOM_TEXT_ROW (w->current_matrix, w);

  if (w->pseudo_window_p)
    {
      area = TEXT_AREA;
      part = ON_MODE_LINE; /* Don't adjust margin. */
      goto text_glyph;
    }

  switch (part)
    {
    case ON_LEFT_MARGIN:
      area = LEFT_MARGIN_AREA;
      goto text_glyph;

    case ON_RIGHT_MARGIN:
      area = RIGHT_MARGIN_AREA;
      goto text_glyph;

    case ON_HEADER_LINE:
    case ON_MODE_LINE:
      gr = (part == ON_HEADER_LINE
	    ? MATRIX_HEADER_LINE_ROW (w->current_matrix)
	    : MATRIX_MODE_LINE_ROW (w->current_matrix));
      gy = gr->y;
      area = TEXT_AREA;
      goto text_glyph_row_found;

    case ON_TEXT:
      area = TEXT_AREA;

    text_glyph:
      gr = 0; gy = 0;
      for (; r <= end_row && r->enabled_p; ++r)
	if (r->y + r->height > y)
	  {
	    gr = r; gy = r->y;
	    break;
	  }

    text_glyph_row_found:
      if (gr && gy <= y)
	{
	  struct glyph *g = gr->glyphs[area];
	  struct glyph *end = g + gr->used[area];

	  height = gr->height;
	  for (gx = gr->x; g < end; gx += g->pixel_width, ++g)
	    if (gx + g->pixel_width > x)
	      break;

	  if (g < end)
	    {
	      if (g->type == IMAGE_GLYPH)
		{
		  /* Don't remember when mouse is over image, as
		     image may have hot-spots.  */
		  STORE_NATIVE_RECT (*rect, 0, 0, 0, 0);
		  return;
		}
	      width = g->pixel_width;
	    }
	  else
	    {
	      /* Use nominal char spacing at end of line.  */
	      x -= gx;
	      gx += (x / width) * width;
	    }

	  if (part != ON_MODE_LINE && part != ON_HEADER_LINE)
	    gx += window_box_left_offset (w, area);
	}
      else
	{
	  /* Use nominal line height at end of window.  */
	  gx = (x / width) * width;
	  y -= gy;
	  gy += (y / height) * height;
	}
      break;

    case ON_LEFT_FRINGE:
      gx = (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
	    ? WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH (w)
	    : window_box_right_offset (w, LEFT_MARGIN_AREA));
      width = WINDOW_LEFT_FRINGE_WIDTH (w);
      goto row_glyph;

    case ON_RIGHT_FRINGE:
      gx = (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
	    ? window_box_right_offset (w, RIGHT_MARGIN_AREA)
	    : window_box_right_offset (w, TEXT_AREA));
      width = WINDOW_RIGHT_FRINGE_WIDTH (w);
      goto row_glyph;

    case ON_SCROLL_BAR:
      gx = (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w)
	    ? 0
	    : (window_box_right_offset (w, RIGHT_MARGIN_AREA)
	       + (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
		  ? WINDOW_RIGHT_FRINGE_WIDTH (w)
		  : 0)));
      width = WINDOW_SCROLL_BAR_AREA_WIDTH (w);

    row_glyph:
      gr = 0, gy = 0;
      for (; r <= end_row && r->enabled_p; ++r)
	if (r->y + r->height > y)
	  {
	    gr = r; gy = r->y;
	    break;
	  }

      if (gr && gy <= y)
	height = gr->height;
      else
	{
	  /* Use nominal line height at end of window.  */
	  y -= gy;
	  gy += (y / height) * height;
	}
      break;

    default:
      ;
    virtual_glyph:
      /* If there is no glyph under the mouse, then we divide the screen
	 into a grid of the smallest glyph in the frame, and use that
	 as our "glyph".  */

      /* Arrange for the division in FRAME_PIXEL_X_TO_COL etc. to
	 round down even for negative values.  */
      if (gx < 0)
	gx -= width - 1;
      if (gy < 0)
	gy -= height - 1;

      gx = (gx / width) * width;
      gy = (gy / height) * height;

      goto store_rect;
    }

  gx += WINDOW_LEFT_EDGE_X (w);
  gy += WINDOW_TOP_EDGE_Y (w);

 store_rect:
  STORE_NATIVE_RECT (*rect, gx, gy, width, height);

  /* Visible feedback for debugging.  */
#if 0
#if HAVE_X_WINDOWS
  XDrawRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		  f->output_data.x->normal_gc,
		  gx, gy, width, height);
#endif
#endif
}


#endif /* HAVE_WINDOW_SYSTEM */


/***********************************************************************
			Lisp form evaluation
 ***********************************************************************/

/* Error handler for safe_eval and safe_call.  */

static Lisp_Object
safe_eval_handler (Lisp_Object arg)
{
  add_to_log ("Error during redisplay: %S", arg, Qnil);
  return Qnil;
}


/* Evaluate SEXPR and return the result, or nil if something went
   wrong.  Prevent redisplay during the evaluation.  */

/* Call function ARGS[0] with arguments ARGS[1] to ARGS[NARGS - 1].
   Return the result, or nil if something went wrong.  Prevent
   redisplay during the evaluation.  */

Lisp_Object
safe_call (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object val;

  if (inhibit_eval_during_redisplay)
    val = Qnil;
  else
    {
      int count = SPECPDL_INDEX ();
      struct gcpro gcpro1;

      GCPRO1 (args[0]);
      gcpro1.nvars = nargs;
      specbind (Qinhibit_redisplay, Qt);
      /* Use Qt to ensure debugger does not run,
	 so there is no possibility of wanting to redisplay.  */
      val = internal_condition_case_n (Ffuncall, nargs, args, Qt,
				       safe_eval_handler);
      UNGCPRO;
      val = unbind_to (count, val);
    }

  return val;
}


/* Call function FN with one argument ARG.
   Return the result, or nil if something went wrong.  */

Lisp_Object
safe_call1 (Lisp_Object fn, Lisp_Object arg)
{
  Lisp_Object args[2];
  args[0] = fn;
  args[1] = arg;
  return safe_call (2, args);
}

static Lisp_Object Qeval;

Lisp_Object
safe_eval (Lisp_Object sexpr)
{
  return safe_call1 (Qeval, sexpr);
}

/* Call function FN with one argument ARG.
   Return the result, or nil if something went wrong.  */

Lisp_Object
safe_call2 (Lisp_Object fn, Lisp_Object arg1, Lisp_Object arg2)
{
  Lisp_Object args[3];
  args[0] = fn;
  args[1] = arg1;
  args[2] = arg2;
  return safe_call (3, args);
}



/***********************************************************************
			      Debugging
 ***********************************************************************/

#if 0

/* Define CHECK_IT to perform sanity checks on iterators.
   This is for debugging.  It is too slow to do unconditionally.  */

static void
check_it (struct it *it)
{
  if (it->method == GET_FROM_STRING)
    {
      xassert (STRINGP (it->string));
      xassert (IT_STRING_CHARPOS (*it) >= 0);
    }
  else
    {
      xassert (IT_STRING_CHARPOS (*it) < 0);
      if (it->method == GET_FROM_BUFFER)
	{
	  /* Check that character and byte positions agree.  */
	  xassert (IT_CHARPOS (*it) == BYTE_TO_CHAR (IT_BYTEPOS (*it)));
	}
    }

  if (it->dpvec)
    xassert (it->current.dpvec_index >= 0);
  else
    xassert (it->current.dpvec_index < 0);
}

#define CHECK_IT(IT)	check_it ((IT))

#else /* not 0 */

#define CHECK_IT(IT)	(void) 0

#endif /* not 0 */


#if GLYPH_DEBUG && XASSERTS

/* Check that the window end of window W is what we expect it
   to be---the last row in the current matrix displaying text.  */

static void
check_window_end (struct window *w)
{
  if (!MINI_WINDOW_P (w)
      && !NILP (w->window_end_valid))
    {
      struct glyph_row *row;
      xassert ((row = MATRIX_ROW (w->current_matrix,
				  XFASTINT (w->window_end_vpos)),
		!row->enabled_p
		|| MATRIX_ROW_DISPLAYS_TEXT_P (row)
		|| MATRIX_ROW_VPOS (row, w->current_matrix) == 0));
    }
}

#define CHECK_WINDOW_END(W)	check_window_end ((W))

#else

#define CHECK_WINDOW_END(W)	(void) 0

#endif



/***********************************************************************
		       Iterator initialization
 ***********************************************************************/

/* Initialize IT for displaying current_buffer in window W, starting
   at character position CHARPOS.  CHARPOS < 0 means that no buffer
   position is specified which is useful when the iterator is assigned
   a position later.  BYTEPOS is the byte position corresponding to
   CHARPOS.  BYTEPOS < 0 means compute it from CHARPOS.

   If ROW is not null, calls to produce_glyphs with IT as parameter
   will produce glyphs in that row.

   BASE_FACE_ID is the id of a base face to use.  It must be one of
   DEFAULT_FACE_ID for normal text, MODE_LINE_FACE_ID,
   MODE_LINE_INACTIVE_FACE_ID, or HEADER_LINE_FACE_ID for displaying
   mode lines, or TOOL_BAR_FACE_ID for displaying the tool-bar.

   If ROW is null and BASE_FACE_ID is equal to MODE_LINE_FACE_ID,
   MODE_LINE_INACTIVE_FACE_ID, or HEADER_LINE_FACE_ID, the iterator
   will be initialized to use the corresponding mode line glyph row of
   the desired matrix of W.  */

void
init_iterator (struct it *it, struct window *w,
	       EMACS_INT charpos, EMACS_INT bytepos,
	       struct glyph_row *row, enum face_id base_face_id)
{
  int highlight_region_p;
  enum face_id remapped_base_face_id = base_face_id;

  /* Some precondition checks.  */
  xassert (w != NULL && it != NULL);
  xassert (charpos < 0 || (charpos >= BUF_BEG (current_buffer)
			   && charpos <= ZV));

  /* If face attributes have been changed since the last redisplay,
     free realized faces now because they depend on face definitions
     that might have changed.  Don't free faces while there might be
     desired matrices pending which reference these faces.  */
  if (face_change_count && !inhibit_free_realized_faces)
    {
      face_change_count = 0;
      free_all_realized_faces (Qnil);
    }

  /* Perhaps remap BASE_FACE_ID to a user-specified alternative.  */
  if (! NILP (Vface_remapping_alist))
    remapped_base_face_id = lookup_basic_face (XFRAME (w->frame), base_face_id);

  /* Use one of the mode line rows of W's desired matrix if
     appropriate.  */
  if (row == NULL)
    {
      if (base_face_id == MODE_LINE_FACE_ID
	  || base_face_id == MODE_LINE_INACTIVE_FACE_ID)
	row = MATRIX_MODE_LINE_ROW (w->desired_matrix);
      else if (base_face_id == HEADER_LINE_FACE_ID)
	row = MATRIX_HEADER_LINE_ROW (w->desired_matrix);
    }

  /* Clear IT.  */
  memset (it, 0, sizeof *it);
  it->current.overlay_string_index = -1;
  it->current.dpvec_index = -1;
  it->base_face_id = remapped_base_face_id;
  it->string = Qnil;
  IT_STRING_CHARPOS (*it) = IT_STRING_BYTEPOS (*it) = -1;
  it->paragraph_embedding = L2R;
  it->bidi_it.string.lstring = Qnil;
  it->bidi_it.string.s = NULL;
  it->bidi_it.string.bufpos = 0;

  /* The window in which we iterate over current_buffer:  */
  XSETWINDOW (it->window, w);
  it->w = w;
  it->f = XFRAME (w->frame);

  it->cmp_it.id = -1;

  /* Extra space between lines (on window systems only).  */
  if (base_face_id == DEFAULT_FACE_ID
      && FRAME_WINDOW_P (it->f))
    {
      if (NATNUMP (BVAR (current_buffer, extra_line_spacing)))
	it->extra_line_spacing = XFASTINT (BVAR (current_buffer, extra_line_spacing));
      else if (FLOATP (BVAR (current_buffer, extra_line_spacing)))
	it->extra_line_spacing = (XFLOAT_DATA (BVAR (current_buffer, extra_line_spacing))
				  * FRAME_LINE_HEIGHT (it->f));
      else if (it->f->extra_line_spacing > 0)
	it->extra_line_spacing = it->f->extra_line_spacing;
      it->max_extra_line_spacing = 0;
    }

  /* If realized faces have been removed, e.g. because of face
     attribute changes of named faces, recompute them.  When running
     in batch mode, the face cache of the initial frame is null.  If
     we happen to get called, make a dummy face cache.  */
  if (FRAME_FACE_CACHE (it->f) == NULL)
    init_frame_faces (it->f);
  if (FRAME_FACE_CACHE (it->f)->used == 0)
    recompute_basic_faces (it->f);

  /* Current value of the `slice', `space-width', and 'height' properties.  */
  it->slice.x = it->slice.y = it->slice.width = it->slice.height = Qnil;
  it->space_width = Qnil;
  it->font_height = Qnil;
  it->override_ascent = -1;

  /* Are control characters displayed as `^C'?  */
  it->ctl_arrow_p = !NILP (BVAR (current_buffer, ctl_arrow));

  /* -1 means everything between a CR and the following line end
     is invisible.  >0 means lines indented more than this value are
     invisible.  */
  it->selective = (INTEGERP (BVAR (current_buffer, selective_display))
		   ? XINT (BVAR (current_buffer, selective_display))
		   : (!NILP (BVAR (current_buffer, selective_display))
		      ? -1 : 0));
  it->selective_display_ellipsis_p
    = !NILP (BVAR (current_buffer, selective_display_ellipses));

  /* Display table to use.  */
  it->dp = window_display_table (w);

  /* Are multibyte characters enabled in current_buffer?  */
  it->multibyte_p = !NILP (BVAR (current_buffer, enable_multibyte_characters));

  /* Non-zero if we should highlight the region.  */
  highlight_region_p
    = (!NILP (Vtransient_mark_mode)
       && !NILP (BVAR (current_buffer, mark_active))
       && XMARKER (BVAR (current_buffer, mark))->buffer != 0);

  /* Set IT->region_beg_charpos and IT->region_end_charpos to the
     start and end of a visible region in window IT->w.  Set both to
     -1 to indicate no region.  */
  if (highlight_region_p
      /* Maybe highlight only in selected window.  */
      && (/* Either show region everywhere.  */
	  highlight_nonselected_windows
	  /* Or show region in the selected window.  */
	  || w == XWINDOW (selected_window)
	  /* Or show the region if we are in the mini-buffer and W is
	     the window the mini-buffer refers to.  */
	  || (MINI_WINDOW_P (XWINDOW (selected_window))
	      && WINDOWP (minibuf_selected_window)
	      && w == XWINDOW (minibuf_selected_window))))
    {
      EMACS_INT markpos = marker_position (BVAR (current_buffer, mark));
      it->region_beg_charpos = min (PT, markpos);
      it->region_end_charpos = max (PT, markpos);
    }
  else
    it->region_beg_charpos = it->region_end_charpos = -1;

  /* Get the position at which the redisplay_end_trigger hook should
     be run, if it is to be run at all.  */
  if (MARKERP (w->redisplay_end_trigger)
      && XMARKER (w->redisplay_end_trigger)->buffer != 0)
    it->redisplay_end_trigger_charpos
      = marker_position (w->redisplay_end_trigger);
  else if (INTEGERP (w->redisplay_end_trigger))
    it->redisplay_end_trigger_charpos = XINT (w->redisplay_end_trigger);

  it->tab_width = SANE_TAB_WIDTH (current_buffer);

  /* Are lines in the display truncated?  */
  if (base_face_id != DEFAULT_FACE_ID
      || XINT (it->w->hscroll)
      || (! WINDOW_FULL_WIDTH_P (it->w)
	  && ((!NILP (Vtruncate_partial_width_windows)
	       && !INTEGERP (Vtruncate_partial_width_windows))
	      || (INTEGERP (Vtruncate_partial_width_windows)
		  && (WINDOW_TOTAL_COLS (it->w)
		      < XINT (Vtruncate_partial_width_windows))))))
    it->line_wrap = TRUNCATE;
  else if (NILP (BVAR (current_buffer, truncate_lines)))
    it->line_wrap = NILP (BVAR (current_buffer, word_wrap))
      ? WINDOW_WRAP : WORD_WRAP;
  else
    it->line_wrap = TRUNCATE;

  /* Get dimensions of truncation and continuation glyphs.  These are
     displayed as fringe bitmaps under X, so we don't need them for such
     frames.  */
  if (!FRAME_WINDOW_P (it->f))
    {
      if (it->line_wrap == TRUNCATE)
	{
	  /* We will need the truncation glyph.  */
	  xassert (it->glyph_row == NULL);
	  produce_special_glyphs (it, IT_TRUNCATION);
	  it->truncation_pixel_width = it->pixel_width;
	}
      else
	{
	  /* We will need the continuation glyph.  */
	  xassert (it->glyph_row == NULL);
	  produce_special_glyphs (it, IT_CONTINUATION);
	  it->continuation_pixel_width = it->pixel_width;
	}

      /* Reset these values to zero because the produce_special_glyphs
	 above has changed them.  */
      it->pixel_width = it->ascent = it->descent = 0;
      it->phys_ascent = it->phys_descent = 0;
    }

  /* Set this after getting the dimensions of truncation and
     continuation glyphs, so that we don't produce glyphs when calling
     produce_special_glyphs, above.  */
  it->glyph_row = row;
  it->area = TEXT_AREA;

  /* Forget any previous info about this row being reversed.  */
  if (it->glyph_row)
    it->glyph_row->reversed_p = 0;

  /* Get the dimensions of the display area.  The display area
     consists of the visible window area plus a horizontally scrolled
     part to the left of the window.  All x-values are relative to the
     start of this total display area.  */
  if (base_face_id != DEFAULT_FACE_ID)
    {
      /* Mode lines, menu bar in terminal frames.  */
      it->first_visible_x = 0;
      it->last_visible_x = WINDOW_TOTAL_WIDTH (w);
    }
  else
    {
      it->first_visible_x
	= XFASTINT (it->w->hscroll) * FRAME_COLUMN_WIDTH (it->f);
      it->last_visible_x = (it->first_visible_x
			    + window_box_width (w, TEXT_AREA));

      /* If we truncate lines, leave room for the truncator glyph(s) at
	 the right margin.  Otherwise, leave room for the continuation
	 glyph(s).  Truncation and continuation glyphs are not inserted
	 for window-based redisplay.  */
      if (!FRAME_WINDOW_P (it->f))
	{
	  if (it->line_wrap == TRUNCATE)
	    it->last_visible_x -= it->truncation_pixel_width;
	  else
	    it->last_visible_x -= it->continuation_pixel_width;
	}

      it->header_line_p = WINDOW_WANTS_HEADER_LINE_P (w);
      it->current_y = WINDOW_HEADER_LINE_HEIGHT (w) + w->vscroll;
    }

  /* Leave room for a border glyph.  */
  if (!FRAME_WINDOW_P (it->f)
      && !WINDOW_RIGHTMOST_P (it->w))
    it->last_visible_x -= 1;

  it->last_visible_y = window_text_bottom_y (w);

  /* For mode lines and alike, arrange for the first glyph having a
     left box line if the face specifies a box.  */
  if (base_face_id != DEFAULT_FACE_ID)
    {
      struct face *face;

      it->face_id = remapped_base_face_id;

      /* If we have a boxed mode line, make the first character appear
	 with a left box line.  */
      face = FACE_FROM_ID (it->f, remapped_base_face_id);
      if (face->box != FACE_NO_BOX)
	it->start_of_box_run_p = 1;
    }

  /* If a buffer position was specified, set the iterator there,
     getting overlays and face properties from that position.  */
  if (charpos >= BUF_BEG (current_buffer))
    {
      it->end_charpos = ZV;
      IT_CHARPOS (*it) = charpos;

      /* We will rely on `reseat' to set this up properly, via
	 handle_face_prop.  */
      it->face_id = it->base_face_id;

      /* Compute byte position if not specified.  */
      if (bytepos < charpos)
	IT_BYTEPOS (*it) = CHAR_TO_BYTE (charpos);
      else
	IT_BYTEPOS (*it) = bytepos;

      it->start = it->current;
      /* Do we need to reorder bidirectional text?  Not if this is a
	 unibyte buffer: by definition, none of the single-byte
	 characters are strong R2L, so no reordering is needed.  And
	 bidi.c doesn't support unibyte buffers anyway.  Also, don't
	 reorder while we are loading loadup.el, since the tables of
	 character properties needed for reordering are not yet
	 available.  */
      it->bidi_p =
	NILP (Vpurify_flag)
	&& !NILP (BVAR (current_buffer, bidi_display_reordering))
	&& it->multibyte_p;

      /* If we are to reorder bidirectional text, init the bidi
	 iterator.  */
      if (it->bidi_p)
	{
	  /* Note the paragraph direction that this buffer wants to
	     use.  */
	  if (EQ (BVAR (current_buffer, bidi_paragraph_direction),
		  Qleft_to_right))
	    it->paragraph_embedding = L2R;
	  else if (EQ (BVAR (current_buffer, bidi_paragraph_direction),
		       Qright_to_left))
	    it->paragraph_embedding = R2L;
	  else
	    it->paragraph_embedding = NEUTRAL_DIR;
	  bidi_unshelve_cache (NULL, 0);
	  bidi_init_it (charpos, IT_BYTEPOS (*it), FRAME_WINDOW_P (it->f),
			&it->bidi_it);
	}

      /* Compute faces etc.  */
      reseat (it, it->current.pos, 1);
    }

  CHECK_IT (it);
}


/* Initialize IT for the display of window W with window start POS.  */

void
start_display (struct it *it, struct window *w, struct text_pos pos)
{
  struct glyph_row *row;
  int first_vpos = WINDOW_WANTS_HEADER_LINE_P (w) ? 1 : 0;

  row = w->desired_matrix->rows + first_vpos;
  init_iterator (it, w, CHARPOS (pos), BYTEPOS (pos), row, DEFAULT_FACE_ID);
  it->first_vpos = first_vpos;

  /* Don't reseat to previous visible line start if current start
     position is in a string or image.  */
  if (it->method == GET_FROM_BUFFER && it->line_wrap != TRUNCATE)
    {
      int start_at_line_beg_p;
      int first_y = it->current_y;

      /* If window start is not at a line start, skip forward to POS to
	 get the correct continuation lines width.  */
      start_at_line_beg_p = (CHARPOS (pos) == BEGV
			     || FETCH_BYTE (BYTEPOS (pos) - 1) == '\n');
      if (!start_at_line_beg_p)
	{
	  int new_x;

	  reseat_at_previous_visible_line_start (it);
	  move_it_to (it, CHARPOS (pos), -1, -1, -1, MOVE_TO_POS);

	  new_x = it->current_x + it->pixel_width;

	  /* If lines are continued, this line may end in the middle
	     of a multi-glyph character (e.g. a control character
	     displayed as \003, or in the middle of an overlay
	     string).  In this case move_it_to above will not have
	     taken us to the start of the continuation line but to the
	     end of the continued line.  */
	  if (it->current_x > 0
	      && it->line_wrap != TRUNCATE /* Lines are continued.  */
	      && (/* And glyph doesn't fit on the line.  */
		  new_x > it->last_visible_x
		  /* Or it fits exactly and we're on a window
		     system frame.  */
		  || (new_x == it->last_visible_x
		      && FRAME_WINDOW_P (it->f))))
	    {
	      if ((it->current.dpvec_index >= 0
		   || it->current.overlay_string_index >= 0)
		  /* If we are on a newline from a display vector or
		     overlay string, then we are already at the end of
		     a screen line; no need to go to the next line in
		     that case, as this line is not really continued.
		     (If we do go to the next line, C-e will not DTRT.)  */
		  && it->c != '\n')
		{
		  set_iterator_to_next (it, 1);
		  move_it_in_display_line_to (it, -1, -1, 0);
		}

	      it->continuation_lines_width += it->current_x;
	    }
	  /* If the character at POS is displayed via a display
	     vector, move_it_to above stops at the final glyph of
	     IT->dpvec.  To make the caller redisplay that character
	     again (a.k.a. start at POS), we need to reset the
	     dpvec_index to the beginning of IT->dpvec.  */
	  else if (it->current.dpvec_index >= 0)
	    it->current.dpvec_index = 0;

	  /* We're starting a new display line, not affected by the
	     height of the continued line, so clear the appropriate
	     fields in the iterator structure.  */
	  it->max_ascent = it->max_descent = 0;
	  it->max_phys_ascent = it->max_phys_descent = 0;

	  it->current_y = first_y;
	  it->vpos = 0;
	  it->current_x = it->hpos = 0;
	}
    }
}


/* Return 1 if POS is a position in ellipses displayed for invisible
   text.  W is the window we display, for text property lookup.  */

static int
in_ellipses_for_invisible_text_p (struct display_pos *pos, struct window *w)
{
  Lisp_Object prop, window;
  int ellipses_p = 0;
  EMACS_INT charpos = CHARPOS (pos->pos);

  /* If POS specifies a position in a display vector, this might
     be for an ellipsis displayed for invisible text.  We won't
     get the iterator set up for delivering that ellipsis unless
     we make sure that it gets aware of the invisible text.  */
  if (pos->dpvec_index >= 0
      && pos->overlay_string_index < 0
      && CHARPOS (pos->string_pos) < 0
      && charpos > BEGV
      && (XSETWINDOW (window, w),
	  prop = Fget_char_property (make_number (charpos),
				     Qinvisible, window),
	  !TEXT_PROP_MEANS_INVISIBLE (prop)))
    {
      prop = Fget_char_property (make_number (charpos - 1), Qinvisible,
				 window);
      ellipses_p = 2 == TEXT_PROP_MEANS_INVISIBLE (prop);
    }

  return ellipses_p;
}


/* Initialize IT for stepping through current_buffer in window W,
   starting at position POS that includes overlay string and display
   vector/ control character translation position information.  Value
   is zero if there are overlay strings with newlines at POS.  */

static int
init_from_display_pos (struct it *it, struct window *w, struct display_pos *pos)
{
  EMACS_INT charpos = CHARPOS (pos->pos), bytepos = BYTEPOS (pos->pos);
  int i, overlay_strings_with_newlines = 0;

  /* If POS specifies a position in a display vector, this might
     be for an ellipsis displayed for invisible text.  We won't
     get the iterator set up for delivering that ellipsis unless
     we make sure that it gets aware of the invisible text.  */
  if (in_ellipses_for_invisible_text_p (pos, w))
    {
      --charpos;
      bytepos = 0;
    }

  /* Keep in mind: the call to reseat in init_iterator skips invisible
     text, so we might end up at a position different from POS.  This
     is only a problem when POS is a row start after a newline and an
     overlay starts there with an after-string, and the overlay has an
     invisible property.  Since we don't skip invisible text in
     display_line and elsewhere immediately after consuming the
     newline before the row start, such a POS will not be in a string,
     but the call to init_iterator below will move us to the
     after-string.  */
  init_iterator (it, w, charpos, bytepos, NULL, DEFAULT_FACE_ID);

  /* This only scans the current chunk -- it should scan all chunks.
     However, OVERLAY_STRING_CHUNK_SIZE has been increased from 3 in 21.1
     to 16 in 22.1 to make this a lesser problem.  */
  for (i = 0; i < it->n_overlay_strings && i < OVERLAY_STRING_CHUNK_SIZE; ++i)
    {
      const char *s = SSDATA (it->overlay_strings[i]);
      const char *e = s + SBYTES (it->overlay_strings[i]);

      while (s < e && *s != '\n')
	++s;

      if (s < e)
	{
	  overlay_strings_with_newlines = 1;
	  break;
	}
    }

  /* If position is within an overlay string, set up IT to the right
     overlay string.  */
  if (pos->overlay_string_index >= 0)
    {
      int relative_index;

      /* If the first overlay string happens to have a `display'
	 property for an image, the iterator will be set up for that
	 image, and we have to undo that setup first before we can
	 correct the overlay string index.  */
      if (it->method == GET_FROM_IMAGE)
	pop_it (it);

      /* We already have the first chunk of overlay strings in
	 IT->overlay_strings.  Load more until the one for
	 pos->overlay_string_index is in IT->overlay_strings.  */
      if (pos->overlay_string_index >= OVERLAY_STRING_CHUNK_SIZE)
	{
	  int n = pos->overlay_string_index / OVERLAY_STRING_CHUNK_SIZE;
	  it->current.overlay_string_index = 0;
	  while (n--)
	    {
	      load_overlay_strings (it, 0);
	      it->current.overlay_string_index += OVERLAY_STRING_CHUNK_SIZE;
	    }
	}

      it->current.overlay_string_index = pos->overlay_string_index;
      relative_index = (it->current.overlay_string_index
			% OVERLAY_STRING_CHUNK_SIZE);
      it->string = it->overlay_strings[relative_index];
      xassert (STRINGP (it->string));
      it->current.string_pos = pos->string_pos;
      it->method = GET_FROM_STRING;
    }

  if (CHARPOS (pos->string_pos) >= 0)
    {
      /* Recorded position is not in an overlay string, but in another
	 string.  This can only be a string from a `display' property.
	 IT should already be filled with that string.  */
      it->current.string_pos = pos->string_pos;
      xassert (STRINGP (it->string));
    }

  /* Restore position in display vector translations, control
     character translations or ellipses.  */
  if (pos->dpvec_index >= 0)
    {
      if (it->dpvec == NULL)
	get_next_display_element (it);
      xassert (it->dpvec && it->current.dpvec_index == 0);
      it->current.dpvec_index = pos->dpvec_index;
    }

  CHECK_IT (it);
  return !overlay_strings_with_newlines;
}


/* Initialize IT for stepping through current_buffer in window W
   starting at ROW->start.  */

static void
init_to_row_start (struct it *it, struct window *w, struct glyph_row *row)
{
  init_from_display_pos (it, w, &row->start);
  it->start = row->start;
  it->continuation_lines_width = row->continuation_lines_width;
  CHECK_IT (it);
}


/* Initialize IT for stepping through current_buffer in window W
   starting in the line following ROW, i.e. starting at ROW->end.
   Value is zero if there are overlay strings with newlines at ROW's
   end position.  */

static int
init_to_row_end (struct it *it, struct window *w, struct glyph_row *row)
{
  int success = 0;

  if (init_from_display_pos (it, w, &row->end))
    {
      if (row->continued_p)
	it->continuation_lines_width
	  = row->continuation_lines_width + row->pixel_width;
      CHECK_IT (it);
      success = 1;
    }

  return success;
}




/***********************************************************************
			   Text properties
 ***********************************************************************/

/* Called when IT reaches IT->stop_charpos.  Handle text property and
   overlay changes.  Set IT->stop_charpos to the next position where
   to stop.  */

static void
handle_stop (struct it *it)
{
  enum prop_handled handled;
  int handle_overlay_change_p;
  struct props *p;

  it->dpvec = NULL;
  it->current.dpvec_index = -1;
  handle_overlay_change_p = !it->ignore_overlay_strings_at_pos_p;
  it->ignore_overlay_strings_at_pos_p = 0;
  it->ellipsis_p = 0;

  /* Use face of preceding text for ellipsis (if invisible) */
  if (it->selective_display_ellipsis_p)
    it->saved_face_id = it->face_id;

  do
    {
      handled = HANDLED_NORMALLY;

      /* Call text property handlers.  */
      for (p = it_props; p->handler; ++p)
	{
	  handled = p->handler (it);

	  if (handled == HANDLED_RECOMPUTE_PROPS)
	    break;
	  else if (handled == HANDLED_RETURN)
	    {
	      /* We still want to show before and after strings from
		 overlays even if the actual buffer text is replaced.  */
	      if (!handle_overlay_change_p
		  || it->sp > 1
		  /* Don't call get_overlay_strings_1 if we already
		     have overlay strings loaded, because doing so
		     will load them again and push the iterator state
		     onto the stack one more time, which is not
		     expected by the rest of the code that processes
		     overlay strings.  */
		  || (it->current.overlay_string_index < 0
		      ? !get_overlay_strings_1 (it, 0, 0)
		      : 0))
		{
		  if (it->ellipsis_p)
		    setup_for_ellipsis (it, 0);
		  /* When handling a display spec, we might load an
		     empty string.  In that case, discard it here.  We
		     used to discard it in handle_single_display_spec,
		     but that causes get_overlay_strings_1, above, to
		     ignore overlay strings that we must check.  */
		  if (STRINGP (it->string) && !SCHARS (it->string))
		    pop_it (it);
		  return;
		}
	      else if (STRINGP (it->string) && !SCHARS (it->string))
		pop_it (it);
	      else
		{
		  it->ignore_overlay_strings_at_pos_p = 1;
		  it->string_from_display_prop_p = 0;
		  it->from_disp_prop_p = 0;
		  handle_overlay_change_p = 0;
		}
	      handled = HANDLED_RECOMPUTE_PROPS;
	      break;
	    }
	  else if (handled == HANDLED_OVERLAY_STRING_CONSUMED)
	    handle_overlay_change_p = 0;
	}

      if (handled != HANDLED_RECOMPUTE_PROPS)
	{
	  /* Don't check for overlay strings below when set to deliver
	     characters from a display vector.  */
	  if (it->method == GET_FROM_DISPLAY_VECTOR)
	    handle_overlay_change_p = 0;

	  /* Handle overlay changes.
	     This sets HANDLED to HANDLED_RECOMPUTE_PROPS
	     if it finds overlays.  */
	  if (handle_overlay_change_p)
	    handled = handle_overlay_change (it);
	}

      if (it->ellipsis_p)
	{
	  setup_for_ellipsis (it, 0);
	  break;
	}
    }
  while (handled == HANDLED_RECOMPUTE_PROPS);

  /* Determine where to stop next.  */
  if (handled == HANDLED_NORMALLY)
    compute_stop_pos (it);
}


/* Compute IT->stop_charpos from text property and overlay change
   information for IT's current position.  */

static void
compute_stop_pos (struct it *it)
{
  register INTERVAL iv, next_iv;
  Lisp_Object object, limit, position;
  EMACS_INT charpos, bytepos;

  if (STRINGP (it->string))
    {
      /* Strings are usually short, so don't limit the search for
	 properties.  */
      it->stop_charpos = it->end_charpos;
      object = it->string;
      limit = Qnil;
      charpos = IT_STRING_CHARPOS (*it);
      bytepos = IT_STRING_BYTEPOS (*it);
    }
  else
    {
      EMACS_INT pos;

      /* If end_charpos is out of range for some reason, such as a
	 misbehaving display function, rationalize it (Bug#5984).  */
      if (it->end_charpos > ZV)
	it->end_charpos = ZV;
      it->stop_charpos = it->end_charpos;

      /* If next overlay change is in front of the current stop pos
	 (which is IT->end_charpos), stop there.  Note: value of
	 next_overlay_change is point-max if no overlay change
	 follows.  */
      charpos = IT_CHARPOS (*it);
      bytepos = IT_BYTEPOS (*it);
      pos = next_overlay_change (charpos);
      if (pos < it->stop_charpos)
	it->stop_charpos = pos;

      /* If showing the region, we have to stop at the region
	 start or end because the face might change there.  */
      if (it->region_beg_charpos > 0)
	{
	  if (IT_CHARPOS (*it) < it->region_beg_charpos)
	    it->stop_charpos = min (it->stop_charpos, it->region_beg_charpos);
	  else if (IT_CHARPOS (*it) < it->region_end_charpos)
	    it->stop_charpos = min (it->stop_charpos, it->region_end_charpos);
	}

      /* Set up variables for computing the stop position from text
         property changes.  */
      XSETBUFFER (object, current_buffer);
      limit = make_number (IT_CHARPOS (*it) + TEXT_PROP_DISTANCE_LIMIT);
    }

  /* Get the interval containing IT's position.  Value is a null
     interval if there isn't such an interval.  */
  position = make_number (charpos);
  iv = validate_interval_range (object, &position, &position, 0);
  if (!NULL_INTERVAL_P (iv))
    {
      Lisp_Object values_here[LAST_PROP_IDX];
      struct props *p;

      /* Get properties here.  */
      for (p = it_props; p->handler; ++p)
	values_here[p->idx] = textget (iv->plist, *p->name);

      /* Look for an interval following iv that has different
	 properties.  */
      for (next_iv = next_interval (iv);
	   (!NULL_INTERVAL_P (next_iv)
	    && (NILP (limit)
		|| XFASTINT (limit) > next_iv->position));
	   next_iv = next_interval (next_iv))
	{
	  for (p = it_props; p->handler; ++p)
	    {
	      Lisp_Object new_value;

	      new_value = textget (next_iv->plist, *p->name);
	      if (!EQ (values_here[p->idx], new_value))
		break;
	    }

	  if (p->handler)
	    break;
	}

      if (!NULL_INTERVAL_P (next_iv))
	{
	  if (INTEGERP (limit)
	      && next_iv->position >= XFASTINT (limit))
	    /* No text property change up to limit.  */
	    it->stop_charpos = min (XFASTINT (limit), it->stop_charpos);
	  else
	    /* Text properties change in next_iv.  */
	    it->stop_charpos = min (it->stop_charpos, next_iv->position);
	}
    }

  if (it->cmp_it.id < 0)
    {
      EMACS_INT stoppos = it->end_charpos;

      if (it->bidi_p && it->bidi_it.scan_dir < 0)
	stoppos = -1;
      composition_compute_stop_pos (&it->cmp_it, charpos, bytepos,
				    stoppos, it->string);
    }

  xassert (STRINGP (it->string)
	   || (it->stop_charpos >= BEGV
	       && it->stop_charpos >= IT_CHARPOS (*it)));
}


/* Return the position of the next overlay change after POS in
   current_buffer.  Value is point-max if no overlay change
   follows.  This is like `next-overlay-change' but doesn't use
   xmalloc.  */

static EMACS_INT
next_overlay_change (EMACS_INT pos)
{
  ptrdiff_t i, noverlays;
  EMACS_INT endpos;
  Lisp_Object *overlays;

  /* Get all overlays at the given position.  */
  GET_OVERLAYS_AT (pos, overlays, noverlays, &endpos, 1);

  /* If any of these overlays ends before endpos,
     use its ending point instead.  */
  for (i = 0; i < noverlays; ++i)
    {
      Lisp_Object oend;
      EMACS_INT oendpos;

      oend = OVERLAY_END (overlays[i]);
      oendpos = OVERLAY_POSITION (oend);
      endpos = min (endpos, oendpos);
    }

  return endpos;
}

/* How many characters forward to search for a display property or
   display string.  Searching too far forward makes the bidi display
   sluggish, especially in small windows.  */
#define MAX_DISP_SCAN 250

/* Return the character position of a display string at or after
   position specified by POSITION.  If no display string exists at or
   after POSITION, return ZV.  A display string is either an overlay
   with `display' property whose value is a string, or a `display'
   text property whose value is a string.  STRING is data about the
   string to iterate; if STRING->lstring is nil, we are iterating a
   buffer.  FRAME_WINDOW_P is non-zero when we are displaying a window
   on a GUI frame.  DISP_PROP is set to zero if we searched
   MAX_DISP_SCAN characters forward without finding any display
   strings, non-zero otherwise.  It is set to 2 if the display string
   uses any kind of `(space ...)' spec that will produce a stretch of
   white space in the text area.  */
EMACS_INT
compute_display_string_pos (struct text_pos *position,
			    struct bidi_string_data *string,
			    int frame_window_p, int *disp_prop)
{
  /* OBJECT = nil means current buffer.  */
  Lisp_Object object =
    (string && STRINGP (string->lstring)) ? string->lstring : Qnil;
  Lisp_Object pos, spec, limpos;
  int string_p = (string && (STRINGP (string->lstring) || string->s));
  EMACS_INT eob = string_p ? string->schars : ZV;
  EMACS_INT begb = string_p ? 0 : BEGV;
  EMACS_INT bufpos, charpos = CHARPOS (*position);
  EMACS_INT lim =
    (charpos < eob - MAX_DISP_SCAN) ? charpos + MAX_DISP_SCAN : eob;
  struct text_pos tpos;
  int rv = 0;

  *disp_prop = 1;

  if (charpos >= eob
      /* We don't support display properties whose values are strings
	 that have display string properties.  */
      || string->from_disp_str
      /* C strings cannot have display properties.  */
      || (string->s && !STRINGP (object)))
    {
      *disp_prop = 0;
      return eob;
    }

  /* If the character at CHARPOS is where the display string begins,
     return CHARPOS.  */
  pos = make_number (charpos);
  if (STRINGP (object))
    bufpos = string->bufpos;
  else
    bufpos = charpos;
  tpos = *position;
  if (!NILP (spec = Fget_char_property (pos, Qdisplay, object))
      && (charpos <= begb
	  || !EQ (Fget_char_property (make_number (charpos - 1), Qdisplay,
				      object),
		  spec))
      && (rv = handle_display_spec (NULL, spec, object, Qnil, &tpos, bufpos,
				    frame_window_p)))
    {
      if (rv == 2)
	*disp_prop = 2;
      return charpos;
    }

  /* Look forward for the first character with a `display' property
     that will replace the underlying text when displayed.  */
  limpos = make_number (lim);
  do {
    pos = Fnext_single_char_property_change (pos, Qdisplay, object, limpos);
    CHARPOS (tpos) = XFASTINT (pos);
    if (CHARPOS (tpos) >= lim)
      {
	*disp_prop = 0;
	break;
      }
    if (STRINGP (object))
      BYTEPOS (tpos) = string_char_to_byte (object, CHARPOS (tpos));
    else
      BYTEPOS (tpos) = CHAR_TO_BYTE (CHARPOS (tpos));
    spec = Fget_char_property (pos, Qdisplay, object);
    if (!STRINGP (object))
      bufpos = CHARPOS (tpos);
  } while (NILP (spec)
	   || !(rv = handle_display_spec (NULL, spec, object, Qnil, &tpos,
					  bufpos, frame_window_p)));
  if (rv == 2)
    *disp_prop = 2;

  return CHARPOS (tpos);
}

/* Return the character position of the end of the display string that
   started at CHARPOS.  If there's no display string at CHARPOS,
   return -1.  A display string is either an overlay with `display'
   property whose value is a string or a `display' text property whose
   value is a string.  */
EMACS_INT
compute_display_string_end (EMACS_INT charpos, struct bidi_string_data *string)
{
  /* OBJECT = nil means current buffer.  */
  Lisp_Object object =
    (string && STRINGP (string->lstring)) ? string->lstring : Qnil;
  Lisp_Object pos = make_number (charpos);
  EMACS_INT eob =
    (STRINGP (object) || (string && string->s)) ? string->schars : ZV;

  if (charpos >= eob || (string->s && !STRINGP (object)))
    return eob;

  /* It could happen that the display property or overlay was removed
     since we found it in compute_display_string_pos above.  One way
     this can happen is if JIT font-lock was called (through
     handle_fontified_prop), and jit-lock-functions remove text
     properties or overlays from the portion of buffer that includes
     CHARPOS.  Muse mode is known to do that, for example.  In this
     case, we return -1 to the caller, to signal that no display
     string is actually present at CHARPOS.  See bidi_fetch_char for
     how this is handled.

     An alternative would be to never look for display properties past
     it->stop_charpos.  But neither compute_display_string_pos nor
     bidi_fetch_char that calls it know or care where the next
     stop_charpos is.  */
  if (NILP (Fget_char_property (pos, Qdisplay, object)))
    return -1;

  /* Look forward for the first character where the `display' property
     changes.  */
  pos = Fnext_single_char_property_change (pos, Qdisplay, object, Qnil);

  return XFASTINT (pos);
}



/***********************************************************************
			    Fontification
 ***********************************************************************/

/* Handle changes in the `fontified' property of the current buffer by
   calling hook functions from Qfontification_functions to fontify
   regions of text.  */

static enum prop_handled
handle_fontified_prop (struct it *it)
{
  Lisp_Object prop, pos;
  enum prop_handled handled = HANDLED_NORMALLY;

  if (!NILP (Vmemory_full))
    return handled;

  /* Get the value of the `fontified' property at IT's current buffer
     position.  (The `fontified' property doesn't have a special
     meaning in strings.)  If the value is nil, call functions from
     Qfontification_functions.  */
  if (!STRINGP (it->string)
      && it->s == NULL
      && !NILP (Vfontification_functions)
      && !NILP (Vrun_hooks)
      && (pos = make_number (IT_CHARPOS (*it)),
	  prop = Fget_char_property (pos, Qfontified, Qnil),
	  /* Ignore the special cased nil value always present at EOB since
	     no amount of fontifying will be able to change it.  */
	  NILP (prop) && IT_CHARPOS (*it) < Z))
    {
      int count = SPECPDL_INDEX ();
      Lisp_Object val;
      struct buffer *obuf = current_buffer;
      int begv = BEGV, zv = ZV;
      int old_clip_changed = current_buffer->clip_changed;

      val = Vfontification_functions;
      specbind (Qfontification_functions, Qnil);

      xassert (it->end_charpos == ZV);

      if (!CONSP (val) || EQ (XCAR (val), Qlambda))
	safe_call1 (val, pos);
      else
	{
	  Lisp_Object fns, fn;
	  struct gcpro gcpro1, gcpro2;

	  fns = Qnil;
	  GCPRO2 (val, fns);

	  for (; CONSP (val); val = XCDR (val))
	    {
	      fn = XCAR (val);

	      if (EQ (fn, Qt))
		{
		  /* A value of t indicates this hook has a local
		     binding; it means to run the global binding too.
		     In a global value, t should not occur.  If it
		     does, we must ignore it to avoid an endless
		     loop.  */
		  for (fns = Fdefault_value (Qfontification_functions);
		       CONSP (fns);
		       fns = XCDR (fns))
		    {
		      fn = XCAR (fns);
		      if (!EQ (fn, Qt))
			safe_call1 (fn, pos);
		    }
		}
	      else
		safe_call1 (fn, pos);
	    }

	  UNGCPRO;
	}

      unbind_to (count, Qnil);

      /* Fontification functions routinely call `save-restriction'.
	 Normally, this tags clip_changed, which can confuse redisplay
	 (see discussion in Bug#6671).  Since we don't perform any
	 special handling of fontification changes in the case where
	 `save-restriction' isn't called, there's no point doing so in
	 this case either.  So, if the buffer's restrictions are
	 actually left unchanged, reset clip_changed.  */
      if (obuf == current_buffer)
      	{
      	  if (begv == BEGV && zv == ZV)
	    current_buffer->clip_changed = old_clip_changed;
      	}
      /* There isn't much we can reasonably do to protect against
      	 misbehaving fontification, but here's a fig leaf.  */
      else if (!NILP (BVAR (obuf, name)))
      	set_buffer_internal_1 (obuf);

      /* The fontification code may have added/removed text.
	 It could do even a lot worse, but let's at least protect against
	 the most obvious case where only the text past `pos' gets changed',
	 as is/was done in grep.el where some escapes sequences are turned
	 into face properties (bug#7876).  */
      it->end_charpos = ZV;

      /* Return HANDLED_RECOMPUTE_PROPS only if function fontified
	 something.  This avoids an endless loop if they failed to
	 fontify the text for which reason ever.  */
      if (!NILP (Fget_char_property (pos, Qfontified, Qnil)))
	handled = HANDLED_RECOMPUTE_PROPS;
    }

  return handled;
}



/***********************************************************************
				Faces
 ***********************************************************************/

/* Set up iterator IT from face properties at its current position.
   Called from handle_stop.  */

static enum prop_handled
handle_face_prop (struct it *it)
{
  int new_face_id;
  EMACS_INT next_stop;

  if (!STRINGP (it->string))
    {
      new_face_id
	= face_at_buffer_position (it->w,
				   IT_CHARPOS (*it),
				   it->region_beg_charpos,
				   it->region_end_charpos,
				   &next_stop,
				   (IT_CHARPOS (*it)
				    + TEXT_PROP_DISTANCE_LIMIT),
				   0, it->base_face_id);

      /* Is this a start of a run of characters with box face?
	 Caveat: this can be called for a freshly initialized
	 iterator; face_id is -1 in this case.  We know that the new
	 face will not change until limit, i.e. if the new face has a
	 box, all characters up to limit will have one.  But, as
	 usual, we don't know whether limit is really the end.  */
      if (new_face_id != it->face_id)
	{
	  struct face *new_face = FACE_FROM_ID (it->f, new_face_id);

	  /* If new face has a box but old face has not, this is
	     the start of a run of characters with box, i.e. it has
	     a shadow on the left side.  The value of face_id of the
	     iterator will be -1 if this is the initial call that gets
	     the face.  In this case, we have to look in front of IT's
	     position and see whether there is a face != new_face_id.  */
	  it->start_of_box_run_p
	    = (new_face->box != FACE_NO_BOX
	       && (it->face_id >= 0
		   || IT_CHARPOS (*it) == BEG
		   || new_face_id != face_before_it_pos (it)));
	  it->face_box_p = new_face->box != FACE_NO_BOX;
	}
    }
  else
    {
      int base_face_id;
      EMACS_INT bufpos;
      int i;
      Lisp_Object from_overlay
	= (it->current.overlay_string_index >= 0
	   ? it->string_overlays[it->current.overlay_string_index]
	   : Qnil);

      /* See if we got to this string directly or indirectly from
	 an overlay property.  That includes the before-string or
	 after-string of an overlay, strings in display properties
	 provided by an overlay, their text properties, etc.

	 FROM_OVERLAY is the overlay that brought us here, or nil if none.  */
      if (! NILP (from_overlay))
	for (i = it->sp - 1; i >= 0; i--)
	  {
	    if (it->stack[i].current.overlay_string_index >= 0)
	      from_overlay
		= it->string_overlays[it->stack[i].current.overlay_string_index];
	    else if (! NILP (it->stack[i].from_overlay))
	      from_overlay = it->stack[i].from_overlay;

	    if (!NILP (from_overlay))
	      break;
	  }

      if (! NILP (from_overlay))
	{
	  bufpos = IT_CHARPOS (*it);
	  /* For a string from an overlay, the base face depends
	     only on text properties and ignores overlays.  */
	  base_face_id
	    = face_for_overlay_string (it->w,
				       IT_CHARPOS (*it),
				       it->region_beg_charpos,
				       it->region_end_charpos,
				       &next_stop,
				       (IT_CHARPOS (*it)
					+ TEXT_PROP_DISTANCE_LIMIT),
				       0,
				       from_overlay);
	}
      else
	{
	  bufpos = 0;

	  /* For strings from a `display' property, use the face at
	     IT's current buffer position as the base face to merge
	     with, so that overlay strings appear in the same face as
	     surrounding text, unless they specify their own
	     faces.  */
	  base_face_id = it->string_from_prefix_prop_p
	    ? DEFAULT_FACE_ID
	    : underlying_face_id (it);
	}

      new_face_id = face_at_string_position (it->w,
					     it->string,
					     IT_STRING_CHARPOS (*it),
					     bufpos,
					     it->region_beg_charpos,
					     it->region_end_charpos,
					     &next_stop,
					     base_face_id, 0);

      /* Is this a start of a run of characters with box?  Caveat:
	 this can be called for a freshly allocated iterator; face_id
	 is -1 is this case.  We know that the new face will not
	 change until the next check pos, i.e. if the new face has a
	 box, all characters up to that position will have a
	 box.  But, as usual, we don't know whether that position
	 is really the end.  */
      if (new_face_id != it->face_id)
	{
	  struct face *new_face = FACE_FROM_ID (it->f, new_face_id);
	  struct face *old_face = FACE_FROM_ID (it->f, it->face_id);

	  /* If new face has a box but old face hasn't, this is the
	     start of a run of characters with box, i.e. it has a
	     shadow on the left side.  */
	  it->start_of_box_run_p
	    = new_face->box && (old_face == NULL || !old_face->box);
	  it->face_box_p = new_face->box != FACE_NO_BOX;
	}
    }

  it->face_id = new_face_id;
  return HANDLED_NORMALLY;
}


/* Return the ID of the face ``underlying'' IT's current position,
   which is in a string.  If the iterator is associated with a
   buffer, return the face at IT's current buffer position.
   Otherwise, use the iterator's base_face_id.  */

static int
underlying_face_id (struct it *it)
{
  int face_id = it->base_face_id, i;

  xassert (STRINGP (it->string));

  for (i = it->sp - 1; i >= 0; --i)
    if (NILP (it->stack[i].string))
      face_id = it->stack[i].face_id;

  return face_id;
}


/* Compute the face one character before or after the current position
   of IT, in the visual order.  BEFORE_P non-zero means get the face
   in front (to the left in L2R paragraphs, to the right in R2L
   paragraphs) of IT's screen position.  Value is the ID of the face.  */

static int
face_before_or_after_it_pos (struct it *it, int before_p)
{
  int face_id, limit;
  EMACS_INT next_check_charpos;
  struct it it_copy;
  void *it_copy_data = NULL;

  xassert (it->s == NULL);

  if (STRINGP (it->string))
    {
      EMACS_INT bufpos, charpos;
      int base_face_id;

      /* No face change past the end of the string (for the case
	 we are padding with spaces).  No face change before the
	 string start.  */
      if (IT_STRING_CHARPOS (*it) >= SCHARS (it->string)
	  || (IT_STRING_CHARPOS (*it) == 0 && before_p))
	return it->face_id;

      if (!it->bidi_p)
	{
	  /* Set charpos to the position before or after IT's current
	     position, in the logical order, which in the non-bidi
	     case is the same as the visual order.  */
	  if (before_p)
	    charpos = IT_STRING_CHARPOS (*it) - 1;
	  else if (it->what == IT_COMPOSITION)
	    /* For composition, we must check the character after the
	       composition.  */
	    charpos = IT_STRING_CHARPOS (*it) + it->cmp_it.nchars;
	  else
	    charpos = IT_STRING_CHARPOS (*it) + 1;
	}
      else
	{
	  if (before_p)
	    {
	      /* With bidi iteration, the character before the current
		 in the visual order cannot be found by simple
		 iteration, because "reverse" reordering is not
		 supported.  Instead, we need to use the move_it_*
		 family of functions.  */
	      /* Ignore face changes before the first visible
		 character on this display line.  */
	      if (it->current_x <= it->first_visible_x)
		return it->face_id;
	      SAVE_IT (it_copy, *it, it_copy_data);
	      /* Implementation note: Since move_it_in_display_line
		 works in the iterator geometry, and thinks the first
		 character is always the leftmost, even in R2L lines,
		 we don't need to distinguish between the R2L and L2R
		 cases here.  */
	      move_it_in_display_line (&it_copy, SCHARS (it_copy.string),
				       it_copy.current_x - 1, MOVE_TO_X);
	      charpos = IT_STRING_CHARPOS (it_copy);
	      RESTORE_IT (it, it, it_copy_data);
	    }
	  else
	    {
	      /* Set charpos to the string position of the character
		 that comes after IT's current position in the visual
		 order.  */
	      int n = (it->what == IT_COMPOSITION ? it->cmp_it.nchars : 1);

	      it_copy = *it;
	      while (n--)
		bidi_move_to_visually_next (&it_copy.bidi_it);

	      charpos = it_copy.bidi_it.charpos;
	    }
	}
      xassert (0 <= charpos && charpos <= SCHARS (it->string));

      if (it->current.overlay_string_index >= 0)
	bufpos = IT_CHARPOS (*it);
      else
	bufpos = 0;

      base_face_id = underlying_face_id (it);

      /* Get the face for ASCII, or unibyte.  */
      face_id = face_at_string_position (it->w,
					 it->string,
					 charpos,
					 bufpos,
					 it->region_beg_charpos,
					 it->region_end_charpos,
					 &next_check_charpos,
					 base_face_id, 0);

      /* Correct the face for charsets different from ASCII.  Do it
	 for the multibyte case only.  The face returned above is
	 suitable for unibyte text if IT->string is unibyte.  */
      if (STRING_MULTIBYTE (it->string))
	{
	  struct text_pos pos1 = string_pos (charpos, it->string);
	  const unsigned char *p = SDATA (it->string) + BYTEPOS (pos1);
	  int c, len;
	  struct face *face = FACE_FROM_ID (it->f, face_id);

	  c = string_char_and_length (p, &len);
	  face_id = FACE_FOR_CHAR (it->f, face, c, charpos, it->string);
	}
    }
  else
    {
      struct text_pos pos;

      if ((IT_CHARPOS (*it) >= ZV && !before_p)
	  || (IT_CHARPOS (*it) <= BEGV && before_p))
	return it->face_id;

      limit = IT_CHARPOS (*it) + TEXT_PROP_DISTANCE_LIMIT;
      pos = it->current.pos;

      if (!it->bidi_p)
	{
	  if (before_p)
	    DEC_TEXT_POS (pos, it->multibyte_p);
	  else
	    {
	      if (it->what == IT_COMPOSITION)
		{
		  /* For composition, we must check the position after
		     the composition.  */
		  pos.charpos += it->cmp_it.nchars;
		  pos.bytepos += it->len;
		}
	      else
		INC_TEXT_POS (pos, it->multibyte_p);
	    }
	}
      else
	{
	  if (before_p)
	    {
	      /* With bidi iteration, the character before the current
		 in the visual order cannot be found by simple
		 iteration, because "reverse" reordering is not
		 supported.  Instead, we need to use the move_it_*
		 family of functions.  */
	      /* Ignore face changes before the first visible
		 character on this display line.  */
	      if (it->current_x <= it->first_visible_x)
		return it->face_id;
	      SAVE_IT (it_copy, *it, it_copy_data);
	      /* Implementation note: Since move_it_in_display_line
		 works in the iterator geometry, and thinks the first
		 character is always the leftmost, even in R2L lines,
		 we don't need to distinguish between the R2L and L2R
		 cases here.  */
	      move_it_in_display_line (&it_copy, ZV,
				       it_copy.current_x - 1, MOVE_TO_X);
	      pos = it_copy.current.pos;
	      RESTORE_IT (it, it, it_copy_data);
	    }
	  else
	    {
	      /* Set charpos to the buffer position of the character
		 that comes after IT's current position in the visual
		 order.  */
	      int n = (it->what == IT_COMPOSITION ? it->cmp_it.nchars : 1);

	      it_copy = *it;
	      while (n--)
		bidi_move_to_visually_next (&it_copy.bidi_it);

	      SET_TEXT_POS (pos,
			    it_copy.bidi_it.charpos, it_copy.bidi_it.bytepos);
	    }
	}
      xassert (BEGV <= CHARPOS (pos) && CHARPOS (pos) <= ZV);

      /* Determine face for CHARSET_ASCII, or unibyte.  */
      face_id = face_at_buffer_position (it->w,
					 CHARPOS (pos),
					 it->region_beg_charpos,
					 it->region_end_charpos,
					 &next_check_charpos,
					 limit, 0, -1);

      /* Correct the face for charsets different from ASCII.  Do it
	 for the multibyte case only.  The face returned above is
	 suitable for unibyte text if current_buffer is unibyte.  */
      if (it->multibyte_p)
	{
	  int c = FETCH_MULTIBYTE_CHAR (BYTEPOS (pos));
	  struct face *face = FACE_FROM_ID (it->f, face_id);
	  face_id = FACE_FOR_CHAR (it->f, face, c, CHARPOS (pos), Qnil);
	}
    }

  return face_id;
}



/***********************************************************************
			    Invisible text
 ***********************************************************************/

/* Set up iterator IT from invisible properties at its current
   position.  Called from handle_stop.  */

static enum prop_handled
handle_invisible_prop (struct it *it)
{
  enum prop_handled handled = HANDLED_NORMALLY;

  if (STRINGP (it->string))
    {
      Lisp_Object prop, end_charpos, limit, charpos;

      /* Get the value of the invisible text property at the
	 current position.  Value will be nil if there is no such
	 property.  */
      charpos = make_number (IT_STRING_CHARPOS (*it));
      prop = Fget_text_property (charpos, Qinvisible, it->string);

      if (!NILP (prop)
	  && IT_STRING_CHARPOS (*it) < it->end_charpos)
	{
	  EMACS_INT endpos;

	  handled = HANDLED_RECOMPUTE_PROPS;

	  /* Get the position at which the next change of the
	     invisible text property can be found in IT->string.
	     Value will be nil if the property value is the same for
	     all the rest of IT->string.  */
	  XSETINT (limit, SCHARS (it->string));
	  end_charpos = Fnext_single_property_change (charpos, Qinvisible,
						      it->string, limit);

	  /* Text at current position is invisible.  The next
	     change in the property is at position end_charpos.
	     Move IT's current position to that position.  */
	  if (INTEGERP (end_charpos)
	      && (endpos = XFASTINT (end_charpos)) < XFASTINT (limit))
	    {
	      struct text_pos old;
	      EMACS_INT oldpos;

	      old = it->current.string_pos;
	      oldpos = CHARPOS (old);
	      if (it->bidi_p)
		{
		  if (it->bidi_it.first_elt
		      && it->bidi_it.charpos < SCHARS (it->string))
		    bidi_paragraph_init (it->paragraph_embedding,
					 &it->bidi_it, 1);
		  /* Bidi-iterate out of the invisible text.  */
		  do
		    {
		      bidi_move_to_visually_next (&it->bidi_it);
		    }
		  while (oldpos <= it->bidi_it.charpos
			 && it->bidi_it.charpos < endpos);

		  IT_STRING_CHARPOS (*it) = it->bidi_it.charpos;
		  IT_STRING_BYTEPOS (*it) = it->bidi_it.bytepos;
		  if (IT_CHARPOS (*it) >= endpos)
		    it->prev_stop = endpos;
		}
	      else
		{
		  IT_STRING_CHARPOS (*it) = XFASTINT (end_charpos);
		  compute_string_pos (&it->current.string_pos, old, it->string);
		}
	    }
	  else
	    {
	      /* The rest of the string is invisible.  If this is an
		 overlay string, proceed with the next overlay string
		 or whatever comes and return a character from there.  */
	      if (it->current.overlay_string_index >= 0)
		{
		  next_overlay_string (it);
		  /* Don't check for overlay strings when we just
		     finished processing them.  */
		  handled = HANDLED_OVERLAY_STRING_CONSUMED;
		}
	      else
		{
		  IT_STRING_CHARPOS (*it) = SCHARS (it->string);
		  IT_STRING_BYTEPOS (*it) = SBYTES (it->string);
		}
	    }
	}
    }
  else
    {
      int invis_p;
      EMACS_INT newpos, next_stop, start_charpos, tem;
      Lisp_Object pos, prop, overlay;

      /* First of all, is there invisible text at this position?  */
      tem = start_charpos = IT_CHARPOS (*it);
      pos = make_number (tem);
      prop = get_char_property_and_overlay (pos, Qinvisible, it->window,
					    &overlay);
      invis_p = TEXT_PROP_MEANS_INVISIBLE (prop);

      /* If we are on invisible text, skip over it.  */
      if (invis_p && start_charpos < it->end_charpos)
	{
	  /* Record whether we have to display an ellipsis for the
	     invisible text.  */
	  int display_ellipsis_p = invis_p == 2;

	  handled = HANDLED_RECOMPUTE_PROPS;

	  /* Loop skipping over invisible text.  The loop is left at
	     ZV or with IT on the first char being visible again.  */
	  do
	    {
	      /* Try to skip some invisible text.  Return value is the
		 position reached which can be equal to where we start
		 if there is nothing invisible there.  This skips both
		 over invisible text properties and overlays with
		 invisible property.  */
	      newpos = skip_invisible (tem, &next_stop, ZV, it->window);

	      /* If we skipped nothing at all we weren't at invisible
		 text in the first place.  If everything to the end of
		 the buffer was skipped, end the loop.  */
	      if (newpos == tem || newpos >= ZV)
		invis_p = 0;
	      else
		{
		  /* We skipped some characters but not necessarily
		     all there are.  Check if we ended up on visible
		     text.  Fget_char_property returns the property of
		     the char before the given position, i.e. if we
		     get invis_p = 0, this means that the char at
		     newpos is visible.  */
		  pos = make_number (newpos);
		  prop = Fget_char_property (pos, Qinvisible, it->window);
		  invis_p = TEXT_PROP_MEANS_INVISIBLE (prop);
		}

	      /* If we ended up on invisible text, proceed to
		 skip starting with next_stop.  */
	      if (invis_p)
		tem = next_stop;

              /* If there are adjacent invisible texts, don't lose the
                 second one's ellipsis. */
              if (invis_p == 2)
                display_ellipsis_p = 1;
	    }
	  while (invis_p);

	  /* The position newpos is now either ZV or on visible text.  */
	  if (it->bidi_p)
	    {
	      EMACS_INT bpos = CHAR_TO_BYTE (newpos);
	      int on_newline =
		bpos == ZV_BYTE || FETCH_BYTE (bpos) == '\n';
	      int after_newline =
		newpos <= BEGV || FETCH_BYTE (bpos - 1) == '\n';

	      /* If the invisible text ends on a newline or on a
		 character after a newline, we can avoid the costly,
		 character by character, bidi iteration to NEWPOS, and
		 instead simply reseat the iterator there.  That's
		 because all bidi reordering information is tossed at
		 the newline.  This is a big win for modes that hide
		 complete lines, like Outline, Org, etc.  */
	      if (on_newline || after_newline)
		{
		  struct text_pos tpos;
		  bidi_dir_t pdir = it->bidi_it.paragraph_dir;

		  SET_TEXT_POS (tpos, newpos, bpos);
		  reseat_1 (it, tpos, 0);
		  /* If we reseat on a newline/ZV, we need to prep the
		     bidi iterator for advancing to the next character
		     after the newline/EOB, keeping the current paragraph
		     direction (so that PRODUCE_GLYPHS does TRT wrt
		     prepending/appending glyphs to a glyph row).  */
		  if (on_newline)
		    {
		      it->bidi_it.first_elt = 0;
		      it->bidi_it.paragraph_dir = pdir;
		      it->bidi_it.ch = (bpos == ZV_BYTE) ? -1 : '\n';
		      it->bidi_it.nchars = 1;
		      it->bidi_it.ch_len = 1;
		    }
		}
	      else	/* Must use the slow method.  */
		{
		  /* With bidi iteration, the region of invisible text
		     could start and/or end in the middle of a
		     non-base embedding level.  Therefore, we need to
		     skip invisible text using the bidi iterator,
		     starting at IT's current position, until we find
		     ourselves outside of the invisible text.
		     Skipping invisible text _after_ bidi iteration
		     avoids affecting the visual order of the
		     displayed text when invisible properties are
		     added or removed.  */
		  if (it->bidi_it.first_elt && it->bidi_it.charpos < ZV)
		    {
		      /* If we were `reseat'ed to a new paragraph,
			 determine the paragraph base direction.  We
			 need to do it now because
			 next_element_from_buffer may not have a
			 chance to do it, if we are going to skip any
			 text at the beginning, which resets the
			 FIRST_ELT flag.  */
		      bidi_paragraph_init (it->paragraph_embedding,
					   &it->bidi_it, 1);
		    }
		  do
		    {
		      bidi_move_to_visually_next (&it->bidi_it);
		    }
		  while (it->stop_charpos <= it->bidi_it.charpos
			 && it->bidi_it.charpos < newpos);
		  IT_CHARPOS (*it) = it->bidi_it.charpos;
		  IT_BYTEPOS (*it) = it->bidi_it.bytepos;
		  /* If we overstepped NEWPOS, record its position in
		     the iterator, so that we skip invisible text if
		     later the bidi iteration lands us in the
		     invisible region again. */
		  if (IT_CHARPOS (*it) >= newpos)
		    it->prev_stop = newpos;
		}
	    }
	  else
	    {
	      IT_CHARPOS (*it) = newpos;
	      IT_BYTEPOS (*it) = CHAR_TO_BYTE (newpos);
	    }

	  /* If there are before-strings at the start of invisible
	     text, and the text is invisible because of a text
	     property, arrange to show before-strings because 20.x did
	     it that way.  (If the text is invisible because of an
	     overlay property instead of a text property, this is
	     already handled in the overlay code.)  */
	  if (NILP (overlay)
	      && get_overlay_strings (it, it->stop_charpos))
	    {
	      handled = HANDLED_RECOMPUTE_PROPS;
	      it->stack[it->sp - 1].display_ellipsis_p = display_ellipsis_p;
	    }
	  else if (display_ellipsis_p)
            {
              /* Make sure that the glyphs of the ellipsis will get
                 correct `charpos' values.  If we would not update
                 it->position here, the glyphs would belong to the
                 last visible character _before_ the invisible
                 text, which confuses `set_cursor_from_row'.

                 We use the last invisible position instead of the
                 first because this way the cursor is always drawn on
                 the first "." of the ellipsis, whenever PT is inside
                 the invisible text.  Otherwise the cursor would be
                 placed _after_ the ellipsis when the point is after the
                 first invisible character.  */
	      if (!STRINGP (it->object))
		{
		  it->position.charpos = newpos - 1;
		  it->position.bytepos = CHAR_TO_BYTE (it->position.charpos);
		}
	      it->ellipsis_p = 1;
	      /* Let the ellipsis display before
		 considering any properties of the following char.
		 Fixes jasonr@gnu.org 01 Oct 07 bug.  */
	      handled = HANDLED_RETURN;
            }
	}
    }

  return handled;
}


/* Make iterator IT return `...' next.
   Replaces LEN characters from buffer.  */

static void
setup_for_ellipsis (struct it *it, int len)
{
  /* Use the display table definition for `...'.  Invalid glyphs
     will be handled by the method returning elements from dpvec.  */
  if (it->dp && VECTORP (DISP_INVIS_VECTOR (it->dp)))
    {
      struct Lisp_Vector *v = XVECTOR (DISP_INVIS_VECTOR (it->dp));
      it->dpvec = v->contents;
      it->dpend = v->contents + v->header.size;
    }
  else
    {
      /* Default `...'.  */
      it->dpvec = default_invis_vector;
      it->dpend = default_invis_vector + 3;
    }

  it->dpvec_char_len = len;
  it->current.dpvec_index = 0;
  it->dpvec_face_id = -1;

  /* Remember the current face id in case glyphs specify faces.
     IT's face is restored in set_iterator_to_next.
     saved_face_id was set to preceding char's face in handle_stop.  */
  if (it->saved_face_id < 0 || it->saved_face_id != it->face_id)
    it->saved_face_id = it->face_id = DEFAULT_FACE_ID;

  it->method = GET_FROM_DISPLAY_VECTOR;
  it->ellipsis_p = 1;
}



/***********************************************************************
			    'display' property
 ***********************************************************************/

/* Set up iterator IT from `display' property at its current position.
   Called from handle_stop.
   We return HANDLED_RETURN if some part of the display property
   overrides the display of the buffer text itself.
   Otherwise we return HANDLED_NORMALLY.  */

static enum prop_handled
handle_display_prop (struct it *it)
{
  Lisp_Object propval, object, overlay;
  struct text_pos *position;
  EMACS_INT bufpos;
  /* Nonzero if some property replaces the display of the text itself.  */
  int display_replaced_p = 0;

  if (STRINGP (it->string))
    {
      object = it->string;
      position = &it->current.string_pos;
      bufpos = CHARPOS (it->current.pos);
    }
  else
    {
      XSETWINDOW (object, it->w);
      position = &it->current.pos;
      bufpos = CHARPOS (*position);
    }

  /* Reset those iterator values set from display property values.  */
  it->slice.x = it->slice.y = it->slice.width = it->slice.height = Qnil;
  it->space_width = Qnil;
  it->font_height = Qnil;
  it->voffset = 0;

  /* We don't support recursive `display' properties, i.e. string
     values that have a string `display' property, that have a string
     `display' property etc.  */
  if (!it->string_from_display_prop_p)
    it->area = TEXT_AREA;

  propval = get_char_property_and_overlay (make_number (position->charpos),
					   Qdisplay, object, &overlay);
  if (NILP (propval))
    return HANDLED_NORMALLY;
  /* Now OVERLAY is the overlay that gave us this property, or nil
     if it was a text property.  */

  if (!STRINGP (it->string))
    object = it->w->buffer;

  display_replaced_p = handle_display_spec (it, propval, object, overlay,
					    position, bufpos,
					    FRAME_WINDOW_P (it->f));

  return display_replaced_p ? HANDLED_RETURN : HANDLED_NORMALLY;
}

/* Subroutine of handle_display_prop.  Returns non-zero if the display
   specification in SPEC is a replacing specification, i.e. it would
   replace the text covered by `display' property with something else,
   such as an image or a display string.  If SPEC includes any kind or
   `(space ...) specification, the value is 2; this is used by
   compute_display_string_pos, which see.

   See handle_single_display_spec for documentation of arguments.
   frame_window_p is non-zero if the window being redisplayed is on a
   GUI frame; this argument is used only if IT is NULL, see below.

   IT can be NULL, if this is called by the bidi reordering code
   through compute_display_string_pos, which see.  In that case, this
   function only examines SPEC, but does not otherwise "handle" it, in
   the sense that it doesn't set up members of IT from the display
   spec.  */
static int
handle_display_spec (struct it *it, Lisp_Object spec, Lisp_Object object,
		     Lisp_Object overlay, struct text_pos *position,
		     EMACS_INT bufpos, int frame_window_p)
{
  int replacing_p = 0;
  int rv;

  if (CONSP (spec)
      /* Simple specifications.  */
      && !EQ (XCAR (spec), Qimage)
      && !EQ (XCAR (spec), Qspace)
      && !EQ (XCAR (spec), Qwhen)
      && !EQ (XCAR (spec), Qslice)
      && !EQ (XCAR (spec), Qspace_width)
      && !EQ (XCAR (spec), Qheight)
      && !EQ (XCAR (spec), Qraise)
      /* Marginal area specifications.  */
      && !(CONSP (XCAR (spec)) && EQ (XCAR (XCAR (spec)), Qmargin))
      && !EQ (XCAR (spec), Qleft_fringe)
      && !EQ (XCAR (spec), Qright_fringe)
      && !NILP (XCAR (spec)))
    {
      for (; CONSP (spec); spec = XCDR (spec))
	{
	  if ((rv = handle_single_display_spec (it, XCAR (spec), object,
						overlay, position, bufpos,
						replacing_p, frame_window_p)))
	    {
	      replacing_p = rv;
	      /* If some text in a string is replaced, `position' no
		 longer points to the position of `object'.  */
	      if (!it || STRINGP (object))
		break;
	    }
	}
    }
  else if (VECTORP (spec))
    {
      int i;
      for (i = 0; i < ASIZE (spec); ++i)
	if ((rv = handle_single_display_spec (it, AREF (spec, i), object,
					      overlay, position, bufpos,
					      replacing_p, frame_window_p)))
	  {
	    replacing_p = rv;
	    /* If some text in a string is replaced, `position' no
	       longer points to the position of `object'.  */
	    if (!it || STRINGP (object))
	      break;
	  }
    }
  else
    {
      if ((rv = handle_single_display_spec (it, spec, object, overlay,
					    position, bufpos, 0,
					    frame_window_p)))
	replacing_p = rv;
    }

  return replacing_p;
}

/* Value is the position of the end of the `display' property starting
   at START_POS in OBJECT.  */

static struct text_pos
display_prop_end (struct it *it, Lisp_Object object, struct text_pos start_pos)
{
  Lisp_Object end;
  struct text_pos end_pos;

  end = Fnext_single_char_property_change (make_number (CHARPOS (start_pos)),
					   Qdisplay, object, Qnil);
  CHARPOS (end_pos) = XFASTINT (end);
  if (STRINGP (object))
    compute_string_pos (&end_pos, start_pos, it->string);
  else
    BYTEPOS (end_pos) = CHAR_TO_BYTE (XFASTINT (end));

  return end_pos;
}


/* Set up IT from a single `display' property specification SPEC.  OBJECT
   is the object in which the `display' property was found.  *POSITION
   is the position in OBJECT at which the `display' property was found.
   BUFPOS is the buffer position of OBJECT (different from POSITION if
   OBJECT is not a buffer).  DISPLAY_REPLACED_P non-zero means that we
   previously saw a display specification which already replaced text
   display with something else, for example an image; we ignore such
   properties after the first one has been processed.

   OVERLAY is the overlay this `display' property came from,
   or nil if it was a text property.

   If SPEC is a `space' or `image' specification, and in some other
   cases too, set *POSITION to the position where the `display'
   property ends.

   If IT is NULL, only examine the property specification in SPEC, but
   don't set up IT.  In that case, FRAME_WINDOW_P non-zero means SPEC
   is intended to be displayed in a window on a GUI frame.

   Value is non-zero if something was found which replaces the display
   of buffer or string text.  */

static int
handle_single_display_spec (struct it *it, Lisp_Object spec, Lisp_Object object,
			    Lisp_Object overlay, struct text_pos *position,
			    EMACS_INT bufpos, int display_replaced_p,
			    int frame_window_p)
{
  Lisp_Object form;
  Lisp_Object location, value;
  struct text_pos start_pos = *position;
  int valid_p;

  /* If SPEC is a list of the form `(when FORM . VALUE)', evaluate FORM.
     If the result is non-nil, use VALUE instead of SPEC.  */
  form = Qt;
  if (CONSP (spec) && EQ (XCAR (spec), Qwhen))
    {
      spec = XCDR (spec);
      if (!CONSP (spec))
	return 0;
      form = XCAR (spec);
      spec = XCDR (spec);
    }

  if (!NILP (form) && !EQ (form, Qt))
    {
      int count = SPECPDL_INDEX ();
      struct gcpro gcpro1;

      /* Bind `object' to the object having the `display' property, a
	 buffer or string.  Bind `position' to the position in the
	 object where the property was found, and `buffer-position'
	 to the current position in the buffer.  */

      if (NILP (object))
	XSETBUFFER (object, current_buffer);
      specbind (Qobject, object);
      specbind (Qposition, make_number (CHARPOS (*position)));
      specbind (Qbuffer_position, make_number (bufpos));
      GCPRO1 (form);
      form = safe_eval (form);
      UNGCPRO;
      unbind_to (count, Qnil);
    }

  if (NILP (form))
    return 0;

  /* Handle `(height HEIGHT)' specifications.  */
  if (CONSP (spec)
      && EQ (XCAR (spec), Qheight)
      && CONSP (XCDR (spec)))
    {
      if (it)
	{
	  if (!FRAME_WINDOW_P (it->f))
	    return 0;

	  it->font_height = XCAR (XCDR (spec));
	  if (!NILP (it->font_height))
	    {
	      struct face *face = FACE_FROM_ID (it->f, it->face_id);
	      int new_height = -1;

	      if (CONSP (it->font_height)
		  && (EQ (XCAR (it->font_height), Qplus)
		      || EQ (XCAR (it->font_height), Qminus))
		  && CONSP (XCDR (it->font_height))
		  && INTEGERP (XCAR (XCDR (it->font_height))))
		{
		  /* `(+ N)' or `(- N)' where N is an integer.  */
		  int steps = XINT (XCAR (XCDR (it->font_height)));
		  if (EQ (XCAR (it->font_height), Qplus))
		    steps = - steps;
		  it->face_id = smaller_face (it->f, it->face_id, steps);
		}
	      else if (FUNCTIONP (it->font_height))
		{
		  /* Call function with current height as argument.
		     Value is the new height.  */
		  Lisp_Object height;
		  height = safe_call1 (it->font_height,
				       face->lface[LFACE_HEIGHT_INDEX]);
		  if (NUMBERP (height))
		    new_height = XFLOATINT (height);
		}
	      else if (NUMBERP (it->font_height))
		{
		  /* Value is a multiple of the canonical char height.  */
		  struct face *f;

		  f = FACE_FROM_ID (it->f,
				    lookup_basic_face (it->f, DEFAULT_FACE_ID));
		  new_height = (XFLOATINT (it->font_height)
				* XINT (f->lface[LFACE_HEIGHT_INDEX]));
		}
	      else
		{
		  /* Evaluate IT->font_height with `height' bound to the
		     current specified height to get the new height.  */
		  int count = SPECPDL_INDEX ();

		  specbind (Qheight, face->lface[LFACE_HEIGHT_INDEX]);
		  value = safe_eval (it->font_height);
		  unbind_to (count, Qnil);

		  if (NUMBERP (value))
		    new_height = XFLOATINT (value);
		}

	      if (new_height > 0)
		it->face_id = face_with_height (it->f, it->face_id, new_height);
	    }
	}

      return 0;
    }

  /* Handle `(space-width WIDTH)'.  */
  if (CONSP (spec)
      && EQ (XCAR (spec), Qspace_width)
      && CONSP (XCDR (spec)))
    {
      if (it)
	{
	  if (!FRAME_WINDOW_P (it->f))
	    return 0;

	  value = XCAR (XCDR (spec));
	  if (NUMBERP (value) && XFLOATINT (value) > 0)
	    it->space_width = value;
	}

      return 0;
    }

  /* Handle `(slice X Y WIDTH HEIGHT)'.  */
  if (CONSP (spec)
      && EQ (XCAR (spec), Qslice))
    {
      Lisp_Object tem;

      if (it)
	{
	  if (!FRAME_WINDOW_P (it->f))
	    return 0;

	  if (tem = XCDR (spec), CONSP (tem))
	    {
	      it->slice.x = XCAR (tem);
	      if (tem = XCDR (tem), CONSP (tem))
		{
		  it->slice.y = XCAR (tem);
		  if (tem = XCDR (tem), CONSP (tem))
		    {
		      it->slice.width = XCAR (tem);
		      if (tem = XCDR (tem), CONSP (tem))
			it->slice.height = XCAR (tem);
		    }
		}
	    }
	}

      return 0;
    }

  /* Handle `(raise FACTOR)'.  */
  if (CONSP (spec)
      && EQ (XCAR (spec), Qraise)
      && CONSP (XCDR (spec)))
    {
      if (it)
	{
	  if (!FRAME_WINDOW_P (it->f))
	    return 0;

#ifdef HAVE_WINDOW_SYSTEM
	  value = XCAR (XCDR (spec));
	  if (NUMBERP (value))
	    {
	      struct face *face = FACE_FROM_ID (it->f, it->face_id);
	      it->voffset = - (XFLOATINT (value)
			       * (FONT_HEIGHT (face->font)));
	    }
#endif /* HAVE_WINDOW_SYSTEM */
	}

      return 0;
    }

  /* Don't handle the other kinds of display specifications
     inside a string that we got from a `display' property.  */
  if (it && it->string_from_display_prop_p)
    return 0;

  /* Characters having this form of property are not displayed, so
     we have to find the end of the property.  */
  if (it)
    {
      start_pos = *position;
      *position = display_prop_end (it, object, start_pos);
    }
  value = Qnil;

  /* Stop the scan at that end position--we assume that all
     text properties change there.  */
  if (it)
    it->stop_charpos = position->charpos;

  /* Handle `(left-fringe BITMAP [FACE])'
     and `(right-fringe BITMAP [FACE])'.  */
  if (CONSP (spec)
      && (EQ (XCAR (spec), Qleft_fringe)
	  || EQ (XCAR (spec), Qright_fringe))
      && CONSP (XCDR (spec)))
    {
      int fringe_bitmap;

      if (it)
	{
	  if (!FRAME_WINDOW_P (it->f))
	    /* If we return here, POSITION has been advanced
	       across the text with this property.  */
	    {
	      /* Synchronize the bidi iterator with POSITION.  This is
		 needed because we are not going to push the iterator
		 on behalf of this display property, so there will be
		 no pop_it call to do this synchronization for us.  */
	      if (it->bidi_p)
		{
		  it->position = *position;
		  iterate_out_of_display_property (it);
		  *position = it->position;
		}
	      return 1;
	    }
	}
      else if (!frame_window_p)
	return 1;

#ifdef HAVE_WINDOW_SYSTEM
      value = XCAR (XCDR (spec));
      if (!SYMBOLP (value)
	  || !(fringe_bitmap = lookup_fringe_bitmap (value)))
	/* If we return here, POSITION has been advanced
	   across the text with this property.  */
	{
	  if (it && it->bidi_p)
	    {
	      it->position = *position;
	      iterate_out_of_display_property (it);
	      *position = it->position;
	    }
	  return 1;
	}

      if (it)
	{
	  int face_id = lookup_basic_face (it->f, DEFAULT_FACE_ID);;

	  if (CONSP (XCDR (XCDR (spec))))
	    {
	      Lisp_Object face_name = XCAR (XCDR (XCDR (spec)));
	      int face_id2 = lookup_derived_face (it->f, face_name,
						  FRINGE_FACE_ID, 0);
	      if (face_id2 >= 0)
		face_id = face_id2;
	    }

	  /* Save current settings of IT so that we can restore them
	     when we are finished with the glyph property value.  */
	  push_it (it, position);

	  it->area = TEXT_AREA;
	  it->what = IT_IMAGE;
	  it->image_id = -1; /* no image */
	  it->position = start_pos;
	  it->object = NILP (object) ? it->w->buffer : object;
	  it->method = GET_FROM_IMAGE;
	  it->from_overlay = Qnil;
	  it->face_id = face_id;
	  it->from_disp_prop_p = 1;

	  /* Say that we haven't consumed the characters with
	     `display' property yet.  The call to pop_it in
	     set_iterator_to_next will clean this up.  */
	  *position = start_pos;

	  if (EQ (XCAR (spec), Qleft_fringe))
	    {
	      it->left_user_fringe_bitmap = fringe_bitmap;
	      it->left_user_fringe_face_id = face_id;
	    }
	  else
	    {
	      it->right_user_fringe_bitmap = fringe_bitmap;
	      it->right_user_fringe_face_id = face_id;
	    }
	}
#endif /* HAVE_WINDOW_SYSTEM */
      return 1;
    }

  /* Prepare to handle `((margin left-margin) ...)',
     `((margin right-margin) ...)' and `((margin nil) ...)'
     prefixes for display specifications.  */
  location = Qunbound;
  if (CONSP (spec) && CONSP (XCAR (spec)))
    {
      Lisp_Object tem;

      value = XCDR (spec);
      if (CONSP (value))
	value = XCAR (value);

      tem = XCAR (spec);
      if (EQ (XCAR (tem), Qmargin)
	  && (tem = XCDR (tem),
	      tem = CONSP (tem) ? XCAR (tem) : Qnil,
	      (NILP (tem)
	       || EQ (tem, Qleft_margin)
	       || EQ (tem, Qright_margin))))
	location = tem;
    }

  if (EQ (location, Qunbound))
    {
      location = Qnil;
      value = spec;
    }

  /* After this point, VALUE is the property after any
     margin prefix has been stripped.  It must be a string,
     an image specification, or `(space ...)'.

     LOCATION specifies where to display: `left-margin',
     `right-margin' or nil.  */

  valid_p = (STRINGP (value)
#ifdef HAVE_WINDOW_SYSTEM
             || ((it ? FRAME_WINDOW_P (it->f) : frame_window_p)
		 && valid_image_p (value))
#endif /* not HAVE_WINDOW_SYSTEM */
             || (CONSP (value) && EQ (XCAR (value), Qspace)));

  if (valid_p && !display_replaced_p)
    {
      int retval = 1;

      if (!it)
	{
	  /* Callers need to know whether the display spec is any kind
	     of `(space ...)' spec that is about to affect text-area
	     display.  */
	  if (CONSP (value) && EQ (XCAR (value), Qspace) && NILP (location))
	    retval = 2;
	  return retval;
	}

      /* Save current settings of IT so that we can restore them
	 when we are finished with the glyph property value.  */
      push_it (it, position);
      it->from_overlay = overlay;
      it->from_disp_prop_p = 1;

      if (NILP (location))
	it->area = TEXT_AREA;
      else if (EQ (location, Qleft_margin))
	it->area = LEFT_MARGIN_AREA;
      else
	it->area = RIGHT_MARGIN_AREA;

      if (STRINGP (value))
	{
	  it->string = value;
	  it->multibyte_p = STRING_MULTIBYTE (it->string);
	  it->current.overlay_string_index = -1;
	  IT_STRING_CHARPOS (*it) = IT_STRING_BYTEPOS (*it) = 0;
	  it->end_charpos = it->string_nchars = SCHARS (it->string);
	  it->method = GET_FROM_STRING;
	  it->stop_charpos = 0;
	  it->prev_stop = 0;
	  it->base_level_stop = 0;
	  it->string_from_display_prop_p = 1;
	  /* Say that we haven't consumed the characters with
	     `display' property yet.  The call to pop_it in
	     set_iterator_to_next will clean this up.  */
	  if (BUFFERP (object))
	    *position = start_pos;

	  /* Force paragraph direction to be that of the parent
	     object.  If the parent object's paragraph direction is
	     not yet determined, default to L2R.  */
	  if (it->bidi_p && it->bidi_it.paragraph_dir == R2L)
	    it->paragraph_embedding = it->bidi_it.paragraph_dir;
	  else
	    it->paragraph_embedding = L2R;

	  /* Set up the bidi iterator for this display string.  */
	  if (it->bidi_p)
	    {
	      it->bidi_it.string.lstring = it->string;
	      it->bidi_it.string.s = NULL;
	      it->bidi_it.string.schars = it->end_charpos;
	      it->bidi_it.string.bufpos = bufpos;
	      it->bidi_it.string.from_disp_str = 1;
	      it->bidi_it.string.unibyte = !it->multibyte_p;
	      bidi_init_it (0, 0, FRAME_WINDOW_P (it->f), &it->bidi_it);
	    }
	}
      else if (CONSP (value) && EQ (XCAR (value), Qspace))
	{
	  it->method = GET_FROM_STRETCH;
	  it->object = value;
	  *position = it->position = start_pos;
	  retval = 1 + (it->area == TEXT_AREA);
	}
#ifdef HAVE_WINDOW_SYSTEM
      else
	{
	  it->what = IT_IMAGE;
	  it->image_id = lookup_image (it->f, value);
	  it->position = start_pos;
	  it->object = NILP (object) ? it->w->buffer : object;
	  it->method = GET_FROM_IMAGE;

	  /* Say that we haven't consumed the characters with
	     `display' property yet.  The call to pop_it in
	     set_iterator_to_next will clean this up.  */
	  *position = start_pos;
	}
#endif /* HAVE_WINDOW_SYSTEM */

      return retval;
    }

  /* Invalid property or property not supported.  Restore
     POSITION to what it was before.  */
  *position = start_pos;
  return 0;
}

/* Check if PROP is a display property value whose text should be
   treated as intangible.  OVERLAY is the overlay from which PROP
   came, or nil if it came from a text property.  CHARPOS and BYTEPOS
   specify the buffer position covered by PROP.  */

int
display_prop_intangible_p (Lisp_Object prop, Lisp_Object overlay,
			   EMACS_INT charpos, EMACS_INT bytepos)
{
  int frame_window_p = FRAME_WINDOW_P (XFRAME (selected_frame));
  struct text_pos position;

  SET_TEXT_POS (position, charpos, bytepos);
  return handle_display_spec (NULL, prop, Qnil, overlay,
			      &position, charpos, frame_window_p);
}


/* Return 1 if PROP is a display sub-property value containing STRING.

   Implementation note: this and the following function are really
   special cases of handle_display_spec and
   handle_single_display_spec, and should ideally use the same code.
   Until they do, these two pairs must be consistent and must be
   modified in sync.  */

static int
single_display_spec_string_p (Lisp_Object prop, Lisp_Object string)
{
  if (EQ (string, prop))
    return 1;

  /* Skip over `when FORM'.  */
  if (CONSP (prop) && EQ (XCAR (prop), Qwhen))
    {
      prop = XCDR (prop);
      if (!CONSP (prop))
	return 0;
      /* Actually, the condition following `when' should be eval'ed,
	 like handle_single_display_spec does, and we should return
	 zero if it evaluates to nil.  However, this function is
	 called only when the buffer was already displayed and some
	 glyph in the glyph matrix was found to come from a display
	 string.  Therefore, the condition was already evaluated, and
	 the result was non-nil, otherwise the display string wouldn't
	 have been displayed and we would have never been called for
	 this property.  Thus, we can skip the evaluation and assume
	 its result is non-nil.  */
      prop = XCDR (prop);
    }

  if (CONSP (prop))
    /* Skip over `margin LOCATION'.  */
    if (EQ (XCAR (prop), Qmargin))
      {
	prop = XCDR (prop);
	if (!CONSP (prop))
	  return 0;

	prop = XCDR (prop);
	if (!CONSP (prop))
	  return 0;
      }

  return EQ (prop, string) || (CONSP (prop) && EQ (XCAR (prop), string));
}


/* Return 1 if STRING appears in the `display' property PROP.  */

static int
display_prop_string_p (Lisp_Object prop, Lisp_Object string)
{
  if (CONSP (prop)
      && !EQ (XCAR (prop), Qwhen)
      && !(CONSP (XCAR (prop)) && EQ (Qmargin, XCAR (XCAR (prop)))))
    {
      /* A list of sub-properties.  */
      while (CONSP (prop))
	{
	  if (single_display_spec_string_p (XCAR (prop), string))
	    return 1;
	  prop = XCDR (prop);
	}
    }
  else if (VECTORP (prop))
    {
      /* A vector of sub-properties.  */
      int i;
      for (i = 0; i < ASIZE (prop); ++i)
	if (single_display_spec_string_p (AREF (prop, i), string))
	  return 1;
    }
  else
    return single_display_spec_string_p (prop, string);

  return 0;
}

/* Look for STRING in overlays and text properties in the current
   buffer, between character positions FROM and TO (excluding TO).
   BACK_P non-zero means look back (in this case, TO is supposed to be
   less than FROM).
   Value is the first character position where STRING was found, or
   zero if it wasn't found before hitting TO.

   This function may only use code that doesn't eval because it is
   called asynchronously from note_mouse_highlight.  */

static EMACS_INT
string_buffer_position_lim (Lisp_Object string,
			    EMACS_INT from, EMACS_INT to, int back_p)
{
  Lisp_Object limit, prop, pos;
  int found = 0;

  pos = make_number (max (from, BEGV));

  if (!back_p)	/* looking forward */
    {
      limit = make_number (min (to, ZV));
      while (!found && !EQ (pos, limit))
	{
	  prop = Fget_char_property (pos, Qdisplay, Qnil);
	  if (!NILP (prop) && display_prop_string_p (prop, string))
	    found = 1;
	  else
	    pos = Fnext_single_char_property_change (pos, Qdisplay, Qnil,
						     limit);
	}
    }
  else		/* looking back */
    {
      limit = make_number (max (to, BEGV));
      while (!found && !EQ (pos, limit))
	{
	  prop = Fget_char_property (pos, Qdisplay, Qnil);
	  if (!NILP (prop) && display_prop_string_p (prop, string))
	    found = 1;
	  else
	    pos = Fprevious_single_char_property_change (pos, Qdisplay, Qnil,
							 limit);
	}
    }

  return found ? XINT (pos) : 0;
}

/* Determine which buffer position in current buffer STRING comes from.
   AROUND_CHARPOS is an approximate position where it could come from.
   Value is the buffer position or 0 if it couldn't be determined.

   This function is necessary because we don't record buffer positions
   in glyphs generated from strings (to keep struct glyph small).
   This function may only use code that doesn't eval because it is
   called asynchronously from note_mouse_highlight.  */

static EMACS_INT
string_buffer_position (Lisp_Object string, EMACS_INT around_charpos)
{
  const int MAX_DISTANCE = 1000;
  EMACS_INT found = string_buffer_position_lim (string, around_charpos,
						around_charpos + MAX_DISTANCE,
						0);

  if (!found)
    found = string_buffer_position_lim (string, around_charpos,
					around_charpos - MAX_DISTANCE, 1);
  return found;
}



/***********************************************************************
			`composition' property
 ***********************************************************************/

/* Set up iterator IT from `composition' property at its current
   position.  Called from handle_stop.  */

static enum prop_handled
handle_composition_prop (struct it *it)
{
  Lisp_Object prop, string;
  EMACS_INT pos, pos_byte, start, end;

  if (STRINGP (it->string))
    {
      unsigned char *s;

      pos = IT_STRING_CHARPOS (*it);
      pos_byte = IT_STRING_BYTEPOS (*it);
      string = it->string;
      s = SDATA (string) + pos_byte;
      it->c = STRING_CHAR (s);
    }
  else
    {
      pos = IT_CHARPOS (*it);
      pos_byte = IT_BYTEPOS (*it);
      string = Qnil;
      it->c = FETCH_CHAR (pos_byte);
    }

  /* If there's a valid composition and point is not inside of the
     composition (in the case that the composition is from the current
     buffer), draw a glyph composed from the composition components.  */
  if (find_composition (pos, -1, &start, &end, &prop, string)
      && COMPOSITION_VALID_P (start, end, prop)
      && (STRINGP (it->string) || (PT <= start || PT >= end)))
    {
      if (start < pos)
	/* As we can't handle this situation (perhaps font-lock added
	   a new composition), we just return here hoping that next
	   redisplay will detect this composition much earlier.  */
	return HANDLED_NORMALLY;
      if (start != pos)
	{
	  if (STRINGP (it->string))
	    pos_byte = string_char_to_byte (it->string, start);
	  else
	    pos_byte = CHAR_TO_BYTE (start);
	}
      it->cmp_it.id = get_composition_id (start, pos_byte, end - start,
					       prop, string);

      if (it->cmp_it.id >= 0)
	{
	  it->cmp_it.ch = -1;
	  it->cmp_it.nchars = COMPOSITION_LENGTH (prop);
	  it->cmp_it.nglyphs = -1;
	}
    }

  return HANDLED_NORMALLY;
}



/***********************************************************************
			   Overlay strings
 ***********************************************************************/

/* The following structure is used to record overlay strings for
   later sorting in load_overlay_strings.  */

struct overlay_entry
{
  Lisp_Object overlay;
  Lisp_Object string;
  int priority;
  int after_string_p;
};


/* Set up iterator IT from overlay strings at its current position.
   Called from handle_stop.  */

static enum prop_handled
handle_overlay_change (struct it *it)
{
  if (!STRINGP (it->string) && get_overlay_strings (it, 0))
    return HANDLED_RECOMPUTE_PROPS;
  else
    return HANDLED_NORMALLY;
}


/* Set up the next overlay string for delivery by IT, if there is an
   overlay string to deliver.  Called by set_iterator_to_next when the
   end of the current overlay string is reached.  If there are more
   overlay strings to display, IT->string and
   IT->current.overlay_string_index are set appropriately here.
   Otherwise IT->string is set to nil.  */

static void
next_overlay_string (struct it *it)
{
  ++it->current.overlay_string_index;
  if (it->current.overlay_string_index == it->n_overlay_strings)
    {
      /* No more overlay strings.  Restore IT's settings to what
	 they were before overlay strings were processed, and
	 continue to deliver from current_buffer.  */

      it->ellipsis_p = (it->stack[it->sp - 1].display_ellipsis_p != 0);
      pop_it (it);
      xassert (it->sp > 0
	       || (NILP (it->string)
		   && it->method == GET_FROM_BUFFER
		   && it->stop_charpos >= BEGV
		   && it->stop_charpos <= it->end_charpos));
      it->current.overlay_string_index = -1;
      it->n_overlay_strings = 0;
      it->overlay_strings_charpos = -1;
      /* If there's an empty display string on the stack, pop the
	 stack, to resync the bidi iterator with IT's position.  Such
	 empty strings are pushed onto the stack in
	 get_overlay_strings_1.  */
      if (it->sp > 0 && STRINGP (it->string) && !SCHARS (it->string))
	pop_it (it);

      /* If we're at the end of the buffer, record that we have
	 processed the overlay strings there already, so that
	 next_element_from_buffer doesn't try it again.  */
      if (NILP (it->string) && IT_CHARPOS (*it) >= it->end_charpos)
	it->overlay_strings_at_end_processed_p = 1;
    }
  else
    {
      /* There are more overlay strings to process.  If
	 IT->current.overlay_string_index has advanced to a position
	 where we must load IT->overlay_strings with more strings, do
	 it.  We must load at the IT->overlay_strings_charpos where
	 IT->n_overlay_strings was originally computed; when invisible
	 text is present, this might not be IT_CHARPOS (Bug#7016).  */
      int i = it->current.overlay_string_index % OVERLAY_STRING_CHUNK_SIZE;

      if (it->current.overlay_string_index && i == 0)
	load_overlay_strings (it, it->overlay_strings_charpos);

      /* Initialize IT to deliver display elements from the overlay
         string.  */
      it->string = it->overlay_strings[i];
      it->multibyte_p = STRING_MULTIBYTE (it->string);
      SET_TEXT_POS (it->current.string_pos, 0, 0);
      it->method = GET_FROM_STRING;
      it->stop_charpos = 0;
      if (it->cmp_it.stop_pos >= 0)
	it->cmp_it.stop_pos = 0;
      it->prev_stop = 0;
      it->base_level_stop = 0;

      /* Set up the bidi iterator for this overlay string.  */
      if (it->bidi_p)
	{
	  it->bidi_it.string.lstring = it->string;
	  it->bidi_it.string.s = NULL;
	  it->bidi_it.string.schars = SCHARS (it->string);
	  it->bidi_it.string.bufpos = it->overlay_strings_charpos;
	  it->bidi_it.string.from_disp_str = it->string_from_display_prop_p;
	  it->bidi_it.string.unibyte = !it->multibyte_p;
	  bidi_init_it (0, 0, FRAME_WINDOW_P (it->f), &it->bidi_it);
	}
    }

  CHECK_IT (it);
}


/* Compare two overlay_entry structures E1 and E2.  Used as a
   comparison function for qsort in load_overlay_strings.  Overlay
   strings for the same position are sorted so that

   1. All after-strings come in front of before-strings, except
   when they come from the same overlay.

   2. Within after-strings, strings are sorted so that overlay strings
   from overlays with higher priorities come first.

   2. Within before-strings, strings are sorted so that overlay
   strings from overlays with higher priorities come last.

   Value is analogous to strcmp.  */


static int
compare_overlay_entries (const void *e1, const void *e2)
{
  struct overlay_entry *entry1 = (struct overlay_entry *) e1;
  struct overlay_entry *entry2 = (struct overlay_entry *) e2;
  int result;

  if (entry1->after_string_p != entry2->after_string_p)
    {
      /* Let after-strings appear in front of before-strings if
	 they come from different overlays.  */
      if (EQ (entry1->overlay, entry2->overlay))
	result = entry1->after_string_p ? 1 : -1;
      else
	result = entry1->after_string_p ? -1 : 1;
    }
  else if (entry1->after_string_p)
    /* After-strings sorted in order of decreasing priority.  */
    result = entry2->priority - entry1->priority;
  else
    /* Before-strings sorted in order of increasing priority.  */
    result = entry1->priority - entry2->priority;

  return result;
}


/* Load the vector IT->overlay_strings with overlay strings from IT's
   current buffer position, or from CHARPOS if that is > 0.  Set
   IT->n_overlays to the total number of overlay strings found.

   Overlay strings are processed OVERLAY_STRING_CHUNK_SIZE strings at
   a time.  On entry into load_overlay_strings,
   IT->current.overlay_string_index gives the number of overlay
   strings that have already been loaded by previous calls to this
   function.

   IT->add_overlay_start contains an additional overlay start
   position to consider for taking overlay strings from, if non-zero.
   This position comes into play when the overlay has an `invisible'
   property, and both before and after-strings.  When we've skipped to
   the end of the overlay, because of its `invisible' property, we
   nevertheless want its before-string to appear.
   IT->add_overlay_start will contain the overlay start position
   in this case.

   Overlay strings are sorted so that after-string strings come in
   front of before-string strings.  Within before and after-strings,
   strings are sorted by overlay priority.  See also function
   compare_overlay_entries.  */

static void
load_overlay_strings (struct it *it, EMACS_INT charpos)
{
  Lisp_Object overlay, window, str, invisible;
  struct Lisp_Overlay *ov;
  EMACS_INT start, end;
  int size = 20;
  int n = 0, i, j, invis_p;
  struct overlay_entry *entries
    = (struct overlay_entry *) alloca (size * sizeof *entries);

  if (charpos <= 0)
    charpos = IT_CHARPOS (*it);

  /* Append the overlay string STRING of overlay OVERLAY to vector
     `entries' which has size `size' and currently contains `n'
     elements.  AFTER_P non-zero means STRING is an after-string of
     OVERLAY.  */
#define RECORD_OVERLAY_STRING(OVERLAY, STRING, AFTER_P)			\
  do									\
    {									\
      Lisp_Object priority;						\
									\
      if (n == size)							\
	{								\
	  int new_size = 2 * size;					\
	  struct overlay_entry *old = entries;				\
	  entries =							\
            (struct overlay_entry *) alloca (new_size			\
					     * sizeof *entries);	\
	  memcpy (entries, old, size * sizeof *entries);		\
	  size = new_size;						\
	}								\
									\
      entries[n].string = (STRING);					\
      entries[n].overlay = (OVERLAY);					\
      priority = Foverlay_get ((OVERLAY), Qpriority);			\
      entries[n].priority = INTEGERP (priority) ? XINT (priority) : 0;  \
      entries[n].after_string_p = (AFTER_P);				\
      ++n;								\
    }									\
  while (0)

  /* Process overlay before the overlay center.  */
  for (ov = current_buffer->overlays_before; ov; ov = ov->next)
    {
      XSETMISC (overlay, ov);
      xassert (OVERLAYP (overlay));
      start = OVERLAY_POSITION (OVERLAY_START (overlay));
      end = OVERLAY_POSITION (OVERLAY_END (overlay));

      if (end < charpos)
	break;

      /* Skip this overlay if it doesn't start or end at IT's current
	 position.  */
      if (end != charpos && start != charpos)
	continue;

      /* Skip this overlay if it doesn't apply to IT->w.  */
      window = Foverlay_get (overlay, Qwindow);
      if (WINDOWP (window) && XWINDOW (window) != it->w)
	continue;

      /* If the text ``under'' the overlay is invisible, both before-
	 and after-strings from this overlay are visible; start and
	 end position are indistinguishable.  */
      invisible = Foverlay_get (overlay, Qinvisible);
      invis_p = TEXT_PROP_MEANS_INVISIBLE (invisible);

      /* If overlay has a non-empty before-string, record it.  */
      if ((start == charpos || (end == charpos && invis_p))
	  && (str = Foverlay_get (overlay, Qbefore_string), STRINGP (str))
	  && SCHARS (str))
	RECORD_OVERLAY_STRING (overlay, str, 0);

      /* If overlay has a non-empty after-string, record it.  */
      if ((end == charpos || (start == charpos && invis_p))
	  && (str = Foverlay_get (overlay, Qafter_string), STRINGP (str))
	  && SCHARS (str))
	RECORD_OVERLAY_STRING (overlay, str, 1);
    }

  /* Process overlays after the overlay center.  */
  for (ov = current_buffer->overlays_after; ov; ov = ov->next)
    {
      XSETMISC (overlay, ov);
      xassert (OVERLAYP (overlay));
      start = OVERLAY_POSITION (OVERLAY_START (overlay));
      end = OVERLAY_POSITION (OVERLAY_END (overlay));

      if (start > charpos)
	break;

      /* Skip this overlay if it doesn't start or end at IT's current
	 position.  */
      if (end != charpos && start != charpos)
	continue;

      /* Skip this overlay if it doesn't apply to IT->w.  */
      window = Foverlay_get (overlay, Qwindow);
      if (WINDOWP (window) && XWINDOW (window) != it->w)
	continue;

      /* If the text ``under'' the overlay is invisible, it has a zero
	 dimension, and both before- and after-strings apply.  */
      invisible = Foverlay_get (overlay, Qinvisible);
      invis_p = TEXT_PROP_MEANS_INVISIBLE (invisible);

      /* If overlay has a non-empty before-string, record it.  */
      if ((start == charpos || (end == charpos && invis_p))
	  && (str = Foverlay_get (overlay, Qbefore_string), STRINGP (str))
	  && SCHARS (str))
	RECORD_OVERLAY_STRING (overlay, str, 0);

      /* If overlay has a non-empty after-string, record it.  */
      if ((end == charpos || (start == charpos && invis_p))
	  && (str = Foverlay_get (overlay, Qafter_string), STRINGP (str))
	  && SCHARS (str))
	RECORD_OVERLAY_STRING (overlay, str, 1);
    }

#undef RECORD_OVERLAY_STRING

  /* Sort entries.  */
  if (n > 1)
    qsort (entries, n, sizeof *entries, compare_overlay_entries);

  /* Record number of overlay strings, and where we computed it.  */
  it->n_overlay_strings = n;
  it->overlay_strings_charpos = charpos;

  /* IT->current.overlay_string_index is the number of overlay strings
     that have already been consumed by IT.  Copy some of the
     remaining overlay strings to IT->overlay_strings.  */
  i = 0;
  j = it->current.overlay_string_index;
  while (i < OVERLAY_STRING_CHUNK_SIZE && j < n)
    {
      it->overlay_strings[i] = entries[j].string;
      it->string_overlays[i++] = entries[j++].overlay;
    }

  CHECK_IT (it);
}


/* Get the first chunk of overlay strings at IT's current buffer
   position, or at CHARPOS if that is > 0.  Value is non-zero if at
   least one overlay string was found.  */

static int
get_overlay_strings_1 (struct it *it, EMACS_INT charpos, int compute_stop_p)
{
  /* Get the first OVERLAY_STRING_CHUNK_SIZE overlay strings to
     process.  This fills IT->overlay_strings with strings, and sets
     IT->n_overlay_strings to the total number of strings to process.
     IT->pos.overlay_string_index has to be set temporarily to zero
     because load_overlay_strings needs this; it must be set to -1
     when no overlay strings are found because a zero value would
     indicate a position in the first overlay string.  */
  it->current.overlay_string_index = 0;
  load_overlay_strings (it, charpos);

  /* If we found overlay strings, set up IT to deliver display
     elements from the first one.  Otherwise set up IT to deliver
     from current_buffer.  */
  if (it->n_overlay_strings)
    {
      /* Make sure we know settings in current_buffer, so that we can
	 restore meaningful values when we're done with the overlay
	 strings.  */
      if (compute_stop_p)
	compute_stop_pos (it);
      xassert (it->face_id >= 0);

      /* Save IT's settings.  They are restored after all overlay
	 strings have been processed.  */
      xassert (!compute_stop_p || it->sp == 0);

      /* When called from handle_stop, there might be an empty display
         string loaded.  In that case, don't bother saving it.  But
         don't use this optimization with the bidi iterator, since we
         need the corresponding pop_it call to resync the bidi
         iterator's position with IT's position, after we are done
         with the overlay strings.  (The corresponding call to pop_it
         in case of an empty display string is in
         next_overlay_string.)  */
      if (!(!it->bidi_p
	    && STRINGP (it->string) && !SCHARS (it->string)))
	push_it (it, NULL);

      /* Set up IT to deliver display elements from the first overlay
	 string.  */
      IT_STRING_CHARPOS (*it) = IT_STRING_BYTEPOS (*it) = 0;
      it->string = it->overlay_strings[0];
      it->from_overlay = Qnil;
      it->stop_charpos = 0;
      xassert (STRINGP (it->string));
      it->end_charpos = SCHARS (it->string);
      it->prev_stop = 0;
      it->base_level_stop = 0;
      it->multibyte_p = STRING_MULTIBYTE (it->string);
      it->method = GET_FROM_STRING;
      it->from_disp_prop_p = 0;

      /* Force paragraph direction to be that of the parent
	 buffer.  */
      if (it->bidi_p && it->bidi_it.paragraph_dir == R2L)
	it->paragraph_embedding = it->bidi_it.paragraph_dir;
      else
	it->paragraph_embedding = L2R;

      /* Set up the bidi iterator for this overlay string.  */
      if (it->bidi_p)
	{
	  EMACS_INT pos = (charpos > 0 ? charpos : IT_CHARPOS (*it));

	  it->bidi_it.string.lstring = it->string;
	  it->bidi_it.string.s = NULL;
	  it->bidi_it.string.schars = SCHARS (it->string);
	  it->bidi_it.string.bufpos = pos;
	  it->bidi_it.string.from_disp_str = it->string_from_display_prop_p;
	  it->bidi_it.string.unibyte = !it->multibyte_p;
	  bidi_init_it (0, 0, FRAME_WINDOW_P (it->f), &it->bidi_it);
	}
      return 1;
    }

  it->current.overlay_string_index = -1;
  return 0;
}

static int
get_overlay_strings (struct it *it, EMACS_INT charpos)
{
  it->string = Qnil;
  it->method = GET_FROM_BUFFER;

  (void) get_overlay_strings_1 (it, charpos, 1);

  CHECK_IT (it);

  /* Value is non-zero if we found at least one overlay string.  */
  return STRINGP (it->string);
}



/***********************************************************************
		      Saving and restoring state
 ***********************************************************************/

/* Save current settings of IT on IT->stack.  Called, for example,
   before setting up IT for an overlay string, to be able to restore
   IT's settings to what they were after the overlay string has been
   processed.  If POSITION is non-NULL, it is the position to save on
   the stack instead of IT->position.  */

static void
push_it (struct it *it, struct text_pos *position)
{
  struct iterator_stack_entry *p;

  xassert (it->sp < IT_STACK_SIZE);
  p = it->stack + it->sp;

  p->stop_charpos = it->stop_charpos;
  p->prev_stop = it->prev_stop;
  p->base_level_stop = it->base_level_stop;
  p->cmp_it = it->cmp_it;
  xassert (it->face_id >= 0);
  p->face_id = it->face_id;
  p->string = it->string;
  p->method = it->method;
  p->from_overlay = it->from_overlay;
  switch (p->method)
    {
    case GET_FROM_IMAGE:
      p->u.image.object = it->object;
      p->u.image.image_id = it->image_id;
      p->u.image.slice = it->slice;
      break;
    case GET_FROM_STRETCH:
      p->u.stretch.object = it->object;
      break;
    }
  p->position = position ? *position : it->position;
  p->current = it->current;
  p->end_charpos = it->end_charpos;
  p->string_nchars = it->string_nchars;
  p->area = it->area;
  p->multibyte_p = it->multibyte_p;
  p->avoid_cursor_p = it->avoid_cursor_p;
  p->space_width = it->space_width;
  p->font_height = it->font_height;
  p->voffset = it->voffset;
  p->string_from_display_prop_p = it->string_from_display_prop_p;
  p->string_from_prefix_prop_p = it->string_from_prefix_prop_p;
  p->display_ellipsis_p = 0;
  p->line_wrap = it->line_wrap;
  p->bidi_p = it->bidi_p;
  p->paragraph_embedding = it->paragraph_embedding;
  p->from_disp_prop_p = it->from_disp_prop_p;
  ++it->sp;

  /* Save the state of the bidi iterator as well. */
  if (it->bidi_p)
    bidi_push_it (&it->bidi_it);
}

static void
iterate_out_of_display_property (struct it *it)
{
  int buffer_p = !STRINGP (it->string);
  EMACS_INT eob = (buffer_p ? ZV : it->end_charpos);
  EMACS_INT bob = (buffer_p ? BEGV : 0);

  xassert (eob >= CHARPOS (it->position) && CHARPOS (it->position) >= bob);

  /* Maybe initialize paragraph direction.  If we are at the beginning
     of a new paragraph, next_element_from_buffer may not have a
     chance to do that.  */
  if (it->bidi_it.first_elt && it->bidi_it.charpos < eob)
    bidi_paragraph_init (it->paragraph_embedding, &it->bidi_it, 1);
  /* prev_stop can be zero, so check against BEGV as well.  */
  while (it->bidi_it.charpos >= bob
	 && it->prev_stop <= it->bidi_it.charpos
	 && it->bidi_it.charpos < CHARPOS (it->position)
	 && it->bidi_it.charpos < eob)
    bidi_move_to_visually_next (&it->bidi_it);
  /* Record the stop_pos we just crossed, for when we cross it
     back, maybe.  */
  if (it->bidi_it.charpos > CHARPOS (it->position))
    it->prev_stop = CHARPOS (it->position);
  /* If we ended up not where pop_it put us, resync IT's
     positional members with the bidi iterator. */
  if (it->bidi_it.charpos != CHARPOS (it->position))
    SET_TEXT_POS (it->position, it->bidi_it.charpos, it->bidi_it.bytepos);
  if (buffer_p)
    it->current.pos = it->position;
  else
    it->current.string_pos = it->position;
}

/* Restore IT's settings from IT->stack.  Called, for example, when no
   more overlay strings must be processed, and we return to delivering
   display elements from a buffer, or when the end of a string from a
   `display' property is reached and we return to delivering display
   elements from an overlay string, or from a buffer.  */

static void
pop_it (struct it *it)
{
  struct iterator_stack_entry *p;
  int from_display_prop = it->from_disp_prop_p;

  xassert (it->sp > 0);
  --it->sp;
  p = it->stack + it->sp;
  it->stop_charpos = p->stop_charpos;
  it->prev_stop = p->prev_stop;
  it->base_level_stop = p->base_level_stop;
  it->cmp_it = p->cmp_it;
  it->face_id = p->face_id;
  it->current = p->current;
  it->position = p->position;
  it->string = p->string;
  it->from_overlay = p->from_overlay;
  if (NILP (it->string))
    SET_TEXT_POS (it->current.string_pos, -1, -1);
  it->method = p->method;
  switch (it->method)
    {
    case GET_FROM_IMAGE:
      it->image_id = p->u.image.image_id;
      it->object = p->u.image.object;
      it->slice = p->u.image.slice;
      break;
    case GET_FROM_STRETCH:
      it->object = p->u.stretch.object;
      break;
    case GET_FROM_BUFFER:
      it->object = it->w->buffer;
      break;
    case GET_FROM_STRING:
      it->object = it->string;
      break;
    case GET_FROM_DISPLAY_VECTOR:
      if (it->s)
	it->method = GET_FROM_C_STRING;
      else if (STRINGP (it->string))
	it->method = GET_FROM_STRING;
      else
	{
	  it->method = GET_FROM_BUFFER;
	  it->object = it->w->buffer;
	}
    }
  it->end_charpos = p->end_charpos;
  it->string_nchars = p->string_nchars;
  it->area = p->area;
  it->multibyte_p = p->multibyte_p;
  it->avoid_cursor_p = p->avoid_cursor_p;
  it->space_width = p->space_width;
  it->font_height = p->font_height;
  it->voffset = p->voffset;
  it->string_from_display_prop_p = p->string_from_display_prop_p;
  it->string_from_prefix_prop_p = p->string_from_prefix_prop_p;
  it->line_wrap = p->line_wrap;
  it->bidi_p = p->bidi_p;
  it->paragraph_embedding = p->paragraph_embedding;
  it->from_disp_prop_p = p->from_disp_prop_p;
  if (it->bidi_p)
    {
      bidi_pop_it (&it->bidi_it);
      /* Bidi-iterate until we get out of the portion of text, if any,
	 covered by a `display' text property or by an overlay with
	 `display' property.  (We cannot just jump there, because the
	 internal coherency of the bidi iterator state can not be
	 preserved across such jumps.)  We also must determine the
	 paragraph base direction if the overlay we just processed is
	 at the beginning of a new paragraph.  */
      if (from_display_prop
	  && (it->method == GET_FROM_BUFFER || it->method == GET_FROM_STRING))
	iterate_out_of_display_property (it);

      xassert ((BUFFERP (it->object)
		&& IT_CHARPOS (*it) == it->bidi_it.charpos
		&& IT_BYTEPOS (*it) == it->bidi_it.bytepos)
	       || (STRINGP (it->object)
		   && IT_STRING_CHARPOS (*it) == it->bidi_it.charpos
		   && IT_STRING_BYTEPOS (*it) == it->bidi_it.bytepos)
	       || (CONSP (it->object) && it->method == GET_FROM_STRETCH));
    }
}



/***********************************************************************
			  Moving over lines
 ***********************************************************************/

/* Set IT's current position to the previous line start.  */

static void
back_to_previous_line_start (struct it *it)
{
  IT_CHARPOS (*it) = find_next_newline_no_quit (IT_CHARPOS (*it) - 1, -1);
  IT_BYTEPOS (*it) = CHAR_TO_BYTE (IT_CHARPOS (*it));
}


/* Move IT to the next line start.

   Value is non-zero if a newline was found.  Set *SKIPPED_P to 1 if
   we skipped over part of the text (as opposed to moving the iterator
   continuously over the text).  Otherwise, don't change the value
   of *SKIPPED_P.

   If BIDI_IT_PREV is non-NULL, store into it the state of the bidi
   iterator on the newline, if it was found.

   Newlines may come from buffer text, overlay strings, or strings
   displayed via the `display' property.  That's the reason we can't
   simply use find_next_newline_no_quit.

   Note that this function may not skip over invisible text that is so
   because of text properties and immediately follows a newline.  If
   it would, function reseat_at_next_visible_line_start, when called
   from set_iterator_to_next, would effectively make invisible
   characters following a newline part of the wrong glyph row, which
   leads to wrong cursor motion.  */

static int
forward_to_next_line_start (struct it *it, int *skipped_p,
			    struct bidi_it *bidi_it_prev)
{
  EMACS_INT old_selective;
  int newline_found_p, n;
  const int MAX_NEWLINE_DISTANCE = 500;

  /* If already on a newline, just consume it to avoid unintended
     skipping over invisible text below.  */
  if (it->what == IT_CHARACTER
      && it->c == '\n'
      && CHARPOS (it->position) == IT_CHARPOS (*it))
    {
      if (it->bidi_p && bidi_it_prev)
	*bidi_it_prev = it->bidi_it;
      set_iterator_to_next (it, 0);
      it->c = 0;
      return 1;
    }

  /* Don't handle selective display in the following.  It's (a)
     unnecessary because it's done by the caller, and (b) leads to an
     infinite recursion because next_element_from_ellipsis indirectly
     calls this function.  */
  old_selective = it->selective;
  it->selective = 0;

  /* Scan for a newline within MAX_NEWLINE_DISTANCE display elements
     from buffer text.  */
  for (n = newline_found_p = 0;
       !newline_found_p && n < MAX_NEWLINE_DISTANCE;
       n += STRINGP (it->string) ? 0 : 1)
    {
      if (!get_next_display_element (it))
	return 0;
      newline_found_p = it->what == IT_CHARACTER && it->c == '\n';
      if (newline_found_p && it->bidi_p && bidi_it_prev)
	*bidi_it_prev = it->bidi_it;
      set_iterator_to_next (it, 0);
    }

  /* If we didn't find a newline near enough, see if we can use a
     short-cut.  */
  if (!newline_found_p)
    {
      EMACS_INT start = IT_CHARPOS (*it);
      EMACS_INT limit = find_next_newline_no_quit (start, 1);
      Lisp_Object pos;

      xassert (!STRINGP (it->string));

      /* If there isn't any `display' property in sight, and no
	 overlays, we can just use the position of the newline in
	 buffer text.  */
      if (it->stop_charpos >= limit
	  || ((pos = Fnext_single_property_change (make_number (start),
						   Qdisplay, Qnil,
						   make_number (limit)),
	       NILP (pos))
	      && next_overlay_change (start) == ZV))
	{
	  if (!it->bidi_p)
	    {
	      IT_CHARPOS (*it) = limit;
	      IT_BYTEPOS (*it) = CHAR_TO_BYTE (limit);
	    }
	  else
	    {
	      struct bidi_it bprev;

	      /* Help bidi.c avoid expensive searches for display
		 properties and overlays, by telling it that there are
		 none up to `limit'.  */
	      if (it->bidi_it.disp_pos < limit)
		{
		  it->bidi_it.disp_pos = limit;
		  it->bidi_it.disp_prop = 0;
		}
	      do {
		bprev = it->bidi_it;
		bidi_move_to_visually_next (&it->bidi_it);
	      } while (it->bidi_it.charpos != limit);
	      IT_CHARPOS (*it) = limit;
	      IT_BYTEPOS (*it) = it->bidi_it.bytepos;
	      if (bidi_it_prev)
		*bidi_it_prev = bprev;
	    }
	  *skipped_p = newline_found_p = 1;
	}
      else
	{
	  while (get_next_display_element (it)
		 && !newline_found_p)
	    {
	      newline_found_p = ITERATOR_AT_END_OF_LINE_P (it);
	      if (newline_found_p && it->bidi_p && bidi_it_prev)
		*bidi_it_prev = it->bidi_it;
	      set_iterator_to_next (it, 0);
	    }
	}
    }

  it->selective = old_selective;
  return newline_found_p;
}


/* Set IT's current position to the previous visible line start.  Skip
   invisible text that is so either due to text properties or due to
   selective display.  Caution: this does not change IT->current_x and
   IT->hpos.  */

static void
back_to_previous_visible_line_start (struct it *it)
{
  while (IT_CHARPOS (*it) > BEGV)
    {
      back_to_previous_line_start (it);

      if (IT_CHARPOS (*it) <= BEGV)
	break;

      /* If selective > 0, then lines indented more than its value are
	 invisible.  */
      if (it->selective > 0
	  && indented_beyond_p (IT_CHARPOS (*it), IT_BYTEPOS (*it),
				it->selective))
	continue;

      /* Check the newline before point for invisibility.  */
      {
	Lisp_Object prop;
	prop = Fget_char_property (make_number (IT_CHARPOS (*it) - 1),
				     Qinvisible, it->window);
	if (TEXT_PROP_MEANS_INVISIBLE (prop))
	  continue;
      }

      if (IT_CHARPOS (*it) <= BEGV)
	break;

      {
	struct it it2;
	void *it2data = NULL;
	EMACS_INT pos;
	EMACS_INT beg, end;
	Lisp_Object val, overlay;

	SAVE_IT (it2, *it, it2data);

	/* If newline is part of a composition, continue from start of composition */
	if (find_composition (IT_CHARPOS (*it), -1, &beg, &end, &val, Qnil)
	    && beg < IT_CHARPOS (*it))
	  goto replaced;

	/* If newline is replaced by a display property, find start of overlay
	   or interval and continue search from that point.  */
	pos = --IT_CHARPOS (it2);
	--IT_BYTEPOS (it2);
	it2.sp = 0;
	bidi_unshelve_cache (NULL, 0);
	it2.string_from_display_prop_p = 0;
	it2.from_disp_prop_p = 0;
	if (handle_display_prop (&it2) == HANDLED_RETURN
	    && !NILP (val = get_char_property_and_overlay
		      (make_number (pos), Qdisplay, Qnil, &overlay))
	    && (OVERLAYP (overlay)
		? (beg = OVERLAY_POSITION (OVERLAY_START (overlay)))
		: get_property_and_range (pos, Qdisplay, &val, &beg, &end, Qnil)))
	  {
	    RESTORE_IT (it, it, it2data);
	    goto replaced;
	  }

	/* Newline is not replaced by anything -- so we are done.  */
	RESTORE_IT (it, it, it2data);
	break;

      replaced:
	if (beg < BEGV)
	  beg = BEGV;
	IT_CHARPOS (*it) = beg;
	IT_BYTEPOS (*it) = buf_charpos_to_bytepos (current_buffer, beg);
      }
    }

  it->continuation_lines_width = 0;

  xassert (IT_CHARPOS (*it) >= BEGV);
  xassert (IT_CHARPOS (*it) == BEGV
	   || FETCH_BYTE (IT_BYTEPOS (*it) - 1) == '\n');
  CHECK_IT (it);
}


/* Reseat iterator IT at the previous visible line start.  Skip
   invisible text that is so either due to text properties or due to
   selective display.  At the end, update IT's overlay information,
   face information etc.  */

void
reseat_at_previous_visible_line_start (struct it *it)
{
  back_to_previous_visible_line_start (it);
  reseat (it, it->current.pos, 1);
  CHECK_IT (it);
}


/* Reseat iterator IT on the next visible line start in the current
   buffer.  ON_NEWLINE_P non-zero means position IT on the newline
   preceding the line start.  Skip over invisible text that is so
   because of selective display.  Compute faces, overlays etc at the
   new position.  Note that this function does not skip over text that
   is invisible because of text properties.  */

static void
reseat_at_next_visible_line_start (struct it *it, int on_newline_p)
{
  int newline_found_p, skipped_p = 0;
  struct bidi_it bidi_it_prev;

  newline_found_p = forward_to_next_line_start (it, &skipped_p, &bidi_it_prev);

  /* Skip over lines that are invisible because they are indented
     more than the value of IT->selective.  */
  if (it->selective > 0)
    while (IT_CHARPOS (*it) < ZV
	   && indented_beyond_p (IT_CHARPOS (*it), IT_BYTEPOS (*it),
				 it->selective))
      {
	xassert (IT_BYTEPOS (*it) == BEGV
		 || FETCH_BYTE (IT_BYTEPOS (*it) - 1) == '\n');
	newline_found_p =
	  forward_to_next_line_start (it, &skipped_p, &bidi_it_prev);
      }

  /* Position on the newline if that's what's requested.  */
  if (on_newline_p && newline_found_p)
    {
      if (STRINGP (it->string))
	{
	  if (IT_STRING_CHARPOS (*it) > 0)
	    {
	      if (!it->bidi_p)
		{
		  --IT_STRING_CHARPOS (*it);
		  --IT_STRING_BYTEPOS (*it);
		}
	      else
		{
		  /* We need to restore the bidi iterator to the state
		     it had on the newline, and resync the IT's
		     position with that.  */
		  it->bidi_it = bidi_it_prev;
		  IT_STRING_CHARPOS (*it) = it->bidi_it.charpos;
		  IT_STRING_BYTEPOS (*it) = it->bidi_it.bytepos;
		}
	    }
	}
      else if (IT_CHARPOS (*it) > BEGV)
	{
	  if (!it->bidi_p)
	    {
	      --IT_CHARPOS (*it);
	      --IT_BYTEPOS (*it);
	    }
	  else
	    {
	      /* We need to restore the bidi iterator to the state it
		 had on the newline and resync IT with that.  */
	      it->bidi_it = bidi_it_prev;
	      IT_CHARPOS (*it) = it->bidi_it.charpos;
	      IT_BYTEPOS (*it) = it->bidi_it.bytepos;
	    }
	  reseat (it, it->current.pos, 0);
	}
    }
  else if (skipped_p)
    reseat (it, it->current.pos, 0);

  CHECK_IT (it);
}



/***********************************************************************
		   Changing an iterator's position
***********************************************************************/

/* Change IT's current position to POS in current_buffer.  If FORCE_P
   is non-zero, always check for text properties at the new position.
   Otherwise, text properties are only looked up if POS >=
   IT->check_charpos of a property.  */

static void
reseat (struct it *it, struct text_pos pos, int force_p)
{
  EMACS_INT original_pos = IT_CHARPOS (*it);

  reseat_1 (it, pos, 0);

  /* Determine where to check text properties.  Avoid doing it
     where possible because text property lookup is very expensive.  */
  if (force_p
      || CHARPOS (pos) > it->stop_charpos
      || CHARPOS (pos) < original_pos)
    {
      if (it->bidi_p)
	{
	  /* For bidi iteration, we need to prime prev_stop and
	     base_level_stop with our best estimations.  */
	  /* Implementation note: Of course, POS is not necessarily a
	     stop position, so assigning prev_pos to it is a lie; we
	     should have called compute_stop_backwards.  However, if
	     the current buffer does not include any R2L characters,
	     that call would be a waste of cycles, because the
	     iterator will never move back, and thus never cross this
	     "fake" stop position.  So we delay that backward search
	     until the time we really need it, in next_element_from_buffer.  */
	  if (CHARPOS (pos) != it->prev_stop)
	    it->prev_stop = CHARPOS (pos);
	  if (CHARPOS (pos) < it->base_level_stop)
	    it->base_level_stop = 0; /* meaning it's unknown */
	  handle_stop (it);
	}
      else
	{
	  handle_stop (it);
	  it->prev_stop = it->base_level_stop = 0;
	}

    }

  CHECK_IT (it);
}


/* Change IT's buffer position to POS.  SET_STOP_P non-zero means set
   IT->stop_pos to POS, also.  */

static void
reseat_1 (struct it *it, struct text_pos pos, int set_stop_p)
{
  /* Don't call this function when scanning a C string.  */
  xassert (it->s == NULL);

  /* POS must be a reasonable value.  */
  xassert (CHARPOS (pos) >= BEGV && CHARPOS (pos) <= ZV);

  it->current.pos = it->position = pos;
  it->end_charpos = ZV;
  it->dpvec = NULL;
  it->current.dpvec_index = -1;
  it->current.overlay_string_index = -1;
  IT_STRING_CHARPOS (*it) = -1;
  IT_STRING_BYTEPOS (*it) = -1;
  it->string = Qnil;
  it->method = GET_FROM_BUFFER;
  it->object = it->w->buffer;
  it->area = TEXT_AREA;
  it->multibyte_p = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  it->sp = 0;
  it->string_from_display_prop_p = 0;
  it->string_from_prefix_prop_p = 0;

  it->from_disp_prop_p = 0;
  it->face_before_selective_p = 0;
  if (it->bidi_p)
    {
      bidi_init_it (IT_CHARPOS (*it), IT_BYTEPOS (*it), FRAME_WINDOW_P (it->f),
		    &it->bidi_it);
      bidi_unshelve_cache (NULL, 0);
      it->bidi_it.paragraph_dir = NEUTRAL_DIR;
      it->bidi_it.string.s = NULL;
      it->bidi_it.string.lstring = Qnil;
      it->bidi_it.string.bufpos = 0;
      it->bidi_it.string.unibyte = 0;
    }

  if (set_stop_p)
    {
      it->stop_charpos = CHARPOS (pos);
      it->base_level_stop = CHARPOS (pos);
    }
}


/* Set up IT for displaying a string, starting at CHARPOS in window W.
   If S is non-null, it is a C string to iterate over.  Otherwise,
   STRING gives a Lisp string to iterate over.

   If PRECISION > 0, don't return more then PRECISION number of
   characters from the string.

   If FIELD_WIDTH > 0, return padding spaces until FIELD_WIDTH
   characters have been returned.  FIELD_WIDTH < 0 means an infinite
   field width.

   MULTIBYTE = 0 means disable processing of multibyte characters,
   MULTIBYTE > 0 means enable it,
   MULTIBYTE < 0 means use IT->multibyte_p.

   IT must be initialized via a prior call to init_iterator before
   calling this function.  */

static void
reseat_to_string (struct it *it, const char *s, Lisp_Object string,
		  EMACS_INT charpos, EMACS_INT precision, int field_width,
		  int multibyte)
{
  /* No region in strings.  */
  it->region_beg_charpos = it->region_end_charpos = -1;

  /* No text property checks performed by default, but see below.  */
  it->stop_charpos = -1;

  /* Set iterator position and end position.  */
  memset (&it->current, 0, sizeof it->current);
  it->current.overlay_string_index = -1;
  it->current.dpvec_index = -1;
  xassert (charpos >= 0);

  /* If STRING is specified, use its multibyteness, otherwise use the
     setting of MULTIBYTE, if specified.  */
  if (multibyte >= 0)
    it->multibyte_p = multibyte > 0;

  /* Bidirectional reordering of strings is controlled by the default
     value of bidi-display-reordering.  Don't try to reorder while
     loading loadup.el, as the necessary character property tables are
     not yet available.  */
  it->bidi_p =
    NILP (Vpurify_flag)
    && !NILP (BVAR (&buffer_defaults, bidi_display_reordering));

  if (s == NULL)
    {
      xassert (STRINGP (string));
      it->string = string;
      it->s = NULL;
      it->end_charpos = it->string_nchars = SCHARS (string);
      it->method = GET_FROM_STRING;
      it->current.string_pos = string_pos (charpos, string);

      if (it->bidi_p)
	{
	  it->bidi_it.string.lstring = string;
	  it->bidi_it.string.s = NULL;
	  it->bidi_it.string.schars = it->end_charpos;
	  it->bidi_it.string.bufpos = 0;
	  it->bidi_it.string.from_disp_str = 0;
	  it->bidi_it.string.unibyte = !it->multibyte_p;
	  bidi_init_it (charpos, IT_STRING_BYTEPOS (*it),
			FRAME_WINDOW_P (it->f), &it->bidi_it);
	}
    }
  else
    {
      it->s = (const unsigned char *) s;
      it->string = Qnil;

      /* Note that we use IT->current.pos, not it->current.string_pos,
	 for displaying C strings.  */
      IT_STRING_CHARPOS (*it) = IT_STRING_BYTEPOS (*it) = -1;
      if (it->multibyte_p)
	{
	  it->current.pos = c_string_pos (charpos, s, 1);
	  it->end_charpos = it->string_nchars = number_of_chars (s, 1);
	}
      else
	{
	  IT_CHARPOS (*it) = IT_BYTEPOS (*it) = charpos;
	  it->end_charpos = it->string_nchars = strlen (s);
	}

      if (it->bidi_p)
	{
	  it->bidi_it.string.lstring = Qnil;
	  it->bidi_it.string.s = (const unsigned char *) s;
	  it->bidi_it.string.schars = it->end_charpos;
	  it->bidi_it.string.bufpos = 0;
	  it->bidi_it.string.from_disp_str = 0;
	  it->bidi_it.string.unibyte = !it->multibyte_p;
	  bidi_init_it (charpos, IT_BYTEPOS (*it), FRAME_WINDOW_P (it->f),
			&it->bidi_it);
	}
      it->method = GET_FROM_C_STRING;
    }

  /* PRECISION > 0 means don't return more than PRECISION characters
     from the string.  */
  if (precision > 0 && it->end_charpos - charpos > precision)
    {
      it->end_charpos = it->string_nchars = charpos + precision;
      if (it->bidi_p)
	it->bidi_it.string.schars = it->end_charpos;
    }

  /* FIELD_WIDTH > 0 means pad with spaces until FIELD_WIDTH
     characters have been returned.  FIELD_WIDTH == 0 means don't pad,
     FIELD_WIDTH < 0 means infinite field width.  This is useful for
     padding with `-' at the end of a mode line.  */
  if (field_width < 0)
    field_width = INFINITY;
  /* Implementation note: We deliberately don't enlarge
     it->bidi_it.string.schars here to fit it->end_charpos, because
     the bidi iterator cannot produce characters out of thin air.  */
  if (field_width > it->end_charpos - charpos)
    it->end_charpos = charpos + field_width;

  /* Use the standard display table for displaying strings.  */
  if (DISP_TABLE_P (Vstandard_display_table))
    it->dp = XCHAR_TABLE (Vstandard_display_table);

  it->stop_charpos = charpos;
  it->prev_stop = charpos;
  it->base_level_stop = 0;
  if (it->bidi_p)
    {
      it->bidi_it.first_elt = 1;
      it->bidi_it.paragraph_dir = NEUTRAL_DIR;
      it->bidi_it.disp_pos = -1;
    }
  if (s == NULL && it->multibyte_p)
    {
      EMACS_INT endpos = SCHARS (it->string);
      if (endpos > it->end_charpos)
	endpos = it->end_charpos;
      composition_compute_stop_pos (&it->cmp_it, charpos, -1, endpos,
				    it->string);
    }
  CHECK_IT (it);
}



/***********************************************************************
			      Iteration
***********************************************************************/

/* Map enum it_method value to corresponding next_element_from_* function.  */

static int (* get_next_element[NUM_IT_METHODS]) (struct it *it) =
{
  next_element_from_buffer,
  next_element_from_display_vector,
  next_element_from_string,
  next_element_from_c_string,
  next_element_from_image,
  next_element_from_stretch
};

#define GET_NEXT_DISPLAY_ELEMENT(it) (*get_next_element[(it)->method]) (it)


/* Return 1 iff a character at CHARPOS (and BYTEPOS) is composed
   (possibly with the following characters).  */

#define CHAR_COMPOSED_P(IT,CHARPOS,BYTEPOS,END_CHARPOS)			\
  ((IT)->cmp_it.id >= 0							\
   || ((IT)->cmp_it.stop_pos == (CHARPOS)				\
       && composition_reseat_it (&(IT)->cmp_it, CHARPOS, BYTEPOS,	\
				 END_CHARPOS, (IT)->w,			\
				 FACE_FROM_ID ((IT)->f, (IT)->face_id),	\
				 (IT)->string)))


/* Lookup the char-table Vglyphless_char_display for character C (-1
   if we want information for no-font case), and return the display
   method symbol.  By side-effect, update it->what and
   it->glyphless_method.  This function is called from
   get_next_display_element for each character element, and from
   x_produce_glyphs when no suitable font was found.  */

Lisp_Object
lookup_glyphless_char_display (int c, struct it *it)
{
  Lisp_Object glyphless_method = Qnil;

  if (CHAR_TABLE_P (Vglyphless_char_display)
      && CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (Vglyphless_char_display)) >= 1)
    {
      if (c >= 0)
	{
	  glyphless_method = CHAR_TABLE_REF (Vglyphless_char_display, c);
	  if (CONSP (glyphless_method))
	    glyphless_method = FRAME_WINDOW_P (it->f)
	      ? XCAR (glyphless_method)
	      : XCDR (glyphless_method);
	}
      else
	glyphless_method = XCHAR_TABLE (Vglyphless_char_display)->extras[0];
    }

 retry:
  if (NILP (glyphless_method))
    {
      if (c >= 0)
	/* The default is to display the character by a proper font.  */
	return Qnil;
      /* The default for the no-font case is to display an empty box.  */
      glyphless_method = Qempty_box;
    }
  if (EQ (glyphless_method, Qzero_width))
    {
      if (c >= 0)
	return glyphless_method;
      /* This method can't be used for the no-font case.  */
      glyphless_method = Qempty_box;
    }
  if (EQ (glyphless_method, Qthin_space))
    it->glyphless_method = GLYPHLESS_DISPLAY_THIN_SPACE;
  else if (EQ (glyphless_method, Qempty_box))
    it->glyphless_method = GLYPHLESS_DISPLAY_EMPTY_BOX;
  else if (EQ (glyphless_method, Qhex_code))
    it->glyphless_method = GLYPHLESS_DISPLAY_HEX_CODE;
  else if (STRINGP (glyphless_method))
    it->glyphless_method = GLYPHLESS_DISPLAY_ACRONYM;
  else
    {
      /* Invalid value.  We use the default method.  */
      glyphless_method = Qnil;
      goto retry;
    }
  it->what = IT_GLYPHLESS;
  return glyphless_method;
}

/* Load IT's display element fields with information about the next
   display element from the current position of IT.  Value is zero if
   end of buffer (or C string) is reached.  */

static struct frame *last_escape_glyph_frame = NULL;
static unsigned last_escape_glyph_face_id = (1 << FACE_ID_BITS);
static int last_escape_glyph_merged_face_id = 0;

struct frame *last_glyphless_glyph_frame = NULL;
unsigned last_glyphless_glyph_face_id = (1 << FACE_ID_BITS);
int last_glyphless_glyph_merged_face_id = 0;

static int
get_next_display_element (struct it *it)
{
  /* Non-zero means that we found a display element.  Zero means that
     we hit the end of what we iterate over.  Performance note: the
     function pointer `method' used here turns out to be faster than
     using a sequence of if-statements.  */
  int success_p;

 get_next:
  success_p = GET_NEXT_DISPLAY_ELEMENT (it);

  if (it->what == IT_CHARACTER)
    {
      /* UAX#9, L4: "A character is depicted by a mirrored glyph if
	 and only if (a) the resolved directionality of that character
	 is R..."  */
      /* FIXME: Do we need an exception for characters from display
	 tables?  */
      if (it->bidi_p && it->bidi_it.type == STRONG_R)
	it->c = bidi_mirror_char (it->c);
      /* Map via display table or translate control characters.
	 IT->c, IT->len etc. have been set to the next character by
	 the function call above.  If we have a display table, and it
	 contains an entry for IT->c, translate it.  Don't do this if
	 IT->c itself comes from a display table, otherwise we could
	 end up in an infinite recursion.  (An alternative could be to
	 count the recursion depth of this function and signal an
	 error when a certain maximum depth is reached.)  Is it worth
	 it?  */
      if (success_p && it->dpvec == NULL)
	{
	  Lisp_Object dv;
	  struct charset *unibyte = CHARSET_FROM_ID (charset_unibyte);
	  int nonascii_space_p = 0;
	  int nonascii_hyphen_p = 0;
	  int c = it->c;	/* This is the character to display.  */

	  if (! it->multibyte_p && ! ASCII_CHAR_P (c))
	    {
	      xassert (SINGLE_BYTE_CHAR_P (c));
	      if (unibyte_display_via_language_environment)
		{
		  c = DECODE_CHAR (unibyte, c);
		  if (c < 0)
		    c = BYTE8_TO_CHAR (it->c);
		}
	      else
		c = BYTE8_TO_CHAR (it->c);
	    }

	  if (it->dp
	      && (dv = DISP_CHAR_VECTOR (it->dp, c),
		  VECTORP (dv)))
	    {
	      struct Lisp_Vector *v = XVECTOR (dv);

	      /* Return the first character from the display table
		 entry, if not empty.  If empty, don't display the
		 current character.  */
	      if (v->header.size)
		{
		  it->dpvec_char_len = it->len;
		  it->dpvec = v->contents;
		  it->dpend = v->contents + v->header.size;
		  it->current.dpvec_index = 0;
		  it->dpvec_face_id = -1;
		  it->saved_face_id = it->face_id;
		  it->method = GET_FROM_DISPLAY_VECTOR;
		  it->ellipsis_p = 0;
		}
	      else
		{
		  set_iterator_to_next (it, 0);
		}
	      goto get_next;
	    }

	  if (! NILP (lookup_glyphless_char_display (c, it)))
	    {
	      if (it->what == IT_GLYPHLESS)
		goto done;
	      /* Don't display this character.  */
	      set_iterator_to_next (it, 0);
	      goto get_next;
	    }

	  /* If `nobreak-char-display' is non-nil, we display
	     non-ASCII spaces and hyphens specially.  */
	  if (! ASCII_CHAR_P (c) && ! NILP (Vnobreak_char_display))
	    {
	      if (c == 0xA0)
		nonascii_space_p = 1;
	      else if (c == 0xAD || c == 0x2010 || c == 0x2011)
		nonascii_hyphen_p = 1;
	    }

	  /* Translate control characters into `\003' or `^C' form.
	     Control characters coming from a display table entry are
	     currently not translated because we use IT->dpvec to hold
	     the translation.  This could easily be changed but I
	     don't believe that it is worth doing.

	     The characters handled by `nobreak-char-display' must be
	     translated too.

	     Non-printable characters and raw-byte characters are also
	     translated to octal form.  */
	  if (((c < ' ' || c == 127) /* ASCII control chars */
	       ? (it->area != TEXT_AREA
		  /* In mode line, treat \n, \t like other crl chars.  */
		  || (c != '\t'
		      && it->glyph_row
		      && (it->glyph_row->mode_line_p || it->avoid_cursor_p))
		  || (c != '\n' && c != '\t'))
	       : (nonascii_space_p
		  || nonascii_hyphen_p
		  || CHAR_BYTE8_P (c)
		  || ! CHAR_PRINTABLE_P (c))))
	    {
	      /* C is a control character, non-ASCII space/hyphen,
		 raw-byte, or a non-printable character which must be
		 displayed either as '\003' or as `^C' where the '\\'
		 and '^' can be defined in the display table.  Fill
		 IT->ctl_chars with glyphs for what we have to
		 display.  Then, set IT->dpvec to these glyphs.  */
	      Lisp_Object gc;
	      int ctl_len;
	      int face_id;
	      EMACS_INT lface_id = 0;
	      int escape_glyph;

	      /* Handle control characters with ^.  */

	      if (ASCII_CHAR_P (c) && it->ctl_arrow_p)
		{
		  int g;

		  g = '^';	     /* default glyph for Control */
		  /* Set IT->ctl_chars[0] to the glyph for `^'.  */
		  if (it->dp
		      && (gc = DISP_CTRL_GLYPH (it->dp), GLYPH_CODE_P (gc))
		      && GLYPH_CODE_CHAR_VALID_P (gc))
		    {
		      g = GLYPH_CODE_CHAR (gc);
		      lface_id = GLYPH_CODE_FACE (gc);
		    }
		  if (lface_id)
		    {
		      face_id = merge_faces (it->f, Qt, lface_id, it->face_id);
		    }
		  else if (it->f == last_escape_glyph_frame
			   && it->face_id == last_escape_glyph_face_id)
		    {
		      face_id = last_escape_glyph_merged_face_id;
		    }
		  else
		    {
		      /* Merge the escape-glyph face into the current face.  */
		      face_id = merge_faces (it->f, Qescape_glyph, 0,
					     it->face_id);
		      last_escape_glyph_frame = it->f;
		      last_escape_glyph_face_id = it->face_id;
		      last_escape_glyph_merged_face_id = face_id;
		    }

		  XSETINT (it->ctl_chars[0], g);
		  XSETINT (it->ctl_chars[1], c ^ 0100);
		  ctl_len = 2;
		  goto display_control;
		}

	      /* Handle non-ascii space in the mode where it only gets
		 highlighting.  */

	      if (nonascii_space_p && EQ (Vnobreak_char_display, Qt))
		{
		  /* Merge `nobreak-space' into the current face.  */
		  face_id = merge_faces (it->f, Qnobreak_space, 0,
					 it->face_id);
		  XSETINT (it->ctl_chars[0], ' ');
		  ctl_len = 1;
		  goto display_control;
		}

	      /* Handle sequences that start with the "escape glyph".  */

	      /* the default escape glyph is \.  */
	      escape_glyph = '\\';

	      if (it->dp
		  && (gc = DISP_ESCAPE_GLYPH (it->dp), GLYPH_CODE_P (gc))
		  && GLYPH_CODE_CHAR_VALID_P (gc))
		{
		  escape_glyph = GLYPH_CODE_CHAR (gc);
		  lface_id = GLYPH_CODE_FACE (gc);
		}
	      if (lface_id)
		{
		  /* The display table specified a face.
		     Merge it into face_id and also into escape_glyph.  */
		  face_id = merge_faces (it->f, Qt, lface_id,
					 it->face_id);
		}
	      else if (it->f == last_escape_glyph_frame
		       && it->face_id == last_escape_glyph_face_id)
		{
		  face_id = last_escape_glyph_merged_face_id;
		}
	      else
		{
		  /* Merge the escape-glyph face into the current face.  */
		  face_id = merge_faces (it->f, Qescape_glyph, 0,
					 it->face_id);
		  last_escape_glyph_frame = it->f;
		  last_escape_glyph_face_id = it->face_id;
		  last_escape_glyph_merged_face_id = face_id;
		}

	      /* Draw non-ASCII hyphen with just highlighting: */

	      if (nonascii_hyphen_p && EQ (Vnobreak_char_display, Qt))
		{
		  XSETINT (it->ctl_chars[0], '-');
		  ctl_len = 1;
		  goto display_control;
		}

	      /* Draw non-ASCII space/hyphen with escape glyph: */

	      if (nonascii_space_p || nonascii_hyphen_p)
		{
		  XSETINT (it->ctl_chars[0], escape_glyph);
		  XSETINT (it->ctl_chars[1], nonascii_space_p ? ' ' : '-');
		  ctl_len = 2;
		  goto display_control;
		}

	      {
		char str[10];
		int len, i;

		if (CHAR_BYTE8_P (c))
		  /* Display \200 instead of \17777600.  */
		  c = CHAR_TO_BYTE8 (c);
		len = sprintf (str, "%03o", c);

		XSETINT (it->ctl_chars[0], escape_glyph);
		for (i = 0; i < len; i++)
		  XSETINT (it->ctl_chars[i + 1], str[i]);
		ctl_len = len + 1;
	      }

	    display_control:
	      /* Set up IT->dpvec and return first character from it.  */
	      it->dpvec_char_len = it->len;
	      it->dpvec = it->ctl_chars;
	      it->dpend = it->dpvec + ctl_len;
	      it->current.dpvec_index = 0;
	      it->dpvec_face_id = face_id;
	      it->saved_face_id = it->face_id;
	      it->method = GET_FROM_DISPLAY_VECTOR;
	      it->ellipsis_p = 0;
	      goto get_next;
	    }
	  it->char_to_display = c;
	}
      else if (success_p)
	{
	  it->char_to_display = it->c;
	}
    }

  /* Adjust face id for a multibyte character.  There are no multibyte
     character in unibyte text.  */
  if ((it->what == IT_CHARACTER || it->what == IT_COMPOSITION)
      && it->multibyte_p
      && success_p
      && FRAME_WINDOW_P (it->f))
    {
      struct face *face = FACE_FROM_ID (it->f, it->face_id);

      if (it->what == IT_COMPOSITION && it->cmp_it.ch >= 0)
	{
	  /* Automatic composition with glyph-string.   */
	  Lisp_Object gstring = composition_gstring_from_id (it->cmp_it.id);

	  it->face_id = face_for_font (it->f, LGSTRING_FONT (gstring), face);
	}
      else
	{
	  EMACS_INT pos = (it->s ? -1
		     : STRINGP (it->string) ? IT_STRING_CHARPOS (*it)
		     : IT_CHARPOS (*it));
	  int c;

	  if (it->what == IT_CHARACTER)
	    c = it->char_to_display;
	  else
	    {
	      struct composition *cmp = composition_table[it->cmp_it.id];
	      int i;

	      c = ' ';
	      for (i = 0; i < cmp->glyph_len; i++)
		/* TAB in a composition means display glyphs with
		   padding space on the left or right.  */
		if ((c = COMPOSITION_GLYPH (cmp, i)) != '\t')
		  break;
	    }
	  it->face_id = FACE_FOR_CHAR (it->f, face, c, pos, it->string);
	}
    }

 done:
  /* Is this character the last one of a run of characters with
     box?  If yes, set IT->end_of_box_run_p to 1.  */
  if (it->face_box_p
      && it->s == NULL)
    {
      if (it->method == GET_FROM_STRING && it->sp)
	{
	  int face_id = underlying_face_id (it);
	  struct face *face = FACE_FROM_ID (it->f, face_id);

	  if (face)
	    {
	      if (face->box == FACE_NO_BOX)
		{
		  /* If the box comes from face properties in a
		     display string, check faces in that string.  */
		  int string_face_id = face_after_it_pos (it);
		  it->end_of_box_run_p
		    = (FACE_FROM_ID (it->f, string_face_id)->box
		       == FACE_NO_BOX);
		}
	      /* Otherwise, the box comes from the underlying face.
		 If this is the last string character displayed, check
		 the next buffer location.  */
	      else if ((IT_STRING_CHARPOS (*it) >= SCHARS (it->string) - 1)
		       && (it->current.overlay_string_index
			   == it->n_overlay_strings - 1))
		{
		  EMACS_INT ignore;
		  int next_face_id;
		  struct text_pos pos = it->current.pos;
		  INC_TEXT_POS (pos, it->multibyte_p);

		  next_face_id = face_at_buffer_position
		    (it->w, CHARPOS (pos), it->region_beg_charpos,
		     it->region_end_charpos, &ignore,
		     (IT_CHARPOS (*it) + TEXT_PROP_DISTANCE_LIMIT), 0,
		     -1);
		  it->end_of_box_run_p
		    = (FACE_FROM_ID (it->f, next_face_id)->box
		       == FACE_NO_BOX);
		}
	    }
	}
      else
	{
	  int face_id = face_after_it_pos (it);
	  it->end_of_box_run_p
	    = (face_id != it->face_id
	       && FACE_FROM_ID (it->f, face_id)->box == FACE_NO_BOX);
	}
    }
  /* If we reached the end of the object we've been iterating (e.g., a
     display string or an overlay string), and there's something on
     IT->stack, proceed with what's on the stack.  It doesn't make
     sense to return zero if there's unprocessed stuff on the stack,
     because otherwise that stuff will never be displayed.  */
  if (!success_p && it->sp > 0)
    {
      set_iterator_to_next (it, 0);
      success_p = get_next_display_element (it);
    }

  /* Value is 0 if end of buffer or string reached.  */
  return success_p;
}


/* Move IT to the next display element.

   RESEAT_P non-zero means if called on a newline in buffer text,
   skip to the next visible line start.

   Functions get_next_display_element and set_iterator_to_next are
   separate because I find this arrangement easier to handle than a
   get_next_display_element function that also increments IT's
   position.  The way it is we can first look at an iterator's current
   display element, decide whether it fits on a line, and if it does,
   increment the iterator position.  The other way around we probably
   would either need a flag indicating whether the iterator has to be
   incremented the next time, or we would have to implement a
   decrement position function which would not be easy to write.  */

void
set_iterator_to_next (struct it *it, int reseat_p)
{
  /* Reset flags indicating start and end of a sequence of characters
     with box.  Reset them at the start of this function because
     moving the iterator to a new position might set them.  */
  it->start_of_box_run_p = it->end_of_box_run_p = 0;

  switch (it->method)
    {
    case GET_FROM_BUFFER:
      /* The current display element of IT is a character from
	 current_buffer.  Advance in the buffer, and maybe skip over
	 invisible lines that are so because of selective display.  */
      if (ITERATOR_AT_END_OF_LINE_P (it) && reseat_p)
	reseat_at_next_visible_line_start (it, 0);
      else if (it->cmp_it.id >= 0)
	{
	  /* We are currently getting glyphs from a composition.  */
	  int i;

	  if (! it->bidi_p)
	    {
	      IT_CHARPOS (*it) += it->cmp_it.nchars;
	      IT_BYTEPOS (*it) += it->cmp_it.nbytes;
	      if (it->cmp_it.to < it->cmp_it.nglyphs)
		{
		  it->cmp_it.from = it->cmp_it.to;
		}
	      else
		{
		  it->cmp_it.id = -1;
		  composition_compute_stop_pos (&it->cmp_it, IT_CHARPOS (*it),
						IT_BYTEPOS (*it),
						it->end_charpos, Qnil);
		}
	    }
	  else if (! it->cmp_it.reversed_p)
	    {
	      /* Composition created while scanning forward.  */
	      /* Update IT's char/byte positions to point to the first
		 character of the next grapheme cluster, or to the
		 character visually after the current composition.  */
	      for (i = 0; i < it->cmp_it.nchars; i++)
		bidi_move_to_visually_next (&it->bidi_it);
	      IT_BYTEPOS (*it) = it->bidi_it.bytepos;
	      IT_CHARPOS (*it) = it->bidi_it.charpos;

	      if (it->cmp_it.to < it->cmp_it.nglyphs)
		{
		  /* Proceed to the next grapheme cluster.  */
		  it->cmp_it.from = it->cmp_it.to;
		}
	      else
		{
		  /* No more grapheme clusters in this composition.
		     Find the next stop position.  */
		  EMACS_INT stop = it->end_charpos;
		  if (it->bidi_it.scan_dir < 0)
		    /* Now we are scanning backward and don't know
		       where to stop.  */
		    stop = -1;
		  composition_compute_stop_pos (&it->cmp_it, IT_CHARPOS (*it),
						IT_BYTEPOS (*it), stop, Qnil);
		}
	    }
	  else
	    {
	      /* Composition created while scanning backward.  */
	      /* Update IT's char/byte positions to point to the last
		 character of the previous grapheme cluster, or the
		 character visually after the current composition.  */
	      for (i = 0; i < it->cmp_it.nchars; i++)
		bidi_move_to_visually_next (&it->bidi_it);
	      IT_BYTEPOS (*it) = it->bidi_it.bytepos;
	      IT_CHARPOS (*it) = it->bidi_it.charpos;
	      if (it->cmp_it.from > 0)
		{
		  /* Proceed to the previous grapheme cluster.  */
		  it->cmp_it.to = it->cmp_it.from;
		}
	      else
		{
		  /* No more grapheme clusters in this composition.
		     Find the next stop position.  */
		  EMACS_INT stop = it->end_charpos;
		  if (it->bidi_it.scan_dir < 0)
		    /* Now we are scanning backward and don't know
		       where to stop.  */
		    stop = -1;
		  composition_compute_stop_pos (&it->cmp_it, IT_CHARPOS (*it),
						IT_BYTEPOS (*it), stop, Qnil);
		}
	    }
	}
      else
	{
	  xassert (it->len != 0);

	  if (!it->bidi_p)
	    {
	      IT_BYTEPOS (*it) += it->len;
	      IT_CHARPOS (*it) += 1;
	    }
	  else
	    {
	      int prev_scan_dir = it->bidi_it.scan_dir;
	      /* If this is a new paragraph, determine its base
		 direction (a.k.a. its base embedding level).  */
	      if (it->bidi_it.new_paragraph)
		bidi_paragraph_init (it->paragraph_embedding, &it->bidi_it, 0);
	      bidi_move_to_visually_next (&it->bidi_it);
	      IT_BYTEPOS (*it) = it->bidi_it.bytepos;
	      IT_CHARPOS (*it) = it->bidi_it.charpos;
	      if (prev_scan_dir != it->bidi_it.scan_dir)
		{
		  /* As the scan direction was changed, we must
		     re-compute the stop position for composition.  */
		  EMACS_INT stop = it->end_charpos;
		  if (it->bidi_it.scan_dir < 0)
		    stop = -1;
		  composition_compute_stop_pos (&it->cmp_it, IT_CHARPOS (*it),
						IT_BYTEPOS (*it), stop, Qnil);
		}
	    }
	  xassert (IT_BYTEPOS (*it) == CHAR_TO_BYTE (IT_CHARPOS (*it)));
	}
      break;

    case GET_FROM_C_STRING:
      /* Current display element of IT is from a C string.  */
      if (!it->bidi_p
	  /* If the string position is beyond string's end, it means
	     next_element_from_c_string is padding the string with
	     blanks, in which case we bypass the bidi iterator,
	     because it cannot deal with such virtual characters.  */
	  || IT_CHARPOS (*it) >= it->bidi_it.string.schars)
	{
	  IT_BYTEPOS (*it) += it->len;
	  IT_CHARPOS (*it) += 1;
	}
      else
	{
	  bidi_move_to_visually_next (&it->bidi_it);
	  IT_BYTEPOS (*it) = it->bidi_it.bytepos;
	  IT_CHARPOS (*it) = it->bidi_it.charpos;
	}
      break;

    case GET_FROM_DISPLAY_VECTOR:
      /* Current display element of IT is from a display table entry.
	 Advance in the display table definition.  Reset it to null if
	 end reached, and continue with characters from buffers/
	 strings.  */
      ++it->current.dpvec_index;

      /* Restore face of the iterator to what they were before the
         display vector entry (these entries may contain faces).  */
      it->face_id = it->saved_face_id;

      if (it->dpvec + it->current.dpvec_index >= it->dpend)
	{
	  int recheck_faces = it->ellipsis_p;

	  if (it->s)
	    it->method = GET_FROM_C_STRING;
	  else if (STRINGP (it->string))
	    it->method = GET_FROM_STRING;
	  else
	    {
	      it->method = GET_FROM_BUFFER;
	      it->object = it->w->buffer;
	    }

	  it->dpvec = NULL;
	  it->current.dpvec_index = -1;

	  /* Skip over characters which were displayed via IT->dpvec.  */
	  if (it->dpvec_char_len < 0)
	    reseat_at_next_visible_line_start (it, 1);
	  else if (it->dpvec_char_len > 0)
	    {
	      if (it->method == GET_FROM_STRING
		  && it->n_overlay_strings > 0)
		it->ignore_overlay_strings_at_pos_p = 1;
	      it->len = it->dpvec_char_len;
	      set_iterator_to_next (it, reseat_p);
	    }

	  /* Maybe recheck faces after display vector */
	  if (recheck_faces)
	    it->stop_charpos = IT_CHARPOS (*it);
	}
      break;

    case GET_FROM_STRING:
      /* Current display element is a character from a Lisp string.  */
      xassert (it->s == NULL && STRINGP (it->string));
      /* Don't advance past string end.  These conditions are true
	 when set_iterator_to_next is called at the end of
	 get_next_display_element, in which case the Lisp string is
	 already exhausted, and all we want is pop the iterator
	 stack.  */
      if (it->current.overlay_string_index >= 0)
	{
	  /* This is an overlay string, so there's no padding with
	     spaces, and the number of characters in the string is
	     where the string ends.  */
	  if (IT_STRING_CHARPOS (*it) >= SCHARS (it->string))
	    goto consider_string_end;
	}
      else
	{
	  /* Not an overlay string.  There could be padding, so test
	     against it->end_charpos . */
	  if (IT_STRING_CHARPOS (*it) >= it->end_charpos)
	    goto consider_string_end;
	}
      if (it->cmp_it.id >= 0)
	{
	  int i;

	  if (! it->bidi_p)
	    {
	      IT_STRING_CHARPOS (*it) += it->cmp_it.nchars;
	      IT_STRING_BYTEPOS (*it) += it->cmp_it.nbytes;
	      if (it->cmp_it.to < it->cmp_it.nglyphs)
		it->cmp_it.from = it->cmp_it.to;
	      else
		{
		  it->cmp_it.id = -1;
		  composition_compute_stop_pos (&it->cmp_it,
						IT_STRING_CHARPOS (*it),
						IT_STRING_BYTEPOS (*it),
						it->end_charpos, it->string);
		}
	    }
	  else if (! it->cmp_it.reversed_p)
	    {
	      for (i = 0; i < it->cmp_it.nchars; i++)
		bidi_move_to_visually_next (&it->bidi_it);
	      IT_STRING_BYTEPOS (*it) = it->bidi_it.bytepos;
	      IT_STRING_CHARPOS (*it) = it->bidi_it.charpos;

	      if (it->cmp_it.to < it->cmp_it.nglyphs)
		it->cmp_it.from = it->cmp_it.to;
	      else
		{
		  EMACS_INT stop = it->end_charpos;
		  if (it->bidi_it.scan_dir < 0)
		    stop = -1;
		  composition_compute_stop_pos (&it->cmp_it,
						IT_STRING_CHARPOS (*it),
						IT_STRING_BYTEPOS (*it), stop,
						it->string);
		}
	    }
	  else
	    {
	      for (i = 0; i < it->cmp_it.nchars; i++)
		bidi_move_to_visually_next (&it->bidi_it);
	      IT_STRING_BYTEPOS (*it) = it->bidi_it.bytepos;
	      IT_STRING_CHARPOS (*it) = it->bidi_it.charpos;
	      if (it->cmp_it.from > 0)
		it->cmp_it.to = it->cmp_it.from;
	      else
		{
		  EMACS_INT stop = it->end_charpos;
		  if (it->bidi_it.scan_dir < 0)
		    stop = -1;
		  composition_compute_stop_pos (&it->cmp_it,
						IT_STRING_CHARPOS (*it),
						IT_STRING_BYTEPOS (*it), stop,
						it->string);
		}
	    }
	}
      else
	{
	  if (!it->bidi_p
	      /* If the string position is beyond string's end, it
		 means next_element_from_string is padding the string
		 with blanks, in which case we bypass the bidi
		 iterator, because it cannot deal with such virtual
		 characters.  */
	      || IT_STRING_CHARPOS (*it) >= it->bidi_it.string.schars)
	    {
	      IT_STRING_BYTEPOS (*it) += it->len;
	      IT_STRING_CHARPOS (*it) += 1;
	    }
	  else
	    {
	      int prev_scan_dir = it->bidi_it.scan_dir;

	      bidi_move_to_visually_next (&it->bidi_it);
	      IT_STRING_BYTEPOS (*it) = it->bidi_it.bytepos;
	      IT_STRING_CHARPOS (*it) = it->bidi_it.charpos;
	      if (prev_scan_dir != it->bidi_it.scan_dir)
		{
		  EMACS_INT stop = it->end_charpos;

		  if (it->bidi_it.scan_dir < 0)
		    stop = -1;
		  composition_compute_stop_pos (&it->cmp_it,
						IT_STRING_CHARPOS (*it),
						IT_STRING_BYTEPOS (*it), stop,
						it->string);
		}
	    }
	}

    consider_string_end:

      if (it->current.overlay_string_index >= 0)
	{
	  /* IT->string is an overlay string.  Advance to the
	     next, if there is one.  */
	  if (IT_STRING_CHARPOS (*it) >= SCHARS (it->string))
	    {
	      it->ellipsis_p = 0;
	      next_overlay_string (it);
	      if (it->ellipsis_p)
		setup_for_ellipsis (it, 0);
	    }
	}
      else
	{
	  /* IT->string is not an overlay string.  If we reached
	     its end, and there is something on IT->stack, proceed
	     with what is on the stack.  This can be either another
	     string, this time an overlay string, or a buffer.  */
	  if (IT_STRING_CHARPOS (*it) == SCHARS (it->string)
	      && it->sp > 0)
	    {
	      pop_it (it);
	      if (it->method == GET_FROM_STRING)
		goto consider_string_end;
	    }
	}
      break;

    case GET_FROM_IMAGE:
    case GET_FROM_STRETCH:
      /* The position etc with which we have to proceed are on
	 the stack.  The position may be at the end of a string,
         if the `display' property takes up the whole string.  */
      xassert (it->sp > 0);
      pop_it (it);
      if (it->method == GET_FROM_STRING)
	goto consider_string_end;
      break;

    default:
      /* There are no other methods defined, so this should be a bug.  */
      abort ();
    }

  xassert (it->method != GET_FROM_STRING
	   || (STRINGP (it->string)
	       && IT_STRING_CHARPOS (*it) >= 0));
}

/* Load IT's display element fields with information about the next
   display element which comes from a display table entry or from the
   result of translating a control character to one of the forms `^C'
   or `\003'.

   IT->dpvec holds the glyphs to return as characters.
   IT->saved_face_id holds the face id before the display vector--it
   is restored into IT->face_id in set_iterator_to_next.  */

static int
next_element_from_display_vector (struct it *it)
{
  Lisp_Object gc;

  /* Precondition.  */
  xassert (it->dpvec && it->current.dpvec_index >= 0);

  it->face_id = it->saved_face_id;

  /* KFS: This code used to check ip->dpvec[0] instead of the current element.
     That seemed totally bogus - so I changed it...  */
  gc = it->dpvec[it->current.dpvec_index];

  if (GLYPH_CODE_P (gc) && GLYPH_CODE_CHAR_VALID_P (gc))
    {
      it->c = GLYPH_CODE_CHAR (gc);
      it->len = CHAR_BYTES (it->c);

      /* The entry may contain a face id to use.  Such a face id is
	 the id of a Lisp face, not a realized face.  A face id of
	 zero means no face is specified.  */
      if (it->dpvec_face_id >= 0)
	it->face_id = it->dpvec_face_id;
      else
	{
	  EMACS_INT lface_id = GLYPH_CODE_FACE (gc);
	  if (lface_id > 0)
	    it->face_id = merge_faces (it->f, Qt, lface_id,
				       it->saved_face_id);
	}
    }
  else
    /* Display table entry is invalid.  Return a space.  */
    it->c = ' ', it->len = 1;

  /* Don't change position and object of the iterator here.  They are
     still the values of the character that had this display table
     entry or was translated, and that's what we want.  */
  it->what = IT_CHARACTER;
  return 1;
}

/* Get the first element of string/buffer in the visual order, after
   being reseated to a new position in a string or a buffer.  */
static void
get_visually_first_element (struct it *it)
{
  int string_p = STRINGP (it->string) || it->s;
  EMACS_INT eob = (string_p ? it->bidi_it.string.schars : ZV);
  EMACS_INT bob = (string_p ? 0 : BEGV);

  if (STRINGP (it->string))
    {
      it->bidi_it.charpos = IT_STRING_CHARPOS (*it);
      it->bidi_it.bytepos = IT_STRING_BYTEPOS (*it);
    }
  else
    {
      it->bidi_it.charpos = IT_CHARPOS (*it);
      it->bidi_it.bytepos = IT_BYTEPOS (*it);
    }

  if (it->bidi_it.charpos == eob)
    {
      /* Nothing to do, but reset the FIRST_ELT flag, like
	 bidi_paragraph_init does, because we are not going to
	 call it.  */
      it->bidi_it.first_elt = 0;
    }
  else if (it->bidi_it.charpos == bob
	   || (!string_p
	       && (FETCH_CHAR (it->bidi_it.bytepos - 1) == '\n'
		   || FETCH_CHAR (it->bidi_it.bytepos) == '\n')))
    {
      /* If we are at the beginning of a line/string, we can produce
	 the next element right away.  */
      bidi_paragraph_init (it->paragraph_embedding, &it->bidi_it, 1);
      bidi_move_to_visually_next (&it->bidi_it);
    }
  else
    {
      EMACS_INT orig_bytepos = it->bidi_it.bytepos;

      /* We need to prime the bidi iterator starting at the line's or
	 string's beginning, before we will be able to produce the
	 next element.  */
      if (string_p)
	it->bidi_it.charpos = it->bidi_it.bytepos = 0;
      else
	{
	  it->bidi_it.charpos = find_next_newline_no_quit (IT_CHARPOS (*it),
							   -1);
	  it->bidi_it.bytepos = CHAR_TO_BYTE (it->bidi_it.charpos);
	}
      bidi_paragraph_init (it->paragraph_embedding, &it->bidi_it, 1);
      do
	{
	  /* Now return to buffer/string position where we were asked
	     to get the next display element, and produce that.  */
	  bidi_move_to_visually_next (&it->bidi_it);
	}
      while (it->bidi_it.bytepos != orig_bytepos
	     && it->bidi_it.charpos < eob);
    }

  /*  Adjust IT's position information to where we ended up.  */
  if (STRINGP (it->string))
    {
      IT_STRING_CHARPOS (*it) = it->bidi_it.charpos;
      IT_STRING_BYTEPOS (*it) = it->bidi_it.bytepos;
    }
  else
    {
      IT_CHARPOS (*it) = it->bidi_it.charpos;
      IT_BYTEPOS (*it) = it->bidi_it.bytepos;
    }

  if (STRINGP (it->string) || !it->s)
    {
      EMACS_INT stop, charpos, bytepos;

      if (STRINGP (it->string))
	{
	  xassert (!it->s);
	  stop = SCHARS (it->string);
	  if (stop > it->end_charpos)
	    stop = it->end_charpos;
	  charpos = IT_STRING_CHARPOS (*it);
	  bytepos = IT_STRING_BYTEPOS (*it);
	}
      else
	{
	  stop = it->end_charpos;
	  charpos = IT_CHARPOS (*it);
	  bytepos = IT_BYTEPOS (*it);
	}
      if (it->bidi_it.scan_dir < 0)
	stop = -1;
      composition_compute_stop_pos (&it->cmp_it, charpos, bytepos, stop,
				    it->string);
    }
}

/* Load IT with the next display element from Lisp string IT->string.
   IT->current.string_pos is the current position within the string.
   If IT->current.overlay_string_index >= 0, the Lisp string is an
   overlay string.  */

static int
next_element_from_string (struct it *it)
{
  struct text_pos position;

  xassert (STRINGP (it->string));
  xassert (!it->bidi_p || EQ (it->string, it->bidi_it.string.lstring));
  xassert (IT_STRING_CHARPOS (*it) >= 0);
  position = it->current.string_pos;

  /* With bidi reordering, the character to display might not be the
     character at IT_STRING_CHARPOS.  BIDI_IT.FIRST_ELT non-zero means
     that we were reseat()ed to a new string, whose paragraph
     direction is not known.  */
  if (it->bidi_p && it->bidi_it.first_elt)
    {
      get_visually_first_element (it);
      SET_TEXT_POS (position, IT_STRING_CHARPOS (*it), IT_STRING_BYTEPOS (*it));
    }

  /* Time to check for invisible text?  */
  if (IT_STRING_CHARPOS (*it) < it->end_charpos)
    {
      if (IT_STRING_CHARPOS (*it) >= it->stop_charpos)
	{
	  if (!(!it->bidi_p
		|| BIDI_AT_BASE_LEVEL (it->bidi_it)
		|| IT_STRING_CHARPOS (*it) == it->stop_charpos))
	    {
	      /* With bidi non-linear iteration, we could find
		 ourselves far beyond the last computed stop_charpos,
		 with several other stop positions in between that we
		 missed.  Scan them all now, in buffer's logical
		 order, until we find and handle the last stop_charpos
		 that precedes our current position.  */
	      handle_stop_backwards (it, it->stop_charpos);
	      return GET_NEXT_DISPLAY_ELEMENT (it);
	    }
	  else
	    {
	      if (it->bidi_p)
		{
		  /* Take note of the stop position we just moved
		     across, for when we will move back across it.  */
		  it->prev_stop = it->stop_charpos;
		  /* If we are at base paragraph embedding level, take
		     note of the last stop position seen at this
		     level.  */
		  if (BIDI_AT_BASE_LEVEL (it->bidi_it))
		    it->base_level_stop = it->stop_charpos;
		}
	      handle_stop (it);

	      /* Since a handler may have changed IT->method, we must
		 recurse here.  */
	      return GET_NEXT_DISPLAY_ELEMENT (it);
	    }
	}
      else if (it->bidi_p
	       /* If we are before prev_stop, we may have overstepped
		  on our way backwards a stop_pos, and if so, we need
		  to handle that stop_pos.  */
	       && IT_STRING_CHARPOS (*it) < it->prev_stop
	       /* We can sometimes back up for reasons that have nothing
		  to do with bidi reordering.  E.g., compositions.  The
		  code below is only needed when we are above the base
		  embedding level, so test for that explicitly.  */
	       && !BIDI_AT_BASE_LEVEL (it->bidi_it))
	{
	  /* If we lost track of base_level_stop, we have no better
	     place for handle_stop_backwards to start from than string
	     beginning.  This happens, e.g., when we were reseated to
	     the previous screenful of text by vertical-motion.  */
	  if (it->base_level_stop <= 0
	      || IT_STRING_CHARPOS (*it) < it->base_level_stop)
	    it->base_level_stop = 0;
	  handle_stop_backwards (it, it->base_level_stop);
	  return GET_NEXT_DISPLAY_ELEMENT (it);
	}
    }

  if (it->current.overlay_string_index >= 0)
    {
      /* Get the next character from an overlay string.  In overlay
	 strings, there is no field width or padding with spaces to
	 do.  */
      if (IT_STRING_CHARPOS (*it) >= SCHARS (it->string))
	{
	  it->what = IT_EOB;
	  return 0;
	}
      else if (CHAR_COMPOSED_P (it, IT_STRING_CHARPOS (*it),
				IT_STRING_BYTEPOS (*it),
				it->bidi_it.scan_dir < 0
				? -1
				: SCHARS (it->string))
	       && next_element_from_composition (it))
	{
	  return 1;
	}
      else if (STRING_MULTIBYTE (it->string))
	{
	  const unsigned char *s = (SDATA (it->string)
				    + IT_STRING_BYTEPOS (*it));
	  it->c = string_char_and_length (s, &it->len);
	}
      else
	{
	  it->c = SREF (it->string, IT_STRING_BYTEPOS (*it));
	  it->len = 1;
	}
    }
  else
    {
      /* Get the next character from a Lisp string that is not an
	 overlay string.  Such strings come from the mode line, for
	 example.  We may have to pad with spaces, or truncate the
	 string.  See also next_element_from_c_string.  */
      if (IT_STRING_CHARPOS (*it) >= it->end_charpos)
	{
	  it->what = IT_EOB;
	  return 0;
	}
      else if (IT_STRING_CHARPOS (*it) >= it->string_nchars)
	{
	  /* Pad with spaces.  */
	  it->c = ' ', it->len = 1;
	  CHARPOS (position) = BYTEPOS (position) = -1;
	}
      else if (CHAR_COMPOSED_P (it, IT_STRING_CHARPOS (*it),
				IT_STRING_BYTEPOS (*it),
				it->bidi_it.scan_dir < 0
				? -1
				: it->string_nchars)
	       && next_element_from_composition (it))
	{
	  return 1;
	}
      else if (STRING_MULTIBYTE (it->string))
	{
	  const unsigned char *s = (SDATA (it->string)
				    + IT_STRING_BYTEPOS (*it));
	  it->c = string_char_and_length (s, &it->len);
	}
      else
	{
	  it->c = SREF (it->string, IT_STRING_BYTEPOS (*it));
	  it->len = 1;
	}
    }

  /* Record what we have and where it came from.  */
  it->what = IT_CHARACTER;
  it->object = it->string;
  it->position = position;
  return 1;
}


/* Load IT with next display element from C string IT->s.
   IT->string_nchars is the maximum number of characters to return
   from the string.  IT->end_charpos may be greater than
   IT->string_nchars when this function is called, in which case we
   may have to return padding spaces.  Value is zero if end of string
   reached, including padding spaces.  */

static int
next_element_from_c_string (struct it *it)
{
  int success_p = 1;

  xassert (it->s);
  xassert (!it->bidi_p || it->s == it->bidi_it.string.s);
  it->what = IT_CHARACTER;
  BYTEPOS (it->position) = CHARPOS (it->position) = 0;
  it->object = Qnil;

  /* With bidi reordering, the character to display might not be the
     character at IT_CHARPOS.  BIDI_IT.FIRST_ELT non-zero means that
     we were reseated to a new string, whose paragraph direction is
     not known.  */
  if (it->bidi_p && it->bidi_it.first_elt)
    get_visually_first_element (it);

  /* IT's position can be greater than IT->string_nchars in case a
     field width or precision has been specified when the iterator was
     initialized.  */
  if (IT_CHARPOS (*it) >= it->end_charpos)
    {
      /* End of the game.  */
      it->what = IT_EOB;
      success_p = 0;
    }
  else if (IT_CHARPOS (*it) >= it->string_nchars)
    {
      /* Pad with spaces.  */
      it->c = ' ', it->len = 1;
      BYTEPOS (it->position) = CHARPOS (it->position) = -1;
    }
  else if (it->multibyte_p)
    it->c = string_char_and_length (it->s + IT_BYTEPOS (*it), &it->len);
  else
    it->c = it->s[IT_BYTEPOS (*it)], it->len = 1;

  return success_p;
}


/* Set up IT to return characters from an ellipsis, if appropriate.
   The definition of the ellipsis glyphs may come from a display table
   entry.  This function fills IT with the first glyph from the
   ellipsis if an ellipsis is to be displayed.  */

static int
next_element_from_ellipsis (struct it *it)
{
  if (it->selective_display_ellipsis_p)
    setup_for_ellipsis (it, it->len);
  else
    {
      /* The face at the current position may be different from the
	 face we find after the invisible text.  Remember what it
	 was in IT->saved_face_id, and signal that it's there by
	 setting face_before_selective_p.  */
      it->saved_face_id = it->face_id;
      it->method = GET_FROM_BUFFER;
      it->object = it->w->buffer;
      reseat_at_next_visible_line_start (it, 1);
      it->face_before_selective_p = 1;
    }

  return GET_NEXT_DISPLAY_ELEMENT (it);
}


/* Deliver an image display element.  The iterator IT is already
   filled with image information (done in handle_display_prop).  Value
   is always 1.  */


static int
next_element_from_image (struct it *it)
{
  it->what = IT_IMAGE;
  it->ignore_overlay_strings_at_pos_p = 0;
  return 1;
}


/* Fill iterator IT with next display element from a stretch glyph
   property.  IT->object is the value of the text property.  Value is
   always 1.  */

static int
next_element_from_stretch (struct it *it)
{
  it->what = IT_STRETCH;
  return 1;
}

/* Scan backwards from IT's current position until we find a stop
   position, or until BEGV.  This is called when we find ourself
   before both the last known prev_stop and base_level_stop while
   reordering bidirectional text.  */

static void
compute_stop_pos_backwards (struct it *it)
{
  const int SCAN_BACK_LIMIT = 1000;
  struct text_pos pos;
  struct display_pos save_current = it->current;
  struct text_pos save_position = it->position;
  EMACS_INT charpos = IT_CHARPOS (*it);
  EMACS_INT where_we_are = charpos;
  EMACS_INT save_stop_pos = it->stop_charpos;
  EMACS_INT save_end_pos = it->end_charpos;

  xassert (NILP (it->string) && !it->s);
  xassert (it->bidi_p);
  it->bidi_p = 0;
  do
    {
      it->end_charpos = min (charpos + 1, ZV);
      charpos = max (charpos - SCAN_BACK_LIMIT, BEGV);
      SET_TEXT_POS (pos, charpos, BYTE_TO_CHAR (charpos));
      reseat_1 (it, pos, 0);
      compute_stop_pos (it);
      /* We must advance forward, right?  */
      if (it->stop_charpos <= charpos)
	abort ();
    }
  while (charpos > BEGV && it->stop_charpos >= it->end_charpos);

  if (it->stop_charpos <= where_we_are)
    it->prev_stop = it->stop_charpos;
  else
    it->prev_stop = BEGV;
  it->bidi_p = 1;
  it->current = save_current;
  it->position = save_position;
  it->stop_charpos = save_stop_pos;
  it->end_charpos = save_end_pos;
}

/* Scan forward from CHARPOS in the current buffer/string, until we
   find a stop position > current IT's position.  Then handle the stop
   position before that.  This is called when we bump into a stop
   position while reordering bidirectional text.  CHARPOS should be
   the last previously processed stop_pos (or BEGV/0, if none were
   processed yet) whose position is less that IT's current
   position.  */

static void
handle_stop_backwards (struct it *it, EMACS_INT charpos)
{
  int bufp = !STRINGP (it->string);
  EMACS_INT where_we_are = (bufp ? IT_CHARPOS (*it) : IT_STRING_CHARPOS (*it));
  struct display_pos save_current = it->current;
  struct text_pos save_position = it->position;
  struct text_pos pos1;
  EMACS_INT next_stop;

  /* Scan in strict logical order.  */
  xassert (it->bidi_p);
  it->bidi_p = 0;
  do
    {
      it->prev_stop = charpos;
      if (bufp)
	{
	  SET_TEXT_POS (pos1, charpos, CHAR_TO_BYTE (charpos));
	  reseat_1 (it, pos1, 0);
	}
      else
	it->current.string_pos = string_pos (charpos, it->string);
      compute_stop_pos (it);
      /* We must advance forward, right?  */
      if (it->stop_charpos <= it->prev_stop)
	abort ();
      charpos = it->stop_charpos;
    }
  while (charpos <= where_we_are);

  it->bidi_p = 1;
  it->current = save_current;
  it->position = save_position;
  next_stop = it->stop_charpos;
  it->stop_charpos = it->prev_stop;
  handle_stop (it);
  it->stop_charpos = next_stop;
}

/* Load IT with the next display element from current_buffer.  Value
   is zero if end of buffer reached.  IT->stop_charpos is the next
   position at which to stop and check for text properties or buffer
   end.  */

static int
next_element_from_buffer (struct it *it)
{
  int success_p = 1;

  xassert (IT_CHARPOS (*it) >= BEGV);
  xassert (NILP (it->string) && !it->s);
  xassert (!it->bidi_p
	   || (EQ (it->bidi_it.string.lstring, Qnil)
	       && it->bidi_it.string.s == NULL));

  /* With bidi reordering, the character to display might not be the
     character at IT_CHARPOS.  BIDI_IT.FIRST_ELT non-zero means that
     we were reseat()ed to a new buffer position, which is potentially
     a different paragraph.  */
  if (it->bidi_p && it->bidi_it.first_elt)
    {
      get_visually_first_element (it);
      SET_TEXT_POS (it->position, IT_CHARPOS (*it), IT_BYTEPOS (*it));
    }

  if (IT_CHARPOS (*it) >= it->stop_charpos)
    {
      if (IT_CHARPOS (*it) >= it->end_charpos)
	{
	  int overlay_strings_follow_p;

	  /* End of the game, except when overlay strings follow that
	     haven't been returned yet.  */
	  if (it->overlay_strings_at_end_processed_p)
	    overlay_strings_follow_p = 0;
	  else
	    {
	      it->overlay_strings_at_end_processed_p = 1;
	      overlay_strings_follow_p = get_overlay_strings (it, 0);
	    }

	  if (overlay_strings_follow_p)
	    success_p = GET_NEXT_DISPLAY_ELEMENT (it);
	  else
	    {
	      it->what = IT_EOB;
	      it->position = it->current.pos;
	      success_p = 0;
	    }
	}
      else if (!(!it->bidi_p
		 || BIDI_AT_BASE_LEVEL (it->bidi_it)
		 || IT_CHARPOS (*it) == it->stop_charpos))
	{
	  /* With bidi non-linear iteration, we could find ourselves
	     far beyond the last computed stop_charpos, with several
	     other stop positions in between that we missed.  Scan
	     them all now, in buffer's logical order, until we find
	     and handle the last stop_charpos that precedes our
	     current position.  */
	  handle_stop_backwards (it, it->stop_charpos);
	  return GET_NEXT_DISPLAY_ELEMENT (it);
	}
      else
	{
	  if (it->bidi_p)
	    {
	      /* Take note of the stop position we just moved across,
		 for when we will move back across it.  */
	      it->prev_stop = it->stop_charpos;
	      /* If we are at base paragraph embedding level, take
		 note of the last stop position seen at this
		 level.  */
	      if (BIDI_AT_BASE_LEVEL (it->bidi_it))
		it->base_level_stop = it->stop_charpos;
	    }
	  handle_stop (it);
	  return GET_NEXT_DISPLAY_ELEMENT (it);
	}
    }
  else if (it->bidi_p
	   /* If we are before prev_stop, we may have overstepped on
	      our way backwards a stop_pos, and if so, we need to
	      handle that stop_pos.  */
	   && IT_CHARPOS (*it) < it->prev_stop
	   /* We can sometimes back up for reasons that have nothing
	      to do with bidi reordering.  E.g., compositions.  The
	      code below is only needed when we are above the base
	      embedding level, so test for that explicitly.  */
	   && !BIDI_AT_BASE_LEVEL (it->bidi_it))
    {
      if (it->base_level_stop <= 0
	  || IT_CHARPOS (*it) < it->base_level_stop)
	{
	  /* If we lost track of base_level_stop, we need to find
	     prev_stop by looking backwards.  This happens, e.g., when
	     we were reseated to the previous screenful of text by
	     vertical-motion.  */
	  it->base_level_stop = BEGV;
	  compute_stop_pos_backwards (it);
	  handle_stop_backwards (it, it->prev_stop);
	}
      else
	handle_stop_backwards (it, it->base_level_stop);
      return GET_NEXT_DISPLAY_ELEMENT (it);
    }
  else
    {
      /* No face changes, overlays etc. in sight, so just return a
	 character from current_buffer.  */
      unsigned char *p;
      EMACS_INT stop;

      /* Maybe run the redisplay end trigger hook.  Performance note:
	 This doesn't seem to cost measurable time.  */
      if (it->redisplay_end_trigger_charpos
	  && it->glyph_row
	  && IT_CHARPOS (*it) >= it->redisplay_end_trigger_charpos)
	run_redisplay_end_trigger_hook (it);

      stop = it->bidi_it.scan_dir < 0 ? -1 : it->end_charpos;
      if (CHAR_COMPOSED_P (it, IT_CHARPOS (*it), IT_BYTEPOS (*it),
			   stop)
	  && next_element_from_composition (it))
	{
	  return 1;
	}

      /* Get the next character, maybe multibyte.  */
      p = BYTE_POS_ADDR (IT_BYTEPOS (*it));
      if (it->multibyte_p && !ASCII_BYTE_P (*p))
	it->c = STRING_CHAR_AND_LENGTH (p, it->len);
      else
	it->c = *p, it->len = 1;

      /* Record what we have and where it came from.  */
      it->what = IT_CHARACTER;
      it->object = it->w->buffer;
      it->position = it->current.pos;

      /* Normally we return the character found above, except when we
	 really want to return an ellipsis for selective display.  */
      if (it->selective)
	{
	  if (it->c == '\n')
	    {
	      /* A value of selective > 0 means hide lines indented more
		 than that number of columns.  */
	      if (it->selective > 0
		  && IT_CHARPOS (*it) + 1 < ZV
		  && indented_beyond_p (IT_CHARPOS (*it) + 1,
					IT_BYTEPOS (*it) + 1,
					it->selective))
		{
		  success_p = next_element_from_ellipsis (it);
		  it->dpvec_char_len = -1;
		}
	    }
	  else if (it->c == '\r' && it->selective == -1)
	    {
	      /* A value of selective == -1 means that everything from the
		 CR to the end of the line is invisible, with maybe an
		 ellipsis displayed for it.  */
	      success_p = next_element_from_ellipsis (it);
	      it->dpvec_char_len = -1;
	    }
	}
    }

  /* Value is zero if end of buffer reached.  */
  xassert (!success_p || it->what != IT_CHARACTER || it->len > 0);
  return success_p;
}


/* Run the redisplay end trigger hook for IT.  */

static void
run_redisplay_end_trigger_hook (struct it *it)
{
  Lisp_Object args[3];

  /* IT->glyph_row should be non-null, i.e. we should be actually
     displaying something, or otherwise we should not run the hook.  */
  xassert (it->glyph_row);

  /* Set up hook arguments.  */
  args[0] = Qredisplay_end_trigger_functions;
  args[1] = it->window;
  XSETINT (args[2], it->redisplay_end_trigger_charpos);
  it->redisplay_end_trigger_charpos = 0;

  /* Since we are *trying* to run these functions, don't try to run
     them again, even if they get an error.  */
  it->w->redisplay_end_trigger = Qnil;
  Frun_hook_with_args (3, args);

  /* Notice if it changed the face of the character we are on.  */
  handle_face_prop (it);
}


/* Deliver a composition display element.  Unlike the other
   next_element_from_XXX, this function is not registered in the array
   get_next_element[].  It is called from next_element_from_buffer and
   next_element_from_string when necessary.  */

static int
next_element_from_composition (struct it *it)
{
  it->what = IT_COMPOSITION;
  it->len = it->cmp_it.nbytes;
  if (STRINGP (it->string))
    {
      if (it->c < 0)
	{
	  IT_STRING_CHARPOS (*it) += it->cmp_it.nchars;
	  IT_STRING_BYTEPOS (*it) += it->cmp_it.nbytes;
	  return 0;
	}
      it->position = it->current.string_pos;
      it->object = it->string;
      it->c = composition_update_it (&it->cmp_it, IT_STRING_CHARPOS (*it),
				     IT_STRING_BYTEPOS (*it), it->string);
    }
  else
    {
      if (it->c < 0)
	{
	  IT_CHARPOS (*it) += it->cmp_it.nchars;
	  IT_BYTEPOS (*it) += it->cmp_it.nbytes;
	  if (it->bidi_p)
	    {
	      if (it->bidi_it.new_paragraph)
		bidi_paragraph_init (it->paragraph_embedding, &it->bidi_it, 0);
	      /* Resync the bidi iterator with IT's new position.
		 FIXME: this doesn't support bidirectional text.  */
	      while (it->bidi_it.charpos < IT_CHARPOS (*it))
		bidi_move_to_visually_next (&it->bidi_it);
	    }
	  return 0;
	}
      it->position = it->current.pos;
      it->object = it->w->buffer;
      it->c = composition_update_it (&it->cmp_it, IT_CHARPOS (*it),
				     IT_BYTEPOS (*it), Qnil);
    }
  return 1;
}



/***********************************************************************
	     Moving an iterator without producing glyphs
 ***********************************************************************/

/* Check if iterator is at a position corresponding to a valid buffer
   position after some move_it_ call.  */

#define IT_POS_VALID_AFTER_MOVE_P(it)			\
  ((it)->method == GET_FROM_STRING			\
   ? IT_STRING_CHARPOS (*it) == 0			\
   : 1)


/* Move iterator IT to a specified buffer or X position within one
   line on the display without producing glyphs.

   OP should be a bit mask including some or all of these bits:
    MOVE_TO_X: Stop upon reaching x-position TO_X.
    MOVE_TO_POS: Stop upon reaching buffer or string position TO_CHARPOS.
   Regardless of OP's value, stop upon reaching the end of the display line.

   TO_X is normally a value 0 <= TO_X <= IT->last_visible_x.
   This means, in particular, that TO_X includes window's horizontal
   scroll amount.

   The return value has several possible values that
   say what condition caused the scan to stop:

   MOVE_POS_MATCH_OR_ZV
     - when TO_POS or ZV was reached.

   MOVE_X_REACHED
     -when TO_X was reached before TO_POS or ZV were reached.

   MOVE_LINE_CONTINUED
     - when we reached the end of the display area and the line must
     be continued.

   MOVE_LINE_TRUNCATED
     - when we reached the end of the display area and the line is
     truncated.

   MOVE_NEWLINE_OR_CR
     - when we stopped at a line end, i.e. a newline or a CR and selective
     display is on.  */

static enum move_it_result
move_it_in_display_line_to (struct it *it,
			    EMACS_INT to_charpos, int to_x,
			    enum move_operation_enum op)
{
  enum move_it_result result = MOVE_UNDEFINED;
  struct glyph_row *saved_glyph_row;
  struct it wrap_it, atpos_it, atx_it, ppos_it;
  void *wrap_data = NULL, *atpos_data = NULL, *atx_data = NULL;
  void *ppos_data = NULL;
  int may_wrap = 0;
  enum it_method prev_method = it->method;
  EMACS_INT prev_pos = IT_CHARPOS (*it);
  int saw_smaller_pos = prev_pos < to_charpos;

  /* Don't produce glyphs in produce_glyphs.  */
  saved_glyph_row = it->glyph_row;
  it->glyph_row = NULL;

  /* Use wrap_it to save a copy of IT wherever a word wrap could
     occur.  Use atpos_it to save a copy of IT at the desired buffer
     position, if found, so that we can scan ahead and check if the
     word later overshoots the window edge.  Use atx_it similarly, for
     pixel positions.  */
  wrap_it.sp = -1;
  atpos_it.sp = -1;
  atx_it.sp = -1;

  /* Use ppos_it under bidi reordering to save a copy of IT for the
     position > CHARPOS that is the closest to CHARPOS.  We restore
     that position in IT when we have scanned the entire display line
     without finding a match for CHARPOS and all the character
     positions are greater than CHARPOS.  */
  if (it->bidi_p)
    {
      SAVE_IT (ppos_it, *it, ppos_data);
      SET_TEXT_POS (ppos_it.current.pos, ZV, ZV_BYTE);
      if ((op & MOVE_TO_POS) && IT_CHARPOS (*it) >= to_charpos)
	SAVE_IT (ppos_it, *it, ppos_data);
    }

#define BUFFER_POS_REACHED_P()					\
  ((op & MOVE_TO_POS) != 0					\
   && BUFFERP (it->object)					\
   && (IT_CHARPOS (*it) == to_charpos				\
       || ((!it->bidi_p						\
	    || BIDI_AT_BASE_LEVEL (it->bidi_it))		\
	   && IT_CHARPOS (*it) > to_charpos)			\
       || (it->what == IT_COMPOSITION				\
	   && ((IT_CHARPOS (*it) > to_charpos			\
		&& to_charpos >= it->cmp_it.charpos)		\
	       || (IT_CHARPOS (*it) < to_charpos		\
		   && to_charpos <= it->cmp_it.charpos))))	\
   && (it->method == GET_FROM_BUFFER				\
       || (it->method == GET_FROM_DISPLAY_VECTOR		\
	   && it->dpvec + it->current.dpvec_index + 1 >= it->dpend)))

  /* If there's a line-/wrap-prefix, handle it.  */
  if (it->hpos == 0 && it->method == GET_FROM_BUFFER
      && it->current_y < it->last_visible_y)
    handle_line_prefix (it);

  if (IT_CHARPOS (*it) < CHARPOS (this_line_min_pos))
    SET_TEXT_POS (this_line_min_pos, IT_CHARPOS (*it), IT_BYTEPOS (*it));

  while (1)
    {
      int x, i, ascent = 0, descent = 0;

/* Utility macro to reset an iterator with x, ascent, and descent.  */
#define IT_RESET_X_ASCENT_DESCENT(IT)			\
  ((IT)->current_x = x, (IT)->max_ascent = ascent,	\
   (IT)->max_descent = descent)

      /* Stop if we move beyond TO_CHARPOS (after an image or a
	 display string or stretch glyph).  */
      if ((op & MOVE_TO_POS) != 0
	  && BUFFERP (it->object)
	  && it->method == GET_FROM_BUFFER
	  && (((!it->bidi_p
		/* When the iterator is at base embedding level, we
		   are guaranteed that characters are delivered for
		   display in strictly increasing order of their
		   buffer positions.  */
		|| BIDI_AT_BASE_LEVEL (it->bidi_it))
	       && IT_CHARPOS (*it) > to_charpos)
	      || (it->bidi_p
		  && (prev_method == GET_FROM_IMAGE
		      || prev_method == GET_FROM_STRETCH
		      || prev_method == GET_FROM_STRING)
		  /* Passed TO_CHARPOS from left to right.  */
		  && ((prev_pos < to_charpos
		       && IT_CHARPOS (*it) > to_charpos)
		      /* Passed TO_CHARPOS from right to left.  */
		      || (prev_pos > to_charpos
			  && IT_CHARPOS (*it) < to_charpos)))))
	{
	  if (it->line_wrap != WORD_WRAP || wrap_it.sp < 0)
	    {
	      result = MOVE_POS_MATCH_OR_ZV;
	      break;
	    }
	  else if (it->line_wrap == WORD_WRAP && atpos_it.sp < 0)
	    /* If wrap_it is valid, the current position might be in a
	       word that is wrapped.  So, save the iterator in
	       atpos_it and continue to see if wrapping happens.  */
	    SAVE_IT (atpos_it, *it, atpos_data);
	}

      /* Stop when ZV reached.
         We used to stop here when TO_CHARPOS reached as well, but that is
         too soon if this glyph does not fit on this line.  So we handle it
         explicitly below.  */
      if (!get_next_display_element (it))
	{
	  result = MOVE_POS_MATCH_OR_ZV;
	  break;
	}

      if (it->line_wrap == TRUNCATE)
	{
	  if (BUFFER_POS_REACHED_P ())
	    {
	      result = MOVE_POS_MATCH_OR_ZV;
	      break;
	    }
	}
      else
	{
	  if (it->line_wrap == WORD_WRAP)
	    {
	      if (IT_DISPLAYING_WHITESPACE (it))
		may_wrap = 1;
	      else if (may_wrap)
		{
		  /* We have reached a glyph that follows one or more
		     whitespace characters.  If the position is
		     already found, we are done.  */
		  if (atpos_it.sp >= 0)
		    {
		      RESTORE_IT (it, &atpos_it, atpos_data);
		      result = MOVE_POS_MATCH_OR_ZV;
		      goto done;
		    }
		  if (atx_it.sp >= 0)
		    {
		      RESTORE_IT (it, &atx_it, atx_data);
		      result = MOVE_X_REACHED;
		      goto done;
		    }
		  /* Otherwise, we can wrap here.  */
		  SAVE_IT (wrap_it, *it, wrap_data);
		  may_wrap = 0;
		}
	    }
	}

      /* Remember the line height for the current line, in case
	 the next element doesn't fit on the line.  */
      ascent = it->max_ascent;
      descent = it->max_descent;

      /* The call to produce_glyphs will get the metrics of the
	 display element IT is loaded with.  Record the x-position
	 before this display element, in case it doesn't fit on the
	 line.  */
      x = it->current_x;

      PRODUCE_GLYPHS (it);

      if (it->area != TEXT_AREA)
	{
	  prev_method = it->method;
	  if (it->method == GET_FROM_BUFFER)
	    prev_pos = IT_CHARPOS (*it);
	  set_iterator_to_next (it, 1);
	  if (IT_CHARPOS (*it) < CHARPOS (this_line_min_pos))
	    SET_TEXT_POS (this_line_min_pos,
			  IT_CHARPOS (*it), IT_BYTEPOS (*it));
	  if (it->bidi_p
	      && (op & MOVE_TO_POS)
	      && IT_CHARPOS (*it) > to_charpos
	      && IT_CHARPOS (*it) < IT_CHARPOS (ppos_it))
	    SAVE_IT (ppos_it, *it, ppos_data);
	  continue;
	}

      /* The number of glyphs we get back in IT->nglyphs will normally
	 be 1 except when IT->c is (i) a TAB, or (ii) a multi-glyph
	 character on a terminal frame, or (iii) a line end.  For the
	 second case, IT->nglyphs - 1 padding glyphs will be present.
	 (On X frames, there is only one glyph produced for a
	 composite character.)

	 The behavior implemented below means, for continuation lines,
	 that as many spaces of a TAB as fit on the current line are
	 displayed there.  For terminal frames, as many glyphs of a
	 multi-glyph character are displayed in the current line, too.
	 This is what the old redisplay code did, and we keep it that
	 way.  Under X, the whole shape of a complex character must
	 fit on the line or it will be completely displayed in the
	 next line.

	 Note that both for tabs and padding glyphs, all glyphs have
	 the same width.  */
      if (it->nglyphs)
	{
	  /* More than one glyph or glyph doesn't fit on line.  All
	     glyphs have the same width.  */
	  int single_glyph_width = it->pixel_width / it->nglyphs;
	  int new_x;
	  int x_before_this_char = x;
	  int hpos_before_this_char = it->hpos;

	  for (i = 0; i < it->nglyphs; ++i, x = new_x)
	    {
	      new_x = x + single_glyph_width;

	      /* We want to leave anything reaching TO_X to the caller.  */
	      if ((op & MOVE_TO_X) && new_x > to_x)
		{
		  if (BUFFER_POS_REACHED_P ())
		    {
		      if (it->line_wrap != WORD_WRAP || wrap_it.sp < 0)
			goto buffer_pos_reached;
		      if (atpos_it.sp < 0)
			{
			  SAVE_IT (atpos_it, *it, atpos_data);
			  IT_RESET_X_ASCENT_DESCENT (&atpos_it);
			}
		    }
		  else
		    {
		      if (it->line_wrap != WORD_WRAP || wrap_it.sp < 0)
			{
			  it->current_x = x;
			  result = MOVE_X_REACHED;
			  break;
			}
		      if (atx_it.sp < 0)
			{
			  SAVE_IT (atx_it, *it, atx_data);
			  IT_RESET_X_ASCENT_DESCENT (&atx_it);
			}
		    }
		}

	      if (/* Lines are continued.  */
		  it->line_wrap != TRUNCATE
		  && (/* And glyph doesn't fit on the line.  */
		      new_x > it->last_visible_x
		      /* Or it fits exactly and we're on a window
			 system frame.  */
		      || (new_x == it->last_visible_x
			  && FRAME_WINDOW_P (it->f))))
		{
		  if (/* IT->hpos == 0 means the very first glyph
			 doesn't fit on the line, e.g. a wide image.  */
		      it->hpos == 0
		      || (new_x == it->last_visible_x
			  && FRAME_WINDOW_P (it->f)))
		    {
		      ++it->hpos;
		      it->current_x = new_x;

		      /* The character's last glyph just barely fits
			 in this row.  */
		      if (i == it->nglyphs - 1)
			{
			  /* If this is the destination position,
			     return a position *before* it in this row,
			     now that we know it fits in this row.  */
			  if (BUFFER_POS_REACHED_P ())
			    {
			      if (it->line_wrap != WORD_WRAP
				  || wrap_it.sp < 0)
				{
				  it->hpos = hpos_before_this_char;
				  it->current_x = x_before_this_char;
				  result = MOVE_POS_MATCH_OR_ZV;
				  break;
				}
			      if (it->line_wrap == WORD_WRAP
				  && atpos_it.sp < 0)
				{
				  SAVE_IT (atpos_it, *it, atpos_data);
				  atpos_it.current_x = x_before_this_char;
				  atpos_it.hpos = hpos_before_this_char;
				}
			    }

			  prev_method = it->method;
			  if (it->method == GET_FROM_BUFFER)
			    prev_pos = IT_CHARPOS (*it);
			  set_iterator_to_next (it, 1);
			  if (IT_CHARPOS (*it) < CHARPOS (this_line_min_pos))
			    SET_TEXT_POS (this_line_min_pos,
					  IT_CHARPOS (*it), IT_BYTEPOS (*it));
			  /* On graphical terminals, newlines may
			     "overflow" into the fringe if
			     overflow-newline-into-fringe is non-nil.
			     On text-only terminals, newlines may
			     overflow into the last glyph on the
			     display line.*/
			  if (!FRAME_WINDOW_P (it->f)
			      || IT_OVERFLOW_NEWLINE_INTO_FRINGE (it))
			    {
			      if (!get_next_display_element (it))
				{
				  result = MOVE_POS_MATCH_OR_ZV;
				  break;
				}
			      if (BUFFER_POS_REACHED_P ())
				{
				  if (ITERATOR_AT_END_OF_LINE_P (it))
				    result = MOVE_POS_MATCH_OR_ZV;
				  else
				    result = MOVE_LINE_CONTINUED;
				  break;
				}
			      if (ITERATOR_AT_END_OF_LINE_P (it))
				{
				  result = MOVE_NEWLINE_OR_CR;
				  break;
				}
			    }
			}
		    }
		  else
		    IT_RESET_X_ASCENT_DESCENT (it);

		  if (wrap_it.sp >= 0)
		    {
		      RESTORE_IT (it, &wrap_it, wrap_data);
		      atpos_it.sp = -1;
		      atx_it.sp = -1;
		    }

		  TRACE_MOVE ((stderr, "move_it_in: continued at %d\n",
			       IT_CHARPOS (*it)));
		  result = MOVE_LINE_CONTINUED;
		  break;
		}

	      if (BUFFER_POS_REACHED_P ())
		{
		  if (it->line_wrap != WORD_WRAP || wrap_it.sp < 0)
		    goto buffer_pos_reached;
		  if (it->line_wrap == WORD_WRAP && atpos_it.sp < 0)
		    {
		      SAVE_IT (atpos_it, *it, atpos_data);
		      IT_RESET_X_ASCENT_DESCENT (&atpos_it);
		    }
		}

	      if (new_x > it->first_visible_x)
		{
		  /* Glyph is visible.  Increment number of glyphs that
		     would be displayed.  */
		  ++it->hpos;
		}
	    }

	  if (result != MOVE_UNDEFINED)
	    break;
	}
      else if (BUFFER_POS_REACHED_P ())
	{
	buffer_pos_reached:
	  IT_RESET_X_ASCENT_DESCENT (it);
	  result = MOVE_POS_MATCH_OR_ZV;
	  break;
	}
      else if ((op & MOVE_TO_X) && it->current_x >= to_x)
	{
	  /* Stop when TO_X specified and reached.  This check is
	     necessary here because of lines consisting of a line end,
	     only.  The line end will not produce any glyphs and we
	     would never get MOVE_X_REACHED.  */
	  xassert (it->nglyphs == 0);
	  result = MOVE_X_REACHED;
	  break;
	}

      /* Is this a line end?  If yes, we're done.  */
      if (ITERATOR_AT_END_OF_LINE_P (it))
	{
	  /* If we are past TO_CHARPOS, but never saw any character
	     positions smaller than TO_CHARPOS, return
	     MOVE_POS_MATCH_OR_ZV, like the unidirectional display
	     did.  */
	  if (it->bidi_p && (op & MOVE_TO_POS) != 0)
	    {
	      if (!saw_smaller_pos && IT_CHARPOS (*it) > to_charpos)
		{
		  if (IT_CHARPOS (ppos_it) < ZV)
		    {
		      RESTORE_IT (it, &ppos_it, ppos_data);
		      result = MOVE_POS_MATCH_OR_ZV;
		    }
		  else
		    goto buffer_pos_reached;
		}
	      else if (it->line_wrap == WORD_WRAP && atpos_it.sp >= 0
		       && IT_CHARPOS (*it) > to_charpos)
		goto buffer_pos_reached;
	      else
		result = MOVE_NEWLINE_OR_CR;
	    }
	  else
	    result = MOVE_NEWLINE_OR_CR;
	  break;
	}

      prev_method = it->method;
      if (it->method == GET_FROM_BUFFER)
	prev_pos = IT_CHARPOS (*it);
      /* The current display element has been consumed.  Advance
	 to the next.  */
      set_iterator_to_next (it, 1);
      if (IT_CHARPOS (*it) < CHARPOS (this_line_min_pos))
	SET_TEXT_POS (this_line_min_pos, IT_CHARPOS (*it), IT_BYTEPOS (*it));
      if (IT_CHARPOS (*it) < to_charpos)
	saw_smaller_pos = 1;
      if (it->bidi_p
	  && (op & MOVE_TO_POS)
	  && IT_CHARPOS (*it) >= to_charpos
	  && IT_CHARPOS (*it) < IT_CHARPOS (ppos_it))
	SAVE_IT (ppos_it, *it, ppos_data);

      /* Stop if lines are truncated and IT's current x-position is
	 past the right edge of the window now.  */
      if (it->line_wrap == TRUNCATE
	  && it->current_x >= it->last_visible_x)
	{
	  if (!FRAME_WINDOW_P (it->f)
	      || IT_OVERFLOW_NEWLINE_INTO_FRINGE (it))
	    {
	      int at_eob_p = 0;

	      if ((at_eob_p = !get_next_display_element (it))
		  || BUFFER_POS_REACHED_P ()
		  /* If we are past TO_CHARPOS, but never saw any
		     character positions smaller than TO_CHARPOS,
		     return MOVE_POS_MATCH_OR_ZV, like the
		     unidirectional display did.  */
		  || (it->bidi_p && (op & MOVE_TO_POS) != 0
		      && !saw_smaller_pos
		      && IT_CHARPOS (*it) > to_charpos))
		{
		  if (it->bidi_p
		      && !at_eob_p && IT_CHARPOS (ppos_it) < ZV)
		    RESTORE_IT (it, &ppos_it, ppos_data);
		  result = MOVE_POS_MATCH_OR_ZV;
		  break;
		}
	      if (ITERATOR_AT_END_OF_LINE_P (it))
		{
		  result = MOVE_NEWLINE_OR_CR;
		  break;
		}
	    }
	  else if (it->bidi_p && (op & MOVE_TO_POS) != 0
		   && !saw_smaller_pos
		   && IT_CHARPOS (*it) > to_charpos)
	    {
	      if (IT_CHARPOS (ppos_it) < ZV)
		RESTORE_IT (it, &ppos_it, ppos_data);
	      result = MOVE_POS_MATCH_OR_ZV;
	      break;
	    }
	  result = MOVE_LINE_TRUNCATED;
	  break;
	}
#undef IT_RESET_X_ASCENT_DESCENT
    }

#undef BUFFER_POS_REACHED_P

  /* If we scanned beyond to_pos and didn't find a point to wrap at,
     restore the saved iterator.  */
  if (atpos_it.sp >= 0)
    RESTORE_IT (it, &atpos_it, atpos_data);
  else if (atx_it.sp >= 0)
    RESTORE_IT (it, &atx_it, atx_data);

 done:

  if (atpos_data)
    bidi_unshelve_cache (atpos_data, 1);
  if (atx_data)
    bidi_unshelve_cache (atx_data, 1);
  if (wrap_data)
    bidi_unshelve_cache (wrap_data, 1);
  if (ppos_data)
    bidi_unshelve_cache (ppos_data, 1);

  /* Restore the iterator settings altered at the beginning of this
     function.  */
  it->glyph_row = saved_glyph_row;
  return result;
}

/* For external use.  */
void
move_it_in_display_line (struct it *it,
			 EMACS_INT to_charpos, int to_x,
			 enum move_operation_enum op)
{
  if (it->line_wrap == WORD_WRAP
      && (op & MOVE_TO_X))
    {
      struct it save_it;
      void *save_data = NULL;
      int skip;

      SAVE_IT (save_it, *it, save_data);
      skip = move_it_in_display_line_to (it, to_charpos, to_x, op);
      /* When word-wrap is on, TO_X may lie past the end
	 of a wrapped line.  Then it->current is the
	 character on the next line, so backtrack to the
	 space before the wrap point.  */
      if (skip == MOVE_LINE_CONTINUED)
	{
	  int prev_x = max (it->current_x - 1, 0);
	  RESTORE_IT (it, &save_it, save_data);
	  move_it_in_display_line_to
	    (it, -1, prev_x, MOVE_TO_X);
	}
      else
	bidi_unshelve_cache (save_data, 1);
    }
  else
    move_it_in_display_line_to (it, to_charpos, to_x, op);
}


/* Move IT forward until it satisfies one or more of the criteria in
   TO_CHARPOS, TO_X, TO_Y, and TO_VPOS.

   OP is a bit-mask that specifies where to stop, and in particular,
   which of those four position arguments makes a difference.  See the
   description of enum move_operation_enum.

   If TO_CHARPOS is in invisible text, e.g. a truncated part of a
   screen line, this function will set IT to the next position that is
   displayed to the right of TO_CHARPOS on the screen.  */

void
move_it_to (struct it *it, EMACS_INT to_charpos, int to_x, int to_y, int to_vpos, int op)
{
  enum move_it_result skip, skip2 = MOVE_X_REACHED;
  int line_height, line_start_x = 0, reached = 0;
  void *backup_data = NULL;

  for (;;)
    {
      if (op & MOVE_TO_VPOS)
	{
	  /* If no TO_CHARPOS and no TO_X specified, stop at the
	     start of the line TO_VPOS.  */
	  if ((op & (MOVE_TO_X | MOVE_TO_POS)) == 0)
	    {
	      if (it->vpos == to_vpos)
		{
		  reached = 1;
		  break;
		}
	      else
		skip = move_it_in_display_line_to (it, -1, -1, 0);
	    }
	  else
	    {
	      /* TO_VPOS >= 0 means stop at TO_X in the line at
		 TO_VPOS, or at TO_POS, whichever comes first.  */
	      if (it->vpos == to_vpos)
		{
		  reached = 2;
		  break;
		}

	      skip = move_it_in_display_line_to (it, to_charpos, to_x, op);

	      if (skip == MOVE_POS_MATCH_OR_ZV || it->vpos == to_vpos)
		{
		  reached = 3;
		  break;
		}
	      else if (skip == MOVE_X_REACHED && it->vpos != to_vpos)
		{
		  /* We have reached TO_X but not in the line we want.  */
		  skip = move_it_in_display_line_to (it, to_charpos,
						     -1, MOVE_TO_POS);
		  if (skip == MOVE_POS_MATCH_OR_ZV)
		    {
		      reached = 4;
		      break;
		    }
		}
	    }
	}
      else if (op & MOVE_TO_Y)
	{
	  struct it it_backup;

	  if (it->line_wrap == WORD_WRAP)
	    SAVE_IT (it_backup, *it, backup_data);

	  /* TO_Y specified means stop at TO_X in the line containing
	     TO_Y---or at TO_CHARPOS if this is reached first.  The
	     problem is that we can't really tell whether the line
	     contains TO_Y before we have completely scanned it, and
	     this may skip past TO_X.  What we do is to first scan to
	     TO_X.

	     If TO_X is not specified, use a TO_X of zero.  The reason
	     is to make the outcome of this function more predictable.
	     If we didn't use TO_X == 0, we would stop at the end of
	     the line which is probably not what a caller would expect
	     to happen.  */
	  skip = move_it_in_display_line_to
	    (it, to_charpos, ((op & MOVE_TO_X) ? to_x : 0),
	     (MOVE_TO_X | (op & MOVE_TO_POS)));

	  /* If TO_CHARPOS is reached or ZV, we don't have to do more.  */
	  if (skip == MOVE_POS_MATCH_OR_ZV)
	    reached = 5;
	  else if (skip == MOVE_X_REACHED)
	    {
	      /* If TO_X was reached, we want to know whether TO_Y is
		 in the line.  We know this is the case if the already
		 scanned glyphs make the line tall enough.  Otherwise,
		 we must check by scanning the rest of the line.  */
	      line_height = it->max_ascent + it->max_descent;
	      if (to_y >= it->current_y
		  && to_y < it->current_y + line_height)
		{
		  reached = 6;
		  break;
		}
	      SAVE_IT (it_backup, *it, backup_data);
	      TRACE_MOVE ((stderr, "move_it: from %d\n", IT_CHARPOS (*it)));
	      skip2 = move_it_in_display_line_to (it, to_charpos, -1,
						  op & MOVE_TO_POS);
	      TRACE_MOVE ((stderr, "move_it: to %d\n", IT_CHARPOS (*it)));
	      line_height = it->max_ascent + it->max_descent;
	      TRACE_MOVE ((stderr, "move_it: line_height = %d\n", line_height));

	      if (to_y >= it->current_y
		  && to_y < it->current_y + line_height)
		{
		  /* If TO_Y is in this line and TO_X was reached
		     above, we scanned too far.  We have to restore
		     IT's settings to the ones before skipping.  But
		     keep the more accurate values of max_ascent and
		     max_descent we've found while skipping the rest
		     of the line, for the sake of callers, such as
		     pos_visible_p, that need to know the line
		     height.  */
		  int max_ascent = it->max_ascent;
		  int max_descent = it->max_descent;

		  RESTORE_IT (it, &it_backup, backup_data);
		  it->max_ascent = max_ascent;
		  it->max_descent = max_descent;
		  reached = 6;
		}
	      else
		{
		  skip = skip2;
		  if (skip == MOVE_POS_MATCH_OR_ZV)
		    reached = 7;
		}
	    }
	  else
	    {
	      /* Check whether TO_Y is in this line.  */
	      line_height = it->max_ascent + it->max_descent;
	      TRACE_MOVE ((stderr, "move_it: line_height = %d\n", line_height));

	      if (to_y >= it->current_y
		  && to_y < it->current_y + line_height)
		{
		  /* When word-wrap is on, TO_X may lie past the end
		     of a wrapped line.  Then it->current is the
		     character on the next line, so backtrack to the
		     space before the wrap point.  */
		  if (skip == MOVE_LINE_CONTINUED
		      && it->line_wrap == WORD_WRAP)
		    {
		      int prev_x = max (it->current_x - 1, 0);
		      RESTORE_IT (it, &it_backup, backup_data);
		      skip = move_it_in_display_line_to
			(it, -1, prev_x, MOVE_TO_X);
		    }
		  reached = 6;
		}
	    }

	  if (reached)
	    break;
	}
      else if (BUFFERP (it->object)
	       && (it->method == GET_FROM_BUFFER
		   || it->method == GET_FROM_STRETCH)
	       && IT_CHARPOS (*it) >= to_charpos
	       /* Under bidi iteration, a call to set_iterator_to_next
		  can scan far beyond to_charpos if the initial
		  portion of the next line needs to be reordered.  In
		  that case, give move_it_in_display_line_to another
		  chance below.  */
	       && !(it->bidi_p
		    && it->bidi_it.scan_dir == -1))
	skip = MOVE_POS_MATCH_OR_ZV;
      else
	skip = move_it_in_display_line_to (it, to_charpos, -1, MOVE_TO_POS);

      switch (skip)
	{
	case MOVE_POS_MATCH_OR_ZV:
	  reached = 8;
	  goto out;

	case MOVE_NEWLINE_OR_CR:
	  set_iterator_to_next (it, 1);
	  it->continuation_lines_width = 0;
	  break;

	case MOVE_LINE_TRUNCATED:
	  it->continuation_lines_width = 0;
	  reseat_at_next_visible_line_start (it, 0);
	  if ((op & MOVE_TO_POS) != 0
	      && IT_CHARPOS (*it) > to_charpos)
	    {
	      reached = 9;
	      goto out;
	    }
	  break;

	case MOVE_LINE_CONTINUED:
	  /* For continued lines ending in a tab, some of the glyphs
	     associated with the tab are displayed on the current
	     line.  Since it->current_x does not include these glyphs,
	     we use it->last_visible_x instead.  */
	  if (it->c == '\t')
	    {
	      it->continuation_lines_width += it->last_visible_x;
	      /* When moving by vpos, ensure that the iterator really
		 advances to the next line (bug#847, bug#969).  Fixme:
		 do we need to do this in other circumstances?  */
	      if (it->current_x != it->last_visible_x
		  && (op & MOVE_TO_VPOS)
	      	  && !(op & (MOVE_TO_X | MOVE_TO_POS)))
		{
		  line_start_x = it->current_x + it->pixel_width
		    - it->last_visible_x;
		  set_iterator_to_next (it, 0);
		}
	    }
	  else
	    it->continuation_lines_width += it->current_x;
	  break;

	default:
	  abort ();
	}

      /* Reset/increment for the next run.  */
      recenter_overlay_lists (current_buffer, IT_CHARPOS (*it));
      it->current_x = line_start_x;
      line_start_x = 0;
      it->hpos = 0;
      it->current_y += it->max_ascent + it->max_descent;
      ++it->vpos;
      last_height = it->max_ascent + it->max_descent;
      last_max_ascent = it->max_ascent;
      it->max_ascent = it->max_descent = 0;
    }

 out:

  /* On text terminals, we may stop at the end of a line in the middle
     of a multi-character glyph.  If the glyph itself is continued,
     i.e. it is actually displayed on the next line, don't treat this
     stopping point as valid; move to the next line instead (unless
     that brings us offscreen).  */
  if (!FRAME_WINDOW_P (it->f)
      && op & MOVE_TO_POS
      && IT_CHARPOS (*it) == to_charpos
      && it->what == IT_CHARACTER
      && it->nglyphs > 1
      && it->line_wrap == WINDOW_WRAP
      && it->current_x == it->last_visible_x - 1
      && it->c != '\n'
      && it->c != '\t'
      && it->vpos < XFASTINT (it->w->window_end_vpos))
    {
      it->continuation_lines_width += it->current_x;
      it->current_x = it->hpos = it->max_ascent = it->max_descent = 0;
      it->current_y += it->max_ascent + it->max_descent;
      ++it->vpos;
      last_height = it->max_ascent + it->max_descent;
      last_max_ascent = it->max_ascent;
    }

  if (backup_data)
    bidi_unshelve_cache (backup_data, 1);

  TRACE_MOVE ((stderr, "move_it_to: reached %d\n", reached));
}


/* Move iterator IT backward by a specified y-distance DY, DY >= 0.

   If DY > 0, move IT backward at least that many pixels.  DY = 0
   means move IT backward to the preceding line start or BEGV.  This
   function may move over more than DY pixels if IT->current_y - DY
   ends up in the middle of a line; in this case IT->current_y will be
   set to the top of the line moved to.  */

void
move_it_vertically_backward (struct it *it, int dy)
{
  int nlines, h;
  struct it it2, it3;
  void *it2data = NULL, *it3data = NULL;
  EMACS_INT start_pos;

 move_further_back:
  xassert (dy >= 0);

  start_pos = IT_CHARPOS (*it);

  /* Estimate how many newlines we must move back.  */
  nlines = max (1, dy / FRAME_LINE_HEIGHT (it->f));

  /* Set the iterator's position that many lines back.  */
  while (nlines-- && IT_CHARPOS (*it) > BEGV)
    back_to_previous_visible_line_start (it);

  /* Reseat the iterator here.  When moving backward, we don't want
     reseat to skip forward over invisible text, set up the iterator
     to deliver from overlay strings at the new position etc.  So,
     use reseat_1 here.  */
  reseat_1 (it, it->current.pos, 1);

  /* We are now surely at a line start.  */
  it->current_x = it->hpos = 0;	/* FIXME: this is incorrect when bidi
				   reordering is in effect.  */
  it->continuation_lines_width = 0;

  /* Move forward and see what y-distance we moved.  First move to the
     start of the next line so that we get its height.  We need this
     height to be able to tell whether we reached the specified
     y-distance.  */
  SAVE_IT (it2, *it, it2data);
  it2.max_ascent = it2.max_descent = 0;
  do
    {
      move_it_to (&it2, start_pos, -1, -1, it2.vpos + 1,
		  MOVE_TO_POS | MOVE_TO_VPOS);
    }
  while (!(IT_POS_VALID_AFTER_MOVE_P (&it2)
	   /* If we are in a display string which starts at START_POS,
	      and that display string includes a newline, and we are
	      right after that newline (i.e. at the beginning of a
	      display line), exit the loop, because otherwise we will
	      infloop, since move_it_to will see that it is already at
	      START_POS and will not move.  */
	   || (it2.method == GET_FROM_STRING
	       && IT_CHARPOS (it2) == start_pos
	       && SREF (it2.string, IT_STRING_BYTEPOS (it2) - 1) == '\n')));
  xassert (IT_CHARPOS (*it) >= BEGV);
  SAVE_IT (it3, it2, it3data);

  move_it_to (&it2, start_pos, -1, -1, -1, MOVE_TO_POS);
  xassert (IT_CHARPOS (*it) >= BEGV);
  /* H is the actual vertical distance from the position in *IT
     and the starting position.  */
  h = it2.current_y - it->current_y;
  /* NLINES is the distance in number of lines.  */
  nlines = it2.vpos - it->vpos;

  /* Correct IT's y and vpos position
     so that they are relative to the starting point.  */
  it->vpos -= nlines;
  it->current_y -= h;

  if (dy == 0)
    {
      /* DY == 0 means move to the start of the screen line.  The
	 value of nlines is > 0 if continuation lines were involved,
	 or if the original IT position was at start of a line.  */
      RESTORE_IT (it, it, it2data);
      if (nlines > 0)
	move_it_by_lines (it, nlines);
      /* The above code moves us to some position NLINES down,
	 usually to its first glyph (leftmost in an L2R line), but
	 that's not necessarily the start of the line, under bidi
	 reordering.  We want to get to the character position
	 that is immediately after the newline of the previous
	 line.  */
      if (it->bidi_p
	  && !it->continuation_lines_width
	  && !STRINGP (it->string)
	  && IT_CHARPOS (*it) > BEGV
	  && FETCH_BYTE (IT_BYTEPOS (*it) - 1) != '\n')
	{
	  EMACS_INT nl_pos =
	    find_next_newline_no_quit (IT_CHARPOS (*it) - 1, -1);

	  move_it_to (it, nl_pos, -1, -1, -1, MOVE_TO_POS);
	}
      bidi_unshelve_cache (it3data, 1);
    }
  else
    {
      /* The y-position we try to reach, relative to *IT.
	 Note that H has been subtracted in front of the if-statement.  */
      int target_y = it->current_y + h - dy;
      int y0 = it3.current_y;
      int y1;
      int line_height;

      RESTORE_IT (&it3, &it3, it3data);
      y1 = line_bottom_y (&it3);
      line_height = y1 - y0;
      RESTORE_IT (it, it, it2data);
      /* If we did not reach target_y, try to move further backward if
	 we can.  If we moved too far backward, try to move forward.  */
      if (target_y < it->current_y
	  /* This is heuristic.  In a window that's 3 lines high, with
	     a line height of 13 pixels each, recentering with point
	     on the bottom line will try to move -39/2 = 19 pixels
	     backward.  Try to avoid moving into the first line.  */
	  && (it->current_y - target_y
	      > min (window_box_height (it->w), line_height * 2 / 3))
	  && IT_CHARPOS (*it) > BEGV)
	{
	  TRACE_MOVE ((stderr, "  not far enough -> move_vert %d\n",
		       target_y - it->current_y));
	  dy = it->current_y - target_y;
	  goto move_further_back;
	}
      else if (target_y >= it->current_y + line_height
	       && IT_CHARPOS (*it) < ZV)
	{
	  /* Should move forward by at least one line, maybe more.

	     Note: Calling move_it_by_lines can be expensive on
	     terminal frames, where compute_motion is used (via
	     vmotion) to do the job, when there are very long lines
	     and truncate-lines is nil.  That's the reason for
	     treating terminal frames specially here.  */

	  if (!FRAME_WINDOW_P (it->f))
	    move_it_vertically (it, target_y - (it->current_y + line_height));
	  else
	    {
	      do
		{
		  move_it_by_lines (it, 1);
		}
	      while (target_y >= line_bottom_y (it) && IT_CHARPOS (*it) < ZV);
	    }
	}
    }
}


/* Move IT by a specified amount of pixel lines DY.  DY negative means
   move backwards.  DY = 0 means move to start of screen line.  At the
   end, IT will be on the start of a screen line.  */

void
move_it_vertically (struct it *it, int dy)
{
  if (dy <= 0)
    move_it_vertically_backward (it, -dy);
  else
    {
      TRACE_MOVE ((stderr, "move_it_v: from %d, %d\n", IT_CHARPOS (*it), dy));
      move_it_to (it, ZV, -1, it->current_y + dy, -1,
		  MOVE_TO_POS | MOVE_TO_Y);
      TRACE_MOVE ((stderr, "move_it_v: to %d\n", IT_CHARPOS (*it)));

      /* If buffer ends in ZV without a newline, move to the start of
	 the line to satisfy the post-condition.  */
      if (IT_CHARPOS (*it) == ZV
	  && ZV > BEGV
	  && FETCH_BYTE (IT_BYTEPOS (*it) - 1) != '\n')
	move_it_by_lines (it, 0);
    }
}


/* Move iterator IT past the end of the text line it is in.  */

void
move_it_past_eol (struct it *it)
{
  enum move_it_result rc;

  rc = move_it_in_display_line_to (it, Z, 0, MOVE_TO_POS);
  if (rc == MOVE_NEWLINE_OR_CR)
    set_iterator_to_next (it, 0);
}


/* Move IT by a specified number DVPOS of screen lines down.  DVPOS
   negative means move up.  DVPOS == 0 means move to the start of the
   screen line.

   Optimization idea: If we would know that IT->f doesn't use
   a face with proportional font, we could be faster for
   truncate-lines nil.  */

void
move_it_by_lines (struct it *it, int dvpos)
{

  /* The commented-out optimization uses vmotion on terminals.  This
     gives bad results, because elements like it->what, on which
     callers such as pos_visible_p rely, aren't updated. */
  /* struct position pos;
    if (!FRAME_WINDOW_P (it->f))
    {
      struct text_pos textpos;

      pos = *vmotion (IT_CHARPOS (*it), dvpos, it->w);
      SET_TEXT_POS (textpos, pos.bufpos, pos.bytepos);
      reseat (it, textpos, 1);
      it->vpos += pos.vpos;
      it->current_y += pos.vpos;
    }
    else */

  if (dvpos == 0)
    {
      /* DVPOS == 0 means move to the start of the screen line.  */
      move_it_vertically_backward (it, 0);
      /* Let next call to line_bottom_y calculate real line height */
      last_height = 0;
    }
  else if (dvpos > 0)
    {
      move_it_to (it, -1, -1, -1, it->vpos + dvpos, MOVE_TO_VPOS);
      if (!IT_POS_VALID_AFTER_MOVE_P (it))
	{
	  /* Only move to the next buffer position if we ended up in a
	     string from display property, not in an overlay string
	     (before-string or after-string).  That is because the
	     latter don't conceal the underlying buffer position, so
	     we can ask to move the iterator to the exact position we
	     are interested in.  Note that, even if we are already at
	     IT_CHARPOS (*it), the call below is not a no-op, as it
	     will detect that we are at the end of the string, pop the
	     iterator, and compute it->current_x and it->hpos
	     correctly.  */
	  move_it_to (it, IT_CHARPOS (*it) + it->string_from_display_prop_p,
		      -1, -1, -1, MOVE_TO_POS);
	}
    }
  else
    {
      struct it it2;
      void *it2data = NULL;
      EMACS_INT start_charpos, i;

      /* Start at the beginning of the screen line containing IT's
	 position.  This may actually move vertically backwards,
         in case of overlays, so adjust dvpos accordingly.  */
      dvpos += it->vpos;
      move_it_vertically_backward (it, 0);
      dvpos -= it->vpos;

      /* Go back -DVPOS visible lines and reseat the iterator there.  */
      start_charpos = IT_CHARPOS (*it);
      for (i = -dvpos; i > 0 && IT_CHARPOS (*it) > BEGV; --i)
	back_to_previous_visible_line_start (it);
      reseat (it, it->current.pos, 1);

      /* Move further back if we end up in a string or an image.  */
      while (!IT_POS_VALID_AFTER_MOVE_P (it))
	{
	  /* First try to move to start of display line.  */
	  dvpos += it->vpos;
	  move_it_vertically_backward (it, 0);
	  dvpos -= it->vpos;
	  if (IT_POS_VALID_AFTER_MOVE_P (it))
	    break;
	  /* If start of line is still in string or image,
	     move further back.  */
	  back_to_previous_visible_line_start (it);
	  reseat (it, it->current.pos, 1);
	  dvpos--;
	}

      it->current_x = it->hpos = 0;

      /* Above call may have moved too far if continuation lines
	 are involved.  Scan forward and see if it did.  */
      SAVE_IT (it2, *it, it2data);
      it2.vpos = it2.current_y = 0;
      move_it_to (&it2, start_charpos, -1, -1, -1, MOVE_TO_POS);
      it->vpos -= it2.vpos;
      it->current_y -= it2.current_y;
      it->current_x = it->hpos = 0;

      /* If we moved too far back, move IT some lines forward.  */
      if (it2.vpos > -dvpos)
	{
	  int delta = it2.vpos + dvpos;

	  RESTORE_IT (&it2, &it2, it2data);
	  SAVE_IT (it2, *it, it2data);
	  move_it_to (it, -1, -1, -1, it->vpos + delta, MOVE_TO_VPOS);
	  /* Move back again if we got too far ahead.  */
	  if (IT_CHARPOS (*it) >= start_charpos)
	    RESTORE_IT (it, &it2, it2data);
	  else
	    bidi_unshelve_cache (it2data, 1);
	}
      else
	RESTORE_IT (it, it, it2data);
    }
}

/* Return 1 if IT points into the middle of a display vector.  */

int
in_display_vector_p (struct it *it)
{
  return (it->method == GET_FROM_DISPLAY_VECTOR
	  && it->current.dpvec_index > 0
	  && it->dpvec + it->current.dpvec_index != it->dpend);
}


/***********************************************************************
			       Messages
 ***********************************************************************/


/* Add a message with format string FORMAT and arguments ARG1 and ARG2
   to *Messages*.  */

void
add_to_log (const char *format, Lisp_Object arg1, Lisp_Object arg2)
{
  Lisp_Object args[3];
  Lisp_Object msg, fmt;
  char *buffer;
  EMACS_INT len;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  USE_SAFE_ALLOCA;

  /* Do nothing if called asynchronously.  Inserting text into
     a buffer may call after-change-functions and alike and
     that would means running Lisp asynchronously.  */
  if (handling_signal)
    return;

  fmt = msg = Qnil;
  GCPRO4 (fmt, msg, arg1, arg2);

  args[0] = fmt = build_string (format);
  args[1] = arg1;
  args[2] = arg2;
  msg = Fformat (3, args);

  len = SBYTES (msg) + 1;
  SAFE_ALLOCA (buffer, char *, len);
  memcpy (buffer, SDATA (msg), len);

  message_dolog (buffer, len - 1, 1, 0);
  SAFE_FREE ();

  UNGCPRO;
}


/* Output a newline in the *Messages* buffer if "needs" one.  */

void
message_log_maybe_newline (void)
{
  if (message_log_need_newline)
    message_dolog ("", 0, 1, 0);
}


/* Add a string M of length NBYTES to the message log, optionally
   terminated with a newline when NLFLAG is non-zero.  MULTIBYTE, if
   nonzero, means interpret the contents of M as multibyte.  This
   function calls low-level routines in order to bypass text property
   hooks, etc. which might not be safe to run.

   This may GC (insert may run before/after change hooks),
   so the buffer M must NOT point to a Lisp string.  */

void
message_dolog (const char *m, EMACS_INT nbytes, int nlflag, int multibyte)
{
  const unsigned char *msg = (const unsigned char *) m;

  if (!NILP (Vmemory_full))
    return;

  if (!NILP (Vmessage_log_max))
    {
      struct buffer *oldbuf;
      Lisp_Object oldpoint, oldbegv, oldzv;
      int old_windows_or_buffers_changed = windows_or_buffers_changed;
      EMACS_INT point_at_end = 0;
      EMACS_INT zv_at_end = 0;
      Lisp_Object old_deactivate_mark, tem;
      struct gcpro gcpro1;

      old_deactivate_mark = Vdeactivate_mark;
      oldbuf = current_buffer;
      Fset_buffer (Fget_buffer_create (Vmessages_buffer_name));
      BVAR (current_buffer, undo_list) = Qt;

      oldpoint = message_dolog_marker1;
      set_marker_restricted (oldpoint, make_number (PT), Qnil);
      oldbegv = message_dolog_marker2;
      set_marker_restricted (oldbegv, make_number (BEGV), Qnil);
      oldzv = message_dolog_marker3;
      set_marker_restricted (oldzv, make_number (ZV), Qnil);
      GCPRO1 (old_deactivate_mark);

      if (PT == Z)
	point_at_end = 1;
      if (ZV == Z)
	zv_at_end = 1;

      BEGV = BEG;
      BEGV_BYTE = BEG_BYTE;
      ZV = Z;
      ZV_BYTE = Z_BYTE;
      TEMP_SET_PT_BOTH (Z, Z_BYTE);

      /* Insert the string--maybe converting multibyte to single byte
	 or vice versa, so that all the text fits the buffer.  */
      if (multibyte
	  && NILP (BVAR (current_buffer, enable_multibyte_characters)))
	{
	  EMACS_INT i;
	  int c, char_bytes;
	  char work[1];

	  /* Convert a multibyte string to single-byte
	     for the *Message* buffer.  */
	  for (i = 0; i < nbytes; i += char_bytes)
	    {
	      c = string_char_and_length (msg + i, &char_bytes);
	      work[0] = (ASCII_CHAR_P (c)
			 ? c
			 : multibyte_char_to_unibyte (c));
	      insert_1_both (work, 1, 1, 1, 0, 0);
	    }
	}
      else if (! multibyte
	       && ! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	{
	  EMACS_INT i;
	  int c, char_bytes;
	  unsigned char str[MAX_MULTIBYTE_LENGTH];
	  /* Convert a single-byte string to multibyte
	     for the *Message* buffer.  */
	  for (i = 0; i < nbytes; i++)
	    {
	      c = msg[i];
	      MAKE_CHAR_MULTIBYTE (c);
	      char_bytes = CHAR_STRING (c, str);
	      insert_1_both ((char *) str, 1, char_bytes, 1, 0, 0);
	    }
	}
      else if (nbytes)
	insert_1 (m, nbytes, 1, 0, 0);

      if (nlflag)
	{
	  EMACS_INT this_bol, this_bol_byte, prev_bol, prev_bol_byte;
	  printmax_t dups;
	  insert_1 ("\n", 1, 1, 0, 0);

	  scan_newline (Z, Z_BYTE, BEG, BEG_BYTE, -2, 0);
	  this_bol = PT;
	  this_bol_byte = PT_BYTE;

	  /* See if this line duplicates the previous one.
	     If so, combine duplicates.  */
	  if (this_bol > BEG)
	    {
	      scan_newline (PT, PT_BYTE, BEG, BEG_BYTE, -2, 0);
	      prev_bol = PT;
	      prev_bol_byte = PT_BYTE;

	      dups = message_log_check_duplicate (prev_bol_byte,
                                                  this_bol_byte);
	      if (dups)
		{
		  del_range_both (prev_bol, prev_bol_byte,
				  this_bol, this_bol_byte, 0);
		  if (dups > 1)
		    {
		      char dupstr[sizeof " [ times]"
				  + INT_STRLEN_BOUND (printmax_t)];
		      int duplen;

		      /* If you change this format, don't forget to also
			 change message_log_check_duplicate.  */
		      sprintf (dupstr, " [%"pMd" times]", dups);
		      duplen = strlen (dupstr);
		      TEMP_SET_PT_BOTH (Z - 1, Z_BYTE - 1);
		      insert_1 (dupstr, duplen, 1, 0, 1);
		    }
		}
	    }

	  /* If we have more than the desired maximum number of lines
	     in the *Messages* buffer now, delete the oldest ones.
	     This is safe because we don't have undo in this buffer.  */

	  if (NATNUMP (Vmessage_log_max))
	    {
	      scan_newline (Z, Z_BYTE, BEG, BEG_BYTE,
			    -XFASTINT (Vmessage_log_max) - 1, 0);
	      del_range_both (BEG, BEG_BYTE, PT, PT_BYTE, 0);
	    }
	}
      BEGV = XMARKER (oldbegv)->charpos;
      BEGV_BYTE = marker_byte_position (oldbegv);

      if (zv_at_end)
	{
	  ZV = Z;
	  ZV_BYTE = Z_BYTE;
	}
      else
	{
	  ZV = XMARKER (oldzv)->charpos;
	  ZV_BYTE = marker_byte_position (oldzv);
	}

      if (point_at_end)
	TEMP_SET_PT_BOTH (Z, Z_BYTE);
      else
	/* We can't do Fgoto_char (oldpoint) because it will run some
           Lisp code.  */
	TEMP_SET_PT_BOTH (XMARKER (oldpoint)->charpos,
			  XMARKER (oldpoint)->bytepos);

      UNGCPRO;
      unchain_marker (XMARKER (oldpoint));
      unchain_marker (XMARKER (oldbegv));
      unchain_marker (XMARKER (oldzv));

      tem = Fget_buffer_window (Fcurrent_buffer (), Qt);
      set_buffer_internal (oldbuf);
      if (NILP (tem))
	windows_or_buffers_changed = old_windows_or_buffers_changed;
      message_log_need_newline = !nlflag;
      Vdeactivate_mark = old_deactivate_mark;
    }
}


/* We are at the end of the buffer after just having inserted a newline.
   (Note: We depend on the fact we won't be crossing the gap.)
   Check to see if the most recent message looks a lot like the previous one.
   Return 0 if different, 1 if the new one should just replace it, or a
   value N > 1 if we should also append " [N times]".  */

static intmax_t
message_log_check_duplicate (EMACS_INT prev_bol_byte, EMACS_INT this_bol_byte)
{
  EMACS_INT i;
  EMACS_INT len = Z_BYTE - 1 - this_bol_byte;
  int seen_dots = 0;
  unsigned char *p1 = BUF_BYTE_ADDRESS (current_buffer, prev_bol_byte);
  unsigned char *p2 = BUF_BYTE_ADDRESS (current_buffer, this_bol_byte);

  for (i = 0; i < len; i++)
    {
      if (i >= 3 && p1[i-3] == '.' && p1[i-2] == '.' && p1[i-1] == '.')
	seen_dots = 1;
      if (p1[i] != p2[i])
	return seen_dots;
    }
  p1 += len;
  if (*p1 == '\n')
    return 2;
  if (*p1++ == ' ' && *p1++ == '[')
    {
      char *pend;
      intmax_t n = strtoimax ((char *) p1, &pend, 10);
      if (0 < n && n < INTMAX_MAX && strncmp (pend, " times]\n", 8) == 0)
	return n+1;
    }
  return 0;
}


/* Display an echo area message M with a specified length of NBYTES
   bytes.  The string may include null characters.  If M is 0, clear
   out any existing message, and let the mini-buffer text show
   through.

   This may GC, so the buffer M must NOT point to a Lisp string.  */

void
message2 (const char *m, EMACS_INT nbytes, int multibyte)
{
  /* First flush out any partial line written with print.  */
  message_log_maybe_newline ();
  if (m)
    message_dolog (m, nbytes, 1, multibyte);
  message2_nolog (m, nbytes, multibyte);
}


/* The non-logging counterpart of message2.  */

void
message2_nolog (const char *m, EMACS_INT nbytes, int multibyte)
{
  struct frame *sf = SELECTED_FRAME ();
  message_enable_multibyte = multibyte;

  if (FRAME_INITIAL_P (sf))
    {
      if (noninteractive_need_newline)
	putc ('\n', stderr);
      noninteractive_need_newline = 0;
      if (m)
	fwrite (m, nbytes, 1, stderr);
      if (cursor_in_echo_area == 0)
	fprintf (stderr, "\n");
      fflush (stderr);
    }
  /* A null message buffer means that the frame hasn't really been
     initialized yet.  Error messages get reported properly by
     cmd_error, so this must be just an informative message; toss it.  */
  else if (INTERACTIVE
	   && sf->glyphs_initialized_p
	   && FRAME_MESSAGE_BUF (sf))
    {
      Lisp_Object mini_window;
      struct frame *f;

      /* Get the frame containing the mini-buffer
	 that the selected frame is using.  */
      mini_window = FRAME_MINIBUF_WINDOW (sf);
      f = XFRAME (WINDOW_FRAME (XWINDOW (mini_window)));

      FRAME_SAMPLE_VISIBILITY (f);
      if (FRAME_VISIBLE_P (sf)
	  && ! FRAME_VISIBLE_P (f))
	Fmake_frame_visible (WINDOW_FRAME (XWINDOW (mini_window)));

      if (m)
	{
	  set_message (m, Qnil, nbytes, multibyte);
	  if (minibuffer_auto_raise)
	    Fraise_frame  (WINDOW_FRAME (XWINDOW (mini_window)));
	}
      else
	clear_message (1, 1);

      do_pending_window_change (0);
      echo_area_display (1);
      do_pending_window_change (0);
      if (FRAME_TERMINAL (f)->frame_up_to_date_hook != 0 && ! gc_in_progress)
	(*FRAME_TERMINAL (f)->frame_up_to_date_hook) (f);
    }
}


/* Display an echo area message M with a specified length of NBYTES
   bytes.  The string may include null characters.  If M is not a
   string, clear out any existing message, and let the mini-buffer
   text show through.

   This function cancels echoing.  */

void
message3 (Lisp_Object m, EMACS_INT nbytes, int multibyte)
{
  struct gcpro gcpro1;

  GCPRO1 (m);
  clear_message (1,1);
  cancel_echoing ();

  /* First flush out any partial line written with print.  */
  message_log_maybe_newline ();
  if (STRINGP (m))
    {
      char *buffer;
      USE_SAFE_ALLOCA;

      SAFE_ALLOCA (buffer, char *, nbytes);
      memcpy (buffer, SDATA (m), nbytes);
      message_dolog (buffer, nbytes, 1, multibyte);
      SAFE_FREE ();
    }
  message3_nolog (m, nbytes, multibyte);

  UNGCPRO;
}


/* The non-logging version of message3.
   This does not cancel echoing, because it is used for echoing.
   Perhaps we need to make a separate function for echoing
   and make this cancel echoing.  */

void
message3_nolog (Lisp_Object m, EMACS_INT nbytes, int multibyte)
{
  struct frame *sf = SELECTED_FRAME ();
  message_enable_multibyte = multibyte;

  if (FRAME_INITIAL_P (sf))
    {
      if (noninteractive_need_newline)
	putc ('\n', stderr);
      noninteractive_need_newline = 0;
      if (STRINGP (m))
	fwrite (SDATA (m), nbytes, 1, stderr);
      if (cursor_in_echo_area == 0)
	fprintf (stderr, "\n");
      fflush (stderr);
    }
  /* A null message buffer means that the frame hasn't really been
     initialized yet.  Error messages get reported properly by
     cmd_error, so this must be just an informative message; toss it.  */
  else if (INTERACTIVE
	   && sf->glyphs_initialized_p
	   && FRAME_MESSAGE_BUF (sf))
    {
      Lisp_Object mini_window;
      Lisp_Object frame;
      struct frame *f;

      /* Get the frame containing the mini-buffer
	 that the selected frame is using.  */
      mini_window = FRAME_MINIBUF_WINDOW (sf);
      frame = XWINDOW (mini_window)->frame;
      f = XFRAME (frame);

      FRAME_SAMPLE_VISIBILITY (f);
      if (FRAME_VISIBLE_P (sf)
	  && !FRAME_VISIBLE_P (f))
	Fmake_frame_visible (frame);

      if (STRINGP (m) && SCHARS (m) > 0)
	{
	  set_message (NULL, m, nbytes, multibyte);
	  if (minibuffer_auto_raise)
	    Fraise_frame (frame);
	  /* Assume we are not echoing.
	     (If we are, echo_now will override this.)  */
	  echo_message_buffer = Qnil;
	}
      else
	clear_message (1, 1);

      do_pending_window_change (0);
      echo_area_display (1);
      do_pending_window_change (0);
      if (FRAME_TERMINAL (f)->frame_up_to_date_hook != 0 && ! gc_in_progress)
	(*FRAME_TERMINAL (f)->frame_up_to_date_hook) (f);
    }
}


/* Display a null-terminated echo area message M.  If M is 0, clear
   out any existing message, and let the mini-buffer text show through.

   The buffer M must continue to exist until after the echo area gets
   cleared or some other message gets displayed there.  Do not pass
   text that is stored in a Lisp string.  Do not pass text in a buffer
   that was alloca'd.  */

void
message1 (const char *m)
{
  message2 (m, (m ? strlen (m) : 0), 0);
}


/* The non-logging counterpart of message1.  */

void
message1_nolog (const char *m)
{
  message2_nolog (m, (m ? strlen (m) : 0), 0);
}

/* Display a message M which contains a single %s
   which gets replaced with STRING.  */

void
message_with_string (const char *m, Lisp_Object string, int log)
{
  CHECK_STRING (string);

  if (noninteractive)
    {
      if (m)
	{
	  if (noninteractive_need_newline)
	    putc ('\n', stderr);
	  noninteractive_need_newline = 0;
	  fprintf (stderr, m, SDATA (string));
	  if (!cursor_in_echo_area)
	    fprintf (stderr, "\n");
	  fflush (stderr);
	}
    }
  else if (INTERACTIVE)
    {
      /* The frame whose minibuffer we're going to display the message on.
	 It may be larger than the selected frame, so we need
	 to use its buffer, not the selected frame's buffer.  */
      Lisp_Object mini_window;
      struct frame *f, *sf = SELECTED_FRAME ();

      /* Get the frame containing the minibuffer
	 that the selected frame is using.  */
      mini_window = FRAME_MINIBUF_WINDOW (sf);
      f = XFRAME (WINDOW_FRAME (XWINDOW (mini_window)));

      /* A null message buffer means that the frame hasn't really been
	 initialized yet.  Error messages get reported properly by
	 cmd_error, so this must be just an informative message; toss it.  */
      if (FRAME_MESSAGE_BUF (f))
	{
	  Lisp_Object args[2], msg;
	  struct gcpro gcpro1, gcpro2;

	  args[0] = build_string (m);
	  args[1] = msg = string;
	  GCPRO2 (args[0], msg);
	  gcpro1.nvars = 2;

	  msg = Fformat (2, args);

	  if (log)
	    message3 (msg, SBYTES (msg), STRING_MULTIBYTE (msg));
	  else
	    message3_nolog (msg, SBYTES (msg), STRING_MULTIBYTE (msg));

	  UNGCPRO;

	  /* Print should start at the beginning of the message
	     buffer next time.  */
	  message_buf_print = 0;
	}
    }
}


/* Dump an informative message to the minibuf.  If M is 0, clear out
   any existing message, and let the mini-buffer text show through.  */

static void
vmessage (const char *m, va_list ap)
{
  if (noninteractive)
    {
      if (m)
	{
	  if (noninteractive_need_newline)
	    putc ('\n', stderr);
	  noninteractive_need_newline = 0;
	  vfprintf (stderr, m, ap);
	  if (cursor_in_echo_area == 0)
	    fprintf (stderr, "\n");
	  fflush (stderr);
	}
    }
  else if (INTERACTIVE)
    {
      /* The frame whose mini-buffer we're going to display the message
	 on.  It may be larger than the selected frame, so we need to
	 use its buffer, not the selected frame's buffer.  */
      Lisp_Object mini_window;
      struct frame *f, *sf = SELECTED_FRAME ();

      /* Get the frame containing the mini-buffer
	 that the selected frame is using.  */
      mini_window = FRAME_MINIBUF_WINDOW (sf);
      f = XFRAME (WINDOW_FRAME (XWINDOW (mini_window)));

      /* A null message buffer means that the frame hasn't really been
	 initialized yet.  Error messages get reported properly by
	 cmd_error, so this must be just an informative message; toss
	 it.  */
      if (FRAME_MESSAGE_BUF (f))
	{
	  if (m)
	    {
	      ptrdiff_t len;

	      len = doprnt (FRAME_MESSAGE_BUF (f),
			    FRAME_MESSAGE_BUF_SIZE (f), m, (char *)0, ap);

	      message2 (FRAME_MESSAGE_BUF (f), len, 0);
	    }
	  else
	    message1 (0);

	  /* Print should start at the beginning of the message
	     buffer next time.  */
	  message_buf_print = 0;
	}
    }
}

void
message (const char *m, ...)
{
  va_list ap;
  va_start (ap, m);
  vmessage (m, ap);
  va_end (ap);
}


#if 0
/* The non-logging version of message.  */

void
message_nolog (const char *m, ...)
{
  Lisp_Object old_log_max;
  va_list ap;
  va_start (ap, m);
  old_log_max = Vmessage_log_max;
  Vmessage_log_max = Qnil;
  vmessage (m, ap);
  Vmessage_log_max = old_log_max;
  va_end (ap);
}
#endif


/* Display the current message in the current mini-buffer.  This is
   only called from error handlers in process.c, and is not time
   critical.  */

void
update_echo_area (void)
{
  if (!NILP (echo_area_buffer[0]))
    {
      Lisp_Object string;
      string = Fcurrent_message ();
      message3 (string, SBYTES (string),
		!NILP (BVAR (current_buffer, enable_multibyte_characters)));
    }
}


/* Make sure echo area buffers in `echo_buffers' are live.
   If they aren't, make new ones.  */

static void
ensure_echo_area_buffers (void)
{
  int i;

  for (i = 0; i < 2; ++i)
    if (!BUFFERP (echo_buffer[i])
	|| NILP (BVAR (XBUFFER (echo_buffer[i]), name)))
      {
	char name[30];
	Lisp_Object old_buffer;
	int j;

	old_buffer = echo_buffer[i];
	sprintf (name, " *Echo Area %d*", i);
	echo_buffer[i] = Fget_buffer_create (build_string (name));
	BVAR (XBUFFER (echo_buffer[i]), truncate_lines) = Qnil;
	/* to force word wrap in echo area -
	   it was decided to postpone this*/
	/* XBUFFER (echo_buffer[i])->word_wrap = Qt; */

	for (j = 0; j < 2; ++j)
	  if (EQ (old_buffer, echo_area_buffer[j]))
	    echo_area_buffer[j] = echo_buffer[i];
      }
}


/* Call FN with args A1..A4 with either the current or last displayed
   echo_area_buffer as current buffer.

   WHICH zero means use the current message buffer
   echo_area_buffer[0].  If that is nil, choose a suitable buffer
   from echo_buffer[] and clear it.

   WHICH > 0 means use echo_area_buffer[1].  If that is nil, choose a
   suitable buffer from echo_buffer[] and clear it.

   If WHICH < 0, set echo_area_buffer[1] to echo_area_buffer[0], so
   that the current message becomes the last displayed one, make
   choose a suitable buffer for echo_area_buffer[0], and clear it.

   Value is what FN returns.  */

static int
with_echo_area_buffer (struct window *w, int which,
		       int (*fn) (EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT),
		       EMACS_INT a1, Lisp_Object a2, EMACS_INT a3, EMACS_INT a4)
{
  Lisp_Object buffer;
  int this_one, the_other, clear_buffer_p, rc;
  int count = SPECPDL_INDEX ();

  /* If buffers aren't live, make new ones.  */
  ensure_echo_area_buffers ();

  clear_buffer_p = 0;

  if (which == 0)
    this_one = 0, the_other = 1;
  else if (which > 0)
    this_one = 1, the_other = 0;
  else
    {
      this_one = 0, the_other = 1;
      clear_buffer_p = 1;

      /* We need a fresh one in case the current echo buffer equals
	 the one containing the last displayed echo area message.  */
      if (!NILP (echo_area_buffer[this_one])
	  && EQ (echo_area_buffer[this_one], echo_area_buffer[the_other]))
	echo_area_buffer[this_one] = Qnil;
    }

  /* Choose a suitable buffer from echo_buffer[] is we don't
     have one.  */
  if (NILP (echo_area_buffer[this_one]))
    {
      echo_area_buffer[this_one]
	= (EQ (echo_area_buffer[the_other], echo_buffer[this_one])
	   ? echo_buffer[the_other]
	   : echo_buffer[this_one]);
      clear_buffer_p = 1;
    }

  buffer = echo_area_buffer[this_one];

  /* Don't get confused by reusing the buffer used for echoing
     for a different purpose.  */
  if (echo_kboard == NULL && EQ (buffer, echo_message_buffer))
    cancel_echoing ();

  record_unwind_protect (unwind_with_echo_area_buffer,
			 with_echo_area_buffer_unwind_data (w));

  /* Make the echo area buffer current.  Note that for display
     purposes, it is not necessary that the displayed window's buffer
     == current_buffer, except for text property lookup.  So, let's
     only set that buffer temporarily here without doing a full
     Fset_window_buffer.  We must also change w->pointm, though,
     because otherwise an assertions in unshow_buffer fails, and Emacs
     aborts.  */
  set_buffer_internal_1 (XBUFFER (buffer));
  if (w)
    {
      w->buffer = buffer;
      set_marker_both (w->pointm, buffer, BEG, BEG_BYTE);
    }

  BVAR (current_buffer, undo_list) = Qt;
  BVAR (current_buffer, read_only) = Qnil;
  specbind (Qinhibit_read_only, Qt);
  specbind (Qinhibit_modification_hooks, Qt);

  if (clear_buffer_p && Z > BEG)
    del_range (BEG, Z);

  xassert (BEGV >= BEG);
  xassert (ZV <= Z && ZV >= BEGV);

  rc = fn (a1, a2, a3, a4);

  xassert (BEGV >= BEG);
  xassert (ZV <= Z && ZV >= BEGV);

  unbind_to (count, Qnil);
  return rc;
}


/* Save state that should be preserved around the call to the function
   FN called in with_echo_area_buffer.  */

static Lisp_Object
with_echo_area_buffer_unwind_data (struct window *w)
{
  int i = 0;
  Lisp_Object vector, tmp;

  /* Reduce consing by keeping one vector in
     Vwith_echo_area_save_vector.  */
  vector = Vwith_echo_area_save_vector;
  Vwith_echo_area_save_vector = Qnil;

  if (NILP (vector))
    vector = Fmake_vector (make_number (7), Qnil);

  XSETBUFFER (tmp, current_buffer); ASET (vector, i, tmp); ++i;
  ASET (vector, i, Vdeactivate_mark); ++i;
  ASET (vector, i, make_number (windows_or_buffers_changed)); ++i;

  if (w)
    {
      XSETWINDOW (tmp, w); ASET (vector, i, tmp); ++i;
      ASET (vector, i, w->buffer); ++i;
      ASET (vector, i, make_number (XMARKER (w->pointm)->charpos)); ++i;
      ASET (vector, i, make_number (XMARKER (w->pointm)->bytepos)); ++i;
    }
  else
    {
      int end = i + 4;
      for (; i < end; ++i)
	ASET (vector, i, Qnil);
    }

  xassert (i == ASIZE (vector));
  return vector;
}


/* Restore global state from VECTOR which was created by
   with_echo_area_buffer_unwind_data.  */

static Lisp_Object
unwind_with_echo_area_buffer (Lisp_Object vector)
{
  set_buffer_internal_1 (XBUFFER (AREF (vector, 0)));
  Vdeactivate_mark = AREF (vector, 1);
  windows_or_buffers_changed = XFASTINT (AREF (vector, 2));

  if (WINDOWP (AREF (vector, 3)))
    {
      struct window *w;
      Lisp_Object buffer, charpos, bytepos;

      w = XWINDOW (AREF (vector, 3));
      buffer = AREF (vector, 4);
      charpos = AREF (vector, 5);
      bytepos = AREF (vector, 6);

      w->buffer = buffer;
      set_marker_both (w->pointm, buffer,
		       XFASTINT (charpos), XFASTINT (bytepos));
    }

  Vwith_echo_area_save_vector = vector;
  return Qnil;
}


/* Set up the echo area for use by print functions.  MULTIBYTE_P
   non-zero means we will print multibyte.  */

void
setup_echo_area_for_printing (int multibyte_p)
{
  /* If we can't find an echo area any more, exit.  */
  if (! FRAME_LIVE_P (XFRAME (selected_frame)))
    Fkill_emacs (Qnil);

  ensure_echo_area_buffers ();

  if (!message_buf_print)
    {
      /* A message has been output since the last time we printed.
	 Choose a fresh echo area buffer.  */
      if (EQ (echo_area_buffer[1], echo_buffer[0]))
	echo_area_buffer[0] = echo_buffer[1];
      else
	echo_area_buffer[0] = echo_buffer[0];

      /* Switch to that buffer and clear it.  */
      set_buffer_internal (XBUFFER (echo_area_buffer[0]));
      BVAR (current_buffer, truncate_lines) = Qnil;

      if (Z > BEG)
	{
	  int count = SPECPDL_INDEX ();
	  specbind (Qinhibit_read_only, Qt);
	  /* Note that undo recording is always disabled.  */
	  del_range (BEG, Z);
	  unbind_to (count, Qnil);
	}
      TEMP_SET_PT_BOTH (BEG, BEG_BYTE);

      /* Set up the buffer for the multibyteness we need.  */
      if (multibyte_p
	  != !NILP (BVAR (current_buffer, enable_multibyte_characters)))
	Fset_buffer_multibyte (multibyte_p ? Qt : Qnil);

      /* Raise the frame containing the echo area.  */
      if (minibuffer_auto_raise)
	{
	  struct frame *sf = SELECTED_FRAME ();
	  Lisp_Object mini_window;
	  mini_window = FRAME_MINIBUF_WINDOW (sf);
	  Fraise_frame  (WINDOW_FRAME (XWINDOW (mini_window)));
	}

      message_log_maybe_newline ();
      message_buf_print = 1;
    }
  else
    {
      if (NILP (echo_area_buffer[0]))
	{
	  if (EQ (echo_area_buffer[1], echo_buffer[0]))
	    echo_area_buffer[0] = echo_buffer[1];
	  else
	    echo_area_buffer[0] = echo_buffer[0];
	}

      if (current_buffer != XBUFFER (echo_area_buffer[0]))
	{
	  /* Someone switched buffers between print requests.  */
	  set_buffer_internal (XBUFFER (echo_area_buffer[0]));
	  BVAR (current_buffer, truncate_lines) = Qnil;
	}
    }
}


/* Display an echo area message in window W.  Value is non-zero if W's
   height is changed.  If display_last_displayed_message_p is
   non-zero, display the message that was last displayed, otherwise
   display the current message.  */

static int
display_echo_area (struct window *w)
{
  int i, no_message_p, window_height_changed_p, count;

  /* Temporarily disable garbage collections while displaying the echo
     area.  This is done because a GC can print a message itself.
     That message would modify the echo area buffer's contents while a
     redisplay of the buffer is going on, and seriously confuse
     redisplay.  */
  count = inhibit_garbage_collection ();

  /* If there is no message, we must call display_echo_area_1
     nevertheless because it resizes the window.  But we will have to
     reset the echo_area_buffer in question to nil at the end because
     with_echo_area_buffer will sets it to an empty buffer.  */
  i = display_last_displayed_message_p ? 1 : 0;
  no_message_p = NILP (echo_area_buffer[i]);

  window_height_changed_p
    = with_echo_area_buffer (w, display_last_displayed_message_p,
			     display_echo_area_1,
			     (intptr_t) w, Qnil, 0, 0);

  if (no_message_p)
    echo_area_buffer[i] = Qnil;

  unbind_to (count, Qnil);
  return window_height_changed_p;
}


/* Helper for display_echo_area.  Display the current buffer which
   contains the current echo area message in window W, a mini-window,
   a pointer to which is passed in A1.  A2..A4 are currently not used.
   Change the height of W so that all of the message is displayed.
   Value is non-zero if height of W was changed.  */

static int
display_echo_area_1 (EMACS_INT a1, Lisp_Object a2, EMACS_INT a3, EMACS_INT a4)
{
  intptr_t i1 = a1;
  struct window *w = (struct window *) i1;
  Lisp_Object window;
  struct text_pos start;
  int window_height_changed_p = 0;

  /* Do this before displaying, so that we have a large enough glyph
     matrix for the display.  If we can't get enough space for the
     whole text, display the last N lines.  That works by setting w->start.  */
  window_height_changed_p = resize_mini_window (w, 0);

  /* Use the starting position chosen by resize_mini_window.  */
  SET_TEXT_POS_FROM_MARKER (start, w->start);

  /* Display.  */
  clear_glyph_matrix (w->desired_matrix);
  XSETWINDOW (window, w);
  try_window (window, start, 0);

  return window_height_changed_p;
}


/* Resize the echo area window to exactly the size needed for the
   currently displayed message, if there is one.  If a mini-buffer
   is active, don't shrink it.  */

void
resize_echo_area_exactly (void)
{
  if (BUFFERP (echo_area_buffer[0])
      && WINDOWP (echo_area_window))
    {
      struct window *w = XWINDOW (echo_area_window);
      int resized_p;
      Lisp_Object resize_exactly;

      if (minibuf_level == 0)
	resize_exactly = Qt;
      else
	resize_exactly = Qnil;

      resized_p = with_echo_area_buffer (w, 0, resize_mini_window_1,
					 (intptr_t) w, resize_exactly,
					 0, 0);
      if (resized_p)
	{
	  ++windows_or_buffers_changed;
	  ++update_mode_lines;
	  redisplay_internal ();
	}
    }
}


/* Callback function for with_echo_area_buffer, when used from
   resize_echo_area_exactly.  A1 contains a pointer to the window to
   resize, EXACTLY non-nil means resize the mini-window exactly to the
   size of the text displayed.  A3 and A4 are not used.  Value is what
   resize_mini_window returns.  */

static int
resize_mini_window_1 (EMACS_INT a1, Lisp_Object exactly, EMACS_INT a3, EMACS_INT a4)
{
  intptr_t i1 = a1;
  return resize_mini_window ((struct window *) i1, !NILP (exactly));
}


/* Resize mini-window W to fit the size of its contents.  EXACT_P
   means size the window exactly to the size needed.  Otherwise, it's
   only enlarged until W's buffer is empty.

   Set W->start to the right place to begin display.  If the whole
   contents fit, start at the beginning.  Otherwise, start so as
   to make the end of the contents appear.  This is particularly
   important for y-or-n-p, but seems desirable generally.

   Value is non-zero if the window height has been changed.  */

int
resize_mini_window (struct window *w, int exact_p)
{
  struct frame *f = XFRAME (w->frame);
  int window_height_changed_p = 0;

  xassert (MINI_WINDOW_P (w));

  /* By default, start display at the beginning.  */
  set_marker_both (w->start, w->buffer,
		   BUF_BEGV (XBUFFER (w->buffer)),
		   BUF_BEGV_BYTE (XBUFFER (w->buffer)));

  /* Don't resize windows while redisplaying a window; it would
     confuse redisplay functions when the size of the window they are
     displaying changes from under them.  Such a resizing can happen,
     for instance, when which-func prints a long message while
     we are running fontification-functions.  We're running these
     functions with safe_call which binds inhibit-redisplay to t.  */
  if (!NILP (Vinhibit_redisplay))
    return 0;

  /* Nil means don't try to resize.  */
  if (NILP (Vresize_mini_windows)
      || (FRAME_X_P (f) && FRAME_X_OUTPUT (f) == NULL))
    return 0;

  if (!FRAME_MINIBUF_ONLY_P (f))
    {
      struct it it;
      struct window *root = XWINDOW (FRAME_ROOT_WINDOW (f));
      int total_height = WINDOW_TOTAL_LINES (root) + WINDOW_TOTAL_LINES (w);
      int height, max_height;
      int unit = FRAME_LINE_HEIGHT (f);
      struct text_pos start;
      struct buffer *old_current_buffer = NULL;

      if (current_buffer != XBUFFER (w->buffer))
	{
	  old_current_buffer = current_buffer;
	  set_buffer_internal (XBUFFER (w->buffer));
	}

      init_iterator (&it, w, BEGV, BEGV_BYTE, NULL, DEFAULT_FACE_ID);

      /* Compute the max. number of lines specified by the user.  */
      if (FLOATP (Vmax_mini_window_height))
	max_height = XFLOATINT (Vmax_mini_window_height) * FRAME_LINES (f);
      else if (INTEGERP (Vmax_mini_window_height))
	max_height = XINT (Vmax_mini_window_height);
      else
	max_height = total_height / 4;

      /* Correct that max. height if it's bogus.  */
      max_height = max (1, max_height);
      max_height = min (total_height, max_height);

      /* Find out the height of the text in the window.  */
      if (it.line_wrap == TRUNCATE)
	height = 1;
      else
	{
	  last_height = 0;
	  move_it_to (&it, ZV, -1, -1, -1, MOVE_TO_POS);
	  if (it.max_ascent == 0 && it.max_descent == 0)
	    height = it.current_y + last_height;
	  else
	    height = it.current_y + it.max_ascent + it.max_descent;
	  height -= min (it.extra_line_spacing, it.max_extra_line_spacing);
	  height = (height + unit - 1) / unit;
	}

      /* Compute a suitable window start.  */
      if (height > max_height)
	{
	  height = max_height;
	  init_iterator (&it, w, ZV, ZV_BYTE, NULL, DEFAULT_FACE_ID);
	  move_it_vertically_backward (&it, (height - 1) * unit);
	  start = it.current.pos;
	}
      else
	SET_TEXT_POS (start, BEGV, BEGV_BYTE);
      SET_MARKER_FROM_TEXT_POS (w->start, start);

      if (EQ (Vresize_mini_windows, Qgrow_only))
	{
	  /* Let it grow only, until we display an empty message, in which
	     case the window shrinks again.  */
	  if (height > WINDOW_TOTAL_LINES (w))
	    {
	      int old_height = WINDOW_TOTAL_LINES (w);
	      freeze_window_starts (f, 1);
	      grow_mini_window (w, height - WINDOW_TOTAL_LINES (w));
	      window_height_changed_p = WINDOW_TOTAL_LINES (w) != old_height;
	    }
	  else if (height < WINDOW_TOTAL_LINES (w)
		   && (exact_p || BEGV == ZV))
	    {
	      int old_height = WINDOW_TOTAL_LINES (w);
	      freeze_window_starts (f, 0);
	      shrink_mini_window (w);
	      window_height_changed_p = WINDOW_TOTAL_LINES (w) != old_height;
	    }
	}
      else
	{
	  /* Always resize to exact size needed.  */
	  if (height > WINDOW_TOTAL_LINES (w))
	    {
	      int old_height = WINDOW_TOTAL_LINES (w);
	      freeze_window_starts (f, 1);
	      grow_mini_window (w, height - WINDOW_TOTAL_LINES (w));
	      window_height_changed_p = WINDOW_TOTAL_LINES (w) != old_height;
	    }
	  else if (height < WINDOW_TOTAL_LINES (w))
	    {
	      int old_height = WINDOW_TOTAL_LINES (w);
	      freeze_window_starts (f, 0);
	      shrink_mini_window (w);

	      if (height)
		{
		  freeze_window_starts (f, 1);
		  grow_mini_window (w, height - WINDOW_TOTAL_LINES (w));
		}

	      window_height_changed_p = WINDOW_TOTAL_LINES (w) != old_height;
	    }
	}

      if (old_current_buffer)
	set_buffer_internal (old_current_buffer);
    }

  return window_height_changed_p;
}


/* Value is the current message, a string, or nil if there is no
   current message.  */

Lisp_Object
current_message (void)
{
  Lisp_Object msg;

  if (!BUFFERP (echo_area_buffer[0]))
    msg = Qnil;
  else
    {
      with_echo_area_buffer (0, 0, current_message_1,
			     (intptr_t) &msg, Qnil, 0, 0);
      if (NILP (msg))
	echo_area_buffer[0] = Qnil;
    }

  return msg;
}


static int
current_message_1 (EMACS_INT a1, Lisp_Object a2, EMACS_INT a3, EMACS_INT a4)
{
  intptr_t i1 = a1;
  Lisp_Object *msg = (Lisp_Object *) i1;

  if (Z > BEG)
    *msg = make_buffer_string (BEG, Z, 1);
  else
    *msg = Qnil;
  return 0;
}


/* Push the current message on Vmessage_stack for later restoration
   by restore_message.  Value is non-zero if the current message isn't
   empty.  This is a relatively infrequent operation, so it's not
   worth optimizing.  */

int
push_message (void)
{
  Lisp_Object msg;
  msg = current_message ();
  Vmessage_stack = Fcons (msg, Vmessage_stack);
  return STRINGP (msg);
}


/* Restore message display from the top of Vmessage_stack.  */

void
restore_message (void)
{
  Lisp_Object msg;

  xassert (CONSP (Vmessage_stack));
  msg = XCAR (Vmessage_stack);
  if (STRINGP (msg))
    message3_nolog (msg, SBYTES (msg), STRING_MULTIBYTE (msg));
  else
    message3_nolog (msg, 0, 0);
}


/* Handler for record_unwind_protect calling pop_message.  */

Lisp_Object
pop_message_unwind (Lisp_Object dummy)
{
  pop_message ();
  return Qnil;
}

/* Pop the top-most entry off Vmessage_stack.  */

static void
pop_message (void)
{
  xassert (CONSP (Vmessage_stack));
  Vmessage_stack = XCDR (Vmessage_stack);
}


/* Check that Vmessage_stack is nil.  Called from emacs.c when Emacs
   exits.  If the stack is not empty, we have a missing pop_message
   somewhere.  */

void
check_message_stack (void)
{
  if (!NILP (Vmessage_stack))
    abort ();
}


/* Truncate to NCHARS what will be displayed in the echo area the next
   time we display it---but don't redisplay it now.  */

void
truncate_echo_area (EMACS_INT nchars)
{
  if (nchars == 0)
    echo_area_buffer[0] = Qnil;
  /* A null message buffer means that the frame hasn't really been
     initialized yet.  Error messages get reported properly by
     cmd_error, so this must be just an informative message; toss it.  */
  else if (!noninteractive
	   && INTERACTIVE
	   && !NILP (echo_area_buffer[0]))
    {
      struct frame *sf = SELECTED_FRAME ();
      if (FRAME_MESSAGE_BUF (sf))
	with_echo_area_buffer (0, 0, truncate_message_1, nchars, Qnil, 0, 0);
    }
}


/* Helper function for truncate_echo_area.  Truncate the current
   message to at most NCHARS characters.  */

static int
truncate_message_1 (EMACS_INT nchars, Lisp_Object a2, EMACS_INT a3, EMACS_INT a4)
{
  if (BEG + nchars < Z)
    del_range (BEG + nchars, Z);
  if (Z == BEG)
    echo_area_buffer[0] = Qnil;
  return 0;
}


/* Set the current message to a substring of S or STRING.

   If STRING is a Lisp string, set the message to the first NBYTES
   bytes from STRING.  NBYTES zero means use the whole string.  If
   STRING is multibyte, the message will be displayed multibyte.

   If S is not null, set the message to the first LEN bytes of S.  LEN
   zero means use the whole string.  MULTIBYTE_P non-zero means S is
   multibyte.  Display the message multibyte in that case.

   Doesn't GC, as with_echo_area_buffer binds Qinhibit_modification_hooks
   to t before calling set_message_1 (which calls insert).
  */

static void
set_message (const char *s, Lisp_Object string,
	     EMACS_INT nbytes, int multibyte_p)
{
  message_enable_multibyte
    = ((s && multibyte_p)
       || (STRINGP (string) && STRING_MULTIBYTE (string)));

  with_echo_area_buffer (0, -1, set_message_1,
			 (intptr_t) s, string, nbytes, multibyte_p);
  message_buf_print = 0;
  help_echo_showing_p = 0;
}


/* Helper function for set_message.  Arguments have the same meaning
   as there, with A1 corresponding to S and A2 corresponding to STRING
   This function is called with the echo area buffer being
   current.  */

static int
set_message_1 (EMACS_INT a1, Lisp_Object a2, EMACS_INT nbytes, EMACS_INT multibyte_p)
{
  intptr_t i1 = a1;
  const char *s = (const char *) i1;
  const unsigned char *msg = (const unsigned char *) s;
  Lisp_Object string = a2;

  /* Change multibyteness of the echo buffer appropriately.  */
  if (message_enable_multibyte
      != !NILP (BVAR (current_buffer, enable_multibyte_characters)))
    Fset_buffer_multibyte (message_enable_multibyte ? Qt : Qnil);

  BVAR (current_buffer, truncate_lines) = message_truncate_lines ? Qt : Qnil;
  if (!NILP (BVAR (current_buffer, bidi_display_reordering)))
    BVAR (current_buffer, bidi_paragraph_direction) = Qleft_to_right;

  /* Insert new message at BEG.  */
  TEMP_SET_PT_BOTH (BEG, BEG_BYTE);

  if (STRINGP (string))
    {
      EMACS_INT nchars;

      if (nbytes == 0)
	nbytes = SBYTES (string);
      nchars = string_byte_to_char (string, nbytes);

      /* This function takes care of single/multibyte conversion.  We
         just have to ensure that the echo area buffer has the right
         setting of enable_multibyte_characters.  */
      insert_from_string (string, 0, 0, nchars, nbytes, 1);
    }
  else if (s)
    {
      if (nbytes == 0)
	nbytes = strlen (s);

      if (multibyte_p && NILP (BVAR (current_buffer, enable_multibyte_characters)))
	{
	  /* Convert from multi-byte to single-byte.  */
	  EMACS_INT i;
	  int c, n;
	  char work[1];

	  /* Convert a multibyte string to single-byte.  */
	  for (i = 0; i < nbytes; i += n)
	    {
	      c = string_char_and_length (msg + i, &n);
	      work[0] = (ASCII_CHAR_P (c)
			 ? c
			 : multibyte_char_to_unibyte (c));
	      insert_1_both (work, 1, 1, 1, 0, 0);
	    }
	}
      else if (!multibyte_p
	       && !NILP (BVAR (current_buffer, enable_multibyte_characters)))
	{
	  /* Convert from single-byte to multi-byte.  */
	  EMACS_INT i;
	  int c, n;
	  unsigned char str[MAX_MULTIBYTE_LENGTH];

	  /* Convert a single-byte string to multibyte.  */
	  for (i = 0; i < nbytes; i++)
	    {
	      c = msg[i];
	      MAKE_CHAR_MULTIBYTE (c);
	      n = CHAR_STRING (c, str);
	      insert_1_both ((char *) str, 1, n, 1, 0, 0);
	    }
	}
      else
	insert_1 (s, nbytes, 1, 0, 0);
    }

  return 0;
}


/* Clear messages.  CURRENT_P non-zero means clear the current
   message.  LAST_DISPLAYED_P non-zero means clear the message
   last displayed.  */

void
clear_message (int current_p, int last_displayed_p)
{
  if (current_p)
    {
      echo_area_buffer[0] = Qnil;
      message_cleared_p = 1;
    }

  if (last_displayed_p)
    echo_area_buffer[1] = Qnil;

  message_buf_print = 0;
}

/* Clear garbaged frames.

   This function is used where the old redisplay called
   redraw_garbaged_frames which in turn called redraw_frame which in
   turn called clear_frame.  The call to clear_frame was a source of
   flickering.  I believe a clear_frame is not necessary.  It should
   suffice in the new redisplay to invalidate all current matrices,
   and ensure a complete redisplay of all windows.  */

static void
clear_garbaged_frames (void)
{
  if (frame_garbaged)
    {
      Lisp_Object tail, frame;
      int changed_count = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);

	  if (FRAME_VISIBLE_P (f) && FRAME_GARBAGED_P (f))
	    {
	      if (f->resized_p)
		{
		  Fredraw_frame (frame);
		  f->force_flush_display_p = 1;
		}
	      clear_current_matrices (f);
	      changed_count++;
	      f->garbaged = 0;
	      f->resized_p = 0;
	    }
	}

      frame_garbaged = 0;
      if (changed_count)
	++windows_or_buffers_changed;
    }
}


/* Redisplay the echo area of the selected frame.  If UPDATE_FRAME_P
   is non-zero update selected_frame.  Value is non-zero if the
   mini-windows height has been changed.  */

static int
echo_area_display (int update_frame_p)
{
  Lisp_Object mini_window;
  struct window *w;
  struct frame *f;
  int window_height_changed_p = 0;
  struct frame *sf = SELECTED_FRAME ();

  mini_window = FRAME_MINIBUF_WINDOW (sf);
  w = XWINDOW (mini_window);
  f = XFRAME (WINDOW_FRAME (w));

  /* Don't display if frame is invisible or not yet initialized.  */
  if (!FRAME_VISIBLE_P (f) || !f->glyphs_initialized_p)
    return 0;

#ifdef HAVE_WINDOW_SYSTEM
  /* When Emacs starts, selected_frame may be the initial terminal
     frame.  If we let this through, a message would be displayed on
     the terminal.  */
  if (FRAME_INITIAL_P (XFRAME (selected_frame)))
    return 0;
#endif /* HAVE_WINDOW_SYSTEM */

  /* Redraw garbaged frames.  */
  if (frame_garbaged)
    clear_garbaged_frames ();

  if (!NILP (echo_area_buffer[0]) || minibuf_level == 0)
    {
      echo_area_window = mini_window;
      window_height_changed_p = display_echo_area (w);
      w->must_be_updated_p = 1;

      /* Update the display, unless called from redisplay_internal.
	 Also don't update the screen during redisplay itself.  The
	 update will happen at the end of redisplay, and an update
	 here could cause confusion.  */
      if (update_frame_p && !redisplaying_p)
	{
	  int n = 0;

	  /* If the display update has been interrupted by pending
	     input, update mode lines in the frame.  Due to the
	     pending input, it might have been that redisplay hasn't
	     been called, so that mode lines above the echo area are
	     garbaged.  This looks odd, so we prevent it here.  */
	  if (!display_completed)
	    n = redisplay_mode_lines (FRAME_ROOT_WINDOW (f), 0);

	  if (window_height_changed_p
	      /* Don't do this if Emacs is shutting down.  Redisplay
	         needs to run hooks.  */
	      && !NILP (Vrun_hooks))
	    {
	      /* Must update other windows.  Likewise as in other
		 cases, don't let this update be interrupted by
		 pending input.  */
	      int count = SPECPDL_INDEX ();
	      specbind (Qredisplay_dont_pause, Qt);
	      windows_or_buffers_changed = 1;
	      redisplay_internal ();
	      unbind_to (count, Qnil);
	    }
	  else if (FRAME_WINDOW_P (f) && n == 0)
	    {
	      /* Window configuration is the same as before.
		 Can do with a display update of the echo area,
		 unless we displayed some mode lines.  */
	      update_single_window (w, 1);
	      FRAME_RIF (f)->flush_display (f);
	    }
	  else
	    update_frame (f, 1, 1);

	  /* If cursor is in the echo area, make sure that the next
	     redisplay displays the minibuffer, so that the cursor will
	     be replaced with what the minibuffer wants.  */
	  if (cursor_in_echo_area)
	    ++windows_or_buffers_changed;
	}
    }
  else if (!EQ (mini_window, selected_window))
    windows_or_buffers_changed++;

  /* Last displayed message is now the current message.  */
  echo_area_buffer[1] = echo_area_buffer[0];
  /* Inform read_char that we're not echoing.  */
  echo_message_buffer = Qnil;

  /* Prevent redisplay optimization in redisplay_internal by resetting
     this_line_start_pos.  This is done because the mini-buffer now
     displays the message instead of its buffer text.  */
  if (EQ (mini_window, selected_window))
    CHARPOS (this_line_start_pos) = 0;

  return window_height_changed_p;
}



/***********************************************************************
		     Mode Lines and Frame Titles
 ***********************************************************************/

/* A buffer for constructing non-propertized mode-line strings and
   frame titles in it; allocated from the heap in init_xdisp and
   resized as needed in store_mode_line_noprop_char.  */

static char *mode_line_noprop_buf;

/* The buffer's end, and a current output position in it.  */

static char *mode_line_noprop_buf_end;
static char *mode_line_noprop_ptr;

#define MODE_LINE_NOPROP_LEN(start) \
  ((mode_line_noprop_ptr - mode_line_noprop_buf) - start)

static enum {
  MODE_LINE_DISPLAY = 0,
  MODE_LINE_TITLE,
  MODE_LINE_NOPROP,
  MODE_LINE_STRING
} mode_line_target;

/* Alist that caches the results of :propertize.
   Each element is (PROPERTIZED-STRING . PROPERTY-LIST).  */
static Lisp_Object mode_line_proptrans_alist;

/* List of strings making up the mode-line.  */
static Lisp_Object mode_line_string_list;

/* Base face property when building propertized mode line string.  */
static Lisp_Object mode_line_string_face;
static Lisp_Object mode_line_string_face_prop;


/* Unwind data for mode line strings */

static Lisp_Object Vmode_line_unwind_vector;

static Lisp_Object
format_mode_line_unwind_data (struct buffer *obuf,
			      Lisp_Object owin,
			      int save_proptrans)
{
  Lisp_Object vector, tmp;

  /* Reduce consing by keeping one vector in
     Vwith_echo_area_save_vector.  */
  vector = Vmode_line_unwind_vector;
  Vmode_line_unwind_vector = Qnil;

  if (NILP (vector))
    vector = Fmake_vector (make_number (8), Qnil);

  ASET (vector, 0, make_number (mode_line_target));
  ASET (vector, 1, make_number (MODE_LINE_NOPROP_LEN (0)));
  ASET (vector, 2, mode_line_string_list);
  ASET (vector, 3, save_proptrans ? mode_line_proptrans_alist : Qt);
  ASET (vector, 4, mode_line_string_face);
  ASET (vector, 5, mode_line_string_face_prop);

  if (obuf)
    XSETBUFFER (tmp, obuf);
  else
    tmp = Qnil;
  ASET (vector, 6, tmp);
  ASET (vector, 7, owin);

  return vector;
}

static Lisp_Object
unwind_format_mode_line (Lisp_Object vector)
{
  mode_line_target = XINT (AREF (vector, 0));
  mode_line_noprop_ptr = mode_line_noprop_buf + XINT (AREF (vector, 1));
  mode_line_string_list = AREF (vector, 2);
  if (! EQ (AREF (vector, 3), Qt))
    mode_line_proptrans_alist = AREF (vector, 3);
  mode_line_string_face = AREF (vector, 4);
  mode_line_string_face_prop = AREF (vector, 5);

  if (!NILP (AREF (vector, 7)))
    /* Select window before buffer, since it may change the buffer.  */
    Fselect_window (AREF (vector, 7), Qt);

  if (!NILP (AREF (vector, 6)))
    {
      set_buffer_internal_1 (XBUFFER (AREF (vector, 6)));
      ASET (vector, 6, Qnil);
    }

  Vmode_line_unwind_vector = vector;
  return Qnil;
}


/* Store a single character C for the frame title in mode_line_noprop_buf.
   Re-allocate mode_line_noprop_buf if necessary.  */

static void
store_mode_line_noprop_char (char c)
{
  /* If output position has reached the end of the allocated buffer,
     increase the buffer's size.  */
  if (mode_line_noprop_ptr == mode_line_noprop_buf_end)
    {
      ptrdiff_t len = MODE_LINE_NOPROP_LEN (0);
      ptrdiff_t size = len;
      mode_line_noprop_buf =
	xpalloc (mode_line_noprop_buf, &size, 1, STRING_BYTES_BOUND, 1);
      mode_line_noprop_buf_end = mode_line_noprop_buf + size;
      mode_line_noprop_ptr = mode_line_noprop_buf + len;
    }

  *mode_line_noprop_ptr++ = c;
}


/* Store part of a frame title in mode_line_noprop_buf, beginning at
   mode_line_noprop_ptr.  STRING is the string to store.  Do not copy
   characters that yield more columns than PRECISION; PRECISION <= 0
   means copy the whole string.  Pad with spaces until FIELD_WIDTH
   number of characters have been copied; FIELD_WIDTH <= 0 means don't
   pad.  Called from display_mode_element when it is used to build a
   frame title.  */

static int
store_mode_line_noprop (const char *string, int field_width, int precision)
{
  const unsigned char *str = (const unsigned char *) string;
  int n = 0;
  EMACS_INT dummy, nbytes;

  /* Copy at most PRECISION chars from STR.  */
  nbytes = strlen (string);
  n += c_string_width (str, nbytes, precision, &dummy, &nbytes);
  while (nbytes--)
    store_mode_line_noprop_char (*str++);

  /* Fill up with spaces until FIELD_WIDTH reached.  */
  while (field_width > 0
	 && n < field_width)
    {
      store_mode_line_noprop_char (' ');
      ++n;
    }

  return n;
}

/***********************************************************************
			     Frame Titles
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

/* Set the title of FRAME, if it has changed.  The title format is
   Vicon_title_format if FRAME is iconified, otherwise it is
   frame_title_format.  */

static void
x_consider_frame_title (Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);

  if (FRAME_WINDOW_P (f)
      || FRAME_MINIBUF_ONLY_P (f)
      || f->explicit_name)
    {
      /* Do we have more than one visible frame on this X display?  */
      Lisp_Object tail;
      Lisp_Object fmt;
      ptrdiff_t title_start;
      char *title;
      ptrdiff_t len;
      struct it it;
      int count = SPECPDL_INDEX ();

      for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object other_frame = XCAR (tail);
	  struct frame *tf = XFRAME (other_frame);

	  if (tf != f
	      && FRAME_KBOARD (tf) == FRAME_KBOARD (f)
	      && !FRAME_MINIBUF_ONLY_P (tf)
	      && !EQ (other_frame, tip_frame)
	      && (FRAME_VISIBLE_P (tf) || FRAME_ICONIFIED_P (tf)))
	    break;
	}

      /* Set global variable indicating that multiple frames exist.  */
      multiple_frames = CONSP (tail);

      /* Switch to the buffer of selected window of the frame.  Set up
	 mode_line_target so that display_mode_element will output into
	 mode_line_noprop_buf; then display the title.  */
      record_unwind_protect (unwind_format_mode_line,
			     format_mode_line_unwind_data
			        (current_buffer, selected_window, 0));

      Fselect_window (f->selected_window, Qt);
      set_buffer_internal_1 (XBUFFER (XWINDOW (f->selected_window)->buffer));
      fmt = FRAME_ICONIFIED_P (f) ? Vicon_title_format : Vframe_title_format;

      mode_line_target = MODE_LINE_TITLE;
      title_start = MODE_LINE_NOPROP_LEN (0);
      init_iterator (&it, XWINDOW (f->selected_window), -1, -1,
		     NULL, DEFAULT_FACE_ID);
      display_mode_element (&it, 0, -1, -1, fmt, Qnil, 0);
      len = MODE_LINE_NOPROP_LEN (title_start);
      title = mode_line_noprop_buf + title_start;
      unbind_to (count, Qnil);

      /* Set the title only if it's changed.  This avoids consing in
	 the common case where it hasn't.  (If it turns out that we've
	 already wasted too much time by walking through the list with
	 display_mode_element, then we might need to optimize at a
	 higher level than this.)  */
      if (! STRINGP (f->name)
	  || SBYTES (f->name) != len
	  || memcmp (title, SDATA (f->name), len) != 0)
	x_implicitly_set_name (f, make_string (title, len), Qnil);
    }
}

#endif /* not HAVE_WINDOW_SYSTEM */




/***********************************************************************
			      Menu Bars
 ***********************************************************************/


/* Prepare for redisplay by updating menu-bar item lists when
   appropriate.  This can call eval.  */

void
prepare_menu_bars (void)
{
  int all_windows;
  struct gcpro gcpro1, gcpro2;
  struct frame *f;
  Lisp_Object tooltip_frame;

#ifdef HAVE_WINDOW_SYSTEM
  tooltip_frame = tip_frame;
#else
  tooltip_frame = Qnil;
#endif

  /* Update all frame titles based on their buffer names, etc.  We do
     this before the menu bars so that the buffer-menu will show the
     up-to-date frame titles.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (windows_or_buffers_changed || update_mode_lines)
    {
      Lisp_Object tail, frame;

      FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);
	  if (!EQ (frame, tooltip_frame)
	      && (FRAME_VISIBLE_P (f) || FRAME_ICONIFIED_P (f)))
	    x_consider_frame_title (frame);
	}
    }
#endif /* HAVE_WINDOW_SYSTEM */

  /* Update the menu bar item lists, if appropriate.  This has to be
     done before any actual redisplay or generation of display lines.  */
  all_windows = (update_mode_lines
		 || buffer_shared > 1
		 || windows_or_buffers_changed);
  if (all_windows)
    {
      Lisp_Object tail, frame;
      int count = SPECPDL_INDEX ();
      /* 1 means that update_menu_bar has run its hooks
	 so any further calls to update_menu_bar shouldn't do so again.  */
      int menu_bar_hooks_run = 0;

      record_unwind_save_match_data ();

      FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);

	  /* Ignore tooltip frame.  */
	  if (EQ (frame, tooltip_frame))
	    continue;

	  /* If a window on this frame changed size, report that to
	     the user and clear the size-change flag.  */
	  if (FRAME_WINDOW_SIZES_CHANGED (f))
	    {
	      Lisp_Object functions;

	      /* Clear flag first in case we get an error below.  */
	      FRAME_WINDOW_SIZES_CHANGED (f) = 0;
	      functions = Vwindow_size_change_functions;
	      GCPRO2 (tail, functions);

	      while (CONSP (functions))
		{
		  if (!EQ (XCAR (functions), Qt))
		    call1 (XCAR (functions), frame);
		  functions = XCDR (functions);
		}
	      UNGCPRO;
	    }

	  GCPRO1 (tail);
	  menu_bar_hooks_run = update_menu_bar (f, 0, menu_bar_hooks_run);
#ifdef HAVE_WINDOW_SYSTEM
	  update_tool_bar (f, 0);
#endif
#ifdef HAVE_NS
          if (windows_or_buffers_changed
	      && FRAME_NS_P (f))
            ns_set_doc_edited (f, Fbuffer_modified_p
			       (XWINDOW (f->selected_window)->buffer));
#endif
	  UNGCPRO;
	}

      unbind_to (count, Qnil);
    }
  else
    {
      struct frame *sf = SELECTED_FRAME ();
      update_menu_bar (sf, 1, 0);
#ifdef HAVE_WINDOW_SYSTEM
      update_tool_bar (sf, 1);
#endif
    }
}


/* Update the menu bar item list for frame F.  This has to be done
   before we start to fill in any display lines, because it can call
   eval.

   If SAVE_MATCH_DATA is non-zero, we must save and restore it here.

   If HOOKS_RUN is 1, that means a previous call to update_menu_bar
   already ran the menu bar hooks for this redisplay, so there
   is no need to run them again.  The return value is the
   updated value of this flag, to pass to the next call.  */

static int
update_menu_bar (struct frame *f, int save_match_data, int hooks_run)
{
  Lisp_Object window;
  register struct window *w;

  /* If called recursively during a menu update, do nothing.  This can
     happen when, for instance, an activate-menubar-hook causes a
     redisplay.  */
  if (inhibit_menubar_update)
    return hooks_run;

  window = FRAME_SELECTED_WINDOW (f);
  w = XWINDOW (window);

  if (FRAME_WINDOW_P (f)
      ?
#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) \
    || defined (HAVE_NS) || defined (USE_GTK)
      FRAME_EXTERNAL_MENU_BAR (f)
#else
      FRAME_MENU_BAR_LINES (f) > 0
#endif
      : FRAME_MENU_BAR_LINES (f) > 0)
    {
      /* If the user has switched buffers or windows, we need to
	 recompute to reflect the new bindings.  But we'll
	 recompute when update_mode_lines is set too; that means
	 that people can use force-mode-line-update to request
	 that the menu bar be recomputed.  The adverse effect on
	 the rest of the redisplay algorithm is about the same as
	 windows_or_buffers_changed anyway.  */
      if (windows_or_buffers_changed
	  /* This used to test w->update_mode_line, but we believe
	     there is no need to recompute the menu in that case.  */
	  || update_mode_lines
	  || ((BUF_SAVE_MODIFF (XBUFFER (w->buffer))
	       < BUF_MODIFF (XBUFFER (w->buffer)))
	      != !NILP (w->last_had_star))
	  || ((!NILP (Vtransient_mark_mode)
	       && !NILP (BVAR (XBUFFER (w->buffer), mark_active)))
	      != !NILP (w->region_showing)))
	{
	  struct buffer *prev = current_buffer;
	  int count = SPECPDL_INDEX ();

	  specbind (Qinhibit_menubar_update, Qt);

	  set_buffer_internal_1 (XBUFFER (w->buffer));
	  if (save_match_data)
	    record_unwind_save_match_data ();
	  if (NILP (Voverriding_local_map_menu_flag))
	    {
	      specbind (Qoverriding_terminal_local_map, Qnil);
	      specbind (Qoverriding_local_map, Qnil);
	    }

	  if (!hooks_run)
	    {
	      /* Run the Lucid hook.  */
	      safe_run_hooks (Qactivate_menubar_hook);

	      /* If it has changed current-menubar from previous value,
		 really recompute the menu-bar from the value.  */
	      if (! NILP (Vlucid_menu_bar_dirty_flag))
		call0 (Qrecompute_lucid_menubar);

	      safe_run_hooks (Qmenu_bar_update_hook);

	      hooks_run = 1;
	    }

	  XSETFRAME (Vmenu_updating_frame, f);
	  FRAME_MENU_BAR_ITEMS (f) = menu_bar_items (FRAME_MENU_BAR_ITEMS (f));

	  /* Redisplay the menu bar in case we changed it.  */
#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) \
    || defined (HAVE_NS) || defined (USE_GTK)
	  if (FRAME_WINDOW_P (f))
            {
#if defined (HAVE_NS)
              /* All frames on Mac OS share the same menubar.  So only
                 the selected frame should be allowed to set it.  */
              if (f == SELECTED_FRAME ())
#endif
		set_frame_menubar (f, 0, 0);
	    }
	  else
	    /* On a terminal screen, the menu bar is an ordinary screen
	       line, and this makes it get updated.  */
	    w->update_mode_line = Qt;
#else /* ! (USE_X_TOOLKIT || HAVE_NTGUI || HAVE_NS || USE_GTK) */
	  /* In the non-toolkit version, the menu bar is an ordinary screen
	     line, and this makes it get updated.  */
	  w->update_mode_line = Qt;
#endif /* ! (USE_X_TOOLKIT || HAVE_NTGUI || HAVE_NS || USE_GTK) */

	  unbind_to (count, Qnil);
	  set_buffer_internal_1 (prev);
	}
    }

  return hooks_run;
}



/***********************************************************************
			    Output Cursor
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

/* EXPORT:
   Nominal cursor position -- where to draw output.
   HPOS and VPOS are window relative glyph matrix coordinates.
   X and Y are window relative pixel coordinates.  */

struct cursor_pos output_cursor;


/* EXPORT:
   Set the global variable output_cursor to CURSOR.  All cursor
   positions are relative to updated_window.  */

void
set_output_cursor (struct cursor_pos *cursor)
{
  output_cursor.hpos = cursor->hpos;
  output_cursor.vpos = cursor->vpos;
  output_cursor.x = cursor->x;
  output_cursor.y = cursor->y;
}


/* EXPORT for RIF:
   Set a nominal cursor position.

   HPOS and VPOS are column/row positions in a window glyph matrix.  X
   and Y are window text area relative pixel positions.

   If this is done during an update, updated_window will contain the
   window that is being updated and the position is the future output
   cursor position for that window.  If updated_window is null, use
   selected_window and display the cursor at the given position.  */

void
x_cursor_to (int vpos, int hpos, int y, int x)
{
  struct window *w;

  /* If updated_window is not set, work on selected_window.  */
  if (updated_window)
    w = updated_window;
  else
    w = XWINDOW (selected_window);

  /* Set the output cursor.  */
  output_cursor.hpos = hpos;
  output_cursor.vpos = vpos;
  output_cursor.x = x;
  output_cursor.y = y;

  /* If not called as part of an update, really display the cursor.
     This will also set the cursor position of W.  */
  if (updated_window == NULL)
    {
      BLOCK_INPUT;
      display_and_set_cursor (w, 1, hpos, vpos, x, y);
      if (FRAME_RIF (SELECTED_FRAME ())->flush_display_optional)
	FRAME_RIF (SELECTED_FRAME ())->flush_display_optional (SELECTED_FRAME ());
      UNBLOCK_INPUT;
    }
}

#endif /* HAVE_WINDOW_SYSTEM */


/***********************************************************************
			       Tool-bars
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

/* Where the mouse was last time we reported a mouse event.  */

FRAME_PTR last_mouse_frame;

/* Tool-bar item index of the item on which a mouse button was pressed
   or -1.  */

int last_tool_bar_item;


static Lisp_Object
update_tool_bar_unwind (Lisp_Object frame)
{
  selected_frame = frame;
  return Qnil;
}

/* Update the tool-bar item list for frame F.  This has to be done
   before we start to fill in any display lines.  Called from
   prepare_menu_bars.  If SAVE_MATCH_DATA is non-zero, we must save
   and restore it here.  */

static void
update_tool_bar (struct frame *f, int save_match_data)
{
#if defined (USE_GTK) || defined (HAVE_NS)
  int do_update = FRAME_EXTERNAL_TOOL_BAR (f);
#else
  int do_update = WINDOWP (f->tool_bar_window)
    && WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)) > 0;
#endif

  if (do_update)
    {
      Lisp_Object window;
      struct window *w;

      window = FRAME_SELECTED_WINDOW (f);
      w = XWINDOW (window);

      /* If the user has switched buffers or windows, we need to
	 recompute to reflect the new bindings.  But we'll
	 recompute when update_mode_lines is set too; that means
	 that people can use force-mode-line-update to request
	 that the menu bar be recomputed.  The adverse effect on
	 the rest of the redisplay algorithm is about the same as
	 windows_or_buffers_changed anyway.  */
      if (windows_or_buffers_changed
	  || !NILP (w->update_mode_line)
	  || update_mode_lines
	  || ((BUF_SAVE_MODIFF (XBUFFER (w->buffer))
	       < BUF_MODIFF (XBUFFER (w->buffer)))
	      != !NILP (w->last_had_star))
	  || ((!NILP (Vtransient_mark_mode)
	       && !NILP (BVAR (XBUFFER (w->buffer), mark_active)))
	      != !NILP (w->region_showing)))
	{
	  struct buffer *prev = current_buffer;
	  int count = SPECPDL_INDEX ();
	  Lisp_Object frame, new_tool_bar;
          int new_n_tool_bar;
	  struct gcpro gcpro1;

	  /* Set current_buffer to the buffer of the selected
	     window of the frame, so that we get the right local
	     keymaps.  */
	  set_buffer_internal_1 (XBUFFER (w->buffer));

	  /* Save match data, if we must.  */
	  if (save_match_data)
	    record_unwind_save_match_data ();

	  /* Make sure that we don't accidentally use bogus keymaps.  */
	  if (NILP (Voverriding_local_map_menu_flag))
	    {
	      specbind (Qoverriding_terminal_local_map, Qnil);
	      specbind (Qoverriding_local_map, Qnil);
	    }

	  GCPRO1 (new_tool_bar);

	  /* We must temporarily set the selected frame to this frame
	     before calling tool_bar_items, because the calculation of
	     the tool-bar keymap uses the selected frame (see
	     `tool-bar-make-keymap' in tool-bar.el).  */
	  record_unwind_protect (update_tool_bar_unwind, selected_frame);
	  XSETFRAME (frame, f);
	  selected_frame = frame;

	  /* Build desired tool-bar items from keymaps.  */
          new_tool_bar = tool_bar_items (Fcopy_sequence (f->tool_bar_items),
                                         &new_n_tool_bar);

	  /* Redisplay the tool-bar if we changed it.  */
	  if (new_n_tool_bar != f->n_tool_bar_items
	      || NILP (Fequal (new_tool_bar, f->tool_bar_items)))
            {
              /* Redisplay that happens asynchronously due to an expose event
                 may access f->tool_bar_items.  Make sure we update both
                 variables within BLOCK_INPUT so no such event interrupts.  */
              BLOCK_INPUT;
              f->tool_bar_items = new_tool_bar;
              f->n_tool_bar_items = new_n_tool_bar;
              w->update_mode_line = Qt;
              UNBLOCK_INPUT;
            }

	  UNGCPRO;

	  unbind_to (count, Qnil);
	  set_buffer_internal_1 (prev);
	}
    }
}


/* Set F->desired_tool_bar_string to a Lisp string representing frame
   F's desired tool-bar contents.  F->tool_bar_items must have
   been set up previously by calling prepare_menu_bars.  */

static void
build_desired_tool_bar_string (struct frame *f)
{
  int i, size, size_needed;
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object image, plist, props;

  image = plist = props = Qnil;
  GCPRO3 (image, plist, props);

  /* Prepare F->desired_tool_bar_string.  If we can reuse it, do so.
     Otherwise, make a new string.  */

  /* The size of the string we might be able to reuse.  */
  size = (STRINGP (f->desired_tool_bar_string)
	  ? SCHARS (f->desired_tool_bar_string)
	  : 0);

  /* We need one space in the string for each image.  */
  size_needed = f->n_tool_bar_items;

  /* Reuse f->desired_tool_bar_string, if possible.  */
  if (size < size_needed || NILP (f->desired_tool_bar_string))
    f->desired_tool_bar_string = Fmake_string (make_number (size_needed),
					       make_number (' '));
  else
    {
      props = list4 (Qdisplay, Qnil, Qmenu_item, Qnil);
      Fremove_text_properties (make_number (0), make_number (size),
			       props, f->desired_tool_bar_string);
    }

  /* Put a `display' property on the string for the images to display,
     put a `menu_item' property on tool-bar items with a value that
     is the index of the item in F's tool-bar item vector.  */
  for (i = 0; i < f->n_tool_bar_items; ++i)
    {
#define PROP(IDX) AREF (f->tool_bar_items, i * TOOL_BAR_ITEM_NSLOTS + (IDX))

      int enabled_p = !NILP (PROP (TOOL_BAR_ITEM_ENABLED_P));
      int selected_p = !NILP (PROP (TOOL_BAR_ITEM_SELECTED_P));
      int hmargin, vmargin, relief, idx, end;

      /* If image is a vector, choose the image according to the
	 button state.  */
      image = PROP (TOOL_BAR_ITEM_IMAGES);
      if (VECTORP (image))
	{
	  if (enabled_p)
	    idx = (selected_p
		   ? TOOL_BAR_IMAGE_ENABLED_SELECTED
		   : TOOL_BAR_IMAGE_ENABLED_DESELECTED);
	  else
	    idx = (selected_p
		   ? TOOL_BAR_IMAGE_DISABLED_SELECTED
		   : TOOL_BAR_IMAGE_DISABLED_DESELECTED);

	  xassert (ASIZE (image) >= idx);
	  image = AREF (image, idx);
	}
      else
	idx = -1;

      /* Ignore invalid image specifications.  */
      if (!valid_image_p (image))
	continue;

      /* Display the tool-bar button pressed, or depressed.  */
      plist = Fcopy_sequence (XCDR (image));

      /* Compute margin and relief to draw.  */
      relief = (tool_bar_button_relief >= 0
		? tool_bar_button_relief
		: DEFAULT_TOOL_BAR_BUTTON_RELIEF);
      hmargin = vmargin = relief;

      if (INTEGERP (Vtool_bar_button_margin)
	  && XINT (Vtool_bar_button_margin) > 0)
	{
	  hmargin += XFASTINT (Vtool_bar_button_margin);
	  vmargin += XFASTINT (Vtool_bar_button_margin);
	}
      else if (CONSP (Vtool_bar_button_margin))
	{
	  if (INTEGERP (XCAR (Vtool_bar_button_margin))
	      && XINT (XCAR (Vtool_bar_button_margin)) > 0)
	    hmargin += XFASTINT (XCAR (Vtool_bar_button_margin));

	  if (INTEGERP (XCDR (Vtool_bar_button_margin))
	      && XINT (XCDR (Vtool_bar_button_margin)) > 0)
	    vmargin += XFASTINT (XCDR (Vtool_bar_button_margin));
	}

      if (auto_raise_tool_bar_buttons_p)
	{
	  /* Add a `:relief' property to the image spec if the item is
	     selected.  */
	  if (selected_p)
	    {
	      plist = Fplist_put (plist, QCrelief, make_number (-relief));
	      hmargin -= relief;
	      vmargin -= relief;
	    }
	}
      else
	{
	  /* If image is selected, display it pressed, i.e. with a
	     negative relief.  If it's not selected, display it with a
	     raised relief.  */
	  plist = Fplist_put (plist, QCrelief,
			      (selected_p
			       ? make_number (-relief)
			       : make_number (relief)));
	  hmargin -= relief;
	  vmargin -= relief;
	}

      /* Put a margin around the image.  */
      if (hmargin || vmargin)
	{
	  if (hmargin == vmargin)
	    plist = Fplist_put (plist, QCmargin, make_number (hmargin));
	  else
	    plist = Fplist_put (plist, QCmargin,
				Fcons (make_number (hmargin),
				       make_number (vmargin)));
	}

      /* If button is not enabled, and we don't have special images
	 for the disabled state, make the image appear disabled by
	 applying an appropriate algorithm to it.  */
      if (!enabled_p && idx < 0)
	plist = Fplist_put (plist, QCconversion, Qdisabled);

      /* Put a `display' text property on the string for the image to
	 display.  Put a `menu-item' property on the string that gives
	 the start of this item's properties in the tool-bar items
	 vector.  */
      image = Fcons (Qimage, plist);
      props = list4 (Qdisplay, image,
		     Qmenu_item, make_number (i * TOOL_BAR_ITEM_NSLOTS));

      /* Let the last image hide all remaining spaces in the tool bar
         string.  The string can be longer than needed when we reuse a
         previous string.  */
      if (i + 1 == f->n_tool_bar_items)
	end = SCHARS (f->desired_tool_bar_string);
      else
	end = i + 1;
      Fadd_text_properties (make_number (i), make_number (end),
			    props, f->desired_tool_bar_string);
#undef PROP
    }

  UNGCPRO;
}


/* Display one line of the tool-bar of frame IT->f.

   HEIGHT specifies the desired height of the tool-bar line.
   If the actual height of the glyph row is less than HEIGHT, the
   row's height is increased to HEIGHT, and the icons are centered
   vertically in the new height.

   If HEIGHT is -1, we are counting needed tool-bar lines, so don't
   count a final empty row in case the tool-bar width exactly matches
   the window width.
*/

static void
display_tool_bar_line (struct it *it, int height)
{
  struct glyph_row *row = it->glyph_row;
  int max_x = it->last_visible_x;
  struct glyph *last;

  prepare_desired_row (row);
  row->y = it->current_y;

  /* Note that this isn't made use of if the face hasn't a box,
     so there's no need to check the face here.  */
  it->start_of_box_run_p = 1;

  while (it->current_x < max_x)
    {
      int x, n_glyphs_before, i, nglyphs;
      struct it it_before;

      /* Get the next display element.  */
      if (!get_next_display_element (it))
	{
	  /* Don't count empty row if we are counting needed tool-bar lines.  */
	  if (height < 0 && !it->hpos)
	    return;
	  break;
	}

      /* Produce glyphs.  */
      n_glyphs_before = row->used[TEXT_AREA];
      it_before = *it;

      PRODUCE_GLYPHS (it);

      nglyphs = row->used[TEXT_AREA] - n_glyphs_before;
      i = 0;
      x = it_before.current_x;
      while (i < nglyphs)
	{
	  struct glyph *glyph = row->glyphs[TEXT_AREA] + n_glyphs_before + i;

	  if (x + glyph->pixel_width > max_x)
	    {
	      /* Glyph doesn't fit on line.  Backtrack.  */
	      row->used[TEXT_AREA] = n_glyphs_before;
	      *it = it_before;
	      /* If this is the only glyph on this line, it will never fit on the
		 tool-bar, so skip it.  But ensure there is at least one glyph,
		 so we don't accidentally disable the tool-bar.  */
	      if (n_glyphs_before == 0
		  && (it->vpos > 0 || IT_STRING_CHARPOS (*it) < it->end_charpos-1))
		break;
	      goto out;
	    }

	  ++it->hpos;
	  x += glyph->pixel_width;
	  ++i;
	}

      /* Stop at line end.  */
      if (ITERATOR_AT_END_OF_LINE_P (it))
	break;

      set_iterator_to_next (it, 1);
    }

 out:;

  row->displays_text_p = row->used[TEXT_AREA] != 0;

  /* Use default face for the border below the tool bar.

     FIXME: When auto-resize-tool-bars is grow-only, there is
     no additional border below the possibly empty tool-bar lines.
     So to make the extra empty lines look "normal", we have to
     use the tool-bar face for the border too.  */
  if (!row->displays_text_p && !EQ (Vauto_resize_tool_bars, Qgrow_only))
    it->face_id = DEFAULT_FACE_ID;

  extend_face_to_end_of_line (it);
  last = row->glyphs[TEXT_AREA] + row->used[TEXT_AREA] - 1;
  last->right_box_line_p = 1;
  if (last == row->glyphs[TEXT_AREA])
    last->left_box_line_p = 1;

  /* Make line the desired height and center it vertically.  */
  if ((height -= it->max_ascent + it->max_descent) > 0)
    {
      /* Don't add more than one line height.  */
      height %= FRAME_LINE_HEIGHT (it->f);
      it->max_ascent += height / 2;
      it->max_descent += (height + 1) / 2;
    }

  compute_line_metrics (it);

  /* If line is empty, make it occupy the rest of the tool-bar.  */
  if (!row->displays_text_p)
    {
      row->height = row->phys_height = it->last_visible_y - row->y;
      row->visible_height = row->height;
      row->ascent = row->phys_ascent = 0;
      row->extra_line_spacing = 0;
    }

  row->full_width_p = 1;
  row->continued_p = 0;
  row->truncated_on_left_p = 0;
  row->truncated_on_right_p = 0;

  it->current_x = it->hpos = 0;
  it->current_y += row->height;
  ++it->vpos;
  ++it->glyph_row;
}


/* Max tool-bar height.  */

#define MAX_FRAME_TOOL_BAR_HEIGHT(f) \
  ((FRAME_LINE_HEIGHT (f) * FRAME_LINES (f)))

/* Value is the number of screen lines needed to make all tool-bar
   items of frame F visible.  The number of actual rows needed is
   returned in *N_ROWS if non-NULL.  */

static int
tool_bar_lines_needed (struct frame *f, int *n_rows)
{
  struct window *w = XWINDOW (f->tool_bar_window);
  struct it it;
  /* tool_bar_lines_needed is called from redisplay_tool_bar after building
     the desired matrix, so use (unused) mode-line row as temporary row to
     avoid destroying the first tool-bar row.  */
  struct glyph_row *temp_row = MATRIX_MODE_LINE_ROW (w->desired_matrix);

  /* Initialize an iterator for iteration over
     F->desired_tool_bar_string in the tool-bar window of frame F.  */
  init_iterator (&it, w, -1, -1, temp_row, TOOL_BAR_FACE_ID);
  it.first_visible_x = 0;
  it.last_visible_x = FRAME_TOTAL_COLS (f) * FRAME_COLUMN_WIDTH (f);
  reseat_to_string (&it, NULL, f->desired_tool_bar_string, 0, 0, 0, -1);
  it.paragraph_embedding = L2R;

  while (!ITERATOR_AT_END_P (&it))
    {
      clear_glyph_row (temp_row);
      it.glyph_row = temp_row;
      display_tool_bar_line (&it, -1);
    }
  clear_glyph_row (temp_row);

  /* f->n_tool_bar_rows == 0 means "unknown"; -1 means no tool-bar.  */
  if (n_rows)
    *n_rows = it.vpos > 0 ? it.vpos : -1;

  return (it.current_y + FRAME_LINE_HEIGHT (f) - 1) / FRAME_LINE_HEIGHT (f);
}


DEFUN ("tool-bar-lines-needed", Ftool_bar_lines_needed, Stool_bar_lines_needed,
       0, 1, 0,
       doc: /* Return the number of lines occupied by the tool bar of FRAME.  */)
  (Lisp_Object frame)
{
  struct frame *f;
  struct window *w;
  int nlines = 0;

  if (NILP (frame))
    frame = selected_frame;
  else
    CHECK_FRAME (frame);
  f = XFRAME (frame);

  if (WINDOWP (f->tool_bar_window)
      && (w = XWINDOW (f->tool_bar_window),
	  WINDOW_TOTAL_LINES (w) > 0))
    {
      update_tool_bar (f, 1);
      if (f->n_tool_bar_items)
	{
	  build_desired_tool_bar_string (f);
	  nlines = tool_bar_lines_needed (f, NULL);
	}
    }

  return make_number (nlines);
}


/* Display the tool-bar of frame F.  Value is non-zero if tool-bar's
   height should be changed.  */

static int
redisplay_tool_bar (struct frame *f)
{
  struct window *w;
  struct it it;
  struct glyph_row *row;

#if defined (USE_GTK) || defined (HAVE_NS)
  if (FRAME_EXTERNAL_TOOL_BAR (f))
    update_frame_tool_bar (f);
  return 0;
#endif

  /* If frame hasn't a tool-bar window or if it is zero-height, don't
     do anything.  This means you must start with tool-bar-lines
     non-zero to get the auto-sizing effect.  Or in other words, you
     can turn off tool-bars by specifying tool-bar-lines zero.  */
  if (!WINDOWP (f->tool_bar_window)
      || (w = XWINDOW (f->tool_bar_window),
          WINDOW_TOTAL_LINES (w) == 0))
    return 0;

  /* Set up an iterator for the tool-bar window.  */
  init_iterator (&it, w, -1, -1, w->desired_matrix->rows, TOOL_BAR_FACE_ID);
  it.first_visible_x = 0;
  it.last_visible_x = FRAME_TOTAL_COLS (f) * FRAME_COLUMN_WIDTH (f);
  row = it.glyph_row;

  /* Build a string that represents the contents of the tool-bar.  */
  build_desired_tool_bar_string (f);
  reseat_to_string (&it, NULL, f->desired_tool_bar_string, 0, 0, 0, -1);
  /* FIXME: This should be controlled by a user option.  But it
     doesn't make sense to have an R2L tool bar if the menu bar cannot
     be drawn also R2L, and making the menu bar R2L is tricky due
     toolkit-specific code that implements it.  If an R2L tool bar is
     ever supported, display_tool_bar_line should also be augmented to
     call unproduce_glyphs like display_line and display_string
     do.  */
  it.paragraph_embedding = L2R;

  if (f->n_tool_bar_rows == 0)
    {
      int nlines;

      if ((nlines = tool_bar_lines_needed (f, &f->n_tool_bar_rows),
	   nlines != WINDOW_TOTAL_LINES (w)))
	{
	  Lisp_Object frame;
	  int old_height = WINDOW_TOTAL_LINES (w);

	  XSETFRAME (frame, f);
	  Fmodify_frame_parameters (frame,
				    Fcons (Fcons (Qtool_bar_lines,
						  make_number (nlines)),
					   Qnil));
	  if (WINDOW_TOTAL_LINES (w) != old_height)
	    {
	      clear_glyph_matrix (w->desired_matrix);
	      fonts_changed_p = 1;
	      return 1;
	    }
	}
    }

  /* Display as many lines as needed to display all tool-bar items.  */

  if (f->n_tool_bar_rows > 0)
    {
      int border, rows, height, extra;

      if (INTEGERP (Vtool_bar_border))
	border = XINT (Vtool_bar_border);
      else if (EQ (Vtool_bar_border, Qinternal_border_width))
	border = FRAME_INTERNAL_BORDER_WIDTH (f);
      else if (EQ (Vtool_bar_border, Qborder_width))
	border = f->border_width;
      else
	border = 0;
      if (border < 0)
	border = 0;

      rows = f->n_tool_bar_rows;
      height = max (1, (it.last_visible_y - border) / rows);
      extra = it.last_visible_y - border - height * rows;

      while (it.current_y < it.last_visible_y)
	{
	  int h = 0;
	  if (extra > 0 && rows-- > 0)
	    {
	      h = (extra + rows - 1) / rows;
	      extra -= h;
	    }
	  display_tool_bar_line (&it, height + h);
	}
    }
  else
    {
      while (it.current_y < it.last_visible_y)
	display_tool_bar_line (&it, 0);
    }

  /* It doesn't make much sense to try scrolling in the tool-bar
     window, so don't do it.  */
  w->desired_matrix->no_scrolling_p = 1;
  w->must_be_updated_p = 1;

  if (!NILP (Vauto_resize_tool_bars))
    {
      int max_tool_bar_height = MAX_FRAME_TOOL_BAR_HEIGHT (f);
      int change_height_p = 0;

      /* If we couldn't display everything, change the tool-bar's
	 height if there is room for more.  */
      if (IT_STRING_CHARPOS (it) < it.end_charpos
	  && it.current_y < max_tool_bar_height)
	change_height_p = 1;

      row = it.glyph_row - 1;

      /* If there are blank lines at the end, except for a partially
	 visible blank line at the end that is smaller than
	 FRAME_LINE_HEIGHT, change the tool-bar's height.  */
      if (!row->displays_text_p
	  && row->height >= FRAME_LINE_HEIGHT (f))
	change_height_p = 1;

      /* If row displays tool-bar items, but is partially visible,
	 change the tool-bar's height.  */
      if (row->displays_text_p
	  && MATRIX_ROW_BOTTOM_Y (row) > it.last_visible_y
	  && MATRIX_ROW_BOTTOM_Y (row) < max_tool_bar_height)
	change_height_p = 1;

      /* Resize windows as needed by changing the `tool-bar-lines'
	 frame parameter.  */
      if (change_height_p)
	{
	  Lisp_Object frame;
	  int old_height = WINDOW_TOTAL_LINES (w);
	  int nrows;
	  int nlines = tool_bar_lines_needed (f, &nrows);

	  change_height_p = ((EQ (Vauto_resize_tool_bars, Qgrow_only)
			      && !f->minimize_tool_bar_window_p)
			     ? (nlines > old_height)
			     : (nlines != old_height));
	  f->minimize_tool_bar_window_p = 0;

	  if (change_height_p)
	    {
	      XSETFRAME (frame, f);
	      Fmodify_frame_parameters (frame,
					Fcons (Fcons (Qtool_bar_lines,
						      make_number (nlines)),
					       Qnil));
	      if (WINDOW_TOTAL_LINES (w) != old_height)
		{
		  clear_glyph_matrix (w->desired_matrix);
		  f->n_tool_bar_rows = nrows;
		  fonts_changed_p = 1;
		  return 1;
		}
	    }
	}
    }

  f->minimize_tool_bar_window_p = 0;
  return 0;
}


/* Get information about the tool-bar item which is displayed in GLYPH
   on frame F.  Return in *PROP_IDX the index where tool-bar item
   properties start in F->tool_bar_items.  Value is zero if
   GLYPH doesn't display a tool-bar item.  */

static int
tool_bar_item_info (struct frame *f, struct glyph *glyph, int *prop_idx)
{
  Lisp_Object prop;
  int success_p;
  int charpos;

  /* This function can be called asynchronously, which means we must
     exclude any possibility that Fget_text_property signals an
     error.  */
  charpos = min (SCHARS (f->current_tool_bar_string), glyph->charpos);
  charpos = max (0, charpos);

  /* Get the text property `menu-item' at pos. The value of that
     property is the start index of this item's properties in
     F->tool_bar_items.  */
  prop = Fget_text_property (make_number (charpos),
			     Qmenu_item, f->current_tool_bar_string);
  if (INTEGERP (prop))
    {
      *prop_idx = XINT (prop);
      success_p = 1;
    }
  else
    success_p = 0;

  return success_p;
}


/* Get information about the tool-bar item at position X/Y on frame F.
   Return in *GLYPH a pointer to the glyph of the tool-bar item in
   the current matrix of the tool-bar window of F, or NULL if not
   on a tool-bar item.  Return in *PROP_IDX the index of the tool-bar
   item in F->tool_bar_items.  Value is

   -1	if X/Y is not on a tool-bar item
   0	if X/Y is on the same item that was highlighted before.
   1	otherwise.  */

static int
get_tool_bar_item (struct frame *f, int x, int y, struct glyph **glyph,
		   int *hpos, int *vpos, int *prop_idx)
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
  struct window *w = XWINDOW (f->tool_bar_window);
  int area;

  /* Find the glyph under X/Y.  */
  *glyph = x_y_to_hpos_vpos (w, x, y, hpos, vpos, 0, 0, &area);
  if (*glyph == NULL)
    return -1;

  /* Get the start of this tool-bar item's properties in
     f->tool_bar_items.  */
  if (!tool_bar_item_info (f, *glyph, prop_idx))
    return -1;

  /* Is mouse on the highlighted item?  */
  if (EQ (f->tool_bar_window, hlinfo->mouse_face_window)
      && *vpos >= hlinfo->mouse_face_beg_row
      && *vpos <= hlinfo->mouse_face_end_row
      && (*vpos > hlinfo->mouse_face_beg_row
	  || *hpos >= hlinfo->mouse_face_beg_col)
      && (*vpos < hlinfo->mouse_face_end_row
	  || *hpos < hlinfo->mouse_face_end_col
	  || hlinfo->mouse_face_past_end))
    return 0;

  return 1;
}


/* EXPORT:
   Handle mouse button event on the tool-bar of frame F, at
   frame-relative coordinates X/Y.  DOWN_P is 1 for a button press,
   0 for button release.  MODIFIERS is event modifiers for button
   release.  */

void
handle_tool_bar_click (struct frame *f, int x, int y, int down_p,
		       unsigned int modifiers)
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
  struct window *w = XWINDOW (f->tool_bar_window);
  int hpos, vpos, prop_idx;
  struct glyph *glyph;
  Lisp_Object enabled_p;

  /* If not on the highlighted tool-bar item, return.  */
  frame_to_window_pixel_xy (w, &x, &y);
  if (get_tool_bar_item (f, x, y, &glyph, &hpos, &vpos, &prop_idx) != 0)
    return;

  /* If item is disabled, do nothing.  */
  enabled_p = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_ENABLED_P);
  if (NILP (enabled_p))
    return;

  if (down_p)
    {
      /* Show item in pressed state.  */
      show_mouse_face (hlinfo, DRAW_IMAGE_SUNKEN);
      hlinfo->mouse_face_image_state = DRAW_IMAGE_SUNKEN;
      last_tool_bar_item = prop_idx;
    }
  else
    {
      Lisp_Object key, frame;
      struct input_event event;
      EVENT_INIT (event);

      /* Show item in released state.  */
      show_mouse_face (hlinfo, DRAW_IMAGE_RAISED);
      hlinfo->mouse_face_image_state = DRAW_IMAGE_RAISED;

      key = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_KEY);

      XSETFRAME (frame, f);
      event.kind = TOOL_BAR_EVENT;
      event.frame_or_window = frame;
      event.arg = frame;
      kbd_buffer_store_event (&event);

      event.kind = TOOL_BAR_EVENT;
      event.frame_or_window = frame;
      event.arg = key;
      event.modifiers = modifiers;
      kbd_buffer_store_event (&event);
      last_tool_bar_item = -1;
    }
}


/* Possibly highlight a tool-bar item on frame F when mouse moves to
   tool-bar window-relative coordinates X/Y.  Called from
   note_mouse_highlight.  */

static void
note_tool_bar_highlight (struct frame *f, int x, int y)
{
  Lisp_Object window = f->tool_bar_window;
  struct window *w = XWINDOW (window);
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
  int hpos, vpos;
  struct glyph *glyph;
  struct glyph_row *row;
  int i;
  Lisp_Object enabled_p;
  int prop_idx;
  enum draw_glyphs_face draw = DRAW_IMAGE_RAISED;
  int mouse_down_p, rc;

  /* Function note_mouse_highlight is called with negative X/Y
     values when mouse moves outside of the frame.  */
  if (x <= 0 || y <= 0)
    {
      clear_mouse_face (hlinfo);
      return;
    }

  rc = get_tool_bar_item (f, x, y, &glyph, &hpos, &vpos, &prop_idx);
  if (rc < 0)
    {
      /* Not on tool-bar item.  */
      clear_mouse_face (hlinfo);
      return;
    }
  else if (rc == 0)
    /* On same tool-bar item as before.  */
    goto set_help_echo;

  clear_mouse_face (hlinfo);

  /* Mouse is down, but on different tool-bar item?  */
  mouse_down_p = (dpyinfo->grabbed
		  && f == last_mouse_frame
		  && FRAME_LIVE_P (f));
  if (mouse_down_p
      && last_tool_bar_item != prop_idx)
    return;

  hlinfo->mouse_face_image_state = DRAW_NORMAL_TEXT;
  draw = mouse_down_p ? DRAW_IMAGE_SUNKEN : DRAW_IMAGE_RAISED;

  /* If tool-bar item is not enabled, don't highlight it.  */
  enabled_p = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_ENABLED_P);
  if (!NILP (enabled_p))
    {
      /* Compute the x-position of the glyph.  In front and past the
	 image is a space.  We include this in the highlighted area.  */
      row = MATRIX_ROW (w->current_matrix, vpos);
      for (i = x = 0; i < hpos; ++i)
	x += row->glyphs[TEXT_AREA][i].pixel_width;

      /* Record this as the current active region.  */
      hlinfo->mouse_face_beg_col = hpos;
      hlinfo->mouse_face_beg_row = vpos;
      hlinfo->mouse_face_beg_x = x;
      hlinfo->mouse_face_beg_y = row->y;
      hlinfo->mouse_face_past_end = 0;

      hlinfo->mouse_face_end_col = hpos + 1;
      hlinfo->mouse_face_end_row = vpos;
      hlinfo->mouse_face_end_x = x + glyph->pixel_width;
      hlinfo->mouse_face_end_y = row->y;
      hlinfo->mouse_face_window = window;
      hlinfo->mouse_face_face_id = TOOL_BAR_FACE_ID;

      /* Display it as active.  */
      show_mouse_face (hlinfo, draw);
      hlinfo->mouse_face_image_state = draw;
    }

 set_help_echo:

  /* Set help_echo_string to a help string to display for this tool-bar item.
     XTread_socket does the rest.  */
  help_echo_object = help_echo_window = Qnil;
  help_echo_pos = -1;
  help_echo_string = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_HELP);
  if (NILP (help_echo_string))
    help_echo_string = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_CAPTION);
}

#endif /* HAVE_WINDOW_SYSTEM */



/************************************************************************
			 Horizontal scrolling
 ************************************************************************/

static int hscroll_window_tree (Lisp_Object);
static int hscroll_windows (Lisp_Object);

/* For all leaf windows in the window tree rooted at WINDOW, set their
   hscroll value so that PT is (i) visible in the window, and (ii) so
   that it is not within a certain margin at the window's left and
   right border.  Value is non-zero if any window's hscroll has been
   changed.  */

static int
hscroll_window_tree (Lisp_Object window)
{
  int hscrolled_p = 0;
  int hscroll_relative_p = FLOATP (Vhscroll_step);
  int hscroll_step_abs = 0;
  double hscroll_step_rel = 0;

  if (hscroll_relative_p)
    {
      hscroll_step_rel = XFLOAT_DATA (Vhscroll_step);
      if (hscroll_step_rel < 0)
	{
	  hscroll_relative_p = 0;
	  hscroll_step_abs = 0;
	}
    }
  else if (INTEGERP (Vhscroll_step))
    {
      hscroll_step_abs = XINT (Vhscroll_step);
      if (hscroll_step_abs < 0)
	hscroll_step_abs = 0;
    }
  else
    hscroll_step_abs = 0;

  while (WINDOWP (window))
    {
      struct window *w = XWINDOW (window);

      if (WINDOWP (w->hchild))
	hscrolled_p |= hscroll_window_tree (w->hchild);
      else if (WINDOWP (w->vchild))
	hscrolled_p |= hscroll_window_tree (w->vchild);
      else if (w->cursor.vpos >= 0)
	{
	  int h_margin;
	  int text_area_width;
	  struct glyph_row *current_cursor_row
	    = MATRIX_ROW (w->current_matrix, w->cursor.vpos);
	  struct glyph_row *desired_cursor_row
	    = MATRIX_ROW (w->desired_matrix, w->cursor.vpos);
	  struct glyph_row *cursor_row
	    = (desired_cursor_row->enabled_p
	       ? desired_cursor_row
	       : current_cursor_row);
	  int row_r2l_p = cursor_row->reversed_p;

	  text_area_width = window_box_width (w, TEXT_AREA);

	  /* Scroll when cursor is inside this scroll margin.  */
	  h_margin = hscroll_margin * WINDOW_FRAME_COLUMN_WIDTH (w);

	  if (!NILP (Fbuffer_local_value (Qauto_hscroll_mode, w->buffer))
	      /* For left-to-right rows, hscroll when cursor is either
		 (i) inside the right hscroll margin, or (ii) if it is
		 inside the left margin and the window is already
		 hscrolled. */
	      && ((!row_r2l_p
		   && ((XFASTINT (w->hscroll)
			&& w->cursor.x <= h_margin)
		       || (cursor_row->enabled_p
			   && cursor_row->truncated_on_right_p
			   && (w->cursor.x >= text_area_width - h_margin))))
		  /* For right-to-left rows, the logic is similar,
		     except that rules for scrolling to left and right
		     are reversed.  E.g., if cursor.x <= h_margin, we
		     need to hscroll "to the right" unconditionally,
		     and that will scroll the screen to the left so as
		     to reveal the next portion of the row.  */
		  || (row_r2l_p
		      && ((cursor_row->enabled_p
			   /* FIXME: It is confusing to set the
			      truncated_on_right_p flag when R2L rows
			      are actually truncated on the left. */
			   && cursor_row->truncated_on_right_p
			   && w->cursor.x <= h_margin)
			  || (XFASTINT (w->hscroll)
			      && (w->cursor.x >= text_area_width - h_margin))))))
	    {
	      struct it it;
	      int hscroll;
	      struct buffer *saved_current_buffer;
	      EMACS_INT pt;
	      int wanted_x;

	      /* Find point in a display of infinite width.  */
	      saved_current_buffer = current_buffer;
	      current_buffer = XBUFFER (w->buffer);

	      if (w == XWINDOW (selected_window))
		pt = PT;
	      else
		{
		  pt = marker_position (w->pointm);
		  pt = max (BEGV, pt);
		  pt = min (ZV, pt);
		}

	      /* Move iterator to pt starting at cursor_row->start in
		 a line with infinite width.  */
	      init_to_row_start (&it, w, cursor_row);
	      it.last_visible_x = INFINITY;
	      move_it_in_display_line_to (&it, pt, -1, MOVE_TO_POS);
	      current_buffer = saved_current_buffer;

	      /* Position cursor in window.  */
	      if (!hscroll_relative_p && hscroll_step_abs == 0)
		hscroll = max (0, (it.current_x
				   - (ITERATOR_AT_END_OF_LINE_P (&it)
				      ? (text_area_width - 4 * FRAME_COLUMN_WIDTH (it.f))
				      : (text_area_width / 2))))
		    	  / FRAME_COLUMN_WIDTH (it.f);
	      else if ((!row_r2l_p
			&& w->cursor.x >= text_area_width - h_margin)
		       || (row_r2l_p && w->cursor.x <= h_margin))
		{
		  if (hscroll_relative_p)
		    wanted_x = text_area_width * (1 - hscroll_step_rel)
		      	       - h_margin;
		  else
		    wanted_x = text_area_width
		      	       - hscroll_step_abs * FRAME_COLUMN_WIDTH (it.f)
		      	       - h_margin;
		  hscroll
		    = max (0, it.current_x - wanted_x) / FRAME_COLUMN_WIDTH (it.f);
		}
	      else
		{
		  if (hscroll_relative_p)
		    wanted_x = text_area_width * hscroll_step_rel
		      	       + h_margin;
		  else
		    wanted_x = hscroll_step_abs * FRAME_COLUMN_WIDTH (it.f)
		      	       + h_margin;
		  hscroll
		    = max (0, it.current_x - wanted_x) / FRAME_COLUMN_WIDTH (it.f);
		}
	      hscroll = max (hscroll, XFASTINT (w->min_hscroll));

	      /* Don't prevent redisplay optimizations if hscroll
		 hasn't changed, as it will unnecessarily slow down
		 redisplay.  */
	      if (XFASTINT (w->hscroll) != hscroll)
		{
		  XBUFFER (w->buffer)->prevent_redisplay_optimizations_p = 1;
		  w->hscroll = make_number (hscroll);
		  hscrolled_p = 1;
		}
	    }
	}

      window = w->next;
    }

  /* Value is non-zero if hscroll of any leaf window has been changed.  */
  return hscrolled_p;
}


/* Set hscroll so that cursor is visible and not inside horizontal
   scroll margins for all windows in the tree rooted at WINDOW.  See
   also hscroll_window_tree above.  Value is non-zero if any window's
   hscroll has been changed.  If it has, desired matrices on the frame
   of WINDOW are cleared.  */

static int
hscroll_windows (Lisp_Object window)
{
  int hscrolled_p = hscroll_window_tree (window);
  if (hscrolled_p)
    clear_desired_matrices (XFRAME (WINDOW_FRAME (XWINDOW (window))));
  return hscrolled_p;
}



/************************************************************************
				Redisplay
 ************************************************************************/

/* Variables holding some state of redisplay if GLYPH_DEBUG is defined
   to a non-zero value.  This is sometimes handy to have in a debugger
   session.  */

#if GLYPH_DEBUG

/* First and last unchanged row for try_window_id.  */

static int debug_first_unchanged_at_end_vpos;
static int debug_last_unchanged_at_beg_vpos;

/* Delta vpos and y.  */

static int debug_dvpos, debug_dy;

/* Delta in characters and bytes for try_window_id.  */

static EMACS_INT debug_delta, debug_delta_bytes;

/* Values of window_end_pos and window_end_vpos at the end of
   try_window_id.  */

static EMACS_INT debug_end_vpos;

/* Append a string to W->desired_matrix->method.  FMT is a printf
   format string.  If trace_redisplay_p is non-zero also printf the
   resulting string to stderr.  */

static void debug_method_add (struct window *, char const *, ...)
  ATTRIBUTE_FORMAT_PRINTF (2, 3);

static void
debug_method_add (struct window *w, char const *fmt, ...)
{
  char buffer[512];
  char *method = w->desired_matrix->method;
  int len = strlen (method);
  int size = sizeof w->desired_matrix->method;
  int remaining = size - len - 1;
  va_list ap;

  va_start (ap, fmt);
  vsprintf (buffer, fmt, ap);
  va_end (ap);
  if (len && remaining)
    {
      method[len] = '|';
      --remaining, ++len;
    }

  strncpy (method + len, buffer, remaining);

  if (trace_redisplay_p)
    fprintf (stderr, "%p (%s): %s\n",
	     w,
	     ((BUFFERP (w->buffer)
	       && STRINGP (BVAR (XBUFFER (w->buffer), name)))
	      ? SSDATA (BVAR (XBUFFER (w->buffer), name))
	      : "no buffer"),
	     buffer);
}

#endif /* GLYPH_DEBUG */


/* Value is non-zero if all changes in window W, which displays
   current_buffer, are in the text between START and END.  START is a
   buffer position, END is given as a distance from Z.  Used in
   redisplay_internal for display optimization.  */

static inline int
text_outside_line_unchanged_p (struct window *w,
			       EMACS_INT start, EMACS_INT end)
{
  int unchanged_p = 1;

  /* If text or overlays have changed, see where.  */
  if (XFASTINT (w->last_modified) < MODIFF
      || XFASTINT (w->last_overlay_modified) < OVERLAY_MODIFF)
    {
      /* Gap in the line?  */
      if (GPT < start || Z - GPT < end)
	unchanged_p = 0;

      /* Changes start in front of the line, or end after it?  */
      if (unchanged_p
	  && (BEG_UNCHANGED < start - 1
	      || END_UNCHANGED < end))
	unchanged_p = 0;

      /* If selective display, can't optimize if changes start at the
	 beginning of the line.  */
      if (unchanged_p
	  && INTEGERP (BVAR (current_buffer, selective_display))
	  && XINT (BVAR (current_buffer, selective_display)) > 0
	  && (BEG_UNCHANGED < start || GPT <= start))
	unchanged_p = 0;

      /* If there are overlays at the start or end of the line, these
	 may have overlay strings with newlines in them.  A change at
	 START, for instance, may actually concern the display of such
	 overlay strings as well, and they are displayed on different
	 lines.  So, quickly rule out this case.  (For the future, it
	 might be desirable to implement something more telling than
	 just BEG/END_UNCHANGED.)  */
      if (unchanged_p)
	{
	  if (BEG + BEG_UNCHANGED == start
	      && overlay_touches_p (start))
	    unchanged_p = 0;
	  if (END_UNCHANGED == end
	      && overlay_touches_p (Z - end))
	    unchanged_p = 0;
	}

      /* Under bidi reordering, adding or deleting a character in the
	 beginning of a paragraph, before the first strong directional
	 character, can change the base direction of the paragraph (unless
	 the buffer specifies a fixed paragraph direction), which will
	 require to redisplay the whole paragraph.  It might be worthwhile
	 to find the paragraph limits and widen the range of redisplayed
	 lines to that, but for now just give up this optimization.  */
      if (!NILP (BVAR (XBUFFER (w->buffer), bidi_display_reordering))
	  && NILP (BVAR (XBUFFER (w->buffer), bidi_paragraph_direction)))
	unchanged_p = 0;
    }

  return unchanged_p;
}


/* Do a frame update, taking possible shortcuts into account.  This is
   the main external entry point for redisplay.

   If the last redisplay displayed an echo area message and that message
   is no longer requested, we clear the echo area or bring back the
   mini-buffer if that is in use.  */

void
redisplay (void)
{
  redisplay_internal ();
}


static Lisp_Object
overlay_arrow_string_or_property (Lisp_Object var)
{
  Lisp_Object val;

  if (val = Fget (var, Qoverlay_arrow_string), STRINGP (val))
    return val;

  return Voverlay_arrow_string;
}

/* Return 1 if there are any overlay-arrows in current_buffer.  */
static int
overlay_arrow_in_current_buffer_p (void)
{
  Lisp_Object vlist;

  for (vlist = Voverlay_arrow_variable_list;
       CONSP (vlist);
       vlist = XCDR (vlist))
    {
      Lisp_Object var = XCAR (vlist);
      Lisp_Object val;

      if (!SYMBOLP (var))
	continue;
      val = find_symbol_value (var);
      if (MARKERP (val)
	  && current_buffer == XMARKER (val)->buffer)
	return 1;
    }
  return 0;
}


/* Return 1 if any overlay_arrows have moved or overlay-arrow-string
   has changed.  */

static int
overlay_arrows_changed_p (void)
{
  Lisp_Object vlist;

  for (vlist = Voverlay_arrow_variable_list;
       CONSP (vlist);
       vlist = XCDR (vlist))
    {
      Lisp_Object var = XCAR (vlist);
      Lisp_Object val, pstr;

      if (!SYMBOLP (var))
	continue;
      val = find_symbol_value (var);
      if (!MARKERP (val))
	continue;
      if (! EQ (COERCE_MARKER (val),
		Fget (var, Qlast_arrow_position))
	  || ! (pstr = overlay_arrow_string_or_property (var),
		EQ (pstr, Fget (var, Qlast_arrow_string))))
	return 1;
    }
  return 0;
}

/* Mark overlay arrows to be updated on next redisplay.  */

static void
update_overlay_arrows (int up_to_date)
{
  Lisp_Object vlist;

  for (vlist = Voverlay_arrow_variable_list;
       CONSP (vlist);
       vlist = XCDR (vlist))
    {
      Lisp_Object var = XCAR (vlist);

      if (!SYMBOLP (var))
	continue;

      if (up_to_date > 0)
	{
	  Lisp_Object val = find_symbol_value (var);
	  Fput (var, Qlast_arrow_position,
		COERCE_MARKER (val));
	  Fput (var, Qlast_arrow_string,
		overlay_arrow_string_or_property (var));
	}
      else if (up_to_date < 0
	       || !NILP (Fget (var, Qlast_arrow_position)))
	{
	  Fput (var, Qlast_arrow_position, Qt);
	  Fput (var, Qlast_arrow_string, Qt);
	}
    }
}


/* Return overlay arrow string to display at row.
   Return integer (bitmap number) for arrow bitmap in left fringe.
   Return nil if no overlay arrow.  */

static Lisp_Object
overlay_arrow_at_row (struct it *it, struct glyph_row *row)
{
  Lisp_Object vlist;

  for (vlist = Voverlay_arrow_variable_list;
       CONSP (vlist);
       vlist = XCDR (vlist))
    {
      Lisp_Object var = XCAR (vlist);
      Lisp_Object val;

      if (!SYMBOLP (var))
	continue;

      val = find_symbol_value (var);

      if (MARKERP (val)
	  && current_buffer == XMARKER (val)->buffer
	  && (MATRIX_ROW_START_CHARPOS (row) == marker_position (val)))
	{
	  if (FRAME_WINDOW_P (it->f)
	      /* FIXME: if ROW->reversed_p is set, this should test
		 the right fringe, not the left one.  */
	      && WINDOW_LEFT_FRINGE_WIDTH (it->w) > 0)
	    {
#ifdef HAVE_WINDOW_SYSTEM
	      if (val = Fget (var, Qoverlay_arrow_bitmap), SYMBOLP (val))
		{
		  int fringe_bitmap;
		  if ((fringe_bitmap = lookup_fringe_bitmap (val)) != 0)
		    return make_number (fringe_bitmap);
		}
#endif
	      return make_number (-1); /* Use default arrow bitmap */
	    }
	  return overlay_arrow_string_or_property (var);
	}
    }

  return Qnil;
}

/* Return 1 if point moved out of or into a composition.  Otherwise
   return 0.  PREV_BUF and PREV_PT are the last point buffer and
   position.  BUF and PT are the current point buffer and position.  */

static int
check_point_in_composition (struct buffer *prev_buf, EMACS_INT prev_pt,
			    struct buffer *buf, EMACS_INT pt)
{
  EMACS_INT start, end;
  Lisp_Object prop;
  Lisp_Object buffer;

  XSETBUFFER (buffer, buf);
  /* Check a composition at the last point if point moved within the
     same buffer.  */
  if (prev_buf == buf)
    {
      if (prev_pt == pt)
	/* Point didn't move.  */
	return 0;

      if (prev_pt > BUF_BEGV (buf) && prev_pt < BUF_ZV (buf)
	  && find_composition (prev_pt, -1, &start, &end, &prop, buffer)
	  && COMPOSITION_VALID_P (start, end, prop)
	  && start < prev_pt && end > prev_pt)
	/* The last point was within the composition.  Return 1 iff
            point moved out of the composition.  */
	return (pt <= start || pt >= end);
    }

  /* Check a composition at the current point.  */
  return (pt > BUF_BEGV (buf) && pt < BUF_ZV (buf)
	  && find_composition (pt, -1, &start, &end, &prop, buffer)
	  && COMPOSITION_VALID_P (start, end, prop)
	  && start < pt && end > pt);
}


/* Reconsider the setting of B->clip_changed which is displayed
   in window W.  */

static inline void
reconsider_clip_changes (struct window *w, struct buffer *b)
{
  if (b->clip_changed
	   && !NILP (w->window_end_valid)
	   && w->current_matrix->buffer == b
	   && w->current_matrix->zv == BUF_ZV (b)
	   && w->current_matrix->begv == BUF_BEGV (b))
    b->clip_changed = 0;

  /* If display wasn't paused, and W is not a tool bar window, see if
     point has been moved into or out of a composition.  In that case,
     we set b->clip_changed to 1 to force updating the screen.  If
     b->clip_changed has already been set to 1, we can skip this
     check.  */
  if (!b->clip_changed
      && BUFFERP (w->buffer) && !NILP (w->window_end_valid))
    {
      EMACS_INT pt;

      if (w == XWINDOW (selected_window))
	pt = PT;
      else
	pt = marker_position (w->pointm);

      if ((w->current_matrix->buffer != XBUFFER (w->buffer)
	   || pt != XINT (w->last_point))
	  && check_point_in_composition (w->current_matrix->buffer,
					 XINT (w->last_point),
					 XBUFFER (w->buffer), pt))
	b->clip_changed = 1;
    }
}


/* Select FRAME to forward the values of frame-local variables into C
   variables so that the redisplay routines can access those values
   directly.  */

static void
select_frame_for_redisplay (Lisp_Object frame)
{
  Lisp_Object tail, tem;
  Lisp_Object old = selected_frame;
  struct Lisp_Symbol *sym;

  xassert (FRAMEP (frame) && FRAME_LIVE_P (XFRAME (frame)));

  selected_frame = frame;

  do {
    for (tail = XFRAME (frame)->param_alist; CONSP (tail); tail = XCDR (tail))
      if (CONSP (XCAR (tail))
	  && (tem = XCAR (XCAR (tail)),
	      SYMBOLP (tem))
	  && (sym = indirect_variable (XSYMBOL (tem)),
	      sym->redirect == SYMBOL_LOCALIZED)
	  && sym->val.blv->frame_local)
	/* Use find_symbol_value rather than Fsymbol_value
	   to avoid an error if it is void.  */
	find_symbol_value (tem);
  } while (!EQ (frame, old) && (frame = old, 1));
}


#define STOP_POLLING					\
do { if (! polling_stopped_here) stop_polling ();	\
       polling_stopped_here = 1; } while (0)

#define RESUME_POLLING					\
do { if (polling_stopped_here) start_polling ();	\
       polling_stopped_here = 0; } while (0)


/* Perhaps in the future avoid recentering windows if it
   is not necessary; currently that causes some problems.  */

static void
redisplay_internal (void)
{
  struct window *w = XWINDOW (selected_window);
  struct window *sw;
  struct frame *fr;
  int pending;
  int must_finish = 0;
  struct text_pos tlbufpos, tlendpos;
  int number_of_visible_frames;
  int count, count1;
  struct frame *sf;
  int polling_stopped_here = 0;
  Lisp_Object old_frame = selected_frame;

  /* Non-zero means redisplay has to consider all windows on all
     frames.  Zero means, only selected_window is considered.  */
  int consider_all_windows_p;

  TRACE ((stderr, "redisplay_internal %d\n", redisplaying_p));

  /* No redisplay if running in batch mode or frame is not yet fully
     initialized, or redisplay is explicitly turned off by setting
     Vinhibit_redisplay.  */
  if (FRAME_INITIAL_P (SELECTED_FRAME ())
      || !NILP (Vinhibit_redisplay))
    return;

  /* Don't examine these until after testing Vinhibit_redisplay.
     When Emacs is shutting down, perhaps because its connection to
     X has dropped, we should not look at them at all.  */
  fr = XFRAME (w->frame);
  sf = SELECTED_FRAME ();

  if (!fr->glyphs_initialized_p)
    return;

#if defined (USE_X_TOOLKIT) || defined (USE_GTK) || defined (HAVE_NS)
  if (popup_activated ())
    return;
#endif

  /* I don't think this happens but let's be paranoid.  */
  if (redisplaying_p)
    return;

  /* Record a function that resets redisplaying_p to its old value
     when we leave this function.  */
  count = SPECPDL_INDEX ();
  record_unwind_protect (unwind_redisplay,
			 Fcons (make_number (redisplaying_p), selected_frame));
  ++redisplaying_p;
  specbind (Qinhibit_free_realized_faces, Qnil);

  {
    Lisp_Object tail, frame;

    FOR_EACH_FRAME (tail, frame)
      {
	struct frame *f = XFRAME (frame);
	f->already_hscrolled_p = 0;
      }
  }

 retry:
  /* Remember the currently selected window.  */
  sw = w;

  if (!EQ (old_frame, selected_frame)
      && FRAME_LIVE_P (XFRAME (old_frame)))
    /* When running redisplay, we play a bit fast-and-loose and allow e.g.
       selected_frame and selected_window to be temporarily out-of-sync so
       when we come back here via `goto retry', we need to resync because we
       may need to run Elisp code (via prepare_menu_bars).  */
    select_frame_for_redisplay (old_frame);

  pending = 0;
  reconsider_clip_changes (w, current_buffer);
  last_escape_glyph_frame = NULL;
  last_escape_glyph_face_id = (1 << FACE_ID_BITS);
  last_glyphless_glyph_frame = NULL;
  last_glyphless_glyph_face_id = (1 << FACE_ID_BITS);

  /* If new fonts have been loaded that make a glyph matrix adjustment
     necessary, do it.  */
  if (fonts_changed_p)
    {
      adjust_glyphs (NULL);
      ++windows_or_buffers_changed;
      fonts_changed_p = 0;
    }

  /* If face_change_count is non-zero, init_iterator will free all
     realized faces, which includes the faces referenced from current
     matrices.  So, we can't reuse current matrices in this case.  */
  if (face_change_count)
    ++windows_or_buffers_changed;

  if ((FRAME_TERMCAP_P (sf) || FRAME_MSDOS_P (sf))
      && FRAME_TTY (sf)->previous_frame != sf)
    {
      /* Since frames on a single ASCII terminal share the same
	 display area, displaying a different frame means redisplay
	 the whole thing.  */
      windows_or_buffers_changed++;
      SET_FRAME_GARBAGED (sf);
#ifndef DOS_NT
      set_tty_color_mode (FRAME_TTY (sf), sf);
#endif
      FRAME_TTY (sf)->previous_frame = sf;
    }

  /* Set the visible flags for all frames.  Do this before checking
     for resized or garbaged frames; they want to know if their frames
     are visible.  See the comment in frame.h for
     FRAME_SAMPLE_VISIBILITY.  */
  {
    Lisp_Object tail, frame;

    number_of_visible_frames = 0;

    FOR_EACH_FRAME (tail, frame)
      {
	struct frame *f = XFRAME (frame);

	FRAME_SAMPLE_VISIBILITY (f);
	if (FRAME_VISIBLE_P (f))
	  ++number_of_visible_frames;
	clear_desired_matrices (f);
      }
  }

  /* Notice any pending interrupt request to change frame size.  */
  do_pending_window_change (1);

  /* do_pending_window_change could change the selected_window due to
     frame resizing which makes the selected window too small.  */
  if (WINDOWP (selected_window) && (w = XWINDOW (selected_window)) != sw)
    {
      sw = w;
      reconsider_clip_changes (w, current_buffer);
    }

  /* Clear frames marked as garbaged.  */
  if (frame_garbaged)
    clear_garbaged_frames ();

  /* Build menubar and tool-bar items.  */
  if (NILP (Vmemory_full))
    prepare_menu_bars ();

  if (windows_or_buffers_changed)
    update_mode_lines++;

  /* Detect case that we need to write or remove a star in the mode line.  */
  if ((SAVE_MODIFF < MODIFF) != !NILP (w->last_had_star))
    {
      w->update_mode_line = Qt;
      if (buffer_shared > 1)
	update_mode_lines++;
    }

  /* Avoid invocation of point motion hooks by `current_column' below.  */
  count1 = SPECPDL_INDEX ();
  specbind (Qinhibit_point_motion_hooks, Qt);

  /* If %c is in the mode line, update it if needed.  */
  if (!NILP (w->column_number_displayed)
      /* This alternative quickly identifies a common case
	 where no change is needed.  */
      && !(PT == XFASTINT (w->last_point)
	   && XFASTINT (w->last_modified) >= MODIFF
	   && XFASTINT (w->last_overlay_modified) >= OVERLAY_MODIFF)
      && (XFASTINT (w->column_number_displayed) != current_column ()))
    w->update_mode_line = Qt;

  unbind_to (count1, Qnil);

  FRAME_SCROLL_BOTTOM_VPOS (XFRAME (w->frame)) = -1;

  /* The variable buffer_shared is set in redisplay_window and
     indicates that we redisplay a buffer in different windows.  See
     there.  */
  consider_all_windows_p = (update_mode_lines || buffer_shared > 1
			    || cursor_type_changed);

  /* If specs for an arrow have changed, do thorough redisplay
     to ensure we remove any arrow that should no longer exist.  */
  if (overlay_arrows_changed_p ())
    consider_all_windows_p = windows_or_buffers_changed = 1;

  /* Normally the message* functions will have already displayed and
     updated the echo area, but the frame may have been trashed, or
     the update may have been preempted, so display the echo area
     again here.  Checking message_cleared_p captures the case that
     the echo area should be cleared.  */
  if ((!NILP (echo_area_buffer[0]) && !display_last_displayed_message_p)
      || (!NILP (echo_area_buffer[1]) && display_last_displayed_message_p)
      || (message_cleared_p
	  && minibuf_level == 0
	  /* If the mini-window is currently selected, this means the
	     echo-area doesn't show through.  */
	  && !MINI_WINDOW_P (XWINDOW (selected_window))))
    {
      int window_height_changed_p = echo_area_display (0);
      must_finish = 1;

      /* If we don't display the current message, don't clear the
	 message_cleared_p flag, because, if we did, we wouldn't clear
	 the echo area in the next redisplay which doesn't preserve
	 the echo area.  */
      if (!display_last_displayed_message_p)
	message_cleared_p = 0;

      if (fonts_changed_p)
	goto retry;
      else if (window_height_changed_p)
	{
	  consider_all_windows_p = 1;
	  ++update_mode_lines;
	  ++windows_or_buffers_changed;

	  /* If window configuration was changed, frames may have been
	     marked garbaged.  Clear them or we will experience
	     surprises wrt scrolling.  */
	  if (frame_garbaged)
	    clear_garbaged_frames ();
	}
    }
  else if (EQ (selected_window, minibuf_window)
	   && (current_buffer->clip_changed
	       || XFASTINT (w->last_modified) < MODIFF
	       || XFASTINT (w->last_overlay_modified) < OVERLAY_MODIFF)
	   && resize_mini_window (w, 0))
    {
      /* Resized active mini-window to fit the size of what it is
         showing if its contents might have changed.  */
      must_finish = 1;
/* FIXME: this causes all frames to be updated, which seems unnecessary
   since only the current frame needs to be considered.  This function needs
   to be rewritten with two variables, consider_all_windows and
   consider_all_frames. */
      consider_all_windows_p = 1;
      ++windows_or_buffers_changed;
      ++update_mode_lines;

      /* If window configuration was changed, frames may have been
	 marked garbaged.  Clear them or we will experience
	 surprises wrt scrolling.  */
      if (frame_garbaged)
	clear_garbaged_frames ();
    }


  /* If showing the region, and mark has changed, we must redisplay
     the whole window.  The assignment to this_line_start_pos prevents
     the optimization directly below this if-statement.  */
  if (((!NILP (Vtransient_mark_mode)
	&& !NILP (BVAR (XBUFFER (w->buffer), mark_active)))
       != !NILP (w->region_showing))
      || (!NILP (w->region_showing)
	  && !EQ (w->region_showing,
		  Fmarker_position (BVAR (XBUFFER (w->buffer), mark)))))
    CHARPOS (this_line_start_pos) = 0;

  /* Optimize the case that only the line containing the cursor in the
     selected window has changed.  Variables starting with this_ are
     set in display_line and record information about the line
     containing the cursor.  */
  tlbufpos = this_line_start_pos;
  tlendpos = this_line_end_pos;
  if (!consider_all_windows_p
      && CHARPOS (tlbufpos) > 0
      && NILP (w->update_mode_line)
      && !current_buffer->clip_changed
      && !current_buffer->prevent_redisplay_optimizations_p
      && FRAME_VISIBLE_P (XFRAME (w->frame))
      && !FRAME_OBSCURED_P (XFRAME (w->frame))
      /* Make sure recorded data applies to current buffer, etc.  */
      && this_line_buffer == current_buffer
      && current_buffer == XBUFFER (w->buffer)
      && NILP (w->force_start)
      && NILP (w->optional_new_start)
      /* Point must be on the line that we have info recorded about.  */
      && PT >= CHARPOS (tlbufpos)
      && PT <= Z - CHARPOS (tlendpos)
      /* All text outside that line, including its final newline,
	 must be unchanged.  */
      && text_outside_line_unchanged_p (w, CHARPOS (tlbufpos),
					CHARPOS (tlendpos)))
    {
      if (CHARPOS (tlbufpos) > BEGV
	  && FETCH_BYTE (BYTEPOS (tlbufpos) - 1) != '\n'
	  && (CHARPOS (tlbufpos) == ZV
	      || FETCH_BYTE (BYTEPOS (tlbufpos)) == '\n'))
	/* Former continuation line has disappeared by becoming empty.  */
	goto cancel;
      else if (XFASTINT (w->last_modified) < MODIFF
	       || XFASTINT (w->last_overlay_modified) < OVERLAY_MODIFF
	       || MINI_WINDOW_P (w))
	{
	  /* We have to handle the case of continuation around a
	     wide-column character (see the comment in indent.c around
	     line 1340).

	     For instance, in the following case:

	     --------  Insert  --------
	     K_A_N_\\   `a'    K_A_N_a\		`X_' are wide-column chars.
	     J_I_       ==>    J_I_		`^^' are cursors.
	     ^^                ^^
	     --------          --------

	     As we have to redraw the line above, we cannot use this
	     optimization.  */

	  struct it it;
	  int line_height_before = this_line_pixel_height;

	  /* Note that start_display will handle the case that the
	     line starting at tlbufpos is a continuation line.  */
	  start_display (&it, w, tlbufpos);

	  /* Implementation note: It this still necessary?  */
	  if (it.current_x != this_line_start_x)
	    goto cancel;

	  TRACE ((stderr, "trying display optimization 1\n"));
	  w->cursor.vpos = -1;
	  overlay_arrow_seen = 0;
	  it.vpos = this_line_vpos;
	  it.current_y = this_line_y;
	  it.glyph_row = MATRIX_ROW (w->desired_matrix, this_line_vpos);
	  display_line (&it);

	  /* If line contains point, is not continued,
             and ends at same distance from eob as before, we win.  */
	  if (w->cursor.vpos >= 0
              /* Line is not continued, otherwise this_line_start_pos
                 would have been set to 0 in display_line.  */
	      && CHARPOS (this_line_start_pos)
	      /* Line ends as before.  */
	      && CHARPOS (this_line_end_pos) == CHARPOS (tlendpos)
              /* Line has same height as before.  Otherwise other lines
                 would have to be shifted up or down.  */
	      && this_line_pixel_height == line_height_before)
	    {
 	      /* If this is not the window's last line, we must adjust
 		 the charstarts of the lines below.  */
 	      if (it.current_y < it.last_visible_y)
  		{
 		  struct glyph_row *row
 		    = MATRIX_ROW (w->current_matrix, this_line_vpos + 1);
  		  EMACS_INT delta, delta_bytes;

		  /* We used to distinguish between two cases here,
		     conditioned by Z - CHARPOS (tlendpos) == ZV, for
		     when the line ends in a newline or the end of the
		     buffer's accessible portion.  But both cases did
		     the same, so they were collapsed.  */
		  delta = (Z
			   - CHARPOS (tlendpos)
			   - MATRIX_ROW_START_CHARPOS (row));
		  delta_bytes = (Z_BYTE
				 - BYTEPOS (tlendpos)
				 - MATRIX_ROW_START_BYTEPOS (row));

  		  increment_matrix_positions (w->current_matrix,
					      this_line_vpos + 1,
					      w->current_matrix->nrows,
					      delta, delta_bytes);
		}

	      /* If this row displays text now but previously didn't,
		 or vice versa, w->window_end_vpos may have to be
		 adjusted.  */
	      if ((it.glyph_row - 1)->displays_text_p)
		{
		  if (XFASTINT (w->window_end_vpos) < this_line_vpos)
		    XSETINT (w->window_end_vpos, this_line_vpos);
		}
	      else if (XFASTINT (w->window_end_vpos) == this_line_vpos
		       && this_line_vpos > 0)
		XSETINT (w->window_end_vpos, this_line_vpos - 1);
	      w->window_end_valid = Qnil;

	      /* Update hint: No need to try to scroll in update_window.  */
	      w->desired_matrix->no_scrolling_p = 1;

#if GLYPH_DEBUG
	      *w->desired_matrix->method = 0;
	      debug_method_add (w, "optimization 1");
#endif
#ifdef HAVE_WINDOW_SYSTEM
	      update_window_fringes (w, 0);
#endif
	      goto update;
	    }
	  else
	    goto cancel;
	}
      else if (/* Cursor position hasn't changed.  */
	       PT == XFASTINT (w->last_point)
	       /* Make sure the cursor was last displayed
		  in this window.  Otherwise we have to reposition it.  */
	       && 0 <= w->cursor.vpos
	       && WINDOW_TOTAL_LINES (w) > w->cursor.vpos)
	{
	  if (!must_finish)
	    {
	      do_pending_window_change (1);
	      /* If selected_window changed, redisplay again.  */
	      if (WINDOWP (selected_window)
		  && (w = XWINDOW (selected_window)) != sw)
		goto retry;

	      /* We used to always goto end_of_redisplay here, but this
		 isn't enough if we have a blinking cursor.  */
	      if (w->cursor_off_p == w->last_cursor_off_p)
		goto end_of_redisplay;
	    }
	  goto update;
	}
      /* If highlighting the region, or if the cursor is in the echo area,
	 then we can't just move the cursor.  */
      else if (! (!NILP (Vtransient_mark_mode)
		  && !NILP (BVAR (current_buffer, mark_active)))
	       && (EQ (selected_window, BVAR (current_buffer, last_selected_window))
		   || highlight_nonselected_windows)
	       && NILP (w->region_showing)
	       && NILP (Vshow_trailing_whitespace)
	       && !cursor_in_echo_area)
	{
	  struct it it;
	  struct glyph_row *row;

	  /* Skip from tlbufpos to PT and see where it is.  Note that
	     PT may be in invisible text.  If so, we will end at the
	     next visible position.  */
	  init_iterator (&it, w, CHARPOS (tlbufpos), BYTEPOS (tlbufpos),
			 NULL, DEFAULT_FACE_ID);
	  it.current_x = this_line_start_x;
	  it.current_y = this_line_y;
	  it.vpos = this_line_vpos;

	  /* The call to move_it_to stops in front of PT, but
	     moves over before-strings.  */
	  move_it_to (&it, PT, -1, -1, -1, MOVE_TO_POS);

	  if (it.vpos == this_line_vpos
	      && (row = MATRIX_ROW (w->current_matrix, this_line_vpos),
		  row->enabled_p))
	    {
	      xassert (this_line_vpos == it.vpos);
	      xassert (this_line_y == it.current_y);
	      set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0);
#if GLYPH_DEBUG
	      *w->desired_matrix->method = 0;
	      debug_method_add (w, "optimization 3");
#endif
	      goto update;
	    }
	  else
	    goto cancel;
	}

    cancel:
      /* Text changed drastically or point moved off of line.  */
      SET_MATRIX_ROW_ENABLED_P (w->desired_matrix, this_line_vpos, 0);
    }

  CHARPOS (this_line_start_pos) = 0;
  consider_all_windows_p |= buffer_shared > 1;
  ++clear_face_cache_count;
#ifdef HAVE_WINDOW_SYSTEM
  ++clear_image_cache_count;
#endif

  /* Build desired matrices, and update the display.  If
     consider_all_windows_p is non-zero, do it for all windows on all
     frames.  Otherwise do it for selected_window, only.  */

  if (consider_all_windows_p)
    {
      Lisp_Object tail, frame;

      FOR_EACH_FRAME (tail, frame)
	XFRAME (frame)->updated_p = 0;

      /* Recompute # windows showing selected buffer.  This will be
	 incremented each time such a window is displayed.  */
      buffer_shared = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);

	  if (FRAME_WINDOW_P (f) || FRAME_TERMCAP_P (f) || f == sf)
	    {
	      if (! EQ (frame, selected_frame))
		/* Select the frame, for the sake of frame-local
		   variables.  */
		select_frame_for_redisplay (frame);

	      /* Mark all the scroll bars to be removed; we'll redeem
		 the ones we want when we redisplay their windows.  */
	      if (FRAME_TERMINAL (f)->condemn_scroll_bars_hook)
		FRAME_TERMINAL (f)->condemn_scroll_bars_hook (f);

	      if (FRAME_VISIBLE_P (f) && !FRAME_OBSCURED_P (f))
		redisplay_windows (FRAME_ROOT_WINDOW (f));

	      /* The X error handler may have deleted that frame.  */
	      if (!FRAME_LIVE_P (f))
		continue;

	      /* Any scroll bars which redisplay_windows should have
		 nuked should now go away.  */
	      if (FRAME_TERMINAL (f)->judge_scroll_bars_hook)
		FRAME_TERMINAL (f)->judge_scroll_bars_hook (f);

	      /* If fonts changed, display again.  */
	      /* ??? rms: I suspect it is a mistake to jump all the way
		 back to retry here.  It should just retry this frame.  */
	      if (fonts_changed_p)
		goto retry;

	      if (FRAME_VISIBLE_P (f) && !FRAME_OBSCURED_P (f))
		{
		  /* See if we have to hscroll.  */
		  if (!f->already_hscrolled_p)
		    {
		      f->already_hscrolled_p = 1;
		      if (hscroll_windows (f->root_window))
			goto retry;
		    }

		  /* Prevent various kinds of signals during display
		     update.  stdio is not robust about handling
		     signals, which can cause an apparent I/O
		     error.  */
		  if (interrupt_input)
		    unrequest_sigio ();
		  STOP_POLLING;

		  /* Update the display.  */
		  set_window_update_flags (XWINDOW (f->root_window), 1);
		  pending |= update_frame (f, 0, 0);
		  f->updated_p = 1;
		}
	    }
	}

      if (!EQ (old_frame, selected_frame)
	  && FRAME_LIVE_P (XFRAME (old_frame)))
	/* We played a bit fast-and-loose above and allowed selected_frame
	   and selected_window to be temporarily out-of-sync but let's make
	   sure this stays contained.  */
	select_frame_for_redisplay (old_frame);
      eassert (EQ (XFRAME (selected_frame)->selected_window, selected_window));

      if (!pending)
	{
	  /* Do the mark_window_display_accurate after all windows have
	     been redisplayed because this call resets flags in buffers
	     which are needed for proper redisplay.  */
	  FOR_EACH_FRAME (tail, frame)
	    {
	      struct frame *f = XFRAME (frame);
              if (f->updated_p)
                {
                  mark_window_display_accurate (f->root_window, 1);
                  if (FRAME_TERMINAL (f)->frame_up_to_date_hook)
                    FRAME_TERMINAL (f)->frame_up_to_date_hook (f);
                }
	    }
	}
    }
  else if (FRAME_VISIBLE_P (sf) && !FRAME_OBSCURED_P (sf))
    {
      Lisp_Object mini_window;
      struct frame *mini_frame;

      displayed_buffer = XBUFFER (XWINDOW (selected_window)->buffer);
      /* Use list_of_error, not Qerror, so that
	 we catch only errors and don't run the debugger.  */
      internal_condition_case_1 (redisplay_window_1, selected_window,
				 list_of_error,
				 redisplay_window_error);

      /* Compare desired and current matrices, perform output.  */

    update:
      /* If fonts changed, display again.  */
      if (fonts_changed_p)
	goto retry;

      /* Prevent various kinds of signals during display update.
	 stdio is not robust about handling signals,
	 which can cause an apparent I/O error.  */
      if (interrupt_input)
	unrequest_sigio ();
      STOP_POLLING;

      if (FRAME_VISIBLE_P (sf) && !FRAME_OBSCURED_P (sf))
	{
	  if (hscroll_windows (selected_window))
	    goto retry;

	  XWINDOW (selected_window)->must_be_updated_p = 1;
	  pending = update_frame (sf, 0, 0);
	}

      /* We may have called echo_area_display at the top of this
	 function.  If the echo area is on another frame, that may
	 have put text on a frame other than the selected one, so the
	 above call to update_frame would not have caught it.  Catch
	 it here.  */
      mini_window = FRAME_MINIBUF_WINDOW (sf);
      mini_frame = XFRAME (WINDOW_FRAME (XWINDOW (mini_window)));

      if (mini_frame != sf && FRAME_WINDOW_P (mini_frame))
	{
	  XWINDOW (mini_window)->must_be_updated_p = 1;
	  pending |= update_frame (mini_frame, 0, 0);
	  if (!pending && hscroll_windows (mini_window))
	    goto retry;
	}
    }

  /* If display was paused because of pending input, make sure we do a
     thorough update the next time.  */
  if (pending)
    {
      /* Prevent the optimization at the beginning of
	 redisplay_internal that tries a single-line update of the
	 line containing the cursor in the selected window.  */
      CHARPOS (this_line_start_pos) = 0;

      /* Let the overlay arrow be updated the next time.  */
      update_overlay_arrows (0);

      /* If we pause after scrolling, some rows in the current
	 matrices of some windows are not valid.  */
      if (!WINDOW_FULL_WIDTH_P (w)
	  && !FRAME_WINDOW_P (XFRAME (w->frame)))
	update_mode_lines = 1;
    }
  else
    {
      if (!consider_all_windows_p)
	{
	  /* This has already been done above if
	     consider_all_windows_p is set.  */
	  mark_window_display_accurate_1 (w, 1);

	  /* Say overlay arrows are up to date.  */
	  update_overlay_arrows (1);

	  if (FRAME_TERMINAL (sf)->frame_up_to_date_hook != 0)
	    FRAME_TERMINAL (sf)->frame_up_to_date_hook (sf);
	}

      update_mode_lines = 0;
      windows_or_buffers_changed = 0;
      cursor_type_changed = 0;
    }

  /* Start SIGIO interrupts coming again.  Having them off during the
     code above makes it less likely one will discard output, but not
     impossible, since there might be stuff in the system buffer here.
     But it is much hairier to try to do anything about that.  */
  if (interrupt_input)
    request_sigio ();
  RESUME_POLLING;

  /* If a frame has become visible which was not before, redisplay
     again, so that we display it.  Expose events for such a frame
     (which it gets when becoming visible) don't call the parts of
     redisplay constructing glyphs, so simply exposing a frame won't
     display anything in this case.  So, we have to display these
     frames here explicitly.  */
  if (!pending)
    {
      Lisp_Object tail, frame;
      int new_count = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  int this_is_visible = 0;

	  if (XFRAME (frame)->visible)
	    this_is_visible = 1;
	  FRAME_SAMPLE_VISIBILITY (XFRAME (frame));
	  if (XFRAME (frame)->visible)
	    this_is_visible = 1;

	  if (this_is_visible)
	    new_count++;
	}

      if (new_count != number_of_visible_frames)
	windows_or_buffers_changed++;
    }

  /* Change frame size now if a change is pending.  */
  do_pending_window_change (1);

  /* If we just did a pending size change, or have additional
     visible frames, or selected_window changed, redisplay again.  */
  if ((windows_or_buffers_changed && !pending)
      || (WINDOWP (selected_window) && (w = XWINDOW (selected_window)) != sw))
    goto retry;

  /* Clear the face and image caches.

     We used to do this only if consider_all_windows_p.  But the cache
     needs to be cleared if a timer creates images in the current
     buffer (e.g. the test case in Bug#6230).  */

  if (clear_face_cache_count > CLEAR_FACE_CACHE_COUNT)
    {
      clear_face_cache (0);
      clear_face_cache_count = 0;
    }

#ifdef HAVE_WINDOW_SYSTEM
  if (clear_image_cache_count > CLEAR_IMAGE_CACHE_COUNT)
    {
      clear_image_caches (Qnil);
      clear_image_cache_count = 0;
    }
#endif /* HAVE_WINDOW_SYSTEM */

 end_of_redisplay:
  unbind_to (count, Qnil);
  RESUME_POLLING;
}


/* Redisplay, but leave alone any recent echo area message unless
   another message has been requested in its place.

   This is useful in situations where you need to redisplay but no
   user action has occurred, making it inappropriate for the message
   area to be cleared.  See tracking_off and
   wait_reading_process_output for examples of these situations.

   FROM_WHERE is an integer saying from where this function was
   called.  This is useful for debugging.  */

void
redisplay_preserve_echo_area (int from_where)
{
  TRACE ((stderr, "redisplay_preserve_echo_area (%d)\n", from_where));

  if (!NILP (echo_area_buffer[1]))
    {
      /* We have a previously displayed message, but no current
	 message.  Redisplay the previous message.  */
      display_last_displayed_message_p = 1;
      redisplay_internal ();
      display_last_displayed_message_p = 0;
    }
  else
    redisplay_internal ();

  if (FRAME_RIF (SELECTED_FRAME ()) != NULL
      && FRAME_RIF (SELECTED_FRAME ())->flush_display_optional)
    FRAME_RIF (SELECTED_FRAME ())->flush_display_optional (NULL);
}


/* Function registered with record_unwind_protect in
   redisplay_internal.  Reset redisplaying_p to the value it had
   before redisplay_internal was called, and clear
   prevent_freeing_realized_faces_p.  It also selects the previously
   selected frame, unless it has been deleted (by an X connection
   failure during redisplay, for example).  */

static Lisp_Object
unwind_redisplay (Lisp_Object val)
{
  Lisp_Object old_redisplaying_p, old_frame;

  old_redisplaying_p = XCAR (val);
  redisplaying_p = XFASTINT (old_redisplaying_p);
  old_frame = XCDR (val);
  if (! EQ (old_frame, selected_frame)
      && FRAME_LIVE_P (XFRAME (old_frame)))
    select_frame_for_redisplay (old_frame);
  return Qnil;
}


/* Mark the display of window W as accurate or inaccurate.  If
   ACCURATE_P is non-zero mark display of W as accurate.  If
   ACCURATE_P is zero, arrange for W to be redisplayed the next time
   redisplay_internal is called.  */

static void
mark_window_display_accurate_1 (struct window *w, int accurate_p)
{
  if (BUFFERP (w->buffer))
    {
      struct buffer *b = XBUFFER (w->buffer);

      w->last_modified
	= make_number (accurate_p ? BUF_MODIFF (b) : 0);
      w->last_overlay_modified
	= make_number (accurate_p ? BUF_OVERLAY_MODIFF (b) : 0);
      w->last_had_star
	= BUF_MODIFF (b) > BUF_SAVE_MODIFF (b) ? Qt : Qnil;

      if (accurate_p)
	{
	  b->clip_changed = 0;
	  b->prevent_redisplay_optimizations_p = 0;

	  BUF_UNCHANGED_MODIFIED (b) = BUF_MODIFF (b);
	  BUF_OVERLAY_UNCHANGED_MODIFIED (b) = BUF_OVERLAY_MODIFF (b);
	  BUF_BEG_UNCHANGED (b) = BUF_GPT (b) - BUF_BEG (b);
	  BUF_END_UNCHANGED (b) = BUF_Z (b) - BUF_GPT (b);

	  w->current_matrix->buffer = b;
	  w->current_matrix->begv = BUF_BEGV (b);
	  w->current_matrix->zv = BUF_ZV (b);

	  w->last_cursor = w->cursor;
	  w->last_cursor_off_p = w->cursor_off_p;

	  if (w == XWINDOW (selected_window))
	    w->last_point = make_number (BUF_PT (b));
	  else
	    w->last_point = make_number (XMARKER (w->pointm)->charpos);
	}
    }

  if (accurate_p)
    {
      w->window_end_valid = w->buffer;
      w->update_mode_line = Qnil;
    }
}


/* Mark the display of windows in the window tree rooted at WINDOW as
   accurate or inaccurate.  If ACCURATE_P is non-zero mark display of
   windows as accurate.  If ACCURATE_P is zero, arrange for windows to
   be redisplayed the next time redisplay_internal is called.  */

void
mark_window_display_accurate (Lisp_Object window, int accurate_p)
{
  struct window *w;

  for (; !NILP (window); window = w->next)
    {
      w = XWINDOW (window);
      mark_window_display_accurate_1 (w, accurate_p);

      if (!NILP (w->vchild))
	mark_window_display_accurate (w->vchild, accurate_p);
      if (!NILP (w->hchild))
	mark_window_display_accurate (w->hchild, accurate_p);
    }

  if (accurate_p)
    {
      update_overlay_arrows (1);
    }
  else
    {
      /* Force a thorough redisplay the next time by setting
	 last_arrow_position and last_arrow_string to t, which is
	 unequal to any useful value of Voverlay_arrow_...  */
      update_overlay_arrows (-1);
    }
}


/* Return value in display table DP (Lisp_Char_Table *) for character
   C.  Since a display table doesn't have any parent, we don't have to
   follow parent.  Do not call this function directly but use the
   macro DISP_CHAR_VECTOR.  */

Lisp_Object
disp_char_vector (struct Lisp_Char_Table *dp, int c)
{
  Lisp_Object val;

  if (ASCII_CHAR_P (c))
    {
      val = dp->ascii;
      if (SUB_CHAR_TABLE_P (val))
	val = XSUB_CHAR_TABLE (val)->contents[c];
    }
  else
    {
      Lisp_Object table;

      XSETCHAR_TABLE (table, dp);
      val = char_table_ref (table, c);
    }
  if (NILP (val))
    val = dp->defalt;
  return val;
}



/***********************************************************************
			   Window Redisplay
 ***********************************************************************/

/* Redisplay all leaf windows in the window tree rooted at WINDOW.  */

static void
redisplay_windows (Lisp_Object window)
{
  while (!NILP (window))
    {
      struct window *w = XWINDOW (window);

      if (!NILP (w->hchild))
	redisplay_windows (w->hchild);
      else if (!NILP (w->vchild))
	redisplay_windows (w->vchild);
      else if (!NILP (w->buffer))
	{
	  displayed_buffer = XBUFFER (w->buffer);
	  /* Use list_of_error, not Qerror, so that
	     we catch only errors and don't run the debugger.  */
	  internal_condition_case_1 (redisplay_window_0, window,
				     list_of_error,
				     redisplay_window_error);
	}

      window = w->next;
    }
}

static Lisp_Object
redisplay_window_error (Lisp_Object ignore)
{
  displayed_buffer->display_error_modiff = BUF_MODIFF (displayed_buffer);
  return Qnil;
}

static Lisp_Object
redisplay_window_0 (Lisp_Object window)
{
  if (displayed_buffer->display_error_modiff < BUF_MODIFF (displayed_buffer))
    redisplay_window (window, 0);
  return Qnil;
}

static Lisp_Object
redisplay_window_1 (Lisp_Object window)
{
  if (displayed_buffer->display_error_modiff < BUF_MODIFF (displayed_buffer))
    redisplay_window (window, 1);
  return Qnil;
}


/* Set cursor position of W.  PT is assumed to be displayed in ROW.
   DELTA and DELTA_BYTES are the numbers of characters and bytes by
   which positions recorded in ROW differ from current buffer
   positions.

   Return 0 if cursor is not on this row, 1 otherwise.  */

static int
set_cursor_from_row (struct window *w, struct glyph_row *row,
		     struct glyph_matrix *matrix,
		     EMACS_INT delta, EMACS_INT delta_bytes,
		     int dy, int dvpos)
{
  struct glyph *glyph = row->glyphs[TEXT_AREA];
  struct glyph *end = glyph + row->used[TEXT_AREA];
  struct glyph *cursor = NULL;
  /* The last known character position in row.  */
  EMACS_INT last_pos = MATRIX_ROW_START_CHARPOS (row) + delta;
  int x = row->x;
  EMACS_INT pt_old = PT - delta;
  EMACS_INT pos_before = MATRIX_ROW_START_CHARPOS (row) + delta;
  EMACS_INT pos_after = MATRIX_ROW_END_CHARPOS (row) + delta;
  struct glyph *glyph_before = glyph - 1, *glyph_after = end;
  /* A glyph beyond the edge of TEXT_AREA which we should never
     touch.  */
  struct glyph *glyphs_end = end;
  /* Non-zero means we've found a match for cursor position, but that
     glyph has the avoid_cursor_p flag set.  */
  int match_with_avoid_cursor = 0;
  /* Non-zero means we've seen at least one glyph that came from a
     display string.  */
  int string_seen = 0;
  /* Largest and smallest buffer positions seen so far during scan of
     glyph row.  */
  EMACS_INT bpos_max = pos_before;
  EMACS_INT bpos_min = pos_after;
  /* Last buffer position covered by an overlay string with an integer
     `cursor' property.  */
  EMACS_INT bpos_covered = 0;
  /* Non-zero means the display string on which to display the cursor
     comes from a text property, not from an overlay.  */
  int string_from_text_prop = 0;

  /* Don't even try doing anything if called for a mode-line or
     header-line row, since the rest of the code isn't prepared to
     deal with such calamities.  */
  xassert (!row->mode_line_p);
  if (row->mode_line_p)
    return 0;

  /* Skip over glyphs not having an object at the start and the end of
     the row.  These are special glyphs like truncation marks on
     terminal frames.  */
  if (row->displays_text_p)
    {
      if (!row->reversed_p)
	{
	  while (glyph < end
		 && INTEGERP (glyph->object)
		 && glyph->charpos < 0)
	    {
	      x += glyph->pixel_width;
	      ++glyph;
	    }
	  while (end > glyph
		 && INTEGERP ((end - 1)->object)
		 /* CHARPOS is zero for blanks and stretch glyphs
		    inserted by extend_face_to_end_of_line.  */
		 && (end - 1)->charpos <= 0)
	    --end;
	  glyph_before = glyph - 1;
	  glyph_after = end;
	}
      else
	{
	  struct glyph *g;

	  /* If the glyph row is reversed, we need to process it from back
	     to front, so swap the edge pointers.  */
	  glyphs_end = end = glyph - 1;
	  glyph += row->used[TEXT_AREA] - 1;

	  while (glyph > end + 1
		 && INTEGERP (glyph->object)
		 && glyph->charpos < 0)
	    {
	      --glyph;
	      x -= glyph->pixel_width;
	    }
	  if (INTEGERP (glyph->object) && glyph->charpos < 0)
	    --glyph;
	  /* By default, in reversed rows we put the cursor on the
	     rightmost (first in the reading order) glyph.  */
	  for (g = end + 1; g < glyph; g++)
	    x += g->pixel_width;
	  while (end < glyph
		 && INTEGERP ((end + 1)->object)
		 && (end + 1)->charpos <= 0)
	    ++end;
	  glyph_before = glyph + 1;
	  glyph_after = end;
	}
    }
  else if (row->reversed_p)
    {
      /* In R2L rows that don't display text, put the cursor on the
	 rightmost glyph.  Case in point: an empty last line that is
	 part of an R2L paragraph.  */
      cursor = end - 1;
      /* Avoid placing the cursor on the last glyph of the row, where
	 on terminal frames we hold the vertical border between
	 adjacent windows.  */
      if (!FRAME_WINDOW_P (WINDOW_XFRAME (w))
	  && !WINDOW_RIGHTMOST_P (w)
	  && cursor == row->glyphs[LAST_AREA] - 1)
	cursor--;
      x = -1;	/* will be computed below, at label compute_x */
    }

  /* Step 1: Try to find the glyph whose character position
     corresponds to point.  If that's not possible, find 2 glyphs
     whose character positions are the closest to point, one before
     point, the other after it.  */
  if (!row->reversed_p)
    while (/* not marched to end of glyph row */
	   glyph < end
	   /* glyph was not inserted by redisplay for internal purposes */
	   && !INTEGERP (glyph->object))
      {
	if (BUFFERP (glyph->object))
	  {
	    EMACS_INT dpos = glyph->charpos - pt_old;

	    if (glyph->charpos > bpos_max)
	      bpos_max = glyph->charpos;
	    if (glyph->charpos < bpos_min)
	      bpos_min = glyph->charpos;
	    if (!glyph->avoid_cursor_p)
	      {
		/* If we hit point, we've found the glyph on which to
		   display the cursor.  */
		if (dpos == 0)
		  {
		    match_with_avoid_cursor = 0;
		    break;
		  }
		/* See if we've found a better approximation to
		   POS_BEFORE or to POS_AFTER.  Note that we want the
		   first (leftmost) glyph of all those that are the
		   closest from below, and the last (rightmost) of all
		   those from above.  */
		if (0 > dpos && dpos > pos_before - pt_old)
		  {
		    pos_before = glyph->charpos;
		    glyph_before = glyph;
		  }
		else if (0 < dpos && dpos <= pos_after - pt_old)
		  {
		    pos_after = glyph->charpos;
		    glyph_after = glyph;
		  }
	      }
	    else if (dpos == 0)
	      match_with_avoid_cursor = 1;
	  }
	else if (STRINGP (glyph->object))
	  {
	    Lisp_Object chprop;
	    EMACS_INT glyph_pos = glyph->charpos;

	    chprop = Fget_char_property (make_number (glyph_pos), Qcursor,
					 glyph->object);
	    if (!NILP (chprop))
	      {
		/* If the string came from a `display' text property,
		   look up the buffer position of that property and
		   use that position to update bpos_max, as if we
		   actually saw such a position in one of the row's
		   glyphs.  This helps with supporting integer values
		   of `cursor' property on the display string in
		   situations where most or all of the row's buffer
		   text is completely covered by display properties,
		   so that no glyph with valid buffer positions is
		   ever seen in the row.  */
		EMACS_INT prop_pos =
		  string_buffer_position_lim (glyph->object, pos_before,
					      pos_after, 0);

		if (prop_pos >= pos_before)
		  bpos_max = prop_pos - 1;
	      }
	    if (INTEGERP (chprop))
	      {
		bpos_covered = bpos_max + XINT (chprop);
		/* If the `cursor' property covers buffer positions up
		   to and including point, we should display cursor on
		   this glyph.  Note that, if a `cursor' property on one
		   of the string's characters has an integer value, we
		   will break out of the loop below _before_ we get to
		   the position match above.  IOW, integer values of
		   the `cursor' property override the "exact match for
		   point" strategy of positioning the cursor.  */
		/* Implementation note: bpos_max == pt_old when, e.g.,
		   we are in an empty line, where bpos_max is set to
		   MATRIX_ROW_START_CHARPOS, see above.  */
		if (bpos_max <= pt_old && bpos_covered >= pt_old)
		  {
		    cursor = glyph;
		    break;
		  }
	      }

	    string_seen = 1;
	  }
	x += glyph->pixel_width;
	++glyph;
      }
  else if (glyph > end)	/* row is reversed */
    while (!INTEGERP (glyph->object))
      {
	if (BUFFERP (glyph->object))
	  {
	    EMACS_INT dpos = glyph->charpos - pt_old;

	    if (glyph->charpos > bpos_max)
	      bpos_max = glyph->charpos;
	    if (glyph->charpos < bpos_min)
	      bpos_min = glyph->charpos;
	    if (!glyph->avoid_cursor_p)
	      {
		if (dpos == 0)
		  {
		    match_with_avoid_cursor = 0;
		    break;
		  }
		if (0 > dpos && dpos > pos_before - pt_old)
		  {
		    pos_before = glyph->charpos;
		    glyph_before = glyph;
		  }
		else if (0 < dpos && dpos <= pos_after - pt_old)
		  {
		    pos_after = glyph->charpos;
		    glyph_after = glyph;
		  }
	      }
	    else if (dpos == 0)
	      match_with_avoid_cursor = 1;
	  }
	else if (STRINGP (glyph->object))
	  {
	    Lisp_Object chprop;
	    EMACS_INT glyph_pos = glyph->charpos;

	    chprop = Fget_char_property (make_number (glyph_pos), Qcursor,
					 glyph->object);
	    if (!NILP (chprop))
	      {
		EMACS_INT prop_pos =
		  string_buffer_position_lim (glyph->object, pos_before,
					      pos_after, 0);

		if (prop_pos >= pos_before)
		  bpos_max = prop_pos - 1;
	      }
	    if (INTEGERP (chprop))
	      {
		bpos_covered = bpos_max + XINT (chprop);
		/* If the `cursor' property covers buffer positions up
		   to and including point, we should display cursor on
		   this glyph.  */
		if (bpos_max <= pt_old && bpos_covered >= pt_old)
		  {
		    cursor = glyph;
		    break;
		  }
	      }
	    string_seen = 1;
	  }
	--glyph;
	if (glyph == glyphs_end) /* don't dereference outside TEXT_AREA */
	  {
	    x--;		/* can't use any pixel_width */
	    break;
	  }
	x -= glyph->pixel_width;
    }

  /* Step 2: If we didn't find an exact match for point, we need to
     look for a proper place to put the cursor among glyphs between
     GLYPH_BEFORE and GLYPH_AFTER.  */
  if (!((row->reversed_p ? glyph > glyphs_end : glyph < glyphs_end)
	&& BUFFERP (glyph->object) && glyph->charpos == pt_old)
      && bpos_covered < pt_old)
    {
      /* An empty line has a single glyph whose OBJECT is zero and
	 whose CHARPOS is the position of a newline on that line.
	 Note that on a TTY, there are more glyphs after that, which
	 were produced by extend_face_to_end_of_line, but their
	 CHARPOS is zero or negative.  */
      int empty_line_p =
	(row->reversed_p ? glyph > glyphs_end : glyph < glyphs_end)
	&& INTEGERP (glyph->object) && glyph->charpos > 0;

      if (row->ends_in_ellipsis_p && pos_after == last_pos)
	{
	  EMACS_INT ellipsis_pos;

	  /* Scan back over the ellipsis glyphs.  */
	  if (!row->reversed_p)
	    {
	      ellipsis_pos = (glyph - 1)->charpos;
	      while (glyph > row->glyphs[TEXT_AREA]
		     && (glyph - 1)->charpos == ellipsis_pos)
		glyph--, x -= glyph->pixel_width;
	      /* That loop always goes one position too far, including
		 the glyph before the ellipsis.  So scan forward over
		 that one.  */
	      x += glyph->pixel_width;
	      glyph++;
	    }
	  else	/* row is reversed */
	    {
	      ellipsis_pos = (glyph + 1)->charpos;
	      while (glyph < row->glyphs[TEXT_AREA] + row->used[TEXT_AREA] - 1
		     && (glyph + 1)->charpos == ellipsis_pos)
		glyph++, x += glyph->pixel_width;
	      x -= glyph->pixel_width;
	      glyph--;
	    }
	}
      else if (match_with_avoid_cursor)
	{
	  cursor = glyph_after;
	  x = -1;
	}
      else if (string_seen)
	{
	  int incr = row->reversed_p ? -1 : +1;

	  /* Need to find the glyph that came out of a string which is
	     present at point.  That glyph is somewhere between
	     GLYPH_BEFORE and GLYPH_AFTER, and it came from a string
	     positioned between POS_BEFORE and POS_AFTER in the
	     buffer.  */
	  struct glyph *start, *stop;
	  EMACS_INT pos = pos_before;

	  x = -1;

	  /* If the row ends in a newline from a display string,
	     reordering could have moved the glyphs belonging to the
	     string out of the [GLYPH_BEFORE..GLYPH_AFTER] range.  So
	     in this case we extend the search to the last glyph in
	     the row that was not inserted by redisplay.  */
	  if (row->ends_in_newline_from_string_p)
	    {
	      glyph_after = end;
	      pos_after = MATRIX_ROW_END_CHARPOS (row) + delta;
	    }

	  /* GLYPH_BEFORE and GLYPH_AFTER are the glyphs that
	     correspond to POS_BEFORE and POS_AFTER, respectively.  We
	     need START and STOP in the order that corresponds to the
	     row's direction as given by its reversed_p flag.  If the
	     directionality of characters between POS_BEFORE and
	     POS_AFTER is the opposite of the row's base direction,
	     these characters will have been reordered for display,
	     and we need to reverse START and STOP.  */
	  if (!row->reversed_p)
	    {
	      start = min (glyph_before, glyph_after);
	      stop = max (glyph_before, glyph_after);
	    }
	  else
	    {
	      start = max (glyph_before, glyph_after);
	      stop = min (glyph_before, glyph_after);
	    }
	  for (glyph = start + incr;
	       row->reversed_p ? glyph > stop : glyph < stop; )
	    {

	      /* Any glyphs that come from the buffer are here because
		 of bidi reordering.  Skip them, and only pay
		 attention to glyphs that came from some string.  */
	      if (STRINGP (glyph->object))
		{
		  Lisp_Object str;
		  EMACS_INT tem;
		  /* If the display property covers the newline, we
		     need to search for it one position farther.  */
		  EMACS_INT lim = pos_after
		    + (pos_after == MATRIX_ROW_END_CHARPOS (row) + delta);

		  string_from_text_prop = 0;
		  str = glyph->object;
		  tem = string_buffer_position_lim (str, pos, lim, 0);
		  if (tem == 0	/* from overlay */
		      || pos <= tem)
		    {
		      /* If the string from which this glyph came is
			 found in the buffer at point, then we've
			 found the glyph we've been looking for.  If
			 it comes from an overlay (tem == 0), and it
			 has the `cursor' property on one of its
			 glyphs, record that glyph as a candidate for
			 displaying the cursor.  (As in the
			 unidirectional version, we will display the
			 cursor on the last candidate we find.)  */
		      if (tem == 0 || tem == pt_old)
			{
			  /* The glyphs from this string could have
			     been reordered.  Find the one with the
			     smallest string position.  Or there could
			     be a character in the string with the
			     `cursor' property, which means display
			     cursor on that character's glyph.  */
			  EMACS_INT strpos = glyph->charpos;

			  if (tem)
			    {
			      cursor = glyph;
			      string_from_text_prop = 1;
			    }
			  for ( ;
			       (row->reversed_p ? glyph > stop : glyph < stop)
				 && EQ (glyph->object, str);
			       glyph += incr)
			    {
			      Lisp_Object cprop;
			      EMACS_INT gpos = glyph->charpos;

			      cprop = Fget_char_property (make_number (gpos),
							  Qcursor,
							  glyph->object);
			      if (!NILP (cprop))
				{
				  cursor = glyph;
				  break;
				}
			      if (tem && glyph->charpos < strpos)
				{
				  strpos = glyph->charpos;
				  cursor = glyph;
				}
			    }

			  if (tem == pt_old)
			    goto compute_x;
			}
		      if (tem)
			pos = tem + 1; /* don't find previous instances */
		    }
		  /* This string is not what we want; skip all of the
		     glyphs that came from it.  */
		  while ((row->reversed_p ? glyph > stop : glyph < stop)
			 && EQ (glyph->object, str))
		    glyph += incr;
		}
	      else
		glyph += incr;
	    }

	  /* If we reached the end of the line, and END was from a string,
	     the cursor is not on this line.  */
	  if (cursor == NULL
	      && (row->reversed_p ? glyph <= end : glyph >= end)
	      && STRINGP (end->object)
	      && row->continued_p)
	    return 0;
	}
      /* A truncated row may not include PT among its character positions.
	 Setting the cursor inside the scroll margin will trigger
	 recalculation of hscroll in hscroll_window_tree.  But if a
	 display string covers point, defer to the string-handling
	 code below to figure this out.  */
      else if (row->truncated_on_left_p && pt_old < bpos_min)
	{
	  cursor = glyph_before;
	  x = -1;
	}
      else if ((row->truncated_on_right_p && pt_old > bpos_max)
	       /* Zero-width characters produce no glyphs.  */
	       || (!empty_line_p
		   && (row->reversed_p
		       ? glyph_after > glyphs_end
		       : glyph_after < glyphs_end)))
	{
	  cursor = glyph_after;
	  x = -1;
	}
    }

 compute_x:
  if (cursor != NULL)
    glyph = cursor;
  if (x < 0)
    {
      struct glyph *g;

      /* Need to compute x that corresponds to GLYPH.  */
      for (g = row->glyphs[TEXT_AREA], x = row->x; g < glyph; g++)
	{
	  if (g >= row->glyphs[TEXT_AREA] + row->used[TEXT_AREA])
	    abort ();
	  x += g->pixel_width;
	}
    }

  /* ROW could be part of a continued line, which, under bidi
     reordering, might have other rows whose start and end charpos
     occlude point.  Only set w->cursor if we found a better
     approximation to the cursor position than we have from previously
     examined candidate rows belonging to the same continued line.  */
  if (/* we already have a candidate row */
      w->cursor.vpos >= 0
      /* that candidate is not the row we are processing */
      && MATRIX_ROW (matrix, w->cursor.vpos) != row
      /* Make sure cursor.vpos specifies a row whose start and end
	 charpos occlude point, and it is valid candidate for being a
	 cursor-row.  This is because some callers of this function
	 leave cursor.vpos at the row where the cursor was displayed
	 during the last redisplay cycle.  */
      && MATRIX_ROW_START_CHARPOS (MATRIX_ROW (matrix, w->cursor.vpos)) <= pt_old
      && pt_old <= MATRIX_ROW_END_CHARPOS (MATRIX_ROW (matrix, w->cursor.vpos))
      && cursor_row_p (MATRIX_ROW (matrix, w->cursor.vpos)))
    {
      struct glyph *g1 =
	MATRIX_ROW_GLYPH_START (matrix, w->cursor.vpos) + w->cursor.hpos;

      /* Don't consider glyphs that are outside TEXT_AREA.  */
      if (!(row->reversed_p ? glyph > glyphs_end : glyph < glyphs_end))
	return 0;
      /* Keep the candidate whose buffer position is the closest to
	 point or has the `cursor' property.  */
      if (/* previous candidate is a glyph in TEXT_AREA of that row */
	  w->cursor.hpos >= 0
	  && w->cursor.hpos < MATRIX_ROW_USED (matrix, w->cursor.vpos)
	  && ((BUFFERP (g1->object)
	       && (g1->charpos == pt_old /* an exact match always wins */
		   || (BUFFERP (glyph->object)
		       && eabs (g1->charpos - pt_old)
		       < eabs (glyph->charpos - pt_old))))
	      /* previous candidate is a glyph from a string that has
		 a non-nil `cursor' property */
	      || (STRINGP (g1->object)
		  && (!NILP (Fget_char_property (make_number (g1->charpos),
						Qcursor, g1->object))
		      /* previous candidate is from the same display
			 string as this one, and the display string
			 came from a text property */
		      || (EQ (g1->object, glyph->object)
			  && string_from_text_prop)
		      /* this candidate is from newline and its
			 position is not an exact match */
		      || (INTEGERP (glyph->object)
			  && glyph->charpos != pt_old)))))
	return 0;
      /* If this candidate gives an exact match, use that.  */
      if (!((BUFFERP (glyph->object) && glyph->charpos == pt_old)
	    /* If this candidate is a glyph created for the
	       terminating newline of a line, and point is on that
	       newline, it wins because it's an exact match.  */
	    || (!row->continued_p
		&& INTEGERP (glyph->object)
		&& glyph->charpos == 0
		&& pt_old == MATRIX_ROW_END_CHARPOS (row) - 1))
	  /* Otherwise, keep the candidate that comes from a row
	     spanning less buffer positions.  This may win when one or
	     both candidate positions are on glyphs that came from
	     display strings, for which we cannot compare buffer
	     positions.  */
	  && MATRIX_ROW_END_CHARPOS (MATRIX_ROW (matrix, w->cursor.vpos))
	     - MATRIX_ROW_START_CHARPOS (MATRIX_ROW (matrix, w->cursor.vpos))
	     < MATRIX_ROW_END_CHARPOS (row) - MATRIX_ROW_START_CHARPOS (row))
	return 0;
    }
  w->cursor.hpos = glyph - row->glyphs[TEXT_AREA];
  w->cursor.x = x;
  w->cursor.vpos = MATRIX_ROW_VPOS (row, matrix) + dvpos;
  w->cursor.y = row->y + dy;

  if (w == XWINDOW (selected_window))
    {
      if (!row->continued_p
	  && !MATRIX_ROW_CONTINUATION_LINE_P (row)
	  && row->x == 0)
	{
	  this_line_buffer = XBUFFER (w->buffer);

	  CHARPOS (this_line_start_pos)
	    = MATRIX_ROW_START_CHARPOS (row) + delta;
	  BYTEPOS (this_line_start_pos)
	    = MATRIX_ROW_START_BYTEPOS (row) + delta_bytes;

	  CHARPOS (this_line_end_pos)
	    = Z - (MATRIX_ROW_END_CHARPOS (row) + delta);
	  BYTEPOS (this_line_end_pos)
	    = Z_BYTE - (MATRIX_ROW_END_BYTEPOS (row) + delta_bytes);

	  this_line_y = w->cursor.y;
	  this_line_pixel_height = row->height;
	  this_line_vpos = w->cursor.vpos;
	  this_line_start_x = row->x;
	}
      else
	CHARPOS (this_line_start_pos) = 0;
    }

  return 1;
}


/* Run window scroll functions, if any, for WINDOW with new window
   start STARTP.  Sets the window start of WINDOW to that position.

   We assume that the window's buffer is really current.  */

static inline struct text_pos
run_window_scroll_functions (Lisp_Object window, struct text_pos startp)
{
  struct window *w = XWINDOW (window);
  SET_MARKER_FROM_TEXT_POS (w->start, startp);

  if (current_buffer != XBUFFER (w->buffer))
    abort ();

  if (!NILP (Vwindow_scroll_functions))
    {
      run_hook_with_args_2 (Qwindow_scroll_functions, window,
			    make_number (CHARPOS (startp)));
      SET_TEXT_POS_FROM_MARKER (startp, w->start);
      /* In case the hook functions switch buffers.  */
      if (current_buffer != XBUFFER (w->buffer))
	set_buffer_internal_1 (XBUFFER (w->buffer));
    }

  return startp;
}


/* Make sure the line containing the cursor is fully visible.
   A value of 1 means there is nothing to be done.
   (Either the line is fully visible, or it cannot be made so,
   or we cannot tell.)

   If FORCE_P is non-zero, return 0 even if partial visible cursor row
   is higher than window.

   A value of 0 means the caller should do scrolling
   as if point had gone off the screen.  */

static int
cursor_row_fully_visible_p (struct window *w, int force_p, int current_matrix_p)
{
  struct glyph_matrix *matrix;
  struct glyph_row *row;
  int window_height;

  if (!make_cursor_line_fully_visible_p)
    return 1;

  /* It's not always possible to find the cursor, e.g, when a window
     is full of overlay strings.  Don't do anything in that case.  */
  if (w->cursor.vpos < 0)
    return 1;

  matrix = current_matrix_p ? w->current_matrix : w->desired_matrix;
  row = MATRIX_ROW (matrix, w->cursor.vpos);

  /* If the cursor row is not partially visible, there's nothing to do.  */
  if (!MATRIX_ROW_PARTIALLY_VISIBLE_P (w, row))
    return 1;

  /* If the row the cursor is in is taller than the window's height,
     it's not clear what to do, so do nothing.  */
  window_height = window_box_height (w);
  if (row->height >= window_height)
    {
      if (!force_p || MINI_WINDOW_P (w)
	  || w->vscroll || w->cursor.vpos == 0)
	return 1;
    }
  return 0;
}


/* Try scrolling PT into view in window WINDOW.  JUST_THIS_ONE_P
   non-zero means only WINDOW is redisplayed in redisplay_internal.
   TEMP_SCROLL_STEP has the same meaning as emacs_scroll_step, and is used
   in redisplay_window to bring a partially visible line into view in
   the case that only the cursor has moved.

   LAST_LINE_MISFIT should be nonzero if we're scrolling because the
   last screen line's vertical height extends past the end of the screen.

   Value is

   1	if scrolling succeeded

   0	if scrolling didn't find point.

   -1	if new fonts have been loaded so that we must interrupt
   redisplay, adjust glyph matrices, and try again.  */

enum
{
  SCROLLING_SUCCESS,
  SCROLLING_FAILED,
  SCROLLING_NEED_LARGER_MATRICES
};

/* If scroll-conservatively is more than this, never recenter.

   If you change this, don't forget to update the doc string of
   `scroll-conservatively' and the Emacs manual.  */
#define SCROLL_LIMIT 100

static int
try_scrolling (Lisp_Object window, int just_this_one_p,
	       EMACS_INT arg_scroll_conservatively, EMACS_INT scroll_step,
	       int temp_scroll_step, int last_line_misfit)
{
  struct window *w = XWINDOW (window);
  struct frame *f = XFRAME (w->frame);
  struct text_pos pos, startp;
  struct it it;
  int this_scroll_margin, scroll_max, rc, height;
  int dy = 0, amount_to_scroll = 0, scroll_down_p = 0;
  int extra_scroll_margin_lines = last_line_misfit ? 1 : 0;
  Lisp_Object aggressive;
  /* We will never try scrolling more than this number of lines.  */
  int scroll_limit = SCROLL_LIMIT;

#if GLYPH_DEBUG
  debug_method_add (w, "try_scrolling");
#endif

  SET_TEXT_POS_FROM_MARKER (startp, w->start);

  /* Compute scroll margin height in pixels.  We scroll when point is
     within this distance from the top or bottom of the window.  */
  if (scroll_margin > 0)
    this_scroll_margin = min (scroll_margin, WINDOW_TOTAL_LINES (w) / 4)
      * FRAME_LINE_HEIGHT (f);
  else
    this_scroll_margin = 0;

  /* Force arg_scroll_conservatively to have a reasonable value, to
     avoid scrolling too far away with slow move_it_* functions.  Note
     that the user can supply scroll-conservatively equal to
     `most-positive-fixnum', which can be larger than INT_MAX.  */
  if (arg_scroll_conservatively > scroll_limit)
    {
      arg_scroll_conservatively = scroll_limit + 1;
      scroll_max = scroll_limit * FRAME_LINE_HEIGHT (f);
    }
  else if (scroll_step || arg_scroll_conservatively || temp_scroll_step)
    /* Compute how much we should try to scroll maximally to bring
       point into view.  */
    scroll_max = (max (scroll_step,
		       max (arg_scroll_conservatively, temp_scroll_step))
		  * FRAME_LINE_HEIGHT (f));
  else if (NUMBERP (BVAR (current_buffer, scroll_down_aggressively))
	   || NUMBERP (BVAR (current_buffer, scroll_up_aggressively)))
    /* We're trying to scroll because of aggressive scrolling but no
       scroll_step is set.  Choose an arbitrary one.  */
    scroll_max = 10 * FRAME_LINE_HEIGHT (f);
  else
    scroll_max = 0;

 too_near_end:

  /* Decide whether to scroll down.  */
  if (PT > CHARPOS (startp))
    {
      int scroll_margin_y;

      /* Compute the pixel ypos of the scroll margin, then move IT to
	 either that ypos or PT, whichever comes first.  */
      start_display (&it, w, startp);
      scroll_margin_y = it.last_visible_y - this_scroll_margin
	- FRAME_LINE_HEIGHT (f) * extra_scroll_margin_lines;
      move_it_to (&it, PT, -1, scroll_margin_y - 1, -1,
		  (MOVE_TO_POS | MOVE_TO_Y));

      if (PT > CHARPOS (it.current.pos))
	{
	  int y0 = line_bottom_y (&it);
	  /* Compute how many pixels below window bottom to stop searching
	     for PT.  This avoids costly search for PT that is far away if
	     the user limited scrolling by a small number of lines, but
	     always finds PT if scroll_conservatively is set to a large
	     number, such as most-positive-fixnum.  */
	  int slack = max (scroll_max, 10 * FRAME_LINE_HEIGHT (f));
	  int y_to_move = it.last_visible_y + slack;

	  /* Compute the distance from the scroll margin to PT or to
	     the scroll limit, whichever comes first.  This should
	     include the height of the cursor line, to make that line
	     fully visible.  */
	  move_it_to (&it, PT, -1, y_to_move,
	  	      -1, MOVE_TO_POS | MOVE_TO_Y);
	  dy = line_bottom_y (&it) - y0;

	  if (dy > scroll_max)
	    return SCROLLING_FAILED;

	  if (dy > 0)
	    scroll_down_p = 1;
	}
    }

  if (scroll_down_p)
    {
      /* Point is in or below the bottom scroll margin, so move the
	 window start down.  If scrolling conservatively, move it just
	 enough down to make point visible.  If scroll_step is set,
	 move it down by scroll_step.  */
      if (arg_scroll_conservatively)
	amount_to_scroll
	  = min (max (dy, FRAME_LINE_HEIGHT (f)),
		 FRAME_LINE_HEIGHT (f) * arg_scroll_conservatively);
      else if (scroll_step || temp_scroll_step)
	amount_to_scroll = scroll_max;
      else
	{
	  aggressive = BVAR (current_buffer, scroll_up_aggressively);
	  height = WINDOW_BOX_TEXT_HEIGHT (w);
	  if (NUMBERP (aggressive))
	    {
	      double float_amount = XFLOATINT (aggressive) * height;
	      amount_to_scroll = float_amount;
	      if (amount_to_scroll == 0 && float_amount > 0)
		amount_to_scroll = 1;
	      /* Don't let point enter the scroll margin near top of
		 the window.  */
	      if (amount_to_scroll > height - 2*this_scroll_margin + dy)
		amount_to_scroll = height - 2*this_scroll_margin + dy;
	    }
	}

      if (amount_to_scroll <= 0)
	return SCROLLING_FAILED;

      start_display (&it, w, startp);
      if (arg_scroll_conservatively <= scroll_limit)
	move_it_vertically (&it, amount_to_scroll);
      else
	{
	  /* Extra precision for users who set scroll-conservatively
	     to a large number: make sure the amount we scroll
	     the window start is never less than amount_to_scroll,
	     which was computed as distance from window bottom to
	     point.  This matters when lines at window top and lines
	     below window bottom have different height.  */
	  struct it it1;
	  void *it1data = NULL;
	  /* We use a temporary it1 because line_bottom_y can modify
	     its argument, if it moves one line down; see there.  */
	  int start_y;

	  SAVE_IT (it1, it, it1data);
	  start_y = line_bottom_y (&it1);
	  do {
	    RESTORE_IT (&it, &it, it1data);
	    move_it_by_lines (&it, 1);
	    SAVE_IT (it1, it, it1data);
	  } while (line_bottom_y (&it1) - start_y < amount_to_scroll);
	}

      /* If STARTP is unchanged, move it down another screen line.  */
      if (CHARPOS (it.current.pos) == CHARPOS (startp))
	move_it_by_lines (&it, 1);
      startp = it.current.pos;
    }
  else
    {
      struct text_pos scroll_margin_pos = startp;

      /* See if point is inside the scroll margin at the top of the
         window.  */
      if (this_scroll_margin)
	{
	  start_display (&it, w, startp);
	  move_it_vertically (&it, this_scroll_margin);
	  scroll_margin_pos = it.current.pos;
	}

      if (PT < CHARPOS (scroll_margin_pos))
	{
	  /* Point is in the scroll margin at the top of the window or
	     above what is displayed in the window.  */
	  int y0, y_to_move;

	  /* Compute the vertical distance from PT to the scroll
	     margin position.  Move as far as scroll_max allows, or
	     one screenful, or 10 screen lines, whichever is largest.
	     Give up if distance is greater than scroll_max.  */
	  SET_TEXT_POS (pos, PT, PT_BYTE);
	  start_display (&it, w, pos);
	  y0 = it.current_y;
	  y_to_move = max (it.last_visible_y,
			   max (scroll_max, 10 * FRAME_LINE_HEIGHT (f)));
	  move_it_to (&it, CHARPOS (scroll_margin_pos), 0,
		      y_to_move, -1,
		      MOVE_TO_POS | MOVE_TO_X | MOVE_TO_Y);
	  dy = it.current_y - y0;
	  if (dy > scroll_max)
	    return SCROLLING_FAILED;

	  /* Compute new window start.  */
	  start_display (&it, w, startp);

	  if (arg_scroll_conservatively)
	    amount_to_scroll = max (dy, FRAME_LINE_HEIGHT (f) *
				    max (scroll_step, temp_scroll_step));
	  else if (scroll_step || temp_scroll_step)
	    amount_to_scroll = scroll_max;
	  else
	    {
	      aggressive = BVAR (current_buffer, scroll_down_aggressively);
	      height = WINDOW_BOX_TEXT_HEIGHT (w);
	      if (NUMBERP (aggressive))
		{
		  double float_amount = XFLOATINT (aggressive) * height;
		  amount_to_scroll = float_amount;
		  if (amount_to_scroll == 0 && float_amount > 0)
		    amount_to_scroll = 1;
		  amount_to_scroll -=
		    this_scroll_margin - dy - FRAME_LINE_HEIGHT (f);
		  /* Don't let point enter the scroll margin near
		     bottom of the window.  */
		  if (amount_to_scroll > height - 2*this_scroll_margin + dy)
		    amount_to_scroll = height - 2*this_scroll_margin + dy;
		}
	    }

	  if (amount_to_scroll <= 0)
	    return SCROLLING_FAILED;

	  move_it_vertically_backward (&it, amount_to_scroll);
	  startp = it.current.pos;
	}
    }

  /* Run window scroll functions.  */
  startp = run_window_scroll_functions (window, startp);

  /* Display the window.  Give up if new fonts are loaded, or if point
     doesn't appear.  */
  if (!try_window (window, startp, 0))
    rc = SCROLLING_NEED_LARGER_MATRICES;
  else if (w->cursor.vpos < 0)
    {
      clear_glyph_matrix (w->desired_matrix);
      rc = SCROLLING_FAILED;
    }
  else
    {
      /* Maybe forget recorded base line for line number display.  */
      if (!just_this_one_p
	  || current_buffer->clip_changed
	  || BEG_UNCHANGED < CHARPOS (startp))
	w->base_line_number = Qnil;

      /* If cursor ends up on a partially visible line,
	 treat that as being off the bottom of the screen.  */
      if (! cursor_row_fully_visible_p (w, extra_scroll_margin_lines <= 1, 0)
	  /* It's possible that the cursor is on the first line of the
	     buffer, which is partially obscured due to a vscroll
	     (Bug#7537).  In that case, avoid looping forever . */
	  && extra_scroll_margin_lines < w->desired_matrix->nrows - 1)
	{
	  clear_glyph_matrix (w->desired_matrix);
	  ++extra_scroll_margin_lines;
	  goto too_near_end;
	}
      rc = SCROLLING_SUCCESS;
    }

  return rc;
}


/* Compute a suitable window start for window W if display of W starts
   on a continuation line.  Value is non-zero if a new window start
   was computed.

   The new window start will be computed, based on W's width, starting
   from the start of the continued line.  It is the start of the
   screen line with the minimum distance from the old start W->start.  */

static int
compute_window_start_on_continuation_line (struct window *w)
{
  struct text_pos pos, start_pos;
  int window_start_changed_p = 0;

  SET_TEXT_POS_FROM_MARKER (start_pos, w->start);

  /* If window start is on a continuation line...  Window start may be
     < BEGV in case there's invisible text at the start of the
     buffer (M-x rmail, for example).  */
  if (CHARPOS (start_pos) > BEGV
      && FETCH_BYTE (BYTEPOS (start_pos) - 1) != '\n')
    {
      struct it it;
      struct glyph_row *row;

      /* Handle the case that the window start is out of range.  */
      if (CHARPOS (start_pos) < BEGV)
	SET_TEXT_POS (start_pos, BEGV, BEGV_BYTE);
      else if (CHARPOS (start_pos) > ZV)
	SET_TEXT_POS (start_pos, ZV, ZV_BYTE);

      /* Find the start of the continued line.  This should be fast
	 because scan_buffer is fast (newline cache).  */
      row = w->desired_matrix->rows + (WINDOW_WANTS_HEADER_LINE_P (w) ? 1 : 0);
      init_iterator (&it, w, CHARPOS (start_pos), BYTEPOS (start_pos),
		     row, DEFAULT_FACE_ID);
      reseat_at_previous_visible_line_start (&it);

      /* If the line start is "too far" away from the window start,
         say it takes too much time to compute a new window start.  */
      if (CHARPOS (start_pos) - IT_CHARPOS (it)
	  < WINDOW_TOTAL_LINES (w) * WINDOW_TOTAL_COLS (w))
	{
	  int min_distance, distance;

	  /* Move forward by display lines to find the new window
	     start.  If window width was enlarged, the new start can
	     be expected to be > the old start.  If window width was
	     decreased, the new window start will be < the old start.
	     So, we're looking for the display line start with the
	     minimum distance from the old window start.  */
	  pos = it.current.pos;
	  min_distance = INFINITY;
	  while ((distance = eabs (CHARPOS (start_pos) - IT_CHARPOS (it))),
		 distance < min_distance)
	    {
	      min_distance = distance;
	      pos = it.current.pos;
	      move_it_by_lines (&it, 1);
	    }

	  /* Set the window start there.  */
	  SET_MARKER_FROM_TEXT_POS (w->start, pos);
	  window_start_changed_p = 1;
	}
    }

  return window_start_changed_p;
}


/* Try cursor movement in case text has not changed in window WINDOW,
   with window start STARTP.  Value is

   CURSOR_MOVEMENT_SUCCESS if successful

   CURSOR_MOVEMENT_CANNOT_BE_USED if this method cannot be used

   CURSOR_MOVEMENT_MUST_SCROLL if we know we have to scroll the
   display.  *SCROLL_STEP is set to 1, under certain circumstances, if
   we want to scroll as if scroll-step were set to 1.  See the code.

   CURSOR_MOVEMENT_NEED_LARGER_MATRICES if we need larger matrices, in
   which case we have to abort this redisplay, and adjust matrices
   first.  */

enum
{
  CURSOR_MOVEMENT_SUCCESS,
  CURSOR_MOVEMENT_CANNOT_BE_USED,
  CURSOR_MOVEMENT_MUST_SCROLL,
  CURSOR_MOVEMENT_NEED_LARGER_MATRICES
};

static int
try_cursor_movement (Lisp_Object window, struct text_pos startp, int *scroll_step)
{
  struct window *w = XWINDOW (window);
  struct frame *f = XFRAME (w->frame);
  int rc = CURSOR_MOVEMENT_CANNOT_BE_USED;

#if GLYPH_DEBUG
  if (inhibit_try_cursor_movement)
    return rc;
#endif

  /* Handle case where text has not changed, only point, and it has
     not moved off the frame.  */
  if (/* Point may be in this window.  */
      PT >= CHARPOS (startp)
      /* Selective display hasn't changed.  */
      && !current_buffer->clip_changed
      /* Function force-mode-line-update is used to force a thorough
	 redisplay.  It sets either windows_or_buffers_changed or
	 update_mode_lines.  So don't take a shortcut here for these
	 cases.  */
      && !update_mode_lines
      && !windows_or_buffers_changed
      && !cursor_type_changed
      /* Can't use this case if highlighting a region.  When a
         region exists, cursor movement has to do more than just
         set the cursor.  */
      && !(!NILP (Vtransient_mark_mode)
	   && !NILP (BVAR (current_buffer, mark_active)))
      && NILP (w->region_showing)
      && NILP (Vshow_trailing_whitespace)
      /* Right after splitting windows, last_point may be nil.  */
      && INTEGERP (w->last_point)
      /* This code is not used for mini-buffer for the sake of the case
	 of redisplaying to replace an echo area message; since in
	 that case the mini-buffer contents per se are usually
	 unchanged.  This code is of no real use in the mini-buffer
	 since the handling of this_line_start_pos, etc., in redisplay
	 handles the same cases.  */
      && !EQ (window, minibuf_window)
      /* When splitting windows or for new windows, it happens that
	 redisplay is called with a nil window_end_vpos or one being
	 larger than the window.  This should really be fixed in
	 window.c.  I don't have this on my list, now, so we do
	 approximately the same as the old redisplay code.  --gerd.  */
      && INTEGERP (w->window_end_vpos)
      && XFASTINT (w->window_end_vpos) < w->current_matrix->nrows
      && (FRAME_WINDOW_P (f)
	  || !overlay_arrow_in_current_buffer_p ()))
    {
      int this_scroll_margin, top_scroll_margin;
      struct glyph_row *row = NULL;

#if GLYPH_DEBUG
      debug_method_add (w, "cursor movement");
#endif

      /* Scroll if point within this distance from the top or bottom
	 of the window.  This is a pixel value.  */
      if (scroll_margin > 0)
	{
	  this_scroll_margin = min (scroll_margin, WINDOW_TOTAL_LINES (w) / 4);
	  this_scroll_margin *= FRAME_LINE_HEIGHT (f);
	}
      else
	this_scroll_margin = 0;

      top_scroll_margin = this_scroll_margin;
      if (WINDOW_WANTS_HEADER_LINE_P (w))
	top_scroll_margin += CURRENT_HEADER_LINE_HEIGHT (w);

      /* Start with the row the cursor was displayed during the last
	 not paused redisplay.  Give up if that row is not valid.  */
      if (w->last_cursor.vpos < 0
	  || w->last_cursor.vpos >= w->current_matrix->nrows)
	rc = CURSOR_MOVEMENT_MUST_SCROLL;
      else
	{
	  row = MATRIX_ROW (w->current_matrix, w->last_cursor.vpos);
	  if (row->mode_line_p)
	    ++row;
	  if (!row->enabled_p)
	    rc = CURSOR_MOVEMENT_MUST_SCROLL;
	}

      if (rc == CURSOR_MOVEMENT_CANNOT_BE_USED)
	{
	  int scroll_p = 0, must_scroll = 0;
	  int last_y = window_text_bottom_y (w) - this_scroll_margin;

	  if (PT > XFASTINT (w->last_point))
	    {
	      /* Point has moved forward.  */
	      while (MATRIX_ROW_END_CHARPOS (row) < PT
		     && MATRIX_ROW_BOTTOM_Y (row) < last_y)
		{
		  xassert (row->enabled_p);
		  ++row;
		}

	      /* If the end position of a row equals the start
		 position of the next row, and PT is at that position,
		 we would rather display cursor in the next line.  */
	      while (MATRIX_ROW_BOTTOM_Y (row) < last_y
		     && MATRIX_ROW_END_CHARPOS (row) == PT
		     && row < w->current_matrix->rows
				+ w->current_matrix->nrows - 1
		     && MATRIX_ROW_START_CHARPOS (row+1) == PT
		     && !cursor_row_p (row))
		++row;

	      /* If within the scroll margin, scroll.  Note that
		 MATRIX_ROW_BOTTOM_Y gives the pixel position at which
		 the next line would be drawn, and that
		 this_scroll_margin can be zero.  */
	      if (MATRIX_ROW_BOTTOM_Y (row) > last_y
		  || PT > MATRIX_ROW_END_CHARPOS (row)
		  /* Line is completely visible last line in window
		     and PT is to be set in the next line.  */
		  || (MATRIX_ROW_BOTTOM_Y (row) == last_y
		      && PT == MATRIX_ROW_END_CHARPOS (row)
		      && !row->ends_at_zv_p
		      && !MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (row)))
		scroll_p = 1;
	    }
	  else if (PT < XFASTINT (w->last_point))
	    {
	      /* Cursor has to be moved backward.  Note that PT >=
		 CHARPOS (startp) because of the outer if-statement.  */
	      while (!row->mode_line_p
		     && (MATRIX_ROW_START_CHARPOS (row) > PT
			 || (MATRIX_ROW_START_CHARPOS (row) == PT
			     && (MATRIX_ROW_STARTS_IN_MIDDLE_OF_CHAR_P (row)
				 || (/* STARTS_IN_MIDDLE_OF_STRING_P (row) */
				     row > w->current_matrix->rows
				     && (row-1)->ends_in_newline_from_string_p))))
		     && (row->y > top_scroll_margin
			 || CHARPOS (startp) == BEGV))
		{
		  xassert (row->enabled_p);
		  --row;
		}

	      /* Consider the following case: Window starts at BEGV,
		 there is invisible, intangible text at BEGV, so that
		 display starts at some point START > BEGV.  It can
		 happen that we are called with PT somewhere between
		 BEGV and START.  Try to handle that case.  */
	      if (row < w->current_matrix->rows
		  || row->mode_line_p)
		{
		  row = w->current_matrix->rows;
		  if (row->mode_line_p)
		    ++row;
		}

	      /* Due to newlines in overlay strings, we may have to
		 skip forward over overlay strings.  */
	      while (MATRIX_ROW_BOTTOM_Y (row) < last_y
		     && MATRIX_ROW_END_CHARPOS (row) == PT
		     && !cursor_row_p (row))
		++row;

	      /* If within the scroll margin, scroll.  */
	      if (row->y < top_scroll_margin
		  && CHARPOS (startp) != BEGV)
		scroll_p = 1;
	    }
	  else
	    {
	      /* Cursor did not move.  So don't scroll even if cursor line
		 is partially visible, as it was so before.  */
		 rc = CURSOR_MOVEMENT_SUCCESS;
	    }

	  if (PT < MATRIX_ROW_START_CHARPOS (row)
	      || PT > MATRIX_ROW_END_CHARPOS (row))
	    {
	      /* if PT is not in the glyph row, give up.  */
	      rc = CURSOR_MOVEMENT_MUST_SCROLL;
	      must_scroll = 1;
	    }
	  else if (rc != CURSOR_MOVEMENT_SUCCESS
		   && !NILP (BVAR (XBUFFER (w->buffer), bidi_display_reordering)))
	    {
	      struct glyph_row *row1;

	      /* If rows are bidi-reordered and point moved, back up
		 until we find a row that does not belong to a
		 continuation line.  This is because we must consider
		 all rows of a continued line as candidates for the
		 new cursor positioning, since row start and end
		 positions change non-linearly with vertical position
		 in such rows.  */
	      /* FIXME: Revisit this when glyph ``spilling'' in
		 continuation lines' rows is implemented for
		 bidi-reordered rows.  */
	      for (row1 = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
		   MATRIX_ROW_CONTINUATION_LINE_P (row);
		   --row)
		{
		  /* If we hit the beginning of the displayed portion
		     without finding the first row of a continued
		     line, give up.  */
		  if (row <= row1)
		    {
		      rc = CURSOR_MOVEMENT_MUST_SCROLL;
		      break;
		    }
		  xassert (row->enabled_p);
		}
	    }
	  if (must_scroll)
	    ;
	  else if (rc != CURSOR_MOVEMENT_SUCCESS
	      && MATRIX_ROW_PARTIALLY_VISIBLE_P (w, row)
	      /* Make sure this isn't a header line by any chance, since
		 then MATRIX_ROW_PARTIALLY_VISIBLE_P might yield non-zero.  */
	      && !row->mode_line_p
	      && make_cursor_line_fully_visible_p)
	    {
	      if (PT == MATRIX_ROW_END_CHARPOS (row)
		  && !row->ends_at_zv_p
		  && !MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (row))
		rc = CURSOR_MOVEMENT_MUST_SCROLL;
	      else if (row->height > window_box_height (w))
		{
		  /* If we end up in a partially visible line, let's
		     make it fully visible, except when it's taller
		     than the window, in which case we can't do much
		     about it.  */
		  *scroll_step = 1;
		  rc = CURSOR_MOVEMENT_MUST_SCROLL;
		}
	      else
		{
		  set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0);
		  if (!cursor_row_fully_visible_p (w, 0, 1))
		    rc = CURSOR_MOVEMENT_MUST_SCROLL;
		  else
		    rc = CURSOR_MOVEMENT_SUCCESS;
		}
	    }
	  else if (scroll_p)
	    rc = CURSOR_MOVEMENT_MUST_SCROLL;
	  else if (rc != CURSOR_MOVEMENT_SUCCESS
		   && !NILP (BVAR (XBUFFER (w->buffer), bidi_display_reordering)))
	    {
	      /* With bidi-reordered rows, there could be more than
		 one candidate row whose start and end positions
		 occlude point.  We need to let set_cursor_from_row
		 find the best candidate.  */
	      /* FIXME: Revisit this when glyph ``spilling'' in
		 continuation lines' rows is implemented for
		 bidi-reordered rows.  */
	      int rv = 0;

	      do
		{
		  int at_zv_p = 0, exact_match_p = 0;

		  if (MATRIX_ROW_START_CHARPOS (row) <= PT
		      && PT <= MATRIX_ROW_END_CHARPOS (row)
		      && cursor_row_p (row))
		    rv |= set_cursor_from_row (w, row, w->current_matrix,
					       0, 0, 0, 0);
		  /* As soon as we've found the exact match for point,
		     or the first suitable row whose ends_at_zv_p flag
		     is set, we are done.  */
		  at_zv_p =
		    MATRIX_ROW (w->current_matrix, w->cursor.vpos)->ends_at_zv_p;
		  if (rv && !at_zv_p
		      && w->cursor.hpos >= 0
		      && w->cursor.hpos < MATRIX_ROW_USED (w->current_matrix,
							   w->cursor.vpos))
		    {
		      struct glyph_row *candidate =
			MATRIX_ROW (w->current_matrix, w->cursor.vpos);
		      struct glyph *g =
			candidate->glyphs[TEXT_AREA] + w->cursor.hpos;
		      EMACS_INT endpos = MATRIX_ROW_END_CHARPOS (candidate);

		      exact_match_p =
			(BUFFERP (g->object) && g->charpos == PT)
			|| (INTEGERP (g->object)
			    && (g->charpos == PT
				|| (g->charpos == 0 && endpos - 1 == PT)));
		    }
		  if (rv && (at_zv_p || exact_match_p))
		    {
		      rc = CURSOR_MOVEMENT_SUCCESS;
		      break;
		    }
		  if (MATRIX_ROW_BOTTOM_Y (row) == last_y)
		    break;
		  ++row;
		}
	      while (((MATRIX_ROW_CONTINUATION_LINE_P (row)
		       || row->continued_p)
		      && MATRIX_ROW_BOTTOM_Y (row) <= last_y)
		     || (MATRIX_ROW_START_CHARPOS (row) == PT
			 && MATRIX_ROW_BOTTOM_Y (row) < last_y));
	      /* If we didn't find any candidate rows, or exited the
		 loop before all the candidates were examined, signal
		 to the caller that this method failed.  */
	      if (rc != CURSOR_MOVEMENT_SUCCESS
		  && !(rv
		       && !MATRIX_ROW_CONTINUATION_LINE_P (row)
		       && !row->continued_p))
		rc = CURSOR_MOVEMENT_MUST_SCROLL;
	      else if (rv)
		rc = CURSOR_MOVEMENT_SUCCESS;
	    }
	  else
	    {
	      do
		{
		  if (set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0))
		    {
		      rc = CURSOR_MOVEMENT_SUCCESS;
		      break;
		    }
		  ++row;
		}
	      while (MATRIX_ROW_BOTTOM_Y (row) < last_y
		     && MATRIX_ROW_START_CHARPOS (row) == PT
		     && cursor_row_p (row));
	    }
	}
    }

  return rc;
}

#if !defined USE_TOOLKIT_SCROLL_BARS || defined USE_GTK
static
#endif
void
set_vertical_scroll_bar (struct window *w)
{
  EMACS_INT start, end, whole;

  /* Calculate the start and end positions for the current window.
     At some point, it would be nice to choose between scrollbars
     which reflect the whole buffer size, with special markers
     indicating narrowing, and scrollbars which reflect only the
     visible region.

     Note that mini-buffers sometimes aren't displaying any text.  */
  if (!MINI_WINDOW_P (w)
      || (w == XWINDOW (minibuf_window)
	  && NILP (echo_area_buffer[0])))
    {
      struct buffer *buf = XBUFFER (w->buffer);
      whole = BUF_ZV (buf) - BUF_BEGV (buf);
      start = marker_position (w->start) - BUF_BEGV (buf);
      /* I don't think this is guaranteed to be right.  For the
	 moment, we'll pretend it is.  */
      end = BUF_Z (buf) - XFASTINT (w->window_end_pos) - BUF_BEGV (buf);

      if (end < start)
	end = start;
      if (whole < (end - start))
	whole = end - start;
    }
  else
    start = end = whole = 0;

  /* Indicate what this scroll bar ought to be displaying now.  */
  if (FRAME_TERMINAL (XFRAME (w->frame))->set_vertical_scroll_bar_hook)
    (*FRAME_TERMINAL (XFRAME (w->frame))->set_vertical_scroll_bar_hook)
      (w, end - start, whole, start);
}


/* Redisplay leaf window WINDOW.  JUST_THIS_ONE_P non-zero means only
   selected_window is redisplayed.

   We can return without actually redisplaying the window if
   fonts_changed_p is nonzero.  In that case, redisplay_internal will
   retry.  */

static void
redisplay_window (Lisp_Object window, int just_this_one_p)
{
  struct window *w = XWINDOW (window);
  struct frame *f = XFRAME (w->frame);
  struct buffer *buffer = XBUFFER (w->buffer);
  struct buffer *old = current_buffer;
  struct text_pos lpoint, opoint, startp;
  int update_mode_line;
  int tem;
  struct it it;
  /* Record it now because it's overwritten.  */
  int current_matrix_up_to_date_p = 0;
  int used_current_matrix_p = 0;
  /* This is less strict than current_matrix_up_to_date_p.
     It indicates that the buffer contents and narrowing are unchanged.  */
  int buffer_unchanged_p = 0;
  int temp_scroll_step = 0;
  int count = SPECPDL_INDEX ();
  int rc;
  int centering_position = -1;
  int last_line_misfit = 0;
  EMACS_INT beg_unchanged, end_unchanged;

  SET_TEXT_POS (lpoint, PT, PT_BYTE);
  opoint = lpoint;

  /* W must be a leaf window here.  */
  xassert (!NILP (w->buffer));
#if GLYPH_DEBUG
  *w->desired_matrix->method = 0;
#endif

 restart:
  reconsider_clip_changes (w, buffer);

  /* Has the mode line to be updated?  */
  update_mode_line = (!NILP (w->update_mode_line)
		      || update_mode_lines
		      || buffer->clip_changed
		      || buffer->prevent_redisplay_optimizations_p);

  if (MINI_WINDOW_P (w))
    {
      if (w == XWINDOW (echo_area_window)
	  && !NILP (echo_area_buffer[0]))
	{
	  if (update_mode_line)
	    /* We may have to update a tty frame's menu bar or a
	       tool-bar.  Example `M-x C-h C-h C-g'.  */
	    goto finish_menu_bars;
	  else
	    /* We've already displayed the echo area glyphs in this window.  */
	    goto finish_scroll_bars;
	}
      else if ((w != XWINDOW (minibuf_window)
		|| minibuf_level == 0)
	       /* When buffer is nonempty, redisplay window normally. */
	       && BUF_Z (XBUFFER (w->buffer)) == BUF_BEG (XBUFFER (w->buffer))
	       /* Quail displays non-mini buffers in minibuffer window.
		  In that case, redisplay the window normally.  */
	       && !NILP (Fmemq (w->buffer, Vminibuffer_list)))
	{
	  /* W is a mini-buffer window, but it's not active, so clear
	     it.  */
	  int yb = window_text_bottom_y (w);
	  struct glyph_row *row;
	  int y;

	  for (y = 0, row = w->desired_matrix->rows;
	       y < yb;
	       y += row->height, ++row)
	    blank_row (w, row, y);
	  goto finish_scroll_bars;
	}

      clear_glyph_matrix (w->desired_matrix);
    }

  /* Otherwise set up data on this window; select its buffer and point
     value.  */
  /* Really select the buffer, for the sake of buffer-local
     variables.  */
  set_buffer_internal_1 (XBUFFER (w->buffer));

  current_matrix_up_to_date_p
    = (!NILP (w->window_end_valid)
       && !current_buffer->clip_changed
       && !current_buffer->prevent_redisplay_optimizations_p
       && XFASTINT (w->last_modified) >= MODIFF
       && XFASTINT (w->last_overlay_modified) >= OVERLAY_MODIFF);

  /* Run the window-bottom-change-functions
     if it is possible that the text on the screen has changed
     (either due to modification of the text, or any other reason).  */
  if (!current_matrix_up_to_date_p
      && !NILP (Vwindow_text_change_functions))
    {
      safe_run_hooks (Qwindow_text_change_functions);
      goto restart;
    }

  beg_unchanged = BEG_UNCHANGED;
  end_unchanged = END_UNCHANGED;

  SET_TEXT_POS (opoint, PT, PT_BYTE);

  specbind (Qinhibit_point_motion_hooks, Qt);

  buffer_unchanged_p
    = (!NILP (w->window_end_valid)
       && !current_buffer->clip_changed
       && XFASTINT (w->last_modified) >= MODIFF
       && XFASTINT (w->last_overlay_modified) >= OVERLAY_MODIFF);

  /* When windows_or_buffers_changed is non-zero, we can't rely on
     the window end being valid, so set it to nil there.  */
  if (windows_or_buffers_changed)
    {
      /* If window starts on a continuation line, maybe adjust the
	 window start in case the window's width changed.  */
      if (XMARKER (w->start)->buffer == current_buffer)
	compute_window_start_on_continuation_line (w);

      w->window_end_valid = Qnil;
    }

  /* Some sanity checks.  */
  CHECK_WINDOW_END (w);
  if (Z == Z_BYTE && CHARPOS (opoint) != BYTEPOS (opoint))
    abort ();
  if (BYTEPOS (opoint) < CHARPOS (opoint))
    abort ();

  /* If %c is in mode line, update it if needed.  */
  if (!NILP (w->column_number_displayed)
      /* This alternative quickly identifies a common case
	 where no change is needed.  */
      && !(PT == XFASTINT (w->last_point)
	   && XFASTINT (w->last_modified) >= MODIFF
	   && XFASTINT (w->last_overlay_modified) >= OVERLAY_MODIFF)
      && (XFASTINT (w->column_number_displayed) != current_column ()))
    update_mode_line = 1;

  /* Count number of windows showing the selected buffer.  An indirect
     buffer counts as its base buffer.  */
  if (!just_this_one_p)
    {
      struct buffer *current_base, *window_base;
      current_base = current_buffer;
      window_base = XBUFFER (XWINDOW (selected_window)->buffer);
      if (current_base->base_buffer)
	current_base = current_base->base_buffer;
      if (window_base->base_buffer)
	window_base = window_base->base_buffer;
      if (current_base == window_base)
	buffer_shared++;
    }

  /* Point refers normally to the selected window.  For any other
     window, set up appropriate value.  */
  if (!EQ (window, selected_window))
    {
      EMACS_INT new_pt = XMARKER (w->pointm)->charpos;
      EMACS_INT new_pt_byte = marker_byte_position (w->pointm);
      if (new_pt < BEGV)
	{
	  new_pt = BEGV;
	  new_pt_byte = BEGV_BYTE;
	  set_marker_both (w->pointm, Qnil, BEGV, BEGV_BYTE);
	}
      else if (new_pt > (ZV - 1))
	{
	  new_pt = ZV;
	  new_pt_byte = ZV_BYTE;
	  set_marker_both (w->pointm, Qnil, ZV, ZV_BYTE);
	}

      /* We don't use SET_PT so that the point-motion hooks don't run.  */
      TEMP_SET_PT_BOTH (new_pt, new_pt_byte);
    }

  /* If any of the character widths specified in the display table
     have changed, invalidate the width run cache.  It's true that
     this may be a bit late to catch such changes, but the rest of
     redisplay goes (non-fatally) haywire when the display table is
     changed, so why should we worry about doing any better?  */
  if (current_buffer->width_run_cache)
    {
      struct Lisp_Char_Table *disptab = buffer_display_table ();

      if (! disptab_matches_widthtab (disptab,
                                      XVECTOR (BVAR (current_buffer, width_table))))
        {
          invalidate_region_cache (current_buffer,
                                   current_buffer->width_run_cache,
                                   BEG, Z);
          recompute_width_table (current_buffer, disptab);
        }
    }

  /* If window-start is screwed up, choose a new one.  */
  if (XMARKER (w->start)->buffer != current_buffer)
    goto recenter;

  SET_TEXT_POS_FROM_MARKER (startp, w->start);

  /* If someone specified a new starting point but did not insist,
     check whether it can be used.  */
  if (!NILP (w->optional_new_start)
      && CHARPOS (startp) >= BEGV
      && CHARPOS (startp) <= ZV)
    {
      w->optional_new_start = Qnil;
      start_display (&it, w, startp);
      move_it_to (&it, PT, 0, it.last_visible_y, -1,
		  MOVE_TO_POS | MOVE_TO_X | MOVE_TO_Y);
      if (IT_CHARPOS (it) == PT)
	w->force_start = Qt;
      /* IT may overshoot PT if text at PT is invisible.  */
      else if (IT_CHARPOS (it) > PT && CHARPOS (startp) <= PT)
	w->force_start = Qt;
    }

 force_start:

  /* Handle case where place to start displaying has been specified,
     unless the specified location is outside the accessible range.  */
  if (!NILP (w->force_start)
      || w->frozen_window_start_p)
    {
      /* We set this later on if we have to adjust point.  */
      int new_vpos = -1;

      w->force_start = Qnil;
      w->vscroll = 0;
      w->window_end_valid = Qnil;

      /* Forget any recorded base line for line number display.  */
      if (!buffer_unchanged_p)
	w->base_line_number = Qnil;

      /* Redisplay the mode line.  Select the buffer properly for that.
	 Also, run the hook window-scroll-functions
	 because we have scrolled.  */
      /* Note, we do this after clearing force_start because
	 if there's an error, it is better to forget about force_start
	 than to get into an infinite loop calling the hook functions
	 and having them get more errors.  */
      if (!update_mode_line
	  || ! NILP (Vwindow_scroll_functions))
	{
	  update_mode_line = 1;
	  w->update_mode_line = Qt;
	  startp = run_window_scroll_functions (window, startp);
	}

      w->last_modified = make_number (0);
      w->last_overlay_modified = make_number (0);
      if (CHARPOS (startp) < BEGV)
	SET_TEXT_POS (startp, BEGV, BEGV_BYTE);
      else if (CHARPOS (startp) > ZV)
	SET_TEXT_POS (startp, ZV, ZV_BYTE);

      /* Redisplay, then check if cursor has been set during the
	 redisplay.  Give up if new fonts were loaded.  */
      /* We used to issue a CHECK_MARGINS argument to try_window here,
	 but this causes scrolling to fail when point begins inside
	 the scroll margin (bug#148) -- cyd  */
      if (!try_window (window, startp, 0))
	{
	  w->force_start = Qt;
	  clear_glyph_matrix (w->desired_matrix);
	  goto need_larger_matrices;
	}

      if (w->cursor.vpos < 0 && !w->frozen_window_start_p)
	{
	  /* If point does not appear, try to move point so it does
	     appear. The desired matrix has been built above, so we
	     can use it here.  */
	  new_vpos = window_box_height (w) / 2;
	}

      if (!cursor_row_fully_visible_p (w, 0, 0))
	{
	  /* Point does appear, but on a line partly visible at end of window.
	     Move it back to a fully-visible line.  */
	  new_vpos = window_box_height (w);
	}

      /* If we need to move point for either of the above reasons,
	 now actually do it.  */
      if (new_vpos >= 0)
	{
	  struct glyph_row *row;

	  row = MATRIX_FIRST_TEXT_ROW (w->desired_matrix);
	  while (MATRIX_ROW_BOTTOM_Y (row) < new_vpos)
	    ++row;

	  TEMP_SET_PT_BOTH (MATRIX_ROW_START_CHARPOS (row),
			    MATRIX_ROW_START_BYTEPOS (row));

	  if (w != XWINDOW (selected_window))
	    set_marker_both (w->pointm, Qnil, PT, PT_BYTE);
	  else if (current_buffer == old)
	    SET_TEXT_POS (lpoint, PT, PT_BYTE);

	  set_cursor_from_row (w, row, w->desired_matrix, 0, 0, 0, 0);

	  /* If we are highlighting the region, then we just changed
	     the region, so redisplay to show it.  */
	  if (!NILP (Vtransient_mark_mode)
	      && !NILP (BVAR (current_buffer, mark_active)))
	    {
	      clear_glyph_matrix (w->desired_matrix);
	      if (!try_window (window, startp, 0))
		goto need_larger_matrices;
	    }
	}

#if GLYPH_DEBUG
      debug_method_add (w, "forced window start");
#endif
      goto done;
    }

  /* Handle case where text has not changed, only point, and it has
     not moved off the frame, and we are not retrying after hscroll.
     (current_matrix_up_to_date_p is nonzero when retrying.)  */
  if (current_matrix_up_to_date_p
      && (rc = try_cursor_movement (window, startp, &temp_scroll_step),
	  rc != CURSOR_MOVEMENT_CANNOT_BE_USED))
    {
      switch (rc)
	{
	case CURSOR_MOVEMENT_SUCCESS:
	  used_current_matrix_p = 1;
	  goto done;

	case CURSOR_MOVEMENT_MUST_SCROLL:
	  goto try_to_scroll;

	default:
	  abort ();
	}
    }
  /* If current starting point was originally the beginning of a line
     but no longer is, find a new starting point.  */
  else if (!NILP (w->start_at_line_beg)
	   && !(CHARPOS (startp) <= BEGV
		|| FETCH_BYTE (BYTEPOS (startp) - 1) == '\n'))
    {
#if GLYPH_DEBUG
      debug_method_add (w, "recenter 1");
#endif
      goto recenter;
    }

  /* Try scrolling with try_window_id.  Value is > 0 if update has
     been done, it is -1 if we know that the same window start will
     not work.  It is 0 if unsuccessful for some other reason.  */
  else if ((tem = try_window_id (w)) != 0)
    {
#if GLYPH_DEBUG
      debug_method_add (w, "try_window_id %d", tem);
#endif

      if (fonts_changed_p)
	goto need_larger_matrices;
      if (tem > 0)
	goto done;

      /* Otherwise try_window_id has returned -1 which means that we
	 don't want the alternative below this comment to execute.  */
    }
  else if (CHARPOS (startp) >= BEGV
	   && CHARPOS (startp) <= ZV
	   && PT >= CHARPOS (startp)
	   && (CHARPOS (startp) < ZV
	       /* Avoid starting at end of buffer.  */
	       || CHARPOS (startp) == BEGV
	       || (XFASTINT (w->last_modified) >= MODIFF
		   && XFASTINT (w->last_overlay_modified) >= OVERLAY_MODIFF)))
    {
      int d1, d2, d3, d4, d5, d6;

      /* If first window line is a continuation line, and window start
	 is inside the modified region, but the first change is before
	 current window start, we must select a new window start.

	 However, if this is the result of a down-mouse event (e.g. by
	 extending the mouse-drag-overlay), we don't want to select a
	 new window start, since that would change the position under
	 the mouse, resulting in an unwanted mouse-movement rather
	 than a simple mouse-click.  */
      if (NILP (w->start_at_line_beg)
	  && NILP (do_mouse_tracking)
      	  && CHARPOS (startp) > BEGV
	  && CHARPOS (startp) > BEG + beg_unchanged
	  && CHARPOS (startp) <= Z - end_unchanged
	  /* Even if w->start_at_line_beg is nil, a new window may
	     start at a line_beg, since that's how set_buffer_window
	     sets it.  So, we need to check the return value of
	     compute_window_start_on_continuation_line.  (See also
	     bug#197).  */
	  && XMARKER (w->start)->buffer == current_buffer
	  && compute_window_start_on_continuation_line (w)
	  /* It doesn't make sense to force the window start like we
	     do at label force_start if it is already known that point
	     will not be visible in the resulting window, because
	     doing so will move point from its correct position
	     instead of scrolling the window to bring point into view.
	     See bug#9324.  */
	  && pos_visible_p (w, PT, &d1, &d2, &d3, &d4, &d5, &d6))
	{
	  w->force_start = Qt;
	  SET_TEXT_POS_FROM_MARKER (startp, w->start);
	  goto force_start;
      	}

#if GLYPH_DEBUG
      debug_method_add (w, "same window start");
#endif

      /* Try to redisplay starting at same place as before.
         If point has not moved off frame, accept the results.  */
      if (!current_matrix_up_to_date_p
	  /* Don't use try_window_reusing_current_matrix in this case
	     because a window scroll function can have changed the
	     buffer.  */
	  || !NILP (Vwindow_scroll_functions)
	  || MINI_WINDOW_P (w)
	  || !(used_current_matrix_p
	       = try_window_reusing_current_matrix (w)))
	{
	  IF_DEBUG (debug_method_add (w, "1"));
	  if (try_window (window, startp, TRY_WINDOW_CHECK_MARGINS) < 0)
	    /* -1 means we need to scroll.
	       0 means we need new matrices, but fonts_changed_p
	       is set in that case, so we will detect it below.  */
	    goto try_to_scroll;
	}

      if (fonts_changed_p)
	goto need_larger_matrices;

      if (w->cursor.vpos >= 0)
	{
	  if (!just_this_one_p
	      || current_buffer->clip_changed
	      || BEG_UNCHANGED < CHARPOS (startp))
	    /* Forget any recorded base line for line number display.  */
	    w->base_line_number = Qnil;

	  if (!cursor_row_fully_visible_p (w, 1, 0))
	    {
	      clear_glyph_matrix (w->desired_matrix);
	      last_line_misfit = 1;
	    }
	    /* Drop through and scroll.  */
	  else
	    goto done;
	}
      else
	clear_glyph_matrix (w->desired_matrix);
    }

 try_to_scroll:

  w->last_modified = make_number (0);
  w->last_overlay_modified = make_number (0);

  /* Redisplay the mode line.  Select the buffer properly for that.  */
  if (!update_mode_line)
    {
      update_mode_line = 1;
      w->update_mode_line = Qt;
    }

  /* Try to scroll by specified few lines.  */
  if ((scroll_conservatively
       || emacs_scroll_step
       || temp_scroll_step
       || NUMBERP (BVAR (current_buffer, scroll_up_aggressively))
       || NUMBERP (BVAR (current_buffer, scroll_down_aggressively)))
      && CHARPOS (startp) >= BEGV
      && CHARPOS (startp) <= ZV)
    {
      /* The function returns -1 if new fonts were loaded, 1 if
	 successful, 0 if not successful.  */
      int ss = try_scrolling (window, just_this_one_p,
			      scroll_conservatively,
			      emacs_scroll_step,
			      temp_scroll_step, last_line_misfit);
      switch (ss)
	{
	case SCROLLING_SUCCESS:
	  goto done;

	case SCROLLING_NEED_LARGER_MATRICES:
	  goto need_larger_matrices;

	case SCROLLING_FAILED:
	  break;

	default:
	  abort ();
	}
    }

  /* Finally, just choose a place to start which positions point
     according to user preferences.  */

 recenter:

#if GLYPH_DEBUG
  debug_method_add (w, "recenter");
#endif

  /* w->vscroll = 0; */

  /* Forget any previously recorded base line for line number display.  */
  if (!buffer_unchanged_p)
    w->base_line_number = Qnil;

  /* Determine the window start relative to point.  */
  init_iterator (&it, w, PT, PT_BYTE, NULL, DEFAULT_FACE_ID);
  it.current_y = it.last_visible_y;
  if (centering_position < 0)
    {
      int margin =
	scroll_margin > 0
	? min (scroll_margin, WINDOW_TOTAL_LINES (w) / 4)
	: 0;
      EMACS_INT margin_pos = CHARPOS (startp);
      Lisp_Object aggressive;
      int scrolling_up;

      /* If there is a scroll margin at the top of the window, find
	 its character position.  */
      if (margin
	  /* Cannot call start_display if startp is not in the
	     accessible region of the buffer.  This can happen when we
	     have just switched to a different buffer and/or changed
	     its restriction.  In that case, startp is initialized to
	     the character position 1 (BEGV) because we did not yet
	     have chance to display the buffer even once.  */
	  && BEGV <= CHARPOS (startp) && CHARPOS (startp) <= ZV)
	{
	  struct it it1;
	  void *it1data = NULL;

	  SAVE_IT (it1, it, it1data);
	  start_display (&it1, w, startp);
	  move_it_vertically (&it1, margin * FRAME_LINE_HEIGHT (f));
	  margin_pos = IT_CHARPOS (it1);
	  RESTORE_IT (&it, &it, it1data);
	}
      scrolling_up = PT > margin_pos;
      aggressive =
	scrolling_up
	? BVAR (current_buffer, scroll_up_aggressively)
	: BVAR (current_buffer, scroll_down_aggressively);

      if (!MINI_WINDOW_P (w)
	  && (scroll_conservatively > SCROLL_LIMIT || NUMBERP (aggressive)))
	{
	  int pt_offset = 0;

	  /* Setting scroll-conservatively overrides
	     scroll-*-aggressively.  */
	  if (!scroll_conservatively && NUMBERP (aggressive))
	    {
	      double float_amount = XFLOATINT (aggressive);

	      pt_offset = float_amount * WINDOW_BOX_TEXT_HEIGHT (w);
	      if (pt_offset == 0 && float_amount > 0)
		pt_offset = 1;
	      if (pt_offset && margin > 0)
		margin -= 1;
	    }
	  /* Compute how much to move the window start backward from
	     point so that point will be displayed where the user
	     wants it.  */
	  if (scrolling_up)
	    {
	      centering_position = it.last_visible_y;
	      if (pt_offset)
		centering_position -= pt_offset;
	      centering_position -=
		FRAME_LINE_HEIGHT (f) * (1 + margin + (last_line_misfit != 0))
		+ WINDOW_HEADER_LINE_HEIGHT (w);
	      /* Don't let point enter the scroll margin near top of
		 the window.  */
	      if (centering_position < margin * FRAME_LINE_HEIGHT (f))
		centering_position = margin * FRAME_LINE_HEIGHT (f);
	    }
	  else
	    centering_position = margin * FRAME_LINE_HEIGHT (f) + pt_offset;
	}
      else
	/* Set the window start half the height of the window backward
	   from point.  */
	centering_position = window_box_height (w) / 2;
    }
  move_it_vertically_backward (&it, centering_position);

  xassert (IT_CHARPOS (it) >= BEGV);

  /* The function move_it_vertically_backward may move over more
     than the specified y-distance.  If it->w is small, e.g. a
     mini-buffer window, we may end up in front of the window's
     display area.  Start displaying at the start of the line
     containing PT in this case.  */
  if (it.current_y <= 0)
    {
      init_iterator (&it, w, PT, PT_BYTE, NULL, DEFAULT_FACE_ID);
      move_it_vertically_backward (&it, 0);
      it.current_y = 0;
    }

  it.current_x = it.hpos = 0;

  /* Set the window start position here explicitly, to avoid an
     infinite loop in case the functions in window-scroll-functions
     get errors.  */
  set_marker_both (w->start, Qnil, IT_CHARPOS (it), IT_BYTEPOS (it));

  /* Run scroll hooks.  */
  startp = run_window_scroll_functions (window, it.current.pos);

  /* Redisplay the window.  */
  if (!current_matrix_up_to_date_p
      || windows_or_buffers_changed
      || cursor_type_changed
      /* Don't use try_window_reusing_current_matrix in this case
	 because it can have changed the buffer.  */
      || !NILP (Vwindow_scroll_functions)
      || !just_this_one_p
      || MINI_WINDOW_P (w)
      || !(used_current_matrix_p
	   = try_window_reusing_current_matrix (w)))
    try_window (window, startp, 0);

  /* If new fonts have been loaded (due to fontsets), give up.  We
     have to start a new redisplay since we need to re-adjust glyph
     matrices.  */
  if (fonts_changed_p)
    goto need_larger_matrices;

  /* If cursor did not appear assume that the middle of the window is
     in the first line of the window.  Do it again with the next line.
     (Imagine a window of height 100, displaying two lines of height
     60.  Moving back 50 from it->last_visible_y will end in the first
     line.)  */
  if (w->cursor.vpos < 0)
    {
      if (!NILP (w->window_end_valid)
	  && PT >= Z - XFASTINT (w->window_end_pos))
	{
	  clear_glyph_matrix (w->desired_matrix);
	  move_it_by_lines (&it, 1);
	  try_window (window, it.current.pos, 0);
	}
      else if (PT < IT_CHARPOS (it))
	{
	  clear_glyph_matrix (w->desired_matrix);
	  move_it_by_lines (&it, -1);
	  try_window (window, it.current.pos, 0);
	}
      else
	{
	  /* Not much we can do about it.  */
	}
    }

  /* Consider the following case: Window starts at BEGV, there is
     invisible, intangible text at BEGV, so that display starts at
     some point START > BEGV.  It can happen that we are called with
     PT somewhere between BEGV and START.  Try to handle that case.  */
  if (w->cursor.vpos < 0)
    {
      struct glyph_row *row = w->current_matrix->rows;
      if (row->mode_line_p)
	++row;
      set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0);
    }

  if (!cursor_row_fully_visible_p (w, 0, 0))
    {
      /* If vscroll is enabled, disable it and try again.  */
      if (w->vscroll)
	{
	  w->vscroll = 0;
	  clear_glyph_matrix (w->desired_matrix);
	  goto recenter;
	}

      /* Users who set scroll-conservatively to a large number want
	 point just above/below the scroll margin.  If we ended up
	 with point's row partially visible, move the window start to
	 make that row fully visible and out of the margin.  */
      if (scroll_conservatively > SCROLL_LIMIT)
	{
	  int margin =
	    scroll_margin > 0
	    ? min (scroll_margin, WINDOW_TOTAL_LINES (w) / 4)
	    : 0;
	  int move_down = w->cursor.vpos >= WINDOW_TOTAL_LINES (w) / 2;

	  move_it_by_lines (&it, move_down ? margin + 1 : -(margin + 1));
	  clear_glyph_matrix (w->desired_matrix);
	  if (1 == try_window (window, it.current.pos,
			       TRY_WINDOW_CHECK_MARGINS))
	    goto done;
	}

      /* If centering point failed to make the whole line visible,
	 put point at the top instead.  That has to make the whole line
	 visible, if it can be done.  */
      if (centering_position == 0)
	goto done;

      clear_glyph_matrix (w->desired_matrix);
      centering_position = 0;
      goto recenter;
    }

 done:

  SET_TEXT_POS_FROM_MARKER (startp, w->start);
  w->start_at_line_beg = ((CHARPOS (startp) == BEGV
			   || FETCH_BYTE (BYTEPOS (startp) - 1) == '\n')
			  ? Qt : Qnil);

  /* Display the mode line, if we must.  */
  if ((update_mode_line
       /* If window not full width, must redo its mode line
	  if (a) the window to its side is being redone and
	  (b) we do a frame-based redisplay.  This is a consequence
	  of how inverted lines are drawn in frame-based redisplay.  */
       || (!just_this_one_p
	   && !FRAME_WINDOW_P (f)
	   && !WINDOW_FULL_WIDTH_P (w))
       /* Line number to display.  */
       || INTEGERP (w->base_line_pos)
       /* Column number is displayed and different from the one displayed.  */
       || (!NILP (w->column_number_displayed)
	   && (XFASTINT (w->column_number_displayed) != current_column ())))
      /* This means that the window has a mode line.  */
      && (WINDOW_WANTS_MODELINE_P (w)
	  || WINDOW_WANTS_HEADER_LINE_P (w)))
    {
      display_mode_lines (w);

      /* If mode line height has changed, arrange for a thorough
	 immediate redisplay using the correct mode line height.  */
      if (WINDOW_WANTS_MODELINE_P (w)
	  && CURRENT_MODE_LINE_HEIGHT (w) != DESIRED_MODE_LINE_HEIGHT (w))
	{
	  fonts_changed_p = 1;
	  MATRIX_MODE_LINE_ROW (w->current_matrix)->height
	    = DESIRED_MODE_LINE_HEIGHT (w);
	}

      /* If header line height has changed, arrange for a thorough
	 immediate redisplay using the correct header line height.  */
      if (WINDOW_WANTS_HEADER_LINE_P (w)
	  && CURRENT_HEADER_LINE_HEIGHT (w) != DESIRED_HEADER_LINE_HEIGHT (w))
	{
	  fonts_changed_p = 1;
	  MATRIX_HEADER_LINE_ROW (w->current_matrix)->height
	    = DESIRED_HEADER_LINE_HEIGHT (w);
	}

      if (fonts_changed_p)
	goto need_larger_matrices;
    }

  if (!line_number_displayed
      && !BUFFERP (w->base_line_pos))
    {
      w->base_line_pos = Qnil;
      w->base_line_number = Qnil;
    }

 finish_menu_bars:

  /* When we reach a frame's selected window, redo the frame's menu bar.  */
  if (update_mode_line
      && EQ (FRAME_SELECTED_WINDOW (f), window))
    {
      int redisplay_menu_p = 0;

      if (FRAME_WINDOW_P (f))
	{
#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) \
    || defined (HAVE_NS) || defined (USE_GTK)
	  redisplay_menu_p = FRAME_EXTERNAL_MENU_BAR (f);
#else
	  redisplay_menu_p = FRAME_MENU_BAR_LINES (f) > 0;
#endif
	}
      else
        redisplay_menu_p = FRAME_MENU_BAR_LINES (f) > 0;

      if (redisplay_menu_p)
        display_menu_bar (w);

#ifdef HAVE_WINDOW_SYSTEM
      if (FRAME_WINDOW_P (f))
        {
#if defined (USE_GTK) || defined (HAVE_NS)
	  if (FRAME_EXTERNAL_TOOL_BAR (f))
	    redisplay_tool_bar (f);
#else
	  if (WINDOWP (f->tool_bar_window)
	      && (FRAME_TOOL_BAR_LINES (f) > 0
		  || !NILP (Vauto_resize_tool_bars))
	      && redisplay_tool_bar (f))
	    ignore_mouse_drag_p = 1;
#endif
        }
#endif
    }

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f)
      && update_window_fringes (w, (just_this_one_p
				    || (!used_current_matrix_p && !overlay_arrow_seen)
				    || w->pseudo_window_p)))
    {
      update_begin (f);
      BLOCK_INPUT;
      if (draw_window_fringes (w, 1))
	x_draw_vertical_border (w);
      UNBLOCK_INPUT;
      update_end (f);
    }
#endif /* HAVE_WINDOW_SYSTEM */

  /* We go to this label, with fonts_changed_p nonzero,
     if it is necessary to try again using larger glyph matrices.
     We have to redeem the scroll bar even in this case,
     because the loop in redisplay_internal expects that.  */
 need_larger_matrices:
  ;
 finish_scroll_bars:

  if (WINDOW_HAS_VERTICAL_SCROLL_BAR (w))
    {
      /* Set the thumb's position and size.  */
      set_vertical_scroll_bar (w);

      /* Note that we actually used the scroll bar attached to this
	 window, so it shouldn't be deleted at the end of redisplay.  */
      if (FRAME_TERMINAL (f)->redeem_scroll_bar_hook)
        (*FRAME_TERMINAL (f)->redeem_scroll_bar_hook) (w);
    }

  /* Restore current_buffer and value of point in it.  The window
     update may have changed the buffer, so first make sure `opoint'
     is still valid (Bug#6177).  */
  if (CHARPOS (opoint) < BEGV)
    TEMP_SET_PT_BOTH (BEGV, BEGV_BYTE);
  else if (CHARPOS (opoint) > ZV)
    TEMP_SET_PT_BOTH (Z, Z_BYTE);
  else
    TEMP_SET_PT_BOTH (CHARPOS (opoint), BYTEPOS (opoint));

  set_buffer_internal_1 (old);
  /* Avoid an abort in TEMP_SET_PT_BOTH if the buffer has become
     shorter.  This can be caused by log truncation in *Messages*. */
  if (CHARPOS (lpoint) <= ZV)
    TEMP_SET_PT_BOTH (CHARPOS (lpoint), BYTEPOS (lpoint));

  unbind_to (count, Qnil);
}


/* Build the complete desired matrix of WINDOW with a window start
   buffer position POS.

   Value is 1 if successful.  It is zero if fonts were loaded during
   redisplay which makes re-adjusting glyph matrices necessary, and -1
   if point would appear in the scroll margins.
   (We check the former only if TRY_WINDOW_IGNORE_FONTS_CHANGE is
   unset in FLAGS, and the latter only if TRY_WINDOW_CHECK_MARGINS is
   set in FLAGS.)  */

int
try_window (Lisp_Object window, struct text_pos pos, int flags)
{
  struct window *w = XWINDOW (window);
  struct it it;
  struct glyph_row *last_text_row = NULL;
  struct frame *f = XFRAME (w->frame);

  /* Make POS the new window start.  */
  set_marker_both (w->start, Qnil, CHARPOS (pos), BYTEPOS (pos));

  /* Mark cursor position as unknown.  No overlay arrow seen.  */
  w->cursor.vpos = -1;
  overlay_arrow_seen = 0;

  /* Initialize iterator and info to start at POS.  */
  start_display (&it, w, pos);

  /* Display all lines of W.  */
  while (it.current_y < it.last_visible_y)
    {
      if (display_line (&it))
	last_text_row = it.glyph_row - 1;
      if (fonts_changed_p && !(flags & TRY_WINDOW_IGNORE_FONTS_CHANGE))
	return 0;
    }

  /* Don't let the cursor end in the scroll margins.  */
  if ((flags & TRY_WINDOW_CHECK_MARGINS)
      && !MINI_WINDOW_P (w))
    {
      int this_scroll_margin;

      if (scroll_margin > 0)
	{
	  this_scroll_margin = min (scroll_margin, WINDOW_TOTAL_LINES (w) / 4);
	  this_scroll_margin *= FRAME_LINE_HEIGHT (f);
	}
      else
	this_scroll_margin = 0;

      if ((w->cursor.y >= 0	/* not vscrolled */
	   && w->cursor.y < this_scroll_margin
	   && CHARPOS (pos) > BEGV
	   && IT_CHARPOS (it) < ZV)
	  /* rms: considering make_cursor_line_fully_visible_p here
	     seems to give wrong results.  We don't want to recenter
	     when the last line is partly visible, we want to allow
	     that case to be handled in the usual way.  */
	  || w->cursor.y > it.last_visible_y - this_scroll_margin - 1)
	{
	  w->cursor.vpos = -1;
	  clear_glyph_matrix (w->desired_matrix);
	  return -1;
	}
    }

  /* If bottom moved off end of frame, change mode line percentage.  */
  if (XFASTINT (w->window_end_pos) <= 0
      && Z != IT_CHARPOS (it))
    w->update_mode_line = Qt;

  /* Set window_end_pos to the offset of the last character displayed
     on the window from the end of current_buffer.  Set
     window_end_vpos to its row number.  */
  if (last_text_row)
    {
      xassert (MATRIX_ROW_DISPLAYS_TEXT_P (last_text_row));
      w->window_end_bytepos
	= Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_text_row);
      w->window_end_pos
	= make_number (Z - MATRIX_ROW_END_CHARPOS (last_text_row));
      w->window_end_vpos
	= make_number (MATRIX_ROW_VPOS (last_text_row, w->desired_matrix));
      xassert (MATRIX_ROW (w->desired_matrix, XFASTINT (w->window_end_vpos))
	       ->displays_text_p);
    }
  else
    {
      w->window_end_bytepos = Z_BYTE - ZV_BYTE;
      w->window_end_pos = make_number (Z - ZV);
      w->window_end_vpos = make_number (0);
    }

  /* But that is not valid info until redisplay finishes.  */
  w->window_end_valid = Qnil;
  return 1;
}



/************************************************************************
    Window redisplay reusing current matrix when buffer has not changed
 ************************************************************************/

/* Try redisplay of window W showing an unchanged buffer with a
   different window start than the last time it was displayed by
   reusing its current matrix.  Value is non-zero if successful.
   W->start is the new window start.  */

static int
try_window_reusing_current_matrix (struct window *w)
{
  struct frame *f = XFRAME (w->frame);
  struct glyph_row *bottom_row;
  struct it it;
  struct run run;
  struct text_pos start, new_start;
  int nrows_scrolled, i;
  struct glyph_row *last_text_row;
  struct glyph_row *last_reused_text_row;
  struct glyph_row *start_row;
  int start_vpos, min_y, max_y;

#if GLYPH_DEBUG
  if (inhibit_try_window_reusing)
    return 0;
#endif

  if (/* This function doesn't handle terminal frames.  */
      !FRAME_WINDOW_P (f)
      /* Don't try to reuse the display if windows have been split
	 or such.  */
      || windows_or_buffers_changed
      || cursor_type_changed)
    return 0;

  /* Can't do this if region may have changed.  */
  if ((!NILP (Vtransient_mark_mode)
       && !NILP (BVAR (current_buffer, mark_active)))
      || !NILP (w->region_showing)
      || !NILP (Vshow_trailing_whitespace))
    return 0;

  /* If top-line visibility has changed, give up.  */
  if (WINDOW_WANTS_HEADER_LINE_P (w)
      != MATRIX_HEADER_LINE_ROW (w->current_matrix)->mode_line_p)
    return 0;

  /* Give up if old or new display is scrolled vertically.  We could
     make this function handle this, but right now it doesn't.  */
  start_row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  if (w->vscroll || MATRIX_ROW_PARTIALLY_VISIBLE_P (w, start_row))
    return 0;

  /* The variable new_start now holds the new window start.  The old
     start `start' can be determined from the current matrix.  */
  SET_TEXT_POS_FROM_MARKER (new_start, w->start);
  start = start_row->minpos;
  start_vpos = MATRIX_ROW_VPOS (start_row, w->current_matrix);

  /* Clear the desired matrix for the display below.  */
  clear_glyph_matrix (w->desired_matrix);

  if (CHARPOS (new_start) <= CHARPOS (start))
    {
      /* Don't use this method if the display starts with an ellipsis
	 displayed for invisible text.  It's not easy to handle that case
	 below, and it's certainly not worth the effort since this is
	 not a frequent case.  */
      if (in_ellipses_for_invisible_text_p (&start_row->start, w))
	return 0;

      IF_DEBUG (debug_method_add (w, "twu1"));

      /* Display up to a row that can be reused.  The variable
	 last_text_row is set to the last row displayed that displays
	 text.  Note that it.vpos == 0 if or if not there is a
         header-line; it's not the same as the MATRIX_ROW_VPOS!  */
      start_display (&it, w, new_start);
      w->cursor.vpos = -1;
      last_text_row = last_reused_text_row = NULL;

      while (it.current_y < it.last_visible_y
	     && !fonts_changed_p)
	{
	  /* If we have reached into the characters in the START row,
	     that means the line boundaries have changed.  So we
	     can't start copying with the row START.  Maybe it will
	     work to start copying with the following row.  */
	  while (IT_CHARPOS (it) > CHARPOS (start))
	    {
	      /* Advance to the next row as the "start".  */
	      start_row++;
	      start = start_row->minpos;
	      /* If there are no more rows to try, or just one, give up.  */
	      if (start_row == MATRIX_MODE_LINE_ROW (w->current_matrix) - 1
		  || w->vscroll || MATRIX_ROW_PARTIALLY_VISIBLE_P (w, start_row)
		  || CHARPOS (start) == ZV)
		{
		  clear_glyph_matrix (w->desired_matrix);
		  return 0;
		}

	      start_vpos = MATRIX_ROW_VPOS (start_row, w->current_matrix);
	    }
	  /* If we have reached alignment, we can copy the rest of the
	     rows.  */
	  if (IT_CHARPOS (it) == CHARPOS (start)
	      /* Don't accept "alignment" inside a display vector,
		 since start_row could have started in the middle of
		 that same display vector (thus their character
		 positions match), and we have no way of telling if
		 that is the case.  */
	      && it.current.dpvec_index < 0)
	    break;

	  if (display_line (&it))
	    last_text_row = it.glyph_row - 1;

	}

      /* A value of current_y < last_visible_y means that we stopped
	 at the previous window start, which in turn means that we
	 have at least one reusable row.  */
      if (it.current_y < it.last_visible_y)
	{
	  struct glyph_row *row;

	  /* IT.vpos always starts from 0; it counts text lines.  */
	  nrows_scrolled = it.vpos - (start_row - MATRIX_FIRST_TEXT_ROW (w->current_matrix));

	  /* Find PT if not already found in the lines displayed.  */
	  if (w->cursor.vpos < 0)
	    {
	      int dy = it.current_y - start_row->y;

	      row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
	      row = row_containing_pos (w, PT, row, NULL, dy);
	      if (row)
		set_cursor_from_row (w, row, w->current_matrix, 0, 0,
				     dy, nrows_scrolled);
	      else
		{
		  clear_glyph_matrix (w->desired_matrix);
		  return 0;
		}
	    }

	  /* Scroll the display.  Do it before the current matrix is
	     changed.  The problem here is that update has not yet
	     run, i.e. part of the current matrix is not up to date.
	     scroll_run_hook will clear the cursor, and use the
	     current matrix to get the height of the row the cursor is
	     in.  */
	  run.current_y = start_row->y;
	  run.desired_y = it.current_y;
	  run.height = it.last_visible_y - it.current_y;

	  if (run.height > 0 && run.current_y != run.desired_y)
	    {
	      update_begin (f);
	      FRAME_RIF (f)->update_window_begin_hook (w);
	      FRAME_RIF (f)->clear_window_mouse_face (w);
	      FRAME_RIF (f)->scroll_run_hook (w, &run);
	      FRAME_RIF (f)->update_window_end_hook (w, 0, 0);
	      update_end (f);
	    }

	  /* Shift current matrix down by nrows_scrolled lines.  */
	  bottom_row = MATRIX_BOTTOM_TEXT_ROW (w->current_matrix, w);
	  rotate_matrix (w->current_matrix,
			 start_vpos,
			 MATRIX_ROW_VPOS (bottom_row, w->current_matrix),
			 nrows_scrolled);

	  /* Disable lines that must be updated.  */
	  for (i = 0; i < nrows_scrolled; ++i)
	    (start_row + i)->enabled_p = 0;

	  /* Re-compute Y positions.  */
	  min_y = WINDOW_HEADER_LINE_HEIGHT (w);
	  max_y = it.last_visible_y;
	  for (row = start_row + nrows_scrolled;
	       row < bottom_row;
	       ++row)
	    {
	      row->y = it.current_y;
	      row->visible_height = row->height;

	      if (row->y < min_y)
		row->visible_height -= min_y - row->y;
	      if (row->y + row->height > max_y)
		row->visible_height -= row->y + row->height - max_y;
	      if (row->fringe_bitmap_periodic_p)
		row->redraw_fringe_bitmaps_p = 1;

	      it.current_y += row->height;

	      if (MATRIX_ROW_DISPLAYS_TEXT_P (row))
		last_reused_text_row = row;
	      if (MATRIX_ROW_BOTTOM_Y (row) >= it.last_visible_y)
		break;
	    }

	  /* Disable lines in the current matrix which are now
	     below the window.  */
	  for (++row; row < bottom_row; ++row)
	    row->enabled_p = row->mode_line_p = 0;
	}

      /* Update window_end_pos etc.; last_reused_text_row is the last
	 reused row from the current matrix containing text, if any.
	 The value of last_text_row is the last displayed line
	 containing text.  */
      if (last_reused_text_row)
	{
	  w->window_end_bytepos
	    = Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_reused_text_row);
	  w->window_end_pos
	    = make_number (Z - MATRIX_ROW_END_CHARPOS (last_reused_text_row));
	  w->window_end_vpos
	    = make_number (MATRIX_ROW_VPOS (last_reused_text_row,
					    w->current_matrix));
	}
      else if (last_text_row)
	{
	  w->window_end_bytepos
	    = Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_text_row);
	  w->window_end_pos
	    = make_number (Z - MATRIX_ROW_END_CHARPOS (last_text_row));
	  w->window_end_vpos
	    = make_number (MATRIX_ROW_VPOS (last_text_row, w->desired_matrix));
	}
      else
	{
	  /* This window must be completely empty.  */
	  w->window_end_bytepos = Z_BYTE - ZV_BYTE;
	  w->window_end_pos = make_number (Z - ZV);
	  w->window_end_vpos = make_number (0);
	}
      w->window_end_valid = Qnil;

      /* Update hint: don't try scrolling again in update_window.  */
      w->desired_matrix->no_scrolling_p = 1;

#if GLYPH_DEBUG
      debug_method_add (w, "try_window_reusing_current_matrix 1");
#endif
      return 1;
    }
  else if (CHARPOS (new_start) > CHARPOS (start))
    {
      struct glyph_row *pt_row, *row;
      struct glyph_row *first_reusable_row;
      struct glyph_row *first_row_to_display;
      int dy;
      int yb = window_text_bottom_y (w);

      /* Find the row starting at new_start, if there is one.  Don't
	 reuse a partially visible line at the end.  */
      first_reusable_row = start_row;
      while (first_reusable_row->enabled_p
	     && MATRIX_ROW_BOTTOM_Y (first_reusable_row) < yb
	     && (MATRIX_ROW_START_CHARPOS (first_reusable_row)
		 < CHARPOS (new_start)))
	++first_reusable_row;

      /* Give up if there is no row to reuse.  */
      if (MATRIX_ROW_BOTTOM_Y (first_reusable_row) >= yb
	  || !first_reusable_row->enabled_p
	  || (MATRIX_ROW_START_CHARPOS (first_reusable_row)
	      != CHARPOS (new_start)))
	return 0;

      /* We can reuse fully visible rows beginning with
         first_reusable_row to the end of the window.  Set
         first_row_to_display to the first row that cannot be reused.
         Set pt_row to the row containing point, if there is any.  */
      pt_row = NULL;
      for (first_row_to_display = first_reusable_row;
	   MATRIX_ROW_BOTTOM_Y (first_row_to_display) < yb;
	   ++first_row_to_display)
	{
	  if (PT >= MATRIX_ROW_START_CHARPOS (first_row_to_display)
	      && (PT < MATRIX_ROW_END_CHARPOS (first_row_to_display)
		  || (PT == MATRIX_ROW_END_CHARPOS (first_row_to_display)
		      && first_row_to_display->ends_at_zv_p
		      && pt_row == NULL)))
	    pt_row = first_row_to_display;
	}

      /* Start displaying at the start of first_row_to_display.  */
      xassert (first_row_to_display->y < yb);
      init_to_row_start (&it, w, first_row_to_display);

      nrows_scrolled = (MATRIX_ROW_VPOS (first_reusable_row, w->current_matrix)
			- start_vpos);
      it.vpos = (MATRIX_ROW_VPOS (first_row_to_display, w->current_matrix)
		 - nrows_scrolled);
      it.current_y = (first_row_to_display->y - first_reusable_row->y
		      + WINDOW_HEADER_LINE_HEIGHT (w));

      /* Display lines beginning with first_row_to_display in the
         desired matrix.  Set last_text_row to the last row displayed
         that displays text.  */
      it.glyph_row = MATRIX_ROW (w->desired_matrix, it.vpos);
      if (pt_row == NULL)
	w->cursor.vpos = -1;
      last_text_row = NULL;
      while (it.current_y < it.last_visible_y && !fonts_changed_p)
	if (display_line (&it))
	  last_text_row = it.glyph_row - 1;

      /* If point is in a reused row, adjust y and vpos of the cursor
	 position.  */
      if (pt_row)
	{
	  w->cursor.vpos -= nrows_scrolled;
	  w->cursor.y -= first_reusable_row->y - start_row->y;
	}

      /* Give up if point isn't in a row displayed or reused.  (This
	 also handles the case where w->cursor.vpos < nrows_scrolled
	 after the calls to display_line, which can happen with scroll
	 margins.  See bug#1295.)  */
      if (w->cursor.vpos < 0)
	{
	  clear_glyph_matrix (w->desired_matrix);
	  return 0;
	}

      /* Scroll the display.  */
      run.current_y = first_reusable_row->y;
      run.desired_y = WINDOW_HEADER_LINE_HEIGHT (w);
      run.height = it.last_visible_y - run.current_y;
      dy = run.current_y - run.desired_y;

      if (run.height)
	{
	  update_begin (f);
	  FRAME_RIF (f)->update_window_begin_hook (w);
	  FRAME_RIF (f)->clear_window_mouse_face (w);
	  FRAME_RIF (f)->scroll_run_hook (w, &run);
	  FRAME_RIF (f)->update_window_end_hook (w, 0, 0);
	  update_end (f);
	}

      /* Adjust Y positions of reused rows.  */
      bottom_row = MATRIX_BOTTOM_TEXT_ROW (w->current_matrix, w);
      min_y = WINDOW_HEADER_LINE_HEIGHT (w);
      max_y = it.last_visible_y;
      for (row = first_reusable_row; row < first_row_to_display; ++row)
	{
	  row->y -= dy;
	  row->visible_height = row->height;
	  if (row->y < min_y)
	    row->visible_height -= min_y - row->y;
	  if (row->y + row->height > max_y)
	    row->visible_height -= row->y + row->height - max_y;
	  if (row->fringe_bitmap_periodic_p)
	    row->redraw_fringe_bitmaps_p = 1;
	}

      /* Scroll the current matrix.  */
      xassert (nrows_scrolled > 0);
      rotate_matrix (w->current_matrix,
		     start_vpos,
		     MATRIX_ROW_VPOS (bottom_row, w->current_matrix),
		     -nrows_scrolled);

      /* Disable rows not reused.  */
      for (row -= nrows_scrolled; row < bottom_row; ++row)
	row->enabled_p = 0;

      /* Point may have moved to a different line, so we cannot assume that
	 the previous cursor position is valid; locate the correct row.  */
      if (pt_row)
	{
	  for (row = MATRIX_ROW (w->current_matrix, w->cursor.vpos);
	       row < bottom_row
		 && PT >= MATRIX_ROW_END_CHARPOS (row)
		 && !row->ends_at_zv_p;
	       row++)
	    {
	      w->cursor.vpos++;
	      w->cursor.y = row->y;
	    }
	  if (row < bottom_row)
	    {
	      struct glyph *glyph = row->glyphs[TEXT_AREA] + w->cursor.hpos;
	      struct glyph *end = row->glyphs[TEXT_AREA] + row->used[TEXT_AREA];

	      /* Can't use this optimization with bidi-reordered glyph
		 rows, unless cursor is already at point. */
	      if (!NILP (BVAR (XBUFFER (w->buffer), bidi_display_reordering)))
		{
		  if (!(w->cursor.hpos >= 0
			&& w->cursor.hpos < row->used[TEXT_AREA]
			&& BUFFERP (glyph->object)
			&& glyph->charpos == PT))
		    return 0;
		}
	      else
		for (; glyph < end
		       && (!BUFFERP (glyph->object)
			   || glyph->charpos < PT);
		     glyph++)
		  {
		    w->cursor.hpos++;
		    w->cursor.x += glyph->pixel_width;
		  }
	    }
	}

      /* Adjust window end.  A null value of last_text_row means that
	 the window end is in reused rows which in turn means that
	 only its vpos can have changed.  */
      if (last_text_row)
	{
	  w->window_end_bytepos
	    = Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_text_row);
	  w->window_end_pos
	    = make_number (Z - MATRIX_ROW_END_CHARPOS (last_text_row));
	  w->window_end_vpos
	    = make_number (MATRIX_ROW_VPOS (last_text_row, w->desired_matrix));
	}
      else
	{
	  w->window_end_vpos
	    = make_number (XFASTINT (w->window_end_vpos) - nrows_scrolled);
	}

      w->window_end_valid = Qnil;
      w->desired_matrix->no_scrolling_p = 1;

#if GLYPH_DEBUG
      debug_method_add (w, "try_window_reusing_current_matrix 2");
#endif
      return 1;
    }

  return 0;
}



/************************************************************************
   Window redisplay reusing current matrix when buffer has changed
 ************************************************************************/

static struct glyph_row *find_last_unchanged_at_beg_row (struct window *);
static struct glyph_row *find_first_unchanged_at_end_row (struct window *,
                                                          EMACS_INT *, EMACS_INT *);
static struct glyph_row *
find_last_row_displaying_text (struct glyph_matrix *, struct it *,
                               struct glyph_row *);


/* Return the last row in MATRIX displaying text.  If row START is
   non-null, start searching with that row.  IT gives the dimensions
   of the display.  Value is null if matrix is empty; otherwise it is
   a pointer to the row found.  */

static struct glyph_row *
find_last_row_displaying_text (struct glyph_matrix *matrix, struct it *it,
			       struct glyph_row *start)
{
  struct glyph_row *row, *row_found;

  /* Set row_found to the last row in IT->w's current matrix
     displaying text.  The loop looks funny but think of partially
     visible lines.  */
  row_found = NULL;
  row = start ? start : MATRIX_FIRST_TEXT_ROW (matrix);
  while (MATRIX_ROW_DISPLAYS_TEXT_P (row))
    {
      xassert (row->enabled_p);
      row_found = row;
      if (MATRIX_ROW_BOTTOM_Y (row) >= it->last_visible_y)
	break;
      ++row;
    }

  return row_found;
}


/* Return the last row in the current matrix of W that is not affected
   by changes at the start of current_buffer that occurred since W's
   current matrix was built.  Value is null if no such row exists.

   BEG_UNCHANGED us the number of characters unchanged at the start of
   current_buffer.  BEG + BEG_UNCHANGED is the buffer position of the
   first changed character in current_buffer.  Characters at positions <
   BEG + BEG_UNCHANGED are at the same buffer positions as they were
   when the current matrix was built.  */

static struct glyph_row *
find_last_unchanged_at_beg_row (struct window *w)
{
  EMACS_INT first_changed_pos = BEG + BEG_UNCHANGED;
  struct glyph_row *row;
  struct glyph_row *row_found = NULL;
  int yb = window_text_bottom_y (w);

  /* Find the last row displaying unchanged text.  */
  for (row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
       MATRIX_ROW_DISPLAYS_TEXT_P (row)
	 && MATRIX_ROW_START_CHARPOS (row) < first_changed_pos;
       ++row)
    {
      if (/* If row ends before first_changed_pos, it is unchanged,
	     except in some case.  */
	  MATRIX_ROW_END_CHARPOS (row) <= first_changed_pos
	  /* When row ends in ZV and we write at ZV it is not
             unchanged.  */
	  && !row->ends_at_zv_p
	  /* When first_changed_pos is the end of a continued line,
	     row is not unchanged because it may be no longer
	     continued.  */
	  && !(MATRIX_ROW_END_CHARPOS (row) == first_changed_pos
	       && (row->continued_p
		   || row->exact_window_width_line_p))
	  /* If ROW->end is beyond ZV, then ROW->end is outdated and
	     needs to be recomputed, so don't consider this row as
	     unchanged.  This happens when the last line was
	     bidi-reordered and was killed immediately before this
	     redisplay cycle.  In that case, ROW->end stores the
	     buffer position of the first visual-order character of
	     the killed text, which is now beyond ZV.  */
	  && CHARPOS (row->end.pos) <= ZV)
	row_found = row;

      /* Stop if last visible row.  */
      if (MATRIX_ROW_BOTTOM_Y (row) >= yb)
	break;
    }

  return row_found;
}


/* Find the first glyph row in the current matrix of W that is not
   affected by changes at the end of current_buffer since the
   time W's current matrix was built.

   Return in *DELTA the number of chars by which buffer positions in
   unchanged text at the end of current_buffer must be adjusted.

   Return in *DELTA_BYTES the corresponding number of bytes.

   Value is null if no such row exists, i.e. all rows are affected by
   changes.  */

static struct glyph_row *
find_first_unchanged_at_end_row (struct window *w,
				 EMACS_INT *delta, EMACS_INT *delta_bytes)
{
  struct glyph_row *row;
  struct glyph_row *row_found = NULL;

  *delta = *delta_bytes = 0;

  /* Display must not have been paused, otherwise the current matrix
     is not up to date.  */
  eassert (!NILP (w->window_end_valid));

  /* A value of window_end_pos >= END_UNCHANGED means that the window
     end is in the range of changed text.  If so, there is no
     unchanged row at the end of W's current matrix.  */
  if (XFASTINT (w->window_end_pos) >= END_UNCHANGED)
    return NULL;

  /* Set row to the last row in W's current matrix displaying text.  */
  row = MATRIX_ROW (w->current_matrix, XFASTINT (w->window_end_vpos));

  /* If matrix is entirely empty, no unchanged row exists.  */
  if (MATRIX_ROW_DISPLAYS_TEXT_P (row))
    {
      /* The value of row is the last glyph row in the matrix having a
	 meaningful buffer position in it.  The end position of row
	 corresponds to window_end_pos.  This allows us to translate
	 buffer positions in the current matrix to current buffer
	 positions for characters not in changed text.  */
      EMACS_INT Z_old =
	MATRIX_ROW_END_CHARPOS (row) + XFASTINT (w->window_end_pos);
      EMACS_INT Z_BYTE_old =
	MATRIX_ROW_END_BYTEPOS (row) + w->window_end_bytepos;
      EMACS_INT last_unchanged_pos, last_unchanged_pos_old;
      struct glyph_row *first_text_row
	= MATRIX_FIRST_TEXT_ROW (w->current_matrix);

      *delta = Z - Z_old;
      *delta_bytes = Z_BYTE - Z_BYTE_old;

      /* Set last_unchanged_pos to the buffer position of the last
	 character in the buffer that has not been changed.  Z is the
	 index + 1 of the last character in current_buffer, i.e. by
	 subtracting END_UNCHANGED we get the index of the last
	 unchanged character, and we have to add BEG to get its buffer
	 position.  */
      last_unchanged_pos = Z - END_UNCHANGED + BEG;
      last_unchanged_pos_old = last_unchanged_pos - *delta;

      /* Search backward from ROW for a row displaying a line that
	 starts at a minimum position >= last_unchanged_pos_old.  */
      for (; row > first_text_row; --row)
	{
	  /* This used to abort, but it can happen.
	     It is ok to just stop the search instead here.  KFS.  */
	  if (!row->enabled_p || !MATRIX_ROW_DISPLAYS_TEXT_P (row))
	    break;

	  if (MATRIX_ROW_START_CHARPOS (row) >= last_unchanged_pos_old)
	    row_found = row;
	}
    }

  eassert (!row_found || MATRIX_ROW_DISPLAYS_TEXT_P (row_found));

  return row_found;
}


/* Make sure that glyph rows in the current matrix of window W
   reference the same glyph memory as corresponding rows in the
   frame's frame matrix.  This function is called after scrolling W's
   current matrix on a terminal frame in try_window_id and
   try_window_reusing_current_matrix.  */

static void
sync_frame_with_window_matrix_rows (struct window *w)
{
  struct frame *f = XFRAME (w->frame);
  struct glyph_row *window_row, *window_row_end, *frame_row;

  /* Preconditions: W must be a leaf window and full-width.  Its frame
     must have a frame matrix.  */
  xassert (NILP (w->hchild) && NILP (w->vchild));
  xassert (WINDOW_FULL_WIDTH_P (w));
  xassert (!FRAME_WINDOW_P (f));

  /* If W is a full-width window, glyph pointers in W's current matrix
     have, by definition, to be the same as glyph pointers in the
     corresponding frame matrix.  Note that frame matrices have no
     marginal areas (see build_frame_matrix).  */
  window_row = w->current_matrix->rows;
  window_row_end = window_row + w->current_matrix->nrows;
  frame_row = f->current_matrix->rows + WINDOW_TOP_EDGE_LINE (w);
  while (window_row < window_row_end)
    {
      struct glyph *start = window_row->glyphs[LEFT_MARGIN_AREA];
      struct glyph *end = window_row->glyphs[LAST_AREA];

      frame_row->glyphs[LEFT_MARGIN_AREA] = start;
      frame_row->glyphs[TEXT_AREA] = start;
      frame_row->glyphs[RIGHT_MARGIN_AREA] = end;
      frame_row->glyphs[LAST_AREA] = end;

      /* Disable frame rows whose corresponding window rows have
	 been disabled in try_window_id.  */
      if (!window_row->enabled_p)
	frame_row->enabled_p = 0;

      ++window_row, ++frame_row;
    }
}


/* Find the glyph row in window W containing CHARPOS.  Consider all
   rows between START and END (not inclusive).  END null means search
   all rows to the end of the display area of W.  Value is the row
   containing CHARPOS or null.  */

struct glyph_row *
row_containing_pos (struct window *w, EMACS_INT charpos,
		    struct glyph_row *start, struct glyph_row *end, int dy)
{
  struct glyph_row *row = start;
  struct glyph_row *best_row = NULL;
  EMACS_INT mindif = BUF_ZV (XBUFFER (w->buffer)) + 1;
  int last_y;

  /* If we happen to start on a header-line, skip that.  */
  if (row->mode_line_p)
    ++row;

  if ((end && row >= end) || !row->enabled_p)
    return NULL;

  last_y = window_text_bottom_y (w) - dy;

  while (1)
    {
      /* Give up if we have gone too far.  */
      if (end && row >= end)
	return NULL;
      /* This formerly returned if they were equal.
	 I think that both quantities are of a "last plus one" type;
	 if so, when they are equal, the row is within the screen. -- rms.  */
      if (MATRIX_ROW_BOTTOM_Y (row) > last_y)
	return NULL;

      /* If it is in this row, return this row.  */
      if (! (MATRIX_ROW_END_CHARPOS (row) < charpos
	     || (MATRIX_ROW_END_CHARPOS (row) == charpos
		 /* The end position of a row equals the start
		    position of the next row.  If CHARPOS is there, we
		    would rather display it in the next line, except
		    when this line ends in ZV.  */
		 && !row->ends_at_zv_p
		 && !MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (row)))
	  && charpos >= MATRIX_ROW_START_CHARPOS (row))
	{
	  struct glyph *g;

	  if (NILP (BVAR (XBUFFER (w->buffer), bidi_display_reordering))
	      || (!best_row && !row->continued_p))
	    return row;
	  /* In bidi-reordered rows, there could be several rows
	     occluding point, all of them belonging to the same
	     continued line.  We need to find the row which fits
	     CHARPOS the best.  */
	  for (g = row->glyphs[TEXT_AREA];
	       g < row->glyphs[TEXT_AREA] + row->used[TEXT_AREA];
	       g++)
	    {
	      if (!STRINGP (g->object))
		{
		  if (g->charpos > 0 && eabs (g->charpos - charpos) < mindif)
		    {
		      mindif = eabs (g->charpos - charpos);
		      best_row = row;
		      /* Exact match always wins.  */
		      if (mindif == 0)
			return best_row;
		    }
		}
	    }
	}
      else if (best_row && !row->continued_p)
	return best_row;
      ++row;
    }
}


/* Try to redisplay window W by reusing its existing display.  W's
   current matrix must be up to date when this function is called,
   i.e. window_end_valid must not be nil.

   Value is

   1	if display has been updated
   0	if otherwise unsuccessful
   -1	if redisplay with same window start is known not to succeed

   The following steps are performed:

   1. Find the last row in the current matrix of W that is not
   affected by changes at the start of current_buffer.  If no such row
   is found, give up.

   2. Find the first row in W's current matrix that is not affected by
   changes at the end of current_buffer.  Maybe there is no such row.

   3. Display lines beginning with the row + 1 found in step 1 to the
   row found in step 2 or, if step 2 didn't find a row, to the end of
   the window.

   4. If cursor is not known to appear on the window, give up.

   5. If display stopped at the row found in step 2, scroll the
   display and current matrix as needed.

   6. Maybe display some lines at the end of W, if we must.  This can
   happen under various circumstances, like a partially visible line
   becoming fully visible, or because newly displayed lines are displayed
   in smaller font sizes.

   7. Update W's window end information.  */

static int
try_window_id (struct window *w)
{
  struct frame *f = XFRAME (w->frame);
  struct glyph_matrix *current_matrix = w->current_matrix;
  struct glyph_matrix *desired_matrix = w->desired_matrix;
  struct glyph_row *last_unchanged_at_beg_row;
  struct glyph_row *first_unchanged_at_end_row;
  struct glyph_row *row;
  struct glyph_row *bottom_row;
  int bottom_vpos;
  struct it it;
  EMACS_INT delta = 0, delta_bytes = 0, stop_pos;
  int dvpos, dy;
  struct text_pos start_pos;
  struct run run;
  int first_unchanged_at_end_vpos = 0;
  struct glyph_row *last_text_row, *last_text_row_at_end;
  struct text_pos start;
  EMACS_INT first_changed_charpos, last_changed_charpos;

#if GLYPH_DEBUG
  if (inhibit_try_window_id)
    return 0;
#endif

  /* This is handy for debugging.  */
#if 0
#define GIVE_UP(X)						\
  do {								\
    fprintf (stderr, "try_window_id give up %d\n", (X));	\
    return 0;							\
  } while (0)
#else
#define GIVE_UP(X) return 0
#endif

  SET_TEXT_POS_FROM_MARKER (start, w->start);

  /* Don't use this for mini-windows because these can show
     messages and mini-buffers, and we don't handle that here.  */
  if (MINI_WINDOW_P (w))
    GIVE_UP (1);

  /* This flag is used to prevent redisplay optimizations.  */
  if (windows_or_buffers_changed || cursor_type_changed)
    GIVE_UP (2);

  /* Verify that narrowing has not changed.
     Also verify that we were not told to prevent redisplay optimizations.
     It would be nice to further
     reduce the number of cases where this prevents try_window_id.  */
  if (current_buffer->clip_changed
      || current_buffer->prevent_redisplay_optimizations_p)
    GIVE_UP (3);

  /* Window must either use window-based redisplay or be full width.  */
  if (!FRAME_WINDOW_P (f)
      && (!FRAME_LINE_INS_DEL_OK (f)
	  || !WINDOW_FULL_WIDTH_P (w)))
    GIVE_UP (4);

  /* Give up if point is known NOT to appear in W.  */
  if (PT < CHARPOS (start))
    GIVE_UP (5);

  /* Another way to prevent redisplay optimizations.  */
  if (XFASTINT (w->last_modified) == 0)
    GIVE_UP (6);

  /* Verify that window is not hscrolled.  */
  if (XFASTINT (w->hscroll) != 0)
    GIVE_UP (7);

  /* Verify that display wasn't paused.  */
  if (NILP (w->window_end_valid))
    GIVE_UP (8);

  /* Can't use this if highlighting a region because a cursor movement
     will do more than just set the cursor.  */
  if (!NILP (Vtransient_mark_mode)
      && !NILP (BVAR (current_buffer, mark_active)))
    GIVE_UP (9);

  /* Likewise if highlighting trailing whitespace.  */
  if (!NILP (Vshow_trailing_whitespace))
    GIVE_UP (11);

  /* Likewise if showing a region.  */
  if (!NILP (w->region_showing))
    GIVE_UP (10);

  /* Can't use this if overlay arrow position and/or string have
     changed.  */
  if (overlay_arrows_changed_p ())
    GIVE_UP (12);

  /* When word-wrap is on, adding a space to the first word of a
     wrapped line can change the wrap position, altering the line
     above it.  It might be worthwhile to handle this more
     intelligently, but for now just redisplay from scratch.  */
  if (!NILP (BVAR (XBUFFER (w->buffer), word_wrap)))
    GIVE_UP (21);

  /* Under bidi reordering, adding or deleting a character in the
     beginning of a paragraph, before the first strong directional
     character, can change the base direction of the paragraph (unless
     the buffer specifies a fixed paragraph direction), which will
     require to redisplay the whole paragraph.  It might be worthwhile
     to find the paragraph limits and widen the range of redisplayed
     lines to that, but for now just give up this optimization and
     redisplay from scratch.  */
  if (!NILP (BVAR (XBUFFER (w->buffer), bidi_display_reordering))
      && NILP (BVAR (XBUFFER (w->buffer), bidi_paragraph_direction)))
    GIVE_UP (22);

  /* Make sure beg_unchanged and end_unchanged are up to date.  Do it
     only if buffer has really changed.  The reason is that the gap is
     initially at Z for freshly visited files.  The code below would
     set end_unchanged to 0 in that case.  */
  if (MODIFF > SAVE_MODIFF
      /* This seems to happen sometimes after saving a buffer.  */
      || BEG_UNCHANGED + END_UNCHANGED > Z_BYTE)
    {
      if (GPT - BEG < BEG_UNCHANGED)
	BEG_UNCHANGED = GPT - BEG;
      if (Z - GPT < END_UNCHANGED)
	END_UNCHANGED = Z - GPT;
    }

  /* The position of the first and last character that has been changed.  */
  first_changed_charpos = BEG + BEG_UNCHANGED;
  last_changed_charpos  = Z - END_UNCHANGED;

  /* If window starts after a line end, and the last change is in
     front of that newline, then changes don't affect the display.
     This case happens with stealth-fontification.  Note that although
     the display is unchanged, glyph positions in the matrix have to
     be adjusted, of course.  */
  row = MATRIX_ROW (w->current_matrix, XFASTINT (w->window_end_vpos));
  if (MATRIX_ROW_DISPLAYS_TEXT_P (row)
      && ((last_changed_charpos < CHARPOS (start)
	   && CHARPOS (start) == BEGV)
	  || (last_changed_charpos < CHARPOS (start) - 1
	      && FETCH_BYTE (BYTEPOS (start) - 1) == '\n')))
    {
      EMACS_INT Z_old, Z_delta, Z_BYTE_old, Z_delta_bytes;
      struct glyph_row *r0;

      /* Compute how many chars/bytes have been added to or removed
	 from the buffer.  */
      Z_old = MATRIX_ROW_END_CHARPOS (row) + XFASTINT (w->window_end_pos);
      Z_BYTE_old = MATRIX_ROW_END_BYTEPOS (row) + w->window_end_bytepos;
      Z_delta = Z - Z_old;
      Z_delta_bytes = Z_BYTE - Z_BYTE_old;

      /* Give up if PT is not in the window.  Note that it already has
	 been checked at the start of try_window_id that PT is not in
	 front of the window start.  */
      if (PT >= MATRIX_ROW_END_CHARPOS (row) + Z_delta)
	GIVE_UP (13);

      /* If window start is unchanged, we can reuse the whole matrix
	 as is, after adjusting glyph positions.  No need to compute
	 the window end again, since its offset from Z hasn't changed.  */
      r0 = MATRIX_FIRST_TEXT_ROW (current_matrix);
      if (CHARPOS (start) == MATRIX_ROW_START_CHARPOS (r0) + Z_delta
	  && BYTEPOS (start) == MATRIX_ROW_START_BYTEPOS (r0) + Z_delta_bytes
	  /* PT must not be in a partially visible line.  */
	  && !(PT >= MATRIX_ROW_START_CHARPOS (row) + Z_delta
	       && MATRIX_ROW_BOTTOM_Y (row) > window_text_bottom_y (w)))
	{
	  /* Adjust positions in the glyph matrix.  */
	  if (Z_delta || Z_delta_bytes)
	    {
	      struct glyph_row *r1
		= MATRIX_BOTTOM_TEXT_ROW (current_matrix, w);
	      increment_matrix_positions (w->current_matrix,
					  MATRIX_ROW_VPOS (r0, current_matrix),
					  MATRIX_ROW_VPOS (r1, current_matrix),
					  Z_delta, Z_delta_bytes);
	    }

	  /* Set the cursor.  */
	  row = row_containing_pos (w, PT, r0, NULL, 0);
	  if (row)
	    set_cursor_from_row (w, row, current_matrix, 0, 0, 0, 0);
	  else
	    abort ();
	  return 1;
	}
    }

  /* Handle the case that changes are all below what is displayed in
     the window, and that PT is in the window.  This shortcut cannot
     be taken if ZV is visible in the window, and text has been added
     there that is visible in the window.  */
  if (first_changed_charpos >= MATRIX_ROW_END_CHARPOS (row)
      /* ZV is not visible in the window, or there are no
	 changes at ZV, actually.  */
      && (current_matrix->zv > MATRIX_ROW_END_CHARPOS (row)
	  || first_changed_charpos == last_changed_charpos))
    {
      struct glyph_row *r0;

      /* Give up if PT is not in the window.  Note that it already has
	 been checked at the start of try_window_id that PT is not in
	 front of the window start.  */
      if (PT >= MATRIX_ROW_END_CHARPOS (row))
	GIVE_UP (14);

      /* If window start is unchanged, we can reuse the whole matrix
	 as is, without changing glyph positions since no text has
	 been added/removed in front of the window end.  */
      r0 = MATRIX_FIRST_TEXT_ROW (current_matrix);
      if (TEXT_POS_EQUAL_P (start, r0->minpos)
	  /* PT must not be in a partially visible line.  */
	  && !(PT >= MATRIX_ROW_START_CHARPOS (row)
	       && MATRIX_ROW_BOTTOM_Y (row) > window_text_bottom_y (w)))
	{
	  /* We have to compute the window end anew since text
	     could have been added/removed after it.  */
	  w->window_end_pos
	    = make_number (Z - MATRIX_ROW_END_CHARPOS (row));
	  w->window_end_bytepos
	    = Z_BYTE - MATRIX_ROW_END_BYTEPOS (row);

	  /* Set the cursor.  */
	  row = row_containing_pos (w, PT, r0, NULL, 0);
	  if (row)
	    set_cursor_from_row (w, row, current_matrix, 0, 0, 0, 0);
	  else
	    abort ();
	  return 2;
	}
    }

  /* Give up if window start is in the changed area.

     The condition used to read

     (BEG_UNCHANGED + END_UNCHANGED != Z - BEG && ...)

     but why that was tested escapes me at the moment.  */
  if (CHARPOS (start) >= first_changed_charpos
      && CHARPOS (start) <= last_changed_charpos)
    GIVE_UP (15);

  /* Check that window start agrees with the start of the first glyph
     row in its current matrix.  Check this after we know the window
     start is not in changed text, otherwise positions would not be
     comparable.  */
  row = MATRIX_FIRST_TEXT_ROW (current_matrix);
  if (!TEXT_POS_EQUAL_P (start, row->minpos))
    GIVE_UP (16);

  /* Give up if the window ends in strings.  Overlay strings
     at the end are difficult to handle, so don't try.  */
  row = MATRIX_ROW (current_matrix, XFASTINT (w->window_end_vpos));
  if (MATRIX_ROW_START_CHARPOS (row) == MATRIX_ROW_END_CHARPOS (row))
    GIVE_UP (20);

  /* Compute the position at which we have to start displaying new
     lines.  Some of the lines at the top of the window might be
     reusable because they are not displaying changed text.  Find the
     last row in W's current matrix not affected by changes at the
     start of current_buffer.  Value is null if changes start in the
     first line of window.  */
  last_unchanged_at_beg_row = find_last_unchanged_at_beg_row (w);
  if (last_unchanged_at_beg_row)
    {
      /* Avoid starting to display in the middle of a character, a TAB
	 for instance.  This is easier than to set up the iterator
	 exactly, and it's not a frequent case, so the additional
	 effort wouldn't really pay off.  */
      while ((MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (last_unchanged_at_beg_row)
	      || last_unchanged_at_beg_row->ends_in_newline_from_string_p)
	     && last_unchanged_at_beg_row > w->current_matrix->rows)
	--last_unchanged_at_beg_row;

      if (MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (last_unchanged_at_beg_row))
	GIVE_UP (17);

      if (init_to_row_end (&it, w, last_unchanged_at_beg_row) == 0)
	GIVE_UP (18);
      start_pos = it.current.pos;

      /* Start displaying new lines in the desired matrix at the same
	 vpos we would use in the current matrix, i.e. below
	 last_unchanged_at_beg_row.  */
      it.vpos = 1 + MATRIX_ROW_VPOS (last_unchanged_at_beg_row,
				     current_matrix);
      it.glyph_row = MATRIX_ROW (desired_matrix, it.vpos);
      it.current_y = MATRIX_ROW_BOTTOM_Y (last_unchanged_at_beg_row);

      xassert (it.hpos == 0 && it.current_x == 0);
    }
  else
    {
      /* There are no reusable lines at the start of the window.
	 Start displaying in the first text line.  */
      start_display (&it, w, start);
      it.vpos = it.first_vpos;
      start_pos = it.current.pos;
    }

  /* Find the first row that is not affected by changes at the end of
     the buffer.  Value will be null if there is no unchanged row, in
     which case we must redisplay to the end of the window.  delta
     will be set to the value by which buffer positions beginning with
     first_unchanged_at_end_row have to be adjusted due to text
     changes.  */
  first_unchanged_at_end_row
    = find_first_unchanged_at_end_row (w, &delta, &delta_bytes);
  IF_DEBUG (debug_delta = delta);
  IF_DEBUG (debug_delta_bytes = delta_bytes);

  /* Set stop_pos to the buffer position up to which we will have to
     display new lines.  If first_unchanged_at_end_row != NULL, this
     is the buffer position of the start of the line displayed in that
     row.  For first_unchanged_at_end_row == NULL, use 0 to indicate
     that we don't stop at a buffer position.  */
  stop_pos = 0;
  if (first_unchanged_at_end_row)
    {
      xassert (last_unchanged_at_beg_row == NULL
	       || first_unchanged_at_end_row >= last_unchanged_at_beg_row);

      /* If this is a continuation line, move forward to the next one
	 that isn't.  Changes in lines above affect this line.
	 Caution: this may move first_unchanged_at_end_row to a row
	 not displaying text.  */
      while (MATRIX_ROW_CONTINUATION_LINE_P (first_unchanged_at_end_row)
	     && MATRIX_ROW_DISPLAYS_TEXT_P (first_unchanged_at_end_row)
	     && (MATRIX_ROW_BOTTOM_Y (first_unchanged_at_end_row)
		 < it.last_visible_y))
	++first_unchanged_at_end_row;

      if (!MATRIX_ROW_DISPLAYS_TEXT_P (first_unchanged_at_end_row)
	  || (MATRIX_ROW_BOTTOM_Y (first_unchanged_at_end_row)
	      >= it.last_visible_y))
	first_unchanged_at_end_row = NULL;
      else
	{
	  stop_pos = (MATRIX_ROW_START_CHARPOS (first_unchanged_at_end_row)
		      + delta);
	  first_unchanged_at_end_vpos
	    = MATRIX_ROW_VPOS (first_unchanged_at_end_row, current_matrix);
	  xassert (stop_pos >= Z - END_UNCHANGED);
	}
    }
  else if (last_unchanged_at_beg_row == NULL)
    GIVE_UP (19);


#if GLYPH_DEBUG

  /* Either there is no unchanged row at the end, or the one we have
     now displays text.  This is a necessary condition for the window
     end pos calculation at the end of this function.  */
  xassert (first_unchanged_at_end_row == NULL
	   || MATRIX_ROW_DISPLAYS_TEXT_P (first_unchanged_at_end_row));

  debug_last_unchanged_at_beg_vpos
    = (last_unchanged_at_beg_row
       ? MATRIX_ROW_VPOS (last_unchanged_at_beg_row, current_matrix)
       : -1);
  debug_first_unchanged_at_end_vpos = first_unchanged_at_end_vpos;

#endif /* GLYPH_DEBUG != 0 */


  /* Display new lines.  Set last_text_row to the last new line
     displayed which has text on it, i.e. might end up as being the
     line where the window_end_vpos is.  */
  w->cursor.vpos = -1;
  last_text_row = NULL;
  overlay_arrow_seen = 0;
  while (it.current_y < it.last_visible_y
	 && !fonts_changed_p
	 && (first_unchanged_at_end_row == NULL
	     || IT_CHARPOS (it) < stop_pos))
    {
      if (display_line (&it))
	last_text_row = it.glyph_row - 1;
    }

  if (fonts_changed_p)
    return -1;


  /* Compute differences in buffer positions, y-positions etc.  for
     lines reused at the bottom of the window.  Compute what we can
     scroll.  */
  if (first_unchanged_at_end_row
      /* No lines reused because we displayed everything up to the
         bottom of the window.  */
      && it.current_y < it.last_visible_y)
    {
      dvpos = (it.vpos
	       - MATRIX_ROW_VPOS (first_unchanged_at_end_row,
				  current_matrix));
      dy = it.current_y - first_unchanged_at_end_row->y;
      run.current_y = first_unchanged_at_end_row->y;
      run.desired_y = run.current_y + dy;
      run.height = it.last_visible_y - max (run.current_y, run.desired_y);
    }
  else
    {
      delta = delta_bytes = dvpos = dy
	= run.current_y = run.desired_y = run.height = 0;
      first_unchanged_at_end_row = NULL;
    }
  IF_DEBUG (debug_dvpos = dvpos; debug_dy = dy);


  /* Find the cursor if not already found.  We have to decide whether
     PT will appear on this window (it sometimes doesn't, but this is
     not a very frequent case.)  This decision has to be made before
     the current matrix is altered.  A value of cursor.vpos < 0 means
     that PT is either in one of the lines beginning at
     first_unchanged_at_end_row or below the window.  Don't care for
     lines that might be displayed later at the window end; as
     mentioned, this is not a frequent case.  */
  if (w->cursor.vpos < 0)
    {
      /* Cursor in unchanged rows at the top?  */
      if (PT < CHARPOS (start_pos)
	  && last_unchanged_at_beg_row)
	{
	  row = row_containing_pos (w, PT,
				    MATRIX_FIRST_TEXT_ROW (w->current_matrix),
				    last_unchanged_at_beg_row + 1, 0);
	  if (row)
	    set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0);
	}

      /* Start from first_unchanged_at_end_row looking for PT.  */
      else if (first_unchanged_at_end_row)
	{
	  row = row_containing_pos (w, PT - delta,
				    first_unchanged_at_end_row, NULL, 0);
	  if (row)
	    set_cursor_from_row (w, row, w->current_matrix, delta,
				 delta_bytes, dy, dvpos);
	}

      /* Give up if cursor was not found.  */
      if (w->cursor.vpos < 0)
	{
	  clear_glyph_matrix (w->desired_matrix);
	  return -1;
	}
    }

  /* Don't let the cursor end in the scroll margins.  */
  {
    int this_scroll_margin, cursor_height;

    this_scroll_margin =
      max (0, min (scroll_margin, WINDOW_TOTAL_LINES (w) / 4));
    this_scroll_margin *= FRAME_LINE_HEIGHT (it.f);
    cursor_height = MATRIX_ROW (w->desired_matrix, w->cursor.vpos)->height;

    if ((w->cursor.y < this_scroll_margin
	 && CHARPOS (start) > BEGV)
	/* Old redisplay didn't take scroll margin into account at the bottom,
	   but then global-hl-line-mode doesn't scroll.  KFS 2004-06-14 */
	|| (w->cursor.y + (make_cursor_line_fully_visible_p
			   ? cursor_height + this_scroll_margin
			   : 1)) > it.last_visible_y)
      {
	w->cursor.vpos = -1;
	clear_glyph_matrix (w->desired_matrix);
	return -1;
      }
  }

  /* Scroll the display.  Do it before changing the current matrix so
     that xterm.c doesn't get confused about where the cursor glyph is
     found.  */
  if (dy && run.height)
    {
      update_begin (f);

      if (FRAME_WINDOW_P (f))
	{
	  FRAME_RIF (f)->update_window_begin_hook (w);
	  FRAME_RIF (f)->clear_window_mouse_face (w);
	  FRAME_RIF (f)->scroll_run_hook (w, &run);
	  FRAME_RIF (f)->update_window_end_hook (w, 0, 0);
	}
      else
	{
	  /* Terminal frame.  In this case, dvpos gives the number of
	     lines to scroll by; dvpos < 0 means scroll up.  */
	  int from_vpos
	    = MATRIX_ROW_VPOS (first_unchanged_at_end_row, w->current_matrix);
	  int from = WINDOW_TOP_EDGE_LINE (w) + from_vpos;
	  int end = (WINDOW_TOP_EDGE_LINE (w)
		     + (WINDOW_WANTS_HEADER_LINE_P (w) ? 1 : 0)
		     + window_internal_height (w));

#if defined (HAVE_GPM) || defined (MSDOS)
	  x_clear_window_mouse_face (w);
#endif
	  /* Perform the operation on the screen.  */
	  if (dvpos > 0)
	    {
	      /* Scroll last_unchanged_at_beg_row to the end of the
		 window down dvpos lines.  */
	      set_terminal_window (f, end);

	      /* On dumb terminals delete dvpos lines at the end
		 before inserting dvpos empty lines.  */
	      if (!FRAME_SCROLL_REGION_OK (f))
		ins_del_lines (f, end - dvpos, -dvpos);

	      /* Insert dvpos empty lines in front of
                 last_unchanged_at_beg_row.  */
	      ins_del_lines (f, from, dvpos);
	    }
	  else if (dvpos < 0)
	    {
	      /* Scroll up last_unchanged_at_beg_vpos to the end of
		 the window to last_unchanged_at_beg_vpos - |dvpos|.  */
	      set_terminal_window (f, end);

	      /* Delete dvpos lines in front of
		 last_unchanged_at_beg_vpos.  ins_del_lines will set
		 the cursor to the given vpos and emit |dvpos| delete
		 line sequences.  */
	      ins_del_lines (f, from + dvpos, dvpos);

	      /* On a dumb terminal insert dvpos empty lines at the
                 end.  */
	      if (!FRAME_SCROLL_REGION_OK (f))
		ins_del_lines (f, end + dvpos, -dvpos);
	    }

	  set_terminal_window (f, 0);
	}

      update_end (f);
    }

  /* Shift reused rows of the current matrix to the right position.
     BOTTOM_ROW is the last + 1 row in the current matrix reserved for
     text.  */
  bottom_row = MATRIX_BOTTOM_TEXT_ROW (current_matrix, w);
  bottom_vpos = MATRIX_ROW_VPOS (bottom_row, current_matrix);
  if (dvpos < 0)
    {
      rotate_matrix (current_matrix, first_unchanged_at_end_vpos + dvpos,
		     bottom_vpos, dvpos);
      enable_glyph_matrix_rows (current_matrix, bottom_vpos + dvpos,
				bottom_vpos, 0);
    }
  else if (dvpos > 0)
    {
      rotate_matrix (current_matrix, first_unchanged_at_end_vpos,
		     bottom_vpos, dvpos);
      enable_glyph_matrix_rows (current_matrix, first_unchanged_at_end_vpos,
				first_unchanged_at_end_vpos + dvpos, 0);
    }

  /* For frame-based redisplay, make sure that current frame and window
     matrix are in sync with respect to glyph memory.  */
  if (!FRAME_WINDOW_P (f))
    sync_frame_with_window_matrix_rows (w);

  /* Adjust buffer positions in reused rows.  */
  if (delta || delta_bytes)
    increment_matrix_positions (current_matrix,
				first_unchanged_at_end_vpos + dvpos,
				bottom_vpos, delta, delta_bytes);

  /* Adjust Y positions.  */
  if (dy)
    shift_glyph_matrix (w, current_matrix,
			first_unchanged_at_end_vpos + dvpos,
			bottom_vpos, dy);

  if (first_unchanged_at_end_row)
    {
      first_unchanged_at_end_row += dvpos;
      if (first_unchanged_at_end_row->y >= it.last_visible_y
	  || !MATRIX_ROW_DISPLAYS_TEXT_P (first_unchanged_at_end_row))
	first_unchanged_at_end_row = NULL;
    }

  /* If scrolling up, there may be some lines to display at the end of
     the window.  */
  last_text_row_at_end = NULL;
  if (dy < 0)
    {
      /* Scrolling up can leave for example a partially visible line
	 at the end of the window to be redisplayed.  */
      /* Set last_row to the glyph row in the current matrix where the
	 window end line is found.  It has been moved up or down in
	 the matrix by dvpos.  */
      int last_vpos = XFASTINT (w->window_end_vpos) + dvpos;
      struct glyph_row *last_row = MATRIX_ROW (current_matrix, last_vpos);

      /* If last_row is the window end line, it should display text.  */
      xassert (last_row->displays_text_p);

      /* If window end line was partially visible before, begin
	 displaying at that line.  Otherwise begin displaying with the
	 line following it.  */
      if (MATRIX_ROW_BOTTOM_Y (last_row) - dy >= it.last_visible_y)
	{
	  init_to_row_start (&it, w, last_row);
	  it.vpos = last_vpos;
	  it.current_y = last_row->y;
	}
      else
	{
	  init_to_row_end (&it, w, last_row);
	  it.vpos = 1 + last_vpos;
	  it.current_y = MATRIX_ROW_BOTTOM_Y (last_row);
	  ++last_row;
	}

      /* We may start in a continuation line.  If so, we have to
	 get the right continuation_lines_width and current_x.  */
      it.continuation_lines_width = last_row->continuation_lines_width;
      it.hpos = it.current_x = 0;

      /* Display the rest of the lines at the window end.  */
      it.glyph_row = MATRIX_ROW (desired_matrix, it.vpos);
      while (it.current_y < it.last_visible_y
	     && !fonts_changed_p)
	{
	  /* Is it always sure that the display agrees with lines in
	     the current matrix?  I don't think so, so we mark rows
	     displayed invalid in the current matrix by setting their
	     enabled_p flag to zero.  */
	  MATRIX_ROW (w->current_matrix, it.vpos)->enabled_p = 0;
	  if (display_line (&it))
	    last_text_row_at_end = it.glyph_row - 1;
	}
    }

  /* Update window_end_pos and window_end_vpos.  */
  if (first_unchanged_at_end_row
      && !last_text_row_at_end)
    {
      /* Window end line if one of the preserved rows from the current
	 matrix.  Set row to the last row displaying text in current
	 matrix starting at first_unchanged_at_end_row, after
	 scrolling.  */
      xassert (first_unchanged_at_end_row->displays_text_p);
      row = find_last_row_displaying_text (w->current_matrix, &it,
					   first_unchanged_at_end_row);
      xassert (row && MATRIX_ROW_DISPLAYS_TEXT_P (row));

      w->window_end_pos = make_number (Z - MATRIX_ROW_END_CHARPOS (row));
      w->window_end_bytepos = Z_BYTE - MATRIX_ROW_END_BYTEPOS (row);
      w->window_end_vpos
	= make_number (MATRIX_ROW_VPOS (row, w->current_matrix));
      xassert (w->window_end_bytepos >= 0);
      IF_DEBUG (debug_method_add (w, "A"));
    }
  else if (last_text_row_at_end)
    {
      w->window_end_pos
	= make_number (Z - MATRIX_ROW_END_CHARPOS (last_text_row_at_end));
      w->window_end_bytepos
	= Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_text_row_at_end);
      w->window_end_vpos
	= make_number (MATRIX_ROW_VPOS (last_text_row_at_end, desired_matrix));
      xassert (w->window_end_bytepos >= 0);
      IF_DEBUG (debug_method_add (w, "B"));
    }
  else if (last_text_row)
    {
      /* We have displayed either to the end of the window or at the
	 end of the window, i.e. the last row with text is to be found
	 in the desired matrix.  */
      w->window_end_pos
	= make_number (Z - MATRIX_ROW_END_CHARPOS (last_text_row));
      w->window_end_bytepos
	= Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_text_row);
      w->window_end_vpos
	= make_number (MATRIX_ROW_VPOS (last_text_row, desired_matrix));
      xassert (w->window_end_bytepos >= 0);
    }
  else if (first_unchanged_at_end_row == NULL
	   && last_text_row == NULL
	   && last_text_row_at_end == NULL)
    {
      /* Displayed to end of window, but no line containing text was
	 displayed.  Lines were deleted at the end of the window.  */
      int first_vpos = WINDOW_WANTS_HEADER_LINE_P (w) ? 1 : 0;
      int vpos = XFASTINT (w->window_end_vpos);
      struct glyph_row *current_row = current_matrix->rows + vpos;
      struct glyph_row *desired_row = desired_matrix->rows + vpos;

      for (row = NULL;
	   row == NULL && vpos >= first_vpos;
	   --vpos, --current_row, --desired_row)
	{
	  if (desired_row->enabled_p)
	    {
	      if (desired_row->displays_text_p)
		row = desired_row;
	    }
	  else if (current_row->displays_text_p)
	    row  = current_row;
	}

      xassert (row != NULL);
      w->window_end_vpos = make_number (vpos + 1);
      w->window_end_pos = make_number (Z - MATRIX_ROW_END_CHARPOS (row));
      w->window_end_bytepos = Z_BYTE - MATRIX_ROW_END_BYTEPOS (row);
      xassert (w->window_end_bytepos >= 0);
      IF_DEBUG (debug_method_add (w, "C"));
    }
  else
    abort ();

  IF_DEBUG (debug_end_pos = XFASTINT (w->window_end_pos);
	    debug_end_vpos = XFASTINT (w->window_end_vpos));

  /* Record that display has not been completed.  */
  w->window_end_valid = Qnil;
  w->desired_matrix->no_scrolling_p = 1;
  return 3;

#undef GIVE_UP
}



/***********************************************************************
			More debugging support
 ***********************************************************************/

#if GLYPH_DEBUG

void dump_glyph_row (struct glyph_row *, int, int) EXTERNALLY_VISIBLE;
void dump_glyph_matrix (struct glyph_matrix *, int) EXTERNALLY_VISIBLE;
void dump_glyph (struct glyph_row *, struct glyph *, int) EXTERNALLY_VISIBLE;


/* Dump the contents of glyph matrix MATRIX on stderr.

   GLYPHS 0 means don't show glyph contents.
   GLYPHS 1 means show glyphs in short form
   GLYPHS > 1 means show glyphs in long form.  */

void
dump_glyph_matrix (struct glyph_matrix *matrix, int glyphs)
{
  int i;
  for (i = 0; i < matrix->nrows; ++i)
    dump_glyph_row (MATRIX_ROW (matrix, i), i, glyphs);
}


/* Dump contents of glyph GLYPH to stderr.  ROW and AREA are
   the glyph row and area where the glyph comes from.  */

void
dump_glyph (struct glyph_row *row, struct glyph *glyph, int area)
{
  if (glyph->type == CHAR_GLYPH)
    {
      fprintf (stderr,
	       "  %5td %4c %6"pI"d %c %3d 0x%05x %c %4d %1.1d%1.1d\n",
	       glyph - row->glyphs[TEXT_AREA],
	       'C',
	       glyph->charpos,
	       (BUFFERP (glyph->object)
		? 'B'
		: (STRINGP (glyph->object)
		   ? 'S'
		   : '-')),
	       glyph->pixel_width,
	       glyph->u.ch,
	       (glyph->u.ch < 0x80 && glyph->u.ch >= ' '
		? glyph->u.ch
		: '.'),
	       glyph->face_id,
	       glyph->left_box_line_p,
	       glyph->right_box_line_p);
    }
  else if (glyph->type == STRETCH_GLYPH)
    {
      fprintf (stderr,
	       "  %5td %4c %6"pI"d %c %3d 0x%05x %c %4d %1.1d%1.1d\n",
	       glyph - row->glyphs[TEXT_AREA],
	       'S',
	       glyph->charpos,
	       (BUFFERP (glyph->object)
		? 'B'
		: (STRINGP (glyph->object)
		   ? 'S'
		   : '-')),
	       glyph->pixel_width,
	       0,
	       '.',
	       glyph->face_id,
	       glyph->left_box_line_p,
	       glyph->right_box_line_p);
    }
  else if (glyph->type == IMAGE_GLYPH)
    {
      fprintf (stderr,
	       "  %5td %4c %6"pI"d %c %3d 0x%05x %c %4d %1.1d%1.1d\n",
	       glyph - row->glyphs[TEXT_AREA],
	       'I',
	       glyph->charpos,
	       (BUFFERP (glyph->object)
		? 'B'
		: (STRINGP (glyph->object)
		   ? 'S'
		   : '-')),
	       glyph->pixel_width,
	       glyph->u.img_id,
	       '.',
	       glyph->face_id,
	       glyph->left_box_line_p,
	       glyph->right_box_line_p);
    }
  else if (glyph->type == COMPOSITE_GLYPH)
    {
      fprintf (stderr,
	       "  %5td %4c %6"pI"d %c %3d 0x%05x",
	       glyph - row->glyphs[TEXT_AREA],
	       '+',
	       glyph->charpos,
	       (BUFFERP (glyph->object)
		? 'B'
		: (STRINGP (glyph->object)
		   ? 'S'
		   : '-')),
	       glyph->pixel_width,
	       glyph->u.cmp.id);
      if (glyph->u.cmp.automatic)
	fprintf (stderr,
		 "[%d-%d]",
		 glyph->slice.cmp.from, glyph->slice.cmp.to);
      fprintf (stderr, " . %4d %1.1d%1.1d\n",
	       glyph->face_id,
	       glyph->left_box_line_p,
	       glyph->right_box_line_p);
    }
}


/* Dump the contents of glyph row at VPOS in MATRIX to stderr.
   GLYPHS 0 means don't show glyph contents.
   GLYPHS 1 means show glyphs in short form
   GLYPHS > 1 means show glyphs in long form.  */

void
dump_glyph_row (struct glyph_row *row, int vpos, int glyphs)
{
  if (glyphs != 1)
    {
      fprintf (stderr, "Row Start   End Used oE><\\CTZFesm     X    Y    W    H    V    A    P\n");
      fprintf (stderr, "======================================================================\n");

      fprintf (stderr, "%3d %5"pI"d %5"pI"d %4d %1.1d%1.1d%1.1d%1.1d\
%1.1d%1.1d%1.1d%1.1d%1.1d%1.1d%1.1d%1.1d  %4d %4d %4d %4d %4d %4d %4d\n",
	       vpos,
	       MATRIX_ROW_START_CHARPOS (row),
	       MATRIX_ROW_END_CHARPOS (row),
	       row->used[TEXT_AREA],
	       row->contains_overlapping_glyphs_p,
	       row->enabled_p,
	       row->truncated_on_left_p,
	       row->truncated_on_right_p,
	       row->continued_p,
	       MATRIX_ROW_CONTINUATION_LINE_P (row),
	       row->displays_text_p,
	       row->ends_at_zv_p,
	       row->fill_line_p,
	       row->ends_in_middle_of_char_p,
	       row->starts_in_middle_of_char_p,
	       row->mouse_face_p,
	       row->x,
	       row->y,
	       row->pixel_width,
	       row->height,
	       row->visible_height,
	       row->ascent,
	       row->phys_ascent);
      fprintf (stderr, "%9d %5d\t%5d\n", row->start.overlay_string_index,
	       row->end.overlay_string_index,
	       row->continuation_lines_width);
      fprintf (stderr, "%9"pI"d %5"pI"d\n",
	       CHARPOS (row->start.string_pos),
	       CHARPOS (row->end.string_pos));
      fprintf (stderr, "%9d %5d\n", row->start.dpvec_index,
	       row->end.dpvec_index);
    }

  if (glyphs > 1)
    {
      int area;

      for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	{
	  struct glyph *glyph = row->glyphs[area];
	  struct glyph *glyph_end = glyph + row->used[area];

	  /* Glyph for a line end in text.  */
	  if (area == TEXT_AREA && glyph == glyph_end && glyph->charpos > 0)
	    ++glyph_end;

	  if (glyph < glyph_end)
	    fprintf (stderr, "  Glyph    Type Pos   O W    Code C Face LR\n");

	  for (; glyph < glyph_end; ++glyph)
	    dump_glyph (row, glyph, area);
	}
    }
  else if (glyphs == 1)
    {
      int area;

      for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	{
	  char *s = (char *) alloca (row->used[area] + 1);
	  int i;

	  for (i = 0; i < row->used[area]; ++i)
	    {
	      struct glyph *glyph = row->glyphs[area] + i;
	      if (glyph->type == CHAR_GLYPH
		  && glyph->u.ch < 0x80
		  && glyph->u.ch >= ' ')
		s[i] = glyph->u.ch;
	      else
		s[i] = '.';
	    }

	  s[i] = '\0';
	  fprintf (stderr, "%3d: (%d) '%s'\n", vpos, row->enabled_p, s);
	}
    }
}


DEFUN ("dump-glyph-matrix", Fdump_glyph_matrix,
       Sdump_glyph_matrix, 0, 1, "p",
       doc: /* Dump the current matrix of the selected window to stderr.
Shows contents of glyph row structures.  With non-nil
parameter GLYPHS, dump glyphs as well.  If GLYPHS is 1 show
glyphs in short form, otherwise show glyphs in long form.  */)
  (Lisp_Object glyphs)
{
  struct window *w = XWINDOW (selected_window);
  struct buffer *buffer = XBUFFER (w->buffer);

  fprintf (stderr, "PT = %"pI"d, BEGV = %"pI"d. ZV = %"pI"d\n",
	   BUF_PT (buffer), BUF_BEGV (buffer), BUF_ZV (buffer));
  fprintf (stderr, "Cursor x = %d, y = %d, hpos = %d, vpos = %d\n",
	   w->cursor.x, w->cursor.y, w->cursor.hpos, w->cursor.vpos);
  fprintf (stderr, "=============================================\n");
  dump_glyph_matrix (w->current_matrix,
		     NILP (glyphs) ? 0 : XINT (glyphs));
  return Qnil;
}


DEFUN ("dump-frame-glyph-matrix", Fdump_frame_glyph_matrix,
       Sdump_frame_glyph_matrix, 0, 0, "", doc: /* */)
  (void)
{
  struct frame *f = XFRAME (selected_frame);
  dump_glyph_matrix (f->current_matrix, 1);
  return Qnil;
}


DEFUN ("dump-glyph-row", Fdump_glyph_row, Sdump_glyph_row, 1, 2, "",
       doc: /* Dump glyph row ROW to stderr.
GLYPH 0 means don't dump glyphs.
GLYPH 1 means dump glyphs in short form.
GLYPH > 1 or omitted means dump glyphs in long form.  */)
  (Lisp_Object row, Lisp_Object glyphs)
{
  struct glyph_matrix *matrix;
  int vpos;

  CHECK_NUMBER (row);
  matrix = XWINDOW (selected_window)->current_matrix;
  vpos = XINT (row);
  if (vpos >= 0 && vpos < matrix->nrows)
    dump_glyph_row (MATRIX_ROW (matrix, vpos),
		    vpos,
		    INTEGERP (glyphs) ? XINT (glyphs) : 2);
  return Qnil;
}


DEFUN ("dump-tool-bar-row", Fdump_tool_bar_row, Sdump_tool_bar_row, 1, 2, "",
       doc: /* Dump glyph row ROW of the tool-bar of the current frame to stderr.
GLYPH 0 means don't dump glyphs.
GLYPH 1 means dump glyphs in short form.
GLYPH > 1 or omitted means dump glyphs in long form.  */)
  (Lisp_Object row, Lisp_Object glyphs)
{
  struct frame *sf = SELECTED_FRAME ();
  struct glyph_matrix *m = XWINDOW (sf->tool_bar_window)->current_matrix;
  int vpos;

  CHECK_NUMBER (row);
  vpos = XINT (row);
  if (vpos >= 0 && vpos < m->nrows)
    dump_glyph_row (MATRIX_ROW (m, vpos), vpos,
		    INTEGERP (glyphs) ? XINT (glyphs) : 2);
  return Qnil;
}


DEFUN ("trace-redisplay", Ftrace_redisplay, Strace_redisplay, 0, 1, "P",
       doc: /* Toggle tracing of redisplay.
With ARG, turn tracing on if and only if ARG is positive.  */)
  (Lisp_Object arg)
{
  if (NILP (arg))
    trace_redisplay_p = !trace_redisplay_p;
  else
    {
      arg = Fprefix_numeric_value (arg);
      trace_redisplay_p = XINT (arg) > 0;
    }

  return Qnil;
}


DEFUN ("trace-to-stderr", Ftrace_to_stderr, Strace_to_stderr, 1, MANY, "",
       doc: /* Like `format', but print result to stderr.
usage: (trace-to-stderr STRING &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object s = Fformat (nargs, args);
  fprintf (stderr, "%s", SDATA (s));
  return Qnil;
}

#endif /* GLYPH_DEBUG */



/***********************************************************************
		     Building Desired Matrix Rows
 ***********************************************************************/

/* Return a temporary glyph row holding the glyphs of an overlay arrow.
   Used for non-window-redisplay windows, and for windows w/o left fringe.  */

static struct glyph_row *
get_overlay_arrow_glyph_row (struct window *w, Lisp_Object overlay_arrow_string)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct buffer *buffer = XBUFFER (w->buffer);
  struct buffer *old = current_buffer;
  const unsigned char *arrow_string = SDATA (overlay_arrow_string);
  int arrow_len = SCHARS (overlay_arrow_string);
  const unsigned char *arrow_end = arrow_string + arrow_len;
  const unsigned char *p;
  struct it it;
  int multibyte_p;
  int n_glyphs_before;

  set_buffer_temp (buffer);
  init_iterator (&it, w, -1, -1, &scratch_glyph_row, DEFAULT_FACE_ID);
  it.glyph_row->used[TEXT_AREA] = 0;
  SET_TEXT_POS (it.position, 0, 0);

  multibyte_p = !NILP (BVAR (buffer, enable_multibyte_characters));
  p = arrow_string;
  while (p < arrow_end)
    {
      Lisp_Object face, ilisp;

      /* Get the next character.  */
      if (multibyte_p)
	it.c = it.char_to_display = string_char_and_length (p, &it.len);
      else
	{
	  it.c = it.char_to_display = *p, it.len = 1;
	  if (! ASCII_CHAR_P (it.c))
	    it.char_to_display = BYTE8_TO_CHAR (it.c);
	}
      p += it.len;

      /* Get its face.  */
      ilisp = make_number (p - arrow_string);
      face = Fget_text_property (ilisp, Qface, overlay_arrow_string);
      it.face_id = compute_char_face (f, it.char_to_display, face);

      /* Compute its width, get its glyphs.  */
      n_glyphs_before = it.glyph_row->used[TEXT_AREA];
      SET_TEXT_POS (it.position, -1, -1);
      PRODUCE_GLYPHS (&it);

      /* If this character doesn't fit any more in the line, we have
	 to remove some glyphs.  */
      if (it.current_x > it.last_visible_x)
	{
	  it.glyph_row->used[TEXT_AREA] = n_glyphs_before;
	  break;
	}
    }

  set_buffer_temp (old);
  return it.glyph_row;
}


/* Insert truncation glyphs at the start of IT->glyph_row.  Truncation
   glyphs are only inserted for terminal frames since we can't really
   win with truncation glyphs when partially visible glyphs are
   involved.  Which glyphs to insert is determined by
   produce_special_glyphs.  */

static void
insert_left_trunc_glyphs (struct it *it)
{
  struct it truncate_it;
  struct glyph *from, *end, *to, *toend;

  xassert (!FRAME_WINDOW_P (it->f));

  /* Get the truncation glyphs.  */
  truncate_it = *it;
  truncate_it.current_x = 0;
  truncate_it.face_id = DEFAULT_FACE_ID;
  truncate_it.glyph_row = &scratch_glyph_row;
  truncate_it.glyph_row->used[TEXT_AREA] = 0;
  CHARPOS (truncate_it.position) = BYTEPOS (truncate_it.position) = -1;
  truncate_it.object = make_number (0);
  produce_special_glyphs (&truncate_it, IT_TRUNCATION);

  /* Overwrite glyphs from IT with truncation glyphs.  */
  if (!it->glyph_row->reversed_p)
    {
      from = truncate_it.glyph_row->glyphs[TEXT_AREA];
      end = from + truncate_it.glyph_row->used[TEXT_AREA];
      to = it->glyph_row->glyphs[TEXT_AREA];
      toend = to + it->glyph_row->used[TEXT_AREA];

      while (from < end)
	*to++ = *from++;

      /* There may be padding glyphs left over.  Overwrite them too.  */
      while (to < toend && CHAR_GLYPH_PADDING_P (*to))
	{
	  from = truncate_it.glyph_row->glyphs[TEXT_AREA];
	  while (from < end)
	    *to++ = *from++;
	}

      if (to > toend)
	it->glyph_row->used[TEXT_AREA] = to - it->glyph_row->glyphs[TEXT_AREA];
    }
  else
    {
      /* In R2L rows, overwrite the last (rightmost) glyphs, and do
	 that back to front.  */
      end = truncate_it.glyph_row->glyphs[TEXT_AREA];
      from = end + truncate_it.glyph_row->used[TEXT_AREA] - 1;
      toend = it->glyph_row->glyphs[TEXT_AREA];
      to = toend + it->glyph_row->used[TEXT_AREA] - 1;

      while (from >= end && to >= toend)
	*to-- = *from--;
      while (to >= toend && CHAR_GLYPH_PADDING_P (*to))
	{
	  from =
	    truncate_it.glyph_row->glyphs[TEXT_AREA]
	    + truncate_it.glyph_row->used[TEXT_AREA] - 1;
	  while (from >= end && to >= toend)
	    *to-- = *from--;
	}
      if (from >= end)
	{
	  /* Need to free some room before prepending additional
	     glyphs.  */
	  int move_by = from - end + 1;
	  struct glyph *g0 = it->glyph_row->glyphs[TEXT_AREA];
	  struct glyph *g = g0 + it->glyph_row->used[TEXT_AREA] - 1;

	  for ( ; g >= g0; g--)
	    g[move_by] = *g;
	  while (from >= end)
	    *to-- = *from--;
	  it->glyph_row->used[TEXT_AREA] += move_by;
	}
    }
}

/* Compute the hash code for ROW.  */
unsigned
row_hash (struct glyph_row *row)
{
  int area, k;
  unsigned hashval = 0;

  for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
    for (k = 0; k < row->used[area]; ++k)
      hashval = ((((hashval << 4) + (hashval >> 24)) & 0x0fffffff)
		  + row->glyphs[area][k].u.val
		  + row->glyphs[area][k].face_id
		  + row->glyphs[area][k].padding_p
		  + (row->glyphs[area][k].type << 2));

  return hashval;
}

/* Compute the pixel height and width of IT->glyph_row.

   Most of the time, ascent and height of a display line will be equal
   to the max_ascent and max_height values of the display iterator
   structure.  This is not the case if

   1. We hit ZV without displaying anything.  In this case, max_ascent
   and max_height will be zero.

   2. We have some glyphs that don't contribute to the line height.
   (The glyph row flag contributes_to_line_height_p is for future
   pixmap extensions).

   The first case is easily covered by using default values because in
   these cases, the line height does not really matter, except that it
   must not be zero.  */

static void
compute_line_metrics (struct it *it)
{
  struct glyph_row *row = it->glyph_row;

  if (FRAME_WINDOW_P (it->f))
    {
      int i, min_y, max_y;

      /* The line may consist of one space only, that was added to
	 place the cursor on it.  If so, the row's height hasn't been
	 computed yet.  */
      if (row->height == 0)
	{
	  if (it->max_ascent + it->max_descent == 0)
	    it->max_descent = it->max_phys_descent = FRAME_LINE_HEIGHT (it->f);
	  row->ascent = it->max_ascent;
	  row->height = it->max_ascent + it->max_descent;
	  row->phys_ascent = it->max_phys_ascent;
	  row->phys_height = it->max_phys_ascent + it->max_phys_descent;
	  row->extra_line_spacing = it->max_extra_line_spacing;
	}

      /* Compute the width of this line.  */
      row->pixel_width = row->x;
      for (i = 0; i < row->used[TEXT_AREA]; ++i)
	row->pixel_width += row->glyphs[TEXT_AREA][i].pixel_width;

      xassert (row->pixel_width >= 0);
      xassert (row->ascent >= 0 && row->height > 0);

      row->overlapping_p = (MATRIX_ROW_OVERLAPS_SUCC_P (row)
			    || MATRIX_ROW_OVERLAPS_PRED_P (row));

      /* If first line's physical ascent is larger than its logical
         ascent, use the physical ascent, and make the row taller.
         This makes accented characters fully visible.  */
      if (row == MATRIX_FIRST_TEXT_ROW (it->w->desired_matrix)
	  && row->phys_ascent > row->ascent)
	{
	  row->height += row->phys_ascent - row->ascent;
	  row->ascent = row->phys_ascent;
	}

      /* Compute how much of the line is visible.  */
      row->visible_height = row->height;

      min_y = WINDOW_HEADER_LINE_HEIGHT (it->w);
      max_y = WINDOW_BOX_HEIGHT_NO_MODE_LINE (it->w);

      if (row->y < min_y)
	row->visible_height -= min_y - row->y;
      if (row->y + row->height > max_y)
	row->visible_height -= row->y + row->height - max_y;
    }
  else
    {
      row->pixel_width = row->used[TEXT_AREA];
      if (row->continued_p)
	row->pixel_width -= it->continuation_pixel_width;
      else if (row->truncated_on_right_p)
	row->pixel_width -= it->truncation_pixel_width;
      row->ascent = row->phys_ascent = 0;
      row->height = row->phys_height = row->visible_height = 1;
      row->extra_line_spacing = 0;
    }

  /* Compute a hash code for this row.  */
  row->hash = row_hash (row);

  it->max_ascent = it->max_descent = 0;
  it->max_phys_ascent = it->max_phys_descent = 0;
}


/* Append one space to the glyph row of iterator IT if doing a
   window-based redisplay.  The space has the same face as
   IT->face_id.  Value is non-zero if a space was added.

   This function is called to make sure that there is always one glyph
   at the end of a glyph row that the cursor can be set on under
   window-systems.  (If there weren't such a glyph we would not know
   how wide and tall a box cursor should be displayed).

   At the same time this space let's a nicely handle clearing to the
   end of the line if the row ends in italic text.  */

static int
append_space_for_newline (struct it *it, int default_face_p)
{
  if (FRAME_WINDOW_P (it->f))
    {
      int n = it->glyph_row->used[TEXT_AREA];

      if (it->glyph_row->glyphs[TEXT_AREA] + n
	  < it->glyph_row->glyphs[1 + TEXT_AREA])
	{
	  /* Save some values that must not be changed.
	     Must save IT->c and IT->len because otherwise
	     ITERATOR_AT_END_P wouldn't work anymore after
	     append_space_for_newline has been called.  */
	  enum display_element_type saved_what = it->what;
	  int saved_c = it->c, saved_len = it->len;
	  int saved_char_to_display = it->char_to_display;
	  int saved_x = it->current_x;
	  int saved_face_id = it->face_id;
	  struct text_pos saved_pos;
	  Lisp_Object saved_object;
	  struct face *face;

	  saved_object = it->object;
	  saved_pos = it->position;

	  it->what = IT_CHARACTER;
	  memset (&it->position, 0, sizeof it->position);
	  it->object = make_number (0);
	  it->c = it->char_to_display = ' ';
	  it->len = 1;

	  /* If the default face was remapped, be sure to use the
	     remapped face for the appended newline. */
	  if (default_face_p)
	    it->face_id = lookup_basic_face (it->f, DEFAULT_FACE_ID);
	  else if (it->face_before_selective_p)
	    it->face_id = it->saved_face_id;
	  face = FACE_FROM_ID (it->f, it->face_id);
	  it->face_id = FACE_FOR_CHAR (it->f, face, 0, -1, Qnil);

	  PRODUCE_GLYPHS (it);

	  it->override_ascent = -1;
	  it->constrain_row_ascent_descent_p = 0;
	  it->current_x = saved_x;
	  it->object = saved_object;
	  it->position = saved_pos;
	  it->what = saved_what;
	  it->face_id = saved_face_id;
	  it->len = saved_len;
	  it->c = saved_c;
	  it->char_to_display = saved_char_to_display;
	  return 1;
	}
    }

  return 0;
}


/* Extend the face of the last glyph in the text area of IT->glyph_row
   to the end of the display line.  Called from display_line.  If the
   glyph row is empty, add a space glyph to it so that we know the
   face to draw.  Set the glyph row flag fill_line_p.  If the glyph
   row is R2L, prepend a stretch glyph to cover the empty space to the
   left of the leftmost glyph.  */

static void
extend_face_to_end_of_line (struct it *it)
{
  struct face *face, *default_face;
  struct frame *f = it->f;

  /* If line is already filled, do nothing.  Non window-system frames
     get a grace of one more ``pixel'' because their characters are
     1-``pixel'' wide, so they hit the equality too early.  This grace
     is needed only for R2L rows that are not continued, to produce
     one extra blank where we could display the cursor.  */
  if (it->current_x >= it->last_visible_x
      + (!FRAME_WINDOW_P (f)
	 && it->glyph_row->reversed_p
	 && !it->glyph_row->continued_p))
    return;

  /* The default face, possibly remapped. */
  default_face = FACE_FROM_ID (f, lookup_basic_face (f, DEFAULT_FACE_ID));

  /* Face extension extends the background and box of IT->face_id
     to the end of the line.  If the background equals the background
     of the frame, we don't have to do anything.  */
  if (it->face_before_selective_p)
    face = FACE_FROM_ID (f, it->saved_face_id);
  else
    face = FACE_FROM_ID (f, it->face_id);

  if (FRAME_WINDOW_P (f)
      && it->glyph_row->displays_text_p
      && face->box == FACE_NO_BOX
      && face->background == FRAME_BACKGROUND_PIXEL (f)
      && !face->stipple
      && !it->glyph_row->reversed_p)
    return;

  /* Set the glyph row flag indicating that the face of the last glyph
     in the text area has to be drawn to the end of the text area.  */
  it->glyph_row->fill_line_p = 1;

  /* If current character of IT is not ASCII, make sure we have the
     ASCII face.  This will be automatically undone the next time
     get_next_display_element returns a multibyte character.  Note
     that the character will always be single byte in unibyte
     text.  */
  if (!ASCII_CHAR_P (it->c))
    {
      it->face_id = FACE_FOR_CHAR (f, face, 0, -1, Qnil);
    }

  if (FRAME_WINDOW_P (f))
    {
      /* If the row is empty, add a space with the current face of IT,
	 so that we know which face to draw.  */
      if (it->glyph_row->used[TEXT_AREA] == 0)
	{
	  it->glyph_row->glyphs[TEXT_AREA][0] = space_glyph;
	  it->glyph_row->glyphs[TEXT_AREA][0].face_id = face->id;
	  it->glyph_row->used[TEXT_AREA] = 1;
	}
#ifdef HAVE_WINDOW_SYSTEM
      if (it->glyph_row->reversed_p)
	{
	  /* Prepend a stretch glyph to the row, such that the
	     rightmost glyph will be drawn flushed all the way to the
	     right margin of the window.  The stretch glyph that will
	     occupy the empty space, if any, to the left of the
	     glyphs.  */
	  struct font *font = face->font ? face->font : FRAME_FONT (f);
	  struct glyph *row_start = it->glyph_row->glyphs[TEXT_AREA];
	  struct glyph *row_end = row_start + it->glyph_row->used[TEXT_AREA];
	  struct glyph *g;
	  int row_width, stretch_ascent, stretch_width;
	  struct text_pos saved_pos;
	  int saved_face_id, saved_avoid_cursor;

	  for (row_width = 0, g = row_start; g < row_end; g++)
	    row_width += g->pixel_width;
	  stretch_width = window_box_width (it->w, TEXT_AREA) - row_width;
	  if (stretch_width > 0)
	    {
	      stretch_ascent =
		(((it->ascent + it->descent)
		  * FONT_BASE (font)) / FONT_HEIGHT (font));
	      saved_pos = it->position;
	      memset (&it->position, 0, sizeof it->position);
	      saved_avoid_cursor = it->avoid_cursor_p;
	      it->avoid_cursor_p = 1;
	      saved_face_id = it->face_id;
	      /* The last row's stretch glyph should get the default
		 face, to avoid painting the rest of the window with
		 the region face, if the region ends at ZV.  */
	      if (it->glyph_row->ends_at_zv_p)
		it->face_id = default_face->id;
	      else
		it->face_id = face->id;
	      append_stretch_glyph (it, make_number (0), stretch_width,
				    it->ascent + it->descent, stretch_ascent);
	      it->position = saved_pos;
	      it->avoid_cursor_p = saved_avoid_cursor;
	      it->face_id = saved_face_id;
	    }
	}
#endif	/* HAVE_WINDOW_SYSTEM */
    }
  else
    {
      /* Save some values that must not be changed.  */
      int saved_x = it->current_x;
      struct text_pos saved_pos;
      Lisp_Object saved_object;
      enum display_element_type saved_what = it->what;
      int saved_face_id = it->face_id;

      saved_object = it->object;
      saved_pos = it->position;

      it->what = IT_CHARACTER;
      memset (&it->position, 0, sizeof it->position);
      it->object = make_number (0);
      it->c = it->char_to_display = ' ';
      it->len = 1;
      /* The last row's blank glyphs should get the default face, to
	 avoid painting the rest of the window with the region face,
	 if the region ends at ZV.  */
      if (it->glyph_row->ends_at_zv_p)
	it->face_id = default_face->id;
      else
	it->face_id = face->id;

      PRODUCE_GLYPHS (it);

      while (it->current_x <= it->last_visible_x)
	PRODUCE_GLYPHS (it);

      /* Don't count these blanks really.  It would let us insert a left
	 truncation glyph below and make us set the cursor on them, maybe.  */
      it->current_x = saved_x;
      it->object = saved_object;
      it->position = saved_pos;
      it->what = saved_what;
      it->face_id = saved_face_id;
    }
}


/* Value is non-zero if text starting at CHARPOS in current_buffer is
   trailing whitespace.  */

static int
trailing_whitespace_p (EMACS_INT charpos)
{
  EMACS_INT bytepos = CHAR_TO_BYTE (charpos);
  int c = 0;

  while (bytepos < ZV_BYTE
	 && (c = FETCH_CHAR (bytepos),
	     c == ' ' || c == '\t'))
    ++bytepos;

  if (bytepos >= ZV_BYTE || c == '\n' || c == '\r')
    {
      if (bytepos != PT_BYTE)
	return 1;
    }
  return 0;
}


/* Highlight trailing whitespace, if any, in ROW.  */

static void
highlight_trailing_whitespace (struct frame *f, struct glyph_row *row)
{
  int used = row->used[TEXT_AREA];

  if (used)
    {
      struct glyph *start = row->glyphs[TEXT_AREA];
      struct glyph *glyph = start + used - 1;

      if (row->reversed_p)
	{
	  /* Right-to-left rows need to be processed in the opposite
	     direction, so swap the edge pointers. */
	  glyph = start;
	  start = row->glyphs[TEXT_AREA] + used - 1;
	}

      /* Skip over glyphs inserted to display the cursor at the
	 end of a line, for extending the face of the last glyph
	 to the end of the line on terminals, and for truncation
	 and continuation glyphs.  */
      if (!row->reversed_p)
	{
	  while (glyph >= start
		 && glyph->type == CHAR_GLYPH
		 && INTEGERP (glyph->object))
	    --glyph;
	}
      else
	{
	  while (glyph <= start
		 && glyph->type == CHAR_GLYPH
		 && INTEGERP (glyph->object))
	    ++glyph;
	}

      /* If last glyph is a space or stretch, and it's trailing
	 whitespace, set the face of all trailing whitespace glyphs in
	 IT->glyph_row to `trailing-whitespace'.  */
      if ((row->reversed_p ? glyph <= start : glyph >= start)
	  && BUFFERP (glyph->object)
	  && (glyph->type == STRETCH_GLYPH
	      || (glyph->type == CHAR_GLYPH
		  && glyph->u.ch == ' '))
	  && trailing_whitespace_p (glyph->charpos))
	{
	  int face_id = lookup_named_face (f, Qtrailing_whitespace, 0);
	  if (face_id < 0)
	    return;

	  if (!row->reversed_p)
	    {
	      while (glyph >= start
		     && BUFFERP (glyph->object)
		     && (glyph->type == STRETCH_GLYPH
			 || (glyph->type == CHAR_GLYPH
			     && glyph->u.ch == ' ')))
		(glyph--)->face_id = face_id;
	    }
	  else
	    {
	      while (glyph <= start
		     && BUFFERP (glyph->object)
		     && (glyph->type == STRETCH_GLYPH
			 || (glyph->type == CHAR_GLYPH
			     && glyph->u.ch == ' ')))
		(glyph++)->face_id = face_id;
	    }
	}
    }
}


/* Value is non-zero if glyph row ROW should be
   used to hold the cursor.  */

static int
cursor_row_p (struct glyph_row *row)
{
  int result = 1;

  if (PT == CHARPOS (row->end.pos)
      || PT == MATRIX_ROW_END_CHARPOS (row))
    {
      /* Suppose the row ends on a string.
	 Unless the row is continued, that means it ends on a newline
	 in the string.  If it's anything other than a display string
	 (e.g., a before-string from an overlay), we don't want the
	 cursor there.  (This heuristic seems to give the optimal
	 behavior for the various types of multi-line strings.)
	 One exception: if the string has `cursor' property on one of
	 its characters, we _do_ want the cursor there.  */
      if (CHARPOS (row->end.string_pos) >= 0)
	{
	  if (row->continued_p)
	    result = 1;
	  else
	    {
	      /* Check for `display' property.  */
	      struct glyph *beg = row->glyphs[TEXT_AREA];
	      struct glyph *end = beg + row->used[TEXT_AREA] - 1;
	      struct glyph *glyph;

	      result = 0;
	      for (glyph = end; glyph >= beg; --glyph)
		if (STRINGP (glyph->object))
		  {
		    Lisp_Object prop
		      = Fget_char_property (make_number (PT),
					    Qdisplay, Qnil);
		    result =
		      (!NILP (prop)
		       && display_prop_string_p (prop, glyph->object));
		    /* If there's a `cursor' property on one of the
		       string's characters, this row is a cursor row,
		       even though this is not a display string.  */
		    if (!result)
		      {
			Lisp_Object s = glyph->object;

			for ( ; glyph >= beg && EQ (glyph->object, s); --glyph)
			  {
			    EMACS_INT gpos = glyph->charpos;

			    if (!NILP (Fget_char_property (make_number (gpos),
							   Qcursor, s)))
			      {
				result = 1;
				break;
			      }
			  }
		      }
		    break;
		  }
	    }
	}
      else if (MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (row))
	{
	  /* If the row ends in middle of a real character,
	     and the line is continued, we want the cursor here.
	     That's because CHARPOS (ROW->end.pos) would equal
	     PT if PT is before the character.  */
	  if (!row->ends_in_ellipsis_p)
	    result = row->continued_p;
	  else
	  /* If the row ends in an ellipsis, then
	     CHARPOS (ROW->end.pos) will equal point after the
	     invisible text.  We want that position to be displayed
	     after the ellipsis.  */
	    result = 0;
	}
      /* If the row ends at ZV, display the cursor at the end of that
	 row instead of at the start of the row below.  */
      else if (row->ends_at_zv_p)
	result = 1;
      else
	result = 0;
    }

  return result;
}



/* Push the property PROP so that it will be rendered at the current
   position in IT.  Return 1 if PROP was successfully pushed, 0
   otherwise.  Called from handle_line_prefix to handle the
   `line-prefix' and `wrap-prefix' properties.  */

static int
push_prefix_prop (struct it *it, Lisp_Object prop)
{
  struct text_pos pos =
    STRINGP (it->string) ? it->current.string_pos : it->current.pos;

  xassert (it->method == GET_FROM_BUFFER
	   || it->method == GET_FROM_DISPLAY_VECTOR
	   || it->method == GET_FROM_STRING);

  /* We need to save the current buffer/string position, so it will be
     restored by pop_it, because iterate_out_of_display_property
     depends on that being set correctly, but some situations leave
     it->position not yet set when this function is called.  */
  push_it (it, &pos);

  if (STRINGP (prop))
    {
      if (SCHARS (prop) == 0)
	{
	  pop_it (it);
	  return 0;
	}

      it->string = prop;
      it->string_from_prefix_prop_p = 1;
      it->multibyte_p = STRING_MULTIBYTE (it->string);
      it->current.overlay_string_index = -1;
      IT_STRING_CHARPOS (*it) = IT_STRING_BYTEPOS (*it) = 0;
      it->end_charpos = it->string_nchars = SCHARS (it->string);
      it->method = GET_FROM_STRING;
      it->stop_charpos = 0;
      it->prev_stop = 0;
      it->base_level_stop = 0;

      /* Force paragraph direction to be that of the parent
	 buffer/string.  */
      if (it->bidi_p && it->bidi_it.paragraph_dir == R2L)
	it->paragraph_embedding = it->bidi_it.paragraph_dir;
      else
	it->paragraph_embedding = L2R;

      /* Set up the bidi iterator for this display string.  */
      if (it->bidi_p)
	{
	  it->bidi_it.string.lstring = it->string;
	  it->bidi_it.string.s = NULL;
	  it->bidi_it.string.schars = it->end_charpos;
	  it->bidi_it.string.bufpos = IT_CHARPOS (*it);
	  it->bidi_it.string.from_disp_str = it->string_from_display_prop_p;
	  it->bidi_it.string.unibyte = !it->multibyte_p;
	  bidi_init_it (0, 0, FRAME_WINDOW_P (it->f), &it->bidi_it);
	}
    }
  else if (CONSP (prop) && EQ (XCAR (prop), Qspace))
    {
      it->method = GET_FROM_STRETCH;
      it->object = prop;
    }
#ifdef HAVE_WINDOW_SYSTEM
  else if (IMAGEP (prop))
    {
      it->what = IT_IMAGE;
      it->image_id = lookup_image (it->f, prop);
      it->method = GET_FROM_IMAGE;
    }
#endif /* HAVE_WINDOW_SYSTEM */
  else
    {
      pop_it (it);		/* bogus display property, give up */
      return 0;
    }

  return 1;
}

/* Return the character-property PROP at the current position in IT.  */

static Lisp_Object
get_it_property (struct it *it, Lisp_Object prop)
{
  Lisp_Object position;

  if (STRINGP (it->object))
    position = make_number (IT_STRING_CHARPOS (*it));
  else if (BUFFERP (it->object))
    position = make_number (IT_CHARPOS (*it));
  else
    return Qnil;

  return Fget_char_property (position, prop, it->object);
}

/* See if there's a line- or wrap-prefix, and if so, push it on IT.  */

static void
handle_line_prefix (struct it *it)
{
  Lisp_Object prefix;

  if (it->continuation_lines_width > 0)
    {
      prefix = get_it_property (it, Qwrap_prefix);
      if (NILP (prefix))
	prefix = Vwrap_prefix;
    }
  else
    {
      prefix = get_it_property (it, Qline_prefix);
      if (NILP (prefix))
	prefix = Vline_prefix;
    }
  if (! NILP (prefix) && push_prefix_prop (it, prefix))
    {
      /* If the prefix is wider than the window, and we try to wrap
	 it, it would acquire its own wrap prefix, and so on till the
	 iterator stack overflows.  So, don't wrap the prefix.  */
      it->line_wrap = TRUNCATE;
      it->avoid_cursor_p = 1;
    }
}



/* Remove N glyphs at the start of a reversed IT->glyph_row.  Called
   only for R2L lines from display_line and display_string, when they
   decide that too many glyphs were produced by PRODUCE_GLYPHS, and
   the line/string needs to be continued on the next glyph row.  */
static void
unproduce_glyphs (struct it *it, int n)
{
  struct glyph *glyph, *end;

  xassert (it->glyph_row);
  xassert (it->glyph_row->reversed_p);
  xassert (it->area == TEXT_AREA);
  xassert (n <= it->glyph_row->used[TEXT_AREA]);

  if (n > it->glyph_row->used[TEXT_AREA])
    n = it->glyph_row->used[TEXT_AREA];
  glyph = it->glyph_row->glyphs[TEXT_AREA] + n;
  end = it->glyph_row->glyphs[TEXT_AREA] + it->glyph_row->used[TEXT_AREA];
  for ( ; glyph < end; glyph++)
    glyph[-n] = *glyph;
}

/* Find the positions in a bidi-reordered ROW to serve as ROW->minpos
   and ROW->maxpos.  */
static void
find_row_edges (struct it *it, struct glyph_row *row,
		EMACS_INT min_pos, EMACS_INT min_bpos,
		EMACS_INT max_pos, EMACS_INT max_bpos)
{
  /* FIXME: Revisit this when glyph ``spilling'' in continuation
     lines' rows is implemented for bidi-reordered rows.  */

  /* ROW->minpos is the value of min_pos, the minimal buffer position
     we have in ROW, or ROW->start.pos if that is smaller.  */
  if (min_pos <= ZV && min_pos < row->start.pos.charpos)
    SET_TEXT_POS (row->minpos, min_pos, min_bpos);
  else
    /* We didn't find buffer positions smaller than ROW->start, or
       didn't find _any_ valid buffer positions in any of the glyphs,
       so we must trust the iterator's computed positions.  */
      row->minpos = row->start.pos;
  if (max_pos <= 0)
    {
      max_pos = CHARPOS (it->current.pos);
      max_bpos = BYTEPOS (it->current.pos);
    }

  /* Here are the various use-cases for ending the row, and the
     corresponding values for ROW->maxpos:

     Line ends in a newline from buffer       eol_pos + 1
     Line is continued from buffer            max_pos + 1
     Line is truncated on right               it->current.pos
     Line ends in a newline from string       max_pos + 1(*)
      (*) + 1 only when line ends in a forward scan
     Line is continued from string            max_pos
     Line is continued from display vector    max_pos
     Line is entirely from a string           min_pos == max_pos
     Line is entirely from a display vector   min_pos == max_pos
     Line that ends at ZV                     ZV

     If you discover other use-cases, please add them here as
     appropriate.  */
  if (row->ends_at_zv_p)
    row->maxpos = it->current.pos;
  else if (row->used[TEXT_AREA])
    {
      int seen_this_string = 0;
      struct glyph_row *r1 = row - 1;

      /* Did we see the same display string on the previous row?  */
      if (STRINGP (it->object)
	  /* this is not the first row */
	  && row > it->w->desired_matrix->rows
	  /* previous row is not the header line */
	  && !r1->mode_line_p
	  /* previous row also ends in a newline from a string */
	  && r1->ends_in_newline_from_string_p)
	{
	  struct glyph *start, *end;

	  /* Search for the last glyph of the previous row that came
	     from buffer or string.  Depending on whether the row is
	     L2R or R2L, we need to process it front to back or the
	     other way round.  */
	  if (!r1->reversed_p)
	    {
	      start = r1->glyphs[TEXT_AREA];
	      end = start + r1->used[TEXT_AREA];
	      /* Glyphs inserted by redisplay have an integer (zero)
		 as their object.  */
	      while (end > start
		     && INTEGERP ((end - 1)->object)
		     && (end - 1)->charpos <= 0)
		--end;
	      if (end > start)
		{
		  if (EQ ((end - 1)->object, it->object))
		    seen_this_string = 1;
		}
	      else
		/* If all the glyphs of the previous row were inserted
		   by redisplay, it means the previous row was
		   produced from a single newline, which is only
		   possible if that newline came from the same string
		   as the one which produced this ROW.  */
		seen_this_string = 1;
	    }
	  else
	    {
	      end = r1->glyphs[TEXT_AREA] - 1;
	      start = end + r1->used[TEXT_AREA];
	      while (end < start
		     && INTEGERP ((end + 1)->object)
		     && (end + 1)->charpos <= 0)
		++end;
	      if (end < start)
		{
		  if (EQ ((end + 1)->object, it->object))
		    seen_this_string = 1;
		}
	      else
		seen_this_string = 1;
	    }
	}
      /* Take note of each display string that covers a newline only
	 once, the first time we see it.  This is for when a display
	 string includes more than one newline in it.  */
      if (row->ends_in_newline_from_string_p && !seen_this_string)
	{
	  /* If we were scanning the buffer forward when we displayed
	     the string, we want to account for at least one buffer
	     position that belongs to this row (position covered by
	     the display string), so that cursor positioning will
	     consider this row as a candidate when point is at the end
	     of the visual line represented by this row.  This is not
	     required when scanning back, because max_pos will already
	     have a much larger value.  */
	  if (CHARPOS (row->end.pos) > max_pos)
	    INC_BOTH (max_pos, max_bpos);
	  SET_TEXT_POS (row->maxpos, max_pos, max_bpos);
	}
      else if (CHARPOS (it->eol_pos) > 0)
	SET_TEXT_POS (row->maxpos,
		      CHARPOS (it->eol_pos) + 1, BYTEPOS (it->eol_pos) + 1);
      else if (row->continued_p)
	{
	  /* If max_pos is different from IT's current position, it
	     means IT->method does not belong to the display element
	     at max_pos.  However, it also means that the display
	     element at max_pos was displayed in its entirety on this
	     line, which is equivalent to saying that the next line
	     starts at the next buffer position.  */
	  if (IT_CHARPOS (*it) == max_pos && it->method != GET_FROM_BUFFER)
	    SET_TEXT_POS (row->maxpos, max_pos, max_bpos);
	  else
	    {
	      INC_BOTH (max_pos, max_bpos);
	      SET_TEXT_POS (row->maxpos, max_pos, max_bpos);
	    }
	}
      else if (row->truncated_on_right_p)
	/* display_line already called reseat_at_next_visible_line_start,
	   which puts the iterator at the beginning of the next line, in
	   the logical order. */
	row->maxpos = it->current.pos;
      else if (max_pos == min_pos && it->method != GET_FROM_BUFFER)
	/* A line that is entirely from a string/image/stretch...  */
	row->maxpos = row->minpos;
      else
	abort ();
    }
  else
    row->maxpos = it->current.pos;
}

/* Construct the glyph row IT->glyph_row in the desired matrix of
   IT->w from text at the current position of IT.  See dispextern.h
   for an overview of struct it.  Value is non-zero if
   IT->glyph_row displays text, as opposed to a line displaying ZV
   only.  */

static int
display_line (struct it *it)
{
  struct glyph_row *row = it->glyph_row;
  Lisp_Object overlay_arrow_string;
  struct it wrap_it;
  void *wrap_data = NULL;
  int may_wrap = 0, wrap_x IF_LINT (= 0);
  int wrap_row_used = -1;
  int wrap_row_ascent IF_LINT (= 0), wrap_row_height IF_LINT (= 0);
  int wrap_row_phys_ascent IF_LINT (= 0), wrap_row_phys_height IF_LINT (= 0);
  int wrap_row_extra_line_spacing IF_LINT (= 0);
  EMACS_INT wrap_row_min_pos IF_LINT (= 0), wrap_row_min_bpos IF_LINT (= 0);
  EMACS_INT wrap_row_max_pos IF_LINT (= 0), wrap_row_max_bpos IF_LINT (= 0);
  int cvpos;
  EMACS_INT min_pos = ZV + 1, max_pos = 0;
  EMACS_INT min_bpos IF_LINT (= 0), max_bpos IF_LINT (= 0);

  /* We always start displaying at hpos zero even if hscrolled.  */
  xassert (it->hpos == 0 && it->current_x == 0);

  if (MATRIX_ROW_VPOS (row, it->w->desired_matrix)
      >= it->w->desired_matrix->nrows)
    {
      it->w->nrows_scale_factor++;
      fonts_changed_p = 1;
      return 0;
    }

  /* Is IT->w showing the region?  */
  it->w->region_showing = it->region_beg_charpos > 0 ? Qt : Qnil;

  /* Clear the result glyph row and enable it.  */
  prepare_desired_row (row);

  row->y = it->current_y;
  row->start = it->start;
  row->continuation_lines_width = it->continuation_lines_width;
  row->displays_text_p = 1;
  row->starts_in_middle_of_char_p = it->starts_in_middle_of_char_p;
  it->starts_in_middle_of_char_p = 0;

  /* Arrange the overlays nicely for our purposes.  Usually, we call
     display_line on only one line at a time, in which case this
     can't really hurt too much, or we call it on lines which appear
     one after another in the buffer, in which case all calls to
     recenter_overlay_lists but the first will be pretty cheap.  */
  recenter_overlay_lists (current_buffer, IT_CHARPOS (*it));

  /* Move over display elements that are not visible because we are
     hscrolled.  This may stop at an x-position < IT->first_visible_x
     if the first glyph is partially visible or if we hit a line end.  */
  if (it->current_x < it->first_visible_x)
    {
      this_line_min_pos = row->start.pos;
      move_it_in_display_line_to (it, ZV, it->first_visible_x,
				  MOVE_TO_POS | MOVE_TO_X);
      /* Record the smallest positions seen while we moved over
	 display elements that are not visible.  This is needed by
	 redisplay_internal for optimizing the case where the cursor
	 stays inside the same line.  The rest of this function only
	 considers positions that are actually displayed, so
	 RECORD_MAX_MIN_POS will not otherwise record positions that
	 are hscrolled to the left of the left edge of the window.  */
      min_pos = CHARPOS (this_line_min_pos);
      min_bpos = BYTEPOS (this_line_min_pos);
    }
  else
    {
      /* We only do this when not calling `move_it_in_display_line_to'
	 above, because move_it_in_display_line_to calls
	 handle_line_prefix itself.  */
      handle_line_prefix (it);
    }

  /* Get the initial row height.  This is either the height of the
     text hscrolled, if there is any, or zero.  */
  row->ascent = it->max_ascent;
  row->height = it->max_ascent + it->max_descent;
  row->phys_ascent = it->max_phys_ascent;
  row->phys_height = it->max_phys_ascent + it->max_phys_descent;
  row->extra_line_spacing = it->max_extra_line_spacing;

/* Utility macro to record max and min buffer positions seen until now.  */
#define RECORD_MAX_MIN_POS(IT)					\
  do								\
    {								\
      int composition_p = !STRINGP ((IT)->string)		\
	&& ((IT)->what == IT_COMPOSITION);			\
      EMACS_INT current_pos =					\
	composition_p ? (IT)->cmp_it.charpos			\
		      : IT_CHARPOS (*(IT));			\
      EMACS_INT current_bpos =					\
	composition_p ? CHAR_TO_BYTE (current_pos)		\
		      : IT_BYTEPOS (*(IT));			\
      if (current_pos < min_pos)				\
	{							\
	  min_pos = current_pos;				\
	  min_bpos = current_bpos;				\
	}							\
      if (IT_CHARPOS (*it) > max_pos)				\
	{							\
	  max_pos = IT_CHARPOS (*it);				\
	  max_bpos = IT_BYTEPOS (*it);				\
	}							\
    }								\
  while (0)

  /* Loop generating characters.  The loop is left with IT on the next
     character to display.  */
  while (1)
    {
      int n_glyphs_before, hpos_before, x_before;
      int x, nglyphs;
      int ascent = 0, descent = 0, phys_ascent = 0, phys_descent = 0;

      /* Retrieve the next thing to display.  Value is zero if end of
	 buffer reached.  */
      if (!get_next_display_element (it))
	{
	  /* Maybe add a space at the end of this line that is used to
	     display the cursor there under X.  Set the charpos of the
	     first glyph of blank lines not corresponding to any text
	     to -1.  */
	  if (IT_OVERFLOW_NEWLINE_INTO_FRINGE (it))
	    row->exact_window_width_line_p = 1;
	  else if ((append_space_for_newline (it, 1) && row->used[TEXT_AREA] == 1)
		   || row->used[TEXT_AREA] == 0)
	    {
	      row->glyphs[TEXT_AREA]->charpos = -1;
	      row->displays_text_p = 0;

	      if (!NILP (BVAR (XBUFFER (it->w->buffer), indicate_empty_lines))
		  && (!MINI_WINDOW_P (it->w)
		      || (minibuf_level && EQ (it->window, minibuf_window))))
		row->indicate_empty_line_p = 1;
	    }

	  it->continuation_lines_width = 0;
	  row->ends_at_zv_p = 1;
	  /* A row that displays right-to-left text must always have
	     its last face extended all the way to the end of line,
	     even if this row ends in ZV, because we still write to
	     the screen left to right.  We also need to extend the
	     last face if the default face is remapped to some
	     different face, otherwise the functions that clear
	     portions of the screen will clear with the default face's
	     background color.  */
	  if (row->reversed_p
	      || lookup_basic_face (it->f, DEFAULT_FACE_ID) != DEFAULT_FACE_ID)
	    extend_face_to_end_of_line (it);
	  break;
	}

      /* Now, get the metrics of what we want to display.  This also
	 generates glyphs in `row' (which is IT->glyph_row).  */
      n_glyphs_before = row->used[TEXT_AREA];
      x = it->current_x;

      /* Remember the line height so far in case the next element doesn't
	 fit on the line.  */
      if (it->line_wrap != TRUNCATE)
	{
	  ascent = it->max_ascent;
	  descent = it->max_descent;
	  phys_ascent = it->max_phys_ascent;
	  phys_descent = it->max_phys_descent;

	  if (it->line_wrap == WORD_WRAP && it->area == TEXT_AREA)
	    {
	      if (IT_DISPLAYING_WHITESPACE (it))
		may_wrap = 1;
	      else if (may_wrap)
		{
		  SAVE_IT (wrap_it, *it, wrap_data);
		  wrap_x = x;
		  wrap_row_used = row->used[TEXT_AREA];
		  wrap_row_ascent = row->ascent;
		  wrap_row_height = row->height;
		  wrap_row_phys_ascent = row->phys_ascent;
		  wrap_row_phys_height = row->phys_height;
		  wrap_row_extra_line_spacing = row->extra_line_spacing;
		  wrap_row_min_pos = min_pos;
		  wrap_row_min_bpos = min_bpos;
		  wrap_row_max_pos = max_pos;
		  wrap_row_max_bpos = max_bpos;
		  may_wrap = 0;
		}
	    }
	}

      PRODUCE_GLYPHS (it);

      /* If this display element was in marginal areas, continue with
	 the next one.  */
      if (it->area != TEXT_AREA)
	{
	  row->ascent = max (row->ascent, it->max_ascent);
	  row->height = max (row->height, it->max_ascent + it->max_descent);
	  row->phys_ascent = max (row->phys_ascent, it->max_phys_ascent);
	  row->phys_height = max (row->phys_height,
				  it->max_phys_ascent + it->max_phys_descent);
	  row->extra_line_spacing = max (row->extra_line_spacing,
					 it->max_extra_line_spacing);
	  set_iterator_to_next (it, 1);
	  continue;
	}

      /* Does the display element fit on the line?  If we truncate
	 lines, we should draw past the right edge of the window.  If
	 we don't truncate, we want to stop so that we can display the
	 continuation glyph before the right margin.  If lines are
	 continued, there are two possible strategies for characters
	 resulting in more than 1 glyph (e.g. tabs): Display as many
	 glyphs as possible in this line and leave the rest for the
	 continuation line, or display the whole element in the next
	 line.  Original redisplay did the former, so we do it also.  */
      nglyphs = row->used[TEXT_AREA] - n_glyphs_before;
      hpos_before = it->hpos;
      x_before = x;

      if (/* Not a newline.  */
	  nglyphs > 0
	  /* Glyphs produced fit entirely in the line.  */
	  && it->current_x < it->last_visible_x)
	{
	  it->hpos += nglyphs;
	  row->ascent = max (row->ascent, it->max_ascent);
	  row->height = max (row->height, it->max_ascent + it->max_descent);
	  row->phys_ascent = max (row->phys_ascent, it->max_phys_ascent);
	  row->phys_height = max (row->phys_height,
				  it->max_phys_ascent + it->max_phys_descent);
	  row->extra_line_spacing = max (row->extra_line_spacing,
					 it->max_extra_line_spacing);
	  if (it->current_x - it->pixel_width < it->first_visible_x)
	    row->x = x - it->first_visible_x;
	  /* Record the maximum and minimum buffer positions seen so
	     far in glyphs that will be displayed by this row.  */
	  if (it->bidi_p)
	    RECORD_MAX_MIN_POS (it);
	}
      else
	{
	  int i, new_x;
	  struct glyph *glyph;

	  for (i = 0; i < nglyphs; ++i, x = new_x)
	    {
	      glyph = row->glyphs[TEXT_AREA] + n_glyphs_before + i;
	      new_x = x + glyph->pixel_width;

	      if (/* Lines are continued.  */
		  it->line_wrap != TRUNCATE
		  && (/* Glyph doesn't fit on the line.  */
		      new_x > it->last_visible_x
		      /* Or it fits exactly on a window system frame.  */
		      || (new_x == it->last_visible_x
			  && FRAME_WINDOW_P (it->f))))
		{
		  /* End of a continued line.  */

		  if (it->hpos == 0
		      || (new_x == it->last_visible_x
			  && FRAME_WINDOW_P (it->f)))
		    {
		      /* Current glyph is the only one on the line or
			 fits exactly on the line.  We must continue
			 the line because we can't draw the cursor
			 after the glyph.  */
		      row->continued_p = 1;
		      it->current_x = new_x;
		      it->continuation_lines_width += new_x;
		      ++it->hpos;
		      if (i == nglyphs - 1)
			{
			  /* If line-wrap is on, check if a previous
			     wrap point was found.  */
			  if (wrap_row_used > 0
			      /* Even if there is a previous wrap
				 point, continue the line here as
				 usual, if (i) the previous character
				 was a space or tab AND (ii) the
				 current character is not.  */
			      && (!may_wrap
				  || IT_DISPLAYING_WHITESPACE (it)))
			    goto back_to_wrap;

			  /* Record the maximum and minimum buffer
			     positions seen so far in glyphs that will be
			     displayed by this row.  */
			  if (it->bidi_p)
			    RECORD_MAX_MIN_POS (it);
			  set_iterator_to_next (it, 1);
			  if (IT_OVERFLOW_NEWLINE_INTO_FRINGE (it))
			    {
			      if (!get_next_display_element (it))
				{
				  row->exact_window_width_line_p = 1;
				  it->continuation_lines_width = 0;
				  row->continued_p = 0;
				  row->ends_at_zv_p = 1;
				}
			      else if (ITERATOR_AT_END_OF_LINE_P (it))
				{
				  row->continued_p = 0;
				  row->exact_window_width_line_p = 1;
				}
			    }
			}
		      else if (it->bidi_p)
			RECORD_MAX_MIN_POS (it);
		    }
		  else if (CHAR_GLYPH_PADDING_P (*glyph)
			   && !FRAME_WINDOW_P (it->f))
		    {
		      /* A padding glyph that doesn't fit on this line.
			 This means the whole character doesn't fit
			 on the line.  */
		      if (row->reversed_p)
			unproduce_glyphs (it, row->used[TEXT_AREA]
					       - n_glyphs_before);
		      row->used[TEXT_AREA] = n_glyphs_before;

		      /* Fill the rest of the row with continuation
			 glyphs like in 20.x.  */
		      while (row->glyphs[TEXT_AREA] + row->used[TEXT_AREA]
			     < row->glyphs[1 + TEXT_AREA])
			produce_special_glyphs (it, IT_CONTINUATION);

		      row->continued_p = 1;
		      it->current_x = x_before;
		      it->continuation_lines_width += x_before;

		      /* Restore the height to what it was before the
			 element not fitting on the line.  */
		      it->max_ascent = ascent;
		      it->max_descent = descent;
		      it->max_phys_ascent = phys_ascent;
		      it->max_phys_descent = phys_descent;
		    }
		  else if (wrap_row_used > 0)
		    {
		    back_to_wrap:
		      if (row->reversed_p)
			unproduce_glyphs (it,
					  row->used[TEXT_AREA] - wrap_row_used);
		      RESTORE_IT (it, &wrap_it, wrap_data);
		      it->continuation_lines_width += wrap_x;
		      row->used[TEXT_AREA] = wrap_row_used;
		      row->ascent = wrap_row_ascent;
		      row->height = wrap_row_height;
		      row->phys_ascent = wrap_row_phys_ascent;
		      row->phys_height = wrap_row_phys_height;
		      row->extra_line_spacing = wrap_row_extra_line_spacing;
		      min_pos = wrap_row_min_pos;
		      min_bpos = wrap_row_min_bpos;
		      max_pos = wrap_row_max_pos;
		      max_bpos = wrap_row_max_bpos;
		      row->continued_p = 1;
		      row->ends_at_zv_p = 0;
		      row->exact_window_width_line_p = 0;
		      it->continuation_lines_width += x;

		      /* Make sure that a non-default face is extended
			 up to the right margin of the window.  */
		      extend_face_to_end_of_line (it);
		    }
		  else if (it->c == '\t' && FRAME_WINDOW_P (it->f))
		    {
		      /* A TAB that extends past the right edge of the
			 window.  This produces a single glyph on
			 window system frames.  We leave the glyph in
			 this row and let it fill the row, but don't
			 consume the TAB.  */
		      it->continuation_lines_width += it->last_visible_x;
		      row->ends_in_middle_of_char_p = 1;
		      row->continued_p = 1;
		      glyph->pixel_width = it->last_visible_x - x;
		      it->starts_in_middle_of_char_p = 1;
		    }
		  else
		    {
		      /* Something other than a TAB that draws past
			 the right edge of the window.  Restore
			 positions to values before the element.  */
		      if (row->reversed_p)
			unproduce_glyphs (it, row->used[TEXT_AREA]
					       - (n_glyphs_before + i));
		      row->used[TEXT_AREA] = n_glyphs_before + i;

		      /* Display continuation glyphs.  */
		      if (!FRAME_WINDOW_P (it->f))
			produce_special_glyphs (it, IT_CONTINUATION);
		      row->continued_p = 1;

		      it->current_x = x_before;
		      it->continuation_lines_width += x;
		      extend_face_to_end_of_line (it);

		      if (nglyphs > 1 && i > 0)
			{
			  row->ends_in_middle_of_char_p = 1;
			  it->starts_in_middle_of_char_p = 1;
			}

		      /* Restore the height to what it was before the
			 element not fitting on the line.  */
		      it->max_ascent = ascent;
		      it->max_descent = descent;
		      it->max_phys_ascent = phys_ascent;
		      it->max_phys_descent = phys_descent;
		    }

		  break;
		}
	      else if (new_x > it->first_visible_x)
		{
		  /* Increment number of glyphs actually displayed.  */
		  ++it->hpos;

		  /* Record the maximum and minimum buffer positions
		     seen so far in glyphs that will be displayed by
		     this row.  */
		  if (it->bidi_p)
		    RECORD_MAX_MIN_POS (it);

		  if (x < it->first_visible_x)
		    /* Glyph is partially visible, i.e. row starts at
		       negative X position.  */
		    row->x = x - it->first_visible_x;
		}
	      else
		{
		  /* Glyph is completely off the left margin of the
		     window.  This should not happen because of the
		     move_it_in_display_line at the start of this
		     function, unless the text display area of the
		     window is empty.  */
		  xassert (it->first_visible_x <= it->last_visible_x);
		}
	    }
	  /* Even if this display element produced no glyphs at all,
	     we want to record its position.  */
	  if (it->bidi_p && nglyphs == 0)
	    RECORD_MAX_MIN_POS (it);

	  row->ascent = max (row->ascent, it->max_ascent);
	  row->height = max (row->height, it->max_ascent + it->max_descent);
	  row->phys_ascent = max (row->phys_ascent, it->max_phys_ascent);
	  row->phys_height = max (row->phys_height,
				  it->max_phys_ascent + it->max_phys_descent);
	  row->extra_line_spacing = max (row->extra_line_spacing,
					 it->max_extra_line_spacing);

	  /* End of this display line if row is continued.  */
	  if (row->continued_p || row->ends_at_zv_p)
	    break;
	}

    at_end_of_line:
      /* Is this a line end?  If yes, we're also done, after making
	 sure that a non-default face is extended up to the right
	 margin of the window.  */
      if (ITERATOR_AT_END_OF_LINE_P (it))
	{
	  int used_before = row->used[TEXT_AREA];

	  row->ends_in_newline_from_string_p = STRINGP (it->object);

	  /* Add a space at the end of the line that is used to
	     display the cursor there.  */
	  if (!IT_OVERFLOW_NEWLINE_INTO_FRINGE (it))
	    append_space_for_newline (it, 0);

	  /* Extend the face to the end of the line.  */
	  extend_face_to_end_of_line (it);

	  /* Make sure we have the position.  */
	  if (used_before == 0)
	    row->glyphs[TEXT_AREA]->charpos = CHARPOS (it->position);

	  /* Record the position of the newline, for use in
	     find_row_edges.  */
	  it->eol_pos = it->current.pos;

	  /* Consume the line end.  This skips over invisible lines.  */
	  set_iterator_to_next (it, 1);
	  it->continuation_lines_width = 0;
	  break;
	}

      /* Proceed with next display element.  Note that this skips
	 over lines invisible because of selective display.  */
      set_iterator_to_next (it, 1);

      /* If we truncate lines, we are done when the last displayed
	 glyphs reach past the right margin of the window.  */
      if (it->line_wrap == TRUNCATE
	  && (FRAME_WINDOW_P (it->f)
	      ? (it->current_x >= it->last_visible_x)
	      : (it->current_x > it->last_visible_x)))
	{
	  /* Maybe add truncation glyphs.  */
	  if (!FRAME_WINDOW_P (it->f))
	    {
	      int i, n;

	      if (!row->reversed_p)
		{
		  for (i = row->used[TEXT_AREA] - 1; i > 0; --i)
		    if (!CHAR_GLYPH_PADDING_P (row->glyphs[TEXT_AREA][i]))
		      break;
		}
	      else
		{
		  for (i = 0; i < row->used[TEXT_AREA]; i++)
		    if (!CHAR_GLYPH_PADDING_P (row->glyphs[TEXT_AREA][i]))
		      break;
		  /* Remove any padding glyphs at the front of ROW, to
		     make room for the truncation glyphs we will be
		     adding below.  The loop below always inserts at
		     least one truncation glyph, so also remove the
		     last glyph added to ROW.  */
		  unproduce_glyphs (it, i + 1);
		  /* Adjust i for the loop below.  */
		  i = row->used[TEXT_AREA] - (i + 1);
		}

	      for (n = row->used[TEXT_AREA]; i < n; ++i)
		{
		  row->used[TEXT_AREA] = i;
		  produce_special_glyphs (it, IT_TRUNCATION);
		}
	    }
	  else if (IT_OVERFLOW_NEWLINE_INTO_FRINGE (it))
	    {
	      /* Don't truncate if we can overflow newline into fringe.  */
	      if (!get_next_display_element (it))
		{
		  it->continuation_lines_width = 0;
		  row->ends_at_zv_p = 1;
		  row->exact_window_width_line_p = 1;
		  break;
		}
	      if (ITERATOR_AT_END_OF_LINE_P (it))
		{
		  row->exact_window_width_line_p = 1;
		  goto at_end_of_line;
		}
	    }

	  row->truncated_on_right_p = 1;
	  it->continuation_lines_width = 0;
	  reseat_at_next_visible_line_start (it, 0);
	  row->ends_at_zv_p = FETCH_BYTE (IT_BYTEPOS (*it) - 1) != '\n';
	  it->hpos = hpos_before;
	  it->current_x = x_before;
	  break;
	}
    }

  if (wrap_data)
    bidi_unshelve_cache (wrap_data, 1);

  /* If line is not empty and hscrolled, maybe insert truncation glyphs
     at the left window margin.  */
  if (it->first_visible_x
      && IT_CHARPOS (*it) != CHARPOS (row->start.pos))
    {
      if (!FRAME_WINDOW_P (it->f))
	insert_left_trunc_glyphs (it);
      row->truncated_on_left_p = 1;
    }

  /* Remember the position at which this line ends.

     BIDI Note: any code that needs MATRIX_ROW_START/END_CHARPOS
     cannot be before the call to find_row_edges below, since that is
     where these positions are determined. */
  row->end = it->current;
  if (!it->bidi_p)
    {
      row->minpos = row->start.pos;
      row->maxpos = row->end.pos;
    }
  else
    {
      /* ROW->minpos and ROW->maxpos must be the smallest and
	 `1 + the largest' buffer positions in ROW.  But if ROW was
	 bidi-reordered, these two positions can be anywhere in the
	 row, so we must determine them now.  */
      find_row_edges (it, row, min_pos, min_bpos, max_pos, max_bpos);
    }

  /* If the start of this line is the overlay arrow-position, then
     mark this glyph row as the one containing the overlay arrow.
     This is clearly a mess with variable size fonts.  It would be
     better to let it be displayed like cursors under X.  */
  if ((row->displays_text_p || !overlay_arrow_seen)
      && (overlay_arrow_string = overlay_arrow_at_row (it, row),
	  !NILP (overlay_arrow_string)))
    {
      /* Overlay arrow in window redisplay is a fringe bitmap.  */
      if (STRINGP (overlay_arrow_string))
	{
	  struct glyph_row *arrow_row
	    = get_overlay_arrow_glyph_row (it->w, overlay_arrow_string);
	  struct glyph *glyph = arrow_row->glyphs[TEXT_AREA];
	  struct glyph *arrow_end = glyph + arrow_row->used[TEXT_AREA];
	  struct glyph *p = row->glyphs[TEXT_AREA];
	  struct glyph *p2, *end;

	  /* Copy the arrow glyphs.  */
	  while (glyph < arrow_end)
	    *p++ = *glyph++;

	  /* Throw away padding glyphs.  */
	  p2 = p;
	  end = row->glyphs[TEXT_AREA] + row->used[TEXT_AREA];
	  while (p2 < end && CHAR_GLYPH_PADDING_P (*p2))
	    ++p2;
	  if (p2 > p)
	    {
	      while (p2 < end)
		*p++ = *p2++;
	      row->used[TEXT_AREA] = p2 - row->glyphs[TEXT_AREA];
	    }
	}
      else
	{
	  xassert (INTEGERP (overlay_arrow_string));
	  row->overlay_arrow_bitmap = XINT (overlay_arrow_string);
	}
      overlay_arrow_seen = 1;
    }

  /* Highlight trailing whitespace.  */
  if (!NILP (Vshow_trailing_whitespace))
    highlight_trailing_whitespace (it->f, it->glyph_row);

  /* Compute pixel dimensions of this line.  */
  compute_line_metrics (it);

  /* Implementation note: No changes in the glyphs of ROW or in their
     faces can be done past this point, because compute_line_metrics
     computes ROW's hash value and stores it within the glyph_row
     structure.  */

  /* Record whether this row ends inside an ellipsis.  */
  row->ends_in_ellipsis_p
    = (it->method == GET_FROM_DISPLAY_VECTOR
       && it->ellipsis_p);

  /* Save fringe bitmaps in this row.  */
  row->left_user_fringe_bitmap = it->left_user_fringe_bitmap;
  row->left_user_fringe_face_id = it->left_user_fringe_face_id;
  row->right_user_fringe_bitmap = it->right_user_fringe_bitmap;
  row->right_user_fringe_face_id = it->right_user_fringe_face_id;

  it->left_user_fringe_bitmap = 0;
  it->left_user_fringe_face_id = 0;
  it->right_user_fringe_bitmap = 0;
  it->right_user_fringe_face_id = 0;

  /* Maybe set the cursor.  */
  cvpos = it->w->cursor.vpos;
  if ((cvpos < 0
       /* In bidi-reordered rows, keep checking for proper cursor
	  position even if one has been found already, because buffer
	  positions in such rows change non-linearly with ROW->VPOS,
	  when a line is continued.  One exception: when we are at ZV,
	  display cursor on the first suitable glyph row, since all
	  the empty rows after that also have their position set to ZV.  */
       /* FIXME: Revisit this when glyph ``spilling'' in continuation
	  lines' rows is implemented for bidi-reordered rows.  */
       || (it->bidi_p
	   && !MATRIX_ROW (it->w->desired_matrix, cvpos)->ends_at_zv_p))
      && PT >= MATRIX_ROW_START_CHARPOS (row)
      && PT <= MATRIX_ROW_END_CHARPOS (row)
      && cursor_row_p (row))
    set_cursor_from_row (it->w, row, it->w->desired_matrix, 0, 0, 0, 0);

  /* Prepare for the next line.  This line starts horizontally at (X
     HPOS) = (0 0).  Vertical positions are incremented.  As a
     convenience for the caller, IT->glyph_row is set to the next
     row to be used.  */
  it->current_x = it->hpos = 0;
  it->current_y += row->height;
  SET_TEXT_POS (it->eol_pos, 0, 0);
  ++it->vpos;
  ++it->glyph_row;
  /* The next row should by default use the same value of the
     reversed_p flag as this one.  set_iterator_to_next decides when
     it's a new paragraph, and PRODUCE_GLYPHS recomputes the value of
     the flag accordingly.  */
  if (it->glyph_row < MATRIX_BOTTOM_TEXT_ROW (it->w->desired_matrix, it->w))
    it->glyph_row->reversed_p = row->reversed_p;
  it->start = row->end;
  return row->displays_text_p;

#undef RECORD_MAX_MIN_POS
}

DEFUN ("current-bidi-paragraph-direction", Fcurrent_bidi_paragraph_direction,
       Scurrent_bidi_paragraph_direction, 0, 1, 0,
       doc: /* Return paragraph direction at point in BUFFER.
Value is either `left-to-right' or `right-to-left'.
If BUFFER is omitted or nil, it defaults to the current buffer.

Paragraph direction determines how the text in the paragraph is displayed.
In left-to-right paragraphs, text begins at the left margin of the window
and the reading direction is generally left to right.  In right-to-left
paragraphs, text begins at the right margin and is read from right to left.

See also `bidi-paragraph-direction'.  */)
  (Lisp_Object buffer)
{
  struct buffer *buf = current_buffer;
  struct buffer *old = buf;

  if (! NILP (buffer))
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }

  if (NILP (BVAR (buf, bidi_display_reordering))
      || NILP (BVAR (buf, enable_multibyte_characters))
      /* When we are loading loadup.el, the character property tables
	 needed for bidi iteration are not yet available.  */
      || !NILP (Vpurify_flag))
    return Qleft_to_right;
  else if (!NILP (BVAR (buf, bidi_paragraph_direction)))
    return BVAR (buf, bidi_paragraph_direction);
  else
    {
      /* Determine the direction from buffer text.  We could try to
	 use current_matrix if it is up to date, but this seems fast
	 enough as it is.  */
      struct bidi_it itb;
      EMACS_INT pos = BUF_PT (buf);
      EMACS_INT bytepos = BUF_PT_BYTE (buf);
      int c;
      void *itb_data = bidi_shelve_cache ();

      set_buffer_temp (buf);
      /* bidi_paragraph_init finds the base direction of the paragraph
	 by searching forward from paragraph start.  We need the base
	 direction of the current or _previous_ paragraph, so we need
	 to make sure we are within that paragraph.  To that end, find
	 the previous non-empty line.  */
      if (pos >= ZV && pos > BEGV)
	{
	  pos--;
	  bytepos = CHAR_TO_BYTE (pos);
	}
      if (fast_looking_at (build_string ("[\f\t ]*\n"),
			   pos, bytepos, ZV, ZV_BYTE, Qnil) > 0)
	{
	  while ((c = FETCH_BYTE (bytepos)) == '\n'
		 || c == ' ' || c == '\t' || c == '\f')
	    {
	      if (bytepos <= BEGV_BYTE)
		break;
	      bytepos--;
	      pos--;
	    }
	  while (!CHAR_HEAD_P (FETCH_BYTE (bytepos)))
	    bytepos--;
	}
      bidi_init_it (pos, bytepos, FRAME_WINDOW_P (SELECTED_FRAME ()), &itb);
      itb.paragraph_dir = NEUTRAL_DIR;
      itb.string.s = NULL;
      itb.string.lstring = Qnil;
      itb.string.bufpos = 0;
      itb.string.unibyte = 0;
      bidi_paragraph_init (NEUTRAL_DIR, &itb, 1);
      bidi_unshelve_cache (itb_data, 0);
      set_buffer_temp (old);
      switch (itb.paragraph_dir)
	{
	case L2R:
	  return Qleft_to_right;
	  break;
	case R2L:
	  return Qright_to_left;
	  break;
	default:
	  abort ();
	}
    }
}



/***********************************************************************
			       Menu Bar
 ***********************************************************************/

/* Redisplay the menu bar in the frame for window W.

   The menu bar of X frames that don't have X toolkit support is
   displayed in a special window W->frame->menu_bar_window.

   The menu bar of terminal frames is treated specially as far as
   glyph matrices are concerned.  Menu bar lines are not part of
   windows, so the update is done directly on the frame matrix rows
   for the menu bar.  */

static void
display_menu_bar (struct window *w)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct it it;
  Lisp_Object items;
  int i;

  /* Don't do all this for graphical frames.  */
#ifdef HAVE_NTGUI
  if (FRAME_W32_P (f))
    return;
#endif
#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
  if (FRAME_X_P (f))
    return;
#endif

#ifdef HAVE_NS
  if (FRAME_NS_P (f))
    return;
#endif /* HAVE_NS */

#ifdef USE_X_TOOLKIT
  xassert (!FRAME_WINDOW_P (f));
  init_iterator (&it, w, -1, -1, f->desired_matrix->rows, MENU_FACE_ID);
  it.first_visible_x = 0;
  it.last_visible_x = FRAME_TOTAL_COLS (f) * FRAME_COLUMN_WIDTH (f);
#else /* not USE_X_TOOLKIT */
  if (FRAME_WINDOW_P (f))
    {
      /* Menu bar lines are displayed in the desired matrix of the
	 dummy window menu_bar_window.  */
      struct window *menu_w;
      xassert (WINDOWP (f->menu_bar_window));
      menu_w = XWINDOW (f->menu_bar_window);
      init_iterator (&it, menu_w, -1, -1, menu_w->desired_matrix->rows,
		     MENU_FACE_ID);
      it.first_visible_x = 0;
      it.last_visible_x = FRAME_TOTAL_COLS (f) * FRAME_COLUMN_WIDTH (f);
    }
  else
    {
      /* This is a TTY frame, i.e. character hpos/vpos are used as
	 pixel x/y.  */
      init_iterator (&it, w, -1, -1, f->desired_matrix->rows,
		     MENU_FACE_ID);
      it.first_visible_x = 0;
      it.last_visible_x = FRAME_COLS (f);
    }
#endif /* not USE_X_TOOLKIT */

  /* FIXME: This should be controlled by a user option.  See the
     comments in redisplay_tool_bar and display_mode_line about
     this.  */
  it.paragraph_embedding = L2R;

  if (! mode_line_inverse_video)
    /* Force the menu-bar to be displayed in the default face.  */
    it.base_face_id = it.face_id = DEFAULT_FACE_ID;

  /* Clear all rows of the menu bar.  */
  for (i = 0; i < FRAME_MENU_BAR_LINES (f); ++i)
    {
      struct glyph_row *row = it.glyph_row + i;
      clear_glyph_row (row);
      row->enabled_p = 1;
      row->full_width_p = 1;
    }

  /* Display all items of the menu bar.  */
  items = FRAME_MENU_BAR_ITEMS (it.f);
  for (i = 0; i < ASIZE (items); i += 4)
    {
      Lisp_Object string;

      /* Stop at nil string.  */
      string = AREF (items, i + 1);
      if (NILP (string))
	break;

      /* Remember where item was displayed.  */
      ASET (items, i + 3, make_number (it.hpos));

      /* Display the item, pad with one space.  */
      if (it.current_x < it.last_visible_x)
	display_string (NULL, string, Qnil, 0, 0, &it,
			SCHARS (string) + 1, 0, 0, -1);
    }

  /* Fill out the line with spaces.  */
  if (it.current_x < it.last_visible_x)
    display_string ("", Qnil, Qnil, 0, 0, &it, -1, 0, 0, -1);

  /* Compute the total height of the lines.  */
  compute_line_metrics (&it);
}



/***********************************************************************
			      Mode Line
 ***********************************************************************/

/* Redisplay mode lines in the window tree whose root is WINDOW.  If
   FORCE is non-zero, redisplay mode lines unconditionally.
   Otherwise, redisplay only mode lines that are garbaged.  Value is
   the number of windows whose mode lines were redisplayed.  */

static int
redisplay_mode_lines (Lisp_Object window, int force)
{
  int nwindows = 0;

  while (!NILP (window))
    {
      struct window *w = XWINDOW (window);

      if (WINDOWP (w->hchild))
	nwindows += redisplay_mode_lines (w->hchild, force);
      else if (WINDOWP (w->vchild))
	nwindows += redisplay_mode_lines (w->vchild, force);
      else if (force
	       || FRAME_GARBAGED_P (XFRAME (w->frame))
	       || !MATRIX_MODE_LINE_ROW (w->current_matrix)->enabled_p)
	{
	  struct text_pos lpoint;
	  struct buffer *old = current_buffer;

	  /* Set the window's buffer for the mode line display.  */
	  SET_TEXT_POS (lpoint, PT, PT_BYTE);
	  set_buffer_internal_1 (XBUFFER (w->buffer));

	  /* Point refers normally to the selected window.  For any
	     other window, set up appropriate value.  */
	  if (!EQ (window, selected_window))
	    {
	      struct text_pos pt;

	      SET_TEXT_POS_FROM_MARKER (pt, w->pointm);
	      if (CHARPOS (pt) < BEGV)
		TEMP_SET_PT_BOTH (BEGV, BEGV_BYTE);
	      else if (CHARPOS (pt) > (ZV - 1))
		TEMP_SET_PT_BOTH (ZV, ZV_BYTE);
	      else
		TEMP_SET_PT_BOTH (CHARPOS (pt), BYTEPOS (pt));
	    }

	  /* Display mode lines.  */
	  clear_glyph_matrix (w->desired_matrix);
	  if (display_mode_lines (w))
	    {
	      ++nwindows;
	      w->must_be_updated_p = 1;
	    }

	  /* Restore old settings.  */
	  set_buffer_internal_1 (old);
	  TEMP_SET_PT_BOTH (CHARPOS (lpoint), BYTEPOS (lpoint));
	}

      window = w->next;
    }

  return nwindows;
}


/* Display the mode and/or header line of window W.  Value is the
   sum number of mode lines and header lines displayed.  */

static int
display_mode_lines (struct window *w)
{
  Lisp_Object old_selected_window, old_selected_frame;
  int n = 0;

  old_selected_frame = selected_frame;
  selected_frame = w->frame;
  old_selected_window = selected_window;
  XSETWINDOW (selected_window, w);

  /* These will be set while the mode line specs are processed.  */
  line_number_displayed = 0;
  w->column_number_displayed = Qnil;

  if (WINDOW_WANTS_MODELINE_P (w))
    {
      struct window *sel_w = XWINDOW (old_selected_window);

      /* Select mode line face based on the real selected window.  */
      display_mode_line (w, CURRENT_MODE_LINE_FACE_ID_3 (sel_w, sel_w, w),
			 BVAR (current_buffer, mode_line_format));
      ++n;
    }

  if (WINDOW_WANTS_HEADER_LINE_P (w))
    {
      display_mode_line (w, HEADER_LINE_FACE_ID,
			 BVAR (current_buffer, header_line_format));
      ++n;
    }

  selected_frame = old_selected_frame;
  selected_window = old_selected_window;
  return n;
}


/* Display mode or header line of window W.  FACE_ID specifies which
   line to display; it is either MODE_LINE_FACE_ID or
   HEADER_LINE_FACE_ID.  FORMAT is the mode/header line format to
   display.  Value is the pixel height of the mode/header line
   displayed.  */

static int
display_mode_line (struct window *w, enum face_id face_id, Lisp_Object format)
{
  struct it it;
  struct face *face;
  int count = SPECPDL_INDEX ();

  init_iterator (&it, w, -1, -1, NULL, face_id);
  /* Don't extend on a previously drawn mode-line.
     This may happen if called from pos_visible_p.  */
  it.glyph_row->enabled_p = 0;
  prepare_desired_row (it.glyph_row);

  it.glyph_row->mode_line_p = 1;

  if (! mode_line_inverse_video)
    /* Force the mode-line to be displayed in the default face.  */
    it.base_face_id = it.face_id = DEFAULT_FACE_ID;

  /* FIXME: This should be controlled by a user option.  But
     supporting such an option is not trivial, since the mode line is
     made up of many separate strings.  */
  it.paragraph_embedding = L2R;

  record_unwind_protect (unwind_format_mode_line,
			 format_mode_line_unwind_data (NULL, Qnil, 0));

  mode_line_target = MODE_LINE_DISPLAY;

  /* Temporarily make frame's keyboard the current kboard so that
     kboard-local variables in the mode_line_format will get the right
     values.  */
  push_kboard (FRAME_KBOARD (it.f));
  record_unwind_save_match_data ();
  display_mode_element (&it, 0, 0, 0, format, Qnil, 0);
  pop_kboard ();

  unbind_to (count, Qnil);

  /* Fill up with spaces.  */
  display_string (" ", Qnil, Qnil, 0, 0, &it, 10000, -1, -1, 0);

  compute_line_metrics (&it);
  it.glyph_row->full_width_p = 1;
  it.glyph_row->continued_p = 0;
  it.glyph_row->truncated_on_left_p = 0;
  it.glyph_row->truncated_on_right_p = 0;

  /* Make a 3D mode-line have a shadow at its right end.  */
  face = FACE_FROM_ID (it.f, face_id);
  extend_face_to_end_of_line (&it);
  if (face->box != FACE_NO_BOX)
    {
      struct glyph *last = (it.glyph_row->glyphs[TEXT_AREA]
			    + it.glyph_row->used[TEXT_AREA] - 1);
      last->right_box_line_p = 1;
    }

  return it.glyph_row->height;
}

/* Move element ELT in LIST to the front of LIST.
   Return the updated list.  */

static Lisp_Object
move_elt_to_front (Lisp_Object elt, Lisp_Object list)
{
  register Lisp_Object tail, prev;
  register Lisp_Object tem;

  tail = list;
  prev = Qnil;
  while (CONSP (tail))
    {
      tem = XCAR (tail);

      if (EQ (elt, tem))
	{
	  /* Splice out the link TAIL.  */
	  if (NILP (prev))
	    list = XCDR (tail);
	  else
	    Fsetcdr (prev, XCDR (tail));

	  /* Now make it the first.  */
	  Fsetcdr (tail, list);
	  return tail;
	}
      else
	prev = tail;
      tail = XCDR (tail);
      QUIT;
    }

  /* Not found--return unchanged LIST.  */
  return list;
}

/* Contribute ELT to the mode line for window IT->w.  How it
   translates into text depends on its data type.

   IT describes the display environment in which we display, as usual.

   DEPTH is the depth in recursion.  It is used to prevent
   infinite recursion here.

   FIELD_WIDTH is the number of characters the display of ELT should
   occupy in the mode line, and PRECISION is the maximum number of
   characters to display from ELT's representation.  See
   display_string for details.

   Returns the hpos of the end of the text generated by ELT.

   PROPS is a property list to add to any string we encounter.

   If RISKY is nonzero, remove (disregard) any properties in any string
   we encounter, and ignore :eval and :propertize.

   The global variable `mode_line_target' determines whether the
   output is passed to `store_mode_line_noprop',
   `store_mode_line_string', or `display_string'.  */

static int
display_mode_element (struct it *it, int depth, int field_width, int precision,
		      Lisp_Object elt, Lisp_Object props, int risky)
{
  int n = 0, field, prec;
  int literal = 0;

 tail_recurse:
  if (depth > 100)
    elt = build_string ("*too-deep*");

  depth++;

  switch (SWITCH_ENUM_CAST (XTYPE (elt)))
    {
    case Lisp_String:
      {
	/* A string: output it and check for %-constructs within it.  */
	unsigned char c;
	EMACS_INT offset = 0;

	if (SCHARS (elt) > 0
	    && (!NILP (props) || risky))
	  {
	    Lisp_Object oprops, aelt;
	    oprops = Ftext_properties_at (make_number (0), elt);

	    /* If the starting string's properties are not what
	       we want, translate the string.  Also, if the string
	       is risky, do that anyway.  */

	    if (NILP (Fequal (props, oprops)) || risky)
	      {
		/* If the starting string has properties,
		   merge the specified ones onto the existing ones.  */
		if (! NILP (oprops) && !risky)
		  {
		    Lisp_Object tem;

		    oprops = Fcopy_sequence (oprops);
		    tem = props;
		    while (CONSP (tem))
		      {
			oprops = Fplist_put (oprops, XCAR (tem),
					     XCAR (XCDR (tem)));
			tem = XCDR (XCDR (tem));
		      }
		    props = oprops;
		  }

		aelt = Fassoc (elt, mode_line_proptrans_alist);
		if (! NILP (aelt) && !NILP (Fequal (props, XCDR (aelt))))
		  {
		    /* AELT is what we want.  Move it to the front
		       without consing.  */
		    elt = XCAR (aelt);
		    mode_line_proptrans_alist
		      = move_elt_to_front (aelt, mode_line_proptrans_alist);
		  }
		else
		  {
		    Lisp_Object tem;

		    /* If AELT has the wrong props, it is useless.
		       so get rid of it.  */
		    if (! NILP (aelt))
		      mode_line_proptrans_alist
			= Fdelq (aelt, mode_line_proptrans_alist);

		    elt = Fcopy_sequence (elt);
		    Fset_text_properties (make_number (0), Flength (elt),
					  props, elt);
		    /* Add this item to mode_line_proptrans_alist.  */
		    mode_line_proptrans_alist
		      = Fcons (Fcons (elt, props),
			       mode_line_proptrans_alist);
		    /* Truncate mode_line_proptrans_alist
		       to at most 50 elements.  */
		    tem = Fnthcdr (make_number (50),
				   mode_line_proptrans_alist);
		    if (! NILP (tem))
		      XSETCDR (tem, Qnil);
		  }
	      }
	  }

	offset = 0;

	if (literal)
	  {
	    prec = precision - n;
	    switch (mode_line_target)
	      {
	      case MODE_LINE_NOPROP:
	      case MODE_LINE_TITLE:
		n += store_mode_line_noprop (SSDATA (elt), -1, prec);
		break;
	      case MODE_LINE_STRING:
		n += store_mode_line_string (NULL, elt, 1, 0, prec, Qnil);
		break;
	      case MODE_LINE_DISPLAY:
		n += display_string (NULL, elt, Qnil, 0, 0, it,
				     0, prec, 0, STRING_MULTIBYTE (elt));
		break;
	      }

	    break;
	  }

	/* Handle the non-literal case.  */

	while ((precision <= 0 || n < precision)
	       && SREF (elt, offset) != 0
	       && (mode_line_target != MODE_LINE_DISPLAY
		   || it->current_x < it->last_visible_x))
	  {
	    EMACS_INT last_offset = offset;

	    /* Advance to end of string or next format specifier.  */
	    while ((c = SREF (elt, offset++)) != '\0' && c != '%')
	      ;

	    if (offset - 1 != last_offset)
	      {
		EMACS_INT nchars, nbytes;

		/* Output to end of string or up to '%'.  Field width
		   is length of string.  Don't output more than
		   PRECISION allows us.  */
		offset--;

		prec = c_string_width (SDATA (elt) + last_offset,
				       offset - last_offset, precision - n,
				       &nchars, &nbytes);

		switch (mode_line_target)
		  {
		  case MODE_LINE_NOPROP:
		  case MODE_LINE_TITLE:
		    n += store_mode_line_noprop (SSDATA (elt) + last_offset, 0, prec);
		    break;
		  case MODE_LINE_STRING:
		    {
		      EMACS_INT bytepos = last_offset;
		      EMACS_INT charpos = string_byte_to_char (elt, bytepos);
		      EMACS_INT endpos = (precision <= 0
					  ? string_byte_to_char (elt, offset)
					  : charpos + nchars);

		      n += store_mode_line_string (NULL,
						   Fsubstring (elt, make_number (charpos),
							       make_number (endpos)),
						   0, 0, 0, Qnil);
		    }
		    break;
		  case MODE_LINE_DISPLAY:
		    {
		      EMACS_INT bytepos = last_offset;
		      EMACS_INT charpos = string_byte_to_char (elt, bytepos);

		      if (precision <= 0)
			nchars = string_byte_to_char (elt, offset) - charpos;
		      n += display_string (NULL, elt, Qnil, 0, charpos,
					   it, 0, nchars, 0,
					   STRING_MULTIBYTE (elt));
		    }
		    break;
		  }
	      }
	    else /* c == '%' */
	      {
		EMACS_INT percent_position = offset;

		/* Get the specified minimum width.  Zero means
		   don't pad.  */
		field = 0;
		while ((c = SREF (elt, offset++)) >= '0' && c <= '9')
		  field = field * 10 + c - '0';

		/* Don't pad beyond the total padding allowed.  */
		if (field_width - n > 0 && field > field_width - n)
		  field = field_width - n;

		/* Note that either PRECISION <= 0 or N < PRECISION.  */
		prec = precision - n;

		if (c == 'M')
		  n += display_mode_element (it, depth, field, prec,
					     Vglobal_mode_string, props,
					     risky);
		else if (c != 0)
		  {
		    int multibyte;
		    EMACS_INT bytepos, charpos;
		    const char *spec;
		    Lisp_Object string;

		    bytepos = percent_position;
		    charpos = (STRING_MULTIBYTE (elt)
			       ? string_byte_to_char (elt, bytepos)
			       : bytepos);
		    spec = decode_mode_spec (it->w, c, field, &string);
		    multibyte = STRINGP (string) && STRING_MULTIBYTE (string);

		    switch (mode_line_target)
		      {
		      case MODE_LINE_NOPROP:
		      case MODE_LINE_TITLE:
			n += store_mode_line_noprop (spec, field, prec);
			break;
		      case MODE_LINE_STRING:
			{
			  Lisp_Object tem = build_string (spec);
			  props = Ftext_properties_at (make_number (charpos), elt);
			  /* Should only keep face property in props */
			  n += store_mode_line_string (NULL, tem, 0, field, prec, props);
			}
			break;
		      case MODE_LINE_DISPLAY:
			{
			  int nglyphs_before, nwritten;

			  nglyphs_before = it->glyph_row->used[TEXT_AREA];
			  nwritten = display_string (spec, string, elt,
						     charpos, 0, it,
						     field, prec, 0,
						     multibyte);

			  /* Assign to the glyphs written above the
			     string where the `%x' came from, position
			     of the `%'.  */
			  if (nwritten > 0)
			    {
			      struct glyph *glyph
				= (it->glyph_row->glyphs[TEXT_AREA]
				   + nglyphs_before);
			      int i;

			      for (i = 0; i < nwritten; ++i)
				{
				  glyph[i].object = elt;
				  glyph[i].charpos = charpos;
				}

			      n += nwritten;
			    }
			}
			break;
		      }
		  }
		else /* c == 0 */
		  break;
	      }
	  }
      }
      break;

    case Lisp_Symbol:
      /* A symbol: process the value of the symbol recursively
	 as if it appeared here directly.  Avoid error if symbol void.
	 Special case: if value of symbol is a string, output the string
	 literally.  */
      {
	register Lisp_Object tem;

	/* If the variable is not marked as risky to set
	   then its contents are risky to use.  */
	if (NILP (Fget (elt, Qrisky_local_variable)))
	  risky = 1;

	tem = Fboundp (elt);
	if (!NILP (tem))
	  {
	    tem = Fsymbol_value (elt);
	    /* If value is a string, output that string literally:
	       don't check for % within it.  */
	    if (STRINGP (tem))
	      literal = 1;

	    if (!EQ (tem, elt))
	      {
		/* Give up right away for nil or t.  */
		elt = tem;
		goto tail_recurse;
	      }
	  }
      }
      break;

    case Lisp_Cons:
      {
	register Lisp_Object car, tem;

	/* A cons cell: five distinct cases.
	   If first element is :eval or :propertize, do something special.
	   If first element is a string or a cons, process all the elements
	   and effectively concatenate them.
	   If first element is a negative number, truncate displaying cdr to
	   at most that many characters.  If positive, pad (with spaces)
	   to at least that many characters.
	   If first element is a symbol, process the cadr or caddr recursively
	   according to whether the symbol's value is non-nil or nil.  */
	car = XCAR (elt);
	if (EQ (car, QCeval))
	  {
	    /* An element of the form (:eval FORM) means evaluate FORM
	       and use the result as mode line elements.  */

	    if (risky)
	      break;

	    if (CONSP (XCDR (elt)))
	      {
		Lisp_Object spec;
		spec = safe_eval (XCAR (XCDR (elt)));
		n += display_mode_element (it, depth, field_width - n,
					   precision - n, spec, props,
					   risky);
	      }
	  }
	else if (EQ (car, QCpropertize))
	  {
	    /* An element of the form (:propertize ELT PROPS...)
	       means display ELT but applying properties PROPS.  */

	    if (risky)
	      break;

	    if (CONSP (XCDR (elt)))
	      n += display_mode_element (it, depth, field_width - n,
					 precision - n, XCAR (XCDR (elt)),
					 XCDR (XCDR (elt)), risky);
	  }
	else if (SYMBOLP (car))
	  {
	    tem = Fboundp (car);
	    elt = XCDR (elt);
	    if (!CONSP (elt))
	      goto invalid;
	    /* elt is now the cdr, and we know it is a cons cell.
	       Use its car if CAR has a non-nil value.  */
	    if (!NILP (tem))
	      {
		tem = Fsymbol_value (car);
		if (!NILP (tem))
		  {
		    elt = XCAR (elt);
		    goto tail_recurse;
		  }
	      }
	    /* Symbol's value is nil (or symbol is unbound)
	       Get the cddr of the original list
	       and if possible find the caddr and use that.  */
	    elt = XCDR (elt);
	    if (NILP (elt))
	      break;
	    else if (!CONSP (elt))
	      goto invalid;
	    elt = XCAR (elt);
	    goto tail_recurse;
	  }
	else if (INTEGERP (car))
	  {
	    register int lim = XINT (car);
	    elt = XCDR (elt);
	    if (lim < 0)
	      {
		/* Negative int means reduce maximum width.  */
		if (precision <= 0)
		  precision = -lim;
		else
		  precision = min (precision, -lim);
	      }
	    else if (lim > 0)
	      {
		/* Padding specified.  Don't let it be more than
		   current maximum.  */
		if (precision > 0)
		  lim = min (precision, lim);

		/* If that's more padding than already wanted, queue it.
		   But don't reduce padding already specified even if
		   that is beyond the current truncation point.  */
		field_width = max (lim, field_width);
	      }
	    goto tail_recurse;
	  }
	else if (STRINGP (car) || CONSP (car))
	  {
	    Lisp_Object halftail = elt;
	    int len = 0;

	    while (CONSP (elt)
		   && (precision <= 0 || n < precision))
	      {
		n += display_mode_element (it, depth,
					   /* Do padding only after the last
					      element in the list.  */
					   (! CONSP (XCDR (elt))
					    ? field_width - n
					    : 0),
					   precision - n, XCAR (elt),
					   props, risky);
		elt = XCDR (elt);
		len++;
		if ((len & 1) == 0)
		  halftail = XCDR (halftail);
		/* Check for cycle.  */
		if (EQ (halftail, elt))
		  break;
	      }
	  }
      }
      break;

    default:
    invalid:
      elt = build_string ("*invalid*");
      goto tail_recurse;
    }

  /* Pad to FIELD_WIDTH.  */
  if (field_width > 0 && n < field_width)
    {
      switch (mode_line_target)
	{
	case MODE_LINE_NOPROP:
	case MODE_LINE_TITLE:
	  n += store_mode_line_noprop ("", field_width - n, 0);
	  break;
	case MODE_LINE_STRING:
	  n += store_mode_line_string ("", Qnil, 0, field_width - n, 0, Qnil);
	  break;
	case MODE_LINE_DISPLAY:
	  n += display_string ("", Qnil, Qnil, 0, 0, it, field_width - n,
			       0, 0, 0);
	  break;
	}
    }

  return n;
}

/* Store a mode-line string element in mode_line_string_list.

   If STRING is non-null, display that C string.  Otherwise, the Lisp
   string LISP_STRING is displayed.

   FIELD_WIDTH is the minimum number of output glyphs to produce.
   If STRING has fewer characters than FIELD_WIDTH, pad to the right
   with spaces.  FIELD_WIDTH <= 0 means don't pad.

   PRECISION is the maximum number of characters to output from
   STRING.  PRECISION <= 0  means don't truncate the string.

   If COPY_STRING is non-zero, make a copy of LISP_STRING before adding
   properties to the string.

   PROPS are the properties to add to the string.
   The mode_line_string_face face property is always added to the string.
 */

static int
store_mode_line_string (const char *string, Lisp_Object lisp_string, int copy_string,
			int field_width, int precision, Lisp_Object props)
{
  EMACS_INT len;
  int n = 0;

  if (string != NULL)
    {
      len = strlen (string);
      if (precision > 0 && len > precision)
	len = precision;
      lisp_string = make_string (string, len);
      if (NILP (props))
	props = mode_line_string_face_prop;
      else if (!NILP (mode_line_string_face))
	{
	  Lisp_Object face = Fplist_get (props, Qface);
	  props = Fcopy_sequence (props);
	  if (NILP (face))
	    face = mode_line_string_face;
	  else
	    face = Fcons (face, Fcons (mode_line_string_face, Qnil));
	  props = Fplist_put (props, Qface, face);
	}
      Fadd_text_properties (make_number (0), make_number (len),
			    props, lisp_string);
    }
  else
    {
      len = XFASTINT (Flength (lisp_string));
      if (precision > 0 && len > precision)
	{
	  len = precision;
	  lisp_string = Fsubstring (lisp_string, make_number (0), make_number (len));
	  precision = -1;
	}
      if (!NILP (mode_line_string_face))
	{
	  Lisp_Object face;
	  if (NILP (props))
	    props = Ftext_properties_at (make_number (0), lisp_string);
	  face = Fplist_get (props, Qface);
	  if (NILP (face))
	    face = mode_line_string_face;
	  else
	    face = Fcons (face, Fcons (mode_line_string_face, Qnil));
	  props = Fcons (Qface, Fcons (face, Qnil));
	  if (copy_string)
	    lisp_string = Fcopy_sequence (lisp_string);
	}
      if (!NILP (props))
	Fadd_text_properties (make_number (0), make_number (len),
			      props, lisp_string);
    }

  if (len > 0)
    {
      mode_line_string_list = Fcons (lisp_string, mode_line_string_list);
      n += len;
    }

  if (field_width > len)
    {
      field_width -= len;
      lisp_string = Fmake_string (make_number (field_width), make_number (' '));
      if (!NILP (props))
	Fadd_text_properties (make_number (0), make_number (field_width),
			      props, lisp_string);
      mode_line_string_list = Fcons (lisp_string, mode_line_string_list);
      n += field_width;
    }

  return n;
}


DEFUN ("format-mode-line", Fformat_mode_line, Sformat_mode_line,
       1, 4, 0,
       doc: /* Format a string out of a mode line format specification.
First arg FORMAT specifies the mode line format (see `mode-line-format'
for details) to use.

By default, the format is evaluated for the currently selected window.

Optional second arg FACE specifies the face property to put on all
characters for which no face is specified.  The value nil means the
default face.  The value t means whatever face the window's mode line
currently uses (either `mode-line' or `mode-line-inactive',
depending on whether the window is the selected window or not).
An integer value means the value string has no text
properties.

Optional third and fourth args WINDOW and BUFFER specify the window
and buffer to use as the context for the formatting (defaults
are the selected window and the WINDOW's buffer).  */)
     (Lisp_Object format, Lisp_Object face,
      Lisp_Object window, Lisp_Object buffer)
{
  struct it it;
  int len;
  struct window *w;
  struct buffer *old_buffer = NULL;
  int face_id;
  int no_props = INTEGERP (face);
  int count = SPECPDL_INDEX ();
  Lisp_Object str;
  int string_start = 0;

  if (NILP (window))
    window = selected_window;
  CHECK_WINDOW (window);
  w = XWINDOW (window);

  if (NILP (buffer))
    buffer = w->buffer;
  CHECK_BUFFER (buffer);

  /* Make formatting the modeline a non-op when noninteractive, otherwise
     there will be problems later caused by a partially initialized frame.  */
  if (NILP (format) || noninteractive)
    return empty_unibyte_string;

  if (no_props)
    face = Qnil;

  face_id = (NILP (face) || EQ (face, Qdefault)) ? DEFAULT_FACE_ID
    : EQ (face, Qt) ? (EQ (window, selected_window)
		       ? MODE_LINE_FACE_ID : MODE_LINE_INACTIVE_FACE_ID)
    : EQ (face, Qmode_line) ? MODE_LINE_FACE_ID
    : EQ (face, Qmode_line_inactive) ? MODE_LINE_INACTIVE_FACE_ID
    : EQ (face, Qheader_line) ? HEADER_LINE_FACE_ID
    : EQ (face, Qtool_bar) ? TOOL_BAR_FACE_ID
    : DEFAULT_FACE_ID;

  if (XBUFFER (buffer) != current_buffer)
    old_buffer = current_buffer;

  /* Save things including mode_line_proptrans_alist,
     and set that to nil so that we don't alter the outer value.  */
  record_unwind_protect (unwind_format_mode_line,
			 format_mode_line_unwind_data
			     (old_buffer, selected_window, 1));
  mode_line_proptrans_alist = Qnil;

  Fselect_window (window, Qt);
  if (old_buffer)
    set_buffer_internal_1 (XBUFFER (buffer));

  init_iterator (&it, w, -1, -1, NULL, face_id);

  if (no_props)
    {
      mode_line_target = MODE_LINE_NOPROP;
      mode_line_string_face_prop = Qnil;
      mode_line_string_list = Qnil;
      string_start = MODE_LINE_NOPROP_LEN (0);
    }
  else
    {
      mode_line_target = MODE_LINE_STRING;
      mode_line_string_list = Qnil;
      mode_line_string_face = face;
      mode_line_string_face_prop
	= (NILP (face) ? Qnil : Fcons (Qface, Fcons (face, Qnil)));
    }

  push_kboard (FRAME_KBOARD (it.f));
  display_mode_element (&it, 0, 0, 0, format, Qnil, 0);
  pop_kboard ();

  if (no_props)
    {
      len = MODE_LINE_NOPROP_LEN (string_start);
      str = make_string (mode_line_noprop_buf + string_start, len);
    }
  else
    {
      mode_line_string_list = Fnreverse (mode_line_string_list);
      str = Fmapconcat (intern ("identity"), mode_line_string_list,
			empty_unibyte_string);
    }

  unbind_to (count, Qnil);
  return str;
}

/* Write a null-terminated, right justified decimal representation of
   the positive integer D to BUF using a minimal field width WIDTH.  */

static void
pint2str (register char *buf, register int width, register EMACS_INT d)
{
  register char *p = buf;

  if (d <= 0)
    *p++ = '0';
  else
    {
      while (d > 0)
	{
	  *p++ = d % 10 + '0';
	  d /= 10;
	}
    }

  for (width -= (int) (p - buf); width > 0; --width)
    *p++ = ' ';
  *p-- = '\0';
  while (p > buf)
    {
      d = *buf;
      *buf++ = *p;
      *p-- = d;
    }
}

/* Write a null-terminated, right justified decimal and "human
   readable" representation of the nonnegative integer D to BUF using
   a minimal field width WIDTH.  D should be smaller than 999.5e24. */

static const char power_letter[] =
  {
    0,	 /* no letter */
    'k', /* kilo */
    'M', /* mega */
    'G', /* giga */
    'T', /* tera */
    'P', /* peta */
    'E', /* exa */
    'Z', /* zetta */
    'Y'	 /* yotta */
  };

static void
pint2hrstr (char *buf, int width, EMACS_INT d)
{
  /* We aim to represent the nonnegative integer D as
     QUOTIENT.TENTHS * 10 ^ (3 * EXPONENT). */
  EMACS_INT quotient = d;
  int remainder = 0;
  /* -1 means: do not use TENTHS. */
  int tenths = -1;
  int exponent = 0;

  /* Length of QUOTIENT.TENTHS as a string. */
  int length;

  char * psuffix;
  char * p;

  if (1000 <= quotient)
    {
      /* Scale to the appropriate EXPONENT. */
      do
	{
	  remainder = quotient % 1000;
	  quotient /= 1000;
	  exponent++;
	}
      while (1000 <= quotient);

      /* Round to nearest and decide whether to use TENTHS or not. */
      if (quotient <= 9)
	{
	  tenths = remainder / 100;
	  if (50 <= remainder % 100)
	    {
	      if (tenths < 9)
		tenths++;
	      else
		{
		  quotient++;
		  if (quotient == 10)
		    tenths = -1;
		  else
		    tenths = 0;
		}
	    }
	}
      else
	if (500 <= remainder)
	  {
	    if (quotient < 999)
	      quotient++;
	    else
	      {
		quotient = 1;
		exponent++;
		tenths = 0;
	      }
	  }
    }

  /* Calculate the LENGTH of QUOTIENT.TENTHS as a string. */
  if (tenths == -1 && quotient <= 99)
    if (quotient <= 9)
      length = 1;
    else
      length = 2;
  else
    length = 3;
  p = psuffix = buf + max (width, length);

  /* Print EXPONENT. */
  *psuffix++ = power_letter[exponent];
  *psuffix = '\0';

  /* Print TENTHS. */
  if (tenths >= 0)
    {
      *--p = '0' + tenths;
      *--p = '.';
    }

  /* Print QUOTIENT. */
  do
    {
      int digit = quotient % 10;
      *--p =  '0' + digit;
    }
  while ((quotient /= 10) != 0);

  /* Print leading spaces. */
  while (buf < p)
    *--p = ' ';
}

/* Set a mnemonic character for coding_system (Lisp symbol) in BUF.
   If EOL_FLAG is 1, set also a mnemonic character for end-of-line
   type of CODING_SYSTEM.  Return updated pointer into BUF.  */

static unsigned char invalid_eol_type[] = "(*invalid*)";

static char *
decode_mode_spec_coding (Lisp_Object coding_system, register char *buf, int eol_flag)
{
  Lisp_Object val;
  int multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  const unsigned char *eol_str;
  int eol_str_len;
  /* The EOL conversion we are using.  */
  Lisp_Object eoltype;

  val = CODING_SYSTEM_SPEC (coding_system);
  eoltype = Qnil;

  if (!VECTORP (val))		/* Not yet decided.  */
    {
      if (multibyte)
	*buf++ = '-';
      if (eol_flag)
	eoltype = eol_mnemonic_undecided;
      /* Don't mention EOL conversion if it isn't decided.  */
    }
  else
    {
      Lisp_Object attrs;
      Lisp_Object eolvalue;

      attrs = AREF (val, 0);
      eolvalue = AREF (val, 2);

      if (multibyte)
	*buf++ = XFASTINT (CODING_ATTR_MNEMONIC (attrs));

      if (eol_flag)
	{
	  /* The EOL conversion that is normal on this system.  */

	  if (NILP (eolvalue))	/* Not yet decided.  */
	    eoltype = eol_mnemonic_undecided;
	  else if (VECTORP (eolvalue)) /* Not yet decided.  */
	    eoltype = eol_mnemonic_undecided;
	  else			/* eolvalue is Qunix, Qdos, or Qmac.  */
	    eoltype = (EQ (eolvalue, Qunix)
		       ? eol_mnemonic_unix
		       : (EQ (eolvalue, Qdos) == 1
			  ? eol_mnemonic_dos : eol_mnemonic_mac));
	}
    }

  if (eol_flag)
    {
      /* Mention the EOL conversion if it is not the usual one.  */
      if (STRINGP (eoltype))
	{
	  eol_str = SDATA (eoltype);
	  eol_str_len = SBYTES (eoltype);
	}
      else if (CHARACTERP (eoltype))
	{
	  unsigned char *tmp = (unsigned char *) alloca (MAX_MULTIBYTE_LENGTH);
	  int c = XFASTINT (eoltype);
	  eol_str_len = CHAR_STRING (c, tmp);
	  eol_str = tmp;
	}
      else
	{
	  eol_str = invalid_eol_type;
	  eol_str_len = sizeof (invalid_eol_type) - 1;
	}
      memcpy (buf, eol_str, eol_str_len);
      buf += eol_str_len;
    }

  return buf;
}

/* Return a string for the output of a mode line %-spec for window W,
   generated by character C.  FIELD_WIDTH > 0 means pad the string
   returned with spaces to that value.  Return a Lisp string in
   *STRING if the resulting string is taken from that Lisp string.

   Note we operate on the current buffer for most purposes,
   the exception being w->base_line_pos.  */

static char lots_of_dashes[] = "--------------------------------------------------------------------------------------------------------------------------------------------";

static const char *
decode_mode_spec (struct window *w, register int c, int field_width,
		  Lisp_Object *string)
{
  Lisp_Object obj;
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  char *decode_mode_spec_buf = f->decode_mode_spec_buffer;
  struct buffer *b = current_buffer;

  obj = Qnil;
  *string = Qnil;

  switch (c)
    {
    case '*':
      if (!NILP (BVAR (b, read_only)))
	return "%";
      if (BUF_MODIFF (b) > BUF_SAVE_MODIFF (b))
	return "*";
      return "-";

    case '+':
      /* This differs from %* only for a modified read-only buffer.  */
      if (BUF_MODIFF (b) > BUF_SAVE_MODIFF (b))
	return "*";
      if (!NILP (BVAR (b, read_only)))
	return "%";
      return "-";

    case '&':
      /* This differs from %* in ignoring read-only-ness.  */
      if (BUF_MODIFF (b) > BUF_SAVE_MODIFF (b))
	return "*";
      return "-";

    case '%':
      return "%";

    case '[':
      {
	int i;
	char *p;

	if (command_loop_level > 5)
	  return "[[[... ";
	p = decode_mode_spec_buf;
	for (i = 0; i < command_loop_level; i++)
	  *p++ = '[';
	*p = 0;
	return decode_mode_spec_buf;
      }

    case ']':
      {
	int i;
	char *p;

	if (command_loop_level > 5)
	  return " ...]]]";
	p = decode_mode_spec_buf;
	for (i = 0; i < command_loop_level; i++)
	  *p++ = ']';
	*p = 0;
	return decode_mode_spec_buf;
      }

    case '-':
      {
	register int i;

	/* Let lots_of_dashes be a string of infinite length.  */
	if (mode_line_target == MODE_LINE_NOPROP ||
	    mode_line_target == MODE_LINE_STRING)
	  return "--";
	if (field_width <= 0
	    || field_width > sizeof (lots_of_dashes))
	  {
	    for (i = 0; i < FRAME_MESSAGE_BUF_SIZE (f) - 1; ++i)
	      decode_mode_spec_buf[i] = '-';
	    decode_mode_spec_buf[i] = '\0';
	    return decode_mode_spec_buf;
	  }
	else
	  return lots_of_dashes;
      }

    case 'b':
      obj = BVAR (b, name);
      break;

    case 'c':
      /* %c and %l are ignored in `frame-title-format'.
         (In redisplay_internal, the frame title is drawn _before_ the
         windows are updated, so the stuff which depends on actual
         window contents (such as %l) may fail to render properly, or
         even crash emacs.)  */
      if (mode_line_target == MODE_LINE_TITLE)
	return "";
      else
	{
	  EMACS_INT col = current_column ();
	  w->column_number_displayed = make_number (col);
	  pint2str (decode_mode_spec_buf, field_width, col);
	  return decode_mode_spec_buf;
	}

    case 'e':
#ifndef SYSTEM_MALLOC
      {
	if (NILP (Vmemory_full))
	  return "";
	else
	  return "!MEM FULL! ";
      }
#else
      return "";
#endif

    case 'F':
      /* %F displays the frame name.  */
      if (!NILP (f->title))
	return SSDATA (f->title);
      if (f->explicit_name || ! FRAME_WINDOW_P (f))
	return SSDATA (f->name);
      return "Emacs";

    case 'f':
      obj = BVAR (b, filename);
      break;

    case 'i':
      {
	EMACS_INT size = ZV - BEGV;
	pint2str (decode_mode_spec_buf, field_width, size);
	return decode_mode_spec_buf;
      }

    case 'I':
      {
	EMACS_INT size = ZV - BEGV;
	pint2hrstr (decode_mode_spec_buf, field_width, size);
	return decode_mode_spec_buf;
      }

    case 'l':
      {
	EMACS_INT startpos, startpos_byte, line, linepos, linepos_byte;
	EMACS_INT topline, nlines, height;
	EMACS_INT junk;

	/* %c and %l are ignored in `frame-title-format'.  */
	if (mode_line_target == MODE_LINE_TITLE)
	  return "";

	startpos = XMARKER (w->start)->charpos;
	startpos_byte = marker_byte_position (w->start);
	height = WINDOW_TOTAL_LINES (w);

	/* If we decided that this buffer isn't suitable for line numbers,
	   don't forget that too fast.  */
	if (EQ (w->base_line_pos, w->buffer))
	  goto no_value;
	/* But do forget it, if the window shows a different buffer now.  */
	else if (BUFFERP (w->base_line_pos))
	  w->base_line_pos = Qnil;

	/* If the buffer is very big, don't waste time.  */
	if (INTEGERP (Vline_number_display_limit)
	    && BUF_ZV (b) - BUF_BEGV (b) > XINT (Vline_number_display_limit))
	  {
	    w->base_line_pos = Qnil;
	    w->base_line_number = Qnil;
	    goto no_value;
	  }

	if (INTEGERP (w->base_line_number)
	    && INTEGERP (w->base_line_pos)
	    && XFASTINT (w->base_line_pos) <= startpos)
	  {
	    line = XFASTINT (w->base_line_number);
	    linepos = XFASTINT (w->base_line_pos);
	    linepos_byte = buf_charpos_to_bytepos (b, linepos);
	  }
	else
	  {
	    line = 1;
	    linepos = BUF_BEGV (b);
	    linepos_byte = BUF_BEGV_BYTE (b);
	  }

	/* Count lines from base line to window start position.  */
	nlines = display_count_lines (linepos_byte,
				      startpos_byte,
				      startpos, &junk);

	topline = nlines + line;

	/* Determine a new base line, if the old one is too close
	   or too far away, or if we did not have one.
	   "Too close" means it's plausible a scroll-down would
	   go back past it.  */
	if (startpos == BUF_BEGV (b))
	  {
	    w->base_line_number = make_number (topline);
	    w->base_line_pos = make_number (BUF_BEGV (b));
	  }
	else if (nlines < height + 25 || nlines > height * 3 + 50
		 || linepos == BUF_BEGV (b))
	  {
	    EMACS_INT limit = BUF_BEGV (b);
	    EMACS_INT limit_byte = BUF_BEGV_BYTE (b);
	    EMACS_INT position;
	    EMACS_INT distance =
	      (height * 2 + 30) * line_number_display_limit_width;

	    if (startpos - distance > limit)
	      {
		limit = startpos - distance;
		limit_byte = CHAR_TO_BYTE (limit);
	      }

	    nlines = display_count_lines (startpos_byte,
					  limit_byte,
					  - (height * 2 + 30),
					  &position);
	    /* If we couldn't find the lines we wanted within
	       line_number_display_limit_width chars per line,
	       give up on line numbers for this window.  */
	    if (position == limit_byte && limit == startpos - distance)
	      {
		w->base_line_pos = w->buffer;
		w->base_line_number = Qnil;
		goto no_value;
	      }

	    w->base_line_number = make_number (topline - nlines);
	    w->base_line_pos = make_number (BYTE_TO_CHAR (position));
	  }

	/* Now count lines from the start pos to point.  */
	nlines = display_count_lines (startpos_byte,
				      PT_BYTE, PT, &junk);

	/* Record that we did display the line number.  */
	line_number_displayed = 1;

	/* Make the string to show.  */
	pint2str (decode_mode_spec_buf, field_width, topline + nlines);
	return decode_mode_spec_buf;
    no_value:
        {
	  char* p = decode_mode_spec_buf;
	  int pad = field_width - 2;
	  while (pad-- > 0)
	    *p++ = ' ';
	  *p++ = '?';
	  *p++ = '?';
	  *p = '\0';
	  return decode_mode_spec_buf;
	}
      }
      break;

    case 'm':
      obj = BVAR (b, mode_name);
      break;

    case 'n':
      if (BUF_BEGV (b) > BUF_BEG (b) || BUF_ZV (b) < BUF_Z (b))
	return " Narrow";
      break;

    case 'p':
      {
	EMACS_INT pos = marker_position (w->start);
	EMACS_INT total = BUF_ZV (b) - BUF_BEGV (b);

	if (XFASTINT (w->window_end_pos) <= BUF_Z (b) - BUF_ZV (b))
	  {
	    if (pos <= BUF_BEGV (b))
	      return "All";
	    else
	      return "Bottom";
	  }
	else if (pos <= BUF_BEGV (b))
	  return "Top";
	else
	  {
	    if (total > 1000000)
	      /* Do it differently for a large value, to avoid overflow.  */
	      total = ((pos - BUF_BEGV (b)) + (total / 100) - 1) / (total / 100);
	    else
	      total = ((pos - BUF_BEGV (b)) * 100 + total - 1) / total;
	    /* We can't normally display a 3-digit number,
	       so get us a 2-digit number that is close.  */
	    if (total == 100)
	      total = 99;
	    sprintf (decode_mode_spec_buf, "%2"pI"d%%", total);
	    return decode_mode_spec_buf;
	  }
      }

      /* Display percentage of size above the bottom of the screen.  */
    case 'P':
      {
	EMACS_INT toppos = marker_position (w->start);
	EMACS_INT botpos = BUF_Z (b) - XFASTINT (w->window_end_pos);
	EMACS_INT total = BUF_ZV (b) - BUF_BEGV (b);

	if (botpos >= BUF_ZV (b))
	  {
	    if (toppos <= BUF_BEGV (b))
	      return "All";
	    else
	      return "Bottom";
	  }
	else
	  {
	    if (total > 1000000)
	      /* Do it differently for a large value, to avoid overflow.  */
	      total = ((botpos - BUF_BEGV (b)) + (total / 100) - 1) / (total / 100);
	    else
	      total = ((botpos - BUF_BEGV (b)) * 100 + total - 1) / total;
	    /* We can't normally display a 3-digit number,
	       so get us a 2-digit number that is close.  */
	    if (total == 100)
	      total = 99;
	    if (toppos <= BUF_BEGV (b))
	      sprintf (decode_mode_spec_buf, "Top%2"pI"d%%", total);
	    else
	      sprintf (decode_mode_spec_buf, "%2"pI"d%%", total);
	    return decode_mode_spec_buf;
	  }
      }

    case 's':
      /* status of process */
      obj = Fget_buffer_process (Fcurrent_buffer ());
      if (NILP (obj))
	return "no process";
#ifndef MSDOS
      obj = Fsymbol_name (Fprocess_status (obj));
#endif
      break;

    case '@':
      {
	int count = inhibit_garbage_collection ();
	Lisp_Object val = call1 (intern ("file-remote-p"),
				 BVAR (current_buffer, directory));
	unbind_to (count, Qnil);

	if (NILP (val))
	  return "-";
	else
	  return "@";
      }

    case 't':			/* indicate TEXT or BINARY */
      return "T";

    case 'z':
      /* coding-system (not including end-of-line format) */
    case 'Z':
      /* coding-system (including end-of-line type) */
      {
	int eol_flag = (c == 'Z');
	char *p = decode_mode_spec_buf;

	if (! FRAME_WINDOW_P (f))
	  {
	    /* No need to mention EOL here--the terminal never needs
	       to do EOL conversion.  */
	    p = decode_mode_spec_coding (CODING_ID_NAME
					 (FRAME_KEYBOARD_CODING (f)->id),
					 p, 0);
	    p = decode_mode_spec_coding (CODING_ID_NAME
					 (FRAME_TERMINAL_CODING (f)->id),
					 p, 0);
	  }
	p = decode_mode_spec_coding (BVAR (b, buffer_file_coding_system),
				     p, eol_flag);

#if 0 /* This proves to be annoying; I think we can do without.  -- rms.  */
#ifdef subprocesses
	obj = Fget_buffer_process (Fcurrent_buffer ());
	if (PROCESSP (obj))
	  {
	    p = decode_mode_spec_coding (XPROCESS (obj)->decode_coding_system,
					 p, eol_flag);
	    p = decode_mode_spec_coding (XPROCESS (obj)->encode_coding_system,
					 p, eol_flag);
	  }
#endif /* subprocesses */
#endif /* 0 */
	*p = 0;
	return decode_mode_spec_buf;
      }
    }

  if (STRINGP (obj))
    {
      *string = obj;
      return SSDATA (obj);
    }
  else
    return "";
}


/* Count up to COUNT lines starting from START_BYTE.
   But don't go beyond LIMIT_BYTE.
   Return the number of lines thus found (always nonnegative).

   Set *BYTE_POS_PTR to 1 if we found COUNT lines, 0 if we hit LIMIT.  */

static EMACS_INT
display_count_lines (EMACS_INT start_byte,
		     EMACS_INT limit_byte, EMACS_INT count,
		     EMACS_INT *byte_pos_ptr)
{
  register unsigned char *cursor;
  unsigned char *base;

  register EMACS_INT ceiling;
  register unsigned char *ceiling_addr;
  EMACS_INT orig_count = count;

  /* If we are not in selective display mode,
     check only for newlines.  */
  int selective_display = (!NILP (BVAR (current_buffer, selective_display))
			   && !INTEGERP (BVAR (current_buffer, selective_display)));

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
	      if (selective_display)
		while (*cursor != '\n' && *cursor != 015 && ++cursor != ceiling_addr)
		  ;
	      else
		while (*cursor != '\n' && ++cursor != ceiling_addr)
		  ;

	      if (cursor != ceiling_addr)
		{
		  if (--count == 0)
		    {
		      start_byte += cursor - base + 1;
		      *byte_pos_ptr = start_byte;
		      return orig_count;
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
	      if (selective_display)
		while (--cursor != ceiling_addr
		       && *cursor != '\n' && *cursor != 015)
		  ;
	      else
		while (--cursor != ceiling_addr && *cursor != '\n')
		  ;

	      if (cursor != ceiling_addr)
		{
		  if (++count == 0)
		    {
		      start_byte += cursor - base + 1;
		      *byte_pos_ptr = start_byte;
		      /* When scanning backwards, we should
			 not count the newline posterior to which we stop.  */
		      return - orig_count - 1;
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

  *byte_pos_ptr = limit_byte;

  if (count < 0)
    return - orig_count + count;
  return orig_count - count;

}



/***********************************************************************
			 Displaying strings
 ***********************************************************************/

/* Display a NUL-terminated string, starting with index START.

   If STRING is non-null, display that C string.  Otherwise, the Lisp
   string LISP_STRING is displayed.  There's a case that STRING is
   non-null and LISP_STRING is not nil.  It means STRING is a string
   data of LISP_STRING.  In that case, we display LISP_STRING while
   ignoring its text properties.

   If FACE_STRING is not nil, FACE_STRING_POS is a position in
   FACE_STRING.  Display STRING or LISP_STRING with the face at
   FACE_STRING_POS in FACE_STRING:

   Display the string in the environment given by IT, but use the
   standard display table, temporarily.

   FIELD_WIDTH is the minimum number of output glyphs to produce.
   If STRING has fewer characters than FIELD_WIDTH, pad to the right
   with spaces.  If STRING has more characters, more than FIELD_WIDTH
   glyphs will be produced.  FIELD_WIDTH <= 0 means don't pad.

   PRECISION is the maximum number of characters to output from
   STRING.  PRECISION < 0  means don't truncate the string.

   This is roughly equivalent to printf format specifiers:

   FIELD_WIDTH	PRECISION	PRINTF
   ----------------------------------------
   -1		-1		%s
   -1		10		%.10s
   10		-1		%10s
   20		10		%20.10s

   MULTIBYTE zero means do not display multibyte chars, > 0 means do
   display them, and < 0 means obey the current buffer's value of
   enable_multibyte_characters.

   Value is the number of columns displayed.  */

static int
display_string (const char *string, Lisp_Object lisp_string, Lisp_Object face_string,
		EMACS_INT face_string_pos, EMACS_INT start, struct it *it,
		int field_width, int precision, int max_x, int multibyte)
{
  int hpos_at_start = it->hpos;
  int saved_face_id = it->face_id;
  struct glyph_row *row = it->glyph_row;
  EMACS_INT it_charpos;

  /* Initialize the iterator IT for iteration over STRING beginning
     with index START.  */
  reseat_to_string (it, NILP (lisp_string) ? string : NULL, lisp_string, start,
		    precision, field_width, multibyte);
  if (string && STRINGP (lisp_string))
    /* LISP_STRING is the one returned by decode_mode_spec.  We should
       ignore its text properties.  */
    it->stop_charpos = it->end_charpos;

  /* If displaying STRING, set up the face of the iterator from
     FACE_STRING, if that's given.  */
  if (STRINGP (face_string))
    {
      EMACS_INT endptr;
      struct face *face;

      it->face_id
	= face_at_string_position (it->w, face_string, face_string_pos,
				   0, it->region_beg_charpos,
				   it->region_end_charpos,
				   &endptr, it->base_face_id, 0);
      face = FACE_FROM_ID (it->f, it->face_id);
      it->face_box_p = face->box != FACE_NO_BOX;
    }

  /* Set max_x to the maximum allowed X position.  Don't let it go
     beyond the right edge of the window.  */
  if (max_x <= 0)
    max_x = it->last_visible_x;
  else
    max_x = min (max_x, it->last_visible_x);

  /* Skip over display elements that are not visible. because IT->w is
     hscrolled.  */
  if (it->current_x < it->first_visible_x)
    move_it_in_display_line_to (it, 100000, it->first_visible_x,
				MOVE_TO_POS | MOVE_TO_X);

  row->ascent = it->max_ascent;
  row->height = it->max_ascent + it->max_descent;
  row->phys_ascent = it->max_phys_ascent;
  row->phys_height = it->max_phys_ascent + it->max_phys_descent;
  row->extra_line_spacing = it->max_extra_line_spacing;

  if (STRINGP (it->string))
    it_charpos = IT_STRING_CHARPOS (*it);
  else
    it_charpos = IT_CHARPOS (*it);

  /* This condition is for the case that we are called with current_x
     past last_visible_x.  */
  while (it->current_x < max_x)
    {
      int x_before, x, n_glyphs_before, i, nglyphs;

      /* Get the next display element.  */
      if (!get_next_display_element (it))
	break;

      /* Produce glyphs.  */
      x_before = it->current_x;
      n_glyphs_before = row->used[TEXT_AREA];
      PRODUCE_GLYPHS (it);

      nglyphs = row->used[TEXT_AREA] - n_glyphs_before;
      i = 0;
      x = x_before;
      while (i < nglyphs)
	{
	  struct glyph *glyph = row->glyphs[TEXT_AREA] + n_glyphs_before + i;

	  if (it->line_wrap != TRUNCATE
	      && x + glyph->pixel_width > max_x)
	    {
	      /* End of continued line or max_x reached.  */
	      if (CHAR_GLYPH_PADDING_P (*glyph))
		{
		  /* A wide character is unbreakable.  */
		  if (row->reversed_p)
		    unproduce_glyphs (it, row->used[TEXT_AREA]
				      - n_glyphs_before);
		  row->used[TEXT_AREA] = n_glyphs_before;
		  it->current_x = x_before;
		}
	      else
		{
		  if (row->reversed_p)
		    unproduce_glyphs (it, row->used[TEXT_AREA]
				      - (n_glyphs_before + i));
		  row->used[TEXT_AREA] = n_glyphs_before + i;
		  it->current_x = x;
		}
	      break;
	    }
	  else if (x + glyph->pixel_width >= it->first_visible_x)
	    {
	      /* Glyph is at least partially visible.  */
	      ++it->hpos;
	      if (x < it->first_visible_x)
		row->x = x - it->first_visible_x;
	    }
	  else
	    {
	      /* Glyph is off the left margin of the display area.
		 Should not happen.  */
	      abort ();
	    }

	  row->ascent = max (row->ascent, it->max_ascent);
	  row->height = max (row->height, it->max_ascent + it->max_descent);
	  row->phys_ascent = max (row->phys_ascent, it->max_phys_ascent);
	  row->phys_height = max (row->phys_height,
				  it->max_phys_ascent + it->max_phys_descent);
	  row->extra_line_spacing = max (row->extra_line_spacing,
					 it->max_extra_line_spacing);
	  x += glyph->pixel_width;
	  ++i;
	}

      /* Stop if max_x reached.  */
      if (i < nglyphs)
	break;

      /* Stop at line ends.  */
      if (ITERATOR_AT_END_OF_LINE_P (it))
	{
	  it->continuation_lines_width = 0;
	  break;
	}

      set_iterator_to_next (it, 1);
      if (STRINGP (it->string))
	it_charpos = IT_STRING_CHARPOS (*it);
      else
	it_charpos = IT_CHARPOS (*it);

      /* Stop if truncating at the right edge.  */
      if (it->line_wrap == TRUNCATE
	  && it->current_x >= it->last_visible_x)
	{
	  /* Add truncation mark, but don't do it if the line is
	     truncated at a padding space.  */
	  if (it_charpos < it->string_nchars)
	    {
	      if (!FRAME_WINDOW_P (it->f))
		{
		  int ii, n;

		  if (it->current_x > it->last_visible_x)
		    {
		      if (!row->reversed_p)
			{
			  for (ii = row->used[TEXT_AREA] - 1; ii > 0; --ii)
			    if (!CHAR_GLYPH_PADDING_P (row->glyphs[TEXT_AREA][ii]))
			      break;
			}
		      else
			{
			  for (ii = 0; ii < row->used[TEXT_AREA]; ii++)
			    if (!CHAR_GLYPH_PADDING_P (row->glyphs[TEXT_AREA][ii]))
			      break;
			  unproduce_glyphs (it, ii + 1);
			  ii = row->used[TEXT_AREA] - (ii + 1);
			}
		      for (n = row->used[TEXT_AREA]; ii < n; ++ii)
			{
			  row->used[TEXT_AREA] = ii;
			  produce_special_glyphs (it, IT_TRUNCATION);
			}
		    }
		  produce_special_glyphs (it, IT_TRUNCATION);
		}
	      row->truncated_on_right_p = 1;
	    }
	  break;
	}
    }

  /* Maybe insert a truncation at the left.  */
  if (it->first_visible_x
      && it_charpos > 0)
    {
      if (!FRAME_WINDOW_P (it->f))
	insert_left_trunc_glyphs (it);
      row->truncated_on_left_p = 1;
    }

  it->face_id = saved_face_id;

  /* Value is number of columns displayed.  */
  return it->hpos - hpos_at_start;
}



/* This is like a combination of memq and assq.  Return 1/2 if PROPVAL
   appears as an element of LIST or as the car of an element of LIST.
   If PROPVAL is a list, compare each element against LIST in that
   way, and return 1/2 if any element of PROPVAL is found in LIST.
   Otherwise return 0.  This function cannot quit.
   The return value is 2 if the text is invisible but with an ellipsis
   and 1 if it's invisible and without an ellipsis.  */

int
invisible_p (register Lisp_Object propval, Lisp_Object list)
{
  register Lisp_Object tail, proptail;

  for (tail = list; CONSP (tail); tail = XCDR (tail))
    {
      register Lisp_Object tem;
      tem = XCAR (tail);
      if (EQ (propval, tem))
	return 1;
      if (CONSP (tem) && EQ (propval, XCAR (tem)))
	return NILP (XCDR (tem)) ? 1 : 2;
    }

  if (CONSP (propval))
    {
      for (proptail = propval; CONSP (proptail); proptail = XCDR (proptail))
	{
	  Lisp_Object propelt;
	  propelt = XCAR (proptail);
	  for (tail = list; CONSP (tail); tail = XCDR (tail))
	    {
	      register Lisp_Object tem;
	      tem = XCAR (tail);
	      if (EQ (propelt, tem))
		return 1;
	      if (CONSP (tem) && EQ (propelt, XCAR (tem)))
		return NILP (XCDR (tem)) ? 1 : 2;
	    }
	}
    }

  return 0;
}

DEFUN ("invisible-p", Finvisible_p, Sinvisible_p, 1, 1, 0,
       doc: /* Non-nil if the property makes the text invisible.
POS-OR-PROP can be a marker or number, in which case it is taken to be
a position in the current buffer and the value of the `invisible' property
is checked; or it can be some other value, which is then presumed to be the
value of the `invisible' property of the text of interest.
The non-nil value returned can be t for truly invisible text or something
else if the text is replaced by an ellipsis.  */)
  (Lisp_Object pos_or_prop)
{
  Lisp_Object prop
    = (NATNUMP (pos_or_prop) || MARKERP (pos_or_prop)
       ? Fget_char_property (pos_or_prop, Qinvisible, Qnil)
       : pos_or_prop);
  int invis = TEXT_PROP_MEANS_INVISIBLE (prop);
  return (invis == 0 ? Qnil
	  : invis == 1 ? Qt
	  : make_number (invis));
}

/* Calculate a width or height in pixels from a specification using
   the following elements:

   SPEC ::=
     NUM      - a (fractional) multiple of the default font width/height
     (NUM)    - specifies exactly NUM pixels
     UNIT     - a fixed number of pixels, see below.
     ELEMENT  - size of a display element in pixels, see below.
     (NUM . SPEC) - equals NUM * SPEC
     (+ SPEC SPEC ...)  - add pixel values
     (- SPEC SPEC ...)  - subtract pixel values
     (- SPEC)           - negate pixel value

   NUM ::=
     INT or FLOAT   - a number constant
     SYMBOL         - use symbol's (buffer local) variable binding.

   UNIT ::=
     in       - pixels per inch  *)
     mm       - pixels per 1/1000 meter  *)
     cm       - pixels per 1/100 meter   *)
     width    - width of current font in pixels.
     height   - height of current font in pixels.

     *) using the ratio(s) defined in display-pixels-per-inch.

   ELEMENT ::=

     left-fringe          - left fringe width in pixels
     right-fringe         - right fringe width in pixels

     left-margin          - left margin width in pixels
     right-margin         - right margin width in pixels

     scroll-bar           - scroll-bar area width in pixels

   Examples:

   Pixels corresponding to 5 inches:
     (5 . in)

   Total width of non-text areas on left side of window (if scroll-bar is on left):
     '(space :width (+ left-fringe left-margin scroll-bar))

   Align to first text column (in header line):
     '(space :align-to 0)

   Align to middle of text area minus half the width of variable `my-image'
   containing a loaded image:
     '(space :align-to (0.5 . (- text my-image)))

   Width of left margin minus width of 1 character in the default font:
     '(space :width (- left-margin 1))

   Width of left margin minus width of 2 characters in the current font:
     '(space :width (- left-margin (2 . width)))

   Center 1 character over left-margin (in header line):
     '(space :align-to (+ left-margin (0.5 . left-margin) -0.5))

   Different ways to express width of left fringe plus left margin minus one pixel:
     '(space :width (- (+ left-fringe left-margin) (1)))
     '(space :width (+ left-fringe left-margin (- (1))))
     '(space :width (+ left-fringe left-margin (-1)))

*/

#define NUMVAL(X)				\
     ((INTEGERP (X) || FLOATP (X))		\
      ? XFLOATINT (X)				\
      : - 1)

static int
calc_pixel_width_or_height (double *res, struct it *it, Lisp_Object prop,
			    struct font *font, int width_p, int *align_to)
{
  double pixels;

#define OK_PIXELS(val) ((*res = (double)(val)), 1)
#define OK_ALIGN_TO(val) ((*align_to = (int)(val)), 1)

  if (NILP (prop))
    return OK_PIXELS (0);

  xassert (FRAME_LIVE_P (it->f));

  if (SYMBOLP (prop))
    {
      if (SCHARS (SYMBOL_NAME (prop)) == 2)
	{
	  char *unit = SSDATA (SYMBOL_NAME (prop));

	  if (unit[0] == 'i' && unit[1] == 'n')
	    pixels = 1.0;
	  else if (unit[0] == 'm' && unit[1] == 'm')
	    pixels = 25.4;
	  else if (unit[0] == 'c' && unit[1] == 'm')
	    pixels = 2.54;
	  else
	    pixels = 0;
	  if (pixels > 0)
	    {
	      double ppi;
#ifdef HAVE_WINDOW_SYSTEM
	      if (FRAME_WINDOW_P (it->f)
		  && (ppi = (width_p
			     ? FRAME_X_DISPLAY_INFO (it->f)->resx
			     : FRAME_X_DISPLAY_INFO (it->f)->resy),
		      ppi > 0))
		return OK_PIXELS (ppi / pixels);
#endif

	      if ((ppi = NUMVAL (Vdisplay_pixels_per_inch), ppi > 0)
		  || (CONSP (Vdisplay_pixels_per_inch)
		      && (ppi = (width_p
				 ? NUMVAL (XCAR (Vdisplay_pixels_per_inch))
				 : NUMVAL (XCDR (Vdisplay_pixels_per_inch))),
			  ppi > 0)))
		return OK_PIXELS (ppi / pixels);

	      return 0;
	    }
	}

#ifdef HAVE_WINDOW_SYSTEM
      if (EQ (prop, Qheight))
	return OK_PIXELS (font ? FONT_HEIGHT (font) : FRAME_LINE_HEIGHT (it->f));
      if (EQ (prop, Qwidth))
	return OK_PIXELS (font ? FONT_WIDTH (font) : FRAME_COLUMN_WIDTH (it->f));
#else
      if (EQ (prop, Qheight) || EQ (prop, Qwidth))
	return OK_PIXELS (1);
#endif

      if (EQ (prop, Qtext))
	  return OK_PIXELS (width_p
			    ? window_box_width (it->w, TEXT_AREA)
			    : WINDOW_BOX_HEIGHT_NO_MODE_LINE (it->w));

      if (align_to && *align_to < 0)
	{
	  *res = 0;
	  if (EQ (prop, Qleft))
	    return OK_ALIGN_TO (window_box_left_offset (it->w, TEXT_AREA));
	  if (EQ (prop, Qright))
	    return OK_ALIGN_TO (window_box_right_offset (it->w, TEXT_AREA));
	  if (EQ (prop, Qcenter))
	    return OK_ALIGN_TO (window_box_left_offset (it->w, TEXT_AREA)
				+ window_box_width (it->w, TEXT_AREA) / 2);
	  if (EQ (prop, Qleft_fringe))
	    return OK_ALIGN_TO (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (it->w)
				? WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH (it->w)
				: window_box_right_offset (it->w, LEFT_MARGIN_AREA));
	  if (EQ (prop, Qright_fringe))
	    return OK_ALIGN_TO (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (it->w)
				? window_box_right_offset (it->w, RIGHT_MARGIN_AREA)
				: window_box_right_offset (it->w, TEXT_AREA));
	  if (EQ (prop, Qleft_margin))
	    return OK_ALIGN_TO (window_box_left_offset (it->w, LEFT_MARGIN_AREA));
	  if (EQ (prop, Qright_margin))
	    return OK_ALIGN_TO (window_box_left_offset (it->w, RIGHT_MARGIN_AREA));
	  if (EQ (prop, Qscroll_bar))
	    return OK_ALIGN_TO (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (it->w)
				? 0
				: (window_box_right_offset (it->w, RIGHT_MARGIN_AREA)
				   + (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (it->w)
				      ? WINDOW_RIGHT_FRINGE_WIDTH (it->w)
				      : 0)));
	}
      else
	{
	  if (EQ (prop, Qleft_fringe))
	    return OK_PIXELS (WINDOW_LEFT_FRINGE_WIDTH (it->w));
	  if (EQ (prop, Qright_fringe))
	    return OK_PIXELS (WINDOW_RIGHT_FRINGE_WIDTH (it->w));
	  if (EQ (prop, Qleft_margin))
	    return OK_PIXELS (WINDOW_LEFT_MARGIN_WIDTH (it->w));
	  if (EQ (prop, Qright_margin))
	    return OK_PIXELS (WINDOW_RIGHT_MARGIN_WIDTH (it->w));
	  if (EQ (prop, Qscroll_bar))
	    return OK_PIXELS (WINDOW_SCROLL_BAR_AREA_WIDTH (it->w));
	}

      prop = Fbuffer_local_value (prop, it->w->buffer);
    }

  if (INTEGERP (prop) || FLOATP (prop))
    {
      int base_unit = (width_p
		       ? FRAME_COLUMN_WIDTH (it->f)
		       : FRAME_LINE_HEIGHT (it->f));
      return OK_PIXELS (XFLOATINT (prop) * base_unit);
    }

  if (CONSP (prop))
    {
      Lisp_Object car = XCAR (prop);
      Lisp_Object cdr = XCDR (prop);

      if (SYMBOLP (car))
	{
#ifdef HAVE_WINDOW_SYSTEM
	  if (FRAME_WINDOW_P (it->f)
	      && valid_image_p (prop))
	    {
	      ptrdiff_t id = lookup_image (it->f, prop);
	      struct image *img = IMAGE_FROM_ID (it->f, id);

	      return OK_PIXELS (width_p ? img->width : img->height);
	    }
#endif
	  if (EQ (car, Qplus) || EQ (car, Qminus))
	    {
	      int first = 1;
	      double px;

	      pixels = 0;
	      while (CONSP (cdr))
		{
		  if (!calc_pixel_width_or_height (&px, it, XCAR (cdr),
						   font, width_p, align_to))
		    return 0;
		  if (first)
		    pixels = (EQ (car, Qplus) ? px : -px), first = 0;
		  else
		    pixels += px;
		  cdr = XCDR (cdr);
		}
	      if (EQ (car, Qminus))
		pixels = -pixels;
	      return OK_PIXELS (pixels);
	    }

	  car = Fbuffer_local_value (car, it->w->buffer);
	}

      if (INTEGERP (car) || FLOATP (car))
	{
	  double fact;
	  pixels = XFLOATINT (car);
	  if (NILP (cdr))
	    return OK_PIXELS (pixels);
	  if (calc_pixel_width_or_height (&fact, it, cdr,
					  font, width_p, align_to))
	    return OK_PIXELS (pixels * fact);
	  return 0;
	}

      return 0;
    }

  return 0;
}


/***********************************************************************
			     Glyph Display
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

#if GLYPH_DEBUG

void
dump_glyph_string (struct glyph_string *s)
{
  fprintf (stderr, "glyph string\n");
  fprintf (stderr, "  x, y, w, h = %d, %d, %d, %d\n",
	   s->x, s->y, s->width, s->height);
  fprintf (stderr, "  ybase = %d\n", s->ybase);
  fprintf (stderr, "  hl = %d\n", s->hl);
  fprintf (stderr, "  left overhang = %d, right = %d\n",
	   s->left_overhang, s->right_overhang);
  fprintf (stderr, "  nchars = %d\n", s->nchars);
  fprintf (stderr, "  extends to end of line = %d\n",
	   s->extends_to_end_of_line_p);
  fprintf (stderr, "  font height = %d\n", FONT_HEIGHT (s->font));
  fprintf (stderr, "  bg width = %d\n", s->background_width);
}

#endif /* GLYPH_DEBUG */

/* Initialize glyph string S.  CHAR2B is a suitably allocated vector
   of XChar2b structures for S; it can't be allocated in
   init_glyph_string because it must be allocated via `alloca'.  W
   is the window on which S is drawn.  ROW and AREA are the glyph row
   and area within the row from which S is constructed.  START is the
   index of the first glyph structure covered by S.  HL is a
   face-override for drawing S.  */

#ifdef HAVE_NTGUI
#define OPTIONAL_HDC(hdc)  HDC hdc,
#define DECLARE_HDC(hdc)   HDC hdc;
#define ALLOCATE_HDC(hdc, f) hdc = get_frame_dc ((f))
#define RELEASE_HDC(hdc, f)  release_frame_dc ((f), (hdc))
#endif

#ifndef OPTIONAL_HDC
#define OPTIONAL_HDC(hdc)
#define DECLARE_HDC(hdc)
#define ALLOCATE_HDC(hdc, f)
#define RELEASE_HDC(hdc, f)
#endif

static void
init_glyph_string (struct glyph_string *s,
		   OPTIONAL_HDC (hdc)
		   XChar2b *char2b, struct window *w, struct glyph_row *row,
		   enum glyph_row_area area, int start, enum draw_glyphs_face hl)
{
  memset (s, 0, sizeof *s);
  s->w = w;
  s->f = XFRAME (w->frame);
#ifdef HAVE_NTGUI
  s->hdc = hdc;
#endif
  s->display = FRAME_X_DISPLAY (s->f);
  s->window = FRAME_X_WINDOW (s->f);
  s->char2b = char2b;
  s->hl = hl;
  s->row = row;
  s->area = area;
  s->first_glyph = row->glyphs[area] + start;
  s->height = row->height;
  s->y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);
  s->ybase = s->y + row->ascent;
}


/* Append the list of glyph strings with head H and tail T to the list
   with head *HEAD and tail *TAIL.  Set *HEAD and *TAIL to the result.  */

static inline void
append_glyph_string_lists (struct glyph_string **head, struct glyph_string **tail,
			   struct glyph_string *h, struct glyph_string *t)
{
  if (h)
    {
      if (*head)
	(*tail)->next = h;
      else
	*head = h;
      h->prev = *tail;
      *tail = t;
    }
}


/* Prepend the list of glyph strings with head H and tail T to the
   list with head *HEAD and tail *TAIL.  Set *HEAD and *TAIL to the
   result.  */

static inline void
prepend_glyph_string_lists (struct glyph_string **head, struct glyph_string **tail,
			    struct glyph_string *h, struct glyph_string *t)
{
  if (h)
    {
      if (*head)
	(*head)->prev = t;
      else
	*tail = t;
      t->next = *head;
      *head = h;
    }
}


/* Append glyph string S to the list with head *HEAD and tail *TAIL.
   Set *HEAD and *TAIL to the resulting list.  */

static inline void
append_glyph_string (struct glyph_string **head, struct glyph_string **tail,
		     struct glyph_string *s)
{
  s->next = s->prev = NULL;
  append_glyph_string_lists (head, tail, s, s);
}


/* Get face and two-byte form of character C in face FACE_ID on frame F.
   The encoding of C is returned in *CHAR2B.  DISPLAY_P non-zero means
   make sure that X resources for the face returned are allocated.
   Value is a pointer to a realized face that is ready for display if
   DISPLAY_P is non-zero.  */

static inline struct face *
get_char_face_and_encoding (struct frame *f, int c, int face_id,
			    XChar2b *char2b, int display_p)
{
  struct face *face = FACE_FROM_ID (f, face_id);

  if (face->font)
    {
      unsigned code = face->font->driver->encode_char (face->font, c);

      if (code != FONT_INVALID_CODE)
	STORE_XCHAR2B (char2b, (code >> 8), (code & 0xFF));
      else
	STORE_XCHAR2B (char2b, 0, 0);
    }

  /* Make sure X resources of the face are allocated.  */
#ifdef HAVE_X_WINDOWS
  if (display_p)
#endif
    {
      xassert (face != NULL);
      PREPARE_FACE_FOR_DISPLAY (f, face);
    }

  return face;
}


/* Get face and two-byte form of character glyph GLYPH on frame F.
   The encoding of GLYPH->u.ch is returned in *CHAR2B.  Value is
   a pointer to a realized face that is ready for display.  */

static inline struct face *
get_glyph_face_and_encoding (struct frame *f, struct glyph *glyph,
			     XChar2b *char2b, int *two_byte_p)
{
  struct face *face;

  xassert (glyph->type == CHAR_GLYPH);
  face = FACE_FROM_ID (f, glyph->face_id);

  if (two_byte_p)
    *two_byte_p = 0;

  if (face->font)
    {
      unsigned code;

      if (CHAR_BYTE8_P (glyph->u.ch))
	code = CHAR_TO_BYTE8 (glyph->u.ch);
      else
	code = face->font->driver->encode_char (face->font, glyph->u.ch);

      if (code != FONT_INVALID_CODE)
	STORE_XCHAR2B (char2b, (code >> 8), (code & 0xFF));
      else
	STORE_XCHAR2B (char2b, 0, 0);
    }

  /* Make sure X resources of the face are allocated.  */
  xassert (face != NULL);
  PREPARE_FACE_FOR_DISPLAY (f, face);
  return face;
}


/* Get glyph code of character C in FONT in the two-byte form CHAR2B.
   Return 1 if FONT has a glyph for C, otherwise return 0.  */

static inline int
get_char_glyph_code (int c, struct font *font, XChar2b *char2b)
{
  unsigned code;

  if (CHAR_BYTE8_P (c))
    code = CHAR_TO_BYTE8 (c);
  else
    code = font->driver->encode_char (font, c);

  if (code == FONT_INVALID_CODE)
    return 0;
  STORE_XCHAR2B (char2b, (code >> 8), (code & 0xFF));
  return 1;
}


/* Fill glyph string S with composition components specified by S->cmp.

   BASE_FACE is the base face of the composition.
   S->cmp_from is the index of the first component for S.

   OVERLAPS non-zero means S should draw the foreground only, and use
   its physical height for clipping.  See also draw_glyphs.

   Value is the index of a component not in S.  */

static int
fill_composite_glyph_string (struct glyph_string *s, struct face *base_face,
			     int overlaps)
{
  int i;
  /* For all glyphs of this composition, starting at the offset
     S->cmp_from, until we reach the end of the definition or encounter a
     glyph that requires the different face, add it to S.  */
  struct face *face;

  xassert (s);

  s->for_overlaps = overlaps;
  s->face = NULL;
  s->font = NULL;
  for (i = s->cmp_from; i < s->cmp->glyph_len; i++)
    {
      int c = COMPOSITION_GLYPH (s->cmp, i);

      /* TAB in a composition means display glyphs with padding space
	 on the left or right.  */
      if (c != '\t')
	{
	  int face_id = FACE_FOR_CHAR (s->f, base_face->ascii_face, c,
				       -1, Qnil);

	  face = get_char_face_and_encoding (s->f, c, face_id,
					     s->char2b + i, 1);
	  if (face)
	    {
	      if (! s->face)
		{
		  s->face = face;
		  s->font = s->face->font;
		}
	      else if (s->face != face)
		break;
	    }
	}
      ++s->nchars;
    }
  s->cmp_to = i;

  if (s->face == NULL)
    {
      s->face = base_face->ascii_face;
      s->font = s->face->font;
    }

  /* All glyph strings for the same composition has the same width,
     i.e. the width set for the first component of the composition.  */
  s->width = s->first_glyph->pixel_width;

  /* If the specified font could not be loaded, use the frame's
     default font, but record the fact that we couldn't load it in
     the glyph string so that we can draw rectangles for the
     characters of the glyph string.  */
  if (s->font == NULL)
    {
      s->font_not_found_p = 1;
      s->font = FRAME_FONT (s->f);
    }

  /* Adjust base line for subscript/superscript text.  */
  s->ybase += s->first_glyph->voffset;

  /* This glyph string must always be drawn with 16-bit functions.  */
  s->two_byte_p = 1;

  return s->cmp_to;
}

static int
fill_gstring_glyph_string (struct glyph_string *s, int face_id,
			   int start, int end, int overlaps)
{
  struct glyph *glyph, *last;
  Lisp_Object lgstring;
  int i;

  s->for_overlaps = overlaps;
  glyph = s->row->glyphs[s->area] + start;
  last = s->row->glyphs[s->area] + end;
  s->cmp_id = glyph->u.cmp.id;
  s->cmp_from = glyph->slice.cmp.from;
  s->cmp_to = glyph->slice.cmp.to + 1;
  s->face = FACE_FROM_ID (s->f, face_id);
  lgstring = composition_gstring_from_id (s->cmp_id);
  s->font = XFONT_OBJECT (LGSTRING_FONT (lgstring));
  glyph++;
  while (glyph < last
	 && glyph->u.cmp.automatic
	 && glyph->u.cmp.id == s->cmp_id
	 && s->cmp_to == glyph->slice.cmp.from)
    s->cmp_to = (glyph++)->slice.cmp.to + 1;

  for (i = s->cmp_from; i < s->cmp_to; i++)
    {
      Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, i);
      unsigned code = LGLYPH_CODE (lglyph);

      STORE_XCHAR2B ((s->char2b + i), code >> 8, code & 0xFF);
    }
  s->width = composition_gstring_width (lgstring, s->cmp_from, s->cmp_to, NULL);
  return glyph - s->row->glyphs[s->area];
}


/* Fill glyph string S from a sequence glyphs for glyphless characters.
   See the comment of fill_glyph_string for arguments.
   Value is the index of the first glyph not in S.  */


static int
fill_glyphless_glyph_string (struct glyph_string *s, int face_id,
			     int start, int end, int overlaps)
{
  struct glyph *glyph, *last;
  int voffset;

  xassert (s->first_glyph->type == GLYPHLESS_GLYPH);
  s->for_overlaps = overlaps;
  glyph = s->row->glyphs[s->area] + start;
  last = s->row->glyphs[s->area] + end;
  voffset = glyph->voffset;
  s->face = FACE_FROM_ID (s->f, face_id);
  s->font = s->face->font;
  s->nchars = 1;
  s->width = glyph->pixel_width;
  glyph++;
  while (glyph < last
	 && glyph->type == GLYPHLESS_GLYPH
	 && glyph->voffset == voffset
	 && glyph->face_id == face_id)
    {
      s->nchars++;
      s->width += glyph->pixel_width;
      glyph++;
    }
  s->ybase += voffset;
  return glyph - s->row->glyphs[s->area];
}


/* Fill glyph string S from a sequence of character glyphs.

   FACE_ID is the face id of the string.  START is the index of the
   first glyph to consider, END is the index of the last + 1.
   OVERLAPS non-zero means S should draw the foreground only, and use
   its physical height for clipping.  See also draw_glyphs.

   Value is the index of the first glyph not in S.  */

static int
fill_glyph_string (struct glyph_string *s, int face_id,
		   int start, int end, int overlaps)
{
  struct glyph *glyph, *last;
  int voffset;
  int glyph_not_available_p;

  xassert (s->f == XFRAME (s->w->frame));
  xassert (s->nchars == 0);
  xassert (start >= 0 && end > start);

  s->for_overlaps = overlaps;
  glyph = s->row->glyphs[s->area] + start;
  last = s->row->glyphs[s->area] + end;
  voffset = glyph->voffset;
  s->padding_p = glyph->padding_p;
  glyph_not_available_p = glyph->glyph_not_available_p;

  while (glyph < last
	 && glyph->type == CHAR_GLYPH
	 && glyph->voffset == voffset
	 /* Same face id implies same font, nowadays.  */
	 && glyph->face_id == face_id
	 && glyph->glyph_not_available_p == glyph_not_available_p)
    {
      int two_byte_p;

      s->face = get_glyph_face_and_encoding (s->f, glyph,
					       s->char2b + s->nchars,
					       &two_byte_p);
      s->two_byte_p = two_byte_p;
      ++s->nchars;
      xassert (s->nchars <= end - start);
      s->width += glyph->pixel_width;
      if (glyph++->padding_p != s->padding_p)
	break;
    }

  s->font = s->face->font;

  /* If the specified font could not be loaded, use the frame's font,
     but record the fact that we couldn't load it in
     S->font_not_found_p so that we can draw rectangles for the
     characters of the glyph string.  */
  if (s->font == NULL || glyph_not_available_p)
    {
      s->font_not_found_p = 1;
      s->font = FRAME_FONT (s->f);
    }

  /* Adjust base line for subscript/superscript text.  */
  s->ybase += voffset;

  xassert (s->face && s->face->gc);
  return glyph - s->row->glyphs[s->area];
}


/* Fill glyph string S from image glyph S->first_glyph.  */

static void
fill_image_glyph_string (struct glyph_string *s)
{
  xassert (s->first_glyph->type == IMAGE_GLYPH);
  s->img = IMAGE_FROM_ID (s->f, s->first_glyph->u.img_id);
  xassert (s->img);
  s->slice = s->first_glyph->slice.img;
  s->face = FACE_FROM_ID (s->f, s->first_glyph->face_id);
  s->font = s->face->font;
  s->width = s->first_glyph->pixel_width;

  /* Adjust base line for subscript/superscript text.  */
  s->ybase += s->first_glyph->voffset;
}


/* Fill glyph string S from a sequence of stretch glyphs.

   START is the index of the first glyph to consider,
   END is the index of the last + 1.

   Value is the index of the first glyph not in S.  */

static int
fill_stretch_glyph_string (struct glyph_string *s, int start, int end)
{
  struct glyph *glyph, *last;
  int voffset, face_id;

  xassert (s->first_glyph->type == STRETCH_GLYPH);

  glyph = s->row->glyphs[s->area] + start;
  last = s->row->glyphs[s->area] + end;
  face_id = glyph->face_id;
  s->face = FACE_FROM_ID (s->f, face_id);
  s->font = s->face->font;
  s->width = glyph->pixel_width;
  s->nchars = 1;
  voffset = glyph->voffset;

  for (++glyph;
       (glyph < last
	&& glyph->type == STRETCH_GLYPH
	&& glyph->voffset == voffset
	&& glyph->face_id == face_id);
       ++glyph)
    s->width += glyph->pixel_width;

  /* Adjust base line for subscript/superscript text.  */
  s->ybase += voffset;

  /* The case that face->gc == 0 is handled when drawing the glyph
     string by calling PREPARE_FACE_FOR_DISPLAY.  */
  xassert (s->face);
  return glyph - s->row->glyphs[s->area];
}

static struct font_metrics *
get_per_char_metric (struct font *font, XChar2b *char2b)
{
  static struct font_metrics metrics;
  unsigned code = (XCHAR2B_BYTE1 (char2b) << 8) | XCHAR2B_BYTE2 (char2b);

  if (! font || code == FONT_INVALID_CODE)
    return NULL;
  font->driver->text_extents (font, &code, 1, &metrics);
  return &metrics;
}

/* EXPORT for RIF:
   Set *LEFT and *RIGHT to the left and right overhang of GLYPH on
   frame F.  Overhangs of glyphs other than type CHAR_GLYPH are
   assumed to be zero.  */

void
x_get_glyph_overhangs (struct glyph *glyph, struct frame *f, int *left, int *right)
{
  *left = *right = 0;

  if (glyph->type == CHAR_GLYPH)
    {
      struct face *face;
      XChar2b char2b;
      struct font_metrics *pcm;

      face = get_glyph_face_and_encoding (f, glyph, &char2b, NULL);
      if (face->font && (pcm = get_per_char_metric (face->font, &char2b)))
	{
	  if (pcm->rbearing > pcm->width)
	    *right = pcm->rbearing - pcm->width;
	  if (pcm->lbearing < 0)
	    *left = -pcm->lbearing;
	}
    }
  else if (glyph->type == COMPOSITE_GLYPH)
    {
      if (! glyph->u.cmp.automatic)
	{
	  struct composition *cmp = composition_table[glyph->u.cmp.id];

	  if (cmp->rbearing > cmp->pixel_width)
	    *right = cmp->rbearing - cmp->pixel_width;
	  if (cmp->lbearing < 0)
	    *left = - cmp->lbearing;
	}
      else
	{
	  Lisp_Object gstring = composition_gstring_from_id (glyph->u.cmp.id);
	  struct font_metrics metrics;

	  composition_gstring_width (gstring, glyph->slice.cmp.from,
				     glyph->slice.cmp.to + 1, &metrics);
	  if (metrics.rbearing > metrics.width)
	    *right = metrics.rbearing - metrics.width;
	  if (metrics.lbearing < 0)
	    *left = - metrics.lbearing;
	}
    }
}


/* Return the index of the first glyph preceding glyph string S that
   is overwritten by S because of S's left overhang.  Value is -1
   if no glyphs are overwritten.  */

static int
left_overwritten (struct glyph_string *s)
{
  int k;

  if (s->left_overhang)
    {
      int x = 0, i;
      struct glyph *glyphs = s->row->glyphs[s->area];
      int first = s->first_glyph - glyphs;

      for (i = first - 1; i >= 0 && x > -s->left_overhang; --i)
	x -= glyphs[i].pixel_width;

      k = i + 1;
    }
  else
    k = -1;

  return k;
}


/* Return the index of the first glyph preceding glyph string S that
   is overwriting S because of its right overhang.  Value is -1 if no
   glyph in front of S overwrites S.  */

static int
left_overwriting (struct glyph_string *s)
{
  int i, k, x;
  struct glyph *glyphs = s->row->glyphs[s->area];
  int first = s->first_glyph - glyphs;

  k = -1;
  x = 0;
  for (i = first - 1; i >= 0; --i)
    {
      int left, right;
      x_get_glyph_overhangs (glyphs + i, s->f, &left, &right);
      if (x + right > 0)
	k = i;
      x -= glyphs[i].pixel_width;
    }

  return k;
}


/* Return the index of the last glyph following glyph string S that is
   overwritten by S because of S's right overhang.  Value is -1 if
   no such glyph is found.  */

static int
right_overwritten (struct glyph_string *s)
{
  int k = -1;

  if (s->right_overhang)
    {
      int x = 0, i;
      struct glyph *glyphs = s->row->glyphs[s->area];
      int first = (s->first_glyph - glyphs) + (s->cmp ? 1 : s->nchars);
      int end = s->row->used[s->area];

      for (i = first; i < end && s->right_overhang > x; ++i)
	x += glyphs[i].pixel_width;

      k = i;
    }

  return k;
}


/* Return the index of the last glyph following glyph string S that
   overwrites S because of its left overhang.  Value is negative
   if no such glyph is found.  */

static int
right_overwriting (struct glyph_string *s)
{
  int i, k, x;
  int end = s->row->used[s->area];
  struct glyph *glyphs = s->row->glyphs[s->area];
  int first = (s->first_glyph - glyphs) + (s->cmp ? 1 : s->nchars);

  k = -1;
  x = 0;
  for (i = first; i < end; ++i)
    {
      int left, right;
      x_get_glyph_overhangs (glyphs + i, s->f, &left, &right);
      if (x - left < 0)
	k = i;
      x += glyphs[i].pixel_width;
    }

  return k;
}


/* Set background width of glyph string S.  START is the index of the
   first glyph following S.  LAST_X is the right-most x-position + 1
   in the drawing area.  */

static inline void
set_glyph_string_background_width (struct glyph_string *s, int start, int last_x)
{
  /* If the face of this glyph string has to be drawn to the end of
     the drawing area, set S->extends_to_end_of_line_p.  */

  if (start == s->row->used[s->area]
      && s->area == TEXT_AREA
      && ((s->row->fill_line_p
	   && (s->hl == DRAW_NORMAL_TEXT
	       || s->hl == DRAW_IMAGE_RAISED
	       || s->hl == DRAW_IMAGE_SUNKEN))
	  || s->hl == DRAW_MOUSE_FACE))
    s->extends_to_end_of_line_p = 1;

  /* If S extends its face to the end of the line, set its
     background_width to the distance to the right edge of the drawing
     area.  */
  if (s->extends_to_end_of_line_p)
    s->background_width = last_x - s->x + 1;
  else
    s->background_width = s->width;
}


/* Compute overhangs and x-positions for glyph string S and its
   predecessors, or successors.  X is the starting x-position for S.
   BACKWARD_P non-zero means process predecessors.  */

static void
compute_overhangs_and_x (struct glyph_string *s, int x, int backward_p)
{
  if (backward_p)
    {
      while (s)
	{
	  if (FRAME_RIF (s->f)->compute_glyph_string_overhangs)
	    FRAME_RIF (s->f)->compute_glyph_string_overhangs (s);
	  x -= s->width;
	  s->x = x;
	  s = s->prev;
	}
    }
  else
    {
      while (s)
	{
	  if (FRAME_RIF (s->f)->compute_glyph_string_overhangs)
	    FRAME_RIF (s->f)->compute_glyph_string_overhangs (s);
	  s->x = x;
	  x += s->width;
	  s = s->next;
	}
    }
}



/* The following macros are only called from draw_glyphs below.
   They reference the following parameters of that function directly:
     `w', `row', `area', and `overlap_p'
   as well as the following local variables:
     `s', `f', and `hdc' (in W32)  */

#ifdef HAVE_NTGUI
/* On W32, silently add local `hdc' variable to argument list of
   init_glyph_string.  */
#define INIT_GLYPH_STRING(s, char2b, w, row, area, start, hl) \
  init_glyph_string (s, hdc, char2b, w, row, area, start, hl)
#else
#define INIT_GLYPH_STRING(s, char2b, w, row, area, start, hl) \
  init_glyph_string (s, char2b, w, row, area, start, hl)
#endif

/* Add a glyph string for a stretch glyph to the list of strings
   between HEAD and TAIL.  START is the index of the stretch glyph in
   row area AREA of glyph row ROW.  END is the index of the last glyph
   in that glyph row area.  X is the current output position assigned
   to the new glyph string constructed.  HL overrides that face of the
   glyph; e.g. it is DRAW_CURSOR if a cursor has to be drawn.  LAST_X
   is the right-most x-position of the drawing area.  */

/* SunOS 4 bundled cc, barfed on continuations in the arg lists here
   and below -- keep them on one line.  */
#define BUILD_STRETCH_GLYPH_STRING(START, END, HEAD, TAIL, HL, X, LAST_X)   \
     do									    \
       {								    \
	 s = (struct glyph_string *) alloca (sizeof *s);		    \
	 INIT_GLYPH_STRING (s, NULL, w, row, area, START, HL);		    \
	 START = fill_stretch_glyph_string (s, START, END);                 \
	 append_glyph_string (&HEAD, &TAIL, s);				    \
         s->x = (X);							    \
       }								    \
     while (0)


/* Add a glyph string for an image glyph to the list of strings
   between HEAD and TAIL.  START is the index of the image glyph in
   row area AREA of glyph row ROW.  END is the index of the last glyph
   in that glyph row area.  X is the current output position assigned
   to the new glyph string constructed.  HL overrides that face of the
   glyph; e.g. it is DRAW_CURSOR if a cursor has to be drawn.  LAST_X
   is the right-most x-position of the drawing area.  */

#define BUILD_IMAGE_GLYPH_STRING(START, END, HEAD, TAIL, HL, X, LAST_X) \
     do									\
       {								\
	 s = (struct glyph_string *) alloca (sizeof *s);		\
	 INIT_GLYPH_STRING (s, NULL, w, row, area, START, HL);		\
	 fill_image_glyph_string (s);					\
	 append_glyph_string (&HEAD, &TAIL, s);				\
	 ++START;							\
         s->x = (X);							\
       }								\
     while (0)


/* Add a glyph string for a sequence of character glyphs to the list
   of strings between HEAD and TAIL.  START is the index of the first
   glyph in row area AREA of glyph row ROW that is part of the new
   glyph string.  END is the index of the last glyph in that glyph row
   area.  X is the current output position assigned to the new glyph
   string constructed.  HL overrides that face of the glyph; e.g. it
   is DRAW_CURSOR if a cursor has to be drawn.  LAST_X is the
   right-most x-position of the drawing area.  */

#define BUILD_CHAR_GLYPH_STRINGS(START, END, HEAD, TAIL, HL, X, LAST_X)	   \
     do									   \
       {								   \
	 int face_id;							   \
	 XChar2b *char2b;						   \
									   \
	 face_id = (row)->glyphs[area][START].face_id;			   \
									   \
	 s = (struct glyph_string *) alloca (sizeof *s);		   \
	 char2b = (XChar2b *) alloca ((END - START) * sizeof *char2b);	   \
	 INIT_GLYPH_STRING (s, char2b, w, row, area, START, HL);	   \
	 append_glyph_string (&HEAD, &TAIL, s);				   \
	 s->x = (X);							   \
	 START = fill_glyph_string (s, face_id, START, END, overlaps);	   \
       }								   \
     while (0)


/* Add a glyph string for a composite sequence to the list of strings
   between HEAD and TAIL.  START is the index of the first glyph in
   row area AREA of glyph row ROW that is part of the new glyph
   string.  END is the index of the last glyph in that glyph row area.
   X is the current output position assigned to the new glyph string
   constructed.  HL overrides that face of the glyph; e.g. it is
   DRAW_CURSOR if a cursor has to be drawn.  LAST_X is the right-most
   x-position of the drawing area.  */

#define BUILD_COMPOSITE_GLYPH_STRING(START, END, HEAD, TAIL, HL, X, LAST_X) \
  do {									    \
    int face_id = (row)->glyphs[area][START].face_id;			    \
    struct face *base_face = FACE_FROM_ID (f, face_id);			    \
    ptrdiff_t cmp_id = (row)->glyphs[area][START].u.cmp.id;		    \
    struct composition *cmp = composition_table[cmp_id];		    \
    XChar2b *char2b;							    \
    struct glyph_string *first_s = NULL;				    \
    int n;								    \
    									    \
    char2b = (XChar2b *) alloca ((sizeof *char2b) * cmp->glyph_len);	    \
    									    \
    /* Make glyph_strings for each glyph sequence that is drawable by	    \
       the same face, and append them to HEAD/TAIL.  */			    \
    for (n = 0; n < cmp->glyph_len;)					    \
      {									    \
	s = (struct glyph_string *) alloca (sizeof *s);			    \
	INIT_GLYPH_STRING (s, char2b, w, row, area, START, HL);		    \
	append_glyph_string (&(HEAD), &(TAIL), s);			    \
	s->cmp = cmp;							    \
	s->cmp_from = n;						    \
	s->x = (X);							    \
	if (n == 0)							    \
	  first_s = s;							    \
	n = fill_composite_glyph_string (s, base_face, overlaps);	    \
      }									    \
    									    \
    ++START;								    \
    s = first_s;							    \
  } while (0)


/* Add a glyph string for a glyph-string sequence to the list of strings
   between HEAD and TAIL.  */

#define BUILD_GSTRING_GLYPH_STRING(START, END, HEAD, TAIL, HL, X, LAST_X) \
  do {									  \
    int face_id;							  \
    XChar2b *char2b;							  \
    Lisp_Object gstring;						  \
    									  \
    face_id = (row)->glyphs[area][START].face_id;			  \
    gstring = (composition_gstring_from_id				  \
	       ((row)->glyphs[area][START].u.cmp.id));			  \
    s = (struct glyph_string *) alloca (sizeof *s);			  \
    char2b = (XChar2b *) alloca ((sizeof *char2b)			  \
				 * LGSTRING_GLYPH_LEN (gstring));	  \
    INIT_GLYPH_STRING (s, char2b, w, row, area, START, HL);		  \
    append_glyph_string (&(HEAD), &(TAIL), s);				  \
    s->x = (X);								  \
    START = fill_gstring_glyph_string (s, face_id, START, END, overlaps); \
  } while (0)


/* Add a glyph string for a sequence of glyphless character's glyphs
   to the list of strings between HEAD and TAIL.  The meanings of
   arguments are the same as those of BUILD_CHAR_GLYPH_STRINGS.  */

#define BUILD_GLYPHLESS_GLYPH_STRING(START, END, HEAD, TAIL, HL, X, LAST_X) \
  do									    \
    {									    \
      int face_id;							    \
									    \
      face_id = (row)->glyphs[area][START].face_id;			    \
									    \
      s = (struct glyph_string *) alloca (sizeof *s);			    \
      INIT_GLYPH_STRING (s, NULL, w, row, area, START, HL);		    \
      append_glyph_string (&HEAD, &TAIL, s);				    \
      s->x = (X);							    \
      START = fill_glyphless_glyph_string (s, face_id, START, END,	    \
					   overlaps);			    \
    }									    \
  while (0)


/* Build a list of glyph strings between HEAD and TAIL for the glyphs
   of AREA of glyph row ROW on window W between indices START and END.
   HL overrides the face for drawing glyph strings, e.g. it is
   DRAW_CURSOR to draw a cursor.  X and LAST_X are start and end
   x-positions of the drawing area.

   This is an ugly monster macro construct because we must use alloca
   to allocate glyph strings (because draw_glyphs can be called
   asynchronously).  */

#define BUILD_GLYPH_STRINGS(START, END, HEAD, TAIL, HL, X, LAST_X)	\
  do									\
    {									\
      HEAD = TAIL = NULL;						\
      while (START < END)						\
	{								\
	  struct glyph *first_glyph = (row)->glyphs[area] + START;	\
	  switch (first_glyph->type)					\
	    {								\
	    case CHAR_GLYPH:						\
	      BUILD_CHAR_GLYPH_STRINGS (START, END, HEAD, TAIL,		\
					HL, X, LAST_X);			\
	      break;							\
									\
	    case COMPOSITE_GLYPH:					\
	      if (first_glyph->u.cmp.automatic)				\
		BUILD_GSTRING_GLYPH_STRING (START, END, HEAD, TAIL,	\
					    HL, X, LAST_X);		\
	      else							\
		BUILD_COMPOSITE_GLYPH_STRING (START, END, HEAD, TAIL,	\
					      HL, X, LAST_X);		\
	      break;							\
									\
	    case STRETCH_GLYPH:						\
	      BUILD_STRETCH_GLYPH_STRING (START, END, HEAD, TAIL,	\
					  HL, X, LAST_X);		\
	      break;							\
									\
	    case IMAGE_GLYPH:						\
	      BUILD_IMAGE_GLYPH_STRING (START, END, HEAD, TAIL,		\
					HL, X, LAST_X);			\
	      break;							\
									\
	    case GLYPHLESS_GLYPH:					\
	      BUILD_GLYPHLESS_GLYPH_STRING (START, END, HEAD, TAIL,	\
					    HL, X, LAST_X);		\
	      break;							\
									\
	    default:							\
	      abort ();							\
	    }								\
									\
	  if (s)							\
	    {								\
	      set_glyph_string_background_width (s, START, LAST_X);	\
	      (X) += s->width;						\
	    }								\
	}								\
    } while (0)


/* Draw glyphs between START and END in AREA of ROW on window W,
   starting at x-position X.  X is relative to AREA in W.  HL is a
   face-override with the following meaning:

   DRAW_NORMAL_TEXT	draw normally
   DRAW_CURSOR		draw in cursor face
   DRAW_MOUSE_FACE	draw in mouse face.
   DRAW_INVERSE_VIDEO	draw in mode line face
   DRAW_IMAGE_SUNKEN	draw an image with a sunken relief around it
   DRAW_IMAGE_RAISED	draw an image with a raised relief around it

   If OVERLAPS is non-zero, draw only the foreground of characters and
   clip to the physical height of ROW.  Non-zero value also defines
   the overlapping part to be drawn:

   OVERLAPS_PRED		overlap with preceding rows
   OVERLAPS_SUCC		overlap with succeeding rows
   OVERLAPS_BOTH		overlap with both preceding/succeeding rows
   OVERLAPS_ERASED_CURSOR	overlap with erased cursor area

   Value is the x-position reached, relative to AREA of W.  */

static int
draw_glyphs (struct window *w, int x, struct glyph_row *row,
	     enum glyph_row_area area, EMACS_INT start, EMACS_INT end,
	     enum draw_glyphs_face hl, int overlaps)
{
  struct glyph_string *head, *tail;
  struct glyph_string *s;
  struct glyph_string *clip_head = NULL, *clip_tail = NULL;
  int i, j, x_reached, last_x, area_left = 0;
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  DECLARE_HDC (hdc);

  ALLOCATE_HDC (hdc, f);

  /* Let's rather be paranoid than getting a SEGV.  */
  end = min (end, row->used[area]);
  start = max (0, start);
  start = min (end, start);

  /* Translate X to frame coordinates.  Set last_x to the right
     end of the drawing area.  */
  if (row->full_width_p)
    {
      /* X is relative to the left edge of W, without scroll bars
	 or fringes.  */
      area_left = WINDOW_LEFT_EDGE_X (w);
      last_x = WINDOW_LEFT_EDGE_X (w) + WINDOW_TOTAL_WIDTH (w);
    }
  else
    {
      area_left = window_box_left (w, area);
      last_x = area_left + window_box_width (w, area);
    }
  x += area_left;

  /* Build a doubly-linked list of glyph_string structures between
     head and tail from what we have to draw.  Note that the macro
     BUILD_GLYPH_STRINGS will modify its start parameter.  That's
     the reason we use a separate variable `i'.  */
  i = start;
  BUILD_GLYPH_STRINGS (i, end, head, tail, hl, x, last_x);
  if (tail)
    x_reached = tail->x + tail->background_width;
  else
    x_reached = x;

  /* If there are any glyphs with lbearing < 0 or rbearing > width in
     the row, redraw some glyphs in front or following the glyph
     strings built above.  */
  if (head && !overlaps && row->contains_overlapping_glyphs_p)
    {
      struct glyph_string *h, *t;
      Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
      int mouse_beg_col IF_LINT (= 0), mouse_end_col IF_LINT (= 0);
      int check_mouse_face = 0;
      int dummy_x = 0;

      /* If mouse highlighting is on, we may need to draw adjacent
	 glyphs using mouse-face highlighting.  */
      if (area == TEXT_AREA && row->mouse_face_p)
	{
	  struct glyph_row *mouse_beg_row, *mouse_end_row;

	  mouse_beg_row = MATRIX_ROW (w->current_matrix, hlinfo->mouse_face_beg_row);
	  mouse_end_row = MATRIX_ROW (w->current_matrix, hlinfo->mouse_face_end_row);

	  if (row >= mouse_beg_row && row <= mouse_end_row)
	    {
	      check_mouse_face = 1;
	      mouse_beg_col = (row == mouse_beg_row)
		? hlinfo->mouse_face_beg_col : 0;
	      mouse_end_col = (row == mouse_end_row)
		? hlinfo->mouse_face_end_col
		: row->used[TEXT_AREA];
	    }
	}

      /* Compute overhangs for all glyph strings.  */
      if (FRAME_RIF (f)->compute_glyph_string_overhangs)
	for (s = head; s; s = s->next)
	  FRAME_RIF (f)->compute_glyph_string_overhangs (s);

      /* Prepend glyph strings for glyphs in front of the first glyph
	 string that are overwritten because of the first glyph
	 string's left overhang.  The background of all strings
	 prepended must be drawn because the first glyph string
	 draws over it.  */
      i = left_overwritten (head);
      if (i >= 0)
	{
	  enum draw_glyphs_face overlap_hl;

	  /* If this row contains mouse highlighting, attempt to draw
	     the overlapped glyphs with the correct highlight.  This
	     code fails if the overlap encompasses more than one glyph
	     and mouse-highlight spans only some of these glyphs.
	     However, making it work perfectly involves a lot more
	     code, and I don't know if the pathological case occurs in
	     practice, so we'll stick to this for now.  --- cyd  */
	  if (check_mouse_face
	      && mouse_beg_col < start && mouse_end_col > i)
	    overlap_hl = DRAW_MOUSE_FACE;
	  else
	    overlap_hl = DRAW_NORMAL_TEXT;

	  j = i;
	  BUILD_GLYPH_STRINGS (j, start, h, t,
			       overlap_hl, dummy_x, last_x);
	  start = i;
	  compute_overhangs_and_x (t, head->x, 1);
	  prepend_glyph_string_lists (&head, &tail, h, t);
	  clip_head = head;
	}

      /* Prepend glyph strings for glyphs in front of the first glyph
	 string that overwrite that glyph string because of their
	 right overhang.  For these strings, only the foreground must
	 be drawn, because it draws over the glyph string at `head'.
	 The background must not be drawn because this would overwrite
	 right overhangs of preceding glyphs for which no glyph
	 strings exist.  */
      i = left_overwriting (head);
      if (i >= 0)
	{
	  enum draw_glyphs_face overlap_hl;

	  if (check_mouse_face
	      && mouse_beg_col < start && mouse_end_col > i)
	    overlap_hl = DRAW_MOUSE_FACE;
	  else
	    overlap_hl = DRAW_NORMAL_TEXT;

	  clip_head = head;
	  BUILD_GLYPH_STRINGS (i, start, h, t,
			       overlap_hl, dummy_x, last_x);
	  for (s = h; s; s = s->next)
	    s->background_filled_p = 1;
	  compute_overhangs_and_x (t, head->x, 1);
	  prepend_glyph_string_lists (&head, &tail, h, t);
	}

      /* Append glyphs strings for glyphs following the last glyph
	 string tail that are overwritten by tail.  The background of
	 these strings has to be drawn because tail's foreground draws
	 over it.  */
      i = right_overwritten (tail);
      if (i >= 0)
	{
	  enum draw_glyphs_face overlap_hl;

	  if (check_mouse_face
	      && mouse_beg_col < i && mouse_end_col > end)
	    overlap_hl = DRAW_MOUSE_FACE;
	  else
	    overlap_hl = DRAW_NORMAL_TEXT;

	  BUILD_GLYPH_STRINGS (end, i, h, t,
			       overlap_hl, x, last_x);
	  /* Because BUILD_GLYPH_STRINGS updates the first argument,
	     we don't have `end = i;' here.  */
	  compute_overhangs_and_x (h, tail->x + tail->width, 0);
	  append_glyph_string_lists (&head, &tail, h, t);
	  clip_tail = tail;
	}

      /* Append glyph strings for glyphs following the last glyph
	 string tail that overwrite tail.  The foreground of such
	 glyphs has to be drawn because it writes into the background
	 of tail.  The background must not be drawn because it could
	 paint over the foreground of following glyphs.  */
      i = right_overwriting (tail);
      if (i >= 0)
	{
	  enum draw_glyphs_face overlap_hl;
	  if (check_mouse_face
	      && mouse_beg_col < i && mouse_end_col > end)
	    overlap_hl = DRAW_MOUSE_FACE;
	  else
	    overlap_hl = DRAW_NORMAL_TEXT;

	  clip_tail = tail;
	  i++;			/* We must include the Ith glyph.  */
	  BUILD_GLYPH_STRINGS (end, i, h, t,
			       overlap_hl, x, last_x);
	  for (s = h; s; s = s->next)
	    s->background_filled_p = 1;
	  compute_overhangs_and_x (h, tail->x + tail->width, 0);
	  append_glyph_string_lists (&head, &tail, h, t);
	}
      if (clip_head || clip_tail)
	for (s = head; s; s = s->next)
	  {
	    s->clip_head = clip_head;
	    s->clip_tail = clip_tail;
	  }
    }

  /* Draw all strings.  */
  for (s = head; s; s = s->next)
    FRAME_RIF (f)->draw_glyph_string (s);

#ifndef HAVE_NS
  /* When focus a sole frame and move horizontally, this sets on_p to 0
     causing a failure to erase prev cursor position. */
  if (area == TEXT_AREA
      && !row->full_width_p
      /* When drawing overlapping rows, only the glyph strings'
	 foreground is drawn, which doesn't erase a cursor
	 completely. */
      && !overlaps)
    {
      int x0 = clip_head ? clip_head->x : (head ? head->x : x);
      int x1 = (clip_tail ? clip_tail->x + clip_tail->background_width
		: (tail ? tail->x + tail->background_width : x));
      x0 -= area_left;
      x1 -= area_left;

      notice_overwritten_cursor (w, TEXT_AREA, x0, x1,
				 row->y, MATRIX_ROW_BOTTOM_Y (row));
    }
#endif

  /* Value is the x-position up to which drawn, relative to AREA of W.
     This doesn't include parts drawn because of overhangs.  */
  if (row->full_width_p)
    x_reached = FRAME_TO_WINDOW_PIXEL_X (w, x_reached);
  else
    x_reached -= area_left;

  RELEASE_HDC (hdc, f);

  return x_reached;
}

/* Expand row matrix if too narrow.  Don't expand if area
   is not present.  */

#define IT_EXPAND_MATRIX_WIDTH(it, area)		\
  {							\
    if (!fonts_changed_p				\
	&& (it->glyph_row->glyphs[area]			\
	    < it->glyph_row->glyphs[area + 1]))		\
      {							\
	it->w->ncols_scale_factor++;			\
	fonts_changed_p = 1;				\
      }							\
  }

/* Store one glyph for IT->char_to_display in IT->glyph_row.
   Called from x_produce_glyphs when IT->glyph_row is non-null.  */

static inline void
append_glyph (struct it *it)
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;

  xassert (it->glyph_row);
  xassert (it->char_to_display != '\n' && it->char_to_display != '\t');

  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      /* If the glyph row is reversed, we need to prepend the glyph
	 rather than append it.  */
      if (it->glyph_row->reversed_p && area == TEXT_AREA)
	{
	  struct glyph *g;

	  /* Make room for the additional glyph.  */
	  for (g = glyph - 1; g >= it->glyph_row->glyphs[area]; g--)
	    g[1] = *g;
	  glyph = it->glyph_row->glyphs[area];
	}
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      if (it->pixel_width > 0)
	{
	  glyph->pixel_width = it->pixel_width;
	  glyph->padding_p = 0;
	}
      else
	{
	  /* Assure at least 1-pixel width.  Otherwise, cursor can't
	     be displayed correctly.  */
	  glyph->pixel_width = 1;
	  glyph->padding_p = 1;
	}
      glyph->ascent = it->ascent;
      glyph->descent = it->descent;
      glyph->voffset = it->voffset;
      glyph->type = CHAR_GLYPH;
      glyph->avoid_cursor_p = it->avoid_cursor_p;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = (it->phys_ascent > it->ascent
				      || it->phys_descent > it->descent);
      glyph->glyph_not_available_p = it->glyph_not_available_p;
      glyph->face_id = it->face_id;
      glyph->u.ch = it->char_to_display;
      glyph->slice.img = null_glyph_slice;
      glyph->font_type = FONT_TYPE_UNKNOWN;
      if (it->bidi_p)
	{
	  glyph->resolved_level = it->bidi_it.resolved_level;
	  if ((it->bidi_it.type & 7) != it->bidi_it.type)
	    abort ();
	  glyph->bidi_type = it->bidi_it.type;
	}
      else
	{
	  glyph->resolved_level = 0;
	  glyph->bidi_type = UNKNOWN_BT;
	}
      ++it->glyph_row->used[area];
    }
  else
    IT_EXPAND_MATRIX_WIDTH (it, area);
}

/* Store one glyph for the composition IT->cmp_it.id in
   IT->glyph_row.  Called from x_produce_glyphs when IT->glyph_row is
   non-null.  */

static inline void
append_composite_glyph (struct it *it)
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;

  xassert (it->glyph_row);

  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      /* If the glyph row is reversed, we need to prepend the glyph
	 rather than append it.  */
      if (it->glyph_row->reversed_p && it->area == TEXT_AREA)
	{
	  struct glyph *g;

	  /* Make room for the new glyph.  */
	  for (g = glyph - 1; g >= it->glyph_row->glyphs[it->area]; g--)
	    g[1] = *g;
	  glyph = it->glyph_row->glyphs[it->area];
	}
      glyph->charpos = it->cmp_it.charpos;
      glyph->object = it->object;
      glyph->pixel_width = it->pixel_width;
      glyph->ascent = it->ascent;
      glyph->descent = it->descent;
      glyph->voffset = it->voffset;
      glyph->type = COMPOSITE_GLYPH;
      if (it->cmp_it.ch < 0)
	{
	  glyph->u.cmp.automatic = 0;
	  glyph->u.cmp.id = it->cmp_it.id;
	  glyph->slice.cmp.from = glyph->slice.cmp.to = 0;
	}
      else
	{
	  glyph->u.cmp.automatic = 1;
	  glyph->u.cmp.id = it->cmp_it.id;
	  glyph->slice.cmp.from = it->cmp_it.from;
	  glyph->slice.cmp.to = it->cmp_it.to - 1;
	}
      glyph->avoid_cursor_p = it->avoid_cursor_p;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = (it->phys_ascent > it->ascent
				      || it->phys_descent > it->descent);
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = 0;
      glyph->face_id = it->face_id;
      glyph->font_type = FONT_TYPE_UNKNOWN;
      if (it->bidi_p)
	{
	  glyph->resolved_level = it->bidi_it.resolved_level;
	  if ((it->bidi_it.type & 7) != it->bidi_it.type)
	    abort ();
	  glyph->bidi_type = it->bidi_it.type;
	}
      ++it->glyph_row->used[area];
    }
  else
    IT_EXPAND_MATRIX_WIDTH (it, area);
}


/* Change IT->ascent and IT->height according to the setting of
   IT->voffset.  */

static inline void
take_vertical_position_into_account (struct it *it)
{
  if (it->voffset)
    {
      if (it->voffset < 0)
	/* Increase the ascent so that we can display the text higher
	   in the line.  */
	it->ascent -= it->voffset;
      else
	/* Increase the descent so that we can display the text lower
	   in the line.  */
	it->descent += it->voffset;
    }
}


/* Produce glyphs/get display metrics for the image IT is loaded with.
   See the description of struct display_iterator in dispextern.h for
   an overview of struct display_iterator.  */

static void
produce_image_glyph (struct it *it)
{
  struct image *img;
  struct face *face;
  int glyph_ascent, crop;
  struct glyph_slice slice;

  xassert (it->what == IT_IMAGE);

  face = FACE_FROM_ID (it->f, it->face_id);
  xassert (face);
  /* Make sure X resources of the face is loaded.  */
  PREPARE_FACE_FOR_DISPLAY (it->f, face);

  if (it->image_id < 0)
    {
      /* Fringe bitmap.  */
      it->ascent = it->phys_ascent = 0;
      it->descent = it->phys_descent = 0;
      it->pixel_width = 0;
      it->nglyphs = 0;
      return;
    }

  img = IMAGE_FROM_ID (it->f, it->image_id);
  xassert (img);
  /* Make sure X resources of the image is loaded.  */
  prepare_image_for_display (it->f, img);

  slice.x = slice.y = 0;
  slice.width = img->width;
  slice.height = img->height;

  if (INTEGERP (it->slice.x))
    slice.x = XINT (it->slice.x);
  else if (FLOATP (it->slice.x))
    slice.x = XFLOAT_DATA (it->slice.x) * img->width;

  if (INTEGERP (it->slice.y))
    slice.y = XINT (it->slice.y);
  else if (FLOATP (it->slice.y))
    slice.y = XFLOAT_DATA (it->slice.y) * img->height;

  if (INTEGERP (it->slice.width))
    slice.width = XINT (it->slice.width);
  else if (FLOATP (it->slice.width))
    slice.width = XFLOAT_DATA (it->slice.width) * img->width;

  if (INTEGERP (it->slice.height))
    slice.height = XINT (it->slice.height);
  else if (FLOATP (it->slice.height))
    slice.height = XFLOAT_DATA (it->slice.height) * img->height;

  if (slice.x >= img->width)
    slice.x = img->width;
  if (slice.y >= img->height)
    slice.y = img->height;
  if (slice.x + slice.width >= img->width)
    slice.width = img->width - slice.x;
  if (slice.y + slice.height > img->height)
    slice.height = img->height - slice.y;

  if (slice.width == 0 || slice.height == 0)
    return;

  it->ascent = it->phys_ascent = glyph_ascent = image_ascent (img, face, &slice);

  it->descent = slice.height - glyph_ascent;
  if (slice.y == 0)
    it->descent += img->vmargin;
  if (slice.y + slice.height == img->height)
    it->descent += img->vmargin;
  it->phys_descent = it->descent;

  it->pixel_width = slice.width;
  if (slice.x == 0)
    it->pixel_width += img->hmargin;
  if (slice.x + slice.width == img->width)
    it->pixel_width += img->hmargin;

  /* It's quite possible for images to have an ascent greater than
     their height, so don't get confused in that case.  */
  if (it->descent < 0)
    it->descent = 0;

  it->nglyphs = 1;

  if (face->box != FACE_NO_BOX)
    {
      if (face->box_line_width > 0)
	{
	  if (slice.y == 0)
	    it->ascent += face->box_line_width;
	  if (slice.y + slice.height == img->height)
	    it->descent += face->box_line_width;
	}

      if (it->start_of_box_run_p && slice.x == 0)
	it->pixel_width += eabs (face->box_line_width);
      if (it->end_of_box_run_p && slice.x + slice.width == img->width)
	it->pixel_width += eabs (face->box_line_width);
    }

  take_vertical_position_into_account (it);

  /* Automatically crop wide image glyphs at right edge so we can
     draw the cursor on same display row.  */
  if ((crop = it->pixel_width - (it->last_visible_x - it->current_x), crop > 0)
      && (it->hpos == 0 || it->pixel_width > it->last_visible_x / 4))
    {
      it->pixel_width -= crop;
      slice.width -= crop;
    }

  if (it->glyph_row)
    {
      struct glyph *glyph;
      enum glyph_row_area area = it->area;

      glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
      if (glyph < it->glyph_row->glyphs[area + 1])
	{
	  glyph->charpos = CHARPOS (it->position);
	  glyph->object = it->object;
	  glyph->pixel_width = it->pixel_width;
	  glyph->ascent = glyph_ascent;
	  glyph->descent = it->descent;
	  glyph->voffset = it->voffset;
	  glyph->type = IMAGE_GLYPH;
	  glyph->avoid_cursor_p = it->avoid_cursor_p;
	  glyph->multibyte_p = it->multibyte_p;
	  glyph->left_box_line_p = it->start_of_box_run_p;
	  glyph->right_box_line_p = it->end_of_box_run_p;
	  glyph->overlaps_vertically_p = 0;
          glyph->padding_p = 0;
	  glyph->glyph_not_available_p = 0;
	  glyph->face_id = it->face_id;
	  glyph->u.img_id = img->id;
	  glyph->slice.img = slice;
	  glyph->font_type = FONT_TYPE_UNKNOWN;
	  if (it->bidi_p)
	    {
	      glyph->resolved_level = it->bidi_it.resolved_level;
	      if ((it->bidi_it.type & 7) != it->bidi_it.type)
		abort ();
	      glyph->bidi_type = it->bidi_it.type;
	    }
	  ++it->glyph_row->used[area];
	}
      else
	IT_EXPAND_MATRIX_WIDTH (it, area);
    }
}


/* Append a stretch glyph to IT->glyph_row.  OBJECT is the source
   of the glyph, WIDTH and HEIGHT are the width and height of the
   stretch.  ASCENT is the ascent of the glyph (0 <= ASCENT <= HEIGHT).  */

static void
append_stretch_glyph (struct it *it, Lisp_Object object,
		      int width, int height, int ascent)
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;

  xassert (ascent >= 0 && ascent <= height);

  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      /* If the glyph row is reversed, we need to prepend the glyph
	 rather than append it.  */
      if (it->glyph_row->reversed_p && area == TEXT_AREA)
	{
	  struct glyph *g;

	  /* Make room for the additional glyph.  */
	  for (g = glyph - 1; g >= it->glyph_row->glyphs[area]; g--)
	    g[1] = *g;
	  glyph = it->glyph_row->glyphs[area];
	}
      glyph->charpos = CHARPOS (it->position);
      glyph->object = object;
      glyph->pixel_width = width;
      glyph->ascent = ascent;
      glyph->descent = height - ascent;
      glyph->voffset = it->voffset;
      glyph->type = STRETCH_GLYPH;
      glyph->avoid_cursor_p = it->avoid_cursor_p;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = 0;
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = 0;
      glyph->face_id = it->face_id;
      glyph->u.stretch.ascent = ascent;
      glyph->u.stretch.height = height;
      glyph->slice.img = null_glyph_slice;
      glyph->font_type = FONT_TYPE_UNKNOWN;
      if (it->bidi_p)
	{
	  glyph->resolved_level = it->bidi_it.resolved_level;
	  if ((it->bidi_it.type & 7) != it->bidi_it.type)
	    abort ();
	  glyph->bidi_type = it->bidi_it.type;
	}
      else
	{
	  glyph->resolved_level = 0;
	  glyph->bidi_type = UNKNOWN_BT;
	}
      ++it->glyph_row->used[area];
    }
  else
    IT_EXPAND_MATRIX_WIDTH (it, area);
}

#endif	/* HAVE_WINDOW_SYSTEM */

/* Produce a stretch glyph for iterator IT.  IT->object is the value
   of the glyph property displayed.  The value must be a list
   `(space KEYWORD VALUE ...)' with the following KEYWORD/VALUE pairs
   being recognized:

   1. `:width WIDTH' specifies that the space should be WIDTH *
   canonical char width wide.  WIDTH may be an integer or floating
   point number.

   2. `:relative-width FACTOR' specifies that the width of the stretch
   should be computed from the width of the first character having the
   `glyph' property, and should be FACTOR times that width.

   3. `:align-to HPOS' specifies that the space should be wide enough
   to reach HPOS, a value in canonical character units.

   Exactly one of the above pairs must be present.

   4. `:height HEIGHT' specifies that the height of the stretch produced
   should be HEIGHT, measured in canonical character units.

   5. `:relative-height FACTOR' specifies that the height of the
   stretch should be FACTOR times the height of the characters having
   the glyph property.

   Either none or exactly one of 4 or 5 must be present.

   6. `:ascent ASCENT'  specifies that ASCENT percent of the height
   of the stretch should be used for the ascent of the stretch.
   ASCENT must be in the range 0 <= ASCENT <= 100.  */

void
produce_stretch_glyph (struct it *it)
{
  /* (space :width WIDTH :height HEIGHT ...)  */
  Lisp_Object prop, plist;
  int width = 0, height = 0, align_to = -1;
  int zero_width_ok_p = 0;
  int ascent = 0;
  double tem;
  struct face *face = NULL;
  struct font *font = NULL;

#ifdef HAVE_WINDOW_SYSTEM
  int zero_height_ok_p = 0;

  if (FRAME_WINDOW_P (it->f))
    {
      face = FACE_FROM_ID (it->f, it->face_id);
      font = face->font ? face->font : FRAME_FONT (it->f);
      PREPARE_FACE_FOR_DISPLAY (it->f, face);
    }
#endif

  /* List should start with `space'.  */
  xassert (CONSP (it->object) && EQ (XCAR (it->object), Qspace));
  plist = XCDR (it->object);

  /* Compute the width of the stretch.  */
  if ((prop = Fplist_get (plist, QCwidth), !NILP (prop))
      && calc_pixel_width_or_height (&tem, it, prop, font, 1, 0))
    {
      /* Absolute width `:width WIDTH' specified and valid.  */
      zero_width_ok_p = 1;
      width = (int)tem;
    }
#ifdef HAVE_WINDOW_SYSTEM
  else if (FRAME_WINDOW_P (it->f)
	   && (prop = Fplist_get (plist, QCrelative_width), NUMVAL (prop) > 0))
    {
      /* Relative width `:relative-width FACTOR' specified and valid.
	 Compute the width of the characters having the `glyph'
	 property.  */
      struct it it2;
      unsigned char *p = BYTE_POS_ADDR (IT_BYTEPOS (*it));

      it2 = *it;
      if (it->multibyte_p)
	it2.c = it2.char_to_display = STRING_CHAR_AND_LENGTH (p, it2.len);
      else
	{
	  it2.c = it2.char_to_display = *p, it2.len = 1;
	  if (! ASCII_CHAR_P (it2.c))
	    it2.char_to_display = BYTE8_TO_CHAR (it2.c);
	}

      it2.glyph_row = NULL;
      it2.what = IT_CHARACTER;
      x_produce_glyphs (&it2);
      width = NUMVAL (prop) * it2.pixel_width;
    }
#endif	/* HAVE_WINDOW_SYSTEM */
  else if ((prop = Fplist_get (plist, QCalign_to), !NILP (prop))
	   && calc_pixel_width_or_height (&tem, it, prop, font, 1, &align_to))
    {
      if (it->glyph_row == NULL || !it->glyph_row->mode_line_p)
	align_to = (align_to < 0
		    ? 0
		    : align_to - window_box_left_offset (it->w, TEXT_AREA));
      else if (align_to < 0)
	align_to = window_box_left_offset (it->w, TEXT_AREA);
      width = max (0, (int)tem + align_to - it->current_x);
      zero_width_ok_p = 1;
    }
  else
    /* Nothing specified -> width defaults to canonical char width.  */
    width = FRAME_COLUMN_WIDTH (it->f);

  if (width <= 0 && (width < 0 || !zero_width_ok_p))
    width = 1;

#ifdef HAVE_WINDOW_SYSTEM
  /* Compute height.  */
  if (FRAME_WINDOW_P (it->f))
    {
      if ((prop = Fplist_get (plist, QCheight), !NILP (prop))
	  && calc_pixel_width_or_height (&tem, it, prop, font, 0, 0))
	{
	  height = (int)tem;
	  zero_height_ok_p = 1;
	}
      else if (prop = Fplist_get (plist, QCrelative_height),
	       NUMVAL (prop) > 0)
	height = FONT_HEIGHT (font) * NUMVAL (prop);
      else
	height = FONT_HEIGHT (font);

      if (height <= 0 && (height < 0 || !zero_height_ok_p))
	height = 1;

      /* Compute percentage of height used for ascent.  If
	 `:ascent ASCENT' is present and valid, use that.  Otherwise,
	 derive the ascent from the font in use.  */
      if (prop = Fplist_get (plist, QCascent),
          NUMVAL (prop) > 0 && NUMVAL (prop) <= 100)
	ascent = height * NUMVAL (prop) / 100.0;
      else if (!NILP (prop)
	       && calc_pixel_width_or_height (&tem, it, prop, font, 0, 0))
	ascent = min (max (0, (int)tem), height);
      else
	ascent = (height * FONT_BASE (font)) / FONT_HEIGHT (font);
    }
  else
#endif	/* HAVE_WINDOW_SYSTEM */
    height = 1;

  if (width > 0 && it->line_wrap != TRUNCATE
      && it->current_x + width > it->last_visible_x)
    {
      width = it->last_visible_x - it->current_x;
#ifdef HAVE_WINDOW_SYSTEM
      /* Subtract one more pixel from the stretch width, but only on
	 GUI frames, since on a TTY each glyph is one "pixel" wide.  */
      width -= FRAME_WINDOW_P (it->f);
#endif
    }

  if (width > 0 && height > 0 && it->glyph_row)
    {
      Lisp_Object o_object = it->object;
      Lisp_Object object = it->stack[it->sp - 1].string;
      int n = width;

      if (!STRINGP (object))
	object = it->w->buffer;
#ifdef HAVE_WINDOW_SYSTEM
      if (FRAME_WINDOW_P (it->f))
	append_stretch_glyph (it, object, width, height, ascent);
      else
#endif
	{
	  it->object = object;
	  it->char_to_display = ' ';
	  it->pixel_width = it->len = 1;
	  while (n--)
	    tty_append_glyph (it);
	  it->object = o_object;
	}
    }

  it->pixel_width = width;
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (it->f))
    {
      it->ascent = it->phys_ascent = ascent;
      it->descent = it->phys_descent = height - it->ascent;
      it->nglyphs = width > 0 && height > 0 ? 1 : 0;
      take_vertical_position_into_account (it);
    }
  else
#endif
    it->nglyphs = width;
}

#ifdef HAVE_WINDOW_SYSTEM

/* Calculate line-height and line-spacing properties.
   An integer value specifies explicit pixel value.
   A float value specifies relative value to current face height.
   A cons (float . face-name) specifies relative value to
   height of specified face font.

   Returns height in pixels, or nil.  */


static Lisp_Object
calc_line_height_property (struct it *it, Lisp_Object val, struct font *font,
			   int boff, int override)
{
  Lisp_Object face_name = Qnil;
  int ascent, descent, height;

  if (NILP (val) || INTEGERP (val) || (override && EQ (val, Qt)))
    return val;

  if (CONSP (val))
    {
      face_name = XCAR (val);
      val = XCDR (val);
      if (!NUMBERP (val))
	val = make_number (1);
      if (NILP (face_name))
	{
	  height = it->ascent + it->descent;
	  goto scale;
	}
    }

  if (NILP (face_name))
    {
      font = FRAME_FONT (it->f);
      boff = FRAME_BASELINE_OFFSET (it->f);
    }
  else if (EQ (face_name, Qt))
    {
      override = 0;
    }
  else
    {
      int face_id;
      struct face *face;

      face_id = lookup_named_face (it->f, face_name, 0);
      if (face_id < 0)
	return make_number (-1);

      face = FACE_FROM_ID (it->f, face_id);
      font = face->font;
      if (font == NULL)
	return make_number (-1);
      boff = font->baseline_offset;
      if (font->vertical_centering)
	boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
    }

  ascent = FONT_BASE (font) + boff;
  descent = FONT_DESCENT (font) - boff;

  if (override)
    {
      it->override_ascent = ascent;
      it->override_descent = descent;
      it->override_boff = boff;
    }

  height = ascent + descent;

 scale:
  if (FLOATP (val))
    height = (int)(XFLOAT_DATA (val) * height);
  else if (INTEGERP (val))
    height *= XINT (val);

  return make_number (height);
}


/* Append a glyph for a glyphless character to IT->glyph_row.  FACE_ID
   is a face ID to be used for the glyph.  FOR_NO_FONT is nonzero if
   and only if this is for a character for which no font was found.

   If the display method (it->glyphless_method) is
   GLYPHLESS_DISPLAY_ACRONYM or GLYPHLESS_DISPLAY_HEX_CODE, LEN is a
   length of the acronym or the hexadecimal string, UPPER_XOFF and
   UPPER_YOFF are pixel offsets for the upper part of the string,
   LOWER_XOFF and LOWER_YOFF are for the lower part.

   For the other display methods, LEN through LOWER_YOFF are zero.  */

static void
append_glyphless_glyph (struct it *it, int face_id, int for_no_font, int len,
			short upper_xoff, short upper_yoff,
			short lower_xoff, short lower_yoff)
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;

  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      /* If the glyph row is reversed, we need to prepend the glyph
	 rather than append it.  */
      if (it->glyph_row->reversed_p && area == TEXT_AREA)
	{
	  struct glyph *g;

	  /* Make room for the additional glyph.  */
	  for (g = glyph - 1; g >= it->glyph_row->glyphs[area]; g--)
	    g[1] = *g;
	  glyph = it->glyph_row->glyphs[area];
	}
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      glyph->pixel_width = it->pixel_width;
      glyph->ascent = it->ascent;
      glyph->descent = it->descent;
      glyph->voffset = it->voffset;
      glyph->type = GLYPHLESS_GLYPH;
      glyph->u.glyphless.method = it->glyphless_method;
      glyph->u.glyphless.for_no_font = for_no_font;
      glyph->u.glyphless.len = len;
      glyph->u.glyphless.ch = it->c;
      glyph->slice.glyphless.upper_xoff = upper_xoff;
      glyph->slice.glyphless.upper_yoff = upper_yoff;
      glyph->slice.glyphless.lower_xoff = lower_xoff;
      glyph->slice.glyphless.lower_yoff = lower_yoff;
      glyph->avoid_cursor_p = it->avoid_cursor_p;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = (it->phys_ascent > it->ascent
				      || it->phys_descent > it->descent);
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = 0;
      glyph->face_id = face_id;
      glyph->font_type = FONT_TYPE_UNKNOWN;
      if (it->bidi_p)
	{
	  glyph->resolved_level = it->bidi_it.resolved_level;
	  if ((it->bidi_it.type & 7) != it->bidi_it.type)
	    abort ();
	  glyph->bidi_type = it->bidi_it.type;
	}
      ++it->glyph_row->used[area];
    }
  else
    IT_EXPAND_MATRIX_WIDTH (it, area);
}


/* Produce a glyph for a glyphless character for iterator IT.
   IT->glyphless_method specifies which method to use for displaying
   the character.  See the description of enum
   glyphless_display_method in dispextern.h for the detail.

   FOR_NO_FONT is nonzero if and only if this is for a character for
   which no font was found.  ACRONYM, if non-nil, is an acronym string
   for the character.  */

static void
produce_glyphless_glyph (struct it *it, int for_no_font, Lisp_Object acronym)
{
  int face_id;
  struct face *face;
  struct font *font;
  int base_width, base_height, width, height;
  short upper_xoff, upper_yoff, lower_xoff, lower_yoff;
  int len;

  /* Get the metrics of the base font.  We always refer to the current
     ASCII face.  */
  face = FACE_FROM_ID (it->f, it->face_id)->ascii_face;
  font = face->font ? face->font : FRAME_FONT (it->f);
  it->ascent = FONT_BASE (font) + font->baseline_offset;
  it->descent = FONT_DESCENT (font) - font->baseline_offset;
  base_height = it->ascent + it->descent;
  base_width = font->average_width;

  /* Get a face ID for the glyph by utilizing a cache (the same way as
     done for `escape-glyph' in get_next_display_element).  */
  if (it->f == last_glyphless_glyph_frame
      && it->face_id == last_glyphless_glyph_face_id)
    {
      face_id = last_glyphless_glyph_merged_face_id;
    }
  else
    {
      /* Merge the `glyphless-char' face into the current face.  */
      face_id = merge_faces (it->f, Qglyphless_char, 0, it->face_id);
      last_glyphless_glyph_frame = it->f;
      last_glyphless_glyph_face_id = it->face_id;
      last_glyphless_glyph_merged_face_id = face_id;
    }

  if (it->glyphless_method == GLYPHLESS_DISPLAY_THIN_SPACE)
    {
      it->pixel_width = THIN_SPACE_WIDTH;
      len = 0;
      upper_xoff = upper_yoff = lower_xoff = lower_yoff = 0;
    }
  else if (it->glyphless_method == GLYPHLESS_DISPLAY_EMPTY_BOX)
    {
      width = CHAR_WIDTH (it->c);
      if (width == 0)
	width = 1;
      else if (width > 4)
	width = 4;
      it->pixel_width = base_width * width;
      len = 0;
      upper_xoff = upper_yoff = lower_xoff = lower_yoff = 0;
    }
  else
    {
      char buf[7];
      const char *str;
      unsigned int code[6];
      int upper_len;
      int ascent, descent;
      struct font_metrics metrics_upper, metrics_lower;

      face = FACE_FROM_ID (it->f, face_id);
      font = face->font ? face->font : FRAME_FONT (it->f);
      PREPARE_FACE_FOR_DISPLAY (it->f, face);

      if (it->glyphless_method == GLYPHLESS_DISPLAY_ACRONYM)
	{
	  if (! STRINGP (acronym) && CHAR_TABLE_P (Vglyphless_char_display))
	    acronym = CHAR_TABLE_REF (Vglyphless_char_display, it->c);
	  if (CONSP (acronym))
	    acronym = XCAR (acronym);
	  str = STRINGP (acronym) ? SSDATA (acronym) : "";
	}
      else
	{
	  xassert (it->glyphless_method == GLYPHLESS_DISPLAY_HEX_CODE);
	  sprintf (buf, "%0*X", it->c < 0x10000 ? 4 : 6, it->c);
	  str = buf;
	}
      for (len = 0; str[len] && ASCII_BYTE_P (str[len]) && len < 6; len++)
	code[len] = font->driver->encode_char (font, str[len]);
      upper_len = (len + 1) / 2;
      font->driver->text_extents (font, code, upper_len,
				  &metrics_upper);
      font->driver->text_extents (font, code + upper_len, len - upper_len,
				  &metrics_lower);



      /* +4 is for vertical bars of a box plus 1-pixel spaces at both side.  */
      width = max (metrics_upper.width, metrics_lower.width) + 4;
      upper_xoff = upper_yoff = 2; /* the typical case */
      if (base_width >= width)
	{
	  /* Align the upper to the left, the lower to the right.  */
	  it->pixel_width = base_width;
	  lower_xoff = base_width - 2 - metrics_lower.width;
	}
      else
	{
	  /* Center the shorter one.  */
	  it->pixel_width = width;
	  if (metrics_upper.width >= metrics_lower.width)
	    lower_xoff = (width - metrics_lower.width) / 2;
	  else
	    {
	      /* FIXME: This code doesn't look right.  It formerly was
		 missing the "lower_xoff = 0;", which couldn't have
		 been right since it left lower_xoff uninitialized.  */
	      lower_xoff = 0;
	      upper_xoff = (width - metrics_upper.width) / 2;
	    }
	}

      /* +5 is for horizontal bars of a box plus 1-pixel spaces at
	 top, bottom, and between upper and lower strings.  */
      height = (metrics_upper.ascent + metrics_upper.descent
		+ metrics_lower.ascent + metrics_lower.descent) + 5;
      /* Center vertically.
	 H:base_height, D:base_descent
	 h:height, ld:lower_descent, la:lower_ascent, ud:upper_descent

	 ascent = - (D - H/2 - h/2 + 1); "+ 1" for rounding up
	 descent = D - H/2 + h/2;
	 lower_yoff = descent - 2 - ld;
	 upper_yoff = lower_yoff - la - 1 - ud;  */
      ascent = - (it->descent - (base_height + height + 1) / 2);
      descent = it->descent - (base_height - height) / 2;
      lower_yoff = descent - 2 - metrics_lower.descent;
      upper_yoff = (lower_yoff - metrics_lower.ascent - 1
		    - metrics_upper.descent);
      /* Don't make the height shorter than the base height. */
      if (height > base_height)
	{
	  it->ascent = ascent;
	  it->descent = descent;
	}
    }

  it->phys_ascent = it->ascent;
  it->phys_descent = it->descent;
  if (it->glyph_row)
    append_glyphless_glyph (it, face_id, for_no_font, len,
			    upper_xoff, upper_yoff,
			    lower_xoff, lower_yoff);
  it->nglyphs = 1;
  take_vertical_position_into_account (it);
}


/* RIF:
   Produce glyphs/get display metrics for the display element IT is
   loaded with.  See the description of struct it in dispextern.h
   for an overview of struct it.  */

void
x_produce_glyphs (struct it *it)
{
  int extra_line_spacing = it->extra_line_spacing;

  it->glyph_not_available_p = 0;

  if (it->what == IT_CHARACTER)
    {
      XChar2b char2b;
      struct face *face = FACE_FROM_ID (it->f, it->face_id);
      struct font *font = face->font;
      struct font_metrics *pcm = NULL;
      int boff;			/* baseline offset */

      if (font == NULL)
	{
	  /* When no suitable font is found, display this character by
	     the method specified in the first extra slot of
	     Vglyphless_char_display.  */
	  Lisp_Object acronym = lookup_glyphless_char_display (-1, it);

	  xassert (it->what == IT_GLYPHLESS);
	  produce_glyphless_glyph (it, 1, STRINGP (acronym) ? acronym : Qnil);
	  goto done;
	}

      boff = font->baseline_offset;
      if (font->vertical_centering)
	boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;

      if (it->char_to_display != '\n' && it->char_to_display != '\t')
	{
	  int stretched_p;

	  it->nglyphs = 1;

 	  if (it->override_ascent >= 0)
 	    {
 	      it->ascent = it->override_ascent;
 	      it->descent = it->override_descent;
 	      boff = it->override_boff;
 	    }
 	  else
 	    {
 	      it->ascent = FONT_BASE (font) + boff;
 	      it->descent = FONT_DESCENT (font) - boff;
 	    }

	  if (get_char_glyph_code (it->char_to_display, font, &char2b))
	    {
	      pcm = get_per_char_metric (font, &char2b);
	      if (pcm->width == 0
		  && pcm->rbearing == 0 && pcm->lbearing == 0)
		pcm = NULL;
	    }

	  if (pcm)
	    {
	      it->phys_ascent = pcm->ascent + boff;
	      it->phys_descent = pcm->descent - boff;
	      it->pixel_width = pcm->width;
	    }
	  else
	    {
	      it->glyph_not_available_p = 1;
	      it->phys_ascent = it->ascent;
	      it->phys_descent = it->descent;
	      it->pixel_width = font->space_width;
	    }

	  if (it->constrain_row_ascent_descent_p)
	    {
	      if (it->descent > it->max_descent)
 		{
 		  it->ascent += it->descent - it->max_descent;
 		  it->descent = it->max_descent;
 		}
 	      if (it->ascent > it->max_ascent)
 		{
 		  it->descent = min (it->max_descent, it->descent + it->ascent - it->max_ascent);
 		  it->ascent = it->max_ascent;
 		}
 	      it->phys_ascent = min (it->phys_ascent, it->ascent);
 	      it->phys_descent = min (it->phys_descent, it->descent);
 	      extra_line_spacing = 0;
  	    }

	  /* If this is a space inside a region of text with
	     `space-width' property, change its width.  */
	  stretched_p = it->char_to_display == ' ' && !NILP (it->space_width);
	  if (stretched_p)
	    it->pixel_width *= XFLOATINT (it->space_width);

	  /* If face has a box, add the box thickness to the character
	     height.  If character has a box line to the left and/or
	     right, add the box line width to the character's width.  */
	  if (face->box != FACE_NO_BOX)
	    {
	      int thick = face->box_line_width;

	      if (thick > 0)
		{
		  it->ascent += thick;
		  it->descent += thick;
		}
	      else
		thick = -thick;

	      if (it->start_of_box_run_p)
		it->pixel_width += thick;
	      if (it->end_of_box_run_p)
		it->pixel_width += thick;
	    }

	  /* If face has an overline, add the height of the overline
	     (1 pixel) and a 1 pixel margin to the character height.  */
	  if (face->overline_p)
	    it->ascent += overline_margin;

	  if (it->constrain_row_ascent_descent_p)
	    {
	      if (it->ascent > it->max_ascent)
		it->ascent = it->max_ascent;
	      if (it->descent > it->max_descent)
		it->descent = it->max_descent;
	    }

	  take_vertical_position_into_account (it);

	  /* If we have to actually produce glyphs, do it.  */
	  if (it->glyph_row)
	    {
	      if (stretched_p)
		{
		  /* Translate a space with a `space-width' property
		     into a stretch glyph.  */
		  int ascent = (((it->ascent + it->descent) * FONT_BASE (font))
				/ FONT_HEIGHT (font));
		  append_stretch_glyph (it, it->object, it->pixel_width,
					it->ascent + it->descent, ascent);
		}
	      else
		append_glyph (it);

	      /* If characters with lbearing or rbearing are displayed
		 in this line, record that fact in a flag of the
		 glyph row.  This is used to optimize X output code.  */
	      if (pcm && (pcm->lbearing < 0 || pcm->rbearing > pcm->width))
		it->glyph_row->contains_overlapping_glyphs_p = 1;
	    }
	  if (! stretched_p && it->pixel_width == 0)
	    /* We assure that all visible glyphs have at least 1-pixel
	       width.  */
	    it->pixel_width = 1;
	}
      else if (it->char_to_display == '\n')
	{
	  /* A newline has no width, but we need the height of the
	     line.  But if previous part of the line sets a height,
	     don't increase that height */

	  Lisp_Object height;
	  Lisp_Object total_height = Qnil;

	  it->override_ascent = -1;
	  it->pixel_width = 0;
	  it->nglyphs = 0;

	  height = get_it_property (it, Qline_height);
	  /* Split (line-height total-height) list */
	  if (CONSP (height)
	      && CONSP (XCDR (height))
	      && NILP (XCDR (XCDR (height))))
	    {
	      total_height = XCAR (XCDR (height));
	      height = XCAR (height);
	    }
	  height = calc_line_height_property (it, height, font, boff, 1);

	  if (it->override_ascent >= 0)
	    {
	      it->ascent = it->override_ascent;
	      it->descent = it->override_descent;
	      boff = it->override_boff;
	    }
	  else
	    {
	      it->ascent = FONT_BASE (font) + boff;
	      it->descent = FONT_DESCENT (font) - boff;
	    }

	  if (EQ (height, Qt))
	    {
	      if (it->descent > it->max_descent)
		{
		  it->ascent += it->descent - it->max_descent;
		  it->descent = it->max_descent;
		}
	      if (it->ascent > it->max_ascent)
		{
		  it->descent = min (it->max_descent, it->descent + it->ascent - it->max_ascent);
		  it->ascent = it->max_ascent;
		}
	      it->phys_ascent = min (it->phys_ascent, it->ascent);
	      it->phys_descent = min (it->phys_descent, it->descent);
	      it->constrain_row_ascent_descent_p = 1;
	      extra_line_spacing = 0;
	    }
	  else
	    {
	      Lisp_Object spacing;

	      it->phys_ascent = it->ascent;
	      it->phys_descent = it->descent;

	      if ((it->max_ascent > 0 || it->max_descent > 0)
		  && face->box != FACE_NO_BOX
		  && face->box_line_width > 0)
		{
		  it->ascent += face->box_line_width;
		  it->descent += face->box_line_width;
		}
	      if (!NILP (height)
		  && XINT (height) > it->ascent + it->descent)
		it->ascent = XINT (height) - it->descent;

	      if (!NILP (total_height))
		spacing = calc_line_height_property (it, total_height, font, boff, 0);
	      else
		{
		  spacing = get_it_property (it, Qline_spacing);
		  spacing = calc_line_height_property (it, spacing, font, boff, 0);
		}
	      if (INTEGERP (spacing))
		{
		  extra_line_spacing = XINT (spacing);
		  if (!NILP (total_height))
		    extra_line_spacing -= (it->phys_ascent + it->phys_descent);
		}
	    }
	}
      else		      /* i.e. (it->char_to_display == '\t') */
	{
	  if (font->space_width > 0)
	    {
	      int tab_width = it->tab_width * font->space_width;
	      int x = it->current_x + it->continuation_lines_width;
	      int next_tab_x = ((1 + x + tab_width - 1) / tab_width) * tab_width;

	      /* If the distance from the current position to the next tab
		 stop is less than a space character width, use the
		 tab stop after that.  */
	      if (next_tab_x - x < font->space_width)
		next_tab_x += tab_width;

	      it->pixel_width = next_tab_x - x;
	      it->nglyphs = 1;
	      it->ascent = it->phys_ascent = FONT_BASE (font) + boff;
	      it->descent = it->phys_descent = FONT_DESCENT (font) - boff;

	      if (it->glyph_row)
		{
		  append_stretch_glyph (it, it->object, it->pixel_width,
					it->ascent + it->descent, it->ascent);
		}
	    }
	  else
	    {
	      it->pixel_width = 0;
	      it->nglyphs = 1;
	    }
	}
    }
  else if (it->what == IT_COMPOSITION && it->cmp_it.ch < 0)
    {
      /* A static composition.

	 Note: A composition is represented as one glyph in the
	 glyph matrix.  There are no padding glyphs.

	 Important note: pixel_width, ascent, and descent are the
	 values of what is drawn by draw_glyphs (i.e. the values of
	 the overall glyphs composed).  */
      struct face *face = FACE_FROM_ID (it->f, it->face_id);
      int boff;			/* baseline offset */
      struct composition *cmp = composition_table[it->cmp_it.id];
      int glyph_len = cmp->glyph_len;
      struct font *font = face->font;

      it->nglyphs = 1;

      /* If we have not yet calculated pixel size data of glyphs of
	 the composition for the current face font, calculate them
	 now.  Theoretically, we have to check all fonts for the
	 glyphs, but that requires much time and memory space.  So,
	 here we check only the font of the first glyph.  This may
	 lead to incorrect display, but it's very rare, and C-l
	 (recenter-top-bottom) can correct the display anyway.  */
      if (! cmp->font || cmp->font != font)
	{
	  /* Ascent and descent of the font of the first character
	     of this composition (adjusted by baseline offset).
	     Ascent and descent of overall glyphs should not be less
	     than these, respectively.  */
	  int font_ascent, font_descent, font_height;
	  /* Bounding box of the overall glyphs.  */
	  int leftmost, rightmost, lowest, highest;
	  int lbearing, rbearing;
	  int i, width, ascent, descent;
	  int left_padded = 0, right_padded = 0;
	  int c IF_LINT (= 0); /* cmp->glyph_len can't be zero; see Bug#8512 */
	  XChar2b char2b;
	  struct font_metrics *pcm;
	  int font_not_found_p;
	  EMACS_INT pos;

	  for (glyph_len = cmp->glyph_len; glyph_len > 0; glyph_len--)
	    if ((c = COMPOSITION_GLYPH (cmp, glyph_len - 1)) != '\t')
	      break;
	  if (glyph_len < cmp->glyph_len)
	    right_padded = 1;
	  for (i = 0; i < glyph_len; i++)
	    {
	      if ((c = COMPOSITION_GLYPH (cmp, i)) != '\t')
		break;
	      cmp->offsets[i * 2] = cmp->offsets[i * 2 + 1] = 0;
	    }
	  if (i > 0)
	    left_padded = 1;

	  pos = (STRINGP (it->string) ? IT_STRING_CHARPOS (*it)
		 : IT_CHARPOS (*it));
	  /* If no suitable font is found, use the default font.  */
	  font_not_found_p = font == NULL;
	  if (font_not_found_p)
	    {
	      face = face->ascii_face;
	      font = face->font;
	    }
	  boff = font->baseline_offset;
	  if (font->vertical_centering)
	    boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
	  font_ascent = FONT_BASE (font) + boff;
	  font_descent = FONT_DESCENT (font) - boff;
	  font_height = FONT_HEIGHT (font);

	  cmp->font = (void *) font;

	  pcm = NULL;
	  if (! font_not_found_p)
	    {
	      get_char_face_and_encoding (it->f, c, it->face_id,
					  &char2b, 0);
	      pcm = get_per_char_metric (font, &char2b);
	    }

	  /* Initialize the bounding box.  */
	  if (pcm)
	    {
	      width = cmp->glyph_len > 0 ? pcm->width : 0;
	      ascent = pcm->ascent;
	      descent = pcm->descent;
	      lbearing = pcm->lbearing;
	      rbearing = pcm->rbearing;
	    }
	  else
	    {
	      width = cmp->glyph_len > 0 ? font->space_width : 0;
	      ascent = FONT_BASE (font);
	      descent = FONT_DESCENT (font);
	      lbearing = 0;
	      rbearing = width;
	    }

	  rightmost = width;
	  leftmost = 0;
	  lowest = - descent + boff;
	  highest = ascent + boff;

	  if (! font_not_found_p
	      && font->default_ascent
	      && CHAR_TABLE_P (Vuse_default_ascent)
	      && !NILP (Faref (Vuse_default_ascent,
			       make_number (it->char_to_display))))
	    highest = font->default_ascent + boff;

	  /* Draw the first glyph at the normal position.  It may be
	     shifted to right later if some other glyphs are drawn
	     at the left.  */
	  cmp->offsets[i * 2] = 0;
	  cmp->offsets[i * 2 + 1] = boff;
	  cmp->lbearing = lbearing;
	  cmp->rbearing = rbearing;

	  /* Set cmp->offsets for the remaining glyphs.  */
	  for (i++; i < glyph_len; i++)
	    {
	      int left, right, btm, top;
	      int ch = COMPOSITION_GLYPH (cmp, i);
	      int face_id;
	      struct face *this_face;

	      if (ch == '\t')
		ch = ' ';
	      face_id = FACE_FOR_CHAR (it->f, face, ch, pos, it->string);
	      this_face = FACE_FROM_ID (it->f, face_id);
	      font = this_face->font;

	      if (font == NULL)
		pcm = NULL;
	      else
		{
		  get_char_face_and_encoding (it->f, ch, face_id,
					      &char2b, 0);
		  pcm = get_per_char_metric (font, &char2b);
		}
	      if (! pcm)
		cmp->offsets[i * 2] = cmp->offsets[i * 2 + 1] = 0;
	      else
		{
		  width = pcm->width;
		  ascent = pcm->ascent;
		  descent = pcm->descent;
		  lbearing = pcm->lbearing;
		  rbearing = pcm->rbearing;
		  if (cmp->method != COMPOSITION_WITH_RULE_ALTCHARS)
		    {
		      /* Relative composition with or without
			 alternate chars.  */
		      left = (leftmost + rightmost - width) / 2;
		      btm = - descent + boff;
		      if (font->relative_compose
			  && (! CHAR_TABLE_P (Vignore_relative_composition)
			      || NILP (Faref (Vignore_relative_composition,
					      make_number (ch)))))
			{

			  if (- descent >= font->relative_compose)
			    /* One extra pixel between two glyphs.  */
			    btm = highest + 1;
			  else if (ascent <= 0)
			    /* One extra pixel between two glyphs.  */
			    btm = lowest - 1 - ascent - descent;
			}
		    }
		  else
		    {
		      /* A composition rule is specified by an integer
			 value that encodes global and new reference
			 points (GREF and NREF).  GREF and NREF are
			 specified by numbers as below:

			 0---1---2 -- ascent
			 |       |
			 |       |
			 |       |
			 9--10--11 -- center
			 |       |
			 ---3---4---5--- baseline
			 |       |
			 6---7---8 -- descent
		      */
		      int rule = COMPOSITION_RULE (cmp, i);
		      int gref, nref, grefx, grefy, nrefx, nrefy, xoff, yoff;

		      COMPOSITION_DECODE_RULE (rule, gref, nref, xoff, yoff);
		      grefx = gref % 3, nrefx = nref % 3;
		      grefy = gref / 3, nrefy = nref / 3;
		      if (xoff)
			xoff = font_height * (xoff - 128) / 256;
		      if (yoff)
			yoff = font_height * (yoff - 128) / 256;

		      left = (leftmost
			      + grefx * (rightmost - leftmost) / 2
			      - nrefx * width / 2
			      + xoff);

		      btm = ((grefy == 0 ? highest
			      : grefy == 1 ? 0
			      : grefy == 2 ? lowest
			      : (highest + lowest) / 2)
			     - (nrefy == 0 ? ascent + descent
				: nrefy == 1 ? descent - boff
				: nrefy == 2 ? 0
				: (ascent + descent) / 2)
			     + yoff);
		    }

		  cmp->offsets[i * 2] = left;
		  cmp->offsets[i * 2 + 1] = btm + descent;

		  /* Update the bounding box of the overall glyphs. */
		  if (width > 0)
		    {
		      right = left + width;
		      if (left < leftmost)
			leftmost = left;
		      if (right > rightmost)
			rightmost = right;
		    }
		  top = btm + descent + ascent;
		  if (top > highest)
		    highest = top;
		  if (btm < lowest)
		    lowest = btm;

		  if (cmp->lbearing > left + lbearing)
		    cmp->lbearing = left + lbearing;
		  if (cmp->rbearing < left + rbearing)
		    cmp->rbearing = left + rbearing;
		}
	    }

	  /* If there are glyphs whose x-offsets are negative,
	     shift all glyphs to the right and make all x-offsets
	     non-negative.  */
	  if (leftmost < 0)
	    {
	      for (i = 0; i < cmp->glyph_len; i++)
		cmp->offsets[i * 2] -= leftmost;
	      rightmost -= leftmost;
	      cmp->lbearing -= leftmost;
	      cmp->rbearing -= leftmost;
	    }

	  if (left_padded && cmp->lbearing < 0)
	    {
	      for (i = 0; i < cmp->glyph_len; i++)
		cmp->offsets[i * 2] -= cmp->lbearing;
	      rightmost -= cmp->lbearing;
	      cmp->rbearing -= cmp->lbearing;
	      cmp->lbearing = 0;
	    }
	  if (right_padded && rightmost < cmp->rbearing)
	    {
	      rightmost = cmp->rbearing;
	    }

	  cmp->pixel_width = rightmost;
	  cmp->ascent = highest;
	  cmp->descent = - lowest;
	  if (cmp->ascent < font_ascent)
	    cmp->ascent = font_ascent;
	  if (cmp->descent < font_descent)
	    cmp->descent = font_descent;
	}

      if (it->glyph_row
	  && (cmp->lbearing < 0
	      || cmp->rbearing > cmp->pixel_width))
	it->glyph_row->contains_overlapping_glyphs_p = 1;

      it->pixel_width = cmp->pixel_width;
      it->ascent = it->phys_ascent = cmp->ascent;
      it->descent = it->phys_descent = cmp->descent;
      if (face->box != FACE_NO_BOX)
	{
	  int thick = face->box_line_width;

	  if (thick > 0)
	    {
	      it->ascent += thick;
	      it->descent += thick;
	    }
	  else
	    thick = - thick;

	  if (it->start_of_box_run_p)
	    it->pixel_width += thick;
	  if (it->end_of_box_run_p)
	    it->pixel_width += thick;
	}

      /* If face has an overline, add the height of the overline
	 (1 pixel) and a 1 pixel margin to the character height.  */
      if (face->overline_p)
	it->ascent += overline_margin;

      take_vertical_position_into_account (it);
      if (it->ascent < 0)
	it->ascent = 0;
      if (it->descent < 0)
	it->descent = 0;

      if (it->glyph_row && cmp->glyph_len > 0)
	append_composite_glyph (it);
    }
  else if (it->what == IT_COMPOSITION)
    {
      /* A dynamic (automatic) composition.  */
      struct face *face = FACE_FROM_ID (it->f, it->face_id);
      Lisp_Object gstring;
      struct font_metrics metrics;

      it->nglyphs = 1;

      gstring = composition_gstring_from_id (it->cmp_it.id);
      it->pixel_width
	= composition_gstring_width (gstring, it->cmp_it.from, it->cmp_it.to,
				     &metrics);
      if (it->glyph_row
	  && (metrics.lbearing < 0 || metrics.rbearing > metrics.width))
	it->glyph_row->contains_overlapping_glyphs_p = 1;
      it->ascent = it->phys_ascent = metrics.ascent;
      it->descent = it->phys_descent = metrics.descent;
      if (face->box != FACE_NO_BOX)
	{
	  int thick = face->box_line_width;

	  if (thick > 0)
	    {
	      it->ascent += thick;
	      it->descent += thick;
	    }
	  else
	    thick = - thick;

	  if (it->start_of_box_run_p)
	    it->pixel_width += thick;
	  if (it->end_of_box_run_p)
	    it->pixel_width += thick;
	}
      /* If face has an overline, add the height of the overline
	 (1 pixel) and a 1 pixel margin to the character height.  */
      if (face->overline_p)
	it->ascent += overline_margin;
      take_vertical_position_into_account (it);
      if (it->ascent < 0)
	it->ascent = 0;
      if (it->descent < 0)
	it->descent = 0;

      if (it->glyph_row)
	append_composite_glyph (it);
    }
  else if (it->what == IT_GLYPHLESS)
    produce_glyphless_glyph (it, 0, Qnil);
  else if (it->what == IT_IMAGE)
    produce_image_glyph (it);
  else if (it->what == IT_STRETCH)
    produce_stretch_glyph (it);

 done:
  /* Accumulate dimensions.  Note: can't assume that it->descent > 0
     because this isn't true for images with `:ascent 100'.  */
  xassert (it->ascent >= 0 && it->descent >= 0);
  if (it->area == TEXT_AREA)
    it->current_x += it->pixel_width;

  if (extra_line_spacing > 0)
    {
      it->descent += extra_line_spacing;
      if (extra_line_spacing > it->max_extra_line_spacing)
	it->max_extra_line_spacing = extra_line_spacing;
    }

  it->max_ascent = max (it->max_ascent, it->ascent);
  it->max_descent = max (it->max_descent, it->descent);
  it->max_phys_ascent = max (it->max_phys_ascent, it->phys_ascent);
  it->max_phys_descent = max (it->max_phys_descent, it->phys_descent);
}

/* EXPORT for RIF:
   Output LEN glyphs starting at START at the nominal cursor position.
   Advance the nominal cursor over the text.  The global variable
   updated_window contains the window being updated, updated_row is
   the glyph row being updated, and updated_area is the area of that
   row being updated.  */

void
x_write_glyphs (struct glyph *start, int len)
{
  int x, hpos, chpos = updated_window->phys_cursor.hpos;

  xassert (updated_window && updated_row);
  /* When the window is hscrolled, cursor hpos can legitimately be out
     of bounds, but we draw the cursor at the corresponding window
     margin in that case.  */
  if (!updated_row->reversed_p && chpos < 0)
    chpos = 0;
  if (updated_row->reversed_p && chpos >= updated_row->used[TEXT_AREA])
    chpos = updated_row->used[TEXT_AREA] - 1;

  BLOCK_INPUT;

  /* Write glyphs.  */

  hpos = start - updated_row->glyphs[updated_area];
  x = draw_glyphs (updated_window, output_cursor.x,
		   updated_row, updated_area,
		   hpos, hpos + len,
		   DRAW_NORMAL_TEXT, 0);

  /* Invalidate old phys cursor if the glyph at its hpos is redrawn.  */
  if (updated_area == TEXT_AREA
      && updated_window->phys_cursor_on_p
      && updated_window->phys_cursor.vpos == output_cursor.vpos
      && chpos >= hpos
      && chpos < hpos + len)
    updated_window->phys_cursor_on_p = 0;

  UNBLOCK_INPUT;

  /* Advance the output cursor.  */
  output_cursor.hpos += len;
  output_cursor.x = x;
}


/* EXPORT for RIF:
   Insert LEN glyphs from START at the nominal cursor position.  */

void
x_insert_glyphs (struct glyph *start, int len)
{
  struct frame *f;
  struct window *w;
  int line_height, shift_by_width, shifted_region_width;
  struct glyph_row *row;
  struct glyph *glyph;
  int frame_x, frame_y;
  EMACS_INT hpos;

  xassert (updated_window && updated_row);
  BLOCK_INPUT;
  w = updated_window;
  f = XFRAME (WINDOW_FRAME (w));

  /* Get the height of the line we are in.  */
  row = updated_row;
  line_height = row->height;

  /* Get the width of the glyphs to insert.  */
  shift_by_width = 0;
  for (glyph = start; glyph < start + len; ++glyph)
    shift_by_width += glyph->pixel_width;

  /* Get the width of the region to shift right.  */
  shifted_region_width = (window_box_width (w, updated_area)
			  - output_cursor.x
			  - shift_by_width);

  /* Shift right.  */
  frame_x = window_box_left (w, updated_area) + output_cursor.x;
  frame_y = WINDOW_TO_FRAME_PIXEL_Y (w, output_cursor.y);

  FRAME_RIF (f)->shift_glyphs_for_insert (f, frame_x, frame_y, shifted_region_width,
                                          line_height, shift_by_width);

  /* Write the glyphs.  */
  hpos = start - row->glyphs[updated_area];
  draw_glyphs (w, output_cursor.x, row, updated_area,
	       hpos, hpos + len,
	       DRAW_NORMAL_TEXT, 0);

  /* Advance the output cursor.  */
  output_cursor.hpos += len;
  output_cursor.x += shift_by_width;
  UNBLOCK_INPUT;
}


/* EXPORT for RIF:
   Erase the current text line from the nominal cursor position
   (inclusive) to pixel column TO_X (exclusive).  The idea is that
   everything from TO_X onward is already erased.

   TO_X is a pixel position relative to updated_area of
   updated_window.  TO_X == -1 means clear to the end of this area.  */

void
x_clear_end_of_line (int to_x)
{
  struct frame *f;
  struct window *w = updated_window;
  int max_x, min_y, max_y;
  int from_x, from_y, to_y;

  xassert (updated_window && updated_row);
  f = XFRAME (w->frame);

  if (updated_row->full_width_p)
    max_x = WINDOW_TOTAL_WIDTH (w);
  else
    max_x = window_box_width (w, updated_area);
  max_y = window_text_bottom_y (w);

  /* TO_X == 0 means don't do anything.  TO_X < 0 means clear to end
     of window.  For TO_X > 0, truncate to end of drawing area.  */
  if (to_x == 0)
    return;
  else if (to_x < 0)
    to_x = max_x;
  else
    to_x = min (to_x, max_x);

  to_y = min (max_y, output_cursor.y + updated_row->height);

  /* Notice if the cursor will be cleared by this operation.  */
  if (!updated_row->full_width_p)
    notice_overwritten_cursor (w, updated_area,
			       output_cursor.x, -1,
			       updated_row->y,
			       MATRIX_ROW_BOTTOM_Y (updated_row));

  from_x = output_cursor.x;

  /* Translate to frame coordinates.  */
  if (updated_row->full_width_p)
    {
      from_x = WINDOW_TO_FRAME_PIXEL_X (w, from_x);
      to_x = WINDOW_TO_FRAME_PIXEL_X (w, to_x);
    }
  else
    {
      int area_left = window_box_left (w, updated_area);
      from_x += area_left;
      to_x += area_left;
    }

  min_y = WINDOW_HEADER_LINE_HEIGHT (w);
  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, max (min_y, output_cursor.y));
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, to_y);

  /* Prevent inadvertently clearing to end of the X window.  */
  if (to_x > from_x && to_y > from_y)
    {
      BLOCK_INPUT;
      FRAME_RIF (f)->clear_frame_area (f, from_x, from_y,
                                       to_x - from_x, to_y - from_y);
      UNBLOCK_INPUT;
    }
}

#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
			     Cursor types
 ***********************************************************************/

/* Value is the internal representation of the specified cursor type
   ARG.  If type is BAR_CURSOR, return in *WIDTH the specified width
   of the bar cursor.  */

static enum text_cursor_kinds
get_specified_cursor_type (Lisp_Object arg, int *width)
{
  enum text_cursor_kinds type;

  if (NILP (arg))
    return NO_CURSOR;

  if (EQ (arg, Qbox))
    return FILLED_BOX_CURSOR;

  if (EQ (arg, Qhollow))
    return HOLLOW_BOX_CURSOR;

  if (EQ (arg, Qbar))
    {
      *width = 2;
      return BAR_CURSOR;
    }

  if (CONSP (arg)
      && EQ (XCAR (arg), Qbar)
      && INTEGERP (XCDR (arg))
      && XINT (XCDR (arg)) >= 0)
    {
      *width = XINT (XCDR (arg));
      return BAR_CURSOR;
    }

  if (EQ (arg, Qhbar))
    {
      *width = 2;
      return HBAR_CURSOR;
    }

  if (CONSP (arg)
      && EQ (XCAR (arg), Qhbar)
      && INTEGERP (XCDR (arg))
      && XINT (XCDR (arg)) >= 0)
    {
      *width = XINT (XCDR (arg));
      return HBAR_CURSOR;
    }

  /* Treat anything unknown as "hollow box cursor".
     It was bad to signal an error; people have trouble fixing
     .Xdefaults with Emacs, when it has something bad in it.  */
  type = HOLLOW_BOX_CURSOR;

  return type;
}

/* Set the default cursor types for specified frame.  */
void
set_frame_cursor_types (struct frame *f, Lisp_Object arg)
{
  int width = 1;
  Lisp_Object tem;

  FRAME_DESIRED_CURSOR (f) = get_specified_cursor_type (arg, &width);
  FRAME_CURSOR_WIDTH (f) = width;

  /* By default, set up the blink-off state depending on the on-state.  */

  tem = Fassoc (arg, Vblink_cursor_alist);
  if (!NILP (tem))
    {
      FRAME_BLINK_OFF_CURSOR (f)
	= get_specified_cursor_type (XCDR (tem), &width);
      FRAME_BLINK_OFF_CURSOR_WIDTH (f) = width;
    }
  else
    FRAME_BLINK_OFF_CURSOR (f) = DEFAULT_CURSOR;
}


#ifdef HAVE_WINDOW_SYSTEM

/* Return the cursor we want to be displayed in window W.  Return
   width of bar/hbar cursor through WIDTH arg.  Return with
   ACTIVE_CURSOR arg set to 1 if cursor in window W is `active'
   (i.e. if the `system caret' should track this cursor).

   In a mini-buffer window, we want the cursor only to appear if we
   are reading input from this window.  For the selected window, we
   want the cursor type given by the frame parameter or buffer local
   setting of cursor-type.  If explicitly marked off, draw no cursor.
   In all other cases, we want a hollow box cursor.  */

static enum text_cursor_kinds
get_window_cursor_type (struct window *w, struct glyph *glyph, int *width,
			int *active_cursor)
{
  struct frame *f = XFRAME (w->frame);
  struct buffer *b = XBUFFER (w->buffer);
  int cursor_type = DEFAULT_CURSOR;
  Lisp_Object alt_cursor;
  int non_selected = 0;

  *active_cursor = 1;

  /* Echo area */
  if (cursor_in_echo_area
      && FRAME_HAS_MINIBUF_P (f)
      && EQ (FRAME_MINIBUF_WINDOW (f), echo_area_window))
    {
      if (w == XWINDOW (echo_area_window))
	{
	  if (EQ (BVAR (b, cursor_type), Qt) || NILP (BVAR (b, cursor_type)))
	    {
	      *width = FRAME_CURSOR_WIDTH (f);
	      return FRAME_DESIRED_CURSOR (f);
	    }
	  else
	    return get_specified_cursor_type (BVAR (b, cursor_type), width);
	}

      *active_cursor = 0;
      non_selected = 1;
    }

  /* Detect a nonselected window or nonselected frame.  */
  else if (w != XWINDOW (f->selected_window)
	   || f != FRAME_X_DISPLAY_INFO (f)->x_highlight_frame)
    {
      *active_cursor = 0;

      if (MINI_WINDOW_P (w) && minibuf_level == 0)
	return NO_CURSOR;

      non_selected = 1;
    }

  /* Never display a cursor in a window in which cursor-type is nil.  */
  if (NILP (BVAR (b, cursor_type)))
    return NO_CURSOR;

  /* Get the normal cursor type for this window.  */
  if (EQ (BVAR (b, cursor_type), Qt))
    {
      cursor_type = FRAME_DESIRED_CURSOR (f);
      *width = FRAME_CURSOR_WIDTH (f);
    }
  else
    cursor_type = get_specified_cursor_type (BVAR (b, cursor_type), width);

  /* Use cursor-in-non-selected-windows instead
     for non-selected window or frame.  */
  if (non_selected)
    {
      alt_cursor = BVAR (b, cursor_in_non_selected_windows);
      if (!EQ (Qt, alt_cursor))
	return get_specified_cursor_type (alt_cursor, width);
      /* t means modify the normal cursor type.  */
      if (cursor_type == FILLED_BOX_CURSOR)
	cursor_type = HOLLOW_BOX_CURSOR;
      else if (cursor_type == BAR_CURSOR && *width > 1)
	--*width;
      return cursor_type;
    }

  /* Use normal cursor if not blinked off.  */
  if (!w->cursor_off_p)
    {
      if (glyph != NULL && glyph->type == IMAGE_GLYPH)
	{
	  if (cursor_type == FILLED_BOX_CURSOR)
	    {
	      /* Using a block cursor on large images can be very annoying.
		 So use a hollow cursor for "large" images.
		 If image is not transparent (no mask), also use hollow cursor.  */
	      struct image *img = IMAGE_FROM_ID (f, glyph->u.img_id);
	      if (img != NULL && IMAGEP (img->spec))
		{
		  /* Arbitrarily, interpret "Large" as >32x32 and >NxN
		     where N = size of default frame font size.
		     This should cover most of the "tiny" icons people may use.  */
		  if (!img->mask
		      || img->width > max (32, WINDOW_FRAME_COLUMN_WIDTH (w))
		      || img->height > max (32, WINDOW_FRAME_LINE_HEIGHT (w)))
		    cursor_type = HOLLOW_BOX_CURSOR;
		}
	    }
	  else if (cursor_type != NO_CURSOR)
	    {
	      /* Display current only supports BOX and HOLLOW cursors for images.
		 So for now, unconditionally use a HOLLOW cursor when cursor is
		 not a solid box cursor.  */
	      cursor_type = HOLLOW_BOX_CURSOR;
	    }
      }
      return cursor_type;
    }

  /* Cursor is blinked off, so determine how to "toggle" it.  */

  /* First look for an entry matching the buffer's cursor-type in blink-cursor-alist.  */
  if ((alt_cursor = Fassoc (BVAR (b, cursor_type), Vblink_cursor_alist), !NILP (alt_cursor)))
    return get_specified_cursor_type (XCDR (alt_cursor), width);

  /* Then see if frame has specified a specific blink off cursor type.  */
  if (FRAME_BLINK_OFF_CURSOR (f) != DEFAULT_CURSOR)
    {
      *width = FRAME_BLINK_OFF_CURSOR_WIDTH (f);
      return FRAME_BLINK_OFF_CURSOR (f);
    }

#if 0
  /* Some people liked having a permanently visible blinking cursor,
     while others had very strong opinions against it.  So it was
     decided to remove it.  KFS 2003-09-03 */

  /* Finally perform built-in cursor blinking:
       filled box      <->   hollow box
       wide [h]bar     <->   narrow [h]bar
       narrow [h]bar   <->   no cursor
       other type      <->   no cursor  */

  if (cursor_type == FILLED_BOX_CURSOR)
    return HOLLOW_BOX_CURSOR;

  if ((cursor_type == BAR_CURSOR || cursor_type == HBAR_CURSOR) && *width > 1)
    {
      *width = 1;
      return cursor_type;
    }
#endif

  return NO_CURSOR;
}


/* Notice when the text cursor of window W has been completely
   overwritten by a drawing operation that outputs glyphs in AREA
   starting at X0 and ending at X1 in the line starting at Y0 and
   ending at Y1.  X coordinates are area-relative.  X1 < 0 means all
   the rest of the line after X0 has been written.  Y coordinates
   are window-relative.  */

static void
notice_overwritten_cursor (struct window *w, enum glyph_row_area area,
			   int x0, int x1, int y0, int y1)
{
  int cx0, cx1, cy0, cy1;
  struct glyph_row *row;

  if (!w->phys_cursor_on_p)
    return;
  if (area != TEXT_AREA)
    return;

  if (w->phys_cursor.vpos < 0
      || w->phys_cursor.vpos >= w->current_matrix->nrows
      || (row = w->current_matrix->rows + w->phys_cursor.vpos,
	  !(row->enabled_p && row->displays_text_p)))
    return;

  if (row->cursor_in_fringe_p)
    {
      row->cursor_in_fringe_p = 0;
      draw_fringe_bitmap (w, row, row->reversed_p);
      w->phys_cursor_on_p = 0;
      return;
    }

  cx0 = w->phys_cursor.x;
  cx1 = cx0 + w->phys_cursor_width;
  if (x0 > cx0 || (x1 >= 0 && x1 < cx1))
    return;

  /* The cursor image will be completely removed from the
     screen if the output area intersects the cursor area in
     y-direction.  When we draw in [y0 y1[, and some part of
     the cursor is at y < y0, that part must have been drawn
     before.  When scrolling, the cursor is erased before
     actually scrolling, so we don't come here.  When not
     scrolling, the rows above the old cursor row must have
     changed, and in this case these rows must have written
     over the cursor image.

     Likewise if part of the cursor is below y1, with the
     exception of the cursor being in the first blank row at
     the buffer and window end because update_text_area
     doesn't draw that row.  (Except when it does, but
     that's handled in update_text_area.)  */

  cy0 = w->phys_cursor.y;
  cy1 = cy0 + w->phys_cursor_height;
  if ((y0 < cy0 || y0 >= cy1) && (y1 <= cy0 || y1 >= cy1))
    return;

  w->phys_cursor_on_p = 0;
}

#endif /* HAVE_WINDOW_SYSTEM */


/************************************************************************
			      Mouse Face
 ************************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

/* EXPORT for RIF:
   Fix the display of area AREA of overlapping row ROW in window W
   with respect to the overlapping part OVERLAPS.  */

void
x_fix_overlapping_area (struct window *w, struct glyph_row *row,
			enum glyph_row_area area, int overlaps)
{
  int i, x;

  BLOCK_INPUT;

  x = 0;
  for (i = 0; i < row->used[area];)
    {
      if (row->glyphs[area][i].overlaps_vertically_p)
	{
	  int start = i, start_x = x;

	  do
	    {
	      x += row->glyphs[area][i].pixel_width;
	      ++i;
	    }
	  while (i < row->used[area]
		 && row->glyphs[area][i].overlaps_vertically_p);

	  draw_glyphs (w, start_x, row, area,
		       start, i,
		       DRAW_NORMAL_TEXT, overlaps);
	}
      else
	{
	  x += row->glyphs[area][i].pixel_width;
	  ++i;
	}
    }

  UNBLOCK_INPUT;
}


/* EXPORT:
   Draw the cursor glyph of window W in glyph row ROW.  See the
   comment of draw_glyphs for the meaning of HL.  */

void
draw_phys_cursor_glyph (struct window *w, struct glyph_row *row,
			enum draw_glyphs_face hl)
{
  /* If cursor hpos is out of bounds, don't draw garbage.  This can
     happen in mini-buffer windows when switching between echo area
     glyphs and mini-buffer.  */
  if ((row->reversed_p
       ? (w->phys_cursor.hpos >= 0)
       : (w->phys_cursor.hpos < row->used[TEXT_AREA])))
    {
      int on_p = w->phys_cursor_on_p;
      int x1;
      int hpos = w->phys_cursor.hpos;

      /* When the window is hscrolled, cursor hpos can legitimately be
	 out of bounds, but we draw the cursor at the corresponding
	 window margin in that case.  */
      if (!row->reversed_p && hpos < 0)
	hpos = 0;
      if (row->reversed_p && hpos >= row->used[TEXT_AREA])
	hpos = row->used[TEXT_AREA] - 1;

      x1 = draw_glyphs (w, w->phys_cursor.x, row, TEXT_AREA, hpos, hpos + 1,
			hl, 0);
      w->phys_cursor_on_p = on_p;

      if (hl == DRAW_CURSOR)
	w->phys_cursor_width = x1 - w->phys_cursor.x;
      /* When we erase the cursor, and ROW is overlapped by other
	 rows, make sure that these overlapping parts of other rows
	 are redrawn.  */
      else if (hl == DRAW_NORMAL_TEXT && row->overlapped_p)
	{
	  w->phys_cursor_width = x1 - w->phys_cursor.x;

	  if (row > w->current_matrix->rows
	      && MATRIX_ROW_OVERLAPS_SUCC_P (row - 1))
	    x_fix_overlapping_area (w, row - 1, TEXT_AREA,
				    OVERLAPS_ERASED_CURSOR);

	  if (MATRIX_ROW_BOTTOM_Y (row) < window_text_bottom_y (w)
	      && MATRIX_ROW_OVERLAPS_PRED_P (row + 1))
	    x_fix_overlapping_area (w, row + 1, TEXT_AREA,
				    OVERLAPS_ERASED_CURSOR);
	}
    }
}


/* EXPORT:
   Erase the image of a cursor of window W from the screen.  */

void
erase_phys_cursor (struct window *w)
{
  struct frame *f = XFRAME (w->frame);
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
  int hpos = w->phys_cursor.hpos;
  int vpos = w->phys_cursor.vpos;
  int mouse_face_here_p = 0;
  struct glyph_matrix *active_glyphs = w->current_matrix;
  struct glyph_row *cursor_row;
  struct glyph *cursor_glyph;
  enum draw_glyphs_face hl;

  /* No cursor displayed or row invalidated => nothing to do on the
     screen.  */
  if (w->phys_cursor_type == NO_CURSOR)
    goto mark_cursor_off;

  /* VPOS >= active_glyphs->nrows means that window has been resized.
     Don't bother to erase the cursor.  */
  if (vpos >= active_glyphs->nrows)
    goto mark_cursor_off;

  /* If row containing cursor is marked invalid, there is nothing we
     can do.  */
  cursor_row = MATRIX_ROW (active_glyphs, vpos);
  if (!cursor_row->enabled_p)
    goto mark_cursor_off;

  /* If line spacing is > 0, old cursor may only be partially visible in
     window after split-window.  So adjust visible height.  */
  cursor_row->visible_height = min (cursor_row->visible_height,
				    window_text_bottom_y (w) - cursor_row->y);

  /* If row is completely invisible, don't attempt to delete a cursor which
     isn't there.  This can happen if cursor is at top of a window, and
     we switch to a buffer with a header line in that window.  */
  if (cursor_row->visible_height <= 0)
    goto mark_cursor_off;

  /* If cursor is in the fringe, erase by drawing actual bitmap there.  */
  if (cursor_row->cursor_in_fringe_p)
    {
      cursor_row->cursor_in_fringe_p = 0;
      draw_fringe_bitmap (w, cursor_row, cursor_row->reversed_p);
      goto mark_cursor_off;
    }

  /* This can happen when the new row is shorter than the old one.
     In this case, either draw_glyphs or clear_end_of_line
     should have cleared the cursor.  Note that we wouldn't be
     able to erase the cursor in this case because we don't have a
     cursor glyph at hand.  */
  if ((cursor_row->reversed_p
       ? (w->phys_cursor.hpos < 0)
       : (w->phys_cursor.hpos >= cursor_row->used[TEXT_AREA])))
    goto mark_cursor_off;

  /* When the window is hscrolled, cursor hpos can legitimately be out
     of bounds, but we draw the cursor at the corresponding window
     margin in that case.  */
  if (!cursor_row->reversed_p && hpos < 0)
    hpos = 0;
  if (cursor_row->reversed_p && hpos >= cursor_row->used[TEXT_AREA])
    hpos = cursor_row->used[TEXT_AREA] - 1;

  /* If the cursor is in the mouse face area, redisplay that when
     we clear the cursor.  */
  if (! NILP (hlinfo->mouse_face_window)
      && coords_in_mouse_face_p (w, hpos, vpos)
      /* Don't redraw the cursor's spot in mouse face if it is at the
	 end of a line (on a newline).  The cursor appears there, but
	 mouse highlighting does not.  */
      && cursor_row->used[TEXT_AREA] > hpos && hpos >= 0)
    mouse_face_here_p = 1;

  /* Maybe clear the display under the cursor.  */
  if (w->phys_cursor_type == HOLLOW_BOX_CURSOR)
    {
      int x, y, left_x;
      int header_line_height = WINDOW_HEADER_LINE_HEIGHT (w);
      int width;

      cursor_glyph = get_phys_cursor_glyph (w);
      if (cursor_glyph == NULL)
	goto mark_cursor_off;

      width = cursor_glyph->pixel_width;
      left_x = window_box_left_offset (w, TEXT_AREA);
      x = w->phys_cursor.x;
      if (x < left_x)
	width -= left_x - x;
      width = min (width, window_box_width (w, TEXT_AREA) - x);
      y = WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height, cursor_row->y));
      x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, max (x, left_x));

      if (width > 0)
	FRAME_RIF (f)->clear_frame_area (f, x, y, width, cursor_row->visible_height);
    }

  /* Erase the cursor by redrawing the character underneath it.  */
  if (mouse_face_here_p)
    hl = DRAW_MOUSE_FACE;
  else
    hl = DRAW_NORMAL_TEXT;
  draw_phys_cursor_glyph (w, cursor_row, hl);

 mark_cursor_off:
  w->phys_cursor_on_p = 0;
  w->phys_cursor_type = NO_CURSOR;
}


/* EXPORT:
   Display or clear cursor of window W.  If ON is zero, clear the
   cursor.  If it is non-zero, display the cursor.  If ON is nonzero,
   where to put the cursor is specified by HPOS, VPOS, X and Y.  */

void
display_and_set_cursor (struct window *w, int on,
			int hpos, int vpos, int x, int y)
{
  struct frame *f = XFRAME (w->frame);
  int new_cursor_type;
  int new_cursor_width;
  int active_cursor;
  struct glyph_row *glyph_row;
  struct glyph *glyph;

  /* This is pointless on invisible frames, and dangerous on garbaged
     windows and frames; in the latter case, the frame or window may
     be in the midst of changing its size, and x and y may be off the
     window.  */
  if (! FRAME_VISIBLE_P (f)
      || FRAME_GARBAGED_P (f)
      || vpos >= w->current_matrix->nrows
      || hpos >= w->current_matrix->matrix_w)
    return;

  /* If cursor is off and we want it off, return quickly.  */
  if (!on && !w->phys_cursor_on_p)
    return;

  glyph_row = MATRIX_ROW (w->current_matrix, vpos);
  /* If cursor row is not enabled, we don't really know where to
     display the cursor.  */
  if (!glyph_row->enabled_p)
    {
      w->phys_cursor_on_p = 0;
      return;
    }

  glyph = NULL;
  if (!glyph_row->exact_window_width_line_p
      || (0 <= hpos && hpos < glyph_row->used[TEXT_AREA]))
    glyph = glyph_row->glyphs[TEXT_AREA] + hpos;

  xassert (interrupt_input_blocked);

  /* Set new_cursor_type to the cursor we want to be displayed.  */
  new_cursor_type = get_window_cursor_type (w, glyph,
					    &new_cursor_width, &active_cursor);

  /* If cursor is currently being shown and we don't want it to be or
     it is in the wrong place, or the cursor type is not what we want,
     erase it.  */
  if (w->phys_cursor_on_p
      && (!on
	  || w->phys_cursor.x != x
	  || w->phys_cursor.y != y
	  || new_cursor_type != w->phys_cursor_type
	  || ((new_cursor_type == BAR_CURSOR || new_cursor_type == HBAR_CURSOR)
	      && new_cursor_width != w->phys_cursor_width)))
    erase_phys_cursor (w);

  /* Don't check phys_cursor_on_p here because that flag is only set
     to zero in some cases where we know that the cursor has been
     completely erased, to avoid the extra work of erasing the cursor
     twice.  In other words, phys_cursor_on_p can be 1 and the cursor
     still not be visible, or it has only been partly erased.  */
  if (on)
    {
      w->phys_cursor_ascent = glyph_row->ascent;
      w->phys_cursor_height = glyph_row->height;

      /* Set phys_cursor_.* before x_draw_.* is called because some
	 of them may need the information.  */
      w->phys_cursor.x = x;
      w->phys_cursor.y = glyph_row->y;
      w->phys_cursor.hpos = hpos;
      w->phys_cursor.vpos = vpos;
    }

  FRAME_RIF (f)->draw_window_cursor (w, glyph_row, x, y,
                                     new_cursor_type, new_cursor_width,
                                     on, active_cursor);
}


/* Switch the display of W's cursor on or off, according to the value
   of ON.  */

static void
update_window_cursor (struct window *w, int on)
{
  /* Don't update cursor in windows whose frame is in the process
     of being deleted.  */
  if (w->current_matrix)
    {
      int hpos = w->phys_cursor.hpos;
      int vpos = w->phys_cursor.vpos;
      struct glyph_row *row;

      if (vpos >= w->current_matrix->nrows
	  || hpos >= w->current_matrix->matrix_w)
	return;

      row = MATRIX_ROW (w->current_matrix, vpos);

      /* When the window is hscrolled, cursor hpos can legitimately be
	 out of bounds, but we draw the cursor at the corresponding
	 window margin in that case.  */
      if (!row->reversed_p && hpos < 0)
	hpos = 0;
      if (row->reversed_p && hpos >= row->used[TEXT_AREA])
	hpos = row->used[TEXT_AREA] - 1;

      BLOCK_INPUT;
      display_and_set_cursor (w, on, hpos, vpos,
			      w->phys_cursor.x, w->phys_cursor.y);
      UNBLOCK_INPUT;
    }
}


/* Call update_window_cursor with parameter ON_P on all leaf windows
   in the window tree rooted at W.  */

static void
update_cursor_in_window_tree (struct window *w, int on_p)
{
  while (w)
    {
      if (!NILP (w->hchild))
	update_cursor_in_window_tree (XWINDOW (w->hchild), on_p);
      else if (!NILP (w->vchild))
	update_cursor_in_window_tree (XWINDOW (w->vchild), on_p);
      else
	update_window_cursor (w, on_p);

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}


/* EXPORT:
   Display the cursor on window W, or clear it, according to ON_P.
   Don't change the cursor's position.  */

void
x_update_cursor (struct frame *f, int on_p)
{
  update_cursor_in_window_tree (XWINDOW (f->root_window), on_p);
}


/* EXPORT:
   Clear the cursor of window W to background color, and mark the
   cursor as not shown.  This is used when the text where the cursor
   is about to be rewritten.  */

void
x_clear_cursor (struct window *w)
{
  if (FRAME_VISIBLE_P (XFRAME (w->frame)) && w->phys_cursor_on_p)
    update_window_cursor (w, 0);
}

#endif /* HAVE_WINDOW_SYSTEM */

/* Implementation of draw_row_with_mouse_face for GUI sessions, GPM,
   and MSDOS.  */
static void
draw_row_with_mouse_face (struct window *w, int start_x, struct glyph_row *row,
			  int start_hpos, int end_hpos,
			  enum draw_glyphs_face draw)
{
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (w->frame)))
    {
      draw_glyphs (w, start_x, row, TEXT_AREA, start_hpos, end_hpos, draw, 0);
      return;
    }
#endif
#if defined (HAVE_GPM) || defined (MSDOS)
  tty_draw_row_with_mouse_face (w, row, start_hpos, end_hpos, draw);
#endif
}

/* Display the active region described by mouse_face_* according to DRAW.  */

static void
show_mouse_face (Mouse_HLInfo *hlinfo, enum draw_glyphs_face draw)
{
  struct window *w = XWINDOW (hlinfo->mouse_face_window);
  struct frame *f = XFRAME (WINDOW_FRAME (w));

  if (/* If window is in the process of being destroyed, don't bother
	 to do anything.  */
      w->current_matrix != NULL
      /* Don't update mouse highlight if hidden */
      && (draw != DRAW_MOUSE_FACE || !hlinfo->mouse_face_hidden)
      /* Recognize when we are called to operate on rows that don't exist
	 anymore.  This can happen when a window is split.  */
      && hlinfo->mouse_face_end_row < w->current_matrix->nrows)
    {
      int phys_cursor_on_p = w->phys_cursor_on_p;
      struct glyph_row *row, *first, *last;

      first = MATRIX_ROW (w->current_matrix, hlinfo->mouse_face_beg_row);
      last = MATRIX_ROW (w->current_matrix, hlinfo->mouse_face_end_row);

      for (row = first; row <= last && row->enabled_p; ++row)
	{
	  int start_hpos, end_hpos, start_x;

	  /* For all but the first row, the highlight starts at column 0.  */
	  if (row == first)
	    {
	      /* R2L rows have BEG and END in reversed order, but the
		 screen drawing geometry is always left to right.  So
		 we need to mirror the beginning and end of the
		 highlighted area in R2L rows.  */
	      if (!row->reversed_p)
		{
		  start_hpos = hlinfo->mouse_face_beg_col;
		  start_x = hlinfo->mouse_face_beg_x;
		}
	      else if (row == last)
		{
		  start_hpos = hlinfo->mouse_face_end_col;
		  start_x = hlinfo->mouse_face_end_x;
		}
	      else
		{
		  start_hpos = 0;
		  start_x = 0;
		}
	    }
	  else if (row->reversed_p && row == last)
	    {
	      start_hpos = hlinfo->mouse_face_end_col;
	      start_x = hlinfo->mouse_face_end_x;
	    }
	  else
	    {
	      start_hpos = 0;
	      start_x = 0;
	    }

	  if (row == last)
	    {
	      if (!row->reversed_p)
		end_hpos = hlinfo->mouse_face_end_col;
	      else if (row == first)
		end_hpos = hlinfo->mouse_face_beg_col;
	      else
		{
		  end_hpos = row->used[TEXT_AREA];
		  if (draw == DRAW_NORMAL_TEXT)
		    row->fill_line_p = 1; /* Clear to end of line */
		}
	    }
	  else if (row->reversed_p && row == first)
	    end_hpos = hlinfo->mouse_face_beg_col;
	  else
	    {
	      end_hpos = row->used[TEXT_AREA];
	      if (draw == DRAW_NORMAL_TEXT)
		row->fill_line_p = 1; /* Clear to end of line */
	    }

	  if (end_hpos > start_hpos)
	    {
	      draw_row_with_mouse_face (w, start_x, row,
					start_hpos, end_hpos, draw);

	      row->mouse_face_p
		= draw == DRAW_MOUSE_FACE || draw == DRAW_IMAGE_RAISED;
	    }
	}

#ifdef HAVE_WINDOW_SYSTEM
      /* When we've written over the cursor, arrange for it to
	 be displayed again.  */
      if (FRAME_WINDOW_P (f)
	  && phys_cursor_on_p && !w->phys_cursor_on_p)
	{
	  int hpos = w->phys_cursor.hpos;

	  /* When the window is hscrolled, cursor hpos can legitimately be
	     out of bounds, but we draw the cursor at the corresponding
	     window margin in that case.  */
	  if (!row->reversed_p && hpos < 0)
	    hpos = 0;
	  if (row->reversed_p && hpos >= row->used[TEXT_AREA])
	    hpos = row->used[TEXT_AREA] - 1;

	  BLOCK_INPUT;
	  display_and_set_cursor (w, 1, hpos, w->phys_cursor.vpos,
				  w->phys_cursor.x, w->phys_cursor.y);
	  UNBLOCK_INPUT;
	}
#endif	/* HAVE_WINDOW_SYSTEM */
    }

#ifdef HAVE_WINDOW_SYSTEM
  /* Change the mouse cursor.  */
  if (FRAME_WINDOW_P (f))
    {
      if (draw == DRAW_NORMAL_TEXT
	  && !EQ (hlinfo->mouse_face_window, f->tool_bar_window))
	FRAME_RIF (f)->define_frame_cursor (f, FRAME_X_OUTPUT (f)->text_cursor);
      else if (draw == DRAW_MOUSE_FACE)
	FRAME_RIF (f)->define_frame_cursor (f, FRAME_X_OUTPUT (f)->hand_cursor);
      else
	FRAME_RIF (f)->define_frame_cursor (f, FRAME_X_OUTPUT (f)->nontext_cursor);
    }
#endif	/* HAVE_WINDOW_SYSTEM */
}

/* EXPORT:
   Clear out the mouse-highlighted active region.
   Redraw it un-highlighted first.  Value is non-zero if mouse
   face was actually drawn unhighlighted.  */

int
clear_mouse_face (Mouse_HLInfo *hlinfo)
{
  int cleared = 0;

  if (!hlinfo->mouse_face_hidden && !NILP (hlinfo->mouse_face_window))
    {
      show_mouse_face (hlinfo, DRAW_NORMAL_TEXT);
      cleared = 1;
    }

  hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
  hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
  hlinfo->mouse_face_window = Qnil;
  hlinfo->mouse_face_overlay = Qnil;
  return cleared;
}

/* Return non-zero if the coordinates HPOS and VPOS on windows W are
   within the mouse face on that window.  */
static int
coords_in_mouse_face_p (struct window *w, int hpos, int vpos)
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (XFRAME (w->frame));

  /* Quickly resolve the easy cases.  */
  if (!(WINDOWP (hlinfo->mouse_face_window)
	&& XWINDOW (hlinfo->mouse_face_window) == w))
    return 0;
  if (vpos < hlinfo->mouse_face_beg_row
      || vpos > hlinfo->mouse_face_end_row)
    return 0;
  if (vpos > hlinfo->mouse_face_beg_row
      && vpos < hlinfo->mouse_face_end_row)
    return 1;

  if (!MATRIX_ROW (w->current_matrix, vpos)->reversed_p)
    {
      if (hlinfo->mouse_face_beg_row == hlinfo->mouse_face_end_row)
	{
	  if (hlinfo->mouse_face_beg_col <= hpos && hpos < hlinfo->mouse_face_end_col)
	    return 1;
	}
      else if ((vpos == hlinfo->mouse_face_beg_row
		&& hpos >= hlinfo->mouse_face_beg_col)
	       || (vpos == hlinfo->mouse_face_end_row
		   && hpos < hlinfo->mouse_face_end_col))
	return 1;
    }
  else
    {
       if (hlinfo->mouse_face_beg_row == hlinfo->mouse_face_end_row)
	{
	  if (hlinfo->mouse_face_end_col < hpos && hpos <= hlinfo->mouse_face_beg_col)
	    return 1;
	}
      else if ((vpos == hlinfo->mouse_face_beg_row
		&& hpos <= hlinfo->mouse_face_beg_col)
	       || (vpos == hlinfo->mouse_face_end_row
		   && hpos > hlinfo->mouse_face_end_col))
	return 1;
    }
  return 0;
}


/* EXPORT:
   Non-zero if physical cursor of window W is within mouse face.  */

int
cursor_in_mouse_face_p (struct window *w)
{
  int hpos = w->phys_cursor.hpos;
  int vpos = w->phys_cursor.vpos;
  struct glyph_row *row = MATRIX_ROW (w->current_matrix, vpos);

  /* When the window is hscrolled, cursor hpos can legitimately be out
     of bounds, but we draw the cursor at the corresponding window
     margin in that case.  */
  if (!row->reversed_p && hpos < 0)
    hpos = 0;
  if (row->reversed_p && hpos >= row->used[TEXT_AREA])
    hpos = row->used[TEXT_AREA] - 1;

  return coords_in_mouse_face_p (w, hpos, vpos);
}



/* Find the glyph rows START_ROW and END_ROW of window W that display
   characters between buffer positions START_CHARPOS and END_CHARPOS
   (excluding END_CHARPOS).  DISP_STRING is a display string that
   covers these buffer positions.  This is similar to
   row_containing_pos, but is more accurate when bidi reordering makes
   buffer positions change non-linearly with glyph rows.  */
static void
rows_from_pos_range (struct window *w,
		     EMACS_INT start_charpos, EMACS_INT end_charpos,
		     Lisp_Object disp_string,
		     struct glyph_row **start, struct glyph_row **end)
{
  struct glyph_row *first = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  int last_y = window_text_bottom_y (w);
  struct glyph_row *row;

  *start = NULL;
  *end = NULL;

  while (!first->enabled_p
	 && first < MATRIX_BOTTOM_TEXT_ROW (w->current_matrix, w))
    first++;

  /* Find the START row.  */
  for (row = first;
       row->enabled_p && MATRIX_ROW_BOTTOM_Y (row) <= last_y;
       row++)
    {
      /* A row can potentially be the START row if the range of the
	 characters it displays intersects the range
	 [START_CHARPOS..END_CHARPOS).  */
      if (! ((start_charpos < MATRIX_ROW_START_CHARPOS (row)
	      && end_charpos < MATRIX_ROW_START_CHARPOS (row))
	     /* See the commentary in row_containing_pos, for the
		explanation of the complicated way to check whether
		some position is beyond the end of the characters
		displayed by a row.  */
	     || ((start_charpos > MATRIX_ROW_END_CHARPOS (row)
		  || (start_charpos == MATRIX_ROW_END_CHARPOS (row)
		      && !row->ends_at_zv_p
		      && !MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (row)))
		 && (end_charpos > MATRIX_ROW_END_CHARPOS (row)
		     || (end_charpos == MATRIX_ROW_END_CHARPOS (row)
			 && !row->ends_at_zv_p
			 && !MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (row))))))
	{
	  /* Found a candidate row.  Now make sure at least one of the
	     glyphs it displays has a charpos from the range
	     [START_CHARPOS..END_CHARPOS).

	     This is not obvious because bidi reordering could make
	     buffer positions of a row be 1,2,3,102,101,100, and if we
	     want to highlight characters in [50..60), we don't want
	     this row, even though [50..60) does intersect [1..103),
	     the range of character positions given by the row's start
	     and end positions.  */
	  struct glyph *g = row->glyphs[TEXT_AREA];
	  struct glyph *e = g + row->used[TEXT_AREA];

	  while (g < e)
	    {
	      if (((BUFFERP (g->object) || INTEGERP (g->object))
		   && start_charpos <= g->charpos && g->charpos < end_charpos)
		  /* A glyph that comes from DISP_STRING is by
		     definition to be highlighted.  */
		  || EQ (g->object, disp_string))
		*start = row;
	      g++;
	    }
	  if (*start)
	    break;
	}
    }

  /* Find the END row.  */
  if (!*start
      /* If the last row is partially visible, start looking for END
	 from that row, instead of starting from FIRST.  */
      && !(row->enabled_p
	   && row->y < last_y && MATRIX_ROW_BOTTOM_Y (row) > last_y))
    row = first;
  for ( ; row->enabled_p && MATRIX_ROW_BOTTOM_Y (row) <= last_y; row++)
    {
      struct glyph_row *next = row + 1;
      EMACS_INT next_start = MATRIX_ROW_START_CHARPOS (next);

      if (!next->enabled_p
	  || next >= MATRIX_BOTTOM_TEXT_ROW (w->current_matrix, w)
	  /* The first row >= START whose range of displayed characters
	     does NOT intersect the range [START_CHARPOS..END_CHARPOS]
	     is the row END + 1.  */
	  || (start_charpos < next_start
	      && end_charpos < next_start)
	  || ((start_charpos > MATRIX_ROW_END_CHARPOS (next)
	       || (start_charpos == MATRIX_ROW_END_CHARPOS (next)
		   && !next->ends_at_zv_p
		   && !MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (next)))
	      && (end_charpos > MATRIX_ROW_END_CHARPOS (next)
		  || (end_charpos == MATRIX_ROW_END_CHARPOS (next)
		      && !next->ends_at_zv_p
		      && !MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (next)))))
	{
	  *end = row;
	  break;
	}
      else
	{
	  /* If the next row's edges intersect [START_CHARPOS..END_CHARPOS],
	     but none of the characters it displays are in the range, it is
	     also END + 1. */
	  struct glyph *g = next->glyphs[TEXT_AREA];
	  struct glyph *s = g;
	  struct glyph *e = g + next->used[TEXT_AREA];

	  while (g < e)
	    {
	      if (((BUFFERP (g->object) || INTEGERP (g->object))
		   && ((start_charpos <= g->charpos && g->charpos < end_charpos)
		       /* If the buffer position of the first glyph in
			  the row is equal to END_CHARPOS, it means
			  the last character to be highlighted is the
			  newline of ROW, and we must consider NEXT as
			  END, not END+1.  */
		       || (((!next->reversed_p && g == s)
			    || (next->reversed_p && g == e - 1))
			   && (g->charpos == end_charpos
			       /* Special case for when NEXT is an
				  empty line at ZV.  */
			       || (g->charpos == -1
				   && !row->ends_at_zv_p
				   && next_start == end_charpos)))))
		  /* A glyph that comes from DISP_STRING is by
		     definition to be highlighted.  */
		  || EQ (g->object, disp_string))
		break;
	      g++;
	    }
	  if (g == e)
	    {
	      *end = row;
	      break;
	    }
	  /* The first row that ends at ZV must be the last to be
	     highlighted.  */
	  else if (next->ends_at_zv_p)
	    {
	      *end = next;
	      break;
	    }
	}
    }
}

/* This function sets the mouse_face_* elements of HLINFO, assuming
   the mouse cursor is on a glyph with buffer charpos MOUSE_CHARPOS in
   window WINDOW.  START_CHARPOS and END_CHARPOS are buffer positions
   for the overlay or run of text properties specifying the mouse
   face.  BEFORE_STRING and AFTER_STRING, if non-nil, are a
   before-string and after-string that must also be highlighted.
   DISP_STRING, if non-nil, is a display string that may cover some
   or all of the highlighted text.  */

static void
mouse_face_from_buffer_pos (Lisp_Object window,
			    Mouse_HLInfo *hlinfo,
			    EMACS_INT mouse_charpos,
			    EMACS_INT start_charpos,
			    EMACS_INT end_charpos,
			    Lisp_Object before_string,
			    Lisp_Object after_string,
			    Lisp_Object disp_string)
{
  struct window *w = XWINDOW (window);
  struct glyph_row *first = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  struct glyph_row *r1, *r2;
  struct glyph *glyph, *end;
  EMACS_INT ignore, pos;
  int x;

  xassert (NILP (disp_string) || STRINGP (disp_string));
  xassert (NILP (before_string) || STRINGP (before_string));
  xassert (NILP (after_string) || STRINGP (after_string));

  /* Find the rows corresponding to START_CHARPOS and END_CHARPOS.  */
  rows_from_pos_range (w, start_charpos, end_charpos, disp_string, &r1, &r2);
  if (r1 == NULL)
    r1 = MATRIX_ROW (w->current_matrix, XFASTINT (w->window_end_vpos));
  /* If the before-string or display-string contains newlines,
     rows_from_pos_range skips to its last row.  Move back.  */
  if (!NILP (before_string) || !NILP (disp_string))
    {
      struct glyph_row *prev;
      while ((prev = r1 - 1, prev >= first)
	     && MATRIX_ROW_END_CHARPOS (prev) == start_charpos
	     && prev->used[TEXT_AREA] > 0)
	{
	  struct glyph *beg = prev->glyphs[TEXT_AREA];
	  glyph = beg + prev->used[TEXT_AREA];
	  while (--glyph >= beg && INTEGERP (glyph->object));
	  if (glyph < beg
	      || !(EQ (glyph->object, before_string)
		   || EQ (glyph->object, disp_string)))
	    break;
	  r1 = prev;
	}
    }
  if (r2 == NULL)
    {
      r2 = MATRIX_ROW (w->current_matrix, XFASTINT (w->window_end_vpos));
      hlinfo->mouse_face_past_end = 1;
    }
  else if (!NILP (after_string))
    {
      /* If the after-string has newlines, advance to its last row.  */
      struct glyph_row *next;
      struct glyph_row *last
	= MATRIX_ROW (w->current_matrix, XFASTINT (w->window_end_vpos));

      for (next = r2 + 1;
	   next <= last
	     && next->used[TEXT_AREA] > 0
	     && EQ (next->glyphs[TEXT_AREA]->object, after_string);
	   ++next)
	r2 = next;
    }
  /* The rest of the display engine assumes that mouse_face_beg_row is
     either above mouse_face_end_row or identical to it.  But with
     bidi-reordered continued lines, the row for START_CHARPOS could
     be below the row for END_CHARPOS.  If so, swap the rows and store
     them in correct order.  */
  if (r1->y > r2->y)
    {
      struct glyph_row *tem = r2;

      r2 = r1;
      r1 = tem;
    }

  hlinfo->mouse_face_beg_y = r1->y;
  hlinfo->mouse_face_beg_row = MATRIX_ROW_VPOS (r1, w->current_matrix);
  hlinfo->mouse_face_end_y = r2->y;
  hlinfo->mouse_face_end_row = MATRIX_ROW_VPOS (r2, w->current_matrix);

  /* For a bidi-reordered row, the positions of BEFORE_STRING,
     AFTER_STRING, DISP_STRING, START_CHARPOS, and END_CHARPOS
     could be anywhere in the row and in any order.  The strategy
     below is to find the leftmost and the rightmost glyph that
     belongs to either of these 3 strings, or whose position is
     between START_CHARPOS and END_CHARPOS, and highlight all the
     glyphs between those two.  This may cover more than just the text
     between START_CHARPOS and END_CHARPOS if the range of characters
     strides the bidi level boundary, e.g. if the beginning is in R2L
     text while the end is in L2R text or vice versa.  */
  if (!r1->reversed_p)
    {
      /* This row is in a left to right paragraph.  Scan it left to
	 right.  */
      glyph = r1->glyphs[TEXT_AREA];
      end = glyph + r1->used[TEXT_AREA];
      x = r1->x;

      /* Skip truncation glyphs at the start of the glyph row.  */
      if (r1->displays_text_p)
	for (; glyph < end
	       && INTEGERP (glyph->object)
	       && glyph->charpos < 0;
	     ++glyph)
	  x += glyph->pixel_width;

      /* Scan the glyph row, looking for BEFORE_STRING, AFTER_STRING,
	 or DISP_STRING, and the first glyph from buffer whose
	 position is between START_CHARPOS and END_CHARPOS.  */
      for (; glyph < end
	     && !INTEGERP (glyph->object)
	     && !EQ (glyph->object, disp_string)
	     && !(BUFFERP (glyph->object)
		  && (glyph->charpos >= start_charpos
		      && glyph->charpos < end_charpos));
	   ++glyph)
	{
	  /* BEFORE_STRING or AFTER_STRING are only relevant if they
	     are present at buffer positions between START_CHARPOS and
	     END_CHARPOS, or if they come from an overlay.  */
	  if (EQ (glyph->object, before_string))
	    {
	      pos = string_buffer_position (before_string,
					    start_charpos);
	      /* If pos == 0, it means before_string came from an
		 overlay, not from a buffer position.  */
	      if (!pos || (pos >= start_charpos && pos < end_charpos))
		break;
	    }
	  else if (EQ (glyph->object, after_string))
	    {
	      pos = string_buffer_position (after_string, end_charpos);
	      if (!pos || (pos >= start_charpos && pos < end_charpos))
		break;
	    }
	  x += glyph->pixel_width;
	}
      hlinfo->mouse_face_beg_x = x;
      hlinfo->mouse_face_beg_col = glyph - r1->glyphs[TEXT_AREA];
    }
  else
    {
      /* This row is in a right to left paragraph.  Scan it right to
	 left.  */
      struct glyph *g;

      end = r1->glyphs[TEXT_AREA] - 1;
      glyph = end + r1->used[TEXT_AREA];

      /* Skip truncation glyphs at the start of the glyph row.  */
      if (r1->displays_text_p)
	for (; glyph > end
	       && INTEGERP (glyph->object)
	       && glyph->charpos < 0;
	     --glyph)
	  ;

      /* Scan the glyph row, looking for BEFORE_STRING, AFTER_STRING,
	 or DISP_STRING, and the first glyph from buffer whose
	 position is between START_CHARPOS and END_CHARPOS.  */
      for (; glyph > end
	     && !INTEGERP (glyph->object)
	     && !EQ (glyph->object, disp_string)
	     && !(BUFFERP (glyph->object)
		  && (glyph->charpos >= start_charpos
		      && glyph->charpos < end_charpos));
	   --glyph)
	{
	  /* BEFORE_STRING or AFTER_STRING are only relevant if they
	     are present at buffer positions between START_CHARPOS and
	     END_CHARPOS, or if they come from an overlay.  */
	  if (EQ (glyph->object, before_string))
	    {
	      pos = string_buffer_position (before_string, start_charpos);
	      /* If pos == 0, it means before_string came from an
		 overlay, not from a buffer position.  */
	      if (!pos || (pos >= start_charpos && pos < end_charpos))
		break;
	    }
	  else if (EQ (glyph->object, after_string))
	    {
	      pos = string_buffer_position (after_string, end_charpos);
	      if (!pos || (pos >= start_charpos && pos < end_charpos))
		break;
	    }
	}

      glyph++; /* first glyph to the right of the highlighted area */
      for (g = r1->glyphs[TEXT_AREA], x = r1->x; g < glyph; g++)
	x += g->pixel_width;
      hlinfo->mouse_face_beg_x = x;
      hlinfo->mouse_face_beg_col = glyph - r1->glyphs[TEXT_AREA];
    }

  /* If the highlight ends in a different row, compute GLYPH and END
     for the end row.  Otherwise, reuse the values computed above for
     the row where the highlight begins.  */
  if (r2 != r1)
    {
      if (!r2->reversed_p)
	{
	  glyph = r2->glyphs[TEXT_AREA];
	  end = glyph + r2->used[TEXT_AREA];
	  x = r2->x;
	}
      else
	{
	  end = r2->glyphs[TEXT_AREA] - 1;
	  glyph = end + r2->used[TEXT_AREA];
	}
    }

  if (!r2->reversed_p)
    {
      /* Skip truncation and continuation glyphs near the end of the
	 row, and also blanks and stretch glyphs inserted by
	 extend_face_to_end_of_line.  */
      while (end > glyph
	     && INTEGERP ((end - 1)->object))
	--end;
      /* Scan the rest of the glyph row from the end, looking for the
	 first glyph that comes from BEFORE_STRING, AFTER_STRING, or
	 DISP_STRING, or whose position is between START_CHARPOS
	 and END_CHARPOS */
      for (--end;
	     end > glyph
	     && !INTEGERP (end->object)
	     && !EQ (end->object, disp_string)
	     && !(BUFFERP (end->object)
		  && (end->charpos >= start_charpos
		      && end->charpos < end_charpos));
	   --end)
	{
	  /* BEFORE_STRING or AFTER_STRING are only relevant if they
	     are present at buffer positions between START_CHARPOS and
	     END_CHARPOS, or if they come from an overlay.  */
	  if (EQ (end->object, before_string))
	    {
	      pos = string_buffer_position (before_string, start_charpos);
	      if (!pos || (pos >= start_charpos && pos < end_charpos))
		break;
	    }
	  else if (EQ (end->object, after_string))
	    {
	      pos = string_buffer_position (after_string, end_charpos);
	      if (!pos || (pos >= start_charpos && pos < end_charpos))
		break;
	    }
	}
      /* Find the X coordinate of the last glyph to be highlighted.  */
      for (; glyph <= end; ++glyph)
	x += glyph->pixel_width;

      hlinfo->mouse_face_end_x = x;
      hlinfo->mouse_face_end_col = glyph - r2->glyphs[TEXT_AREA];
    }
  else
    {
      /* Skip truncation and continuation glyphs near the end of the
	 row, and also blanks and stretch glyphs inserted by
	 extend_face_to_end_of_line.  */
      x = r2->x;
      end++;
      while (end < glyph
	     && INTEGERP (end->object))
	{
	  x += end->pixel_width;
	  ++end;
	}
      /* Scan the rest of the glyph row from the end, looking for the
	 first glyph that comes from BEFORE_STRING, AFTER_STRING, or
	 DISP_STRING, or whose position is between START_CHARPOS
	 and END_CHARPOS */
      for ( ;
	     end < glyph
	     && !INTEGERP (end->object)
	     && !EQ (end->object, disp_string)
	     && !(BUFFERP (end->object)
		  && (end->charpos >= start_charpos
		      && end->charpos < end_charpos));
	   ++end)
	{
	  /* BEFORE_STRING or AFTER_STRING are only relevant if they
	     are present at buffer positions between START_CHARPOS and
	     END_CHARPOS, or if they come from an overlay.  */
	  if (EQ (end->object, before_string))
	    {
	      pos = string_buffer_position (before_string, start_charpos);
	      if (!pos || (pos >= start_charpos && pos < end_charpos))
		break;
	    }
	  else if (EQ (end->object, after_string))
	    {
	      pos = string_buffer_position (after_string, end_charpos);
	      if (!pos || (pos >= start_charpos && pos < end_charpos))
		break;
	    }
	  x += end->pixel_width;
	}
      /* If we exited the above loop because we arrived at the last
	 glyph of the row, and its buffer position is still not in
	 range, it means the last character in range is the preceding
	 newline.  Bump the end column and x values to get past the
	 last glyph.  */
      if (end == glyph
	  && BUFFERP (end->object)
	  && (end->charpos < start_charpos
	      || end->charpos >= end_charpos))
	{
	  x += end->pixel_width;
	  ++end;
	}
      hlinfo->mouse_face_end_x = x;
      hlinfo->mouse_face_end_col = end - r2->glyphs[TEXT_AREA];
    }

  hlinfo->mouse_face_window = window;
  hlinfo->mouse_face_face_id
    = face_at_buffer_position (w, mouse_charpos, 0, 0, &ignore,
			       mouse_charpos + 1,
			       !hlinfo->mouse_face_hidden, -1);
  show_mouse_face (hlinfo, DRAW_MOUSE_FACE);
}

/* The following function is not used anymore (replaced with
   mouse_face_from_string_pos), but I leave it here for the time
   being, in case someone would.  */

#if 0	/* not used */

/* Find the position of the glyph for position POS in OBJECT in
   window W's current matrix, and return in *X, *Y the pixel
   coordinates, and return in *HPOS, *VPOS the column/row of the glyph.

   RIGHT_P non-zero means return the position of the right edge of the
   glyph, RIGHT_P zero means return the left edge position.

   If no glyph for POS exists in the matrix, return the position of
   the glyph with the next smaller position that is in the matrix, if
   RIGHT_P is zero.  If RIGHT_P is non-zero, and no glyph for POS
   exists in the matrix, return the position of the glyph with the
   next larger position in OBJECT.

   Value is non-zero if a glyph was found.  */

static int
fast_find_string_pos (struct window *w, EMACS_INT pos, Lisp_Object object,
		      int *hpos, int *vpos, int *x, int *y, int right_p)
{
  int yb = window_text_bottom_y (w);
  struct glyph_row *r;
  struct glyph *best_glyph = NULL;
  struct glyph_row *best_row = NULL;
  int best_x = 0;

  for (r = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
       r->enabled_p && r->y < yb;
       ++r)
    {
      struct glyph *g = r->glyphs[TEXT_AREA];
      struct glyph *e = g + r->used[TEXT_AREA];
      int gx;

      for (gx = r->x; g < e; gx += g->pixel_width, ++g)
	if (EQ (g->object, object))
	  {
	    if (g->charpos == pos)
	      {
		best_glyph = g;
		best_x = gx;
		best_row = r;
		goto found;
	      }
	    else if (best_glyph == NULL
		     || ((eabs (g->charpos - pos)
			 < eabs (best_glyph->charpos - pos))
			 && (right_p
			     ? g->charpos < pos
			     : g->charpos > pos)))
	      {
		best_glyph = g;
		best_x = gx;
		best_row = r;
	      }
	  }
    }

 found:

  if (best_glyph)
    {
      *x = best_x;
      *hpos = best_glyph - best_row->glyphs[TEXT_AREA];

      if (right_p)
	{
	  *x += best_glyph->pixel_width;
	  ++*hpos;
	}

      *y = best_row->y;
      *vpos = best_row - w->current_matrix->rows;
    }

  return best_glyph != NULL;
}
#endif	/* not used */

/* Find the positions of the first and the last glyphs in window W's
   current matrix that occlude positions [STARTPOS..ENDPOS] in OBJECT
   (assumed to be a string), and return in HLINFO's mouse_face_*
   members the pixel and column/row coordinates of those glyphs.  */

static void
mouse_face_from_string_pos (struct window *w, Mouse_HLInfo *hlinfo,
			    Lisp_Object object,
			    EMACS_INT startpos, EMACS_INT endpos)
{
  int yb = window_text_bottom_y (w);
  struct glyph_row *r;
  struct glyph *g, *e;
  int gx;
  int found = 0;

  /* Find the glyph row with at least one position in the range
     [STARTPOS..ENDPOS], and the first glyph in that row whose
     position belongs to that range.  */
  for (r = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
       r->enabled_p && r->y < yb;
       ++r)
    {
      if (!r->reversed_p)
	{
	  g = r->glyphs[TEXT_AREA];
	  e = g + r->used[TEXT_AREA];
	  for (gx = r->x; g < e; gx += g->pixel_width, ++g)
	    if (EQ (g->object, object)
		&& startpos <= g->charpos && g->charpos <= endpos)
	      {
		hlinfo->mouse_face_beg_row = r - w->current_matrix->rows;
		hlinfo->mouse_face_beg_y = r->y;
		hlinfo->mouse_face_beg_col = g - r->glyphs[TEXT_AREA];
		hlinfo->mouse_face_beg_x = gx;
		found = 1;
		break;
	      }
	}
      else
	{
	  struct glyph *g1;

	  e = r->glyphs[TEXT_AREA];
	  g = e + r->used[TEXT_AREA];
	  for ( ; g > e; --g)
	    if (EQ ((g-1)->object, object)
		&& startpos <= (g-1)->charpos && (g-1)->charpos <= endpos)
	      {
		hlinfo->mouse_face_beg_row = r - w->current_matrix->rows;
		hlinfo->mouse_face_beg_y = r->y;
		hlinfo->mouse_face_beg_col = g - r->glyphs[TEXT_AREA];
		for (gx = r->x, g1 = r->glyphs[TEXT_AREA]; g1 < g; ++g1)
		  gx += g1->pixel_width;
		hlinfo->mouse_face_beg_x = gx;
		found = 1;
		break;
	      }
	}
      if (found)
	break;
    }

  if (!found)
    return;

  /* Starting with the next row, look for the first row which does NOT
     include any glyphs whose positions are in the range.  */
  for (++r; r->enabled_p && r->y < yb; ++r)
    {
      g = r->glyphs[TEXT_AREA];
      e = g + r->used[TEXT_AREA];
      found = 0;
      for ( ; g < e; ++g)
	if (EQ (g->object, object)
	    && startpos <= g->charpos && g->charpos <= endpos)
	  {
	    found = 1;
	    break;
	  }
      if (!found)
	break;
    }

  /* The highlighted region ends on the previous row.  */
  r--;

  /* Set the end row and its vertical pixel coordinate.  */
  hlinfo->mouse_face_end_row = r - w->current_matrix->rows;
  hlinfo->mouse_face_end_y = r->y;

  /* Compute and set the end column and the end column's horizontal
     pixel coordinate.  */
  if (!r->reversed_p)
    {
      g = r->glyphs[TEXT_AREA];
      e = g + r->used[TEXT_AREA];
      for ( ; e > g; --e)
	if (EQ ((e-1)->object, object)
	    && startpos <= (e-1)->charpos && (e-1)->charpos <= endpos)
	  break;
      hlinfo->mouse_face_end_col = e - g;

      for (gx = r->x; g < e; ++g)
	gx += g->pixel_width;
      hlinfo->mouse_face_end_x = gx;
    }
  else
    {
      e = r->glyphs[TEXT_AREA];
      g = e + r->used[TEXT_AREA];
      for (gx = r->x ; e < g; ++e)
	{
	  if (EQ (e->object, object)
	      && startpos <= e->charpos && e->charpos <= endpos)
	    break;
	  gx += e->pixel_width;
	}
      hlinfo->mouse_face_end_col = e - r->glyphs[TEXT_AREA];
      hlinfo->mouse_face_end_x = gx;
    }
}

#ifdef HAVE_WINDOW_SYSTEM

/* See if position X, Y is within a hot-spot of an image.  */

static int
on_hot_spot_p (Lisp_Object hot_spot, int x, int y)
{
  if (!CONSP (hot_spot))
    return 0;

  if (EQ (XCAR (hot_spot), Qrect))
    {
      /* CDR is (Top-Left . Bottom-Right) = ((x0 . y0) . (x1 . y1))  */
      Lisp_Object rect = XCDR (hot_spot);
      Lisp_Object tem;
      if (!CONSP (rect))
	return 0;
      if (!CONSP (XCAR (rect)))
	return 0;
      if (!CONSP (XCDR (rect)))
	return 0;
      if (!(tem = XCAR (XCAR (rect)), INTEGERP (tem) && x >= XINT (tem)))
	return 0;
      if (!(tem = XCDR (XCAR (rect)), INTEGERP (tem) && y >= XINT (tem)))
	return 0;
      if (!(tem = XCAR (XCDR (rect)), INTEGERP (tem) && x <= XINT (tem)))
	return 0;
      if (!(tem = XCDR (XCDR (rect)), INTEGERP (tem) && y <= XINT (tem)))
	return 0;
      return 1;
    }
  else if (EQ (XCAR (hot_spot), Qcircle))
    {
      /* CDR is (Center . Radius) = ((x0 . y0) . r) */
      Lisp_Object circ = XCDR (hot_spot);
      Lisp_Object lr, lx0, ly0;
      if (CONSP (circ)
	  && CONSP (XCAR (circ))
	  && (lr = XCDR (circ), INTEGERP (lr) || FLOATP (lr))
	  && (lx0 = XCAR (XCAR (circ)), INTEGERP (lx0))
	  && (ly0 = XCDR (XCAR (circ)), INTEGERP (ly0)))
	{
	  double r = XFLOATINT (lr);
	  double dx = XINT (lx0) - x;
	  double dy = XINT (ly0) - y;
	  return (dx * dx + dy * dy <= r * r);
	}
    }
  else if (EQ (XCAR (hot_spot), Qpoly))
    {
      /* CDR is [x0 y0 x1 y1 x2 y2 ...x(n-1) y(n-1)] */
      if (VECTORP (XCDR (hot_spot)))
	{
	  struct Lisp_Vector *v = XVECTOR (XCDR (hot_spot));
	  Lisp_Object *poly = v->contents;
	  int n = v->header.size;
	  int i;
	  int inside = 0;
	  Lisp_Object lx, ly;
	  int x0, y0;

	  /* Need an even number of coordinates, and at least 3 edges.  */
	  if (n < 6 || n & 1)
	    return 0;

	  /* Count edge segments intersecting line from (X,Y) to (X,infinity).
	     If count is odd, we are inside polygon.  Pixels on edges
	     may or may not be included depending on actual geometry of the
	     polygon.  */
	  if ((lx = poly[n-2], !INTEGERP (lx))
	      || (ly = poly[n-1], !INTEGERP (lx)))
	    return 0;
	  x0 = XINT (lx), y0 = XINT (ly);
	  for (i = 0; i < n; i += 2)
	    {
	      int x1 = x0, y1 = y0;
	      if ((lx = poly[i], !INTEGERP (lx))
		  || (ly = poly[i+1], !INTEGERP (ly)))
		return 0;
	      x0 = XINT (lx), y0 = XINT (ly);

	      /* Does this segment cross the X line?  */
	      if (x0 >= x)
		{
		  if (x1 >= x)
		    continue;
		}
	      else if (x1 < x)
		continue;
	      if (y > y0 && y > y1)
		continue;
	      if (y < y0 + ((y1 - y0) * (x - x0)) / (x1 - x0))
		inside = !inside;
	    }
	  return inside;
	}
    }
  return 0;
}

Lisp_Object
find_hot_spot (Lisp_Object map, int x, int y)
{
  while (CONSP (map))
    {
      if (CONSP (XCAR (map))
	  && on_hot_spot_p (XCAR (XCAR (map)), x, y))
	return XCAR (map);
      map = XCDR (map);
    }

  return Qnil;
}

DEFUN ("lookup-image-map", Flookup_image_map, Slookup_image_map,
       3, 3, 0,
       doc: /* Lookup in image map MAP coordinates X and Y.
An image map is an alist where each element has the format (AREA ID PLIST).
An AREA is specified as either a rectangle, a circle, or a polygon:
A rectangle is a cons (rect . ((x0 . y0) . (x1 . y1))) specifying the
pixel coordinates of the upper left and bottom right corners.
A circle is a cons (circle . ((x0 . y0) . r)) specifying the center
and the radius of the circle; r may be a float or integer.
A polygon is a cons (poly . [x0 y0 x1 y1 ...]) where each pair in the
vector describes one corner in the polygon.
Returns the alist element for the first matching AREA in MAP.  */)
  (Lisp_Object map, Lisp_Object x, Lisp_Object y)
{
  if (NILP (map))
    return Qnil;

  CHECK_NUMBER (x);
  CHECK_NUMBER (y);

  return find_hot_spot (map, XINT (x), XINT (y));
}


/* Display frame CURSOR, optionally using shape defined by POINTER.  */
static void
define_frame_cursor1 (struct frame *f, Cursor cursor, Lisp_Object pointer)
{
  /* Do not change cursor shape while dragging mouse.  */
  if (!NILP (do_mouse_tracking))
    return;

  if (!NILP (pointer))
    {
      if (EQ (pointer, Qarrow))
	cursor = FRAME_X_OUTPUT (f)->nontext_cursor;
      else if (EQ (pointer, Qhand))
	cursor = FRAME_X_OUTPUT (f)->hand_cursor;
      else if (EQ (pointer, Qtext))
	cursor = FRAME_X_OUTPUT (f)->text_cursor;
      else if (EQ (pointer, intern ("hdrag")))
	cursor = FRAME_X_OUTPUT (f)->horizontal_drag_cursor;
#ifdef HAVE_X_WINDOWS
      else if (EQ (pointer, intern ("vdrag")))
	cursor = FRAME_X_DISPLAY_INFO (f)->vertical_scroll_bar_cursor;
#endif
      else if (EQ (pointer, intern ("hourglass")))
	cursor = FRAME_X_OUTPUT (f)->hourglass_cursor;
      else if (EQ (pointer, Qmodeline))
	cursor = FRAME_X_OUTPUT (f)->modeline_cursor;
      else
	cursor = FRAME_X_OUTPUT (f)->nontext_cursor;
    }

  if (cursor != No_Cursor)
    FRAME_RIF (f)->define_frame_cursor (f, cursor);
}

#endif	/* HAVE_WINDOW_SYSTEM */

/* Take proper action when mouse has moved to the mode or header line
   or marginal area AREA of window W, x-position X and y-position Y.
   X is relative to the start of the text display area of W, so the
   width of bitmap areas and scroll bars must be subtracted to get a
   position relative to the start of the mode line.  */

static void
note_mode_line_or_margin_highlight (Lisp_Object window, int x, int y,
				    enum window_part area)
{
  struct window *w = XWINDOW (window);
  struct frame *f = XFRAME (w->frame);
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
#ifdef HAVE_WINDOW_SYSTEM
  Display_Info *dpyinfo;
#endif
  Cursor cursor = No_Cursor;
  Lisp_Object pointer = Qnil;
  int dx, dy, width, height;
  EMACS_INT charpos;
  Lisp_Object string, object = Qnil;
  Lisp_Object pos, help;

  Lisp_Object mouse_face;
  int original_x_pixel = x;
  struct glyph * glyph = NULL, * row_start_glyph = NULL;
  struct glyph_row *row;

  if (area == ON_MODE_LINE || area == ON_HEADER_LINE)
    {
      int x0;
      struct glyph *end;

      /* Kludge alert: mode_line_string takes X/Y in pixels, but
	 returns them in row/column units!  */
      string = mode_line_string (w, area, &x, &y, &charpos,
				 &object, &dx, &dy, &width, &height);

      row = (area == ON_MODE_LINE
	     ? MATRIX_MODE_LINE_ROW (w->current_matrix)
	     : MATRIX_HEADER_LINE_ROW (w->current_matrix));

      /* Find the glyph under the mouse pointer.  */
      if (row->mode_line_p && row->enabled_p)
	{
	  glyph = row_start_glyph = row->glyphs[TEXT_AREA];
	  end = glyph + row->used[TEXT_AREA];

	  for (x0 = original_x_pixel;
	       glyph < end && x0 >= glyph->pixel_width;
	       ++glyph)
	    x0 -= glyph->pixel_width;

	  if (glyph >= end)
	    glyph = NULL;
	}
    }
  else
    {
      x -= WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH (w);
      /* Kludge alert: marginal_area_string takes X/Y in pixels, but
	 returns them in row/column units!  */
      string = marginal_area_string (w, area, &x, &y, &charpos,
				     &object, &dx, &dy, &width, &height);
    }

  help = Qnil;

#ifdef HAVE_WINDOW_SYSTEM
  if (IMAGEP (object))
    {
      Lisp_Object image_map, hotspot;
      if ((image_map = Fplist_get (XCDR (object), QCmap),
	   !NILP (image_map))
	  && (hotspot = find_hot_spot (image_map, dx, dy),
	      CONSP (hotspot))
	  && (hotspot = XCDR (hotspot), CONSP (hotspot)))
	{
	  Lisp_Object plist;

	  /* Could check XCAR (hotspot) to see if we enter/leave this hot-spot.
	     If so, we could look for mouse-enter, mouse-leave
	     properties in PLIST (and do something...).  */
	  hotspot = XCDR (hotspot);
	  if (CONSP (hotspot)
	      && (plist = XCAR (hotspot), CONSP (plist)))
	    {
	      pointer = Fplist_get (plist, Qpointer);
	      if (NILP (pointer))
		pointer = Qhand;
	      help = Fplist_get (plist, Qhelp_echo);
	      if (!NILP (help))
		{
		  help_echo_string = help;
		  /* Is this correct?  ++kfs */
		  XSETWINDOW (help_echo_window, w);
		  help_echo_object = w->buffer;
		  help_echo_pos = charpos;
		}
	    }
	}
      if (NILP (pointer))
	pointer = Fplist_get (XCDR (object), QCpointer);
    }
#endif	/* HAVE_WINDOW_SYSTEM */

  if (STRINGP (string))
    {
      pos = make_number (charpos);
      /* If we're on a string with `help-echo' text property, arrange
	 for the help to be displayed.  This is done by setting the
	 global variable help_echo_string to the help string.  */
      if (NILP (help))
	{
	  help = Fget_text_property (pos, Qhelp_echo, string);
	  if (!NILP (help))
	    {
	      help_echo_string = help;
	      XSETWINDOW (help_echo_window, w);
	      help_echo_object = string;
	      help_echo_pos = charpos;
	    }
	}

#ifdef HAVE_WINDOW_SYSTEM
      if (FRAME_WINDOW_P (f))
	{
	  dpyinfo = FRAME_X_DISPLAY_INFO (f);
	  cursor  = FRAME_X_OUTPUT (f)->nontext_cursor;
	  if (NILP (pointer))
	    pointer = Fget_text_property (pos, Qpointer, string);

	  /* Change the mouse pointer according to what is under X/Y.  */
	  if (NILP (pointer)
	      && ((area == ON_MODE_LINE) || (area == ON_HEADER_LINE)))
	    {
	      Lisp_Object map;
	      map = Fget_text_property (pos, Qlocal_map, string);
	      if (!KEYMAPP (map))
		map = Fget_text_property (pos, Qkeymap, string);
	      if (!KEYMAPP (map))
		cursor = dpyinfo->vertical_scroll_bar_cursor;
	    }
	}
#endif

     /* Change the mouse face according to what is under X/Y.  */
      mouse_face = Fget_text_property (pos, Qmouse_face, string);
      if (!NILP (mouse_face)
	  && ((area == ON_MODE_LINE) || (area == ON_HEADER_LINE))
	  && glyph)
	{
	  Lisp_Object b, e;

	  struct glyph * tmp_glyph;

	  int gpos;
	  int gseq_length;
	  int total_pixel_width;
	  EMACS_INT begpos, endpos, ignore;

	  int vpos, hpos;

	  b = Fprevious_single_property_change (make_number (charpos + 1),
						Qmouse_face, string, Qnil);
	  if (NILP (b))
	    begpos = 0;
	  else
	    begpos = XINT (b);

	  e = Fnext_single_property_change (pos, Qmouse_face, string, Qnil);
	  if (NILP (e))
	    endpos = SCHARS (string);
	  else
	    endpos = XINT (e);

	  /* Calculate the glyph position GPOS of GLYPH in the
	     displayed string, relative to the beginning of the
	     highlighted part of the string.

	     Note: GPOS is different from CHARPOS.  CHARPOS is the
	     position of GLYPH in the internal string object.  A mode
	     line string format has structures which are converted to
	     a flattened string by the Emacs Lisp interpreter.  The
	     internal string is an element of those structures.  The
	     displayed string is the flattened string.  */
	  tmp_glyph = row_start_glyph;
	  while (tmp_glyph < glyph
		 && (!(EQ (tmp_glyph->object, glyph->object)
		       && begpos <= tmp_glyph->charpos
		       && tmp_glyph->charpos < endpos)))
	    tmp_glyph++;
	  gpos = glyph - tmp_glyph;

	  /* Calculate the length GSEQ_LENGTH of the glyph sequence of
	     the highlighted part of the displayed string to which
	     GLYPH belongs.  Note: GSEQ_LENGTH is different from
	     SCHARS (STRING), because the latter returns the length of
	     the internal string.  */
	  for (tmp_glyph = row->glyphs[TEXT_AREA] + row->used[TEXT_AREA] - 1;
	       tmp_glyph > glyph
		 && (!(EQ (tmp_glyph->object, glyph->object)
		       && begpos <= tmp_glyph->charpos
		       && tmp_glyph->charpos < endpos));
	       tmp_glyph--)
	    ;
	  gseq_length = gpos + (tmp_glyph - glyph) + 1;

	  /* Calculate the total pixel width of all the glyphs between
	     the beginning of the highlighted area and GLYPH.  */
	  total_pixel_width = 0;
	  for (tmp_glyph = glyph - gpos; tmp_glyph != glyph; tmp_glyph++)
	    total_pixel_width += tmp_glyph->pixel_width;

	  /* Pre calculation of re-rendering position.  Note: X is in
	     column units here, after the call to mode_line_string or
	     marginal_area_string.  */
	  hpos = x - gpos;
	  vpos = (area == ON_MODE_LINE
		  ? (w->current_matrix)->nrows - 1
		  : 0);

	  /* If GLYPH's position is included in the region that is
	     already drawn in mouse face, we have nothing to do.  */
	  if ( EQ (window, hlinfo->mouse_face_window)
	       && (!row->reversed_p
		   ? (hlinfo->mouse_face_beg_col <= hpos
		      && hpos < hlinfo->mouse_face_end_col)
		   /* In R2L rows we swap BEG and END, see below.  */
		   : (hlinfo->mouse_face_end_col <= hpos
		      && hpos < hlinfo->mouse_face_beg_col))
	       && hlinfo->mouse_face_beg_row == vpos )
	    return;

	  if (clear_mouse_face (hlinfo))
	    cursor = No_Cursor;

	  if (!row->reversed_p)
	    {
	      hlinfo->mouse_face_beg_col = hpos;
	      hlinfo->mouse_face_beg_x   = original_x_pixel
					    - (total_pixel_width + dx);
	      hlinfo->mouse_face_end_col = hpos + gseq_length;
	      hlinfo->mouse_face_end_x   = 0;
	    }
	  else
	    {
	      /* In R2L rows, show_mouse_face expects BEG and END
		 coordinates to be swapped.  */
	      hlinfo->mouse_face_end_col = hpos;
	      hlinfo->mouse_face_end_x   = original_x_pixel
					    - (total_pixel_width + dx);
	      hlinfo->mouse_face_beg_col = hpos + gseq_length;
	      hlinfo->mouse_face_beg_x   = 0;
	    }

	  hlinfo->mouse_face_beg_row  = vpos;
	  hlinfo->mouse_face_end_row  = hlinfo->mouse_face_beg_row;
	  hlinfo->mouse_face_beg_y    = 0;
	  hlinfo->mouse_face_end_y    = 0;
	  hlinfo->mouse_face_past_end = 0;
	  hlinfo->mouse_face_window   = window;

	  hlinfo->mouse_face_face_id = face_at_string_position (w, string,
								charpos,
								0, 0, 0,
								&ignore,
								glyph->face_id,
								1);
	  show_mouse_face (hlinfo, DRAW_MOUSE_FACE);

	  if (NILP (pointer))
	    pointer = Qhand;
	}
      else if ((area == ON_MODE_LINE) || (area == ON_HEADER_LINE))
	clear_mouse_face (hlinfo);
    }
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    define_frame_cursor1 (f, cursor, pointer);
#endif
}


/* EXPORT:
   Take proper action when the mouse has moved to position X, Y on
   frame F as regards highlighting characters that have mouse-face
   properties.  Also de-highlighting chars where the mouse was before.
   X and Y can be negative or out of range.  */

void
note_mouse_highlight (struct frame *f, int x, int y)
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
  enum window_part part = ON_NOTHING;
  Lisp_Object window;
  struct window *w;
  Cursor cursor = No_Cursor;
  Lisp_Object pointer = Qnil;  /* Takes precedence over cursor!  */
  struct buffer *b;

  /* When a menu is active, don't highlight because this looks odd.  */
#if defined (USE_X_TOOLKIT) || defined (USE_GTK) || defined (HAVE_NS) || defined (MSDOS)
  if (popup_activated ())
    return;
#endif

  if (NILP (Vmouse_highlight)
      || !f->glyphs_initialized_p
      || f->pointer_invisible)
    return;

  hlinfo->mouse_face_mouse_x = x;
  hlinfo->mouse_face_mouse_y = y;
  hlinfo->mouse_face_mouse_frame = f;

  if (hlinfo->mouse_face_defer)
    return;

  if (gc_in_progress)
    {
      hlinfo->mouse_face_deferred_gc = 1;
      return;
    }

  /* Which window is that in?  */
  window = window_from_coordinates (f, x, y, &part, 1);

  /* If displaying active text in another window, clear that.  */
  if (! EQ (window, hlinfo->mouse_face_window)
      /* Also clear if we move out of text area in same window.  */
      || (!NILP (hlinfo->mouse_face_window)
	  && !NILP (window)
	  && part != ON_TEXT
	  && part != ON_MODE_LINE
	  && part != ON_HEADER_LINE))
    clear_mouse_face (hlinfo);

  /* Not on a window -> return.  */
  if (!WINDOWP (window))
    return;

  /* Reset help_echo_string. It will get recomputed below.  */
  help_echo_string = Qnil;

  /* Convert to window-relative pixel coordinates.  */
  w = XWINDOW (window);
  frame_to_window_pixel_xy (w, &x, &y);

#ifdef HAVE_WINDOW_SYSTEM
  /* Handle tool-bar window differently since it doesn't display a
     buffer.  */
  if (EQ (window, f->tool_bar_window))
    {
      note_tool_bar_highlight (f, x, y);
      return;
    }
#endif

  /* Mouse is on the mode, header line or margin?  */
  if (part == ON_MODE_LINE || part == ON_HEADER_LINE
      || part == ON_LEFT_MARGIN || part == ON_RIGHT_MARGIN)
    {
      note_mode_line_or_margin_highlight (window, x, y, part);
      return;
    }

#ifdef HAVE_WINDOW_SYSTEM
  if (part == ON_VERTICAL_BORDER)
    {
      cursor = FRAME_X_OUTPUT (f)->horizontal_drag_cursor;
      help_echo_string = build_string ("drag-mouse-1: resize");
    }
  else if (part == ON_LEFT_FRINGE || part == ON_RIGHT_FRINGE
	   || part == ON_SCROLL_BAR)
    cursor = FRAME_X_OUTPUT (f)->nontext_cursor;
  else
    cursor = FRAME_X_OUTPUT (f)->text_cursor;
#endif

  /* Are we in a window whose display is up to date?
     And verify the buffer's text has not changed.  */
  b = XBUFFER (w->buffer);
  if (part == ON_TEXT
      && EQ (w->window_end_valid, w->buffer)
      && XFASTINT (w->last_modified) == BUF_MODIFF (b)
      && XFASTINT (w->last_overlay_modified) == BUF_OVERLAY_MODIFF (b))
    {
      int hpos, vpos, dx, dy, area = LAST_AREA;
      EMACS_INT pos;
      struct glyph *glyph;
      Lisp_Object object;
      Lisp_Object mouse_face = Qnil, position;
      Lisp_Object *overlay_vec = NULL;
      ptrdiff_t i, noverlays;
      struct buffer *obuf;
      EMACS_INT obegv, ozv;
      int same_region;

      /* Find the glyph under X/Y.  */
      glyph = x_y_to_hpos_vpos (w, x, y, &hpos, &vpos, &dx, &dy, &area);

#ifdef HAVE_WINDOW_SYSTEM
      /* Look for :pointer property on image.  */
      if (glyph != NULL && glyph->type == IMAGE_GLYPH)
	{
	  struct image *img = IMAGE_FROM_ID (f, glyph->u.img_id);
	  if (img != NULL && IMAGEP (img->spec))
	    {
	      Lisp_Object image_map, hotspot;
	      if ((image_map = Fplist_get (XCDR (img->spec), QCmap),
		   !NILP (image_map))
		  && (hotspot = find_hot_spot (image_map,
					       glyph->slice.img.x + dx,
					       glyph->slice.img.y + dy),
		      CONSP (hotspot))
		  && (hotspot = XCDR (hotspot), CONSP (hotspot)))
		{
		  Lisp_Object plist;

		  /* Could check XCAR (hotspot) to see if we enter/leave
		     this hot-spot.
		     If so, we could look for mouse-enter, mouse-leave
		     properties in PLIST (and do something...).  */
		  hotspot = XCDR (hotspot);
		  if (CONSP (hotspot)
		      && (plist = XCAR (hotspot), CONSP (plist)))
		    {
		      pointer = Fplist_get (plist, Qpointer);
		      if (NILP (pointer))
			pointer = Qhand;
		      help_echo_string = Fplist_get (plist, Qhelp_echo);
		      if (!NILP (help_echo_string))
			{
			  help_echo_window = window;
			  help_echo_object = glyph->object;
			  help_echo_pos = glyph->charpos;
			}
		    }
		}
	      if (NILP (pointer))
		pointer = Fplist_get (XCDR (img->spec), QCpointer);
	    }
	}
#endif	/* HAVE_WINDOW_SYSTEM */

      /* Clear mouse face if X/Y not over text.  */
      if (glyph == NULL
	  || area != TEXT_AREA
	  || !MATRIX_ROW (w->current_matrix, vpos)->displays_text_p
	  /* Glyph's OBJECT is an integer for glyphs inserted by the
	     display engine for its internal purposes, like truncation
	     and continuation glyphs and blanks beyond the end of
	     line's text on text terminals.  If we are over such a
	     glyph, we are not over any text.  */
	  || INTEGERP (glyph->object)
	  /* R2L rows have a stretch glyph at their front, which
	     stands for no text, whereas L2R rows have no glyphs at
	     all beyond the end of text.  Treat such stretch glyphs
	     like we do with NULL glyphs in L2R rows.  */
	  || (MATRIX_ROW (w->current_matrix, vpos)->reversed_p
	      && glyph == MATRIX_ROW (w->current_matrix, vpos)->glyphs[TEXT_AREA]
	      && glyph->type == STRETCH_GLYPH
	      && glyph->avoid_cursor_p))
	{
	  if (clear_mouse_face (hlinfo))
	    cursor = No_Cursor;
#ifdef HAVE_WINDOW_SYSTEM
	  if (FRAME_WINDOW_P (f) && NILP (pointer))
	    {
	      if (area != TEXT_AREA)
		cursor = FRAME_X_OUTPUT (f)->nontext_cursor;
	      else
		pointer = Vvoid_text_area_pointer;
	    }
#endif
	  goto set_cursor;
	}

      pos = glyph->charpos;
      object = glyph->object;
      if (!STRINGP (object) && !BUFFERP (object))
	goto set_cursor;

      /* If we get an out-of-range value, return now; avoid an error.  */
      if (BUFFERP (object) && pos > BUF_Z (b))
	goto set_cursor;

      /* Make the window's buffer temporarily current for
	 overlays_at and compute_char_face.  */
      obuf = current_buffer;
      current_buffer = b;
      obegv = BEGV;
      ozv = ZV;
      BEGV = BEG;
      ZV = Z;

      /* Is this char mouse-active or does it have help-echo?  */
      position = make_number (pos);

      if (BUFFERP (object))
	{
	  /* Put all the overlays we want in a vector in overlay_vec.  */
	  GET_OVERLAYS_AT (pos, overlay_vec, noverlays, NULL, 0);
	  /* Sort overlays into increasing priority order.  */
	  noverlays = sort_overlays (overlay_vec, noverlays, w);
	}
      else
	noverlays = 0;

      same_region = coords_in_mouse_face_p (w, hpos, vpos);

      if (same_region)
	cursor = No_Cursor;

      /* Check mouse-face highlighting.  */
      if (! same_region
	  /* If there exists an overlay with mouse-face overlapping
	     the one we are currently highlighting, we have to
	     check if we enter the overlapping overlay, and then
	     highlight only that.  */
	  || (OVERLAYP (hlinfo->mouse_face_overlay)
	      && mouse_face_overlay_overlaps (hlinfo->mouse_face_overlay)))
	{
	  /* Find the highest priority overlay with a mouse-face.  */
	  Lisp_Object overlay = Qnil;
	  for (i = noverlays - 1; i >= 0 && NILP (overlay); --i)
	    {
	      mouse_face = Foverlay_get (overlay_vec[i], Qmouse_face);
	      if (!NILP (mouse_face))
		overlay = overlay_vec[i];
	    }

	  /* If we're highlighting the same overlay as before, there's
	     no need to do that again.  */
	  if (!NILP (overlay) && EQ (overlay, hlinfo->mouse_face_overlay))
	    goto check_help_echo;
	  hlinfo->mouse_face_overlay = overlay;

	  /* Clear the display of the old active region, if any.  */
	  if (clear_mouse_face (hlinfo))
	    cursor = No_Cursor;

	  /* If no overlay applies, get a text property.  */
	  if (NILP (overlay))
	    mouse_face = Fget_text_property (position, Qmouse_face, object);

	  /* Next, compute the bounds of the mouse highlighting and
	     display it.  */
	  if (!NILP (mouse_face) && STRINGP (object))
	    {
	      /* The mouse-highlighting comes from a display string
		 with a mouse-face.  */
	      Lisp_Object s, e;
	      EMACS_INT ignore;

	      s = Fprevious_single_property_change
		(make_number (pos + 1), Qmouse_face, object, Qnil);
	      e = Fnext_single_property_change
		(position, Qmouse_face, object, Qnil);
	      if (NILP (s))
		s = make_number (0);
	      if (NILP (e))
		e = make_number (SCHARS (object) - 1);
	      mouse_face_from_string_pos (w, hlinfo, object,
					  XINT (s), XINT (e));
	      hlinfo->mouse_face_past_end = 0;
	      hlinfo->mouse_face_window = window;
	      hlinfo->mouse_face_face_id
		= face_at_string_position (w, object, pos, 0, 0, 0, &ignore,
					   glyph->face_id, 1);
	      show_mouse_face (hlinfo, DRAW_MOUSE_FACE);
	      cursor = No_Cursor;
	    }
	  else
	    {
	      /* The mouse-highlighting, if any, comes from an overlay
		 or text property in the buffer.  */
	      Lisp_Object buffer IF_LINT (= Qnil);
	      Lisp_Object disp_string IF_LINT (= Qnil);

	      if (STRINGP (object))
		{
		  /* If we are on a display string with no mouse-face,
		     check if the text under it has one.  */
		  struct glyph_row *r = MATRIX_ROW (w->current_matrix, vpos);
		  EMACS_INT start = MATRIX_ROW_START_CHARPOS (r);
		  pos = string_buffer_position (object, start);
		  if (pos > 0)
		    {
		      mouse_face = get_char_property_and_overlay
			(make_number (pos), Qmouse_face, w->buffer, &overlay);
		      buffer = w->buffer;
		      disp_string = object;
		    }
		}
	      else
		{
		  buffer = object;
		  disp_string = Qnil;
		}

	      if (!NILP (mouse_face))
		{
		  Lisp_Object before, after;
		  Lisp_Object before_string, after_string;
		  /* To correctly find the limits of mouse highlight
		     in a bidi-reordered buffer, we must not use the
		     optimization of limiting the search in
		     previous-single-property-change and
		     next-single-property-change, because
		     rows_from_pos_range needs the real start and end
		     positions to DTRT in this case.  That's because
		     the first row visible in a window does not
		     necessarily display the character whose position
		     is the smallest.  */
		  Lisp_Object lim1 =
		    NILP (BVAR (XBUFFER (buffer), bidi_display_reordering))
		    ? Fmarker_position (w->start)
		    : Qnil;
		  Lisp_Object lim2 =
		    NILP (BVAR (XBUFFER (buffer), bidi_display_reordering))
		    ? make_number (BUF_Z (XBUFFER (buffer))
				   - XFASTINT (w->window_end_pos))
		    : Qnil;

		  if (NILP (overlay))
		    {
		      /* Handle the text property case.  */
		      before = Fprevious_single_property_change
			(make_number (pos + 1), Qmouse_face, buffer, lim1);
		      after = Fnext_single_property_change
			(make_number (pos), Qmouse_face, buffer, lim2);
		      before_string = after_string = Qnil;
		    }
		  else
		    {
		      /* Handle the overlay case.  */
		      before = Foverlay_start (overlay);
		      after = Foverlay_end (overlay);
		      before_string = Foverlay_get (overlay, Qbefore_string);
		      after_string = Foverlay_get (overlay, Qafter_string);

		      if (!STRINGP (before_string)) before_string = Qnil;
		      if (!STRINGP (after_string))  after_string = Qnil;
		    }

		  mouse_face_from_buffer_pos (window, hlinfo, pos,
					      NILP (before)
					      ? 1
					      : XFASTINT (before),
					      NILP (after)
					      ? BUF_Z (XBUFFER (buffer))
					      : XFASTINT (after),
					      before_string, after_string,
					      disp_string);
		  cursor = No_Cursor;
		}
	    }
	}

    check_help_echo:

      /* Look for a `help-echo' property.  */
      if (NILP (help_echo_string)) {
	Lisp_Object help, overlay;

	/* Check overlays first.  */
	help = overlay = Qnil;
	for (i = noverlays - 1; i >= 0 && NILP (help); --i)
	  {
	    overlay = overlay_vec[i];
	    help = Foverlay_get (overlay, Qhelp_echo);
	  }

	if (!NILP (help))
	  {
	    help_echo_string = help;
	    help_echo_window = window;
	    help_echo_object = overlay;
	    help_echo_pos = pos;
	  }
	else
	  {
	    Lisp_Object obj = glyph->object;
	    EMACS_INT charpos = glyph->charpos;

	    /* Try text properties.  */
	    if (STRINGP (obj)
		&& charpos >= 0
		&& charpos < SCHARS (obj))
	      {
		help = Fget_text_property (make_number (charpos),
					   Qhelp_echo, obj);
		if (NILP (help))
		  {
		    /* If the string itself doesn't specify a help-echo,
		       see if the buffer text ``under'' it does.  */
		    struct glyph_row *r
		      = MATRIX_ROW (w->current_matrix, vpos);
		    EMACS_INT start = MATRIX_ROW_START_CHARPOS (r);
		    EMACS_INT p = string_buffer_position (obj, start);
		    if (p > 0)
		      {
			help = Fget_char_property (make_number (p),
						   Qhelp_echo, w->buffer);
			if (!NILP (help))
			  {
			    charpos = p;
			    obj = w->buffer;
			  }
		      }
		  }
	      }
	    else if (BUFFERP (obj)
		     && charpos >= BEGV
		     && charpos < ZV)
	      help = Fget_text_property (make_number (charpos), Qhelp_echo,
					 obj);

	    if (!NILP (help))
	      {
		help_echo_string = help;
		help_echo_window = window;
		help_echo_object = obj;
		help_echo_pos = charpos;
	      }
	  }
      }

#ifdef HAVE_WINDOW_SYSTEM
      /* Look for a `pointer' property.  */
      if (FRAME_WINDOW_P (f) && NILP (pointer))
	{
	  /* Check overlays first.  */
	  for (i = noverlays - 1; i >= 0 && NILP (pointer); --i)
	    pointer = Foverlay_get (overlay_vec[i], Qpointer);

	  if (NILP (pointer))
	    {
	      Lisp_Object obj = glyph->object;
	      EMACS_INT charpos = glyph->charpos;

	      /* Try text properties.  */
	      if (STRINGP (obj)
		  && charpos >= 0
		  && charpos < SCHARS (obj))
		{
		  pointer = Fget_text_property (make_number (charpos),
						Qpointer, obj);
		  if (NILP (pointer))
		    {
		      /* If the string itself doesn't specify a pointer,
			 see if the buffer text ``under'' it does.  */
		      struct glyph_row *r
			= MATRIX_ROW (w->current_matrix, vpos);
		      EMACS_INT start = MATRIX_ROW_START_CHARPOS (r);
		      EMACS_INT p = string_buffer_position (obj, start);
		      if (p > 0)
			pointer = Fget_char_property (make_number (p),
						      Qpointer, w->buffer);
		    }
		}
	      else if (BUFFERP (obj)
		       && charpos >= BEGV
		       && charpos < ZV)
		pointer = Fget_text_property (make_number (charpos),
					      Qpointer, obj);
	    }
	}
#endif	/* HAVE_WINDOW_SYSTEM */

      BEGV = obegv;
      ZV = ozv;
      current_buffer = obuf;
    }

 set_cursor:

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    define_frame_cursor1 (f, cursor, pointer);
#else
  /* This is here to prevent a compiler error, about "label at end of
     compound statement".  */
  return;
#endif
}


/* EXPORT for RIF:
   Clear any mouse-face on window W.  This function is part of the
   redisplay interface, and is called from try_window_id and similar
   functions to ensure the mouse-highlight is off.  */

void
x_clear_window_mouse_face (struct window *w)
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (XFRAME (w->frame));
  Lisp_Object window;

  BLOCK_INPUT;
  XSETWINDOW (window, w);
  if (EQ (window, hlinfo->mouse_face_window))
    clear_mouse_face (hlinfo);
  UNBLOCK_INPUT;
}


/* EXPORT:
   Just discard the mouse face information for frame F, if any.
   This is used when the size of F is changed.  */

void
cancel_mouse_face (struct frame *f)
{
  Lisp_Object window;
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);

  window = hlinfo->mouse_face_window;
  if (! NILP (window) && XFRAME (XWINDOW (window)->frame) == f)
    {
      hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
      hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
      hlinfo->mouse_face_window = Qnil;
    }
}



/***********************************************************************
			   Exposure Events
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

/* Redraw the part of glyph row area AREA of glyph row ROW on window W
   which intersects rectangle R.  R is in window-relative coordinates.  */

static void
expose_area (struct window *w, struct glyph_row *row, XRectangle *r,
	     enum glyph_row_area area)
{
  struct glyph *first = row->glyphs[area];
  struct glyph *end = row->glyphs[area] + row->used[area];
  struct glyph *last;
  int first_x, start_x, x;

  if (area == TEXT_AREA && row->fill_line_p)
    /* If row extends face to end of line write the whole line.  */
    draw_glyphs (w, 0, row, area,
		 0, row->used[area],
		 DRAW_NORMAL_TEXT, 0);
  else
    {
      /* Set START_X to the window-relative start position for drawing glyphs of
	 AREA.  The first glyph of the text area can be partially visible.
	 The first glyphs of other areas cannot.  */
      start_x = window_box_left_offset (w, area);
      x = start_x;
      if (area == TEXT_AREA)
	x += row->x;

      /* Find the first glyph that must be redrawn.  */
      while (first < end
	     && x + first->pixel_width < r->x)
	{
	  x += first->pixel_width;
	  ++first;
	}

      /* Find the last one.  */
      last = first;
      first_x = x;
      while (last < end
	     && x < r->x + r->width)
	{
	  x += last->pixel_width;
	  ++last;
	}

      /* Repaint.  */
      if (last > first)
	draw_glyphs (w, first_x - start_x, row, area,
		     first - row->glyphs[area], last - row->glyphs[area],
		     DRAW_NORMAL_TEXT, 0);
    }
}


/* Redraw the parts of the glyph row ROW on window W intersecting
   rectangle R.  R is in window-relative coordinates.  Value is
   non-zero if mouse-face was overwritten.  */

static int
expose_line (struct window *w, struct glyph_row *row, XRectangle *r)
{
  xassert (row->enabled_p);

  if (row->mode_line_p || w->pseudo_window_p)
    draw_glyphs (w, 0, row, TEXT_AREA,
		 0, row->used[TEXT_AREA],
		 DRAW_NORMAL_TEXT, 0);
  else
    {
      if (row->used[LEFT_MARGIN_AREA])
	expose_area (w, row, r, LEFT_MARGIN_AREA);
      if (row->used[TEXT_AREA])
	expose_area (w, row, r, TEXT_AREA);
      if (row->used[RIGHT_MARGIN_AREA])
	expose_area (w, row, r, RIGHT_MARGIN_AREA);
      draw_row_fringe_bitmaps (w, row);
    }

  return row->mouse_face_p;
}


/* Redraw those parts of glyphs rows during expose event handling that
   overlap other rows.  Redrawing of an exposed line writes over parts
   of lines overlapping that exposed line; this function fixes that.

   W is the window being exposed.  FIRST_OVERLAPPING_ROW is the first
   row in W's current matrix that is exposed and overlaps other rows.
   LAST_OVERLAPPING_ROW is the last such row.  */

static void
expose_overlaps (struct window *w,
		 struct glyph_row *first_overlapping_row,
		 struct glyph_row *last_overlapping_row,
		 XRectangle *r)
{
  struct glyph_row *row;

  for (row = first_overlapping_row; row <= last_overlapping_row; ++row)
    if (row->overlapping_p)
      {
	xassert (row->enabled_p && !row->mode_line_p);

	row->clip = r;
	if (row->used[LEFT_MARGIN_AREA])
	  x_fix_overlapping_area (w, row, LEFT_MARGIN_AREA, OVERLAPS_BOTH);

	if (row->used[TEXT_AREA])
	  x_fix_overlapping_area (w, row, TEXT_AREA, OVERLAPS_BOTH);

	if (row->used[RIGHT_MARGIN_AREA])
	  x_fix_overlapping_area (w, row, RIGHT_MARGIN_AREA, OVERLAPS_BOTH);
	row->clip = NULL;
      }
}


/* Return non-zero if W's cursor intersects rectangle R.  */

static int
phys_cursor_in_rect_p (struct window *w, XRectangle *r)
{
  XRectangle cr, result;
  struct glyph *cursor_glyph;
  struct glyph_row *row;

  if (w->phys_cursor.vpos >= 0
      && w->phys_cursor.vpos < w->current_matrix->nrows
      && (row = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos),
	  row->enabled_p)
      && row->cursor_in_fringe_p)
    {
      /* Cursor is in the fringe.  */
      cr.x = window_box_right_offset (w,
				      (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
				       ? RIGHT_MARGIN_AREA
				       : TEXT_AREA));
      cr.y = row->y;
      cr.width = WINDOW_RIGHT_FRINGE_WIDTH (w);
      cr.height = row->height;
      return x_intersect_rectangles (&cr, r, &result);
    }

  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph)
    {
      /* r is relative to W's box, but w->phys_cursor.x is relative
	 to left edge of W's TEXT area.  Adjust it.  */
      cr.x = window_box_left_offset (w, TEXT_AREA) + w->phys_cursor.x;
      cr.y = w->phys_cursor.y;
      cr.width = cursor_glyph->pixel_width;
      cr.height = w->phys_cursor_height;
      /* ++KFS: W32 version used W32-specific IntersectRect here, but
	 I assume the effect is the same -- and this is portable.  */
      return x_intersect_rectangles (&cr, r, &result);
    }
  /* If we don't understand the format, pretend we're not in the hot-spot.  */
  return 0;
}


/* EXPORT:
   Draw a vertical window border to the right of window W if W doesn't
   have vertical scroll bars.  */

void
x_draw_vertical_border (struct window *w)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));

  /* We could do better, if we knew what type of scroll-bar the adjacent
     windows (on either side) have...  But we don't :-(
     However, I think this works ok.  ++KFS 2003-04-25 */

  /* Redraw borders between horizontally adjacent windows.  Don't
     do it for frames with vertical scroll bars because either the
     right scroll bar of a window, or the left scroll bar of its
     neighbor will suffice as a border.  */
  if (FRAME_HAS_VERTICAL_SCROLL_BARS (XFRAME (w->frame)))
    return;

  if (!WINDOW_RIGHTMOST_P (w)
      && !WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w))
    {
      int x0, x1, y0, y1;

      window_box_edges (w, -1, &x0, &y0, &x1, &y1);
      y1 -= 1;

      if (WINDOW_LEFT_FRINGE_WIDTH (w) == 0)
        x1 -= 1;

      FRAME_RIF (f)->draw_vertical_window_border (w, x1, y0, y1);
    }
  else if (!WINDOW_LEFTMOST_P (w)
	   && !WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w))
    {
      int x0, x1, y0, y1;

      window_box_edges (w, -1, &x0, &y0, &x1, &y1);
      y1 -= 1;

      if (WINDOW_LEFT_FRINGE_WIDTH (w) == 0)
        x0 -= 1;

      FRAME_RIF (f)->draw_vertical_window_border (w, x0, y0, y1);
    }
}


/* Redraw the part of window W intersection rectangle FR.  Pixel
   coordinates in FR are frame-relative.  Call this function with
   input blocked.  Value is non-zero if the exposure overwrites
   mouse-face.  */

static int
expose_window (struct window *w, XRectangle *fr)
{
  struct frame *f = XFRAME (w->frame);
  XRectangle wr, r;
  int mouse_face_overwritten_p = 0;

  /* If window is not yet fully initialized, do nothing.  This can
     happen when toolkit scroll bars are used and a window is split.
     Reconfiguring the scroll bar will generate an expose for a newly
     created window.  */
  if (w->current_matrix == NULL)
    return 0;

  /* When we're currently updating the window, display and current
     matrix usually don't agree.  Arrange for a thorough display
     later.  */
  if (w == updated_window)
    {
      SET_FRAME_GARBAGED (f);
      return 0;
    }

  /* Frame-relative pixel rectangle of W.  */
  wr.x = WINDOW_LEFT_EDGE_X (w);
  wr.y = WINDOW_TOP_EDGE_Y (w);
  wr.width = WINDOW_TOTAL_WIDTH (w);
  wr.height = WINDOW_TOTAL_HEIGHT (w);

  if (x_intersect_rectangles (fr, &wr, &r))
    {
      int yb = window_text_bottom_y (w);
      struct glyph_row *row;
      int cursor_cleared_p, phys_cursor_on_p;
      struct glyph_row *first_overlapping_row, *last_overlapping_row;

      TRACE ((stderr, "expose_window (%d, %d, %d, %d)\n",
	      r.x, r.y, r.width, r.height));

      /* Convert to window coordinates.  */
      r.x -= WINDOW_LEFT_EDGE_X (w);
      r.y -= WINDOW_TOP_EDGE_Y (w);

      /* Turn off the cursor.  */
      if (!w->pseudo_window_p
	  && phys_cursor_in_rect_p (w, &r))
	{
	  x_clear_cursor (w);
	  cursor_cleared_p = 1;
	}
      else
	cursor_cleared_p = 0;

      /* If the row containing the cursor extends face to end of line,
	 then expose_area might overwrite the cursor outside the
	 rectangle and thus notice_overwritten_cursor might clear
	 w->phys_cursor_on_p.  We remember the original value and
	 check later if it is changed.  */
      phys_cursor_on_p = w->phys_cursor_on_p;

      /* Update lines intersecting rectangle R.  */
      first_overlapping_row = last_overlapping_row = NULL;
      for (row = w->current_matrix->rows;
	   row->enabled_p;
	   ++row)
	{
	  int y0 = row->y;
	  int y1 = MATRIX_ROW_BOTTOM_Y (row);

	  if ((y0 >= r.y && y0 < r.y + r.height)
	      || (y1 > r.y && y1 < r.y + r.height)
	      || (r.y >= y0 && r.y < y1)
	      || (r.y + r.height > y0 && r.y + r.height < y1))
	    {
	      /* A header line may be overlapping, but there is no need
		 to fix overlapping areas for them.  KFS 2005-02-12 */
	      if (row->overlapping_p && !row->mode_line_p)
		{
		  if (first_overlapping_row == NULL)
		    first_overlapping_row = row;
		  last_overlapping_row = row;
		}

	      row->clip = fr;
	      if (expose_line (w, row, &r))
		mouse_face_overwritten_p = 1;
	      row->clip = NULL;
	    }
	  else if (row->overlapping_p)
	    {
	      /* We must redraw a row overlapping the exposed area.  */
	      if (y0 < r.y
		  ? y0 + row->phys_height > r.y
		  : y0 + row->ascent - row->phys_ascent < r.y +r.height)
		{
		  if (first_overlapping_row == NULL)
		    first_overlapping_row = row;
		  last_overlapping_row = row;
		}
	    }

	  if (y1 >= yb)
	    break;
	}

      /* Display the mode line if there is one.  */
      if (WINDOW_WANTS_MODELINE_P (w)
	  && (row = MATRIX_MODE_LINE_ROW (w->current_matrix),
	      row->enabled_p)
	  && row->y < r.y + r.height)
	{
	  if (expose_line (w, row, &r))
	    mouse_face_overwritten_p = 1;
	}

      if (!w->pseudo_window_p)
	{
	  /* Fix the display of overlapping rows.  */
	  if (first_overlapping_row)
	    expose_overlaps (w, first_overlapping_row, last_overlapping_row,
			     fr);

	  /* Draw border between windows.  */
	  x_draw_vertical_border (w);

	  /* Turn the cursor on again.  */
	  if (cursor_cleared_p
	      || (phys_cursor_on_p && !w->phys_cursor_on_p))
	    update_window_cursor (w, 1);
	}
    }

  return mouse_face_overwritten_p;
}



/* Redraw (parts) of all windows in the window tree rooted at W that
   intersect R.  R contains frame pixel coordinates.  Value is
   non-zero if the exposure overwrites mouse-face.  */

static int
expose_window_tree (struct window *w, XRectangle *r)
{
  struct frame *f = XFRAME (w->frame);
  int mouse_face_overwritten_p = 0;

  while (w && !FRAME_GARBAGED_P (f))
    {
      if (!NILP (w->hchild))
	mouse_face_overwritten_p
	  |= expose_window_tree (XWINDOW (w->hchild), r);
      else if (!NILP (w->vchild))
	mouse_face_overwritten_p
	  |= expose_window_tree (XWINDOW (w->vchild), r);
      else
	mouse_face_overwritten_p |= expose_window (w, r);

      w = NILP (w->next) ? NULL : XWINDOW (w->next);
    }

  return mouse_face_overwritten_p;
}


/* EXPORT:
   Redisplay an exposed area of frame F.  X and Y are the upper-left
   corner of the exposed rectangle.  W and H are width and height of
   the exposed area.  All are pixel values.  W or H zero means redraw
   the entire frame.  */

void
expose_frame (struct frame *f, int x, int y, int w, int h)
{
  XRectangle r;
  int mouse_face_overwritten_p = 0;

  TRACE ((stderr, "expose_frame "));

  /* No need to redraw if frame will be redrawn soon.  */
  if (FRAME_GARBAGED_P (f))
    {
      TRACE ((stderr, " garbaged\n"));
      return;
    }

  /* If basic faces haven't been realized yet, there is no point in
     trying to redraw anything.  This can happen when we get an expose
     event while Emacs is starting, e.g. by moving another window.  */
  if (FRAME_FACE_CACHE (f) == NULL
      || FRAME_FACE_CACHE (f)->used < BASIC_FACE_ID_SENTINEL)
    {
      TRACE ((stderr, " no faces\n"));
      return;
    }

  if (w == 0 || h == 0)
    {
      r.x = r.y = 0;
      r.width = FRAME_COLUMN_WIDTH (f) * FRAME_COLS (f);
      r.height = FRAME_LINE_HEIGHT (f) * FRAME_LINES (f);
    }
  else
    {
      r.x = x;
      r.y = y;
      r.width = w;
      r.height = h;
    }

  TRACE ((stderr, "(%d, %d, %d, %d)\n", r.x, r.y, r.width, r.height));
  mouse_face_overwritten_p = expose_window_tree (XWINDOW (f->root_window), &r);

  if (WINDOWP (f->tool_bar_window))
    mouse_face_overwritten_p
      |= expose_window (XWINDOW (f->tool_bar_window), &r);

#ifdef HAVE_X_WINDOWS
#ifndef MSDOS
#ifndef USE_X_TOOLKIT
  if (WINDOWP (f->menu_bar_window))
    mouse_face_overwritten_p
      |= expose_window (XWINDOW (f->menu_bar_window), &r);
#endif /* not USE_X_TOOLKIT */
#endif
#endif

  /* Some window managers support a focus-follows-mouse style with
     delayed raising of frames.  Imagine a partially obscured frame,
     and moving the mouse into partially obscured mouse-face on that
     frame.  The visible part of the mouse-face will be highlighted,
     then the WM raises the obscured frame.  With at least one WM, KDE
     2.1, Emacs is not getting any event for the raising of the frame
     (even tried with SubstructureRedirectMask), only Expose events.
     These expose events will draw text normally, i.e. not
     highlighted.  Which means we must redo the highlight here.
     Subsume it under ``we love X''.  --gerd 2001-08-15  */
  /* Included in Windows version because Windows most likely does not
     do the right thing if any third party tool offers
     focus-follows-mouse with delayed raise.  --jason 2001-10-12  */
  if (mouse_face_overwritten_p && !FRAME_GARBAGED_P (f))
    {
      Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
      if (f == hlinfo->mouse_face_mouse_frame)
	{
	  int mouse_x = hlinfo->mouse_face_mouse_x;
	  int mouse_y = hlinfo->mouse_face_mouse_y;
	  clear_mouse_face (hlinfo);
	  note_mouse_highlight (f, mouse_x, mouse_y);
	}
    }
}


/* EXPORT:
   Determine the intersection of two rectangles R1 and R2.  Return
   the intersection in *RESULT.  Value is non-zero if RESULT is not
   empty.  */

int
x_intersect_rectangles (XRectangle *r1, XRectangle *r2, XRectangle *result)
{
  XRectangle *left, *right;
  XRectangle *upper, *lower;
  int intersection_p = 0;

  /* Rearrange so that R1 is the left-most rectangle.  */
  if (r1->x < r2->x)
    left = r1, right = r2;
  else
    left = r2, right = r1;

  /* X0 of the intersection is right.x0, if this is inside R1,
     otherwise there is no intersection.  */
  if (right->x <= left->x + left->width)
    {
      result->x = right->x;

      /* The right end of the intersection is the minimum of
	 the right ends of left and right.  */
      result->width = (min (left->x + left->width, right->x + right->width)
		       - result->x);

      /* Same game for Y.  */
      if (r1->y < r2->y)
	upper = r1, lower = r2;
      else
	upper = r2, lower = r1;

      /* The upper end of the intersection is lower.y0, if this is inside
	 of upper.  Otherwise, there is no intersection.  */
      if (lower->y <= upper->y + upper->height)
	{
	  result->y = lower->y;

	  /* The lower end of the intersection is the minimum of the lower
	     ends of upper and lower.  */
	  result->height = (min (lower->y + lower->height,
				 upper->y + upper->height)
			    - result->y);
	  intersection_p = 1;
	}
    }

  return intersection_p;
}

#endif /* HAVE_WINDOW_SYSTEM */


/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_xdisp (void)
{
  Vwith_echo_area_save_vector = Qnil;
  staticpro (&Vwith_echo_area_save_vector);

  Vmessage_stack = Qnil;
  staticpro (&Vmessage_stack);

  DEFSYM (Qinhibit_redisplay, "inhibit-redisplay");

  message_dolog_marker1 = Fmake_marker ();
  staticpro (&message_dolog_marker1);
  message_dolog_marker2 = Fmake_marker ();
  staticpro (&message_dolog_marker2);
  message_dolog_marker3 = Fmake_marker ();
  staticpro (&message_dolog_marker3);

#if GLYPH_DEBUG
  defsubr (&Sdump_frame_glyph_matrix);
  defsubr (&Sdump_glyph_matrix);
  defsubr (&Sdump_glyph_row);
  defsubr (&Sdump_tool_bar_row);
  defsubr (&Strace_redisplay);
  defsubr (&Strace_to_stderr);
#endif
#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Stool_bar_lines_needed);
  defsubr (&Slookup_image_map);
#endif
  defsubr (&Sformat_mode_line);
  defsubr (&Sinvisible_p);
  defsubr (&Scurrent_bidi_paragraph_direction);

  DEFSYM (Qmenu_bar_update_hook, "menu-bar-update-hook");
  DEFSYM (Qoverriding_terminal_local_map, "overriding-terminal-local-map");
  DEFSYM (Qoverriding_local_map, "overriding-local-map");
  DEFSYM (Qwindow_scroll_functions, "window-scroll-functions");
  DEFSYM (Qwindow_text_change_functions, "window-text-change-functions");
  DEFSYM (Qredisplay_end_trigger_functions, "redisplay-end-trigger-functions");
  DEFSYM (Qinhibit_point_motion_hooks, "inhibit-point-motion-hooks");
  DEFSYM (Qeval, "eval");
  DEFSYM (QCdata, ":data");
  DEFSYM (Qdisplay, "display");
  DEFSYM (Qspace_width, "space-width");
  DEFSYM (Qraise, "raise");
  DEFSYM (Qslice, "slice");
  DEFSYM (Qspace, "space");
  DEFSYM (Qmargin, "margin");
  DEFSYM (Qpointer, "pointer");
  DEFSYM (Qleft_margin, "left-margin");
  DEFSYM (Qright_margin, "right-margin");
  DEFSYM (Qcenter, "center");
  DEFSYM (Qline_height, "line-height");
  DEFSYM (QCalign_to, ":align-to");
  DEFSYM (QCrelative_width, ":relative-width");
  DEFSYM (QCrelative_height, ":relative-height");
  DEFSYM (QCeval, ":eval");
  DEFSYM (QCpropertize, ":propertize");
  DEFSYM (QCfile, ":file");
  DEFSYM (Qfontified, "fontified");
  DEFSYM (Qfontification_functions, "fontification-functions");
  DEFSYM (Qtrailing_whitespace, "trailing-whitespace");
  DEFSYM (Qescape_glyph, "escape-glyph");
  DEFSYM (Qnobreak_space, "nobreak-space");
  DEFSYM (Qimage, "image");
  DEFSYM (Qtext, "text");
  DEFSYM (Qboth, "both");
  DEFSYM (Qboth_horiz, "both-horiz");
  DEFSYM (Qtext_image_horiz, "text-image-horiz");
  DEFSYM (QCmap, ":map");
  DEFSYM (QCpointer, ":pointer");
  DEFSYM (Qrect, "rect");
  DEFSYM (Qcircle, "circle");
  DEFSYM (Qpoly, "poly");
  DEFSYM (Qmessage_truncate_lines, "message-truncate-lines");
  DEFSYM (Qgrow_only, "grow-only");
  DEFSYM (Qinhibit_menubar_update, "inhibit-menubar-update");
  DEFSYM (Qinhibit_eval_during_redisplay, "inhibit-eval-during-redisplay");
  DEFSYM (Qposition, "position");
  DEFSYM (Qbuffer_position, "buffer-position");
  DEFSYM (Qobject, "object");
  DEFSYM (Qbar, "bar");
  DEFSYM (Qhbar, "hbar");
  DEFSYM (Qbox, "box");
  DEFSYM (Qhollow, "hollow");
  DEFSYM (Qhand, "hand");
  DEFSYM (Qarrow, "arrow");
  DEFSYM (Qinhibit_free_realized_faces, "inhibit-free-realized-faces");

  list_of_error = Fcons (Fcons (intern_c_string ("error"),
				Fcons (intern_c_string ("void-variable"), Qnil)),
			 Qnil);
  staticpro (&list_of_error);

  DEFSYM (Qlast_arrow_position, "last-arrow-position");
  DEFSYM (Qlast_arrow_string, "last-arrow-string");
  DEFSYM (Qoverlay_arrow_string, "overlay-arrow-string");
  DEFSYM (Qoverlay_arrow_bitmap, "overlay-arrow-bitmap");

  echo_buffer[0] = echo_buffer[1] = Qnil;
  staticpro (&echo_buffer[0]);
  staticpro (&echo_buffer[1]);

  echo_area_buffer[0] = echo_area_buffer[1] = Qnil;
  staticpro (&echo_area_buffer[0]);
  staticpro (&echo_area_buffer[1]);

  Vmessages_buffer_name = make_pure_c_string ("*Messages*");
  staticpro (&Vmessages_buffer_name);

  mode_line_proptrans_alist = Qnil;
  staticpro (&mode_line_proptrans_alist);
  mode_line_string_list = Qnil;
  staticpro (&mode_line_string_list);
  mode_line_string_face = Qnil;
  staticpro (&mode_line_string_face);
  mode_line_string_face_prop = Qnil;
  staticpro (&mode_line_string_face_prop);
  Vmode_line_unwind_vector = Qnil;
  staticpro (&Vmode_line_unwind_vector);

  help_echo_string = Qnil;
  staticpro (&help_echo_string);
  help_echo_object = Qnil;
  staticpro (&help_echo_object);
  help_echo_window = Qnil;
  staticpro (&help_echo_window);
  previous_help_echo_string = Qnil;
  staticpro (&previous_help_echo_string);
  help_echo_pos = -1;

  DEFSYM (Qright_to_left, "right-to-left");
  DEFSYM (Qleft_to_right, "left-to-right");

#ifdef HAVE_WINDOW_SYSTEM
  DEFVAR_BOOL ("x-stretch-cursor", x_stretch_cursor_p,
    doc: /* *Non-nil means draw block cursor as wide as the glyph under it.
For example, if a block cursor is over a tab, it will be drawn as
wide as that tab on the display.  */);
  x_stretch_cursor_p = 0;
#endif

  DEFVAR_LISP ("show-trailing-whitespace", Vshow_trailing_whitespace,
    doc: /* *Non-nil means highlight trailing whitespace.
The face used for trailing whitespace is `trailing-whitespace'.  */);
  Vshow_trailing_whitespace = Qnil;

  DEFVAR_LISP ("nobreak-char-display", Vnobreak_char_display,
    doc: /* Control highlighting of non-ASCII space and hyphen chars.
If the value is t, Emacs highlights non-ASCII chars which have the
same appearance as an ASCII space or hyphen, using the `nobreak-space'
or `escape-glyph' face respectively.

U+00A0 (no-break space), U+00AD (soft hyphen), U+2010 (hyphen), and
U+2011 (non-breaking hyphen) are affected.

Any other non-nil value means to display these characters as a escape
glyph followed by an ordinary space or hyphen.

A value of nil means no special handling of these characters.  */);
  Vnobreak_char_display = Qt;

  DEFVAR_LISP ("void-text-area-pointer", Vvoid_text_area_pointer,
    doc: /* *The pointer shape to show in void text areas.
A value of nil means to show the text pointer.  Other options are `arrow',
`text', `hand', `vdrag', `hdrag', `modeline', and `hourglass'.  */);
  Vvoid_text_area_pointer = Qarrow;

  DEFVAR_LISP ("inhibit-redisplay", Vinhibit_redisplay,
    doc: /* Non-nil means don't actually do any redisplay.
This is used for internal purposes.  */);
  Vinhibit_redisplay = Qnil;

  DEFVAR_LISP ("global-mode-string", Vglobal_mode_string,
    doc: /* String (or mode line construct) included (normally) in `mode-line-format'.  */);
  Vglobal_mode_string = Qnil;

  DEFVAR_LISP ("overlay-arrow-position", Voverlay_arrow_position,
    doc: /* Marker for where to display an arrow on top of the buffer text.
This must be the beginning of a line in order to work.
See also `overlay-arrow-string'.  */);
  Voverlay_arrow_position = Qnil;

  DEFVAR_LISP ("overlay-arrow-string", Voverlay_arrow_string,
    doc: /* String to display as an arrow in non-window frames.
See also `overlay-arrow-position'.  */);
  Voverlay_arrow_string = make_pure_c_string ("=>");

  DEFVAR_LISP ("overlay-arrow-variable-list", Voverlay_arrow_variable_list,
    doc: /* List of variables (symbols) which hold markers for overlay arrows.
The symbols on this list are examined during redisplay to determine
where to display overlay arrows.  */);
  Voverlay_arrow_variable_list
    = Fcons (intern_c_string ("overlay-arrow-position"), Qnil);

  DEFVAR_INT ("scroll-step", emacs_scroll_step,
    doc: /* *The number of lines to try scrolling a window by when point moves out.
If that fails to bring point back on frame, point is centered instead.
If this is zero, point is always centered after it moves off frame.
If you want scrolling to always be a line at a time, you should set
`scroll-conservatively' to a large value rather than set this to 1.  */);

  DEFVAR_INT ("scroll-conservatively", scroll_conservatively,
    doc: /* *Scroll up to this many lines, to bring point back on screen.
If point moves off-screen, redisplay will scroll by up to
`scroll-conservatively' lines in order to bring point just barely
onto the screen again.  If that cannot be done, then redisplay
recenters point as usual.

If the value is greater than 100, redisplay will never recenter point,
but will always scroll just enough text to bring point into view, even
if you move far away.

A value of zero means always recenter point if it moves off screen.  */);
  scroll_conservatively = 0;

  DEFVAR_INT ("scroll-margin", scroll_margin,
    doc: /* *Number of lines of margin at the top and bottom of a window.
Recenter the window whenever point gets within this many lines
of the top or bottom of the window.  */);
  scroll_margin = 0;

  DEFVAR_LISP ("display-pixels-per-inch",  Vdisplay_pixels_per_inch,
    doc: /* Pixels per inch value for non-window system displays.
Value is a number or a cons (WIDTH-DPI . HEIGHT-DPI).  */);
  Vdisplay_pixels_per_inch = make_float (72.0);

#if GLYPH_DEBUG
  DEFVAR_INT ("debug-end-pos", debug_end_pos, doc: /* Don't ask.  */);
#endif

  DEFVAR_LISP ("truncate-partial-width-windows",
	       Vtruncate_partial_width_windows,
    doc: /* Non-nil means truncate lines in windows narrower than the frame.
For an integer value, truncate lines in each window narrower than the
full frame width, provided the window width is less than that integer;
otherwise, respect the value of `truncate-lines'.

For any other non-nil value, truncate lines in all windows that do
not span the full frame width.

A value of nil means to respect the value of `truncate-lines'.

If `word-wrap' is enabled, you might want to reduce this.  */);
  Vtruncate_partial_width_windows = make_number (50);

  DEFVAR_BOOL ("mode-line-inverse-video", mode_line_inverse_video,
    doc: /* When nil, display the mode-line/header-line/menu-bar in the default face.
Any other value means to use the appropriate face, `mode-line',
`header-line', or `menu' respectively.  */);
  mode_line_inverse_video = 1;

  DEFVAR_LISP ("line-number-display-limit", Vline_number_display_limit,
    doc: /* *Maximum buffer size for which line number should be displayed.
If the buffer is bigger than this, the line number does not appear
in the mode line.  A value of nil means no limit.  */);
  Vline_number_display_limit = Qnil;

  DEFVAR_INT ("line-number-display-limit-width",
	      line_number_display_limit_width,
    doc: /* *Maximum line width (in characters) for line number display.
If the average length of the lines near point is bigger than this, then the
line number may be omitted from the mode line.  */);
  line_number_display_limit_width = 200;

  DEFVAR_BOOL ("highlight-nonselected-windows", highlight_nonselected_windows,
    doc: /* *Non-nil means highlight region even in nonselected windows.  */);
  highlight_nonselected_windows = 0;

  DEFVAR_BOOL ("multiple-frames", multiple_frames,
    doc: /* Non-nil if more than one frame is visible on this display.
Minibuffer-only frames don't count, but iconified frames do.
This variable is not guaranteed to be accurate except while processing
`frame-title-format' and `icon-title-format'.  */);

  DEFVAR_LISP ("frame-title-format", Vframe_title_format,
    doc: /* Template for displaying the title bar of visible frames.
\(Assuming the window manager supports this feature.)

This variable has the same structure as `mode-line-format', except that
the %c and %l constructs are ignored.  It is used only on frames for
which no explicit name has been set \(see `modify-frame-parameters').  */);

  DEFVAR_LISP ("icon-title-format", Vicon_title_format,
    doc: /* Template for displaying the title bar of an iconified frame.
\(Assuming the window manager supports this feature.)
This variable has the same structure as `mode-line-format' (which see),
and is used only on frames for which no explicit name has been set
\(see `modify-frame-parameters').  */);
  Vicon_title_format
    = Vframe_title_format
    = pure_cons (intern_c_string ("multiple-frames"),
		 pure_cons (make_pure_c_string ("%b"),
			    pure_cons (pure_cons (empty_unibyte_string,
						  pure_cons (intern_c_string ("invocation-name"),
							     pure_cons (make_pure_c_string ("@"),
									pure_cons (intern_c_string ("system-name"),
										   Qnil)))),
				       Qnil)));

  DEFVAR_LISP ("message-log-max", Vmessage_log_max,
    doc: /* Maximum number of lines to keep in the message log buffer.
If nil, disable message logging.  If t, log messages but don't truncate
the buffer when it becomes large.  */);
  Vmessage_log_max = make_number (100);

  DEFVAR_LISP ("window-size-change-functions", Vwindow_size_change_functions,
    doc: /* Functions called before redisplay, if window sizes have changed.
The value should be a list of functions that take one argument.
Just before redisplay, for each frame, if any of its windows have changed
size since the last redisplay, or have been split or deleted,
all the functions in the list are called, with the frame as argument.  */);
  Vwindow_size_change_functions = Qnil;

  DEFVAR_LISP ("window-scroll-functions", Vwindow_scroll_functions,
    doc: /* List of functions to call before redisplaying a window with scrolling.
Each function is called with two arguments, the window and its new
display-start position.  Note that these functions are also called by
`set-window-buffer'.  Also note that the value of `window-end' is not
valid when these functions are called.

Warning: Do not use this feature to alter the way the window
is scrolled.  It is not designed for that, and such use probably won't
work.  */);
  Vwindow_scroll_functions = Qnil;

  DEFVAR_LISP ("window-text-change-functions",
	       Vwindow_text_change_functions,
    doc: /* Functions to call in redisplay when text in the window might change.  */);
  Vwindow_text_change_functions = Qnil;

  DEFVAR_LISP ("redisplay-end-trigger-functions", Vredisplay_end_trigger_functions,
    doc: /* Functions called when redisplay of a window reaches the end trigger.
Each function is called with two arguments, the window and the end trigger value.
See `set-window-redisplay-end-trigger'.  */);
  Vredisplay_end_trigger_functions = Qnil;

  DEFVAR_LISP ("mouse-autoselect-window", Vmouse_autoselect_window,
     doc: /* *Non-nil means autoselect window with mouse pointer.
If nil, do not autoselect windows.
A positive number means delay autoselection by that many seconds: a
window is autoselected only after the mouse has remained in that
window for the duration of the delay.
A negative number has a similar effect, but causes windows to be
autoselected only after the mouse has stopped moving.  \(Because of
the way Emacs compares mouse events, you will occasionally wait twice
that time before the window gets selected.\)
Any other value means to autoselect window instantaneously when the
mouse pointer enters it.

Autoselection selects the minibuffer only if it is active, and never
unselects the minibuffer if it is active.

When customizing this variable make sure that the actual value of
`focus-follows-mouse' matches the behavior of your window manager.  */);
  Vmouse_autoselect_window = Qnil;

  DEFVAR_LISP ("auto-resize-tool-bars", Vauto_resize_tool_bars,
    doc: /* *Non-nil means automatically resize tool-bars.
This dynamically changes the tool-bar's height to the minimum height
that is needed to make all tool-bar items visible.
If value is `grow-only', the tool-bar's height is only increased
automatically; to decrease the tool-bar height, use \\[recenter].  */);
  Vauto_resize_tool_bars = Qt;

  DEFVAR_BOOL ("auto-raise-tool-bar-buttons", auto_raise_tool_bar_buttons_p,
    doc: /* *Non-nil means raise tool-bar buttons when the mouse moves over them.  */);
  auto_raise_tool_bar_buttons_p = 1;

  DEFVAR_BOOL ("make-cursor-line-fully-visible", make_cursor_line_fully_visible_p,
    doc: /* *Non-nil means to scroll (recenter) cursor line if it is not fully visible.  */);
  make_cursor_line_fully_visible_p = 1;

  DEFVAR_LISP ("tool-bar-border", Vtool_bar_border,
    doc: /* *Border below tool-bar in pixels.
If an integer, use it as the height of the border.
If it is one of `internal-border-width' or `border-width', use the
value of the corresponding frame parameter.
Otherwise, no border is added below the tool-bar.  */);
  Vtool_bar_border = Qinternal_border_width;

  DEFVAR_LISP ("tool-bar-button-margin", Vtool_bar_button_margin,
    doc: /* *Margin around tool-bar buttons in pixels.
If an integer, use that for both horizontal and vertical margins.
Otherwise, value should be a pair of integers `(HORZ . VERT)' with
HORZ specifying the horizontal margin, and VERT specifying the
vertical margin.  */);
  Vtool_bar_button_margin = make_number (DEFAULT_TOOL_BAR_BUTTON_MARGIN);

  DEFVAR_INT ("tool-bar-button-relief", tool_bar_button_relief,
    doc: /* *Relief thickness of tool-bar buttons.  */);
  tool_bar_button_relief = DEFAULT_TOOL_BAR_BUTTON_RELIEF;

  DEFVAR_LISP ("tool-bar-style", Vtool_bar_style,
    doc: /* Tool bar style to use.
It can be one of
 image            - show images only
 text             - show text only
 both             - show both, text below image
 both-horiz       - show text to the right of the image
 text-image-horiz - show text to the left of the image
 any other        - use system default or image if no system default.  */);
  Vtool_bar_style = Qnil;

  DEFVAR_INT ("tool-bar-max-label-size", tool_bar_max_label_size,
    doc: /* *Maximum number of characters a label can have to be shown.
The tool bar style must also show labels for this to have any effect, see
`tool-bar-style'.  */);
  tool_bar_max_label_size = DEFAULT_TOOL_BAR_LABEL_SIZE;

  DEFVAR_LISP ("fontification-functions", Vfontification_functions,
    doc: /* List of functions to call to fontify regions of text.
Each function is called with one argument POS.  Functions must
fontify a region starting at POS in the current buffer, and give
fontified regions the property `fontified'.  */);
  Vfontification_functions = Qnil;
  Fmake_variable_buffer_local (Qfontification_functions);

  DEFVAR_BOOL ("unibyte-display-via-language-environment",
               unibyte_display_via_language_environment,
    doc: /* *Non-nil means display unibyte text according to language environment.
Specifically, this means that raw bytes in the range 160-255 decimal
are displayed by converting them to the equivalent multibyte characters
according to the current language environment.  As a result, they are
displayed according to the current fontset.

Note that this variable affects only how these bytes are displayed,
but does not change the fact they are interpreted as raw bytes.  */);
  unibyte_display_via_language_environment = 0;

  DEFVAR_LISP ("max-mini-window-height", Vmax_mini_window_height,
    doc: /* *Maximum height for resizing mini-windows (the minibuffer and the echo area).
If a float, it specifies a fraction of the mini-window frame's height.
If an integer, it specifies a number of lines.  */);
  Vmax_mini_window_height = make_float (0.25);

  DEFVAR_LISP ("resize-mini-windows", Vresize_mini_windows,
    doc: /* How to resize mini-windows (the minibuffer and the echo area).
A value of nil means don't automatically resize mini-windows.
A value of t means resize them to fit the text displayed in them.
A value of `grow-only', the default, means let mini-windows grow only;
they return to their normal size when the minibuffer is closed, or the
echo area becomes empty.  */);
  Vresize_mini_windows = Qgrow_only;

  DEFVAR_LISP ("blink-cursor-alist", Vblink_cursor_alist,
    doc: /* Alist specifying how to blink the cursor off.
Each element has the form (ON-STATE . OFF-STATE).  Whenever the
`cursor-type' frame-parameter or variable equals ON-STATE,
comparing using `equal', Emacs uses OFF-STATE to specify
how to blink it off.  ON-STATE and OFF-STATE are values for
the `cursor-type' frame parameter.

If a frame's ON-STATE has no entry in this list,
the frame's other specifications determine how to blink the cursor off.  */);
  Vblink_cursor_alist = Qnil;

  DEFVAR_BOOL ("auto-hscroll-mode", automatic_hscrolling_p,
    doc: /* Allow or disallow automatic horizontal scrolling of windows.
If non-nil, windows are automatically scrolled horizontally to make
point visible.  */);
  automatic_hscrolling_p = 1;
  DEFSYM (Qauto_hscroll_mode, "auto-hscroll-mode");

  DEFVAR_INT ("hscroll-margin", hscroll_margin,
    doc: /* *How many columns away from the window edge point is allowed to get
before automatic hscrolling will horizontally scroll the window.  */);
  hscroll_margin = 5;

  DEFVAR_LISP ("hscroll-step", Vhscroll_step,
    doc: /* *How many columns to scroll the window when point gets too close to the edge.
When point is less than `hscroll-margin' columns from the window
edge, automatic hscrolling will scroll the window by the amount of columns
determined by this variable.  If its value is a positive integer, scroll that
many columns.  If it's a positive floating-point number, it specifies the
fraction of the window's width to scroll.  If it's nil or zero, point will be
centered horizontally after the scroll.  Any other value, including negative
numbers, are treated as if the value were zero.

Automatic hscrolling always moves point outside the scroll margin, so if
point was more than scroll step columns inside the margin, the window will
scroll more than the value given by the scroll step.

Note that the lower bound for automatic hscrolling specified by `scroll-left'
and `scroll-right' overrides this variable's effect.  */);
  Vhscroll_step = make_number (0);

  DEFVAR_BOOL ("message-truncate-lines", message_truncate_lines,
    doc: /* If non-nil, messages are truncated instead of resizing the echo area.
Bind this around calls to `message' to let it take effect.  */);
  message_truncate_lines = 0;

  DEFVAR_LISP ("menu-bar-update-hook",  Vmenu_bar_update_hook,
    doc: /* Normal hook run to update the menu bar definitions.
Redisplay runs this hook before it redisplays the menu bar.
This is used to update submenus such as Buffers,
whose contents depend on various data.  */);
  Vmenu_bar_update_hook = Qnil;

  DEFVAR_LISP ("menu-updating-frame", Vmenu_updating_frame,
	       doc: /* Frame for which we are updating a menu.
The enable predicate for a menu binding should check this variable.  */);
  Vmenu_updating_frame = Qnil;

  DEFVAR_BOOL ("inhibit-menubar-update", inhibit_menubar_update,
    doc: /* Non-nil means don't update menu bars.  Internal use only.  */);
  inhibit_menubar_update = 0;

  DEFVAR_LISP ("wrap-prefix", Vwrap_prefix,
    doc: /* Prefix prepended to all continuation lines at display time.
The value may be a string, an image, or a stretch-glyph; it is
interpreted in the same way as the value of a `display' text property.

This variable is overridden by any `wrap-prefix' text or overlay
property.

To add a prefix to non-continuation lines, use `line-prefix'.  */);
  Vwrap_prefix = Qnil;
  DEFSYM (Qwrap_prefix, "wrap-prefix");
  Fmake_variable_buffer_local (Qwrap_prefix);

  DEFVAR_LISP ("line-prefix", Vline_prefix,
    doc: /* Prefix prepended to all non-continuation lines at display time.
The value may be a string, an image, or a stretch-glyph; it is
interpreted in the same way as the value of a `display' text property.

This variable is overridden by any `line-prefix' text or overlay
property.

To add a prefix to continuation lines, use `wrap-prefix'.  */);
  Vline_prefix = Qnil;
  DEFSYM (Qline_prefix, "line-prefix");
  Fmake_variable_buffer_local (Qline_prefix);

  DEFVAR_BOOL ("inhibit-eval-during-redisplay", inhibit_eval_during_redisplay,
    doc: /* Non-nil means don't eval Lisp during redisplay.  */);
  inhibit_eval_during_redisplay = 0;

  DEFVAR_BOOL ("inhibit-free-realized-faces", inhibit_free_realized_faces,
    doc: /* Non-nil means don't free realized faces.  Internal use only.  */);
  inhibit_free_realized_faces = 0;

#if GLYPH_DEBUG
  DEFVAR_BOOL ("inhibit-try-window-id", inhibit_try_window_id,
	       doc: /* Inhibit try_window_id display optimization.  */);
  inhibit_try_window_id = 0;

  DEFVAR_BOOL ("inhibit-try-window-reusing", inhibit_try_window_reusing,
	       doc: /* Inhibit try_window_reusing display optimization.  */);
  inhibit_try_window_reusing = 0;

  DEFVAR_BOOL ("inhibit-try-cursor-movement", inhibit_try_cursor_movement,
	       doc: /* Inhibit try_cursor_movement display optimization.  */);
  inhibit_try_cursor_movement = 0;
#endif /* GLYPH_DEBUG */

  DEFVAR_INT ("overline-margin", overline_margin,
	       doc: /* *Space between overline and text, in pixels.
The default value is 2: the height of the overline (1 pixel) plus 1 pixel
margin to the character height.  */);
  overline_margin = 2;

  DEFVAR_INT ("underline-minimum-offset",
	       underline_minimum_offset,
     doc: /* Minimum distance between baseline and underline.
This can improve legibility of underlined text at small font sizes,
particularly when using variable `x-use-underline-position-properties'
with fonts that specify an UNDERLINE_POSITION relatively close to the
baseline.  The default value is 1.  */);
  underline_minimum_offset = 1;

  DEFVAR_BOOL ("display-hourglass", display_hourglass_p,
	       doc: /* Non-nil means show an hourglass pointer, when Emacs is busy.
This feature only works when on a window system that can change
cursor shapes.  */);
  display_hourglass_p = 1;

  DEFVAR_LISP ("hourglass-delay", Vhourglass_delay,
	       doc: /* *Seconds to wait before displaying an hourglass pointer when Emacs is busy.  */);
  Vhourglass_delay = make_number (DEFAULT_HOURGLASS_DELAY);

  hourglass_atimer = NULL;
  hourglass_shown_p = 0;

  DEFSYM (Qglyphless_char, "glyphless-char");
  DEFSYM (Qhex_code, "hex-code");
  DEFSYM (Qempty_box, "empty-box");
  DEFSYM (Qthin_space, "thin-space");
  DEFSYM (Qzero_width, "zero-width");

  DEFSYM (Qglyphless_char_display, "glyphless-char-display");
  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern_c_string ("char-table-extra-slots");
  Fput (Qglyphless_char_display, Qchar_table_extra_slots, make_number (1));

  DEFVAR_LISP ("glyphless-char-display", Vglyphless_char_display,
	       doc: /* Char-table defining glyphless characters.
Each element, if non-nil, should be one of the following:
  an ASCII acronym string: display this string in a box
  `hex-code':   display the hexadecimal code of a character in a box
  `empty-box':  display as an empty box
  `thin-space': display as 1-pixel width space
  `zero-width': don't display
An element may also be a cons cell (GRAPHICAL . TEXT), which specifies the
display method for graphical terminals and text terminals respectively.
GRAPHICAL and TEXT should each have one of the values listed above.

The char-table has one extra slot to control the display of a character for
which no font is found.  This slot only takes effect on graphical terminals.
Its value should be an ASCII acronym string, `hex-code', `empty-box', or
`thin-space'.  The default is `empty-box'.  */);
  Vglyphless_char_display = Fmake_char_table (Qglyphless_char_display, Qnil);
  Fset_char_table_extra_slot (Vglyphless_char_display, make_number (0),
			      Qempty_box);
}


/* Initialize this module when Emacs starts.  */

void
init_xdisp (void)
{
  current_header_line_height = current_mode_line_height = -1;

  CHARPOS (this_line_start_pos) = 0;

  if (!noninteractive)
    {
      struct window *m = XWINDOW (minibuf_window);
      Lisp_Object frame = m->frame;
      struct frame *f = XFRAME (frame);
      Lisp_Object root = FRAME_ROOT_WINDOW (f);
      struct window *r = XWINDOW (root);
      int i;

      echo_area_window = minibuf_window;

      XSETFASTINT (r->top_line, FRAME_TOP_MARGIN (f));
      XSETFASTINT (r->total_lines, FRAME_LINES (f) - 1 - FRAME_TOP_MARGIN (f));
      XSETFASTINT (r->total_cols, FRAME_COLS (f));
      XSETFASTINT (m->top_line, FRAME_LINES (f) - 1);
      XSETFASTINT (m->total_lines, 1);
      XSETFASTINT (m->total_cols, FRAME_COLS (f));

      scratch_glyph_row.glyphs[TEXT_AREA] = scratch_glyphs;
      scratch_glyph_row.glyphs[TEXT_AREA + 1]
	= scratch_glyphs + MAX_SCRATCH_GLYPHS;

      /* The default ellipsis glyphs `...'.  */
      for (i = 0; i < 3; ++i)
	default_invis_vector[i] = make_number ('.');
    }

  {
    /* Allocate the buffer for frame titles.
       Also used for `format-mode-line'.  */
    int size = 100;
    mode_line_noprop_buf = (char *) xmalloc (size);
    mode_line_noprop_buf_end = mode_line_noprop_buf + size;
    mode_line_noprop_ptr = mode_line_noprop_buf;
    mode_line_target = MODE_LINE_DISPLAY;
  }

  help_echo_showing_p = 0;
}

/* Since w32 does not support atimers, it defines its own implementation of
   the following three functions in w32fns.c.  */
#ifndef WINDOWSNT

/* Platform-independent portion of hourglass implementation. */

/* Return non-zero if hourglass timer has been started or hourglass is
   shown.  */
int
hourglass_started (void)
{
  return hourglass_shown_p || hourglass_atimer != NULL;
}

/* Cancel a currently active hourglass timer, and start a new one.  */
void
start_hourglass (void)
{
#if defined (HAVE_WINDOW_SYSTEM)
  EMACS_TIME delay;
  int secs, usecs = 0;

  cancel_hourglass ();

  if (INTEGERP (Vhourglass_delay)
      && XINT (Vhourglass_delay) > 0)
    secs = XFASTINT (Vhourglass_delay);
  else if (FLOATP (Vhourglass_delay)
	   && XFLOAT_DATA (Vhourglass_delay) > 0)
    {
      Lisp_Object tem;
      tem = Ftruncate (Vhourglass_delay, Qnil);
      secs = XFASTINT (tem);
      usecs = (XFLOAT_DATA (Vhourglass_delay) - secs) * 1000000;
    }
  else
    secs = DEFAULT_HOURGLASS_DELAY;

  EMACS_SET_SECS_USECS (delay, secs, usecs);
  hourglass_atimer = start_atimer (ATIMER_RELATIVE, delay,
				   show_hourglass, NULL);
#endif
}


/* Cancel the hourglass cursor timer if active, hide a busy cursor if
   shown.  */
void
cancel_hourglass (void)
{
#if defined (HAVE_WINDOW_SYSTEM)
  if (hourglass_atimer)
    {
      cancel_atimer (hourglass_atimer);
      hourglass_atimer = NULL;
    }

  if (hourglass_shown_p)
    hide_hourglass ();
#endif
}
#endif /* ! WINDOWSNT  */
