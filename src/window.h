/* Window definitions for GNU Emacs.
   Copyright (C) 1985-1986, 1993, 1995, 1997-2012
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

#ifndef WINDOW_H_INCLUDED
#define WINDOW_H_INCLUDED

#include "dispextern.h"

extern Lisp_Object Qleft, Qright;

/* Windows are allocated as if they were vectors, but then the
Lisp data type is changed to Lisp_Window.  They are garbage
collected along with the vectors.

All windows in use are arranged into a tree, with pointers up and down.

Windows that are leaves of the tree are actually displayed
and show the contents of buffers.  Windows that are not leaves
are used for representing the way groups of leaf windows are
arranged on the frame.  Leaf windows never become non-leaves.
They are deleted only by calling delete-window on them (but
this can be done implicitly).  Combination windows can be created
and deleted at any time.

A leaf window has a non-nil buffer field, and also
 has markers in its start and pointm fields.  Non-leaf windows
 have nil in these fields.

Non-leaf windows are either vertical or horizontal combinations.

A vertical combination window has children that are arranged on the frame
one above the next.  Its vchild field points to the uppermost child.
The parent field of each of the children points to the vertical
combination window.  The next field of each child points to the
child below it, or is nil for the lowest child.  The prev field
of each child points to the child above it, or is nil for the
highest child.

A horizontal combination window has children that are side by side.
Its hchild field points to the leftmost child.  In each child
the next field points to the child to the right and the prev field
points to the child to the left.

The children of a vertical combination window may be leaf windows
or horizontal combination windows.  The children of a horizontal
combination window may be leaf windows or vertical combination windows.

At the top of the tree are two windows which have nil as parent.
The second of these is minibuf_window.  The first one manages all
the frame area that is not minibuffer, and is called the root window.
Different windows can be the root at different times;
initially the root window is a leaf window, but if more windows
are created then that leaf window ceases to be root and a newly
made combination window becomes root instead.

In any case, on screens which have an ordinary window and a
minibuffer, prev of the minibuf window is the root window and next of
the root window is the minibuf window.  On minibufferless screens or
minibuffer-only screens, the root window and the minibuffer window are
one and the same, so its prev and next members are nil.

A dead window has its buffer, hchild, and vchild windows all nil.  */

struct cursor_pos
{
  /* Pixel position.  These are always window relative.  */
  int x, y;

  /* Glyph matrix position.  */
  int hpos, vpos;
};

struct window
  {
    /* This is for Lisp; the terminal code does not refer to it.  */
    struct vectorlike_header header;

    /* The frame this window is on.  */
    Lisp_Object frame;

    /* t if this window is a minibuffer window.  */
    Lisp_Object mini_p;

    /* Following (to right or down) and preceding (to left or up) child
       at same level of tree.  */
    Lisp_Object next, prev;

    /* First child of this window: vchild is used if this is a vertical
       combination, hchild if this is a horizontal combination.  Of the
       fields vchild, hchild and buffer, one and only one is non-nil
       unless the window is dead.  */
    Lisp_Object hchild, vchild;

    /* The window this one is a child of.  */
    Lisp_Object parent;

    /* The upper left corner coordinates of this window, as integers
       relative to upper left corner of frame = 0, 0.  */
    Lisp_Object left_col;
    Lisp_Object top_line;

    /* The size of the window.  */
    Lisp_Object total_lines;
    Lisp_Object total_cols;

    /* The normal size of the window.  */
    Lisp_Object normal_lines;
    Lisp_Object normal_cols;

    /* New sizes of the window.  */
    Lisp_Object new_total;
    Lisp_Object new_normal;

    /* The buffer displayed in this window.  Of the fields vchild,
       hchild and buffer, one and only one is non-nil unless the window
       is dead.  */
    Lisp_Object buffer;

    /* A marker pointing to where in the text to start displaying.
       BIDI Note: This is the _logical-order_ start, i.e. the smallest
       buffer position visible in the window, not necessarily the
       character displayed in the top left corner of the window.  */
    Lisp_Object start;

    /* A marker pointing to where in the text point is in this window,
       used only when the window is not selected.
       This exists so that when multiple windows show one buffer
       each one can have its own value of point.  */
    Lisp_Object pointm;

    /* Non-nil means next redisplay must use the value of start
       set up for it in advance.  Set by scrolling commands.  */
    Lisp_Object force_start;
    /* Non-nil means we have explicitly changed the value of start,
       but that the next redisplay is not obliged to use the new value.
       This is used in Fdelete_other_windows to force a call to
       Vwindow_scroll_functions; also by Frecenter with argument.  */
    Lisp_Object optional_new_start;

    /* Number of columns display within the window is scrolled to the left.  */
    Lisp_Object hscroll;
    /* Minimum hscroll for automatic hscrolling.  This is the value
       the user has set, by set-window-hscroll for example.  */
    Lisp_Object min_hscroll;

    /* Number saying how recently window was selected.  */
    Lisp_Object use_time;

    /* Unique number of window assigned when it was created.  */
    Lisp_Object sequence_number;

    /* No permanent meaning; used by save-window-excursion's
       bookkeeping.  */
    Lisp_Object temslot;

    /* text.modified of displayed buffer as of last time display
       completed.  */
    Lisp_Object last_modified;
    /* BUF_OVERLAY_MODIFIED of displayed buffer as of last complete update.  */
    Lisp_Object last_overlay_modified;
    /* Value of point at that time.  */
    Lisp_Object last_point;
    /* Non-nil if the buffer was "modified" when the window
       was last updated.  */
    Lisp_Object last_had_star;

    /* This window's vertical scroll bar.  This field is only for use
       by the window-system-dependent code which implements the
       scroll bars; it can store anything it likes here.  If this
       window is newly created and we haven't displayed a scroll bar in
       it yet, or if the frame doesn't have any scroll bars, this is nil.  */
    Lisp_Object vertical_scroll_bar;

    /* Width of left and right marginal areas.  A value of nil means
       no margin.  */
    Lisp_Object left_margin_cols, right_margin_cols;

    /* Width of left and right fringes.
       A value of nil or t means use frame values.  */
    Lisp_Object left_fringe_width, right_fringe_width;
    /* Non-nil means fringes are drawn outside display margins;
       othersize draw them between margin areas and text.  */
    Lisp_Object fringes_outside_margins;

    /* Pixel width of scroll bars.
       A value of nil or t means use frame values.  */
    Lisp_Object scroll_bar_width;

    /* Type of vertical scroll bar.  A value of nil means
       no scroll bar.  A value of t means use frame value.  */
    Lisp_Object vertical_scroll_bar_type;

    /* Frame coords of mark as of last time display completed */
    /* May be nil if mark does not exist or was not on frame */
    Lisp_Object last_mark_x;
    Lisp_Object last_mark_y;

    /* Z - the buffer position of the last glyph in the current matrix
       of W.  Only valid if WINDOW_END_VALID is not nil.  */
    Lisp_Object window_end_pos;
    /* Glyph matrix row of the last glyph in the current matrix
       of W.  Only valid if WINDOW_END_VALID is not nil.  */
    Lisp_Object window_end_vpos;
    /* t if window_end_pos is truly valid.
       This is nil if nontrivial redisplay is preempted
       since in that case the frame image that window_end_pos
       did not get onto the frame.  */
    Lisp_Object window_end_valid;

    /* Non-nil means must regenerate mode line of this window */
    Lisp_Object update_mode_line;

    /* Non-nil means current value of `start'
       was the beginning of a line when it was chosen.  */
    Lisp_Object start_at_line_beg;

    /* Display-table to use for displaying chars in this window.
       Nil means use the buffer's own display-table.  */
    Lisp_Object display_table;

    /* Non-nil means window is marked as dedicated.  */
    Lisp_Object dedicated;

    /* Line number and position of a line somewhere above the top of the
       screen.  If this field is nil, it means we don't have a base
       line.  */
    Lisp_Object base_line_number;
    /* If this field is nil, it means we don't have a base line.
       If it is a buffer, it means don't display the line number
       as long as the window shows that buffer.  */
    Lisp_Object base_line_pos;

    /* If we have highlighted the region (or any part of it),
       this is the mark position that we used, as an integer.  */
    Lisp_Object region_showing;

    /* The column number currently displayed in this window's mode line,
       or nil if column numbers are not being displayed.  */
    Lisp_Object column_number_displayed;

    /* If redisplay in this window goes beyond this buffer position,
       must run the redisplay-end-trigger-hook.  */
    Lisp_Object redisplay_end_trigger;

    /* t means this window's child windows are not (re-)combined.  */
    Lisp_Object combination_limit;

    /* Alist of <buffer, window-start, window-point> triples listing
       buffers previously shown in this window.  */
    Lisp_Object prev_buffers;

    /* List of buffers re-shown in this window.  */
    Lisp_Object next_buffers;

    /* An alist with parameters.  */
    Lisp_Object window_parameters;

    /* No Lisp data may follow below this point without changing
       mark_object in alloc.c.  The member current_matrix must be the
       first non-Lisp member.  */

    /* Glyph matrices.  */
    struct glyph_matrix *current_matrix;
    struct glyph_matrix *desired_matrix;

    /* Scaling factor for the glyph_matrix size calculation in this window.
       Used if window contains many small images or uses proportional fonts,
       as the normal  may yield a matrix which is too small.  */
    int nrows_scale_factor, ncols_scale_factor;

    /* Cursor position as of last update that completed without
       pause.  This is the position of last_point.  */
    struct cursor_pos last_cursor;

    /* Intended cursor position.   This is a position within the
       glyph matrix.  */
    struct cursor_pos cursor;

    /* Where the cursor actually is.  */
    struct cursor_pos phys_cursor;

    /* Cursor type and width of last cursor drawn on the window.
       Used for X and w32 frames; -1 initially.  */
    int phys_cursor_type, phys_cursor_width;

    /* This is handy for undrawing the cursor.  */
    int phys_cursor_ascent, phys_cursor_height;

    /* Non-zero means the cursor is currently displayed.  This can be
       set to zero by functions overpainting the cursor image.  */
    unsigned phys_cursor_on_p : 1;

    /* 0 means cursor is logically on, 1 means it's off.  Used for
       blinking cursor.  */
    unsigned cursor_off_p : 1;

    /* Value of cursor_off_p as of the last redisplay.  */
    unsigned last_cursor_off_p : 1;

    /* 1 means desired matrix has been build and window must be
       updated in update_frame.  */
    unsigned must_be_updated_p : 1;

    /* Flag indicating that this window is not a real one.
       Currently only used for menu bar windows of frames.  */
    unsigned pseudo_window_p : 1;

    /* 1 means the window start of this window is frozen and may not
       be changed during redisplay.  If point is not in the window,
       accept that.  */
    unsigned frozen_window_start_p : 1;

    /* Amount by which lines of this window are scrolled in
       y-direction (smooth scrolling).  */
    int vscroll;

    /* Z_BYTE - the buffer position of the last glyph in the current matrix
       of W.  Only valid if WINDOW_END_VALID is not nil.  */
    int window_end_bytepos;
};

/* 1 if W is a minibuffer window.  */

#define MINI_WINDOW_P(W)	(!NILP ((W)->mini_p))

/* General window layout:

   LEFT_EDGE_COL         RIGHT_EDGE_COL
   |                                  |
   |                                  |
   |  BOX_LEFT_EDGE_COL               |
   |  |           BOX_RIGHT_EDGE_COL  |
   |  |                            |  |
   v  v                            v  v
   <-><-><---><-----------><---><-><->
    ^  ^   ^        ^        ^   ^  ^
    |  |   |        |        |   |  |
    |  |   |        |        |   |  +-- RIGHT_SCROLL_BAR_COLS
    |  |   |        |        |   +----- RIGHT_FRINGE_WIDTH
    |  |   |        |        +--------- RIGHT_MARGIN_COLS
    |  |   |        |
    |  |   |        +------------------ TEXT_AREA_COLS
    |  |   |
    |  |   +--------------------------- LEFT_MARGIN_COLS
    |  +------------------------------- LEFT_FRINGE_WIDTH
    +---------------------------------- LEFT_SCROLL_BAR_COLS

*/


/* A handy macro.  */

#define WINDOW_XFRAME(W) \
  (XFRAME (WINDOW_FRAME ((W))))

/* Return the canonical column width of the frame of window W.  */

#define WINDOW_FRAME_COLUMN_WIDTH(W) \
  (FRAME_COLUMN_WIDTH (WINDOW_XFRAME ((W))))

/* Return the canonical column width of the frame of window W.  */

#define WINDOW_FRAME_LINE_HEIGHT(W) \
  (FRAME_LINE_HEIGHT (WINDOW_XFRAME ((W))))

/* Return the width of window W in canonical column units.
   This includes scroll bars and fringes.  */

#define WINDOW_TOTAL_COLS(W) \
  (XFASTINT ((W)->total_cols))

/* Return the height of window W in canonical line units.
   This includes header and mode lines, if any.  */

#define WINDOW_TOTAL_LINES(W) \
  (XFASTINT ((W)->total_lines))

/* Return the total pixel width of window W.  */

#define WINDOW_TOTAL_WIDTH(W) \
  (WINDOW_TOTAL_COLS (W) * WINDOW_FRAME_COLUMN_WIDTH (W))

/* Return the total pixel height of window W.  */

#define WINDOW_TOTAL_HEIGHT(W) \
  (WINDOW_TOTAL_LINES (W) * WINDOW_FRAME_LINE_HEIGHT (W))

/* For HORFLAG non-zero the total number of columns of window W.  Otherwise
   the total number of lines of W.  */

#define WINDOW_TOTAL_SIZE(w, horflag) \
  (horflag ? WINDOW_TOTAL_COLS (w) : WINDOW_TOTAL_LINES (w))

/* The smallest acceptable dimensions for a window.  Anything smaller
   might crash Emacs.  */

#define MIN_SAFE_WINDOW_WIDTH  (2)
#define MIN_SAFE_WINDOW_HEIGHT (1)

/* Return the canonical frame column at which window W starts.
   This includes a left-hand scroll bar, if any.  */

#define WINDOW_LEFT_EDGE_COL(W) \
  (XFASTINT ((W)->left_col))

/* Return the canonical frame column before which window W ends.
   This includes a right-hand scroll bar, if any.  */

#define WINDOW_RIGHT_EDGE_COL(W) \
  (WINDOW_LEFT_EDGE_COL (W) + WINDOW_TOTAL_COLS (W))

/* Return the canonical frame line at which window W starts.
   This includes a header line, if any.  */

#define WINDOW_TOP_EDGE_LINE(W) \
  (XFASTINT ((W)->top_line))

/* Return the canonical frame line before which window W ends.
   This includes a mode line, if any.  */

#define WINDOW_BOTTOM_EDGE_LINE(W) \
  (WINDOW_TOP_EDGE_LINE (W) + WINDOW_TOTAL_LINES (W))


/* Return the frame x-position at which window W starts.
   This includes a left-hand scroll bar, if any.  */

#define WINDOW_LEFT_EDGE_X(W) \
  (FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W)) \
   + WINDOW_LEFT_EDGE_COL (W) * WINDOW_FRAME_COLUMN_WIDTH (W))

/* Return the frame x- position before which window W ends.
   This includes a right-hand scroll bar, if any.  */

#define WINDOW_RIGHT_EDGE_X(W) \
  (FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W)) \
   + WINDOW_RIGHT_EDGE_COL (W) * WINDOW_FRAME_COLUMN_WIDTH (W))

/* 1 if W is a menu bar window.  */

#define WINDOW_MENU_BAR_P(W) \
  (WINDOWP (WINDOW_XFRAME (W)->menu_bar_window) \
   && (W) == XWINDOW (WINDOW_XFRAME (W)->menu_bar_window))

/* 1 if W is a tool bar window.  */

#define WINDOW_TOOL_BAR_P(W) \
  (WINDOWP (WINDOW_XFRAME (W)->tool_bar_window) \
   && (W) == XWINDOW (WINDOW_XFRAME (W)->tool_bar_window))

/* Return the frame y-position at which window W starts.
   This includes a header line, if any.  */

#define WINDOW_TOP_EDGE_Y(W) \
  (((WINDOW_MENU_BAR_P (W) || WINDOW_TOOL_BAR_P (W)) \
    ? 0 : FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W))) \
   + WINDOW_TOP_EDGE_LINE (W) * WINDOW_FRAME_LINE_HEIGHT (W))

/* Return the frame y-position before which window W ends.
   This includes a mode line, if any.  */

#define WINDOW_BOTTOM_EDGE_Y(W) \
  (((WINDOW_MENU_BAR_P (W) || WINDOW_TOOL_BAR_P (W)) \
    ? 0 : FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W))) \
   + WINDOW_BOTTOM_EDGE_LINE (W) * WINDOW_FRAME_LINE_HEIGHT (W))


/* 1 if window W takes up the full width of its frame.  */

#define WINDOW_FULL_WIDTH_P(W) \
  (WINDOW_TOTAL_COLS (W) == FRAME_TOTAL_COLS (WINDOW_XFRAME (W)))

/* 1 if window W's has no other windows to its left in its frame.  */

#define WINDOW_LEFTMOST_P(W) \
  (WINDOW_LEFT_EDGE_COL (W) == 0)

/* 1 if window W's has no other windows to its right in its frame.  */

#define WINDOW_RIGHTMOST_P(W) \
  (WINDOW_RIGHT_EDGE_COL (W) == FRAME_TOTAL_COLS (WINDOW_XFRAME (W)))


/* Return the frame column at which the text (or left fringe) in
   window W starts.  This is different from the `LEFT_EDGE' because it
   does not include a left-hand scroll bar if any.  */

#define WINDOW_BOX_LEFT_EDGE_COL(W) \
  (WINDOW_LEFT_EDGE_COL (W) \
   + WINDOW_LEFT_SCROLL_BAR_COLS (W))

/* Return the window column before which the text in window W ends.
   This is different from WINDOW_RIGHT_EDGE_COL because it does not
   include a scroll bar or window-separating line on the right edge.  */

#define WINDOW_BOX_RIGHT_EDGE_COL(W) \
  (WINDOW_RIGHT_EDGE_COL (W) \
   - WINDOW_RIGHT_SCROLL_BAR_COLS (W))


/* Return the frame position at which the text (or left fringe) in
   window W starts.  This is different from the `LEFT_EDGE' because it
   does not include a left-hand scroll bar if any.  */

#define WINDOW_BOX_LEFT_EDGE_X(W) \
  (FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W)) \
   + WINDOW_BOX_LEFT_EDGE_COL (W) * WINDOW_FRAME_COLUMN_WIDTH (W))

/* Return the window column before which the text in window W ends.
   This is different from WINDOW_RIGHT_EDGE_COL because it does not
   include a scroll bar or window-separating line on the right edge.  */

#define WINDOW_BOX_RIGHT_EDGE_X(W) \
  (FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W)) \
   + WINDOW_BOX_RIGHT_EDGE_COL (W) * WINDOW_FRAME_COLUMN_WIDTH (W))


/* Width of left margin area in columns.  */

#define WINDOW_LEFT_MARGIN_COLS(W)			\
  (NILP ((W)->left_margin_cols)				\
   ? 0							\
   : XINT ((W)->left_margin_cols))

/* Width of right marginal area in columns.  */

#define WINDOW_RIGHT_MARGIN_COLS(W)			\
  (NILP ((W)->right_margin_cols)			\
   ? 0							\
   : XINT ((W)->right_margin_cols))

/* Width of left margin area in pixels.  */

#define WINDOW_LEFT_MARGIN_WIDTH(W)			\
  (NILP ((W)->left_margin_cols)				\
   ? 0							\
   : (XINT ((W)->left_margin_cols)			\
      * WINDOW_FRAME_COLUMN_WIDTH (W)))

/* Width of right marginal area in pixels.  */

#define WINDOW_RIGHT_MARGIN_WIDTH(W)			\
  (NILP ((W)->right_margin_cols)			\
   ? 0							\
   : (XINT ((W)->right_margin_cols)			\
      * WINDOW_FRAME_COLUMN_WIDTH (W)))

/* Total width of fringes reserved for drawing truncation bitmaps,
   continuation bitmaps and alike.  The width is in canonical char
   units of the frame.  This must currently be the case because window
   sizes aren't pixel values.  If it weren't the case, we wouldn't be
   able to split windows horizontally nicely.  */

#define WINDOW_FRINGE_COLS(W)				\
  ((INTEGERP ((W)->left_fringe_width)			\
    || INTEGERP ((W)->right_fringe_width))		\
   ? ((WINDOW_LEFT_FRINGE_WIDTH (W)			\
       + WINDOW_RIGHT_FRINGE_WIDTH (W)			\
       + WINDOW_FRAME_COLUMN_WIDTH (W) - 1)		\
      / WINDOW_FRAME_COLUMN_WIDTH (W))			\
   : FRAME_FRINGE_COLS (WINDOW_XFRAME (W)))

/* Column-width of the left and right fringe.  */

#define WINDOW_LEFT_FRINGE_COLS(W)			\
  ((WINDOW_LEFT_FRINGE_WIDTH ((W))			\
    + WINDOW_FRAME_COLUMN_WIDTH (W) - 1)		\
   / WINDOW_FRAME_COLUMN_WIDTH (W))

#define WINDOW_RIGHT_FRINGE_COLS(W)			\
  ((WINDOW_RIGHT_FRINGE_WIDTH ((W))			\
    + WINDOW_FRAME_COLUMN_WIDTH (W) - 1)		\
   / WINDOW_FRAME_COLUMN_WIDTH (W))

/* Pixel-width of the left and right fringe.  */

#define WINDOW_LEFT_FRINGE_WIDTH(W)			\
  (INTEGERP ((W)->left_fringe_width)			\
   ? XFASTINT ((W)->left_fringe_width)			\
   : FRAME_LEFT_FRINGE_WIDTH (WINDOW_XFRAME (W)))

#define WINDOW_RIGHT_FRINGE_WIDTH(W)			\
  (INTEGERP ((W)->right_fringe_width)			\
   ? XFASTINT ((W)->right_fringe_width)			\
   : FRAME_RIGHT_FRINGE_WIDTH (WINDOW_XFRAME (W)))

/* Total width of fringes in pixels.  */

#define WINDOW_TOTAL_FRINGE_WIDTH(W)		\
  (WINDOW_LEFT_FRINGE_WIDTH (W) + WINDOW_RIGHT_FRINGE_WIDTH (W))

/* Are fringes outside display margins in window W.  */

#define WINDOW_HAS_FRINGES_OUTSIDE_MARGINS(W)	\
  (!NILP ((W)->fringes_outside_margins))

/* Say whether scroll bars are currently enabled for window W,
   and which side they are on.  */

#define WINDOW_VERTICAL_SCROLL_BAR_TYPE(w)		\
  (EQ ((w)->vertical_scroll_bar_type, Qt)		\
   ? FRAME_VERTICAL_SCROLL_BAR_TYPE (WINDOW_XFRAME (w))	\
   : EQ ((w)->vertical_scroll_bar_type, Qleft)		\
   ? vertical_scroll_bar_left				\
   : EQ ((w)->vertical_scroll_bar_type, Qright)		\
   ? vertical_scroll_bar_right				\
   : vertical_scroll_bar_none)				\

#define WINDOW_HAS_VERTICAL_SCROLL_BAR(w)		\
  (EQ ((w)->vertical_scroll_bar_type, Qt)		\
   ? FRAME_HAS_VERTICAL_SCROLL_BARS (WINDOW_XFRAME (w))	\
   : !NILP ((w)->vertical_scroll_bar_type))

#define WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT(w)		\
  (EQ ((w)->vertical_scroll_bar_type, Qt)			\
   ? FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (WINDOW_XFRAME (w))	\
   : EQ ((w)->vertical_scroll_bar_type, Qleft))

#define WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT(w)		\
  (EQ ((w)->vertical_scroll_bar_type, Qt)			\
   ? FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (WINDOW_XFRAME (w))\
   : EQ ((w)->vertical_scroll_bar_type, Qright))

/* Width that a scroll bar in window W should have, if there is one.
   Measured in pixels.  If scroll bars are turned off, this is still
   nonzero.  */

#define WINDOW_CONFIG_SCROLL_BAR_WIDTH(w)		\
  (INTEGERP ((w)->scroll_bar_width)			\
   ? XFASTINT ((w)->scroll_bar_width)			\
   : FRAME_CONFIG_SCROLL_BAR_WIDTH (WINDOW_XFRAME (w)))

/* Width that a scroll bar in window W should have, if there is one.
   Measured in columns (characters).  If scroll bars are turned off,
   this is still nonzero.  */

#define WINDOW_CONFIG_SCROLL_BAR_COLS(w)		\
  (INTEGERP ((w)->scroll_bar_width)			\
   ? ((XFASTINT ((w)->scroll_bar_width)			\
       + WINDOW_FRAME_COLUMN_WIDTH (w) - 1)		\
      / WINDOW_FRAME_COLUMN_WIDTH (w))			\
   : FRAME_CONFIG_SCROLL_BAR_COLS (WINDOW_XFRAME (w)))

/* Width of a scroll bar in window W, measured in columns (characters),
   but only if scroll bars are on the left.  If scroll bars are on
   the right in this frame, or there are no scroll bars, value is 0.  */

#define WINDOW_LEFT_SCROLL_BAR_COLS(w)	       \
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w) \
   ? (WINDOW_CONFIG_SCROLL_BAR_COLS (w))       \
   : 0)

/* Width of a left scroll bar area in window W , measured in pixels.  */

#define WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH(w) \
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w) \
   ? (WINDOW_CONFIG_SCROLL_BAR_COLS (w) * WINDOW_FRAME_COLUMN_WIDTH (w)) \
   : 0)

/* Width of a scroll bar in window W, measured in columns (characters),
   but only if scroll bars are on the right.  If scroll bars are on
   the left in this frame, or there are no scroll bars, value is 0.  */

#define WINDOW_RIGHT_SCROLL_BAR_COLS(w)		\
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w) \
   ? WINDOW_CONFIG_SCROLL_BAR_COLS (w)		\
   : 0)

/* Width of a left scroll bar area in window W , measured in pixels.  */

#define WINDOW_RIGHT_SCROLL_BAR_AREA_WIDTH(w)				 \
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w)				 \
   ? (WINDOW_CONFIG_SCROLL_BAR_COLS (w) * WINDOW_FRAME_COLUMN_WIDTH (w)) \
   : 0)


/* Actual width of a scroll bar in window W, measured in columns.  */

#define WINDOW_SCROLL_BAR_COLS(w)	       \
  (WINDOW_HAS_VERTICAL_SCROLL_BAR (w)	       \
   ? WINDOW_CONFIG_SCROLL_BAR_COLS (w)	       \
   : 0)

/* Width of a left scroll bar area in window W , measured in pixels.  */

#define WINDOW_SCROLL_BAR_AREA_WIDTH(w)					 \
  (WINDOW_HAS_VERTICAL_SCROLL_BAR (w)					\
   ? (WINDOW_CONFIG_SCROLL_BAR_COLS (w) * WINDOW_FRAME_COLUMN_WIDTH (w)) \
   : 0)


/* Return the frame position where the scroll bar of window W starts.  */

#define WINDOW_SCROLL_BAR_AREA_X(W)		\
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (W)	\
   ? WINDOW_BOX_RIGHT_EDGE_X (W)		\
   : WINDOW_LEFT_EDGE_X (W))


/* Height in pixels, and in lines, of the mode line.
   May be zero if W doesn't have a mode line.  */

#define WINDOW_MODE_LINE_HEIGHT(W)	\
  (WINDOW_WANTS_MODELINE_P ((W))	\
   ? CURRENT_MODE_LINE_HEIGHT (W)	\
   : 0)

#define WINDOW_MODE_LINE_LINES(W)		\
  (!! WINDOW_WANTS_MODELINE_P ((W)))

/* Height in pixels, and in lines, of the header line.
   Zero if W doesn't have a header line.  */

#define WINDOW_HEADER_LINE_HEIGHT(W)	\
  (WINDOW_WANTS_HEADER_LINE_P ((W))	\
   ? CURRENT_HEADER_LINE_HEIGHT (W)	\
   : 0)

#define WINDOW_HEADER_LINE_LINES(W)		\
  (!! WINDOW_WANTS_HEADER_LINE_P ((W)))

/* Pixel height of window W without mode line.  */

#define WINDOW_BOX_HEIGHT_NO_MODE_LINE(W)	\
  (WINDOW_TOTAL_HEIGHT ((W))			\
   - WINDOW_MODE_LINE_HEIGHT ((W)))

/* Pixel height of window W without mode and header line.  */

#define WINDOW_BOX_TEXT_HEIGHT(W)		\
  (WINDOW_TOTAL_HEIGHT ((W))			\
   - WINDOW_MODE_LINE_HEIGHT ((W))		\
   - WINDOW_HEADER_LINE_HEIGHT ((W)))


/* Convert window W relative pixel X to frame pixel coordinates.  */

#define WINDOW_TO_FRAME_PIXEL_X(W, X)		\
  ((X) + WINDOW_BOX_LEFT_EDGE_X ((W)))

/* Convert window W relative pixel Y to frame pixel coordinates.  */

#define WINDOW_TO_FRAME_PIXEL_Y(W, Y)		\
  ((Y) + WINDOW_TOP_EDGE_Y ((W)))

/* Convert frame relative pixel X to window relative pixel X.  */

#define FRAME_TO_WINDOW_PIXEL_X(W, X)		\
  ((X) - WINDOW_BOX_LEFT_EDGE_X ((W)))

/* Convert frame relative pixel Y to window relative pixel Y.  */

#define FRAME_TO_WINDOW_PIXEL_Y(W, Y)		\
  ((Y) - WINDOW_TOP_EDGE_Y ((W)))

/* Convert a text area relative x-position in window W to frame X
   pixel coordinates.  */

#define WINDOW_TEXT_TO_FRAME_PIXEL_X(W, X)		\
  (window_box_left ((W), TEXT_AREA) + (X))

/* This is the window in which the terminal's cursor should
   be left when nothing is being done with it.  This must
   always be a leaf window, and its buffer is selected by
   the top level editing loop at the end of each command.

   This value is always the same as
    FRAME_SELECTED_WINDOW (selected_frame).  */

extern Lisp_Object selected_window;

/* This is a time stamp for window selection, so we can find the least
   recently used window.  Its only users are Fselect_window,
   init_window_once, and make_frame.  */

extern int window_select_count;

/* The minibuffer window of the selected frame.
   Note that you cannot test for minibufferness of an arbitrary window
   by comparing against this; use the MINI_WINDOW_P macro instead.  */

extern Lisp_Object minibuf_window;

/* Non-nil means it is the window whose mode line should be
   shown as the selected window when the minibuffer is selected.  */

extern Lisp_Object minibuf_selected_window;

/* Window that the mouse is over (nil if no mouse support).  */

extern Lisp_Object Vmouse_window;

/* Last mouse-click event (nil if no mouse support).  */

extern Lisp_Object Vmouse_event;

EXFUN (Fnext_window, 3);
EXFUN (Fselect_window, 2);
EXFUN (Fset_window_buffer, 3);
EXFUN (Fset_window_point, 2);
extern Lisp_Object make_window (void);
extern Lisp_Object window_from_coordinates (struct frame *, int, int,
                                            enum window_part *, int);
EXFUN (Fwindow_dedicated_p, 1);
extern void resize_frame_windows (struct frame *, int, int);
extern void delete_all_child_windows (Lisp_Object);
extern void freeze_window_starts (struct frame *, int);
extern void grow_mini_window (struct window *, int);
extern void shrink_mini_window (struct window *);
extern int window_relative_x_coord (struct window *, enum window_part, int);

void run_window_configuration_change_hook (struct frame *f);

/* Make WINDOW display BUFFER as its contents.  RUN_HOOKS_P non-zero
   means it's allowed to run hooks.  See make_frame for a case where
   it's not allowed.  */

void set_window_buffer (Lisp_Object window, Lisp_Object buffer,
                        int run_hooks_p, int keep_margins_p);

/* This is the window where the echo area message was displayed.  It
   is always a minibuffer window, but it may not be the same window
   currently active as a minibuffer.  */

extern Lisp_Object echo_area_window;

/* Depth in recursive edits.  */

extern EMACS_INT command_loop_level;

/* Depth in minibuffer invocations.  */

extern EMACS_INT minibuf_level;

/* true if we should redraw the mode lines on the next redisplay.  */

extern int update_mode_lines;

/* Nonzero if BEGV - BEG or Z - ZV of current buffer has changed since
   last redisplay that finished.  */

extern int clip_changed;

/* Nonzero if window sizes or contents have changed since last
   redisplay that finished */

extern int windows_or_buffers_changed;

/* Nonzero means a frame's cursor type has been changed.  */

extern int cursor_type_changed;

/* Number of windows displaying the selected buffer.  Normally this is
   1, but it can be more.  */

extern int buffer_shared;

/* If *ROWS or *COLS are too small a size for FRAME, set them to the
   minimum allowable size.  */

extern void check_frame_size (struct frame *frame, int *rows, int *cols);

/* Return a pointer to the glyph W's physical cursor is on.  Value is
   null if W's current matrix is invalid, so that no meaningful glyph
   can be returned.  */

struct glyph *get_phys_cursor_glyph (struct window *w);

/* Value is non-zero if WINDOW is a live window.  */

#define WINDOW_LIVE_P(WINDOW) \
     (WINDOWP ((WINDOW)) && !NILP (XWINDOW ((WINDOW))->buffer))


/* These used to be in lisp.h.  */

extern Lisp_Object Qwindowp, Qwindow_live_p;
extern Lisp_Object Vwindow_list;

EXFUN (Fwindow_buffer, 1);
EXFUN (Fget_buffer_window, 2);
EXFUN (Fwindow_minibuffer_p, 1);
EXFUN (Fselected_window, 0);
EXFUN (Fframe_root_window, 1);
EXFUN (Fframe_first_window, 1);
EXFUN (Fset_frame_selected_window, 3);
EXFUN (Fset_window_configuration, 1);
EXFUN (Fcurrent_window_configuration, 1);
extern int compare_window_configurations (Lisp_Object, Lisp_Object, int);
EXFUN (Fpos_visible_in_window_p, 3);
extern void mark_window_cursors_off (struct window *);
extern int window_internal_height (struct window *);
extern int window_body_cols (struct window *w);
EXFUN (Frecenter, 1);
extern void temp_output_buffer_show (Lisp_Object);
extern void replace_buffer_in_windows (Lisp_Object);
extern void replace_buffer_in_windows_safely (Lisp_Object);
extern void init_window_once (void);
extern void init_window (void);
extern void syms_of_window (void);
extern void keys_of_window (void);

#endif /* not WINDOW_H_INCLUDED */
