/* Define frame-object for GNU Emacs.
   Copyright (C) 1993-1994, 1999-2012 Free Software Foundation, Inc.

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

/* Don't multiply include: dispextern.h includes macterm.h which
   includes frame.h some emacs source includes both dispextern.h and
   frame.h */

#ifndef EMACS_FRAME_H
#define EMACS_FRAME_H

#include "dispextern.h"


/* Miscellanea.  */

/* Nonzero means there is at least one garbaged frame. */
extern int frame_garbaged;


/* The structure representing a frame.  */

enum output_method
{
  output_initial,
  output_termcap,
  output_x_window,
  output_msdos_raw,
  output_w32,
  output_mac,
  output_ns
};

enum vertical_scroll_bar_type
{
  vertical_scroll_bar_none,
  vertical_scroll_bar_left,
  vertical_scroll_bar_right
};

enum text_cursor_kinds
{
  DEFAULT_CURSOR = -2,
  NO_CURSOR = -1,
  FILLED_BOX_CURSOR,
  HOLLOW_BOX_CURSOR,
  BAR_CURSOR,
  HBAR_CURSOR
};

enum fullscreen_type
{
  FULLSCREEN_NONE,
  FULLSCREEN_WIDTH     = 0x001,
  FULLSCREEN_HEIGHT    = 0x002,
  FULLSCREEN_BOTH      = 0x003,
  FULLSCREEN_MAXIMIZED = 0x013,
  FULLSCREEN_WAIT      = 0x100
};


#define FRAME_FOREGROUND_PIXEL(f) ((f)->foreground_pixel)
#define FRAME_BACKGROUND_PIXEL(f) ((f)->background_pixel)

struct terminal;

struct font_driver_list;

struct frame
{
  struct vectorlike_header header;

  /* All Lisp_Object components must come first.
     That ensures they are all aligned normally.  */

  /* Name of this frame: a Lisp string.  It is used for looking up resources,
     as well as for the title in some cases.  */
  Lisp_Object name;

  /* The name to use for the icon, the last time
     it was refreshed.  nil means not explicitly specified.  */
  Lisp_Object icon_name;

  /* This is the frame title specified explicitly, if any.
     Usually it is nil.  */
  Lisp_Object title;

  /* The frame which should receive keystrokes that occur in this
     frame, or nil if they should go to the frame itself.  This is
     usually nil, but if the frame is minibufferless, we can use this
     to redirect keystrokes to a surrogate minibuffer frame when
     needed.

     Note that a value of nil is different than having the field point
     to the frame itself.  Whenever the Fselect_frame function is used
     to shift from one frame to the other, any redirections to the
     original frame are shifted to the newly selected frame; if
     focus_frame is nil, Fselect_frame will leave it alone.  */
  Lisp_Object focus_frame;

  /* This frame's root window.  Every frame has one.
     If the frame has only a minibuffer window, this is it.
     Otherwise, if the frame has a minibuffer window, this is its sibling.  */
  Lisp_Object root_window;

  /* This frame's selected window.
     Each frame has its own window hierarchy
     and one of the windows in it is selected within the frame.
     The selected window of the selected frame is Emacs's selected window.  */
  Lisp_Object selected_window;

  /* This frame's minibuffer window.
     Most frames have their own minibuffer windows,
     but only the selected frame's minibuffer window
     can actually appear to exist.  */
  Lisp_Object minibuffer_window;

  /* Parameter alist of this frame.
     These are the parameters specified when creating the frame
     or modified with modify-frame-parameters.  */
  Lisp_Object param_alist;

  /* List of scroll bars on this frame.
     Actually, we don't specify exactly what is stored here at all; the
     scroll bar implementation code can use it to store anything it likes.
     This field is marked by the garbage collector.  It is here
     instead of in the `device' structure so that the garbage
     collector doesn't need to look inside the window-system-dependent
     structure.  */
  Lisp_Object scroll_bars;
  Lisp_Object condemned_scroll_bars;

  /* Vector describing the items to display in the menu bar.
     Each item has four elements in this vector.
     They are KEY, STRING, SUBMAP, and HPOS.
     (HPOS is not used in when the X toolkit is in use.)
     There are four additional elements of nil at the end, to terminate.  */
  Lisp_Object menu_bar_items;

  /* Alist of elements (FACE-NAME . FACE-VECTOR-DATA).  */
  Lisp_Object face_alist;

  /* A vector that records the entire structure of this frame's menu bar.
     For the format of the data, see extensive comments in xmenu.c.
     Only the X toolkit version uses this.  */
  Lisp_Object menu_bar_vector;

  /* Predicate for selecting buffers for other-buffer.  */
  Lisp_Object buffer_predicate;

  /* List of buffers viewed in this frame, for other-buffer.  */
  Lisp_Object buffer_list;

  /* List of buffers that were viewed, then buried in this frame.  The
     most recently buried buffer is first.  For last-buffer.  */
  Lisp_Object buried_buffer_list;

  /* A dummy window used to display menu bars under X when no X
     toolkit support is available.  */
  Lisp_Object menu_bar_window;

  /* A window used to display the tool-bar of a frame.  */
  Lisp_Object tool_bar_window;

  /* Desired and current tool-bar items.  */
  Lisp_Object tool_bar_items;

  /* Where tool bar is, can be left, right, top or bottom.  The native
     tool bar only supports top.  */
  Lisp_Object tool_bar_position;

  /* Desired and current contents displayed in tool_bar_window.  */
  Lisp_Object desired_tool_bar_string, current_tool_bar_string;

  /* Beyond here, there should be no more Lisp_Object components.  */

  /* Cache of realized faces.  */
  struct face_cache *face_cache;

  /* Number of elements in `menu_bar_vector' that have meaningful data.  */
  int menu_bar_items_used;

  /* A buffer to hold the frame's name.  We can't use the Lisp
     string's pointer (`name', above) because it might get relocated.  */
  char *namebuf;

  /* Glyph pool and matrix. */
  struct glyph_pool *current_pool;
  struct glyph_pool *desired_pool;
  struct glyph_matrix *desired_matrix;
  struct glyph_matrix *current_matrix;

  /* 1 means that glyphs on this frame have been initialized so it can
     be used for output.  */
  unsigned glyphs_initialized_p : 1;

  /* Set to non-zero in change_frame_size when size of frame changed
     Clear the frame in clear_garbaged_frames if set.  */
  unsigned resized_p : 1;

  /* Set to non-zero in when we want for force a flush_display in
     update_frame, usually after resizing the frame.  */
  unsigned force_flush_display_p : 1;

  /* Set to non-zero if the default face for the frame has been
     realized.  Reset to zero whenever the default face changes.
     Used to see the difference between a font change and face change.  */
  unsigned default_face_done_p : 1;

  /* Set to non-zero if this frame has already been hscrolled during
     current redisplay.  */
  unsigned already_hscrolled_p : 1;

  /* Set to non-zero when current redisplay has updated frame.  */
  unsigned updated_p : 1;

  /* Set to non-zero to minimize tool-bar height even when
     auto-resize-tool-bar is set to grow-only.  */
  unsigned minimize_tool_bar_window_p : 1;

#if defined (USE_GTK) || defined (HAVE_NS)
  /* Nonzero means using a tool bar that comes from the toolkit.  */
  int external_tool_bar;
#endif

  /* Margin at the top of the frame.  Used to display the tool-bar.  */
  int tool_bar_lines;

  int n_tool_bar_rows;
  int n_tool_bar_items;

  /* A buffer for decode_mode_line. */
  char *decode_mode_spec_buffer;

  /* See do_line_insertion_deletion_costs for info on these arrays. */
  /* Cost of inserting 1 line on this frame */
  int *insert_line_cost;
  /* Cost of deleting 1 line on this frame */
  int *delete_line_cost;
  /* Cost of inserting n lines on this frame */
  int *insert_n_lines_cost;
  /* Cost of deleting n lines on this frame */
  int *delete_n_lines_cost;

  /* Size of this frame, excluding fringes, scroll bars etc.,
     in units of canonical characters.  */
  int text_lines, text_cols;

  /* Total size of this frame (i.e. its native window), in units of
     canonical characters.  */
  int total_lines, total_cols;

  /* New text height and width for pending size change.
     0 if no change pending.  */
  int new_text_lines, new_text_cols;

  /* Pixel position of the frame window (x and y offsets in root window).  */
  int left_pos, top_pos;

  /* Size of the frame window in pixels.  */
  int pixel_height, pixel_width;

  /* Dots per inch of the screen the frame is on.  */
  double resx, resy;

  /* These many pixels are the difference between the outer window (i.e. the
     left and top of the window manager decoration) and FRAME_X_WINDOW. */
  int x_pixels_diff, y_pixels_diff;

  /* This is the gravity value for the specified window position.  */
  int win_gravity;

  /* The geometry flags for this window.  */
  int size_hint_flags;

  /* Border width of the frame window as known by the (X) window system.  */
  int border_width;

  /* Width of the internal border.  This is a line of background color
     just inside the window's border.  When the frame is selected,
     a highlighting is displayed inside the internal border.  */
  int internal_border_width;

  /* Canonical X unit.  Width of default font, in pixels.  */
  int column_width;

  /* Width of space glyph of default font, in pixels.  */
  int space_width;

  /* Canonical Y unit.  Height of a line, in pixels.  */
  int line_height;

  /* The output method says how the contents of this frame are
     displayed.  It could be using termcap, or using an X window.
     This must be the same as the terminal->type. */
  enum output_method output_method;

  /* The terminal device that this frame uses.  If this is NULL, then
     the frame has been deleted. */
  struct terminal *terminal;

  /* Device-dependent, frame-local auxiliary data used for displaying
     the contents.  When the frame is deleted, this data is deleted as
     well. */
  union output_data
  {
    struct tty_output *tty;     /* termchar.h */
    struct x_output *x;         /* xterm.h */
    struct w32_output *w32;     /* w32term.h */
    struct ns_output *ns;       /* nsterm.h */
    EMACS_INT nothing;
  }
  output_data;

  /* List of font-drivers available on the frame. */
  struct font_driver_list *font_driver_list;
  /* List of data specific to font-driver and frame, but common to
     faces.  */
  struct font_data_list *font_data_list;

  /* Total width of fringes reserved for drawing truncation bitmaps,
     continuation bitmaps and alike.  The width is in canonical char
     units of the frame.  This must currently be the case because window
     sizes aren't pixel values.  If it weren't the case, we wouldn't be
     able to split windows horizontally nicely.  */
  int fringe_cols;

  /* The extra width (in pixels) currently allotted for fringes.  */
  int left_fringe_width, right_fringe_width;

  /* See FULLSCREEN_ enum below */
  enum fullscreen_type want_fullscreen;

  /* Number of lines of menu bar.  */
  int menu_bar_lines;

#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) \
    || defined (HAVE_NS) || defined (USE_GTK)
  /* Nonzero means using a menu bar that comes from the X toolkit.  */
  unsigned int external_menu_bar : 1;
#endif

  /* Nonzero if last attempt at redisplay on this frame was preempted.  */
  unsigned char display_preempted : 1;

  /* visible is nonzero if the frame is currently displayed; we check
     it to see if we should bother updating the frame's contents.
     DON'T SET IT DIRECTLY; instead, use FRAME_SET_VISIBLE.

     Note that, since invisible frames aren't updated, whenever a
     frame becomes visible again, it must be marked as garbaged.  The
     FRAME_SAMPLE_VISIBILITY macro takes care of this.

     On ttys and on Windows NT/9X, to avoid wasting effort updating
     visible frames that are actually completely obscured by other
     windows on the display, we bend the meaning of visible slightly:
     if greater than 1, then the frame is obscured - we still consider
     it to be "visible" as seen from lisp, but we don't bother
     updating it.  We must take care to garbage the frame when it
     ceases to be obscured though.

     iconified is nonzero if the frame is currently iconified.

     Asynchronous input handlers should NOT change these directly;
     instead, they should change async_visible or async_iconified, and
     let the FRAME_SAMPLE_VISIBILITY macro set visible and iconified
     at the next redisplay.

     These should probably be considered read-only by everyone except
     FRAME_SAMPLE_VISIBILITY.

     These two are mutually exclusive.  They might both be zero, if the
     frame has been made invisible without an icon.  */
  unsigned char visible : 2;
  unsigned char iconified : 1;

  /* Let's not use bitfields for volatile variables.  */

  /* Asynchronous input handlers change these, and
     FRAME_SAMPLE_VISIBILITY copies them into visible and iconified.
     See FRAME_SAMPLE_VISIBILITY, below.  */
  volatile char async_visible, async_iconified;

  /* Nonzero if this frame should be redrawn.  */
  volatile char garbaged;

  /* True if frame actually has a minibuffer window on it.
     0 if using a minibuffer window that isn't on this frame.  */
  unsigned char has_minibuffer : 1;

  /* 0 means, if this frame has just one window,
     show no modeline for that window.  */
  unsigned char wants_modeline : 1;

  /* Non-zero if the hardware device this frame is displaying on can
     support scroll bars.  */
  char can_have_scroll_bars;

  /* Non-0 means raise this frame to the top of the heap when selected.  */
  unsigned char auto_raise : 1;

  /* Non-0 means lower this frame to the bottom of the stack when left.  */
  unsigned char auto_lower : 1;

  /* True if frame's root window can't be split.  */
  unsigned char no_split : 1;

  /* If this is set, then Emacs won't change the frame name to indicate
     the current buffer, etcetera.  If the user explicitly sets the frame
     name, this gets set.  If the user sets the name to Qnil, this is
     cleared.  */
  unsigned char explicit_name : 1;

  /* Nonzero if size of some window on this frame has changed.  */
  unsigned char window_sizes_changed : 1;

  /* Nonzero if the mouse has moved on this display device
     since the last time we checked.  */
  unsigned char mouse_moved :1;

  /* Nonzero means that the pointer is invisible. */
  unsigned char pointer_invisible :1;

  /* If can_have_scroll_bars is non-zero, this is non-zero if we should
     actually display them on this frame.  */
  enum vertical_scroll_bar_type vertical_scroll_bar_type;

  /* What kind of text cursor should we draw in the future?
     This should always be filled_box_cursor or bar_cursor.  */
  enum text_cursor_kinds desired_cursor;

  /* Width of bar cursor (if we are using that).  */
  int cursor_width;

  /* What kind of text cursor should we draw when the cursor blinks off?
     This can be filled_box_cursor or bar_cursor or no_cursor.  */
  enum text_cursor_kinds blink_off_cursor;

  /* Width of bar cursor (if we are using that) for blink-off state.  */
  int blink_off_cursor_width;

  /* Storage for messages to this frame. */
  char *message_buf;

  /* Nonnegative if current redisplay should not do scroll computation
     for lines beyond a certain vpos.  This is the vpos.  */
  int scroll_bottom_vpos;

  /* Configured width of the scroll bar, in pixels and in characters.
     config_scroll_bar_cols tracks config_scroll_bar_width if the
     latter is positive; a zero value in config_scroll_bar_width means
     to compute the actual width on the fly, using config_scroll_bar_cols
     and the current font width.  */
  int config_scroll_bar_width;
  int config_scroll_bar_cols;

  /* The size of the extra width currently allotted for vertical
     scroll bars in this frame, in pixels.  */
  int scroll_bar_actual_width;

  /* The baud rate that was used to calculate costs for this frame.  */
  int cost_calculation_baud_rate;

  /* frame opacity
     alpha[0]: alpha transparency of the active frame
     alpha[1]: alpha transparency of inactive frames
     Negative values mean not to change alpha.  */
  double alpha[2];

  /* Exponent for gamma correction of colors.  1/(VIEWING_GAMMA *
     SCREEN_GAMMA) where viewing_gamma is 0.4545 and SCREEN_GAMMA is a
     frame parameter.  0 means don't do gamma correction.  */
  double gamma;

  /* Additional space to put between text lines on this frame.  */
  int extra_line_spacing;

  /* All display backends seem to need these two pixel values. */
  unsigned long background_pixel;
  unsigned long foreground_pixel;
};

#define FRAME_KBOARD(f) ((f)->terminal->kboard)

/* Return a pointer to the image cache of frame F.  */
#define FRAME_IMAGE_CACHE(F) ((F)->terminal->image_cache)

typedef struct frame *FRAME_PTR;

#define XFRAME(p) (eassert (FRAMEP(p)),(struct frame *) XPNTR (p))
#define XSETFRAME(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_FRAME))

/* Given a window, return its frame as a Lisp_Object.  */
#define WINDOW_FRAME(w) (w)->frame

/* Test a frame for particular kinds of display methods.  */
#define FRAME_INITIAL_P(f) ((f)->output_method == output_initial)
#define FRAME_TERMCAP_P(f) ((f)->output_method == output_termcap)
#define FRAME_X_P(f) ((f)->output_method == output_x_window)
#define FRAME_W32_P(f) ((f)->output_method == output_w32)
#define FRAME_MSDOS_P(f) ((f)->output_method == output_msdos_raw)
#define FRAME_MAC_P(f) ((f)->output_method == output_mac)
#define FRAME_NS_P(f) ((f)->output_method == output_ns)

/* FRAME_WINDOW_P tests whether the frame is a window, and is
   defined to be the predicate for the window system being used.  */

#ifdef HAVE_X_WINDOWS
#define FRAME_WINDOW_P(f) FRAME_X_P (f)
#endif
#ifdef HAVE_NTGUI
#define FRAME_WINDOW_P(f) FRAME_W32_P (f)
#endif
#ifdef HAVE_NS
#define FRAME_WINDOW_P(f) FRAME_NS_P(f)
#endif
#ifndef FRAME_WINDOW_P
#define FRAME_WINDOW_P(f) (0)
#endif

/* Return a pointer to the structure holding information about the
   region of text, if any, that is currently shown in mouse-face on
   frame F.  We need to define two versions because a TTY-only build
   does not have FRAME_X_DISPLAY_INFO.  */
#ifdef HAVE_WINDOW_SYSTEM
# define MOUSE_HL_INFO(F)					\
  (FRAME_WINDOW_P(F)						\
   ? &FRAME_X_DISPLAY_INFO(F)->mouse_highlight			\
   : &(F)->output_data.tty->display_info->mouse_highlight)
#else
# define MOUSE_HL_INFO(F)					\
  (&(F)->output_data.tty->display_info->mouse_highlight)
#endif

/* Nonzero if frame F is still alive (not deleted).  */
#define FRAME_LIVE_P(f) ((f)->terminal != 0)

/* Nonzero if frame F is a minibuffer-only frame.  */
#define FRAME_MINIBUF_ONLY_P(f) \
  EQ (FRAME_ROOT_WINDOW (f), FRAME_MINIBUF_WINDOW (f))

/* Nonzero if frame F contains a minibuffer window.
   (If this is 0, F must use some other minibuffer window.)  */
#define FRAME_HAS_MINIBUF_P(f) ((f)->has_minibuffer)

/* Pixel height of frame F, including non-toolkit menu bar and
   non-toolkit tool bar lines.  */
#define FRAME_PIXEL_HEIGHT(f) ((f)->pixel_height)

/* Pixel width of frame F.  */
#define FRAME_PIXEL_WIDTH(f) ((f)->pixel_width)

/* Height of frame F, measured in canonical lines, including
   non-toolkit menu bar and non-toolkit tool bar lines.  */
#define FRAME_LINES(f) (f)->text_lines

/* Width of frame F, measured in canonical character columns,
   not including scroll bars if any.  */
#define FRAME_COLS(f) (f)->text_cols

/* Number of lines of frame F used for menu bar.
   This is relevant on terminal frames and on
   X Windows when not using the X toolkit.
   These lines are counted in FRAME_LINES.  */
#define FRAME_MENU_BAR_LINES(f) (f)->menu_bar_lines

/* Nonzero if this frame should display a tool bar
   in a way that does not use any text lines.  */
#if defined (USE_GTK) || defined (HAVE_NS)
#define FRAME_EXTERNAL_TOOL_BAR(f) (f)->external_tool_bar
#else
#define FRAME_EXTERNAL_TOOL_BAR(f) 0
#endif

/* Number of lines of frame F used for the tool-bar.  */

#define FRAME_TOOL_BAR_LINES(f) (f)->tool_bar_lines


/* Lines above the top-most window in frame F.  */

#define FRAME_TOP_MARGIN(F) \
  (FRAME_MENU_BAR_LINES (F) + FRAME_TOOL_BAR_LINES (F))

/* Pixel height of the top margin above.  */

#define FRAME_TOP_MARGIN_HEIGHT(f) \
  (FRAME_TOP_MARGIN (f) * FRAME_LINE_HEIGHT (f))

/* Nonzero if this frame should display a menu bar
   in a way that does not use any text lines.  */
#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) \
     || defined (HAVE_NS) || defined (USE_GTK)
#define FRAME_EXTERNAL_MENU_BAR(f) (f)->external_menu_bar
#else
#define FRAME_EXTERNAL_MENU_BAR(f) 0
#endif
#define FRAME_VISIBLE_P(f) ((f)->visible != 0)

/* Nonzero if frame F is currently visible but hidden.  */
#define FRAME_OBSCURED_P(f) ((f)->visible > 1)

/* Nonzero if frame F is currently iconified.  */
#define FRAME_ICONIFIED_P(f) (f)->iconified

#define FRAME_SET_VISIBLE(f,p) \
  ((f)->async_visible = (p), FRAME_SAMPLE_VISIBILITY (f))
#define SET_FRAME_GARBAGED(f) (frame_garbaged = 1, f->garbaged = 1)
#define FRAME_GARBAGED_P(f) (f)->garbaged

/* Nonzero means do not allow splitting this frame's window.  */
#define FRAME_NO_SPLIT_P(f) (f)->no_split

/* Not really implemented.  */
#define FRAME_WANTS_MODELINE_P(f) (f)->wants_modeline

/* Nonzero if a size change has been requested for frame F
   but not yet really put into effect.  This can be true temporarily
   when an X event comes in at a bad time.  */
#define FRAME_WINDOW_SIZES_CHANGED(f) (f)->window_sizes_changed

/* The minibuffer window of frame F, if it has one; otherwise nil.  */
#define FRAME_MINIBUF_WINDOW(f) (f)->minibuffer_window

/* The root window of the window tree of frame F.  */
#define FRAME_ROOT_WINDOW(f) (f)->root_window

/* The currently selected window of the window tree of frame F.  */
#define FRAME_SELECTED_WINDOW(f) (f)->selected_window

#define FRAME_INSERT_COST(f) (f)->insert_line_cost
#define FRAME_DELETE_COST(f) (f)->delete_line_cost
#define FRAME_INSERTN_COST(f) (f)->insert_n_lines_cost
#define FRAME_DELETEN_COST(f) (f)->delete_n_lines_cost
#define FRAME_MESSAGE_BUF(f) (f)->message_buf
#define FRAME_SCROLL_BOTTOM_VPOS(f) (f)->scroll_bottom_vpos
#define FRAME_FOCUS_FRAME(f) (f)->focus_frame

/* Nonzero if frame F supports scroll bars.
   If this is zero, then it is impossible to enable scroll bars
   on frame F.  */
#define FRAME_CAN_HAVE_SCROLL_BARS(f) ((f)->can_have_scroll_bars)

/* This frame slot says whether scroll bars are currently enabled for frame F,
   and which side they are on.  */
#define FRAME_VERTICAL_SCROLL_BAR_TYPE(f) ((f)->vertical_scroll_bar_type)
#define FRAME_HAS_VERTICAL_SCROLL_BARS(f) \
  ((f)->vertical_scroll_bar_type != vertical_scroll_bar_none)
#define FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT(f) \
  ((f)->vertical_scroll_bar_type == vertical_scroll_bar_left)
#define FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT(f) \
  ((f)->vertical_scroll_bar_type == vertical_scroll_bar_right)

/* Width that a scroll bar in frame F should have, if there is one.
   Measured in pixels.
   If scroll bars are turned off, this is still nonzero.  */
#define FRAME_CONFIG_SCROLL_BAR_WIDTH(f) ((f)->config_scroll_bar_width)

/* Width that a scroll bar in frame F should have, if there is one.
   Measured in columns (characters).
   If scroll bars are turned off, this is still nonzero.  */
#define FRAME_CONFIG_SCROLL_BAR_COLS(f) ((f)->config_scroll_bar_cols)

/* Width of a scroll bar in frame F, measured in columns (characters),
   but only if scroll bars are on the left.  If scroll bars are on
   the right in this frame, or there are no scroll bars, value is 0.  */

#define FRAME_LEFT_SCROLL_BAR_COLS(f)			\
  (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f)		\
   ? FRAME_CONFIG_SCROLL_BAR_COLS (f)			\
   : 0)

/* Width of a left scroll bar in frame F, measured in pixels */

#define FRAME_LEFT_SCROLL_BAR_AREA_WIDTH(f)				\
  (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f)				\
   ? (FRAME_CONFIG_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f))	\
   : 0)

/* Width of a scroll bar in frame F, measured in columns (characters),
   but only if scroll bars are on the right.  If scroll bars are on
   the left in this frame, or there are no scroll bars, value is 0.  */

#define FRAME_RIGHT_SCROLL_BAR_COLS(f)			\
  (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f)		\
   ? FRAME_CONFIG_SCROLL_BAR_COLS (f)			\
   : 0)

/* Width of a right scroll bar area in frame F, measured in pixels */

#define FRAME_RIGHT_SCROLL_BAR_AREA_WIDTH(f)				\
  (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f)				\
   ? (FRAME_CONFIG_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f))	\
   : 0)

/* Actual width of a scroll bar in frame F, measured in columns.  */

#define FRAME_SCROLL_BAR_COLS(f)			\
  (FRAME_HAS_VERTICAL_SCROLL_BARS (f)			\
   ? FRAME_CONFIG_SCROLL_BAR_COLS (f)			\
   : 0)

/* Actual width of a scroll bar area in frame F, measured in pixels.  */

#define FRAME_SCROLL_BAR_AREA_WIDTH(f)					\
  (FRAME_HAS_VERTICAL_SCROLL_BARS (f)					\
   ? (FRAME_CONFIG_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f))	\
   : 0)

/* Total width of frame F, in columns (characters),
   including the width used by scroll bars if any.  */

#define FRAME_TOTAL_COLS(f) ((f)->total_cols)

/* Set the width of frame F to VAL.
   VAL is the width of a full-frame window,
   not including scroll bars and fringes.  */

#define SET_FRAME_COLS(f, val)						\
  (FRAME_COLS (f) = (val),						\
   (f)->total_cols = FRAME_TOTAL_COLS_ARG (f, FRAME_COLS (f)))

/* Given a value WIDTH for frame F's nominal width,
   return the value that FRAME_TOTAL_COLS should have.  */

#define FRAME_TOTAL_COLS_ARG(f, width)		\
  ((width)					\
   + FRAME_SCROLL_BAR_COLS (f)			\
   + FRAME_FRINGE_COLS (f))

/* Maximum + 1 legitimate value for FRAME_CURSOR_X.  */

#define FRAME_CURSOR_X_LIMIT(f) \
  (FRAME_COLS (f) + FRAME_LEFT_SCROLL_BAR_COLS (f))

/* Nonzero if frame F has scroll bars.  */

#define FRAME_SCROLL_BARS(f) ((f)->scroll_bars)

#define FRAME_CONDEMNED_SCROLL_BARS(f) ((f)->condemned_scroll_bars)
#define FRAME_MENU_BAR_ITEMS(f) ((f)->menu_bar_items)
#define FRAME_COST_BAUD_RATE(f) ((f)->cost_calculation_baud_rate)

#define FRAME_DESIRED_CURSOR(f) ((f)->desired_cursor)
#define FRAME_BLINK_OFF_CURSOR(f) ((f)->blink_off_cursor)
#define FRAME_CURSOR_WIDTH(f) ((f)->cursor_width)
#define FRAME_BLINK_OFF_CURSOR_WIDTH(f) ((f)->blink_off_cursor_width)

/* Return a pointer to the face cache of frame F.  */

#define FRAME_FACE_CACHE(F)	(F)->face_cache

/* Return the size of message_buf of the frame F.  We multiply the
   width of the frame by 4 because multi-byte form may require at most
   4-byte for a character.  */

#define FRAME_MESSAGE_BUF_SIZE(f) (((int) FRAME_COLS (f)) * 4)

/* Emacs's redisplay code could become confused if a frame's
   visibility changes at arbitrary times.  For example, if a frame is
   visible while the desired glyphs are being built, but becomes
   invisible before they are updated, then some rows of the
   desired_glyphs will be left marked as enabled after redisplay is
   complete, which should never happen.  The next time the frame
   becomes visible, redisplay will probably barf.

   Currently, there are no similar situations involving iconified, but
   the principle is the same.

   So instead of having asynchronous input handlers directly set and
   clear the frame's visibility and iconification flags, they just set
   the async_visible and async_iconified flags; the redisplay code
   calls the FRAME_SAMPLE_VISIBILITY macro before doing any redisplay,
   which sets visible and iconified from their asynchronous
   counterparts.

   Synchronous code must use the FRAME_SET_VISIBLE macro.

   Also, if a frame used to be invisible, but has just become visible,
   it must be marked as garbaged, since redisplay hasn't been keeping
   up its contents.

   Note that a tty frame is visible if and only if it is the topmost
   frame. */

#define FRAME_SAMPLE_VISIBILITY(f) \
  (((f)->async_visible && (f)->visible != (f)->async_visible) ? \
   SET_FRAME_GARBAGED (f) : 0, \
   (f)->visible = (f)->async_visible, \
   (f)->iconified = (f)->async_iconified)

#define CHECK_FRAME(x) \
  CHECK_TYPE (FRAMEP (x), Qframep, x)

#define CHECK_LIVE_FRAME(x) \
  CHECK_TYPE (FRAMEP (x) && FRAME_LIVE_P (XFRAME (x)), Qframe_live_p, x)

/* FOR_EACH_FRAME (LIST_VAR, FRAME_VAR) followed by a statement is a
   `for' loop which iterates over the elements of Vframe_list.  The
   loop will set FRAME_VAR, a Lisp_Object, to each frame in
   Vframe_list in succession and execute the statement.  LIST_VAR
   should be a Lisp_Object too; it is used to iterate through the
   Vframe_list.

   This macro is a holdover from a time when multiple frames weren't always
   supported.  An alternate definition of the macro would expand to
   something which executes the statement once.  */

#define FOR_EACH_FRAME(list_var, frame_var)			\
  for ((list_var) = Vframe_list;				\
       (CONSP (list_var)					\
	&& (frame_var = XCAR (list_var), 1));		\
       list_var = XCDR (list_var))


extern Lisp_Object Qframep, Qframe_live_p;
extern Lisp_Object Qtty, Qtty_type;
extern Lisp_Object Qtty_color_mode;
extern Lisp_Object Qterminal, Qterminal_live_p;
extern Lisp_Object Qnoelisp;

extern struct frame *last_nonminibuf_frame;

extern void set_menu_bar_lines (struct frame *, Lisp_Object, Lisp_Object);
extern struct frame *make_initial_frame (void);
extern struct frame *make_frame (int);
#ifdef HAVE_WINDOW_SYSTEM
extern struct frame *make_minibuffer_frame (void);
extern struct frame *make_frame_without_minibuffer (Lisp_Object,
                                                    struct kboard *,
                                                    Lisp_Object);
#endif /* HAVE_WINDOW_SYSTEM */
extern void frame_make_pointer_invisible (void);
extern void frame_make_pointer_visible (void);
extern Lisp_Object delete_frame (Lisp_Object, Lisp_Object);

extern Lisp_Object Vframe_list;

/* The currently selected frame.  */

extern Lisp_Object selected_frame;

/* Value is a pointer to the selected frame.  If the selected frame
   isn't live, abort.  */

#define SELECTED_FRAME()				\
     ((FRAMEP (selected_frame)				\
       && FRAME_LIVE_P (XFRAME (selected_frame)))	\
      ? XFRAME (selected_frame)				\
      : (abort (), (struct frame *) 0))


/***********************************************************************
			Display-related Macros
 ***********************************************************************/

/* Canonical y-unit on frame F.
   This value currently equals the line height of the frame (which is
   the height of the default font of F).  */

#define FRAME_LINE_HEIGHT(F) ((F)->line_height)

/* Canonical x-unit on frame F.
   This value currently equals the average width of the default font of F.  */

#define FRAME_COLUMN_WIDTH(F) ((F)->column_width)

/* Space glyph width of the default font of frame F.  */

#define FRAME_SPACE_WIDTH(F) ((F)->space_width)


/* Pixel width of areas used to display truncation marks, continuation
   marks, overlay arrows.  This is 0 for terminal frames.  */

#ifdef HAVE_WINDOW_SYSTEM

/* Total width of fringes reserved for drawing truncation bitmaps,
   continuation bitmaps and alike.  The width is in canonical char
   units of the frame.  This must currently be the case because window
   sizes aren't pixel values.  If it weren't the case, we wouldn't be
   able to split windows horizontally nicely.  */

#define FRAME_FRINGE_COLS(F) ((F)->fringe_cols)

/* Pixel-width of the left and right fringe.  */

#define FRAME_LEFT_FRINGE_WIDTH(F) ((F)->left_fringe_width)
#define FRAME_RIGHT_FRINGE_WIDTH(F) ((F)->right_fringe_width)

/* Total width of fringes in pixels.  */

#define FRAME_TOTAL_FRINGE_WIDTH(F) \
  (FRAME_LEFT_FRINGE_WIDTH (F) + FRAME_RIGHT_FRINGE_WIDTH (F))


/* Pixel-width of internal border lines */

#define FRAME_INTERNAL_BORDER_WIDTH(F) ((F)->internal_border_width)

#else /* not HAVE_WINDOW_SYSTEM */

#define FRAME_FRINGE_COLS(F)	0
#define FRAME_TOTAL_FRINGE_WIDTH(F)	0
#define FRAME_LEFT_FRINGE_WIDTH(F)  0
#define FRAME_RIGHT_FRINGE_WIDTH(F) 0
#define FRAME_INTERNAL_BORDER_WIDTH(F) 0

#endif /* not HAVE_WINDOW_SYSTEM */




/***********************************************************************
	    Conversion between canonical units and pixels
 ***********************************************************************/

/* Canonical x-values are fractions of FRAME_COLUMN_WIDTH, canonical
   y-unit are fractions of FRAME_LINE_HEIGHT of a frame.  Both are
   represented as Lisp numbers, i.e. integers or floats.  */

/* Convert canonical value X to pixels.  F is the frame whose
   canonical char width is to be used.  X must be a Lisp integer or
   float.  Value is a C integer.  */

#define FRAME_PIXEL_X_FROM_CANON_X(F, X)		\
  (INTEGERP (X)						\
   ? XINT (X) * FRAME_COLUMN_WIDTH (F)			\
   : (int) (XFLOAT_DATA (X) * FRAME_COLUMN_WIDTH (F)))

/* Convert canonical value Y to pixels.  F is the frame whose
   canonical character height is to be used.  X must be a Lisp integer
   or float.  Value is a C integer.  */

#define FRAME_PIXEL_Y_FROM_CANON_Y(F, Y)		\
  (INTEGERP (Y)						\
   ? XINT (Y) * FRAME_LINE_HEIGHT (F)			\
   : (int) (XFLOAT_DATA (Y) * FRAME_LINE_HEIGHT (F)))

/* Convert pixel-value X to canonical units.  F is the frame whose
   canonical character width is to be used.  X is a C integer.  Result
   is a Lisp float if X is not a multiple of the canon width,
   otherwise it's a Lisp integer.  */

#define FRAME_CANON_X_FROM_PIXEL_X(F, X)			\
  ((X) % FRAME_COLUMN_WIDTH (F) != 0				\
   ? make_float ((double) (X) / FRAME_COLUMN_WIDTH (F))		\
   : make_number ((X) / FRAME_COLUMN_WIDTH (F)))

/* Convert pixel-value Y to canonical units.  F is the frame whose
   canonical character height is to be used.  Y is a C integer.
   Result is a Lisp float if Y is not a multiple of the canon width,
   otherwise it's a Lisp integer.  */

#define FRAME_CANON_Y_FROM_PIXEL_Y(F, Y)			\
  ((Y) % FRAME_LINE_HEIGHT (F)					\
   ? make_float ((double) (Y) / FRAME_LINE_HEIGHT (F))		\
   : make_number ((Y) / FRAME_LINE_HEIGHT (F)))



/* Manipulating pixel sizes and character sizes.
   Knowledge of which factors affect the overall size of the window should
   be hidden in these macros, if that's possible.

   Return the upper/left pixel position of the character cell on frame F
   at ROW/COL.  */

#define FRAME_LINE_TO_PIXEL_Y(f, row) \
  (((row) < FRAME_TOP_MARGIN (f) ? 0 : FRAME_INTERNAL_BORDER_WIDTH (f))	\
   + (row) * FRAME_LINE_HEIGHT (f))

#define FRAME_COL_TO_PIXEL_X(f, col) \
  (FRAME_INTERNAL_BORDER_WIDTH (f) \
   + (col) * FRAME_COLUMN_WIDTH (f))

/* Return the pixel width/height of frame F if it has
   COLS columns/LINES rows.  */

#define FRAME_TEXT_COLS_TO_PIXEL_WIDTH(f, cols) \
  (FRAME_COL_TO_PIXEL_X (f, cols) \
   + (f)->scroll_bar_actual_width \
   + FRAME_TOTAL_FRINGE_WIDTH (f)      \
   + FRAME_INTERNAL_BORDER_WIDTH (f))

#define FRAME_TEXT_LINES_TO_PIXEL_HEIGHT(f, lines) \
  ((lines) * FRAME_LINE_HEIGHT (f) \
   + 2 * FRAME_INTERNAL_BORDER_WIDTH (f))


/* Return the row/column (zero-based) of the character cell containing
   the pixel on FRAME at Y/X.  */

#define FRAME_PIXEL_Y_TO_LINE(f, y) \
  (((y) < FRAME_TOP_MARGIN_HEIGHT (f)	\
    ? (y)	\
    : ((y) < FRAME_TOP_MARGIN_HEIGHT (f) + FRAME_INTERNAL_BORDER_WIDTH (f) \
       ? (y) - (FRAME_TOP_MARGIN_HEIGHT (f) + FRAME_INTERNAL_BORDER_WIDTH (f) \
		/* Arrange for the division to round down.  */	\
		+ FRAME_LINE_HEIGHT (f) - 1)	\
       : (y) - FRAME_INTERNAL_BORDER_WIDTH (f)))	\
   / FRAME_LINE_HEIGHT (f))

#define FRAME_PIXEL_X_TO_COL(f, x) \
  (((x) - FRAME_INTERNAL_BORDER_WIDTH (f))	\
   / FRAME_COLUMN_WIDTH (f))

/* How many columns/rows of text can we fit in WIDTH/HEIGHT pixels on
   frame F?  */

#define FRAME_PIXEL_WIDTH_TO_TEXT_COLS(f, width)		\
  (FRAME_PIXEL_X_TO_COL (f, ((width)				\
			     - FRAME_INTERNAL_BORDER_WIDTH (f)	\
			     - FRAME_TOTAL_FRINGE_WIDTH (f)	\
			     - (f)->scroll_bar_actual_width)))

#define FRAME_PIXEL_HEIGHT_TO_TEXT_LINES(f, height) \
  (FRAME_PIXEL_Y_TO_LINE (f, ((height) \
			      - FRAME_INTERNAL_BORDER_WIDTH (f))))


/***********************************************************************
				Frame Parameters
 ***********************************************************************/

extern Lisp_Object Qauto_raise, Qauto_lower;
extern Lisp_Object Qborder_color, Qborder_width;
extern Lisp_Object Qbuffer_predicate;
extern Lisp_Object Qcursor_color, Qcursor_type;
extern Lisp_Object Qfont;
extern Lisp_Object Qbackground_color, Qforeground_color;
extern Lisp_Object Qicon, Qicon_name, Qicon_type, Qicon_left, Qicon_top;
extern Lisp_Object Qinternal_border_width;
extern Lisp_Object Qtooltip;
extern Lisp_Object Qmenu_bar_lines, Qtool_bar_lines, Qtool_bar_position;
extern Lisp_Object Qmouse_color;
extern Lisp_Object Qname, Qtitle;
extern Lisp_Object Qparent_id;
extern Lisp_Object Qunsplittable, Qvisibility;
extern Lisp_Object Qscroll_bar_width, Qvertical_scroll_bars;
extern Lisp_Object Qscroll_bar_foreground, Qscroll_bar_background;
extern Lisp_Object Qscreen_gamma;
extern Lisp_Object Qline_spacing;
extern Lisp_Object Qwait_for_wm;
extern Lisp_Object Qfullscreen;
extern Lisp_Object Qfullwidth, Qfullheight, Qfullboth, Qmaximized;
extern Lisp_Object Qsticky;
extern Lisp_Object Qfont_backend;
extern Lisp_Object Qalpha;

extern Lisp_Object Qleft_fringe, Qright_fringe;
extern Lisp_Object Qheight, Qwidth;
extern Lisp_Object Qminibuffer, Qmodeline;
extern Lisp_Object Qx, Qw32, Qmac, Qpc, Qns;
extern Lisp_Object Qvisible;
extern Lisp_Object Qdisplay_type;

extern Lisp_Object Qx_resource_name;

extern Lisp_Object Qleft, Qright, Qtop, Qbox, Qbottom;
extern Lisp_Object Qdisplay;

extern Lisp_Object Qrun_hook_with_args;

#ifdef HAVE_WINDOW_SYSTEM

/* The class of this X application.  */
#define EMACS_CLASS "Emacs"

/* These are in xterm.c, w32term.c, etc.  */

extern void x_set_scroll_bar_default_width (struct frame *);
extern void x_set_offset (struct frame *, int, int, int);
extern void x_wm_set_icon_position (struct frame *, int, int);

extern Lisp_Object x_new_font (struct frame *, Lisp_Object, int);


extern Lisp_Object Qface_set_after_frame_default;

#ifdef WINDOWSNT
extern void x_fullscreen_adjust (struct frame *f, int *, int *,
                                 int *, int *);
#endif

extern void x_set_frame_parameters (struct frame *, Lisp_Object);

extern void x_set_fullscreen (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_line_spacing (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_screen_gamma (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_font (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_font_backend (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_fringe_width (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_border_width (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_internal_border_width (struct frame *, Lisp_Object,
                                         Lisp_Object);
extern void x_set_visibility (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_autoraise (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_autolower (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_unsplittable (struct frame *, Lisp_Object, Lisp_Object);
extern void x_set_vertical_scroll_bars (struct frame *, Lisp_Object,
                                        Lisp_Object);
extern void x_set_scroll_bar_width (struct frame *, Lisp_Object,
                                    Lisp_Object);

extern Lisp_Object x_icon_type (struct frame *);

extern int x_figure_window_size (struct frame *, Lisp_Object, int);

extern void x_set_alpha (struct frame *, Lisp_Object, Lisp_Object);

extern void validate_x_resource_name (void);

extern Lisp_Object display_x_get_resource (Display_Info *,
					   Lisp_Object attribute,
					   Lisp_Object class,
					   Lisp_Object component,
					   Lisp_Object subclass);

#if defined HAVE_X_WINDOWS && !defined USE_X_TOOLKIT
extern char *x_get_resource_string (const char *, const char *);
#endif

/* In xmenu.c */
extern void set_frame_menubar (FRAME_PTR, int, int);

#endif /* HAVE_WINDOW_SYSTEM */

#endif /* not EMACS_FRAME_H */
