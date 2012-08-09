/* Definitions and headers for communication on the Microsoft W32 API.
   Copyright (C) 1995, 2001-2012 Free Software Foundation, Inc.

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

/* Added by Kevin Gallo */

#include "w32gui.h"


#define BLACK_PIX_DEFAULT(f) PALETTERGB(0,0,0)
#define WHITE_PIX_DEFAULT(f) PALETTERGB(255,255,255)

#define FONT_WIDTH(f)     ((f)->max_width)
#define FONT_HEIGHT(f)    ((f)->height)
#define FONT_BASE(f)      ((f)->ascent)
#define FONT_DESCENT(f)   ((f)->descent)

#define CP_DEFAULT 1004

#define CHECK_W32_FRAME(f, frame)		\
  if (NILP (frame))				\
    f = SELECTED_FRAME ();			\
  else						\
    {						\
      CHECK_LIVE_FRAME (frame, 0);		\
      f = XFRAME (frame);			\
    }						\
  if (! FRAME_W32_P (f))

/* Indicates whether we are in the readsocket call and the message we
   are processing in the current loop */

extern MSG CurMsg;
extern BOOL bUseDflt;

/* Structure recording bitmaps and reference count.
   If REFCOUNT is 0 then this record is free to be reused.  */

struct w32_bitmap_record
{
  Pixmap pixmap;
  char *file;
  HINSTANCE hinst; /* Used to load the file */
  int refcount;
  /* Record some info about this pixmap.  */
  int height, width, depth;
};

struct w32_palette_entry {
  struct w32_palette_entry * next;
  PALETTEENTRY entry;
#if 0
  unsigned refcount;
#endif
};

extern void w32_regenerate_palette (struct frame *f);


/* For each display (currently only one on w32), we have a structure that
   records information about it.  */

struct w32_display_info
{
  /* Chain of all w32_display_info structures.  */
  struct w32_display_info *next;

  /* The generic display parameters corresponding to this w32 display.  */
  struct terminal *terminal;

  /* This is a cons cell of the form (NAME . FONT-LIST-CACHE).
     The same cons cell also appears in x_display_name_list.  */
  Lisp_Object name_list_element;

  /* Number of frames that are on this display.  */
  int reference_count;

  /* Dots per inch of the screen.  */
  double resx, resy;

  /* Number of planes on this screen.  */
  int n_planes;

  /* Number of bits per pixel on this screen.  */
  int n_cbits;

  /* Mask of things that cause the mouse to be grabbed.  */
  int grabbed;

  /* Emacs bitmap-id of the default icon bitmap for this frame.
     Or -1 if none has been allocated yet.  */
  ptrdiff_t icon_bitmap_id;

  /* The root window of this screen.  */
  Window root_window;

  /* The cursor to use for vertical scroll bars.  */
  Cursor vertical_scroll_bar_cursor;

  /* Resource data base */
  XrmDatabase xrdb;

  /* color palette information.  */
  int has_palette;
  struct w32_palette_entry * color_list;
  unsigned num_colors;
  HPALETTE palette;

  /* deferred action flags checked when starting frame update.  */
  int regen_palette;

  /* Keystroke that has been faked by Emacs and will be ignored when
     received; value is reset after key is received.  */
  int faked_key;

  /* Minimum width over all characters in all fonts in font_table.  */
  int smallest_char_width;

  /* Minimum font height over all fonts in font_table.  */
  int smallest_font_height;

  /* Reusable Graphics Context for drawing a cursor in a non-default face. */
  XGCValues *scratch_cursor_gc;

  /* Information about the range of text currently shown in
     mouse-face.  */
  Mouse_HLInfo mouse_highlight;

  char *w32_id_name;

  /* The number of fonts actually stored in w32_font_table.
     font_table[n] is used and valid if 0 <= n < n_fonts. 0 <=
     n_fonts <= font_table_size. and font_table[i].name != 0. */
  int n_fonts;

  /* Pointer to bitmap records.  */
  struct w32_bitmap_record *bitmaps;

  /* Allocated size of bitmaps field.  */
  ptrdiff_t bitmaps_size;

  /* Last used bitmap index.  */
  ptrdiff_t bitmaps_last;

  /* The frame (if any) which has the window that has keyboard focus.
     Zero if none.  This is examined by Ffocus_frame in w32fns.c.  Note
     that a mere EnterNotify event can set this; if you need to know the
     last frame specified in a FocusIn or FocusOut event, use
     w32_focus_event_frame.  */
  struct frame *w32_focus_frame;

  /* The last frame mentioned in a FocusIn or FocusOut event.  This is
     separate from w32_focus_frame, because whether or not LeaveNotify
     events cause us to lose focus depends on whether or not we have
     received a FocusIn event for it.  */
  struct frame *w32_focus_event_frame;

  /* The frame which currently has the visual highlight, and should get
     keyboard input (other sorts of input have the frame encoded in the
     event).  It points to the focus frame's selected window's
     frame.  It differs from w32_focus_frame when we're using a global
     minibuffer.  */
  struct frame *x_highlight_frame;
};

/* This is a chain of structures for all the displays currently in use.  */
extern struct w32_display_info *x_display_list;
extern struct w32_display_info one_w32_display_info;

/* This is a list of cons cells, each of the form (NAME . FONT-LIST-CACHE),
   one for each element of w32_display_list and in the same order.
   NAME is the name of the frame.
   FONT-LIST-CACHE records previous values returned by x-list-fonts.  */
extern Lisp_Object w32_display_name_list;

extern struct frame *x_window_to_frame (struct w32_display_info *, HWND);

struct w32_display_info *x_display_info_for_name (Lisp_Object);

Lisp_Object display_x_get_resource (struct w32_display_info *,
                                    Lisp_Object, Lisp_Object,
                                    Lisp_Object, Lisp_Object);

extern struct w32_display_info *w32_term_init (Lisp_Object,
					       char *, char *);

extern int x_display_pixel_height (struct w32_display_info *);
extern int x_display_pixel_width (struct w32_display_info *);


#define PIX_TYPE COLORREF

/* Each W32 frame object points to its own struct w32_display object
   in the output_data.w32 field.  The w32_display structure contains all
   the information that is specific to W32 windows.  */

/* Put some things in x_output for compatibility.
   NTEMACS_TODO: Move all common things here to eliminate unnecessary
   diffs between X and w32 code.  */
struct x_output
{
#if 0 /* These are also defined in struct frame.  Use that instead.  */
  PIX_TYPE background_pixel;
  PIX_TYPE foreground_pixel;
#endif

  /* Keep track of focus.  May be EXPLICIT if we received a FocusIn for this
     frame, or IMPLICIT if we received an EnterNotify.
     FocusOut and LeaveNotify clears EXPLICIT/IMPLICIT. */
  int focus_state;

};

enum
{
  /* Values for focus_state, used as bit mask.
     EXPLICIT means we received a FocusIn for the frame and know it has
     the focus.  IMPLICIT means we received an EnterNotify and the frame
     may have the focus if no window manager is running.
     FocusOut and LeaveNotify clears EXPLICIT/IMPLICIT. */
  FOCUS_NONE     = 0,
  FOCUS_IMPLICIT = 1,
  FOCUS_EXPLICIT = 2
};

struct w32_output
{
  /* Placeholder for things accessed through output_data.x.  */
  struct x_output x_compatible;

  /* Menubar "widget" handle.  */
  HMENU menubar_widget;

  /* Original palette (used to deselect real palette after drawing) */
  HPALETTE old_palette;

  /* Here are the Graphics Contexts for the default font.  */
  XGCValues *cursor_gc;				/* cursor drawing */

  /* The window used for this frame.
     May be zero while the frame object is being created
     and the window has not yet been created.  */
  Window window_desc;

  /* The window that is the parent of this window.
     Usually this is a window that was made by the window manager,
     but it can be the root window, and it can be explicitly specified
     (see the explicit_parent field, below).  */
  Window parent_desc;

  /* Default ASCII font of this frame. */
  struct font *font;

  /* The baseline offset of the default ASCII font.  */
  int baseline_offset;

  /* If a fontset is specified for this frame instead of font, this
     value contains an ID of the fontset, else -1.  */
  int fontset;

  /* Pixel values used for various purposes.
     border_pixel may be -1 meaning use a gray tile.  */
  COLORREF cursor_pixel;
  COLORREF border_pixel;
  COLORREF mouse_pixel;
  COLORREF cursor_foreground_pixel;

  /* Foreground color for scroll bars.  A value of -1 means use the
     default (black for non-toolkit scroll bars).  */
  COLORREF scroll_bar_foreground_pixel;

  /* Background color for scroll bars.  A value of -1 means use the
     default (background color of the frame for non-toolkit scroll
     bars).  */
  COLORREF scroll_bar_background_pixel;

  /* Descriptor for the cursor in use for this window.  */
  Cursor text_cursor;
  Cursor nontext_cursor;
  Cursor modeline_cursor;
  Cursor hand_cursor;
  Cursor hourglass_cursor;
  Cursor horizontal_drag_cursor;

  /* Non-zero means hourglass cursor is currently displayed.  */
  unsigned hourglass_p : 1;

  /* Non-hourglass cursor that is currently active.  */
  Cursor current_cursor;

  /* Flag to set when the window needs to be completely repainted.  */
  int needs_exposure;

  DWORD dwStyle;

  /* This is the Emacs structure for the display this frame is on.  */
  /* struct w32_display_info *display_info; */

  /* Nonzero means our parent is another application's window
     and was explicitly specified.  */
  char explicit_parent;

  /* Nonzero means tried already to make this frame visible.  */
  char asked_for_visible;

  /* Nonzero means menubar is currently active.  */
  char menubar_active;

  /* Nonzero means menubar is about to become active, but should be
     brought up to date first.  */
  volatile char pending_menu_activation;

  /* Relief GCs, colors etc.  */
  struct relief
  {
    XGCValues *gc;
    unsigned long pixel;
    int allocated_p;
  }
  black_relief, white_relief;

  /* The background for which the above relief GCs were set up.
     They are changed only when a different background is involved.  */
  unsigned long relief_background;
};

extern struct w32_output w32term_display;

/* Return the X output data for frame F.  */
#define FRAME_X_OUTPUT(f) ((f)->output_data.w32)

/* Return the window associated with the frame F.  */
#define FRAME_W32_WINDOW(f) ((f)->output_data.w32->window_desc)
#define FRAME_X_WINDOW(f) ((f)->output_data.w32->window_desc)

#define FRAME_FONT(f) ((f)->output_data.w32->font)
#define FRAME_FONTSET(f) ((f)->output_data.w32->fontset)
#define FRAME_BASELINE_OFFSET(f) ((f)->output_data.w32->baseline_offset)

/* This gives the w32_display_info structure for the display F is on.  */
#define FRAME_W32_DISPLAY_INFO(f) (&one_w32_display_info)
#define FRAME_X_DISPLAY_INFO(f) (&one_w32_display_info)

/* This is the `Display *' which frame F is on.  */
#define FRAME_X_DISPLAY(f) (0)

/* Value is the smallest width of any character in any font on frame F.  */

#define FRAME_SMALLEST_CHAR_WIDTH(F) \
     FRAME_W32_DISPLAY_INFO(F)->smallest_char_width

/* Value is the smallest height of any font on frame F.  */

#define FRAME_SMALLEST_FONT_HEIGHT(F) \
     FRAME_W32_DISPLAY_INFO(F)->smallest_font_height

/* W32-specific scroll bar stuff.  */

/* We represent scroll bars as lisp vectors.  This allows us to place
   references to them in windows without worrying about whether we'll
   end up with windows referring to dead scroll bars; the garbage
   collector will free it when its time comes.

   We use struct scroll_bar as a template for accessing fields of the
   vector.  */

struct scroll_bar {

  /* These fields are shared by all vectors.  */
  EMACS_INT size_from_Lisp_Vector_struct;
  struct Lisp_Vector *next_from_Lisp_Vector_struct;

  /* The window we're a scroll bar for.  */
  Lisp_Object window;

  /* The next and previous in the chain of scroll bars in this frame.  */
  Lisp_Object next, prev;

  /* The window representing this scroll bar.  Since this is a full
     32-bit quantity, we store it split into two 32-bit values.  */
  Lisp_Object w32_window_low, w32_window_high;

  /* Same as above for the widget.  */
  Lisp_Object w32_widget_low, w32_widget_high;

  /* The position and size of the scroll bar in pixels, relative to the
     frame.  */
  Lisp_Object top, left, width, height;

  /* The starting and ending positions of the handle, relative to the
     handle area (i.e. zero is the top position, not
     SCROLL_BAR_TOP_BORDER).  If they're equal, that means the handle
     hasn't been drawn yet.

     These are not actually the locations where the beginning and end
     are drawn; in order to keep handles from becoming invisible when
     editing large files, we establish a minimum height by always
     drawing handle bottoms VERTICAL_SCROLL_BAR_MIN_HANDLE pixels below
     where they would be normally; the bottom and top are in a
     different co-ordinate system.  */
  Lisp_Object start, end;

  /* If the scroll bar handle is currently being dragged by the user,
     this is the number of pixels from the top of the handle to the
     place where the user grabbed it.  If the handle isn't currently
     being dragged, this is Qnil.  */
  Lisp_Object dragging;

  /* t if the background of the fringe that is adjacent to a scroll
     bar is extended to the gap between the fringe and the bar.  */
  Lisp_Object fringe_extended_p;
};

/* The number of elements a vector holding a struct scroll_bar needs.  */
#define SCROLL_BAR_VEC_SIZE					\
  ((sizeof (struct scroll_bar)					\
    - sizeof (EMACS_INT) - sizeof (struct Lisp_Vector *))	\
   / sizeof (Lisp_Object))

/* Turning a lisp vector value into a pointer to a struct scroll_bar.  */
#define XSCROLL_BAR(vec) ((struct scroll_bar *) XVECTOR (vec))


/* Building a 32-bit C integer from two 16-bit lisp integers.  */
#define SCROLL_BAR_PACK(low, high) (XINT (high) << 16 | XINT (low))

/* Setting two lisp integers to the low and high words of a 32-bit C int.  */
#define SCROLL_BAR_UNPACK(low, high, int32) \
  (XSETINT ((low),   (int32)        & 0xffff), \
   XSETINT ((high), ((int32) >> 16) & 0xffff))


/* Extract the window id of the scroll bar from a struct scroll_bar.  */
#define SCROLL_BAR_W32_WINDOW(ptr) \
  ((Window) SCROLL_BAR_PACK ((ptr)->w32_window_low, (ptr)->w32_window_high))

/* Store a window id in a struct scroll_bar.  */
#define SET_SCROLL_BAR_W32_WINDOW(ptr, id) \
  (SCROLL_BAR_UNPACK ((ptr)->w32_window_low, (ptr)->w32_window_high, (int) id))

/* Extract the X widget of the scroll bar from a struct scroll_bar.  */
#define SCROLL_BAR_X_WIDGET(ptr) \
  ((Widget) SCROLL_BAR_PACK ((ptr)->x_widget_low, (ptr)->x_widget_high))

/* Store a widget id in a struct scroll_bar.  */
#define SET_SCROLL_BAR_X_WIDGET(ptr, w) \
  (SCROLL_BAR_UNPACK ((ptr)->x_widget_low, (ptr)->x_widget_high, (int) w))

/* Return the inside width of a vertical scroll bar, given the outside
   width.  */
#define VERTICAL_SCROLL_BAR_INSIDE_WIDTH(f,width) \
  ((width) \
   - VERTICAL_SCROLL_BAR_LEFT_BORDER \
   - VERTICAL_SCROLL_BAR_RIGHT_BORDER \
   - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2)

/* Return the length of the rectangle within which the top of the
   handle must stay.  This isn't equivalent to the inside height,
   because the scroll bar handle has a minimum height.

   This is the real range of motion for the scroll bar, so when we're
   scaling buffer positions to scroll bar positions, we use this, not
   VERTICAL_SCROLL_BAR_INSIDE_HEIGHT.  */
#define VERTICAL_SCROLL_BAR_TOP_RANGE(f,height) \
  (VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, height) - VERTICAL_SCROLL_BAR_MIN_HANDLE)

/* Return the inside height of vertical scroll bar, given the outside
   height.  See VERTICAL_SCROLL_BAR_TOP_RANGE too.  */
#define VERTICAL_SCROLL_BAR_INSIDE_HEIGHT(f,height) \
  ((height) - VERTICAL_SCROLL_BAR_TOP_BORDER - VERTICAL_SCROLL_BAR_BOTTOM_BORDER)


/* Border widths for scroll bars.

   Scroll bar windows don't have any borders; their border width is
   set to zero, and we redraw borders ourselves.  This makes the code
   a bit cleaner, since we don't have to convert between outside width
   (used when relating to the rest of the screen) and inside width
   (used when sizing and drawing the scroll bar window itself).

   The handle moves up and down/back and forth in a rectangle inset
   from the edges of the scroll bar.  These are widths by which we
   inset the handle boundaries from the scroll bar edges.  */
#define VERTICAL_SCROLL_BAR_LEFT_BORDER (0)
#define VERTICAL_SCROLL_BAR_RIGHT_BORDER (0)
#define VERTICAL_SCROLL_BAR_TOP_BORDER (vertical_scroll_bar_top_border)
#define VERTICAL_SCROLL_BAR_BOTTOM_BORDER (vertical_scroll_bar_bottom_border)

/* Minimum lengths for scroll bar handles, in pixels.  */
#define VERTICAL_SCROLL_BAR_MIN_HANDLE (vertical_scroll_bar_min_handle)

/* Trimming off a few pixels from each side prevents
   text from glomming up against the scroll bar */
#define VERTICAL_SCROLL_BAR_WIDTH_TRIM (0)


struct frame;  /* from frame.h */

extern void w32_fill_rect (struct frame *, HDC, COLORREF, RECT *);
extern void w32_clear_window (struct frame *);

#define w32_fill_area(f,hdc,pix,x,y,nx,ny) \
do { \
    RECT rect; \
    rect.left = x; \
    rect.top = y; \
    rect.right = x + nx; \
    rect.bottom = y + ny; \
    w32_fill_rect (f,hdc,pix,&rect); \
} while (0)

#define w32_clear_rect(f,hdc,lprect) \
  w32_fill_rect (f, hdc, FRAME_BACKGROUND_PIXEL (f), lprect)

#define w32_clear_area(f,hdc,px,py,nx,ny) \
  w32_fill_area (f, hdc, FRAME_BACKGROUND_PIXEL (f), px, py, nx, ny)

/* Define for earlier versions of Visual C */
#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL 		       (WM_MOUSELAST + 1)
#endif /* WM_MOUSEWHEEL */
#ifndef MSH_MOUSEWHEEL
#define MSH_MOUSEWHEEL		       "MSWHEEL_ROLLMSG"
#endif /* MSH_MOUSEWHEEL */
#ifndef WM_XBUTTONDOWN
#define WM_XBUTTONDOWN                 (WM_MOUSEWHEEL + 1)
#define WM_XBUTTONUP                   (WM_MOUSEWHEEL + 2)
#endif /* WM_XBUTTONDOWN */
#ifndef WM_MOUSEHWHEEL
#define WM_MOUSEHWHEEL                 (WM_MOUSEWHEEL + 4)
#endif /* WM_MOUSEHWHEEL  */
#ifndef WM_APPCOMMAND
#define WM_APPCOMMAND 0x319
#define GET_APPCOMMAND_LPARAM(lParam)  (HIWORD(lParam) & 0x7fff)
#endif
#ifndef WM_UNICHAR
#define WM_UNICHAR 0x109
#endif
#ifndef UNICODE_NOCHAR
#define UNICODE_NOCHAR 0xFFFF
#endif

#define WM_EMACS_START                 (WM_USER + 1)
#define WM_EMACS_KILL                  (WM_EMACS_START + 0)
#define WM_EMACS_CREATEWINDOW          (WM_EMACS_START + 1)
#define WM_EMACS_DONE                  (WM_EMACS_START + 2)
#define WM_EMACS_CREATESCROLLBAR       (WM_EMACS_START + 3)
#define WM_EMACS_SHOWWINDOW            (WM_EMACS_START + 4)
#define WM_EMACS_SETWINDOWPOS          (WM_EMACS_START + 5)
#define WM_EMACS_DESTROYWINDOW         (WM_EMACS_START + 6)
#define WM_EMACS_TRACKPOPUPMENU        (WM_EMACS_START + 7)
#define WM_EMACS_SETFOCUS              (WM_EMACS_START + 8)
#define WM_EMACS_SETFOREGROUND         (WM_EMACS_START + 9)
#define WM_EMACS_SETLOCALE             (WM_EMACS_START + 10)
#define WM_EMACS_SETKEYBOARDLAYOUT     (WM_EMACS_START + 11)
#define WM_EMACS_REGISTER_HOT_KEY      (WM_EMACS_START + 12)
#define WM_EMACS_UNREGISTER_HOT_KEY    (WM_EMACS_START + 13)
#define WM_EMACS_TOGGLE_LOCK_KEY       (WM_EMACS_START + 14)
#define WM_EMACS_TRACK_CARET           (WM_EMACS_START + 15)
#define WM_EMACS_DESTROY_CARET         (WM_EMACS_START + 16)
#define WM_EMACS_SHOW_CARET            (WM_EMACS_START + 17)
#define WM_EMACS_HIDE_CARET            (WM_EMACS_START + 18)
#define WM_EMACS_SETCURSOR             (WM_EMACS_START + 19)
#define WM_EMACS_PAINT                 (WM_EMACS_START + 20)
#define WM_EMACS_END                   (WM_EMACS_START + 21)

#define WND_FONTWIDTH_INDEX    (0)
#define WND_LINEHEIGHT_INDEX   (4)
#define WND_BORDER_INDEX       (8)
#define WND_SCROLLBAR_INDEX    (12)
#define WND_BACKGROUND_INDEX   (16)
#define WND_LAST_INDEX         (20)

#define WND_EXTRA_BYTES     (WND_LAST_INDEX)

extern DWORD dwWindowsThreadId;
extern HANDLE hWindowsThread;
extern DWORD dwMainThreadId;
extern HANDLE hMainThread;

typedef struct W32Msg {
    MSG msg;
    DWORD dwModifiers;
    RECT rect;
} W32Msg;

/* Structure for recording message when input thread must return a
   result that depends on lisp thread to compute.  Lisp thread can
   complete deferred messages out of order.  */
typedef struct deferred_msg
{
  struct deferred_msg * next;
  W32Msg                w32msg;
  LRESULT               result;
  int                   completed;
} deferred_msg;

extern CRITICAL_SECTION critsect;

extern void init_crit (void);
extern void delete_crit (void);

extern void signal_quit (void);

#define enter_crit() EnterCriticalSection (&critsect)
#define leave_crit() LeaveCriticalSection (&critsect)

extern void select_palette (struct frame * f, HDC hdc);
extern void deselect_palette (struct frame * f, HDC hdc);
extern HDC get_frame_dc (struct frame * f);
extern int release_frame_dc (struct frame * f, HDC hDC);

extern void drain_message_queue (void);

extern BOOL get_next_msg (W32Msg *, BOOL);
extern BOOL post_msg (W32Msg *);
extern void complete_deferred_msg (HWND hwnd, UINT msg, LRESULT result);

extern BOOL parse_button (int, int, int *, int *);

extern void w32_sys_ring_bell (struct frame *f);
extern void x_delete_display (struct w32_display_info *dpyinfo);

/* Keypad command key support.  W32 doesn't have virtual keys defined
   for the function keys on the keypad (they are mapped to the standard
   function keys), so we define our own.  */
#define VK_NUMPAD_BEGIN		0x92
#define VK_NUMPAD_CLEAR		(VK_NUMPAD_BEGIN + 0)
#define VK_NUMPAD_ENTER		(VK_NUMPAD_BEGIN + 1)
#define VK_NUMPAD_PRIOR		(VK_NUMPAD_BEGIN + 2)
#define VK_NUMPAD_NEXT		(VK_NUMPAD_BEGIN + 3)
#define VK_NUMPAD_END		(VK_NUMPAD_BEGIN + 4)
#define VK_NUMPAD_HOME		(VK_NUMPAD_BEGIN + 5)
#define VK_NUMPAD_LEFT		(VK_NUMPAD_BEGIN + 6)
#define VK_NUMPAD_UP		(VK_NUMPAD_BEGIN + 7)
#define VK_NUMPAD_RIGHT		(VK_NUMPAD_BEGIN + 8)
#define VK_NUMPAD_DOWN		(VK_NUMPAD_BEGIN + 9)
#define VK_NUMPAD_INSERT	(VK_NUMPAD_BEGIN + 10)
#define VK_NUMPAD_DELETE	(VK_NUMPAD_BEGIN + 11)

#ifndef VK_LWIN
/* Older compiler environments don't have these defined.  */
#define VK_LWIN			0x5B
#define VK_RWIN			0x5C
#define VK_APPS			0x5D
#endif

/* Support for treating Windows and Apps keys as modifiers.  These
   constants must not overlap with any of the dwControlKeyState flags in
   KEY_EVENT_RECORD.  */
#define LEFT_WIN_PRESSED       0x8000
#define RIGHT_WIN_PRESSED      0x4000
#define APPS_PRESSED           0x2000

/* When compiling on Windows 9x/ME and NT 3.x, the following are not defined
   (even though they are supported on 98 and ME.  */
#ifndef WM_MOUSELEAVE
#define WM_MOUSELEAVE 0x02A3
#define TME_LEAVE 0x00000002;

typedef struct tagTRACKMOUSEEVENT
{
  DWORD cbSize;
  DWORD dwFlags;
  HWND hwndTrack;
  DWORD dwHoverTime;
} TRACKMOUSEEVENT;
#endif

struct image;
struct face;

XGCValues *XCreateGC (void *, Window, unsigned long, XGCValues *);
struct frame * check_x_frame (Lisp_Object);

EXFUN (Fx_display_color_p, 1);
EXFUN (Fx_display_grayscale_p, 1);

typedef DWORD (WINAPI * ClipboardSequence_Proc) (void);
typedef BOOL (WINAPI * AppendMenuW_Proc) (
    IN HMENU,
    IN UINT,
    IN UINT_PTR,
    IN LPCWSTR);

extern HWND w32_system_caret_hwnd;
extern int w32_system_caret_height;
extern int w32_system_caret_x;
extern int w32_system_caret_y;
