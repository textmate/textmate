/* NeXT/Open/GNUstep / MacOSX communication module.

Copyright (C) 1989, 1993-1994, 2005-2006, 2008-2012
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

/*
Originally by Carl Edman
Updated by Christian Limpach (chris@nice.ch)
OpenStep/Rhapsody port by Scott Bender (sbender@harmony-ds.com)
MacOSX/Aqua port by Christophe de Dinechin (descubes@earthlink.net)
GNUstep port and post-20 update by Adrian Robert (arobert@cogsci.ucsd.edu)
*/

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
#include <config.h>

#include <math.h>
#include <sys/types.h>
#include <time.h>
#include <signal.h>
#include <unistd.h>
#include <setjmp.h>

#include "lisp.h"
#include "blockinput.h"
#include "sysselect.h"
#include "nsterm.h"
#include "systime.h"
#include "character.h"
#include "fontset.h"
#include "composite.h"
#include "ccl.h"

#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"

#include "window.h"
#include "keyboard.h"

#include "font.h"

/* call tracing */
#if 0
int term_trace_num = 0;
#define NSTRACE(x)        fprintf (stderr, "%s:%d: [%d] " #x "\n",         \
                                __FILE__, __LINE__, ++term_trace_num)
#else
#define NSTRACE(x)
#endif

extern NSString *NSMenuDidBeginTrackingNotification;

/* ==========================================================================

    Local declarations

   ========================================================================== */

/* Convert a symbol indexed with an NSxxx value to a value as defined
   in keyboard.c (lispy_function_key). I hope this is a correct way
   of doing things... */
static unsigned convert_ns_to_X_keysym[] =
{
  NSHomeFunctionKey,            0x50,
  NSLeftArrowFunctionKey,       0x51,
  NSUpArrowFunctionKey,         0x52,
  NSRightArrowFunctionKey,      0x53,
  NSDownArrowFunctionKey,       0x54,
  NSPageUpFunctionKey,          0x55,
  NSPageDownFunctionKey,        0x56,
  NSEndFunctionKey,             0x57,
  NSBeginFunctionKey,           0x58,
  NSSelectFunctionKey,          0x60,
  NSPrintFunctionKey,           0x61,
  NSExecuteFunctionKey,         0x62,
  NSInsertFunctionKey,          0x63,
  NSUndoFunctionKey,            0x65,
  NSRedoFunctionKey,            0x66,
  NSMenuFunctionKey,            0x67,
  NSFindFunctionKey,            0x68,
  NSHelpFunctionKey,            0x6A,
  NSBreakFunctionKey,           0x6B,

  NSF1FunctionKey,              0xBE,
  NSF2FunctionKey,              0xBF,
  NSF3FunctionKey,              0xC0,
  NSF4FunctionKey,              0xC1,
  NSF5FunctionKey,              0xC2,
  NSF6FunctionKey,              0xC3,
  NSF7FunctionKey,              0xC4,
  NSF8FunctionKey,              0xC5,
  NSF9FunctionKey,              0xC6,
  NSF10FunctionKey,             0xC7,
  NSF11FunctionKey,             0xC8,
  NSF12FunctionKey,             0xC9,
  NSF13FunctionKey,             0xCA,
  NSF14FunctionKey,             0xCB,
  NSF15FunctionKey,             0xCC,
  NSF16FunctionKey,             0xCD,
  NSF17FunctionKey,             0xCE,
  NSF18FunctionKey,             0xCF,
  NSF19FunctionKey,             0xD0,
  NSF20FunctionKey,             0xD1,
  NSF21FunctionKey,             0xD2,
  NSF22FunctionKey,             0xD3,
  NSF23FunctionKey,             0xD4,
  NSF24FunctionKey,             0xD5,

  NSBackspaceCharacter,         0x08,  /* 8: Not on some KBs. */
  NSDeleteCharacter,            0xFF,  /* 127: Big 'delete' key upper right. */
  NSDeleteFunctionKey,          0x9F,  /* 63272: Del forw key off main array. */

  NSTabCharacter,		0x09,
  0x19,				0x09,  /* left tab->regular since pass shift */
  NSCarriageReturnCharacter,	0x0D,
  NSNewlineCharacter,		0x0D,
  NSEnterCharacter,		0x8D,

  0x1B,				0x1B   /* escape */
};

static Lisp_Object Qmodifier_value;
Lisp_Object Qalt, Qcontrol, Qhyper, Qmeta, Qsuper, Qnone;
extern Lisp_Object Qcursor_color, Qcursor_type, Qns, Qleft;

static Lisp_Object QUTF8_STRING;

/* On OS X picks up the default NSGlobalDomain AppleAntiAliasingThreshold,
   the maximum font size to NOT antialias.  On GNUstep there is currently
   no way to control this behavior. */
float ns_antialias_threshold;

/* Used to pick up AppleHighlightColor on OS X */
NSString *ns_selection_color;

NSArray *ns_send_types =0, *ns_return_types =0, *ns_drag_types =0;
NSString *ns_app_name = @"Emacs";  /* default changed later */

/* Display variables */
struct ns_display_info *x_display_list; /* Chain of existing displays */
Lisp_Object ns_display_name_list;
long context_menu_value = 0;

/* display update */
NSPoint last_mouse_motion_position;
static NSRect last_mouse_glyph;
static Time last_mouse_movement_time = 0;
static Lisp_Object last_mouse_motion_frame;
static EmacsScroller *last_mouse_scroll_bar = nil;
static struct frame *ns_updating_frame;
static NSView *focus_view = NULL;
static int ns_window_num = 0;
static NSRect uRect;
static BOOL gsaved = NO;
BOOL ns_in_resize = NO;
static BOOL ns_fake_keydown = NO;
int ns_tmp_flags; /* FIXME */
struct nsfont_info *ns_tmp_font; /* FIXME */
static BOOL ns_menu_bar_is_hidden = NO;
/*static int debug_lock = 0; */

/* event loop */
static BOOL send_appdefined = YES;
static NSEvent *last_appdefined_event = 0;
static NSTimer *timed_entry = 0;
static NSTimer *fd_entry = nil;
static NSTimer *scroll_repeat_entry = nil;
static fd_set select_readfds, t_readfds;
static struct timeval select_timeout;
static int select_nfds;
static NSAutoreleasePool *outerpool;
static struct input_event *emacs_event = NULL;
static struct input_event *q_event_ptr = NULL;
static int n_emacs_events_pending = 0;
static NSMutableArray *ns_pending_files, *ns_pending_service_names,
  *ns_pending_service_args;
static BOOL inNsSelect = 0;

/* Convert modifiers in a NeXTstep event to emacs style modifiers.  */
#define NS_FUNCTION_KEY_MASK 0x800000
#define NSLeftControlKeyMask    (0x000001 | NSControlKeyMask)
#define NSRightControlKeyMask   (0x002000 | NSControlKeyMask)
#define NSLeftCommandKeyMask    (0x000008 | NSCommandKeyMask)
#define NSRightCommandKeyMask   (0x000010 | NSCommandKeyMask)
#define NSLeftAlternateKeyMask  (0x000020 | NSAlternateKeyMask)
#define NSRightAlternateKeyMask (0x000040 | NSAlternateKeyMask)
#define EV_MODIFIERS(e)                               \
    ((([e modifierFlags] & NSHelpKeyMask) ?           \
           hyper_modifier : 0)                        \
     | (!EQ (ns_right_alternate_modifier, Qleft) && \
        (([e modifierFlags] & NSRightAlternateKeyMask) \
         == NSRightAlternateKeyMask) ? \
           parse_solitary_modifier (ns_right_alternate_modifier) : 0) \
     | (([e modifierFlags] & NSAlternateKeyMask) ?                 \
           parse_solitary_modifier (ns_alternate_modifier) : 0)   \
     | (([e modifierFlags] & NSShiftKeyMask) ?     \
           shift_modifier : 0)                        \
     | (!EQ (ns_right_control_modifier, Qleft) && \
        (([e modifierFlags] & NSRightControlKeyMask) \
         == NSRightControlKeyMask) ? \
           parse_solitary_modifier (ns_right_control_modifier) : 0) \
     | (([e modifierFlags] & NSControlKeyMask) ?      \
           parse_solitary_modifier (ns_control_modifier) : 0)     \
     | (([e modifierFlags] & NS_FUNCTION_KEY_MASK) ?  \
           parse_solitary_modifier (ns_function_modifier) : 0)    \
     | (!EQ (ns_right_command_modifier, Qleft) && \
        (([e modifierFlags] & NSRightCommandKeyMask) \
         == NSRightCommandKeyMask) ? \
           parse_solitary_modifier (ns_right_command_modifier) : 0) \
     | (([e modifierFlags] & NSCommandKeyMask) ?      \
           parse_solitary_modifier (ns_command_modifier):0))

#define EV_UDMODIFIERS(e)                                      \
    ((([e type] == NSLeftMouseDown) ? down_modifier : 0)       \
     | (([e type] == NSRightMouseDown) ? down_modifier : 0)    \
     | (([e type] == NSOtherMouseDown) ? down_modifier : 0)    \
     | (([e type] == NSLeftMouseDragged) ? down_modifier : 0)  \
     | (([e type] == NSRightMouseDragged) ? down_modifier : 0) \
     | (([e type] == NSOtherMouseDragged) ? down_modifier : 0) \
     | (([e type] == NSLeftMouseUp)   ? up_modifier   : 0)     \
     | (([e type] == NSRightMouseUp)   ? up_modifier   : 0)    \
     | (([e type] == NSOtherMouseUp)   ? up_modifier   : 0))

#define EV_BUTTON(e)                                                         \
    ((([e type] == NSLeftMouseDown) || ([e type] == NSLeftMouseUp)) ? 0 :    \
      (([e type] == NSRightMouseDown) || ([e type] == NSRightMouseUp)) ? 2 : \
     [e buttonNumber] - 1)

/* Convert the time field to a timestamp in milliseconds. */
#define EV_TIMESTAMP(e) ([e timestamp] * 1000)

/* This is a piece of code which is common to all the event handling
   methods.  Maybe it should even be a function.  */
#define EV_TRAILER(e)                                         \
  {                                                           \
  XSETFRAME (emacs_event->frame_or_window, emacsframe);       \
  if (e) emacs_event->timestamp = EV_TIMESTAMP (e);           \
  n_emacs_events_pending++;                                   \
  kbd_buffer_store_event_hold (emacs_event, q_event_ptr);     \
  EVENT_INIT (*emacs_event);                                  \
  ns_send_appdefined (-1);                                    \
  }

void x_set_cursor_type (struct frame *, Lisp_Object, Lisp_Object);

/* TODO: get rid of need for these forward declarations */
static void ns_condemn_scroll_bars (struct frame *f);
static void ns_judge_scroll_bars (struct frame *f);
void x_set_frame_alpha (struct frame *f);


/* ==========================================================================

    Utilities

   ========================================================================== */


static Lisp_Object
append2 (Lisp_Object list, Lisp_Object item)
/* --------------------------------------------------------------------------
   Utility to append to a list
   -------------------------------------------------------------------------- */
{
  Lisp_Object array[2];
  array[0] = list;
  array[1] = Fcons (item, Qnil);
  return Fnconc (2, &array[0]);
}


void
ns_init_paths (void)
/* --------------------------------------------------------------------------
   Used to allow emacs to find its resources under Emacs.app
   Called from emacs.c at startup.
   -------------------------------------------------------------------------- */
{
  NSBundle *bundle = [NSBundle mainBundle];
  NSString *binDir = [bundle bundlePath], *resourceDir = [bundle resourcePath];
  NSString *resourcePath, *resourcePaths;
  NSRange range;
  BOOL onWindows = NO; /* how do I determine this? */
  NSString *pathSeparator = onWindows ? @";" : @":";
  NSFileManager *fileManager = [NSFileManager defaultManager];
  BOOL isDir;
/*NSLog (@"ns_init_paths: '%@'\n%@\n", [[NSBundle mainBundle] bundlePath], [[NSBundle mainBundle] resourcePath]); */

  /* get bindir from base */
  range = [resourceDir rangeOfString: @"Contents"];
  if (range.location != NSNotFound)
    {
      binDir = [binDir stringByAppendingPathComponent: @"Contents"];
#ifdef NS_IMPL_COCOA
      binDir = [binDir stringByAppendingPathComponent: @"MacOS"];
#endif
    }

  /* the following based on Andrew Choi's init_mac_osx_environment () */
  if (!getenv ("EMACSLOADPATH"))
    {
      NSArray *paths = [resourceDir stringsByAppendingPaths:
                                  [NSArray arrayWithObjects:
                                         @"site-lisp", @"lisp", @"leim", nil]];
      NSEnumerator *pathEnum = [paths objectEnumerator];
      resourcePaths = @"";
      while (resourcePath = [pathEnum nextObject])
        {
          if ([fileManager fileExistsAtPath: resourcePath isDirectory: &isDir])
            if (isDir)
              {
                if ([resourcePaths length] > 0)
                  resourcePaths
		    = [resourcePaths stringByAppendingString: pathSeparator];
                resourcePaths
		  = [resourcePaths stringByAppendingString: resourcePath];
              }
        }
      if ([resourcePaths length] > 0)
        setenv ("EMACSLOADPATH", [resourcePaths UTF8String], 1);
/*NSLog (@"loadPath: '%@'\n", resourcePaths); */
    }

  if (!getenv ("EMACSPATH"))
    {
      NSArray *paths = [binDir stringsByAppendingPaths:
                                  [NSArray arrayWithObjects: @"bin",
                                                             @"lib-exec", nil]];
      NSEnumerator *pathEnum = [paths objectEnumerator];
      resourcePaths = @"";
      while (resourcePath = [pathEnum nextObject])
        {
          if ([fileManager fileExistsAtPath: resourcePath isDirectory: &isDir])
            if (isDir)
              {
                if ([resourcePaths length] > 0)
                  resourcePaths
		    = [resourcePaths stringByAppendingString: pathSeparator];
                resourcePaths
		  = [resourcePaths stringByAppendingString: resourcePath];
              }
        }
      if ([resourcePaths length] > 0)
        setenv ("EMACSPATH", [resourcePaths UTF8String], 1);
    }

  resourcePath = [resourceDir stringByAppendingPathComponent: @"etc"];
  if ([fileManager fileExistsAtPath: resourcePath isDirectory: &isDir])
    {
      if (isDir)
        {
          if (!getenv ("EMACSDATA"))
            setenv ("EMACSDATA", [resourcePath UTF8String], 1);
          if (!getenv ("EMACSDOC"))
            setenv ("EMACSDOC", [resourcePath UTF8String], 1);
        }
    }

  if (!getenv ("INFOPATH"))
    {
      resourcePath = [resourceDir stringByAppendingPathComponent: @"info"];
      if ([fileManager fileExistsAtPath: resourcePath isDirectory: &isDir])
        if (isDir)
          setenv ("INFOPATH", [[resourcePath stringByAppendingString: @":"]
                                             UTF8String], 1);
      /* Note, extra colon needed to cause merge w/later user additions. */
    }
}


static int
timeval_subtract (struct timeval *result, struct timeval x, struct timeval y)
/* --------------------------------------------------------------------------
   Subtract the `struct timeval' values X and Y, storing the result in RESULT.
   Return 1 if the difference is negative, otherwise 0.
   -------------------------------------------------------------------------- */
{
  /* Perform the carry for the later subtraction by updating y.
     This is safer because on some systems
     the tv_sec member is unsigned.  */
  if (x.tv_usec < y.tv_usec)
    {
      int nsec = (y.tv_usec - x.tv_usec) / 1000000 + 1;
      y.tv_usec -= 1000000 * nsec;
      y.tv_sec += nsec;
    }
  if (x.tv_usec - y.tv_usec > 1000000)
    {
      int nsec = (y.tv_usec - x.tv_usec) / 1000000;
      y.tv_usec += 1000000 * nsec;
      y.tv_sec -= nsec;
    }

  /* Compute the time remaining to wait.  tv_usec is certainly positive.  */
  result->tv_sec = x.tv_sec - y.tv_sec;
  result->tv_usec = x.tv_usec - y.tv_usec;

  /* Return indication of whether the result should be considered negative.  */
  return x.tv_sec < y.tv_sec;
}

static void
ns_timeout (int usecs)
/* --------------------------------------------------------------------------
     Blocking timer utility used by ns_ring_bell
   -------------------------------------------------------------------------- */
{
  struct timeval wakeup;

  EMACS_GET_TIME (wakeup);

  /* Compute time to wait until, propagating carry from usecs.  */
  wakeup.tv_usec += usecs;
  wakeup.tv_sec += (wakeup.tv_usec / 1000000);
  wakeup.tv_usec %= 1000000;

  /* Keep waiting until past the time wakeup.  */
  while (1)
    {
      struct timeval timeout;

      EMACS_GET_TIME (timeout);

      /* In effect, timeout = wakeup - timeout.
	 Break if result would be negative.  */
      if (timeval_subtract (&timeout, wakeup, timeout))
	break;

      /* Try to wait that long--but we might wake up sooner.  */
      select (0, NULL, NULL, NULL, &timeout);
    }
}


void
ns_release_object (void *obj)
/* --------------------------------------------------------------------------
    Release an object (callable from C)
   -------------------------------------------------------------------------- */
{
    [(id)obj release];
}


void
ns_retain_object (void *obj)
/* --------------------------------------------------------------------------
    Retain an object (callable from C)
   -------------------------------------------------------------------------- */
{
    [(id)obj retain];
}


void *
ns_alloc_autorelease_pool (void)
/* --------------------------------------------------------------------------
     Allocate a pool for temporary objects (callable from C)
   -------------------------------------------------------------------------- */
{
  return [[NSAutoreleasePool alloc] init];
}


void
ns_release_autorelease_pool (void *pool)
/* --------------------------------------------------------------------------
     Free a pool and temporary objects it refers to (callable from C)
   -------------------------------------------------------------------------- */
{
  ns_release_object (pool);
}



/* ==========================================================================

    Focus (clipping) and screen update

   ========================================================================== */

static NSRect
ns_resize_handle_rect (NSWindow *window)
{
  NSRect r = [window frame];
  r.origin.x = r.size.width - RESIZE_HANDLE_SIZE;
  r.origin.y = 0;
  r.size.width = r.size.height = RESIZE_HANDLE_SIZE;
  return r;
}


//
// Window constraining
// -------------------
//
// To ensure that the windows are not placed under the menu bar, they
// are typically moved by the call-back constrainFrameRect. However,
// by overriding it, it's possible to inhibit this, leaving the window
// in it's original position.
//
// It's possible to hide the menu bar. However, technically, it's only
// possible to hide it when the application is active. To ensure that
// this work properly, the menu bar and window constraining are
// deferred until the application becomes active.
//
// Even though it's not possible to manually move a window above the
// top of the screen, it is allowed if it's done programmatically,
// when the menu is hidden. This allows the editable area to cover the
// full screen height.
//
// Test cases
// ----------
//
// Use the following extra files:
//
//    init.el:
//       ;; Hide menu and place frame slightly above the top of the screen.
//       (setq ns-auto-hide-menu-bar t)
//       (set-frame-position (selected-frame) 0 -20)
//
// Test 1:
//
//    emacs -Q -l init.el
//
//    Result: No menu bar, and the title bar should be above the screen.
//
// Test 2:
//
//    emacs -Q
//
//    Result: Menu bar visible, frame placed immediately below the menu.
//

static void
ns_constrain_all_frames (void)
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_NS_P (f))
        {
          NSView *view = FRAME_NS_VIEW (f);
          /* This no-op will trigger the default window placing
           * constraint system. */
          f->output_data.ns->dont_constrain = 0;
          [[view window] setFrameOrigin:[[view window] frame].origin];
        }
    }
}


/* True, if the menu bar should be hidden.  */

static BOOL
ns_menu_bar_should_be_hidden (void)
{
  return !NILP (ns_auto_hide_menu_bar)
    && [NSApp respondsToSelector:@selector(setPresentationOptions:)];
}


/* Show or hide the menu bar, based on user setting.  */

static void
ns_update_auto_hide_menu_bar (void)
{
#ifndef MAC_OS_X_VERSION_10_6
#define MAC_OS_X_VERSION_10_6 1060
#endif
#ifdef NS_IMPL_COCOA
#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6
  BLOCK_INPUT;

  NSTRACE (ns_update_auto_hide_menu_bar);

  if (NSApp != nil
      && [NSApp isActive]
      && [NSApp respondsToSelector:@selector(setPresentationOptions:)])
    {
      // Note, "setPresentationOptions" triggers an error unless the
      // application is active.
      BOOL menu_bar_should_be_hidden = ns_menu_bar_should_be_hidden ();

      if (menu_bar_should_be_hidden != ns_menu_bar_is_hidden)
        {
          NSApplicationPresentationOptions options
            = NSApplicationPresentationAutoHideDock;

          if (menu_bar_should_be_hidden)
            options |= NSApplicationPresentationAutoHideMenuBar;

          [NSApp setPresentationOptions: options];

          ns_menu_bar_is_hidden = menu_bar_should_be_hidden;

          if (!ns_menu_bar_is_hidden)
            {
              ns_constrain_all_frames ();
            }
        }
    }

  UNBLOCK_INPUT;
#endif
#endif
}


static void
ns_update_begin (struct frame *f)
/* --------------------------------------------------------------------------
   Prepare for a grouped sequence of drawing calls
   external (RIF) call; whole frame, called before update_window_begin
   -------------------------------------------------------------------------- */
{
  NSView *view = FRAME_NS_VIEW (f);
  NSTRACE (ns_update_begin);

  ns_update_auto_hide_menu_bar ();

  ns_updating_frame = f;
  [view lockFocus];

#ifdef NS_IMPL_GNUSTEP
  uRect = NSMakeRect (0, 0, 0, 0);
#endif
}


static void
ns_update_window_begin (struct window *w)
/* --------------------------------------------------------------------------
   Prepare for a grouped sequence of drawing calls
   external (RIF) call; for one window, called after update_begin
   -------------------------------------------------------------------------- */
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
 Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
  NSTRACE (ns_update_window_begin);

  updated_window = w;
  set_output_cursor (&w->cursor);

  BLOCK_INPUT;

  if (f == hlinfo->mouse_face_mouse_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      hlinfo->mouse_face_defer = 1;

        /* If the frame needs to be redrawn,
           simply forget about any prior mouse highlighting.  */
      if (FRAME_GARBAGED_P (f))
        hlinfo->mouse_face_window = Qnil;

      /* (further code for mouse faces ifdef'd out in other terms elided) */
    }

  UNBLOCK_INPUT;
}


static void
ns_update_window_end (struct window *w, int cursor_on_p,
                      int mouse_face_overwritten_p)
/* --------------------------------------------------------------------------
   Finished a grouped sequence of drawing calls
   external (RIF) call; for one window called before update_end
   -------------------------------------------------------------------------- */
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (XFRAME (w->frame));

  /* note: this fn is nearly identical in all terms */
  if (!w->pseudo_window_p)
    {
      BLOCK_INPUT;

      if (cursor_on_p)
	display_and_set_cursor (w, 1,
                                output_cursor.hpos, output_cursor.vpos,
				output_cursor.x, output_cursor.y);

      if (draw_window_fringes (w, 1))
	x_draw_vertical_border (w);

      UNBLOCK_INPUT;
    }

  /* If a row with mouse-face was overwritten, arrange for
     frame_up_to_date to redisplay the mouse highlight.  */
  if (mouse_face_overwritten_p)
    {
      hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
      hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
      hlinfo->mouse_face_window = Qnil;
    }

  updated_window = NULL;
  NSTRACE (update_window_end);
}


static void
ns_update_end (struct frame *f)
/* --------------------------------------------------------------------------
   Finished a grouped sequence of drawing calls
   external (RIF) call; for whole frame, called after update_window_end
   -------------------------------------------------------------------------- */
{
  NSView *view = FRAME_NS_VIEW (f);

/*   if (f == MOUSE_HL_INFO (f)->mouse_face_mouse_frame) */
    MOUSE_HL_INFO (f)->mouse_face_defer = 0;

  BLOCK_INPUT;

#ifdef NS_IMPL_GNUSTEP
  /* trigger flush only in the rectangle we tracked as being drawn */
  [view unlockFocusNeedsFlush: NO];
/*fprintf (stderr, " (%.0f, %.0f : %.0f x %.0f)", uRect.origin.x, uRect.origin.y, uRect.size.width, uRect.size.height); */
  [view lockFocusInRect: uRect];
#endif

  [view unlockFocus];
  [[view window] flushWindow];

  UNBLOCK_INPUT;
  ns_updating_frame = NULL;
  NSTRACE (ns_update_end);
}


static void
ns_flush (struct frame *f)
/* --------------------------------------------------------------------------
   external (RIF) call
   NS impl is no-op since currently we flush in ns_update_end and elsewhere
   -------------------------------------------------------------------------- */
{
    NSTRACE (ns_flush);
}


static void
ns_focus (struct frame *f, NSRect *r, int n)
/* --------------------------------------------------------------------------
   Internal: Focus on given frame.  During small local updates this is used to
     draw, however during large updates, ns_update_begin and ns_update_end are
     called to wrap the whole thing, in which case these calls are stubbed out.
     Except, on GNUstep, we accumulate the rectangle being drawn into, because
     the back end won't do this automatically, and will just end up flushing
     the entire window.
   -------------------------------------------------------------------------- */
{
//  NSTRACE (ns_focus);
#ifdef NS_IMPL_GNUSTEP
  NSRect u;
    if (n == 2)
      u = NSUnionRect (r[0], r[1]);
    else if (r)
      u = *r;
#endif
/* static int c =0;
   fprintf (stderr, "focus: %d", c++);
   if (r) fprintf (stderr, " (%.0f, %.0f : %.0f x %.0f)", r->origin.x, r->origin.y, r->size.width, r->size.height);
   fprintf (stderr, "\n"); */

  if (f != ns_updating_frame)
    {
      NSView *view = FRAME_NS_VIEW (f);
      if (view != focus_view)
        {
          if (focus_view != NULL)
            {
              [focus_view unlockFocus];
              [[focus_view window] flushWindow];
/*debug_lock--; */
            }

          if (view)
#ifdef NS_IMPL_GNUSTEP
            r ? [view lockFocusInRect: u] : [view lockFocus];
#else
            [view lockFocus];
#endif
          focus_view = view;
/*if (view) debug_lock++; */
        }
#ifdef NS_IMPL_GNUSTEP
      else
        {
          /* more than one rect being drawn into */
          if (view && r)
            {
              [view unlockFocus]; /* add prev rect to redraw list */
              [view lockFocusInRect: u]; /* focus for draw in new rect */
            }
        }
#endif
    }
#ifdef NS_IMPL_GNUSTEP
  else
    {
      /* in batch mode, but in GNUstep must still track rectangles explicitly */
      uRect = (r ? NSUnionRect (uRect, u) : [FRAME_NS_VIEW (f) visibleRect]);
    }
#endif

  /* clipping */
  if (r)
    {
      [[NSGraphicsContext currentContext] saveGraphicsState];
      if (n == 2)
        NSRectClipList (r, 2);
      else
        NSRectClip (*r);
      gsaved = YES;
    }
}


static void
ns_unfocus (struct frame *f)
/* --------------------------------------------------------------------------
     Internal: Remove focus on given frame
   -------------------------------------------------------------------------- */
{
//  NSTRACE (ns_unfocus);

  if (gsaved)
    {
      [[NSGraphicsContext currentContext] restoreGraphicsState];
      gsaved = NO;
    }

  if (f != ns_updating_frame)
    {
      if (focus_view != NULL)
        {
          [focus_view unlockFocus];
          [[focus_view window] flushWindow];
          focus_view = NULL;
/*debug_lock--; */
        }
    }
}


static void
ns_clip_to_row (struct window *w, struct glyph_row *row, int area, BOOL gc)
/* --------------------------------------------------------------------------
     Internal (but parallels other terms): Focus drawing on given row
   -------------------------------------------------------------------------- */
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  NSRect clip_rect;
  int window_x, window_y, window_width;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  clip_rect.origin.x = window_x - FRAME_INTERNAL_BORDER_WIDTH (f);
  clip_rect.origin.y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, row->y));
  clip_rect.origin.y = max (clip_rect.origin.y, window_y);
  clip_rect.size.width = window_width + 2 * FRAME_INTERNAL_BORDER_WIDTH (f);
  clip_rect.size.height = row->visible_height;

  /* allow a full-height row at the top when requested
     (used to draw fringe all the way through internal border area) */
  if (gc && clip_rect.origin.y < 5)
    {
      clip_rect.origin.y -= FRAME_INTERNAL_BORDER_WIDTH (f);
      clip_rect.size.height += FRAME_INTERNAL_BORDER_WIDTH (f);
    }

  /* likewise at bottom */
  if (gc &&
      FRAME_PIXEL_HEIGHT (f) - (clip_rect.origin.y + clip_rect.size.height) < 5)
    clip_rect.size.height += FRAME_INTERNAL_BORDER_WIDTH (f);

  ns_focus (f, &clip_rect, 1);
}


static void
ns_ring_bell (struct frame *f)
/* --------------------------------------------------------------------------
     "Beep" routine
   -------------------------------------------------------------------------- */
{
  NSTRACE (ns_ring_bell);
  if (visible_bell)
    {
      NSAutoreleasePool *pool;
      struct frame *frame = SELECTED_FRAME ();
      NSView *view;

      BLOCK_INPUT;
      pool = [[NSAutoreleasePool alloc] init];

      view = FRAME_NS_VIEW (frame);
      if (view != nil)
        {
          NSRect r, surr;
          NSPoint dim = NSMakePoint (128, 128);

          r = [view bounds];
          r.origin.x += (r.size.width - dim.x) / 2;
          r.origin.y += (r.size.height - dim.y) / 2;
          r.size.width = dim.x;
          r.size.height = dim.y;
          surr = NSInsetRect (r, -2, -2);
          ns_focus (frame, &surr, 1);
          [[view window] cacheImageInRect: [view convertRect: surr toView:nil]];
          [ns_lookup_indexed_color (NS_FACE_FOREGROUND
                                      (FRAME_DEFAULT_FACE (frame)), frame) set];
          NSRectFill (r);
          [[view window] flushWindow];
          ns_timeout (150000);
          [[view window] restoreCachedImage];
          [[view window] flushWindow];
          ns_unfocus (frame);
        }
      [pool release];
      UNBLOCK_INPUT;
    }
  else
    {
      NSBeep ();
    }
}


static void
ns_reset_terminal_modes (struct terminal *terminal)
/*  Externally called as hook */
{
  NSTRACE (ns_reset_terminal_modes);
}


static void
ns_set_terminal_modes (struct terminal *terminal)
/*  Externally called as hook */
{
  NSTRACE (ns_set_terminal_modes);
}



/* ==========================================================================

    Frame / window manager related functions

   ========================================================================== */


static void
ns_raise_frame (struct frame *f)
/* --------------------------------------------------------------------------
     Bring window to foreground and make it active
   -------------------------------------------------------------------------- */
{
  NSView *view = FRAME_NS_VIEW (f);
  check_ns ();
  BLOCK_INPUT;
  FRAME_SAMPLE_VISIBILITY (f);
  if (FRAME_VISIBLE_P (f))
    {
      [[view window] makeKeyAndOrderFront: NSApp];
    }
  UNBLOCK_INPUT;
}


static void
ns_lower_frame (struct frame *f)
/* --------------------------------------------------------------------------
     Send window to back
   -------------------------------------------------------------------------- */
{
  NSView *view = FRAME_NS_VIEW (f);
  check_ns ();
  BLOCK_INPUT;
  [[view window] orderBack: NSApp];
  UNBLOCK_INPUT;
}


static void
ns_frame_raise_lower (struct frame *f, int raise)
/* --------------------------------------------------------------------------
     External (hook)
   -------------------------------------------------------------------------- */
{
  NSTRACE (ns_frame_raise_lower);

  if (raise)
    ns_raise_frame (f);
  else
    ns_lower_frame (f);
}


static void
ns_frame_rehighlight (struct frame *frame)
/* --------------------------------------------------------------------------
     External (hook): called on things like window switching within frame
   -------------------------------------------------------------------------- */
{
  struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (frame);
  struct frame *old_highlight = dpyinfo->x_highlight_frame;

  NSTRACE (ns_frame_rehighlight);
  if (dpyinfo->x_focus_frame)
    {
      dpyinfo->x_highlight_frame
	= (FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame))
           ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame))
           : dpyinfo->x_focus_frame);
      if (!FRAME_LIVE_P (dpyinfo->x_highlight_frame))
        {
          FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame) = Qnil;
          dpyinfo->x_highlight_frame = dpyinfo->x_focus_frame;
        }
    }
  else
      dpyinfo->x_highlight_frame = 0;

  if (dpyinfo->x_highlight_frame &&
         dpyinfo->x_highlight_frame != old_highlight)
    {
      if (old_highlight)
	{
          x_update_cursor (old_highlight, 1);
	  x_set_frame_alpha (old_highlight);
	}
      if (dpyinfo->x_highlight_frame)
	{
          x_update_cursor (dpyinfo->x_highlight_frame, 1);
          x_set_frame_alpha (dpyinfo->x_highlight_frame);
	}
    }
}


void
x_make_frame_visible (struct frame *f)
/* --------------------------------------------------------------------------
     External: Show the window (X11 semantics)
   -------------------------------------------------------------------------- */
{
  NSTRACE (x_make_frame_visible);
  /* XXX: at some points in past this was not needed, as the only place that
     called this (frame.c:Fraise_frame ()) also called raise_lower;
     if this ends up the case again, comment this out again. */
  if (!FRAME_VISIBLE_P (f))
    {
      f->async_visible = 1;
      ns_raise_frame (f);
    }
}


void
x_make_frame_invisible (struct frame *f)
/* --------------------------------------------------------------------------
     External: Hide the window (X11 semantics)
   -------------------------------------------------------------------------- */
{
  NSView * view = FRAME_NS_VIEW (f);
  NSTRACE (x_make_frame_invisible);
  check_ns ();
  [[view window] orderOut: NSApp];
  f->async_visible = 0;
  f->async_iconified = 0;
}


void
x_iconify_frame (struct frame *f)
/* --------------------------------------------------------------------------
     External: Iconify window
   -------------------------------------------------------------------------- */
{
  NSView * view = FRAME_NS_VIEW (f);
  struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (f);
  NSTRACE (x_iconify_frame);
  check_ns ();

  if (dpyinfo->x_highlight_frame == f)
    dpyinfo->x_highlight_frame = 0;

  if ([[view window] windowNumber] <= 0)
    {
      /* the window is still deferred.  Make it very small, bring it
         on screen and order it out. */
      NSRect s = { { 100, 100}, {0, 0} };
      NSRect t;
      t = [[view window] frame];
      [[view window] setFrame: s display: NO];
      [[view window] orderBack: NSApp];
      [[view window] orderOut: NSApp];
      [[view window] setFrame: t display: NO];
    }
  [[view window] miniaturize: NSApp];
}

/* Free X resources of frame F.  */

void
x_free_frame_resources (struct frame *f)
{
  NSView *view = FRAME_NS_VIEW (f);
  struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (f);
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
  NSTRACE (x_destroy_window);
  check_ns ();

  [(EmacsView *)view setWindowClosing: YES]; /* may not have been informed */

  BLOCK_INPUT;

  free_frame_menubar (f);

  if (FRAME_FACE_CACHE (f))
    free_frame_faces (f);

  if (f == dpyinfo->x_focus_frame)
    dpyinfo->x_focus_frame = 0;
  if (f == dpyinfo->x_highlight_frame)
    dpyinfo->x_highlight_frame = 0;
  if (f == hlinfo->mouse_face_mouse_frame)
    {
      hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
      hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
      hlinfo->mouse_face_window = Qnil;
      hlinfo->mouse_face_deferred_gc = 0;
      hlinfo->mouse_face_mouse_frame = 0;
    }

  xfree (f->output_data.ns);

  if (f->output_data.ns->miniimage != nil)
    [f->output_data.ns->miniimage release];

  [[view window] close];
  [view release];

  UNBLOCK_INPUT;
}

void
x_destroy_window (struct frame *f)
/* --------------------------------------------------------------------------
     External: Delete the window
   -------------------------------------------------------------------------- */
{
  NSTRACE (x_destroy_window);
  check_ns ();
  x_free_frame_resources (f);
  ns_window_num--;
}


void
x_set_offset (struct frame *f, int xoff, int yoff, int change_grav)
/* --------------------------------------------------------------------------
     External: Position the window
   -------------------------------------------------------------------------- */
{
  NSView *view = FRAME_NS_VIEW (f);
  NSArray *screens = [NSScreen screens];
  NSScreen *fscreen = [screens objectAtIndex: 0];
  NSScreen *screen = [[view window] screen];

  NSTRACE (x_set_offset);

  BLOCK_INPUT;

  f->left_pos = xoff;
  f->top_pos = yoff;

  if (view != nil && screen && fscreen)
    {
      f->left_pos = f->size_hint_flags & XNegative
        ? [screen visibleFrame].size.width + f->left_pos - FRAME_PIXEL_WIDTH (f)
        : f->left_pos;
      /* We use visibleFrame here to take menu bar into account.
	 Ideally we should also adjust left/top with visibleFrame.origin.  */

      f->top_pos = f->size_hint_flags & YNegative
        ? ([screen visibleFrame].size.height + f->top_pos
           - FRAME_PIXEL_HEIGHT (f) - FRAME_NS_TITLEBAR_HEIGHT (f)
           - FRAME_TOOLBAR_HEIGHT (f))
        : f->top_pos;
#ifdef NS_IMPL_GNUSTEP
      if (f->left_pos < 100)
        f->left_pos = 100;  /* don't overlap menu */
#endif
      /* Constrain the setFrameTopLeftPoint so we don't move behind the
         menu bar.  */
      f->output_data.ns->dont_constrain = 0;
      [[view window] setFrameTopLeftPoint:
                       NSMakePoint (SCREENMAXBOUND (f->left_pos),
                                    SCREENMAXBOUND ([fscreen frame].size.height
                                                    - NS_TOP_POS (f)))];
      f->size_hint_flags &= ~(XNegative|YNegative);
    }

  UNBLOCK_INPUT;
}


void
x_set_window_size (struct frame *f, int change_grav, int cols, int rows)
/* --------------------------------------------------------------------------
     Adjust window pixel size based on given character grid size
     Impl is a bit more complex than other terms, need to do some
     internal clipping.
   -------------------------------------------------------------------------- */
{
  EmacsView *view = FRAME_NS_VIEW (f);
  EmacsToolbar *toolbar = [view toolbar];
  NSWindow *window = [view window];
  NSRect wr = [window frame];
  int tb = FRAME_EXTERNAL_TOOL_BAR (f);
  int pixelwidth, pixelheight;
  static int oldRows, oldCols, oldFontWidth, oldFontHeight;
  static int oldTB;
  static struct frame *oldF;

  NSTRACE (x_set_window_size);

  if (view == nil ||
      (f == oldF
       && rows == oldRows && cols == oldCols
       && oldFontWidth == FRAME_COLUMN_WIDTH (f)
       && oldFontHeight == FRAME_LINE_HEIGHT (f)
       && oldTB == tb))
    return;

/*fprintf (stderr, "\tsetWindowSize: %d x %d, font size %d x %d\n", cols, rows, FRAME_COLUMN_WIDTH (f), FRAME_LINE_HEIGHT (f)); */

  BLOCK_INPUT;

  check_frame_size (f, &rows, &cols);
  oldF = f;
  oldRows = rows;
  oldCols = cols;
  oldFontWidth = FRAME_COLUMN_WIDTH (f);
  oldFontHeight = FRAME_LINE_HEIGHT (f);
  oldTB = tb;

  f->scroll_bar_actual_width = NS_SCROLL_BAR_WIDTH (f);
  compute_fringe_widths (f, 0);

  pixelwidth =  FRAME_TEXT_COLS_TO_PIXEL_WIDTH   (f, cols);
  pixelheight = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, rows);

  /* If we have a toolbar, take its height into account. */
  if (tb)
    /* NOTE: previously this would generate wrong result if toolbar not
             yet displayed and fixing toolbar_height=32 helped, but
             now (200903) seems no longer needed */
    FRAME_TOOLBAR_HEIGHT (f) =
      NSHeight ([window frameRectForContentRect: NSMakeRect (0, 0, 0, 0)])
        - FRAME_NS_TITLEBAR_HEIGHT (f);
  else
    FRAME_TOOLBAR_HEIGHT (f) = 0;

  wr.size.width = pixelwidth + f->border_width;
  wr.size.height = pixelheight + FRAME_NS_TITLEBAR_HEIGHT (f)
                  + FRAME_TOOLBAR_HEIGHT (f);

  /* Do not try to constrain to this screen.  We may have multiple
     screens, and want Emacs to span those.  Constraining to screen
     prevents that, and that is not nice to the user.  */
 if (f->output_data.ns->zooming)
   f->output_data.ns->zooming = 0;
 else
   wr.origin.y += FRAME_PIXEL_HEIGHT (f) - pixelheight;

  [view setRows: rows andColumns: cols];
  [window setFrame: wr display: YES];

/*fprintf (stderr, "\tx_set_window_size %d, %d\t%d, %d\n", cols, rows, pixelwidth, pixelheight); */

  /* This is a trick to compensate for Emacs' managing the scrollbar area
     as a fixed number of standard character columns.  Instead of leaving
     blank space for the extra, we chopped it off above.  Now for
     left-hand scrollbars, we shift all rendering to the left by the
     difference between the real width and Emacs' imagined one.  For
     right-hand bars, don't worry about it since the extra is never used.
     (Obviously doesn't work for vertically split windows tho..) */
  {
    NSPoint origin = FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f)
      ? NSMakePoint (FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f)
                     - NS_SCROLL_BAR_WIDTH (f), 0)
      : NSMakePoint (0, 0);
    [view setFrame: NSMakeRect (0, 0, pixelwidth, pixelheight)];
    [view setBoundsOrigin: origin];
  }

  change_frame_size (f, rows, cols, 0, 1, 0); /* pretend, delay, safe */
  FRAME_PIXEL_WIDTH (f) = pixelwidth;
  FRAME_PIXEL_HEIGHT (f) = pixelheight;
/*  SET_FRAME_GARBAGED (f); // this short-circuits expose call in drawRect */

  mark_window_cursors_off (XWINDOW (f->root_window));
  cancel_mouse_face (f);

  UNBLOCK_INPUT;
}



/* ==========================================================================

    Color management

   ========================================================================== */


NSColor *
ns_lookup_indexed_color (unsigned long idx, struct frame *f)
{
  struct ns_color_table *color_table = FRAME_NS_DISPLAY_INFO (f)->color_table;
  if (idx < 1 || idx >= color_table->avail)
    return nil;
  return color_table->colors[idx];
}


unsigned long
ns_index_color (NSColor *color, struct frame *f)
{
  struct ns_color_table *color_table = FRAME_NS_DISPLAY_INFO (f)->color_table;
  ptrdiff_t idx;
  ptrdiff_t i;

  if (!color_table->colors)
    {
      color_table->size = NS_COLOR_CAPACITY;
      color_table->avail = 1; /* skip idx=0 as marker */
      color_table->colors
	= (NSColor **)xmalloc (color_table->size * sizeof (NSColor *));
      color_table->colors[0] = nil;
      color_table->empty_indices = [[NSMutableSet alloc] init];
    }

  /* do we already have this color ? */
  for (i = 1; i < color_table->avail; i++)
    if (color_table->colors[i] && [color_table->colors[i] isEqual: color])
      return i;

  if ([color_table->empty_indices count] > 0)
    {
      NSNumber *index = [color_table->empty_indices anyObject];
      [color_table->empty_indices removeObject: index];
      idx = [index unsignedLongValue];
    }
  else
    {
      if (color_table->avail == color_table->size)
	color_table->colors =
	  xpalloc (color_table->colors, &color_table->size, 1,
		   min (ULONG_MAX, PTRDIFF_MAX), sizeof *color_table->colors);
      idx = color_table->avail++;
    }

  color_table->colors[idx] = color;
  [color retain];
/*fprintf(stderr, "color_table: allocated %d\n",idx);*/
  return idx;
}


void
ns_free_indexed_color (unsigned long idx, struct frame *f)
{
  struct ns_color_table *color_table;
  NSColor *color;
  NSNumber *index;

  if (!f)
    return;

  color_table = FRAME_NS_DISPLAY_INFO (f)->color_table;

  if (idx <= 0 || idx >= color_table->size) {
    message1 ("ns_free_indexed_color: Color index out of range.\n");
    return;
  }

  index = [NSNumber numberWithUnsignedInt: idx];
  if ([color_table->empty_indices containsObject: index]) {
    message1 ("ns_free_indexed_color: attempt to free already freed color.\n");
    return;
  }

  color = color_table->colors[idx];
  [color release];
  color_table->colors[idx] = nil;
  [color_table->empty_indices addObject: index];
/*fprintf(stderr, "color_table: FREED %d\n",idx);*/
}


static int
ns_get_color (const char *name, NSColor **col)
/* --------------------------------------------------------------------------
     Parse a color name
   -------------------------------------------------------------------------- */
/* On *Step, we attempt to mimic the X11 platform here, down to installing an
   X11 rgb.txt-compatible color list in Emacs.clr (see ns_term_init()).
   See: http://thread.gmane.org/gmane.emacs.devel/113050/focus=113272). */
{
  NSColor *new = nil;
  static char hex[20];
  int scaling;
  float r = -1.0, g, b;
  NSString *nsname = [NSString stringWithUTF8String: name];

/*fprintf (stderr, "ns_get_color: '%s'\n", name); */
  BLOCK_INPUT;

  if ([nsname isEqualToString: @"ns_selection_color"])
    {
      nsname = ns_selection_color;
      name = [ns_selection_color UTF8String];
    }

  /* First, check for some sort of numeric specification. */
  hex[0] = '\0';

  if (name[0] == '0' || name[0] == '1' || name[0] == '.')  /* RGB decimal */
    {
      NSScanner *scanner = [NSScanner scannerWithString: nsname];
      [scanner scanFloat: &r];
      [scanner scanFloat: &g];
      [scanner scanFloat: &b];
    }
  else if (!strncmp(name, "rgb:", 4))  /* A newer X11 format -- rgb:r/g/b */
    {
      strncpy (hex, name + 4, 19);
      hex[19] = '\0';
      scaling = (strlen(hex) - 2) / 3;
    }
  else if (name[0] == '#')        /* An old X11 format; convert to newer */
    {
      int len = (strlen(name) - 1);
      int start = (len % 3 == 0) ? 1 : len / 4 + 1;
      int i;
      scaling = strlen(name+start) / 3;
      for (i=0; i<3; i++) {
        strncpy(hex + i * (scaling + 1), name + start + i * scaling, scaling);
        hex[(i+1) * (scaling + 1) - 1] = '/';
      }
      hex[3 * (scaling + 1) - 1] = '\0';
    }

  if (hex[0])
    {
      int rr, gg, bb;
      float fscale = scaling == 4 ? 65535.0 : (scaling == 2 ? 255.0 : 15.0);
      if (sscanf (hex, "%x/%x/%x", &rr, &gg, &bb))
        {
          r = rr / fscale;
          g = gg / fscale;
          b = bb / fscale;
        }
    }

  if (r >= 0.0)
    {
      *col = [NSColor colorWithCalibratedRed: r green: g blue: b alpha: 1.0];
      UNBLOCK_INPUT;
      return 0;
    }

  /* Otherwise, color is expected to be from a list */
  {
    NSEnumerator *lenum, *cenum;
    NSString *name;
    NSColorList *clist;

#ifdef NS_IMPL_GNUSTEP
    /* XXX: who is wrong, the requestor or the implementation? */
    if ([nsname compare: @"Highlight" options: NSCaseInsensitiveSearch]
        == NSOrderedSame)
      nsname = @"highlightColor";
#endif

    lenum = [[NSColorList availableColorLists] objectEnumerator];
    while ( (clist = [lenum nextObject]) && new == nil)
      {
        cenum = [[clist allKeys] objectEnumerator];
        while ( (name = [cenum nextObject]) && new == nil )
          {
            if ([name compare: nsname
                      options: NSCaseInsensitiveSearch] == NSOrderedSame )
              new = [clist colorWithKey: name];
          }
      }
  }

  if (new)
    *col = [new colorUsingColorSpaceName: NSCalibratedRGBColorSpace];
  UNBLOCK_INPUT;
  return new ? 0 : 1;
}


static NSColor *
ns_get_color_default (const char *name, NSColor *dflt)
/* --------------------------------------------------------------------------
     Parse a color or use a default value
   -------------------------------------------------------------------------- */
{
  NSColor * col;

  if (ns_get_color (name, &col))
    return dflt;
  else
    return col;
}


int
ns_lisp_to_color (Lisp_Object color, NSColor **col)
/* --------------------------------------------------------------------------
     Convert a Lisp string object to a NS color
   -------------------------------------------------------------------------- */
{
  NSTRACE (ns_lisp_to_color);
  if (STRINGP (color))
    return ns_get_color (SDATA (color), col);
  else if (SYMBOLP (color))
    return ns_get_color (SDATA (SYMBOL_NAME (color)), col);
  return 1;
}


Lisp_Object
ns_color_to_lisp (NSColor *col)
/* --------------------------------------------------------------------------
     Convert a color to a lisp string with the RGB equivalent
   -------------------------------------------------------------------------- */
{
  CGFloat red, green, blue, alpha, gray;
  char buf[1024];
  const char *str;
  NSTRACE (ns_color_to_lisp);

  BLOCK_INPUT;
  if ([[col colorSpaceName] isEqualToString: NSNamedColorSpace])

      if ((str =[[col colorNameComponent] UTF8String]))
        {
          UNBLOCK_INPUT;
          return build_string ((char *)str);
        }

    [[col colorUsingColorSpaceName: NSCalibratedRGBColorSpace]
        getRed: &red green: &green blue: &blue alpha: &alpha];
  if (red ==green && red ==blue)
    {
      [[col colorUsingColorSpaceName: NSCalibratedWhiteColorSpace]
            getWhite: &gray alpha: &alpha];
      snprintf (buf, sizeof (buf), "#%2.2lx%2.2lx%2.2lx",
		lrint (gray * 0xff), lrint (gray * 0xff), lrint (gray * 0xff));
      UNBLOCK_INPUT;
      return build_string (buf);
    }

  snprintf (buf, sizeof (buf), "#%2.2lx%2.2lx%2.2lx",
            lrint (red*0xff), lrint (green*0xff), lrint (blue*0xff));

  UNBLOCK_INPUT;
  return build_string (buf);
}


void
ns_query_color(void *col, XColor *color_def, int setPixel)
/* --------------------------------------------------------------------------
         Get ARGB values out of NSColor col and put them into color_def.
         If setPixel, set the pixel to a concatenated version.
         and set color_def pixel to the resulting index.
   -------------------------------------------------------------------------- */
{
  CGFloat r, g, b, a;

  [((NSColor *)col) getRed: &r green: &g blue: &b alpha: &a];
  color_def->red   = r * 65535;
  color_def->green = g * 65535;
  color_def->blue  = b * 65535;

  if (setPixel == YES)
    color_def->pixel
      = ARGB_TO_ULONG((int)(a*255),
		      (int)(r*255), (int)(g*255), (int)(b*255));
}


int
ns_defined_color (struct frame *f,
                  const char *name,
                  XColor *color_def,
                  int alloc,
                  char makeIndex)
/* --------------------------------------------------------------------------
         Return 1 if named color found, and set color_def rgb accordingly.
         If makeIndex and alloc are nonzero put the color in the color_table,
         and set color_def pixel to the resulting index.
         If makeIndex is zero, set color_def pixel to ARGB.
         Return 0 if not found
   -------------------------------------------------------------------------- */
{
  NSColor *col;
  NSTRACE (ns_defined_color);

  BLOCK_INPUT;
  if (ns_get_color (name, &col) != 0) /* Color not found  */
    {
      UNBLOCK_INPUT;
      return 0;
    }
  if (makeIndex && alloc)
    color_def->pixel = ns_index_color (col, f);
  ns_query_color (col, color_def, !makeIndex);
  UNBLOCK_INPUT;
  return 1;
}


unsigned long
ns_get_rgb_color (struct frame *f, float r, float g, float b, float a)
/* --------------------------------------------------------------------------
    return an autoreleased RGB color
   -------------------------------------------------------------------------- */
{
/*static int c = 1; fprintf (stderr, "color request %d\n", c++); */
  if (r < 0.0) r = 0.0;
  else if (r > 1.0) r = 1.0;
  if (g < 0.0) g = 0.0;
  else if (g > 1.0) g = 1.0;
  if (b < 0.0) b = 0.0;
  else if (b > 1.0) b = 1.0;
  if (a < 0.0) a = 0.0;
  else if (a > 1.0) a = 1.0;
  return (unsigned long) ns_index_color(
    [NSColor colorWithCalibratedRed: r green: g blue: b alpha: a], f);
}


void
x_set_frame_alpha (struct frame *f)
/* --------------------------------------------------------------------------
     change the entire-frame transparency
   -------------------------------------------------------------------------- */
{
  struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (f);
  EmacsView *view = FRAME_NS_VIEW (f);
  double alpha = 1.0;
  double alpha_min = 1.0;

  if (dpyinfo->x_highlight_frame == f)
    alpha = f->alpha[0];
  else
    alpha = f->alpha[1];

  if (FLOATP (Vframe_alpha_lower_limit))
    alpha_min = XFLOAT_DATA (Vframe_alpha_lower_limit);
  else if (INTEGERP (Vframe_alpha_lower_limit))
    alpha_min = (XINT (Vframe_alpha_lower_limit)) / 100.0;

  if (alpha < 0.0)
    return;
  else if (1.0 < alpha)
    alpha = 1.0;
  else if (0.0 <= alpha && alpha < alpha_min && alpha_min <= 1.0)
    alpha = alpha_min;

#ifdef NS_IMPL_COCOA
  [[view window] setAlphaValue: alpha];
#endif
}


/* ==========================================================================

    Mouse handling

   ========================================================================== */


void
x_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y)
/* --------------------------------------------------------------------------
     Programmatically reposition mouse pointer in pixel coordinates
   -------------------------------------------------------------------------- */
{
  NSTRACE (x_set_mouse_pixel_position);
  ns_raise_frame (f);
#if 0
  /* FIXME: this does not work, and what about GNUstep? */
#ifdef NS_IMPL_COCOA
  [FRAME_NS_VIEW (f) lockFocus];
  PSsetmouse ((float)pix_x, (float)pix_y);
  [FRAME_NS_VIEW (f) unlockFocus];
#endif
#endif
}


void
x_set_mouse_position (struct frame *f, int h, int v)
/* --------------------------------------------------------------------------
     Programmatically reposition mouse pointer in character coordinates
   -------------------------------------------------------------------------- */
{
  int pix_x, pix_y;

  pix_x = FRAME_COL_TO_PIXEL_X (f, h) + FRAME_COLUMN_WIDTH (f) / 2;
  pix_y = FRAME_LINE_TO_PIXEL_Y (f, v) + FRAME_LINE_HEIGHT (f) / 2;

  if (pix_x < 0) pix_x = 0;
  if (pix_x > FRAME_PIXEL_WIDTH (f)) pix_x = FRAME_PIXEL_WIDTH (f);

  if (pix_y < 0) pix_y = 0;
  if (pix_y > FRAME_PIXEL_HEIGHT (f)) pix_y = FRAME_PIXEL_HEIGHT (f);

  x_set_mouse_pixel_position (f, pix_x, pix_y);
}


static int
note_mouse_movement (struct frame *frame, float x, float y)
/*   ------------------------------------------------------------------------
     Called by EmacsView on mouseMovement events.  Passes on
     to emacs mainstream code if we moved off of a rect of interest
     known as last_mouse_glyph.
     ------------------------------------------------------------------------ */
{
//  NSTRACE (note_mouse_movement);

  XSETFRAME (last_mouse_motion_frame, frame);

  /* Note, this doesn't get called for enter/leave, since we don't have a
     position.  Those are taken care of in the corresponding NSView methods. */

  /* has movement gone beyond last rect we were tracking? */
  if (x < last_mouse_glyph.origin.x ||
      x >= (last_mouse_glyph.origin.x + last_mouse_glyph.size.width) ||
      y < last_mouse_glyph.origin.y ||
      y >= (last_mouse_glyph.origin.y + last_mouse_glyph.size.height))
    {
      ns_update_begin(frame);
      frame->mouse_moved = 1;
      note_mouse_highlight (frame, x, y);
      remember_mouse_glyph (frame, x, y, &last_mouse_glyph);
      ns_update_end(frame);
      return 1;
    }

  return 0;
}


static void
ns_mouse_position (struct frame **fp, int insist, Lisp_Object *bar_window,
                   enum scroll_bar_part *part, Lisp_Object *x, Lisp_Object *y,
                   Time *time)
/* --------------------------------------------------------------------------
    External (hook): inform emacs about mouse position and hit parts.
    If a scrollbar is being dragged, set bar_window, part, x, y, time.
    x & y should be position in the scrollbar (the whole bar, not the handle)
    and length of scrollbar respectively
   -------------------------------------------------------------------------- */
{
  id view;
  NSPoint position;
  int xchar, ychar;
  Lisp_Object frame, tail;
  struct frame *f;
  struct ns_display_info *dpyinfo;

  NSTRACE (ns_mouse_position);

  if (*fp == NULL)
    {
      fprintf (stderr, "Warning: ns_mouse_position () called with null *fp.\n");
      return;
    }

  dpyinfo = FRAME_NS_DISPLAY_INFO (*fp);

  BLOCK_INPUT;

  if (last_mouse_scroll_bar != nil && insist == 0)
    {
      /* TODO: we do not use this path at the moment because drag events will
           go directly to the EmacsScroller.  Leaving code in for now. */
      [last_mouse_scroll_bar getMouseMotionPart: (int *)part window: bar_window
                                              x: x y: y];
      if (time) *time = last_mouse_movement_time;
      last_mouse_scroll_bar = nil;
    }
  else
    {
      /* Clear the mouse-moved flag for every frame on this display.  */
      FOR_EACH_FRAME (tail, frame)
        if (FRAME_NS_P (XFRAME (frame))
            && FRAME_NS_DISPLAY (XFRAME (frame)) == FRAME_NS_DISPLAY (*fp))
          XFRAME (frame)->mouse_moved = 0;

      last_mouse_scroll_bar = nil;
      if (last_mouse_frame && FRAME_LIVE_P (last_mouse_frame))
        f = last_mouse_frame;
      else
        f = dpyinfo->x_focus_frame ? dpyinfo->x_focus_frame
                                    : SELECTED_FRAME ();

      if (f && f->output_data.ns)  /* TODO: 2nd check no longer needed? */
        {
          view = FRAME_NS_VIEW (*fp);

          position = [[view window] mouseLocationOutsideOfEventStream];
          position = [view convertPoint: position fromView: nil];
          remember_mouse_glyph (f, position.x, position.y, &last_mouse_glyph);
/*fprintf (stderr, "ns_mouse_position: %.0f, %.0f\n", position.x, position.y); */

          if (bar_window) *bar_window = Qnil;
          if (part) *part = 0; /*scroll_bar_handle; */

          if (x) XSETINT (*x, lrint (position.x));
          if (y) XSETINT (*y, lrint (position.y));
          if (time) *time = last_mouse_movement_time;
          *fp = f;
        }
    }

  UNBLOCK_INPUT;
}


static void
ns_frame_up_to_date (struct frame *f)
/* --------------------------------------------------------------------------
    External (hook): Fix up mouse highlighting right after a full update.
    Some highlighting was deferred if GC was happening during
    note_mouse_highlight (), while other highlighting was deferred for update.
   -------------------------------------------------------------------------- */
{
  NSTRACE (ns_frame_up_to_date);

  if (FRAME_NS_P (f))
    {
      Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
      if ((hlinfo->mouse_face_deferred_gc || f ==hlinfo->mouse_face_mouse_frame)
      /*&& hlinfo->mouse_face_mouse_frame*/)
        {
          BLOCK_INPUT;
	  ns_update_begin(f);
          if (hlinfo->mouse_face_mouse_frame)
            note_mouse_highlight (hlinfo->mouse_face_mouse_frame,
                                  hlinfo->mouse_face_mouse_x,
                                  hlinfo->mouse_face_mouse_y);
          hlinfo->mouse_face_deferred_gc = 0;
	  ns_update_end(f);
          UNBLOCK_INPUT;
        }
    }
}


void
ns_define_frame_cursor (struct frame *f, Cursor cursor)
/* --------------------------------------------------------------------------
    External (RIF): set frame mouse pointer type.
   -------------------------------------------------------------------------- */
{
  NSTRACE (ns_define_frame_cursor);
  if (FRAME_POINTER_TYPE (f) != cursor)
    {
      EmacsView *view = FRAME_NS_VIEW (f);
      FRAME_POINTER_TYPE (f) = cursor;
      [[view window] invalidateCursorRectsForView: view];
      /* Redisplay assumes this function also draws the changed frame
         cursor, but this function doesn't, so do it explicitly.  */
      x_update_cursor (f, 1);
    }
}



/* ==========================================================================

    Keyboard handling

   ========================================================================== */


static unsigned
ns_convert_key (unsigned code)
/* --------------------------------------------------------------------------
    Internal call used by NSView-keyDown.
   -------------------------------------------------------------------------- */
{
  const unsigned last_keysym = (sizeof (convert_ns_to_X_keysym)
                                / sizeof (convert_ns_to_X_keysym[0]));
  unsigned keysym;
  /* An array would be faster, but less easy to read. */
  for (keysym = 0; keysym < last_keysym; keysym += 2)
    if (code == convert_ns_to_X_keysym[keysym])
      return 0xFF00 | convert_ns_to_X_keysym[keysym+1];
  return 0;
/* if decide to use keyCode and Carbon table, use this line:
     return code > 0xff ? 0 : 0xFF00 | ns_keycode_to_xkeysym_table[code]; */
}


char *
x_get_keysym_name (int keysym)
/* --------------------------------------------------------------------------
    Called by keyboard.c.  Not sure if the return val is important, except
    that it be unique.
   -------------------------------------------------------------------------- */
{
  static char value[16];
  NSTRACE (x_get_keysym_name);
  sprintf (value, "%d", keysym);
  return value;
}



/* ==========================================================================

    Block drawing operations

   ========================================================================== */


static void
ns_redraw_scroll_bars (struct frame *f)
{
  int i;
  id view;
  NSArray *subviews = [[FRAME_NS_VIEW (f) superview] subviews];
  NSTRACE (ns_judge_scroll_bars);
  for (i =[subviews count]-1; i >= 0; i--)
    {
      view = [subviews objectAtIndex: i];
      if (![view isKindOfClass: [EmacsScroller class]]) continue;
      [view display];
    }
}


void
ns_clear_frame (struct frame *f)
/* --------------------------------------------------------------------------
      External (hook): Erase the entire frame
   -------------------------------------------------------------------------- */
{
  NSView *view = FRAME_NS_VIEW (f);
  NSRect r;

  NSTRACE (ns_clear_frame);
  if (ns_in_resize)
    return;

 /* comes on initial frame because we have
    after-make-frame-functions = select-frame */
 if (!FRAME_DEFAULT_FACE (f))
   return;

  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));

  output_cursor.hpos = output_cursor.vpos = 0;
  output_cursor.x = -1;

  r = [view bounds];

  BLOCK_INPUT;
  ns_focus (f, &r, 1);
  [ns_lookup_indexed_color (NS_FACE_BACKGROUND (FRAME_DEFAULT_FACE (f)), f) set];
  NSRectFill (r);
  ns_unfocus (f);

#ifdef NS_IMPL_COCOA
  [[view window] display];  /* redraw resize handle */
#endif

  /* as of 2006/11 or so this is now needed */
  ns_redraw_scroll_bars (f);
  UNBLOCK_INPUT;
}


void
ns_clear_frame_area (struct frame *f, int x, int y, int width, int height)
/* --------------------------------------------------------------------------
    External (RIF):  Clear section of frame
   -------------------------------------------------------------------------- */
{
  NSRect r = NSMakeRect (x, y, width, height);
  NSView *view = FRAME_NS_VIEW (f);
  struct face *face = FRAME_DEFAULT_FACE (f);

  if (!view || !face)
    return;

  NSTRACE (ns_clear_frame_area);

  r = NSIntersectionRect (r, [view frame]);
  ns_focus (f, &r, 1);
  [ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), f) set];

#ifdef NS_IMPL_COCOA
  {
    /* clip out the resize handle */
    NSWindow *window = [FRAME_NS_VIEW (f) window];
    NSRect ir
      = [view convertRect: ns_resize_handle_rect (window) fromView: nil];

    ir = NSIntersectionRect (r, ir);
    if (NSIsEmptyRect (ir))
      {
#endif

  NSRectFill (r);

#ifdef NS_IMPL_COCOA
      }
    else
      {
        NSRect r1 = r, r2 = r; /* upper and lower non-intersecting */
        r1.size.height -= ir.size.height;
        r2.origin.y += r1.size.height;
        r2.size.width -= ir.size.width;
        r2.size.height = ir.size.height;
        NSRectFill (r1);
        NSRectFill (r2);
      }
  }
#endif

  ns_unfocus (f);
  return;
}


static void
ns_scroll_run (struct window *w, struct run *run)
/* --------------------------------------------------------------------------
    External (RIF):  Insert or delete n lines at line vpos
   -------------------------------------------------------------------------- */
{
  struct frame *f = XFRAME (w->frame);
  int x, y, width, height, from_y, to_y, bottom_y;

  NSTRACE (ns_scroll_run);

  /* begin copy from other terms */
  /* Get frame-relative bounding box of the text display area of W,
     without mode lines.  Include in this box the left and right
     fringe of W.  */
  window_box (w, -1, &x, &y, &width, &height);

  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->current_y);
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->desired_y);
  bottom_y = y + height;

  if (to_y < from_y)
    {
      /* Scrolling up.  Make sure we don't copy part of the mode
	 line at the bottom.  */
      if (from_y + run->height > bottom_y)
	height = bottom_y - from_y;
      else
	height = run->height;
    }
  else
    {
      /* Scrolling down.  Make sure we don't copy over the mode line.
	 at the bottom.  */
      if (to_y + run->height > bottom_y)
	height = bottom_y - to_y;
      else
	height = run->height;
    }
  /* end copy from other terms */

  if (height == 0)
      return;

  BLOCK_INPUT;

  updated_window = w;
  x_clear_cursor (w);

  {
    NSRect srcRect = NSMakeRect (x, from_y, width, height);
    NSRect dstRect = NSMakeRect (x, to_y, width, height);
    NSPoint dstOrigin = NSMakePoint (x, to_y);

    ns_focus (f, &dstRect, 1);
    NSCopyBits (0, srcRect , dstOrigin);
    ns_unfocus (f);
  }

  UNBLOCK_INPUT;
}


static void
ns_after_update_window_line (struct glyph_row *desired_row)
/* --------------------------------------------------------------------------
    External (RIF): preparatory to fringe update after text was updated
   -------------------------------------------------------------------------- */
{
  struct window *w = updated_window;
  struct frame *f;
  int width, height;

  NSTRACE (ns_after_update_window_line);

  /* begin copy from other terms */
  xassert (w);

  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    desired_row->redraw_fringe_bitmaps_p = 1;

  /* When a window has disappeared, make sure that no rest of
     full-width rows stays visible in the internal border.
     Under NS this is drawn inside the fringes. */
  if (windows_or_buffers_changed
      && (f = XFRAME (w->frame),
	  width = FRAME_INTERNAL_BORDER_WIDTH (f),
	  width != 0)
      && (height = desired_row->visible_height,
	  height > 0))
    {
      int y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, desired_row->y));

      /* Internal border is drawn below the tool bar.  */
      if (WINDOWP (f->tool_bar_window)
	  && w == XWINDOW (f->tool_bar_window))
	y -= width;
      /* end copy from other terms */

      BLOCK_INPUT;
      if (!desired_row->full_width_p)
        {
          int x1 = WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH (w)
            + WINDOW_LEFT_FRINGE_WIDTH (w);
          int x2 = WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH (w)
            + FRAME_PIXEL_WIDTH (f) - NS_SCROLL_BAR_WIDTH (f)
            - WINDOW_RIGHT_FRINGE_WIDTH (w)
            - FRAME_INTERNAL_BORDER_WIDTH (f);
          ns_clear_frame_area (f, x1, y, width, height);
          ns_clear_frame_area (f, x2, y, width, height);
        }
      UNBLOCK_INPUT;
    }
}


static void
ns_shift_glyphs_for_insert (struct frame *f,
                           int x, int y, int width, int height,
                           int shift_by)
/* --------------------------------------------------------------------------
    External (RIF): copy an area horizontally, don't worry about clearing src
   -------------------------------------------------------------------------- */
{
  NSRect srcRect = NSMakeRect (x, y, width, height);
  NSRect dstRect = NSMakeRect (x+shift_by, y, width, height);
  NSPoint dstOrigin = dstRect.origin;

  NSTRACE (ns_shift_glyphs_for_insert);

  ns_focus (f, &dstRect, 1);
  NSCopyBits (0, srcRect, dstOrigin);
  ns_unfocus (f);
}



/* ==========================================================================

    Character encoding and metrics

   ========================================================================== */


static inline void
ns_compute_glyph_string_overhangs (struct glyph_string *s)
/* --------------------------------------------------------------------------
     External (RIF); compute left/right overhang of whole string and set in s
   -------------------------------------------------------------------------- */
{
  struct face *face = FACE_FROM_ID (s->f, s->first_glyph->face_id);
  struct font *font = s->font; /*face->font; */

  if (s->char2b)
    {
      struct font_metrics metrics;
      unsigned int codes[2];
      codes[0] = *(s->char2b);
      codes[1] = *(s->char2b + s->nchars - 1);

      font->driver->text_extents (font, codes, 2, &metrics);
      s->left_overhang = -metrics.lbearing;
      s->right_overhang
	= metrics.rbearing > metrics.width
	? metrics.rbearing - metrics.width : 0;
    }
  else
    {
      s->left_overhang = 0;
      s->right_overhang = ((struct nsfont_info *)font)->ital ?
        FONT_HEIGHT (font) * 0.2 : 0;
    }
}



/* ==========================================================================

    Fringe and cursor drawing

   ========================================================================== */


extern int max_used_fringe_bitmap;
static void
ns_draw_fringe_bitmap (struct window *w, struct glyph_row *row,
                      struct draw_fringe_bitmap_params *p)
/* --------------------------------------------------------------------------
    External (RIF); fringe-related
   -------------------------------------------------------------------------- */
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face = p->face;
  int rowY;
  static EmacsImage **bimgs = NULL;
  static int nBimgs = 0;
  /* NS-specific: move internal border inside fringe */
  int x = p->bx < 0 ? p->x : p->bx;
  int wd = p->bx < 0 ? p->wd : p->nx;
  BOOL fringeOnVeryLeft
    = x - WINDOW_LEFT_SCROLL_BAR_COLS (w) * WINDOW_FRAME_COLUMN_WIDTH (w)
      - FRAME_INTERNAL_BORDER_WIDTH (f) < 10;
  BOOL fringeOnVeryRight
    = FRAME_PIXEL_WIDTH (f) - x - wd - FRAME_INTERNAL_BORDER_WIDTH (f)
      - WINDOW_RIGHT_SCROLL_BAR_COLS (w) * WINDOW_FRAME_COLUMN_WIDTH (w) < 10;
  int xAdjust = FRAME_INTERNAL_BORDER_WIDTH (f) *
    (fringeOnVeryLeft ? -1 : (fringeOnVeryRight ? 1 : 0));

  /* grow bimgs if needed */
  if (nBimgs < max_used_fringe_bitmap)
    {
      EmacsImage **newBimgs
	= xmalloc (max_used_fringe_bitmap * sizeof (EmacsImage *));
      memset (newBimgs, 0, max_used_fringe_bitmap * sizeof (EmacsImage *));

      if (nBimgs)
        {
          memcpy (newBimgs, bimgs, nBimgs * sizeof (EmacsImage *));
          xfree (bimgs);
        }

      bimgs = newBimgs;
      nBimgs = max_used_fringe_bitmap;
    }

  /* Must clip because of partially visible lines.  */
  rowY = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);
  ns_clip_to_row (w, row, -1, YES);

  if (p->bx >= 0 && !p->overlay_p)
    {
      int yAdjust = rowY - FRAME_INTERNAL_BORDER_WIDTH (f) < 5 ?
        -FRAME_INTERNAL_BORDER_WIDTH (f) : 0;
      int yIncr = FRAME_PIXEL_HEIGHT (f) - (p->by+yAdjust + p->ny) < 5 ?
        FRAME_INTERNAL_BORDER_WIDTH (f) : 0
        + (yAdjust ? FRAME_INTERNAL_BORDER_WIDTH (f) : 0);
      NSRect r = NSMakeRect (p->bx+xAdjust, p->by+yAdjust, p->nx, p->ny+yIncr);
      NSRectClip (r);
      [ns_lookup_indexed_color(face->background, f) set];
      NSRectFill (r);
    }

  if (p->which)
    {
      NSRect r = NSMakeRect (p->x+xAdjust, p->y, p->wd, p->h);
      NSPoint pt = r.origin;
      EmacsImage *img = bimgs[p->which - 1];

      if (!img)
        {
          unsigned short *bits = p->bits + p->dh;
          int len = p->h;
          int i;
          unsigned char *cbits = xmalloc (len);

          for (i =0; i<len; i++)
            cbits[i] = ~(bits[i] & 0xff);
          img = [[EmacsImage alloc] initFromXBM: cbits width: 8 height: p->h
                                           flip: NO];
          bimgs[p->which - 1] = img;
          xfree (cbits);
        }

      NSRectClip (r);
      /* Since we composite the bitmap instead of just blitting it, we need
         to erase the whole background. */
      [ns_lookup_indexed_color(face->background, f) set];
      NSRectFill (r);
      pt.y += p->h;
      [img setXBMColor: ns_lookup_indexed_color(face->foreground, f)];
      [img compositeToPoint: pt operation: NSCompositeSourceOver];
    }
  ns_unfocus (f);
}


void
ns_draw_window_cursor (struct window *w, struct glyph_row *glyph_row,
                       int x, int y, int cursor_type, int cursor_width,
                       int on_p, int active_p)
/* --------------------------------------------------------------------------
     External call (RIF): draw cursor.
     Note that CURSOR_WIDTH is meaningful only for (h)bar cursors.
   -------------------------------------------------------------------------- */
{
  NSRect r, s;
  int fx, fy, h, cursor_height;
  struct frame *f = WINDOW_XFRAME (w);
  struct glyph *phys_cursor_glyph;
  int overspill;
  struct glyph *cursor_glyph;
  struct face *face;
  NSColor *hollow_color = FRAME_BACKGROUND_COLOR (f);

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */

  NSTRACE (dumpcursor);

  if (!on_p)
    return;

  w->phys_cursor_type = cursor_type;
  w->phys_cursor_on_p = on_p;

  if (cursor_type == NO_CURSOR)
    {
      w->phys_cursor_width = 0;
      return;
    }

  if ((phys_cursor_glyph = get_phys_cursor_glyph (w)) == NULL)
    {
      if (glyph_row->exact_window_width_line_p
          && w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])
        {
          glyph_row->cursor_in_fringe_p = 1;
          draw_fringe_bitmap (w, glyph_row, 0);
        }
      return;
    }

  /* We draw the cursor (with NSRectFill), then draw the glyph on top
     (other terminals do it the other way round).  We must set
     w->phys_cursor_width to the cursor width.  For bar cursors, that
     is CURSOR_WIDTH; for box cursors, it is the glyph width.  */
  get_phys_cursor_geometry (w, glyph_row, phys_cursor_glyph, &fx, &fy, &h);

  /* The above get_phys_cursor_geometry call set w->phys_cursor_width
     to the glyph width; replace with CURSOR_WIDTH for (V)BAR cursors. */
  if (cursor_type == BAR_CURSOR)
    {
      if (cursor_width < 1)
	cursor_width = max (FRAME_CURSOR_WIDTH (f), 1);
      w->phys_cursor_width = cursor_width;
    }
  /* If we have an HBAR, "cursor_width" MAY specify height. */
  else if (cursor_type == HBAR_CURSOR)
    {
      cursor_height = (cursor_width < 1) ? lrint (0.25 * h) : cursor_width;
      fy += h - cursor_height;
      h = cursor_height;
    }

  r.origin.x = fx, r.origin.y = fy;
  r.size.height = h;
  r.size.width = w->phys_cursor_width;

  /* FIXME: if we overwrite the internal border area, it does not get erased;
     fix by truncating cursor, but better would be to erase properly */
  overspill = r.origin.x + r.size.width -
    WINDOW_TEXT_TO_FRAME_PIXEL_X (w, WINDOW_BOX_RIGHT_EDGE_X (w)
      - WINDOW_TOTAL_FRINGE_WIDTH (w) - FRAME_INTERNAL_BORDER_WIDTH (f));
  if (overspill > 0)
    r.size.width -= overspill;

  /* TODO: only needed in rare cases with last-resort font in HELLO..
     should we do this more efficiently? */
  ns_clip_to_row (w, glyph_row, -1, NO); /* do ns_focus(f, &r, 1); if remove */


  face = FACE_FROM_ID (f, phys_cursor_glyph->face_id);
  if (face && NS_FACE_BACKGROUND (face)
      == ns_index_color (FRAME_CURSOR_COLOR (f), f))
    {
      [ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), f) set];
      hollow_color = FRAME_CURSOR_COLOR (f);
    }
  else
    [FRAME_CURSOR_COLOR (f) set];

#ifdef NS_IMPL_COCOA
  /* TODO: This makes drawing of cursor plus that of phys_cursor_glyph
           atomic.  Cleaner ways of doing this should be investigated.
           One way would be to set a global variable DRAWING_CURSOR
  	   when making the call to draw_phys..(), don't focus in that
  	   case, then move the ns_unfocus() here after that call. */
  NSDisableScreenUpdates ();
#endif

  switch (cursor_type)
    {
    case NO_CURSOR:
      break;
    case FILLED_BOX_CURSOR:
      NSRectFill (r);
      break;
    case HOLLOW_BOX_CURSOR:
      NSRectFill (r);
      [hollow_color set];
      NSRectFill (NSInsetRect (r, 1, 1));
      [FRAME_CURSOR_COLOR (f) set];
      break;
    case HBAR_CURSOR:
      NSRectFill (r);
      break;
    case BAR_CURSOR:
      s = r;
      /* If the character under cursor is R2L, draw the bar cursor
         on the right of its glyph, rather than on the left.  */
      cursor_glyph = get_phys_cursor_glyph (w);
      if ((cursor_glyph->resolved_level & 1) != 0)
        s.origin.x += cursor_glyph->pixel_width - s.size.width;

      NSRectFill (s);
      break;
    }
  ns_unfocus (f);

  /* draw the character under the cursor */
  if (cursor_type != NO_CURSOR)
    draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);

#ifdef NS_IMPL_COCOA
  NSEnableScreenUpdates ();
#endif

}


static void
ns_draw_vertical_window_border (struct window *w, int x, int y0, int y1)
/* --------------------------------------------------------------------------
     External (RIF): Draw a vertical line.
   -------------------------------------------------------------------------- */
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face;
  NSRect r = NSMakeRect (x, y0, 1, y1-y0);

  NSTRACE (ns_draw_vertical_window_border);

  face = FACE_FROM_ID (f, VERTICAL_BORDER_FACE_ID);
  if (face)
      [ns_lookup_indexed_color(face->foreground, f) set];

  ns_focus (f, &r, 1);
  NSRectFill(r);
  ns_unfocus (f);
}


void
show_hourglass (struct atimer *timer)
{
  if (hourglass_shown_p)
    return;

  BLOCK_INPUT;

  /* TODO: add NSProgressIndicator to selected frame (see macfns.c) */

  hourglass_shown_p = 1;
  UNBLOCK_INPUT;
}


void
hide_hourglass (void)
{
  if (!hourglass_shown_p)
    return;

  BLOCK_INPUT;

  /* TODO: remove NSProgressIndicator from all frames */

  hourglass_shown_p = 0;
  UNBLOCK_INPUT;
}



/* ==========================================================================

    Glyph drawing operations

   ========================================================================== */


static inline NSRect
ns_fix_rect_ibw (NSRect r, int fibw, int frame_pixel_width)
/* --------------------------------------------------------------------------
    Under NS we draw internal borders inside fringes, and want full-width
    rendering to go all the way to edge.  This function makes that correction.
   -------------------------------------------------------------------------- */
{
  if (r.origin.y <= fibw+1)
    {
      r.size.height += r.origin.y;
      r.origin.y = 0;
    }
  if (r.origin.x <= fibw+1)
    {
      r.size.width += r.origin.x;
      r.origin.x = 0;
    }
  if (frame_pixel_width - (r.origin.x+r.size.width) <= fibw+1)
    r.size.width += fibw;

  return r;
}


static int
ns_get_glyph_string_clip_rect (struct glyph_string *s, NativeRectangle *nr)
/* --------------------------------------------------------------------------
    Wrapper utility to account for internal border width on full-width lines,
    and allow top full-width rows to hit the frame top.  nr should be pointer
    to two successive NSRects.  Number of rects actually used is returned.
   -------------------------------------------------------------------------- */
{
  int n = get_glyph_string_clip_rects (s, nr, 2);
  if (s->row->full_width_p)
    {
      *nr = ns_fix_rect_ibw (*nr, FRAME_INTERNAL_BORDER_WIDTH (s->f),
                            FRAME_PIXEL_WIDTH (s->f));
      if (n == 2)
        *nr = ns_fix_rect_ibw (*(nr+1), FRAME_INTERNAL_BORDER_WIDTH (s->f),
                              FRAME_PIXEL_WIDTH (s->f));
    }
  return n;
}

void
ns_draw_text_decoration (struct glyph_string *s, struct face *face,
                         NSColor *defaultCol, CGFloat width, CGFloat x)
/* --------------------------------------------------------------------------
   Draw underline, overline, and strike-through on glyph string s.
   -------------------------------------------------------------------------- */
{
  if (s->for_overlaps)
    return;

  /* Do underline. */
  if (face->underline_p)
    {
      NSRect r;
      unsigned long thickness, position;

      /* If the prev was underlined, match its appearance. */
      if (s->prev && s->prev->face->underline_p
          && s->prev->underline_thickness > 0)
        {
          thickness = s->prev->underline_thickness;
          position = s->prev->underline_position;
        }
      else
        {
          struct font *font;
          unsigned long descent;

          font=s->font;
          descent = s->y + s->height - s->ybase;

          /* Use underline thickness of font, defaulting to 1. */
          thickness = (font && font->underline_thickness > 0)
            ? font->underline_thickness : 1;

          /* Determine the offset of underlining from the baseline. */
          if (x_underline_at_descent_line)
            position = descent - thickness;
          else if (x_use_underline_position_properties
                   && font && font->underline_position >= 0)
            position = font->underline_position;
          else if (font)
            position = lround (font->descent / 2);
          else
            position = underline_minimum_offset;

          position = max (position, underline_minimum_offset);

          /* Ensure underlining is not cropped. */
          if (descent <= position)
            {
              position = descent - 1;
              thickness = 1;
            }
          else if (descent < position + thickness)
            thickness = 1;
        }

      s->underline_thickness = thickness;
      s->underline_position = position;

      r = NSMakeRect (x, s->ybase + position, width, thickness);

      if (face->underline_defaulted_p)
        [defaultCol set];
      else
        [ns_lookup_indexed_color (face->underline_color, s->f) set];
      NSRectFill (r);
    }

  /* Do overline. We follow other terms in using a thickness of 1
     and ignoring overline_margin. */
  if (face->overline_p)
    {
      NSRect r;
      r = NSMakeRect (x, s->y, width, 1);

      if (face->overline_color_defaulted_p)
        [defaultCol set];
      else
        [ns_lookup_indexed_color (face->overline_color, s->f) set];
      NSRectFill (r);
    }

  /* Do strike-through.  We follow other terms for thickness and
     vertical position.*/
  if (face->strike_through_p)
    {
      NSRect r;
      unsigned long dy;

      dy = lrint ((s->height - 1) / 2);
      r = NSMakeRect (x, s->y + dy, width, 1);

      if (face->strike_through_color_defaulted_p)
        [defaultCol set];
      else
        [ns_lookup_indexed_color (face->strike_through_color, s->f) set];
      NSRectFill (r);
    }
}

static void
ns_draw_box (NSRect r, float thickness, NSColor *col, char left_p, char right_p)
/* --------------------------------------------------------------------------
    Draw an unfilled rect inside r, optionally leaving left and/or right open.
    Note we can't just use an NSDrawRect command, because of the possibility
    of some sides not being drawn, and because the rect will be filled.
   -------------------------------------------------------------------------- */
{
  NSRect s = r;
  [col set];

  /* top, bottom */
  s.size.height = thickness;
  NSRectFill (s);
  s.origin.y += r.size.height - thickness;
  NSRectFill (s);

  s.size.height = r.size.height;
  s.origin.y = r.origin.y;

  /* left, right (optional) */
  s.size.width = thickness;
  if (left_p)
    NSRectFill (s);
  if (right_p)
    {
      s.origin.x += r.size.width - thickness;
      NSRectFill (s);
    }
}


static void
ns_draw_relief (NSRect r, int thickness, char raised_p,
               char top_p, char bottom_p, char left_p, char right_p,
               struct glyph_string *s)
/* --------------------------------------------------------------------------
    Draw a relief rect inside r, optionally leaving some sides open.
    Note we can't just use an NSDrawBezel command, because of the possibility
    of some sides not being drawn, and because the rect will be filled.
   -------------------------------------------------------------------------- */
{
  static NSColor *baseCol = nil, *lightCol = nil, *darkCol = nil;
  NSColor *newBaseCol = nil;
  NSRect sr = r;

  NSTRACE (ns_draw_relief);

  /* set up colors */

  if (s->face->use_box_color_for_shadows_p)
    {
      newBaseCol = ns_lookup_indexed_color (s->face->box_color, s->f);
    }
/*     else if (s->first_glyph->type == IMAGE_GLYPH
	   && s->img->pixmap
   	   && !IMAGE_BACKGROUND_TRANSPARENT (s->img, s->f, 0))
       {
         newBaseCol = IMAGE_BACKGROUND  (s->img, s->f, 0);
       } */
  else
    {
      newBaseCol = ns_lookup_indexed_color (s->face->background, s->f);
    }

  if (newBaseCol == nil)
    newBaseCol = [NSColor grayColor];

  if (newBaseCol != baseCol)  /* TODO: better check */
    {
      [baseCol release];
      baseCol = [newBaseCol retain];
      [lightCol release];
      lightCol = [[baseCol highlightWithLevel: 0.2] retain];
      [darkCol release];
      darkCol = [[baseCol shadowWithLevel: 0.3] retain];
    }

  [(raised_p ? lightCol : darkCol) set];

  /* TODO: mitering. Using NSBezierPath doesn't work because of color switch. */

  /* top */
  sr.size.height = thickness;
  if (top_p) NSRectFill (sr);

  /* left */
  sr.size.height = r.size.height;
  sr.size.width = thickness;
  if (left_p) NSRectFill (sr);

  [(raised_p ? darkCol : lightCol) set];

  /* bottom */
  sr.size.width = r.size.width;
  sr.size.height = thickness;
  sr.origin.y += r.size.height - thickness;
  if (bottom_p) NSRectFill (sr);

  /* right */
  sr.size.height = r.size.height;
  sr.origin.y = r.origin.y;
  sr.size.width = thickness;
  sr.origin.x += r.size.width - thickness;
  if (right_p) NSRectFill (sr);
}


static void
ns_dumpglyphs_box_or_relief (struct glyph_string *s)
/* --------------------------------------------------------------------------
      Function modeled after x_draw_glyph_string_box ().
      Sets up parameters for drawing.
   -------------------------------------------------------------------------- */
{
  int right_x, last_x;
  char left_p, right_p;
  struct glyph *last_glyph;
  NSRect r;
  int thickness;
  struct face *face;

  if (s->hl == DRAW_MOUSE_FACE)
    {
      face = FACE_FROM_ID (s->f, MOUSE_HL_INFO (s->f)->mouse_face_face_id);
      if (!face)
        face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
    }
  else
    face = s->face;

  thickness = face->box_line_width;

  NSTRACE (ns_dumpglyphs_box_or_relief);

  last_x = ((s->row->full_width_p && !s->w->pseudo_window_p)
	    ? WINDOW_RIGHT_EDGE_X (s->w)
	    : window_box_right (s->w, s->area));
  last_glyph = (s->cmp || s->img
                ? s->first_glyph : s->first_glyph + s->nchars-1);

  right_x = ((s->row->full_width_p && s->extends_to_end_of_line_p
	      ? last_x - 1 : min (last_x, s->x + s->background_width) - 1));

  left_p = (s->first_glyph->left_box_line_p
	    || (s->hl == DRAW_MOUSE_FACE
		&& (s->prev == NULL || s->prev->hl != s->hl)));
  right_p = (last_glyph->right_box_line_p
	     || (s->hl == DRAW_MOUSE_FACE
		 && (s->next == NULL || s->next->hl != s->hl)));

  r = NSMakeRect (s->x, s->y, right_x - s->x + 1, s->height);

  /* expand full-width row over internal borders */
  if (s->row->full_width_p)
    r = ns_fix_rect_ibw (r, FRAME_INTERNAL_BORDER_WIDTH (s->f),
                        FRAME_PIXEL_WIDTH (s->f));

  /* TODO: Sometimes box_color is 0 and this seems wrong; should investigate. */
  if (s->face->box == FACE_SIMPLE_BOX && s->face->box_color)
    {
      ns_draw_box (r, abs (thickness),
                   ns_lookup_indexed_color (face->box_color, s->f),
                  left_p, right_p);
    }
  else
    {
      ns_draw_relief (r, abs (thickness), s->face->box == FACE_RAISED_BOX,
                     1, 1, left_p, right_p, s);
    }
}


static void
ns_maybe_dumpglyphs_background (struct glyph_string *s, char force_p)
/* --------------------------------------------------------------------------
      Modeled after x_draw_glyph_string_background, which draws BG in
      certain cases.  Others are left to the text rendering routine.
   -------------------------------------------------------------------------- */
{
  NSTRACE (ns_maybe_dumpglyphs_background);

  if (!s->background_filled_p/* || s->hl == DRAW_MOUSE_FACE*/)
    {
      int box_line_width = max (s->face->box_line_width, 0);
      if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
          || s->font_not_found_p || s->extends_to_end_of_line_p || force_p)
	{
          struct face *face;
          if (s->hl == DRAW_MOUSE_FACE)
            {
              face = FACE_FROM_ID (s->f,
				   MOUSE_HL_INFO (s->f)->mouse_face_face_id);
              if (!face)
                face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
            }
          else
            face = FACE_FROM_ID (s->f, s->first_glyph->face_id);
          if (!face->stipple)
            [(NS_FACE_BACKGROUND (face) != 0
              ? ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f)
              : FRAME_BACKGROUND_COLOR (s->f)) set];
          else
            {
              struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (s->f);
              [[dpyinfo->bitmaps[face->stipple-1].img stippleMask] set];
            }

          if (s->hl != DRAW_CURSOR)
            {
              NSRect r = NSMakeRect (s->x, s->y + box_line_width,
                                    s->background_width,
                                    s->height-2*box_line_width);

              /* expand full-width row over internal borders */
              if (s->row->full_width_p)
                {
                  int fibw = FRAME_INTERNAL_BORDER_WIDTH (s->f);
                  if (r.origin.y <= fibw+1 + box_line_width)
                    {
                      r.size.height += r.origin.y;
                      r.origin.y = 0;
                    }
                  if (r.origin.x <= fibw+1)
                    {
                      r.size.width += 2*r.origin.x;
                      r.origin.x = 0;
                    }
                  if (FRAME_PIXEL_WIDTH (s->f) - (r.origin.x + r.size.width)
                      <= fibw+1)
                    r.size.width += fibw;
                }

              NSRectFill (r);
            }

	  s->background_filled_p = 1;
	}
    }
}


static void
ns_dumpglyphs_image (struct glyph_string *s, NSRect r)
/* --------------------------------------------------------------------------
      Renders an image and associated borders.
   -------------------------------------------------------------------------- */
{
  EmacsImage *img = s->img->pixmap;
  int box_line_vwidth = max (s->face->box_line_width, 0);
  int x = s->x, y = s->ybase - image_ascent (s->img, s->face, &s->slice);
  int bg_x, bg_y, bg_height;
  int th;
  char raised_p;
  NSRect br;
  struct face *face;
  NSColor *tdCol;

  NSTRACE (ns_dumpglyphs_image);

  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p && s->slice.x == 0)
    x += abs (s->face->box_line_width);

  bg_x = x;
  bg_y =  s->slice.y == 0 ? s->y : s->y + box_line_vwidth;
  bg_height = s->height;
  /* other terms have this, but was causing problems w/tabbar mode */
  /* - 2 * box_line_vwidth; */

  if (s->slice.x == 0) x += s->img->hmargin;
  if (s->slice.y == 0) y += s->img->vmargin;

  /* Draw BG: if we need larger area than image itself cleared, do that,
     otherwise, since we composite the image under NS (instead of mucking
     with its background color), we must clear just the image area. */
  if (s->hl == DRAW_MOUSE_FACE)
    {
      face = FACE_FROM_ID (s->f, MOUSE_HL_INFO (s->f)->mouse_face_face_id);
      if (!face)
       face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
    }
  else
    face = FACE_FROM_ID (s->f, s->first_glyph->face_id);

  [ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f) set];

  if (bg_height > s->slice.height || s->img->hmargin || s->img->vmargin
      || s->img->mask || s->img->pixmap == 0 || s->width != s->background_width)
    {
      br = NSMakeRect (bg_x, bg_y, s->background_width, bg_height);
      s->background_filled_p = 1;
    }
  else
    {
      br = NSMakeRect (x, y, s->slice.width, s->slice.height);
    }

  /* expand full-width row over internal borders */
  if (s->row->full_width_p)
    {
      int fibw = FRAME_INTERNAL_BORDER_WIDTH (s->f);
      if (br.origin.y <= fibw+1 + box_line_vwidth)
        {
          br.size.height += br.origin.y;
          br.origin.y = 0;
        }
      if (br.origin.x <= fibw+1 + box_line_vwidth)
        {
          br.size.width += br.origin.x;
          br.origin.x = 0;
        }
      if (FRAME_PIXEL_WIDTH (s->f) - (br.origin.x + br.size.width) <= fibw+1)
        br.size.width += fibw;
    }

  NSRectFill (br);

  /* Draw the image.. do we need to draw placeholder if img ==nil? */
  if (img != nil)
    [img compositeToPoint: NSMakePoint (x, y + s->slice.height)
                operation: NSCompositeSourceOver];

  if (s->hl == DRAW_CURSOR)
    {
    [FRAME_CURSOR_COLOR (s->f) set];
    if (s->w->phys_cursor_type == FILLED_BOX_CURSOR)
      tdCol = ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f);
    else
      /* Currently on NS img->mask is always 0. Since
         get_window_cursor_type specifies a hollow box cursor when on
         a non-masked image we never reach this clause. But we put it
         in in anticipation of better support for image masks on
         NS. */
      tdCol = ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), s->f);
    }
  else
    {
      tdCol = ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), s->f);
    }

  /* Draw underline, overline, strike-through. */
  ns_draw_text_decoration (s, face, tdCol, br.size.width, br.origin.x);

  /* Draw relief, if requested */
  if (s->img->relief || s->hl ==DRAW_IMAGE_RAISED || s->hl ==DRAW_IMAGE_SUNKEN)
    {
      if (s->hl == DRAW_IMAGE_SUNKEN || s->hl == DRAW_IMAGE_RAISED)
        {
          th = tool_bar_button_relief >= 0 ?
            tool_bar_button_relief : DEFAULT_TOOL_BAR_BUTTON_RELIEF;
          raised_p = (s->hl == DRAW_IMAGE_RAISED);
        }
      else
        {
          th = abs (s->img->relief);
          raised_p = (s->img->relief > 0);
        }

      r.origin.x = x - th;
      r.origin.y = y - th;
      r.size.width = s->slice.width + 2*th-1;
      r.size.height = s->slice.height + 2*th-1;
      ns_draw_relief (r, th, raised_p,
                      s->slice.y == 0,
                      s->slice.y + s->slice.height == s->img->height,
                      s->slice.x == 0,
                      s->slice.x + s->slice.width == s->img->width, s);
    }

  /* If there is no mask, the background won't be seen,
     so draw a rectangle on the image for the cursor.
     Do this for all images, getting transparency right is not reliable.  */
  if (s->hl == DRAW_CURSOR)
    {
      int thickness = abs (s->img->relief);
      if (thickness == 0) thickness = 1;
      ns_draw_box (br, thickness, FRAME_CURSOR_COLOR (s->f), 1, 1);
    }
}


static void
ns_dumpglyphs_stretch (struct glyph_string *s)
{
  NSRect r[2];
  int n, i;
  struct face *face;
  NSColor *fgCol, *bgCol;

  if (!s->background_filled_p)
    {
      n = ns_get_glyph_string_clip_rect (s, r);
      *r = NSMakeRect (s->x, s->y, s->background_width, s->height);

      ns_focus (s->f, r, n);

      if (s->hl == DRAW_MOUSE_FACE)
       {
         face = FACE_FROM_ID (s->f, MOUSE_HL_INFO (s->f)->mouse_face_face_id);
         if (!face)
           face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);
       }
      else
       face = FACE_FROM_ID (s->f, s->first_glyph->face_id);

      bgCol = ns_lookup_indexed_color (NS_FACE_BACKGROUND (face), s->f);
      fgCol = ns_lookup_indexed_color (NS_FACE_FOREGROUND (face), s->f);

      for (i=0; i<n; i++)
        {
          if (!s->row->full_width_p)
            {
	      int overrun, leftoverrun;

              /* truncate to avoid overwriting fringe and/or scrollbar */
	      overrun = max (0, (s->x + s->background_width)
			     - (WINDOW_BOX_RIGHT_EDGE_X (s->w)
				- WINDOW_RIGHT_FRINGE_WIDTH (s->w)));
              r[i].size.width -= overrun;

	      /* truncate to avoid overwriting to left of the window box */
	      leftoverrun = (WINDOW_BOX_LEFT_EDGE_X (s->w)
			     + WINDOW_LEFT_FRINGE_WIDTH (s->w)) - s->x;

	      if (leftoverrun > 0)
		{
		  r[i].origin.x += leftoverrun;
		  r[i].size.width -= leftoverrun;
		}

              /* XXX: Try to work between problem where a stretch glyph on
                 a partially-visible bottom row will clear part of the
                 modeline, and another where list-buffers headers and similar
                 rows erroneously have visible_height set to 0.  Not sure
                 where this is coming from as other terms seem not to show. */
              r[i].size.height = min (s->height, s->row->visible_height);
            }

          /* expand full-width rows over internal borders */
          else
            {
              r[i] = ns_fix_rect_ibw (r[i], FRAME_INTERNAL_BORDER_WIDTH (s->f),
                                      FRAME_PIXEL_WIDTH (s->f));
            }

          [bgCol set];

          /* NOTE: under NS this is NOT used to draw cursors, but we must avoid
             overwriting cursor (usually when cursor on a tab) */
          if (s->hl == DRAW_CURSOR)
            {
              CGFloat x, width;

              x = r[i].origin.x;
              width = s->w->phys_cursor_width;
              r[i].size.width -= width;
              r[i].origin.x += width;

              NSRectFill (r[i]);

              /* Draw overlining, etc. on the cursor. */
              if (s->w->phys_cursor_type == FILLED_BOX_CURSOR)
                ns_draw_text_decoration (s, face, bgCol, width, x);
              else
                ns_draw_text_decoration (s, face, fgCol, width, x);
            }
          else
            {
              NSRectFill (r[i]);
            }

          /* Draw overlining, etc. on the stretch glyph (or the part
             of the stretch glyph after the cursor). */
          ns_draw_text_decoration (s, face, fgCol, r[i].size.width,
                                   r[i].origin.x);
        }
      ns_unfocus (s->f);
      s->background_filled_p = 1;
    }
}


static void
ns_draw_glyph_string (struct glyph_string *s)
/* --------------------------------------------------------------------------
      External (RIF): Main draw-text call.
   -------------------------------------------------------------------------- */
{
  /* TODO (optimize): focus for box and contents draw */
  NSRect r[2];
  int n;
  char box_drawn_p = 0;

  NSTRACE (ns_draw_glyph_string);

  if (s->next && s->right_overhang && !s->for_overlaps/*&&s->hl!=DRAW_CURSOR*/)
    {
      int width;
      struct glyph_string *next;

      for (width = 0, next = s->next;
	   next && width < s->right_overhang;
	   width += next->width, next = next->next)
	if (next->first_glyph->type != IMAGE_GLYPH)
          {
            if (next->first_glyph->type != STRETCH_GLYPH)
              {
                n = ns_get_glyph_string_clip_rect (s->next, r);
                ns_focus (s->f, r, n);
                ns_maybe_dumpglyphs_background (s->next, 1);
                ns_unfocus (s->f);
              }
            else
              {
                ns_dumpglyphs_stretch (s->next);
              }
            next->num_clips = 0;
          }
    }

  if (!s->for_overlaps && s->face->box != FACE_NO_BOX
        && (s->first_glyph->type == CHAR_GLYPH
	    || s->first_glyph->type == COMPOSITE_GLYPH))
    {
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);
      ns_maybe_dumpglyphs_background (s, 1);
      ns_dumpglyphs_box_or_relief (s);
      ns_unfocus (s->f);
      box_drawn_p = 1;
    }

  switch (s->first_glyph->type)
    {

    case IMAGE_GLYPH:
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);
      ns_dumpglyphs_image (s, r[0]);
      ns_unfocus (s->f);
      break;

    case STRETCH_GLYPH:
      ns_dumpglyphs_stretch (s);
      break;

    case CHAR_GLYPH:
    case COMPOSITE_GLYPH:
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);

      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
        s->background_filled_p = 1;
      else
        ns_maybe_dumpglyphs_background
          (s, s->first_glyph->type == COMPOSITE_GLYPH);

      ns_tmp_flags = s->hl == DRAW_CURSOR ? NS_DUMPGLYPH_CURSOR :
                    (s->hl == DRAW_MOUSE_FACE ? NS_DUMPGLYPH_MOUSEFACE :
                     (s->for_overlaps ? NS_DUMPGLYPH_FOREGROUND :
                      NS_DUMPGLYPH_NORMAL));
      ns_tmp_font = (struct nsfont_info *)s->face->font;
      if (ns_tmp_font == NULL)
          ns_tmp_font = (struct nsfont_info *)FRAME_FONT (s->f);

      if (s->hl == DRAW_CURSOR && s->w->phys_cursor_type == FILLED_BOX_CURSOR)
        {
          unsigned long tmp = NS_FACE_BACKGROUND (s->face);
          NS_FACE_BACKGROUND (s->face) = NS_FACE_FOREGROUND (s->face);
          NS_FACE_FOREGROUND (s->face) = tmp;
        }

      ns_tmp_font->font.driver->draw
        (s, 0, s->nchars, s->x, s->y,
         (ns_tmp_flags == NS_DUMPGLYPH_NORMAL && !s->background_filled_p)
         || ns_tmp_flags == NS_DUMPGLYPH_MOUSEFACE);

      if (s->hl == DRAW_CURSOR && s->w->phys_cursor_type == FILLED_BOX_CURSOR)
        {
          unsigned long tmp = NS_FACE_BACKGROUND (s->face);
          NS_FACE_BACKGROUND (s->face) = NS_FACE_FOREGROUND (s->face);
          NS_FACE_FOREGROUND (s->face) = tmp;
        }

      ns_unfocus (s->f);
      break;

    case GLYPHLESS_GLYPH:
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);

      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
        s->background_filled_p = 1;
      else
        ns_maybe_dumpglyphs_background
          (s, s->first_glyph->type == COMPOSITE_GLYPH);
      /* ... */
      /* Not yet implemented.  */
      /* ... */
      ns_unfocus (s->f);
      break;

    default:
      abort ();
    }

  /* Draw box if not done already. */
  if (!s->for_overlaps && !box_drawn_p && s->face->box != FACE_NO_BOX)
    {
      n = ns_get_glyph_string_clip_rect (s, r);
      ns_focus (s->f, r, n);
      ns_dumpglyphs_box_or_relief (s);
      ns_unfocus (s->f);
    }

  s->num_clips = 0;
}



/* ==========================================================================

    Event loop

   ========================================================================== */


static void
ns_send_appdefined (int value)
/* --------------------------------------------------------------------------
    Internal: post an appdefined event which EmacsApp-sendEvent will
              recognize and take as a command to halt the event loop.
   -------------------------------------------------------------------------- */
{
  /*NSTRACE (ns_send_appdefined); */

  /* Only post this event if we haven't already posted one.  This will end
       the [NXApp run] main loop after having processed all events queued at
       this moment.  */
  if (send_appdefined)
    {
      NSEvent *nxev;

      /* We only need one NX_APPDEFINED event to stop NXApp from running.  */
      send_appdefined = NO;

      /* Don't need wakeup timer any more */
      if (timed_entry)
        {
          [timed_entry invalidate];
          [timed_entry release];
          timed_entry = nil;
        }

      /* Ditto for file descriptor poller */
      if (fd_entry)
        {
          [fd_entry invalidate];
          [fd_entry release];
          fd_entry = nil;
        }

      nxev = [NSEvent otherEventWithType: NSApplicationDefined
                                location: NSMakePoint (0, 0)
                           modifierFlags: 0
                               timestamp: 0
                            windowNumber: [[NSApp mainWindow] windowNumber]
                                 context: [NSApp context]
                                 subtype: 0
                                   data1: value
                                   data2: 0];

      /* Post an application defined event on the event queue.  When this is
         received the [NXApp run] will return, thus having processed all
         events which are currently queued.  */
      [NSApp postEvent: nxev atStart: NO];
    }
}


static int
ns_read_socket (struct terminal *terminal, int expected,
                struct input_event *hold_quit)
/* --------------------------------------------------------------------------
     External (hook): Post an event to ourself and keep reading events until
     we read it back again.  In effect process all events which were waiting.
     From 21+ we have to manage the event buffer ourselves.
   -------------------------------------------------------------------------- */
{
  struct input_event ev;
  int nevents;

/* NSTRACE (ns_read_socket); */

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
#ifdef SYNC_INPUT
      pending_signals = 1;
#endif
      return -1;
    }

  interrupt_input_pending = 0;
#ifdef SYNC_INPUT
  pending_signals = pending_atimers;
#endif

  BLOCK_INPUT;
  n_emacs_events_pending = 0;
  EVENT_INIT (ev);
  emacs_event = &ev;
  q_event_ptr = hold_quit;

  /* we manage autorelease pools by allocate/reallocate each time around
     the loop; strict nesting is occasionally violated but seems not to
     matter.. earlier methods using full nesting caused major memory leaks */
  [outerpool release];
  outerpool = [[NSAutoreleasePool alloc] init];

  /* If have pending open-file requests, attend to the next one of those. */
  if (ns_pending_files && [ns_pending_files count] != 0
      && [(EmacsApp *)NSApp openFile: [ns_pending_files objectAtIndex: 0]])
    {
      [ns_pending_files removeObjectAtIndex: 0];
    }
  /* Deal with pending service requests. */
  else if (ns_pending_service_names && [ns_pending_service_names count] != 0
    && [(EmacsApp *)
         NSApp fulfillService: [ns_pending_service_names objectAtIndex: 0]
                      withArg: [ns_pending_service_args objectAtIndex: 0]])
    {
      [ns_pending_service_names removeObjectAtIndex: 0];
      [ns_pending_service_args removeObjectAtIndex: 0];
    }
  else
    {
      /* Run and wait for events.  We must always send one NX_APPDEFINED event
         to ourself, otherwise [NXApp run] will never exit.  */
      send_appdefined = YES;

      /* If called via ns_select, this is called once with expected=1,
         because we expect either the timeout or file descriptor activity.
         In this case the first event through will either be real input or
         one of these.  read_avail_input() then calls once more with expected=0
         and in that case we need to return quickly if there is nothing.
         If we're being called outside of that, it's also OK to return quickly
         after one iteration through the event loop, since other terms do
         this and emacs expects it. */
      if (!(inNsSelect && expected))
        {
          /* Post an application defined event on the event queue.  When this is
             received the [NXApp run] will return, thus having processed all
             events which are currently queued, if any.  */
          ns_send_appdefined (-1);
        }

      [NSApp run];
    }

  nevents = n_emacs_events_pending;
  n_emacs_events_pending = 0;
  emacs_event = q_event_ptr = NULL;
  UNBLOCK_INPUT;

  return nevents;
}


int
ns_select (int nfds, fd_set *readfds, fd_set *writefds,
           fd_set *exceptfds, struct timeval *timeout)
/* --------------------------------------------------------------------------
     Replacement for select, checking for events
   -------------------------------------------------------------------------- */
{
  int result;
  double time;
  NSEvent *ev;
/*  NSTRACE (ns_select); */

  if (NSApp == nil || inNsSelect == 1 /* || ([NSApp isActive] == NO &&
                      [NSApp nextEventMatchingMask:NSAnyEventMask untilDate:nil
 inMode:NSDefaultRunLoopMode dequeue:NO] == nil) */)
    return select (nfds, readfds, writefds, exceptfds, timeout);

  /* Save file descriptor set, which gets overwritten in calls to select ()
     Note, this is called from process.c, and only readfds is ever set */
  if (readfds)
    {
      memcpy (&select_readfds, readfds, sizeof (fd_set));
      select_nfds = nfds;
    }
  else
    select_nfds = 0;

    /* Try an initial select for pending data on input files */
  select_timeout.tv_sec = select_timeout.tv_usec = 0;
  result = select (nfds, readfds, writefds, exceptfds, &select_timeout);
  if (result)
    return result;

  /* if (!timeout || timed_entry || fd_entry)
       fprintf (stderr, "assertion failed: timeout null or timed_entry/fd_entry non-null in ns_select\n"); */

    /* set a timeout and run the main AppKit event loop while continuing
       to monitor the files */
  time = ((double) timeout->tv_sec) + ((double) timeout->tv_usec)/1000000.0;
  timed_entry = [[NSTimer scheduledTimerWithTimeInterval: time
                                           target: NSApp
                                         selector: @selector (timeout_handler:)
                                         userInfo: 0
                                          repeats: YES] /* for safe removal */
                                                         retain];

  /* set a periodic task to try the select () again */
  fd_entry = [[NSTimer scheduledTimerWithTimeInterval: 0.1
                                               target: NSApp
                                             selector: @selector (fd_handler:)
                                             userInfo: 0
                                              repeats: YES]
               retain];

  /* Let Application dispatch events until it receives an event of the type
     NX_APPDEFINED, which should only be sent by timeout_handler.
     We tell read_avail_input() that input is "expected" because we do expect
     either the timeout or fd handler to fire, and if they don't, the original
     call from process.c that got us here expects us to wait until some input
     comes. */
  inNsSelect = 1;
  gobble_input (1);
  ev = last_appdefined_event;
  inNsSelect = 0;

  if (ev)
    {
      int t;
      if ([ev type] != NSApplicationDefined)
        abort ();

      t = [ev data1];
      last_appdefined_event = 0;

      if (t == -2)
        {
          /* The NX_APPDEFINED event we received was a timeout. */
          return 0;
        }
      else if (t == -1)
        {
          /* The NX_APPDEFINED event we received was the result of
             at least one real input event arriving.  */
          errno = EINTR;
          return -1;
        }
      else
        {
          /* Received back from select () in fd_handler; copy the results */
          if (readfds)
            memcpy (readfds, &select_readfds, sizeof (fd_set));
          return t;
        }
    }
  /* never reached, shut compiler up */
  return 0;
}



/* ==========================================================================

    Scrollbar handling

   ========================================================================== */


static void
ns_set_vertical_scroll_bar (struct window *window,
                           int portion, int whole, int position)
/* --------------------------------------------------------------------------
      External (hook): Update or add scrollbar
   -------------------------------------------------------------------------- */
{
  Lisp_Object win;
  NSRect r, v;
  struct frame *f = XFRAME (WINDOW_FRAME (window));
  EmacsView *view = FRAME_NS_VIEW (f);
  int window_y, window_height;
  BOOL barOnVeryLeft, barOnVeryRight;
  int top, left, height, width, sb_width, sb_left;
  EmacsScroller *bar;
static int count = 0;

  /* optimization; display engine sends WAY too many of these.. */
  if (!NILP (window->vertical_scroll_bar))
    {
      bar = XNS_SCROLL_BAR (window->vertical_scroll_bar);
      if ([bar checkSamePosition: position portion: portion whole: whole])
        {
          if (view->scrollbarsNeedingUpdate == 0)
            {
              if (!windows_or_buffers_changed)
                  return;
            }
          else
            view->scrollbarsNeedingUpdate--;
        }
    }

  NSTRACE (ns_set_vertical_scroll_bar);

  /* Get dimensions.  */
  window_box (window, -1, 0, &window_y, 0, &window_height);
  top = window_y;
  height = window_height;
  width = WINDOW_CONFIG_SCROLL_BAR_COLS (window) * FRAME_COLUMN_WIDTH (f);
  left = WINDOW_SCROLL_BAR_AREA_X (window);

  if (top < 5) /* top scrollbar adjustment */
    {
      top -= FRAME_INTERNAL_BORDER_WIDTH (f);
      height += FRAME_INTERNAL_BORDER_WIDTH (f);
    }

  /* allow for displaying a skinnier scrollbar than char area allotted */
  sb_width = (WINDOW_CONFIG_SCROLL_BAR_WIDTH (window) > 0) ?
    WINDOW_CONFIG_SCROLL_BAR_WIDTH (window) : width;

  barOnVeryLeft = left < 5;
  barOnVeryRight = FRAME_PIXEL_WIDTH (f) - left - width < 5;
  sb_left = left + FRAME_INTERNAL_BORDER_WIDTH (f)
      * (barOnVeryLeft ? -1 : (barOnVeryRight ? 1 : 0));

  r = NSMakeRect (sb_left, top, sb_width, height);
  /* the parent view is flipped, so we need to flip y value */
  v = [view frame];
  r.origin.y = (v.size.height - r.size.height - r.origin.y);

  XSETWINDOW (win, window);
  BLOCK_INPUT;

  /* we want at least 5 lines to display a scrollbar */
  if (WINDOW_TOTAL_LINES (window) < 5)
    {
      if (!NILP (window->vertical_scroll_bar))
        {
          bar = XNS_SCROLL_BAR (window->vertical_scroll_bar);
          [bar removeFromSuperview];
          window->vertical_scroll_bar = Qnil;
        }
      ns_clear_frame_area (f, sb_left, top, width, height);
      UNBLOCK_INPUT;
      return;
    }

  if (NILP (window->vertical_scroll_bar))
    {
      ns_clear_frame_area (f, sb_left, top, width, height);
      bar = [[EmacsScroller alloc] initFrame: r window: win];
      window->vertical_scroll_bar = make_save_value (bar, 0);
    }
  else
    {
      NSRect oldRect;
      bar = XNS_SCROLL_BAR (window->vertical_scroll_bar);
      oldRect = [bar frame];
      r.size.width = oldRect.size.width;
      if (FRAME_LIVE_P (f) && !NSEqualRects (oldRect, r))
        {
          if (oldRect.origin.x != r.origin.x)
              ns_clear_frame_area (f, sb_left, top, width, height);
          [bar setFrame: r];
        }
    }

  [bar setPosition: position portion: portion whole: whole];
  UNBLOCK_INPUT;
}


static void
ns_condemn_scroll_bars (struct frame *f)
/* --------------------------------------------------------------------------
     External (hook): arrange for all frame's scrollbars to be removed
     at next call to judge_scroll_bars, except for those redeemed.
   -------------------------------------------------------------------------- */
{
  int i;
  id view;
  NSArray *subviews = [[FRAME_NS_VIEW (f) superview] subviews];

  NSTRACE (ns_condemn_scroll_bars);

  for (i =[subviews count]-1; i >= 0; i--)
    {
      view = [subviews objectAtIndex: i];
      if ([view isKindOfClass: [EmacsScroller class]])
        [view condemn];
    }
}


static void
ns_redeem_scroll_bar (struct window *window)
/* --------------------------------------------------------------------------
     External (hook): arrange to spare this window's scrollbar
     at next call to judge_scroll_bars.
   -------------------------------------------------------------------------- */
{
  id bar;
  NSTRACE (ns_redeem_scroll_bar);
  if (!NILP (window->vertical_scroll_bar))
    {
      bar =XNS_SCROLL_BAR (window->vertical_scroll_bar);
      [bar reprieve];
    }
}


static void
ns_judge_scroll_bars (struct frame *f)
/* --------------------------------------------------------------------------
     External (hook): destroy all scrollbars on frame that weren't
     redeemed after call to condemn_scroll_bars.
   -------------------------------------------------------------------------- */
{
  int i;
  id view;
  NSArray *subviews = [[FRAME_NS_VIEW (f) superview] subviews];
  NSTRACE (ns_judge_scroll_bars);
  for (i =[subviews count]-1; i >= 0; i--)
    {
      view = [subviews objectAtIndex: i];
      if (![view isKindOfClass: [EmacsScroller class]]) continue;
      [view judge];
    }
}


void
x_wm_set_icon_position (struct frame *f, int icon_x, int icon_y)
{
  /* XXX irrelevant under NS */
}



/* ==========================================================================

    Initialization

   ========================================================================== */

int
x_display_pixel_height (struct ns_display_info *dpyinfo)
{
  NSScreen *screen = [NSScreen mainScreen];
  return [screen frame].size.height;
}

int
x_display_pixel_width (struct ns_display_info *dpyinfo)
{
  NSScreen *screen = [NSScreen mainScreen];
  return [screen frame].size.width;
}


static Lisp_Object ns_string_to_lispmod (const char *s)
/* --------------------------------------------------------------------------
     Convert modifier name to lisp symbol
   -------------------------------------------------------------------------- */
{
  if (!strncmp (SDATA (SYMBOL_NAME (Qmeta)), s, 10))
    return Qmeta;
  else if (!strncmp (SDATA (SYMBOL_NAME (Qsuper)), s, 10))
    return Qsuper;
  else if (!strncmp (SDATA (SYMBOL_NAME (Qcontrol)), s, 10))
    return Qcontrol;
  else if (!strncmp (SDATA (SYMBOL_NAME (Qalt)), s, 10))
    return Qalt;
  else if (!strncmp (SDATA (SYMBOL_NAME (Qhyper)), s, 10))
    return Qhyper;
  else if (!strncmp (SDATA (SYMBOL_NAME (Qnone)), s, 10))
    return Qnone;
  else
    return Qnil;
}


static Lisp_Object ns_mod_to_lisp (int m)
/* --------------------------------------------------------------------------
     Convert modifier code (see lisp.h) to lisp symbol
   -------------------------------------------------------------------------- */
{
  if (m == CHAR_META)
    return Qmeta;
  else if (m == CHAR_SUPER)
    return Qsuper;
  else if (m == CHAR_CTL)
    return Qcontrol;
  else if (m == CHAR_ALT)
    return Qalt;
  else if (m == CHAR_HYPER)
    return Qhyper;
  else /* if (m == 0) */
    return Qnone;
}


static void
ns_default (const char *parameter, Lisp_Object *result,
           Lisp_Object yesval, Lisp_Object noval,
           BOOL is_float, BOOL is_modstring)
/* --------------------------------------------------------------------------
      Check a parameter value in user's preferences
   -------------------------------------------------------------------------- */
{
  const char *value = ns_get_defaults_value (parameter);

  if (value)
    {
      double f;
      char *pos;
      if (strcasecmp (value, "YES") == 0)
        *result = yesval;
      else if (strcasecmp (value, "NO") == 0)
        *result = noval;
      else if (is_float && (f = strtod (value, &pos), pos != value))
        *result = make_float (f);
      else if (is_modstring && value)
        *result = ns_string_to_lispmod (value);
      else fprintf (stderr,
                   "Bad value for default \"%s\": \"%s\"\n", parameter, value);
    }
}


void
ns_initialize_display_info (struct ns_display_info *dpyinfo)
/* --------------------------------------------------------------------------
      Initialize global info and storage for display.
   -------------------------------------------------------------------------- */
{
    NSScreen *screen = [NSScreen mainScreen];
    NSWindowDepth depth = [screen depth];
    Mouse_HLInfo *hlinfo = &dpyinfo->mouse_highlight;

    dpyinfo->resx = 72.27; /* used 75.0, but this makes pt == pixel, expected */
    dpyinfo->resy = 72.27;
    dpyinfo->color_p = ![NSDeviceWhiteColorSpace isEqualToString:
                                                  NSColorSpaceFromDepth (depth)]
                && ![NSCalibratedWhiteColorSpace isEqualToString:
                                                 NSColorSpaceFromDepth (depth)];
    dpyinfo->n_planes = NSBitsPerPixelFromDepth (depth);
    dpyinfo->image_cache = make_image_cache ();
    dpyinfo->color_table
      = (struct ns_color_table *)xmalloc (sizeof (struct ns_color_table));
    dpyinfo->color_table->colors = NULL;
    dpyinfo->root_window = 42; /* a placeholder.. */

    hlinfo->mouse_face_mouse_frame = NULL;
    hlinfo->mouse_face_deferred_gc = 0;
    hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
    hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
    hlinfo->mouse_face_face_id = DEFAULT_FACE_ID;
    hlinfo->mouse_face_window = hlinfo->mouse_face_overlay = Qnil;
    hlinfo->mouse_face_hidden = 0;

    hlinfo->mouse_face_mouse_x = hlinfo->mouse_face_mouse_y = 0;
    hlinfo->mouse_face_defer = 0;

    dpyinfo->x_highlight_frame = dpyinfo->x_focus_frame = NULL;

    dpyinfo->n_fonts = 0;
    dpyinfo->smallest_font_height = 1;
    dpyinfo->smallest_char_width = 1;
}


/* This and next define (many of the) public functions in this file. */
/* x_... are generic versions in xdisp.c that we, and other terms, get away
         with using despite presence in the "system dependent" redisplay
         interface.  In addition, many of the ns_ methods have code that is
         shared with all terms, indicating need for further refactoring. */
extern frame_parm_handler ns_frame_parm_handlers[];
static struct redisplay_interface ns_redisplay_interface =
{
  ns_frame_parm_handlers,
  x_produce_glyphs,
  x_write_glyphs,
  x_insert_glyphs,
  x_clear_end_of_line,
  ns_scroll_run,
  ns_after_update_window_line,
  ns_update_window_begin,
  ns_update_window_end,
  x_cursor_to,
  ns_flush,
  0, /* flush_display_optional */
  x_clear_window_mouse_face,
  x_get_glyph_overhangs,
  x_fix_overlapping_area,
  ns_draw_fringe_bitmap,
  0, /* define_fringe_bitmap */ /* FIXME: simplify ns_draw_fringe_bitmap */
  0, /* destroy_fringe_bitmap */
  ns_compute_glyph_string_overhangs,
  ns_draw_glyph_string, /* interface to nsfont.m */
  ns_define_frame_cursor,
  ns_clear_frame_area,
  ns_draw_window_cursor,
  ns_draw_vertical_window_border,
  ns_shift_glyphs_for_insert
};


static void
ns_delete_display (struct ns_display_info *dpyinfo)
{
  /* TODO... */
}


/* This function is called when the last frame on a display is deleted. */
static void
ns_delete_terminal (struct terminal *terminal)
{
  struct ns_display_info *dpyinfo = terminal->display_info.ns;
  int i;

  /* Protect against recursive calls.  delete_frame in
     delete_terminal calls us back when it deletes our last frame.  */
  if (!terminal->name)
    return;

  BLOCK_INPUT;

  x_destroy_all_bitmaps (dpyinfo);
  ns_delete_display (dpyinfo);
  UNBLOCK_INPUT;
}


static struct terminal *
ns_create_terminal (struct ns_display_info *dpyinfo)
/* --------------------------------------------------------------------------
      Set up use of NS before we make the first connection.
   -------------------------------------------------------------------------- */
{
  struct terminal *terminal;

  NSTRACE (ns_create_terminal);

  terminal = create_terminal ();

  terminal->type = output_ns;
  terminal->display_info.ns = dpyinfo;
  dpyinfo->terminal = terminal;

  terminal->rif = &ns_redisplay_interface;

  terminal->clear_frame_hook = ns_clear_frame;
  terminal->ins_del_lines_hook = 0; /* XXX vestigial? */
  terminal->delete_glyphs_hook = 0; /* XXX vestigial? */
  terminal->ring_bell_hook = ns_ring_bell;
  terminal->reset_terminal_modes_hook = ns_reset_terminal_modes;
  terminal->set_terminal_modes_hook = ns_set_terminal_modes;
  terminal->update_begin_hook = ns_update_begin;
  terminal->update_end_hook = ns_update_end;
  terminal->set_terminal_window_hook = NULL; /* XXX vestigial? */
  terminal->read_socket_hook = ns_read_socket;
  terminal->frame_up_to_date_hook = ns_frame_up_to_date;
  terminal->mouse_position_hook = ns_mouse_position;
  terminal->frame_rehighlight_hook = ns_frame_rehighlight;
  terminal->frame_raise_lower_hook = ns_frame_raise_lower;

  terminal->fullscreen_hook = 0; /* see XTfullscreen_hook */

  terminal->set_vertical_scroll_bar_hook = ns_set_vertical_scroll_bar;
  terminal->condemn_scroll_bars_hook = ns_condemn_scroll_bars;
  terminal->redeem_scroll_bar_hook = ns_redeem_scroll_bar;
  terminal->judge_scroll_bars_hook = ns_judge_scroll_bars;

  terminal->delete_frame_hook = x_destroy_window;
  terminal->delete_terminal_hook = ns_delete_terminal;

  terminal->scroll_region_ok = 1;
  terminal->char_ins_del_ok = 1;
  terminal->line_ins_del_ok = 1;
  terminal->fast_clear_end_of_line = 1;
  terminal->memory_below_frame = 0;

  return terminal;
}


struct ns_display_info *
ns_term_init (Lisp_Object display_name)
/* --------------------------------------------------------------------------
     Start the Application and get things rolling.
   -------------------------------------------------------------------------- */
{
  struct terminal *terminal;
  struct ns_display_info *dpyinfo;
  static int ns_initialized = 0;
  Lisp_Object tmp;

  NSTRACE (ns_term_init);

  /* count object allocs (About, click icon); on OS X use ObjectAlloc tool */
  /*GSDebugAllocationActive (YES); */
  BLOCK_INPUT;
  handling_signal = 0;

  if (!ns_initialized)
    {
      baud_rate = 38400;
      Fset_input_interrupt_mode (Qnil);
      ns_initialized = 1;
    }

  ns_pending_files = [[NSMutableArray alloc] init];
  ns_pending_service_names = [[NSMutableArray alloc] init];
  ns_pending_service_args = [[NSMutableArray alloc] init];

  /* Start app and create the main menu, window, view.
     Needs to be here because ns_initialize_display_info () uses AppKit classes.
     The view will then ask the NSApp to stop and return to Emacs. */
  [EmacsApp sharedApplication];
  if (NSApp == nil)
    return NULL;
  [NSApp setDelegate: NSApp];

  /* debugging: log all notifications */
  /*   [[NSNotificationCenter defaultCenter] addObserver: NSApp
                                         selector: @selector (logNotification:)
                                             name: nil object: nil]; */

  dpyinfo = (struct ns_display_info *)xmalloc (sizeof (struct ns_display_info));
  memset (dpyinfo, 0, sizeof (struct ns_display_info));

  ns_initialize_display_info (dpyinfo);
  terminal = ns_create_terminal (dpyinfo);

  terminal->kboard = (KBOARD *) xmalloc (sizeof (KBOARD));
  init_kboard (terminal->kboard);
  KVAR (terminal->kboard, Vwindow_system) = Qns;
  terminal->kboard->next_kboard = all_kboards;
  all_kboards = terminal->kboard;
  /* Don't let the initial kboard remain current longer than necessary.
     That would cause problems if a file loaded on startup tries to
     prompt in the mini-buffer.  */
  if (current_kboard == initial_kboard)
    current_kboard = terminal->kboard;
  terminal->kboard->reference_count++;

  dpyinfo->next = x_display_list;
  x_display_list = dpyinfo;

  /* Put it on ns_display_name_list */
  ns_display_name_list = Fcons (Fcons (display_name, Qnil),
                                ns_display_name_list);
  dpyinfo->name_list_element = XCAR (ns_display_name_list);

  /* Set the name of the terminal. */
  terminal->name = (char *) xmalloc (SBYTES (display_name) + 1);
  strncpy (terminal->name, SDATA (display_name), SBYTES (display_name));
  terminal->name[SBYTES (display_name)] = 0;

  UNBLOCK_INPUT;

  if (!inhibit_x_resources)
    {
      ns_default ("GSFontAntiAlias", &ns_antialias_text,
                 Qt, Qnil, NO, NO);
      tmp = Qnil;
      /* this is a standard variable */
      ns_default ("AppleAntiAliasingThreshold", &tmp,
                 make_float (10.0), make_float (6.0), YES, NO);
      ns_antialias_threshold = NILP (tmp) ? 10.0 : XFLOATINT (tmp);
    }

  ns_selection_color = [[NSUserDefaults standardUserDefaults]
			 stringForKey: @"AppleHighlightColor"];
  if (ns_selection_color == nil)
    ns_selection_color = NS_SELECTION_COLOR_DEFAULT;

  {
    NSColorList *cl = [NSColorList colorListNamed: @"Emacs"];

    if ( cl == nil )
      {
        Lisp_Object color_file, color_map, color;
        int r,g,b;
        unsigned long c;
        char *name;

        color_file = Fexpand_file_name (build_string ("rgb.txt"),
                         Fsymbol_value (intern ("data-directory")));
        if (NILP (Ffile_readable_p (color_file)))
          fatal ("Could not find %s.\n", SDATA (color_file));

        color_map = Fx_load_color_file (color_file);
        if (NILP (color_map))
          fatal ("Could not read %s.\n", SDATA (color_file));

        cl = [[NSColorList alloc] initWithName: @"Emacs"];
        for ( ; CONSP (color_map); color_map = XCDR (color_map))
          {
            color = XCAR (color_map);
            name = SDATA (XCAR (color));
            c = XINT (XCDR (color));
            [cl setColor:
                  [NSColor colorWithCalibratedRed: RED_FROM_ULONG (c) / 255.0
                                            green: GREEN_FROM_ULONG (c) / 255.0
                                             blue: BLUE_FROM_ULONG (c) / 255.0
                                            alpha: 1.0]
                  forKey: [NSString stringWithUTF8String: name]];
          }
        [cl writeToFile: nil];
      }
  }

  {
    char c[128];
#ifdef NS_IMPL_GNUSTEP
    strncpy (c, gnustep_base_version, sizeof (c));
#else
    /*PSnextrelease (128, c); */
    snprintf (c, sizeof (c), "%g", NSAppKitVersionNumber);
#endif
    Vwindow_system_version = build_string (c);
  }

  delete_keyboard_wait_descriptor (0);

  ns_app_name = [[NSProcessInfo processInfo] processName];

/* Set up OS X app menu */
#ifdef NS_IMPL_COCOA
  {
    NSMenu *appMenu;
    NSMenuItem *item;
    /* set up the application menu */
    svcsMenu = [[EmacsMenu alloc] initWithTitle: @"Services"];
    [svcsMenu setAutoenablesItems: NO];
    appMenu = [[EmacsMenu alloc] initWithTitle: @"Emacs"];
    [appMenu setAutoenablesItems: NO];
    mainMenu = [[EmacsMenu alloc] initWithTitle: @""];
    dockMenu = [[EmacsMenu alloc] initWithTitle: @""];

    [appMenu insertItemWithTitle: @"About Emacs"
                          action: @selector (orderFrontStandardAboutPanel:)
                   keyEquivalent: @""
                         atIndex: 0];
    [appMenu insertItem: [NSMenuItem separatorItem] atIndex: 1];
    [appMenu insertItemWithTitle: @"Preferences..."
                          action: @selector (showPreferencesWindow:)
                   keyEquivalent: @","
                         atIndex: 2];
    [appMenu insertItem: [NSMenuItem separatorItem] atIndex: 3];
    item = [appMenu insertItemWithTitle: @"Services"
                                 action: @selector (menuDown:)
                          keyEquivalent: @""
                                atIndex: 4];
    [appMenu setSubmenu: svcsMenu forItem: item];
    [appMenu insertItem: [NSMenuItem separatorItem] atIndex: 5];
    [appMenu insertItemWithTitle: @"Hide Emacs"
                          action: @selector (hide:)
                   keyEquivalent: @"h"
                         atIndex: 6];
    item =  [appMenu insertItemWithTitle: @"Hide Others"
                          action: @selector (hideOtherApplications:)
                   keyEquivalent: @"h"
                         atIndex: 7];
    [item setKeyEquivalentModifierMask: NSCommandKeyMask | NSAlternateKeyMask];
    [appMenu insertItem: [NSMenuItem separatorItem] atIndex: 8];
    [appMenu insertItemWithTitle: @"Quit Emacs"
                          action: @selector (terminate:)
                   keyEquivalent: @"q"
                         atIndex: 9];

    item = [mainMenu insertItemWithTitle: ns_app_name
                                  action: @selector (menuDown:)
                           keyEquivalent: @""
                                 atIndex: 0];
    [mainMenu setSubmenu: appMenu forItem: item];
    [dockMenu insertItemWithTitle: @"New Frame"
			   action: @selector (newFrame:)
		    keyEquivalent: @""
			  atIndex: 0];

    [NSApp setMainMenu: mainMenu];
    [NSApp setAppleMenu: appMenu];
    [NSApp setServicesMenu: svcsMenu];
    /* Needed at least on Cocoa, to get dock menu to show windows */
    [NSApp setWindowsMenu: [[NSMenu alloc] init]];

    [[NSNotificationCenter defaultCenter]
      addObserver: mainMenu
         selector: @selector (trackingNotification:)
             name: NSMenuDidBeginTrackingNotification object: mainMenu];
    [[NSNotificationCenter defaultCenter]
      addObserver: mainMenu
         selector: @selector (trackingNotification:)
             name: NSMenuDidEndTrackingNotification object: mainMenu];
  }
#endif /* MAC OS X menu setup */

  [NSApp run];

  return dpyinfo;
}


void
ns_term_shutdown (int sig)
{
  [[NSUserDefaults standardUserDefaults] synchronize];

  /* code not reached in emacs.c after this is called by shut_down_emacs: */
  if (STRINGP (Vauto_save_list_file_name))
    unlink (SDATA (Vauto_save_list_file_name));

  if (sig == 0 || sig == SIGTERM)
    {
      [NSApp terminate: NSApp];
    }
  else // force a stack trace to happen
    {
      abort();
    }
}


/* ==========================================================================

    EmacsApp implementation

   ========================================================================== */


@implementation EmacsApp

- (void)logNotification: (NSNotification *)notification
{
  const char *name = [[notification name] UTF8String];
  if (!strstr (name, "Update") && !strstr (name, "NSMenu")
      && !strstr (name, "WindowNumber"))
    NSLog (@"notification: '%@'", [notification name]);
}


- (void)sendEvent: (NSEvent *)theEvent
/* --------------------------------------------------------------------------
     Called when NSApp is running for each event received.  Used to stop
     the loop when we choose, since there's no way to just run one iteration.
   -------------------------------------------------------------------------- */
{
  int type = [theEvent type];
  NSWindow *window = [theEvent window];
/*  NSTRACE (sendEvent); */
/*fprintf (stderr, "received event of type %d\t%d\n", type);*/

#ifdef NS_IMPL_COCOA
  if (type == NSApplicationDefined
      && [theEvent data2] == NSAPP_DATA2_RUNASSCRIPT)
    {
      ns_run_ascript ();
      [self stop: self];
      return;
    }
#endif

  if (type == NSCursorUpdate && window == nil)
    {
      fprintf (stderr, "Dropping external cursor update event.\n");
      return;
    }

#ifdef NS_IMPL_COCOA
  /* pass mouse down in resize handle and subsequent drags directly to
     EmacsWindow so we can generate continuous redisplays */
  if (ns_in_resize)
    {
      if (type == NSLeftMouseDragged)
        {
          [window mouseDragged: theEvent];
          return;
        }
      else if (type == NSLeftMouseUp)
        {
          [window mouseUp: theEvent];
          return;
        }
    }
  else if (type == NSLeftMouseDown)
    {
      NSRect r = ns_resize_handle_rect (window);
      if (NSPointInRect ([theEvent locationInWindow], r))
        {
          ns_in_resize = YES;
          [window mouseDown: theEvent];
          return;
        }
    }
#endif

  if (type == NSApplicationDefined)
    {
      /* Events posted by ns_send_appdefined interrupt the run loop here.
         But, if a modal window is up, an appdefined can still come through,
         (e.g., from a makeKeyWindow event) but stopping self also stops the
         modal loop. Just defer it until later. */
      if ([NSApp modalWindow] == nil)
        {
          last_appdefined_event = theEvent;
          [self stop: self];
        }
      else
        {
          send_appdefined = YES;
        }
    }

  [super sendEvent: theEvent];
}


- (void)showPreferencesWindow: (id)sender
{
  struct frame *emacsframe = SELECTED_FRAME ();
  NSEvent *theEvent = [NSApp currentEvent];

  if (!emacs_event)
    return;
  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_SHOW_PREFS;
  emacs_event->modifiers = 0;
  EV_TRAILER (theEvent);
}


- (void)newFrame: (id)sender
{
  struct frame *emacsframe = SELECTED_FRAME ();
  NSEvent *theEvent = [NSApp currentEvent];

  if (!emacs_event)
    return;
  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_NEW_FRAME;
  emacs_event->modifiers = 0;
  EV_TRAILER (theEvent);
}


/* Open a file (used by below, after going into queue read by ns_read_socket) */
- (BOOL) openFile: (NSString *)fileName
{
  struct frame *emacsframe = SELECTED_FRAME ();
  NSEvent *theEvent = [NSApp currentEvent];

  if (!emacs_event)
    return NO;

  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_OPEN_FILE_LINE;
  ns_input_file = append2 (ns_input_file, build_string ([fileName UTF8String]));
  ns_input_line = Qnil; /* can be start or cons start,end */
  emacs_event->modifiers =0;
  EV_TRAILER (theEvent);

  return YES;
}


/* **************************************************************************

      EmacsApp delegate implementation

   ************************************************************************** */

- (void)applicationDidFinishLaunching: (NSNotification *)notification
/* --------------------------------------------------------------------------
     When application is loaded, terminate event loop in ns_term_init
   -------------------------------------------------------------------------- */
{
  NSTRACE (applicationDidFinishLaunching);
  [NSApp setServicesProvider: NSApp];
  ns_send_appdefined (-2);
}


/* Termination sequences:
    C-x C-c:
    Cmd-Q:
    MenuBar | File | Exit:
    Select Quit from App menubar:
        -terminate
	KEY_NS_POWER_OFF, (save-buffers-kill-emacs)
	ns_term_shutdown()

    Select Quit from Dock menu:
    Logout attempt:
        -appShouldTerminate
          Cancel -> Nothing else
          Accept ->

	  -terminate
	  KEY_NS_POWER_OFF, (save-buffers-kill-emacs)
	  ns_term_shutdown()

*/

- (void) terminate: (id)sender
{
  struct frame *emacsframe = SELECTED_FRAME ();

  if (!emacs_event)
    return;

  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_POWER_OFF;
  emacs_event->arg = Qt; /* mark as non-key event */
  EV_TRAILER ((id)nil);
}


- (NSApplicationTerminateReply)applicationShouldTerminate: (id)sender
{
  int ret;

  if (NILP (ns_confirm_quit)) //   || ns_shutdown_properly  --> TO DO
    return NSTerminateNow;

    ret = NSRunAlertPanel(ns_app_name,
                          [NSString stringWithUTF8String:"Exit requested.  Would you like to Save Buffers and Exit, or Cancel the request?"],
                          @"Save Buffers and Exit", @"Cancel", nil);

    if (ret == NSAlertDefaultReturn)
        return NSTerminateNow;
    else if (ret == NSAlertAlternateReturn)
        return NSTerminateCancel;
    return NSTerminateNow;  /* just in case */
}


/*   Notification from the Workspace to open a file */
- (BOOL)application: sender openFile: (NSString *)file
{
  [ns_pending_files addObject: file];
  return YES;
}


/*   Open a file as a temporary file */
- (BOOL)application: sender openTempFile: (NSString *)file
{
  [ns_pending_files addObject: file];
  return YES;
}


/*   Notification from the Workspace to open a file noninteractively (?) */
- (BOOL)application: sender openFileWithoutUI: (NSString *)file
{
  [ns_pending_files addObject: file];
  return YES;
}


/*   Notification from the Workspace to open multiple files */
- (void)application: sender openFiles: (NSArray *)fileList
{
  NSEnumerator *files = [fileList objectEnumerator];
  NSString *file;
  while ((file = [files nextObject]) != nil)
    [ns_pending_files addObject: file];

  [self replyToOpenOrPrint: NSApplicationDelegateReplySuccess];

}


/* Handle dock menu requests.  */
- (NSMenu *)applicationDockMenu: (NSApplication *) sender
{
  return dockMenu;
}


/* TODO: these may help w/IO switching btwn terminal and NSApp */
- (void)applicationWillBecomeActive: (NSNotification *)notification
{
  //ns_app_active=YES;
}
- (void)applicationDidBecomeActive: (NSNotification *)notification
{
  NSTRACE (applicationDidBecomeActive);

  //ns_app_active=YES;

  ns_update_auto_hide_menu_bar ();
  // No constraining takes place when the application is not active.
  ns_constrain_all_frames ();
}
- (void)applicationDidResignActive: (NSNotification *)notification
{
  //ns_app_active=NO;
  ns_send_appdefined (-1);
}



/* ==========================================================================

    EmacsApp aux handlers for managing event loop

   ========================================================================== */


- (void)timeout_handler: (NSTimer *)timedEntry
/* --------------------------------------------------------------------------
     The timeout specified to ns_select has passed.
   -------------------------------------------------------------------------- */
{
  /*NSTRACE (timeout_handler); */
  ns_send_appdefined (-2);
}

- (void)fd_handler: (NSTimer *) fdEntry
/* --------------------------------------------------------------------------
     Check data waiting on file descriptors and terminate if so
   -------------------------------------------------------------------------- */
{
  int result;
  /* NSTRACE (fd_handler); */

  if (select_nfds == 0)
    return;

  memcpy (&t_readfds, &select_readfds, sizeof (fd_set));

  select_timeout.tv_sec = select_timeout.tv_usec = 0;
  result = select (select_nfds, &t_readfds, (SELECT_TYPE *)0, (SELECT_TYPE *)0,
                  &select_timeout);
  if (result)
    {
      memcpy (&select_readfds, &t_readfds, sizeof (fd_set));
      ns_send_appdefined (result);
    }
}



/* ==========================================================================

    Service provision

   ========================================================================== */

/* called from system: queue for next pass through event loop */
- (void)requestService: (NSPasteboard *)pboard
              userData: (NSString *)userData
                 error: (NSString **)error
{
  [ns_pending_service_names addObject: userData];
  [ns_pending_service_args addObject: [NSString stringWithUTF8String:
      SDATA (ns_string_from_pasteboard (pboard))]];
}


/* called from ns_read_socket to clear queue */
- (BOOL)fulfillService: (NSString *)name withArg: (NSString *)arg
{
  struct frame *emacsframe = SELECTED_FRAME ();
  NSEvent *theEvent = [NSApp currentEvent];

  if (!emacs_event)
    return NO;

  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_SPI_SERVICE_CALL;
  ns_input_spi_name = build_string ([name UTF8String]);
  ns_input_spi_arg = build_string ([arg UTF8String]);
  emacs_event->modifiers = EV_MODIFIERS (theEvent);
  EV_TRAILER (theEvent);

  return YES;
}


@end  /* EmacsApp */



/* ==========================================================================

    EmacsView implementation

   ========================================================================== */


@implementation EmacsView

/* needed to inform when window closed from LISP */
- (void) setWindowClosing: (BOOL)closing
{
  windowClosing = closing;
}


- (void)dealloc
{
  NSTRACE (EmacsView_dealloc);
  [toolbar release];
  [super dealloc];
}


/* called on font panel selection */
- (void)changeFont: (id)sender
{
  NSEvent *e =[[self window] currentEvent];
  struct face *face =FRAME_DEFAULT_FACE (emacsframe);
  id newFont;
  float size;

  NSTRACE (changeFont);
  if (!emacs_event)
    return;

  if (newFont = [sender convertFont:
                           ((struct nsfont_info *)face->font)->nsfont])
    {
      SET_FRAME_GARBAGED (emacsframe); /* now needed as of 2008/10 */

      emacs_event->kind = NS_NONKEY_EVENT;
      emacs_event->modifiers = 0;
      emacs_event->code = KEY_NS_CHANGE_FONT;

      size = [newFont pointSize];
      ns_input_fontsize = make_number (lrint (size));
      ns_input_font = build_string ([[newFont familyName] UTF8String]);
      EV_TRAILER (e);
    }
}


- (BOOL)acceptsFirstResponder
{
  NSTRACE (acceptsFirstResponder);
  return YES;
}


- (void)resetCursorRects
{
  NSRect visible = [self visibleRect];
  NSCursor *currentCursor = FRAME_POINTER_TYPE (emacsframe);
  NSTRACE (resetCursorRects);

  if (currentCursor == nil)
    currentCursor = [NSCursor arrowCursor];

  if (!NSIsEmptyRect (visible))
    [self addCursorRect: visible cursor: currentCursor];
  [currentCursor setOnMouseEntered: YES];
}



/*****************************************************************************/
/* Keyboard handling. */
#define NS_KEYLOG 0

- (void)keyDown: (NSEvent *)theEvent
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (emacsframe);
  int code;
  unsigned fnKeysym = 0;
  int flags;
  static NSMutableArray *nsEvArray;
#if !defined (NS_IMPL_COCOA) || MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_6
  static BOOL firstTime = YES;
#endif
  int left_is_none;

  NSTRACE (keyDown);

  /* Rhapsody and OS X give up and down events for the arrow keys */
  if (ns_fake_keydown == YES)
    ns_fake_keydown = NO;
  else if ([theEvent type] != NSKeyDown)
    return;

  if (!emacs_event)
    return;

 if (![[self window] isKeyWindow]
     && [[theEvent window] isKindOfClass: [EmacsWindow class]]
     /* we must avoid an infinite loop here. */
     && (EmacsView *)[[theEvent window] delegate] != self)
   {
     /* XXX: There is an occasional condition in which, when Emacs display
         updates a different frame from the current one, and temporarily
         selects it, then processes some interrupt-driven input
         (dispnew.c:3878), OS will send the event to the correct NSWindow, but
         for some reason that window has its first responder set to the NSView
         most recently updated (I guess), which is not the correct one. */
     [(EmacsView *)[[theEvent window] delegate] keyDown: theEvent];
     return;
   }

  if (nsEvArray == nil)
    nsEvArray = [[NSMutableArray alloc] initWithCapacity: 1];

  [NSCursor setHiddenUntilMouseMoves: YES];

  if (hlinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight))
    {
      clear_mouse_face (hlinfo);
      hlinfo->mouse_face_hidden = 1;
    }

  if (!processingCompose)
    {
      code = ([[theEvent charactersIgnoringModifiers] length] == 0) ?
        0 : [[theEvent charactersIgnoringModifiers] characterAtIndex: 0];
      /* (Carbon way: [theEvent keyCode]) */

      /* is it a "function key"? */
      fnKeysym = ns_convert_key (code);
      if (fnKeysym)
        {
          /* COUNTERHACK: map 'Delete' on upper-right main KB to 'Backspace',
             because Emacs treats Delete and KP-Delete same (in simple.el). */
          if (fnKeysym == 0xFFFF && [theEvent keyCode] == 0x33)
            code = 0xFF08; /* backspace */
          else
            code = fnKeysym;
        }

      /* are there modifiers? */
      emacs_event->modifiers = 0;
      flags = [theEvent modifierFlags];

      if (flags & NSHelpKeyMask)
          emacs_event->modifiers |= hyper_modifier;

      if (flags & NSShiftKeyMask)
        emacs_event->modifiers |= shift_modifier;

      if ((flags & NSRightCommandKeyMask) == NSRightCommandKeyMask)
        emacs_event->modifiers |= parse_solitary_modifier
          (EQ (ns_right_command_modifier, Qleft)
           ? ns_command_modifier
           : ns_right_command_modifier);

      if ((flags & NSLeftCommandKeyMask) == NSLeftCommandKeyMask)
        {
          emacs_event->modifiers |= parse_solitary_modifier
            (ns_command_modifier);

          /* if super (default), take input manager's word so things like
             dvorak / qwerty layout work */
          if (EQ (ns_command_modifier, Qsuper)
              && !fnKeysym
              && [[theEvent characters] length] != 0)
            {
              /* XXX: the code we get will be unshifted, so if we have
                 a shift modifier, must convert ourselves */
              if (!(flags & NSShiftKeyMask))
                code = [[theEvent characters] characterAtIndex: 0];
#if 0
              /* this is ugly and also requires linking w/Carbon framework
                 (for LMGetKbdType) so for now leave this rare (?) case
                 undealt with.. in future look into CGEvent methods */
              else
                {
                  long smv = GetScriptManagerVariable (smKeyScript);
                  Handle uchrHandle = GetResource
                    ('uchr', GetScriptVariable (smv, smScriptKeys));
                  UInt32 dummy = 0;
                  UCKeyTranslate ((UCKeyboardLayout*)*uchrHandle,
                                 [[theEvent characters] characterAtIndex: 0],
                                 kUCKeyActionDisplay,
                                 (flags & ~NSCommandKeyMask) >> 8,
                                 LMGetKbdType (), kUCKeyTranslateNoDeadKeysMask,
                                 &dummy, 1, &dummy, &code);
                  code &= 0xFF;
                }
#endif
            }
        }

      if ((flags & NSRightControlKeyMask) == NSRightControlKeyMask)
          emacs_event->modifiers |= parse_solitary_modifier
              (EQ (ns_right_control_modifier, Qleft)
               ? ns_control_modifier
               : ns_right_control_modifier);

      if ((flags & NSLeftControlKeyMask) == NSLeftControlKeyMask)
        emacs_event->modifiers |= parse_solitary_modifier
          (ns_control_modifier);

      if (flags & NS_FUNCTION_KEY_MASK && !fnKeysym)
          emacs_event->modifiers |=
            parse_solitary_modifier (ns_function_modifier);

      left_is_none = NILP (ns_alternate_modifier)
        || EQ (ns_alternate_modifier, Qnone);

      if ((flags & NSRightAlternateKeyMask) == NSRightAlternateKeyMask)
        {
          if ((NILP (ns_right_alternate_modifier)
               || EQ (ns_right_alternate_modifier, Qnone)
               || (EQ (ns_right_alternate_modifier, Qleft) && left_is_none))
              && !fnKeysym)
            {   /* accept pre-interp alt comb */
              if ([[theEvent characters] length] > 0)
                code = [[theEvent characters] characterAtIndex: 0];
              /*HACK: clear lone shift modifier to stop next if from firing */
              if (emacs_event->modifiers == shift_modifier)
                emacs_event->modifiers = 0;
            }
          else
            emacs_event->modifiers |= parse_solitary_modifier
              (EQ (ns_right_alternate_modifier, Qleft)
               ? ns_alternate_modifier
               : ns_right_alternate_modifier);
        }

      if ((flags & NSLeftAlternateKeyMask) == NSLeftAlternateKeyMask) /* default = meta */
        {
          if (left_is_none && !fnKeysym)
            {   /* accept pre-interp alt comb */
              if ([[theEvent characters] length] > 0)
                code = [[theEvent characters] characterAtIndex: 0];
              /*HACK: clear lone shift modifier to stop next if from firing */
              if (emacs_event->modifiers == shift_modifier)
                emacs_event->modifiers = 0;
            }
          else
              emacs_event->modifiers |=
                parse_solitary_modifier (ns_alternate_modifier);
        }

  if (NS_KEYLOG)
    fprintf (stderr, "keyDown: code =%x\tfnKey =%x\tflags = %x\tmods = %x\n",
             code, fnKeysym, flags, emacs_event->modifiers);

      /* if it was a function key or had modifiers, pass it directly to emacs */
      if (fnKeysym || (emacs_event->modifiers
                       && (emacs_event->modifiers != shift_modifier)
                       && [[theEvent charactersIgnoringModifiers] length] > 0))
/*[[theEvent characters] length] */
        {
          emacs_event->kind = NON_ASCII_KEYSTROKE_EVENT;
          if (code < 0x20)
            code |= (1<<28)|(3<<16);
          else if (code == 0x7f)
            code |= (1<<28)|(3<<16);
          else if (!fnKeysym)
            emacs_event->kind = code > 0xFF
              ? MULTIBYTE_CHAR_KEYSTROKE_EVENT : ASCII_KEYSTROKE_EVENT;

          emacs_event->code = code;
          EV_TRAILER (theEvent);
          return;
        }
    }


#if !defined (NS_IMPL_COCOA) || MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_6
  /* if we get here we should send the key for input manager processing */
  if (firstTime && [[NSInputManager currentInputManager]
                     wantsToDelayTextChangeNotifications] == NO)
    fprintf (stderr,
          "Emacs: WARNING: TextInput mgr wants marked text to be permanent!\n");
  firstTime = NO;
#endif
  if (NS_KEYLOG && !processingCompose)
    fprintf (stderr, "keyDown: Begin compose sequence.\n");

  processingCompose = YES;
  [nsEvArray addObject: theEvent];
  [self interpretKeyEvents: nsEvArray];
  [nsEvArray removeObject: theEvent];
}


#ifdef NS_IMPL_COCOA
/* Needed to pick up Ctrl-tab and possibly other events that OS X has
   decided not to send key-down for.
   See http://osdir.com/ml/editors.vim.mac/2007-10/msg00141.html
   This only applies on Tiger and earlier.
   If it matches one of these, send it on to keyDown. */
-(void)keyUp: (NSEvent *)theEvent
{
  int flags = [theEvent modifierFlags];
  int code = [theEvent keyCode];
  if (floor (NSAppKitVersionNumber) <= 824 /*NSAppKitVersionNumber10_4*/ &&
      code == 0x30 && (flags & NSControlKeyMask) && !(flags & NSCommandKeyMask))
    {
      if (NS_KEYLOG)
        fprintf (stderr, "keyUp: passed test");
      ns_fake_keydown = YES;
      [self keyDown: theEvent];
    }
}
#endif


/* <NSTextInput> implementation (called through super interpretKeyEvents:]). */


/* <NSTextInput>: called when done composing;
   NOTE: also called when we delete over working text, followed immed.
         by doCommandBySelector: deleteBackward: */
- (void)insertText: (id)aString
{
  int code;
  int len = [(NSString *)aString length];
  int i;

  if (NS_KEYLOG)
    NSLog (@"insertText '%@'\tlen = %d", aString, len);
  processingCompose = NO;

  if (!emacs_event)
    return;

  /* first, clear any working text */
  if (workingText != nil)
    [self deleteWorkingText];

  /* now insert the string as keystrokes */
  for (i =0; i<len; i++)
    {
      code = [aString characterAtIndex: i];
      /* TODO: still need this? */
      if (code == 0x2DC)
        code = '~'; /* 0x7E */
      emacs_event->modifiers = 0;
      emacs_event->kind
	= code > 0xFF ? MULTIBYTE_CHAR_KEYSTROKE_EVENT : ASCII_KEYSTROKE_EVENT;
      emacs_event->code = code;
      EV_TRAILER ((id)nil);
    }
}


/* <NSTextInput>: inserts display of composing characters */
- (void)setMarkedText: (id)aString selectedRange: (NSRange)selRange
{
  NSString *str = [aString respondsToSelector: @selector (string)] ?
    [aString string] : aString;
  if (NS_KEYLOG)
    NSLog (@"setMarkedText '%@' len =%d range %d from %d", str, [str length],
           selRange.length, selRange.location);

  if (workingText != nil)
    [self deleteWorkingText];
  if ([str length] == 0)
    return;

  if (!emacs_event)
    return;

  processingCompose = YES;
  workingText = [str copy];
  ns_working_text = build_string ([workingText UTF8String]);

  emacs_event->kind = NS_TEXT_EVENT;
  emacs_event->code = KEY_NS_PUT_WORKING_TEXT;
  EV_TRAILER ((id)nil);
}


/* delete display of composing characters [not in <NSTextInput>] */
- (void)deleteWorkingText
{
  if (workingText == nil)
    return;
  if (NS_KEYLOG)
    NSLog(@"deleteWorkingText len =%d\n", [workingText length]);
  [workingText release];
  workingText = nil;
  processingCompose = NO;

  if (!emacs_event)
    return;

  emacs_event->kind = NS_TEXT_EVENT;
  emacs_event->code = KEY_NS_UNPUT_WORKING_TEXT;
  EV_TRAILER ((id)nil);
}


- (BOOL)hasMarkedText
{
  return workingText != nil;
}


- (NSRange)markedRange
{
  NSRange rng = workingText != nil
    ? NSMakeRange (0, [workingText length]) : NSMakeRange (NSNotFound, 0);
  if (NS_KEYLOG)
    NSLog (@"markedRange request");
  return rng;
}


- (void)unmarkText
{
  if (NS_KEYLOG)
    NSLog (@"unmark (accept) text");
  [self deleteWorkingText];
  processingCompose = NO;
}


/* used to position char selection windows, etc. */
- (NSRect)firstRectForCharacterRange: (NSRange)theRange
{
  NSRect rect;
  NSPoint pt;
  struct window *win = XWINDOW (FRAME_SELECTED_WINDOW (emacsframe));
  if (NS_KEYLOG)
    NSLog (@"firstRectForCharRange request");

  rect.size.width = theRange.length * FRAME_COLUMN_WIDTH (emacsframe);
  rect.size.height = FRAME_LINE_HEIGHT (emacsframe);
  pt.x = WINDOW_TEXT_TO_FRAME_PIXEL_X (win, win->phys_cursor.x);
  pt.y = WINDOW_TO_FRAME_PIXEL_Y (win, win->phys_cursor.y
                                       +FRAME_LINE_HEIGHT (emacsframe));

  pt = [self convertPoint: pt toView: nil];
  pt = [[self window] convertBaseToScreen: pt];
  rect.origin = pt;
  return rect;
}


- (long)conversationIdentifier
{
  return (long)self;
}


- (void)doCommandBySelector: (SEL)aSelector
{
  if (NS_KEYLOG)
    NSLog (@"doCommandBySelector: %@", NSStringFromSelector (aSelector));

  if (aSelector == @selector (deleteBackward:))
    {
      /* happens when user backspaces over an ongoing composition:
         throw a 'delete' into the event queue */
      if (!emacs_event)
        return;
      emacs_event->kind = NON_ASCII_KEYSTROKE_EVENT;
      emacs_event->code = 0xFF08;
      EV_TRAILER ((id)nil);
    }
}

- (NSArray *)validAttributesForMarkedText
{
  static NSArray *arr = nil;
  if (arr == nil) arr = [NSArray new];
 /* [[NSArray arrayWithObject: NSUnderlineStyleAttributeName] retain]; */
  return arr;
}

- (NSRange)selectedRange
{
  if (NS_KEYLOG)
    NSLog (@"selectedRange request");
  return NSMakeRange (NSNotFound, 0);
}

- (NSUInteger)characterIndexForPoint: (NSPoint)thePoint
{
  if (NS_KEYLOG)
    NSLog (@"characterIndexForPoint request");
  return 0;
}

- (NSAttributedString *)attributedSubstringFromRange: (NSRange)theRange
{
  static NSAttributedString *str = nil;
  if (str == nil) str = [NSAttributedString new];
  if (NS_KEYLOG)
    NSLog (@"attributedSubstringFromRange request");
  return str;
}

/* End <NSTextInput> impl. */
/*****************************************************************************/


/* This is what happens when the user presses a mouse button.  */
- (void)mouseDown: (NSEvent *)theEvent
{
  NSPoint p = [self convertPoint: [theEvent locationInWindow] fromView: nil];
  Lisp_Object window;

  NSTRACE (mouseDown);

  [self deleteWorkingText];

  if (!emacs_event)
    return;

  last_mouse_frame = emacsframe;
  /* appears to be needed to prevent spurious movement events generated on
     button clicks */
  last_mouse_frame->mouse_moved = 0;

  if ([theEvent type] == NSScrollWheel)
    {
      float delta = [theEvent deltaY];
      /* Mac notebooks send wheel events w/delta =0 when trackpad scrolling */
      if (delta == 0)
        return;
      emacs_event->kind = WHEEL_EVENT;
      emacs_event->code = 0;
      emacs_event->modifiers = EV_MODIFIERS (theEvent) |
        ((delta > 0) ? up_modifier : down_modifier);
    }
  else
    {
      emacs_event->kind = MOUSE_CLICK_EVENT;
      emacs_event->code = EV_BUTTON (theEvent);
      emacs_event->modifiers = EV_MODIFIERS (theEvent)
                             | EV_UDMODIFIERS (theEvent);
    }
  XSETINT (emacs_event->x, lrint (p.x));
  XSETINT (emacs_event->y, lrint (p.y));
  EV_TRAILER (theEvent);
}


- (void)rightMouseDown: (NSEvent *)theEvent
{
  NSTRACE (rightMouseDown);
  [self mouseDown: theEvent];
}


- (void)otherMouseDown: (NSEvent *)theEvent
{
  NSTRACE (otherMouseDown);
  [self mouseDown: theEvent];
}


- (void)mouseUp: (NSEvent *)theEvent
{
  NSTRACE (mouseUp);
  [self mouseDown: theEvent];
}


- (void)rightMouseUp: (NSEvent *)theEvent
{
  NSTRACE (rightMouseUp);
  [self mouseDown: theEvent];
}


- (void)otherMouseUp: (NSEvent *)theEvent
{
  NSTRACE (otherMouseUp);
  [self mouseDown: theEvent];
}


- (void) scrollWheel: (NSEvent *)theEvent
{
  NSTRACE (scrollWheel);
  [self mouseDown: theEvent];
}


/* Tell emacs the mouse has moved. */
- (void)mouseMoved: (NSEvent *)e
{
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (emacsframe);
  Lisp_Object frame;

//  NSTRACE (mouseMoved);

  last_mouse_movement_time = EV_TIMESTAMP (e);
  last_mouse_motion_position
    = [self convertPoint: [e locationInWindow] fromView: nil];

  /* update any mouse face */
  if (hlinfo->mouse_face_hidden)
    {
      hlinfo->mouse_face_hidden = 0;
      clear_mouse_face (hlinfo);
    }

  /* tooltip handling */
  previous_help_echo_string = help_echo_string;
  help_echo_string = Qnil;

  if (!note_mouse_movement (emacsframe, last_mouse_motion_position.x,
                            last_mouse_motion_position.y))
    help_echo_string = previous_help_echo_string;

  XSETFRAME (frame, emacsframe);
  if (!NILP (help_echo_string) || !NILP (previous_help_echo_string))
    {
      /* NOTE: help_echo_{window,pos,object} are set in xdisp.c
         (note_mouse_highlight), which is called through the
         note_mouse_movement () call above */
      gen_help_event (help_echo_string, frame, help_echo_window,
                      help_echo_object, help_echo_pos);
    }
  else
    {
      help_echo_string = Qnil;
      gen_help_event (Qnil, frame, Qnil, Qnil, 0);
    }

  if (emacsframe->mouse_moved && send_appdefined)
    ns_send_appdefined (-1);
}


- (void)mouseDragged: (NSEvent *)e
{
  NSTRACE (mouseDragged);
  [self mouseMoved: e];
}


- (void)rightMouseDragged: (NSEvent *)e
{
  NSTRACE (rightMouseDragged);
  [self mouseMoved: e];
}


- (void)otherMouseDragged: (NSEvent *)e
{
  NSTRACE (otherMouseDragged);
  [self mouseMoved: e];
}


- (BOOL)windowShouldClose: (id)sender
{
  NSEvent *e =[[self window] currentEvent];

  NSTRACE (windowShouldClose);
  windowClosing = YES;
  if (!emacs_event)
    return NO;
  emacs_event->kind = DELETE_WINDOW_EVENT;
  emacs_event->modifiers = 0;
  emacs_event->code = 0;
  EV_TRAILER (e);
  /* Don't close this window, let this be done from lisp code.  */
  return NO;
}


- (NSSize)windowWillResize: (NSWindow *)sender toSize: (NSSize)frameSize
/* normalize frame to gridded text size */
{
  NSTRACE (windowWillResize);
/*fprintf (stderr,"Window will resize: %.0f x %.0f\n",frameSize.width,frameSize.height); */

  cols = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (emacsframe,
#ifdef NS_IMPL_GNUSTEP
                                        frameSize.width + 3);
#else
                                        frameSize.width);
#endif
  if (cols < MINWIDTH)
    cols = MINWIDTH;

  rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (emacsframe, frameSize.height
#ifdef NS_IMPL_GNUSTEP
      - FRAME_NS_TITLEBAR_HEIGHT (emacsframe) + 3
        - FRAME_TOOLBAR_HEIGHT (emacsframe));
#else
      - FRAME_NS_TITLEBAR_HEIGHT (emacsframe)
        - FRAME_TOOLBAR_HEIGHT (emacsframe));
#endif
  if (rows < MINHEIGHT)
    rows = MINHEIGHT;
#ifdef NS_IMPL_COCOA
  {
    /* this sets window title to have size in it; the wm does this under GS */
    NSRect r = [[self window] frame];
    if (r.size.height == frameSize.height && r.size.width == frameSize.width)
      {
        if (old_title != 0)
          {
            xfree (old_title);
            old_title = 0;
          }
      }
    else
      {
        char *size_title;
        NSWindow *window = [self window];
        if (old_title == 0)
          {
            const char *t = [[[self window] title] UTF8String];
            char *pos = strstr (t, "  —  ");
            if (pos)
              *pos = '\0';
            old_title = (char *) xmalloc (strlen (t) + 1);
            strcpy (old_title, t);
          }
        size_title = xmalloc (strlen (old_title) + 40);
	esprintf (size_title, "%s  —  (%d x %d)", old_title, cols, rows);
        [window setTitle: [NSString stringWithUTF8String: size_title]];
        [window display];
        xfree (size_title);
      }
  }
#endif /* NS_IMPL_COCOA */
/*fprintf (stderr,"    ...size became %.0f x %.0f  (%d x %d)\n",frameSize.width,frameSize.height,cols,rows); */

  return frameSize;
}


- (void)windowDidResize: (NSNotification *)notification
{
  NSWindow *theWindow = [notification object];

#ifdef NS_IMPL_GNUSTEP
   /* in GNUstep, at least currently, it's possible to get a didResize
      without getting a willResize.. therefore we need to act as if we got
      the willResize now */
  NSSize sz = [theWindow frame].size;
  sz = [self windowWillResize: theWindow toSize: sz];
#endif /* NS_IMPL_GNUSTEP */

  NSTRACE (windowDidResize);
/*fprintf (stderr,"windowDidResize: %.0f\n",[theWindow frame].size.height); */

#ifdef NS_IMPL_COCOA
  if (old_title != 0)
    {
      xfree (old_title);
      old_title = 0;
    }
#endif /* NS_IMPL_COCOA */

  /* Avoid loop under GNUstep due to call at beginning of this function.
     (x_set_window_size causes a resize which causes
     a "windowDidResize" which calls x_set_window_size).  */
#ifndef NS_IMPL_GNUSTEP
  if (cols > 0 && rows > 0)
    {
      if (ns_in_resize)
        x_set_window_size (emacsframe, 0, cols, rows);
      else
        {
          NSWindow *window = [self window];
          NSRect wr = [window frame];
          FRAME_PIXEL_WIDTH (emacsframe) = (int)wr.size.width
            - emacsframe->border_width;
          FRAME_PIXEL_HEIGHT (emacsframe) = (int)wr.size.height
            - FRAME_NS_TITLEBAR_HEIGHT (emacsframe)
            - FRAME_TOOLBAR_HEIGHT (emacsframe);
          change_frame_size (emacsframe, rows, cols, 0, 0, 1);
          SET_FRAME_GARBAGED (emacsframe);
          cancel_mouse_face (emacsframe);
        }
    }
#endif

  ns_send_appdefined (-1);
}


- (void)windowDidBecomeKey: (NSNotification *)notification
/* cf. x_detect_focus_change(), x_focus_changed(), x_new_focus_frame() */
{
  struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (emacsframe);
  struct frame *old_focus = dpyinfo->x_focus_frame;

  NSTRACE (windowDidBecomeKey);

  if (emacsframe != old_focus)
    dpyinfo->x_focus_frame = emacsframe;

  ns_frame_rehighlight (emacsframe);

  if (emacs_event)
    {
      emacs_event->kind = FOCUS_IN_EVENT;
      EV_TRAILER ((id)nil);
    }
}


- (void)windowDidResignKey: (NSNotification *)notification
/* cf. x_detect_focus_change(), x_focus_changed(), x_new_focus_frame() */
{
  struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (emacsframe);
  NSTRACE (windowDidResignKey);

  if (dpyinfo->x_focus_frame == emacsframe)
    dpyinfo->x_focus_frame = 0;

  ns_frame_rehighlight (emacsframe);

  /* FIXME: for some reason needed on second and subsequent clicks away
            from sole-frame Emacs to get hollow box to show */
  if (!windowClosing && [[self window] isVisible] == YES)
    {
      x_update_cursor (emacsframe, 1);
      x_set_frame_alpha (emacsframe);
    }

  if (emacs_event)
    {
      [self deleteWorkingText];
      emacs_event->kind = FOCUS_IN_EVENT;
      EV_TRAILER ((id)nil);
    }
}


- (void)windowWillMiniaturize: sender
{
  NSTRACE (windowWillMiniaturize);
}


- (BOOL)isFlipped
{
  return YES;
}


- (BOOL)isOpaque
{
  return NO;
}


- initFrameFromEmacs: (struct frame *)f
{
  NSRect r, wr;
  Lisp_Object tem;
  NSWindow *win;
  NSButton *toggleButton;
  int vbextra = NS_SCROLL_BAR_WIDTH (f);
  NSSize sz;
  NSColor *col;
  NSString *name;

  NSTRACE (initFrameFromEmacs);

  windowClosing = NO;
  processingCompose = NO;
  scrollbarsNeedingUpdate = 0;

/*fprintf (stderr,"init with %d, %d\n",f->text_cols, f->text_lines); */

  ns_userRect = NSMakeRect (0, 0, 0, 0);
  r = NSMakeRect (0, 0, FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, f->text_cols),
                 FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, f->text_lines));
  [self initWithFrame: r];
  [self setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];

  FRAME_NS_VIEW (f) = self;
  emacsframe = f;
  old_title = 0;

  win = [[EmacsWindow alloc]
            initWithContentRect: r
                      styleMask: (NSResizableWindowMask |
#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_7
                                  NSTitledWindowMask |
#endif
                                  NSMiniaturizableWindowMask |
                                  NSClosableWindowMask)
                        backing: NSBackingStoreBuffered
                          defer: YES];

  wr = [win frame];
  f->border_width = wr.size.width - r.size.width;
  FRAME_NS_TITLEBAR_HEIGHT (f) = wr.size.height - r.size.height;

  [win setAcceptsMouseMovedEvents: YES];
  [win setDelegate: self];
  [win useOptimizedDrawing: YES];

  sz.width = FRAME_COLUMN_WIDTH (f);
  sz.height = FRAME_LINE_HEIGHT (f);
  [win setResizeIncrements: sz];

  [[win contentView] addSubview: self];

  if (ns_drag_types)
    [self registerForDraggedTypes: ns_drag_types];

  tem = f->name;
  name = [NSString stringWithUTF8String:
                   NILP (tem) ? (unsigned char *)"Emacs" : SDATA (tem)];
  [win setTitle: name];

  /* toolbar support */
  toolbar = [[EmacsToolbar alloc] initForView: self withIdentifier:
                         [NSString stringWithFormat: @"Emacs Frame %d",
                                   ns_window_num]];
  [win setToolbar: toolbar];
  [toolbar setVisible: NO];
#ifdef NS_IMPL_COCOA
  toggleButton = [win standardWindowButton: NSWindowToolbarButton];
  [toggleButton setTarget: self];
  [toggleButton setAction: @selector (toggleToolbar: )];
#endif
  FRAME_TOOLBAR_HEIGHT (f) = 0;

  tem = f->icon_name;
  if (!NILP (tem))
    [win setMiniwindowTitle:
           [NSString stringWithUTF8String: SDATA (tem)]];

  {
    NSScreen *screen = [win screen];

    if (screen != 0)
      [win setFrameTopLeftPoint: NSMakePoint
           (IN_BOUND (-SCREENMAX, f->left_pos, SCREENMAX),
            IN_BOUND (-SCREENMAX,
                     [screen frame].size.height - NS_TOP_POS (f), SCREENMAX))];
  }

  [win makeFirstResponder: self];

  col = ns_lookup_indexed_color (NS_FACE_BACKGROUND
                                  (FRAME_DEFAULT_FACE (emacsframe)), emacsframe);
  [win setBackgroundColor: col];
  if ([col alphaComponent] != 1.0)
    [win setOpaque: NO];

  [self allocateGState];

  [NSApp registerServicesMenuSendTypes: ns_send_types
                           returnTypes: nil];

  ns_window_num++;
  return self;
}


- (void)windowDidMove: sender
{
  NSWindow *win = [self window];
  NSRect r = [win frame];
  NSArray *screens = [NSScreen screens];
  NSScreen *screen = [screens objectAtIndex: 0];

  NSTRACE (windowDidMove);

  if (!emacsframe->output_data.ns)
    return;
  if (screen != nil)
    {
      emacsframe->left_pos = r.origin.x;
      emacsframe->top_pos =
        [screen frame].size.height - (r.origin.y + r.size.height);
    }
}


/* Called AFTER method below, but before our windowWillResize call there leads
   to windowDidResize -> x_set_window_size.  Update emacs' notion of frame
   location so set_window_size moves the frame. */
- (BOOL)windowShouldZoom: (NSWindow *)sender toFrame: (NSRect)newFrame
{
  emacsframe->output_data.ns->zooming = 1;
  return YES;
}


/* Override to do something slightly nonstandard, but nice.  First click on
   zoom button will zoom vertically.  Second will zoom completely.  Third
   returns to original. */
- (NSRect)windowWillUseStandardFrame:(NSWindow *)sender
                        defaultFrame:(NSRect)defaultFrame
{
  NSRect result = [sender frame];

  NSTRACE (windowWillUseStandardFrame);

  if (abs (defaultFrame.size.height - result.size.height)
      > FRAME_LINE_HEIGHT (emacsframe))
    {
      /* first click */
      ns_userRect = result;
      result.size.height = defaultFrame.size.height;
      result.origin.y = defaultFrame.origin.y;
    }
  else
    {
      if (abs (defaultFrame.size.width - result.size.width)
          > FRAME_COLUMN_WIDTH (emacsframe))
        result = defaultFrame;  /* second click */
      else
        {
          /* restore */
          result = ns_userRect.size.height ? ns_userRect : result;
          ns_userRect = NSMakeRect (0, 0, 0, 0);
        }
    }

  [self windowWillResize: sender toSize: result.size];
  return result;
}


- (void)windowDidDeminiaturize: sender
{
  NSTRACE (windowDidDeminiaturize);
  if (!emacsframe->output_data.ns)
    return;
  emacsframe->async_iconified = 0;
  emacsframe->async_visible   = 1;
  windows_or_buffers_changed++;

  if (emacs_event)
    {
      emacs_event->kind = ICONIFY_EVENT;
      EV_TRAILER ((id)nil);
    }
}


- (void)windowDidExpose: sender
{
  NSTRACE (windowDidExpose);
  if (!emacsframe->output_data.ns)
    return;
  emacsframe->async_visible = 1;
  SET_FRAME_GARBAGED (emacsframe);

  if (send_appdefined)
    ns_send_appdefined (-1);
}


- (void)windowDidMiniaturize: sender
{
  NSTRACE (windowDidMiniaturize);
  if (!emacsframe->output_data.ns)
    return;

  emacsframe->async_iconified = 1;
  emacsframe->async_visible = 0;

  if (emacs_event)
    {
      emacs_event->kind = ICONIFY_EVENT;
      EV_TRAILER ((id)nil);
    }
}


- (void)mouseEntered: (NSEvent *)theEvent
{
  NSPoint p = [self convertPoint: [theEvent locationInWindow] fromView: nil];
  struct ns_display_info *dpyinfo = FRAME_NS_DISPLAY_INFO (emacsframe);
  NSTRACE (mouseEntered);

  last_mouse_movement_time = EV_TIMESTAMP (theEvent);
}


- (void)mouseExited: (NSEvent *)theEvent
{
  NSPoint p = [self convertPoint: [theEvent locationInWindow] fromView: nil];
  NSRect r;
  Mouse_HLInfo *hlinfo = emacsframe ? MOUSE_HL_INFO (emacsframe) : NULL;

  NSTRACE (mouseExited);

  if (!hlinfo)
    return;

  last_mouse_movement_time = EV_TIMESTAMP (theEvent);

  if (emacsframe == hlinfo->mouse_face_mouse_frame)
    {
      clear_mouse_face (hlinfo);
      hlinfo->mouse_face_mouse_frame = 0;
    }
}


- menuDown: sender
{
  NSTRACE (menuDown);
  if (context_menu_value == -1)
    context_menu_value = [sender tag];
  else
    find_and_call_menu_selection (emacsframe, emacsframe->menu_bar_items_used,
                                  emacsframe->menu_bar_vector,
                                  (void *)[sender tag]);
  ns_send_appdefined (-1);
  return self;
}


- (EmacsToolbar *)toolbar
{
  return toolbar;
}


/* this gets called on toolbar button click */
- toolbarClicked: (id)item
{
  NSEvent *theEvent;
  int idx = [item tag] * TOOL_BAR_ITEM_NSLOTS;

  NSTRACE (toolbarClicked);

  if (!emacs_event)
    return self;

  /* send first event (for some reason two needed) */
  theEvent = [[self window] currentEvent];
  emacs_event->kind = TOOL_BAR_EVENT;
  XSETFRAME (emacs_event->arg, emacsframe);
  EV_TRAILER (theEvent);

  emacs_event->kind = TOOL_BAR_EVENT;
/*   XSETINT (emacs_event->code, 0); */
  emacs_event->arg = AREF (emacsframe->tool_bar_items,
                          idx + TOOL_BAR_ITEM_KEY);
  emacs_event->modifiers = EV_MODIFIERS (theEvent);
  EV_TRAILER (theEvent);
  return self;
}


- toggleToolbar: (id)sender
{
  if (!emacs_event)
    return self;

  emacs_event->kind = NS_NONKEY_EVENT;
  emacs_event->code = KEY_NS_TOGGLE_TOOLBAR;
  EV_TRAILER ((id)nil);
  return self;
}


- (void)drawRect: (NSRect)rect
{
  int x = NSMinX (rect), y = NSMinY (rect);
  int width = NSWidth (rect), height = NSHeight (rect);

  NSTRACE (drawRect);

  if (!emacsframe || !emacsframe->output_data.ns || ns_in_resize)
    return;

  ns_clear_frame_area (emacsframe, x, y, width, height);
  expose_frame (emacsframe, x, y, width, height);

  /*
    drawRect: may be called (at least in OS X 10.5) for invisible
    views as well for some reason.  Thus, do not infer visibility
    here.

    emacsframe->async_visible = 1;
    emacsframe->async_iconified = 0;
  */
}


/* NSDraggingDestination protocol methods.  Actually this is not really a
   protocol, but a category of Object.  O well...  */

-(NSUInteger) draggingEntered: (id <NSDraggingInfo>) sender
{
  NSTRACE (draggingEntered);
  return NSDragOperationGeneric;
}


-(BOOL)prepareForDragOperation: (id <NSDraggingInfo>) sender
{
  return YES;
}


-(BOOL)performDragOperation: (id <NSDraggingInfo>) sender
{
  id pb;
  int x, y;
  NSString *type;
  NSEvent *theEvent = [[self window] currentEvent];
  NSPoint position;

  NSTRACE (performDragOperation);

  if (!emacs_event)
    return NO;

  position = [self convertPoint: [sender draggingLocation] fromView: nil];
  x = lrint (position.x);  y = lrint (position.y);

  pb = [sender draggingPasteboard];
  type = [pb availableTypeFromArray: ns_drag_types];
  if (type == 0)
    {
      return NO;
    }
  else if ([type isEqualToString: NSFilenamesPboardType])
    {
      NSArray *files;
      NSEnumerator *fenum;
      NSString *file;

      if (!(files = [pb propertyListForType: type]))
        return NO;

      fenum = [files objectEnumerator];
      while ( (file = [fenum nextObject]) )
        {
          emacs_event->kind = NS_NONKEY_EVENT;
          emacs_event->code = KEY_NS_DRAG_FILE;
          XSETINT (emacs_event->x, x);
          XSETINT (emacs_event->y, y);
          ns_input_file = append2 (ns_input_file,
                                   build_string ([file UTF8String]));
          emacs_event->modifiers = EV_MODIFIERS (theEvent);
          EV_TRAILER (theEvent);
        }
      return YES;
    }
  else if ([type isEqualToString: NSURLPboardType])
    {
      NSString *file;
      NSURL *fileURL;

      if (!(fileURL = [NSURL URLFromPasteboard: pb]) ||
          [fileURL isFileURL] == NO)
        return NO;

      file = [fileURL path];
      emacs_event->kind = NS_NONKEY_EVENT;
      emacs_event->code = KEY_NS_DRAG_FILE;
      XSETINT (emacs_event->x, x);
      XSETINT (emacs_event->y, y);
      ns_input_file = append2 (ns_input_file, build_string ([file UTF8String]));
      emacs_event->modifiers = EV_MODIFIERS (theEvent);
      EV_TRAILER (theEvent);
      return YES;
    }
  else if ([type isEqualToString: NSStringPboardType]
           || [type isEqualToString: NSTabularTextPboardType])
    {
      NSString *data;

      if (! (data = [pb stringForType: type]))
        return NO;

      emacs_event->kind = NS_NONKEY_EVENT;
      emacs_event->code = KEY_NS_DRAG_TEXT;
      XSETINT (emacs_event->x, x);
      XSETINT (emacs_event->y, y);
      ns_input_text = build_string ([data UTF8String]);
      emacs_event->modifiers = EV_MODIFIERS (theEvent);
      EV_TRAILER (theEvent);
      return YES;
    }
  else if ([type isEqualToString: NSColorPboardType])
    {
      NSColor *c = [NSColor colorFromPasteboard: pb];
      emacs_event->kind = NS_NONKEY_EVENT;
      emacs_event->code = KEY_NS_DRAG_COLOR;
      XSETINT (emacs_event->x, x);
      XSETINT (emacs_event->y, y);
      ns_input_color = ns_color_to_lisp (c);
      emacs_event->modifiers = EV_MODIFIERS (theEvent);
      EV_TRAILER (theEvent);
      return YES;
    }
  else if ([type isEqualToString: NSFontPboardType])
    {
      /* impl based on GNUstep NSTextView.m */
      NSData *data = [pb dataForType: NSFontPboardType];
      NSDictionary *dict = [NSUnarchiver unarchiveObjectWithData: data];
      NSFont *font = [dict objectForKey: NSFontAttributeName];
      char fontSize[10];

      if (font == nil)
        return NO;

      emacs_event->kind = NS_NONKEY_EVENT;
      emacs_event->code = KEY_NS_CHANGE_FONT;
      XSETINT (emacs_event->x, x);
      XSETINT (emacs_event->y, y);
      ns_input_font = build_string ([[font fontName] UTF8String]);
      snprintf (fontSize, 10, "%f", [font pointSize]);
      ns_input_fontsize = build_string (fontSize);
      emacs_event->modifiers = EV_MODIFIERS (theEvent);
      EV_TRAILER (theEvent);
      return YES;
    }
  else
    {
      error ("Invalid data type in dragging pasteboard.");
      return NO;
    }
}


- (id) validRequestorForSendType: (NSString *)typeSent
                      returnType: (NSString *)typeReturned
{
  NSTRACE (validRequestorForSendType);
  if (typeSent != nil && [ns_send_types indexOfObject: typeSent] != NSNotFound
      && typeReturned == nil)
    {
      if (! NILP (ns_get_local_selection (QPRIMARY, QUTF8_STRING)))
        return self;
    }

  return [super validRequestorForSendType: typeSent
                               returnType: typeReturned];
}


/* The next two methods are part of NSServicesRequests informal protocol,
   supposedly called when a services menu item is chosen from this app.
   But this should not happen because we override the services menu with our
   own entries which call ns-perform-service.
   Nonetheless, it appeared to happen (under strange circumstances): bug#1435.
   So let's at least stub them out until further investigation can be done. */

- (BOOL) readSelectionFromPasteboard: (NSPasteboard *)pb
{
  /* we could call ns_string_from_pasteboard(pboard) here but then it should
     be written into the buffer in place of the existing selection..
     ordinary service calls go through functions defined in ns-win.el */
  return NO;
}

- (BOOL) writeSelectionToPasteboard: (NSPasteboard *)pb types: (NSArray *)types
{
  NSArray *typesDeclared;
  Lisp_Object val;

  /* We only support NSStringPboardType */
  if ([types containsObject:NSStringPboardType] == NO) {
    return NO;
  }

  val = ns_get_local_selection (QPRIMARY, QUTF8_STRING);
  if (CONSP (val) && SYMBOLP (XCAR (val)))
    {
      val = XCDR (val);
      if (CONSP (val) && NILP (XCDR (val)))
        val = XCAR (val);
    }
  if (! STRINGP (val))
    return NO;

  typesDeclared = [NSArray arrayWithObject:NSStringPboardType];
  [pb declareTypes:typesDeclared owner:nil];
  ns_string_to_pasteboard (pb, val);
  return YES;
}


/* setMini =YES means set from internal (gives a finder icon), NO means set nil
   (gives a miniaturized version of the window); currently we use the latter for
   frames whose active buffer doesn't correspond to any file
   (e.g., '*scratch*') */
- setMiniwindowImage: (BOOL) setMini
{
  id image = [[self window] miniwindowImage];
  NSTRACE (setMiniwindowImage);

  /* NOTE: under Cocoa miniwindowImage always returns nil, documentation
     about "AppleDockIconEnabled" notwithstanding, however the set message
     below has its effect nonetheless. */
  if (image != emacsframe->output_data.ns->miniimage)
    {
      if (image && [image isKindOfClass: [EmacsImage class]])
        [image release];
      [[self window] setMiniwindowImage:
                       setMini ? emacsframe->output_data.ns->miniimage : nil];
    }

  return self;
}


- (void) setRows: (int) r andColumns: (int) c
{
  rows = r;
  cols = c;
}

@end  /* EmacsView */



/* ==========================================================================

    EmacsWindow implementation

   ========================================================================== */

@implementation EmacsWindow

/* If we have multiple monitors, one above the other, we don't want to
   restrict the height to just one monitor.  So we override this.  */
- (NSRect)constrainFrameRect:(NSRect)frameRect toScreen:(NSScreen *)screen
{
  /* When making the frame visible for the first time or if there is just
     one screen, we want to constrain.  Other times not.  */
  NSUInteger nr_screens = [[NSScreen screens] count];
  struct frame *f = ((EmacsView *)[self delegate])->emacsframe;
  NSTRACE (constrainFrameRect);

  if (nr_screens == 1)
    return [super constrainFrameRect:frameRect toScreen:screen];
  
  if (f->output_data.ns->dont_constrain
      || ns_menu_bar_should_be_hidden ())
    return frameRect;

  f->output_data.ns->dont_constrain = 1;
  return [super constrainFrameRect:frameRect toScreen:screen];
}


/* called only on resize clicks by special case in EmacsApp-sendEvent */
- (void)mouseDown: (NSEvent *)theEvent
{
  if (ns_in_resize)
    {
      NSSize size = [[theEvent window] frame].size;
      grabOffset = [theEvent locationInWindow];
      grabOffset.x = size.width - grabOffset.x;
    }
  else
    [super mouseDown: theEvent];
}


/* stop resizing */
- (void)mouseUp: (NSEvent *)theEvent
{
  if (ns_in_resize)
    {
      struct frame *f = ((EmacsView *)[self delegate])->emacsframe;
      ns_in_resize = NO;
      ns_set_name_as_filename (f);
      [self display];
      ns_send_appdefined (-1);
    }
  else
    [super mouseUp: theEvent];
}


/* send resize events */
- (void)mouseDragged: (NSEvent *)theEvent
{
  if (ns_in_resize)
    {
      NSPoint p = [theEvent locationInWindow];
      NSSize size, vettedSize, origSize = [self frame].size;

      size.width = p.x + grabOffset.x;
      size.height = origSize.height - p.y + grabOffset.y;

      if (size.width == origSize.width && size.height == origSize.height)
        return;

      vettedSize = [[self delegate] windowWillResize: self toSize: size];
      [[NSNotificationCenter defaultCenter]
            postNotificationName: NSWindowDidResizeNotification
                          object: self];
    }
  else
    [super mouseDragged: theEvent];
}

@end /* EmacsWindow */


/* ==========================================================================

    EmacsScroller implementation

   ========================================================================== */


@implementation EmacsScroller

/* for repeat button push */
#define SCROLL_BAR_FIRST_DELAY 0.5
#define SCROLL_BAR_CONTINUOUS_DELAY (1.0 / 15)

+ (CGFloat) scrollerWidth
{
  /* TODO: if we want to allow variable widths, this is the place to do it,
           however neither GNUstep nor Cocoa support it very well */
  return [NSScroller scrollerWidth];
}


- initFrame: (NSRect )r window: (Lisp_Object)nwin
{
  NSTRACE (EmacsScroller_initFrame);

  r.size.width = [EmacsScroller scrollerWidth];
  [super initWithFrame: r/*NSMakeRect (0, 0, 0, 0)*/];
  [self setContinuous: YES];
  [self setEnabled: YES];

  /* Ensure auto resizing of scrollbars occurs within the emacs frame's view
     locked against the top and bottom edges, and right edge on OS X, where
     scrollers are on right. */
#ifdef NS_IMPL_GNUSTEP
  [self setAutoresizingMask: NSViewMaxXMargin | NSViewHeightSizable];
#else
  [self setAutoresizingMask: NSViewMinXMargin | NSViewHeightSizable];
#endif

  win = nwin;
  condemned = NO;
  pixel_height = NSHeight (r);
  if (pixel_height == 0) pixel_height = 1;
  min_portion = 20 / pixel_height;

  frame = XFRAME (XWINDOW (win)->frame);
  if (FRAME_LIVE_P (frame))
    {
      int i;
      EmacsView *view = FRAME_NS_VIEW (frame);
      NSView *sview = [[view window] contentView];
      NSArray *subs = [sview subviews];

      /* disable optimization stopping redraw of other scrollbars */
      view->scrollbarsNeedingUpdate = 0;
      for (i =[subs count]-1; i >= 0; i--)
        if ([[subs objectAtIndex: i] isKindOfClass: [EmacsScroller class]])
          view->scrollbarsNeedingUpdate++;
      [sview addSubview: self];
    }

/*  [self setFrame: r]; */

  return self;
}


- (void)setFrame: (NSRect)newRect
{
  NSTRACE (EmacsScroller_setFrame);
/*  BLOCK_INPUT; */
  pixel_height = NSHeight (newRect);
  if (pixel_height == 0) pixel_height = 1;
  min_portion = 20 / pixel_height;
  [super setFrame: newRect];
  [self display];
/*  UNBLOCK_INPUT; */
}


- (void)dealloc
{
  NSTRACE (EmacsScroller_dealloc);
  if (!NILP (win))
    XWINDOW (win)->vertical_scroll_bar = Qnil;
  [super dealloc];
}


- condemn
{
  NSTRACE (condemn);
  condemned =YES;
  return self;
}


- reprieve
{
  NSTRACE (reprieve);
  condemned =NO;
  return self;
}


- judge
{
  NSTRACE (judge);
  if (condemned)
    {
      EmacsView *view;
      BLOCK_INPUT;
      /* ensure other scrollbar updates after deletion */
      view = (EmacsView *)FRAME_NS_VIEW (frame);
      if (view != nil)
        view->scrollbarsNeedingUpdate++;
      [self removeFromSuperview];
      [self release];
      UNBLOCK_INPUT;
    }
  return self;
}


- (void)resetCursorRects
{
  NSRect visible = [self visibleRect];
  NSTRACE (resetCursorRects);

  if (!NSIsEmptyRect (visible))
    [self addCursorRect: visible cursor: [NSCursor arrowCursor]];
  [[NSCursor arrowCursor] setOnMouseEntered: YES];
}


- (int) checkSamePosition: (int) position portion: (int) portion
                    whole: (int) whole
{
  return em_position ==position && em_portion ==portion && em_whole ==whole
    && portion != whole; /* needed for resize empty buf */
}


- setPosition: (int)position portion: (int)portion whole: (int)whole
{
  NSTRACE (setPosition);

  em_position = position;
  em_portion = portion;
  em_whole = whole;

  if (portion >= whole)
    {
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED > MAC_OS_X_VERSION_10_5
      [self setKnobProportion: 1.0];
      [self setDoubleValue: 1.0];
#else
      [self setFloatValue: 0.0 knobProportion: 1.0];
#endif
    }
  else
    {
      float pos, por;
      portion = max ((float)whole*min_portion/pixel_height, portion);
      pos = (float)position / (whole - portion);
      por = (float)portion/whole;
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED > MAC_OS_X_VERSION_10_5
      [self setKnobProportion: por];
      [self setDoubleValue: pos];
#else
      [self setFloatValue: pos knobProportion: por];
#endif
    }
  return self;
}

/* FIXME: unused at moment (see ns_mouse_position) at the moment because
     drag events will go directly to the EmacsScroller.  Leaving in for now. */
-(void)getMouseMotionPart: (int *)part window: (Lisp_Object *)window
                        x: (Lisp_Object *)x y: ( Lisp_Object *)y
{
  *part = last_hit_part;
  *window = win;
  XSETINT (*y, pixel_height);
  if ([self floatValue] > 0.999)
    XSETINT (*x, pixel_height);
  else
    XSETINT (*x, pixel_height * [self floatValue]);
}


/* set up emacs_event */
- (void) sendScrollEventAtLoc: (float)loc fromEvent: (NSEvent *)e
{
  if (!emacs_event)
    return;

  emacs_event->part = last_hit_part;
  emacs_event->code = 0;
  emacs_event->modifiers = EV_MODIFIERS (e) | down_modifier;
  emacs_event->frame_or_window = win;
  emacs_event->timestamp = EV_TIMESTAMP (e);
  emacs_event->kind = SCROLL_BAR_CLICK_EVENT;
  emacs_event->arg = Qnil;
  XSETINT (emacs_event->x, loc * pixel_height);
  XSETINT (emacs_event->y, pixel_height-20);

  n_emacs_events_pending++;
  kbd_buffer_store_event_hold (emacs_event, q_event_ptr);
  EVENT_INIT (*emacs_event);
  ns_send_appdefined (-1);
}


/* called manually thru timer to implement repeated button action w/hold-down */
- repeatScroll: (NSTimer *)scrollEntry
{
  NSEvent *e = [[self window] currentEvent];
  NSPoint p =  [[self window] mouseLocationOutsideOfEventStream];
  BOOL inKnob = [self testPart: p] == NSScrollerKnob;

  /* clear timer if need be */
  if (inKnob || [scroll_repeat_entry timeInterval] == SCROLL_BAR_FIRST_DELAY)
    {
        [scroll_repeat_entry invalidate];
        [scroll_repeat_entry release];
        scroll_repeat_entry = nil;

        if (inKnob)
          return self;

        scroll_repeat_entry
	  = [[NSTimer scheduledTimerWithTimeInterval:
			SCROLL_BAR_CONTINUOUS_DELAY
                                            target: self
                                          selector: @selector (repeatScroll:)
                                          userInfo: 0
                                           repeats: YES]
	      retain];
    }

  [self sendScrollEventAtLoc: 0 fromEvent: e];
  return self;
}


/* Asynchronous mouse tracking for scroller.  This allows us to dispatch
   mouseDragged events without going into a modal loop. */
- (void)mouseDown: (NSEvent *)e
{
  NSRect sr, kr;
  /* hitPart is only updated AFTER event is passed on */
  NSScrollerPart part = [self testPart: [e locationInWindow]];
  double inc = 0.0, loc, kloc, pos;
  int edge = 0;

  NSTRACE (EmacsScroller_mouseDown);

  switch (part)
    {
    case NSScrollerDecrementPage:
        last_hit_part = scroll_bar_above_handle; inc = -1.0; break;
    case NSScrollerIncrementPage:
        last_hit_part = scroll_bar_below_handle; inc = 1.0; break;
    case NSScrollerDecrementLine:
      last_hit_part = scroll_bar_up_arrow; inc = -0.1; break;
    case NSScrollerIncrementLine:
      last_hit_part = scroll_bar_down_arrow; inc = 0.1; break;
    case NSScrollerKnob:
      last_hit_part = scroll_bar_handle; break;
    case NSScrollerKnobSlot:  /* GNUstep-only */
      last_hit_part = scroll_bar_move_ratio; break;
    default:  /* NSScrollerNoPart? */
      fprintf (stderr, "EmacsScoller-mouseDown: unexpected part %ld\n",
               (long) part);
      return;
    }

  if (inc != 0.0)
    {
      pos = 0;      /* ignored */

      /* set a timer to repeat, as we can't let superclass do this modally */
      scroll_repeat_entry
	= [[NSTimer scheduledTimerWithTimeInterval: SCROLL_BAR_FIRST_DELAY
                                            target: self
                                          selector: @selector (repeatScroll:)
                                          userInfo: 0
                                           repeats: YES]
	    retain];
    }
  else
    {
      /* handle, or on GNUstep possibly slot */
      NSEvent *fake_event;

      /* compute float loc in slot and mouse offset on knob */
      sr = [self convertRect: [self rectForPart: NSScrollerKnobSlot]
                      toView: nil];
      loc = NSHeight (sr) - ([e locationInWindow].y - NSMinY (sr));
      if (loc <= 0.0)
        {
          loc = 0.0;
          edge = -1;
        }
      else if (loc >= NSHeight (sr))
        {
          loc = NSHeight (sr);
          edge = 1;
        }

      if (edge)
        kloc = 0.5 * edge;
      else
        {
          kr = [self convertRect: [self rectForPart: NSScrollerKnob]
                          toView: nil];
          kloc = NSHeight (kr) - ([e locationInWindow].y - NSMinY (kr));
        }
      last_mouse_offset = kloc;

      /* if knob, tell emacs a location offset by knob pos
         (to indicate top of handle) */
      if (part == NSScrollerKnob)
          pos = (loc - last_mouse_offset) / NSHeight (sr);
      else
        /* else this is a slot click on GNUstep: go straight there */
        pos = loc / NSHeight (sr);

      /* send a fake mouse-up to super to preempt modal -trackKnob: mode */
      fake_event = [NSEvent mouseEventWithType: NSLeftMouseUp
                                      location: [e locationInWindow]
                                 modifierFlags: [e modifierFlags]
                                     timestamp: [e timestamp]
                                  windowNumber: [e windowNumber]
                                       context: [e context]
                                   eventNumber: [e eventNumber]
                                    clickCount: [e clickCount]
                                      pressure: [e pressure]];
      [super mouseUp: fake_event];
    }

  if (part != NSScrollerKnob)
    [self sendScrollEventAtLoc: pos fromEvent: e];
}


/* Called as we manually track scroller drags, rather than superclass. */
- (void)mouseDragged: (NSEvent *)e
{
    NSRect sr;
    double loc, pos;
    int edge = 0;

    NSTRACE (EmacsScroller_mouseDragged);

      sr = [self convertRect: [self rectForPart: NSScrollerKnobSlot]
                      toView: nil];
      loc = NSHeight (sr) - ([e locationInWindow].y - NSMinY (sr));

      if (loc <= 0.0)
        {
          loc = 0.0;
          edge = -1;
        }
      else if (loc >= NSHeight (sr) + last_mouse_offset)
        {
          loc = NSHeight (sr) + last_mouse_offset;
          edge = 1;
        }

      pos = /*(edge ? loc :*/ (loc - last_mouse_offset) / NSHeight (sr);
      [self sendScrollEventAtLoc: pos fromEvent: e];
}


- (void)mouseUp: (NSEvent *)e
{
  if (scroll_repeat_entry)
    {
      [scroll_repeat_entry invalidate];
      [scroll_repeat_entry release];
      scroll_repeat_entry = nil;
    }
  last_hit_part = 0;
}


/* treat scrollwheel events in the bar as though they were in the main window */
- (void) scrollWheel: (NSEvent *)theEvent
{
  EmacsView *view = (EmacsView *)FRAME_NS_VIEW (frame);
  [view mouseDown: theEvent];
}

@end  /* EmacsScroller */




/* ==========================================================================

   Font-related functions; these used to be in nsfaces.m

   ========================================================================== */


Lisp_Object
x_new_font (struct frame *f, Lisp_Object font_object, int fontset)
{
  struct font *font = XFONT_OBJECT (font_object);

  if (fontset < 0)
    fontset = fontset_from_font (font_object);
  FRAME_FONTSET (f) = fontset;

  if (FRAME_FONT (f) == font)
    /* This font is already set in frame F.  There's nothing more to
       do.  */
    return font_object;

  FRAME_FONT (f) = font;

  FRAME_BASELINE_OFFSET (f) = font->baseline_offset;
  FRAME_COLUMN_WIDTH (f) = font->average_width;
  FRAME_SPACE_WIDTH (f) = font->space_width;
  FRAME_LINE_HEIGHT (f) = font->height;

  compute_fringe_widths (f, 1);

  /* Compute the scroll bar width in character columns.  */
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f)
	= (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + wid - 1) / wid;
    }
  else
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;
    }

  /* Now make the frame display the given font.  */
  if (FRAME_NS_WINDOW (f) != 0)
	x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));

  return font_object;
}


/* XLFD: -foundry-family-weight-slant-swidth-adstyle-pxlsz-ptSz-resx-resy-spc-avgWidth-rgstry-encoding */
/* Note: ns_font_to_xlfd and ns_fontname_to_xlfd no longer needed, removed
         in 1.43. */

const char *
ns_xlfd_to_fontname (const char *xlfd)
/* --------------------------------------------------------------------------
    Convert an X font name (XLFD) to an NS font name.
    Only family is used.
    The string returned is temporarily allocated.
   -------------------------------------------------------------------------- */
{
  char *name = xmalloc (180);
  int i, len;
  const char *ret;

  if (!strncmp (xlfd, "--", 2))
    sscanf (xlfd, "--%*[^-]-%[^-]179-", name);
  else
    sscanf (xlfd, "-%*[^-]-%[^-]179-", name);

  /* stopgap for malformed XLFD input */
  if (strlen (name) == 0)
    strcpy (name, "Monaco");

  /* undo hack in ns_fontname_to_xlfd, converting '$' to '-', '_' to ' '
     also uppercase after '-' or ' ' */
  name[0] = toupper (name[0]);
  for (len =strlen (name), i =0; i<len; i++)
    {
      if (name[i] == '$')
        {
          name[i] = '-';
          if (i+1<len)
            name[i+1] = toupper (name[i+1]);
        }
      else if (name[i] == '_')
        {
          name[i] = ' ';
          if (i+1<len)
            name[i+1] = toupper (name[i+1]);
        }
    }
/*fprintf (stderr, "converted '%s' to '%s'\n",xlfd,name);  */
  ret = [[NSString stringWithUTF8String: name] UTF8String];
  xfree (name);
  return ret;
}


void
syms_of_nsterm (void)
{
  NSTRACE (syms_of_nsterm);

  ns_antialias_threshold = 10.0;

  /* from 23+ we need to tell emacs what modifiers there are.. */
  DEFSYM (Qmodifier_value, "modifier-value");
  DEFSYM (Qalt, "alt");
  DEFSYM (Qhyper, "hyper");
  DEFSYM (Qmeta, "meta");
  DEFSYM (Qsuper, "super");
  DEFSYM (Qcontrol, "control");
  DEFSYM (Qnone, "none");
  DEFSYM (QUTF8_STRING, "UTF8_STRING");

  Fput (Qalt, Qmodifier_value, make_number (alt_modifier));
  Fput (Qhyper, Qmodifier_value, make_number (hyper_modifier));
  Fput (Qmeta, Qmodifier_value, make_number (meta_modifier));
  Fput (Qsuper, Qmodifier_value, make_number (super_modifier));
  Fput (Qcontrol, Qmodifier_value, make_number (ctrl_modifier));

  DEFVAR_LISP ("ns-input-file", ns_input_file,
              "The file specified in the last NS event.");
  ns_input_file =Qnil;

  DEFVAR_LISP ("ns-input-text", ns_input_text,
              "The data received in the last NS text drag event.");
  ns_input_text =Qnil;

  DEFVAR_LISP ("ns-working-text", ns_working_text,
              "String for visualizing working composition sequence.");
  ns_working_text =Qnil;

  DEFVAR_LISP ("ns-input-font", ns_input_font,
              "The font specified in the last NS event.");
  ns_input_font =Qnil;

  DEFVAR_LISP ("ns-input-fontsize", ns_input_fontsize,
              "The fontsize specified in the last NS event.");
  ns_input_fontsize =Qnil;

  DEFVAR_LISP ("ns-input-line", ns_input_line,
               "The line specified in the last NS event.");
  ns_input_line =Qnil;

  DEFVAR_LISP ("ns-input-color", ns_input_color,
               "The color specified in the last NS event.");
  ns_input_color =Qnil;

  DEFVAR_LISP ("ns-input-spi-name", ns_input_spi_name,
               "The service name specified in the last NS event.");
  ns_input_spi_name =Qnil;

  DEFVAR_LISP ("ns-input-spi-arg", ns_input_spi_arg,
               "The service argument specified in the last NS event.");
  ns_input_spi_arg =Qnil;

  DEFVAR_LISP ("ns-alternate-modifier", ns_alternate_modifier,
               "This variable describes the behavior of the alternate or option key.\n\
Set to control, meta, alt, super, or hyper means it is taken to be that key.\n\
Set to none means that the alternate / option key is not interpreted by Emacs\n\
at all, allowing it to be used at a lower level for accented character entry.");
  ns_alternate_modifier = Qmeta;

  DEFVAR_LISP ("ns-right-alternate-modifier", ns_right_alternate_modifier,
               "This variable describes the behavior of the right alternate or option key.\n\
Set to control, meta, alt, super, or hyper means it is taken to be that key.\n\
Set to left means be the same key as `ns-alternate-modifier'.\n\
Set to none means that the alternate / option key is not interpreted by Emacs\n\
at all, allowing it to be used at a lower level for accented character entry.");
  ns_right_alternate_modifier = Qleft;

  DEFVAR_LISP ("ns-command-modifier", ns_command_modifier,
               "This variable describes the behavior of the command key.\n\
Set to control, meta, alt, super, or hyper means it is taken to be that key.");
  ns_command_modifier = Qsuper;

  DEFVAR_LISP ("ns-right-command-modifier", ns_right_command_modifier,
               "This variable describes the behavior of the right command key.\n\
Set to control, meta, alt, super, or hyper means it is taken to be that key.\n\
Set to left means be the same key as `ns-command-modifier'.\n\
Set to none means that the command / option key is not interpreted by Emacs\n\
at all, allowing it to be used at a lower level for accented character entry.");
  ns_right_command_modifier = Qleft;

  DEFVAR_LISP ("ns-control-modifier", ns_control_modifier,
               "This variable describes the behavior of the control key.\n\
Set to control, meta, alt, super, or hyper means it is taken to be that key.");
  ns_control_modifier = Qcontrol;

  DEFVAR_LISP ("ns-right-control-modifier", ns_right_control_modifier,
               "This variable describes the behavior of the right control key.\n\
Set to control, meta, alt, super, or hyper means it is taken to be that key.\n\
Set to left means be the same key as `ns-control-modifier'.\n\
Set to none means that the control / option key is not interpreted by Emacs\n\
at all, allowing it to be used at a lower level for accented character entry.");
  ns_right_control_modifier = Qleft;

  DEFVAR_LISP ("ns-function-modifier", ns_function_modifier,
               "This variable describes the behavior of the function key (on laptops).\n\
Set to control, meta, alt, super, or hyper means it is taken to be that key.\n\
Set to none means that the function key is not interpreted by Emacs at all,\n\
allowing it to be used at a lower level for accented character entry.");
  ns_function_modifier = Qnone;

  DEFVAR_LISP ("ns-antialias-text", ns_antialias_text,
               "Non-nil (the default) means to render text antialiased. Only has an effect on OS X Panther and above.");
  ns_antialias_text = Qt;

  DEFVAR_LISP ("ns-confirm-quit", ns_confirm_quit,
               "Whether to confirm application quit using dialog.");
  ns_confirm_quit = Qnil;

  staticpro (&ns_display_name_list);
  ns_display_name_list = Qnil;

  staticpro (&last_mouse_motion_frame);
  last_mouse_motion_frame = Qnil;

  DEFVAR_LISP ("ns-auto-hide-menu-bar", ns_auto_hide_menu_bar,
               doc: /* Non-nil means that the menu bar is hidden, but appears when the mouse is near.
Only works on OSX 10.6 or later.  */);
  ns_auto_hide_menu_bar = Qnil;

  /* TODO: move to common code */
  DEFVAR_LISP ("x-toolkit-scroll-bars", Vx_toolkit_scroll_bars,
	       doc: /* Which toolkit scroll bars Emacs uses, if any.
A value of nil means Emacs doesn't use toolkit scroll bars.
With the X Window system, the value is a symbol describing the
X toolkit.  Possible values are: gtk, motif, xaw, or xaw3d.
With MS Windows or Nextstep, the value is t.  */);
  Vx_toolkit_scroll_bars = Qt;

  DEFVAR_BOOL ("x-use-underline-position-properties",
	       x_use_underline_position_properties,
     doc: /*Non-nil means make use of UNDERLINE_POSITION font properties.
A value of nil means ignore them.  If you encounter fonts with bogus
UNDERLINE_POSITION font properties, for example 7x13 on XFree prior
to 4.1, set this to nil. */);
  x_use_underline_position_properties = 0;

  DEFVAR_BOOL ("x-underline-at-descent-line",
	       x_underline_at_descent_line,
     doc: /* Non-nil means to draw the underline at the same place as the descent line.
A value of nil means to draw the underline according to the value of the
variable `x-use-underline-position-properties', which is usually at the
baseline level.  The default value is nil.  */);
  x_underline_at_descent_line = 0;

  /* Tell emacs about this window system. */
  Fprovide (intern ("ns"), Qnil);
}
