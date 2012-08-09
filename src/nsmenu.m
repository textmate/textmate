/* NeXT/Open/GNUstep and MacOSX Cocoa menu and toolbar module.
   Copyright (C) 2007-2012 Free Software Foundation, Inc.

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
By Adrian Robert, based on code from original nsmenu.m (Carl Edman,
Christian Limpach, Scott Bender, Christophe de Dinechin) and code in the
Carbon version by Yamamoto Mitsuharu. */

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
#include <config.h>
#include <setjmp.h>

#include "lisp.h"
#include "window.h"
#include "buffer.h"
#include "keymap.h"
#include "coding.h"
#include "commands.h"
#include "blockinput.h"
#include "nsterm.h"
#include "termhooks.h"
#include "keyboard.h"
#include "menu.h"

#define NSMENUPROFILE 0

#if NSMENUPROFILE
#include <sys/timeb.h>
#include <sys/types.h>
#endif

#define MenuStagger 10.0

#if 0
int menu_trace_num = 0;
#define NSTRACE(x)        fprintf (stderr, "%s:%d: [%d] " #x "\n",        \
                                __FILE__, __LINE__, ++menu_trace_num)
#else
#define NSTRACE(x)
#endif

#if 0
/* Include lisp -> C common menu parsing code */
#define ENCODE_MENU_STRING(str) ENCODE_UTF_8 (str)
#include "nsmenu_common.c"
#endif

extern Lisp_Object Qundefined, Qmenu_enable, Qmenu_bar_update_hook;
extern Lisp_Object QCtoggle, QCradio;

Lisp_Object Qdebug_on_next_call;
extern Lisp_Object Qoverriding_local_map, Qoverriding_terminal_local_map;

extern long context_menu_value;
EmacsMenu *mainMenu, *svcsMenu, *dockMenu;

/* Nonzero means a menu is currently active.  */
static int popup_activated_flag;
static NSModalSession popupSession;

/* Nonzero means we are tracking and updating menus.  */
static int trackingMenu;


/* NOTE: toolbar implementation is at end,
  following complete menu implementation. */


/* ==========================================================================

    Menu: Externally-called functions

   ========================================================================== */


/* FIXME: not currently used, but should normalize with other terms. */
void
x_activate_menubar (struct frame *f)
{
    fprintf (stderr, "XXX: Received x_activate_menubar event.\n");
}


/* Supposed to discard menubar and free storage.  Since we share the
   menubar among frames and update its context for the focused window,
   there is nothing to do here. */
void
free_frame_menubar (struct frame *f)
{
  return;
}


int
popup_activated (void)
{
  return popup_activated_flag;
}


/* --------------------------------------------------------------------------
    Update menubar.  Three cases:
    1) deep_p = 0, submenu = nil: Fresh switch onto a frame -- either set up
       just top-level menu strings (OS X), or goto case (2) (GNUstep).
    2) deep_p = 1, submenu = nil: Recompute all submenus.
    3) deep_p = 1, submenu = non-nil: Update contents of a single submenu.
   -------------------------------------------------------------------------- */
void
ns_update_menubar (struct frame *f, int deep_p, EmacsMenu *submenu)
{
  NSAutoreleasePool *pool;
  id menu = [NSApp mainMenu];
  static EmacsMenu *last_submenu = nil;
  BOOL needsSet = NO;
  const char *submenuTitle = [[submenu title] UTF8String];
  extern int waiting_for_input;
  int owfi;
  Lisp_Object items;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i;

#if NSMENUPROFILE
  struct timeb tb;
  long t;
#endif

  NSTRACE (set_frame_menubar);

  if (f != SELECTED_FRAME ())
      return;
  XSETFRAME (Vmenu_updating_frame, f);
/*fprintf (stderr, "ns_update_menubar: frame: %p\tdeep: %d\tsub: %p\n", f, deep_p, submenu); */

  BLOCK_INPUT;
  pool = [[NSAutoreleasePool alloc] init];

  /* Menu may have been created automatically; if so, discard it. */
  if ([menu isKindOfClass: [EmacsMenu class]] == NO)
    {
      [menu release];
      menu = nil;
    }

  if (menu == nil)
    {
      menu = [[EmacsMenu alloc] initWithTitle: ns_app_name];
      needsSet = YES;
    }
  else
    {  /* close up anything on there */
      id attMenu = [menu attachedMenu];
      if (attMenu != nil)
        [attMenu close];
    }

#if NSMENUPROFILE
  ftime (&tb);
  t = -(1000*tb.time+tb.millitm);
#endif

#ifdef NS_IMPL_GNUSTEP
  deep_p = 1; /* until GNUstep NSMenu implements the Panther delegation model */
#endif

  if (deep_p)
    {
      /* Fully parse one or more of the submenus. */
      int n = 0;
      int *submenu_start, *submenu_end;
      int *submenu_top_level_items, *submenu_n_panes;
      struct buffer *prev = current_buffer;
      Lisp_Object buffer;
      int specpdl_count = SPECPDL_INDEX ();
      int previous_menu_items_used = f->menu_bar_items_used;
      Lisp_Object *previous_items
	= (Lisp_Object *) alloca (previous_menu_items_used
				  * sizeof (Lisp_Object));

      /* lisp preliminaries */
      buffer = XWINDOW (FRAME_SELECTED_WINDOW (f))->buffer;
      specbind (Qinhibit_quit, Qt);
      specbind (Qdebug_on_next_call, Qnil);
      record_unwind_save_match_data ();
      if (NILP (Voverriding_local_map_menu_flag))
	{
	  specbind (Qoverriding_terminal_local_map, Qnil);
	  specbind (Qoverriding_local_map, Qnil);
	}
      set_buffer_internal_1 (XBUFFER (buffer));

      /* TODO: for some reason this is not needed in other terms,
           but some menu updates call Info-extract-pointer which causes
           abort-on-error if waiting-for-input.  Needs further investigation. */
      owfi = waiting_for_input;
      waiting_for_input = 0;

      /* lucid hook and possible reset */
      safe_run_hooks (Qactivate_menubar_hook);
      if (! NILP (Vlucid_menu_bar_dirty_flag))
	call0 (Qrecompute_lucid_menubar);
      safe_run_hooks (Qmenu_bar_update_hook);
      FRAME_MENU_BAR_ITEMS (f) = menu_bar_items (FRAME_MENU_BAR_ITEMS (f));

      /* Now ready to go */
      items = FRAME_MENU_BAR_ITEMS (f);

      /* Save the frame's previous menu bar contents data */
      if (previous_menu_items_used)
	memcpy (previous_items, &AREF (f->menu_bar_vector, 0),
		previous_menu_items_used * sizeof (Lisp_Object));

      /* parse stage 1: extract from lisp */
      save_menu_items ();

      menu_items = f->menu_bar_vector;
      menu_items_allocated = VECTORP (menu_items) ? ASIZE (menu_items) : 0;
      submenu_start = (int *) alloca (ASIZE (items) * sizeof (int *));
      submenu_end = (int *) alloca (ASIZE (items) * sizeof (int *));
      submenu_n_panes = (int *) alloca (ASIZE (items) * sizeof (int));
      submenu_top_level_items
	= (int *) alloca (ASIZE (items) * sizeof (int *));
      init_menu_items ();
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object key, string, maps;

	  key = AREF (items, i);
	  string = AREF (items, i + 1);
	  maps = AREF (items, i + 2);
	  if (NILP (string))
	    break;

          /* FIXME: we'd like to only parse the needed submenu, but this
               was causing crashes in the _common parsing code.. need to make
               sure proper initialization done.. */
/*        if (submenu && strcmp (submenuTitle, SDATA (string)))
             continue; */

	  submenu_start[i] = menu_items_used;

	  menu_items_n_panes = 0;
	  submenu_top_level_items[i] = parse_single_submenu (key, string, maps);
	  submenu_n_panes[i] = menu_items_n_panes;
	  submenu_end[i] = menu_items_used;
          n++;
	}

      finish_menu_items ();
      waiting_for_input = owfi;


      if (submenu && n == 0)
        {
          /* should have found a menu for this one but didn't */
          fprintf (stderr, "ERROR: did not find lisp menu for submenu '%s'.\n",
                  submenuTitle);
	  discard_menu_items ();
	  unbind_to (specpdl_count, Qnil);
          [pool release];
          UNBLOCK_INPUT;
	  return;
        }

      /* parse stage 2: insert into lucid 'widget_value' structures
         [comments in other terms say not to evaluate lisp code here] */
      wv = xmalloc_widget_value ();
      wv->name = "menubar";
      wv->value = 0;
      wv->enabled = 1;
      wv->button_type = BUTTON_TYPE_NONE;
      wv->help = Qnil;
      first_wv = wv;

      for (i = 0; i < 4*n; i += 4)
	{
	  menu_items_n_panes = submenu_n_panes[i];
	  wv = digest_single_submenu (submenu_start[i], submenu_end[i],
				      submenu_top_level_items[i]);
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    first_wv->contents = wv;
	  /* Don't set wv->name here; GC during the loop might relocate it.  */
	  wv->enabled = 1;
	  wv->button_type = BUTTON_TYPE_NONE;
	  prev_wv = wv;
	}

      set_buffer_internal_1 (prev);

      /* Compare the new menu items with previous, and leave off if no change */
      /* FIXME: following other terms here, but seems like this should be
           done before parse stage 2 above, since its results aren't used */
      if (previous_menu_items_used
          && (!submenu || (submenu && submenu == last_submenu))
          && menu_items_used == previous_menu_items_used)
        {
          for (i = 0; i < previous_menu_items_used; i++)
            /* FIXME: this ALWAYS fails on Buffers menu items.. something
                 about their strings causes them to change every time, so we
                 double-check failures */
            if (!EQ (previous_items[i], AREF (menu_items, i)))
              if (!(STRINGP (previous_items[i])
                    && STRINGP (AREF (menu_items, i))
                    && !strcmp (SDATA (previous_items[i]),
				SDATA (AREF (menu_items, i)))))
                  break;
          if (i == previous_menu_items_used)
            {
              /* No change.. */

#if NSMENUPROFILE
              ftime (&tb);
              t += 1000*tb.time+tb.millitm;
              fprintf (stderr, "NO CHANGE!  CUTTING OUT after %ld msec.\n", t);
#endif

              free_menubar_widget_value_tree (first_wv);
              discard_menu_items ();
              unbind_to (specpdl_count, Qnil);
              [pool release];
              UNBLOCK_INPUT;
              return;
            }
        }
      /* The menu items are different, so store them in the frame */
      /* FIXME: this is not correct for single-submenu case */
      f->menu_bar_vector = menu_items;
      f->menu_bar_items_used = menu_items_used;

      /* Calls restore_menu_items, etc., as they were outside */
      unbind_to (specpdl_count, Qnil);

      /* Parse stage 2a: now GC cannot happen during the lifetime of the
         widget_value, so it's safe to store data from a Lisp_String */
      wv = first_wv->contents;
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object string;
	  string = AREF (items, i + 1);
	  if (NILP (string))
	    break;
/*           if (submenu && strcmp (submenuTitle, SDATA (string)))
               continue; */

	  wv->name = SSDATA (string);
          update_submenu_strings (wv->contents);
	  wv = wv->next;
	}

      /* Now, update the NS menu; if we have a submenu, use that, otherwise
         create a new menu for each sub and fill it. */
      if (submenu)
        {
          for (wv = first_wv->contents; wv; wv = wv->next)
            {
              if (!strcmp (submenuTitle, wv->name))
                {
                  [submenu fillWithWidgetValue: wv->contents];
                  last_submenu = submenu;
                  break;
                }
            }
        }
      else
        {
          [menu fillWithWidgetValue: first_wv->contents];
        }

    }
  else
    {
      static int n_previous_strings = 0;
      static char previous_strings[100][10];
      static struct frame *last_f = NULL;
      int n;
      Lisp_Object string;

      wv = xmalloc_widget_value ();
      wv->name = "menubar";
      wv->value = 0;
      wv->enabled = 1;
      wv->button_type = BUTTON_TYPE_NONE;
      wv->help = Qnil;
      first_wv = wv;

      /* Make widget-value tree w/ just the top level menu bar strings */
      items = FRAME_MENU_BAR_ITEMS (f);
      if (NILP (items))
        {
          free_menubar_widget_value_tree (first_wv);
          [pool release];
          UNBLOCK_INPUT;
          return;
        }


      /* check if no change.. this mechanism is a bit rough, but ready */
      n = ASIZE (items) / 4;
      if (f == last_f && n_previous_strings == n)
        {
          for (i = 0; i<n; i++)
            {
	      string = AREF (items, 4*i+1);

              if (EQ (string, make_number (0))) // FIXME: Why???  --Stef
                continue;
              if (NILP (string))
                if (previous_strings[i][0])
                  break;
              else
                continue;
              if (strncmp (previous_strings[i], SDATA (string), 10))
                break;
            }

          if (i == n)
            {
              free_menubar_widget_value_tree (first_wv);
              [pool release];
              UNBLOCK_INPUT;
              return;
            }
        }

      [menu clear];
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  string = AREF (items, i + 1);
	  if (NILP (string))
	    break;

          if (n < 100)
            strncpy (previous_strings[i/4], SDATA (string), 10);

	  wv = xmalloc_widget_value ();
	  wv->name = SSDATA (string);
	  wv->value = 0;
	  wv->enabled = 1;
	  wv->button_type = BUTTON_TYPE_NONE;
	  wv->help = Qnil;
	  wv->call_data = (void *) (EMACS_INT) (-1);

#ifdef NS_IMPL_COCOA
          /* we'll update the real copy under app menu when time comes */
          if (!strcmp ("Services", wv->name))
            {
              /* but we need to make sure it will update on demand */
              [svcsMenu setFrame: f];
            }
          else
#endif
          [menu addSubmenuWithTitle: wv->name forFrame: f];

	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    first_wv->contents = wv;
	  prev_wv = wv;
	}

      last_f = f;
      if (n < 100)
        n_previous_strings = n;
      else
        n_previous_strings = 0;

    }
  free_menubar_widget_value_tree (first_wv);


#if NSMENUPROFILE
  ftime (&tb);
  t += 1000*tb.time+tb.millitm;
  fprintf (stderr, "Menu update took %ld msec.\n", t);
#endif

  /* set main menu */
  if (needsSet)
    [NSApp setMainMenu: menu];

  [pool release];
  UNBLOCK_INPUT;

}


/* Main emacs core entry point for menubar menus: called to indicate that the
   frame's menus have changed, and the *step representation should be updated
   from Lisp. */
void
set_frame_menubar (struct frame *f, int first_time, int deep_p)
{
  ns_update_menubar (f, deep_p, nil);
}


/* ==========================================================================

    Menu: class implementation

   ========================================================================== */


/* Menu that can define itself from Emacs "widget_value"s and will lazily
   update itself when user clicked.  Based on Carbon/AppKit implementation
   by Yamamoto Mitsuharu. */
@implementation EmacsMenu

/* override designated initializer */
- initWithTitle: (NSString *)title
{
  if (self = [super initWithTitle: title])
    [self setAutoenablesItems: NO];
  return self;
}


/* used for top-level */
- initWithTitle: (NSString *)title frame: (struct frame *)f
{
  [self initWithTitle: title];
  frame = f;
#ifdef NS_IMPL_COCOA
  [self setDelegate: self];
#endif
  return self;
}


- (void)setFrame: (struct frame *)f
{
  frame = f;
}

#ifdef NS_IMPL_COCOA
#if MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_5
extern NSString *NSMenuDidBeginTrackingNotification;
#endif
#endif

#ifdef NS_IMPL_COCOA
-(void)trackingNotification:(NSNotification *)notification
{
  /* Update menu in menuNeedsUpdate only while tracking menus.  */
  trackingMenu = ([notification name] == NSMenuDidBeginTrackingNotification
                  ? 1 : 0);
}
#endif

/* delegate method called when a submenu is being opened: run a 'deep' call
   to set_frame_menubar */
- (void)menuNeedsUpdate: (NSMenu *)menu
{
  if (!FRAME_LIVE_P (frame))
    return;

  /* Cocoa/Carbon will request update on every keystroke
     via IsMenuKeyEvent -> CheckMenusForKeyEvent.  These are not needed
     since key equivalents are handled through emacs.
     On Leopard, even keystroke events generate SystemDefined event.
     Third-party applications that enhance mouse / trackpad
     interaction, or also VNC/Remote Desktop will send events
     of type AppDefined rather than SysDefined.
     Menus will fail to show up if they haven't been initialized.
     AppDefined events may lack timing data.

     Thus, we rely on the didBeginTrackingNotification notification
     as above to indicate the need for updates.
     From 10.6 on, we could also use -[NSMenu propertiesToUpdate]: In the
     key press case, NSMenuPropertyItemImage (e.g.) won't be set.
  */
  if (trackingMenu == 0
      /* Also, don't try this if from an event picked up asynchronously,
         as lots of lisp evaluation happens in ns_update_menubar. */
      || handling_signal != 0)
    return;
/*fprintf (stderr, "Updating menu '%s'\n", [[self title] UTF8String]); NSLog (@"%@\n", event); */
  ns_update_menubar (frame, 1, self);
}


- (BOOL)performKeyEquivalent: (NSEvent *)theEvent
{
  if (SELECTED_FRAME () && FRAME_NS_P (SELECTED_FRAME ())
      && FRAME_NS_VIEW (SELECTED_FRAME ()))
    [FRAME_NS_VIEW (SELECTED_FRAME ()) keyDown: theEvent];
  return YES;
}


/* Parse a widget_value's key rep (examples: 's-p', 's-S', '(C-x C-s)', '<f13>')
   into an accelerator string.  We are only able to display a single character
   for an accelerator, together with an optional modifier combination.  (Under
   Carbon more control was possible, but in Cocoa multi-char strings passed to
   NSMenuItem get ignored.  For now we try to display a super-single letter
   combo, and return the others as strings to be appended to the item title.
   (This is signaled by setting keyEquivModMask to 0 for now.) */
-(NSString *)parseKeyEquiv: (const char *)key
{
  const char *tpos = key;
  keyEquivModMask = NSCommandKeyMask;

  if (!key || !strlen (key))
    return @"";

  while (*tpos == ' ' || *tpos == '(')
    tpos++;
  if ((*tpos == 's') && (*(tpos+1) == '-'))
    {
      return [NSString stringWithFormat: @"%c", tpos[2]];
    }
  keyEquivModMask = 0; /* signal */
  return [NSString stringWithUTF8String: tpos];
}


- (NSMenuItem *)addItemWithWidgetValue: (void *)wvptr
{
  NSMenuItem *item;
  widget_value *wv = (widget_value *)wvptr;

  if (menu_separator_name_p (wv->name))
    {
      item = [NSMenuItem separatorItem];
      [self addItem: item];
    }
  else
    {
      NSString *title, *keyEq;
      title = [NSString stringWithUTF8String: wv->name];
      if (title == nil)
        title = @"< ? >";  /* (get out in the open so we know about it) */

      keyEq = [self parseKeyEquiv: wv->key];
#ifdef NS_IMPL_COCOA
      /* OS X just ignores modifier strings longer than one character */
      if (keyEquivModMask == 0)
        title = [title stringByAppendingFormat: @" (%@)", keyEq];
#endif

      item = [self addItemWithTitle: (NSString *)title
                             action: @selector (menuDown:)
                      keyEquivalent: keyEq];
      [item setKeyEquivalentModifierMask: keyEquivModMask];

      [item setEnabled: wv->enabled];

      /* Draw radio buttons and tickboxes */
      if (wv->selected && (wv->button_type == BUTTON_TYPE_TOGGLE ||
                           wv->button_type == BUTTON_TYPE_RADIO))
        [item setState: NSOnState];
      else
        [item setState: NSOffState];

      [item setTag: (NSInteger)wv->call_data];
    }

  return item;
}


/* convenience */
-(void)clear
{
  int n;

  for (n = [self numberOfItems]-1; n >= 0; n--)
    {
      NSMenuItem *item = [self itemAtIndex: n];
      NSString *title = [item title];
      if (([title length] == 0  /* OSX 10.5 */
	   || [ns_app_name isEqualToString: title]  /* from 10.6 on */
	   || [@"Apple" isEqualToString: title]) /* older */
          && ![item isSeparatorItem])
        continue;
      [self removeItemAtIndex: n];
    }
}


- (void)fillWithWidgetValue: (void *)wvptr
{
  widget_value *wv = (widget_value *)wvptr;

  /* clear existing contents */
  [self setMenuChangedMessagesEnabled: NO];
  [self clear];

  /* add new contents */
  for (; wv != NULL; wv = wv->next)
    {
      NSMenuItem *item = [self addItemWithWidgetValue: wv];

      if (wv->contents)
        {
          EmacsMenu *submenu = [[EmacsMenu alloc] initWithTitle: [item title]];

          [self setSubmenu: submenu forItem: item];
          [submenu fillWithWidgetValue: wv->contents];
          [submenu release];
          [item setAction: nil];
        }
    }

  [self setMenuChangedMessagesEnabled: YES];
#ifdef NS_IMPL_GNUSTEP
  if ([[self window] isVisible])
    [self sizeToFit];
#else
#if MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_2
  if ([self supermenu] == nil)
    [self sizeToFit];
#endif
#endif
}


/* adds an empty submenu and returns it */
- (EmacsMenu *)addSubmenuWithTitle: (const char *)title forFrame: (struct frame *)f
{
  NSString *titleStr = [NSString stringWithUTF8String: title];
  NSMenuItem *item = [self addItemWithTitle: titleStr
                                     action: nil /*@selector (menuDown:) */
                              keyEquivalent: @""];
  EmacsMenu *submenu = [[EmacsMenu alloc] initWithTitle: titleStr frame: f];
  [self setSubmenu: submenu forItem: item];
  [submenu release];
  return submenu;
}

/* run a menu in popup mode */
- (Lisp_Object)runMenuAt: (NSPoint)p forFrame: (struct frame *)f
                 keymaps: (int)keymaps
{
  EmacsView *view = FRAME_NS_VIEW (f);
  NSEvent *e, *event;
  long retVal;

/*   p = [view convertPoint:p fromView: nil]; */
  p.y = NSHeight ([view frame]) - p.y;
  e = [[view window] currentEvent];
   event = [NSEvent mouseEventWithType: NSRightMouseDown
                              location: p
                         modifierFlags: 0
                             timestamp: [e timestamp]
                          windowNumber: [[view window] windowNumber]
                               context: [e context]
                           eventNumber: 0/*[e eventNumber] */
                            clickCount: 1
                              pressure: 0];

  context_menu_value = -1;
  [NSMenu popUpContextMenu: self withEvent: event forView: view];
  retVal = context_menu_value;
  context_menu_value = 0;
  return retVal > 0
      ? find_and_return_menu_selection (f, keymaps, (void *)retVal)
      : Qnil;
}

@end  /* EmacsMenu */



/* ==========================================================================

    Context Menu: implementing functions

   ========================================================================== */

Lisp_Object
ns_menu_show (FRAME_PTR f, int x, int y, int for_click, int keymaps,
	      Lisp_Object title, const char **error)
{
  EmacsMenu *pmenu;
  NSPoint p;
  Lisp_Object window, tem, keymap;
  int specpdl_count = SPECPDL_INDEX ();
  widget_value *wv, *first_wv = 0;

  p.x = x; p.y = y;

  /* now parse stage 2 as in ns_update_menubar */
  wv = xmalloc_widget_value ();
  wv->name = "contextmenu";
  wv->value = 0;
  wv->enabled = 1;
  wv->button_type = BUTTON_TYPE_NONE;
  wv->help = Qnil;
  first_wv = wv;

#if 0
  /* FIXME: a couple of one-line differences prevent reuse */
  wv = digest_single_submenu (0, menu_items_used, Qnil);
#else
  {
  widget_value *save_wv = 0, *prev_wv = 0;
  widget_value **submenu_stack
    = (widget_value **) alloca (menu_items_used * sizeof (widget_value *));
/*   Lisp_Object *subprefix_stack
       = (Lisp_Object *) alloca (menu_items_used * sizeof (Lisp_Object)); */
  int submenu_depth = 0;
  int first_pane = 1;
  int i;

  /* Loop over all panes and items, filling in the tree.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (EQ (AREF (menu_items, i), Qnil))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  first_pane = 1;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qlambda))
	{
	  prev_wv = save_wv;
	  save_wv = submenu_stack[--submenu_depth];
	  first_pane = 0;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (AREF (menu_items, i), Qquote))
	i += 1;
      else if (EQ (AREF (menu_items, i), Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  const char *pane_string;

	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);
	  prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);

#ifndef HAVE_MULTILINGUAL_MENU
	  if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
	    {
	      pane_name = ENCODE_MENU_STRING (pane_name);
	      ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
	    }
#endif
	  pane_string = (NILP (pane_name)
			 ? "" : SSDATA (pane_name));
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (!keymaps && strcmp (pane_string, ""))
	    {
	      wv = xmalloc_widget_value ();
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      wv->name = pane_string;
	      if (keymaps && !NILP (prefix))
		wv->name++;
	      wv->value = 0;
	      wv->enabled = 1;
	      wv->button_type = BUTTON_TYPE_NONE;
	      wv->help = Qnil;
	      save_wv = wv;
	      prev_wv = 0;
	    }
	  else if (first_pane)
	    {
	      save_wv = wv;
	      prev_wv = 0;
	    }
	  first_pane = 0;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, def, type, selected, help;
	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
	  def = AREF (menu_items, i + MENU_ITEMS_ITEM_DEFINITION);
	  type = AREF (menu_items, i + MENU_ITEMS_ITEM_TYPE);
	  selected = AREF (menu_items, i + MENU_ITEMS_ITEM_SELECTED);
	  help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);

#ifndef HAVE_MULTILINGUAL_MENU
          if (STRINGP (item_name) && STRING_MULTIBYTE (item_name))
	    {
	      item_name = ENCODE_MENU_STRING (item_name);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
	    }

          if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
	    {
	      descrip = ENCODE_MENU_STRING (descrip);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
	    }
#endif /* not HAVE_MULTILINGUAL_MENU */

	  wv = xmalloc_widget_value ();
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;
	  wv->name = SSDATA (item_name);
	  if (!NILP (descrip))
	    wv->key = SSDATA (descrip);
	  wv->value = 0;
	  /* If this item has a null value,
	     make the call_data null so that it won't display a box
	     when the mouse is on it.  */
	  wv->call_data
	      = !NILP (def) ? (void *) &AREF (menu_items, i) : 0;
	  wv->enabled = !NILP (enable);

	  if (NILP (type))
	    wv->button_type = BUTTON_TYPE_NONE;
	  else if (EQ (type, QCtoggle))
	    wv->button_type = BUTTON_TYPE_TOGGLE;
	  else if (EQ (type, QCradio))
	    wv->button_type = BUTTON_TYPE_RADIO;
	  else
	    abort ();

	  wv->selected = !NILP (selected);

          if (! STRINGP (help))
	    help = Qnil;

	  wv->help = help;

	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }
  }
#endif

  if (!NILP (title))
    {
      widget_value *wv_title = xmalloc_widget_value ();
      widget_value *wv_sep = xmalloc_widget_value ();

      /* Maybe replace this separator with a bitmap or owner-draw item
	 so that it looks better.  Having two separators looks odd.  */
      wv_sep->name = "--";
      wv_sep->next = first_wv->contents;
      wv_sep->help = Qnil;

#ifndef HAVE_MULTILINGUAL_MENU
      if (STRING_MULTIBYTE (title))
	title = ENCODE_MENU_STRING (title);
#endif

      wv_title->name = SSDATA (title);
      wv_title->enabled = NO;
      wv_title->button_type = BUTTON_TYPE_NONE;
      wv_title->help = Qnil;
      wv_title->next = wv_sep;
      first_wv->contents = wv_title;
    }

  pmenu = [[EmacsMenu alloc] initWithTitle:
                               [NSString stringWithUTF8String: SDATA (title)]];
  [pmenu fillWithWidgetValue: first_wv->contents];
  free_menubar_widget_value_tree (first_wv);
  unbind_to (specpdl_count, Qnil);

  popup_activated_flag = 1;
  tem = [pmenu runMenuAt: p forFrame: f keymaps: keymaps];
  popup_activated_flag = 0;
  [[FRAME_NS_VIEW (SELECTED_FRAME ()) window] makeKeyWindow];

  return tem;
}


/* ==========================================================================

    Toolbar: externally-called functions

   ========================================================================== */

void
free_frame_tool_bar (FRAME_PTR f)
/* --------------------------------------------------------------------------
    Under NS we just hide the toolbar until it might be needed again.
   -------------------------------------------------------------------------- */
{
  BLOCK_INPUT;
  [[FRAME_NS_VIEW (f) toolbar] setVisible: NO];
  FRAME_TOOLBAR_HEIGHT (f) = 0;
  UNBLOCK_INPUT;
}

void
update_frame_tool_bar (FRAME_PTR f)
/* --------------------------------------------------------------------------
    Update toolbar contents
   -------------------------------------------------------------------------- */
{
  int i;
  EmacsView *view = FRAME_NS_VIEW (f);
  NSWindow *window = [view window];
  EmacsToolbar *toolbar = [view toolbar];

  BLOCK_INPUT;
  [toolbar clearActive];

  /* update EmacsToolbar as in GtkUtils, build items list */
  for (i = 0; i < f->n_tool_bar_items; ++i)
    {
#define TOOLPROP(IDX) AREF (f->tool_bar_items, \
                            i * TOOL_BAR_ITEM_NSLOTS + (IDX))

      BOOL enabled_p = !NILP (TOOLPROP (TOOL_BAR_ITEM_ENABLED_P));
      BOOL selected_p = !NILP (TOOLPROP (TOOL_BAR_ITEM_SELECTED_P));
      int idx;
      ptrdiff_t img_id;
      struct image *img;
      Lisp_Object image;
      Lisp_Object helpObj;
      const char *helpText;

      /* If image is a vector, choose the image according to the
	 button state.  */
      image = TOOLPROP (TOOL_BAR_ITEM_IMAGES);
      if (VECTORP (image))
	{
          /* NS toolbar auto-computes disabled and selected images */
          idx = TOOL_BAR_IMAGE_ENABLED_SELECTED;
	  xassert (ASIZE (image) >= idx);
	  image = AREF (image, idx);
	}
      else
        {
          idx = -1;
        }
      helpObj = TOOLPROP (TOOL_BAR_ITEM_HELP);
      if (NILP (helpObj))
        helpObj = TOOLPROP (TOOL_BAR_ITEM_CAPTION);
      helpText = NILP (helpObj) ? "" : SSDATA (helpObj);

      /* Ignore invalid image specifications.  */
      if (!valid_image_p (image))
        {
          /* Don't log anything, GNUS makes invalid images all the time.  */
          continue;
        }

      img_id = lookup_image (f, image);
      img = IMAGE_FROM_ID (f, img_id);
      prepare_image_for_display (f, img);

      if (img->load_failed_p || img->pixmap == nil)
        {
          NSLog (@"Could not prepare toolbar image for display.");
          continue;
        }

      [toolbar addDisplayItemWithImage: img->pixmap idx: i helpText: helpText
                               enabled: enabled_p];
#undef TOOLPROP
    }

  if (![toolbar isVisible])
      [toolbar setVisible: YES];

  if ([toolbar changed])
    {
      /* inform app that toolbar has changed */
      NSDictionary *dict = [toolbar configurationDictionary];
      NSMutableDictionary *newDict = [dict mutableCopy];
      NSEnumerator *keys = [[dict allKeys] objectEnumerator];
      NSObject *key;
      while ((key = [keys nextObject]) != nil)
        {
          NSObject *val = [dict objectForKey: key];
          if ([val isKindOfClass: [NSArray class]])
            {
              [newDict setObject:
                         [toolbar toolbarDefaultItemIdentifiers: toolbar]
                          forKey: key];
              break;
            }
        }
      [toolbar setConfigurationFromDictionary: newDict];
      [newDict release];
    }

  FRAME_TOOLBAR_HEIGHT (f) =
    NSHeight ([window frameRectForContentRect: NSMakeRect (0, 0, 0, 0)])
    - FRAME_NS_TITLEBAR_HEIGHT (f);
  UNBLOCK_INPUT;
}


/* ==========================================================================

    Toolbar: class implementation

   ========================================================================== */

@implementation EmacsToolbar

- initForView: (EmacsView *)view withIdentifier: (NSString *)identifier
{
  self = [super initWithIdentifier: identifier];
  emacsView = view;
  [self setDisplayMode: NSToolbarDisplayModeIconOnly];
  [self setSizeMode: NSToolbarSizeModeSmall];
  [self setDelegate: self];
  identifierToItem = [[NSMutableDictionary alloc] initWithCapacity: 10];
  activeIdentifiers = [[NSMutableArray alloc] initWithCapacity: 8];
  prevEnablement = enablement = 0L;
  return self;
}

- (void)dealloc
{
  [prevIdentifiers release];
  [activeIdentifiers release];
  [identifierToItem release];
  [super dealloc];
}

- (void) clearActive
{
  [prevIdentifiers release];
  prevIdentifiers = [activeIdentifiers copy];
  [activeIdentifiers removeAllObjects];
  prevEnablement = enablement;
  enablement = 0L;
}

- (BOOL) changed
{
  return [activeIdentifiers isEqualToArray: prevIdentifiers] &&
    enablement == prevEnablement ? NO : YES;
}

- (void) addDisplayItemWithImage: (EmacsImage *)img idx: (int)idx
                        helpText: (const char *)help enabled: (BOOL)enabled
{
  /* 1) come up w/identifier */
  NSString *identifier
      = [NSString stringWithFormat: @"%u", [img hash]];

  /* 2) create / reuse item */
  NSToolbarItem *item = [identifierToItem objectForKey: identifier];
  if (item == nil)
    {
      item = [[[NSToolbarItem alloc] initWithItemIdentifier: identifier]
               autorelease];
      [item setImage: img];
      [item setToolTip: [NSString stringWithUTF8String: help]];
      [item setTarget: emacsView];
      [item setAction: @selector (toolbarClicked:)];
    }

  [item setTag: idx];
  [item setEnabled: enabled];

  /* 3) update state */
  [identifierToItem setObject: item forKey: identifier];
  [activeIdentifiers addObject: identifier];
  enablement = (enablement << 1) | (enabled == YES);
}

/* This overrides super's implementation, which automatically sets
   all items to enabled state (for some reason). */
- (void)validateVisibleItems { }


/* delegate methods */

- (NSToolbarItem *)toolbar: (NSToolbar *)toolbar
      itemForItemIdentifier: (NSString *)itemIdentifier
  willBeInsertedIntoToolbar: (BOOL)flag
{
  /* look up NSToolbarItem by identifier and return... */
  return [identifierToItem objectForKey: itemIdentifier];
}

- (NSArray *)toolbarDefaultItemIdentifiers: (NSToolbar *)toolbar
{
  /* return entire set.. */
  return activeIdentifiers;
}

/* for configuration palette (not yet supported) */
- (NSArray *)toolbarAllowedItemIdentifiers: (NSToolbar *)toolbar
{
  /* return entire set... */
  return [identifierToItem allKeys];
}

/* optional and unneeded */
/* - toolbarWillAddItem: (NSNotification *)notification { } */
/* - toolbarDidRemoveItem: (NSNotification *)notification { } */
/* - (NSArray *)toolbarSelectableItemIdentifiers: (NSToolbar *)toolbar */

@end  /* EmacsToolbar */



/* ==========================================================================

    Tooltip: class implementation

   ========================================================================== */

/* Needed because NeXTstep does not provide enough control over tooltip
   display. */
@implementation EmacsTooltip

- init
{
  NSColor *col = [NSColor colorWithCalibratedRed: 1.0 green: 1.0
                                            blue: 0.792 alpha: 0.95];
  NSFont *font = [NSFont toolTipsFontOfSize: 0];
  NSFont *sfont = [font screenFont];
  int height = [sfont ascender] - [sfont descender];
/*[font boundingRectForFont].size.height; */
  NSRect r = NSMakeRect (0, 0, 100, height+6);

  textField = [[NSTextField alloc] initWithFrame: r];
  [textField setFont: font];
  [textField setBackgroundColor: col];

  [textField setEditable: NO];
  [textField setSelectable: NO];
  [textField setBordered: NO];
  [textField setBezeled: NO];
  [textField setDrawsBackground: YES];

  win = [[NSWindow alloc]
            initWithContentRect: [textField frame]
                      styleMask: 0
                        backing: NSBackingStoreBuffered
                          defer: YES];
  [win setHasShadow: YES];
  [win setReleasedWhenClosed: NO];
  [win setDelegate: self];
  [[win contentView] addSubview: textField];
/*  [win setBackgroundColor: col]; */
  [win setOpaque: NO];

  return self;
}

- (void) dealloc
{
  [win close];
  [win release];
  [textField release];
  [super dealloc];
}

- (void) setText: (char *)text
{
  NSString *str = [NSString stringWithUTF8String: text];
  NSRect r  = [textField frame];
  NSSize tooltipDims;

  [textField setStringValue: str];
  tooltipDims = [[textField cell] cellSize];

  r.size.width = tooltipDims.width;
  r.size.height = tooltipDims.height;
  [textField setFrame: r];
}

- (void) showAtX: (int)x Y: (int)y for: (int)seconds
{
  NSRect wr = [win frame];

  wr.origin = NSMakePoint (x, y);
  wr.size = [textField frame].size;

  [win setFrame: wr display: YES];
  [win orderFront: self];
  [win display];
  timer = [NSTimer scheduledTimerWithTimeInterval: (float)seconds target: self
                                         selector: @selector (hide)
                                         userInfo: nil repeats: NO];
  [timer retain];
}

- (void) hide
{
  [win close];
  if (timer != nil)
    {
      if ([timer isValid])
        [timer invalidate];
      [timer release];
      timer = nil;
    }
}

- (BOOL) isActive
{
  return timer != nil;
}

- (NSRect) frame
{
  return [textField frame];
}

@end  /* EmacsTooltip */



/* ==========================================================================

    Popup Dialog: implementing functions

   ========================================================================== */


static Lisp_Object
pop_down_menu (Lisp_Object arg)
{
  struct Lisp_Save_Value *p = XSAVE_VALUE (arg);
  if (popup_activated_flag)
    {
      popup_activated_flag = 0;
      BLOCK_INPUT;
      [NSApp endModalSession: popupSession];
      [((EmacsDialogPanel *) (p->pointer)) close];
      [[FRAME_NS_VIEW (SELECTED_FRAME ()) window] makeKeyWindow];
      UNBLOCK_INPUT;
    }
  return Qnil;
}


Lisp_Object
ns_popup_dialog (Lisp_Object position, Lisp_Object contents, Lisp_Object header)
{
  id dialog;
  Lisp_Object window, tem, title;
  struct frame *f;
  NSPoint p;
  BOOL isQ;

  NSTRACE (x-popup-dialog);

  check_ns ();

  isQ = NILP (header);

  if (EQ (position, Qt)
      || (CONSP (position) && (EQ (XCAR (position), Qmenu_bar)
                               || EQ (XCAR (position), Qtool_bar))))
    {
      window = selected_window;
    }
  else if (CONSP (position))
    {
      Lisp_Object tem;
      tem = Fcar (position);
      if (XTYPE (tem) == Lisp_Cons)
        window = Fcar (Fcdr (position));
      else
        {
          tem = Fcar (Fcdr (position));  /* EVENT_START (position) */
          window = Fcar (tem);	     /* POSN_WINDOW (tem) */
        }
    }
  else if (WINDOWP (position) || FRAMEP (position))
    {
      window = position;
    }
  else
    window = Qnil;

  if (FRAMEP (window))
    f = XFRAME (window);
  else if (WINDOWP (window))
    {
      CHECK_LIVE_WINDOW (window);
      f = XFRAME (WINDOW_FRAME (XWINDOW (window)));
    }
  else
    CHECK_WINDOW (window);

  p.x = (int)f->left_pos + ((int)FRAME_COLUMN_WIDTH (f) * f->text_cols)/2;
  p.y = (int)f->top_pos + (FRAME_LINE_HEIGHT (f) * f->text_lines)/2;

  title = Fcar (contents);
  CHECK_STRING (title);

  if (NILP (Fcar (Fcdr (contents))))
    /* No buttons specified, add an "Ok" button so users can pop down
       the dialog.  */
    contents = Fcons (title, Fcons (Fcons (build_string ("Ok"), Qt), Qnil));

  BLOCK_INPUT;
  dialog = [[EmacsDialogPanel alloc] initFromContents: contents
                                           isQuestion: isQ];
  {
    int specpdl_count = SPECPDL_INDEX ();
    record_unwind_protect (pop_down_menu, make_save_value (dialog, 0));
    popup_activated_flag = 1;
    tem = [dialog runDialogAt: p];
    unbind_to (specpdl_count, Qnil);  /* calls pop_down_menu */
  }
  UNBLOCK_INPUT;

  return tem;
}


/* ==========================================================================

    Popup Dialog: class implementation

   ========================================================================== */

@interface FlippedView : NSView
{
}
@end

@implementation FlippedView
- (BOOL)isFlipped
{
  return YES;
}
@end

@implementation EmacsDialogPanel

#define SPACER		8.0
#define ICONSIZE	64.0
#define TEXTHEIGHT	20.0
#define MINCELLWIDTH	90.0

- initWithContentRect: (NSRect)contentRect styleMask: (NSUInteger)aStyle
              backing: (NSBackingStoreType)backingType defer: (BOOL)flag
{
  NSSize spacing = {SPACER, SPACER};
  NSRect area;
  char this_cmd_name[80];
  id cell;
  static NSImageView *imgView;
  static FlippedView *contentView;

  if (imgView == nil)
    {
      NSImage *img;
      area.origin.x   = 3*SPACER;
      area.origin.y   = 2*SPACER;
      area.size.width = ICONSIZE;
      area.size.height= ICONSIZE;
      img = [[NSImage imageNamed: @"NSApplicationIcon"] copy];
      [img setScalesWhenResized: YES];
      [img setSize: NSMakeSize (ICONSIZE, ICONSIZE)];
      imgView = [[NSImageView alloc] initWithFrame: area];
      [imgView setImage: img];
      [imgView setEditable: NO];
      [img release];
    }

  aStyle = NSTitledWindowMask;
  flag = YES;
  rows = 0;
  cols = 1;
  [super initWithContentRect: contentRect styleMask: aStyle
                     backing: backingType defer: flag];
  contentView = [[FlippedView alloc] initWithFrame: [[self contentView] frame]];
  [self setContentView: contentView];

  [[self contentView] setAutoresizesSubviews: YES];

  [[self contentView] addSubview: imgView];
  [self setTitle: @""];

  area.origin.x   += ICONSIZE+2*SPACER;
/*  area.origin.y   = TEXTHEIGHT; ICONSIZE/2-10+SPACER; */
  area.size.width = 400;
  area.size.height= TEXTHEIGHT;
  command = [[[NSTextField alloc] initWithFrame: area] autorelease];
  [[self contentView] addSubview: command];
  [command setStringValue: ns_app_name];
  [command setDrawsBackground: NO];
  [command setBezeled: NO];
  [command setSelectable: NO];
  [command setFont: [NSFont boldSystemFontOfSize: 13.0]];

/*  area.origin.x   = ICONSIZE+2*SPACER;
  area.origin.y   = TEXTHEIGHT + 2*SPACER;
  area.size.width = 400;
  area.size.height= 2;
  tem = [[[NSBox alloc] initWithFrame: area] autorelease];
  [[self contentView] addSubview: tem];
  [tem setTitlePosition: NSNoTitle];
  [tem setAutoresizingMask: NSViewWidthSizable];*/

/*  area.origin.x = ICONSIZE+2*SPACER; */
  area.origin.y += TEXTHEIGHT+SPACER;
  area.size.width = 400;
  area.size.height= TEXTHEIGHT;
  title = [[[NSTextField alloc] initWithFrame: area] autorelease];
  [[self contentView] addSubview: title];
  [title setDrawsBackground: NO];
  [title setBezeled: NO];
  [title setSelectable: NO];
  [title setFont: [NSFont systemFontOfSize: 11.0]];

  cell = [[[NSButtonCell alloc] initTextCell: @""] autorelease];
  [cell setBordered: NO];
  [cell setEnabled: NO];
  [cell setCellAttribute: NSCellIsInsetButton to: 8];
  [cell setBezelStyle: NSRoundedBezelStyle];

  matrix = [[NSMatrix alloc] initWithFrame: contentRect
                                      mode: NSHighlightModeMatrix
                                 prototype: cell
                              numberOfRows: 0
                           numberOfColumns: 1];
  [[self contentView] addSubview: matrix];
  [matrix release];
  [matrix setFrameOrigin: NSMakePoint (area.origin.x,
                                      area.origin.y + (TEXTHEIGHT+3*SPACER))];
  [matrix setIntercellSpacing: spacing];

  [self setOneShot: YES];
  [self setReleasedWhenClosed: YES];
  [self setHidesOnDeactivate: YES];
  return self;
}


- (BOOL)windowShouldClose: (id)sender
{
  [NSApp stopModalWithCode: XHASH (Qnil)]; // FIXME: BIG UGLY HACK!!
  return NO;
}


void process_dialog (id window, Lisp_Object list)
{
  Lisp_Object item;
  int row = 0;

  for (; XTYPE (list) == Lisp_Cons; list = XCDR (list))
    {
      item = XCAR (list);
      if (XTYPE (item) == Lisp_String)
        {
          [window addString: SDATA (item) row: row++];
        }
      else if (XTYPE (item) == Lisp_Cons)
        {
          [window addButton: SDATA (XCAR (item))
                      value: XCDR (item) row: row++];
        }
      else if (NILP (item))
        {
          [window addSplit];
          row = 0;
        }
    }
}


- addButton: (char *)str value: (Lisp_Object)val row: (int)row
{
  id cell;

  if (row >= rows)
    {
      [matrix addRow];
      rows++;
    }
  cell = [matrix cellAtRow: row column: cols-1];
  [cell setTarget: self];
  [cell setAction: @selector (clicked: )];
  [cell setTitle: [NSString stringWithUTF8String: str]];
  [cell setTag: XHASH (val)];	// FIXME: BIG UGLY HACK!!
  [cell setBordered: YES];
  [cell setEnabled: YES];

  return self;
}


- addString: (char *)str row: (int)row
{
  id cell;

  if (row >= rows)
    {
      [matrix addRow];
      rows++;
    }
  cell = [matrix cellAtRow: row column: cols-1];
  [cell setTitle: [NSString stringWithUTF8String: str]];
  [cell setBordered: YES];
  [cell setEnabled: NO];

  return self;
}


- addSplit
{
  [matrix addColumn];
  cols++;
  return self;
}


- clicked: sender
{
  NSArray *sellist = nil;
  EMACS_INT seltag;

  sellist = [sender selectedCells];
  if ([sellist count]<1)
    return self;

  seltag = [[sellist objectAtIndex: 0] tag];
  if (seltag != XHASH (Qundefined)) // FIXME: BIG UGLY HACK!!
    [NSApp stopModalWithCode: seltag];
  return self;
}


- initFromContents: (Lisp_Object)contents isQuestion: (BOOL)isQ
{
  Lisp_Object head;
  [super init];

  if (XTYPE (contents) == Lisp_Cons)
    {
      head = Fcar (contents);
      process_dialog (self, Fcdr (contents));
    }
  else
    head = contents;

  if (XTYPE (head) == Lisp_String)
      [title setStringValue:
                 [NSString stringWithUTF8String: SDATA (head)]];
  else if (isQ == YES)
      [title setStringValue: @"Question"];
  else
      [title setStringValue: @"Information"];

  {
    int i;
    NSRect r, s, t;

    if (cols == 1 && rows > 1)	/* Never told where to split */
      {
        [matrix addColumn];
        for (i = 0; i<rows/2; i++)
          {
            [matrix putCell: [matrix cellAtRow: (rows+1)/2 column: 0]
                      atRow: i column: 1];
            [matrix removeRow: (rows+1)/2];
          }
      }

    [matrix sizeToFit];
    {
      NSSize csize = [matrix cellSize];
      if (csize.width < MINCELLWIDTH)
        {
          csize.width = MINCELLWIDTH;
          [matrix setCellSize: csize];
          [matrix sizeToCells];
        }
    }

    [title sizeToFit];
    [command sizeToFit];

    t = [matrix frame];
    r = [title frame];
    if (r.size.width+r.origin.x > t.size.width+t.origin.x)
      {
        t.origin.x   = r.origin.x;
        t.size.width = r.size.width;
      }
    r = [command frame];
    if (r.size.width+r.origin.x > t.size.width+t.origin.x)
      {
        t.origin.x   = r.origin.x;
        t.size.width = r.size.width;
      }

    r = [self frame];
    s = [(NSView *)[self contentView] frame];
    r.size.width  += t.origin.x+t.size.width +2*SPACER-s.size.width;
    r.size.height += t.origin.y+t.size.height+SPACER-s.size.height;
    [self setFrame: r display: NO];
  }

  return self;
}


- (void)dealloc
{
  { [super dealloc]; return; };
}


- (Lisp_Object)runDialogAt: (NSPoint)p
{
  NSInteger ret;

  /* initiate a session that will be ended by pop_down_menu */
  popupSession = [NSApp beginModalSessionForWindow: self];
  while (popup_activated_flag
         && (ret = [NSApp runModalSession: popupSession])
              == NSRunContinuesResponse)
    {
      /* Run this for timers.el, indep of atimers; might not return.
         TODO: use return value to avoid calling every iteration. */
      timer_check ();
      [NSThread sleepUntilDate: [NSDate dateWithTimeIntervalSinceNow: 0.1]];
    }

  {				/* FIXME: BIG UGLY HACK!!! */
      Lisp_Object tmp;
      *(EMACS_INT*)(&tmp) = ret;
      return tmp;
  }
}

@end


/* ==========================================================================

    Lisp definitions

   ========================================================================== */

DEFUN ("ns-reset-menu", Fns_reset_menu, Sns_reset_menu, 0, 0, 0,
       doc: /* Cause the NS menu to be re-calculated.  */)
     (void)
{
  set_frame_menubar (SELECTED_FRAME (), 1, 0);
  return Qnil;
}


DEFUN ("x-popup-dialog", Fx_popup_dialog, Sx_popup_dialog, 2, 3, 0,
       doc: /* Pop up a dialog box and return user's selection.
POSITION specifies which frame to use.
This is normally a mouse button event or a window or frame.
If POSITION is t, it means to use the frame the mouse is on.
The dialog box appears in the middle of the specified frame.

CONTENTS specifies the alternatives to display in the dialog box.
It is a list of the form (DIALOG ITEM1 ITEM2...).
Each ITEM is a cons cell (STRING . VALUE).
The return value is VALUE from the chosen item.

An ITEM may also be just a string--that makes a nonselectable item.
An ITEM may also be nil--that means to put all preceding items
on the left of the dialog box and all following items on the right.
\(By default, approximately half appear on each side.)

If HEADER is non-nil, the frame title for the box is "Information",
otherwise it is "Question".

If the user gets rid of the dialog box without making a valid choice,
for instance using the window manager, then this produces a quit and
`x-popup-dialog' does not return.  */)
     (Lisp_Object position, Lisp_Object contents, Lisp_Object header)
{
  return ns_popup_dialog (position, contents, header);
}

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* Return t if a menu or popup dialog is active.  */)
     (void)
{
  return popup_activated () ? Qt : Qnil;
}

/* ==========================================================================

    Lisp interface declaration

   ========================================================================== */

void
syms_of_nsmenu (void)
{
#ifndef NS_IMPL_COCOA
  /* Don't know how to keep track of this in Next/Open/Gnustep.  Always
     update menus there.  */
  trackingMenu = 1;
#endif
  defsubr (&Sx_popup_dialog);
  defsubr (&Sns_reset_menu);
  defsubr (&Smenu_or_popup_active_p);

  Qdebug_on_next_call = intern_c_string ("debug-on-next-call");
  staticpro (&Qdebug_on_next_call);
}
