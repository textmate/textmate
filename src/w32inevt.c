/* Input event support for Emacs on the Microsoft W32 API.
   Copyright (C) 1992-1993, 1995, 2001-2012  Free Software Foundation, Inc.

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
   Drew Bliss                   01-Oct-93
     Adapted from ntkbd.c by Tim Fleehart
*/


#include <config.h>
#include <stdio.h>
#include <windows.h>
#include <setjmp.h>

#ifndef MOUSE_MOVED
#define MOUSE_MOVED   1
#endif

#include "lisp.h"
#include "keyboard.h"
#include "frame.h"
#include "dispextern.h"
#include "blockinput.h"
#include "termhooks.h"
#include "w32heap.h"
#include "w32term.h"

/* stdin, from w32console.c */
extern HANDLE keyboard_handle;

/* Info for last mouse motion */
static COORD movement_pos;
static Time movement_time;

/* from w32fns.c */
extern unsigned int map_keypad_keys (unsigned int, unsigned int);
extern unsigned int w32_key_to_modifier (int key);

/* Event queue */
#define EVENT_QUEUE_SIZE 50
static INPUT_RECORD event_queue[EVENT_QUEUE_SIZE];
static INPUT_RECORD *queue_ptr = event_queue, *queue_end = event_queue;

/* Temporarily store lead byte of DBCS input sequences.  */
static char dbcs_lead = 0;

static int
fill_queue (BOOL block)
{
  BOOL rc;
  DWORD events_waiting;

  if (queue_ptr < queue_end)
    return queue_end-queue_ptr;

  if (!block)
    {
      /* Check to see if there are some events to read before we try
	 because we can't block.  */
      if (!GetNumberOfConsoleInputEvents (keyboard_handle, &events_waiting))
	return -1;
      if (events_waiting == 0)
	return 0;
    }

  rc = ReadConsoleInput (keyboard_handle, event_queue, EVENT_QUEUE_SIZE,
			 &events_waiting);
  if (!rc)
    return -1;
  queue_ptr = event_queue;
  queue_end = event_queue + events_waiting;
  return (int) events_waiting;
}

/* In a generic, multi-frame world this should take a console handle
   and return the frame for it

   Right now, there's only one frame so return it.  */
static FRAME_PTR
get_frame (void)
{
  return SELECTED_FRAME ();
}

/* Translate console modifiers to emacs modifiers.
   German keyboard support (Kai Morgan Zeise 2/18/95).  */
int
w32_kbd_mods_to_emacs (DWORD mods, WORD key)
{
  int retval = 0;

  /* If we recognize right-alt and left-ctrl as AltGr, and it has been
     pressed, first remove those modifiers.  */
  if (!NILP (Vw32_recognize_altgr)
      && (mods & (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED))
      == (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED))
    mods &= ~ (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED);

  if (mods & (RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED))
    retval = ((NILP (Vw32_alt_is_meta)) ? alt_modifier : meta_modifier);

  if (mods & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
    {
      retval |= ctrl_modifier;
      if ((mods & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
	  == (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
	retval |= meta_modifier;
    }

  if (mods & LEFT_WIN_PRESSED)
    retval |= w32_key_to_modifier (VK_LWIN);
  if (mods & RIGHT_WIN_PRESSED)
    retval |= w32_key_to_modifier (VK_RWIN);
  if (mods & APPS_PRESSED)
    retval |= w32_key_to_modifier (VK_APPS);
  if (mods & SCROLLLOCK_ON)
    retval |= w32_key_to_modifier (VK_SCROLL);

  /* Just in case someone wanted the original behavior, make it
     optional by setting w32-capslock-is-shiftlock to t.  */
  if (NILP (Vw32_capslock_is_shiftlock)
      /* Keys that should _not_ be affected by CapsLock.  */
      && (    (key == VK_BACK)
	   || (key == VK_TAB)
	   || (key == VK_CLEAR)
	   || (key == VK_RETURN)
	   || (key == VK_ESCAPE)
	   || ((key >= VK_SPACE) && (key <= VK_HELP))
	   || ((key >= VK_NUMPAD0) && (key <= VK_F24))
	   || ((key >= VK_NUMPAD_CLEAR) && (key <= VK_NUMPAD_DELETE))
	 ))
    {
      /* Only consider shift state.  */
      if ((mods & SHIFT_PRESSED) != 0)
	retval |= shift_modifier;
    }
  else
    {
      /* Ignore CapsLock state if not enabled.  */
      if (NILP (Vw32_enable_caps_lock))
	mods &= ~CAPSLOCK_ON;
      if ((mods & (SHIFT_PRESSED | CAPSLOCK_ON)) != 0)
	retval |= shift_modifier;
    }

  return retval;
}

#if 0
/* Return nonzero if the virtual key is a dead key.  */
static int
is_dead_key (int wparam)
{
  unsigned int code = MapVirtualKey (wparam, 2);

  /* Windows 95 returns 0x8000, NT returns 0x80000000.  */
  return (code & 0x80008000) ? 1 : 0;
}
#endif

/* The return code indicates key code size. */
int
w32_kbd_patch_key (KEY_EVENT_RECORD *event)
{
  unsigned int key_code = event->wVirtualKeyCode;
  unsigned int mods = event->dwControlKeyState;
  BYTE keystate[256];
  static BYTE ansi_code[4];
  static int isdead = 0;

  if (isdead == 2)
    {
      event->uChar.AsciiChar = ansi_code[2];
      isdead = 0;
      return 1;
    }
  if (event->uChar.AsciiChar != 0)
    return 1;

  memset (keystate, 0, sizeof (keystate));
  keystate[key_code] = 0x80;
  if (mods & SHIFT_PRESSED)
    keystate[VK_SHIFT] = 0x80;
  if (mods & CAPSLOCK_ON)
    keystate[VK_CAPITAL] = 1;
  /* If we recognize right-alt and left-ctrl as AltGr, set the key
     states accordingly before invoking ToAscii.  */
  if (!NILP (Vw32_recognize_altgr)
      && (mods & LEFT_CTRL_PRESSED) && (mods & RIGHT_ALT_PRESSED))
    {
      keystate[VK_CONTROL] = 0x80;
      keystate[VK_LCONTROL] = 0x80;
      keystate[VK_MENU] = 0x80;
      keystate[VK_RMENU] = 0x80;
    }

#if 0
  /* Because of an OS bug, ToAscii corrupts the stack when called to
     convert a dead key in console mode on NT4.  Unfortunately, trying
     to check for dead keys using MapVirtualKey doesn't work either -
     these functions apparently use internal information about keyboard
     layout which doesn't get properly updated in console programs when
     changing layout (though apparently it gets partly updated,
     otherwise ToAscii wouldn't crash).  */
  if (is_dead_key (event->wVirtualKeyCode))
    return 0;
#endif

  /* On NT, call ToUnicode instead and then convert to the current
     locale's default codepage.  */
  if (os_subtype == OS_NT)
    {
      WCHAR buf[128];

      isdead = ToUnicode (event->wVirtualKeyCode, event->wVirtualScanCode,
			  keystate, buf, 128, 0);
      if (isdead > 0)
	{
	  char cp[20];
	  int cpId;

	  event->uChar.UnicodeChar = buf[isdead - 1];

	  GetLocaleInfo (GetThreadLocale (),
			 LOCALE_IDEFAULTANSICODEPAGE, cp, 20);
	  cpId = atoi (cp);
	  isdead = WideCharToMultiByte (cpId, 0, buf, isdead,
					ansi_code, 4, NULL, NULL);
	}
      else
	isdead = 0;
    }
  else
    {
      isdead = ToAscii (event->wVirtualKeyCode, event->wVirtualScanCode,
                        keystate, (LPWORD) ansi_code, 0);
    }

  if (isdead == 0)
    return 0;
  event->uChar.AsciiChar = ansi_code[0];
  return isdead;
}


static int faked_key = 0;

/* return code -1 means that event_queue_ptr won't be incremented.
   In other word, this event makes two key codes.   (by himi)       */
static int
key_event (KEY_EVENT_RECORD *event, struct input_event *emacs_ev, int *isdead)
{
  static int mod_key_state = 0;
  int wParam;

  *isdead = 0;

  /* Skip key-up events.  */
  if (!event->bKeyDown)
    {
      switch (event->wVirtualKeyCode)
	{
	case VK_LWIN:
	  mod_key_state &= ~LEFT_WIN_PRESSED;
	  break;
	case VK_RWIN:
	  mod_key_state &= ~RIGHT_WIN_PRESSED;
	  break;
	case VK_APPS:
	  mod_key_state &= ~APPS_PRESSED;
	  break;
	}
      return 0;
    }

  /* Ignore keystrokes we fake ourself; see below.  */
  if (faked_key == event->wVirtualKeyCode)
    {
      faked_key = 0;
      return 0;
    }

  /* To make it easier to debug this code, ignore modifier keys!  */
  switch (event->wVirtualKeyCode)
    {
    case VK_LWIN:
      if (NILP (Vw32_pass_lwindow_to_system))
	{
	  /* Prevent system from acting on keyup (which opens the Start
	     menu if no other key was pressed) by simulating a press of
	     Space which we will ignore.  */
	  if ((mod_key_state & LEFT_WIN_PRESSED) == 0)
	    {
	      if (NUMBERP (Vw32_phantom_key_code))
		faked_key = XUINT (Vw32_phantom_key_code) & 255;
	      else
		faked_key = VK_SPACE;
	      keybd_event (faked_key, (BYTE) MapVirtualKey (faked_key, 0), 0, 0);
	    }
	}
      mod_key_state |= LEFT_WIN_PRESSED;
      if (!NILP (Vw32_lwindow_modifier))
	return 0;
      break;
    case VK_RWIN:
      if (NILP (Vw32_pass_rwindow_to_system))
	{
	  if ((mod_key_state & RIGHT_WIN_PRESSED) == 0)
	    {
	      if (NUMBERP (Vw32_phantom_key_code))
		faked_key = XUINT (Vw32_phantom_key_code) & 255;
	      else
		faked_key = VK_SPACE;
	      keybd_event (faked_key, (BYTE) MapVirtualKey (faked_key, 0), 0, 0);
	    }
	}
      mod_key_state |= RIGHT_WIN_PRESSED;
      if (!NILP (Vw32_rwindow_modifier))
	return 0;
      break;
    case VK_APPS:
      mod_key_state |= APPS_PRESSED;
      if (!NILP (Vw32_apps_modifier))
	return 0;
      break;
    case VK_CAPITAL:
      /* Decide whether to treat as modifier or function key.  */
      if (NILP (Vw32_enable_caps_lock))
	goto disable_lock_key;
      return 0;
    case VK_NUMLOCK:
      /* Decide whether to treat as modifier or function key.  */
      if (NILP (Vw32_enable_num_lock))
	goto disable_lock_key;
      return 0;
    case VK_SCROLL:
      /* Decide whether to treat as modifier or function key.  */
      if (NILP (Vw32_scroll_lock_modifier))
	goto disable_lock_key;
      return 0;
    disable_lock_key:
      /* Ensure the appropriate lock key state is off (and the
	 indicator light as well).  */
      wParam = event->wVirtualKeyCode;
      if (GetAsyncKeyState (wParam) & 0x8000)
	{
	  /* Fake another press of the relevant key.  Apparently, this
	     really is the only way to turn off the indicator.  */
	  faked_key = wParam;
	  keybd_event ((BYTE) wParam, (BYTE) MapVirtualKey (wParam, 0),
		       KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
	  keybd_event ((BYTE) wParam, (BYTE) MapVirtualKey (wParam, 0),
		       KEYEVENTF_EXTENDEDKEY | 0, 0);
	  keybd_event ((BYTE) wParam, (BYTE) MapVirtualKey (wParam, 0),
		       KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
	}
      break;
    case VK_MENU:
    case VK_CONTROL:
    case VK_SHIFT:
      return 0;
    case VK_CANCEL:
      /* Windows maps Ctrl-Pause (aka Ctrl-Break) into VK_CANCEL,
	 which is confusing for purposes of key binding; convert
	 VK_CANCEL events into VK_PAUSE events.  */
      event->wVirtualKeyCode = VK_PAUSE;
      break;
    case VK_PAUSE:
      /* Windows maps Ctrl-NumLock into VK_PAUSE, which is confusing
	 for purposes of key binding; convert these back into
	 VK_NUMLOCK events, at least when we want to see NumLock key
	 presses.  (Note that there is never any possibility that
	 VK_PAUSE with Ctrl really is C-Pause as per above.)  */
      if (NILP (Vw32_enable_num_lock)
	  && (event->dwControlKeyState
	      & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)) != 0)
	event->wVirtualKeyCode = VK_NUMLOCK;
      break;
    }

  /* Recognize state of Windows and Apps keys.  */
  event->dwControlKeyState |= mod_key_state;

  /* Distinguish numeric keypad keys from extended keys.  */
  event->wVirtualKeyCode =
    map_keypad_keys (event->wVirtualKeyCode,
		     (event->dwControlKeyState & ENHANCED_KEY));

  if (lispy_function_keys[event->wVirtualKeyCode] == 0)
    {
      if (!NILP (Vw32_recognize_altgr)
	  && (event->dwControlKeyState & LEFT_CTRL_PRESSED)
	  && (event->dwControlKeyState & RIGHT_ALT_PRESSED))
	{
	  /* Don't try to interpret AltGr key chords; ToAscii seems not
	     to process them correctly.  */
	}
      /* Handle key chords including any modifiers other than shift
         directly, in order to preserve as much modifier information as
         possible.  */
      else if (event->dwControlKeyState
	       & (  RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED
		  | RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED
		  | (!NILP (Vw32_lwindow_modifier) ? LEFT_WIN_PRESSED : 0)
		  | (!NILP (Vw32_rwindow_modifier) ? RIGHT_WIN_PRESSED : 0)
		  | (!NILP (Vw32_apps_modifier) ? APPS_PRESSED : 0)
		  | (!NILP (Vw32_scroll_lock_modifier) ? SCROLLLOCK_ON : 0)))
	{
	  /* Don't translate modified alphabetic keystrokes, so the user
	     doesn't need to constantly switch layout to type control or
	     meta keystrokes when the normal layout translates
	     alphabetic characters to non-ascii characters.  */
	  if ('A' <= event->wVirtualKeyCode && event->wVirtualKeyCode <= 'Z')
	    {
	      event->uChar.AsciiChar = event->wVirtualKeyCode;
	      if ((event->dwControlKeyState & SHIFT_PRESSED) == 0)
		event->uChar.AsciiChar += ('a' - 'A');
	    }
	  /* Try to handle unrecognized keystrokes by determining the
             base character (ie. translating the base key plus shift
             modifier).  */
	  else if (event->uChar.AsciiChar == 0)
	    w32_kbd_patch_key (event);
	}

      if (event->uChar.AsciiChar == 0)
	{
	  emacs_ev->kind = NO_EVENT;
	  return 0;
	}
      else if (event->uChar.AsciiChar > 0)
	{
	  emacs_ev->kind = ASCII_KEYSTROKE_EVENT;
	  emacs_ev->code = event->uChar.AsciiChar;
	}
      else if (event->uChar.UnicodeChar > 0)
	{
	  emacs_ev->kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;
	  emacs_ev->code = event->uChar.UnicodeChar;
	}
      else
	{
	  /* Fallback for non-Unicode versions of Windows.  */
	  wchar_t code;
	  char dbcs[2];
          char cp[20];
          int cpId;

	  /* Get the codepage to interpret this key with.  */
          GetLocaleInfo (GetThreadLocale (),
			 LOCALE_IDEFAULTANSICODEPAGE, cp, 20);
          cpId = atoi (cp);

	  dbcs[0] = dbcs_lead;
	  dbcs[1] = event->uChar.AsciiChar;
	  if (dbcs_lead)
	    {
	      dbcs_lead = 0;
	      if (!MultiByteToWideChar (cpId, 0, dbcs, 2, &code, 1))
		{
		  /* Garbage  */
		  DebPrint (("Invalid DBCS sequence: %d %d\n",
			     dbcs[0], dbcs[1]));
		  emacs_ev->kind = NO_EVENT;
		}
	    }
	  else if (IsDBCSLeadByteEx (cpId, dbcs[1]))
	    {
	      dbcs_lead = dbcs[1];
	      emacs_ev->kind = NO_EVENT;
	    }
	  else
	    {
	      if (!MultiByteToWideChar (cpId, 0, &dbcs[1], 1, &code, 1))
		{
		  /* Garbage  */
		  DebPrint (("Invalid character: %d\n", dbcs[1]));
		  emacs_ev->kind = NO_EVENT;
		}
	    }
	  emacs_ev->kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;
	  emacs_ev->code = code;
	}
    }
  else
    {
      emacs_ev->kind = NON_ASCII_KEYSTROKE_EVENT;
      emacs_ev->code = event->wVirtualKeyCode;
    }

  XSETFRAME (emacs_ev->frame_or_window, get_frame ());
  emacs_ev->modifiers = w32_kbd_mods_to_emacs (event->dwControlKeyState,
					       event->wVirtualKeyCode);
  emacs_ev->timestamp = GetTickCount ();
  return 1;
}

int
w32_console_toggle_lock_key (int vk_code, Lisp_Object new_state)
{
  int cur_state = (GetKeyState (vk_code) & 1);

  if (NILP (new_state)
      || (NUMBERP (new_state)
	  && ((XUINT (new_state)) & 1) != cur_state))
    {
      faked_key = vk_code;

      keybd_event ((BYTE) vk_code,
		   (BYTE) MapVirtualKey (vk_code, 0),
		   KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
      keybd_event ((BYTE) vk_code,
		   (BYTE) MapVirtualKey (vk_code, 0),
		   KEYEVENTF_EXTENDEDKEY | 0, 0);
      keybd_event ((BYTE) vk_code,
		   (BYTE) MapVirtualKey (vk_code, 0),
		   KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
      cur_state = !cur_state;
    }

  return cur_state;
}

/* Mouse position hook.  */
void
w32_console_mouse_position (FRAME_PTR *f,
			    int insist,
			    Lisp_Object *bar_window,
			    enum scroll_bar_part *part,
			    Lisp_Object *x,
			    Lisp_Object *y,
			    Time *time)
{
  BLOCK_INPUT;

  insist = insist;

  *f = get_frame ();
  *bar_window = Qnil;
  *part = 0;
  SELECTED_FRAME ()->mouse_moved = 0;

  XSETINT (*x, movement_pos.X);
  XSETINT (*y, movement_pos.Y);
  *time = movement_time;

  UNBLOCK_INPUT;
}

/* Remember mouse motion and notify emacs.  */
static void
mouse_moved_to (int x, int y)
{
  /* If we're in the same place, ignore it */
  if (x != movement_pos.X || y != movement_pos.Y)
    {
      SELECTED_FRAME ()->mouse_moved = 1;
      movement_pos.X = x;
      movement_pos.Y = y;
      movement_time = GetTickCount ();
    }
}

/* Consoles return button bits in a strange order:
     least significant - Leftmost button
     next - Rightmost button
     next - Leftmost+1
     next - Leftmost+2...

   Assume emacs likes three button mice, so
     Left == 0
     Middle == 1
     Right == 2
   Others increase from there.  */

#define NUM_TRANSLATED_MOUSE_BUTTONS 3
static int emacs_button_translation[NUM_TRANSLATED_MOUSE_BUTTONS] =
{
  0, 2, 1
};

static int
do_mouse_event (MOUSE_EVENT_RECORD *event,
		struct input_event *emacs_ev)
{
  static DWORD button_state = 0;
  DWORD but_change, mask;
  int i;

  if (event->dwEventFlags == MOUSE_MOVED)
    {
      /* For movement events we just note that the mouse has moved
	 so that emacs will generate drag events.  */
      mouse_moved_to (event->dwMousePosition.X, event->dwMousePosition.Y);
      return 0;
    }

  /* It looks like the console code sends us a mouse event with
     dwButtonState == 0 when a window is activated.  Ignore this case.  */
  if (event->dwButtonState == button_state)
    return 0;

  emacs_ev->kind = MOUSE_CLICK_EVENT;

  /* Find out what button has changed state since the last button event.  */
  but_change = button_state ^ event->dwButtonState;
  mask = 1;
  for (i = 0; mask; i++, mask <<= 1)
    if (but_change & mask)
      {
        if (i < NUM_TRANSLATED_MOUSE_BUTTONS)
          emacs_ev->code = emacs_button_translation[i];
        else
          emacs_ev->code = i;
	break;
      }

  button_state = event->dwButtonState;
  emacs_ev->timestamp = GetTickCount ();
  emacs_ev->modifiers = w32_kbd_mods_to_emacs (event->dwControlKeyState, 0) |
    ((event->dwButtonState & mask) ? down_modifier : up_modifier);

  XSETFASTINT (emacs_ev->x, event->dwMousePosition.X);
  XSETFASTINT (emacs_ev->y, event->dwMousePosition.Y);
/* for Mule 2.2 (Based on Emacs 19.28 */
#ifdef MULE
  XSET (emacs_ev->frame_or_window, Lisp_Frame, get_frame ());
#else
  XSETFRAME (emacs_ev->frame_or_window, get_frame ());
#endif

  return 1;
}

static void
resize_event (WINDOW_BUFFER_SIZE_RECORD *event)
{
  FRAME_PTR f = get_frame ();

  change_frame_size (f, event->dwSize.Y, event->dwSize.X, 0, 1, 0);
  SET_FRAME_GARBAGED (f);
}

static void
maybe_generate_resize_event (void)
{
  CONSOLE_SCREEN_BUFFER_INFO info;
  FRAME_PTR f = get_frame ();

  GetConsoleScreenBufferInfo (GetStdHandle (STD_OUTPUT_HANDLE), &info);

  /* It is okay to call this unconditionally, since it will do nothing
     if the size hasn't actually changed.  */
  change_frame_size (f,
		     1 + info.srWindow.Bottom - info.srWindow.Top,
		     1 + info.srWindow.Right - info.srWindow.Left,
		     0, 0, 0);
}

int
w32_console_read_socket (struct terminal *terminal,
                         int expected,
                         struct input_event *hold_quit)
{
  int nev, ret = 0, add;
  int isdead;

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }

  interrupt_input_pending = 0;
  BLOCK_INPUT;

  for (;;)
    {
      nev = fill_queue (0);
      if (nev <= 0)
        {
	  /* If nev == -1, there was some kind of error
	     If nev == 0 then waitp must be zero and no events were available
	     so return.  */
	  UNBLOCK_INPUT;
	  return nev;
        }

      while (nev > 0)
        {
	  struct input_event inev;

	  EVENT_INIT (inev);
	  inev.kind = NO_EVENT;
	  inev.arg = Qnil;

	  switch (queue_ptr->EventType)
            {
            case KEY_EVENT:
	      add = key_event (&queue_ptr->Event.KeyEvent, &inev, &isdead);
	      if (add == -1) /* 95.7.25 by himi */
		{
		  queue_ptr--;
		  add = 1;
		}
	      if (add)
		kbd_buffer_store_event_hold (&inev, hold_quit);
	      break;

            case MOUSE_EVENT:
	      add = do_mouse_event (&queue_ptr->Event.MouseEvent, &inev);
	      if (add)
		kbd_buffer_store_event_hold (&inev, hold_quit);
	      break;

            case WINDOW_BUFFER_SIZE_EVENT:
	      if (w32_use_full_screen_buffer)
		resize_event (&queue_ptr->Event.WindowBufferSizeEvent);
	      break;

            case MENU_EVENT:
            case FOCUS_EVENT:
	      /* Internal event types, ignored. */
	      break;
            }

	  queue_ptr++;
	  nev--;
        }

      if (ret > 0 || expected == 0)
	break;
    }

  /* We don't get told about changes in the window size (only the buffer
     size, which we no longer care about), so we have to check it
     periodically.  */
  if (!w32_use_full_screen_buffer)
    maybe_generate_resize_event ();

  UNBLOCK_INPUT;
  return ret;
}
