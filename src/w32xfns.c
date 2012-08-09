/* Functions taken directly from X sources for use with the Microsoft W32 API.
   Copyright (C) 1989, 1992-1995, 1999, 2001-2012  Free Software Foundation, Inc.

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
#include <signal.h>
#include <stdio.h>
#include <setjmp.h>
#include "lisp.h"
#include "keyboard.h"
#include "frame.h"
#include "charset.h"
#include "fontset.h"
#include "blockinput.h"
#include "w32term.h"
#include "windowsx.h"

#define myalloc(cb) GlobalAllocPtr (GPTR, cb)
#define myfree(lp) GlobalFreePtr (lp)

CRITICAL_SECTION critsect;
extern HANDLE keyboard_handle;
HANDLE input_available = NULL;
HANDLE interrupt_handle = NULL;

void
init_crit (void)
{
  InitializeCriticalSection (&critsect);

  /* For safety, input_available should only be reset by get_next_msg
     when the input queue is empty, so make it a manual reset event. */
  keyboard_handle = input_available = CreateEvent (NULL, TRUE, FALSE, NULL);

  /* interrupt_handle is signaled when quit (C-g) is detected, so that
     blocking system calls can be interrupted.  We make it a manual
     reset event, so that if we should ever have multiple threads
     performing system calls, they will all be interrupted (I'm guessing
     that would the right response).  Note that we use PulseEvent to
     signal this event, so that it never remains signaled.  */
  interrupt_handle = CreateEvent (NULL, TRUE, FALSE, NULL);
}

void
delete_crit (void)
{
  DeleteCriticalSection (&critsect);

  if (input_available)
    {
      CloseHandle (input_available);
      input_available = NULL;
    }
  if (interrupt_handle)
    {
      CloseHandle (interrupt_handle);
      interrupt_handle = NULL;
    }
}

void
signal_quit (void)
{
  /* Make sure this event never remains signaled; if the main thread
     isn't in a blocking call, then this should do nothing.  */
  PulseEvent (interrupt_handle);
}

void
select_palette (FRAME_PTR f, HDC hdc)
{
  struct w32_display_info *display_info = FRAME_W32_DISPLAY_INFO (f);

  if (!display_info->has_palette)
    return;

  if (display_info->palette == 0)
    return;

  if (!NILP (Vw32_enable_palette))
    f->output_data.w32->old_palette =
      SelectPalette (hdc, display_info->palette, FALSE);
  else
    f->output_data.w32->old_palette = NULL;

  if (RealizePalette (hdc) != GDI_ERROR)
  {
    Lisp_Object frame, framelist;
    FOR_EACH_FRAME (framelist, frame)
    {
      SET_FRAME_GARBAGED (XFRAME (frame));
    }
  }
}

void
deselect_palette (FRAME_PTR f, HDC hdc)
{
  if (f->output_data.w32->old_palette)
    SelectPalette (hdc, f->output_data.w32->old_palette, FALSE);
}

/* Get a DC for frame and select palette for drawing; force an update of
   all frames if palette's mapping changes.  */
HDC
get_frame_dc (FRAME_PTR f)
{
  HDC hdc;

  if (f->output_method != output_w32)
    abort ();

  enter_crit ();

  hdc = GetDC (f->output_data.w32->window_desc);

  /* If this gets called during startup before the frame is valid,
     there is a chance of corrupting random data or crashing. */
  if (hdc)
    select_palette (f, hdc);

  return hdc;
}

int
release_frame_dc (FRAME_PTR f, HDC hdc)
{
  int ret;

  deselect_palette (f, hdc);
  ret = ReleaseDC (f->output_data.w32->window_desc, hdc);

  leave_crit ();

  return ret;
}

typedef struct int_msg
{
  W32Msg w32msg;
  struct int_msg *lpNext;
} int_msg;

int_msg *lpHead = NULL;
int_msg *lpTail = NULL;
int nQueue = 0;

BOOL
get_next_msg (W32Msg * lpmsg, BOOL bWait)
{
  BOOL bRet = FALSE;

  enter_crit ();

  /* The while loop takes care of multiple sets */

  while (!nQueue && bWait)
    {
      leave_crit ();
      WaitForSingleObject (input_available, INFINITE);
      enter_crit ();
    }

  if (nQueue)
    {
      memcpy (lpmsg, &lpHead->w32msg, sizeof (W32Msg));

      {
	int_msg * lpCur = lpHead;

	lpHead = lpHead->lpNext;

	myfree (lpCur);
      }

      nQueue--;
      /* Consolidate WM_PAINT messages to optimize redrawing.  */
      if (lpmsg->msg.message == WM_PAINT && nQueue)
        {
          int_msg * lpCur = lpHead;
          int_msg * lpPrev = NULL;
          int_msg * lpNext = NULL;

          while (lpCur && nQueue)
            {
              lpNext = lpCur->lpNext;
              if (lpCur->w32msg.msg.message == WM_PAINT)
                {
                  /* Remove this message from the queue.  */
                  if (lpPrev)
                    lpPrev->lpNext = lpNext;
                  else
                    lpHead = lpNext;

                  if (lpCur == lpTail)
                    lpTail = lpPrev;

                  /* Adjust clip rectangle to cover both.  */
                  if (!UnionRect (&(lpmsg->rect), &(lpmsg->rect),
                                  &(lpCur->w32msg.rect)))
                    {
                      SetRectEmpty (&(lpmsg->rect));
                    }

                  myfree (lpCur);

                  nQueue--;

                  lpCur = lpNext;
                }
              else
                {
                  lpPrev = lpCur;
                  lpCur = lpNext;
                }
            }
        }

      bRet = TRUE;
    }

  if (nQueue == 0)
    ResetEvent (input_available);

  leave_crit ();

  return (bRet);
}

BOOL
post_msg (W32Msg * lpmsg)
{
  int_msg * lpNew = (int_msg *) myalloc (sizeof (int_msg));

  if (!lpNew)
    return (FALSE);

  memcpy (&lpNew->w32msg, lpmsg, sizeof (W32Msg));
  lpNew->lpNext = NULL;

  enter_crit ();

  if (nQueue++)
    {
      lpTail->lpNext = lpNew;
    }
  else
    {
      lpHead = lpNew;
    }

  lpTail = lpNew;
  SetEvent (input_available);

  leave_crit ();

  return (TRUE);
}

BOOL
prepend_msg (W32Msg *lpmsg)
{
  int_msg * lpNew = (int_msg *) myalloc (sizeof (int_msg));

  if (!lpNew)
    return (FALSE);

  memcpy (&lpNew->w32msg, lpmsg, sizeof (W32Msg));

  enter_crit ();

  nQueue++;
  lpNew->lpNext = lpHead;
  lpHead = lpNew;

  leave_crit ();

  return (TRUE);
}

/* Process all messages in the current thread's queue.  */
void
drain_message_queue (void)
{
  MSG msg;
  while (PeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
    {
      TranslateMessage (&msg);
      DispatchMessage (&msg);
    }
}


/*
 *    XParseGeometry parses strings of the form
 *   "=<width>x<height>{+-}<xoffset>{+-}<yoffset>", where
 *   width, height, xoffset, and yoffset are unsigned integers.
 *   Example:  "=80x24+300-49"
 *   The equal sign is optional.
 *   It returns a bitmask that indicates which of the four values
 *   were actually found in the string.  For each value found,
 *   the corresponding argument is updated;  for each value
 *   not found, the corresponding argument is left unchanged.
 */

static int
read_integer (register char *string, char **NextString)
{
  register int Result = 0;
  int Sign = 1;

  if (*string == '+')
    string++;
  else if (*string == '-')
    {
      string++;
      Sign = -1;
    }
  for (; (*string >= '0') && (*string <= '9'); string++)
    {
      Result = (Result * 10) + (*string - '0');
    }
  *NextString = string;
  if (Sign >= 0)
    return (Result);
  else
    return (-Result);
}

int
XParseGeometry (char *string,
		int *x, int *y,
		unsigned int *width, unsigned int *height)
{
  int mask = NoValue;
  register char *strind;
  unsigned int tempWidth, tempHeight;
  int tempX, tempY;
  char *nextCharacter;

  if ((string == NULL) || (*string == '\0')) return (mask);
  if (*string == '=')
    string++;  /* ignore possible '=' at beg of geometry spec */

  strind = (char *)string;
  if (*strind != '+' && *strind != '-' && *strind != 'x')
    {
      tempWidth = read_integer (strind, &nextCharacter);
      if (strind == nextCharacter)
	return (0);
      strind = nextCharacter;
      mask |= WidthValue;
    }

  if (*strind == 'x' || *strind == 'X')
    {
      strind++;
      tempHeight = read_integer (strind, &nextCharacter);
      if (strind == nextCharacter)
	return (0);
      strind = nextCharacter;
      mask |= HeightValue;
    }

  if ((*strind == '+') || (*strind == '-'))
    {
      if (*strind == '-')
	{
	  strind++;
	  tempX = -read_integer (strind, &nextCharacter);
	  if (strind == nextCharacter)
	    return (0);
	  strind = nextCharacter;
	  mask |= XNegative;

	}
      else
	{
	  strind++;
	  tempX = read_integer (strind, &nextCharacter);
	  if (strind == nextCharacter)
	    return (0);
	  strind = nextCharacter;
	}
      mask |= XValue;
      if ((*strind == '+') || (*strind == '-'))
	{
	  if (*strind == '-')
	    {
	      strind++;
	      tempY = -read_integer (strind, &nextCharacter);
	      if (strind == nextCharacter)
		return (0);
	      strind = nextCharacter;
	      mask |= YNegative;
	    }
	  else
	    {
	      strind++;
	      tempY = read_integer (strind, &nextCharacter);
	      if (strind == nextCharacter)
		return (0);
	      strind = nextCharacter;
	    }
	  mask |= YValue;
	}
    }

  /* If strind isn't at the end of the string then it's an invalid
     geometry specification. */

  if (*strind != '\0') return (0);

  if (mask & XValue)
    *x = tempX;
  if (mask & YValue)
    *y = tempY;
  if (mask & WidthValue)
    *width = tempWidth;
  if (mask & HeightValue)
    *height = tempHeight;
  return (mask);
}

/* x_sync is a no-op on W32.  */
void
x_sync (void *f)
{
}
