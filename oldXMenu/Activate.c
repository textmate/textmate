/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"

/*
Copyright (C) 2001-2012  Free Software Foundation, Inc.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuActivate -	Maps a given menu to the display and activates
 *			the menu for user selection.  The user is allowed to
 *			specify which pane and selection will be current,
 *			the X and Y location of the menu (relative to the
 *			parent window) and the mouse button event mask that
 *			will be used to identify a selection request.
 *
 *			A menu selection is shown to be current by placing
 *			a highlight box around the selection as the mouse
 *			cursor enters its active region.  Inactive selections
 *			will not be highlighted.  As the mouse cursor moved
 *			from one menu pane to another menu pane the pane being
 *			entered is raised and made current and the pane being
 *			left is lowered.
 *
 *			Anytime XMenuActivate returns, the p_num and
 *			s_num are left at their last known values (i.e.,
 *			the last known current pane and selection indices).
 *			The following are the defined return states:
 *
 *			1)	If at any time an error occurs the data
 *				pointer is left untouched and XM_FAILURE
 *				is returned.
 *
 *			2)	When a selection request is received (i.e.,
 *				when the specified mouse event occurs) the
 *				data pointer will be set to the data
 *				associated with the particular selection
 *				current at the time of the selection request
 *				and XM_SUCCESS is returned.
 *
 *			3)	If no selection was current at the time a
 *				selection request is made the data pointer
 *				will be left untouched and XM_NO_SELECT will
 *				be returned.
 *
 *			4)	If the selection that was current at the time
 *				a selection request is made is not an active
 *				selection the data pointer will be left
 *				untouched and XM_IA_SELECT will be returned.
 *
 *			Since X processes events in an asynchronous manner
 *			it is likely that XMenuActivate will encounter
 *			a "foreign event" while it is executing.  Foreign
 *			events are handled in one of three ways:
 *
 *			1)	The event is discarded.  This is the default
 *				mode and requires no action on the part of the
 *				application.
 *
 *			2)	The application has identified an asynchronous
 *				event handler that will be called and the
 *				foreign event handed off to it.  Note:
 *				AEQ mode disables this mode temporarily.
 *
 *			3)	The application has enabled asynchronous event
 *				queuing mode.  In this mode all foreign events
 *				will be	queued up until XMenuActivate
 *				terminates; at which time they will be
 *				returned to the	X event queue.  As long as
 *				AEQ mode is enabled any asynchronous event
 *				handler as temporarily disabled.
 *
 *			Any events encountered while taking down the menu
 *			(i.e., exposure events from occluded windows) will
 *			automatically be returned to the X event queue after
 *			XMenuActivate has cleaned the queue of any of its own
 *			events that are no longer needed.
 *
 *	Author:		Tony Della Fera, DEC
 *			March 12, 1986
 *
 */

#include <config.h>
#include "XMenuInt.h"
#include <X11/keysym.h>

/* For debug, set this to 0 to not grab the keyboard on menu popup */
int x_menu_grab_keyboard = 1;

static Wait_func wait_func;
static void* wait_data;

void
XMenuActivateSetWaitFunction (Wait_func func, void *data)
{
  wait_func = func;
  wait_data = data;
}

int
XMenuActivate(
    register Display *display,		/* Display to put menu on. */
    register XMenu *menu,		/* Menu to activate. */
    int *p_num,				/* Pane number selected. */
    int *s_num,				/* Selection number selected. */
    int x_pos,				/* X coordinate of menu position. */
    int y_pos,				/* Y coordinate of menu position. */
    unsigned int event_mask,		/* Mouse button event mask. */
    char **data,			/* Pointer to return data value. */
    void (*help_callback) (char const *, int, int)) /* Help callback.  */
{
    int status;				/* X routine call status. */
    int orig_x;				/* Upper left menu origin X coord. */
    int orig_y;				/* Upper left menu origin Y coord. */
    int ret_val;			/* Return value. */

    register XMPane *p_ptr;		/* Current XMPane. */
    register XMPane *event_xmp;		/* Event XMPane pointer. */
    register XMPane *cur_p;		/* Current pane. */
    register XMSelect *cur_s;		/* Current selection. */
    XMWindow *event_xmw;		/* Event XMWindow pointer. */
    XEvent event;			/* X input event. */
    XEvent peek_event;			/* X input peek ahead event. */

    Bool selection = False;		/* Selection has been made. */
    Bool forward = True;		/* Moving forward in the pane list. */

    Window root, child;
    int root_x, root_y, win_x, win_y;
    unsigned int mask;
    KeySym keysym;

    /*
     * Define and allocate a foreign event queue to hold events
     * that don't belong to XMenu.  These events are later restored
     * to the X event queue.
     */
    typedef struct _xmeventque {
	XEvent event;
	struct _xmeventque *next;
    } XMEventQue;

    XMEventQue *feq = NULL;    		/* Foreign event queue. */
    XMEventQue *feq_tmp;		/* Foreign event queue temporary. */

    /*
     * If there are no panes in the menu then return failure
     * because the menu is not initialized.
     */
    if (menu->p_count == 0) {
	_XMErrorCode = XME_NOT_INIT;
	return(XM_FAILURE);
    }

    /*
     * Find the desired current pane.
     */
    cur_p = _XMGetPanePtr(menu, *p_num);
    if (cur_p == NULL) {
	return(XM_FAILURE);
    }
    cur_p->activated = cur_p->active;

    /*
     * Find the desired current selection.
     * If the current selection index is out of range a null current selection
     * will be assumed and the cursor will be placed in the current pane
     * header.
     */
    cur_s = _XMGetSelectionPtr(cur_p, *s_num);

    /*
     * Compute origin of menu so that cursor is in
     * Correct pane and selection.
     */
    _XMTransToOrigin(display,
		     menu,
		     cur_p, cur_s,
		     x_pos, y_pos,
		     &orig_x, &orig_y);
    menu->x_pos = orig_x;	/* Store X and Y coords of menu. */
    menu->y_pos = orig_y;

    if (XMenuRecompute(display, menu) == XM_FAILURE) {
	return(XM_FAILURE);
    }

    /*
     * Flush the window creation queue.
     * This batches all window creates since lazy evaluation
     * is more efficient than individual evaluation.
     * This routine also does an XFlush().
     */
    if (_XMWinQueFlush(display, menu, cur_p, cur_s) == _FAILURE) {
	return(XM_FAILURE);
    }

    /*
     * Make sure windows are in correct order (in case we were passed
     * an already created menu in incorrect order.)
     */
    for(p_ptr = menu->p_list->next; p_ptr != cur_p; p_ptr = p_ptr->next)
	XRaiseWindow(display, p_ptr->window);
    for(p_ptr = menu->p_list->prev; p_ptr != cur_p->prev; p_ptr = p_ptr->prev)
	XRaiseWindow(display, p_ptr->window);

    /*
     * Make sure all selection windows are mapped.
     */
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->next
    ){
	XMapSubwindows(display, p_ptr->window);
    }

    /*
     * Synchronize the X buffers and the event queue.
     * From here on, all events in the queue that don't belong to
     * XMenu are sent back to the application via an application
     * provided event handler or discarded if the application has
     * not provided an event handler.
     */
    XSync(display, 0);

    /*
     * Grab the mouse for menu input.
     */

    status = XGrabPointer(
			  display,
			  menu->parent,
			  True,
			  event_mask,
			  GrabModeAsync,
			  GrabModeAsync,
			  None,
			  menu->mouse_cursor,
			  CurrentTime
			  );
    if (status == Success && x_menu_grab_keyboard)
      {
        status = XGrabKeyboard (display,
                                menu->parent,
                                False,
                                GrabModeAsync,
                                GrabModeAsync,
                                CurrentTime);
        if (status != Success)
          XUngrabPointer(display, CurrentTime);
      }

    if (status == _X_FAILURE) {
	_XMErrorCode = XME_GRAB_MOUSE;
	return(XM_FAILURE);
    }

    /*
     * Map the menu panes.
     */
    XMapWindow(display, cur_p->window);
    for (p_ptr = menu->p_list->next;
	 p_ptr != cur_p;
	 p_ptr = p_ptr->next)
      XMapWindow(display, p_ptr->window);
    for (p_ptr = cur_p->next;
	 p_ptr != menu->p_list;
	 p_ptr = p_ptr->next)
      XMapWindow(display, p_ptr->window);

    XRaiseWindow(display, cur_p->window);	/* Make sure current */
						/* pane is on top. */

    cur_s = NULL;			/* Clear current selection. */

    /*
     * Begin event processing loop.
     */
    while (1) {
        if (wait_func) (*wait_func) (wait_data);
	XNextEvent(display, &event);	/* Get next event. */
	switch (event.type) {		/* Dispatch on the event type. */
    case Expose:
	    event_xmp = (XMPane *)XLookUpAssoc(display,
					       menu->assoc_tab,
					       event.xexpose.window);
	    if (event_xmp == NULL) {
		/*
		 * If AEQ mode is enabled then queue the event.
		 */
		if (menu->aeq) {
		    feq_tmp = (XMEventQue *)malloc(sizeof(XMEventQue));
		    if (feq_tmp == NULL) {
			_XMErrorCode = XME_CALLOC;
			return(XM_FAILURE);
		    }
		    feq_tmp->event = event;
		    feq_tmp->next = feq;
		    feq = feq_tmp;
		}
		else if (_XMEventHandler) (*_XMEventHandler)(&event);
		break;
	    }
	    if (event_xmp->activated) {
		XSetWindowBackground(display,
				     event_xmp->window,
				     menu->bkgnd_color);
	    }
	    else {
		XSetWindowBackgroundPixmap(display,
					   event_xmp->window,
					   menu->inact_pixmap);
	    }
	    _XMRefreshPane(display, menu, event_xmp);
	    break;
    case EnterNotify:
	    /*
	     * First wait a small period of time, and see
	     * if another EnterNotify event follows hard on the
	     * heels of this one. i.e., the user is simply
	     * "passing through". If so, ignore this one.
	     */

	    event_xmw = (XMWindow *)XLookUpAssoc(display,
						 menu->assoc_tab,
						 event.xcrossing.window);
	    if (event_xmw == NULL) break;
	    if (event_xmw->type == SELECTION) {
		/*
		 * We have entered a selection.
		 */
		/* if (XPending(display) == 0) usleep(150000); */
		if (XPending(display) != 0) {
		    XPeekEvent(display, &peek_event);
		    if(peek_event.type == LeaveNotify) {
			break;
		    }
		}
		cur_s = (XMSelect *)event_xmw;
		help_callback (cur_s->help_string,
			       cur_p->serial, cur_s->serial);

		/*
		 * If the pane we are in is active and the
		 * selection entered is active then activate
		 * the selection.
		 */
		if (cur_p->active && cur_s->active > 0) {
		    cur_s->activated = 1;
		    _XMRefreshSelection(display, menu, cur_s);
		}
	    }
	    else {
		/*
		 * We have entered a pane.
		 */
		/* if (XPending(display) == 0) usleep(150000); */
		if (XPending(display) != 0) {
		    XPeekEvent(display, &peek_event);
		    if (peek_event.type == EnterNotify) break;
		}
		XQueryPointer(display,
			      menu->parent,
			      &root, &child,
			      &root_x, &root_y,
			      &win_x, &win_y,
			      &mask);
		event_xmp = (XMPane *)XLookUpAssoc(display,
						   menu->assoc_tab,
						   child);
		if (event_xmp == NULL) break;
		if (event_xmp == cur_p) break;
		if (event_xmp->serial > cur_p->serial) forward = True;
		else forward = False;
		p_ptr = cur_p;
		while (p_ptr != event_xmp) {
		    if (forward) p_ptr = p_ptr->next;
		    else p_ptr = p_ptr->prev;
		    XRaiseWindow(display, p_ptr->window);
		}
		if (cur_p->activated) {
		    cur_p->activated = False;
		    XSetWindowBackgroundPixmap(display,
					       cur_p->window,
					       menu->inact_pixmap);
		    _XMRefreshPane(display, menu, cur_p);
		}
		if (event_xmp->active) event_xmp->activated = True;
#if 1
		/*
		 * i suspect the we don't get an EXPOSE event when backing
		 * store is enabled; the menu windows content is probably
		 * not drawn in when it should be in that case.
		 * in that case, this is probably an ugly fix!
		 * i hope someone more familiar with this code would
		 * take it from here.  -- caveh@eng.sun.com.
		 */
		XSetWindowBackground(display,
				     event_xmp->window,
				     menu->bkgnd_color);
		_XMRefreshPane(display, menu, event_xmp);
#endif
		cur_p = event_xmp;
	    }
	    break;
    case LeaveNotify:
	    event_xmw = (XMWindow *)XLookUpAssoc(
						 display,
						 menu->assoc_tab,
						 event.xcrossing.window
						 );
	    if (event_xmw == NULL) break;
	    if(cur_s == NULL) break;

	    /*
	     * If the current selection was activated then
	     * deactivate it.
	     */
	    if (cur_s->activated) {
		cur_s->activated = False;
		_XMRefreshSelection(display, menu, cur_s);
	    }
	    cur_s = NULL;
	    break;

    case ButtonPress:
    case ButtonRelease:
		*p_num = cur_p->serial;
		/*
		 * Check to see if there is a current selection.
		 */
		if (cur_s != NULL) {
		    /*
		     * Set the selection number to the current selection.
		     */
		    *s_num = cur_s->serial;
		    /*
		     * If the current selection was activated then
		     * we have a valid selection otherwise we have
		     * an inactive selection.
		     */
		    if (cur_s->activated) {
			*data = cur_s->data;
			ret_val = XM_SUCCESS;
		    }
		    else {
			ret_val = XM_IA_SELECT;
		    }
		}
		else {
		    /*
		     * No selection was current.
		     */
		    ret_val = XM_NO_SELECT;
		}
		selection = True;
		break;
        case KeyPress:
        case KeyRelease:
                keysym = XLookupKeysym (&event.xkey, 0);

                /* Pop down on C-g and Escape.  */
                if ((keysym == XK_g && (event.xkey.state & ControlMask) != 0)
                    || keysym == XK_Escape) /* Any escape, ignore modifiers.  */
                  {
                    ret_val = XM_NO_SELECT;
                    selection = True;
                  }
               break;
	    default:
		/*
		 * If AEQ mode is enabled then queue the event.
		 */
		if (menu->aeq) {
		    feq_tmp = (XMEventQue *)malloc(sizeof(XMEventQue));
		    if (feq_tmp == NULL) {
			_XMErrorCode = XME_CALLOC;
			return(XM_FAILURE);
		    }
		    feq_tmp->event = event;
		    feq_tmp->next = feq;
		    feq = feq_tmp;
		}
		else if (_XMEventHandler) (*_XMEventHandler)(&event);
	}
	/*
	 * If a selection has been made, break out of the event loop.
	 */
	if (selection == True) break;
    }

    /*
     * Unmap the menu.
     */
    for ( p_ptr = menu->p_list->next;
	 p_ptr != menu->p_list;
	 p_ptr = p_ptr->next)
      {
	  XUnmapWindow(display, p_ptr->window);
      }

    /*
     * Ungrab the mouse.
     */
    XUngrabPointer(display, CurrentTime);
    XUngrabKeyboard(display, CurrentTime);

    /*
     * Restore bits under where the menu was if we managed
     * to save them and free the pixmap.
     */

    /*
     * If there is a current selection deactivate it.
     */
    if (cur_s != NULL) cur_s->activated = 0;

    /*
     * Deactivate the current pane.
     */
    cur_p->activated = 0;
    XSetWindowBackgroundPixmap(display, cur_p->window, menu->inact_pixmap);

    /*
     * Synchronize the X buffers and the X event queue.
     */
    XSync(display, 0);

    /*
     * Dispatch any events remaining on the queue.
     */
    while (QLength(display)) {
	/*
	 * Fetch the next event.
	 */
	XNextEvent(display, &event);

	/*
	 * Discard any events left on the queue that belong to XMenu.
	 * All others are held and then returned to the event queue.
	 */
	switch (event.type) {
	    case Expose:
	    case EnterNotify:
	    case LeaveNotify:
	    case ButtonPress:
	    case ButtonRelease:
		/*
		 * Does this event belong to one of XMenu's windows?
		 * If so, discard it and process the next event.
		 * If not fall through and treat it as a foreign event.
		 */
		event_xmp = (XMPane *)XLookUpAssoc(
						   display,
						   menu->assoc_tab,
						   event.xbutton.window
						   );
		if (event_xmp != NULL) continue;
	    default:
		/*
		 * This is a foreign event.
		 * Queue it for later return to the X event queue.
		 */
		feq_tmp = (XMEventQue *)malloc(sizeof(XMEventQue));
		if (feq_tmp == NULL) {
		    _XMErrorCode = XME_CALLOC;
		    return(XM_FAILURE);
		}
		feq_tmp->event = event;
		feq_tmp->next = feq;
		feq = feq_tmp;
	    }
    }
    /*
     * Return any foreign events that were queued to the X event queue.
     */
    while (feq != NULL) {
	feq_tmp = feq;
	XPutBackEvent(display, &feq_tmp->event);
	feq = feq_tmp->next;
	free((char *)feq_tmp);
    }

    wait_func = 0;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(ret_val);

}
