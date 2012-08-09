/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"

/*
Copyright (C) 1993, 1996, 2001-2012  Free Software Foundation, Inc.

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
 * 	XMenuInternal.c - XMenu internal (not user visible) routines.
 *
 *	Author:		Tony Della Fera, DEC
 *			November, 1985
 *
 */

#include <config.h>
#include "XMenuInt.h"

/*
 * Internal Window creation queue sizes.
 */
#define S_QUE_SIZE	300
#define P_QUE_SIZE	20


/*
 * XMWinQue - Internal window creation queue datatype.
 */
typedef struct _xmwinquedef {
    int sq_size;
    XMSelect *sq[S_QUE_SIZE];
    XMSelect **sq_ptr;
    int pq_size;
    XMPane *pq[P_QUE_SIZE];
    XMPane **pq_ptr;
} XMWinQue;

/*
 * _XMWinQue - Internal static window creation queue.
 */
static Bool _XMWinQueIsInit = False;
static XMWinQue _XMWinQue;

/*
 * _XMErrorCode - Global XMenu error code.
 */
int _XMErrorCode = XME_NO_ERROR;
/*
 * _XMErrorList - Global XMenu error code description strings.
 */
char const *const
_XMErrorList[XME_CODE_COUNT] = {
    "No error",				/* XME_NO_ERROR */
    "Menu not initialized",		/* XME_NOT_INIT */
    "Argument out of bounds",		/* XME_ARG_BOUNDS */
    "Pane not found",			/* XME_P_NOT_FOUND */
    "Selection not found",		/* XME_S_NOT_FOUND */
    "Invalid menu style parameter",	/* XME_STYLE_PARAM */
    "Unable to grab mouse",		/* XME_GRAB_MOUSE */
    "Unable to interpret locator",	/* XME_INTERP_LOC */
    "Unable to calloc memory",		/* XME_CALLOC */
    "Unable to create XAssocTable",	/* XME_CREATE_ASSOC */
    "Unable to store bitmap",		/* XME_STORE_BITMAP */
    "Unable to make tile pixmaps",	/* XME_MAKE_TILES */
    "Unable to make pixmap",		/* XME_MAKE_PIXMAP */
    "Unable to create cursor",		/* XME_CREATE_CURSOR */
    "Unable to open font",		/* XME_OPEN_FONT */
    "Unable to create windows",		/* XME_CREATE_WINDOW */
    "Unable to create transparencies",	/* XME_CREATE_TRANSP */
};

/*
 * _XMEventHandler - Internal event handler variable.
 */
int (*_XMEventHandler)(XEvent*) = NULL;



/*
 * _XMWinQueInit - Internal routine to initialize the window
 *		   queue.
 */
void
_XMWinQueInit(void)
{
    /*
     * If the queue is not initialized initialize it.
     */
    if (!_XMWinQueIsInit) {
	/*
	 * Blank the queue structure.
	 */
	register int i;

	for (i = 0; i < S_QUE_SIZE; i++)
	  _XMWinQue.sq[i] = 0;

	for (i = 0; i < P_QUE_SIZE; i++)
	  _XMWinQue.pq[i] = 0;

	_XMWinQue.sq_size = _XMWinQue.pq_size = 0;

	/*
	 * Initialize the next free location pointers.
	 */
	_XMWinQue.sq_ptr = _XMWinQue.sq;
	_XMWinQue.pq_ptr = _XMWinQue.pq;
    }
}



/*
 * _XMWinQueAddPane - Internal routine to add a pane to the pane
 *		      window queue.
 */
int
_XMWinQueAddPane(register Display *display, register XMenu *menu, register XMPane *p_ptr)

                         	/* Menu being manipulated. */
                           	/* XMPane being queued. */
{
    /*
     * If the queue is currently full then flush it.
     */
    if (_XMWinQue.pq_size == P_QUE_SIZE) {
	if (_XMWinQueFlush(display, menu, 0, 0) == _FAILURE) return(_FAILURE);
    }

    /*
     * Insert the new XMPane pointer and increment the queue pointer
     * and the queue size.
     */
    *_XMWinQue.pq_ptr = p_ptr;
    _XMWinQue.pq_ptr++;
    _XMWinQue.pq_size++;

    /*
     * All went well, return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(_SUCCESS);
}



/*
 * _XMWinQueAddSelection - Internal routine to add a selection to
 *			   the selection window queue.
 */
int
_XMWinQueAddSelection(register Display *display, register XMenu *menu, register XMSelect *s_ptr)

                         	/* Menu being manipulated. */
                             	/* XMSelection being queued. */
{
    /*
     * If this entry will overflow the queue then flush it.
     */
    if (_XMWinQue.sq_size == S_QUE_SIZE) {
	if (_XMWinQueFlush(display, menu, 0, 0) == _FAILURE) return(_FAILURE);
    }

    /*
     * Insert the new XMSelect pointer and increment the queue pointer
     * and the queue size.
     */
    *_XMWinQue.sq_ptr = s_ptr;
    _XMWinQue.sq_ptr++;
    _XMWinQue.sq_size++;

    /*
     * All went well, return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(_SUCCESS);
}



/*
 * _XMWinQueFlush - Internal routine to flush the pane and
 *		    selection window queues.
 */
int
_XMWinQueFlush(register Display *display, register XMenu *menu, register XMPane *pane, XMSelect *sel)

                         		/* Menu being manipulated. */
                          		/* Current pane. */
{
    register int pq_index;		/* Pane queue index. */
    register int sq_index;		/* Selection queue index. */
    register XMPane *p_ptr;		/* XMPane pointer. */
    register XMSelect *s_ptr;   	/* XMSelect pointer. */
    unsigned long valuemask;    	/* Which attributes to set. */
    XSetWindowAttributes attributes_buf; /* Attributes to be set. */
    XSetWindowAttributes *attributes = &attributes_buf;

    /*
     * If the pane window queue is not empty...
     */

    if (_XMWinQue.pq_size > 0) {
	/*
	 * set up attributes for pane window to be created.
	 */
	valuemask = (CWBackPixmap | CWBorderPixel | CWOverrideRedirect);
	attributes->border_pixel = menu->p_bdr_color;
	attributes->background_pixmap = menu->inact_pixmap;
	attributes->override_redirect = True;

	/*
	 * Create all the pending panes in order, so that the
	 * current pane will be on top, with the others
	 * stacked appropriately under it.
	 */
	for (pq_index = _XMWinQue.pq_size - 1;
	     pq_index >= 0;
	     pq_index--)
	  {
	      p_ptr = _XMWinQue.pq[pq_index];  /* Retrieve next pane. */
	      if (p_ptr == pane) break;
	      p_ptr->window = XCreateWindow(display,
					    menu->parent,
					    p_ptr->window_x,
					    p_ptr->window_y,
					    p_ptr->window_w,
					    p_ptr->window_h,
					    menu->p_bdr_width,
					    CopyFromParent,
					    InputOutput,
					    CopyFromParent,
					    valuemask,
					    attributes);
	      XMakeAssoc(display, menu->assoc_tab, p_ptr->window, p_ptr);
	      XSelectInput(display, p_ptr->window, menu->p_events);
	  }
	for (pq_index = 0;
	     pq_index < _XMWinQue.pq_size;
	     pq_index++)
	  {
	      p_ptr = _XMWinQue.pq[pq_index];	/* Retrieve next pane. */
	      p_ptr->window = XCreateWindow(display,
					    menu->parent,
					    p_ptr->window_x,
					    p_ptr->window_y,
					    p_ptr->window_w,
					    p_ptr->window_h,
					    menu->p_bdr_width,
					    CopyFromParent,
					    InputOutput,
					    CopyFromParent,
					    valuemask,
					    attributes);
	      XMakeAssoc(display, menu->assoc_tab, p_ptr->window, p_ptr);
	      XSelectInput(display, p_ptr->window, menu->p_events);
	      if (p_ptr == pane) break;
	}

	/*
	 * Reset the pane queue pointer and size.
	 */
	_XMWinQue.pq_size = 0;
	_XMWinQue.pq_ptr = _XMWinQue.pq;
    }

    /*
     * If the selection window queue is not empty...
     */

    if (_XMWinQue.sq_size > 0) {

	for (sq_index = 0; sq_index < _XMWinQue.sq_size; sq_index++) {
	    /*
	     * Retrieve the XMSelect pointer.
	     */
	    s_ptr = _XMWinQue.sq[sq_index];
	    s_ptr->window = XCreateWindow(display,
				   s_ptr->parent_p->window,
				   s_ptr->window_x,
				   s_ptr->window_y,
				   s_ptr->window_w,
				   s_ptr->window_h,
				   0,                /* border width*/
				   CopyFromParent,
				   InputOnly,
				   CopyFromParent,
				   0,
				   attributes);

	    /*
	     * Insert the new window id and its
	     * associated XMSelect structure into the
	     * association table.
	     */
	    XMakeAssoc(display, menu->assoc_tab, s_ptr->window, s_ptr);
	    XSelectInput(display, s_ptr->window, menu->s_events);
	}

	/*
	 * Reset the selection queue pointer and size.
	 */
	_XMWinQue.sq_size = 0;
	_XMWinQue.sq_ptr = _XMWinQue.sq;
    }

    /*
     * Flush X's internal queues.
     */
    XFlush(display);

    /*
     * All went well, return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(_SUCCESS);
}



/*
 * _XMGetPanePtr - 	Given a menu pointer and a pane index number, return
 *			a pane pointer that points to the indexed pane.
 */
XMPane *
_XMGetPanePtr(register XMenu *menu, register int p_num)
                         	/* Menu to find the pane in. */
                       		/* Index number of pane to find. */
{
    register XMPane *p_ptr;	/* Pane pointer to be returned. */
    register int i;		/* Loop counter. */

    /*
     * Is the pane number out of range?
     */
    if ((p_num < 0) || (p_num > (menu->p_count - 1))) {
	_XMErrorCode = XME_P_NOT_FOUND;
	return(NULL);
    }

    /*
     * Find the right pane.
     */
    p_ptr = menu->p_list->next;
    for (i = 0; i < p_num; i++) p_ptr = p_ptr->next;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(p_ptr);
}



/*
 * _XMGetSelectionPtr -	Given pane pointer and a selection index number,
 *			return a selection pointer that points to the
 *			indexed selection.
 */
XMSelect *
_XMGetSelectionPtr(register XMPane *p_ptr, register int s_num)
                           	/* Pane to find the selection in. */
                       		/* Index number of the selection to find. */
{
    register XMSelect *s_ptr;	/* Selection pointer to be returned. */
    register int i;		/* Loop counter. */

    /*
     * Is the selection number out of range?
     */
    if ((s_num < 0) || (s_num > (p_ptr->s_count - 1))) {
	_XMErrorCode = XME_S_NOT_FOUND;
	return(NULL);
    }

    /*
     * Find the right selection.
     */
    s_ptr = p_ptr->s_list->next;
    for (i = 0; i < s_num; i++) s_ptr = s_ptr->next;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_ptr);
}



/*
 * _XMRecomputeGlobals - Internal subroutine to recompute menu wide
 *			 global values.
 */
void
_XMRecomputeGlobals(register Display *display, register XMenu *menu)
                               /*X11 display variable. */
                         	/* Menu object to compute from. */
{
    register XMPane *p_ptr;	/* Pane pointer. */
    register XMSelect *s_ptr;	/* Selection pointer. */

    register int max_p_label = 0;	/* Maximum pane label width. */
    register int max_s_label = 0;	/* Maximum selection label width. */
    register int s_count = 0;		/* Maximum selection count. */

    int p_s_pad;		/* Pane <-> selection padding. */
    int p_s_diff;		/* Pane <-> selection separation. */

    int p_height;		/* Pane window height. */
    int p_width;		/* Pane window width. */
    int s_width;		/* Selection window width. */

    int screen;			/* DefaultScreen holder. */

    /*
     * For each pane...
     */
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->next
    ){

	/*
	 * Recompute maximum pane label width.
	 */
	max_p_label = max(max_p_label, p_ptr->label_width);

	/*
	 * Recompute maximum selection count.
	 */
	s_count = max(s_count, p_ptr->s_count);

	/*
	 * For each selection in the current pane...
	 */
	for (
	    s_ptr = p_ptr->s_list->next;
	    s_ptr != p_ptr->s_list;
	    s_ptr = s_ptr->next
	){

	    /*
	     * Recompute maximum selection label width.
	     */
	    max_s_label = max(max_s_label, s_ptr->label_width);
	}
    }

    /*
     * Recompute pane height.
     */
    p_height = (menu->flag_height << 1) + (menu->s_y_off * s_count);

    /*
     * Recompute horizontal padding between the pane window and the
     * selection windows.
     */
    p_s_pad = menu->p_x_off << 1;

    /*
     * Recompute pane and selection window widths.
     * This is done by first computing the window sizes from the maximum
     * label widths.  If the spacing between the selection window and the
     * containing pane window is less than the pane selection padding value
     * (twice the pane X offset) then change the size of the pane to be
     * the size of the selection window plus the padding.  If, however the
     * spacing between the selection window and the containing pane window
     * is more than the pane selection padding value increase the size of
     * the selection to its maximum possible value (the pane width minus
     * the pane selection padding value).
     */
    p_width = max_p_label + p_s_pad;
    s_width = max_s_label + (menu->s_fnt_pad << 1) + (menu->s_bdr_width << 1);
    p_s_diff = p_width - s_width;
    if (p_s_diff < p_s_pad) {
	p_width = s_width + p_s_pad;
    }
    else if (p_s_diff > p_s_pad) {
	s_width = p_width - p_s_pad;
    }

    /*
     * Reset menu wide global values.
     */
    menu->s_count = s_count;
    menu->p_height = p_height;
    menu->p_width = p_width;
    menu->s_width = s_width;

    /*
     * Ensure that the origin of the menu is placed so that
     * None of the panes ore selections are off the screen.
     */
    screen = DefaultScreen(display);
    if (menu->x_pos + menu->width > DisplayWidth(display, screen))
	    menu->x_pos = DisplayWidth(display, screen) - menu->width;
    else if (menu->x_pos < 0) menu->x_pos = 0;
    if(menu->y_pos + menu->height > DisplayHeight(display, screen))
	    menu->y_pos = DisplayHeight(display, screen) - menu->height;
    else if (menu->y_pos < 0) menu->y_pos = 0;
}


/*
 * _XMRecomputePane - Internal subroutine to recompute pane
 *		      window dependencies.
 */
int
_XMRecomputePane(register Display *display, register XMenu *menu, register XMPane *p_ptr, register int p_num)
                              	/* Standard X display variable. */
                         	/* Menu object being recomputed. */
                           	/* Pane pointer. */
                       		/* Pane sequence number. */
{
    register int window_x;	/* Recomputed window X coordinate. */
    register int window_y;	/* Recomputed window Y coordinate. */

    unsigned long change_mask;	/* Value mask to reconfigure window. */
    XWindowChanges *changes;	/* Values to use in configure window. */

    register Bool config_p = False;	/* Reconfigure pane window? */

    /*
     * Update the pane serial number.
     */
    p_ptr->serial = p_num;

    /*
     * Recompute window X and Y coordinates.
     */
    switch (menu->menu_style) {
	case LEFT:
	    window_x = menu->p_x_off * ((menu->p_count - 1) - p_num);
	    window_y = menu->p_y_off * ((menu->p_count - 1) - p_num);
	    break;
	case RIGHT:
	    window_x = menu->p_x_off * p_num;
	    window_y = menu->p_y_off * ((menu->p_count - 1) - p_num);
	    break;
	case CENTER:
	    window_x = 0;
	    window_y = menu->p_y_off * ((menu->p_count - 1) - p_num);
	    break;
	default:
	    /* Error! Invalid style parameter. */
	    _XMErrorCode = XME_STYLE_PARAM;
	    return(_FAILURE);
    }
    window_x += menu->x_pos;
    window_y += menu->y_pos;

    /*
     * If the newly compute pane coordinates differ from the
     * current coordinates, reset the current coordinates and
     * reconfigure the pane.
     */
    if (
	(window_x != p_ptr->window_x) ||
	(window_y != p_ptr->window_y)
    ){
	/*
	 * Reset the coordinates and schedule
	 * the pane for reconfiguration.
	 */
	p_ptr->window_x = window_x;
	p_ptr->window_y = window_y;
	config_p = True;
    }

    /*
     * If the local pane width and height differs from the
     * menu pane width and height, reset the local values.
     */
    if (
	(p_ptr->window_w != menu->p_width) ||
	(p_ptr->window_h != menu->p_height)
    ){
	/*
	 * Reset window width and height and schedule
	 * the pane for reconfiguration.
	 */
	p_ptr->window_w = menu->p_width;
	p_ptr->window_h = menu->p_height;
	config_p = True;
    }

    /*
     * If we need to reconfigure the pane window do it now.
     */
    if (config_p == True) {
	/*
	 * If the pane window has already been created then
	 * reconfigure the existing window, otherwise queue
	 * it for creation with the new configuration.
	 */
	if (p_ptr->window) {
	    change_mask = (CWX | CWY | CWWidth | CWHeight);
	    changes = (XWindowChanges *)malloc(sizeof(XWindowChanges));
	    changes->x = p_ptr->window_x;
	    changes->y = p_ptr->window_y;
	    changes->width = p_ptr->window_w;
	    changes->height = p_ptr->window_h;

	    XConfigureWindow(
			     display,
			     p_ptr->window,
			     change_mask,
			     changes
			     );
	    free(changes);

	}
	else {
	    if (_XMWinQueAddPane(display, menu, p_ptr) == _FAILURE) {
		return(_FAILURE);
	    }
	}
    }

    /*
     * Recompute label X position.
     */
    switch (menu->p_style) {
	case LEFT:
	    p_ptr->label_x = menu->p_x_off + menu->p_fnt_pad;
	    break;
	case RIGHT:
	    p_ptr->label_x = menu->p_width -
		(p_ptr->label_width + menu->p_x_off + menu->p_fnt_pad);
	    break;
	case CENTER:
	    p_ptr->label_x = (menu->p_width - p_ptr->label_width) >> 1;
	    break;
	default:
	    /* Error! Invalid style parameter. */
	    _XMErrorCode = XME_STYLE_PARAM;
	    return(_FAILURE);
    }
    /*
     * Recompute label Y positions.
     */
    p_ptr->label_uy = menu->p_fnt_pad + menu->p_fnt_info->max_bounds.ascent;
    p_ptr->label_ly = (menu->p_height - menu->p_fnt_pad - menu->p_fnt_info->max_bounds.descent);

    /*
     * All went well, return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(_SUCCESS);
}



/*
 * _XMRecomputeSelection - Internal subroutine to recompute
 *			   selection window dependencies.
 */
int
_XMRecomputeSelection(register Display *display, register XMenu *menu, register XMSelect *s_ptr, register int s_num)

                         	/* Menu object being recomputed. */
                             	/* Selection pointer. */
                       		/* Selection sequence number. */
{
    register Bool config_s = False;	/* Reconfigure selection window? */
    XWindowChanges *changes;		/* Values to change in configure. */
    unsigned long change_mask;		/* Value mask for XConfigureWindow. */

    /*
     * If the selection serial numbers are out of order, begin
     * resequencing selections.  Recompute selection window coordinates
     * and serial number.
     *
     * When selections are created they are given a serial number of
     * -1, this causes this routine to give a new selection
     * its initial coordinates and serial number.
     */
    if (s_ptr->serial != s_num) {
	/*
	 * Fix the sequence number.
	 */
	s_ptr->serial = s_num;
	/*
	 * Recompute window X and Y coordinates.
	 */
	s_ptr->window_x = menu->s_x_off;
	s_ptr->window_y = menu->flag_height + (menu->s_y_off * s_num);
	/*
	 * We must reconfigure the window.
	 */
	config_s = True;
    }

    /*
     * If the local selection width and height differs from the
     * menu selection width and height, reset the local values.
     */
    if (
	(s_ptr->window_w != menu->s_width) ||
	(s_ptr->window_h != menu->s_height)
    ){
	/*
	 * We must reconfigure the window.
	 */
	config_s = True;
	/*
	 * Reset window width and height.
	 */
	s_ptr->window_w = menu->s_width;
	s_ptr->window_h = menu->s_height;
    }

    /*
     * If we need to reconfigure the selection window do it now.
     */
    if (config_s == True) {
	/*
	 * If the selection window has already been created then
	 * reconfigure the existing window, otherwise queue it
	 * for creation with the new configuration.
	 */
	if (s_ptr->window) {
	    changes = (XWindowChanges *)malloc(sizeof(XWindowChanges));
	    change_mask = (CWX | CWY | CWWidth | CWHeight);
	    changes = (XWindowChanges *)malloc(sizeof(XWindowChanges));
	    changes->x = s_ptr->window_x;
	    changes->y = s_ptr->window_y;
	    changes->width = s_ptr->window_w;
	    changes->height = s_ptr->window_h;

	    XConfigureWindow(
			     display,
			     s_ptr->window,
			     change_mask,
			     changes
			     );
	    free(changes);

	}
	else {
	    if (_XMWinQueAddSelection(display, menu, s_ptr) == _FAILURE) {
		return(_FAILURE);
	    }
	}
    }

    /*
     * Recompute label X position.
     */
    switch (menu->s_style) {
	case LEFT:
	    s_ptr->label_x = menu->s_bdr_width + menu->s_fnt_pad + s_ptr->window_x;
	    break;
	case RIGHT:
	    s_ptr->label_x = s_ptr->window_x + menu->s_width -
		(s_ptr->label_width + menu->s_bdr_width + menu->s_fnt_pad);
	    break;
	case CENTER:
	    s_ptr->label_x = s_ptr->window_x + ((menu->s_width - s_ptr->label_width) >> 1);
	    break;
	default:
	    /* Error! Invalid style parameter. */
	    _XMErrorCode = XME_STYLE_PARAM;
	    return(_FAILURE);
    }
    /*
     * Recompute label Y position.
     */
    s_ptr->label_y = s_ptr->window_y + menu->s_fnt_info->max_bounds.ascent + menu->s_fnt_pad + menu->s_bdr_width;

    /*
     * All went well, return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(_SUCCESS);
}



/*
 * _XMTransToOrigin - Internal subroutine to translate the point at
 *		      the center of the current pane and selection to the
 *		      the menu origin.
 *
 *	WARNING! ******	Be certain that all menu dependencies have been
 *			recomputed before calling this routine or
 *			unpredictable results will follow.
 */
void
_XMTransToOrigin(Display *display, register XMenu *menu, register XMPane *p_ptr, register XMSelect *s_ptr, int x_pos, int y_pos, int *orig_x, int *orig_y)
                     		/* Not used. Included for consistency. */
                         	/* Menu being computed against. */
                           	/* Current pane pointer. */
                             	/* Current selection pointer. */
              			/* X coordinate of point to translate. */
              			/* Y coordinate of point to translate. */
                		/* Return value X coord. of the menu origin. */
                		/* Return value Y coord. of the menu origin. */
{
    register int l_orig_x;	/* Local X coordinate of the menu origin. */
    register int l_orig_y;	/* Local Y coordinate of the menu origin. */

    /*
     * Translate the menu origin such that the cursor hot point will be in the
     * center of the desired current selection and pane.
     * If the current selection pointer is NULL then assume that the hot point
     * will be in the center of the current pane flag.
     */

    if (s_ptr == NULL) {
	/*
	 * Translate from the center of the pane flag to the upper left
	 * of the current pane window.
	 */
	l_orig_x = x_pos - (menu->p_width >> 1) - menu->p_bdr_width;
	l_orig_y = y_pos - (menu->flag_height >> 1) - menu->p_bdr_width;
    }
    else {
	/*
	 * First translate from the center of the current selection
	 * to the upper left of the current selection window.
	 */
	l_orig_x = x_pos - (menu->s_width >> 1);
	l_orig_y = y_pos - (menu->s_height >> 1);

	/*
	 * Then translate to the upper left of the current pane window.
	 */
	l_orig_x -= (s_ptr->window_x + menu->p_bdr_width);
	l_orig_y -= (s_ptr->window_y + menu->p_bdr_width);
    }

    /*
     * Finally translate to the upper left of the menu.
     */
    l_orig_x -= (p_ptr->window_x - menu->x_pos);
    l_orig_y -= (p_ptr->window_y - menu->y_pos);

    /*
     * Set the return values.
     */
    *orig_x = l_orig_x;
    *orig_y = l_orig_y;
}

/*
 * _XMRefreshPane - Internal subroutine to completely refresh
 *		    the contents of a pane.
 */
void
_XMRefreshPane(register Display *display, register XMenu *menu, register XMPane *pane)
{
    register XMSelect *s_list = pane->s_list;
    register XMSelect *s_ptr;

    /*
     * First clear the pane.
     */
    XClearWindow(display, pane->window);
    if (!pane->activated) {
	XFillRectangle(display,
		       pane->window,
		       menu->inverse_select_GC,
		       pane->label_x - menu->p_fnt_pad,
		       pane->label_uy - menu->p_fnt_info->max_bounds.ascent - menu->p_fnt_pad,
		       pane->label_width + (menu->p_fnt_pad << 1),
		       menu->flag_height);

	XFillRectangle(display,
		       pane->window,
		       menu->inverse_select_GC,
		       pane->label_x - menu->p_fnt_pad,
		       pane->label_ly - menu->p_fnt_info->max_bounds.ascent - menu->p_fnt_pad,
		       pane->label_width + (menu->p_fnt_pad << 1),
		       menu->flag_height);
    }
    if (!pane->active) {
	XDrawString(display,
		    pane->window,
		    menu->inact_GC,
		    pane->label_x, pane->label_uy,
		    pane->label, pane->label_length);
	XDrawString(display,
		    pane->window,
		    menu->inact_GC,
		    pane->label_x, pane->label_ly,
		    pane->label, pane->label_length);
    }
    else {
	XDrawString(display,
			 pane->window,
			 menu->pane_GC,
			 pane->label_x, pane->label_uy,
			 pane->label, pane->label_length);
	XDrawString(display,
			 pane->window,
			 menu->pane_GC,
			 pane->label_x, pane->label_ly,
			 pane->label, pane->label_length);

	/*
	 * Finally refresh each selection if the pane is activated.
	 */
	if (pane->activated) {
	    for (s_ptr = s_list->next; s_ptr != s_list; s_ptr = s_ptr->next)
		_XMRefreshSelection(display, menu, s_ptr);
	}
    }
}




/*
 * _XMRefreshSelection - Internal subroutine that refreshes
 *			 a single selection window.
 */
void
_XMRefreshSelection(register Display *display, register XMenu *menu, register XMSelect *sel)
{
    register int width = sel->window_w;
    register int height = sel->window_h;
    register int bdr_width = menu->s_bdr_width;

    if (sel->type == SEPARATOR) {
        XDrawLine(display,
                  sel->parent_p->window,
                  menu->normal_select_GC,
                  sel->window_x,
                  sel->window_y + height / 2,
                  sel->window_x + width,
                  sel->window_y + height / 2);
    }
    else if (sel->activated) {
	if (menu->menu_mode == INVERT) {
	    XFillRectangle(display,
			   sel->parent_p->window,
			   menu->normal_select_GC,
			   sel->window_x, sel->window_y,
			   width, height);
	    XDrawString(display,
			sel->parent_p->window,
			menu->inverse_select_GC,
			sel->label_x,
			sel->label_y,
			sel->label, sel->label_length);
	}
        else {
            /*
	     * Using BOX mode.
             * Since most drawing routines with arbitrary width lines
	     * are slow compared to raster-ops let's use a raster-op to
	     * draw the boxes.
             */

	    XDrawRectangle(display,
			   sel->parent_p->window,
			   menu->normal_select_GC,
			   sel->window_x + (bdr_width >> 1),
			   sel->window_y + (bdr_width >> 1 ),
			   width - bdr_width,
			   height - bdr_width);
	    XDrawString(display,
			sel->parent_p->window,
			menu->normal_select_GC,
			sel->label_x,
			sel->label_y,
			sel->label, sel->label_length);
        }
    }
    else {
	XClearArea(display,
		   sel->parent_p->window,
		   sel->window_x, sel->window_y,
		   width, height,
		   False);
	if (sel->active) {
	    XDrawString(display,
			sel->parent_p->window,
			menu->normal_select_GC,
			sel->label_x,
			sel->label_y,
			sel->label, sel->label_length);
	}
	else {
	    XDrawString(display,
			sel->parent_p->window,
			menu->inact_GC,
			sel->label_x,
			sel->label_y,
			sel->label, sel->label_length);
	}
    }
}
