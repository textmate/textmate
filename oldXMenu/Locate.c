/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuLocate - Return data necessary to position and locate
 *		      a menu on the screen.
 *
 *	Author:		Tony Della Fera, DEC
 *			January 11, 1985
 *
 */

#include "XMenuInt.h"

int
XMenuLocate(register Display *display, register XMenu *menu, int p_num, int s_num, int x_pos, int y_pos, int *ul_x, int *ul_y, int *width, int *height)
                              	/* Previously opened display. */
                         	/* Menu object being located. */
              			/* Active pane number. */
              			/* Active selection number. */
              			/* X coordinate of mouse active position. */
              			/* Y coordinate of mouse active position. */
              			/* Returned upper left menu X coordinate. */
              			/* Returned upper left menu Y coordinate. */
               			/* Returned menu width. */
                		/* Returned menu height. */
{
    register XMPane *p_ptr;	/* XMPane pointer. */
    register XMSelect *s_ptr;	/* XMSelect pointer. */

    /*
     * Are the position arguments positive?
     */
    if ((x_pos <= 0) || (y_pos <= 0)) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }

    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Find the right selection.
     */
      s_ptr = _XMGetSelectionPtr(p_ptr, s_num);

    /*
     * Check to see that the menu's dependencies have been
     * recomputed and are up to date.  If not, do it now.
     */
    if (menu->recompute) XMenuRecompute(display, menu);

    /*
     * Compute the new menu origin such that the active point lies
     * in the center of the desired active pane and selection.
     * This sets the values of ul_x and ul_y.
     */
    _XMTransToOrigin(display, menu, p_ptr, s_ptr, x_pos, y_pos, ul_x, ul_y);

    /*
     * Set remaining return argument values.
     */
    *width = menu->width;
    *height = menu->height;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(XM_SUCCESS);
}

