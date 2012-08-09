/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuPost -	Maps a given menu to the display and activates
 *			the menu for user selection.  The user is allowed to
 *			specify the mouse button event mask that will be used
 *			to identify a selection request.  When a selection
 *			request is received (i.e., when the specified mouse
 *			event occurs) the data  returned will be either the
 *			data associated with the particular selection active
 *			at the time of the selection request or NULL if no
 *			selection was active.  A menu selection is shown to
 *			be active by placing a highlight box around the
 *			selection as the mouse cursor enters its active
 *			region.  Inactive selections will not be highlighted.
 *			As the mouse cursor moved from one menu pane
 *			to another menu pane the pane being entered is raised
 *			and activated and the pane being left is deactivated.
 *			If an error occurs NULL will be returned with the
 *			p_num set to POST_ERROR, s_num set to
 *			NO_SELECTION and _XMErrorCode set to an
 *			appropriate value.
 *			Every time the routine returns successfully the
 *			p_num and s_num indices will be set to indicate
 *			the currently active pane and/or selection.  If the
 *			mouse was not in a selection window at the time
 *			s_num will be set to NO_SELECTION.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1984
 *
 */

#include "XMenuInt.h"

char *
XMenuPost(register Display *display, register XMenu *menu, register int *p_num, register int *s_num, register int x_pos, register int y_pos, int event_mask)
                              	/* Previously opened display. */
                         	/* Menu to post. */
                        	/* Pane number selected. */
                        	/* Selection number selected. */
                       		/* X coordinate of menu position. */
                       		/* Y coordinate of menu position. */
                   		/* Mouse button event mask. */
{
    register int stat;		/* Routine call return status. */
    char *data;			/* Return data. */

    /*
     * Set up initial pane and selection assumptions.
     */

    /*
     * Make the procedure call.
     */
    stat = XMenuActivate(
			 display,
			 menu,
			 p_num, s_num,
			 x_pos, y_pos,
			 event_mask,
			 &data, 0);

    /*
     * Check the return value and return accordingly.
     */
    switch (stat) {
	case XM_FAILURE:
	    *p_num = POST_ERROR;
	    *s_num = NO_SELECTION;
	    return(NULL);
	case XM_NO_SELECT:
	case XM_IA_SELECT:
	    *s_num = NO_SELECTION;
	    return(NULL);
	case XM_SUCCESS:
	default:
	    return(data);
    }
}

