/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuAddPane - Adds a pane to an XMenu object.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include <config.h>
#include "XMenuInt.h"

int
XMenuAddPane(Display *display, register XMenu *menu, register char const *label, int active)

                         	/* Menu object to be modified. */
                         	/* Selection label. */
               			/* Make selection active? */
{
    register XMPane *pane;	/* Newly created pane. */
    register XMSelect *sel;	/* Initial selection for the new pane. */

    int label_length;		/* Label length in characters. */
    int label_width;		/* Label width in pixels. */

    /*
     * Check for NULL pointers!
     */
    if (label == NULL) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }

    /*
     * Calloc the XMPane structure and the initial XMSelect.
     */
    pane = (XMPane *)calloc(1, sizeof(XMPane));
    if (pane == NULL) {
	_XMErrorCode = XME_CALLOC;
	return(XM_FAILURE);
    }
    sel = (XMSelect *)calloc(1, sizeof(XMSelect));
    if (sel == NULL) {
	_XMErrorCode = XME_CALLOC;
	return(XM_FAILURE);
    }

    /*
     * Determine label size.
     */
    label_length = strlen(label);
    label_width = XTextWidth(menu->p_fnt_info,
			     label,
			     label_length);

    /*
     * Set up the initial selection.
     * Values not explicitly set are zeroed by calloc.
     */
    sel->next = sel;
    sel->prev = sel;
    sel->type = SL_HEADER;
    sel->serial = -1;
    sel->parent_p = pane;

    /*
     * Fill the XMPane structure.
     * X and Y position are set to 0 since a recompute will follow.
     */
    pane->type = PANE;
    pane->active = active;
    pane->serial = -1;
    pane->label = label;
    pane->label_width = label_width;
    pane->label_length = label_length;
    pane->s_list = sel;

    /*
     * Insert the pane at the end of the pane list.
     */
    emacs_insque(pane, menu->p_list->prev);

    /*
     * Update the pane count.
     */
    menu->p_count++;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the pane number just added.
     */
    _XMErrorCode = XME_NO_ERROR;
    return((menu->p_count - 1));
}
