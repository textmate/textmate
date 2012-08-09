/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuInsertPane - Inserts a pane into an XMenu object in
 *			  a particular position.
 *
 *	Author:		Tony Della Fera, DEC
 *			20-Nov-85
 *
 */

#include <config.h>
#include "XMenuInt.h"

int
XMenuInsertPane(register XMenu *menu, register int p_num, char *label, int active)
                         	/* Menu object to be modified. */
                       		/* Pane number of new pane. */
                		/* Selection label. */
               			/* Make selection active? */
{
    register XMPane *p_ptr;	/* XMPane pointer. */
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
     * Find the pane number one less than the one specified since that
     * is the pane after which the insertion will occur.
     */
    p_ptr = _XMGetPanePtr(menu, (p_num - 1));
    if (p_ptr == NULL) return(XM_FAILURE);

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
    label_width = XTextWidth(menu->p_fnt_info, label, label_length);

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
     */
    pane->type = PANE;
    pane->active = active;
    pane->serial = -1;
    pane->label = label;
    pane->label_width = label_width;
    pane->label_length = label_length;
    pane->s_list = sel;

    /*
     * Insert the pane after the pane with the pane
     * number one less than the desired number for the
     * new pane.
     */
    emacs_insque(pane, p_ptr);

    /*
     * Update the pane count.
     */
    menu->p_count++;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the number of the pane just added.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(p_num);
}
