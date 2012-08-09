/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuDeletePane - Deletes a pane from an XMenu object.
 *
 *	Author:		Tony Della Fera, DEC
 *			20-Nov-85
 *
 */

#include "XMenuInt.h"

int
XMenuDeletePane(register Display *display, register XMenu *menu, register int p_num)
                              	/* Previously opened display */
                         	/* Menu object to be modified. */
                       		/* Pane number to be deleted. */
{
    register XMPane *p_ptr;	/* Pointer to pane being deleted. */
    register XMSelect *s_ptr;	/* Pointer to selections being deleted. */
    register XMSelect *s_next;  /* Pointer to next selection to be deleted. */

    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Remove the pane from the association table.
     */
    XDeleteAssoc(display, menu->assoc_tab, p_ptr->window);

    /*
     * Remove the pane from the pane list and update
     * the pane count.
     */
    emacs_remque(p_ptr);
    menu->p_count--;

    /*
     * Remove all the selections in the pane from the
     * association table and free their XMSelect structures.
     */
    for (
	s_ptr = p_ptr->s_list->next;
	s_ptr != p_ptr->s_list;
	s_ptr = s_next
    ) {
	XDeleteAssoc(display, menu->assoc_tab, s_ptr->window);
	s_next = s_ptr->next;
	free(s_ptr);
    }
    free(p_ptr->s_list);

    if (p_ptr->window) {
	/*
	 * Destroy the selection transparencies.
	 */
	XDestroySubwindows(display, p_ptr->window);

	/*
	 * Destroy the pane window.
	 */
	XDestroyWindow(display, p_ptr->window);
    }

    /*
     * Free the pane's XMPane structure.
     */
    free(p_ptr);

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the pane number just deleted.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(p_num);
}

