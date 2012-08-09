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
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuFindSelection - Find the first selection in a pane who's
 *			     label matches a particular string.
 *
 *	Author:		Tony Della Fera, DEC
 *			January 22, 1986
 *
 */

#include "XMenuInt.h"
#include <string.h>

int
XMenuFindSelection(register XMenu *menu, int p_num, register char *label)
{
    register XMPane *p_ptr;
    register XMSelect *s_ptr;
    register int i = 0;

    /*
     * Check for NULL pointers!
     */
    if (label == NULL) {
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
    for (
	s_ptr = p_ptr->s_list->next;
	s_ptr != p_ptr->s_list;
	s_ptr = s_ptr->next
    ){
	if (s_ptr->label_length == 0) {
	    if (*label == '\0') {
		_XMErrorCode = XME_NO_ERROR;
		return (i);
	    }
	}
	else {
	    if (strncmp (label, s_ptr->label, s_ptr->label_length) == 0) {
		_XMErrorCode = XME_NO_ERROR;
		return (i);
	    }
	}
	i++;
    }

    /*
     * If we get here then we have not found
     * a match.
     */
    _XMErrorCode = XME_S_NOT_FOUND;
    return (XM_FAILURE);
}
