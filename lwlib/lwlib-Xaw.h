#ifndef LWLIB_XAW_H
#define LWLIB_XAW_H

#include "lwlib-int.h"

extern widget_creation_entry xaw_creation_table [];

Widget
xaw_create_dialog (widget_instance*);

Boolean
lw_xaw_widget_p (Widget);

void
xaw_update_one_widget (widget_instance *, Widget, widget_value *, Boolean);

void
xaw_update_one_value (widget_instance *, Widget, widget_value *);

void
xaw_destroy_instance (widget_instance *);

void
xaw_popup_menu (Widget, XEvent *);

void
xaw_pop_instance (widget_instance *, Boolean);

#endif /* LWLIB_XAW_H */

