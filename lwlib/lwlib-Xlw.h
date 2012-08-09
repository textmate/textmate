#ifndef LWLIB_XLW_H
#define LWLIB_XLW_H

#include "lwlib-int.h"

extern widget_creation_entry xlw_creation_table [];
extern widget_creation_function xlw_create_dialog;

Boolean
lw_lucid_widget_p (Widget widget);

void
xlw_update_one_widget (widget_instance* instance, Widget widget,
                       widget_value* val, Boolean deep_p);

void
xlw_update_one_value (widget_instance* instance, Widget widget,
                      widget_value* val);

void
xlw_destroy_instance (widget_instance* instance);

void
xlw_pop_instance (widget_instance* instance, Boolean up);

void
xlw_popup_menu (Widget widget, XEvent * event);

#endif /* LWLIB_XLW_H */

