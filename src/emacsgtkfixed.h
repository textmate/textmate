/* A Gtk Widget that inherits GtkFixed, but can be shrunk.
This file is only use when compiling with Gtk+ 3.

Copyright (C) 2011-2012  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef EMACSGTKFIXED_H
#define EMACSGTKFIXED_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

struct frame;

#define EMACS_TYPE_FIXED                  (emacs_fixed_get_type ())
#define EMACS_FIXED(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), EMACS_TYPE_FIXED, EmacsFixed))
#define EMACS_FIXED_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), EMACS_TYPE_FIXED, EmacsFixedClass))
#define EMACS_IS_FIXED(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EMACS_TYPE_FIXED))
#define EMACS_IS_FIXED_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), EMACS_TYPE_FIXED))
#define EMACS_FIXED_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), EMACS_TYPE_FIXED, EmacsFixedClass))

typedef struct _EmacsFixed              EmacsFixed;
typedef struct _EmacsFixedPrivate       EmacsFixedPrivate;
typedef struct _EmacsFixedClass         EmacsFixedClass;

struct _EmacsFixed
{
  GtkFixed container;

  /*< private >*/
  EmacsFixedPrivate *priv;
};


struct _EmacsFixedClass
{
  GtkFixedClass parent_class;
};

extern GtkWidget *emacs_fixed_new (struct frame *f);
extern GType emacs_fixed_get_type (void);

G_END_DECLS

#endif /* EMACSGTKFIXED_H */
