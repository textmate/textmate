"""Definitions used by commands sent to inferior Python in python.el."""

# Copyright (C) 2004-2012  Free Software Foundation, Inc.
# Author: Dave Love <fx@gnu.org>

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

import os, sys, traceback, inspect, __main__

try:
    set
except:
    from sets import Set as set

__all__ = ["eexecfile", "eargs", "complete", "ehelp", "eimport", "modpath"]

def format_exception (filename, should_remove_self):
    type, value, tb = sys.exc_info ()
    sys.last_type = type
    sys.last_value = value
    sys.last_traceback = tb
    if type is SyntaxError:
        try: # parse the error message
            msg, (dummy_filename, lineno, offset, line) = value
        except:
            pass # Not the format we expect; leave it alone
        else:
            # Stuff in the right filename
            value = SyntaxError(msg, (filename, lineno, offset, line))
            sys.last_value = value
    res = traceback.format_exception_only (type, value)
    # There are some compilation errors which do not provide traceback so we
    # should not massage it.
    if should_remove_self:
        tblist = traceback.extract_tb (tb)
        del tblist[:1]
        res = traceback.format_list (tblist)
        if res:
            res.insert(0, "Traceback (most recent call last):\n")
        res[len(res):] = traceback.format_exception_only (type, value)
    # traceback.print_exception(type, value, tb)
    for line in res: print line,

def eexecfile (file):
    """Execute FILE and then remove it.
    Execute the file within the __main__ namespace.
    If we get an exception, print a traceback with the top frame
    (ourselves) excluded."""
    # We cannot use real execfile since it has a bug where the file stays
    # locked forever (under w32) if SyntaxError occurs.
    # --- code based on code.py and PyShell.py.
    try:
        try:
            source = open (file, "r").read()
            code = compile (source, file, "exec")
        # Other exceptions (shouldn't be any...) will (correctly) fall
        # through to "final".
        except (OverflowError, SyntaxError, ValueError):
            # FIXME: When can compile() raise anything else than
            # SyntaxError ????
            format_exception (file, False)
            return
        try:
            exec code in __main__.__dict__
        except:
            format_exception (file, True)
    finally:
        os.remove (file)

def eargs (name, imports):
    "Get arglist of NAME for Eldoc &c."
    try:
        if imports: exec imports
        parts = name.split ('.')
        if len (parts) > 1:
            exec 'import ' + parts[0] # might fail
        func = eval (name)
        if inspect.isbuiltin (func) or type(func) is type:
            doc = func.__doc__
            if doc.find (' ->') != -1:
                print '_emacs_out', doc.split (' ->')[0]
            else:
                print '_emacs_out', doc.split ('\n')[0]
            return
        if inspect.ismethod (func):
            func = func.im_func
        if not inspect.isfunction (func):
            print '_emacs_out '
            return
        (args, varargs, varkw, defaults) = inspect.getargspec (func)
        # No space between name and arglist for consistency with builtins.
        print '_emacs_out', \
            func.__name__ + inspect.formatargspec (args, varargs, varkw,
                                                   defaults)
    except:
        print "_emacs_out "

def all_names (object):
    """Return (an approximation to) a list of all possible attribute
    names reachable via the attributes of OBJECT, i.e. roughly the
    leaves of the dictionary tree under it."""

    def do_object (object, names):
        if inspect.ismodule (object):
            do_module (object, names)
        elif inspect.isclass (object):
            do_class (object, names)
        # Might have an object without its class in scope.
        elif hasattr (object, '__class__'):
            names.add ('__class__')
            do_class (object.__class__, names)
        # Probably not a good idea to try to enumerate arbitrary
        # dictionaries...
        return names

    def do_module (module, names):
        if hasattr (module, '__all__'): # limited export list
            names.update(module.__all__)
            for i in module.__all__:
                do_object (getattr (module, i), names)
        else:                   # use all names
            names.update(dir (module))
            for i in dir (module):
                do_object (getattr (module, i), names)
        return names

    def do_class (object, names):
        ns = dir (object)
        names.update(ns)
        if hasattr (object, '__bases__'): # superclasses
            for i in object.__bases__: do_object (i, names)
        return names

    return do_object (object, set([]))

def complete (name, imports):
    """Complete TEXT in NAMESPACE and print a Lisp list of completions.
    Exec IMPORTS first."""
    import __main__, keyword

    def class_members(object):
        names = dir (object)
        if hasattr (object, '__bases__'):
            for super in object.__bases__:
                names = class_members (super)
        return names    

    names = set([])
    base = None
    try:
        dict = __main__.__dict__.copy()
        if imports: exec imports in dict
        l = len (name)
        if not "." in name:
            for src in [dir (__builtins__), keyword.kwlist, dict.keys()]:
                for elt in src:
                    if elt[:l] == name: names.add(elt)
        else:
            base = name[:name.rfind ('.')]
            name = name[name.rfind('.')+1:]
            try:
                object = eval (base, dict)
                names = set(dir (object))
                if hasattr (object, '__class__'):
                    names.add('__class__')
                    names.update(class_members (object))
            except: names = all_names (dict)
    except: 
        print sys.exc_info()
        names = []
        
    l = len(name)
    print '_emacs_out (',
    for n in names:
        if name == n[:l]:
            if base: print '"%s.%s"' % (base, n),
            else: print '"%s"' % n,
    print ')'

def ehelp (name, imports):
    """Get help on string NAME.
    First try to eval name for, e.g. user definitions where we need
    the object.  Otherwise try the string form."""
    locls = {}
    if imports:
        try: exec imports in locls
        except: pass
    try: help (eval (name, globals(), locls))
    except: help (name)

def eimport (mod, dir):
    """Import module MOD with directory DIR at the head of the search path.
    NB doesn't load from DIR if MOD shadows a system module."""
    from __main__ import __dict__

    path0 = sys.path[0]
    sys.path[0] = dir
    try:
        try:
            if __dict__.has_key(mod) and inspect.ismodule (__dict__[mod]):
                reload (__dict__[mod])
            else:
                __dict__[mod] = __import__ (mod)
        except:
            (type, value, tb) = sys.exc_info ()
            print "Traceback (most recent call last):"
            traceback.print_exception (type, value, tb.tb_next)
    finally:
        sys.path[0] = path0

def modpath (module):
    """Return the source file for the given MODULE (or None).
Assumes that MODULE.py and MODULE.pyc are in the same directory."""
    try:
        path = __import__ (module).__file__
        if path[-4:] == '.pyc' and os.path.exists (path[0:-1]):
            path = path[:-1]
        print "_emacs_out", path
    except:
        print "_emacs_out ()"

# print '_emacs_ok'             # ready for input and can call continuation

