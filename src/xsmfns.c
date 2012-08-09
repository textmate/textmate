/* Session management module for systems which understand the X Session
   management protocol.

Copyright (C) 2002-2012  Free Software Foundation, Inc.

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

#include <config.h>

#ifdef HAVE_X_SM

#include <X11/SM/SMlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <unistd.h>
#include <sys/param.h>
#include <stdio.h>
#include <setjmp.h>

#include "lisp.h"
#include "systime.h"
#include "sysselect.h"
#include "frame.h"
#include "termhooks.h"
#include "termopts.h"
#include "xterm.h"
#include "process.h"
#include "keyboard.h"

#if defined USE_GTK && !defined HAVE_GTK3
#define gdk_x11_set_sm_client_id(w) gdk_set_sm_client_id (w)
#endif

/* This is the event used when SAVE_SESSION_EVENT occurs.  */

static struct input_event emacs_event;

/* The descriptor that we use to check for data from the session manager.  */

static int ice_fd;

/* A flag that says if we are in shutdown interactions or not.  */

static int doing_interact;

/* The session manager object for the session manager connection.  */

static SmcConn smc_conn;

/* The client session id for this session.  */

static char *client_id;

/* The full path name to the Emacs program.  */

static char *emacs_program;

/* The option we tell the session manager to start Emacs with when
   restarting Emacs.  The client_id is appended.  */

#define SMID_OPT "--smid="


/* The option to start Emacs without the splash screen when
   restarting Emacs.  */

static char NOSPLASH_OPT[] = "--no-splash";

/* The option to make Emacs start in the given directory.  */

#define CHDIR_OPT "--chdir="

static void
ice_connection_closed (void)
{
  if (ice_fd >= 0)
    delete_read_fd (ice_fd);
  ice_fd = -1;
}


/* Handle any messages from the session manager.  If no connection is
   open to a session manager, just return.  */

static void
x_session_check_input (int fd, void *data, int for_read)
{
  int ret;

  if (ice_fd == -1) return;

  /* Reset this so wo can check kind after callbacks have been called by
     IceProcessMessages.  The smc_interact_CB sets the kind to
     SAVE_SESSION_EVENT, but we don't know beforehand if that callback
     will be called.  */
  emacs_event.kind = NO_EVENT;

  ret = IceProcessMessages (SmcGetIceConnection (smc_conn),
                            (IceReplyWaitInfo *)0, (Bool *)0);
  if (ret != IceProcessMessagesSuccess)
    {
      /* Either IO error or Connection closed.  */
      if (ret == IceProcessMessagesIOError)
        IceCloseConnection (SmcGetIceConnection (smc_conn));

      ice_connection_closed ();
    }

  /* Check if smc_interact_CB was called and we shall generate a
     SAVE_SESSION_EVENT.  */
  if (emacs_event.kind != NO_EVENT)
    kbd_buffer_store_event (&emacs_event);
}

/* Return non-zero if we have a connection to a session manager.  */

int
x_session_have_connection (void)
{
  return ice_fd != -1;
}

/* This is called when the session manager says it is OK to interact with the
   user.  Here we set the kind to SAVE_SESSION_EVENT so an event is generated.
   Then lisp code can interact with the user.  */

static void
smc_interact_CB (SmcConn smcConn, SmPointer clientData)
{
  doing_interact = True;
  emacs_event.kind = SAVE_SESSION_EVENT;
  emacs_event.arg = Qnil;
}

/* This is called when the session manager tells us to save ourselves.
   We set the required properties so the session manager can restart us,
   plus the current working directory property (not mandatory) so we
   are started in the correct directory.

   If this is a shutdown and we can request to interact with the user,
   we do so, because we don't know what the lisp code might do.  */

static void
smc_save_yourself_CB (SmcConn smcConn,
		      SmPointer clientData,
		      int saveType,
		      Bool shutdown,
		      int interactStyle,
		      Bool fast)
{
#define NR_PROPS 5

  SmProp *props[NR_PROPS];
  SmProp prop_ptr[NR_PROPS];

  SmPropValue values[20], *vp;
  int val_idx = 0, vp_idx = 0;
  int props_idx = 0;
  int i;
  char *cwd = get_current_dir_name ();
  char *smid_opt, *chdir_opt = NULL;

  /* How to start a new instance of Emacs.  */
  props[props_idx] = &prop_ptr[props_idx];
  props[props_idx]->name = xstrdup (SmCloneCommand);
  props[props_idx]->type = xstrdup (SmLISTofARRAY8);
  props[props_idx]->num_vals = 1;
  props[props_idx]->vals = &values[val_idx++];
  props[props_idx]->vals[0].length = strlen (emacs_program);
  props[props_idx]->vals[0].value = emacs_program;
  ++props_idx;

  /* The name of the program.  */
  props[props_idx] = &prop_ptr[props_idx];
  props[props_idx]->name = xstrdup (SmProgram);
  props[props_idx]->type = xstrdup (SmARRAY8);
  props[props_idx]->num_vals = 1;
  props[props_idx]->vals = &values[val_idx++];
  props[props_idx]->vals[0].length = SBYTES (Vinvocation_name);
  props[props_idx]->vals[0].value = SDATA (Vinvocation_name);
  ++props_idx;

  /* User id.  */
  props[props_idx] = &prop_ptr[props_idx];
  props[props_idx]->name = xstrdup (SmUserID);
  props[props_idx]->type = xstrdup (SmARRAY8);
  props[props_idx]->num_vals = 1;
  props[props_idx]->vals = &values[val_idx++];
  props[props_idx]->vals[0].length = SBYTES (Vuser_login_name);
  props[props_idx]->vals[0].value = SDATA (Vuser_login_name);
  ++props_idx;


  if (cwd)
    {
      props[props_idx] = &prop_ptr[props_idx];
      props[props_idx]->name = xstrdup (SmCurrentDirectory);
      props[props_idx]->type = xstrdup (SmARRAY8);
      props[props_idx]->num_vals = 1;
      props[props_idx]->vals = &values[val_idx++];
      props[props_idx]->vals[0].length = strlen (cwd);
      props[props_idx]->vals[0].value = cwd;
      ++props_idx;
    }


  /* How to restart Emacs.  */
  props[props_idx] = &prop_ptr[props_idx];
  props[props_idx]->name = xstrdup (SmRestartCommand);
  props[props_idx]->type = xstrdup (SmLISTofARRAY8);
  /* /path/to/emacs, --smid=xxx --no-splash --chdir=dir ... */
  if (INT_MAX - 3 < initial_argc)
    memory_full (SIZE_MAX);
  i = 3 + initial_argc;
  props[props_idx]->num_vals = i;
  vp = xnmalloc (i, sizeof *vp);
  props[props_idx]->vals = vp;
  props[props_idx]->vals[vp_idx].length = strlen (emacs_program);
  props[props_idx]->vals[vp_idx++].value = emacs_program;

  smid_opt = xmalloc (strlen (SMID_OPT) + strlen (client_id) + 1);
  strcpy (smid_opt, SMID_OPT);
  strcat (smid_opt, client_id);

  props[props_idx]->vals[vp_idx].length = strlen (smid_opt);
  props[props_idx]->vals[vp_idx++].value = smid_opt;

  props[props_idx]->vals[vp_idx].length = strlen (NOSPLASH_OPT);
  props[props_idx]->vals[vp_idx++].value = NOSPLASH_OPT;

  if (cwd)
    {
      chdir_opt = xmalloc (strlen (CHDIR_OPT) + strlen (cwd) + 1);
      strcpy (chdir_opt, CHDIR_OPT);
      strcat (chdir_opt, cwd);

      props[props_idx]->vals[vp_idx].length = strlen (chdir_opt);
      props[props_idx]->vals[vp_idx++].value = chdir_opt;
    }

  for (i = 1; i < initial_argc; ++i)
    {
      props[props_idx]->vals[vp_idx].length = strlen (initial_argv[i]);
      props[props_idx]->vals[vp_idx++].value = initial_argv[i];
    }

  ++props_idx;

  SmcSetProperties (smcConn, props_idx, props);

  xfree (smid_opt);
  xfree (chdir_opt);
  xfree (cwd);
  xfree (vp);

  for (i = 0; i < props_idx; ++i)
    {
      xfree (props[i]->type);
      xfree (props[i]->name);
    }

  /* See if we maybe shall interact with the user.  */
  if (interactStyle != SmInteractStyleAny
      || ! shutdown
      || saveType == SmSaveLocal
      || ! SmcInteractRequest (smcConn, SmDialogNormal, smc_interact_CB, 0))
    {
      /* No interaction, we are done saving ourself.  */
      SmcSaveYourselfDone (smcConn, True);
    }
}

/* According to the SM specification, this shall close the connection.  */

static void
smc_die_CB (SmcConn smcConn, SmPointer clientData)
{
  emacs_event.kind = SAVE_SESSION_EVENT;
  emacs_event.arg = Qt;
}

/* We don't use the next two but they are mandatory, leave them empty.
   According to the SM specification, we should not interact with the
   user between smc_save_yourself_CB is called and until smc_save_complete_CB
   is called.  It seems like a lot of job to implement this and it doesn't
   even seem necessary.  */

static void
smc_save_complete_CB (SmcConn smcConn, SmPointer clientData)
{
  /* Empty */
}

static void
smc_shutdown_cancelled_CB (SmcConn smcConn, SmPointer clientData)
{
  /* Empty */
}

/* Error handlers for SM and ICE.  We don't want to exit Emacs just
   because there is some error in the session management.  */

static void
smc_error_handler (SmcConn smcConn,
		   Bool swap,
		   int offendingMinorOpcode,
		   unsigned long offendingSequence,
		   int errorClass,
		   int severity,
		   SmPointer values)
{
  /* Empty  */
}

static void
ice_error_handler (IceConn iceConn,
		   Bool swap,
		   int offendingMinorOpcode,
		   unsigned long offendingSequence,
		   int errorClass,
		   int severity,
		   IcePointer values)
{
  /* Empty  */
}


static void
ice_io_error_handler (IceConn iceConn)
{
  /* Connection probably gone.  */
  ice_connection_closed ();
}

/* This is called when the ICE connection is created or closed.  The SM library
   uses ICE as it transport protocol.  */

static void
ice_conn_watch_CB (IceConn iceConn, IcePointer clientData,
                   int opening, IcePointer *watchData)
{
  if (! opening)
    {
      ice_connection_closed ();
      return;
    }

  ice_fd = IceConnectionNumber (iceConn);
  add_read_fd (ice_fd, x_session_check_input, NULL);
}

/* Create the client leader window.  */

#ifndef USE_GTK
static void
create_client_leader_window (struct x_display_info *dpyinfo, char *client_ID)
{
  Window w;
  XClassHint class_hints;

  w = XCreateSimpleWindow (dpyinfo->display,
                           dpyinfo->root_window,
                           -1, -1, 1, 1,
                           CopyFromParent, CopyFromParent, CopyFromParent);

  class_hints.res_name = SSDATA (Vx_resource_name);
  class_hints.res_class = SSDATA (Vx_resource_class);
  XSetClassHint (dpyinfo->display, w, &class_hints);
  XStoreName (dpyinfo->display, w, class_hints.res_name);

  XChangeProperty (dpyinfo->display, w, dpyinfo->Xatom_SM_CLIENT_ID,
                   XA_STRING, 8, PropModeReplace,
                   (unsigned char *) client_ID, strlen (client_ID));

  dpyinfo->client_leader_window = w;
}
#endif /* ! USE_GTK */


/* Try to open a connection to the session manager.  */

void
x_session_initialize (struct x_display_info *dpyinfo)
{
#define SM_ERRORSTRING_LEN 512
  char errorstring[SM_ERRORSTRING_LEN];
  char* previous_id = NULL;
  SmcCallbacks callbacks;
  ptrdiff_t name_len = 0;

  ice_fd = -1;
  doing_interact = False;

  /* Check if we where started by the session manager.  If so, we will
     have a previous id.  */
  if (! EQ (Vx_session_previous_id, Qnil) && STRINGP (Vx_session_previous_id))
    previous_id = SSDATA (Vx_session_previous_id);

  /* Construct the path to the Emacs program.  */
  if (! EQ (Vinvocation_directory, Qnil))
    name_len += SBYTES (Vinvocation_directory);
  name_len += SBYTES (Vinvocation_name);

  /* This malloc will not be freed, but it is only done once, and hopefully
     not very large   */
  emacs_program = xmalloc (name_len + 1);
  emacs_program[0] = '\0';

  if (! EQ (Vinvocation_directory, Qnil))
    strcpy (emacs_program, SSDATA (Vinvocation_directory));
  strcat (emacs_program, SSDATA (Vinvocation_name));

  /* The SM protocol says all callbacks are mandatory, so set up all
     here and in the mask passed to SmcOpenConnection.  */
  callbacks.save_yourself.callback = smc_save_yourself_CB;
  callbacks.save_yourself.client_data = 0;
  callbacks.die.callback = smc_die_CB;
  callbacks.die.client_data = 0;
  callbacks.save_complete.callback = smc_save_complete_CB;
  callbacks.save_complete.client_data = 0;
  callbacks.shutdown_cancelled.callback = smc_shutdown_cancelled_CB;
  callbacks.shutdown_cancelled.client_data = 0;

  /* Set error handlers.  */
  SmcSetErrorHandler (smc_error_handler);
  IceSetErrorHandler (ice_error_handler);
  IceSetIOErrorHandler (ice_io_error_handler);

  /* Install callback for when connection status changes.  */
  IceAddConnectionWatch (ice_conn_watch_CB, 0);

  /* Open the connection to the session manager.  A failure is not
     critical, it usually means that no session manager is running.
     The errorstring is here for debugging.  */
  smc_conn = SmcOpenConnection (NULL, NULL, 1, 0,
                                (SmcSaveYourselfProcMask|
                                 SmcDieProcMask|
                                 SmcSaveCompleteProcMask|
                                 SmcShutdownCancelledProcMask),
                                &callbacks,
                                previous_id,
                                &client_id,
                                SM_ERRORSTRING_LEN,
                                errorstring);

  if (smc_conn != 0)
    {
      Vx_session_id = build_string (client_id);

#ifdef USE_GTK
      /* GTK creates a leader window by itself, but we need to tell
         it about our client_id.  */
      gdk_x11_set_sm_client_id (client_id);
#else
      create_client_leader_window (dpyinfo, client_id);
#endif
    }
}

/* Ensure that the session manager is not contacted again. */

void
x_session_close (void)
{
  ice_connection_closed ();
}


DEFUN ("handle-save-session", Fhandle_save_session,
       Shandle_save_session, 1, 1, "e",
       doc: /* Handle the save_yourself event from a session manager.
A session manager can tell Emacs that the window system is shutting down
by sending Emacs a save_yourself message.  Emacs executes this function when
such an event occurs.  This function then executes `emacs-session-save'.
After that, this function informs the session manager that it can continue
or abort shutting down the window system depending on the return value
from `emacs-session-save'  If the return value is non-nil the session manager
is told to abort the window system shutdown.

Do not call this function yourself. */)
  (Lisp_Object event)
{
  int kill_emacs = CONSP (event) && CONSP (XCDR (event))
    && EQ (Qt, XCAR (XCDR (event)));

  /* Check doing_interact so that we don't do anything if someone called
     this at the wrong time. */
  if (doing_interact && ! kill_emacs)
    {
      Bool cancel_shutdown = False;

      cancel_shutdown = ! EQ (call0 (intern ("emacs-session-save")), Qnil);

      SmcInteractDone (smc_conn, cancel_shutdown);
      SmcSaveYourselfDone (smc_conn, True);

      doing_interact = False;
    }
  else if (kill_emacs)
    {
      /* We should not do user interaction here, but it is not easy to
         prevent.  Fix this in next version.  */
      Fkill_emacs (Qnil);

      /* This will not be reached, but we want kill-emacs-hook to be run.  */
      SmcCloseConnection (smc_conn, 0, 0);
      ice_connection_closed ();
    }

  return Qnil;
}



/***********************************************************************
			    Initialization
 ***********************************************************************/
void
syms_of_xsmfns (void)
{
  DEFVAR_LISP ("x-session-id", Vx_session_id,
    doc: /* The session id Emacs got from the session manager for this session.
Changing the value does not change the session id used by Emacs.
The value is nil if no session manager is running.
See also `x-session-previous-id', `emacs-save-session-functions',
`emacs-session-save' and `emacs-session-restore'." */);
  Vx_session_id = Qnil;

  DEFVAR_LISP ("x-session-previous-id", Vx_session_previous_id,
    doc: /* The previous session id Emacs got from session manager.
If Emacs is running on a window system that has a session manager, the
session manager gives Emacs a session id.  It is feasible for Emacs Lisp
code to use the session id to save configuration in, for example, a file
with a file name based on the session id.  If Emacs is running when the
window system is shut down, the session manager remembers that Emacs was
running and saves the session id Emacs had.

When the window system is started again, the session manager restarts
Emacs and hands Emacs the session id it had the last time it was
running.  This is now the previous session id and the value of this
variable.  If configuration was saved in a file as stated above, the
previous session id shall be used to reconstruct the file name.

The session id Emacs has while it is running is in the variable
`x-session-id'.  The value of this variable and `x-session-id' may be the
same, depending on how the session manager works.

See also `emacs-save-session-functions', `emacs-session-save' and
`emacs-session-restore'." */);
  Vx_session_previous_id = Qnil;

  defsubr (&Shandle_save_session);
}

#endif /* HAVE_X_SM */
