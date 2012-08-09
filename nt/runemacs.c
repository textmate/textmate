/* runemacs --- Simple program to start Emacs with its console window hidden.

Copyright (C) 2001-2012  Free Software Foundation, Inc.

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


/*
  Simple program to start Emacs with its console window hidden.

  This program is provided purely for convenience, since most users will
  use Emacs in windowing (GUI) mode, and will not want to have an extra
  console window lying around.  */

/*
   You may want to define this if you want to be able to install updated
   emacs binaries even when other users are using the current version.
   The problem with some file servers (notably Novell) is that an open
   file cannot be overwritten, deleted, or even renamed.  So if someone
   is running emacs.exe already, you cannot install a newer version.
   By defining CHOOSE_NEWEST_EXE, you can name your new emacs.exe
   something else which matches "emacs*.exe", and runemacs will
   automatically select the newest emacs executable in the bin directory.
   (So you'll probably be able to delete the old version some hours/days
   later).
*/

/* #define CHOOSE_NEWEST_EXE */

#include <windows.h>
#include <string.h>
#include <malloc.h>

static void set_user_model_id (void);
static int ensure_unicows_dll (void);

int WINAPI
WinMain (HINSTANCE hSelf, HINSTANCE hPrev, LPSTR cmdline, int nShow)
{
  STARTUPINFO start;
  SECURITY_ATTRIBUTES sec_attrs;
  PROCESS_INFORMATION child;
  int wait_for_child = FALSE;
  DWORD priority_class = NORMAL_PRIORITY_CLASS;
  DWORD ret_code = 0;
  char *new_cmdline;
  char *p;
  char modname[MAX_PATH];

  if (!ensure_unicows_dll ())
    goto error;

  set_user_model_id ();

  if (!GetModuleFileName (NULL, modname, MAX_PATH))
    goto error;
  if ((p = strrchr (modname, '\\')) == NULL)
    goto error;
  *p = 0;

  new_cmdline = alloca (MAX_PATH + strlen (cmdline) + 3);
  /* Quote executable name in case of spaces in the path. */
  *new_cmdline = '"';
  strcpy (new_cmdline + 1, modname);

#ifdef CHOOSE_NEWEST_EXE
  {
    /* Silly hack to allow new versions to be installed on
       server even when current version is in use. */

    char * best_name = alloca (MAX_PATH + 1);
    FILETIME best_time = {0,0};
    WIN32_FIND_DATA wfd;
    HANDLE fh;
    p = new_cmdline + strlen (new_cmdline);
    strcpy (p, "\\emacs*.exe\" ");
    fh = FindFirstFile (new_cmdline, &wfd);
    if (fh == INVALID_HANDLE_VALUE)
      goto error;
    do
      {
	if (wfd.ftLastWriteTime.dwHighDateTime > best_time.dwHighDateTime
	    || (wfd.ftLastWriteTime.dwHighDateTime == best_time.dwHighDateTime
		&& wfd.ftLastWriteTime.dwLowDateTime > best_time.dwLowDateTime))
	  {
	    best_time = wfd.ftLastWriteTime;
	    strcpy (best_name, wfd.cFileName);
	  }
      }
    while (FindNextFile (fh, &wfd));
    FindClose (fh);
    *p++ = '\\';
    strcpy (p, best_name);
    strcat (p, " ");
  }
#else
  strcat (new_cmdline, "\\emacs.exe\" ");
#endif

  /* Append original arguments if any; first look for arguments we
     recognize (-wait, -high, and -low), and apply them ourselves.  */
  while (cmdline[0] == '-' || cmdline[0] == '/')
    {
      if (strncmp (cmdline+1, "wait", 4) == 0)
	{
	  wait_for_child = TRUE;
	  cmdline += 5;
	}
      else if (strncmp (cmdline+1, "high", 4) == 0)
	{
	  priority_class = HIGH_PRIORITY_CLASS;
	  cmdline += 5;
	}
      else if (strncmp (cmdline+1, "low", 3) == 0)
	{
	  priority_class = IDLE_PRIORITY_CLASS;
	  cmdline += 4;
	}
      else
	break;
      /* Look for next argument.  */
      while (*++cmdline == ' ');
    }

  strcat (new_cmdline, cmdline);

  /* Set emacs_dir variable if runemacs was in "%emacs_dir%\bin".  */
  if ((p = strrchr (modname, '\\')) && stricmp (p, "\\bin") == 0)
    {
      *p = 0;
      for (p = modname; *p; p++)
	if (*p == '\\') *p = '/';
      SetEnvironmentVariable ("emacs_dir", modname);
    }

  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);
  start.dwFlags = STARTF_USESHOWWINDOW | STARTF_USECOUNTCHARS;
  start.wShowWindow = SW_HIDE;
  /* Ensure that we don't waste memory if the user has specified a huge
     default screen buffer for command windows.  */
  start.dwXCountChars = 80;
  start.dwYCountChars = 25;

  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = NULL;
  sec_attrs.bInheritHandle = FALSE;

  if (CreateProcess (NULL, new_cmdline, &sec_attrs, NULL, TRUE, priority_class,
		     NULL, NULL, &start, &child))
    {
      if (wait_for_child)
	{
	  WaitForSingleObject (child.hProcess, INFINITE);
	  GetExitCodeProcess (child.hProcess, &ret_code);
	}
      CloseHandle (child.hThread);
      CloseHandle (child.hProcess);
    }
  else
    goto error;
  return (int) ret_code;

error:
  MessageBox (NULL, "Could not start Emacs.", "Error", MB_ICONSTOP);
  return 1;
}

void
set_user_model_id (void)
{
  HMODULE shell;
  HRESULT (WINAPI * set_user_model) (wchar_t * id);

  /* On Windows 7 and later, we need to set the user model ID
     to associate emacsclient launched files with Emacs frames
     in the UI.  */
  shell = LoadLibrary ("shell32.dll");
  if (shell)
    {
      set_user_model
	= (void *) GetProcAddress (shell,
				   "SetCurrentProcessExplicitAppUserModelID");

      /* If the function is defined, then we are running on Windows 7
	 or newer, and the UI uses this to group related windows
	 together.  Since emacs, runemacs, emacsclient are related, we
	 want them grouped even though the executables are different,
	 so we need to set a consistent ID between them.  */
      if (set_user_model)
	set_user_model (L"GNU.Emacs");

      FreeLibrary (shell);
    }
}

static int
ensure_unicows_dll (void)
{
  OSVERSIONINFO os_ver;
  HMODULE h;

  ZeroMemory (&os_ver, sizeof (OSVERSIONINFO));
  os_ver.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
  if (!GetVersionEx (&os_ver))
    return 0;

  if (os_ver.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
    {
      h = LoadLibrary ("Unicows.dll");
      if (!h)
	{
	  int button;

	  button = MessageBox (NULL,
			       "Emacs cannot load the UNICOWS.DLL library.\n"
			       "This library is essential for using Emacs\n"
			       "on this system.  You need to install it.\n\n"
			       "However, you can still use Emacs by invoking\n"
			       "it with the '-nw' command-line option.\n\n"
			       "Emacs will exit when you click OK.",
			       "Emacs cannot load UNICOWS.DLL",
			       MB_ICONERROR | MB_TASKMODAL
			       | MB_SETFOREGROUND | MB_OK);
	  switch (button)
	    {
	      case IDOK:
	      default:
	        return 0;
	    }
	}
      FreeLibrary (h);
      return 1;
    }
  return 1;
}
