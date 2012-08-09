/* Simple client interface to DDE servers.
   Copyright (C) 1998, 2001-2012  Free Software Foundation, Inc.

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

#include <windows.h>
#include <ddeml.h>
#include <stdlib.h>
#include <stdio.h>

HDDEDATA CALLBACK
DdeCallback (UINT uType, UINT uFmt, HCONV hconv,
	     HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
	     DWORD dwData1, DWORD dwData2)
{
  return ((HDDEDATA) NULL);
}

#define DdeCommand(str) 	\
	DdeClientTransaction (str, strlen (str)+1, HConversation, (HSZ)NULL, \
		              CF_TEXT, XTYP_EXECUTE, 30000, NULL)

int
main (int argc, char *argv[])
{
  DWORD idDde = 0;
  HCONV HConversation;
  HSZ   Server;
  HSZ   Topic = 0;
  char  command[1024];

  if (argc < 2)
    {
      fprintf (stderr, "usage: ddeclient server [topic]\n");
      exit (1);
    }

  DdeInitialize (&idDde, (PFNCALLBACK)DdeCallback, APPCMD_CLIENTONLY, 0);

  Server = DdeCreateStringHandle (idDde, argv[1], CP_WINANSI);
  if (argc > 2)
    Topic = DdeCreateStringHandle (idDde, argv[2], CP_WINANSI);

  HConversation = DdeConnect (idDde, Server, Topic, NULL);
  if (HConversation != 0)
    {
      while (fgets (command, sizeof(command), stdin) != NULL)
	DdeCommand (command);

      DdeDisconnect (HConversation);
    }

  DdeFreeStringHandle (idDde, Server);
  if (Topic)
    DdeFreeStringHandle (idDde, Topic);
  DdeUninitialize (idDde);

  return (0);
}

