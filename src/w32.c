/* Utility and Unix shadow routines for GNU Emacs on the Microsoft W32 API.
   Copyright (C) 1994-1995, 2000-2012  Free Software Foundation, Inc.

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
   Geoff Voelker (voelker@cs.washington.edu)                         7-29-94
*/
#include <stddef.h> /* for offsetof */
#include <stdlib.h>
#include <stdio.h>
#include <float.h>	/* for DBL_EPSILON */
#include <io.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/utime.h>
#include <mbstring.h>	/* for _mbspbrk */
#include <math.h>
#include <setjmp.h>
#include <time.h>

/* must include CRT headers *before* config.h */

#include <config.h>

#undef access
#undef chdir
#undef chmod
#undef creat
#undef ctime
#undef fopen
#undef link
#undef mkdir
#undef mktemp
#undef open
#undef rename
#undef rmdir
#undef unlink

#undef close
#undef dup
#undef dup2
#undef pipe
#undef read
#undef write

#undef strerror

#undef localtime

#include "lisp.h"

#include <pwd.h>
#include <grp.h>

#ifdef __GNUC__
#define _ANONYMOUS_UNION
#define _ANONYMOUS_STRUCT
#endif
#include <windows.h>
/* Some versions of compiler define MEMORYSTATUSEX, some don't, so we
   use a different name to avoid compilation problems.  */
typedef struct _MEMORY_STATUS_EX {
  DWORD dwLength;
  DWORD dwMemoryLoad;
  DWORDLONG ullTotalPhys;
  DWORDLONG ullAvailPhys;
  DWORDLONG ullTotalPageFile;
  DWORDLONG ullAvailPageFile;
  DWORDLONG ullTotalVirtual;
  DWORDLONG ullAvailVirtual;
  DWORDLONG ullAvailExtendedVirtual;
} MEMORY_STATUS_EX,*LPMEMORY_STATUS_EX;

#include <lmcons.h>
#include <shlobj.h>

#include <tlhelp32.h>
#include <psapi.h>
#ifndef _MSC_VER
#include <w32api.h>
#endif
#if !defined (__MINGW32__) || __W32API_MAJOR_VERSION < 3 || (__W32API_MAJOR_VERSION == 3 && __W32API_MINOR_VERSION < 15)
/* This either is not in psapi.h or guarded by higher value of
   _WIN32_WINNT than what we use.  w32api supplied with MinGW 3.15
   defines it in psapi.h  */
typedef struct _PROCESS_MEMORY_COUNTERS_EX {
  DWORD cb;
  DWORD PageFaultCount;
  DWORD PeakWorkingSetSize;
  DWORD WorkingSetSize;
  DWORD QuotaPeakPagedPoolUsage;
  DWORD QuotaPagedPoolUsage;
  DWORD QuotaPeakNonPagedPoolUsage;
  DWORD QuotaNonPagedPoolUsage;
  DWORD PagefileUsage;
  DWORD PeakPagefileUsage;
  DWORD PrivateUsage;
} PROCESS_MEMORY_COUNTERS_EX,*PPROCESS_MEMORY_COUNTERS_EX;
#endif

/* TCP connection support.  */
#include <sys/socket.h>
#undef socket
#undef bind
#undef connect
#undef htons
#undef ntohs
#undef inet_addr
#undef gethostname
#undef gethostbyname
#undef getservbyname
#undef getpeername
#undef shutdown
#undef setsockopt
#undef listen
#undef getsockname
#undef accept
#undef recvfrom
#undef sendto

#include "w32.h"
#include "ndir.h"
#include "w32heap.h"
#include "systime.h"
#include "dispextern.h"		/* for xstrcasecmp */
#include "coding.h"		/* for Vlocale_coding_system */

#include "careadlinkat.h"
#include "allocator.h"

/* For serial_configure and serial_open.  */
#include "process.h"

typedef HRESULT (WINAPI * ShGetFolderPath_fn)
  (IN HWND, IN int, IN HANDLE, IN DWORD, OUT char *);

Lisp_Object QCloaded_from;

void globals_of_w32 (void);
static DWORD get_rid (PSID);


/* Initialization states.

   WARNING: If you add any more such variables for additional APIs,
            you MUST add initialization for them to globals_of_w32
            below.  This is because these variables might get set
            to non-NULL values during dumping, but the dumped Emacs
            cannot reuse those values, because it could be run on a
            different version of the OS, where API addresses are
            different.  */
static BOOL g_b_init_is_windows_9x;
static BOOL g_b_init_open_process_token;
static BOOL g_b_init_get_token_information;
static BOOL g_b_init_lookup_account_sid;
static BOOL g_b_init_get_sid_sub_authority;
static BOOL g_b_init_get_sid_sub_authority_count;
static BOOL g_b_init_get_file_security;
static BOOL g_b_init_get_security_descriptor_owner;
static BOOL g_b_init_get_security_descriptor_group;
static BOOL g_b_init_is_valid_sid;
static BOOL g_b_init_create_toolhelp32_snapshot;
static BOOL g_b_init_process32_first;
static BOOL g_b_init_process32_next;
static BOOL g_b_init_open_thread_token;
static BOOL g_b_init_impersonate_self;
static BOOL g_b_init_revert_to_self;
static BOOL g_b_init_get_process_memory_info;
static BOOL g_b_init_get_process_working_set_size;
static BOOL g_b_init_global_memory_status;
static BOOL g_b_init_global_memory_status_ex;
static BOOL g_b_init_get_length_sid;
static BOOL g_b_init_equal_sid;
static BOOL g_b_init_copy_sid;
static BOOL g_b_init_get_native_system_info;
static BOOL g_b_init_get_system_times;

/*
  BEGIN: Wrapper functions around OpenProcessToken
  and other functions in advapi32.dll that are only
  supported in Windows NT / 2k / XP
*/
  /* ** Function pointer typedefs ** */
typedef BOOL (WINAPI * OpenProcessToken_Proc) (
    HANDLE ProcessHandle,
    DWORD DesiredAccess,
    PHANDLE TokenHandle);
typedef BOOL (WINAPI * GetTokenInformation_Proc) (
    HANDLE TokenHandle,
    TOKEN_INFORMATION_CLASS TokenInformationClass,
    LPVOID TokenInformation,
    DWORD TokenInformationLength,
    PDWORD ReturnLength);
typedef BOOL (WINAPI * GetProcessTimes_Proc) (
    HANDLE process_handle,
    LPFILETIME creation_time,
    LPFILETIME exit_time,
    LPFILETIME kernel_time,
    LPFILETIME user_time);

GetProcessTimes_Proc get_process_times_fn = NULL;

#ifdef _UNICODE
const char * const LookupAccountSid_Name = "LookupAccountSidW";
const char * const GetFileSecurity_Name =  "GetFileSecurityW";
#else
const char * const LookupAccountSid_Name = "LookupAccountSidA";
const char * const GetFileSecurity_Name =  "GetFileSecurityA";
#endif
typedef BOOL (WINAPI * LookupAccountSid_Proc) (
    LPCTSTR lpSystemName,
    PSID Sid,
    LPTSTR Name,
    LPDWORD cbName,
    LPTSTR DomainName,
    LPDWORD cbDomainName,
    PSID_NAME_USE peUse);
typedef PDWORD (WINAPI * GetSidSubAuthority_Proc) (
    PSID pSid,
    DWORD n);
typedef PUCHAR (WINAPI * GetSidSubAuthorityCount_Proc) (
    PSID pSid);
typedef BOOL (WINAPI * GetFileSecurity_Proc) (
    LPCTSTR lpFileName,
    SECURITY_INFORMATION RequestedInformation,
    PSECURITY_DESCRIPTOR pSecurityDescriptor,
    DWORD nLength,
    LPDWORD lpnLengthNeeded);
typedef BOOL (WINAPI * GetSecurityDescriptorOwner_Proc) (
    PSECURITY_DESCRIPTOR pSecurityDescriptor,
    PSID *pOwner,
    LPBOOL lpbOwnerDefaulted);
typedef BOOL (WINAPI * GetSecurityDescriptorGroup_Proc) (
    PSECURITY_DESCRIPTOR pSecurityDescriptor,
    PSID *pGroup,
    LPBOOL lpbGroupDefaulted);
typedef BOOL (WINAPI * IsValidSid_Proc) (
    PSID sid);
typedef HANDLE (WINAPI * CreateToolhelp32Snapshot_Proc) (
    DWORD dwFlags,
    DWORD th32ProcessID);
typedef BOOL (WINAPI * Process32First_Proc) (
    HANDLE hSnapshot,
    LPPROCESSENTRY32 lppe);
typedef BOOL (WINAPI * Process32Next_Proc) (
    HANDLE hSnapshot,
    LPPROCESSENTRY32 lppe);
typedef BOOL (WINAPI * OpenThreadToken_Proc) (
    HANDLE ThreadHandle,
    DWORD DesiredAccess,
    BOOL OpenAsSelf,
    PHANDLE TokenHandle);
typedef BOOL (WINAPI * ImpersonateSelf_Proc) (
    SECURITY_IMPERSONATION_LEVEL ImpersonationLevel);
typedef BOOL (WINAPI * RevertToSelf_Proc) (void);
typedef BOOL (WINAPI * GetProcessMemoryInfo_Proc) (
    HANDLE Process,
    PPROCESS_MEMORY_COUNTERS ppsmemCounters,
    DWORD cb);
typedef BOOL (WINAPI * GetProcessWorkingSetSize_Proc) (
    HANDLE hProcess,
    DWORD * lpMinimumWorkingSetSize,
    DWORD * lpMaximumWorkingSetSize);
typedef BOOL (WINAPI * GlobalMemoryStatus_Proc) (
    LPMEMORYSTATUS lpBuffer);
typedef BOOL (WINAPI * GlobalMemoryStatusEx_Proc) (
    LPMEMORY_STATUS_EX lpBuffer);
typedef BOOL (WINAPI * CopySid_Proc) (
    DWORD nDestinationSidLength,
    PSID pDestinationSid,
    PSID pSourceSid);
typedef BOOL (WINAPI * EqualSid_Proc) (
    PSID pSid1,
    PSID pSid2);
typedef DWORD (WINAPI * GetLengthSid_Proc) (
    PSID pSid);
typedef void (WINAPI * GetNativeSystemInfo_Proc) (
    LPSYSTEM_INFO lpSystemInfo);
typedef BOOL (WINAPI * GetSystemTimes_Proc) (
    LPFILETIME lpIdleTime,
    LPFILETIME lpKernelTime,
    LPFILETIME lpUserTime);

  /* ** A utility function ** */
static BOOL
is_windows_9x (void)
{
  static BOOL s_b_ret = 0;
  OSVERSIONINFO os_ver;
  if (g_b_init_is_windows_9x == 0)
    {
      g_b_init_is_windows_9x = 1;
      ZeroMemory (&os_ver, sizeof (OSVERSIONINFO));
      os_ver.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
      if (GetVersionEx (&os_ver))
        {
          s_b_ret = (os_ver.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS);
        }
    }
  return s_b_ret;
}

/* Get total user and system times for get-internal-run-time.
   Returns a list of three integers if the times are provided by the OS
   (NT derivatives), otherwise it returns the result of current-time. */
Lisp_Object
w32_get_internal_run_time (void)
{
  if (get_process_times_fn)
    {
      FILETIME create, exit, kernel, user;
      HANDLE proc = GetCurrentProcess ();
      if ((*get_process_times_fn) (proc, &create, &exit, &kernel, &user))
        {
          LARGE_INTEGER user_int, kernel_int, total;
          int microseconds;
          user_int.LowPart = user.dwLowDateTime;
          user_int.HighPart = user.dwHighDateTime;
          kernel_int.LowPart = kernel.dwLowDateTime;
          kernel_int.HighPart = kernel.dwHighDateTime;
          total.QuadPart = user_int.QuadPart + kernel_int.QuadPart;
          /* FILETIME is 100 nanosecond increments, Emacs only wants
             microsecond resolution.  */
          total.QuadPart /= 10;
          microseconds = total.QuadPart % 1000000;
          total.QuadPart /= 1000000;

          /* Sanity check to make sure we can represent the result.  */
          if (total.HighPart == 0)
            {
              int secs = total.LowPart;

              return list3 (make_number ((secs >> 16) & 0xffff),
                            make_number (secs & 0xffff),
                            make_number (microseconds));
            }
        }
    }

  return Fcurrent_time ();
}

  /* ** The wrapper functions ** */

static BOOL WINAPI
open_process_token (HANDLE ProcessHandle,
		    DWORD DesiredAccess,
		    PHANDLE TokenHandle)
{
  static OpenProcessToken_Proc s_pfn_Open_Process_Token = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_open_process_token == 0)
    {
      g_b_init_open_process_token = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Open_Process_Token =
        (OpenProcessToken_Proc) GetProcAddress (hm_advapi32, "OpenProcessToken");
    }
  if (s_pfn_Open_Process_Token == NULL)
    {
      return FALSE;
    }
  return (
      s_pfn_Open_Process_Token (
          ProcessHandle,
          DesiredAccess,
          TokenHandle)
      );
}

static BOOL WINAPI
get_token_information (HANDLE TokenHandle,
		       TOKEN_INFORMATION_CLASS TokenInformationClass,
		       LPVOID TokenInformation,
		       DWORD TokenInformationLength,
		       PDWORD ReturnLength)
{
  static GetTokenInformation_Proc s_pfn_Get_Token_Information = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_token_information == 0)
    {
      g_b_init_get_token_information = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Token_Information =
        (GetTokenInformation_Proc) GetProcAddress (hm_advapi32, "GetTokenInformation");
    }
  if (s_pfn_Get_Token_Information == NULL)
    {
      return FALSE;
    }
  return (
      s_pfn_Get_Token_Information (
          TokenHandle,
          TokenInformationClass,
          TokenInformation,
          TokenInformationLength,
          ReturnLength)
      );
}

static BOOL WINAPI
lookup_account_sid (LPCTSTR lpSystemName,
		    PSID Sid,
		    LPTSTR Name,
		    LPDWORD cbName,
		    LPTSTR DomainName,
		    LPDWORD cbDomainName,
		    PSID_NAME_USE peUse)
{
  static LookupAccountSid_Proc s_pfn_Lookup_Account_Sid = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_lookup_account_sid == 0)
    {
      g_b_init_lookup_account_sid = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Lookup_Account_Sid =
        (LookupAccountSid_Proc) GetProcAddress (hm_advapi32, LookupAccountSid_Name);
    }
  if (s_pfn_Lookup_Account_Sid == NULL)
    {
      return FALSE;
    }
  return (
      s_pfn_Lookup_Account_Sid (
          lpSystemName,
          Sid,
          Name,
          cbName,
          DomainName,
          cbDomainName,
          peUse)
      );
}

static PDWORD WINAPI
get_sid_sub_authority (PSID pSid, DWORD n)
{
  static GetSidSubAuthority_Proc s_pfn_Get_Sid_Sub_Authority = NULL;
  static DWORD zero = 0U;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return &zero;
    }
  if (g_b_init_get_sid_sub_authority == 0)
    {
      g_b_init_get_sid_sub_authority = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Sid_Sub_Authority =
        (GetSidSubAuthority_Proc) GetProcAddress (
            hm_advapi32, "GetSidSubAuthority");
    }
  if (s_pfn_Get_Sid_Sub_Authority == NULL)
    {
      return &zero;
    }
  return (s_pfn_Get_Sid_Sub_Authority (pSid, n));
}

static PUCHAR WINAPI
get_sid_sub_authority_count (PSID pSid)
{
  static GetSidSubAuthorityCount_Proc s_pfn_Get_Sid_Sub_Authority_Count = NULL;
  static UCHAR zero = 0U;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return &zero;
    }
  if (g_b_init_get_sid_sub_authority_count == 0)
    {
      g_b_init_get_sid_sub_authority_count = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Sid_Sub_Authority_Count =
        (GetSidSubAuthorityCount_Proc) GetProcAddress (
            hm_advapi32, "GetSidSubAuthorityCount");
    }
  if (s_pfn_Get_Sid_Sub_Authority_Count == NULL)
    {
      return &zero;
    }
  return (s_pfn_Get_Sid_Sub_Authority_Count (pSid));
}

static BOOL WINAPI
get_file_security (LPCTSTR lpFileName,
		   SECURITY_INFORMATION RequestedInformation,
		   PSECURITY_DESCRIPTOR pSecurityDescriptor,
		   DWORD nLength,
		   LPDWORD lpnLengthNeeded)
{
  static GetFileSecurity_Proc s_pfn_Get_File_Security = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_file_security == 0)
    {
      g_b_init_get_file_security = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_File_Security =
        (GetFileSecurity_Proc) GetProcAddress (
            hm_advapi32, GetFileSecurity_Name);
    }
  if (s_pfn_Get_File_Security == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Get_File_Security (lpFileName, RequestedInformation,
				   pSecurityDescriptor, nLength,
				   lpnLengthNeeded));
}

static BOOL WINAPI
get_security_descriptor_owner (PSECURITY_DESCRIPTOR pSecurityDescriptor,
			       PSID *pOwner,
			       LPBOOL lpbOwnerDefaulted)
{
  static GetSecurityDescriptorOwner_Proc s_pfn_Get_Security_Descriptor_Owner = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_security_descriptor_owner == 0)
    {
      g_b_init_get_security_descriptor_owner = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Security_Descriptor_Owner =
        (GetSecurityDescriptorOwner_Proc) GetProcAddress (
            hm_advapi32, "GetSecurityDescriptorOwner");
    }
  if (s_pfn_Get_Security_Descriptor_Owner == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Get_Security_Descriptor_Owner (pSecurityDescriptor, pOwner,
					       lpbOwnerDefaulted));
}

static BOOL WINAPI
get_security_descriptor_group (PSECURITY_DESCRIPTOR pSecurityDescriptor,
			       PSID *pGroup,
			       LPBOOL lpbGroupDefaulted)
{
  static GetSecurityDescriptorGroup_Proc s_pfn_Get_Security_Descriptor_Group = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_security_descriptor_group == 0)
    {
      g_b_init_get_security_descriptor_group = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Security_Descriptor_Group =
        (GetSecurityDescriptorGroup_Proc) GetProcAddress (
            hm_advapi32, "GetSecurityDescriptorGroup");
    }
  if (s_pfn_Get_Security_Descriptor_Group == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Get_Security_Descriptor_Group (pSecurityDescriptor, pGroup,
					       lpbGroupDefaulted));
}

static BOOL WINAPI
is_valid_sid (PSID sid)
{
  static IsValidSid_Proc s_pfn_Is_Valid_Sid = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_is_valid_sid == 0)
    {
      g_b_init_is_valid_sid = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Is_Valid_Sid =
        (IsValidSid_Proc) GetProcAddress (
            hm_advapi32, "IsValidSid");
    }
  if (s_pfn_Is_Valid_Sid == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Is_Valid_Sid (sid));
}

static BOOL WINAPI
equal_sid (PSID sid1, PSID sid2)
{
  static EqualSid_Proc s_pfn_Equal_Sid = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_equal_sid == 0)
    {
      g_b_init_equal_sid = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Equal_Sid =
        (EqualSid_Proc) GetProcAddress (
            hm_advapi32, "EqualSid");
    }
  if (s_pfn_Equal_Sid == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Equal_Sid (sid1, sid2));
}

static DWORD WINAPI
get_length_sid (PSID sid)
{
  static GetLengthSid_Proc s_pfn_Get_Length_Sid = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return 0;
    }
  if (g_b_init_get_length_sid == 0)
    {
      g_b_init_get_length_sid = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Length_Sid =
        (GetLengthSid_Proc) GetProcAddress (
            hm_advapi32, "GetLengthSid");
    }
  if (s_pfn_Get_Length_Sid == NULL)
    {
      return 0;
    }
  return (s_pfn_Get_Length_Sid (sid));
}

static BOOL WINAPI
copy_sid (DWORD destlen, PSID dest, PSID src)
{
  static CopySid_Proc s_pfn_Copy_Sid = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_copy_sid == 0)
    {
      g_b_init_copy_sid = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Copy_Sid =
        (CopySid_Proc) GetProcAddress (
            hm_advapi32, "CopySid");
    }
  if (s_pfn_Copy_Sid == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Copy_Sid (destlen, dest, src));
}

/*
  END: Wrapper functions around OpenProcessToken
  and other functions in advapi32.dll that are only
  supported in Windows NT / 2k / XP
*/

static void WINAPI
get_native_system_info (LPSYSTEM_INFO lpSystemInfo)
{
  static GetNativeSystemInfo_Proc s_pfn_Get_Native_System_Info = NULL;
  if (is_windows_9x () != TRUE)
    {
      if (g_b_init_get_native_system_info == 0)
	{
	  g_b_init_get_native_system_info = 1;
	  s_pfn_Get_Native_System_Info =
	    (GetNativeSystemInfo_Proc)GetProcAddress (GetModuleHandle ("kernel32.dll"),
						      "GetNativeSystemInfo");
	}
      if (s_pfn_Get_Native_System_Info != NULL)
	s_pfn_Get_Native_System_Info (lpSystemInfo);
    }
  else
    lpSystemInfo->dwNumberOfProcessors = -1;
}

static BOOL WINAPI
get_system_times (LPFILETIME lpIdleTime,
		  LPFILETIME lpKernelTime,
		  LPFILETIME lpUserTime)
{
  static GetSystemTimes_Proc s_pfn_Get_System_times = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_system_times == 0)
    {
      g_b_init_get_system_times = 1;
      s_pfn_Get_System_times =
	(GetSystemTimes_Proc)GetProcAddress (GetModuleHandle ("kernel32.dll"),
					     "GetSystemTimes");
    }
  if (s_pfn_Get_System_times == NULL)
    return FALSE;
  return (s_pfn_Get_System_times (lpIdleTime, lpKernelTime, lpUserTime));
}

/* Equivalent of strerror for W32 error codes.  */
char *
w32_strerror (int error_no)
{
  static char buf[500];

  if (error_no == 0)
    error_no = GetLastError ();

  buf[0] = '\0';
  if (!FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM, NULL,
		      error_no,
		      0, /* choose most suitable language */
		      buf, sizeof (buf), NULL))
    sprintf (buf, "w32 error %u", error_no);
  return buf;
}

/* Return 1 if P is a valid pointer to an object of size SIZE.  Return
   0 if P is NOT a valid pointer.  Return -1 if we cannot validate P.

   This is called from alloc.c:valid_pointer_p.  */
int
w32_valid_pointer_p (void *p, int size)
{
  SIZE_T done;
  HANDLE h = OpenProcess (PROCESS_VM_READ, FALSE, GetCurrentProcessId ());

  if (h)
    {
      unsigned char *buf = alloca (size);
      int retval = ReadProcessMemory (h, p, buf, size, &done);

      CloseHandle (h);
      return retval;
    }
  else
    return -1;
}

static char startup_dir[MAXPATHLEN];

/* Get the current working directory.  */
char *
getwd (char *dir)
{
#if 0
  if (GetCurrentDirectory (MAXPATHLEN, dir) > 0)
    return dir;
  return NULL;
#else
  /* Emacs doesn't actually change directory itself, and we want to
     force our real wd to be where emacs.exe is to avoid unnecessary
     conflicts when trying to rename or delete directories.  */
  strcpy (dir, startup_dir);
  return dir;
#endif
}

/* Emulate getloadavg.  */

struct load_sample {
  time_t sample_time;
  ULONGLONG idle;
  ULONGLONG kernel;
  ULONGLONG user;
};

/* Number of processors on this machine.  */
static unsigned num_of_processors;

/* We maintain 1-sec samples for the last 16 minutes in a circular buffer.  */
static struct load_sample samples[16*60];
static int first_idx = -1, last_idx = -1;
static int max_idx = sizeof (samples) / sizeof (samples[0]);

static int
buf_next (int from)
{
  int next_idx = from + 1;

  if (next_idx >= max_idx)
    next_idx = 0;

  return next_idx;
}

static int
buf_prev (int from)
{
  int prev_idx = from - 1;

  if (prev_idx < 0)
    prev_idx = max_idx - 1;

  return prev_idx;
}

static void
sample_system_load (ULONGLONG *idle, ULONGLONG *kernel, ULONGLONG *user)
{
  SYSTEM_INFO sysinfo;
  FILETIME ft_idle, ft_user, ft_kernel;

  /* Initialize the number of processors on this machine.  */
  if (num_of_processors <= 0)
    {
      get_native_system_info (&sysinfo);
      num_of_processors = sysinfo.dwNumberOfProcessors;
      if (num_of_processors <= 0)
	{
	  GetSystemInfo (&sysinfo);
	  num_of_processors = sysinfo.dwNumberOfProcessors;
	}
      if (num_of_processors <= 0)
	num_of_processors = 1;
    }

  /* TODO: Take into account threads that are ready to run, by
     sampling the "\System\Processor Queue Length" performance
     counter.  The code below accounts only for threads that are
     actually running.  */

  if (get_system_times (&ft_idle, &ft_kernel, &ft_user))
    {
      ULARGE_INTEGER uidle, ukernel, uuser;

      memcpy (&uidle, &ft_idle, sizeof (ft_idle));
      memcpy (&ukernel, &ft_kernel, sizeof (ft_kernel));
      memcpy (&uuser, &ft_user, sizeof (ft_user));
      *idle = uidle.QuadPart;
      *kernel = ukernel.QuadPart;
      *user = uuser.QuadPart;
    }
  else
    {
      *idle = 0;
      *kernel = 0;
      *user = 0;
    }
}

/* Produce the load average for a given time interval, using the
   samples in the samples[] array.  WHICH can be 0, 1, or 2, meaning
   1-minute, 5-minute, or 15-minute average, respectively. */
static double
getavg (int which)
{
  double retval = -1.0;
  double tdiff;
  int idx;
  double span = (which == 0 ? 1.0 : (which == 1 ? 5.0 : 15.0)) * 60;
  time_t now = samples[last_idx].sample_time;

  if (first_idx != last_idx)
    {
      for (idx = buf_prev (last_idx); ; idx = buf_prev (idx))
	{
	  tdiff = difftime (now, samples[idx].sample_time);
	  if (tdiff >= span - 2*DBL_EPSILON*now)
	    {
	      long double sys =
		samples[last_idx].kernel + samples[last_idx].user
		- (samples[idx].kernel + samples[idx].user);
	      long double idl = samples[last_idx].idle - samples[idx].idle;

	      retval = (1.0 - idl / sys) * num_of_processors;
	      break;
	    }
	  if (idx == first_idx)
	    break;
	}
    }

  return retval;
}

int
getloadavg (double loadavg[], int nelem)
{
  int elem;
  ULONGLONG idle, kernel, user;
  time_t now = time (NULL);

  /* Store another sample.  We ignore samples that are less than 1 sec
     apart.  */
  if (difftime (now, samples[last_idx].sample_time) >= 1.0 - 2*DBL_EPSILON*now)
    {
      sample_system_load (&idle, &kernel, &user);
      last_idx = buf_next (last_idx);
      samples[last_idx].sample_time = now;
      samples[last_idx].idle = idle;
      samples[last_idx].kernel = kernel;
      samples[last_idx].user = user;
      /* If the buffer has more that 15 min worth of samples, discard
	 the old ones.  */
      if (first_idx == -1)
	first_idx = last_idx;
      while (first_idx != last_idx
	     && (difftime (now, samples[first_idx].sample_time)
	         >= 15.0*60 + 2*DBL_EPSILON*now))
	first_idx = buf_next (first_idx);
    }

  for (elem = 0; elem < nelem; elem++)
    {
      double avg = getavg (elem);

      if (avg < 0)
	break;
      loadavg[elem] = avg;
    }

  return elem;
}

/* Emulate getpwuid, getpwnam and others.  */

#define PASSWD_FIELD_SIZE 256

static char dflt_passwd_name[PASSWD_FIELD_SIZE];
static char dflt_passwd_passwd[PASSWD_FIELD_SIZE];
static char dflt_passwd_gecos[PASSWD_FIELD_SIZE];
static char dflt_passwd_dir[PASSWD_FIELD_SIZE];
static char dflt_passwd_shell[PASSWD_FIELD_SIZE];

static struct passwd dflt_passwd =
{
  dflt_passwd_name,
  dflt_passwd_passwd,
  0,
  0,
  0,
  dflt_passwd_gecos,
  dflt_passwd_dir,
  dflt_passwd_shell,
};

static char dflt_group_name[GNLEN+1];

static struct group dflt_group =
{
  /* When group information is not available, we return this as the
     group for all files.  */
  dflt_group_name,
  0,
};

unsigned
getuid (void)
{
  return dflt_passwd.pw_uid;
}

unsigned
geteuid (void)
{
  /* I could imagine arguing for checking to see whether the user is
     in the Administrators group and returning a UID of 0 for that
     case, but I don't know how wise that would be in the long run.  */
  return getuid ();
}

unsigned
getgid (void)
{
  return dflt_passwd.pw_gid;
}

unsigned
getegid (void)
{
  return getgid ();
}

struct passwd *
getpwuid (unsigned uid)
{
  if (uid == dflt_passwd.pw_uid)
    return &dflt_passwd;
  return NULL;
}

struct group *
getgrgid (gid_t gid)
{
  return &dflt_group;
}

struct passwd *
getpwnam (char *name)
{
  struct passwd *pw;

  pw = getpwuid (getuid ());
  if (!pw)
    return pw;

  if (xstrcasecmp (name, pw->pw_name))
    return NULL;

  return pw;
}

static void
init_user_info (void)
{
  /* Find the user's real name by opening the process token and
     looking up the name associated with the user-sid in that token.

     Use the relative portion of the identifier authority value from
     the user-sid as the user id value (same for group id using the
     primary group sid from the process token). */

  char         uname[UNLEN+1], gname[GNLEN+1], domain[1025];
  DWORD        ulength = sizeof (uname), dlength = sizeof (domain), needed;
  DWORD	       glength = sizeof (gname);
  HANDLE       token = NULL;
  SID_NAME_USE user_type;
  unsigned char *buf = NULL;
  DWORD        blen = 0;
  TOKEN_USER   user_token;
  TOKEN_PRIMARY_GROUP group_token;
  BOOL         result;

  result = open_process_token (GetCurrentProcess (), TOKEN_QUERY, &token);
  if (result)
    {
      result = get_token_information (token, TokenUser, NULL, 0, &blen);
      if (!result && GetLastError () == ERROR_INSUFFICIENT_BUFFER)
	{
	  buf = xmalloc (blen);
	  result = get_token_information (token, TokenUser,
					  (LPVOID)buf, blen, &needed);
	  if (result)
	    {
	      memcpy (&user_token, buf, sizeof (user_token));
	      result = lookup_account_sid (NULL, user_token.User.Sid,
					   uname, &ulength,
					   domain, &dlength, &user_type);
	    }
	}
      else
	result = FALSE;
    }
  if (result)
    {
      strcpy (dflt_passwd.pw_name, uname);
      /* Determine a reasonable uid value.  */
      if (xstrcasecmp ("administrator", uname) == 0)
	{
	  dflt_passwd.pw_uid = 500; /* well-known Administrator uid */
	  dflt_passwd.pw_gid = 513; /* well-known None gid */
	}
      else
	{
	  /* Use the last sub-authority value of the RID, the relative
	     portion of the SID, as user/group ID. */
	  dflt_passwd.pw_uid = get_rid (user_token.User.Sid);

	  /* Get group id and name.  */
	  result = get_token_information (token, TokenPrimaryGroup,
					  (LPVOID)buf, blen, &needed);
	  if (!result && GetLastError () == ERROR_INSUFFICIENT_BUFFER)
	    {
	      buf = xrealloc (buf, blen = needed);
	      result = get_token_information (token, TokenPrimaryGroup,
					      (LPVOID)buf, blen, &needed);
	    }
	  if (result)
	    {
	      memcpy (&group_token, buf, sizeof (group_token));
	      dflt_passwd.pw_gid = get_rid (group_token.PrimaryGroup);
	      dlength = sizeof (domain);
	      /* If we can get at the real Primary Group name, use that.
		 Otherwise, the default group name was already set to
		 "None" in globals_of_w32.  */
	      if (lookup_account_sid (NULL, group_token.PrimaryGroup,
				      gname, &glength, NULL, &dlength,
				      &user_type))
		strcpy (dflt_group_name, gname);
	    }
	  else
	    dflt_passwd.pw_gid = dflt_passwd.pw_uid;
	}
    }
  /* If security calls are not supported (presumably because we
     are running under Windows 9X), fallback to this: */
  else if (GetUserName (uname, &ulength))
    {
      strcpy (dflt_passwd.pw_name, uname);
      if (xstrcasecmp ("administrator", uname) == 0)
	dflt_passwd.pw_uid = 0;
      else
	dflt_passwd.pw_uid = 123;
      dflt_passwd.pw_gid = dflt_passwd.pw_uid;
    }
  else
    {
      strcpy (dflt_passwd.pw_name, "unknown");
      dflt_passwd.pw_uid = 123;
      dflt_passwd.pw_gid = 123;
    }
  dflt_group.gr_gid = dflt_passwd.pw_gid;

  /* Ensure HOME and SHELL are defined. */
  if (getenv ("HOME") == NULL)
    abort ();
  if (getenv ("SHELL") == NULL)
    abort ();

  /* Set dir and shell from environment variables. */
  strcpy (dflt_passwd.pw_dir, getenv ("HOME"));
  strcpy (dflt_passwd.pw_shell, getenv ("SHELL"));

  xfree (buf);
  if (token)
    CloseHandle (token);
}

int
random (void)
{
  /* rand () on NT gives us 15 random bits...hack together 30 bits.  */
  return ((rand () << 15) | rand ());
}

void
srandom (int seed)
{
  srand (seed);
}


/* Normalize filename by converting all path separators to
   the specified separator.  Also conditionally convert upper
   case path name components to lower case.  */

static void
normalize_filename (register char *fp, char path_sep)
{
  char sep;
  char *elem;

  /* Always lower-case drive letters a-z, even if the filesystem
     preserves case in filenames.
     This is so filenames can be compared by string comparison
     functions that are case-sensitive.  Even case-preserving filesystems
     do not distinguish case in drive letters.  */
  if (fp[1] == ':' && *fp >= 'A' && *fp <= 'Z')
    {
      *fp += 'a' - 'A';
      fp += 2;
    }

  if (NILP (Vw32_downcase_file_names))
    {
      while (*fp)
	{
	  if (*fp == '/' || *fp == '\\')
	    *fp = path_sep;
	  fp++;
	}
      return;
    }

  sep = path_sep;		/* convert to this path separator */
  elem = fp;			/* start of current path element */

  do {
    if (*fp >= 'a' && *fp <= 'z')
      elem = 0;			/* don't convert this element */

    if (*fp == 0 || *fp == ':')
      {
	sep = *fp;		/* restore current separator (or 0) */
	*fp = '/';		/* after conversion of this element */
      }

    if (*fp == '/' || *fp == '\\')
      {
	if (elem && elem != fp)
	  {
	    *fp = 0;		/* temporary end of string */
	    _strlwr (elem);	/* while we convert to lower case */
	  }
	*fp = sep;		/* convert (or restore) path separator */
	elem = fp + 1;		/* next element starts after separator */
	sep = path_sep;
      }
  } while (*fp++);
}

/* Destructively turn backslashes into slashes.  */
void
dostounix_filename (register char *p)
{
  normalize_filename (p, '/');
}

/* Destructively turn slashes into backslashes.  */
void
unixtodos_filename (register char *p)
{
  normalize_filename (p, '\\');
}

/* Remove all CR's that are followed by a LF.
   (From msdos.c...probably should figure out a way to share it,
   although this code isn't going to ever change.)  */
static int
crlf_to_lf (register int n, register unsigned char *buf)
{
  unsigned char *np = buf;
  unsigned char *startp = buf;
  unsigned char *endp = buf + n;

  if (n == 0)
    return n;
  while (buf < endp - 1)
    {
      if (*buf == 0x0d)
	{
	  if (*(++buf) != 0x0a)
	    *np++ = 0x0d;
	}
      else
	*np++ = *buf++;
    }
  if (buf < endp)
    *np++ = *buf++;
  return np - startp;
}

/* Parse the root part of file name, if present.  Return length and
    optionally store pointer to char after root.  */
static int
parse_root (char * name, char ** pPath)
{
  char * start = name;

  if (name == NULL)
    return 0;

  /* find the root name of the volume if given */
  if (isalpha (name[0]) && name[1] == ':')
    {
      /* skip past drive specifier */
      name += 2;
      if (IS_DIRECTORY_SEP (name[0]))
	name++;
    }
  else if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
    {
      int slashes = 2;
      name += 2;
      do
        {
	  if (IS_DIRECTORY_SEP (*name) && --slashes == 0)
	    break;
	  name++;
	}
      while ( *name );
      if (IS_DIRECTORY_SEP (name[0]))
	name++;
    }

  if (pPath)
    *pPath = name;

  return name - start;
}

/* Get long base name for name; name is assumed to be absolute.  */
static int
get_long_basename (char * name, char * buf, int size)
{
  WIN32_FIND_DATA find_data;
  HANDLE dir_handle;
  int len = 0;

  /* must be valid filename, no wild cards or other invalid characters */
  if (_mbspbrk (name, "*?|<>\""))
    return 0;

  dir_handle = FindFirstFile (name, &find_data);
  if (dir_handle != INVALID_HANDLE_VALUE)
    {
      if ((len = strlen (find_data.cFileName)) < size)
	memcpy (buf, find_data.cFileName, len + 1);
      else
	len = 0;
      FindClose (dir_handle);
    }
  return len;
}

/* Get long name for file, if possible (assumed to be absolute).  */
BOOL
w32_get_long_filename (char * name, char * buf, int size)
{
  char * o = buf;
  char * p;
  char * q;
  char full[ MAX_PATH ];
  int len;

  len = strlen (name);
  if (len >= MAX_PATH)
    return FALSE;

  /* Use local copy for destructive modification.  */
  memcpy (full, name, len+1);
  unixtodos_filename (full);

  /* Copy root part verbatim.  */
  len = parse_root (full, &p);
  memcpy (o, full, len);
  o += len;
  *o = '\0';
  size -= len;

  while (p != NULL && *p)
    {
      q = p;
      p = strchr (q, '\\');
      if (p) *p = '\0';
      len = get_long_basename (full, o, size);
      if (len > 0)
	{
	  o += len;
	  size -= len;
	  if (p != NULL)
	    {
	      *p++ = '\\';
	      if (size < 2)
		return FALSE;
	      *o++ = '\\';
	      size--;
	      *o = '\0';
	    }
	}
      else
	return FALSE;
    }

  return TRUE;
}

static int
is_unc_volume (const char *filename)
{
  const char *ptr = filename;

  if (!IS_DIRECTORY_SEP (ptr[0]) || !IS_DIRECTORY_SEP (ptr[1]) || !ptr[2])
    return 0;

  if (_mbspbrk (ptr + 2, "*?|<>\"\\/"))
    return 0;

  return 1;
}

/* Routines that are no-ops on NT but are defined to get Emacs to compile.  */

int
sigsetmask (int signal_mask)
{
  return 0;
}

int
sigmask (int sig)
{
  return 0;
}

int
sigblock (int sig)
{
  return 0;
}

int
sigunblock (int sig)
{
  return 0;
}

int
sigemptyset (sigset_t *set)
{
  return 0;
}

int
sigaddset (sigset_t *set, int signo)
{
  return 0;
}

int
sigfillset (sigset_t *set)
{
  return 0;
}

int
sigprocmask (int how, const sigset_t *set, sigset_t *oset)
{
  return 0;
}

int
pthread_sigmask (int how, const sigset_t *set, sigset_t *oset)
{
  if (sigprocmask (how, set, oset) == -1)
    return EINVAL;
  return 0;
}

int
setpgrp (int pid, int gid)
{
  return 0;
}

int
alarm (int seconds)
{
  return 0;
}

#define REG_ROOT "SOFTWARE\\GNU\\Emacs"

LPBYTE
w32_get_resource (char *key, LPDWORD lpdwtype)
{
  LPBYTE lpvalue;
  HKEY hrootkey = NULL;
  DWORD cbData;

  /* Check both the current user and the local machine to see if
     we have any resources.  */

  if (RegOpenKeyEx (HKEY_CURRENT_USER, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
    {
      lpvalue = NULL;

      if (RegQueryValueEx (hrootkey, key, NULL, NULL, NULL, &cbData) == ERROR_SUCCESS
	  && (lpvalue = (LPBYTE) xmalloc (cbData)) != NULL
	  && RegQueryValueEx (hrootkey, key, NULL, lpdwtype, lpvalue, &cbData) == ERROR_SUCCESS)
	{
          RegCloseKey (hrootkey);
	  return (lpvalue);
	}

      xfree (lpvalue);

      RegCloseKey (hrootkey);
    }

  if (RegOpenKeyEx (HKEY_LOCAL_MACHINE, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
    {
      lpvalue = NULL;

      if (RegQueryValueEx (hrootkey, key, NULL, NULL, NULL, &cbData) == ERROR_SUCCESS
	  && (lpvalue = (LPBYTE) xmalloc (cbData)) != NULL
	  && RegQueryValueEx (hrootkey, key, NULL, lpdwtype, lpvalue, &cbData) == ERROR_SUCCESS)
	{
          RegCloseKey (hrootkey);
	  return (lpvalue);
	}

      xfree (lpvalue);

      RegCloseKey (hrootkey);
    }

  return (NULL);
}

char *get_emacs_configuration (void);

void
init_environment (char ** argv)
{
  static const char * const tempdirs[] = {
    "$TMPDIR", "$TEMP", "$TMP", "c:/"
  };

  int i;

  const int imax = sizeof (tempdirs) / sizeof (tempdirs[0]);

  /* Make sure they have a usable $TMPDIR.  Many Emacs functions use
     temporary files and assume "/tmp" if $TMPDIR is unset, which
     will break on DOS/Windows.  Refuse to work if we cannot find
     a directory, not even "c:/", usable for that purpose.  */
  for (i = 0; i < imax ; i++)
    {
      const char *tmp = tempdirs[i];

      if (*tmp == '$')
	tmp = getenv (tmp + 1);
      /* Note that `access' can lie to us if the directory resides on a
	 read-only filesystem, like CD-ROM or a write-protected floppy.
	 The only way to be really sure is to actually create a file and
	 see if it succeeds.  But I think that's too much to ask.  */
#ifdef _MSC_VER
      /* MSVC's _access crashes with D_OK.  */
      if (tmp && sys_access (tmp, D_OK) == 0)
#else
      if (tmp && _access (tmp, D_OK) == 0)
#endif
	{
	  char * var = alloca (strlen (tmp) + 8);
	  sprintf (var, "TMPDIR=%s", tmp);
	  _putenv (strdup (var));
	  break;
	}
    }
  if (i >= imax)
    cmd_error_internal
      (Fcons (Qerror,
	      Fcons (build_string ("no usable temporary directories found!!"),
		     Qnil)),
       "While setting TMPDIR: ");

  /* Check for environment variables and use registry settings if they
     don't exist.  Fallback on default values where applicable.  */
  {
    int i;
    LPBYTE lpval;
    DWORD dwType;
    char locale_name[32];
    struct stat ignored;
    char default_home[MAX_PATH];
    int appdata = 0;

    static const struct env_entry
    {
      char * name;
      char * def_value;
    } dflt_envvars[] =
    {
      {"HOME", "C:/"},
      {"PRELOAD_WINSOCK", NULL},
      {"emacs_dir", "C:/emacs"},
      {"EMACSLOADPATH", "%emacs_dir%/site-lisp;%emacs_dir%/../site-lisp;%emacs_dir%/lisp;%emacs_dir%/leim"},
      {"SHELL", "%emacs_dir%/bin/cmdproxy.exe"},
      {"EMACSDATA", "%emacs_dir%/etc"},
      {"EMACSPATH", "%emacs_dir%/bin"},
      /* We no longer set INFOPATH because Info-default-directory-list
	 is then ignored.  */
      /*  {"INFOPATH", "%emacs_dir%/info"},  */
      {"EMACSDOC", "%emacs_dir%/etc"},
      {"TERM", "cmd"},
      {"LANG", NULL},
    };

#define N_ENV_VARS sizeof (dflt_envvars)/sizeof (dflt_envvars[0])

    /* We need to copy dflt_envvars[] and work on the copy because we
       don't want the dumped Emacs to inherit the values of
       environment variables we saw during dumping (which could be on
       a different system).  The defaults above must be left intact.  */
    struct env_entry env_vars[N_ENV_VARS];

    for (i = 0; i < N_ENV_VARS; i++)
      env_vars[i] = dflt_envvars[i];

    /* For backwards compatibility, check if a .emacs file exists in C:/
       If not, then we can try to default to the appdata directory under the
       user's profile, which is more likely to be writable.   */
    if (stat ("C:/.emacs", &ignored) < 0)
      {
	HRESULT profile_result;
	/* Dynamically load ShGetFolderPath, as it won't exist on versions
	   of Windows 95 and NT4 that have not been updated to include
	   MSIE 5.  */
	ShGetFolderPath_fn get_folder_path;
	get_folder_path = (ShGetFolderPath_fn)
	  GetProcAddress (GetModuleHandle ("shell32.dll"), "SHGetFolderPathA");

	if (get_folder_path != NULL)
	  {
	    profile_result = get_folder_path (NULL, CSIDL_APPDATA, NULL,
					      0, default_home);

	    /* If we can't get the appdata dir, revert to old behavior.	 */
	    if (profile_result == S_OK)
	      {
		env_vars[0].def_value = default_home;
		appdata = 1;
	      }
	  }
      }

  /* Get default locale info and use it for LANG.  */
  if (GetLocaleInfo (LOCALE_USER_DEFAULT,
                     LOCALE_SABBREVLANGNAME | LOCALE_USE_CP_ACP,
                     locale_name, sizeof (locale_name)))
    {
      for (i = 0; i < N_ENV_VARS; i++)
        {
          if (strcmp (env_vars[i].name, "LANG") == 0)
            {
              env_vars[i].def_value = locale_name;
              break;
            }
        }
    }

  /* When Emacs is invoked with --no-site-lisp, we must remove the
     site-lisp directories from the default value of EMACSLOADPATH.
     This assumes that the site-lisp entries are at the front, and
     that additional entries do exist.  */
  if (no_site_lisp)
    {
      for (i = 0; i < N_ENV_VARS; i++)
        {
          if (strcmp (env_vars[i].name, "EMACSLOADPATH") == 0)
            {
              char *site;
              while ((site = strstr (env_vars[i].def_value, "site-lisp")))
                env_vars[i].def_value = strchr (site, ';') + 1;
              break;
            }
        }
    }

#define SET_ENV_BUF_SIZE (4 * MAX_PATH)	/* to cover EMACSLOADPATH */

    /* Treat emacs_dir specially: set it unconditionally based on our
       location, if it appears that we are running from the bin subdir
       of a standard installation.  */
    {
      char *p;
      char modname[MAX_PATH];

      if (!GetModuleFileName (NULL, modname, MAX_PATH))
	abort ();
      if ((p = strrchr (modname, '\\')) == NULL)
	abort ();
      *p = 0;

      if ((p = strrchr (modname, '\\')) && xstrcasecmp (p, "\\bin") == 0)
	{
	  char buf[SET_ENV_BUF_SIZE];

	  *p = 0;
	  for (p = modname; *p; p++)
	    if (*p == '\\') *p = '/';

	  _snprintf (buf, sizeof (buf)-1, "emacs_dir=%s", modname);
	  _putenv (strdup (buf));
	}
      /* Handle running emacs from the build directory: src/oo-spd/i386/  */

      /* FIXME: should use substring of get_emacs_configuration ().
	 But I don't think the Windows build supports alpha, mips etc
         anymore, so have taken the easy option for now.  */
      else if (p && xstrcasecmp (p, "\\i386") == 0)
	{
	  *p = 0;
	  p = strrchr (modname, '\\');
	  if (p != NULL)
	    {
	      *p = 0;
	      p = strrchr (modname, '\\');
	      if (p && xstrcasecmp (p, "\\src") == 0)
		{
		  char buf[SET_ENV_BUF_SIZE];

		  *p = 0;
		  for (p = modname; *p; p++)
		    if (*p == '\\') *p = '/';

		  _snprintf (buf, sizeof (buf)-1, "emacs_dir=%s", modname);
		  _putenv (strdup (buf));
		}
	    }
	}
    }

    for (i = 0; i < N_ENV_VARS; i++)
      {
	if (!getenv (env_vars[i].name))
	  {
	    int dont_free = 0;

	    if ((lpval = w32_get_resource (env_vars[i].name, &dwType)) == NULL
		/* Also ignore empty environment variables.  */
		|| *lpval == 0)
	      {
		xfree (lpval);
		lpval = env_vars[i].def_value;
		dwType = REG_EXPAND_SZ;
		dont_free = 1;
		if (!strcmp (env_vars[i].name, "HOME") && !appdata)
		  {
		    Lisp_Object warning[2];
		    warning[0] = intern ("initialization");
		    warning[1] = build_string ("Setting HOME to C:\\ by default is deprecated");
		    Vdelayed_warnings_list = Fcons (Flist (2, warning),
						    Vdelayed_warnings_list);
		  }
	      }

	    if (lpval)
	      {
		char buf1[SET_ENV_BUF_SIZE], buf2[SET_ENV_BUF_SIZE];

		if (dwType == REG_EXPAND_SZ)
		  ExpandEnvironmentStrings ((LPSTR) lpval, buf1, sizeof (buf1));
		else if (dwType == REG_SZ)
		  strcpy (buf1, lpval);
		if (dwType == REG_EXPAND_SZ || dwType == REG_SZ)
		  {
		    _snprintf (buf2, sizeof (buf2)-1, "%s=%s", env_vars[i].name,
			       buf1);
		    _putenv (strdup (buf2));
		  }

		if (!dont_free)
		  xfree (lpval);
	      }
	  }
      }
  }

  /* Rebuild system configuration to reflect invoking system.  */
  Vsystem_configuration = build_string (EMACS_CONFIGURATION);

  /* Another special case: on NT, the PATH variable is actually named
     "Path" although cmd.exe (perhaps NT itself) arranges for
     environment variable lookup and setting to be case insensitive.
     However, Emacs assumes a fully case sensitive environment, so we
     need to change "Path" to "PATH" to match the expectations of
     various elisp packages.  We do this by the sneaky method of
     modifying the string in the C runtime environ entry.

     The same applies to COMSPEC.  */
  {
    char ** envp;

    for (envp = environ; *envp; envp++)
      if (_strnicmp (*envp, "PATH=", 5) == 0)
	memcpy (*envp, "PATH=", 5);
      else if (_strnicmp (*envp, "COMSPEC=", 8) == 0)
	memcpy (*envp, "COMSPEC=", 8);
  }

  /* Remember the initial working directory for getwd, then make the
     real wd be the location of emacs.exe to avoid conflicts when
     renaming or deleting directories.  (We also don't call chdir when
     running subprocesses for the same reason.)  */
  if (!GetCurrentDirectory (MAXPATHLEN, startup_dir))
    abort ();

  {
    char *p;
    static char modname[MAX_PATH];

    if (!GetModuleFileName (NULL, modname, MAX_PATH))
      abort ();
    if ((p = strrchr (modname, '\\')) == NULL)
      abort ();
    *p = 0;

    SetCurrentDirectory (modname);

    /* Ensure argv[0] has the full path to Emacs.  */
    *p = '\\';
    argv[0] = modname;
  }

  /* Determine if there is a middle mouse button, to allow parse_button
     to decide whether right mouse events should be mouse-2 or
     mouse-3. */
  w32_num_mouse_buttons = GetSystemMetrics (SM_CMOUSEBUTTONS);

  init_user_info ();
}

char *
emacs_root_dir (void)
{
  static char root_dir[FILENAME_MAX];
  const char *p;

  p = getenv ("emacs_dir");
  if (p == NULL)
    abort ();
  strcpy (root_dir, p);
  root_dir[parse_root (root_dir, NULL)] = '\0';
  dostounix_filename (root_dir);
  return root_dir;
}

/* We don't have scripts to automatically determine the system configuration
   for Emacs before it's compiled, and we don't want to have to make the
   user enter it, so we define EMACS_CONFIGURATION to invoke this runtime
   routine.  */

char *
get_emacs_configuration (void)
{
  char *arch, *oem, *os;
  int build_num;
  static char configuration_buffer[32];

  /* Determine the processor type.  */
  switch (get_processor_type ())
    {

#ifdef PROCESSOR_INTEL_386
    case PROCESSOR_INTEL_386:
    case PROCESSOR_INTEL_486:
    case PROCESSOR_INTEL_PENTIUM:
      arch = "i386";
      break;
#endif

#ifdef PROCESSOR_MIPS_R2000
    case PROCESSOR_MIPS_R2000:
    case PROCESSOR_MIPS_R3000:
    case PROCESSOR_MIPS_R4000:
      arch = "mips";
      break;
#endif

#ifdef PROCESSOR_ALPHA_21064
    case PROCESSOR_ALPHA_21064:
      arch = "alpha";
      break;
#endif

    default:
      arch = "unknown";
      break;
    }

  /* Use the OEM field to reflect the compiler/library combination.  */
#ifdef _MSC_VER
#define COMPILER_NAME	"msvc"
#else
#ifdef __GNUC__
#define COMPILER_NAME	"mingw"
#else
#define COMPILER_NAME	"unknown"
#endif
#endif
  oem = COMPILER_NAME;

  switch (osinfo_cache.dwPlatformId) {
  case VER_PLATFORM_WIN32_NT:
    os = "nt";
    build_num = osinfo_cache.dwBuildNumber;
    break;
  case VER_PLATFORM_WIN32_WINDOWS:
    if (osinfo_cache.dwMinorVersion == 0) {
      os = "windows95";
    } else {
      os = "windows98";
    }
    build_num = LOWORD (osinfo_cache.dwBuildNumber);
    break;
  case VER_PLATFORM_WIN32s:
    /* Not supported, should not happen. */
    os = "windows32s";
    build_num = LOWORD (osinfo_cache.dwBuildNumber);
    break;
  default:
    os = "unknown";
    build_num = 0;
    break;
  }

  if (osinfo_cache.dwPlatformId == VER_PLATFORM_WIN32_NT) {
    sprintf (configuration_buffer, "%s-%s-%s%d.%d.%d", arch, oem, os,
	     get_w32_major_version (), get_w32_minor_version (), build_num);
  } else {
    sprintf (configuration_buffer, "%s-%s-%s.%d", arch, oem, os, build_num);
  }

  return configuration_buffer;
}

char *
get_emacs_configuration_options (void)
{
  static char *options_buffer;
  char cv[32];  /* Enough for COMPILER_VERSION.  */
  char *options[] = {
    cv,  /* To be filled later.  */
#ifdef EMACSDEBUG
    " --no-opt",
#endif
#ifdef ENABLE_CHECKING
    " --enable-checking",
#endif
    /* configure.bat already sets USER_CFLAGS and USER_LDFLAGS
       with a starting space to save work here.  */
#ifdef USER_CFLAGS
    " --cflags", USER_CFLAGS,
#endif
#ifdef USER_LDFLAGS
    " --ldflags", USER_LDFLAGS,
#endif
    NULL
  };
  size_t size = 0;
  int i;

/* Work out the effective configure options for this build.  */
#ifdef _MSC_VER
#define COMPILER_VERSION	"--with-msvc (%d.%02d)", _MSC_VER / 100, _MSC_VER % 100
#else
#ifdef __GNUC__
#define COMPILER_VERSION	"--with-gcc (%d.%d)", __GNUC__, __GNUC_MINOR__
#else
#define COMPILER_VERSION	""
#endif
#endif

  if (_snprintf (cv, sizeof (cv) - 1, COMPILER_VERSION) < 0)
    return "Error: not enough space for compiler version";
  cv[sizeof (cv) - 1] = '\0';

  for (i = 0; options[i]; i++)
    size += strlen (options[i]);

  options_buffer = xmalloc (size + 1);
  options_buffer[0] = '\0';

  for (i = 0; options[i]; i++)
    strcat (options_buffer, options[i]);

  return options_buffer;
}


#include <sys/timeb.h>

/* Emulate gettimeofday (Ulrich Leodolter, 1/11/95).  */
void
gettimeofday (struct timeval *tv, struct timezone *tz)
{
  struct _timeb tb;
  _ftime (&tb);

  tv->tv_sec = tb.time;
  tv->tv_usec = tb.millitm * 1000L;
  /* Implementation note: _ftime sometimes doesn't update the dstflag
     according to the new timezone when the system timezone is
     changed.  We could fix that by using GetSystemTime and
     GetTimeZoneInformation, but that doesn't seem necessary, since
     Emacs always calls gettimeofday with the 2nd argument NULL (see
     EMACS_GET_TIME).  */
  if (tz)
    {
      tz->tz_minuteswest = tb.timezone;	/* minutes west of Greenwich  */
      tz->tz_dsttime = tb.dstflag;	/* type of dst correction  */
    }
}

/* ------------------------------------------------------------------------- */
/* IO support and wrapper functions for W32 API. */
/* ------------------------------------------------------------------------- */

/* Place a wrapper around the MSVC version of ctime.  It returns NULL
   on network directories, so we handle that case here.
   (Ulrich Leodolter, 1/11/95).  */
char *
sys_ctime (const time_t *t)
{
  char *str = (char *) ctime (t);
  return (str ? str : "Sun Jan 01 00:00:00 1970");
}

/* Emulate sleep...we could have done this with a define, but that
   would necessitate including windows.h in the files that used it.
   This is much easier.  */
void
sys_sleep (int seconds)
{
  Sleep (seconds * 1000);
}

/* Internal MSVC functions for low-level descriptor munging */
extern int __cdecl _set_osfhnd (int fd, long h);
extern int __cdecl _free_osfhnd (int fd);

/* parallel array of private info on file handles */
filedesc fd_info [ MAXDESC ];

typedef struct volume_info_data {
  struct volume_info_data * next;

  /* time when info was obtained */
  DWORD     timestamp;

  /* actual volume info */
  char *    root_dir;
  DWORD     serialnum;
  DWORD     maxcomp;
  DWORD     flags;
  char *    name;
  char *    type;
} volume_info_data;

/* Global referenced by various functions.  */
static volume_info_data volume_info;

/* Vector to indicate which drives are local and fixed (for which cached
   data never expires).  */
static BOOL fixed_drives[26];

/* Consider cached volume information to be stale if older than 10s,
   at least for non-local drives.  Info for fixed drives is never stale.  */
#define DRIVE_INDEX( c ) ( (c) <= 'Z' ? (c) - 'A' : (c) - 'a' )
#define VOLINFO_STILL_VALID( root_dir, info )		\
  ( ( isalpha (root_dir[0]) &&				\
      fixed_drives[ DRIVE_INDEX (root_dir[0]) ] )	\
    || GetTickCount () - info->timestamp < 10000 )

/* Cache support functions.  */

/* Simple linked list with linear search is sufficient.  */
static volume_info_data *volume_cache = NULL;

static volume_info_data *
lookup_volume_info (char * root_dir)
{
  volume_info_data * info;

  for (info = volume_cache; info; info = info->next)
    if (xstrcasecmp (info->root_dir, root_dir) == 0)
      break;
  return info;
}

static void
add_volume_info (char * root_dir, volume_info_data * info)
{
  info->root_dir = xstrdup (root_dir);
  info->next = volume_cache;
  volume_cache = info;
}


/* Wrapper for GetVolumeInformation, which uses caching to avoid
   performance penalty (~2ms on 486 for local drives, 7.5ms for local
   cdrom drive, ~5-10ms or more for remote drives on LAN).  */
static volume_info_data *
GetCachedVolumeInformation (char * root_dir)
{
  volume_info_data * info;
  char default_root[ MAX_PATH ];

  /* NULL for root_dir means use root from current directory.  */
  if (root_dir == NULL)
    {
      if (GetCurrentDirectory (MAX_PATH, default_root) == 0)
	return NULL;
      parse_root (default_root, &root_dir);
      *root_dir = 0;
      root_dir = default_root;
    }

  /* Local fixed drives can be cached permanently.  Removable drives
     cannot be cached permanently, since the volume name and serial
     number (if nothing else) can change.  Remote drives should be
     treated as if they are removable, since there is no sure way to
     tell whether they are or not.  Also, the UNC association of drive
     letters mapped to remote volumes can be changed at any time (even
     by other processes) without notice.

     As a compromise, so we can benefit from caching info for remote
     volumes, we use a simple expiry mechanism to invalidate cache
     entries that are more than ten seconds old.  */

#if 0
  /* No point doing this, because WNetGetConnection is even slower than
     GetVolumeInformation, consistently taking ~50ms on a 486 (FWIW,
     GetDriveType is about the only call of this type which does not
     involve network access, and so is extremely quick).  */

  /* Map drive letter to UNC if remote. */
  if (isalpha (root_dir[0]) && !fixed[DRIVE_INDEX (root_dir[0])])
    {
      char remote_name[ 256 ];
      char drive[3] = { root_dir[0], ':' };

      if (WNetGetConnection (drive, remote_name, sizeof (remote_name))
	  == NO_ERROR)
	/* do something */ ;
    }
#endif

  info = lookup_volume_info (root_dir);

  if (info == NULL || ! VOLINFO_STILL_VALID (root_dir, info))
    {
      char  name[ 256 ];
      DWORD serialnum;
      DWORD maxcomp;
      DWORD flags;
      char  type[ 256 ];

      /* Info is not cached, or is stale. */
      if (!GetVolumeInformation (root_dir,
				 name, sizeof (name),
				 &serialnum,
				 &maxcomp,
				 &flags,
				 type, sizeof (type)))
	return NULL;

      /* Cache the volume information for future use, overwriting existing
	 entry if present.  */
      if (info == NULL)
	{
	  info = (volume_info_data *) xmalloc (sizeof (volume_info_data));
	  add_volume_info (root_dir, info);
	}
      else
	{
	  xfree (info->name);
	  xfree (info->type);
	}

      info->name = xstrdup (name);
      info->serialnum = serialnum;
      info->maxcomp = maxcomp;
      info->flags = flags;
      info->type = xstrdup (type);
      info->timestamp = GetTickCount ();
    }

  return info;
}

/* Get information on the volume where name is held; set path pointer to
   start of pathname in name (past UNC header\volume header if present).  */
static int
get_volume_info (const char * name, const char ** pPath)
{
  char temp[MAX_PATH];
  char *rootname = NULL;  /* default to current volume */
  volume_info_data * info;

  if (name == NULL)
    return FALSE;

  /* find the root name of the volume if given */
  if (isalpha (name[0]) && name[1] == ':')
    {
      rootname = temp;
      temp[0] = *name++;
      temp[1] = *name++;
      temp[2] = '\\';
      temp[3] = 0;
    }
  else if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
    {
      char *str = temp;
      int slashes = 4;
      rootname = temp;
      do
        {
	  if (IS_DIRECTORY_SEP (*name) && --slashes == 0)
	    break;
	  *str++ = *name++;
	}
      while ( *name );

      *str++ = '\\';
      *str = 0;
    }

  if (pPath)
    *pPath = name;

  info = GetCachedVolumeInformation (rootname);
  if (info != NULL)
    {
      /* Set global referenced by other functions.  */
      volume_info = *info;
      return TRUE;
    }
  return FALSE;
}

/* Determine if volume is FAT format (ie. only supports short 8.3
   names); also set path pointer to start of pathname in name.  */
static int
is_fat_volume (const char * name, const char ** pPath)
{
  if (get_volume_info (name, pPath))
    return (volume_info.maxcomp == 12);
  return FALSE;
}

/* Map filename to a valid 8.3 name if necessary. */
const char *
map_w32_filename (const char * name, const char ** pPath)
{
  static char shortname[MAX_PATH];
  char * str = shortname;
  char c;
  char * path;
  const char * save_name = name;

  if (strlen (name) >= MAX_PATH)
    {
      /* Return a filename which will cause callers to fail.  */
      strcpy (shortname, "?");
      return shortname;
    }

  if (is_fat_volume (name, (const char **)&path)) /* truncate to 8.3 */
    {
      register int left = 8;	/* maximum number of chars in part */
      register int extn = 0;	/* extension added? */
      register int dots = 2;	/* maximum number of dots allowed */

      while (name < path)
	*str++ = *name++;	/* skip past UNC header */

      while ((c = *name++))
        {
	  switch ( c )
	    {
	    case '\\':
	    case '/':
	      *str++ = '\\';
	      extn = 0;		/* reset extension flags */
	      dots = 2;		/* max 2 dots */
	      left = 8;		/* max length 8 for main part */
	      break;
	    case ':':
	      *str++ = ':';
	      extn = 0;		/* reset extension flags */
	      dots = 2;		/* max 2 dots */
	      left = 8;		/* max length 8 for main part */
	      break;
	    case '.':
	      if ( dots )
	        {
		  /* Convert path components of the form .xxx to _xxx,
		     but leave . and .. as they are.  This allows .emacs
		     to be read as _emacs, for example.  */

		  if (! *name ||
		      *name == '.' ||
		      IS_DIRECTORY_SEP (*name))
		    {
		      *str++ = '.';
		      dots--;
		    }
		  else
		    {
		      *str++ = '_';
		      left--;
		      dots = 0;
		    }
		}
	      else if ( !extn )
	        {
		  *str++ = '.';
		  extn = 1;		/* we've got an extension */
		  left = 3;		/* 3 chars in extension */
		}
	      else
	        {
		  /* any embedded dots after the first are converted to _ */
		  *str++ = '_';
		}
	      break;
	    case '~':
	    case '#':			/* don't lose these, they're important */
	      if ( ! left )
		str[-1] = c;		/* replace last character of part */
	      /* FALLTHRU */
	    default:
	      if ( left )
	        {
		  *str++ = tolower (c);	/* map to lower case (looks nicer) */
		  left--;
		  dots = 0;		/* started a path component */
		}
	      break;
	    }
	}
      *str = '\0';
    }
  else
    {
      strcpy (shortname, name);
      unixtodos_filename (shortname);
    }

  if (pPath)
    *pPath = shortname + (path - save_name);

  return shortname;
}

static int
is_exec (const char * name)
{
  char * p = strrchr (name, '.');
  return
    (p != NULL
     && (xstrcasecmp (p, ".exe") == 0 ||
	 xstrcasecmp (p, ".com") == 0 ||
	 xstrcasecmp (p, ".bat") == 0 ||
	 xstrcasecmp (p, ".cmd") == 0));
}

/* Emulate the Unix directory procedures opendir, closedir,
   and readdir.  We can't use the procedures supplied in sysdep.c,
   so we provide them here.  */

struct direct dir_static;       /* simulated directory contents */
static HANDLE dir_find_handle = INVALID_HANDLE_VALUE;
static int    dir_is_fat;
static char   dir_pathname[MAXPATHLEN+1];
static WIN32_FIND_DATA dir_find_data;

/* Support shares on a network resource as subdirectories of a read-only
   root directory. */
static HANDLE wnet_enum_handle = INVALID_HANDLE_VALUE;
static HANDLE open_unc_volume (const char *);
static char  *read_unc_volume (HANDLE, char *, int);
static void   close_unc_volume (HANDLE);

DIR *
opendir (char *filename)
{
  DIR *dirp;

  /* Opening is done by FindFirstFile.  However, a read is inherent to
     this operation, so we defer the open until read time.  */

  if (dir_find_handle != INVALID_HANDLE_VALUE)
    return NULL;
  if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    return NULL;

  if (is_unc_volume (filename))
    {
      wnet_enum_handle = open_unc_volume (filename);
      if (wnet_enum_handle == INVALID_HANDLE_VALUE)
	return NULL;
    }

  if (!(dirp = (DIR *) malloc (sizeof (DIR))))
    return NULL;

  dirp->dd_fd = 0;
  dirp->dd_loc = 0;
  dirp->dd_size = 0;

  strncpy (dir_pathname, map_w32_filename (filename, NULL), MAXPATHLEN);
  dir_pathname[MAXPATHLEN] = '\0';
  dir_is_fat = is_fat_volume (filename, NULL);

  return dirp;
}

void
closedir (DIR *dirp)
{
  /* If we have a find-handle open, close it.  */
  if (dir_find_handle != INVALID_HANDLE_VALUE)
    {
      FindClose (dir_find_handle);
      dir_find_handle = INVALID_HANDLE_VALUE;
    }
  else if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    {
      close_unc_volume (wnet_enum_handle);
      wnet_enum_handle = INVALID_HANDLE_VALUE;
    }
  xfree ((char *) dirp);
}

struct direct *
readdir (DIR *dirp)
{
  int downcase = !NILP (Vw32_downcase_file_names);

  if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    {
      if (!read_unc_volume (wnet_enum_handle,
                            dir_find_data.cFileName,
                            MAX_PATH))
	return NULL;
    }
  /* If we aren't dir_finding, do a find-first, otherwise do a find-next. */
  else if (dir_find_handle == INVALID_HANDLE_VALUE)
    {
      char filename[MAXNAMLEN + 3];
      int ln;

      strcpy (filename, dir_pathname);
      ln = strlen (filename) - 1;
      if (!IS_DIRECTORY_SEP (filename[ln]))
	strcat (filename, "\\");
      strcat (filename, "*");

      dir_find_handle = FindFirstFile (filename, &dir_find_data);

      if (dir_find_handle == INVALID_HANDLE_VALUE)
	return NULL;
    }
  else
    {
      if (!FindNextFile (dir_find_handle, &dir_find_data))
	return NULL;
    }

  /* Emacs never uses this value, so don't bother making it match
     value returned by stat().  */
  dir_static.d_ino = 1;

  strcpy (dir_static.d_name, dir_find_data.cFileName);

  /* If the file name in cFileName[] includes `?' characters, it means
     the original file name used characters that cannot be represented
     by the current ANSI codepage.  To avoid total lossage, retrieve
     the short 8+3 alias of the long file name.  */
  if (_mbspbrk (dir_static.d_name, "?"))
    {
      strcpy (dir_static.d_name, dir_find_data.cAlternateFileName);
      downcase = 1;	/* 8+3 aliases are returned in all caps */
    }
  dir_static.d_namlen = strlen (dir_static.d_name);
  dir_static.d_reclen = sizeof (struct direct) - MAXNAMLEN + 3 +
    dir_static.d_namlen - dir_static.d_namlen % 4;

  /* If the file name in cFileName[] includes `?' characters, it means
     the original file name used characters that cannot be represented
     by the current ANSI codepage.  To avoid total lossage, retrieve
     the short 8+3 alias of the long file name.  */
  if (_mbspbrk (dir_find_data.cFileName, "?"))
    {
      strcpy (dir_static.d_name, dir_find_data.cAlternateFileName);
      /* 8+3 aliases are returned in all caps, which could break
	 various alists that look at filenames' extensions.  */
      downcase = 1;
    }
  else
    strcpy (dir_static.d_name, dir_find_data.cFileName);
  dir_static.d_namlen = strlen (dir_static.d_name);
  if (dir_is_fat)
    _strlwr (dir_static.d_name);
  else if (downcase)
    {
      register char *p;
      for (p = dir_static.d_name; *p; p++)
	if (*p >= 'a' && *p <= 'z')
	  break;
      if (!*p)
	_strlwr (dir_static.d_name);
    }

  return &dir_static;
}

static HANDLE
open_unc_volume (const char *path)
{
  NETRESOURCE nr;
  HANDLE henum;
  int result;

  nr.dwScope = RESOURCE_GLOBALNET;
  nr.dwType = RESOURCETYPE_DISK;
  nr.dwDisplayType = RESOURCEDISPLAYTYPE_SERVER;
  nr.dwUsage = RESOURCEUSAGE_CONTAINER;
  nr.lpLocalName = NULL;
  nr.lpRemoteName = (LPSTR)map_w32_filename (path, NULL);
  nr.lpComment = NULL;
  nr.lpProvider = NULL;

  result = WNetOpenEnum (RESOURCE_GLOBALNET, RESOURCETYPE_DISK,
			 RESOURCEUSAGE_CONNECTABLE, &nr, &henum);

  if (result == NO_ERROR)
    return henum;
  else
    return INVALID_HANDLE_VALUE;
}

static char *
read_unc_volume (HANDLE henum, char *readbuf, int size)
{
  DWORD count;
  int result;
  DWORD bufsize = 512;
  char *buffer;
  char *ptr;

  count = 1;
  buffer = alloca (bufsize);
  result = WNetEnumResource (henum, &count, buffer, &bufsize);
  if (result != NO_ERROR)
    return NULL;

  /* WNetEnumResource returns \\resource\share...skip forward to "share". */
  ptr = ((LPNETRESOURCE) buffer)->lpRemoteName;
  ptr += 2;
  while (*ptr && !IS_DIRECTORY_SEP (*ptr)) ptr++;
  ptr++;

  strncpy (readbuf, ptr, size);
  return readbuf;
}

static void
close_unc_volume (HANDLE henum)
{
  if (henum != INVALID_HANDLE_VALUE)
    WNetCloseEnum (henum);
}

static DWORD
unc_volume_file_attributes (const char *path)
{
  HANDLE henum;
  DWORD attrs;

  henum = open_unc_volume (path);
  if (henum == INVALID_HANDLE_VALUE)
    return -1;

  attrs = FILE_ATTRIBUTE_READONLY | FILE_ATTRIBUTE_DIRECTORY;

  close_unc_volume (henum);

  return attrs;
}

/* Ensure a network connection is authenticated.  */
static void
logon_network_drive (const char *path)
{
  NETRESOURCE resource;
  char share[MAX_PATH];
  int i, n_slashes;
  char drive[4];
  UINT drvtype;

  if (IS_DIRECTORY_SEP (path[0]) && IS_DIRECTORY_SEP (path[1]))
    drvtype = DRIVE_REMOTE;
  else if (path[0] == '\0' || path[1] != ':')
    drvtype = GetDriveType (NULL);
  else
    {
      drive[0] = path[0];
      drive[1] = ':';
      drive[2] = '\\';
      drive[3] = '\0';
      drvtype = GetDriveType (drive);
    }

  /* Only logon to networked drives.  */
  if (drvtype != DRIVE_REMOTE)
    return;

  n_slashes = 2;
  strncpy (share, path, MAX_PATH);
  /* Truncate to just server and share name.  */
  for (i = 2; i < MAX_PATH; i++)
    {
      if (IS_DIRECTORY_SEP (share[i]) && ++n_slashes > 3)
        {
          share[i] = '\0';
          break;
        }
    }

  resource.dwType = RESOURCETYPE_DISK;
  resource.lpLocalName = NULL;
  resource.lpRemoteName = share;
  resource.lpProvider = NULL;

  WNetAddConnection2 (&resource, NULL, NULL, CONNECT_INTERACTIVE);
}

/* Shadow some MSVC runtime functions to map requests for long filenames
   to reasonable short names if necessary.  This was originally added to
   permit running Emacs on NT 3.1 on a FAT partition, which doesn't support
   long file names.  */

int
sys_access (const char * path, int mode)
{
  DWORD attributes;

  /* MSVC implementation doesn't recognize D_OK.  */
  path = map_w32_filename (path, NULL);
  if (is_unc_volume (path))
    {
      attributes = unc_volume_file_attributes (path);
      if (attributes == -1) {
	errno = EACCES;
	return -1;
      }
    }
  else if ((attributes = GetFileAttributes (path)) == -1)
    {
      /* Should try mapping GetLastError to errno; for now just indicate
	 that path doesn't exist.  */
      errno = EACCES;
      return -1;
    }
  if ((mode & X_OK) != 0 && !is_exec (path))
    {
      errno = EACCES;
      return -1;
    }
  if ((mode & W_OK) != 0 && (attributes & FILE_ATTRIBUTE_READONLY) != 0)
    {
      errno = EACCES;
      return -1;
    }
  if ((mode & D_OK) != 0 && (attributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
    {
      errno = EACCES;
      return -1;
    }
  return 0;
}

int
sys_chdir (const char * path)
{
  return _chdir (map_w32_filename (path, NULL));
}

int
sys_chmod (const char * path, int mode)
{
  return _chmod (map_w32_filename (path, NULL), mode);
}

int
sys_chown (const char *path, uid_t owner, gid_t group)
{
  if (sys_chmod (path, S_IREAD) == -1) /* check if file exists */
    return -1;
  return 0;
}

int
sys_creat (const char * path, int mode)
{
  return _creat (map_w32_filename (path, NULL), mode);
}

FILE *
sys_fopen (const char * path, const char * mode)
{
  int fd;
  int oflag;
  const char * mode_save = mode;

  /* Force all file handles to be non-inheritable.  This is necessary to
     ensure child processes don't unwittingly inherit handles that might
     prevent future file access. */

  if (mode[0] == 'r')
    oflag = O_RDONLY;
  else if (mode[0] == 'w' || mode[0] == 'a')
    oflag = O_WRONLY | O_CREAT | O_TRUNC;
  else
    return NULL;

  /* Only do simplistic option parsing. */
  while (*++mode)
    if (mode[0] == '+')
      {
	oflag &= ~(O_RDONLY | O_WRONLY);
	oflag |= O_RDWR;
      }
    else if (mode[0] == 'b')
      {
	oflag &= ~O_TEXT;
	oflag |= O_BINARY;
      }
    else if (mode[0] == 't')
      {
	oflag &= ~O_BINARY;
	oflag |= O_TEXT;
      }
    else break;

  fd = _open (map_w32_filename (path, NULL), oflag | _O_NOINHERIT, 0644);
  if (fd < 0)
    return NULL;

  return _fdopen (fd, mode_save);
}

/* This only works on NTFS volumes, but is useful to have.  */
int
sys_link (const char * old, const char * new)
{
  HANDLE fileh;
  int   result = -1;
  char oldname[MAX_PATH], newname[MAX_PATH];

  if (old == NULL || new == NULL)
    {
      errno = ENOENT;
      return -1;
    }

  strcpy (oldname, map_w32_filename (old, NULL));
  strcpy (newname, map_w32_filename (new, NULL));

  fileh = CreateFile (oldname, 0, 0, NULL, OPEN_EXISTING,
		      FILE_FLAG_BACKUP_SEMANTICS, NULL);
  if (fileh != INVALID_HANDLE_VALUE)
    {
      int wlen;

      /* Confusingly, the "alternate" stream name field does not apply
         when restoring a hard link, and instead contains the actual
         stream data for the link (ie. the name of the link to create).
         The WIN32_STREAM_ID structure before the cStreamName field is
         the stream header, which is then immediately followed by the
         stream data.  */

      struct {
	WIN32_STREAM_ID wid;
	WCHAR wbuffer[MAX_PATH];	/* extra space for link name */
      } data;

      wlen = MultiByteToWideChar (CP_ACP, MB_PRECOMPOSED, newname, -1,
				  data.wid.cStreamName, MAX_PATH);
      if (wlen > 0)
	{
	  LPVOID context = NULL;
	  DWORD wbytes = 0;

	  data.wid.dwStreamId = BACKUP_LINK;
	  data.wid.dwStreamAttributes = 0;
	  data.wid.Size.LowPart = wlen * sizeof (WCHAR);
	  data.wid.Size.HighPart = 0;
	  data.wid.dwStreamNameSize = 0;

	  if (BackupWrite (fileh, (LPBYTE)&data,
			   offsetof (WIN32_STREAM_ID, cStreamName)
			   + data.wid.Size.LowPart,
			   &wbytes, FALSE, FALSE, &context)
	      && BackupWrite (fileh, NULL, 0, &wbytes, TRUE, FALSE, &context))
	    {
	      /* succeeded */
	      result = 0;
	    }
	  else
	    {
	      /* Should try mapping GetLastError to errno; for now just
		 indicate a general error (eg. links not supported).  */
	      errno = EINVAL;  // perhaps EMLINK?
	    }
	}

      CloseHandle (fileh);
    }
  else
    errno = ENOENT;

  return result;
}

int
sys_mkdir (const char * path)
{
  return _mkdir (map_w32_filename (path, NULL));
}

/* Because of long name mapping issues, we need to implement this
   ourselves.  Also, MSVC's _mktemp returns NULL when it can't generate
   a unique name, instead of setting the input template to an empty
   string.

   Standard algorithm seems to be use pid or tid with a letter on the
   front (in place of the 6 X's) and cycle through the letters to find a
   unique name.  We extend that to allow any reasonable character as the
   first of the 6 X's.  */
char *
sys_mktemp (char * template)
{
  char * p;
  int i;
  unsigned uid = GetCurrentThreadId ();
  static char first_char[] = "abcdefghijklmnopqrstuvwyz0123456789!%-_@#";

  if (template == NULL)
    return NULL;
  p = template + strlen (template);
  i = 5;
  /* replace up to the last 5 X's with uid in decimal */
  while (--p >= template && p[0] == 'X' && --i >= 0)
    {
      p[0] = '0' + uid % 10;
      uid /= 10;
    }

  if (i < 0 && p[0] == 'X')
    {
      i = 0;
      do
	{
	  int save_errno = errno;
	  p[0] = first_char[i];
	  if (sys_access (template, 0) < 0)
	    {
	      errno = save_errno;
	      return template;
	    }
	}
      while (++i < sizeof (first_char));
    }

  /* Template is badly formed or else we can't generate a unique name,
     so return empty string */
  template[0] = 0;
  return template;
}

int
sys_open (const char * path, int oflag, int mode)
{
  const char* mpath = map_w32_filename (path, NULL);
  /* Try to open file without _O_CREAT, to be able to write to hidden
     and system files. Force all file handles to be
     non-inheritable. */
  int res = _open (mpath, (oflag & ~_O_CREAT) | _O_NOINHERIT, mode);
  if (res >= 0)
    return res;
  return _open (mpath, oflag | _O_NOINHERIT, mode);
}

int
sys_rename (const char * oldname, const char * newname)
{
  BOOL result;
  char temp[MAX_PATH];
  int newname_dev;
  int oldname_dev;

  /* MoveFile on Windows 95 doesn't correctly change the short file name
     alias in a number of circumstances (it is not easy to predict when
     just by looking at oldname and newname, unfortunately).  In these
     cases, renaming through a temporary name avoids the problem.

     A second problem on Windows 95 is that renaming through a temp name when
     newname is uppercase fails (the final long name ends up in
     lowercase, although the short alias might be uppercase) UNLESS the
     long temp name is not 8.3.

     So, on Windows 95 we always rename through a temp name, and we make sure
     the temp name has a long extension to ensure correct renaming.  */

  strcpy (temp, map_w32_filename (oldname, NULL));

  /* volume_info is set indirectly by map_w32_filename.  */
  oldname_dev = volume_info.serialnum;

  if (os_subtype == OS_WIN95)
    {
      char * o;
      char * p;
      int    i = 0;

      oldname = map_w32_filename (oldname, NULL);
      if ((o = strrchr (oldname, '\\')))
	o++;
      else
	o = (char *) oldname;

      if ((p = strrchr (temp, '\\')))
	p++;
      else
	p = temp;

      do
	{
	  /* Force temp name to require a manufactured 8.3 alias - this
	     seems to make the second rename work properly.  */
	  sprintf (p, "_.%s.%u", o, i);
	  i++;
	  result = rename (oldname, temp);
	}
      /* This loop must surely terminate!  */
      while (result < 0 && errno == EEXIST);
      if (result < 0)
	return -1;
    }

  /* Emulate Unix behavior - newname is deleted if it already exists
     (at least if it is a file; don't do this for directories).

     Since we mustn't do this if we are just changing the case of the
     file name (we would end up deleting the file we are trying to
     rename!), we let rename detect if the destination file already
     exists - that way we avoid the possible pitfalls of trying to
     determine ourselves whether two names really refer to the same
     file, which is not always possible in the general case.  (Consider
     all the permutations of shared or subst'd drives, etc.)  */

  newname = map_w32_filename (newname, NULL);

  /* volume_info is set indirectly by map_w32_filename.  */
  newname_dev = volume_info.serialnum;

  result = rename (temp, newname);

  if (result < 0)
    {

      if (errno == EACCES
	  && newname_dev != oldname_dev)
	{
	  /* The implementation of `rename' on Windows does not return
	     errno = EXDEV when you are moving a directory to a
	     different storage device (ex. logical disk).  It returns
	     EACCES instead.  So here we handle such situations and
	     return EXDEV.  */
	  DWORD attributes;

	  if ((attributes = GetFileAttributes (temp)) != -1
	      && attributes & FILE_ATTRIBUTE_DIRECTORY)
	    errno = EXDEV;
	}
      else if (errno == EEXIST)
	{
	  if (_chmod (newname, 0666) != 0)
	    return result;
	  if (_unlink (newname) != 0)
	    return result;
	  result = rename (temp, newname);
	}
    }

  return result;
}

int
sys_rmdir (const char * path)
{
  return _rmdir (map_w32_filename (path, NULL));
}

int
sys_unlink (const char * path)
{
  path = map_w32_filename (path, NULL);

  /* On Unix, unlink works without write permission. */
  _chmod (path, 0666);
  return _unlink (path);
}

static FILETIME utc_base_ft;
static ULONGLONG utc_base;  /* In 100ns units */
static int init = 0;

#define FILETIME_TO_U64(result, ft)        \
  do {                                     \
    ULARGE_INTEGER uiTemp;                 \
    uiTemp.LowPart = (ft).dwLowDateTime;   \
    uiTemp.HighPart = (ft).dwHighDateTime; \
    result = uiTemp.QuadPart;              \
  } while (0)

static void
initialize_utc_base (void)
{
  /* Determine the delta between 1-Jan-1601 and 1-Jan-1970. */
  SYSTEMTIME st;

  st.wYear = 1970;
  st.wMonth = 1;
  st.wDay = 1;
  st.wHour = 0;
  st.wMinute = 0;
  st.wSecond = 0;
  st.wMilliseconds = 0;

  SystemTimeToFileTime (&st, &utc_base_ft);
  FILETIME_TO_U64 (utc_base, utc_base_ft);
}

static time_t
convert_time (FILETIME ft)
{
  ULONGLONG tmp;

  if (!init)
    {
      initialize_utc_base ();
      init = 1;
    }

  if (CompareFileTime (&ft, &utc_base_ft) < 0)
    return 0;

  FILETIME_TO_U64 (tmp, ft);
  return (time_t) ((tmp - utc_base) / 10000000L);
}

static void
convert_from_time_t (time_t time, FILETIME * pft)
{
  ULARGE_INTEGER tmp;

  if (!init)
    {
      initialize_utc_base ();
      init = 1;
    }

  /* time in 100ns units since 1-Jan-1601 */
  tmp.QuadPart = (ULONGLONG) time * 10000000L + utc_base;
  pft->dwHighDateTime = tmp.HighPart;
  pft->dwLowDateTime = tmp.LowPart;
}

#if 0
/* No reason to keep this; faking inode values either by hashing or even
   using the file index from GetInformationByHandle, is not perfect and
   so by default Emacs doesn't use the inode values on Windows.
   Instead, we now determine file-truename correctly (except for
   possible drive aliasing etc).  */

/*  Modified version of "PJW" algorithm (see the "Dragon" compiler book). */
static unsigned
hashval (const unsigned char * str)
{
  unsigned h = 0;
  while (*str)
    {
      h = (h << 4) + *str++;
      h ^= (h >> 28);
    }
  return h;
}

/* Return the hash value of the canonical pathname, excluding the
   drive/UNC header, to get a hopefully unique inode number. */
static DWORD
generate_inode_val (const char * name)
{
  char fullname[ MAX_PATH ];
  char * p;
  unsigned hash;

  /* Get the truly canonical filename, if it exists.  (Note: this
     doesn't resolve aliasing due to subst commands, or recognize hard
     links.  */
  if (!w32_get_long_filename ((char *)name, fullname, MAX_PATH))
    abort ();

  parse_root (fullname, &p);
  /* Normal W32 filesystems are still case insensitive. */
  _strlwr (p);
  return hashval (p);
}

#endif

static PSECURITY_DESCRIPTOR
get_file_security_desc (const char *fname)
{
  PSECURITY_DESCRIPTOR psd = NULL;
  DWORD sd_len, err;
  SECURITY_INFORMATION si = OWNER_SECURITY_INFORMATION
    | GROUP_SECURITY_INFORMATION  /* | DACL_SECURITY_INFORMATION */ ;

  if (!get_file_security (fname, si, psd, 0, &sd_len))
    {
      err = GetLastError ();
      if (err != ERROR_INSUFFICIENT_BUFFER)
	return NULL;
    }

  psd = xmalloc (sd_len);
  if (!get_file_security (fname, si, psd, sd_len, &sd_len))
    {
      xfree (psd);
      return NULL;
    }

  return psd;
}

static DWORD
get_rid (PSID sid)
{
  unsigned n_subauthorities;

  /* Use the last sub-authority value of the RID, the relative
     portion of the SID, as user/group ID. */
  n_subauthorities = *get_sid_sub_authority_count (sid);
  if (n_subauthorities < 1)
    return 0;	/* the "World" RID */
  return *get_sid_sub_authority (sid, n_subauthorities - 1);
}

/* Caching SID and account values for faster lokup.  */

#ifdef __GNUC__
# define FLEXIBLE_ARRAY_MEMBER
#else
# define FLEXIBLE_ARRAY_MEMBER 1
#endif

struct w32_id {
  unsigned rid;
  struct w32_id *next;
  char name[GNLEN+1];
  unsigned char sid[FLEXIBLE_ARRAY_MEMBER];
};

static struct w32_id *w32_idlist;

static int
w32_cached_id (PSID sid, unsigned *id, char *name)
{
  struct w32_id *tail, *found;

  for (found = NULL, tail = w32_idlist; tail; tail = tail->next)
    {
      if (equal_sid ((PSID)tail->sid, sid))
	{
	  found = tail;
	  break;
	}
    }
  if (found)
    {
      *id = found->rid;
      strcpy (name, found->name);
      return 1;
    }
  else
    return 0;
}

static void
w32_add_to_cache (PSID sid, unsigned id, char *name)
{
  DWORD sid_len;
  struct w32_id *new_entry;

  /* We don't want to leave behind stale cache from when Emacs was
     dumped.  */
  if (initialized)
    {
      sid_len = get_length_sid (sid);
      new_entry = xmalloc (offsetof (struct w32_id, sid) + sid_len);
      if (new_entry)
	{
	  new_entry->rid = id;
	  strcpy (new_entry->name, name);
	  copy_sid (sid_len, (PSID)new_entry->sid, sid);
	  new_entry->next = w32_idlist;
	  w32_idlist = new_entry;
	}
    }
}

#define UID 1
#define GID 2

static int
get_name_and_id (PSECURITY_DESCRIPTOR psd, const char *fname,
		 unsigned *id, char *nm, int what)
{
  PSID sid = NULL;
  char machine[MAX_COMPUTERNAME_LENGTH+1];
  BOOL dflt;
  SID_NAME_USE ignore;
  char name[UNLEN+1];
  DWORD name_len = sizeof (name);
  char domain[1024];
  DWORD domain_len = sizeof (domain);
  char *mp = NULL;
  int use_dflt = 0;
  int result;

  if (what == UID)
    result = get_security_descriptor_owner (psd, &sid, &dflt);
  else if (what == GID)
    result = get_security_descriptor_group (psd, &sid, &dflt);
  else
    result = 0;

  if (!result || !is_valid_sid (sid))
    use_dflt = 1;
  else if (!w32_cached_id (sid, id, nm))
    {
      /* If FNAME is a UNC, we need to lookup account on the
	 specified machine.  */
      if (IS_DIRECTORY_SEP (fname[0]) && IS_DIRECTORY_SEP (fname[1])
	  && fname[2] != '\0')
	{
	  const char *s;
	  char *p;

	  for (s = fname + 2, p = machine;
	       *s && !IS_DIRECTORY_SEP (*s); s++, p++)
	    *p = *s;
	  *p = '\0';
	  mp = machine;
	}

      if (!lookup_account_sid (mp, sid, name, &name_len,
			       domain, &domain_len, &ignore)
	  || name_len > UNLEN+1)
	use_dflt = 1;
      else
	{
	  *id = get_rid (sid);
	  strcpy (nm, name);
	  w32_add_to_cache (sid, *id, name);
	}
    }
  return use_dflt;
}

static void
get_file_owner_and_group (PSECURITY_DESCRIPTOR psd,
			  const char *fname,
			  struct stat *st)
{
  int dflt_usr = 0, dflt_grp = 0;

  if (!psd)
    {
      dflt_usr = 1;
      dflt_grp = 1;
    }
  else
    {
      if (get_name_and_id (psd, fname, &st->st_uid, st->st_uname, UID))
	dflt_usr = 1;
      if (get_name_and_id (psd, fname, &st->st_gid, st->st_gname, GID))
	dflt_grp = 1;
    }
  /* Consider files to belong to current user/group, if we cannot get
     more accurate information.  */
  if (dflt_usr)
    {
      st->st_uid = dflt_passwd.pw_uid;
      strcpy (st->st_uname, dflt_passwd.pw_name);
    }
  if (dflt_grp)
    {
      st->st_gid = dflt_passwd.pw_gid;
      strcpy (st->st_gname, dflt_group.gr_name);
    }
}

/* Return non-zero if NAME is a potentially slow filesystem.  */
int
is_slow_fs (const char *name)
{
  char drive_root[4];
  UINT devtype;

  if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
    devtype = DRIVE_REMOTE;	   /* assume UNC name is remote */
  else if (!(strlen (name) >= 2 && IS_DEVICE_SEP (name[1])))
    devtype = GetDriveType (NULL); /* use root of current drive */
  else
    {
      /* GetDriveType needs the root directory of the drive.  */
      strncpy (drive_root, name, 2);
      drive_root[2] = '\\';
      drive_root[3] = '\0';
      devtype = GetDriveType (drive_root);
    }
  return !(devtype == DRIVE_FIXED || devtype == DRIVE_RAMDISK);
}

/* MSVC stat function can't cope with UNC names and has other bugs, so
   replace it with our own.  This also allows us to calculate consistent
   inode values without hacks in the main Emacs code. */
int
stat (const char * path, struct stat * buf)
{
  char *name, *r;
  WIN32_FIND_DATA wfd;
  HANDLE fh;
  unsigned __int64 fake_inode;
  int permission;
  int len;
  int rootdir = FALSE;
  PSECURITY_DESCRIPTOR psd = NULL;

  if (path == NULL || buf == NULL)
    {
      errno = EFAULT;
      return -1;
    }

  name = (char *) map_w32_filename (path, &path);
  /* Must be valid filename, no wild cards or other invalid
     characters.  We use _mbspbrk to support multibyte strings that
     might look to strpbrk as if they included literal *, ?, and other
     characters mentioned below that are disallowed by Windows
     filesystems.  */
  if (_mbspbrk (name, "*?|<>\""))
    {
      errno = ENOENT;
      return -1;
    }

  /* If name is "c:/.." or "/.." then stat "c:/" or "/".  */
  r = IS_DEVICE_SEP (name[1]) ? &name[2] : name;
  if (IS_DIRECTORY_SEP (r[0]) && r[1] == '.' && r[2] == '.' && r[3] == '\0')
    {
      r[1] = r[2] = '\0';
    }

  /* Remove trailing directory separator, unless name is the root
     directory of a drive or UNC volume in which case ensure there
     is a trailing separator. */
  len = strlen (name);
  rootdir = (path >= name + len - 1
	     && (IS_DIRECTORY_SEP (*path) || *path == 0));
  name = strcpy (alloca (len + 2), name);

  if (is_unc_volume (name))
    {
      DWORD attrs = unc_volume_file_attributes (name);

      if (attrs == -1)
	return -1;

      memset (&wfd, 0, sizeof (wfd));
      wfd.dwFileAttributes = attrs;
      wfd.ftCreationTime = utc_base_ft;
      wfd.ftLastAccessTime = utc_base_ft;
      wfd.ftLastWriteTime = utc_base_ft;
      strcpy (wfd.cFileName, name);
    }
  else if (rootdir)
    {
      if (!IS_DIRECTORY_SEP (name[len-1]))
	strcat (name, "\\");
      if (GetDriveType (name) < 2)
	{
	  errno = ENOENT;
	  return -1;
	}
      memset (&wfd, 0, sizeof (wfd));
      wfd.dwFileAttributes = FILE_ATTRIBUTE_DIRECTORY;
      wfd.ftCreationTime = utc_base_ft;
      wfd.ftLastAccessTime = utc_base_ft;
      wfd.ftLastWriteTime = utc_base_ft;
      strcpy (wfd.cFileName, name);
    }
  else
    {
      if (IS_DIRECTORY_SEP (name[len-1]))
	name[len - 1] = 0;

      /* (This is hacky, but helps when doing file completions on
	 network drives.)  Optimize by using information available from
	 active readdir if possible.  */
      len = strlen (dir_pathname);
      if (IS_DIRECTORY_SEP (dir_pathname[len-1]))
	len--;
      if (dir_find_handle != INVALID_HANDLE_VALUE
	  && strnicmp (name, dir_pathname, len) == 0
	  && IS_DIRECTORY_SEP (name[len])
	  && xstrcasecmp (name + len + 1, dir_static.d_name) == 0)
	{
	  /* This was the last entry returned by readdir.  */
	  wfd = dir_find_data;
	}
      else
	{
          logon_network_drive (name);

	  fh = FindFirstFile (name, &wfd);
	  if (fh == INVALID_HANDLE_VALUE)
	    {
	      errno = ENOENT;
	      return -1;
	    }
	  FindClose (fh);
	}
    }

  if (!(NILP (Vw32_get_true_file_attributes)
	|| (EQ (Vw32_get_true_file_attributes, Qlocal) && is_slow_fs (name)))
      /* No access rights required to get info.  */
      && (fh = CreateFile (name, 0, 0, NULL, OPEN_EXISTING,
			   FILE_FLAG_BACKUP_SEMANTICS, NULL))
         != INVALID_HANDLE_VALUE)
    {
      /* This is more accurate in terms of getting the correct number
	 of links, but is quite slow (it is noticeable when Emacs is
	 making a list of file name completions). */
      BY_HANDLE_FILE_INFORMATION info;

      if (GetFileInformationByHandle (fh, &info))
	{
	  buf->st_nlink = info.nNumberOfLinks;
	  /* Might as well use file index to fake inode values, but this
	     is not guaranteed to be unique unless we keep a handle open
	     all the time (even then there are situations where it is
	     not unique).  Reputedly, there are at most 48 bits of info
	     (on NTFS, presumably less on FAT). */
	  fake_inode = info.nFileIndexHigh;
	  fake_inode <<= 32;
	  fake_inode += info.nFileIndexLow;
	}
      else
	{
	  buf->st_nlink = 1;
	  fake_inode = 0;
	}

      if (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
	{
	  buf->st_mode = S_IFDIR;
	}
      else
	{
	  switch (GetFileType (fh))
	    {
	    case FILE_TYPE_DISK:
	      buf->st_mode = S_IFREG;
	      break;
	    case FILE_TYPE_PIPE:
	      buf->st_mode = S_IFIFO;
	      break;
	    case FILE_TYPE_CHAR:
	    case FILE_TYPE_UNKNOWN:
	    default:
	      buf->st_mode = S_IFCHR;
	    }
	}
      CloseHandle (fh);
      psd = get_file_security_desc (name);
      get_file_owner_and_group (psd, name, buf);
    }
  else
    {
      /* Don't bother to make this information more accurate.  */
      buf->st_mode = (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ?
	S_IFDIR : S_IFREG;
      buf->st_nlink = 1;
      fake_inode = 0;

      get_file_owner_and_group (NULL, name, buf);
    }
  xfree (psd);

#if 0
  /* Not sure if there is any point in this.  */
  if (!NILP (Vw32_generate_fake_inodes))
    fake_inode = generate_inode_val (name);
  else if (fake_inode == 0)
    {
      /* For want of something better, try to make everything unique.  */
      static DWORD gen_num = 0;
      fake_inode = ++gen_num;
    }
#endif

  /* MSVC defines _ino_t to be short; other libc's might not.  */
  if (sizeof (buf->st_ino) == 2)
    buf->st_ino = fake_inode ^ (fake_inode >> 16);
  else
    buf->st_ino = fake_inode;

  /* volume_info is set indirectly by map_w32_filename */
  buf->st_dev = volume_info.serialnum;
  buf->st_rdev = volume_info.serialnum;

  buf->st_size = wfd.nFileSizeHigh;
  buf->st_size <<= 32;
  buf->st_size += wfd.nFileSizeLow;

  /* Convert timestamps to Unix format. */
  buf->st_mtime = convert_time (wfd.ftLastWriteTime);
  buf->st_atime = convert_time (wfd.ftLastAccessTime);
  if (buf->st_atime == 0) buf->st_atime = buf->st_mtime;
  buf->st_ctime = convert_time (wfd.ftCreationTime);
  if (buf->st_ctime == 0) buf->st_ctime = buf->st_mtime;

  /* determine rwx permissions */
  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    permission = S_IREAD;
  else
    permission = S_IREAD | S_IWRITE;

  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    permission |= S_IEXEC;
  else if (is_exec (name))
    permission |= S_IEXEC;

  buf->st_mode |= permission | (permission >> 3) | (permission >> 6);

  return 0;
}

/* Provide fstat and utime as well as stat for consistent handling of
   file timestamps. */
int
fstat (int desc, struct stat * buf)
{
  HANDLE fh = (HANDLE) _get_osfhandle (desc);
  BY_HANDLE_FILE_INFORMATION info;
  unsigned __int64 fake_inode;
  int permission;

  switch (GetFileType (fh) & ~FILE_TYPE_REMOTE)
    {
    case FILE_TYPE_DISK:
      buf->st_mode = S_IFREG;
      if (!GetFileInformationByHandle (fh, &info))
	{
	  errno = EACCES;
	  return -1;
	}
      break;
    case FILE_TYPE_PIPE:
      buf->st_mode = S_IFIFO;
      goto non_disk;
    case FILE_TYPE_CHAR:
    case FILE_TYPE_UNKNOWN:
    default:
      buf->st_mode = S_IFCHR;
    non_disk:
      memset (&info, 0, sizeof (info));
      info.dwFileAttributes = 0;
      info.ftCreationTime = utc_base_ft;
      info.ftLastAccessTime = utc_base_ft;
      info.ftLastWriteTime = utc_base_ft;
    }

  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
      buf->st_mode = S_IFDIR;

  buf->st_nlink = info.nNumberOfLinks;
  /* Might as well use file index to fake inode values, but this
     is not guaranteed to be unique unless we keep a handle open
     all the time (even then there are situations where it is
     not unique).  Reputedly, there are at most 48 bits of info
     (on NTFS, presumably less on FAT). */
  fake_inode = info.nFileIndexHigh;
  fake_inode <<= 32;
  fake_inode += info.nFileIndexLow;

  /* MSVC defines _ino_t to be short; other libc's might not.  */
  if (sizeof (buf->st_ino) == 2)
    buf->st_ino = fake_inode ^ (fake_inode >> 16);
  else
    buf->st_ino = fake_inode;

  /* Consider files to belong to current user.
     FIXME: this should use GetSecurityInfo API, but it is only
     available for _WIN32_WINNT >= 0x501.  */
  buf->st_uid = dflt_passwd.pw_uid;
  buf->st_gid = dflt_passwd.pw_gid;
  strcpy (buf->st_uname, dflt_passwd.pw_name);
  strcpy (buf->st_gname, dflt_group.gr_name);

  buf->st_dev = info.dwVolumeSerialNumber;
  buf->st_rdev = info.dwVolumeSerialNumber;

  buf->st_size = info.nFileSizeHigh;
  buf->st_size <<= 32;
  buf->st_size += info.nFileSizeLow;

  /* Convert timestamps to Unix format. */
  buf->st_mtime = convert_time (info.ftLastWriteTime);
  buf->st_atime = convert_time (info.ftLastAccessTime);
  if (buf->st_atime == 0) buf->st_atime = buf->st_mtime;
  buf->st_ctime = convert_time (info.ftCreationTime);
  if (buf->st_ctime == 0) buf->st_ctime = buf->st_mtime;

  /* determine rwx permissions */
  if (info.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    permission = S_IREAD;
  else
    permission = S_IREAD | S_IWRITE;

  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    permission |= S_IEXEC;
  else
    {
#if 0 /* no way of knowing the filename */
      char * p = strrchr (name, '.');
      if (p != NULL &&
	  (xstrcasecmp (p, ".exe") == 0 ||
	   xstrcasecmp (p, ".com") == 0 ||
	   xstrcasecmp (p, ".bat") == 0 ||
	   xstrcasecmp (p, ".cmd") == 0))
	permission |= S_IEXEC;
#endif
    }

  buf->st_mode |= permission | (permission >> 3) | (permission >> 6);

  return 0;
}

int
utime (const char *name, struct utimbuf *times)
{
  struct utimbuf deftime;
  HANDLE fh;
  FILETIME mtime;
  FILETIME atime;

  if (times == NULL)
    {
      deftime.modtime = deftime.actime = time (NULL);
      times = &deftime;
    }

  /* Need write access to set times.  */
  fh = CreateFile (name, GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
		   0, OPEN_EXISTING, 0, NULL);
  if (fh)
    {
      convert_from_time_t (times->actime, &atime);
      convert_from_time_t (times->modtime, &mtime);
      if (!SetFileTime (fh, NULL, &atime, &mtime))
	{
	  CloseHandle (fh);
	  errno = EACCES;
	  return -1;
	}
      CloseHandle (fh);
    }
  else
    {
      errno = EINVAL;
      return -1;
    }
  return 0;
}


/* Symlink-related functions that always fail.  Used in fileio.c and in
   sysdep.c to avoid #ifdef's.  */
int
symlink (char const *dummy1, char const *dummy2)
{
  errno = ENOSYS;
  return -1;
}

ssize_t
readlink (const char *name, char *dummy1, size_t dummy2)
{
  /* `access' is much faster than `stat' on MS-Windows.  */
  if (sys_access (name, 0) == 0)
    errno = EINVAL;
  return -1;
}

char *
careadlinkat (int fd, char const *filename,
              char *buffer, size_t buffer_size,
              struct allocator const *alloc,
              ssize_t (*preadlinkat) (int, char const *, char *, size_t))
{
  errno = ENOSYS;
  return NULL;
}

ssize_t
careadlinkatcwd (int fd, char const *filename, char *buffer,
                 size_t buffer_size)
{
  (void) fd;
  return readlink (filename, buffer, buffer_size);
}


/* Support for browsing other processes and their attributes.  See
   process.c for the Lisp bindings.  */

/* Helper wrapper functions.  */

static HANDLE WINAPI
create_toolhelp32_snapshot (DWORD Flags, DWORD Ignored)
{
  static CreateToolhelp32Snapshot_Proc s_pfn_Create_Toolhelp32_Snapshot = NULL;

  if (g_b_init_create_toolhelp32_snapshot == 0)
    {
      g_b_init_create_toolhelp32_snapshot = 1;
      s_pfn_Create_Toolhelp32_Snapshot = (CreateToolhelp32Snapshot_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"CreateToolhelp32Snapshot");
    }
  if (s_pfn_Create_Toolhelp32_Snapshot == NULL)
    {
      return INVALID_HANDLE_VALUE;
    }
  return (s_pfn_Create_Toolhelp32_Snapshot (Flags, Ignored));
}

static BOOL WINAPI
process32_first (HANDLE hSnapshot, LPPROCESSENTRY32 lppe)
{
  static Process32First_Proc s_pfn_Process32_First = NULL;

  if (g_b_init_process32_first == 0)
    {
      g_b_init_process32_first = 1;
      s_pfn_Process32_First = (Process32First_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"Process32First");
    }
  if (s_pfn_Process32_First == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Process32_First (hSnapshot, lppe));
}

static BOOL WINAPI
process32_next (HANDLE hSnapshot, LPPROCESSENTRY32 lppe)
{
  static Process32Next_Proc s_pfn_Process32_Next = NULL;

  if (g_b_init_process32_next == 0)
    {
      g_b_init_process32_next = 1;
      s_pfn_Process32_Next = (Process32Next_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"Process32Next");
    }
  if (s_pfn_Process32_Next == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Process32_Next (hSnapshot, lppe));
}

static BOOL WINAPI
open_thread_token (HANDLE ThreadHandle,
		   DWORD DesiredAccess,
		   BOOL OpenAsSelf,
		   PHANDLE TokenHandle)
{
  static OpenThreadToken_Proc s_pfn_Open_Thread_Token = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      SetLastError (ERROR_NOT_SUPPORTED);
      return FALSE;
    }
  if (g_b_init_open_thread_token == 0)
    {
      g_b_init_open_thread_token = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Open_Thread_Token =
        (OpenThreadToken_Proc) GetProcAddress (hm_advapi32, "OpenThreadToken");
    }
  if (s_pfn_Open_Thread_Token == NULL)
    {
      SetLastError (ERROR_NOT_SUPPORTED);
      return FALSE;
    }
  return (
      s_pfn_Open_Thread_Token (
          ThreadHandle,
          DesiredAccess,
	  OpenAsSelf,
          TokenHandle)
      );
}

static BOOL WINAPI
impersonate_self (SECURITY_IMPERSONATION_LEVEL ImpersonationLevel)
{
  static ImpersonateSelf_Proc s_pfn_Impersonate_Self = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_impersonate_self == 0)
    {
      g_b_init_impersonate_self = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Impersonate_Self =
        (ImpersonateSelf_Proc) GetProcAddress (hm_advapi32, "ImpersonateSelf");
    }
  if (s_pfn_Impersonate_Self == NULL)
    {
      return FALSE;
    }
  return s_pfn_Impersonate_Self (ImpersonationLevel);
}

static BOOL WINAPI
revert_to_self (void)
{
  static RevertToSelf_Proc s_pfn_Revert_To_Self = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_revert_to_self == 0)
    {
      g_b_init_revert_to_self = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Revert_To_Self =
        (RevertToSelf_Proc) GetProcAddress (hm_advapi32, "RevertToSelf");
    }
  if (s_pfn_Revert_To_Self == NULL)
    {
      return FALSE;
    }
  return s_pfn_Revert_To_Self ();
}

static BOOL WINAPI
get_process_memory_info (HANDLE h_proc,
			 PPROCESS_MEMORY_COUNTERS mem_counters,
			 DWORD bufsize)
{
  static GetProcessMemoryInfo_Proc s_pfn_Get_Process_Memory_Info = NULL;
  HMODULE hm_psapi = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_process_memory_info == 0)
    {
      g_b_init_get_process_memory_info = 1;
      hm_psapi = LoadLibrary ("Psapi.dll");
      if (hm_psapi)
	s_pfn_Get_Process_Memory_Info = (GetProcessMemoryInfo_Proc)
	  GetProcAddress (hm_psapi, "GetProcessMemoryInfo");
    }
  if (s_pfn_Get_Process_Memory_Info == NULL)
    {
      return FALSE;
    }
  return s_pfn_Get_Process_Memory_Info (h_proc, mem_counters, bufsize);
}

static BOOL WINAPI
get_process_working_set_size (HANDLE h_proc,
			      DWORD *minrss,
			      DWORD *maxrss)
{
  static GetProcessWorkingSetSize_Proc
    s_pfn_Get_Process_Working_Set_Size = NULL;

  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_process_working_set_size == 0)
    {
      g_b_init_get_process_working_set_size = 1;
      s_pfn_Get_Process_Working_Set_Size = (GetProcessWorkingSetSize_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"GetProcessWorkingSetSize");
    }
  if (s_pfn_Get_Process_Working_Set_Size == NULL)
    {
      return FALSE;
    }
  return s_pfn_Get_Process_Working_Set_Size (h_proc, minrss, maxrss);
}

static BOOL WINAPI
global_memory_status (MEMORYSTATUS *buf)
{
  static GlobalMemoryStatus_Proc s_pfn_Global_Memory_Status = NULL;

  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_global_memory_status == 0)
    {
      g_b_init_global_memory_status = 1;
      s_pfn_Global_Memory_Status = (GlobalMemoryStatus_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"GlobalMemoryStatus");
    }
  if (s_pfn_Global_Memory_Status == NULL)
    {
      return FALSE;
    }
  return s_pfn_Global_Memory_Status (buf);
}

static BOOL WINAPI
global_memory_status_ex (MEMORY_STATUS_EX *buf)
{
  static GlobalMemoryStatusEx_Proc s_pfn_Global_Memory_Status_Ex = NULL;

  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_global_memory_status_ex == 0)
    {
      g_b_init_global_memory_status_ex = 1;
      s_pfn_Global_Memory_Status_Ex = (GlobalMemoryStatusEx_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"GlobalMemoryStatusEx");
    }
  if (s_pfn_Global_Memory_Status_Ex == NULL)
    {
      return FALSE;
    }
  return s_pfn_Global_Memory_Status_Ex (buf);
}

Lisp_Object
list_system_processes (void)
{
  struct gcpro gcpro1;
  Lisp_Object proclist = Qnil;
  HANDLE h_snapshot;

  h_snapshot = create_toolhelp32_snapshot (TH32CS_SNAPPROCESS, 0);

  if (h_snapshot != INVALID_HANDLE_VALUE)
    {
      PROCESSENTRY32 proc_entry;
      DWORD proc_id;
      BOOL res;

      GCPRO1 (proclist);

      proc_entry.dwSize = sizeof (PROCESSENTRY32);
      for (res = process32_first (h_snapshot, &proc_entry); res;
	   res = process32_next  (h_snapshot, &proc_entry))
	{
	  proc_id = proc_entry.th32ProcessID;
	  proclist = Fcons (make_fixnum_or_float (proc_id), proclist);
	}

      CloseHandle (h_snapshot);
      UNGCPRO;
      proclist = Fnreverse (proclist);
    }

  return proclist;
}

static int
enable_privilege (LPCTSTR priv_name, BOOL enable_p, TOKEN_PRIVILEGES *old_priv)
{
  TOKEN_PRIVILEGES priv;
  DWORD priv_size = sizeof (priv);
  DWORD opriv_size = sizeof (*old_priv);
  HANDLE h_token = NULL;
  HANDLE h_thread = GetCurrentThread ();
  int ret_val = 0;
  BOOL res;

  res = open_thread_token (h_thread,
			   TOKEN_QUERY | TOKEN_ADJUST_PRIVILEGES,
			   FALSE, &h_token);
  if (!res && GetLastError () == ERROR_NO_TOKEN)
    {
      if (impersonate_self (SecurityImpersonation))
	  res = open_thread_token (h_thread,
				   TOKEN_QUERY | TOKEN_ADJUST_PRIVILEGES,
				   FALSE, &h_token);
    }
  if (res)
    {
      priv.PrivilegeCount = 1;
      priv.Privileges[0].Attributes = enable_p ? SE_PRIVILEGE_ENABLED : 0;
      LookupPrivilegeValue (NULL, priv_name, &priv.Privileges[0].Luid);
      if (AdjustTokenPrivileges (h_token, FALSE, &priv, priv_size,
				 old_priv, &opriv_size)
	  && GetLastError () != ERROR_NOT_ALL_ASSIGNED)
	ret_val = 1;
    }
  if (h_token)
    CloseHandle (h_token);

  return ret_val;
}

static int
restore_privilege (TOKEN_PRIVILEGES *priv)
{
  DWORD priv_size = sizeof (*priv);
  HANDLE h_token = NULL;
  int ret_val = 0;

  if (open_thread_token (GetCurrentThread (),
			 TOKEN_QUERY | TOKEN_ADJUST_PRIVILEGES,
			 FALSE, &h_token))
    {
      if (AdjustTokenPrivileges (h_token, FALSE, priv, priv_size, NULL, NULL)
	  && GetLastError () != ERROR_NOT_ALL_ASSIGNED)
	ret_val = 1;
    }
  if (h_token)
    CloseHandle (h_token);

  return ret_val;
}

static Lisp_Object
ltime (long time_sec, long time_usec)
{
  return list3 (make_number ((time_sec >> 16) & 0xffff),
		make_number (time_sec & 0xffff),
		make_number (time_usec));
}

#define U64_TO_LISP_TIME(time) ltime ((time) / 1000000L, (time) % 1000000L)

static int
process_times (HANDLE h_proc, Lisp_Object *ctime, Lisp_Object *etime,
	       Lisp_Object *stime, Lisp_Object *utime, Lisp_Object *ttime,
	       double *pcpu)
{
  FILETIME ft_creation, ft_exit, ft_kernel, ft_user, ft_current;
  ULONGLONG tem1, tem2, tem3, tem;

  if (!h_proc
      || !get_process_times_fn
      || !(*get_process_times_fn) (h_proc, &ft_creation, &ft_exit,
				   &ft_kernel, &ft_user))
    return 0;

  GetSystemTimeAsFileTime (&ft_current);

  FILETIME_TO_U64 (tem1, ft_kernel);
  tem1 /= 10L;
  *stime = U64_TO_LISP_TIME (tem1);

  FILETIME_TO_U64 (tem2, ft_user);
  tem2 /= 10L;
  *utime = U64_TO_LISP_TIME (tem2);

  tem3 = tem1 + tem2;
  *ttime = U64_TO_LISP_TIME (tem3);

  FILETIME_TO_U64 (tem, ft_creation);
  /* Process no 4 (System) returns zero creation time.  */
  if (tem)
    tem = (tem - utc_base) / 10L;
  *ctime = U64_TO_LISP_TIME (tem);

  if (tem)
    {
      FILETIME_TO_U64 (tem3, ft_current);
      tem = (tem3 - utc_base) / 10L - tem;
    }
  *etime = U64_TO_LISP_TIME (tem);

  if (tem)
    {
      *pcpu = 100.0 * (tem1 + tem2) / tem;
      if (*pcpu > 100)
	*pcpu = 100.0;
    }
  else
    *pcpu = 0;

  return 1;
}

Lisp_Object
system_process_attributes (Lisp_Object pid)
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object attrs = Qnil;
  Lisp_Object cmd_str, decoded_cmd, tem;
  HANDLE h_snapshot, h_proc;
  DWORD proc_id;
  int found_proc = 0;
  char uname[UNLEN+1], gname[GNLEN+1], domain[1025];
  DWORD ulength = sizeof (uname), dlength = sizeof (domain), needed;
  DWORD glength = sizeof (gname);
  HANDLE token = NULL;
  SID_NAME_USE user_type;
  unsigned char *buf = NULL;
  DWORD blen = 0;
  TOKEN_USER user_token;
  TOKEN_PRIMARY_GROUP group_token;
  unsigned euid;
  unsigned egid;
  PROCESS_MEMORY_COUNTERS mem;
  PROCESS_MEMORY_COUNTERS_EX mem_ex;
  DWORD minrss, maxrss;
  MEMORYSTATUS memst;
  MEMORY_STATUS_EX memstex;
  double totphys = 0.0;
  Lisp_Object ctime, stime, utime, etime, ttime;
  double pcpu;
  BOOL result = FALSE;

  CHECK_NUMBER_OR_FLOAT (pid);
  proc_id = FLOATP (pid) ? XFLOAT_DATA (pid) : XINT (pid);

  h_snapshot = create_toolhelp32_snapshot (TH32CS_SNAPPROCESS, 0);

  GCPRO3 (attrs, decoded_cmd, tem);

  if (h_snapshot != INVALID_HANDLE_VALUE)
    {
      PROCESSENTRY32 pe;
      BOOL res;

      pe.dwSize = sizeof (PROCESSENTRY32);
      for (res = process32_first (h_snapshot, &pe); res;
	   res = process32_next  (h_snapshot, &pe))
	{
	  if (proc_id == pe.th32ProcessID)
	    {
	      if (proc_id == 0)
		decoded_cmd = build_string ("Idle");
	      else
		{
		  /* Decode the command name from locale-specific
		     encoding.  */
		  cmd_str = make_unibyte_string (pe.szExeFile,
						 strlen (pe.szExeFile));
		  decoded_cmd =
		    code_convert_string_norecord (cmd_str,
						  Vlocale_coding_system, 0);
		}
	      attrs = Fcons (Fcons (Qcomm, decoded_cmd), attrs);
	      attrs = Fcons (Fcons (Qppid,
				    make_fixnum_or_float (pe.th32ParentProcessID)),
			     attrs);
	      attrs = Fcons (Fcons (Qpri, make_number (pe.pcPriClassBase)),
			     attrs);
	      attrs = Fcons (Fcons (Qthcount,
				    make_fixnum_or_float (pe.cntThreads)),
			     attrs);
	      found_proc = 1;
	      break;
	    }
	}

      CloseHandle (h_snapshot);
    }

  if (!found_proc)
    {
      UNGCPRO;
      return Qnil;
    }

  h_proc = OpenProcess (PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
			FALSE, proc_id);
  /* If we were denied a handle to the process, try again after
     enabling the SeDebugPrivilege in our process.  */
  if (!h_proc)
    {
      TOKEN_PRIVILEGES priv_current;

      if (enable_privilege (SE_DEBUG_NAME, TRUE, &priv_current))
	{
	  h_proc = OpenProcess (PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
				FALSE, proc_id);
	  restore_privilege (&priv_current);
	  revert_to_self ();
	}
    }
  if (h_proc)
    {
      result = open_process_token (h_proc, TOKEN_QUERY, &token);
      if (result)
	{
	  result = get_token_information (token, TokenUser, NULL, 0, &blen);
	  if (!result && GetLastError () == ERROR_INSUFFICIENT_BUFFER)
	    {
	      buf = xmalloc (blen);
	      result = get_token_information (token, TokenUser,
					      (LPVOID)buf, blen, &needed);
	      if (result)
		{
		  memcpy (&user_token, buf, sizeof (user_token));
		  if (!w32_cached_id (user_token.User.Sid, &euid, uname))
		    {
		      euid = get_rid (user_token.User.Sid);
		      result = lookup_account_sid (NULL, user_token.User.Sid,
						   uname, &ulength,
						   domain, &dlength,
						   &user_type);
		      if (result)
			w32_add_to_cache (user_token.User.Sid, euid, uname);
		      else
			{
			  strcpy (uname, "unknown");
			  result = TRUE;
			}
		    }
		  ulength = strlen (uname);
		}
	    }
	}
      if (result)
	{
	  /* Determine a reasonable euid and gid values.  */
	  if (xstrcasecmp ("administrator", uname) == 0)
	    {
	      euid = 500;	/* well-known Administrator uid */
	      egid = 513;	/* well-known None gid */
	    }
	  else
	    {
	      /* Get group id and name.  */
	      result = get_token_information (token, TokenPrimaryGroup,
					      (LPVOID)buf, blen, &needed);
	      if (!result && GetLastError () == ERROR_INSUFFICIENT_BUFFER)
		{
		  buf = xrealloc (buf, blen = needed);
		  result = get_token_information (token, TokenPrimaryGroup,
						  (LPVOID)buf, blen, &needed);
		}
	      if (result)
		{
		  memcpy (&group_token, buf, sizeof (group_token));
		  if (!w32_cached_id (group_token.PrimaryGroup, &egid, gname))
		    {
		      egid = get_rid (group_token.PrimaryGroup);
		      dlength = sizeof (domain);
		      result =
			lookup_account_sid (NULL, group_token.PrimaryGroup,
					    gname, &glength, NULL, &dlength,
					    &user_type);
		      if (result)
			w32_add_to_cache (group_token.PrimaryGroup,
					  egid, gname);
		      else
			{
			  strcpy (gname, "None");
			  result = TRUE;
			}
		    }
		  glength = strlen (gname);
		}
	    }
	}
      xfree (buf);
    }
  if (!result)
    {
      if (!is_windows_9x ())
	{
	  /* We couldn't open the process token, presumably because of
	     insufficient access rights.  Assume this process is run
	     by the system.  */
	  strcpy (uname, "SYSTEM");
	  strcpy (gname, "None");
	  euid = 18;	/* SYSTEM */
	  egid = 513;	/* None */
	  glength = strlen (gname);
	  ulength = strlen (uname);
	}
      /* If we are running under Windows 9X, where security calls are
	 not supported, we assume all processes are run by the current
	 user.  */
      else if (GetUserName (uname, &ulength))
	{
	  if (xstrcasecmp ("administrator", uname) == 0)
	    euid = 0;
	  else
	    euid = 123;
	  egid = euid;
	  strcpy (gname, "None");
	  glength = strlen (gname);
	  ulength = strlen (uname);
	}
      else
	{
	  euid = 123;
	  egid = 123;
	  strcpy (uname, "administrator");
	  ulength = strlen (uname);
	  strcpy (gname, "None");
	  glength = strlen (gname);
	}
      if (token)
	CloseHandle (token);
    }

  attrs = Fcons (Fcons (Qeuid, make_fixnum_or_float (euid)), attrs);
  tem = make_unibyte_string (uname, ulength);
  attrs = Fcons (Fcons (Quser,
			 code_convert_string_norecord (tem, Vlocale_coding_system, 0)),
		 attrs);
  attrs = Fcons (Fcons (Qegid, make_fixnum_or_float (egid)), attrs);
  tem = make_unibyte_string (gname, glength);
  attrs = Fcons (Fcons (Qgroup,
			 code_convert_string_norecord (tem, Vlocale_coding_system, 0)),
		 attrs);

  if (global_memory_status_ex (&memstex))
#if __GNUC__ || (defined (_MSC_VER) && _MSC_VER >= 1300)
    totphys = memstex.ullTotalPhys / 1024.0;
#else
  /* Visual Studio 6 cannot convert an unsigned __int64 type to
     double, so we need to do this for it...  */
    {
      DWORD tot_hi = memstex.ullTotalPhys >> 32;
      DWORD tot_md = (memstex.ullTotalPhys & 0x00000000ffffffff) >> 10;
      DWORD tot_lo = memstex.ullTotalPhys % 1024;

      totphys = tot_hi * 4194304.0 + tot_md + tot_lo / 1024.0;
    }
#endif	/* __GNUC__ || _MSC_VER >= 1300 */
  else if (global_memory_status (&memst))
    totphys = memst.dwTotalPhys / 1024.0;

  if (h_proc
      && get_process_memory_info (h_proc, (PROCESS_MEMORY_COUNTERS *)&mem_ex,
				  sizeof (mem_ex)))
    {
      DWORD rss = mem_ex.WorkingSetSize / 1024;

      attrs = Fcons (Fcons (Qmajflt,
			    make_fixnum_or_float (mem_ex.PageFaultCount)),
		     attrs);
      attrs = Fcons (Fcons (Qvsize,
			    make_fixnum_or_float (mem_ex.PrivateUsage / 1024)),
		     attrs);
      attrs = Fcons (Fcons (Qrss, make_fixnum_or_float (rss)), attrs);
      if (totphys)
	attrs = Fcons (Fcons (Qpmem, make_float (100. * rss / totphys)), attrs);
    }
  else if (h_proc
	   && get_process_memory_info (h_proc, &mem, sizeof (mem)))
    {
      DWORD rss = mem_ex.WorkingSetSize / 1024;

      attrs = Fcons (Fcons (Qmajflt,
			    make_fixnum_or_float (mem.PageFaultCount)),
		     attrs);
      attrs = Fcons (Fcons (Qrss, make_fixnum_or_float (rss)), attrs);
      if (totphys)
	attrs = Fcons (Fcons (Qpmem, make_float (100. * rss / totphys)), attrs);
    }
  else if (h_proc
	   && get_process_working_set_size (h_proc, &minrss, &maxrss))
    {
      DWORD rss = maxrss / 1024;

      attrs = Fcons (Fcons (Qrss, make_fixnum_or_float (maxrss / 1024)), attrs);
      if (totphys)
	attrs = Fcons (Fcons (Qpmem, make_float (100. * rss / totphys)), attrs);
    }

  if (process_times (h_proc, &ctime, &etime, &stime, &utime, &ttime, &pcpu))
    {
      attrs = Fcons (Fcons (Qutime, utime), attrs);
      attrs = Fcons (Fcons (Qstime, stime), attrs);
      attrs = Fcons (Fcons (Qtime,  ttime), attrs);
      attrs = Fcons (Fcons (Qstart, ctime), attrs);
      attrs = Fcons (Fcons (Qetime, etime), attrs);
      attrs = Fcons (Fcons (Qpcpu, make_float (pcpu)), attrs);
    }

  /* FIXME: Retrieve command line by walking the PEB of the process.  */

  if (h_proc)
    CloseHandle (h_proc);
  UNGCPRO;
  return attrs;
}


/* Wrappers for  winsock functions to map between our file descriptors
   and winsock's handles; also set h_errno for convenience.

   To allow Emacs to run on systems which don't have winsock support
   installed, we dynamically link to winsock on startup if present, and
   otherwise provide the minimum necessary functionality
   (eg. gethostname). */

/* function pointers for relevant socket functions */
int (PASCAL *pfn_WSAStartup) (WORD wVersionRequired, LPWSADATA lpWSAData);
void (PASCAL *pfn_WSASetLastError) (int iError);
int (PASCAL *pfn_WSAGetLastError) (void);
int (PASCAL *pfn_WSAEventSelect) (SOCKET s, HANDLE hEventObject, long lNetworkEvents);
HANDLE (PASCAL *pfn_WSACreateEvent) (void);
int (PASCAL *pfn_WSACloseEvent) (HANDLE hEvent);
int (PASCAL *pfn_socket) (int af, int type, int protocol);
int (PASCAL *pfn_bind) (SOCKET s, const struct sockaddr *addr, int namelen);
int (PASCAL *pfn_connect) (SOCKET s, const struct sockaddr *addr, int namelen);
int (PASCAL *pfn_ioctlsocket) (SOCKET s, long cmd, u_long *argp);
int (PASCAL *pfn_recv) (SOCKET s, char * buf, int len, int flags);
int (PASCAL *pfn_send) (SOCKET s, const char * buf, int len, int flags);
int (PASCAL *pfn_closesocket) (SOCKET s);
int (PASCAL *pfn_shutdown) (SOCKET s, int how);
int (PASCAL *pfn_WSACleanup) (void);

u_short (PASCAL *pfn_htons) (u_short hostshort);
u_short (PASCAL *pfn_ntohs) (u_short netshort);
unsigned long (PASCAL *pfn_inet_addr) (const char * cp);
int (PASCAL *pfn_gethostname) (char * name, int namelen);
struct hostent * (PASCAL *pfn_gethostbyname) (const char * name);
struct servent * (PASCAL *pfn_getservbyname) (const char * name, const char * proto);
int (PASCAL *pfn_getpeername) (SOCKET s, struct sockaddr *addr, int * namelen);
int (PASCAL *pfn_setsockopt) (SOCKET s, int level, int optname,
			      const char * optval, int optlen);
int (PASCAL *pfn_listen) (SOCKET s, int backlog);
int (PASCAL *pfn_getsockname) (SOCKET s, struct sockaddr * name,
			       int * namelen);
SOCKET (PASCAL *pfn_accept) (SOCKET s, struct sockaddr * addr, int * addrlen);
int (PASCAL *pfn_recvfrom) (SOCKET s, char * buf, int len, int flags,
		       struct sockaddr * from, int * fromlen);
int (PASCAL *pfn_sendto) (SOCKET s, const char * buf, int len, int flags,
			  const struct sockaddr * to, int tolen);

/* SetHandleInformation is only needed to make sockets non-inheritable. */
BOOL (WINAPI *pfn_SetHandleInformation) (HANDLE object, DWORD mask, DWORD flags);
#ifndef HANDLE_FLAG_INHERIT
#define HANDLE_FLAG_INHERIT	1
#endif

HANDLE winsock_lib;
static int winsock_inuse;

BOOL
term_winsock (void)
{
  if (winsock_lib != NULL && winsock_inuse == 0)
    {
      /* Not sure what would cause WSAENETDOWN, or even if it can happen
	 after WSAStartup returns successfully, but it seems reasonable
	 to allow unloading winsock anyway in that case. */
      if (pfn_WSACleanup () == 0 ||
	  pfn_WSAGetLastError () == WSAENETDOWN)
	{
	  if (FreeLibrary (winsock_lib))
	  winsock_lib = NULL;
	  return TRUE;
	}
    }
  return FALSE;
}

BOOL
init_winsock (int load_now)
{
  WSADATA  winsockData;

  if (winsock_lib != NULL)
    return TRUE;

  pfn_SetHandleInformation
    = (void *) GetProcAddress (GetModuleHandle ("kernel32.dll"),
			       "SetHandleInformation");

  winsock_lib = LoadLibrary ("Ws2_32.dll");

  if (winsock_lib != NULL)
    {
      /* dynamically link to socket functions */

#define LOAD_PROC(fn) \
      if ((pfn_##fn = (void *) GetProcAddress (winsock_lib, #fn)) == NULL) \
        goto fail;

      LOAD_PROC (WSAStartup);
      LOAD_PROC (WSASetLastError);
      LOAD_PROC (WSAGetLastError);
      LOAD_PROC (WSAEventSelect);
      LOAD_PROC (WSACreateEvent);
      LOAD_PROC (WSACloseEvent);
      LOAD_PROC (socket);
      LOAD_PROC (bind);
      LOAD_PROC (connect);
      LOAD_PROC (ioctlsocket);
      LOAD_PROC (recv);
      LOAD_PROC (send);
      LOAD_PROC (closesocket);
      LOAD_PROC (shutdown);
      LOAD_PROC (htons);
      LOAD_PROC (ntohs);
      LOAD_PROC (inet_addr);
      LOAD_PROC (gethostname);
      LOAD_PROC (gethostbyname);
      LOAD_PROC (getservbyname);
      LOAD_PROC (getpeername);
      LOAD_PROC (WSACleanup);
      LOAD_PROC (setsockopt);
      LOAD_PROC (listen);
      LOAD_PROC (getsockname);
      LOAD_PROC (accept);
      LOAD_PROC (recvfrom);
      LOAD_PROC (sendto);
#undef LOAD_PROC

      /* specify version 1.1 of winsock */
      if (pfn_WSAStartup (0x101, &winsockData) == 0)
        {
	  if (winsockData.wVersion != 0x101)
	    goto fail;

	  if (!load_now)
	    {
	      /* Report that winsock exists and is usable, but leave
		 socket functions disabled.  I am assuming that calling
		 WSAStartup does not require any network interaction,
		 and in particular does not cause or require a dial-up
		 connection to be established. */

	      pfn_WSACleanup ();
	      FreeLibrary (winsock_lib);
	      winsock_lib = NULL;
	    }
	  winsock_inuse = 0;
	  return TRUE;
	}

    fail:
      FreeLibrary (winsock_lib);
      winsock_lib = NULL;
    }

  return FALSE;
}


int h_errno = 0;

/* function to set h_errno for compatibility; map winsock error codes to
   normal system codes where they overlap (non-overlapping definitions
   are already in <sys/socket.h> */
static void
set_errno (void)
{
  if (winsock_lib == NULL)
    h_errno = EINVAL;
  else
    h_errno = pfn_WSAGetLastError ();

  switch (h_errno)
    {
    case WSAEACCES:		h_errno = EACCES; break;
    case WSAEBADF: 		h_errno = EBADF; break;
    case WSAEFAULT:		h_errno = EFAULT; break;
    case WSAEINTR: 		h_errno = EINTR; break;
    case WSAEINVAL:		h_errno = EINVAL; break;
    case WSAEMFILE:		h_errno = EMFILE; break;
    case WSAENAMETOOLONG: 	h_errno = ENAMETOOLONG; break;
    case WSAENOTEMPTY:		h_errno = ENOTEMPTY; break;
    }
  errno = h_errno;
}

static void
check_errno (void)
{
  if (h_errno == 0 && winsock_lib != NULL)
    pfn_WSASetLastError (0);
}

/* Extend strerror to handle the winsock-specific error codes.  */
struct {
  int errnum;
  char * msg;
} _wsa_errlist[] = {
  {WSAEINTR                , "Interrupted function call"},
  {WSAEBADF                , "Bad file descriptor"},
  {WSAEACCES               , "Permission denied"},
  {WSAEFAULT               , "Bad address"},
  {WSAEINVAL               , "Invalid argument"},
  {WSAEMFILE               , "Too many open files"},

  {WSAEWOULDBLOCK          , "Resource temporarily unavailable"},
  {WSAEINPROGRESS          , "Operation now in progress"},
  {WSAEALREADY             , "Operation already in progress"},
  {WSAENOTSOCK             , "Socket operation on non-socket"},
  {WSAEDESTADDRREQ         , "Destination address required"},
  {WSAEMSGSIZE             , "Message too long"},
  {WSAEPROTOTYPE           , "Protocol wrong type for socket"},
  {WSAENOPROTOOPT          , "Bad protocol option"},
  {WSAEPROTONOSUPPORT      , "Protocol not supported"},
  {WSAESOCKTNOSUPPORT      , "Socket type not supported"},
  {WSAEOPNOTSUPP           , "Operation not supported"},
  {WSAEPFNOSUPPORT         , "Protocol family not supported"},
  {WSAEAFNOSUPPORT         , "Address family not supported by protocol family"},
  {WSAEADDRINUSE           , "Address already in use"},
  {WSAEADDRNOTAVAIL        , "Cannot assign requested address"},
  {WSAENETDOWN             , "Network is down"},
  {WSAENETUNREACH          , "Network is unreachable"},
  {WSAENETRESET            , "Network dropped connection on reset"},
  {WSAECONNABORTED         , "Software caused connection abort"},
  {WSAECONNRESET           , "Connection reset by peer"},
  {WSAENOBUFS              , "No buffer space available"},
  {WSAEISCONN              , "Socket is already connected"},
  {WSAENOTCONN             , "Socket is not connected"},
  {WSAESHUTDOWN            , "Cannot send after socket shutdown"},
  {WSAETOOMANYREFS         , "Too many references"},	    /* not sure */
  {WSAETIMEDOUT            , "Connection timed out"},
  {WSAECONNREFUSED         , "Connection refused"},
  {WSAELOOP                , "Network loop"},		    /* not sure */
  {WSAENAMETOOLONG         , "Name is too long"},
  {WSAEHOSTDOWN            , "Host is down"},
  {WSAEHOSTUNREACH         , "No route to host"},
  {WSAENOTEMPTY            , "Buffer not empty"},	    /* not sure */
  {WSAEPROCLIM             , "Too many processes"},
  {WSAEUSERS               , "Too many users"},		    /* not sure */
  {WSAEDQUOT               , "Double quote in host name"},  /* really not sure */
  {WSAESTALE               , "Data is stale"},		    /* not sure */
  {WSAEREMOTE              , "Remote error"},		    /* not sure */

  {WSASYSNOTREADY          , "Network subsystem is unavailable"},
  {WSAVERNOTSUPPORTED      , "WINSOCK.DLL version out of range"},
  {WSANOTINITIALISED       , "Winsock not initialized successfully"},
  {WSAEDISCON              , "Graceful shutdown in progress"},
#ifdef WSAENOMORE
  {WSAENOMORE              , "No more operations allowed"}, /* not sure */
  {WSAECANCELLED           , "Operation cancelled"},	    /* not sure */
  {WSAEINVALIDPROCTABLE    , "Invalid procedure table from service provider"},
  {WSAEINVALIDPROVIDER     , "Invalid service provider version number"},
  {WSAEPROVIDERFAILEDINIT  , "Unable to initialize a service provider"},
  {WSASYSCALLFAILURE       , "System call failure"},
  {WSASERVICE_NOT_FOUND    , "Service not found"},	    /* not sure */
  {WSATYPE_NOT_FOUND       , "Class type not found"},
  {WSA_E_NO_MORE           , "No more resources available"}, /* really not sure */
  {WSA_E_CANCELLED         , "Operation already cancelled"}, /* really not sure */
  {WSAEREFUSED             , "Operation refused"},	    /* not sure */
#endif

  {WSAHOST_NOT_FOUND       , "Host not found"},
  {WSATRY_AGAIN            , "Authoritative host not found during name lookup"},
  {WSANO_RECOVERY          , "Non-recoverable error during name lookup"},
  {WSANO_DATA              , "Valid name, no data record of requested type"},

  {-1, NULL}
};

char *
sys_strerror (int error_no)
{
  int i;
  static char unknown_msg[40];

  if (error_no >= 0 && error_no < sys_nerr)
    return sys_errlist[error_no];

  for (i = 0; _wsa_errlist[i].errnum >= 0; i++)
    if (_wsa_errlist[i].errnum == error_no)
      return _wsa_errlist[i].msg;

  sprintf (unknown_msg, "Unidentified error: %d", error_no);
  return unknown_msg;
}

/* [andrewi 3-May-96] I've had conflicting results using both methods,
   but I believe the method of keeping the socket handle separate (and
   insuring it is not inheritable) is the correct one. */

#define SOCK_HANDLE(fd) ((SOCKET) fd_info[fd].hnd)

static int socket_to_fd (SOCKET s);

int
sys_socket (int af, int type, int protocol)
{
  SOCKET s;

  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return INVALID_SOCKET;
    }

  check_errno ();

  /* call the real socket function */
  s = pfn_socket (af, type, protocol);

  if (s != INVALID_SOCKET)
    return socket_to_fd (s);

  set_errno ();
  return -1;
}

/* Convert a SOCKET to a file descriptor.  */
static int
socket_to_fd (SOCKET s)
{
  int fd;
  child_process * cp;

  /* Although under NT 3.5 _open_osfhandle will accept a socket
     handle, if opened with SO_OPENTYPE == SO_SYNCHRONOUS_NONALERT,
     that does not work under NT 3.1.  However, we can get the same
     effect by using a backdoor function to replace an existing
     descriptor handle with the one we want. */

  /* allocate a file descriptor (with appropriate flags) */
  fd = _open ("NUL:", _O_RDWR);
  if (fd >= 0)
    {
      /* Make a non-inheritable copy of the socket handle.  Note
	 that it is possible that sockets aren't actually kernel
	 handles, which appears to be the case on Windows 9x when
	 the MS Proxy winsock client is installed.  */
      {
	/* Apparently there is a bug in NT 3.51 with some service
	   packs, which prevents using DuplicateHandle to make a
	   socket handle non-inheritable (causes WSACleanup to
	   hang).  The work-around is to use SetHandleInformation
	   instead if it is available and implemented. */
	if (pfn_SetHandleInformation)
	  {
	    pfn_SetHandleInformation ((HANDLE) s, HANDLE_FLAG_INHERIT, 0);
	  }
	else
	  {
	    HANDLE parent = GetCurrentProcess ();
	    HANDLE new_s = INVALID_HANDLE_VALUE;

	    if (DuplicateHandle (parent,
				 (HANDLE) s,
				 parent,
				 &new_s,
				 0,
				 FALSE,
				 DUPLICATE_SAME_ACCESS))
	      {
		/* It is possible that DuplicateHandle succeeds even
		   though the socket wasn't really a kernel handle,
		   because a real handle has the same value.  So
		   test whether the new handle really is a socket.  */
		long nonblocking = 0;
		if (pfn_ioctlsocket ((SOCKET) new_s, FIONBIO, &nonblocking) == 0)
		  {
		    pfn_closesocket (s);
		    s = (SOCKET) new_s;
		  }
		else
		  {
		    CloseHandle (new_s);
		  }
	      }
	  }
      }
      fd_info[fd].hnd = (HANDLE) s;

      /* set our own internal flags */
      fd_info[fd].flags = FILE_SOCKET | FILE_BINARY | FILE_READ | FILE_WRITE;

      cp = new_child ();
      if (cp)
	{
	  cp->fd = fd;
	  cp->status = STATUS_READ_ACKNOWLEDGED;

	  /* attach child_process to fd_info */
	  if (fd_info[ fd ].cp != NULL)
	    {
	      DebPrint (("sys_socket: fd_info[%d] apparently in use!\n", fd));
	      abort ();
	    }

	  fd_info[ fd ].cp = cp;

	  /* success! */
	  winsock_inuse++;	/* count open sockets */
	  return fd;
	}

      /* clean up */
      _close (fd);
    }
  pfn_closesocket (s);
  h_errno = EMFILE;
  return -1;
}

int
sys_bind (int s, const struct sockaddr * addr, int namelen)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENOTSOCK;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_bind (SOCK_HANDLE (s), addr, namelen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_connect (int s, const struct sockaddr * name, int namelen)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENOTSOCK;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_connect (SOCK_HANDLE (s), name, namelen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

u_short
sys_htons (u_short hostshort)
{
  return (winsock_lib != NULL) ?
    pfn_htons (hostshort) : hostshort;
}

u_short
sys_ntohs (u_short netshort)
{
  return (winsock_lib != NULL) ?
    pfn_ntohs (netshort) : netshort;
}

unsigned long
sys_inet_addr (const char * cp)
{
  return (winsock_lib != NULL) ?
    pfn_inet_addr (cp) : INADDR_NONE;
}

int
sys_gethostname (char * name, int namelen)
{
  if (winsock_lib != NULL)
    return pfn_gethostname (name, namelen);

  if (namelen > MAX_COMPUTERNAME_LENGTH)
    return !GetComputerName (name, (DWORD *)&namelen);

  h_errno = EFAULT;
  return SOCKET_ERROR;
}

struct hostent *
sys_gethostbyname (const char * name)
{
  struct hostent * host;

  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return NULL;
    }

  check_errno ();
  host = pfn_gethostbyname (name);
  if (!host)
    set_errno ();
  return host;
}

struct servent *
sys_getservbyname (const char * name, const char * proto)
{
  struct servent * serv;

  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return NULL;
    }

  check_errno ();
  serv = pfn_getservbyname (name, proto);
  if (!serv)
    set_errno ();
  return serv;
}

int
sys_getpeername (int s, struct sockaddr *addr, int * namelen)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_getpeername (SOCK_HANDLE (s), addr, namelen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_shutdown (int s, int how)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_shutdown (SOCK_HANDLE (s), how);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_setsockopt (int s, int level, int optname, const void * optval, int optlen)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_setsockopt (SOCK_HANDLE (s), level, optname,
			       (const char *)optval, optlen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_listen (int s, int backlog)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_listen (SOCK_HANDLE (s), backlog);
      if (rc == SOCKET_ERROR)
	set_errno ();
      else
	fd_info[s].flags |= FILE_LISTEN;
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_getsockname (int s, struct sockaddr * name, int * namelen)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_getsockname (SOCK_HANDLE (s), name, namelen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_accept (int s, struct sockaddr * addr, int * addrlen)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return -1;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_LISTEN)
    {
      SOCKET t = pfn_accept (SOCK_HANDLE (s), addr, addrlen);
      int fd = -1;
      if (t == INVALID_SOCKET)
	set_errno ();
      else
	fd = socket_to_fd (t);

      fd_info[s].cp->status = STATUS_READ_ACKNOWLEDGED;
      ResetEvent (fd_info[s].cp->char_avail);
      return fd;
    }
  h_errno = ENOTSOCK;
  return -1;
}

int
sys_recvfrom (int s, char * buf, int len, int flags,
	      struct sockaddr * from, int * fromlen)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_recvfrom (SOCK_HANDLE (s), buf, len, flags, from, fromlen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_sendto (int s, const char * buf, int len, int flags,
	    const struct sockaddr * to, int tolen)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_sendto (SOCK_HANDLE (s), buf, len, flags, to, tolen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}

/* Windows does not have an fcntl function.  Provide an implementation
   solely for making sockets non-blocking.  */
int
fcntl (int s, int cmd, int options)
{
  if (winsock_lib == NULL)
    {
      h_errno = ENETDOWN;
      return -1;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      if (cmd == F_SETFL && options == O_NDELAY)
	{
	  unsigned long nblock = 1;
	  int rc = pfn_ioctlsocket (SOCK_HANDLE (s), FIONBIO, &nblock);
	  if (rc == SOCKET_ERROR)
	    set_errno ();
	  /* Keep track of the fact that we set this to non-blocking.  */
	  fd_info[s].flags |= FILE_NDELAY;
	  return rc;
	}
      else
	{
	  h_errno = EINVAL;
	  return SOCKET_ERROR;
	}
    }
  h_errno = ENOTSOCK;
  return SOCKET_ERROR;
}


/* Shadow main io functions: we need to handle pipes and sockets more
   intelligently, and implement non-blocking mode as well. */

int
sys_close (int fd)
{
  int rc;

  if (fd < 0)
    {
      errno = EBADF;
      return -1;
    }

  if (fd < MAXDESC && fd_info[fd].cp)
    {
      child_process * cp = fd_info[fd].cp;

      fd_info[fd].cp = NULL;

      if (CHILD_ACTIVE (cp))
        {
	  /* if last descriptor to active child_process then cleanup */
	  int i;
	  for (i = 0; i < MAXDESC; i++)
	    {
	      if (i == fd)
		continue;
	      if (fd_info[i].cp == cp)
		break;
	    }
	  if (i == MAXDESC)
	    {
	      if (fd_info[fd].flags & FILE_SOCKET)
		{
		  if (winsock_lib == NULL) abort ();

		  pfn_shutdown (SOCK_HANDLE (fd), 2);
		  rc = pfn_closesocket (SOCK_HANDLE (fd));

		  winsock_inuse--; /* count open sockets */
		}
	      delete_child (cp);
	    }
	}
    }

  /* Note that sockets do not need special treatment here (at least on
     NT and Windows 95 using the standard tcp/ip stacks) - it appears that
     closesocket is equivalent to CloseHandle, which is to be expected
     because socket handles are fully fledged kernel handles. */
  rc = _close (fd);

  if (rc == 0 && fd < MAXDESC)
    fd_info[fd].flags = 0;

  return rc;
}

int
sys_dup (int fd)
{
  int new_fd;

  new_fd = _dup (fd);
  if (new_fd >= 0 && new_fd < MAXDESC)
    {
      /* duplicate our internal info as well */
      fd_info[new_fd] = fd_info[fd];
    }
  return new_fd;
}

int
sys_dup2 (int src, int dst)
{
  int rc;

  if (dst < 0 || dst >= MAXDESC)
    {
      errno = EBADF;
      return -1;
    }

  /* make sure we close the destination first if it's a pipe or socket */
  if (src != dst && fd_info[dst].flags != 0)
    sys_close (dst);

  rc = _dup2 (src, dst);
  if (rc == 0)
    {
      /* duplicate our internal info as well */
      fd_info[dst] = fd_info[src];
    }
  return rc;
}

/* Unix pipe() has only one arg */
int
sys_pipe (int * phandles)
{
  int rc;
  unsigned flags;

  /* make pipe handles non-inheritable; when we spawn a child, we
     replace the relevant handle with an inheritable one.  Also put
     pipes into binary mode; we will do text mode translation ourselves
     if required.  */
  rc = _pipe (phandles, 0, _O_NOINHERIT | _O_BINARY);

  if (rc == 0)
    {
      /* Protect against overflow, since Windows can open more handles than
	 our fd_info array has room for.  */
      if (phandles[0] >= MAXDESC || phandles[1] >= MAXDESC)
	{
	  _close (phandles[0]);
	  _close (phandles[1]);
	  rc = -1;
	}
      else
	{
	  flags = FILE_PIPE | FILE_READ | FILE_BINARY;
	  fd_info[phandles[0]].flags = flags;

	  flags = FILE_PIPE | FILE_WRITE | FILE_BINARY;
	  fd_info[phandles[1]].flags = flags;
	}
    }

  return rc;
}

/* Function to do blocking read of one byte, needed to implement
   select.  It is only allowed on sockets and pipes. */
int
_sys_read_ahead (int fd)
{
  child_process * cp;
  int rc;

  if (fd < 0 || fd >= MAXDESC)
    return STATUS_READ_ERROR;

  cp = fd_info[fd].cp;

  if (cp == NULL || cp->fd != fd || cp->status != STATUS_READ_READY)
    return STATUS_READ_ERROR;

  if ((fd_info[fd].flags & (FILE_PIPE | FILE_SERIAL | FILE_SOCKET)) == 0
      || (fd_info[fd].flags & FILE_READ) == 0)
    {
      DebPrint (("_sys_read_ahead: internal error: fd %d is not a pipe, serial port, or socket!\n", fd));
      abort ();
    }

  cp->status = STATUS_READ_IN_PROGRESS;

  if (fd_info[fd].flags & FILE_PIPE)
    {
      rc = _read (fd, &cp->chr, sizeof (char));

      /* Give subprocess time to buffer some more output for us before
	 reporting that input is available; we need this because Windows 95
	 connects DOS programs to pipes by making the pipe appear to be
	 the normal console stdout - as a result most DOS programs will
	 write to stdout without buffering, ie. one character at a
	 time.  Even some W32 programs do this - "dir" in a command
	 shell on NT is very slow if we don't do this. */
      if (rc > 0)
	{
	  int wait = w32_pipe_read_delay;

	  if (wait > 0)
	    Sleep (wait);
	  else if (wait < 0)
	    while (++wait <= 0)
	      /* Yield remainder of our time slice, effectively giving a
		 temporary priority boost to the child process. */
	      Sleep (0);
	}
    }
  else if (fd_info[fd].flags & FILE_SERIAL)
    {
      HANDLE hnd = fd_info[fd].hnd;
      OVERLAPPED *ovl = &fd_info[fd].cp->ovl_read;
      COMMTIMEOUTS ct;

      /* Configure timeouts for blocking read.  */
      if (!GetCommTimeouts (hnd, &ct))
	return STATUS_READ_ERROR;
      ct.ReadIntervalTimeout		= 0;
      ct.ReadTotalTimeoutMultiplier	= 0;
      ct.ReadTotalTimeoutConstant	= 0;
      if (!SetCommTimeouts (hnd, &ct))
	return STATUS_READ_ERROR;

      if (!ReadFile (hnd, &cp->chr, sizeof (char), (DWORD*) &rc, ovl))
	{
	  if (GetLastError () != ERROR_IO_PENDING)
	    return STATUS_READ_ERROR;
	  if (!GetOverlappedResult (hnd, ovl, (DWORD*) &rc, TRUE))
	    return STATUS_READ_ERROR;
	}
    }
  else if (fd_info[fd].flags & FILE_SOCKET)
    {
      unsigned long nblock = 0;
      /* We always want this to block, so temporarily disable NDELAY.  */
      if (fd_info[fd].flags & FILE_NDELAY)
	pfn_ioctlsocket (SOCK_HANDLE (fd), FIONBIO, &nblock);

      rc = pfn_recv (SOCK_HANDLE (fd), &cp->chr, sizeof (char), 0);

      if (fd_info[fd].flags & FILE_NDELAY)
	{
	  nblock = 1;
	  pfn_ioctlsocket (SOCK_HANDLE (fd), FIONBIO, &nblock);
	}
    }

  if (rc == sizeof (char))
    cp->status = STATUS_READ_SUCCEEDED;
  else
    cp->status = STATUS_READ_FAILED;

  return cp->status;
}

int
_sys_wait_accept (int fd)
{
  HANDLE hEv;
  child_process * cp;
  int rc;

  if (fd < 0 || fd >= MAXDESC)
    return STATUS_READ_ERROR;

  cp = fd_info[fd].cp;

  if (cp == NULL || cp->fd != fd || cp->status != STATUS_READ_READY)
    return STATUS_READ_ERROR;

  cp->status = STATUS_READ_FAILED;

  hEv = pfn_WSACreateEvent ();
  rc = pfn_WSAEventSelect (SOCK_HANDLE (fd), hEv, FD_ACCEPT);
  if (rc != SOCKET_ERROR)
    {
      rc = WaitForSingleObject (hEv, INFINITE);
      pfn_WSAEventSelect (SOCK_HANDLE (fd), NULL, 0);
      if (rc == WAIT_OBJECT_0)
	cp->status = STATUS_READ_SUCCEEDED;
    }
  pfn_WSACloseEvent (hEv);

  return cp->status;
}

int
sys_read (int fd, char * buffer, unsigned int count)
{
  int nchars;
  int to_read;
  DWORD waiting;
  char * orig_buffer = buffer;

  if (fd < 0)
    {
      errno = EBADF;
      return -1;
    }

  if (fd < MAXDESC && fd_info[fd].flags & (FILE_PIPE | FILE_SOCKET | FILE_SERIAL))
    {
      child_process *cp = fd_info[fd].cp;

      if ((fd_info[fd].flags & FILE_READ) == 0)
        {
	  errno = EBADF;
	  return -1;
	}

      nchars = 0;

      /* re-read CR carried over from last read */
      if (fd_info[fd].flags & FILE_LAST_CR)
	{
	  if (fd_info[fd].flags & FILE_BINARY) abort ();
	  *buffer++ = 0x0d;
	  count--;
	  nchars++;
	  fd_info[fd].flags &= ~FILE_LAST_CR;
	}

      /* presence of a child_process structure means we are operating in
	 non-blocking mode - otherwise we just call _read directly.
	 Note that the child_process structure might be missing because
	 reap_subprocess has been called; in this case the pipe is
	 already broken, so calling _read on it is okay. */
      if (cp)
        {
	  int current_status = cp->status;

	  switch (current_status)
	    {
	    case STATUS_READ_FAILED:
	    case STATUS_READ_ERROR:
	      /* report normal EOF if nothing in buffer */
	      if (nchars <= 0)
		fd_info[fd].flags |= FILE_AT_EOF;
	      return nchars;

	    case STATUS_READ_READY:
	    case STATUS_READ_IN_PROGRESS:
	      DebPrint (("sys_read called when read is in progress\n"));
	      errno = EWOULDBLOCK;
	      return -1;

	    case STATUS_READ_SUCCEEDED:
	      /* consume read-ahead char */
	      *buffer++ = cp->chr;
	      count--;
	      nchars++;
	      cp->status = STATUS_READ_ACKNOWLEDGED;
	      ResetEvent (cp->char_avail);

	    case STATUS_READ_ACKNOWLEDGED:
	      break;

	    default:
	      DebPrint (("sys_read: bad status %d\n", current_status));
	      errno = EBADF;
	      return -1;
	    }

	  if (fd_info[fd].flags & FILE_PIPE)
	    {
	      PeekNamedPipe ((HANDLE) _get_osfhandle (fd), NULL, 0, NULL, &waiting, NULL);
	      to_read = min (waiting, (DWORD) count);

	      if (to_read > 0)
		nchars += _read (fd, buffer, to_read);
	    }
	  else if (fd_info[fd].flags & FILE_SERIAL)
	    {
	      HANDLE hnd = fd_info[fd].hnd;
	      OVERLAPPED *ovl = &fd_info[fd].cp->ovl_read;
	      int rc = 0;
	      COMMTIMEOUTS ct;

	      if (count > 0)
		{
		  /* Configure timeouts for non-blocking read.  */
		  if (!GetCommTimeouts (hnd, &ct))
		    {
		      errno = EIO;
		      return -1;
		    }
		  ct.ReadIntervalTimeout	 = MAXDWORD;
		  ct.ReadTotalTimeoutMultiplier	 = 0;
		  ct.ReadTotalTimeoutConstant	 = 0;
		  if (!SetCommTimeouts (hnd, &ct))
		    {
		      errno = EIO;
		      return -1;
		    }

		  if (!ResetEvent (ovl->hEvent))
		    {
		      errno = EIO;
		      return -1;
		    }
		  if (!ReadFile (hnd, buffer, count, (DWORD*) &rc, ovl))
		    {
		      if (GetLastError () != ERROR_IO_PENDING)
			{
			  errno = EIO;
			  return -1;
			}
		      if (!GetOverlappedResult (hnd, ovl, (DWORD*) &rc, TRUE))
			{
			  errno = EIO;
			  return -1;
			}
		    }
		  nchars += rc;
		}
	    }
	  else /* FILE_SOCKET */
	    {
	      if (winsock_lib == NULL) abort ();

	      /* do the equivalent of a non-blocking read */
	      pfn_ioctlsocket (SOCK_HANDLE (fd), FIONREAD, &waiting);
	      if (waiting == 0 && nchars == 0)
	        {
		  h_errno = errno = EWOULDBLOCK;
		  return -1;
		}

	      if (waiting)
	        {
		  /* always use binary mode for sockets */
		  int res = pfn_recv (SOCK_HANDLE (fd), buffer, count, 0);
		  if (res == SOCKET_ERROR)
		    {
		      DebPrint (("sys_read.recv failed with error %d on socket %ld\n",
				 pfn_WSAGetLastError (), SOCK_HANDLE (fd)));
		      set_errno ();
		      return -1;
		    }
		  nchars += res;
		}
	    }
	}
      else
	{
	  int nread = _read (fd, buffer, count);
	  if (nread >= 0)
	    nchars += nread;
	  else if (nchars == 0)
	    nchars = nread;
	}

      if (nchars <= 0)
	fd_info[fd].flags |= FILE_AT_EOF;
      /* Perform text mode translation if required.  */
      else if ((fd_info[fd].flags & FILE_BINARY) == 0)
	{
	  nchars = crlf_to_lf (nchars, orig_buffer);
	  /* If buffer contains only CR, return that.  To be absolutely
	     sure we should attempt to read the next char, but in
	     practice a CR to be followed by LF would not appear by
	     itself in the buffer.  */
	  if (nchars > 1 && orig_buffer[nchars - 1] == 0x0d)
	    {
	      fd_info[fd].flags |= FILE_LAST_CR;
	      nchars--;
	    }
	}
    }
  else
    nchars = _read (fd, buffer, count);

  return nchars;
}

/* From w32xfns.c */
extern HANDLE interrupt_handle;

/* For now, don't bother with a non-blocking mode */
int
sys_write (int fd, const void * buffer, unsigned int count)
{
  int nchars;

  if (fd < 0)
    {
      errno = EBADF;
      return -1;
    }

  if (fd < MAXDESC && fd_info[fd].flags & (FILE_PIPE | FILE_SOCKET | FILE_SERIAL))
    {
      if ((fd_info[fd].flags & FILE_WRITE) == 0)
	{
	  errno = EBADF;
	  return -1;
	}

      /* Perform text mode translation if required.  */
      if ((fd_info[fd].flags & FILE_BINARY) == 0)
	{
	  char * tmpbuf = alloca (count * 2);
	  unsigned char * src = (void *)buffer;
	  unsigned char * dst = tmpbuf;
	  int nbytes = count;

	  while (1)
	    {
	      unsigned char *next;
	      /* copy next line or remaining bytes */
	      next = _memccpy (dst, src, '\n', nbytes);
	      if (next)
		{
		  /* copied one line ending with '\n' */
		  int copied = next - dst;
		  nbytes -= copied;
		  src += copied;
		  /* insert '\r' before '\n' */
		  next[-1] = '\r';
		  next[0] = '\n';
		  dst = next + 1;
		  count++;
		}
	      else
		/* copied remaining partial line -> now finished */
		break;
	    }
	  buffer = tmpbuf;
	}
    }

  if (fd < MAXDESC && fd_info[fd].flags & FILE_SERIAL)
    {
      HANDLE hnd = (HANDLE) _get_osfhandle (fd);
      OVERLAPPED *ovl = &fd_info[fd].cp->ovl_write;
      HANDLE wait_hnd[2] = { interrupt_handle, ovl->hEvent };
      DWORD active = 0;

      if (!WriteFile (hnd, buffer, count, (DWORD*) &nchars, ovl))
	{
	  if (GetLastError () != ERROR_IO_PENDING)
	    {
	      errno = EIO;
	      return -1;
	    }
	  if (detect_input_pending ())
	    active = MsgWaitForMultipleObjects (2, wait_hnd, FALSE, INFINITE,
						QS_ALLINPUT);
	  else
	    active = WaitForMultipleObjects (2, wait_hnd, FALSE, INFINITE);
	  if (active == WAIT_OBJECT_0)
	    { /* User pressed C-g, cancel write, then leave.  Don't bother
		 cleaning up as we may only get stuck in buggy drivers.  */
	      PurgeComm (hnd, PURGE_TXABORT | PURGE_TXCLEAR);
	      CancelIo (hnd);
	      errno = EIO;
	      return -1;
	    }
	  if (active == WAIT_OBJECT_0 + 1
	      && !GetOverlappedResult (hnd, ovl, (DWORD*) &nchars, TRUE))
	    {
	      errno = EIO;
	      return -1;
	    }
	}
    }
  else if (fd < MAXDESC && fd_info[fd].flags & FILE_SOCKET)
    {
      unsigned long nblock = 0;
      if (winsock_lib == NULL) abort ();

      /* TODO: implement select() properly so non-blocking I/O works. */
      /* For now, make sure the write blocks.  */
      if (fd_info[fd].flags & FILE_NDELAY)
	pfn_ioctlsocket (SOCK_HANDLE (fd), FIONBIO, &nblock);

      nchars =  pfn_send (SOCK_HANDLE (fd), buffer, count, 0);

      /* Set the socket back to non-blocking if it was before,
	 for other operations that support it.  */
      if (fd_info[fd].flags & FILE_NDELAY)
	{
	  nblock = 1;
	  pfn_ioctlsocket (SOCK_HANDLE (fd), FIONBIO, &nblock);
	}

      if (nchars == SOCKET_ERROR)
        {
	  DebPrint (("sys_write.send failed with error %d on socket %ld\n",
		     pfn_WSAGetLastError (), SOCK_HANDLE (fd)));
	  set_errno ();
	}
    }
  else
    {
      /* Some networked filesystems don't like too large writes, so
	 break them into smaller chunks.  See the Comments section of
	 the MSDN documentation of WriteFile for details behind the
	 choice of the value of CHUNK below.  See also the thread
	 http://thread.gmane.org/gmane.comp.version-control.git/145294
	 in the git mailing list.  */
      const unsigned char *p = buffer;
      const unsigned chunk = 30 * 1024 * 1024;

      nchars = 0;
      while (count > 0)
	{
	  unsigned this_chunk = count < chunk ? count : chunk;
	  int n = _write (fd, p, this_chunk);

	  nchars += n;
	  if (n < 0)
	    {
	      nchars = n;
	      break;
	    }
	  else if (n < this_chunk)
	    break;
	  count -= n;
	  p += n;
	}
    }

  return nchars;
}

/* The Windows CRT functions are "optimized for speed", so they don't
   check for timezone and DST changes if they were last called less
   than 1 minute ago (see http://support.microsoft.com/kb/821231).  So
   all Emacs features that repeatedly call time functions (e.g.,
   display-time) are in real danger of missing timezone and DST
   changes.  Calling tzset before each localtime call fixes that.  */
struct tm *
sys_localtime (const time_t *t)
{
  tzset ();
  return localtime (t);
}



/* Delayed loading of libraries.  */

Lisp_Object Vlibrary_cache;

/* The argument LIBRARIES is an alist that associates a symbol
   LIBRARY_ID, identifying an external DLL library known to Emacs, to
   a list of filenames under which the library is usually found.  In
   most cases, the argument passed as LIBRARIES is the variable
   `dynamic-library-alist', which is initialized to a list of common
   library names.  If the function loads the library successfully, it
   returns the handle of the DLL, and records the filename in the
   property :loaded-from of LIBRARY_ID; it returns NULL if the library
   could not be found, or when it was already loaded (because the
   handle is not recorded anywhere, and so is lost after use).  It
   would be trivial to save the handle too in :loaded-from, but
   currently there's no use case for it.  */
HMODULE
w32_delayed_load (Lisp_Object libraries, Lisp_Object library_id)
{
  HMODULE library_dll = NULL;

  CHECK_SYMBOL (library_id);

  if (CONSP (libraries) && NILP (Fassq (library_id, Vlibrary_cache)))
    {
      Lisp_Object found = Qnil;
      Lisp_Object dlls = Fassq (library_id, libraries);

      if (CONSP (dlls))
        for (dlls = XCDR (dlls); CONSP (dlls); dlls = XCDR (dlls))
          {
            CHECK_STRING_CAR (dlls);
            if ((library_dll = LoadLibrary (SDATA (XCAR (dlls)))))
              {
                found = XCAR (dlls);
                break;
              }
          }

      Fput (library_id, QCloaded_from, found);
    }

  return library_dll;
}


static void
check_windows_init_file (void)
{
  /* A common indication that Emacs is not installed properly is when
     it cannot find the Windows installation file.  If this file does
     not exist in the expected place, tell the user.  */

  if (!noninteractive && !inhibit_window_system
      /* Vload_path is not yet initialized when we are loading
	 loadup.el.  */
      && NILP (Vpurify_flag))
    {
      Lisp_Object objs[2];
      Lisp_Object full_load_path;
      Lisp_Object init_file;
      int fd;

      objs[0] = Vload_path;
      objs[1] = decode_env_path (0, (getenv ("EMACSLOADPATH")));
      full_load_path = Fappend (2, objs);
      init_file = build_string ("term/w32-win");
      fd = openp (full_load_path, init_file, Fget_load_suffixes (), NULL, Qnil);
      if (fd < 0)
	{
	  Lisp_Object load_path_print = Fprin1_to_string (full_load_path, Qnil);
	  char *init_file_name = SDATA (init_file);
	  char *load_path = SDATA (load_path_print);
	  char *buffer = alloca (1024
				 + strlen (init_file_name)
				 + strlen (load_path));

	  sprintf (buffer,
		   "The Emacs Windows initialization file \"%s.el\" "
		   "could not be found in your Emacs installation.  "
		   "Emacs checked the following directories for this file:\n"
		   "\n%s\n\n"
		   "When Emacs cannot find this file, it usually means that it "
		   "was not installed properly, or its distribution file was "
		   "not unpacked properly.\nSee the README.W32 file in the "
		   "top-level Emacs directory for more information.",
		   init_file_name, load_path);
	  MessageBox (NULL,
		      buffer,
		      "Emacs Abort Dialog",
		      MB_OK | MB_ICONEXCLAMATION | MB_TASKMODAL);
      /* Use the low-level Emacs abort. */
#undef abort
	  abort ();
	}
      else
	{
	  _close (fd);
	}
    }
}

void
term_ntproc (void)
{
  /* shutdown the socket interface if necessary */
  term_winsock ();

  term_w32select ();
}

void
init_ntproc (void)
{
  /* Initialize the socket interface now if available and requested by
     the user by defining PRELOAD_WINSOCK; otherwise loading will be
     delayed until open-network-stream is called (w32-has-winsock can
     also be used to dynamically load or reload winsock).

     Conveniently, init_environment is called before us, so
     PRELOAD_WINSOCK can be set in the registry. */

  /* Always initialize this correctly. */
  winsock_lib = NULL;

  if (getenv ("PRELOAD_WINSOCK") != NULL)
    init_winsock (TRUE);

  /* Initial preparation for subprocess support: replace our standard
     handles with non-inheritable versions. */
  {
    HANDLE parent;
    HANDLE stdin_save =  INVALID_HANDLE_VALUE;
    HANDLE stdout_save = INVALID_HANDLE_VALUE;
    HANDLE stderr_save = INVALID_HANDLE_VALUE;

    parent = GetCurrentProcess ();

    /* ignore errors when duplicating and closing; typically the
       handles will be invalid when running as a gui program. */
    DuplicateHandle (parent,
		     GetStdHandle (STD_INPUT_HANDLE),
		     parent,
		     &stdin_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);

    DuplicateHandle (parent,
		     GetStdHandle (STD_OUTPUT_HANDLE),
		     parent,
		     &stdout_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);

    DuplicateHandle (parent,
		     GetStdHandle (STD_ERROR_HANDLE),
		     parent,
		     &stderr_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);

    fclose (stdin);
    fclose (stdout);
    fclose (stderr);

    if (stdin_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stdin_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_RDONLY);
    _fdopen (0, "r");

    if (stdout_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stdout_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_WRONLY);
    _fdopen (1, "w");

    if (stderr_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stderr_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_WRONLY);
    _fdopen (2, "w");
  }

  /* unfortunately, atexit depends on implementation of malloc */
  /* atexit (term_ntproc); */
  signal (SIGABRT, term_ntproc);

  /* determine which drives are fixed, for GetCachedVolumeInformation */
  {
    /* GetDriveType must have trailing backslash. */
    char drive[] = "A:\\";

    /* Loop over all possible drive letters */
    while (*drive <= 'Z')
    {
      /* Record if this drive letter refers to a fixed drive. */
      fixed_drives[DRIVE_INDEX (*drive)] =
	(GetDriveType (drive) == DRIVE_FIXED);

      (*drive)++;
    }

    /* Reset the volume info cache.  */
    volume_cache = NULL;
  }

  /* Check to see if Emacs has been installed correctly.  */
  check_windows_init_file ();
}

/*
        shutdown_handler ensures that buffers' autosave files are
	up to date when the user logs off, or the system shuts down.
*/
static BOOL WINAPI
shutdown_handler (DWORD type)
{
  /* Ctrl-C and Ctrl-Break are already suppressed, so don't handle them.  */
  if (type == CTRL_CLOSE_EVENT        /* User closes console window.  */
      || type == CTRL_LOGOFF_EVENT    /* User logs off.  */
      || type == CTRL_SHUTDOWN_EVENT) /* User shutsdown.  */
    {
      /* Shut down cleanly, making sure autosave files are up to date.  */
      shut_down_emacs (0, 0, Qnil);
    }

  /* Allow other handlers to handle this signal.  */
  return FALSE;
}

/*
	globals_of_w32 is used to initialize those global variables that
	must always be initialized on startup even when the global variable
	initialized is non zero (see the function main in emacs.c).
*/
void
globals_of_w32 (void)
{
  HMODULE kernel32 = GetModuleHandle ("kernel32.dll");

  get_process_times_fn = (GetProcessTimes_Proc)
    GetProcAddress (kernel32, "GetProcessTimes");

  DEFSYM (QCloaded_from, ":loaded-from");

  Vlibrary_cache = Qnil;
  staticpro (&Vlibrary_cache);

  g_b_init_is_windows_9x = 0;
  g_b_init_open_process_token = 0;
  g_b_init_get_token_information = 0;
  g_b_init_lookup_account_sid = 0;
  g_b_init_get_sid_sub_authority = 0;
  g_b_init_get_sid_sub_authority_count = 0;
  g_b_init_get_file_security = 0;
  g_b_init_get_security_descriptor_owner = 0;
  g_b_init_get_security_descriptor_group = 0;
  g_b_init_is_valid_sid = 0;
  g_b_init_create_toolhelp32_snapshot = 0;
  g_b_init_process32_first = 0;
  g_b_init_process32_next = 0;
  g_b_init_open_thread_token = 0;
  g_b_init_impersonate_self = 0;
  g_b_init_revert_to_self = 0;
  g_b_init_get_process_memory_info = 0;
  g_b_init_get_process_working_set_size = 0;
  g_b_init_global_memory_status = 0;
  g_b_init_global_memory_status_ex = 0;
  g_b_init_equal_sid = 0;
  g_b_init_copy_sid = 0;
  g_b_init_get_length_sid = 0;
  g_b_init_get_native_system_info = 0;
  g_b_init_get_system_times = 0;
  num_of_processors = 0;
  /* The following sets a handler for shutdown notifications for
     console apps. This actually applies to Emacs in both console and
     GUI modes, since we had to fool windows into thinking emacs is a
     console application to get console mode to work.  */
  SetConsoleCtrlHandler (shutdown_handler, TRUE);

  /* "None" is the default group name on standalone workstations.  */
  strcpy (dflt_group_name, "None");
}

/* For make-serial-process  */
int
serial_open (char *port)
{
  HANDLE hnd;
  child_process *cp;
  int fd = -1;

  hnd = CreateFile (port, GENERIC_READ | GENERIC_WRITE, 0, 0,
		    OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
  if (hnd == INVALID_HANDLE_VALUE)
    error ("Could not open %s", port);
  fd = (int) _open_osfhandle ((int) hnd, 0);
  if (fd == -1)
    error ("Could not open %s", port);

  cp = new_child ();
  if (!cp)
    error ("Could not create child process");
  cp->fd = fd;
  cp->status = STATUS_READ_ACKNOWLEDGED;
  fd_info[ fd ].hnd = hnd;
  fd_info[ fd ].flags |=
    FILE_READ | FILE_WRITE | FILE_BINARY | FILE_SERIAL;
  if (fd_info[ fd ].cp != NULL)
    {
      error ("fd_info[fd = %d] is already in use", fd);
    }
  fd_info[ fd ].cp = cp;
  cp->ovl_read.hEvent = CreateEvent (NULL, TRUE, FALSE, NULL);
  if (cp->ovl_read.hEvent == NULL)
      error ("Could not create read event");
  cp->ovl_write.hEvent = CreateEvent (NULL, TRUE, FALSE, NULL);
  if (cp->ovl_write.hEvent == NULL)
      error ("Could not create write event");

  return fd;
}

/* For serial-process-configure  */
void
serial_configure (struct Lisp_Process *p, Lisp_Object contact)
{
  Lisp_Object childp2 = Qnil;
  Lisp_Object tem = Qnil;
  HANDLE hnd;
  DCB dcb;
  COMMTIMEOUTS ct;
  char summary[4] = "???"; /* This usually becomes "8N1".  */

  if ((fd_info[ p->outfd ].flags & FILE_SERIAL) == 0)
    error ("Not a serial process");
  hnd = fd_info[ p->outfd ].hnd;

  childp2 = Fcopy_sequence (p->childp);

  /* Initialize timeouts for blocking read and blocking write.  */
  if (!GetCommTimeouts (hnd, &ct))
    error ("GetCommTimeouts() failed");
  ct.ReadIntervalTimeout	 = 0;
  ct.ReadTotalTimeoutMultiplier	 = 0;
  ct.ReadTotalTimeoutConstant	 = 0;
  ct.WriteTotalTimeoutMultiplier = 0;
  ct.WriteTotalTimeoutConstant	 = 0;
  if (!SetCommTimeouts (hnd, &ct))
    error ("SetCommTimeouts() failed");
  /* Read port attributes and prepare default configuration.  */
  memset (&dcb, 0, sizeof (dcb));
  dcb.DCBlength = sizeof (DCB);
  if (!GetCommState (hnd, &dcb))
    error ("GetCommState() failed");
  dcb.fBinary	    = TRUE;
  dcb.fNull	    = FALSE;
  dcb.fAbortOnError = FALSE;
  /* dcb.XonLim and dcb.XoffLim are set by GetCommState() */
  dcb.ErrorChar	    = 0;
  dcb.EofChar	    = 0;
  dcb.EvtChar       = 0;

  /* Configure speed.  */
  if (!NILP (Fplist_member (contact, QCspeed)))
    tem = Fplist_get (contact, QCspeed);
  else
    tem = Fplist_get (p->childp, QCspeed);
  CHECK_NUMBER (tem);
  dcb.BaudRate = XINT (tem);
  childp2 = Fplist_put (childp2, QCspeed, tem);

  /* Configure bytesize.  */
  if (!NILP (Fplist_member (contact, QCbytesize)))
    tem = Fplist_get (contact, QCbytesize);
  else
    tem = Fplist_get (p->childp, QCbytesize);
  if (NILP (tem))
    tem = make_number (8);
  CHECK_NUMBER (tem);
  if (XINT (tem) != 7 && XINT (tem) != 8)
    error (":bytesize must be nil (8), 7, or 8");
  dcb.ByteSize = XINT (tem);
  summary[0] = XINT (tem) + '0';
  childp2 = Fplist_put (childp2, QCbytesize, tem);

  /* Configure parity.  */
  if (!NILP (Fplist_member (contact, QCparity)))
    tem = Fplist_get (contact, QCparity);
  else
    tem = Fplist_get (p->childp, QCparity);
  if (!NILP (tem) && !EQ (tem, Qeven) && !EQ (tem, Qodd))
    error (":parity must be nil (no parity), `even', or `odd'");
  dcb.fParity = FALSE;
  dcb.Parity = NOPARITY;
  dcb.fErrorChar = FALSE;
  if (NILP (tem))
    {
      summary[1] = 'N';
    }
  else if (EQ (tem, Qeven))
    {
      summary[1] = 'E';
      dcb.fParity = TRUE;
      dcb.Parity = EVENPARITY;
      dcb.fErrorChar = TRUE;
    }
  else if (EQ (tem, Qodd))
    {
      summary[1] = 'O';
      dcb.fParity = TRUE;
      dcb.Parity = ODDPARITY;
      dcb.fErrorChar = TRUE;
    }
  childp2 = Fplist_put (childp2, QCparity, tem);

  /* Configure stopbits.  */
  if (!NILP (Fplist_member (contact, QCstopbits)))
    tem = Fplist_get (contact, QCstopbits);
  else
    tem = Fplist_get (p->childp, QCstopbits);
  if (NILP (tem))
    tem = make_number (1);
  CHECK_NUMBER (tem);
  if (XINT (tem) != 1 && XINT (tem) != 2)
    error (":stopbits must be nil (1 stopbit), 1, or 2");
  summary[2] = XINT (tem) + '0';
  if (XINT (tem) == 1)
    dcb.StopBits = ONESTOPBIT;
  else if (XINT (tem) == 2)
    dcb.StopBits = TWOSTOPBITS;
  childp2 = Fplist_put (childp2, QCstopbits, tem);

  /* Configure flowcontrol.  */
  if (!NILP (Fplist_member (contact, QCflowcontrol)))
    tem = Fplist_get (contact, QCflowcontrol);
  else
    tem = Fplist_get (p->childp, QCflowcontrol);
  if (!NILP (tem) && !EQ (tem, Qhw) && !EQ (tem, Qsw))
    error (":flowcontrol must be nil (no flowcontrol), `hw', or `sw'");
  dcb.fOutxCtsFlow	= FALSE;
  dcb.fOutxDsrFlow	= FALSE;
  dcb.fDtrControl	= DTR_CONTROL_DISABLE;
  dcb.fDsrSensitivity	= FALSE;
  dcb.fTXContinueOnXoff	= FALSE;
  dcb.fOutX		= FALSE;
  dcb.fInX		= FALSE;
  dcb.fRtsControl	= RTS_CONTROL_DISABLE;
  dcb.XonChar		= 17; /* Control-Q  */
  dcb.XoffChar		= 19; /* Control-S  */
  if (NILP (tem))
    {
      /* Already configured.  */
    }
  else if (EQ (tem, Qhw))
    {
      dcb.fRtsControl = RTS_CONTROL_HANDSHAKE;
      dcb.fOutxCtsFlow = TRUE;
    }
  else if (EQ (tem, Qsw))
    {
      dcb.fOutX = TRUE;
      dcb.fInX = TRUE;
    }
  childp2 = Fplist_put (childp2, QCflowcontrol, tem);

  /* Activate configuration.  */
  if (!SetCommState (hnd, &dcb))
    error ("SetCommState() failed");

  childp2 = Fplist_put (childp2, QCsummary, build_string (summary));
  p->childp = childp2;
}

#ifdef HAVE_GNUTLS

ssize_t
emacs_gnutls_pull (gnutls_transport_ptr_t p, void* buf, size_t sz)
{
  int n, sc, err;
  SELECT_TYPE fdset;
  EMACS_TIME timeout;
  struct Lisp_Process *process = (struct Lisp_Process *)p;
  int fd = process->infd;

  for (;;)
    {
      n = sys_read (fd, (char*)buf, sz);

      if (n >= 0)
        return n;

      err = errno;

      if (err == EWOULDBLOCK)
        {
          /* Set a small timeout.  */
          EMACS_SET_SECS_USECS (timeout, 1, 0);
          FD_ZERO (&fdset);
          FD_SET ((int)fd, &fdset);

          /* Use select with the timeout to poll the selector.  */
          sc = select (fd + 1, &fdset, (SELECT_TYPE *)0, (SELECT_TYPE *)0,
                       &timeout);

          if (sc > 0)
            continue;  /* Try again.  */

          /* Translate the WSAEWOULDBLOCK alias EWOULDBLOCK to EAGAIN.
             Also accept select return 0 as an indicator to EAGAIN.  */
          if (sc == 0 || errno == EWOULDBLOCK)
            err = EAGAIN;
          else
            err = errno; /* Other errors are just passed on.  */
        }

      emacs_gnutls_transport_set_errno (process->gnutls_state, err);

      return -1;
    }
}

ssize_t
emacs_gnutls_push (gnutls_transport_ptr_t p, const void* buf, size_t sz)
{
  struct Lisp_Process *process = (struct Lisp_Process *)p;
  int fd = process->outfd;
  ssize_t n = sys_write (fd, buf, sz);

  /* 0 or more bytes written means everything went fine.  */
  if (n >= 0)
    return n;

  /* Negative bytes written means we got an error in errno.
     Translate the WSAEWOULDBLOCK alias EWOULDBLOCK to EAGAIN.  */
  emacs_gnutls_transport_set_errno (process->gnutls_state,
                                    errno == EWOULDBLOCK ? EAGAIN : errno);

  return -1;
}
#endif /* HAVE_GNUTLS */

/* end of w32.c */
