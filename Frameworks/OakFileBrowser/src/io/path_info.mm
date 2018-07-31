#include "path_info.h"
#include <io/path.h>

static bool hide_volume (dev_t device, std::string const& path = "")
{
	if(path == "/dev")
		return true;

	struct statfs buf;
	if(statfs(path.c_str(), &buf) == 0)
		return path == buf.f_mntonname && buf.f_flags & MNT_DONTBROWSE;
	return false;
}

namespace path
{
	namespace flag
	{
		uint32_t
			meta_self        = (1 <<  0),
			meta_parent      = (1 <<  1),

			file_bsd         = (1 <<  2),
			file_finder      = (1 <<  3),
			directory_bsd    = (1 <<  4),
			directory_finder = (1 <<  5),
			symlink_bsd      = (1 <<  6),
			symlink_finder   = (1 <<  7),
			socket_bsd       = (1 <<  8),

			hidden_bsd       = (1 <<  9),
			hidden_finder    = (1 << 10), /* this is (hidden_bsd|hidden_dotfile) */
			hidden_dotfile   = (1 << 11),
			hidden_volume    = (1 << 12),

			volume_bsd       = (1 << 13),
			volume_finder    = (1 << 14),

			alias            = (1 << 15),
			package          = (1 << 16),
			application      = (1 << 17),
			stationery_pad   = (1 << 18),
			hidden_extension = (1 << 19),

			meta             = (meta_self|meta_parent),
			file             = file_bsd,
			directory        = directory_bsd,
			symlink          = symlink_bsd,
			dotfile          = hidden_dotfile,
			hidden           = (meta|hidden_bsd|hidden_volume);
	}

	uint32_t info (std::string const& path, uint32_t mask)
	{
		uint32_t res = 0;
		if(path == NULL_STR)
			return res;

		std::string const& name = path::name(path);
		if(name == ".")
			res |= flag::meta_self;
		else if(name == "..")
			res |= flag::meta_parent;
		else if(!name.empty() && name[0] == '.')
			res |= flag::hidden_dotfile;

		if(res & flag::meta)
			return res;

		struct stat buf;
		if(lstat(path.c_str(), &buf) == 0)
		{
			if(S_ISREG(buf.st_mode))
				res |= flag::file_bsd;
			if(S_ISDIR(buf.st_mode))
				res |= flag::directory_bsd;
			if(S_ISLNK(buf.st_mode))
				res |= flag::symlink_bsd;
			if(S_ISFIFO(buf.st_mode))
				res |= flag::socket_bsd;
			if(buf.st_flags & UF_HIDDEN)
				res |= flag::hidden_bsd;

			if((res & flag::directory_bsd) && hide_volume(buf.st_dev, path))
				res |= flag::hidden_volume;
		}

		if(CFURLRef url = CFURLCreateFromFileSystemRepresentation(kCFAllocatorDefault, (UInt8 const*)path.data(), path.size(), false))
		{
			LSItemInfoRecord itemInfo;
			if(LSCopyItemInfoForURL(url, kLSRequestBasicFlagsOnly, &itemInfo) == noErr)
			{
				OptionBits flags = itemInfo.flags;

				if(flags & kLSItemInfoIsInvisible)
					res |= flag::hidden_finder;
				if(flags & kLSItemInfoIsVolume)
					res |= flag::volume_finder;
				if(flags & kLSItemInfoExtensionIsHidden)
					res |= flag::hidden_extension;

				if(flags & kLSItemInfoIsSymlink)
					res |= flag::symlink_finder;

				if(!(res & (flag::symlink_bsd|flag::symlink_finder)))
				{
					if(flags & kLSItemInfoIsAliasFile) // this is true also for symbolic links
						res |= flag::alias;
				}

				if(flags & kLSItemInfoIsPlainFile)
					res |= flag::file_finder;
				if(flags & kLSItemInfoIsContainer)
					res |= flag::directory_finder;
				if(flags & kLSItemInfoIsPackage)
					res |= flag::package;
				if(flags & kLSItemInfoIsApplication)
					res |= flag::application;
			}
			CFRelease(url);
		}

		if((mask & flag::stationery_pad) == flag::stationery_pad)
		{
			struct { u_int32_t length; FileInfo fileInfo; ExtendedFileInfo extendedInfo; } attrBuf;
			attrlist list = { ATTR_BIT_MAP_COUNT, 0, ATTR_CMN_FNDRINFO, 0, 0, 0, 0 };
			if(getattrlist(path.c_str(), &list, &attrBuf, sizeof(attrBuf), 0) == 0 && attrBuf.length == sizeof(attrBuf))
			{
				if((ntohs(attrBuf.fileInfo.finderFlags) & kIsStationery) == kIsStationery)
					res |= flag::stationery_pad;
			}
		}

		return res;
	}

} /* path */
