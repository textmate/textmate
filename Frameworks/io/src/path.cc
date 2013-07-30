#include "io.h"
#include "fsref.h"
#include "intermediate.h"
#include "entries.h"
#include <oak/debug.h>
#include <text/tokenize.h>
#include <text/format.h>
#include <regexp/regexp.h>
#include <cf/cf.h>

OAK_DEBUG_VAR(IO_Path);
OAK_DEBUG_VAR(IO_AuthIO);

namespace path
{
	// ==============================
	// = Simple String Manipulation =
	// ==============================

	static std::vector<std::string> split (std::string const& path)
	{
		std::vector<std::string> res;

		std::string::size_type from = 0;
		while(from < path.size() && from != std::string::npos)
		{
			std::string::size_type to = path.find('/', from);
			res.push_back(std::string(path.begin() + from, to == std::string::npos ? path.end() : path.begin() + to));
			from = to == std::string::npos ? to : to + 1;
		}

		return res;
	}

	static std::string join (std::vector<std::string> const& components)
	{
		if(components == std::vector<std::string>(1, ""))
			return "/";
		return text::join(components, "/");
	}

	static void remove_current_dir (std::string& path)
	{
		size_t src = 0, dst = 0;
		char prev = 0, pprev = 0;
		while(src < path.size())
		{
			if((src == 1 || pprev == '/') && prev == '.' && path[src] == '/')
			{
				--dst;
			}
			else if(!(src && prev == '/' && path[src] == '/'))
			{
				if(src != dst)
					path[dst] = path[src];
				++dst;
			}

			pprev = prev;
			prev = path[src];

			++src;
		}
		path.resize(dst > 1 && prev == '/' ? dst-1 : (pprev == '/' && prev == '.' ? dst-2 : dst));
	}

	static bool is_parent_meta_entry (char* first, char* last)
	{
		switch(last - first)
		{
			case 2: return strncmp(first, "..",  2) == 0;
			case 3: return strncmp(first, "/..", 3) == 0;
		}
		return false;
	}

	static void remove_parent_dir (std::string& path)
	{
		char* first = &path[0];
		char* last = first + path.size();
		if(first != last && *first == '/')
			++first;
		std::reverse(first, last);

		char* src = first;
		char* dst = first;

		size_t toSkip = 0;
		while(src != last)
		{
			char* from = src;
			while(src != last && (src == from || *src != '/'))
				++src;

			if(is_parent_meta_entry(from, src))
				++toSkip;
			else if(toSkip)
				--toSkip;
			else
				dst = std::copy(from, src, dst);
		}

		static char const parent_str[3] = { '/', '.', '.' };
		while(toSkip--)
			dst = std::copy(parent_str, parent_str + sizeof(parent_str), dst);

		std::reverse(first, dst);
		if(first != dst && dst[-1] == '/') // workaround for paths ending with ‘..’ e.g. ‘/path/to/foo/..’
			--dst;
		path.resize(dst - &path[0]);
	}

	std::string normalize (std::string path)
	{
		remove_current_dir(path);
		remove_parent_dir(path);
		return path;
	}

	std::string name (std::string const& p)
	{
		std::string const& path = normalize(p);
		std::string::size_type n = path.rfind('/');
		return n == std::string::npos ? path : path.substr(n+1);
	}

	std::string parent (std::string const& p)
	{
		return p == "/" || p == NULL_STR ? p : join(p, "..");
	}

	std::string strip_extension (std::string const& p)
	{
		std::string const& path = normalize(p);
		return path.substr(0, path.size() - extension(path).size());
	}

	std::string strip_extensions (std::string const& p)
	{
		std::string const& path = normalize(p);
		return path.substr(0, path.size() - extensions(path).size());
	}

	std::string extension (std::string const& p)
	{
		std::string const& path = name(normalize(p));
		std::string::size_type n = path.rfind('.');
		return n == std::string::npos ? "" : path.substr(n);
	}

	std::string extensions (std::string const& p)
	{
		std::string const& path = name(normalize(p));
		std::string::size_type n = path.rfind('.');
		if(n != std::string::npos && n > 0)
		{
			std::string::size_type m = path.rfind('.', n-1);
			if(m != std::string::npos && path.find_first_not_of("abcdefghijklmnopqrstuvwxyz", m+1) == n)
				n = m;
		}
		return n == std::string::npos ? "" : path.substr(n);
	}

	size_t rank (std::string const& path, std::string const& ext)
	{
		size_t rank = 0;
		if(path.rfind(ext) == path.size() - ext.size())
		{
			char ch = path[path.size() - ext.size() - 1];
			if(ch == '.' || ch == '/' || ch == '_')
				rank = std::max(path.size() - ext.size(), rank);
		}
		return rank;
	}

	std::string join (std::string const& base, std::string const& path)
	{
		return !path.empty() && path[0] == '/' ? normalize(path) : normalize(base + "/" + path);
	}

	std::string join (std::initializer_list<std::string> const& components)
	{
		return normalize(text::join(components, "/"));
	}

	bool is_absolute (std::string const& path)
	{
		if(!path.empty() && path[0] == '/')
		{
			std::string p = normalize(path);
			if(p != "/.." && p.find("/../") != 0)
				return true;
		}
		return false;
	}

	bool is_child (std::string const& nonNormalizedChild, std::string const& nonNormalizedParent)
	{
		std::string const child  = normalize(nonNormalizedChild);
		std::string const parent = normalize(nonNormalizedParent);
		return child.find(parent) == 0 && (parent.size() == child.size() || child[parent.size()] == '/');
	}

	std::string with_tilde (std::string const& p)
	{
		std::string const& base = home();
		std::string const& path = normalize(p) + (p.size() > 1 && p[p.size()-1] == '/' ? "/" : "");
		if(oak::has_prefix(path.begin(), path.end(), base.begin(), base.end()) && (path.size() == base.size() || path[base.size()] == '/'))
			return "~" + path.substr(base.size());
		return path;
	}

	std::string relative_to (std::string const& p, std::string const& b)
	{
		if(b.empty() || b == NULL_STR)
			return p;
		else if(p.empty() || p == NULL_STR)
			return b;

		ASSERTF(b[0] == '/', "‘%s’ - ‘%s’\n", p.c_str(), b.c_str());
		std::string const& path = normalize(p);
		std::string const& base = normalize(b);
		if(path[0] != '/')
			return path;

		std::vector<std::string> const& abs = split(base);
		std::vector<std::string> const& rel = split(path);

		size_t i = 0;
		while(i < abs.size() && i < rel.size() && abs[i] == rel[i])
			++i;

		if(i == 1) // only "/" in common, return absolute path
			return b == "/" ? path.substr(1) : path;

		std::vector<std::string> res;
		for(size_t j = abs.size(); j != i; --j)
			res.push_back("..");
		res.insert(res.end(), rel.begin() + i, rel.end());

		return join(res);
	}

	// ==============================
	// = Requires stat’ing and more =
	// ==============================

	static std::string resolve_alias (std::string const& path)
	{
		fsref_t ref(path);
		Boolean aliasFlag = FALSE, dummy;
		OSErr err = FSIsAliasFile(ref, &aliasFlag, &dummy);
		if(err == noErr && aliasFlag == TRUE)
		{
			OSErr err = FSResolveAliasFile(ref, TRUE, &dummy, &dummy);
			if(err == noErr)
				return ref.path();
		}
		return path;
	}

	static std::string resolve_links (std::string const& p, bool resolveParent, std::set<std::string>& seen)
	{
		if(p == "/" || !path::is_absolute(p))
			return p;

		if(seen.find(p) != seen.end())
			return p;
		seen.insert(p);

		std::string resolvedParent = resolveParent ? resolve_links(parent(p), resolveParent, seen) : parent(p);
		std::string path = path::join(resolvedParent, name(p));

		struct stat buf;
		if(lstat(path.c_str(), &buf) == 0)
		{
			if(S_ISLNK(buf.st_mode))
			{
				char buf[PATH_MAX];
				ssize_t len = readlink(path.c_str(), buf, sizeof(buf));
				if(0 < len && len < PATH_MAX)
				{
					path = resolve_links(join(resolvedParent, std::string(buf, buf + len)), resolveParent, seen);
				}
				else
				{
					std::string errStr = len == -1 ? strerror(errno) : text::format("Result outside allowed range %zd", len);
					fprintf(stderr, "*** readlink(‘%s’) failed: %s\n", path.c_str(), errStr.c_str());
				}
			}
			else if(S_ISREG(buf.st_mode))
			{
				path = resolve_alias(path);
			}
		}
		return path;
	}

	std::string resolve (std::string const& path)
	{
		std::set<std::string> seen;
		return resolve_links(normalize(path), true, seen);
	}

	std::string resolve_head (std::string const& path)
	{
		std::set<std::string> seen;
		return resolve_links(normalize(path), false, seen);
	}

	bool is_readable (std::string const& path)
	{
		return path != NULL_STR && access(path.c_str(), R_OK) == 0;
	}

	bool is_writable (std::string const& path)
	{
		return path != NULL_STR && access(path.c_str(), W_OK) == 0;
	}

	bool is_executable (std::string const& path)
	{
		return path != NULL_STR && access(path.c_str(), X_OK) == 0 && !path::is_directory(path);
	}

	bool exists (std::string const& path)
	{
		return path != NULL_STR && access(path.c_str(), F_OK) == 0;
	}

	bool is_directory (std::string const& path)
	{
		return path != NULL_STR && path::info(path::resolve_head(path)) & path::flag::directory;
	}

	bool is_local (std::string const& path)
	{
		CFURLRef url = CFURLCreateFromFileSystemRepresentation(kCFAllocatorDefault, (UInt8 const*)path.data(), path.size(), is_directory(path));
		if(!url) return false;
		CFBooleanRef pathIsLocal;
		bool ok = CFURLCopyResourcePropertyForKey(url, kCFURLVolumeIsLocalKey, &pathIsLocal, NULL);
		CFRelease(url);
		if(!ok) return false;
		return (pathIsLocal == kCFBooleanTrue);
	}

	bool is_trashed (std::string const& path)
	{
		Boolean res;
		return DetermineIfPathIsEnclosedByFolder(kOnAppropriateDisk, kTrashFolderType, (UInt8 const*)path.c_str(), false, &res) == noErr ? res : false;
	}

	static uint16_t finder_flags (std::string const& path)
	{
#if 01
		FSCatalogInfo catalogInfo;
		if(noErr == FSGetCatalogInfo(fsref_t(path), kFSCatInfoFinderInfo, &catalogInfo, NULL, NULL, NULL))
			return ((FileInfo*)&catalogInfo.finderInfo)->finderFlags;
#else
		struct { u_int32_t length; FileInfo fileInfo; ExtendedFileInfo extendedInfo; } attrBuf;
		if(getattrlist(path.c_str(), &(attrlist){ ATTR_BIT_MAP_COUNT, 0, ATTR_CMN_FNDRINFO, 0, 0, 0, 0 }, &attrBuf, sizeof(attrBuf), 0) == 0 && attrBuf.length == sizeof(attrBuf))
			return ntohs(attrBuf.fileInfo.finderFlags);
		else if(errno != ENOENT)
			perror(text::format("getattrlist(‘%s’)", path.c_str()).c_str());
#endif
		return 0;
	}

	size_t label_index (std::string const& path)
	{
		return (finder_flags(path) & kColor) >> 1;
	}

	bool set_label_index (std::string const& path, size_t labelIndex)
	{
		FSCatalogInfo catalogInfo;
		((FileInfo*)&catalogInfo.finderInfo)->finderFlags = (finder_flags(path) & ~kColor) | (labelIndex << 1);
		return noErr == FSSetCatalogInfo(fsref_t(path), kFSCatInfoFinderInfo, &catalogInfo);
	}

	// ========
	// = Info =
	// ========

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

	static bool hide_volume (dev_t device, std::string const& path = "")
	{
		if(path == "/dev")
			return true;

		struct statfs buf;
		if(statfs(path.c_str(), &buf) == 0)
			return path == buf.f_mntonname && buf.f_flags & MNT_DONTBROWSE;
		return false;
	}

	dev_t device (std::string const& path)
	{
		struct stat buf;
		if(stat(path.c_str(), &buf) == 0)
			return buf.st_dev;
		return -1;
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

		LSItemInfoRecord itemInfo;
		if(LSCopyItemInfoForRef(fsref_t(path), kLSRequestBasicFlagsOnly, &itemInfo) == noErr)
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

		if((mask & flag::stationery_pad) == flag::stationery_pad)
		{
			struct { u_int32_t length; FileInfo fileInfo; ExtendedFileInfo extendedInfo; } attrBuf;
			if(getattrlist(path.c_str(), &(attrlist){ ATTR_BIT_MAP_COUNT, 0, ATTR_CMN_FNDRINFO, 0, 0, 0, 0 }, &attrBuf, sizeof(attrBuf), 0) == 0 && attrBuf.length == sizeof(attrBuf))
			{
				if((ntohs(attrBuf.fileInfo.finderFlags) & kIsStationery) == kIsStationery)
					res |= flag::stationery_pad;
			}
		}

		return res;
	}

	std::string for_fd (int fd)
	{
		for(size_t i = 0; i < 100; ++i)
		{
			char buf[MAXPATHLEN];
			if(fcntl(fd, F_GETPATH, buf) == 0 && fcntl(fd, F_GETPATH, buf) == 0) // this seems to be enough to workaround <rdar://6149247>
			{
				if(access(buf, F_OK) == 0)
					return std::string(buf);
				fprintf(stderr, "F_GETPATH gave us %s, but that file does not exist (retry %zu)\n", buf, i);
				usleep(10);
			}
		}
		return NULL_STR;
	}

	// ================
	// = Helper stuff =
	// ================

	std::string system_display_name (std::string const& path)
	{
		CFStringRef displayName;
		if(path.find("/Volumes/") != 0 && path.find("/home/") != 0 && LSCopyDisplayNameForRef(fsref_t(path), &displayName) == noErr)
		{
			std::string const& res = cf::to_s(displayName);
			CFRelease(displayName);
			return res;
		}
		return name(path);
	}

	std::string display_name (std::string const& p, size_t n)
	{
		std::string const& path = normalize(p);
		std::string res(system_display_name(path));

		std::string::const_reverse_iterator const& last = path.rend();
		std::string::const_reverse_iterator const& to   = std::find(path.rbegin(), last, '/');

		if(n > 0 && to != last)
		{
			std::string::const_reverse_iterator from = to;
			std::string components;
			for(; n > 0 && from != last; --n)
			{
				if(components.size() > 0)
					components = "/" + components;
				components = system_display_name(std::string(path.begin(), from.base()-1)) + components;
				if(n > 0)
					from = std::find(++from, last, '/');
			}

			res += " — " + components;
		}

		return res;
	}

	static size_t count_slashes (std::string const& s1, std::string const& s2)
	{
		auto s1First = s1.rbegin(), s1Last = s1.rend();
		auto s2First = s2.rbegin(), s2Last = s2.rend();
		while(s1First != s1Last && s2First != s2Last)
		{
			if(*s1First != *s2First)
				break;
			++s1First, ++s2First;
		}
		return std::count(s1.rbegin(), s1First, '/');
	}

	std::vector<size_t> disambiguate (std::vector<std::string> const& paths)
	{
		std::vector<size_t> v(paths.size());
		std::iota(v.begin(), v.end(), 0);

		std::sort(v.begin(), v.end(), [&paths](size_t const& lhs, size_t const& rhs) -> bool {
			auto s1First = paths[lhs].rbegin(), s1Last = paths[lhs].rend();
			auto s2First = paths[rhs].rbegin(), s2Last = paths[rhs].rend();
			while(s1First != s1Last && s2First != s2Last)
			{
				if(*s1First < *s2First)
					return true;
				else if(*s1First != *s2First)
					return false;
				++s1First, ++s2First;
			}
			return s1First == s1Last && s2First != s2Last;
		});

		std::vector<size_t> levels(paths.size());
		for(size_t i = 0; i < v.size(); )
		{
			std::string const& current = paths[v[i]];
			size_t above = 0, below = 0;

			if(i != 0)
				above = count_slashes(current, paths[v[i-1]]);

			size_t j = i;
			while(j < v.size() && current == paths[v[j]])
				++j;
			if(j < v.size())
				below = count_slashes(current, paths[v[j]]);

			for(; i < j; ++i)
				levels[v[i]] = std::max(above, below);
		}

		return levels;
	}

	std::string unique (std::string const& requestedPath, std::string const& suffix)
	{
		if(!exists(requestedPath))
			return requestedPath;

		std::string dir  = parent(requestedPath);
		std::string base = name(strip_extension(requestedPath));
		std::string ext  = extension(requestedPath);

		if(regexp::match_t const& m = regexp::search(" \\d+$", base))
			base.erase(m.begin());
		if(suffix != "" && base.size() > suffix.size() && base.find(suffix) == base.size() - suffix.size())
			base.erase(base.size() - suffix.size());

		for(size_t i = 1; i < 500; ++i)
		{
			std::string const num = i > 1 ? text::format(" %zu", i) : "";
			std::string const path = path::join(dir, base + suffix + num + ext);
			if(!exists(path))
				return path;
		}
		return NULL_STR;
	}

	// ===========
	// = Actions =
	// ===========

	std::string content (std::string const& path)
	{
		int fd = open(path.c_str(), O_RDONLY|O_CLOEXEC);
		if(fd == -1)
			return NULL_STR;

		std::string res = "";
		char buf[8192];
		ssize_t len;
		fcntl(fd, F_NOCACHE, 1);
		while((len = read(fd, buf, sizeof(buf))) > 0)
			res.insert(res.end(), buf, buf + len);
		close(fd);

		return res;
	}

	bool set_content (std::string const& path, char const* first, char const* last)
	{
		intermediate_t dest(path);

		int fd = open(dest, O_CREAT|O_TRUNC|O_WRONLY|O_CLOEXEC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
		if(fd == -1)
			return false;

		int res DB_VAR = write(fd, first, last - first);
		ASSERT_EQ(res, last - first);
		int rc DB_VAR = close(fd);
		ASSERT_EQ(rc, 0);

		return dest.commit();
	}

	std::string get_attr (std::string const& p, std::string const& attr)
	{
		std::string const& path = resolve(p);

		ssize_t size = getxattr(path.c_str(), attr.c_str(), NULL, 0, 0, 0);
		if(size <= 0)
			return NULL_STR;

		char data[size];
		getxattr(path.c_str(), attr.c_str(), data, size, 0, 0);
		return std::string(data, data + size);
	}

	void set_attr (std::string const& path, std::string const& attr, std::string const& value)
	{
		if(value == NULL_STR)
				removexattr(resolve(path).c_str(), attr.c_str(), 0);
		else	setxattr(resolve(path).c_str(), attr.c_str(), value.data(), value.size(), 0, 0);
	}

	std::map<std::string, std::string> attributes (std::string const& path)
	{
		std::map<std::string, std::string> res;

		int fd = open(path.c_str(), O_RDONLY|O_CLOEXEC);
		if(fd != -1)
		{
			ssize_t listSize = flistxattr(fd, NULL, 0, 0);
			if(listSize > 0)
			{
				char mem[listSize];
				if(flistxattr(fd, mem, listSize, 0) == listSize)
				{
					size_t i = 0;
					while(i < listSize)
					{
						ssize_t size = fgetxattr(fd, mem + i, NULL, 0, 0, 0);
						if(size > 0)
						{
							char value[size];
							if(fgetxattr(fd, mem + i, value, size, 0, 0) == size)
								res.insert(std::make_pair(mem + i, std::string(value, value + size)));
							else
								perror(("fgetxattr(" + path + ", " + (mem+i) + ")").c_str());
						}
						else if(size == -1)
						{
							perror(("fgetxattr(" + path + ", " + (mem+i) + ")").c_str());
						}
						i += strlen(mem + i) + 1;
					}
				}
			}
			else if(listSize == -1)
			{
				perror(("flistxattr(" + path + ")").c_str());
			}
			close(fd);
		}
		else
		{
			perror(("open(" + path + ")").c_str());
		}
		return res;
	}

	bool set_attributes (std::string const& path, std::map<std::string, std::string> const& attributes)
	{
		bool res = false;
		if(attributes.empty())
			return true;

		int fd = open(path.c_str(), O_RDONLY|O_CLOEXEC);
		if(fd != -1)
		{
			res = true;
			iterate(pair, attributes)
			{
				int rc = 0;
				if(pair->second == NULL_STR)
						rc = fremovexattr(fd, pair->first.c_str(), 0);
				else	rc = fsetxattr(fd, pair->first.c_str(), pair->second.data(), pair->second.size(), 0, 0);

				if(rc != 0 && errno != ENOTSUP && errno != ENOATTR)
				{
					// We only log the error since:
					// fremovexattr() on AFP for non-existing attributes gives us EINVAL
					// fsetxattr() on Samba saving to ext4 via virtual machine gives us ENOENT
					// sshfs with ‘-o noappledouble’ will return ENOATTR or EPERM
					perror((pair->second == NULL_STR ? text::format("fremovexattr(%d, \"%s\")", fd, pair->first.c_str()) : text::format("fsetxattr(%d, %s, \"%s\")", fd, pair->first.c_str(), pair->second.c_str())).c_str());
				}
			}
			close(fd);
		}
		else
		{
			perror("open");
		}
		return res;
	}

	bool link (std::string const& from, std::string const& to)
	{
		return symlink(from.c_str(), to.c_str()) == 0;
	}

	bool rename (std::string const& from, std::string const& to, bool replace)
	{
		if(replace || !exists(to))
			return move(from, to, replace);
		errno = EEXIST;
		return false;
	}

	std::string move_to_trash (std::string const& path)
	{
		fsref_t result;
		if(noErr != FSMoveObjectToTrashSync(fsref_t(path), result, 0))
			return NULL_STR;
		return result.path();
	}

	std::string duplicate (std::string const& src, std::string dst, bool overwrite)
	{
		if(dst == NULL_STR)
		{
			ASSERT(overwrite == false);
			dst = unique(src, " copy");
			if(dst == NULL_STR)
			{
				errno = ENOSPC;
				return NULL_STR;
			}
		}

		if(copy(src, dst))
			return dst;

		return NULL_STR;
	}

	bool make_dir (std::string const& path)
	{
		D(DBF_IO_Path, bug("%s\n", path.c_str()););
		if(path != NULL_STR && !exists(path))
		{
			make_dir(parent(path));
			mkdir(path.c_str(), S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IWGRP|S_IXGRP|S_IROTH|S_IWOTH|S_IXOTH);
		}
		return exists(path) && info(resolve(path)) & flag::directory;
	}

	void touch_tree (std::string const& basePath)
	{
		lutimes(basePath.c_str(), NULL);

		citerate(entry, path::entries(basePath))
		{
			std::string path = path::join(basePath, (*entry)->d_name);
			int type = (*entry)->d_type;
			if(type == DT_DIR)
				touch_tree(path);
			if(type == DT_LNK || type == DT_REG)
				lutimes(path.c_str(), NULL);
		}
	}

	// ===============
	// = Global Info =
	// ===============

	std::vector<std::string> volumes ()
	{
		std::vector<std::string> res;

		struct statfs* mnts;
		int mnt_count = getmntinfo(&mnts, MNT_WAIT); // getfsstat
		for(int i = 0; i < mnt_count; ++i)
		{
			// We explicitly ignore /dev since it does not have the proper flag set <rdar://5923503> — fixed in 10.6
			char const* path = mnts[i].f_mntonname;
			if(mnts[i].f_flags & MNT_DONTBROWSE || strcmp(path, "/dev") == 0)
				continue;
			res.push_back(path);
		}

		return res;
	}

	std::string cwd ()
	{
		std::string res = NULL_STR;
		if(char* cwd = getcwd(NULL, (size_t)-1))
		{
			res = cwd;
			free(cwd);
		}
		return res;
	}

	passwd* passwd_entry ()
	{
		passwd* entry = getpwuid(getuid());
		while(!entry || !entry->pw_dir || access(entry->pw_dir, R_OK) != 0) // Home folder might be missing <rdar://10261043>
		{
			char* errStr = strerror(errno);
			std::string message = text::format("Unable to obtain basic system information such as your home folder.\n\ngetpwuid(%d): %s", getuid(), errStr);

			CFOptionFlags responseFlags;
			CFUserNotificationDisplayAlert(0 /* timeout */, kCFUserNotificationStopAlertLevel, NULL /* iconURL */, NULL /* soundURL */, NULL /* localizationURL */, CFSTR("Missing User Database"), cf::wrap(message), CFSTR("Retry"), CFSTR("Show Radar Entry"), nil /* otherButtonTitle */, &responseFlags);

			if((responseFlags & 0x3) == kCFUserNotificationDefaultResponse)
			{
				entry = getpwuid(getuid());
			}
			else if((responseFlags & 0x3) == kCFUserNotificationAlternateResponse)
			{
				CFURLRef url = CFURLCreateWithString(kCFAllocatorDefault, cf::wrap("http://openradar.appspot.com/10261043"), NULL);
				CFMutableArrayRef urls = CFArrayCreateMutable(kCFAllocatorDefault, 0, &kCFTypeArrayCallBacks);
				CFArrayAppendValue(urls, url);
				LSOpenURLsWithRole(urls, kLSRolesViewer, NULL, NULL, NULL, 0);
				CFRelease(urls);
				CFRelease(url);
			}
		}
		return entry;
	}

	std::string home ()
	{
		return passwd_entry()->pw_dir;
	}

	std::string trash (std::string const& forPath)
	{
		FSRef res;
		FSCatalogInfo info;
		FSGetCatalogInfo(fsref_t(forPath), kFSCatInfoVolume, &info, 0, 0, 0); 
		return FSFindFolder(info.volume, kTrashFolderType, false, &res) == noErr ? to_s(res) : NULL_STR;;
	}

	static std::string temp_file_in_directory (std::string const& path, std::string const& file)
	{
		if(file == NULL_STR)
			return path;

		std::string res = path::join(path, std::string(getprogname() ?: "untitled") + "_" + file + ".XXXXXX");
		res.c_str(); // ensure the buffer is zero terminated, should probably move to a better approach
		mktemp(&res[0]);

		D(DBF_IO_Path, bug("%s\n", res.c_str()););
		return res;
	}

	std::string temp (std::string const& file)
	{
		std::string str(128, ' ');
		size_t len = confstr(_CS_DARWIN_USER_TEMP_DIR, &str[0], str.size());
		if(0 < len && len < 128) // if length is 128 the path was truncated and unusable
				str.resize(len - 1);
		else	str = getenv("TMPDIR") ?: "/tmp";

		return temp_file_in_directory(str, file);
	}

	std::string cache (std::string const& file)
	{
		std::string str(128, ' ');
		size_t len = confstr(_CS_DARWIN_USER_CACHE_DIR, &str[0], str.size());
		if(0 < len && len < 128) // if length is 128 the path was truncated and unusable
				str.resize(len - 1);
		else	str = path::temp();

		return temp_file_in_directory(str, file);
	}

	std::string desktop ()
	{
		return home() + "/Desktop";
	}

} /* path */ 
