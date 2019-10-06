#include "intermediate.h"
#include "path.h"
#include <ns/ns.h>
#include <text/format.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(IO_Intermediate);
OAK_DEBUG_VAR(IO_Swap_File_Data);

__attribute__ ((format (printf, 1, 2))) static std::string format_error (char const* format, ...)
{
	char* err = strerror(errno);
	char* msg = nullptr;

	va_list ap;
	va_start(ap, format);
	vasprintf(&msg, format, ap);
	va_end(ap);

	char* res = nullptr;
	asprintf(&res, "%s: %s\n", msg, err);
	std::string str(res);
	free(res);
	free(msg);
	return str;
}

static bool swap_and_unlink (std::string const& src, std::string const& dst, std::string& errorMsg)
{
	D(DBF_IO_Swap_File_Data, bug("%s → %s\n", src.c_str(), dst.c_str()););
	ASSERT_EQ(access(src.c_str(), F_OK), 0);
	if(access(dst.c_str(), F_OK) != 0 && !path::make_dir(path::parent(dst)))
	{
		errorMsg = format_error("mkdir_p(\"%s\")", path::parent(dst).c_str());
		return false;
	}

	if(exchangedata(src.c_str(), dst.c_str(), 0) == 0)
	{
		bool res = unlink(src.c_str()) == 0;
		if(!res)
			errorMsg = format_error("unlink(\"%s\")", src.c_str());
		return res;
	}

	if(errno != ENOTSUP && errno != ENOENT && errno != EXDEV)
	{
		// ExpanDrive returns EFAULT
		perrorf("exchangedata(\"%s\", \"%s\")", src.c_str(), dst.c_str());
		errno = ENOTSUP;
	}

	D(DBF_IO_Swap_File_Data, bug("exchangedata() failed: %s\n", strerror(errno)););
	if(errno == ENOTSUP || errno == ENOENT)
	{
		if(errno == ENOTSUP && access(dst.c_str(), F_OK) == 0)
		{
			// Skip COPYFILE_METADATA for network drives <rdar://17480649>
			if(path::is_local(src))
			{
				copyfile(dst.c_str(), src.c_str(), nullptr, COPYFILE_METADATA);
				utimes(src.c_str(), nullptr);
			}
			else
			{
				struct stat sbuf;
				if(stat(dst.c_str(), &sbuf) == 0)
					chmod(src.c_str(), sbuf.st_mode & (S_IRWXU|S_IRWXG|S_IRWXO));
			}
		}

		if(::rename(src.c_str(), dst.c_str()) == 0)
			return true;
		errorMsg = format_error("rename(\"%s\", \"%s\")", src.c_str(), dst.c_str());
		D(DBF_IO_Swap_File_Data, bug("rename() failed: %s\n", strerror(errno)););
	}

	if(errno == EXDEV)
	{
		// TODO this should copy to dst under a new name, then re-run swap_files
		if(copyfile(src.c_str(), dst.c_str(), nullptr, COPYFILE_DATA|COPYFILE_MOVE) == 0)
		{
			bool res = unlink(src.c_str()) == 0;
			if(!res)
				errorMsg = format_error("unlink(\"%s\")", src.c_str());
			return res;
		}
		errorMsg = format_error("copyfile(\"%s\", \"%s\", nullptr, COPYFILE_DATA|COPYFILE_MOVE)", src.c_str(), dst.c_str());
		D(DBF_IO_Swap_File_Data, bug("copyfile() failed: %s\n", strerror(errno)););
	}

	return false;
}

static std::string create_path (std::string const& path)
{
	if(!path::exists(path))
		return path::make_dir(path::parent(path)), path;
	else if(path::device(path) != path::device(path::temp()) && access(path::parent(path).c_str(), W_OK) == 0)
		return path + "~";
	return path::temp("atomic_save");
}

namespace path
{
	struct filemanager_strategy_t : intermediate_t::strategy_t
	{
		filemanager_strategy_t (NSURL* destURL)
		{
			_destURL = destURL;
		}

		~filemanager_strategy_t ()
		{
			NSError* error;
			if(_tempDirectoryURL && ![NSFileManager.defaultManager removeItemAtURL:_tempDirectoryURL error:&error])
				os_log_error(OS_LOG_DEFAULT, "failed removing %{public}@: %{public}@\n", _tempDirectoryURL, error);
		}

		char const* setup (std::string* errorMsg)
		{
			NSError* error;
			if(_tempDirectoryURL = [NSFileManager.defaultManager URLForDirectory:NSItemReplacementDirectory inDomain:NSUserDomainMask appropriateForURL:_destURL create:YES error:&error])
			{
				_tempURL = [_tempDirectoryURL URLByAppendingPathComponent:_destURL.lastPathComponent];
				return _tempURL.fileSystemRepresentation;
			}

			NSString* displayName;
			[_destURL getResourceValue:&displayName forKey:NSURLLocalizedNameKey error:nil];
			*errorMsg = to_s([NSString stringWithFormat:@"Failed to obtain replacement directory for %@: %@", displayName ?: _destURL.path, error.localizedDescription]);

			os_log_error(OS_LOG_DEFAULT, "failed to obtain NSItemReplacementDirectory for %{public}@: %{public}@\n", _destURL, error);
			return nullptr;
		}

		bool commit (std::string* errorMsg)
		{
			struct stat sbuf;
			if(stat(_destURL.fileSystemRepresentation, &sbuf) == 0)
			{
				// Skip copyfile() for network drives <rdar://17480649>
				if(path::is_local(_destURL.fileSystemRepresentation))
					copyfile(_destURL.fileSystemRepresentation, _tempURL.fileSystemRepresentation, nullptr, COPYFILE_XATTR|COPYFILE_ACL);
				chmod(_tempURL.fileSystemRepresentation, sbuf.st_mode & (S_IRWXU|S_IRWXG|S_IRWXO));
			}

			NSError* error;
			if([NSFileManager.defaultManager replaceItemAtURL:_destURL withItemAtURL:_tempURL backupItemName:nil options:NSFileManagerItemReplacementUsingNewMetadataOnly resultingItemURL:nil error:&error])
				return true;

			NSString* displayName;
			[_destURL getResourceValue:&displayName forKey:NSURLLocalizedNameKey error:nil];
			*errorMsg = to_s([NSString stringWithFormat:@"Failed replacing %@ with %@: %@", displayName ?: _destURL.path, _tempURL.path, error.localizedDescription]);

			os_log_error(OS_LOG_DEFAULT, "failed replacing %{public}@ with %{public}@: %{public}@\n", _destURL, _tempURL, error);
			return false;
		}

	private:
		NSURL* _destURL;
		NSURL* _tempDirectoryURL;
		NSURL* _tempURL;
	};

	struct atomic_strategy_t : intermediate_t::strategy_t
	{
		atomic_strategy_t (std::string const& dest) : _resolved(path::resolve_head(dest)), _intermediate(create_path(_resolved))
		{
			D(DBF_IO_Intermediate, bug("%s → %s → %s\n", dest.c_str(), _resolved.c_str(), _intermediate.c_str()););
		}

		char const* setup (std::string* errorMsg)
		{
			return _intermediate.c_str();
		}

		bool commit (std::string* errorMsg)
		{
			D(DBF_IO_Intermediate, bug("%s ⇔ %s (swap: %s)\n", _resolved.c_str(), _intermediate.c_str(), BSTR(_intermediate != _resolved)););
			return _intermediate == _resolved ? true : swap_and_unlink(_intermediate, _resolved, *errorMsg);
		}

	private:
		std::string _resolved;
		std::string _intermediate;
	};

	struct non_atomic_strategy_t : intermediate_t::strategy_t
	{
		non_atomic_strategy_t (std::string const& dest) : _path(dest)
		{
		}

		char const* setup (std::string* errorMsg)
		{
			return _path.c_str();
		}

		bool commit (std::string* errorMsg)
		{
			return true;
		}

	private:
		std::string _path;
	};

	intermediate_t::intermediate_t (std::string const& originalDest, atomic_t atomicSave, mode_t mode) : _mode(mode)
	{
		std::string const dest = path::resolve_head(originalDest);

		if(atomicSave == atomic_t::legacy)
		{
			_strategy.reset(new atomic_strategy_t(dest));
		}
		else if(path::exists(dest))
 		{
			struct stat buf;
			if(stat(dest.c_str(), &buf) == 0)
				_mode = buf.st_mode;

			NSURL* destURL = [NSURL fileURLWithPath:to_ns(dest) isDirectory:NO];

			NSError* error;
			NSNumber* boolean;

			BOOL isInternalVolume = [destURL getResourceValue:&boolean forKey:NSURLVolumeIsInternalKey error:&error] && boolean.boolValue;
			BOOL isLocalVolume    = [destURL getResourceValue:&boolean forKey:NSURLVolumeIsLocalKey error:&error] && boolean.boolValue;

			if(atomicSave == atomic_t::always || atomicSave == atomic_t::external_volumes && !isInternalVolume || atomicSave == atomic_t::remote_volumes && !isLocalVolume)
					_strategy.reset(new filemanager_strategy_t(destURL));
			else	_strategy.reset(new non_atomic_strategy_t(dest));
		}
		else
		{
			_strategy.reset(new non_atomic_strategy_t(dest));
		}
	}

	intermediate_t::~intermediate_t ()
	{
		if(_fileDescriptor != -1)
			::close(_fileDescriptor);
	}

	int intermediate_t::open (std::string* errorMsg, int oflag)
	{
		std::string ignoredErrorMsg;
		if(char const* path = _strategy->setup(errorMsg ?: &ignoredErrorMsg))
		{
			_fileDescriptor = ::open(path, oflag, _mode);
			if(_fileDescriptor == -1 && errorMsg)
				*errorMsg = text::format("open(\"%s\"): %s", path, strerror(errno));
		}
		return _fileDescriptor;
	}

	bool intermediate_t::close (std::string* errorMsg)
	{
		int res = ::close(_fileDescriptor);
		if(res == -1 && errorMsg)
			*errorMsg = text::format("close(\"%d\"): %s", _fileDescriptor, strerror(errno));

		_fileDescriptor = -1;
		std::string ignoredErrorMsg;
		return res != -1 && _strategy->commit(errorMsg ?: &ignoredErrorMsg);
	}

} /* path */
