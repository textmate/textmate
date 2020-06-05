#ifndef FILE_STATUS_H_CIXQCEQO
#define FILE_STATUS_H_CIXQCEQO

enum file_status_t
{
	kFileTestWritable,
	kFileTestWritableByRoot,
	kFileTestNotWritable,
	kFileTestNotWritableButOwner,
	kFileTestNoParent,
	kFileTestReadOnly,
	kFileTestUnhandled,
};

inline std::string to_s (file_status_t status)
{
	switch(status)
	{
		case kFileTestWritable:             return "kFileTestWritable";
		case kFileTestWritableByRoot:       return "kFileTestWritableByRoot";
		case kFileTestNotWritable:          return "kFileTestNotWritable";
		case kFileTestNotWritableButOwner:  return "kFileTestNotWritableButOwner";
		case kFileTestReadOnly:             return "kFileTestReadOnly";
		case kFileTestNoParent:             return "kFileTestNoParent";
		case kFileTestUnhandled:            return "kFileTestUnhandled";
	}
	return NULL_STR;
}

namespace file
{
	file_status_t status (std::string const& path);

} /* file */

#endif /* end of include guard: FILE_STATUS_H_CIXQCEQO */
