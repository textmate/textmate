#ifndef FILE_TYPE_H_2Z10CFJ1
#define FILE_TYPE_H_2Z10CFJ1

#include "bytes.h"
#include <oak/misc.h>

namespace file
{
	PUBLIC std::string type (std::string const& path, io::bytes_ptr const& bytes, std::string const& virtualPath = NULL_STR);
	PUBLIC void set_type (std::string const& path, std::string const& fileType);

} /* file */

#endif /* end of include guard: FILE_TYPE_H_2Z10CFJ1 */
