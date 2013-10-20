#ifndef NG_WRITE_H_N8L9UVMF
#define NG_WRITE_H_N8L9UVMF

#include <buffer/buffer.h>
#include <selection/selection.h>
#include <command/parser.h>

namespace ng
{
	PUBLIC ng::range_t write_unit_to_fd (buffer_t const& buffer, range_t const& ranges, size_t tabSize, int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection);

} /* ng */

#endif /* end of include guard: NG_WRITE_H_N8L9UVMF */
