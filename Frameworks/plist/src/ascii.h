#ifndef ASCII_PARSER_H_D3YVICTX
#define ASCII_PARSER_H_D3YVICTX

#include "plist.h"

namespace plist
{
	plist::any_t parse_ascii (std::string const& str, bool* success = NULL);
}

#endif /* end of include guard: ASCII_PARSER_H_D3YVICTX */
