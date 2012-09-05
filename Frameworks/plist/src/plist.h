#ifndef PLIST_H_34L7NUFO
#define PLIST_H_34L7NUFO

#include "date.h"
#include "uuid.h"
#include <oak/misc.h>
#include <text/format.h>
#include <oak/debug.h>

namespace plist
{
	typedef boost::make_recursive_variant<
		bool, int32_t, uint64_t, std::string, std::vector<char>, oak::date_t,
		std::vector<boost::recursive_variant_>,
		std::map<std::string, boost::recursive_variant_>
	>::type any_t;

	typedef std::map<std::string, any_t> dictionary_t;
	typedef std::vector<any_t> array_t;

	enum plist_format_t { kPlistFormatBinary, kPlistFormatXML };

	PUBLIC dictionary_t load (std::string const& path);
	PUBLIC bool save (std::string const& path, any_t const& plist, plist_format_t format = kPlistFormatBinary);
	PUBLIC any_t parse (std::string const& str);
	PUBLIC dictionary_t convert (CFPropertyListRef plist);
	PUBLIC CFPropertyListRef create_cf_property_list (any_t const& plist);
	PUBLIC bool equal (any_t const& lhs, any_t const& rhs);

	PUBLIC bool is_true (any_t const& item);

	template <typename T> PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, T& ref);
	template <typename T> PUBLIC T get (plist::any_t const& from);

	// to_s flags
	enum { kStandard = 0, kPreferSingleQuotedStrings = 1 };

} /* plist */ 

namespace boost // we place this in the boost namespace to support ADL
{
	PUBLIC std::string to_s (plist::any_t const& plist, int flags = plist::kStandard, std::vector<std::string> const& keySortOrder = std::vector<std::string>());
}

#endif /* end of include guard: PLIST_H_34L7NUFO */
