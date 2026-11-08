#ifndef PLIST_H_34L7NUFO
#define PLIST_H_34L7NUFO

#include "date.h"
#include "uuid.h"
#include <text/format.h>
#include <oak/debug.h>
#include <variant>

namespace plist
{
	struct any_t;
	typedef std::map<std::string, any_t> dictionary_t;
	typedef std::vector<any_t> array_t;

	struct any_t
	{
		using variant_type = std::variant<
			std::monostate,
			bool, int32_t, uint64_t, std::string, std::vector<char>, oak::date_t,
			array_t, dictionary_t
		>;

		variant_type data;

		any_t () : data(std::monostate{}) { }
		any_t (bool v) : data(v) { }
		any_t (int32_t v) : data(v) { }
		any_t (uint64_t v) : data(v) { }
		any_t (std::string v) : data(std::move(v)) { }
		any_t (char const* v) : data(std::string(v)) { }
		any_t (std::vector<char> v) : data(std::move(v)) { }
		any_t (oak::date_t v) : data(std::move(v)) { }
		any_t (array_t v) : data(std::move(v)) { }
		any_t (dictionary_t v) : data(std::move(v)) { }

		any_t (any_t const&) = default;
		any_t (any_t&&) = default;
		any_t& operator= (any_t const&) = default;
		any_t& operator= (any_t&&) = default;

		any_t& operator= (bool v)               { data = v; return *this; }
		any_t& operator= (int32_t v)             { data = v; return *this; }
		any_t& operator= (uint64_t v)            { data = v; return *this; }
		any_t& operator= (std::string v)         { data = std::move(v); return *this; }
		any_t& operator= (char const* v)         { data = std::string(v); return *this; }
		any_t& operator= (std::vector<char> v)   { data = std::move(v); return *this; }
		any_t& operator= (oak::date_t v)         { data = std::move(v); return *this; }
		any_t& operator= (array_t v)             { data = std::move(v); return *this; }
		any_t& operator= (dictionary_t v)        { data = std::move(v); return *this; }

		bool empty () const { return std::holds_alternative<std::monostate>(data); }

		bool operator== (any_t const& rhs) const { return data == rhs.data; }
		bool operator!= (any_t const& rhs) const { return data != rhs.data; }
		bool operator< (any_t const& rhs) const  { return data < rhs.data; }
	};

	// Drop-in replacements for boost::get
	template <typename T> T& get (any_t& v)             { return std::get<T>(v.data); }
	template <typename T> T const& get (any_t const& v)  { return std::get<T>(v.data); }
	template <typename T> T* get (any_t* v)              { return v ? std::get_if<T>(&v->data) : nullptr; }
	template <typename T> T const* get (any_t const* v)  { return v ? std::get_if<T>(&v->data) : nullptr; }

	enum plist_format_t { kPlistFormatBinary, kPlistFormatXML };

	dictionary_t load (std::string const& path);
	bool save (std::string const& path, any_t const& plist, plist_format_t format = kPlistFormatBinary);
	any_t parse (std::string const& str);
	dictionary_t convert (CFPropertyListRef plist);
	CFPropertyListRef create_cf_property_list (any_t const& plist);
	bool equal (any_t const& lhs, any_t const& rhs);

	bool is_true (any_t const& item);

	template <typename T> bool get_key_path (any_t const& plist, std::string const& keyPath, T& ref);
	template <typename T> T convert (plist::any_t const& from);

	// to_s flags
	enum { kStandard = 0, kPreferSingleQuotedStrings = 1, kSingleLine = 2 };

	std::string to_s (any_t const& plist, int flags = kStandard, std::vector<std::string> const& keySortOrder = std::vector<std::string>());

} /* plist */

#endif /* end of include guard: PLIST_H_34L7NUFO */
