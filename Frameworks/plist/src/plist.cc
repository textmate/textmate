#include "plist.h"
#include <io/intermediate.h>
#include <oak/oak.h>
#include <cf/cf.h>

namespace plist
{
	// ==============
	// = Converting =
	// ==============

	static void convert_boolean (CFBooleanRef flag, any_t& res)
	{
		res = bool(CFBooleanGetValue(flag));
	}

	static void convert_number (CFNumberRef num, any_t& res)
	{
		int64_t val = 0;
		CFNumberGetValue(num, kCFNumberSInt64Type, &val);
		if(oak::cap<int64_t>(INT32_MIN, val, INT32_MAX) == val)
				res = int32_t(val);
		else	res = uint64_t(val);
	}

	static void convert_string (CFStringRef str, any_t& res)
	{
		res = cf::to_s(str);
	}

	static void convert_url (CFURLRef url, any_t& res)
	{
		res = cf::to_s(CFURLGetString(url));
	}

	static void convert_data (CFDataRef data, any_t& res)
	{
		UInt8 const* bytes = CFDataGetBytePtr(data);
		CFIndex len = CFDataGetLength(data);
		res = std::vector<char>(bytes, bytes + len);
	}

	static void convert_date (CFDateRef date, any_t& res)
	{
		res = oak::date_t(CFDateGetAbsoluteTime(date));
	}

	static void convert_any (CFPropertyListRef plist, any_t& res);

	static void convert_array (CFArrayRef array, any_t& res)
	{
		std::vector<any_t>& ref = boost::get< std::vector<any_t> >(res = std::vector<any_t>());
		for(CFIndex i = 0; i < CFArrayGetCount(array); ++i)
		{
			ref.emplace_back();
			convert_any(CFArrayGetValueAtIndex(array, i), ref.back());
		}
	}

	static void convert_dictionary (CFDictionaryRef dict, any_t& res)
	{
		std::map<std::string, any_t>& ref = boost::get< std::map<std::string, any_t> >(res = std::map<std::string, any_t>());
		
		CFIndex len = CFDictionaryGetCount(dict);
		CFPropertyListRef keys[len];
		CFPropertyListRef values[len];
		CFDictionaryGetKeysAndValues(dict, keys, values);
		for(CFIndex i = 0; i < len; ++i)
		{
			std::map<std::string, any_t>::iterator it = ref.emplace(cf::to_s((CFStringRef)keys[i]), any_t()).first;
			convert_any(values[i], it->second);
		}
	}

	static void convert_any (CFPropertyListRef plist, any_t& res)
	{
		if(CFGetTypeID(plist) == CFNumberGetTypeID())
			return convert_number((CFNumberRef)plist, res);
		else if(CFGetTypeID(plist) == CFBooleanGetTypeID())
			return convert_boolean((CFBooleanRef)plist, res);
		else if(CFGetTypeID(plist) == CFStringGetTypeID())
			return convert_string((CFStringRef)plist, res);
		else if(CFGetTypeID(plist) == CFURLGetTypeID())
			return convert_url((CFURLRef)plist, res);
		else if(CFGetTypeID(plist) == CFDataGetTypeID())
			return convert_data((CFDataRef)plist, res);
		else if(CFGetTypeID(plist) == CFDateGetTypeID())
			return convert_date((CFDateRef)plist, res);
		else if(CFGetTypeID(plist) == CFArrayGetTypeID())
			return convert_array((CFArrayRef)plist, res);
		else if(CFGetTypeID(plist) == CFDictionaryGetTypeID())
			return convert_dictionary((CFDictionaryRef)plist, res);
	}
	
	dictionary_t convert (CFPropertyListRef plist)
	{
		if(plist && CFGetTypeID(plist) == CFDictionaryGetTypeID())
		{
			any_t res;
			convert_dictionary((CFDictionaryRef)plist, res);
			return boost::get<dictionary_t>(res);
		}
		return dictionary_t();
	}
	
	// ===========
	// = Loading =
	// ===========
	
	dictionary_t load (std::string const& path)
	{
		dictionary_t res;
		if(FILE* fp = fopen(path.c_str(), "r"))
		{
			std::vector<UInt8> v;
			UInt8 tmp[4096];
			while(size_t len = fread(tmp, 1, sizeof(tmp), fp))
				v.insert(v.end(), tmp, tmp + len);
			fclose(fp);

			CFDataRef data = CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, &v[0], v.size(), kCFAllocatorNull);
			if(CFPropertyListRef plist = CFPropertyListCreateFromXMLData(kCFAllocatorDefault, data, kCFPropertyListImmutable, NULL))
			{
				res = convert(plist);
				CFRelease(plist);
			}
			CFRelease(data);
		}
		return res;
	}

	any_t parse (std::string const& str)
	{
		any_t res;

		CFDataRef data = CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, (UInt8 const*)str.data(), str.size(), kCFAllocatorNull);
		if(CFPropertyListRef plist = CFPropertyListCreateFromXMLData(kCFAllocatorDefault, data, kCFPropertyListImmutable, NULL))
		{
			convert_any(plist, res);
			CFRelease(plist);
		}
		else
		{
			fprintf(stderr, "error parsing plist: ‘%s’\n", str.c_str());
		}
		CFRelease(data);

		return res;
	}

	// ========
	// = Save =
	// ========

	namespace
	{
		struct create_cf_property_list_t : boost::static_visitor<CFPropertyListRef>
		{
			CFPropertyListRef operator() (bool flag) const                     { return CFRetain(flag ? kCFBooleanTrue : kCFBooleanFalse); }
			CFPropertyListRef operator() (int32_t i) const                     { return CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &i); }
			CFPropertyListRef operator() (uint64_t i) const                    { return CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt64Type, &i); }
			CFPropertyListRef operator() (std::string const& str) const        { return CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*)str.data(), str.size(), kCFStringEncodingUTF8, false); }
			CFPropertyListRef operator() (std::vector<char> const& data) const { return CFDataCreate(kCFAllocatorDefault, data.empty() ? NULL : (UInt8*)&data[0], data.size()); }
			CFPropertyListRef operator() (oak::date_t const& date) const       { return CFDateCreate(kCFAllocatorDefault, date.value()); }

			CFPropertyListRef operator() (plist::array_t const& array) const
			{
				CFMutableArrayRef res = CFArrayCreateMutable(kCFAllocatorDefault, 0, &kCFTypeArrayCallBacks);
				for(auto const& it : array)
				{
					CFPropertyListRef value = boost::apply_visitor(*this, it);
					CFArrayAppendValue(res, value);
					CFRelease(value);
				}
				return res;
			}

			CFPropertyListRef operator() (plist::dictionary_t const& dict) const
			{
				CFMutableDictionaryRef res = CFDictionaryCreateMutable(kCFAllocatorDefault, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
				for(auto const& it : dict)
				{
					CFPropertyListRef key = (*this)(it.first);
					CFPropertyListRef value = boost::apply_visitor(*this, it.second);
					CFDictionarySetValue(res, key, value);
					CFRelease(key);
					CFRelease(value);
				}
				return res;
			}
		};
	}

	CFPropertyListRef create_cf_property_list (any_t const& plist)
	{
		return boost::apply_visitor(create_cf_property_list_t(), plist);
	}

	bool save (std::string const& path, any_t const& plist, plist_format_t format)
	{
		bool res = false;
		if(CFPropertyListRef cfPlist = create_cf_property_list(plist))
		{
			path::intermediate_t dest(path);
			std::string const& str = dest;
			if(CFURLRef url = CFURLCreateFromFileSystemRepresentation(kCFAllocatorDefault, (UInt8 const*)str.data(), str.size(), false))
			{
				if(CFWriteStreamRef writeStream = CFWriteStreamCreateWithFile(kCFAllocatorDefault, url))
				{
					CFStringRef error = NULL;
					if(CFWriteStreamOpen(writeStream))
					{
						CFPropertyListWriteToStream(cfPlist, writeStream, format == kPlistFormatBinary ? kCFPropertyListBinaryFormat_v1_0 : kCFPropertyListXMLFormat_v1_0, &error);
						if(error == NULL)
							res = dest.commit();
						CFWriteStreamClose(writeStream);
					}
					CFRelease(writeStream);
				}
				CFRelease(url);
			}
			CFRelease(cfPlist);
		}
		return res;
	}

	// =========
	// = Equal =
	// =========

	namespace
	{
		struct equal_helper_t : boost::static_visitor<bool>
		{
			template <typename T, typename U>
			bool operator() (T const& lhs, U const& rhs) const      { return false; }
			template <typename T>
			bool operator() (T const& lhs, T const& rhs) const      { return lhs == rhs; }

			bool operator() (plist::array_t const& lhs, plist::array_t const& rhs) const
			{
				plist::array_t::const_iterator lhsIter = lhs.begin();
				plist::array_t::const_iterator rhsIter = rhs.begin();
				for(; lhsIter != lhs.end() && rhsIter != rhs.end(); ++lhsIter, ++rhsIter)
				{
					if(!boost::apply_visitor(*this, *lhsIter, *rhsIter))
						break;
				}
				return lhsIter == lhs.end() && rhsIter == rhs.end();
			}

			bool operator() (plist::dictionary_t const& lhs, plist::dictionary_t const& rhs) const
			{
				plist::dictionary_t::const_iterator lhsIter = lhs.begin();
				plist::dictionary_t::const_iterator rhsIter = rhs.begin();
				for(; lhsIter != lhs.end() && rhsIter != rhs.end(); ++lhsIter, ++rhsIter)
				{
					if(lhsIter->first != rhsIter->first || !boost::apply_visitor(*this, lhsIter->second, rhsIter->second))
						break;
				}
				return lhsIter == lhs.end() && rhsIter == rhs.end();
			}
		};
	}

	bool equal (any_t const& lhs, any_t const& rhs)
	{
	   return boost::apply_visitor(equal_helper_t(), lhs, rhs);
	}

	// ================
	// = Get Key Path =
	// ================

	static bool convert_to (bool from, int32_t& to)                          { to = from ?  1  :  0 ;           return true; }
	static bool convert_to (bool from, std::string& to)                      { to = from ? "1" : "0";           return true; }

	static bool convert_to (int32_t from, bool& to)                          { to = from ? true : false;        return true; }
	static bool convert_to (int32_t from, uint64_t& to)                      { to = from;                       return from >= 0; }
	static bool convert_to (int32_t from, std::string& to)                   { to = std::to_string(from);       return true; }

	static bool convert_to (uint64_t from, bool& to)                         { to = from ? true : false;        return true; }
	static bool convert_to (uint64_t from, int32_t& to)                      { to = from;                       return from <= INT32_MAX; }
	static bool convert_to (uint64_t from, std::string& to)                  { to = std::to_string(from);       return true; }

	static bool convert_to (std::string const& from, bool& to)               { to = from != "0" ? true : false;      return true; }
	static bool convert_to (std::string const& from, int32_t& to)            { to = strtol(from.c_str(), NULL, 0);   return true; }
	static bool convert_to (std::string const& from, uint64_t& to)           { to = strtoull(from.c_str(), NULL, 0); return true; }
	static bool convert_to (std::string const& from, oak::uuid_t& to)        { return oak::uuid_t::is_valid(from) ? (to = from), true : false; }

	static bool convert_to (bool from, plist::any_t& to)                       { to = from; return true; }
	static bool convert_to (int32_t from, plist::any_t& to)                    { to = from; return true; }
	static bool convert_to (uint64_t from, plist::any_t& to)                   { to = from; return true; }
	static bool convert_to (std::string const& from, plist::any_t& to)         { to = from; return true; }
	static bool convert_to (std::vector<char> const& from, plist::any_t& to)   { to = from; return true; }
	static bool convert_to (oak::date_t const& from, plist::any_t& to)         { to = from; return true; }
	static bool convert_to (plist::array_t const& from, plist::any_t& to)      { to = from; return true; }
	static bool convert_to (plist::dictionary_t const& from, plist::any_t& to) { to = from; return true; }

	template <typename T> bool convert_to (T const& from, T& to)             { to = from; return true;  }
	template <typename T, typename U> bool convert_to (T const& from, U& to) {            return false; }

	template <typename T>
	struct convert_to_helper_t : boost::static_visitor<bool>
	{
		convert_to_helper_t (T& ref) : ref(ref) { }
		T& ref;

		bool operator() (bool flag) const                       { return convert_to(flag,  ref); }
		bool operator() (int32_t i) const                       { return convert_to(i,     ref); }
		bool operator() (uint64_t i) const                      { return convert_to(i,     ref); }
		bool operator() (std::string const& str) const          { return convert_to(str,   ref); }
		bool operator() (std::vector<char> const& data) const   { return convert_to(data,  ref); }
		bool operator() (oak::date_t const& date) const         { return convert_to(date,  ref); }
		bool operator() (plist::array_t const& array) const     { return convert_to(array, ref); }
		bool operator() (plist::dictionary_t const& dict) const { return convert_to(dict,  ref); }
	};

	template <typename T>
	bool get_key_path (any_t const& plist, std::string const& keyPath, T& ref)
	{
		dictionary_t const* dict = boost::get<dictionary_t>(&plist);
		if(!dict)
			return false;

		std::string::size_type sep = keyPath.find('.');
		dictionary_t::const_iterator it = dict->find(keyPath.substr(0, sep));
		while(it == dict->end() && sep != std::string::npos)
			it = dict->find(keyPath.substr(0, sep = keyPath.find('.', sep+1)));

		if(it == dict->end())
			return false;

		if(sep == std::string::npos)
				return boost::apply_visitor(convert_to_helper_t<T>(ref), it->second);
		else	return get_key_path(it->second, keyPath.substr(sep+1), ref);
	}

	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, bool& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, int32_t& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, uint64_t& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, float& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, double& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, oak::date_t& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, oak::uuid_t& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, std::string& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, std::vector<char>& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, any_t& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, array_t& ref);
	template PUBLIC bool get_key_path (any_t const& plist, std::string const& keyPath, dictionary_t& ref);

	// ========================
	// = Supporting Functions =
	// ========================

	bool is_true (any_t const& item)
	{
		bool flag;
		return !item.empty() && boost::apply_visitor(convert_to_helper_t<bool>(flag), item) && flag;
	}

	template <typename T> T get (plist::any_t const& from)
	{
		T to;
		bool res DB_VAR = boost::apply_visitor(convert_to_helper_t<T>(to), from);
		ASSERT(res);
		return to;
	}

	template PUBLIC bool get (plist::any_t const& from);
	template PUBLIC int32_t get (plist::any_t const& from);
	template PUBLIC uint64_t get (plist::any_t const& from);
	template PUBLIC std::string get (plist::any_t const& from);
	template PUBLIC plist::array_t get (plist::any_t const& from);
	template PUBLIC plist::dictionary_t get (plist::any_t const& from);

} /* plist */ 
