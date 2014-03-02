#ifndef PLIST_SCHEMA_H_K1GXRIRW
#define PLIST_SCHEMA_H_K1GXRIRW

#include "plist.h"

namespace plist
{
	// ==============
	// = Public API =
	// ==============

	template <typename OBJ_TYPE>
	struct schema_t
	{
		template <typename T>
		schema_t<OBJ_TYPE>& map (char const* field, T OBJ_TYPE::* dstField);

		template <typename SRC_T, typename DST_T>
		schema_t<OBJ_TYPE>& map (char const* field, DST_T OBJ_TYPE::* dstField, bool(*converter)(SRC_T const&, DST_T&));

		bool convert (plist::dictionary_t const& dict, OBJ_TYPE* dst) const;

	private:
		struct field_t
		{
			virtual bool handle (plist::any_t const& value, OBJ_TYPE* obj) const = 0;
		};

		typedef std::shared_ptr<field_t> field_ptr;
		std::map<std::string, field_ptr> _fields;
	};

	// ==================
	// = Implementation =
	// ==================

	template <typename OBJ_TYPE>
	bool schema_t<OBJ_TYPE>::convert (plist::dictionary_t const& dict, OBJ_TYPE* dst) const
	{
		for(auto const& pair : dict)
		{
			typename std::map<std::string, field_ptr>::const_iterator it = _fields.find(pair.first);
			if(it != _fields.end() && !it->second->handle(pair.second, dst))
				return false;
		}
		return true;
	}

	template <typename OBJ_TYPE> template <typename T>
	schema_t<OBJ_TYPE>& schema_t<OBJ_TYPE>::map (char const* field, T OBJ_TYPE::* dstField)
	{
		struct variant_field_t : field_t
		{
			variant_field_t (T OBJ_TYPE::* field) : _field(field) { }

			bool handle (plist::any_t const& value, OBJ_TYPE* obj) const
			{
				if(T const* typedValue = boost::get<T>(&value))
						obj->*_field = *typedValue;
				else	obj->*_field = plist::get<T>(value);
				return true;
			}

			T OBJ_TYPE::* _field;
		};

		_fields.emplace(field, field_ptr((field_t*)new variant_field_t(dstField)));
		return *this;
	}

	template <typename OBJ_TYPE> template <typename SRC_T, typename DST_T>
	schema_t<OBJ_TYPE>& schema_t<OBJ_TYPE>::map (char const* field, DST_T OBJ_TYPE::* dstField, bool(*converter)(SRC_T const&, DST_T&))
	{
		struct variant_field_t : field_t
		{
			variant_field_t (DST_T OBJ_TYPE::* field, bool(*converter)(SRC_T const&, DST_T&)) : _field(field), _converter(converter) { }

			bool handle (plist::any_t const& value, OBJ_TYPE* obj) const
			{
				if(SRC_T const* typedValue = boost::get<SRC_T>(&value))
						return _converter(*typedValue, obj->*_field);
				else	return _converter(plist::get<SRC_T>(value), obj->*_field);
			}

			DST_T OBJ_TYPE::* _field;
			bool(*_converter)(SRC_T const&, DST_T&);
		};

		_fields.emplace(field, field_ptr((field_t*)new variant_field_t(dstField, converter)));
		return *this;
	}

} /* plist */

#endif /* end of include guard: PLIST_SCHEMA_H_K1GXRIRW */
