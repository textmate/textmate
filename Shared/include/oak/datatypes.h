#ifndef OAK_DATATYPES_H_665IJ16Q
#define OAK_DATATYPES_H_665IJ16Q

namespace oak
{
	namespace
	{
		struct c_array
		{
			c_array (std::map<std::string, std::string> const& map)
			{
				char** p = new char* [map.size() + 1];
				_array = p;
				iterate(pair, map)
					*p++ = strdup((pair->first + "=" + pair->second).c_str());
				*p = NULL;
			}

			~c_array ()
			{
				char* const* p = _array;
				while(p && *p)
					free(*p++);
				delete[] _array;
			}

			operator char* const* () const { return _array; }

		private:
			char* const* _array;
		};
	}
}

#endif /* end of include guard: OAK_DATATYPES_H_665IJ16Q */
