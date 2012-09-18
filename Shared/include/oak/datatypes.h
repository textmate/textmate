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
				{
					std::string const tmp = pair->first + "=" + pair->second;
					*p++ = strdup(tmp.c_str());
				}
				*p = NULL;
			}

			~c_array ()
			{
				for(char* const* p = _array; p && *p; ++p)
					free(*p);
				delete[] _array;
			}

			operator char* const* () const { return _array; }

		private:
			char* const* _array;
		};
	}
}

#endif /* end of include guard: OAK_DATATYPES_H_665IJ16Q */
