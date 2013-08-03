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
				_array = new char* [map.size() + 1];
				std::transform(map.begin(), map.end(), _array, [](std::map<std::string, std::string>::value_type const& p){
					std::string const tmp = p.first + "=" + p.second;
					return strdup(tmp.c_str());
				});
				_array[map.size()] = NULL;
			}

			~c_array ()
			{
				for(char* const* p = _array; p && *p; ++p)
					free(*p);
				delete[] _array;
			}

			operator char* const* () const { return _array; }

		private:
			char** _array;
		};
	}
}

#endif /* end of include guard: OAK_DATATYPES_H_665IJ16Q */
