#include "parse.h"
#include "my_ctype.h"
#include "utf8.h"
#include <oak/oak.h>
#include <oak/debug.h>

namespace text
{
	std::vector<std::string> split (std::string const& str, std::string const& sep)
	{
		ASSERT_LT(0, sep.size());
		std::vector<std::string> res;

		std::string::size_type bow = 0;
		while(true)
		{
			std::string::size_type eow = str.find(sep, bow);
			if(eow == std::string::npos)
			{
				res.push_back(str.substr(bow));
				break;
			}
			res.push_back(str.substr(bow, eow - bow));
			bow = eow + sep.size();
		}

		return res;
	}

	std::vector<size_t> soft_breaks (std::string const& str, size_t width, size_t tabSize, size_t prefixSize)
	{
		std::vector<size_t> res;

		size_t col = 0, len = 0, spaceCol = 0, spaceLen = 0;
		citerate(ch, diacritics::make_range(str.data(), str.data() + str.size()))
		{
			size_t prevLen = len, prevCol = col;
			len += ch.length();
			col += (*ch == '\t' ? tabSize - (col % tabSize) : (text::is_east_asian_width(*ch) ? 2 : 1));

			if(*ch == '\n')
			{
				col = 0;

				spaceLen = len;
				spaceCol = col;
			}
			else if(col > width)
			{
				if(spaceCol == 0)
				{
					res.push_back(prevLen);
					width -= prefixSize;
					prefixSize = 0;
					col = col - prevCol;
				}
				else
				{
					res.push_back(spaceLen);
					width -= prefixSize;
					prefixSize = 0;
					col = col - spaceCol;
					spaceCol = 0;

					if(col > width) // Backtrack if text right of the soft break is still too wide: this can only happen for second line of indented soft wrap where the width is decreased after the first break.
					{
						ch = diacritics::make_range(str.data() + spaceLen, str.data() + str.size()).begin();
						len = spaceLen + ch.length();
						col = (*ch == '\t' ? tabSize : 1);
					}
				}
			}
			else if(*ch == ' ')
			{
				spaceLen = len;
				spaceCol = col;
			}
		}
		return res;
	}
}
