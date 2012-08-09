#ifndef TOKENIZE_H_XSMXEP1D
#define TOKENIZE_H_XSMXEP1D

namespace text
{
	template <typename _ForwardIter, typename _CharT>
	struct tokenize_helper_t
	{
		struct iterator : std::iterator<std::forward_iterator_tag, std::string, off_t>
		{
			iterator (_ForwardIter const& first, _ForwardIter const& last, _CharT delimiter, bool atEnd = false) : first(first), current(first), last(last), delimiter(delimiter), at_end(atEnd)
			{
				while(current != last && *current != delimiter)
					++current;
			}

			bool operator== (iterator const& rhs) const { return first == rhs.first && at_end == rhs.at_end; }
			bool operator!= (iterator const& rhs) const { return first != rhs.first || at_end != rhs.at_end; }

			std::string operator* () const { return std::string(first, current); }

			iterator& operator++ ()
			{
				first = current;
				if(current == last)
				{
					at_end = true;
				}
				else
				{
					if(current != last && *current == delimiter)
						++current;
					first = current;

					while(current != last && *current != delimiter)
						++current;
				}
				return *this;
			}

		private:
			_ForwardIter first, current, last;
			_CharT delimiter;
			bool at_end;
		};

		tokenize_helper_t (_ForwardIter const& first, _ForwardIter const& last, _CharT delimiter) : first(first, last, delimiter, false), last(last, last, delimiter, true) { }

		iterator begin () const { return first; }
		iterator end () const   { return last; }

	private:
		iterator first, last;
	};

	template <typename _ForwardIter, typename _CharT>
	tokenize_helper_t<_ForwardIter, _CharT> tokenize (_ForwardIter first, _ForwardIter last, _CharT delimiter)
	{
		return tokenize_helper_t<_ForwardIter, _CharT>(first, last, delimiter);
	}

} /* text */

#endif /* end of include guard: TOKENIZE_H_XSMXEP1D */
