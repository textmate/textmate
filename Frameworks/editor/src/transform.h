#ifndef TRANSFORM_H_XNV2O2H
#define TRANSFORM_H_XNV2O2H

#include <text/src/indent.h>

namespace transform
{
	std::string null (std::string const& src);
	std::string unwrap (std::string src);
	std::string capitalize (std::string const& src);
	std::string transpose (std::string const& src);
	std::string decompose (std::string src);

	struct shift
	{
		shift (ssize_t amount, text::indent_t const& indent) : amount(amount), indent(indent) { }
		std::string operator() (std::string const& src) const;
	private:
		ssize_t amount;
		text::indent_t indent;
	};

	struct reformat
	{
		reformat (size_t wrap, size_t tabSize, bool newlineTerminate = true) : wrap(wrap), tabSize(tabSize), newline(newlineTerminate) { }
		std::string operator() (std::string const& src) const;
	private:
		size_t wrap, tabSize;
		bool newline;
	};

	struct justify
	{
		justify (size_t wrap, size_t tabSize, bool newlineTerminate = true) : wrap(wrap), tabSize(tabSize), newline(newlineTerminate) { }
		std::string operator() (std::string const& src) const;
	private:
		size_t wrap, tabSize;
		bool newline;
	};

	struct replace
	{
		replace (std::string const& text) : text(text) { }
		std::string operator() (std::string const& src) const;
	private:
		std::string text;
	};

	struct surround
	{
		surround (std::string const& prefix, std::string const& suffix) : prefix(prefix), suffix(suffix) { }
		std::string operator() (std::string const& src) const;
	private:
		std::string prefix, suffix;
	};

} /* transform */

#endif /* end of include guard: TRANSFORM_H_XNV2O2H */
