#import "spellcheck.h"

#import <OakFoundation/NSString Additions.h>
#import <text/utf16.h>

namespace ns
{
	spelling_tag_t::helper_t::helper_t ()
	{
		_tag = [NSSpellChecker uniqueSpellDocumentTag];
	}

	spelling_tag_t::helper_t::~helper_t ()
	{
		NSAutoreleasePool* pool = [NSAutoreleasePool new];
		[[NSSpellChecker sharedSpellChecker] closeSpellDocumentWithTag:_tag];
		[pool drain];
	}

	template <typename _OutputIter>
	_OutputIter spellcheck (char const* first, char const* last, std::string const& language, int tag, size_t offset, _OutputIter out)
	{
		NSSpellChecker* spellChecker = [NSSpellChecker sharedSpellChecker];
		NSString* lang               = [NSString stringWithCxxString:language];
		NSString* str                = [NSString stringWithUTF8String:first length:last - first];
		NSRange range                = [spellChecker checkSpellingOfString:str startingAt:0 language:lang wrap:NO inSpellDocumentWithTag:tag wordCount:NULL];

		while(range.location != NSNotFound && range.length)
		{
			char const* from = utf16::advance(first, range.location, last);
			char const* to   = utf16::advance(from,  range.length,   last);
			*out++ = ns::range_t(offset + from - first, offset + to - first);
			range = [spellChecker checkSpellingOfString:str startingAt:range.location + range.length language:lang wrap:NO inSpellDocumentWithTag:tag wordCount:NULL];
		}
		return out;
	}

	std::vector<ns::range_t> spellcheck (char const* first, char const* last, std::string const& language, spelling_tag_t const& tag)
	{
		NSAutoreleasePool* pool = [NSAutoreleasePool new];
		std::vector<ns::range_t> res;
		size_t offset = 0;

		while(first != last)
		{
			char const* eol = std::find(first, last, '\n');
			spellcheck(first, eol, language, tag, offset, back_inserter(res));
			while(eol != last && *eol == '\n')
				++eol;
			offset += eol - first;
			first = eol;
		}

		[pool drain];
		return res;
	}

	bool is_misspelled (char const* first, char const* last, std::string const& language, spelling_tag_t const& tag)
	{
		return !spellcheck(first, last, language, tag).empty();
	}

} /* ns */

