#import "spellcheck.h"

#import <OakFoundation/NSString Additions.h>
#import <text/utf16.h>

namespace ns
{
	long int spelling_tag_t::helper_t::tag ()
	{
		if(!_did_setup)
		{
			// This should only be called from spellcheck() which runs on the main queue
			ASSERT(dispatch_get_current_queue() == dispatch_get_main_queue());

			_tag = [NSSpellChecker uniqueSpellDocumentTag];
			_did_setup = true;
		}
		return _tag;
	}

	spelling_tag_t::helper_t::~helper_t ()
	{
		if(_did_setup)
		{
			NSInteger tag = _tag;
			dispatch_async(dispatch_get_main_queue(), ^{
				@autoreleasepool {
					[[NSSpellChecker sharedSpellChecker] closeSpellDocumentWithTag:tag];
				}
			});
		}
	}

	template <typename _OutputIter>
	_OutputIter spellcheck (char const* first, char const* last, std::string const& language, long int tag, size_t offset, _OutputIter out)
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
		__block std::vector<ns::range_t> res;
		void(^block)() = ^{
			@autoreleasepool {
				size_t offset = 0;
				for(char const* it = first; it != last; )
				{
					char const* eol = std::find(it, last, '\n');
					spellcheck(it, eol, language, tag, offset, back_inserter(res));
					while(eol != last && *eol == '\n')
						++eol;
					offset += eol - it;
					it = eol;
				}
			}
		};

		if(dispatch_get_current_queue() != dispatch_get_main_queue())
				dispatch_sync(dispatch_get_main_queue(), block);
		else	block();

		return res;
	}

	bool is_misspelled (char const* first, char const* last, std::string const& language, spelling_tag_t const& tag)
	{
		return !spellcheck(first, last, language, tag).empty();
	}

} /* ns */

