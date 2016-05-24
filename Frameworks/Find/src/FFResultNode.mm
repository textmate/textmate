#import "FFResultNode.h"
#import "attr_string.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <ns/ns.h>
#import <text/tokenize.h>
#import <text/format.h>
#import <text/utf8.h>
#import <regexp/format_string.h>

static NSAttributedString* PathComponentString (std::string const& path, std::string const& base)
{
	std::vector<std::string> components;
	std::string str = path::relative_to(path, base);
	for(auto const& component : text::tokenize(str.begin(), str.end(), '/'))
		components.push_back(component);
	if(components.front() == "")
		components.front() = path::display_name("/");
	components.back() = "";

	return ns::attr_string_t()
		<< ns::style::line_break(NSLineBreakByTruncatingMiddle)
		<< [NSFont controlContentFontOfSize:0]
		<< [NSColor darkGrayColor]
		<< text::join(std::vector<std::string>(components.begin(), components.end()), " ▸ ")
		<< ns::style::bold
		<< (path::is_absolute(path) ? path::display_name(path) : path);
}

static void append (ns::attr_string_t& dst, std::string const& src, size_t from, size_t to)
{
	size_t begin = from;
	for(size_t i = from; i != to; ++i)
	{
		if(src[i] == '\t' || src[i] == '\r')
		{
			dst.add(src.substr(begin, i-begin));
			if(src[i] == '\t')
			{
				dst.add("\u2003");
			}
			else if(src[i] == '\r')
			{
				dst.add(ns::attr_string_t()
					<< [NSFont controlContentFontOfSize:11]
					<< [NSColor lightGrayColor]
					<< "<CR>"
				);
			}
			begin = i+1;
		}
	}
	dst.add(src.substr(begin, to-begin));
}

static NSAttributedString* AttributedStringForMatch (std::string const& text, size_t from, size_t to, size_t n)
{
	ns::attr_string_t str;
	str.add(ns::style::line_break(NSLineBreakByTruncatingTail));
	str.add([NSColor darkGrayColor]);
	str.add([NSFont controlContentFontOfSize:11]);

	str.add(text::pad(++n, 4) + ": ");

	bool inMatch = false;
	size_t last = text.size();
	for(size_t it = 0; it != last; )
	{
		size_t eol = std::find(text.begin() + it, text.end(), '\n') - text.begin();

		if(oak::cap(it, from, eol) == from)
		{
			append(str, text, it, from);
			it = from;
			inMatch = true;
		}

		if(inMatch)
		{
			str.add(ns::style::bold);
			str.add([NSColor blackColor]);
		}

		if(inMatch && oak::cap(it, to, eol) == to)
		{
			append(str, text, it, to);
			it = to;
			inMatch = false;

			str.add([NSColor darkGrayColor]);
			str.add(ns::style::unbold);
		}

		append(str, text, it, eol);

		if(eol != last)
		{
			str.add("¬");

			if(inMatch)
			{
				str.add([NSColor darkGrayColor]);
				str.add(ns::style::unbold);
			}

			if(++eol == to)
				inMatch = false;

			if(eol != last)
				str.add("\n" + text::pad(++n, 4) + ": ");
		}

		it = eol;
	}

	return str;
}

@interface FFResultNode ()
{
	document::document_t::callback_t* _callback;
	NSAttributedString* _excerpt;
	NSString* _excerptReplaceString;
	find::match_t _match;
}
@property (nonatomic, readwrite) NSUInteger countOfLeafs;
@property (nonatomic, readwrite) NSUInteger countOfExcluded;
@end

@implementation FFResultNode
+ (FFResultNode*)resultNodeWithMatch:(find::match_t const&)aMatch baseDirectory:(NSString*)base
{
	FFResultNode* res = [FFResultNode new];
	res->_match     = aMatch;
	res.children    = [NSMutableArray array];
	res.displayPath = PathComponentString(base && to_s(base) != find::kSearchOpenFiles && aMatch.document->path() != NULL_STR ? aMatch.document->path() : aMatch.document->display_name(), to_s(base));
	return res;
}

+ (FFResultNode*)resultNodeWithMatch:(find::match_t const&)aMatch
{
	FFResultNode* res = [FFResultNode new];
	res.countOfLeafs = 1;
	res->_match = aMatch;
	return res;
}

- (void)dealloc
{
	if(_callback)
	{
		self.document->remove_callback(_callback);
		delete _callback;
	}
}

- (void)setCountOfLeafs:(NSUInteger)countOfLeafs
{
	if(_countOfLeafs == countOfLeafs)
		return;
	_parent.countOfLeafs += countOfLeafs - _countOfLeafs;
	_countOfLeafs = countOfLeafs;
}

- (void)setCountOfExcluded:(NSUInteger)countOfExcluded
{
	if(_countOfExcluded == countOfExcluded)
		return;
	_parent.countOfExcluded += countOfExcluded - _countOfExcluded;
	_countOfExcluded = countOfExcluded;
}

- (void)addResultNode:(FFResultNode*)aMatch
{
	if(!_children)
	{
		_children = [NSMutableArray array];
		if(_countOfLeafs)
			self.countOfLeafs -= 1;
	}

	aMatch.parent = self;

	[(NSMutableArray*)_children addObject:aMatch];
	self.countOfLeafs    += aMatch.countOfLeafs;
	self.countOfExcluded += aMatch.ignored ? aMatch.countOfLeafs : aMatch.countOfExcluded;
}

- (void)removeFromParent
{
	[(NSMutableArray*)_parent.children removeObject:self];
	_parent.countOfExcluded -= _ignored ? _countOfLeafs : _countOfExcluded;
	_parent.countOfLeafs    -= _countOfLeafs;
}

- (void)setExcluded:(BOOL)flag
{
	if(_children)
	{
		if(!_ignored)
		{
			for(FFResultNode* child in _children)
				child.excluded = flag;
		}
	}
	else
	{
		if(_ignored)
				_countOfExcluded = flag ? 1 : 0;
		else	self.countOfExcluded = flag ? 1 : 0;
	}
}

- (BOOL)excluded
{
	return _countOfExcluded == (_children ? _countOfLeafs : 1);
}

- (void)setIgnored:(BOOL)flag
{
	if(_ignored == flag)
		return;

	_ignored = flag;
	if(_children)
	{
		NSUInteger countOfExcluded = 0;
		for(FFResultNode* child in _children)
			countOfExcluded += (child.ignored = flag) || child.excluded ? 1 : 0;
		self.countOfExcluded = countOfExcluded;
	}
}

- (FFResultNode*)firstResultNode   { return [_children firstObject]; }
- (FFResultNode*)lastResultNode    { return [_children lastObject]; }
- (find::match_t const&)match      { return _match; }
- (document::document_ptr)document { return _match.document; }
- (NSString*)path                  { return [NSString stringWithCxxString:self.document->path()]; }
- (NSString*)identifier            { return [NSString stringWithCxxString:self.document->identifier()]; }

- (NSUInteger)lineSpan
{
	text::pos_t const from = _match.range.from;
	text::pos_t const to   = _match.range.to;
	return to.line - from.line + (from == to || to.column != 0 ? 1 : 0);
}

- (NSAttributedString*)excerptWithReplacement:(NSString*)replacementString
{
	if(_excerpt && (replacementString == _excerptReplaceString || [replacementString isEqualToString:_excerptReplaceString]))
		return _excerpt;

	find::match_t const& m = _match;

	size_t from = m.first - m.excerpt_offset;
	size_t to   = m.last  - m.excerpt_offset;

	std::string prefix = m.excerpt.substr(0, from);
	std::string middle = m.excerpt.substr(from, to - from);
	std::string suffix = m.excerpt.substr(to);

	if(replacementString)
		middle = m.captures.empty() ? to_s(replacementString) : format_string::expand(to_s(replacementString), m.captures);

	if(!utf8::is_valid(prefix.begin(), prefix.end()) || !utf8::is_valid(middle.begin(), middle.end()) || !utf8::is_valid(suffix.begin(), suffix.end()))
	{
		return ns::attr_string_t()
			<< [NSColor darkGrayColor] << [NSFont controlContentFontOfSize:11]
			<< ns::style::line_break(NSLineBreakByTruncatingTail)
			<< text::format("%zu-%zu: Range is not valid UTF-8, please contact: http://macromates.com/support", m.first, m.last);
	}

	_excerpt = AttributedStringForMatch(prefix + middle + suffix, prefix.size(), prefix.size() + middle.size(), m.line_number);
	_excerptReplaceString = replacementString;
	return _excerpt;
}

- (NSImage*)icon
{
	struct document_callback_t : document::document_t::callback_t
	{
		WATCH_LEAKS(document_callback_t);
		document_callback_t (FFResultNode* self) : _self(self) {}
		void handle_document_event (document::document_ptr document, event_t event)
		{
			if(event == did_change_modified_status)
				_self.icon = nil;
		}

	private:
		__weak FFResultNode* _self;
	};

	if(!_icon)
		_icon = [OakFileIconImage fileIconImageWithPath:self.path isModified:self.document->is_modified()];
	if(!_callback)
		self.document->add_callback(_callback = new document_callback_t(self));

	return _icon;
}
@end
