#import "FFResultNode.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <ns/ns.h>
#import <text/tokenize.h>
#import <text/format.h>
#import <text/utf8.h>
#import <regexp/format_string.h>

namespace
{
	struct string_builder_t
	{
		string_builder_t (NSLineBreakMode mode = NSLineBreakByTruncatingTail)
		{
			NSMutableParagraphStyle* paragraph = [NSMutableParagraphStyle new];
			[paragraph setLineBreakMode:mode];
			if([paragraph respondsToSelector:@selector(setAllowsDefaultTighteningForTruncation:)]) // MAC_OS_X_VERSION_10_11
				[paragraph setAllowsDefaultTighteningForTruncation:NO];

			_string = [[NSMutableAttributedString alloc] init];
			_attributes.push_back(@{ NSParagraphStyleAttributeName : paragraph });
			_font_traits.push_back(0);
		}

		void push_style (NSDictionary* newAttributes, NSFontTraitMask newTraits = 0)
		{
			NSMutableDictionary* dict = [_attributes.back() mutableCopy];
			[dict addEntriesFromDictionary:newAttributes];
			_attributes.push_back(dict);
			_font_traits.push_back(_font_traits.back() | newTraits);
		}

		void pop_style ()
		{
			_attributes.pop_back();
			_font_traits.pop_back();
		}

		void append (NSString* str, NSFontTraitMask fontTraits)
		{
			append(str, nil, fontTraits);
		}

		void append (NSString* str, NSDictionary* attrs = nil, NSFontTraitMask fontTraits = 0)
		{
			push_style(attrs, fontTraits);

			NSMutableAttributedString* aStr = [[NSMutableAttributedString alloc] initWithString:str attributes:_attributes.back()];
			[aStr applyFontTraits:_font_traits.back() range:NSMakeRange(0, str.length)];
			[_string appendAttributedString:aStr];

			pop_style();
		}

		NSAttributedString* attributed_string () const
		{
			return _string;
		}

	private:
		NSMutableAttributedString* _string;
		std::vector<NSDictionary*> _attributes;
		std::vector<NSFontTraitMask> _font_traits;
	};

}

static NSAttributedString* PathComponentString (std::string const& path, std::string const& base, NSFont* font)
{
	std::vector<std::string> components;
	std::string str = path::relative_to(path, base);
	for(auto const& component : text::tokenize(str.begin(), str.end(), '/'))
		components.push_back(component);
	if(components.front() == "")
		components.front() = path::display_name("/");
	components.back() = "";

	string_builder_t builder(NSLineBreakByTruncatingMiddle);
	builder.push_style(@{
		NSFontAttributeName            : font,
		NSForegroundColorAttributeName : [NSColor darkGrayColor]
	});
	builder.append(to_ns(text::join(std::vector<std::string>(components.begin(), components.end()), " ▸ ")));
	builder.append(to_ns((path::is_absolute(path) ? path::display_name(path) : path)), NSBoldFontMask);
	return builder.attributed_string();
}

static void append (string_builder_t& dst, std::string const& src, size_t from, size_t to)
{
	size_t begin = from;
	for(size_t i = from; i != to; ++i)
	{
		if(src[i] == '\t' || src[i] == '\r')
		{
			dst.append(to_ns(src.substr(begin, i-begin)));
			if(src[i] == '\t')
				dst.append(@"\u2003");
			else if(src[i] == '\r')
				dst.append(@"<CR>", @{ NSForegroundColorAttributeName : [NSColor lightGrayColor] });
			begin = i+1;
		}
	}
	dst.append(to_ns(src.substr(begin, to-begin)));
}

static NSAttributedString* AttributedStringForMatch (std::string const& text, size_t from, size_t to, size_t n, std::string const& newlines, NSFont* font)
{
	NSFontTraitMask matchFontTraits = NSBoldFontMask;
	NSDictionary* matchAttributes = @{
		NSForegroundColorAttributeName : [NSColor blackColor],
		NSBackgroundColorAttributeName : [NSColor tmMatchedTextBackgroundColor],
		NSUnderlineStyleAttributeName  : @(NSUnderlineStyleSingle),
		NSUnderlineColorAttributeName  : [NSColor tmMatchedTextUnderlineColor],
	};

	string_builder_t builder(NSLineBreakByTruncatingTail);
	builder.push_style(@{
		NSFontAttributeName            : font,
		NSForegroundColorAttributeName : [NSColor darkGrayColor]
	});

	// Ensure monospaced digits for the line number prefix
	NSFontDescriptor* descriptor = [font.fontDescriptor fontDescriptorByAddingAttributes:@{
		NSFontFeatureSettingsAttribute: @[ @{ NSFontFeatureTypeIdentifierKey : @(kNumberSpacingType), NSFontFeatureSelectorIdentifierKey : @(kMonospacedNumbersSelector) } ]
	}];
	builder.append(to_ns(text::pad(++n, 4) + ": "), @{ NSFontAttributeName : [NSFont fontWithDescriptor:descriptor size:0] });

	bool inMatch = false;
	size_t last = text.size();
	for(size_t it = 0; it != last; )
	{
		size_t eol = text.find(newlines, it);
		eol = eol != std::string::npos ? eol : last;

		if(oak::cap(it, from, eol) == from)
		{
			append(builder, text, it, from);
			it = from;

			if(!inMatch)
				builder.push_style(matchAttributes, matchFontTraits);
			inMatch = true;
		}

		if(inMatch && oak::cap(it, to, eol) == to)
		{
			append(builder, text, it, to);
			it = to;

			builder.pop_style();
			inMatch = false;
		}

		append(builder, text, it, eol);

		if(eol != last)
		{
			builder.append(@"¬");

			if((eol += newlines.size()) == to)
			{
				builder.pop_style();
				inMatch = false;
			}

			if(eol != last)
			{
				if(inMatch)
					builder.pop_style();
				builder.append(@"\n");
				builder.append(to_ns(text::pad(++n, 4) + ": "), @{ NSFontAttributeName : [NSFont fontWithDescriptor:descriptor size:0] });
				if(inMatch)
					builder.push_style(matchAttributes, matchFontTraits);
			}
		}

		it = eol;
	}

	return builder.attributed_string();
}

@interface FFResultNode ()
{
	OBJC_WATCH_LEAKS(FFResultNode);
	document::document_t::callback_t* _callback;
	NSAttributedString* _excerpt;
	NSString* _excerptReplaceString;
	find::match_t _match;
}
@property (nonatomic, readwrite) NSUInteger countOfLeafs;
@property (nonatomic, readwrite) NSUInteger countOfExcluded;
@property (nonatomic, readwrite) NSUInteger countOfReadOnly;
@property (nonatomic, readwrite) NSUInteger countOfExcludedReadOnly;
@end

@implementation FFResultNode
- (instancetype)initWithMatch:(find::match_t const&)aMatch
{
	if(self = [super init])
		_match = aMatch;
	return self;
}

+ (FFResultNode*)resultNodeWithMatch:(find::match_t const&)aMatch baseDirectory:(NSString*)base
{
	FFResultNode* res = [[FFResultNode alloc] initWithMatch:aMatch];
	res.children    = [NSMutableArray array];
	res.displayPath = PathComponentString(base && to_s(base) != find::kSearchOpenFiles && aMatch.document->path() != NULL_STR ? aMatch.document->path() : aMatch.document->display_name(), to_s(base), [NSFont controlContentFontOfSize:0]);
	return res;
}

+ (FFResultNode*)resultNodeWithMatch:(find::match_t const&)aMatch
{
	FFResultNode* res = [[FFResultNode alloc] initWithMatch:aMatch];
	res.countOfLeafs = 1;
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

- (void)setCountOfLeafs:(NSUInteger)count             { if(_countOfLeafs            != count) { _parent.countOfLeafs            += count - _countOfLeafs;            _countOfLeafs            = count; } }
- (void)setCountOfExcluded:(NSUInteger)count          { if(_countOfExcluded         != count) { _parent.countOfExcluded         += count - _countOfExcluded;         _countOfExcluded         = count; } }
- (void)setCountOfReadOnly:(NSUInteger)count          { if(_countOfReadOnly         != count) { _parent.countOfReadOnly         += count - _countOfReadOnly;         _countOfReadOnly         = count; } }
- (void)setCountOfExcludedReadOnly:(NSUInteger)count  { if(_countOfExcludedReadOnly != count) { _parent.countOfExcludedReadOnly += count - _countOfExcludedReadOnly; _countOfExcludedReadOnly = count; } }

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
	self.countOfLeafs            += aMatch.countOfLeafs;
	self.countOfExcluded         += aMatch.countOfExcluded;
	self.countOfReadOnly         += aMatch.countOfReadOnly;
	self.countOfExcludedReadOnly += aMatch.countOfExcludedReadOnly;
}

- (void)removeFromParent
{
	[(NSMutableArray*)_parent.children removeObject:self];
	_parent.countOfExcludedReadOnly -= _countOfExcludedReadOnly;
	_parent.countOfReadOnly         -= _countOfReadOnly;
	_parent.countOfExcluded         -= _countOfExcluded;
	_parent.countOfLeafs            -= _countOfLeafs;
}

- (void)setExcluded:(BOOL)flag
{
	if(_children)
	{
		for(FFResultNode* child in _children)
		{
			if(!child.isReadOnly)
				child.excluded = flag;
		}
	}
	else
	{
		self.countOfExcluded         = flag ? 1 : 0;
		self.countOfExcludedReadOnly = flag && _countOfReadOnly ? 1 : 0;
	}
}

- (BOOL)excluded
{
	return _countOfExcluded == (_children ? _countOfLeafs : 1);
}

- (void)setReadOnly:(BOOL)flag
{
	if(_children)
	{
		for(FFResultNode* child in _children)
			child.readOnly = flag;
	}
	else
	{
		self.countOfReadOnly         = flag ? 1 : 0;
		self.countOfExcludedReadOnly = flag && self.excluded ? 1 : 0;
	}
}

- (BOOL)isReadOnly
{
	return _countOfReadOnly == (_children ? _countOfLeafs : 1);
}

- (FFResultNode*)firstResultNode   { return [_children firstObject]; }
- (FFResultNode*)lastResultNode    { return [_children lastObject]; }
- (find::match_t const&)match      { return _match; }
- (document::document_ptr)document { return _match.document; }
- (NSString*)path                  { return [NSString stringWithCxxString:self.document->path()]; }

- (NSUInteger)lineSpan
{
	text::pos_t const from = _match.range.from;
	text::pos_t const to   = _match.range.to;
	return to.line - from.line + (from == to || to.column != 0 ? 1 : 0);
}

- (NSAttributedString*)excerptWithReplacement:(NSString*)replacementString font:(NSFont*)font
{
	if(_excerpt && (replacementString == _excerptReplaceString || [replacementString isEqualToString:_excerptReplaceString]))
		return _excerpt;

	find::match_t const& m = _match;
	size_t from = m.first - m.excerpt_offset;
	size_t to   = m.last  - m.excerpt_offset;

	ASSERT_LE(m.first, m.last);
	ASSERT_LE(from, m.excerpt.size());
	ASSERT_LE(to, m.excerpt.size());

	std::string prefix = m.excerpt.substr(0, from);
	std::string middle = m.excerpt.substr(from, to - from);
	std::string suffix = m.excerpt.substr(to);

	if(m.truncate_head)
		prefix.insert(0, "…");
	if(m.truncate_tail)
		suffix.insert(suffix.size(), "…");

	if(replacementString)
		middle = m.captures.empty() ? to_s(replacementString) : format_string::expand(to_s(replacementString), m.captures);

	if(!utf8::is_valid(prefix.begin(), prefix.end()) || !utf8::is_valid(middle.begin(), middle.end()) || !utf8::is_valid(suffix.begin(), suffix.end()))
	{
		string_builder_t builder(NSLineBreakByTruncatingTail);
		builder.append(to_ns(text::format("%zu-%zu: Range is not valid UTF-8, please contact: http://macromates.com/support", m.first, m.last)), @{
			NSFontAttributeName            : font,
			NSForegroundColorAttributeName : [NSColor darkGrayColor]
		});
		return builder.attributed_string();
	}

	_excerpt = AttributedStringForMatch(prefix + middle + suffix, prefix.size(), prefix.size() + middle.size(), m.line_number(), m.newlines, font);
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
