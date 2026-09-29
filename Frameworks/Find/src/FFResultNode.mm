#import "FFResultNode.h"
#import <OakFoundation/src/NSString Additions.h>
#import <OakAppKit/src/NSColor Additions.h>
#import <document/src/OakDocument.h>
#import <ns/src/ns.h>
#import <text/src/tokenize.h>
#import <text/src/format.h>
#import <text/src/utf8.h>
#import <regexp/src/format_string.h>

namespace
{
	struct string_builder_t
	{
		string_builder_t (NSLineBreakMode mode = NSLineBreakByTruncatingTail)
		{
			NSMutableParagraphStyle* paragraph = [NSMutableParagraphStyle new];
			[paragraph setLineBreakMode:mode];
			[paragraph setAllowsDefaultTighteningForTruncation:NO];

			_string = [[NSMutableAttributedString alloc] init];
			_attributes.push_back(@{ NSParagraphStyleAttributeName: paragraph });
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

		void append (std::string const& cStr, NSFontTraitMask fontTraits)
		{
			append(cStr, nil, fontTraits);
		}

		void append (std::string const& cStr, NSDictionary* attrs = nil, NSFontTraitMask fontTraits = 0)
		{
			if(NSString* str = to_ns(cStr))
			{
				push_style(attrs, fontTraits);

				NSMutableAttributedString* aStr = [[NSMutableAttributedString alloc] initWithString:str attributes:_attributes.back()];
				[aStr applyFontTraits:_font_traits.back() range:NSMakeRange(0, str.length)];
				[_string appendAttributedString:aStr];

				pop_style();
			}
			else
			{
				NSDictionary* attributes = @{
					NSForegroundColorAttributeName: NSColor.systemYellowColor,
					NSBackgroundColorAttributeName: NSColor.systemRedColor,
				};
				[_string appendAttributedString:[[NSMutableAttributedString alloc] initWithString:@"Error: Please file a bug report!" attributes:attributes]];
			}
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
		NSFontAttributeName:            font,
		NSForegroundColorAttributeName: [NSColor secondaryLabelColor]
	});
	builder.append(text::join(std::vector<std::string>(components.begin(), components.end()), " ‣ "));
	builder.append((path::is_absolute(path) ? path::display_name(path) : path), NSBoldFontMask);
	return builder.attributed_string();
}

static void append (string_builder_t& dst, std::string const& src, size_t from, size_t to)
{
	size_t begin = from;
	for(size_t i = from; i != to; ++i)
	{
		if(src[i] == '\t' || src[i] == '\r')
		{
			dst.append(src.substr(begin, i-begin));
			if(src[i] == '\t')
				dst.append("\u2003");
			else if(src[i] == '\r')
				dst.append("<CR>", @{ NSForegroundColorAttributeName: NSColor.tertiaryLabelColor });
			begin = i+1;
		}
	}
	dst.append(src.substr(begin, to-begin));
}

static NSAttributedString* AttributedStringForMatch (std::string const& text, size_t from, size_t to, size_t n, std::string const& newlines, NSFont* font)
{
	NSFontTraitMask matchFontTraits = NSBoldFontMask;
	NSDictionary* matchAttributes = @{
		NSForegroundColorAttributeName: [NSColor textColor],
		NSBackgroundColorAttributeName: [NSColor tmMatchedTextBackgroundColor],
		NSUnderlineStyleAttributeName:  @(NSUnderlineStyleSingle),
		NSUnderlineColorAttributeName:  [NSColor tmMatchedTextUnderlineColor],
	};

	string_builder_t builder(NSLineBreakByTruncatingTail);
	builder.push_style(@{
		NSFontAttributeName:            font,
		NSForegroundColorAttributeName: [NSColor secondaryLabelColor]
	});

	// Ensure monospaced digits for the line number prefix
	NSFontDescriptor* descriptor = [font.fontDescriptor fontDescriptorByAddingAttributes:@{
		NSFontFeatureSettingsAttribute: @[ @{ NSFontFeatureTypeIdentifierKey: @(kNumberSpacingType), NSFontFeatureSelectorIdentifierKey: @(kMonospacedNumbersSelector) } ]
	}];
	builder.append(text::pad(++n, 4) + ": ", @{ NSFontAttributeName: [NSFont fontWithDescriptor:descriptor size:0] });

	bool inMatch = false;
	size_t last = text.size();
	for(size_t it = 0; it != last; )
	{
		size_t eol = text.find(newlines, it);
		eol = eol != std::string::npos ? eol : last;

		if(std::clamp(from, it, eol) == from)
		{
			append(builder, text, it, from);
			it = from;

			if(!inMatch)
				builder.push_style(matchAttributes, matchFontTraits);
			inMatch = true;
		}

		if(inMatch && std::clamp(to, it, eol) == to)
		{
			append(builder, text, it, to);
			it = to;

			builder.pop_style();
			inMatch = false;
		}

		append(builder, text, it, eol);

		if(eol != last)
		{
			builder.append("¬");

			if((eol += newlines.size()) == to)
			{
				builder.pop_style();
				inMatch = false;
			}

			if(eol != last)
			{
				if(inMatch)
					builder.pop_style();
				builder.append("\n");
				builder.append(text::pad(++n, 4) + ": ", @{ NSFontAttributeName: [NSFont fontWithDescriptor:descriptor size:0] });
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
	NSAttributedString* _excerpt;
	NSString* _cachedReplaceString;
}
@property (nonatomic, readwrite) OakDocumentMatch* match;
@property (nonatomic, readwrite) NSUInteger countOfLeafs;
@property (nonatomic, readwrite) NSUInteger countOfExcluded;
@property (nonatomic, readwrite) NSUInteger countOfReadOnly;
@property (nonatomic, readwrite) NSUInteger countOfExcludedReadOnly;
@end

@implementation FFResultNode
- (instancetype)initWithMatch:(OakDocumentMatch*)aMatch
{
	if(self = [super init])
		_match = aMatch;
	return self;
}

+ (FFResultNode*)resultNodeWithMatch:(OakDocumentMatch*)aMatch baseDirectory:(NSString*)base
{
	FFResultNode* res = [[FFResultNode alloc] initWithMatch:aMatch];
	res.children    = [NSMutableArray array];
	res.displayPath = PathComponentString(to_s(base && aMatch.document.path ? aMatch.document.path : aMatch.document.displayName), to_s(base), [NSFont controlContentFontOfSize:0]);
	return res;
}

+ (FFResultNode*)resultNodeWithMatch:(OakDocumentMatch*)aMatch
{
	FFResultNode* res = [[FFResultNode alloc] initWithMatch:aMatch];
	res.countOfLeafs = 1;
	return res;
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
- (OakDocument*)document           { return _match.document; }
- (NSString*)path                  { return _match.document.path; }

- (NSUInteger)lineSpan
{
	text::pos_t const from = _match.range.from;
	text::pos_t const to   = _match.range.to;
	return to.line - from.line + (from == to || to.column != 0 ? 1 : 0);
}

- (NSAttributedString*)excerptWithReplacement:(NSString*)replacementString font:(NSFont*)font
{
	if(_excerpt && (replacementString == _cachedReplaceString || (replacementString && [replacementString isEqualToString:_cachedReplaceString])))
		return _excerpt;

	OakDocumentMatch* m = _match;
	size_t from = m.first - m.excerptOffset;
	size_t to   = m.last  - m.excerptOffset;

	std::string const excerpt = to_s(m.excerpt);
	ASSERT_LE(m.first, m.last);
	ASSERT_LE(from, excerpt.size());
	ASSERT_LE(to, excerpt.size());

	std::string prefix = excerpt.substr(0, from);
	std::string middle = excerpt.substr(from, to - from);
	std::string suffix = excerpt.substr(to);

	if(m.headTruncated)
		prefix.insert(0, "…");
	if(m.tailTruncated)
		suffix.insert(suffix.size(), "…");

	if(replacementString)
		middle = m.captures.empty() ? to_s(replacementString) : format_string::expand(to_s(replacementString), m.captures);

	_excerpt = AttributedStringForMatch(prefix + middle + suffix, prefix.size(), prefix.size() + middle.size(), m.lineNumber, to_s(m.newlines), font);
	_cachedReplaceString = replacementString;
	return _excerpt;
}
@end
