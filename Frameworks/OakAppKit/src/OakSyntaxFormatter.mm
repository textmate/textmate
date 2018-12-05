#import "OakSyntaxFormatter.h"
#import <OakFoundation/OakFoundation.h>
#import <bundles/query.h>
#import <theme/OakTheme.h>
#import <parse/parse.h>
#import <parse/grammar.h>
#import <text/utf16.h>
#import <ns/ns.h>

static size_t kParseSizeLimit = 1024;

@interface OakSyntaxFormatter ()
{
	NSString* _grammarName;
	NSFont* _font;

	BOOL _didLoadGrammarAndTheme;
	parse::grammar_ptr _grammar;
	OakTheme* _theme;
}
@end

@implementation OakSyntaxFormatter
- (instancetype)initWithGrammarName:(NSString*)grammarName
{
	if(self = [self init])
	{
		_grammarName = grammarName;
	}
	return self;
}

- (NSString*)stringForObjectValue:(id)value
{
	return value;
}

- (BOOL)getObjectValue:(id*)valueRef forString:(NSString*)aString errorDescription:(NSString**)errorRef
{
	// We break NSContinuouslyUpdatesValueBindingOption unless a new instance is returned
	*valueRef = [aString copy];
	return YES;
}

- (NSAttributedString*)attributedStringForObjectValue:(id)value withDefaultAttributes:(NSDictionary*)attributes
{
	NSMutableAttributedString* styled = [[NSMutableAttributedString alloc] initWithString:value attributes:attributes];
	[self addStylesToString:styled];
	return styled;
}

- (BOOL)tryLoadGrammarAndTheme
{
	if(_didLoadGrammarAndTheme == NO)
	{
		for(auto const& bundleItem : bundles::query(bundles::kFieldGrammarScope, to_s(_grammarName), scope::wildcard, bundles::kItemTypeGrammar))
		{
			if(_grammar = parse::parse_grammar(bundleItem))
				break;
		}

		_theme = [OakTheme theme];

		if(!_grammar)
			NSLog(@"Failed to load grammar: %@", _grammarName);

		_didLoadGrammarAndTheme = YES;
	}
	return _grammar && _theme;
}

- (void)addStylesToString:(NSMutableAttributedString*)styled
{
	NSString* plain = styled.string;
	if(OakIsEmptyString(plain) || !_grammarName)
		return;

	if(!_font)
	{
		NSDictionary* attributes = [styled fontAttributesInRange:NSMakeRange(0, plain.length)];
		_font = attributes[NSFontAttributeName] ?: [NSFont systemFontOfSize:0];
	}

	for(NSString* attr in @[ NSForegroundColorAttributeName, NSBackgroundColorAttributeName, NSUnderlineStyleAttributeName, NSStrikethroughStyleAttributeName ])
		[styled removeAttribute:attr range:NSMakeRange(0, plain.length)];

	if(_enabled == NO || ![self tryLoadGrammarAndTheme])
	{
		[styled addAttributes:@{ NSFontAttributeName: _font, NSForegroundColorAttributeName: [NSColor controlTextColor] } range:NSMakeRange(0, plain.length)];
		return;
	}

	std::string str = to_s(plain);
	std::map<size_t, scope::scope_t> scopes;
	parse::parse(str.data(), str.data() + std::min(str.size(), kParseSizeLimit), _grammar->seed(), scopes, true);

	size_t from = 0, pos = 0;
	for(auto pair = scopes.begin(); pair != scopes.end(); )
	{
		OakThemeStyles* styles = [_theme stylesForScope:pair->second];

		size_t to = ++pair != scopes.end() ? pair->first : str.size();
		size_t len = utf16::distance(str.data() + from, str.data() + to);
		NSMutableDictionary* attributes = [@{
			NSFontAttributeName:               _font,
			NSForegroundColorAttributeName:    styles.foregroundColor,
			NSUnderlineStyleAttributeName:     @(styles.underlined ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
			NSStrikethroughStyleAttributeName: @(styles.strikethrough ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
		} mutableCopy];

		if(![styles.backgroundColor isEqual:_theme.backgroundColor])
			attributes[NSBackgroundColorAttributeName] = styles.backgroundColor;

		if(styles.fontTraits)
		{
			attributes[NSFontAttributeName] = [NSFont fontWithDescriptor:[NSFontDescriptor fontDescriptorWithFontAttributes:@{
				NSFontFamilyAttribute: _font.familyName,
				NSFontTraitsAttribute: @{ NSFontSymbolicTrait: @(styles.fontTraits) },
			}] size:_font.pointSize];
		}

		[styled addAttributes:attributes range:NSMakeRange(pos, len)];

		pos += len;
		from = to;
	}
}
@end
