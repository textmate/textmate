#import "OakSyntaxFormatter.h"
#import <bundles/query.h>
#import <theme/theme.h>
#import <parse/parse.h>
#import <parse/grammar.h>
#import <text/utf16.h>
#import <ns/ns.h>

static NSString* const kUserDefaultsUIThemeUUID = @"UIThemeUUID";
static size_t kParseSizeLimit = 1024;

@interface OakSyntaxFormatter ()
{
	NSString* _grammarName;

	BOOL _didLoadGrammarAndTheme;
	parse::grammar_ptr _grammar;
	theme_ptr _theme;
}
@end

@implementation OakSyntaxFormatter
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsUIThemeUUID : @(kMacClassicThemeUUID),
	}];
}

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

		if(bundles::item_ptr themeItem = bundles::lookup(to_s([[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsUIThemeUUID])))
			_theme = parse_theme(themeItem);

		if(!_grammar)
			NSLog(@"Failed to load grammar: %@", _grammarName);
		if(!_theme)
			NSLog(@"Failed to load theme: %@", [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsUIThemeUUID]);

		_didLoadGrammarAndTheme = YES;
	}
	return _grammar && _theme;
}

- (void)addStylesToString:(NSMutableAttributedString*)styled
{
	if(!styled || !_grammarName)
		return;

	NSString* plain = styled.string;
	for(NSString* attr in @[ NSForegroundColorAttributeName, NSBackgroundColorAttributeName,NSUnderlineStyleAttributeName, NSStrikethroughStyleAttributeName ])
		[styled removeAttribute:attr range:NSMakeRange(0, plain.length)];

	if(_enabled == NO || ![self tryLoadGrammarAndTheme])
		return;

	std::string str = to_s(plain);
	std::map<size_t, scope::scope_t> scopes;
	parse::parse(str.data(), str.data() + std::min(str.size(), kParseSizeLimit), _grammar->seed(), scopes, true);

	size_t from = 0, pos = 0;
	for(auto pair = scopes.begin(); pair != scopes.end(); )
	{
		styles_t styles = _theme->styles_for_scope(pair->second);
		size_t to = ++pair != scopes.end() ? pair->first : str.size();
		size_t len = utf16::distance(str.data() + from, str.data() + to);
		[styled addAttributes:@{
			NSForegroundColorAttributeName    : [NSColor colorWithCGColor:styles.foreground()],
			NSBackgroundColorAttributeName    : [NSColor colorWithCGColor:styles.background()],
			NSUnderlineStyleAttributeName     : @(styles.underlined() ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
			NSStrikethroughStyleAttributeName : @(styles.strikethrough() ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
		} range:NSMakeRange(pos, len)];

		pos += len;
		from = to;
	}
}
@end
