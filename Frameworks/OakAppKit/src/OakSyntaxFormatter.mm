#import "OakSyntaxFormatter.h"
#import "OakPasteboard.h"
#import <bundles/query.h>
#import <theme/theme.h>
#import <parse/parse.h>
#import <parse/grammar.h>
#import <text/utf16.h>
#import <ns/ns.h>

static NSString* const kUserDefaultsUIThemeUUID = @"UIThemeUUID";
static size_t kParseSizeLimit = 1024;

@interface OakSyntaxFormatterTransformer : NSValueTransformer
@property (nonatomic, copy) NSString* name;
@property (nonatomic, weak) OakSyntaxFormatter* syntaxFormatter;
@end

@implementation OakSyntaxFormatterTransformer
{
@protected
	__weak OakSyntaxFormatter* _syntaxFormatter;
}
+ (Class)transformedValueClass
{
	return [NSAttributedString class];
}

- (void)dealloc
{
	[self deregisterTransformer];
}

- (void)registerTransformer
{
	[NSValueTransformer setValueTransformer:self forName:_name];
}

- (void)deregisterTransformer
{
	if(_name)
		[NSValueTransformer setValueTransformer:nil forName:_name];
}

- (void)setName:(NSString*)name
{
	[self deregisterTransformer];
	_name = [name copy];
	[self registerTransformer];
}

- (id)transformedValue:(id)value
{
	if(!value)
		return nil;

	if([value isKindOfClass:[NSArray class]])
	{
      NSMutableArray *res = [NSMutableArray array];
      for (id element in value)
	  		[res addObject:[self transformedValue:element]];
		return res;
	}

	return [self concreteTransformedValue:value];
}

- (id)concreteTransformedValue:(id)value
{
	return [_syntaxFormatter attributedStringForObjectValue:value withDefaultAttributes:@{}];
}

- (id)reverseTransformedValue:(id)transformedValue
{
	if([transformedValue isKindOfClass:[NSAttributedString class]])
		transformedValue = [transformedValue string];

	id res;
	[_syntaxFormatter getObjectValue:&res forString:transformedValue errorDescription:nullptr];
	return res;
}
@end

@interface OakPasteboardEntryFormatterTransformer : OakSyntaxFormatterTransformer
@end

@implementation OakPasteboardEntryFormatterTransformer
+ (BOOL)allowsReverseTransformation
{
	return NO;
}

- (id)concreteTransformedValue:(id)value
{
	return [_syntaxFormatter attributedStringForObjectValue:[value string] withDefaultAttributes:@{}];
}
@end

@interface OakPasteboardEntryRegexOnlyFormatterTransformer : OakSyntaxFormatterTransformer
@end

@implementation OakPasteboardEntryRegexOnlyFormatterTransformer
+ (BOOL)allowsReverseTransformation
{
	return NO;
}

- (id)concreteTransformedValue:(id)value
{
	BOOL enabled = _syntaxFormatter.enabled;
	_syntaxFormatter.enabled = (enabled && [(OakPasteboardEntry*)value regularExpression]);
	id res = [_syntaxFormatter attributedStringForObjectValue:[value string] withDefaultAttributes:@{}];
	_syntaxFormatter.enabled = enabled;
	return res;
}
@end

@interface OakSyntaxFormatter ()
{
	NSString* _grammarName;

	BOOL _didLoadGrammarAndTheme;
	parse::grammar_ptr _grammar;
	theme_ptr _theme;
}
@property (nonatomic, readonly) OakSyntaxFormatterTransformer* transformer;
@property (nonatomic, readonly) OakPasteboardEntryFormatterTransformer* pasteboardEntryTransformer;
@property (nonatomic, readonly) OakPasteboardEntryRegexOnlyFormatterTransformer* pasteboardEntryRegexOnlyTransformer;
@end

@implementation OakSyntaxFormatter
@synthesize transformer = _transformer, pasteboardEntryTransformer = _pasteboardEntryTransformer, pasteboardEntryRegexOnlyTransformer = _pasteboardEntryRegexOnlyTransformer;

+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsUIThemeUUID: @(kMacClassicThemeUUID),
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

- (OakSyntaxFormatter*)copyWithZone:(NSZone*)zone
{
	OakSyntaxFormatter* copy = [[self.class allocWithZone:zone] initWithGrammarName:_grammarName];
	copy.enabled = _enabled;
	copy->_didLoadGrammarAndTheme = _didLoadGrammarAndTheme;
	copy->_grammar = _grammar;
	copy->_theme = _theme;
	return copy;
}

- (NSValueTransformer*)transformer
{
	if(!_transformer)
	{
		_transformer = [OakSyntaxFormatterTransformer new];
		_transformer.name = self.transformerName;
		_transformer.syntaxFormatter = self;
	}
	return _transformer;
}

- (NSValueTransformerName)transformerName
{
	(void)[self transformer];
	return [NSString stringWithFormat:@"OakSyntaxFormatter_%p", self];
}

- (NSValueTransformer*)pasteboardEntryTransformer
{
	if(!_pasteboardEntryTransformer)
	{
		_pasteboardEntryTransformer = [OakPasteboardEntryFormatterTransformer new];
		_pasteboardEntryTransformer.name = self.pasteboardEntryTransformerName;
		_pasteboardEntryTransformer.syntaxFormatter = self;
	}
	return _pasteboardEntryTransformer;
}

- (NSValueTransformerName)pasteboardEntryTransformerName
{
	(void)[self pasteboardEntryTransformer];
	return [NSString stringWithFormat:@"OakPasteboardEntryFormatter_%p", self];
}

- (NSValueTransformer*)pasteboardEntryRegexOnlyTransformer
{
	if(!_pasteboardEntryRegexOnlyTransformer)
	{
		_pasteboardEntryRegexOnlyTransformer = [OakPasteboardEntryRegexOnlyFormatterTransformer new];
		_pasteboardEntryRegexOnlyTransformer.name = self.pasteboardEntryRegexOnlyTransformerName;
		_pasteboardEntryRegexOnlyTransformer.syntaxFormatter = self;
	}
	return _pasteboardEntryRegexOnlyTransformer;
}

- (NSValueTransformerName)pasteboardEntryRegexOnlyTransformerName
{
	(void)[self pasteboardEntryRegexOnlyTransformer];
	return [NSString stringWithFormat:@"OakPasteboardEntryRegexOnlyFormatter_%p", self];
}

- (NSString*)stringForObjectValue:(id)value
{
	if([value isKindOfClass:[NSAttributedString class]])
		value = [value string];
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
	if([value isKindOfClass:[NSAttributedString class]])
		value = [value string];
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
	for(NSString* attr in @[ NSForegroundColorAttributeName, NSBackgroundColorAttributeName, NSUnderlineStyleAttributeName, NSStrikethroughStyleAttributeName ])
		[styled removeAttribute:attr range:NSMakeRange(0, plain.length)];

	if(_enabled == NO || ![self tryLoadGrammarAndTheme])
	{
		[styled addAttributes:@{ NSForegroundColorAttributeName: [NSColor controlTextColor] } range:NSMakeRange(0, plain.length)];
		return;
	}

	std::string str = to_s(plain);
	std::map<size_t, scope::scope_t> scopes;
	parse::parse(str.data(), str.data() + std::min(str.size(), kParseSizeLimit), _grammar->seed(), scopes, true);

	size_t from = 0, pos = 0;
	for(auto pair = scopes.begin(); pair != scopes.end(); )
	{
		styles_t styles = _theme->styles_for_scope(pair->second);
		size_t to = ++pair != scopes.end() ? pair->first : str.size();
		size_t len = utf16::distance(str.data() + from, str.data() + to);
		NSMutableDictionary* attributes = [@{
			NSForegroundColorAttributeName:    [NSColor colorWithCGColor:styles.foreground()],
			NSUnderlineStyleAttributeName:     @(styles.underlined() ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
			NSStrikethroughStyleAttributeName: @(styles.strikethrough() ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
		} mutableCopy];

		if(!CGColorEqualToColor(styles.background(), _theme->background()))
			attributes[NSBackgroundColorAttributeName] = [NSColor colorWithCGColor:styles.background()];

		[styled addAttributes:attributes range:NSMakeRange(pos, len)];

		pos += len;
		from = to;
	}
}
@end
