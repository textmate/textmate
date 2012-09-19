#import "OakAbbreviations.h"
#import <OakFoundation/OakFoundation.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(FilterList_Abbreviations);

static NSString* const FCAbbreviationKey		= @"short";
static NSString* const FCExpandedStringKey	= @"long";

@interface OakAbbreviations ()
- (id)initWithName:(NSString*)aName;
@end

@implementation OakAbbreviations
+ (OakAbbreviations*)abbreviationsForName:(NSString*)aName
{
	static NSMutableDictionary* SharedInstances = [NSMutableDictionary new];
	if(![SharedInstances objectForKey:aName])
		[SharedInstances setObject:[[[self alloc] initWithName:aName] autorelease] forKey:aName];
	return [SharedInstances objectForKey:aName];
}

- (id)initWithName:(NSString*)aName
{
	if(self = [self init])
	{
		name = [aName copy];
		bindings = [[[NSUserDefaults standardUserDefaults] arrayForKey:name] mutableCopy] ?: [NSMutableArray new];
		D(DBF_FilterList_Abbreviations, bug("%s\n", [[bindings description] UTF8String]););
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	}
	return self;
}

- (void)dealloc
{
	[self applicationWillTerminate:nil];
	[[NSNotificationCenter defaultCenter] removeObserver:self name:NSApplicationWillTerminateNotification object:NSApp];
	[bindings release];
	[name release];
	[super dealloc];
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	D(DBF_FilterList_Abbreviations, bug("%s\n", [[bindings description] UTF8String]););
	if([bindings count] > 50)
		[bindings setArray:[bindings subarrayWithRange:NSMakeRange(0, 50)]];
	[[NSUserDefaults standardUserDefaults] setObject:bindings forKey:name];
}

- (NSArray*)stringsForAbbreviation:(NSString*)anAbbreviation
{
	NSMutableArray* exactMatches  = [NSMutableArray array];
	NSMutableArray* prefixMatches = [NSMutableArray array];

	if(NSIsEmptyString(anAbbreviation))
		return exactMatches;

	for(NSDictionary* binding in bindings)
	{
		NSString* abbr = [binding objectForKey:FCAbbreviationKey];
		NSString* path = [binding objectForKey:FCExpandedStringKey];

		if([abbr isEqualToString:anAbbreviation])
			[exactMatches addObject:path];
		else if([abbr hasPrefix:anAbbreviation])
			[prefixMatches addObject:path];
	}

	D(DBF_FilterList_Abbreviations, bug("%s, exact → %s, prefix → %s\n", [anAbbreviation UTF8String], [[exactMatches description] UTF8String], [[prefixMatches description] UTF8String]););

	[exactMatches addObjectsFromArray:prefixMatches];
	return exactMatches;
}

- (void)learnAbbreviation:(NSString*)anAbbreviation forString:(NSString*)aString
{
	D(DBF_FilterList_Abbreviations, bug("%s → %s\n", [anAbbreviation UTF8String], [aString UTF8String]););
	if(NSIsEmptyString(anAbbreviation) || NSIsEmptyString(aString))
		return;

	NSDictionary* dict = [NSDictionary dictionaryWithObjectsAndKeys:
		anAbbreviation,   FCAbbreviationKey,
		aString,          FCExpandedStringKey,
		nil];

	[bindings removeObject:dict];
	[bindings insertObject:dict atIndex:0];
}
@end
