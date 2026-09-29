#import "OakAbbreviations.h"
#import <OakFoundation/src/OakFoundation.h>
#import <oak/debug.h>

static NSString* const FCAbbreviationKey   = @"short";
static NSString* const FCExpandedStringKey = @"long";

@interface OakAbbreviations ()
@property (nonatomic, copy)   NSString* name;
@property (nonatomic) NSMutableArray* bindings;
- (id)initWithName:(NSString*)aName;
@end

@implementation OakAbbreviations
+ (OakAbbreviations*)abbreviationsForName:(NSString*)aName
{
	static NSMutableDictionary* SharedInstances = [NSMutableDictionary new];
	if(!SharedInstances[aName])
		SharedInstances[aName] = [[OakAbbreviations alloc] initWithName:aName];
	return SharedInstances[aName];
}

- (id)initWithName:(NSString*)aName
{
	if(self = [self init])
	{
		self.name     = aName;
		self.bindings = [[NSUserDefaults.standardUserDefaults arrayForKey:self.name] mutableCopy] ?: [NSMutableArray new];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	}
	return self;
}

- (void)dealloc
{
	[self applicationWillTerminate:nil];
	[NSNotificationCenter.defaultCenter removeObserver:self name:NSApplicationWillTerminateNotification object:NSApp];
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	if([self.bindings count] > 50)
		[self.bindings setArray:[self.bindings subarrayWithRange:NSMakeRange(0, 50)]];
	[NSUserDefaults.standardUserDefaults setObject:self.bindings forKey:self.name];
}

- (NSArray*)stringsForAbbreviation:(NSString*)anAbbreviation
{
	NSMutableArray* exactMatches  = [NSMutableArray array];
	NSMutableArray* prefixMatches = [NSMutableArray array];

	if(OakIsEmptyString(anAbbreviation))
		return exactMatches;

	for(NSDictionary* binding in self.bindings)
	{
		NSString* abbr = binding[FCAbbreviationKey];
		NSString* path = binding[FCExpandedStringKey];

		if([abbr isEqualToString:anAbbreviation])
			[exactMatches addObject:path];
		else if([abbr hasPrefix:anAbbreviation])
			[prefixMatches addObject:path];
	}

	[exactMatches addObjectsFromArray:prefixMatches];
	return exactMatches;
}

- (void)learnAbbreviation:(NSString*)anAbbreviation forString:(NSString*)aString
{
	if(OakIsEmptyString(anAbbreviation) || OakIsEmptyString(aString))
		return;

	NSDictionary* dict = @{ FCAbbreviationKey: anAbbreviation, FCExpandedStringKey: aString };
	[self.bindings removeObject:dict];
	[self.bindings insertObject:dict atIndex:0];
}
@end
