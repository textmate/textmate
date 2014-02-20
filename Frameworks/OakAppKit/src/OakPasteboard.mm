/*
	TODO Filter duplicates
*/

#import "OakPasteboard.h"
#import "OakPasteboardSelector.h"
#import <OakFoundation/NSArray Additions.h>
#import <oak/oak.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(Pasteboard);

NSString* const OakReplacePboard                   = @"OakReplacePboard";
NSString* const OakPasteboardDidChangeNotification = @"OakClipboardDidChangeNotification";
NSString* const OakPasteboardOptionsPboardType     = @"OakPasteboardOptionsPboardType";

NSString* const kUserDefaultsFindWrapAround        = @"findWrapAround";
NSString* const kUserDefaultsFindIgnoreCase        = @"findIgnoreCase";

NSString* const OakFindIgnoreWhitespaceOption      = @"ignoreWhitespace";
NSString* const OakFindFullWordsOption             = @"fullWordMatch";
NSString* const OakFindRegularExpressionOption     = @"regularExpression";

NSString* const kUserDefaultsDisablePersistentClipboardHistory = @"disablePersistentClipboardHistory";

@interface OakPasteboardEntry ()
@property (nonatomic) OakPasteboard* pasteboard;
@end

@interface OakPasteboardEntry (PrimitiveAccessors)
- (NSString*)primitiveOptions;
- (void)setPrimitiveOptions:(NSDictionary*)aDictionary;
@end

@implementation OakPasteboardEntry
@dynamic string, options, pasteboard;

+ (OakPasteboardEntry*)pasteboardEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions inContext:(NSManagedObjectContext*)context
{
	D(DBF_Pasteboard, bug("%s, %s\n", aString.UTF8String, someOptions.description.UTF8String););
	ASSERT(aString != nil);
	OakPasteboardEntry* res = [NSEntityDescription insertNewObjectForEntityForName:@"PasteboardEntry" inManagedObjectContext:context];
	res.string = aString;
	res.options = someOptions;
	return res;
}

- (void)setOptions:(NSDictionary*)aDictionary
{
	NSSet* keysToRemvoe = [aDictionary keysOfEntriesPassingTest:^(id key, id obj, BOOL* stop){ return BOOL([obj isKindOfClass:[NSNumber class]] && ![obj boolValue]); }];
	if([keysToRemvoe count])
	{
		NSMutableDictionary* tmp = [aDictionary mutableCopy];
		for(id key in keysToRemvoe)
			[tmp removeObjectForKey:key];
		aDictionary = tmp;
	}
	[self willChangeValueForKey:@"options"];
	[self setPrimitiveOptions:[aDictionary count] ? aDictionary : nil];
	[self didChangeValueForKey:@"options"];
}

- (BOOL)fullWordMatch       { return [self.options[OakFindFullWordsOption] boolValue]; };
- (BOOL)ignoreWhitespace    { return [self.options[OakFindIgnoreWhitespaceOption] boolValue]; };
- (BOOL)regularExpression   { return [self.options[OakFindRegularExpressionOption] boolValue]; };

- (void)setOption:(NSString*)aKey toBoolean:(BOOL)flag
{
	if([self.options[aKey] boolValue] == flag)
		return;

	NSMutableDictionary* newOptions = [self.options mutableCopy] ?: [NSMutableDictionary dictionary];
	if(flag)
			[newOptions setObject:@YES forKey:aKey];
	else	[newOptions removeObjectForKey:aKey];
	self.options = newOptions;
}

- (void)setFullWordMatch:(BOOL)flag       { return [self setOption:OakFindFullWordsOption toBoolean:flag]; };
- (void)setIgnoreWhitespace:(BOOL)flag    { return [self setOption:OakFindIgnoreWhitespaceOption toBoolean:flag]; };
- (void)setRegularExpression:(BOOL)flag   { return [self setOption:OakFindRegularExpressionOption toBoolean:flag]; };

- (find::options_t)findOptions
{
	return find::options_t(
		([self fullWordMatch]       ? find::full_words         : find::none) |
		([[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindIgnoreCase] ? find::ignore_case : find::none) |
		([[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindWrapAround] ? find::wrap_around : find::none) |
		([self ignoreWhitespace]    ? find::ignore_whitespace  : find::none) |
		([self regularExpression]   ? find::regular_expression : find::none));
}
@end

@interface OakPasteboard ()
@property (nonatomic) NSString* name;
@property (nonatomic) NSInteger changeCount;
@property (nonatomic) NSMutableOrderedSet* entries;
@property (nonatomic) NSUInteger index;
- (BOOL)avoidsDuplicates;
- (void)checkForExternalPasteboardChanges;
@end

// ============================
// = Event Loop Idle Callback =
// ============================

namespace
{
	struct event_loop_idle_callback_t;
	static event_loop_idle_callback_t& idle_callback ();

	struct event_loop_idle_callback_t
	{
		event_loop_idle_callback_t () : _running(false) { _observer = CFRunLoopObserverCreate(kCFAllocatorDefault, kCFRunLoopBeforeWaiting, true, 100, &callback, NULL); start(); }
		~event_loop_idle_callback_t ()                  { stop(); CFRelease(_observer); }

		void start ()
		{
			if(_running)
				return;
			_running = true;
			CFRunLoopAddObserver(CFRunLoopGetCurrent(), _observer, kCFRunLoopCommonModes);
		}

		void stop ()
		{
			if(!_running)
				return;
			_running = false;
			CFRunLoopRemoveObserver(CFRunLoopGetCurrent(), _observer, kCFRunLoopCommonModes);
		}

		void add (OakPasteboard* aPasteboard)    { _pasteboards.insert(aPasteboard); }
		void remove (OakPasteboard* aPasteboard) { ASSERT(_pasteboards.find(aPasteboard) != _pasteboards.end()); _pasteboards.erase(aPasteboard); }

	private:
		static void callback (CFRunLoopObserverRef observer, CFRunLoopActivity activity, void* info)
		{
			iterate(it, idle_callback()._pasteboards)
				[*it checkForExternalPasteboardChanges];
		}

		bool _running;
		CFRunLoopObserverRef _observer;
		std::set<OakPasteboard*> _pasteboards;
	};

	static event_loop_idle_callback_t& idle_callback ()
	{
		static event_loop_idle_callback_t res;
		return res;
	}
}

static NSMutableDictionary* SharedInstances = [NSMutableDictionary new];

@implementation OakPasteboard
@dynamic name, entries, index, auxiliaryOptionsForCurrent;
@synthesize changeCount;

+ (void)initialize
{
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidBecomeActiveNotification:) name:NSApplicationDidBecomeActiveNotification object:NSApp];
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidResignActiveNotification:) name:NSApplicationDidResignActiveNotification object:NSApp];
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
}

+ (void)applicationDidBecomeActiveNotification:(id)sender
{
	for(NSString* key in SharedInstances)
		[SharedInstances[key] checkForExternalPasteboardChanges];
	idle_callback().start();
}

+ (void)applicationDidResignActiveNotification:(id)sender
{
	idle_callback().stop();
}

+ (void)applicationWillTerminate:(NSNotification*)aNotification
{
	[self saveContext];
}

+ (NSManagedObjectContext*)managedObjectContext
{
	static NSManagedObjectContext* managedObjectContext;
	if(!managedObjectContext)
	{
		managedObjectContext = [[NSManagedObjectContext alloc] init];
		managedObjectContext.persistentStoreCoordinator = self.persistentStoreCoordinator;
		managedObjectContext.undoManager = nil;
	}
	return managedObjectContext;
}

+ (NSPersistentStoreCoordinator*)persistentStoreCoordinator
{
	static NSPersistentStoreCoordinator* persistentStoreCoordinator;
	if(!persistentStoreCoordinator)
	{
		NSURL* appSupport = [[[[NSFileManager defaultManager] URLsForDirectory:NSApplicationSupportDirectory inDomains:NSUserDomainMask] firstObject] URLByAppendingPathComponent:@"TextMate"];

		NSError* error = nil;
		if(![[NSFileManager defaultManager] createDirectoryAtURL:appSupport withIntermediateDirectories:YES attributes:nil error:&error])
		{
			NSLog(@"unable to create folder ‘%@’: %@", [appSupport path], error);
			[NSApp presentError:error];
			abort();
		}

		NSURL* storeURL = [appSupport URLByAppendingPathComponent:@"ClipboardHistory.db"];
		if([[NSFileManager defaultManager] fileExistsAtPath:[storeURL path]])
		{
			if(NSDictionary* metaData = [NSPersistentStoreCoordinator metadataForPersistentStoreOfType:NSSQLiteStoreType URL:storeURL error:&error])
			{
				if(![self.managedObjectModel isConfiguration:nil compatibleWithStoreMetadata:metaData])
				{
					NSLog(@"delete old (incompatible) store: ‘%@’", [storeURL path]);
					if(![[NSFileManager defaultManager] removeItemAtURL:storeURL error:&error])
						NSLog(@"unable to delete old store ‘%@’: %@", [storeURL path], error);
				}
			}
			else
			{
				NSLog(@"unable to obtain metadata for ‘%@’: %@", [storeURL path], error);
			}
		}

		persistentStoreCoordinator = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:self.managedObjectModel];
		if(![persistentStoreCoordinator addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:storeURL options:nil error:&error])
		{
			NSLog(@"unable to create persistent store ‘%@’: %@", [storeURL path], error);
			[NSApp presentError:error];
			abort();
		}
	}
	return persistentStoreCoordinator;
}

+ (NSManagedObjectModel*)managedObjectModel
{
	static NSManagedObjectModel* managedObjectModel;
	if(!managedObjectModel)
	{
		NSEntityDescription* pasteboardEntity        = [[NSEntityDescription alloc] init];
		NSAttributeDescription* pasteboardName       = [[NSAttributeDescription alloc] init];
		NSRelationshipDescription* pasteboardItems   = [[NSRelationshipDescription alloc] init];
		NSAttributeDescription* pasteboardIndex      = [[NSAttributeDescription alloc] init];
		NSAttributeDescription* pasteboardAuxOptions = [[NSAttributeDescription alloc] init];

		NSEntityDescription* itemEntity              = [[NSEntityDescription alloc] init];
		NSAttributeDescription* itemContent          = [[NSAttributeDescription alloc] init];
		NSAttributeDescription* itemOptions          = [[NSAttributeDescription alloc] init];
		NSRelationshipDescription* itemPasteboard    = [[NSRelationshipDescription alloc] init];

		pasteboardName.name                          = @"name";
		pasteboardName.attributeType                 = NSStringAttributeType;
		pasteboardName.optional                      = NO;

		pasteboardItems.name                         = @"entries";
		pasteboardItems.destinationEntity            = itemEntity;
		pasteboardItems.inverseRelationship          = itemPasteboard;
		pasteboardItems.deleteRule                   = NSCascadeDeleteRule;
		pasteboardItems.ordered                      = YES;

		pasteboardIndex.name                         = @"index";
		pasteboardIndex.attributeType                = NSInteger64AttributeType;

		pasteboardAuxOptions.name                    = @"auxiliaryOptionsForCurrent";
		pasteboardAuxOptions.transient               = YES;
		pasteboardAuxOptions.attributeType           = NSUndefinedAttributeType;
		pasteboardAuxOptions.attributeValueClassName = @"NSDictionary";

		pasteboardEntity.name                        = @"Pasteboard";
		pasteboardEntity.managedObjectClassName      = @"OakPasteboard";
		pasteboardEntity.properties                  = @[ pasteboardName, pasteboardItems, pasteboardIndex, pasteboardAuxOptions ];

		itemContent.name                             = @"string";
		itemContent.attributeType                    = NSStringAttributeType;
		itemContent.optional                         = NO;

		itemOptions.name                             = @"options";
		itemOptions.attributeType                    = NSTransformableAttributeType;

		itemPasteboard.name                          = @"pasteboard";
		itemPasteboard.destinationEntity             = pasteboardEntity;
		itemPasteboard.inverseRelationship           = pasteboardItems;
		itemPasteboard.deleteRule                    = NSNullifyDeleteRule;
		itemPasteboard.maxCount                      = 1;
		itemPasteboard.minCount                      = 1;

		itemEntity.name                              = @"PasteboardEntry";
		itemEntity.managedObjectClassName            = @"OakPasteboardEntry";
		itemEntity.properties                        = @[ itemContent, itemOptions, itemPasteboard ];

		managedObjectModel = [[NSManagedObjectModel alloc] init];
		managedObjectModel.entities = @[ pasteboardEntity, itemEntity ];
	}
	return managedObjectModel;
}

+ (BOOL)saveContext
{
	if([[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsDisablePersistentClipboardHistory] boolValue])
		return YES;

	BOOL res = YES;
	if([self.managedObjectContext hasChanges])
	{
		NSError* error = nil;
		if(!(res = [self.managedObjectContext save:&error]))
			NSLog(@"failed to save context: %@", error);
	}
	return res;
}

+ (OakPasteboard*)pasteboardWithName:(NSString*)aName
{
	OakPasteboard* res = SharedInstances[aName];
	if(!res)
	{
		NSFetchRequest* request = [[NSFetchRequest alloc] init];
		request.entity = [NSEntityDescription entityForName:@"Pasteboard" inManagedObjectContext:self.managedObjectContext];
		request.predicate = [NSPredicate predicateWithFormat:@"name == %@", aName];
		NSError* error;
		if(!(res = [[self.managedObjectContext executeFetchRequest:request error:&error] lastObject]))
		{
			NSMutableOrderedSet* entries = [NSMutableOrderedSet new];

			NSString* userDefaultsKey = [aName isEqualToString:OakReplacePboard] ? @"NSReplacePboard" : aName;
			if(NSArray* history = [[NSUserDefaults standardUserDefaults] arrayForKey:userDefaultsKey])
			{
				for(NSDictionary* entry in history)
				{
					NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:entry];
					[dict removeObjectForKey:@"string"];
					if(NSString* str = entry[@"string"])
						[entries addObject:[OakPasteboardEntry pasteboardEntryWithString:str andOptions:dict inContext:self.managedObjectContext]];
				}

				[[NSUserDefaults standardUserDefaults] removeObjectForKey:userDefaultsKey];
			}

			res = [NSEntityDescription insertNewObjectForEntityForName:@"Pasteboard" inManagedObjectContext:self.managedObjectContext];
			res.name    = aName;
			res.entries = entries;
			res.index   = [entries count]-1;
		}

		SharedInstances[aName] = res;
		idle_callback().add(res);
	}

	[res checkForExternalPasteboardChanges];
	return res;
}

- (NSPasteboard*)pasteboard
{
	return [NSPasteboard pasteboardWithName:self.name];
}

- (BOOL)avoidsDuplicates
{
	return ![self.name isEqualToString:NSGeneralPboard];
}

- (void)checkForExternalPasteboardChanges
{
	D(DBF_Pasteboard, bug("new data: %s (%zd = %zd)\n", BSTR((self.changeCount != [[self pasteboard] changeCount])), (ssize_t)self.changeCount, (ssize_t)[[self pasteboard] changeCount]););
	if(self.changeCount != [[self pasteboard] changeCount])
	{
		// Do not add to history, see http://nspasteboard.org
		if([[self pasteboard] availableTypeFromArray:@[ @"org.nspasteboard.TransientType", @"org.nspasteboard.ConcealedType", @"org.nspasteboard.AutoGeneratedType" ]])
			return;

		NSString* onClipboard = [[self pasteboard] availableTypeFromArray:@[ NSStringPboardType ]] ? [[self pasteboard] stringForType:NSStringPboardType] : nil;
		NSString* onStack = [([self.entries count] == 0 ? nil : self.entries[self.index]) string];
		self.changeCount = [[self pasteboard] changeCount];
		if((onClipboard && !onStack) || (onClipboard && onStack && ![onStack isEqualToString:onClipboard]))
		{
			NSMutableOrderedSet* entries = [self mutableOrderedSetValueForKey:@"entries"];
			[entries addObject:[OakPasteboardEntry pasteboardEntryWithString:onClipboard andOptions:[[self pasteboard] availableTypeFromArray:@[ OakPasteboardOptionsPboardType ]] ? [[self pasteboard] propertyListForType:OakPasteboardOptionsPboardType] : nil inContext:self.managedObjectContext]];
			self.index = [self.entries count]-1;
			self.auxiliaryOptionsForCurrent = nil;
			[[NSNotificationCenter defaultCenter] postNotificationName:OakPasteboardDidChangeNotification object:self];
			[OakPasteboard saveContext];
		}
	}
}

- (void)didUpdateHistoryShouldSave:(BOOL)saveFlag
{
	if(OakPasteboardEntry* current = [self.entries count] == 0 ? nil : self.entries[self.index])
	{
		[[self pasteboard] declareTypes:@[ NSStringPboardType, OakPasteboardOptionsPboardType ] owner:nil];
		[[self pasteboard] setString:[current string] forType:NSStringPboardType];
		[[self pasteboard] setPropertyList:[current options] forType:OakPasteboardOptionsPboardType];
		self.changeCount = [[self pasteboard] changeCount];
	}

	self.auxiliaryOptionsForCurrent = nil;
	[[NSNotificationCenter defaultCenter] postNotificationName:OakPasteboardDidChangeNotification object:self];

	if(saveFlag)
		[OakPasteboard saveContext];
}

- (void)addEntryWithString:(NSString*)aString
{
	[self addEntryWithString:aString andOptions:nil];
}

- (void)addEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions
{
	D(DBF_Pasteboard, bug("%s (currently at %zu / %zu)\n", [aString UTF8String], (size_t)self.index, (size_t)[self.entries count]););
	[self checkForExternalPasteboardChanges];

	BOOL createNewEntry = YES;
	if(self.avoidsDuplicates && [self.entries count])
	{
		if([aString isEqual:[self.entries[self.index] string]])
			return;
		else if([aString isEqual:[[self.entries lastObject] string]])
			createNewEntry = NO;
	}

	if(createNewEntry)
	{
		static NSInteger const kHistorySize = 10000;
		if([self.entries count] > kHistorySize)
		{
			for(OakPasteboardEntry* entry in [self.entries objectsAtIndexes:[[NSIndexSet alloc] initWithIndexesInRange:NSMakeRange(0, [self.entries count] - kHistorySize/2)]])
				[entry.managedObjectContext deleteObject:entry];
		}

		NSMutableOrderedSet* entries = [self mutableOrderedSetValueForKey:@"entries"];
		[entries addObject:[OakPasteboardEntry pasteboardEntryWithString:aString andOptions:someOptions inContext:self.managedObjectContext]];
	}
	self.index = [self.entries count]-1;
	[self didUpdateHistoryShouldSave:createNewEntry];
}

- (OakPasteboardEntry*)previous
{
	D(DBF_Pasteboard, bug("%zu / %zu\n", (size_t)self.index, (size_t)[self.entries count]););
	[self checkForExternalPasteboardChanges];
	self.index = self.index == 0 ? self.index : self.index-1;
	[self didUpdateHistoryShouldSave:NO];
	return [self current];
}

- (OakPasteboardEntry*)current
{
	D(DBF_Pasteboard, bug("%zu / %zu\n", (size_t)self.index, (size_t)[self.entries count]););
	[self checkForExternalPasteboardChanges];
	if([[self pasteboard] availableTypeFromArray:@[ @"org.nspasteboard.TransientType", @"org.nspasteboard.ConcealedType", @"org.nspasteboard.AutoGeneratedType" ]])
	{
		if(NSString* onClipboard = [[self pasteboard] availableTypeFromArray:@[ NSStringPboardType ]] ? [[self pasteboard] stringForType:NSStringPboardType] : nil)
		{
			OakPasteboardEntry* res = (OakPasteboardEntry*)[[NSManagedObject alloc] initWithEntity:[NSEntityDescription entityForName:@"PasteboardEntry" inManagedObjectContext:self.managedObjectContext] insertIntoManagedObjectContext:nil];
			res.string  = onClipboard;
			res.options = [[self pasteboard] availableTypeFromArray:@[ OakPasteboardOptionsPboardType ]] ? [[self pasteboard] propertyListForType:OakPasteboardOptionsPboardType] : nil;
			return res;
		}
	}
	return [self.entries count] == 0 ? nil : self.entries[self.index];
}

- (OakPasteboardEntry*)next
{
	D(DBF_Pasteboard, bug("%zu / %zu\n", (size_t)self.index, (size_t)[self.entries count]););
	[self checkForExternalPasteboardChanges];
	self.index = self.index+1 == [self.entries count] ? self.index : self.index+1;
	[self didUpdateHistoryShouldSave:NO];
	return [self current];
}

- (BOOL)selectItemAtPosition:(NSPoint)location withWidth:(CGFloat)width respondToSingleClick:(BOOL)singleClick
{
	[self checkForExternalPasteboardChanges];

	NSUInteger selectedRow = ([self.entries count]-1) - self.index;
	OakPasteboardSelector* pasteboardSelector = [OakPasteboardSelector sharedInstance];
	[pasteboardSelector setEntries:[[self.entries reverseObjectEnumerator] allObjects]];
	[pasteboardSelector setIndex:selectedRow];
	if(width)
		[pasteboardSelector setWidth:width];
	if(singleClick)
		[pasteboardSelector setPerformsActionOnSingleClick];
	selectedRow = [pasteboardSelector showAtLocation:location];

	NSSet* keep = [NSSet setWithArray:[pasteboardSelector entries]];
	for(OakPasteboardEntry* entry in self.entries)
	{
		if(![keep containsObject:entry])
			[entry.managedObjectContext deleteObject:entry];
	}

	self.index = ([keep count]-1) - selectedRow;
	[self didUpdateHistoryShouldSave:YES];

	return [pasteboardSelector shouldSendAction];
}

- (void)selectItemAtPosition:(NSPoint)aLocation andCall:(SEL)aSelector
{
	if([self selectItemAtPosition:aLocation withWidth:0 respondToSingleClick:NO])
		[NSApp sendAction:aSelector to:nil from:self];
}

- (void)selectItemForControl:(NSView*)controlView
{
	NSPoint origin = [[controlView window] convertBaseToScreen:[controlView frame].origin];
	[self selectItemAtPosition:origin withWidth:[controlView frame].size.width respondToSingleClick:YES];
}
@end
