#import "OakPasteboard.h"
#import "OakPasteboardSelector.h"
#import <crash/info.h>
#import <ns/ns.h>
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
NSString* const kUserDefaultsClipboardHistoryKeepAtLeast       = @"clipboardHistoryKeepAtLeast";
NSString* const kUserDefaultsClipboardHistoryKeepAtMost        = @"clipboardHistoryKeepAtMost";
NSString* const kUserDefaultsClipboardHistoryDaysToKeep        = @"clipboardHistoryDaysToKeep";

@interface OakPasteboardEntry ()
@property (nonatomic) OakPasteboard* pasteboard;
@property (nonatomic) NSDictionary* primitiveOptions;
@end

@implementation OakPasteboardEntry
@dynamic string, options, date, pasteboard, primitiveOptions;

+ (OakPasteboardEntry*)pasteboardEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions inContext:(NSManagedObjectContext*)context
{
	D(DBF_Pasteboard, bug("%s, %s\n", aString.UTF8String, someOptions.description.UTF8String););
	ASSERT(aString != nil);
	OakPasteboardEntry* res = [NSEntityDescription insertNewObjectForEntityForName:@"PasteboardEntry" inManagedObjectContext:context];
	res.string = aString;
	res.options = someOptions;
	res.date = [NSDate date];
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
@property (nonatomic) NSInteger changeCount;
@property (nonatomic) BOOL disableSystemPasteboardUpdating;
@property (nonatomic) OakPasteboardEntry* primitiveCurrentEntry;
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
			for(auto const& it : idle_callback()._pasteboards)
				[it checkForExternalPasteboardChanges];
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

static NSMutableDictionary<NSString*, OakPasteboard*>* SharedInstances = [NSMutableDictionary new];
static BOOL HasPersistentStore = NO;

@implementation OakPasteboard
@dynamic name, currentEntry, auxiliaryOptionsForCurrent, primitiveCurrentEntry;
@synthesize changeCount, disableSystemPasteboardUpdating;

+ (void)initialize
{
	static dispatch_once_t onceToken = 0;
	dispatch_once(&onceToken, ^{
		[[NSUserDefaults standardUserDefaults] registerDefaults:@{
			kUserDefaultsClipboardHistoryKeepAtLeast :  @25,
			kUserDefaultsClipboardHistoryKeepAtMost  : @500,
			kUserDefaultsClipboardHistoryDaysToKeep  :  @30,
		}];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidBecomeActiveNotification:) name:NSApplicationDidBecomeActiveNotification object:NSApp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidResignActiveNotification:) name:NSApplicationDidResignActiveNotification object:NSApp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	});
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
		managedObjectContext.mergePolicy = NSMergeByPropertyObjectTrumpMergePolicy;
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
			crash_reporter_info_t info(to_s(error));
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
		while(![persistentStoreCoordinator addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:storeURL options:nil error:&error])
		{
			NSLog(@"unable to create persistent store ‘%@’: %@", [storeURL path], error);
			if([error.domain isEqualToString:NSCocoaErrorDomain] && error.code == NSFileReadCorruptFileError)
			{
				if([[NSFileManager defaultManager] removeItemAtURL:storeURL error:&error])
					continue;
			}
			[NSApp presentError:error];
			break;
		}
		HasPersistentStore = YES;
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
		NSRelationshipDescription* pasteboardCurrent = [[NSRelationshipDescription alloc] init];
		NSAttributeDescription* pasteboardAuxOptions = [[NSAttributeDescription alloc] init];

		NSEntityDescription* itemEntity              = [[NSEntityDescription alloc] init];
		NSAttributeDescription* itemContent          = [[NSAttributeDescription alloc] init];
		NSAttributeDescription* itemOptions          = [[NSAttributeDescription alloc] init];
		NSAttributeDescription* itemDate             = [[NSAttributeDescription alloc] init];
		NSRelationshipDescription* itemPasteboard    = [[NSRelationshipDescription alloc] init];

		pasteboardName.name                          = @"name";
		pasteboardName.attributeType                 = NSStringAttributeType;
		pasteboardName.optional                      = NO;

		pasteboardCurrent.name                       = @"currentEntry";
		pasteboardCurrent.destinationEntity          = itemEntity;
		pasteboardCurrent.deleteRule                 = NSNullifyDeleteRule;
		pasteboardCurrent.maxCount                   = 1;
		pasteboardCurrent.minCount                   = 1;

		pasteboardAuxOptions.name                    = @"auxiliaryOptionsForCurrent";
		pasteboardAuxOptions.transient               = YES;
		pasteboardAuxOptions.attributeType           = NSUndefinedAttributeType;
		pasteboardAuxOptions.attributeValueClassName = @"NSDictionary";

		pasteboardEntity.name                        = @"Pasteboard";
		pasteboardEntity.managedObjectClassName      = @"OakPasteboard";
		pasteboardEntity.properties                  = @[ pasteboardName, pasteboardCurrent, pasteboardAuxOptions ];

		itemContent.name                             = @"string";
		itemContent.attributeType                    = NSStringAttributeType;
		itemContent.optional                         = NO;
		itemContent.indexed                          = YES;

		itemOptions.name                             = @"options";
		itemOptions.attributeType                    = NSTransformableAttributeType;

		itemDate.name                                = @"date";
		itemDate.attributeType                       = NSDateAttributeType;
		itemDate.optional                            = NO;
		itemDate.indexed                             = YES;

		itemPasteboard.name                          = @"pasteboard";
		itemPasteboard.destinationEntity             = pasteboardEntity;
		itemPasteboard.deleteRule                    = NSNullifyDeleteRule;
		itemPasteboard.maxCount                      = 1;
		itemPasteboard.minCount                      = 1;

		itemEntity.name                              = @"PasteboardEntry";
		itemEntity.managedObjectClassName            = @"OakPasteboardEntry";
		itemEntity.properties                        = @[ itemContent, itemOptions, itemDate, itemPasteboard ];

		managedObjectModel = [[NSManagedObjectModel alloc] init];
		managedObjectModel.entities = @[ pasteboardEntity, itemEntity ];
	}
	return managedObjectModel;
}

+ (BOOL)saveContext
{
	if([[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisablePersistentClipboardHistory])
		return YES;

	BOOL res = YES;
	if([self.managedObjectContext hasChanges] && HasPersistentStore)
	{
		for(NSString* name in SharedInstances)
			[SharedInstances[name] pruneHistory:self];

		@try {
			NSError* error = nil;
			if(!(res = [self.managedObjectContext save:&error]))
				NSLog(@"failed to save context: %@", error);
		}
		@catch(NSException* e) {
			NSAlert* alert        = [[NSAlert alloc] init];
			alert.messageText     = @"Failed Saving Clipboard History";
			alert.informativeText = [NSString stringWithFormat:@"CoreData threw the following exception: %@", e];
			[alert addButtonWithTitle:@"Continue"];
			[alert runModal];
		}
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
			res = [NSEntityDescription insertNewObjectForEntityForName:@"Pasteboard" inManagedObjectContext:self.managedObjectContext];
			res.name = aName;

			// LEGACY format used prior to 2.0-alpha.9513
			NSString* userDefaultsKey = [aName isEqualToString:OakReplacePboard] ? @"NSReplacePboard" : aName;
			if(NSArray* history = [[NSUserDefaults standardUserDefaults] arrayForKey:userDefaultsKey])
			{
				NSTimeInterval interval = 0;
				for(NSDictionary* entry in history)
				{
					NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:entry];
					[dict removeObjectForKey:@"string"];
					if(NSString* str = entry[@"string"])
					{
						OakPasteboardEntry* entry = [OakPasteboardEntry pasteboardEntryWithString:str andOptions:dict inContext:self.managedObjectContext];
						entry.pasteboard = res;
						entry.date = [NSDate dateWithTimeIntervalSinceReferenceDate:interval += 2];
						res.currentEntry = entry;
					}
				}

				[[NSUserDefaults standardUserDefaults] removeObjectForKey:userDefaultsKey];
			}
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
		{
			self.currentEntry = nil;
			return;
		}

		NSString* onClipboard = [[self pasteboard] availableTypeFromArray:@[ NSStringPboardType ]] ? [[self pasteboard] stringForType:NSStringPboardType] : nil;
		NSString* inHistory = self.currentEntry.string;
		self.changeCount = [[self pasteboard] changeCount];
		if((onClipboard && !inHistory) || (onClipboard && inHistory && ![inHistory isEqualToString:onClipboard]))
		{
			self.disableSystemPasteboardUpdating = YES;
			[self internalAddEntryWithString:onClipboard andOptions:[[self pasteboard] availableTypeFromArray:@[ OakPasteboardOptionsPboardType ]] ? [[self pasteboard] propertyListForType:OakPasteboardOptionsPboardType] : nil];
			self.disableSystemPasteboardUpdating = NO;
		}
	}
}

- (void)setCurrentEntry:(OakPasteboardEntry*)newEntry
{
	D(DBF_Pasteboard, bug("%s\n", [newEntry.string UTF8String]););
	self.auxiliaryOptionsForCurrent = nil;
	if(self.primitiveCurrentEntry == newEntry)
		return;

	if(newEntry)
	{
		if(!self.disableSystemPasteboardUpdating)
		{
			[[self pasteboard] declareTypes:@[ NSStringPboardType, OakPasteboardOptionsPboardType ] owner:nil];
			[[self pasteboard] setString:newEntry.string forType:NSStringPboardType];
			[[self pasteboard] setPropertyList:newEntry.options forType:OakPasteboardOptionsPboardType];
		}
		self.changeCount = [[self pasteboard] changeCount];
		[self scheduleSaveHistory:self];
	}

	[self willChangeValueForKey:@"currentEntry"];
	[self setPrimitiveCurrentEntry:newEntry];
	[self didChangeValueForKey:@"currentEntry"];

	[[NSNotificationCenter defaultCenter] postNotificationName:OakPasteboardDidChangeNotification object:self];
}

- (void)addEntryWithString:(NSString*)aString
{
	[self addEntryWithString:aString andOptions:nil];
}

- (void)addEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions
{
	D(DBF_Pasteboard, bug("%s\n", [aString UTF8String]););
	[self checkForExternalPasteboardChanges];
	[self internalAddEntryWithString:aString andOptions:someOptions];
}

- (void)scheduleSaveHistory:(id)sender
{
	static NSTimer* saveHistoryTimer = nil;
	[saveHistoryTimer invalidate];
	saveHistoryTimer = [NSTimer scheduledTimerWithTimeInterval:30 target:[OakPasteboard class] selector:@selector(saveHistoryTimerDidFire:) userInfo:nil repeats:NO];
}

- (void)pruneHistory:(id)sender
{
	NSInteger keepAtLeast = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsClipboardHistoryKeepAtLeast];
	NSInteger keepAtMost  = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsClipboardHistoryKeepAtMost];
	CGFloat daysToKeep    = [[NSUserDefaults standardUserDefaults] floatForKey:kUserDefaultsClipboardHistoryDaysToKeep];

	NSFetchRequest* request = [NSFetchRequest fetchRequestWithEntityName:@"PasteboardEntry"];
	request.sortDescriptors = @[ [NSSortDescriptor sortDescriptorWithKey:@"date" ascending:NO] ];
	request.predicate       = [NSPredicate predicateWithFormat:@"pasteboard = %@", self];
	request.fetchLimit      = keepAtMost;

	NSArray<OakPasteboardEntry*>* allEntries = [self.managedObjectContext executeFetchRequest:request error:nullptr];

	NSDate* keepUntil = [NSDate dateWithTimeIntervalSinceNow:-daysToKeep*24*60*60];
	if(keepAtMost <= allEntries.count)
		keepUntil = [keepUntil laterDate:allEntries[keepAtMost-1].date];
	if(keepAtLeast <= allEntries.count)
		keepUntil = [keepUntil earlierDate:allEntries[keepAtLeast-1].date];

	request = [NSFetchRequest fetchRequestWithEntityName:@"PasteboardEntry"];
	request.predicate = [NSPredicate predicateWithFormat:@"pasteboard = %@ AND date < %@ AND SELF != %@", self, keepUntil, self.currentEntry];
	request.includesPropertyValues = NO;

	for(OakPasteboardEntry* entry in [self.managedObjectContext executeFetchRequest:request error:nullptr])
		[entry.managedObjectContext deleteObject:entry];
}

+ (void)saveHistoryTimerDidFire:(NSTimer*)aTimer
{
	[OakPasteboard saveContext];
}

- (void)internalAddEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions
{
	if(self.avoidsDuplicates)
	{
		NSFetchRequest* request = [[NSFetchRequest alloc] init];
		request.entity = [NSEntityDescription entityForName:@"PasteboardEntry" inManagedObjectContext:self.managedObjectContext];
		request.predicate = [NSPredicate predicateWithFormat:@"pasteboard == %@ AND string == %@", self, aString];

		NSError* error;
		if(OakPasteboardEntry* entry = [[self.managedObjectContext executeFetchRequest:request error:&error] firstObject])
		{
			entry.options = someOptions;
			entry.date = [NSDate date];
			self.currentEntry = entry;
			return;
		}
	}

	OakPasteboardEntry* entry = [OakPasteboardEntry pasteboardEntryWithString:aString andOptions:someOptions inContext:self.managedObjectContext];
	entry.pasteboard = self;
	self.currentEntry = entry;
}

- (OakPasteboardEntry*)previous
{
	D(DBF_Pasteboard, bug("\n"););
	[self checkForExternalPasteboardChanges];

	NSFetchRequest* request = [NSFetchRequest fetchRequestWithEntityName:@"PasteboardEntry"];
	request.predicate = [NSPredicate predicateWithFormat:(self.currentEntry ? @"pasteboard = %@ AND date < %@" : @"pasteboard = %@"), self, self.currentEntry.date];
	request.sortDescriptors = @[ [NSSortDescriptor sortDescriptorWithKey:@"date" ascending:NO] ];
	request.fetchLimit = 1;

	if(OakPasteboardEntry* prev = [[self.managedObjectContext executeFetchRequest:request error:nullptr] firstObject])
		self.currentEntry = prev;

	return self.current;
}

- (OakPasteboardEntry*)current
{
	[self checkForExternalPasteboardChanges];

	OakPasteboardEntry* res = self.currentEntry;
	if(!res && [[self pasteboard] availableTypeFromArray:@[ NSStringPboardType ]])
	{
		res = (OakPasteboardEntry*)[[NSManagedObject alloc] initWithEntity:[NSEntityDescription entityForName:@"PasteboardEntry" inManagedObjectContext:self.managedObjectContext] insertIntoManagedObjectContext:nil];
		res.string  = [[self pasteboard] stringForType:NSStringPboardType];
		res.options = [[self pasteboard] availableTypeFromArray:@[ OakPasteboardOptionsPboardType ]] ? [[self pasteboard] propertyListForType:OakPasteboardOptionsPboardType] : nil;
	}
	return res;
}

- (OakPasteboardEntry*)next
{
	D(DBF_Pasteboard, bug("\n"););
	[self checkForExternalPasteboardChanges];

	NSFetchRequest* request = [NSFetchRequest fetchRequestWithEntityName:@"PasteboardEntry"];
	request.predicate = [NSPredicate predicateWithFormat:(self.currentEntry ? @"pasteboard = %@ AND date > %@" : @"pasteboard = %@"), self, self.currentEntry.date];
	request.sortDescriptors = @[ [NSSortDescriptor sortDescriptorWithKey:@"date" ascending:YES] ];
	request.fetchLimit = 1;

	if(OakPasteboardEntry* next = [[self.managedObjectContext executeFetchRequest:request error:nullptr] firstObject])
		self.currentEntry = next;

	return self.current;
}

- (void)selectItemAtPosition:(NSPoint)location withWidth:(CGFloat)width respondToSingleClick:(BOOL)singleClick
{
	[self checkForExternalPasteboardChanges];
	NSFetchRequest* request = [NSFetchRequest fetchRequestWithEntityName:@"PasteboardEntry"];
	request.predicate = [NSPredicate predicateWithFormat:@"pasteboard = %@", self];
	request.sortDescriptors = @[ [NSSortDescriptor sortDescriptorWithKey:@"date" ascending:NO] ];

	NSError* error;
	NSArray* entries = [self.managedObjectContext executeFetchRequest:request error:&error];
	if(!entries)
	{
		NSLog(@"%s %@", sel_getName(_cmd), error);
		return;
	}

	NSUInteger selectedRow = self.currentEntry ? [entries indexOfObject:self.currentEntry] : 0;
	OakPasteboardSelector* pasteboardSelector = [OakPasteboardSelector sharedInstance];
	[pasteboardSelector setEntries:entries];
	[pasteboardSelector setIndex:selectedRow == NSNotFound ? 0 : selectedRow];
	if(width)
		[pasteboardSelector setWidth:width];
	if(singleClick)
		[pasteboardSelector setPerformsActionOnSingleClick];

	NSInteger newSelection = [pasteboardSelector showAtLocation:location];
	NSArray* newEntries = [pasteboardSelector entries];

	NSSet* keep = [NSSet setWithArray:newEntries];
	for(OakPasteboardEntry* entry in entries)
	{
		if(![keep containsObject:entry])
			[entry.managedObjectContext deleteObject:entry];
	}

	if(newSelection != -1)
		self.currentEntry = [newEntries objectAtIndex:newSelection];
}

- (void)selectItemForControl:(NSView*)controlView
{
	NSPoint origin = [[controlView window] convertRectToScreen:[controlView frame]].origin;
	[self selectItemAtPosition:origin withWidth:[controlView frame].size.width respondToSingleClick:YES];
}
@end
