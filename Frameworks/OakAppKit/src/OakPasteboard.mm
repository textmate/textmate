/*
	TODO Filter duplicates
*/

#import "OakPasteboard.h"
#import "OakPasteboardSelector.h"
#import <oak/oak.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(Pasteboard);

NSString* const OakReplacePboard                   = @"NSReplacePboard";
NSString* const OakPasteboardDidChangeNotification = @"OakClipboardDidChangeNotification";
NSString* const OakPasteboardOptionsPboardType     = @"OakPasteboardOptionsPboardType";

NSString* const kUserDefaultsFindWrapAround        = @"findWrapAround";
NSString* const kUserDefaultsFindIgnoreCase        = @"findIgnoreCase";

NSString* const OakFindIgnoreWhitespaceOption      = @"ignoreWhitespace";
NSString* const OakFindFullWordsOption             = @"fullWordMatch";
NSString* const OakFindRegularExpressionOption     = @"regularExpression";

NSString* const kUserDefaultsDisablePersistentClipboardHistory = @"disablePersistentClipboardHistory";

@interface OakPasteboardEntry ()
{
	NSDictionary* _options;
}
@end

@implementation OakPasteboardEntry
+ (OakPasteboardEntry*)pasteboardEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions
{
	D(DBF_Pasteboard, bug("%s, %s\n", aString.UTF8String, someOptions.description.UTF8String););
	ASSERT(aString != nil);
	OakPasteboardEntry* res = [[self alloc] init];
	res.string = aString;
	res.options = someOptions;
	return res;
}

- (void)setOptions:(NSDictionary*)aDictionary
{
	if(_options == aDictionary)
		return;

	NSSet* keysToRemvoe = [aDictionary keysOfEntriesPassingTest:^(id key, id obj, BOOL* stop){ return BOOL([obj isKindOfClass:[NSNumber class]] && ![obj boolValue]); }];
	if([keysToRemvoe count])
	{
		NSMutableDictionary* tmp = [aDictionary mutableCopy];
		for(id key in keysToRemvoe)
			[tmp removeObjectForKey:key];
		aDictionary = tmp;
	}

	_options = [aDictionary count] ? aDictionary : nil;
}

- (BOOL)fullWordMatch       { return [_options[OakFindFullWordsOption] boolValue]; };
- (BOOL)ignoreWhitespace    { return [_options[OakFindIgnoreWhitespaceOption] boolValue]; };
- (BOOL)regularExpression   { return [_options[OakFindRegularExpressionOption] boolValue]; };

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

- (NSDictionary*)asDictionary
{
	NSMutableDictionary* res = [NSMutableDictionary dictionaryWithDictionary:_options];
	res[@"string"] = self.string ?: @"";
	return res;
}
@end

@interface OakPasteboard ()
{
	NSMutableArray* _entries;
}
@property (nonatomic) BOOL avoidsDuplicates;
@property (nonatomic) NSString* pasteboardName;
@property (nonatomic) NSInteger changeCount;
@property (nonatomic) NSArray* entries;
@property (nonatomic) NSUInteger index;
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
	for(NSString* key in SharedInstances)
		[SharedInstances[key] saveToDefaults];
}

+ (OakPasteboard*)pasteboardWithName:(NSString*)aName
{
	OakPasteboard* res = SharedInstances[aName];
	if(!res)
	{
		NSMutableArray* entries = [NSMutableArray new];

		if(![[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsDisablePersistentClipboardHistory] boolValue])
		{
			if(NSArray* history = [[NSUserDefaults standardUserDefaults] arrayForKey:aName])
			{
				for(NSDictionary* entry in history)
				{
					NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:entry];
					[dict removeObjectForKey:@"string"];
					if(NSString* str = entry[@"string"])
						[entries addObject:[OakPasteboardEntry pasteboardEntryWithString:str andOptions:dict]];
				}
			}
		}

		SharedInstances[aName] = res = [[OakPasteboard alloc] init];
		res.pasteboardName   = aName;
		res.avoidsDuplicates = ![aName isEqualToString:NSGeneralPboard];
		res.entries          = entries;
		res.index            = [entries count]-1;

		idle_callback().add(res);
	}

	[res checkForExternalPasteboardChanges];
	return res;
}

- (NSPasteboard*)pasteboard
{
	return [NSPasteboard pasteboardWithName:_pasteboardName];
}

- (void)saveToDefaults
{
	if([[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsDisablePersistentClipboardHistory] boolValue])
		return [[NSUserDefaults standardUserDefaults] removeObjectForKey:_pasteboardName];

	NSArray* tmp = [_entries count] < 50 ? _entries : [_entries subarrayWithRange:NSMakeRange([_entries count]-50, 50)];
	[[NSUserDefaults standardUserDefaults] setObject:[tmp valueForKey:@"asDictionary"] forKey:_pasteboardName];
}

- (void)checkForExternalPasteboardChanges
{
	D(DBF_Pasteboard, bug("new data: %s (%zd = %zd)\n", BSTR((_changeCount != [[self pasteboard] changeCount])), (ssize_t)_changeCount, (ssize_t)[[self pasteboard] changeCount]););
	if(_changeCount != [[self pasteboard] changeCount])
	{
		// Do not add to history, see http://nspasteboard.org
		if([[self pasteboard] availableTypeFromArray:@[ @"org.nspasteboard.TransientType", @"org.nspasteboard.ConcealedType", @"org.nspasteboard.AutoGeneratedType" ]])
			return;

		NSString* onClipboard = [[self pasteboard] availableTypeFromArray:@[ NSStringPboardType ]] ? [[self pasteboard] stringForType:NSStringPboardType] : nil;
		NSString* onStack = [([_entries count] == 0 ? nil : _entries[_index]) string];
		_changeCount = [[self pasteboard] changeCount];
		if((onClipboard && !onStack) || (onClipboard && onStack && ![onStack isEqualToString:onClipboard]))
		{
			[_entries addObject:[OakPasteboardEntry pasteboardEntryWithString:onClipboard andOptions:[[self pasteboard] availableTypeFromArray:@[ OakPasteboardOptionsPboardType ]] ? [[self pasteboard] propertyListForType:OakPasteboardOptionsPboardType] : nil]];
			_index = [_entries count]-1;
			_auxiliaryOptionsForCurrent = nil;
			[[NSNotificationCenter defaultCenter] postNotificationName:OakPasteboardDidChangeNotification object:self];
			[self saveToDefaults];
		}
	}
}

- (void)didUpdateHistoryShouldSave:(BOOL)saveFlag
{
	if(OakPasteboardEntry* current = [_entries count] == 0 ? nil : _entries[_index])
	{
		[[self pasteboard] declareTypes:@[ NSStringPboardType, OakPasteboardOptionsPboardType ] owner:nil];
		[[self pasteboard] setString:[current string] forType:NSStringPboardType];
		[[self pasteboard] setPropertyList:[current options] forType:OakPasteboardOptionsPboardType];
		_changeCount = [[self pasteboard] changeCount];
	}

	_auxiliaryOptionsForCurrent = nil;
	[[NSNotificationCenter defaultCenter] postNotificationName:OakPasteboardDidChangeNotification object:self];

	if(saveFlag)
		[self saveToDefaults];
}

- (void)addEntryWithString:(NSString*)aString
{
	[self addEntryWithString:aString andOptions:nil];
}

- (void)addEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions
{
	D(DBF_Pasteboard, bug("%s (currently at %zu / %zu)\n", [aString UTF8String], (size_t)_index, (size_t)[_entries count]););
	[self checkForExternalPasteboardChanges];

	BOOL createNewEntry = YES;
	if(_avoidsDuplicates && [_entries count])
	{
		if([aString isEqual:[_entries[_index] string]])
			return;
		else if([aString isEqual:[[_entries lastObject] string]])
			createNewEntry = NO;
	}

	if(createNewEntry)
		[_entries addObject:[OakPasteboardEntry pasteboardEntryWithString:aString andOptions:someOptions]];
	_index = [_entries count]-1;
	[self didUpdateHistoryShouldSave:createNewEntry];
}

- (OakPasteboardEntry*)previous
{
	D(DBF_Pasteboard, bug("%zu / %zu\n", (size_t)_index, (size_t)[_entries count]););
	[self checkForExternalPasteboardChanges];
	_index = _index == 0 ? _index : _index-1;
	[self didUpdateHistoryShouldSave:NO];
	return [self current];
}

- (OakPasteboardEntry*)current
{
	D(DBF_Pasteboard, bug("%zu / %zu\n", (size_t)_index, (size_t)[_entries count]););
	[self checkForExternalPasteboardChanges];
	if([[self pasteboard] availableTypeFromArray:@[ @"org.nspasteboard.TransientType", @"org.nspasteboard.ConcealedType", @"org.nspasteboard.AutoGeneratedType" ]])
	{
		if(NSString* onClipboard = [[self pasteboard] availableTypeFromArray:@[ NSStringPboardType ]] ? [[self pasteboard] stringForType:NSStringPboardType] : nil)
			return [OakPasteboardEntry pasteboardEntryWithString:onClipboard andOptions:[[self pasteboard] availableTypeFromArray:@[ OakPasteboardOptionsPboardType ]] ? [[self pasteboard] propertyListForType:OakPasteboardOptionsPboardType] : nil];
	}
	return [_entries count] == 0 ? nil : _entries[_index];
}

- (OakPasteboardEntry*)next
{
	D(DBF_Pasteboard, bug("%zu / %zu\n", (size_t)_index, (size_t)[_entries count]););
	[self checkForExternalPasteboardChanges];
	_index = _index+1 == [_entries count] ? _index : _index+1;
	[self didUpdateHistoryShouldSave:NO];
	return [self current];
}

- (BOOL)selectItemAtPosition:(NSPoint)location withWidth:(CGFloat)width respondToSingleClick:(BOOL)singleClick
{
	[self checkForExternalPasteboardChanges];

	NSUInteger selectedRow = ([_entries count]-1) - _index;
	OakPasteboardSelector* pasteboardSelector = [OakPasteboardSelector sharedInstance];
	[pasteboardSelector setEntries:[[_entries reverseObjectEnumerator] allObjects]];
	[pasteboardSelector setIndex:selectedRow];
	if(width)
		[pasteboardSelector setWidth:width];
	if(singleClick)
		[pasteboardSelector setPerformsActionOnSingleClick];
	selectedRow = [pasteboardSelector showAtLocation:location];

	_entries = [[[[pasteboardSelector entries] reverseObjectEnumerator] allObjects] mutableCopy];
	_index   = ([_entries count]-1) - selectedRow;
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
