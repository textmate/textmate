/*
	TODO Filter duplicates
*/

#import "OakPasteboard.h"
#import "OakPasteboardSelector.h"
#import <oak/oak.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(Pasteboard);

NSString* const NSReplacePboard                    = @"NSReplacePboard";
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
	NSMutableDictionary* _options;
}
@end

@implementation OakPasteboardEntry
- (id)initWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions
{
	D(DBF_Pasteboard, bug("%s, %s\n", aString.UTF8String, someOptions.description.UTF8String););
	ASSERT(aString != nil);
	if(self = [super init])
	{
		self.string = aString;
		self.options = [[NSMutableDictionary alloc] initWithDictionary:someOptions];
	}
	return self;
}

+ (OakPasteboardEntry*)pasteboardEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions
{
	return [[self alloc] initWithString:aString andOptions:someOptions];
}

- (void)setOptions:(NSDictionary*)aDictionary
{
	if(_options == aDictionary)
		return;
	_options = [NSMutableDictionary dictionaryWithDictionary:aDictionary];
}

- (BOOL)isEqual:(OakPasteboardEntry*)otherEntry
{
	return [otherEntry isKindOfClass:[self class]] && [self.string isEqual:otherEntry.string];
}

- (BOOL)fullWordMatch       { return [_options[OakFindFullWordsOption] boolValue]; };
- (BOOL)ignoreWhitespace    { return [_options[OakFindIgnoreWhitespaceOption] boolValue]; };
- (BOOL)regularExpression   { return [_options[OakFindRegularExpressionOption] boolValue]; };

- (void)setOption:(NSString*)aKey toBoolean:(BOOL)flag
{
	if(!flag)
	{
		[_options removeObjectForKey:aKey];
		return;
	}

	if(!_options)
		_options = [NSMutableDictionary new];
	_options[aKey] = @YES;
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

- (void)setFindOptions:(find::options_t)findOptions
{
	self.fullWordMatch     = (findOptions & find::full_words        ) != 0;
	self.ignoreWhitespace  = (findOptions & find::ignore_whitespace ) != 0;
	self.regularExpression = (findOptions & find::regular_expression) != 0;
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

@implementation OakPasteboard
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
			id options = [[self pasteboard] availableTypeFromArray:@[ OakPasteboardOptionsPboardType ]] ? [[self pasteboard] propertyListForType:OakPasteboardOptionsPboardType] : nil;
			[_entries addObject:[OakPasteboardEntry pasteboardEntryWithString:onClipboard andOptions:options]];
			_index = [_entries count]-1;
			_auxiliaryOptionsForCurrent = nil;

			[[NSNotificationCenter defaultCenter] postNotificationName:OakPasteboardDidChangeNotification object:self];
			[self saveToDefaults];
		}
	}
}

- (void)applicationDidBecomeActiveNotification:(id)sender
{
	[self checkForExternalPasteboardChanges];
	idle_callback().start();
}

- (void)applicationDidResignActiveNotification:(id)sender
{
	idle_callback().stop();
}

- (void)setIndex:(NSUInteger)newIndex
{
	D(DBF_Pasteboard, bug("%zu → %zu\n", (size_t)_index, (size_t)newIndex););
	if(_index != newIndex)
	{
		_index = newIndex;
		_auxiliaryOptionsForCurrent = nil;
		if(OakPasteboardEntry* current = [_entries count] == 0 ? nil : _entries[_index])
		{
			[[self pasteboard] declareTypes:@[ NSStringPboardType, OakPasteboardOptionsPboardType ] owner:nil];
			[[self pasteboard] setString:[current string] forType:NSStringPboardType];
			[[self pasteboard] setPropertyList:[current options] forType:OakPasteboardOptionsPboardType];
			_changeCount = [[self pasteboard] changeCount];

			[[NSNotificationCenter defaultCenter] postNotificationName:OakPasteboardDidChangeNotification object:self];
		}
	}
}

- (id)initWithName:(NSString*)aName
{
	if(self = [super init])
	{
		_pasteboardName = aName;

		_entries = [NSMutableArray new];
		if(![[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsDisablePersistentClipboardHistory] boolValue])
		{
			if(NSArray* history = [[NSUserDefaults standardUserDefaults] arrayForKey:_pasteboardName])
			{
				for(NSDictionary* entry in history)
				{
					NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:entry];
					[dict removeObjectForKey:@"string"];
					if(NSString* str = entry[@"string"])
						[_entries addObject:[OakPasteboardEntry pasteboardEntryWithString:str andOptions:dict]];
				}

				_index = [_entries count]-1;
			}
		}

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidBecomeActiveNotification:) name:NSApplicationDidBecomeActiveNotification object:NSApp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidResignActiveNotification:) name:NSApplicationDidResignActiveNotification object:NSApp];
		[self checkForExternalPasteboardChanges];
		idle_callback().add(self);
	}
	return self;
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	[self saveToDefaults];
}

- (void)dealloc
{
	idle_callback().remove(self);
	[[NSNotificationCenter defaultCenter] removeObserver:self name:NSApplicationDidBecomeActiveNotification object:NSApp];
	[[NSNotificationCenter defaultCenter] removeObserver:self name:NSApplicationDidResignActiveNotification object:NSApp];
	[[NSNotificationCenter defaultCenter] removeObserver:self name:NSApplicationWillTerminateNotification object:NSApp];
}

+ (OakPasteboard*)pasteboardWithName:(NSString*)aName
{
	static NSMutableDictionary* SharedInstances = [NSMutableDictionary new];
	OakPasteboard* res = SharedInstances[aName];
	if(!res)
	{
		SharedInstances[aName] = res = [[OakPasteboard alloc] initWithName:aName];
		if(![aName isEqualToString:NSGeneralPboard])
			res.avoidsDuplicates = YES;
	}

	[res checkForExternalPasteboardChanges];
	return res;
}

- (void)addEntryWithString:(NSString*)aString
{
	[self addEntryWithString:aString andOptions:nil];
}

- (void)addEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions
{
	D(DBF_Pasteboard, bug("%s (currently at %zu / %zu)\n", [aString UTF8String], (size_t)_index, (size_t)[_entries count]););
	[self checkForExternalPasteboardChanges];
	OakPasteboardEntry* anEntry = [OakPasteboardEntry pasteboardEntryWithString:aString andOptions:someOptions];
	if(_avoidsDuplicates && [[_entries lastObject] isEqual:anEntry])
	{
		_index = [_entries count]-1;
		_auxiliaryOptionsForCurrent = nil;
	}
	else if(!_avoidsDuplicates || ![_entries count] || ![_entries[_index] isEqual:anEntry])
	{
		[_entries addObject:anEntry];
		[self setIndex:[_entries count]-1];
	}
	[self saveToDefaults];
}

- (OakPasteboardEntry*)previous
{
	D(DBF_Pasteboard, bug("%zu / %zu\n", (size_t)_index, (size_t)[_entries count]););
	[self checkForExternalPasteboardChanges];
	[self setIndex:_index == 0 ? _index : _index-1];
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
	[self setIndex:_index+1 == [_entries count] ? _index : _index+1];
	return [self current];
}

- (void)setEntries:(NSArray*)newEntries
{
	if(![newEntries isEqual:_entries])
	{
		_entries = [newEntries mutableCopy];
		_index = [_entries count]-1;
		_auxiliaryOptionsForCurrent = nil;
		[[NSNotificationCenter defaultCenter] postNotificationName:OakPasteboardDidChangeNotification object:self];
		[self saveToDefaults];
	}
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

	[self setEntries:[[[pasteboardSelector entries] reverseObjectEnumerator] allObjects]];
	[self setIndex:([_entries count]-1) - selectedRow];

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
