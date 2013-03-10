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

+ (OakPasteboardEntry*)pasteboardEntryWithString:(NSString*)aString
{
	return [self pasteboardEntryWithString:aString andOptions:nil];
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
	NSString* pasteboardName;
	NSMutableArray* entries;
	NSDictionary* auxiliaryOptionsForCurrent;
	NSUInteger index;
	NSInteger changeCount;
	BOOL avoidsDuplicates;
}
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
@synthesize avoidsDuplicates, auxiliaryOptionsForCurrent;

- (NSPasteboard*)pasteboard
{
	return [NSPasteboard pasteboardWithName:pasteboardName];
}

- (void)saveToDefaults
{
	if([[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsDisablePersistentClipboardHistory] boolValue])
		return [[NSUserDefaults standardUserDefaults] removeObjectForKey:pasteboardName];

	NSArray* tmp = [entries count] < 50 ? entries : [entries subarrayWithRange:NSMakeRange([entries count]-50, 50)];
	[[NSUserDefaults standardUserDefaults] setObject:[tmp valueForKey:@"asDictionary"] forKey:pasteboardName];
}

- (void)checkForExternalPasteboardChanges
{
	D(DBF_Pasteboard, bug("new data: %s (%zd = %zd)\n", BSTR((changeCount != [[self pasteboard] changeCount])), (ssize_t)changeCount, (ssize_t)[[self pasteboard] changeCount]););
	if(changeCount != [[self pasteboard] changeCount])
	{
		NSString* onClipboard = [[self pasteboard] availableTypeFromArray:@[ NSStringPboardType ]] ? [[self pasteboard] stringForType:NSStringPboardType] : nil;
		NSString* onStack = [([entries count] == 0 ? nil : [entries objectAtIndex:index]) string];
		changeCount = [[self pasteboard] changeCount];
		if((onClipboard && !onStack) || (onClipboard && onStack && ![onStack isEqualToString:onClipboard]))
		{
			id options = [[self pasteboard] availableTypeFromArray:@[ OakPasteboardOptionsPboardType ]] ? [[self pasteboard] propertyListForType:OakPasteboardOptionsPboardType] : nil;
			[entries addObject:[OakPasteboardEntry pasteboardEntryWithString:onClipboard andOptions:options]];
			index = [entries count]-1;
			self.auxiliaryOptionsForCurrent = nil;

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
	D(DBF_Pasteboard, bug("%zu → %zu\n", (size_t)index, (size_t)newIndex););
	if(index != newIndex)
	{
		index = newIndex;
		self.auxiliaryOptionsForCurrent = nil;
		if(OakPasteboardEntry* current = [entries count] == 0 ? nil : [entries objectAtIndex:index])
		{
			[[self pasteboard] declareTypes:@[ NSStringPboardType, OakPasteboardOptionsPboardType ] owner:nil];
			[[self pasteboard] setString:[current string] forType:NSStringPboardType];
			[[self pasteboard] setPropertyList:[current options] forType:OakPasteboardOptionsPboardType];
			changeCount = [[self pasteboard] changeCount];

			[[NSNotificationCenter defaultCenter] postNotificationName:OakPasteboardDidChangeNotification object:self];
		}
	}
}

- (id)initWithName:(NSString*)aName
{
	if(self = [super init])
	{
		pasteboardName = aName;

		entries = [NSMutableArray new];
		if(![[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsDisablePersistentClipboardHistory] boolValue])
		{
			if(NSArray* history = [[NSUserDefaults standardUserDefaults] arrayForKey:pasteboardName])
			{
				for(NSDictionary* entry in history)
				{
					NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:entry];
					[dict removeObjectForKey:@"string"];
					if(NSString* str = [entry objectForKey:@"string"])
						[entries addObject:[OakPasteboardEntry pasteboardEntryWithString:str andOptions:dict]];
				}

				index = [entries count]-1;
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
	if(![SharedInstances objectForKey:aName])
	{
		[SharedInstances setObject:[[OakPasteboard alloc] initWithName:aName] forKey:aName];
		if(![aName isEqualToString:NSGeneralPboard])
			[[SharedInstances objectForKey:aName] setAvoidsDuplicates:YES];
	}

	OakPasteboard* res = [SharedInstances objectForKey:aName];
	[res checkForExternalPasteboardChanges];
	return res;
}

- (void)addEntry:(OakPasteboardEntry*)anEntry
{
	D(DBF_Pasteboard, bug("%s (currently at %zu / %zu)\n", [[anEntry string] UTF8String], (size_t)index, (size_t)[entries count]););
	[self checkForExternalPasteboardChanges];
	if(avoidsDuplicates && [[entries lastObject] isEqual:anEntry])
	{
		index = [entries count]-1;
		self.auxiliaryOptionsForCurrent = nil;
	}
	else if(!avoidsDuplicates || ![[self current] isEqual:anEntry])
	{
		[entries addObject:anEntry];
		[self setIndex:[entries count]-1];
	}
	[self saveToDefaults];
}

- (OakPasteboardEntry*)previous
{
	D(DBF_Pasteboard, bug("%zu / %zu\n", (size_t)index, (size_t)[entries count]););
	[self checkForExternalPasteboardChanges];
	[self setIndex:index == 0 ? index : index-1];
	return [self current];
}

- (OakPasteboardEntry*)current
{
	D(DBF_Pasteboard, bug("%zu / %zu\n", (size_t)index, (size_t)[entries count]););
	[self checkForExternalPasteboardChanges];
	return [entries count] == 0 ? nil : [entries objectAtIndex:index];
}

- (OakPasteboardEntry*)next
{
	D(DBF_Pasteboard, bug("%zu / %zu\n", (size_t)index, (size_t)[entries count]););
	[self checkForExternalPasteboardChanges];
	[self setIndex:index+1 == [entries count] ? index : index+1];
	return [self current];
}

- (void)setEntries:(NSArray*)newEntries
{
	if(![newEntries isEqual:entries])
	{
		entries = [newEntries mutableCopy];
		index = [entries count]-1;
		self.auxiliaryOptionsForCurrent = nil;
		[[NSNotificationCenter defaultCenter] postNotificationName:OakPasteboardDidChangeNotification object:self];
		[self saveToDefaults];
	}
}

- (BOOL)selectItemAtPosition:(NSPoint)location withWidth:(CGFloat)width respondToSingleClick:(BOOL)singleClick
{
	[self checkForExternalPasteboardChanges];

	NSUInteger selectedRow = ([entries count]-1) - index;
	OakPasteboardSelector* pasteboardSelector = [OakPasteboardSelector sharedInstance];
	[pasteboardSelector setEntries:[[entries reverseObjectEnumerator] allObjects]];
	[pasteboardSelector setIndex:selectedRow];
	if(width)
		[pasteboardSelector setWidth:width];
	if(singleClick)
		[pasteboardSelector setPerformsActionOnSingleClick];
	selectedRow = [pasteboardSelector showAtLocation:location];

	[self setEntries:[[[pasteboardSelector entries] reverseObjectEnumerator] allObjects]];
	[self setIndex:([entries count]-1) - selectedRow];

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
