#import "GlobalHotkey.h"
#import <oak/misc.h>

static NSString* key (unichar code)
{
	return [NSString stringWithFormat:@"%C", code];
}

static NSDictionary<NSString*, NSNumber*>* const legacyMapping = @{
	@"a":                          @(kVK_ANSI_A),
	@"s":                          @(kVK_ANSI_S),
	@"d":                          @(kVK_ANSI_D),
	@"f":                          @(kVK_ANSI_F),
	@"h":                          @(kVK_ANSI_H),
	@"g":                          @(kVK_ANSI_G),
	@"z":                          @(kVK_ANSI_Z),
	@"x":                          @(kVK_ANSI_X),
	@"c":                          @(kVK_ANSI_C),
	@"v":                          @(kVK_ANSI_V),
	@"b":                          @(kVK_ANSI_B),
	@"q":                          @(kVK_ANSI_Q),
	@"w":                          @(kVK_ANSI_W),
	@"e":                          @(kVK_ANSI_E),
	@"r":                          @(kVK_ANSI_R),
	@"y":                          @(kVK_ANSI_Y),
	@"t":                          @(kVK_ANSI_T),
	@"1":                          @(kVK_ANSI_1),
	@"2":                          @(kVK_ANSI_2),
	@"3":                          @(kVK_ANSI_3),
	@"4":                          @(kVK_ANSI_4),
	@"6":                          @(kVK_ANSI_6),
	@"5":                          @(kVK_ANSI_5),
	@"9":                          @(kVK_ANSI_9),
	@"7":                          @(kVK_ANSI_7),
	@"8":                          @(kVK_ANSI_8),
	@"0":                          @(kVK_ANSI_0),
	@"o":                          @(kVK_ANSI_O),
	@"u":                          @(kVK_ANSI_U),
	@"i":                          @(kVK_ANSI_I),
	@"p":                          @(kVK_ANSI_P),
	@"l":                          @(kVK_ANSI_L),
	@"j":                          @(kVK_ANSI_J),
	@"k":                          @(kVK_ANSI_K),
	@"n":                          @(kVK_ANSI_N),
	@"m":                          @(kVK_ANSI_M),
	@"=":                          @(kVK_ANSI_Equal),
	@"-":                          @(kVK_ANSI_Minus),
	@"[":                          @(kVK_ANSI_RightBracket),
	@"]":                          @(kVK_ANSI_LeftBracket),
	@"'":                          @(kVK_ANSI_Quote),
	@";":                          @(kVK_ANSI_Semicolon),
	@"\\":                         @(kVK_ANSI_Backslash),
	@",":                          @(kVK_ANSI_Comma),
	@"/":                          @(kVK_ANSI_Slash),
	@".":                          @(kVK_ANSI_Period),
	@"`":                          @(kVK_ANSI_Grave),
	@"\r":                         @(kVK_Return),
	@"\t":                         @(kVK_Tab),
	@" ":                          @(kVK_Space),
	key(NSDeleteCharacter):        @(kVK_Delete),
	@"\e":                         @(kVK_Escape),
	key(NSF1FunctionKey):          @(kVK_F1),
	key(NSF2FunctionKey):          @(kVK_F2),
	key(NSF3FunctionKey):          @(kVK_F3),
	key(NSF4FunctionKey):          @(kVK_F4),
	key(NSF5FunctionKey):          @(kVK_F5),
	key(NSF6FunctionKey):          @(kVK_F6),
	key(NSF7FunctionKey):          @(kVK_F7),
	key(NSF8FunctionKey):          @(kVK_F8),
	key(NSF9FunctionKey):          @(kVK_F9),
	key(NSF10FunctionKey):         @(kVK_F10),
	key(NSF11FunctionKey):         @(kVK_F11),
	key(NSF12FunctionKey):         @(kVK_F12),
	key(NSF13FunctionKey):         @(kVK_F13),
	key(NSF14FunctionKey):         @(kVK_F14),
	key(NSF15FunctionKey):         @(kVK_F15),
	key(NSF16FunctionKey):         @(kVK_F16),
	key(NSF17FunctionKey):         @(kVK_F17),
	key(NSF18FunctionKey):         @(kVK_F18),
	key(NSF19FunctionKey):         @(kVK_F19),
	key(NSF20FunctionKey):         @(kVK_F20),
	key(NSHelpFunctionKey):        @(kVK_Help),
	key(NSDeleteFunctionKey):      @(kVK_ForwardDelete),
	key(NSHomeFunctionKey):        @(kVK_Home),
	key(NSEndFunctionKey):         @(kVK_End),
	key(NSPageUpFunctionKey):      @(kVK_PageUp),
	key(NSPageDownFunctionKey):    @(kVK_PageDown),
	key(NSLeftArrowFunctionKey):   @(kVK_LeftArrow),
	key(NSRightArrowFunctionKey):  @(kVK_RightArrow),
	key(NSDownArrowFunctionKey):   @(kVK_DownArrow),
	key(NSUpArrowFunctionKey):     @(kVK_UpArrow),
	@"#":                          @(kVK_Function),
	@"volumeUpKey":                @(kVK_VolumeUp),
	@"volumeDownKey":              @(kVK_VolumeDown),
	@"muteKey":                    @(kVK_Mute),
	key(NSClearLineFunctionKey):   @(kVK_ANSI_KeypadClear),
	key(NSEnterCharacter):         @(kVK_ANSI_KeypadEnter),
	@"#.":                         @(kVK_ANSI_KeypadDecimal),
	@"#*":                         @(kVK_ANSI_KeypadMultiply),
	@"#+":                         @(kVK_ANSI_KeypadPlus),
	@"#/":                         @(kVK_ANSI_KeypadDivide),
	@"#-":                         @(kVK_ANSI_KeypadMinus),
	@"#=":                         @(kVK_ANSI_KeypadEquals),
	@"#0":                         @(kVK_ANSI_Keypad0),
	@"#1":                         @(kVK_ANSI_Keypad1),
	@"#2":                         @(kVK_ANSI_Keypad2),
	@"#3":                         @(kVK_ANSI_Keypad3),
	@"#4":                         @(kVK_ANSI_Keypad4),
	@"#5":                         @(kVK_ANSI_Keypad5),
	@"#6":                         @(kVK_ANSI_Keypad6),
	@"#7":                         @(kVK_ANSI_Keypad7),
	@"#8":                         @(kVK_ANSI_Keypad8),
	@"#9":                         @(kVK_ANSI_Keypad9),
};

@interface GlobalHotkey ()
{
	EventHotKeyRef _hotkey_ref;
	UInt32 _identifier;
}
@end

namespace
{
	struct legacy_key_t
	{
		UInt32 modifiers;
		UInt32 code;
	};

	static legacy_key_t parse (NSString* str)
	{
		BOOL numeric = NO;
		UInt32 modifiers = 0;

		for(NSUInteger i = 0; i < str.length; ++i)
		{
			switch([str characterAtIndex:i])
			{
				case '$': modifiers |= shiftKey;   break;
				case '^': modifiers |= controlKey; break;
				case '~': modifiers |= optionKey;  break;
				case '@': modifiers |= cmdKey;     break;
				case '#': numeric = YES;           break;

				default:
				{
					NSString* code = [str substringFromIndex:i];
					if(numeric)
						code = [@"#" stringByAppendingString:code];

					if(NSNumber* legacy = legacyMapping[code])
						return { .modifiers = modifiers, .code = legacy.unsignedIntValue };

					return { };
				}
				break;
			}
		}
		return { };
	}
}

static std::map<UInt32, OSStatus(^)()> _blocks;

static OSStatus hotkey_callback (EventHandlerCallRef nextHandler, EventRef theEvent, void* userData)
{
   if(GetEventKind(theEvent) == kEventHotKeyPressed)
	{
      EventHotKeyID hotKeyID;
      GetEventParameter(theEvent, kEventParamDirectObject, typeEventHotKeyID, nullptr, sizeof(hotKeyID), nullptr, &hotKeyID);
		auto it = _blocks.find(hotKeyID.id);
		if(it != _blocks.end())
			it->second();

		if(nextHandler)
			CallNextEventHandler(nextHandler, theEvent);
	}
	return eventNotHandledErr;
}

@implementation GlobalHotkey
+ (instancetype)globalHotkeyForEventString:(NSString*)anEventString handler:(OSStatus(^)())callback
{
	return [[GlobalHotkey alloc] initWithEventString:anEventString handler:callback];
}

- (instancetype)initWithEventString:(NSString*)anEventString handler:(OSStatus(^)())callback
{
	if(self = [super init])
	{
		_eventString = anEventString;

		static dispatch_once_t onceToken;
		static EventHandlerRef eventHandler;

		dispatch_once(&onceToken, ^{
			EventTypeSpec eventType[] = {
				{ kEventClassKeyboard, kEventHotKeyPressed },
			};
			InstallApplicationEventHandler(&hotkey_callback, sizeofA(eventType), eventType, nullptr, &eventHandler);
		});

		static UInt32 lastHotkeyId = 0;
		EventHotKeyID hotKeyID = { 'hkey', ++lastHotkeyId };
		_blocks.emplace(lastHotkeyId, callback);

		EventHotKeyRef temp;
		legacy_key_t key = parse(anEventString);
		RegisterEventHotKey(key.code, key.modifiers, hotKeyID, GetApplicationEventTarget(), kEventHotKeyNoOptions, &temp);
		_hotkey_ref = temp;
	}
	return self;
}

- (void)dealloc
{
	UnregisterEventHotKey(_hotkey_ref);
	_blocks.erase(_identifier);
}
@end
