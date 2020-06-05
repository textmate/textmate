#import "MenuBuilder.h"

static NSString* MBInternalDumpMenu (NSMenu* menu, NSInteger indent)
{
	static std::map<unichar, NSString*> functionKeys = {
		{ NSEnterCharacter,             @"NSEnterCharacter"          },
		{ NSBackspaceCharacter,         @"NSBackspaceCharacter"      },
		{ NSTabCharacter,               @"NSTabCharacter"            },
		{ NSNewlineCharacter,           @"NSNewlineCharacter"        },
		{ NSFormFeedCharacter,          @"NSFormFeedCharacter"       },
		{ NSCarriageReturnCharacter,    @"NSCarriageReturnCharacter" },
		{ NSBackTabCharacter,           @"NSBackTabCharacter"        },
		{ NSDeleteCharacter,            @"NSDeleteCharacter"         },
		{ NSUpArrowFunctionKey,         @"NSUpArrowFunctionKey"      },
		{ NSDownArrowFunctionKey,       @"NSDownArrowFunctionKey"    },
		{ NSLeftArrowFunctionKey,       @"NSLeftArrowFunctionKey"    },
		{ NSRightArrowFunctionKey,      @"NSRightArrowFunctionKey"   },
		{ NSF1FunctionKey,              @"NSF1FunctionKey"           },
		{ NSF2FunctionKey,              @"NSF2FunctionKey"           },
		{ NSF3FunctionKey,              @"NSF3FunctionKey"           },
		{ NSF4FunctionKey,              @"NSF4FunctionKey"           },
		{ NSF5FunctionKey,              @"NSF5FunctionKey"           },
		{ NSF6FunctionKey,              @"NSF6FunctionKey"           },
		{ NSF7FunctionKey,              @"NSF7FunctionKey"           },
		{ NSF8FunctionKey,              @"NSF8FunctionKey"           },
		{ NSF9FunctionKey,              @"NSF9FunctionKey"           },
		{ NSF10FunctionKey,             @"NSF10FunctionKey"          },
		{ NSF11FunctionKey,             @"NSF11FunctionKey"          },
		{ NSF12FunctionKey,             @"NSF12FunctionKey"          },
		{ NSInsertFunctionKey,          @"NSInsertFunctionKey"       },
		{ NSDeleteFunctionKey,          @"NSDeleteFunctionKey"       },
		{ NSHomeFunctionKey,            @"NSHomeFunctionKey"         },
		{ NSBeginFunctionKey,           @"NSBeginFunctionKey"        },
		{ NSEndFunctionKey,             @"NSEndFunctionKey"          },
		{ NSPageUpFunctionKey,          @"NSPageUpFunctionKey"       },
		{ NSPageDownFunctionKey,        @"NSPageDownFunctionKey"     },
		{ NSHelpFunctionKey,            @"NSHelpFunctionKey"         },
	};

	struct row_t
	{
		NSString* title = nil;
		NSString* action = nil;
		NSString* keyEquivalent = nil;
		NSMutableArray* other = [NSMutableArray array];
		BOOL hasSubMenu = NO;
		BOOL isSeparator = NO;
		NSString* submenu = nil;
	};

	std::vector<row_t> rows;

	for(NSMenuItem* item in menu.itemArray)
	{
		if(item.isSeparatorItem)
		{
			rows.push_back({ .isSeparator = YES });
			continue;
		}

		row_t row;
		row.title  = item.title;
		row.action = item.action ? NSStringFromSelector(item.action) : nil;

		if(item.keyEquivalent && ![item.keyEquivalent isEqualToString:@""])
		{
			auto it = functionKeys.find([item.keyEquivalent characterAtIndex:0]);
			if(it != functionKeys.end())
					[row.other addObject:[NSString stringWithFormat:@".key = %@", it->second]];
			else	row.keyEquivalent = item.keyEquivalent;

			NSUInteger flags = item.keyEquivalentModifierMask;
			BOOL includeFlags = YES;
			if(flags == NSEventModifierFlagCommand)
				includeFlags = NO;

			if(flags == (NSEventModifierFlagShift|NSEventModifierFlagCommand) && item.keyEquivalent.length == 1)
			{
				unichar ch = [item.keyEquivalent characterAtIndex:0];
				if(ch < 0x80 && isalpha(ch))
					includeFlags = NO;
			}

			if(includeFlags)
			{
				struct modifier_mapping_t
				{
					NSUInteger flag;
					NSString* symbol;
				};

				modifier_mapping_t mappings[] = {
					{ NSEventModifierFlagCommand, @"NSEventModifierFlagCommand" },
					{ NSEventModifierFlagShift,   @"NSEventModifierFlagShift"   },
					{ NSEventModifierFlagOption,  @"NSEventModifierFlagOption"  },
					{ NSEventModifierFlagControl, @"NSEventModifierFlagControl" },
				};

				NSMutableArray* array = [NSMutableArray array];
				for(auto const& mapping : mappings)
				{
					if(flags & mapping.flag)
						[array addObject:mapping.symbol];
				}

				[row.other addObject:[NSString stringWithFormat:@".modifierFlags = %@", array.count ? [array componentsJoinedByString:@"|"] : @"0"]];
			}
		}

		if(item.action == @selector(submenuAction:) && item.submenu)
		{
			row.action = nil;
		}
		else
		{
			if(item.target == NSApp)
				[row.other addObject:@".target = NSApp"];
			else if(item.target == NSFontManager.sharedFontManager)
				[row.other addObject:@".target = NSFontManager.sharedFontManager"];
			else if(item.target == NSApp.delegate)
				[row.other addObject:@".target = NSApp.delegate"];
			else if(item.target != nil)
				[row.other addObject:@".target = «unknown»"];
		}

		if(item.tag)
			[row.other addObject:[NSString stringWithFormat:@".tag = %ld", item.tag]];
		if(item.isAlternate)
			[row.other addObject:@".alternate = YES"];
		if(item.isEnabled == NO)
			[row.other addObject:@".enabled = NO"];
		if(item.indentationLevel)
			[row.other addObject:[NSString stringWithFormat:@".indent = %ld", item.indentationLevel]];
		if(item.isHidden)
			[row.other addObject:@".hidden = YES"];
		if(item.image)
			[row.other addObject:@".image = «unknown»"];
		if(item.state)
			[row.other addObject:[NSString stringWithFormat:@".state = %ld", item.state]];
		if(item.representedObject)
			[row.other addObject:@".representedObject = «unknown»"];

		if(NSMenu* submenu = item.submenu)
		{
			if(submenu == NSApp.servicesMenu)
				[row.other addObject:@".systemMenu = MBMenuTypeServices"];
			else if(submenu == [NSFontManager.sharedFontManager fontMenu:NO])
				[row.other addObject:@".systemMenu = MBMenuTypeFont"];
			else if(submenu == NSApp.windowsMenu)
				[row.other addObject:@".systemMenu = MBMenuTypeWindows"];
			else if(submenu == NSApp.helpMenu)
				[row.other addObject:@".systemMenu = MBMenuTypeHelp"];
			else if(submenu.itemArray.firstObject.action == @selector(clearRecentDocuments:))
				[row.other addObject:@".systemMenu = MBMenuTypeOpenRecent"];
			else if(submenu.delegate)
				[row.other addObject:@".delegate = «unknown»"];

			row.submenu = MBInternalDumpMenu(submenu, indent + 2);
		}

		rows.push_back(row);
	}

	// ================
	// = Pretty Print =
	// ================

	for(auto& row : rows)
	{
		row.title = [NSString stringWithFormat:@"@\"%@\"", row.title];
		if(row.action || row.keyEquivalent || row.other.count)
			row.title = [row.title stringByAppendingString:@", "];

		if(row.action)
		{
			row.action = [NSString stringWithFormat:@"@selector(%@)", row.action];
			if(row.keyEquivalent || row.other.count)
				row.action = [row.action stringByAppendingString:@", "];
		}
		else if(row.keyEquivalent)
		{
			row.action = @"NULL, ";
		}
		else
		{
			row.action = @"";
		}

		if(row.keyEquivalent)
		{
			row.keyEquivalent = [NSString stringWithFormat:@"@\"%@\"", row.keyEquivalent];
			if(row.other.count)
				row.keyEquivalent = [row.keyEquivalent stringByAppendingString:@", "];
		}
		else
		{
			row.keyEquivalent = @"";
		}
	}

	NSUInteger columnWidths[3] = { };
	for(auto const& row : rows)
	{
		if(row.isSeparator || row.submenu)
			continue;

		columnWidths[0] = std::max([row.title lengthOfBytesUsingEncoding:NSUTF32StringEncoding]/4, columnWidths[0]);
		columnWidths[1] = std::max([row.action lengthOfBytesUsingEncoding:NSUTF32StringEncoding]/4, columnWidths[1]);
		columnWidths[2] = std::max([row.keyEquivalent lengthOfBytesUsingEncoding:NSUTF32StringEncoding]/4, columnWidths[2]);
	}

	for(auto& row : rows)
	{
		if(row.isSeparator || row.submenu)
			continue;

		row.title         = [row.title stringByPaddingToLength:columnWidths[0] withString:@" " startingAtIndex:0];
		row.action        = [row.action stringByPaddingToLength:columnWidths[1] withString:@" " startingAtIndex:0];
		row.keyEquivalent = [row.keyEquivalent stringByPaddingToLength:columnWidths[2] withString:@" " startingAtIndex:0];
	}

	std::vector<char> v(indent, '\t');

	NSMutableString* res = [NSMutableString string];
	for(auto const& row : rows)
	{
		if(row.isSeparator)
		{
			[res appendFormat:@"%.*s{ /* -------- */ },\n", (int)v.size(), v.data()];
		}
		else if(row.submenu)
		{
			[res appendFormat:
				@"%.*s{ %@%@%@%@\n%.*s\t%@%@.submenu = {\n%@%.*s\t}\n%.*s},\n",
				(int)v.size(), v.data(), 
				row.title, row.action, row.keyEquivalent, 
				row.other.count ? @"" : @",",
				(int)v.size(), v.data(), 
				[row.other componentsJoinedByString:@", "], 
				row.other.count ? @", " : @"",
				row.submenu, (int)v.size(), v.data(), (int)v.size(), v.data()];
		}
		else
		{
			[res appendFormat:@"%.*s{ %@%@%@%@ },\n", (int)v.size(), v.data(), row.title, row.action, row.keyEquivalent, [row.other componentsJoinedByString:@", "]];
		}
	}
	return res;
}

NSString* MBDumpMenu (NSMenu* menu)
{
	return [NSString stringWithFormat:@"MBMenu const items = {\n%@};\n", MBInternalDumpMenu(menu, 1)];
}
