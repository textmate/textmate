#import "OakTabBarView.h"
#import "NSColor Additions.h"
#import "NSImage Additions.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/oak.h>
#import <text/format.h>
#import <regexp/format_string.h>
#import <ns/ns.h>

OAK_DEBUG_VAR(TabBarView);

NSString* const kUserDefaultsDisableTabBarCollapsingKey = @"disableTabBarCollapsing";
NSString* const OakTabBarViewTabType                    = @"OakTabBarViewTabType";

struct binding_info_t
{
	binding_info_t (std::string const& property, id controller, std::string const& key_path) : property(property), controller(controller), key_path(key_path) { }

	std::string property;
	id controller;
	std::string key_path;
};

struct value_t
{
	value_t (double v = 0)
	{
		records.push_back((record_t){ 0, 0, v, v });
	}

	double current (double t) const
	{
		double target = records.back().target;
		riterate(it, records)
			target = it->source + (target - it->source) * oak::slow_in_out((t - it->start) / it->duration);
		return round(target);
	}

	double set_time (double t)
	{
		while(records.size() > 1 && records.front().start + records.front().duration < t)
			records.erase(records.begin());
		return current(t);
	}

	void set_new_target (double target, double now, double duration = 1)
	{
		if(records.back().target != target)
			records.push_back((record_t){ now, duration, records.back().target, target });
	}
private:
	struct record_t { double start, duration, source, target; };
	std::vector<record_t> records;
};

namespace tab_bar_requisites
{
	uint32_t modified = layer_t::last_requisite << 0,
	         first    = layer_t::last_requisite << 1,
	         dragged  = layer_t::last_requisite << 2;

	uint32_t all      = (modified|first|dragged);
};

// ==================
// = Layout Metrics =
// ==================

struct layout_metrics_t
{
	double tabSpacing;
	double firstTabOffset;
	double minTabSize;
	double maxTabSize;

	std::vector<layer_t> layers_for (std::string const& layer_id, CGRect const& rect = CGRectZero, int tag = 0, NSString* label = nil, NSString* toolTip = nil, uint32_t requisiteFilter = layer_t::no_requisite) const;
	static layout_metrics_ptr parse (NSDictionary* dict);

private:
	struct raw_layer_t
	{
		raw_layer_t () : has_label(false), has_tool_tip(false) { }
		layer_t layer;
		bool has_label;
		bool has_tool_tip;
		std::vector<std::string> padding;
	};

	std::map<std::string, std::vector<raw_layer_t> > layers;

	static id ExpandVariables (id obj, std::map<std::string, std::string> const& someVariables);
	static void AddItemsForKeyToArray (NSDictionary* dict, NSString* key, std::map<std::string, std::string> const& values, NSMutableArray* array);

	static raw_layer_t parse_layer (NSDictionary* item);
	static uint32_t parse_requisite (char const* str);
	static uint32_t requisite_from_string (char const* str);
};

// ===========================================
// = Parsing and querying the layout metrics =
// ===========================================

std::vector<layer_t> layout_metrics_t::layers_for (std::string const& layer_id, CGRect const& rect, int tag, NSString* label, NSString* toolTip, uint32_t requisiteFilter) const
{
	std::vector<layer_t> res;

	std::map<std::string, std::vector<raw_layer_t> >::const_iterator l = layers.find(layer_id);
	if(l == layers.end())
		return res;

	iterate(it, l->second)
	{
		if((it->layer.requisite_mask & tab_bar_requisites::all) != layer_t::no_requisite) // layer is testing on OTBV requisite flags
		{
			if((it->layer.requisite & tab_bar_requisites::all) != (requisiteFilter & it->layer.requisite_mask))
				continue;
		}

		if(it->padding.size() != 4)
			continue;

		CGFloat values[4] = { };
		for(size_t i = 0; i < sizeofA(values); ++i)
		{
			std::string const& str = it->padding[i];
			switch(str.empty() ? '\0' : str[0])
			{
				case 'W': values[i] = CGRectGetWidth(rect)  + strtod(str.substr(1).c_str(), NULL); break;
				case 'H': values[i] = CGRectGetHeight(rect) + strtod(str.substr(1).c_str(), NULL); break;
				default:  values[i] = strtod(str.c_str(), NULL);               break;
			}
		}

		res.push_back(it->layer);
		res.back().requisite      &= ~tab_bar_requisites::all;
		res.back().requisite_mask &= ~tab_bar_requisites::all;

		if(it->has_label)
			res.back().text = label;
		if(it->has_tool_tip)
			res.back().tool_tip = toolTip;
		res.back().tag = tag;
		res.back().rect = CGRectMake(CGRectGetMinX(rect) + values[0], CGRectGetMinY(rect) + values[2], values[1]-values[0] + 1, values[3]-values[2] + 1);
	}
	return res;
}

id layout_metrics_t::ExpandVariables (id obj, std::map<std::string, std::string> const& someVariables)
{
	if([obj isKindOfClass:[NSArray class]])
	{
		NSMutableArray* array = [NSMutableArray array];
		for(id item in obj)
			[array addObject:ExpandVariables(item, someVariables)];
		obj = array;
	}
	else if([obj isKindOfClass:[NSDictionary class]])
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionary];
		iterate(it, (NSDictionary*)obj)
			[dict setObject:ExpandVariables(it->second, someVariables) forKey:it->first];
		obj = dict;
	}
	else if([obj isKindOfClass:[NSString class]])
	{
		obj = [NSString stringWithCxxString:format_string::expand([obj UTF8String], someVariables)];
	}
	return obj;
}

void layout_metrics_t::AddItemsForKeyToArray (NSDictionary* dict, NSString* key, std::map<std::string, std::string> const& values, NSMutableArray* array)
{
	for(NSDictionary* item in [dict objectForKey:key])
	{
		if(NSString* includeKey = [item objectForKey:@"include"])
		{
			std::map<std::string, std::string> tmp = values;
			iterate(it, (NSDictionary*)[item objectForKey:@"values"])
				tmp[[it->first UTF8String]] = format_string::expand([it->second UTF8String], tmp);
			AddItemsForKeyToArray(dict, includeKey, tmp, array);
		}
		else
		{
			[array addObject:ExpandVariables(item, values)];
		}
	}
}

layout_metrics_ptr layout_metrics_t::parse (NSDictionary* dict)
{
	layout_metrics_t r;

	r.tabSpacing     = [[dict objectForKey:@"tabSpacing"] floatValue];
	r.firstTabOffset = [[dict objectForKey:@"firstTabOffset"] floatValue];
	r.minTabSize     = [[dict objectForKey:@"minTabSize"] floatValue];
	r.maxTabSize     = [[dict objectForKey:@"maxTabSize"] floatValue] ?: DBL_MAX;

	std::map<std::string, std::string> variables;
	iterate(it, dict)
	{
		if([it->second isKindOfClass:[NSString class]])
			variables[[it->first UTF8String]] = [it->second UTF8String];
	}

	iterate(it, dict)
	{
		if(![it->second isKindOfClass:[NSArray class]])
			continue;

		NSMutableArray* array = [NSMutableArray array];
		AddItemsForKeyToArray(dict, it->first, variables, array);
		for(NSDictionary* item in array)
			r.layers[[it->first UTF8String]].push_back(parse_layer(item));
	}

	return layout_metrics_ptr(new layout_metrics_t(r));
}

uint32_t layout_metrics_t::requisite_from_string (char const* str)
{
	static struct { std::string name; uint32_t value; } mapping[] =
	{
		{ "no_requisite",         layer_t::no_requisite       },
		{ "mouse_inside",         layer_t::mouse_inside       },
		{ "mouse_down",           layer_t::mouse_down         },
		{ "mouse_dragged",        layer_t::mouse_dragged      },
		{ "mouse_clicked",        layer_t::mouse_clicked      },
		{ "mouse_double_clicked", layer_t::mouse_double_clicked      },
		{ "control",              layer_t::control            },
		{ "option",               layer_t::option             },
		{ "shift",                layer_t::shift              },
		{ "command",              layer_t::command            },
		{ "window_key",           layer_t::window_key         },
		{ "window_main",          layer_t::window_main        },
		{ "window_main_or_key",   layer_t::window_main_or_key },

		{ "modified",             tab_bar_requisites::modified},
		{ "first",                tab_bar_requisites::first   },
		{ "dragged",              tab_bar_requisites::dragged },
	};

	for(size_t i = 0; i < sizeofA(mapping); ++i)
	{
		if(mapping[i].name == str)
			return mapping[i].value;
	}
	ASSERTF(false, "unknown requisite: %s\n", str);
	return layer_t::no_requisite;
}

uint32_t layout_metrics_t::parse_requisite (char const* str)
{
	uint32_t res = 0;

	char* mutableStr = strdup(str);
	char* arr[] = { mutableStr };
	char* req;
	while((req = strsep(arr, "|")) && *req)
		res |= requisite_from_string(req);
	free(mutableStr);
	return res;
}

layout_metrics_t::raw_layer_t layout_metrics_t::parse_layer (NSDictionary* item)
{
	raw_layer_t res;

	if(NSString* color = [item objectForKey:@"color"])
		res.layer.color = [NSColor colorWithString:color];
	if(NSString* requisite = [item objectForKey:@"requisite"])
		res.layer.requisite = parse_requisite([requisite UTF8String]);
	res.layer.requisite_mask = res.layer.requisite;
	if(NSString* requisiteMask = [item objectForKey:@"requisiteMask"])
		res.layer.requisite_mask = parse_requisite([requisiteMask UTF8String]);
	if(NSString* action = [item objectForKey:@"action"])
		res.layer.action = NSSelectorFromString(action);
	if([[item objectForKey:@"preventWindowOrdering"] boolValue])
		res.layer.prevent_window_ordering = true;
	if(NSDictionary* textOptions = [item objectForKey:@"text"]) // TODO we probably want to read some text options…
	{
		res.has_label = true;
		if([[textOptions objectForKey:@"shadow"] boolValue])
			res.layer.text_options = res.layer.text_options | layer_t::shadow;
	}
	if([[item objectForKey:@"toolTip"] boolValue])
		res.has_tool_tip = true;
	if(NSString* imageName = [item objectForKey:@"image"]) // TODO we probably want to read some image options…
		res.layer.image = [NSImage imageNamed:imageName inSameBundleAsClass:[OakTabBarView class]];

	for(NSString* pos in [item objectForKey:@"rect"])
		res.padding.push_back([pos UTF8String]);

	return res;
}

// ===========================================

static id SafeObjectAtIndex (NSArray* array, NSUInteger index)
{
	return (index < [array count] && [array objectAtIndex:index] != [NSNull null]) ? [array objectAtIndex:index] : nil;
}

@interface OakTabBarView ()
- (void)updateLayout;
@property (nonatomic, retain) OakTimer* slideAroundAnimationTimer;
@property (nonatomic, assign) BOOL layoutNeedsUpdate;
@end

@implementation OakTabBarView
@synthesize isExpanded, delegate, dataSource, slideAroundAnimationTimer, layoutNeedsUpdate;

- (BOOL)performKeyEquivalent:(NSEvent*)anEvent
{
	// this should be in the window controller, but there we need subclassing mojo to get key events
	std::string const keyStr = to_s(anEvent);
	if(keyStr == "~@\uF702")
		return [NSApp sendAction:@selector(selectPreviousTab:) to:nil from:self];
	else if(keyStr == "~@\uF703")
		return [NSApp sendAction:@selector(selectNextTab:) to:nil from:self];
	return NO;
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		metrics           = layout_metrics_t::parse([NSDictionary dictionaryWithContentsOfFile:[[NSBundle bundleForClass:[self class]] pathForResource:@"TabBar" ofType:@"plist"]]);
		hiddenTab         = NSNotFound;
		tabTitles         = [NSMutableArray new];
		tabToolTips       = [NSMutableArray new];
		tabModifiedStates = [NSMutableArray new];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(viewFrameChanged:) name:NSViewFrameDidChangeNotification object:self];
		[self registerForDraggedTypes:@[ OakTabBarViewTabType ]];
	}
	return self;
}

- (void)dealloc
{
	self.slideAroundAnimationTimer = nil;

	[[NSNotificationCenter defaultCenter] removeObserver:self name:NSViewFrameDidChangeNotification object:self];

	iterate(it, bindings)
	{
		[it->controller removeObserver:self forKeyPath:[NSString stringWithCxxString:it->key_path]];
		[it->controller release];
	}

	[tabTitles release];
	[tabToolTips release];
	[tabModifiedStates release];
	[super dealloc];
}

- (void)viewDidMoveToWindow
{
	self.layoutNeedsUpdate = YES;
	[super viewDidMoveToWindow];
}

- (std::string const&)layerNameForTabIndex:(NSUInteger)tabIndex
{
	static std::string str;
	str = "tab";
	if(tabIndex == selectedTab)
		str += "Selected";
	return str;
}

- (uint32_t)filterForTabIndex:(NSUInteger)tabIndex
{
	uint32_t filter = 0;
	if([SafeObjectAtIndex(tabModifiedStates, tabIndex) boolValue])
		filter |= tab_bar_requisites::modified;
	if(tabIndex == 0)
		filter |= tab_bar_requisites::first;
	return filter;
}

- (double)putDefaultTabWidthsInto:(std::vector<double>&)tabSizes
{
	double totalWidth = 0;
	for(NSUInteger tabIndex = 0; tabIndex < tabTitles.count; ++tabIndex)
	{
		double width = WidthOfText(SafeObjectAtIndex(tabTitles, tabIndex));
		citerate(it, metrics->layers_for([self layerNameForTabIndex:tabIndex], CGRectZero, tabIndex, @"LabelPlaceholder"))
		{
			if(it->text)
				width -= it->rect.size.width - 1;
		}
		width = oak::cap(metrics->minTabSize, width, metrics->maxTabSize);
		tabSizes.push_back(width);
		totalWidth += width;
	}
	return totalWidth;
}

- (NSUInteger)countOfVisibleTabs
{
	NSRect rect = NSInsetRect([self bounds], metrics->firstTabOffset, 0);
	NSUInteger maxNumberOfTabs = floor((NSWidth(rect) + metrics->tabSpacing) / (metrics->minTabSize + metrics->tabSpacing));
	return std::max<NSUInteger>(maxNumberOfTabs, 8);
}

- (void)updateLayout
{
	NSRect rect = [self bounds];
	tabRects.clear();

	D(DBF_TabBarView, bug("\n"););
	if(!isExpanded)
		return [self setLayout:metrics->layers_for("backgroundCollapsed", rect)];

	std::vector<layer_t> newLayout, selectedTabLayers;
	newLayout = metrics->layers_for("background", rect);

	// ==========

	std::vector<double> tabSizes;
	double totalWidth = [self putDefaultTabWidthsInto:tabSizes];

	rect.origin.x   += 1 * metrics->firstTabOffset;
	rect.size.width -= 2 * metrics->firstTabOffset;

	size_t numberOfTabs = tabSizes.size();
	if(NSWidth(rect) < totalWidth + (tabSizes.size()-1)*metrics->tabSpacing)
	{
		size_t maxNumberOfTabs = floor((NSWidth(rect) + metrics->tabSpacing) / (metrics->minTabSize + metrics->tabSpacing));
		if(numberOfTabs > maxNumberOfTabs)
		{
			numberOfTabs = maxNumberOfTabs ?: 1;
			tabSizes.resize(numberOfTabs);
			totalWidth = 0;
			iterate(it, tabSizes)
				totalWidth += *it;
		}

		double fat  = totalWidth - (numberOfTabs * metrics->minTabSize);
		double cut  = totalWidth - (NSWidth(rect) - ((numberOfTabs-1) * metrics->tabSpacing));
		double keep = fat - cut;

		double aggregatedFat = 0;
		for(size_t i = 0; i < numberOfTabs; ++i)
		{
			double from    = floor(aggregatedFat * keep/fat);
			aggregatedFat += tabSizes[i] - metrics->minTabSize;
			double to      = floor(aggregatedFat * keep/fat);
			tabSizes[i]    = metrics->minTabSize + to - from;
		}
	}

	// ==========

	for(NSUInteger tabIndex = 0; tabIndex < numberOfTabs; ++tabIndex)
	{
		rect.origin.x += tabDropSpacing.find(tabIndex) != tabDropSpacing.end() ? tabDropSpacing[tabIndex].set_time(CFAbsoluteTimeGetCurrent()) : 0;
		if(tabIndex == hiddenTab)
		{
			tabRects.push_back(NSZeroRect);
			continue;
		}

		rect.size.width = tabSizes[tabIndex];

		std::string layer_id  = [self layerNameForTabIndex:tabIndex];
		NSString* toolTipText = SafeObjectAtIndex(tabToolTips, tabIndex);
		NSString* title       = SafeObjectAtIndex(tabTitles, tabIndex);

		std::vector<layer_t> const& layers = metrics->layers_for(layer_id, rect, tabIndex, title, toolTipText, [self filterForTabIndex:tabIndex]);
		if(tabIndex == selectedTab)
				selectedTabLayers.insert(selectedTabLayers.end(), layers.begin(), layers.end());
		else	newLayout.insert(newLayout.end(), layers.begin(), layers.end());

		tabRects.push_back(rect);
		rect.origin.x += rect.size.width + metrics->tabSpacing;
	}
	newLayout.insert(newLayout.end(), selectedTabLayers.begin(), selectedTabLayers.end());
	[self setLayout:newLayout];
}

- (void)viewFrameChanged:(NSNotification*)aNotification
{
	self.layoutNeedsUpdate = YES;
}

- (void)setIsExpanded:(BOOL)flag
{
	if(isExpanded == flag)
		return;

	CGFloat delta = isExpanded ? 1-NSHeight(self.frame) : 23-NSHeight(self.frame);
	isExpanded = flag;

	BOOL visible = NO/*[[self window] isVisible]*/;
	for(NSView* view in [[self superview] subviews])
	{
		D(DBF_TabBarView, bug("%s: %s\n", view.description.UTF8String, NSStringFromRect(view.frame).UTF8String););
		NSRect frame = view.frame;
		if(view == self)
		{
			frame.origin.y -= delta;
			frame.size.height += delta;
		}
		else
		{
			frame.size.height -= delta;
		}
		[(visible ? [view animator] : view) setFrame:frame];
	}
	self.layoutNeedsUpdate = YES;
}

- (void)selectTab:(id)sender
{
	if(self.tag != selectedTab)
	{
		if(delegate && [delegate respondsToSelector:@selector(tabBarView:shouldSelectIndex:)] && ![delegate tabBarView:self shouldSelectIndex:self.tag])
			return;

		selectedTab = self.tag;
		iterate(it, bindings)
		{
			if(it->property == "selectionIndexes")
				[it->controller setValue:[NSIndexSet indexSetWithIndex:selectedTab] forKey:[NSString stringWithCxxString:it->key_path]];
		}
		self.layoutNeedsUpdate = YES;
	}
}

- (void)didDoubleClickTab:(id)sender
{
	if([delegate respondsToSelector:@selector(tabBarView:didDoubleClickIndex:)])
		[delegate tabBarView:self didDoubleClickIndex:selectedTab];
}

- (void)didDoubleClickTabBar:(id)sender
{
	if([delegate respondsToSelector:@selector(tabBarViewDidDoubleClick:)])
		[delegate tabBarViewDidDoubleClick:self];
}

- (void)performClose:(id)sender
{
	D(DBF_TabBarView, bug("\n"););
	tag = selectedTab; // performCloseTab: asks for [sender tag]
	[NSApp sendAction:@selector(performCloseTab:) to:nil from:self];
}

- (void)setSelectedTab:(NSUInteger)anIndex
{
	if(selectedTab == anIndex)
		return;
	selectedTab = anIndex;
	self.layoutNeedsUpdate = YES;
}

- (void)reloadData
{
	if(!dataSource)
		return;

	NSMutableArray* titles         = [NSMutableArray array];
	NSMutableArray* toolTips       = [NSMutableArray array];
	NSMutableArray* modifiedStates = [NSMutableArray array];

	NSUInteger count = [dataSource numberOfRowsInTabBarView:self];
	for(NSUInteger i = 0; i < count; ++i)
	{
		[titles addObject:[dataSource tabBarView:self titleForIndex:i]];
		[toolTips addObject:[dataSource tabBarView:self toolTipForIndex:i]];
		[modifiedStates addObject:@([dataSource tabBarView:self isEditedAtIndex:i])];
	}

	[tabTitles setArray:titles];
	[tabToolTips setArray:toolTips];
	[tabModifiedStates setArray:modifiedStates];

	selectedTab = [tabToolTips count] && selectedTab != NSNotFound ? std::min(selectedTab, [tabToolTips count]-1) : NSNotFound;

	BOOL newIsExpanded = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableTabBarCollapsingKey] ? YES : [tabTitles count] > 1;
	if(newIsExpanded != self.isExpanded)
			self.isExpanded = newIsExpanded;
	else	self.layoutNeedsUpdate = YES;
}

// ============
// = Dragging =
// ============

- (void)drawRect:(NSRect)aRect
{
	if(layoutNeedsUpdate)
		[self updateLayout];
	self.layoutNeedsUpdate = NO;
	[super drawRect:aRect];
}

- (void)setLayoutNeedsUpdate:(BOOL)flag
{
	if(layoutNeedsUpdate == flag)
		return;
	if(layoutNeedsUpdate = flag)
		[self setNeedsDisplay:YES];
}

- (void)hideTabAtIndex:(NSUInteger)anIndex
{
	if(hiddenTab == anIndex)
		return;
	hiddenTab = anIndex;
	self.layoutNeedsUpdate = YES;
}

- (void)setDropAreaWidth:(CGFloat)aWidth beforeTabAtIndex:(NSUInteger)anIndex animate:(BOOL)flag
{
	double t = CFAbsoluteTimeGetCurrent();
	double duration = flag ? (([[NSApp currentEvent] modifierFlags] & NSShiftKeyMask) == NSShiftKeyMask ? 3 : 0.5) : 0;
	for(NSUInteger i = 0; i < tabRects.size(); ++i)
		tabDropSpacing[i].set_new_target(i == anIndex ? aWidth + metrics->tabSpacing : 0, t, duration);

	self.slideAroundAnimationTimer = flag ? (self.slideAroundAnimationTimer ?: [OakTimer scheduledTimerWithTimeInterval:0.02 target:self selector:@selector(updateSlideAroundAnimation:) repeats:YES]) : nil;
	if(aWidth == 0 && !flag)
		tabDropSpacing.clear();

	self.layoutNeedsUpdate = YES;
}

- (void)updateSlideAroundAnimation:(NSTimer*)aTimer
{
	self.layoutNeedsUpdate = YES;
}

- (NSUInteger)dropIndexForMouse:(NSPoint)mousePos
{
	NSRect rect = [self bounds];
	rect.origin.x += metrics->firstTabOffset;
	NSUInteger tabIndex = 0;
	for(; tabIndex < tabRects.size(); ++tabIndex)
	{
		if(tabIndex == hiddenTab)
			continue;
		rect.size.width = NSWidth(tabRects[tabIndex]);
		if(mousePos.x < NSMaxX(rect))
			break;
		rect.origin.x += rect.size.width + metrics->tabSpacing;
	}
	return tabIndex;
}

- (void)dragTab:(id)sender
{
	NSUInteger draggedTab = self.tag;
	NSRect tabRect = tabRects[draggedTab];

	NSImage* image = [[[NSImage alloc] initWithSize:tabRect.size] autorelease];
	[image lockFocus];

	uint32_t state = [self currentState] | layer_t::mouse_inside | layer_t::mouse_down;
	citerate(it, metrics->layers_for([self layerNameForTabIndex:draggedTab], (NSRect){NSZeroPoint, tabRect.size}, draggedTab, [tabTitles objectAtIndex:draggedTab], nil, [self filterForTabIndex:draggedTab] | tab_bar_requisites::dragged))
	{
		if((state & it->requisite_mask) == it->requisite)
			[self drawLayer:*it];
	}
	[image unlockFocus];

	NSImage* dragImage = [[[NSImage alloc] initWithSize:image.size] autorelease];
	[dragImage lockFocus];
	[image drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeCopy fraction:0.8];
	[dragImage unlockFocus];

	NSPasteboard* pboard = [NSPasteboard pasteboardWithName:NSDragPboard];
	[pboard declareTypes:@[ OakTabBarViewTabType ] owner:self];
	[pboard setString:[NSString stringWithFormat:@"%lu", draggedTab] forType:OakTabBarViewTabType];
	[delegate setupPasteboard:pboard forTabAtIndex:draggedTab];

	[self hideTabAtIndex:draggedTab];
	[self setDropAreaWidth:[dragImage size].width beforeTabAtIndex:draggedTab animate:NO];
	self.mouseTrackingDisabled = YES;

	[self dragImage:dragImage
                at:tabRect.origin
            offset:NSZeroSize
             event:[NSApp currentEvent]
        pasteboard:[NSPasteboard pasteboardWithName:NSDragPboard]
            source:self
         slideBack:YES];
}

- (NSDragOperation)draggingSourceOperationMaskForLocal:(BOOL)isLocal
{
	return NSDragOperationMove|NSDragOperationCopy|NSDragOperationLink;
}

- (void)draggedImage:(NSImage*)image endedAt:(NSPoint)point operation:(NSDragOperation)operation
{
	[self hideTabAtIndex:NSNotFound];
	[self setDropAreaWidth:0 beforeTabAtIndex:NSNotFound animate:NO];
	self.mouseTrackingDisabled = NO;
}

- (BOOL)performDragOperation:(id <NSDraggingInfo>)sender
{
	[self hideTabAtIndex:NSNotFound];
	[self setDropAreaWidth:0 beforeTabAtIndex:NSNotFound animate:NO];

	NSDragOperation mask = [sender draggingSourceOperationMask];
	NSPoint mousePos = [self convertPoint:[sender draggingLocation] fromView:nil];
	BOOL success = [delegate performTabDropFromTabBar:[sender draggingSource]
                                             atIndex:[self dropIndexForMouse:mousePos]
                                      fromPasteboard:[NSPasteboard pasteboardWithName:NSDragPboard]
                                           operation:(mask & NSDragOperationMove) ?: (mask & NSDragOperationCopy)];
	return success;
}

- (NSDragOperation)draggingUpdated:(id <NSDraggingInfo>)sender
{
	NSDragOperation operation = [sender draggingSourceOperationMask];
	operation = (operation & NSDragOperationMove) ?: (operation & NSDragOperationCopy);
	if(operation == NSDragOperationNone)
		return operation;

	if([sender draggingSource] == self)
		[self hideTabAtIndex:operation == NSDragOperationCopy ? NSNotFound : [[[sender draggingPasteboard] stringForType:OakTabBarViewTabType] intValue]];

	NSPoint mousePos = [self convertPoint:[sender draggingLocation] fromView:nil];
	[self setDropAreaWidth:[[sender draggedImage] size].width beforeTabAtIndex:[self dropIndexForMouse:mousePos] animate:YES];

	return operation;
}

- (void)draggingExited:(id <NSDraggingInfo>)sender
{
	[self setDropAreaWidth:0 beforeTabAtIndex:NSNotFound animate:YES];
}

// ============
// = Bindings =
// ============

- (void)bind:(NSString*)aBinding toObject:(id)observableController withKeyPath:(NSString*)aKeyPath options:(NSDictionary*)someOptions
{
	bindings.push_back(binding_info_t([aBinding UTF8String], [observableController retain], [aKeyPath UTF8String]));
	[observableController addObserver:self forKeyPath:aKeyPath options:NSKeyValueObservingOptionInitial context:NULL];
}

- (void)unbind:(NSString*)aBinding
{
	for(size_t i = bindings.size(); i > 0; --i)
	{
		binding_info_t const& info = bindings[i-1];
		if(info.property != [aBinding UTF8String])
			continue;

		[info.controller removeObserver:self forKeyPath:[NSString stringWithCxxString:info.key_path]];
		[info.controller release];
		bindings.erase(bindings.begin() + (i-1));
	}
}

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)observableController change:(NSDictionary*)changeDictionary context:(void*)userData
{
	iterate(it, bindings)
	{
		if(it->controller != observableController || it->key_path != [aKeyPath UTF8String])
			continue;

		if(it->property == "value")
			[tabTitles setArray:[observableController valueForKeyPath:aKeyPath]];
		else if(it->property == "toolTip")
			[tabToolTips setArray:[observableController valueForKeyPath:aKeyPath]];
		else if(it->property == "selectionIndexes")
			selectedTab = [[observableController valueForKeyPath:aKeyPath] firstIndex];
		else if(it->property == "isEdited")
			[tabModifiedStates setArray:[observableController valueForKeyPath:aKeyPath]];
	}

	BOOL newIsExpanded = [tabTitles count] > 1;
	if(newIsExpanded != self.isExpanded)
		self.isExpanded = newIsExpanded;
	else
		self.layoutNeedsUpdate = YES;
}
@end
