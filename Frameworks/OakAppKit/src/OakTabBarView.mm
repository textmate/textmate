#import "OakTabBarView.h"
#import "OakControl Private.h"
#import "NSColor Additions.h"
#import "NSImage Additions.h"
#import "NSMenuItem Additions.h"
#import "NSView Additions.h"
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/NSArray Additions.h>
#import <oak/oak.h>
#import <text/format.h>
#import <regexp/format_string.h>
#import <ns/ns.h>

OAK_DEBUG_VAR(TabBarView);

NSString* const kUserDefaultsDisableTabBarCollapsingKey = @"disableTabBarCollapsing";
NSString* const OakTabBarViewTabType                    = @"OakTabBarViewTabType";

struct value_t
{
	value_t (double v = 0);
	double current (double t) const;
	double set_time (double t);
	void set_new_target (double target, double now, double duration = 1);

private:
	struct record_t { double start, duration, source, target; };
	std::vector<record_t> records;
};

value_t::value_t (double v)
{
	records.push_back((record_t){ 0, 0, v, v });
}

double value_t::current (double t) const
{
	double target = records.back().target;
	riterate(it, records)
		target = it->source + (target - it->source) * oak::slow_in_out((t - it->start) / it->duration);
	return round(target);
}

double value_t::set_time (double t)
{
	while(records.size() > 1 && records.front().start + records.front().duration < t)
		records.erase(records.begin());
	return current(t);
}

void value_t::set_new_target (double target, double now, double duration)
{
	if(records.back().target != target)
		records.push_back((record_t){ now, duration, records.back().target, target });
}

namespace tab_bar_requisites
{
	uint32_t modified = layer_t::last_requisite << 0,
	         first    = layer_t::last_requisite << 1,
	         dragged  = layer_t::last_requisite << 2,
	         overflow = layer_t::last_requisite << 3;

	uint32_t all      = (modified|first|dragged|overflow);
};

// ==================
// = Layout Metrics =
// ==================

struct layout_metrics_t;
typedef std::shared_ptr<layout_metrics_t> layout_metrics_ptr;

struct layout_metrics_t
{
	double tabSpacing;
	double firstTabOffset;
	double minTabSize;
	double maxTabSize;

	std::vector<layer_t> layers_for (std::string const& layer_id, CGRect const& rect, int tag, NSString* label = nil, NSString* toolTip = nil, uint32_t requisiteFilter = layer_t::no_requisite) const;
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
		for(NSString* key in obj)
			[dict setObject:ExpandVariables(obj[key], someVariables) forKey:key];
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
			NSDictionary* values = [item objectForKey:@"values"];
			for(NSString* key in values)
				tmp[[key UTF8String]] = format_string::expand([values[key] UTF8String], tmp);
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
	for(NSString* key in dict)
	{
		if([dict[key] isKindOfClass:[NSString class]])
			variables[[key UTF8String]] = [dict[key] UTF8String];
	}

	for(NSString* key in dict)
	{
		if(![dict[key] isKindOfClass:[NSArray class]])
			continue;

		NSMutableArray* array = [NSMutableArray array];
		AddItemsForKeyToArray(dict, key, variables, array);
		for(NSDictionary* item in array)
			r.layers[[key UTF8String]].push_back(parse_layer(item));
	}

	return std::make_shared<layout_metrics_t>(r);
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
		{ "overflow",             tab_bar_requisites::overflow},
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

@interface OakTabBarView ()
{
	OBJC_WATCH_LEAKS(OakTabBarView);

	NSMutableArray* tabTitles;
	NSMutableArray* tabToolTips;
	NSMutableArray* tabModifiedStates;

	BOOL layoutNeedsUpdate;
	NSUInteger selectedTab;
	NSUInteger hiddenTab;
	NSUInteger previousShowAsLastTab;

	layout_metrics_ptr metrics;
	std::vector<NSRect> tabRects;
	std::map<NSUInteger, value_t> tabDropSpacing;
	OakTimer* slideAroundAnimationTimer;
}
- (void)updateLayout;
- (void)selectTab:(id)sender;
@property (nonatomic) OakTimer* slideAroundAnimationTimer;
@property (nonatomic) BOOL layoutNeedsUpdate;
@property (nonatomic) BOOL shouldCollapse;
@end

// =================
// = Accessibility =
// =================

@interface OakTabFauxUIElement : NSObject
{
	OBJC_WATCH_LEAKS(OakTabFauxUIElement);
}
- (id)initWithTabBarView:(OakTabBarView*)tabBarView index:(NSUInteger)index rect:(NSRect)rect title:(NSString*)title toolTip:(NSString*)toolTip modified:(BOOL)modified selected:(BOOL)selected;
@property (nonatomic, weak) OakTabBarView* tabBarView;
@property (nonatomic) NSUInteger index;
@property (nonatomic) NSRect rect;
@property (nonatomic) NSString* title;
@property (nonatomic) NSString* toolTip;
@property (nonatomic) BOOL modified;
@property (nonatomic) BOOL selected;
@end

@implementation OakTabFauxUIElement
- (id)initWithTabBarView:(OakTabBarView*)tabBarView index:(NSUInteger)index rect:(NSRect)rect title:(NSString*)title toolTip:(NSString*)toolTip modified:(BOOL)modified selected:(BOOL)selected
{
	if((self = [super init]))
	{
		_tabBarView = tabBarView;
		_index = index;
		_rect = rect;
		_title = title;
		_toolTip = toolTip;
		_modified = modified;
		_selected = selected;
	}
	return self;
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"<%@: parent=%@, title=\"%@\", index=%ld, rect=%@>", [self class], self.tabBarView, self.title, self.index, NSStringFromRect(self.rect)];
}

- (BOOL)accessibilityIsIgnored
{
	return NO;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = @[
		// generic
		NSAccessibilityParentAttribute,
		NSAccessibilityPositionAttribute,
		NSAccessibilityRoleAttribute,
		NSAccessibilityRoleDescriptionAttribute,
		NSAccessibilitySizeAttribute,
		NSAccessibilityTopLevelUIElementAttribute,
		NSAccessibilityWindowAttribute,
		// radio button
		NSAccessibilityEnabledAttribute,
		NSAccessibilityFocusedAttribute,
		NSAccessibilityTitleAttribute,
		NSAccessibilityValueAttribute,
		NSAccessibilityHelpAttribute,
	];
	return attributes;
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	// generic attributes
	if([attribute isEqualToString:NSAccessibilityParentAttribute])
		return self.tabBarView;
	else if([attribute isEqualToString:NSAccessibilityPositionAttribute] || [attribute isEqualToString:NSAccessibilitySizeAttribute])
	{
		NSRect rect = [self screenRect];
		if([attribute isEqualToString:NSAccessibilityPositionAttribute])
			return [NSValue valueWithPoint:rect.origin];
		else
			return [NSValue valueWithSize:rect.size];
	}
	else if([attribute isEqualToString:NSAccessibilityRoleAttribute])
		return NSAccessibilityRadioButtonRole;
	else if([attribute isEqualToString:NSAccessibilityRoleDescriptionAttribute])
		return NSAccessibilityRoleDescription([self accessibilityAttributeValue:NSAccessibilityRoleAttribute], nil);
	else if([attribute isEqualToString:NSAccessibilityTopLevelUIElementAttribute])
		return [self.tabBarView accessibilityAttributeValue:NSAccessibilityTopLevelUIElementAttribute];
	else if([attribute isEqualToString:NSAccessibilityWindowAttribute])
		return [self.tabBarView accessibilityAttributeValue:NSAccessibilityWindowAttribute];
	// radio button attributes
	else if([attribute isEqualToString:NSAccessibilityEnabledAttribute])
		return [NSNumber numberWithBool:YES];
	else if([attribute isEqualToString:NSAccessibilityFocusedAttribute])
		return [NSNumber numberWithBool:NO];
	else if([attribute isEqualToString:NSAccessibilityTitleAttribute])
	{
		NSString* title = self.title;
		if(self.modified)
			title = [title stringByAppendingString:@" (modified)"];
		return title;
	}
	else if([attribute isEqualToString:NSAccessibilityValueAttribute])
		return [NSNumber numberWithBool:self.selected];
	else if([attribute isEqualToString:NSAccessibilityHelpAttribute])
		return self.toolTip;
	else
		@throw [NSException exceptionWithName:NSAccessibilityException reason:[NSString stringWithFormat:@"Accessibility attribute %@ not supported", attribute] userInfo:nil];
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	return NO;
}

- (void)accessibilitySetValue:(id)value forAttribute:(NSString*)attribute
{
	@throw [NSException exceptionWithName:NSAccessibilityException reason:[NSString stringWithFormat:@"Accessibility attribute %@ not settable", attribute] userInfo:nil];
}

- (NSArray*)accessibilityActionNames
{
	static NSArray* actions = nil;
	if(!actions)
	{
		actions = @[
			NSAccessibilityPressAction,
			NSAccessibilityShowMenuAction,
		];
	}
	return actions;
}

- (NSString*)accessibilityActionDescription:(NSString*)action
{
	return NSAccessibilityActionDescription(action);
}

- (void)accessibilityPerformAction:(NSString*)action
{
	if([action isEqualToString:NSAccessibilityPressAction])
	{
		self.tabBarView.tag = self.index;
		[self.tabBarView selectTab:self.tabBarView];
	}
	else if([action isEqualToString:NSAccessibilityShowMenuAction])
	{
		self.tabBarView.tag = self.index;
		if([self.tabBarView.delegate respondsToSelector:@selector(menuForTabBarView:)])
			[[self.tabBarView.delegate menuForTabBarView:self.tabBarView] popUpMenuPositioningItem:nil atLocation:self.rect.origin inView:self.tabBarView];
	}
	else
	{
		@throw [NSException exceptionWithName:NSAccessibilityException reason:[NSString stringWithFormat:@"Accessibility action %@ not supported", action] userInfo:nil];
	}
}

- (NSRect)windowRect
{
	return [self.tabBarView convertRect:self.rect toView:nil];
}

- (NSRect)screenRect
{
	return [[self.tabBarView window] convertRectToScreen:[self windowRect]];
}
@end

// ==========================

@implementation OakTabBarView
@synthesize slideAroundAnimationTimer, layoutNeedsUpdate;

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		metrics           = layout_metrics_t::parse([NSDictionary dictionaryWithContentsOfFile:[[NSBundle bundleForClass:[self class]] pathForResource:@"TabBar" ofType:@"plist"]]);
		hiddenTab         = NSNotFound;
		tabTitles         = [NSMutableArray new];
		tabToolTips       = [NSMutableArray new];
		tabModifiedStates = [NSMutableArray new];

		[self userDefaultsDidChange:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(viewFrameChanged:) name:NSViewFrameDidChangeNotification object:self];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
		[self registerForDraggedTypes:@[ OakTabBarViewTabType ]];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.shouldCollapse = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableTabBarCollapsingKey];
	self.expanded       = self.shouldCollapse ? [tabTitles count] > 1 : YES;
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
	if([[tabModifiedStates safeObjectAtIndex:tabIndex] boolValue])
		filter |= tab_bar_requisites::modified;
	if(tabIndex == 0)
		filter |= tab_bar_requisites::first;
	if(tabIndex >= [self countOfVisibleTabs] - 1 && tabTitles.count > [self countOfVisibleTabs])
		filter |= tab_bar_requisites::overflow;
	return filter;
}

- (double)putDefaultTabWidthsInto:(std::vector<double>&)tabSizes
{
	double totalWidth = 0;
	for(NSUInteger tabIndex = 0; tabIndex < tabTitles.count; ++tabIndex)
	{
		double width = WidthOfText([tabTitles safeObjectAtIndex:tabIndex]);
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
	return maxNumberOfTabs;
}

- (void)updateLayout
{
	NSRect rect = [self bounds];
	tabRects.clear();

	D(DBF_TabBarView, bug("\n"););
	if(!self.isExpanded)
		return [self setLayers:metrics->layers_for("backgroundCollapsed", rect, -1)];

	std::vector<layer_t> newLayout, selectedTabLayers;
	newLayout = metrics->layers_for("background", rect, -1);

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
	NSUInteger lastVisibleTab = numberOfTabs > 1 ? numberOfTabs-1 : 0;

	NSUInteger showAsLastTab = lastVisibleTab;
	if(lastVisibleTab <= selectedTab)
		showAsLastTab = selectedTab;
	else if(lastVisibleTab < previousShowAsLastTab)
		showAsLastTab = previousShowAsLastTab;
	previousShowAsLastTab = showAsLastTab == lastVisibleTab ? 0 : showAsLastTab;

	for(NSUInteger tabIndex = 0; tabIndex < tabTitles.count; ++tabIndex)
	{
		rect.origin.x += tabDropSpacing.find(tabIndex) != tabDropSpacing.end() ? tabDropSpacing[tabIndex].set_time(CFAbsoluteTimeGetCurrent()) : 0;
		if(tabIndex == hiddenTab || (tabIndex >= lastVisibleTab && tabIndex != showAsLastTab))
		{
			tabRects.push_back(NSZeroRect);
			continue;
		}

		if(tabIndex == showAsLastTab)
				rect.size.width = tabSizes.back();
		else	rect.size.width = tabSizes[tabIndex];

		std::string layer_id  = [self layerNameForTabIndex:tabIndex];
		NSString* toolTipText = [tabToolTips safeObjectAtIndex:tabIndex];
		NSString* title       = [tabTitles safeObjectAtIndex:tabIndex];

		std::vector<layer_t> const& layers = metrics->layers_for(layer_id, rect, tabIndex, title, toolTipText, [self filterForTabIndex:tabIndex]);
		if(tabIndex == selectedTab)
				selectedTabLayers.insert(selectedTabLayers.end(), layers.begin(), layers.end());
		else	newLayout.insert(newLayout.end(), layers.begin(), layers.end());

		tabRects.push_back(rect);
		rect.origin.x += rect.size.width + metrics->tabSpacing;
	}
	newLayout.insert(newLayout.end(), selectedTabLayers.begin(), selectedTabLayers.end());
	[self setLayers:newLayout];
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, self.isExpanded ? 23 : 1);
}

- (void)viewFrameChanged:(NSNotification*)aNotification
{
	self.layoutNeedsUpdate = YES;
}

- (void)setExpanded:(BOOL)flag
{
	if(_expanded == flag)
		return;

	_expanded = flag;
	self.layoutNeedsUpdate = YES;
	[self invalidateIntrinsicContentSize];
}

- (void)selectTab:(id)sender
{
	if(self.tag != selectedTab)
	{
		if(self.delegate && [self.delegate respondsToSelector:@selector(tabBarView:shouldSelectIndex:)] && ![self.delegate tabBarView:self shouldSelectIndex:self.tag])
			return;

		selectedTab = self.tag;
		self.layoutNeedsUpdate = YES;
	}
}

- (void)didDoubleClickTab:(id)sender
{
	if([self.delegate respondsToSelector:@selector(tabBarView:didDoubleClickIndex:)])
		[self.delegate tabBarView:self didDoubleClickIndex:selectedTab];
}

- (void)didDoubleClickTabBar:(id)sender
{
	if([self.delegate respondsToSelector:@selector(tabBarViewDidDoubleClick:)])
		[self.delegate tabBarViewDidDoubleClick:self];
}

- (void)performClose:(id)sender
{
	D(DBF_TabBarView, bug("\n"););
	self.tag = selectedTab; // performCloseTab: asks for [sender tag]
	[NSApp sendAction:@selector(performCloseTab:) to:nil from:self];
}

- (NSMenu*)overflowTabMenu
{
	NSMenu* menu = [NSMenu new];
	for(NSUInteger i = self.countOfVisibleTabs-1; i < [tabTitles count]; ++i)
	{
		NSMenuItem* item = [menu addItemWithTitle:[tabTitles objectAtIndex:i] action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:@""];
		item.tag     = i;
		item.toolTip = [tabToolTips objectAtIndex:i];
		if(i == selectedTab)
			[item setState:NSOnState];
		else if([[tabModifiedStates objectAtIndex:i] boolValue])
			[item setModifiedState:YES];
	}
	return menu;
}

- (void)showOverflowTabMenu:(id)sender
{
	NSUInteger overflowTab = self.tag;
	NSRect tabRect = tabRects[overflowTab];
	NSRect rect;
	uint32_t state = [self currentState] | layer_t::mouse_inside | layer_t::mouse_clicked;
	citerate(it, metrics->layers_for([self layerNameForTabIndex:overflowTab], tabRect, overflowTab, [tabTitles objectAtIndex:overflowTab], nil, [self filterForTabIndex:overflowTab] | tab_bar_requisites::overflow))
	{
		if((state & it->requisite_mask) == it->requisite)
			rect = it->rect;
	}
	rect.origin.y -= 6; //shift menu down slightly to base of tab
	[[self overflowTabMenu] popUpMenuPositioningItem:nil atLocation:(rect.origin) inView:self];
	self.layoutNeedsUpdate = YES;
}

- (NSMenu*)menuForEvent:(NSEvent*)anEvent
{
	NSPoint pos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	self.tag = [self tagForLayerContainingPoint:pos];
	if(self.tag != NSNotFound && [self.delegate respondsToSelector:@selector(menuForTabBarView:)])
		return [self.delegate menuForTabBarView:self];
	return [super menuForEvent:anEvent];
}

- (void)setSelectedTab:(NSUInteger)anIndex
{
	if(selectedTab == anIndex)
		return;
	selectedTab = anIndex;
	NSAccessibilityPostNotification(self, NSAccessibilityValueChangedNotification);
	self.layoutNeedsUpdate = YES;
}

- (void)reloadData
{
	if(!self.dataSource)
		return;

	NSMutableArray* titles         = [NSMutableArray array];
	NSMutableArray* toolTips       = [NSMutableArray array];
	NSMutableArray* modifiedStates = [NSMutableArray array];

	NSUInteger count = [self.dataSource numberOfRowsInTabBarView:self];
	for(NSUInteger i = 0; i < count; ++i)
	{
		[titles addObject:[self.dataSource tabBarView:self titleForIndex:i]];
		[toolTips addObject:[self.dataSource tabBarView:self toolTipForIndex:i]];
		[modifiedStates addObject:@([self.dataSource tabBarView:self isEditedAtIndex:i])];
	}

	if(previousShowAsLastTab != 0 && count != tabTitles.count && previousShowAsLastTab < tabTitles.count)
	{
		// We use tool tip as identifer since this is tilde-abbreviated path and thus more unique than the title
		// Ideally we should introduce a real (unique) identifier, like the document’s UUID
		NSString* tabIdentifier = tabToolTips[previousShowAsLastTab];
		if([tabIdentifier isEqualToString:@""])
				previousShowAsLastTab = [toolTips indexOfObject:tabTitles[previousShowAsLastTab]];
		else	previousShowAsLastTab = [toolTips indexOfObject:tabIdentifier];

		if(previousShowAsLastTab == NSNotFound)
			previousShowAsLastTab = 0;
	}

	[tabTitles setArray:titles];
	[tabToolTips setArray:toolTips];
	[tabModifiedStates setArray:modifiedStates];

	selectedTab = [tabToolTips count] && selectedTab != NSNotFound ? std::min(selectedTab, [tabToolTips count]-1) : NSNotFound;

	BOOL shouldBeExpanded = self.shouldCollapse ? [tabTitles count] > 1 : YES;
	if(shouldBeExpanded != self.isExpanded)
			self.expanded = shouldBeExpanded;
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

	NSImage* image = [[NSImage alloc] initWithSize:tabRect.size];
	[image lockFocus];

	uint32_t state = [self currentState] | layer_t::mouse_inside | layer_t::mouse_down;
	citerate(it, metrics->layers_for([self layerNameForTabIndex:draggedTab], (NSRect){NSZeroPoint, tabRect.size}, draggedTab, [tabTitles objectAtIndex:draggedTab], nil, [self filterForTabIndex:draggedTab] | tab_bar_requisites::dragged))
	{
		if((state & it->requisite_mask) == it->requisite)
			[self drawLayer:*it];
	}
	[image unlockFocus];

	NSImage* dragImage = [[NSImage alloc] initWithSize:image.size];
	[dragImage lockFocus];
	[image drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeCopy fraction:0.8];
	[dragImage unlockFocus];

	NSPasteboard* pboard = [NSPasteboard pasteboardWithName:NSDragPboard];
	[pboard declareTypes:@[ OakTabBarViewTabType ] owner:self];
	[pboard setString:[NSString stringWithFormat:@"%lu", draggedTab] forType:OakTabBarViewTabType];
	[self.delegate setupPasteboard:pboard forTabAtIndex:draggedTab];

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
	BOOL success = [self.delegate performTabDropFromTabBar:[sender draggingSource]
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

// =================
// = Accessibility =
// =================

- (BOOL)accessibilityIsIgnored
{
	return NO;
}

- (NSSet*)myAccessibilityAttributeNames
{
	static NSSet* set = [NSSet setWithArray:@[
		// generic
		NSAccessibilityRoleAttribute,
		// tab group
		NSAccessibilityChildrenAttribute,
		NSAccessibilityContentsAttribute,
		NSAccessibilityFocusedAttribute,
		NSAccessibilityTabsAttribute,
		NSAccessibilityValueAttribute,
	]];
	return set;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = [[[self myAccessibilityAttributeNames] setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
	return attributes;
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	if([[self myAccessibilityAttributeNames] containsObject:attribute])
		return NO;
	return [super accessibilityIsAttributeSettable:attribute];
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	// generic attributes
	if([attribute isEqualToString:NSAccessibilityRoleAttribute])
		return NSAccessibilityTabGroupRole;
	// tab group attributes
	else if([attribute isEqualToString:NSAccessibilityChildrenAttribute] || [attribute isEqualToString:NSAccessibilityContentsAttribute] || [attribute isEqualToString:NSAccessibilityTabsAttribute])
		return [self accessibilityArrayAttributeValues:attribute index:0 maxCount:[self accessibilityArrayAttributeCount:attribute]];
	else if([attribute isEqualToString:NSAccessibilityFocusedAttribute])
		return [NSNumber numberWithBool:NO];
	else if([attribute isEqualToString:NSAccessibilityValueAttribute])
		return [self accessibilityChildAtIndex:selectedTab];
	else
		return [super accessibilityAttributeValue:attribute];
}

- (NSUInteger)accessibilityArrayAttributeCount:(NSString*)attribute
{
	if([attribute isEqualToString:NSAccessibilityChildrenAttribute] || [attribute isEqualToString:NSAccessibilityContentsAttribute] || [attribute isEqualToString:NSAccessibilityTabsAttribute])
		return [self.dataSource numberOfRowsInTabBarView:self];
	else
		return [super accessibilityArrayAttributeCount:attribute];
}

- (NSArray*)accessibilityArrayAttributeValues:(NSString*)attribute index:(NSUInteger)index maxCount:(NSUInteger)maxCount
{
	if([attribute isEqualToString:NSAccessibilityChildrenAttribute] || [attribute isEqualToString:NSAccessibilityContentsAttribute] || [attribute isEqualToString:NSAccessibilityTabsAttribute])
	{
		NSUInteger count = [self accessibilityArrayAttributeCount:attribute];
		if(index + maxCount < count)
			count = index + maxCount;
		NSMutableArray *children = [NSMutableArray arrayWithCapacity:count - index];
		for(; index < count; ++index)
			[children addObject:[self accessibilityChildAtIndex:index]];
		return children;
	}
	else
	{
		return [super accessibilityArrayAttributeValues:attribute index:index maxCount:maxCount];
	}
}

- (NSUInteger)accessibilityIndexOfChild:(id)child
{
	OakTabFauxUIElement *element = (OakTabFauxUIElement*)child;
	if([child isMemberOfClass:[OakTabFauxUIElement class]] && element.tabBarView == self)
		return element.index;
	return NSNotFound;
}

- (OakTabFauxUIElement*)accessibilityChildAtIndex:(NSUInteger)index
{
	NSRect rect = index < tabRects.size() ? tabRects[index] : [self bounds];
	NSString* title = [tabTitles safeObjectAtIndex:index];
	NSString* toolTip = [tabToolTips safeObjectAtIndex:index];
	BOOL modified = [(NSNumber*)[tabModifiedStates safeObjectAtIndex:index] boolValue];
	return [[OakTabFauxUIElement alloc] initWithTabBarView:self index:index rect:rect title:title toolTip:toolTip modified:modified selected:selectedTab==index];
}

- (id)accessibilityHitTest:(NSPoint)point
{
	point = [self convertRect:[[self window] convertRectFromScreen:NSMakeRect(point.x, point.y, 0, 0)] fromView:nil].origin;
	if(!NSPointInRect(point, [self bounds]))
		return self;
	iterate(rect, tabRects)
	{
		if(NSPointInRect(point, *rect))
			return [self accessibilityChildAtIndex:rect - tabRects.begin()];
	}
	return self;
}
@end
