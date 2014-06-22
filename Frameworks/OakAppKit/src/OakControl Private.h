#import "OakControl.h"
#import <oak/debug.h>

struct layer_t
{
	WATCH_LEAKS(layer_t);

	// ===========================================================================================================
	// = Probably makes sense to use boost::any_t or a base class with different subclasses for different things =
	// ===========================================================================================================
	layer_t () : rect(NSZeroRect),
		requisite(0),
		requisite_mask(0),
		tag(0),
		action(nil),
		color(nil),
		borderColor(nil),
		cornerRadius(0),
		text(nil),
		image(nil),
		view(nil),
		text_options(none),
		image_options(no_repeat),
		content_offset(),
		prevent_window_ordering(false) { }

	NSRect rect;
	enum requisite_t
	{
		no_requisite         = (      0),
		mouse_inside         = (1 <<  1),
		mouse_down           = (1 <<  2),
		menu_gesture         = (1 <<  3),
		mouse_dragged        = (1 <<  4),
		mouse_clicked        = (1 <<  5),
		mouse_double_clicked = (1 <<  6),
		control              = (1 <<  7),
		option               = (1 <<  8),
		shift                = (1 <<  9),
		command              = (1 << 10),
		window_key           = (1 << 11),
		window_main          = (1 << 12),
		window_main_or_key   = (1 << 13),
		app_active           = (1 << 14),
		last_requisite       = (1 << 15),
	};
	uint32_t requisite;
	uint32_t requisite_mask;
	NSInteger tag;
	SEL action;
	SEL menuAction;

	NSColor* color;
	NSColor* borderColor;
	CGFloat cornerRadius;
	NSString* text;
	NSImage* image;
	NSString* tool_tip;
	NSView* view;
	enum text_options_t { none, shadow };
	uint32_t text_options;
	enum image_options_t { no_repeat, stretch, /* repeat_x, repeat_y, repeat_xy */ };
	uint32_t image_options;
	NSPoint content_offset;
	bool prevent_window_ordering;

	/*
	// TODO these are unsupported and unrequired, but can be added if needed.
	enum alignment_t { left, center, right };
	uint32_t alignment;
	enum vertical_alignment_t { top, middle, bottom };
	uint32_t vertical_alignment;
	*/
};

@interface OakControl ()
@property (nonatomic) NSInteger tag; // tag of the most recent layer causing an action
@property (nonatomic) BOOL mouseTrackingDisabled;
- (uint32_t)currentState;
- (NSInteger)tagForLayerContainingPoint:(NSPoint)aPoint;
- (void)drawLayer:(layer_t const&)aLayer;
- (std::vector<layer_t> const&)layers;
- (void)setLayers:(std::vector<layer_t> const&)aLayout;
- (void)sendAction:(SEL)action fromLayer:(layer_t const&)aLayer;
@end

double WidthOfText (NSString* string);
