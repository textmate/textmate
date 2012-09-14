#import "OakStatusBar.h"
#import "NSColor Additions.h"
#import "NSImage Additions.h"
#import "NSView Additions.h"
#import <OakFoundation/NSString Additions.h>
#import <text/format.h>

@interface OakStatusBar ()
- (void)updateLayout;
@end

@implementation OakStatusBar
@synthesize borderEdges;

- (id)initWithFrame:(NSRect)frame
{
	if(self = [super initWithFrame:frame])
	{
		self.postsFrameChangedNotifications = YES;
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(viewFrameChanged:) name:NSViewFrameDidChangeNotification object:self];
		borderEdges = sb::border::top;
		[self updateLayout];
	}
	return self;
}

- (void)viewFrameChanged:(NSNotification*)aNotification
{
	[self updateLayout];
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, OakStatusBarHeight);
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[super dealloc];
}

// ==========
// = Layout =
// ==========

- (void)setBorderEdges:(NSInteger)edges
{
	borderEdges = edges;
	[self updateLayout];
}

- (void)setCells:(std::vector<sb::cell_t> const&)newCells;
{
	cells.clear();
	NSInteger nextTag = NSIntegerMax;
	for(size_t cellIndex = 0; cellIndex < newCells.size(); ++cellIndex)
	{
		sb::cell_t cell = newCells[cellIndex]; // copy here is intentional as we may mutate cell
		if(cell.action && !cell.tag)
			cell.tag = nextTag--;

		if(cell.min == 0)
		{
			if(cell.image)
				cell.min += [cell.image.get() size].width;
			if(!cell.text.empty())
				cell.min += WidthOfText([NSString stringWithCxxString:cell.text]);

			if(cell.max == 0)
				cell.max = cell.min;
		}
		if(cell.padding != -1)
		{
			if(cell.image)
				cell.padding = 3;
			else if(!cell.text.empty())
				cell.padding = 5;
		}
		else
		{
			cell.padding = 0;
		}

		if(cell.type == sb::cell::popup)
		{
			CGFloat arrowsWidth = [[NSImage imageNamed:@"Statusbar Popup Arrows" inSameBundleAsClass:[OakStatusBar class]] size].width;
			cell.min += arrowsWidth;
			if(cell.max != CGFLOAT_MAX)
				cell.max += arrowsWidth;
		}
		else if(cell.type == sb::cell::dropdown)
		{
			CGFloat arrowWidth = [[NSImage imageNamed:@"Statusbar Dropdown Arrow" inSameBundleAsClass:[OakStatusBar class]] size].width;
			cell.min += arrowWidth;
			if(cell.max != CGFLOAT_MAX)
				cell.max += arrowWidth;
		}

		cells.push_back(cell);

		if(cellIndex != newCells.size() - 1 && !cell.skipTrailingSeparator)
		{
			sb::cell_t separator;
			separator.min   = separator.max = 1;
			separator.image = [NSImage imageNamed:@"Statusbar Separator" inSameBundleAsClass:[OakStatusBar class]];
			cells.push_back(separator);
		}
	}
	[self updateLayout];
}

struct cell_layout_t { CGFloat x; CGFloat width; };

static std::vector<cell_layout_t> layout (CGFloat frameWidth, std::vector<sb::cell_t>& cells)
{
	CGFloat minWidth = 0, maxWidth = 0;
	iterate(it, cells)
	{
		minWidth += it->padding + it->min + it->padding;
		maxWidth += it->padding + ((it->max == CGFLOAT_MAX) ? frameWidth : it->max) + it->padding;
	}

	CGFloat available = frameWidth - minWidth;
	CGFloat delta     = maxWidth - minWidth;

	std::vector<cell_layout_t> cellLayout;
	CGFloat x = 0;
	iterate(it, cells)
	{
		CGFloat width = it->min;
		if(minWidth < frameWidth && maxWidth != minWidth)
		{
			CGFloat cellDelta = (it->max == CGFLOAT_MAX ? frameWidth : it->max) - it->min;
			width += round(available * cellDelta / delta);
		}
		width += it->padding * 2;
		cellLayout.push_back((cell_layout_t){x, width});
		x += width;
	}

	return cellLayout;
}

- (void)updateLayout
{
	std::vector<layer_t> newLayout;

	NSImage* backgroundImage = [NSImage imageNamed:@"Statusbar Background" inSameBundleAsClass:[OakStatusBar class]];

	CGFloat y = 0;
	if(borderEdges & sb::border::bottom)
	{
		layer_t bottomBorder;
		bottomBorder.rect  = NSMakeRect(0, 0, self.bounds.size.width, 1);
		bottomBorder.color = [NSColor colorWithString:@"#9d9d9d"];
		newLayout.push_back(bottomBorder);
		y += 1;
	}

	layer_t background;
	background.rect          = NSMakeRect(0, y, self.bounds.size.width, [backgroundImage size].height);
	background.image         = backgroundImage;
	background.image_options = layer_t::stretch;
	newLayout.push_back(background);
	y += background.rect.size.height;

	if(borderEdges & sb::border::top)
	{
		layer_t topBorder;
		topBorder.rect  = NSMakeRect(0, y, self.bounds.size.width, 1);
		topBorder.color = [NSColor colorWithString:@"#9d9d9d"];
		newLayout.push_back(topBorder);
	}

	std::vector<cell_layout_t> const& cellLayout = ::layout(self.bounds.size.width, cells);

	for(NSUInteger cellIndex = 0; cellIndex < cells.size(); ++cellIndex)
	{
		sb::cell_t const& cell = cells[cellIndex];
		CGFloat cellWidth  = cellLayout[cellIndex].width;
		CGFloat x          = cellLayout[cellIndex].x;

		if(cell.image)
		{
			layer_t layer;
			CGFloat imageHeight     = [cell.image.get() size].height;
			CGFloat statusBarHeight = [backgroundImage size].height;
			layer.rect = NSMakeRect(cell.padding + x, (borderEdges & sb::border::bottom) ? 1 : 0 + round((statusBarHeight - imageHeight) / 2), cellWidth - cell.padding * 2, imageHeight);
			if(cell.state == NSOffState)
			{
				NSImage* disabledImage = cell.disabledImage.get();
				if(!disabledImage)
				{
					disabledImage = [[[NSImage alloc] initWithSize:[cell.image.get() size]] autorelease];
					[disabledImage lockFocus];
					[cell.image.get() drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:0.25];
					[disabledImage unlockFocus];
				}
				layer.image = disabledImage;
			}
			else
			{
				layer.image = cell.image.get();
			}
			newLayout.push_back(layer);
		}
		if(!cell.text.empty())
		{
			CGFloat offset = cell.padding + x;
			CGFloat width = cellWidth - cell.padding * 2;
			if(cell.image)
			{
				offset += [cell.image.get() size].width + 1;
				width  -= [cell.image.get() size].width + 1;
			}
			layer_t layer;
			CGFloat textHeight = 13;
			layer.rect = NSMakeRect(offset, (borderEdges & sb::border::bottom) ? 2 : 1, width, textHeight);
			layer.text = [NSString stringWithCxxString:cell.text];
			newLayout.push_back(layer);
		}
		if(cell.view)
		{
			CGFloat y = ((borderEdges & sb::border::bottom) ? 1 : 0) + ([backgroundImage size].height - [cell.view.get() frame].size.height) / 2;
			layer_t layer;
			layer.rect = NSMakeRect(cell.padding + x, y, cellWidth - cell.padding * 2, [backgroundImage size].height);
			layer.view = cell.view;
			newLayout.push_back(layer);
		}

		if(cell.action)
		{
			if(cell.type == sb::cell::popup)
			{
				layer_t arrows;
				arrows.image         = [NSImage imageNamed:@"Statusbar Popup Arrows" inSameBundleAsClass:[OakStatusBar class]];
				arrows.rect.size     = [arrows.image.get() size];
				arrows.rect.origin.x = x + cellWidth - [arrows.image.get() size].width;
				arrows.rect.origin.y = (self.frame.size.height - arrows.rect.size.height) + ((borderEdges & sb::border::top) ? 0 : 1);
				newLayout.push_back(arrows);
			}
			else if(cell.type == sb::cell::dropdown)
			{
				layer_t arrow;
				arrow.image         = [NSImage imageNamed:@"Statusbar Dropdown Arrow" inSameBundleAsClass:[OakStatusBar class]];
				arrow.rect.size     = [arrow.image.get() size];
				arrow.rect.origin.x = x + cellWidth - [arrow.image.get() size].width - 2;
				arrow.rect.origin.y = (self.bounds.size.height - arrow.rect.size.height) / 2;
				newLayout.push_back(arrow);
			}

			if(cell.state == NSOnState)
			{
				layer_t trigger;
				trigger.rect      = NSMakeRect(x, (borderEdges & sb::border::bottom) ? 1 : 0, cellWidth, [backgroundImage size].height);
				trigger.requisite = trigger.requisite_mask = layer_t::mouse_down;
				trigger.action    = cell.action;
				trigger.tag       = cell.tag;

				if(cell.type == sb::cell::button)
				{
					if(cell.pressedImage)
					{
						CGFloat imageHeight     = [cell.pressedImage.get() size].height;
						CGFloat statusBarHeight = [backgroundImage size].height;
						trigger.content_offset  = NSMakePoint(cell.padding, round((statusBarHeight - imageHeight) / 2));
						trigger.image           = cell.pressedImage;
					}
					else
					{
						trigger.image         = [NSImage imageNamed:@"Statusbar Background Pressed" inSameBundleAsClass:[OakStatusBar class]];
						trigger.image_options = layer_t::stretch;
					}
					trigger.requisite  = trigger.requisite_mask = cell.menuAction ? (layer_t::mouse_clicked | layer_t::menu_gesture) : layer_t::mouse_clicked;
					trigger.menuAction = cell.menuAction;
				}

				newLayout.push_back(trigger);
			}
		}

		if(!cell.toolTip.empty())
		{
			layer_t toolTip;
			toolTip.rect     = NSMakeRect(x, (borderEdges & sb::border::bottom) ? 1 : 0, cellWidth, [backgroundImage size].height);
			toolTip.tool_tip = [NSString stringWithCxxString:cell.toolTip];
			newLayout.push_back(toolTip);
		}
	}

	[self setLayers:newLayout];
	[self setNeedsDisplay:YES];
}

- (CGFloat)minimumWidth
{
	CGFloat minWidth = 0;
	iterate(it, cells)
		minWidth += it->padding + it->min + it->padding;
	return minWidth;
}

// ===========
// = Actions =
// ===========

- (void)sendAction:(SEL)action fromLayer:(layer_t const&)aLayer
{
	iterate(it, cells)
	{
		if(it->tag == aLayer.tag)
		{
			tag = aLayer.tag;
			[NSApp sendAction:action to:it->target from:self];
			return;
		}
	}
}

- (void)showMenu:(NSMenu*)menu withSelectedIndex:(NSUInteger)index forCellWithTag:(NSInteger)cellTag font:(NSFont*)font popup:(BOOL)isPopup
{
	NSRect rect = [self bounds];

	// Find the layer with the provided tag
	iterate(it, [self layers])
	{
		if(it->tag == cellTag)
		{
			rect = it->rect;
			break;
		}
	}

	NSPoint pos       = [[self window] convertBaseToScreen:[self convertPoint:NSMakePoint(0, NSMaxY(rect)) toView:nil]];
	CGFloat scrBottom = NSMinY([[[self window] screen] visibleFrame]) + 30.0;
	if(pos.y < scrBottom)
		rect.origin.y += 30.0;

	[self showMenu:menu inRect:rect withSelectedIndex:index font:font popup:isPopup];
}

- (void)rightMouseDown:(NSEvent*)theEvent
{
	[self mouseDown:theEvent];
}
@end
