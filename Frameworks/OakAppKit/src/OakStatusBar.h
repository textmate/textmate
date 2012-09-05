#import "OakControl.h"

@class OakStatusBar;

const CGFloat OakStatusBarHeight = 16;

namespace sb
{
	namespace cell { enum type { info, popup, dropdown, button }; }

	struct cell_t
	{
		WATCH_LEAKS(cell_t);

		cell_t (cell::type type = cell::info) : type(type), text(), toolTip(), image(), disabledImage(), action(), menuAction(), tag(0), min(0), max(0), skipTrailingSeparator(NO), state(NSOnState), padding(0)
		{}

		cell::type type;
		std::string text;
		std::string toolTip;
		objc_ptr<NSImage*> image;
		objc_ptr<NSImage*> pressedImage;
		objc_ptr<NSImage*> disabledImage;
		objc_ptr<NSView*> view;

		id target;
		SEL action;
		SEL menuAction;
		NSInteger tag;

		CGFloat min, max;
		BOOL skipTrailingSeparator;
		NSCellStateValue state;
		BOOL allowsHold;

		cell_t& size (CGFloat minSize, CGFloat maxSize = 0)
		{
			min = minSize;
			max = maxSize ?: minSize;
			return *this;
		}

		cell_t& set_tag (NSInteger newTag) { tag = newTag; return *this; }
		cell_t& set_image (NSImage* img) { image = img; return *this; }
		cell_t& set_view (NSView* aView) { view = aView; return *this; }
		cell_t& pressed_image (NSImage* img) { pressedImage = img; return *this; }
		cell_t& disabled_image (NSImage* img) { disabledImage = img; return *this; }
		cell_t& tool_tip (std::string const& str) { toolTip = str; return *this; }
		cell_t& enabled (BOOL enabled) { state = enabled ? NSOnState : NSOffState; return *this; }
		cell_t& no_padding () { padding = -1; return *this; }
		cell_t& no_separator () { skipTrailingSeparator = YES; return *this; }
		cell_t& set_menu_action (SEL anAction) { menuAction = anAction; return *this; }

		static cell_t info () { return cell_t(cell::info); }

		template<typename T>
		static cell_t info (T content)
		{
			cell_t cell(cell::info);
			cell.set_content(content);
			return cell;
		}

		template<typename T>
		static cell_t dropdown (T content, SEL action, id target = nil)
		{
			cell_t cell(cell::dropdown);
			cell.set_content(content);
			cell.action = action;
			cell.target = target;
			return cell;
		}

		template<typename T>
		static cell_t popup (T content, SEL action, id target = nil)
		{
			cell_t cell(cell::popup);
			cell.set_content(content);
			cell.action = action;
			cell.target = target;
			return cell;
		}

		template<typename T>
		static cell_t button (T content, SEL action, id target = nil)
		{
			cell_t cell(cell::button);
			cell.set_content(content);
			cell.action = action;
			cell.target = target;
			return cell;
		}

		// This will be overwritten by OakStatusBar
		CGFloat padding;

		struct template_image
		{
			template_image (NSString* const name) : name(name) {}
			// Not retained as this struct will never be around longer than
			// a runloop iteration, and template names are constant anyway.
			NSString* name;
		};

	private:
		void set_content (template_image const& tpl)
		{
			NSImage* templateImage = [NSImage imageNamed:tpl.name];
			ASSERT(templateImage);
			NSImage* standardImage = [[[NSImage alloc] initWithSize:templateImage.size] autorelease];
			[standardImage lockFocus];
			[templateImage drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:0.5];
			[standardImage unlockFocus];
			set_image(standardImage);
			pressed_image(templateImage);
		}
		void set_content (NSView* aView) { view = aView; }
		void set_content (NSString* str) { text = [str UTF8String]; }
		void set_content (std::string const& str) { text = str; }
		void set_content (NSImage* img)
		{
			image = img;
			if(type == cell::button)
				no_padding();
		}
	};

	namespace border { enum { top = 1, bottom = 2 }; }
}

PUBLIC @interface OakStatusBar : OakControl
{
	std::vector<sb::cell_t> cells;
	NSInteger borderEdges;
}
@property (nonatomic, readonly) CGFloat minimumWidth;
@property (nonatomic, assign) NSInteger borderEdges;
- (void)setCells:(std::vector<sb::cell_t> const&)newCells;
- (void)showMenu:(NSMenu*)menu withSelectedIndex:(NSUInteger)index forCellWithTag:(NSInteger)cellTag font:(NSFont*)font popup:(BOOL)isPopup;
@end

template <size_t cellCount> void SetCells (OakStatusBar* statusBar, sb::cell_t const (&cellList)[cellCount])
{
	[statusBar setCells:std::vector<sb::cell_t>(cellList, cellList + cellCount)];
}
