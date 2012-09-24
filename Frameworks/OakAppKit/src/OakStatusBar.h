#import "OakControl.h"

@class OakStatusBar;

const CGFloat OakStatusBarHeight = 16;

namespace sb
{
	namespace cell { enum type { info, popup, dropdown, button }; }

	struct PUBLIC cell_t
	{
		WATCH_LEAKS(cell_t);

		cell_t (cell::type type = cell::info);

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

		cell_t& size (CGFloat minSize, CGFloat maxSize = 0);

		cell_t& set_tag (NSInteger newTag);
		cell_t& set_image (NSImage* img);
		cell_t& set_view (NSView* aView);
		cell_t& pressed_image (NSImage* img);
		cell_t& disabled_image (NSImage* img);
		cell_t& tool_tip (std::string const& str);
		cell_t& enabled (BOOL enabled);
		cell_t& no_padding ();
		cell_t& no_separator ();
		cell_t& set_menu_action (SEL anAction);

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
		void set_content (template_image const& tpl);
		void set_content (NSView* aView);
		void set_content (NSString* str);
		void set_content (std::string const& str);
		void set_content (NSImage* img);
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
