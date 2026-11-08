#ifndef OAKTEXTVIEW_LSPUTILITIES_H_8KP3VWQ2
#define OAKTEXTVIEW_LSPUTILITIES_H_8KP3VWQ2

#import "OakTextView_Private.h"
#import <OakAppKit/OakPopOutAnimation.h>
#import <OakFoundation/NSString Additions.h>

static ng::ranges_t merge (ng::ranges_t lhs, ng::ranges_t const& rhs)
{
	for(auto const& range : rhs)
		lhs.push_back(range);
	return lhs;
}

struct refresh_helper_t
{
	refresh_helper_t (OakTextView* self, std::shared_ptr<document_view_t> const& documentView) : _self(self), _document_view(documentView)
	{
		if(documentView->begin_change_grouping())
		{
			_revision  = documentView->revision();
			_selection = documentView->ranges();
			documentView->begin_refresh_cycle(merge(_selection, [_self markedRanges]), [_self liveSearchRanges]);
		}
	}

	~refresh_helper_t ()
	{
		if(auto documentView = _document_view.lock())
		{
			if(documentView->end_change_grouping())
			{
				if(_revision == documentView->revision())
				{
					for(auto const& range : ng::highlight_ranges_for_movement(*documentView, _selection, documentView->ranges()))
					{
						NSRect imageRect;
						NSImage* image = [_self imageForRanges:range imageRect:&imageRect];
						imageRect = [[_self window] convertRectToScreen:[_self convertRect:imageRect toView:nil]];
						OakShowPopOutAnimation(_self, imageRect, image);
					}
				}

				if(_revision != documentView->revision() || _selection != documentView->ranges())
				{
					[_self updateMarkedRanges];
					[_self updateSelection];
					[_self updateSymbol];

					if(_revision != documentView->revision())
						[_self scheduleCopilotGhostText];
				}

				auto damagedRects = documentView->end_refresh_cycle(merge(documentView->ranges(), [_self markedRanges]), [_self visibleRect], [_self liveSearchRanges]);

				NSRect r = [[_self enclosingScrollView] documentVisibleRect];
				CGFloat extraH = [_self hasGhostText] ? [_self ghostTextExtraHeight] : 0;
				NSSize newSize = NSMakeSize(std::max(NSWidth(r), documentView->width()), std::max(NSHeight(r), documentView->height() + extraH));
				if(!NSEqualSizes([_self frame].size, newSize))
					[_self setFrameSize:newSize];

				NSView* gutterView = find_gutter_view([[_self enclosingScrollView] superview]);
				for(auto const& rect : damagedRects)
				{
					[_self setNeedsDisplayInRect:rect];
					if(gutterView)
					{
						NSRect r = rect;
						r.origin.x = 0;
						r.size.width = NSWidth([gutterView frame]);
						[gutterView setNeedsDisplayInRect:r];
					}
				}

				if(_revision != documentView->revision() || _selection != documentView->ranges() || _self.needsEnsureSelectionIsInVisibleArea)
				{
					[_self ensureSelectionIsInVisibleArea:nil];
					[_self resetBlinkCaretTimer];
					[_self updateChoiceMenu:nil];
				}
			}
		}
	}

private:
	static NSView* find_gutter_view (NSView* view)
	{
		for(NSView* candidate in [view subviews])
		{
			if([candidate isKindOfClass:NSClassFromString(@"GutterView")])
				return candidate;
			else if(NSView* res = find_gutter_view(candidate))
				return res;
		}
		return nil;
	}

	OakTextView* _self;
	std::weak_ptr<document_view_t> _document_view;
	size_t _revision;
	ng::ranges_t _selection;
};

#define AUTO_REFRESH refresh_helper_t _dummy(self, documentView)

inline size_t lspPositionToOffset (ng::buffer_api_t const& buffer, NSInteger line, NSInteger character)
{
	if(line < 0)
		return 0;
	if(line >= (NSInteger)buffer.lines())
		return buffer.size();
	return std::min(buffer.convert(text::pos_t(line, std::max<NSInteger>(0, character))), buffer.size());
}

inline std::multimap<std::pair<size_t, size_t>, std::string> replacementsFromTextEdits (ng::buffer_api_t const& buffer, NSArray<NSDictionary*>* edits)
{
	std::multimap<std::pair<size_t, size_t>, std::string> replacements;
	for(NSDictionary* edit in edits)
	{
		NSDictionary* range = edit[@"range"];
		NSDictionary* start = range[@"start"];
		NSDictionary* end   = range[@"end"];
		NSString* newText   = edit[@"newText"];
		if(!start || !end || !newText)
			continue;

		size_t from = lspPositionToOffset(buffer, [start[@"line"] integerValue], [start[@"character"] integerValue]);
		size_t to   = lspPositionToOffset(buffer, [end[@"line"] integerValue], [end[@"character"] integerValue]);
		if(from > to) std::swap(from, to);

		replacements.emplace(std::make_pair(from, to), to_s(newText));
	}
	return replacements;
}

#endif /* end of include guard: OAKTEXTVIEW_LSPUTILITIES_H_8KP3VWQ2 */
