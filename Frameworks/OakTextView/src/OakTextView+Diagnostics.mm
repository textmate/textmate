#import "OakTextView_Private.h"
#import <lsp/LSPManager.h>
#import <document/OakDocument.h>
#import <layout/layout.h>

static NSInteger severityOf (NSDictionary* diagnostic)
{
	NSNumber* severity = diagnostic[@"severity"];
	return severity ? severity.integerValue : 4; // absent: treat as hint
}

// Hue alone does not distinguish error from warning for a red-green colour
// blind reader, so severity is also carried by a symbol on every banner.
static NSString* diagnosticSymbolNameForSeverity (NSInteger severity)
{
	switch(severity)
	{
		case 1: return @"exclamationmark.octagon.fill";
		case 2: return @"exclamationmark.triangle.fill";
		case 3: return @"info.circle.fill";
		default: return @"lightbulb.fill";
	}
}

static NSString* diagnosticSymbolLabelForSeverity (NSInteger severity)
{
	switch(severity)
	{
		case 1: return @"Error";
		case 2: return @"Warning";
		case 3: return @"Information";
		default: return @"Hint";
	}
}

// nil for hint (4) and unknown: those get neither a tint nor a severity bar.
static NSColor* diagnosticBaseColorForSeverity (NSInteger severity)
{
	switch(severity)
	{
		case 1: return NSColor.systemRedColor;
		case 2: return NSColor.systemYellowColor;
		case 3: return NSColor.systemBlueColor;
		default: return nil;
	}
}

static NSColor* diagnosticTextColorForSeverity (NSInteger severity)
{
	switch(severity)
	{
		case 1: return NSColor.systemRedColor;
		case 2: return NSColor.systemOrangeColor;
		case 3: return NSColor.systemBlueColor;
		default: return NSColor.secondaryLabelColor;
	}
}

// Symbol images dominate the cost of a diagnostics draw, yet depend only on the
// severity, the font size and the tint, none of which change between draws. The
// tint is keyed by its resolved components rather than by the colour object,
// because the severity colours are dynamic: a light/dark switch has to miss.
static NSImage* diagnosticSymbolImage (NSInteger severity, CGFloat pointSize, NSColor* tint)
{
	static NSMutableDictionary<NSString*, NSImage*>* cache = [NSMutableDictionary dictionary];

	CGFloat red = 0, green = 0, blue = 0, alpha = 0;
	if(NSColor* resolved = [tint colorUsingColorSpace:NSColorSpace.sRGBColorSpace])
		[resolved getRed:&red green:&green blue:&blue alpha:&alpha];

	NSString* key = [NSString stringWithFormat:@"%ld/%.2f/%.3f,%.3f,%.3f,%.3f", (long)severity, pointSize, red, green, blue, alpha];
	if(NSImage* cached = cache[key])
		return cached;

	NSImageSymbolConfiguration* configuration = [NSImageSymbolConfiguration configurationWithPointSize:pointSize weight:NSFontWeightSemibold];
	configuration = [configuration configurationByApplyingConfiguration:[NSImageSymbolConfiguration configurationWithHierarchicalColor:tint]];

	NSImage* image = [[NSImage imageWithSystemSymbolName:diagnosticSymbolNameForSeverity(severity) accessibilityDescription:diagnosticSymbolLabelForSeverity(severity)] imageWithSymbolConfiguration:configuration];
	if(image)
		cache[key] = image;
	return image;
}

// The code is more useful than the server name, so it wins when both are given.
static NSString* diagnosticPrefixedMessage (NSDictionary* diagnostic)
{
	NSString* code = nil;
	id codeValue = diagnostic[@"code"];
	if([codeValue isKindOfClass:[NSString class]])
		code = codeValue;
	else if([codeValue isKindOfClass:[NSNumber class]])
		code = [codeValue stringValue];

	NSString* source  = diagnostic[@"source"];
	NSString* message = diagnostic[@"message"] ?: @"";

	if(code.length)
		return [NSString stringWithFormat:@"%@: %@", code, message];
	else if(source.length)
		return [NSString stringWithFormat:@"%@: %@", source, message];
	return message;
}

// Rounded on the left, square on the right: the banner sits flush against the
// editor's right edge, where a curve would leave a notch against the border.
static NSBezierPath* diagnosticBannerPath (NSRect rect, CGFloat radius)
{
	radius = MIN(radius, MIN(NSWidth(rect), NSHeight(rect)) / 2);

	NSBezierPath* path = [NSBezierPath bezierPath];
	[path moveToPoint:NSMakePoint(NSMaxX(rect), NSMinY(rect))];
	[path appendBezierPathWithArcFromPoint:NSMakePoint(NSMinX(rect), NSMinY(rect)) toPoint:NSMakePoint(NSMinX(rect), NSMaxY(rect)) radius:radius];
	[path appendBezierPathWithArcFromPoint:NSMakePoint(NSMinX(rect), NSMaxY(rect)) toPoint:NSMakePoint(NSMaxX(rect), NSMaxY(rect)) radius:radius];
	[path lineToPoint:NSMakePoint(NSMaxX(rect), NSMaxY(rect))];
	[path closePath];
	return path;
}

// Diagnostics that map to one visible line: those actually on it, plus any
// collapsed into it because they live inside a fold starting at that line.
struct diagnostic_group_t
{
	NSMutableArray<NSDictionary*>* own    = nil;
	NSMutableArray<NSDictionary*>* folded = nil;
};

static CGFloat const kDiagnosticBoxPaddingH = 8;
static CGFloat const kDiagnosticBoxPaddingV = 6;
static CGFloat const kDiagnosticBoxMinWidth = 160;
static CGFloat const kDiagnosticBoxRadius   = 5;
static CGFloat const kDiagnosticBoxUnbound  = 1.0e7; // stand-in for an unconstrained text container

// Measured on a throwaway TextKit stack rather than by reading back through the
// live view: once the box is on screen its own container answers in terms of
// the frame it already has, so the box grew on the first redraw after it
// appeared. Same engine and same configuration, so the numbers match what the
// view will draw, but nothing about the view can feed back into them.
//
// One pass at the widest the box may be gives both dimensions: the used rect is
// the text's natural width when the message fits, and the wrapped width when it
// does not.
static NSSize diagnosticBoxTextSize (NSAttributedString* content, CGFloat maxWidth)
{
	NSTextStorage* storage     = [[NSTextStorage alloc] initWithAttributedString:content];
	NSLayoutManager* manager   = [[NSLayoutManager alloc] init];
	NSTextContainer* container = [[NSTextContainer alloc] initWithSize:NSMakeSize(maxWidth, kDiagnosticBoxUnbound)];

	container.lineFragmentPadding = 0;
	[manager addTextContainer:container];
	[storage addLayoutManager:manager];

	[manager ensureLayoutForTextContainer:container];
	NSRect const used = [manager usedRectForTextContainer:container];
	return NSMakeSize(MAX(ceil(NSWidth(used)), 1.0), MAX(ceil(NSHeight(used)), 1.0));
}

@implementation OakTextView (Diagnostics)

// MARK: - Data Querying

- (std::map<size_t, diagnostic_group_t>)groupedDiagnosticsByLine
{
	std::map<size_t, diagnostic_group_t> grouped;

	OakDocument* doc = self.document;
	if(!doc)
		return grouped;

	NSArray<NSDictionary*>* diagnostics = [[LSPManager sharedManager] diagnosticsForDocument:doc];
	size_t const lineCount = documentView->lines();

	// Walking back to a fold's start marker is linear in the fold's length, so
	// remember the spans already walked: several diagnostics usually share one.
	std::vector<std::pair<size_t, size_t>> knownFolds; // start -> highest line known folded

	// Diagnostics inside a collapsed fold have no visible line of their own, so
	// attribute them to the fold's start marker instead of dropping them.
	auto visibleLineFor = [&](size_t line) -> size_t {
		if(!documentView->is_line_folded(line))
			return line;

		for(auto const& fold : knownFolds)
		{
			if(line > fold.first && line <= fold.second)
				return fold.first;
		}

		size_t start = line;
		while(start > 0 && documentView->is_line_folded(start))
			--start;

		for(auto& fold : knownFolds)
		{
			if(fold.first == start)
			{
				fold.second = std::max(fold.second, line);
				return start;
			}
		}
		knownFolds.emplace_back(start, line);
		return start;
	};

	for(NSDictionary* diag in diagnostics)
	{
		NSNumber* line = diag[@"line"];
		if(!line || line.unsignedLongValue >= lineCount)
			continue;

		size_t const actual  = line.unsignedLongValue;
		size_t const visible = visibleLineFor(actual);

		diagnostic_group_t& group = grouped[visible];
		if(visible == actual)
		{
			if(!group.own)
				group.own = [NSMutableArray array];
			[group.own addObject:diag];
		}
		else
		{
			if(!group.folded)
				group.folded = [NSMutableArray array];
			[group.folded addObject:diag];
		}
	}
	return grouped;
}

- (NSDictionary*)highestSeverityDiagnostic:(NSArray<NSDictionary*>*)diagnostics
{
	NSDictionary* best = diagnostics.firstObject;
	NSInteger bestSeverity = severityOf(best);
	for(NSDictionary* diag in diagnostics)
	{
		NSInteger sev = severityOf(diag);
		if(sev < bestSeverity)
		{
			best = diag;
			bestSeverity = sev;
		}
	}
	return best;
}

// MARK: - Drawing

- (void)drawDiagnosticsInRect:(NSRect)aRect
{
	// The hit-test cache is rebuilt from scratch on every draw, so it must be
	// dropped before any early return or a disabled setting would leave the
	// previous frame's banners clickable.
	_diagnosticBannerRects.clear();

	if(!documentView || !_diagnosticBannersVisible)
		return [self dismissExpandedDiagnosticBox];

	auto const grouped = [self groupedDiagnosticsByLine];
	if(grouped.empty())
		return [self dismissExpandedDiagnosticBox];

	// Multi-line ghost text shifts every line below the caret down; mirror the
	// shift so tints and banners stay aligned with the text they annotate.
	CGFloat ghostSplitY = CGFLOAT_MAX;
	if(_ghostText && _ghostTextCaret <= documentView->size() && _ghostTextExtraHeight > 0)
		ghostSplitY = CGRectGetMaxY(documentView->rect_at_index(ng::index_t(_ghostTextCaret)));

	NSRect const visibleRect = [self visibleRect];

	CGFloat const bannerPaddingH     = 7.0;
	CGFloat const severityBarWidth   = 3.0;
	CGFloat const codeGap            = 16.0; // clearance kept between code and banner
	CGFloat const badgePaddingH      = 3.0;  // symbol inset of the icon-only badge

	NSWorkspace* workspace = NSWorkspace.sharedWorkspace;
	BOOL const increaseContrast   = workspace.accessibilityDisplayShouldIncreaseContrast;
	BOOL const reduceTransparency = workspace.accessibilityDisplayShouldReduceTransparency;

	CGFloat const tintAlpha   = increaseContrast ? 0.22 : 0.12;
	CGFloat const bannerAlpha = increaseContrast ? 0.35 : 0.20;
	CGFloat const iconAlpha   = increaseContrast ? 0.95 : 0.85;

	NSColor* editorBackground = [NSColor colorWithCGColor:self.theme->background(documentView->file_type())] ?: NSColor.textBackgroundColor;

	// Flush with the editor's right edge. Nothing is subtracted for the
	// scroller: a legacy one takes layout width, so the clip view has already
	// excluded it, and an overlay one floats above the content and only covers
	// the banner's last few points while it is being dragged.
	CGFloat const bannerRightEdge = NSMaxX(visibleRect);
	// A short line leaves plenty of room, but a banner that spans the viewport
	// is its own kind of unreadable, so cap the collapsed form regardless.
	CGFloat const maxBannerWidth  = MIN(NSWidth(visibleRect) * 0.5, 500.0);

	CGFloat const basePointSize = (self.font ?: [NSFont systemFontOfSize:0]).pointSize * documentView->font_scale_factor();
	NSFont* bannerFont = [NSFont systemFontOfSize:MAX(basePointSize - 1, 1.0)];
	NSFont* countFont  = [NSFont systemFontOfSize:bannerFont.pointSize weight:NSFontWeightSemibold];

	NSMutableParagraphStyle* truncatingStyle = [[NSMutableParagraphStyle alloc] init];
	truncatingStyle.lineBreakMode = NSLineBreakByTruncatingTail;

	// A soft-wrapped line occupies several visual rows; tint spans all of them,
	// the banner sits on the first. Rows are accumulated then emitted together.
	struct span_t
	{
		bool active       = false;
		bool hasFirstRow  = false;
		size_t line       = 0;
		CGFloat top       = 0;
		CGFloat firstRowBottom = 0;
		CGFloat bottom    = 0;
	};

	// TextMate themes are independent of the system appearance, so resolve the
	// semantic colors against the theme rather than against the window.
	NSAppearance* appearance = [NSAppearance appearanceNamed:(self.theme->is_dark() ? NSAppearanceNameDarkAqua : NSAppearanceNameAqua)];

	[appearance performAsCurrentDrawingAppearance:^{
		span_t span;

		// The translucent pill needs a severity coloured glyph, the solid badge a
		// white one; diagnosticSymbolImage caches both.
		NSImage* (^iconFor)(NSInteger, BOOL) = ^NSImage* (NSInteger severity, BOOL onSolidFill) {
			return diagnosticSymbolImage(severity, bannerFont.pointSize, onSolidFill ? NSColor.whiteColor : diagnosticTextColorForSeverity(severity));
		};

		auto emit = [&](span_t const& s){
			if(!s.active)
				return;

			auto it = grouped.find(s.line);
			if(it == grouped.end())
				return;

			NSArray<NSDictionary*>* own    = it->second.own;
			NSArray<NSDictionary*>* folded = it->second.folded;

			NSDictionary* primary = own.count ? [self highestSeverityDiagnostic:own] : [self highestSeverityDiagnostic:folded];
			NSInteger severity = severityOf(primary);
			if(folded.count)
				severity = MIN(severity, severityOf([self highestSeverityDiagnostic:folded]));

			CGFloat const shift = s.top >= ghostSplitY ? self->_ghostTextExtraHeight : 0;

			if(NSColor* baseColor = diagnosticBaseColorForSeverity(severity))
			{
				// The wash is drawn over the text, so it cannot simply be made
				// opaque: reduced transparency gets a solid edge bar instead.
				NSRect lineRect = reduceTransparency
					? NSMakeRect(NSMinX(visibleRect), s.top + shift, severityBarWidth, s.bottom - s.top)
					: NSMakeRect(NSMinX(visibleRect), s.top + shift, NSWidth(visibleRect), s.bottom - s.top);

				if(NSIntersectsRect(aRect, lineRect))
				{
					[(reduceTransparency ? baseColor : [baseColor colorWithAlphaComponent:tintAlpha]) setFill];
					NSRectFillUsingOperation(lineRect, NSCompositingOperationSourceOver);
				}
			}

			// The banner belongs on the line's first row; if that row is
			// scrolled above the viewport there is nowhere to put it.
			if(!s.hasFirstRow)
				return;

			NSUInteger const diagnosticCount = own.count + folded.count;

			NSMutableString* bannerText = [NSMutableString string];
			NSString* countText = nil;
			if(own.count)
			{
				[bannerText appendString:diagnosticPrefixedMessage(primary)];

				if(diagnosticCount > 1)
					countText = [NSString stringWithFormat:@"+%lu", (unsigned long)(diagnosticCount - 1)];
			}
			else
			{
				// The count is already the message here, so no +N alongside it.
				[bannerText appendFormat:@"%lu diagnostic%s in folded region", (unsigned long)folded.count, folded.count == 1 ? "" : "s"];
			}

			NSColor* baseColor = diagnosticBaseColorForSeverity(severity) ?: NSColor.systemGrayColor;
			NSColor* textColor = diagnosticTextColorForSeverity(severity);

			CGFloat const rowHeight = s.firstRowBottom - s.top;
			CGFloat const rowTop    = s.top + shift;
			CGFloat const iconWidth = MAX(round(rowHeight), 20.0);

			// Keep clear of the code: the message shrinks into whatever space is
			// left after the line's text, and drops entirely when that is tight.
			CGRect const codeRect = self->documentView->rect_for_range(self->documentView->begin(s.line), self->documentView->eol(s.line));
			CGFloat const available = bannerRightEdge - (NSMaxX(codeRect) + codeGap);

			NSAttributedString* attrText = [[NSAttributedString alloc] initWithString:bannerText attributes:@{
				NSFontAttributeName:            bannerFont,
				NSForegroundColorAttributeName: textColor,
				NSParagraphStyleAttributeName:  truncatingStyle,
			}];

			// The count is laid out ahead of the message rather than appended to
			// it, so that clipping the message cannot take the count with it.
			NSAttributedString* attrCount = countText ? [[NSAttributedString alloc] initWithString:countText attributes:@{
				NSFontAttributeName:            countFont,
				NSForegroundColorAttributeName: textColor,
			}] : nil;

			CGFloat const countGap   = attrCount ? bannerPaddingH : 0.0;
			CGFloat const countWidth = attrCount ? ceil(attrCount.size.width) : 0.0;

			CGFloat const wantedTextWidth = ceil(attrText.size.width) + bannerPaddingH * 2;
			CGFloat textWidth = 0;
			if(available >= iconWidth + 40.0) // otherwise an icon-only badge
				textWidth = MAX(MIN(wantedTextWidth, MIN(available, maxBannerWidth) - iconWidth - countGap - countWidth), 0);

			bool const iconOnly = textWidth == 0;

			NSImage* icon = iconFor(severity, iconOnly);

			NSRect bannerRect, iconRect;
			if(iconOnly)
			{
				CGFloat const badgeWidth  = icon ? ceil(icon.size.width) + badgePaddingH * 2 : iconWidth;
				CGFloat const badgeHeight = MIN(icon ? ceil(icon.size.height) + badgePaddingH * 2 : iconWidth, rowHeight);
				bannerRect = NSMakeRect(bannerRightEdge - badgeWidth, round(rowTop + (rowHeight - badgeHeight) / 2), badgeWidth, badgeHeight);
				iconRect   = bannerRect;
			}
			else
			{
				CGFloat const pillWidth = iconWidth + countGap + countWidth + textWidth;
				bannerRect = NSMakeRect(bannerRightEdge - pillWidth, rowTop, pillWidth, rowHeight);
				iconRect   = NSMakeRect(NSMinX(bannerRect), rowTop, iconWidth, rowHeight);
			}

			diagnostic_banner_t banner;
			banner.rect       = bannerRect;
			banner.line       = s.line;
			banner.diagnostic = primary;
			self->_diagnosticBannerRects.push_back(banner);

			if(!NSIntersectsRect(aRect, bannerRect))
				return;

			CGFloat const radius = MIN(5.0, NSHeight(bannerRect) / 2.0);
			NSBezierPath* capsule = diagnosticBannerPath(bannerRect, radius);

			[NSGraphicsContext saveGraphicsState];
			[capsule addClip];

			if(iconOnly)
			{
				// At badge size, over code, a translucent tint is unreadable, so
				// this one element is solid — deliberately unlike the pill.
				[[baseColor colorWithAlphaComponent:iconAlpha] setFill];
				NSRectFillUsingOperation(bannerRect, NSCompositingOperationSourceOver);
			}
			else
			{
				if(reduceTransparency)
				{
					[editorBackground setFill];
					NSRectFill(bannerRect);
				}

				[[baseColor colorWithAlphaComponent:bannerAlpha] setFill];
				NSRectFillUsingOperation(bannerRect, NSCompositingOperationSourceOver);
			}

			[NSGraphicsContext restoreGraphicsState];

			if(increaseContrast)
			{
				[textColor setStroke];
				capsule.lineWidth = 1.0;
				[capsule stroke];
			}

			if(icon)
			{
				NSRect glyphRect = NSMakeRect(round(NSMidX(iconRect) - icon.size.width / 2), round(NSMidY(iconRect) - icon.size.height / 2), icon.size.width, icon.size.height);
				[icon drawInRect:glyphRect fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:1.0 respectFlipped:YES hints:nil];
			}

			if(iconOnly)
				return;

			if(attrCount)
			{
				NSRect countRect = NSMakeRect(NSMaxX(iconRect) + countGap, round(NSMidY(bannerRect) - attrCount.size.height / 2), countWidth, attrCount.size.height);
				[attrCount drawWithRect:countRect options:NSStringDrawingUsesLineFragmentOrigin];
			}

			NSRect textRect = NSMakeRect(NSMaxX(iconRect) + countGap + countWidth + bannerPaddingH, round(NSMidY(bannerRect) - attrText.size.height / 2), textWidth - bannerPaddingH * 2, attrText.size.height);
			[attrText drawWithRect:textRect options:NSStringDrawingUsesLineFragmentOrigin | NSStringDrawingTruncatesLastVisibleLine];
		};

		// Walk the whole viewport, not just the dirty rect: a partial redraw
		// (hover highlight) must not drop live banners from the hit-test cache.
		CGFloat const scanBottom = NSMaxY(visibleRect) + (ghostSplitY == CGFLOAT_MAX ? 0 : self->_ghostTextExtraHeight);

		auto prevLine = std::make_pair<size_t, size_t>(-1, 0);
		for(CGFloat y = NSMinY(visibleRect); y < scanBottom; )
		{
			auto record = self->documentView->line_record_for(y);
			if(record.bottom <= y || prevLine == std::make_pair(record.line, record.softline))
				break;
			prevLine = std::make_pair(record.line, record.softline);

			if(record.softline == 0)
			{
				emit(span);
				span = { true, true, record.line, record.top, record.bottom, record.bottom };
			}
			else if(span.active && span.line == record.line)
			{
				span.bottom = record.bottom;
			}
			else // viewport starts partway through a soft-wrapped line
			{
				emit(span);
				span = { true, false, record.line, record.top, record.bottom, record.bottom };
			}

			y = record.bottom;
		}
		emit(span);

		if(self->_expandedDiagnosticLine != kNoExpandedDiagnostic)
			[self updateExpandedDiagnosticBoxWithGroups:grouped rightEdge:bannerRightEdge visibleRect:visibleRect font:bannerFont increaseContrast:increaseContrast];
	}];

	[self updateDiagnosticBannerCursor];
}

// The box is a subview so that it scrolls with the line it annotates, but it
// still has to follow horizontal scrolling, resizing, folding and editing —
// and drawDiagnosticsInRect: is the one place that already runs on all of
// those. Its geometry is derived from the line rather than from the viewport
// line-walk above, so that it keeps tracking the line once it scrolls out of
// sight; the frame assignment is a no-op unless something actually moved.
- (void)updateExpandedDiagnosticBoxWithGroups:(std::map<size_t, diagnostic_group_t> const&)grouped rightEdge:(CGFloat)rightEdge visibleRect:(NSRect)visibleRect font:(NSFont*)font increaseContrast:(BOOL)increaseContrast
{
	size_t const line = _expandedDiagnosticLine;

	auto it = grouped.find(line);
	if(it == grouped.end()) // the line lost its diagnostics to an edit or a fold
		return [self dismissExpandedDiagnosticBox];

	// Ghost text shifts the lines below the caret, but it is cleared by the very
	// mouseDown: that expands a box, so the box never coexists with it.
	CGRect const lineRect = documentView->rect_for_range(documentView->begin(line), documentView->eol(line));

	size_t const bol = documentView->begin(line);
	std::string const lineText = documentView->substr(bol, documentView->eol(line));
	size_t const firstNonSpace = lineText.find_first_not_of(" \t");
	CGFloat left = CGRectGetMinX(documentView->rect_at_index(ng::index_t(bol + (firstNonSpace == std::string::npos ? 0 : firstNonSpace))));

	// A deeply indented line would otherwise leave a sliver too narrow to read.
	left = round(MIN(left, MAX(NSMinX(visibleRect), rightEdge - kDiagnosticBoxMinWidth)));

	NSTextView* box = _expandedDiagnosticBox;
	if(!box)
	{
		box = [self makeExpandedDiagnosticBoxForGroup:it->second line:line font:font increaseContrast:increaseContrast];
		_expandedDiagnosticBox = box;
		[self addSubview:box];
	}

	// Anchored to the same right edge as the pill it belongs to and grown
	// leftward only as far as the message needs: stretching to the indentation
	// regardless of content leaves a wide empty half that reads as a bug. The
	// indentation stays as the left bound, so a long message still ends up
	// spanning the line it annotates before it starts wrapping.
	//
	// Sized with TextKit rather than through a text field, which measures, draws
	// and selects through three separate paths that disagree on the wrapping
	// width — the box came out too tall for its text before a click and too
	// short for it afterwards.
	CGFloat const maxTextWidth = MAX(rightEdge - left - kDiagnosticBoxPaddingH * 2, 1.0);
	NSSize const textSize      = diagnosticBoxTextSize(box.textStorage, maxTextWidth);

	CGFloat const width  = textSize.width + kDiagnosticBoxPaddingH * 2;
	CGFloat const height = textSize.height + kDiagnosticBoxPaddingV * 2;

	// The container is only ever written, never asked: it has to wrap exactly
	// where the measurement said it would.
	box.textContainer.size = NSMakeSize(textSize.width, kDiagnosticBoxUnbound);

	// Only the last visible row genuinely has nowhere to put the box below the
	// line; a box that merely overflows the viewport is fine, the user scrolls.
	CGFloat const roomBelow = NSMaxY(visibleRect) - CGRectGetMaxY(lineRect);
	CGFloat const top = roomBelow >= 0 && roomBelow < MIN(height, 48.0) ? CGRectGetMinY(lineRect) : CGRectGetMaxY(lineRect);

	NSRect const frame = NSMakeRect(round(rightEdge - width), round(top), width, height);
	if(!NSEqualRects(box.frame, frame))
		box.frame = frame;
}

- (NSTextView*)makeExpandedDiagnosticBoxForGroup:(diagnostic_group_t const&)group line:(size_t)line font:(NSFont*)font increaseContrast:(BOOL)increaseContrast
{
	NSMutableArray<NSDictionary*>* diagnostics = [NSMutableArray array];
	if(group.own)
		[diagnostics addObjectsFromArray:group.own];
	if(group.folded)
		[diagnostics addObjectsFromArray:group.folded];
	[diagnostics sortWithOptions:NSSortStable usingComparator:^NSComparisonResult(NSDictionary* lhs, NSDictionary* rhs){
		NSInteger const a = severityOf(lhs), b = severityOf(rhs);
		return a < b ? NSOrderedAscending : (a > b ? NSOrderedDescending : NSOrderedSame);
	}];

	NSColor* foreground = NSColor.textColor;
	NSColor* background = NSColor.textBackgroundColor;
	if(self.theme)
	{
		if(CGColorRef color = self.theme->styles_for_scope(documentView->file_type()).foreground())
			foreground = [NSColor colorWithCGColor:color] ?: foreground;
		if(CGColorRef color = self.theme->background(documentView->file_type()))
			background = [NSColor colorWithCGColor:color] ?: background;
	}
	// A translucent theme would let the code show through the message, which is
	// the very failure the expanded box exists to fix.
	background = [background colorWithAlphaComponent:1.0];

	// Matching the editor background exactly leaves only a hairline border to say
	// the box is a surface sitting on top of the code. Lifting toward the
	// foreground reads as raised in either direction: a dark theme lightens where
	// a light one darkens.
	NSColor* blendBackground = [background colorUsingColorSpace:NSColorSpace.sRGBColorSpace];
	NSColor* blendForeground = [foreground colorUsingColorSpace:NSColorSpace.sRGBColorSpace];
	if(blendBackground && blendForeground)
		background = [blendBackground blendedColorWithFraction:0.09 ofColor:blendForeground];

	// A single diagnostic needs no symbol: the pill directly above the box
	// already carries the severity in both colour and glyph. Several do, or
	// mixed severities become indistinguishable once the tint is dropped.
	BOOL const showSymbols = diagnostics.count > 1;

	CGFloat headIndent = 0;
	if(showSymbols)
	{
		if(NSImage* icon = diagnosticSymbolImage(1, font.pointSize, foreground))
			headIndent = ceil(icon.size.width) + 4.0;
	}

	NSMutableParagraphStyle* firstStyle = [[NSMutableParagraphStyle alloc] init];
	firstStyle.lineBreakMode = NSLineBreakByWordWrapping;
	firstStyle.headIndent    = headIndent; // wrapped rows clear the symbol

	NSMutableParagraphStyle* laterStyle = [firstStyle mutableCopy];
	laterStyle.paragraphSpacingBefore = 5.0;

	NSMutableAttributedString* content = [[NSMutableAttributedString alloc] init];
	NSMutableArray<NSValue*>* entryRanges = [NSMutableArray array];
	for(NSDictionary* diagnostic in diagnostics)
	{
		if(content.length)
			[content appendAttributedString:[[NSAttributedString alloc] initWithString:@"\n"]];

		NSUInteger const start = content.length;
		NSInteger const severity = severityOf(diagnostic);

		if(showSymbols)
		{
			if(NSImage* icon = diagnosticSymbolImage(severity, font.pointSize, diagnosticTextColorForSeverity(severity)))
			{
				// Left as a template the text system would tint the glyph with the
				// message colour, dropping the only cue that carries severity here.
				[icon setTemplate:NO];

				NSTextAttachment* attachment = [[NSTextAttachment alloc] init];
				attachment.image  = icon;
				attachment.bounds = NSMakeRect(0, round((font.capHeight - icon.size.height) / 2), icon.size.width, icon.size.height);
				[content appendAttributedString:[NSAttributedString attributedStringWithAttachment:attachment]];
			}
		}

		NSString* message = diagnosticPrefixedMessage(diagnostic);
		[content appendAttributedString:[[NSAttributedString alloc] initWithString:showSymbols ? [NSString stringWithFormat:@" %@", message] : message]];
		[entryRanges addObject:[NSValue valueWithRange:NSMakeRange(start, content.length - start)]];
	}

	[content addAttributes:@{
		NSFontAttributeName:            font,
		NSForegroundColorAttributeName: foreground,
		NSParagraphStyleAttributeName:  firstStyle,
	} range:NSMakeRange(0, content.length)];

	// Spacing goes before every entry but the first, so that the gap between
	// entries does not also show up as a taller inset at the top of the box.
	for(NSUInteger i = 1; i < entryRanges.count; ++i)
		[content addAttribute:NSParagraphStyleAttributeName value:laterStyle range:entryRanges[i].rangeValue];

	// An NSTextView rather than an NSTextField: the field renders through a cell,
	// selects through a field editor and would have to be measured through
	// NSStringDrawing, and those three disagree on the wrapping width. Here the
	// same layout manager does all three, so what is measured is what is drawn
	// and clicking cannot reflow the text.
	NSTextView* box = [[NSTextView alloc] initWithFrame:NSZeroRect];

	// TextMate themes are independent of the system appearance, so the box has
	// to resolve its own colors the way the surrounding drawing code does.
	box.appearance          = NSAppearance.currentDrawingAppearance;
	box.editable            = NO;
	box.selectable          = YES;
	box.richText            = YES;
	box.drawsBackground     = YES;
	box.backgroundColor     = background;
	box.font                = font;
	box.textColor           = foreground;
	box.textContainerInset  = NSMakeSize(kDiagnosticBoxPaddingH, kDiagnosticBoxPaddingV);
	box.autoresizingMask    = NSViewNotSizable;

	// The frame is derived from the text, so the container must not be derived
	// from the frame or the two chase each other.
	box.textContainer.lineFragmentPadding = 0;
	box.textContainer.widthTracksTextView = NO;
	box.horizontallyResizable             = NO;
	box.verticallyResizable               = NO;

	[box.textStorage setAttributedString:content];
	box.accessibilityLabel = [NSString stringWithFormat:@"Diagnostics for line %lu", (unsigned long)(line + 1)];

	box.wantsLayer            = YES;
	box.layer.cornerRadius    = kDiagnosticBoxRadius;
	// Square against the editor's right edge, matching the pill above it. Both
	// left corners, so which one is visually the top does not matter here.
	box.layer.maskedCorners   = kCALayerMinXMinYCorner | kCALayerMinXMaxYCorner;
	box.layer.borderWidth     = 1.0;
	box.layer.borderColor     = [foreground colorWithAlphaComponent:increaseContrast ? 0.7 : 0.3].CGColor;
	box.layer.backgroundColor = background.CGColor;
	box.layer.masksToBounds   = YES; // the text view fills its bounds squarely

	return box;
}

// MARK: - Hit Testing

- (BOOL)isPointInExpandableDiagnosticBanner:(NSPoint)point
{
	if(!documentView || !_diagnosticBannersVisible)
		return NO;

	for(auto const& banner : _diagnosticBannerRects)
	{
		if(NSPointInRect(point, banner.rect))
			return YES;
	}
	return NO;
}

- (BOOL)handleDiagnosticBannerClickAtPoint:(NSPoint)point
{
	if(!documentView || !_diagnosticBannersVisible)
		return NO;

	for(auto const& banner : _diagnosticBannerRects)
	{
		// Every banner opens a box, even one whose message already fits: the box
		// is the only place the text can be selected and copied.
		if(!NSPointInRect(point, banner.rect))
			continue;

		// The box is built once, so that a redraw cannot discard a selection the
		// user made inside it; that makes tearing it down the only way to move it
		// to another line.
		size_t const wasExpanded = _expandedDiagnosticLine;
		[self dismissExpandedDiagnosticBox];
		if(wasExpanded != banner.line)
			_expandedDiagnosticLine = banner.line;

		[self setNeedsDisplay:YES];
		return YES;
	}
	return NO;
}

// Teardown without a redraw request, so that it is safe to call from drawRect:.
- (void)dismissExpandedDiagnosticBox
{
	_expandedDiagnosticLine = kNoExpandedDiagnostic;

	if(!_expandedDiagnosticBox)
		return;

	// Removing the view that holds first responder would leave the window with a
	// dangling responder, so hand focus back to the text view first.
	NSWindow* window = _expandedDiagnosticBox.window;
	NSResponder* responder = window.firstResponder;
	if([responder isKindOfClass:[NSView class]] && [(NSView*)responder isDescendantOf:_expandedDiagnosticBox])
		[window makeFirstResponder:self];

	[_expandedDiagnosticBox removeFromSuperview];
	_expandedDiagnosticBox = nil;
}

- (void)collapseExpandedDiagnostic
{
	if(_expandedDiagnosticLine == kNoExpandedDiagnostic && !_expandedDiagnosticBox)
		return;

	[self dismissExpandedDiagnosticBox];
	[self setNeedsDisplay:YES];
}

- (void)updateDiagnosticBannerCursor
{
	NSPoint point = [self convertPoint:[self.window mouseLocationOutsideOfEventStream] fromView:nil];
	BOOL overBanner = [self isPointInExpandableDiagnosticBanner:point];
	if(overBanner == _showDiagnosticBannerCursor)
		return;

	_showDiagnosticBannerCursor = overBanner;
	[self.window invalidateCursorRectsForView:self];
}

// MARK: - Notifications

- (void)lspDiagnosticsDidChange:(NSNotification*)notification
{
	NSString* uri = notification.userInfo[@"uri"];
	OakDocument* doc = self.document;
	if(!doc || !doc.path)
		return;
	NSString* myURI = [NSURL fileURLWithPath:doc.path].absoluteString;
	if(![uri isEqualToString:myURI])
		return;

	// The republished set may not contain the diagnostic that was expanded.
	[self dismissExpandedDiagnosticBox];
	_diagnosticBannerRects.clear();
	[self setNeedsDisplay:YES];
}

- (void)diagnosticAccessibilityOptionsDidChange:(NSNotification*)notification
{
	if(_diagnosticBannersVisible)
		[self setNeedsDisplay:YES];
}

- (void)diagnosticScrollBoundsDidChange:(NSNotification*)notification
{
	// Banners are anchored to the viewport's right edge, so only a horizontal
	// scroll invalidates them — vertical scroll blits them along with the text.
	NSClipView* clipView = self.enclosingScrollView.contentView;
	if(!clipView)
		return;

	CGFloat x = NSMinX(clipView.bounds);
	if(x == _diagnosticLastScrollX)
		return;
	_diagnosticLastScrollX = x;

	if(_diagnosticBannersVisible && !_diagnosticBannerRects.empty())
		[self setNeedsDisplay:YES];
}

@end
