#include "paragraph.h"
#include "ct.h"
#include "render.h"
#include <cf/cf.h>
#include <text/parse.h>
#include <text/utf8.h>
#include <regexp/format_string.h>

static double const kFoldingDotsRatio = 16.0 / 10.0; // FIXME Folding dots ratio should be obtained from the image and given to layout_t

namespace ng
{
	namespace
	{
		static std::string representation_for (uint32_t ch)
		{
			static std::set<uint32_t> const SpaceCharacters = {
				0x200B, // ZERO WIDTH SPACE
				0x200C, // ZERO WIDTH NON-JOINER
				0x200D, // ZERO WIDTH JOINER
				0x2028, // LINE SEPARATOR
				0x2029, // PARAGRAPH SEPARATOR
				0x2060, // WORD JOINER
				0xFEFF  // ZERO WIDTH NO-BREAK SPACE
			};

			if(0x20 <= ch && ch <= 0x7E || ch == '\t' || ch == '\n')
				return NULL_STR;

			switch(ch)
			{
				case '\f': return "<NP>";
				case '\r': return "<CR>";
				case '\b': return "<BS>";
				case 0x00: return "<NUL>";
				case 0x1B: return "<ESC>";
				case 0x1C: return "<FS>";
				case 0x1D: return "<GS>";
				case 0x1E: return "<RS>";
				case 0x1F: return "<US>";
				case 0xA0: return "·";

				default:
				{
					if(0x00 < ch && ch <= 'Z'-'A'+1)
						return "^" + std::string(1, ch-1+'A');
					else if(ch < 0x20 || (0x7E < ch && ch < 0xA0))
						return "◆";
					else if(SpaceCharacters.find(ch) != SpaceCharacters.end())
						return text::format("<U+%04X>", ch);
				}
				break;
			}
			return NULL_STR;
		}

		static void draw_line (CGPoint pos, std::string const& text, CGColorRef color, CTFontRef font, CGContextRef context, bool isFlipped)
		{
			ASSERT(utf8::is_valid(text.begin(), text.end()));

			CFMutableAttributedStringRef str = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
			CFAttributedStringReplaceString(str, CFRangeMake(0, 0), cf::wrap(text));
			CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTFontAttributeName, font);
			CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTForegroundColorAttributeName, color);
			CTLineRef line = CTLineCreateWithAttributedString(str);
			CFRelease(str);

			CGContextSaveGState(context);
			if(isFlipped)
				CGContextConcatCTM(context, CGAffineTransformMake(1, 0, 0, -1, 0, 2 * pos.y));
			CGContextSetTextPosition(context, pos.x, pos.y);
			CTLineDraw(line, context);
			CGContextRestoreGState(context);

			CFRelease(line);
		}
	}

	// =======================
	// = paragraph_t::node_t =
	// =======================

	void paragraph_t::node_t::insert (size_t i, size_t len)
	{
		_length += len;
		_line.reset();
	}

	void paragraph_t::node_t::erase (size_t from, size_t to)
	{
		ASSERT_LE(from, to); ASSERT_LE(to, _length);
		_length -= to - from;
		_line.reset();
	}

	void paragraph_t::node_t::did_update_scopes (size_t from, size_t to)
	{
		_line.reset();
	}

	void paragraph_t::node_t::layout (CGFloat x, CGFloat tabWidth, theme_ptr const& theme, bool softWrap, size_t wrapColumn, ct::metrics_t const& metrics, ng::buffer_t const& buffer, size_t bufferOffset, std::string const& fillStr)
	{
		if(_line)
			return;

		switch(_type)
		{
			case kNodeTypeText:
			{
				_line = std::make_shared<ct::line_t>(buffer.substr(bufferOffset, bufferOffset + _length), buffer.scopes(bufferOffset, bufferOffset + _length), theme, nullptr);
			}
			break;

			case kNodeTypeUnprintable:
			{
				scope::scope_t scope = buffer.scope(bufferOffset).right;
				scope.push_scope("deco.unprintable");
				_line = std::make_shared<ct::line_t>(representation_for(utf8::to_ch(buffer.substr(bufferOffset, bufferOffset + _length))), std::map<size_t, scope::scope_t>{ { 0, scope } }, theme, nullptr);
			}
			break;

			case kNodeTypeTab:
			{
				update_tab_width(x, tabWidth, metrics);
			}
			break;

			case kNodeTypeFolding:
			{
				_width = round(metrics.cap_height() * kFoldingDotsRatio);
			}
			break;

			case kNodeTypeSoftBreak:
			{
				scope::context_t const context = buffer.scope(bufferOffset);
				scope::scope_t scope = shared_prefix(context.left, context.right);
				scope.push_scope("deco.indented-wrap");
				_line = std::make_shared<ct::line_t>(fillStr, std::map<size_t, scope::scope_t>{ { 0, scope } }, theme, nullptr);
			}
			break;
		}
	}

	void paragraph_t::node_t::reset_font_metrics (ct::metrics_t const& metrics)
	{
		_line.reset();
	}

	CGFloat paragraph_t::node_t::width () const
	{
		return _line ? _line->width() : _width;
	}

	void paragraph_t::node_t::update_tab_width (CGFloat x, CGFloat tabWidth, ct::metrics_t const& metrics)
	{
		double r = remainder(x, tabWidth);
		_width = (r < 0 ? 0 : tabWidth) - r;
		if(_width < 0.5 * metrics.column_width())
			_width += tabWidth;
	}

	void paragraph_t::node_t::draw_background (theme_ptr const& theme, ng::context_t const& context, bool isFlipped, CGRect visibleRect, ng::invisibles_t const& invisibles, CGColorRef backgroundColor, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor, CGFloat lineHeight) const
	{
		if(_line)
			_line->draw_background(CGPointMake(anchor.x, anchor.y), lineHeight, context, isFlipped, backgroundColor);

		if(_type != kNodeTypeText)
		{
			scope::scope_t scope = _type == kNodeTypeSoftBreak ? buffer.scope(bufferOffset).left : buffer.scope(bufferOffset).right;
			switch(_type)
			{
				case kNodeTypeUnprintable: scope.push_scope("deco.unprintable");   break;
				case kNodeTypeFolding:     scope.push_scope("deco.folding");       break;
				case kNodeTypeSoftBreak:   scope.push_scope("deco.indented-wrap"); break;
			}

			styles_t const styles = theme->styles_for_scope(scope);
			if(!CFEqual(backgroundColor, styles.background()))
			{
				CGFloat x1 = round(anchor.x);
				CGFloat x2 = round(_type == kNodeTypeSoftBreak || _type == kNodeTypeNewline ? CGRectGetMaxX(visibleRect) : anchor.x + _width);
				render::fill_rect(context, styles.background(), CGRectMake(x1, anchor.y, x2 - x1, lineHeight));
			}
		}
	}

	void paragraph_t::node_t::draw_foreground (theme_ptr const& theme, ng::context_t const& context, bool isFlipped, CGRect visibleRect, ng::invisibles_t const& invisibles, ng::buffer_t const& buffer, size_t bufferOffset, std::vector< std::pair<size_t, size_t> > const& misspelled, CGPoint anchor, CGFloat baseline) const
	{
		if(_line)
			_line->draw_foreground(CGPointMake(anchor.x, anchor.y + baseline), context, isFlipped, misspelled);

		if(invisibles.enabled || (_type != kNodeTypeTab && _type != kNodeTypeNewline))
		{
			std::string str = NULL_STR;
			scope::scope_t scope = buffer.scope(bufferOffset).right;
			switch(_type)
			{
				case kNodeTypeTab:
					str = invisibles.tab;
					scope.push_scope("deco.invisible.tab");
				break;
				case kNodeTypeNewline:
					str = invisibles.newline;
					scope.push_scope("deco.invisible.newline");
				break;
				case kNodeTypeFolding:
					scope.push_scope("deco.folding");
				break;
			}

			if(str != NULL_STR)
			{
				styles_t const styles = theme->styles_for_scope(scope);
				draw_line(CGPointMake(anchor.x, anchor.y + baseline), str, styles.foreground(), styles.font(), context, isFlipped);
			}

			if(_type == kNodeTypeFolding)
			{
				styles_t const styles = theme->styles_for_scope(scope);

				CGFloat x1 = round(anchor.x);
				CGFloat x2 = round(anchor.x + _width);
				CGFloat y2 = round(anchor.y + baseline);
				CGFloat y1 = y2 - round(_width / kFoldingDotsRatio);

				CGRect rect = CGRectMake(x1, y1, x2 - x1, y2 - y1);
				if(CGImageRef imageMask = context.folding_dots(CGRectGetWidth(rect), CGRectGetHeight(rect)))
				{
					CGContextSaveGState(context);
					CGContextClipToMask(context, rect, imageMask);
					render::fill_rect(context, styles.foreground(), rect);
					CGContextRestoreGState(context);
				}
			}
		}
	}

	// ===============
	// = paragraph_t =
	// ===============

	void paragraph_t::insert (size_t pos, size_t len, ng::buffer_t const& buffer, size_t bufferOffset)
	{
		std::vector<node_t> newNodes;

		std::string const str = buffer.substr(pos, pos + len);
		size_t from = 0, i = 0;
		citerate(ch, diacritics::make_range(str.data(), str.data() + str.size()))
		{
			if(*ch == '\t' || *ch == '\n' || representation_for(*ch) != NULL_STR)
			{
				if(from != i)
					insert_text(pos - bufferOffset + from, i - from);

				if(*ch == '\t')
					insert_tab(pos - bufferOffset + i);
				else if(*ch == '\n')
					insert_newline(pos - bufferOffset + i, ch.length());
				else
					insert_unprintable(pos - bufferOffset + i, ch.length());

				from = i + ch.length();
			}
			i += ch.length();
		}

		if(from != str.size())
			insert_text(pos - bufferOffset + from, str.size() - from);

		_dirty = true;
	}

	void paragraph_t::insert_folded (size_t pos, size_t len, ng::buffer_t const& buffer, size_t bufferOffset)
	{
		_nodes.insert(iterator_at(pos - bufferOffset), node_t(kNodeTypeFolding, len));
		_dirty = true;
	}

	void paragraph_t::erase (size_t from, size_t to, ng::buffer_t const& buffer, size_t bufferOffset)
	{
		ASSERT_LE(bufferOffset, from); ASSERT_LE(to, bufferOffset + length());

		size_t i = bufferOffset;
		for(auto& node : _nodes)
		{
			size_t len = node.length();
			if(i <= from && from < i + len)
			{
				size_t last = std::min(to - i, len);
				node.erase(from - i, last);
				from = i + last;
				if(to - i <= last)
					break;
			}
			i += len;
		}

		for(auto it = _nodes.begin(); it != _nodes.end(); )
		{
			if(it->length() == 0 && it->type() != kNodeTypeSoftBreak)
					it = _nodes.erase(it);
			else	++it;
		}

		if(from != to)
			fprintf(stderr, "error erasing %zu-%zu, %zu\n", from, to, bufferOffset);

		_dirty = true;
	}

	void paragraph_t::did_update_scopes (size_t from, size_t to, ng::buffer_t const& buffer, size_t bufferOffset)
	{
		size_t i = bufferOffset;
		for(auto& node : _nodes)
		{
			node.did_update_scopes(from - i, to - i);
			i += node.length();
		}
		_dirty = true;
	}

	bool paragraph_t::layout (theme_ptr const& theme, bool softWrap, size_t wrapColumn, ct::metrics_t const& metrics, CGRect visibleRect, ng::buffer_t const& buffer, size_t bufferOffset)
	{
		if(!_dirty)
			return false;

		std::vector<node_t> newNodes;
		bool hasFoldings = false;
		for(auto const& node : _nodes)
		{
			if(node.type() != kNodeTypeSoftBreak)
				newNodes.push_back(node);
			hasFoldings = hasFoldings || node.type() == kNodeTypeFolding;
		}
		_nodes.swap(newNodes);

		size_t const tabSize = buffer.indent().tab_size();
		std::string fillStr = NULL_STR;
		if(!hasFoldings && softWrap)
		{
			fillStr = "";
			size_t fillStrWidth = 0;

			std::string str = buffer.substr(bufferOffset, bufferOffset + length());
			ASSERT(utf8::is_valid(str.begin(), str.end()));

			bundles::item_ptr indentedSoftWrapItem;
			scope::context_t scope(buffer.scope(bufferOffset, false).right, buffer.scope(bufferOffset + length(), false).left);
			plist::any_t const& indentedSoftWrapValue = bundles::value_for_setting("indentedSoftWrap", scope, &indentedSoftWrapItem);
			if(indentedSoftWrapItem)
			{
				std::string pattern, format;
				if(plist::get_key_path(indentedSoftWrapValue, "match", pattern) && plist::get_key_path(indentedSoftWrapValue, "format", format))
				{
					if(regexp::match_t const& m = regexp::search(pattern, str))
					{
						std::string tmp = format_string::expand(format, m.captures());
						citerate(ch, diacritics::make_range(tmp.data(), tmp.data() + tmp.size()))
						{
							if(*ch == '\t')
									fillStr.append(std::string(tabSize - (fillStrWidth % tabSize), ' '));
							else	fillStr.append(&ch, ch.length());
							fillStrWidth += (*ch == '\t' ? tabSize - (fillStrWidth % tabSize) : 1);
						}
					}
				}

				if(wrapColumn < fillStrWidth)
				{
					fillStr = "    ";
					fillStrWidth = 4;
				}
			}

			for(auto const& offset : text::soft_breaks(str, wrapColumn, tabSize, fillStrWidth))
				_nodes.insert(iterator_at(offset), node_t(kNodeTypeSoftBreak, 0, fillStrWidth * metrics.column_width()));
		}

		CGFloat x = 0;
		size_t i = bufferOffset;
		for(auto& node : _nodes)
		{
			node.layout(x, tabSize * metrics.column_width(), theme, softWrap, wrapColumn, metrics, buffer, i, fillStr);
			x += node.width();
			i += node.length();
		}

		_dirty = false;
		return true;
	}

	std::vector<paragraph_t::softline_t> paragraph_t::softlines (ct::metrics_t const& metrics, bool softBreaksOnNewline) const
	{
		std::vector<softline_t> softlines;

		CGFloat x = 0, y = 0;
		size_t first = 0, firstOffset = 0, offset = 0;
		CGFloat ascent = 0, descent = 0, leading = 0;
		for(size_t i = 0; i < _nodes.size(); ++i)
		{
			auto node = _nodes.begin() + i;
			if(node->type() == kNodeTypeSoftBreak)
			{
				softlines.emplace_back(firstOffset, x, y, metrics.baseline(ascent), metrics.line_height(ascent, descent, leading), first, i + (softBreaksOnNewline ? 0 : 1));
				firstOffset = offset;
				x = (softBreaksOnNewline ? 0 : node->width());
				y += metrics.line_height(ascent, descent, leading);
				first = i + (softBreaksOnNewline ? 0 : 1);
				ascent = 0;
				descent = 0;
				leading = 0;
			}

			if(node->line())
			{
				CGFloat a, d, l;
				node->line()->width(&a, &d, &l);
				ascent  = std::max(ascent,  a);
				descent = std::max(descent, d);
				leading = std::max(leading, l);
			}

			offset += node->length();
		}

		softlines.emplace_back(firstOffset, x, y, metrics.baseline(ascent), metrics.line_height(ascent, descent, leading), first, _nodes.size());
		return softlines;
	}

	size_t paragraph_t::softline_count (ct::metrics_t const& metrics, bool softBreaksOnNewline) const
	{
		return softlines(metrics, softBreaksOnNewline).size();
	}

	size_t paragraph_t::softline_for_index (ng::index_t const& index, ng::buffer_t const& buffer, size_t bufferOffset, size_t softlineOffset, ct::metrics_t const& metrics, bool softBreaksOnNewline) const
	{
		std::vector<softline_t> const softlines = this->softlines(metrics, softBreaksOnNewline);
		auto it = std::upper_bound(softlines.begin(), softlines.end(), index.index - bufferOffset, [](size_t offset, softline_t const& softline) -> bool {
			return offset < softline.offset;
		});
		ASSERT(it != softlines.begin());
		return softlineOffset + (--it - softlines.begin());
	}

	ng::range_t paragraph_t::range_for_softline (size_t softline, ng::buffer_t const& buffer, size_t bufferOffset, size_t softlineOffset, ct::metrics_t const& metrics, bool softBreaksOnNewline) const
	{
		std::vector<softline_t> const softlines = this->softlines(metrics, softBreaksOnNewline);
		ASSERT_LT(softline - softlineOffset, softlines.size());
		auto it = softlines.begin() + (softline - softlineOffset);
		ng::range_t range(it->offset);
		range.last = ++it != softlines.end() ? it->offset : length();
		return range + bufferOffset;
	}

	std::vector<paragraph_t::node_t>::iterator paragraph_t::iterator_at (size_t i)
	{
		size_t from = 0;
		iterate(node, _nodes)
		{
			if(from == i)
				return node;
			else if(from < i && i < from + node->length())
			{
				ASSERT_EQ(node->type(), kNodeTypeText);
				size_t len = node->length() - (i - from);
				node->erase(i - from, node->length());
				return _nodes.insert(++node, node_t(kNodeTypeText, len));
			}
			from += node->length();
		}
		return _nodes.end();
	}

	void paragraph_t::insert_text (size_t i, size_t len)
	{
		size_t from = 0;
		for(auto& node : _nodes)
		{
			if(from <= i && i <= from + node.length() && node.type() == kNodeTypeText)
				return node.insert(i - from, len);
			from += node.length();
		}
		_nodes.insert(iterator_at(i), node_t(kNodeTypeText, len));
	}

	void paragraph_t::insert_tab (size_t i)
	{
		_nodes.insert(iterator_at(i), node_t(kNodeTypeTab, 1, 10));
	}

	void paragraph_t::insert_unprintable (size_t i, size_t len)
	{
		_nodes.insert(iterator_at(i), node_t(kNodeTypeUnprintable, len));
	}

	void paragraph_t::insert_newline (size_t i, size_t len)
	{
		_nodes.insert(iterator_at(i), node_t(kNodeTypeNewline, len));
	}

	void paragraph_t::set_wrapping (bool softWrap, size_t wrapColumn, ct::metrics_t const& metrics)
	{
		_dirty = true;
	}

	void paragraph_t::set_tab_size (size_t tabSize, ct::metrics_t const& metrics)
	{
		double const tabWidth = tabSize * metrics.column_width();

		auto lines = softlines(metrics);
		for(size_t i = 0; i < lines.size(); ++i)
		{
			CGFloat x = lines[i].x;
			foreach(node, _nodes.begin() + lines[i].first, _nodes.begin() + lines[i].last)
			{
				if(node->type() == kNodeTypeTab)
					node->update_tab_width(x, tabWidth, metrics);
				x += node->width();
			}
		}
	}

	void paragraph_t::reset_font_metrics (ct::metrics_t const& metrics)
	{
		_dirty = true;
		for(auto& node : _nodes)
			node.reset_font_metrics(metrics);
	}

	size_t paragraph_t::length () const
	{
		size_t res = 0;
		for(auto const& node : _nodes)
			res += node.length();
		return res;
	}

	CGFloat paragraph_t::width () const
	{
		CGFloat x = 0, res = 0;
		for(auto const& node : _nodes)
		{
			if(node.type() == kNodeTypeSoftBreak)
				x = 0;
			x += node.width();
			res = std::max(x, res);
		}
		return res;
	}

	CGFloat paragraph_t::height (ct::metrics_t const& metrics) const
	{
		auto lines = softlines(metrics);
		return lines.back().y + lines.back().height;
	}

	ng::index_t paragraph_t::index_at_point (CGPoint point, ct::metrics_t const& metrics, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor) const
	{
		auto lines = softlines(metrics);
		for(size_t i = 0; i < lines.size(); ++i)
		{
			if(anchor.y + lines[i].y <= point.y && point.y < anchor.y + lines[i].y + lines[i].height)
			{
				CGFloat x = lines[i].x;
				size_t offset = lines[i].offset;
				foreach(node, _nodes.begin() + lines[i].first, _nodes.begin() + lines[i].last)
				{
					if(node->type() == kNodeTypeSoftBreak)
					{
						size_t res = bufferOffset + offset;
						if(i+1 != lines.size() && res == bufferOffset + lines[i+1].offset)
							res -= buffer[res-1].size();
						return res;
					}
					else if(node->type() == kNodeTypeNewline)
					{
						size_t carry = point.x > anchor.x + x ? (size_t)floor((point.x - (anchor.x + x)) / metrics.column_width()) : 0;
						return ng::index_t(bufferOffset + offset, carry);
					}

					if(point.x <= anchor.x + x)
						return bufferOffset + offset;
					else if(anchor.x + x < point.x && point.x < anchor.x + x + node->width())
					{
						CGFloat delta = point.x - (anchor.x + x);
						if(node->type() == kNodeTypeText && node->line())
						{
							size_t res = bufferOffset + offset + node->line()->index_for_offset(delta);
							if(i+1 != lines.size() && res == bufferOffset + lines[i+1].offset)
								res -= buffer[res-1].size();
							return res;
						}
						return bufferOffset + offset + lround(delta / node->width()) * node->length();
					}

					x += node->width();
					offset += node->length();
				}
			}
		}
		return bufferOffset + length();
	}

	CGRect paragraph_t::rect_at_index (ng::index_t const& index, ct::metrics_t const& metrics, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor, bool bol_as_eol) const
	{
		size_t needle = index.index - bufferOffset;
		CGFloat caretOffset = index.carry * metrics.column_width();

		auto lines = softlines(metrics);
		for(size_t i = 0; i < lines.size(); ++i)
		{
			if(lines[i].offset <= needle && (i+1 == lines.size() || needle < lines[i+1].offset || (bol_as_eol && needle == lines[i+1].offset)))
			{
				CGFloat x = lines[i].x, y = lines[i].y;
				size_t offset = lines[i].offset;
				foreach(node, _nodes.begin() + lines[i].first, _nodes.begin() + lines[i].last)
				{
					if(offset <= needle && needle < offset + node->length())
					{
						if(node->type() == kNodeTypeText && node->line())
							return CGRectMake(anchor.x + x + node->line()->offset_for_index(needle - offset) + caretOffset, anchor.y + y, metrics.column_width(), lines[i].height);
						return CGRectMake(anchor.x + x + (needle - offset) * node->width() / node->length() + caretOffset, anchor.y + y, 1, lines[i].height);
					}

					x += node->width();
					offset += node->length();
				}
				return CGRectMake(anchor.x + x + caretOffset, anchor.y + y, 1, lines[i].height);
			}
		}

		return CGRectMake(anchor.x + caretOffset, anchor.y, 1, lines.back().height);
	}

	ng::line_record_t paragraph_t::line_record_for (size_t line, size_t pos, ct::metrics_t const& metrics, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor) const
	{
		size_t needle = pos - bufferOffset;

		auto lines = softlines(metrics);
		CGFloat y = anchor.y;
		for(size_t i = 0; i < lines.size(); ++i)
		{
			if(lines[i].offset <= needle && (i+1 == lines.size() || needle < lines[i+1].offset))
				return ng::line_record_t(line, lines[i].offset, y, y + lines[i].height, lines[i].baseline);
			y += lines[i].height;
		}
		return ng::line_record_t(line, 0, 0, 0, 0);
	}

	ng::range_t paragraph_t::folded_range_at_point (CGPoint point, ct::metrics_t const& metrics, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor) const
	{
		auto lines = softlines(metrics);
		for(size_t i = 0; i < lines.size(); ++i)
		{
			if(anchor.y + lines[i].y <= point.y && point.y < anchor.y + lines[i].y + lines[i].height)
			{
				CGFloat x = lines[i].x;
				size_t offset = lines[i].offset;

				foreach(node, _nodes.begin() + lines[i].first, _nodes.begin() + lines[i].last)
				{
					if(point.x <= anchor.x + x)
						return {};
					else if(anchor.x + x < point.x && point.x < anchor.x + x + node->width() && node->type() == kNodeTypeFolding)
						return ng::range_t{ bufferOffset + offset, bufferOffset + offset + node->length() };

					x += node->width();
					offset += node->length();
				}
			}
		}
		return {};
	}

	size_t paragraph_t::bol (size_t index, ng::buffer_t const& buffer, size_t bufferOffset) const
	{
		size_t i = bufferOffset;
		size_t bol = i;
		for(auto const& node : _nodes)
		{
			if(index < i)
				break;
			i += node.length();
			if(node.type() == kNodeTypeSoftBreak)
				bol = i;
		}
		return bol;
	}

	size_t paragraph_t::eol (size_t index, ng::buffer_t const& buffer, size_t bufferOffset) const
	{
		size_t i = bufferOffset;
		for(auto const& node : _nodes)
		{
			if(index < i && node.type() == kNodeTypeSoftBreak)
				return i - buffer[i-1].size();
			if(index <= i && node.type() == kNodeTypeNewline)
				return i;
			i += node.length();
		}
		return i;
	}

	size_t paragraph_t::index_left_of (size_t index, ng::buffer_t const& buffer, size_t bufferOffset) const
	{
		if(index != bufferOffset)
			index -= buffer[index-1].size();;

		size_t i = bufferOffset;
		for(auto const& node : _nodes)
		{
			if(i < index && index < i + node.length() && node.type() == kNodeTypeFolding)
				return i;
			i += node.length();
		}

		return index;
	}

	size_t paragraph_t::index_right_of (size_t index, ng::buffer_t const& buffer, size_t bufferOffset) const
	{
		if(index != bufferOffset + length())
			index += buffer[index].size();

		size_t i = bufferOffset;
		for(auto const& node : _nodes)
		{
			if(i < index && index < i + node.length() && node.type() == kNodeTypeFolding)
				return i + node.length();
			i += node.length();
		}

		return index;
	}

	void paragraph_t::draw_background (theme_ptr const& theme, ct::metrics_t const& metrics, ng::context_t const& context, bool isFlipped, CGRect visibleRect, ng::invisibles_t const& invisibles, CGColorRef backgroundColor, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor) const
	{
		auto lines = softlines(metrics);
		for(size_t i = 0; i < lines.size(); ++i)
		{
			CGFloat x = lines[i].x;
			size_t offset = lines[i].offset;
			foreach(node, _nodes.begin() + lines[i].first, _nodes.begin() + lines[i].last)
			{
				node->draw_background(theme, context, isFlipped, visibleRect, invisibles, backgroundColor, buffer, bufferOffset + offset, CGPointMake(anchor.x + x, anchor.y + lines[i].y), lines[i].height);
				x += node->width();
				offset += node->length();
			}
		}
	}

	void paragraph_t::draw_foreground (theme_ptr const& theme, ct::metrics_t const& metrics, ng::context_t const& context, bool isFlipped, CGRect visibleRect, ng::invisibles_t const& invisibles, ng::buffer_t const& buffer, size_t bufferOffset, ng::ranges_t const& selection, CGPoint anchor) const
	{
		CGContextSetTextMatrix(context, CGAffineTransformMake(1, 0, 0, 1, 0, 0));

		auto lines = softlines(metrics, false);
		for(size_t i = 0; i < lines.size(); ++i)
		{
			CGFloat x = lines[i].x;
			size_t offset = bufferOffset + lines[i].offset;
			foreach(node, _nodes.begin() + lines[i].first, _nodes.begin() + lines[i].last)
			{
				std::vector< std::pair<size_t, size_t> > misspelled;
				if(node->type() == kNodeTypeText)
				{
					auto misspellings = buffer.misspellings(offset, offset + node->length());
					for(auto it = misspellings.begin(); it != misspellings.end(); )
					{
						bool flag = it->second;
						size_t from = it->first;
						size_t to = ++it != misspellings.end() ? it->first : node->length();
						if(flag)
						{
							bool intersects = false;
							for(auto const& range : selection)
								intersects = intersects || !(to + offset < range.min().index || range.max().index < from + offset);

							if(!intersects)
								misspelled.push_back(std::make_pair(from, to));
						}
					}
				}

				node->draw_foreground(theme, context, isFlipped, visibleRect, invisibles, buffer, offset, misspelled, CGPointMake(anchor.x + x, anchor.y + lines[i].y), lines[i].baseline);
				x += node->width();
				offset += node->length();
			}
		}
	}

	// ========
	// = to_s =
	// ========

	std::string to_s (paragraph_t const& paragraph)
	{
		return "paragraph";
	}

} /* ng */
