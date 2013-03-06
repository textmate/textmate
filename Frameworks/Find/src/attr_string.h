// A C++ wrapper around NSAttributedString
// Ciarán Walsh, 2008
// Visit http://github.com/ciaran/attr_string/tree/master for the latest version

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#import <string>
#import <vector>
#import <stack>

#ifndef sizeofA
#define sizeofA(a) (sizeof(a)/sizeof(a[0]))
#endif

namespace ns
{
	namespace style
	{
		enum type
		{
			bold,      unbold,
			italic,    unitalic,
			underline, nounderline,
			emboss,    noemboss,

			push,      pop,
		};
		struct background
		{
			background(NSColor* color) : color(color) {}
			NSColor* color;
		};
		struct line_break
		{
			line_break(NSUInteger mode) : mode(mode) {}
			NSUInteger mode;
		};
	}

	struct attr_string_t
	{
	private:
		std::string _string;
		struct attr_t { size_t pos; NSString* attr; id value; };
		std::vector<attr_t> _attributes;

		attr_string_t& set_attribute (NSString* name, id value)
		{
			_attributes.push_back((attr_t){ _string.length(), name, value });
			return *this;
		}

	public:
		attr_string_t (char const* s = "") : _string(s) {}
		template <typename T> attr_string_t (T arg) : _string("") { add(arg); }

		template <typename T> attr_string_t operator<< (T value) const { return attr_string_t(*this).add(value); }

		attr_string_t& add (const std::string& string) { _string.append(string); return *this;                              }
		attr_string_t& add (char const* text)          { _string.append(text); return *this;                                }
		attr_string_t& add (NSString* text)            { return add([text UTF8String]);                                     }
		attr_string_t& add (style::type value)         { return set_attribute(nil, [NSNumber numberWithInt:value]);         }
		attr_string_t& add (style::background value)   { return set_attribute(NSBackgroundColorAttributeName, value.color); }
		attr_string_t& add (style::line_break value)
		{
			// FIXME here we create a new paragraph style, but there may be one already in effect
			// we should instead copy and mutate the current style if there is one
			NSMutableParagraphStyle* paragraph = [NSMutableParagraphStyle new];
			[paragraph setLineBreakMode:value.mode];
			return add(paragraph);
		}
		attr_string_t& add (NSImage* image)
		{
			if(image)
			{
				NSFileWrapper* fileWrapper = [NSFileWrapper new];
				[fileWrapper setIcon:image];

				NSTextAttachment* textAttachment = [[NSTextAttachment alloc] initWithFileWrapper:fileWrapper];
				set_attribute(NSAttachmentAttributeName, textAttachment);
				add("\uFFFC"); // This is the object replacement character for the text attachment
				set_attribute(NSAttachmentAttributeName, NULL);
			}
			return *this;
		}
		attr_string_t& add (NSURL* link)
		{
			if(link)
			{
				set_attribute(NSLinkAttributeName, [link absoluteString]);
				add([NSColor blueColor]);
				add(style::underline);
			}
			return *this;
		}
		attr_string_t& add (const attr_string_t& attr_string)
		{
			add(style::push);
			for(std::vector<attr_t>::const_iterator it = attr_string._attributes.begin(); it != attr_string._attributes.end(); ++it)
			{
				attr_t attr;
				attr.attr  = it->attr;
				attr.value = it->value;
				attr.pos   = it->pos + _string.length();
				_attributes.push_back(attr);
			}
			add(attr_string._string);
			add(style::pop);
			return *this;
		}

		inline NSString* attribute_for (NSFont*)                  { return NSFontAttributeName;                    }
		inline NSString* attribute_for (NSColor*)                 { return NSForegroundColorAttributeName;         }
		inline NSString* attribute_for (NSShadow*)                { return NSShadowAttributeName;                  }
		inline NSString* attribute_for (NSParagraphStyle*)        { return NSParagraphStyleAttributeName;          }
		inline NSString* attribute_for (NSMutableParagraphStyle*) { return NSParagraphStyleAttributeName;          }
		template <typename T> attr_string_t& add (T arg)          { return set_attribute(attribute_for(arg), arg); }

		NSAttributedString* get () const
		{
			std::stack<NSDictionary*> attribute_stack;
			NSMutableDictionary* attributes   = [NSMutableDictionary new];
			NSMutableAttributedString* result = [NSMutableAttributedString new];
			size_t last_pos = 0;

			for(std::vector<attr_t>::const_iterator it = _attributes.begin(); it != _attributes.end(); ++it)
			{
				if(last_pos < it->pos)
					[result appendAttributedString:[[NSAttributedString alloc] initWithString:@(_string.substr(last_pos, it->pos - last_pos).c_str()) attributes:attributes]];
				last_pos = it->pos;

				NSString* attr = it->attr;
				id value       = it->value;

				if(!it->attr)
				{
					static struct { style::type style; NSFontTraitMask trait; } const FontTraits[] = {
						{ style::bold,     NSBoldFontMask     },
						{ style::unbold,   NSUnboldFontMask   },
						{ style::italic,   NSItalicFontMask   },
						{ style::unitalic, NSUnitalicFontMask },
					};

					style::type style     = (style::type)[value intValue];
					bool did_handle_style = false;

					// Handle font trait styles
					for(int i = 0; i < sizeofA(FontTraits); ++i)
					{
						if(style == FontTraits[i].style)
						{
							attr             = NSFontAttributeName;
							value            = [[NSFontManager sharedFontManager] convertFont:[attributes objectForKey:NSFontAttributeName] toHaveTrait:FontTraits[i].trait];
							did_handle_style = true;
						}
					}

					if(!did_handle_style)
					{
						// Handle custom styles
						switch(style)
						{
							case style::underline:
								attr  = NSUnderlineStyleAttributeName;
								value = @(NSUnderlineStyleSingle);
								break;
							case style::nounderline:
								attr  = NSUnderlineStyleAttributeName;
								value = nil;
								break;
							case style::emboss:
							{
								NSShadow* shadow = [NSShadow new];
								[shadow setShadowColor:[NSColor colorWithCalibratedWhite:1 alpha:0.7]];
								[shadow setShadowOffset:NSMakeSize(0,-1)];
								[shadow setShadowBlurRadius:1];
								attr  = NSShadowAttributeName;
								value = shadow;
							}
							break;
							case style::noemboss:
								attr  = NSShadowAttributeName;
								value = nil;
								break;
							case style::push:
								attribute_stack.push([attributes copy]);
								[attributes removeAllObjects];
								value = attr = nil;
								break;
							case style::pop:
								assert(!attribute_stack.empty());
								[attributes removeAllObjects];
								[attributes addEntriesFromDictionary:attribute_stack.top()];
								attribute_stack.pop();
								value = attr = nil;
								break;
						}
					}
				}

				if(value)
					[attributes setObject:value forKey:attr];
				else if(attr)
					[attributes removeObjectForKey:attr];
			}

			if(last_pos < _string.length())
				[result appendAttributedString:[[NSAttributedString alloc] initWithString:@(_string.substr(last_pos, _string.length() - last_pos).c_str()) attributes:attributes]];

			return result;
		}

		operator NSAttributedString* () const { return get(); }

		explicit operator bool () const { return !is_empty(); };
		bool is_empty () const { return _string.size() == 0; };
	};

	template<typename T> attr_string_t operator<<(style::type left, T right) { return attr_string_t(left) << right; }
}
