#include <regexp/src/indent.h>
#include <text/src/format.h>
#include <oak/oak.h>

struct line_t
{
	size_t indent; std::string content;
};

static std::map<indent::pattern_type, regexp::pattern_t> const& patterns ()
{
	static std::map<indent::pattern_type, regexp::pattern_t> const res =
	{
		{ indent::pattern_type::kIncrease,
			"(?x)"
			"    ^ .* \\{ [^}\"']* $"
			"|   ^ \\s* (public|private|protected): \\s* $"
			"|   ^ \\s* @(public|private|protected) \\s* $",
		},
		{ indent::pattern_type::kDecrease,
			"(?x)"
			"	    ^ (.*\\*/)? \\s* \\} ( [^}{\"']* \\{ | \\s* while \\s* \\( .* )? [;\\s]* (//.*|/\\*.*\\*/\\s*)? $"
			"	|   ^ \\s* (public|private|protected): \\s* $"
			"	|   ^ \\s* @(public|private|protected) \\s* $",
		},
		{ indent::pattern_type::kIncreaseNext,
			"(?x)^"
			"	 (?! .* [;:{}]                   # do not indent when line ends with ;, :, {, or }\n"
			"	   \\s* (//|/[*] .* [*]/ \\s* $) #  …account for potential trailing comment\n"
			"	 |   @(public|private|protected) # do not indent after obj-c data access keywords\n"
			"	 )"
			"	 .* [^\\s;:{}] \\s* $            # indent next if this one isn’t\n"
			"	                                 #  terminated with ;, :, {, or }\n",
		},
		{ indent::pattern_type::kIgnore,
			"^\\s*((/\\*|\\*/|//|#|template\\b.*?>(?!\\(.*\\))|@protocol|@optional|@interface(?!.*\\{)|@implementation|@end).*)?$"
		}
	};
	return res;
}

static size_t const indentSize = 4;
static size_t const tabSize    = 4;

void test_indent ()
{
	static line_t const lines[] =
	{
		{ 0, "#include <foo>" },
		{ 0, ""               },
		{ 0, "int main ()"    },
		{ 0, "{"              },
		{ 1, "while(true)"    },
		{ 2, "putc('x');"     },
		{ 1, ""               },
		{ 1, "if(true)"       },
		{ 1, "{"              },
		{ 2, "while(true)"    },
		{ 3, "putc('x');"     },
		{ 2, "if(true)"       },
		{ 3, "if(true)"       },
		{ 4, "if(true)"       },
		{ 5, "continue;"      },
		{ 1, "}"              },
		{ 1, ""               },
		{ 1, "return 42;"     },
		{ 0, "}"              },
	};

	indent::fsm_t fsm(indentSize, tabSize);
	iterate(line, lines)
	{
		OAK_MASSERT_EQ(text::format("%td: %s", line - std::begin(lines), line->content.c_str()), fsm.scan_line(line->content, patterns()), indentSize * line->indent);
	}
}

void test_seeding_proper_1 ()
{
	static std::string const seed_lines[] =
	{
		"void main ()",
		"{",
		"	if(true)",
		"		if(true)",
		"			if(true)",
	};

	indent::fsm_t fsm(indentSize, tabSize);
	for(size_t i = sizeofA(seed_lines); i-- && !fsm.is_seeded(seed_lines[i], patterns()); )
		continue;

	static line_t const lines[] =
	{
		{ 4, "continue;" },
		{ 1, "continue;" },
		{ 0, "}"         },
	};

	iterate(line, lines)
	{
		OAK_MASSERT_EQ(text::format("%td: %s", line - std::begin(lines), line->content.c_str()), fsm.scan_line(line->content, patterns()), indentSize * line->indent);
	}
}

void test_seeding_proper_2 ()
{
	static std::string const seed_lines[] =
	{
		"void main ()",
		"{",
		"	if(true)",
		"		if(true)",
		"			if(true)",
		"				continue;",
	};

	indent::fsm_t fsm(indentSize, tabSize);
	for(size_t i = sizeofA(seed_lines); i-- && !fsm.is_seeded(seed_lines[i], patterns()); )
		continue;

	static line_t const lines[] =
	{
		{ 1, "continue;" },
		{ 0, "}"         },
	};

	iterate(line, lines)
	{
		OAK_MASSERT_EQ(text::format("%td: %s", line - std::begin(lines), line->content.c_str()), fsm.scan_line(line->content, patterns()), indentSize * line->indent);
	}
}

void test_seeding_improper ()
{
	static std::string const seed_lines[] =
	{
		"void main ()",
		"{",
		"	if(true)",
		"	if(true)",
		"	if(true)",
	};

	indent::fsm_t fsm(indentSize, tabSize);
	for(size_t i = sizeofA(seed_lines); i-- && !fsm.is_seeded(seed_lines[i], patterns()); )
		continue;

	static line_t const lines[] =
	{
		{ 2, "continue;" },
		{ 1, "continue;" },
		{ 0, "}"         },
	};

	iterate(line, lines)
	{
		OAK_MASSERT_EQ(text::format("%td: %s", line - std::begin(lines), line->content.c_str()), fsm.scan_line(line->content, patterns()), indentSize * line->indent);
	}
}

void test_seeding_extra_indent ()
{
	static std::string const seed_lines[] =
	{
		"void main ()",
		"{",
		"    foo(bar,",
		"            42);",
	};

	indent::fsm_t fsm(indentSize, tabSize);
	for(size_t i = sizeofA(seed_lines); i-- && !fsm.is_seeded(seed_lines[i], patterns()); )
		continue;

	static line_t const lines[] =
	{
		{ 1, "return 0;" },
		{ 0, "}"         },
	};

	iterate(line, lines)
	{
		OAK_MASSERT_EQ(text::format("%td: %s", line - std::begin(lines), line->content.c_str()), fsm.scan_line(line->content, patterns()), indentSize * line->indent);
	}
}

void test_mixing ()
{
	static line_t const lines[] =
	{
		{ 0, "int main ()"  },
		{ 0, "{"            },
		{ 1, "str = { foo," },
		{ 2, "bar };"       },
		{ 1, "}"            },
	};

	indent::fsm_t fsm(indentSize, tabSize);
	iterate(line, lines)
	{
		OAK_MASSERT_EQ(text::format("%td: %s", line - std::begin(lines), line->content.c_str()), fsm.scan_line(line->content, patterns()), indentSize * line->indent);
	}
}
