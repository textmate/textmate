#include <regexp/src/snippet.h>

void test_snippet ()
{
	std::string src = "foo\n$0\nbar\n";
	snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "" /* indent string */, text::indent_t(2, 2, false), nullptr /* run command callback */);

	OAK_ASSERT_EQ(s.text, "foo\n\nbar\n");
	OAK_ASSERT_EQ(s.fields.size(), 1);
	OAK_ASSERT_EQ(s.fields.begin()->second->range.from.offset, 4);
	OAK_ASSERT_EQ(s.fields.begin()->second->range.to.offset,   4);
}

void test_indent ()
{
	std::string src = "foo\n$0\nbar\n";
	snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "\t" /* indent string */, text::indent_t(2, 2, false), nullptr /* run command callback */);

	OAK_ASSERT_EQ(s.text, "foo\n\t\n\tbar\n\t");
	OAK_ASSERT_EQ(s.fields.size(), 1);
	OAK_ASSERT_EQ(s.fields.begin()->second->range.from.offset, 5);
	OAK_ASSERT_EQ(s.fields.begin()->second->range.to.offset,   5);
}

void test_soft_tabs_1 ()
{
	std::string src = "{\n\t$0\n}\n";
	snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "\t" /* indent string */, text::indent_t(2, 2, true), nullptr /* run command callback */);

	OAK_ASSERT_EQ(s.text, "{\n\t  \n\t}\n\t");
	OAK_ASSERT_EQ(s.fields.size(), 1);
	OAK_ASSERT_EQ(s.fields.begin()->second->range.from.offset, 5);
	OAK_ASSERT_EQ(s.fields.begin()->second->range.to.offset,   5);
}

void test_soft_tabs_2 ()
{
	std::string src = "{\n\t$0\n}\n";
	snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "  " /* indent string */, text::indent_t(2, 2, true), nullptr /* run command callback */);

	OAK_ASSERT_EQ(s.text, "{\n    \n  }\n  ");
	OAK_ASSERT_EQ(s.fields.size(), 1);
	OAK_ASSERT_EQ(s.fields.begin()->second->range.from.offset, 6);
	OAK_ASSERT_EQ(s.fields.begin()->second->range.to.offset,   6);
}

void test_replace ()
{
	std::string src = "- (${1:id})${2:${TM_SELECTED_TEXT:method}}${3::(${4:id})${5:${4/(NS([AEIOQUY])?(\\w+).*)|(.)?.*/(?1:a(?2:n$2)$3:(?4:anArgument))/}}}\n{$0${1/^(void|IBAction)$|(.*)/(?2:\n\treturn nil;)/}\n}";
	snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "" /* indent string */, text::indent_t(3, 3, false), nullptr /* run command callback */);

	OAK_ASSERT_EQ(s.text, "- (id)method:(id)anArgument\n{\n\treturn nil;\n}");
	OAK_ASSERT_EQ(s.fields.size(), 6);
	OAK_ASSERT_EQ(s.fields[0]->range.from.offset, 29); // $0
	OAK_ASSERT_EQ(s.fields[0]->range.to.offset,   29);
	OAK_ASSERT_EQ(s.fields[1]->range.from.offset,  3); // $1
	OAK_ASSERT_EQ(s.fields[1]->range.to.offset,    5);

	s.replace(s.fields[1]->range, "void");
	OAK_ASSERT_EQ(s.text, "- (void)method:(id)anArgument\n{\n}");

	s.replace(s.fields[1]->range, "int");
	OAK_ASSERT_EQ(s.text, "- (int)method:(id)anArgument\n{\n\treturn nil;\n}");
}

void test_replace_soft_tabs ()
{
	std::string src = "- (${1:id})${2:${TM_SELECTED_TEXT:method}}${3::(${4:id})${5:${4/(NS([AEIOQUY])?(\\w+).*)|(.)?.*/(?1:a(?2:n$2)$3:(?4:anArgument))/}}}\n{$0${1/^(void|IBAction)$|(.*)/(?2:\n\treturn nil;)/}\n}";
	snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "" /* indent string */, text::indent_t(3, 3, true), nullptr /* run command callback */);

	OAK_ASSERT_EQ(s.text, "- (id)method:(id)anArgument\n{\n   return nil;\n}");
	OAK_ASSERT_EQ(s.fields.size(), 6);
	OAK_ASSERT_EQ(s.fields[0]->range.from.offset, 29); // $0
	OAK_ASSERT_EQ(s.fields[0]->range.to.offset,   29);
	OAK_ASSERT_EQ(s.fields[1]->range.from.offset,  3); // $1
	OAK_ASSERT_EQ(s.fields[1]->range.to.offset,    5);

	s.replace(s.fields[1]->range, "void");
	OAK_ASSERT_EQ(s.text, "- (void)method:(id)anArgument\n{\n}");

	s.replace(s.fields[1]->range, "int");
	OAK_ASSERT_EQ(s.text, "- (int)method:(id)anArgument\n{\n   return nil;\n}");
}

void test_indent_replace_soft_tabs ()
{
	std::string src = "- (${1:id})${2:${TM_SELECTED_TEXT:method}}${3::(${4:id})${5:${4/(NS([AEIOQUY])?(\\w+).*)|(.)?.*/(?1:a(?2:n$2)$3:(?4:anArgument))/}}}\n{$0${1/^(void|IBAction)$|(.*)/(?2:\n\treturn nil;)/}\n}";
	snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "   " /* indent string */, text::indent_t(3, 3, true), nullptr /* run command callback */);

	OAK_ASSERT_EQ(s.text, "- (id)method:(id)anArgument\n   {\n      return nil;\n   }");
	OAK_ASSERT_EQ(s.fields.size(), 6);
	OAK_ASSERT_EQ(s.fields[0]->range.from.offset, 32); // $0
	OAK_ASSERT_EQ(s.fields[0]->range.to.offset,   32);
	OAK_ASSERT_EQ(s.fields[1]->range.from.offset,  3); // $1
	OAK_ASSERT_EQ(s.fields[1]->range.to.offset,    5);

	s.replace(s.fields[1]->range, "void");
	OAK_ASSERT_EQ(s.text, "- (void)method:(id)anArgument\n   {\n   }");

	s.replace(s.fields[1]->range, "int");
	OAK_ASSERT_EQ(s.text, "- (int)method:(id)anArgument\n   {\n      return nil;\n   }");
}

void test_repopulating_mirrors ()
{
	std::string src = "${1/.+/-/}${1:x}${1/.+/-/}";
	snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "" /* indent string */, text::indent_t(2, 2, false), nullptr /* run command callback */);

	OAK_ASSERT_EQ(s.text, "-x-");
	OAK_ASSERT_EQ(s.fields.size(), 2);
	OAK_ASSERT_EQ(s.fields[0]->range.from.offset, 3);
	OAK_ASSERT_EQ(s.fields[0]->range.to.offset,   3);
	OAK_ASSERT_EQ(s.fields[1]->range.from.offset, 1);
	OAK_ASSERT_EQ(s.fields[1]->range.to.offset,   2);

	s.replace(s.fields[1]->range, "");
	OAK_ASSERT_EQ(s.text, "");
	OAK_ASSERT_EQ(s.fields.size(), 2);
	OAK_ASSERT_EQ(s.fields[0]->range.from.offset, 0);
	OAK_ASSERT_EQ(s.fields[0]->range.to.offset,   0);
	OAK_ASSERT_EQ(s.fields[1]->range.from.offset, 0);
	OAK_ASSERT_EQ(s.fields[1]->range.to.offset,   0);

	auto res = s.replace(s.fields[1]->range, "y");
	OAK_ASSERT_EQ(s.text, "-y-");
	OAK_ASSERT_EQ(s.fields.size(), 2);
	OAK_ASSERT_EQ(s.fields[0]->range.from.offset, 3);
	OAK_ASSERT_EQ(s.fields[0]->range.to.offset,   3);
	OAK_ASSERT_EQ(s.fields[1]->range.from.offset, 1);
	OAK_ASSERT_EQ(s.fields[1]->range.to.offset,   2);
}
