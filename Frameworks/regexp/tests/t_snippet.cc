#include <regexp/snippet.h>

class SnippetTests : public CxxTest::TestSuite
{
public:
	void test_snippet ()
	{
		std::string src = "foo\n$0\nbar\n";
		snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "" /* indent string */, text::indent_t(2, 2, false), NULL /* run command callback */);

		TS_ASSERT_EQUALS(s.text, "foo\n\nbar\n");
		TS_ASSERT_EQUALS(s.fields.size(), 1);
		TS_ASSERT_EQUALS(s.fields.begin()->second->range.from.offset, 4);
		TS_ASSERT_EQUALS(s.fields.begin()->second->range.to.offset,   4);
	}

	void test_indent ()
	{
		std::string src = "foo\n$0\nbar\n";
		snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "\t" /* indent string */, text::indent_t(2, 2, false), NULL /* run command callback */);

		TS_ASSERT_EQUALS(s.text, "foo\n\t\n\tbar\n\t");
		TS_ASSERT_EQUALS(s.fields.size(), 1);
		TS_ASSERT_EQUALS(s.fields.begin()->second->range.from.offset, 5);
		TS_ASSERT_EQUALS(s.fields.begin()->second->range.to.offset,   5);
	}

	void test_soft_tabs_1 ()
	{
		std::string src = "{\n\t$0\n}\n";
		snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "\t" /* indent string */, text::indent_t(2, 2, true), NULL /* run command callback */);

		TS_ASSERT_EQUALS(s.text, "{\n\t  \n\t}\n\t");
		TS_ASSERT_EQUALS(s.fields.size(), 1);
		TS_ASSERT_EQUALS(s.fields.begin()->second->range.from.offset, 5);
		TS_ASSERT_EQUALS(s.fields.begin()->second->range.to.offset,   5);
	}

	void test_soft_tabs_2 ()
	{
		std::string src = "{\n\t$0\n}\n";
		snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "  " /* indent string */, text::indent_t(2, 2, true), NULL /* run command callback */);

		TS_ASSERT_EQUALS(s.text, "{\n    \n  }\n  ");
		TS_ASSERT_EQUALS(s.fields.size(), 1);
		TS_ASSERT_EQUALS(s.fields.begin()->second->range.from.offset, 6);
		TS_ASSERT_EQUALS(s.fields.begin()->second->range.to.offset,   6);
	}

	void test_replace ()
	{
		std::string src = "- (${1:id})${2:${TM_SELECTED_TEXT:method}}${3::(${4:id})${5:${4/(NS([AEIOQUY])?(\\w+).*)|(.)?.*/(?1:a(?2:n$2)$3:(?4:anArgument))/}}}\n{$0${1/^(void|IBAction)$|(.*)/(?2:\n\treturn nil;)/}\n}";
		snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "" /* indent string */, text::indent_t(3, 3, false), NULL /* run command callback */);

		TS_ASSERT_EQUALS(s.text, "- (id)method:(id)anArgument\n{\n\treturn nil;\n}");
		TS_ASSERT_EQUALS(s.fields.size(), 6);
		TS_ASSERT_EQUALS(s.fields[0]->range.from.offset, 29); // $0
		TS_ASSERT_EQUALS(s.fields[0]->range.to.offset,   29);
		TS_ASSERT_EQUALS(s.fields[1]->range.from.offset,  3); // $1
		TS_ASSERT_EQUALS(s.fields[1]->range.to.offset,    5);

		s.replace(s.fields[1]->range, "void");
		TS_ASSERT_EQUALS(s.text, "- (void)method:(id)anArgument\n{\n}");

		s.replace(s.fields[1]->range, "int");
		TS_ASSERT_EQUALS(s.text, "- (int)method:(id)anArgument\n{\n\treturn nil;\n}");
	}

	void test_replace_soft_tabs ()
	{
		std::string src = "- (${1:id})${2:${TM_SELECTED_TEXT:method}}${3::(${4:id})${5:${4/(NS([AEIOQUY])?(\\w+).*)|(.)?.*/(?1:a(?2:n$2)$3:(?4:anArgument))/}}}\n{$0${1/^(void|IBAction)$|(.*)/(?2:\n\treturn nil;)/}\n}";
		snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "" /* indent string */, text::indent_t(3, 3, true), NULL /* run command callback */);

		TS_ASSERT_EQUALS(s.text, "- (id)method:(id)anArgument\n{\n   return nil;\n}");
		TS_ASSERT_EQUALS(s.fields.size(), 6);
		TS_ASSERT_EQUALS(s.fields[0]->range.from.offset, 29); // $0
		TS_ASSERT_EQUALS(s.fields[0]->range.to.offset,   29);
		TS_ASSERT_EQUALS(s.fields[1]->range.from.offset,  3); // $1
		TS_ASSERT_EQUALS(s.fields[1]->range.to.offset,    5);

		s.replace(s.fields[1]->range, "void");
		TS_ASSERT_EQUALS(s.text, "- (void)method:(id)anArgument\n{\n}");

		s.replace(s.fields[1]->range, "int");
		TS_ASSERT_EQUALS(s.text, "- (int)method:(id)anArgument\n{\n   return nil;\n}");
	}

	void test_indent_replace_soft_tabs ()
	{
		std::string src = "- (${1:id})${2:${TM_SELECTED_TEXT:method}}${3::(${4:id})${5:${4/(NS([AEIOQUY])?(\\w+).*)|(.)?.*/(?1:a(?2:n$2)$3:(?4:anArgument))/}}}\n{$0${1/^(void|IBAction)$|(.*)/(?2:\n\treturn nil;)/}\n}";
		snippet::snippet_t s = snippet::parse(src, std::map<std::string, std::string>(), "   " /* indent string */, text::indent_t(3, 3, true), NULL /* run command callback */);

		TS_ASSERT_EQUALS(s.text, "- (id)method:(id)anArgument\n   {\n      return nil;\n   }");
		TS_ASSERT_EQUALS(s.fields.size(), 6);
		TS_ASSERT_EQUALS(s.fields[0]->range.from.offset, 32); // $0
		TS_ASSERT_EQUALS(s.fields[0]->range.to.offset,   32);
		TS_ASSERT_EQUALS(s.fields[1]->range.from.offset,  3); // $1
		TS_ASSERT_EQUALS(s.fields[1]->range.to.offset,    5);

		s.replace(s.fields[1]->range, "void");
		TS_ASSERT_EQUALS(s.text, "- (void)method:(id)anArgument\n   {\n   }");

		s.replace(s.fields[1]->range, "int");
		TS_ASSERT_EQUALS(s.text, "- (int)method:(id)anArgument\n   {\n      return nil;\n   }");
	}
};
