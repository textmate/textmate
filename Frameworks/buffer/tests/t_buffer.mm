#import <buffer/buffer.h>
#import <text/format.h>
#import <test/bundle_index.h>
#import <oak/duration.h>

static bundles::item_ptr TestGrammarItem;

static class GrammarFixture : public CxxTest::GlobalFixture
{
public:
	bool setUpWorld()
	{
		static std::string TestLanguageGrammar =
			"{	fileTypes      = ( txt );\n"
			"	name           = 'Test';\n"
			"	patterns       = (\n"
			"    { name = 'foo'; match = 'foo'; },\n"
			"    { name = 'bar'; match = 'bar'; },\n"
			"  );\n"
			"	scopeName      = 'test';\n"
			"	uuid           = '978BF73C-B36D-490F-AEBF-74EF2C6EA7D1';\n"
			"}\n";

		test::bundle_index_t bundleIndex;
		TestGrammarItem = bundleIndex.add(bundles::kItemTypeGrammar, TestLanguageGrammar);
		return bundleIndex.commit();
	}

} grammar_fixture;

static class AppKitFixture : public CxxTest::GlobalFixture
{
public:
	bool setUpWorld() { return NSApplicationLoad(); }

} fixture;

class BufferTests : public CxxTest::TestSuite
{
public:
	// void test_copy_constructor ()
	// {
	// 	ng::buffer_t org, dup;
	// 	org.insert(0, "Hello");
	// 	dup = org;
	// 	org.insert(org.size(), ", world!");
	//
	// 	TS_ASSERT_EQUALS(org.substr(0, org.size()), "Hello, world!");
	// 	TS_ASSERT_EQUALS(dup.substr(0, dup.size()), "Hello");
	// }

	void test_subset_replace ()
	{
		struct callback_t : ng::callback_t
		{
			void will_replace (size_t from, size_t to, std::string const& str) { actual.push_back(str); }
			std::vector<std::string> actual;
		};

		static callback_t cb;

		ng::buffer_t buf;
		buf.insert(0, "grød");
		buf.add_callback(&cb);

		buf.replace(0, buf.size(), "græd");
		TS_ASSERT_EQUALS("ø"[0], "æ"[0]);
		TS_ASSERT_EQUALS(buf.substr(0, buf.size()), "græd");
		TS_ASSERT_EQUALS(cb.actual.back(), "æ");

		buf.replace(0, buf.size(), "gr¦d");
		TS_ASSERT_EQUALS("æ"[1], "¦"[1]);
		TS_ASSERT_EQUALS(buf.substr(0, buf.size()), "gr¦d");
		TS_ASSERT_EQUALS(cb.actual.back(), "¦");

		buf.replace(0, buf.size(), "bl¦d");
		TS_ASSERT_EQUALS(buf.substr(0, buf.size()), "bl¦d");
		TS_ASSERT_EQUALS(cb.actual.back(), "bl");

		buf.replace(0, buf.size(), "bl¦gen");
		TS_ASSERT_EQUALS(buf.substr(0, buf.size()), "bl¦gen");
		TS_ASSERT_EQUALS(cb.actual.back(), "gen");

		buf.remove_callback(&cb);
	}

	void test_markup ()
	{
		ng::buffer_t buf;
		buf.insert(0, "Hello");

		TS_ASSERT_EQUALS(to_s(buf), "«text»Hello«/text»");
		TS_ASSERT_EQUALS(to_s(buf, 1, 4), "«text»ell«/text»");
	}

	void test_xml_markup ()
	{
		ng::buffer_t buf;
		buf.insert(0, "Hello <World> & Fun");

		TS_ASSERT_EQUALS(to_xml(buf), "<text>Hello &lt;World> &amp; Fun</text>");
		TS_ASSERT_EQUALS(to_xml(buf, 6, 13), "<text>&lt;World></text>");
	}

	void test_spelling ()
	{
		ng::buffer_t buf;
		buf.set_grammar(TestGrammarItem);
		buf.set_live_spelling(true);
		buf.insert(0, "myfo god\nthat ibs nice\nlamere check\n");
		buf.bump_revision();
		buf.wait_for_repair();

		TS_ASSERT_EQUALS(buf.misspellings(0, buf.size()).size(), 6);

		std::map<size_t, bool> bad = buf.misspellings(9, 23);
		TS_ASSERT_EQUALS(bad.size(), 2);
		static std::map<size_t, bool> const expected = { { 5, true }, { 8, false } };
		TS_ASSERT(bad == expected);
	}

	void test_spelling_2 ()
	{
		ng::buffer_t buf;
		buf.set_grammar(TestGrammarItem);
		buf.set_live_spelling(true);
		buf.insert(0, "it mq xy");
		buf.bump_revision();
		buf.wait_for_repair();

		std::map<size_t, bool> bad = buf.misspellings(0, buf.size());
		TS_ASSERT_EQUALS(bad.size(), 3);
		static std::map<size_t, bool> const expected = { { 3, true }, { 5, false }, { 6, true } };
		TS_ASSERT(bad == expected);
	}

	void test_spelling_3 ()
	{
		ng::buffer_t buf;
		buf.set_grammar(TestGrammarItem);
		buf.set_live_spelling(true);
		buf.insert(0, "it mq xy");
		buf.bump_revision();
		buf.wait_for_repair();

		std::map<size_t, bool> bad = buf.misspellings(4, 7);
		TS_ASSERT_EQUALS(bad.size(), 3);
		static std::map<size_t, bool> const expected = { { 0, true }, { 1, false }, { 2, true } };
		TS_ASSERT(bad == expected);
	}

	void test_spelling_4 ()
	{
		ng::buffer_t buf;
		buf.set_grammar(TestGrammarItem);
		buf.set_live_spelling(true);

		buf.insert(0, "hxllo world");
		buf.bump_revision();
		buf.wait_for_repair();

		buf.replace(1, 2, "e");
		buf.bump_revision();
		buf.wait_for_repair();
		TS_ASSERT_EQUALS(buf.substr(0, buf.size()), "hello world");

		std::map<size_t, bool> bad = buf.misspellings(0, buf.size());
		TS_ASSERT_EQUALS(bad.size(), 0);
	}

	void test_scopes ()
	{
		ng::buffer_t buf;
		buf.set_grammar(TestGrammarItem);
		buf.insert(0, "foobar");
		buf.bump_revision();
		buf.wait_for_repair();

		TS_ASSERT_EQUALS(to_s(buf), "«test»«foo»foo«/foo»«bar»bar«/bar»«/test»");

		TS_ASSERT_EQUALS(to_s(buf.scope( 0).left),  "test");
		TS_ASSERT_EQUALS(to_s(buf.scope( 0).right), "test foo");
		TS_ASSERT_EQUALS(to_s(buf.scope( 1).left),  "test foo");
		TS_ASSERT_EQUALS(to_s(buf.scope( 1).right), "test foo");
		TS_ASSERT_EQUALS(to_s(buf.scope( 3).left),  "test foo");
		TS_ASSERT_EQUALS(to_s(buf.scope( 3).right), "test bar");
		TS_ASSERT_EQUALS(to_s(buf.scope( 4).left),  "test bar");
		TS_ASSERT_EQUALS(to_s(buf.scope( 4).right), "test bar");
		TS_ASSERT_EQUALS(to_s(buf.scope( 6).left),  "test bar");
		// TS_ASSERT_EQUALS(to_s(buf.scope( 6).right), "test");
	}

	void test_sanitize_index ()
	{
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index( 0),  0);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index( 1),  0);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index( 2),  0);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index( 3),  0);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index( 4),  0);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index( 5),  5);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index( 6),  5);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index( 7),  5);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index( 8),  5);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index( 9),  9);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").sanitize_index(10),  9);
		TS_ASSERT_EQUALS(ng::buffer_t("c̄̌𠻵").size(), 9);
	}

	void test_speed ()
	{
		std::string tmp = std::string(0x7F - 0x20, '\0');
		std::iota(tmp.begin(), tmp.end(), 0x20);
		tmp.append("\n");
		while(tmp.size() < 32*1024)
			tmp.insert(tmp.end(), tmp.begin(), tmp.end());
		std::random_shuffle(tmp.begin(), tmp.end());

		oak::duration_t timer;
		ng::buffer_t buf;
		size_t cnt = 50*1024*1024 / tmp.size();
		for(size_t i = 0; i < cnt; ++i)
			buf.insert(buf.size(), tmp);
		printf("%.1f seconds to insert %zu chunks of %s (total %s, %zu lines)\n", timer.duration(), cnt, text::format_size(tmp.size()).c_str(), text::format_size(buf.size()).c_str(), buf.lines());
	}
};
