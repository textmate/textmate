#import <buffer/src/buffer.h>
#import <text/src/format.h>
#import <test/bundle_index.h>
#import <oak/duration.h>

static bundles::item_ptr TestGrammarItem;

void setup_fixtures ()
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
	bundleIndex.commit();

	NSApplicationLoad();
	NSSpellChecker* spellChecker = [NSSpellChecker sharedSpellChecker];
	spellChecker.automaticallyIdentifiesLanguages = NO;
}

void benchmark_insert_50_mb ()
{
	std::string tmp(32*1024, '\0');
	for(size_t i = 0; i < tmp.size(); ++i)
		tmp[i] = (i % 0x61) == 0x60 ? '\n' : 0x20 + (i % 0x61);
	oak::random_shuffle(tmp.begin(), tmp.end());

	ng::buffer_t buf;
	size_t cnt = 50*1024*1024 / tmp.size();
	for(size_t i = 0; i < cnt; ++i)
		buf.insert(buf.size(), tmp);
}

// void test_copy_constructor ()
// {
// 	ng::buffer_t org, dup;
// 	org.insert(0, "Hello");
// 	dup = org;
// 	org.insert(org.size(), ", world!");
//
// 	OAK_ASSERT_EQ(org.substr(0, org.size()), "Hello, world!");
// 	OAK_ASSERT_EQ(dup.substr(0, dup.size()), "Hello");
// }

void test_subset_replace ()
{
	struct callback_t : ng::callback_t
	{
		void will_replace (size_t from, size_t to, char const* buf, size_t len) { actual.emplace_back(buf, len); }
		std::vector<std::string> actual;
	};

	static callback_t cb;

	ng::buffer_t buf;
	buf.insert(0, "grød");
	buf.add_callback(&cb);

	buf.replace(0, buf.size(), "græd");
	OAK_ASSERT_EQ("ø"[0], "æ"[0]);
	OAK_ASSERT_EQ(buf.substr(0, buf.size()), "græd");
	OAK_ASSERT_EQ(cb.actual.back(), "æ");

	buf.replace(0, buf.size(), "gr¦d");
	OAK_ASSERT_EQ("æ"[1], "¦"[1]);
	OAK_ASSERT_EQ(buf.substr(0, buf.size()), "gr¦d");
	OAK_ASSERT_EQ(cb.actual.back(), "¦");

	buf.replace(0, buf.size(), "bl¦d");
	OAK_ASSERT_EQ(buf.substr(0, buf.size()), "bl¦d");
	OAK_ASSERT_EQ(cb.actual.back(), "bl");

	buf.replace(0, buf.size(), "bl¦gen");
	OAK_ASSERT_EQ(buf.substr(0, buf.size()), "bl¦gen");
	OAK_ASSERT_EQ(cb.actual.back(), "gen");

	buf.remove_callback(&cb);
}

void test_markup ()
{
	ng::buffer_t buf;
	buf.insert(0, "Hello");

	OAK_ASSERT_EQ(to_s(buf), "«text»Hello«/text»");
	OAK_ASSERT_EQ(to_s(buf, 1, 4), "«text»ell«/text»");
}

void test_xml_markup ()
{
	ng::buffer_t buf;
	buf.insert(0, "Hello <World> & Fun");

	OAK_ASSERT_EQ(buf.xml_substr(), "<text>Hello &lt;World> &amp; Fun</text>");
	OAK_ASSERT_EQ(buf.xml_substr(6, 13), "<text>&lt;World></text>");
}

void test_spelling ()
{
	ng::buffer_t buf;
	buf.set_grammar(TestGrammarItem);
	buf.set_spelling_language("en");
	buf.set_live_spelling(true);
	buf.insert(0, "myfo god\nthat ibs nice\nlamere check\n");
	buf.bump_revision();
	buf.wait_for_repair();

	OAK_ASSERT_EQ(buf.misspellings(0, buf.size()).size(), 6);

	std::map<size_t, bool> bad = buf.misspellings(9, 23);
	OAK_ASSERT_EQ(bad.size(), 2);
	std::map<size_t, bool> const expected = { { 5, true }, { 8, false } };
	OAK_ASSERT(bad == expected);
}

void test_spelling_2 ()
{
	ng::buffer_t buf;
	buf.set_grammar(TestGrammarItem);
	buf.set_spelling_language("en");
	buf.set_live_spelling(true);
	buf.insert(0, "it mq xy");
	buf.bump_revision();
	buf.wait_for_repair();

	std::map<size_t, bool> bad = buf.misspellings(0, buf.size());
	OAK_ASSERT_EQ(bad.size(), 3);
	std::map<size_t, bool> const expected = { { 3, true }, { 5, false }, { 6, true } };
	OAK_ASSERT(bad == expected);
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
	OAK_ASSERT_EQ(bad.size(), 3);
	std::map<size_t, bool> const expected = { { 0, true }, { 1, false }, { 2, true } };
	OAK_ASSERT(bad == expected);
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
	OAK_ASSERT_EQ(buf.substr(0, buf.size()), "hello world");

	std::map<size_t, bool> bad = buf.misspellings(0, buf.size());
	OAK_ASSERT_EQ(bad.size(), 0);
}

void test_scopes ()
{
	ng::buffer_t buf;
	buf.set_grammar(TestGrammarItem);
	buf.insert(0, "foobar");
	buf.bump_revision();
	buf.wait_for_repair();

	OAK_ASSERT_EQ(to_s(buf), "«test»«foo»foo«/foo»«bar»bar«/bar»«/test»");

	OAK_ASSERT_EQ(to_s(buf.scope( 0).left),  "test");
	OAK_ASSERT_EQ(to_s(buf.scope( 0).right), "test foo");
	OAK_ASSERT_EQ(to_s(buf.scope( 1).left),  "test foo");
	OAK_ASSERT_EQ(to_s(buf.scope( 1).right), "test foo");
	OAK_ASSERT_EQ(to_s(buf.scope( 3).left),  "test foo");
	OAK_ASSERT_EQ(to_s(buf.scope( 3).right), "test bar");
	OAK_ASSERT_EQ(to_s(buf.scope( 4).left),  "test bar");
	OAK_ASSERT_EQ(to_s(buf.scope( 4).right), "test bar");
	OAK_ASSERT_EQ(to_s(buf.scope( 6).left),  "test bar");
	// OAK_ASSERT_EQ(to_s(buf.scope( 6).right), "test");
}

void test_sanitize_index ()
{
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index( 0),  0);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index( 1),  0);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index( 2),  0);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index( 3),  0);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index( 4),  0);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index( 5),  5);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index( 6),  5);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index( 7),  5);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index( 8),  5);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index( 9),  9);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").sanitize_index(10),  9);
	OAK_ASSERT_EQ(ng::buffer_t("c̄̌𠻵").size(), 9);
}

void test_thai_script ()
{
	ng::buffer_t buf;
	buf.insert(0, "สวัสดี");

	size_t i = 0;
	OAK_ASSERT_EQ("ส", buf[i]);
	i += buf[i].size();
	OAK_ASSERT_EQ("วั", buf[i]);
	i += buf[i].size();
	OAK_ASSERT_EQ("ส", buf[i]);
	i += buf[i].size();
	OAK_ASSERT_EQ("ดี", buf[i]);
	i += buf[i].size();
	OAK_ASSERT_EQ(i, buf.size());
}
