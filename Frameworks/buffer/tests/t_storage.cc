#include <buffer/storage.h>
#include <oak/basic_tree.h>
#include <io/path.h>
#include <text/format.h>
#include <regexp/format_string.h>

// #define ENABLE_BM 1

#ifdef ENABLE_BM
#define BM_OUT(...) fprintf(stderr, __VA_ARGS__)
#else
#define BM_OUT(...)
#endif

class StorageTests : public CxxTest::TestSuite
{
	static std::string format_number (size_t i)
	{
		return format_string::replace(text::format("%zu", i), "\\d{1,3}(?=\\d{3}+(?!\\d))", "$0,");
	}

	struct key_t
	{
		key_t () : length(0), children(0) { }
		key_t (size_t len, size_t children = 1) : length(len), children(children) { }

		key_t operator+ (key_t const& rhs) { return key_t(length + rhs.length, children + rhs.children); }
		key_t operator- (key_t const& rhs) { return key_t(length - rhs.length, children - rhs.children); }

		size_t length;
		size_t children;
	};

	static int comp_nth (ssize_t key, key_t const& offset, key_t const& node) { return key < offset.children ? -1 : (key == offset.children ? 0 : +1); }

	struct fragment_t { size_t dst, src, len; };

	static std::vector<fragment_t> random_ranges (size_t len)
	{
		oak::basic_tree_t<key_t, size_t> tree;
		while(tree.aggregated().length < len)
			tree.insert(tree.end(), std::min<size_t>((random() % 15) + 15, len - tree.aggregated().length), tree.aggregated().length);

		std::vector<fragment_t> res;
		while(!tree.empty())
		{
			size_t i = random() % tree.size();
			auto it = tree.find(i, &comp_nth);
			res.push_back((fragment_t){ it->offset.length, it->value, it->key.length });
			tree.erase(it);
		}

		return res;
	}

	std::string buffer;

public:
	StorageTests ()
	{
#ifdef ENABLE_BM
		buffer = std::string(0x7F - 0x20, '\0');
		ext::iota(buffer.begin(), buffer.end(), 0x20);
		while(buffer.size() < 32 * 1024*1024)
			buffer.insert(buffer.end(), buffer.begin(), buffer.end());
		std::random_shuffle(buffer.begin(), buffer.end());
#else
		buffer = path::content(__FILE__);
#endif
	}

	void DiSABLED_test_cfstring_insert ()
	{
		CFMutableStringRef storage = CFStringCreateMutable(kCFAllocatorDefault, 0);
		std::vector<fragment_t> insertRanges = random_ranges(buffer.size());

		oak::duration_t timer;
		riterate(range, insertRanges)
		{
			CFStringRef substr = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8 const*)buffer.data() + range->src, range->len, kCFStringEncodingASCII, false);
			CFStringInsert(storage, range->dst, substr);
			CFRelease(substr);
		}
		BM_OUT("%.1f seconds to insert %s substrings in CFString (%s)\n", timer.duration(), format_number(insertRanges.size()).c_str(), text::format_size(CFStringGetLength(storage)).c_str());

		std::string c(CFStringGetLength(storage) + 1, '\0');
		TS_ASSERT(CFStringGetCString(storage, &c[0], c.size(), kCFStringEncodingASCII));
		TS_ASSERT_EQUALS(buffer, c.substr(0, c.size()-1));
	}

	void DiSABLED_test_cxxstring_insert ()
	{
		std::string storage;
		std::vector<fragment_t> insertRanges = random_ranges(buffer.size());

		oak::duration_t timer;
		riterate(range, insertRanges)
			storage.insert(range->dst, buffer.substr(range->src, range->len));
		BM_OUT("%.1f seconds to insert %s substrings in std::string (%s)\n", timer.duration(), format_number(insertRanges.size()).c_str(), text::format_size(storage.size()).c_str());

		TS_ASSERT_EQUALS(buffer, storage);
	}

	void test_bracket_operator ()
	{
		ng::detail::storage_t storage;
		auto v = random_ranges(buffer.size());
		riterate(range, v)
			storage.insert(range->dst, buffer.data() + range->src, range->len);

		oak::duration_t timer;
		for(size_t i = 0; i < storage.size(); ++i)
		{
			TS_ASSERT_EQUALS(storage[i], buffer[i]);
		}

		BM_OUT("%.1f seconds to scan %s\n", timer.duration(), text::format_size(storage.size()).c_str());
	}

	void test_replace ()
	{
		ng::detail::storage_t storage;
		storage.insert(0, buffer.data(), buffer.size());
		size_t total = 0;

		oak::duration_t timer;
		for(size_t i = 0; i < storage.size(); ++i)
		{
			if(storage[i] == '=')
			{
				storage.erase(i, i + 1);
				storage.insert(i, " ", 1);
				++total;
			}
		}

		BM_OUT("%.1f seconds to replace %s substrings (%s)\n", timer.duration(), format_number(total).c_str(), text::format_size(storage.size()).c_str());
	}

	void test_insert ()
	{
		ng::detail::storage_t storage;
		std::vector<fragment_t> insertRanges = random_ranges(buffer.size());

		oak::duration_t timer;
		riterate(range, insertRanges)
			storage.insert(range->dst, buffer.data() + range->src, range->len);
		BM_OUT("%.1f seconds to insert %s substrings (%s)\n", timer.duration(), format_number(insertRanges.size()).c_str(), text::format_size(storage.size()).c_str());

		timer.reset();
		std::string str = storage.substr(0, storage.size());
		BM_OUT("%.1f seconds to create %s large substring\n", timer.duration(), text::format_size(str.size()).c_str());

		TS_ASSERT_EQUALS(str, buffer);
	}

	void test_erase ()
	{
		ng::detail::storage_t storage;
		storage.insert(0, buffer.data(), buffer.size());
		std::vector<fragment_t> eraseRanges = random_ranges(storage.size());

		oak::duration_t timer;
		for(auto const& range : eraseRanges)
			storage.erase(range.dst, range.dst + range.len);
		BM_OUT("%.1f seconds to erase %s substrings\n", timer.duration(), format_number(eraseRanges.size()).c_str());

		TS_ASSERT_EQUALS(storage.size(), 0);
	}

	void test_iteration ()
	{
		ng::detail::storage_t storage;
		std::vector<fragment_t> insertRanges = random_ranges(buffer.size());

		riterate(range, insertRanges)
			storage.insert(range->dst, buffer.data() + range->src, range->len);

		std::string str;
		citerate(node, storage)
			str.append((*node).data(), (*node).size());

		TS_ASSERT_EQUALS(str, buffer);
	}

	void test_access ()
	{
		ng::detail::storage_t storage;
		storage.insert(0, buffer.data(), buffer.size());
		std::vector<fragment_t> accessRanges = random_ranges(storage.size());

		oak::duration_t timer;
		for(auto const& range : accessRanges)
			TS_ASSERT_EQUALS(storage.substr(range.src, range.src + range.len), buffer.substr(range.src, range.len));
		BM_OUT("%.1f seconds to create %s substrings\n", timer.duration(), format_number(accessRanges.size()).c_str());
	}
};
