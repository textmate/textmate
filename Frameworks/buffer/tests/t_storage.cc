#include <buffer/src/storage.h>
#include <oak/basic_tree.h>
#include <io/src/path.h>
#include <text/src/format.h>
#include <regexp/src/format_string.h>

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
		tree.insert(tree.end(), std::min<size_t>(arc4random_uniform(15) + 15, len - tree.aggregated().length), tree.aggregated().length);

	std::vector<fragment_t> res;
	while(!tree.empty())
	{
		size_t i = arc4random_uniform(tree.size());
		auto it = tree.find(i, &comp_nth);
		res.push_back((fragment_t){ it->offset.length, it->value, it->key.length });
		tree.erase(it);
	}

	return res;
}

static std::vector<fragment_t> reverse (std::vector<fragment_t> ranges)
{
	std::reverse(ranges.begin(), ranges.end());
	return ranges;
}

static std::string create_buffer (size_t size = 50 * 1024)
{
	std::string buffer(size, '\0');
	for(size_t i = 0; i < buffer.size(); ++i)
		buffer[i] = 0x20 + (i % 0x60);
	oak::random_shuffle(buffer.begin(), buffer.end());
	return buffer;
}

void benchmark_cfstring_random_insert_1_mb ()
{
	std::string const buffer = create_buffer(1 * 1024*1024);
	if(CFMutableStringRef storage = CFStringCreateMutable(kCFAllocatorDefault, 0))
	{
		for(auto range : reverse(random_ranges(buffer.size())))
		{
			if(CFStringRef substr = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8 const*)buffer.data() + range.src, range.len, kCFStringEncodingASCII, false))
			{
				CFStringInsert(storage, range.dst, substr);
				CFRelease(substr);
			}
		}

		std::string c(CFStringGetLength(storage) + 1, '\0');
		OAK_ASSERT(CFStringGetCString(storage, &c[0], c.size(), kCFStringEncodingASCII));
		OAK_ASSERT_EQ(buffer, c.substr(0, c.size()-1));

		CFRelease(storage);
	}
}

void benchmark_cxxstring_random_insert_1_mb ()
{
	std::string const buffer = create_buffer(1 * 1024*1024);
	std::string storage;

	for(auto range : reverse(random_ranges(buffer.size())))
		storage.insert(range.dst, buffer.substr(range.src, range.len));

	OAK_ASSERT_EQ(buffer, storage);
}

void benchmark_ng_storage_random_insert_5_mb_overhead ()
{
	std::string const buffer = create_buffer(5 * 1024*1024);
	reverse(random_ranges(buffer.size()));
}

void benchmark_ng_storage_random_insert_5_mb ()
{
	std::string const buffer = create_buffer(5 * 1024*1024);
	ng::detail::storage_t storage;

	for(auto range : reverse(random_ranges(buffer.size())))
		storage.insert(range.dst, buffer.data() + range.src, range.len);

	OAK_ASSERT_EQ(storage.substr(0, storage.size()), buffer);
}

void benchmark_erase_and_insert_in_5_mb ()
{
	std::string const buffer = create_buffer(5 * 1024*1024);
	ng::detail::storage_t storage;
	storage.insert(0, buffer.data(), buffer.size());

	size_t total = 0;
	for(size_t i = 0; i < storage.size(); ++i)
	{
		if(storage[i] == '=')
		{
			storage.erase(i, i + 1);
			storage.insert(i, " ", 1);
			++total;
		}
	}
}

void test_bracket_operator ()
{
	std::string const buffer = create_buffer();
	ng::detail::storage_t storage;

	for(auto range : reverse(random_ranges(buffer.size())))
		storage.insert(range.dst, buffer.data() + range.src, range.len);

	for(size_t i = 0; i < storage.size(); ++i)
		OAK_ASSERT_EQ(storage[i], buffer[i]);
}

void test_equality_operator ()
{
	std::string const buffer = "All composite phenomena are impermanent - All contaminated things and events are unsatisfactory - All phenomena are empty and selfless - Nirvana is true peace.";
	ng::detail::storage_t lhs, rhs;

	for(auto range : reverse(random_ranges(buffer.size())))
		lhs.insert(range.dst, buffer.data() + range.src, range.len);
	for(auto range : reverse(random_ranges(buffer.size())))
		rhs.insert(range.dst, buffer.data() + range.src, range.len);

	OAK_ASSERT_EQ(lhs, rhs);
	OAK_ASSERT_EQ(rhs, lhs);

	lhs.erase(lhs.size()-1, lhs.size());
	lhs.insert(lhs.size(), "!", 1);

	OAK_ASSERT_NE(lhs, rhs);
	OAK_ASSERT_NE(rhs, lhs);
}

void test_random_insert ()
{
	std::string const buffer = create_buffer();
	ng::detail::storage_t storage;

	for(auto range : reverse(random_ranges(buffer.size())))
		storage.insert(range.dst, buffer.data() + range.src, range.len);

	OAK_ASSERT_EQ(storage.substr(0, storage.size()), buffer);
}

void test_random_erase ()
{
	std::string const buffer = create_buffer();
	ng::detail::storage_t storage;
	storage.insert(0, buffer.data(), buffer.size());

	for(auto const& range : random_ranges(storage.size()))
		storage.erase(range.dst, range.dst + range.len);

	OAK_ASSERT_EQ(storage.size(), 0);
}

void test_buffer_chunk_iterator ()
{
	std::string const buffer = create_buffer();
	ng::detail::storage_t storage;

	for(auto range : reverse(random_ranges(buffer.size())))
		storage.insert(range.dst, buffer.data() + range.src, range.len);

	std::string str;
	for(auto node : storage)
		str.append(node.data(), node.size());

	OAK_ASSERT_EQ(str, buffer);
}

void test_random_substr_access ()
{
	std::string const buffer = create_buffer();
	ng::detail::storage_t storage;
	storage.insert(0, buffer.data(), buffer.size());

	for(auto range : random_ranges(storage.size()))
		OAK_ASSERT_EQ(storage.substr(range.src, range.src + range.len), buffer.substr(range.src, range.len));
}
