#include <oak/basic_tree.h>
#include <oak/oak.h>

struct annotation_t
{
	annotation_t (size_t bufferSize = 0, size_t numberOfChildren = 0, size_t width = SIZE_T_MAX) : buffer_size(bufferSize), number_of_children(numberOfChildren), width(width == SIZE_T_MAX ? bufferSize : width) { }
	bool operator== (annotation_t const& rhs) const { return buffer_size == rhs.buffer_size && number_of_children == rhs.number_of_children && width == rhs.width; }

	annotation_t operator+ (annotation_t const& rhs) const
	{
		return annotation_t(buffer_size + rhs.buffer_size, number_of_children + rhs.number_of_children, std::max(width, rhs.width));
	}

	annotation_t operator- (annotation_t const& rhs) const
	{
		return annotation_t(buffer_size - rhs.buffer_size, number_of_children - rhs.number_of_children, std::min(width, rhs.width));
	}

	size_t buffer_size;
	size_t number_of_children;
	size_t width;
};

static int buffer_size_comp (size_t key, annotation_t const& offset, annotation_t const& node)        { return key < offset.buffer_size        ? -1 : (key == offset.buffer_size        ? 0 : +1); }
static int number_of_children_comp (size_t key, annotation_t const& offset, annotation_t const& node) { return key < offset.number_of_children ? -1 : (key == offset.number_of_children ? 0 : +1); }

static void random_insert (oak::basic_tree_t<annotation_t>& tree, std::string const& buf)
{
	std::vector<size_t> lengths;
	for(size_t i = 0; i < buf.size(); i += lengths.back())
	{
		size_t len = std::min<size_t>((random() % 15) + 15, buf.size() - i);
		while(i + len < buf.size() && buf[i + len - 1] != ' ')
			++len;
		lengths.push_back(len);
	}

	std::vector<size_t> ordering(lengths.size());
	std::iota(ordering.begin(), ordering.end(), 0);
	std::random_shuffle(ordering.begin(), ordering.end());

	std::vector<size_t> srcOffsets(lengths.size(), 0);
	std::vector< std::pair<size_t, size_t> > insertRanges;
	for(size_t index : ordering)
	{
		insertRanges.push_back(std::make_pair(srcOffsets[index], lengths[index]));
		for(size_t i = index + 1; i < srcOffsets.size(); ++i)
			srcOffsets[i] += lengths[index];
	}

	for(auto const& range : insertRanges)
		tree.insert(tree.find(range.first, &buffer_size_comp), annotation_t(range.second, 1));
}

void test_basic_tree_delta ()
{
	static std::string const buffer = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.";

	oak::basic_tree_t<annotation_t> tree;
	random_insert(tree, buffer);

	OAK_ASSERT(tree.structural_integrity());
	OAK_ASSERT_EQ(tree.aggregated().number_of_children, tree.size());

	// test primary key
	std::string buf = "";
	iterate(it, tree)
		buf.insert(buf.size(), buffer.substr(it->offset.buffer_size, it->key.buffer_size));
	OAK_ASSERT_EQ(buf, buffer);

	buf = "";
	rforeach(it, tree.begin(), tree.end())
		buf.insert(0, buffer.substr(it->offset.buffer_size, it->key.buffer_size));
	OAK_ASSERT_EQ(buf, buffer);

	// test secondary key
	buf = "";
	for(size_t n = 0; n < tree.aggregated().number_of_children; ++n)
	{
		auto it = tree.find(n, &number_of_children_comp);
		buf.insert(buf.size(), buffer.substr(it->offset.buffer_size, it->key.buffer_size));
	}
	OAK_ASSERT_EQ(buf, buffer);

	buf = "";
	for(size_t n = tree.aggregated().number_of_children; n > 0; )
	{
		auto it = tree.find(--n, &number_of_children_comp);
		buf.insert(0, buffer.substr(it->offset.buffer_size, it->key.buffer_size));
	}
	OAK_ASSERT_EQ(buf, buffer);

	// test aggregation
	size_t maxLen = 0;
	for(auto const& info : tree)
		maxLen = std::max(info.key.width, maxLen);
	OAK_ASSERT_EQ(maxLen, tree.aggregated().width);

	// cleanup
	OAK_ASSERT(tree.structural_integrity());
	tree.clear(); 
	OAK_ASSERT(tree.structural_integrity());
};
