#include <oak/basic_tree.h>
#include <oak/oak.h>

struct key_t
{
	key_t (ssize_t pos = 0) : position(pos)   { }
	key_t operator== (key_t const& rhs) const { return position == rhs.position; }
	key_t operator+ (key_t const& rhs) const { return key_t(position + rhs.position); }
	key_t operator- (key_t const& rhs) const { return key_t(position - rhs.position); }

	ssize_t position;
};

static int position_comp (ssize_t key, key_t const& offset, key_t const& node) { return key < offset.position + node.position ? -1 : (key == offset.position + node.position ? 0 : +1); }
typedef oak::basic_tree_t<key_t> tree_t;

static void set (tree_t& tree, ssize_t pos)
{
	auto it = tree.upper_bound(pos, &position_comp);
	if(it != tree.begin())
	{
		auto tmp = it;
		--tmp;
		pos -= tmp->offset.position + tmp->key.position;
	}

	if(it != tree.end())
	{
		it->key.position -= pos;
		tree.update_key(it);
	}

	tree.insert(it, pos);
}

static void unset (tree_t& tree, ssize_t pos)
{
	auto it = tree.find(pos, &position_comp);
	if(it == tree.end())
		return;

	auto tmp = it;
	if(++tmp != tree.end())
	{
		tmp->key.position += it->key.position;
		tree.update_key(tmp);
	}

	tree.erase(it);
}

void adjust (tree_t& tree, ssize_t pos, size_t distance)
{
	auto it = tree.lower_bound(pos, &position_comp);
	if(it == tree.end())
		return;

	it->key.position += distance;
	tree.update_key(it);
}

void test_adjustment ()
{
	tree_t tree;

	set(tree, 20);
	set(tree, 30);

	adjust(tree, 15, 5); // 20 → 25, 30 → 35
	adjust(tree, 25, 5); // 25 → 30, 35 → 40
	adjust(tree, 35, 5); // 30 → 30, 40 → 45
	adjust(tree, 45, 5); // 30 → 30, 45 → 50
	adjust(tree, 55, 5); // 30 → 30, 50 → 50

	OAK_ASSERT_EQ(tree.size(), 2);
	OAK_ASSERT(tree.find(30, &position_comp) != tree.end());
	OAK_ASSERT(tree.find(50, &position_comp) != tree.end());

	adjust(tree, 60, -5); // 30 → 30, 50 → 50
	adjust(tree, 50, -5); // 30 → 30, 50 → 45
	adjust(tree, 40, -5); // 30 → 30, 45 → 40
	adjust(tree, 30, -5); // 30 → 25, 40 → 35
	adjust(tree, 20, -5); // 25 → 20, 35 → 30

	OAK_ASSERT_EQ(tree.size(), 2);
	OAK_ASSERT(tree.find(20, &position_comp) != tree.end());
	OAK_ASSERT(tree.find(30, &position_comp) != tree.end());
}

void test_range_tree ()
{
	std::set<ssize_t> tmp;
	while(tmp.size() < 32)
		tmp.insert((random() & 0xFFFFFF) - 0x7FFFFF);

	std::vector<ssize_t> keys(tmp.begin(), tmp.end());
	std::random_shuffle(keys.begin(), keys.end());

	tree_t tree;
	for(auto const& key : keys)
		set(tree, key);

	std::vector<ssize_t> fromTree;
	for(auto const& it : tree)
		fromTree.push_back(it.offset.position + it.key.position);

	std::sort(keys.begin(), keys.end());
	OAK_ASSERT_EQ(fromTree, keys);

	// ==================
	// = Test adjusting =
	// ==================

	std::random_shuffle(keys.begin(), keys.end());
	ssize_t pos = keys[0], distance = 200;
	adjust(tree, pos, distance);
	for(auto& key : keys)
	{
		if(pos <= key)
			key += distance;
	}

	for(auto const& key : keys)
	{
		OAK_ASSERT(tree.find(key, &position_comp) != tree.end());
	}

	// =======================
	// = Erase half the keys =
	// =======================

	std::random_shuffle(keys.begin(), keys.end());
	for(size_t i = keys.size() >> 1; i < keys.size(); ++i)
	{
		auto it = tree.find(keys[i], &position_comp);
		OAK_ASSERT(it != tree.end());
		unset(tree, it->offset.position + it->key.position);
		tmp.erase(keys[i]);
	}

	keys.resize(keys.size() >> 1);
	OAK_ASSERT_EQ(tree.size(), keys.size());

	std::random_shuffle(keys.begin(), keys.end());
	for(auto const& key : keys)
		unset(tree, key);
	OAK_ASSERT(tree.empty());
}
