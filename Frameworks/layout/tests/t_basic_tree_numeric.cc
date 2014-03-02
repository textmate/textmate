#include <oak/basic_tree.h>
#include <oak/oak.h>

static int numeric_comp (ssize_t key, ssize_t const& offset, ssize_t const& node) { return key < node ? -1 : (key == node ? 0 : +1); }
// static std::string numeric_to_s (ssize_t const& offset, ssize_t const& node)      { return std::to_string(node); }
static bool numeric_bin_comp (oak::basic_tree_t<ssize_t>::value_type const& node, ssize_t key) { return node.key == key; }
static bool numeric_bin_comp_2 (oak::basic_tree_t<ssize_t>::value_type const& lhs, oak::basic_tree_t<ssize_t>::value_type const& rhs) { return lhs.key == rhs.key; }

static size_t const kTreeSize = 4000;

std::vector<ssize_t> create_keys ()
{
	std::set<ssize_t> tmp;
	while(tmp.size() < kTreeSize)
		tmp.insert((random() & 0xFFFFFF) - 0x7FFFFF);

	std::vector<ssize_t> res(tmp.begin(), tmp.end());
	std::random_shuffle(res.begin(), res.end());
	return res;
}

oak::basic_tree_t<ssize_t> create_tree (std::vector<ssize_t> const& keys)
{
	oak::basic_tree_t<ssize_t> tree;
	for(ssize_t key : keys)
		tree.insert(tree.lower_bound(key, &numeric_comp), key);
	return tree;
}

void test_integrity ()
{
	auto keys = create_keys();
	auto tree = create_tree(keys);
	OAK_ASSERT(tree.structural_integrity());
	OAK_ASSERT_EQ(tree.size(), keys.size());
}

void test_iteration ()
{
	auto keys = create_keys();
	auto tree = create_tree(keys);

	OAK_ASSERT_EQ(tree.size(), keys.size());
	std::sort(keys.begin(), keys.end());
	OAK_ASSERT(std::equal(tree.begin(),  tree.end(),  keys.begin(),  &numeric_bin_comp));
	OAK_ASSERT(std::equal(tree.rbegin(), tree.rend(), keys.rbegin(), &numeric_bin_comp));
}

void test_copy ()
{
	auto keys = create_keys();
	auto tree = create_tree(keys);

	oak::basic_tree_t<ssize_t> tmp(tree);
	tree.clear();

	OAK_ASSERT(tmp.structural_integrity());
	OAK_ASSERT_EQ(tmp.size(), keys.size());

	std::sort(keys.begin(), keys.end());
	OAK_ASSERT(std::equal(tmp.begin(), tmp.end(), keys.begin(), &numeric_bin_comp));

	std::swap(tmp, tree);
	OAK_ASSERT(tmp.empty());
	OAK_ASSERT(std::equal(tree.begin(), tree.end(), keys.begin(), &numeric_bin_comp));

	tmp = tree;
	OAK_ASSERT_EQ(tmp.size(), keys.size());
	OAK_ASSERT(std::equal(tmp.begin(), tmp.end(), tree.begin(), &numeric_bin_comp_2));
}

void test_erase ()
{
	auto keys = create_keys();
	auto tree = create_tree(keys);

	std::set<ssize_t> tmp(keys.begin(), keys.end());
	std::random_shuffle(keys.begin(), keys.end());

	for(size_t i = keys.size() >> 1; i < keys.size(); ++i)
	{
		OAK_ASSERT(tree.find(keys[i], &numeric_comp) != tree.end());
		tree.erase(tree.find(keys[i], &numeric_comp));
		tmp.erase(keys[i]);

		OAK_ASSERT_EQ(tree.size(), tmp.size());
		OAK_ASSERT(tree.structural_integrity());
		OAK_ASSERT(std::equal(tree.begin(), tree.end(), tmp.begin(), &numeric_bin_comp));
	}

	keys.resize(keys.size() >> 1);
	OAK_ASSERT_EQ(tree.size(), keys.size());

	std::sort(keys.begin(), keys.end());
	OAK_ASSERT(std::equal(tree.begin(), tree.end(), keys.begin(), &numeric_bin_comp));

	std::random_shuffle(keys.begin(), keys.end());
	for(auto const& key : keys)
	{
		OAK_ASSERT(tree.find(key, &numeric_comp) != tree.end());
		tree.erase(tree.find(key, &numeric_comp));
		OAK_ASSERT(tree.structural_integrity());
	}

	OAK_ASSERT(tree.empty());
}

void test_search ()
{
	auto keys = create_keys();
	auto tree = create_tree(keys);

	std::set<ssize_t> existingKeys(keys.begin(), keys.end()), nonExistingKeys;
	while(nonExistingKeys.size() < kTreeSize)
	{
		ssize_t key = (random() & 0xFFFFFF) - 0x7FFFFF;
		if(existingKeys.find(key) == existingKeys.end())
			nonExistingKeys.insert(key);
	}

	for(auto const& key : existingKeys)
	{
		OAK_ASSERT(tree.find(key, &numeric_comp) != tree.end());
		OAK_ASSERT_EQ(tree.find(key, &numeric_comp)->key, key);

		OAK_ASSERT(tree.lower_bound(key, &numeric_comp) != tree.end());
		OAK_ASSERT_EQ(tree.lower_bound(key, &numeric_comp)->key, key);

		OAK_ASSERT(tree.upper_bound(key, &numeric_comp) == ++tree.find(key, &numeric_comp));
		if(++tree.find(key, &numeric_comp) != tree.end())
		{
			OAK_ASSERT_LT(key, tree.upper_bound(key, &numeric_comp)->key);
		}
	}

	// ========
	// = find =
	// ========

	for(auto const& key : nonExistingKeys)
	{
		OAK_ASSERT(tree.find(key, &numeric_comp) == tree.end());
	}

	// ===============
	// = lower bound =
	// ===============

	for(auto const& key : nonExistingKeys)
	{
		if(existingKeys.lower_bound(key) == existingKeys.end())
		{
			OAK_ASSERT(tree.lower_bound(key, &numeric_comp) == tree.end());
		}
		else
		{
			OAK_ASSERT_LE(key, tree.lower_bound(key, &numeric_comp)->key);
			OAK_ASSERT_EQ(tree.lower_bound(key, &numeric_comp)->key, *existingKeys.lower_bound(key));
		}
	}

	// ===============
	// = upper bound =
	// ===============

	for(auto const& key : nonExistingKeys)
	{
		if(existingKeys.upper_bound(key) == existingKeys.end())
		{
			OAK_ASSERT(tree.upper_bound(key, &numeric_comp) == tree.end());
		}
		else
		{
			OAK_ASSERT_LT(key, tree.upper_bound(key, &numeric_comp)->key);
			OAK_ASSERT_EQ(tree.upper_bound(key, &numeric_comp)->key, *existingKeys.upper_bound(key));
		}
	}
}

void test_duplicates ()
{
	oak::basic_tree_t<ssize_t> tree;

	ssize_t keys[] = { 1, 2, 3, 3, 3, 4, 4, 5, 6 };
	for(auto key : keys)
		tree.insert(tree.end(), key);

	OAK_ASSERT(std::equal(tree.begin(), tree.end(), &keys[0], &numeric_bin_comp));

	OAK_ASSERT(tree.lower_bound(0, &numeric_comp) == tree.begin());
	OAK_ASSERT(tree.lower_bound(1, &numeric_comp) == tree.begin());
	OAK_ASSERT(tree.lower_bound(2, &numeric_comp) != tree.begin());
	OAK_ASSERT(tree.upper_bound(5, &numeric_comp) != tree.end());
	OAK_ASSERT(tree.upper_bound(6, &numeric_comp) == tree.end());
	OAK_ASSERT(tree.upper_bound(7, &numeric_comp) == tree.end());

	OAK_ASSERT_EQ(std::distance(tree.begin(), tree.lower_bound(3, &numeric_comp)), 2);
	OAK_ASSERT_EQ(std::distance(tree.begin(), tree.upper_bound(3, &numeric_comp)), 5);

	OAK_ASSERT_EQ(std::distance(tree.begin(), tree.lower_bound(4, &numeric_comp)), 5);
	OAK_ASSERT_EQ(std::distance(tree.begin(), tree.upper_bound(4, &numeric_comp)), 7);

	OAK_ASSERT_EQ(std::distance(tree.begin(), tree.lower_bound(5, &numeric_comp)), 7);
	OAK_ASSERT_EQ(std::distance(tree.begin(), tree.upper_bound(5, &numeric_comp)), 8);

	OAK_ASSERT_EQ(std::distance(tree.begin(), tree.lower_bound(6, &numeric_comp)), 8);
	OAK_ASSERT_EQ(std::distance(tree.begin(), tree.upper_bound(6, &numeric_comp)), 9);

	OAK_ASSERT(tree.structural_integrity());
}
