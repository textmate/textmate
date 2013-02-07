#include <oak/basic_tree.h>
#include <oak/oak.h>

class NumericTreeTests : public CxxTest::TestSuite
{
	static int numeric_comp (ssize_t key, ssize_t const& offset, ssize_t const& node) { return key < node ? -1 : (key == node ? 0 : +1); }
	static std::string numeric_to_s (ssize_t const& offset, ssize_t const& node)      { return std::to_string(node); }
	static bool numeric_bin_comp (oak::basic_tree_t<ssize_t>::value_type const& node, ssize_t key) { return node.key == key; }
	static bool numeric_bin_comp_2 (oak::basic_tree_t<ssize_t>::value_type const& lhs, oak::basic_tree_t<ssize_t>::value_type const& rhs) { return lhs.key == rhs.key; }

	oak::basic_tree_t<ssize_t> tree;
	std::vector<ssize_t> keys;

	static size_t const kTreeSize = 4000;

public:
	void setUp ()
	{
		std::set<ssize_t> tmp;
		while(tmp.size() < kTreeSize)
			tmp.insert((random() & 0xFFFFFF) - 0x7FFFFF);

		keys.insert(keys.end(), tmp.begin(), tmp.end());
		std::random_shuffle(keys.begin(), keys.end());
		iterate(key, keys)
			tree.insert(tree.lower_bound(*key, &numeric_comp), *key);
	}

	void tearDown ()
	{
		TS_ASSERT(tree.structural_integrity());
		tree.clear();
		keys.clear();
	}

	void test_integrity ()
	{
		TS_ASSERT(tree.structural_integrity());
		TS_ASSERT_EQUALS(tree.size(), keys.size());
	}

	void test_iteration ()
	{
		TS_ASSERT_EQUALS(tree.size(), keys.size());

		std::sort(keys.begin(), keys.end());
		TS_ASSERT(std::equal(tree.begin(),  tree.end(),  keys.begin(),  &numeric_bin_comp));
		TS_ASSERT(std::equal(tree.rbegin(), tree.rend(), keys.rbegin(), &numeric_bin_comp));
	}

	void test_copy ()
	{
		oak::basic_tree_t<ssize_t> tmp(tree);
		tree.clear();

		TS_ASSERT(tmp.structural_integrity());
		TS_ASSERT_EQUALS(tmp.size(), keys.size());

		std::sort(keys.begin(), keys.end());
		TS_ASSERT(std::equal(tmp.begin(), tmp.end(), keys.begin(), &numeric_bin_comp));

		std::swap(tmp, tree);
		TS_ASSERT(tmp.empty());
		TS_ASSERT(std::equal(tree.begin(), tree.end(), keys.begin(), &numeric_bin_comp));

		tmp = tree;
		TS_ASSERT_EQUALS(tmp.size(), keys.size());
		TS_ASSERT(std::equal(tmp.begin(), tmp.end(), tree.begin(), &numeric_bin_comp_2));
	}

	void test_erase ()
	{
		std::set<ssize_t> tmp(keys.begin(), keys.end());
		std::random_shuffle(keys.begin(), keys.end());

		for(size_t i = keys.size() >> 1; i < keys.size(); ++i)
		{
			TS_ASSERT_DIFFERS(tree.find(keys[i], &numeric_comp), tree.end());
			tree.erase(tree.find(keys[i], &numeric_comp));
			tmp.erase(keys[i]);

			TS_ASSERT_EQUALS(tree.size(), tmp.size());
			TS_ASSERT(tree.structural_integrity());
			TS_ASSERT(std::equal(tree.begin(), tree.end(), tmp.begin(), &numeric_bin_comp));
		}

		keys.resize(keys.size() >> 1);
		TS_ASSERT_EQUALS(tree.size(), keys.size());

		std::sort(keys.begin(), keys.end());
		TS_ASSERT(std::equal(tree.begin(), tree.end(), keys.begin(), &numeric_bin_comp));

		std::random_shuffle(keys.begin(), keys.end());
		iterate(key, keys)
		{
			TS_ASSERT_DIFFERS(tree.find(*key, &numeric_comp), tree.end());
			tree.erase(tree.find(*key, &numeric_comp));
			TS_ASSERT(tree.structural_integrity());
		}

		TS_ASSERT(tree.empty());
	}

	void test_search ()
	{
		std::set<ssize_t> existingKeys(keys.begin(), keys.end()), nonExistingKeys;
		while(nonExistingKeys.size() < kTreeSize)
		{
			ssize_t key = (random() & 0xFFFFFF) - 0x7FFFFF;
			if(existingKeys.find(key) == existingKeys.end())
				nonExistingKeys.insert(key);
		}

		iterate(key, existingKeys)
		{
			TS_ASSERT_DIFFERS(tree.find(*key, &numeric_comp), tree.end());
			TS_ASSERT_EQUALS(tree.find(*key, &numeric_comp)->key, *key);

			TS_ASSERT_DIFFERS(tree.lower_bound(*key, &numeric_comp), tree.end());
			TS_ASSERT_EQUALS(tree.lower_bound(*key, &numeric_comp)->key, *key);

			TS_ASSERT_EQUALS(tree.upper_bound(*key, &numeric_comp), ++tree.find(*key, &numeric_comp));
			if(++tree.find(*key, &numeric_comp) != tree.end())
			{
				TS_ASSERT_LESS_THAN(*key, tree.upper_bound(*key, &numeric_comp)->key);
			}
		}

		// ========
		// = find =
		// ========

		iterate(key, nonExistingKeys)
		{
			TS_ASSERT_EQUALS(tree.find(*key, &numeric_comp), tree.end());
		}

		// ===============
		// = lower bound =
		// ===============

		iterate(key, nonExistingKeys)
		{
			if(existingKeys.lower_bound(*key) == existingKeys.end())
			{
				TS_ASSERT_EQUALS(tree.lower_bound(*key, &numeric_comp), tree.end());
			}
			else
			{
				TS_ASSERT_LESS_THAN_EQUALS(*key, tree.lower_bound(*key, &numeric_comp)->key);
				TS_ASSERT_EQUALS(tree.lower_bound(*key, &numeric_comp)->key, *existingKeys.lower_bound(*key));
			}
		}

		// ===============
		// = upper bound =
		// ===============

		iterate(key, nonExistingKeys)
		{
			if(existingKeys.upper_bound(*key) == existingKeys.end())
			{
				TS_ASSERT_EQUALS(tree.upper_bound(*key, &numeric_comp), tree.end());
			}
			else
			{
				TS_ASSERT_LESS_THAN(*key, tree.upper_bound(*key, &numeric_comp)->key);
				TS_ASSERT_EQUALS(tree.upper_bound(*key, &numeric_comp)->key, *existingKeys.upper_bound(*key));
			}
		}
	}

	void test_duplicates ()
	{
		tree.clear();
		ssize_t keys[] = { 1, 2, 3, 3, 3, 4, 4, 5, 6 };
		for(auto key : keys)
			tree.insert(tree.end(), key);

		TS_ASSERT(std::equal(tree.begin(), tree.end(), &keys[0], &numeric_bin_comp));

		TS_ASSERT_EQUALS(tree.lower_bound(0, &numeric_comp),  tree.begin());
		TS_ASSERT_EQUALS(tree.lower_bound(1, &numeric_comp),  tree.begin());
		TS_ASSERT_DIFFERS(tree.lower_bound(2, &numeric_comp), tree.begin());
		TS_ASSERT_DIFFERS(tree.upper_bound(5, &numeric_comp), tree.end());
		TS_ASSERT_EQUALS(tree.upper_bound(6, &numeric_comp),  tree.end());
		TS_ASSERT_EQUALS(tree.upper_bound(7, &numeric_comp),  tree.end());

		TS_ASSERT_EQUALS(std::distance(tree.begin(), tree.lower_bound(3, &numeric_comp)), 2);
		TS_ASSERT_EQUALS(std::distance(tree.begin(), tree.upper_bound(3, &numeric_comp)), 5);

		TS_ASSERT_EQUALS(std::distance(tree.begin(), tree.lower_bound(4, &numeric_comp)), 5);
		TS_ASSERT_EQUALS(std::distance(tree.begin(), tree.upper_bound(4, &numeric_comp)), 7);

		TS_ASSERT_EQUALS(std::distance(tree.begin(), tree.lower_bound(5, &numeric_comp)), 7);
		TS_ASSERT_EQUALS(std::distance(tree.begin(), tree.upper_bound(5, &numeric_comp)), 8);

		TS_ASSERT_EQUALS(std::distance(tree.begin(), tree.lower_bound(6, &numeric_comp)), 8);
		TS_ASSERT_EQUALS(std::distance(tree.begin(), tree.upper_bound(6, &numeric_comp)), 9);

		TS_ASSERT(tree.structural_integrity());
	}
};
