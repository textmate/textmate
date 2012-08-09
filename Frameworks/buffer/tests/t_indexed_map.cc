#include <buffer/indexed_map.h>
#include <oak/oak.h>

static ssize_t const TestKeys[5][3] =
{
	{ 10, 20, 30 },
	{ 20, 30, 40 },
	{ 30, 40, 50 },
	{ 40, 50, 60 },
	{ 50, 60, 70 }
};

template <typename T>
void seed_map (T& map)
{
	for(size_t i = 1; i < 6; ++i)
		map.set(i, i);
}

template <typename T>
std::vector< std::pair<ssize_t, size_t> > values (T const& map)
{
	return std::vector< std::pair<ssize_t, size_t> >(map.begin(), map.end());
}

struct value_pair
{
	size_t first; ssize_t second;
};

template <int N>
std::vector< std::pair<ssize_t, size_t> > expected (value_pair(&array)[N])
{
	std::vector< std::pair<ssize_t, size_t> > res;
	iterate(pair, array)
		res.push_back(std::make_pair(pair->first, pair->second));
	return res;
}

class IndexedMapTests : public CxxTest::TestSuite
{
public:
	void test_basic ()
	{
		indexed_map_t<bool> map;

		std::set<ssize_t> tmp;
		while(tmp.size() < 2000)
			tmp.insert((random() & 0xFFFFFF) - 0x7FFFFF);

		std::vector<ssize_t> keys(tmp.begin(), tmp.end());
		std::random_shuffle(beginof(keys), endof(keys));
		iterate(key, keys)
			map.set(*key, true);

		std::vector<ssize_t> sorted;
		std::transform(map.begin(), map.end(), back_inserter(sorted), [](std::pair<ssize_t, bool> const& p){ return p.first; });
		TS_ASSERT_EQUALS(tmp.size(), sorted.size());
		TS_ASSERT(std::equal(tmp.begin(), tmp.end(), sorted.begin()));
	
		std::random_shuffle(beginof(keys), endof(keys));
		for(size_t i = keys.size() >> 1; i < keys.size(); ++i)
		{
			map.remove(keys[i]);

			tmp.erase(keys[i]);
			sorted.clear();
			iterate(pair, map)
				sorted.push_back(pair->first);
			TS_ASSERT_EQUALS(tmp.size(), sorted.size());
			TS_ASSERT(std::equal(tmp.begin(), tmp.end(), sorted.begin()));
		}
		keys.resize(keys.size() >> 1);
		std::sort(keys.begin(), keys.end());

		sorted.clear();
		iterate(pair, map)
			sorted.push_back(pair->first);
		TS_ASSERT_EQUALS(keys.size(), sorted.size());
		TS_ASSERT(std::equal(keys.begin(), keys.end(), sorted.begin()));

		std::random_shuffle(beginof(sorted), endof(sorted));
		iterate(key, sorted)
			map.remove(*key);
		TS_ASSERT(map.empty());
	}

	static void test_child_count ()
	{
		static ssize_t const TestKeys[] = { 10, 20, 30, 40, 50 };
		std::vector<ssize_t> keys(beginof(TestKeys), endof(TestKeys));
		do {

			std::next_permutation(keys.begin(), keys.end());
			indexed_map_t<bool> map;
			iterate(key, keys)
				map.set(*key, true);

			TS_ASSERT_EQUALS(map.size(), 5);

			TS_ASSERT_EQUALS(map.nth(0)->first, 10);
			TS_ASSERT_EQUALS(map.nth(1)->first, 20);
			TS_ASSERT_EQUALS(map.nth(2)->first, 30);
			TS_ASSERT_EQUALS(map.nth(3)->first, 40);
			TS_ASSERT_EQUALS(map.nth(4)->first, 50);

			TS_ASSERT_EQUALS(map.nth(0).index(), 0);
			TS_ASSERT_EQUALS(map.nth(1).index(), 1);
			TS_ASSERT_EQUALS(map.nth(2).index(), 2);
			TS_ASSERT_EQUALS(map.nth(3).index(), 3);
			TS_ASSERT_EQUALS(map.nth(4).index(), 4);

			TS_ASSERT_EQUALS(map.find(10).index(), 0);
			TS_ASSERT_EQUALS(map.find(20).index(), 1);
			TS_ASSERT_EQUALS(map.find(30).index(), 2);
			TS_ASSERT_EQUALS(map.find(40).index(), 3);
			TS_ASSERT_EQUALS(map.find(50).index(), 4);

			map.remove(30);

			TS_ASSERT_EQUALS(map.size(), 4);

			TS_ASSERT_EQUALS(map.nth(0)->first, 10);
			TS_ASSERT_EQUALS(map.nth(1)->first, 20);
			TS_ASSERT_EQUALS(map.nth(2)->first, 40);
			TS_ASSERT_EQUALS(map.nth(3)->first, 50);

			TS_ASSERT_EQUALS(map.nth(0).index(), 0);
			TS_ASSERT_EQUALS(map.nth(1).index(), 1);
			TS_ASSERT_EQUALS(map.nth(2).index(), 2);
			TS_ASSERT_EQUALS(map.nth(3).index(), 3);

			TS_ASSERT_EQUALS(map.find(10).index(), 0);
			TS_ASSERT_EQUALS(map.find(20).index(), 1);
			TS_ASSERT_EQUALS(map.find(40).index(), 2);
			TS_ASSERT_EQUALS(map.find(50).index(), 3);

		} while(!std::equal(keys.begin(), keys.end(), beginof(TestKeys)));
	}

	static void test_find ()
	{
		static ssize_t const TestKeys[] = { 10, 20, 30, 40, 50 };
		std::vector<ssize_t> keys(beginof(TestKeys), endof(TestKeys));
		do {

			std::next_permutation(keys.begin(), keys.end());
			indexed_map_t<bool> map;
			iterate(key, TestKeys)
				map.set(*key, true);

			TS_ASSERT_EQUALS(map.find(10-1).index(), 5);
			TS_ASSERT_EQUALS(map.find(20-1).index(), 5);
			TS_ASSERT_EQUALS(map.find(30-1).index(), 5);
			TS_ASSERT_EQUALS(map.find(40-1).index(), 5);
			TS_ASSERT_EQUALS(map.find(50-1).index(), 5);

			TS_ASSERT_EQUALS(map.find(10).index(), 0);
			TS_ASSERT_EQUALS(map.find(20).index(), 1);
			TS_ASSERT_EQUALS(map.find(30).index(), 2);
			TS_ASSERT_EQUALS(map.find(40).index(), 3);
			TS_ASSERT_EQUALS(map.find(50).index(), 4);

			TS_ASSERT_EQUALS(map.find(10+1).index(), 5);
			TS_ASSERT_EQUALS(map.find(20+1).index(), 5);
			TS_ASSERT_EQUALS(map.find(30+1).index(), 5);
			TS_ASSERT_EQUALS(map.find(40+1).index(), 5);
			TS_ASSERT_EQUALS(map.find(50+1).index(), 5);

		} while(!std::equal(keys.begin(), keys.end(), beginof(TestKeys)));
	}

	static void test_lower_bound ()
	{
		static ssize_t const TestKeys[] = { 10, 20, 30, 40, 50 };
		std::vector<ssize_t> keys(beginof(TestKeys), endof(TestKeys));
		do {

			std::next_permutation(keys.begin(), keys.end());
			indexed_map_t<bool> map;
			iterate(key, TestKeys)
				map.set(*key, true);

			TS_ASSERT_EQUALS(map.lower_bound(10-1).index(), 0);
			TS_ASSERT_EQUALS(map.lower_bound(20-1).index(), 1);
			TS_ASSERT_EQUALS(map.lower_bound(30-1).index(), 2);
			TS_ASSERT_EQUALS(map.lower_bound(40-1).index(), 3);
			TS_ASSERT_EQUALS(map.lower_bound(50-1).index(), 4);

			TS_ASSERT_EQUALS(map.lower_bound(10).index(), 0);
			TS_ASSERT_EQUALS(map.lower_bound(20).index(), 1);
			TS_ASSERT_EQUALS(map.lower_bound(30).index(), 2);
			TS_ASSERT_EQUALS(map.lower_bound(40).index(), 3);
			TS_ASSERT_EQUALS(map.lower_bound(50).index(), 4);

			TS_ASSERT_EQUALS(map.lower_bound(10+1).index(), 1);
			TS_ASSERT_EQUALS(map.lower_bound(20+1).index(), 2);
			TS_ASSERT_EQUALS(map.lower_bound(30+1).index(), 3);
			TS_ASSERT_EQUALS(map.lower_bound(40+1).index(), 4);
			TS_ASSERT_EQUALS(map.lower_bound(50+1).index(), 5);

		} while(!std::equal(keys.begin(), keys.end(), beginof(TestKeys)));
	}

	static void test_upper_bound ()
	{
		static ssize_t const TestKeys[] = { 10, 20, 30, 40, 50 };
		std::vector<ssize_t> keys(beginof(TestKeys), endof(TestKeys));
		do {

			std::next_permutation(keys.begin(), keys.end());
			indexed_map_t<bool> map;
			iterate(key, TestKeys)
				map.set(*key, true);

			TS_ASSERT_EQUALS(map.upper_bound(10-1).index(), 0);
			TS_ASSERT_EQUALS(map.upper_bound(20-1).index(), 1);
			TS_ASSERT_EQUALS(map.upper_bound(30-1).index(), 2);
			TS_ASSERT_EQUALS(map.upper_bound(40-1).index(), 3);
			TS_ASSERT_EQUALS(map.upper_bound(50-1).index(), 4);

			TS_ASSERT_EQUALS(map.upper_bound(10).index(), 1);
			TS_ASSERT_EQUALS(map.upper_bound(20).index(), 2);
			TS_ASSERT_EQUALS(map.upper_bound(30).index(), 3);
			TS_ASSERT_EQUALS(map.upper_bound(40).index(), 4);
			TS_ASSERT_EQUALS(map.upper_bound(50).index(), 5);

			TS_ASSERT_EQUALS(map.upper_bound(10+1).index(), 1);
			TS_ASSERT_EQUALS(map.upper_bound(20+1).index(), 2);
			TS_ASSERT_EQUALS(map.upper_bound(30+1).index(), 3);
			TS_ASSERT_EQUALS(map.upper_bound(40+1).index(), 4);
			TS_ASSERT_EQUALS(map.upper_bound(50+1).index(), 5);

		} while(!std::equal(keys.begin(), keys.end(), beginof(TestKeys)));
	}

	void test_preserve ()
	{
		indexed_map_t<size_t> nonPreserveLeft;
		seed_map(nonPreserveLeft);
		nonPreserveLeft.replace(2, 4, 2, false /* bindRight */);
		value_pair nonPreserveLeftPost[] = { { 1, 1 }, { 2, 2 }, { 5, 5 } };
		TS_ASSERT_EQUALS(values(nonPreserveLeft), expected(nonPreserveLeftPost));

		indexed_map_t<size_t> nonPreserveRight;
		seed_map(nonPreserveRight);
		nonPreserveRight.replace(2, 4, 2, true  /* bindRight */);
		value_pair nonPreserveRightPost[] = { { 1, 1 }, { 4, 4 }, { 5, 5 } };
		TS_ASSERT_EQUALS(values(nonPreserveRight), expected(nonPreserveRightPost));
	}

	void test_bind_right ()
	{
		iterate(row, TestKeys)
		{
			std::vector<ssize_t> keys(beginof(*row), endof(*row));
			do {

				indexed_map_t<bool> map;

				ssize_t pivot = 30, pad = 5;
				std::vector<ssize_t> expected, actual;
				iterate(key, keys)
				{
					map.set(*key, true);
					expected.push_back(*key < pivot ? *key : *key + pad);
				}

				map.replace(pivot, pivot, pad, true /* bindRight */);

				std::sort(expected.begin(), expected.end());
				std::transform(map.begin(), map.end(), back_inserter(actual), [](std::pair<ssize_t, bool> const& p){ return p.first; });
				TS_ASSERT_EQUALS(actual, expected);

				std::next_permutation(keys.begin(), keys.end());

			} while(!std::equal(keys.begin(), keys.end(), beginof(*row)));
		}
	}

	void test_bind_left ()
	{
		iterate(row, TestKeys)
		{
			std::vector<ssize_t> keys(beginof(*row), endof(*row));
			do {

				indexed_map_t<bool> map;

				ssize_t pivot = 30, pad = 5;
				std::vector<ssize_t> expected, actual;
				iterate(key, keys)
				{
					map.set(*key, true);
					expected.push_back(*key <= pivot ? *key : *key + pad);
				}

				map.replace(pivot, pivot, pad, false /* bindRight */);

				std::sort(expected.begin(), expected.end());
				std::transform(map.begin(), map.end(), back_inserter(actual), [](std::pair<ssize_t, bool> const& p){ return p.first; });
				TS_ASSERT_EQUALS(actual, expected);

				std::next_permutation(keys.begin(), keys.end());

			} while(!std::equal(keys.begin(), keys.end(), beginof(*row)));
		}
	}

	void test_duplicate ()
	{
		indexed_map_t<bool> map;

		ssize_t random[] = { 2, 7, 13, 15, 29 };
		std::random_shuffle(random, random + sizeofA(random));
		for(auto i : random)
			map.set(i, true);

		std::vector<bool> values;
		std::transform(map.begin(), map.end(), back_inserter(values), [](std::pair<ssize_t, bool> const& p){ return p.second; });
		TS_ASSERT_EQUALS(values.size(), sizeofA(random));
		TS_ASSERT(std::find(values.begin(), values.end(), false) == values.end());

		std::random_shuffle(random, random + sizeofA(random));
		for(auto i : random)
			map.set(i, false);

		values.clear();
		std::transform(map.begin(), map.end(), back_inserter(values), [](std::pair<ssize_t, bool> const& p){ return p.second; });
		TS_ASSERT_EQUALS(values.size(), sizeofA(random));
		TS_ASSERT(std::find(values.begin(), values.end(), true) == values.end());
	}

	void test_bind_left_right ()
	{
		indexed_map_t<bool> map;

		ssize_t random[] = { 2, 7, 13, 15, 29 };
		std::random_shuffle(random, random + sizeofA(random));
		for(auto i : random)
			map.set(i, true);

		for(size_t from = 0; from < 30; ++from)
		{
			for(size_t to = from; to < 30; ++to)
			{
				for(size_t len = 0; len < 10; ++len)
				{
					indexed_map_t<bool> left = map, right = map;
					left.replace(from, to, len, false /* bindRight */);
					right.replace(from, to, len, true /* bindRight */);

					std::vector<ssize_t> sortedLeft, sortedRight;
					std::transform(left.begin(), left.end(), back_inserter(sortedLeft), [](std::pair<ssize_t, bool> const& p){ return p.first; });
					std::transform(right.begin(), right.end(), back_inserter(sortedRight), [](std::pair<ssize_t, bool> const& p){ return p.first; });

					std::set<ssize_t> keysLeft, keysRight;
					for(auto i : random)
					{
						if(!(from < i && i <= to))
							keysLeft.insert(i > to ? i - (to - from) + len : i);

						if(!(from <= i && i < to))
							keysRight.insert(i >= to ? i - (to - from) + len : i);
					}

					TS_ASSERT_EQUALS(sortedLeft.size(), keysLeft.size());
					TS_ASSERT_EQUALS(sortedRight.size(), keysRight.size());
					TS_ASSERT(std::equal(sortedLeft.begin(), sortedLeft.end(), keysLeft.begin()));
					TS_ASSERT(std::equal(sortedRight.begin(), sortedRight.end(), keysRight.begin()));
				}
			}
		}
	}
};
