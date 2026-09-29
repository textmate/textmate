#include <buffer/src/indexed_map.h>
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
	for(auto const& pair : array)
		res.emplace_back(pair.first, pair.second);
	return res;
}

void test_basic ()
{
	indexed_map_t<bool> map;

	std::set<ssize_t> tmp;
	while(tmp.size() < 2000)
		tmp.insert(arc4random_uniform(0xFFFFFF) - 0x7FFFFF);

	std::vector<ssize_t> keys(tmp.begin(), tmp.end());
	oak::random_shuffle(keys.begin(), keys.end());
	for(auto const& key : keys)
		map.set(key, true);

	std::vector<ssize_t> sorted;
	std::transform(map.begin(), map.end(), back_inserter(sorted), [](std::pair<ssize_t, bool> const& p){ return p.first; });
	OAK_ASSERT_EQ(tmp.size(), sorted.size());
	OAK_ASSERT(std::equal(tmp.begin(), tmp.end(), sorted.begin()));

	oak::random_shuffle(keys.begin(), keys.end());
	for(size_t i = keys.size() >> 1; i < keys.size(); ++i)
	{
		map.remove(keys[i]);

		tmp.erase(keys[i]);
		sorted.clear();
		for(auto const& pair : map)
			sorted.push_back(pair.first);
		OAK_ASSERT_EQ(tmp.size(), sorted.size());
		OAK_ASSERT(std::equal(tmp.begin(), tmp.end(), sorted.begin()));
	}
	keys.resize(keys.size() >> 1);
	std::sort(keys.begin(), keys.end());

	sorted.clear();
	for(auto const& pair : map)
		sorted.push_back(pair.first);
	OAK_ASSERT_EQ(keys.size(), sorted.size());
	OAK_ASSERT(std::equal(keys.begin(), keys.end(), sorted.begin()));

	oak::random_shuffle(sorted.begin(), sorted.end());
	for(auto const& key : sorted)
		map.remove(key);
	OAK_ASSERT(map.empty());
}

void test_child_count ()
{
	static std::vector<ssize_t> const TestKeys = { 10, 20, 30, 40, 50 };
	std::vector<ssize_t> keys = TestKeys;
	do {

		std::next_permutation(keys.begin(), keys.end());
		indexed_map_t<bool> map;
		for(auto const& key : keys)
			map.set(key, true);

		OAK_ASSERT_EQ(map.size(), 5);

		OAK_ASSERT_EQ(map.nth(0)->first, 10);
		OAK_ASSERT_EQ(map.nth(1)->first, 20);
		OAK_ASSERT_EQ(map.nth(2)->first, 30);
		OAK_ASSERT_EQ(map.nth(3)->first, 40);
		OAK_ASSERT_EQ(map.nth(4)->first, 50);

		OAK_ASSERT_EQ(map.nth(0).index(), 0);
		OAK_ASSERT_EQ(map.nth(1).index(), 1);
		OAK_ASSERT_EQ(map.nth(2).index(), 2);
		OAK_ASSERT_EQ(map.nth(3).index(), 3);
		OAK_ASSERT_EQ(map.nth(4).index(), 4);

		OAK_ASSERT_EQ(map.find(10).index(), 0);
		OAK_ASSERT_EQ(map.find(20).index(), 1);
		OAK_ASSERT_EQ(map.find(30).index(), 2);
		OAK_ASSERT_EQ(map.find(40).index(), 3);
		OAK_ASSERT_EQ(map.find(50).index(), 4);

		map.remove(30);

		OAK_ASSERT_EQ(map.size(), 4);

		OAK_ASSERT_EQ(map.nth(0)->first, 10);
		OAK_ASSERT_EQ(map.nth(1)->first, 20);
		OAK_ASSERT_EQ(map.nth(2)->first, 40);
		OAK_ASSERT_EQ(map.nth(3)->first, 50);

		OAK_ASSERT_EQ(map.nth(0).index(), 0);
		OAK_ASSERT_EQ(map.nth(1).index(), 1);
		OAK_ASSERT_EQ(map.nth(2).index(), 2);
		OAK_ASSERT_EQ(map.nth(3).index(), 3);

		OAK_ASSERT_EQ(map.find(10).index(), 0);
		OAK_ASSERT_EQ(map.find(20).index(), 1);
		OAK_ASSERT_EQ(map.find(40).index(), 2);
		OAK_ASSERT_EQ(map.find(50).index(), 3);

	} while(keys != TestKeys);
}

void test_find ()
{
	static std::vector<ssize_t> const TestKeys = { 10, 20, 30, 40, 50 };
	std::vector<ssize_t> keys = TestKeys;
	do {

		std::next_permutation(keys.begin(), keys.end());
		indexed_map_t<bool> map;
		for(auto const& key : TestKeys)
			map.set(key, true);

		OAK_ASSERT_EQ(map.find(10-1).index(), 5);
		OAK_ASSERT_EQ(map.find(20-1).index(), 5);
		OAK_ASSERT_EQ(map.find(30-1).index(), 5);
		OAK_ASSERT_EQ(map.find(40-1).index(), 5);
		OAK_ASSERT_EQ(map.find(50-1).index(), 5);

		OAK_ASSERT_EQ(map.find(10).index(), 0);
		OAK_ASSERT_EQ(map.find(20).index(), 1);
		OAK_ASSERT_EQ(map.find(30).index(), 2);
		OAK_ASSERT_EQ(map.find(40).index(), 3);
		OAK_ASSERT_EQ(map.find(50).index(), 4);

		OAK_ASSERT_EQ(map.find(10+1).index(), 5);
		OAK_ASSERT_EQ(map.find(20+1).index(), 5);
		OAK_ASSERT_EQ(map.find(30+1).index(), 5);
		OAK_ASSERT_EQ(map.find(40+1).index(), 5);
		OAK_ASSERT_EQ(map.find(50+1).index(), 5);

	} while(keys != TestKeys);
}

void test_lower_bound ()
{
	static std::vector<ssize_t> const TestKeys = { 10, 20, 30, 40, 50 };
	std::vector<ssize_t> keys = TestKeys;
	do {

		std::next_permutation(keys.begin(), keys.end());
		indexed_map_t<bool> map;
		for(auto const& key : TestKeys)
			map.set(key, true);

		OAK_ASSERT_EQ(map.lower_bound(10-1).index(), 0);
		OAK_ASSERT_EQ(map.lower_bound(20-1).index(), 1);
		OAK_ASSERT_EQ(map.lower_bound(30-1).index(), 2);
		OAK_ASSERT_EQ(map.lower_bound(40-1).index(), 3);
		OAK_ASSERT_EQ(map.lower_bound(50-1).index(), 4);

		OAK_ASSERT_EQ(map.lower_bound(10).index(), 0);
		OAK_ASSERT_EQ(map.lower_bound(20).index(), 1);
		OAK_ASSERT_EQ(map.lower_bound(30).index(), 2);
		OAK_ASSERT_EQ(map.lower_bound(40).index(), 3);
		OAK_ASSERT_EQ(map.lower_bound(50).index(), 4);

		OAK_ASSERT_EQ(map.lower_bound(10+1).index(), 1);
		OAK_ASSERT_EQ(map.lower_bound(20+1).index(), 2);
		OAK_ASSERT_EQ(map.lower_bound(30+1).index(), 3);
		OAK_ASSERT_EQ(map.lower_bound(40+1).index(), 4);
		OAK_ASSERT_EQ(map.lower_bound(50+1).index(), 5);

	} while(keys != TestKeys);
}

void test_upper_bound ()
{
	static std::vector<ssize_t> const TestKeys = { 10, 20, 30, 40, 50 };
	std::vector<ssize_t> keys = TestKeys;
	do {

		std::next_permutation(keys.begin(), keys.end());
		indexed_map_t<bool> map;
		for(auto const& key : TestKeys)
			map.set(key, true);

		OAK_ASSERT_EQ(map.upper_bound(10-1).index(), 0);
		OAK_ASSERT_EQ(map.upper_bound(20-1).index(), 1);
		OAK_ASSERT_EQ(map.upper_bound(30-1).index(), 2);
		OAK_ASSERT_EQ(map.upper_bound(40-1).index(), 3);
		OAK_ASSERT_EQ(map.upper_bound(50-1).index(), 4);

		OAK_ASSERT_EQ(map.upper_bound(10).index(), 1);
		OAK_ASSERT_EQ(map.upper_bound(20).index(), 2);
		OAK_ASSERT_EQ(map.upper_bound(30).index(), 3);
		OAK_ASSERT_EQ(map.upper_bound(40).index(), 4);
		OAK_ASSERT_EQ(map.upper_bound(50).index(), 5);

		OAK_ASSERT_EQ(map.upper_bound(10+1).index(), 1);
		OAK_ASSERT_EQ(map.upper_bound(20+1).index(), 2);
		OAK_ASSERT_EQ(map.upper_bound(30+1).index(), 3);
		OAK_ASSERT_EQ(map.upper_bound(40+1).index(), 4);
		OAK_ASSERT_EQ(map.upper_bound(50+1).index(), 5);

	} while(keys != TestKeys);
}

void test_preserve ()
{
	indexed_map_t<size_t> nonPreserveLeft;
	seed_map(nonPreserveLeft);
	nonPreserveLeft.replace(2, 4, 2, false /* bindRight */);
	value_pair nonPreserveLeftPost[] = { { 1, 1 }, { 2, 2 }, { 5, 5 } };
	OAK_ASSERT_EQ(values(nonPreserveLeft), expected(nonPreserveLeftPost));

	indexed_map_t<size_t> nonPreserveRight;
	seed_map(nonPreserveRight);
	nonPreserveRight.replace(2, 4, 2, true  /* bindRight */);
	value_pair nonPreserveRightPost[] = { { 1, 1 }, { 4, 4 }, { 5, 5 } };
	OAK_ASSERT_EQ(values(nonPreserveRight), expected(nonPreserveRightPost));
}

void test_bind_right ()
{
	for(auto const& row : TestKeys)
	{
		std::vector<ssize_t> keys(std::begin(row), std::end(row));
		do {

			indexed_map_t<bool> map;

			ssize_t pivot = 30, pad = 5;
			std::vector<ssize_t> expected, actual;
			for(auto const& key : keys)
			{
				map.set(key, true);
				expected.push_back(key < pivot ? key : key + pad);
			}

			map.replace(pivot, pivot, pad, true /* bindRight */);

			std::sort(expected.begin(), expected.end());
			std::transform(map.begin(), map.end(), back_inserter(actual), [](std::pair<ssize_t, bool> const& p){ return p.first; });
			OAK_ASSERT_EQ(actual, expected);

			std::next_permutation(keys.begin(), keys.end());

		} while(!std::equal(keys.begin(), keys.end(), std::begin(row)));
	}
}

void test_bind_left ()
{
	for(auto const& row : TestKeys)
	{
		std::vector<ssize_t> keys(std::begin(row), std::end(row));
		do {

			indexed_map_t<bool> map;

			ssize_t pivot = 30, pad = 5;
			std::vector<ssize_t> expected, actual;
			for(auto const& key : keys)
			{
				map.set(key, true);
				expected.push_back(key <= pivot ? key : key + pad);
			}

			map.replace(pivot, pivot, pad, false /* bindRight */);

			std::sort(expected.begin(), expected.end());
			std::transform(map.begin(), map.end(), back_inserter(actual), [](std::pair<ssize_t, bool> const& p){ return p.first; });
			OAK_ASSERT_EQ(actual, expected);

			std::next_permutation(keys.begin(), keys.end());

		} while(!std::equal(keys.begin(), keys.end(), std::begin(row)));
	}
}

void test_duplicate ()
{
	indexed_map_t<bool> map;

	ssize_t random[] = { 2, 7, 13, 15, 29 };
	oak::random_shuffle(std::begin(random), std::end(random));
	for(auto i : random)
		map.set(i, true);

	std::vector<bool> values;
	std::transform(map.begin(), map.end(), back_inserter(values), [](std::pair<ssize_t, bool> const& p){ return p.second; });
	OAK_ASSERT_EQ(values.size(), sizeofA(random));
	OAK_ASSERT(std::find(values.begin(), values.end(), false) == values.end());

	oak::random_shuffle(std::begin(random), std::end(random));
	for(auto i : random)
		map.set(i, false);

	values.clear();
	std::transform(map.begin(), map.end(), back_inserter(values), [](std::pair<ssize_t, bool> const& p){ return p.second; });
	OAK_ASSERT_EQ(values.size(), sizeofA(random));
	OAK_ASSERT(std::find(values.begin(), values.end(), true) == values.end());
}

void test_bind_left_right ()
{
	indexed_map_t<bool> map;

	ssize_t random[] = { 2, 7, 13, 15, 29 };
	oak::random_shuffle(std::begin(random), std::end(random));
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

				OAK_ASSERT_EQ(sortedLeft.size(), keysLeft.size());
				OAK_ASSERT_EQ(sortedRight.size(), keysRight.size());
				OAK_ASSERT(std::equal(sortedLeft.begin(), sortedLeft.end(), keysLeft.begin()));
				OAK_ASSERT(std::equal(sortedRight.begin(), sortedRight.end(), keysRight.begin()));
			}
		}
	}
}
