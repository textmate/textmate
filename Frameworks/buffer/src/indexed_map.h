#ifndef INDEXED_MAP_H_MY6VEIKA
#define INDEXED_MAP_H_MY6VEIKA

#include <oak/basic_tree.h>
#include <oak/misc.h>
#include <oak/debug.h>
#include <text/format.h>

template <typename _ValT = bool>
struct indexed_map_t
{
	WATCH_LEAKS(indexed_map_t);

private:
	struct key_t
	{
		key_t () : length(0), number_of_children(0) { }
		key_t (ssize_t len, size_t numberOfChildren = 1) : length(len), number_of_children(numberOfChildren) { }

		key_t operator+ (key_t const& rhs) const { return key_t(length + rhs.length, number_of_children + rhs.number_of_children); }
		key_t operator- (key_t const& rhs) const { return key_t(length - rhs.length, number_of_children - rhs.number_of_children); }

		ssize_t length;
		size_t number_of_children;
	};

	static int comp_abs (ssize_t key, key_t const& offset, key_t const& node) { return key < offset.length + node.length ? -1 : (key == offset.length + node.length ? 0 : +1); }
	static int comp_nth (ssize_t key, key_t const& offset, key_t const& node) { return key < offset.number_of_children ? -1 : (key == offset.number_of_children ? 0 : +1); }

	typedef oak::basic_tree_t<key_t, _ValT> tree_t;
	mutable tree_t _tree; // this is made mutable because the type doesn’t have const versions of find, {upper,lower}_bound, and begin/end.

	void remove (typename tree_t::iterator first, typename tree_t::iterator last)
	{
		std::vector<ssize_t> toRemove;
		for(typename tree_t::iterator info = first; info != last; ++info)
			toRemove.push_back(info->offset.length + info->key.length);
		riterate(key, toRemove)
			remove(*key);
	}

public:
	struct iterator : public std::iterator< std::bidirectional_iterator_tag, std::pair<ssize_t, _ValT> >
	{
		iterator (tree_t& tree, typename tree_t::iterator const& base) : _tree(tree), _base(base) { update_value(); }

		bool operator== (iterator const& rhs) const { return _base == rhs._base; }
		bool operator!= (iterator const& rhs) const { return _base != rhs._base; }
		iterator& operator-- ()                     { --_base; update_value(); return *this; }
		iterator& operator++ ()                     { ++_base; update_value(); return *this; }
		size_t index () const                       { return _base != _tree.end() ? _base->offset.number_of_children : _tree.aggregated().number_of_children; }

		iterator& operator= (iterator const& rhs)   { this->~iterator(); new(this) iterator(rhs); return *this; }

		std::pair<ssize_t, _ValT> const* operator-> () const { return &_value; }
		std::pair<ssize_t, _ValT> const& operator* () const  { return _value; }

		typename tree_t::iterator base ()                    { return _base; }

	private:
		void update_value ()
		{
			if(_base != _tree.end())
			{
				_value.first  = _base->offset.length + _base->key.length;
				_value.second = _base->value;
			}
		}

		tree_t& _tree;
		typename tree_t::iterator _base;
		std::pair<ssize_t, _ValT> _value;
	};

	bool empty () const                      { return _tree.empty(); }
	void swap (indexed_map_t& rhs)           { _tree.swap(rhs._tree); }
	void clear ()                            { _tree.clear(); }
	size_t size () const                     { return _tree.aggregated().number_of_children; }
	size_t number_of_nodes () const          { return _tree.size(); }
	size_t height () const                   { return _tree.height(); }

	iterator begin () const                  { return iterator(_tree, _tree.begin());                     }
	iterator end () const                    { return iterator(_tree, _tree.end());                       }
	iterator find (ssize_t key) const        { return iterator(_tree, _tree.find(key, &comp_abs));        }
	iterator lower_bound (ssize_t key) const { return iterator(_tree, _tree.lower_bound(key, &comp_abs)); }
	iterator upper_bound (ssize_t key) const { return iterator(_tree, _tree.upper_bound(key, &comp_abs)); }
	iterator nth (size_t n) const            { return iterator(_tree, _tree.find(n, &comp_nth));          }

	void set (ssize_t pos, _ValT const& value)
	{
		auto alreadyThere = _tree.find(pos, &comp_abs);
		if(alreadyThere != _tree.end())
		{
			alreadyThere->value = value;
			return;
		}

		auto it = _tree.upper_bound(pos, &comp_abs);
		if(it != _tree.begin())
		{
			auto tmp = it;
			--tmp;
			pos -= tmp->offset.length + tmp->key.length;
		}

		if(it != _tree.end())
		{
			it->key.length -= pos;
			_tree.update_key(it);
		}

		_tree.insert(it, pos, value);
	}

	void remove (ssize_t pos)
	{
		auto it = _tree.find(pos, &comp_abs);
		if(it == _tree.end())
			return;

		auto tmp = it;
		if(++tmp != _tree.end())
		{
			tmp->key.length += it->key.length;
			_tree.update_key(tmp);
		}

		_tree.erase(it);
		ASSERT(find(pos) == end());
	}

	void remove (iterator first, iterator last)
	{
		remove(first.base(), last.base());
	}

	void replace (ssize_t from, ssize_t to, size_t newLength, bool bindRight = true)
	{
		auto it = bindRight ? _tree.lower_bound(to, &comp_abs) : _tree.upper_bound(to, &comp_abs);
		if(it != _tree.end())
		{
			it->key.length += newLength;
			_tree.update_key(it);
		}

		if(bindRight)
				remove(_tree.lower_bound(from, &comp_abs), _tree.lower_bound(to, &comp_abs));
		else	remove(_tree.upper_bound(from, &comp_abs), _tree.upper_bound(to, &comp_abs));

		it = bindRight ? _tree.lower_bound(to, &comp_abs) : _tree.upper_bound(to, &comp_abs);
		if(it != _tree.end())
		{
			it->key.length -= to - from;
			_tree.update_key(it);
		}
	}
};

#endif /* end of include guard: INDEXED_MAP_H_MY6VEIKA */
