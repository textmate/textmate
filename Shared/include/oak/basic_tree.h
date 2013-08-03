#ifndef BASIC_TREE_H_7EA7G2TN
#define BASIC_TREE_H_7EA7G2TN

#include <text/format.h>
#include <oak/debug.h>

namespace oak
{
	template <typename _KeyT, typename _ValueT = bool>
	struct basic_tree_t
	{
		basic_tree_t ()                                   { }
		basic_tree_t (basic_tree_t const& rhs)            { _root = clone_node(rhs._root); _size = rhs._size; }
		~basic_tree_t ()                                  { clear(); }
		basic_tree_t& operator= (basic_tree_t const& rhs) { _root = clone_node(rhs._root); _size = rhs._size; return *this; }

		struct value_type
		{
			value_type (_KeyT const& offset, _KeyT& key, _ValueT& value) : offset(offset), key(key), value(value) { }

			value_type& operator= (value_type const& rhs)
			{
				this->~value_type();
				new(this) value_type(rhs);
				return *this;
			}

			_KeyT offset;
			_KeyT& key;
			_ValueT& value;
		};

	private:
		struct node_t
		{
			node_t () : _left(this), _right(this), _parent(this), _level(0) { }
			node_t (_KeyT const& key, _ValueT const& value) : _left(null_ptr()), _right(null_ptr()), _parent(null_ptr()), _relative_key(key), _key_offset(key), _value(value) { }

			static node_t* null_ptr () { static node_t dummy; return &dummy; }
			bool is_null () const      { return _level == 0; }
			void update_key_offset ()  { _key_offset = _left->_key_offset + _relative_key + _right->_key_offset; }

			_KeyT const& relative_key () const { return _relative_key; }
			_KeyT const& key_offset () const   { return _key_offset; }

			static void skew (node_t*& node)
			{
				if(node->_level == node->_left->_level)
				{
					//      A              B
					//     / \            / \
					//    B   C   ===>   D   A
					//   / \                / \
					//  D   E              E   C

					if(!node->_left->_right->is_null())
						node->_left->_right->_parent = node;
					node->_left->_parent = node->_parent;
					node->_parent = node->_left;

					node_t* oldLeft = node->_left;
					node->_left = oldLeft->_right;
					oldLeft->_right = node;
					node = oldLeft;

					node->_right->update_key_offset();
					node->update_key_offset();
				}
			}

			static void split (node_t*& node)
			{
				if(node->_level == node->_right->_right->_level)
				{
					//      A              C¹
					//     / \            / \
					//    B   C   ===>   A   E
					//       / \        / \
					//      D   E      B   D    ¹Increase level by one.

					if(!node->_right->_left->is_null())
						node->_right->_left->_parent = node;
					node->_right->_parent = node->_parent;
					node->_parent = node->_right;

					node_t* oldRight = node->_right;
					node->_right = oldRight->_left;
					oldRight->_left = node;
					node = oldRight;

					node->_level++;

					node->_left->update_key_offset();
					node->update_key_offset();
				}
			}

			node_t* _left;
			node_t* _right;
			node_t* _parent;
			size_t _level = 1;

			_KeyT _relative_key;			// relative to parent->left->_key_offset + parent->_relative_key + left->_key_offset
			_KeyT _key_offset;			// left->_key_offset + _relative_key + right->_key_offset
			_ValueT _value;
		};

		static bool eq (node_t* lhs, node_t* rhs) { return (lhs->is_null() && rhs->is_null()) || lhs == rhs; }

	public:
		struct iterator : std::iterator<std::bidirectional_iterator_tag, value_type>
		{
			iterator (node_t* node, basic_tree_t* tree) : _node(node), _info(_KeyT(), _node->_relative_key, _node->_value), _tree(tree) { }

			bool operator== (iterator const& rhs) const { return  eq(_node, rhs._node); }
			bool operator!= (iterator const& rhs) const { return !eq(_node, rhs._node); }

			value_type& operator*  ()             { ASSERT(*this != _tree->end()); return _info;  }
			value_type* operator-> ()             { ASSERT(*this != _tree->end()); return &_info; }
			value_type const& operator*  () const { ASSERT(*this != _tree->end()); return _info;  }
			value_type const* operator-> () const { ASSERT(*this != _tree->end()); return &_info; }

			iterator& operator++ ()
			{
				_info.offset = _info.offset + _node->relative_key();
				if(!_node->_right->is_null())
				{
					_node = _node->_right;
					while(!_node->_left->is_null())
						_node = _node->_left;
				}
		      else
				{
		         while(eq(_node, _node->_parent->_right))
						_node = _node->_parent;
					_node = _node->_parent;
				}
				_info = value_type(_info.offset, _node->_relative_key, _node->_value);
				return *this;
			}

			iterator& operator-- ()
			{
				if(*this == _tree->end())
				{
					_node = _tree->_root;
					_info.offset = _node->_left->key_offset();
					while(!_node->_right->is_null())
					{
						_info.offset = _info.offset + _node->relative_key();
						_node = _node->_right;
						_info.offset = _info.offset + _node->_left->key_offset();
					}
				}
				else
				{
					if(!_node->_left->is_null())
					{
						_node = _node->_left;
						while(!_node->_right->is_null())
							_node = _node->_right;
					}
			      else
					{
			         while(eq(_node, _node->_parent->_left))
							_node = _node->_parent;
						_node = _node->_parent;
					}
					_info.offset = _info.offset - _node->relative_key();
				}
				_info = value_type(_info.offset, _node->_relative_key, _node->_value);
				return *this;
			}

			iterator operator-- (int)
			{
				iterator tmp(*this);
				--(*this);
				return tmp;
			}

		private:
			friend struct basic_tree_t;
			node_t* _node;
			value_type _info;
			basic_tree_t* _tree;
		};

		typedef std::reverse_iterator<iterator> reverse_iterator;

		iterator begin ()                    { node_t* res = _root; while(!res->_left->is_null()) res = res->_left; return iterator(res, this); }
		iterator end ()                      { return iterator(node_t::null_ptr(), this); }
		reverse_iterator rbegin ()           { return reverse_iterator(end()); }
		reverse_iterator rend ()             { return reverse_iterator(begin()); }

		_KeyT const& aggregated () const     { return _root->key_offset(); }

		size_t size () const                 { return _size; }
		bool empty () const                  { return _size == 0; }
		void clear ()                        { dispose_node(_root, true); _root = node_t::null_ptr(); _size = 0; }
		void swap (basic_tree_t& rhs)        { std::swap(_root, rhs._root); std::swap(_size, rhs._size); }

		iterator insert (iterator const& it, _KeyT const& key)                { return insert(it, key, _ValueT()); }
		iterator insert (iterator it, _KeyT const& key, _ValueT const& value) { return insert_node(it._node, new node_t(key, value)); }
		void erase (iterator const& it)                                       { if(it != end()) remove_node(it._node); }

		void erase (iterator it, iterator const& last)
		{
			while(it != last)
			{
				iterator tmp = it;
				++it;
				erase(tmp);
			}
		}

		void update_key (iterator it)
		{
			for(node_t* node = it._node; !node->is_null(); node = node->_parent)
				node->update_key_offset();
		}

		template <typename T, typename _Functor>
		iterator find (T key, _Functor const& comp)                 { return find(_root, key, comp); }

		template <typename T, typename _Functor>
		iterator lower_bound (T key, _Functor const& comp)          { return lower_bound(_root, key, comp); }

		template <typename T, typename _Functor>
		iterator upper_bound (T key, _Functor const& comp)          { return upper_bound(_root, key, comp); }

		// ========================
		// = Debug/test Functions =
		// ========================

		bool structural_integrity () const                          { return structural_integrity(_root); }
		size_t height () const                                      { return height(_root); }

		std::string to_s (std::function<std::string(value_type)> const& dump) const
		{
			std::string res = "";
			to_s(_root, _KeyT(), 1, res, dump);
			return res;
		}

	private:
		static void to_s (node_t* node, _KeyT const& offset, size_t level, std::string& out, std::function<std::string(value_type)> const& dump)
		{
			if(!node->is_null())
			{
				to_s(node->_right, offset + node->_left->key_offset() + node->relative_key(), level + 1, out, dump);
				out += std::string(2*level, ' ') + text::format("%p, %zu — ", node, node->_level) + dump(value_type(offset + node->_left->key_offset(), node->_relative_key, node->_value)) + "\n";
				to_s(node->_left, offset, level + 1, out, dump);
			}
		}

		static bool structural_integrity (node_t* node)
		{
			if(node->is_null())
				return true;

			bool res = true;
			if(!node->_left->is_null() && !eq(node, node->_left->_parent))
				return fprintf(stderr, "parent of %p is %p, should be %p\n", node->_left, node->_left->_parent, node), false;
			if(!node->_right->is_null() && !eq(node, node->_right->_parent))
				return fprintf(stderr, "parent of %p is %p, should be %p\n", node->_right, node->_right->_parent, node), false;
		
			res = res && (node->_level == node->_left->is_null() ? 1 : node->_left->_level+1);
			res = res && (node->_level - node->_right->_level <= 1);
			res = res && (node->_level > node->_right->_right->_level);

			if(!(node->_key_offset == node->_left->_key_offset + node->_relative_key + node->_right->_key_offset))
				return fprintf(stderr, "left/right sum of %p is wrong\n", node), false;

			return res && structural_integrity(node->_left) && structural_integrity(node->_right);
		}

		static size_t height (node_t* root)
		{
			return root->is_null() ? 0 : 1 + std::max(height(root->_left), height(root->_right));
		}

		template <typename T, typename _Functor>
		iterator lower_bound (node_t* root, T key, _Functor const& comp)
		{
			iterator x(root, this), y = end();
			while(!x._node->is_null())
			{
				int ternary = comp(key, x->offset + x._node->_left->key_offset(), x._node->relative_key());
				if(ternary <= 0) // key <= node
				{
					y = x;
					x._node = x._node->_left;
				}
				else // key > node
				{
					x._info.offset = x._info.offset + x._node->_left->key_offset() + x._node->relative_key();
					x._node = x._node->_right;
				}
			}

			if(!y._node->is_null())
				y._info = value_type(y._info.offset + y._node->_left->key_offset(), y._node->_relative_key, y._node->_value);

			return y;
		}

		template <typename T, typename _Functor>
		iterator upper_bound (node_t* root, T key, _Functor const& comp)
		{
			iterator x(root, this), y = end();
			while(!x._node->is_null())
			{
				int ternary = comp(key, x->offset + x._node->_left->key_offset(), x._node->relative_key());
				if(ternary < 0) // key < node
				{
					y = x;
					x._node = x._node->_left;
				}
				else // key >= node
				{
					x._info.offset = x._info.offset + x._node->_left->key_offset() + x._node->relative_key();
					x._node = x._node->_right;
				}
			}

			if(!y._node->is_null())
				y._info = value_type(y._info.offset + y._node->_left->key_offset(), y._node->_relative_key, y._node->_value);

			return y;
		}

		template <typename T, typename _Functor>
		iterator find (node_t* root, T key, _Functor const& comp)
		{
			iterator res = lower_bound(root, key, comp);
			return res != end() && comp(key, res->offset, res._node->relative_key()) == 0 ? res : end();
		}

		static node_t* pred (node_t* node) { for(node = node->_left;  !node->_right->is_null(); ) node = node->_right; return node; }
		static node_t* succ (node_t* node) { for(node = node->_right; !node->_left->is_null();  ) node = node->_left;  return node; }

		iterator insert_node (node_t* node, node_t* newNode)
		{
			if(node->is_null())
			{
				node_t* last = _root;
				while(!last->_right->is_null())
					last = last->_right;

				newNode->_parent = last;
				if(last->is_null())
						_root = newNode;
				else	newNode->_parent->_right = newNode;
			}
			else if(node->_left->is_null())
			{
				newNode->_parent = node;
				newNode->_parent->_left = newNode;
			}
			else
			{
				newNode->_parent = pred(node);
				newNode->_parent->_right = newNode;
			}

			for(node_t* bottom = newNode; !bottom->is_null(); bottom = bottom->_parent)
			{
				bottom->update_key_offset();

				node_t** ref = NULL;
				if(bottom->_parent->is_null())
					ref = &_root;
				else if(eq(bottom->_parent->_left, bottom))
					ref = &bottom->_parent->_left;
				else
					ref = &bottom->_parent->_right;

				node_t::skew(*ref);
				node_t::split(*ref);

				bottom = *ref;
			}

			++_size;

			iterator res(newNode, this);
			res._info.offset = newNode->_left->key_offset();
			for(node_t* bottom = newNode; !bottom->_parent->is_null(); bottom = bottom->_parent)
			{
				if(eq(bottom->_parent->_right, bottom))
					res._info.offset = res._info.offset + bottom->_parent->_left->key_offset() + bottom->_parent->relative_key();
			}
			return res;
		}

		static void dispose_node (node_t* node, bool recursive)
		{
			if(node->is_null())
				return;

			size_t index = 0;
			std::vector<node_t*> queue(1, node);
			while(index < queue.size() && recursive)
			{
				node_t* current = queue[index++];
				if(!current->_left->is_null())
					queue.push_back(current->_left);
				if(!current->_right->is_null())
					queue.push_back(current->_right);
			}

			iterate(node, queue)
				delete *node;
		}

		void remove_node (node_t* node)
		{
			node_t* leaf     = node_t::null_ptr();
			node_t* bottom   = node->_parent;

		   if(!node->_left->is_null() || !node->_right->is_null())
			{
				leaf   = node->_left->is_null() ? succ(node) : pred(node);
				bottom = leaf->_parent;

				if(!eq(leaf->_parent, node))
				{
					if(!leaf->_left->is_null())
						leaf->_left->_parent = leaf->_parent;
					if(!leaf->_right->is_null())
						leaf->_right->_parent = leaf->_parent;

					if(eq(leaf->_parent->_left, leaf))
							leaf->_parent->_left  = leaf->_left->is_null() ? leaf->_right : leaf->_left;
					else	leaf->_parent->_right = leaf->_left->is_null() ? leaf->_right : leaf->_left;

					leaf->_left  = node->_left;
					leaf->_right = node->_right;
				}
				else
				{
					if(!eq(node->_left, leaf))
							leaf->_left = node->_left;
					else	leaf->_right = node->_right;

					bottom = leaf;
				}

				if(!leaf->_left->is_null())
					leaf->_left->_parent = leaf;
				if(!leaf->_right->is_null())
					leaf->_right->_parent = leaf;

				leaf->_parent = node->_parent;
				leaf->_level  = node->_level;
			}

			if(node->_parent->is_null())
				_root = leaf;
			else if(eq(node->_parent->_left, node))
				node->_parent->_left = leaf;
			else if(eq(node->_parent->_right, node))
				node->_parent->_right = leaf;

			for(; !bottom->is_null(); bottom = bottom->_parent)
			{
				bottom->update_key_offset();

				if(bottom->_level - bottom->_left->_level > 1 || bottom->_level - bottom->_right->_level > 1)
				{
					if(--bottom->_level < bottom->_right->_level)
						--bottom->_right->_level;

					node_t** ref = NULL;
					if(bottom->_parent->is_null())
						ref = &_root;
					else if(eq(bottom->_parent->_left, bottom))
						ref = &bottom->_parent->_left;
					else
						ref = &bottom->_parent->_right;

					node_t::skew((*ref));
					node_t::skew((*ref)->_right);
					node_t::skew((*ref)->_right->_right);
					node_t::split((*ref));
					node_t::split((*ref)->_right);

					bottom = *ref;
				}
			}

			--_size;
			dispose_node(node, false);
		}

		static node_t* clone_node (node_t* node, node_t* parent = node_t::null_ptr())
		{
			if(node->is_null())
				return node;

			node_t* res = new node_t(node->_relative_key, node->_value);
			res->_left       = clone_node(node->_left,  res);
			res->_right      = clone_node(node->_right, res);
			res->_parent     = parent;
			res->_level      = node->_level;
			res->_key_offset = node->_key_offset;
			return res;
		}

	private:
		node_t* _root = node_t::null_ptr();
		size_t _size = 0;
	};

} /* oak */

#endif /* end of include guard: BASIC_TREE_H_7EA7G2TN */
