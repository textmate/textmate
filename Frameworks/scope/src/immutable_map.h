#ifndef IMMUTABLE_MAP_H_H204NXEZ
#define IMMUTABLE_MAP_H_H204NXEZ

template <typename K, typename V>
class immutable_map {
	typedef std::vector<std::pair<K,V> > backing_type;
#define CUT_OFF 5
	backing_type backing;
	struct pair_less
	{
		bool operator() ( const std::pair<K, V>& lhs, const K& rhs ) const 
		{ return lhs.first < rhs; }
		bool operator() ( const K& lhs, const std::pair<K, V >& rhs ) const 
		{ return lhs < rhs.first; }
	};
	
public:
	typedef typename backing_type::const_iterator const_iterator;
private:
	const_iterator linear_find ( const K& value ) const
	{ 
		typename backing_type::const_iterator iter = backing.begin();
		while( iter != backing.end())
		{
			if(iter->first == value)
				return iter;
			iter++;
		}
		return iter;
	}

	const_iterator binary_find ( const K& value ) const
	{ 
		typename backing_type::const_iterator first = lower_bound(backing.begin(), backing.end(), value, pair_less());
		if(backing.end() != first && value == first->first)
			return first;
		return backing.end();
	}

	template<typename Comparator>
	const_iterator binary_search (const K& key, Comparator comp) const
	{

		int imin = 0;
		int imax = backing.size() - 1;
	  // continue searching while [imin,imax] is not empty

	  while (imax >= imin)
	    {
			 signed int imid = (imin + imax) / 2;
			if (comp(backing.at(imid), key))
				imin = imid + 1;
	      else if (comp(key, backing.at(imid)))
				imax = imid - 1;
	      else{
				assert(0 <= imid && imid < backing.size());
				return backing.begin() + imid;
			}
		}
		// key not found
		return backing.end();
	}
public:

 template<typename InputIterator>
   immutable_map (InputIterator first, InputIterator last) : backing(first, last) {}
	immutable_map () {}
	const_iterator begin () const { return backing.begin();}
	const_iterator end () const { return backing.end();}
	
	size_t size () const { return backing.size();}

	const_iterator find ( const K& value ) const
	{
		if(size() > 9)
			return binary_search(value, pair_less());
		else
			return linear_find(value);
	}
	
	
};
#endif /* end of include guard: IMMUTABLE_MAP_H_H204NXEZ */