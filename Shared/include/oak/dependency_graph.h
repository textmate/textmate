#ifndef DEPENDENCY_GRAPH_H_A254DOQ
#define DEPENDENCY_GRAPH_H_A254DOQ

namespace oak
{
	struct dependency_graph
	{
		// a node is an integer and represents a task to perform
		void add_node (size_t node)
		{
			dependencies.emplace(node, std::set<size_t>());
		}

		// an edge establishes a dependency from one node (first argument) to another (second argument)
		// e.g. if node RUN depends on node BUILD we call: add_edge(RUN, NODE)
		void add_edge (size_t node, size_t dependsOn)
		{
			dependencies[node].insert(dependsOn);
		}

		// mark a node as “modified” and return all nodes which in effect are then also modified
		std::set<size_t> touch (size_t node) const
		{
			std::set<size_t> res;
			std::vector<size_t> active(1, node);

			while(!active.empty())
			{
				size_t n = active.back();
				res.insert(n);
				active.pop_back();

				for(auto const& it : dependencies)
				{
					std::set<size_t>::const_iterator node = it.second.find(n);
					if(node == it.second.end() || res.find(it.first) != res.end())
						continue;

					active.push_back(it.first);
				}
			}

			return res;
		}

		// return list of all nodes ordered so that for each node, all it depends on is before that node in this list
		std::vector<size_t> topological_order () const
		{
			std::vector<size_t> res, active;
			for(auto const& it : dependencies)
			{
				if(it.second.empty())
					active.push_back(it.first);
			}

			std::map< size_t, std::set<size_t> > tmp = dependencies;
			while(!active.empty())
			{
				size_t n = active.back();
				res.push_back(n);
				active.pop_back();

				for(auto& it : tmp)
				{
					std::set<size_t>::const_iterator node = it.second.find(n);
					if(node == it.second.end())
						continue;

					it.second.erase(node);
					if(it.second.empty())
						active.push_back(it.first);
				}
			}

			return res;
		}

	private:
		std::map< size_t, std::set<size_t> > dependencies;
	};

} /* oak */ 

#endif /* end of include guard: DEPENDENCY_GRAPH_H_A254DOQ */
