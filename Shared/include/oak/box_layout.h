#ifndef BOX_LAYOUT_H_JU6T39XY
#define BOX_LAYOUT_H_JU6T39XY

#include "oak.h"

namespace oak
{
	template <typename T>
	struct box_layout_t
	{
		box_layout_t () : last_leaf(0) { }

		size_t add       (T const& newValue)                        { return add(newValue, place_below, 0); }
		size_t add_left  (T const& newValue, size_t relativeTo = 0) { return add(newValue, place_left,  relativeTo); }
		size_t add_right (T const& newValue, size_t relativeTo = 0) { return add(newValue, place_right, relativeTo); }
		size_t add_above (T const& newValue, size_t relativeTo = 0) { return add(newValue, place_above, relativeTo); }
		size_t add_below (T const& newValue, size_t relativeTo = 0) { return add(newValue, place_below, relativeTo); }

		void remove (size_t leaf)
		{
			container = container.identifier == leaf ? box_t() : remove(container, leaf);
			leafs.erase(leafs.find(leaf));
		}

		void layout (size_t x0, size_t x1, size_t y0, size_t y1)
		{
			layout(container, x0, x1, y0, y1);
		}

	private:
		enum placement_t { place_left, place_above, place_right, place_below };
		enum orientation_t { rows, columns };

		struct box_t
		{
			box_t (size_t identifier = 0) : identifier(identifier) { }
			size_t identifier;
			orientation_t orientation;
			std::vector<box_t> children;
		};

		size_t last_leaf;
		std::map<size_t, T> leafs;
		box_t container;

		box_t add (box_t& box, size_t newLeaf, placement_t placement, size_t relativeTo)
		{
			if(box.identifier == relativeTo)
			{
				box_t wrapper;
				wrapper.orientation = (placement == place_left || placement == place_right) ? columns : rows;
				wrapper.children.push_back(box);
				wrapper.children.insert(placement == place_left || placement == place_above ? wrapper.children.begin() : wrapper.children.end(), newLeaf);
				return wrapper;
			}
			else
			{
				iterate(child, box.children)
					*child = add(*child, newLeaf, placement, relativeTo);
			}
			return box;
		}

		size_t add (T const& newValue, placement_t placement, size_t relativeTo)
		{
			leafs.insert(std::make_pair(++last_leaf, newValue));

			if(relativeTo)
				container = add(container, last_leaf, placement, relativeTo);
			else if(container.children.empty())
				container = last_leaf;
			else
			{
				box_t wrapper;
				wrapper.orientation = (placement == place_left || placement == place_right) ? columns : rows;
				wrapper.children.push_back(container);
				wrapper.children.insert(placement == place_left || placement == place_above ? wrapper.children.begin() : wrapper.children.end(), last_leaf);
				container = wrapper;
			}

			return last_leaf;
		}

		box_t remove (box_t& box, size_t leaf)
		{
			std::vector<box_t> newChildren;
			iterate(child, box.children)
			{
				if(child->identifier != leaf)
					newChildren.push_back(remove(*child, leaf));
				else	fprintf(stderr, "found and removed %zu\n", leaf);
			}
			box.children.swap(newChildren);

			switch(box.children.size())
			{
				case 0:  return box;
				case 1:  return box.children.back();
				default: return box;
			}
		}

		// ==================
		// = Layout Helpers =
		// ==================

		size_t number_of (box_t const& box, orientation_t unit)
		{
			if(box.identifier)
				return 1;
			else if(box.orientation != unit)
				return 1;

			size_t res = 0;
			iterate(child, box.children)
				res += number_of(*child, unit);
			return res;
		}

		size_t weight (box_t const& box, orientation_t unit)
		{
			size_t res = 0;
			if(box.identifier)
			{
				res = (unit == rows ? max_height(leafs[box.identifier]) : max_width(leafs[box.identifier])) == SIZE_T_MAX ? 1 : 0;
			}
			else if(box.orientation != unit)
			{
				iterate(child, box.children)
					res = std::max(weight(*child, unit), res);
			}
			else if(box.orientation == unit)
			{
				iterate(child, box.children)
					res += weight(*child, unit);
			}
			return res;
		}

		size_t min_size (box_t const& box, orientation_t unit)
		{
			size_t res = 0;
			if(box.identifier)
			{
				res = unit == rows ? min_height(leafs[box.identifier]) : min_width(leafs[box.identifier]);
			}
			else if(box.orientation != unit)
			{
				iterate(child, box.children)
					res = std::max(min_size(*child, unit), res);
			}
			else if(box.orientation == unit)
			{
				iterate(child, box.children)
					res += min_size(*child, unit);
			}
			return res;
		}

		size_t max_size (box_t const& box, orientation_t unit)
		{
			size_t res = 0;
			if(box.identifier)
			{
				res = unit == rows ? max_height(leafs[box.identifier]) : max_width(leafs[box.identifier]);
			}
			else if(box.orientation != unit)
			{
				res = SIZE_T_MAX;
				iterate(child, box.children)
					res = std::min(max_size(*child, unit), res);
			}
			else if(box.orientation == unit)
			{
				iterate(child, box.children)
				{
					if(max_size(*child, unit) == SIZE_T_MAX)
						return SIZE_T_MAX;
					res += max_size(*child, unit);
				}
			}
			return res;
		}

		void layout (box_t& box, size_t x0, size_t x1, size_t y0, size_t y1)
		{
			if(box.identifier)
			{
				set_frame(leafs[box.identifier], x0, x1, y0, y1);
			}
			else
			{
				size_t totalSize = box.orientation == rows ? y1 - y0 : x1 - x0;
				size_t minSize = 0, maxSize = 0, totalDelta = 0;
				iterate(child, box.children)
				{
					size_t boxWeight = weight(*child, box.orientation);
					size_t boxMin    = min_size(*child, box.orientation);
					size_t boxMax    = max_size(*child, box.orientation) == SIZE_T_MAX ? totalSize : max_size(*child, box.orientation);

					minSize += boxMin;
					maxSize += boxMax;
					totalDelta += boxWeight * (boxMax - boxMin);
				}

				size_t available = totalSize - minSize;

				size_t delta = 0, reserved = 0;
				iterate(child, box.children)
				{
					size_t from = reserved + (totalDelta ? available * delta / totalDelta : 0);
					size_t boxWeight = weight(*child, box.orientation);
					size_t boxMin    = min_size(*child, box.orientation);
					size_t boxMax    = max_size(*child, box.orientation) == SIZE_T_MAX ? totalSize : max_size(*child, box.orientation);
					delta += boxWeight * (boxMax - boxMin);
					reserved += boxMin;
					size_t to = reserved + (totalDelta ? available * delta / totalDelta : 0);

					if(box.orientation == rows)
							layout(*child, x0, x1, y0 + from, y0 + to);
					else	layout(*child, x0 + from, x0 + to, y0, y1);
				}
			}
		}

		// =========
		// = Other =
		// =========

		void print (box_t const& box, std::string const& indent = "")
		{
			if(box.identifier)
			{
				fprintf(stderr, "%s* %s (%zu)\n", indent.c_str(), description(leafs[box.identifier]).c_str(), box.identifier);
			}
			else
			{
				fprintf(stderr, "%s* rows/columns %zu×%zu, weights %zu×%zu, min/max sizes: %zu-%zu / %zu-%zu\n", indent.c_str(), number_of(box, rows), number_of(box, columns), weight(box, rows), weight(box, columns), min_size(box, rows), max_size(box, rows), min_size(box, columns), max_size(box, columns));
				iterate(child, box.children)
					print(*child, indent + "   ");
			}
		}
public:
		void print () { print(container); }
	};

} /* oak */

#endif /* end of include guard: BOX_LAYOUT_H_JU6T39XY */
