#include "box.h"
#include <oak/debug.h>

int kViewSpacing = 1;

box_t::box_t (int width, int height, int minWidth, int minHeight, bool locked, bool horizontal) : _bounds(0, width, 0, height), _min_width(minWidth), _min_height(minHeight), _ideal_width(width), _ideal_height(height), _locked(locked), _horizontal_layout(horizontal)
{
	ASSERT_LE(_min_width, _bounds.width());
	ASSERT_LE(_min_height, _bounds.height());
}

void box_t::push_front (box_t* box)
{
	ASSERT_LE(box->min_width(), box->width());
	ASSERT_LE(box->min_height(), box->height());

	rect_t bounds = _bounds;
	if(_horizontal_layout)
	{
		bounds.move_to(_bounds.x0 - box->width() - kViewSpacing, _bounds.y0);
		bounds.set_width(box->width());
		bounds.set_height(std::max(height(), box->min_height()));
		box->set_size(bounds);
		_bounds.x0 = box->x0();
	}
	else
	{
		bounds.move_to(_bounds.x0, _bounds.y0 - box->height() - kViewSpacing);
		bounds.set_width(std::max(width(), box->min_width()));
		bounds.set_height(box->height());
		box->set_size(bounds);
		_bounds.y0 = box->y0();
	}
	_children.insert(_children.begin(), box);
}

void box_t::push_back (box_t* box)
{
	ASSERT_LE(box->min_width(), box->width());
	ASSERT_LE(box->min_height(), box->height());

	rect_t bounds = _bounds;
	if(_horizontal_layout)
	{
		bounds.move_to(_bounds.x1 + kViewSpacing, _bounds.y0);
		bounds.set_width(box->width());
		bounds.set_height(std::max(height(), box->min_height()));
		box->set_size(bounds);
		_bounds.set_width(_bounds.width() + kViewSpacing + box->width());
	}
	else
	{
		bounds.move_to(_bounds.x0, _bounds.y1 + kViewSpacing);
		bounds.set_width(std::max(width(), box->min_width()));
		bounds.set_height(box->height());
		box->set_size(bounds);
		_bounds.set_height(_bounds.height() + kViewSpacing + box->height());
	}
	_children.push_back(box);
}

box_t* box_t::erase (box_t* box)
{
	if(box == this)
	{
		delete this;
		return NULL;
	}

	if(_children.empty())
		return this;

	std::vector<box_t*> newChildren;
	iterate(child, _children)
		*child = (*child)->erase(box);

	for(size_t i = 0; i < _children.size(); ++i)
	{
		if(_children[i] == NULL)
		{
			if(_horizontal_layout)
			{
				if(i != 0 && i+1 != _children.size())
				{
					rect_t left = _children[i-1]->bounds(), right = _children[i+1]->bounds();
					int x = (left.x1 + right.x0) / 2;
					left.x1 = x;
					right.x0 = x + kViewSpacing;
					_children[i-1]->set_size(left);
					_children[i+1]->set_size(right);
				}
				else if(i != 0)
				{
					rect_t left = _children[i-1]->bounds();
					left.x1 = _bounds.x1;
					_children[i-1]->set_size(left);
				}
				else if(i+1 != _children.size())
				{
					rect_t right = _children[i+1]->bounds();
					right.x0 = _bounds.x0;
					_children[i+1]->set_size(right);
				}
			}
			else
			{
				if(i != 0 && i+1 != _children.size())
				{
					rect_t left = _children[i-1]->bounds(), right = _children[i+1]->bounds();
					int y = (left.y1 + right.y0) / 2;
					left.y1 = y;
					right.y0 = y + kViewSpacing;
					_children[i-1]->set_size(left);
					_children[i+1]->set_size(right);
				}
				else if(i != 0)
				{
					rect_t left = _children[i-1]->bounds();
					left.y1 = _bounds.y1;
					_children[i-1]->set_size(left);
				}
				else if(i+1 != _children.size())
				{
					rect_t right = _children[i+1]->bounds();
					right.y0 = _bounds.y0;
					_children[i+1]->set_size(right);
				}
			}

			_children.erase(_children.begin() + i);
			break;
		}
	}

	if(_children.empty())
	{
		delete this;
		return NULL;
	}
	else if(_children.size() == 1)
	{
		box_t* res = _children.back();
		delete this;
		return res;
	}
	return this;
}

box_t* box_t::find (int x, int y)
{
	if(!_bounds.contains(x, y))
		return NULL;

	iterate(child, _children)
	{
		if(box_t* res = (*child)->find(x, y))
			return res;
	}
	return this;
}

struct size_info_t
{
	size_info_t (size_t minSize, size_t extraSize, bool locked) : size(minSize), extra_size(extraSize), locked(locked) { }

	size_t size;
	size_t extra_size;
	bool locked;
};

static void layout (std::vector<size_info_t>& sizes, int size)
{
	int idealWidth = 0;
	int lockedWidth = 0;
	int flexibleWidth = 0;
	iterate(it, sizes)
	{
		idealWidth += it->extra_size;
		if(it->locked)
				lockedWidth += it->extra_size;
		else	flexibleWidth += it->extra_size;
		size -= it->size;
	}

	if(size >= lockedWidth)
	{
		size -= lockedWidth;
		int counter = 0;
		iterate(it, sizes)
		{
			if(it->locked)
			{
				it->size += it->extra_size;
			}
			else
			{
				it->size += size * (counter + it->extra_size) / flexibleWidth - size * counter / flexibleWidth;
				counter += it->extra_size;
			}
		}
		ASSERT_EQ(counter, flexibleWidth);
	}
	else
	{
		int counter = 0;
		iterate(it, sizes)
		{
			if(it->locked)
			{
				it->size += size * (counter + it->extra_size) / lockedWidth - size * counter / lockedWidth;
				counter += it->extra_size;
			}
		}
		ASSERT_EQ(counter, lockedWidth);
	}
}

void box_t::set_size (rect_t const& newBounds)
{
	ASSERT_GE(newBounds.width(), min_width());
	ASSERT_GE(newBounds.height(), min_height());

	if(_children.empty())
	{
		_bounds = newBounds;
		return;
	}

	std::vector<size_info_t> sizes;
	if(_horizontal_layout)
	{
		iterate(child, _children)
			sizes.push_back(size_info_t((*child)->min_width(), (*child)->_ideal_width - (*child)->min_width(), (*child)->locked()));
		layout(sizes, newBounds.width() - kViewSpacing * (_children.size() - 1));

		int x = newBounds.x0;
		for(size_t i = 0; i < _children.size(); ++i)
		{
			_children[i]->set_size(rect_t(x, x + sizes[i].size, newBounds.y0, newBounds.y1));
			x += sizes[i].size + kViewSpacing;
		}
	}
	else
	{
		iterate(child, _children)
			sizes.push_back(size_info_t((*child)->min_height(), (*child)->_ideal_height - (*child)->min_height(), (*child)->locked()));
		layout(sizes, newBounds.height() - kViewSpacing * (_children.size() - 1));

		int y = newBounds.y0;
		for(size_t i = 0; i < _children.size(); ++i)
		{
			_children[i]->set_size(rect_t(newBounds.x0, newBounds.x1, y, y + sizes[i].size));
			y += sizes[i].size + kViewSpacing;
		}
	}

	_bounds = newBounds;
}

void box_t::freeze_layout ()
{
	iterate(child, _children)
		(*child)->freeze_layout();

	_ideal_width  = width();
	_ideal_height = height();
}

bool box_t::resize (box_t* box, rect_t const& newRect)
{
	if(box == this)
		return _bounds == newRect ? false : (set_size(newRect), true);

	for(size_t i = 0; i < _children.size(); ++i)
	{
		rect_t oldRect = _children[i]->bounds();
		if(_children[i]->resize(box, newRect))
		{
			rect_t newRect = _children[i]->bounds();
			if(_horizontal_layout)
			{
				if(oldRect.height() != newRect.height())
				{
					iterate(child, _children)
					{
						rect_t bounds = (*child)->bounds();
						bounds.y0 = newRect.y0;
						bounds.y1 = newRect.y1;
						(*child)->set_size(bounds);
					}
				}
				else
				{
					if(0 < i && oldRect.x0 != newRect.x0)
					{
						rect_t bounds = _children[i-1]->bounds();
						bounds.x1 = newRect.x0 - kViewSpacing;
						_children[i-1]->set_size(bounds);
					}

					if(i+1 < _children.size() && oldRect.x1 != newRect.x1)
					{
						rect_t bounds = _children[i+1]->bounds();
						bounds.x0 = newRect.x1 + kViewSpacing;
						_children[i+1]->set_size(bounds);
					}
				}
			}
			else
			{
				if(oldRect.width() != newRect.width())
				{
					iterate(child, _children)
					{
						rect_t bounds = (*child)->bounds();
						bounds.x0 = newRect.x0;
						bounds.x1 = newRect.x1;
						(*child)->set_size(bounds);
					}
				}
				else
				{
					if(0 < i && oldRect.y0 != newRect.y0)
					{
						rect_t bounds = _children[i-1]->bounds();
						bounds.y1 = newRect.y0 - kViewSpacing;
						_children[i-1]->set_size(bounds);
					}

					if(i+1 < _children.size() && oldRect.y1 != newRect.y1)
					{
						rect_t bounds = _children[i+1]->bounds();
						bounds.y0 = newRect.y1 + kViewSpacing;
						_children[i+1]->set_size(bounds);
					}
				}
			}

			rect_t oldBounds = _bounds;
			_bounds = rect_t(INT_MAX, INT_MIN, INT_MAX, INT_MIN);
			iterate(child, _children)
			{
				_bounds.x0 = std::min(_bounds.x0, (*child)->x0());
				_bounds.x1 = std::max(_bounds.x1, (*child)->x1());
				_bounds.y0 = std::min(_bounds.y0, (*child)->y0());
				_bounds.y1 = std::max(_bounds.y1, (*child)->y1());
			}

			return _bounds != oldBounds;
		}
	}
	return false;
}
