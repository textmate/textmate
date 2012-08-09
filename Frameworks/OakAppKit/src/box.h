#ifndef BOX_H_LQ7DU4L2
#define BOX_H_LQ7DU4L2

#include <oak/oak.h>
#include <text/format.h>

struct rect_t
{
	rect_t (int x0, int x1, int y0, int y1) : x0(x0), x1(x1), y0(y0), y1(y1) { }
	int width () const                 { return x1 - x0; }
	int height () const                { return y1 - y0; }
	void set_width (int width)         { x1 = x0 + width; }
	void set_height (int height)       { y1 = y0 + height; }
	void move_to (int x, int y)        { *this = rect_t(x, x + width(), y, y + height()); }
	bool contains (int x, int y) const { return oak::cap(x0, x, x1-1) == x && oak::cap(y0, y, y1-1) == y; }

	bool operator== (rect_t const& rhs) const { return x0 == rhs.x0 && x1 == rhs.x1 && y0 == rhs.y0 && y1 == rhs.y1; }
	bool operator!= (rect_t const& rhs) const { return x0 != rhs.x0 || x1 != rhs.x1 || y0 != rhs.y0 || y1 != rhs.y1; }

	int x0, x1, y0, y1;
};

inline std::string to_s (rect_t const& r)
{
	return text::format("%d, %d — %d × %d", r.x0, r.y0, r.width(), r.height());
}

extern int kViewSpacing;

struct box_t
{
	box_t (int width = 0, int height = 0, int minWidth = 0, int minHeight = 0, bool locked = false, bool horizontal = true);

	int x0 () const                              { return _bounds.x0; }
	int y0 () const                              { return _bounds.y0; }
	int x1 () const                              { return _bounds.x1; }
	int y1 () const                              { return _bounds.y1; }
	int width () const                           { return _bounds.width(); }
	int height () const                          { return _bounds.height(); }
	void set_width (int aWidth)                  { set_size(rect_t(x0(), x0() + aWidth, y0(), y1())); }
	void set_height (int aHeight)                { set_size(rect_t(x0(), x1(), y0(), y0() + aHeight)); }
	rect_t const& bounds () const                { return _bounds; }

	bool locked () const                         { return _locked; }
	void set_locked (bool flag)                  { _locked = flag; }
	bool horizontal () const                     { return _horizontal_layout; }
	std::vector<box_t*>& children ()             { return _children; }
	std::vector<box_t*> const& children () const { return _children; }

	int min_width () const
	{
		int res = 0;
		iterate(child, _children)
			res = _horizontal_layout ? res + (*child)->min_width() : std::max(res, (*child)->min_width());
		return std::max(res, _min_width);
	}

	int min_height () const
	{
		int res = 0;
		iterate(child, _children)
			res = !_horizontal_layout ? res + (*child)->min_height() : std::max(res, (*child)->min_height());
		return std::max(res, _min_height);
	}

	void set_size (rect_t const& newBounds);
	bool resize (box_t* box, rect_t const& newRect);
	void freeze_layout ();
	box_t* find (int x, int y);
	void push_front (box_t* box);
	void push_back (box_t* box);
	box_t* erase (box_t* box);

private:
	rect_t _bounds;
	int _min_width, _min_height;
	int _ideal_width, _ideal_height;
	std::vector<box_t*> _children;
	bool _locked, _horizontal_layout;
};

#endif /* end of include guard: BOX_H_LQ7DU4L2 */
