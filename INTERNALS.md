Developer Documentation
=======================

TextMate is written in Objective-C++: the low-level data structures (mostly non-GUI specific code) are written in C++, the GUI part in Objective-C++ (the C++ part here coming from the need to use the low-level C++ data structures).

## Model

### `oak::basic_tree_t`
This is basically a balanced [binary indexed](http://en.wikipedia.org/wiki/Fenwick_tree) tree. I.e. it has 2 specifics:

* it is *balanced*: this is achieved by using an [AA-tree](http://en.wikipedia.org/wiki/AA_tree)
* it is a *binary indexed tree*: you have O(1) access to `std::accumulate(tree.begin(), it, key_type(), [](key_type const& key, value_type const& value) -> key_type { return key + value.key })` for any `it`

It is a template parameterized by 2 types:

* *`key_type`*: this type has to implement the `+` and `-` operations, and also a default constructor that yields the identity element w.r.t. the operations (i.e. for any `key_type key`, it must hold that `key + key_type() == key_type() + key == key - key_type() == key_type() - key`).
* *`value`*: the value stored by each tree node

When iterating over the values in the tree, the iterator's value type (i.e. what you get from `*it`) has 3 members:

* `offset`: result of `std::accumulate(tree.begin(), it, key_type(), [](key_type const& key, value_type const& value) -> key_type { return key + value.key })`
* `key`: simply a reference to the key user stored in the node
* `value`: simply a reference the value the user stored in the node

Note that for the `key` and `value` members, a reference to the actual object is stored. While it's not a surprise that you can modify the `it->value`, what's really interesting is that you can modify an `it->key` and then call `tree->update_key(it)` to make the tree recalculate the `offset` information for the whole tree (takes O(log(N))).

Unlike the standard associative containers which have a comparison object inherent in their type (as a template parameter), with `oak::basic_tree_t` you pass a comparison object directly to the methods working with comparisons. These are `lower_bound`, `upper_bound` and `find`.

Also unlike the standard comparison object which takes 2 parameters (and models a `<` relation), here the comparison object takes 3 parameters, all of type `key_type`: `search`, `offset` and `key`. The `search` parameter is the one passed to one of the 3 comparison methods above. The `offset` and `key` parameters correspond to an iterator's value_type. The object returns a value in the set {-1, 0, 1}: -1 means iterator's node is "less" than search, 0 means it is "equal" and 1 means it is "more". Analogically to the standard associative containers then, the comparison methods return the following:

* `it = tree.lower_bound(search, comp)`: then `it` is the first node for which `comp(search, it->offset, it->key) != 1` (i.e. the first node that is "not less than" `search`)
* `it = tree.upper_bound(search, comp)`: then `it` is the first node for which `comp(search, it->offset, it->key) == -1` (i.e. the first node that is "more than" `search`)
* `it = tree.find(search, comp)`: `it` is the node for which `comp(search, it->offset, it->key) == 0`, or `tree->end()` if no such node exists (i.e. the first node that is "equal" to `search`)

`oak::basic_tree_t` is a very important data structure in TextMate as it is used in various places and contexts, including text storage, layout, to implement `ng::indexed_map_t` (see later), etc.

### `ng::detail::storage_t`
This is a type used to store a (potentially big) sequence of bytes using chunks of memory stored in `oak::basic_tree_t`. Think of it as an efficient std::string :-). More specifically, inserting and deleting a string in a storage representing a string of length `N` is better than `O(N)`.

### `ng::buffer_t`
This type builds on top of the raw character storage provided by `ng::detail::storage_t` and provides some semantical services for the text stored within it:

* _lines_: it detects newline characters and provides a way to translate between position in text and the line and column number
* _spelling_: it checks the text for spelling errors and provides a way to retrieve them
* _scopes_: it parses the text (using one or more bundles) and assigns one or more *scopes* to some ranges of the text; these usually correspond to various markup or syntax parts of the language the text is written in
* _marks_: TODO

### `ng::indexed_map_t`
This data structure is what it is called:

* it's a *map*: behaves like std::map<ssize_t, ValT> in that it provides the `find`, `lower_bound` and `upper_bound` methods
* it's *indexed*: you get O(log(N)) access to its n-th element for any `n`

It is implemented as an `oak::basic_tree_t` which dictates its `key_type` and provides some services built around the `key_type` members:

* `number_of_children`: this enables the efficient indexing of nodes, i.e. getting the n-th iterator is O(log(N)) (instead of O(N) in the general `oak::basic_tree_t`)
* `length`: this is used for the `std::map`-like functionality.

This structure basically provides a segment tree where a value is valid for a specific `ssize_t` range: `ng::indexed_map_t::iterator it` represents a value `it->value` that is valid in the semi-open range `[it.base()->offset.length, it.base()->offset.length + it.base()->key.length)`, and also that it is the `it->offset.number_of_children`-th value in the indexed map (you can get this more nicely and reliably as `it->index()` which also works if `it == map.end()`).

You can also work with this segment map in the following ways:

* `map.upper_bound(position)`: find a value valid at a given `position`
* `map.set(position, value)`: set a `value` to be valid for a range ending at `position`. The value that was valid at this position before the setting is then valid only after the `position` (the end of the range for the previously valid value remains unchanged). If the `position` was beyond the total range currently represented by the map, then the total range is appropriately extended and the `value` is valid from the end of the previous total range until `position`.
* `map.remove(position)`: remove a value valid exactly until `position`, extending the range of the next value to be valid for the range of the removed value. If this is the last value, then the total range represented by the map gets reduced. Note that if you want to remove a value valid *at* `position` (as opposed to a value valid *exactly until* `position`), you have to do it like `map.remove(map.upper_bound(position)->second)`
* `map.replace(from, to, newLength, bindRight)`: this models replacing a range `(from, to)` with a new one of length `newLength` and making valid for this whole range the value that was previously valid at position `to`.

### `ng::layout_t`
This data structure holds a `ng::buffer_t` and a viewport width and height and provides services for calculating the layout of text, i.e. how the semantical lines (divided by newline character) are divided into visual softlines (induced by wrapping the text at the viewport width and folding), what is the interline spacing, font size etc.. It provides a way to retrieve various geometrical characteristics of ranges of text. Finally, it can use all this information to draw portions of the buffer into a CGContext.

## GUI

### `OakTextView.framework`
The `OakTextView.framework` contains the components you work most with when using TextMate:

* `OakTextView`: the text view itself; it uses a `ng::buffer_t` together with `ng::layout_t` to display text, and among other things, implements input handling consistent with Cocoa's key bindings mechanism.
* `GutterView`: the view left to the text view containing line numbers, folding marks etc.
* `OTVStatusBar`: the bar below the text view containing e.g. current bundle, symbol etc.
* `OakDocumentView`: a view that contains as its subviews an `OakTextView`, `GutterView` and `OTVStatusBar` and makes them work together