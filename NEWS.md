# XploRer 0.8.0

* simplification of the package for mtg computations:

  - two main functions are exported: `descendants()` and `ancestors()`. The first help get values from parent to parent, and the other from children to children. They share the same arguments to simplify their usage, with filters for the scale, the symbol, the link, and also for user-defined filters (`filter_fun`). These functions are very powerfull but they can be hard to master. 
  
  - several helpers: `children()`, `parents()` and `leaves()`. They are wrappers around the previous functions designed to help the user make basic computations in a simple way.

* add new example: `simple_plant_2.mtg`. It is a plant that is a little bit more complex than `simple_plant.mtg`. Use the following code to use it:

  ```r
  filepath= system.file("extdata", "simple_plant_2.mtg", package = "XploRer")
  MTG2 = read_mtg(filepath)
  ```

* add `.symbol` argument to `mutate_mtg()` and .`scale` argument is now used for the scale.

* Fix bug when reading mtg with trailing/leading white space in column names

* Fix bug when reading mtg for classes defined with spaces before ":", e.g. "CODE :…"

* Make the package compatible with scenes (several trees in a scene)

* `read_mtg()` is more explicit when an error occurs in the mtg

* `recursive` argument now became `continue` for all functions used to compute variables in the mtg

* Add `recursivity_level` argument to the functions for computing variables in the mtg. It is used to stop the recursive search at a given level. It also takes into account the scale, meaning traversing a scale that is filtered out does not count as a level


# XploRer 0.7.0

* `get_parent_value()`, `get_children_values()` and `get_ancestors_values()` all accept expressions as attribute argument. Use `node$variable` to be sure to use the value from the node (as for `rlang::.data` is used in `dplyr`)

* replace fieldsAll by attributesAll in nodes following data.tree 1.0.0

* add Getting started vignette 

* add contributing + code of conduct + issue template


# XploRer 0.6.0

* improve doc in readme 

* add a recursive argument to `get_parent_value()`

* get_children_values and `get_ancestors_values()` now return the node name

* fix issues with `get_ancestors_values()` (didn't work properly when called from `mutate_mtg()`)


# XploRer 0.5.0

 * Add `get_ancestors_values()` to get all values from the ancestors of a node (with or without the values of the current node, see `self` argument) 

# XploRer 0.4.0

* Fix issue in `get_children_values()` when requiring a particular scale

* Fix issue in tooltip for `plotly_mtg()` when requiring several user variables

* Rename `.scale` in `scale` for helpers (they don't use ellipsis anymore)

# XploRer 0.3.0

* `get_parent_value()` was added to get the variables of the parent of a node

* `get_children_values()` was added to get the variables of the children of a node

* `mutate_mtg()` can use both `get_parent_value()` and `get_children_values()`

* It is now possible to add any variable to the tooltip on hoover of a node in `plotly_mtg()`, e.g.: `plotly_mtg(MTG,Length,Width)`

# XploRer 0.2.0

* Mutating an MTG is now possible using `mutate_mtg()`. Its design is close to the one from `dplyr::mutate`

* Plotting an MTG is now done using `autoplot(MTG)`  

* Interactive plots can be done using `plotly_mtg(MTG)`, which is an interface to the plotly API

# XploRer 0.1.1

* Added a `NEWS.md` file to track changes to the package.  

* First (alpha) working version of the package. What seems to work: read an MTG, and plot an MTG

