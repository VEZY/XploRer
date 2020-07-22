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

