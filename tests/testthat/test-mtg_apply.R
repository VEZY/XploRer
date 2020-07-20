context("Testing MTG apply")

filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
MTG = read_MTG(filepath)

test_that("mtg_apply works", {
  fun = function(node) {
    node$section_surface = pi * ((get_parent_value("Width",  node = node)[[1]] / 2)^2)
  }
  mtg_apply(MTG, fun = fun)
  mtg_df = ToDataFrameTree(MTG$MTG,"length","diameter","topological_order","section_surface","section_surface_childs","section_surface_parent")
})

MTG$MTG$node_2$node_3$node_4$Width
fun(MTG$MTG$node_2$node_3$node_4)
MTG$MTG$node_2$node_3$section_surface
print(MTG$MTG, "section_surface")
