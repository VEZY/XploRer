

# File 2: A simple plant:
filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
MTG = read_mtg(filepath)
node_6 = extract_node(MTG, "node_6")
node_7 = extract_node(MTG, "node_7")

# File 2: A simple plant with a more complex topology than the first:
filepath2= system.file("extdata", "simple_plant_2.mtg", package = "XploRer")
MTG2 = read_mtg(filepath2)
node_7_2 = extract_node(MTG2, "node_7")

# File 3: real mtg of a full branch (Walnut tree):
filepath3= system.file("extdata", "tree1h.mtg", package = "XploRer")
tree1h = read_mtg(file = filepath3)
# Taking only a sub-part here:
A4 = list()
A4$MTG = extract_node(tree1h, "node_6")



test_that("get_parent_value: requesting an attribute", {
  test = get_parent_value("Length", node = node_7)
  expect_equal(test,6.0)
  test_2 = get_parent_value("Length", node = node_7_2)
  expect_equal(test_2,4.0)
})

test_that("get_parent_value: requesting an attributes that is missing", {
  test = get_parent_value("test", node = node_6)
  expect_true(is.na(test))
})

test_that("get_parent_value: test root node", {
  test = get_parent_value("Length", node = MTG$MTG)
  expect_length(test, 1)
  expect_true(is.na(test))
})

test_that("get_parent_value: test filters", {
  test = get_parent_value(attribute = ".symbol",
                          node = extract_node(MTG2,'node_4'),
                          scale = 2)
  expect_equal(test, "Axis")
})

test_that("get_children_values: requesting an attribute", {
  test = get_children_values(attribute = "Length", node = node_6)
  expect_equal(test,c(node_7 = 12.0))

  test2 = get_children_values(attribute = "Length", node = MTG2$MTG$node_2$node_3$node_4)
  expect_equal(test2,c(node_5 = NA, node_8 = 6.0))
})

test_that("get_children_values: requesting an attributes that is missing", {
  test = get_children_values("test", node = node_6)
  expect_true(is.na(test))
})

test_that("get_children_values: test filter", {
  test = get_children_values(attribute = ".symbol",
                             node = extract_node(MTG2,"node_3"),
                             scale = 2)
  expect_equal(test,c(node_5 = "Axis", node_9 = "Axis"))

  test2 = get_children_values(attribute = ".symbol",
                             node = extract_node(MTG2,"node_3"),
                             symbol = "Leaf")
  expect_equal(test2,c(node_7 = "Leaf", node_11 = "Leaf"))

  test3 = get_children_values(attribute = ".symbol",
                              node = extract_node(MTG2,"node_3"))
  expect_equal(test3,c(node_4 = "Internode"))
})

test_that("get_ancestors_values: requesting an attribute", {
  test = get_ancestors_values(attribute = "Length", node = node_6)
  expect_equal(setNames(test[1],NULL),get_parent_value("Length", node = node_6))
  expect_equal(test,c(node_4 = 4, node_3 = NA, node_2 = NA, node_1 = NA))
})

test_that("get_ancestors_values: get an attribute with self", {
  test = get_ancestors_values(attribute = "Length", node = node_6, self = TRUE)
  expect_equal(setNames(test[1],NULL),node_6$Length)
  expect_equal(setNames(test[2],NULL),get_parent_value("Length", node = node_6))

  expect_equal(test,c(node_6 = 6, node_4 = 4, node_3 = NA, node_2 = NA, node_1 = NA))
})

test_that("get_ancestors_values: get an attribute with filter", {
  test = get_ancestors_values(attribute = "Length", node = node_6, symbol = "Internode")
  expect_length(test,1)
  expect_equal(setNames(test,NULL),4.0)

  test = get_ancestors_values(attribute = "Length", node = node_6, symbol = "Internode", self = TRUE)
  expect_length(test,2)
  expect_equal(test,c(node_6 = 6, node_4 = 4))

  expect_equal(get_ancestors_values("Width", node = node_7, symbol = "Leaf"), vector())
})


test_that("get_descendants_values: requesting an attribute", {
  test = get_descendants_values(attribute = "Length", node = node_6)
  expect_equal(test,get_children_values("Length", node = node_6))
  expect_equal(test,c(node_7 = 12))
})


test_that("get_descendants_values: requesting an attribute", {
  test = get_descendants_values(attribute = "Length", node = node_6)
  expect_equal(test,get_children_values("Length", node = node_6))
  expect_equal(test,c(node_7 = 12))
})


test_that("get_descendants_values", {
  test = get_descendants_values(attribute = "length", node = A4$MTG)
  expect_equal(test,c(node_7 = 27.5, node_8 = NA, node_13 = 6, node_9 = 3.5,
                      node_10 = NA, node_12 = 10.3, node_11 = 5.2, node_14 = NA,
                      node_16 = 19.5, node_15 = 6))
})

test_that("get_descendants_values: filter by symbol", {
  # By S:
  test = get_descendants_values(attribute = "length", node = A4$MTG, symbol = "S")
  expect_equal(test,c(node_7 = 27.5, node_13 = 6, node_9 = 3.5, node_12 = 10.3,
                      node_11 = 5.2, node_16 = 19.5, node_15 = 6))

  # By A:
  test = get_descendants_values(attribute = "length", node = A4$MTG, symbol = "A")
  expect_equal(test, c(node_8 = NA_real_, node_10 = NA_real_, node_14 = NA_real_))
})

test_that("get_descendants_values: no recursivity", {
  # By S, non-recursive:
  test = get_descendants_values(attribute = "length", node = A4$MTG, symbol = "S",
                                recursive = FALSE)
  expect_equal(test,c(node_7 = 27.5, node_13 = 6, node_16 = 19.5))

  # By A, non-recursive:
  test = get_descendants_values(attribute = "length", node = A4$MTG, symbol = "A",
                                recursive = FALSE)
  expect_null(test)
})

test_that("get_descendants_values: with self", {
  test = get_descendants_values(attribute = "length", node = A4$MTG$node_7, symbol = "S",
                                self = TRUE)
  expect_equal(test,c(node_7 = 27.5, node_13 = 6, node_9 = 3.5, node_12 = 10.3,
                      node_11 = 5.2, node_16 = 19.5, node_15 = 6))

  # Without self:
  test = get_descendants_values(attribute = "length", node = A4$MTG$node_7, symbol = "S",
                                self = FALSE)
  expect_equal(test,c(node_13 = 6, node_9 = 3.5, node_12 = 10.3,
                      node_11 = 5.2, node_16 = 19.5, node_15 = 6))
})

test_that("get_descendants_values: not recursive and self", {
  test = get_descendants_values(attribute = "length", node = A4$MTG$node_7, symbol = "S",
                                recursive = FALSE, self = TRUE)
  expect_equal(test,c(node_7 = 27.5, node_13 = 6, node_16 = 19.5))
})
