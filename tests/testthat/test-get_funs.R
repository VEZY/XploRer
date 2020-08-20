

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

# File 4: A simple plant with a leaf added on the first internode
filepath4= system.file("extdata", "simple_plant_3.mtg", package = "XploRer")
MTG4 = read_mtg(filepath4)
node_3_4 = extract_node(MTG4, "node_3")


test_that("parent: requesting an attribute", {
  test = parent("Length", node = node_7)
  expect_equal(test,c(node_6 = 6.0))
  test_2 = parent("Length", node = node_7_2)
  expect_equal(test_2,c(node_6 = 4.0))
})

test_that("parent: requesting an attributes that is missing", {
  test = parent("test", node = node_6)
  expect_true(is.na(test))
})

test_that("parent: test root node", {
  test = parent("Length", node = MTG$MTG)
  expect_length(test, 0)
  expect_true(is.null(test))
})

test_that("parent: test filters", {
  test = parent(attribute = ".symbol",
                          node = extract_node(MTG2,'node_4'),
                          scale = 2)
  expect_equal(test, c(node_3 = "Axis"))
})

test_that("children: requesting an attribute", {
  test = children(attribute = "Length", node = node_6)
  expect_equal(test,c(node_7 = 12.0))

  test2 = children(attribute = "Length", node = MTG2$MTG$node_2$node_3$node_4)
  expect_equal(test2,c(node_5 = NA, node_8 = 6.0))
})

test_that("children: requesting an attributes that is missing", {
  test = children("test", node = node_6)
  expect_true(is.na(test))
})

test_that("children: test filter", {
  test = children(attribute = ".symbol",
                             node = extract_node(MTG2,"node_3"),
                             scale = 2)
  expect_equal(test,c(node_5 = "Axis", node_9 = "Axis"))

  test2 = children(attribute = ".symbol",
                             node = extract_node(MTG2,"node_3"),
                             symbol = "Leaf")
  expect_equal(test2,c(node_7 = "Leaf", node_11 = "Leaf"))

  test3 = children(attribute = ".symbol",
                              node = extract_node(MTG2,"node_3"))
  expect_equal(test3,c(node_4 = "Internode"))
})

test_that("ancestors: requesting an attribute", {
  test = ancestors(attribute = "Length", node = node_6)
  expect_equal(test[1],parent("Length", node = node_6))
  expect_equal(test,c(node_4 = 4, node_3 = NA, node_2 = NA, node_1 = NA))
})

test_that("ancestors: get an attribute with self", {
  test = ancestors(attribute = "Length", node = node_6, self = TRUE)
  expect_equal(setNames(test[1],NULL),node_6$Length)
  expect_equal(test[2],parent("Length", node = node_6))

  expect_equal(test,c(node_6 = 6, node_4 = 4, node_3 = NA, node_2 = NA, node_1 = NA))
})

test_that("ancestors: get an attribute with filter", {
  test = ancestors(attribute = "Length", node = node_6, symbol = "Internode")
  expect_length(test,1)
  expect_equal(setNames(test,NULL),4.0)

  test = ancestors(attribute = "Length", node = node_6, symbol = "Internode", self = TRUE)
  expect_length(test,2)
  expect_equal(test,c(node_6 = 6, node_4 = 4))

  expect_equal(ancestors("Width", node = node_7, symbol = "Leaf"), vector())
})


test_that("descendants: requesting an attribute", {
  test = descendants(attribute = "Length", node = node_6)
  expect_equal(test,children("Length", node = node_6))
  expect_equal(test,c(node_7 = 12))
})


test_that("descendants: requesting an attribute", {
  test = descendants(attribute = "Length", node = node_6)
  expect_equal(test,children("Length", node = node_6))
  expect_equal(test,c(node_7 = 12))
})


test_that("descendants", {
  test = descendants(attribute = "length", node = A4$MTG)
  expect_equal(test,c(node_7 = 27.5, node_8 = NA, node_13 = 6, node_9 = 3.5,
                      node_10 = NA, node_12 = 10.3, node_11 = 5.2, node_14 = NA,
                      node_16 = 19.5, node_15 = 6))
})

test_that("descendants: filter by symbol", {
  # By S:
  test = descendants(attribute = "length", node = A4$MTG, symbol = "S")
  expect_equal(test,c(node_7 = 27.5, node_13 = 6, node_9 = 3.5, node_12 = 10.3,
                      node_11 = 5.2, node_16 = 19.5, node_15 = 6))

  # By A:
  test = descendants(attribute = "length", node = A4$MTG, symbol = "A")
  expect_equal(test, c(node_8 = NA_real_, node_10 = NA_real_, node_14 = NA_real_))
})

test_that("descendants: no recursivity", {
  # By S, non-recursive:
  test = descendants(attribute = "length", node = A4$MTG, symbol = "S",
                                continue = FALSE)
  expect_equal(test,c(node_7 = 27.5, node_13 = 6, node_16 = 19.5))

  # By A, non-recursive:
  test = descendants(attribute = "length", node = A4$MTG, symbol = "A",
                                continue = FALSE)
  expect_null(test)
})

test_that("descendants: with self", {
  test = descendants(attribute = "length", node = A4$MTG$node_7, symbol = "S",
                                self = TRUE)
  expect_equal(test,c(node_7 = 27.5, node_13 = 6, node_9 = 3.5, node_12 = 10.3,
                      node_11 = 5.2, node_16 = 19.5, node_15 = 6))

  # Without self:
  test = descendants(attribute = "length", node = A4$MTG$node_7, symbol = "S",
                                self = FALSE)
  expect_equal(test,c(node_13 = 6, node_9 = 3.5, node_12 = 10.3,
                      node_11 = 5.2, node_16 = 19.5, node_15 = 6))
})

test_that("descendants: not recursive and self", {
  test = descendants(attribute = "length", node = A4$MTG$node_7, symbol = "S",
                                continue = FALSE, self = TRUE)
  expect_equal(test,c(node_7 = 27.5, node_13 = 6, node_16 = 19.5))
})

test_that("leaves", {
  expect_equal(leaves("length", node = A4$MTG),
               c(node_12 = 10.3, node_11 = 5.2, node_16 = 19.5, node_15 = 6))
})


test_that("leaves: using filters", {
  expect_equal(leaves("ID", node = A4$MTG, scale = 2),
               c(node_10 = 643, node_14 = 647))
  expect_equal(leaves("ID", node = A4$MTG, symbol = "A"),
               c(node_10 = 643, node_14 = 647))
})


test_that("decompose", {
  expect_equal(decompose("length", node = A4$MTG),
               c(node_7 = 27.5, node_13 = 6, node_16 = 19.5))
})

test_that("decompose: use filter", {
  expect_equal(decompose(".symbol", node = node_3_4),
               c(node_4 = 'Internode', node_5 = 'Leaf', node_9 = 'Internode'))

  expect_equal(decompose(".symbol", node = node_3_4, symbol = "Internode"),
               c(node_4 = 'Internode', node_9 = 'Internode'))
})

