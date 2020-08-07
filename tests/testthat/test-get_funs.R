filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")

MTG = read_mtg(filepath)
node_5 = data.tree::FindNode(MTG$MTG, "node_5")
node_6 = data.tree::FindNode(MTG$MTG, "node_6")

filepath2= system.file("extdata", "simple_plant_2.mtg", package = "XploRer")
MTG2 = read_mtg(filepath2)
node_6_2 = data.tree::FindNode(MTG2$MTG, "node_6")


test_that("get_parent_value: requesting an attribute", {
  test = get_parent_value("Length", node = node_6)
  expect_equal(test,6.0)
  test_2 = get_parent_value("Length", node = node_6_2)
  expect_equal(test_2,4.0)
})

test_that("get_parent_value: requesting an attributes that is missing", {
  test = get_parent_value("test", node = node_5)
  expect_true(is.na(test))
})

test_that("get_parent_value: test root node", {
  test = get_parent_value("Length", node = MTG$MTG)
  expect_length(test, 1)
  expect_true(is.na(test))
})

test_that("get_parent_value: test filters", {
  test = get_parent_value(attribute = ".symbol",
                          node = data.tree::FindNode(MTG2$MTG,'node_4'),
                          scale = 2)
  expect_equal(test, "Axis")
})

test_that("get_children_values: requesting an attribute", {
  test = get_children_values(attribute = "Length", node = node_5)
  expect_equal(test,c(node_6 = 12.0))

  test2 = get_children_values(attribute = "Length", node = MTG2$MTG$node_2$node_3)
  expect_equal(test2,c(node_4 = NA, node_7 = 6.0))
})

test_that("get_children_values: requesting an attributes that is missing", {
  test = get_children_values("test", node = node_5)
  expect_true(is.na(test))
})

test_that("get_children_values: test filter", {
  test = get_children_values(attribute = ".symbol",
                             node = data.tree::FindNode(MTG2$MTG,"node_2"),
                             scale = 2)
  expect_equal(test,c(node_4 = "Axis", node_8 = "Axis"))

  test2 = get_children_values(attribute = ".symbol",
                             node = data.tree::FindNode(MTG2$MTG,"node_2"),
                             symbol = "Leaf")
  expect_equal(test2,c(node_6 = "Leaf", node_10 = "Leaf"))

  test3 = get_children_values(attribute = ".symbol",
                              node = data.tree::FindNode(MTG2$MTG,"node_2"))
  expect_equal(test3,c(node_3 = "Internode"))
})

test_that("get_ancestors_values: requesting an attribute", {
  test = get_ancestors_values(attribute = "Length", node = node_5)
  expect_equal(setNames(test[1],NULL),get_parent_value("Length", node = node_5))
  expect_equal(test,c(node_3 = 4, node_2 = NA, node_1 = NA))
})

test_that("get_ancestors_values: get an attribute with self", {
  test = get_ancestors_values(attribute = "Length", node = node_5, self = TRUE)
  expect_equal(setNames(test[1],NULL),node_5$Length)
  expect_equal(setNames(test[2],NULL),get_parent_value("Length", node = node_5))

  expect_equal(test,c(node_5 = 6, node_3 = 4, node_2 = NA, node_1 = NA))
})

test_that("get_ancestors_values: get an attribute with filter", {
  test = get_ancestors_values(attribute = "Length", node = node_5, symbol = "Internode")
  expect_length(test,1)
  expect_equal(setNames(test,NULL),4.0)

  test = get_ancestors_values(attribute = "Length", node = node_5, symbol = "Internode", self = TRUE)
  expect_length(test,2)
  expect_equal(test,c(node_5 = 6, node_3 = 4))

  expect_equal(get_ancestors_values("Width", node = node_6, symbol = "Leaf"), vector())
})
