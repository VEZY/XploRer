context("'Get' functions")

filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
MTG = read_mtg(filepath)
node_5 = data.tree::FindNode(MTG$MTG, "node_5")

test_that("get_parent_value: requesting an attribute", {
  test = get_parent_value("Length", node = node_5)
  expect_equal(test,4.0)
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

test_that("get_children_values: requesting an attribute", {
  test = get_children_values("Length", node = node_5)
  expect_equal(test,12.0)
})

test_that("get_children_values: requesting an attributes that is missing", {
  test = get_children_values("test", node = node_5)
  expect_true(is.na(test))
})

