context("'Get' functions")

filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
MTG = read_MTG(filepath)
node_5 = data.tree::FindNode(MTG$MTG, "node_5")

test_that("get_parent_value: requesting one attribute", {
  test = get_parent_value("Length", node = node_5)
  expect_equal(test,list(Length = 4.0))
})

test_that("get_parent_value: requesting two attributes", {
  test = get_parent_value("Length", "Width", node = node_5)
  expect_equal(test,list(Length = 4.0, Width = 1.0))
})

test_that("get_parent_value: requesting two attributes, one is absent", {
  test = get_parent_value("Length", "test", node = node_5)
  expect_null(test$test)
})

test_that("get_parent_value: test root node", {
  test = get_parent_value("Length", "test", node = MTG$MTG)
  expect_length(test, 2)
  expect_null(test$test)
  expect_null(test$Length)
})

