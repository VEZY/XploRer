filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
MTG = read_mtg(filepath)

test_that("check_filters", {
  expect_error(check_filters(node = extract_node(MTG, "node_5"), scale = 1,
                             symbol = "Individual",link = "/"),
               regexp = NA)

  expect_error(check_filters(node = extract_node(MTG, "node_5"), scale = 10,
                symbol = "Individual",link = "/"),
               regexp = "The scale argument should be one of: 0, 1, 2, 3")

  expect_error(check_filters(node = extract_node(MTG, "node_5"), scale = 1,
                             symbol = "a",link = "/"))

  expect_error(check_filters(node = extract_node(MTG, "node_5"), scale = 1,
                             symbol = "Individual",link = "q"))
})
