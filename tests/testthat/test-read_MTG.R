context("Read MTG")

filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
MTG_file = readLines(file)
MTG_file = strip_comments(MTG_file)
MTG_file = strip_empty_lines(MTG_file)

test_that("Check the sections", {
  expect_null(check_sections(MTG_file))
})

test_that("Read code", {
  expect_equal(read_MTG_code(MTG_file), "FORM-A")
})

test_that("Read classes", {
  classes = read_MTG_classes(MTG_file)
  expect_true(is.data.frame(classes))
  expect_equal(nrow(classes),5)
  expect_known_hash(classes, hash = "d8df027e91")
})

test_that("Read description", {
  description = read_MTG_description(MTG_file)
  expect_known_hash(description, hash = "2ab85874bd")
})

test_that("Read features", {
  features = read_MTG_section(MTG_file,"FEATURES:",
                              c("NAME", "TYPE"),
                              "MTG:",TRUE)
  expect_known_hash(features, hash = "d2f93f8d5c")
})
