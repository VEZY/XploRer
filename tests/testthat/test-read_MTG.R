context("Read MTG")

filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
MTG_file = readLines(filepath)
MTG_file = strip_comments(MTG_file)
MTG_file = strip_empty_lines(MTG_file)

test_that("Check the sections", {
  expect_null(check_sections(MTG_file))
})

test_that("Parse code", {
  expect_equal(parse_MTG_code(MTG_file), "FORM-A")
})

classes = parse_MTG_classes(MTG_file)

test_that("Parse classes", {
  expect_true(is.data.frame(classes))
  expect_equal(nrow(classes),5)
  expect_known_hash(classes, hash = "d8df027e91")
})

description = parse_MTG_description(MTG_file)

test_that("Parse description", {
  expect_known_hash(description, hash = "2ab85874bd")
})

features = parse_MTG_section(MTG_file,"FEATURES:",
                             c("NAME", "TYPE"),
                             "MTG:",TRUE)

test_that("Parse features", {
  expect_known_hash(features, hash = "d2f93f8d5c")
})

test_that("Parse MTG", {
  MTG = parse_MTG_MTG(MTG_file,classes,description,features)
  expect_known_hash(MTG, hash = "b6a55f4b86")
})

test_that("Read MTG file", {
  MTG = read_MTG(filepath)
  expect_known_hash(MTG, hash = "e0f411dbe9")
})


