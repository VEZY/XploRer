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
  expect_equal(classes$SYMBOL,c("$","Individual","Axis","Internode","Leaf"))
  expect_equal(classes$SCALE,c(0,1,2,3,3))
  expect_equal(classes$DECOMPOSITION,rep("FREE",5))
  expect_equal(classes$INDEXATION,rep("FREE",5))
  expect_equal(classes$DEFINITION,rep("IMPLICIT",5))
})

description = parse_MTG_description(MTG_file)

test_that("Parse description", {
  expect_true(is.data.frame(description))
  expect_equal(nrow(description),2)
  expect_equal(description$LEFT,rep("Internode",2))
  expect_equal(description$RELTYPE,c("+","<"))
  expect_equal(description$MAX,c("?","?"))
})

features = parse_MTG_section(MTG_file,"FEATURES:",
                             c("NAME", "TYPE"),
                             "MTG:",TRUE)

test_that("Parse features", {
  expect_true(is.data.frame(features))
  expect_equal(nrow(features),7)
  expect_equal(features$NAME,c('XX','YY','ZZ','FileName','Length','Width','XEuler'))
  expect_equal(features$TYPE,c('REAL','REAL','REAL','ALPHA','ALPHA','ALPHA','REAL'))
})

test_that("Parse MTG", {
  MTG = parse_MTG_MTG(MTG_file,classes,description,features)
  expect_equal(MTG$totalCount,6) # number of nodes
  expect_equal(MTG$leafCount,2)
  expect_equal(MTG$height,5)
  expect_equal(MTG$averageBranchingFactor,1.25)
})

test_that("Read MTG file", {
  MTG = read_mtg(filepath)
  expect_length(MTG,4)
  expect_equal(names(MTG),c("classes","description","features","MTG"))
  expect_equal(MTG$MTG,parse_MTG_MTG(MTG_file,classes,description,features))
})
