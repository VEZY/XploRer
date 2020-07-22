context("Testing MTG apply")

filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
MTG = read_mtg(filepath)

test_that("adding a variable with mutate_mtg works", {
  mutate_mtg(MTG, test = 0+1)
  mtg_df = ToDataFrameTree(MTG$MTG,"test")
  expect_equal(mtg_df$test, rep(1,6))
})

test_that("adding a variable with mutate_mtg using another node attribute works", {
  mutate_mtg(MTG, test = node$Width + 1)
  mtg_df = ToDataFrameTree(MTG$MTG,"test","Width")
  expect_equal(mtg_df$test, mtg_df$Width + 1)
})

test_that("mutate_mtg works in all its glory", {
  # Testing several things here:
  # Computation of three new variables: section_surface, Width_parent, section_surface_parent
  # section_surface uses a node attribute already present in the MTG
  # Width_parent uses a function to get the parent value of the node
  # section_surface_parent uses a node that is computed during the mutation (Width_parent)

  mutate_mtg(MTG,
             section_surface = pi * ((node$Width / 2)^2),
             Width_parent = get_parent_value("Width",  node = node)[[1]],
             section_surface_parent = pi * (node$Width_parent / 2)^2)
  mtg_df =
    ToDataFrameTree(MTG$MTG,"Width","Width_parent","section_surface",
                    "section_surface_parent")

  expect_equal(mtg_df$section_surface, pi * ((mtg_df$Width / 2)^2))
  expect_equal(mtg_df$section_surface_parent, pi * ((mtg_df$Width_parent / 2)^2))
})
