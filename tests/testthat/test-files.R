context("File predicates")

test_that("File predicates work", {
  expect_true(does_file_exist(system.file("DESCRIPTION")))
  expect_false(does_file_exist(system.file("~DESCRIPTION~")))
  expect_true(is_directory(system.file()))
  expect_false(is_directory(system.file("DESCRIPTION")))
  expect_true(has_extension(system.file("DESCRIPTION"), ""))
  expect_true(has_extension(system.file("R/base.rdb"), "rdb"))
})