context("Bare types")

test_that("Bare type predicates work", {
  expect_true(is_list(data.frame(a = 1)))
  expect_false(is_bare_list(data.frame(a = 1)))
  expect_true(is_bare_list(list(a = 1)))
  expect_true(is_double(Sys.Date()))
  expect_false(is_bare_double(Sys.Date()))
  expect_true(is_bare_double(1))
  expect_true(is_bare_integer(as.matrix(1:10)))
  expect_false(is_bare_integer(matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3,
    byrow = TRUE, dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3")))))
})