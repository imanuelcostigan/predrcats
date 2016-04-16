context("Scalar types")

test_that("Scalar type predicates work", {
  expect_true(is_scalar_list(list(1)))
  expect_false(is_scalar_list(list(1, 2)))
  expect_true(is_integer(1:2))
  expect_false(is_scalar_integer(1:2))
  expect_true(is_scalar_integer(1L))
})