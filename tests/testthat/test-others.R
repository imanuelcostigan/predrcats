context("Other predicates")

test_that("is_empty works", {
  expect_true(is_empty(NULL))
  expect_true(is_empty(character()))
  expect_false(is_empty("a"))
})

test_that("is_scalar works", {
  expect_true(is_scalar(1))
  expect_false(is_scalar(c(1, 2)))
})

test_that("is_formula works", {
  expect_true(is_formula(disp ~ am))
})

test_that("is_date works", {
  expect_true(is_date(Sys.Date()))
  expect_false(is_date(Sys.time()))
})

test_that("is_time works", {
  expect_false(is_time(Sys.Date()))
  expect_true(is_time(Sys.time()))
  expect_true(is_time(as.POSIXlt(Sys.time())))
})

test_that("is_na, is_scalar_na and any_na work", {
  expect_true(is_na(NA))
  expect_true(is_scalar_na(NA))
  expect_true(any_na(NA))
  expect_false(is_na(c(1, NA)))
  expect_false(is_scalar_na(c(1, NA)))
  expect_true(any_na(c(1, NA)))
  expect_false(any_na(list(1, list(1, NA))))
})

