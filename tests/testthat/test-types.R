context("Type predicates")

test_that("Type predicates work", {
  expect_false(is_atomic(NULL))
  expect_true(is_vector(1:3))
  expect_true(is_vector(list(1:3)))
  expect_true(is_numeric(1))
  expect_true(is_numeric(1:3))
  expect_true(is_integer(1:3))
  expect_false(is_double(1:3))
  expect_true(is_double(c(1, 2, 3)))
  expect_true(is_atomic(as.Date("2013-01-01")))
  expect_true(is_numeric(as.Date("2013-01-01")))
  expect_true(is_numeric(as.POSIXct("2013-01-01")))
  expect_true(is_character(letters))
  expect_false(is_character(factor(letters)))
  expect_true(is_integer(factor(letters)))
  expect_true(is_logical(NA))
  expect_false(is_logical(0))
  expect_true(is_null(NULL))
  expect_true(is_function(function(x) x))
  expect_false(is_function(.Primitive("sqrt")))
  expect_true(is_complex(1i))
  expect_false(is_complex(1))
  expect_true(is_raw(as.raw(40)))
  expect_true(is_name(as.name("arrg")))
  expect_true(is_call(call("round", 10.5)))
  expect_true(is_language(call("round", 10.5)))
  expect_true(is_language(y~x))
})