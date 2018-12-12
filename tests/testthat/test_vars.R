library(gggenomes)
context("checking object vars")
test_that("require variables in object", {
  x <- tibble(a=1)
  expect_identical(x,require_vars(x, "a"))
  expect_error(require_vars(x,"b"), "Require")
})

test_that("test if variables exist in object", {
  x <- tibble(a=1)
  expect_true(has_vars(x, "a"))
  expect_false(has_vars(x, "b"))
  expect_false(has_vars(x, c("a","b")))
  expect_true(has_vars(x, c("a","b"), any=TRUE))
  expect_false(has_vars(x, c("c","b"), any=TRUE))
})
