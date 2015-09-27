context("cv expectations")
test_that("computes coefficient of variation", {
  set.seed(51)
  x <- sample(100)
  expect_equal(cv(x), 0.574485, tolerance=1e-3)
})
