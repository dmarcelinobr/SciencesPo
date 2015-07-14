context("se expectations")
test_that("computes standard error", {
  expect_equal(se(c(1, 2.3, 2, 3, 4, 8, 12, 43, -1,-4)), 4.236195, tolerance=1e-3)
})

context("cv expectations")
test_that("computes coefficient of variation", {
set.seed(51)
x <- sample(100)
expect_equal(cv(x), 0.574485, tolerance=1e-3)
})
