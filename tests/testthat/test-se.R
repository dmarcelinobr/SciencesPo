context("se expectations")
test_that("computes standard error", {
  expect_equal(se(c(1, 2.3, 2, 3, 4, 8, 12, 43, -1,-4)), 4.236195, tolerance=1e-3)
})


