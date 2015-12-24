context("Cramer's Phi coefficient for tables expectations.\n Friendly (2000), 'Visualizing Categorical Data', SAS Institute Inc., p. 61.")
test_that("The Cramer's Phi coefficient for tables", {
  mt = matrix(c(5, 29, 14, 16, 15, 54, 14, 10, 20,  84, 17, 94, 68, 119, 26, 7),ncol=4, byrow=TRUE);
 expect_equal(phi(mt), 0.483, tolerance=1e-3)
})

