context("Political Diversity Measures")
test_that("The effective number of parties given Laakso and Taagepera, and Golosov", {
  B <- c(.75,.10,rep(0.01,15));
  expect_equal(politicalDiversity(B), 1.74, tolerance=.05)
  expect_equal(object = politicalDiversity(B, index = "Golosov"), expected = 1.42, tolerance =.05, scale = 1.42)
})

